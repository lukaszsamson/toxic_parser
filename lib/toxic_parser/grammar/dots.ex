defmodule ToxicParser.Grammar.Dots do
  @moduledoc """
  Dot expression parsing helpers (`foo.bar`, `Foo.Bar`, `foo.(...)`, `foo.()`).
  """

  alias ToxicParser.{Builder, Context, Cursor, Error, EventLog, Identifiers, Position, Pratt, Result, State, TokenAdapter}
  alias ToxicParser.Builder.{Helpers, Meta}
  alias ToxicParser.Grammar.{Delimited, EOE, Expressions, Keywords}

  @type result ::
          {:ok, Macro.t(), State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}

  @doc """
  Parse a dot call `expr.(...)` when the current token is `:dot_call_op`.
  """
  @spec parse_dot_call(Macro.t(), State.t(), Cursor.t(), Pratt.context(), EventLog.t()) ::
          result()
  def parse_dot_call(left, %State{} = state, cursor, %Context{} = ctx, %EventLog{} = log) do
    {:ok, dot_tok, state, cursor} = TokenAdapter.next(state, cursor)
    dot_meta = Helpers.token_meta(dot_tok)

    # dot_call_op is immediately followed by (
    {:ok, _open, state, cursor} = TokenAdapter.next(state, cursor)

    # Skip leading EOE and count newlines
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    with {:ok, args, state, cursor, log} <-
           ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, cursor, ctx, log),
         {:ok, close_meta, trailing_newlines, state, cursor} <-
           Meta.consume_closing(state, cursor, :")") do
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      call_meta = Meta.closing_meta(dot_meta, close_meta, total_newlines)

      {:ok, {{:., dot_meta, [left]}, call_meta, Enum.reverse(args)}, state, cursor, log}
    else
      other -> Result.normalize_error(other, cursor, log)
    end
  end

  @doc """
  Parses the RHS of a dot: identifier/alias/call/bracket/paren call.
  Returns `{:ok, {member_value, member_meta}, state, cursor, log}` for simple identifiers,
  or `{:ok, call_ast, state, cursor, log}` for calls.

  For simple identifiers, also returns `:no_parens_call` flag when the identifier
  was classified as :op_identifier, indicating that a
  no-parens call is expected.

  The optional `dot_meta` parameter provides the dot's metadata for curly calls
  where the call metadata should use the dot's position.
  """
  @spec parse_member(State.t(), Cursor.t(), Pratt.context(), EventLog.t(), keyword()) :: result()
  def parse_member(state, cursor, ctx, log, dot_meta \\ [])

  def parse_member(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log, dot_meta) do
    case TokenAdapter.next(state, cursor) do
      {:ok, {tok_kind, _meta, tok_value} = tok, state, cursor} ->
        case Identifiers.classify(tok_kind) do
          kind
          when kind in [
                 :identifier,
                 :do_identifier
               ] ->
            {:ok, {tok_value, Helpers.token_meta(tok)}, state, cursor, log}

          :bracket_identifier ->
            # Bracket identifier - bracket access IS allowed (no whitespace before [)
            {:ok, {tok_value, Helpers.token_meta(tok), :allows_bracket}, state, cursor, log}

          # op_identifier indicates no-parens call is expected
          :op_identifier ->
            # Return with :no_parens_call tag to indicate caller should expect no-parens args
            {:ok, {tok_value, Helpers.token_meta(tok), :no_parens_call}, state, cursor, log}

          :alias ->
            # Alias needs to be wrapped as __aliases__
            alias_ast = Helpers.from_token(tok)

            case Cursor.peek(cursor) do
              {:ok, {:"[", _meta, _value}, cursor} ->
                {:ok, {alias_ast, :allows_bracket}, state, cursor, log}

              {:ok, _next_tok, cursor} ->
                {:ok, alias_ast, state, cursor, log}

              {:eof, cursor} ->
                {:ok, alias_ast, state, cursor, log}
            end

          :paren_identifier ->
            parse_paren_call(tok, state, cursor, ctx, log)

          _ ->
            cond do
              # Check for quoted identifier: D."foo"
              tok_kind == :quoted_identifier_start ->
                parse_quoted_identifier(tok, state, cursor, ctx, log)

              # Handle curly container call: n.{} or n.{a, b}
              # Grammar: dot_alias -> matched_expr dot_op open_curly container_args close_curly
              tok_kind == :"{" ->
                parse_curly_call(tok, state, cursor, ctx, log, dot_meta)

              true ->
                {:error, {:expected, :dot_member, got: tok_kind}, state, cursor, log}
            end
        end

      {:eof, state, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, state, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  defp parse_paren_call({_, _, callee} = tok, state, cursor, ctx, log) do
    {:ok, _open, state, cursor} = TokenAdapter.next(state, cursor)

    # Skip leading EOE and count newlines
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    with {:ok, args, state, cursor, log} <-
           ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, cursor, ctx, log),
         {:ok, close_meta, trailing_newlines, state, cursor} <-
           Meta.consume_closing(state, cursor, :")") do
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      callee_meta = Helpers.token_meta(tok)
      meta = Meta.closing_meta(callee_meta, close_meta, total_newlines)

      # Use just the atom value for the callee, not the full AST form
      # Dot call RHS should be :bar, not {:bar, meta, nil}
      {:ok, {callee, meta, Enum.reverse(args)}, state, cursor, log}
    else
      other -> Result.normalize_error(other, cursor, log)
    end
  end

  # Parse curly container call: n.{} or n.{a, b}
  # Grammar: dot_alias -> matched_expr dot_op open_curly container_args close_curly
  # Returns {:ok, {:{}, meta, args}, state, cursor, log} where :{} is the atom function name
  # The dot_meta parameter provides the dot's metadata for the call metadata (column should be dot's position)
  defp parse_curly_call(open_tok, state, cursor, _ctx, log, dot_meta) do
    # Skip leading EOE and count newlines
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    container_ctx = Context.container_expr()

    with {:ok, state, cursor, log} <- reject_initial_kw_data(state, cursor, container_ctx, log),
         {:ok, tagged_items, state, cursor, log} <-
           parse_curly_args(state, cursor, container_ctx, log),
         {:ok, close_meta, trailing_newlines, state, cursor} <-
           Meta.consume_closing(state, cursor, :"}") do
      args = finalize_curly_items(tagged_items)
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      # Use dot's metadata for the call column (not the curly's position)
      # If dot_meta is empty (no dot context), fall back to open_tok metadata
      base_meta =
        if dot_meta == [], do: Helpers.token_meta(open_tok), else: dot_meta

      meta = Meta.closing_meta(base_meta, close_meta, total_newlines)

      # Return call format with :{} as the function name
      # This matches {name, meta, args} pattern in the caller
      {:ok, {:{}, meta, args}, state, cursor, log}
    else
      other -> Result.normalize_error(other, cursor, log)
    end
  end

  defp parse_curly_args(state, cursor, container_ctx, log) do
    item_fun = fn state, cursor, _ctx, log ->
      case Keywords.try_parse_kw_data(state, cursor, container_ctx, log) do
        {:ok, kw_list, state, cursor, log} ->
          {state, cursor, _newlines} = EOE.skip_count_newlines(state, cursor, 0)

          case Cursor.peek(cursor) do
            {:ok, {:"}", _meta, _value}, cursor} ->
              {:ok, {:kw_data, kw_list}, state, cursor, log}

            {:ok, {kind, _meta, _value}, cursor} ->
              {:error, {:expected, :"}", got: kind}, state, cursor, log}

            {:eof, cursor} ->
              {:error, :unexpected_eof, state, cursor, log}

            {:error, diag, cursor} ->
              {:error, diag, state, cursor, log}
          end

        {:no_kw, state, cursor, log} ->
          with {:ok, expr, state, cursor, log} <-
                 Expressions.expr(state, cursor, container_ctx, log) do
            {:ok, {:expr, expr}, state, cursor, log}
          end

        {:error, reason, state, cursor, log} ->
          {:error, reason, state, cursor, log}
      end
    end

    Delimited.parse_comma_separated(state, cursor, container_ctx, log, :"}", item_fun,
      allow_empty?: true
    )
  end

  defp reject_initial_kw_data(state, cursor, container_ctx, log) do
    case Cursor.peek(cursor) do
      {:ok, {:"}", _meta, _value}, cursor} ->
        {:ok, state, cursor, log}

      {:ok, {_kind, _meta, tok_value} = tok, cursor} ->
        cond do
          # Definite kw_data start - no need to checkpoint/parse.
          Keywords.starts_kw?(tok) ->
            meta = Helpers.token_meta(tok)
            {:error, syntax_error_before(meta, Atom.to_string(tok_value)), state, cursor, log}

          # Quoted key may or may not be kw_data; only this case needs checkpoint.
          Keywords.starts_kw_or_quoted_key?(tok) ->
            {ref, checkpoint_state} = TokenAdapter.checkpoint(state, cursor)

            case Keywords.try_parse_kw_data(checkpoint_state, cursor, container_ctx, log) do
              {:ok, kw_list, _checkpoint_state, _checkpoint_cursor, log} ->
                {state, cursor} = TokenAdapter.rewind(state, ref)
                meta = Helpers.token_meta(tok)
                token_display = kw_data_first_key_display(kw_list)
                {:error, syntax_error_before(meta, token_display), state, cursor, log}

              {:no_kw, _checkpoint_state, _checkpoint_cursor, log} ->
                {state, cursor} = TokenAdapter.rewind(state, ref)
                {:ok, state, cursor, log}

              {:error, reason, _checkpoint_state, _checkpoint_cursor, log} ->
                {state, cursor} = TokenAdapter.rewind(state, ref)
                {:error, reason, state, cursor, log}
            end

          true ->
            {:ok, state, cursor, log}
        end

      {:eof, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  defp kw_data_first_key_display([{key, _} | _]) when is_atom(key), do: Atom.to_string(key)
  defp kw_data_first_key_display([{key, _} | _]), do: Macro.to_string(key)
  defp kw_data_first_key_display(_), do: ""

  defp finalize_curly_items([]), do: []

  defp finalize_curly_items(tagged_items) do
    case List.last(tagged_items) do
      {:kw_data, kw_list} ->
        exprs =
          tagged_items
          |> Enum.drop(-1)
          |> Enum.map(fn {:expr, expr} -> expr end)

        exprs ++ [kw_list]

      _ ->
        Enum.map(tagged_items, fn {:expr, expr} -> expr end)
    end
  end

  defp syntax_error_before(meta, token_value) do
    line = Keyword.get(meta, :line, 1)
    column = Keyword.get(meta, :column, 1)
    {[line: line, column: column], "syntax error before: ", token_value}
  end

  # Parse quoted identifier: D."foo" or D."foo"() or D."foo" arg (no-parens)
  # Token sequence: quoted_identifier_start -> string_fragment* -> quoted_identifier_end/quoted_op_identifier_end
  defp parse_quoted_identifier(
         {:quoted_identifier_start, _meta, start_value} = start_tok,
         state,
         cursor,
         ctx,
         log
       ) do
    start_meta = Helpers.token_meta(start_tok)
    delimiter = delimiter_from_value(start_value)

    case collect_fragments([], state, cursor, :quoted_identifier_end, log) do
      {:ok, fragments, end_kind, state, cursor, log} ->
        with {:ok, _close, state, cursor} <- TokenAdapter.next(state, cursor) do
          case safe_unescape_quoted_identifier(fragments, state, start_meta) do
            {:ok, content} ->
              atom = String.to_atom(content)

              # Build metadata with delimiter
              meta_with_delimiter = [{:delimiter, delimiter} | start_meta]

              cond do
                # quoted_op_identifier_end means there's a space before next token -> no-parens call
                end_kind == :quoted_op_identifier_end ->
                  # D."foo" arg - parse no-parens call arguments
                  with {:ok, args, state, cursor, log} <-
                         ToxicParser.Grammar.Calls.parse_no_parens_args(
                           [],
                           state,
                           cursor,
                           ctx,
                           log
                         ) do
                    # Return as call AST: {atom, meta, args}
                    {:ok, {atom, meta_with_delimiter, args}, state, cursor, log}
                  end

                # quoted_bracket_identifier_end: D."foo"[1] - return with :allows_bracket flag
                # so caller knows bracket access is allowed
                end_kind == :quoted_bracket_identifier_end ->
                  {:ok, {atom, meta_with_delimiter, :allows_bracket}, state, cursor, log}

                # quoted_do_identifier_end: D."foo" do...end - return as call AST, do-block parsed by caller
                end_kind == :quoted_do_identifier_end ->
                  # Return as call expression {atom, meta, []} with no_parens: true for do-block attachment
                  meta_with_no_parens = [{:no_parens, true} | meta_with_delimiter]
                  {:ok, {atom, meta_with_no_parens, []}, state, cursor, log}

                true ->
                  {quoted_paren_identifier_end_or_immediately_parens, cursor} =
                    if end_kind == :quoted_paren_identifier_end do
                      {true, cursor}
                    else
                      case Cursor.peek(cursor) do
                        {:ok, {:"(", _, _}, cursor} -> {true, cursor}
                        {:ok, _, cursor} -> {false, cursor}
                        {:eof, cursor} -> {false, cursor}
                      end
                    end

                  if quoted_paren_identifier_end_or_immediately_parens do
                    # quoted_paren_identifier_end or ( immediately follows: D."foo"()
                    {:ok, _open, state, cursor} = TokenAdapter.next(state, cursor)

                    # Skip leading EOE and count newlines
                    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

                    with {:ok, args, state, cursor, log} <-
                           ToxicParser.Grammar.CallsPrivate.parse_paren_args(
                             [],
                             state,
                             cursor,
                             Context.matched_expr(),
                             log
                           ),
                         {:ok, close_meta, trailing_newlines, state, cursor} <-
                           Meta.consume_closing(state, cursor, :")") do
                      total_newlines =
                        Meta.total_newlines(leading_newlines, trailing_newlines, true)

                      call_meta = Meta.closing_meta(meta_with_delimiter, close_meta, total_newlines)

                      # Return as call AST: {atom, call_meta, args}
                      {:ok, {atom, call_meta, Enum.reverse(args)}, state, cursor, log}
                    else
                      other -> Result.normalize_error(other, cursor, log)
                    end
                  else
                    # Simple quoted identifier without parens
                    {:ok, {atom, meta_with_delimiter}, state, cursor, log}
                  end
              end

            {:error, reason} ->
              {:error, reason, state, cursor, log}
          end
        end

      {:error, {:missing_terminator, error_tok}, state, cursor, log} ->
        if state.mode == :tolerant do
          {state, cursor} = sync_to_quoted_identifier_end(state, cursor)
          {state, cursor} = recover_missing_terminator(state, cursor, start_meta)
          error_ast = build_missing_terminator_node(error_tok, start_meta, state, cursor)
          {:ok, error_ast, state, cursor, log}
        else
          {:error, {:missing_terminator, error_tok}, state, cursor, log}
        end

      {:error, reason, state, cursor, log} ->
        {:error, reason, state, cursor, log}
    end
  end

  defp safe_unescape_quoted_identifier(fragments, %State{mode: :tolerant} = _state, start_meta) do
    try do
      content = fragments |> Enum.reverse() |> Enum.join("") |> Macro.unescape_string()
      {:ok, content}
    rescue
      error in ArgumentError ->
        {:error, {:unescape_error, Exception.message(error), start_meta}}
    end
  end

  defp safe_unescape_quoted_identifier(fragments, %State{} = _state, _start_meta) do
    content = fragments |> Enum.reverse() |> Enum.join("") |> Macro.unescape_string()
    {:ok, content}
  end

  # Possible end tokens for quoted identifiers
  # - quoted_identifier_end: D."foo" (no following token)
  # - quoted_op_identifier_end: D."foo" arg (space before next token - no-parens call)
  # - quoted_paren_identifier_end: D."foo"() (immediately followed by parens)
  # - quoted_bracket_identifier_end: D."foo"[1] (immediately followed by bracket)
  # - quoted_do_identifier_end: D."foo" do...end (space before do-block)
  @quoted_id_ends [
    :quoted_identifier_end,
    :quoted_op_identifier_end,
    :quoted_paren_identifier_end,
    :quoted_bracket_identifier_end,
    :quoted_do_identifier_end
  ]

  defp collect_fragments(acc, state, cursor, target_end, log) do
    case Cursor.peek(cursor) do
      {:ok, {tok_kind, _meta, fragment}, cursor} ->
        cond do
          tok_kind == target_end ->
            {:ok, acc, target_end, state, cursor, log}

          tok_kind in @quoted_id_ends and target_end == :quoted_identifier_end ->
            # Accept any quoted identifier end token
            {:ok, acc, tok_kind, state, cursor, log}

          tok_kind == :string_fragment ->
            {:ok, _frag, state, cursor} = TokenAdapter.next(state, cursor)
            collect_fragments([fragment | acc], state, cursor, target_end, log)

          tok_kind == :error_token and state.mode == :tolerant ->
            {:ok, error_tok, state, cursor} = TokenAdapter.next(state, cursor)

            case error_tok do
              {:error_token, _meta, %Toxic.Error{code: :string_missing_terminator}} ->
                {:error, {:missing_terminator, error_tok}, state, cursor, log}

              _ ->
                {:error, {:unexpected_string_token, tok_kind}, state, cursor, log}
            end

          true ->
            {:error, {:unexpected_string_token, tok_kind}, state, cursor, log}
        end

      {:eof, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  defp delimiter_from_value(?"), do: "\""
  defp delimiter_from_value(?'), do: "'"

  defp sync_to_quoted_identifier_end(state, cursor) do
    case Cursor.peek(cursor) do
      {:ok, {kind, _meta, _value}, cursor} ->
        if kind in @quoted_id_ends do
          case TokenAdapter.next(state, cursor) do
            {:ok, _tok, state, cursor} -> {state, cursor}
            {:eof, state, cursor} -> {state, cursor}
            {:error, _reason, state, cursor} -> {state, cursor}
          end
        else
          case TokenAdapter.next(state, cursor) do
            {:ok, _tok, state, cursor} -> sync_to_quoted_identifier_end(state, cursor)
            {:eof, state, cursor} -> {state, cursor}
            {:error, _reason, state, cursor} -> {state, cursor}
          end
        end

      {:eof, cursor} ->
        {state, cursor}

      {:error, _diag, cursor} ->
        case TokenAdapter.next(state, cursor) do
          {:ok, _tok, state, cursor} -> sync_to_quoted_identifier_end(state, cursor)
          {:eof, state, cursor} -> {state, cursor}
          {:error, _reason, state, cursor} -> {state, cursor}
        end
    end
  end

  defp recover_missing_terminator(%State{} = state, cursor, start_meta) do
    line = Keyword.get(start_meta, :line, 1)
    column = Keyword.get(start_meta, :column, 1)
    %{offset: offset} = Position.to_location(line, column, state.line_index)
    size = byte_size(state.source)

    if offset >= size do
      {state, cursor}
    else
      rest = binary_part(state.source, offset, size - offset)

      case :binary.match(rest, "\n") do
        {idx, 1} when idx + 1 < byte_size(rest) ->
          suffix = binary_part(rest, idx + 1, byte_size(rest) - idx - 1)
          suffix_cursor = Cursor.new(suffix, Keyword.put(state.opts, :line, line + 1) |> Keyword.put(:column, 1))
          tokens = lex_suffix_tokens([], suffix_cursor)
          TokenAdapter.pushback_many(state, cursor, tokens)

        _ ->
          {state, cursor}
      end
    end
  end

  defp lex_suffix_tokens(acc, suffix_cursor) do
    case Cursor.next(suffix_cursor) do
      {:ok, tok, suffix_cursor} ->
        lex_suffix_tokens([tok | acc], suffix_cursor)

      {:eof, _suffix_cursor} ->
        Enum.reverse(acc)

      {:error, _reason, _suffix_cursor} ->
        Enum.reverse(acc)
    end
  end

  defp build_missing_terminator_node(error_tok, start_meta, %State{} = state, cursor) do
    synthetic? = synthetic_meta?(start_meta, state)

    payload =
      case State.error_token_diagnostic(state, TokenAdapter.meta(error_tok)) do
        %Error{} = diagnostic ->
          Error.error_node_payload(diagnostic,
            kind: :token,
            original: elem(error_tok, 2),
            synthetic?: synthetic?
          )

        _ ->
          elem(error_tok, 2)
      end

    {line, column, _} = error_anchor(start_meta, state, cursor)

    error_meta =
      [line: line, column: column, toxic: %{synthetic?: synthetic?, anchor: %{line: line, column: column}}]
    Builder.Helpers.error(payload, error_meta)
  end

  defp error_anchor(meta, %State{} = state, cursor) do
    {line, column} =
      case meta do
        {{line, column}, _, _} -> {line, column}
        meta when is_list(meta) -> {Keyword.get(meta, :line), Keyword.get(meta, :column)}
        _ -> {nil, nil}
      end

    synthetic? = synthetic_meta?(meta, state)

    {line, column} =
      case {line, column} do
        {nil, nil} -> Cursor.position(cursor)
        {line, column} -> {line || 1, column || 1}
      end

    {line, column, synthetic?}
  end

  defp synthetic_meta?(meta, %State{} = state) do
    missing? =
      case meta do
        {{_, _}, _, _} -> false
        meta when is_list(meta) -> Keyword.get(meta, :line) == nil and Keyword.get(meta, :column) == nil
        _ -> true
      end

    missing? or not Keyword.get(state.opts, :token_metadata, true)
  end
end
