defmodule ToxicParser.Grammar.Dots do
  @moduledoc """
  Dot expression parsing helpers (`foo.bar`, `Foo.Bar`, `foo.(...)`, `foo.()`).
  """

  alias ToxicParser.{Context, EventLog, Identifiers, Pratt, Result, State, TokenAdapter}
  alias ToxicParser.Builder.{Helpers, Meta}
  alias ToxicParser.Grammar.{Delimited, EOE, Expressions, Keywords}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @doc """
  Parse a dot call `expr.(...)` when the current token is `:dot_call_op`.
  """
  @spec parse_dot_call(Macro.t(), State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_dot_call(left, %State{} = state, %Context{} = ctx, %EventLog{} = log) do
    {:ok, dot_tok, state} = TokenAdapter.next(state)
    dot_meta = Helpers.token_meta(dot_tok)

    # dot_call_op is immediately followed by (
    {:ok, _open, state} = TokenAdapter.next(state)

    # Skip leading EOE and count newlines
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    with {:ok, args, state, log} <-
           ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, ctx, log),
         {:ok, close_meta, trailing_newlines, state} <- Meta.consume_closing(state, :")") do
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      call_meta = Meta.closing_meta(dot_meta, close_meta, total_newlines)

      {:ok, {{:., dot_meta, [left]}, call_meta, Enum.reverse(args)}, state, log}
    else
      other -> Result.normalize_error(other, log)
    end
  end

  @doc """
  Parses the RHS of a dot: identifier/alias/call/bracket/paren call.
  Returns `{:ok, {member_value, member_meta}, state, log}` for simple identifiers,
  or `{:ok, call_ast, state, log}` for calls.

  For simple identifiers, also returns `:no_parens_call` flag when the identifier
  was classified as :op_identifier, indicating that a
  no-parens call is expected.

  The optional `dot_meta` parameter provides the dot's metadata for curly calls
  where the call metadata should use the dot's position.
  """
  @spec parse_member(State.t(), Pratt.context(), EventLog.t(), keyword()) :: result()
  def parse_member(state, ctx, log, dot_meta \\ [])

  def parse_member(%State{} = state, %Context{} = ctx, %EventLog{} = log, dot_meta) do
    case TokenAdapter.next(state) do
      {:ok, tok, state} ->
        tok_kind = TokenAdapter.kind(tok)

        case Identifiers.classify(tok_kind) do
          kind
          when kind in [
                 :identifier,
                 :do_identifier
               ] ->
            {:ok, {TokenAdapter.value(tok), Helpers.token_meta(tok)}, state, log}

          :bracket_identifier ->
            # Bracket identifier - bracket access IS allowed (no whitespace before [)
            {:ok, {TokenAdapter.value(tok), Helpers.token_meta(tok), :allows_bracket}, state, log}

          # op_identifier indicates no-parens call is expected
          :op_identifier ->
            # Return with :no_parens_call tag to indicate caller should expect no-parens args
            {:ok, {TokenAdapter.value(tok), Helpers.token_meta(tok), :no_parens_call}, state, log}

          :alias ->
            # Alias needs to be wrapped as __aliases__
            alias_ast = Helpers.from_token(tok)

            case TokenAdapter.peek(state) do
              {:ok, {:"[", _}, _} ->
                {:ok, {alias_ast, :allows_bracket}, state, log}

              _ ->
                {:ok, alias_ast, state, log}
            end

          :paren_identifier ->
            parse_paren_call(tok, state, ctx, log)

          _ ->
            cond do
              # Check for quoted identifier: D."foo"
              tok_kind == :quoted_identifier_start ->
                parse_quoted_identifier(tok, state, ctx, log)

              # Handle curly container call: n.{} or n.{a, b}
              # Grammar: dot_alias -> matched_expr dot_op open_curly container_args close_curly
              tok_kind == :"{" ->
                parse_curly_call(tok, state, ctx, log, dot_meta)

              true ->
                {:error, {:expected, :dot_member, got: tok_kind}, state, log}
            end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_paren_call(tok, state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    # Skip leading EOE and count newlines
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    with {:ok, args, state, log} <-
           ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, ctx, log),
         {:ok, close_meta, trailing_newlines, state} <- Meta.consume_closing(state, :")") do
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      callee_meta = Helpers.token_meta(tok)
      meta = Meta.closing_meta(callee_meta, close_meta, total_newlines)

      # Use just the atom value for the callee, not the full AST form
      # Dot call RHS should be :bar, not {:bar, meta, nil}
      callee = TokenAdapter.value(tok)
      {:ok, {callee, meta, Enum.reverse(args)}, state, log}
    else
      other -> Result.normalize_error(other, log)
    end
  end

  # Parse curly container call: n.{} or n.{a, b}
  # Grammar: dot_alias -> matched_expr dot_op open_curly container_args close_curly
  # Returns {:ok, {:{}, meta, args}, state, log} where :{} is the atom function name
  # The dot_meta parameter provides the dot's metadata for the call metadata (column should be dot's position)
  defp parse_curly_call(open_tok, state, _ctx, log, dot_meta) do
    # Skip leading EOE and count newlines
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    container_ctx = Context.container_expr()

    with {:ok, state, log} <- reject_initial_kw_data(state, container_ctx, log),
         {:ok, tagged_items, state, log} <- parse_curly_args(state, container_ctx, log),
         {:ok, close_meta, trailing_newlines, state} <- Meta.consume_closing(state, :"}") do
      args = finalize_curly_items(tagged_items)
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      # Use dot's metadata for the call column (not the curly's position)
      # If dot_meta is empty (no dot context), fall back to open_tok metadata
      base_meta =
        if dot_meta == [], do: Helpers.token_meta(open_tok), else: dot_meta

      meta = Meta.closing_meta(base_meta, close_meta, total_newlines)

      # Return call format with :{} as the function name
      # This matches {name, meta, args} pattern in the caller
      {:ok, {:{}, meta, args}, state, log}
    else
      other -> Result.normalize_error(other, log)
    end
  end

  defp parse_curly_args(state, container_ctx, log) do
    item_fun = fn state, _ctx, log ->
      case Keywords.try_parse_kw_data(state, container_ctx, log) do
        {:ok, kw_list, state, log} ->
          {state, _newlines} = EOE.skip_count_newlines(state, 0)

          case TokenAdapter.peek(state) do
            {:ok, {:"}", _}, _} ->
              {:ok, {:kw_data, kw_list}, state, log}

            {:ok, tok, state} ->
              {:error, {:expected, :"}", got: TokenAdapter.kind(tok)}, state, log}

            {:eof, state} ->
              {:error, :unexpected_eof, state, log}

            {:error, diag, state} ->
              {:error, diag, state, log}
          end

        {:no_kw, state, log} ->
          with {:ok, expr, state, log} <- Expressions.expr(state, container_ctx, log) do
            {:ok, {:expr, expr}, state, log}
          end

        {:error, reason, state, log} ->
          {:error, reason, state, log}
      end
    end

    Delimited.parse_comma_separated(state, container_ctx, log, :"}", item_fun, allow_empty?: true)
  end

  defp reject_initial_kw_data(state, container_ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, {:"}", _}, state} ->
        {:ok, state, log}

      {:ok, tok, state} ->
        cond do
          # Definite kw_data start - no need to checkpoint/parse.
          Keywords.starts_kw?(tok) ->
            meta = Helpers.token_meta(tok)
            tok_value = TokenAdapter.value(tok)
            {:error, syntax_error_before(meta, Atom.to_string(tok_value)), state, log}

          # Quoted key may or may not be kw_data; only this case needs checkpoint.
          Keywords.starts_kw_or_quoted_key?(tok) ->
            {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

            case Keywords.try_parse_kw_data(checkpoint_state, container_ctx, log) do
              {:ok, kw_list, state, log} ->
                state = TokenAdapter.rewind(state, ref)
                meta = Helpers.token_meta(tok)
                token_display = kw_data_first_key_display(kw_list)
                {:error, syntax_error_before(meta, token_display), state, log}

              {:no_kw, state, log} ->
                {:ok, TokenAdapter.rewind(state, ref), log}

              {:error, reason, state, log} ->
                {:error, reason, TokenAdapter.rewind(state, ref), log}
            end

          true ->
            {:ok, state, log}
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
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
  defp parse_quoted_identifier(start_tok, state, ctx, log) do
    start_meta = Helpers.token_meta(start_tok)
    delimiter = delimiter_from_value(TokenAdapter.value(start_tok))

    with {:ok, fragments, end_kind, state, log} <-
           collect_fragments([], state, :quoted_identifier_end, log),
         {:ok, _close, state} <- TokenAdapter.next(state) do
      content = fragments |> Enum.reverse() |> Enum.join("") |> Macro.unescape_string()
      atom = String.to_atom(content)

      # Build metadata with delimiter
      meta_with_delimiter = [{:delimiter, delimiter} | start_meta]

      cond do
        # quoted_op_identifier_end means there's a space before next token -> no-parens call
        end_kind == :quoted_op_identifier_end ->
          # D."foo" arg - parse no-parens call arguments
          with {:ok, args, state, log} <-
                 ToxicParser.Grammar.Calls.parse_no_parens_args([], state, ctx, log) do
            # Return as call AST: {atom, meta, args}
            {:ok, {atom, meta_with_delimiter, args}, state, log}
          end

        # quoted_bracket_identifier_end: D."foo"[1] - return with :allows_bracket flag
        # so caller knows bracket access is allowed
        end_kind == :quoted_bracket_identifier_end ->
          {:ok, {atom, meta_with_delimiter, :allows_bracket}, state, log}

        # quoted_do_identifier_end: D."foo" do...end - return as call AST, do-block parsed by caller
        end_kind == :quoted_do_identifier_end ->
          # Return as call expression {atom, meta, []} with no_parens: true for do-block attachment
          meta_with_no_parens = [{:no_parens, true} | meta_with_delimiter]
          {:ok, {atom, meta_with_no_parens, []}, state, log}

        # quoted_paren_identifier_end or ( immediately follows: D."foo"()
        end_kind == :quoted_paren_identifier_end or
            match?({:ok, {:"(", _}, _}, TokenAdapter.peek(state)) ->
          {:ok, _open, state} = TokenAdapter.next(state)

          # Skip leading EOE and count newlines
          {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

          with {:ok, args, state, log} <-
                 ToxicParser.Grammar.CallsPrivate.parse_paren_args(
                   [],
                   state,
                   Context.matched_expr(),
                   log
                 ),
               {:ok, close_meta, trailing_newlines, state} <-
                 Meta.consume_closing(state, :")") do
            total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, true)
            call_meta = Meta.closing_meta(meta_with_delimiter, close_meta, total_newlines)

            # Return as call AST: {atom, call_meta, args}
            {:ok, {atom, call_meta, Enum.reverse(args)}, state, log}
          else
            other -> Result.normalize_error(other, log)
          end

        true ->
          # Simple quoted identifier without parens
          {:ok, {atom, meta_with_delimiter}, state, log}
      end
    end
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

  defp collect_fragments(acc, state, target_end, log) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        tok_kind = TokenAdapter.kind(tok)

        cond do
          tok_kind == target_end ->
            {:ok, acc, target_end, state, log}

          tok_kind in @quoted_id_ends and target_end == :quoted_identifier_end ->
            # Accept any quoted identifier end token
            {:ok, acc, tok_kind, state, log}

          tok_kind == :string_fragment ->
            {:ok, _frag, state} = TokenAdapter.next(state)
            collect_fragments([TokenAdapter.value(tok) | acc], state, target_end, log)

          true ->
            {:error, {:unexpected_string_token, tok_kind}, state, log}
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp delimiter_from_value(?"), do: "\""
  defp delimiter_from_value(?'), do: "'"
end
