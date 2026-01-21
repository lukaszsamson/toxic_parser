defmodule ToxicParser.Grammar.Expressions do
  @moduledoc """
  Expression dispatcher for matched/unmatched/no-parens contexts.
  """

  alias ToxicParser.{
    Builder,
    Context,
    Cursor,
    Error,
    EventLog,
    Pratt,
    Recovery,
    State,
    TokenAdapter
  }

  alias ToxicParser.Grammar.{Blocks, Calls, Containers, EOE, Keywords}

  @type result ::
          {:ok, Macro.t(), State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}

  @doc """
  Parses a list of expressions separated by logical EOE.

  Handles the grammar rules:
  - grammar -> '$empty' : {'__block__', [], []}.
  - grammar -> eoe : {'__block__', meta_from_token('$1'), []}.
  - grammar -> expr_list : build_block(reverse('$1')).
  - grammar -> eoe expr_list : build_block(reverse('$2')).
  - grammar -> expr_list eoe : build_block(reverse(annotate_eoe('$2', '$1'))).
  - grammar -> eoe expr_list eoe : build_block(reverse(annotate_eoe('$3', '$2'))).
  """
  @spec expr_list(State.t(), Cursor.t(), Pratt.context(), EventLog.t()) :: result()
  def expr_list(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log) do
    # If input is empty (or only EOE), return empty block early
    {state, cursor, leading_eoe_meta} = EOE.skip_with_meta(state, cursor)

    case Cursor.peek(cursor) do
      {:eof, cursor} ->
        ast = build_empty_block(leading_eoe_meta)
        {:ok, ast, state, cursor, log}

      {:error, diag, cursor} ->
        if state.mode == :tolerant do
          recover_expr_error([], diag, state, cursor, ctx, log)
        else
          {:error, diag, state, cursor, log}
        end

      {:ok, _, cursor} ->
        # Proceed with expression parsing
        {ref, checkpoint_state} = TokenAdapter.checkpoint(state, cursor)

        case expr(checkpoint_state, cursor, ctx, log) do
          {:ok, first, state, cursor, log} ->
            if state.mode == :tolerant and error_node?(first) and
                 cursor_line_after_error?(first, cursor) and
                 not error_node_from_lexer?(first) do
              {state, cursor} = TokenAdapter.rewind(state, ref)

              reason = error_node_reason(first)
              recover_expr_error([], reason, state, cursor, ctx, log)
            else
              state = TokenAdapter.drop_checkpoint(state, ref)
              # Check for trailing EOE and annotate expression
              {first, state, cursor} = maybe_annotate_eoe(first, state, cursor)
              collect_exprs([first], state, cursor, ctx, log)
            end

          {:error, :unexpected_eof, state, cursor, log} ->
            state = TokenAdapter.drop_checkpoint(state, ref)
            {:error, :unexpected_eof, state, cursor, log}

          {:error, reason, state, cursor, log} ->
            state = TokenAdapter.drop_checkpoint(state, ref)
            recover_expr_error([], reason, state, cursor, ctx, log)
        end
    end
  end

  # Build empty block with metadata from first EOE token if present
  defp build_empty_block(meta), do: {:__block__, meta, []}

  @doc """
  Dispatches to the Pratt parser based on expression context.
  """
  @spec expr(State.t(), Cursor.t(), Pratt.context(), EventLog.t()) :: result()
  def expr(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log) do
    parse_with_layers(state, cursor, ctx, log)
  end

  @doc "Parses a matched expression (no trailing do-block attachment)."
  @spec matched_expr(State.t(), Cursor.t(), EventLog.t()) :: result()
  def matched_expr(%State{} = state, cursor, %EventLog{} = log) do
    parse_with_layers(state, cursor, Context.matched_expr(), log)
  end

  @doc "Parses an unmatched expression (do-block capable context)."
  @spec unmatched_expr(State.t(), Cursor.t(), EventLog.t()) :: result()
  def unmatched_expr(%State{} = state, cursor, %EventLog{} = log) do
    parse_with_layers(state, cursor, Context.unmatched_expr(), log)
  end

  @doc "Parses a no-parens expression (call-ambiguous context)."
  @spec no_parens_expr(State.t(), Cursor.t(), EventLog.t()) :: result()
  def no_parens_expr(%State{} = state, cursor, %EventLog{} = log) do
    parse_with_layers(state, cursor, Context.no_parens_expr(), log)
  end

  defp parse_with_layers(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log) do
    case Blocks.parse(state, cursor, ctx, log) do
      {:ok, ast, state, cursor, log} ->
        # Continue with led() to handle trailing binary operators like `fn -> a end ** b`
        Pratt.led(ast, state, cursor, log, 0, ctx)

      {:error, reason, state, cursor, log} ->
        maybe_recover_inline_error(reason, state, cursor, ctx, log)

      {:no_block, state, cursor} ->
        case Containers.parse(state, cursor, ctx, log) do
          {:ok, ast, state, cursor, log} ->
            handle_literal_encoder(ast, state, cursor, log)

          {:error, reason, state, cursor, log} ->
            maybe_recover_inline_error(reason, state, cursor, ctx, log)

          {:no_container, state, cursor} ->
            case ToxicParser.Grammar.Strings.parse(state, cursor, ctx, log) do
              {:ok, ast, state, cursor, log} ->
                handle_literal_encoder(ast, state, cursor, log)

              {:keyword_key, key_atom, key_meta, state, cursor, log} ->
                {state, cursor} = EOE.skip(state, cursor)

                with {:ok, value_ast, state, cursor, log} <-
                       expr(state, cursor, keyword_value_context(ctx), log) do
                  key_ast = ToxicParser.Builder.Helpers.literal(key_atom, key_meta, state)
                  handle_literal_encoder([{key_ast, value_ast}], state, cursor, log)
                end

              {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, cursor, log} ->
                {state, cursor} = EOE.skip(state, cursor)

                with {:ok, value_ast, state, cursor, log} <-
                       expr(state, cursor, keyword_value_context(ctx), log) do
                  key_ast = build_interpolated_keyword_key(parts, kind, start_meta, delimiter)
                  handle_literal_encoder([{key_ast, value_ast}], state, cursor, log)
                end

              {:no_string, state, cursor} ->
                case Calls.parse(state, cursor, ctx, log) do
                  {:ok, ast, state, cursor, log} ->
                    handle_literal_encoder(ast, state, cursor, log)

                  {:keyword_key, key_atom, key_meta, state, cursor, log} ->
                    {state, cursor} = EOE.skip(state, cursor)

                    with {:ok, value_ast, state, cursor, log} <-
                           expr(state, cursor, keyword_value_context(ctx), log) do
                      key_ast = ToxicParser.Builder.Helpers.literal(key_atom, key_meta, state)
                      handle_literal_encoder([{key_ast, value_ast}], state, cursor, log)
                    end

                  {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, cursor,
                   log} ->
                    {state, cursor} = EOE.skip(state, cursor)

                    with {:ok, value_ast, state, cursor, log} <-
                           expr(state, cursor, keyword_value_context(ctx), log) do
                      key_ast = build_interpolated_keyword_key(parts, kind, start_meta, delimiter)
                      handle_literal_encoder([{key_ast, value_ast}], state, cursor, log)
                    end

                  {:error, reason, state, cursor, log} ->
                    maybe_recover_inline_error(reason, state, cursor, ctx, log)
                end

              {:error, reason, state, cursor, log} ->
                maybe_recover_inline_error(reason, state, cursor, ctx, log)
            end
        end
    end
  end

  defp collect_exprs(acc, state, cursor, ctx, log) do
    case Cursor.peek(cursor) do
      {:ok, {kind, _meta, _value}, cursor} when kind in [:eol, :";"] ->
        # Skip all consecutive separator tokens (handles cases like "1\n;2")
        {state, cursor} = EOE.skip(state, cursor)

        # Check if we've reached EOF or terminator after trailing EOE
        case Cursor.peek(cursor) do
          {:eof, cursor} ->
            finalize_exprs(acc, state, cursor, log)

          # Stop at terminators that shouldn't start new expressions
          {:ok, {kind, _meta, _value}, cursor} ->
            case kind do
              kind when kind in [:end_interpolation, :"}"] ->
                finalize_exprs(acc, state, cursor, log)

              _ ->
                case expr(state, cursor, ctx, log) do
                  {:ok, next_expr, state, cursor, log} ->
                    # Annotate expression with trailing EOE if present
                    {next_expr, state, cursor} = maybe_annotate_eoe(next_expr, state, cursor)
                    collect_exprs([next_expr | acc], state, cursor, ctx, log)

                  {:error, reason, state, cursor, log} ->
                    recover_expr_error(acc, reason, state, cursor, ctx, log)
                end
            end

          {:error, diag, cursor} ->
            {:error, diag, state, cursor, log}
        end

      {:eof, cursor} ->
        finalize_exprs(acc, state, cursor, log)

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}

      {:ok, token, cursor} ->
        cond do
          should_continue_after_error?(acc, state, cursor) or
              should_parse_error_token?(state, token) ->
            case expr(state, cursor, ctx, log) do
              {:ok, next_expr, state, cursor, log} ->
                {next_expr, state, cursor} = maybe_annotate_eoe(next_expr, state, cursor)
                collect_exprs([next_expr | acc], state, cursor, ctx, log)

              {:error, reason, state, cursor, log} ->
                recover_expr_error(acc, reason, state, cursor, ctx, log)
            end

          should_sync_after_error_expr?(acc, state, token) ->
            case Recovery.sync_expr(state, cursor, log) do
              {:ok, state, cursor, log} ->
                case Cursor.peek(cursor) do
                  {:ok, {kind, _meta, _value}, cursor} when kind in [:eol, :";"] ->
                    {:ok, _sep, state, cursor} = TokenAdapter.next(state, cursor)
                    collect_exprs(acc, state, cursor, ctx, log)

                  {:ok, {:",", _meta, _value}, cursor} ->
                    {:ok, _sep, state, cursor} = TokenAdapter.next(state, cursor)
                    collect_exprs(acc, state, cursor, ctx, log)

                  {:ok, {kind, _meta, _value} = tok, cursor}
                  when kind in [:end_interpolation, :"}", :"]", :")", :end] ->
                    with [last | _] <- acc,
                         error_line when is_integer(error_line) <- error_expr_line(last),
                         token_line when is_integer(token_line) <- TokenAdapter.line(tok),
                         true <- token_line == error_line,
                         true <- kind in [:")", :"]", :"}", :end] do
                      {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)
                      collect_exprs(acc, state, cursor, ctx, log)
                    else
                      _ -> finalize_exprs(acc, state, cursor, log)
                    end

                  {:ok, _tok, cursor} ->
                    collect_exprs(acc, state, cursor, ctx, log)

                  {:eof, cursor} ->
                    finalize_exprs(acc, state, cursor, log)

                  {:error, diag, cursor} ->
                    {:error, diag, state, cursor, log}
                end
            end

          true ->
            case {state.mode, TokenAdapter.kind(token)} do
              {:tolerant, kind}
              when kind in [:kw_identifier, :kw_identifier_safe, :kw_identifier_unsafe] ->
                recover_expr_error(
                  acc,
                  Keywords.invalid_kw_identifier_error(state, cursor, kind),
                  state,
                  cursor,
                  ctx,
                  log
                )

              _ ->
                finalize_exprs(acc, state, cursor, log)
            end
        end
    end
  end

  defp finalize_exprs([{:unquote_splicing, _meta, [_]} = single], state, cursor, log) do
    {:ok, {:__block__, [], [single]}, state, cursor, log}
  end

  defp finalize_exprs([single], state, cursor, log), do: {:ok, single, state, cursor, log}

  defp finalize_exprs(many, state, cursor, log) do
    exprs =
      many
      |> Enum.reverse()
      |> Enum.map(fn
        {:__block__, [], [{:unquote_splicing, _meta, [_]} = u]} -> u
        other -> other
      end)

    block = {:__block__, [], exprs}
    {:ok, block, state, cursor, log}
  end

  defp handle_literal_encoder({:literal_encoder_error, reason, meta}, state, cursor, log) do
    location = Keyword.take(meta, [:line, :column])
    {:error, {location, to_string(reason) <> ": ", "literal"}, state, cursor, log}
  end

  defp handle_literal_encoder(ast, state, cursor, log) when is_list(ast) do
    case Enum.find(ast, &match?({:literal_encoder_error, _, _}, &1)) do
      {:literal_encoder_error, reason, meta} ->
        {:error, {meta, to_string(reason) <> ": ", "literal"}, state, cursor, log}

      _ ->
        {:ok, ast, state, cursor, log}
    end
  end

  defp handle_literal_encoder(ast, state, cursor, log) do
    {:ok, ast, state, cursor, log}
  end

  # Annotate an expression with end_of_expression metadata if followed by separator.
  # This implements the `annotate_eoe` pattern from elixir_parser.yrl.
  @doc false
  def maybe_annotate_eoe(ast, state, cursor) do
    case Cursor.peek(cursor) do
      {:ok, {kind, _meta, _value} = sep_token, cursor} when kind in [:eol, :";"] ->
        sep_meta = EOE.build_sep_meta(sep_token)
        annotated = EOE.annotate_eoe(ast, sep_meta)
        {annotated, state, cursor}

      {:ok, _, cursor} ->
        {ast, state, cursor}

      {:eof, cursor} ->
        {ast, state, cursor}

      {:error, _, cursor} ->
        {ast, state, cursor}
    end
  end

  defp recover_expr_error(acc, reason, %State{mode: :tolerant} = state, cursor, ctx, log) do
    {error_ast, state} = build_error_node(reason, state, cursor)

    with {:ok, state, cursor, log} <- Recovery.sync_expr(state, cursor, log) do
      case Cursor.peek(cursor) do
        {:ok, {kind, _meta, _value}, cursor} when kind in [:eol, :";"] ->
          # consume separator to make progress before continuing
          {:ok, _sep, state, cursor} = TokenAdapter.next(state, cursor)
          collect_exprs([error_ast | acc], state, cursor, ctx, log)

        {:ok, _, cursor} ->
          collect_exprs([error_ast | acc], state, cursor, ctx, log)

        {:eof, cursor} ->
          finalize_exprs([error_ast | acc], state, cursor, log)

        {:error, diag, cursor} ->
          {:error, diag, state, cursor, log}
      end
    end
  end

  defp recover_expr_error(_acc, reason, state, cursor, _ctx, log) do
    {:error, reason, state, cursor, log}
  end

  defp maybe_recover_inline_error(reason, %State{mode: :tolerant} = state, cursor, _ctx, log) do
    {error_ast, state} = build_error_node(reason, state, cursor)
    {state, cursor} = maybe_pushback_last_token(reason, state, cursor)
    {:ok, error_ast, state, cursor, log}
  end

  defp maybe_recover_inline_error(reason, state, cursor, _ctx, log) do
    {:error, reason, state, cursor, log}
  end

  defp maybe_pushback_last_token(:unexpected_eof, %State{} = state, cursor) do
    case Cursor.peek(cursor) do
      {:eof, _cursor} ->
        case Cursor.last_token(cursor) do
          nil -> {state, cursor}
          last_tok -> TokenAdapter.pushback(state, cursor, last_tok)
        end

      _ ->
        {state, cursor}
    end
  end

  defp maybe_pushback_last_token(_reason, state, cursor), do: {state, cursor}

  defp build_error_node(reason, %State{} = state, cursor) do
    {token_meta, range_meta} =
      case Cursor.peek(cursor) do
        {:ok, tok, _cursor} -> {TokenAdapter.token_meta(tok), TokenAdapter.meta(tok)}
        _ ->
          case Cursor.last_token(cursor) do
            nil -> {[], nil}
            last_tok -> {TokenAdapter.token_meta(last_tok), TokenAdapter.meta(last_tok)}
          end
      end

    token_meta = adjust_error_meta(token_meta, reason, cursor)

    {diagnostic, state} = parser_diagnostic(reason, range_meta, state, cursor)

    payload =
      Error.error_node_payload(diagnostic,
        kind: :invalid,
        original: reason,
        synthetic?: synthetic_error_node?(range_meta)
      )

    meta = maybe_mark_synthetic(token_meta, cursor, range_meta)
    {Builder.Helpers.error(payload, meta), state}
  end

  defp parser_diagnostic(reason, range_meta, %State{} = state, cursor) do
    {id, state} = State.next_diagnostic_id(state)

    position =
      case range_meta do
        nil ->
          {line, column} = error_anchor_position(reason, cursor)
          {{line, column}, {line, column}}

        _ ->
          nil
      end

    diagnostic =
      Error.from_parser(range_meta, reason,
        line_index: state.line_index,
        source: state.source,
        position: position
      )
      |> Error.annotate(%{
        id: id,
        anchor: %{kind: :error_node, path: [], note: nil},
        synthetic?: synthetic_error_node?(range_meta),
        lexer_error_code: nil
      })

    diagnostic = %{diagnostic | details: Map.put(diagnostic.details, :source, :grammar)}
    state = %{state | diagnostics: [diagnostic | state.diagnostics]}
    {diagnostic, state}
  end

  defp synthetic_error_node?(nil), do: true
  defp synthetic_error_node?(_meta), do: false

  defp maybe_mark_synthetic(meta, cursor, range_meta) do
    {line, column} =
      case {Keyword.get(meta, :line), Keyword.get(meta, :column)} do
        {nil, nil} -> Cursor.position(cursor)
        {line, column} -> {line || 1, column || 1}
      end

    toxic_meta = %{
      synthetic?: synthetic_error_node?(range_meta),
      anchor: %{line: line, column: column}
    }

    Keyword.put(meta, :toxic, toxic_meta)
  end

  defp should_continue_after_error?(acc, %State{mode: :tolerant} = _state, cursor) do
    case acc do
      [last | _] ->
        with error_line when is_integer(error_line) <- error_expr_line(last),
             {:ok, tok, _cursor} <- Cursor.peek(cursor),
             token_line when is_integer(token_line) <- TokenAdapter.line(tok) do
          token_line > error_line
        else
          _ -> false
        end

      _ ->
        false
    end
  end

  defp should_continue_after_error?(_acc, _state, _cursor), do: false

  defp should_parse_error_token?(%State{mode: :tolerant}, token) do
    TokenAdapter.kind(token) == :error_token
  end

  defp should_parse_error_token?(_state, _token), do: false

  defp adjust_error_meta(meta, :unexpected_eof, cursor) do
    {line, column} = Cursor.position(cursor)

    if is_integer(line) and is_integer(column) and line > 1 do
      Keyword.merge(meta, [line: line - 1, column: 1])
    else
      meta
    end
  end

  defp adjust_error_meta(meta, _reason, _cursor), do: meta

  defp error_anchor_position(:unexpected_eof, cursor) do
    {line, column} = Cursor.position(cursor)

    if is_integer(line) and is_integer(column) and line > 1 do
      {line - 1, 1}
    else
      {line, column}
    end
  end

  defp error_anchor_position(_reason, cursor), do: Cursor.position(cursor)

  defp should_sync_after_error_expr?([last | _], %State{mode: :tolerant}, token) do
    with error_line when is_integer(error_line) <- error_expr_line(last),
         token_line when is_integer(token_line) <- TokenAdapter.line(token) do
      token_line == error_line
    else
      _ -> false
    end
  end

  defp should_sync_after_error_expr?(_acc, _state, _token), do: false

  defp error_expr_line({:__error__, meta, _payload}) do
    Keyword.get(meta, :line)
  end

  defp error_expr_line({{:__error__, meta, _payload}, _call_meta, _args}) do
    Keyword.get(meta, :line)
  end

  defp error_expr_line(_other), do: nil

  defp error_node?({:__error__, _meta, _payload}), do: true
  defp error_node?(_other), do: false

  defp error_node_reason({:__error__, _meta, [payload]}) when is_map(payload) do
    Map.get(payload, :original)
  end

  defp error_node_reason({:__error__, _meta, payload}) when is_map(payload) do
    Map.get(payload, :original)
  end

  defp error_node_reason(_other), do: :unexpected_eof

  defp error_node_from_lexer?(error_node) do
    case error_node_reason(error_node) do
      %Toxic.Error{} -> true
      _ -> false
    end
  end

  defp cursor_line_after_error?(error_node, cursor) do
    {line, _column} = Cursor.position(cursor)

    case error_expr_line(error_node) do
      error_line when is_integer(error_line) -> line > error_line
      _ -> false
    end
  end

  @doc """
  Builds an AST node for an interpolated keyword key like "foo\#{x}":
  Returns {:., meta, [:erlang, :binary_to_atom]} call.

  Elixir produces: {{:., meta, [:erlang, :binary_to_atom]}, call_meta, [binary, :utf8]}
  where binary is {:<<>>, meta, [parts...]}
  """
  @spec build_interpolated_keyword_key(list(), atom(), keyword(), String.t()) :: Macro.t()
  def build_interpolated_keyword_key(parts, kind, start_meta, delimiter) do
    # Convert parts to binary parts for {:<<>>, ...}
    binary_parts = parts_to_binary(parts, kind, start_meta)
    binary_ast = {:<<>>, start_meta, binary_parts}

    # Build the :erlang.binary_to_atom call
    dot_ast = {:., start_meta, [:erlang, :binary_to_atom]}
    call_meta = [{:delimiter, delimiter}, {:format, :keyword} | start_meta]

    {dot_ast, call_meta, [binary_ast, :utf8]}
  end

  # Convert string parts to binary AST parts
  # Each part is either {:fragment, content} or {:interpolation, ast}
  # The interpolation ast already has the full structure with to_string and ::binary
  defp parts_to_binary(parts, kind, start_meta) do
    Enum.flat_map(parts, fn
      {:fragment, content} ->
        if content == "", do: [], else: [content]

      {:interpolation, ast} ->
        # Keyword keys always become binaries. Charlist interpolations need ::binary.
        [normalize_keyword_interpolation(ast, kind, start_meta)]
    end)
  end

  defp normalize_keyword_interpolation({:"::", _meta, _parts} = ast, _kind, _start_meta), do: ast

  defp normalize_keyword_interpolation(ast, kind, start_meta)
       when kind in [:charlist, :heredoc_charlist] do
    meta = interpolation_meta(ast, start_meta)
    {:"::", meta, [ast, {:binary, meta, nil}]}
  end

  defp normalize_keyword_interpolation(ast, _kind, _start_meta), do: ast

  defp interpolation_meta({{:., meta, _}, _call_meta, _args}, _fallback), do: meta
  defp interpolation_meta({_fun, meta, _args}, _fallback), do: meta
  defp interpolation_meta(_ast, fallback), do: fallback

  # Determine keyword value context for quoted keyword keys like "a":
  #
  # elixir_parser.yrl:
  #   kw_base -> kw_eol container_expr
  #   container_expr -> matched_expr | unmatched_expr | no_parens_expr (error)
  defp keyword_value_context(%Context{} = ctx) do
    case ctx.allow_do_block do
      true -> Context.container_expr()
      false -> Context.kw_no_parens_value()
    end
  end
end
