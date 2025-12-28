defmodule ToxicParser.Grammar.Expressions do
  @moduledoc """
  Expression dispatcher for matched/unmatched/no-parens contexts.
  """

  alias ToxicParser.{Builder, Context, Cursor, EventLog, Pratt, Recovery, State, TokenAdapter}
  alias ToxicParser.Grammar.{Blocks, Calls, Containers, EOE}

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
      {:eof, _cursor} ->
        ast = build_empty_block(leading_eoe_meta)
        {:ok, ast, state, cursor, log}

      {:error, diag, _cursor} ->
        {:error, diag, state, cursor, log}

      _ ->
        # Proceed with expression parsing
        case expr(state, cursor, ctx, log) do
          {:ok, first, state, cursor, log} ->
            # Check for trailing EOE and annotate expression
            {first, state, cursor} = maybe_annotate_eoe(first, state, cursor)
            collect_exprs([first], state, cursor, ctx, log)

          {:error, :unexpected_eof, state, cursor, log} ->
            {:error, :unexpected_eof, state, cursor, log}

          {:error, reason, state, cursor, log} ->
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
        {:error, reason, state, cursor, log}

      {:no_block, state, cursor} ->
        case Containers.parse(state, cursor, ctx, log) do
          {:ok, ast, state, cursor, log} ->
            {:ok, ast, state, cursor, log}

          {:error, reason, state, cursor, log} ->
            {:error, reason, state, cursor, log}

          {:no_container, state, cursor} ->
            case ToxicParser.Grammar.Strings.parse(state, cursor, ctx, log) do
              {:ok, ast, state, cursor, log} ->
                {:ok, ast, state, cursor, log}

              {:keyword_key, key_atom, key_meta, state, cursor, log} ->
                {state, cursor} = EOE.skip(state, cursor)

                with {:ok, value_ast, state, cursor, log} <-
                       expr(state, cursor, keyword_value_context(ctx), log) do
                  key_ast = ToxicParser.Builder.Helpers.literal(key_atom, key_meta, state)
                  {:ok, [{key_ast, value_ast}], state, cursor, log}
                end

              {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, cursor, log} ->
                {state, cursor} = EOE.skip(state, cursor)

                with {:ok, value_ast, state, cursor, log} <-
                       expr(state, cursor, keyword_value_context(ctx), log) do
                  key_ast = build_interpolated_keyword_key(parts, kind, start_meta, delimiter)
                  {:ok, [{key_ast, value_ast}], state, cursor, log}
                end

              {:no_string, state, cursor} ->
                case Calls.parse(state, cursor, ctx, log) do
                  {:ok, ast, state, cursor, log} ->
                    {:ok, ast, state, cursor, log}

                  {:keyword_key, key_atom, key_meta, state, cursor, log} ->
                    {state, cursor} = EOE.skip(state, cursor)

                    with {:ok, value_ast, state, cursor, log} <-
                           expr(state, cursor, keyword_value_context(ctx), log) do
                      key_ast = ToxicParser.Builder.Helpers.literal(key_atom, key_meta, state)
                      {:ok, [{key_ast, value_ast}], state, cursor, log}
                    end

                  {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, cursor,
                   log} ->
                    {state, cursor} = EOE.skip(state, cursor)

                    with {:ok, value_ast, state, cursor, log} <-
                           expr(state, cursor, keyword_value_context(ctx), log) do
                      key_ast = build_interpolated_keyword_key(parts, kind, start_meta, delimiter)
                      {:ok, [{key_ast, value_ast}], state, cursor, log}
                    end

                  {:error, reason, state, cursor, log} ->
                    {:error, reason, state, cursor, log}
                end

              {:error, reason, state, cursor, log} ->
                {:error, reason, state, cursor, log}
            end
        end
    end
  end

  defp collect_exprs(acc, state, cursor, ctx, log) do
    case Cursor.peek(cursor) do
      {:ok, {kind, _meta, _value}, _cursor} when kind in [:eol, :";"] ->
        # Skip all consecutive separator tokens (handles cases like "1\n;2")
        {state, cursor} = EOE.skip(state, cursor)

        # Check if we've reached EOF or terminator after trailing EOE
        case Cursor.peek(cursor) do
          {:eof, _cursor} ->
            finalize_exprs(acc, state, cursor, log)

          # Stop at terminators that shouldn't start new expressions
          {:ok, {kind, _meta, _value}, _cursor} ->
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

          {:error, diag, _cursor} ->
            {:error, diag, state, cursor, log}
        end

      {:eof, _cursor} ->
        finalize_exprs(acc, state, cursor, log)

      {:error, diag, _cursor} ->
        {:error, diag, state, cursor, log}

      _ ->
        finalize_exprs(acc, state, cursor, log)
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

    block = Builder.Helpers.literal({:__block__, [], exprs})
    {:ok, block, state, cursor, log}
  end

  # Annotate an expression with end_of_expression metadata if followed by separator.
  # This implements the `annotate_eoe` pattern from elixir_parser.yrl.
  defp maybe_annotate_eoe(ast, state, cursor) do
    case Cursor.peek(cursor) do
      {:ok, {kind, _meta, _value} = sep_token, _cursor} when kind in [:eol, :";"] ->
        sep_meta = EOE.build_sep_meta(sep_token)
        annotated = EOE.annotate_eoe(ast, sep_meta)
        {annotated, state, cursor}

      _ ->
        {ast, state, cursor}
    end
  end

  defp recover_expr_error(acc, reason, %State{mode: :tolerant} = state, cursor, ctx, log) do
    error_ast = build_error_node(reason, state, cursor)

    with {:ok, state, cursor, log} <- Recovery.sync_expr(state, cursor, log) do
      case Cursor.peek(cursor) do
        {:ok, {kind, _meta, _value}, _cursor} when kind in [:eol, :";"] ->
          # consume separator to make progress before continuing
          {:ok, _sep, state, cursor} = TokenAdapter.next(state, cursor)
          collect_exprs([error_ast | acc], state, cursor, ctx, log)

        _ ->
          collect_exprs([error_ast | acc], state, cursor, ctx, log)
      end
    end
  end

  defp recover_expr_error(_acc, reason, state, cursor, _ctx, log) do
    {:error, reason, state, cursor, log}
  end

  defp build_error_node(reason, _state, cursor) do
    meta =
      case Cursor.peek(cursor) do
        {:ok, tok, _cursor} -> TokenAdapter.token_meta(tok)
        _ -> []
      end

    Builder.Helpers.error(reason, meta)
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
