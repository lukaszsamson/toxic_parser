defmodule ToxicParser.Grammar.Expressions do
  @moduledoc """
  Expression dispatcher for matched/unmatched/no-parens contexts.
  """

  alias ToxicParser.{Builder, EventLog, Pratt, Recovery, State, TokenAdapter}
  alias ToxicParser.Grammar.{Blocks, Calls, Containers}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

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
  @spec expr_list(State.t(), Pratt.context(), EventLog.t()) :: result()
  def expr_list(%State{} = state, ctx, %EventLog{} = log) do
    # Track if we consumed any leading EOE tokens for metadata
    {state, leading_eoe_meta} = skip_eoe_with_meta(state)

    case expr(state, ctx, log) do
      {:ok, first, state, log} ->
        # Check for trailing EOE and annotate expression
        {first, state} = maybe_annotate_eoe(first, state)
        collect_exprs([first], state, ctx, log)

      {:error, :unexpected_eof, state, log} ->
        # No expressions found - return empty block with appropriate metadata
        ast = build_empty_block(leading_eoe_meta)
        {:ok, ast, state, log}

      {:error, reason, state, log} ->
        recover_expr_error([], reason, state, ctx, log)
    end
  end

  # Build empty block with metadata from first EOE token if present
  defp build_empty_block(nil), do: {:__block__, [], []}
  defp build_empty_block(meta), do: {:__block__, meta, []}

  @doc """
  Dispatches to the Pratt parser based on expression context.
  """
  @spec expr(State.t(), Pratt.context(), EventLog.t()) :: result()
  def expr(%State{} = state, ctx, %EventLog{} = log) do
    case ctx do
      :matched -> matched_expr(state, log)
      :unmatched -> unmatched_expr(state, log)
      :no_parens -> no_parens_expr(state, log)
      _ -> matched_expr(state, log)
    end
  end

  @doc "Parses a matched expression (no trailing do-block attachment)."
  @spec matched_expr(State.t(), EventLog.t()) :: result()
  def matched_expr(%State{} = state, %EventLog{} = log) do
    parse_with_layers(state, :matched, log)
  end

  @doc "Parses an unmatched expression (do-block capable context)."
  @spec unmatched_expr(State.t(), EventLog.t()) :: result()
  def unmatched_expr(%State{} = state, %EventLog{} = log) do
    parse_with_layers(state, :unmatched, log)
  end

  @doc "Parses a no-parens expression (call-ambiguous context)."
  @spec no_parens_expr(State.t(), EventLog.t()) :: result()
  def no_parens_expr(%State{} = state, %EventLog{} = log) do
    parse_with_layers(state, :no_parens, log)
  end

  defp parse_with_layers(state, ctx, log) do
    case Blocks.parse(state, ctx, log) do
      {:ok, ast, state, log} ->
        {:ok, ast, state, log}

      {:error, reason, state, log} ->
        {:error, reason, state, log}

      {:no_block, state} ->
        case Containers.parse(state, ctx, log) do
          {:ok, ast, state, log} ->
            {:ok, ast, state, log}

          {:error, reason, state, log} ->
            {:error, reason, state, log}

          {:no_container, state} ->
            case ToxicParser.Grammar.Strings.parse(state, ctx, log) do
              {:ok, ast, state, log} ->
                {:ok, ast, state, log}

              # String was a quoted keyword key like "a": - parse value and return as keyword pair
              {:keyword_key, key_atom, state, log} ->
                with {:ok, value_ast, state, log} <- expr(state, :matched, log) do
                  {:ok, [{key_atom, value_ast}], state, log}
                end

              {:no_string, state} ->
                case Calls.parse(state, ctx, log) do
                  {:ok, ast, state, log} -> {:ok, ast, state, log}
                  {:error, reason, state, log} -> {:error, reason, state, log}
                end

              {:error, reason, state, log} ->
                {:error, reason, state, log}
            end
        end
    end
  end

  defp collect_exprs(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe}, _} ->
        # Skip all consecutive EOE tokens (handles cases like "1\n;2")
        state = skip_eoe(state)

        # Check if we've reached EOF after trailing EOE
        case TokenAdapter.peek(state) do
          {:eof, state} ->
            finalize_exprs(acc, state, log)

          {:ok, _, _} ->
            case expr(state, ctx, log) do
              {:ok, next_expr, state, log} ->
                # Annotate expression with trailing EOE if present
                {next_expr, state} = maybe_annotate_eoe(next_expr, state)
                collect_exprs([next_expr | acc], state, ctx, log)

              {:error, reason, state, log} ->
                recover_expr_error(acc, reason, state, ctx, log)
            end

          {:error, diag, state} ->
            {:error, diag, state, log}
        end

      {:eof, state} ->
        finalize_exprs(acc, state, log)

      {:error, diag, state} ->
        {:error, diag, state, log}

      _ ->
        finalize_exprs(acc, state, log)
    end
  end

  defp finalize_exprs([single], state, log), do: {:ok, single, state, log}

  defp finalize_exprs(many, state, log) do
    block = Builder.Helpers.literal({:__block__, [], Enum.reverse(many)})
    {:ok, block, state, log}
  end

  # Skip all EOE tokens
  defp skip_eoe(state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe(state)

      _ ->
        state
    end
  end

  # Skip EOE tokens but capture metadata from first one
  defp skip_eoe_with_meta(state, first_meta \\ nil) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, metadata: meta}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        # Capture metadata from first EOE token only
        new_meta = first_meta || token_to_meta(meta)
        skip_eoe_with_meta(state, new_meta)

      _ ->
        {state, first_meta}
    end
  end

  defp token_to_meta(%{range: %{start: %{line: line, column: column}}}) do
    [line: line, column: column]
  end

  defp token_to_meta(_), do: []

  # Annotate an expression with end_of_expression metadata if followed by EOE.
  # This implements the `annotate_eoe` pattern from elixir_parser.yrl.
  defp maybe_annotate_eoe(ast, state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe} = eoe_token, _} ->
        eoe_meta = build_eoe_meta(eoe_token)
        annotated = annotate_eoe(ast, eoe_meta)
        {annotated, state}

      _ ->
        {ast, state}
    end
  end

  # Build end_of_expression metadata from an EOE token.
  # Format: [newlines: N, line: L, column: C] when newlines count is available
  # Based on elixir_parser.yrl end_of_expression/1:
  # - Always include newlines if it's an integer (including 0)
  # - Otherwise just include line/column
  defp build_eoe_meta(%{kind: :eoe, value: %{newlines: newlines}, metadata: meta})
       when is_integer(newlines) do
    case meta do
      %{range: %{start: %{line: line, column: column}}} ->
        [newlines: newlines, line: line, column: column]

      _ ->
        []
    end
  end

  defp build_eoe_meta(%{kind: :eoe, metadata: meta}) do
    case meta do
      %{range: %{start: %{line: line, column: column}}} ->
        [line: line, column: column]

      _ ->
        []
    end
  end

  defp build_eoe_meta(_), do: []

  defp recover_expr_error(acc, reason, %State{mode: :tolerant} = state, ctx, log) do
    error_ast = build_error_node(reason, state)

    with {:ok, state, log} <- Recovery.sync_expr(state, log) do
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :eoe}, _} ->
          # consume separator to make progress before continuing
          {:ok, _eoe, state} = TokenAdapter.next(state)
          collect_exprs([error_ast | acc], state, ctx, log)

        _ ->
          collect_exprs([error_ast | acc], state, ctx, log)
      end
    end
  end

  defp recover_expr_error(_acc, reason, state, _ctx, log) do
    {:error, reason, state, log}
  end

  defp build_error_node(reason, state) do
    meta =
      case TokenAdapter.peek(state) do
        {:ok, tok, _} -> Builder.Helpers.token_meta(tok.metadata)
        _ -> []
    end

    Builder.Helpers.error(reason, meta)
  end

  # Annotate an AST node with end_of_expression metadata.
  # Based on elixir_parser.yrl annotate_eoe/2:
  # - For stab expressions, annotate the RHS body
  # - For other 3-tuples with list metadata, prepend end_of_expression
  # - Skip stab operators themselves (they handle their own EOE)
  defp annotate_eoe({:->, stab_meta, [stab_args, {left, meta, right}]}, eoe_meta)
       when is_list(meta) do
    {:->, stab_meta, [stab_args, {left, [{:end_of_expression, eoe_meta} | meta], right}]}
  end

  defp annotate_eoe({left, meta, right}, eoe_meta) when is_list(meta) and left != :-> do
    {left, [{:end_of_expression, eoe_meta} | meta], right}
  end

  defp annotate_eoe(ast, _eoe_meta), do: ast
end
