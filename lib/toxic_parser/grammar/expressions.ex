defmodule ToxicParser.Grammar.Expressions do
  @moduledoc """
  Expression dispatcher for matched/unmatched/no-parens contexts.
  """

  alias ToxicParser.{Builder, EventLog, Pratt, State, TokenAdapter}
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
        collect_exprs([first], state, ctx, log)

      {:error, :unexpected_eof, state, log} ->
        # No expressions found - return empty block with appropriate metadata
        ast = build_empty_block(leading_eoe_meta)
        {:ok, ast, state, log}

      {:error, reason, state, log} ->
        {:error, reason, state, log}
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
            case Calls.parse(state, ctx, log) do
              {:ok, ast, state, log} -> {:ok, ast, state, log}
              {:error, reason, state, log} -> {:error, reason, state, log}
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
                collect_exprs([next_expr | acc], state, ctx, log)

              {:error, _reason, state, log} ->
                finalize_exprs(acc, state, log)
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
end
