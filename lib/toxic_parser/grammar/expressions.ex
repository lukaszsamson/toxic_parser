defmodule ToxicParser.Grammar.Expressions do
  @moduledoc """
  Expression dispatcher for matched/unmatched/no-parens contexts.
  """

  alias ToxicParser.{Builder, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.{Calls, Containers}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @doc """
  Parses a list of expressions separated by logical EOE.

  Currently parses a single expression and returns it; full expr_list logic will
  be expanded in later phases.
  """
  @spec expr_list(State.t(), Pratt.context(), EventLog.t()) :: result()
  def expr_list(%State{} = state, ctx, %EventLog{} = log) do
    {state, _log} = skip_eoe(state, log)

    case expr(state, ctx, log) do
      {:ok, first, state, log} ->
        collect_exprs([first], state, ctx, log)

      {:error, :unexpected_eof, state, log} ->
        {:ok, Builder.Helpers.literal(:ok), state, log}

      {:error, reason, state, log} ->
        {:error, reason, state, log}
    end
  end

  @doc """
  Dispatches to the Pratt parser based on expression context.
  """
  @spec expr(State.t(), Pratt.context(), EventLog.t()) :: result()
  def expr(%State{} = state, ctx, %EventLog{} = log) do
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

  defp collect_exprs(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe}, state} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)

        case expr(state, ctx, log) do
          {:ok, next_expr, state, log} ->
            collect_exprs([next_expr | acc], state, ctx, log)

          {:error, _reason, state, log} ->
            finalize_exprs(acc, state, log)
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

  defp skip_eoe(state, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe}, state} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe(state, log)

      _ ->
        {state, log}
    end
  end
end
