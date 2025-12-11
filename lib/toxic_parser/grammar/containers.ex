defmodule ToxicParser.Grammar.Containers do
  @moduledoc """
  Container parsing for lists and tuples (Phase 6 scaffolding).
  """

  alias ToxicParser.{EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.{Expressions, Maps}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}
          | {:no_container, State.t()}

  @spec parse(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse(%State{} = state, ctx, %EventLog{} = log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"["}, _} ->
        parse_list(state, ctx, log)

      {:ok, %{kind: :"{", value: _}, _} ->
        parse_tuple(state, ctx, log)

      {:ok, %{kind: :%{}}, _} ->
        Maps.parse_map(state, ctx, log)

      {:ok, %{kind: :"%"}, _} ->
        Maps.parse_map(state, ctx, log)

      {:eof, state} ->
        {:no_container, state}

      {:error, _diag, state} ->
        {:no_container, state}

      _ ->
        {:no_container, state}
    end
  end

  defp parse_list(state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    with {:ok, elements, state, log} <- parse_elements([], :"]", state, ctx, log) do
      {:ok, Enum.reverse(elements), state, log}
    end
  end

  defp parse_tuple(state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    with {:ok, elements, state, log} <- parse_elements([], :"}", state, ctx, log) do
      ast =
        case elements do
          [] -> {:{}, [], []}
          list -> {:{}, [], Enum.reverse(list)}
        end

      {:ok, ast, state, log}
    end
  end

  defp parse_elements(acc, terminator, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: ^terminator}, state} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        {:ok, acc, state, log}

      {:ok, _tok, _state} ->
        with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :","}, state} ->
              {:ok, _comma, state} = TokenAdapter.next(state)
              parse_elements([expr | acc], terminator, state, ctx, log)

            {:ok, %{kind: ^terminator}, _} ->
              parse_elements([expr | acc], terminator, state, ctx, log)

            _ ->
              {:error, {:expected_comma_or, terminator}, state, log}
          end
        end

      {:eof, state} ->
        {:ok, acc, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end
end
