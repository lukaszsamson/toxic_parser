defmodule ToxicParser.Grammar.Bitstrings do
  @moduledoc """
  Parsing for bitstring literals (<<>>).
  """

  alias ToxicParser.{Builder, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.Expressions

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @spec parse(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse(%State{} = state, ctx, %EventLog{} = log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    case TokenAdapter.peek(state) do
      {:ok, %{kind: :">>"}, state} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        {:ok, {:"<<>>", [], []}, state, log}

      _ ->
        with {:ok, parts, state, log} <- parse_segments([], state, ctx, log),
             {:ok, _close, state} <- expect(state, :">>") do
          {:ok, {:"<<>>", [], Enum.reverse(parts)}, state, log}
        end
    end
  end

  defp parse_segments(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :">>"}, _} ->
        {:ok, acc, state, log}

      {:ok, _tok, _} ->
        with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :","}, state} ->
              {:ok, _comma, state} = TokenAdapter.next(state)
              parse_segments([expr | acc], state, ctx, log)

            {:ok, %{kind: :">>"}, _} ->
              parse_segments([expr | acc], state, ctx, log)

            _ ->
              {:error, {:expected_comma_or, :">>"}, state, log}
          end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp expect(state, kind) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: ^kind}, state} -> {:ok, kind, state}
      {:ok, token, state} -> {:error, {:expected, kind, got: token.kind}, state}
      {:eof, state} -> {:error, :unexpected_eof, state}
      {:error, diag, state} -> {:error, diag, state}
    end
  end
end
