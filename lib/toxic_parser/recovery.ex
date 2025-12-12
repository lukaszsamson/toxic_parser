defmodule ToxicParser.Recovery do
  @moduledoc """
  Minimal recovery helpers for skipping to synchronization points.
  """

  alias ToxicParser.{EventLog, State, TokenAdapter}

  @expr_sync [:eoe, :"}", :"]", :")", :end]

  @spec sync_expr(State.t(), EventLog.t()) :: {:ok, State.t(), EventLog.t()}
  def sync_expr(%State{} = state, %EventLog{} = log) do
    skip_until(state, @expr_sync, log)
  end

  defp skip_until(state, kinds, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: kind}, _} ->
        if kind in kinds do
          {:ok, state, log}
        else
          case TokenAdapter.next(state) do
            {:ok, _t, state} -> skip_until(state, kinds, log)
            {:eof, state} -> {:ok, state, log}
            {:error, _reason, state} -> {:ok, state, log}
          end
        end

      {:eof, state} ->
        {:ok, state, log}

      {:error, _diag, state} ->
        {:ok, state, log}
    end
  end
end
