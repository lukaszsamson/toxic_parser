defmodule ToxicParser.Recovery do
  @moduledoc """
  Minimal recovery helpers for skipping to synchronization points.
  """

  alias ToxicParser.{Cursor, EventLog, State, TokenAdapter}

  @expr_sync [:eol, :";", :"}", :"]", :")", :end]

  @spec sync_expr(State.t(), Cursor.t(), EventLog.t()) :: {:ok, State.t(), Cursor.t(), EventLog.t()}
  def sync_expr(%State{} = state, cursor, %EventLog{} = log) do
    skip_until(state, cursor, @expr_sync, log)
  end

  defp skip_until(state, cursor, kinds, log) do
    case TokenAdapter.peek(state, cursor) do
      {:ok, tok, _, _} ->
        if TokenAdapter.kind(tok) in kinds do
          {:ok, state, cursor, log}
        else
          case TokenAdapter.next(state, cursor) do
            {:ok, _t, state, cursor} -> skip_until(state, cursor, kinds, log)
            {:eof, state, cursor} -> {:ok, state, cursor, log}
            {:error, _reason, state, cursor} -> {:ok, state, cursor, log}
          end
        end

      {:eof, state, cursor} ->
        {:ok, state, cursor, log}

      {:error, _diag, state, cursor} ->
        {:ok, state, cursor, log}
    end
  end
end
