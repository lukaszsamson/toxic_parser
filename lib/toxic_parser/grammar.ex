defmodule ToxicParser.Grammar do
  @moduledoc """
  Grammar entry points (recursive descent dispatcher) that delegate expression
  parsing to the Pratt core. Phase 4 starts with a minimal expr_list/expr
  dispatcher; full grammar nonterminals land in later phases.
  """

  alias ToxicParser.{Context, Cursor, EventLog, Position, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.Expressions

  @type result ::
          {:ok, Macro.t(), State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}

  @doc """
  Parses the given source string using the grammar dispatcher.

  This does not yet integrate with the public `ToxicParser.parse_string/2` API
  to avoid breaking Phase 0 harness; use explicitly for incremental bring-up.
  """
  @spec parse_string(String.t(), keyword()) :: result()
  def parse_string(source, opts \\ []) when is_binary(source) do
    {state, cursor} = TokenAdapter.new(source, opts)
    log = EventLog.new() |> EventLog.start_node(:root, zero_meta())

    with {:ok, ast, state, cursor, log} <- Expressions.expr_list(state, cursor, Context.expr(), log) do
      log = EventLog.end_node(log, :root, zero_meta())
      {:ok, ast, state, cursor, log}
    end
  end

  @doc "Entry point for already-initialized parser state."
  @spec parse(State.t(), Cursor.t(), EventLog.t(), Pratt.context()) :: result()
  def parse(%State{} = state, cursor, %EventLog{} = log, %Context{} = ctx \\ Context.expr()) do
    Expressions.expr_list(state, cursor, ctx, log)
  end

  defp zero_meta do
    %{
      range: %{start: Position.to_location(1, 1, {0}), end: Position.to_location(1, 1, {0})},
      delimiter: nil,
      newlines: 0,
      synthesized?: false,
      terminators: [],
      role: :none
    }
  end
end
