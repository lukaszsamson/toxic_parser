defmodule ToxicParser.GrammarExprListTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Context, Grammar, TokenAdapter, EventLog}

  test "handles leading and trailing eoe with empty input" do
    {state, cursor} = TokenAdapter.new("\n\n")
    log = EventLog.new()

    assert {:ok, {:__block__, _, []}, _state, _cursor, _log} =
             Grammar.Expressions.expr_list(state, cursor, Context.matched_expr(), log)
  end

  test "parses multiple expressions into a block" do
    {state, cursor} = TokenAdapter.new("1\n2\n3")
    log = EventLog.new()

    assert {:ok, {:__block__, [], [one, two, three]}, _state, _cursor, _log} =
             Grammar.Expressions.expr_list(state, cursor, Context.matched_expr(), log)

    assert Macro.to_string(one) == "1"
    assert Macro.to_string(two) == "2"
    assert Macro.to_string(three) == "3"
  end

  test "tolerant mode turns lexer errors into error nodes" do
    {state, cursor} = TokenAdapter.new(")\n2", mode: :tolerant)
    log = EventLog.new()

    assert {:ok, {:__block__, _, [error_node | _]}, _state, _cursor, _log} =
             Grammar.Expressions.expr_list(state, cursor, Context.matched_expr(), log)

    # Accept both direct error nodes and the wrapped form
    case error_node do
      {:__error__, _, _} -> :ok
      {{:__error__, _, _}, _, []} -> :ok
      other -> flunk("unexpected error node shape: #{inspect(other)}")
    end
  end
end
