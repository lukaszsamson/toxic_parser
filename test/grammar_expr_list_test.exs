defmodule ToxicParser.GrammarExprListTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Context, Grammar, TokenAdapter, EventLog}

  test "handles leading and trailing eoe with empty input" do
    state = TokenAdapter.new("\n\n")
    log = EventLog.new()

    assert {:ok, {:__block__, _, []}, _state, _log} =
             Grammar.Expressions.expr_list(state, Context.matched_expr(), log)
  end

  test "parses multiple expressions into a block" do
    state = TokenAdapter.new("1\n2\n3")
    log = EventLog.new()

    assert {:ok, {:__block__, [], [one, two, three]}, _state, _log} =
             Grammar.Expressions.expr_list(state, Context.matched_expr(), log)

    assert Macro.to_string(one) == "1"
    assert Macro.to_string(two) == "2"
    assert Macro.to_string(three) == "3"
  end

  test "tolerant mode turns lexer errors into error nodes" do
    state = TokenAdapter.new(")\n2", mode: :tolerant)
    log = EventLog.new()

    assert {:ok, {:__block__, _, [error_node | _]}, _state, _log} =
             Grammar.Expressions.expr_list(state, Context.matched_expr(), log)

    # The error node seems to be wrapped in a call structure in this case
    assert {{:__error__, _, _}, _, []} = error_node
  end
end
