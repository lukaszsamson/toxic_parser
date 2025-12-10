defmodule ToxicParser.GrammarExprListTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Grammar, TokenAdapter, EventLog}

  test "handles leading and trailing eoe with empty input" do
    state = TokenAdapter.new("\n\n")
    log = EventLog.new()

    assert {:ok, :ok, _state, _log} = Grammar.Expressions.expr_list(state, :matched, log)
  end

  test "parses multiple expressions into a block" do
    state = TokenAdapter.new("1\n2\n3")
    log = EventLog.new()

    assert {:ok, {:__block__, [], [one, two, three]}, _state, _log} =
             Grammar.Expressions.expr_list(state, :matched, log)

    assert Macro.to_string(one) == "1"
    assert Macro.to_string(two) == "2"
    assert Macro.to_string(three) == "3"
  end
end
