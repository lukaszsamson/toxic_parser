defmodule ToxicParser.DotsTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Grammar, TokenAdapter, EventLog}
  import ExUnit.Assertions

  test "dot identifiers and aliases" do
    state = TokenAdapter.new("foo.bar")
    log = EventLog.new()
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert Macro.to_string(ast) =~ "bar"

    state = TokenAdapter.new("Foo.Bar")
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert Macro.to_string(ast) =~ "Bar"
  end

  test "dot calls" do
    state = TokenAdapter.new("foo.bar(1)")
    log = EventLog.new()
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert Macro.to_string(ast) =~ "bar(1)"

    state = TokenAdapter.new("foo.(1)")
    assert {:ok, _ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)

    state = TokenAdapter.new("foo.()")
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert Macro.to_string(ast) =~ "foo"

    state = TokenAdapter.new("foo.bar().baz")
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert Macro.to_string(ast) =~ "baz"
  end
end
