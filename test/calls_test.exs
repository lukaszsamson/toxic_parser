defmodule ToxicParser.CallsTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Grammar, TokenAdapter, EventLog}

  test "parses empty paren call" do
    state = TokenAdapter.new("foo()")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert ast == {:foo, [], []}
  end

  test "parses paren call with args" do
    state = TokenAdapter.new("foo(1, 2)")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert ast == {:foo, [], [1, 2]}
  end

  test "parses nested paren calls" do
    state = TokenAdapter.new("foo(bar(1))")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert ast == {:foo, [], [{:bar, [], [1]}]}
  end

  test "parses simple no-parens calls" do
    state = TokenAdapter.new("foo 1")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert Macro.to_string(ast) == "foo(1)"

    state = TokenAdapter.new("foo 1, 2")
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert Macro.to_string(ast) == "foo(1, 2)"
  end

  test "parses no-parens call with keywords" do
    state = TokenAdapter.new("foo a: 1")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert Macro.to_string(ast) in ["foo([a: 1])", "foo([[a: 1]])"]

    state = TokenAdapter.new("foo 1, a: 2")
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert Macro.to_string(ast) in ["foo(1, [a: 2])", "foo(1, [[a: 2]])"]
  end

  test "parses nested no-parens calls" do
    state = TokenAdapter.new("foo bar 1, 2")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert Macro.to_string(ast) == "foo(bar(1, 2))"

    state = TokenAdapter.new("foo -bar 1, 2")
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert Macro.to_string(ast) =~ "bar(1, 2)"
  end
end
