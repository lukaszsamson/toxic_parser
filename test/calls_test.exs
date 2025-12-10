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
end
