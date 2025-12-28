defmodule ToxicParser.CallsTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Context, Grammar, TokenAdapter, EventLog}

  test "parses empty paren call" do
    {state, cursor} = TokenAdapter.new("foo()")
    log = EventLog.new()

    assert {:ok, ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)

    assert {:foo, _, []} = ast
  end

  test "parses paren call with args" do
    {state, cursor} = TokenAdapter.new("foo(1, 2)")
    log = EventLog.new()

    assert {:ok, ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)

    assert {:foo, _, [1, 2]} = ast
  end

  test "parses nested paren calls" do
    {state, cursor} = TokenAdapter.new("foo(bar(1))")
    log = EventLog.new()

    assert {:ok, ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)

    assert {:foo, _, [{:bar, _, [1]}]} = ast
  end

  test "parses simple no-parens calls" do
    {state, cursor} = TokenAdapter.new("foo 1")
    log = EventLog.new()

    assert {:ok, ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)

    assert Macro.to_string(ast) == "foo(1)"

    {state, cursor} = TokenAdapter.new("foo 1, 2")

    assert {:ok, ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)

    assert Macro.to_string(ast) == "foo(1, 2)"
  end

  test "parses no-parens call with keywords" do
    {state, cursor} = TokenAdapter.new("foo a: 1")
    log = EventLog.new()

    assert {:ok, ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)

    assert Macro.to_string(ast) == "foo(a: 1)"

    {state, cursor} = TokenAdapter.new("foo 1, a: 2")

    assert {:ok, ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)

    assert Macro.to_string(ast) == "foo(1, a: 2)"
  end

  test "parses nested no-parens calls" do
    {state, cursor} = TokenAdapter.new("foo bar 1, 2")
    log = EventLog.new()

    assert {:ok, ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)

    assert Macro.to_string(ast) == "foo(bar(1, 2))"

    {state, cursor} = TokenAdapter.new("foo -bar 1, 2")

    assert {:ok, ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)

    assert Macro.to_string(ast) =~ "bar(1, 2)"
  end

  test "parses no-parens dot calls" do
    {state, cursor} = TokenAdapter.new("foo.bar 1, 2")
    log = EventLog.new()

    assert {:ok, ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)

    assert Macro.to_string(ast) =~ "bar"
  end
end
