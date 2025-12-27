defmodule ToxicParser.DotsTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Context, Grammar, TokenAdapter, EventLog}
  import ExUnit.Assertions

  test "dot identifiers and aliases" do
    {state, cursor} = TokenAdapter.new("foo.bar")
    log = EventLog.new()
    assert {:ok, ast, _state, _cursor, _log} = Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)
    assert Macro.to_string(ast) =~ "bar"

    {state, cursor} = TokenAdapter.new("Foo.Bar")
    assert {:ok, ast, _state, _cursor, _log} = Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)
    assert Macro.to_string(ast) =~ "Bar"
  end

  test "dot calls" do
    {state, cursor} = TokenAdapter.new("foo.bar(1)")
    log = EventLog.new()
    assert {:ok, ast, _state, _cursor, _log} = Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)
    assert Macro.to_string(ast) =~ "bar(1)"

    {state, cursor} = TokenAdapter.new("foo.(1)")

    assert {:ok, _ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)

    {state, cursor} = TokenAdapter.new("foo.()")
    assert {:ok, ast, _state, _cursor, _log} = Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)
    assert Macro.to_string(ast) =~ "foo"

    {state, cursor} = TokenAdapter.new("foo.bar().baz")
    assert {:ok, ast, _state, _cursor, _log} = Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)
    assert Macro.to_string(ast) =~ "baz"
  end

  test "dot curly call rejects initial kw_data" do
    log = EventLog.new()

    {state, cursor} = TokenAdapter.new("foo.{a: 1}")

    assert {:error, {[_ | _], "syntax error before: ", "a"}, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)

    {state, cursor} = TokenAdapter.new(~s(foo.{"a": 1}))

    assert {:error, {[_ | _], "syntax error before: ", "a"}, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)
  end
end
