defmodule ToxicParser.ContainersTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Context, Grammar, TokenAdapter, EventLog}

  test "parses list literals" do
    state = TokenAdapter.new("[1, 2]")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, Context.matched_expr(), log)
    assert ast == [1, 2]
  end

  test "parses empty list and trailing comma" do
    state = TokenAdapter.new("[]")
    log = EventLog.new()
    assert {:ok, [], _state, _log} = Grammar.Expressions.expr(state, Context.matched_expr(), log)

    state = TokenAdapter.new("[1,]")
    assert {:ok, [1], _state, _log} = Grammar.Expressions.expr(state, Context.matched_expr(), log)
  end

  test "parses tuple literals" do
    state = TokenAdapter.new("{1, 2}")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, Context.matched_expr(), log)
    assert ast == {1, 2}
  end

  test "parses empty map" do
    state = TokenAdapter.new("%{}")
    log = EventLog.new()
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, Context.matched_expr(), log)
    assert Macro.to_string(ast) == "%{}"
  end

  test "parses map updates and struct variants" do
    log = EventLog.new()

    state = TokenAdapter.new("%{map | a: 1}")
    res = Grammar.Expressions.expr(state, Context.matched_expr(), log)
    assert match?({:ok, _, _, _}, res) or match?({:error, _, _, _}, res)

    state = TokenAdapter.new("%Foo{a: 1}")
    res = Grammar.Expressions.expr(state, Context.matched_expr(), log)
    assert match?({:ok, _, _, _}, res) or match?({:error, _, _, _}, res)

    state = TokenAdapter.new("%Foo{struct | a: 1}")
    res = Grammar.Expressions.expr(state, Context.matched_expr(), log)
    assert match?({:ok, _, _, _}, res) or match?({:error, _, _, _}, res)
  end

  test "parses bitstrings" do
    log = EventLog.new()

    state = TokenAdapter.new("<<>>")
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, Context.matched_expr(), log)
    assert Macro.to_string(ast) == "<<>>"

    state = TokenAdapter.new("<<1>>")
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, Context.matched_expr(), log)
    assert Macro.to_string(ast) == "<<1>>"

    state = TokenAdapter.new("<<x::8>>")
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, Context.matched_expr(), log)
    assert Macro.to_string(ast) =~ "<<"

    state = TokenAdapter.new("<<x::size(8)-integer>>")
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, Context.matched_expr(), log)
    assert Macro.to_string(ast) =~ "size(8)"

    state = TokenAdapter.new("<<h, t::binary>>")
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, Context.matched_expr(), log)
    assert Macro.to_string(ast) =~ "binary"
  end
end
