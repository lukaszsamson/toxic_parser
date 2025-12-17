defmodule ToxicParser.StringsTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Context, Grammar, TokenAdapter, EventLog}

  test "parses binary string" do
    state = TokenAdapter.new(~S("abc"))
    log = EventLog.new()
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, Context.matched_expr(), log)
    assert ast == "abc"
  end

  test "parses charlist" do
    state = TokenAdapter.new(~S('abc'))
    log = EventLog.new()
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, Context.matched_expr(), log)
    assert ast == ~c'abc'
  end

  test "parses sigil" do
    state = TokenAdapter.new("~r/foo/")
    log = EventLog.new()
    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, Context.matched_expr(), log)
    assert match?({:sigil_r, _, [_content, _mods]}, ast)
  end
end
