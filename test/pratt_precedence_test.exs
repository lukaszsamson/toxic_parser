defmodule ToxicParser.PrattPrecedenceTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Context, Precedence, Pratt, TokenAdapter, EventLog}
  alias ToxicParser.Grammar

  test "binary binding powers include dot/not_in" do
    table = Precedence.binary_table()
    assert {_, _} = Map.get(table, :.)
    assert Precedence.not_in() == elem(Precedence.binary(:in_op), 0)
  end

  test "binary entries carry associativity" do
    assert {_bp, :right} = Map.get(Precedence.binary_table(), :match_op)
    assert {_bp, :left} = Map.get(Precedence.binary_table(), :dual_op)
  end

  test "unary binding powers include not and capture" do
    assert {_, :nonassoc} = Precedence.unary(:unary_op)
    assert {_, :nonassoc} = Precedence.unary(:capture_op)
  end

  test "pratt stub returns token value or EOF error" do
    {state, cursor} = TokenAdapter.new("1 + 2")
    log = EventLog.new()

    assert {:ok, ast, _state, _cursor, %EventLog{}} =
             Pratt.parse(state, cursor, Context.matched_expr(), log)

    assert match?({_, _, _}, ast)
  end

  test "grammar expr_list dispatches to pratt" do
    {state, cursor} = TokenAdapter.new("1 + 2\n3")
    log = EventLog.new()

    assert {:ok, ast, _state, _cursor, %EventLog{}} =
             Grammar.Expressions.expr_list(state, cursor, Context.matched_expr(), log)

    assert {:__block__, [], [_a, _b]} = ast
  end

  test "calls parser falls back to Pratt when identifier is not a call" do
    {state, cursor} = TokenAdapter.new("foo")
    log = EventLog.new()

    assert {:ok, ast, _state, _cursor, %EventLog{}} =
             Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)

    assert {:foo, _, nil} = ast
  end
end
