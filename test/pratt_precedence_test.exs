defmodule ToxicParser.PrattPrecedenceTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Precedence, Pratt, TokenAdapter, EventLog}
  alias ToxicParser.Grammar

  test "binary binding powers are ordered and include dot/not_in" do
    table = Precedence.binary_table()
    assert {:dot_op, _, _} = List.keyfind(table, :dot_op, 0)
    assert Precedence.not_in() == elem(Precedence.binary(:in_op), 0)

    # Ensure ordering is ascending by binding power
    bps = Enum.map(table, fn {_, bp, _} -> bp end)
    assert bps == Enum.sort(bps)
  end

  test "binary entries carry associativity" do
    assert {:match_op, _bp, :right} = List.keyfind(Precedence.binary_table(), :match_op, 0)
    assert {:dual_op, _bp, :left} = List.keyfind(Precedence.binary_table(), :dual_op, 0)
  end

  test "unary binding powers include not and capture" do
    assert {_, :nonassoc} = Precedence.unary(:unary_op)
    assert {_, :nonassoc} = Precedence.unary(:capture_op)
  end

  test "pratt stub returns token value or EOF error" do
    state = TokenAdapter.new("1 + 2")
    log = EventLog.new()

    assert {:ok, ast, _state, %EventLog{}} = Pratt.parse(state, :matched, log)
    assert match?({_, _, _}, ast)
  end

  test "grammar expr_list dispatches to pratt" do
    state = TokenAdapter.new("1 + 2\n3")
    log = EventLog.new()

    assert {:ok, ast, _state, %EventLog{}} = Grammar.Expressions.expr_list(state, :matched, log)
    assert {:__block__, [], [_a, _b]} = ast
  end

  test "calls parser falls back to Pratt when identifier is not a call" do
    state = TokenAdapter.new("foo")
    log = EventLog.new()

    assert {:ok, ast, _state, %EventLog{}} = Grammar.Expressions.expr(state, :matched, log)
    assert {:foo, _, nil} = ast
  end
end
