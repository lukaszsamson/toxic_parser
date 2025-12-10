defmodule ToxicParser.PrattPrecedenceTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Precedence, Pratt, TokenAdapter, EventLog}

  test "binary binding powers are ordered and include dot/access/not_in" do
    table = Precedence.binary_table()
    assert {:dot_op, _, _} = List.keyfind(table, :dot_op, 0)
    assert {:access_op, _, _} = List.keyfind(table, :access_op, 0)
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
    assert {_, :nonassoc} = Precedence.unary(:unary_not_op)
    assert {_, :nonassoc} = Precedence.unary(:capture_op)
  end

  test "pratt stub returns token value or EOF error" do
    state = TokenAdapter.new("1 + 2")
    log = EventLog.new()

    assert {:ok, ast, _state, %EventLog{}} = Pratt.parse(state, :matched, log)
    assert match?({_, _, _}, ast)
  end
end
