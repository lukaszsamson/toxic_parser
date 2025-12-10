defmodule ToxicParser.BlocksTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Grammar, EventLog, TokenAdapter}

  test "parses zero-arity fn blocks" do
    state = TokenAdapter.new("fn -> :ok end")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert {:fn, [], [{:->, [], [[], :ok]}]} = ast
  end

  test "parses fn with args and guard" do
    state = TokenAdapter.new("fn x, y when y > 0 -> x end")
    log = EventLog.new()

    assert {:ok, {:fn, [], [{:->, [], [[{:when, [], [:x, :y, {:rel_op, [], [:y, 0]}]}], :x]}]}, _state, _log} =
             Grammar.Expressions.expr(state, :matched, log)
  end

  test "attaches do-block to identifier call" do
    state = TokenAdapter.new("foo do :ok end")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert {:foo, [], [[do: :ok]]} = ast
  end

  test "parses simple case do block" do
    state = TokenAdapter.new("case x do x -> :ok end")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert {:case, [], [:x, [do: [{:->, [], [[:x], :ok]}]]]} = ast
  end

  test "parses try with rescue and after" do
    state = TokenAdapter.new("try do :ok rescue e -> e after :after end")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)

    assert {:try, [], [[do: :ok, rescue: [{:->, [], [[:e], :e]}], after: :after]]} = ast
  end

  test "parses with generator clauses and else" do
    state = TokenAdapter.new("with x <- y do :ok else x -> :err end")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)

    assert {:with, [], [{:<-, [], [:x, :y]}, [do: :ok, else: [{:->, [], [[:x], :err]}]]]} =
             ast
  end

  test "parses cond" do
    state = TokenAdapter.new("cond do true -> :ok end")
    log = EventLog.new()

    assert {:ok, {:cond, [], [[do: [{:->, [], [[true], :ok]}]]]}, _state, _log} =
             Grammar.Expressions.expr(state, :matched, log)
  end

  test "parses receive with after" do
    state = TokenAdapter.new("receive do msg -> msg after 10 -> :timeout end")
    log = EventLog.new()

    assert {:ok, {:receive, [], [[do: [{:->, [], [[:msg], :msg]}], after: [{:->, [], [[10], :timeout]}]]]}, _state, _log} =
             Grammar.Expressions.expr(state, :matched, log)
  end

  test "parses for comprehension with filter" do
    state = TokenAdapter.new("for x <- [1,2], x > 1 do x end")
    log = EventLog.new()

    assert {:ok, {:for, [], quals}, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert [{:<-, [], [:x, [1, 2]]}, _filter, [do: :x]] = quals
  end
end
