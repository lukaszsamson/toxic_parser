defmodule ToxicParser.BlocksTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Grammar, EventLog, TokenAdapter}

  test "parses zero-arity fn blocks" do
    state = TokenAdapter.new("fn -> :ok end")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :matched, log)
    assert {:fn, _, [{:->, _, [[], :ok]}]} = ast
  end

  test "parses fn with args and guard" do
    state = TokenAdapter.new("fn x, y when y > 0 -> x end")
    log = EventLog.new()

    assert {:ok,
            {:fn, _,
             [
               {:->, _,
                [
                  [{:when, _, [{:x, _, nil}, {:y, _, nil}, {:>, _, [{:y, _, nil}, 0]}]}],
                  {:x, _, nil}
                ]}
             ]}, _state, _log} =
             Grammar.Expressions.expr(state, :unmatched, log)
  end

  test "attaches do-block to identifier call" do
    state = TokenAdapter.new("foo do :ok end")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :unmatched, log)
    assert {:foo, _, [[do: :ok]]} = ast
  end

  test "parses simple case do block" do
    state = TokenAdapter.new("case x do x -> :ok end")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :unmatched, log)
    assert {:case, _, [{:x, _, nil}, [do: [{:->, _, [[{:x, _, nil}], :ok]}]]]} = ast
  end

  test "parses try with rescue and after" do
    state = TokenAdapter.new("try do :ok rescue e -> e after :after end")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :unmatched, log)

    assert {:try, _,
            [[do: :ok, rescue: [{:->, _, [[{:e, _, nil}], {:e, _, nil}]}], after: :after]]} = ast
  end

  test "parses with generator clauses and else" do
    state = TokenAdapter.new("with x <- y do :ok else x -> :err end")
    log = EventLog.new()

    assert {:ok, ast, _state, _log} = Grammar.Expressions.expr(state, :unmatched, log)

    assert {:with, _,
            [
              {:<-, _, [{:x, _, nil}, {:y, _, nil}]},
              [do: :ok, else: [{:->, _, [[{:x, _, nil}], :err]}]]
            ]} =
             ast
  end

  test "parses cond" do
    state = TokenAdapter.new("cond do true -> :ok end")
    log = EventLog.new()

    assert {:ok, {:cond, _, [[do: [{:->, _, [[true], :ok]}]]]}, _state, _log} =
             Grammar.Expressions.expr(state, :unmatched, log)
  end

  test "parses receive with after" do
    state = TokenAdapter.new("receive do msg -> msg after 10 -> :timeout end")
    log = EventLog.new()

    assert {:ok,
            {:receive, _,
             [
               [
                 do: [{:->, _, [[{:msg, _, nil}], {:msg, _, nil}]}],
                 after: [{:->, _, [[10], :timeout]}]
               ]
             ]}, _state, _log} =
             Grammar.Expressions.expr(state, :unmatched, log)
  end

  test "parses for comprehension with filter" do
    state = TokenAdapter.new("for x <- [1,2], x > 1 do x end")
    log = EventLog.new()

    assert {:ok, {:for, _, quals}, _state, _log} =
             Grammar.Expressions.expr(state, :unmatched, log)

    assert [{:<-, _, [{:x, _, nil}, [1, 2]]}, {:>, _, [{:x, _, nil}, 1]}, [do: {:x, _, nil}]] =
             quals
  end
end
