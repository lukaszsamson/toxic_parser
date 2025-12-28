defmodule ToxicParser.BlocksTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Context, Grammar, EventLog, TokenAdapter}

  test "parses zero-arity fn blocks" do
    {state, cursor} = TokenAdapter.new("fn -> :ok end")
    log = EventLog.new()

    assert {:ok, ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.matched_expr(), log)

    assert {:fn, _, [{:->, _, [[], :ok]}]} = ast
  end

  test "parses fn with args and guard" do
    {state, cursor} = TokenAdapter.new("fn x, y when y > 0 -> x end")
    log = EventLog.new()

    assert {:ok,
            {:fn, _,
             [
               {:->, _,
                [
                  [{:when, _, [{:x, _, nil}, {:y, _, nil}, {:>, _, [{:y, _, nil}, 0]}]}],
                  {:x, _, nil}
                ]}
             ]}, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.unmatched_expr(), log)
  end

  test "attaches do-block to identifier call" do
    {state, cursor} = TokenAdapter.new("foo do :ok end")
    log = EventLog.new()

    assert {:ok, ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.unmatched_expr(), log)

    assert {:foo, _, [[do: :ok]]} = ast
  end

  test "parses simple case do block" do
    {state, cursor} = TokenAdapter.new("case x do x -> :ok end")
    log = EventLog.new()

    assert {:ok, ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.unmatched_expr(), log)

    assert {:case, _, [{:x, _, nil}, [do: [{:->, _, [[{:x, _, nil}], :ok]}]]]} = ast
  end

  test "parses try with rescue and after" do
    {state, cursor} = TokenAdapter.new("try do :ok rescue e -> e after :after end")
    log = EventLog.new()

    assert {:ok, ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.unmatched_expr(), log)

    assert {:try, _,
            [[do: :ok, rescue: [{:->, _, [[{:e, _, nil}], {:e, _, nil}]}], after: :after]]} = ast
  end

  test "parses with generator clauses and else" do
    {state, cursor} = TokenAdapter.new("with x <- y do :ok else x -> :err end")
    log = EventLog.new()

    assert {:ok, ast, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.unmatched_expr(), log)

    assert {:with, _,
            [
              {:<-, _, [{:x, _, nil}, {:y, _, nil}]},
              [do: :ok, else: [{:->, _, [[{:x, _, nil}], :err]}]]
            ]} =
             ast
  end

  test "parses cond" do
    {state, cursor} = TokenAdapter.new("cond do true -> :ok end")
    log = EventLog.new()

    assert {:ok, {:cond, _, [[do: [{:->, _, [[true], :ok]}]]]}, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.unmatched_expr(), log)
  end

  test "parses receive with after" do
    {state, cursor} = TokenAdapter.new("receive do msg -> msg after 10 -> :timeout end")
    log = EventLog.new()

    assert {:ok,
            {:receive, _,
             [
               [
                 do: [{:->, _, [[{:msg, _, nil}], {:msg, _, nil}]}],
                 after: [{:->, _, [[10], :timeout]}]
               ]
             ]}, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.unmatched_expr(), log)
  end

  test "parses for comprehension with filter" do
    {state, cursor} = TokenAdapter.new("for x <- [1,2], x > 1 do x end")
    log = EventLog.new()

    assert {:ok, {:for, _, quals}, _state, _cursor, _log} =
             Grammar.Expressions.expr(state, cursor, Context.unmatched_expr(), log)

    assert [{:<-, _, [{:x, _, nil}, [1, 2]]}, {:>, _, [{:x, _, nil}, 1]}, [do: {:x, _, nil}]] =
             quals
  end
end
