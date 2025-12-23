defmodule ToxicParser.LayoutTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Layout, TokenAdapter}

  test "peek_sep returns view token for newlines and semicolons" do
    state = TokenAdapter.new("1\n\n;2")

    {:ok, _tok, state} = TokenAdapter.next(state)

    assert {:ok, {:eoe, _meta, %{source: :eol, newlines: 2}}, state} = Layout.peek_sep(state)
    {:ok, _tok, state} = TokenAdapter.next(state)
    assert {:ok, {:eoe, _meta, %{source: :semicolon, newlines: 0}}, _} = Layout.peek_sep(state)
  end

  test "skip_newlines_only counts only newline EOEs" do
    state = TokenAdapter.new("1\n\n;2")
    {:ok, _tok, state} = TokenAdapter.next(state)

    {state, count} = Layout.skip_newlines_only(state)
    assert count == 2
    # Semicolon remains - use tuple pattern
    assert {:ok, {:eoe, _, %{source: :semicolon}}, _} = TokenAdapter.peek(state)
  end
end
