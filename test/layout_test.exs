defmodule ToxicParser.LayoutTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Layout, TokenAdapter}

  test "peek_sep returns raw separator tokens" do
    {state, cursor} = TokenAdapter.new("1\n\n;2")

    {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)

    # Raw eol token with newline count in third element
    assert {:ok, {:eol, {{1, 2}, {3, 1}, 2}}, state, cursor} = Layout.peek_sep(state, cursor)
    {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)
    # Raw semicolon token
    assert {:ok, {:";", _meta}, _, _} = Layout.peek_sep(state, cursor)
  end

  test "skip_newlines_only counts only newline tokens, stops at semicolon" do
    {state, cursor} = TokenAdapter.new("1\n\n;2")
    {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)

    {state, cursor, count} = Layout.skip_newlines_only(state, cursor, nil, 0)
    assert count == 2
    # Semicolon remains - not consumed by skip_newlines_only
    assert {:ok, {:";", _meta}, _, _} = TokenAdapter.peek(state, cursor)
  end
end
