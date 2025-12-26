defmodule ToxicParser.LayoutTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Layout, TokenAdapter}

  test "peek_sep returns raw separator tokens" do
    state = TokenAdapter.new("1\n\n;2")

    {:ok, _tok, state} = TokenAdapter.next(state)

    # Raw eol token with newline count in third element
    assert {:ok, {:eol, {{1, 2}, {3, 1}, 2}}, state} = Layout.peek_sep(state)
    {:ok, _tok, state} = TokenAdapter.next(state)
    # Raw semicolon token
    assert {:ok, {:";", _meta}, _} = Layout.peek_sep(state)
  end

  test "skip_newlines_only counts only newline tokens, stops at semicolon" do
    state = TokenAdapter.new("1\n\n;2")
    {:ok, _tok, state} = TokenAdapter.next(state)

    {state, count} = Layout.skip_newlines_only(state)
    assert count == 2
    # Semicolon remains - not consumed by skip_newlines_only
    assert {:ok, {:";", _meta}, _} = TokenAdapter.peek(state)
  end
end
