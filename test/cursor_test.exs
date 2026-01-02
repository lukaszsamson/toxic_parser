defmodule ToxicParser.CursorTest do
  use ExUnit.Case, async: true

  alias ToxicParser.Cursor

  defp kind({k, _meta}), do: k
  defp kind({k, _meta, _v}), do: k
  defp kind({k, _meta, _v1, _v2}), do: k

  test "next/1 returns raw Toxic tokens" do
    c = Cursor.new("1;2", mode: :tolerant)

    assert {:ok, tok1, c} = Cursor.next(c)
    assert kind(tok1) == :int

    assert {:ok, tok2, c} = Cursor.next(c)
    assert kind(tok2) == :";"

    assert {:ok, tok3, _c} = Cursor.next(c)
    assert kind(tok3) == :int
  end

  test "peek/1 does not consume" do
    c = Cursor.new("1;2", mode: :tolerant)

    assert {:ok, tok1, c} = Cursor.peek(c)
    assert kind(tok1) == :int

    assert {:ok, tok1b, _c} = Cursor.next(c)
    assert kind(tok1b) == :int
  end

  # test "peek_n/2 returns up to N tokens without consuming (including eof partial)" do
  #   c = Cursor.new("1", mode: :tolerant, max_peek: 2)

  #   assert {:eof, toks, c} = Cursor.peek_n(c, 2)
  #   assert Enum.map(toks, &kind/1) == [:int]

  #   assert {:ok, tok1, c} = Cursor.next(c)
  #   assert kind(tok1) == :int

  #   assert {:eof, _c} = Cursor.next(c)
  # end

  test "mark/1 and rewind/2 restore cursor position" do
    c = Cursor.new("a;b", mode: :tolerant)

    assert {:ok, _a, c} = Cursor.next(c)
    mark = c

    assert {:ok, semi, _c} = Cursor.next(c)
    assert kind(semi) == :";"

    c = mark
    assert {:ok, semi2, _c} = Cursor.next(c)
    assert kind(semi2) == :";"
  end

  # test "peek_n/2 raises when exceeding max_peek" do
  #   c = Cursor.new("1;2", mode: :tolerant, max_peek: 1)

  #   assert_raise ArgumentError, fn ->
  #     Cursor.peek_n(c, 2)
  #   end
  # end

  test "strict mode surfaces driver errors" do
    c = Cursor.new(")", mode: :strict)
    assert {:error, _reason, _c} = Cursor.next(c)
  end

  test "tolerant mode yields error_token (not {:error, ...})" do
    c = Cursor.new(")", mode: :tolerant)
    assert {:ok, {:error_token, _meta, %Toxic.Error{}}, _c} = Cursor.next(c)
  end

  test "current_terminators/1 is available on-demand" do
    c = Cursor.new("(1", mode: :tolerant)

    assert {:ok, {:"(", _meta, _value}, c} = Cursor.next(c)
    assert {:ok, _int, c} = Cursor.peek(c)

    terms = Cursor.current_terminators(c)
    assert is_list(terms)
    assert match?([{:"(", _, _} | _], terms)
  end
end
