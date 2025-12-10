defmodule ToxicParser.BuilderHelpersTest do
  use ExUnit.Case, async: true

  alias ToxicParser.Builder.Helpers

  test "builds unary and binary ops" do
    assert {:not, [line: 1], [:foo]} = Helpers.unary(:not, :foo, line: 1)
    assert {:+, [line: 2], [1, 2]} = Helpers.binary(:+, 1, 2, line: 2)
  end

  test "builds dot and access nodes" do
    assert {:., [ctx: :dot], [:foo, :bar]} = Helpers.dot(:foo, :bar, ctx: :dot)

    access = Helpers.access(:foo, [1, 2], ctx: :access)
    assert {{:., [ctx: :access], [Access, :get]}, [ctx: :access], [:foo, 1, 2]} = access
  end

  test "builds call nodes" do
    assert {:foo, [], [1, 2]} = Helpers.call(:foo, [1, 2])
    assert {:bar, [line: 1], []} = Helpers.call(:bar, [], line: 1)
  end

  test "builds alias segments" do
    assert {:__aliases__, [], [:Foo, :Bar]} = Helpers.alias_segments([:Foo, :Bar])
  end
end
