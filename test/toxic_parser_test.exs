defmodule ToxicParserTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Conformance, Nonterminals}

  test "strict mode parses a simple call" do
    source = "add(1, 2)"
    assert {:ok, result} = ToxicParser.parse_string(source, mode: :strict)
    assert Macro.to_string(result.ast) == "add(1, 2)"
    assert result.diagnostics == []
  end

  # test "tolerant mode collects diagnostics instead of erroring" do
  #   source = "defmodule( do"
  #
  #   assert {:error, strict_result} = ToxicParser.parse_string(source, mode: :strict)
  #   assert length(strict_result.diagnostics) >= 1
  #
  #   assert {:ok, tolerant_result} = ToxicParser.parse_string(source, mode: :tolerant)
  #   assert length(tolerant_result.diagnostics) >= 1
  # end

  test "conformance harness returns strict/tolerant results" do
    source = "1 + 2"

    assert {:ok, comparison} = Conformance.compare(source)
    assert comparison.strict.ast == comparison.tolerant.ast
  end

  test "nonterminal mapping is loaded and accessible" do
    assert {:ok, :recursive_descent} = Nonterminals.fetch("expr")
    assert {:ok, :pratt} = Nonterminals.fetch("matched_expr")
    assert [_ | _] = Nonterminals.list()
  end
end
