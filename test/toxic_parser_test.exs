defmodule ToxicParserTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Conformance, Nonterminals}

  test "strict mode matches Elixir parser for a simple snippet" do
    source = "add(1, 2)"
    {:ok, expected_ast, expected_comments} = Code.string_to_quoted_with_comments(source)

    assert {:ok, result} =
             ToxicParser.parse_string(source,
               mode: :strict,
               preserve_comments: true
             )

    assert result.ast == expected_ast
    assert result.comments == expected_comments
    assert result.diagnostics == []
  end

  test "tolerant mode collects diagnostics instead of erroring" do
    source = "defmodule( do"

    assert {:error, strict_result} = ToxicParser.parse_string(source, mode: :strict)
    assert length(strict_result.diagnostics) == 1

    assert {:ok, tolerant_result} = ToxicParser.parse_string(source, mode: :tolerant)
    assert tolerant_result.ast == nil
    assert length(tolerant_result.diagnostics) == 1
  end

  test "conformance harness compares strict and tolerant outputs" do
    source = """
    # leading comment
    foo = 1
    bar = foo + 2
    """

    assert {:ok, comparison} =
             Conformance.compare(source, preserve_comments: true, token_metadata: true)

    assert comparison.strict.ast == comparison.reference_ast
    assert comparison.tolerant.ast == comparison.reference_ast
    assert comparison.strict.comments == comparison.reference_comments
  end

  test "nonterminal mapping is loaded and accessible" do
    assert {:ok, :recursive_descent} = Nonterminals.fetch("expr")
    assert {:ok, :pratt} = Nonterminals.fetch("matched_expr")
    assert [_ | _] = Nonterminals.list()
  end
end
