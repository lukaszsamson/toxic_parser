defmodule ToxicParser.ErrorsStrictTest do
  @moduledoc false
  use ExUnit.Case, async: true

  @default_opts [columns: true, token_metadata: true, emit_warnings: false]

  describe "strict parser errors" do
    test "literal encoder error" do
      opts = [literal_encoder: fn _literal, _meta -> {:error, "ENCODER_FAIL"} end]
      assert_error_conforms("1", opts)
    end

    test "range step operator must follow range" do
      assert_error_conforms("foo++bar//bat")
    end

    test "atom cannot be followed by alias" do
      assert_error_conforms(":foo.Bar")
    end

    test "fn without -> clauses" do
      assert_error_conforms("fn 1 end")
    end

    test "unexpected -> placement" do
      assert_error_conforms("""
      fn 1
      2 -> 3 end
      """)
    end

    test "keyword list inside tuple" do
      assert_error_conforms("{foo: :bar}")
    end

    test "keyword list inside bitstring" do
      assert_error_conforms("<<foo: :bar, baz: :bar>>")
    end

    test "expression after keyword list in call" do
      assert_error_conforms("call foo: 1, :bar")
    end

    test "expression after keyword list in list" do
      assert_error_conforms("[foo: 1, :bar]")
    end

    test "unexpected parentheses due to space" do
      assert_error_conforms("foo (hello, world)")
    end

    test "unexpected comma in nested calls" do
      assert_error_conforms("foo 1, foo 2, 3")
    end

    test "unexpected comma inside containers" do
      assert_error_conforms("[foo 1, 2]")
    end

    test "too many arguments in access syntax" do
      assert_error_conforms("foo[1, 2]")
    end

    test "invalid keyword identifier with do" do
      assert_error_conforms("if true do:\n")
    end

    @tag :skip
    test "invalid keyword identifier" do
      assert_error_conforms("if true else: 1")
    end

    @tag :skip
    test "unicode conversion error in list string" do
      assert true
    end
  end

  defp assert_error_conforms(code, opts \\ []) do
    reference = s2q(code, opts)
    actual = toxic_parse(code, opts)

    assert match?({:error, _}, reference),
           "Expected reference parser to error on: #{inspect(code)}, got: #{inspect(reference)}"

    assert actual == reference,
           """
           Error mismatch for: #{inspect(code)}

           Reference:
           #{inspect(reference, pretty: true)}

           Actual:
           #{inspect(actual, pretty: true)}
           """
  end

  defp s2q(code, opts) do
    Code.string_to_quoted(code, Keyword.merge(@default_opts, opts))
  end

  defp toxic_parse(code, opts) do
    case ToxicParser.parse_string(
           code,
           Keyword.merge([mode: :strict, token_metadata: true], opts)
         ) do
      {:ok, result} -> {:ok, result.ast}
      {:error, result} -> {:error, format_error(result)}
    end
  end

  defp format_error(result) do
    case result.diagnostics do
      [%{reason: reason} | _] -> reason
      _ -> :unknown_error
    end
  end
end
