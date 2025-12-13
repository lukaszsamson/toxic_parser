defmodule ToxicParser.ElixirSourceReprosTest do
  @moduledoc """
  Minimal reproduction test cases for issues found when parsing real Elixir source files.
  Each test represents a bug discovered during elixir sources conformance testing.
  """
  use ExUnit.Case, async: true

  describe "multiline call newlines metadata" do
    test "call with multiline args should not have newlines when first arg on same line" do
      # From: /Users/lukaszsamson/elixir/lib/eex/lib/eex/compiler.ex
      # IO.warn call spanning multiple lines
      code = """
      IO.warn("message",
        line: line,
        column: column,
        file: file
      )
      """

      assert_conforms(code)
    end
  end

  describe "nested keyword lists" do
    test "list of keyword lists should not be flattened" do
      # From: /Users/lukaszsamson/elixir/lib/eex/test/eex_test.exs
      # A list containing multiple keyword lists
      code = """
      [[line: 1, column: 5],
       [line: 1, column: 15]]
      """

      assert_conforms(code)
    end

    test "call with explicit keyword list followed by trailing keywords" do
      # From: /Users/lukaszsamson/elixir/lib/eex/test/eex_test.exs line 998
      # Function call with explicit [kwlist] followed by trailing keywords
      code = """
      foo("a", [jose: "valid"],
        parser_options: [x: 1]
      )
      """

      assert_conforms(code)
    end
  end

  describe "heredoc line continuation" do
    test "line continuation in indented heredoc should strip following indentation" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/access.ex around line 212
      # Heredoc with indentation and line continuation (backslash at end of line)
      code = ~S'''
        """
        #{x} does not work

        first. \
        second
        """
      '''

      assert_conforms(code)
    end
  end

  describe "bitstring stab patterns" do
    test "case clause with bitstring pattern should be separate clause" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/base.ex around line 790
      # Bitstring pattern in case clause should not consume -> into the pattern
      code = """
      case x do
        _ when not y ->
          false

        <<c1::8>> ->
          c1
      end
      """

      assert_conforms(code)
    end
  end

  # Helper functions

  defp assert_conforms(code) do
    reference = s2q(code)
    actual = toxic_parse(code)

    assert actual == reference,
           """
           AST mismatch for: #{inspect(code)}

           Reference:
           #{inspect(reference, pretty: true)}

           Actual:
           #{inspect(actual, pretty: true)}
           """
  end

  defp s2q(code) do
    Code.string_to_quoted(
      code,
      columns: true,
      token_metadata: true,
      emit_warnings: false
    )
  end

  defp toxic_parse(code) do
    case ToxicParser.parse_string(code, mode: :strict, token_metadata: true) do
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
