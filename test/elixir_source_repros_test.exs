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

  describe "stab clause unions" do
    test "union of two stab clauses in parentheses" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/calendar.ex line 168
      # Function types with union patterns: (-> 1) | (-> 2)
      # The closing ) of a stab clause should allow led to continue with trailing operators
      code = ~S'''
      (-> 1) | (-> 2)
      '''

      assert_conforms(code)
    end

    test "complex union function types in typespecs" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/calendar.ex line 168
      # Pattern: (:am | :pm -> String.t()) | (:am | :pm, map() -> String.t())
      code = ~S'''
      (:am | :pm -> String.t()) | (:am | :pm, map() -> String.t())
      '''

      assert_conforms(code)
    end

    test "keyword list with union function type values" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/calendar.ex line 168
      code = ~S'''
      @type t :: [
        am_pm_names: (:am | :pm -> String.t()) | (:am | :pm, map() -> String.t())
      ]
      '''

      assert_conforms(code)
    end
  end

  describe "dual_op after EOE" do
    test "dual_op (-) after newlines is unary, not binary" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/calendar/date.ex line 120
      # When -1 appears on a separate line after a call, it should be unary minus
      # on a new expression, not binary subtraction
      code = ~S'''
      if a do
        1
      else
        x()

        -1
      end
      '''

      assert_conforms(code)
    end

    test "dual_op (+) after newlines is unary, not binary" do
      # Similar to above but with + operator
      code = ~S'''
      if true do
        1
      else
        foo()

        +42
      end
      '''

      assert_conforms(code)
    end
  end

  describe "bracket_identifier handling" do
    test "bracket access preserves variable AST" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/code.ex line 501
      # opts[:cache] should have opts as {:opts, [line: L, column: C], nil}
      # not just the atom :opts
      code = ~S'''
      opts[:cache]
      '''

      assert_conforms(code)
    end

    test "bracket access in conditional" do
      # Full context from code.ex
      code = ~S'''
      if opts[:cache], do: [:cache], else: [:nocache]
      '''

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

  describe "paren call vs parenthesized expression" do
    test "if with space before paren is not a paren call" do
      # `if (a)` with space - the ( is a parenthesized expression, not a paren call
      code = ~S'''
      if (a) do 1 end
      '''

      assert_conforms(code)
    end

    test "if with space in condition with or" do
      # `if (a) or (b)` - neither are paren calls
      code = ~S'''
      if (a) or (b) do 1 end
      '''

      assert_conforms(code)
    end

    test "paren call without space" do
      # `if(a)` without space IS a paren call
      code = ~S'''
      if(a) do 1 end
      '''

      assert_conforms(code)
    end
  end

  describe "cond with multiline or conditions" do
    test "cond clause with parenthesized or condition" do
      # The parenthesized (e and f) is part of the condition, not stab_parens_many
      code = ~S'''
      cond do
        a and b and c ->
          d

        (e and f) or
          (g and h) or i or
            (j and k) ->
          {l, m} =
            n(o, p, q, r, s, t, u, v)

          {wrap(l), m}

        true ->
          x(y, z)
      end
      '''

      assert_conforms(code)
    end
  end

  describe "map update newlines" do
    test "pipe operator in map update should have newlines metadata" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/dynamic_supervisor.ex
      # Map update with | on separate line should have newlines metadata on |
      code = ~S'''
      %{
        state
        | extra: val
      }
      '''

      assert_conforms(code)
    end
  end

  describe "capture operator is unary only" do
    test "capture operator after newline starts new expression" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/inspect.ex
      # When & appears on a new line after an expression, it should
      # start a new expression (unary), NOT be a binary operator
      code = ~S'''
      a = 1
      &b(1)
      '''

      assert_conforms(code)
    end

    test "capture in stab body with multiple statements" do
      # & in else block with preceding statement should be separate expressions
      code = ~S'''
      if true do
        &a/1
      else
        sep = 1
        &b(&1, sep)
      end
      '''

      assert_conforms(code)
    end
  end

  describe "dot call with list literal and trailing keywords" do
    test "list literal followed by keyword args in dot call" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/io/ansi/docs.ex
      # String.split(["\r\n", "\n"], trim: false) should have two args:
      # 1. ["\r\n", "\n"] (list literal)
      # 2. [trim: false] (keyword list)
      # NOT: [["\r\n", "\n", {:trim, false}]] (merged)
      code = ~S'''
      String.split(["\r\n", "\n"], trim: false)
      '''

      assert_conforms(code)
    end

    test "tuple literal followed by keyword args in dot call" do
      # Similar case with tuple
      code = ~S'''
      Foo.bar({1, 2}, key: value)
      '''

      assert_conforms(code)
    end
  end

  describe "quoted atom in when guard" do
    test "quoted string atom dot call in case when clause" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/kernel.ex
      # When guard contains :"Elixir.Kernel".in(x, ...), the -> should not
      # be consumed by the when expression but be the stab at the top level
      code = ~S'''
      case x do
        y when :"foo".bar(y) -> true
      end
      '''

      assert_conforms(code)
    end

    test "quoted atom dot call with multiple args" do
      # Similar case with more complex call
      code = ~S'''
      case value do
        x when :"Elixir.Kernel".in(x, [false, nil]) -> false
        _ -> true
      end
      '''

      assert_conforms(code)
    end
  end
end
