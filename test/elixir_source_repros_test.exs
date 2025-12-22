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

    test "call with multiline args should not have newlines when first arg on same line - quoted" do
      code = """
      IO.warn("message",
        'line': line,
        'column': column,
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

      code = ~S'''
        """
        #{x} does not work

        first. \\
        second
        """
      '''

      assert_conforms(code)

      code = ~s'''
        """\r\n  \#{x} does not work\r\n  first. \\\r\n  second\r\n  """
      '''

      assert_conforms(code)

      code = ~s'''
        """\r\n  \#{x} does not work\r\n  first. \\\\\r\n  second\r\n  """
      '''

      assert_conforms(code)
    end

    test "escaped backslash at EOL in heredoc is preserved" do
      # From: /Users/lukaszsamson/elixir/lib/iex/test/iex/interaction_test.exs
      # In heredocs, a trailing backslash (\) escapes the newline (line continuation),
      # but an escaped backslash (\\) should produce a literal backslash and KEEP the newline.
      code = ~S'''
      code = """
      1 \\
      + 2
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

  describe "keyword lists with do/else keys" do
    test "keyword list can continue after do:" do
      # Regression fixed: `do` must not be treated as a terminator after a comma
      # when parsing keyword lists.
      code = ~S'''
      foo(do: 1, else: 2)
      '''

      assert_conforms(code)
    end

    test "keyword list literal with do/else" do
      code = ~S'''
      [do: 1, else: 2]
      '''

      assert_conforms(code)
    end
  end

  describe "map assoc keys in no-parens call args" do
    test "map key can be try-do-end block inside no-parens call arg" do
      # Repro for Elixir's MapTest: a map key can be a block expression.
      # This must work even when the map appears in no-parens call args
      # (our args parse in :matched context).
      code = ~S'''
      f %{
        try do
          raise "error"
        rescue
          _ -> 1
        end => 1
      }
      '''

      assert_conforms(code)
    end
  end

  describe "struct base expressions" do
    test "struct base can be a dotted paren call (URI.t())" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/test/elixir/typespec_test.exs
      # Repro: ensure `URI.t()` parses as a call on the dotted target:
      #   {{:., ..., [__aliases__(URI), :t]}, meta, []}
      # not as a dot whose member is a call:
      #   {:., ..., [__aliases__(URI), {:t, meta, []}]}
      code = "test_module do\n  @type my_type :: %URI.t(){}\nend\n"
      assert_conforms(code)
    end
  end

  describe "capture expressions" do
    test "capture of case with &1 inside parens call" do
      # From: /Users/lukaszsamson/elixir/lib/mix/lib/mix/tasks/compile.ex
      # This form relies on `&1` being a valid no-parens argument.
      code =
        "Enum.map([:noop], &case &1 do\n  :noop -> {:noop, []}\n  {status, diagnostics} -> {status, diagnostics}\nend)"

      assert_conforms(code)
    end
  end

  describe "regressions" do
    test "defmodule name is an alias, not a 0-arity call" do
      # Ensure `EEx` is represented as {:__aliases__, ..., [:EEx]} (like s2q)
      # and not as {:EEx, meta, []}.
      code = "defmodule EEx do\nend"
      assert_conforms(code)
    end

    test "double bang in cond guard keeps precedence" do
      # From: /Users/lukaszsamson/elixir/lib/mix/lib/mix/compilers/elixir.ex:103
      code = ~S'''
      cond do
        !!opts[:force] or is_nil(old_deps_config) or old_cache_key != new_cache_key or
          (Keyword.get(opts, :check_cwd, true) and old_cwd != File.cwd!()) ->
          {true, stale, deps_config(local_deps)}
      end
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

    test "map literal followed by keyword args in dot call" do
      code = ~S'''
      String.split(%{"\r\n" => "\n"}, trim: false)
      '''

      assert_conforms(code)

      code = ~S'''
      String.split({"\r\n", "\n", 1}, trim: false)
      '''

      assert_conforms(code)
    end

    test "bitstring literal followed by keyword args in dot call" do
      code = ~S'''
      String.split(<<"\r\n", "\n">>, trim: false)
      '''

      assert_conforms(code)
    end

    test "stub expression followed by keyword args in dot call" do
      code = ~S'''
      String.split(("\r\n" -> "\n"), trim: false)
      '''

      assert_conforms(code)
    end

    test "fn expression followed by keyword args in dot call" do
      code = ~S'''
      String.split(fn "\r\n" -> "\n" end, trim: false)
      '''

      assert_conforms(code)
    end

    test "tuple literal followed by keyword args in dot call" do
      # Similar case with tuple
      code = ~S'''
      Foo.bar({1, 2}, key: value)
      '''

      assert_conforms(code)

      code = ~S'''
      Foo.bar({1, 2, 3}, key: value)
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

  describe "tuple with keyword tail" do
    test "tuple with 3+ elements followed by keyword list" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/test/elixir/code_normalizer/quoted_ast_test.exs
      # A tuple like {1, 2, 3, foo: :bar} should be {:{}, meta, [1, 2, 3, [foo: :bar]]}
      code = ~S'{1, 2, 3, foo: :bar}'

      assert_conforms(code)
    end

    test "tuple with 4 elements and multiple keyword pairs" do
      code = ~S'{1, 2, 3, 4, foo: 1, bar: 2}'

      assert_conforms(code)
    end
  end

  describe "dot paren call newlines" do
    test "newlines only counts leading newlines after open paren" do
      # From: /Users/lukaszsamson/claude_fun/elixir_oss/projects/livebook
      # The newlines metadata on a dot paren call should only count
      # newlines after the opening paren, not trailing before closing
      code = """
      error =
        error_cons.(
          file: 1
        )
      """

      assert_conforms(code)
    end

    test "no newlines when no newline after open paren" do
      # Newline only before ) should NOT set newlines metadata
      code = "foo.(1\n)"

      assert_conforms(code)
    end
  end

  describe "bracket access vs no-parens call" do
    test "dot member with space before [ is no-parens call, not bracket access" do
      # From: /Users/lukaszsamson/claude_fun/elixir_oss/projects/phoenix/installer/test/
      # Mix.Tasks.Foo.run [1] - the [1] is a list argument, NOT bracket access
      code = ~S'Mix.Tasks.Foo.run ["a"]'

      assert_conforms(code)
    end

    test "dot member without space before [ is bracket access" do
      # Mix.Tasks.Foo.run[1] - the [1] is bracket access
      code = ~S'Mix.Tasks.Foo.run[1]'

      assert_conforms(code)
    end

    test "dot paren call followed by bracket access" do
      # Application.spec(:pythonx)[:vsn] - bracket access on call result
      code = ~S'Application.spec(:pythonx)[:vsn]'

      assert_conforms(code)
    end

    test "quoted identifier bracket access" do
      # D."foo"[1] - bracket access on quoted identifier
      code = ~S'D."foo"[1]'

      assert_conforms(code)
    end
  end

  describe "ellipsis operator as standalone" do
    test "ellipsis followed by comma in list is standalone" do
      # From: /Users/lukaszsamson/claude_fun/elixir_oss/projects/ecto/integration_test/
      # [p, ..., c] - the ... should be standalone, not try to parse c as operand
      code = ~S'[p, ..., c]'

      assert_conforms(code)
    end

    test "ellipsis in dynamic query pattern" do
      # dynamic([p, ..., c], expr) - common pattern in Ecto
      code = ~S'dynamic([p, ..., c], x)'

      assert_conforms(code)
    end
  end

  describe "nested no-parens calls with do-blocks in matched context" do
    test "check all inside property block - do-block belongs to outer call" do
      # From: /Users/lukaszsamson/claude_fun/elixir_oss/projects/oban/test/oban/backoff_test.exs
      # When parsing `foo all x <- y do ... end` inside a do-block (matched context):
      # - `all` should be parsed as call with args `x <- y`
      # - The `do ... end` should attach to `foo`, NOT to `all`
      # Current behavior: do-block attaches to `all` instead of `foo`
      code = """
      foo do
        bar all x <- y do
          :ok
        end
      end
      """

      assert_conforms(code)
    end

    test "check all with multiple generators inside property block" do
      # Same issue with multiple generators separated by comma
      code = """
      property "test" do
        check all mult <- integer(1..10),
                  attempt <- integer(1..20) do
          :ok
        end
      end
      """

      assert_conforms(code)
    end
  end

  describe "ambiguous_op metadata" do
    test "op_identifier with single arg should have ambiguous_op: nil" do
      # When an op_identifier (like `assert`) is called with a single argument
      # that is an operator expression (like `-1`), it should have ambiguous_op: nil
      code = "assert -1 == x"

      assert_conforms(code)
    end

    test "op_identifier with do-block should NOT have ambiguous_op: nil" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/kernel.ex line 1557
      # `def +value do ... end` - the def has a do-block, so no ambiguity
      code = """
      defmodule Foo do
        def +value do
          :ok
        end
      end
      """

      assert_conforms(code)
    end
  end

  describe "guard expressions with do-blocks" do
    test "if do-block inside fn guard clause with empty parens" do
      # Guard expressions use `expr` not `matched_expr` in elixir_parser.yrl
      # So do-blocks should be allowed in guard expressions
      # Note: this only works with empty parens (), NOT with (x)
      code = ~S'fn () when if a do :ok end -> foo() end'

      assert_conforms(code)
    end
  end

  describe "keyword entry with pipe value in maps" do
    test "keyword with list | type should not parse as map update" do
      # From: /Users/lukaszsamson/claude_fun/elixir_oss/projects/phoenix_live_view/lib/phoenix_live_view/engine.ex line 64
      # `static: [String.t()] | non_neg_integer()` inside a struct definition
      # The `static:` should be a keyword entry with value `[String.t()] | non_neg_integer()`
      # NOT a map update with base `:static[String.t()]` (bracket access)
      code = ~S'%{static: [String.t()] | non_neg_integer()}'

      assert_conforms(code)
    end

    test "keyword with list | simple value" do
      # Simpler version of the same issue
      code = ~S'%{a: [1] | 2}'

      assert_conforms(code)
    end

    test "quoted list keyword with list | simple value" do
      # Simpler version of the same issue
      code = ~S/%{'a': [1] | 2}/

      assert_conforms(code)
    end

    test "keyword string with list | simple value" do
      # Simpler version of the same issue
      code = ~S'%{"a": [1] | 2}'

      assert_conforms(code)
    end
  end

  describe "interpolated strings in guards" do
    test "fn with interpolated string in when guard" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/test/elixir/kernel/expansion_test.exs line 828
      # fn with interpolated string in when guard context
      code = ~S'fn arg when "#{arg}foo" == "argfoo" -> arg end'

      assert_conforms(code)
    end
  end

  describe "@spec with unary operators" do
    test "@spec with unary + operator" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/kernel.ex line 1555
      # @spec +integer :: integer - the :: should be inside spec's argument
      # NOT: (@spec +integer) :: integer
      code = "@spec +integer :: integer"

      assert_conforms(code)
    end

    test "@spec with unary - operator" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/kernel.ex line 1598
      code = "@spec -integer :: integer"

      assert_conforms(code)
    end

    test "@spec with unary + float" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/kernel.ex line 1556
      code = "@spec +float :: float"

      assert_conforms(code)
    end

    test "@spec with unary - float" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/lib/kernel.ex line 1599
      code = "@spec -float :: float"

      assert_conforms(code)
    end
  end

  describe "map update with unquote_splicing" do
    test "map update with only unquote_splicing should wrap RHS in list" do
      # From: /Users/lukaszsamson/elixir/lib/elixir/test/elixir/kernel/quote_test.exs line 234
      # Map update where RHS is only unquote_splicing should have RHS wrapped in list
      # Reference: {:|, _, [map, [{:unquote_splicing, ...}]]}
      # Not: {:|, _, [map, {:unquote_splicing, ...}]}
      code = "%{map | unquote_splicing(foo: :bar)}"

      assert_conforms(code)
    end
  end

  describe "@ operator precedence in lists" do
    test "[@for | modules] should parse @ before |" do
      # From: /Users/lukaszsamson/elixir/lib/iex/lib/iex/info.ex line 440
      # @ (bp 320) should bind tighter than | (bp 70)
      # Reference: [{:|, _, [{:@, _, [{:for, _, nil}]}, {:modules, _, nil}]}]
      # Not: [{:@, _, [{:|, _, [{:for, _, nil}, {:modules, _, nil}]}]}]
      code = "[@for | modules]"

      assert_conforms(code)
    end
  end

  describe "interpolated atoms in with clauses" do
    test "interpolated atom should not trigger no_parens error" do
      # From: livebook/lib/livebook/runtime/fly.ex line 133
      # Interpolated atoms generate remote calls like :erlang.binary_to_atom/2
      # which should be classified as matched, not no_parens.
      # The AST is: {{:., _, [:erlang, :binary_to_atom]}, meta, [binary, :utf8]}
      # which has 2 args but no :closing metadata (compiler-generated call).
      code = ~S'with a <- :"foo#{bar}", do: a'

      assert_conforms(code)
    end

    test "interpolated atom in complex with pattern" do
      # More realistic case from livebook
      code = ~S'''
      with child_node <- :"child@#{host}",
           :pong <- Node.ping(child_node) do
        {:ok, child_node}
      end
      '''

      assert_conforms(code)
    end

    test "interpolated atom assignment" do
      # Simpler case - just the interpolated atom
      code = ~S'x = :"foo#{bar}baz"'

      assert_conforms(code)
    end
  end

  describe "map update with do-block keyword value" do
    test "map update with if-do keyword value should parse full do-block" do
      # From: /Users/lukaszsamson/claude_fun/elixir_oss/projects/livebook/lib/livebook/session/data.ex line 1812
      # Map update where keyword value is an if-do expression
      # The do-block must be fully consumed as part of the keyword value
      code = "%{foo | bar: if true do 1 end}"

      assert_conforms(code)
    end

    test "map update with nested if-else do-block" do
      # More complex case with else clause
      code = """
      %{
        foo
        | bar:
            if true do
              :a
            else
              :b
            end
      }
      """

      assert_conforms(code)
    end
  end
end
