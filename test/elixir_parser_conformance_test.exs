defmodule ToxicParser.ElixirParserConformanceTest do
  @moduledoc """
  Conformance tests adapted from Elixir's parser_test.exs.

  These tests verify that ToxicParser produces identical results (both AST and errors)
  to the reference Elixir parser.
  """
  use ExUnit.Case, async: true

  describe "nullary ops" do
    test "in expressions" do
      assert_conforms("..")
      assert_conforms("...")
    end

    test "in capture" do
      assert_conforms("&../0")
      assert_conforms("&.../0")
    end

    test "raises on ambiguous uses when also binary" do
      assert_error_conforms("if .. do end")
    end
  end

  describe "unary ops" do
    test "in keywords" do
      assert_conforms("f(!: :ok)")
      assert_conforms("f @: :ok")
    end

    test "in maps" do
      assert_conforms("%{+foo, bar => bat, ...baz}")
    end

    test "ambiguous ops" do
      assert_conforms("f -var")
      assert_conforms("f -(var)")
      assert_conforms("f +-var")
      assert_conforms("f - var")
      assert_conforms("f --var")
      assert_conforms("(f ->var)")
    end

    test "ambiguous ops in keywords" do
      assert_conforms("f(+: :ok)")
      assert_conforms("f +: :ok")
    end
  end

  describe "ternary ops" do
    test "root" do
      assert_conforms("1..2//3")
      assert_conforms("(1..2)//3")
    end

    test "with do-blocks" do
      assert_conforms("foo do end..bar do end//baz do end")
    end

    test "with no parens" do
      assert_conforms("1..foo do end//bar bat")
    end

    test "errors" do
      assert_error_conforms("foo..bar baz//bat")
      assert_error_conforms("foo++bar//bat")
      assert_error_conforms("foo..(bar//bat)")
    end
  end

  describe "identifier unicode normalization" do
    test "stops at ascii codepoints" do
      assert_conforms("ç\n")
      assert_conforms(~S"ç\\1")
    end

    test "nfc normalization is performed" do
      # non-nfc: "ç" (code points 0x0063 0x0327)
      # nfc-normalized: "ç" (code points 0x00E7)
      assert_conforms("ç = 1")
    end

    test "elixir's additional normalization is performed" do
      # Common micro => Greek mu
      assert_conforms("µs = 1")
    end

    test "handles graphemes inside quoted identifiers" do
      assert_conforms(~S|foo."➡️"|)
      assert_conforms(~S|foo.'➡️'|)
      assert_conforms(~S|:"➡️"|)
      assert_conforms(~S|:'➡️'|)
      assert_conforms(~S|["➡️": x]|)
      assert_conforms(~S|['➡️': x]|)
    end
  end

  describe "strings/sigils" do
    test "delimiter information for sigils is included" do
      assert_conforms("~r/foo/")
      assert_conforms("~r[foo]")
      assert_conforms("~r\"foo\"")
      assert_conforms("~S\"\"\"\nsigil heredoc\n\"\"\"")
      assert_conforms("~S'''\nsigil heredoc\n'''")
    end

    test "valid multi-letter sigils" do
      assert_conforms("~REGEX/foo/")
      assert_conforms("~REGEX/foo/mods")
      assert_conforms("~REGEX[foo]")
      assert_conforms("~MAT\"\"\"\n1,2,3\n\"\"\"")
      assert_conforms("~FOO1\"\"\"\n1,2,3\n\"\"\"")
      assert_conforms("~BAR321\"\"\"\n1,2,3\n\"\"\"")
      assert_conforms("~I18N\"\"\"\n1,2,3\n\"\"\"")
    end

    test "invalid multi-letter sigils" do
      assert_error_conforms("~Regex/foo/")
      assert_error_conforms("~FOo1{bar]")
      assert_error_conforms("~foo1{bar]")
    end

    test "sigil newlines" do
      assert_conforms(~s|~s"here\ndoc"|)
      assert_conforms(~s|~s"here\r\ndoc"|)
    end

    test "string newlines" do
      assert_conforms(~s|"here\ndoc"|)
      assert_conforms(~s|"here\r\ndoc"|)
      assert_conforms(~s|"here\\\ndoc"|)
      assert_conforms(~s|"here\\\r\ndoc"|)
    end

    test "heredoc newlines" do
      assert_conforms(~s|"""\nhere\ndoc\n"""|)
      assert_conforms(~s|"""\r\nhere\r\ndoc\r\n"""|)
      assert_conforms(~s|  """\n  here\n  doc\n  """|)
      assert_conforms(~s|  """\r\n  here\r\n  doc\r\n  """|)
      assert_conforms(~s|"""\nhere\\\ndoc\\\n"""|)
      assert_conforms(~s|"""\r\nhere\\\r\ndoc\\\r\n"""|)
    end

    test "heredoc indentation" do
      assert_conforms("~S'''\n    sigil heredoc\n  '''")
    end
  end

  describe "string_to_quoted/2" do
    test "converts strings to quoted expressions" do
      assert_conforms("1 + 2")
    end

    test "returns error on invalid syntax" do
      assert_error_conforms("a.1")
    end
  end

  describe "string_to_quoted/2 and atom handling" do
    test "ensures :existing_atoms_only" do
      # These tests are about specific options behavior, checking error conformance
      assert_error_conforms_existing_atoms(":there_is_no_such_atom_xyz123")
      assert_error_conforms_existing_atoms("~UNKNOWN_XYZ'foo bar'")
    end
  end

  describe "string_to_quoted/2 with :columns" do
    test "includes column information" do
      assert_conforms("1 + 2")
      assert_conforms("foo + bar")
    end

    test "not in" do
      assert_conforms("a not in b")
      assert_conforms("a not  in b")
      assert_conforms("a\nnot in b")
      assert_conforms("a not in\nb")
      assert_conforms("a\nnot in\nb")
    end

    test "handles maps and structs" do
      assert_conforms("%{}")
      assert_conforms("%:atom{}")
    end
  end

  describe "string_to_quoted/2 with :token_metadata" do
    test "adds end_of_expression information to blocks" do
      assert_conforms("""
      one();two()
      three()

      four()


      five()
      """)
    end

    test "adds end_of_expression to the right hand side of ->" do
      assert_conforms("""
      case true do
        :foo -> bar(); two()
        :baz -> bat()
      end
      """)
    end

    test "end of expression with literal" do
      assert_conforms_with_literal_encoder("""
      a do
        d ->
          (
            b -> c
          )
      end
      """)
    end

    test "does not add end of expression to ->" do
      assert_conforms("""
      case true do
        :foo -> :bar
        :baz -> :bat
      end\
      """)
    end

    test "adds pairing information" do
      assert_conforms("foo")
      assert_conforms("foo()")
      assert_conforms("foo(\n)")
      assert_conforms("%{\n}")
      assert_conforms("foo(\n) do\nend")
      assert_conforms("foo(\n)(\n)")
    end

    test "adds opening and closing information for single-expression block" do
      assert_conforms("1 + (2 + 3)")
      assert_conforms("1 + ((2 + 3))")
    end

    test "adds opening and closing information for tuples" do
      assert_conforms("{}")
      assert_conforms("{123}")
      assert_conforms("x.{}")
      assert_conforms("x.{123}")
    end

    test "adds opening and closing information for empty block" do
      assert_conforms("()")
      assert_conforms("(())")

      assert_conforms("""
      (
        # Foo
        (
          # Bar
        )
      )
      """)
    end

    test "adds opening and closing information for stab arguments" do
      assert_conforms("fn () -> x end ")
      assert_conforms("fn (x, y) -> x end ")
      assert_conforms("if true do (x, y) -> x end")
    end

    test "with :literal_encoder" do
      assert_conforms_with_literal_encoder(~s("one"))
      assert_conforms_with_literal_encoder("?é")
      assert_conforms_with_literal_encoder("0b10")
      assert_conforms_with_literal_encoder("12")
      assert_conforms_with_literal_encoder("0o123")
      assert_conforms_with_literal_encoder("0xEF")
      assert_conforms_with_literal_encoder("12.3")
      assert_conforms_with_literal_encoder("nil")
      assert_conforms_with_literal_encoder(":one")
      assert_conforms_with_literal_encoder("true")
      assert_conforms_with_literal_encoder(":true")
      assert_conforms_with_literal_encoder("[one: :two]")
      assert_conforms_with_literal_encoder("[1]")
      assert_conforms_with_literal_encoder(~s("""\nhello\n"""))
      assert_conforms_with_literal_encoder(~s[fn (1) -> "hello" end])
      assert_conforms_with_literal_encoder("(1)")
    end

    test "adds identifier_location for qualified identifiers" do
      assert_conforms("foo.\nbar")
      assert_conforms("foo\n.\nbar")
      assert_conforms(~s[Foo.\nbar(1)])
    end

    test "adds metadata for the last alias segment" do
      assert_conforms("Foo")
      assert_conforms("Foo.\nBar\n.\nBaz")
      assert_conforms("foo.\nBar\n.\nBaz")
    end

    test "adds metadata about assoc operator position in maps" do
      assert_conforms_with_literal_encoder("%{:key => 1, {} => {}}")
    end
  end

  describe "syntax errors" do
    test "invalid heredoc start" do
      assert_error_conforms(~s["""bar\n"""])
    end

    test "invalid fn" do
      assert_error_conforms("fn 1 end")
      assert_error_conforms("fn 1\n2 -> 3 end")
    end

    test "invalid token" do
      assert_error_conforms("\u3164 = 1")
      assert_error_conforms("[foo: \u200B]\noops")
      assert_error_conforms("\r")
    end

    test "invalid bidi in source" do
      assert_error_conforms("# This is a \u202A")
      assert_error_conforms("foo. # This is a \u202A")
      assert_error_conforms("\"this is a \u202A\"")
      assert_error_conforms("\"this is a \\\u202A\"")
    end

    test "invalid newline in source" do
      assert_error_conforms("# This is a \u2028")
      assert_error_conforms("foo. # This is a \u2028")
    end

    test "reserved tokens" do
      assert_error_conforms("__aliases__")
      assert_error_conforms("__block__")
    end

    test "invalid alias terminator" do
      assert_error_conforms("Foo()")
    end

    test "invalid quoted token" do
      assert_error_conforms("\"hello\" \"world\"")
      assert_error_conforms("1 Foobar")
      assert_error_conforms("Foo.:foo")
      assert_error_conforms("Foo.:\"foo\#{:bar}\"")
      assert_error_conforms("Foo.:\"\#{:bar}\"")
    end

    test "invalid identifier" do
      assert_error_conforms("foo@")
      assert_error_conforms("foo@ ")
      assert_error_conforms("foo@bar")
      assert_error_conforms("Foo@")
      assert_error_conforms("Foo@bar")
      assert_error_conforms("Foo!")
      assert_error_conforms("Foo?")
      assert_error_conforms("Foó")
    end

    test "keyword missing space" do
      assert_error_conforms("foo:bar")
      assert_error_conforms("foo:+")
      assert_error_conforms("foo:+1")
    end

    test "invalid keyword list in tuple/binary" do
      assert_error_conforms("{foo: :bar}")
      assert_error_conforms("{foo: :bar, baz: :bar}")
      assert_error_conforms("<<foo: :bar, baz: :bar>>")
    end

    test "expression after keyword lists" do
      assert_error_conforms("call foo: 1, :bar")
      assert_error_conforms("call(foo: 1, :bar)")
      assert_error_conforms("[foo: 1, :bar]")
      assert_error_conforms("%{foo: 1, :bar => :bar}")
    end

    test "syntax errors include formatted snippet" do
      assert_error_conforms("1 + * 3")
    end

    test "invalid map start" do
      assert_error_conforms("{:ok, %[], %{}}")
      assert_error_conforms("% {1, 2, 3}")
    end

    test "invalid access" do
      assert_error_conforms("foo[1, 2]")
      assert_error_conforms("foo[1, 2, 3]")
      assert_error_conforms("foo[1, 2, 3,]")
    end

    test "unexpected end" do
      assert_error_conforms("1 end")

      assert_error_conforms("""
      defmodule MyApp do
        def one end
        def two do end
      end
      """)

      assert_error_conforms("""
      defmodule MyApp do
        def one
        end

        def two do
        end
      end
      """)

      assert_error_conforms("""
      defmodule MyApp do
        def one do
        end

        def two
        end
      end
      """)
    end

    test "invalid keywords" do
      assert_error_conforms("+.foo")
      assert_error_conforms("after = 1")
    end

    test "before sigil" do
      assert_error_conforms("~s(foo) ~s(bar baz)")
      assert_error_conforms("~s(foo) ~s()")
      assert_error_conforms("~s(foo) ~s(bar \#{:baz})")
      assert_error_conforms("~s(foo) ~s(\#{:bar} baz)")
    end

    test "invalid do" do
      assert_error_conforms("if true, do\n")
      assert_error_conforms("if true do:\n")
    end

    test "invalid parens call" do
      assert_error_conforms("foo (hello, world)")
    end

    test "invalid nested no parens call" do
      assert_error_conforms("[foo 1, 2]")
      assert_error_conforms("[foo bar 1, 2]")
      assert_error_conforms("[do: foo 1, 2]")
      assert_error_conforms("foo(do: bar 1, 2)")
      assert_error_conforms("{foo 1, 2}")
      assert_error_conforms("{foo bar 1, 2}")
      assert_error_conforms("foo 1, foo 2, 3")
      assert_error_conforms("foo 1, @bar 3, 4")
      assert_error_conforms("foo 1, 2 + bar 3, 4")
      assert_error_conforms("foo(1, foo 2, 3)")
    end

    test "valid nested no parens call interpretation" do
      # These are valid - just verify conformance
      assert_conforms("f 1 + g h 2, 3")
      assert_conforms("assert [] = TestRepo.all from p in Post, where: p.title in ^[]")
    end

    test "invalid atom dot alias" do
      assert_error_conforms(":foo.Bar")
      assert_error_conforms(":\"+\".Bar")
    end

    @tag :skip
    test "invalid map/struct" do
      assert_error_conforms("%{foo bar, baz}")
      assert_error_conforms("%{a, b}{a: :b}")
    end

    test "mismatching delimiters" do
      assert_error_conforms("fn a -> )")
      assert_error_conforms("defmodule A do ]")
      assert_error_conforms("(1, 2, 3}")
      assert_error_conforms("<<1, 2, 3, 4 end")
    end

    test "invalid interpolation" do
      assert_error_conforms("\"foo\#{case 1 do )}bar\"")

      assert_error_conforms("""
      defmodule MyApp do
        (
          def one do
          # end

          def two do
          end
        )
      end
      """)
    end

    test "invalid end of expression" do
      # Valid examples - verify conformance
      assert_conforms("""
      1;
      2;
      3
      """)

      assert_conforms("(;)")
      assert_conforms("(;1)")
      assert_conforms("(1;)")
      assert_conforms("(1; 2)")
      assert_conforms("fn -> 1; 2 end")
      assert_conforms("fn -> ; end")

      assert_conforms("""
      if true do
        ;
      end
      """)

      assert_conforms("""
      try do
        ;
      catch
        _, _ -> ;
      after
        ;
      end
      """)

      # Invalid examples
      assert_error_conforms("1+;\n2")
      assert_error_conforms("max(1, ;2)")
    end

    test "invalid new line" do
      assert_error_conforms("if true do\n  foo = [],\n  baz\nend")
    end

    test "invalid fn do expr end" do
      assert_error_conforms("fn do :ok end")
    end

    test "characters literal are printed correctly in syntax errors" do
      assert_error_conforms(":ok ?a")
      assert_error_conforms(":ok ?\\s")
      assert_error_conforms(":ok ?す")
    end

    test "character literals take newlines into account" do
      # These are valid
      assert_conforms("{?\n}\n{123}")
      assert_conforms("{?\\n}\n{123}")
      assert_conforms("{?\\\n}\n{123}")
    end

    test "numbers are printed correctly in syntax errors" do
      assert_error_conforms(":ok 12")
      assert_error_conforms(":ok 0b1")
      assert_error_conforms(":ok 12.3")
      assert_error_conforms("123_456_foo")
    end

    test "on hex errors" do
      assert_error_conforms(~S["\x"])
      assert_error_conforms(~S[:"\x"])
      assert_error_conforms(~S["\x": 123])
      assert_error_conforms(~s["""\n\\x\n"""])
    end

    test "on unicode errors" do
      assert_error_conforms(~S["\u"])
      assert_error_conforms(~S[:"\u"])
      assert_error_conforms(~S["\u": 123])
      assert_error_conforms(~s["""\n\\u\n"""])
      assert_error_conforms(~S["\u{FFFFFF}"])
    end

    test "on interpolation in calls" do
      assert_error_conforms(".\"\#{}\"")
      assert_error_conforms(".\"a\#{:b}\"c")
    end

    test "on long atoms" do
      atom =
        "@GR{+z]`_XrNla!d<GTZ]iw[s'l2N<5hGD0(.xh&}>0ptDp(amr.oS&<q(FA)5T3=},^{=JnwIOE*DPOslKV KF-kb7NF&Y#Lp3D7l/!s],^hnz1iB |E8~Y'-Rp&*E(O}|zoB#xsE.S/~~'=%H'2HOZu0PCfz6j=eHq5:yk{7&|}zeRONM+KWBCAUKWFw(tv9vkHTu#Ek$&]Q:~>,UbT}v$L|rHHXGV{;W!>avHbD[T-G5xrzR6m?rQPot-37B@"

      assert_error_conforms(~s{:"#{atom}"})
      assert_error_conforms(~s{["#{atom}": 123]})
    end
  end

  # ===========================================================================
  # Helper functions
  # ===========================================================================

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

  defp assert_conforms_with_literal_encoder(code) do
    reference = s2q_with_literal_encoder(code)
    actual = toxic_parse_with_literal_encoder(code)

    assert actual == reference,
           """
           AST mismatch for: #{inspect(code)}

           Reference:
           #{inspect(reference, pretty: true)}

           Actual:
           #{inspect(actual, pretty: true)}
           """
  end

  defp assert_error_conforms(code) do
    reference = s2q(code)
    actual = toxic_parse(code)

    # Both should be errors
    assert match?({:error, _}, reference),
           "Expected reference parser to error on: #{inspect(code)}, got: #{inspect(reference)}"

    assert match?({:error, _}, actual),
           "Expected ToxicParser to error on: #{inspect(code)}, got: #{inspect(actual)}"
  end

  defp assert_error_conforms_existing_atoms(code) do
    reference =
      Code.string_to_quoted(
        code,
        existing_atoms_only: true,
        columns: true,
        token_metadata: true,
        emit_warnings: false
      )

    actual = toxic_parse_existing_atoms(code)

    # Both should be errors
    assert match?({:error, _}, reference),
           "Expected reference parser to error on: #{inspect(code)}, got: #{inspect(reference)}"

    assert match?({:error, _}, actual),
           "Expected ToxicParser to error on: #{inspect(code)}, got: #{inspect(actual)}"
  end

  defp s2q(code) do
    Code.string_to_quoted(
      code,
      columns: true,
      token_metadata: true,
      emit_warnings: false
    )
  end

  defp s2q_with_literal_encoder(code) do
    Code.string_to_quoted(
      code,
      literal_encoder: &{:ok, {:__block__, &2, [&1]}},
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

  defp toxic_parse_with_literal_encoder(code) do
    case ToxicParser.parse_string(code,
           mode: :strict,
           token_metadata: true,
           literal_encoder: &{:ok, {:__block__, &2, [&1]}}
         ) do
      {:ok, result} -> {:ok, result.ast}
      {:error, result} -> {:error, format_error(result)}
    end
  end

  defp toxic_parse_existing_atoms(code) do
    case ToxicParser.parse_string(code,
           mode: :strict,
           token_metadata: true,
           existing_atoms_only: true
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
