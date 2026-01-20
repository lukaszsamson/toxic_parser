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

    test "expression after keyword list in map" do
      assert_error_conforms("%{foo: 1, :bar => :bar}")
    end

    test "keyword list missing comma in map" do
      assert_error_conforms("%{foo: 1 bar: 2}")
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

    test "unexpected comma inside tuple" do
      assert_error_conforms("{foo 1, 2}")
    end

    test "no-parens trailing comma eof" do
      assert_error_conforms("foo a,", [], false)
    end

    test "no-parens kw list invalid arg" do
      assert_error_conforms("foo a: 1, +", [], false)
    end

    test "no-parens kw list missing item" do
      assert_error_conforms("foo a: 1, , b: 2")
    end

    test "no-parens kw list trailing comma eof" do
      assert_error_conforms("foo a: 1,", [], false)
    end

    test "too many arguments in access syntax" do
      assert_error_conforms("foo[1, 2]")
    end

    test "bracket access missing closer" do
      assert_error_conforms("foo[1", [], false)
    end

    test "bracket empty args" do
      assert_error_conforms("foo[]")
    end

    test "invalid keyword identifier with do" do
      assert_error_conforms("if true do:\n")
    end

    test "invalid keyword identifier" do
      assert_error_conforms("if true else: 1")
    end

    test "kw identifier at expression position" do
      assert_error_conforms("foo:")
    end

    test "binary operator at expression start" do
      assert_error_conforms("* 1")
    end

    test "unicode conversion error in list string" do
      assert_error_conforms("'\\xFF'", [], false)
    end

    test "unexpected closer" do
      assert_error_conforms(")")
    end

    test "mismatched closer" do
      assert_error_conforms("([)")
    end

    test "missing closer at eof" do
      assert_error_conforms("(", [], false)
    end

    test "list missing closer" do
      assert_error_conforms("[1, 2", [], false)
    end

    test "tuple missing closer" do
      assert_error_conforms("{1, 2", [], false)
    end

    test "paren call missing closer" do
      assert_error_conforms("call(1, 2", [], false)
    end

    test "map missing closer" do
      assert_error_conforms("%{a: 1", [], false)
    end

    test "map kw tail trailing comma eof" do
      assert_error_conforms("%{foo: 1,", [], false)
    end

    test "map kw tail unexpected token" do
      assert_error_conforms("%{foo: 1 bar}", [], false)
    end

    test "bitstring missing closer" do
      assert_error_conforms("<<1, 2", [], false)
    end

    test "bitstring no-parens expr" do
      assert_error_conforms("<<foo 1 2>>")
    end

    test "bitstring invalid expr" do
      assert_error_conforms("<<1, +>>", [], false)
    end

    test "paren expression missing closer" do
      assert_error_conforms("(1 + 2", [], false)
    end

    test "binary op missing rhs eof" do
      assert_error_conforms("1 +", [], false)
    end

    test "fn paren patterns missing closer" do
      assert_error_conforms("fn (a, b", [], false)
    end

    test "unexpected end keyword" do
      assert_error_conforms("end", [], false)
    end

    test "unexpected end in expression" do
      assert_error_conforms("1 end")
    end

    test "map invalid open delimiter" do
      assert_error_conforms("%( )")
    end

    test "map unexpected space after percent" do
      assert_error_conforms("% {}", [], false)
    end

    test "keyword missing space after colon" do
      assert_error_conforms("[foo:bar]")
    end

    test "string missing terminator" do
      assert_error_conforms(~S("unclosed), [], false)
    end

    test "interpolation missing terminator" do
      assert_error_conforms(~S("foo #{bar), [], false)
    end

    test "interpolation missing terminator without metadata" do
      assert_error_conforms(~S("foo #{bar), [token_metadata: false], false)
    end

    test "number trailing garbage" do
      assert_error_conforms("0x")
    end

    test "invalid float number" do
      assert_error_conforms("1.0e309")
    end

    test "consecutive semicolons" do
      assert_error_conforms(";;")
    end

    test "alias unexpected paren" do
      assert_error_conforms("Foo()")
    end

    test "dot missing member" do
      assert_error_conforms("Foo.")
    end

    test "dot container missing closer" do
      assert_error_conforms("Foo.{1", [], false)
    end

    test "dot container empty eof" do
      assert_error_conforms("Foo.{", [], false)
    end

    test "alias invalid character" do
      assert_error_conforms("Foöbar")
    end

    test "heredoc missing terminator" do
      assert_error_conforms(~S("""
    unclosed heredoc))
    end

    test "quoted atom missing terminator" do
      assert_error_conforms(~S(:"unclosed), [], false)
    end

    test "quoted call missing terminator" do
      assert_error_conforms(~S(Foo."unclosed), [], false)
    end

    test "capture missing int" do
      assert_error_conforms("&", [], false)
    end

    test "fn missing end" do
      assert_error_conforms("fn -> 1", [], false)
    end

    test "do block missing end" do
      assert_error_conforms("if true do 1", [], false)
    end

    test "case block missing end" do
      assert_error_conforms("case x do 1 -> 2", [], false)
    end

    test "stab invalid placement" do
      assert_error_conforms("fn 1; 2 -> 3 end")
    end

    test "sigil missing terminator" do
      assert_error_conforms(~S(~s"unclosed), [], false)
    end

    test "heredoc invalid header" do
      assert_error_conforms(~S("""invalid))
    end

    test "sigil invalid lowercase name" do
      assert_error_conforms("~ab()")
    end

    test "sigil invalid mixed case name" do
      assert_error_conforms("~Ab()")
    end

    test "sigil invalid delimiter" do
      assert_error_conforms("~s$foo$")
    end

    test "identifier mixed script" do
      assert_error_conforms("fooАbar", [], false)
    end

    test "identifier invalid char" do
      assert_error_conforms("foo@bar")
    end

    test "nonexistent atom with existing_atoms_only" do
      assert_error_conforms(
        ":this_atom_definitely_does_not_exist_xyz123",
        existing_atoms_only: true
      )
    end

    test "comment invalid bidi" do
      assert_error_conforms("# comment with bidi \u202E")
    end

    test "comment invalid linebreak" do
      assert_error_conforms("# comment with line\u2028break")
    end

    test "version control merge conflict marker" do
      assert_error_conforms("<<<<<<< HEAD")
    end

    test "interpolation not allowed in quoted identifier" do
      assert_error_conforms(~S(Foo."bar#{baz}"))
    end

    test "string invalid hex escape" do
      assert_unescape_error("\"\\x\"")
    end

    test "string invalid unicode escape" do
      assert_unescape_error("\"\\u\"")
    end

    test "string invalid unicode codepoint" do
      assert_unescape_error("\"\\u{FFFFFF}\"")
    end

    test "charlist invalid hex escape" do
      assert_unescape_error("'\\x'")
    end

    test "charlist invalid unicode escape" do
      assert_unescape_error("'\\u'")
    end

    test "charlist invalid unicode codepoint" do
      assert_unescape_error("'\\u{FFFFFF}'")
    end

    test "string heredoc invalid hex escape" do
      assert_unescape_error("\"\"\"\n\\x\n\"\"\"")
    end

    test "string heredoc invalid unicode escape" do
      assert_unescape_error("\"\"\"\n\\u\n\"\"\"")
    end

    test "charlist heredoc invalid hex escape" do
      assert_unescape_error("'''\n\\x\n'''")
    end

    test "charlist heredoc invalid unicode escape" do
      assert_unescape_error("'''\n\\u\n'''")
    end

    test "quoted atom invalid hex escape" do
      assert_unescape_error(":\"\\x\"")
    end

    test "quoted atom invalid unicode escape" do
      assert_unescape_error(":\"\\u\"")
    end

    test "quoted keyword invalid hex escape" do
      assert_unescape_error("\"\\x\": 1")
    end

    test "quoted keyword invalid unicode escape" do
      assert_unescape_error("\"\\u\": 1")
    end

    test "quoted call invalid hex escape" do
      assert_unescape_error_raises("Foo.\"\\x\"")
    end

    test "quoted call invalid unicode escape" do
      assert_unescape_error_raises("Foo.\"\\u\"")
    end

    test "quoted call invalid bidi character" do
      assert_bidi_error("Foo.\"\u202A\"")
    end

    test "sigil lowercase invalid delimiter hex escape" do
      assert_error_conforms("~s$\\x$")
    end

    test "sigil uppercase invalid delimiter hex escape" do
      assert_error_conforms("~S$\\x$")
    end

    test "sigil lowercase invalid delimiter unicode escape" do
      assert_error_conforms("~s$\\u$")
    end

    test "sigil uppercase invalid delimiter unicode escape" do
      assert_error_conforms("~S$\\u$")
    end

    test "string invalid bidi character" do
      assert_bidi_error("\"\u202A\"")
    end

    test "charlist invalid bidi character" do
      assert_bidi_error("'\u202A'")
    end

    test "quoted atom invalid bidi character" do
      assert_bidi_error(":\"\u202A\"")
    end

    test "quoted keyword invalid bidi character" do
      assert_bidi_error("\"\u202A\": 1")
    end

    test "string heredoc invalid bidi character" do
      assert_bidi_error("\"\"\"\n\u202A\n\"\"\"")
    end

    test "charlist heredoc invalid bidi character" do
      assert_bidi_error("'''\n\u202A\n'''")
    end

    test "sigil lowercase invalid bidi character" do
      assert_bidi_error("~s\"\u202A\"")
    end

    test "sigil uppercase invalid bidi character" do
      assert_bidi_error("~S\"\u202A\"")
    end

    test "sigil lowercase heredoc invalid bidi character" do
      assert_bidi_error("~s\"\"\"\n\u202A\n\"\"\"")
    end

    test "sigil uppercase heredoc invalid bidi character" do
      assert_bidi_error("~S\"\"\"\n\u202A\n\"\"\"")
    end

    test "sigil lowercase invalid bidi delimiter" do
      assert_error_conforms("~s$\u202A$")
    end

    test "sigil uppercase invalid bidi delimiter" do
      assert_error_conforms("~S$\u202A$")
    end

    test "property 1" do
      assert_error_conforms("foo do :ok end  do :error end")
    end

    test "property 2" do
      assert_error_conforms("for x <- [1], 0@, do: x")
    end

    test "property 3" do
      assert_error_conforms("%{x | }")
    end

    test "property 4" do
      assert_error_conforms("\"\#{!}\"")
    end

    test "property 5" do
      assert_error_conforms("-(b")
    end

    test "property 6" do
      assert_error_conforms("case x do 1 -> @ end")
    end

    test "property 7" do
      assert_error_conforms("foo 1, (b")
    end

    test "property 8" do
      assert_error_conforms("[a: ]")
    end

    test "property 9" do
      assert_error_conforms("<<a::0!>>")
    end

    test "property 10" do
      assert_error_conforms("fn x when { -> 1 end")
    end

    test "property 11" do
      assert_error_conforms("fn () when / -> :ok end")
    end

    test "property 12" do
      assert_error_conforms("A.0.foo()")
    end

    test "property 13" do
      assert_error_conforms("[\"foo\#{i<}\": 1]")
    end

    test "property 14" do
      assert_error_conforms("<<a, s:  >>")
    end

    test "property 15" do
      assert_error_conforms("x + { * y")
    end

    test "property 16" do
      assert_error_conforms("foo do :ok end 0")
    end

    test "property 17" do
      assert_error_conforms("{/")
    end

    test "property 18" do
      assert_error_conforms("foo do ! end")
    end

    test "property 19" do
      assert_error_conforms("%e+{}")
    end

    test "property 20" do
      assert_error_conforms(":ok |> (b")
    end

    test "property 21" do
      assert_error_conforms("foo(a: 1, 0)")
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

  defp assert_error_conforms(code, opts, validate_token_meta?) do
    reference = s2q(code, opts)
    actual = toxic_parse(code, opts)

    assert match?({:error, _}, reference),
           "Expected reference parser to error on: #{inspect(code)}"

    case {reference, actual, validate_token_meta?} do
      {{:error, {[line: line, column: column], msg, token}},
       {:error, {[line: line, column: column], msg, token}}, false} ->
        :ok

      {{:error, {meta_ref, msg, token}}, {:error, {meta_act, msg, token}}, false}
      when is_list(meta_ref) and is_list(meta_act) ->
        :ok

      _ ->
        assert actual == reference,
               """
               Error mismatch for: #{inspect(code)}

               Reference:
               #{inspect(reference, pretty: true)}

               Actual:
               #{inspect(actual, pretty: true)}
               """
    end
  end

  defp assert_unescape_error(code, opts \\ []) do
    reference = s2q(code, opts)
    actual = toxic_parse(code, opts)

    assert match?({:error, _}, reference),
           "Expected reference parser to error on: #{inspect(code)}"

    case {reference, actual} do
      {{:error, {[line: line, column: _column], message, token}},
       {:error, {:unescape_error, actual_message, actual_meta}}} ->
        assert actual_message == String.replace(message, ". Syntax error after: ", "")
        assert token in ["\\x", "\\u"]
        assert Keyword.get(actual_meta, :line) == line

      _ ->
        assert actual == reference,
               """
               Error mismatch for: #{inspect(code)}

               Reference:
               #{inspect(reference, pretty: true)}

               Actual:
               #{inspect(actual, pretty: true)}
               """
    end
  end

  defp assert_unescape_error_raises(code) do
    reference = s2q(code, [])

    assert match?({:error, _}, reference),
           "Expected reference parser to error on: #{inspect(code)}"

    assert_raise ArgumentError, fn ->
      toxic_parse(code, [])
    end
  end

  defp assert_bidi_error(code, opts \\ []) do
    reference = s2q(code, opts)
    actual = toxic_parse(code, opts)

    assert match?({:error, _}, reference),
           "Expected reference parser to error on: #{inspect(code)}"

    case {reference, actual} do
      {{:error, {[line: line, column: column], {prefix, suffix}, token}},
       {:error, {[line: line, column: column], message, token}}}
      when is_binary(message) ->
        assert message == "#{prefix}#{token}#{suffix}"

      _ ->
        assert actual == reference,
               """
               Error mismatch for: #{inspect(code)}

               Reference:
               #{inspect(reference, pretty: true)}

               Actual:
               #{inspect(actual, pretty: true)}
               """
    end
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
