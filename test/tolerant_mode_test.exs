defmodule ToxicParser.TolerantModeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias ToxicParser.Result

  @default_opts [columns: true, token_metadata: true, emit_warnings: false]
  @moduletag :skip
  @enabled_cases [
    "unexpected closer",
    "unexpected end keyword",
    "unexpected end in expression",
    "number trailing garbage",
    "invalid float number",
    "consecutive semicolons",
    "keyword list inside tuple",
    "keyword list inside bitstring",
    "expression after keyword list in list",
    "expression after keyword list in call",
    "expression after keyword list in map",
    "unexpected comma inside containers",
    "unexpected comma inside tuple",
    "unexpected comma inside map",
    "invalid expression inside paren call",
    "too many arguments in access syntax",
    "string missing terminator",
    "interpolation missing terminator",
    "heredoc missing terminator",
    "string invalid hex escape",
    "interpolation missing terminator without metadata",
    "string invalid unicode escape",
    "string invalid unicode codepoint",
    "charlist invalid hex escape",
    "charlist invalid unicode escape",
    "charlist invalid unicode codepoint",
    "string heredoc invalid hex escape",
    "string heredoc invalid unicode escape",
    "charlist heredoc invalid hex escape",
    "charlist heredoc invalid unicode escape",
    "quoted atom invalid hex escape",
    "quoted atom invalid unicode escape",
    "quoted keyword invalid hex escape",
    "quoted keyword invalid unicode escape",
    "quoted call invalid hex escape",
    "quoted call invalid unicode escape",
    "quoted call invalid bidi character",
    "sigil lowercase invalid delimiter hex escape",
    "sigil uppercase invalid delimiter hex escape",
    "sigil lowercase invalid delimiter unicode escape",
    "sigil uppercase invalid delimiter unicode escape",
    "string invalid bidi character",
    "charlist invalid bidi character",
    "quoted atom invalid bidi character",
    "quoted keyword invalid bidi character",
    "string heredoc invalid bidi character",
    "charlist heredoc invalid bidi character",
    "sigil lowercase invalid bidi character",
    "sigil uppercase invalid bidi character",
    "sigil lowercase heredoc invalid bidi character",
    "sigil uppercase heredoc invalid bidi character",
    "sigil lowercase invalid bidi delimiter",
    "sigil uppercase invalid bidi delimiter",
    "heredoc invalid header",
    "sigil invalid lowercase name",
    "sigil invalid mixed case name",
    "sigil invalid delimiter",
    "interpolation not allowed in quoted identifier",
    "literal encoder error",
    "range step operator must follow range",
    "atom cannot be followed by alias",
    "fn without -> clauses",
    "unexpected -> placement",
    "unexpected parentheses due to space",
    "unexpected comma in nested calls",
    "no-parens arg after comma",
    "invalid keyword identifier with do",
    "invalid keyword identifier",
    "unicode conversion error in list string",
    "mismatched closer",
    "missing closer at eof",
    "map invalid open delimiter",
    "map unexpected space after percent",
    "keyword missing space after colon",
    "alias unexpected paren",
    "alias invalid character",
    "identifier mixed script",
    "identifier invalid char",
    "nonexistent atom with existing_atoms_only",
    "comment invalid bidi",
    "comment invalid linebreak",
    "version control merge conflict marker",
    "quoted atom missing terminator",
    "sigil missing terminator",
    "quoted call missing terminator",
    "kw identifier at expression position",
    "binary operator at expression start",
    "dot missing member",
    "dot container missing closer",
    "bracket access missing closer",
    "bracket empty args",
    "capture missing int",
    "fn missing end",
    "do block missing end",
    "case block missing end",
    "stab invalid placement",
    "list missing closer",
    "tuple missing closer",
    "paren call missing closer",
    "map missing closer",
    "map kw tail trailing comma eof",
    "bitstring missing closer",
    "paren expression missing closer",
    "binary op missing rhs eof",
    "fn paren patterns missing closer",
    "bitstring no-parens expr",
    "map kw tail missing comma",
    "no-parens trailing comma eof",
    "no-parens kw list invalid arg",
    "map kw tail unexpected token",
    "no-parens kw list missing item",
    "no-parens kw list trailing comma eof",
    "bitstring invalid expr"
  ]

  @error_cases [
    %{
      name: "literal encoder error",
      code: "1"
    },
    %{name: "range step operator must follow range", code: "foo++bar//bat"},
    %{name: "atom cannot be followed by alias", code: ":foo.Bar"},
    %{name: "fn without -> clauses", code: "fn 1 end"},
    %{name: "unexpected -> placement", code: "fn 1\n2 -> 3 end"},
    %{name: "keyword list inside tuple", code: "{foo: :bar}"},
    %{name: "keyword list inside bitstring", code: "<<foo: :bar, baz: :bar>>"},
    %{name: "expression after keyword list in call", code: "call foo: 1, :bar"},
    %{name: "expression after keyword list in list", code: "[foo: 1, :bar]"},
    %{name: "expression after keyword list in map", code: "%{foo: 1, :bar => :bar}"},
    %{name: "unexpected parentheses due to space", code: "foo (hello, world)"},
    %{name: "unexpected comma in nested calls", code: "foo 1, foo 2, 3"},
    %{name: "no-parens arg after comma", code: "foo a, b c, d", no_following: true},
    %{name: "unexpected comma inside containers", code: "[foo 1, 2]"},
    %{name: "unexpected comma inside tuple", code: "{foo 1, 2}"},
    %{name: "unexpected comma inside map", code: "%{foo 1, bar: 2}"},
    %{name: "invalid expression inside paren call", code: "call(1 + *, 2)"},
    %{name: "too many arguments in access syntax", code: "foo[1, 2]"},
    %{name: "invalid keyword identifier with do", code: "if true do:\n"},
    %{name: "invalid keyword identifier", code: "if true else: 1"},
    %{name: "unicode conversion error in list string", code: "'\\xFF'"},
    %{name: "unexpected closer", code: ")"},
    %{name: "mismatched closer", code: "([)"},
    %{name: "missing closer at eof", code: "("},
    %{name: "unexpected end keyword", code: "end"},
    %{name: "unexpected end in expression", code: "1 end"},
    %{name: "map invalid open delimiter", code: "%( )"},
    %{name: "map unexpected space after percent", code: "% {}"},
    %{name: "keyword missing space after colon", code: "[foo:bar]"},
    %{name: "string missing terminator", code: ~S("unclosed)},
    %{name: "interpolation missing terminator", code: ~S("foo #{bar)},
    %{
      name: "interpolation missing terminator without metadata",
      code: ~S("foo #{bar),
      opts: [token_metadata: false]
    },
    %{name: "number trailing garbage", code: "0x"},
    %{name: "invalid float number", code: "1.0e309"},
    %{name: "consecutive semicolons", code: ";;"},
    %{name: "alias unexpected paren", code: "Foo()"},
    %{name: "alias invalid character", code: "Foöbar"},
    %{name: "heredoc missing terminator", code: "\"\"\"\n    unclosed heredoc"},
    %{name: "heredoc invalid header", code: "\"\"\"invalid"},
    %{name: "sigil invalid lowercase name", code: "~ab()"},
    %{name: "sigil invalid mixed case name", code: "~Ab()"},
    %{name: "sigil invalid delimiter", code: "~s$foo$"},
    %{name: "sigil missing terminator", code: ~S(~s"unclosed)},
    %{name: "identifier mixed script", code: "fooАbar"},
    %{name: "identifier invalid char", code: "foo@bar"},
    %{
      name: "nonexistent atom with existing_atoms_only",
      code: ":this_atom_definitely_does_not_exist_xyz123",
      opts: [existing_atoms_only: true]
    },
    %{name: "comment invalid bidi", code: "# comment with bidi \u202E"},
    %{name: "comment invalid linebreak", code: "# comment with line\u2028break"},
    %{name: "version control merge conflict marker", code: "<<<<<<< HEAD"},
    %{name: "interpolation not allowed in quoted identifier", code: ~S(Foo."bar#{baz}")},
    %{name: "string invalid hex escape", code: "\"\\x\""},
    %{name: "string invalid unicode escape", code: "\"\\u\""},
    %{name: "string invalid unicode codepoint", code: "\"\\u{FFFFFF}\""},
    %{name: "charlist invalid hex escape", code: "'\\x'"},
    %{name: "charlist invalid unicode escape", code: "'\\u'"},
    %{name: "charlist invalid unicode codepoint", code: "'\\u{FFFFFF}'"},
    %{name: "string heredoc invalid hex escape", code: "\"\"\"\n\\x\n\"\"\""},
    %{name: "string heredoc invalid unicode escape", code: "\"\"\"\n\\u\n\"\"\""},
    %{name: "charlist heredoc invalid hex escape", code: "'''\n\\x\n'''"},
    %{name: "charlist heredoc invalid unicode escape", code: "'''\n\\u\n'''"},
    %{name: "quoted atom invalid hex escape", code: ":\"\\x\""},
    %{name: "quoted atom invalid unicode escape", code: ":\"\\u\""},
    %{name: "quoted atom missing terminator", code: ~S(:"unclosed)},
    %{name: "quoted keyword invalid hex escape", code: "\"\\x\": 1"},
    %{name: "quoted keyword invalid unicode escape", code: "\"\\u\": 1"},
    %{name: "quoted call invalid hex escape", code: "Foo.\"\\x\""},
    %{name: "quoted call invalid unicode escape", code: "Foo.\"\\u\""},
    %{name: "quoted call invalid bidi character", code: "Foo.\"\u202A\""},
    %{name: "quoted call missing terminator", code: ~S(Foo."unclosed)},
    %{name: "kw identifier at expression position", code: "foo:", no_following: true},
    %{name: "binary operator at expression start", code: "* 1"},
    %{name: "dot missing member", code: "Foo."},
    %{name: "dot container missing closer", code: "Foo.{1", no_following: true},
    %{name: "bracket access missing closer", code: "foo[1", no_following: true},
    %{name: "bracket empty args", code: "foo[]"},
    %{name: "capture missing int", code: "&", no_following: true},
    %{name: "fn missing end", code: "fn -> 1", no_following: true},
    %{name: "do block missing end", code: "if true do 1", no_following: true},
    %{name: "case block missing end", code: "case x do 1 -> 2", no_following: true},
    %{name: "stab invalid placement", code: "fn 1; 2 -> 3 end"},
    %{name: "list missing closer", code: "[1, 2", no_following: true},
    %{name: "tuple missing closer", code: "{1, 2", no_following: true},
    %{name: "paren call missing closer", code: "call(1, 2", no_following: true},
    %{name: "map missing closer", code: "%{a: 1", no_following: true},
    %{name: "map kw tail trailing comma eof", code: "%{foo: 1,", no_following: true},
    %{name: "bitstring missing closer", code: "<<1, 2", no_following: true},
    %{name: "paren expression missing closer", code: "(1 + 2", no_following: true},
    %{name: "binary op missing rhs eof", code: "1 +", no_following: true},
    %{name: "fn paren patterns missing closer", code: "fn (a, b", no_following: true},
    %{name: "bitstring no-parens expr", code: "<<foo 1 2>>"},
    %{name: "map kw tail missing comma", code: "%{foo: 1 bar: 2}"},
    %{name: "no-parens trailing comma eof", code: "foo a,", no_following: true},
    %{name: "no-parens kw list invalid arg", code: "foo a: 1, +", no_following: true},
    %{name: "map kw tail unexpected token", code: "%{foo: 1 bar}", no_following: true},
    %{name: "no-parens kw list missing item", code: "foo a: 1, , b: 2"},
    %{name: "no-parens kw list trailing comma eof", code: "foo a: 1,", no_following: true},
    %{name: "bitstring invalid expr", code: "<<1, +>>"},
    %{name: "sigil lowercase invalid delimiter hex escape", code: "~s$\\x$"},
    %{name: "sigil uppercase invalid delimiter hex escape", code: "~S$\\x$"},
    %{name: "sigil lowercase invalid delimiter unicode escape", code: "~s$\\u$"},
    %{name: "sigil uppercase invalid delimiter unicode escape", code: "~S$\\u$"},
    %{name: "string invalid bidi character", code: "\"\u202A\""},
    %{name: "charlist invalid bidi character", code: "'\u202A'"},
    %{name: "quoted atom invalid bidi character", code: ":\"\u202A\""},
    %{name: "quoted keyword invalid bidi character", code: "\"\u202A\": 1"},
    %{name: "string heredoc invalid bidi character", code: "\"\"\"\n\u202A\n\"\"\""},
    %{name: "charlist heredoc invalid bidi character", code: "'''\n\u202A\n'''"},
    %{name: "sigil lowercase invalid bidi character", code: "~s\"\u202A\""},
    %{name: "sigil uppercase invalid bidi character", code: "~S\"\u202A\""},
    %{name: "sigil lowercase heredoc invalid bidi character", code: "~s\"\"\"\n\u202A\n\"\"\""},
    %{name: "sigil uppercase heredoc invalid bidi character", code: "~S\"\"\"\n\u202A\n\"\"\""},
    %{name: "sigil lowercase invalid bidi delimiter", code: "~s$\u202A$"},
    %{name: "sigil uppercase invalid bidi delimiter", code: "~S$\u202A$"}
  ]

  for %{name: name, code: code} = data <- @error_cases do
    opts = Map.get(data, :opts, [])
    no_following? = Map.get(data, :no_following, false)

    if name in @enabled_cases do
      @tag skip: false
    end

    test "#{name} recovers and emits error node" do
      opts = opts_for(unquote(name), unquote(opts))
      source =
        if unquote(no_following?) do
          unquote(code)
        else
          with_following(unquote(code))
        end

      assert {:ok, %Result{} = result} = parse_tolerant(source, opts)

      error_nodes = collect_error_nodes(result.ast)
      assert error_nodes != []

      {_, error_meta, payload} = hd(error_nodes)
      assert_error_payload(payload)
      assert_error_meta_consistency(error_meta, payload)

      diagnostic = find_diagnostic(result, payload.diag_id)
      assert diagnostic != nil
      assert payload.phase == diagnostic.phase
      assert payload.diag_id == diagnostic.details[:id]
      assert diagnostic.details[:anchor][:kind] == :error_node
      assert_recovered_expression(result.ast, error_meta, unquote(name))
      assert_preserves_valid_siblings(result.ast, unquote(name))
      assert_synthetic_meta(result.ast, error_meta, unquote(name))
    end
  end

  defp parse_tolerant(code, opts) do
    ToxicParser.parse_string(code, Keyword.merge(@default_opts, opts) |> Keyword.put(:mode, :tolerant))
  end

  defp with_following(code), do: code <> "\n2"

  defp collect_error_nodes(ast) do
    Enum.reverse(collect_error_nodes(ast, []))
  end

  defp collect_error_nodes({:__error__, _meta, _payload} = node, acc), do: [node | acc]

  defp collect_error_nodes({fun, _meta, args}, acc) when is_list(args) do
    acc = collect_error_nodes(fun, acc)
    Enum.reduce(args, acc, fn arg, acc -> collect_error_nodes(arg, acc) end)
  end

  defp collect_error_nodes(tuple, acc) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> Enum.reduce(acc, fn item, acc -> collect_error_nodes(item, acc) end)
  end

  defp collect_error_nodes(list, acc) when is_list(list) do
    Enum.reduce(list, acc, fn item, acc -> collect_error_nodes(item, acc) end)
  end

  defp collect_error_nodes(_other, acc), do: acc

  defp find_diagnostic(%Result{diagnostics: diagnostics}, diag_id) do
    Enum.find(diagnostics, fn diag -> diag.details[:id] == diag_id end)
  end

  defp assert_error_payload(payload) do
    assert is_map(payload)
    assert is_integer(payload.diag_id)
    assert payload.phase in [:lexer, :parser]
    assert payload.kind in [:token, :missing, :unexpected, :invalid, :ambiguous, :internal]
    assert Map.has_key?(payload, :synthetic?)
    assert Map.has_key?(payload, :original)
    assert is_list(payload.children)
  end

  defp assert_recovered_expression(ast, error_meta, name) do
    exprs = expr_list(ast)
    container_names = [
      "unexpected comma inside containers",
      "unexpected comma inside tuple",
      "unexpected comma inside map",
      "invalid expression inside paren call"
    ]
    no_following_names = [
      "kw identifier at expression position",
      "capture missing int",
      "fn missing end",
      "do block missing end",
      "case block missing end",
      "list missing closer",
      "tuple missing closer",
      "paren call missing closer",
      "map missing closer",
      "map kw tail trailing comma eof",
      "bitstring missing closer",
      "paren expression missing closer",
      "binary op missing rhs eof",
      "fn paren patterns missing closer",
      "dot container missing closer",
      "bracket access missing closer",
      "bracket empty args",
      "no-parens arg after comma",
      "no-parens trailing comma eof",
      "no-parens kw list invalid arg",
      "map kw tail unexpected token",
      "no-parens kw list missing item",
      "no-parens kw list trailing comma eof",
      "bitstring invalid expr"
    ]

    cond do
      name in ["literal encoder error"] ->
        assert error_meta != []

      name in no_following_names ->
        assert error_meta != []

      name in container_names ->
        [container_ast | _] = exprs
        assert_container_error_and_two(container_ast)
        assert Enum.any?(exprs, &(&1 == 2))
        assert error_meta != []

      name in ["quoted call missing terminator"] ->
        assert has_literal_two?(ast)
        assert error_meta != []

      name in ["dot missing member"] ->
        assert has_literal_two?(ast)
        assert error_meta != []

      true ->
        assert Enum.any?(exprs, &(&1 == 2))
        assert error_meta != []
    end
  end

  defp expr_list({:__block__, _meta, exprs}), do: exprs
  defp expr_list(ast), do: [ast]

  defp assert_error_meta_consistency(error_meta, payload) do
    case Keyword.get(error_meta, :toxic) do
      %{synthetic?: synthetic?} -> assert payload.synthetic? == synthetic?
      _ -> :ok
    end
  end

  defp assert_preserves_valid_siblings(ast, name) do
    if name in [
         "expression after keyword list in call",
         "expression after keyword list in list",
         "expression after keyword list in map"
       ] do
      assert has_foo_keyword?(ast)
    end
  end

  defp assert_synthetic_meta(_ast, error_meta, "fn without -> clauses") do
    assert match?(%{synthetic?: false}, Keyword.get(error_meta, :toxic, %{}))
  end

  defp assert_synthetic_meta(_ast, _error_meta, _name), do: :ok

  defp has_foo_keyword?(ast) do
    has_foo_keyword?(ast, false)
  end

  defp has_foo_keyword?({:__error__, _meta, _payload}, found), do: found

  defp has_foo_keyword?({:%{}, _meta, kvs}, found) when is_list(kvs) do
    found =
      found or
        Enum.any?(kvs, fn entry ->
          match?({:foo, 1}, entry) or match?({:foo, 1, _}, entry)
        end)

    has_foo_keyword?(kvs, found)
  end

  defp has_foo_keyword?({_, _meta, args}, found) when is_list(args) do
    has_foo_keyword?(args, found)
  end

  defp has_foo_keyword?(list, found) when is_list(list) do
    cond do
      found ->
        true

      Keyword.keyword?(list) ->
        Keyword.get(list, :foo) == 1

      true ->
        Enum.any?(list, fn entry -> has_foo_keyword?(entry, false) end)
    end
  end

  defp has_foo_keyword?(_other, found), do: found

  defp assert_container_error_and_two(ast) do
    case container_elements(ast) do
      elements when is_list(elements) ->
        assert collect_error_nodes(elements) != []
        assert has_literal_two?(elements)

      _ ->
        flunk("expected container AST with elements, got: #{inspect(ast)}")
    end
  end

  defp container_elements(list) when is_list(list), do: list
  defp container_elements({:{}, _meta, elements}) when is_list(elements), do: elements
  defp container_elements({:%{}, _meta, elements}) when is_list(elements), do: elements

  defp container_elements(tuple) when is_tuple(tuple) and tuple_size(tuple) == 2,
    do: Tuple.to_list(tuple)

  defp container_elements({name, _meta, args}) when is_atom(name) and is_list(args),
    do: args

  defp container_elements(_), do: nil

  defp has_literal_two?(ast), do: has_literal_two?(ast, false)

  defp has_literal_two?(2, _found), do: true
  defp has_literal_two?({:__error__, _meta, _payload}, found), do: found

  defp has_literal_two?({fun, _meta, args}, found) when is_list(args) do
    found = has_literal_two?(fun, found)
    Enum.reduce(args, found, fn arg, acc -> has_literal_two?(arg, acc) end)
  end

  defp has_literal_two?(tuple, found) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> Enum.reduce(found, fn item, acc -> has_literal_two?(item, acc) end)
  end

  defp has_literal_two?(list, found) when is_list(list) do
    Enum.reduce(list, found, fn item, acc -> has_literal_two?(item, acc) end)
  end

  defp has_literal_two?(_other, found), do: found

  defp opts_for("literal encoder error", base_opts) do
    Keyword.put(base_opts, :literal_encoder, &literal_encoder_fail/2)
  end

  defp opts_for(_name, base_opts), do: base_opts

  defp literal_encoder_fail(_literal, _meta), do: {:error, "ENCODER_FAIL"}
end
