defmodule ToxicParser.TolerantModeTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias ToxicParser.Result

  @default_opts [columns: true, token_metadata: true, emit_warnings: false]
  @moduletag :skip

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
    %{name: "unexpected comma inside containers", code: "[foo 1, 2]"},
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
    %{name: "alias invalid character", code: "Fo\\u00F6bar"},
    %{name: "heredoc missing terminator", code: "\"\"\"\n    unclosed heredoc"},
    %{name: "heredoc invalid header", code: "\"\"\"invalid"},
    %{name: "sigil invalid lowercase name", code: "~ab()"},
    %{name: "sigil invalid mixed case name", code: "~Ab()"},
    %{name: "sigil invalid delimiter", code: "~s$foo$"},
    %{name: "identifier mixed script", code: "foo\\u0410bar"},
    %{name: "identifier invalid char", code: "foo@bar"},
    %{
      name: "nonexistent atom with existing_atoms_only",
      code: ":this_atom_definitely_does_not_exist_xyz123",
      opts: [existing_atoms_only: true]
    },
    %{name: "comment invalid bidi", code: "# comment with bidi \\u202E"},
    %{name: "comment invalid linebreak", code: "# comment with line\\u2028break"},
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
    %{name: "quoted keyword invalid hex escape", code: "\"\\x\": 1"},
    %{name: "quoted keyword invalid unicode escape", code: "\"\\u\": 1"},
    %{name: "quoted call invalid hex escape", code: "Foo.\"\\x\""},
    %{name: "quoted call invalid unicode escape", code: "Foo.\"\\u\""},
    %{name: "quoted call invalid bidi character", code: "Foo.\"\\u202A\""},
    %{name: "sigil lowercase invalid delimiter hex escape", code: "~s$\\x$"},
    %{name: "sigil uppercase invalid delimiter hex escape", code: "~S$\\x$"},
    %{name: "sigil lowercase invalid delimiter unicode escape", code: "~s$\\u$"},
    %{name: "sigil uppercase invalid delimiter unicode escape", code: "~S$\\u$"},
    %{name: "string invalid bidi character", code: "\"\\u202A\""},
    %{name: "charlist invalid bidi character", code: "'\\u202A'"},
    %{name: "quoted atom invalid bidi character", code: ":\"\\u202A\""},
    %{name: "quoted keyword invalid bidi character", code: "\"\\u202A\": 1"},
    %{name: "string heredoc invalid bidi character", code: "\"\"\"\n\\u202A\n\"\"\""},
    %{name: "charlist heredoc invalid bidi character", code: "'''\n\\u202A\n'''"},
    %{name: "sigil lowercase invalid bidi character", code: "~s\"\\u202A\""},
    %{name: "sigil uppercase invalid bidi character", code: "~S\"\\u202A\""},
    %{name: "sigil lowercase heredoc invalid bidi character", code: "~s\"\"\"\n\\u202A\n\"\"\""},
    %{name: "sigil uppercase heredoc invalid bidi character", code: "~S\"\"\"\n\\u202A\n\"\"\""},
    %{name: "sigil lowercase invalid bidi delimiter", code: "~s$\\u202A$"},
    %{name: "sigil uppercase invalid bidi delimiter", code: "~S$\\u202A$"}
  ]

  for %{name: name, code: code} = data <- @error_cases do
    opts = Map.get(data, :opts, [])

    test "#{name} recovers and emits error node" do
      opts = opts_for(unquote(name), unquote(opts))
      source = with_following(unquote(code))

      assert {:ok, %Result{} = result} = parse_tolerant(source, opts)

      error_nodes = collect_error_nodes(result.ast)
      assert error_nodes != []

      {_, error_meta, payload} = hd(error_nodes)
      assert_error_payload(payload)

      diagnostic = find_diagnostic(result, payload.diag_id)
      assert diagnostic != nil
      assert payload.phase == diagnostic.phase
      assert payload.diag_id == diagnostic.details[:id]
      assert diagnostic.details[:anchor][:kind] == :error_node
      assert_recovered_expression(result.ast, error_meta)
    end
  end

  defp parse_tolerant(code, opts) do
    ToxicParser.parse_string(code, Keyword.merge(@default_opts, opts) |> Keyword.put(:mode, :tolerant))
  end

  defp with_following(code), do: code <> "\n2"

  defp collect_error_nodes(ast) do
    {_ast, acc} =
      Macro.prewalk(ast, [], fn
        {:__error__, _meta, _payload} = node, acc -> {node, [node | acc]}
        node, acc -> {node, acc}
      end)

    Enum.reverse(acc)
  end

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
  end

  defp assert_recovered_expression(ast, error_meta) do
    exprs = expr_list(ast)
    assert Enum.any?(exprs, &(&1 == 2))
    assert error_meta != []
  end

  defp expr_list({:__block__, _meta, exprs}), do: exprs
  defp expr_list(ast), do: [ast]

  defp opts_for("literal encoder error", base_opts) do
    Keyword.put(base_opts, :literal_encoder, &literal_encoder_fail/2)
  end

  defp opts_for(_name, base_opts), do: base_opts

  defp literal_encoder_fail(_literal, _meta), do: {:error, "ENCODER_FAIL"}
end
