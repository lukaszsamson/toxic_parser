defmodule ToxicParser.WarningsTest do
  @moduledoc false

  use ExUnit.Case, async: true

  test "deprecated not expr1 in expr2" do
    assert_warning_conforms("not left in right")
  end

  test "ambiguous pipe into call" do
    assert_warning_conforms("""
    [5, 6, 7, 3]
    |> Enum.map_join "", &(Integer.to_string(&1))
    |> String.to_integer
    """)
  end

  test "missing parens after operator" do
    assert_warning_conforms("""
    quote do
      case do
      end || raise 1, 2
    end
    """)
  end

  test "missing parens inside keyword" do
    assert_warning_conforms("""
    quote do
      IO.inspect arg, label: if true, do: "foo", else: "baz"
    end
    """)
  end

  test "trailing comma in call" do
    assert_warning_conforms("Keyword.merge([], foo: 1,)")
  end

  test "empty parentheses expression" do
    assert_warning_conforms("()")
  end

  test "empty stab clause" do
    assert_warning_conforms("fn x -> end")
  end

  test "character literal with escape followed by non-escaped char" do
    assert_lexer_warning_conforms("?\\a")
  end

  test "character literal unknown escape sequence" do
    assert_lexer_warning_conforms("?\\q")
  end

  test "character literal non-escaped special char" do
    assert_lexer_warning_conforms("?\\t")
  end

  test "single-quoted heredoc charlist" do
    assert_lexer_warning_conforms("'''\nhello\n'''")
  end

  test "single-quoted string charlist" do
    assert_lexer_warning_conforms("'hello'")
  end

  test "triple colon atom ambiguity" do
    assert_lexer_warning_conforms(":::")
  end

  test "single quotes around atoms deprecated" do
    assert_lexer_warning_conforms(":'1hello'")
  end

  test "unnecessary quotes around atoms" do
    assert_lexer_warning_conforms(":'hello'")
    assert_lexer_warning_conforms(":\"hello\"")
  end

  test "single quotes around keyword identifier deprecated" do
    assert_lexer_warning_conforms("'1hello': 1")
  end

  test "unnecessary quotes around keyword identifier" do
    assert_lexer_warning_conforms("'hello': 1")
    assert_lexer_warning_conforms("\"hello\": 1")
  end

  test "four ampersands warning" do
    assert_lexer_warning_conforms("&&&& true")
  end

  test "four pipes warning" do
    assert_lexer_warning_conforms("|||| true")
  end

  test "four plus warning" do
    assert_lexer_warning_conforms("++++")
  end

  test "four minus warning" do
    assert_lexer_warning_conforms("----")
  end

  test "four caret warning" do
    assert_lexer_warning_conforms("^^^^")
  end

  test "ambiguous bang before equals in atom" do
    assert_lexer_warning_conforms(":foo!= 1")
  end

  test "ambiguous bang before equals in identifier" do
    assert_lexer_warning_conforms("foo!= 1")
  end

  test "ambiguous question before equals in atom" do
    assert_lexer_warning_conforms(":foo?= 1")
  end

  test "ambiguous question before equals in identifier" do
    assert_lexer_warning_conforms("foo?= 1")
  end

  test "deprecated xor operator ^^^" do
    assert_lexer_warning_conforms("1 ^^^ 2")
  end

  test "deprecated bnot operator ~~~" do
    assert_lexer_warning_conforms("~~~1")
  end

  test "deprecated pipe operator <|>" do
    assert_lexer_warning_conforms("1 <|> 2")
  end

  test "single quotes around calls deprecated" do
    assert_lexer_warning_conforms("Foo.'1bar'")
  end

  test "unnecessary quotes around calls" do
    assert_lexer_warning_conforms("Foo.'bar'")
    assert_lexer_warning_conforms("Foo.\"bar\"")
    assert_lexer_warning_conforms("Foo.\"bar\"()")
    assert_lexer_warning_conforms("Foo.\"bar\"[]")
    assert_lexer_warning_conforms("Foo.\"bar\" +1")
    assert_lexer_warning_conforms("Foo.\"bar\" do\n:ok\nend")
  end

  test "escaped delimiter in uppercase sigil" do
    assert_lexer_warning_conforms("~S|foo\\|")
  end

  defp assert_warning_conforms(code) do
    {reference, warnings} =
      Code.with_diagnostics(fn ->
        Code.string_to_quoted(code)
      end)

    assert match?({:ok, _}, reference),
           "Expected reference parser to succeed on: #{inspect(code)}"

    warning_messages =
      warnings
      |> Enum.map(& &1.message)

    assert warning_messages != [], "Expected reference warnings for: #{inspect(code)}"

    {:ok, result} =
      ToxicParser.parse_string(code, mode: :strict, emit_warnings: true)

    toxic_messages =
      result.warnings
      |> Enum.map(& &1.message)

    assert toxic_messages == warning_messages,
           """
           Warning mismatch for: #{inspect(code)}

           Reference:
           #{inspect(warning_messages, pretty: true)}

           Toxic:
           #{inspect(toxic_messages, pretty: true)}
           """
  end

  defp assert_lexer_warning_conforms(code) do
    charlist = to_charlist(code)

    elixir_warnings =
      case :elixir_tokenizer.tokenize(charlist, 1, 1, existing_atoms_only: false) do
        {:ok, _line, _column, elixir_warnings, _tokens, _terminators} ->
          elixir_warnings

        {:error, reason, _rest, _line, _column, _warnings, _tokens} ->
          case reason do
            {:missing_terminator, %{warnings: warnings}} -> warnings
            _ -> flunk("Elixir tokenizer failed: #{inspect(reason)}")
          end

        {:error, {meta, warnings, _terminators}, _rest, _line, _column} when is_list(warnings) ->
          if warnings == [] do
            flunk("Elixir tokenizer failed: #{inspect(meta)}")
          else
            warnings
          end

        {:error, error} ->
          flunk("Elixir tokenizer failed: #{inspect(error)}")
      end

    result =
      case ToxicParser.parse_string(code, mode: :strict, emit_warnings: true) do
        {:ok, result} -> result
        {:error, result} -> result
      end

    toxic_warnings =
      result.warnings
      |> Enum.filter(&match?(%Toxic.Warning{}, &1))
      |> Enum.reverse()

    Enum.each(elixir_warnings, fn
      {ref_pos, ref_msg} ->
        ref_msg = IO.iodata_to_binary(ref_msg)

        assert Enum.any?(toxic_warnings, fn %Toxic.Warning{details: details} = warning ->
                 line = details.line
                 column = details.column

                 msg =
                   case details do
                     %{message: message} -> IO.iodata_to_binary(message)
                     _ -> format_lexer_warning_message(warning)
                   end

                 {line, column} == ref_pos and msg == ref_msg
               end),
               "Expected lexer warning #{inspect(ref_pos)} #{inspect(ref_msg)}"

      _ ->
        :ok
    end)
  end

  defp format_lexer_warning_message(%Toxic.Warning{
         code: :deprecated_charlist,
         details: %{suggestion: _}
       }) do
    "single-quoted string represent charlists. Use ~c''' if you indeed want a charlist or use \"\"\" instead"
  end

  defp format_lexer_warning_message(%Toxic.Warning{
         code: :deprecated_single_quote_atom,
         details: %{suggestion: _}
       }) do
    "single quotes around atoms are deprecated. Use double quotes instead"
  end

  defp format_lexer_warning_message(%Toxic.Warning{details: %{message: message}}) do
    IO.iodata_to_binary(message)
  end

  defp format_lexer_warning_message(_warning), do: "unknown warning"
end
