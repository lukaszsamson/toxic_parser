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

  @tag :skip
  test "empty parentheses expression" do
    assert_warning_conforms("()")
  end

  @tag :skip
  test "empty stab clause" do
    assert_warning_conforms("fn x -> end")
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
end
