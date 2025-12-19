defmodule ToxicParser.TokenPropertyTest do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias ToxicParser.Generator

  @tag :property
  property "generated token lists round-trip through Toxic.to_string/1 and lex" do
    check all(tokens <- Generator.tokens_gen(max_forms: 3, depth: 2), max_runs: 10_000) do
      code = Toxic.to_string(tokens)

      lexed =
        Toxic.new(code, 1, 1, error_mode: :strict, insert_structural_closers: false)
        |> Toxic.to_stream()
        |> Enum.to_list()

      assert normalize_tokens(lexed) == normalize_tokens(tokens)
    end
  end

  defp normalize_tokens(tokens), do: Enum.map(tokens, &normalize_token/1)

  defp normalize_token({:identifier, _meta, atom}), do: {:identifier, atom}
  defp normalize_token({:bracket_identifier, _meta, atom}), do: {:bracket_identifier, atom}
  defp normalize_token({:alias, _meta, atom}), do: {:alias, atom}
  defp normalize_token({:at_op, _meta, :@}), do: :at
  defp normalize_token({:capture_int, _meta, :&}), do: :capture_int
  defp normalize_token({:atom, _meta, atom}), do: {:atom, atom}
  defp normalize_token({:int, _meta, chars}), do: {:int, chars}
  defp normalize_token({:flt, _meta, chars}), do: {:flt, chars}
  defp normalize_token({:char, _meta, cp}), do: {:char, cp}
  defp normalize_token({:eol, meta}), do: {:eol, elem(meta, 2)}
  defp normalize_token({:range_op, meta, op}), do: {:range_op, op, elem(meta, 2) || 0}
  defp normalize_token({:ellipsis_op, meta, op}), do: {:ellipsis_op, op, elem(meta, 2) || 0}
  defp normalize_token({:"[", _meta}), do: :lbracket
  defp normalize_token({:"]", _meta}), do: :rbracket
  defp normalize_token({:"{", _meta}), do: :lcurly
  defp normalize_token({:"}", meta}), do: {:rcurly, elem(meta, 2) || 0}
  defp normalize_token({:";", _meta}), do: {:";"}
  defp normalize_token({:., _meta}), do: :dot
  defp normalize_token({:"(", _meta}), do: {:"("}
  defp normalize_token({:")", _meta}), do: {:")"}
  defp normalize_token({true, _meta}), do: true
  defp normalize_token({false, _meta}), do: false
  defp normalize_token({nil, _meta}), do: nil
  defp normalize_token(other), do: other
end
