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

  @tag :property
  property "generated token lists round-trip with random feature flags" do
    check all(
            flags <- Generator.flags_gen(),
            tokens <- Generator.tokens_gen(flags: flags, max_forms: 3, depth: 2),
            max_runs: 5_000
          ) do
      code = Toxic.to_string(tokens)

      lexed =
        Toxic.new(code, 1, 1, error_mode: :strict, insert_structural_closers: false)
        |> Toxic.to_stream()
        |> Enum.to_list()

      assert normalize_tokens(lexed) == normalize_tokens(tokens)
    end
  end

  @tag :property
  property "generated token lists round-trip with all optional features disabled" do
    flags =
      Generator.default_flags()
      |> Map.merge(%{
        enable_lists: false,
        enable_maps: false,
        enable_do_blocks: false,
        enable_tuples: false,
        enable_bitstrings: false,
        enable_fn: false,
        enable_parens_stab: false,
        enable_binary_op: false,
        enable_unary_op: false,
        enable_kw: false,
        enable_parens_calls: false,
        enable_no_parens_calls: false
      })

    check all(
            tokens <- Generator.tokens_gen(flags: flags, max_forms: 3, depth: 2),
            max_runs: 2_000
          ) do
      code = Toxic.to_string(tokens)

      lexed =
        Toxic.new(code, 1, 1, error_mode: :strict, insert_structural_closers: false)
        |> Toxic.to_stream()
        |> Enum.to_list()

      assert normalize_tokens(lexed) == normalize_tokens(tokens)
    end
  end

  defp normalize_tokens(tokens) do
    tokens
    |> Enum.map(&normalize_token/1)
    |> expand_range_leading_eols()
  end

  defp expand_range_leading_eols(tokens) do
    Enum.flat_map(tokens, fn
      {:range_op, op, n} when is_integer(n) and n > 0 ->
        [{:eol, n}, {:range_op, op, 0}]

      {:stab_op, op, n} when is_integer(n) and n > 0 ->
        [{:eol, n}, {:stab_op, op, 0}]

      other ->
        [other]
    end)
  end

  defp normalize_token({:do_identifier, _meta, atom}), do: {:identifier, atom}
  defp normalize_token({:identifier, _meta, :@}), do: :at
  defp normalize_token({:identifier, _meta, :...}), do: {:ellipsis_op, :..., 0}
  defp normalize_token({:identifier, _meta, :..}), do: {:range_op, :.., 0}
  defp normalize_token({:identifier, _meta, :\\}), do: {:in_match_op, :\\, 0}
  defp normalize_token({:identifier, _meta, :<-}), do: {:in_match_op, :<-, 0}
  defp normalize_token({:identifier, _meta, :+}), do: {:dual_op, :+}
  defp normalize_token({:identifier, _meta, :-}), do: {:dual_op, :-}
  defp normalize_token({:identifier, _meta, :!}), do: {:unary_op, :!}
  defp normalize_token({:identifier, _meta, :^}), do: {:unary_op, :^}
  defp normalize_token({:identifier, _meta, :"::"}), do: {:type_op, :"::", 0}
  defp normalize_token({:identifier, _meta, :+++}), do: {:concat_op, :+++, 0}
  defp normalize_token({:identifier, _meta, :or}), do: {:or_op, :or, 0}
  defp normalize_token({:identifier, _meta, :||}), do: {:or_op, :||, 0}
  defp normalize_token({:identifier, _meta, :|}), do: {:pipe_op, :|, 0}
  defp normalize_token({:identifier, _meta, :&&}), do: {:and_op, :&&, 0}
  defp normalize_token({:identifier, _meta, :**}), do: {:power_op, :**, 0}
  defp normalize_token({:identifier, _meta, :=}), do: {:match_op, :=, 0}
  defp normalize_token({:identifier, _meta, :/}), do: {:mult_op, :/, 0}
  defp normalize_token({:identifier, _meta, :*}), do: {:mult_op, :*, 0}
  defp normalize_token({:identifier, _meta, :<}), do: {:rel_op, :<, 0}
  defp normalize_token({:identifier, _meta, :>}), do: {:rel_op, :>, 0}
  defp normalize_token({:identifier, _meta, :<=}), do: {:rel_op, :<=, 0}
  defp normalize_token({:identifier, _meta, :>=}), do: {:rel_op, :>=, 0}
  defp normalize_token({:identifier, _meta, :==}), do: {:comp_op, :==, 0}
  defp normalize_token({:identifier, _meta, :!=}), do: {:comp_op, :!=, 0}
  defp normalize_token({:identifier, _meta, :===}), do: {:comp_op, :===, 0}
  defp normalize_token({:identifier, _meta, :!==}), do: {:comp_op, :!==, 0}
  defp normalize_token({:identifier, _meta, :=~}), do: {:comp_op, :=~, 0}
  defp normalize_token({:identifier, _meta, atom}), do: {:identifier, atom}
  defp normalize_token({:paren_identifier, _meta, atom}), do: {:paren_identifier, atom}
  defp normalize_token({:op_identifier, _meta, atom}), do: {:identifier, atom}
  defp normalize_token({:bracket_identifier, _meta, atom}), do: {:bracket_identifier, atom}
  defp normalize_token({:block_identifier, _meta, atom}), do: {:block_identifier, atom}
  defp normalize_token({:alias, _meta, atom}), do: {:alias, atom}
  defp normalize_token({:at_op, _meta, :@}), do: :at
  defp normalize_token({:capture_int, _meta, :&}), do: :capture_int
  defp normalize_token({:capture_op, _meta, :&}), do: :capture_op
  defp normalize_token({:kw_identifier, _meta, atom}), do: {:kw_identifier, atom}
  defp normalize_token({:atom, _meta, atom}), do: {:atom, atom}
  defp normalize_token({:int, _meta, chars}), do: {:int, chars}
  defp normalize_token({:flt, _meta, chars}), do: {:flt, chars}
  defp normalize_token({:char, _meta, cp}), do: {:char, cp}
  defp normalize_token({:eol, meta}), do: {:eol, elem(meta, 2)}
  defp normalize_token({:range_op, meta, op}), do: {:range_op, op, elem(meta, 2) || 0}
  defp normalize_token({:ellipsis_op, meta, op}), do: {:ellipsis_op, op, elem(meta, 2) || 0}
  defp normalize_token({:%{}, _meta}), do: :map_op
  defp normalize_token({:%, _meta}), do: :percent
  defp normalize_token({:assoc_op, meta, op}), do: {:assoc_op, op, elem(meta, 2) || 0}
  defp normalize_token({:pipe_op, meta, op}), do: {:pipe_op, op, elem(meta, 2) || 0}
  defp normalize_token({:unary_op, _meta, op}), do: {:unary_op, op}
  defp normalize_token({:dual_op, _meta, op}), do: {:dual_op, op}
  defp normalize_token({:ternary_op, meta, op}), do: {:ternary_op, op, elem(meta, 2) || 0}
  defp normalize_token({:match_op, meta, op}), do: {:match_op, op, elem(meta, 2) || 0}
  defp normalize_token({:mult_op, meta, op}), do: {:mult_op, op, elem(meta, 2) || 0}
  defp normalize_token({:power_op, meta, op}), do: {:power_op, op, elem(meta, 2) || 0}
  defp normalize_token({:concat_op, meta, op}), do: {:concat_op, op, elem(meta, 2) || 0}
  defp normalize_token({:xor_op, meta, op}), do: {:xor_op, op, elem(meta, 2) || 0}
  defp normalize_token({:and_op, meta, op}), do: {:and_op, op, elem(meta, 2) || 0}
  defp normalize_token({:or_op, meta, op}), do: {:or_op, op, elem(meta, 2) || 0}
  defp normalize_token({:in_op, meta, op}), do: {:in_op, op, elem(meta, 2) || 0}
  defp normalize_token({:in_match_op, meta, op}), do: {:in_match_op, op, elem(meta, 2) || 0}
  defp normalize_token({:type_op, meta, op}), do: {:type_op, op, elem(meta, 2) || 0}
  defp normalize_token({:when_op, meta, _op}), do: {:when_op, :when, elem(meta, 2) || 0}
  defp normalize_token({:stab_op, meta, op}), do: {:stab_op, op, elem(meta, 2) || 0}
  defp normalize_token({:comp_op, meta, op}), do: {:comp_op, op, elem(meta, 2) || 0}
  defp normalize_token({:rel_op, meta, op}), do: {:rel_op, op, elem(meta, 2) || 0}
  defp normalize_token({:arrow_op, meta, op}), do: {:arrow_op, op, elem(meta, 2) || 0}
  defp normalize_token({:"[", _meta}), do: :lbracket
  defp normalize_token({:"]", meta}), do: {:rbracket, elem(meta, 2) || 0}
  defp normalize_token({:"<<", _meta}), do: :open_bit
  defp normalize_token({:">>", meta}), do: {:close_bit, elem(meta, 2) || 0}
  defp normalize_token({:"{", _meta}), do: :lcurly
  defp normalize_token({:"}", meta}), do: {:rcurly, elem(meta, 2) || 0}
  defp normalize_token({:",", _meta}), do: :comma
  defp normalize_token({:";", _meta}), do: {:";"}
  defp normalize_token({:., _meta}), do: :dot
  defp normalize_token({:dot_call_op, _meta, :.}), do: :dot_call_op
  defp normalize_token({:"(", _meta}), do: {:"("}
  defp normalize_token({:")", _meta}), do: {:")"}
  defp normalize_token({:fn, _meta}), do: :fn
  defp normalize_token({:end, _meta}), do: :end
  defp normalize_token({true, _meta}), do: true
  defp normalize_token({false, _meta}), do: false
  defp normalize_token({nil, _meta}), do: nil
  defp normalize_token(other), do: other
end
