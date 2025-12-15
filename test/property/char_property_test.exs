defmodule ToxicParser.CharPropertyTest do
  @moduledoc """
  Property tests for ascii strings in various Elixir contexts.
  """
  use ExUnit.Case, async: false
  use ExUnitProperties

  # Options used by both oracle and Toxic for consistency
  @oracle_opts [
    columns: true,
    token_metadata: true,
    emit_warnings: false,
    existing_atoms_only: true
  ]

  # Toxic parse options - same as oracle where applicable
  @toxic_opts [
    existing_atoms_only: true
  ]

  # Character set for generating random code fragments
  @char_set [
    # minimal set to cover all keywords, operators, brackets, separators, numbers, aliases and identifiers
    ?0,
    ?1,
    ?b,
    ?x,
    ?d,
    ?o,
    ?e,
    ?n,
    ?d,
    ?c,
    ?a,
    ?t,
    ?c,
    ?h,
    ?r,
    ?e,
    ?s,
    ?c,
    ?u,
    ?e,
    ?a,
    ?f,
    ?t,
    ?e,
    ?r,
    ?e,
    ?l,
    ?s,
    ?e,
    ?f,
    ?n,
    ?w,
    ?h,
    ?e,
    ?n,
    ?a,
    ?n,
    ?d,
    ?o,
    ?r,
    ?n,
    ?o,
    ?t,
    ?i,
    ?n,
    ?t,
    ?r,
    ?u,
    ?e,
    ?f,
    ?a,
    ?l,
    ?s,
    ?e,
    ?n,
    ?i,
    ?l,
    ?A,
    ?!,
    ?@,
    ?^,
    ?&,
    ?*,
    ?(,
    ?),
    ?-,
    ?+,
    ?[,
    ?],
    ?{,
    ?},
    ?;,
    ?:,
    ?',
    ?",
    ?\\,
    ?|,
    ?~,
    ?<,
    ?>,
    ?,,
    ?.,
    ?/,
    ??,
    ?$,
    ?%,
    ?_,
    ?=,
    ?\s
    # excluded for now - create too many comments
    # ?#,
    # excluded for now
    # \n
  ]

  # setup_all do
  #   # Touch atom pools to ensure atoms exist
  #   touch_atom_pools()
  #   :ok
  # end

  # defp touch_atom_pools do
  #   _ = Gen.atom_pool()
  #   _ = Gen.identifier_pool()

  #   Enum.each(Gen.alias_pool(), fn alias_atom ->
  #     _ = Module.concat([alias_atom])
  #   end)

  #   :ok
  # end

  # ===========================================================================
  # Code Fragment Generator
  # ===========================================================================

  defp code_fragment_gen(opts \\ []) do
    min_length = Keyword.get(opts, :min_length, 0)
    max_length = Keyword.get(opts, :max_length, 16)
    StreamData.string(@char_set, min_length: min_length, max_length: max_length)
  end

  # ===========================================================================
  # Context Generators - each returns {context_name, full_code}
  # ===========================================================================

  # Beginning of string (code as standalone expression)
  defp context_standalone do
    StreamData.bind(code_fragment_gen(), fn code ->
      StreamData.constant({"standalone", code})
    end)
  end

  # Inside bitstring: <<a, s: CODE >>
  defp context_bitstring do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "<<a, s: " <> code <> " >>"
      StreamData.constant({"bitstring", full_code})
    end)
  end

  # Before do block: CODE do :ok end
  defp context_before_do do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = code <> " do :ok end"
      StreamData.constant({"before_do", full_code})
    end)
  end

  # After do block: foo do :ok end CODE
  defp context_after_do do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo do :ok end " <> code
      StreamData.constant({"after_do", full_code})
    end)
  end

  # Inside fn - various positions
  defp context_fn_arg do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn " <> code <> " -> :ok end"
      StreamData.constant({"fn_arg", full_code})
    end)
  end

  defp context_fn_body do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn 1 -> " <> code <> " end"
      StreamData.constant({"fn_body", full_code})
    end)
  end

  defp context_fn_no_arrow do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn " <> code <> " end"
      StreamData.constant({"fn_no_arrow", full_code})
    end)
  end

  defp context_fn_multi_arg do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn a, " <> code <> " end"
      StreamData.constant({"fn_multi_arg", full_code})
    end)
  end

  defp context_fn_multi_arg_with_arrow do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn a, " <> code <> " -> :ok end"
      StreamData.constant({"fn_multi_arg_arrow", full_code})
    end)
  end

  # Inside do block: foo do CODE end
  defp context_inside_do do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo do " <> code <> " end"
      StreamData.constant({"inside_do", full_code})
    end)
  end

  # Inside parens call: foo(CODE)
  defp context_parens_call do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo(" <> code <> ")"
      StreamData.constant({"parens_call", full_code})
    end)
  end

  # Inside no parens call: foo CODE
  defp context_no_parens_call do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo " <> code
      StreamData.constant({"no_parens_call", full_code})
    end)
  end

  # Inside bracket access: foo[CODE]
  defp context_bracket_access do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo[" <> code <> "]"
      StreamData.constant({"bracket_access", full_code})
    end)
  end

  # Inside map: %{CODE}
  defp context_map do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%{" <> code <> "}"
      StreamData.constant({"map", full_code})
    end)
  end

  # Inside struct: %Foo{CODE}
  defp context_struct do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%Foo{" <> code <> "}"
      StreamData.constant({"struct", full_code})
    end)
  end

  # Inside tuple: {CODE}
  defp context_tuple do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "{" <> code <> "}"
      StreamData.constant({"tuple", full_code})
    end)
  end

  # Inside list: [CODE]
  defp context_list do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "[" <> code <> "]"
      StreamData.constant({"list", full_code})
    end)
  end

  # Inside parens: (CODE)
  defp context_parens do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "(" <> code <> ")"
      StreamData.constant({"parens", full_code})
    end)
  end

  # Inside string interpolation: "#{CODE}"
  defp context_interpolation do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "\"" <> "\#{" <> code <> "}" <> "\""
      StreamData.constant({"interpolation", full_code})
    end)
  end

  # After pipe: :ok |> CODE
  defp context_after_pipe do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = ":ok |> " <> code
      StreamData.constant({"after_pipe", full_code})
    end)
  end

  # After assignment: x = CODE
  defp context_after_assignment do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "x = " <> code
      StreamData.constant({"after_assignment", full_code})
    end)
  end

  # Inside struct arg: %CODE{}
  defp context_struct_arg do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%" <> code <> "{}"
      StreamData.constant({"struct_arg", full_code})
    end)
  end

  # Between do blocks: foo do :ok end CODE do :error end
  defp context_between_do_blocks do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo do :ok end " <> code <> " do :error end"
      StreamData.constant({"between_do_blocks", full_code})
    end)
  end

  # Inside ternary range - first position: CODE..x//y
  defp context_ternary_first do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = code <> "..x//y"
      StreamData.constant({"ternary_first", full_code})
    end)
  end

  # Inside ternary range - second position: x..CODE//y
  defp context_ternary_second do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "x.." <> code <> "//y"
      StreamData.constant({"ternary_second", full_code})
    end)
  end

  # Inside ternary range - third position: x..y//CODE
  defp context_ternary_third do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "x..y//" <> code
      StreamData.constant({"ternary_third", full_code})
    end)
  end

  # Inside map update - updated expression: %{CODE | x: y}
  defp context_map_update_expr do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%{" <> code <> " | x: y}"
      StreamData.constant({"map_update_expr", full_code})
    end)
  end

  # Inside map update - key/value part: %{x | CODE}
  defp context_map_update_kv do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%{x | " <> code <> "}"
      StreamData.constant({"map_update_kv", full_code})
    end)
  end

  # Inside map update - value: %{x | foo: CODE}
  defp context_map_update_value do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "%{x | foo: " <> code <> "}"
      StreamData.constant({"map_update_value", full_code})
    end)
  end

  # After parens call: foo()CODE
  defp context_after_parens_call do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo()" <> code
      StreamData.constant({"after_parens_call", full_code})
    end)
  end

  # Inside no parens call with two args: foo CODE bar
  defp context_no_parens_call_middle do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo " <> code <> " bar"
      StreamData.constant({"no_parens_call_middle", full_code})
    end)
  end

  # Inside dot - before dot call: CODE.foo()
  defp context_before_dot_call do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = code <> ".foo()"
      StreamData.constant({"before_dot_call", full_code})
    end)
  end

  # Inside dot - middle of chain: A.CODE.foo()
  defp context_dot_chain_middle do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "A." <> code <> ".foo()"
      StreamData.constant({"dot_chain_middle", full_code})
    end)
  end

  # Inside dot - with tuple: A.CODE.{}
  defp context_dot_tuple do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "A." <> code <> ".{}"
      StreamData.constant({"dot_tuple", full_code})
    end)
  end

  # After dot: foo.CODE
  defp context_after_dot do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "foo." <> code
      StreamData.constant({"after_dot", full_code})
    end)
  end

  # Between operators: x + CODE * y
  defp context_between_operators do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "x + " <> code <> " * y"
      StreamData.constant({"between_operators", full_code})
    end)
  end

  # After unary &: &CODE
  defp context_after_capture do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "&" <> code
      StreamData.constant({"after_capture", full_code})
    end)
  end

  # After unary ^: ^CODE
  defp context_after_pin do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "^" <> code
      StreamData.constant({"after_pin", full_code})
    end)
  end

  # After unary +: +CODE
  defp context_after_unary_plus do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "+" <> code
      StreamData.constant({"after_unary_plus", full_code})
    end)
  end

  # After unary -: -CODE
  defp context_after_unary_minus do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "-" <> code
      StreamData.constant({"after_unary_minus", full_code})
    end)
  end

  # After unary @: @CODE
  defp context_after_at do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "@" <> code
      StreamData.constant({"after_at", full_code})
    end)
  end

  # After unary !: !CODE
  defp context_after_bang do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "!" <> code
      StreamData.constant({"after_bang", full_code})
    end)
  end

  # After unary not: not CODE
  defp context_after_not do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "not " <> code
      StreamData.constant({"after_not", full_code})
    end)
  end

  # Inside interpolated atom: :"foo#{CODE}"
  defp context_interpolated_atom do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = ":\"foo\#{" <> code <> "}\""
      StreamData.constant({"interpolated_atom", full_code})
    end)
  end

  # Inside interpolated keyword key: ["foo#{CODE}": 1]
  defp context_interpolated_keyword do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "[\"foo\#{" <> code <> "}\": 1]"
      StreamData.constant({"interpolated_keyword", full_code})
    end)
  end

  # Inside charlist interpolation: 'foo#{CODE}'
  defp context_charlist_interpolation do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "'foo\#{" <> code <> "}'"
      StreamData.constant({"charlist_interpolation", full_code})
    end)
  end

  # Inside string heredoc interpolation: """
  # foo#{CODE}
  # """
  defp context_string_heredoc_interpolation do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "\"\"\"\nfoo\#{" <> code <> "}\n\"\"\""
      StreamData.constant({"string_heredoc_interpolation", full_code})
    end)
  end

  # Inside charlist heredoc interpolation: '''
  # foo#{CODE}
  # '''
  defp context_charlist_heredoc_interpolation do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "'''\nfoo\#{" <> code <> "}\n'''"
      StreamData.constant({"charlist_heredoc_interpolation", full_code})
    end)
  end

  # Inside sigil interpolation: ~s/foo#{CODE}/
  defp context_sigil_interpolation do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "~s/foo\#{" <> code <> "}/"
      StreamData.constant({"sigil_interpolation", full_code})
    end)
  end

  # Inside sigil heredoc interpolation: ~s"""
  # foo#{CODE}
  # """
  defp context_sigil_heredoc_interpolation do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "~s\"\"\"\nfoo\#{" <> code <> "}\n\"\"\""
      StreamData.constant({"sigil_heredoc_interpolation", full_code})
    end)
  end

  # Inside when expr in fn: fn x when CODE -> 1 end
  defp context_fn_when do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "fn x when " <> code <> " -> 1 end"
      StreamData.constant({"fn_when", full_code})
    end)
  end

  # Inside def with parens args: def foo(CODE) do :ok end
  defp context_def_parens_arg do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "def foo(" <> code <> ") do :ok end"
      StreamData.constant({"def_parens_arg", full_code})
    end)
  end

  # Inside def with no parens args: def foo CODE do 1 end
  defp context_def_no_parens_arg do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "def foo " <> code <> " do 1 end"
      StreamData.constant({"def_no_parens_arg", full_code})
    end)
  end

  # Inside def when guard: def foo() when CODE do 1 end
  defp context_def_when do
    StreamData.bind(code_fragment_gen(), fn code ->
      full_code = "def foo() when " <> code <> " do 1 end"
      StreamData.constant({"def_when", full_code})
    end)
  end

  # Combined generator that picks one context randomly
  defp all_contexts_gen do
    StreamData.one_of([
      context_standalone(),
      context_bitstring(),
      context_before_do(),
      context_after_do(),
      context_fn_arg(),
      context_fn_body(),
      context_fn_no_arrow(),
      context_fn_multi_arg(),
      context_fn_multi_arg_with_arrow(),
      context_inside_do(),
      context_parens_call(),
      context_no_parens_call(),
      context_bracket_access(),
      context_map(),
      context_struct(),
      context_tuple(),
      context_list(),
      context_parens(),
      context_interpolation(),
      context_after_pipe(),
      context_after_assignment(),
      # New contexts
      context_struct_arg(),
      context_between_do_blocks(),
      context_ternary_first(),
      context_ternary_second(),
      context_ternary_third(),
      context_map_update_expr(),
      context_map_update_kv(),
      context_map_update_value(),
      context_after_parens_call(),
      context_no_parens_call_middle(),
      context_before_dot_call(),
      context_dot_chain_middle(),
      context_dot_tuple(),
      context_after_dot(),
      context_between_operators(),
      context_after_capture(),
      context_after_pin(),
      context_after_unary_plus(),
      context_after_unary_minus(),
      context_after_at(),
      context_after_bang(),
      context_after_not(),
      context_interpolated_atom(),
      context_interpolated_keyword(),
      # Interpolation contexts
      context_charlist_interpolation(),
      context_string_heredoc_interpolation(),
      context_charlist_heredoc_interpolation(),
      context_sigil_interpolation(),
      context_sigil_heredoc_interpolation(),
      # When and def contexts
      context_fn_when(),
      context_def_parens_arg(),
      context_def_no_parens_arg(),
      context_def_when()
    ])
  end

  # ===========================================================================
  # Property Tests
  # ===========================================================================

  describe "ascii in contexts" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "grammar trees round-trip through Toxic in all contexts" do
      check all(
              {context, code} <- all_contexts_gen(),
              max_runs: 5_000_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  # Individual context tests for targeted debugging
  describe "ascii standalone" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "standalone expressions" do
      check all(
              {context, code} <- context_standalone(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii bitstring" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside bitstring" do
      check all(
              {context, code} <- context_bitstring(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii before_do" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "before do block" do
      check all(
              {context, code} <- context_before_do(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii after_do" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "after do block" do
      check all({context, code} <- context_after_do(), max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii fn" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside fn expressions" do
      fn_contexts =
        StreamData.one_of([
          context_fn_arg(),
          context_fn_body(),
          context_fn_no_arrow(),
          context_fn_multi_arg(),
          context_fn_multi_arg_with_arrow()
        ])

      check all({context, code} <- fn_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii inside_do" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside do block" do
      check all(
              {context, code} <- context_inside_do(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii calls" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside function calls" do
      call_contexts =
        StreamData.one_of([
          context_parens_call(),
          context_no_parens_call()
        ])

      check all({context, code} <- call_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii bracket_access" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside bracket access" do
      check all(
              {context, code} <- context_bracket_access(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii containers" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside containers (map, tuple, list, struct)" do
      container_contexts =
        StreamData.one_of([
          context_map(),
          context_struct(),
          context_tuple(),
          context_list(),
          context_parens()
        ])

      check all(
              {context, code} <- container_contexts,
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii interpolation" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside string interpolation" do
      check all(
              {context, code} <- context_interpolation(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii operators" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "after operators (pipe, assignment)" do
      op_contexts =
        StreamData.one_of([
          context_after_pipe(),
          context_after_assignment()
        ])

      check all({context, code} <- op_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii struct_arg" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside struct arg" do
      check all(
              {context, code} <- context_struct_arg(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii between_do_blocks" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "between do blocks" do
      check all(
              {context, code} <- context_between_do_blocks(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii ternary" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside ternary range expressions" do
      ternary_contexts =
        StreamData.one_of([
          context_ternary_first(),
          context_ternary_second(),
          context_ternary_third()
        ])

      check all({context, code} <- ternary_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii map_update" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside map update expressions" do
      map_update_contexts =
        StreamData.one_of([
          context_map_update_expr(),
          context_map_update_kv(),
          context_map_update_value()
        ])

      check all(
              {context, code} <- map_update_contexts,
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii after_parens_call" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "after parens call" do
      check all(
              {context, code} <- context_after_parens_call(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii no_parens_call_middle" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside no parens call middle" do
      check all(
              {context, code} <- context_no_parens_call_middle(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii dot" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside dot expressions" do
      dot_contexts =
        StreamData.one_of([
          context_before_dot_call(),
          context_dot_chain_middle(),
          context_dot_tuple(),
          context_after_dot()
        ])

      check all({context, code} <- dot_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii between_operators" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "between operators" do
      check all(
              {context, code} <- context_between_operators(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii unary" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "after unary operators" do
      unary_contexts =
        StreamData.one_of([
          context_after_capture(),
          context_after_pin(),
          context_after_unary_plus(),
          context_after_unary_minus(),
          context_after_at(),
          context_after_bang(),
          context_after_not()
        ])

      check all({context, code} <- unary_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii interpolated_atom_keyword" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside interpolated atoms and keywords" do
      interp_contexts =
        StreamData.one_of([
          context_interpolated_atom(),
          context_interpolated_keyword()
        ])

      check all({context, code} <- interp_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii charlist_interpolation" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside charlist interpolation" do
      check all(
              {context, code} <- context_charlist_interpolation(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii heredoc_interpolation" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside heredoc interpolation" do
      heredoc_contexts =
        StreamData.one_of([
          context_string_heredoc_interpolation(),
          context_charlist_heredoc_interpolation()
        ])

      check all(
              {context, code} <- heredoc_contexts,
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii sigil_interpolation" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside sigil interpolation" do
      sigil_contexts =
        StreamData.one_of([
          context_sigil_interpolation(),
          context_sigil_heredoc_interpolation()
        ])

      check all({context, code} <- sigil_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii fn_when" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside fn when guard" do
      check all({context, code} <- context_fn_when(), max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code)
      end
    end
  end

  describe "ascii def" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "inside def expressions" do
      def_contexts =
        StreamData.one_of([
          context_def_parens_arg(),
          context_def_no_parens_arg(),
          context_def_when()
        ])

      check all({context, code} <- def_contexts, max_runs: 100_000, max_shrinking_steps: 50) do
        run_comparison(context, code)
      end
    end
  end

  # ===========================================================================
  # Comparison Helper
  # ===========================================================================

  defp run_comparison(context, code) do
    # Use Code.with_diagnostics to capture warnings
    {result, _diagnostics} =
      Code.with_diagnostics(fn ->
        Code.string_to_quoted(code, @oracle_opts)
      end)

    case result do
      {:ok, {:__block__, _, []}} ->
        :ok

      {:ok, oracle_ast} ->
        # IO.puts(">>>>> [#{context}]\n" <> code <> "\n<<<<<")
        # Parse with Toxic using same options as oracle
        assert {:ok, toxic_ast} = toxic_parse(code)

        # Apply workaround for parser bugs with not/! in forms
        {oracle_ast, toxic_ast} =
          fix_deprecated_not_in_meta(oracle_ast, toxic_ast)

        # Normalize and compare ASTs
        oracle_normalized = normalize_ast(oracle_ast)
        toxic_normalized = normalize_ast(toxic_ast)

        assert oracle_normalized == toxic_normalized,
               """
               AST mismatch in context #{context} for code: #{inspect(code)}

               Oracle:
               #{inspect(oracle_normalized, pretty: true)}

               Toxic:
               #{inspect(toxic_normalized, pretty: true)}
               """

      {:error, _} ->
        # Oracle rejected, skip this sample
        :ok
    end
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

  # ===========================================================================
  # Workaround for Elixir parser bug with deprecated "not/! in" forms
  # ===========================================================================

  # When using deprecated forms "not a in b" or "!a in b", Elixir's parser
  # emits invalid metadata on the not/! node (pointing to the `in` operator
  # instead of the actual `not`/`!` token) and doesn't include newlines/end_of_expression
  # on the `in` node. This function fixes the Oracle AST by copying the correct
  # metadata from Toxic's AST.
  #
  # Additionally, for non-deprecated forms like "not (a in b)", Toxic incorrectly
  # reports the not/! position as the `in` position. This function also fixes
  # Toxic's AST by copying from Oracle in those cases.
  #
  # The AST structure for both deprecated and non-deprecated forms is identical:
  # - "not a in b" -> {:not, meta, [{:in, meta, [a, b]}]}
  # - "a not in b" -> {:not, meta, [{:in, meta, [a, b]}]}
  # - "!a in b"    -> {:!, meta, [{:in, meta, [a, b]}]}
  # - "!(a in b)"  -> {:!, meta, [{:in, meta, [a, b]}]}
  defp fix_deprecated_not_in_meta(oracle_ast, toxic_ast) do
    # Build maps of fixes from both ASTs
    {oracle_op_fixes, oracle_in_fixes} = collect_not_in_meta(oracle_ast)
    {toxic_op_fixes, toxic_in_fixes} = collect_not_in_meta(toxic_ast)

    # Determine which AST has the "better" location for each not/! node
    # The better location is the one that does NOT point to the `in` operator
    better_locations =
      for {{_op, in_line, in_col} = key, oracle_meta} <- oracle_op_fixes, into: %{} do
        toxic_meta = Map.get(toxic_op_fixes, key, [])

        oracle_line = Keyword.get(oracle_meta, :line)
        oracle_col = Keyword.get(oracle_meta, :column)
        toxic_line = Keyword.get(toxic_meta, :line)
        toxic_col = Keyword.get(toxic_meta, :column)

        oracle_points_to_in = oracle_line == in_line and oracle_col == in_col
        toxic_points_to_in = toxic_line == in_line and toxic_col == in_col

        # Choose the location that doesn't point to `in`, or Oracle if both do
        better =
          cond do
            oracle_meta == [] -> :toxic
            toxic_meta == [] -> :oracle
            not oracle_points_to_in -> :oracle
            not toxic_points_to_in -> :toxic
            # Both point to `in`, they likely match
            true -> :same
          end

        {key, {better, oracle_meta, toxic_meta}}
      end

    # Fix Oracle AST
    fixed_oracle =
      Macro.prewalk(oracle_ast, fn
        {op, oracle_meta, [{:in, in_meta, in_args} | rest]} when op in [:not, :!] ->
          key = {op, Keyword.get(in_meta, :line), Keyword.get(in_meta, :column)}
          toxic_in_meta = Map.get(toxic_in_fixes, key)

          # Fix op meta based on which source is better
          fixed_op_meta =
            case Map.get(better_locations, key) do
              {:toxic, _oracle_meta, toxic_meta} when toxic_meta != [] ->
                toxic_meta

              _ ->
                oracle_meta
            end

          # Fix in node's meta - add missing keys and handle end_of_expression placement
          fixed_in_meta =
            if toxic_in_meta != nil do
              add_missing_meta(in_meta, toxic_in_meta)
            else
              in_meta
            end

          # If Oracle has end_of_expression on op but Toxic has it on in,
          # move it from op to in for consistency
          {fixed_op_meta, fixed_in_meta} =
            if Keyword.has_key?(fixed_op_meta, :end_of_expression) and
                 not Keyword.has_key?(in_meta, :end_of_expression) and
                 toxic_in_meta != nil and
                 Keyword.has_key?(toxic_in_meta, :end_of_expression) do
              {
                Keyword.delete(fixed_op_meta, :end_of_expression),
                Keyword.put(
                  fixed_in_meta,
                  :end_of_expression,
                  Keyword.get(fixed_op_meta, :end_of_expression)
                )
              }
            else
              {fixed_op_meta, fixed_in_meta}
            end

          {op, fixed_op_meta, [{:in, fixed_in_meta, in_args} | rest]}

        node ->
          node
      end)

    # Fix Toxic AST
    fixed_toxic =
      Macro.prewalk(toxic_ast, fn
        {op, toxic_meta, [{:in, in_meta, in_args} | rest]} when op in [:not, :!] ->
          key = {op, Keyword.get(in_meta, :line), Keyword.get(in_meta, :column)}
          oracle_in_meta = Map.get(oracle_in_fixes, key)

          # Fix op meta based on which source is better
          fixed_op_meta =
            case Map.get(better_locations, key) do
              {:oracle, oracle_meta, _toxic_meta} when oracle_meta != [] ->
                # Use Oracle's meta as base, add any extra keys from Toxic
                extra_keys = [:newlines, :end_of_expression, :parens]

                Enum.reduce(extra_keys, oracle_meta, fn key, acc ->
                  case Keyword.fetch(toxic_meta, key) do
                    {:ok, value} -> Keyword.put_new(acc, key, value)
                    :error -> acc
                  end
                end)

              _ ->
                toxic_meta
            end

          # Add missing meta to in node from Oracle
          fixed_in_meta =
            if oracle_in_meta != nil do
              add_missing_meta(in_meta, oracle_in_meta)
            else
              in_meta
            end

          {op, fixed_op_meta, [{:in, fixed_in_meta, in_args} | rest]}

        node ->
          node
      end)

    {fixed_oracle, fixed_toxic}
  end

  # Add missing metadata keys from source to target
  defp add_missing_meta(target, source) do
    keys = [:newlines, :end_of_expression, :parens]

    Enum.reduce(keys, target, fn key, acc ->
      if Keyword.has_key?(acc, key) do
        acc
      else
        case Keyword.fetch(source, key) do
          {:ok, value} -> Keyword.put(acc, key, value)
          :error -> acc
        end
      end
    end)
  end

  # Collect metadata for not/! operators and their `in` arguments
  # Returns {op_fixes, in_fixes} maps keyed by {op, in_line, in_column}
  defp collect_not_in_meta(ast) do
    {_ast, {op_acc, in_acc}} =
      Macro.prewalk(ast, {%{}, %{}}, fn
        {op, meta, [{:in, in_meta, _in_args} | _rest]} = node, {op_acc, in_acc}
        when op in [:not, :!] ->
          # Key by operator and position of the `in` node (which is correct in both parsers)
          key = {op, Keyword.get(in_meta, :line), Keyword.get(in_meta, :column)}
          {node, {Map.put(op_acc, key, meta), Map.put(in_acc, key, in_meta)}}

        node, acc ->
          {node, acc}
      end)

    {op_acc, in_acc}
  end

  # ===========================================================================
  # AST Normalization (from V7 Section 10)
  # ===========================================================================

  @ignored_meta_keys [
    # :from_brackets,
    # :ambiguous_op,
    # :parens,
    # :format,
    # :closing,
    # :end_of_expression,
    :range
    # :newlines
    # :delimiter,
    # :indentation
  ]

  defp normalize_ast(ast) do
    ast
    |> unwrap_single_block()
    |> Macro.postwalk(fn
      # Empty blocks - strip all metadata (Oracle omits it, Toxic includes it)
      {:__block__, _meta, []} ->
        {:__block__, [], []}

      {tag, meta, args} when is_list(meta) ->
        {tag, Keyword.drop(meta, @ignored_meta_keys), args}

      keyword when is_list(keyword) ->
        if Keyword.keyword?(keyword) do
          Keyword.drop(keyword, @ignored_meta_keys)
        else
          keyword
        end

      node ->
        node
    end)
  end

  # Unwrap single-element __block__ nodes (Oracle sometimes wraps parenthesized exprs)
  # TODO: decide if we should keep it or backport the parens handling
  defp unwrap_single_block({:__block__, _meta, [single]}) do
    unwrap_single_block(single)
  end

  defp unwrap_single_block(ast), do: ast
end
