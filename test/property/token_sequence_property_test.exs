defmodule ToxicParser.TokenSequencePropertyTest do
  @moduledoc """
  Property tests that generate programs from a closed list of identifiers,
  keywords, operators, and container tokens glued by separators.
  """
  use ExUnit.Case, async: true
  use ExUnitProperties
  import ExUnit.CaptureIO

  # Options used by both oracle and Toxic for consistency
  @oracle_opts [
    columns: true,
    token_metadata: true,
    emit_warnings: false,
    existing_atoms_only: false
  ]

  # ===========================================================================
  # Token Lists
  # ===========================================================================

  # Identifiers
  @identifiers ~w(a b c x y z foo bar baz qux name value key item)

  # Elixir reserved keywords
  @keywords ~w(
    true false nil
    when and or not in
    fn do end catch rescue after else
    case cond for if unless try receive with
    defmodule def defp defmacro defmacrop defguard defguardp defdelegate defstruct defexception defimpl defprotocol defoverridable
    import require alias use
    quote unquote unquote_splicing
    super __MODULE__ __DIR__ __ENV__ __CALLER__ __STACKTRACE__
  )

  # Operators (binary and unary)
  @operators ~w(
    + - * / ++ -- ** .. ... <> |> <<< >>> <<~ ~>> <~ ~> <~>
    < > <= >= == != === !== =~
    && || !
    ^^^
    & | ^ ~~~
    = <- -> => ::
    \\ //
    @
    .
  )

  # Container openers/closers
  @containers ~w( ( \) [ ] { } << >> %{ % \#{  )

  # Special tokens
  @special ~w( : ; , \n | _ ? ! )

  # Atoms and strings
  @atoms [":ok", ":error", ":foo", ":bar", ":_"]

  # Numbers
  @numbers ~w(0 1 2 42 100 0x10 0b101 0o17 1.0 1.5e10)

  # Strings
  @strings ["\"hello\"", "\"\"", "'c'"]

  # All tokens combined
  @all_tokens @identifiers ++
                @keywords ++
                @operators ++
                @containers ++
                @special ++
                @atoms ++
                @numbers ++
                @strings

  # Separators for gluing tokens
  @separators ["", " ", "\n", "  ", " \n", "\n ", " \n ", ";", ",", ", ", ",\n", "/n;"]

  # ===========================================================================
  # Generators
  # ===========================================================================

  defp token_gen do
    StreamData.member_of(@all_tokens)
  end

  defp separator_gen do
    StreamData.member_of(@separators)
  end

  defp token_sequence_gen(opts \\ []) do
    min_length = Keyword.get(opts, :min_length, 1)
    max_length = Keyword.get(opts, :max_length, 10)

    StreamData.bind(
      StreamData.integer(min_length..max_length),
      fn length ->
        StreamData.bind(
          StreamData.list_of(token_gen(), length: length),
          fn tokens ->
            StreamData.bind(
              StreamData.list_of(separator_gen(), length: max(0, length - 1)),
              fn separators ->
                code = interleave(tokens, separators)
                StreamData.constant(code)
              end
            )
          end
        )
      end
    )
  end

  defp interleave([token], []), do: token
  defp interleave([token | rest_tokens], [sep | rest_seps]) do
    token <> sep <> interleave(rest_tokens, rest_seps)
  end
  defp interleave([], []), do: ""

  # ===========================================================================
  # Context Generators - each returns {context_name, full_code}
  # ===========================================================================

  # Standalone expression
  defp context_standalone do
    StreamData.bind(token_sequence_gen(), fn code ->
      StreamData.constant({"standalone", code})
    end)
  end

  # Inside bitstring: <<CODE>>
  defp context_bitstring do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "<<" <> code <> ">>"
      StreamData.constant({"bitstring", full_code})
    end)
  end

  # Before do block: CODE do :ok end
  defp context_before_do do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = code <> " do :ok end"
      StreamData.constant({"before_do", full_code})
    end)
  end

  # After do block: foo do :ok end CODE
  defp context_after_do do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "foo do :ok end " <> code
      StreamData.constant({"after_do", full_code})
    end)
  end

  # Inside fn arg: fn CODE -> :ok end
  defp context_fn_arg do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "fn " <> code <> " -> :ok end"
      StreamData.constant({"fn_arg", full_code})
    end)
  end

  # Inside fn body: fn x -> CODE end
  defp context_fn_body do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "fn x -> " <> code <> " end"
      StreamData.constant({"fn_body", full_code})
    end)
  end

  # Inside do block: foo do CODE end
  defp context_inside_do do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "foo do " <> code <> " end"
      StreamData.constant({"inside_do", full_code})
    end)
  end

  # Inside parens call: foo(CODE)
  defp context_parens_call do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "foo(" <> code <> ")"
      StreamData.constant({"parens_call", full_code})
    end)
  end

  # Inside no parens call: foo CODE
  defp context_no_parens_call do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "foo " <> code
      StreamData.constant({"no_parens_call", full_code})
    end)
  end

  # Inside bracket access: foo[CODE]
  defp context_bracket_access do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "foo[" <> code <> "]"
      StreamData.constant({"bracket_access", full_code})
    end)
  end

  # Inside map: %{CODE}
  defp context_map do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "%{" <> code <> "}"
      StreamData.constant({"map", full_code})
    end)
  end

  # Inside struct: %Foo{CODE}
  defp context_struct do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "%Foo{" <> code <> "}"
      StreamData.constant({"struct", full_code})
    end)
  end

  # Inside tuple: {CODE}
  defp context_tuple do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "{" <> code <> "}"
      StreamData.constant({"tuple", full_code})
    end)
  end

  # Inside list: [CODE]
  defp context_list do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "[" <> code <> "]"
      StreamData.constant({"list", full_code})
    end)
  end

  # Inside parens: (CODE)
  defp context_parens do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "(" <> code <> ")"
      StreamData.constant({"parens", full_code})
    end)
  end

  # Inside string interpolation: "#{CODE}"
  defp context_interpolation do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "\"" <> "\#{" <> code <> "}" <> "\""
      StreamData.constant({"interpolation", full_code})
    end)
  end

  # After pipe: :ok |> CODE
  defp context_after_pipe do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = ":ok |> " <> code
      StreamData.constant({"after_pipe", full_code})
    end)
  end

  # After assignment: x = CODE
  defp context_after_assignment do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "x = " <> code
      StreamData.constant({"after_assignment", full_code})
    end)
  end

  # Inside struct arg: %CODE{}
  defp context_struct_arg do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "%" <> code <> "{}"
      StreamData.constant({"struct_arg", full_code})
    end)
  end

  # Between do blocks: foo do :ok end CODE do :error end
  defp context_between_do_blocks do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "foo do :ok end " <> code <> " do :error end"
      StreamData.constant({"between_do_blocks", full_code})
    end)
  end

  # Inside ternary range - first position: CODE..x//y
  defp context_ternary_first do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = code <> "..x//y"
      StreamData.constant({"ternary_first", full_code})
    end)
  end

  # Inside ternary range - second position: x..CODE//y
  defp context_ternary_second do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "x.." <> code <> "//y"
      StreamData.constant({"ternary_second", full_code})
    end)
  end

  # Inside ternary range - third position: x..y//CODE
  defp context_ternary_third do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "x..y//" <> code
      StreamData.constant({"ternary_third", full_code})
    end)
  end

  # Inside map update: %{x | CODE}
  defp context_map_update do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "%{x | " <> code <> "}"
      StreamData.constant({"map_update", full_code})
    end)
  end

  # After parens call: foo()CODE
  defp context_after_parens_call do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "foo()" <> code
      StreamData.constant({"after_parens_call", full_code})
    end)
  end

  # After dot: foo.CODE
  defp context_after_dot do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "foo." <> code
      StreamData.constant({"after_dot", full_code})
    end)
  end

  # Parenthesized stab: (x -> CODE)
  defp context_paren_stab do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "(x -> " <> code <> ")"
      StreamData.constant({"paren_stab", full_code})
    end)
  end

  # Between operators: x + CODE * y
  defp context_between_operators do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "x + " <> code <> " * y"
      StreamData.constant({"between_operators", full_code})
    end)
  end

  # After unary &: &CODE
  defp context_after_capture do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "&" <> code
      StreamData.constant({"after_capture", full_code})
    end)
  end

  # After unary ^: ^CODE
  defp context_after_pin do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "^" <> code
      StreamData.constant({"after_pin", full_code})
    end)
  end

  # After unary @: @CODE
  defp context_after_at do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "@" <> code
      StreamData.constant({"after_at", full_code})
    end)
  end

  # After unary !: !CODE
  defp context_after_bang do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "!" <> code
      StreamData.constant({"after_bang", full_code})
    end)
  end

  # After unary not: not CODE
  defp context_after_not do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "not " <> code
      StreamData.constant({"after_not", full_code})
    end)
  end

  # Inside fn when guard: fn x when CODE -> 1 end
  defp context_fn_when do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "fn x when " <> code <> " -> 1 end"
      StreamData.constant({"fn_when", full_code})
    end)
  end

  # Inside def with parens args: def foo(CODE) do :ok end
  defp context_def_parens_arg do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "def foo(" <> code <> ") do :ok end"
      StreamData.constant({"def_parens_arg", full_code})
    end)
  end

  # Inside keyword list value: [a: CODE]
  defp context_keyword_list_value do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "[a: " <> code <> "]"
      StreamData.constant({"keyword_list_value", full_code})
    end)
  end

  # Inside map kv value: %{a: CODE}
  defp context_map_kv_value do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "%{a: " <> code <> "}"
      StreamData.constant({"map_kv_value", full_code})
    end)
  end

  # Inside map rocket key: %{CODE => 1}
  defp context_map_rocket_key do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "%{" <> code <> " => 1}"
      StreamData.constant({"map_rocket_key", full_code})
    end)
  end

  # Case clause pattern: case x do CODE -> :ok end
  defp context_case_clause_lhs do
    StreamData.bind(token_sequence_gen(min_length: 1), fn code ->
      full_code = "case x do " <> code <> " -> :ok end"
      StreamData.constant({"case_clause_lhs", full_code})
    end)
  end

  # Case clause body: case x do 1 -> CODE end
  defp context_case_clause_rhs do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "case x do 1 -> " <> code <> " end"
      StreamData.constant({"case_clause_rhs", full_code})
    end)
  end

  # If condition: if CODE do :ok end
  defp context_if_condition do
    StreamData.bind(token_sequence_gen(min_length: 1), fn code ->
      full_code = "if " <> code <> " do :ok end"
      StreamData.constant({"if_condition", full_code})
    end)
  end

  # If else body: if true do :ok else CODE end
  defp context_if_else_body do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "if true do :ok else " <> code <> " end"
      StreamData.constant({"if_else_body", full_code})
    end)
  end

  # With generator RHS: with x <- CODE do x end
  defp context_with_generator_rhs do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "with x <- " <> code <> " do x end"
      StreamData.constant({"with_generator_rhs", full_code})
    end)
  end

  # For generator RHS: for x <- CODE, do: x
  defp context_for_generator_rhs do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "for x <- " <> code <> ", do: x"
      StreamData.constant({"for_generator_rhs", full_code})
    end)
  end

  # Try body: try do CODE after :ok end
  defp context_try_body do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "try do " <> code <> " after :ok end"
      StreamData.constant({"try_body", full_code})
    end)
  end

  # Receive clause pattern: receive do CODE -> :ok after 0 -> :timeout end
  defp context_receive_clause_lhs do
    StreamData.bind(token_sequence_gen(min_length: 1), fn code ->
      full_code = "receive do " <> code <> " -> :ok after 0 -> :timeout end"
      StreamData.constant({"receive_clause_lhs", full_code})
    end)
  end

  # Bitstring segment spec: <<a::CODE>>
  defp context_bitstring_segment_spec do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "<<a::" <> code <> ">>"
      StreamData.constant({"bitstring_segment_spec", full_code})
    end)
  end

  # Charlist interpolation: 'foo#{CODE}'
  defp context_charlist_interpolation do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "'foo\#{" <> code <> "}'"
      StreamData.constant({"charlist_interpolation", full_code})
    end)
  end

  # Sigil interpolation: ~s/foo#{CODE}/
  defp context_sigil_interpolation do
    StreamData.bind(token_sequence_gen(), fn code ->
      full_code = "~s/foo\#{" <> code <> "}/"
      StreamData.constant({"sigil_interpolation", full_code})
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
      context_struct_arg(),
      context_between_do_blocks(),
      context_ternary_first(),
      context_ternary_second(),
      context_ternary_third(),
      context_map_update(),
      context_after_parens_call(),
      context_after_dot(),
      context_paren_stab(),
      context_between_operators(),
      context_after_capture(),
      context_after_pin(),
      context_after_at(),
      context_after_bang(),
      context_after_not(),
      context_fn_when(),
      context_def_parens_arg(),
      context_keyword_list_value(),
      context_map_kv_value(),
      context_map_rocket_key(),
      context_case_clause_lhs(),
      context_case_clause_rhs(),
      context_if_condition(),
      context_if_else_body(),
      context_with_generator_rhs(),
      context_for_generator_rhs(),
      context_try_body(),
      context_receive_clause_lhs(),
      context_bitstring_segment_spec(),
      context_charlist_interpolation(),
      context_sigil_interpolation()
    ])
  end

  # ===========================================================================
  # Property Tests
  # ===========================================================================

  describe "token sequence conformance" do
    @tag :property
    @tag timeout: 120_000
    property "generated token sequences produce conformant ASTs" do
      check all(
              code <- token_sequence_gen(min_length: 1, max_length: 12),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - short" do
    @tag :property
    @tag timeout: 120_000
    property "short token sequences produce conformant ASTs" do
      check all(
              code <- token_sequence_gen(min_length: 1, max_length: 5),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - long" do
    @tag :property
    @tag timeout: 120_000
    property "long token sequences produce conformant ASTs" do
      check all(
              code <- token_sequence_gen(min_length: 8, max_length: 20),
              max_runs: 50_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - identifiers only" do
    @tag :property
    @tag timeout: 120_000
    property "identifier-only sequences produce conformant ASTs" do
      identifier_gen = StreamData.member_of(@identifiers)

      check all(
              tokens <- StreamData.list_of(identifier_gen, min_length: 1, max_length: 8),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - keywords only" do
    @tag :property
    @tag timeout: 120_000
    property "keyword-only sequences produce conformant ASTs" do
      keyword_gen = StreamData.member_of(@keywords)

      check all(
              tokens <- StreamData.list_of(keyword_gen, min_length: 1, max_length: 8),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - operators only" do
    @tag :property
    @tag timeout: 120_000
    property "operator-only sequences produce conformant ASTs" do
      operator_gen = StreamData.member_of(@operators)

      check all(
              tokens <- StreamData.list_of(operator_gen, min_length: 1, max_length: 8),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - containers only" do
    @tag :property
    @tag timeout: 120_000
    property "container-only sequences produce conformant ASTs" do
      container_gen = StreamData.member_of(@containers)

      check all(
              tokens <- StreamData.list_of(container_gen, min_length: 1, max_length: 12),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - identifiers and operators" do
    @tag :property
    @tag timeout: 120_000
    property "identifier and operator sequences produce conformant ASTs" do
      token_gen = StreamData.member_of(@identifiers ++ @operators)

      check all(
              tokens <- StreamData.list_of(token_gen, min_length: 1, max_length: 10),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - identifiers and containers" do
    @tag :property
    @tag timeout: 120_000
    property "identifier and container sequences produce conformant ASTs" do
      token_gen = StreamData.member_of(@identifiers ++ @containers)

      check all(
              tokens <- StreamData.list_of(token_gen, min_length: 1, max_length: 10),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - keywords and containers" do
    @tag :property
    @tag timeout: 120_000
    property "keyword and container sequences produce conformant ASTs" do
      token_gen = StreamData.member_of(@keywords ++ @containers)

      check all(
              tokens <- StreamData.list_of(token_gen, min_length: 1, max_length: 10),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - no containers" do
    @tag :property
    @tag timeout: 120_000
    property "sequences without containers produce conformant ASTs" do
      token_gen = StreamData.member_of(@identifiers ++ @keywords ++ @operators ++ @atoms ++ @numbers)

      check all(
              tokens <- StreamData.list_of(token_gen, min_length: 1, max_length: 10),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - basic expressions" do
    @tag :property
    @tag timeout: 120_000
    property "basic expression-like sequences produce conformant ASTs" do
      # More likely to produce valid expressions
      basic_tokens = @identifiers ++ @atoms ++ @numbers ++ ["(", ")", "[", "]", "{", "}", ","]
      token_gen = StreamData.member_of(basic_tokens)

      check all(
              tokens <- StreamData.list_of(token_gen, min_length: 1, max_length: 12),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  # ===========================================================================
  # Context-based Property Tests
  # ===========================================================================

  describe "token sequence in all contexts" do
    @tag :property
    @tag timeout: 120_000
    property "token sequences in random contexts produce conformant ASTs" do
      check all(
              {context, code} <- all_contexts_gen(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison_with_context(context, code)
      end
    end
  end

  describe "token sequence in containers" do
    @tag :property
    @tag timeout: 120_000
    property "token sequences in container contexts produce conformant ASTs" do
      container_contexts =
        StreamData.one_of([
          context_list(),
          context_tuple(),
          context_map(),
          context_struct(),
          context_parens(),
          context_bitstring()
        ])

      check all(
              {context, code} <- container_contexts,
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison_with_context(context, code)
      end
    end
  end

  describe "token sequence in calls" do
    @tag :property
    @tag timeout: 120_000
    property "token sequences in call contexts produce conformant ASTs" do
      call_contexts =
        StreamData.one_of([
          context_parens_call(),
          context_no_parens_call(),
          context_bracket_access(),
          context_after_dot()
        ])

      check all(
              {context, code} <- call_contexts,
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison_with_context(context, code)
      end
    end
  end

  describe "token sequence in fn expressions" do
    @tag :property
    @tag timeout: 120_000
    property "token sequences in fn contexts produce conformant ASTs" do
      fn_contexts =
        StreamData.one_of([
          context_fn_arg(),
          context_fn_body(),
          context_fn_when(),
          context_paren_stab()
        ])

      check all(
              {context, code} <- fn_contexts,
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison_with_context(context, code)
      end
    end
  end

  describe "token sequence in do blocks" do
    @tag :property
    @tag timeout: 120_000
    property "token sequences in do block contexts produce conformant ASTs" do
      do_contexts =
        StreamData.one_of([
          context_before_do(),
          context_after_do(),
          context_inside_do(),
          context_between_do_blocks()
        ])

      check all(
              {context, code} <- do_contexts,
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison_with_context(context, code)
      end
    end
  end

  describe "token sequence after unary operators" do
    @tag :property
    @tag timeout: 120_000
    property "token sequences after unary operators produce conformant ASTs" do
      unary_contexts =
        StreamData.one_of([
          context_after_capture(),
          context_after_pin(),
          context_after_at(),
          context_after_bang(),
          context_after_not()
        ])

      check all(
              {context, code} <- unary_contexts,
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison_with_context(context, code)
      end
    end
  end

  describe "token sequence in interpolation" do
    @tag :property
    @tag timeout: 120_000
    property "token sequences in interpolation contexts produce conformant ASTs" do
      interp_contexts =
        StreamData.one_of([
          context_interpolation(),
          context_charlist_interpolation(),
          context_sigil_interpolation()
        ])

      check all(
              {context, code} <- interp_contexts,
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison_with_context(context, code)
      end
    end
  end

  describe "token sequence in control flow" do
    @tag :property
    @tag timeout: 120_000
    property "token sequences in control flow contexts produce conformant ASTs" do
      control_contexts =
        StreamData.one_of([
          context_case_clause_lhs(),
          context_case_clause_rhs(),
          context_if_condition(),
          context_if_else_body(),
          context_with_generator_rhs(),
          context_for_generator_rhs(),
          context_try_body(),
          context_receive_clause_lhs()
        ])

      check all(
              {context, code} <- control_contexts,
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison_with_context(context, code)
      end
    end
  end

  describe "token sequence in operators" do
    @tag :property
    @tag timeout: 120_000
    property "token sequences in operator contexts produce conformant ASTs" do
      op_contexts =
        StreamData.one_of([
          context_after_pipe(),
          context_after_assignment(),
          context_between_operators(),
          context_ternary_first(),
          context_ternary_second(),
          context_ternary_third()
        ])

      check all(
              {context, code} <- op_contexts,
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison_with_context(context, code)
      end
    end
  end

  describe "token sequence in keyword/map contexts" do
    @tag :property
    @tag timeout: 120_000
    property "token sequences in keyword/map contexts produce conformant ASTs" do
      kv_contexts =
        StreamData.one_of([
          context_keyword_list_value(),
          context_map_kv_value(),
          context_map_rocket_key(),
          context_map_update()
        ])

      check all(
              {context, code} <- kv_contexts,
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison_with_context(context, code)
      end
    end
  end

  describe "token sequence in def" do
    @tag :property
    @tag timeout: 120_000
    property "token sequences in def contexts produce conformant ASTs" do
      check all(
              {context, code} <- context_def_parens_arg(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison_with_context(context, code)
      end
    end
  end

  describe "token sequence in bitstring spec" do
    @tag :property
    @tag timeout: 120_000
    property "token sequences in bitstring spec contexts produce conformant ASTs" do
      check all(
              {context, code} <- context_bitstring_segment_spec(),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison_with_context(context, code)
      end
    end
  end

  # ===========================================================================
  # Comparison Helper
  # ===========================================================================

  defp run_comparison(code) do
    run_comparison_with_context("standalone", code)
  end

  defp run_comparison_with_context(context, code) do
    capture_io(:standard_error, fn ->
    result =
      try do
        Code.string_to_quoted(code, @oracle_opts)
      rescue
        _ -> {:error, :oracle_crash}
      end

    case result do
      {:ok, {:__block__, _, []}} ->
        # Empty block, skip
        :ok

      {:ok, oracle_ast} ->
        # Parse with Toxic using same options as oracle
        case toxic_parse(code) do
          {:ok, toxic_ast} ->
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

          {:error, reason} ->
            flunk("""
            Parser error in context #{context} for: #{inspect(code)}

            Toxic:
            #{inspect(reason, pretty: true)}
            """)
        end

      {:error, _} ->
        # Oracle rejected, skip this sample
        :ok
    end
  end)
  end

  defp toxic_parse(code) do
    case ToxicParser.parse_string(code,
           mode: :strict,
           token_metadata: true,
           existing_atoms_only: false
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

  # ===========================================================================
  # AST Normalization
  # ===========================================================================

  @ignored_meta_keys [
    :range
  ]

  defp normalize_ast(ast) do
    ast
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
end
