defmodule ToxicParser.Property.TokenGrammarGenerators do
  @moduledoc """
  StreamData generators for grammar trees.

  Generates grammar tree nodes that can be compiled to Toxic tokens
  using `TokenCompiler.to_tokens/2`.

  ## Current Phase Support

  - **Increment 1**: Literals (integers, floats, chars, atoms, bools, nil),
    identifiers, and aliases.
  - **Increment 2**: Binary and unary operators with newline handling.
  - **Increment 3**: Calls (call_parens, call_no_parens_one, capture_int).
  - **Increment 4**: fn_single with stab clauses.
  - **Increment 5**: Full unmatched_expr coverage per grammar lines 163-171.
  - **Increment 6**: Full block_expr coverage per grammar lines 181-185.
  - **Increment 7**: paren_stab forms per grammar lines 277-279.
  - **Increment 8**: Full do_block coverage per grammar lines 322-329.
  - **Increment 9**: Full no_parens_expr coverage per grammar lines 173-179, 252-256.

  ## Grammar Coverage: unmatched_expr (Increment 5)

  All 9 productions from elixir_parser.yrl lines 163-171 are implemented:

  1. `matched_expr unmatched_op_expr` - gen_unmatched_op variant_a
  2. `unmatched_expr matched_op_expr` - gen_unmatched_op variant_b
  3. `unmatched_expr unmatched_op_expr` - gen_unmatched_op variant_c
  4. `unmatched_expr no_parens_op_expr` - gen_unmatched_op variant_d (warn_pipe)
  5. `unary_op_eol expr` - gen_unmatched_unary
  6. `at_op_eol expr` - gen_unmatched_at_op
  7. `capture_op_eol expr` - gen_unmatched_capture_op
  8. `ellipsis_op expr` - gen_unmatched_ellipsis
  9. `block_expr` - gen_block_expr (see below)

  ## Grammar Coverage: block_expr (Increment 6)

  All 5 productions from elixir_parser.yrl lines 181-185 are implemented:

  1. `dot_call_identifier call_args_parens do_block` - gen_block_parens
     Examples: `foo() do end`, `Mod.func() do end`, `expr.() do end`

  2. `dot_call_identifier call_args_parens call_args_parens do_block` - gen_block_parens_nested
     Examples: `foo()() do end`, `Mod.func()() do end` (higher-order function calls)

  3. `dot_do_identifier do_block` - gen_call_do
     Examples: `if true do end`, `Mod.if cond do end`, `expr.unless flag do end`
     Supports both simple and dotted do_identifier targets via gen_dotted_do_identifier_call.

  4. `dot_op_identifier call_args_no_parens_all do_block` - gen_block_no_parens_op
     Examples: `.+ 1 do end`, `expr.* arg do end` (operator-as-function with do block)

  5. `dot_identifier call_args_no_parens_all do_block` - gen_block_no_parens
     Examples: `foo 1 do end`, `Mod.func arg do end`

  ## Grammar Coverage: paren_stab (Increment 7)

  All 3 paren_stab productions from elixir_parser.yrl lines 277-279 are implemented:

  1. `open_paren stab_eoe ')' : build_paren_stab` - gen_paren_stab
     Examples: `(x -> x + 1)`, `(x, y -> x + y)`, `(a -> b; c -> d)`
     Parenthesized anonymous function patterns with 1+ stab clauses.

  2. `open_paren ';' stab_eoe ')' : build_paren_stab` - gen_paren_stab_semi
     Examples: `(; x -> x)`, `(; -> :ok)`
     With leading semicolon (trailing newline/semicolon from previous context).

  3. `open_paren ';' close_paren : build_paren_stab` - gen_paren_stab_empty
     Examples: `(;)`
     Empty parenthesized stab (edge case for macro expansions).

  ## Grammar Coverage: do_block (Increment 8)

  All 4 do_block productions from elixir_parser.yrl lines 322-329 are implemented:

  1. `do_eoe 'end'` - Empty do block
     Examples: `do end`
     Generated with empty body: `{:do_block, [], []}`

  2. `do_eoe stab_eoe 'end'` - Do block with stab clauses
     Examples: `do x -> y end`, `do a -> b; c -> d end`
     Generated with stab clauses: `{:do_block, [{:stab_clause, ...}, ...], []}`

  3. `do_eoe block_list 'end'` - Do block with extras (empty body)
     Examples: `do else :ok end`, `do rescue e -> handle(e) end`
     Generated with: `{:do_block, [], [{:block_item, :else, body}]}`

  4. `do_eoe stab_eoe block_list 'end'` - Stab clauses + block extras
     Examples: `do x -> y else :error end`, `do :ok after cleanup() end`
     Full combination with stab body and block extras.

  Block extras (block_item per grammar lines 368-374):
  - `:else` - else block with expressions (for if/unless)
  - `:rescue` - rescue block with stab clauses (for try)
  - `:catch` - catch block with stab clauses (for try)
  - `:after` - after block with expressions (for try/receive)

  ## Grammar Coverage: no_parens_expr (Increment 9)

  All 7 no_parens_expr productions from elixir_parser.yrl lines 173-179 are implemented:

  1. `matched_expr no_parens_op_expr` - gen_no_parens_op
     Examples: `foo + bar a, b` (binary op with no_parens right side)
     Generates `{:no_parens_op, left, op_eol, right}`

  2. `unary_op_eol no_parens_expr` - gen_no_parens_unary
     Examples: `not foo a, b`, `!bar x, y`
     Generates `{:no_parens_unary, {op_kind, op}, newlines, operand}`

  3. `at_op_eol no_parens_expr` - gen_no_parens_at_op
     Examples: `@foo a, b`
     Generates `{:no_parens_at_op, newlines, operand}`

  4. `capture_op_eol no_parens_expr` - gen_no_parens_capture_op
     Examples: `&foo a, b`
     Generates `{:no_parens_capture_op, newlines, operand}`

  5. `ellipsis_op no_parens_expr` - gen_no_parens_ellipsis
     Examples: `...foo a, b`
     Generates `{:no_parens_ellipsis, operand}`

  6. `no_parens_one_ambig_expr` - gen_no_parens_one_ambig_expr (lines 252-253)
     Examples: `foo bar a, b` (nested ambiguous call)
     Generates `{:no_parens_one_ambig, target, arg}`
     Where arg is a no_parens_expr.

  7. `no_parens_many_expr` - gen_no_parens_many_expr (lines 255-256)
     Examples: `foo a, b, c`, `foo x, y, key: val`
     Generates `{:no_parens_many, target, args}`
     Multi-argument calls without parentheses.

  ## Deferred to Future Phases (TODO)

  The following are intentionally not generated in the current phase:
  - **access_expr additions** (Phase 7+):
    - Bitstrings: `<<1, 2, 3>>`
    - Heredocs: `\"\"\"text\"\"\"`
    - Sigils: `~r/regex/`, `~s[string]`
    - Quoted atoms: `:\"hello world\"`
  - **Strings** (Phase 7+): bin_string interpolation, list_string
  """

  use ExUnitProperties

  alias ToxicParser.Property.GrammarTree

  # ===========================================================================
  # Atom/Identifier pools (same as existing generators)
  # ===========================================================================

  @identifiers ~w(foo bar baz qux spam eggs alpha beta gamma delta)a
  @aliases ~w(Foo Bar Baz Qux Remote Mod State Schema Context Config Default)a
  @atoms ~w(ok error foo bar baz one two three alice bob)a

  # Binary operators: {token_kind, operator_atom}
  # Per elixir_parser.yrl matched_op_expr rules (lines 187-204)
  @binary_ops [
    # match_op (=)
    {:match_op, :=},
    # Arithmetic (dual_op)
    {:dual_op, :+},
    {:dual_op, :-},
    {:mult_op, :*},
    {:mult_op, :/},
    # power_op (**)
    {:power_op, :**},
    # concat_op (++, --, <>, +++, ---)
    {:concat_op, :++},
    {:concat_op, :--},
    {:concat_op, :<>},
    {:concat_op, :+++},
    {:concat_op, :---},
    # range_op (..) as binary (ternary_op // is handled separately in gen_range_step)
    {:range_op, :..},
    # ternary_op (//) - include so no_parens_op can produce // as an operator per grammar
    {:ternary_op, :"//"},
    # xor_op (^^^)
    {:xor_op, :"^^^"},
    # Comparison (comp_op)
    {:comp_op, :==},
    {:comp_op, :!=},
    {:comp_op, :===},
    {:comp_op, :!==},
    {:comp_op, :=~},
    # Relational (rel_op)
    {:rel_op, :<},
    {:rel_op, :>},
    {:rel_op, :<=},
    {:rel_op, :>=},
    # Boolean (and_op, or_op)
    {:and_op, :and},
    {:and_op, :&&},
    {:and_op, :&&&},
    {:or_op, :or},
    {:or_op, :||},
    {:or_op, :|||},
    # in_op (in)
    {:in_op, :in},
    # in_match_op (<-, \\)
    {:in_match_op, :<-},
    {:in_match_op, :\\},
    # type_op (::)
    {:type_op, :"::"},
    # when_op (when)
    {:when_op, :when},
    # arrow_op (<<<, >>>, <~, ~>, <<~, ~>>, <~>, <|>)
    {:arrow_op, :<<<},
    {:arrow_op, :>>>},
    {:arrow_op, :<~},
    {:arrow_op, :~>},
    {:arrow_op, :<<~},
    {:arrow_op, :~>>},
    {:arrow_op, :<~>},
    {:arrow_op, :"<|>"},
    # pipe_op (|>, |)
    {:pipe_op, :|>},
    {:pipe_op, :|}
  ]

  # Unary operators: {token_kind, operator_atom}
  # Per elixir_parser.yrl unary_op_eol rules (lines 402-407)
  # and Code.Identifier.unary_op (line 21): :!, :^, :not, :+, :-, :~~~
  # Note: ternary_op (//) is NOT a unary operator - it's only valid after range_op (..)
  # as in 1..10//2. See gen_range_step for that pattern.
  @unary_ops [
    {:unary_op, :not},
    {:unary_op, :!},
    {:unary_op, :^},
    {:unary_op, :"~~~"},
    {:dual_op, :+},
    {:dual_op, :-}
  ]

  # Arrow operators that can trigger warn_pipe when followed by no_parens_one_expr
  # Per grammar line 209: matched_op_expr -> arrow_op_eol no_parens_one_expr : warn_pipe('$1', '$2')
  @arrow_ops [
    {:arrow_op, :<<<},
    {:arrow_op, :>>>},
    {:arrow_op, :<~},
    {:arrow_op, :~>},
    {:arrow_op, :<<~},
    {:arrow_op, :~>>},
    {:arrow_op, :<~>},
    {:arrow_op, :"<|>"},
    {:pipe_op, :|>}
  ]

  # Fallback literals when budget is exhausted
  @fallback_literals [nil, 0, :ok]

  def atom_pool, do: @atoms
  def identifier_pool, do: @identifiers
  def alias_pool, do: @aliases
  def binary_op_pool, do: @binary_ops
  def unary_op_pool, do: @unary_ops
  def arrow_op_pool, do: @arrow_ops

  # ===========================================================================
  # Public API: grammar/1
  # ===========================================================================

  @doc """
  Generate a grammar tree using category-aware generators.

  Models all grammar variants per elixir_parser.yrl:
  - `grammar -> expr_list` (no leading/trailing eoe)
  - `grammar -> expr_list eoe` (trailing eoe)
  - `grammar -> eoe expr_list` (leading eoe)
  - `grammar -> eoe expr_list eoe` (both)
  - `grammar -> eoe` (just eoe, empty program)
  - `grammar -> '$empty'` (completely empty)

  Uses `{:grammar_v2, leading_eoe, [{expr, eoe|nil}], trailing_eoe}` format.

  ## Options

  - `:max_depth` - Maximum expression depth, default 4
  - `:max_nodes` - Maximum nodes in the tree, default 100
  - `:max_forms` - Maximum top-level expressions, default 3
  """
  @spec grammar(keyword()) :: StreamData.t(GrammarTree.t())
  def grammar(opts \\ []) do
    max_depth = Keyword.get(opts, :max_depth, 4)
    max_nodes = Keyword.get(opts, :max_nodes, 100)
    max_forms = Keyword.get(opts, :max_forms, 3)

    # Set allow_unmatched: true for top-level context
    context = %{
      GrammarTree.new_context()
      | allow_unmatched: Keyword.get(opts, :allow_unmatched, true),
        allow_no_parens: Keyword.get(opts, :allow_no_parens, true)
    }

    state = %{budget: GrammarTree.initial_budget(max_depth, max_nodes), context: context}

    # Generate all grammar variants with appropriate frequencies
    StreamData.frequency([
      # grammar -> expr_list (most common)
      {5, gen_grammar_expr_list(state, max_forms)},
      # grammar -> expr_list eoe (trailing newline - common)
      {3, gen_grammar_expr_list_eoe(state, max_forms)},
      # grammar -> eoe expr_list (leading newline - less common)
      {1, gen_grammar_eoe_expr_list(state, max_forms)},
      # grammar -> eoe expr_list eoe (both - rare)
      {1, gen_grammar_eoe_expr_list_eoe(state, max_forms)},
      # grammar -> eoe (only eoe - edge case)
      {1, gen_grammar_eoe_only(state)}
      # grammar -> '$empty' (completely empty)
      # Disabled: Oracle and Spitfire produce different metadata for empty blocks
      # {1, gen_grammar_empty()}
    ])
  end

  # grammar -> expr_list : build_block(reverse('$1')).
  defp gen_grammar_expr_list(state, max_forms) do
    StreamData.bind(StreamData.integer(1..max_forms), fn count ->
      gen_expr_list(state, count)
    end)
    |> StreamData.map(fn exprs -> {:grammar_v2, nil, exprs, nil} end)
  end

  # grammar -> expr_list eoe : build_block(reverse(annotate_eoe('$2', '$1'))).
  defp gen_grammar_expr_list_eoe(state, max_forms) do
    StreamData.bind(StreamData.integer(1..max_forms), fn count ->
      StreamData.bind(gen_expr_list(state, count), fn exprs ->
        StreamData.bind(gen_eoe(), fn trailing_eoe ->
          StreamData.constant({:grammar_v2, nil, exprs, trailing_eoe})
        end)
      end)
    end)
  end

  # grammar -> eoe expr_list : build_block(reverse('$2')).
  defp gen_grammar_eoe_expr_list(state, max_forms) do
    StreamData.bind(gen_eoe(), fn leading_eoe ->
      StreamData.bind(StreamData.integer(1..max_forms), fn count ->
        gen_expr_list(state, count)
      end)
      |> StreamData.map(fn exprs -> {:grammar_v2, leading_eoe, exprs, nil} end)
    end)
  end

  # grammar -> eoe expr_list eoe : build_block(reverse(annotate_eoe('$3', '$2'))).
  defp gen_grammar_eoe_expr_list_eoe(state, max_forms) do
    StreamData.bind(gen_eoe(), fn leading_eoe ->
      StreamData.bind(StreamData.integer(1..max_forms), fn count ->
        StreamData.bind(gen_expr_list(state, count), fn exprs ->
          StreamData.bind(gen_eoe(), fn trailing_eoe ->
            StreamData.constant({:grammar_v2, leading_eoe, exprs, trailing_eoe})
          end)
        end)
      end)
    end)
  end

  # grammar -> eoe : {'__block__', meta_from_token('$1'), []}.
  # Represented as grammar_v2 with leading_eoe and empty expr list.
  defp gen_grammar_eoe_only(_state) do
    StreamData.bind(gen_eoe(), fn leading_eoe ->
      StreamData.constant({:grammar_v2, leading_eoe, [], nil})
    end)
  end

  # ===========================================================================
  # Generator: expr_list (expressions with eoe markers)
  # ===========================================================================

  # Per grammar rules:
  #   expr_list -> expr : ['$1'].
  #   expr_list -> expr_list eoe expr : ['$3' | annotate_eoe('$2', '$1')].
  #
  # The eoe goes BETWEEN expressions, not after the last one.
  # Returns list of {expr, eoe | nil} tuples where last has nil.

  defp gen_expr_list(_state, count) when count <= 0 do
    StreamData.constant([])
  end

  defp gen_expr_list(state, 1) do
    # Single expression, NO eoe (per: expr_list -> expr)
    StreamData.bind(gen_expr(state), fn expr ->
      StreamData.constant([{expr, nil}])
    end)
  end

  defp gen_expr_list(state, count) when count > 1 do
    # First expr has eoe after it (between this and next)
    StreamData.bind(gen_expr(state), fn expr ->
      StreamData.bind(gen_eoe(), fn eoe ->
        StreamData.bind(gen_expr_list(GrammarTree.decr_nodes(state), count - 1), fn rest ->
          StreamData.constant([{expr, eoe} | rest])
        end)
      end)
    end)
  end

  # ===========================================================================
  # Generator: expressions
  # ===========================================================================

  # Generate expression based on context.
  # Per grammar rule: expr -> matched_expr | no_parens_expr | unmatched_expr
  defp gen_expr(state) do
    if GrammarTree.budget_exhausted?(state) do
      # Literal is a matched_expr
      gen_fallback_literal()
    else
      if state.context.allow_unmatched or state.context.allow_no_parens do
        # Top-level context: can generate any expression type
        # Per grammar: expr -> matched_expr | no_parens_expr | unmatched_expr
        StreamData.frequency(
          [
            {6, gen_matched_expr(state)}
          ] ++
            if(state.context.allow_unmatched, do: [{3, gen_unmatched_expr(state)}], else: []) ++
            if(state.context.allow_no_parens, do: [{2, gen_no_parens_expr(state)}], else: [])
        )
      else
        # Restricted context (e.g., operand position): only matched
        gen_matched_expr(state)
      end
    end
  end

  # Generate a simple expression (no operators) for use as operands
  defp gen_simple_expr do
    StreamData.frequency([
      {5, gen_literal()},
      {3, gen_identifier()},
      {2, gen_alias()}
    ])
  end

  defp gen_fallback_literal do
    StreamData.member_of(@fallback_literals)
    |> StreamData.map(fn
      nil -> :nil_lit
      0 -> {:int, 0, :dec, ~c"0"}
      :ok -> {:atom_lit, :ok}
    end)
  end

  # ===========================================================================
  # Generator: literals
  # ===========================================================================

  defp gen_literal do
    StreamData.frequency([
      {4, gen_int()},
      {2, gen_float()},
      {1, gen_char()},
      {3, gen_atom_lit()},
      {2, gen_bool_lit()},
      {1, StreamData.constant(:nil_lit)}
    ])
  end

  defp gen_int do
    StreamData.frequency([
      {5, gen_int_dec()},
      {1, gen_int_hex()},
      {1, gen_int_bin()},
      {1, gen_int_oct()}
    ])
  end

  defp gen_int_dec do
    StreamData.integer(-1000..1000)
    |> StreamData.map(fn n ->
      chars = Integer.to_charlist(n)
      {:int, n, :dec, chars}
    end)
  end

  defp gen_int_hex do
    StreamData.integer(0..255)
    |> StreamData.map(fn n ->
      hex = Integer.to_string(n, 16)
      chars = String.to_charlist("0x" <> hex)
      {:int, n, :hex, chars}
    end)
  end

  defp gen_int_bin do
    StreamData.integer(0..15)
    |> StreamData.map(fn n ->
      bin = Integer.to_string(n, 2)
      chars = String.to_charlist("0b" <> bin)
      {:int, n, :bin, chars}
    end)
  end

  defp gen_int_oct do
    StreamData.integer(0..63)
    |> StreamData.map(fn n ->
      oct = Integer.to_string(n, 8)
      chars = String.to_charlist("0o" <> oct)
      {:int, n, :oct, chars}
    end)
  end

  defp gen_float do
    StreamData.bind(StreamData.integer(0..100), fn int_part ->
      StreamData.bind(StreamData.integer(0..99), fn frac_part ->
        value = int_part + frac_part / 100.0
        # Ensure consistent representation
        chars = :erlang.float_to_list(value, [{:decimals, 2}, :compact])
        StreamData.constant({:float, value, chars})
      end)
    end)
  end

  defp gen_char do
    StreamData.frequency([
      {5, gen_simple_char()},
      {1, gen_escape_char()}
    ])
  end

  defp gen_simple_char do
    StreamData.integer(?a..?z)
    |> StreamData.map(fn c ->
      chars = [??, c]
      {:char, c, chars}
    end)
  end

  defp gen_escape_char do
    StreamData.member_of([?\n, ?\t, ?\r, ?\\])
    |> StreamData.map(fn c ->
      escape =
        case c do
          ?\n -> ~c"?\\n"
          ?\t -> ~c"?\\t"
          ?\r -> ~c"?\\r"
          ?\\ -> ~c"?\\\\"
        end

      {:char, c, escape}
    end)
  end

  defp gen_atom_lit do
    StreamData.member_of(@atoms)
    |> StreamData.map(fn atom -> {:atom_lit, atom} end)
  end

  defp gen_bool_lit do
    StreamData.member_of([true, false])
    |> StreamData.map(fn b -> {:bool_lit, b} end)
  end

  # ===========================================================================
  # Generator: identifiers and aliases
  # ===========================================================================

  defp gen_identifier do
    StreamData.member_of(@identifiers)
    |> StreamData.map(fn atom -> {:identifier, atom} end)
  end

  defp gen_alias do
    StreamData.frequency([
      # Simple alias: Foo
      {4, StreamData.member_of(@aliases) |> StreamData.map(&{:alias, &1})},
      # Dotted alias: Foo.Bar (multi-segment alias)
      {2, gen_dot_alias()}
    ])
  end

  # Generate a dotted alias: Foo.Bar or Foo.Bar.Baz
  # Per grammar: dot_alias -> matched_expr dot_op alias
  defp gen_dot_alias do
    # First segment is always a simple alias
    StreamData.bind(StreamData.member_of(@aliases), fn first ->
      # Optionally add 1-2 more segments
      StreamData.bind(StreamData.integer(1..2), fn extra_count ->
        gen_alias_segments(extra_count)
        |> StreamData.map(fn segments ->
          {:dot_alias, [{:alias, first} | segments]}
        end)
      end)
    end)
  end

  defp gen_alias_segments(0), do: StreamData.constant([])

  defp gen_alias_segments(count) when count > 0 do
    StreamData.bind(StreamData.member_of(@aliases), fn segment ->
      StreamData.bind(gen_alias_segments(count - 1), fn rest ->
        StreamData.constant([{:alias, segment} | rest])
      end)
    end)
  end

  # ===========================================================================
  # Generator: binary operators
  # ===========================================================================

  # Generate op_eol: {op_kind, op} with optional newlines
  defp gen_op_eol do
    StreamData.bind(StreamData.member_of(@binary_ops), fn {op_kind, op} ->
      # Most of the time no newline, occasionally 1 newline
      StreamData.bind(gen_newlines(), fn newlines ->
        StreamData.constant({:op_eol, {op_kind, op}, newlines})
      end)
    end)
  end

  # Generate newline count (0 most of the time, occasionally 1)
  defp gen_newlines do
    StreamData.frequency([
      {8, StreamData.constant(0)},
      {2, StreamData.constant(1)}
    ])
  end

  # ===========================================================================
  # Generator: calls
  # ===========================================================================

  # Generate a call with parentheses: foo(a, b) or expr.(a)
  # Grammar: parens_call -> dot_call_identifier call_args_parens
  defp gen_call_parens(state) do
    child_state = GrammarTree.decr_depth(state)

    # Generate target - either a paren_identifier or a dot_call
    # Per grammar: dot_call_identifier -> dot_paren_identifier | matched_expr dot_call_op
    target_gen =
      StreamData.frequency([
        {4, gen_paren_identifier()},
        {2, gen_dot_paren_identifier(child_state)},
        {1, gen_dot_call_target(child_state)}
      ])

    # Generate arguments using full call_args_parens grammar
    args_gen = gen_call_args_parens(child_state)

    StreamData.bind(target_gen, fn target ->
      StreamData.bind(args_gen, fn args ->
        StreamData.constant({:call_parens, target, args})
      end)
    end)
  end

  # Generate nested parens call: foo()() (no do block)
  # Grammar: parens_call -> dot_call_identifier call_args_parens call_args_parens
  defp gen_call_parens_nested(state) do
    child_state = GrammarTree.decr_depth(state)

    target_gen =
      StreamData.frequency([
        {4, gen_paren_identifier()},
        {2, gen_dot_paren_identifier(child_state)},
        {1, gen_dot_call_target(child_state)}
      ])

    args1_gen = gen_call_args_parens(child_state)
    args2_gen = gen_call_args_parens(child_state)

    StreamData.bind(target_gen, fn target ->
      StreamData.bind(args1_gen, fn args1 ->
        StreamData.bind(args2_gen, fn args2 ->
          StreamData.constant({:call_parens_nested, target, args1, args2})
        end)
      end)
    end)
  end

  # ===========================================================================
  # Generator: container_expr, container_args, kw_eol, kw_base, kw_call, kw_data
  # ===========================================================================
  #
  # Per grammar lines 533-542:
  #   container_expr -> matched_expr
  #   container_expr -> unmatched_expr
  #   container_expr -> no_parens_expr  (ERROR case - error_no_parens_container_strict)
  #
  #   container_args_base -> container_expr
  #   container_args_base -> container_args_base ',' container_expr
  #
  #   container_args -> container_args_base
  #   container_args -> container_args_base ','                   (trailing comma)
  #   container_args -> container_args_base ',' kw_data           (positional + kw)
  #
  # Per grammar lines 566-582:
  #   kw_eol -> kw_identifier [eol]         (bare keyword: foo:)
  #   kw_eol -> kw_identifier_safe [eol]    (quoted keyword: "foo":)
  #   kw_eol -> kw_identifier_unsafe [eol]  (interpolated keyword: "#{x}":)
  #
  #   kw_base -> kw_eol container_expr (',' kw_eol container_expr)*
  #
  #   kw_call -> kw_base
  #   kw_call -> kw_base ','              (trailing comma, warns)
  #   kw_call -> kw_base ',' matched_expr (ERROR - maybe_bad_keyword_call_follow_up)
  #
  #   kw_data -> kw_base
  #   kw_data -> kw_base ','              (trailing comma)
  #   kw_data -> kw_base ',' matched_expr (ERROR - maybe_bad_keyword_data_follow_up)

  @doc """
  Generate a container_expr.

  Per grammar lines 533-535:
  - container_expr -> matched_expr
  - container_expr -> unmatched_expr
  - container_expr -> no_parens_expr (ERROR - not generated in normal mode)

  Container expressions are used inside containers (lists, maps, tuples, etc.)
  where no_parens_expr would be ambiguous.
  """
  def gen_container_expr(state) do
    child_state = GrammarTree.decr_depth(state)

    if child_state.budget.depth <= 1 do
      gen_simple_expr()
    else
      restricted_state = restrict_unmatched(child_state)

      StreamData.frequency([
        # matched_expr - most common
        {6, gen_matched_expr(restricted_state)},
        # unmatched_expr - block expressions, unary with unmatched args
        {2, gen_unmatched_expr(restricted_state)}
        # no_parens_expr is ERROR - not generated
      ])
    end
  end

  @doc """
  Generate container_args_base - a list of comma-separated container expressions.

  Per grammar lines 537-538:
  - container_args_base -> container_expr
  - container_args_base -> container_args_base ',' container_expr
  """
  def gen_container_args_base(state, min_count \\ 1, max_count \\ 3) do
    child_state = GrammarTree.decr_depth(state)

    StreamData.bind(StreamData.integer(min_count..max_count), fn count ->
      gen_container_expr_list(child_state, count)
    end)
  end

  defp gen_container_expr_list(_state, 0), do: StreamData.constant([])

  defp gen_container_expr_list(state, count) when count > 0 do
    StreamData.bind(gen_container_expr(state), fn expr ->
      StreamData.bind(
        gen_container_expr_list(GrammarTree.decr_nodes(state), count - 1),
        fn rest ->
          StreamData.constant([expr | rest])
        end
      )
    end)
  end

  @doc """
  Generate container_args.

  Per grammar lines 540-542:
  - container_args -> container_args_base
  - container_args -> container_args_base ','                (trailing comma)
  - container_args -> container_args_base ',' kw_data        (positional + kw)

  Returns one of:
  - `{:container_args, exprs}` - just positional exprs
  - `{:container_args_trailing, exprs}` - positional with trailing comma
  - `{:container_args_with_kw, exprs, kw_data}` - positional + keyword data
  """
  def gen_container_args(state) do
    child_state = GrammarTree.decr_depth(state)

    StreamData.frequency([
      # Just positional
      {6, gen_container_args_base(child_state) |> StreamData.map(&{:container_args, &1})},
      # Trailing comma
      {1,
       gen_container_args_base(child_state) |> StreamData.map(&{:container_args_trailing, &1})},
      # Positional + kw_data
      {3,
       StreamData.bind(gen_container_args_base(child_state, 1, 2), fn positional ->
         StreamData.bind(gen_kw_data(child_state), fn kw_data ->
           StreamData.constant({:container_args_with_kw, positional, kw_data})
         end)
       end)}
    ])
  end

  @doc """
  Generate a kw_eol - keyword identifier with optional newline.

  Per grammar lines 566-571:
  - kw_eol -> kw_identifier [eol]
  - kw_eol -> kw_identifier_safe [eol]
  - kw_eol -> kw_identifier_unsafe [eol]

  For simplicity, we generate bare kw_identifier (atom followed by colon).
  The token will be `:kw_identifier` which is the terminal.

  Returns `{:kw_eol, key, has_eol}` where key is an atom.
  """
  def gen_kw_eol do
    StreamData.bind(StreamData.member_of(@identifiers), fn key ->
      StreamData.bind(StreamData.boolean(), fn has_eol ->
        StreamData.constant({:kw_eol, key, has_eol})
      end)
    end)
  end

  @doc """
  Generate kw_base - one or more keyword pairs.

  Per grammar lines 573-574:
  - kw_base -> kw_eol container_expr
  - kw_base -> kw_base ',' kw_eol container_expr

  Returns a list of `{kw_eol, container_expr}` pairs.
  """
  def gen_kw_base(state, min_pairs \\ 1, max_pairs \\ 3) do
    child_state = GrammarTree.decr_depth(state)

    StreamData.bind(StreamData.integer(min_pairs..max_pairs), fn count ->
      gen_kw_base_pairs(child_state, count)
    end)
  end

  defp gen_kw_base_pairs(_state, 0), do: StreamData.constant([])

  defp gen_kw_base_pairs(state, count) when count > 0 do
    StreamData.bind(gen_kw_eol(), fn kw_eol ->
      StreamData.bind(gen_container_expr(state), fn value ->
        StreamData.bind(gen_kw_base_pairs(GrammarTree.decr_nodes(state), count - 1), fn rest ->
          StreamData.constant([{kw_eol, value} | rest])
        end)
      end)
    end)
  end

  @doc """
  Generate kw_call - keyword arguments for function calls inside parentheses.

  Per grammar lines 576-578:
  - kw_call -> kw_base
  - kw_call -> kw_base ','  (trailing comma, warns but valid)
  - kw_call -> kw_base ',' matched_expr (ERROR - maybe_bad_keyword_call_follow_up)

  Returns one of:
  - `{:kw_call, pairs}` - keyword pairs
  - `{:kw_call_trailing, pairs}` - with trailing comma

  Note: The error case (kw_base ',' matched_expr) is not generated.
  """
  def gen_kw_call(state) do
    StreamData.frequency([
      {8, gen_kw_base(state) |> StreamData.map(&{:kw_call, &1})},
      {1, gen_kw_base(state) |> StreamData.map(&{:kw_call_trailing, &1})}
    ])
  end

  @doc """
  Generate kw_data - keyword data for containers (lists, maps, brackets).

  Per grammar lines 580-582:
  - kw_data -> kw_base
  - kw_data -> kw_base ','  (trailing comma)
  - kw_data -> kw_base ',' matched_expr (ERROR - maybe_bad_keyword_data_follow_up)

  Returns one of:
  - `{:kw_data, pairs}` - keyword pairs
  - `{:kw_data_trailing, pairs}` - with trailing comma

  Note: The error case (kw_base ',' matched_expr) is not generated.
  """
  def gen_kw_data(state) do
    StreamData.frequency([
      {8, gen_kw_base(state) |> StreamData.map(&{:kw_data, &1})},
      {1, gen_kw_base(state) |> StreamData.map(&{:kw_data_trailing, &1})}
    ])
  end

  # Generate keyword arguments for no-parens call: a: 1 or a: 1, b: 2
  # Per grammar: call_args_no_parens_kw -> call_args_no_parens_kw_expr [',' ...]
  # call_args_no_parens_kw_expr -> kw_eol matched_expr | kw_eol no_parens_expr
  defp gen_call_args_no_parens_kw(state \\ GrammarTree.initial_state()) do
    StreamData.bind(StreamData.integer(1..3), fn count ->
      gen_kw_arg_list(state, count)
    end)
    |> StreamData.map(fn pairs -> {:kw_args, pairs} end)
  end

  # Generate list of keyword argument pairs
  defp gen_kw_arg_list(_state, 0), do: StreamData.constant([])

  defp gen_kw_arg_list(state, count) when count > 0 do
    # Allow keyword values to be either simple/matched expressions or
    # occasionally full no_parens_expr to model nested no-parens in keywords
    value_gen =
      if state.budget.depth <= 1 do
        gen_simple_expr()
      else
        StreamData.frequency([
          {4, gen_simple_expr()},
          {1, gen_no_parens_expr(GrammarTree.decr_depth(state))}
        ])
      end

    StreamData.bind(StreamData.member_of(@identifiers), fn key ->
      StreamData.bind(value_gen, fn value ->
        StreamData.bind(gen_kw_arg_list(GrammarTree.decr_nodes(state), count - 1), fn rest ->
          StreamData.constant([{key, value} | rest])
        end)
      end)
    end)
  end

  # ===========================================================================
  # Generator: call_args_parens
  # ===========================================================================
  #
  # Per grammar lines 553-562:
  #   call_args_parens -> open_paren ')' :                                      % empty
  #   call_args_parens -> open_paren no_parens_expr close_paren :               % single no_parens_expr
  #   call_args_parens -> open_paren kw_call close_paren :                      % keyword only
  #   call_args_parens -> open_paren call_args_parens_base close_paren :        % positional args
  #   call_args_parens -> open_paren call_args_parens_base ',' kw_call close_paren : % positional + kw
  #
  # call_args_parens_expr (lines 546-548) -> matched_expr | unmatched_expr
  # call_args_parens_base (lines 550-551) -> call_args_parens_expr (',' call_args_parens_expr)*
  #
  # Returns one of:
  # - `{:call_args_parens, :empty}` - empty args ()
  # - `{:call_args_parens, {:no_parens_expr, expr}}` - single no_parens_expr
  # - `{:call_args_parens, {:kw_only, pairs}}` - keyword only
  # - `{:call_args_parens, {:positional, exprs}}` - positional args only
  # - `{:call_args_parens, {:positional_with_kw, exprs, kw_pairs}}` - positional + trailing kw

  @doc """
  Generate call_args_parens - arguments inside parentheses for function calls.

  Per grammar lines 553-562, this covers all valid argument forms:
  - Empty: `()`
  - Single no_parens_expr: `(foo bar)` - special handling for nested calls
  - Keywords only: `(a: 1, b: 2)`
  - Positional args: `(1, 2, 3)` - call_args_parens_base
  - Positional + keywords: `(1, 2, a: 3)` - call_args_parens_base + kw_call

  The `open_paren` and `close_paren` productions (lines 378-381) allow:
  - open_paren -> '(' | '(' eol
  - close_paren -> ')' | eol ')'
  """
  def gen_call_args_parens(state) do
    child_state = GrammarTree.decr_depth(state)

    if child_state.budget.depth <= 1 do
      # At shallow depth, only simple forms (no nested no_parens_expr)
      StreamData.frequency([
        # Empty: ()
        {2, StreamData.constant({:call_args_parens, :empty})},
        # Keyword only: (a: 1)
        {2, gen_kw_call(child_state) |> StreamData.map(&{:call_args_parens, {:kw_only, &1}})},
        # Positional only: (1, 2)
        {4,
         gen_call_args_parens_base_simple(1, 3)
         |> StreamData.map(&{:call_args_parens, {:positional, &1}})},
        # Positional + kw: (1, a: 2)
        {2,
         gen_positional_with_kw_simple(child_state) |> StreamData.map(&{:call_args_parens, &1})}
      ])
    else
      restricted_state = restrict_unmatched(child_state)

      StreamData.frequency([
        # Empty: ()
        {2, StreamData.constant({:call_args_parens, :empty})},
        # Single no_parens_expr: (foo bar)
        {1,
         gen_no_parens_expr(child_state)
         |> StreamData.map(&{:call_args_parens, {:no_parens_expr, &1}})},
        # Keyword only: (a: 1, b: 2)
        {2,
         gen_kw_call(restricted_state) |> StreamData.map(&{:call_args_parens, {:kw_only, &1}})},
        # Positional only: (expr, expr)
        {4,
         gen_call_args_parens_base(restricted_state, 1, 3)
         |> StreamData.map(&{:call_args_parens, {:positional, &1}})},
        # Positional + kw: (expr, a: 1)
        {2, gen_positional_with_kw(restricted_state) |> StreamData.map(&{:call_args_parens, &1})}
      ])
    end
  end

  # Generate call_args_parens_base with simple expressions
  defp gen_call_args_parens_base_simple(min_args, max_args) do
    StreamData.bind(StreamData.integer(min_args..max_args), fn count ->
      gen_simple_expr_list(count)
    end)
  end

  # Generate a list of simple expressions
  defp gen_simple_expr_list(0), do: StreamData.constant([])

  defp gen_simple_expr_list(count) when count > 0 do
    StreamData.bind(gen_simple_expr(), fn expr ->
      StreamData.bind(gen_simple_expr_list(count - 1), fn rest ->
        StreamData.constant([expr | rest])
      end)
    end)
  end

  # Generate call_args_parens_base with matched/unmatched expressions
  defp gen_call_args_parens_base(state, min_args, max_args) do
    StreamData.bind(StreamData.integer(min_args..max_args), fn count ->
      gen_call_args_parens_expr_list(state, count)
    end)
  end

  # Generate a list of call_args_parens_expr (matched or unmatched, not no_parens)
  defp gen_call_args_parens_expr_list(_state, 0), do: StreamData.constant([])

  # Single argument: avoid unmatched at the end to prevent ambiguity with do blocks
  defp gen_call_args_parens_expr_list(state, 1) do
    expr_gen =
      StreamData.frequency([
        {4, gen_sub_matched_expr(state)},
        {2, gen_matched_expr(state)}
      ])

    StreamData.bind(expr_gen, fn expr -> StreamData.constant([expr]) end)
  end

  defp gen_call_args_parens_expr_list(state, count) when count > 1 do
    # call_args_parens_expr -> matched_expr | unmatched_expr
    # Allow unmatched expressions in non-final positions (safer than final position)
    expr_gen =
      StreamData.frequency([
        {4, gen_sub_matched_expr(state)},
        {3, gen_unmatched_expr(state)},
        {2, gen_matched_expr(state)}
      ])

    StreamData.bind(expr_gen, fn expr ->
      StreamData.bind(
        gen_call_args_parens_expr_list(GrammarTree.decr_nodes(state), count - 1),
        fn rest ->
          StreamData.constant([expr | rest])
        end
      )
    end)
  end

  # Generate positional args + trailing kw (simple version)
  defp gen_positional_with_kw_simple(state) do
    StreamData.bind(StreamData.integer(1..2), fn pos_count ->
      StreamData.bind(gen_simple_expr_list(pos_count), fn positional ->
        StreamData.bind(gen_kw_call(state), fn kw_call ->
          StreamData.constant({:positional_with_kw, positional, kw_call})
        end)
      end)
    end)
  end

  # Generate positional args + trailing kw (full version)
  defp gen_positional_with_kw(state) do
    StreamData.bind(StreamData.integer(1..2), fn pos_count ->
      StreamData.bind(gen_call_args_parens_expr_list(state, pos_count), fn positional ->
        StreamData.bind(gen_kw_call(state), fn kw_call ->
          StreamData.constant({:positional_with_kw, positional, kw_call})
        end)
      end)
    end)
  end

  # Generate no_parens_one_expr: identifier/dotted_identifier/op_identifier with one arg
  # Per grammar: no_parens_one_expr -> dot_op_identifier call_args_no_parens_one
  #              no_parens_one_expr -> dot_identifier call_args_no_parens_one
  defp gen_call_no_parens_one(state) do
    child_state = GrammarTree.decr_depth(state)

    # Target can be simple identifier, dotted identifier, dotted operator identifier, or operator-as-identifier
    target_gen =
      StreamData.frequency([
        # Simple identifier: foo bar
        {4, StreamData.member_of(@identifiers) |> StreamData.map(&{:identifier, &1})},
        # Dotted identifier: Mod.fun bar or expr.fun bar
        {2, gen_dot_identifier_for_call(child_state)},
        # Dotted operator identifier: Expr.+ bar (rare)
        {1, gen_dot_op_identifier_for_call(child_state)},
        # Operator-as-identifier: +/2, -/2 (op_identifier)
        {1,
         StreamData.bind(StreamData.member_of(@binary_ops), fn {_, op} ->
           StreamData.constant({:op_identifier, op})
         end)}
      ])

    # Args can be a single matched_expr or keyword arguments
    # Per grammar: call_args_no_parens_one -> matched_expr | no_parens_kw
    args_gen =
      if child_state.budget.depth <= 1 do
        StreamData.frequency([
          # Single arg (simple): foo bar
          {4, gen_simple_expr() |> StreamData.map(&{:single_arg, &1})},
          # Keyword args: foo a: 1, b: 2
          {2, gen_call_args_no_parens_kw()}
        ])
      else
        StreamData.frequency([
          # Single arg (matched_expr): foo (a + b)
          {4, gen_matched_expr(child_state) |> StreamData.map(&{:single_arg, &1})},
          # Keyword args: foo a: 1, b: 2
          {2, gen_call_args_no_parens_kw()}
        ])
      end

    StreamData.bind(target_gen, fn target ->
      StreamData.bind(args_gen, fn args ->
        StreamData.constant({:call_no_parens_one, target, args})
      end)
    end)
  end

  # Generate dotted identifier for call target: Mod.fun, foo.bar, or expr.identifier
  # Per grammar: dot_identifier -> identifier | matched_expr dot_op identifier
  defp gen_dot_identifier_for_call(state) do
    left_gen =
      if state.budget.depth <= 1 do
        # At low depth, only use simple expressions
        StreamData.frequency([
          {2, gen_alias()},
          {1, gen_identifier()}
        ])
      else
        # At higher depth, allow matched_expr on left side
        StreamData.frequency([
          {3, gen_alias()},
          {2, gen_identifier()},
          {1, gen_matched_expr(GrammarTree.decr_depth(state))}
        ])
      end

    StreamData.bind(left_gen, fn left ->
      StreamData.bind(StreamData.member_of(@identifiers), fn right_name ->
        StreamData.constant({:dot_identifier, left, right_name})
      end)
    end)
  end

  # Generate a dotted operator identifier for use as call target: expr.+
  # Per grammar: dot_op_identifier -> op_identifier | matched_expr dot_op op_identifier
  defp gen_dot_op_identifier_for_call(state) do
    left_gen =
      if state.budget.depth <= 1 do
        # At low depth, only use simple expressions
        gen_simple_expr()
      else
        # At higher depth, allow matched_expr on left side
        StreamData.frequency([
          {3, gen_simple_expr()},
          {1, gen_matched_expr(GrammarTree.decr_depth(state))}
        ])
      end

    StreamData.bind(left_gen, fn left ->
      StreamData.bind(StreamData.member_of(@binary_ops), fn {_, op} ->
        StreamData.constant({:dot_op_identifier, left, op})
      end)
    end)
  end

  # Generate a capture integer: &1, &10
  defp gen_capture_int do
    StreamData.integer(1..10)
    |> StreamData.map(fn n -> {:capture_int, n} end)
  end

  # Generate a paren_identifier: foo
  defp gen_paren_identifier do
    StreamData.member_of(@identifiers)
    |> StreamData.map(fn atom -> {:paren_identifier, atom} end)
  end

  # Generate a dot_call target: expr.
  defp gen_dot_call_target(_state) do
    # Use simple expression for the target to avoid deep nesting
    gen_simple_expr()
    |> StreamData.map(fn expr -> {:dot_call, expr} end)
  end

  # ===========================================================================
  # Generator: fn_single
  # ===========================================================================

  # Generate a single-clause fn expression: fn pattern -> body end
  # Per grammar line 276: access_expr -> fn_eoe stab_eoe 'end'
  # fn_eoe (grammar lines 335-336):
  #   fn_eoe -> 'fn'        (inline fn)
  #   fn_eoe -> 'fn' eoe    (fn followed by eol/semi/eol+semi)
  defp gen_fn_single(state) do
    child_state = GrammarTree.decr_depth(state)

    # Generate fn_eoe (what comes after 'fn')
    fn_eoe_gen = gen_fn_eoe()

    # Generate trailing eoe for stab_eoe
    # Per grammar lines 331-333: eoe -> eol | ';' | eol ';'
    trailing_eoe_gen =
      StreamData.frequency([
        {6, StreamData.constant(:none)},
        {3, StreamData.constant(:eol)},
        {1, StreamData.constant(:semi)},
        {1, StreamData.constant(:eol_semi)}
      ])

    StreamData.bind(fn_eoe_gen, fn fn_eoe ->
      StreamData.bind(gen_stab_clause(child_state), fn clause ->
        StreamData.bind(trailing_eoe_gen, fn trailing_eoe ->
          StreamData.constant({:fn_single, fn_eoe, {:stab_eoe, [clause], trailing_eoe}})
        end)
      end)
    end)
  end

  # Generate a multi-clause fn expression: fn clause1; clause2; ... end
  # Per grammar line 276: access_expr -> fn_eoe stab_eoe 'end'
  # fn_eoe (grammar lines 335-336):
  #   fn_eoe -> 'fn'        (inline fn)
  #   fn_eoe -> 'fn' eoe    (fn followed by eol/semi/eol+semi)
  defp gen_fn_multi(state) do
    child_state = GrammarTree.decr_depth(state)

    # Generate fn_eoe (what comes after 'fn')
    fn_eoe_gen = gen_fn_eoe()

    # Generate trailing eoe for stab_eoe
    # Per grammar lines 331-333: eoe -> eol | ';' | eol ';'
    trailing_eoe_gen =
      StreamData.frequency([
        {6, StreamData.constant(:none)},
        {3, StreamData.constant(:eol)},
        {1, StreamData.constant(:semi)},
        {1, StreamData.constant(:eol_semi)}
      ])

    # Generate 2-4 clauses
    StreamData.bind(fn_eoe_gen, fn fn_eoe ->
      StreamData.bind(StreamData.integer(2..4), fn count ->
        StreamData.bind(gen_stab_clause_list(child_state, count), fn clauses ->
          StreamData.bind(trailing_eoe_gen, fn trailing_eoe ->
            StreamData.constant({:fn_multi, fn_eoe, {:stab_eoe, clauses, trailing_eoe}})
          end)
        end)
      end)
    end)
  end

  # Generate a list of stab clauses for fn_multi
  defp gen_stab_clause_list(_state, 0), do: StreamData.constant([])

  defp gen_stab_clause_list(state, count) when count > 0 do
    StreamData.bind(gen_stab_clause_varied(state), fn clause ->
      StreamData.bind(gen_stab_clause_list(state, count - 1), fn rest ->
        StreamData.constant([clause | rest])
      end)
    end)
  end

  # Generate a stab clause with varied patterns (literals, identifiers, atoms)
  # for better pattern matching diversity in fn_multi
  defp gen_stab_clause_varied(_state) do
    # Generate pattern - use varied patterns for multi-clause fns
    pattern_gen =
      StreamData.frequency([
        {3, gen_single_pattern()},
        {2, gen_single_literal_pattern()},
        {1, gen_single_atom_pattern()}
      ])

    # No guards for simplicity in multi-clause (guards added separately)
    guard_gen = StreamData.constant(nil)

    # Generate stab_op_eol (newlines after ->)
    stab_op_eol_gen = gen_stab_op_eol()

    # Generate simple body
    body_gen = gen_simple_expr()

    StreamData.bind(pattern_gen, fn pattern ->
      StreamData.bind(guard_gen, fn guard ->
        StreamData.bind(stab_op_eol_gen, fn stab_op_eol ->
          StreamData.bind(body_gen, fn body ->
            StreamData.constant({:stab_clause, pattern, guard, stab_op_eol, body})
          end)
        end)
      end)
    end)
  end

  # Generate a single literal pattern for fn clauses: {:single, literal}
  defp gen_single_literal_pattern do
    StreamData.frequency([
      {3,
       StreamData.integer(0..10)
       |> StreamData.map(fn n -> {:single, {:int, n, :dec, Integer.to_charlist(n)}} end)},
      {2, StreamData.member_of(@atoms) |> StreamData.map(fn a -> {:single, {:atom_lit, a}} end)}
    ])
  end

  # Generate a single atom pattern: {:single, {:atom_lit, atom}}
  defp gen_single_atom_pattern do
    StreamData.member_of(@atoms)
    |> StreamData.map(fn atom -> {:single, {:atom_lit, atom}} end)
  end

  # ===========================================================================
  # Generator: paren_stab (parenthesized stab clauses)
  # ===========================================================================

  # Per grammar lines 277-279:
  #   access_expr -> open_paren stab_eoe ')' : build_paren_stab('$1', '$2', '$3').
  #   access_expr -> open_paren ';' stab_eoe ')' : build_paren_stab('$1', '$3', '$4').
  #   access_expr -> open_paren ';' close_paren : build_paren_stab('$1', [], '$3').
  #
  # paren_stab forms represent anonymous function-like constructs in parentheses:
  #   (x -> x + 1)     - basic paren_stab
  #   (x, y -> x + y)  - with multiple args
  #   (; x -> x)       - with leading semicolon
  #   (;)              - empty with semicolon

  # Generate basic paren_stab: (clause) or (clause1; clause2)
  # Grammar: open_paren stab_eoe ')' : build_paren_stab('$1', '$2', '$3')
  defp gen_paren_stab(state) do
    child_state = GrammarTree.decr_depth(state)

    # Generate trailing eoe for stab_eoe
    # Per grammar lines 331-333: eoe -> eol | ';' | eol ';'
    trailing_eoe_gen =
      StreamData.frequency([
        {6, StreamData.constant(:none)},
        {3, StreamData.constant(:eol)},
        {1, StreamData.constant(:semi)},
        {1, StreamData.constant(:eol_semi)}
      ])

    # Generate 1-2 stab clauses
    StreamData.bind(StreamData.integer(1..2), fn count ->
      StreamData.bind(gen_stab_clause_list(child_state, count), fn clauses ->
        StreamData.bind(trailing_eoe_gen, fn trailing_eoe ->
          # Also randomly produce the variant where open_paren had a newline
          StreamData.frequency([
            {8, StreamData.constant({:paren_stab, {:stab_eoe, clauses, trailing_eoe}})},
            {2, StreamData.constant({:paren_stab_nl, {:stab_eoe, clauses, trailing_eoe}, 1})}
          ])
        end)
      end)
    end)
  end

  # Generate paren_stab with leading semicolon: (; clause) or (; clause1; clause2)
  # Grammar: open_paren ';' stab_eoe ')' : build_paren_stab('$1', '$3', '$4')
  defp gen_paren_stab_semi(state) do
    child_state = GrammarTree.decr_depth(state)

    # Generate trailing eoe for stab_eoe
    # Per grammar lines 331-333: eoe -> eol | ';' | eol ';'
    trailing_eoe_gen =
      StreamData.frequency([
        {6, StreamData.constant(:none)},
        {3, StreamData.constant(:eol)},
        {1, StreamData.constant(:semi)},
        {1, StreamData.constant(:eol_semi)}
      ])

    # Generate 1-2 stab clauses
    StreamData.bind(StreamData.integer(1..2), fn count ->
      StreamData.bind(gen_stab_clause_list(child_state, count), fn clauses ->
        StreamData.bind(trailing_eoe_gen, fn trailing_eoe ->
          # Also allow open_paren newline variant for semi form
          StreamData.frequency([
            {9, StreamData.constant({:paren_stab_semi, {:stab_eoe, clauses, trailing_eoe}})},
            {1, StreamData.constant({:paren_stab_nl_semi, {:stab_eoe, clauses, trailing_eoe}, 1})}
          ])
        end)
      end)
    end)
  end

  # Generate empty paren_stab with semicolon: (;)
  # Grammar: open_paren ';' close_paren : build_paren_stab('$1', [], '$3')
  defp gen_paren_stab_empty do
    StreamData.constant({:paren_stab_empty})
  end

  # ===========================================================================
  # Generator: call_do (if/unless/case with do blocks)
  # ===========================================================================

  # Identifiers that can precede do blocks (do_identifier in grammar)
  # These are tokenized as :do_identifier by the lexer
  @do_identifiers ~w(if unless case for try receive with cond)a

  # Generate a call with do block: if cond do body end
  # Rule 3: dot_do_identifier do_block
  # Per grammar lines 260-261 and 488-489:
  #   dot_do_identifier -> do_identifier
  #   dot_do_identifier -> matched_expr dot_op do_identifier
  defp gen_call_do(state) do
    child_state = GrammarTree.decr_depth(state)

    StreamData.frequency([
      {4, gen_if_unless(child_state)},
      {2, gen_case(child_state)},
      # Dotted do_identifier variant: Mod.if true do end (rare)
      {1, gen_dotted_do_identifier_call(child_state)}
    ])
  end

  # Generate if/unless with do block (simple identifier target)
  defp gen_if_unless(state) do
    StreamData.bind(StreamData.member_of(@do_identifiers), fn name ->
      StreamData.bind(gen_do_condition(), fn cond_expr ->
        StreamData.bind(gen_do_block(state), fn do_block ->
          StreamData.constant({:call_do, {:identifier, name}, [cond_expr], do_block})
        end)
      end)
    end)
  end

  # Generate case expression with stab clauses
  defp gen_case(state) do
    StreamData.bind(gen_simple_expr(), fn match_expr ->
      StreamData.bind(gen_case_block(state), fn do_block ->
        StreamData.constant({:call_do, {:identifier, :case}, [match_expr], do_block})
      end)
    end)
  end

  # Generate dotted do_identifier call: Mod.if true do end, expr.unless cond do end
  # Per grammar: matched_expr dot_op do_identifier
  defp gen_dotted_do_identifier_call(state) do
    left_gen =
      if state.budget.depth <= 1 do
        StreamData.frequency([
          {2, gen_alias()},
          {1, gen_identifier()}
        ])
      else
        StreamData.frequency([
          {3, gen_alias()},
          {2, gen_identifier()},
          {1, gen_matched_expr(GrammarTree.decr_depth(state))}
        ])
      end

    StreamData.bind(left_gen, fn left ->
      StreamData.bind(StreamData.member_of(@do_identifiers), fn name ->
        StreamData.bind(gen_do_condition(), fn cond_expr ->
          StreamData.bind(gen_do_block(state), fn do_block ->
            StreamData.constant(
              {:call_do, {:dot_do_identifier, left, name}, [cond_expr], do_block}
            )
          end)
        end)
      end)
    end)
  end

  # Generate case block with stab clauses
  # Returns {:do_block, do_eoe, {:stab_eoe, clauses, trailing_eoe}, extras}
  defp gen_case_block(_state) do
    # Generate trailing eoe for stab_eoe
    # Per grammar lines 331-333: eoe -> eol | ';' | eol ';'
    trailing_eoe_gen =
      StreamData.frequency([
        {6, StreamData.constant(:none)},
        {3, StreamData.constant(:eol)},
        {1, StreamData.constant(:semi)},
        {1, StreamData.constant(:eol_semi)}
      ])

    do_eoe_gen = gen_do_eoe()

    # Generate 2-3 stab clauses for case
    StreamData.bind(StreamData.integer(2..3), fn count ->
      StreamData.bind(gen_case_clause_list(count), fn clauses ->
        StreamData.bind(trailing_eoe_gen, fn trailing_eoe ->
          StreamData.bind(do_eoe_gen, fn do_eoe ->
            StreamData.constant({:do_block, do_eoe, {:stab_eoe, clauses, trailing_eoe}, []})
          end)
        end)
      end)
    end)
  end

  # Generate a list of case clauses
  defp gen_case_clause_list(0), do: StreamData.constant([])

  defp gen_case_clause_list(count) when count > 0 do
    StreamData.bind(gen_case_clause(), fn clause ->
      StreamData.bind(gen_case_clause_list(count - 1), fn rest ->
        StreamData.constant([clause | rest])
      end)
    end)
  end

  # Generate a single case clause (stab clause with pattern)
  defp gen_case_clause do
    pattern_gen =
      StreamData.frequency([
        {3,
         StreamData.member_of(@atoms) |> StreamData.map(fn a -> {:single, {:atom_lit, a}} end)},
        {2,
         StreamData.member_of(@identifiers)
         |> StreamData.map(fn i -> {:single, {:identifier, i}} end)},
        {1, StreamData.constant({:single, {:identifier, :_}})}
      ])

    # Generate stab_op_eol (newlines after ->)
    stab_op_eol_gen = gen_stab_op_eol()

    body_gen = gen_simple_expr()

    StreamData.bind(pattern_gen, fn pattern ->
      StreamData.bind(stab_op_eol_gen, fn stab_op_eol ->
        StreamData.bind(body_gen, fn body ->
          StreamData.constant({:stab_clause, pattern, nil, stab_op_eol, body})
        end)
      end)
    end)
  end

  # Generate condition for if/unless (simple expressions)
  defp gen_do_condition do
    StreamData.frequency([
      {3, StreamData.member_of([true, false]) |> StreamData.map(&{:bool_lit, &1})},
      {2, StreamData.member_of(@identifiers) |> StreamData.map(&{:identifier, &1})}
    ])
  end

  # Generate do block: {:do_block, body, extras}
  # Per grammar lines 322-329, do_block has 4 cases:
  #   1. do_eoe 'end'                  -> empty body, no extras
  #   2. do_eoe stab_eoe 'end'         -> stab clauses, no extras
  #   3. do_eoe block_list 'end'       -> empty body, with extras
  #   4. do_eoe stab_eoe block_list 'end' -> stab clauses, with extras
  #
  # Per grammar lines 338-339:
  #   do_eoe -> 'do'         (inline: do expr end)
  #   do_eoe -> 'do' eoe     (with eoe: do\n expr end or do; expr end)
  #
  # The do_eoe field can be:
  #   :none - inline form, no eol after 'do'
  #   :eol  - newline after 'do' (most common)
  #   :semi - semicolon after 'do'
  defp gen_do_block(state) do
    body_gen = gen_do_body(state)
    extras_gen = gen_block_extras(state)
    do_eoe_gen = gen_do_eoe()

    StreamData.bind(do_eoe_gen, fn do_eoe ->
      StreamData.bind(body_gen, fn body ->
        StreamData.bind(extras_gen, fn extras ->
          StreamData.constant({:do_block, do_eoe, body, extras})
        end)
      end)
    end)
  end

  # Generate fn_eoe variant (whether 'fn' is followed by eol, semicolon, or nothing)
  # Per grammar lines 335-336:
  #   fn_eoe -> 'fn'      (inline)
  #   fn_eoe -> 'fn' eoe  (eoe = eol | ';' | eol ';')
  defp gen_fn_eoe do
    StreamData.frequency([
      # Most common: inline fn with no eoe (fn x -> x end)
      {6, StreamData.constant(:none)},
      # Newline after 'fn' (fn\n x -> x end)
      {3, StreamData.constant(:eol)},
      # Semicolon after 'fn' (fn; x -> x end) - rare but valid
      {1, StreamData.constant(:semi)},
      # eol followed by semicolon (fn\n; x -> x end) - very rare
      {1, StreamData.constant(:eol_semi)}
    ])
  end

  # Generate do_eoe variant (whether 'do' is followed by eol, semicolon, or nothing)
  # Per grammar lines 338-339:
  #   do_eoe -> 'do'      (inline)
  #   do_eoe -> 'do' eoe  (eoe = eol | ';' | eol ';')
  defp gen_do_eoe do
    StreamData.frequency([
      # Most common: newline after 'do'
      {7, StreamData.constant(:eol)},
      # Inline form: do :expr end (no eol, for simple single expressions)
      {2, StreamData.constant(:none)},
      # Semicolon form: do; expr end (rare but valid)
      {1, StreamData.constant(:semi)},
      # eol followed by semicolon (do\n; expr end) - very rare
      {1, StreamData.constant(:eol_semi)}
    ])
  end

  # Generate body for do block - can be:
  # - Empty: [] (grammar case 1 and 3)
  # - Simple expressions: [expr1, expr2, ...] (common case)
  # - Stab clauses: [{:stab_clause, ...}, ...] (grammar case 2 and 4)
  defp gen_do_body(state) do
    child_state = GrammarTree.decr_depth(state)

    StreamData.frequency([
      # Empty body (grammar: do_eoe 'end')
      {1, StreamData.constant([])},
      # Simple expressions (1-2 expressions)
      {6, gen_do_body_exprs(child_state)},
      # Stab clauses (grammar: do_eoe stab_eoe 'end')
      {3, gen_do_body_stab(child_state)}
    ])
  end

  # Generate do body with simple expressions (1-2 expressions)
  defp gen_do_body_exprs(_state) do
    StreamData.bind(StreamData.integer(1..2), fn count ->
      gen_simple_expr_list(count)
    end)
  end

  # Generate do body with stab clauses (for case/receive/cond/try)
  # This produces bodies like: x -> y; a -> b
  # Returns {:stab_eoe, clauses, trailing_eoe}
  defp gen_do_body_stab(state) do
    # Generate trailing eoe for stab_eoe
    # Per grammar lines 331-333: eoe -> eol | ';' | eol ';'
    trailing_eoe_gen =
      StreamData.frequency([
        {6, StreamData.constant(:none)},
        {3, StreamData.constant(:eol)},
        {1, StreamData.constant(:semi)},
        {1, StreamData.constant(:eol_semi)}
      ])

    StreamData.bind(StreamData.integer(1..3), fn count ->
      StreamData.bind(gen_stab_clause_list(state, count), fn clauses ->
        StreamData.bind(trailing_eoe_gen, fn trailing_eoe ->
          StreamData.constant({:stab_eoe, clauses, trailing_eoe})
        end)
      end)
    end)
  end

  # Generate block extras (block_list from grammar lines 368-374)
  # block_list -> block_item | block_item block_list
  # block_item -> block_eoe stab_eoe | block_eoe
  #
  # block_identifier tokens: else, rescue, catch, after
  defp gen_block_extras(state) do
    child_state = GrammarTree.decr_depth(state)

    StreamData.frequency([
      # No extras (most common)
      {5, StreamData.constant([])},
      # Just else (for if/unless)
      {3, gen_else_block(child_state)},
      # Just rescue (for try)
      {1, gen_rescue_block(child_state)},
      # Just catch (for try)
      {1, gen_catch_block(child_state)},
      # Just after (for try/receive)
      {1, gen_after_block(child_state)},
      # rescue + after combo (for try)
      {1, gen_rescue_after_combo(child_state)}
    ])
  end

  # Generate block_eoe variant (whether block_identifier is followed by eol, semicolon, or nothing)
  # Per grammar lines 341-342:
  #   block_eoe -> block_identifier       (inline)
  #   block_eoe -> block_identifier eoe   (eoe = eol | ';' | eol ';')
  defp gen_block_eoe do
    StreamData.frequency([
      # Most common: newline after block_identifier (else\n body)
      {7, StreamData.constant(:eol)},
      # Inline form: else body (no eol)
      {2, StreamData.constant(:none)},
      # Semicolon form: else; body (rare but valid)
      {1, StreamData.constant(:semi)},
      # eol followed by semicolon (else\n; body) - very rare
      {1, StreamData.constant(:eol_semi)}
    ])
  end

  # Generate else block: [{:block_item, :else, block_eoe, body}]
  # Per grammar lines 368-371:
  #   block_item -> block_eoe stab_eoe   (with stab clauses)
  #   block_item -> block_eoe            (empty body)
  # else can use expressions or stab clauses (grammar allows both)
  defp gen_else_block(state) do
    block_eoe_gen = gen_block_eoe()

    StreamData.bind(block_eoe_gen, fn block_eoe ->
      StreamData.frequency([
        # Empty else using [] (block_eoe only) - grammar line 370
        {1, StreamData.constant([{:block_item, :else, block_eoe, []}])},
        # Empty else using parser-style {:__block__, [], []} for literal parity
        {1, StreamData.constant([{:block_item, :else, block_eoe, {:__block__, [], []}}])},
        # else with expression body (common case)
        {4, gen_block_item_body(state, :else, block_eoe)},
        # else with stab clauses (grammar line 368: block_eoe stab_eoe)
        # Valid but uncommon: else x -> y; z -> w
        {1, gen_block_item_stab(state, :else, block_eoe)}
      ])
    end)
  end

  # Generate rescue block: [{:block_item, :rescue, block_eoe, body}]
  # rescue uses stab clauses: rescue e -> handle(e)
  defp gen_rescue_block(state) do
    block_eoe_gen = gen_block_eoe()

    StreamData.bind(block_eoe_gen, fn block_eoe ->
      StreamData.frequency([
        # Empty rescue using []
        {1, StreamData.constant([{:block_item, :rescue, block_eoe, []}])},
        # Empty rescue using parser-style {:__block__, [], []} for literal parity
        {1, StreamData.constant([{:block_item, :rescue, block_eoe, {:__block__, [], []}}])},
        # rescue with stab clauses
        {4, gen_block_item_stab(state, :rescue, block_eoe)}
      ])
    end)
  end

  # Generate catch block: [{:block_item, :catch, block_eoe, body}]
  # catch uses stab clauses: catch :throw, value -> handle(value)
  defp gen_catch_block(state) do
    block_eoe_gen = gen_block_eoe()

    StreamData.bind(block_eoe_gen, fn block_eoe ->
      StreamData.frequency([
        # Empty catch using []
        {1, StreamData.constant([{:block_item, :catch, block_eoe, []}])},
        # Empty catch using parser-style {:__block__, [], []} for literal parity
        {1, StreamData.constant([{:block_item, :catch, block_eoe, {:__block__, [], []}}])},
        # catch with stab clauses
        {4, gen_block_item_stab(state, :catch, block_eoe)}
      ])
    end)
  end

  # Generate after block: [{:block_item, :after, block_eoe, body}]
  # after uses expressions: after cleanup()
  defp gen_after_block(state) do
    block_eoe_gen = gen_block_eoe()

    StreamData.bind(block_eoe_gen, fn block_eoe ->
      StreamData.frequency([
        # Empty after using []
        {1, StreamData.constant([{:block_item, :after, block_eoe, []}])},
        # Empty after using parser-style {:__block__, [], []} for literal parity
        {1, StreamData.constant([{:block_item, :after, block_eoe, {:__block__, [], []}}])},
        # after with body
        {4, gen_block_item_body(state, :after, block_eoe)}
      ])
    end)
  end

  # Generate rescue + after combo for try blocks
  defp gen_rescue_after_combo(state) do
    StreamData.bind(gen_rescue_block(state), fn rescue_items ->
      StreamData.bind(gen_after_block(state), fn after_items ->
        StreamData.constant(rescue_items ++ after_items)
      end)
    end)
  end

  # Generate block item with simple expression body
  # Returns [{:block_item, block_type, block_eoe, body}]
  defp gen_block_item_body(_state, block_type, block_eoe) do
    StreamData.bind(StreamData.integer(1..2), fn count ->
      gen_simple_expr_list(count)
    end)
    |> StreamData.map(fn body -> [{:block_item, block_type, block_eoe, body}] end)
  end

  # Generate block item with stab clause body
  # Returns [{:block_item, block_type, block_eoe, {:stab_eoe, clauses, trailing_eoe}}]
  defp gen_block_item_stab(state, block_type, block_eoe) do
    # Generate trailing eoe for stab_eoe
    # Per grammar lines 331-333: eoe -> eol | ';' | eol ';'
    trailing_eoe_gen =
      StreamData.frequency([
        {6, StreamData.constant(:none)},
        {3, StreamData.constant(:eol)},
        {1, StreamData.constant(:semi)},
        {1, StreamData.constant(:eol_semi)}
      ])

    StreamData.bind(StreamData.integer(1..2), fn count ->
      StreamData.bind(gen_stab_clause_list(state, count), fn clauses ->
        StreamData.bind(trailing_eoe_gen, fn trailing_eoe ->
          StreamData.constant([
            {:block_item, block_type, block_eoe, {:stab_eoe, clauses, trailing_eoe}}
          ])
        end)
      end)
    end)
  end

  # ===========================================================================
  # Generator: block_expr (all 5 rules from elixir_parser.yrl lines 181-185)
  # ===========================================================================

  # Generate block_expr according to all 5 grammar rules:
  # 1. dot_call_identifier call_args_parens do_block          -> gen_block_parens
  # 2. dot_call_identifier call_args_parens call_args_parens do_block -> gen_block_parens_nested
  # 3. dot_do_identifier do_block                             -> gen_call_do (existing)
  # 4. dot_op_identifier call_args_no_parens_all do_block     -> gen_block_no_parens_op
  # 5. dot_identifier call_args_no_parens_all do_block        -> gen_block_no_parens
  defp gen_block_expr(state) do
    child_state = GrammarTree.decr_depth(state)

    StreamData.frequency([
      # Rule 1: paren call with do block - foo() do end
      {3, gen_block_parens(child_state)},
      # Rule 2: nested paren calls - foo()() do end (rare)
      {1, gen_block_parens_nested(child_state)},
      # Rule 3: do_identifier do block - if do end (most common)
      {5, gen_call_do(state)},
      # Rule 4: op_identifier with args + do block - .+ 1 do end (very rare)
      {1, gen_block_no_parens_op(child_state)},
      # Rule 5: identifier with args + do block - foo 1 do end
      {3, gen_block_no_parens(child_state)}
    ])
  end

  # Rule 1: dot_call_identifier call_args_parens do_block
  # Examples: foo() do end, Mod.func() do end, expr.() do end
  defp gen_block_parens(state) do
    child_state = GrammarTree.decr_depth(state)

    # dot_call_identifier is either:
    # - paren_identifier (simple): foo
    # - matched_expr.paren_identifier: Mod.foo, expr.foo
    # - matched_expr dot_call_op: expr.
    target_gen =
      StreamData.frequency([
        {4, gen_paren_identifier()},
        {2, gen_dot_paren_identifier(child_state)},
        {1, gen_dot_call_target(child_state)}
      ])

    # Use full call_args_parens grammar
    args_gen = gen_call_args_parens(child_state)

    StreamData.bind(target_gen, fn target ->
      StreamData.bind(args_gen, fn args ->
        StreamData.bind(gen_do_block(child_state), fn do_block ->
          StreamData.constant({:block_parens, target, args, do_block})
        end)
      end)
    end)
  end

  # Generate dot_paren_identifier: matched_expr.paren_identifier
  # Per grammar: dot_paren_identifier -> paren_identifier | matched_expr dot_op paren_identifier
  defp gen_dot_paren_identifier(state) do
    left_gen =
      if state.budget.depth <= 1 do
        StreamData.frequency([
          {2, gen_alias()},
          {1, gen_identifier()}
        ])
      else
        StreamData.frequency([
          {3, gen_alias()},
          {2, gen_identifier()},
          {1, gen_matched_expr(GrammarTree.decr_depth(state))}
        ])
      end

    StreamData.bind(left_gen, fn left ->
      StreamData.bind(StreamData.member_of(@identifiers), fn right_name ->
        StreamData.constant({:dot_paren_identifier, left, right_name})
      end)
    end)
  end

  # Rule 2: dot_call_identifier call_args_parens call_args_parens do_block
  # Examples: foo()() do end, Mod.func()() do end (higher-order function calls)
  defp gen_block_parens_nested(state) do
    child_state = GrammarTree.decr_depth(state)

    # Similar to Rule 1 but with two sets of args
    target_gen =
      StreamData.frequency([
        {4, gen_paren_identifier()},
        {2, gen_dot_paren_identifier(child_state)}
      ])

    # Use full call_args_parens grammar
    args1_gen = gen_call_args_parens(child_state)
    args2_gen = gen_call_args_parens(child_state)

    StreamData.bind(target_gen, fn target ->
      StreamData.bind(args1_gen, fn args1 ->
        StreamData.bind(args2_gen, fn args2 ->
          StreamData.bind(gen_do_block(child_state), fn do_block ->
            StreamData.constant({:block_parens_nested, target, args1, args2, do_block})
          end)
        end)
      end)
    end)
  end

  # Rule 4: dot_op_identifier call_args_no_parens_all do_block
  # Examples: .+ 1 do end, expr.* a do end (operator-as-function with do block)
  defp gen_block_no_parens_op(state) do
    child_state = GrammarTree.decr_depth(state)

    # dot_op_identifier: op_identifier | matched_expr dot_op op_identifier
    target_gen =
      StreamData.frequency([
        {2,
         StreamData.bind(StreamData.member_of(@binary_ops), fn {_, op} ->
           StreamData.constant({:op_identifier, op})
         end)},
        {1, gen_dot_op_identifier_for_call(child_state)}
      ])

    # call_args_no_parens_all: one | ambig | many
    # Per grammar lines 508-510
    args_gen = gen_call_args_no_parens_all(child_state)

    StreamData.bind(target_gen, fn target ->
      StreamData.bind(args_gen, fn args ->
        StreamData.bind(gen_do_block(child_state), fn do_block ->
          StreamData.constant({:block_no_parens_op, target, args, do_block})
        end)
      end)
    end)
  end

  # Rule 5: dot_identifier call_args_no_parens_all do_block
  # Examples: foo 1 do end, Mod.func arg do end
  defp gen_block_no_parens(state) do
    child_state = GrammarTree.decr_depth(state)

    # dot_identifier: identifier | matched_expr dot_op identifier
    target_gen =
      StreamData.frequency([
        {3, StreamData.member_of(@identifiers) |> StreamData.map(&{:identifier, &1})},
        {2, gen_dot_identifier_for_call(child_state)}
      ])

    # call_args_no_parens_all: one | ambig | many
    # Per grammar lines 508-510
    args_gen = gen_call_args_no_parens_all(child_state)

    StreamData.bind(target_gen, fn target ->
      StreamData.bind(args_gen, fn args ->
        StreamData.bind(gen_do_block(child_state), fn do_block ->
          StreamData.constant({:block_no_parens, target, args, do_block})
        end)
      end)
    end)
  end

  # ===========================================================================
  # Generator: stab_op_eol (-> operator with optional newlines)
  # ===========================================================================

  # Per grammar lines 460-461:
  #   stab_op_eol -> stab_op : '$1'.
  #   stab_op_eol -> stab_op eol : next_is_eol('$1', '$2').
  #
  # The stab operator -> can be followed by 0 or more newlines. In the grammar,
  # `eol` is a single token that can represent multiple consecutive newlines
  # (the count is stored in the token's location tuple). The `next_is_eol` action
  # copies the newline count from the eol token to the operator token for AST
  # metadata purposes.
  #
  # We generate 0 (inline), 1 (single newline), or 2-3 (multiple newlines) to test
  # all cases. Multiple newlines are tokenized as a single eol token with count > 1.
  defp gen_stab_op_eol do
    StreamData.frequency([
      # No newline after -> (most common, inline body)
      {6, StreamData.constant(0)},
      # One newline after ->
      {3, StreamData.constant(1)},
      # Multiple newlines after -> (rare, single eol token with count > 1)
      {1, StreamData.integer(2..3)}
    ])
  end

  # Generate a stab clause: pattern -> body (or pattern when guard -> body)
  # Per grammar lines 350-363 (stab_expr productions)
  #
  # Structure: {:stab_clause, pattern, guard, stab_op_eol, body}
  # - pattern: :empty, :empty_parens, {:single, expr}, {:many, [expr]}, {:many_parens, [expr]}
  # - guard: nil or guard expression
  # - stab_op_eol: newlines after -> (0 = inline, 1+ = newlines)
  # - body: expression or :empty_body (for stab_op_eol_and_expr -> stab_op_eol)
  #
  # Grammar coverage:
  # - stab_expr -> stab_op_eol_and_expr: :empty pattern, no guard
  # - stab_expr -> empty_paren stab_op_eol_and_expr: :empty_parens pattern
  # - stab_expr -> empty_paren when_op expr stab_op_eol_and_expr: :empty_parens with guard
  # - stab_expr -> call_args_no_parens_all stab_op_eol_and_expr: {:single, _} or {:many, _}
  # - stab_expr -> stab_parens_many stab_op_eol_and_expr: {:many_parens, _}
  # - stab_expr -> stab_parens_many when_op expr stab_op_eol_and_expr: {:many_parens, _} with guard
  # - stab_op_eol_and_expr -> stab_op_eol: empty body (:empty_body)
  defp gen_stab_clause(state) do
    # Generate pattern
    pattern_gen = gen_pattern()

    # Generate guard (nil most of the time, occasionally a guard expression)
    guard_gen = gen_optional_guard(state)

    # Generate stab_op_eol (newlines after ->)
    stab_op_eol_gen = gen_stab_op_eol()

    # Generate body - can be expression or :empty_body
    # Per grammar line 366: stab_op_eol_and_expr -> stab_op_eol (empty, warns)
    body_gen =
      StreamData.frequency([
        {19,
         if state.budget.depth <= 1 do
           gen_simple_expr()
         else
           gen_expr(state)
         end},
        # Empty body (rare, generates warning): -> (no body)
        {1, StreamData.constant(:empty_body)}
      ])

    # Additionally, the grammar allows a stab_expr that is just an expr (no ->),
    # i.e., stab_expr -> expr : '$1'. Model that by occasionally emitting a bare
    # expr as a stab_clause-like tuple with :bare_expr marker so the compiler can
    # handle it.
    bare_expr_gen =
      if state.budget.depth <= 1 do
        gen_simple_expr()
      else
        gen_expr(state)
      end

    StreamData.frequency([
      {4,
       StreamData.bind(bare_expr_gen, fn expr ->
         StreamData.constant({:stab_expr_bare, expr})
       end)},
      {6,
       StreamData.bind(pattern_gen, fn pattern ->
         StreamData.bind(guard_gen, fn guard ->
           StreamData.bind(stab_op_eol_gen, fn stab_op_eol ->
             StreamData.bind(body_gen, fn body ->
               StreamData.constant({:stab_clause, pattern, guard, stab_op_eol, body})
             end)
           end)
         end)
       end)}
    ])
  end

  # Generate a pattern: :empty, :empty_parens, {:single, expr}, {:many, [expr]}, {:many_parens, [expr]}
  # Per grammar stab_expr rules (lines 350-363):
  #   - :empty corresponds to stab_op_eol_and_expr (no pattern, just -> body)
  #   - :empty_parens corresponds to empty_paren stab_op_eol_and_expr: () -> body
  #   - {:single, expr} corresponds to call_args_no_parens_one (single arg)
  #   - {:many, [exprs]} corresponds to call_args_no_parens_all (multiple args without parens)
  #   - {:many_parens, [exprs]} corresponds to stab_parens_many (multiple args with parens)
  defp gen_pattern do
    StreamData.frequency([
      {2, StreamData.constant(:empty)},
      {1, StreamData.constant(:empty_parens)},
      {4, gen_single_pattern()},
      {2, gen_many_pattern()},
      {2, gen_many_parens_pattern()}
    ])
  end

  # Generate a single pattern for fn: {:single, expr}
  defp gen_single_pattern do
    # Use identifiers for patterns (most common)
    StreamData.member_of(@identifiers)
    |> StreamData.map(fn atom -> {:single, {:identifier, atom}} end)
  end

  # Generate multiple patterns without parens: {:many, [expr1, expr2, ...]}
  # Per grammar: call_args_no_parens_all stab_op_eol_and_expr
  defp gen_many_pattern do
    # Generate 2-4 pattern expressions (identifiers only for simplicity)
    StreamData.bind(StreamData.integer(2..4), fn count ->
      gen_pattern_identifier_list(count)
    end)
    |> StreamData.map(fn exprs -> {:many, exprs} end)
  end

  # Generate multiple patterns with parens: {:many_parens, [expr1, expr2, ...]}
  # Per grammar lines 528-529 (stab_parens_many):
  #   stab_parens_many -> open_paren call_args_no_parens_kw close_paren : {'$1', ['$2'], '$3'}.
  #   stab_parens_many -> open_paren call_args_no_parens_many close_paren : {'$1', '$2', '$3'}.
  #
  # call_args_no_parens_kw = keyword args only: (key: val, key2: val2)
  # call_args_no_parens_many = positional args, optionally with trailing kw:
  #   - (a, b) - matched_expr comma matched_expr
  #   - (a, b, key: val) - matched_expr comma ... comma kw
  #
  # We generate three variants to cover both grammar productions:
  defp gen_many_parens_pattern do
    StreamData.frequency([
      # Variant 1: Positional-only (call_args_no_parens_many without trailing kw)
      # Examples: (a, b), (x, y, z)
      {4,
       StreamData.bind(StreamData.integer(2..4), fn count ->
         gen_pattern_identifier_list(count)
       end)
       |> StreamData.map(fn exprs -> {:many_parens, exprs} end)},

      # Variant 2: Positional with trailing keyword args (call_args_no_parens_many with kw)
      # Examples: (a, key: 1), (x, y, foo: :bar)
      {2,
       StreamData.bind(StreamData.integer(1..3), fn pos_count ->
         StreamData.bind(gen_pattern_identifier_list(pos_count), fn pos ->
           StreamData.bind(gen_call_args_no_parens_kw(), fn kw ->
             StreamData.constant({:many_parens, pos ++ [kw]})
           end)
         end)
       end)},

      # Variant 3: Keyword-only inside parens (call_args_no_parens_kw)
      # Examples: (key: val), (a: 1, b: 2)
      {1,
       StreamData.bind(gen_call_args_no_parens_kw(), fn kw ->
         StreamData.constant({:many_parens, [kw]})
       end)}
    ])
  end

  # Generate a list of unique identifiers for patterns
  defp gen_pattern_identifier_list(count) do
    # Pick `count` distinct identifiers to avoid duplicate patterns
    StreamData.uniq_list_of(
      StreamData.member_of(@identifiers),
      length: count
    )
    |> StreamData.map(fn atoms ->
      Enum.map(atoms, fn atom -> {:identifier, atom} end)
    end)
  end

  # Generate optional guard (nil most of the time)
  defp gen_optional_guard(state) do
    StreamData.frequency([
      {7, StreamData.constant(nil)},
      {3, gen_guard(state)}
    ])
  end

  # Generate a guard expression
  # Guards are restricted: comparisons, type checks, boolean ops
  defp gen_guard(_state) do
    StreamData.frequency([
      {4, gen_guard_comparison()},
      {3, gen_guard_type_check()},
      {2, gen_guard_boolean()}
    ])
  end

  # Generate comparison guard: x > 0, x == :ok, etc.
  defp gen_guard_comparison do
    comp_ops = [{:rel_op, :>}, {:rel_op, :<}, {:rel_op, :>=}, {:rel_op, :<=}, {:comp_op, :==}]

    StreamData.bind(StreamData.member_of(@identifiers), fn var ->
      StreamData.bind(StreamData.member_of(comp_ops), fn {op_kind, op} ->
        StreamData.bind(gen_simple_literal_value(), fn rhs ->
          guard = {:binary_op, {:identifier, var}, {:op_eol, {op_kind, op}, 0}, rhs}
          StreamData.constant(guard)
        end)
      end)
    end)
  end

  # Generate type check guard: is_integer(x), is_atom(x), etc.
  @type_checks ~w(is_integer is_atom is_binary is_list is_map is_nil is_boolean)a

  defp gen_guard_type_check do
    StreamData.bind(StreamData.member_of(@type_checks), fn check ->
      StreamData.bind(StreamData.member_of(@identifiers), fn var ->
        guard = {:call_parens, {:paren_identifier, check}, [{:identifier, var}]}
        StreamData.constant(guard)
      end)
    end)
  end

  # Generate simple boolean guard: x and true, not x
  defp gen_guard_boolean do
    StreamData.bind(StreamData.member_of(@identifiers), fn var ->
      StreamData.frequency([
        {2,
         StreamData.constant(
           {:binary_op, {:identifier, var}, {:op_eol, {:and_op, :and}, 0}, {:bool_lit, true}}
         )},
        {1, StreamData.constant({:unary_op, {:unary_op, :not}, {:identifier, var}})}
      ])
    end)
  end

  # Generate a simple literal value for guard RHS
  defp gen_simple_literal_value do
    StreamData.frequency([
      {3,
       StreamData.integer(0..100)
       |> StreamData.map(fn n -> {:int, n, :dec, Integer.to_charlist(n)} end)},
      {2, StreamData.member_of(@atoms) |> StreamData.map(fn a -> {:atom_lit, a} end)},
      {1, StreamData.constant({:bool_lit, true})},
      {1, StreamData.constant({:bool_lit, false})}
    ])
  end

  # ===========================================================================
  # Generator: eoe (end-of-expression)
  # ===========================================================================

  @doc """
  Generate end-of-expression marker.

  Per grammar rules 331-333:
  - `:eol` - newline only
  - `:semi` - semicolon only
  - `:eol_semi` - newline followed by semicolon
  """
  def gen_eoe do
    StreamData.frequency([
      {7, StreamData.constant(:eol)},
      {2, StreamData.constant(:semi)},
      {1, StreamData.constant(:eol_semi)}
    ])
  end

  # ===========================================================================
  # Category-Aware Generators (per grammar alignment)
  # ===========================================================================

  @doc """
  Generate a matched expression (safe as operands).

  Per grammar lines 155-161, matched expressions include:
  - matched_expr matched_op_expr (binary ops)
  - unary_op_eol matched_expr (unary ops)
  - at_op_eol matched_expr (@foo)
  - capture_op_eol matched_expr (&expr)
  - ellipsis_op matched_expr (...expr)
  - no_parens_one_expr
  - sub_matched_expr

  Additional patterns:
  - Range with step: left..middle//step (ternary_op, grammar lines 739-746)
  - Arrow + no_parens_one: generates warn_pipe pattern (grammar line 209)
  """
  def gen_matched_expr(state) do
    if GrammarTree.budget_exhausted?(state) do
      gen_sub_matched_expr(state)
    else
      StreamData.frequency([
        {4, gen_sub_matched_expr(state)},
        {3, gen_matched_op(state)},
        {2, gen_matched_unary(state)},
        {1, gen_at_op(state)},
        {1, gen_capture_op(state)},
        {1, gen_ellipsis_prefix(state)},
        {1, gen_call_no_parens_one(state)},
        # Range with step: 1..10//2 (ternary_op is only valid after range_op)
        {1, gen_range_step(state)},
        # Arrow + no_parens_one: foo 1 |> bar 2 (warn_pipe pattern)
        {1, gen_matched_op_warn_pipe(state)}
      ])
    end
  end

  @doc """
  Generate an unmatched expression (has trailing do block).

  Per grammar lines 163-171, unmatched expressions include:
  - block_expr (if, unless, case, try, etc. with do blocks)
  - matched_expr unmatched_op_expr (binary op with unmatched right operand)
  - unary_op_eol expr (unary op with any expr)
  - at_op_eol expr (@foo with any expr)
  - capture_op_eol expr (&expr with any expr, including do blocks)
  - ellipsis_op expr (...expr with any expr)
  """
  def gen_unmatched_expr(state) do
    if GrammarTree.budget_exhausted?(state) do
      # Fallback to simple call_do when budget exhausted
      gen_simple_call_do(state)
    else
      StreamData.frequency([
        # All 5 block_expr rules (lines 181-185 in grammar)
        {5, gen_block_expr(state)},
        {3, gen_unmatched_op(state)},
        # Prefix operators with unmatched operand (e.g., &if true do :ok end)
        {1, gen_unmatched_unary(state)},
        {1, gen_unmatched_at_op(state)},
        {1, gen_unmatched_capture_op(state)},
        {1, gen_unmatched_ellipsis(state)}
      ])
    end
  end

  @doc """
  Generate a sub-matched expression (atomic/access expressions).

  Per grammar lines 263-267:
  - sub_matched_expr -> no_parens_zero_expr (bare identifiers)
  - sub_matched_expr -> range_op (nullary ..)
  - sub_matched_expr -> ellipsis_op (nullary ...)
  - sub_matched_expr -> access_expr (literals, fn, calls, etc.)
  """
  def gen_sub_matched_expr(state) do
    StreamData.frequency([
      {9, gen_access_expr(state)},
      {5, gen_no_parens_zero_expr(state)},
      {1, gen_nullary_range()},
      {1, gen_nullary_ellipsis()},
      # Access expression followed by a keyword identifier (invalid in grammar)
      # This models: access_expr kw_identifier -> error_invalid_kw_identifier('$2')
      {1, gen_access_expr_kw_identifier(state)}
    ])
  end

  @doc """
  Generate a no_parens_zero_expr (bare identifier or dotted identifier).

  Per grammar lines 260-261:
  - no_parens_zero_expr -> dot_do_identifier
  - no_parens_zero_expr -> dot_identifier

  And per grammar lines 477-478 (dot_identifier):
  - dot_identifier -> identifier
  - dot_identifier -> matched_expr dot_op identifier

  And per grammar lines 488-489 (dot_do_identifier):
  - dot_do_identifier -> do_identifier
  - dot_do_identifier -> matched_expr dot_op do_identifier

  This is where identifiers belong in the grammar (not access_expr).
  When state has remaining depth, matched_expr variants are included.
  """
  def gen_no_parens_zero_expr(state) do
    child_state = GrammarTree.decr_depth(state)

    if child_state.budget.depth <= 1 do
      # At low depth, only simple forms
      StreamData.frequency([
        {6, gen_identifier()},
        {2, gen_dot_identifier_simple()},
        {1, gen_dot_do_identifier_simple()}
      ])
    else
      # At higher depth, allow matched_expr on left side
      StreamData.frequency([
        # Simple identifier (most common)
        {6, gen_identifier()},
        # Dotted identifier: expr.identifier (e.g., foo.bar, Mod.func, (a+b).foo)
        {2, gen_dot_identifier_full(child_state)},
        # Do identifier: if, unless, case, etc. (as bare or dotted identifiers)
        {1, gen_dot_do_identifier_full(child_state)}
      ])
    end
  end

  @doc """
  Generate a dot_do_identifier (do_identifier or dotted do_identifier).

  Per grammar lines 488-489:
  - dot_do_identifier -> do_identifier (e.g., if, unless, case)
  - dot_do_identifier -> matched_expr dot_op do_identifier (e.g., Foo.if)

  These are identifiers that can precede do blocks when used with arguments.
  When used alone (in no_parens_zero_expr), they're just bare identifiers.
  """
  def gen_dot_do_identifier_simple do
    StreamData.frequency([
      # Simple do_identifier: if, unless, case, etc.
      {4, StreamData.member_of(@do_identifiers) |> StreamData.map(&{:do_identifier, &1})},
      # Dotted do_identifier with simple left: Foo.if, Mod.case, etc.
      {1, gen_dotted_do_identifier_simple()}
    ])
  end

  # Full version with matched_expr on left side
  defp gen_dot_do_identifier_full(state) do
    StreamData.frequency([
      # Simple do_identifier: if, unless, case, etc.
      {4, StreamData.member_of(@do_identifiers) |> StreamData.map(&{:do_identifier, &1})},
      # Dotted do_identifier: (matched_expr).if, (matched_expr).case, etc.
      {1, gen_dotted_do_identifier_full(state)}
    ])
  end

  # Generate a dotted do_identifier with simple left: Alias.do_id or identifier.do_id
  defp gen_dotted_do_identifier_simple do
    left_gen =
      StreamData.frequency([
        {2, gen_alias()},
        {1, gen_identifier()}
      ])

    StreamData.bind(left_gen, fn left ->
      StreamData.bind(StreamData.member_of(@do_identifiers), fn do_id ->
        StreamData.constant({:dot_do_identifier, left, do_id})
      end)
    end)
  end

  # Generate a dotted do_identifier with matched_expr left: (matched_expr).do_id
  # Per grammar: dot_do_identifier -> matched_expr dot_op do_identifier
  defp gen_dotted_do_identifier_full(state) do
    restricted_state = restrict_unmatched(state)

    left_gen =
      StreamData.frequency([
        {3, gen_alias()},
        {2, gen_identifier()},
        {1, gen_sub_matched_expr(restricted_state)}
      ])

    StreamData.bind(left_gen, fn left ->
      StreamData.bind(StreamData.member_of(@do_identifiers), fn do_id ->
        StreamData.constant({:dot_do_identifier, left, do_id})
      end)
    end)
  end

  # Generate a dotted identifier with simple left: Alias.id or identifier.id
  # Per grammar: dot_identifier -> matched_expr dot_op identifier
  defp gen_dot_identifier_simple do
    left_gen =
      StreamData.frequency([
        {3, gen_identifier()},
        {2, gen_alias()}
      ])

    StreamData.bind(left_gen, fn left ->
      StreamData.bind(StreamData.member_of(@identifiers), fn right_name ->
        StreamData.constant({:dot_identifier, left, right_name})
      end)
    end)
  end

  # Generate a dotted identifier with matched_expr left: (matched_expr).id
  # Per grammar: dot_identifier -> matched_expr dot_op identifier
  defp gen_dot_identifier_full(state) do
    restricted_state = restrict_unmatched(state)

    left_gen =
      StreamData.frequency([
        {3, gen_identifier()},
        {2, gen_alias()},
        {1, gen_sub_matched_expr(restricted_state)}
      ])

    StreamData.bind(left_gen, fn left ->
      StreamData.bind(StreamData.member_of(@identifiers), fn right_name ->
        StreamData.constant({:dot_identifier, left, right_name})
      end)
    end)
  end

  @doc """
  Generate an access expression (leaf nodes).

  Per grammar lines 273-301, includes:
  - Literals (int, float, char, atom, bool, nil)
  - Aliases (dot_alias)
  - fn expressions
  - Parenthesized calls (parens_call)
  - Capture integers (capture_int int)
  - Parenthesized expressions
  - Empty parentheses (empty_paren)
  - Lists ([a, b, c])
  - Tuples ({a, b})
  - Bracket access (foo[bar], @foo[bar])

  NOTE: Identifiers are NOT in access_expr per grammar.
  They belong to no_parens_zero_expr (sub_matched_expr).

  ## Currently Disabled (Toxic limitations)
  - Maps (%{a: 1}) - Toxic doesn't render %{} token correctly
  - Binary strings ("hello") - Toxic doesn't support this token format yet

  ## TODO Phase 7+: Additional access_expr forms
  - list_string / list_heredoc ('hello')
  - bin_heredoc (\"\"\")
  - bitstring (<<1, 2, 3>>)
  - sigil (~r/regex/, ~s[string])
  - atom_quoted / atom_safe / atom_unsafe (:"hello world")
  """
  def gen_access_expr(state) do
    if GrammarTree.budget_exhausted?(state) do
      gen_fallback_literal()
    else
      StreamData.frequency([
        {5, gen_literal()},
        {2, gen_alias()},
        {2, gen_fn_single(state)},
        {1, gen_fn_multi(state)},
        {2, gen_call_parens(state)},
        {1, gen_call_parens_nested(state)},
        {1, gen_capture_int()},
        {1, gen_paren_expr(state)},
        {1, gen_empty_paren()},
        # Paren stab forms: (x -> y), (; x -> y), (;)
        {2, gen_paren_stab(state)},
        {1, gen_paren_stab_semi(state)},
        {1, gen_paren_stab_empty()},
        # Container types
        {2, gen_list(state)},
        {2, gen_tuple(state)},
        # Bracket access
        {2, gen_bracket_expr(state)},
        # Bracket at access: @foo[bar]
        {1, gen_bracket_at_expr(state)},
        # Bitstring: <<1, 2, 3>>
        {2, gen_bitstring(state)}
        # NOTE: bin_string disabled - Toxic doesn't support this token format yet
        # {2, gen_bin_string()}
        # NOTE: map disabled - Toxic doesn't render %{} token correctly
        # {2, gen_map(state)}
      ])
    end
  end

  # ===========================================================================
  # Category-Aware: Matched Operators
  # ===========================================================================

  # Generate matched binary operator: left op right (both matched)
  # Per grammar: matched_expr -> matched_expr matched_op_expr
  defp gen_matched_op(state) do
    child_state = GrammarTree.decr_depth(state)
    restricted_state = restrict_unmatched(child_state)

    operand_gen =
      if child_state.budget.depth <= 1 do
        gen_sub_matched_expr(restricted_state)
      else
        gen_matched_expr(restricted_state)
      end

    StreamData.bind(operand_gen, fn left ->
      StreamData.bind(gen_op_eol(), fn op_eol ->
        StreamData.bind(operand_gen, fn right ->
          StreamData.constant({:matched_op, left, op_eol, right})
        end)
      end)
    end)
  end

  # Generate range with step: left..middle//step
  # Per grammar lines 739-746: ternary_op (//) is only valid immediately after range_op (..)
  # The result is: {'..//', Meta, [Left, Middle, Step]}
  #
  # Example: 1..10//2 produces range 1 to 10 with step 2
  defp gen_range_step(state) do
    child_state = GrammarTree.decr_depth(state)
    restricted_state = restrict_unmatched(child_state)

    operand_gen =
      if child_state.budget.depth <= 1 do
        gen_sub_matched_expr(restricted_state)
      else
        gen_matched_expr(restricted_state)
      end

    StreamData.bind(operand_gen, fn left ->
      StreamData.bind(gen_newlines(), fn range_newlines ->
        StreamData.bind(operand_gen, fn middle ->
          StreamData.bind(gen_newlines(), fn step_newlines ->
            StreamData.bind(operand_gen, fn step ->
              StreamData.constant(
                {:range_step, left, range_newlines, middle, step_newlines, step}
              )
            end)
          end)
        end)
      end)
    end)
  end

  # Generate matched_op with warn_pipe pattern: left arrow_op no_parens_one_expr
  # Per grammar line 209: matched_op_expr -> arrow_op_eol no_parens_one_expr : warn_pipe('$1', '$2')
  # This generates code like: foo 1 |> bar 2
  # The parser will emit a warning for this pattern.
  defp gen_matched_op_warn_pipe(state) do
    child_state = GrammarTree.decr_depth(state)
    restricted_state = restrict_unmatched(child_state)

    left_gen =
      if child_state.budget.depth <= 1 do
        gen_sub_matched_expr(restricted_state)
      else
        gen_matched_expr(restricted_state)
      end

    # Right side must be no_parens_one_expr
    right_gen = gen_call_no_parens_one(child_state)

    StreamData.bind(left_gen, fn left ->
      StreamData.bind(StreamData.member_of(@arrow_ops), fn {op_kind, op} ->
        StreamData.bind(gen_newlines(), fn newlines ->
          StreamData.bind(right_gen, fn right ->
            # Use :matched_op_warn_pipe to signal this is the warn_pipe pattern
            StreamData.constant(
              {:matched_op_warn_pipe, left, {:op_eol, {op_kind, op}, newlines}, right}
            )
          end)
        end)
      end)
    end)
  end

  # Generate matched unary operator: op operand (operand matched)
  # Per grammar: matched_expr -> unary_op_eol matched_expr
  # unary_op_eol -> unary_op | unary_op eol
  defp gen_matched_unary(state) do
    child_state = GrammarTree.decr_depth(state)
    restricted_state = restrict_unmatched(child_state)

    operand_gen =
      if child_state.budget.depth <= 1 do
        gen_sub_matched_expr(restricted_state)
      else
        gen_matched_expr(restricted_state)
      end

    StreamData.bind(StreamData.member_of(@unary_ops), fn {op_kind, op} ->
      StreamData.bind(gen_newlines(), fn newlines ->
        StreamData.bind(operand_gen, fn operand ->
          StreamData.constant({:matched_unary, {op_kind, op}, newlines, operand})
        end)
      end)
    end)
  end

  # Generate at_op expression: @foo, @spec, etc.
  # Per grammar: matched_expr -> at_op_eol matched_expr
  # at_op_eol -> at_op | at_op eol
  defp gen_at_op(state) do
    child_state = GrammarTree.decr_depth(state)
    restricted_state = restrict_unmatched(child_state)

    operand_gen =
      if child_state.budget.depth <= 1 do
        gen_sub_matched_expr(restricted_state)
      else
        gen_matched_expr(restricted_state)
      end

    StreamData.bind(gen_newlines(), fn newlines ->
      StreamData.bind(operand_gen, fn operand ->
        StreamData.constant({:at_op, newlines, operand})
      end)
    end)
  end

  # Generate capture_op expression: &expr, &Mod.fun/1, &(&1 + &2)
  # Per grammar: matched_expr -> capture_op_eol matched_expr
  # capture_op_eol -> capture_op | capture_op eol
  defp gen_capture_op(state) do
    child_state = GrammarTree.decr_depth(state)
    restricted_state = restrict_unmatched(child_state)

    operand_gen =
      if child_state.budget.depth <= 1 do
        gen_sub_matched_expr(restricted_state)
      else
        gen_matched_expr(restricted_state)
      end

    StreamData.bind(gen_newlines(), fn newlines ->
      StreamData.bind(operand_gen, fn operand ->
        StreamData.constant({:capture_op, newlines, operand})
      end)
    end)
  end

  # Generate ellipsis as prefix operator: ...expr
  # Per grammar: matched_expr -> ellipsis_op matched_expr
  defp gen_ellipsis_prefix(state) do
    child_state = GrammarTree.decr_depth(state)
    restricted_state = restrict_unmatched(child_state)

    operand_gen =
      if child_state.budget.depth <= 1 do
        gen_sub_matched_expr(restricted_state)
      else
        gen_matched_expr(restricted_state)
      end

    StreamData.bind(operand_gen, fn operand ->
      StreamData.constant({:ellipsis_prefix, operand})
    end)
  end

  # Per grammar lines 173-179, no_parens_expr allows function calls without
  # parentheses where nesting ambiguity exists. These require careful handling.
  #
  # no_parens_expr -> matched_expr no_parens_op_expr      (binary op with no_parens right)
  # no_parens_expr -> unary_op_eol no_parens_expr         (unary prefix)
  # no_parens_expr -> at_op_eol no_parens_expr            (at prefix)
  # no_parens_expr -> capture_op_eol no_parens_expr       (capture prefix)
  # no_parens_expr -> ellipsis_op no_parens_expr          (ellipsis prefix)
  # no_parens_expr -> no_parens_one_ambig_expr            (ambiguous nested call)
  # no_parens_expr -> no_parens_many_expr                 (multi-arg call)

  # Main generator for no_parens_expr
  def gen_no_parens_expr(state) do
    if GrammarTree.budget_exhausted?(state) do
      # Fallback to simplest no_parens form: multi-arg call
      gen_no_parens_many_expr_simple()
    else
      child_state = GrammarTree.decr_depth(state)

      StreamData.frequency([
        # no_parens_one_ambig_expr: foo bar 1, 2 (nested ambiguous call)
        {3, gen_no_parens_one_ambig_expr(child_state)},
        # no_parens_many_expr: foo a, b, c (multi-arg call)
        {4, gen_no_parens_many_expr(child_state)},
        # Binary op with no_parens_expr on right: matched_expr no_parens_op_expr
        {2, gen_no_parens_op(child_state)},
        # Unary prefix operators with no_parens_expr operand
        {1, gen_no_parens_unary(child_state)},
        {1, gen_no_parens_at_op(child_state)},
        {1, gen_no_parens_capture_op(child_state)},
        {1, gen_no_parens_ellipsis(child_state)}
      ])
    end
  end

  # Generate binary op: matched_expr no_parens_op_expr
  # Per grammar line 173: no_parens_expr -> matched_expr no_parens_op_expr
  defp gen_no_parens_op(state) do
    child_state = GrammarTree.decr_depth(state)
    restricted_state = restrict_unmatched(child_state)

    left_gen =
      if child_state.budget.depth <= 1 do
        gen_sub_matched_expr(restricted_state)
      else
        gen_matched_expr(restricted_state)
      end

    # Right side is no_parens_expr
    right_gen =
      if child_state.budget.depth <= 1 do
        gen_no_parens_many_expr_simple()
      else
        gen_no_parens_expr(child_state)
      end

    StreamData.bind(left_gen, fn left ->
      StreamData.frequency([
        # General case: op_eol followed by no_parens_expr (covers grammar rules 1-18)
        # Per grammar lines 230-247: no_parens_op_expr -> *_op_eol no_parens_expr
        {8,
         StreamData.bind(gen_op_eol(), fn op_eol ->
           StreamData.bind(right_gen, fn right ->
             StreamData.constant({:no_parens_op, left, op_eol, right})
           end)
         end)},

        # Special case: when_op_eol followed by call_args_no_parens_kw (grammar rule 19)
        # Per grammar line 250: no_parens_op_expr -> when_op_eol call_args_no_parens_kw
        {2,
         StreamData.bind(gen_newlines(), fn newlines ->
           StreamData.bind(gen_call_args_no_parens_kw(child_state), fn kw_args ->
             StreamData.constant(
               {:no_parens_op, left, {:op_eol, {:when_op, :when}, newlines}, kw_args}
             )
           end)
         end)}
      ])
    end)
  end

  # Generate unary op with no_parens_expr operand
  # Per grammar line 174: no_parens_expr -> unary_op_eol no_parens_expr
  defp gen_no_parens_unary(state) do
    child_state = GrammarTree.decr_depth(state)

    operand_gen =
      if child_state.budget.depth <= 1 do
        gen_no_parens_many_expr_simple()
      else
        gen_no_parens_expr(child_state)
      end

    StreamData.bind(StreamData.member_of(@unary_ops), fn {op_kind, op} ->
      StreamData.bind(gen_newlines(), fn newlines ->
        StreamData.bind(operand_gen, fn operand ->
          StreamData.constant({:no_parens_unary, {op_kind, op}, newlines, operand})
        end)
      end)
    end)
  end

  # Generate at_op with no_parens_expr operand
  # Per grammar line 175: no_parens_expr -> at_op_eol no_parens_expr
  defp gen_no_parens_at_op(state) do
    child_state = GrammarTree.decr_depth(state)

    operand_gen =
      if child_state.budget.depth <= 1 do
        gen_no_parens_many_expr_simple()
      else
        gen_no_parens_expr(child_state)
      end

    StreamData.bind(gen_newlines(), fn newlines ->
      StreamData.bind(operand_gen, fn operand ->
        StreamData.constant({:no_parens_at_op, newlines, operand})
      end)
    end)
  end

  # Generate capture_op with no_parens_expr operand
  # Per grammar line 176: no_parens_expr -> capture_op_eol no_parens_expr
  defp gen_no_parens_capture_op(state) do
    child_state = GrammarTree.decr_depth(state)

    operand_gen =
      if child_state.budget.depth <= 1 do
        gen_no_parens_many_expr_simple()
      else
        gen_no_parens_expr(child_state)
      end

    StreamData.bind(gen_newlines(), fn newlines ->
      StreamData.bind(operand_gen, fn operand ->
        StreamData.constant({:no_parens_capture_op, newlines, operand})
      end)
    end)
  end

  # Generate ellipsis_op with no_parens_expr operand
  # Per grammar line 177: no_parens_expr -> ellipsis_op no_parens_expr
  defp gen_no_parens_ellipsis(state) do
    child_state = GrammarTree.decr_depth(state)

    operand_gen =
      if child_state.budget.depth <= 1 do
        gen_no_parens_many_expr_simple()
      else
        gen_no_parens_expr(child_state)
      end

    StreamData.bind(operand_gen, fn operand ->
      StreamData.constant({:no_parens_ellipsis, operand})
    end)
  end

  # ===========================================================================
  # no_parens_one_ambig_expr (nested ambiguous calls)
  # ===========================================================================
  #
  # Per grammar lines 252-253:
  # no_parens_one_ambig_expr -> dot_op_identifier call_args_no_parens_ambig
  # no_parens_one_ambig_expr -> dot_identifier call_args_no_parens_ambig
  #
  # Where call_args_no_parens_ambig -> no_parens_expr : ['$1']
  #
  # This represents: foo bar 1, 2 where "bar 1, 2" is the no_parens_expr argument
  # The outer call (foo) has one argument that is itself a no_parens call.

  defp gen_no_parens_one_ambig_expr(state) do
    child_state = GrammarTree.decr_depth(state)

    # Target: identifier, dot_identifier, op_identifier, or dot_op_identifier
    target_gen =
      StreamData.frequency([
        {4, StreamData.member_of(@identifiers) |> StreamData.map(&{:identifier, &1})},
        {1, gen_dot_identifier(child_state)},
        {1,
         StreamData.bind(StreamData.member_of(@binary_ops), fn {_, op} ->
           StreamData.constant({:op_identifier, op})
         end)},
        {1, gen_dot_op_identifier_for_call(child_state)}
      ])

    # Argument: a no_parens_expr (the ambiguous nested call)
    arg_gen =
      if child_state.budget.depth <= 1 do
        gen_no_parens_many_expr_simple()
      else
        # Prefer no_parens_many to create clear nested patterns
        StreamData.frequency([
          {4, gen_no_parens_many_expr(child_state)},
          {1, gen_no_parens_expr(child_state)}
        ])
      end

    StreamData.bind(target_gen, fn target ->
      StreamData.bind(arg_gen, fn arg ->
        StreamData.constant({:no_parens_one_ambig, target, arg})
      end)
    end)
  end

  # ===========================================================================
  # no_parens_many_expr (multi-argument calls without parentheses)
  # ===========================================================================
  #
  # Per grammar lines 255-256:
  # no_parens_many_expr -> dot_op_identifier call_args_no_parens_many_strict
  # no_parens_many_expr -> dot_identifier call_args_no_parens_many_strict
  #
  # Where call_args_no_parens_many (lines 520-522):
  # - matched_expr ',' call_args_no_parens_kw     (expr, kw: val)
  # - call_args_no_parens_comma_expr              (a, b, c)
  # - call_args_no_parens_comma_expr ',' kw       (a, b, kw: val)
  #
  # Examples: foo a, b, c  or  foo a, b, key: val

  defp gen_no_parens_many_expr(state) do
    child_state = GrammarTree.decr_depth(state)

    # Target: identifier or dot_identifier (also allow operator identifier targets per grammar)
    target_gen =
      StreamData.frequency([
        {4, StreamData.member_of(@identifiers) |> StreamData.map(&{:identifier, &1})},
        {1, gen_dot_identifier(child_state)},
        # Operator-as-identifier targets (e.g., +/2) and dotted operator targets (expr.+)
        {1,
         StreamData.bind(StreamData.member_of(@binary_ops), fn {_, op} ->
           StreamData.constant({:op_identifier, op})
         end)},
        {1, gen_dot_op_identifier_for_call(child_state)}
      ])

    # Arguments: 2+ args (call_args_no_parens_many)
    args_gen = gen_call_args_no_parens_many(child_state)

    StreamData.bind(target_gen, fn target ->
      StreamData.bind(args_gen, fn args ->
        StreamData.constant({:no_parens_many, target, args})
      end)
    end)
  end

  # Simplified no_parens_many for depth exhaustion fallback
  defp gen_no_parens_many_expr_simple do
    StreamData.bind(StreamData.member_of(@identifiers), fn name ->
      StreamData.bind(StreamData.integer(2..3), fn count ->
        StreamData.bind(gen_simple_expr_list(count), fn args ->
          StreamData.constant({:no_parens_many, {:identifier, name}, args})
        end)
      end)
    end)
  end

  # Generate call_args_no_parens_many: 2+ arguments
  # Per grammar lines 520-522
  defp gen_call_args_no_parens_many(state) do
    restricted_state = restrict_unmatched(state)

    StreamData.frequency([
      # Just positional args: a, b, c
      {4, gen_positional_args_list(restricted_state, 2, 4)},
      # Positional + trailing keyword: a, b, key: val
      {2, gen_args_with_trailing_kw(restricted_state)}
    ])
  end

  # Generate 2-4 positional arguments (matched_expr only)
  defp gen_positional_args_list(state, min, max) do
    StreamData.bind(StreamData.integer(min..max), fn count ->
      gen_matched_expr_list(state, count)
    end)
  end

  # Generate list of matched expressions
  defp gen_matched_expr_list(_state, 0), do: StreamData.constant([])

  defp gen_matched_expr_list(state, count) when count > 0 do
    expr_gen =
      if state.budget.depth <= 1 do
        gen_simple_expr()
      else
        gen_sub_matched_expr(state)
      end

    StreamData.bind(expr_gen, fn expr ->
      StreamData.bind(gen_matched_expr_list(state, count - 1), fn rest ->
        StreamData.constant([expr | rest])
      end)
    end)
  end

  # Generate positional args + trailing keyword args: a, b, key: val
  defp gen_args_with_trailing_kw(state) do
    StreamData.bind(StreamData.integer(1..2), fn pos_count ->
      StreamData.bind(gen_matched_expr_list(state, pos_count), fn positional ->
        StreamData.bind(gen_call_args_no_parens_kw(state), fn kw_args ->
          StreamData.constant(positional ++ [kw_args])
        end)
      end)
    end)
  end

  # ===========================================================================
  # call_args_no_parens_all generator
  # ===========================================================================
  #
  # Per grammar lines 508-510:
  # call_args_no_parens_all -> call_args_no_parens_one   : single arg or kw
  # call_args_no_parens_all -> call_args_no_parens_ambig : nested no_parens call
  # call_args_no_parens_all -> call_args_no_parens_many  : 2+ args
  #
  # This is used in:
  # - block_expr rules (lines 184-185): dot_op_identifier/dot_identifier call_args_no_parens_all do_block
  # - stab_expr (line 358): call_args_no_parens_all stab_op_eol_and_expr

  @doc """
  Generate call_args_no_parens_all - any valid argument form for no-parens calls.

  Returns one of:
  - `{:call_args_one, {:single_arg, expr}}` - single matched_expr
  - `{:call_args_one, {:kw_args, pairs}}` - keyword args only
  - `{:call_args_ambig, no_parens_expr}` - nested ambiguous call
  - `{:call_args_many, [exprs]}` - 2+ positional args, optionally with trailing kw
  """
  def gen_call_args_no_parens_all(state) do
    child_state = GrammarTree.decr_depth(state)

    if child_state.budget.depth <= 1 do
      # At shallow depth, only simple forms
      StreamData.frequency([
        # call_args_no_parens_one: single arg
        {4, gen_simple_expr() |> StreamData.map(&{:call_args_one, {:single_arg, &1}})},
        # call_args_no_parens_one: keyword args
        {2, gen_call_args_no_parens_kw() |> StreamData.map(&{:call_args_one, &1})},
        # call_args_no_parens_many: 2+ args
        {3, gen_positional_args_list(child_state, 2, 3) |> StreamData.map(&{:call_args_many, &1})}
      ])
    else
      restricted_state = restrict_unmatched(child_state)

      StreamData.frequency([
        # call_args_no_parens_one: single matched_expr
        {4,
         gen_sub_matched_expr(restricted_state)
         |> StreamData.map(&{:call_args_one, {:single_arg, &1}})},
        # call_args_no_parens_one: keyword args
        {2, gen_call_args_no_parens_kw() |> StreamData.map(&{:call_args_one, &1})},
        # call_args_no_parens_ambig: nested no_parens call
        {2, gen_no_parens_expr(child_state) |> StreamData.map(&{:call_args_ambig, &1})},
        # call_args_no_parens_many: 2+ args (positional only)
        {3,
         gen_positional_args_list(restricted_state, 2, 4)
         |> StreamData.map(&{:call_args_many, &1})},
        # call_args_no_parens_many: positional + trailing kw
        {2, gen_args_with_trailing_kw(restricted_state) |> StreamData.map(&{:call_args_many, &1})}
      ])
    end
  end

  # Helper: generate dot_identifier target (matched_expr.identifier)
  defp gen_dot_identifier(state) do
    restricted_state = restrict_unmatched(state)

    left_gen =
      if state.budget.depth <= 1 do
        gen_simple_expr()
      else
        gen_sub_matched_expr(restricted_state)
      end

    StreamData.bind(left_gen, fn left ->
      StreamData.bind(StreamData.member_of(@identifiers), fn name ->
        StreamData.constant({:dot_identifier, left, name})
      end)
    end)
  end

  # ===========================================================================
  # Category-Aware: Unmatched Prefix Operators
  # ===========================================================================
  #
  # Per grammar lines 167-170, unmatched_expr includes prefix operators with
  # any expr (not just matched_expr) as the operand:
  #   unmatched_expr -> unary_op_eol expr
  #   unmatched_expr -> at_op_eol expr
  #   unmatched_expr -> capture_op_eol expr
  #   unmatched_expr -> ellipsis_op expr
  #
  # This allows expressions like: &if true do :ok end, @case x do ... end, etc.

  # Generate unmatched unary operator: op expr (operand can be unmatched)
  # Per grammar: unmatched_expr -> unary_op_eol expr
  defp gen_unmatched_unary(state) do
    child_state = GrammarTree.decr_depth(state)

    # Operand can be any expression including unmatched
    operand_gen =
      if child_state.budget.depth <= 1 do
        gen_simple_call_do(child_state)
      else
        gen_unmatched_expr(child_state)
      end

    StreamData.bind(StreamData.member_of(@unary_ops), fn {op_kind, op} ->
      StreamData.bind(gen_newlines(), fn newlines ->
        StreamData.bind(operand_gen, fn operand ->
          # Use same :matched_unary tag - TokenCompiler handles it the same way
          StreamData.constant({:matched_unary, {op_kind, op}, newlines, operand})
        end)
      end)
    end)
  end

  # Generate unmatched at_op expression: @expr where expr can be unmatched
  # Per grammar: unmatched_expr -> at_op_eol expr
  # Example: @if true do :ok end
  defp gen_unmatched_at_op(state) do
    child_state = GrammarTree.decr_depth(state)

    # Operand can be any expression including unmatched
    operand_gen =
      if child_state.budget.depth <= 1 do
        gen_simple_call_do(child_state)
      else
        gen_unmatched_expr(child_state)
      end

    StreamData.bind(gen_newlines(), fn newlines ->
      StreamData.bind(operand_gen, fn operand ->
        # Use same :at_op tag - TokenCompiler handles it the same way
        StreamData.constant({:at_op, newlines, operand})
      end)
    end)
  end

  # Generate unmatched capture_op expression: &expr where expr can be unmatched
  # Per grammar: unmatched_expr -> capture_op_eol expr
  # Example: &if true do :ok end, &Mod.fun/1 with do-block
  defp gen_unmatched_capture_op(state) do
    child_state = GrammarTree.decr_depth(state)

    # Operand can be any expression including unmatched
    operand_gen =
      if child_state.budget.depth <= 1 do
        gen_simple_call_do(child_state)
      else
        gen_unmatched_expr(child_state)
      end

    StreamData.bind(gen_newlines(), fn newlines ->
      StreamData.bind(operand_gen, fn operand ->
        # Use same :capture_op tag - TokenCompiler handles it the same way
        StreamData.constant({:capture_op, newlines, operand})
      end)
    end)
  end

  # Generate unmatched ellipsis expression: ...expr where expr can be unmatched
  # Per grammar: unmatched_expr -> ellipsis_op expr
  # Example: ...if true do :ok end
  defp gen_unmatched_ellipsis(state) do
    child_state = GrammarTree.decr_depth(state)

    # Operand can be any expression including unmatched
    operand_gen =
      if child_state.budget.depth <= 1 do
        gen_simple_call_do(child_state)
      else
        gen_unmatched_expr(child_state)
      end

    StreamData.bind(operand_gen, fn operand ->
      # Use same :ellipsis_prefix tag - TokenCompiler handles it the same way
      StreamData.constant({:ellipsis_prefix, operand})
    end)
  end

  # ===========================================================================
  # Category-Aware: Unmatched Operators
  # ===========================================================================

  # Generate unmatched binary operator: various combinations per grammar
  # Per grammar variants:
  # - matched_expr unmatched_op_expr
  # - unmatched_expr matched_op_expr
  # - unmatched_expr unmatched_op_expr
  # - unmatched_expr no_parens_op_expr (warn_pipe)
  defp gen_unmatched_op(state) do
    child_state = GrammarTree.decr_depth(state)
    restricted_state = restrict_unmatched(child_state)

    # Variant A: matched_left + unmatched_right  (existing common case)
    variant_a =
      if child_state.budget.depth <= 1 do
        StreamData.bind(gen_sub_matched_expr(restricted_state), fn left ->
          StreamData.bind(gen_op_eol(), fn op_eol ->
            StreamData.bind(gen_simple_call_do(child_state), fn right ->
              StreamData.constant({:unmatched_op, left, op_eol, right})
            end)
          end)
        end)
      else
        StreamData.bind(gen_matched_expr(restricted_state), fn left ->
          StreamData.bind(gen_op_eol(), fn op_eol ->
            StreamData.bind(gen_unmatched_expr(child_state), fn right ->
              StreamData.constant({:unmatched_op, left, op_eol, right})
            end)
          end)
        end)
      end

    # Variant B: unmatched_left + matched_right (unmatched_expr -> unmatched_expr matched_op_expr)
    # Represent using :unmatched_op to reflect that the left side can be unmatched
    variant_b =
      if child_state.budget.depth <= 1 do
        StreamData.bind(gen_simple_call_do(child_state), fn left ->
          StreamData.bind(gen_op_eol(), fn op_eol ->
            StreamData.bind(gen_sub_matched_expr(restricted_state), fn right ->
              StreamData.constant({:unmatched_op, left, op_eol, right})
            end)
          end)
        end)
      else
        StreamData.bind(gen_unmatched_expr(child_state), fn left ->
          StreamData.bind(gen_op_eol(), fn op_eol ->
            StreamData.bind(gen_matched_expr(restricted_state), fn right ->
              StreamData.constant({:unmatched_op, left, op_eol, right})
            end)
          end)
        end)
      end

    # Variant C: unmatched_left + unmatched_right (unmatched_expr -> unmatched_expr unmatched_op_expr)
    variant_c =
      StreamData.bind(gen_unmatched_expr(child_state), fn left ->
        StreamData.bind(gen_op_eol(), fn op_eol ->
          StreamData.bind(gen_unmatched_expr(child_state), fn right ->
            StreamData.constant({:unmatched_op, left, op_eol, right})
          end)
        end)
      end)

    # Variant D: warn-pipe pattern where right side is a no_parens_one_expr
    # warn-pipe: allow either matched or unmatched left (unmatched_expr no_parens_op_expr)
    variant_d =
      StreamData.bind(
        StreamData.frequency([
          {3, gen_matched_expr(restricted_state)},
          {1, gen_unmatched_expr(child_state)}
        ]),
        fn left ->
          StreamData.bind(StreamData.member_of(@arrow_ops), fn {op_kind, op} ->
            StreamData.bind(gen_newlines(), fn newlines ->
              StreamData.bind(gen_call_no_parens_one(child_state), fn right ->
                StreamData.constant(
                  {:matched_op_warn_pipe, left, {:op_eol, {op_kind, op}, newlines}, right}
                )
              end)
            end)
          end)
        end
      )

    StreamData.frequency([
      {5, variant_a},
      {3, variant_b},
      {2, variant_c},
      {1, variant_d}
    ])
  end

  # Generate a simple call_do when depth is limited
  defp gen_simple_call_do(_state) do
    StreamData.bind(gen_do_condition(), fn cond ->
      body = [{:atom_lit, :ok}]
      do_block = {:do_block, body, []}
      StreamData.constant({:call_do, {:identifier, :if}, [cond], do_block})
    end)
  end

  # ===========================================================================
  # Category-Aware: Nullary Operators
  # ===========================================================================

  @doc "Generate nullary range operator (..)"
  def gen_nullary_range do
    StreamData.constant({:nullary_range, nil})
  end

  @doc "Generate nullary ellipsis operator (...)"
  def gen_nullary_ellipsis do
    StreamData.constant({:nullary_ellipsis, nil})
  end

  # Generate an access_expr followed by a kw identifier (e.g. foo a:)
  # This corresponds to the grammar case that should be reported as an
  # invalid keyword identifier when it follows an access expression.
  def gen_access_expr_kw_identifier(state) do
    StreamData.bind(gen_access_expr(state), fn acc ->
      StreamData.bind(StreamData.member_of(@identifiers), fn key ->
        StreamData.constant({:access_expr_kw_identifier, acc, key})
      end)
    end)
  end

  # ===========================================================================
  # Category-Aware: Parenthesized Expressions
  # ===========================================================================

  # Generate parenthesized expression: (expr)
  defp gen_paren_expr(state) do
    child_state = GrammarTree.decr_depth(state)

    expr_gen =
      if child_state.budget.depth <= 1 do
        gen_simple_expr()
      else
        gen_matched_expr(child_state)
      end

    StreamData.map(expr_gen, fn expr -> {:paren_expr, expr} end)
  end

  @doc "Generate empty parentheses: ()"
  def gen_empty_paren do
    StreamData.constant({:empty_paren, nil})
  end

  # ===========================================================================
  # Container Generators: Lists, Tuples, Maps
  # ===========================================================================

  @doc """
  Generate a list: [elem1, elem2, ...]

  Per grammar lines 598-599:
  - list -> open_bracket ']'
  - list -> open_bracket list_args close_bracket
  """
  def gen_list(state) do
    child_state = GrammarTree.decr_depth(state)

    StreamData.frequency([
      # Empty list
      {1, StreamData.constant({:list, []})},
      # List with 1-3 elements
      {4, gen_list_with_elements(child_state, 1, 3)}
    ])
  end

  defp gen_list_with_elements(state, min, max) do
    StreamData.bind(StreamData.integer(min..max), fn count ->
      gen_container_args(state, count)
    end)
    |> StreamData.map(fn args -> {:list, args} end)
  end

  @doc """
  Generate a tuple: {elem1, elem2, ...}

  Per grammar lines 603-605:
  - tuple -> open_curly '}'
  - tuple -> open_curly container_args close_curly
  """
  def gen_tuple(state) do
    child_state = GrammarTree.decr_depth(state)

    StreamData.frequency([
      # Empty tuple
      {1, StreamData.constant({:tuple, []})},
      # Tuple with 1-3 elements
      {4, gen_tuple_with_elements(child_state, 1, 3)}
    ])
  end

  defp gen_tuple_with_elements(state, min, max) do
    StreamData.bind(StreamData.integer(min..max), fn count ->
      gen_container_args(state, count)
    end)
    |> StreamData.map(fn args -> {:tuple, args} end)
  end

  @doc """
  Generate a map: %{key => value, ...} or %{key: value, ...}

  Per grammar lines 648-656:
  - map -> map_op map_args
  """
  def gen_map(state) do
    child_state = GrammarTree.decr_depth(state)

    StreamData.frequency([
      # Empty map
      {1, StreamData.constant({:map, []})},
      # Map with keyword syntax (key: value)
      {3, gen_map_keyword(child_state, 1, 3)},
      # Map with arrow syntax (key => value)
      {2, gen_map_arrow(child_state, 1, 3)}
    ])
  end

  # Generate map with keyword syntax: %{foo: 1, bar: 2}
  defp gen_map_keyword(state, min, max) do
    StreamData.bind(StreamData.integer(min..max), fn count ->
      gen_kw_pairs(state, count)
    end)
    |> StreamData.map(fn pairs -> {:map, {:kw, pairs}} end)
  end

  # Generate map with arrow syntax: %{:foo => 1, :bar => 2}
  defp gen_map_arrow(state, min, max) do
    StreamData.bind(StreamData.integer(min..max), fn count ->
      gen_assoc_pairs(state, count)
    end)
    |> StreamData.map(fn pairs -> {:map, {:assoc, pairs}} end)
  end

  # Generate keyword pairs: [{key, value}, ...]
  defp gen_kw_pairs(_state, 0), do: StreamData.constant([])

  defp gen_kw_pairs(state, count) when count > 0 do
    StreamData.bind(StreamData.member_of(@atoms), fn key ->
      StreamData.bind(gen_simple_expr(), fn value ->
        StreamData.bind(gen_kw_pairs(state, count - 1), fn rest ->
          StreamData.constant([{key, value} | rest])
        end)
      end)
    end)
  end

  # Generate association pairs: [{key, value}, ...] for arrow syntax
  defp gen_assoc_pairs(_state, 0), do: StreamData.constant([])

  defp gen_assoc_pairs(state, count) when count > 0 do
    StreamData.bind(gen_simple_expr(), fn key ->
      StreamData.bind(gen_simple_expr(), fn value ->
        StreamData.bind(gen_assoc_pairs(state, count - 1), fn rest ->
          StreamData.constant([{key, value} | rest])
        end)
      end)
    end)
  end

  # Generate container arguments (for lists and tuples)
  defp gen_container_args(_state, 0), do: StreamData.constant([])

  defp gen_container_args(state, count) when count > 0 do
    arg_gen =
      if state.budget.depth <= 1 do
        gen_simple_expr()
      else
        gen_matched_expr(state)
      end

    StreamData.bind(arg_gen, fn arg ->
      StreamData.bind(gen_container_args(GrammarTree.decr_nodes(state), count - 1), fn rest ->
        StreamData.constant([arg | rest])
      end)
    end)
  end

  # ===========================================================================
  # Bracket Access Generators
  # ===========================================================================

  @doc """
  Generate a bracket access expression: foo[bar]

  Per grammar lines 312-313:
  - bracket_expr -> dot_bracket_identifier bracket_arg
  - bracket_expr -> access_expr bracket_arg

  Where bracket_arg is: open_bracket container_expr close_bracket
  """
  def gen_bracket_expr(state) do
    child_state = GrammarTree.decr_depth(state)

    StreamData.frequency([
      # Identifier with bracket access: foo[bar]
      {4, gen_bracket_identifier(child_state)},
      # Dotted bracket identifier: expr.foo[bar]
      {2, gen_dot_bracket_identifier(child_state)},
      # Expression with bracket access: expr[key] (less common to avoid nesting)
      {1, gen_bracket_access_expr(child_state)}
    ])
  end

  # Generate dotted bracket identifier: matched_expr . bracket_identifier [arg]
  defp gen_dot_bracket_identifier(state) do
    child_state = GrammarTree.decr_depth(state)
    restricted = restrict_unmatched(child_state)

    left_gen =
      if child_state.budget.depth <= 1 do
        StreamData.frequency([
          {2, gen_alias()},
          {1, gen_identifier()}
        ])
      else
        StreamData.frequency([
          {3, gen_alias()},
          {2, gen_identifier()},
          {1, gen_sub_matched_expr(restricted)}
        ])
      end

    StreamData.bind(left_gen, fn left ->
      StreamData.bind(StreamData.member_of(@identifiers), fn name ->
        StreamData.bind(gen_bracket_arg(state), fn arg ->
          StreamData.constant({:bracket_expr, {:dot_bracket_identifier, left, name}, arg})
        end)
      end)
    end)
  end

  @doc """
  Generate a bracket access expression with at operator: @foo[bar]

  Per grammar lines 310-311:
  - bracket_at_expr -> at_op_eol dot_bracket_identifier bracket_arg
  - bracket_at_expr -> at_op_eol access_expr bracket_arg
  """
  def gen_bracket_at_expr(state) do
    child_state = GrammarTree.decr_depth(state)

    StreamData.frequency([
      # @identifier[key]: @foo[bar]
      {4, gen_bracket_at_identifier(child_state)},
      # @expr.foo[key]: @Mod.foo[bar] (dotted bracket identifier)
      {2, gen_bracket_at_dot_bracket_identifier(child_state)},
      # @(expr)[key]: @(foo)[bar] - less common
      {1, gen_bracket_at_access_expr(child_state)}
    ])
  end

  # Generate @matched_expr.bracket_identifier[key]
  defp gen_bracket_at_dot_bracket_identifier(state) do
    child_state = GrammarTree.decr_depth(state)
    restricted = restrict_unmatched(child_state)

    left_gen =
      if child_state.budget.depth <= 1 do
        StreamData.frequency([
          {2, gen_alias()},
          {1, gen_identifier()}
        ])
      else
        StreamData.frequency([
          {3, gen_alias()},
          {2, gen_identifier()},
          {1, gen_sub_matched_expr(restricted)}
        ])
      end

    StreamData.bind(gen_newlines(), fn newlines ->
      StreamData.bind(left_gen, fn left ->
        StreamData.bind(StreamData.member_of(@identifiers), fn name ->
          StreamData.bind(gen_bracket_arg(state), fn arg ->
            StreamData.constant(
              {:bracket_at_expr, newlines, {:dot_bracket_identifier, left, name}, arg}
            )
          end)
        end)
      end)
    end)
  end

  # Generate @identifier[key]
  defp gen_bracket_at_identifier(state) do
    StreamData.bind(gen_newlines(), fn newlines ->
      StreamData.bind(StreamData.member_of(@identifiers), fn name ->
        StreamData.bind(gen_bracket_arg(state), fn arg ->
          StreamData.constant({:bracket_at_expr, newlines, {:bracket_identifier, name}, arg})
        end)
      end)
    end)
  end

  # Generate @(expr)[key]
  # Per grammar: bracket_at_expr -> at_op_eol access_expr bracket_arg
  # The access_expr can be any of: literals, aliases, fn, parens calls, lists, tuples, etc.
  # We use a subset to avoid deep recursion (excluding bracket_at_expr itself)
  defp gen_bracket_at_access_expr(state) do
    child_state = GrammarTree.decr_depth(state)

    # Generate access_expr variants (excluding bracket_at_expr to avoid recursion)
    expr_gen =
      if child_state.budget.depth <= 1 do
        # At shallow depth, use simple expressions
        gen_simple_expr()
      else
        StreamData.frequency([
          # Simple expressions (literals, identifiers)
          {4, gen_simple_expr()},
          # Aliases: @Mod[key]
          {2, gen_alias()},
          # Lists: @[1,2,3][0]
          {2, gen_list(child_state)},
          # Tuples: @{:ok, val}[0] - note: requires special handling
          {2, gen_tuple(child_state)},
          # Parens calls: @foo()[bar]
          {2, gen_call_parens(child_state)},
          # fn expressions: @(fn -> :ok end)[x]
          {1, gen_fn_single(child_state)},
          # Paren expressions: @(expr)[key]
          {1, gen_paren_expr(child_state)},
          # Dot identifiers: @Mod.foo[bar] (covered by dot_bracket_identifier, but also valid here)
          {1, gen_dot_identifier(child_state)},
          # Bracket expr: @foo[a][b] (nested bracket access)
          {1, gen_bracket_expr(child_state)},
          # Bitstrings: @<<1,2>>[x]
          {1, gen_bitstring(child_state)}
        ])
      end

    StreamData.bind(gen_newlines(), fn newlines ->
      StreamData.bind(expr_gen, fn expr ->
        StreamData.bind(gen_bracket_arg(state), fn arg ->
          StreamData.constant({:bracket_at_expr, newlines, {:expr, expr}, arg})
        end)
      end)
    end)
  end

  # Generate bracket identifier access: foo[bar]
  defp gen_bracket_identifier(state) do
    StreamData.bind(StreamData.member_of(@identifiers), fn name ->
      StreamData.bind(gen_bracket_arg(state), fn arg ->
        StreamData.constant({:bracket_expr, {:bracket_identifier, name}, arg})
      end)
    end)
  end

  # Generate expression bracket access: (expr)[key]
  defp gen_bracket_access_expr(state) do
    child_state = GrammarTree.decr_depth(state)

    expr_gen =
      StreamData.frequency([
        {3, gen_simple_expr()},
        {1, gen_dot_identifier(child_state)}
      ])

    StreamData.bind(expr_gen, fn expr ->
      StreamData.bind(gen_bracket_arg(state), fn arg ->
        StreamData.constant({:bracket_expr, {:expr, expr}, arg})
      end)
    end)
  end

  # Generate bracket argument per grammar lines 307-310:
  # bracket_arg -> open_bracket kw_data close_bracket            (Rule 1)
  # bracket_arg -> open_bracket container_expr close_bracket     (Rule 2)
  # bracket_arg -> open_bracket container_expr ',' close_bracket (Rule 3 - trailing comma)
  # bracket_arg -> open_bracket container_expr ',' container_args close_bracket (Rule 4 - ERROR)
  #
  # Note: Rule 4 is an error case (error_too_many_access_syntax) and is NOT generated.
  #
  # Container_expr per grammar (lines 533-535) can be:
  # - matched_expr
  # - unmatched_expr (e.g., do-blocks, unary ops with unmatched args)
  # - no_parens_expr (error case, not generated)
  defp gen_bracket_arg(state) do
    child_state = GrammarTree.decr_depth(state)
    restricted_state = restrict_unmatched(child_state)

    # Generate a container_expr - can be matched or unmatched
    # Per grammar: container_expr -> matched_expr | unmatched_expr
    container_expr_gen =
      if child_state.budget.depth <= 1 do
        gen_simple_expr()
      else
        StreamData.frequency([
          # matched_expr - most common case
          {8, gen_matched_expr(restricted_state)},
          # unmatched_expr - do-blocks, prefix ops with unmatched args
          # Use lower frequency as these are more complex constructs
          {1, gen_unmatched_expr(restricted_state)}
        ])
      end

    StreamData.frequency([
      # Rule 2: Single container expression: foo[bar], foo[x + y], foo[func()]
      {6, container_expr_gen},
      # Rule 1: Keyword data: foo[a: 1], foo[a: 1, b: 2]
      {2, gen_kw_data(child_state)},
      # Rule 3: Container expr with trailing comma: foo[bar,], foo[x + y,]
      {1,
       StreamData.bind(container_expr_gen, fn expr ->
         StreamData.constant({:trailing, expr})
       end)}
    ])
  end

  # ===========================================================================
  # Bitstring Generators
  # ===========================================================================

  @doc """
  Generate a bitstring: <<elem1, elem2, ...>>

  Per grammar lines 609-611:
  - bitstring -> open_bit '>>'
  - bitstring -> open_bit container_args close_bit
  """
  def gen_bitstring(state) do
    child_state = GrammarTree.decr_depth(state)

    StreamData.frequency([
      # Empty bitstring: <<>>
      {1, StreamData.constant({:bitstring, []})},
      # Bitstring with 1-3 elements
      {4, gen_bitstring_with_elements(child_state, 1, 3)}
    ])
  end

  defp gen_bitstring_with_elements(state, min, max) do
    StreamData.bind(StreamData.integer(min..max), fn count ->
      gen_bitstring_args(state, count)
    end)
    |> StreamData.map(fn args -> {:bitstring, args} end)
  end

  # Generate bitstring arguments - simpler than regular container_args
  # to avoid type specifiers which add complexity
  defp gen_bitstring_args(_state, 0), do: StreamData.constant([])

  defp gen_bitstring_args(state, count) when count > 0 do
    # Bitstring elements are typically integers or other bitstrings
    arg_gen = gen_bitstring_element()

    StreamData.bind(arg_gen, fn arg ->
      StreamData.bind(gen_bitstring_args(GrammarTree.decr_nodes(state), count - 1), fn rest ->
        StreamData.constant([arg | rest])
      end)
    end)
  end

  # Generate a bitstring element - typically integers or simple expressions
  defp gen_bitstring_element do
    StreamData.frequency([
      {5, gen_bitstring_int()},
      {2, StreamData.member_of(@identifiers) |> StreamData.map(&{:identifier, &1})}
      # NOTE: strings inside bitstrings disabled for simplicity
      # {1, StreamData.bind(StreamData.string(:alphanumeric, min_length: 1, max_length: 5), fn s ->
      #   StreamData.constant({:bin_string, s})
      # end)}
    ])
  end

  # Generate a decimal integer for bitstring elements (0-255 typical byte range)
  defp gen_bitstring_int do
    StreamData.integer(0..255)
    |> StreamData.map(fn n ->
      chars = Integer.to_charlist(n)
      {:int, n, :dec, chars}
    end)
  end

  # ===========================================================================
  # String Generators
  # ===========================================================================

  @doc """
  Generate a binary string: "hello"

  Per grammar lines 290-292:
  - access_expr -> bin_string
  - access_expr -> bin_heredoc (TODO)
  """
  def gen_bin_string do
    StreamData.frequency([
      # Simple string without interpolation
      {5, gen_simple_bin_string()},
      # Empty string
      {1, StreamData.constant({:bin_string, ""})}
    ])
  end

  # Generate a simple string (ASCII letters and spaces, no interpolation)
  defp gen_simple_bin_string do
    StreamData.bind(StreamData.string(:alphanumeric, min_length: 1, max_length: 20), fn str ->
      StreamData.constant({:bin_string, str})
    end)
  end

  # ===========================================================================
  # Context Helpers
  # ===========================================================================

  # Restrict context to disallow unmatched expressions
  defp restrict_unmatched(state) do
    %{state | context: %{state.context | allow_unmatched: false}}
  end
end
