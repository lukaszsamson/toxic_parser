defmodule ToxicParser.Generator do
  @moduledoc false

  import StreamData

  @identifiers ~w(foo bar baz qux spam eggs alpha beta gamma delta)a
  @aliases ~w(Foo Bar Baz Qux Remote Mod State Schema Context Config Default MyApp Context Kernel String)a
  @do_identifiers ~w(if case cond unless with for try receive)a

  # Minimal grammar-faithful token generator (starter subset).
  #
  # Generates *Toxic streaming tokens* (with ranged metas) in a way that matches
  # Toxic's lexer behavior for this subset (notably: range_op carries leading EOL
  # count in meta.extra and does not emit a standalone :eol token).

  def tokens_gen(opts \\ []) do
    grammar_raw(opts)
    |> map(&coalesce_eols/1)
    |> map(&materialize/1)
  end

  # ---------------------------------------------------------------------------
  # Grammar (raw token lists)
  # ---------------------------------------------------------------------------

  defp grammar_raw(opts) do
    state = %{
      depth: Keyword.get(opts, :depth, 2),
      max_forms: Keyword.get(opts, :max_forms, 3)
    }

    grammar_state_raw(state)
  end

  defp grammar_state_raw(state) do
    frequency([
      {5, grammar_expr_list_raw(state)},
      {3, grammar_expr_list_eoe_raw(state)},
      {3, grammar_eoe_expr_list_raw(state)},
      {1, grammar_eoe_expr_list_eoe_raw(state)},
      {1, grammar_eoe_only_raw()},
      {1, grammar_empty_raw()}
    ])
  end

  # grammar -> '$empty'
  defp grammar_empty_raw, do: constant([])

  # grammar -> eoe
  defp grammar_eoe_only_raw, do: eoe_raw()

  # grammar -> expr_list
  defp grammar_expr_list_raw(state), do: expr_list_raw(state)

  # grammar -> expr_list eoe
  defp grammar_expr_list_eoe_raw(state) do
    bind(expr_list_raw(state), fn exprs ->
      bind(eoe_raw(), fn trailing ->
        constant(exprs ++ trailing)
      end)
    end)
  end

  # grammar -> eoe expr_list
  defp grammar_eoe_expr_list_raw(state) do
    bind(eoe_raw(), fn leading ->
      map(expr_list_raw(state), fn exprs ->
        attach_leading_eoe(leading, exprs)
      end)
    end)
  end

  # grammar -> eoe expr_list eoe
  defp grammar_eoe_expr_list_eoe_raw(state) do
    bind(eoe_raw(), fn leading ->
      bind(expr_list_raw(state), fn exprs ->
        bind(eoe_raw(), fn trailing ->
          constant(attach_leading_eoe(leading, exprs) ++ trailing)
        end)
      end)
    end)
  end

  defp attach_leading_eoe(leading, exprs) do
    concat_with_eoe([], leading, exprs)
  end

  # expr_list -> expr
  # expr_list -> expr_list eoe expr
  defp expr_list_raw(%{max_forms: max_forms} = state) do
    bind(integer(1..max_forms), fn count ->
      expr_list_n_raw(state, count)
    end)
  end

  defp expr_list_n_raw(state, 1), do: expr_raw(state)

  defp expr_list_n_raw(state, count) when count > 1 do
    bind(expr_list_n_raw(state, count - 1), fn prev ->
      bind(eoe_raw(), fn sep ->
        bind(expr_raw(state), fn last ->
          constant(concat_with_eoe(prev, sep, last))
        end)
      end)
    end)
  end

  # expr -> matched_expr
  # expr -> no_parens_expr
  # expr -> unmatched_expr
  defp expr_raw(state) do
    frequency([
      {8, matched_expr_raw(state)},
      {2, no_parens_expr_raw(state)},
      {2, unmatched_expr_raw(state)}
    ])
  end

  defp expr_no_ternary_prefix_raw(state) do
    frequency([
      {8, matched_expr_no_ternary_prefix_raw(state)},
      {2, no_parens_expr_no_ternary_prefix_raw(state)},
      {2, unmatched_expr_no_ternary_prefix_raw(state)}
    ])
  end

  # matched_expr -> sub_matched_expr
  # matched_expr -> no_parens_one_expr
  defp matched_expr_raw(%{depth: depth} = state) do
    if depth <= 0 do
      sub_matched_expr_raw(state)
    else
      state = decr_depth(state)

      frequency([
        {10, matched_expr_no_binary_raw(state)},
        {3, matched_expr_binary_raw(state)}
      ])
    end
  end

  defp matched_expr_no_binary_raw(state) do
    frequency([
      {12, sub_matched_expr_raw(state)},
      {3, no_parens_one_expr_raw(state)},
      {2,
       bind(unary_op_eol_no_ternary_raw(), fn op ->
         bind(matched_expr_no_ternary_prefix_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)},
      {1,
       bind(ternary_op_eol_raw(), fn op ->
         bind(matched_expr_no_binary_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)},
      {2,
       bind(at_op_eol_raw(), fn op ->
         # `@` only lexes as :at_op when applied to an identifier-ish target.
         bind(map_at_target_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)},
      {2,
       bind(capture_op_eol_raw(), fn op ->
         bind(matched_expr_no_binary_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)},
      {1,
       bind(ellipsis_op_raw(), fn op ->
         bind(matched_expr_no_ternary_prefix_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)}
    ])
  end

  defp matched_expr_binary_raw(state) do
    bind(matched_expr_no_binary_no_nullary_raw(state), fn lhs ->
      bind(matched_op_expr_raw(state), fn rhs ->
        constant(lhs ++ rhs)
      end)
    end)
  end

  defp matched_expr_no_binary_no_nullary_raw(state) do
    frequency([
      {12, sub_matched_expr_no_nullary_raw(state)},
      {3, no_parens_one_expr_raw(state)},
      {2,
       bind(unary_op_eol_no_ternary_raw(), fn op ->
         bind(matched_expr_no_ternary_prefix_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)},
      {1,
       bind(ternary_op_eol_raw(), fn op ->
         bind(matched_expr_no_binary_no_nullary_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)},
      {2,
       bind(at_op_eol_raw(), fn op ->
         bind(map_at_target_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)},
      {2,
       bind(capture_op_eol_raw(), fn op ->
         bind(matched_expr_no_binary_no_nullary_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)},
      {1,
       bind(ellipsis_op_raw(), fn op ->
         bind(matched_expr_no_ternary_prefix_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)}
    ])
  end

  defp matched_op_expr_raw(state) do
    pre = [{:gap_space, 1}]

    frequency([
      {4, bin_op_rhs_raw(pre, match_op_eol_raw(), state)},
      {3, bin_op_rhs_raw(pre, dual_op_eol_raw(), state)},
      {3, bin_op_rhs_raw(pre, mult_op_eol_raw(), state)},
      {2, bin_op_rhs_raw(pre, power_op_eol_raw(), state)},
      {2, bin_op_rhs_raw(pre, concat_op_eol_raw(), state)},
      {2, bin_op_rhs_raw(pre, range_op_eol_raw(), state)},
      {2, bin_op_rhs_raw(pre, ternary_op_eol_raw(), state)},
      {1, bin_op_rhs_raw(pre, xor_op_eol_raw(), state)},
      {1, bin_op_rhs_raw(pre, and_op_eol_raw(), state)},
      {1, bin_op_rhs_raw(pre, or_op_eol_raw(), state)},
      {1, bin_op_rhs_raw(pre, in_op_eol_raw(), state)},
      {1, bin_op_rhs_raw(pre, in_match_op_eol_raw(), state)},
      {1, bin_op_rhs_raw(pre, type_op_eol_raw(), state)},
      {1, bin_op_rhs_raw(pre, when_op_eol_raw(), state)},
      {1, bin_op_rhs_pipe_raw(pre, state)},
      {1, bin_op_rhs_raw(pre, comp_op_eol_raw(), state)},
      {1, bin_op_rhs_raw(pre, rel_op_eol_raw(), state)},
      {1, bin_op_rhs_arrow_raw(pre, state)},
      {1, bin_op_rhs_arrow_no_parens_raw(pre, state)}
    ])
  end

  defp bin_op_rhs_arrow_raw(pre, state) do
    frequency([
      {3,
       bind(arrow_op_no_eol_raw(), fn op ->
         bind(matched_expr_no_ternary_prefix_raw(state), fn rhs ->
           constant(pre ++ op ++ [{:gap_space, 1}] ++ rhs)
         end)
       end)},
      {1,
       bind(arrow_op_no_eol_raw(), fn op ->
         bind(eol_raw(), fn eol ->
           bind(matched_expr_no_binary_raw(state), fn rhs ->
             constant(pre ++ op ++ eol ++ rhs)
           end)
         end)
       end)}
    ])
  end

  defp bin_op_rhs_raw(pre, op_eol_gen, state) do
    bind(op_eol_gen, fn op ->
      rhs_gen =
        case List.last(op) do
          {:eol, _} -> matched_expr_no_binary_raw(state)
          _ -> matched_expr_no_ternary_prefix_raw(state)
        end

      bind(rhs_gen, fn rhs ->
        constant(pre ++ op ++ rhs)
      end)
    end)
  end

  defp bin_op_rhs_pipe_raw(pre, state) do
    frequency([
      {3,
       bind(pipe_op_no_eol_raw(), fn op ->
         bind(matched_expr_no_ternary_prefix_raw(state), fn rhs ->
           constant(pre ++ op ++ [{:gap_space, 1}] ++ rhs)
         end)
       end)},
      {1,
       bind(pipe_op_no_eol_raw(), fn op ->
         bind(eol_raw(), fn eol ->
           bind(matched_expr_no_binary_raw(state), fn rhs ->
             constant(pre ++ op ++ eol ++ rhs)
           end)
         end)
       end)}
    ])
  end

  defp bin_op_rhs_arrow_no_parens_raw(pre, state) do
    bind(arrow_op_eol_raw(), fn op ->
      bind(no_parens_one_expr_raw(state), fn rhs ->
        constant(pre ++ op ++ rhs)
      end)
    end)
  end

  defp matched_expr_no_ternary_prefix_raw(%{depth: depth} = state) do
    if depth <= 0 do
      sub_matched_expr_raw(state)
    else
      state = decr_depth(state)

      frequency([
        {12, sub_matched_expr_raw(state)},
        {3, no_parens_one_expr_raw(state)},
        {2,
         bind(unary_op_eol_no_ternary_raw(), fn op ->
           bind(matched_expr_no_ternary_prefix_raw(state), fn rhs ->
             constant(op ++ rhs)
           end)
         end)},
        {2,
         bind(at_op_eol_raw(), fn op ->
           bind(map_at_target_raw(state), fn rhs ->
             constant(op ++ rhs)
           end)
         end)},
        {2,
         bind(capture_op_eol_raw(), fn op ->
           bind(matched_expr_no_ternary_prefix_raw(state), fn rhs ->
             constant(op ++ rhs)
           end)
         end)},
        {1,
         bind(ellipsis_op_raw(), fn op ->
           bind(matched_expr_no_ternary_prefix_raw(state), fn rhs ->
             constant(op ++ rhs)
           end)
         end)}
      ])
    end
  end

  defp ternary_op_eol_raw do
    frequency([
      {4, constant([{:ternary_op, :"//", 0}, {:gap_space, 1}])},
      {1,
       bind(eol_raw(), fn eol ->
         constant([{:ternary_op, :"//", 0}] ++ eol)
       end)}
    ])
  end

  defp match_op_eol_raw do
    frequency([
      {4, constant([{:match_op, :=, 0}, {:gap_space, 1}])},
      {1,
       bind(eol_raw(), fn eol ->
         constant([{:match_op, :=, 0}] ++ eol)
       end)}
    ])
  end

  defp dual_op_eol_raw do
    bind(member_of([:+, :-]), fn op ->
      frequency([
        {4, constant([{:dual_op, op}, {:gap_space, 1}])},
        {1,
         bind(eol_raw(), fn eol ->
           constant([{:dual_op, op}] ++ eol)
         end)}
      ])
    end)
  end

  defp mult_op_eol_raw do
    bind(member_of([:*, :/]), fn op ->
      frequency([
        {4, constant([{:mult_op, op, 0}, {:gap_space, 1}])},
        {1,
         bind(eol_raw(), fn eol ->
           constant([{:mult_op, op, 0}] ++ eol)
         end)}
      ])
    end)
  end

  defp power_op_eol_raw do
    frequency([
      {4, constant([{:power_op, :**, 0}, {:gap_space, 1}])},
      {1,
       bind(eol_raw(), fn eol ->
         constant([{:power_op, :**, 0}] ++ eol)
       end)}
    ])
  end

  defp concat_op_eol_raw do
    bind(member_of([:++, :--, :+++, :---, :<>]), fn op ->
      frequency([
        {4, constant([{:concat_op, op, 0}, {:gap_space, 1}])},
        {1,
         bind(eol_raw(), fn eol ->
           constant([{:concat_op, op, 0}] ++ eol)
         end)}
      ])
    end)
  end

  defp range_op_eol_raw do
    # range_op carries leading EOL count in meta.extra
    frequency([
      {4, constant([{:range_op, :.., 0}, {:gap_space, 1}])},
      {1,
       bind(eol_raw(), fn eol ->
         constant([{:range_op, :.., 0}] ++ eol)
       end)}
    ])
  end

  defp xor_op_eol_raw do
    frequency([
      {4, constant([{:xor_op, :"^^^", 0}, {:gap_space, 1}])},
      {1,
       bind(eol_raw(), fn eol ->
         constant([{:xor_op, :"^^^", 0}] ++ eol)
       end)}
    ])
  end

  defp and_op_eol_raw do
    bind(member_of([:and, :&&, :&&&]), fn op ->
      frequency([
        {4, constant([{:and_op, op, 0}, {:gap_space, 1}])},
        {1,
         bind(eol_raw(), fn eol ->
           constant([{:and_op, op, 0}] ++ eol)
         end)}
      ])
    end)
  end

  defp or_op_eol_raw do
    bind(member_of([:or, :||, :|||]), fn op ->
      frequency([
        {4, constant([{:or_op, op, 0}, {:gap_space, 1}])},
        {1,
         bind(eol_raw(), fn eol ->
           constant([{:or_op, op, 0}] ++ eol)
         end)}
      ])
    end)
  end

  defp in_op_eol_raw do
    bind(member_of([:in, :"not in"]), fn op ->
      frequency([
        {4, constant([{:in_op, op, 0}, {:gap_space, 1}])},
        {1,
         bind(eol_raw(), fn eol ->
           constant([{:in_op, op, 0}] ++ eol)
         end)}
      ])
    end)
  end

  defp in_match_op_eol_raw do
    bind(member_of([:<-, :"\\\\"]), fn op ->
      frequency([
        {4, constant([{:in_match_op, op, 0}, {:gap_space, 1}])},
        {1,
         bind(eol_raw(), fn eol ->
           constant([{:in_match_op, op, 0}] ++ eol)
         end)}
      ])
    end)
  end

  defp type_op_eol_raw do
    frequency([
      {4, constant([{:type_op, :"::", 0}, {:gap_space, 1}])},
      {1,
       bind(eol_raw(), fn eol ->
         constant([{:type_op, :"::", 0}] ++ eol)
       end)}
    ])
  end

  defp when_op_eol_raw do
    frequency([
      {4, constant([{:when_op, :when, 0}, {:gap_space, 1}])},
      {1,
       bind(eol_raw(), fn eol ->
         constant([{:when_op, :when, 0}] ++ eol)
       end)}
    ])
  end

  defp pipe_op_no_eol_raw, do: pipe_op_raw()

  defp arrow_op_no_eol_raw do
    bind(member_of([:|>, :<<<, :>>>, :<<~, :~>>, :<~, :~>, :<~>, :<|>, :"<|>"]), fn op ->
      constant([{:arrow_op, op, 0}])
    end)
  end

  defp comp_op_eol_raw do
    bind(member_of([:==, :!=, :=~, :===, :!==]), fn op ->
      frequency([
        {4, constant([{:comp_op, op, 0}, {:gap_space, 1}])},
        {1,
         bind(eol_raw(), fn eol ->
           constant([{:comp_op, op, 0}] ++ eol)
         end)}
      ])
    end)
  end

  defp rel_op_eol_raw do
    bind(member_of([:<, :>, :<=, :>=]), fn op ->
      frequency([
        {4, constant([{:rel_op, op, 0}, {:gap_space, 1}])},
        {1,
         bind(eol_raw(), fn eol ->
           constant([{:rel_op, op, 0}] ++ eol)
         end)}
      ])
    end)
  end

  defp arrow_op_eol_raw do
    bind(member_of([:|>, :<<<, :>>>, :<<~, :~>>, :<~, :~>, :<~>, :<|>, :"<|>"]), fn op ->
      frequency([
        {4, constant([{:arrow_op, op, 0}, {:gap_space, 1}])},
        {1,
         bind(eol_raw(), fn eol ->
           constant([{:arrow_op, op, 0}] ++ eol)
         end)}
      ])
    end)
  end

  # sub_matched_expr -> no_parens_zero_expr
  # sub_matched_expr -> range_op
  # sub_matched_expr -> ellipsis_op
  # sub_matched_expr -> access_expr
  # (sub_matched_expr -> access_expr kw_identifier) intentionally NOT generated (would be error)
  defp sub_matched_expr_raw(%{depth: depth} = state) do
    if depth <= 0 do
      no_parens_zero_expr_raw(state)
    else
      frequency([
        {10, no_parens_zero_expr_raw(state)},
        {1, range_op_raw()},
        {1, ellipsis_op_raw()},
        {4, access_expr_raw(decr_depth(state))}
      ])
    end
  end

  defp sub_matched_expr_no_nullary_raw(%{depth: depth} = state) do
    if depth <= 0 do
      no_parens_zero_expr_raw(state)
    else
      frequency([
        {10, no_parens_zero_expr_raw(state)},
        {4, access_expr_raw(decr_depth(state))}
      ])
    end
  end

  # eoe -> eol | ';' | eol ';'
  defp eoe_raw do
    frequency([
      {6, eol_raw()},
      {5, semi_raw()},
      {4, eol_semi_raw()}
    ])
  end

  defp eol_raw do
    integer(1..2)
    |> map(fn n -> [{:eol, n}] end)
  end

  defp semi_raw do
    constant([:semi])
  end

  defp eol_semi_raw do
    bind(eol_raw(), fn eol_tokens ->
      map(semi_raw(), fn semi_tokens -> eol_tokens ++ semi_tokens end)
    end)
  end

  # no_parens_zero_expr -> dot_do_identifier | dot_identifier
  defp no_parens_zero_expr_raw(state) do
    frequency([
      {2, dot_do_identifier_raw(state)},
      {8, dot_identifier_raw(state)}
    ])
  end

  # no_parens_one_expr -> dot_op_identifier call_args_no_parens_one
  # no_parens_one_expr -> dot_identifier call_args_no_parens_one
  defp no_parens_one_expr_raw(state) do
    frequency([
      {3,
       bind(dot_identifier_raw(state), fn fun ->
         bind(call_args_no_parens_one_raw(state), fn args ->
           constant(fun ++ [{:gap_space, 1}] ++ args)
         end)
       end)},
      {2,
       bind(dot_op_identifier_raw(state), fn fun ->
         bind(call_args_no_parens_one_for_op_identifier_raw(state), fn args ->
           constant(fun ++ [{:gap_space, 1}] ++ args)
         end)
       end)}
    ])
  end

  # dot_op_identifier -> op_identifier | matched_expr dot_op op_identifier
  defp dot_op_identifier_raw(state) do
    frequency([
      {5, op_identifier_raw()},
      {2,
       bind(matched_expr_raw(decr_depth(state)), fn lhs ->
         bind(dot_op_raw(), fn dot ->
           bind(op_identifier_raw(), fn rhs ->
             constant(lhs ++ dot ++ rhs)
           end)
         end)
       end)}
    ])
  end

  defp op_identifier_raw do
    member_of(@identifiers)
    |> map(fn atom -> [{:op_identifier, atom}] end)
  end

  # call_args_no_parens_one -> call_args_no_parens_kw | matched_expr
  defp call_args_no_parens_one_raw(state) do
    frequency([
      {2, call_args_no_parens_kw_raw(state)},
      {4, matched_expr_raw(decr_depth(state))}
    ])
  end

  # For op_identifier calls, keep RHS from starting with `//` and keep unary operators tight
  # so the lexer re-emits :op_identifier (e.g. `foo +1`).
  defp call_args_no_parens_one_for_op_identifier_raw(state), do: unary_tight_arg_raw(state)

  defp unary_tight_arg_raw(_state) do
    bind(member_of([:+, :-]), fn op ->
      bind(frequency([{3, int_raw()}, {1, identifier_raw()}]), fn rhs ->
        constant([{:dual_op, op}] ++ rhs)
      end)
    end)
  end

  # call_args_no_parens_kw_expr -> kw_eol matched_expr | kw_eol no_parens_expr
  defp call_args_no_parens_kw_expr_raw(state) do
    bind(kw_eol_raw(), fn kw ->
      frequency([
        {4,
         bind(matched_expr_raw(decr_depth(state)), fn expr ->
           constant(kw ++ expr)
         end)},
        {1,
         bind(no_parens_expr_raw(decr_depth(state)), fn expr ->
           constant(kw ++ expr)
         end)}
      ])
    end)
  end

  # call_args_no_parens_kw -> kw_expr | kw_expr ',' call_args_no_parens_kw
  defp call_args_no_parens_kw_raw(state) do
    max = min(state.max_forms, 3)

    bind(integer(1..max), fn count ->
      call_args_no_parens_kw_n_raw(state, count)
    end)
  end

  defp call_args_no_parens_kw_n_raw(state, 1), do: call_args_no_parens_kw_expr_raw(state)

  defp call_args_no_parens_kw_n_raw(state, count) when count > 1 do
    bind(call_args_no_parens_kw_expr_raw(state), fn first ->
      bind(call_args_no_parens_kw_n_raw(state, count - 1), fn rest ->
        constant(first ++ [:comma, {:gap_space, 1}] ++ rest)
      end)
    end)
  end

  # call_args_no_parens_all -> call_args_no_parens_one | call_args_no_parens_ambig | call_args_no_parens_many
  defp call_args_no_parens_all_raw(state) do
    frequency([
      {5, call_args_no_parens_one_raw(state)},
      {1, call_args_no_parens_ambig_raw(state)},
      {3, call_args_no_parens_many_raw(state)}
    ])
  end

  # call_args_no_parens_ambig -> no_parens_expr
  # call_args_no_parens_ambig -> no_parens_expr
  defp call_args_no_parens_ambig_raw(state), do: no_parens_expr_raw(decr_depth(state))

  # call_args_no_parens_many_strict -> call_args_no_parens_many
  # (error-producing strict-parens variants intentionally not generated)
  defp call_args_no_parens_many_strict_raw(state), do: call_args_no_parens_many_raw(state)

  # For :op_identifier stability, ensure first arg is a tight unary (+1/-1) form.
  defp call_args_no_parens_ambig_for_op_identifier_raw(state), do: unary_tight_arg_raw(state)

  defp call_args_no_parens_many_strict_for_op_identifier_raw(state) do
    bind(unary_tight_arg_raw(state), fn first ->
      bind(call_args_no_parens_kw_raw(state), fn kw ->
        constant(first ++ [:comma, {:gap_space, 1}] ++ kw)
      end)
    end)
  end

  # call_args_no_parens_expr (minimal) -> matched_expr
  defp call_args_no_parens_expr_raw(state), do: matched_expr_raw(decr_depth(state))

  # call_args_no_parens_comma_expr -> matched_expr ',' call_args_no_parens_expr
  # call_args_no_parens_comma_expr -> call_args_no_parens_comma_expr ',' call_args_no_parens_expr
  defp call_args_no_parens_comma_expr_raw(state) do
    max = min(state.max_forms, 3)

    bind(integer(2..max), fn count ->
      call_args_no_parens_comma_expr_n_raw(state, count)
    end)
  end

  defp call_args_no_parens_comma_expr_n_raw(state, 2) do
    bind(matched_expr_raw(decr_depth(state)), fn first ->
      bind(call_args_no_parens_expr_raw(state), fn second ->
        constant(first ++ [:comma, {:gap_space, 1}] ++ second)
      end)
    end)
  end

  defp call_args_no_parens_comma_expr_n_raw(state, count) when count > 2 do
    bind(call_args_no_parens_comma_expr_n_raw(state, count - 1), fn prev ->
      bind(call_args_no_parens_expr_raw(state), fn last ->
        constant(prev ++ [:comma, {:gap_space, 1}] ++ last)
      end)
    end)
  end

  # call_args_no_parens_many -> matched_expr ',' call_args_no_parens_kw
  # call_args_no_parens_many -> call_args_no_parens_comma_expr
  # call_args_no_parens_many -> call_args_no_parens_comma_expr ',' call_args_no_parens_kw
  defp call_args_no_parens_many_raw(state) do
    frequency([
      {2,
       bind(matched_expr_raw(decr_depth(state)), fn first ->
         bind(call_args_no_parens_kw_raw(state), fn kw ->
           constant(first ++ [:comma, {:gap_space, 1}] ++ kw)
         end)
       end)},
      {4, call_args_no_parens_comma_expr_raw(state)},
      {2,
       bind(call_args_no_parens_comma_expr_raw(state), fn exprs ->
         bind(call_args_no_parens_kw_raw(state), fn kw ->
           constant(exprs ++ [:comma, {:gap_space, 1}] ++ kw)
         end)
       end)}
    ])
  end

  # stab_parens_many -> open_paren call_args_no_parens_kw close_paren
  # stab_parens_many -> open_paren call_args_no_parens_many close_paren
  defp stab_parens_many_raw(state) do
    frequency([
      {3,
       bind(open_paren_raw(), fn open ->
         bind(call_args_no_parens_kw_raw(state), fn kw ->
           bind(close_paren_raw(), fn close ->
             constant(open ++ kw ++ close)
           end)
         end)
       end)},
      {2,
       bind(open_paren_raw(), fn open ->
         bind(call_args_no_parens_many_raw(state), fn many ->
           bind(close_paren_raw(), fn close ->
             constant(open ++ many ++ close)
           end)
         end)
       end)}
    ])
  end

  # dot_identifier -> identifier | matched_expr dot_op identifier
  defp dot_identifier_raw(state) do
    frequency([
      {5, identifier_raw()},
      {2,
       bind(dot_lhs_raw(state), fn lhs ->
         bind(dot_op_raw(), fn dot ->
           bind(identifier_raw(), fn rhs ->
             constant(lhs ++ dot ++ rhs)
           end)
         end)
       end)}
    ])
  end

  # dot_do_identifier -> do_identifier | matched_expr dot_op do_identifier
  defp dot_do_identifier_raw(state) do
    frequency([
      {3, do_identifier_raw()},
      {1,
       bind(dot_lhs_raw(state), fn lhs ->
         bind(dot_op_raw(), fn dot ->
           bind(do_identifier_raw(), fn rhs ->
             constant(lhs ++ dot ++ rhs)
           end)
         end)
       end)}
    ])
  end

  # Minimal lhs for dotted identifiers.
  defp dot_lhs_raw(state) do
    frequency([
      {5, identifier_raw()},
      {1, parens_raw(state)}
    ])
  end

  defp do_identifier_raw do
    # NOTE: these lex as :identifier unless used in a do-block context.
    member_of(@do_identifiers)
    |> map(fn atom -> [{:identifier, atom}] end)
  end

  # dot_op -> '.' | '.' eol
  # NOTE: Toxic does NOT emit a standalone :eol token after '.', it only shifts the
  # next token's position to the next line.
  defp dot_op_raw do
    frequency([
      {4, constant([:dot])},
      {1,
       integer(1..2)
       |> map(fn n -> [:dot, {:gap_eol, n}] end)}
    ])
  end

  defp identifier_raw do
    member_of(@identifiers)
    |> map(fn atom -> [{:identifier, atom}] end)
  end

  # Range op tokens in Toxic carry *leading* EOL count in meta.extra and do not emit a standalone
  # :eol token. We represent it as {:range_op, :.., leading_eol_count}.
  defp range_op_raw, do: constant([{:range_op, :.., 0}])

  defp ellipsis_op_raw, do: constant([{:ellipsis_op, :...}, {:gap_space, 1}])

  # Minimal subset of access_expr (yrl has many more).
  # Focus: bracket_at_expr, bracket_expr, capture_int int, flt, char, atom, dot_alias,
  # list/tuple/bitstring.
  defp access_expr_raw(state) do
    frequency([
      {5, empty_paren_raw()},
      {1, fn_stab_raw(state)},
      {1, paren_stab_raw(state)},
      {3, list_raw(state)},
      {2, tuple_raw(state)},
      {2, bitstring_raw(state)},
      {2, map_raw(state)},
      {3, parens_call_raw(state)},
      {3, dot_alias_raw(state)},
      {4, bracket_expr_raw(state)},
      {2, bracket_at_expr_raw(state)},
      {2, capture_int_int_raw()},
      {3, int_raw()},
      {2, flt_raw()},
      {2, char_raw()},
      {2, atom_raw()},
      {1, true_raw()},
      {1, false_raw()},
      {1, nil_raw()},
      {2, parens_raw(state)}
    ])
  end

  defp empty_paren_raw, do: constant([:lparen, :rparen])

  defp open_bracket_raw do
    frequency([
      {3, constant([:lbracket])},
      {1,
       bind(eol_raw(), fn eol ->
         constant([:lbracket] ++ eol)
       end)}
    ])
  end

  defp close_bracket_raw do
    frequency([
      {3, constant([{:rbracket, nil}])},
      {1,
       bind(eol_raw(), fn [{:eol, _n}] = eol ->
         constant(eol ++ [{:rbracket, nil}])
       end)}
    ])
  end

  # fn_eoe -> 'fn' | 'fn' eoe
  defp fn_eoe_raw do
    frequency([
      {4, constant([:fn, {:gap_space, 1}])},
      {1,
       bind(eoe_raw(), fn eoe ->
         constant([:fn] ++ eoe)
       end)}
    ])
  end

  # do_eoe -> 'do' | 'do' eoe
  defp do_eoe_raw do
    frequency([
      {4, constant([:do, {:gap_space, 1}])},
      {1,
       bind(eoe_raw(), fn eoe ->
         constant([:do] ++ eoe)
       end)}
    ])
  end

  # do_block -> do_eoe 'end'
  # do_block -> do_eoe stab_eoe 'end'
  # do_block -> do_eoe block_list 'end'
  # do_block -> do_eoe stab_eoe block_list 'end'
  defp do_block_raw(state) do
    state = decr_depth(state)

    bind(do_eoe_raw(), fn do_kw ->
      frequency([
        {3, constant(do_kw ++ [:end])},
        {3,
         bind(stab_eoe_raw(state), fn stab ->
           constant(do_kw ++ stab ++ [{:gap_space, 1}, :end])
         end)},
        {2,
         bind(block_list_raw(state), fn blocks ->
           constant(do_kw ++ blocks ++ [{:gap_space, 1}, :end])
         end)},
        {1,
         bind(stab_eoe_raw(state), fn stab ->
           bind(block_list_raw(state), fn blocks ->
             constant(do_kw ++ stab ++ [{:gap_space, 1}] ++ blocks ++ [{:gap_space, 1}, :end])
           end)
         end)}
      ])
    end)
  end

  # block_eoe -> block_identifier | block_identifier eoe
  defp block_eoe_raw do
    bind(block_identifier_raw(), fn block_id ->
      frequency([
        {3, constant(block_id)},
        {1,
         bind(eoe_raw(), fn eoe ->
           constant(block_id ++ eoe)
         end)}
      ])
    end)
  end

  # block_item -> block_eoe stab_eoe | block_eoe
  defp block_item_raw(state) do
    bind(block_eoe_raw(), fn label ->
      frequency([
        {3,
         bind(stab_eoe_raw(state), fn stab ->
           constant(label ++ [{:gap_space, 1}] ++ stab)
         end)},
        {1, constant(label)}
      ])
    end)
  end

  # block_list -> block_item | block_item block_list
  defp block_list_raw(state) do
    max = min(state.max_forms, 2)

    bind(integer(1..max), fn count ->
      block_list_n_raw(state, count)
    end)
  end

  defp block_list_n_raw(state, 1), do: block_item_raw(state)

  defp block_list_n_raw(state, count) when count > 1 do
    bind(block_item_raw(state), fn head ->
      bind(block_list_n_raw(state, count - 1), fn tail ->
        constant(head ++ [{:gap_space, 1}] ++ tail)
      end)
    end)
  end

  defp block_identifier_raw do
    member_of([:after, :else, :catch, :rescue])
    |> map(fn atom -> [{:block_identifier, atom}] end)
  end

  # block_expr -> dot_call_identifier call_args_parens do_block
  # block_expr -> dot_call_identifier call_args_parens call_args_parens do_block
  # block_expr -> dot_do_identifier do_block
  # block_expr -> dot_op_identifier call_args_no_parens_all do_block
  # block_expr -> dot_identifier call_args_no_parens_all do_block
  defp block_expr_raw(state) do
    state = decr_depth(state)

    frequency([
      {3,
       bind(dot_call_identifier_raw(state), fn fun ->
         bind(call_args_parens_raw(state), fn args ->
           bind(do_block_raw(state), fn block ->
             constant(fun ++ args ++ [{:gap_space, 1}] ++ block)
           end)
         end)
       end)},
      {1,
       bind(dot_call_identifier_raw(state), fn fun ->
         bind(call_args_parens_raw(state), fn args1 ->
           bind(call_args_parens_raw(state), fn args2 ->
             bind(do_block_raw(state), fn block ->
               constant(fun ++ args1 ++ args2 ++ [{:gap_space, 1}] ++ block)
             end)
           end)
         end)
       end)},
      {2,
       bind(dot_do_identifier_raw(state), fn fun ->
         bind(do_block_raw(state), fn block ->
           constant(fun ++ [{:gap_space, 1}] ++ block)
         end)
       end)},
      {2,
       bind(dot_op_identifier_raw(state), fn fun ->
         bind(call_args_no_parens_all_raw(state), fn args ->
           bind(do_block_raw(state), fn block ->
             constant(fun ++ [{:gap_space, 1}] ++ args ++ [{:gap_space, 1}] ++ block)
           end)
         end)
       end)},
      {3,
       bind(dot_identifier_raw(state), fn fun ->
         bind(call_args_no_parens_all_raw(state), fn args ->
           bind(do_block_raw(state), fn block ->
             constant(fun ++ [{:gap_space, 1}] ++ args ++ [{:gap_space, 1}] ++ block)
           end)
         end)
       end)}
    ])
  end

  # stab_expr has multiple forms in yrl; implement a small subset.
  defp stab_expr_raw(state) do
    state = decr_depth(state)

    frequency([
      {6, expr_raw(state)},
      {4, stab_op_eol_and_expr_raw(state)},
      {2,
       bind(empty_paren_raw(), fn parens ->
         bind(stab_op_eol_and_expr_raw(state), fn op_and_rhs ->
           constant(parens ++ [{:gap_space, 1}] ++ op_and_rhs)
         end)
       end)},
      {1,
       bind(empty_paren_raw(), fn parens ->
         bind(expr_raw(state), fn when_expr ->
           bind(stab_op_eol_and_expr_raw(state), fn op_and_rhs ->
             constant(parens ++ [{:gap_space, 1}] ++ when_op_raw() ++ when_expr ++ [{:gap_space, 1}] ++ op_and_rhs)
           end)
         end)
       end)},
      {2,
       bind(call_args_no_parens_all_raw(state), fn args ->
         bind(stab_op_eol_and_expr_raw(state), fn op_and_rhs ->
           constant(args ++ [{:gap_space, 1}] ++ op_and_rhs)
         end)
       end)},
      {2,
       bind(stab_parens_many_raw(state), fn parens_many ->
         bind(stab_op_eol_and_expr_raw(state), fn op_and_rhs ->
           constant(parens_many ++ [{:gap_space, 1}] ++ op_and_rhs)
         end)
       end)},
      {1,
       bind(stab_parens_many_raw(state), fn parens_many ->
         bind(expr_raw(state), fn when_expr ->
           bind(stab_op_eol_and_expr_raw(state), fn op_and_rhs ->
             constant(parens_many ++ [{:gap_space, 1}] ++ when_op_raw() ++ when_expr ++ [{:gap_space, 1}] ++ op_and_rhs)
           end)
         end)
       end)},
      {1, stab_op_eol_raw()}
    ])
  end

  # stab_op_eol -> stab_op | stab_op eol
  defp stab_op_eol_raw do
    frequency([
      {4, constant([{:stab_op, :->, 0}, {:gap_space, 1}])},
      {1,
       bind(eol_raw(), fn eol ->
         constant([{:stab_op, :->, 0}] ++ eol)
       end)}
    ])
  end

  defp when_op_raw, do: [{:when_op, :when, 0}, {:gap_space, 1}]

  # stab_op_eol_and_expr -> stab_op_eol expr
  defp stab_op_eol_and_expr_raw(state) do
    bind(stab_op_eol_raw(), fn op ->
      bind(expr_raw(state), fn rhs ->
        constant(op ++ rhs)
      end)
    end)
  end

  # stab -> stab_expr | stab eoe stab_expr
  defp stab_raw(state) do
    max = min(state.max_forms, 3)

    bind(integer(1..max), fn count ->
      stab_n_raw(state, count)
    end)
  end

  defp stab_n_raw(state, 1), do: stab_expr_raw(state)

  defp stab_n_raw(state, count) when count > 1 do
    bind(stab_n_raw(state, count - 1), fn prev ->
      bind(eoe_raw(), fn sep ->
        bind(stab_expr_raw(state), fn last ->
          constant(prev ++ sep ++ last)
        end)
      end)
    end)
  end

  # stab_eoe -> stab | stab eoe
  defp stab_eoe_raw(state) do
    bind(stab_raw(state), fn stab ->
      frequency([
        {4, constant(stab)},
        {1,
         bind(eoe_raw(), fn eoe ->
           constant(stab ++ eoe)
         end)}
      ])
    end)
  end

  # access_expr -> fn_eoe stab_eoe 'end'
  defp fn_stab_raw(state) do
    state = decr_depth(state)

    bind(fn_eoe_raw(), fn fn_kw ->
      bind(stab_eoe_raw(state), fn stab ->
        # Ensure separation before `end` even when stab_eoe doesn't end with eoe.
        constant(fn_kw ++ stab ++ [{:gap_space, 1}, :end])
      end)
    end)
  end

  # access_expr -> open_paren stab_eoe ')'
  # access_expr -> open_paren ';' stab_eoe ')'
  # access_expr -> open_paren ';' close_paren
  defp paren_stab_raw(state) do
    state = decr_depth(state)

    frequency([
      {4,
       bind(open_paren_raw(), fn open ->
         bind(stab_eoe_raw(state), fn stab ->
           bind(close_paren_raw(), fn close ->
             constant(open ++ stab ++ close)
           end)
         end)
       end)},
      {2,
       bind(open_paren_raw(), fn open ->
         bind(semi_raw(), fn semi ->
           bind(stab_eoe_raw(state), fn stab ->
             bind(close_paren_raw(), fn close ->
               constant(open ++ semi ++ stab ++ close)
             end)
           end)
         end)
       end)},
      {1,
       bind(open_paren_raw(), fn open ->
         bind(semi_raw(), fn semi ->
           bind(close_paren_raw(), fn close ->
             constant(open ++ semi ++ close)
           end)
         end)
       end)}
    ])
  end

  defp open_bit_raw do
    frequency([
      {3, constant([:open_bit, {:gap_space, 1}])},
      {1,
       bind(eol_raw(), fn eol ->
         constant([:open_bit] ++ eol)
       end)}
    ])
  end

  defp close_bit_raw do
    frequency([
      {3, constant([{:close_bit, nil}])},
      {1,
       bind(eol_raw(), fn [{:eol, _n}] = eol ->
         constant(eol ++ [{:close_bit, nil}])
       end)}
    ])
  end

  # list_args -> kw_data
  # list_args -> container_args_base
  # list_args -> container_args_base ','
  # list_args -> container_args_base ',' kw_data
  defp list_args_raw(state) do
    frequency([
      {2, kw_data_raw(state)},
      {5,
       bind(container_args_base_raw(state), fn base ->
         frequency([
           {5, constant(base)},
           {2, constant(base ++ [:comma])},
           {2,
            bind(kw_data_raw(state), fn kw ->
              constant(base ++ [:comma] ++ kw)
            end)}
         ])
       end)}
    ])
  end

  # list -> open_bracket ']'
  # list -> open_bracket list_args close_bracket
  defp list_raw(state) do
    frequency([
      {2,
       bind(open_bracket_raw(), fn open ->
         bind(close_bracket_raw(), fn close ->
           constant(open ++ close)
         end)
       end)},
      {5,
       bind(open_bracket_raw(), fn open ->
         bind(list_args_raw(state), fn args ->
           bind(close_bracket_raw(), fn close ->
             constant(open ++ args ++ close)
           end)
         end)
       end)}
    ])
  end

  # tuple -> open_curly '}'
  # tuple -> open_curly container_args close_curly
  defp tuple_raw(state) do
    frequency([
      {2,
       bind(open_curly_raw(), fn open ->
         bind(close_curly_raw(), fn close ->
           constant(open ++ close)
         end)
       end)},
      {5,
       bind(open_curly_raw(), fn open ->
         bind(container_args_raw(state), fn args ->
           bind(close_curly_raw(), fn close ->
             constant(open ++ args ++ close)
           end)
         end)
       end)}
    ])
  end

  # bitstring -> open_bit '>>'
  # bitstring -> open_bit container_args close_bit
  defp bitstring_raw(state) do
    frequency([
      {2,
       bind(open_bit_raw(), fn open ->
         bind(close_bit_raw(), fn close ->
           constant(open ++ close)
         end)
       end)},
      {5,
       bind(open_bit_raw(), fn open ->
         bind(container_args_raw(state), fn args ->
           bind(close_bit_raw(), fn close ->
             constant(open ++ args ++ close)
           end)
         end)
       end)}
    ])
  end

  # map -> map_op map_args
  # map -> '%' map_base_expr map_args
  # map -> '%' map_base_expr eol map_args
  defp map_raw(state) do
    frequency([
      {6,
       bind(map_op_raw(), fn op ->
         bind(map_args_raw(state), fn args ->
           constant(op ++ args)
         end)
       end)},
      {2,
       bind(map_struct_base_expr_raw(state), fn base ->
         bind(map_args_raw(state), fn args ->
           constant([:percent] ++ base ++ args)
         end)
       end)},
      {1,
       bind(map_struct_base_expr_raw(state), fn base ->
         bind(eol_raw(), fn eol ->
           bind(map_args_raw(state), fn args ->
             constant([:percent] ++ base ++ eol ++ args)
           end)
         end)
       end)}
    ])
  end

  defp map_op_raw do
    # Toxic emits %{} token plus a separate '{' token.
    constant([:map_op])
  end

  # In strict lexing, `%` only works when followed by an identifier/alias base.
  # Also, `@` only lexes as :at_op when it's applied to a non-operator target.
  defp map_struct_base_expr_raw(_state) do
    frequency([
      {6, map_struct_base_atom_raw()},
      {2,
       bind(at_op_eol_raw(), fn at ->
         bind(map_struct_base_atom_raw(), fn expr ->
           constant(at ++ expr)
         end)
       end)},
      {2,
       bind(unary_op_eol_raw(), fn op ->
         bind(map_struct_base_atom_raw(), fn expr ->
           constant(op ++ expr)
         end)
       end)},
      {1,
       bind(ellipsis_op_raw(), fn op ->
         bind(map_struct_base_atom_raw(), fn expr ->
           constant(op ++ expr)
         end)
       end)}
    ])
  end

  defp map_struct_base_atom_raw do
    frequency([
      {2, alias_raw()},
      {4, identifier_raw()}
    ])
  end

  # map_base_expr -> sub_matched_expr
  # map_base_expr -> at_op_eol map_base_expr
  # map_base_expr -> unary_op_eol map_base_expr
  # map_base_expr -> ellipsis_op map_base_expr
  defp map_base_expr_raw(state) do
    frequency([
      {6, sub_matched_expr_raw(state)},
      {2,
       bind(at_op_eol_raw(), fn at ->
         bind(map_at_target_raw(state), fn expr ->
           constant(at ++ expr)
         end)
       end)},
      {2,
       bind(unary_op_eol_raw(), fn op ->
         bind(map_at_target_raw(state), fn expr ->
           constant(op ++ expr)
         end)
       end)},
      {1,
       bind(ellipsis_op_raw(), fn op ->
         bind(map_at_target_raw(state), fn expr ->
           constant(op ++ expr)
         end)
       end)}
    ])
  end

  defp map_at_target_raw(state) do
    frequency([
      {6, identifier_raw()},
      {2, alias_raw()},
      {2, dot_alias_raw(state)}
    ])
  end

  defp at_op_eol_raw do
    frequency([
      {4, constant([:at, {:gap_space, 1}])},
      {1,
       bind(eol_raw(), fn eol ->
         constant([:at] ++ eol)
       end)}
    ])
  end

  # capture_op_eol -> capture_op | capture_op eol
  defp capture_op_eol_raw do
    frequency([
      {4, constant([:capture_op, {:gap_space, 1}])},
      {1,
       bind(eol_raw(), fn eol ->
         constant([:capture_op] ++ eol)
       end)}
    ])
  end

  defp unary_op_eol_raw do
    op_raw =
      frequency([
        {4, constant([{:unary_op, :!}])},
        {1, constant([{:unary_op, :^}])},
        {2, constant([{:unary_op, :not}])},
        {1, constant([{:unary_op, :"~~~"}])},
        {2, constant([{:dual_op, :+}])},
        {2, constant([{:dual_op, :-}])},
        {1, constant([{:ternary_op, :"//", 0}])}
      ])

    bind(op_raw, fn op ->
      frequency([
        # Ensure we don't accidentally glue operators into different tokens (e.g. "!//").
        {4, constant(op ++ [{:gap_space, 1}])},
        {1,
         bind(eol_raw(), fn eol ->
           constant(op ++ eol)
         end)}
      ])
    end)
  end

  # assoc_op_eol -> assoc_op | assoc_op eol
  defp assoc_op_eol_raw do
    bind(assoc_op_raw(), fn op ->
      frequency([
        {4, constant(op ++ [{:gap_space, 1}])},
        {1,
         bind(eol_raw(), fn eol ->
           constant(op ++ eol)
         end)}
      ])
    end)
  end

  defp assoc_op_raw, do: constant([{:assoc_op, :"=>", 0}])

  # assoc_expr -> matched_expr assoc_op_eol matched_expr
  # assoc_expr -> unmatched_expr assoc_op_eol unmatched_expr
  # assoc_expr -> matched_expr assoc_op_eol unmatched_expr
  # assoc_expr -> unmatched_expr assoc_op_eol matched_expr
  # assoc_expr -> map_base_expr
  defp assoc_expr_raw(state) do
    frequency([
      {4, map_base_expr_raw(state)},
      # matched_expr assoc_op_eol matched_expr
      {2,
       bind(matched_expr_raw(decr_depth(state)), fn lhs ->
         bind(assoc_op_eol_raw(), fn op ->
           bind(matched_expr_raw(decr_depth(state)), fn rhs ->
             constant(lhs ++ op ++ rhs)
           end)
         end)
       end)},
      # unmatched_expr assoc_op_eol unmatched_expr
      {1,
       bind(unmatched_expr_raw(decr_depth(state)), fn lhs ->
         bind(assoc_op_eol_raw(), fn op ->
           bind(unmatched_expr_raw(decr_depth(state)), fn rhs ->
             constant(lhs ++ op ++ rhs)
           end)
         end)
       end)},
      # matched_expr assoc_op_eol unmatched_expr
      {1,
       bind(matched_expr_raw(decr_depth(state)), fn lhs ->
         bind(assoc_op_eol_raw(), fn op ->
           bind(unmatched_expr_raw(decr_depth(state)), fn rhs ->
             constant(lhs ++ op ++ rhs)
           end)
         end)
       end)},
      # unmatched_expr assoc_op_eol matched_expr
      {1,
       bind(unmatched_expr_raw(decr_depth(state)), fn lhs ->
         bind(assoc_op_eol_raw(), fn op ->
           bind(matched_expr_raw(decr_depth(state)), fn rhs ->
             constant(lhs ++ op ++ rhs)
           end)
         end)
       end)}
    ])
  end

  # pipe_op_eol -> pipe_op | pipe_op eol
  defp pipe_op_eol_raw do
    bind(pipe_op_raw(), fn op ->
      frequency([
        {4, constant(op)},
        {1,
         bind(eol_raw(), fn eol ->
           constant(op ++ eol)
         end)}
      ])
    end)
  end

  defp pipe_op_raw, do: constant([{:pipe_op, :|, 0}])

  # assoc_update -> expr pipe_op_eol assoc_expr
  defp assoc_update_raw(state) do
    lhs_gen = frequency([{1, matched_expr_raw(decr_depth(state))}, {1, unmatched_expr_raw(decr_depth(state))}])

    bind(lhs_gen, fn lhs ->
      frequency([
        # Same-line update: keep rhs from starting with `//` because then `|` lexes as identifier.
        {4,
         bind(pipe_op_raw(), fn pipe ->
           bind(map_base_expr_no_ternary_raw(decr_depth(state)), fn expr ->
             constant(lhs ++ pipe ++ expr)
           end)
         end)},
        # New-line update: allow full assoc_expr (eol before operators folds into operator meta.extra).
        {1,
         bind(pipe_op_raw(), fn pipe ->
           bind(eol_raw(), fn eol ->
             bind(assoc_expr_raw(decr_depth(state)), fn expr ->
               constant(lhs ++ pipe ++ eol ++ expr)
             end)
           end)
         end)}
      ])
    end)
  end

  # assoc_update_kw -> expr pipe_op_eol kw_data
  defp assoc_update_kw_raw(state) do
    bind(frequency([{1, matched_expr_raw(decr_depth(state))}, {1, unmatched_expr_raw(decr_depth(state))}]), fn lhs ->
      bind(pipe_op_eol_raw(), fn pipe ->
        bind(kw_data_raw(decr_depth(state)), fn kw ->
          constant(lhs ++ pipe ++ kw)
        end)
      end)
    end)
  end

  defp map_base_expr_no_ternary_raw(state) do
    frequency([
      {6, sub_matched_expr_raw(state)},
      {2,
       bind(at_op_eol_raw(), fn at ->
         bind(map_at_target_raw(state), fn expr ->
           constant(at ++ expr)
         end)
       end)},
      {2,
       bind(unary_op_eol_no_ternary_raw(), fn op ->
         bind(map_at_target_raw(state), fn expr ->
           constant(op ++ expr)
         end)
       end)},
      {1,
       bind(ellipsis_op_raw(), fn op ->
         bind(map_at_target_raw(state), fn expr ->
           constant(op ++ expr)
         end)
       end)}
    ])
  end

  defp unary_op_eol_no_ternary_raw do
    op_raw =
      frequency([
        {4, constant([{:unary_op, :!}])},
        {1, constant([{:unary_op, :^}])},
        {2, constant([{:unary_op, :not}])},
        {1, constant([{:unary_op, :"~~~"}])},
        {2, constant([{:dual_op, :+}])},
        {2, constant([{:dual_op, :-}])}
      ])

    bind(op_raw, fn op ->
      frequency([
        {4, constant(op ++ [{:gap_space, 1}])},
        {1,
         bind(eol_raw(), fn eol ->
           constant(op ++ eol)
         end)}
      ])
    end)
  end

  # assoc_base -> assoc_expr | assoc_base ',' assoc_expr
  defp assoc_base_raw(state) do
    max = min(state.max_forms, 3)

    bind(integer(1..max), fn count ->
      assoc_base_n_raw(state, count)
    end)
  end

  defp assoc_base_n_raw(state, 1), do: assoc_expr_raw(state)

  defp assoc_base_n_raw(state, count) when count > 1 do
    bind(assoc_base_n_raw(state, count - 1), fn prev ->
      bind(assoc_expr_raw(state), fn last ->
        constant(prev ++ [:comma] ++ last)
      end)
    end)
  end

  # assoc -> assoc_base | assoc_base ','
  defp assoc_raw(state) do
    bind(assoc_base_raw(state), fn base ->
      frequency([
        {5, constant(base)},
        {2, constant(base ++ [:comma])}
      ])
    end)
  end

  # map_close -> kw_data close_curly
  # map_close -> assoc close_curly
  # map_close -> assoc_base ',' kw_data close_curly
  defp map_close_raw(state) do
    frequency([
      {3,
       bind(kw_data_raw(state), fn kw ->
         bind(close_curly_raw(), fn close ->
           constant(kw ++ close)
         end)
       end)},
      {5,
       bind(assoc_raw(state), fn assoc ->
         bind(close_curly_raw(), fn close ->
           constant(assoc ++ close)
         end)
       end)},
      {1,
       bind(assoc_base_raw(state), fn base ->
         bind(kw_data_raw(state), fn kw ->
           bind(close_curly_raw(), fn close ->
             constant(base ++ [:comma] ++ kw ++ close)
           end)
         end)
       end)}
    ])
  end

  # map_args -> open_curly '}'
  # map_args -> open_curly map_close
  # map_args -> open_curly assoc_update close_curly
  # map_args -> open_curly assoc_update ',' close_curly
  # map_args -> open_curly assoc_update ',' map_close
  # map_args -> open_curly assoc_update_kw close_curly
  defp map_args_raw(state) do
    frequency([
      {3,
       bind(open_curly_raw(), fn open ->
         bind(close_curly_raw(), fn close ->
           constant(open ++ close)
         end)
       end)},
      {4,
       bind(open_curly_raw(), fn open ->
         bind(map_close_raw(state), fn close ->
           constant(open ++ close)
         end)
       end)},
      {2,
       bind(open_curly_raw(), fn open ->
         bind(assoc_update_raw(state), fn upd ->
           bind(close_curly_raw(), fn close ->
             constant(open ++ upd ++ close)
           end)
         end)
       end)},
      {1,
       bind(open_curly_raw(), fn open ->
         bind(assoc_update_raw(state), fn upd ->
           bind(close_curly_raw(), fn close ->
             constant(open ++ upd ++ [:comma] ++ close)
           end)
         end)
       end)},
      {1,
       bind(open_curly_raw(), fn open ->
         bind(assoc_update_raw(state), fn upd ->
           bind(map_close_raw(state), fn close ->
             constant(open ++ upd ++ [:comma] ++ close)
           end)
         end)
       end)},
      {1,
       bind(open_curly_raw(), fn open ->
         bind(assoc_update_kw_raw(state), fn upd ->
           bind(close_curly_raw(), fn close ->
             constant(open ++ upd ++ close)
           end)
         end)
       end)}
    ])
  end

  # bracket_arg -> open_bracket kw_data close_bracket
  # bracket_arg -> open_bracket container_expr close_bracket
  # bracket_arg -> open_bracket container_expr ',' close_bracket
  # (open_bracket container_expr ',' container_args close_bracket) intentionally NOT generated (would be error)
  defp bracket_arg_raw(state) do
    frequency([
      {3,
       bind(open_bracket_raw(), fn open ->
         bind(kw_data_raw(state), fn kw ->
           bind(close_bracket_raw(), fn close ->
             constant(open ++ kw ++ close)
           end)
         end)
       end)},
      {4,
       bind(open_bracket_raw(), fn open ->
         bind(container_expr_raw(state), fn expr ->
           bind(close_bracket_raw(), fn close ->
             constant(open ++ expr ++ close)
           end)
         end)
       end)},
      {2,
       bind(open_bracket_raw(), fn open ->
         bind(container_expr_raw(state), fn expr ->
           bind(close_bracket_raw(), fn close ->
             constant(open ++ expr ++ [:comma] ++ close)
           end)
         end)
       end)}
    ])
  end

  # dot_bracket_identifier -> bracket_identifier
  # dot_bracket_identifier -> matched_expr dot_op bracket_identifier
  defp dot_bracket_identifier_raw(state) do
    frequency([
      {5, bracket_identifier_raw()},
      {2,
       bind(matched_expr_raw(decr_depth(state)), fn lhs ->
         bind(dot_op_raw(), fn dot ->
           bind(bracket_identifier_raw(), fn rhs ->
             constant(lhs ++ dot ++ rhs)
           end)
         end)
       end)}
    ])
  end

  # bracket_expr -> dot_bracket_identifier bracket_arg
  # bracket_expr -> access_expr bracket_arg
  defp bracket_expr_raw(state) do
    state = decr_depth(state)

    frequency([
      {5,
       bind(dot_bracket_identifier_raw(state), fn lhs ->
         bind(bracket_arg_raw(state), fn arg ->
           constant(lhs ++ arg)
         end)
       end)},
      {2,
       bind(access_expr_no_brackets_raw(state), fn lhs ->
         bind(bracket_arg_raw(state), fn arg ->
           constant(lhs ++ arg)
         end)
       end)}
    ])
  end

  defp access_expr_no_brackets_raw(state) do
    frequency([
      {5, empty_paren_raw()},
      {1, fn_stab_raw(state)},
      {1, paren_stab_raw(state)},
      {3, list_raw(state)},
      {2, tuple_raw(state)},
      {2, bitstring_raw(state)},
      {2, map_raw(state)},
      {3, parens_call_raw(state)},
      {3, dot_alias_raw(state)},
      {2, capture_int_int_raw()},
      {3, int_raw()},
      {2, flt_raw()},
      {2, char_raw()},
      {2, atom_raw()},
      {1, true_raw()},
      {1, false_raw()},
      {1, nil_raw()},
      {2, parens_raw(state)}
    ])
  end

  # access_expr -> parens_call
  # parens_call -> dot_call_identifier call_args_parens
  # parens_call -> dot_call_identifier call_args_parens call_args_parens
  defp parens_call_raw(state) do
    frequency([
      {4,
       bind(dot_call_identifier_raw(decr_depth(state)), fn fun ->
         bind(call_args_parens_raw(decr_depth(state)), fn args ->
           constant(fun ++ args)
         end)
       end)},
      {1,
       bind(dot_call_identifier_raw(decr_depth(state)), fn fun ->
         bind(call_args_parens_raw(decr_depth(state)), fn args1 ->
           bind(call_args_parens_raw(decr_depth(state)), fn args2 ->
             constant(fun ++ args1 ++ args2)
           end)
         end)
       end)}
    ])
  end

  # dot_call_identifier -> dot_paren_identifier
  # dot_call_identifier -> matched_expr dot_call_op
  defp dot_call_identifier_raw(state) do
    frequency([
      {6, dot_paren_identifier_raw(state)},
      {2,
       bind(matched_expr_raw(decr_depth(state)), fn lhs ->
         constant(lhs ++ [:dot_call_op])
       end)}
    ])
  end

  # dot_paren_identifier -> paren_identifier
  # dot_paren_identifier -> matched_expr dot_op paren_identifier
  defp dot_paren_identifier_raw(state) do
    frequency([
      {5, paren_identifier_raw()},
      {2,
       bind(matched_expr_raw(decr_depth(state)), fn lhs ->
         bind(dot_op_raw(), fn dot ->
           bind(paren_identifier_raw(), fn rhs ->
             constant(lhs ++ dot ++ rhs)
           end)
         end)
       end)}
    ])
  end

  defp paren_identifier_raw do
    member_of(@identifiers)
    |> map(fn atom -> [{:paren_identifier, atom}] end)
  end

  # call_args_parens -> open_paren ')'
  # call_args_parens -> open_paren no_parens_expr close_paren
  # call_args_parens -> open_paren kw_call close_paren
  # call_args_parens -> open_paren call_args_parens_base close_paren
  # call_args_parens -> open_paren call_args_parens_base ',' kw_call close_paren
  defp call_args_parens_raw(state) do
    frequency([
      {3,
       bind(open_paren_raw(), fn open ->
         constant(open ++ [:rparen])
       end)},
      {1,
       bind(open_paren_raw(), fn open ->
         bind(no_parens_expr_raw(decr_depth(state)), fn expr ->
           bind(close_paren_raw(), fn close ->
             constant(open ++ expr ++ close)
           end)
         end)
       end)},
      {2,
       bind(open_paren_raw(), fn open ->
         bind(kw_call_raw(state), fn kw ->
           bind(close_paren_raw(), fn close ->
             constant(open ++ kw ++ close)
           end)
         end)
       end)},
      {4,
       bind(open_paren_raw(), fn open ->
         bind(call_args_parens_base_raw(decr_depth(state)), fn base ->
           bind(close_paren_raw(), fn close ->
             constant(open ++ base ++ close)
           end)
         end)
       end)},
      {2,
       bind(open_paren_raw(), fn open ->
         bind(call_args_parens_base_raw(decr_depth(state)), fn base ->
           bind(kw_call_raw(state), fn kw ->
             bind(close_paren_raw(), fn close ->
               constant(open ++ base ++ [:comma] ++ kw ++ close)
             end)
           end)
         end)
       end)}
    ])
  end

  defp open_paren_raw do
    frequency([
      {3, constant([:lparen])},
      {1,
       bind(eol_raw(), fn eol ->
         constant([:lparen] ++ eol)
       end)}
    ])
  end

  defp close_paren_raw do
    frequency([
      {3, constant([:rparen])},
      {1,
       bind(eol_raw(), fn eol ->
         constant(eol ++ [:rparen])
       end)}
    ])
  end

  # no_parens_expr
  # no_parens_expr -> at_op_eol no_parens_expr
  # no_parens_expr -> capture_op_eol no_parens_expr
  # no_parens_expr -> ellipsis_op no_parens_expr
  # no_parens_expr -> unary_op_eol no_parens_expr
  # no_parens_expr -> no_parens_one_ambig_expr
  # no_parens_expr -> no_parens_many_expr
  # no_parens_expr -> matched_expr no_parens_op_expr
  defp no_parens_expr_raw(%{depth: depth} = state) do
    state = decr_depth(state)

    base =
      frequency([
        {6, no_parens_one_ambig_expr_raw(state)},
        {4, no_parens_many_expr_raw(state)}
      ])

    if depth <= 0 do
      base
    else
      frequency([
        {6, base},
        {2, no_parens_expr_binary_raw(state)},
        {1,
         bind(unary_op_eol_raw(), fn op ->
           rhs_gen =
             case {List.first(op), List.last(op)} do
               {{:ternary_op, _, _}, _} -> no_parens_expr_raw(state)
               {_, {:eol, _}} -> no_parens_expr_raw(state)
               _ -> no_parens_expr_no_ternary_prefix_raw(state)
             end

           bind(rhs_gen, fn expr ->
             constant(op ++ expr)
           end)
         end)},
        {1,
         bind(at_op_eol_raw(), fn op ->
           bind(no_parens_expr_raw(state), fn expr ->
             constant(op ++ expr)
           end)
         end)},
        {1,
         bind(capture_op_eol_raw(), fn op ->
           bind(no_parens_expr_raw(state), fn expr ->
             constant(op ++ expr)
           end)
         end)},
        {1,
         bind(ellipsis_op_raw(), fn op ->
           bind(no_parens_expr_raw(state), fn expr ->
             constant(op ++ expr)
           end)
         end)}
      ])
    end
  end

  defp no_parens_expr_no_ternary_prefix_raw(%{depth: depth} = state) do
    state = decr_depth(state)

    base =
      frequency([
        {6, no_parens_one_ambig_expr_raw(state)},
        {4, no_parens_many_expr_raw(state)}
      ])

    if depth <= 0 do
      base
    else
      frequency([
        {6, base},
        {2, no_parens_expr_binary_raw(state)},
        {1,
         bind(unary_op_eol_no_ternary_raw(), fn op ->
           bind(no_parens_expr_no_ternary_prefix_raw(state), fn expr ->
             constant(op ++ expr)
           end)
         end)},
        {1,
         bind(at_op_eol_raw(), fn op ->
           bind(no_parens_expr_no_ternary_prefix_raw(state), fn expr ->
             constant(op ++ expr)
           end)
         end)},
        {1,
         bind(capture_op_eol_raw(), fn op ->
           bind(no_parens_expr_no_ternary_prefix_raw(state), fn expr ->
             constant(op ++ expr)
           end)
         end)},
        {1,
         bind(ellipsis_op_raw(), fn op ->
           bind(no_parens_expr_no_ternary_prefix_raw(state), fn expr ->
             constant(op ++ expr)
           end)
         end)}
      ])
    end
  end

  defp no_parens_expr_binary_raw(state) do
    bind(no_parens_lhs_raw(state), fn lhs ->
      bind(no_parens_op_expr_raw(state), fn rhs ->
        constant(lhs ++ rhs)
      end)
    end)
  end

  # Keep LHS free of :op_identifier (it is context-sensitive and breaks roundtrips).
  defp no_parens_lhs_raw(state) do
    frequency([
      {10, sub_matched_expr_raw(state)},
      {2,
       bind(unary_op_eol_no_ternary_raw(), fn op ->
         bind(sub_matched_expr_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)}
    ])
  end

  # no_parens_op_expr -> *_op_eol no_parens_expr | when_op_eol call_args_no_parens_kw
  defp no_parens_op_expr_raw(state) do
    pre = [{:gap_space, 1}]

    frequency([
      {4, no_parens_bin_op_rhs_raw(pre, match_op_eol_raw(), state)},
      {3, no_parens_bin_op_rhs_raw(pre, dual_op_eol_raw(), state)},
      {3, no_parens_bin_op_rhs_raw(pre, mult_op_eol_raw(), state)},
      {2, no_parens_bin_op_rhs_raw(pre, power_op_eol_raw(), state)},
      {2, no_parens_bin_op_rhs_raw(pre, concat_op_eol_raw(), state)},
      {2, no_parens_bin_op_rhs_raw(pre, range_op_eol_raw(), state)},
      {2, no_parens_bin_op_rhs_raw(pre, ternary_op_eol_raw(), state)},
      {1, no_parens_bin_op_rhs_raw(pre, xor_op_eol_raw(), state)},
      {1, no_parens_bin_op_rhs_raw(pre, and_op_eol_raw(), state)},
      {1, no_parens_bin_op_rhs_raw(pre, or_op_eol_raw(), state)},
      {1, no_parens_bin_op_rhs_raw(pre, in_op_eol_raw(), state)},
      {1, no_parens_bin_op_rhs_raw(pre, in_match_op_eol_raw(), state)},
      {1, no_parens_bin_op_rhs_raw(pre, type_op_eol_raw(), state)},
      {1, no_parens_bin_op_rhs_raw(pre, when_op_eol_raw(), state)},
      {1, no_parens_bin_op_rhs_pipe_raw(pre, state)},
      {1, no_parens_bin_op_rhs_raw(pre, comp_op_eol_raw(), state)},
      {1, no_parens_bin_op_rhs_raw(pre, rel_op_eol_raw(), state)},
      {1, no_parens_bin_op_rhs_raw(pre, arrow_op_eol_raw(), state)},
      {1, no_parens_when_kw_rhs_raw(pre, state)}
    ])
  end

  defp no_parens_bin_op_rhs_raw(pre, op_eol_gen, state) do
    bind(op_eol_gen, fn op ->
      rhs_gen =
        case List.last(op) do
          {:eol, _} -> no_parens_expr_raw(state)
          _ -> no_parens_expr_no_ternary_prefix_raw(state)
        end

      bind(rhs_gen, fn rhs ->
        constant(pre ++ op ++ rhs)
      end)
    end)
  end

  defp no_parens_bin_op_rhs_pipe_raw(pre, state) do
    bind(pipe_op_eol_raw(), fn op ->
      case List.last(op) do
        {:eol, _} ->
          bind(no_parens_expr_raw(state), fn rhs ->
            constant(pre ++ op ++ rhs)
          end)

        _ ->
          bind(no_parens_expr_raw(state), fn rhs ->
            constant(pre ++ op ++ [{:gap_space, 1}] ++ rhs)
          end)
      end
    end)
  end

  defp no_parens_when_kw_rhs_raw(pre, state) do
    bind(when_op_eol_raw(), fn op ->
      bind(call_args_no_parens_kw_raw(state), fn kw ->
        constant(pre ++ op ++ kw)
      end)
    end)
  end

  # no_parens_one_ambig_expr -> dot_op_identifier call_args_no_parens_ambig
  # no_parens_one_ambig_expr -> dot_identifier call_args_no_parens_ambig
  defp no_parens_one_ambig_expr_raw(state) do
    frequency([
      {3,
       bind(dot_identifier_raw(state), fn fun ->
         bind(call_args_no_parens_ambig_raw(state), fn arg ->
           constant(fun ++ [{:gap_space, 1}] ++ arg)
         end)
       end)},
      # :op_identifier is only stable when followed by a tight unary (+1/-1) form.
      {2,
       bind(dot_op_identifier_raw(state), fn fun ->
         bind(call_args_no_parens_ambig_for_op_identifier_raw(state), fn arg ->
           constant(fun ++ [{:gap_space, 1}] ++ arg)
         end)
       end)}
    ])
  end

  # no_parens_many_expr -> dot_op_identifier call_args_no_parens_many_strict
  # no_parens_many_expr -> dot_identifier call_args_no_parens_many_strict
  defp no_parens_many_expr_raw(state) do
    frequency([
      {3,
       bind(dot_identifier_raw(state), fn fun ->
         bind(call_args_no_parens_many_strict_raw(state), fn args ->
           constant(fun ++ [{:gap_space, 1}] ++ args)
         end)
       end)},
      # :op_identifier is only stable when the first arg is a tight unary (+1/-1) form.
      {2,
       bind(dot_op_identifier_raw(state), fn fun ->
         bind(call_args_no_parens_many_strict_for_op_identifier_raw(state), fn args ->
           constant(fun ++ [{:gap_space, 1}] ++ args)
         end)
       end)}
    ])
  end

  # kw_call is close enough to kw_data for this property test.
  defp kw_call_raw(state), do: kw_data_raw(state)

  # call_args_parens_base -> call_args_parens_expr (',' call_args_parens_expr)*
  defp call_args_parens_base_raw(state) do
    max = min(state.max_forms, 3)

    bind(integer(1..max), fn count ->
      call_args_parens_base_n_raw(state, count)
    end)
  end

  defp call_args_parens_base_n_raw(state, 1), do: call_args_parens_expr_raw(state)

  defp call_args_parens_base_n_raw(state, count) when count > 1 do
    bind(call_args_parens_base_n_raw(state, count - 1), fn prev ->
      bind(call_args_parens_expr_raw(state), fn last ->
        constant(prev ++ [:comma] ++ last)
      end)
    end)
  end

  # call_args_parens_expr -> matched_expr | unmatched_expr
  defp call_args_parens_expr_raw(state) do
    frequency([
      {4, matched_expr_raw(state)},
      {1, unmatched_expr_raw(state)}
    ])
  end

  # bracket_at_expr -> at_op_eol dot_bracket_identifier bracket_arg
  # bracket_at_expr -> at_op_eol access_expr bracket_arg
  defp bracket_at_expr_raw(state) do
    state = decr_depth(state)

    bind(at_op_eol_raw(), fn at ->
      frequency([
        {3,
         bind(dot_bracket_identifier_raw(state), fn lhs ->
           bind(bracket_arg_raw(state), fn arg ->
             constant(at ++ lhs ++ arg)
           end)
         end)},
        {1,
         bind(access_expr_no_brackets_raw(state), fn lhs ->
           bind(bracket_arg_raw(state), fn arg ->
             constant(at ++ lhs ++ arg)
           end)
         end)}
      ])
    end)
  end

  defp bracket_identifier_raw do
    member_of(@identifiers)
    |> map(fn atom -> [{:bracket_identifier, atom}] end)
  end

  defp capture_int_int_raw do
    bind(int_raw(), fn int ->
      constant([:capture_int] ++ int)
    end)
  end

  defp flt_raw do
    # Small, simple floats only (avoid exponent/underscores for now).
    tuple({integer(0..100), integer(0..99)})
    |> map(fn {a, b} ->
      frac = String.pad_leading(Integer.to_string(b), 2, "0")
      str = Integer.to_string(a) <> "." <> frac
      [{:flt, String.to_charlist(str)}]
    end)
  end

  defp char_raw do
    member_of([
      {~c"?a", ?a},
      {~c"?b", ?b},
      {~c"?z", ?z}
    ])
    |> map(fn {repr, cp} -> [{:char, repr, cp}] end)
  end

  defp atom_raw do
    member_of([:ok, :error, :foo, :bar, :baz])
    |> map(fn atom -> [{:atom, atom}] end)
  end

  # dot_alias -> alias | matched_expr dot_op alias
  # (dot container variants omitted for now)
  defp dot_alias_raw(%{depth: depth} = state) do
    if depth <= 0 do
      alias_raw()
    else
      frequency([
        # dot_alias -> alias
        {4, alias_raw()},
        # dot_alias -> matched_expr dot_op alias
        {3,
         bind(matched_expr_raw(decr_depth(state)), fn lhs ->
           bind(dot_op_raw(), fn dot ->
             bind(alias_raw(), fn rhs ->
               constant(lhs ++ dot ++ rhs)
             end)
           end)
         end)},
        # dot_alias -> matched_expr dot_op open_curly '}'
        {1,
         bind(matched_expr_raw(decr_depth(state)), fn lhs ->
           bind(dot_op_raw(), fn dot ->
             bind(open_curly_raw(), fn open ->
               bind(close_curly_raw(), fn close ->
                 constant(lhs ++ dot ++ open ++ close)
               end)
             end)
           end)
         end)},
        # dot_alias -> matched_expr dot_op open_curly container_args close_curly
        {1,
         bind(matched_expr_raw(decr_depth(state)), fn lhs ->
           bind(dot_op_raw(), fn dot ->
             bind(open_curly_raw(), fn open ->
               bind(container_args_raw(decr_depth(state)), fn args ->
                 bind(close_curly_raw(), fn close ->
                   constant(lhs ++ dot ++ open ++ args ++ close)
                 end)
               end)
             end)
           end)
         end)}
      ])
    end
  end

  defp open_curly_raw do
    frequency([
      {3, constant([:lcurly])},
      {1,
       bind(eol_raw(), fn eol ->
         constant([:lcurly] ++ eol)
       end)}
    ])
  end

  defp close_curly_raw do
    frequency([
      {3, constant([{:rcurly, nil}])},
      {1,
       bind(eol_raw(), fn [{:eol, _n}] = eol ->
         constant(eol ++ [{:rcurly, nil}])
       end)}
    ])
  end

  # container_expr -> matched_expr | unmatched_expr
  defp container_expr_raw(state) do
    frequency([
      {5, matched_expr_raw(state)},
      {1, unmatched_expr_raw(state)}
    ])
  end

  # unmatched_expr
  # unmatched_expr -> matched_expr unmatched_op_expr
  # unmatched_expr -> unmatched_expr matched_op_expr
  # unmatched_expr -> unmatched_expr unmatched_op_expr
  # unmatched_expr -> unmatched_expr no_parens_op_expr
  # unmatched_expr -> unary_op_eol expr
  # unmatched_expr -> at_op_eol expr
  # unmatched_expr -> capture_op_eol expr
  # unmatched_expr -> ellipsis_op expr
  defp unmatched_expr_raw(%{depth: depth} = state) do
    if depth <= 0 do
      matched_expr_raw(state)
    else
      state = decr_depth(state)

      frequency([
        {10, unmatched_expr_no_binary_raw(state)},
        {2, unmatched_expr_matched_unmatched_raw(state)},
        {3, unmatched_expr_binary_raw(state)}
      ])
    end
  end

  defp unmatched_expr_no_ternary_prefix_raw(%{depth: depth} = state) do
    if depth <= 0 do
      matched_expr_no_ternary_prefix_raw(state)
    else
      state = decr_depth(state)

      frequency([
        {10, unmatched_expr_no_binary_no_ternary_prefix_raw(state)},
        {2, unmatched_expr_matched_unmatched_no_ternary_prefix_raw(state)},
        {3, unmatched_expr_binary_no_ternary_prefix_raw(state)}
      ])
    end
  end

  defp unmatched_expr_matched_unmatched_raw(state) do
    bind(matched_expr_raw(state), fn lhs ->
      bind(unmatched_op_expr_raw(state), fn rhs ->
        constant(lhs ++ rhs)
      end)
    end)
  end

  defp unmatched_expr_matched_unmatched_no_ternary_prefix_raw(state) do
    bind(matched_expr_no_ternary_prefix_raw(state), fn lhs ->
      bind(unmatched_op_expr_raw(state), fn rhs ->
        constant(lhs ++ rhs)
      end)
    end)
  end

  defp unmatched_expr_no_binary_raw(state) do
    frequency([
      {10, matched_expr_raw(state)},
      {1, block_expr_raw(state)},
      {2,
       bind(unary_op_eol_raw(), fn op ->
         rhs_gen =
           case List.last(op) do
             {:eol, _} -> expr_raw(state)
             _ -> expr_no_ternary_prefix_raw(state)
           end

         bind(rhs_gen, fn rhs ->
           constant(op ++ rhs)
         end)
       end)},
      {2,
       bind(at_op_eol_raw(), fn op ->
         bind(expr_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)},
      {2,
       bind(capture_op_eol_raw(), fn op ->
         bind(expr_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)},
      {1,
       bind(ellipsis_op_raw(), fn op ->
         bind(expr_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)}
    ])
  end

  defp unmatched_expr_no_binary_no_ternary_prefix_raw(state) do
    frequency([
      {10, matched_expr_no_ternary_prefix_raw(state)},
      {1, block_expr_raw(state)},
      {2,
       bind(unary_op_eol_no_ternary_raw(), fn op ->
         bind(expr_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)},
      {2,
       bind(at_op_eol_raw(), fn op ->
         bind(expr_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)},
      {2,
       bind(capture_op_eol_raw(), fn op ->
         bind(expr_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)},
      {1,
       bind(ellipsis_op_raw(), fn op ->
         bind(expr_raw(state), fn rhs ->
           constant(op ++ rhs)
         end)
       end)}
    ])
  end

  defp unmatched_expr_binary_raw(state) do
    bind(unmatched_expr_no_binary_raw(state), fn lhs ->
      bind(
        frequency([
          {4, matched_op_expr_raw(state)},
          {4, unmatched_op_expr_raw(state)},
          {2, no_parens_op_expr_raw(state)}
        ]),
        fn rhs ->
          constant(lhs ++ rhs)
        end
      )
    end)
  end

  defp unmatched_expr_binary_no_ternary_prefix_raw(state) do
    bind(unmatched_expr_no_binary_no_ternary_prefix_raw(state), fn lhs ->
      bind(
        frequency([
          {4, matched_op_expr_raw(state)},
          {4, unmatched_op_expr_raw(state)},
          {2, no_parens_op_expr_raw(state)}
        ]),
        fn rhs ->
          constant(lhs ++ rhs)
        end
      )
    end)
  end

  # unmatched_op_expr -> *_op_eol unmatched_expr
  defp unmatched_op_expr_raw(state) do
    pre = [{:gap_space, 1}]

    frequency([
      {4, unmatched_bin_op_rhs_raw(pre, match_op_eol_raw(), state)},
      {3, unmatched_bin_op_rhs_raw(pre, dual_op_eol_raw(), state)},
      {3, unmatched_bin_op_rhs_raw(pre, mult_op_eol_raw(), state)},
      {2, unmatched_bin_op_rhs_raw(pre, power_op_eol_raw(), state)},
      {2, unmatched_bin_op_rhs_raw(pre, concat_op_eol_raw(), state)},
      {2, unmatched_bin_op_rhs_raw(pre, range_op_eol_raw(), state)},
      {2, unmatched_bin_op_rhs_raw(pre, ternary_op_eol_raw(), state)},
      {1, unmatched_bin_op_rhs_raw(pre, xor_op_eol_raw(), state)},
      {1, unmatched_bin_op_rhs_raw(pre, and_op_eol_raw(), state)},
      {1, unmatched_bin_op_rhs_raw(pre, or_op_eol_raw(), state)},
      {1, unmatched_bin_op_rhs_raw(pre, in_op_eol_raw(), state)},
      {1, unmatched_bin_op_rhs_raw(pre, in_match_op_eol_raw(), state)},
      {1, unmatched_bin_op_rhs_raw(pre, type_op_eol_raw(), state)},
      {1, unmatched_bin_op_rhs_raw(pre, when_op_eol_raw(), state)},
      {1, unmatched_bin_op_rhs_pipe_raw(pre, state)},
      {1, unmatched_bin_op_rhs_raw(pre, comp_op_eol_raw(), state)},
      {1, unmatched_bin_op_rhs_raw(pre, rel_op_eol_raw(), state)},
      {1, unmatched_bin_op_rhs_raw(pre, arrow_op_eol_raw(), state)}
    ])
  end

  defp unmatched_bin_op_rhs_raw(pre, op_eol_gen, state) do
    bind(op_eol_gen, fn op ->
      rhs_gen =
        case List.last(op) do
          {:eol, _} -> unmatched_expr_raw(state)
          _ -> unmatched_expr_no_ternary_prefix_raw(state)
        end

      bind(rhs_gen, fn rhs ->
        constant(pre ++ op ++ rhs)
      end)
    end)
  end

  defp unmatched_bin_op_rhs_pipe_raw(pre, state) do
    bind(pipe_op_eol_raw(), fn op ->
      case List.last(op) do
        {:eol, _} ->
          bind(unmatched_expr_raw(state), fn rhs ->
            constant(pre ++ op ++ rhs)
          end)

        _ ->
          bind(unmatched_expr_raw(state), fn rhs ->
            constant(pre ++ op ++ [{:gap_space, 1}] ++ rhs)
          end)
      end
    end)
  end

  # container_args_base -> container_expr | container_args_base ',' container_expr
  defp container_args_base_raw(state) do
    max = min(state.max_forms, 3)

    bind(integer(1..max), fn count ->
      container_args_base_n_raw(state, count)
    end)
  end

  defp container_args_base_n_raw(state, 1), do: container_expr_raw(state)

  defp container_args_base_n_raw(state, count) when count > 1 do
    bind(container_args_base_n_raw(state, count - 1), fn prev ->
      bind(container_expr_raw(state), fn last ->
        constant(prev ++ [:comma] ++ last)
      end)
    end)
  end

  # container_args -> container_args_base
  # container_args -> container_args_base ','
  # container_args -> container_args_base ',' kw_data
  defp container_args_raw(state) do
    bind(container_args_base_raw(state), fn base ->
      frequency([
        {5, constant(base)},
        {2, constant(base ++ [:comma])},
        {2,
         bind(kw_data_raw(state), fn kw ->
           constant(base ++ [:comma] ++ kw)
         end)}
      ])
    end)
  end

  # kw_data -> kw_base | kw_base ','
  defp kw_data_raw(state) do
    bind(kw_base_raw(state), fn base ->
      frequency([
        {5, constant(base)},
        {1, constant(base ++ [:comma])}
      ])
    end)
  end

  # kw_base -> kw_eol container_expr
  # kw_base -> kw_base ',' kw_eol container_expr
  defp kw_base_raw(state) do
    max = min(state.max_forms, 3)

    bind(integer(1..max), fn count ->
      kw_base_n_raw(state, count)
    end)
  end

  defp kw_base_n_raw(state, 1) do
    bind(kw_eol_raw(), fn kw ->
      bind(container_expr_raw(state), fn expr ->
        constant(kw ++ expr)
      end)
    end)
  end

  defp kw_base_n_raw(state, count) when count > 1 do
    bind(kw_base_n_raw(state, count - 1), fn prev ->
      bind(kw_eol_raw(), fn kw ->
        bind(container_expr_raw(state), fn expr ->
          constant(prev ++ [:comma] ++ kw ++ expr)
        end)
      end)
    end)
  end

  # kw_eol -> kw_identifier | kw_identifier eol
  # NOTE: we always add a gap-space after the ':' so Toxic lexes it as kw_identifier
  # and doesn't error on missing space after colon.
  defp kw_eol_raw do
    bind(kw_identifier_raw(), fn kw ->
      frequency([
        {4, constant(kw ++ [{:gap_space, 1}])},
        {1,
         bind(eol_raw(), fn eol ->
           constant(kw ++ [{:gap_space, 1}] ++ eol)
         end)}
      ])
    end)
  end

  defp kw_identifier_raw do
    member_of(@identifiers)
    |> map(fn atom -> [{:kw_identifier, atom}] end)
  end

  defp alias_raw do
    member_of(@aliases)
    |> map(fn atom -> [{:alias, atom}] end)
  end

  defp true_raw, do: constant([true])
  defp false_raw, do: constant([false])
  defp nil_raw, do: constant([nil])

  defp int_raw do
    integer(0..100)
    |> map(fn n -> [{:int, Integer.to_charlist(n)}] end)
  end

  # '(' grammar ')'
  defp parens_raw(state) do
    bind(grammar_state_raw(state), fn inner ->
      constant([:lparen] ++ inner ++ [:rparen])
    end)
  end

  defp decr_depth(%{depth: depth} = state), do: %{state | depth: depth - 1}

  # Concatenate expr_list eoe expr respecting Toxic's range_op newline folding.
  defp concat_with_eoe(lhs, eoe, rhs) do
    case {eoe, rhs} do
      {[{:eol, n}], [{:range_op, op, existing} | rest]} ->
        existing = if is_integer(existing), do: existing, else: 0
        lhs ++ [{:range_op, op, existing + n} | rest]

      _ ->
        lhs ++ eoe ++ rhs
    end
  end

  # ---------------------------------------------------------------------------
  # Materialize raw tokens -> Toxic tokens with ranged meta
  # ---------------------------------------------------------------------------

  defp materialize(raw_tokens) do
    {tokens_rev, _pos, _prev_raw} =
      Enum.reduce(raw_tokens, {[], {1, 1}, nil}, fn raw, {acc, pos, prev_raw} ->
        raw =
          case {raw, prev_raw} do
            {{:rcurly, nil}, {:eol, n}} when is_integer(n) -> {:rcurly, n}
            {{:rcurly, nil}, {:comma, n}} when is_integer(n) -> {:rcurly, n}
            {{:rbracket, nil}, {:eol, n}} when is_integer(n) -> {:rbracket, n}
            {{:rbracket, nil}, {:comma, n}} when is_integer(n) -> {:rbracket, n}
            {{:close_bit, nil}, {:eol, n}} when is_integer(n) -> {:close_bit, n}
            {{:close_bit, nil}, {:comma, n}} when is_integer(n) -> {:close_bit, n}
            _ -> raw
          end

        {tok, pos} = materialize_token(raw, pos)
        acc = if is_nil(tok), do: acc, else: [tok | acc]
        {acc, pos, raw}
      end)

    Enum.reverse(tokens_rev)
  end

  defp coalesce_eols(raw_tokens), do: do_coalesce_eols(raw_tokens, [])

  defp do_coalesce_eols([{:eol, a}, {:eol, b} | rest], acc), do: do_coalesce_eols([{:eol, a + b} | rest], acc)

  # Toxic folds leading EOLs into some operator meta.extra (no standalone :eol token).
  defp do_coalesce_eols([{:eol, n}, {:range_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:range_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:pipe_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:pipe_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:assoc_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:assoc_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:ternary_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:ternary_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:concat_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:concat_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:comp_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:comp_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:rel_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:rel_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:arrow_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:arrow_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:match_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:match_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:mult_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:mult_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:power_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:power_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:xor_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:xor_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:and_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:and_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:or_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:or_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:in_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:in_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:in_match_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:in_match_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:type_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:type_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:when_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:when_op, op, extra + n} | rest], acc)
  end

  defp do_coalesce_eols([{:eol, n}, {:stab_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:stab_op, op, extra + n} | rest], acc)
  end

  # Toxic folds newlines after comma into the comma token meta (no standalone :eol token).
  defp do_coalesce_eols([:comma, {:eol, n} | rest], acc), do: do_coalesce_eols([{:comma, n} | rest], acc)
  defp do_coalesce_eols([{:comma, a}, {:eol, b} | rest], acc), do: do_coalesce_eols([{:comma, a + b} | rest], acc)

  # Toxic folds newlines after semicolon into the semicolon token meta (no standalone :eol token).
  defp do_coalesce_eols([:semi, {:eol, n} | rest], acc), do: do_coalesce_eols([{:semi, n} | rest], acc)
  defp do_coalesce_eols([{:semi, a}, {:eol, b} | rest], acc), do: do_coalesce_eols([{:semi, a + b} | rest], acc)

  defp do_coalesce_eols([h | t], acc), do: do_coalesce_eols(t, [h | acc])
  defp do_coalesce_eols([], acc), do: Enum.reverse(acc)

  defp materialize_token({:gap_eol, n}, {line, _col}) do
    {nil, {line + n, 1}}
  end

  defp materialize_token({:gap_space, n}, {line, col}) do
    {nil, {line, col + n}}
  end

  defp materialize_token({:eol, n}, {line, col}) do
    start = {line, col}
    stop = {line + n, 1}
    {{:eol, {start, stop, n}}, stop}
  end

  defp materialize_token(:semi, pos), do: materialize_token({:semi, 0}, pos)

  defp materialize_token({:semi, extra}, {line, col}) do
    extra = extra || 0
    start = {line, col}
    stop = if extra > 0, do: {line + extra, 1}, else: {line, col + 1}
    {{:";", {start, stop, extra}}, stop}
  end

  defp materialize_token(:map_op, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:%{}, {start, stop, nil}}, stop}
  end

  defp materialize_token(:percent, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:%, {start, stop, nil}}, stop}
  end

  defp materialize_token({:assoc_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + 2}
    {{:assoc_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:assoc_op, op}, pos), do: materialize_token({:assoc_op, op, 0}, pos)

  defp materialize_token({:pipe_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + 1}
    {{:pipe_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:pipe_op, op}, pos), do: materialize_token({:pipe_op, op, 0}, pos)

  defp materialize_token({:unary_op, op}, {line, col}) do
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:unary_op, {start, stop, nil}, op}, stop}
  end

  defp materialize_token({:dual_op, op}, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:dual_op, {start, stop, nil}, op}, stop}
  end

  defp materialize_token({:ternary_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:ternary_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:match_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:match_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:mult_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:mult_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:power_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:power_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:concat_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:concat_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:xor_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:xor_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:and_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:and_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:or_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:or_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:in_op, op, extra}, {line, col}) do
    extra = if extra in [0, nil], do: nil, else: extra
    {line, col} = if is_integer(extra) and extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}

    case op do
      :"not in" ->
        stop = {line, col + String.length(Atom.to_string(op))}
        in_start = {line, col + 4}
        in_meta = {in_start, stop, nil}
        {{:in_op, {start, stop, extra}, op, in_meta}, stop}

      _ ->
        stop = {line, col + String.length(Atom.to_string(op))}
        {{:in_op, {start, stop, extra}, op}, stop}
    end
  end

  defp materialize_token({:in_match_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:in_match_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:type_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:type_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:when_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:when_op, {start, stop, extra}, :when}, stop}
  end

  defp materialize_token({:comp_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:comp_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:rel_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:rel_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:arrow_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:arrow_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:stab_op, op, extra}, {line, col}) do
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + String.length(Atom.to_string(op))}
    {{:stab_op, {start, stop, extra}, op}, stop}
  end

  defp materialize_token({:ternary_op, op}, pos), do: materialize_token({:ternary_op, op, 0}, pos)

  defp materialize_token(:comma, pos), do: materialize_token({:comma, 0}, pos)

  defp materialize_token({:comma, eol_count}, {line, col}) do
    start = {line, col}

    {stop, next_pos} =
      if eol_count > 0 do
        {{line + eol_count, 1}, {line + eol_count, 1}}
      else
        {{line, col + 1}, {line, col + 1}}
      end

    {{:",", {start, stop, eol_count}}, next_pos}
  end

  defp materialize_token(:dot, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:., {start, stop, nil}}, stop}
  end

  defp materialize_token(:dot_call_op, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:dot_call_op, {start, stop, nil}, :.}, stop}
  end

  defp materialize_token(:at, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:at_op, {start, stop, nil}, :@}, stop}
  end

  defp materialize_token(:capture_int, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:capture_int, {start, stop, nil}, :&}, stop}
  end

  defp materialize_token(:capture_op, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:capture_op, {start, stop, nil}, :&}, stop}
  end

  defp materialize_token(:lbracket, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:"[", {start, stop, nil}}, stop}
  end

  defp materialize_token(:rbracket, pos), do: materialize_token({:rbracket, nil}, pos)

  defp materialize_token({:rbracket, extra}, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:"]", {start, stop, extra}}, stop}
  end

  defp materialize_token(:open_bit, {line, col}) do
    start = {line, col}
    stop = {line, col + 2}
    {{:"<<", {start, stop, nil}}, stop}
  end

  defp materialize_token({:close_bit, extra}, {line, col}) do
    # Ensure nested bitstrings can be rendered unambiguously: `<< <<a>> >>`.
    start = {line, col + 1}
    stop = {line, col + 3}
    {{:">>", {start, stop, extra}}, stop}
  end

  defp materialize_token(:lcurly, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:"{", {start, stop, nil}}, stop}
  end

  defp materialize_token({:rcurly, extra}, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:"}", {start, stop, extra}}, stop}
  end

  defp materialize_token(:lparen, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:"(", {start, stop, nil}}, stop}
  end

  defp materialize_token(:rparen, pos), do: materialize_token({:rparen, nil}, pos)

  defp materialize_token({:rparen, extra}, {line, col}) do
    extra = extra || 0
    {line, col} = if extra > 0, do: {line + extra, 1}, else: {line, col}
    start = {line, col}
    stop = {line, col + 1}
    {{:")", {start, stop, extra}}, stop}
  end

  defp materialize_token(:fn, {line, col}) do
    start = {line, col}
    stop = {line, col + 2}
    {{:fn, {start, stop, nil}}, stop}
  end

  defp materialize_token(:do, {line, col}) do
    start = {line, col}
    stop = {line, col + 2}
    {{:do, {start, stop, nil}}, stop}
  end

  defp materialize_token(:end, {line, col}) do
    start = {line, col}
    stop = {line, col + 3}
    {{:end, {start, stop, nil}}, stop}
  end

  defp materialize_token({:block_identifier, atom}, {line, col}) do
    text = Atom.to_string(atom)
    start = {line, col}
    stop = {line, col + String.length(text)}
    {{:block_identifier, {start, stop, Atom.to_charlist(atom)}, atom}, stop}
  end

  defp materialize_token({:identifier, atom}, {line, col}) do
    text = Atom.to_string(atom)
    start = {line, col}
    stop = {line, col + String.length(text)}
    {{:identifier, {start, stop, Atom.to_charlist(atom)}, atom}, stop}
  end

  defp materialize_token({:paren_identifier, atom}, {line, col}) do
    text = Atom.to_string(atom)
    start = {line, col}
    stop = {line, col + String.length(text)}
    {{:paren_identifier, {start, stop, Atom.to_charlist(atom)}, atom}, stop}
  end

  defp materialize_token({:op_identifier, atom}, {line, col}) do
    text = Atom.to_string(atom)
    start = {line, col}
    stop = {line, col + String.length(text)}
    {{:op_identifier, {start, stop, Atom.to_charlist(atom)}, atom}, stop}
  end

  defp materialize_token({:kw_identifier, atom}, {line, col}) do
    text = Atom.to_string(atom)
    start = {line, col}
    stop = {line, col + String.length(text) + 1}
    {{:kw_identifier, {start, stop, Atom.to_charlist(atom)}, atom}, stop}
  end

  defp materialize_token({:bracket_identifier, atom}, {line, col}) do
    text = Atom.to_string(atom)
    start = {line, col}
    stop = {line, col + String.length(text)}
    {{:bracket_identifier, {start, stop, Atom.to_charlist(atom)}, atom}, stop}
  end

  defp materialize_token({:alias, atom}, {line, col}) do
    text = Atom.to_string(atom)
    start = {line, col}
    stop = {line, col + String.length(text)}
    {{:alias, {start, stop, Atom.to_charlist(atom)}, atom}, stop}
  end

  defp materialize_token({:atom, atom}, {line, col}) do
    name = Atom.to_string(atom)
    start = {line, col}
    stop = {line, col + 1 + String.length(name)}
    {{:atom, {start, stop, Atom.to_charlist(atom)}, atom}, stop}
  end

  defp materialize_token({:flt, chars}, {line, col}) do
    text = List.to_string(chars)
    start = {line, col}
    stop = {line, col + String.length(text)}

    extra =
      case Float.parse(text) do
        {f, ""} -> f
        _ -> nil
      end

    {{:flt, {start, stop, extra}, chars}, stop}
  end

  defp materialize_token({:char, repr, cp}, {line, col}) do
    text = List.to_string(repr)
    start = {line, col}
    stop = {line, col + String.length(text)}
    {{:char, {start, stop, repr}, cp}, stop}
  end

  defp materialize_token({:int, chars}, {line, col}) do
    text = List.to_string(chars)
    start = {line, col}
    stop = {line, col + String.length(text)}

    extra =
      case Integer.parse(text) do
        {n, ""} -> n
        _ -> nil
      end

    {{:int, {start, stop, extra}, chars}, stop}
  end

  defp materialize_token({:range_op, op, leading_eol}, {line, col}) do
    {line, col} = if leading_eol && leading_eol > 0, do: {line + leading_eol, 1}, else: {line, col}

    start = {line, col}
    stop = {line, col + 2}
    {{:range_op, {start, stop, leading_eol}, op}, stop}
  end

  defp materialize_token({:ellipsis_op, op}, {line, col}) do
    start = {line, col}
    stop = {line, col + 3}
    {{:ellipsis_op, {start, stop, nil}, op}, stop}
  end

  defp materialize_token(true, {line, col}) do
    start = {line, col}
    stop = {line, col + 4}
    {{true, {start, stop, nil}}, stop}
  end

  defp materialize_token(false, {line, col}) do
    start = {line, col}
    stop = {line, col + 5}
    {{false, {start, stop, nil}}, stop}
  end

  defp materialize_token(nil, {line, col}) do
    start = {line, col}
    stop = {line, col + 3}
    {{nil, {start, stop, nil}}, stop}
  end
end
