# defmodule ToxicParser.Generator do

#   # Terminators

#   @identifiers ~w(foo bar baz qux spam eggs alpha beta gamma delta)a
#   @aliases ~w(Foo Bar Baz Qux Remote Mod State Schema Context Config Default)a
#   @atoms ~w(ok error foo bar baz one two three alice bob nil true false)a

#   defp identifier do
#     StreamData.member_of(@identifiers)
#     |> StreamData.map(fn atom -> {:identifier, atom} end)
#   end

#   defp fallback_literal do
#     StreamData.member_of(@fallback_literals)
#     |> StreamData.map(fn
#       nil -> :nil_lit
#       0 -> {:int, 0, :dec, ~c"0"}
#       :ok -> {:atom_lit, :ok}
#     end)
#   end

#   def grammar(opts \\ []) do
#     StreamData.frequency([
#       # grammar -> expr_list (most common)
#       {5, expr_list(state, max_forms)},
#       # grammar -> expr_list eoe (trailing newline - common)
#       {3, expr_list_eoe(state, max_forms)},
#       # grammar -> eoe expr_list (leading newline - less common)
#       {1, eoe_expr_list(state, max_forms)},
#       # grammar -> eoe expr_list eoe (both - rare)
#       {1, eoe_expr_list_eoe(state, max_forms)},
#       # grammar -> eoe (only eoe - edge case)
#       {1, eoe_only(state)}
#       # grammar -> '$empty' (completely empty)
#       # Disabled
#       # {1, gen_grammar_empty()}
#     ])
#   end

#   # grammar -> expr_list : build_block(reverse('$1')).
#   defp expr_list(state, max_forms) do
#     StreamData.bind(StreamData.integer(1..max_forms), fn count ->
#       gen_expr_list(state, count)
#     end)
#     |> StreamData.map(fn exprs -> {:grammar_v2, nil, exprs, nil} end)
#   end

#   # grammar -> expr_list eoe : build_block(reverse(annotate_eoe('$2', '$1'))).
#   defp grammar_expr_list_eoe(state, max_forms) do
#     StreamData.bind(StreamData.integer(1..max_forms), fn count ->
#       StreamData.bind(gen_expr_list(state, count), fn exprs ->
#         StreamData.bind(gen_eoe(), fn trailing_eoe ->
#           StreamData.constant({:grammar_v2, nil, exprs, trailing_eoe})
#         end)
#       end)
#     end)
#   end

#   # grammar -> eoe expr_list : build_block(reverse('$2')).
#   defp grammar_eoe_expr_list(state, max_forms) do
#     StreamData.bind(eoe(), fn leading_eoe ->
#       StreamData.bind(StreamData.integer(1..max_forms), fn count ->
#         gen_expr_list(state, count)
#       end)
#       |> StreamData.map(fn exprs -> {:grammar_v2, leading_eoe, exprs, nil} end)
#     end)
#   end

#   # grammar -> eoe expr_list eoe : build_block(reverse(annotate_eoe('$3', '$2'))).
#   defp grammar_eoe_expr_list_eoe(state, max_forms) do
#     StreamData.bind(eoe(), fn leading_eoe ->
#       StreamData.bind(StreamData.integer(1..max_forms), fn count ->
#         StreamData.bind(gen_expr_list(state, count), fn exprs ->
#           StreamData.bind(eoe(), fn trailing_eoe ->
#             StreamData.constant({:grammar_v2, leading_eoe, exprs, trailing_eoe})
#           end)
#         end)
#       end)
#     end)
#   end

#   # grammar -> eoe : {'__block__', meta_from_token('$1'), []}.
#   # Represented as grammar_v2 with leading_eoe and empty expr list.
#   defp gen_grammar_eoe_only(_state) do
#     StreamData.bind(gen_eoe(), fn leading_eoe ->
#       StreamData.constant({:grammar_v2, leading_eoe, [], nil})
#     end)
#   end

#   # expr_list -> expr : ['$1'].
#   # expr_list -> expr_list eoe expr : ['$3' | annotate_eoe('$2', '$1')].
#   defp expr_list(state, 1) do
#     expr(state)
#   end

#   defp expr_list(state, count) when count > 1 do
#     # First expr has eoe after it (between this and next)
#     StreamData.bind(expr(state), fn expr ->
#       StreamData.bind(eoe(), fn eoe ->
#         StreamData.bind(expr_list(state, count - 1), fn rest ->
#           StreamData.constant([expr, eoe | rest])
#         end)
#       end)
#     end)
#   end

#   # expr -> matched_expr | no_parens_expr | unmatched_expr
#   defp gen_expr(state) do
#     if GrammarTree.budget_exhausted?(state) do
#       # Literal is a matched_expr
#       gen_fallback_literal()
#     else
#       if state.context.allow_unmatched or state.context.allow_no_parens do
#         # Top-level context: can generate any expression type
#         # Per grammar: expr -> matched_expr | no_parens_expr | unmatched_expr
#         StreamData.frequency(
#           [
#             {6, gen_matched_expr(state)}
#           ] ++
#             if(state.context.allow_unmatched, do: [{3, gen_unmatched_expr(state)}], else: []) ++
#             if(state.context.allow_no_parens, do: [{2, gen_no_parens_expr(state)}], else: [])
#         )
#       else
#         # Restricted context (e.g., operand position): only matched
#         gen_matched_expr(state)
#       end
#     end
#   end

#   # eoe -> eol : '$1'.
#   # eoe -> ';' : '$1'.
#   # eoe -> eol ';' : '$1'.
#   defp eoe do
#     StreamData.frequency([
#       {6, StreamData.constant(:eol)},
#       {5, StreamData.constant(:semi)},
#       {4, StreamData.constant(:eol_semi)}
#     ])
#   end

#   defp matched_expr(state) do
#     if GrammarTree.budget_exhausted?(state) do
#       sub_matched_expr(state)
#     else
#       StreamData.frequency([
#         {4, sub_matched_expr(state)},
#         # {3, gen_matched_op(state)},
#         # {2, gen_matched_unary(state)},
#         # {1, gen_at_op(state)},
#         # {1, gen_capture_op(state)},
#         # {1, gen_ellipsis_prefix(state)},
#         # {1, gen_call_no_parens_one(state)},
#         # # Range with step: 1..10//2 (ternary_op is only valid after range_op)
#         # {1, gen_range_step(state)},
#         # # Arrow + no_parens_one: foo 1 |> bar 2 (warn_pipe pattern)
#         # {1, gen_matched_op_warn_pipe(state)}
#       ])
#     end
#   end

#   defp sub_matched_expr(state) do
#     StreamData.frequency([
#       # {9, gen_access_expr(state)},
#       {5, no_parens_zero_expr(state)},
#       # {1, gen_nullary_range()},
#       # {1, gen_nullary_ellipsis()},
#       # # Access expression followed by a keyword identifier (invalid in grammar)
#       # # This models: access_expr kw_identifier -> error_invalid_kw_identifier('$2')
#       # {1, gen_access_expr_kw_identifier(state)}
#     ])
#   end

#   def no_parens_zero_expr(state) do
#     child_state = GrammarTree.decr_depth(state)

#     if child_state.budget.depth <= 1 do
#       # At low depth, only simple forms
#       StreamData.frequency([
#         {6, identifier()},
#         # {2, gen_dot_identifier_simple()},
#         # {1, gen_dot_do_identifier_simple()}
#       ])
#     else
#       # At higher depth, allow matched_expr on left side
#       StreamData.frequency([
#         # Simple identifier (most common)
#         {6, gen_identifier()},
#         # # Dotted identifier: expr.identifier (e.g., foo.bar, Mod.func, (a+b).foo)
#         # {2, gen_dot_identifier_full(child_state)},
#         # # Do identifier: if, unless, case, etc. (as bare or dotted identifiers)
#         # {1, gen_dot_do_identifier_full(child_state)}
#       ])
#     end
#   end
# end
