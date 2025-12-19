defmodule ToxicParser.Generator do
  @moduledoc false

  import StreamData

  @identifiers ~w(foo bar baz qux spam eggs alpha beta gamma delta)a

  # Minimal grammar-faithful token generator (starter subset).
  #
  # Implements (from elixir_parser.yrl):
  #   grammar -> eoe
  #          -> expr_list
  #          -> eoe expr_list
  #          -> expr_list eoe
  #          -> eoe expr_list eoe
  #          -> '$empty'
  #
  #   expr_list -> expr
  #            -> expr_list eoe expr
  #
  #   eoe -> eol | ';' | eol ';'
  #
  #   expr -> matched_expr
  #   matched_expr -> sub_matched_expr
  #   sub_matched_expr -> identifier | int | '(' grammar ')'
  #
  # NOTE: metas are stubbed (fixed positions). The property test compares
  # normalized token sequences (ignoring meta ranges), so this is fine for now.

  def tokens_gen(opts \\ []) do
    grammar(opts)
  end

  def grammar(opts \\ []) do
    state = %{
      depth: Keyword.get(opts, :depth, 2),
      max_forms: Keyword.get(opts, :max_forms, 3)
    }

    grammar_state(state)
  end

  defp grammar_state(state) do
    frequency([
      {5, grammar_expr_list(state)},
      {3, grammar_expr_list_eoe(state)},
      {3, grammar_eoe_expr_list(state)},
      {1, grammar_eoe_expr_list_eoe(state)},
      {1, grammar_eoe_only()},
      {1, grammar_empty()}
    ])
  end

  # grammar -> '$empty'
  defp grammar_empty, do: constant([])

  # grammar -> eoe
  defp grammar_eoe_only do
    eoe()
  end

  # grammar -> expr_list
  defp grammar_expr_list(state) do
    expr_list(state)
  end

  # grammar -> expr_list eoe
  defp grammar_expr_list_eoe(state) do
    bind(expr_list(state), fn exprs ->
      bind(eoe(), fn trailing ->
        constant(exprs ++ trailing)
      end)
    end)
  end

  # grammar -> eoe expr_list
  defp grammar_eoe_expr_list(state) do
    bind(eoe(), fn leading ->
      map(expr_list(state), fn exprs ->
        leading ++ exprs
      end)
    end)
  end

  # grammar -> eoe expr_list eoe
  defp grammar_eoe_expr_list_eoe(state) do
    bind(eoe(), fn leading ->
      bind(expr_list(state), fn exprs ->
        bind(eoe(), fn trailing ->
          constant(leading ++ exprs ++ trailing)
        end)
      end)
    end)
  end

  # expr_list -> expr
  # expr_list -> expr_list eoe expr
  defp expr_list(%{max_forms: max_forms} = state) do
    bind(integer(1..max_forms), fn count ->
      expr_list_n(state, count)
    end)
  end

  defp expr_list_n(state, 1), do: expr(state)

  defp expr_list_n(state, count) when count > 1 do
    bind(expr_list_n(state, count - 1), fn prev ->
      bind(eoe(), fn sep ->
        bind(expr(state), fn last ->
          constant(prev ++ sep ++ last)
        end)
      end)
    end)
  end

  # expr -> matched_expr
  defp expr(state), do: matched_expr(state)

  # matched_expr -> sub_matched_expr
  defp matched_expr(state), do: sub_matched_expr(state)

  # sub_matched_expr -> identifier | int | '(' grammar ')'
  defp sub_matched_expr(%{depth: depth} = state) do
    if depth <= 0 do
      identifier_expr()
    else
      frequency([
        {10, identifier_expr()},
        {2, int_expr()},
        {3, parens_expr(decr_depth(state))}
      ])
    end
  end

  # eoe -> eol | ';' | eol ';'
  defp eoe do
    frequency([
      {6, eol()},
      {5, semi()},
      {4, eol_semi()}
    ])
  end

  defp eol do
    integer(1..2)
    |> map(fn n -> [{:eol, meta(n)}] end)
  end

  defp semi do
    constant([{:";", meta()}])
  end

  defp eol_semi do
    bind(eol(), fn eol_tokens ->
      map(semi(), fn semi_tokens -> eol_tokens ++ semi_tokens end)
    end)
  end

  defp identifier_expr do
    member_of(@identifiers)
    |> map(fn atom -> [{:identifier, meta(), atom}] end)
  end

  defp int_expr do
    integer(0..100)
    |> map(fn n -> [{:int, meta(n), Integer.to_charlist(n)}] end)
  end

  # '(' grammar ')'
  defp parens_expr(state) do
    bind(grammar_state(state), fn inner ->
      constant([{:"(", meta()}] ++ inner ++ [{:")", meta()}])
    end)
  end

  defp decr_depth(%{depth: depth} = state), do: %{state | depth: depth - 1}

  defp meta(extra \\ nil), do: {{1, 1}, {1, 1}, extra}
end
