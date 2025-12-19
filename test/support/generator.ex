defmodule ToxicParser.Generator do
  @moduledoc false

  import StreamData

  @identifiers ~w(foo bar baz qux spam eggs alpha beta gamma delta)a

  # Minimal grammar-faithful token generator (starter subset).
  #
  # Generates *Toxic streaming tokens* (with ranged metas) in a way that matches
  # Toxic's lexer behavior for this subset (notably: range_op carries leading EOL
  # count in meta.extra and does not emit a standalone :eol token).

  def tokens_gen(opts \\ []) do
    grammar_raw(opts)
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
  defp expr_raw(state), do: matched_expr_raw(state)

  # matched_expr -> sub_matched_expr
  defp matched_expr_raw(state), do: sub_matched_expr_raw(state)

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

  defp no_parens_zero_expr_raw(_state), do: identifier_raw()

  defp identifier_raw do
    member_of(@identifiers)
    |> map(fn atom -> [{:identifier, atom}] end)
  end

  # Range op tokens in Toxic carry *leading* EOL count in meta.extra and do not emit a standalone
  # :eol token. We represent it as {:range_op, :.., leading_eol_count}.
  defp range_op_raw, do: constant([{:range_op, :.., 0}])

  defp ellipsis_op_raw, do: constant([{:ellipsis_op, :...}])

  # Minimal subset of access_expr (yrl has many more).
  defp access_expr_raw(state) do
    frequency([
      {5, empty_paren_raw()},
      {3, int_raw()},
      {1, true_raw()},
      {1, false_raw()},
      {1, nil_raw()},
      {3, parens_raw(state)}
    ])
  end

  defp empty_paren_raw, do: constant([:lparen, :rparen])

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
    {tokens, _pos} = Enum.map_reduce(raw_tokens, {1, 1}, &materialize_token/2)
    tokens
  end

  defp materialize_token({:eol, n}, {line, col}) do
    start = {line, col}
    stop = {line + n, 1}
    {{:eol, {start, stop, n}}, stop}
  end

  defp materialize_token(:semi, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:";", {start, stop, 0}}, stop}
  end

  defp materialize_token(:lparen, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:"(", {start, stop, nil}}, stop}
  end

  defp materialize_token(:rparen, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:")", {start, stop, nil}}, stop}
  end

  defp materialize_token({:identifier, atom}, {line, col}) do
    text = Atom.to_string(atom)
    start = {line, col}
    stop = {line, col + String.length(text)}
    {{:identifier, {start, stop, nil}, atom}, stop}
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
