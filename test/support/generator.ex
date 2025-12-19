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

  # no_parens_zero_expr -> dot_do_identifier | dot_identifier
  defp no_parens_zero_expr_raw(state) do
    frequency([
      {2, dot_do_identifier_raw(state)},
      {8, dot_identifier_raw(state)}
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

  defp ellipsis_op_raw, do: constant([{:ellipsis_op, :...}])

  # Minimal subset of access_expr (yrl has many more).
  # Focus: bracket_at_expr, bracket_expr, capture_int int, flt, char, atom, dot_alias,
  # list/tuple/bitstring.
  defp access_expr_raw(state) do
    frequency([
      {5, empty_paren_raw()},
      {3, list_raw(state)},
      {2, tuple_raw(state)},
      {2, bitstring_raw(state)},
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

  defp bracket_expr_raw(_state) do
    bind(bracket_identifier_raw(), fn lhs ->
      bind(identifier_raw(), fn idx ->
        constant(lhs ++ [:lbracket] ++ idx ++ [:rbracket])
      end)
    end)
  end

  defp bracket_at_expr_raw(_state) do
    bind(bracket_identifier_raw(), fn lhs ->
      bind(identifier_raw(), fn idx ->
        # If we ever decide to allow eol between @ and identifier, we'd need more rules.
        constant([:at] ++ lhs ++ [:lbracket] ++ idx ++ [:rbracket])
      end)
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

  defp unmatched_expr_raw(state), do: matched_expr_raw(state)

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

  # Toxic folds leading EOLs into :range_op meta.extra (no standalone :eol token).
  defp do_coalesce_eols([{:eol, n}, {:range_op, op, extra} | rest], acc) do
    extra = if is_integer(extra), do: extra, else: 0
    do_coalesce_eols([{:range_op, op, extra + n} | rest], acc)
  end

  # Toxic folds newlines after comma into the comma token meta (no standalone :eol token).
  defp do_coalesce_eols([:comma, {:eol, n} | rest], acc), do: do_coalesce_eols([{:comma, n} | rest], acc)
  defp do_coalesce_eols([{:comma, a}, {:eol, b} | rest], acc), do: do_coalesce_eols([{:comma, a + b} | rest], acc)

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

  defp materialize_token(:semi, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:";", {start, stop, 0}}, stop}
  end

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

  defp materialize_token(:rparen, {line, col}) do
    start = {line, col}
    stop = {line, col + 1}
    {{:")", {start, stop, nil}}, stop}
  end

  defp materialize_token({:identifier, atom}, {line, col}) do
    text = Atom.to_string(atom)
    start = {line, col}
    stop = {line, col + String.length(text)}
    {{:identifier, {start, stop, Atom.to_charlist(atom)}, atom}, stop}
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
