defmodule ToxicParser.Grammar.Maps do
  @moduledoc """
  Parsing for maps and structs, including updates inside `%{}`.
  """

  alias ToxicParser.{Builder, Context, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.{Delimited, EOE, Keywords}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @spec parse_map(State.t(), Pratt.context(), EventLog.t(), non_neg_integer()) :: result()
  def parse_map(%State{} = state, %Context{} = ctx, %EventLog{} = log, min_bp \\ 0) do
    with {:ok, ast, state, log} <- parse_map_base(state, ctx, log) do
      # Continue with Pratt.led to handle trailing operators
      Pratt.led(ast, state, log, min_bp, ctx)
    end
  end

  # Parse map without calling Pratt.led - internal implementation
  defp parse_map_base(state, ctx, log) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: :%{}, metadata: meta}, state} ->
        # map -> map_op map_args where map_op is '%{}'
        # The tokenizer emits %{} as three tokens: :%{}, :"{", :"}"
        # We need to consume the "{" token (the "}" will be consumed by parse_map_args_body)
        percent_meta = token_meta(meta)
        # Consume the opening brace token
        case TokenAdapter.next(state) do
          {:ok, %{kind: :"{"}, state} ->
            # For %{} maps, use percent_meta for position (not brace position)
            # Use a tuple sentinel that can never be a valid struct name
            parse_map_args_after_brace(
              {:__no_struct_base__},
              percent_meta,
              percent_meta,
              state,
              ctx,
              log
            )

          {:ok, tok, state} ->
            {:error, {:expected, :"{", got: tok.kind}, state, log}

          {:eof, state} ->
            {:error, :unexpected_eof, state, log}

          {:error, diag, state} ->
            {:error, diag, state, log}
        end

      {:ok, %{kind: :%, metadata: meta}, state} ->
        # map -> '%' map_base_expr map_args (struct)
        percent_meta = token_meta(meta)
        # Skip optional EOE after %
        state = EOE.skip(state)
        # Parse map_base_expr (the struct name)
        with {:ok, base, state, log} <- parse_map_base_expr(state, ctx, log) do
          # Skip optional EOE after base
          state = EOE.skip(state)
          parse_map_args(base, percent_meta, state, ctx, log)
        end

      other ->
        other
    end
  end

  # map_base_expr: sub_matched_expr, or unary ops applied to map_base_expr
  # sub_matched_expr is a BASE expression without trailing operators
  # Note: dual_op (+ and -) can be used as unary operators here
  # Note: ternary_op (//) used as unary becomes {:/, outer, [{:/, inner, nil}, rhs]}
  #
  # Special handling for dotted aliases: %Foo.Bar{} requires parsing the full
  # alias chain Foo.Bar before {. We use parse_base_with_dots which handles
  # dot operators for alias chaining but does NOT consume {} as call arguments.
  defp parse_map_base_expr(state, _ctx, log) do
    case TokenAdapter.peek(state) do
      # Special case for @ (at_op): precedence 320 > dot 310
      # So @0.a should be (@0).a - @ takes just the base, then . applies
      {:ok, %{kind: :at_op} = _tok, _} ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        with {:ok, operand, state, log} <- parse_unary_operand(state, Context.matched_expr(), log) do
          op_meta = token_meta(op_tok.metadata)
          ast = {op_tok.value, op_meta, [operand]}
          # Continue with dots/calls to handle trailing .foo or .Bar
          Pratt.led_dots_and_calls(ast, state, log, Context.matched_expr())
        end

      # Other unary ops (^, !, not, +, -, ...): precedence < dot 310
      # So ^t.d should be ^(t.d) - dots are part of the operand
      {:ok, %{kind: kind} = _tok, _} when kind in [:unary_op, :ellipsis_op, :dual_op] ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        # Parse operand with dots applied (since dot binds tighter)
        with {:ok, operand, state, log} <-
               parse_unary_operand_with_dots(state, Context.matched_expr(), log) do
          op_meta = token_meta(op_tok.metadata)
          ast = {op_tok.value, op_meta, [operand]}
          {:ok, ast, state, log}
        end

      # ternary_op :"//" used as unary prefix (e.g., %//foo{})
      # Also has precedence < dot 310, so dots are part of the operand
      {:ok, %{kind: :ternary_op, value: :"//"} = _tok, _} ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        with {:ok, operand, state, log} <-
               parse_unary_operand_with_dots(state, Context.matched_expr(), log) do
          op_meta = token_meta(op_tok.metadata)
          # Calculate inner/outer metadata with column adjustment
          {outer_meta, inner_meta} =
            case {Keyword.get(op_meta, :line), Keyword.get(op_meta, :column)} do
              {line, col} when is_integer(line) and is_integer(col) ->
                {[line: line, column: col + 1], [line: line, column: col]}

              _ ->
                {op_meta, op_meta}
            end

          ast = {:/, outer_meta, [{:/, inner_meta, nil}, operand]}
          {:ok, ast, state, log}
        end

      # Parenthesized expression or list/tuple/bitstring as struct base
      # Grammar allows these as sub_matched_expr -> access_expr -> list | tuple | bitstring | empty_paren
      # Example: %!(){} where () is the operand of !
      # Example: %@[]{} where [] is the operand of @
      {:ok, %{kind: kind}, _} when kind in [:"(", :"[", :"{", :"<<"] ->
        alias ToxicParser.Grammar.Containers
        Containers.parse(state, Context.matched_expr(), log)

      _ ->
        # sub_matched_expr with dot operator and paren call support
        # parse_base_with_dots_and_calls handles:
        #   - Foo.Bar.Baz (dotted aliases)
        #   - unquote(struct) (paren calls)
        #   - module.Foo.Bar (dotted calls)
        # But stops at { which is the struct body
        Pratt.parse_base_with_dots_and_calls(state, Context.matched_expr(), log)
    end
  end

  # Parse the operand of a unary operator in struct base context.
  # This parses just the BASE expression without dots.
  # For example, in %@0.a{}, the operand of @ is just 0, not 0.a.
  # The .a is handled separately after building the unary expression.
  defp parse_unary_operand(state, ctx, log) do
    case TokenAdapter.peek(state) do
      # For nested @ operator, continue without dots since @ (320) > dot (310)
      {:ok, %{kind: :at_op} = _tok, _} ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        with {:ok, operand, state, log} <- parse_unary_operand(state, ctx, log) do
          op_meta = token_meta(op_tok.metadata)
          ast = {op_tok.value, op_meta, [operand]}
          {:ok, ast, state, log}
        end

      # For other unary operators (precedence < dot 310), use parse_unary_operand_with_dots
      # so that dots are included in their operand.
      # E.g., %@+a.i{} should be @(+(a.i)){} not (@(+a)).i{}
      {:ok, %{kind: kind} = _tok, _} when kind in [:unary_op, :ellipsis_op, :dual_op] ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        with {:ok, operand, state, log} <- parse_unary_operand_with_dots(state, ctx, log) do
          op_meta = token_meta(op_tok.metadata)
          ast = {op_tok.value, op_meta, [operand]}
          {:ok, ast, state, log}
        end

      # ternary_op :"//" used as unary prefix (precedence < dot 310)
      {:ok, %{kind: :ternary_op, value: :"//"} = _tok, _} ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        with {:ok, operand, state, log} <- parse_unary_operand_with_dots(state, ctx, log) do
          op_meta = token_meta(op_tok.metadata)

          {outer_meta, inner_meta} =
            case {Keyword.get(op_meta, :line), Keyword.get(op_meta, :column)} do
              {line, col} when is_integer(line) and is_integer(col) ->
                {[line: line, column: col + 1], [line: line, column: col]}

              _ ->
                {op_meta, op_meta}
            end

          ast = {:/, outer_meta, [{:/, inner_meta, nil}, operand]}
          {:ok, ast, state, log}
        end

      # Container as operand (e.g., %@[]{} where [] is the operand)
      {:ok, %{kind: kind}, _} when kind in [:"(", :"[", :"{", :"<<"] ->
        alias ToxicParser.Grammar.Containers
        Containers.parse(state, ctx, log)

      # Paren call as operand (e.g., %@i(){} where i() is the operand)
      # Parse identifier then consume () as a paren call
      {:ok, %{kind: :paren_identifier} = tok, _} ->
        {:ok, _tok, state} = TokenAdapter.next(state)
        Pratt.parse_paren_call_base(tok, state, ctx, log)

      # Base expression (literal, identifier, etc.) - parse without dots
      _ ->
        Pratt.parse_base(state, ctx, log)
    end
  end

  # Parse a unary operand with dots applied (for unary ops with precedence < dot).
  # Similar to parse_unary_operand but applies dots to the base expression.
  # Used for operators like ^, !, not where %^t.d{} should parse as %^(t.d){}.
  defp parse_unary_operand_with_dots(state, ctx, log) do
    case TokenAdapter.peek(state) do
      # Nested @ - special handling since @ binds tighter than dot
      {:ok, %{kind: :at_op} = _tok, _} ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        with {:ok, operand, state, log} <- parse_unary_operand(state, ctx, log) do
          op_meta = token_meta(op_tok.metadata)
          ast = {op_tok.value, op_meta, [operand]}
          # Apply dots after @ since @ binds tighter
          Pratt.led_dots_and_calls(ast, state, log, ctx)
        end

      # Other unary ops - recurse with dots
      {:ok, %{kind: kind} = _tok, _} when kind in [:unary_op, :ellipsis_op, :dual_op] ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        with {:ok, operand, state, log} <- parse_unary_operand_with_dots(state, ctx, log) do
          op_meta = token_meta(op_tok.metadata)
          ast = {op_tok.value, op_meta, [operand]}
          {:ok, ast, state, log}
        end

      # ternary_op :"//" used as unary prefix
      {:ok, %{kind: :ternary_op, value: :"//"} = _tok, _} ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        with {:ok, operand, state, log} <- parse_unary_operand_with_dots(state, ctx, log) do
          op_meta = token_meta(op_tok.metadata)

          {outer_meta, inner_meta} =
            case {Keyword.get(op_meta, :line), Keyword.get(op_meta, :column)} do
              {line, col} when is_integer(line) and is_integer(col) ->
                {[line: line, column: col + 1], [line: line, column: col]}

              _ ->
                {op_meta, op_meta}
            end

          ast = {:/, outer_meta, [{:/, inner_meta, nil}, operand]}
          {:ok, ast, state, log}
        end

      # Container as operand
      {:ok, %{kind: kind}, _} when kind in [:"(", :"[", :"{", :"<<"] ->
        alias ToxicParser.Grammar.Containers
        Containers.parse(state, ctx, log)

      # Base expression - parse and apply dots
      _ ->
        with {:ok, base, state, log} <- Pratt.parse_base(state, ctx, log) do
          Pratt.led_dots_and_calls(base, state, log, ctx)
        end
    end
  end

  # Parse map_args: { } | { map_close } | { assoc_update ... }
  defp parse_map_args(base, percent_meta, state, ctx, log) do
    # Consume the opening brace
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"{", metadata: open_meta}, _} ->
        {:ok, _open, state} = TokenAdapter.next(state)
        brace_meta = token_meta(open_meta)
        # Skip leading EOE and count newlines
        {state, leading_newlines} = EOE.skip_count_newlines(state, 0)
        parse_map_args_body(base, percent_meta, brace_meta, leading_newlines, state, ctx, log)

      {:ok, tok, state} ->
        {:error, {:expected, :"{", got: tok.kind}, state, log}

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Called after the opening brace has been consumed (for %{} case)
  defp parse_map_args_after_brace(base, percent_meta, brace_meta, state, ctx, log) do
    # Skip leading EOE and count newlines
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)
    parse_map_args_body(base, percent_meta, brace_meta, leading_newlines, state, ctx, log)
  end

  defp parse_map_args_body(base, percent_meta, brace_meta, leading_newlines, state, ctx, log) do
    case TokenAdapter.peek(state) do
      # Empty map: map_args -> open_curly '}'
      {:ok, %{kind: :"}"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        map_meta = Meta.closing_meta(brace_meta, close_meta, leading_newlines)
        {:ok, build_map_ast(base, [], percent_meta, map_meta), state, log}

      {:ok, _, _} ->
        # Try to parse map update first, then fall back to regular entries
        case try_parse_map_update(state, ctx, log) do
          {:ok, update_ast, close_meta, state, log} ->
            map_meta = Meta.closing_meta(brace_meta, close_meta, leading_newlines)
            {:ok, build_map_update_ast(base, update_ast, percent_meta, map_meta), state, log}

          {:not_update, state} ->
            # Parse map_close: kw_data | assoc | assoc_base ',' kw_data
            with {:ok, entries, close_meta, state, log} <- parse_map_close(state, ctx, log) do
              map_meta = Meta.closing_meta(brace_meta, close_meta, leading_newlines)
              {:ok, build_map_ast(base, entries, percent_meta, map_meta), state, log}
            end

          {:error, _, _, _} = err ->
            err
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Try to parse assoc_update: matched_expr pipe_op_eol assoc_expr
  # Returns {:ok, update_ast, close_meta, state, log} or :not_update or {:error, ...}
  #
  # Grammar rules:
  #   assoc_update -> matched_expr pipe_op_eol assoc_expr
  #   assoc_update -> unmatched_expr pipe_op_eol assoc_expr
  #   assoc_update_kw -> matched_expr pipe_op_eol kw_data
  #   assoc_update_kw -> unmatched_expr pipe_op_eol kw_data
  #
  # Strategy:
  # 1. Parse the base expression (matched_expr/unmatched_expr)
  # 2. Check for pipe_op token (|)
  # 3. If | found, check what follows:
  #    - kw_data (keyword list) -> map update with keywords
  #    - assoc_expr (k => v) -> map update with assoc expressions
  # 4. op_identifier calls (like c!) consume | as part of their argument,
  #    so if we still see | after parsing base, it's the map update separator
  defp try_parse_map_update(state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :kw_identifier}, _} ->
        {:not_update, state}

      _ ->
        {checkpoint_id, checkpoint_state} = TokenAdapter.checkpoint(state)

        case parse_map_update_candidate(checkpoint_state, ctx, log) do
          {:ok, update_ast, close_meta, state, log} ->
            {:ok, update_ast, close_meta, TokenAdapter.drop_checkpoint(state, checkpoint_id), log}

          {:not_update, state} ->
            {:not_update, TokenAdapter.rewind(state, checkpoint_id)}

          {:error, diag, state, log} ->
            {:error, diag, TokenAdapter.rewind(state, checkpoint_id), log}
        end
    end
  end

  # Binding power just above pipe_op (70) to prevent | from being consumed
  @pipe_op_bp 70

  defp parse_map_update_candidate(state, _ctx, log) do
    starts_with_quoted_kw_key? =
      case TokenAdapter.peek(state) do
        {:ok, %{kind: kind}, _} when kind in [:bin_string_start, :list_string_start] -> true
        _ -> false
      end

    # First, try parsing the full expression without restricting |
    # This allows op_identifier calls (like c!) to consume | as part of their argument
    # e.g., %{c!s|n => 1} should parse as c!(s|n) => 1, not c!s | n => 1
    case Pratt.parse(state, Context.container_expr(), log) do
      {:ok, base_expr, state, log} ->
        # Check if the parsed expression IS a pipe expression with valid map update RHS
        # e.g., map | :a => 1 parses as {:|, _, [map, {:"=>", _, [:a, 1]}]}
        case check_embedded_map_update(base_expr) do
          {:update, base, pipe_meta, rhs_entries} ->
            # The expression itself was a map update pattern
            state = EOE.skip(state)

            case TokenAdapter.peek(state) do
              {:ok, %{kind: :"}"} = close_tok, _} ->
                {:ok, _close, state} = TokenAdapter.next(state)
                close_meta = token_meta(close_tok.metadata)
                update_ast = {:|, pipe_meta, [base, rhs_entries]}
                {:ok, update_ast, close_meta, state, log}

              {:ok, %{kind: :","}, _} ->
                # More entries after initial: %{base | k => v, more...}
                {:ok, _comma, state} = TokenAdapter.next(state)
                state = EOE.skip(state)
                parse_map_update_trailing_entries(base, pipe_meta, rhs_entries, state, log)

              _ ->
                {:not_update, state}
            end

          :not_update ->
            # Check for | token AFTER the expression
            {state, newlines} = EOE.skip_count_newlines(state, 0)

            case TokenAdapter.peek(state) do
              {:ok, %{kind: :pipe_op, metadata: pipe_meta}, _} ->
                # Found | - this is potentially a map update
                {:ok, _pipe, state} = TokenAdapter.next(state)
                state = EOE.skip(state)
                pipe_meta_kw = token_meta_with_newlines(pipe_meta, newlines)
                parse_map_update_rhs(base_expr, pipe_meta_kw, state, log)

              _ ->
                # No | found - not a map update
                {:not_update, state}
            end
        end

      {:keyword_key, _, _state, _} ->
        if starts_with_quoted_kw_key? do
          {:not_update, state}
        else
          parse_map_update_with_min_bp(state, log)
        end

      {:keyword_key_interpolated, _, _, _, _, _state, _} ->
        if starts_with_quoted_kw_key? do
          {:not_update, state}
        else
          parse_map_update_with_min_bp(state, log)
        end

      {:error, _diag, _state, _log} ->
        # Parsing failed - likely due to keyword after |
        # Retry with min_bp above pipe_op to not consume |
        parse_map_update_with_min_bp(state, log)
    end
  end

  # Check if an expression is a pipe expression with valid map update RHS
  # e.g., map | :a => 1 parses as {:|, meta, [map, {:"=>", _, [:a, 1]}]}
  # Returns {:update, base, pipe_meta, entries} or :not_update
  defp check_embedded_map_update({:|, pipe_meta, [base, rhs]}) do
    # Parenthesized pipes inside %{} are regular pipe expressions, not map updates.
    if Keyword.has_key?(pipe_meta, :parens) do
      :not_update
    else
      case classify_pipe_rhs_for_map_update(rhs) do
        {:valid, entries} -> {:update, base, pipe_meta, entries}
        :invalid -> :not_update
      end
    end
  end

  defp check_embedded_map_update(_), do: :not_update

  # Check if RHS of | is valid for map update
  # Valid: single assoc {:"=>", _, [k, v]} or keyword list
  # Annotates keys with :assoc metadata indicating the position of =>
  defp classify_pipe_rhs_for_map_update({:"=>", assoc_meta, [key, value]} = _assoc) do
    annotated_key = annotate_assoc(key, assoc_meta)
    {:valid, [{annotated_key, value}]}
  end

  # Handle nested pipe in map update RHS: a | b | c => d
  # Pratt produces: {:|, _, [a, {:|, _, [b, {:"=>", _, [c, d]}]}]}
  # The RHS of first | is: {:|, _, [b, {:"=>", _, [c, d]}]}
  # This should become: [{(b | c), d}] - key is b | c, value is d
  # Use extract_assoc to properly handle any level of nesting
  defp classify_pipe_rhs_for_map_update({:|, _, _} = expr) do
    case extract_assoc(expr) do
      {:assoc, key, value, assoc_meta} ->
        annotated_key = annotate_assoc(key, assoc_meta)
        {:valid, [{annotated_key, value}]}

      :not_assoc ->
        :invalid
    end
  end

  defp classify_pipe_rhs_for_map_update(list) when is_list(list) do
    cond do
      list == [] ->
        # Empty list literal (e.g. ''), treated as a map_base_expr, wrapped.
        {:valid, [list]}

      Enum.all?(list, &is_keyword_or_assoc_entry?/1) ->
        # Annotate keys in assoc entries with :assoc metadata
        annotated = Enum.map(list, &annotate_assoc_entry/1)
        {:valid, annotated}

      true ->
        # A non-keyword list literal is a valid map_base_expr on the RHS.
        {:valid, [list]}
    end
  end

  # Fallback: if RHS contains a rightmost assoc (=>), treat it as a single assoc entry.
  # Otherwise it's a map_base_expr, wrapped in a list.
  defp classify_pipe_rhs_for_map_update(rhs) do
    case extract_assoc(rhs) do
      {:assoc, key, value, assoc_meta} ->
        {:valid, [{annotate_assoc(key, assoc_meta), value}]}

      :not_assoc ->
        {:valid, [rhs]}
    end
  end

  # Annotate a single assoc entry with :assoc metadata
  defp annotate_assoc_entry({:"=>", assoc_meta, [key, value]}) do
    {annotate_assoc(key, assoc_meta), value}
  end

  defp annotate_assoc_entry(entry), do: entry

  defp is_keyword_or_assoc_entry?({key, _value}) when is_atom(key), do: true
  defp is_keyword_or_assoc_entry?({{_expr, _meta, _args}, _value}), do: true
  defp is_keyword_or_assoc_entry?({:"=>", _meta, [_k, _v]}), do: true
  defp is_keyword_or_assoc_entry?(_), do: false

  # Parse trailing entries after map update: %{base | k => v, more...}
  defp parse_map_update_trailing_entries(base, pipe_meta, initial_entries, state, log) do
    case TokenAdapter.peek(state) do
      # Trailing comma case: %{base | k => v,}
      {:ok, %{kind: :"}"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        update_ast = {:|, pipe_meta, [base, initial_entries]}
        {:ok, update_ast, close_meta, state, log}

      {:ok, tok, _} ->
        _tok = tok

        # map_close already handles:
        # - kw_data close_curly
        # - assoc close_curly
        # - assoc_base ',' kw_data close_curly
        with {:ok, more_entries, close_meta, state, log} <-
               parse_map_close(state, Context.container_expr(), log) do
          all_entries = initial_entries ++ more_entries
          update_ast = {:|, pipe_meta, [base, all_entries]}
          {:ok, update_ast, close_meta, state, log}
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Fallback: parse with min_bp restriction to handle cases like %{map | a: 1}
  # where | is followed by keyword data (not valid as binary operator RHS)
  defp parse_map_update_with_min_bp(state, log) do
    case Pratt.parse_with_min_bp(state, Context.container_expr(), log, @pipe_op_bp + 1) do
      {:ok, base_expr, state, log} ->
        {state, newlines} = EOE.skip_count_newlines(state, 0)

        case TokenAdapter.peek(state) do
          {:ok, %{kind: :pipe_op, metadata: pipe_meta}, _} ->
            {:ok, _pipe, state} = TokenAdapter.next(state)
            state = EOE.skip(state)
            pipe_meta_kw = token_meta_with_newlines(pipe_meta, newlines)
            parse_map_update_rhs(base_expr, pipe_meta_kw, state, log)

          _ ->
            {:not_update, state}
        end

      {:keyword_key, _, state, _} ->
        {:not_update, state}

      {:keyword_key_interpolated, _, _, _, _, state, _} ->
        {:not_update, state}

      {:error, diag, state, log} ->
        {:error, diag, state, log}
    end
  end

  # Parse the RHS of a map update (after the |)
  # Can be kw_data or assoc_expr(s)
  defp parse_map_update_rhs(base, pipe_meta, state, log) do
    container_ctx = Context.container_expr()

    case Keywords.try_parse_kw_data(state, container_ctx, log) do
      {:ok, kw_list, state, log} ->
        # assoc_update_kw -> matched_expr pipe_op_eol kw_data
        # assoc_update_kw -> unmatched_expr pipe_op_eol kw_data
        state = EOE.skip(state)

        case TokenAdapter.peek(state) do
          {:ok, %{kind: :"}"} = close_tok, _} ->
            {:ok, _close, state} = TokenAdapter.next(state)
            close_meta = token_meta(close_tok.metadata)
            update_ast = {:|, pipe_meta, [base, kw_list]}
            {:ok, update_ast, close_meta, state, log}

          _ ->
            {:not_update, state}
        end

      {:no_kw, state, log} ->
        # assoc_expr(s): %{base | k => v, ...}
        parse_map_update_assoc_entries(base, pipe_meta, [], state, log)

      {:error, reason, state, log} ->
        {:error, reason, state, log}
    end
  end

  # Parse assoc entries for map update RHS
  defp parse_map_update_assoc_entries(base, pipe_meta, acc, state, log) do
    with {:ok, entry, state, log} <- parse_assoc_expr(state, Context.container_expr(), log) do
      # Verify it's actually an assoc (has =>)
      case entry do
        {key, value} ->
          # It's a proper assoc entry
          state = EOE.skip(state)

          case TokenAdapter.peek(state) do
            {:ok, %{kind: :"}"} = close_tok, _} ->
              {:ok, _close, state} = TokenAdapter.next(state)
              close_meta = token_meta(close_tok.metadata)
              entries = Enum.reverse([{key, value} | acc])
              update_ast = {:|, pipe_meta, [base, entries]}
              {:ok, update_ast, close_meta, state, log}

            {:ok, %{kind: :","}, _} ->
              {:ok, _comma, state} = TokenAdapter.next(state)
              state = EOE.skip(state)

              case TokenAdapter.peek(state) do
                {:ok, %{kind: :"}"} = close_tok, _} ->
                  # Trailing comma
                  {:ok, _close, state} = TokenAdapter.next(state)
                  close_meta = token_meta(close_tok.metadata)
                  entries = Enum.reverse([{key, value} | acc])
                  update_ast = {:|, pipe_meta, [base, entries]}
                  {:ok, update_ast, close_meta, state, log}

                {:ok, _tok, _} ->
                  # Parse the remaining entries using map_close (assoc and/or kw tail)
                  with {:ok, more_entries, close_meta, state, log} <-
                         parse_map_close(state, Context.container_expr(), log) do
                    entries = Enum.reverse([{key, value} | acc]) ++ more_entries
                    update_ast = {:|, pipe_meta, [base, entries]}
                    {:ok, update_ast, close_meta, state, log}
                  end

                {:eof, state} ->
                  {:error, :unexpected_eof, state, log}

                {:error, diag, state} ->
                  {:error, diag, state, log}
              end

            _ ->
              {:not_update, state}
          end

        map_base_expr ->
          # It's a map_base_expr without => (like unquote_splicing)
          # Valid only as a single entry at the end, wrap in a list
          # Grammar: assoc_update -> matched_expr pipe_op_eol assoc_expr : {'$2', '$1', ['$3']}.
          # Grammar: assoc_expr -> map_base_expr : '$1'.
          state = EOE.skip(state)

          case TokenAdapter.peek(state) do
            {:ok, %{kind: :"}"} = close_tok, _} ->
              {:ok, _close, state} = TokenAdapter.next(state)
              close_meta = token_meta(close_tok.metadata)
              # Wrap in a list per grammar
              entries = Enum.reverse([map_base_expr | acc])
              update_ast = {:|, pipe_meta, [base, entries]}
              {:ok, update_ast, close_meta, state, log}

            {:ok, %{kind: :","}, _} ->
              # There's more after - map_base_expr can't be followed by more entries
              # unless it's followed by kw_data (which would be handled by kw check)
              {:ok, _comma, state} = TokenAdapter.next(state)
              state = EOE.skip(state)

              case TokenAdapter.peek(state) do
                {:ok, %{kind: :"}"} = close_tok, _} ->
                  # Trailing comma
                  {:ok, _close, state} = TokenAdapter.next(state)
                  close_meta = token_meta(close_tok.metadata)
                  entries = Enum.reverse([map_base_expr | acc])
                  update_ast = {:|, pipe_meta, [base, entries]}
                  {:ok, update_ast, close_meta, state, log}

                {:ok, _tok, _} ->
                  # Parse the remaining entries using map_close (assoc and/or kw tail)
                  with {:ok, more_entries, close_meta, state, log} <-
                         parse_map_close(state, Context.container_expr(), log) do
                    entries = Enum.reverse([map_base_expr | acc]) ++ more_entries
                    update_ast = {:|, pipe_meta, [base, entries]}
                    {:ok, update_ast, close_meta, state, log}
                  end

                {:eof, state} ->
                  {:error, :unexpected_eof, state, log}

                {:error, diag, state} ->
                  {:error, diag, state, log}
              end

            _ ->
              {:not_update, state}
          end
      end
    end
  end

  # Parse map_close: kw_data close_curly | assoc close_curly | assoc_base ',' kw_data close_curly
  defp parse_map_close(state, ctx, log) do
    item_fun = fn state, _ctx, log ->
      case Keywords.try_parse_kw_data(state, ctx, log) do
        {:ok, kw_list, state, log} ->
          # Keyword data must come last in maps. Only accept it if the closing
          # curly follows (kw_data close_curly).
          state = EOE.skip(state)

          case TokenAdapter.peek(state) do
            {:ok, %{kind: :"}"}, _} ->
              {:ok, {:kw_data, kw_list}, state, log}

            {:ok, %{kind: kind}, state} ->
              {:error, {:expected, :"}", got: kind}, state, log}

            {:eof, state} ->
              {:error, :unexpected_eof, state, log}

            {:error, diag, state} ->
              {:error, diag, state, log}
          end

        {:no_kw, state, log} ->
          with {:ok, entry, state, log} <- parse_assoc_expr(state, ctx, log) do
            {:ok, {:entry, entry}, state, log}
          end

        {:error, reason, state, log} ->
          {:error, reason, state, log}
      end
    end

    with {:ok, tagged_items, state, log} <-
           Delimited.parse_comma_separated(state, ctx, log, :"}", item_fun) do
      state = EOE.skip(state)

      case TokenAdapter.next(state) do
        {:ok, %{kind: :"}"} = close_tok, state} ->
          close_meta = token_meta(close_tok.metadata)
          {:ok, finalize_map_close_items(tagged_items), close_meta, state, log}

        {:ok, tok, state} ->
          {:error, {:expected, :"}", got: tok.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  defp finalize_map_close_items(tagged_items) do
    case List.last(tagged_items) do
      {:kw_data, kw_list} ->
        entries =
          tagged_items
          |> Enum.drop(-1)
          |> Enum.map(fn {:entry, entry} -> entry end)

        entries ++ kw_list

      _ ->
        Enum.map(tagged_items, fn {:entry, entry} -> entry end)
    end
  end

  # Parse assoc_expr: key => value - returns {key, value} tuple
  # Grammar: assoc_expr -> matched_expr '=>' expr
  # Note: In maps, => acts as a delimiter, not as a regular binary operator
  # So %{a :: b => c} has key=(a :: b) and value=c, even though :: has lower
  # precedence than => normally.
  #
  # Strategy: Parse using min_bp just above => to get the key, then manually
  # consume => and parse the value. This ensures => is not consumed as part
  # of the key expression.
  defp parse_assoc_expr(state, ctx, log) do
    # Even when the surrounding context is :matched (e.g. inside no-parens call
    # args), map keys/values must still allow full container expressions,
    # including block expressions like `try do ... end` used as a key.
    #
    # This matches Elixir's behavior: you can write `%{try do ... end => 1}`.
    _ctx = ctx

    # Parse the full expression - this will include => as a binary operator
    # Then extract the key/value from the rightmost => in the expression tree.
    case Pratt.parse(state, Context.unmatched_expr(), log) do
      {:ok, expr, state, log} ->
        case extract_assoc(expr) do
          {:assoc, key, value, assoc_meta} ->
            annotated_key = annotate_assoc(key, assoc_meta)
            {:ok, {annotated_key, value}, state, log}

          :not_assoc ->
            {:ok, expr, state, log}
        end

      # keyword_key means "string": - convert to keyword pair
      {:keyword_key, key_atom, state, log} ->
        alias ToxicParser.Grammar.Expressions
        state = EOE.skip(state)

        with {:ok, value_ast, state, log} <-
               Expressions.expr(state, Context.unmatched_expr(), log) do
          {:ok, {key_atom, value_ast}, state, log}
        end

      {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, log} ->
        alias ToxicParser.Grammar.Expressions
        state = EOE.skip(state)

        with {:ok, value_ast, state, log} <-
               Expressions.expr(state, Context.unmatched_expr(), log) do
          key_ast = Expressions.build_interpolated_keyword_key(parts, kind, start_meta, delimiter)
          {:ok, {key_ast, value_ast}, state, log}
        end

      {:error, _, _, _} = error ->
        error
    end
  end

  # Extract the rightmost => from the expression tree
  # In a :: (b => c), we want key=(a :: b), value=c
  # This transforms the AST to extract the correct key/value split
  defp extract_assoc({:"=>", meta, [key, value]}) do
    # Found => at top level - this is an assoc expression
    {:assoc, key, value, meta}
  end

  # Any expression node with arguments (operators and calls).
  # Scan args right-to-left and pull out the rightmost assoc, rebuilding the node.
  # Call / remote call - scan args right-to-left and pull out the rightmost assoc.
  defp extract_assoc({callee, meta, args} = _expr) when is_list(meta) and is_list(args) do
    case extract_assoc_in_args(args) do
      {:assoc, new_args, value, assoc_meta} ->
        {:assoc, {callee, meta, new_args}, value, assoc_meta}

      :not_assoc ->
        :not_assoc
    end
  end

  defp extract_assoc(_), do: :not_assoc

  defp extract_assoc_in_args(args) do
    args
    |> Enum.with_index()
    |> Enum.reverse()
    |> Enum.reduce_while(:not_assoc, fn {arg, idx}, _acc ->
      case extract_assoc(arg) do
        {:assoc, key, value, assoc_meta} ->
          {:halt, {:assoc, List.replace_at(args, idx, key), value, assoc_meta}}

        :not_assoc ->
          {:cont, :not_assoc}
      end
    end)
  end

  # Annotate expression with :assoc metadata (for LHS of => operator)
  # The assoc_meta comes from the => node's metadata, which may include :newlines
  # but the :assoc annotation should only have :line and :column
  defp annotate_assoc({name, meta, args}, assoc_meta)
       when is_list(meta) and is_list(assoc_meta) do
    # Filter to only include :line and :column in the assoc annotation
    clean_assoc_meta =
      Keyword.take(assoc_meta, [:line, :column])

    {name, [assoc: clean_assoc_meta] ++ meta, args}
  end

  defp annotate_assoc(other, _assoc_meta), do: other

  # Build token metadata with explicit newlines count.
  # Some tokens (notably `|` in map updates) carry their own `:newlines` count,
  # so we take the max of the skipped EOE newlines and the token's own newlines.
  defp token_meta_with_newlines(meta, newlines) when is_map(meta) and is_integer(newlines) do
    token_newlines = Map.get(meta, :newlines, 0)
    effective_newlines = max(newlines, token_newlines)

    if effective_newlines > 0 do
      [newlines: effective_newlines] ++ token_meta(meta)
    else
      token_meta(meta)
    end
  end

  defp token_meta_with_newlines(_, _), do: []

  defp token_meta(meta), do: Builder.Helpers.token_meta(meta)

  # Build map AST: %{} or %Struct{}
  # Use {:__no_struct_base__} tuple as sentinel for "no struct base" (i.e., %{} not %Foo{})
  # A tuple can never be a valid struct name, so this allows nil and any atom to be valid bases
  defp build_map_ast({:__no_struct_base__}, pairs, _percent_meta, map_meta) do
    {:%{}, map_meta, pairs}
  end

  defp build_map_ast(base, pairs, percent_meta, map_meta) do
    inner = {:%{}, map_meta, pairs}
    {:%, percent_meta, [base, inner]}
  end

  # Build map update AST: %{expr | ...} or %Struct{expr | ...}
  defp build_map_update_ast({:__no_struct_base__}, update_ast, _percent_meta, map_meta) do
    {:%{}, map_meta, [update_ast]}
  end

  defp build_map_update_ast(base, update_ast, percent_meta, map_meta) do
    inner = {:%{}, map_meta, [update_ast]}
    {:%, percent_meta, [base, inner]}
  end
end
