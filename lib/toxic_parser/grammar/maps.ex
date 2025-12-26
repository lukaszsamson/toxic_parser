defmodule ToxicParser.Grammar.Maps do
  @moduledoc """
  Parsing for maps and structs, including updates inside `%{}`.
  """

  alias ToxicParser.{Context, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Builder.{Helpers, Meta}
  alias ToxicParser.Grammar.{EOE, Expressions, Keywords}

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
      {:ok, {:%{}, _meta} = tok, state} ->
        # map -> map_op map_args where map_op is '%{}'
        # The tokenizer emits %{} as three tokens: :%{}, :"{", :"}"
        # We need to consume the "{" token (the "}" will be consumed by parse_map_args_body)
        percent_meta = Helpers.token_meta(tok)
        # Consume the opening brace token
        case TokenAdapter.next(state) do
          {:ok, {:"{", _}, state} ->
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
            {:error, {:expected, :"{", got: TokenAdapter.kind(tok)}, state, log}

          {:eof, state} ->
            {:error, :unexpected_eof, state, log}

          {:error, diag, state} ->
            {:error, diag, state, log}
        end

      {:ok, {:%, _meta} = tok, state} ->
        # map -> '%' map_base_expr map_args (struct)
        percent_meta = Helpers.token_meta(tok)
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
      {:ok, {:at_op, _, _} = _tok, _} ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        with {:ok, operand, state, log} <- parse_unary_operand(state, Context.matched_expr(), log) do
          op_meta = Helpers.token_meta(op_tok)
          ast = {TokenAdapter.value(op_tok), op_meta, [operand]}
          # Continue with dots/calls to handle trailing .foo or .Bar
          Pratt.led_dots_and_calls(ast, state, log, Context.matched_expr())
        end

      # Other unary ops (^, !, not, +, -): precedence < dot 310
      # So ^t.d should be ^(t.d) - dots are part of the operand
      {:ok, tok, _} ->
        tok_kind = TokenAdapter.kind(tok)

        cond do
          tok_kind in [:unary_op, :dual_op] ->
            {:ok, op_tok, state} = TokenAdapter.next(state)
            state = EOE.skip(state)

            # Parse operand with dots applied (since dot binds tighter)
            with {:ok, operand, state, log} <-
                   parse_unary_operand_with_dots(state, Context.matched_expr(), log) do
              op_meta = Helpers.token_meta(op_tok)
              ast = {TokenAdapter.value(op_tok), op_meta, [operand]}
              {:ok, ast, state, log}
            end

          # ellipsis_op can also be nullary (elixir_parser.yrl: sub_matched_expr -> ellipsis_op)
          # as in `%...{}`. If the next token is `{`, treat it as nullary.
          tok_kind == :ellipsis_op ->
            {:ok, op_tok, state} = TokenAdapter.next(state)
            op_meta = Helpers.token_meta(op_tok)

            case TokenAdapter.peek(state) do
              {:ok, {:"{", _}, _} ->
                {:ok, {TokenAdapter.value(op_tok), op_meta, []}, state, log}

              _ ->
                state = EOE.skip(state)

                with {:ok, operand, state, log} <-
                       parse_unary_operand_with_dots(state, Context.matched_expr(), log) do
                  {:ok, {TokenAdapter.value(op_tok), op_meta, [operand]}, state, log}
                end
            end

          # ternary_op :"//" used as unary prefix (e.g., %//foo{})
          # Also has precedence < dot 310, so dots are part of the operand
          tok_kind == :ternary_op and TokenAdapter.value(tok) == :"//" ->
            {:ok, op_tok, state} = TokenAdapter.next(state)
            state = EOE.skip(state)

            with {:ok, operand, state, log} <-
                   parse_unary_operand_with_dots(state, Context.matched_expr(), log) do
              op_meta = Helpers.token_meta(op_tok)
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

          # fn ... end is a valid sub_matched_expr for struct base (e.g. %fn a -> 1 end{})
          tok_kind == :fn ->
            alias ToxicParser.Grammar.Blocks

            case Blocks.parse(state, Context.matched_expr(), log) do
              {:ok, ast, state, log} ->
                {:ok, ast, state, log}

              {:no_block, state} ->
                {:error, :expected_fn, state, log}
            end

          # Parenthesized expression or list/tuple/bitstring as struct base
          # Grammar allows these as sub_matched_expr -> access_expr -> list | tuple | bitstring | empty_paren
          # Example: %!(){} where () is the operand of !
          # Example: %@[]{} where [] is the operand of @
          tok_kind in [:"(", :"[", :"{", :"<<"] ->
            alias ToxicParser.Grammar.Containers
            Containers.parse(state, Context.matched_expr(), log)

          tok_kind in [:%, :%{}] ->
            # Nested maps/structs are valid map_base_expr (sub_matched_expr).
            # Needed for things like `%%{}{}.`
            parse_map_base(state, Context.matched_expr(), log)

          true ->
            # sub_matched_expr with dot operator and paren call support
            # parse_base_with_dots_and_calls handles:
            #   - Foo.Bar.Baz (dotted aliases)
            #   - unquote(struct) (paren calls)
            #   - module.Foo.Bar (dotted calls)
            # But stops at { which is the struct body
            Pratt.parse_base_with_dots_and_calls(state, Context.matched_expr(), log)
        end

      _ ->
        # Default case - parse as base expression
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
      {:ok, {:at_op, _, _} = _tok, _} ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        with {:ok, operand, state, log} <- parse_unary_operand(state, ctx, log) do
          op_meta = Helpers.token_meta(op_tok)
          ast = {TokenAdapter.value(op_tok), op_meta, [operand]}
          {:ok, ast, state, log}
        end

      # For other unary operators (precedence < dot 310), use parse_unary_operand_with_dots
      # so that dots are included in their operand.
      # E.g., %@+a.i{} should be @(+(a.i)){} not (@(+a)).i{}
      {:ok, tok, _} ->
        tok_kind = TokenAdapter.kind(tok)

        cond do
          tok_kind in [:unary_op, :ellipsis_op, :dual_op] ->
            {:ok, op_tok, state} = TokenAdapter.next(state)
            state = EOE.skip(state)

            with {:ok, operand, state, log} <- parse_unary_operand_with_dots(state, ctx, log) do
              op_meta = Helpers.token_meta(op_tok)
              ast = {TokenAdapter.value(op_tok), op_meta, [operand]}
              {:ok, ast, state, log}
            end

          # ternary_op :"//" used as unary prefix (precedence < dot 310)
          tok_kind == :ternary_op and TokenAdapter.value(tok) == :"//" ->
            raise "dead code"
            {:ok, op_tok, state} = TokenAdapter.next(state)
            state = EOE.skip(state)

            with {:ok, operand, state, log} <- parse_unary_operand_with_dots(state, ctx, log) do
              op_meta = Helpers.token_meta(op_tok)

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
          # In struct-base context we must not let the container consume trailing dots/calls,
          # because `{` belongs to the outer `%...{}` body.
          tok_kind in [:"(", :"[", :"{", :"<<", :%, :%{}] ->
            alias ToxicParser.Grammar.Containers
            Containers.parse(state, ctx, log, 10000)

          # Paren call as operand (e.g., %@i(){} where i() is the operand)
          # Parse identifier then consume () as a paren call
          tok_kind == :paren_identifier ->
            {:ok, _tok, state} = TokenAdapter.next(state)
            Pratt.parse_paren_call_base(tok, state, ctx, log)

          true ->
            # Base expression (literal, identifier, etc.) - parse without dots
            Pratt.parse_base(state, ctx, log)
        end

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
      {:ok, {:at_op, _, _} = _tok, _} ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        with {:ok, operand, state, log} <- parse_unary_operand(state, ctx, log) do
          op_meta = Helpers.token_meta(op_tok)
          ast = {TokenAdapter.value(op_tok), op_meta, [operand]}
          # Apply dots after @ since @ binds tighter
          Pratt.led_dots_and_calls(ast, state, log, ctx)
        end

      # Other unary ops - recurse with dots
      {:ok, tok, _} ->
        tok_kind = TokenAdapter.kind(tok)

        cond do
          tok_kind in [:unary_op, :ellipsis_op, :dual_op] ->
            {:ok, op_tok, state} = TokenAdapter.next(state)
            state = EOE.skip(state)

            with {:ok, operand, state, log} <- parse_unary_operand_with_dots(state, ctx, log) do
              op_meta = Helpers.token_meta(op_tok)
              ast = {TokenAdapter.value(op_tok), op_meta, [operand]}
              {:ok, ast, state, log}
            end

          # ternary_op :"//" used as unary prefix
          tok_kind == :ternary_op and TokenAdapter.value(tok) == :"//" ->
            raise "dead code"
            {:ok, op_tok, state} = TokenAdapter.next(state)
            state = EOE.skip(state)

            with {:ok, operand, state, log} <- parse_unary_operand_with_dots(state, ctx, log) do
              op_meta = Helpers.token_meta(op_tok)

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
          tok_kind in [:"(", :"[", :"{", :"<<", :%, :%{}] ->
            alias ToxicParser.Grammar.Containers

            with {:ok, base, state, log} <- Containers.parse(state, ctx, log, 10000) do
              Pratt.led_dots_and_calls(base, state, log, ctx)
            end

          true ->
            # Base expression - parse and apply dots
            with {:ok, base, state, log} <- Pratt.parse_base(state, ctx, log) do
              Pratt.led_dots_and_calls(base, state, log, ctx)
            end
        end

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
      {:ok, {:"{", _} = open_tok, _} ->
        {:ok, _open, state} = TokenAdapter.next(state)
        brace_meta = Helpers.token_meta(open_tok)
        # Skip leading EOE and count newlines
        {state, leading_newlines} = EOE.skip_count_newlines(state, 0)
        parse_map_args_body(base, percent_meta, brace_meta, leading_newlines, state, ctx, log)

      {:ok, tok, state} ->
        {:error, {:expected, :"{", got: TokenAdapter.kind(tok)}, state, log}

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
      {:ok, {:"}", _} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = Helpers.token_meta(close_tok)
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
      {:ok, {:kw_identifier, _, _}, _} ->
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
        {:ok, tok, _} ->
          tok_kind = TokenAdapter.kind(tok)
          tok_kind in [:bin_string_start, :list_string_start]

        _ ->
          false
      end

    # First, try parsing the full expression without restricting |
    # This allows op_identifier calls (like c!) to consume | as part of their argument
    # e.g., %{c!s|n => 1} should parse as c!(s|n) => 1, not c!s | n => 1
    state0 = state

    case Pratt.parse(state, Context.container_expr(), log) do
      {:ok, base_expr, state, log} ->
        case {base_expr, TokenAdapter.peek(state)} do
          {{:|, pipe_meta, _}, {:ok, {:assoc_op, _, _}, _}} ->
            if Keyword.has_key?(pipe_meta, :parens) do
              handle_map_update_after_base_expr(base_expr, state, log)
            else
              parse_map_update_with_min_bp(state0, log)
            end

          _ ->
            handle_map_update_after_base_expr(base_expr, state, log)
        end

      {:keyword_key, _, _, _state, _} ->
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

  defp handle_map_update_after_base_expr(base_expr, state, log) do
    case check_embedded_map_update(base_expr) do
      {:update, base, pipe_meta, rhs_entries} ->
        state = EOE.skip(state)

        case TokenAdapter.peek(state) do
          {:ok, {:"}", _} = close_tok, _} ->
            {:ok, _close, state} = TokenAdapter.next(state)
            close_meta = Helpers.token_meta(close_tok)
            update_ast = {:|, pipe_meta, [base, rhs_entries]}
            {:ok, update_ast, close_meta, state, log}

          {:ok, {:",", _}, _} ->
            {:ok, _comma, state} = TokenAdapter.next(state)
            state = EOE.skip(state)
            parse_map_update_trailing_entries(base, pipe_meta, rhs_entries, state, log)

          _ ->
            raise "dead code"
            {:not_update, state}
        end

      :not_update ->
        {state, newlines} = EOE.skip_count_newlines(state, 0)

        case TokenAdapter.peek(state) do
          {:ok, {:pipe_op, _, _} = pipe_tok, _} ->
            raise "dead code"
            {:ok, _pipe, state} = TokenAdapter.next(state)
            {state, newlines_after_pipe} = EOE.skip_newlines_only(state, 0)
            pipe_meta_kw = token_meta_with_newlines(pipe_tok, max(newlines, newlines_after_pipe))
            parse_map_update_rhs(base_expr, pipe_meta_kw, state, log)

          _ ->
            {:not_update, state}
        end
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
        {:valid, entries} ->
          {:update, base, pipe_meta, entries}

        :invalid ->
          raise "dead code"
          :not_update
      end
    end
  end

  defp check_embedded_map_update(_), do: :not_update

  # Check if RHS of | is valid for map update
  # Valid: single assoc {:"=>", _, [k, v]} or keyword list
  # Annotates keys with :assoc metadata indicating the position of =>
  defp classify_pipe_rhs_for_map_update({:"=>", assoc_meta, [key, value]} = _assoc) do
    raise "dead code"
    annotated_key = annotate_assoc(key, assoc_meta)
    {:valid, [{annotated_key, value}]}
  end

  # Handle nested pipe in map update RHS: a | b | c => d
  # Pratt produces: {:|, _, [a, {:|, _, [b, {:"=>", _, [c, d]}]}]}
  # The RHS of first | is: {:|, _, [b, {:"=>", _, [c, d]}]}
  # This should become: [{(b | c), d}] - key is b | c, value is d
  # Use extract_assoc to properly handle any level of nesting
  defp classify_pipe_rhs_for_map_update({:|, rhs_meta, _} = expr) do
    if Keyword.has_key?(rhs_meta, :parens) do
      {:valid, [expr]}
    else
      raise "dead code"

      case extract_assoc(expr) do
        {:assoc, key, value, assoc_meta} ->
          annotated_key = annotate_assoc(key, assoc_meta)
          {:valid, [{annotated_key, value}]}

        :not_assoc ->
          :invalid
      end
    end
  end

  defp classify_pipe_rhs_for_map_update(list) when is_list(list) do
    cond do
      list == [] ->
        # Empty list literal (e.g. ''), treated as a map_base_expr, wrapped.
        {:valid, [list]}

      Enum.all?(list, &is_keyword_or_assoc_entry?/1) ->
        # raise "dead code"
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
        raise "dead code"
        {:valid, [{annotate_assoc(key, assoc_meta), value}]}

      :not_assoc ->
        {:valid, [rhs]}
    end
  end

  # Extract the rightmost => from an expression tree.
  # Used for validating map-update RHS like `base | k1 | k2 => v`.
  defp extract_assoc({:"=>", meta, [key, value]}), do: {:assoc, key, value, meta}

  defp extract_assoc({callee, meta, args}) when is_list(meta) and is_list(args) do
    case extract_assoc_in_args(args) do
      {:assoc, new_args, value, assoc_meta} ->
        raise "dead code"
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
          raise "dead code"
          {:halt, {:assoc, List.replace_at(args, idx, key), value, assoc_meta}}

        :not_assoc ->
          {:cont, :not_assoc}
      end
    end)
  end

  # Annotate a single assoc entry with :assoc metadata
  defp annotate_assoc_entry({:"=>", assoc_meta, [key, value]}) do
    raise "dead code"
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
      {:ok, {:"}", _} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = Helpers.token_meta(close_tok)
        update_ast = {:|, pipe_meta, [base, initial_entries]}
        {:ok, update_ast, close_meta, state, log}

      {:ok, _tok, _} ->
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
          {:ok, {:pipe_op, _, _} = pipe_tok, _} ->
            {:ok, _pipe, state} = TokenAdapter.next(state)
            {state, newlines_after_pipe} = EOE.skip_newlines_only(state, 0)
            pipe_meta_kw = token_meta_with_newlines(pipe_tok, max(newlines, newlines_after_pipe))
            parse_map_update_rhs(base_expr, pipe_meta_kw, state, log)

          _ ->
            raise "dead code"
            {:not_update, state}
        end

      {:keyword_key, _, _, state, _} ->
        raise "dead code"
        {:not_update, state}

      {:keyword_key_interpolated, _, _, _, _, state, _} ->
        raise "dead code"
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
          {:ok, {:"}", _} = close_tok, _} ->
            {:ok, _close, state} = TokenAdapter.next(state)
            close_meta = Helpers.token_meta(close_tok)
            update_ast = {:|, pipe_meta, [base, kw_list]}
            {:ok, update_ast, close_meta, state, log}

          _ ->
            raise "dead code"
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
            {:ok, {:"}", _} = close_tok, _} ->
              {:ok, _close, state} = TokenAdapter.next(state)
              close_meta = Helpers.token_meta(close_tok)
              entries = Enum.reverse([{key, value} | acc])
              update_ast = {:|, pipe_meta, [base, entries]}
              {:ok, update_ast, close_meta, state, log}

            {:ok, {:",", _}, _} ->
              {:ok, _comma, state} = TokenAdapter.next(state)
              state = EOE.skip(state)

              case TokenAdapter.peek(state) do
                {:ok, {:"}", _} = close_tok, _} ->
                  # Trailing comma
                  {:ok, _close, state} = TokenAdapter.next(state)
                  close_meta = Helpers.token_meta(close_tok)
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

            {:eof, state} ->
              {:error, :unexpected_eof, state, log}

            {:error, diag, state} ->
              {:error, diag, state, log}

            {:ok, _tok, state} ->
              {:error, :expected_closing_brace, state, log}
          end

        _ ->
          {:error, :expected_assoc, state, log}
      end
    end
  end

  # Helper to create [line:, column:, newlines: n] metadata for pipe_op
  # Extracts newlines from both the token's embedded count and the passed-in value
  defp token_meta_with_newlines(tok, newlines) do
    token_newlines = TokenAdapter.newlines(tok)
    effective_newlines = max(newlines, token_newlines)

    base_meta = Helpers.token_meta(tok)

    if effective_newlines > 0 do
      [{:newlines, effective_newlines} | base_meta]
    else
      base_meta
    end
  end

  # Build map AST: %{...} or %Base{...}
  defp build_map_ast({:__no_struct_base__}, entries, _percent_meta, map_meta) do
    # Plain map: %{...}
    {:%{}, map_meta, entries}
  end

  defp build_map_ast(base, entries, percent_meta, map_meta) do
    # Struct: %Base{...}
    {:%, percent_meta, [base, {:%{}, map_meta, entries}]}
  end

  # Build map update AST: %{map | ...} or %Base{map | ...}
  defp build_map_update_ast({:__no_struct_base__}, update_ast, _percent_meta, map_meta) do
    # Plain map update: %{...}
    {:%{}, map_meta, [update_ast]}
  end

  defp build_map_update_ast(base, update_ast, percent_meta, map_meta) do
    # Struct update: %Base{...}
    {:%, percent_meta, [base, {:%{}, map_meta, [update_ast]}]}
  end

  # Parse map_close: kw_data | assoc | assoc_base ',' kw_data
  # Returns {:ok, entries, close_meta, state, log}
  defp parse_map_close(state, ctx, log) do
    container_ctx = Context.container_expr()

    # Try keyword data first
    case Keywords.try_parse_kw_data(state, container_ctx, log) do
      {:ok, kw_list, state, log} ->
        # map_close -> kw_data close_curly
        state = EOE.skip(state)

        case TokenAdapter.next(state) do
          {:ok, {:"}", _} = close_tok, state} ->
            close_meta = Helpers.token_meta(close_tok)
            {:ok, kw_list, close_meta, state, log}

          {:ok, tok, state} ->
            {:error, {:expected, :"}", got: TokenAdapter.kind(tok)}, state, log}

          {:eof, state} ->
            {:error, :unexpected_eof, state, log}

          {:error, diag, state} ->
            {:error, diag, state, log}
        end

      {:no_kw, state, log} ->
        # Try assoc entries
        parse_assoc_list([], state, ctx, log)

      {:error, reason, state, log} ->
        {:error, reason, state, log}
    end
  end

  # Parse assoc list (assoc_base): one or more assoc_expr separated by commas
  # assoc_base -> assoc_expr | assoc_expr ',' assoc_base
  defp parse_assoc_list(acc, state, ctx, log) do
    with {:ok, entry, state, log} <- parse_assoc_expr(state, Context.container_expr(), log) do
      state = EOE.skip(state)

      case TokenAdapter.peek(state) do
        {:ok, {:"}", _} = close_tok, _} ->
          # End of assoc list
          {:ok, _close, state} = TokenAdapter.next(state)
          close_meta = Helpers.token_meta(close_tok)
          {:ok, Enum.reverse([entry | acc]), close_meta, state, log}

        {:ok, {:",", _}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state)
          state = EOE.skip(state)

          case TokenAdapter.peek(state) do
            {:ok, {:"}", _} = close_tok, _} ->
              # Trailing comma
              {:ok, _close, state} = TokenAdapter.next(state)
              close_meta = Helpers.token_meta(close_tok)
              {:ok, Enum.reverse([entry | acc]), close_meta, state, log}

            {:ok, _, _} ->
              # Check if next is kw_data (assoc_base ',' kw_data)
              case Keywords.try_parse_kw_data(state, Context.container_expr(), log) do
                {:ok, kw_list, state, log} ->
                  state = EOE.skip(state)

                  case TokenAdapter.next(state) do
                    {:ok, {:"}", _} = close_tok, state} ->
                      close_meta = Helpers.token_meta(close_tok)
                      # kw_list is already a list of keyword tuples, don't wrap in another list
                      entries = Enum.reverse([entry | acc]) ++ kw_list
                      {:ok, entries, close_meta, state, log}

                    {:ok, tok, state} ->
                      {:error, {:expected, :"}", got: TokenAdapter.kind(tok)}, state, log}

                    {:eof, state} ->
                      {:error, :unexpected_eof, state, log}

                    {:error, diag, state} ->
                      {:error, diag, state, log}
                  end

                {:no_kw, state, log} ->
                  # Continue with more assoc entries
                  parse_assoc_list([entry | acc], state, ctx, log)

                {:error, reason, state, log} ->
                  {:error, reason, state, log}
              end

            {:eof, state} ->
              {:error, :unexpected_eof, state, log}

            {:error, diag, state} ->
              {:error, diag, state, log}
          end

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}

        {:ok, _tok, state} ->
          {:error, :expected_closing_brace, state, log}
      end
    end
  end

  # Parse assoc_expr: container_expr | container_expr '=>' container_expr
  # Returns {:ok, entry, state, log} where entry is {key, value} or just expr
  defp parse_assoc_expr(state, _ctx, log) do
    with {:ok, left, state, log} <- Expressions.expr(state, Context.container_expr(), log) do
      state = EOE.skip(state)

      case TokenAdapter.peek(state) do
        {:ok, {:assoc_op, _, _}, _} ->
          {:ok, assoc_tok, state} = TokenAdapter.next(state)
          assoc_meta = Helpers.token_meta(assoc_tok)
          state = EOE.skip(state)

          with {:ok, right, state, log} <- Expressions.expr(state, Context.container_expr(), log) do
            # Annotate key with :assoc metadata
            annotated_key = annotate_assoc(left, assoc_meta)
            {:ok, {annotated_key, right}, state, log}
          end

        _ ->
          # No assoc operator - return as-is
          {:ok, left, state, log}
      end
    end
  end

  # Annotate a key expression with :assoc metadata
  # For AST nodes {name, meta, args_or_context}, merge assoc position into meta
  # Note: Variables have {name, meta, nil} where nil is the context, not args list
  defp annotate_assoc({name, meta, args_or_context}, assoc_meta) when is_list(meta) do
    # Merge :assoc metadata into existing metadata
    merged_meta = [{:assoc, assoc_meta} | meta]
    {name, merged_meta, args_or_context}
  end

  # For atoms, DON'T wrap in AST form - just return the atom
  # Elixir converts :a => 1 to keyword pairs a: 1 without metadata annotation
  defp annotate_assoc(key, _assoc_meta) when is_atom(key) do
    key
  end

  defp annotate_assoc(key, _assoc_meta) do
    # For other expressions (literals, etc.), return as-is
    # The Elixir parser doesn't annotate these
    key
  end
end
