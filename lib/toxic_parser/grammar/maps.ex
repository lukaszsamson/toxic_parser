defmodule ToxicParser.Grammar.Maps do
  @moduledoc """
  Parsing for maps and structs, including updates inside `%{}`.
  """

  alias ToxicParser.{EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.{Expressions, Keywords}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @spec parse_map(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_map(%State{} = state, ctx, %EventLog{} = log) do
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
            parse_map_args_after_brace(nil, percent_meta, percent_meta, state, ctx, log)

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
        state = skip_eoe(state)
        # Parse map_base_expr (the struct name)
        with {:ok, base, state, log} <- parse_map_base_expr(state, ctx, log) do
          # Skip optional EOE after base
          state = skip_eoe(state)
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
      {:ok, %{kind: kind} = _tok, _} when kind in [:at_op, :unary_op, :ellipsis_op, :dual_op] ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = skip_eoe(state)

        with {:ok, operand, state, log} <- parse_map_base_expr(state, :matched, log) do
          op_meta = token_meta(op_tok.metadata)
          ast = {op_tok.value, op_meta, [operand]}
          {:ok, ast, state, log}
        end

      # ternary_op :"//" used as unary prefix (e.g., %//foo{})
      # Produces: {:/, outer_meta, [{:/, inner_meta, nil}, operand]}
      {:ok, %{kind: :ternary_op, value: :"//"} = _tok, _} ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = skip_eoe(state)

        with {:ok, operand, state, log} <- parse_map_base_expr(state, :matched, log) do
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

      _ ->
        # sub_matched_expr with dot operator support for dotted aliases
        # parse_base_with_dots handles Foo.Bar.Baz but stops before {}
        Pratt.parse_base_with_dots(state, :matched, log)
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
        {state, leading_newlines} = skip_eoe_count_newlines(state, 0)
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
    {state, leading_newlines} = skip_eoe_count_newlines(state, 0)
    parse_map_args_body(base, percent_meta, brace_meta, leading_newlines, state, ctx, log)
  end

  defp parse_map_args_body(base, percent_meta, brace_meta, leading_newlines, state, ctx, log) do
    case TokenAdapter.peek(state) do
      # Empty map: map_args -> open_curly '}'
      {:ok, %{kind: :"}"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        newlines_meta = if leading_newlines > 0, do: [newlines: leading_newlines], else: []
        map_meta = newlines_meta ++ [closing: close_meta] ++ brace_meta
        {:ok, build_map_ast(base, [], percent_meta, map_meta), state, log}

      {:ok, _, _} ->
        # Try to parse map update first, then fall back to regular entries
        case try_parse_map_update(state, ctx, log) do
          {:ok, update_ast, close_meta, state, log} ->
            newlines_meta = if leading_newlines > 0, do: [newlines: leading_newlines], else: []
            map_meta = newlines_meta ++ [closing: close_meta] ++ brace_meta
            {:ok, build_map_update_ast(base, update_ast, percent_meta, map_meta), state, log}

          :not_update ->
            # Parse map_close: kw_data | assoc | assoc_base ',' kw_data
            with {:ok, entries, close_meta, state, log} <- parse_map_close(state, ctx, log) do
              newlines_meta = if leading_newlines > 0, do: [newlines: leading_newlines], else: []
              map_meta = newlines_meta ++ [closing: close_meta] ++ brace_meta
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
  # The grammar rule is: assoc_update -> matched_expr pipe_op_eol assoc_expr
  #
  # Strategy: Parse the full expression. If the result is {:|, meta, [base, first_entry]},
  # it's a map update. We then need to collect any remaining entries after a comma.
  defp try_parse_map_update(state, ctx, log) do
    # Checkpoint before attempting to parse as map update
    {checkpoint_id, state} = TokenAdapter.checkpoint(state)

    # Parse the full expression - this may consume |
    case Expressions.expr(state, ctx, log) do
      {:ok, {:|, pipe_meta, [base_expr, first_rhs]}, state, log} ->
        # This is a map update! Handle different RHS forms:
        # 1. {:"=>", meta, [k, v]} - assoc expression, convert to {k, v}
        # 2. atom - keyword style like "a: 1" where "a:" parsed as :a, need to get value
        # 3. keyword list - already parsed keyword data
        case first_rhs do
          # Case: keyword style "map | a: 1" - first_rhs is :a, value follows
          key when is_atom(key) ->
            # Parse the value that follows the keyword key
            with {:ok, value, state, log} <- Expressions.expr(state, :matched, log) do
              first_entry = {key, value}
              finish_map_update(base_expr, first_entry, pipe_meta, state, ctx, log)
            end

          # Case: already a keyword list (parsed by kw handling in Pratt)
          [{key, _value} | _rest] = kw_list when is_atom(key) ->
            # Already have keyword list as entries
            state = skip_eoe(state)

            case TokenAdapter.peek(state) do
              {:ok, %{kind: :"}"} = close_tok, _} ->
                {:ok, _close, state} = TokenAdapter.next(state)
                close_meta = token_meta(close_tok.metadata)
                update_ast = {:|, pipe_meta, [base_expr, kw_list]}
                {:ok, update_ast, close_meta, state, log}

              {:ok, tok, state} ->
                {:error, {:expected, :"}", got: tok.kind}, state, log}

              {:eof, state} ->
                {:error, :unexpected_eof, state, log}

              {:error, diag, state} ->
                {:error, diag, state, log}
            end

          # Case: assoc expression {:"=>", meta, [k, v]} or other expression
          other ->
            first_entry = normalize_assoc_entry(other)
            finish_map_update(base_expr, first_entry, pipe_meta, state, ctx, log)
        end

      {:ok, _other_expr, state, _log} ->
        # Not a pipe expression at top level - not a map update
        _state = TokenAdapter.rewind(state, checkpoint_id)
        :not_update

      {:error, _, state, _} ->
        # Parse failed, restore checkpoint
        _state = TokenAdapter.rewind(state, checkpoint_id)
        :not_update
    end
  end

  # Convert {:"=>", meta, [key, value]} to {key, value}, leaving other forms unchanged
  defp normalize_assoc_entry({:"=>", _meta, [key, value]}), do: {key, value}
  defp normalize_assoc_entry(other), do: other

  # Finish parsing map update after we have the first entry
  defp finish_map_update(base_expr, first_entry, pipe_meta, state, ctx, log) do
    state = skip_eoe(state)

    case TokenAdapter.peek(state) do
      {:ok, %{kind: :","}, _} ->
        {:ok, _comma, state} = TokenAdapter.next(state)
        state = skip_eoe(state)
        # Check for trailing comma (just close brace)
        case TokenAdapter.peek(state) do
          {:ok, %{kind: :"}"} = close_tok, _} ->
            # Trailing comma - just the first entry
            {:ok, _close, state} = TokenAdapter.next(state)
            close_meta = token_meta(close_tok.metadata)
            update_ast = {:|, pipe_meta, [base_expr, [first_entry]]}
            {:ok, update_ast, close_meta, state, log}

          _ ->
            # Parse remaining entries (could be kw_data or more assocs)
            case parse_map_close(state, ctx, log) do
              {:ok, more_entries, close_meta, state, log} ->
                all_entries = [first_entry | more_entries]
                update_ast = {:|, pipe_meta, [base_expr, all_entries]}
                {:ok, update_ast, close_meta, state, log}

              {:error, _, _, _} = err ->
                err
            end
        end

      {:ok, %{kind: :"}"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        update_ast = {:|, pipe_meta, [base_expr, [first_entry]]}
        {:ok, update_ast, close_meta, state, log}

      {:ok, tok, state} ->
        {:error, {:expected_comma_or, :"}", got: tok.kind}, state, log}

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse map_close: kw_data close_curly | assoc close_curly | assoc_base ',' kw_data close_curly
  defp parse_map_close(state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          # kw_data close_curly
          with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
            state = skip_eoe(state)

            case TokenAdapter.next(state) do
              {:ok, %{kind: :"}"} = close_tok, state} ->
                close_meta = token_meta(close_tok.metadata)
                {:ok, kw_list, close_meta, state, log}

              {:ok, tok, state} ->
                {:error, {:expected, :"}", got: tok.kind}, state, log}

              {:eof, state} ->
                {:error, :unexpected_eof, state, log}

              {:error, diag, state} ->
                {:error, diag, state, log}
            end
          end
        else
          # assoc or assoc_base ',' kw_data
          parse_assoc_entries([], state, ctx, log)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse assoc entries (key => value pairs)
  defp parse_assoc_entries(acc, state, ctx, log) do
    with {:ok, entry, state, log} <- parse_assoc_expr(state, ctx, log) do
      state = skip_eoe(state)

      case TokenAdapter.peek(state) do
        {:ok, %{kind: :"}"} = close_tok, _} ->
          {:ok, _close, state} = TokenAdapter.next(state)
          close_meta = token_meta(close_tok.metadata)
          {:ok, Enum.reverse([entry | acc]), close_meta, state, log}

        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state)
          state = skip_eoe(state)
          # Check for trailing comma, kw_data, or more assoc_expr
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :"}"} = close_tok, _} ->
              {:ok, _close, state} = TokenAdapter.next(state)
              close_meta = token_meta(close_tok.metadata)
              {:ok, Enum.reverse([entry | acc]), close_meta, state, log}

            {:ok, tok, _} ->
              if Keywords.starts_kw?(tok) do
                # assoc_base ',' kw_data
                with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
                  state = skip_eoe(state)

                  case TokenAdapter.next(state) do
                    {:ok, %{kind: :"}"} = close_tok, state} ->
                      close_meta = token_meta(close_tok.metadata)
                      {:ok, Enum.reverse([entry | acc]) ++ kw_list, close_meta, state, log}

                    {:ok, tok, state} ->
                      {:error, {:expected, :"}", got: tok.kind}, state, log}

                    {:eof, state} ->
                      {:error, :unexpected_eof, state, log}

                    {:error, diag, state} ->
                      {:error, diag, state, log}
                  end
                end
              else
                parse_assoc_entries([entry | acc], state, ctx, log)
              end

            {:eof, state} ->
              {:error, :unexpected_eof, state, log}

            {:error, diag, state} ->
              {:error, diag, state, log}
          end

        {:ok, tok, state} ->
          {:error, {:expected_comma_or, :"}", got: tok.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  # Parse assoc_expr: key => value - returns {key, value} tuple
  # Expressions.expr will parse "key => value" as {:"=>", meta, [key, value]}
  # We need to convert this to {key, value} tuple format
  defp parse_assoc_expr(state, ctx, log) do
    with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
      case expr do
        {:"=>", _meta, [key, value]} ->
          # Convert assoc operator AST to tuple
          {:ok, {key, value}, state, log}

        other ->
          # Not an assoc expression - just return the expression
          {:ok, other, state, log}
      end
    end
  end

  defp skip_eoe(state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe(state)

      _ ->
        state
    end
  end

  defp skip_eoe_count_newlines(state, count) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, value: %{newlines: n}}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe_count_newlines(state, count + n)

      _ ->
        {state, count}
    end
  end

  defp token_meta(%{range: %{start: %{line: line, column: column}}}),
    do: [line: line, column: column]

  defp token_meta(_), do: []

  # Build map AST: %{} or %Struct{}
  defp build_map_ast(nil, pairs, _percent_meta, map_meta) do
    {:%{}, map_meta, pairs}
  end

  defp build_map_ast(base, pairs, percent_meta, map_meta) do
    inner = {:%{}, map_meta, pairs}
    {:%, percent_meta, [base, inner]}
  end

  # Build map update AST: %{expr | ...} or %Struct{expr | ...}
  defp build_map_update_ast(nil, update_ast, _percent_meta, map_meta) do
    {:%{}, map_meta, [update_ast]}
  end

  defp build_map_update_ast(base, update_ast, percent_meta, map_meta) do
    inner = {:%{}, map_meta, [update_ast]}
    {:%, percent_meta, [base, inner]}
  end
end
