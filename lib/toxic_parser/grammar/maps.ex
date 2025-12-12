defmodule ToxicParser.Grammar.Maps do
  @moduledoc """
  Parsing for maps and structs, including updates inside `%{}`.
  """

  alias ToxicParser.{EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.Keywords

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @spec parse_map(State.t(), Pratt.context(), EventLog.t(), non_neg_integer()) :: result()
  def parse_map(%State{} = state, ctx, %EventLog{} = log, min_bp \\ 0) do
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

          {:not_update, state} ->
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
  # Strategy: Parse potential base expression with min_bp > pipe_op (70),
  # then check if | follows. This prevents consuming | as part of assoc value.
  # Important: If the base_expr contains =>, it's an assoc expr not an update base.
  defp try_parse_map_update(state, ctx, log) do
    # Checkpoint before attempting to parse as map update
    {checkpoint_id, state} = TokenAdapter.checkpoint(state)

    # Parse potential base expression, stopping at | (bp=70)
    # Use min_bp=71 to stop before |
    case Pratt.parse_with_min_bp(state, :matched, log, 71) do
      {:ok, base_expr, state, log} ->
        # Check if base_expr is an assoc expression - if so, this is NOT a map update
        # because %{a => b | c} is a single entry with value (b | c), not an update
        if is_assoc_expr?(base_expr) do
          state = TokenAdapter.rewind(state, checkpoint_id)
          {:not_update, state}
        else
          state = skip_eoe(state)

          case TokenAdapter.peek(state) do
            {:ok, %{kind: :pipe_op} = pipe_tok, _} ->
              # This is a map update!
              {:ok, _pipe, state} = TokenAdapter.next(state)
              # Skip EOE after | and count newlines for metadata
              {state, newlines_after_pipe} = skip_eoe_count_newlines(state, 0)
              # Build pipe metadata with newlines if present
              pipe_meta = token_meta_with_newlines(pipe_tok.metadata, newlines_after_pipe)

              # Parse entries after |
              case TokenAdapter.peek(state) do
                {:ok, tok, _} ->
                  if Keywords.starts_kw?(tok) do
                    # Keyword entries: %{base | a: 1, b: 2}
                    with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
                      state = skip_eoe(state)

                      case TokenAdapter.next(state) do
                        {:ok, %{kind: :"}"} = close_tok, state} ->
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
                    end
                  else
                    # Assoc entries: %{base | a => b, c => d}
                    with {:ok, entries, close_meta, state, log} <-
                           parse_map_close(state, ctx, log) do
                      update_ast = {:|, pipe_meta, [base_expr, entries]}
                      {:ok, update_ast, close_meta, state, log}
                    end
                  end

                {:eof, state} ->
                  {:error, :unexpected_eof, state, log}

                {:error, diag, state} ->
                  {:error, diag, state, log}
              end

            _ ->
              # No | operator - not a map update
              state = TokenAdapter.rewind(state, checkpoint_id)
              {:not_update, state}
          end
        end

      {:error, _, state, _} ->
        # Parse failed, restore checkpoint
        state = TokenAdapter.rewind(state, checkpoint_id)
        {:not_update, state}
    end
  end

  # Check if an expression is an assoc expression (has => at top level)
  defp is_assoc_expr?({:"=>", _, _}), do: true
  defp is_assoc_expr?(_), do: false

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
  # Grammar: assoc_expr -> matched_expr '=>' expr
  # Note: In maps, => acts as a delimiter, not as a regular binary operator
  # So %{a :: b => c} has key=(a :: b) and value=c, even though :: has lower
  # precedence than => normally.
  #
  # Strategy: Parse using min_bp just above => to get the key, then manually
  # consume => and parse the value. This ensures => is not consumed as part
  # of the key expression.
  defp parse_assoc_expr(state, ctx, log) do
    # Parse the full expression - this will include => as a binary operator
    # Then extract the key/value from the rightmost => in the expression tree
    with {:ok, expr, state, log} <- Pratt.parse(state, ctx, log) do
      # Check if the result has => at top level or nested
      case extract_assoc(expr) do
        {:assoc, key, value, assoc_meta} ->
          # Annotate the key with :assoc metadata (the position of =>)
          # This is what Elixir's parser does with token_metadata: true
          annotated_key = annotate_assoc(key, assoc_meta)
          {:ok, {annotated_key, value}, state, log}

        :not_assoc ->
          # No => found - this is just an expression (for map update base)
          {:ok, expr, state, log}
      end
    end
  end

  # Extract the rightmost => from the expression tree
  # In a :: (b => c), we want key=(a :: b), value=c
  # This transforms the AST to extract the correct key/value split
  defp extract_assoc({:"=>", meta, [key, value]}) do
    # Found => at top level - this is an assoc expression
    {:assoc, key, value, meta}
  end

  defp extract_assoc({op, meta, [left, right]} = _expr) when is_atom(op) do
    # Check if right side contains =>
    case extract_assoc(right) do
      {:assoc, right_key, value, assoc_meta} ->
        # Reconstruct: {op, meta, [left, right_key]} => value
        new_key = {op, meta, [left, right_key]}
        {:assoc, new_key, value, assoc_meta}

      :not_assoc ->
        # Check left side (for operators with left-to-right parsing)
        case extract_assoc(left) do
          {:assoc, left_key, left_value, assoc_meta} ->
            # This shouldn't happen for well-formed expressions, but handle it
            {:assoc, left_key, {op, meta, [left_value, right]}, assoc_meta}

          :not_assoc ->
            :not_assoc
        end
    end
  end

  defp extract_assoc(_), do: :not_assoc

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

  defp token_meta_with_newlines(meta, 0), do: token_meta(meta)

  defp token_meta_with_newlines(%{range: %{start: %{line: line, column: column}}}, newlines)
       when newlines > 0 do
    [newlines: newlines, line: line, column: column]
  end

  defp token_meta_with_newlines(_, _), do: []

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
