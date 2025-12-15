defmodule ToxicParser.Grammar.Maps do
  @moduledoc """
  Parsing for maps and structs, including updates inside `%{}`.
  """

  alias ToxicParser.{Builder, EventLog, Pratt, Precedence, State, TokenAdapter}
  alias ToxicParser.Grammar.{EOE, Keywords}

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
      {:ok, %{kind: kind} = _tok, _} when kind in [:at_op, :unary_op, :ellipsis_op, :dual_op] ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        with {:ok, operand, state, log} <- parse_map_base_expr(state, :matched, log) do
          op_meta = token_meta(op_tok.metadata)
          ast = {op_tok.value, op_meta, [operand]}
          {:ok, ast, state, log}
        end

      # ternary_op :"//" used as unary prefix (e.g., %//foo{})
      # Produces: {:/, outer_meta, [{:/, inner_meta, nil}, operand]}
      {:ok, %{kind: :ternary_op, value: :"//"} = _tok, _} ->
        {:ok, op_tok, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

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
        # sub_matched_expr with dot operator and paren call support
        # parse_base_with_dots_and_calls handles:
        #   - Foo.Bar.Baz (dotted aliases)
        #   - unquote(struct) (paren calls)
        #   - module.Foo.Bar (dotted calls)
        # But stops at { which is the struct body
        Pratt.parse_base_with_dots_and_calls(state, :matched, log)
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
    if starts_with_kw_or_string?(state) do
      {:not_update, state}
    else
      {checkpoint_id, checkpoint_state} = TokenAdapter.checkpoint(state)

      case parse_map_update_candidate(checkpoint_state, ctx, log) do
        {:ok, update_ast, close_meta, state, log} ->
          {:ok, update_ast, close_meta, discard_checkpoint(state, checkpoint_id), log}

        {:not_update, state} ->
          {:not_update, TokenAdapter.rewind(state, checkpoint_id)}

        {:error, diag, state, log} ->
          {:error, diag, TokenAdapter.rewind(state, checkpoint_id), log}
      end
    end
  end

  defp parse_map_update_candidate(state, ctx, log) do
    with {:ok, base_expr, state, log} <- parse_map_update_base(state, ctx, log),
         :ok <- validate_map_update_base(base_expr),
         {:ok, pipe_meta, state} <- parse_map_update_pipe(state, log),
         {:ok, update_ast, close_meta, state, log} <-
           parse_map_update_rhs(base_expr, pipe_meta, state, ctx, log) do
      {:ok, update_ast, close_meta, state, log}
    else
      :not_update -> {:not_update, state}
      {:error, _, _, _} = err -> err
    end
  end

  defp parse_map_update_base(state, _ctx, log) do
    case Pratt.parse_with_min_bp(state, :unmatched, log, Precedence.pipe_op_bp() + 1) do
      {:ok, base_expr, state, log} ->
        {:ok, base_expr, state, log}

      {:keyword_key, _, state, _} ->
        {:not_update, state}

      {:keyword_key_interpolated, _, _, _, _, state, _} ->
        {:not_update, state}

      {:error, diag, state, log} ->
        {:error, diag, state, log}
    end
  end

  defp validate_map_update_base(base_expr) do
    cond do
      is_assoc_expr?(base_expr) ->
        :not_update

      # Grammar rule: unmatched_expr -> unary_op_eol expr
      # A unary operator with an unmatched operand consumes the following expr, including |.
      is_unary_with_unmatched_operand?(base_expr) ->
        :not_update

      true ->
        :ok
    end
  end

  defp parse_map_update_pipe(state, log) do
    state = EOE.skip(state)

    case TokenAdapter.peek(state) do
      {:ok, %{kind: :pipe_op} = pipe_tok, _} ->
        {:ok, _pipe, state} = TokenAdapter.next(state)
        {state, newlines_after_pipe} = EOE.skip_count_newlines(state, 0)
        {:ok, build_pipe_meta(pipe_tok, newlines_after_pipe), state}

      {:ok, _tok, _} ->
        :not_update

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_map_update_rhs(base_expr, pipe_meta, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          parse_map_update_keyword_rhs(base_expr, pipe_meta, state, ctx, log)
        else
          parse_map_update_assoc_rhs(base_expr, pipe_meta, state, ctx, log)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_map_update_keyword_rhs(base_expr, pipe_meta, state, ctx, log) do
    with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
      state = EOE.skip(state)

      case TokenAdapter.next(state) do
        {:ok, %{kind: :"}"} = close_tok, state} ->
          close_meta = token_meta(close_tok.metadata)
          {:ok, {:|, pipe_meta, [base_expr, kw_list]}, close_meta, state, log}

        {:ok, tok, state} ->
          {:error, {:expected, :"}", got: tok.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  defp parse_map_update_assoc_rhs(base_expr, pipe_meta, state, ctx, log) do
    with {:ok, entries, close_meta, state, log} <- parse_map_close(state, ctx, log) do
      first_key = get_first_entry_key(entries)

      if first_key != nil and key_has_lower_precedence_op?(first_key) do
        :not_update
      else
        {:ok, {:|, pipe_meta, [base_expr, entries]}, close_meta, state, log}
      end
    end
  end

  defp starts_with_kw_or_string?(state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: kind}, _}
      when kind in [:kw_identifier, :bin_string_start, :list_string_start] ->
        true

      _ ->
        false
    end
  end

  defp build_pipe_meta(pipe_tok, newlines_after_pipe) do
    token_newlines = Map.get(pipe_tok.metadata, :newlines, 0)
    effective_newlines = max(token_newlines, newlines_after_pipe)
    token_meta_with_newlines(pipe_tok.metadata, effective_newlines)
  end

  # Check if an expression is an assoc expression (has => at top level)
  defp is_assoc_expr?({:"=>", _, _}), do: true
  defp is_assoc_expr?(_), do: false

  # Check if expression is a unary operator with an unmatched operand (has do-block)
  # In Elixir grammar: unmatched_expr -> unary_op_eol expr
  # This means the unary operator consumes the full `expr` including any following |
  # So %{+ foo do end | b => c} should NOT be a map update
  #
  # Unary operators: +, -, !, ^, not, ~~~, &
  @unary_ops [:+, :-, :!, :^, :not, :"~~~", :&]

  defp is_unary_with_unmatched_operand?({op, _meta, [operand]}) when op in @unary_ops do
    has_do_block?(operand)
  end

  defp is_unary_with_unmatched_operand?(_), do: false

  # Check if an expression has a do-block (making it "unmatched")
  # Do-blocks appear as a keyword list with :do key in the args
  defp has_do_block?({_name, meta, args}) when is_list(meta) and is_list(args) do
    Keyword.has_key?(meta, :do) or
      (length(args) > 0 and is_list(List.last(args)) and has_do_keyword?(List.last(args)))
  end

  defp has_do_block?(_), do: false

  defp has_do_keyword?(args) when is_list(args) do
    Enum.any?(args, fn
      {:do, _} -> true
      {key, _} when is_atom(key) -> key == :do
      _ -> false
    end)
  end

  defp has_do_keyword?(_), do: false

  # Get the key of the first entry in a list of map entries
  # Entries can be {key, value} tuples or keyword pairs
  defp get_first_entry_key([{key, _value} | _]), do: key
  defp get_first_entry_key(_), do: nil

  # Check if the KEY of an assoc entry contains operators with precedence lower than pipe_op (70).
  # If so, the | we saw should have been part of the key, not the map update separator.
  # This handles cases like %{a | b :: c => d} where :: (bp=60) < | (bp=70), meaning
  # the | is part of the key expression (a | b) :: c, not a map update separator.
  #
  # Operators with precedence < 70:
  # - type_op (::): 60
  # - when_op: 50
  # - in_match_op (<-): 40
  # - stab_op (->): 10
  defp key_has_lower_precedence_op?({op, _, args}) when is_atom(op) and is_list(args) do
    # Check if this node's operator has precedence < 70
    op_has_low_precedence?(op) or
      Enum.any?(args, &key_has_lower_precedence_op?/1)
  end

  defp key_has_lower_precedence_op?(_), do: false

  # Check if operator has precedence lower than pipe_op (70)
  # Operators with precedence < 70:
  # - type_op (::): 60
  # - when_op: 50
  # - in_match_op (<-, \\): 40
  # - stab_op (->): 10
  defp op_has_low_precedence?(op) when op in [:"::", :when, :<-, :\\, :",", :->, :do], do: true
  defp op_has_low_precedence?(_), do: false

  # Parse map_close: kw_data close_curly | assoc close_curly | assoc_base ',' kw_data close_curly
  defp parse_map_close(state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          # kw_data close_curly
          with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
            state = EOE.skip(state)

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
      state = EOE.skip(state)

      case TokenAdapter.peek(state) do
        {:ok, %{kind: :"}"} = close_tok, _} ->
          {:ok, _close, state} = TokenAdapter.next(state)
          close_meta = token_meta(close_tok.metadata)
          {:ok, Enum.reverse([entry | acc]), close_meta, state, log}

        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state)
          state = EOE.skip(state)
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
                  state = EOE.skip(state)

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
    # Even when the surrounding context is :matched (e.g. inside no-parens call
    # args), map keys/values must still allow full container expressions,
    # including block expressions like `try do ... end` used as a key.
    #
    # This matches Elixir's behavior: you can write `%{try do ... end => 1}`.
    _ctx = ctx

    # Parse the full expression - this will include => as a binary operator
    # Then extract the key/value from the rightmost => in the expression tree
    case Pratt.parse(state, :unmatched, log) do
      {:ok, expr, state, log} ->
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

      # keyword_key means "string": - convert to keyword pair
      {:keyword_key, key_atom, state, log} ->
        alias ToxicParser.Grammar.Expressions
        # Skip EOE (newlines) after the colon before parsing value
        state = EOE.skip(state)

        with {:ok, value_ast, state, log} <- Expressions.expr(state, :unmatched, log) do
          {:ok, {key_atom, value_ast}, state, log}
        end

      {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, log} ->
        alias ToxicParser.Grammar.Expressions
        # Skip EOE (newlines) after the colon before parsing value
        state = EOE.skip(state)

        with {:ok, value_ast, state, log} <- Expressions.expr(state, :unmatched, log) do
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

  # Binary operator - check both sides
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

  # Unary operator - check the single operand
  defp extract_assoc({op, meta, [operand]} = _expr) when is_atom(op) do
    case extract_assoc(operand) do
      {:assoc, operand_key, value, assoc_meta} ->
        # Reconstruct: {op, meta, [operand_key]} => value
        new_key = {op, meta, [operand_key]}
        {:assoc, new_key, value, assoc_meta}

      :not_assoc ->
        :not_assoc
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

  # Build token metadata with explicit newlines count
  defp discard_checkpoint(state, checkpoint_id) do
    %{state | checkpoints: Map.delete(state.checkpoints, checkpoint_id)}
  end

  defp token_meta_with_newlines(meta, 0), do: token_meta(meta)

  defp token_meta_with_newlines(meta, newlines) when is_integer(newlines) and newlines > 0 do
    [newlines: newlines] ++ Builder.Helpers.token_meta(meta)
  end

  defp token_meta_with_newlines(_, _), do: []

  defp token_meta(meta), do: Builder.Helpers.token_meta(meta)

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
