defmodule ToxicParser.Grammar.Containers do
  @moduledoc """
  Container parsing for lists and tuples (Phase 6 scaffolding).
  """

  alias ToxicParser.{EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.{Bitstrings, Expressions, Keywords, Maps}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}
          | {:no_container, State.t()}

  @spec parse(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse(%State{} = state, ctx, %EventLog{} = log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"("}, _} ->
        parse_paren(state, ctx, log)

      {:ok, %{kind: :"["}, _} ->
        parse_list(state, ctx, log)

      {:ok, %{kind: :"{", value: _}, _} ->
        parse_tuple(state, ctx, log)

      {:ok, %{kind: :%{}}, _} ->
        Maps.parse_map(state, ctx, log)

      {:ok, %{kind: :%}, _} ->
        Maps.parse_map(state, ctx, log)

      {:ok, %{kind: :"<<", value: _}, _} ->
        Bitstrings.parse(state, ctx, log)

      {:eof, state} ->
        {:no_container, state}

      {:error, _diag, state} ->
        {:no_container, state}

      _ ->
        {:no_container, state}
    end
  end

  # Parse parenthesized expression, empty parens, or stab expression
  # Grammar rules:
  #   access_expr -> open_paren stab_eoe ')'         : build_paren_stab
  #   access_expr -> open_paren ';' stab_eoe ')'     : build_paren_stab
  #   access_expr -> open_paren ';' close_paren      : build_paren_stab with nil
  #   access_expr -> empty_paren                     : wrap in __block__
  defp parse_paren(state, ctx, log) do
    {:ok, open_tok, state} = TokenAdapter.next(state)
    open_meta = token_meta(open_tok.metadata)

    # Skip leading newlines (but not semicolons)
    state = skip_eoe_not_semicolon(state)

    # Check for leading semicolon (forces stab interpretation)
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, value: %{source: :semicolon}}, _} ->
        # Leading semicolon - parse as stab or empty
        {:ok, _semi, state} = TokenAdapter.next(state)
        # Skip any additional EOE
        state = skip_eoe(state)
        parse_paren_stab_or_empty(open_meta, state, ctx, log)

      _ ->
        # Skip any remaining EOE tokens
        state = skip_eoe(state)
        parse_paren_content(open_meta, state, ctx, log)
    end
  end

  # Parse content after open paren (no leading semicolon)
  defp parse_paren_content(open_meta, state, ctx, log) do
    case TokenAdapter.peek(state) do
      # Empty parens: () -> {:__block__, [parens: ...], []}
      {:ok, %{kind: :")"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        parens_meta = [parens: open_meta ++ [closing: close_meta]]
        ast = {:__block__, parens_meta, []}
        {:ok, ast, state, log}

      # Check for stab operator at start: (-> expr)
      {:ok, %{kind: :stab_op}, _} ->
        parse_paren_stab(open_meta, state, ctx, log)

      # Check for empty inner parens followed by stab/when: (() -> expr) or (() when g -> expr)
      {:ok, %{kind: :"("}, _} ->
        try_parse_stab_parens_many(open_meta, state, ctx, log)

      # Content that could be expression or stab pattern
      {:ok, _, _} ->
        # Try to parse stab first using checkpoint
        try_parse_stab_or_expr(open_meta, state, ctx, log)

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # After leading semicolon: either close paren (empty) or stab content
  defp parse_paren_stab_or_empty(open_meta, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :")"} = close_tok, _} ->
        # (;) -> empty stab with semicolon
        # Output: {:__block__, [closing: [...], line: L, column: C], []}
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        meta = [closing: close_meta] ++ open_meta
        ast = {:__block__, meta, []}
        {:ok, ast, state, log}

      _ ->
        # Parse stab expression(s) after leading semicolon
        parse_paren_stab(open_meta, state, ctx, log)
    end
  end

  # Parse stab expressions inside parens
  defp parse_paren_stab(open_meta, state, ctx, log) do
    with {:ok, clauses, state, log} <- parse_stab_eoe([], state, ctx, log) do
      state = skip_eoe(state)
      case TokenAdapter.next(state) do
        {:ok, %{kind: :")"}, state} ->
          {:ok, clauses, state, log}

        {:ok, token, state} ->
          {:error, {:expected, :")", got: token.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  # Try to parse stab_parens_many: ((args) -> expr) or ((args) when g -> expr)
  defp try_parse_stab_parens_many(open_meta, state, ctx, log) do
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

    # Consume the inner open paren
    {:ok, inner_open_tok, inner_state} = TokenAdapter.next(checkpoint_state)
    inner_open_meta = token_meta(inner_open_tok.metadata)

    # Skip EOE inside inner parens
    inner_state = skip_eoe(inner_state)

    case TokenAdapter.peek(inner_state) do
      # Empty inner parens: () -> or () when
      {:ok, %{kind: :")"} = inner_close_tok, _} ->
        {:ok, _close, inner_state} = TokenAdapter.next(inner_state)
        inner_close_meta = token_meta(inner_close_tok.metadata)
        # Check what follows: -> or when
        case TokenAdapter.peek(inner_state) do
          {:ok, %{kind: :stab_op}, _} ->
            # (() -> expr)
            parse_empty_paren_stab(open_meta, inner_open_meta, inner_close_meta, inner_state, ctx, log)

          {:ok, %{kind: :when_op}, _} ->
            # (() when guard -> expr)
            parse_empty_paren_when_stab(open_meta, inner_open_meta, inner_close_meta, inner_state, ctx, log)

          _ ->
            # Not a stab - rewind and parse as expression
            state = TokenAdapter.rewind(checkpoint_state, ref)
            parse_expr_in_paren_with_meta(open_meta, state, ctx, log)
        end

      # Inner parens has content - could be stab_parens_many
      {:ok, _, _} ->
        try_parse_stab_parens_many_content(open_meta, inner_open_meta, ref, checkpoint_state, inner_state, ctx, log)

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse (() -> expr) - empty paren stab
  defp parse_empty_paren_stab(_open_meta, inner_open_meta, inner_close_meta, state, ctx, log) do
    {:ok, stab_tok, state} = TokenAdapter.next(state)
    stab_base_meta = token_meta(stab_tok.metadata)

    # Skip EOE after stab and count newlines
    {state, newlines} = skip_eoe_count_newlines(state, 0)
    newlines_meta = if newlines > 0, do: [newlines: newlines], else: []

    # Parse body (or empty if just ->)
    with {:ok, body, state, log} <- parse_stab_body(state, ctx, log) do
      # Build stab clause with empty patterns and parens metadata
      parens_meta = [parens: inner_open_meta ++ [closing: inner_close_meta]]
      clause = {:->, parens_meta ++ newlines_meta ++ stab_base_meta, [[], body]}

      # Check for more clauses
      parse_remaining_stab_clauses([clause], state, ctx, log)
    end
  end

  # Parse (() when guard -> expr) - empty paren with guard
  defp parse_empty_paren_when_stab(_open_meta, inner_open_meta, inner_close_meta, state, ctx, log) do
    {:ok, when_tok, state} = TokenAdapter.next(state)
    when_meta = token_meta(when_tok.metadata)

    # Skip EOE after when
    state = skip_eoe(state)

    # Parse guard expression with min_bp > stab_op (10) to stop before ->
    with {:ok, guard, state, log} <- Pratt.parse_with_min_bp(state, :matched, log, 11) do
      # Expect stab
      case TokenAdapter.next(state) do
        {:ok, %{kind: :stab_op} = stab_tok, state} ->
          stab_base_meta = token_meta(stab_tok.metadata)
          # Skip EOE after stab and count newlines
          {state, newlines} = skip_eoe_count_newlines(state, 0)
          newlines_meta = if newlines > 0, do: [newlines: newlines], else: []

          with {:ok, body, state, log} <- parse_stab_body(state, ctx, log) do
            # Build guarded stab clause
            parens_meta = [parens: inner_open_meta ++ [closing: inner_close_meta]]
            guard_ast = {:when, when_meta, [guard]}
            clause = {:->, parens_meta ++ newlines_meta ++ stab_base_meta, [[guard_ast], body]}

            parse_remaining_stab_clauses([clause], state, ctx, log)
          end

        {:ok, token, state} ->
          {:error, {:expected, :stab_op, got: token.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  # Try parse ((args) -> expr) where args are call_args_no_parens_kw or call_args_no_parens_many
  defp try_parse_stab_parens_many_content(open_meta, inner_open_meta, ref, checkpoint_state, inner_state, ctx, log) do
    # Try to parse the content as stab pattern arguments
    case parse_stab_parens_args(inner_state, ctx, log) do
      {:ok, args, inner_state, log} ->
        inner_state = skip_eoe(inner_state)
        case TokenAdapter.next(inner_state) do
          {:ok, %{kind: :")"} = inner_close_tok, inner_state} ->
            inner_close_meta = token_meta(inner_close_tok.metadata)
            # Check for -> or when
            case TokenAdapter.peek(inner_state) do
              {:ok, %{kind: :stab_op}, _} ->
                parse_stab_parens_many_stab(open_meta, inner_open_meta, inner_close_meta, args, inner_state, ctx, log)

              {:ok, %{kind: :when_op}, _} ->
                parse_stab_parens_many_when(open_meta, inner_open_meta, inner_close_meta, args, inner_state, ctx, log)

              _ ->
                # Not a stab - rewind and parse as expression
                state = TokenAdapter.rewind(checkpoint_state, ref)
                parse_expr_in_paren_with_meta(open_meta, state, ctx, log)
            end

          _ ->
            # Not valid stab parens - rewind and parse as expression
            state = TokenAdapter.rewind(checkpoint_state, ref)
            parse_expr_in_paren_with_meta(open_meta, state, ctx, log)
        end

      {:error, _, _, _} ->
        # Failed to parse stab args - rewind and parse as expression
        state = TokenAdapter.rewind(checkpoint_state, ref)
        parse_expr_in_paren_with_meta(open_meta, state, ctx, log)
    end
  end

  # ((args) -> expr)
  defp parse_stab_parens_many_stab(_open_meta, inner_open_meta, inner_close_meta, args, state, ctx, log) do
    {:ok, stab_tok, state} = TokenAdapter.next(state)
    stab_base_meta = token_meta(stab_tok.metadata)
    # Skip EOE after stab and count newlines
    {state, newlines} = skip_eoe_count_newlines(state, 0)
    newlines_meta = if newlines > 0, do: [newlines: newlines], else: []

    with {:ok, body, state, log} <- parse_stab_body(state, ctx, log) do
      parens_meta = [parens: inner_open_meta ++ [closing: inner_close_meta]]
      clause = {:->, parens_meta ++ newlines_meta ++ stab_base_meta, [args, body]}
      parse_remaining_stab_clauses([clause], state, ctx, log)
    end
  end

  # ((args) when guard -> expr)
  defp parse_stab_parens_many_when(_open_meta, inner_open_meta, inner_close_meta, args, state, ctx, log) do
    {:ok, when_tok, state} = TokenAdapter.next(state)
    when_meta = token_meta(when_tok.metadata)
    state = skip_eoe(state)

    # Parse guard expression with min_bp > stab_op (10) to stop before ->
    with {:ok, guard, state, log} <- Pratt.parse_with_min_bp(state, :matched, log, 11) do
      case TokenAdapter.next(state) do
        {:ok, %{kind: :stab_op} = stab_tok, state} ->
          stab_base_meta = token_meta(stab_tok.metadata)
          # Skip EOE after stab and count newlines
          {state, newlines} = skip_eoe_count_newlines(state, 0)
          newlines_meta = if newlines > 0, do: [newlines: newlines], else: []

          with {:ok, body, state, log} <- parse_stab_body(state, ctx, log) do
            parens_meta = [parens: inner_open_meta ++ [closing: inner_close_meta]]
            guard_ast = {:when, when_meta, args ++ [guard]}
            clause = {:->, parens_meta ++ newlines_meta ++ stab_base_meta, [[guard_ast], body]}
            parse_remaining_stab_clauses([clause], state, ctx, log)
          end

        {:ok, token, state} ->
          {:error, {:expected, :stab_op, got: token.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  # Parse arguments inside stab_parens_many: call_args_no_parens_kw or call_args_no_parens_many
  defp parse_stab_parens_args(state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          # call_args_no_parens_kw
          with {:ok, kw_list, state, log} <- Keywords.parse_kw_call(state, ctx, log) do
            {:ok, [kw_list], state, log}
          end
        else
          # Parse matched expressions
          parse_stab_parens_exprs([], state, ctx, log)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_stab_parens_exprs(acc, state, _ctx, log) do
    # Parse expression with min_bp > when_op (50) to stop before -> and when
    with {:ok, expr, state, log} <- Pratt.parse_with_min_bp(state, :matched, log, 51) do
      state_after_eoe = skip_eoe(state)
      case TokenAdapter.peek(state_after_eoe) do
        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state_after_eoe)
          state = skip_eoe(state)
          # Check for keyword after comma
          case TokenAdapter.peek(state) do
            {:ok, tok, _} ->
              if Keywords.starts_kw?(tok) do
                # call_args_no_parens_many: exprs followed by kw
                with {:ok, kw_list, state, log} <- Keywords.parse_kw_call(state, :matched, log) do
                  {:ok, Enum.reverse(acc) ++ [expr, kw_list], state, log}
                end
              else
                parse_stab_parens_exprs([expr | acc], state, :matched, log)
              end

            _ ->
              parse_stab_parens_exprs([expr | acc], state, :matched, log)
          end

        {:ok, %{kind: :")"}, _} ->
          {:ok, Enum.reverse([expr | acc]), state_after_eoe, log}

        _ ->
          {:ok, Enum.reverse([expr | acc]), state, log}
      end
    end
  end

  # Try parsing as stab or fallback to expression
  defp try_parse_stab_or_expr(open_meta, state, ctx, log) do
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

    case try_parse_stab_clause(checkpoint_state, ctx, log) do
      {:ok, clause, new_state, log} ->
        # Successfully parsed a stab clause, continue with remaining clauses
        parse_remaining_stab_clauses([clause], new_state, ctx, log)

      {:not_stab, _state, _log} ->
        # Not a stab - rewind and parse as regular expression
        state = TokenAdapter.rewind(checkpoint_state, ref)
        parse_expr_in_paren_with_meta(open_meta, state, ctx, log)

      {:error, reason, state, log} ->
        # Error during stab parsing - could try expression fallback
        {:error, reason, state, log}
    end
  end

  # Try to parse a stab clause, returning {:not_stab, ...} if it's not a stab
  defp try_parse_stab_clause(state, ctx, log) do
    # Parse the pattern part first
    with {:ok, patterns, state, log} <- parse_stab_patterns([], state, ctx, log) do
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :stab_op} = stab_tok, _} ->
          # This is definitely a stab clause
          {:ok, _stab, state} = TokenAdapter.next(state)
          stab_base_meta = token_meta(stab_tok.metadata)
          # Skip EOE after stab and count newlines
          {state, newlines} = skip_eoe_count_newlines(state, 0)
          stab_meta = if newlines > 0, do: [newlines: newlines] ++ stab_base_meta, else: stab_base_meta

          with {:ok, body, state, log} <- parse_stab_body(state, ctx, log) do
            clause = {:->, stab_meta, [patterns, body]}
            {:ok, clause, state, log}
          end

        {:ok, %{kind: :when_op} = when_tok, _} ->
          # Pattern followed by guard then stab
          {:ok, _when, state} = TokenAdapter.next(state)
          when_meta = token_meta(when_tok.metadata)
          state = skip_eoe(state)

          # Parse guard expression with min_bp > stab_op (10) to stop before ->
          with {:ok, guard, state, log} <- Pratt.parse_with_min_bp(state, :matched, log, 11) do
            case TokenAdapter.next(state) do
              {:ok, %{kind: :stab_op} = stab_tok, state} ->
                stab_base_meta = token_meta(stab_tok.metadata)
                # Skip EOE after stab and count newlines
                {state, newlines} = skip_eoe_count_newlines(state, 0)
                stab_meta = if newlines > 0, do: [newlines: newlines] ++ stab_base_meta, else: stab_base_meta

                with {:ok, body, state, log} <- parse_stab_body(state, ctx, log) do
                  # Wrap patterns with guard in a when clause
                  patterns_with_guard = unwrap_when(patterns, guard, when_meta)
                  clause = {:->, stab_meta, [patterns_with_guard, body]}
                  {:ok, clause, state, log}
                end

              {:ok, token, state} ->
                {:error, {:expected, :stab_op, got: token.kind}, state, log}

              {:eof, state} ->
                {:not_stab, state, log}

              {:error, diag, state} ->
                {:error, diag, state, log}
            end
          end

        _ ->
          # No stab operator - not a stab clause
          {:not_stab, state, log}
      end
    end
  end

  # Unwrap when: combine patterns with guard
  defp unwrap_when(patterns, guard, when_meta) do
    [{:when, when_meta, patterns ++ [guard]}]
  end

  # Parse stab patterns: either single expression or comma-separated matched expressions
  # Grammar: stab_expr -> stab_op_eol_and_expr (no patterns, just -> body)
  defp parse_stab_patterns(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      # If we immediately see stab_op, there are no patterns
      {:ok, %{kind: :stab_op}, _} ->
        {:ok, [], state, log}

      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          # call_args_no_parens_kw: (x: 1 -> body)
          # Use min_bp > stab_op (10) to stop keyword values before ->
          # The kw_list is already [x: 1], and we need to wrap it in a list to get [[x: 1]]
          with {:ok, kw_list, state, log} <- Keywords.parse_kw_call_with_min_bp(state, ctx, log, 11) do
            {:ok, [kw_list], state, log}
          end
        else
          parse_stab_pattern_exprs(acc, state, ctx, log)
        end

      _ ->
        parse_stab_pattern_exprs(acc, state, ctx, log)
    end
  end

  defp parse_stab_pattern_exprs(acc, state, _ctx, log) do
    # Parse expression with min_bp > when_op (50) to stop before -> and when
    # This implements call_args_no_parens_expr -> matched_expr
    with {:ok, expr, state, log} <- Pratt.parse_with_min_bp(state, :matched, log, 51) do
      state_after_eoe = skip_eoe(state)
      case TokenAdapter.peek(state_after_eoe) do
        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state_after_eoe)
          state = skip_eoe(state)
          # Check for keyword after comma
          case TokenAdapter.peek(state) do
            {:ok, tok, _} ->
              if Keywords.starts_kw?(tok) do
                # call_args_no_parens_many: exprs followed by kw
                # Use min_bp > stab_op (10) to stop keyword values before ->
                with {:ok, kw_list, state, log} <- Keywords.parse_kw_call_with_min_bp(state, :matched, log, 11) do
                  {:ok, Enum.reverse(acc) ++ [expr, kw_list], state, log}
                end
              else
                parse_stab_pattern_exprs([expr | acc], state, :matched, log)
              end

            _ ->
              parse_stab_pattern_exprs([expr | acc], state, :matched, log)
          end

        {:ok, %{kind: :stab_op}, _} ->
          {:ok, Enum.reverse([expr | acc]), state, log}

        {:ok, %{kind: :when_op}, _} ->
          {:ok, Enum.reverse([expr | acc]), state, log}

        _ ->
          {:ok, Enum.reverse([expr | acc]), state, log}
      end
    end
  end

  # Parse the body of a stab clause (expression or nil if empty)
  defp parse_stab_body(state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: kind}, _} when kind in [:")", :eoe] ->
        # Empty body - return nil (as per elixir_parser: handle_literal(nil, '$1'))
        {:ok, nil, state, log}

      {:ok, _, _} ->
        Expressions.expr(state, ctx, log)

      {:eof, state} ->
        {:ok, nil, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse remaining stab clauses after the first one
  defp parse_remaining_stab_clauses(acc, state, ctx, log) do
    state = skip_eoe(state)
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :")"}, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        {:ok, Enum.reverse(acc), state, log}

      {:ok, _, _} ->
        # Try to parse another stab clause
        case try_parse_stab_clause(state, ctx, log) do
          {:ok, clause, state, log} ->
            parse_remaining_stab_clauses([clause | acc], state, ctx, log)

          {:not_stab, state, log} ->
            # Expression without stab - this is an error in stab context
            {:error, {:expected, :stab_op, got: :expression}, state, log}

          {:error, reason, state, log} ->
            {:error, reason, state, log}
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse stab_eoe: list of stab clauses separated by eoe
  defp parse_stab_eoe(acc, state, ctx, log) do
    case try_parse_stab_clause(state, ctx, log) do
      {:ok, clause, state, log} ->
        state = skip_eoe(state)
        case TokenAdapter.peek(state) do
          {:ok, %{kind: :")"}, _} ->
            {:ok, Enum.reverse([clause | acc]), state, log}

          {:ok, _, _} ->
            parse_stab_eoe([clause | acc], state, ctx, log)

          {:eof, state} ->
            {:error, :unexpected_eof, state, log}

          {:error, diag, state} ->
            {:error, diag, state, log}
        end

      {:not_stab, state, log} ->
        # Could be just an expression (stab_expr -> expr)
        with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
          {:ok, Enum.reverse([expr | acc]), state, log}
        end

      {:error, reason, state, log} ->
        {:error, reason, state, log}
    end
  end

  # Parse expression in parentheses (simple case, no stab)
  # Need to capture open/close metadata for parens: metadata
  defp parse_expr_in_paren(state, ctx, log) do
    # We need open_tok but it was consumed in parse_paren
    # Actually we need to restructure - for now, peek back to get open_meta
    # This is called after we've already consumed ( and skipped EOE
    # We don't have access to open_tok here, so we need to pass it through
    # For now, let me refactor to pass open_meta through
    parse_expr_in_paren_impl(nil, state, ctx, log)
  end

  defp parse_expr_in_paren_with_meta(open_meta, state, ctx, log) do
    parse_expr_in_paren_impl(open_meta, state, ctx, log)
  end

  defp parse_expr_in_paren_impl(open_meta, state, ctx, log) do
    with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
      state = skip_eoe(state)
      case TokenAdapter.next(state) do
        {:ok, %{kind: :")"} = close_tok, state} ->
          close_meta = token_meta(close_tok.metadata)
          # Add parens metadata to 3-tuple AST nodes
          expr = add_parens_meta(expr, open_meta, close_meta)
          {:ok, expr, state, log}

        {:ok, token, state} ->
          {:error, {:expected, :")", got: token.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  # Add parens: metadata to 3-tuple AST nodes
  defp add_parens_meta({name, meta, args}, open_meta, close_meta) when is_list(meta) do
    parens_meta = build_parens_meta(open_meta, close_meta)
    {name, [parens: parens_meta] ++ meta, args}
  end

  defp add_parens_meta(literal, _open_meta, _close_meta), do: literal

  defp build_parens_meta(nil, close_meta) do
    # No open_meta available - just use closing
    [closing: close_meta]
  end

  defp build_parens_meta(open_meta, close_meta) do
    open_meta ++ [closing: close_meta]
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

  # Skip EOE tokens but stop at semicolons (semicolons force stab interpretation)
  defp skip_eoe_not_semicolon(state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, value: %{source: :semicolon}}, _} ->
        # Don't skip semicolons
        state

      {:ok, %{kind: :eoe}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe_not_semicolon(state)

      _ ->
        state
    end
  end

  defp token_meta(%{range: %{start: %{line: line, column: column}}}), do: [line: line, column: column]
  defp token_meta(_), do: []

  defp expect(state, kind) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: ^kind}, state} -> {:ok, kind, state}
      {:ok, token, state} -> {:error, {:expected, kind, got: token.kind}, state}
      {:eof, state} -> {:error, :unexpected_eof, state}
      {:error, diag, state} -> {:error, diag, state}
    end
  end

  defp parse_list(state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    # Skip leading EOE
    {state, _newlines} = skip_eoe_count_newlines(state, 0)

    case TokenAdapter.peek(state) do
      # Empty list
      {:ok, %{kind: :"]"}, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        {:ok, [], state, log}

      {:ok, _, _} ->
        parse_list_elements([], state, ctx, log)

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse list elements following the list_args grammar
  defp parse_list_elements(acc, state, ctx, log) do
    # Check if next token starts a keyword list
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          # Parse keyword data and finish (list_args -> kw_data)
          with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
            # Skip EOE before close
            {state, _newlines} = skip_eoe_count_newlines(state, 0)
            case TokenAdapter.next(state) do
              {:ok, %{kind: :"]"}, state} ->
                {:ok, Enum.reverse(acc) ++ kw_list, state, log}

              {:ok, tok, state} ->
                {:error, {:expected, :"]", got: tok.kind}, state, log}

              {:eof, state} ->
                {:error, :unexpected_eof, state, log}

              {:error, diag, state} ->
                {:error, diag, state, log}
            end
          end
        else
          parse_list_element(acc, state, ctx, log)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_list_element(acc, state, ctx, log) do
    with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
      # Skip EOE after expression
      {state, _newlines} = skip_eoe_count_newlines(state, 0)

      case TokenAdapter.peek(state) do
        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state)
          # Skip EOE after comma
          {state, _newlines} = skip_eoe_count_newlines(state, 0)
          # Check for trailing comma or close
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :"]"}, _} ->
              {:ok, _close, state} = TokenAdapter.next(state)
              {:ok, Enum.reverse([expr | acc]), state, log}

            _ ->
              parse_list_elements([expr | acc], state, ctx, log)
          end

        {:ok, %{kind: :"]"}, _} ->
          {:ok, _close, state} = TokenAdapter.next(state)
          {:ok, Enum.reverse([expr | acc]), state, log}

        {:ok, tok, state} ->
          {:error, {:expected_comma_or, :"]", got: tok.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  defp parse_tuple(state, ctx, log) do
    {:ok, open_tok, state} = TokenAdapter.next(state)
    open_meta = token_meta(open_tok.metadata)

    with {:ok, elements, newlines, close_meta, state, log} <- parse_tuple_args(state, ctx, log) do
      newlines_meta = if newlines > 0, do: [newlines: newlines], else: []
      meta = newlines_meta ++ [closing: close_meta] ++ open_meta

      # 2-element tuples are represented as literal {a, b}
      # Other sizes use {:{}, meta, elements}
      ast =
        case elements do
          [a, b] -> {a, b}
          _ -> {:{}, meta, elements}
        end

      {:ok, ast, state, log}
    end
  end

  defp parse_tuple_args(state, ctx, log) do
    # Skip leading EOE and count newlines
    {state, leading_newlines} = skip_eoe_count_newlines(state, 0)

    case TokenAdapter.peek(state) do
      # Empty tuple
      {:ok, %{kind: :"}"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        {:ok, [], leading_newlines, close_meta, state, log}

      {:ok, _, _} ->
        with {:ok, elements, close_meta, state, log} <- parse_tuple_elements([], state, ctx, log) do
          {:ok, elements, leading_newlines, close_meta, state, log}
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
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

  # Parse tuple elements, returning close token metadata
  defp parse_tuple_elements(acc, state, ctx, log) do
    # Check if next token starts a keyword list
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          # Parse keyword data and finish
          with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
            # Skip EOE before close
            {state, _newlines} = skip_eoe_count_newlines(state, 0)
            case TokenAdapter.next(state) do
              {:ok, %{kind: :"}"} = close_tok, state} ->
                close_meta = token_meta(close_tok.metadata)
                {:ok, Enum.reverse([kw_list | acc]), close_meta, state, log}

              {:ok, tok, state} ->
                {:error, {:expected, :"}", got: tok.kind}, state, log}

              {:eof, state} ->
                {:error, :unexpected_eof, state, log}

              {:error, diag, state} ->
                {:error, diag, state, log}
            end
          end
        else
          parse_tuple_element(acc, state, ctx, log)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_tuple_element(acc, state, ctx, log) do
    with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
      # Skip EOE after expression
      {state, _newlines} = skip_eoe_count_newlines(state, 0)

      case TokenAdapter.peek(state) do
        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state)
          # Skip EOE after comma
          {state, _newlines} = skip_eoe_count_newlines(state, 0)
          # Check for trailing comma or close
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :"}"} = close_tok, _} ->
              {:ok, _close, state} = TokenAdapter.next(state)
              close_meta = token_meta(close_tok.metadata)
              {:ok, Enum.reverse([expr | acc]), close_meta, state, log}

            _ ->
              parse_tuple_elements([expr | acc], state, ctx, log)
          end

        {:ok, %{kind: :"}"} = close_tok, _} ->
          {:ok, _close, state} = TokenAdapter.next(state)
          close_meta = token_meta(close_tok.metadata)
          {:ok, Enum.reverse([expr | acc]), close_meta, state, log}

        {:ok, tok, state} ->
          {:error, {:expected_comma_or, :"}", got: tok.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

end
