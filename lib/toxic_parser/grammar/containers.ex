defmodule ToxicParser.Grammar.Containers do
  @moduledoc """
  Container parsing for lists and tuples (Phase 6 scaffolding).
  """

  alias ToxicParser.{EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.{Bitstrings, Expressions, Keywords, Maps}

  # Check if an expression result is a keyword list (from quoted keyword parsing)
  defguardp is_keyword_list_result(arg)
            when is_list(arg) and length(arg) > 0

  # Stab pattern parsing uses min_bp=11 to stop before `->` (bp=10) but allow all other
  # operators including `when` (bp=50) and `<-`/`\\` (bp=40). The `when` at the TOP LEVEL
  # of patterns is handled specially after parsing (to extract guards).
  @stab_pattern_min_bp 11

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}
          | {:no_container, State.t()}

  @spec parse(State.t(), Pratt.context(), EventLog.t(), non_neg_integer()) :: result()
  def parse(%State{} = state, ctx, %EventLog{} = log, min_bp \\ 0) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"("}, _} ->
        parse_paren(state, ctx, log, min_bp)

      {:ok, %{kind: :"["}, _} ->
        parse_list(state, ctx, log, min_bp)

      {:ok, %{kind: :"{", value: _}, _} ->
        parse_tuple(state, ctx, log, min_bp)

      {:ok, %{kind: :%{}}, _} ->
        Maps.parse_map(state, ctx, log, min_bp)

      {:ok, %{kind: :%}, _} ->
        Maps.parse_map(state, ctx, log, min_bp)

      {:ok, %{kind: :"<<", value: _}, _} ->
        Bitstrings.parse(state, ctx, log, min_bp)

      {:eof, state} ->
        {:no_container, state}

      {:error, _diag, state} ->
        {:no_container, state}

      _ ->
        {:no_container, state}
    end
  end

  @doc """
  Parse a container without calling Pratt.led at the end.
  Used when the caller needs to control operator binding (e.g., stab patterns).
  """
  @spec parse_container_base(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_container_base(%State{} = state, ctx, %EventLog{} = log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"["}, _} ->
        parse_list_base(state, ctx, log)

      {:ok, %{kind: :"{", value: _}, _} ->
        parse_tuple_base(state, ctx, log)

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
  defp parse_paren(state, ctx, log, min_bp) do
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
        parse_paren_stab_or_empty(open_meta, state, ctx, log, min_bp)

      _ ->
        # Skip any remaining EOE tokens
        state = skip_eoe(state)
        parse_paren_content(open_meta, state, ctx, log, min_bp)
    end
  end

  # Parse content after open paren (no leading semicolon)
  defp parse_paren_content(open_meta, state, ctx, log, min_bp) do
    case TokenAdapter.peek(state) do
      # Empty parens: () -> {:__block__, [parens: ...], []}
      {:ok, %{kind: :")"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        parens_meta = [parens: open_meta ++ [closing: close_meta]]
        ast = {:__block__, parens_meta, []}
        # Continue with Pratt.led to handle trailing operators
        Pratt.led(ast, state, log, min_bp, ctx)

      # Check for stab operator at start: (-> expr)
      {:ok, %{kind: :stab_op}, _} ->
        parse_paren_stab(open_meta, state, ctx, log)

      # Check for empty inner parens followed by stab/when: (() -> expr) or (() when g -> expr)
      {:ok, %{kind: :"("}, _} ->
        try_parse_stab_parens_many(open_meta, state, ctx, log, min_bp)

      # Content that could be expression or stab pattern
      {:ok, _, _} ->
        # Try to parse stab first using checkpoint
        try_parse_stab_or_expr(open_meta, state, ctx, log, min_bp)

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # After leading semicolon: either close paren (empty) or stab content
  defp parse_paren_stab_or_empty(open_meta, state, ctx, log, min_bp) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :")"} = close_tok, _} ->
        # (;) -> empty stab with semicolon
        # Output: {:__block__, [closing: [...], line: L, column: C], []}
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        meta = [closing: close_meta] ++ open_meta
        ast = {:__block__, meta, []}
        # Continue with Pratt.led to handle trailing operators
        Pratt.led(ast, state, log, min_bp, ctx)

      _ ->
        # Parse stab expression(s) after leading semicolon
        parse_paren_stab(open_meta, state, ctx, log)
    end
  end

  # Parse stab expressions inside parens
  defp parse_paren_stab(_open_meta, state, ctx, log) do
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
  defp try_parse_stab_parens_many(open_meta, state, ctx, log, min_bp) do
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
            parse_empty_paren_stab(
              open_meta,
              inner_open_meta,
              inner_close_meta,
              inner_state,
              ctx,
              log
            )

          {:ok, %{kind: :when_op}, _} ->
            # (() when guard -> expr)
            parse_empty_paren_when_stab(
              open_meta,
              inner_open_meta,
              inner_close_meta,
              inner_state,
              ctx,
              log
            )

          _ ->
            # Not a stab - rewind and parse as expression
            state = TokenAdapter.rewind(checkpoint_state, ref)
            parse_expr_in_paren_with_meta(open_meta, state, ctx, log, min_bp)
        end

      # Inner parens has content - could be stab_parens_many
      {:ok, _, _} ->
        try_parse_stab_parens_many_content(
          open_meta,
          inner_open_meta,
          ref,
          checkpoint_state,
          inner_state,
          ctx,
          log,
          min_bp
        )

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
  defp try_parse_stab_parens_many_content(
         open_meta,
         inner_open_meta,
         ref,
         checkpoint_state,
         inner_state,
         ctx,
         log,
         min_bp
       ) do
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
                parse_stab_parens_many_stab(
                  open_meta,
                  inner_open_meta,
                  inner_close_meta,
                  args,
                  inner_state,
                  ctx,
                  log
                )

              {:ok, %{kind: :when_op}, _} ->
                parse_stab_parens_many_when(
                  open_meta,
                  inner_open_meta,
                  inner_close_meta,
                  args,
                  inner_state,
                  ctx,
                  log
                )

              _ ->
                # Not a stab - rewind and parse as expression
                state = TokenAdapter.rewind(checkpoint_state, ref)
                parse_expr_in_paren_with_meta(open_meta, state, ctx, log, min_bp)
            end

          _ ->
            # Not valid stab parens - rewind and parse as expression
            state = TokenAdapter.rewind(checkpoint_state, ref)
            parse_expr_in_paren_with_meta(open_meta, state, ctx, log, min_bp)
        end

      {:error, _, _, _} ->
        # Failed to parse stab args - rewind and parse as expression
        state = TokenAdapter.rewind(checkpoint_state, ref)
        parse_expr_in_paren_with_meta(open_meta, state, ctx, log, min_bp)
    end
  end

  # ((args) -> expr)
  defp parse_stab_parens_many_stab(
         _open_meta,
         inner_open_meta,
         inner_close_meta,
         args,
         state,
         ctx,
         log
       ) do
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
  defp parse_stab_parens_many_when(
         _open_meta,
         inner_open_meta,
         inner_close_meta,
         args,
         state,
         ctx,
         log
       ) do
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
  defp try_parse_stab_or_expr(open_meta, state, ctx, log, min_bp) do
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

    case try_parse_stab_clause(checkpoint_state, ctx, log) do
      {:ok, clause, new_state, log} ->
        # Successfully parsed a stab clause, continue with remaining clauses
        parse_remaining_stab_clauses([clause], new_state, ctx, log)

      {:not_stab, _state, _log} ->
        # Not a stab - rewind and parse as regular expression
        state = TokenAdapter.rewind(checkpoint_state, ref)
        parse_expr_in_paren_with_meta(open_meta, state, ctx, log, min_bp)

      {:error, reason, state, log} ->
        # Error during stab parsing - could try expression fallback
        {:error, reason, state, log}
    end
  end

  # Try to parse a stab clause, returning {:not_stab, ...} if it's not a stab
  # Default terminator is :) for paren stabs
  defp try_parse_stab_clause(state, ctx, log) do
    try_parse_stab_clause(state, ctx, log, :")")
  end

  @doc """
  Try to parse a stab clause with custom terminator.
  Returns {:ok, clause, state, log}, {:not_stab, state, log}, or {:error, ...}
  Exported for use by fn parsing.
  """
  def try_parse_stab_clause(state, ctx, log, terminator) do
    # Parse the pattern part first
    case parse_stab_patterns([], state, ctx, log) do
      # 5-tuple: patterns with parens metadata (from stab_parens_many)
      {:ok, patterns, state, log, {open_meta, close_meta}} ->
        parens_meta = [parens: open_meta ++ [closing: close_meta]]
        parse_stab_clause_after_patterns(patterns, parens_meta, state, ctx, log, terminator)

      # 4-tuple: patterns without parens metadata
      {:ok, patterns, state, log} ->
        parse_stab_clause_after_patterns(patterns, [], state, ctx, log, terminator)

      {:error, reason, state, log} ->
        {:error, reason, state, log}
    end
  end

  defp parse_stab_clause_after_patterns(patterns, parens_meta, state, ctx, log, terminator) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :stab_op} = stab_tok, _} ->
        # This is definitely a stab clause
        {:ok, _stab, state} = TokenAdapter.next(state)
        stab_base_meta = token_meta(stab_tok.metadata)
        # Skip EOE after stab and count newlines
        {state, newlines} = skip_eoe_count_newlines(state, 0)

        stab_meta =
          if newlines > 0, do: [newlines: newlines] ++ stab_base_meta, else: stab_base_meta

        with {:ok, body, state, log} <- parse_stab_body(state, ctx, log, terminator) do
          # Check if the last pattern is a top-level `when` that should be extracted as guard
          # This handles cases like `fn x, y when y > 0 -> x end` where:
          # - patterns = [x, {:when, _, [y, guard]}]
          # - should become [{:when, _, [x, y, guard]}]
          {final_patterns, final_stab_meta} =
            case extract_trailing_when_guard(patterns) do
              {:guard, new_patterns, guard, when_meta} ->
                # Apply parens meta before wrapping with guard
                {pats, stab_m} = apply_parens_meta(new_patterns, parens_meta, stab_meta)
                # Wrap with guard
                guard_patterns = [{:when, when_meta, pats ++ [guard]}]
                {guard_patterns, stab_m}

              :no_guard ->
                # No guard extraction needed
                apply_parens_meta(patterns, parens_meta, stab_meta)
            end

          clause = {:->, final_stab_meta, [final_patterns, body]}
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

              stab_meta =
                if newlines > 0, do: [newlines: newlines] ++ stab_base_meta, else: stab_base_meta

              with {:ok, body, state, log} <- parse_stab_body(state, ctx, log, terminator) do
                # Apply parens meta before wrapping with guard
                {final_patterns, final_stab_meta} =
                  apply_parens_meta(patterns, parens_meta, stab_meta)

                # Wrap patterns with guard in a when clause
                patterns_with_guard = unwrap_when(final_patterns, guard, when_meta)
                clause = {:->, final_stab_meta, [patterns_with_guard, body]}
                {:ok, clause, state, log}
              end

            {:ok, _token, state} ->
              # No stab after when+guard - this is just `when` as a binary operator
              # Return :not_stab so we can try parsing as a regular expression
              {:not_stab, state, log}

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

  # Apply parens metadata according to Elixir rules:
  # - 0 patterns: parens go on stab arrow
  # - 1 pattern: parens go on the single pattern
  # - 2+ patterns: parens go on stab arrow
  defp apply_parens_meta([], parens_meta, stab_meta) do
    # 0 patterns - parens on stab
    {[], parens_meta ++ stab_meta}
  end

  defp apply_parens_meta([single_pattern], parens_meta, stab_meta) when parens_meta != [] do
    # 1 pattern - parens go on the pattern if it's a 3-tuple with metadata
    # For keyword lists (plain lists), parens go on the stab arrow
    case single_pattern do
      {_name, meta, _args} when is_list(meta) ->
        pattern_with_parens = add_parens_to_pattern(single_pattern, parens_meta)
        {[pattern_with_parens], stab_meta}

      _ ->
        # Pattern can't hold metadata (e.g., keyword list) - put parens on stab
        {[single_pattern], parens_meta ++ stab_meta}
    end
  end

  defp apply_parens_meta(patterns, parens_meta, stab_meta) do
    # 2+ patterns or no parens_meta - parens on stab
    {patterns, parens_meta ++ stab_meta}
  end

  # Add parens metadata to a pattern
  defp add_parens_to_pattern({name, meta, args}, parens_meta) when is_list(meta) do
    {name, parens_meta ++ meta, args}
  end

  defp add_parens_to_pattern(other, _parens_meta), do: other

  # Unwrap when: combine patterns with guard
  defp unwrap_when(patterns, guard, when_meta) do
    [{:when, when_meta, patterns ++ [guard]}]
  end

  # Extract guard from trailing when in pattern list
  # Handles cases like `fn x, y when guard -> body` where patterns = [x, {:when, _, [y, guard]}]
  # Returns {:guard, [x, y], guard, when_meta} in that case
  # Also handles single pattern case: `fn a when b -> c` where patterns = [{:when, _, [a, b]}]
  # Returns {:guard, [a], b, when_meta}
  # Returns :no_guard if no extraction needed (e.g., `fn a when a <- b -> a`)
  defp extract_trailing_when_guard([]) do
    :no_guard
  end

  defp extract_trailing_when_guard(patterns) do
    {init, [last]} = Enum.split(patterns, -1)

    case last do
      {:when, when_meta, when_args} when is_list(when_args) and length(when_args) >= 2 ->
        # Last pattern is a `when` expression - extract it
        # when_args = [pattern1, pattern2, ..., guard]
        {when_patterns, [guard]} = Enum.split(when_args, -1)
        new_patterns = init ++ when_patterns
        {:guard, new_patterns, guard, when_meta}

      _ ->
        # Last pattern is not a bare `when` - no extraction
        :no_guard
    end
  end

  # Parse stab patterns: either single expression or comma-separated matched expressions
  # Grammar: stab_expr -> stab_op_eol_and_expr (no patterns, just -> body)
  #          stab_parens_many -> open_paren call_args_parens close_paren
  defp parse_stab_patterns(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      # If we immediately see stab_op, there are no patterns
      {:ok, %{kind: :stab_op}, _} ->
        {:ok, [], state, log}

      # stab_parens_many: fn (args) -> body end
      {:ok, %{kind: :"("} = open_tok, _} ->
        parse_stab_parens_many(open_tok, state, ctx, log)

      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          # call_args_no_parens_kw: (x: 1 -> body)
          # Use min_bp > stab_op (10) to stop keyword values before ->
          # The kw_list is already [x: 1], and we need to wrap it in a list to get [[x: 1]]
          with {:ok, kw_list, state, log} <-
                 Keywords.parse_kw_call_with_min_bp(state, ctx, log, 11) do
            {:ok, [kw_list], state, log}
          end
        else
          parse_stab_pattern_exprs(acc, state, ctx, log)
        end

      _ ->
        parse_stab_pattern_exprs(acc, state, ctx, log)
    end
  end

  # Parse stab_parens_many: fn (args) -> body end
  # Grammar: stab_parens_many -> open_paren call_args_parens close_paren : build_stab_parens_many
  defp parse_stab_parens_many(open_tok, state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)
    open_meta = token_meta(open_tok.metadata)

    # Skip EOE inside parens
    state = skip_eoe(state)

    case TokenAdapter.peek(state) do
      # Empty parens: fn () -> body end
      {:ok, %{kind: :")"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        # Return empty patterns with parens metadata attached to the clause later
        # Store parens meta for use in try_parse_stab_clause
        {:ok, [], state, log, {open_meta, close_meta}}

      # Content inside parens - parse as call_args_parens
      {:ok, _, _} ->
        with {:ok, args, state, log} <- parse_call_args_parens(state, ctx, log) do
          state = skip_eoe(state)

          case TokenAdapter.next(state) do
            {:ok, %{kind: :")"} = close_tok, state} ->
              close_meta = token_meta(close_tok.metadata)
              {:ok, args, state, log, {open_meta, close_meta}}

            {:ok, token, state} ->
              {:error, {:expected, :")", got: token.kind}, state, log}

            {:eof, state} ->
              {:error, :unexpected_eof, state, log}

            {:error, diag, state} ->
              {:error, diag, state, log}
          end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse call_args_parens: comma-separated expressions, optionally followed by keywords
  defp parse_call_args_parens(state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          # Just keywords: fn (x: 1) -> body end
          with {:ok, kw_list, state, log} <- Keywords.parse_kw_call(state, ctx, log) do
            {:ok, [kw_list], state, log}
          end
        else
          parse_call_args_parens_exprs([], state, ctx, log)
        end

      _ ->
        parse_call_args_parens_exprs([], state, ctx, log)
    end
  end

  defp parse_call_args_parens_exprs(acc, state, ctx, log) do
    # Parse matched expression
    with {:ok, expr, state, log} <- Expressions.expr(state, :matched, log) do
      state = skip_eoe(state)

      case TokenAdapter.peek(state) do
        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state)
          state = skip_eoe(state)
          # Check for keyword after comma
          case TokenAdapter.peek(state) do
            {:ok, tok, _} ->
              if Keywords.starts_kw?(tok) do
                # Expressions followed by keywords
                with {:ok, kw_list, state, log} <- Keywords.parse_kw_call(state, ctx, log) do
                  {:ok, Enum.reverse([expr | acc]) ++ [kw_list], state, log}
                end
              else
                parse_call_args_parens_exprs([expr | acc], state, ctx, log)
              end

            _ ->
              parse_call_args_parens_exprs([expr | acc], state, ctx, log)
          end

        _ ->
          {:ok, Enum.reverse([expr | acc]), state, log}
      end
    end
  end

  defp parse_stab_pattern_exprs(acc, state, _ctx, log) do
    # Parse expression as matched_expr, stopping before -> but allowing when and <-
    # This implements call_args_no_parens_expr -> matched_expr
    with {:ok, expr, state, log} <- parse_stab_pattern_expr(state, log) do
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
                with {:ok, kw_list, state, log} <-
                       Keywords.parse_kw_call_with_min_bp(state, :matched, log, 11) do
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

        _ ->
          {:ok, Enum.reverse([expr | acc]), state, log}
      end
    end
  end

  # Parse a single stab pattern expression.
  # Handles containers ({}, [], <<>>) specially, then delegates to Pratt.
  # Uses @stab_pattern_min_bp (11) to stop before -> only, allowing when and <-/\\.
  defp parse_stab_pattern_expr(state, log) do
    case TokenAdapter.peek(state) do
      # Container tokens - parse container base then continue with led at min_bp
      # Use parse_container_base which doesn't call Pratt.led internally
      {:ok, %{kind: kind}, _} when kind in [:"{", :"[", :"<<"] ->
        with {:ok, ast, state, log} <- parse_container_base(state, :matched, log) do
          # Continue with led to handle trailing operators
          Pratt.led(ast, state, log, @stab_pattern_min_bp, :matched)
        end

      # Map literal %{} or struct %Name{}
      {:ok, %{kind: kind}, _} when kind in [:%{}, :%] ->
        Maps.parse_map(state, :matched, log, @stab_pattern_min_bp)

      # Other tokens - use Pratt parser with min_bp to stop before ->
      _ ->
        Pratt.parse_with_min_bp(state, :matched, log, @stab_pattern_min_bp)
    end
  end

  # Parse the body of a stab clause (expression or nil if empty)
  # Default terminator is :) for paren stabs
  defp parse_stab_body(state, ctx, log) do
    parse_stab_body(state, ctx, log, :")")
  end

  @doc """
  Parse stab clause body with custom terminator.
  Exported for use by fn parsing.
  """
  def parse_stab_body(state, ctx, log, terminator) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: ^terminator}, _} ->
        # Empty body at terminator - return nil
        {:ok, nil, state, log}

      {:ok, %{kind: :eoe}, _} ->
        # Empty body at EOE - return nil
        {:ok, nil, state, log}

      # For :end terminator, also stop at block identifiers (after, else, catch, rescue)
      {:ok, %{kind: :block_identifier}, _} when terminator == :end ->
        {:ok, nil, state, log}

      {:ok, _, _} ->
        with {:ok, body, state, log} <- Expressions.expr(state, ctx, log) do
          # Annotate with end_of_expression if followed by EOE
          body = maybe_annotate_stab_body_eoe(body, state)
          {:ok, body, state, log}
        end

      {:eof, state} ->
        {:ok, nil, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Annotate stab body with end_of_expression metadata if followed by EOE
  defp maybe_annotate_stab_body_eoe(body, state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe} = eoe_tok, _} ->
        eoe_meta = build_eoe_meta(eoe_tok)
        annotate_body_eoe(body, eoe_meta)

      _ ->
        body
    end
  end

  # Build end_of_expression metadata from EOE token
  defp build_eoe_meta(%{kind: :eoe, value: %{newlines: newlines}, metadata: meta})
       when is_integer(newlines) do
    case meta do
      %{range: %{start: %{line: line, column: column}}} ->
        [newlines: newlines, line: line, column: column]

      _ ->
        []
    end
  end

  defp build_eoe_meta(%{kind: :eoe, metadata: meta}) do
    case meta do
      %{range: %{start: %{line: line, column: column}}} ->
        [line: line, column: column]

      _ ->
        []
    end
  end

  # Annotate an AST node with end_of_expression metadata
  defp annotate_body_eoe({left, meta, right}, eoe_meta) when is_list(meta) do
    {left, [{:end_of_expression, eoe_meta} | meta], right}
  end

  defp annotate_body_eoe(body, _eoe_meta), do: body

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
  # Terminates on :) for paren stab
  defp parse_stab_eoe(acc, state, ctx, log) do
    parse_stab_eoe_until(acc, state, ctx, log, :")")
  end

  @doc """
  Parse stab_eoe until a given terminator kind.
  Exported for use by fn parsing in Blocks module.
  For fn, stab is required (no fallback to plain expression).
  """
  @spec parse_stab_eoe_until(list(), State.t(), Pratt.context(), EventLog.t(), atom()) ::
          {:ok, [Macro.t()], State.t(), EventLog.t()} | {:error, term(), State.t(), EventLog.t()}
  def parse_stab_eoe_until(acc, state, ctx, log, terminator) do
    case try_parse_stab_clause(state, ctx, log, terminator) do
      {:ok, clause, state, log} ->
        state = skip_eoe(state)

        case TokenAdapter.peek(state) do
          {:ok, %{kind: ^terminator}, _} ->
            {:ok, Enum.reverse([clause | acc]), state, log}

          {:ok, _, _} ->
            parse_stab_eoe_until([clause | acc], state, ctx, log, terminator)

          {:eof, state} ->
            {:error, :unexpected_eof, state, log}

          {:error, diag, state} ->
            {:error, diag, state, log}
        end

      {:not_stab, state, log} ->
        # For fn (terminator = :end), stab is required
        # For parens (terminator = :)), expression fallback is allowed
        if terminator == :")" do
          # Could be just an expression (stab_expr -> expr)
          with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
            {:ok, Enum.reverse([expr | acc]), state, log}
          end
        else
          # fn requires stab - but check for empty clauses list
          if acc == [] do
            # First "clause" isn't a stab - error
            {:error, {:expected, :stab_op, got: :expression}, state, log}
          else
            # Already have clauses, this might be end of input
            {:ok, Enum.reverse(acc), state, log}
          end
        end

      {:error, reason, state, log} ->
        {:error, reason, state, log}
    end
  end

  defp parse_expr_in_paren_with_meta(open_meta, state, ctx, log, min_bp) do
    parse_expr_in_paren_impl(open_meta, [], state, ctx, log, min_bp)
  end

  # Parse expressions in parens, handling semicolons as separators
  # Grammar: paren_expr -> expr (';' expr)* ')'
  # Multiple expressions become a __block__
  defp parse_expr_in_paren_impl(open_meta, acc, state, ctx, log, min_bp) do
    with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
      # Check for EOE (semicolon) or close paren
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :eoe}, _} ->
          # Consume EOE
          {:ok, _eoe, state} = TokenAdapter.next(state)
          state = skip_eoe(state)
          # Check if there's more content or close paren
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :")"} = close_tok, _} ->
              {:ok, _close, state} = TokenAdapter.next(state)
              close_meta = token_meta(close_tok.metadata)
              finish_paren_exprs([expr | acc], open_meta, close_meta, state, log, min_bp, ctx)

            {:ok, _, _} ->
              # More expressions after semicolon
              parse_expr_in_paren_impl(open_meta, [expr | acc], state, ctx, log, min_bp)

            {:eof, state} ->
              {:error, :unexpected_eof, state, log}

            {:error, diag, state} ->
              {:error, diag, state, log}
          end

        {:ok, %{kind: :")"} = close_tok, _} ->
          {:ok, _close, state} = TokenAdapter.next(state)
          close_meta = token_meta(close_tok.metadata)
          finish_paren_exprs([expr | acc], open_meta, close_meta, state, log, min_bp, ctx)

        {:ok, token, state} ->
          {:error, {:expected, :")", got: token.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  # Finish parsing paren expressions - build appropriate AST
  defp finish_paren_exprs([single], open_meta, close_meta, state, log, min_bp, ctx) do
    # Special case: Single-argument unary expressions with `not` or `!` operators
    # need to be wrapped in __block__ to match Elixir's behavior.
    # This is from elixir_parser.yrl:
    #   build_paren_stab(_Before, [{Op, _, [_]}]=Exprs, _After) when ?rearrange_uop(Op) ->
    #     {'__block__', [], Exprs};
    # where ?rearrange_uop(Op) is (Op == 'not' orelse Op == '!')
    expr =
      case single do
        {op, _meta, [_arg]} when op in [:not, :!] ->
          {:__block__, [], [single]}

        _ ->
          # Regular single expression - add parens metadata to 3-tuple AST nodes
          add_parens_meta(single, open_meta, close_meta)
      end

    # Continue with Pratt.led to handle trailing operators like *, /, etc.
    Pratt.led(expr, state, log, min_bp, ctx)
  end

  defp finish_paren_exprs(exprs, open_meta, close_meta, state, log, min_bp, ctx) do
    # Multiple expressions - wrap in __block__
    ast = {:__block__, [closing: close_meta] ++ open_meta, Enum.reverse(exprs)}
    Pratt.led(ast, state, log, min_bp, ctx)
  end

  # Add parens: metadata to 3-tuple AST nodes
  defp add_parens_meta({name, meta, args}, open_meta, close_meta) when is_list(meta) do
    parens_meta = build_parens_meta(open_meta, close_meta)
    {name, [parens: parens_meta] ++ meta, args}
  end

  defp add_parens_meta(literal, _open_meta, _close_meta), do: literal

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

  defp token_meta(%{range: %{start: %{line: line, column: column}}}),
    do: [line: line, column: column]

  defp token_meta(_), do: []

  defp parse_list(state, ctx, log, min_bp) do
    with {:ok, ast, state, log} <- parse_list_base(state, ctx, log) do
      # Continue with Pratt's led() to handle trailing operators
      Pratt.led(ast, state, log, min_bp, ctx)
    end
  end

  # Parse list without calling Pratt.led - used when caller controls led binding
  defp parse_list_base(state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    # Skip leading EOE
    {state, _newlines} = skip_eoe_count_newlines(state, 0)

    case TokenAdapter.peek(state) do
      # Empty list
      {:ok, %{kind: :"]"}, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        {:ok, [], state, log}

      {:ok, _, _} ->
        parse_list_elements_base([], state, ctx, log)

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse list elements without calling Pratt.led - used when caller controls led binding
  defp parse_list_elements_base(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
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
          parse_list_element_base(acc, state, ctx, log)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_list_element_base(acc, state, ctx, log) do
    with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
      {state, _newlines} = skip_eoe_count_newlines(state, 0)

      # Check if expr is a keyword list (from quoted keyword parsing like "": 1)
      # If so, we should merge it rather than wrap it as a single element
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state)
          {state, _newlines} = skip_eoe_count_newlines(state, 0)

          case TokenAdapter.peek(state) do
            {:ok, %{kind: :"]"}, _} ->
              {:ok, _close, state} = TokenAdapter.next(state)
              {:ok, merge_keyword_expr(acc, expr), state, log}

            _ ->
              parse_list_elements_base(prepend_expr(acc, expr), state, ctx, log)
          end

        {:ok, %{kind: :"]"}, _} ->
          {:ok, _close, state} = TokenAdapter.next(state)
          {:ok, merge_keyword_expr(acc, expr), state, log}

        {:ok, tok, state} ->
          {:error, {:expected_comma_or, :"]", got: tok.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  # Check if expr is a keyword list (list of {atom, value} tuples)
  # If so, merge it into the result; otherwise prepend as single element
  defp merge_keyword_expr(acc, expr) when is_list(expr) do
    if is_keyword_list?(expr) do
      Enum.reverse(acc) ++ expr
    else
      Enum.reverse([expr | acc])
    end
  end

  defp merge_keyword_expr(acc, expr) do
    Enum.reverse([expr | acc])
  end

  # Prepend expr to acc, handling keyword lists specially
  defp prepend_expr(acc, expr) when is_list(expr) do
    if is_keyword_list?(expr) do
      # Reverse keyword list and prepend each element to acc
      Enum.reduce(Enum.reverse(expr), acc, fn elem, acc -> [elem | acc] end)
    else
      [expr | acc]
    end
  end

  defp prepend_expr(acc, expr), do: [expr | acc]

  # Check if a list is a keyword-like list (list of {key, value} tuples)
  # The key can be an atom (standard keyword) or an AST (interpolated keyword)
  defp is_keyword_list?([{_key, _value} | rest]), do: is_keyword_list?(rest)
  defp is_keyword_list?([]), do: true
  defp is_keyword_list?(_), do: false

  defp parse_tuple(state, ctx, log, min_bp) do
    with {:ok, ast, state, log} <- parse_tuple_base(state, ctx, log) do
      # Continue with Pratt's led() to handle trailing operators like <-, =, etc.
      Pratt.led(ast, state, log, min_bp, ctx)
    end
  end

  # Parse tuple without calling Pratt.led - used when caller controls led binding
  defp parse_tuple_base(state, ctx, log) do
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
          # Check for trailing comma, close, or keyword continuation
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :"}"} = close_tok, _} ->
              {:ok, _close, state} = TokenAdapter.next(state)
              close_meta = token_meta(close_tok.metadata)
              {:ok, Enum.reverse([expr | acc]), close_meta, state, log}

            {:ok, kw_tok, _} ->
              # Check if expr was a keyword list and next is also a keyword
              cond do
                is_keyword_list_result(expr) and Keywords.starts_kw?(kw_tok) ->
                  # Merge with remaining keywords
                  with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
                    {state, _newlines} = skip_eoe_count_newlines(state, 0)

                    case TokenAdapter.peek(state) do
                      {:ok, %{kind: :"}"} = close_tok, _} ->
                        {:ok, _close, state} = TokenAdapter.next(state)
                        close_meta = token_meta(close_tok.metadata)
                        merged_kw = expr ++ kw_list
                        {:ok, Enum.reverse([merged_kw | acc]), close_meta, state, log}

                      {:ok, %{kind: :","}, _} ->
                        # Trailing comma after merged keywords
                        {:ok, _comma, state} = TokenAdapter.next(state)
                        {state, _newlines} = skip_eoe_count_newlines(state, 0)

                        case TokenAdapter.peek(state) do
                          {:ok, %{kind: :"}"} = close_tok, _} ->
                            {:ok, _close, state} = TokenAdapter.next(state)
                            close_meta = token_meta(close_tok.metadata)
                            merged_kw = expr ++ kw_list
                            {:ok, Enum.reverse([merged_kw | acc]), close_meta, state, log}

                          _ ->
                            {:error, {:expected, :"}"}, state, log}
                        end

                      {:ok, tok, state} ->
                        {:error, {:expected_comma_or, :"}", got: tok.kind}, state, log}

                      {:eof, state} ->
                        {:error, :unexpected_eof, state, log}

                      {:error, diag, state} ->
                        {:error, diag, state, log}
                    end
                  end

                is_keyword_list_result(expr) and
                    kw_tok.kind in [:list_string_start, :bin_string_start] ->
                  # Another quoted keyword - parse and merge
                  parse_tuple_quoted_kw_continuation(expr, acc, state, ctx, log)

                true ->
                  parse_tuple_elements([expr | acc], state, ctx, log)
              end

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

  # Parse continuation of keyword list in tuple when we encounter another quoted keyword
  defp parse_tuple_quoted_kw_continuation(acc_kw, tuple_acc, state, ctx, log) do
    case Expressions.expr(state, ctx, log) do
      {:ok, expr, state, log} when is_keyword_list_result(expr) ->
        {state, _newlines} = skip_eoe_count_newlines(state, 0)

        case TokenAdapter.peek(state) do
          {:ok, %{kind: :","}, _} ->
            {:ok, _comma, state} = TokenAdapter.next(state)
            {state, _newlines} = skip_eoe_count_newlines(state, 0)

            case TokenAdapter.peek(state) do
              {:ok, %{kind: :"}"} = close_tok, _} ->
                {:ok, _close, state} = TokenAdapter.next(state)
                close_meta = token_meta(close_tok.metadata)
                merged_kw = acc_kw ++ expr
                {:ok, Enum.reverse([merged_kw | tuple_acc]), close_meta, state, log}

              {:ok, kw_tok, _} ->
                cond do
                  Keywords.starts_kw?(kw_tok) ->
                    with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
                      {state, _newlines} = skip_eoe_count_newlines(state, 0)

                      case TokenAdapter.peek(state) do
                        {:ok, %{kind: :"}"} = close_tok, _} ->
                          {:ok, _close, state} = TokenAdapter.next(state)
                          close_meta = token_meta(close_tok.metadata)
                          merged_kw = acc_kw ++ expr ++ kw_list
                          {:ok, Enum.reverse([merged_kw | tuple_acc]), close_meta, state, log}

                        {:ok, tok, state} ->
                          {:error, {:expected_comma_or, :"}", got: tok.kind}, state, log}

                        {:eof, state} ->
                          {:error, :unexpected_eof, state, log}

                        {:error, diag, state} ->
                          {:error, diag, state, log}
                      end
                    end

                  kw_tok.kind in [:list_string_start, :bin_string_start] ->
                    parse_tuple_quoted_kw_continuation(acc_kw ++ expr, tuple_acc, state, ctx, log)

                  true ->
                    {:error, {:expected, :keyword}, state, log}
                end

              _ ->
                {:error, {:expected, :keyword}, state, log}
            end

          {:ok, %{kind: :"}"} = close_tok, _} ->
            {:ok, _close, state} = TokenAdapter.next(state)
            close_meta = token_meta(close_tok.metadata)
            merged_kw = acc_kw ++ expr
            {:ok, Enum.reverse([merged_kw | tuple_acc]), close_meta, state, log}

          {:ok, tok, state} ->
            {:error, {:expected_comma_or, :"}", got: tok.kind}, state, log}

          {:eof, state} ->
            {:error, :unexpected_eof, state, log}

          {:error, diag, state} ->
            {:error, diag, state, log}
        end

      {:ok, _expr, state, log} ->
        {:error, {:expected, :keyword}, state, log}

      {:error, _, _, _} = error ->
        error
    end
  end
end
