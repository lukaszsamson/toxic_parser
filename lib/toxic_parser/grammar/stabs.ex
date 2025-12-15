defmodule ToxicParser.Grammar.Stabs do
  @moduledoc """
  Stab parsing extracted from Containers to keep paren/list/tuple handling focused.
  """

  alias ToxicParser.{Builder, EventLog, Pratt, Precedence, State, TokenAdapter}
  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.{Containers, EOE, Expressions, Keywords, Maps}

  # Stab pattern parsing uses min_bp one higher than stab_op (bp=10) to stop before `->`
  # but allow all other operators including `when` (bp=50) and `<-`/`\` (bp=40).
  # The `when` at the TOP LEVEL of patterns is handled specially after parsing (to extract guards).
  @stab_pattern_min_bp Precedence.stab_op_bp() + 1

  # After leading semicolon: either close paren (empty) or stab content
  def parse_paren_stab_or_empty(open_meta, state, ctx, log, min_bp) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :")"} = close_tok, _} ->
        # (;) -> empty stab with semicolon
        # Output: {:__block__, [closing: [...], line: L, column: C], []}
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        meta = Meta.closing_meta(open_meta, close_meta)
        ast = {:__block__, meta, []}
        # Continue with Pratt.led to handle trailing operators
        Pratt.led(ast, state, log, min_bp, ctx)

      _ ->
        # Parse stab expression(s) after leading semicolon
        parse_paren_stab(open_meta, state, ctx, log, min_bp)
    end
  end

  # Parse stab expressions inside parens
  # min_bp controls whether to continue with led() for trailing operators
  def parse_paren_stab(_open_meta, state, ctx, log, min_bp) do
    with {:ok, clauses, state, log} <- parse_stab_eoe([], state, ctx, log) do
      state = EOE.skip(state)

      case TokenAdapter.next(state) do
        {:ok, %{kind: :")"}, state} ->
          # Continue with Pratt.led to handle trailing operators like |
          Pratt.led(clauses, state, log, min_bp, ctx)

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
  def try_parse_stab_parens_many(open_meta, state, ctx, log, min_bp, fallback_fun) do
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

    # Consume the inner open paren
    {:ok, inner_open_tok, inner_state} = TokenAdapter.next(checkpoint_state)
    inner_open_meta = token_meta(inner_open_tok.metadata)

    # Skip EOE inside inner parens
    inner_state = EOE.skip(inner_state)

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
            fallback_fun.(state, log)
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
          min_bp,
          fallback_fun
        )

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Try parsing as stab or fallback to expression
  def try_parse_stab_or_expr(_open_meta, state, ctx, log, min_bp, fallback_fun) do
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

    case try_parse_stab_clause(checkpoint_state, ctx, log) do
      {:ok, clause, new_state, log} ->
        # Successfully parsed a stab clause, continue with remaining clauses
        # Pass min_bp to continue with led() for trailing operators
        parse_remaining_stab_clauses([clause], new_state, ctx, log, min_bp)

      {:not_stab, _state, _log} ->
        # Not a stab - rewind and parse as regular expression
        state = TokenAdapter.rewind(checkpoint_state, ref)
        fallback_fun.(state, log)

      {:error, reason, state, log} ->
        # Error during stab parsing - could try expression fallback
        {:error, reason, state, log}
    end
  end

  # Parse (() -> expr) - empty paren stab
  defp parse_empty_paren_stab(_open_meta, inner_open_meta, inner_close_meta, state, ctx, log) do
    {:ok, stab_tok, state} = TokenAdapter.next(state)
    stab_base_meta = token_meta(stab_tok.metadata)

    # Skip EOE after stab and count newlines
    {state, newlines} = EOE.skip_count_newlines(state, 0)
    newlines_meta = Meta.newlines_meta(newlines)

    # Parse body (or empty if just ->)
    with {:ok, body, state, log} <- parse_stab_body(state, ctx, log) do
      # Build stab clause with empty patterns and parens metadata
      parens_meta =
        [parens: Meta.closing_meta(inner_open_meta, inner_close_meta, 0, [], base_first: true)]

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
    state = EOE.skip(state)

    # Parse guard expression with min_bp > stab_op (10) to stop before ->
    # Use :unmatched context so do-blocks can attach to expressions like `if a do :ok end`
    with {:ok, guard, state, log} <-
           Pratt.parse_with_min_bp(state, :unmatched, log, Precedence.stab_op_bp() + 1) do
      # Expect stab
      case TokenAdapter.next(state) do
        {:ok, %{kind: :stab_op} = stab_tok, state} ->
          stab_base_meta = token_meta(stab_tok.metadata)
          # Skip EOE after stab and count newlines
          {state, newlines} = EOE.skip_count_newlines(state, 0)
          newlines_meta = Meta.newlines_meta(newlines)

          with {:ok, body, state, log} <- parse_stab_body(state, ctx, log) do
            # Build guarded stab clause
            parens_meta =
              [
                parens:
                  Meta.closing_meta(inner_open_meta, inner_close_meta, 0, [], base_first: true)
              ]

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

  defp try_parse_stab_parens_many_content(
         open_meta,
         inner_open_meta,
         ref,
         checkpoint_state,
         inner_state,
         ctx,
         log,
         _min_bp,
         fallback_fun
       ) do
    # Try to parse the content as stab pattern arguments
    case parse_stab_parens_args(inner_state, ctx, log) do
      {:ok, args, inner_state, log} ->
        inner_state = EOE.skip(inner_state)

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
                fallback_fun.(state, log)
            end

          _ ->
            # Not valid stab parens - rewind and parse as expression
            state = TokenAdapter.rewind(checkpoint_state, ref)
            fallback_fun.(state, log)
        end

      {:error, _, _, _} ->
        # Failed to parse stab args - rewind and parse as expression
        state = TokenAdapter.rewind(checkpoint_state, ref)
        fallback_fun.(state, log)
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
    {state, newlines} = EOE.skip_count_newlines(state, 0)
    newlines_meta = Meta.newlines_meta(newlines)

    with {:ok, body, state, log} <- parse_stab_body(state, ctx, log) do
      parens_meta =
        [parens: Meta.closing_meta(inner_open_meta, inner_close_meta, 0, [], base_first: true)]

      stab_meta = newlines_meta ++ stab_base_meta
      # Apply parens meta and unwrap splice for stab patterns
      unwrapped = unwrap_splice(args)
      {final_args, final_stab_meta} = apply_parens_meta(unwrapped, parens_meta, stab_meta)
      clause = {:->, final_stab_meta, [final_args, body]}
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
    state = EOE.skip(state)

    # Parse guard expression with min_bp > stab_op (10) to stop before ->
    # Use :unmatched context so do-blocks can attach to expressions like `if a do :ok end`
    with {:ok, guard, state, log} <-
           Pratt.parse_with_min_bp(state, :unmatched, log, Precedence.stab_op_bp() + 1) do
      case TokenAdapter.next(state) do
        {:ok, %{kind: :stab_op} = stab_tok, state} ->
          stab_base_meta = token_meta(stab_tok.metadata)
          # Skip EOE after stab and count newlines
          {state, newlines} = EOE.skip_count_newlines(state, 0)
          newlines_meta = Meta.newlines_meta(newlines)

          with {:ok, body, state, log} <- parse_stab_body(state, ctx, log) do
            parens_meta =
              [
                parens:
                  Meta.closing_meta(inner_open_meta, inner_close_meta, 0, [], base_first: true)
              ]

            stab_meta = newlines_meta ++ stab_base_meta
            # Apply parens meta and unwrap splice for stab patterns
            unwrapped = unwrap_splice(args)
            {final_args, final_stab_meta} = apply_parens_meta(unwrapped, parens_meta, stab_meta)
            guard_ast = {:when, when_meta, final_args ++ [guard]}
            clause = {:->, final_stab_meta, [[guard_ast], body]}
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
        cond do
          Keywords.starts_kw?(tok) ->
            # call_args_no_parens_kw
            with {:ok, kw_list, state, log} <- Keywords.parse_kw_no_parens_call(state, ctx, log) do
              {:ok, [kw_list], state, log}
            end

          tok.kind in [:list_string_start, :bin_string_start] ->
            # Potentially a quoted keyword - parse as expression and check
            case parse_stab_parens_exprs([], state, ctx, log) do
              {:ok, [kw_list], state, log} when is_list(kw_list) and length(kw_list) > 0 ->
                {:ok, [kw_list], state, log}

              {:ok, exprs, state, log} ->
                {:ok, exprs, state, log}

              {:error, _, _, _} = error ->
                error
            end

          true ->
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
    case parse_stab_parens_single_expr(state, log) do
      {:ok, expr, state, log} ->
        state_after_eoe = EOE.skip(state)

        case TokenAdapter.peek(state_after_eoe) do
          {:ok, %{kind: :","}, _} ->
            {:ok, _comma, state} = TokenAdapter.next(state_after_eoe)
            state = EOE.skip(state)
            # Check for keyword after comma
            case TokenAdapter.peek(state) do
              {:ok, tok, _} ->
                cond do
                  Keywords.starts_kw?(tok) ->
                    # call_args_no_parens_many: exprs followed by kw
                    with {:ok, kw_list, state, log} <-
                           Keywords.parse_kw_no_parens_call(state, :matched, log) do
                      # If expr was a keyword list from quoted key, merge them
                      if is_keyword_list(expr) do
                        {:ok, Enum.reverse(acc) ++ [expr ++ kw_list], state, log}
                      else
                        {:ok, Enum.reverse(acc) ++ [expr, kw_list], state, log}
                      end
                    end

                  is_keyword_list(expr) and tok.kind in [:list_string_start, :bin_string_start] ->
                    # Another quoted keyword after a keyword list - continue merging
                    parse_stab_parens_exprs_quoted_kw_continuation(expr, acc, state, log)

                  true ->
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

      {:error, reason, state, log} ->
        {:error, reason, state, log}
    end
  end

  # Parse a single expression for stab parens, handling quoted keywords
  defp parse_stab_parens_single_expr(state, log) do
    case Pratt.parse_with_min_bp(state, :matched, log, Precedence.when_op_bp() + 1) do
      {:ok, ast, state, log} ->
        {:ok, ast, state, log}

      # Handle quoted keyword key - parse value and return as keyword pair
      {:keyword_key, key_atom, state, log} ->
        state = EOE.skip(state)

        with {:ok, value_ast, state, log} <-
               Pratt.parse_with_min_bp(state, :matched, log, Precedence.when_op_bp() + 1) do
          {:ok, [{key_atom, value_ast}], state, log}
        end

      {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, log} ->
        state = EOE.skip(state)

        with {:ok, value_ast, state, log} <-
               Pratt.parse_with_min_bp(state, :matched, log, Precedence.when_op_bp() + 1) do
          key_ast = Expressions.build_interpolated_keyword_key(parts, kind, start_meta, delimiter)
          {:ok, [{key_ast, value_ast}], state, log}
        end

      {:error, reason, state, log} ->
        {:error, reason, state, log}
    end
  end

  # Parse continuation of keyword list in stab parens context
  defp parse_stab_parens_exprs_quoted_kw_continuation(acc_kw, acc, state, log) do
    case parse_stab_parens_single_expr(state, log) do
      {:ok, expr, state, log} when is_list(expr) and length(expr) > 0 ->
        state_after_eoe = EOE.skip(state)

        case TokenAdapter.peek(state_after_eoe) do
          {:ok, %{kind: :","}, _} ->
            {:ok, _comma, state} = TokenAdapter.next(state_after_eoe)
            state = EOE.skip(state)

            case TokenAdapter.peek(state) do
              {:ok, %{kind: :")"}, _} ->
                {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state, log}

              {:ok, tok, _} ->
                cond do
                  Keywords.starts_kw?(tok) ->
                    with {:ok, more_kw, state, log} <-
                           Keywords.parse_kw_no_parens_call(state, :matched, log) do
                      {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr ++ more_kw], state, log}
                    end

                  tok.kind in [:list_string_start, :bin_string_start] ->
                    parse_stab_parens_exprs_quoted_kw_continuation(
                      acc_kw ++ expr,
                      acc,
                      state,
                      log
                    )

                  true ->
                    {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state, log}
                end

              _ ->
                {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state, log}
            end

          {:ok, %{kind: :")"}, _} ->
            {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state_after_eoe, log}

          _ ->
            {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state, log}
        end

      {:ok, _expr, state, log} ->
        {:error, {:expected, :keyword}, state, log}

      {:error, _, _, _} = error ->
        error
    end
  end

  # Try to parse a stab clause, returning {:not_stab, ...} if it's not a stab
  # Default terminator is :) for paren stabs
  def try_parse_stab_clause(state, ctx, log) do
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
        parens_meta =
          [parens: Meta.closing_meta(open_meta, close_meta, 0, [], base_first: true)]

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
        {state, newlines} = EOE.skip_count_newlines(state, 0)

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
                # unwrap_splice for stab clause patterns
                unwrapped = unwrap_splice(new_patterns)
                {pats, stab_m} = apply_parens_meta(unwrapped, parens_meta, stab_meta)
                # Wrap with guard
                guard_patterns = [{:when, when_meta, pats ++ [guard]}]
                {guard_patterns, stab_m}

              :no_guard ->
                # unwrap_splice for stab clause patterns
                unwrapped = unwrap_splice(patterns)
                apply_parens_meta(unwrapped, parens_meta, stab_meta)
            end

          clause = {:->, final_stab_meta, [final_patterns, body]}
          {:ok, clause, state, log}
        end

      {:ok, %{kind: :when_op} = when_tok, _} ->
        # Pattern followed by guard then stab
        {:ok, _when, state} = TokenAdapter.next(state)
        when_meta = token_meta(when_tok.metadata)
        state = EOE.skip(state)

        # Parse guard expression with min_bp > stab_op (10) to stop before ->
        # Use :unmatched context so do-blocks can attach to expressions like `if a do :ok end`
        with {:ok, guard, state, log} <-
               Pratt.parse_with_min_bp(state, :unmatched, log, Precedence.stab_op_bp() + 1) do
          case TokenAdapter.next(state) do
            {:ok, %{kind: :stab_op} = stab_tok, state} ->
              stab_base_meta = token_meta(stab_tok.metadata)
              # Skip EOE after stab and count newlines
              {state, newlines} = EOE.skip_count_newlines(state, 0)

              stab_meta =
                if newlines > 0, do: [newlines: newlines] ++ stab_base_meta, else: stab_base_meta

              with {:ok, body, state, log} <- parse_stab_body(state, ctx, log, terminator) do
                # unwrap_splice for stab clause patterns
                unwrapped = unwrap_splice(patterns)
                # Apply parens meta before wrapping with guard
                {final_patterns, final_stab_meta} =
                  apply_parens_meta(unwrapped, parens_meta, stab_meta)

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
      # But we need to check if this is actually stab_parens_many or just
      # a parenthesized expression in the pattern (like `(e and f) or g -> ...`)
      {:ok, %{kind: :"("} = open_tok, _} ->
        # Try parsing as stab_parens_many first
        {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

        case parse_stab_parens_many(open_tok, checkpoint_state, ctx, log) do
          {:ok, patterns, state2, log2, parens_meta} ->
            # Check if followed by -> or when (actual stab_parens_many)
            case TokenAdapter.peek(state2) do
              {:ok, %{kind: kind}, _} when kind in [:stab_op, :when_op] ->
                # Yes, this is stab_parens_many
                {:ok, patterns, state2, log2, parens_meta}

              _ ->
                # No, it's just a parenthesized expression - rewind and parse normally
                state = TokenAdapter.rewind(checkpoint_state, ref)
                parse_stab_pattern_exprs(acc, state, ctx, log)
            end

          {:error, _reason, _state2, _log2} ->
            # Error parsing as stab_parens_many - try as regular pattern
            state = TokenAdapter.rewind(checkpoint_state, ref)
            parse_stab_pattern_exprs(acc, state, ctx, log)
        end

      {:ok, tok, _} ->
        cond do
          Keywords.starts_kw?(tok) ->
            # call_args_no_parens_kw: (x: 1 -> body)
            # Use min_bp > stab_op (10) to stop keyword values before ->
            # The kw_list is already [x: 1], and we need to wrap it in a list to get [[x: 1]]
            with {:ok, kw_list, state, log} <-
                   Keywords.parse_kw_call_with_min_bp(
                     state,
                     ctx,
                     log,
                     Precedence.stab_op_bp() + 1
                   ) do
              {:ok, [kw_list], state, log}
            end

          tok.kind in [:list_string_start, :bin_string_start] ->
            # Potentially a quoted keyword like ('x': 1 -> body)
            # Parse as expression and check if it's a keyword list
            case parse_stab_pattern_exprs(acc, state, ctx, log) do
              {:ok, [kw_list], state, log} when is_list(kw_list) and length(kw_list) > 0 ->
                # It was a keyword list - return it wrapped
                {:ok, [kw_list], state, log}

              {:ok, patterns, state, log} ->
                {:ok, patterns, state, log}

              {:error, _, _, _} = error ->
                error
            end

          true ->
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
    state = EOE.skip(state)

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
          state = EOE.skip(state)

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
        # TODO: no coverage?
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
      state = EOE.skip(state)

      case TokenAdapter.peek(state) do
        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state)
          state = EOE.skip(state)
          # Check for keyword after comma
          case TokenAdapter.peek(state) do
            {:ok, tok, _} ->
              cond do
                Keywords.starts_kw?(tok) ->
                  # Expressions followed by keywords
                  with {:ok, kw_list, state, log} <- Keywords.parse_kw_call(state, ctx, log) do
                    # If expr was a keyword list from quoted key, merge them
                    if is_keyword_list(expr) do
                      {:ok, Enum.reverse(acc) ++ [expr ++ kw_list], state, log}
                    else
                      {:ok, Enum.reverse([expr | acc]) ++ [kw_list], state, log}
                    end
                  end

                is_keyword_list(expr) and tok.kind in [:list_string_start, :bin_string_start] ->
                  # Another quoted keyword after a keyword list - continue merging
                  parse_call_args_parens_quoted_kw_continuation(expr, acc, state, ctx, log)

                true ->
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

  # Check if result is a keyword list (from quoted keyword parsing)
  defp is_keyword_list(list) when is_list(list) and length(list) > 0, do: true
  defp is_keyword_list(_), do: false

  # Parse continuation of keyword list in paren call context
  defp parse_call_args_parens_quoted_kw_continuation(acc_kw, acc, state, ctx, log) do
    # Parse the quoted keyword via expression
    case Expressions.expr(state, :matched, log) do
      {:ok, expr, state, log} when is_list(expr) and length(expr) > 0 ->
        state = EOE.skip(state)

        case TokenAdapter.peek(state) do
          {:ok, %{kind: :","}, _} ->
            {:ok, _comma, state} = TokenAdapter.next(state)
            state = EOE.skip(state)

            case TokenAdapter.peek(state) do
              {:ok, %{kind: :")"}, _} ->
                # Trailing comma
                {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state, log}

              {:ok, tok, _} ->
                cond do
                  Keywords.starts_kw?(tok) ->
                    with {:ok, more_kw, state, log} <- Keywords.parse_kw_call(state, ctx, log) do
                      {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr ++ more_kw], state, log}
                    end

                  tok.kind in [:list_string_start, :bin_string_start] ->
                    parse_call_args_parens_quoted_kw_continuation(
                      acc_kw ++ expr,
                      acc,
                      state,
                      ctx,
                      log
                    )

                  true ->
                    {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state, log}
                end

              _ ->
                {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state, log}
            end

          _ ->
            {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state, log}
        end

      {:ok, _expr, state, log} ->
        # Not a keyword
        {:error, {:expected, :keyword}, state, log}

      {:error, _, _, _} = error ->
        error
    end
  end

  defp parse_stab_pattern_exprs(acc, state, _ctx, log) do
    # Parse expression as matched_expr, stopping before -> but allowing when and <-
    # This implements call_args_no_parens_expr -> matched_expr
    with {:ok, expr, state, log} <- parse_stab_pattern_expr(state, log) do
      state_after_eoe = EOE.skip(state)

      case TokenAdapter.peek(state_after_eoe) do
        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state_after_eoe)
          state = EOE.skip(state)
          # Check for keyword after comma
          case TokenAdapter.peek(state) do
            {:ok, tok, _} ->
              cond do
                Keywords.starts_kw?(tok) ->
                  # call_args_no_parens_many: exprs followed by kw
                  # Use min_bp > stab_op (10) to stop keyword values before ->
                  with {:ok, kw_list, state, log} <-
                         Keywords.parse_kw_no_parens_call_with_min_bp(
                           state,
                           :matched,
                           log,
                           Precedence.stab_op_bp() + 1
                         ) do
                    # If expr was a keyword list from quoted key, merge them
                    if is_keyword_list(expr) do
                      {:ok, Enum.reverse(acc) ++ [expr ++ kw_list], state, log}
                    else
                      {:ok, Enum.reverse(acc) ++ [expr, kw_list], state, log}
                    end
                  end

                is_keyword_list(expr) and tok.kind in [:list_string_start, :bin_string_start] ->
                  # Another quoted keyword after a keyword list - continue merging
                  parse_stab_pattern_exprs_quoted_kw_continuation(expr, acc, state, log)

                true ->
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

  # Parse continuation of keyword list in stab pattern context when we encounter another quoted keyword
  defp parse_stab_pattern_exprs_quoted_kw_continuation(acc_kw, acc, state, log) do
    # Parse the quoted keyword via expression
    case parse_stab_pattern_expr(state, log) do
      {:ok, expr, state, log} when is_list(expr) and length(expr) > 0 ->
        state_after_eoe = EOE.skip(state)

        case TokenAdapter.peek(state_after_eoe) do
          {:ok, %{kind: :","}, _} ->
            {:ok, _comma, state} = TokenAdapter.next(state_after_eoe)
            state = EOE.skip(state)

            case TokenAdapter.peek(state) do
              {:ok, %{kind: :stab_op}, _} ->
                # End before ->
                {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state, log}

              {:ok, tok, _} ->
                cond do
                  Keywords.starts_kw?(tok) ->
                    with {:ok, more_kw, state, log} <-
                           Keywords.parse_kw_no_parens_call_with_min_bp(
                             state,
                             :matched,
                             log,
                             Precedence.stab_op_bp() + 1
                           ) do
                      {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr ++ more_kw], state, log}
                    end

                  tok.kind in [:list_string_start, :bin_string_start] ->
                    parse_stab_pattern_exprs_quoted_kw_continuation(
                      acc_kw ++ expr,
                      acc,
                      state,
                      log
                    )

                  true ->
                    {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state, log}
                end

              _ ->
                {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state, log}
            end

          {:ok, %{kind: :stab_op}, _} ->
            {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state, log}

          _ ->
            {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state, log}
        end

      {:ok, _expr, state, log} ->
        {:error, {:expected, :keyword}, state, log}

      {:error, _, _, _} = error ->
        error
    end
  end

  # Parse a single stab pattern expression.
  # Handles containers ({}, [], <<>>) specially, then delegates to Pratt.
  # Uses @stab_pattern_min_bp to stop before -> only, allowing when and <-/\.
  defp parse_stab_pattern_expr(state, log) do
    case TokenAdapter.peek(state) do
      # Container tokens - parse container base then continue with led at min_bp
      # Use parse_container_base which doesn't call Pratt.led internally
      {:ok, %{kind: kind}, _} when kind in [:"{", :"[", :"<<"] ->
        with {:ok, ast, state, log} <- Containers.parse_container_base(state, :matched, log) do
          # Continue with led to handle trailing operators
          Pratt.led(ast, state, log, @stab_pattern_min_bp, :matched)
        end

      # Map literal %{} or struct %Name{}
      {:ok, %{kind: kind}, _} when kind in [:%{}, :%] ->
        Maps.parse_map(state, :matched, log, @stab_pattern_min_bp)

      # Other tokens - use Pratt parser with min_bp to stop before ->
      _ ->
        case Pratt.parse_with_min_bp(state, :matched, log, @stab_pattern_min_bp) do
          {:ok, ast, state, log} ->
            {:ok, ast, state, log}

          # Handle quoted keyword key - parse value and return as keyword pair
          {:keyword_key, key_atom, state, log} ->
            state = EOE.skip(state)

            with {:ok, value_ast, state, log} <-
                   Pratt.parse_with_min_bp(state, :matched, log, @stab_pattern_min_bp) do
              {:ok, [{key_atom, value_ast}], state, log}
            end

          {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, log} ->
            state = EOE.skip(state)

            with {:ok, value_ast, state, log} <-
                   Pratt.parse_with_min_bp(state, :matched, log, @stab_pattern_min_bp) do
              key_ast =
                Expressions.build_interpolated_keyword_key(parts, kind, start_meta, delimiter)

              {:ok, [{key_ast, value_ast}], state, log}
            end

          {:error, reason, state, log} ->
            {:error, reason, state, log}
        end
    end
  end

  # Parse the body of a stab clause (expression or nil if empty)
  # Default terminator is :) for paren stabs
  def parse_stab_body(state, ctx, log) do
    parse_stab_body(state, ctx, log, :")")
  end

  @doc """
  Parse stab clause body with custom terminator.
  Exported for use by fn parsing.

  The body can contain multiple expressions separated by EOE (newlines/semicolons).
  Expressions are collected until we see:
  - The terminator (e.g., :end, :))
  - A block identifier (e.g., :else, :rescue)
  - The start of a new stab clause (detected by trying to parse and seeing ->)
  """
  def parse_stab_body(state, _ctx, log, terminator) do
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
        # Parse first expression - always use :unmatched in stab body to allow do-blocks
        with {:ok, first_expr, state, log} <- Expressions.expr(state, :unmatched, log) do
          # Check if there are more expressions in the body
          collect_stab_body_exprs([first_expr], state, :unmatched, log, terminator)
        end

      {:eof, state} ->
        {:ok, nil, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Collect additional expressions for the stab body
  # Expressions after the first are separated by EOE (semicolons/newlines)
  defp collect_stab_body_exprs(acc, state, ctx, log, terminator) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe} = eoe_tok, _} ->
        # Annotate the last expression with EOE metadata
        acc = annotate_last_expr_eoe(acc, eoe_tok)
        # Consume EOE
        {:ok, _eoe, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        # Check what's next after EOE
        case TokenAdapter.peek(state) do
          {:ok, %{kind: ^terminator}, _} ->
            # Terminator - done collecting
            build_stab_body(acc, state, log)

          # Block identifiers stop collection (for do blocks)
          {:ok, %{kind: :block_identifier}, _} when terminator == :end ->
            build_stab_body(acc, state, log)

          {:ok, _, _} ->
            # More content after EOE - need to check if it's a new stab clause
            # or more body expressions
            check_and_collect_stab_body(acc, state, ctx, log, terminator)

          {:eof, state} ->
            build_stab_body(acc, state, log)

          {:error, diag, state} ->
            {:error, diag, state, log}
        end

      {:ok, %{kind: ^terminator}, _} ->
        # Terminator without EOE - done
        build_stab_body(acc, state, log)

      {:ok, %{kind: :block_identifier}, _} when terminator == :end ->
        build_stab_body(acc, state, log)

      {:eof, state} ->
        build_stab_body(acc, state, log)

      {:error, diag, state} ->
        {:error, diag, state, log}

      {:ok, _, _} ->
        # Unexpected token - stop collecting (caller will handle)
        build_stab_body(acc, state, log)
    end
  end

  # Check if the next content is a new stab clause or more body expressions
  defp check_and_collect_stab_body(acc, state, ctx, log, terminator) do
    # Try to see if this looks like the start of a new stab clause
    # We use checkpoint to try parsing, then rewind if it's a stab
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

    case try_parse_stab_clause(checkpoint_state, ctx, log, terminator) do
      {:ok, _clause, _state2, _log2} ->
        # This is a new stab clause - stop collecting body, rewind
        build_stab_body(acc, TokenAdapter.rewind(checkpoint_state, ref), log)

      {:not_stab, _state2, _log2} ->
        # Not a stab - this expression is part of the current body
        # Rewind and parse as expression
        # Use min_bp > stab_op (10) to stop before -> so it doesn't get consumed
        # as a binary operator. The -> belongs to the clause parser.
        state = TokenAdapter.rewind(checkpoint_state, ref)

        with {:ok, expr, state, log} <-
               Pratt.parse_with_min_bp(state, ctx, log, Precedence.stab_op_bp() + 1) do
          collect_stab_body_exprs([expr | acc], state, ctx, log, terminator)
        end

      {:error, _reason, _state2, _log2} ->
        # Error parsing as stab - try as expression
        # Use min_bp > stab_op (10) to stop before -> so it doesn't get consumed
        # as a binary operator. The -> belongs to the clause parser.
        state = TokenAdapter.rewind(checkpoint_state, ref)

        with {:ok, expr, state, log} <-
               Pratt.parse_with_min_bp(state, ctx, log, Precedence.stab_op_bp() + 1) do
          collect_stab_body_exprs([expr | acc], state, ctx, log, terminator)
        end
    end
  end

  # Build the final stab body from collected expressions
  # unquote_splicing always gets wrapped in __block__ (parser assumes block is being spliced)
  # See elixir_parser.yrl: build_block([{unquote_splicing, _, [_]}]=Exprs, BeforeAfter)
  defp build_stab_body([{:unquote_splicing, _, [_]} = single], state, log) do
    {:ok, {:__block__, [], [single]}, state, log}
  end

  defp build_stab_body([single], state, log), do: {:ok, single, state, log}

  defp build_stab_body(exprs, state, log) do
    # Multiple expressions - wrap in __block__
    # Expressions are accumulated in reverse order, so reverse them
    block = {:__block__, [], Enum.reverse(exprs)}
    {:ok, block, state, log}
  end

  # Annotate the last (most recent) expression with EOE metadata
  defp annotate_last_expr_eoe([last | rest], eoe_tok) do
    eoe_meta = EOE.build_eoe_meta(eoe_tok)
    annotated = EOE.annotate_eoe(last, eoe_meta)
    [annotated | rest]
  end

  # Parse remaining stab clauses after the first one
  # min_bp controls whether to continue with led() for trailing operators
  defp parse_remaining_stab_clauses(acc, state, ctx, log, min_bp \\ 0) do
    state = EOE.skip(state)

    case TokenAdapter.peek(state) do
      {:ok, %{kind: :")"}, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        # Continue with Pratt.led to handle trailing operators like |
        Pratt.led(Enum.reverse(acc), state, log, min_bp, ctx)

      {:ok, _, _} ->
        # Try to parse another stab clause
        case try_parse_stab_clause(state, ctx, log) do
          {:ok, clause, state, log} ->
            parse_remaining_stab_clauses([clause | acc], state, ctx, log, min_bp)

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
        state = EOE.skip(state)

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

  defp token_meta(meta), do: Builder.Helpers.token_meta(meta)

  #   %% an arg style call. unwrap_splice unwraps the splice
  defp unwrap_splice([{:__block__, _, [{:unquote_splicing, _, _}] = splice}]), do: splice
  defp unwrap_splice(other), do: other
end
