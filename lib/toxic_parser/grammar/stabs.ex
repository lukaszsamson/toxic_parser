defmodule ToxicParser.Grammar.Stabs do
  @moduledoc """
  Stab parsing extracted from Containers to keep paren/list/tuple handling focused.
  """

  alias ToxicParser.{Context, Cursor, EventLog, Pratt, State, TokenAdapter, NoParens}
  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.{Containers, EOE, ErrorHelpers, Expressions, Keywords, Maps}
  require NoParens

  # Inlined from ToxicParser.Precedence - stab_op binding power is 10
  @stab_op_bp 10

  # Stab pattern parsing uses min_bp one higher than stab_op (bp=10) to stop before `->`
  # but allow all other operators including `when` (bp=50) and `<-`/`\` (bp=40).
  # The `when` at the TOP LEVEL of patterns is handled specially after parsing (to extract guards).
  @stab_pattern_min_bp @stab_op_bp + 1

  # After leading semicolon: either close paren (empty) or stab content
  def parse_paren_stab_or_empty(
        open_meta,
        newlines,
        %State{} = state,
        cursor,
        %Context{} = ctx,
        %EventLog{} = log,
        min_bp
      ) do
    case Cursor.peek(cursor) do
      {:ok, {:")", _meta, _value} = close_tok, cursor} ->
        # (;) -> empty stab with semicolon
        # Output: {:__block__, [closing: [...], line: L, column: C], []}
        # NOTE: Empty (;) parens don't include newlines in metadata
        {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
        close_meta = TokenAdapter.token_meta(close_tok)
        meta = Meta.closing_meta(open_meta, close_meta, 0)
        ast = {:__block__, meta, []}
        # Continue with Pratt.led to handle trailing operators
        Pratt.led(ast, state, cursor, log, min_bp, ctx)

      {:ok, _, cursor} ->
        # Parse stab expression(s) after leading semicolon
        parse_paren_stab(open_meta, newlines, state, cursor, ctx, log, min_bp)
    end
  end

  # Parse stab expressions inside parens
  # min_bp controls whether to continue with led() for trailing operators
  # newlines is the count of newlines after the opening paren
  def parse_paren_stab(
        open_meta,
        newlines,
        %State{} = state,
        cursor,
        %Context{} = ctx,
        %EventLog{} = log,
        min_bp
      ) do
    with {:ok, items, state, cursor, log} <-
           parse_stab_items_until([], state, cursor, ctx, log, :")") do
      {state, cursor} = EOE.skip(state, cursor)

      case TokenAdapter.next(state, cursor) do
        {:ok, {:")", _meta, _value} = close_tok, state, cursor} ->
          close_meta = TokenAdapter.token_meta(close_tok)

          result =
            case check_stab(items) do
              :stab ->
                # For stab clauses in parens, encode the stab list through literal_encoder
                # This matches Elixir's behavior where parenthesized stabs get wrapped
                stab_list = collect_stab(items)
                # Include newlines in paren_meta if > 0
                paren_meta = Meta.closing_meta(open_meta, close_meta, newlines)
                ToxicParser.Builder.Helpers.literal(stab_list, paren_meta, state)

              _ ->
                unwrap_single_non_stab_with_parens(
                  Enum.reverse(items),
                  open_meta,
                  close_meta,
                  newlines
                )
            end

          Pratt.led(result, state, cursor, log, min_bp, ctx)

        {:ok, {got_kind, _meta, _value}, state, cursor} ->
          {:error, {:expected, :")", got: got_kind}, state, cursor, log}

        {:eof, state, cursor} ->
          {:error, missing_terminator_reason(open_meta, cursor, :")"), state, cursor, log}

        {:error, diag, state, cursor} ->
          {:error, diag, state, cursor, log}
      end
    end
  end

  # Unwrap a single non-stab expression from a list with parens metadata
  # Stab clauses are kept as lists: [{:->, ...}]
  # Plain expressions are unwrapped and get parens metadata added (matching Elixir's build_block)
  defp unwrap_single_non_stab_with_parens(
         [{:->, _, _} | _] = stabs,
         _open_meta,
         _close_meta,
         _newlines
       ) do
    stabs
  end

  # Special case for unary ! and not operators with single argument
  # These get wrapped in __block__ with empty metadata instead of adding parens
  # See elixir_parser.yrl build_paren_stab: ?rearrange_uop(Op) clause
  defp unwrap_single_non_stab_with_parens(
         [{op, _meta, [_single_arg]} = expr],
         _open_meta,
         _close_meta,
         _newlines
       )
       when op in [:!, :not] do
    {:__block__, [], [expr]}
  end

  defp unwrap_single_non_stab_with_parens(
         [{:unquote_splicing, _meta, [_]} = single],
         open_meta,
         close_meta,
         _newlines
       ) do
    # Elixir does not attach newline metadata for this paren form.
    {:__block__, Meta.closing_meta(open_meta, close_meta, 0), [single]}
  end

  defp unwrap_single_non_stab_with_parens([{name, meta, args}], open_meta, close_meta, _newlines)
       when is_list(meta) do
    # Single 3-tuple expression - add parens metadata like Elixir's build_block does
    # For parens metadata, Elixir uses: [line: L, column: C, closing: [...]]
    # (base_meta first, no newlines for single-expression parens)
    parens_meta = open_meta ++ [closing: close_meta]
    {name, [{:parens, parens_meta} | meta], args}
  end

  defp unwrap_single_non_stab_with_parens([single], _open_meta, _close_meta, _newlines),
    do: single

  # Multiple non-stab expressions - wrap in __block__ with closing metadata
  # NOTE: Non-stab expressions in parens don't get newlines in metadata (only stab lists do)
  defp unwrap_single_non_stab_with_parens(exprs, open_meta, close_meta, _newlines)
       when is_list(exprs) do
    meta = Meta.closing_meta(open_meta, close_meta, 0)
    {:__block__, meta, exprs}
  end

  @doc """
  Try to parse a stab clause with custom terminator.
  Returns {:ok, clause, state, cursor, log}, {:not_stab, state, cursor, log}, or {:error, ...}
  Exported for use by fn parsing.
  """
  def try_parse_stab_clause(
        %State{} = state,
        cursor,
        %Context{} = ctx,
        %EventLog{} = log,
        terminator
      ) do
    # Parse the pattern part first
    case parse_stab_patterns([], state, cursor, ctx, log) do
      # 6-tuple: patterns with parens metadata (from stab_parens_many)
      {:ok, patterns, state, cursor, log, {open_meta, close_meta}} ->
        parens_meta =
          [parens: Meta.closing_meta(open_meta, close_meta, 0, [], base_first: true)]

        parse_stab_clause_after_patterns(
          patterns,
          parens_meta,
          state,
          cursor,
          ctx,
          log,
          terminator
        )

      # 5-tuple: patterns without parens metadata
      {:ok, patterns, state, cursor, log} ->
        parse_stab_clause_after_patterns(patterns, [], state, cursor, ctx, log, terminator)

      {:error, reason, state, cursor, log} ->
        {:error, reason, state, cursor, log}
    end
  end

  defp parse_stab_clause_after_patterns(
         patterns,
         parens_meta,
         state,
         cursor,
         ctx,
         log,
         terminator
       ) do
    case Cursor.peek(cursor) do
      {:ok, {:stab_op, _, _} = stab_tok, cursor} ->
        # This is definitely a stab clause
        {:ok, _stab, state, cursor} = TokenAdapter.next(state, cursor)
        stab_base_meta = TokenAdapter.token_meta(stab_tok)
        # Newlines metadata for `->` comes from the EOE after the operator (stab_op_eol)
        # and from the token itself when `->` starts on a new line.
        token_newlines = TokenAdapter.newlines(stab_tok)
        {state, cursor, newlines_after} = EOE.skip_newlines_only(state, cursor, 0)

        newlines = if newlines_after > 0, do: newlines_after, else: token_newlines

        stab_meta =
          if newlines > 0, do: [{:newlines, newlines} | stab_base_meta], else: stab_base_meta

        with {:ok, body, state, cursor, log} <-
               parse_stab_body(state, cursor, ctx, log, terminator) do
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

          body = wrap_unquote_splicing_body(body)
          clause = {:->, final_stab_meta, [final_patterns, body]}
          {:ok, clause, state, cursor, log}
        end

      {:ok, {:when_op, _, _} = when_tok, cursor} ->
        # Pattern followed by guard then stab
        {:ok, _when, state, cursor} = TokenAdapter.next(state, cursor)
        when_meta = TokenAdapter.token_meta(when_tok)
        {state, cursor} = EOE.skip(state, cursor)

        # If the next token is a keyword pair starter (`y:`), this cannot be a stab guard.
        # It's the `when` operator with a keyword list RHS (see `no_parens_op_expr`).
        case Cursor.peek(cursor) do
          {:ok, {_, _meta, tok_value} = tok, cursor} ->
            if Keywords.starts_kw?(tok) do
              cond do
                patterns == [] ->
                  # `when foo: 1` has no LHS for the `when` operator, and `foo: 1` is not a valid
                  # guard expression on its own. Match Elixir's "syntax error before: foo".
                  {:error, syntax_error_before(TokenAdapter.token_meta(tok), tok_value), state,
                   cursor, log}

                true ->
                  # This may be the special `when` operator form that allows a keyword list RHS:
                  # `no_parens_op_expr -> when_op_eol call_args_no_parens_kw`.
                  #
                  # If it is followed by `->`, treat it as part of the patterns (not a guard).
                  min_bp = @stab_op_bp + 1

                  with {:ok, kw_list, state, cursor, log} <-
                         Keywords.parse_kw_no_parens_call_with_min_bp(
                           state,
                           cursor,
                           ctx,
                           log,
                           min_bp
                         ) do
                    {state, cursor} = EOE.skip(state, cursor)

                    case Cursor.peek(cursor) do
                      {:ok, {:stab_op, _, _}, cursor} ->
                        {patterns, parens_meta} =
                          if match?([_], patterns) and parens_meta != [] do
                            unwrapped = unwrap_splice(patterns)

                            {patterns_with_parens, _stab_meta} =
                              apply_parens_meta(unwrapped, parens_meta, [])

                            {patterns_with_parens, []}
                          else
                            {patterns, parens_meta}
                          end

                        left = List.last(patterns)
                        when_ast = {:when, when_meta, [left, kw_list]}
                        new_patterns = List.replace_at(patterns, -1, when_ast)

                        parse_stab_clause_after_patterns(
                          new_patterns,
                          parens_meta,
                          state,
                          cursor,
                          ctx,
                          log,
                          terminator
                        )

                      {:ok, _, cursor} ->
                        {:not_stab, state, cursor, log}
                    end
                  end
              end
            else
              # Parse guard expression with min_bp > stab_op (10) to stop before ->
              # Use `expr` context (elixir_parser.yrl: `... when_op expr stab_op ...`).
              with {:ok, guard, state, cursor, log} <-
                     Pratt.parse_with_min_bp(
                       state,
                       cursor,
                       Context.expr(),
                       log,
                       @stab_op_bp + 1
                     ) do
                case TokenAdapter.next(state, cursor) do
                  {:ok, {:stab_op, _, _} = stab_tok, state, cursor} ->
                    stab_base_meta = TokenAdapter.token_meta(stab_tok)

                    # Newlines metadata for `->` comes from the EOE after the operator (stab_op_eol)
                    # and from the token itself when `->` starts on a new line.
                    token_newlines = TokenAdapter.newlines(stab_tok)
                    {state, cursor, newlines_after} = EOE.skip_newlines_only(state, cursor, 0)
                    newlines = if newlines_after > 0, do: newlines_after, else: token_newlines

                    stab_meta =
                      if newlines > 0,
                        do: [{:newlines, newlines} | stab_base_meta],
                        else: stab_base_meta

                    with {:ok, body, state, cursor, log} <-
                           parse_stab_body(state, cursor, ctx, log, terminator) do
                      # unwrap_splice for stab clause patterns
                      unwrapped = unwrap_splice(patterns)
                      # Apply parens meta before wrapping with guard
                      {final_patterns, final_stab_meta} =
                        apply_parens_meta(unwrapped, parens_meta, stab_meta)

                      # Wrap patterns with guard in a when clause
                      patterns_with_guard = unwrap_when(final_patterns, guard, when_meta)
                      body = wrap_unquote_splicing_body(body)
                      clause = {:->, final_stab_meta, [patterns_with_guard, body]}
                      {:ok, clause, state, cursor, log}
                    end

                  {:ok, _token, state, cursor} ->
                    # No stab after when+guard - this is just `when` as a binary operator
                    # Return :not_stab so we can try parsing as a regular expression
                    {:not_stab, state, cursor, log}

                  {:eof, state, cursor} ->
                    {:not_stab, state, cursor, log}

                  {:error, diag, state, cursor} ->
                    {:error, diag, state, cursor, log}
                end
              end
            end

          {:ok, _, cursor} ->
            # Cursor.peek should not return eof here (terminator inserted), but be defensive.
            {:not_stab, state, cursor, log}
        end

      {:ok, _, cursor} ->
        # No stab operator - not a stab clause
        {:not_stab, state, cursor, log}
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
    # 1 pattern - parens go on the pattern if it's a 3-tuple with metadata.
    # For keyword lists (plain lists), parens go on the stab arrow.
    # For literals (no metadata carrier), Elixir drops the parens.
    case single_pattern do
      # elixir_parser.yrl build_paren_stab/3: rearrange_uop(not/!) into a __block__
      {op, _meta, [_]} when op in [:!, :not] ->
        {[{:__block__, [], [single_pattern]}], stab_meta}

      {_name, meta, _args} when is_list(meta) ->
        pattern_with_parens = add_parens_to_pattern(single_pattern, parens_meta)
        {[pattern_with_parens], stab_meta}

      _ when is_list(single_pattern) ->
        if is_keyword_list(single_pattern) do
          # Keyword lists (including interpolated keys) keep parens metadata on the stab arrow.
          {[single_pattern], parens_meta ++ stab_meta}
        else
          # Literal lists drop parens metadata in Elixir.
          {[single_pattern], stab_meta}
        end

      _ ->
        {[single_pattern], stab_meta}
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

  defp wrap_unquote_splicing_body({:unquote_splicing, _, [_]} = u), do: {:__block__, [], [u]}
  defp wrap_unquote_splicing_body(other), do: other

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
      {:when, when_meta, when_args = [_, _ | _]} ->
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
  defp parse_stab_patterns(acc, state, cursor, ctx, log) do
    case Cursor.peek(cursor) do
      # If we immediately see stab_op, there are no patterns
      {:ok, {:stab_op, _, _}, cursor} ->
        {:ok, [], state, cursor, log}

      # stab_parens_many: fn (args) -> body end
      # But we need to check if this is actually stab_parens_many or just
      # a parenthesized expression in the pattern (like `(e and f) or g -> ...`)
      {:ok, {:"(", _meta, _value} = open_tok, cursor} ->
        # Try parsing as stab_parens_many first
        {ref, checkpoint_state} = TokenAdapter.checkpoint(state, cursor)

        case find_missing_paren_closer(checkpoint_state, cursor) do
          {:missing, eof_cursor} ->
            open_meta = TokenAdapter.token_meta(open_tok)
            state = TokenAdapter.drop_checkpoint(checkpoint_state, ref)

            {:error, missing_terminator_reason(open_meta, eof_cursor, :")"), state, eof_cursor,
             log}

          :found ->
            case parse_stab_parens_many(open_tok, checkpoint_state, cursor, ctx, log) do
              {:ok, patterns, state2, cursor2, log2, parens_meta} ->
                # Check if followed by -> or when (actual stab_parens_many)
                case Cursor.peek(cursor2) do
                  {:ok, {kind, _meta, _value}, cursor2} when kind in [:stab_op, :when_op] ->
                    # Yes, this is stab_parens_many
                    state2 = TokenAdapter.drop_checkpoint(state2, ref)
                    {:ok, patterns, state2, cursor2, log2, parens_meta}

                  _ ->
                    # No, it's just a parenthesized expression - rewind and parse normally
                    {state, cursor} = TokenAdapter.rewind(checkpoint_state, ref)

                    case parse_stab_pattern_exprs(acc, state, cursor, ctx, log) do
                      {:error, reason, state, cursor, log} ->
                        {terminators, state} = TokenAdapter.current_terminators(state, cursor)

                        if syntax_error_before_comma?(reason) or :")" in terminators do
                          open_meta = TokenAdapter.token_meta(open_tok)

                          {:error, missing_terminator_reason(open_meta, cursor, :")"), state,
                           cursor, log}
                        else
                          {:error, reason, state, cursor, log}
                        end

                      other ->
                        other
                    end
                end

              {:error, reason, state2, cursor2, log2} ->
                cond do
                  missing_terminator_reason?(reason) ->
                    {:error, reason, state2, cursor2, log2}

                  Cursor.eof?(cursor2) ->
                    open_meta = TokenAdapter.token_meta(open_tok)

                    {:error, missing_terminator_reason(open_meta, cursor2, :")"), state2, cursor2,
                     log2}

                  true ->
                    {terminators, state2} = TokenAdapter.current_terminators(state2, cursor2)

                    if :")" in terminators do
                      open_meta = TokenAdapter.token_meta(open_tok)

                      {:error, missing_terminator_reason(open_meta, cursor2, :")"), state2,
                       cursor2, log2}
                    else
                      {state, cursor} = TokenAdapter.rewind(checkpoint_state, ref)

                      case parse_stab_pattern_exprs(acc, state, cursor, ctx, log) do
                        {:error, reason, state, cursor, log} ->
                          {terminators, state} = TokenAdapter.current_terminators(state, cursor)

                          if syntax_error_before_comma?(reason) or :")" in terminators do
                            open_meta = TokenAdapter.token_meta(open_tok)

                            {:error, missing_terminator_reason(open_meta, cursor, :")"), state,
                             cursor, log}
                          else
                            {:error, reason, state, cursor, log}
                          end

                        other ->
                          other
                      end
                    end
                end
            end
        end

      {:ok, {kind, _meta, _value} = tok, cursor} ->
        cond do
          Keywords.starts_kw?(tok) ->
            # call_args_no_parens_kw: (x: 1 -> body)
            # Use min_bp > stab_op (10) to stop keyword values before ->
            # The kw_list is already [x: 1], and we need to wrap it in a list to get [[x: 1]]
            # allow_no_parens: true because stab patterns use call_args_no_parens_kw grammar
            with {:ok, kw_list, state, cursor, log} <-
                   Keywords.parse_kw_call_with_min_bp(
                     state,
                     cursor,
                     ctx,
                     log,
                     @stab_op_bp + 1,
                     allow_no_parens: true
                   ) do
              {:ok, [kw_list], state, cursor, log}
            end

          kind in [:list_string_start, :bin_string_start] ->
            # Potentially a quoted keyword like ('x': 1 -> body)
            # Parse as expression and check if it's a keyword list
            case parse_stab_pattern_exprs(acc, state, cursor, ctx, log) do
              {:ok, [kw_list = [_ | _]], state, cursor, log} ->
                # It was a keyword list - return it wrapped
                {:ok, [kw_list], state, cursor, log}

              {:ok, patterns, state, cursor, log} ->
                {:ok, patterns, state, cursor, log}

              {:error, _, _, _, _} = error ->
                error
            end

          true ->
            parse_stab_pattern_exprs(acc, state, cursor, ctx, log)
        end

      {:ok, _, cursor} ->
        parse_stab_pattern_exprs(acc, state, cursor, ctx, log)
    end
  end

  # Parse stab_parens_many: fn (args) -> body end
  # Grammar: stab_parens_many -> open_paren call_args_parens close_paren : build_stab_parens_many
  defp parse_stab_parens_many(open_tok, state, cursor, ctx, log) do
    {:ok, _open, state, cursor} = TokenAdapter.next(state, cursor)
    open_meta = TokenAdapter.token_meta(open_tok)

    # Skip only newlines inside parens (NOT semicolons).
    # This ensures `(; )` is parsed as a parenthesized empty block pattern, not as `fn () ->`.
    {state, cursor, _newlines} = EOE.skip_newlines_only(state, cursor, 0)

    case Cursor.peek(cursor) do
      {:ok, {:";", _meta, _value}, cursor} ->
        # Force fallback to regular parenthesized-expression parsing.
        {:error, :paren_semicolon, state, cursor, log}

      # Empty parens: fn () -> body end
      {:ok, {:")", _meta, _value} = close_tok, cursor} ->
        {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
        close_meta = TokenAdapter.token_meta(close_tok)
        # Return empty patterns with parens metadata attached to the clause later
        # Store parens meta for use in try_parse_stab_clause
        {:ok, [], state, cursor, log, {open_meta, close_meta}}

      # Content inside parens - parse as call_args_parens
      {:ok, _, cursor} ->
        with {:ok, args, state, cursor, log} <-
               parse_call_args_parens(state, cursor, ctx, log) do
          {state, cursor} = EOE.skip(state, cursor)

          case TokenAdapter.next(state, cursor) do
            {:ok, {:")", _meta, _value} = close_tok, state, cursor} ->
              close_meta = TokenAdapter.token_meta(close_tok)
              {:ok, args, state, cursor, log, {open_meta, close_meta}}

            {:ok, {got_kind, _meta, _value}, state, cursor} ->
              {:error, {:expected, :")", got: got_kind}, state, cursor, log}

            {:eof, state, cursor} ->
              {:error, :unexpected_eof, state, cursor, log}

            {:error, diag, state, cursor} ->
              {:error, diag, state, cursor, log}
          end
        end

      {:eof, cursor} ->
        {:error, missing_terminator_reason(open_meta, cursor, :")"), state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  # Parse call_args_parens: comma-separated expressions, optionally followed by keywords
  defp parse_call_args_parens(state, cursor, ctx, log) do
    case Cursor.peek(cursor) do
      {:ok, tok, cursor} ->
        # TODO: no coverage?
        if Keywords.starts_kw?(tok) do
          # Just keywords: fn (x: 1) -> body end
          with {:ok, kw_list, state, cursor, log} <-
                 Keywords.parse_kw_no_parens_call(state, cursor, ctx, log) do
            {:ok, [kw_list], state, cursor, log}
          end
        else
          parse_call_args_parens_exprs([], state, cursor, ctx, log)
        end

      {:eof, cursor} ->
        parse_call_args_parens_exprs([], state, cursor, ctx, log)

      {:error, _, cursor} ->
        parse_call_args_parens_exprs([], state, cursor, ctx, log)
    end
  end

  defp parse_call_args_parens_exprs(acc, state, cursor, ctx, log) do
    # Parse matched expression
    with {:ok, expr, state, cursor, log} <-
           Expressions.expr(state, cursor, Context.matched_expr(), log) do
      {expr, state, cursor} =
        case Cursor.peek(cursor) do
          # Only annotate trailing newline EOE when this is a single-arg paren group.
          # For multi-arg groups like `(a, b\n)`, Elixir does NOT annotate the last arg.
          {:ok, {kind, _meta, _value} = sep_tok, cursor} when kind in [:eol, :";"] ->
            sep_meta = EOE.build_sep_meta(sep_tok)
            {state_after_sep, cursor} = EOE.skip(state, cursor)

            if acc == [] do
              case Cursor.peek(cursor) do
                {:ok, {:")", _, _}, cursor} ->
                  {EOE.annotate_eoe(expr, sep_meta), state_after_sep, cursor}

                {:ok, _, cursor} ->
                  {expr, state_after_sep, cursor}
              end
            else
              {expr, state_after_sep, cursor}
            end

          {:ok, _, cursor} ->
            {expr, state, cursor}
        end

      case Cursor.peek(cursor) do
        {:ok, {:",", _meta, _value}, cursor} ->
          {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)
          {state, cursor} = EOE.skip(state, cursor)
          # Check for keyword after comma
          case Cursor.peek(cursor) do
            {:ok, tok, cursor} ->
              cond do
                Keywords.starts_kw?(tok) ->
                  # Expressions followed by keywords
                  with {:ok, kw_list, state, cursor, log} <-
                         Keywords.parse_kw_no_parens_call(state, cursor, ctx, log) do
                    # If expr was a keyword list from quoted key, merge them
                    if is_keyword_list(expr) do
                      {:ok, :lists.reverse(acc, [expr ++ kw_list]), state, cursor, log}
                    else
                      {:ok, :lists.reverse([expr | acc], [kw_list]), state, cursor, log}
                    end
                  end

                is_keyword_list(expr) and
                    match?({kind, _, _} when kind in [:list_string_start, :bin_string_start], tok) ->
                  # Another quoted keyword after a keyword list - continue merging
                  parse_call_args_parens_quoted_kw_continuation(
                    expr,
                    acc,
                    state,
                    cursor,
                    ctx,
                    log
                  )

                true ->
                  parse_call_args_parens_exprs([expr | acc], state, cursor, ctx, log)
              end

            {:eof, cursor} ->
              parse_call_args_parens_exprs([expr | acc], state, cursor, ctx, log)

            {:error, _, cursor} ->
              parse_call_args_parens_exprs([expr | acc], state, cursor, ctx, log)
          end

        {:ok, _, cursor} ->
          {:ok, Enum.reverse([expr | acc]), state, cursor, log}
      end
    end
  end

  # Check if result is a keyword list (from quoted keyword parsing)
  defp is_keyword_list(list = [_ | _]) do
    Enum.all?(list, &keyword_pair?/1)
  end

  defp is_keyword_list(_), do: false

  defp keyword_pair?({key, _value}) when is_atom(key), do: true
  defp keyword_pair?({key, _value}) when is_tuple(key), do: interpolated_keyword_key?(key)
  defp keyword_pair?(_), do: false

  defp interpolated_keyword_key?({{:., _, [:erlang, :binary_to_atom]}, _, [_binary_ast, :utf8]}),
    do: true

  defp interpolated_keyword_key?(_), do: false

  # Parse continuation of keyword list in paren call context
  defp parse_call_args_parens_quoted_kw_continuation(acc_kw, acc, state, cursor, ctx, log) do
    # Parse the quoted keyword via expression
    case Expressions.expr(state, cursor, Context.matched_expr(), log) do
      {:ok, expr = [_ | _], state, cursor, log} ->
        {state, cursor} = EOE.skip(state, cursor)

        case Cursor.peek(cursor) do
          {:ok, {:",", _meta, _value}, cursor} ->
            {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)
            {state, cursor} = EOE.skip(state, cursor)

            case Cursor.peek(cursor) do
              {:ok, {:")", _meta, _value}, cursor} ->
                # Trailing comma
                {:ok, Enum.reverse(acc, [acc_kw ++ expr]), state, cursor, log}

              {:ok, tok, cursor} ->
                cond do
                  Keywords.starts_kw?(tok) ->
                    # allow_no_parens: true because stab patterns use call_args_no_parens_kw grammar
                    with {:ok, more_kw, state, cursor, log} <-
                           Keywords.parse_kw_call(state, cursor, ctx, log, allow_no_parens: true) do
                      {:ok, Enum.reverse(acc, [acc_kw ++ expr ++ more_kw]), state, cursor, log}
                    end

                  match?({kind, _, _} when kind in [:list_string_start, :bin_string_start], tok) ->
                    parse_call_args_parens_quoted_kw_continuation(
                      acc_kw ++ expr,
                      acc,
                      state,
                      cursor,
                      ctx,
                      log
                    )

                  true ->
                    {:ok, Enum.reverse(acc, [acc_kw ++ expr]), state, cursor, log}
                end

              {:eof, cursor} ->
                {:ok, Enum.reverse(acc, [acc_kw ++ expr]), state, cursor, log}

              {:error, _, cursor} ->
                {:ok, Enum.reverse(acc, [acc_kw ++ expr]), state, cursor, log}
            end

          {:ok, _, cursor} ->
            {:ok, Enum.reverse(acc, [acc_kw ++ expr]), state, cursor, log}
        end

      {:ok, _expr, state, cursor, log} ->
        # Not a keyword
        {:error, {:expected, :keyword}, state, cursor, log}

      {:error, _, _, _, _} = error ->
        error
    end
  end

  defp parse_stab_pattern_exprs(acc, state, cursor, _ctx, log) do
    # Parse expression as matched_expr, stopping before -> but allowing when and <-
    # This implements call_args_no_parens_expr -> matched_expr
    with {:ok, expr, state, cursor, log} <- parse_stab_pattern_expr(state, cursor, log) do
      {state_after_eoe, cursor} = EOE.skip(state, cursor)

      case Cursor.peek(cursor) do
        {:ok, {:",", _meta, _value}, cursor} ->
          {:ok, _comma, state, cursor} = TokenAdapter.next(state_after_eoe, cursor)
          {state, cursor} = EOE.skip(state, cursor)
          # Check for keyword after comma
          case Cursor.peek(cursor) do
            {:ok, tok, cursor} ->
              cond do
                Keywords.starts_kw?(tok) ->
                  # call_args_no_parens_many: exprs followed by kw
                  # Use min_bp > stab_op (10) to stop keyword values before ->
                  with {:ok, kw_list, state, cursor, log} <-
                         Keywords.parse_kw_no_parens_call_with_min_bp(
                           state,
                           cursor,
                           Context.matched_expr(),
                           log,
                           @stab_op_bp + 1
                         ) do
                    # If expr was a keyword list from quoted key, merge them
                    if is_keyword_list(expr) do
                      {:ok, Enum.reverse(acc, [expr ++ kw_list]), state, cursor, log}
                    else
                      {:ok, Enum.reverse(acc, [expr, kw_list]), state, cursor, log}
                    end
                  end

                is_keyword_list(expr) and
                    match?({kind, _, _} when kind in [:list_string_start, :bin_string_start], tok) ->
                  # Another quoted keyword after a keyword list - continue merging
                  parse_stab_pattern_exprs_quoted_kw_continuation(expr, acc, state, cursor, log)

                true ->
                  parse_stab_pattern_exprs(
                    [expr | acc],
                    state,
                    cursor,
                    Context.matched_expr(),
                    log
                  )
              end

            {:eof, cursor} ->
              parse_stab_pattern_exprs([expr | acc], state, cursor, Context.matched_expr(), log)

            {:error, _, cursor} ->
              parse_stab_pattern_exprs([expr | acc], state, cursor, Context.matched_expr(), log)
          end

        {:ok, {:stab_op, _meta, _value}, cursor} ->
          {:ok, Enum.reverse([expr | acc]), state_after_eoe, cursor, log}

        {:ok, _, cursor} ->
          {:ok, Enum.reverse([expr | acc]), state_after_eoe, cursor, log}
      end
    end
  end

  # Parse continuation of keyword list in stab pattern context when we encounter another quoted keyword
  defp parse_stab_pattern_exprs_quoted_kw_continuation(acc_kw, acc, state, cursor, log) do
    # Parse the quoted keyword via expression
    case parse_stab_pattern_expr(state, cursor, log) do
      {:ok, expr = [_ | _], state, cursor, log} ->
        {state_after_eoe, cursor} = EOE.skip(state, cursor)

        case Cursor.peek(cursor) do
          {:ok, {:",", _meta, _value}, cursor} ->
            {:ok, _comma, state, cursor} = TokenAdapter.next(state_after_eoe, cursor)
            {state, cursor} = EOE.skip(state, cursor)

            case Cursor.peek(cursor) do
              {:ok, {:stab_op, _meta, _value}, cursor} ->
                # End before ->
                {:ok, Enum.reverse(acc, [acc_kw ++ expr]), state, cursor, log}

              {:ok, tok, cursor} ->
                cond do
                  Keywords.starts_kw?(tok) ->
                    with {:ok, more_kw, state, cursor, log} <-
                           Keywords.parse_kw_no_parens_call_with_min_bp(
                             state,
                             cursor,
                             Context.matched_expr(),
                             log,
                             @stab_op_bp + 1
                           ) do
                      {:ok, Enum.reverse(acc, [acc_kw ++ expr ++ more_kw]), state, cursor, log}
                    end

                  match?({kind, _, _} when kind in [:list_string_start, :bin_string_start], tok) ->
                    parse_stab_pattern_exprs_quoted_kw_continuation(
                      acc_kw ++ expr,
                      acc,
                      state,
                      cursor,
                      log
                    )

                  true ->
                    {:ok, Enum.reverse(acc, [acc_kw ++ expr]), state, cursor, log}
                end

              {:eof, cursor} ->
                {:ok, Enum.reverse(acc, [acc_kw ++ expr]), state, cursor, log}

              {:error, _, cursor} ->
                {:ok, Enum.reverse(acc, [acc_kw ++ expr]), state, cursor, log}
            end

          {:ok, {:stab_op, _meta, _value}, cursor} ->
            {:ok, Enum.reverse(acc, [acc_kw ++ expr]), state_after_eoe, cursor, log}

          {:ok, _, cursor} ->
            {:ok, Enum.reverse(acc, [acc_kw ++ expr]), state_after_eoe, cursor, log}
        end

      {:ok, _expr, state, cursor, log} ->
        {:error, {:expected, :keyword}, state, cursor, log}

      {:error, _, _, _, _} = error ->
        error
    end
  end

  # Parse a single stab pattern expression.
  # Handles containers ({}, [], <<>>) specially, then delegates to Pratt.
  # Uses @stab_pattern_min_bp to stop before -> only, allowing when and <-/\.
  defp parse_stab_pattern_expr(state, cursor, log) do
    case Cursor.peek(cursor) do
      # Container tokens - parse container base then continue with led at min_bp
      # Use parse_container_base which doesn't call Pratt.led internally
      {:ok, {kind, _meta, _value}, cursor} when kind in [:"{", :"[", :"<<"] ->
        with {:ok, ast, state, cursor, log} <-
               Containers.parse_container_base(state, cursor, Context.expr(), log) do
          # Continue with led to handle trailing operators
          Pratt.led(ast, state, cursor, log, @stab_pattern_min_bp, Context.expr())
        end

      # Map literal %{} or struct %Name{}
      {:ok, {kind, _meta, _value}, cursor} when kind in [:%{}, :%] ->
        Maps.parse_map(state, cursor, Context.expr(), log, @stab_pattern_min_bp)

      # Other tokens - use Pratt parser with min_bp to stop before ->
      {:ok, _, cursor} ->
        case Pratt.parse_with_min_bp(
               state,
               cursor,
               Context.expr(),
               log,
               @stab_pattern_min_bp
             ) do
          {:ok, ast, state, cursor, log} ->
            {:ok, ast, state, cursor, log}

          # Handle quoted keyword key - parse value and return as keyword pair
          {:keyword_key, key_atom, key_meta, state, cursor, log} ->
            {state, cursor} = EOE.skip(state, cursor)

            with {:ok, value_ast, state, cursor, log} <-
                   Pratt.parse_with_min_bp(
                     state,
                     cursor,
                     Context.expr(),
                     log,
                     @stab_pattern_min_bp
                   ) do
              key_ast = ToxicParser.Builder.Helpers.literal(key_atom, key_meta, state)
              {:ok, [{key_ast, value_ast}], state, cursor, log}
            end

          {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, cursor, log} ->
            {state, cursor} = EOE.skip(state, cursor)

            with {:ok, value_ast, state, cursor, log} <-
                   Pratt.parse_with_min_bp(
                     state,
                     cursor,
                     Context.expr(),
                     log,
                     @stab_pattern_min_bp
                   ) do
              key_ast =
                Expressions.build_interpolated_keyword_key(parts, kind, start_meta, delimiter)

              {:ok, [{key_ast, value_ast}], state, cursor, log}
            end

          {:error, reason, state, cursor, log} ->
            {:error, reason, state, cursor, log}
        end
    end
  end

  # Parse the body of a stab clause (single expr or nil if empty)
  # Default terminator is :) for paren stabs
  def parse_stab_body(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log) do
    parse_stab_body(state, cursor, ctx, log, :")")
  end

  @doc """
  Parse stab clause body with custom terminator.

  YRL-aligned: a stab clause body is a single `expr` (or omitted / `nil`).
  Multi-expression bodies are handled later via `collect_stab/1`.
  """
  def parse_stab_body(%State{} = state, cursor, %Context{}, %EventLog{} = log, terminator) do
    body_ctx = Context.expr()

    case Cursor.peek(cursor) do
      {:ok, {^terminator, _meta, _value}, cursor} ->
        {:ok, nil, maybe_warn_empty_stab_clause(state, cursor, :terminator), cursor, log}

      {:ok, {:block_identifier, _, _}, cursor} when terminator == :end ->
        {:ok, nil, maybe_warn_empty_stab_clause(state, cursor, :block_identifier), cursor, log}

      {:ok, {kind, _meta, _value}, cursor} when kind in [:eol, :";"] ->
        # `-> ;` / `-> \n` (newlines are normally already skipped by stab_op_eol)
        {:ok, nil, maybe_warn_empty_stab_clause(state, cursor, :eol), cursor, log}

      {:ok, _, cursor} ->
        Expressions.expr(state, cursor, body_ctx, log)

      {:eof, cursor} ->
        {:ok, nil, maybe_warn_empty_stab_clause(state, cursor, :eof), cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  defp maybe_warn_empty_stab_clause(%State{} = state, _cursor, _kind)
       when state.emit_warnings? == false do
    state
  end

  defp maybe_warn_empty_stab_clause(state, cursor, kind) do
    {line, column} = Cursor.position(cursor)
    adjusted_column = if kind == :block_identifier and column > 1, do: column - 1, else: column

    warning = %ToxicParser.Warning{
      code: :empty_stab_clause,
      message:
        "an expression is always required on the right side of ->. Please provide a value after ->",
      range: %{
        start: %{line: line, column: adjusted_column, offset: 0},
        end: %{line: line, column: adjusted_column, offset: 0}
      },
      details: %{}
    }

    %{state | warnings: [warning | state.warnings]}
  end

  @doc """
  Parse stab items (clauses or plain expressions) separated by EOE until `terminator` or stop token.

  Returns items in reverse source order (latest first), matching `collect_stab/1` input.
  """
  @spec parse_stab_items_until(
          list(),
          State.t(),
          Cursor.t(),
          Pratt.context(),
          EventLog.t(),
          atom(),
          [atom()]
        ) ::
          {:ok, [Macro.t()], State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}
  def parse_stab_items_until(
        acc,
        %State{} = state,
        cursor,
        %Context{} = ctx,
        %EventLog{} = log,
        terminator,
        stop_kinds \\ []
      ) do
    {state, cursor} = EOE.skip(state, cursor)

    case Cursor.peek(cursor) do
      {:ok, tok, cursor} ->
        cond do
          stop_token?(tok, terminator, stop_kinds) ->
            {:ok, acc, state, cursor, log}

          match?({:stab_op, _, _}, tok) ->
            case try_parse_stab_clause(state, cursor, ctx, log, terminator) do
              {:ok, clause, state, cursor, log} ->
                {clause, state, cursor} = maybe_annotate_and_consume_eoe(clause, state, cursor)

                parse_stab_items_until(
                  [clause | acc],
                  state,
                  cursor,
                  ctx,
                  log,
                  terminator,
                  stop_kinds
                )

              {:not_stab, _state, cursor, log} ->
                with {:ok, expr, state, cursor, log} <-
                       Pratt.parse_with_min_bp(
                         state,
                         cursor,
                         Context.expr(),
                         log,
                         @stab_op_bp + 1
                       ) do
                  {expr, state, cursor} = maybe_annotate_and_consume_eoe(expr, state, cursor)

                  parse_stab_items_until(
                    [expr | acc],
                    state,
                    cursor,
                    ctx,
                    log,
                    terminator,
                    stop_kinds
                  )
                end

              {:error, reason, state, cursor, log} ->
                {:error, reason, state, cursor, log}
            end

          true ->
            {kind, state, cursor} = classify_stab_item(state, cursor, terminator, stop_kinds)

            case kind do
              :clause ->
                case try_parse_stab_clause(state, cursor, ctx, log, terminator) do
                  {:ok, clause, state, cursor, log} ->
                    {clause, state, cursor} =
                      maybe_annotate_and_consume_eoe(clause, state, cursor)

                    parse_stab_items_until(
                      [clause | acc],
                      state,
                      cursor,
                      ctx,
                      log,
                      terminator,
                      stop_kinds
                    )

                  {:not_stab, _state, cursor, log} ->
                    with {:ok, expr, state, cursor, log} <-
                           Pratt.parse_with_min_bp(
                             state,
                             cursor,
                             Context.expr(),
                             log,
                             @stab_op_bp + 1
                           ) do
                      {expr, state, cursor} = maybe_annotate_and_consume_eoe(expr, state, cursor)

                      parse_stab_items_until(
                        [expr | acc],
                        state,
                        cursor,
                        ctx,
                        log,
                        terminator,
                        stop_kinds
                      )
                    end

                  {:error, reason, state, cursor, log} ->
                    {:error, reason, state, cursor, log}
                end

              :expr ->
                with {:ok, expr, state, cursor, log} <-
                       Pratt.parse_with_min_bp(
                         state,
                         cursor,
                         Context.expr(),
                         log,
                         @stab_op_bp + 1
                       ) do
                  {expr, state, cursor} = maybe_annotate_and_consume_eoe(expr, state, cursor)

                  parse_stab_items_until(
                    [expr | acc],
                    state,
                    cursor,
                    ctx,
                    log,
                    terminator,
                    stop_kinds
                  )
                end

              :unknown ->
                with {:ok, expr, state, cursor, log} <-
                       Pratt.parse_with_min_bp(
                         state,
                         cursor,
                         Context.expr(),
                         log,
                         @stab_op_bp + 1
                       ) do
                  {expr, state, cursor} = maybe_annotate_and_consume_eoe(expr, state, cursor)

                  parse_stab_items_until(
                    [expr | acc],
                    state,
                    cursor,
                    ctx,
                    log,
                    terminator,
                    stop_kinds
                  )
                end
            end
        end

      {:eof, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  defp maybe_annotate_and_consume_eoe(expr, state, cursor) do
    case Cursor.peek(cursor) do
      {:ok, {kind, _meta, _value} = sep_tok, cursor} when kind in [:eol, :";"] ->
        sep_meta = EOE.build_sep_meta(sep_tok)

        annotated =
          case expr do
            {:->, meta, [pats, {:__block__, [], [{:unquote_splicing, _, [_]} = u]}]} ->
              u = EOE.annotate_eoe(u, sep_meta)
              {:->, meta, [pats, {:__block__, [], [u]}]}

            {:->, meta, [pats, body]} ->
              {:->, meta, [pats, EOE.annotate_eoe(body, sep_meta)]}

            _ ->
              EOE.annotate_eoe(expr, sep_meta)
          end

        {:ok, _sep, state, cursor} = TokenAdapter.next(state, cursor)
        {state, cursor} = EOE.skip(state, cursor)
        {annotated, state, cursor}

      {:ok, _, cursor} ->
        {expr, state, cursor}

      {:eof, cursor} ->
        {expr, state, cursor}

      {:error, _diag, cursor} ->
        {expr, state, cursor}
    end
  end

  defp stop_token?({terminator, _meta, _value}, terminator, _stop_kinds), do: true
  defp stop_token?({:block_identifier, _meta, _value}, _terminator, _stop_kinds), do: true

  defp stop_token?({kind, _meta, _value}, _terminator, stop_kinds) do
    kind in stop_kinds
  end

  # Scan context tuple: {delim, block, open?, percent_pending?}
  # Using tuple instead of map for zero-allocation updates.
  @type scan_ctx :: {non_neg_integer(), non_neg_integer(), boolean(), boolean()}

  defp classify_stab_item(%State{} = state, cursor, terminator, stop_kinds) do
    # {delim, block, open?, percent_pending?}
    # Scan limit tuned via benchmarks: 220 is optimal for real Elixir code
    ctx = {0, 0, true, false}
    scan_classify(state, cursor, [], ctx, terminator, stop_kinds, 220, false)
  end

  defp scan_classify(state, cursor, consumed_rev, _ctx, _terminator, _stop_kinds, max, _boundary?)
       when max <= 0 do
    cursor = Cursor.pushback_many_rev(cursor, consumed_rev)
    {:expr, state, cursor}
  end

  defp scan_classify(state, cursor, consumed_rev, ctx, terminator, stop_kinds, max, boundary?) do
    # ctx = {delim, block, open?, percent_pending?}
    {delim, block, open?, _pp} = ctx

    case Cursor.next(cursor) do
      {:ok, {tok_kind, _meta, _value} = tok, cursor2} ->
        top? = delim == 0 and block == 0
        consumed_rev = [tok | consumed_rev]

        cond do
          top? and tok_kind == :stab_op and boundary? ->
            cursor3 = Cursor.pushback_many_rev(cursor2, consumed_rev)
            {:expr, state, cursor3}

          top? and tok_kind == :stab_op ->
            cursor3 = Cursor.pushback_many_rev(cursor2, consumed_rev)
            {:clause, state, cursor3}

          top? and boundary? and tok_kind in [:eol, :";"] ->
            scan_classify(
              state,
              cursor2,
              consumed_rev,
              ctx,
              terminator,
              stop_kinds,
              max - 1,
              true
            )

          top? and boundary? ->
            cursor3 = Cursor.pushback_many_rev(cursor2, consumed_rev)
            {:expr, state, cursor3}

          top? and stop_token?(tok, terminator, stop_kinds) ->
            cursor3 = Cursor.pushback_many_rev(cursor2, consumed_rev)
            {:expr, state, cursor3}

          top? and tok_kind == :";" ->
            cursor3 = Cursor.pushback_many_rev(cursor2, consumed_rev)
            {:expr, state, cursor3}

          top? and tok_kind == :eol and open? == false ->
            scan_classify(
              state,
              cursor2,
              consumed_rev,
              ctx,
              terminator,
              stop_kinds,
              max - 1,
              true
            )

          top? and tok_kind == :eol ->
            scan_classify(
              state,
              cursor2,
              consumed_rev,
              ctx,
              terminator,
              stop_kinds,
              max - 1,
              boundary?
            )

          tok_kind == :error_token ->
            cursor3 = Cursor.pushback_many_rev(cursor2, consumed_rev)
            {:unknown, state, cursor3}

          true ->
            ctx2 = scan_update_ctx(ctx, tok)

            scan_classify(
              state,
              cursor2,
              consumed_rev,
              ctx2,
              terminator,
              stop_kinds,
              max - 1,
              false
            )
        end

      {:eof, cursor2} ->
        cursor3 = Cursor.pushback_many_rev(cursor2, consumed_rev)
        {:expr, state, cursor3}

      {:error, _reason, cursor2} ->
        cursor3 = Cursor.pushback_many_rev(cursor2, consumed_rev)
        {:unknown, state, cursor3}
    end
  end

  # ctx = {delim, block, open?, percent_pending?}
  defp scan_update_ctx({delim, block, _open?, percent_pending?}, {kind, _meta, _value} = tok) do
    {d2, b2} =
      case kind do
        :"(" -> {delim + 1, block}
        :"[" -> {delim + 1, block}
        :"{" -> {delim + 1, block}
        :"<<" -> {delim + 1, block}
        :")" -> {max(delim - 1, 0), block}
        :"]" -> {max(delim - 1, 0), block}
        :"}" -> {max(delim - 1, 0), block}
        :">>" -> {max(delim - 1, 0), block}
        :do -> {delim, block + 1}
        :fn -> {delim, block + 1}
        :end when block > 0 -> {delim, block - 1}
        _ -> {delim, block}
      end

    pp2 =
      case {percent_pending?, kind} do
        {true, :"{"} -> false
        {false, :%} -> true
        _ -> percent_pending?
      end

    open2 =
      case percent_pending? do
        true -> map_base_expr_start?(tok)
        false -> open_kind?(kind)
      end

    {d2, b2, open2, pp2}
  end

  defp map_base_expr_start?({kind, _meta, _value} = tok) do
    NoParens.can_start_no_parens_arg?(tok) or kind in [:range_op, :ternary_op]
  end

  defp open_kind?(:","), do: true
  defp open_kind?(:kw_identifier), do: true
  defp open_kind?(:kw_identifier_unsafe_end), do: true
  defp open_kind?(:kw_identifier_safe_end), do: true
  defp open_kind?(:"("), do: true
  defp open_kind?(:"["), do: true
  defp open_kind?(:"{"), do: true
  defp open_kind?(:"<<"), do: true
  defp open_kind?(:%), do: true
  defp open_kind?(:stab_op), do: true
  defp open_kind?(:in_match_op), do: true
  defp open_kind?(:when_op), do: true
  defp open_kind?(:type_op), do: true
  defp open_kind?(:pipe_op), do: true
  defp open_kind?(:assoc_op), do: true
  defp open_kind?(:match_op), do: true
  defp open_kind?(:or_op), do: true
  defp open_kind?(:and_op), do: true
  defp open_kind?(:comp_op), do: true
  defp open_kind?(:rel_op), do: true
  defp open_kind?(:arrow_op), do: true
  defp open_kind?(:in_op), do: true
  defp open_kind?(:xor_op), do: true
  defp open_kind?(:ternary_op), do: true
  defp open_kind?(:concat_op), do: true
  defp open_kind?(:range_op), do: true
  defp open_kind?(:dual_op), do: true
  defp open_kind?(:mult_op), do: true
  defp open_kind?(:power_op), do: true
  defp open_kind?(:.), do: true
  defp open_kind?(:dot_call_op), do: true
  defp open_kind?(:capture_op), do: true
  defp open_kind?(:ellipsis_op), do: true
  defp open_kind?(:unary_op), do: true
  defp open_kind?(:at_op), do: true
  defp open_kind?(_kind), do: false

  @doc "Build the value for a do-block section (block or stab clauses)."
  @spec build_section_value([Macro.t()]) :: Macro.t()
  def build_section_value(items_rev) do
    case check_stab(items_rev) do
      :stab -> collect_stab(items_rev)
      _ -> build_block(Enum.reverse(items_rev))
    end
  end

  defp build_block([{:unquote_splicing, _, [_]} = single]), do: {:__block__, [], [single]}
  defp build_block([single]), do: single
  defp build_block(items), do: {:__block__, [], items}

  defp check_stab(items_rev) do
    items = Enum.reverse(items_rev)

    if Enum.any?(items, &match?({:->, _, _}, &1)) do
      first = Enum.find_index(items, &match?({:->, _, _}, &1))

      if Enum.any?(Enum.take(items, first), &(!match?({:->, _, _}, &1))) do
        :invalid
      else
        :stab
      end
    else
      :block
    end
  end

  @doc """
  Parse stab_eoe until a given terminator kind.
  Exported for use by fn parsing in Blocks module.
  For fn, stab is required (no fallback to plain expression).

  This implements Elixir's `stab` and `collect_stab` behavior where:
  - Multiple stab clauses and expressions can be separated by EOE
  - Non-stab expressions that follow a stab clause are merged into that stab's body
  - For example: `fn 1 -> ;fs end` has body {:__block__, [], [nil, fs]}
  """
  @spec parse_stab_eoe_until(list(), State.t(), Cursor.t(), Pratt.context(), EventLog.t(), atom()) ::
          {:ok, [Macro.t()], State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}
  def parse_stab_eoe_until(
        acc,
        %State{} = state,
        cursor,
        %Context{} = ctx,
        %EventLog{} = log,
        terminator
      ) do
    with {:ok, items, state, cursor, log} <-
           parse_stab_items_until(acc, state, cursor, ctx, log, terminator) do
      case check_stab(items) do
        :stab ->
          {:ok, collect_stab(items), state, cursor, log}

        :invalid ->
          if state.mode == :tolerant do
            reason = invalid_stab_error(items)
            {error_node, state} = build_stab_error_node(reason, state, cursor)
            {:ok, [error_node], state, cursor, log}
          else
            {:error, invalid_stab_error(items), state, cursor, log}
          end

        :block ->
          if state.mode == :tolerant do
            reason = fn_missing_stab_error(items)
            {error_node, state} = build_stab_error_node(reason, state, cursor)
            {:ok, [error_node], state, cursor, log}
          else
            {:error, fn_missing_stab_error(items), state, cursor, log}
          end
      end
    end
  end

  # Implements Elixir's collect_stab function from elixir_parser.yrl
  # Walks through the stab list, collecting non-stab expressions that follow
  # a stab clause and merging them into that clause's body.
  #
  # The input list is in REVERSE source order (latest clause first).
  # Non-stab expressions that come BEFORE a stab in the list (which means AFTER in source)
  # are merged into that stab's body.
  #
  # Example input: [fs, {:->, [[1], nil]}] (fs first, then stab)
  # In source order: `1 -> nil ; fs` where fs follows the stab
  # Output: [{:->, [[1], {:__block__, [], [nil, fs]}]}]
  defp collect_stab(items) do
    # Items are in reverse source order. Process and output in source order.
    do_collect_stab(items, [], [])
  end

  # Walk through items (in reverse source order), collecting non-stabs
  # When we see a stab, the collected exprs came BEFORE it in the list
  # (which means AFTER it in source), so they get merged into its body.
  defp do_collect_stab([{:->, meta, [left, right]} | rest], exprs, stabs) do
    # Found a stab - merge collected exprs into its body
    # exprs are in reverse source order too, so [right | exprs] puts right first
    new_body = build_stab_block([right | exprs])
    new_stab = {:->, meta, [left, new_body]}
    # Prepend to stabs (building result in source order)
    do_collect_stab(rest, [], [new_stab | stabs])
  end

  defp do_collect_stab([expr | rest], exprs, stabs) do
    # Non-stab expression - collect it
    do_collect_stab(rest, [expr | exprs], stabs)
  end

  defp do_collect_stab([], [], stabs) do
    stabs
  end

  defp do_collect_stab([], exprs, stabs) do
    # Remaining expressions without a stab - this shouldn't happen in valid fn
    # but handle gracefully by returning them
    exprs ++ stabs
  end

  defp fn_missing_stab_error([{:__block__, meta, _} | _]) when is_list(meta) do
    {Keyword.take(meta, [:line, :column]),
     "expected anonymous functions to be defined with -> inside: ", "'fn'"}
  end

  defp fn_missing_stab_error([first | _]) do
    meta = extract_meta(first)

    location =
      if meta == [] do
        [line: 1, column: 1]
      else
        meta
      end

    {location, "expected anonymous functions to be defined with -> inside: ", "'fn'"}
  end

  defp fn_missing_stab_error([]) do
    {[line: 1, column: 1], "expected anonymous functions to be defined with -> inside: ", "'fn'"}
  end

  defp invalid_stab_error(items) do
    case Enum.find(items, &match?({:->, _, _}, &1)) do
      {:->, meta, _} when is_list(meta) ->
        location = Keyword.take(meta, [:line, :column])

        {location,
         "unexpected operator ->. If you want to define multiple clauses, the first expression must use ->. Syntax error before: ",
         "'->'"}

      _ ->
        {[],
         "unexpected operator ->. If you want to define multiple clauses, the first expression must use ->. Syntax error before: ",
         "'->'"}
    end
  end

  defp extract_meta({_name, meta, _args}) when is_list(meta) do
    Keyword.take(meta, [:line, :column])
  end

  defp extract_meta(_), do: []

  defp build_stab_error_node(reason, %State{} = state, cursor) do
    ErrorHelpers.build_error_node_from_reason(:invalid, reason, state, cursor)
  end

  # Build a block from multiple expressions, or return single expression as-is
  defp build_stab_block([single]), do: single

  defp build_stab_block(exprs) when is_list(exprs) do
    exprs =
      Enum.map(exprs, fn
        {:__block__, [], [{:unquote_splicing, _meta, [_]} = u]} -> u
        other -> other
      end)

    {:__block__, [], exprs}
  end

  defp syntax_error_before(meta, token_value) when is_atom(token_value) do
    syntax_error_before(meta, Atom.to_string(token_value))
  end

  defp syntax_error_before(meta, token_value) when is_binary(token_value) do
    line = Keyword.get(meta, :line, 1)
    column = Keyword.get(meta, :column, 1)
    {[line: line, column: column], "syntax error before: ", token_value}
  end

  defp missing_terminator_reason(open_meta, cursor, expected_delimiter) do
    line = Keyword.get(open_meta, :line, 1)
    column = Keyword.get(open_meta, :column, 1)
    {end_line, end_column} = Cursor.position(cursor)

    reason_meta = [
      opening_delimiter: :"(",
      expected_delimiter: expected_delimiter,
      line: line,
      column: column,
      end_line: end_line || line,
      end_column: end_column || column
    ]

    {reason_meta, "missing terminator: #{expected_delimiter}", ""}
  end

  defp missing_terminator_reason?({meta, message, ""})
       when is_list(meta) and is_binary(message) do
    String.starts_with?(message, "missing terminator:")
  end

  defp missing_terminator_reason?(_), do: false

  defp syntax_error_before_comma?({meta, "syntax error before: ", "','"})
       when is_list(meta) do
    true
  end

  defp syntax_error_before_comma?(_), do: false

  defp find_missing_paren_closer(state, cursor) do
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state, cursor)
    {result, _state, cursor_after} = scan_paren_depth(checkpoint_state, cursor, 0)
    _state = TokenAdapter.drop_checkpoint(checkpoint_state, ref)

    case result do
      :found -> :found
      :missing -> {:missing, cursor_after}
    end
  end

  defp scan_paren_depth(state, cursor, depth) do
    case TokenAdapter.next(state, cursor) do
      {:ok, {:"(", _meta, _value}, state, cursor} ->
        scan_paren_depth(state, cursor, depth + 1)

      {:ok, {:")", _meta, _value}, state, cursor} ->
        if depth <= 1 do
          {:found, state, cursor}
        else
          scan_paren_depth(state, cursor, depth - 1)
        end

      {:ok, _tok, state, cursor} ->
        scan_paren_depth(state, cursor, depth)

      {:eof, state, cursor} ->
        {:missing, state, cursor}

      {:error, _diag, state, cursor} ->
        {:missing, state, cursor}
    end
  end

  #   %% an arg style call. unwrap_splice unwraps the splice
  defp unwrap_splice([{:__block__, _, [{:unquote_splicing, _, _}] = splice}]), do: splice
  defp unwrap_splice(other), do: other
end
