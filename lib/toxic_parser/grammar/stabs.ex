defmodule ToxicParser.Grammar.Stabs do
  @moduledoc """
  Stab parsing extracted from Containers to keep paren/list/tuple handling focused.
  """

  alias ToxicParser.{Builder, Context, EventLog, Pratt, Precedence, State, TokenAdapter}
  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.{Containers, EOE, Expressions, Keywords, Maps}

  # Stab pattern parsing uses min_bp one higher than stab_op (bp=10) to stop before `->`
  # but allow all other operators including `when` (bp=50) and `<-`/`\` (bp=40).
  # The `when` at the TOP LEVEL of patterns is handled specially after parsing (to extract guards).
  @stab_pattern_min_bp Precedence.stab_op_bp() + 1

  # After leading semicolon: either close paren (empty) or stab content
  def parse_paren_stab_or_empty(
        open_meta,
        %State{} = state,
        %Context{} = ctx,
        %EventLog{} = log,
        min_bp
      ) do
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
  def parse_paren_stab(open_meta, %State{} = state, %Context{} = ctx, %EventLog{} = log, min_bp) do
    with {:ok, items, state, log} <- parse_stab_items_until([], state, ctx, log, :")") do
      state = EOE.skip(state)

      case TokenAdapter.next(state) do
        {:ok, %{kind: :")"} = close_tok, state} ->
          close_meta = token_meta(close_tok.metadata)

          result =
            case check_stab(items) do
              :stab ->
                collect_stab(items)

              _ ->
                unwrap_single_non_stab_with_parens(Enum.reverse(items), open_meta, close_meta)
            end

          Pratt.led(result, state, log, min_bp, ctx)

        {:ok, token, state} ->
          {:error, {:expected, :")", got: token.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  # Unwrap a single non-stab expression from a list with parens metadata
  # Stab clauses are kept as lists: [{:->, ...}]
  # Plain expressions are unwrapped and get parens metadata added (matching Elixir's build_block)
  defp unwrap_single_non_stab_with_parens([{:->, _, _} | _] = stabs, _open_meta, _close_meta) do
    stabs
  end

  # Special case for unary ! and not operators with single argument
  # These get wrapped in __block__ with empty metadata instead of adding parens
  # See elixir_parser.yrl build_paren_stab: ?rearrange_uop(Op) clause
  defp unwrap_single_non_stab_with_parens(
         [{op, _meta, [_single_arg]} = expr],
         _open_meta,
         _close_meta
       )
       when op in [:!, :not] do
    {:__block__, [], [expr]}
  end

  defp unwrap_single_non_stab_with_parens([{:unquote_splicing, _meta, [_]} = single], open_meta, close_meta) do
    {:__block__, Meta.closing_meta(open_meta, close_meta), [single]}
  end

  defp unwrap_single_non_stab_with_parens([{name, meta, args}], open_meta, close_meta)
       when is_list(meta) do
    # Single 3-tuple expression - add parens metadata like Elixir's build_block does
    parens_meta = open_meta ++ [closing: close_meta]
    {name, [parens: parens_meta] ++ meta, args}
  end

  defp unwrap_single_non_stab_with_parens([single], _open_meta, _close_meta), do: single

  # Multiple non-stab expressions - wrap in __block__ with closing metadata
  defp unwrap_single_non_stab_with_parens(exprs, open_meta, close_meta) when is_list(exprs) do
    meta = [closing: close_meta] ++ open_meta
    {:__block__, meta, exprs}
  end

  # Try to parse stab_parens_many: ((args) -> expr) or ((args) when g -> expr)
  def try_parse_stab_parens_many(
        open_meta,
        %State{} = state,
        %Context{} = ctx,
        %EventLog{} = log,
        min_bp,
        fallback_fun
      ) do
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
            inner_state = TokenAdapter.drop_checkpoint(inner_state, ref)
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
            inner_state = TokenAdapter.drop_checkpoint(inner_state, ref)
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
  def try_parse_stab_or_expr(
        _open_meta,
        %State{} = state,
        %Context{} = ctx,
        %EventLog{} = log,
        min_bp,
        fallback_fun
      ) do
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

    case try_parse_stab_clause(checkpoint_state, ctx, log) do
      {:ok, clause, new_state, log} ->
        # Successfully parsed a stab clause, continue with remaining clauses
        # Pass min_bp to continue with led() for trailing operators
        new_state = TokenAdapter.drop_checkpoint(new_state, ref)
        parse_remaining_stab_clauses([clause], new_state, ctx, log, min_bp)

      {:not_stab, _state, _log} ->
        # Not a stab - rewind and parse as regular expression
        state = TokenAdapter.rewind(checkpoint_state, ref)
        fallback_fun.(state, log)

      {:error, reason, state, log} ->
        # Error during stab parsing - this may still be a valid parenthesized expression
        # (e.g. `x when y: z`), so try the expression fallback before returning the error.
        fallback_state = TokenAdapter.rewind(checkpoint_state, ref)

        case fallback_fun.(fallback_state, log) do
          {:ok, _ast, _state, _log} = ok ->
            ok

          _ ->
            {:error, reason, state, log}
        end
    end
  end

  # Parse (() -> expr) - empty paren stab
  defp parse_empty_paren_stab(_open_meta, inner_open_meta, inner_close_meta, state, ctx, log) do
    {:ok, stab_tok, state} = TokenAdapter.next(state)
    stab_base_meta = token_meta(stab_tok.metadata)

    token_newlines = min(Map.get(stab_tok.metadata, :newlines, 0), 1)
    {state, newlines_after} = EOE.skip_newlines_only(state, 0)
    newlines_meta = Meta.newlines_meta(max(token_newlines, newlines_after))

    # Parse body (or empty if just ->)
    with {:ok, body, state, log} <- parse_stab_body(state, ctx, log) do
      # Build stab clause with empty patterns and parens metadata
      parens_meta =
        [parens: Meta.closing_meta(inner_open_meta, inner_close_meta, 0, [], base_first: true)]

      body = wrap_unquote_splicing_body(body)
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

    case TokenAdapter.peek(state) do
      {:ok, tok, state} ->
        if Keywords.starts_kw?(tok) do
          {:error, syntax_error_before(token_meta(tok.metadata), tok.value), state, log}
        else
          # Parse guard expression with min_bp > stab_op (10) to stop before ->
          # Use `expr` context (elixir_parser.yrl: `stab_expr -> empty_paren when_op expr ...`).
          # This allows both do-blocks and `no_parens_expr` (needed for `x when y: z`).
          with {:ok, guard, state, log} <-
                 Pratt.parse_with_min_bp(
                   state,
                   Context.expr(),
                   log,
                   Precedence.stab_op_bp() + 1
                 ) do
            # Expect stab
            case TokenAdapter.next(state) do
              {:ok, %{kind: :stab_op} = stab_tok, state} ->
                stab_base_meta = token_meta(stab_tok.metadata)
                token_newlines = min(Map.get(stab_tok.metadata, :newlines, 0), 1)
                {state, newlines_after} = EOE.skip_newlines_only(state, 0)
                newlines_meta = Meta.newlines_meta(max(token_newlines, newlines_after))

                with {:ok, body, state, log} <- parse_stab_body(state, ctx, log) do
                  # Build guarded stab clause
                  parens_meta =
                    [
                      parens:
                        Meta.closing_meta(
                          inner_open_meta,
                          inner_close_meta,
                          0,
                          [],
                          base_first: true
                        )
                    ]

                  guard_ast = {:when, when_meta, [guard]}

                  clause =
                    {:->, parens_meta ++ newlines_meta ++ stab_base_meta, [[guard_ast], body]}

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

      _ ->
        {:error, :unexpected_eof, state, log}
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
                inner_state = TokenAdapter.drop_checkpoint(inner_state, ref)

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
                inner_state = TokenAdapter.drop_checkpoint(inner_state, ref)

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
    token_newlines = min(Map.get(stab_tok.metadata, :newlines, 0), 1)
    {state, newlines_after} = EOE.skip_newlines_only(state, 0)
    newlines_meta = Meta.newlines_meta(max(token_newlines, newlines_after))

    with {:ok, body, state, log} <- parse_stab_body(state, ctx, log) do
      parens_meta =
        [parens: Meta.closing_meta(inner_open_meta, inner_close_meta, 0, [], base_first: true)]

      stab_meta = newlines_meta ++ stab_base_meta
      # Apply parens meta and unwrap splice for stab patterns
      unwrapped = unwrap_splice(args)
      {final_args, final_stab_meta} = apply_parens_meta(unwrapped, parens_meta, stab_meta)
      body = wrap_unquote_splicing_body(body)
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

    case TokenAdapter.peek(state) do
      {:ok, tok, state} ->
        if Keywords.starts_kw?(tok) do
          # Ambiguity: this can be parsed as the special `when` operator with keyword RHS
          # (not as a stab guard). Example: `((a) when foo: 1 -> ...)`.
          min_bp = Precedence.stab_op_bp() + 1

          with {:ok, kw_list, state, log} <-
                 Keywords.parse_kw_no_parens_call_with_min_bp(state, ctx, log, min_bp) do
            state = EOE.skip(state)

            case TokenAdapter.next(state) do
              {:ok, %{kind: :stab_op} = stab_tok, state} ->
                stab_base_meta = token_meta(stab_tok.metadata)
                token_newlines = min(Map.get(stab_tok.metadata, :newlines, 0), 1)
                {state, newlines_after} = EOE.skip_newlines_only(state, 0)
                newlines_meta = Meta.newlines_meta(max(token_newlines, newlines_after))

                with {:ok, body, state, log} <- parse_stab_body(state, ctx, log) do
                  parens_meta =
                    [
                      parens:
                        Meta.closing_meta(
                          inner_open_meta,
                          inner_close_meta,
                          0,
                          [],
                          base_first: true
                        )
                    ]

                  stab_meta = newlines_meta ++ stab_base_meta
                  unwrapped = unwrap_splice(args)

                  {final_args, final_stab_meta} =
                    apply_parens_meta(unwrapped, parens_meta, stab_meta)

                  left = List.last(final_args)
                  when_ast = {:when, when_meta, [left, kw_list]}
                  final_args = List.replace_at(final_args, -1, when_ast)

                  body = wrap_unquote_splicing_body(body)
                  clause = {:->, final_stab_meta, [final_args, body]}
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
        else
          # Parse guard expression with min_bp > stab_op (10) to stop before ->
          # Use `expr` context (elixir_parser.yrl: `stab_expr -> stab_parens_many when_op expr ...`).
          # This allows both do-blocks and `no_parens_expr`.
          with {:ok, guard, state, log} <-
                 Pratt.parse_with_min_bp(
                   state,
                   Context.expr(),
                   log,
                   Precedence.stab_op_bp() + 1
                 ) do
            case TokenAdapter.next(state) do
              {:ok, %{kind: :stab_op} = stab_tok, state} ->
                stab_base_meta = token_meta(stab_tok.metadata)
                token_newlines = min(Map.get(stab_tok.metadata, :newlines, 0), 1)
                {state, newlines_after} = EOE.skip_newlines_only(state, 0)
                newlines_meta = Meta.newlines_meta(max(token_newlines, newlines_after))

                with {:ok, body, state, log} <- parse_stab_body(state, ctx, log) do
                  parens_meta =
                    [
                      parens:
                        Meta.closing_meta(
                          inner_open_meta,
                          inner_close_meta,
                          0,
                          [],
                          base_first: true
                        )
                    ]

                  stab_meta = newlines_meta ++ stab_base_meta
                  # Apply parens meta and unwrap splice for stab patterns
                  unwrapped = unwrap_splice(args)

                  {final_args, final_stab_meta} =
                    apply_parens_meta(unwrapped, parens_meta, stab_meta)

                  guard_ast = {:when, when_meta, final_args ++ [guard]}
                  body = wrap_unquote_splicing_body(body)
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

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse arguments inside stab_parens_many: call_args_no_parens_kw or call_args_no_parens_many
  defp parse_stab_parens_args(state, ctx, log) do
    # Stop keyword values before `->` (but allow `when` inside values, including `when` with kw RHS).
    min_bp = Precedence.stab_op_bp() + 1

    case Keywords.try_parse_call_args_no_parens_kw(state, ctx, log, min_bp: min_bp) do
      {:ok, kw_list, state, log} ->
        {:ok, [kw_list], state, log}

      {:no_kw, state, log} ->
        parse_stab_parens_exprs([], state, ctx, log)

      {:error, reason, state, log} ->
        {:error, reason, state, log}
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
            # Check for keyword list tail after comma.
            # Stop keyword values before `->`, but allow nested `when` inside values.
            min_bp = Precedence.stab_op_bp() + 1

            case Keywords.try_parse_call_args_no_parens_kw(state, Context.matched_expr(), log,
                   min_bp: min_bp
                 ) do
              {:ok, kw_list, state, log} ->
                {:ok, Enum.reverse(acc) ++ [expr, kw_list], state, log}

              {:no_kw, state, log} ->
                parse_stab_parens_exprs([expr | acc], state, Context.matched_expr(), log)

              {:error, reason, state, log} ->
                {:error, reason, state, log}
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
    case Pratt.parse_with_min_bp(
           state,
           Context.matched_expr(),
           log,
           Precedence.when_op_bp() + 1
         ) do
      {:ok, ast, state, log} ->
        {:ok, ast, state, log}

      # Handle quoted keyword key - parse value and return as keyword pair
      {:keyword_key, key_atom, state, log} ->
        state = EOE.skip(state)

        with {:ok, value_ast, state, log} <-
               Pratt.parse_with_min_bp(
                 state,
                 Context.matched_expr(),
                 log,
                 Precedence.when_op_bp() + 1
               ) do
          {:ok, [{key_atom, value_ast}], state, log}
        end

      {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, log} ->
        state = EOE.skip(state)

        with {:ok, value_ast, state, log} <-
               Pratt.parse_with_min_bp(
                 state,
                 Context.matched_expr(),
                 log,
                 Precedence.when_op_bp() + 1
               ) do
          key_ast = Expressions.build_interpolated_keyword_key(parts, kind, start_meta, delimiter)
          {:ok, [{key_ast, value_ast}], state, log}
        end

      {:error, reason, state, log} ->
        {:error, reason, state, log}
    end
  end

  # Try to parse a stab clause, returning {:not_stab, ...} if it's not a stab
  # Default terminator is :) for paren stabs
  def try_parse_stab_clause(%State{} = state, %Context{} = ctx, %EventLog{} = log) do
    try_parse_stab_clause(state, ctx, log, :")")
  end

  @doc """
  Try to parse a stab clause with custom terminator.
  Returns {:ok, clause, state, log}, {:not_stab, state, log}, or {:error, ...}
  Exported for use by fn parsing.
  """
  def try_parse_stab_clause(%State{} = state, %Context{} = ctx, %EventLog{} = log, terminator) do
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
        # Newlines metadata for `->` comes from the EOE after the operator (stab_op_eol)
        # and from the token itself when `->` starts on a new line.
        token_newlines = min(Map.get(stab_tok.metadata, :newlines, 0), 1)
        {state, newlines_after} = EOE.skip_newlines_only(state, 0)
        newlines = max(token_newlines, newlines_after)

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

          body = wrap_unquote_splicing_body(body)
          clause = {:->, final_stab_meta, [final_patterns, body]}
          {:ok, clause, state, log}
        end

      {:ok, %{kind: :when_op} = when_tok, _} ->
        # Pattern followed by guard then stab
        {:ok, _when, state} = TokenAdapter.next(state)
        when_meta = token_meta(when_tok.metadata)
        state = EOE.skip(state)

        # If the next token is a keyword pair starter (`y:`), this cannot be a stab guard.
        # It's the `when` operator with a keyword list RHS (see `no_parens_op_expr`).
        case TokenAdapter.peek(state) do
          {:ok, tok, _} ->
            if Keywords.starts_kw?(tok) do
              cond do
                patterns == [] ->
                  # `when foo: 1` has no LHS for the `when` operator, and `foo: 1` is not a valid
                  # guard expression on its own. Match Elixir's "syntax error before: foo".
                  {:error, syntax_error_before(token_meta(tok.metadata), tok.value), state, log}

                true ->
                  # This may be the special `when` operator form that allows a keyword list RHS:
                  # `no_parens_op_expr -> when_op_eol call_args_no_parens_kw`.
                  #
                  # If it is followed by `->`, treat it as part of the patterns (not a guard).
                  min_bp = Precedence.stab_op_bp() + 1

                  with {:ok, kw_list, state, log} <-
                         Keywords.parse_kw_no_parens_call_with_min_bp(state, ctx, log, min_bp) do
                    state = EOE.skip(state)

                    case TokenAdapter.peek(state) do
                      {:ok, %{kind: :stab_op}, _} ->
                        {patterns, parens_meta} =
                          if length(patterns) == 1 and parens_meta != [] do
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
                          ctx,
                          log,
                          terminator
                        )

                      _ ->
                        {:not_stab, state, log}
                    end
                  end
              end
            else
              # Parse guard expression with min_bp > stab_op (10) to stop before ->
              # Use `expr` context (elixir_parser.yrl: `... when_op expr stab_op ...`).
              with {:ok, guard, state, log} <-
                     Pratt.parse_with_min_bp(
                       state,
                       Context.expr(),
                       log,
                       Precedence.stab_op_bp() + 1
                     ) do
                case TokenAdapter.next(state) do
                  {:ok, %{kind: :stab_op} = stab_tok, state} ->
                    stab_base_meta = token_meta(stab_tok.metadata)
                    # Newlines metadata for `->` comes from the EOE after the operator (stab_op_eol)
                    # and from the token itself when `->` starts on a new line.
                    token_newlines = min(Map.get(stab_tok.metadata, :newlines, 0), 1)
                    {state, newlines_after} = EOE.skip_newlines_only(state, 0)
                    newlines = max(token_newlines, newlines_after)

                    stab_meta =
                      if newlines > 0,
                        do: [newlines: newlines] ++ stab_base_meta,
                        else: stab_base_meta

                    with {:ok, body, state, log} <- parse_stab_body(state, ctx, log, terminator) do
                      # unwrap_splice for stab clause patterns
                      unwrapped = unwrap_splice(patterns)
                      # Apply parens meta before wrapping with guard
                      {final_patterns, final_stab_meta} =
                        apply_parens_meta(unwrapped, parens_meta, stab_meta)

                      # Wrap patterns with guard in a when clause
                      patterns_with_guard = unwrap_when(final_patterns, guard, when_meta)
                      body = wrap_unquote_splicing_body(body)
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
            end

          _ ->
            # TokenAdapter.peek should not return eof here (terminator inserted), but be defensive.
            {:not_stab, state, log}
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
    # 1 pattern - parens go on the pattern if it's a 3-tuple with metadata.
    # For keyword lists (plain lists), parens go on the stab arrow.
    # For literals (no metadata carrier), Elixir drops the parens.
    case single_pattern do
      {_name, meta, _args} when is_list(meta) ->
        pattern_with_parens = add_parens_to_pattern(single_pattern, parens_meta)
        {[pattern_with_parens], stab_meta}

      _ when is_list(single_pattern) ->
        {[single_pattern], parens_meta ++ stab_meta}

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
                state2 = TokenAdapter.drop_checkpoint(state2, ref)
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

    # Skip only newlines inside parens (NOT semicolons).
    # This ensures `(; )` is parsed as a parenthesized empty block pattern, not as `fn () ->`.
    {state, _newlines} = EOE.skip_newlines_only(state, 0)

    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, value: %{source: :semicolon}}, _} ->
        # Force fallback to regular parenthesized-expression parsing.
        {:error, :paren_semicolon, state, log}

      _ ->
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
  end

  # Parse call_args_parens: comma-separated expressions, optionally followed by keywords
  defp parse_call_args_parens(state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        # TODO: no coverage?
        if Keywords.starts_kw?(tok) do
          # Just keywords: fn (x: 1) -> body end
          with {:ok, kw_list, state, log} <- Keywords.parse_kw_no_parens_call(state, ctx, log) do
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
    with {:ok, expr, state, log} <- Expressions.expr(state, Context.matched_expr(), log) do
      {expr, state} =
        case TokenAdapter.peek(state) do
          # Only annotate trailing newline EOE when this is a single-arg paren group.
          # For multi-arg groups like `(a, b\n)`, Elixir does NOT annotate the last arg.
          {:ok, %{kind: :eoe, value: %{source: source}} = eoe_tok, _} when source != :semicolon ->
            eoe_meta = EOE.build_eoe_meta(eoe_tok)
            {state, _newlines} = EOE.skip_newlines_only(state, 0)

            if acc == [] and match?({:ok, %{kind: :")"}, _}, TokenAdapter.peek(state)) do
              {EOE.annotate_eoe(expr, eoe_meta), state}
            else
              {expr, state}
            end

          _ ->
            {expr, state}
        end

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
                  with {:ok, kw_list, state, log} <- Keywords.parse_kw_no_parens_call(state, ctx, log) do
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
    case Expressions.expr(state, Context.matched_expr(), log) do
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
                           Context.matched_expr(),
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
                  parse_stab_pattern_exprs([expr | acc], state, Context.matched_expr(), log)
              end

            _ ->
              parse_stab_pattern_exprs([expr | acc], state, Context.matched_expr(), log)
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
                             Context.matched_expr(),
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
        with {:ok, ast, state, log} <-
               Containers.parse_container_base(state, Context.expr(), log) do
          # Continue with led to handle trailing operators
          Pratt.led(ast, state, log, @stab_pattern_min_bp, Context.expr())
        end

      # Map literal %{} or struct %Name{}
      {:ok, %{kind: kind}, _} when kind in [:%{}, :%] ->
        Maps.parse_map(state, Context.expr(), log, @stab_pattern_min_bp)

      # Other tokens - use Pratt parser with min_bp to stop before ->
      _ ->
        case Pratt.parse_with_min_bp(
               state,
               Context.expr(),
               log,
               @stab_pattern_min_bp
             ) do
          {:ok, ast, state, log} ->
            {:ok, ast, state, log}

          # Handle quoted keyword key - parse value and return as keyword pair
          {:keyword_key, key_atom, state, log} ->
            state = EOE.skip(state)

            with {:ok, value_ast, state, log} <-
                   Pratt.parse_with_min_bp(
                     state,
                     Context.expr(),
                     log,
                     @stab_pattern_min_bp
                   ) do
              {:ok, [{key_atom, value_ast}], state, log}
            end

          {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, log} ->
            state = EOE.skip(state)

            with {:ok, value_ast, state, log} <-
                   Pratt.parse_with_min_bp(
                     state,
                     Context.expr(),
                     log,
                     @stab_pattern_min_bp
                   ) do
              key_ast =
                Expressions.build_interpolated_keyword_key(parts, kind, start_meta, delimiter)

              {:ok, [{key_ast, value_ast}], state, log}
            end

          {:error, reason, state, log} ->
            {:error, reason, state, log}
        end
    end
  end

  # Parse the body of a stab clause (single expr or nil if empty)
  # Default terminator is :) for paren stabs
  def parse_stab_body(%State{} = state, %Context{} = ctx, %EventLog{} = log) do
    parse_stab_body(state, ctx, log, :")")
  end

  @doc """
  Parse stab clause body with custom terminator.

  YRL-aligned: a stab clause body is a single `expr` (or omitted / `nil`).
  Multi-expression bodies are handled later via `collect_stab/1`.
  """
  def parse_stab_body(%State{} = state, %Context{}, %EventLog{} = log, terminator) do
    body_ctx = Context.expr()

    case TokenAdapter.peek(state) do
      {:ok, %{kind: ^terminator}, _} ->
        {:ok, nil, state, log}

      {:ok, %{kind: :block_identifier}, _} when terminator == :end ->
        {:ok, nil, state, log}

      {:ok, %{kind: :eoe}, _} ->
        # `-> ;` / `-> \n` (newlines are normally already skipped by stab_op_eol)
        {:ok, nil, state, log}

      {:ok, _, _} ->
        Expressions.expr(state, body_ctx, log)

      {:eof, state} ->
        {:ok, nil, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  @doc """
  Parse stab items (clauses or plain expressions) separated by EOE until `terminator` or stop token.

  Returns items in reverse source order (latest first), matching `collect_stab/1` input.
  """
  @spec parse_stab_items_until(list(), State.t(), Pratt.context(), EventLog.t(), atom(), [atom()]) ::
          {:ok, [Macro.t()], State.t(), EventLog.t()} | {:error, term(), State.t(), EventLog.t()}
  def parse_stab_items_until(acc, %State{} = state, %Context{} = ctx, %EventLog{} = log, terminator, stop_kinds \\ []) do
    state = EOE.skip(state)

    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if stop_token?(tok, terminator, stop_kinds) do
          {:ok, acc, state, log}
        else
          {kind, state} = classify_stab_item(state, terminator, stop_kinds)

          case kind do
            :clause ->
              case try_parse_stab_clause(state, ctx, log, terminator) do
                {:ok, clause, state, log} ->
                  {clause, state} = maybe_annotate_and_consume_eoe(clause, state)
                  parse_stab_items_until([clause | acc], state, ctx, log, terminator, stop_kinds)

                {:not_stab, _state, log} ->
                  with {:ok, expr, state, log} <-
                         Pratt.parse_with_min_bp(state, Context.expr(), log, Precedence.stab_op_bp() + 1) do
                    {expr, state} = maybe_annotate_and_consume_eoe(expr, state)
                    parse_stab_items_until([expr | acc], state, ctx, log, terminator, stop_kinds)
                  end

                {:error, reason, state, log} ->
                  {:error, reason, state, log}
              end

            :unknown ->
              {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

              case try_parse_stab_clause(checkpoint_state, ctx, log, terminator) do
                {:ok, clause, state, log} ->
                  state = TokenAdapter.drop_checkpoint(state, ref)
                  {clause, state} = maybe_annotate_and_consume_eoe(clause, state)
                  parse_stab_items_until([clause | acc], state, ctx, log, terminator, stop_kinds)

                {:not_stab, _state, log} ->
                  state = TokenAdapter.rewind(checkpoint_state, ref)

                  with {:ok, expr, state, log} <-
                         Pratt.parse_with_min_bp(state, Context.expr(), log, Precedence.stab_op_bp() + 1) do
                    {expr, state} = maybe_annotate_and_consume_eoe(expr, state)
                    parse_stab_items_until([expr | acc], state, ctx, log, terminator, stop_kinds)
                  end

                {:error, reason, state, log} ->
                  {:error, reason, state, log}
              end

            _ ->
              # Parse as plain expr, stopping before `->`.
              with {:ok, expr, state, log} <-
                     Pratt.parse_with_min_bp(state, Context.expr(), log, Precedence.stab_op_bp() + 1) do
                {expr, state} = maybe_annotate_and_consume_eoe(expr, state)
                parse_stab_items_until([expr | acc], state, ctx, log, terminator, stop_kinds)
              end
          end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp maybe_annotate_and_consume_eoe(expr, state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe} = eoe_tok, _} ->
        eoe_meta = EOE.build_eoe_meta(eoe_tok)

        annotated =
          case expr do
            {:->, meta, [pats, {:__block__, [], [{:unquote_splicing, _, [_]} = u]}]} ->
              u = EOE.annotate_eoe(u, eoe_meta)
              {:->, meta, [pats, {:__block__, [], [u]}]}

            {:->, meta, [pats, body]} ->
              {:->, meta, [pats, EOE.annotate_eoe(body, eoe_meta)]}

            _ ->
              EOE.annotate_eoe(expr, eoe_meta)
          end

        {:ok, _eoe, state} = TokenAdapter.next(state)
        {annotated, EOE.skip(state)}

      _ ->
        {expr, state}
    end
  end

  defp stop_token?(%{kind: kind} = tok, terminator, stop_kinds) do
    kind == terminator or kind in stop_kinds or block_label?(tok)
  end

  defp block_label?(%{kind: :block_identifier, value: value}),
    do: value in [:else, :catch, :rescue, :after]

  defp block_label?(%{kind: kind}) when kind in [:else, :catch, :rescue, :after], do: true
  defp block_label?(_), do: false

  defp classify_stab_item(%State{} = state, terminator, stop_kinds) do
    ctx = %{delim: 0, block: 0, open?: true, percent_pending?: false}
    scan_classify(state, [], ctx, terminator, stop_kinds, 200, false)
  end

  defp scan_classify(state, consumed, _ctx, _terminator, _stop_kinds, max, _boundary?) when max <= 0 do
    {:unknown, TokenAdapter.pushback_many(state, Enum.reverse(consumed))}
  end

  defp scan_classify(state, consumed, ctx, terminator, stop_kinds, max, boundary?) do
    case TokenAdapter.next(state) do
      {:ok, tok, state2} ->
        top? = ctx.delim == 0 and ctx.block == 0

        cond do
          top? and tok.kind == :stab_op ->
            {:clause, TokenAdapter.pushback_many(state2, Enum.reverse([tok | consumed]))}

          top? and boundary? and tok.kind == :eoe ->
            scan_classify(state2, [tok | consumed], ctx, terminator, stop_kinds, max - 1, true)

          top? and boundary? ->
            {:expr, TokenAdapter.pushback_many(state2, Enum.reverse([tok | consumed]))}

          top? and stop_token?(tok, terminator, stop_kinds) ->
            {:expr, TokenAdapter.pushback_many(state2, Enum.reverse([tok | consumed]))}

          top? and tok.kind == :eoe and tok.value.source == :semicolon ->
            {:expr, TokenAdapter.pushback_many(state2, Enum.reverse([tok | consumed]))}

          top? and tok.kind == :eoe and ctx.open? == false ->
            scan_classify(state2, [tok | consumed], ctx, terminator, stop_kinds, max - 1, true)

          top? and tok.kind == :eoe ->
            scan_classify(state2, [tok | consumed], ctx, terminator, stop_kinds, max - 1, boundary?)

          tok.kind == :error_token ->
            {:unknown, TokenAdapter.pushback_many(state2, Enum.reverse([tok | consumed]))}

          true ->
            ctx2 = scan_update_ctx(ctx, tok)
            scan_classify(state2, [tok | consumed], ctx2, terminator, stop_kinds, max - 1, false)
        end

      {:eof, state2} ->
        {:expr, TokenAdapter.pushback_many(state2, Enum.reverse(consumed))}

      {:error, _diag, state2} ->
        {:unknown, TokenAdapter.pushback_many(state2, Enum.reverse(consumed))}
    end
  end

  defp scan_update_ctx(ctx, tok) do
    ctx
    |> scan_update_delims(tok)
    |> scan_update_percent(tok)
    |> Map.put(:open?, scan_open?(tok, ctx))
  end

  defp scan_update_percent(%{percent_pending?: true} = ctx, %{kind: :"{"}), do: %{ctx | percent_pending?: false}
  defp scan_update_percent(ctx, %{kind: :%}), do: %{ctx | percent_pending?: true}
  defp scan_update_percent(ctx, _), do: ctx

  defp scan_update_delims(%{delim: d, block: b} = ctx, %{kind: kind}) do
    ctx =
      case kind do
        kind when kind in [:"(", :"[", :"{", :"<<"] ->
          %{ctx | delim: d + 1}

        kind when kind in [:")", :"]", :"}", :">>"] ->
          %{ctx | delim: max(d - 1, 0)}

        :do ->
          %{ctx | block: b + 1}

        :fn ->
          %{ctx | block: b + 1}

        :end when b > 0 ->
          %{ctx | block: b - 1}

        _ ->
          ctx
      end

    ctx
  end

  defp scan_open?(%{kind: kind}, %{percent_pending?: true}) when kind in [:identifier, :alias], do: true

  defp scan_open?(%{kind: kind}, _ctx) do
    kind in [:",", :kw_identifier, :"(", :"[", :"{", :"<<", :%] or
      Precedence.binary(kind) != nil or
      Precedence.unary(kind) != nil
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
  defp build_block(items), do: Builder.Helpers.literal({:__block__, [], items})

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
  @spec parse_stab_eoe_until(list(), State.t(), Pratt.context(), EventLog.t(), atom()) ::
          {:ok, [Macro.t()], State.t(), EventLog.t()} | {:error, term(), State.t(), EventLog.t()}
  def parse_stab_eoe_until(acc, %State{} = state, %Context{} = ctx, %EventLog{} = log, terminator) do
    with {:ok, items, state, log} <- parse_stab_items_until(acc, state, ctx, log, terminator) do
      case check_stab(items) do
        :stab ->
          {:ok, collect_stab(items), state, log}

        _ ->
          {:error, {:expected, :stab_op, got: :expression}, state, log}
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

  # Build a block from multiple expressions, or return single expression as-is
  defp build_stab_block([single]), do: single

  defp build_stab_block(exprs) when is_list(exprs) do
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

  defp token_meta(meta), do: Builder.Helpers.token_meta(meta)

  #   %% an arg style call. unwrap_splice unwraps the splice
  defp unwrap_splice([{:__block__, _, [{:unquote_splicing, _, _}] = splice}]), do: splice
  defp unwrap_splice(other), do: other
end
