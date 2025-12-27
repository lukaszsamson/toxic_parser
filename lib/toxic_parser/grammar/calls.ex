defmodule ToxicParser.Grammar.Calls do
  @moduledoc """
  Call parsing skeleton for Phase 5.

  This module classifies identifiers and dispatches to paren/no-parens call
  parsers. Full call argument parsing and ambiguity handling will be filled in
  subsequent iterations.
  """

  alias ToxicParser.{
    Builder,
    Context,
    Cursor,
    EventLog,
    ExprClass,
    Identifiers,
    NoParens,
    NoParensErrors,
    Pratt,
    Result,
    State,
    TokenAdapter
  }

  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.{Brackets, DoBlocks, EOE, Expressions, Keywords}

  @type result ::
          {:ok, Macro.t(), State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}
          | {:keyword_key, atom(), keyword(), State.t(), Cursor.t(), EventLog.t()}
          | {:keyword_key_interpolated, list(), atom(), keyword(), String.t(), State.t(), Cursor.t(),
             EventLog.t()}

  @doc """
  Parses a call-or-identifier expression. Currently falls back to Pratt for
  non-call forms and wraps bare identifiers as AST.
  """
  @spec parse(State.t(), Cursor.t(), Pratt.context(), EventLog.t()) :: result()
  def parse(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log) do
    case TokenAdapter.peek(state, cursor) do
      {:ok, tok, _, cursor} ->
        case Identifiers.classify(TokenAdapter.kind(tok)) do
          :other ->
            Pratt.parse(state, cursor, ctx, log)

          ident_kind ->
            {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)
            parse_identifier(ident_kind, tok, state, cursor, ctx, log)
        end

      {:eof, state, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, state, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  defp parse_identifier(kind, tok, state, cursor, ctx, log) do
    case TokenAdapter.peek(state, cursor) do
      # Only treat as paren call if the identifier was :paren_identifier
      # (tokenized without space before the paren, e.g., "foo(a)" not "foo (a)")
      # Plain :identifier followed by ( means the ( is a separate parenthesized expression
      {:ok, next_tok, _, cursor} when elem(next_tok, 0) == :"(" and kind == :paren_identifier ->
        parse_paren_call(tok, state, cursor, ctx, log)

      {:ok, open_tok, _, cursor} when elem(open_tok, 0) == :"[" and kind == :bracket_identifier ->
        # bracket_identifier followed by [ is bracket access: foo[:bar]
        # Grammar: bracket_expr -> dot_bracket_identifier bracket_arg
        parse_bracket_access(tok, open_tok, state, cursor, ctx, log)

      # Alias followed by [ is bracket access: A[d]
      # Grammar: bracket_expr -> access_expr bracket_arg
      # Push back the alias token and let Pratt.parse handle it (led_bracket will process [)
      {:ok, next_tok, _, cursor} when elem(next_tok, 0) == :"[" and kind == :alias ->
        {state, cursor} = TokenAdapter.pushback(state, cursor, tok)
        Pratt.parse(state, cursor, ctx, log)

      {:ok, next_tok, _, cursor} ->
        next_kind = TokenAdapter.kind(next_tok)

        cond do
          # op_identifier means tokenizer determined this is a no-parens call
          # with a unary expression argument (e.g., "a -1" where - is unary)
          kind == :op_identifier ->
            parse_op_identifier_call(tok, state, cursor, ctx, log)

          # Binary operator or dot operator follows - let Pratt handle expression
          Pratt.bp(next_kind) != nil or next_kind in [:., :dot_call_op] ->
            {state, cursor} = TokenAdapter.pushback(state, cursor, tok)
            Pratt.parse(state, cursor, ctx, log)

          # Could be no-parens call argument - parse the call
          # This must come BEFORE the do_identifier check below, because
          # `if a do :ok end` should parse `if` with arg `a` and do-block
          NoParens.can_start_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok) ->
            parse_no_parens_call(tok, state, cursor, ctx, log)

          # do_identifier in matched context with no arguments: this identifier
          # precedes a do-block but the do-block belongs to an outer call.
          # Just return the bare identifier.
          kind == :do_identifier and not Context.allow_do_block?(ctx) ->
            ast = Builder.Helpers.from_token(tok)
            {:ok, ast, state, cursor, log}

          # Just a bare identifier - may have trailing do-block
          true ->
            ast = Builder.Helpers.from_token(tok)

            DoBlocks.maybe_do_block(ast, state, cursor, ctx, log,
              after: fn ast, state, cursor, log ->
                Pratt.led(ast, state, cursor, log, 0, ctx)
              end
            )
        end

      # No next token or at terminator - check for do-block
      _ ->
        ast = Builder.Helpers.from_token(tok)

        DoBlocks.maybe_do_block(ast, state, cursor, ctx, log,
          after: fn ast, state, cursor, log ->
            Pratt.led(ast, state, cursor, log, 0, ctx)
          end
        )
    end
  end

  # Parse op_identifier call like "a -1" or "a -1, 2" where the first argument starts with a unary op
  # The tokenizer has determined this is a call, not a binary expression
  # Grammar: no_parens_many_expr -> op_identifier call_args_no_parens_ambig
  #          no_parens_one_ambig -> op_identifier call_args_no_parens_ambig
  defp parse_op_identifier_call(callee_tok, state, cursor, ctx, log) do
    # Parse first arg as no_parens_expr (elixir_parser.yrl: call_args_no_parens_ambig -> no_parens_expr)
    with {:ok, first_arg, state, cursor, log} <- Expressions.expr(state, cursor, Context.no_parens_expr(), log) do
      # Check for comma to see if there are more args
      case TokenAdapter.peek(state, cursor) do
        {:ok, comma_tok, _, cursor} when elem(comma_tok, 0) == :"," ->
          # Multiple args - continue parsing
          {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)

          with {:ok, args, state, cursor, log} <- parse_no_parens_args([first_arg], state, cursor, ctx, log) do
            callee = TokenAdapter.value(callee_tok)
            # No ambiguous_op metadata when multiple args
            meta = TokenAdapter.token_meta(callee_tok)
            ast = {callee, meta, args}

            DoBlocks.maybe_do_block(ast, state, cursor, ctx, log,
              after: fn ast, state, cursor, log ->
                Pratt.led(ast, state, cursor, log, 0, ctx)
              end
            )
          end

        _ ->
          # Single arg - add ambiguous_op: nil metadata
          callee = TokenAdapter.value(callee_tok)
          meta = [ambiguous_op: nil] ++ TokenAdapter.token_meta(callee_tok)
          ast = {callee, meta, [first_arg]}

          DoBlocks.maybe_do_block(ast, state, cursor, ctx, log,
            after: fn ast, state, cursor, log ->
              Pratt.led(ast, state, cursor, log, 0, ctx)
            end
          )
      end
    end
  end

  # Parse bracket access expression: foo[:bar]
  # Grammar: bracket_expr -> dot_bracket_identifier bracket_arg
  # Produces: {{:., meta, [Access, :get]}, meta, [subject, key]}
  defp parse_bracket_access(ident_tok, _open_tok, state, cursor, ctx, log) do
    # Consume the opening bracket
    {:ok, open_tok, state, cursor} = TokenAdapter.next(state, cursor)

    # Skip leading EOE and count newlines (only leading newlines matter for metadata)
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    # Parse the bracket argument (container_expr or kw_data)
    with {:ok, arg, state, cursor, log} <- parse_bracket_arg_no_skip(state, cursor, ctx, log) do
      # Skip trailing EOE before close bracket (don't count these)
      {state, cursor, _trailing_newlines} = EOE.skip_count_newlines(state, cursor, 0)

      case expect_token(state, cursor, :"]") do
        {:ok, close_tok, state, cursor} ->
          # Build access expression
          subject = Builder.Helpers.from_token(ident_tok)
          bracket_meta = build_bracket_meta_with_newlines(open_tok, close_tok, leading_newlines)
          ast = build_access(subject, arg, bracket_meta)
          # Continue with led() to handle chained access like foo[:a][:b]
          Pratt.led(ast, state, cursor, log, 0, ctx)

        other ->
          Result.normalize_error(other, cursor, log)
      end
    end
  end

  # Parse bracket_arg: the content inside [...]
  # Grammar:
  #   bracket_arg -> open_bracket kw_data close_bracket
  #   bracket_arg -> open_bracket container_expr close_bracket
  #   bracket_arg -> open_bracket container_expr ',' close_bracket
  # Note: caller handles EOE skipping before calling this
  defp parse_bracket_arg_no_skip(state, cursor, _ctx, log) do
    Brackets.parse_bracket_arg_no_skip(state, cursor, log)
  end

  # Build metadata for bracket access with newlines
  defp build_bracket_meta_with_newlines(open_tok, close_tok, newlines) do
    open_meta = TokenAdapter.token_meta(open_tok)
    close_meta = TokenAdapter.token_meta(close_tok)
    Meta.closing_meta(open_meta, close_meta, newlines, from_brackets: true)
  end

  # Build Access.get call AST
  defp build_access(subject, key, meta) do
    {{:., meta, [Access, :get]}, meta, [subject, key]}
  end

  # Parse a no-parens call like `foo 1`, `foo 1, 2`, or `foo a: 1`.
  defp parse_no_parens_call(callee_tok, state, cursor, ctx, log) do
    with {:ok, args, state, cursor, log} <- parse_no_parens_args([], state, cursor, ctx, log) do
      callee = TokenAdapter.value(callee_tok)
      meta = TokenAdapter.token_meta(callee_tok)
      ast = {callee, meta, args}

      DoBlocks.maybe_do_block(ast, state, cursor, ctx, log,
        after: fn ast, state, cursor, log ->
          Pratt.led(ast, state, cursor, log, 0, ctx)
        end
      )
    end
  end

  defp parse_paren_call(callee_tok, state, cursor, ctx, log) do
    {:ok, _open_tok, state, cursor} = TokenAdapter.next(state, cursor)

    # Skip leading EOE and count newlines
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    with {:ok, args, state, cursor, log} <- parse_paren_args([], state, cursor, ctx, log),
         {:ok, close_meta, trailing_newlines, state, cursor} <- Meta.consume_closing(state, cursor, :")") do
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      callee_meta = TokenAdapter.token_meta(callee_tok)
      meta = Meta.closing_meta(callee_meta, close_meta, total_newlines)

      ast = {TokenAdapter.value(callee_tok), meta, Enum.reverse(args)}
      # Check for nested call: foo()() - another paren call on the result
      maybe_nested_call(ast, state, cursor, ctx, log)
    else
      other -> Result.normalize_error(other, cursor, log)
    end
  end

  # Handle nested paren calls: foo()(), foo(1)(2), etc.
  # Rule: parens_call -> dot_call_identifier call_args_parens call_args_parens
  defp maybe_nested_call(ast, state, cursor, ctx, log) do
    case TokenAdapter.peek(state, cursor) do
      {:ok, tok, _, cursor} when elem(tok, 0) == :"(" ->
        parse_nested_paren_call(ast, state, cursor, ctx, log)

      _ ->
        DoBlocks.maybe_do_block(ast, state, cursor, ctx, log,
          after: fn ast, state, cursor, log ->
            Pratt.led(ast, state, cursor, log, 0, ctx)
          end
        )
    end
  end

  defp parse_nested_paren_call(callee_ast, state, cursor, ctx, log) do
    {:ok, _open_tok, state, cursor} = TokenAdapter.next(state, cursor)

    # Skip leading EOE and count newlines
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    with {:ok, args, state, cursor, log} <- parse_paren_args([], state, cursor, ctx, log),
         {:ok, close_meta, trailing_newlines, state, cursor} <- Meta.consume_closing(state, cursor, :")") do
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      callee_meta = extract_meta(callee_ast)
      meta = Meta.closing_meta(callee_meta, close_meta, total_newlines)

      ast = {callee_ast, meta, Enum.reverse(args)}
      # Recurse for chained calls like foo()()()
      maybe_nested_call(ast, state, cursor, ctx, log)
    else
      other -> Result.normalize_error(other, cursor, log)
    end
  end

  defp extract_meta({_name, meta, _args}) when is_list(meta) do
    Keyword.take(meta, [:line, :column])
  end

  defp expect_token(state, cursor, kind) do
    case TokenAdapter.next(state, cursor) do
      {:ok, token, state, cursor} when elem(token, 0) == kind -> {:ok, token, state, cursor}
      {:ok, token, state, cursor} -> {:error, {:expected, kind, got: TokenAdapter.kind(token)}, state, cursor}
      {:eof, state, cursor} -> {:error, :unexpected_eof, state, cursor}
      {:error, diag, state, cursor} -> {:error, diag, state, cursor}
    end
  end

  defp parse_paren_args(acc, state, cursor, ctx, log),
    do: ToxicParser.Grammar.CallsPrivate.parse_paren_args(acc, state, cursor, ctx, log)

  @doc """
  Parse no-parens call arguments (exported for use by Pratt parser for dot calls).
  Optional `min_bp` parameter (default 0) stops argument parsing before operators
  with binding power < min_bp. This is essential for proper precedence handling
  in contexts like stab clauses where we must stop before `->`.
  """
  def parse_no_parens_args(
        acc,
        %State{} = state,
        cursor,
        %Context{} = ctx,
        %EventLog{} = log,
        min_bp \\ 0,
        opts \\ []
      ) do
    case TokenAdapter.peek(state, cursor) do
      {:ok, tok, _, cursor} ->
        kind = TokenAdapter.kind(tok)

        case kind do
          k when k in [:eol, :";", :")", :"]", :"}", :do] ->
            {:ok, Enum.reverse(acc), state, cursor, log}

          _ ->
            # Check if we should stop at this operator due to min_bp.
            # BUT only for pure binary operators - tokens like dual_op can be unary,
            # so they should be allowed to start no-parens arguments.
            # The stab_op check is now handled by excluding it from led() entirely.
            # Some tokens have a binary bp but are also valid as standalone/nullary expressions
            # (elixir_parser.yrl: sub_matched_expr -> range_op | ellipsis_op).
            can_be_unary =
              kind in [
                :dual_op,
                :unary_op,
                :at_op,
                :capture_op,
                :ternary_op,
                :range_op,
                :ellipsis_op
              ]

            case {can_be_unary, Pratt.bp(kind)} do
              {false, bp} when is_integer(bp) and bp < min_bp ->
                # Stop before this pure binary operator (e.g., -> is now handled elsewhere)
                {:ok, Enum.reverse(acc), state, cursor, log}

              _ ->
                case Keywords.try_parse_call_args_no_parens_kw(state, cursor, ctx, log) do
                  {:ok, kw_list, state, cursor, log} ->
                    {:ok, Enum.reverse([kw_list | acc]), state, cursor, log}

                  {:no_kw, state, cursor, log} ->
                    # Parse args in no_parens_expr context:
                    # - allow_do_block: false - do-blocks belong to outer call, not arguments
                    # - allow_no_parens_expr: true - allows operators like `when` to extend inside args
                    # Use min_bp=0 for argument values - they are full expressions not constrained
                    # by outer operator precedence. The outer min_bp only affects when to STOP
                    # parsing arguments (checked at lines 362-365), not the argument values themselves.
                    # Use stop_at_assoc: true to prevent => from being consumed - it's only valid in maps
                    arg_context = Context.no_parens_expr()

                    case Pratt.parse_with_min_bp(state, cursor, arg_context, log, 0, stop_at_assoc: true) do
                      {:ok, arg, state, cursor, log} ->
                        handle_no_parens_arg(arg, acc, state, cursor, ctx, log, min_bp, opts)

                      {:error, reason, state, cursor, log} ->
                        {:error, reason, state, cursor, log}
                    end

                  {:error, reason, state, cursor, log} ->
                    {:error, reason, state, cursor, log}
                end
            end
        end

      {:eof, state, cursor} ->
        {:ok, Enum.reverse(acc), state, cursor, log}

      {:error, diag, state, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  # Helper to handle a parsed argument in no-parens context
  defp handle_no_parens_arg(arg, acc, state, cursor, ctx, log, min_bp, opts) do
    # Validate no_parens expressions after a comma (grammar: call_args_no_parens_expr -> no_parens_expr : error)
    # The first arg is allowed to be no_parens (call_args_no_parens_ambig), but subsequent args must not be
    if acc != [] and ExprClass.classify(arg) == :no_parens do
      {:error, NoParensErrors.error_no_parens_many_strict(arg), state, cursor, log}
    else
      case TokenAdapter.peek(state, cursor) do
        {:ok, comma_tok, _, cursor} when elem(comma_tok, 0) == :"," ->
          if Keyword.get(opts, :stop_at_comma, false) do
            {:ok, Enum.reverse([arg | acc]), state, cursor, log}
          else
            {:ok, comma_tok, state, cursor} = TokenAdapter.next(state, cursor)
            {state, cursor} = EOE.skip(state, cursor)

            case TokenAdapter.peek(state, cursor) do
              {:ok, tok, _, cursor} ->
                kind = TokenAdapter.kind(tok)

                if kind in [:eol, :";", :")", :"]", :"}", :do] do
                  meta = TokenAdapter.token_meta(comma_tok)

                  {:error, {meta, "syntax error before: ", ""}, state, cursor, log}
                else
                  case Keywords.try_parse_call_args_no_parens_kw(state, cursor, ctx, log) do
                    {:ok, kw_list, state, cursor, log} ->
                      {:ok, Enum.reverse([kw_list, arg | acc]), state, cursor, log}

                    {:no_kw, state, cursor, log} ->
                      parse_no_parens_args([arg | acc], state, cursor, ctx, log, min_bp, opts)

                    {:error, reason, state, cursor, log} ->
                      {:error, reason, state, cursor, log}
                  end
                end

              {:eof, state, cursor} ->
                meta = TokenAdapter.token_meta(comma_tok)

                {:error, {meta, "syntax error before: ", ""}, state, cursor, log}

              {:error, diag, state, cursor} ->
                {:error, diag, state, cursor, log}
            end
          end

        _ ->
          {:ok, Enum.reverse([arg | acc]), state, cursor, log}
      end
    end
  end

  @doc """
  Parse a call without calling led() at the end.
  Used by Pratt parser when it needs to preserve min_bp for proper associativity.
  The caller is responsible for calling led() with the correct min_bp.
  Optional `min_bp` parameter (default 0) is threaded through to argument parsing.
  """
  @spec parse_without_led(State.t(), Cursor.t(), Pratt.context(), EventLog.t(), non_neg_integer(), keyword()) ::
          result()
  def parse_without_led(
        %State{} = state,
        cursor,
        %Context{} = ctx,
        %EventLog{} = log,
        min_bp \\ 0,
        opts \\ []
      ) do
    case TokenAdapter.peek(state, cursor) do
      {:ok, tok, _, cursor} ->
        case Identifiers.classify(TokenAdapter.kind(tok)) do
          :other ->
            Pratt.parse_with_min_bp(state, cursor, ctx, log, min_bp)

          ident_kind ->
            {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)
            parse_identifier_no_led(ident_kind, tok, state, cursor, ctx, log, min_bp, opts)
        end

      {:eof, state, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, state, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  # Parse identifier without calling led at the end
  defp parse_identifier_no_led(kind, tok, state, cursor, ctx, log, min_bp, opts) do
    case TokenAdapter.peek(state, cursor) do
      {:ok, next_tok, _, cursor} when elem(next_tok, 0) == :"(" and kind == :paren_identifier ->
        raise "dead code"
        parse_paren_call_no_led(tok, state, cursor, ctx, log)

      {:ok, open_tok, _, cursor} when elem(open_tok, 0) == :"[" and kind == :bracket_identifier ->
        # bracket_identifier followed by [ is bracket access
        # Note: bracket access calls led internally for chaining, need special handling
        parse_bracket_access_no_led(tok, open_tok, state, cursor, ctx, log)

      {:ok, next_tok, _, cursor} ->
        next_kind = TokenAdapter.kind(next_tok)

        cond do
          kind == :op_identifier ->
            parse_op_identifier_call_no_led(tok, state, cursor, ctx, log, min_bp, opts)

          # Check no-parens arg BEFORE binary operator check.
          # This handles cases like `spec +integer :: integer` where + is dual_op:
          # - dual_op has binary precedence (so Pratt.bp returns non-nil)
          # - BUT it can also start a unary no-parens argument
          # We want the no-parens interpretation here, so check can_start_no_parens_arg first.
          NoParens.can_start_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok) ->
            parse_no_parens_call_no_led(tok, state, cursor, ctx, log, min_bp, opts)

          Pratt.bp(next_kind) != nil or next_kind in [:., :dot_call_op] ->
            raise "dead code"
            # Binary operator follows - return as bare identifier, caller will handle
            ast = Builder.Helpers.from_token(tok)
            {:ok, ast, state, cursor, log}

          kind == :do_identifier and not Context.allow_do_block?(ctx) ->
            raise "dead code"
            ast = Builder.Helpers.from_token(tok)
            {:ok, ast, state, cursor, log}

          true ->
            ast = Builder.Helpers.from_token(tok)
            DoBlocks.maybe_do_block(ast, state, cursor, ctx, log)
        end

      _ ->
        raise "dead code"
        ast = Builder.Helpers.from_token(tok)
        DoBlocks.maybe_do_block(ast, state, cursor, ctx, log)
    end
  end

  defp parse_paren_call_no_led(callee_tok, state, cursor, ctx, log) do
    {:ok, _open_tok, state, cursor} = TokenAdapter.next(state, cursor)
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    with {:ok, args, state, cursor, log} <- parse_paren_args([], state, cursor, ctx, log),
         {:ok, close_meta, trailing_newlines, state, cursor} <- Meta.consume_closing(state, cursor, :")") do
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      callee_meta = TokenAdapter.token_meta(callee_tok)
      meta = Meta.closing_meta(callee_meta, close_meta, total_newlines)
      ast = {TokenAdapter.value(callee_tok), meta, Enum.reverse(args)}
      # Return without calling led
      {:ok, ast, state, cursor, log}
    else
      other -> Result.normalize_error(other, cursor, log)
    end
  end

  defp parse_bracket_access_no_led(ident_tok, _open_tok, state, cursor, ctx, log) do
    {:ok, open_tok, state, cursor} = TokenAdapter.next(state, cursor)
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    with {:ok, arg, state, cursor, log} <- parse_bracket_arg_no_skip(state, cursor, ctx, log) do
      {state, cursor, _trailing_newlines} = EOE.skip_count_newlines(state, cursor, 0)

      case expect_token(state, cursor, :"]") do
        {:ok, close_tok, state, cursor} ->
          subject = Builder.Helpers.from_token(ident_tok)
          bracket_meta = build_bracket_meta_with_newlines(open_tok, close_tok, leading_newlines)
          ast = build_access(subject, arg, bracket_meta)
          # Return without calling led
          {:ok, ast, state, cursor, log}

        other ->
          Result.normalize_error(other, cursor, log)
      end
    end
  end

  defp parse_op_identifier_call_no_led(callee_tok, state, cursor, ctx, log, _min_bp, opts) do
    # Use min_bp=0 for op_identifier arguments - same reasoning as parse_no_parens_args.
    # This allows @spec +integer :: integer to parse :: as part of the argument.
    # Use stop_at_assoc: true to prevent => from being consumed - it's only valid in maps
    # elixir_parser.yrl: call_args_no_parens_ambig -> no_parens_expr
    with {:ok, first_arg, state, cursor, log} <-
           Pratt.parse_with_min_bp(state, cursor, Context.no_parens_expr(), log, 0, stop_at_assoc: true) do
      case TokenAdapter.peek(state, cursor) do
        {:ok, comma_tok, _, cursor} when elem(comma_tok, 0) == :"," ->
          # raise "dead code"
          {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)

          with {:ok, args, state, cursor, log} <-
                 parse_no_parens_args([first_arg], state, cursor, ctx, log, 0, opts) do
            callee = TokenAdapter.value(callee_tok)
            meta = TokenAdapter.token_meta(callee_tok)
            ast = {callee, meta, args}
            DoBlocks.maybe_do_block(ast, state, cursor, ctx, log)
          end

        _ ->
          callee = TokenAdapter.value(callee_tok)
          meta = [ambiguous_op: nil] ++ TokenAdapter.token_meta(callee_tok)
          ast = {callee, meta, [first_arg]}
          DoBlocks.maybe_do_block(ast, state, cursor, ctx, log)
      end
    end
  end

  defp parse_no_parens_call_no_led(callee_tok, state, cursor, ctx, log, min_bp, opts) do
    with {:ok, args, state, cursor, log} <- parse_no_parens_args([], state, cursor, ctx, log, min_bp, opts) do
      callee = TokenAdapter.value(callee_tok)
      meta = TokenAdapter.token_meta(callee_tok)
      ast = {callee, meta, args}
      DoBlocks.maybe_do_block(ast, state, cursor, ctx, log)
    end
  end
end
