defmodule ToxicParser.Grammar.Calls do
  @moduledoc """
  Call parsing skeleton for Phase 5.

  This module classifies identifiers and dispatches to paren/no-parens call
  parsers. Full call argument parsing and ambiguity handling will be filled in
  subsequent iterations.
  """

  alias ToxicParser.{Builder, EventLog, Identifiers, NoParens, Pratt, Result, State, TokenAdapter}
  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.{DoBlocks, EOE, Expressions, Keywords}
  import Keywords, only: [{:is_keyword_list_result, 1}]

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @doc """
  Parses a call-or-identifier expression. Currently falls back to Pratt for
  non-call forms and wraps bare identifiers as AST.
  """
  @spec parse(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse(%State{} = state, ctx, %EventLog{} = log) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        case Identifiers.classify(tok.kind) do
          :other ->
            Pratt.parse(state, ctx, log)

          ident_kind ->
            {:ok, _tok, state} = TokenAdapter.next(state)
            parse_identifier(ident_kind, tok, state, ctx, log)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_identifier(kind, tok, state, ctx, log) do
    case TokenAdapter.peek(state) do
      # Only treat as paren call if the identifier was :paren_identifier
      # (tokenized without space before the paren, e.g., "foo(a)" not "foo (a)")
      # Plain :identifier followed by ( means the ( is a separate parenthesized expression
      {:ok, %{kind: :"("}, _} when kind == :paren_identifier ->
        parse_paren_call(tok, state, ctx, log)

      {:ok, %{kind: :"["} = open_tok, _} when kind == :bracket_identifier ->
        # bracket_identifier followed by [ is bracket access: foo[:bar]
        # Grammar: bracket_expr -> dot_bracket_identifier bracket_arg
        parse_bracket_access(tok, open_tok, state, ctx, log)

      # Alias followed by [ is bracket access: A[d]
      # Grammar: bracket_expr -> access_expr bracket_arg
      # Push back the alias token and let Pratt.parse handle it (led_bracket will process [)
      {:ok, %{kind: :"["}, _} when kind == :alias ->
        state = TokenAdapter.pushback(state, tok)
        Pratt.parse(state, ctx, log)

      {:ok, next_tok, _} ->
        cond do
          # op_identifier means tokenizer determined this is a no-parens call
          # with a unary expression argument (e.g., "a -1" where - is unary)
          kind == :op_identifier ->
            parse_op_identifier_call(tok, state, ctx, log)

          # Binary operator or dot operator follows - let Pratt handle expression
          Pratt.bp(next_tok.kind) != nil or next_tok.kind in [:dot_op, :dot_call_op] ->
            state = TokenAdapter.pushback(state, tok)
            Pratt.parse(state, ctx, log)

          # Could be no-parens call argument - parse the call
          # This must come BEFORE the do_identifier check below, because
          # `if a do :ok end` should parse `if` with arg `a` and do-block
          NoParens.can_start_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok) ->
            parse_no_parens_call(tok, state, ctx, log)

          # do_identifier in matched context with no arguments: this identifier
          # precedes a do-block but the do-block belongs to an outer call.
          # Just return the bare identifier.
          kind == :do_identifier and ctx == :matched ->
            ast = Builder.Helpers.from_token(tok)
            {:ok, ast, state, log}

          # Just a bare identifier - may have trailing do-block
          true ->
            ast = Builder.Helpers.from_token(tok)

            DoBlocks.maybe_do_block(ast, state, ctx, log,
              after: fn ast, state, log ->
                Pratt.led(ast, state, log, 0, ctx)
              end
            )
        end

      # No next token or at terminator - check for do-block
      _ ->
        ast = Builder.Helpers.from_token(tok)

        DoBlocks.maybe_do_block(ast, state, ctx, log,
          after: fn ast, state, log ->
            Pratt.led(ast, state, log, 0, ctx)
          end
        )
    end
  end

  # Parse op_identifier call like "a -1" or "a -1, 2" where the first argument starts with a unary op
  # The tokenizer has determined this is a call, not a binary expression
  # Grammar: no_parens_many_expr -> op_identifier call_args_no_parens_ambig
  #          no_parens_one_ambig -> op_identifier call_args_no_parens_ambig
  defp parse_op_identifier_call(callee_tok, state, ctx, log) do
    # Parse first arg (the unary expression)
    with {:ok, first_arg, state, log} <- Expressions.expr(state, :matched, log) do
      # Check for comma to see if there are more args
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :","}, _} ->
          # Multiple args - continue parsing
          {:ok, _comma, state} = TokenAdapter.next(state)

          with {:ok, args, state, log} <- parse_no_parens_args([first_arg], state, ctx, log) do
            callee = callee_tok.value
            # No ambiguous_op metadata when multiple args
            meta = Builder.Helpers.token_meta(callee_tok.metadata)
            ast = {callee, meta, args}

            DoBlocks.maybe_do_block(ast, state, ctx, log,
              after: fn ast, state, log ->
                Pratt.led(ast, state, log, 0, ctx)
              end
            )
          end

        _ ->
          # Single arg - add ambiguous_op: nil metadata
          callee = callee_tok.value
          meta = [ambiguous_op: nil] ++ Builder.Helpers.token_meta(callee_tok.metadata)
          ast = {callee, meta, [first_arg]}

          DoBlocks.maybe_do_block(ast, state, ctx, log,
            after: fn ast, state, log ->
              Pratt.led(ast, state, log, 0, ctx)
            end
          )
      end
    end
  end

  # Parse bracket access expression: foo[:bar]
  # Grammar: bracket_expr -> dot_bracket_identifier bracket_arg
  # Produces: {{:., meta, [Access, :get]}, meta, [subject, key]}
  defp parse_bracket_access(ident_tok, _open_tok, state, ctx, log) do
    # Consume the opening bracket
    {:ok, open_tok, state} = TokenAdapter.next(state)

    # Skip leading EOE and count newlines (only leading newlines matter for metadata)
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    # Parse the bracket argument (container_expr or kw_data)
    with {:ok, arg, state, log} <- parse_bracket_arg_no_skip(state, ctx, log) do
      # Skip trailing EOE before close bracket (don't count these)
      {state, _trailing_newlines} = EOE.skip_count_newlines(state, 0)

      case expect_token(state, :"]") do
        {:ok, close_tok, state} ->
          # Build access expression
          subject = Builder.Helpers.from_token(ident_tok)
          bracket_meta = build_bracket_meta_with_newlines(open_tok, close_tok, leading_newlines)
          ast = build_access(subject, arg, bracket_meta)
          # Continue with led() to handle chained access like foo[:a][:b]
          Pratt.led(ast, state, log, 0, ctx)

        other ->
          Result.normalize_error(other, log)
      end
    end
  end

  # Parse bracket_arg: the content inside [...]
  # Grammar:
  #   bracket_arg -> open_bracket kw_data close_bracket
  #   bracket_arg -> open_bracket container_expr close_bracket
  #   bracket_arg -> open_bracket container_expr ',' close_bracket
  # Note: caller handles EOE skipping before calling this
  defp parse_bracket_arg_no_skip(state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          # kw_data case: [a: 1, b: 2]
          Keywords.parse_kw_data(state, ctx, log)
        else
          # container_expr case
          with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
            # Check for trailing comma (allowed by grammar)
            case TokenAdapter.peek(state) do
              {:ok, %{kind: :","}, _} ->
                {:ok, _comma, state} = TokenAdapter.next(state)
                # After comma, check if next is keyword or close bracket
                case TokenAdapter.peek(state) do
                  {:ok, %{kind: :"]"}, _} ->
                    # Trailing comma - return expr
                    {:ok, expr, state, log}

                  {:ok, kw_tok, _} ->
                    # Check if expr was a keyword list from quoted key parsing
                    # If so, and next is also a keyword (regular or quoted), merge them
                    cond do
                      is_keyword_list_result(expr) and Keywords.starts_kw?(kw_tok) ->
                        with {:ok, more_kw, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
                          {:ok, expr ++ more_kw, state, log}
                        end

                      is_keyword_list_result(expr) and
                          kw_tok.kind in [:list_string_start, :bin_string_start] ->
                        # Another quoted keyword - parse it and continue
                        parse_quoted_kw_continuation(expr, state, ctx, log)

                      true ->
                        # Not a keyword continuation - just return expr with trailing comma consumed
                        {:ok, expr, state, log}
                    end

                  _ ->
                    {:ok, expr, state, log}
                end

              _ ->
                {:ok, expr, state, log}
            end
          end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Build metadata for bracket access with newlines
  defp build_bracket_meta_with_newlines(open_tok, close_tok, newlines) do
    open_meta = Builder.Helpers.token_meta(open_tok.metadata)
    close_meta = Builder.Helpers.token_meta(close_tok.metadata)
    Meta.closing_meta(open_meta, close_meta, newlines, from_brackets: true)
  end

  # Build Access.get call AST
  defp build_access(subject, key, meta) do
    {{:., meta, [Access, :get]}, meta, [subject, key]}
  end

  # Parse a no-parens call like `foo 1`, `foo 1, 2`, or `foo a: 1`.
  defp parse_no_parens_call(callee_tok, state, ctx, log) do
    with {:ok, args, state, log} <- parse_no_parens_args([], state, ctx, log) do
      callee = callee_tok.value
      meta = Builder.Helpers.token_meta(callee_tok.metadata)
      ast = {callee, meta, args}

      DoBlocks.maybe_do_block(ast, state, ctx, log,
        after: fn ast, state, log ->
          Pratt.led(ast, state, log, 0, ctx)
        end
      )
    end
  end

  defp parse_paren_call(callee_tok, state, ctx, log) do
    {:ok, _open_tok, state} = TokenAdapter.next(state)

    # Skip leading EOE and count newlines
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    with {:ok, args, state, log} <- parse_paren_args([], state, ctx, log),
         {:ok, close_meta, trailing_newlines, state} <- Meta.consume_closing(state, :")") do
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      callee_meta = Builder.Helpers.token_meta(callee_tok.metadata)
      meta = Meta.closing_meta(callee_meta, close_meta, total_newlines)

      ast = {callee_tok.value, meta, Enum.reverse(args)}
      # Check for nested call: foo()() - another paren call on the result
      maybe_nested_call(ast, state, ctx, log)
    else
      other -> Result.normalize_error(other, log)
    end
  end

  # Handle nested paren calls: foo()(), foo(1)(2), etc.
  # Rule: parens_call -> dot_call_identifier call_args_parens call_args_parens
  defp maybe_nested_call(ast, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"("}, _} ->
        parse_nested_paren_call(ast, state, ctx, log)

      _ ->
        DoBlocks.maybe_do_block(ast, state, ctx, log,
          after: fn ast, state, log ->
            Pratt.led(ast, state, log, 0, ctx)
          end
        )
    end
  end

  defp parse_nested_paren_call(callee_ast, state, ctx, log) do
    {:ok, _open_tok, state} = TokenAdapter.next(state)

    # Skip leading EOE and count newlines
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    with {:ok, args, state, log} <- parse_paren_args([], state, ctx, log),
         {:ok, close_meta, trailing_newlines, state} <- Meta.consume_closing(state, :")") do
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      callee_meta = extract_meta(callee_ast)
      meta = Meta.closing_meta(callee_meta, close_meta, total_newlines)

      ast = {callee_ast, meta, Enum.reverse(args)}
      # Recurse for chained calls like foo()()()
      maybe_nested_call(ast, state, ctx, log)
    else
      other -> Result.normalize_error(other, log)
    end
  end

  defp extract_meta({_name, meta, _args}) when is_list(meta) do
    Keyword.take(meta, [:line, :column])
  end

  defp expect_token(state, kind) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: ^kind} = token, state} -> {:ok, token, state}
      {:ok, token, state} -> {:error, {:expected, kind, got: token.kind}, state}
      {:eof, state} -> {:error, :unexpected_eof, state}
      {:error, diag, state} -> {:error, diag, state}
    end
  end

  defp parse_paren_args(acc, state, ctx, log),
    do: ToxicParser.Grammar.CallsPrivate.parse_paren_args(acc, state, ctx, log)

  @doc """
  Parse no-parens call arguments (exported for use by Pratt parser for dot calls).
  Optional `min_bp` parameter (default 0) stops argument parsing before operators
  with binding power < min_bp. This is essential for proper precedence handling
  in contexts like stab clauses where we must stop before `->`.
  """
  def parse_no_parens_args(acc, state, ctx, log, min_bp \\ 0) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: kind}, _} when kind in [:eoe, :")", :"]", :"}", :do] ->
        {:ok, Enum.reverse(acc), state, log}

      {:ok, %{kind: kind} = tok, _} ->
        # Check if we should stop at this operator due to min_bp.
        # BUT only for pure binary operators - tokens like dual_op can be unary,
        # so they should be allowed to start no-parens arguments.
        # The stab_op check is now handled by excluding it from led() entirely.
        can_be_unary = kind in [:dual_op, :unary_op, :at_op, :capture_op, :ternary_op]

        case {can_be_unary, Pratt.bp(kind)} do
          {false, bp} when is_integer(bp) and bp < min_bp ->
            # Stop before this pure binary operator (e.g., -> is now handled elsewhere)
            {:ok, Enum.reverse(acc), state, log}

          _ ->
            cond do
              Keywords.starts_kw?(tok) ->
                # kw_list is already [x: 1], wrap once for args: [[x: 1]]
                with {:ok, kw_list, state, log} <-
                       Keywords.parse_kw_no_parens_call(state, ctx, log) do
                  {:ok, Enum.reverse([kw_list | acc]), state, log}
                end

              true ->
                # Parse args in :matched context to prevent do-block attachment to arguments.
                # The do-block belongs to the outer call, not to individual args.
                # Use min_bp=0 for argument values - the argument should consume all binary
                # operators like :: and when. The caller's min_bp is only used to check
                # whether to *start* parsing (the check above), not to constrain argument values.
                # This allows @spec foo(a) :: bar to parse :: as part of spec's argument.
                case Pratt.parse_with_min_bp(state, :matched, log, 0) do
                  {:ok, arg, state, log} ->
                    handle_no_parens_arg(arg, acc, state, ctx, log, min_bp)

                  # Handle quoted keyword key - parse value and return as keyword pair
                  {:keyword_key, key_atom, state, log} ->
                    state = EOE.skip(state)

                    with {:ok, value_ast, state, log} <-
                           Pratt.parse_with_min_bp(state, :matched, log, 0) do
                      kw_pair = [{key_atom, value_ast}]
                      handle_no_parens_arg(kw_pair, acc, state, ctx, log, min_bp)
                    end

                  # Handle interpolated keyword key
                  {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, log} ->
                    state = EOE.skip(state)

                    with {:ok, value_ast, state, log} <-
                           Pratt.parse_with_min_bp(state, :matched, log, 0) do
                      key_ast =
                        Expressions.build_interpolated_keyword_key(
                          parts,
                          kind,
                          start_meta,
                          delimiter
                        )

                      kw_pair = [{key_ast, value_ast}]
                      handle_no_parens_arg(kw_pair, acc, state, ctx, log, min_bp)
                    end

                  {:error, reason, state, log} ->
                    {:error, reason, state, log}
                end
            end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Helper to handle a parsed argument in no-parens context
  defp handle_no_parens_arg(arg, acc, state, ctx, log, min_bp) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :","}, _} ->
        {:ok, _comma, state} = TokenAdapter.next(state)
        # Check if arg was a keyword list from quoted key parsing (e.g., "foo": 1)
        # If so, and next is also a keyword (regular or quoted), merge them
        case TokenAdapter.peek(state) do
          {:ok, next_tok, _} when is_keyword_list_result(arg) ->
            cond do
              Keywords.starts_kw?(next_tok) ->
                # Continue collecting keywords into this list
                with {:ok, more_kw, state, log} <-
                       Keywords.parse_kw_no_parens_call(state, ctx, log) do
                  merged_kw = arg ++ more_kw
                  {:ok, Enum.reverse([merged_kw | acc]), state, log}
                end

              next_tok.kind in [:list_string_start, :bin_string_start] ->
                # Another quoted keyword - parse via expression and merge
                parse_no_parens_quoted_kw_continuation(arg, acc, state, ctx, log, min_bp)

              true ->
                parse_no_parens_args([arg | acc], state, ctx, log, min_bp)
            end

          _ ->
            parse_no_parens_args([arg | acc], state, ctx, log, min_bp)
        end

      _ ->
        {:ok, Enum.reverse([arg | acc]), state, log}
    end
  end

  @doc """
  Parse a call without calling led() at the end.
  Used by Pratt parser when it needs to preserve min_bp for proper associativity.
  The caller is responsible for calling led() with the correct min_bp.
  Optional `min_bp` parameter (default 0) is threaded through to argument parsing.
  """
  @spec parse_without_led(State.t(), Pratt.context(), EventLog.t(), non_neg_integer()) :: result()
  def parse_without_led(%State{} = state, ctx, %EventLog{} = log, min_bp \\ 0) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        case Identifiers.classify(tok.kind) do
          :other ->
            Pratt.parse_with_min_bp(state, ctx, log, min_bp)

          ident_kind ->
            {:ok, _tok, state} = TokenAdapter.next(state)
            parse_identifier_no_led(ident_kind, tok, state, ctx, log, min_bp)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse identifier without calling led at the end
  defp parse_identifier_no_led(kind, tok, state, ctx, log, min_bp) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"("}, _} when kind == :paren_identifier ->
        parse_paren_call_no_led(tok, state, ctx, log)

      {:ok, %{kind: :"["} = open_tok, _} when kind == :bracket_identifier ->
        # bracket_identifier followed by [ is bracket access
        # Note: bracket access calls led internally for chaining, need special handling
        parse_bracket_access_no_led(tok, open_tok, state, ctx, log)

      {:ok, next_tok, _} ->
        cond do
          kind == :op_identifier ->
            parse_op_identifier_call_no_led(tok, state, ctx, log, min_bp)

          # Check no-parens arg BEFORE binary operator check.
          # This handles cases like `spec +integer :: integer` where + is dual_op:
          # - dual_op has binary precedence (so Pratt.bp returns non-nil)
          # - BUT it can also start a unary no-parens argument
          # We want the no-parens interpretation here, so check can_start_no_parens_arg first.
          NoParens.can_start_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok) ->
            parse_no_parens_call_no_led(tok, state, ctx, log, min_bp)

          Pratt.bp(next_tok.kind) != nil or next_tok.kind in [:dot_op, :dot_call_op] ->
            # Binary operator follows - return as bare identifier, caller will handle
            ast = Builder.Helpers.from_token(tok)
            {:ok, ast, state, log}

          kind == :do_identifier and ctx == :matched ->
            ast = Builder.Helpers.from_token(tok)
            {:ok, ast, state, log}

          true ->
            ast = Builder.Helpers.from_token(tok)
            DoBlocks.maybe_do_block(ast, state, ctx, log)
        end

      _ ->
        ast = Builder.Helpers.from_token(tok)
        DoBlocks.maybe_do_block(ast, state, ctx, log)
    end
  end

  defp parse_paren_call_no_led(callee_tok, state, ctx, log) do
    {:ok, _open_tok, state} = TokenAdapter.next(state)
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    with {:ok, args, state, log} <- parse_paren_args([], state, ctx, log),
         {:ok, close_meta, trailing_newlines, state} <- Meta.consume_closing(state, :")") do
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      callee_meta = Builder.Helpers.token_meta(callee_tok.metadata)
      meta = Meta.closing_meta(callee_meta, close_meta, total_newlines)
      ast = {callee_tok.value, meta, Enum.reverse(args)}
      # Return without calling led
      {:ok, ast, state, log}
    else
      other -> Result.normalize_error(other, log)
    end
  end

  defp parse_bracket_access_no_led(ident_tok, _open_tok, state, ctx, log) do
    {:ok, open_tok, state} = TokenAdapter.next(state)
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    with {:ok, arg, state, log} <- parse_bracket_arg_no_skip(state, ctx, log) do
      {state, _trailing_newlines} = EOE.skip_count_newlines(state, 0)

      case expect_token(state, :"]") do
        {:ok, close_tok, state} ->
          subject = Builder.Helpers.from_token(ident_tok)
          bracket_meta = build_bracket_meta_with_newlines(open_tok, close_tok, leading_newlines)
          ast = build_access(subject, arg, bracket_meta)
          # Return without calling led
          {:ok, ast, state, log}

        other ->
          Result.normalize_error(other, log)
      end
    end
  end

  defp parse_op_identifier_call_no_led(callee_tok, state, ctx, log, _min_bp) do
    # Use min_bp=0 for op_identifier arguments - same reasoning as parse_no_parens_args.
    # This allows @spec +integer :: integer to parse :: as part of the argument.
    with {:ok, first_arg, state, log} <- Pratt.parse_with_min_bp(state, :matched, log, 0) do
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state)

          with {:ok, args, state, log} <- parse_no_parens_args([first_arg], state, ctx, log, 0) do
            callee = callee_tok.value
            meta = Builder.Helpers.token_meta(callee_tok.metadata)
            ast = {callee, meta, args}
            DoBlocks.maybe_do_block(ast, state, ctx, log)
          end

        _ ->
          callee = callee_tok.value
          meta = [ambiguous_op: nil] ++ Builder.Helpers.token_meta(callee_tok.metadata)
          ast = {callee, meta, [first_arg]}
          DoBlocks.maybe_do_block(ast, state, ctx, log)
      end
    end
  end

  defp parse_no_parens_call_no_led(callee_tok, state, ctx, log, min_bp) do
    with {:ok, args, state, log} <- parse_no_parens_args([], state, ctx, log, min_bp) do
      callee = callee_tok.value
      meta = Builder.Helpers.token_meta(callee_tok.metadata)
      ast = {callee, meta, args}
      DoBlocks.maybe_do_block(ast, state, ctx, log)
    end
  end

  # Parse continuation of keyword list in no-parens context when we encounter another quoted keyword
  # Used when we have foo 'a': 0, 'b': bar 1, 2
  defp parse_no_parens_quoted_kw_continuation(acc_kw, acc, state, ctx, log, min_bp) do
    # Parse the quoted keyword via expression - should return keyword list
    # Use min_bp=0 for argument values (see comment in parse_no_parens_args)
    case Pratt.parse_with_min_bp(state, :matched, log, 0) do
      {:ok, expr, state, log} when is_keyword_list_result(expr) ->
        # Got another keyword pair, continue accumulating
        continue_quoted_kw_accumulation(acc_kw ++ expr, acc, state, ctx, log, min_bp)

      # Handle keyword_key return - parse value and continue
      {:keyword_key, key_atom, state, log} ->
        state = EOE.skip(state)

        with {:ok, value_ast, state, log} <-
               Pratt.parse_with_min_bp(state, :matched, log, 0) do
          expr = [{key_atom, value_ast}]
          continue_quoted_kw_accumulation(acc_kw ++ expr, acc, state, ctx, log, min_bp)
        end

      # Handle interpolated keyword key
      {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, log} ->
        state = EOE.skip(state)

        with {:ok, value_ast, state, log} <-
               Pratt.parse_with_min_bp(state, :matched, log, 0) do
          key_ast = Expressions.build_interpolated_keyword_key(parts, kind, start_meta, delimiter)
          expr = [{key_ast, value_ast}]
          continue_quoted_kw_accumulation(acc_kw ++ expr, acc, state, ctx, log, min_bp)
        end

      {:ok, _expr, state, log} ->
        # Not a keyword - error
        {:error, {:expected, :keyword}, state, log}

      {:error, _, _, _} = error ->
        error
    end
  end

  # Continue accumulating keywords after successfully parsing a keyword pair
  defp continue_quoted_kw_accumulation(acc_kw, acc, state, ctx, log, min_bp) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :","}, _} ->
        {:ok, _comma, state} = TokenAdapter.next(state)

        case TokenAdapter.peek(state) do
          {:ok, %{kind: kind}, _} when kind in [:eoe, :")", :"]", :"}", :do] ->
            # Trailing comma or terminator
            {:ok, Enum.reverse([acc_kw | acc]), state, log}

          {:ok, kw_tok, _} ->
            cond do
              Keywords.starts_kw?(kw_tok) ->
                with {:ok, more_kw, state, log} <-
                       Keywords.parse_kw_no_parens_call(state, ctx, log) do
                  {:ok, Enum.reverse([acc_kw ++ more_kw | acc]), state, log}
                end

              kw_tok.kind in [:list_string_start, :bin_string_start] ->
                parse_no_parens_quoted_kw_continuation(acc_kw, acc, state, ctx, log, min_bp)

              true ->
                {:ok, Enum.reverse([acc_kw | acc]), state, log}
            end

          _ ->
            {:ok, Enum.reverse([acc_kw | acc]), state, log}
        end

      _ ->
        {:ok, Enum.reverse([acc_kw | acc]), state, log}
    end
  end

  # Parse continuation of keyword list when we encounter another quoted keyword
  # Used when we have ['foo': 1, 'bar': 2] and need to parse 'bar': 2
  defp parse_quoted_kw_continuation(acc_kw, state, ctx, log) do
    # Parse the quoted keyword - should return keyword_key
    case Expressions.expr(state, ctx, log) do
      {:ok, expr, state, log} when is_keyword_list_result(expr) ->
        # Got another keyword pair, check for more
        case TokenAdapter.peek(state) do
          {:ok, %{kind: :","}, _} ->
            {:ok, _comma, state} = TokenAdapter.next(state)

            case TokenAdapter.peek(state) do
              {:ok, %{kind: :"]"}, _} ->
                # Trailing comma
                {:ok, acc_kw ++ expr, state, log}

              {:ok, kw_tok, _} ->
                cond do
                  # TODO: no coverage?
                  Keywords.starts_kw?(kw_tok) ->
                    with {:ok, more_kw, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
                      {:ok, acc_kw ++ expr ++ more_kw, state, log}
                    end

                  kw_tok.kind in [:list_string_start, :bin_string_start] ->
                    parse_quoted_kw_continuation(acc_kw ++ expr, state, ctx, log)

                  true ->
                    {:ok, acc_kw ++ expr, state, log}
                end

              _ ->
                {:ok, acc_kw ++ expr, state, log}
            end

          _ ->
            {:ok, acc_kw ++ expr, state, log}
        end

      {:ok, _expr, state, log} ->
        # Not a keyword - error
        {:error, {:expected, :keyword}, state, log}

      {:error, _, _, _} = error ->
        error
    end
  end
end
