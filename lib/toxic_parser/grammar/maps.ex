defmodule ToxicParser.Grammar.Maps do
  @moduledoc """
  Parsing for maps and structs, including updates inside `%{}`.
  """

  alias ToxicParser.{Builder, Context, Cursor, Error, EventLog, ExprClass, Pratt, State, TokenAdapter}
  alias ToxicParser.Builder.{Helpers, Meta}
  alias ToxicParser.Grammar.{Brackets, EOE, Expressions, Keywords}

  @type result ::
          {:ok, Macro.t(), State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}

  @spec parse_map(State.t(), Cursor.t(), Pratt.context(), EventLog.t(), non_neg_integer()) ::
          result()
  def parse_map(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log, min_bp \\ 0) do
    with {:ok, ast, state, cursor, log} <- parse_map_base(state, cursor, ctx, log) do
      # Continue with Pratt.led to handle trailing operators
      Pratt.led(ast, state, cursor, log, min_bp, ctx)
    end
  end

  # Parse map without calling Pratt.led - internal implementation
  defp parse_map_base(state, cursor, ctx, log) do
    case TokenAdapter.next(state, cursor) do
      {:ok, {:%{}, _meta, _value} = tok, state, cursor} ->
        # map -> map_op map_args where map_op is '%{}'
        # The tokenizer emits %{} as three tokens: :%{}, :"{", :"}"
        # We need to consume the "{" token (the "}" will be consumed by parse_map_args_body)
        percent_meta = Helpers.token_meta(tok)
        # Consume the opening brace token
        case TokenAdapter.next(state, cursor) do
          {:ok, {:"{", _meta, _value}, state, cursor} ->
            # For %{} maps, use percent_meta for position (not brace position)
            # Use a tuple sentinel that can never be a valid struct name
            parse_map_args_after_brace(
              {:__no_struct_base__},
              percent_meta,
              percent_meta,
              state,
              cursor,
              ctx,
              log
            )

          {:ok, {got_kind, _meta, _value}, state, cursor} ->
            {:error, {:expected, :"{", got: got_kind}, state, cursor, log}

          {:eof, state, cursor} ->
            {:error, :unexpected_eof, state, cursor, log}

          {:error, diag, state, cursor} ->
            {:error, diag, state, cursor, log}
        end

      {:ok, {:%, _meta, _value} = tok, state, cursor} ->
        # map -> '%' map_base_expr map_args (struct)
        percent_meta = Helpers.token_meta(tok)
        # Skip optional EOE after %
        {state, cursor} = EOE.skip(state, cursor)
        # Parse map_base_expr (the struct name)
        with {:ok, base, state, cursor, log} <- parse_map_base_expr(state, cursor, ctx, log) do
          # Skip optional EOE after base
          {state, cursor} = EOE.skip(state, cursor)
          parse_map_args(base, percent_meta, state, cursor, ctx, log)
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
  defp parse_map_base_expr(state, cursor, _ctx, log) do
    case Cursor.peek(cursor) do
      # Special case for @ (at_op): precedence 320 > dot 310
      # So @0.a should be (@0).a - @ takes just the base, then . applies
      {:ok, {:at_op, _meta, _value}, cursor} ->
        {:ok, {:at_op, _op_meta, op_value} = op_tok, state, cursor} =
          TokenAdapter.next(state, cursor)

        {state, cursor} = EOE.skip(state, cursor)

        with {:ok, operand, state, cursor, log} <-
               parse_unary_operand(state, cursor, Context.matched_expr(), log) do
          op_meta = Helpers.token_meta(op_tok)
          ast = {op_value, op_meta, [operand]}
          # Continue with dots/calls to handle trailing .foo or .Bar
          Pratt.led_dots_and_calls(ast, state, cursor, log, Context.matched_expr())
        end

      # Other unary ops (^, !, not, +, -): precedence < dot 310
      # So ^t.d should be ^(t.d) - dots are part of the operand
      {:ok, {tok_kind, _tok_meta, tok_value}, cursor} ->
        cond do
          tok_kind in [:unary_op, :dual_op] ->
            {:ok, {_op_kind, _op_meta, op_value} = op_tok, state, cursor} =
              TokenAdapter.next(state, cursor)

            {state, cursor} = EOE.skip(state, cursor)

            # Parse operand with dots applied (since dot binds tighter)
            with {:ok, operand, state, cursor, log} <-
                   parse_unary_operand_with_dots(state, cursor, Context.matched_expr(), log) do
              op_meta = Helpers.token_meta(op_tok)
              ast = {op_value, op_meta, [operand]}
              {:ok, ast, state, cursor, log}
            end

          # ellipsis_op can also be nullary (elixir_parser.yrl: sub_matched_expr -> ellipsis_op)
          # as in `%...{}`. If the next token is `{`, treat it as nullary.
          tok_kind == :ellipsis_op ->
            {:ok, {_op_kind, _op_meta, op_value} = op_tok, state, cursor} =
              TokenAdapter.next(state, cursor)

            op_meta = Helpers.token_meta(op_tok)

            case Cursor.peek(cursor) do
              {:ok, {:"{", _meta, _value}, cursor} ->
                {:ok, {op_value, op_meta, []}, state, cursor, log}

              {:ok, _, cursor} ->
                {state, cursor} = EOE.skip(state, cursor)

                with {:ok, operand, state, cursor, log} <-
                       parse_unary_operand_with_dots(state, cursor, Context.matched_expr(), log) do
                  {:ok, {op_value, op_meta, [operand]}, state, cursor, log}
                end
            end

          # ternary_op :"//" used as unary prefix (e.g., %//foo{})
          # Also has precedence < dot 310, so dots are part of the operand
          tok_kind == :ternary_op and tok_value == :"//" ->
            {:ok, {_op_kind, _op_meta, _op_value} = op_tok, state, cursor} =
              TokenAdapter.next(state, cursor)

            {state, cursor} = EOE.skip(state, cursor)

            with {:ok, operand, state, cursor, log} <-
                   parse_unary_operand_with_dots(state, cursor, Context.matched_expr(), log) do
              op_meta = Helpers.token_meta(op_tok)
              # Calculate inner/outer metadata with column adjustment
              {outer_meta, inner_meta} =
                case {Keyword.get(op_meta, :line), Keyword.get(op_meta, :column)} do
                  {line, col} when is_integer(line) and is_integer(col) ->
                    {[line: line, column: col + 1], [line: line, column: col]}

                  _ ->
                    {op_meta, op_meta}
                end

              ast = {:/, outer_meta, [{:/, inner_meta, nil}, operand]}
              {:ok, ast, state, cursor, log}
            end

          # fn ... end is a valid sub_matched_expr for struct base (e.g. %fn a -> 1 end{})
          tok_kind == :fn ->
            alias ToxicParser.Grammar.Blocks

            case Blocks.parse(state, cursor, Context.matched_expr(), log) do
              {:ok, ast, state, cursor, log} ->
                {:ok, ast, state, cursor, log}

              {:no_block, state, cursor} ->
                {:error, :expected_fn, state, cursor, log}
            end

          # Parenthesized expression or list/tuple/bitstring as struct base
          # Grammar allows these as sub_matched_expr -> access_expr -> list | tuple | bitstring | empty_paren
          # Example: %!(){} where () is the operand of !
          # Example: %@[]{} where [] is the operand of @
          tok_kind in [:"(", :"[", :"{", :"<<"] ->
            alias ToxicParser.Grammar.Containers
            Containers.parse(state, cursor, Context.matched_expr(), log)

          tok_kind in [:%, :%{}] ->
            # Nested maps/structs are valid map_base_expr (sub_matched_expr).
            # Needed for things like `%%{}{}.`
            parse_map_base(state, cursor, Context.matched_expr(), log)

          true ->
            # sub_matched_expr with dot operator and paren call support
            # parse_base_with_dots_and_calls handles:
            #   - Foo.Bar.Baz (dotted aliases)
            #   - unquote(struct) (paren calls)
            #   - module.Foo.Bar (dotted calls)
            # But stops at { which is the struct body
            Pratt.parse_base_with_dots_and_calls(state, cursor, Context.matched_expr(), log)
        end

      {:error, _, cursor} ->
        # Default case - parse as base expression
        Pratt.parse_base_with_dots_and_calls(state, cursor, Context.matched_expr(), log)
    end
  end

  # Parse the operand of a unary operator in struct base context.
  # This parses just the BASE expression without dots.
  # For example, in %@0.a{}, the operand of @ is just 0, not 0.a.
  # The .a is handled separately after building the unary expression.
  defp parse_unary_operand(state, cursor, ctx, log) do
    case Cursor.peek(cursor) do
      # For nested @ operator, continue without dots since @ (320) > dot (310)
      {:ok, {:at_op, _meta, _value}, cursor} ->
        {:ok, {:at_op, _op_meta, op_value} = op_tok, state, cursor} =
          TokenAdapter.next(state, cursor)

        {state, cursor} = EOE.skip(state, cursor)

        with {:ok, operand, state, cursor, log} <- parse_unary_operand(state, cursor, ctx, log) do
          op_meta = Helpers.token_meta(op_tok)
          ast = {op_value, op_meta, [operand]}
          maybe_parse_bracket_access(ast, state, cursor, log)
        end

      # For other unary operators (precedence < dot 310), use parse_unary_operand_with_dots
      # so that dots are included in their operand.
      # E.g., %@+a.i{} should be @(+(a.i)){} not (@(+a)).i{}
      {:ok, {tok_kind, _tok_meta, tok_value} = tok, cursor} ->
        cond do
          tok_kind in [:unary_op, :ellipsis_op, :dual_op] ->
            {:ok, {_op_kind, _op_meta, op_value} = op_tok, state, cursor} =
              TokenAdapter.next(state, cursor)

            op_meta = Helpers.token_meta(op_tok)

            case tok_kind do
              :ellipsis_op ->
                case Cursor.peek(cursor) do
                  {:ok, {:"{", _meta, _value}, cursor} ->
                    {:ok, {op_value, op_meta, []}, state, cursor, log}

                  {:ok, _, cursor} ->
                    {state, cursor} = EOE.skip(state, cursor)

                    with {:ok, operand, state, cursor, log} <-
                           parse_unary_operand_with_dots(state, cursor, ctx, log) do
                      ast = {op_value, op_meta, [operand]}
                      {:ok, ast, state, cursor, log}
                    end
                end

              _ ->
                {state, cursor} = EOE.skip(state, cursor)

                with {:ok, operand, state, cursor, log} <-
                       parse_unary_operand_with_dots(state, cursor, ctx, log) do
                  ast = {op_value, op_meta, [operand]}
                  {:ok, ast, state, cursor, log}
                end
            end

          # ternary_op :"//" used as unary prefix (precedence < dot 310)
          tok_kind == :ternary_op and tok_value == :"//" ->
            raise "dead code"

            {:ok, {_op_kind, _op_meta, _op_value} = op_tok, state, cursor} =
              TokenAdapter.next(state, cursor)

            {state, cursor} = EOE.skip(state, cursor)

            with {:ok, operand, state, cursor, log} <-
                   parse_unary_operand_with_dots(state, cursor, ctx, log) do
              op_meta = Helpers.token_meta(op_tok)

              {outer_meta, inner_meta} =
                case {Keyword.get(op_meta, :line), Keyword.get(op_meta, :column)} do
                  {line, col} when is_integer(line) and is_integer(col) ->
                    {[line: line, column: col + 1], [line: line, column: col]}

                  _ ->
                    {op_meta, op_meta}
                end

              ast = {:/, outer_meta, [{:/, inner_meta, nil}, operand]}
              {:ok, ast, state, cursor, log}
            end

          # Container as operand (e.g., %@[]{} where [] is the operand)
          # In struct-base context we must not let the container consume trailing dots/calls,
          # because `{` belongs to the outer `%...{}` body.
          tok_kind in [:"(", :"[", :"{", :"<<", :%, :%{}] ->
            alias ToxicParser.Grammar.Containers
            Containers.parse(state, cursor, ctx, log, 10000)

          # Paren call as operand (e.g., %@i(){} where i() is the operand)
          # Parse identifier then consume () as a paren call
          tok_kind == :paren_identifier ->
            {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)
            Pratt.parse_paren_call_base(tok, state, cursor, ctx, log)

          true ->
            # Base expression (literal, identifier, etc.) - parse without dots
            Pratt.parse_base(state, cursor, ctx, log)
        end

      {:ok, _, cursor} ->
        Pratt.parse_base(state, cursor, ctx, log)
    end
  end

  defp maybe_parse_bracket_access(base, state, cursor, log) do
    case Cursor.peek(cursor) do
      {:ok, {:"[", _meta, _value}, cursor} ->
        parse_bracket_access_chain(base, state, cursor, log)

      {:ok, _, cursor} ->
        {:ok, base, state, cursor, log}
    end
  end

  defp parse_bracket_access_chain(base, state, cursor, log) do
    case Cursor.peek(cursor) do
      {:ok, {:"[", _meta, _value}, cursor} ->
        {:ok, open_tok, state, cursor} = TokenAdapter.next(state, cursor)

        {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

        with {:ok, arg, state, cursor, log} <-
               Brackets.parse_bracket_arg_no_skip(state, cursor, log) do
          {state, cursor, _trailing_newlines} = EOE.skip_count_newlines(state, cursor, 0)

          case TokenAdapter.next(state, cursor) do
            {:ok, {:"]", _meta, _value} = close_tok, state, cursor} ->
              open_meta = TokenAdapter.token_meta(open_tok)
              close_meta = TokenAdapter.token_meta(close_tok)

              bracket_meta =
                Meta.closing_meta(open_meta, close_meta, leading_newlines, from_brackets: true)

              combined =
                {{:., bracket_meta, [Access, :get]}, bracket_meta, [base, arg]}

              parse_bracket_access_chain(combined, state, cursor, log)

            {:ok, {got_kind, _meta, _value}, state, cursor} ->
              {:error, {:expected, :"]", got: got_kind}, state, cursor, log}

            {:eof, state, cursor} ->
              {:error, :unexpected_eof, state, cursor, log}

            {:error, diag, state, cursor} ->
              {:error, diag, state, cursor, log}
          end
        end

      {:ok, _, cursor} ->
        {:ok, base, state, cursor, log}
    end
  end

  # Parse a unary operand with dots applied (for unary ops with precedence < dot).
  # Similar to parse_unary_operand but applies dots to the base expression.
  # Used for operators like ^, !, not where %^t.d{} should parse as %^(t.d){}.
  defp parse_unary_operand_with_dots(state, cursor, ctx, log) do
    case Cursor.peek(cursor) do
      # Nested @ - special handling since @ binds tighter than dot
      {:ok, {:at_op, _meta, _value}, cursor} ->
        {:ok, {:at_op, _op_meta, op_value} = op_tok, state, cursor} =
          TokenAdapter.next(state, cursor)

        {state, cursor} = EOE.skip(state, cursor)

        with {:ok, operand, state, cursor, log} <- parse_unary_operand(state, cursor, ctx, log) do
          op_meta = Helpers.token_meta(op_tok)
          ast = {op_value, op_meta, [operand]}
          # Apply dots after @ since @ binds tighter
          Pratt.led_dots_and_calls(ast, state, cursor, log, ctx)
        end

      # Other unary ops - recurse with dots
      {:ok, {tok_kind, _tok_meta, tok_value}, cursor} ->
        cond do
          tok_kind in [:unary_op, :ellipsis_op, :dual_op] ->
            {:ok, {_op_kind, _op_meta, op_value} = op_tok, state, cursor} =
              TokenAdapter.next(state, cursor)

            op_meta = Helpers.token_meta(op_tok)

            case tok_kind do
              :ellipsis_op ->
                case Cursor.peek(cursor) do
                  {:ok, {:"{", _meta, _value}, cursor} ->
                    {:ok, {op_value, op_meta, []}, state, cursor, log}

                  {:ok, _, cursor} ->
                    {state, cursor} = EOE.skip(state, cursor)

                    with {:ok, operand, state, cursor, log} <-
                           parse_unary_operand_with_dots(state, cursor, ctx, log) do
                      ast = {op_value, op_meta, [operand]}
                      {:ok, ast, state, cursor, log}
                    end
                end

              _ ->
                {state, cursor} = EOE.skip(state, cursor)

                with {:ok, operand, state, cursor, log} <-
                       parse_unary_operand_with_dots(state, cursor, ctx, log) do
                  ast = {op_value, op_meta, [operand]}
                  {:ok, ast, state, cursor, log}
                end
            end

          # ternary_op :"//" used as unary prefix
          tok_kind == :ternary_op and tok_value == :"//" ->
            {:ok, {_op_kind, _op_meta, _op_value} = op_tok, state, cursor} =
              TokenAdapter.next(state, cursor)

            {state, cursor} = EOE.skip(state, cursor)

            with {:ok, operand, state, cursor, log} <-
                   parse_unary_operand_with_dots(state, cursor, ctx, log) do
              op_meta = Helpers.token_meta(op_tok)

              {outer_meta, inner_meta} =
                case {Keyword.get(op_meta, :line), Keyword.get(op_meta, :column)} do
                  {line, col} when is_integer(line) and is_integer(col) ->
                    {[line: line, column: col + 1], [line: line, column: col]}

                  _ ->
                    {op_meta, op_meta}
                end

              ast = {:/, outer_meta, [{:/, inner_meta, nil}, operand]}
              {:ok, ast, state, cursor, log}
            end

          # Container as operand
          tok_kind in [:"(", :"[", :"{", :"<<", :%, :%{}] ->
            alias ToxicParser.Grammar.Containers

            with {:ok, base, state, cursor, log} <-
                   Containers.parse(state, cursor, ctx, log, 10000) do
              Pratt.led_dots_and_calls(base, state, cursor, log, ctx)
            end

          true ->
            # Base expression - parse and apply dots
            with {:ok, base, state, cursor, log} <- Pratt.parse_base(state, cursor, ctx, log) do
              Pratt.led_dots_and_calls(base, state, cursor, log, ctx)
            end
        end

      {:ok, _, cursor} ->
        with {:ok, base, state, cursor, log} <- Pratt.parse_base(state, cursor, ctx, log) do
          Pratt.led_dots_and_calls(base, state, cursor, log, ctx)
        end
    end
  end

  # Parse map_args: { } | { map_close } | { assoc_update ... }
  defp parse_map_args(base, percent_meta, state, cursor, ctx, log) do
    # Consume the opening brace
    case Cursor.peek(cursor) do
      {:ok, {:"{", _meta, _value} = open_tok, cursor} ->
        {:ok, _open, state, cursor} = TokenAdapter.next(state, cursor)
        brace_meta = Helpers.token_meta(open_tok)
        # Skip leading EOE and count newlines
        {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

        parse_map_args_body(
          base,
          percent_meta,
          brace_meta,
          leading_newlines,
          state,
          cursor,
          ctx,
          log
        )

      {:ok, {got_kind, _meta, _value}, cursor} ->
        {:error, {:expected, :"{", got: got_kind}, state, cursor, log}

      {:eof, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  # Called after the opening brace has been consumed (for %{} case)
  defp parse_map_args_after_brace(base, percent_meta, brace_meta, state, cursor, ctx, log) do
    # Skip leading EOE and count newlines
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)
    parse_map_args_body(base, percent_meta, brace_meta, leading_newlines, state, cursor, ctx, log)
  end

  defp parse_map_args_body(
         base,
         percent_meta,
         brace_meta,
         leading_newlines,
         state,
         cursor,
         ctx,
         log
       ) do
    case Cursor.peek(cursor) do
      # Empty map: map_args -> open_curly '}'
      {:ok, {:"}", _meta, _value} = close_tok, cursor} ->
        {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
        close_meta = Helpers.token_meta(close_tok)
        map_meta = Meta.closing_meta(brace_meta, close_meta, leading_newlines)
        {:ok, build_map_ast(base, [], percent_meta, map_meta), state, cursor, log}

      {:ok, _, cursor} ->
        # Try to parse map update first, then fall back to regular entries
        case try_parse_map_update(state, cursor, ctx, log) do
          {:ok, update_ast, close_meta, state, cursor, log} ->
            map_meta = Meta.closing_meta(brace_meta, close_meta, leading_newlines)

            {:ok, build_map_update_ast(base, update_ast, percent_meta, map_meta), state, cursor,
             log}

          {:not_update, state, cursor} ->
            # Parse map_close: kw_data | assoc | assoc_base ',' kw_data
            with {:ok, entries, close_meta, state, cursor, log} <-
                   parse_map_close(state, cursor, ctx, log) do
              map_meta = Meta.closing_meta(brace_meta, close_meta, leading_newlines)
              {:ok, build_map_ast(base, entries, percent_meta, map_meta), state, cursor, log}
            end

          {:error, _, _, _, _} = err ->
            err
        end

      {:eof, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  # Try to parse assoc_update: matched_expr pipe_op_eol assoc_expr
  # Returns {:ok, update_ast, close_meta, state, cursor, log} or :not_update or {:error, ...}
  #
  # Grammar rules:
  #   assoc_update -> matched_expr pipe_op_eol assoc_expr
  #   assoc_update -> unmatched_expr pipe_op_eol assoc_expr
  #   assoc_update_kw -> matched_expr pipe_op_eol kw_data
  #   assoc_update_kw -> unmatched_expr pipe_op_eol kw_data
  #
  # Strategy:
  # 1. Parse the base expression (matched_expr/unmatched_expr)
  # 2. Check for pipe_op token (|)
  # 3. If | found, check what follows:
  #    - kw_data (keyword list) -> map update with keywords
  #    - assoc_expr (k => v) -> map update with assoc expressions
  # 4. op_identifier calls (like c!) consume | as part of their argument,
  #    so if we still see | after parsing base, it's the map update separator
  defp try_parse_map_update(state, cursor, ctx, log) do
    case Cursor.peek(cursor) do
      {:ok, {:kw_identifier, _, _}, cursor} ->
        {:not_update, state, cursor}

      {:ok, _, cursor} ->
        {checkpoint_id, checkpoint_state} = TokenAdapter.checkpoint(state, cursor)

        case parse_map_update_candidate(checkpoint_state, cursor, ctx, log) do
          {:ok, update_ast, close_meta, state, cursor, log} ->
            {:ok, update_ast, close_meta, TokenAdapter.drop_checkpoint(state, checkpoint_id),
             cursor, log}

          {:not_update, state, _cursor} ->
            {state, cursor} = TokenAdapter.rewind(state, checkpoint_id)
            {:not_update, state, cursor}

          {:error, diag, state, _cursor, log} ->
            {state, cursor} = TokenAdapter.rewind(state, checkpoint_id)
            {:error, diag, state, cursor, log}
        end
    end
  end

  # Binding power just above pipe_op (70) to prevent | from being consumed
  @pipe_op_bp 70

  defp parse_map_update_candidate(state, cursor, _ctx, log) do
    {starts_with_quoted_kw_key?, cursor} =
      case Cursor.peek(cursor) do
        {:ok, {tok_kind, _meta, _value}, cursor} ->
          {tok_kind in [:bin_string_start, :list_string_start], cursor}

        {:ok, _, cursor} ->
          {false, cursor}
      end

    # First, try parsing the full expression without restricting |
    # This allows op_identifier calls (like c!) to consume | as part of their argument
    # e.g., %{c!s|n => 1} should parse as c!(s|n) => 1, not c!s | n => 1
    # Save checkpoint so we can retry with min_bp if needed
    {reparse_ref, state} = TokenAdapter.checkpoint(state, cursor)

    case Pratt.parse(state, cursor, Context.container_expr(), log) do
      {:ok, base_expr, state, cursor, log} ->
        case {base_expr, Cursor.peek(cursor)} do
          {{:|, pipe_meta, [base, rhs]}, {:ok, {:assoc_op, _, _}, cursor}} ->
            if Keyword.has_key?(pipe_meta, :parens) do
              state = TokenAdapter.drop_checkpoint(state, reparse_ref)
              handle_map_update_after_base_expr(base_expr, state, cursor, log)
            else
              if expr_contains_pipe?(base) do
                case parse_map_update_from_parsed_pipe(base, pipe_meta, rhs, state, cursor, log) do
                  {:ok, update_ast, close_meta, state, cursor, log} ->
                    {:ok, update_ast, close_meta,
                     TokenAdapter.drop_checkpoint(state, reparse_ref), cursor, log}

                  {:not_update, _state, _cursor} ->
                    {state, cursor} = TokenAdapter.rewind(state, reparse_ref)
                    {:not_update, state, cursor}

                  {:error, diag, _state, _cursor, log} ->
                    {state, cursor} = TokenAdapter.rewind(state, reparse_ref)
                    {:error, diag, state, cursor, log}
                end
              else
                # Rewind and reparse with min_bp to not consume |
                {state, cursor} = TokenAdapter.rewind(state, reparse_ref)
                parse_map_update_with_min_bp(state, cursor, log)
              end
            end

          {{:|, pipe_meta, [_base, rhs]}, {:ok, _, cursor}} ->
            if not Keyword.has_key?(pipe_meta, :parens) and is_list(rhs) and
                 rhs != [] and Enum.all?(rhs, &is_keyword_or_assoc_entry?/1) do
              # Reparse to distinguish between kw_data and bracketed list `[kw]`.
              {state, cursor} = TokenAdapter.rewind(state, reparse_ref)
              parse_map_update_with_min_bp(state, cursor, log)
            else
              state = TokenAdapter.drop_checkpoint(state, reparse_ref)
              handle_map_update_after_base_expr(base_expr, state, cursor, log)
            end

          {_, {:ok, _, cursor}} ->
            state = TokenAdapter.drop_checkpoint(state, reparse_ref)
            handle_map_update_after_base_expr(base_expr, state, cursor, log)
        end

      {:keyword_key, _, _, state, cursor, _} ->
        if starts_with_quoted_kw_key? do
          state = TokenAdapter.drop_checkpoint(state, reparse_ref)
          {:not_update, state, cursor}
        else
          {state, cursor} = TokenAdapter.rewind(state, reparse_ref)
          parse_map_update_with_min_bp(state, cursor, log)
        end

      {:keyword_key_interpolated, _, _, _, _, state, cursor, _} ->
        if starts_with_quoted_kw_key? do
          state = TokenAdapter.drop_checkpoint(state, reparse_ref)
          {:not_update, state, cursor}
        else
          {state, cursor} = TokenAdapter.rewind(state, reparse_ref)
          parse_map_update_with_min_bp(state, cursor, log)
        end

      {:error, _diag, _state, _cursor, _log} ->
        # Parsing failed - likely due to keyword after |
        # Retry with min_bp above pipe_op to not consume |
        {state, cursor} = TokenAdapter.rewind(state, reparse_ref)
        parse_map_update_with_min_bp(state, cursor, log)
    end
  end

  defp handle_map_update_after_base_expr(base_expr, state, cursor, log) do
    case check_embedded_map_update(base_expr) do
      {:update, base, pipe_meta, rhs_entries} ->
        {state, cursor} = EOE.skip(state, cursor)

        case Cursor.peek(cursor) do
          {:ok, {:"}", _meta, _value} = close_tok, cursor} ->
            {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
            close_meta = Helpers.token_meta(close_tok)
            update_ast = map_update_ast(pipe_meta, base, rhs_entries)
            {:ok, update_ast, close_meta, state, cursor, log}

          {:ok, {:",", _meta, _value}, cursor} ->
            {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)
            {state, cursor} = EOE.skip(state, cursor)
            parse_map_update_trailing_entries(base, pipe_meta, rhs_entries, state, cursor, log)

          _ ->
            raise "dead code"
            {:not_update, state, cursor}
        end

      :not_update ->
        {state, cursor, newlines} = EOE.skip_count_newlines(state, cursor, 0)

        case Cursor.peek(cursor) do
          {:ok, {:pipe_op, _, _} = pipe_tok, cursor} ->
            raise "dead code"
            {:ok, _pipe, state, cursor} = TokenAdapter.next(state, cursor)
            {state, cursor, newlines_after_pipe} = EOE.skip_newlines_only(state, cursor, 0)
            pipe_meta_kw = token_meta_with_newlines(pipe_tok, max(newlines, newlines_after_pipe))
            parse_map_update_rhs(base_expr, pipe_meta_kw, state, cursor, log)

          {:ok, _, cursor} ->
            {:not_update, state, cursor}
        end
    end
  end

  # Check if an expression is a pipe expression with valid map update RHS
  # e.g., map | :a => 1 parses as {:|, meta, [map, {:"=>", _, [:a, 1]}]}
  # Returns {:update, base, pipe_meta, entries} or :not_update
  defp check_embedded_map_update({:|, pipe_meta, [base, rhs]}) do
    # Parenthesized pipes inside %{} are regular pipe expressions, not map updates.
    if Keyword.has_key?(pipe_meta, :parens) do
      :not_update
    else
      base_class = ExprClass.classify(base)

      case classify_pipe_rhs_for_map_update(rhs, base_class) do
        {:valid, entries} ->
          {:update, base, pipe_meta, entries}

        _ ->
          :not_update
      end
    end
  end

  defp check_embedded_map_update(_), do: :not_update

  # Check if RHS of | is valid for map update
  # Valid: single assoc {:"=>", _, [k, v]} or keyword list
  # Annotates keys with :assoc metadata indicating the position of =>
  defp classify_pipe_rhs_for_map_update({:"=>", assoc_meta, [key, value]} = _assoc, base_class) do
    if valid_update_entry?({key, value}, base_class) do
      annotated_key = annotate_assoc(key, assoc_meta)
      {:valid, [{annotated_key, value}]}
    else
      :invalid
    end
  end

  # Handle nested pipe in map update RHS: a | b | c => d
  # Pratt produces: {:|, _, [a, {:|, _, [b, {:"=>", _, [c, d]}]}]}
  # The RHS of first | is: {:|, _, [b, {:"=>", _, [c, d]}]}
  # This should become: [{(b | c), d}] - key is b | c, value is d
  # Use extract_assoc to properly handle any level of nesting
  defp classify_pipe_rhs_for_map_update({:|, rhs_meta, _} = expr, _base_class) do
    if Keyword.has_key?(rhs_meta, :parens) do
      {:valid, [expr]}
    else
      raise "dead code"

      case extract_assoc(expr) do
        {:assoc, key, value, assoc_meta} ->
          annotated_key = annotate_assoc(key, assoc_meta)
          {:valid, [{annotated_key, value}]}

        :not_assoc ->
          :invalid
      end
    end
  end

  defp classify_pipe_rhs_for_map_update(list, base_class) when is_list(list) do
    cond do
      list == [] ->
        # Empty list literal (e.g. ''), treated as a map_base_expr, wrapped.
        {:valid, [list]}

      Enum.all?(list, &is_keyword_or_assoc_entry?/1) ->
        if Enum.all?(list, &valid_update_entry?(&1, base_class)) do
          # Annotate keys in assoc entries with :assoc metadata
          annotated = Enum.map(list, &annotate_assoc_entry/1)
          {:valid, annotated}
        else
          :invalid
        end

      true ->
        # A non-keyword list literal is a valid map_base_expr on the RHS.
        {:valid, [list]}
    end
  end

  # Fallback: if RHS contains a rightmost assoc (=>), treat it as a single assoc entry.
  # Otherwise it's a map_base_expr, wrapped in a list.
  defp classify_pipe_rhs_for_map_update(rhs, base_class) do
    case extract_assoc(rhs) do
      {:assoc, key, value, assoc_meta} ->
        if valid_update_entry?({key, value}, base_class) do
          {:valid, [{annotate_assoc(key, assoc_meta), value}]}
        else
          :invalid
        end

      :not_assoc ->
        {:valid, [rhs]}
    end
  end

  # Extract the rightmost => from an expression tree.
  # Used for validating map-update RHS like `base | k1 | k2 => v`.
  defp extract_assoc({:"=>", meta, [key, value]}), do: {:assoc, key, value, meta}

  defp extract_assoc({callee, meta, args}) when is_list(meta) and is_list(args) do
    case extract_assoc_in_args(args) do
      {:assoc, new_args, value, assoc_meta} ->
        raise "dead code"
        {:assoc, {callee, meta, new_args}, value, assoc_meta}

      :not_assoc ->
        :not_assoc
    end
  end

  defp extract_assoc(_), do: :not_assoc

  defp extract_assoc_in_args(args) do
    args
    |> Enum.with_index()
    |> Enum.reverse()
    |> Enum.reduce_while(:not_assoc, fn {arg, idx}, _acc ->
      case extract_assoc(arg) do
        {:assoc, key, value, assoc_meta} ->
          raise "dead code"
          {:halt, {:assoc, List.replace_at(args, idx, key), value, assoc_meta}}

        :not_assoc ->
          {:cont, :not_assoc}
      end
    end)
  end

  # Annotate a single assoc entry with :assoc metadata
  defp annotate_assoc_entry({:"=>", assoc_meta, [key, value]}) do
    {annotate_assoc(key, assoc_meta), value}
  end

  defp annotate_assoc_entry(entry), do: entry

  defp is_keyword_or_assoc_entry?({key, _value}) when is_atom(key), do: true
  defp is_keyword_or_assoc_entry?({{_expr, _meta, _args}, _value}), do: true
  defp is_keyword_or_assoc_entry?({:"=>", _meta, [_k, _v]}), do: true
  defp is_keyword_or_assoc_entry?(_), do: false

  defp valid_update_entry?({:"=>", _meta, [key, _value]}, base_class),
    do: valid_update_entry?({key, nil}, base_class)

  defp valid_update_entry?({key, _value}, base_class) do
    case ExprClass.classify(key) do
      :no_parens -> allow_no_parens_update_key?(key, base_class)
      _ -> true
    end
  end

  defp valid_update_entry?(_, _base_class), do: true

  # Allow operator keys with no-parens calls only when the base is matched.
  defp allow_no_parens_update_key?(key, :matched), do: operator_key_allows_no_parens?(key)

  defp allow_no_parens_update_key?(_, _), do: false

  defp operator_key_allows_no_parens?({name, _meta, args})
       when is_atom(name) and is_list(args) do
    case args do
      [arg] ->
        if operator_name?(name, 1) do
          allow_unary_operand?(arg)
        else
          false
        end

      [left, right] ->
        if operator_name?(name, 2) do
          allow_binary_operand?(left) and allow_binary_operand?(right)
        else
          false
        end

      _ ->
        false
    end
  end

  defp operator_key_allows_no_parens?(_), do: false

  defp allow_unary_operand?(arg) do
    case ExprClass.classify(arg) do
      :no_parens -> no_parens_call?(arg) or operator_key_allows_no_parens?(arg)
      _ -> true
    end
  end

  defp allow_binary_operand?(arg) do
    case ExprClass.classify(arg) do
      :no_parens -> no_parens_call?(arg) or operator_key_allows_no_parens?(arg)
      _ -> true
    end
  end

  defp no_parens_call?({name, meta, args})
       when is_atom(name) and is_list(meta) and is_list(args) do
    not Macro.operator?(name, length(args)) and no_parens_meta?(meta) and args != []
  end

  defp no_parens_call?({_callee, meta, args}) when is_list(meta) and is_list(args) do
    no_parens_meta?(meta) and args != []
  end

  defp no_parens_call?(_), do: false

  defp no_parens_meta?(meta) do
    not Keyword.has_key?(meta, :closing) and not Keyword.has_key?(meta, :parens)
  end

  defp operator_name?(name, arity) when is_atom(name) do
    Macro.operator?(name, arity)
  end

  # Parse trailing entries after map update: %{base | k => v, more...}
  defp parse_map_update_trailing_entries(base, pipe_meta, initial_entries, state, cursor, log) do
    case Cursor.peek(cursor) do
      # Trailing comma case: %{base | k => v,}
      {:ok, {:"}", _meta, _value} = close_tok, cursor} ->
        {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
        close_meta = Helpers.token_meta(close_tok)
        update_ast = map_update_ast(pipe_meta, base, initial_entries)
        {:ok, update_ast, close_meta, state, cursor, log}

      {:ok, _tok, cursor} ->
        # map_close already handles:
        # - kw_data close_curly
        # - assoc close_curly
        # - assoc_base ',' kw_data close_curly
        with {:ok, more_entries, close_meta, state, cursor, log} <-
               parse_map_close(state, cursor, Context.container_expr(), log) do
          all_entries = initial_entries ++ more_entries
          update_ast = map_update_ast(pipe_meta, base, all_entries)
          {:ok, update_ast, close_meta, state, cursor, log}
        end

      {:eof, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  # Fallback: parse with min_bp restriction to handle cases like %{map | a: 1}
  # where | is followed by keyword data (not valid as binary operator RHS)
  defp parse_map_update_with_min_bp(state, cursor, log) do
    case Pratt.parse_with_min_bp(state, cursor, Context.container_expr(), log, @pipe_op_bp + 1) do
      {:ok, base_expr, state, cursor, log} ->
        {state, cursor, newlines} = EOE.skip_count_newlines(state, cursor, 0)

        case Cursor.peek(cursor) do
          {:ok, {:pipe_op, _, _} = pipe_tok, cursor} ->
            {:ok, _pipe, state, cursor} = TokenAdapter.next(state, cursor)
            {state, cursor, newlines_after_pipe} = EOE.skip_newlines_only(state, cursor, 0)
            pipe_meta_kw = token_meta_with_newlines(pipe_tok, max(newlines, newlines_after_pipe))
            parse_map_update_rhs(base_expr, pipe_meta_kw, state, cursor, log)

          _ ->
            raise "dead code"
            {:not_update, state, cursor}
        end

      {:keyword_key, _, _, state, cursor, _} ->
        raise "dead code"
        {:not_update, state, cursor}

      {:keyword_key_interpolated, _, _, _, _, state, cursor, _} ->
        raise "dead code"
        {:not_update, state, cursor}

      {:error, diag, state, cursor, log} ->
        {:error, diag, state, cursor, log}
    end
  end

  # Parse the RHS of a map update (after the |)
  # Can be kw_data or assoc_expr(s)
  defp parse_map_update_rhs(base, pipe_meta, state, cursor, log) do
    container_ctx = Context.container_expr()

    case Cursor.peek(cursor) do
      {:ok, {:"[", _meta, _value}, cursor} ->
        # Bracketed list literal: treat as single entry, unless followed by =>.
        with {:ok, rhs_expr, state, cursor, log} <-
               Expressions.expr(state, cursor, container_ctx, log) do
          {state, cursor} = EOE.skip(state, cursor)

          case Cursor.peek(cursor) do
            {:ok, {:assoc_op, _meta, _value} = assoc_tok, cursor} ->
              {:ok, _assoc, state, cursor} = TokenAdapter.next(state, cursor)
              {state, cursor} = EOE.skip(state, cursor)

              with {:ok, value, state, cursor, log} <-
                     Expressions.expr(state, cursor, Context.container_expr(), log) do
                entries = [{annotate_assoc(rhs_expr, Helpers.token_meta(assoc_tok)), value}]
                {state, cursor} = EOE.skip(state, cursor)

                case Cursor.peek(cursor) do
                  {:ok, {:"}", _meta, _value} = close_tok, cursor} ->
                    {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
                    close_meta = Helpers.token_meta(close_tok)
                    update_ast = map_update_ast(pipe_meta, base, entries)
                    {:ok, update_ast, close_meta, state, cursor, log}

                  {:ok, {:",", _meta, _value}, cursor} ->
                    {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)
                    {state, cursor} = EOE.skip(state, cursor)

                    with {:ok, more_entries, close_meta, state, cursor, log} <-
                           parse_map_close(state, cursor, Context.container_expr(), log) do
                      update_ast = map_update_ast(pipe_meta, base, entries ++ more_entries)
                      {:ok, update_ast, close_meta, state, cursor, log}
                    end

                  {:ok, _, cursor} ->
                    {:error, :expected_closing_brace, state, cursor, log}
                end
              end

            {:ok, {:"}", _meta, _value} = close_tok, cursor} ->
              {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
              close_meta = Helpers.token_meta(close_tok)
              update_ast = map_update_ast(pipe_meta, base, [rhs_expr])
              {:ok, update_ast, close_meta, state, cursor, log}

            {:ok, {:",", _meta, _value}, cursor} ->
              {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)
              {state, cursor} = EOE.skip(state, cursor)

              case Cursor.peek(cursor) do
                {:ok, {:"}", _meta, _value} = close_tok, cursor} ->
                  # Trailing comma after bracketed list.
                  {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
                  close_meta = Helpers.token_meta(close_tok)
                  update_ast = map_update_ast(pipe_meta, base, [rhs_expr])
                  {:ok, update_ast, close_meta, state, cursor, log}

                {:ok, _, cursor} ->
                  with {:ok, more_entries, close_meta, state, cursor, log} <-
                         parse_map_close(state, cursor, Context.container_expr(), log) do
                    update_ast = map_update_ast(pipe_meta, base, [rhs_expr | more_entries])
                    {:ok, update_ast, close_meta, state, cursor, log}
                  end
              end

            {:ok, _, cursor} ->
              {:error, :expected_closing_brace, state, cursor, log}
          end
        end

      {:ok, _, cursor} ->
        case Keywords.try_parse_kw_data(state, cursor, container_ctx, log) do
          {:ok, kw_list, state, cursor, log} ->
            # assoc_update_kw -> matched_expr pipe_op_eol kw_data
            # assoc_update_kw -> unmatched_expr pipe_op_eol kw_data
            {state, cursor} = EOE.skip(state, cursor)

            case Cursor.peek(cursor) do
              {:ok, {:"}", _meta, _value} = close_tok, cursor} ->
                {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
                close_meta = Helpers.token_meta(close_tok)
                update_ast = map_update_ast(pipe_meta, base, kw_list)
                {:ok, update_ast, close_meta, state, cursor, log}

              _ ->
                raise "dead code"
                {:not_update, state, cursor}
            end

          {:no_kw, state, cursor, log} ->
            # assoc_expr(s): %{base | k => v, ...}
            base_class = ExprClass.classify(base)

            case parse_map_update_assoc_entries(
                   base,
                   pipe_meta,
                   base_class,
                   [],
                   state,
                   cursor,
                   log
                 ) do
              {:not_update, state, cursor} -> {:not_update, state, cursor}
              other -> other
            end

          {:error, reason, state, cursor, log} ->
            {:error, reason, state, cursor, log}
        end
    end
  end

  defp parse_map_update_from_parsed_pipe(base, pipe_meta, key_expr, state, cursor, log) do
    base_class = ExprClass.classify(base)

    case Cursor.peek(cursor) do
      {:ok, {:assoc_op, _, _} = assoc_tok, cursor} ->
        {:ok, _assoc, state, cursor} = TokenAdapter.next(state, cursor)
        assoc_meta = Helpers.token_meta(assoc_tok)
        {state, cursor} = EOE.skip(state, cursor)

        with {:ok, value, state, cursor, log} <-
               Expressions.expr(state, cursor, Context.container_expr(), log) do
          if not valid_update_entry?({key_expr, value}, base_class) do
            {:not_update, state, cursor}
          else
            annotated_key = annotate_assoc(key_expr, assoc_meta)
            entries = [{annotated_key, value}]
            {state, cursor} = EOE.skip(state, cursor)

            case Cursor.peek(cursor) do
              {:ok, {:"}", _meta, _value} = close_tok, cursor} ->
                {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
                close_meta = Helpers.token_meta(close_tok)
                update_ast = map_update_ast(pipe_meta, base, entries)
                {:ok, update_ast, close_meta, state, cursor, log}

              {:ok, {:",", _meta, _value}, cursor} ->
                {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)
                {state, cursor} = EOE.skip(state, cursor)
                parse_map_update_trailing_entries(base, pipe_meta, entries, state, cursor, log)

              {:eof, cursor} ->
                {:error, :unexpected_eof, state, cursor, log}

              {:error, diag, cursor} ->
                {:error, diag, state, cursor, log}

              {:ok, _tok, cursor} ->
                {:error, :expected_closing_brace, state, cursor, log}
            end
          end
        end

      {:ok, _, cursor} ->
        {:not_update, state, cursor}
    end
  end

  defp expr_contains_pipe?({:|, _meta, _args}), do: true

  defp expr_contains_pipe?({callee, _meta, args}) when is_list(args) do
    expr_contains_pipe?(callee) or Enum.any?(args, &expr_contains_pipe?/1)
  end

  defp expr_contains_pipe?(list) when is_list(list) do
    Enum.any?(list, &expr_contains_pipe?/1)
  end

  defp expr_contains_pipe?(_), do: false

  # Parse assoc entries for map update RHS
  defp parse_map_update_assoc_entries(base, pipe_meta, base_class, acc, state, cursor, log) do
    with {:ok, entry, state, cursor, log} <-
           parse_assoc_expr(state, cursor, Context.container_expr(), log) do
      # Verify it's actually an assoc (has =>)
      case entry do
        {key, value} ->
          if not valid_update_entry?({key, value}, base_class) do
            {:not_update, state, cursor}
          else
            # It's a proper assoc entry
            {state, cursor} = EOE.skip(state, cursor)

            case Cursor.peek(cursor) do
              {:ok, {:"}", _meta, _value} = close_tok, cursor} ->
                {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
                close_meta = Helpers.token_meta(close_tok)
                entries = Enum.reverse([{key, value} | acc])
                update_ast = map_update_ast(pipe_meta, base, entries)
                {:ok, update_ast, close_meta, state, cursor, log}

              {:ok, {:",", _meta, _value}, cursor} ->
                {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)
                {state, cursor} = EOE.skip(state, cursor)

                case Cursor.peek(cursor) do
                  {:ok, {:"}", _meta, _value} = close_tok, cursor} ->
                    # Trailing comma
                    {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
                    close_meta = Helpers.token_meta(close_tok)
                    entries = Enum.reverse([{key, value} | acc])
                    update_ast = map_update_ast(pipe_meta, base, entries)
                    {:ok, update_ast, close_meta, state, cursor, log}

                  {:ok, _tok, cursor} ->
                    # Parse the remaining entries using map_close (assoc and/or kw tail)
                    with {:ok, more_entries, close_meta, state, cursor, log} <-
                           parse_map_close(state, cursor, Context.container_expr(), log) do
                      entries = :lists.reverse([{key, value} | acc], more_entries)
                      update_ast = map_update_ast(pipe_meta, base, entries)
                      {:ok, update_ast, close_meta, state, cursor, log}
                    end

                  {:eof, cursor} ->
                    {:error, :unexpected_eof, state, cursor, log}

                  {:error, diag, cursor} ->
                    {:error, diag, state, cursor, log}
                end

              {:eof, cursor} ->
                {:error, :unexpected_eof, state, cursor, log}

              {:error, diag, cursor} ->
                {:error, diag, state, cursor, log}

              {:ok, _tok, cursor} ->
                {:error, :expected_closing_brace, state, cursor, log}
            end
          end

        _ ->
          {:error, :expected_assoc, state, cursor, log}
      end
    end
  end

  # Helper to create [line:, column:, newlines: n] metadata for pipe_op
  # Extracts newlines from both the token's embedded count and the passed-in value
  defp token_meta_with_newlines(tok, newlines) do
    token_newlines = TokenAdapter.newlines(tok)
    effective_newlines = max(newlines, token_newlines)

    base_meta = Helpers.token_meta(tok)

    if effective_newlines > 0 do
      [{:newlines, effective_newlines} | base_meta]
    else
      base_meta
    end
  end

  defp map_update_ast(pipe_meta, base, entries) do
    {:|, Keyword.delete(pipe_meta, :assoc), [base, entries]}
  end

  # Build map AST: %{...} or %Base{...}
  defp build_map_ast({:__no_struct_base__}, entries, _percent_meta, map_meta) do
    # Plain map: %{...}
    {:%{}, map_meta, entries}
  end

  defp build_map_ast(base, entries, percent_meta, map_meta) do
    # Struct: %Base{...}
    {:%, percent_meta, [base, {:%{}, map_meta, entries}]}
  end

  # Build map update AST: %{map | ...} or %Base{map | ...}
  defp build_map_update_ast({:__no_struct_base__}, update_ast, _percent_meta, map_meta) do
    # Plain map update: %{...}
    {:%{}, map_meta, [update_ast]}
  end

  defp build_map_update_ast(base, update_ast, percent_meta, map_meta) do
    # Struct update: %Base{...}
    {:%, percent_meta, [base, {:%{}, map_meta, [update_ast]}]}
  end

  # Parse map_close: kw_data | assoc | assoc_base ',' kw_data
  # Returns {:ok, entries, close_meta, state, cursor, log}
  defp parse_map_close(state, cursor, ctx, log) do
    container_ctx = Context.container_expr()

    # Try keyword data first
    case Keywords.try_parse_kw_data(state, cursor, container_ctx, log) do
      {:ok, kw_list, state, cursor, log} ->
        # map_close -> kw_data close_curly
        {state, cursor} = EOE.skip(state, cursor)

        case Cursor.peek(cursor) do
          {:ok, {:"}", _meta, _value}, cursor} ->
            {:ok, close_tok, state, cursor} = TokenAdapter.next(state, cursor)
            close_meta = Helpers.token_meta(close_tok)
            {:ok, kw_list, close_meta, state, cursor, log}

          {:ok, {:",", comma_meta, _value}, cursor} when state.mode == :tolerant ->
            {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)

            case parse_assoc_list([], state, cursor, ctx, log) do
              {:ok, entries, close_meta, state, cursor, log} ->
                {error_node, state} =
                  build_kw_tail_error_node(
                    kw_tail_error_at(comma_meta),
                    comma_meta,
                    state,
                    cursor,
                    entries
                  )

                {:ok, [kw_list, error_node], close_meta, state, cursor, log}

              {:error, reason, state, cursor, log} ->
                {:error, reason, state, cursor, log}
            end

          {:ok, {got_kind, _meta, _value}, cursor} ->
            {:error, {:expected, :"}", got: got_kind}, state, cursor, log}

          {:eof, cursor} ->
            {:error, :unexpected_eof, state, cursor, log}

          {:error, diag, cursor} ->
            {:error, diag, state, cursor, log}
        end

      {:no_kw, state, cursor, log} ->
        # Try assoc entries
        parse_assoc_list([], state, cursor, ctx, log)

      {:error, reason, state, cursor, log} ->
        {:error, reason, state, cursor, log}
    end
  end

  # Parse assoc list (assoc_base): one or more assoc_expr separated by commas
  # assoc_base -> assoc_expr | assoc_expr ',' assoc_base
  defp parse_assoc_list(acc, state, cursor, ctx, log) do
    with {:ok, entry, state, cursor, log} <-
           parse_assoc_expr(state, cursor, Context.container_expr(), log) do
      {state, cursor} = EOE.skip(state, cursor)

      case Cursor.peek(cursor) do
        {:ok, {:"}", _meta, _value} = close_tok, cursor} ->
          # End of assoc list
          {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
          close_meta = Helpers.token_meta(close_tok)
          {:ok, Enum.reverse([entry | acc]), close_meta, state, cursor, log}

        {:ok, {:",", _meta, _value}, cursor} ->
          {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)
          {state, cursor} = EOE.skip(state, cursor)

          case Cursor.peek(cursor) do
            {:ok, {:"}", _meta, _value} = close_tok, cursor} ->
              # Trailing comma
              {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
              close_meta = Helpers.token_meta(close_tok)
              {:ok, Enum.reverse([entry | acc]), close_meta, state, cursor, log}

            {:ok, _, cursor} ->
              # Check if next is kw_data (assoc_base ',' kw_data)
              case Keywords.try_parse_kw_data(state, cursor, Context.container_expr(), log) do
                {:ok, kw_list, state, cursor, log} ->
                  {state, cursor} = EOE.skip(state, cursor)

                  case TokenAdapter.next(state, cursor) do
                    {:ok, {:"}", _meta, _value} = close_tok, state, cursor} ->
                      close_meta = Helpers.token_meta(close_tok)
                      # kw_list is already a list of keyword tuples, don't wrap in another list
                      entries = :lists.reverse([entry | acc], kw_list)
                      {:ok, entries, close_meta, state, cursor, log}

                    {:ok, {got_kind, _meta, _value}, state, cursor} ->
                      {:error, {:expected, :"}", got: got_kind}, state, cursor, log}

                    {:eof, state, cursor} ->
                      {:error, :unexpected_eof, state, cursor, log}

                    {:error, diag, state, cursor} ->
                      {:error, diag, state, cursor, log}
                  end

                {:no_kw, state, cursor, log} ->
                  # Continue with more assoc entries
                  parse_assoc_list([entry | acc], state, cursor, ctx, log)

                {:error, reason, state, cursor, log} ->
                  {:error, reason, state, cursor, log}
              end

            {:eof, cursor} ->
              {:error, :unexpected_eof, state, cursor, log}

            {:error, diag, cursor} ->
              {:error, diag, state, cursor, log}
          end

        {:eof, cursor} ->
          {:error, :unexpected_eof, state, cursor, log}

        {:error, diag, cursor} ->
          {:error, diag, state, cursor, log}

        {:ok, _tok, cursor} ->
          {:error, :expected_closing_brace, state, cursor, log}
      end
    end
  end

  # Parse assoc_expr: container_expr | container_expr '=>' container_expr
  # Returns {:ok, entry, state, cursor, log} where entry is {key, value} or just expr
  defp parse_assoc_expr(state, cursor, _ctx, log) do
    with {:ok, left, state, cursor, log} <-
           Expressions.expr(state, cursor, Context.container_expr(), log) do
      {state, cursor} = EOE.skip(state, cursor)

      case Cursor.peek(cursor) do
        {:ok, {:assoc_op, _, _}, cursor} ->
          {:ok, assoc_tok, state, cursor} = TokenAdapter.next(state, cursor)
          assoc_meta = Helpers.token_meta(assoc_tok)
          {state, cursor} = EOE.skip(state, cursor)

          with {:ok, right, state, cursor, log} <-
                 Expressions.expr(state, cursor, Context.container_expr(), log) do
            # Annotate key with :assoc metadata
            annotated_key = annotate_assoc(left, assoc_meta)
            {:ok, {annotated_key, right}, state, cursor, log}
          end

        {:ok, _, cursor} ->
          if ExprClass.classify(left) == :no_parens do
            {:error, ToxicParser.NoParensErrors.error_no_parens_container_strict(left), state,
             cursor, log}
          else
            # No assoc operator - return as-is
            {:ok, left, state, cursor, log}
          end
      end
    end
  end

  # Annotate a key expression with :assoc metadata
  # For AST nodes {name, meta, args_or_context}, merge assoc position into meta
  # Note: Variables have {name, meta, nil} where nil is the context, not args list
  defp annotate_assoc({name, meta, args_or_context}, assoc_meta) when is_list(meta) do
    # Merge :assoc metadata into existing metadata
    merged_meta = [{:assoc, assoc_meta} | meta]
    {name, merged_meta, args_or_context}
  end

  # For atoms, DON'T wrap in AST form - just return the atom
  # Elixir converts :a => 1 to keyword pairs a: 1 without metadata annotation
  defp annotate_assoc(key, _assoc_meta) when is_atom(key) do
    key
  end

  defp annotate_assoc(key, _assoc_meta) do
    # For other expressions (literals, etc.), return as-is
    # The Elixir parser doesn't annotate these
    key
  end

  defp kw_tail_error_at({{line, column}, _, _}) do
    location = [line: line, column: column]

    {location,
     "unexpected expression after keyword list. Keyword lists must always come last in lists and maps. Therefore, this is not allowed:\n\n" <>
       "    [some: :value, :another]\n" <>
       "    %{some: :value, another => value}\n\n" <>
       "Instead, reorder it to be the last entry:\n\n" <>
       "    [:another, some: :value]\n" <>
       "    %{another => value, some: :value}\n\n" <>
       "Syntax error after: ", "','"}
  end

  defp kw_tail_error_at(_), do: {[], "syntax error before: ", ""}

  defp build_kw_tail_error_node(reason, meta, %State{} = state, cursor, children) do
    {line, column} =
      case meta do
        {{line, column}, _, _} -> {line, column}
        _ -> Cursor.position(cursor)
      end

    {id, state} = State.next_diagnostic_id(state)

    diagnostic =
      Error.from_parser(nil, reason,
        line_index: state.line_index,
        source: state.source,
        position: {{line, column}, {line, column}}
      )
      |> Error.annotate(%{
        id: id,
        anchor: %{kind: :error_node, path: [], note: nil},
        synthetic?: false,
        lexer_error_code: nil
      })

    diagnostic = %{diagnostic | details: Map.put(diagnostic.details, :source, :grammar)}
    state = %{state | diagnostics: [diagnostic | state.diagnostics]}

    payload =
      Error.error_node_payload(diagnostic,
        kind: :unexpected,
        original: reason,
        children: children,
        synthetic?: false
      )

    error_meta = [line: line, column: column, toxic: %{synthetic?: false, anchor: %{line: line, column: column}}]
    {Builder.Helpers.error(payload, error_meta), state}
  end
end
