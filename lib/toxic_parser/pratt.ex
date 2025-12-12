defmodule ToxicParser.Pratt do
  @moduledoc """
  Pratt parser skeleton for matched/unmatched/no-parens expressions.

  Phase 3 implements binding power lookup and API shape; full parselets are
  implemented in later phases.

  Dual operator spacing (`dual_op` as unary vs binary) relies on Toxic token
  shapes; spacing-sensitive disambiguation will occur in parselets using token
  metadata.
  """

  alias ToxicParser.{Builder, EventLog, Identifiers, Precedence, State, TokenAdapter}
  alias ToxicParser.Grammar.Keywords
  alias ToxicParser.Grammar.{Blocks, Calls, Dots}

  @type context :: :matched | :unmatched | :no_parens

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @doc """
  Parses an expression in the given context.
  """
  @spec parse(State.t(), context(), EventLog.t()) :: result()
  def parse(%State{} = state, context, %EventLog{} = log) do
    with {:ok, token, state} <- TokenAdapter.next(state),
         {:ok, left, state, log} <- nud(token, state, context, log) do
      led(left, state, log, 0, context)
    else
      {:eof, state} -> {:error, :unexpected_eof, state, log}
      {:error, diag, state} -> {:error, diag, state, log}
    end
  end

  @doc """
  Parses a base expression (nud only, no trailing operators via led).
  Used for sub_matched_expr in map_base_expr grammar rule.
  This version does NOT check for do-blocks or no-parens call arguments.
  """
  @spec parse_base(State.t(), context(), EventLog.t()) :: result()
  def parse_base(%State{} = state, context, %EventLog{} = log) do
    with {:ok, token, state} <- TokenAdapter.next(state),
         {:ok, ast, state, log} <- nud_base(token, state, context, log) do
      {:ok, ast, state, log}
    else
      {:eof, state} -> {:error, :unexpected_eof, state, log}
      {:error, diag, state} -> {:error, diag, state, log}
    end
  end

  @doc """
  Parses a base expression with dot operator support for dotted aliases.
  Used for struct names like `%Foo.Bar{}` where we need to parse the full
  alias chain (Foo.Bar) but NOT consume {} as tuple/call arguments.

  Only processes dot_op for alias chaining. Does not handle:
  - Paren calls: foo.bar()
  - No-parens calls: foo.bar arg
  - Bracket access: foo.bar[x]
  - Do-blocks: foo.bar do...end
  """
  @spec parse_base_with_dots(State.t(), context(), EventLog.t()) :: result()
  def parse_base_with_dots(%State{} = state, context, %EventLog{} = log) do
    with {:ok, token, state} <- TokenAdapter.next(state),
         {:ok, ast, state, log} <- nud_base(token, state, context, log) do
      led_dot_only(ast, state, log, context)
    else
      {:eof, state} -> {:error, :unexpected_eof, state, log}
      {:error, diag, state} -> {:error, diag, state, log}
    end
  end

  @doc """
  Parses an expression with a minimum binding power.
  Used for map updates where we need to stop before the pipe operator.
  Also used for guard expressions in stab clauses where we need to stop before ->.
  """
  @spec parse_with_min_bp(State.t(), context(), EventLog.t(), non_neg_integer()) :: result()
  def parse_with_min_bp(%State{} = state, context, %EventLog{} = log, min_bp) do
    with {:ok, token, state} <- TokenAdapter.next(state),
         {:ok, left, state, log} <- nud_with_min_bp(token, state, context, log, min_bp) do
      led(left, state, log, min_bp, context)
    else
      {:eof, state} -> {:error, :unexpected_eof, state, log}
      {:error, diag, state} -> {:error, diag, state, log}
    end
  end

  @doc "Exposes binary binding power."
  @spec bp(atom()) :: Precedence.bp() | nil
  def bp(kind) do
    case Precedence.binary(kind) do
      {bp, _assoc} -> bp
      _ -> nil
    end
  end

  @doc "Exposes unary binding power."
  @spec unary_bp(atom()) :: Precedence.bp() | nil
  def unary_bp(kind) do
    case Precedence.unary(kind) do
      {bp, _assoc} -> bp
      _ -> nil
    end
  end

  # Handle unary operators in nud (null denotation)
  defp nud(token, state, context, log) do
    nud_with_min_bp(token, state, context, log, 0)
  end

  # Handle nud for base expressions only - no do-blocks or no-parens calls
  # This is used by parse_base for struct names like %Foo{} where we want
  # just the identifier, not trying to parse { as a no-parens call argument
  defp nud_base(token, state, context, log) do
    case token.kind do
      :error_token ->
        meta = build_meta(token.metadata)
        ast = Builder.Helpers.error(token.value, meta)
        {:ok, ast, state, log}

      :capture_int ->
        parse_capture_int(token, state, log)

      _ ->
        case Precedence.unary(token.kind) do
          {bp, _assoc} ->
            parse_unary(token, state, context, log, bp)

          nil ->
            cond do
              token.kind == :dual_op ->
                parse_unary(token, state, context, log, 300)

              # ternary_op ://" used as unary (e.g., //foo) becomes {:/,_,[{:/,_,nil},rhs]}
              token.kind == :ternary_op and token.value == :"//" ->
                parse_ternary_unary(token, state, context, log)

              true ->
                # Just convert to literal AST, no do-block or no-parens call check
                ast = literal_to_ast(token)
                {:ok, ast, state, log}
            end
        end
    end
  end

  # Handle unary operators in nud with a minimum binding power constraint
  # The min_bp is passed through to argument parsing for no-parens calls
  defp nud_with_min_bp(token, state, context, log, min_bp) do
    case token.kind do
      :error_token ->
        meta = build_meta(token.metadata)
        ast = Builder.Helpers.error(token.value, meta)
        {:ok, ast, state, log}

      # capture_int (&) followed by int - special form like &1, &2
      :capture_int ->
        parse_capture_int(token, state, log)

      # Container tokens need to be handled by Containers.parse
      kind when kind in [:"[", :"{", :"(", :"<<", :%{}, :%] ->
        state = TokenAdapter.pushback(state, token)
        alias ToxicParser.Grammar.Containers
        # Containers.parse will call led after parsing the container
        Containers.parse(state, context, log)

      _ ->
        case Precedence.unary(token.kind) do
          {bp, _assoc} ->
            # This is a unary operator - parse operand
            parse_unary(token, state, context, log, bp)

          nil ->
            cond do
              # dual_op used as unary (e.g., -1, +1)
              token.kind == :dual_op ->
                # dual_op as unary has fixed precedence
                parse_unary(token, state, context, log, 300)

              # ternary_op ://" used as unary (e.g., //foo) becomes {:/,_,[{:/,_,nil},rhs]}
              token.kind == :ternary_op and token.value == :"//" ->
                parse_ternary_unary(token, state, context, log)

              true ->
                # Not a unary operator - convert to literal AST
                ast = literal_to_ast(token)
                # Check if identifier followed by do/end block or no-parens call
                maybe_do_block_with_min_bp(ast, token, state, context, log, min_bp)
            end
        end
    end
  end

  # Parse capture_int followed by int (e.g., &1, &2)
  # Rule: access_expr -> capture_int int : build_unary_op('$1', number_value('$2'))
  defp parse_capture_int(capture_token, state, log) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: :int} = int_token, state} ->
        meta = build_meta(capture_token.metadata)
        # Extract the integer value from the int token
        int_value = literal_to_ast(int_token)
        ast = {:&, meta, [int_value]}
        {:ok, ast, state, log}

      {:ok, other_token, state} ->
        {:error, {:expected, :int, got: other_token.kind}, state, log}

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Check for do/end block after an identifier (for RHS of binary ops)
  # Handles both :identifier and :do_identifier (e.g., `if`, `unless`, `case`, etc.)
  # Note: This function returns JUST the AST without calling led, so the caller
  # can call led with the appropriate min_bp.
  # This version threads min_bp through to argument parsing so that operators
  # like -> in guard expressions are not consumed as binary operators.
  defp maybe_do_block_with_min_bp(ast, token, state, context, log, min_bp) do
    case token.kind do
      # do_identifier can attach do-blocks (e.g., `if cond do ... end`)
      :do_identifier ->
        case TokenAdapter.peek(state) do
          {:ok, %{kind: :do}, _} ->
            # In matched context, do_identifier tokens should NOT consume the do-block
            # The do-block belongs to an outer call, not to this argument
            if context == :matched do
              {:ok, ast, state, log}
            else
              with {:ok, {block_meta, sections}, state, log} <-
                     Blocks.parse_do_block(state, context, log) do
                token_meta = Builder.Helpers.token_meta(token.metadata)
                call_ast = {token.value, block_meta ++ token_meta, [sections]}
                {:ok, call_ast, state, log}
              end
            end

          {:ok, next_tok, _} ->
            # do_identifier can also have no-parens args before do-block
            if is_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok) do
              parse_no_parens_call_nud_with_min_bp(token, state, context, log, min_bp)
            else
              {:ok, ast, state, log}
            end

          _ ->
            {:ok, ast, state, log}
        end

      # op_identifier indicates no-parens call (tokenizer detected arg-like token follows)
      # e.g., `foo -1` tokenizes foo as :op_identifier
      :op_identifier ->
        case TokenAdapter.peek(state) do
          {:ok, next_tok, _} ->
            if is_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok) do
              parse_no_parens_call_nud_with_min_bp(token, state, context, log, min_bp)
            else
              {:ok, ast, state, log}
            end

          _ ->
            {:ok, ast, state, log}
        end

      # Plain :identifier - can start no-parens calls but NOT when followed by dual_op
      # e.g., `x + y` - x is :identifier, + is dual_op with spaces, so it's binary op
      # e.g., `if a do :ok end` - if is :identifier, a is do_identifier, so it's no-parens call
      :identifier ->
        case TokenAdapter.peek(state) do
          {:ok, %{kind: :do}, _} ->
            # identifier followed by do - parse as call with do-block
            with {:ok, {block_meta, sections}, state, log} <-
                   Blocks.parse_do_block(state, context, log) do
              token_meta = Builder.Helpers.token_meta(token.metadata)
              call_ast = {token.value, block_meta ++ token_meta, [sections]}
              {:ok, call_ast, state, log}
            end

          {:ok, next_tok, _} ->
            # For plain identifiers, only parse no-parens call if next token is NOT dual_op
            # (dual_op after identifier with spaces is binary operator, not unary argument)
            if next_tok.kind != :dual_op and
                 (is_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok)) do
              parse_no_parens_call_nud_with_min_bp(token, state, context, log, min_bp)
            else
              {:ok, ast, state, log}
            end

          _ ->
            {:ok, ast, state, log}
        end

      # Other tokens (literals, etc.) - just return AST
      _ ->
        {:ok, ast, state, log}
    end
  end

  # Parse a no-parens call in nud context (for identifiers followed by args)
  # This is similar to Calls.parse_no_parens_call but doesn't call led at the end
  # This version uses min_bp to stop argument parsing before certain operators
  defp parse_no_parens_call_nud_with_min_bp(callee_tok, state, context, log, min_bp) do
    with {:ok, args, state, log} <-
           parse_no_parens_args_with_min_bp([], state, context, log, min_bp) do
      callee = callee_tok.value
      meta = Builder.Helpers.token_meta(callee_tok.metadata)
      ast = {callee, meta, args}
      # Check for do-block ONLY, don't call led (caller will do that)
      maybe_do_block_no_led(ast, state, context, log)
    end
  end

  # Check for do-block after call, but don't call led
  defp maybe_do_block_no_led(ast, state, context, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :do}, _} ->
        with {:ok, {block_meta, sections}, state, log} <-
               Blocks.parse_do_block(state, context, log) do
          ast =
            case ast do
              {name, meta, args} when is_list(args) ->
                # Prepend do/end metadata to the call's existing metadata
                {name, block_meta ++ meta, args ++ [sections]}
            end

          {:ok, ast, state, log}
        end

      _ ->
        {:ok, ast, state, log}
    end
  end

  # Parse no-parens call arguments with a minimum binding power constraint
  # This stops parsing before operators with binding power < min_bp
  defp parse_no_parens_args_with_min_bp(acc, state, ctx, log, min_bp) do
    case TokenAdapter.peek(state) do
      # Check for operator that should stop us
      {:ok, %{kind: kind}, _} when kind in [:eoe, :")", :"]", :"}", :do] ->
        {:ok, Enum.reverse(acc), state, log}

      {:ok, %{kind: kind} = tok, _} ->
        # Check if we should stop at this operator due to min_bp
        case Precedence.binary(kind) do
          {bp, _} when bp < min_bp ->
            # Stop before this operator (e.g., -> with bp=10 when min_bp=11)
            {:ok, Enum.reverse(acc), state, log}

          _ ->
            cond do
              Keywords.starts_kw?(tok) ->
                # kw_list is already [x: 1], we need to wrap it once for args: [[x: 1]]
                with {:ok, kw_list, state, log} <- Keywords.parse_kw_call(state, ctx, log) do
                  {:ok, Enum.reverse([kw_list | acc]), state, log}
                end

              true ->
                # Parse arg with min_bp constraint
                with {:ok, arg, state, log} <- parse_with_min_bp(state, :matched, log, min_bp) do
                  case TokenAdapter.peek(state) do
                    {:ok, %{kind: :","}, _} ->
                      {:ok, _comma, state} = TokenAdapter.next(state)
                      parse_no_parens_args_with_min_bp([arg | acc], state, ctx, log, min_bp)

                    _ ->
                      {:ok, Enum.reverse([arg | acc]), state, log}
                  end
                end
            end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse unary operator expression
  defp parse_unary(op_token, state, context, log, min_bp) do
    # Skip EOE after unary operator (allows "!\ntrue")
    # Note: Unlike binary ops, unary ops don't include newlines metadata
    {state, _newlines} = skip_eoe_after_op(state)

    case TokenAdapter.next(state) do
      {:ok, operand_token, state} ->
        # Special case: at_op + bracket_identifier (e.g., @foo[1])
        # Grammar: bracket_at_expr -> at_op_eol dot_bracket_identifier bracket_arg
        # The @foo becomes the subject, bracket access is handled at outer level
        if op_token.kind == :at_op and operand_token.kind == :bracket_identifier do
          op = op_token.value
          meta = build_meta(op_token.metadata)
          operand = Builder.Helpers.from_token(operand_token)
          ast = Builder.Helpers.unary(op, operand, meta)
          {:ok, ast, state, log}
        else
          with {:ok, operand, state, log} <- parse_rhs(operand_token, state, context, log, min_bp) do
            op = op_token.value
            meta = build_meta(op_token.metadata)
            ast = Builder.Helpers.unary(op, operand, meta)
            {:ok, ast, state, log}
          end
        end

      {:eof, state} ->
        # Special case: ... and .. can be standalone at EOF
        if op_token.kind in [:ellipsis_op, :range_op] do
          ast = {op_token.value, build_meta(op_token.metadata), []}
          {:ok, ast, state, log}
        else
          {:error, :unexpected_eof, state, log}
        end

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse ternary_op :"//" as binary operator (e.g., 1..10//2)
  # When left is a range {:.., meta, [start, stop]}, combines into {:..//, meta, [start, stop, step]}
  # Otherwise, produces an error (// must follow ..)
  defp parse_ternary_op(left, state, min_bp, context, log) do
    {:ok, op_token, state} = TokenAdapter.next(state)
    {bp, _assoc} = Precedence.binary(:ternary_op)

    # Skip EOE after operator
    {state, _newlines} = skip_eoe_after_op(state)

    case TokenAdapter.next(state) do
      {:ok, rhs_token, state} ->
        with {:ok, step, state, log} <- parse_rhs(rhs_token, state, context, log, bp) do
          case left do
            # Range expression: {:.., meta, [start, stop]} -> {:..//, meta, [start, stop, step]}
            {:.., range_meta, [start, stop]} ->
              combined = {:..//, range_meta, [start, stop, step]}
              led(combined, state, log, min_bp, context)

            # Not a range - this is an error but we still parse it as {:"//", meta, [left, step]}
            # The error handling follows spitfire's pattern
            _ ->
              op_meta = build_meta(op_token.metadata)
              combined = {op_token.value, op_meta, [left, step]}
              led(combined, state, log, min_bp, context)
          end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse ternary_op :"//" as unary operator (e.g., //foo)
  # This produces: {:/, outer_meta, [{:/, inner_meta, nil}, rhs]}
  # where outer has column+1 and inner has original column
  defp parse_ternary_unary(op_token, state, context, log) do
    meta = build_meta(op_token.metadata)

    # Calculate inner/outer metadata with column adjustment
    {outer_meta, inner_meta} =
      case {Keyword.get(meta, :line), Keyword.get(meta, :column)} do
        {line, col} when is_integer(line) and is_integer(col) ->
          rest_meta = Keyword.drop(meta, [:line, :column])
          {[line: line, column: col + 1] ++ rest_meta, [line: line, column: col] ++ rest_meta}

        _ ->
          {meta, meta}
      end

    # Skip EOE after operator
    {state, _newlines} = skip_eoe_after_op(state)

    # Parse the operand with unary precedence
    case TokenAdapter.next(state) do
      {:ok, operand_token, state} ->
        with {:ok, operand, state, log} <- parse_rhs(operand_token, state, context, log, 300) do
          # Build: {:/, outer_meta, [{:/, inner_meta, nil}, operand]}
          ast = {:/, outer_meta, [{:/, inner_meta, nil}, operand]}
          {:ok, ast, state, log}
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse RHS of binary operator, handling chained operators with proper precedence
  # For identifiers that could be calls with do-blocks (like `if true do...end`),
  # we need special handling but must preserve min_bp for associativity
  defp parse_rhs(token, state, context, log, min_bp) do
    # Check if this is an identifier that could be a call with arguments
    cond do
      Identifiers.classify(token.kind) != :other ->
        # Handle identifier specially to preserve min_bp
        parse_rhs_identifier(token, state, context, log, min_bp)

      # Container tokens need Expressions.expr to handle them properly
      token.kind in [:"[", :"{", :"(", :"<<", :%{}, :%] ->
        state = TokenAdapter.pushback(state, token)
        alias ToxicParser.Grammar.Expressions

        with {:ok, right, state, log} <- Expressions.expr(state, context, log) do
          led(right, state, log, min_bp, context)
        end

      # String tokens need Expressions.expr to handle them properly
      token.kind in [
        :bin_string_start,
        :list_string_start,
        :bin_heredoc_start,
        :list_heredoc_start,
        :sigil_start
      ] ->
        state = TokenAdapter.pushback(state, token)
        alias ToxicParser.Grammar.Expressions

        with {:ok, right, state, log} <- Expressions.expr(state, context, log) do
          led(right, state, log, min_bp, context)
        end

      true ->
        with {:ok, right, state, log} <- nud(token, state, context, log) do
          led(right, state, log, min_bp, context)
        end
    end
  end

  # Parse identifier on RHS, handling calls while preserving min_bp for associativity
  defp parse_rhs_identifier(token, state, context, log, min_bp) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"("}, _} ->
        # Paren call - parse directly to preserve min_bp
        # Don't use Calls.parse because it calls led(_, 0, _) which loses min_bp
        with {:ok, ast, state, log} <- parse_paren_call_base(token, state, context, log) do
          led(ast, state, log, min_bp, context)
        end

      {:ok, %{kind: :do}, _} ->
        # Do-block - delegate to Calls.parse_without_led to preserve min_bp
        state = TokenAdapter.pushback(state, token)

        with {:ok, right, state, log} <- Calls.parse_without_led(state, context, log) do
          led(right, state, log, min_bp, context)
        end

      {:ok, next_tok, _} ->
        cond do
          # op_identifier means tokenizer determined this is a no-parens call
          # (e.g., `a -2` where `-2` is unary argument, not binary subtraction)
          # This must be checked BEFORE binary operator check
          token.kind == :op_identifier and
              (is_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok)) ->
            state = TokenAdapter.pushback(state, token)

            with {:ok, right, state, log} <- Calls.parse_without_led(state, context, log) do
              led(right, state, log, min_bp, context)
            end

          # Binary operator follows - just return identifier, led will handle with min_bp
          bp(next_tok.kind) ->
            ast = Builder.Helpers.from_token(token)
            led(ast, state, log, min_bp, context)

          # Could be no-parens call argument - delegate to Calls
          is_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok) ->
            state = TokenAdapter.pushback(state, token)

            with {:ok, right, state, log} <- Calls.parse_without_led(state, context, log) do
              led(right, state, log, min_bp, context)
            end

          # Just a bare identifier
          true ->
            ast = Builder.Helpers.from_token(token)
            led(ast, state, log, min_bp, context)
        end

      _ ->
        # EOF or error - just return identifier
        ast = Builder.Helpers.from_token(token)
        led(ast, state, log, min_bp, context)
    end
  end

  # Parse a paren call without calling led at the end
  # Used by parse_rhs_identifier to preserve min_bp for associativity
  defp parse_paren_call_base(callee_tok, state, context, log) do
    {:ok, _open_tok, state} = TokenAdapter.next(state)

    # Skip leading EOE and count newlines
    {state, leading_newlines} = skip_eoe_count_newlines(state, 0)

    with {:ok, args, state, log} <-
           ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, context, log),
         # Skip trailing EOE before close paren
         {state, trailing_newlines} = skip_eoe_count_newlines(state, 0),
         {:ok, close_tok, state} <- expect_close_paren(state) do
      # For non-empty calls, only count leading newlines
      # For empty calls, count all newlines
      total_newlines =
        if args == [] do
          leading_newlines + trailing_newlines
        else
          leading_newlines
        end

      callee_meta = build_meta(callee_tok.metadata)
      close_meta = build_meta(close_tok.metadata)

      # Build metadata: [newlines: N, closing: [...], line: L, column: C]
      newlines_meta = if total_newlines > 0, do: [newlines: total_newlines], else: []
      meta = newlines_meta ++ [closing: close_meta] ++ callee_meta

      ast = {callee_tok.value, meta, Enum.reverse(args)}
      {:ok, ast, state, log}
    end
  end

  defp expect_close_paren(state) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: :")"} = tok, state} -> {:ok, tok, state}
      {:ok, tok, state} -> {:error, {:expected, :")", got: tok.kind}, state}
      {:eof, state} -> {:error, :unexpected_eof, state}
      {:error, diag, state} -> {:error, diag, state}
    end
  end

  # Check if a token can be the start of a no-parens call argument
  defp is_no_parens_arg?(%{kind: kind}) do
    kind in [
      :int,
      :flt,
      :char,
      :atom,
      :string,
      :identifier,
      :do_identifier,
      :alias,
      true,
      false,
      nil,
      :"{",
      :"[",
      :"<<",
      :unary_op,
      :at_op,
      :capture_op,
      :dual_op
    ]
  end

  @doc """
  Continue parsing with left-hand expression, looking for trailing operators.
  This is exposed for modules like Calls that need to continue parsing after
  building a call expression.
  """
  def led(left, state, log, min_bp, context) do
    case TokenAdapter.peek(state) do
      {:ok, next_token, _} ->
        case {next_token.kind, Precedence.binary(next_token.kind)} do
          # Parens call: identifier(args) or expr()(args) (nested call)
          # Rule: parens_call -> dot_call_identifier call_args_parens call_args_parens
          {:"(", _} ->
            {:ok, _open_tok, state} = TokenAdapter.next(state)

            # Skip leading EOE and count newlines
            {state, leading_newlines} = skip_eoe_count_newlines(state, 0)

            with {:ok, args, state, log} <-
                   ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, context, log) do
              # Skip trailing EOE before close paren
              {state, trailing_newlines} = skip_eoe_count_newlines(state, 0)

              case TokenAdapter.next(state) do
                {:ok, %{kind: :")"} = close_tok, state} ->
                  # For non-empty calls, only count leading newlines
                  # For empty calls, count all newlines
                  total_newlines =
                    if args == [] do
                      leading_newlines + trailing_newlines
                    else
                      leading_newlines
                    end

                  close_meta = build_meta(close_tok.metadata)
                  # Get line/column from left AST (the callee)
                  callee_meta = extract_meta(left)

                  newlines_meta = if total_newlines > 0, do: [newlines: total_newlines], else: []
                  call_meta = newlines_meta ++ [closing: close_meta] ++ callee_meta

                  # If left is a simple identifier {name, meta, nil}, convert to call {name, call_meta, args}
                  # Otherwise it's a nested call: {left, call_meta, args}
                  combined =
                    case left do
                      {name, _meta, nil} when is_atom(name) ->
                        {name, call_meta, Enum.reverse(args)}

                      _ ->
                        {left, call_meta, Enum.reverse(args)}
                    end

                  # Check for nested calls and do-blocks (foo() do...end, foo()() do...end)
                  maybe_nested_call_or_do_block(combined, state, log, min_bp, context)

                {:ok, other, state} ->
                  {:error, {:expected, :")", got: other.kind}, state, log}

                {:eof, state} ->
                  {:error, :unexpected_eof, state, log}

                {:error, diag, state} ->
                  {:error, diag, state, log}
              end
            end

          # dot_call_op: expr.(args) - anonymous function call
          {:dot_call_op, _} ->
            {:ok, dot_tok, state} = TokenAdapter.next(state)
            dot_meta = build_meta(dot_tok.metadata)

            # The dot_call_op is followed by (args)
            # Build: {{:., dot_meta, [left]}, call_meta, args}
            # consume (
            {:ok, _open_tok, state} = TokenAdapter.next(state)

            # Skip leading EOE and count newlines
            {state, leading_newlines} = skip_eoe_count_newlines(state, 0)

            with {:ok, args, state, log} <-
                   ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, context, log) do
              # Skip trailing EOE before close paren
              {state, trailing_newlines} = skip_eoe_count_newlines(state, 0)

              case TokenAdapter.next(state) do
                {:ok, %{kind: :")"} = close_tok, state} ->
                  total_newlines = leading_newlines + trailing_newlines
                  close_meta = build_meta(close_tok.metadata)

                  newlines_meta = if total_newlines > 0, do: [newlines: total_newlines], else: []
                  call_meta = newlines_meta ++ [closing: close_meta] ++ dot_meta

                  combined = {{:., dot_meta, [left]}, call_meta, Enum.reverse(args)}
                  # Check for nested calls (foo.()()) and do-blocks (foo.() do ... end)
                  maybe_nested_call_or_do_block(combined, state, log, min_bp, context)

                {:ok, other, state} ->
                  {:error, {:expected, :")", got: other.kind}, state, log}

                {:eof, state} ->
                  {:error, :unexpected_eof, state, log}

                {:error, diag, state} ->
                  {:error, diag, state, log}
              end
            end

          {:dot_op, {bp, _}} when bp >= min_bp ->
            {:ok, dot_tok, state} = TokenAdapter.next(state)
            dot_meta = build_meta(dot_tok.metadata)

            # Check for dot_container: expr.{...}
            case TokenAdapter.peek(state) do
              {:ok, %{kind: :"{"}, _} ->
                {:ok, _open, state} = TokenAdapter.next(state)

                with {:ok, args, newlines, close_meta, state, log} <-
                       parse_dot_container_args(state, context, log) do
                  # Build: {{:., dot_meta, [left, :{}]}, [newlines: n, closing: close_meta] ++ dot_meta, args}
                  # Only add newlines to metadata if > 0
                  newlines_meta = if newlines > 0, do: [newlines: newlines], else: []

                  combined =
                    {{:., dot_meta, [left, :{}]},
                     newlines_meta ++ [closing: close_meta] ++ dot_meta, args}

                  led(combined, state, log, min_bp, context)
                end

              _ ->
                with {:ok, rhs, state, log} <- Dots.parse_member(state, context, log) do
                  combined =
                    case rhs do
                      # Simple identifier: {member_atom, member_meta}
                      # Build: {{:., dot_meta, [left, member]}, [no_parens: true | member_meta], []}
                      {member, member_meta} when is_atom(member) ->
                        {{:., dot_meta, [left, member]}, [no_parens: true] ++ member_meta, []}

                      # Alias on RHS - build combined __aliases__ (dot_alias rule)
                      # Must come before the general {name, meta, args} pattern
                      {:__aliases__, rhs_meta, [rhs_alias]} ->
                        build_dot_alias(left, rhs_alias, rhs_meta, dot_meta)

                      # Call with args: {name, meta, args}
                      {name, meta, args} when is_list(args) ->
                        {{:., dot_meta, [left, name]}, meta, args}

                      # Other AST node
                      other ->
                        {:., dot_meta, [left, other]}
                    end

                  case TokenAdapter.peek(state) do
                    {:ok, %{kind: :"("}, _} ->
                      {:ok, _open, state} = TokenAdapter.next(state)
                      # Skip leading EOE and count newlines
                      {state, leading_newlines} = skip_eoe_count_newlines(state, 0)

                      with {:ok, args, state, log} <-
                             ToxicParser.Grammar.CallsPrivate.parse_paren_args(
                               [],
                               state,
                               context,
                               log
                             ) do
                        # Skip trailing EOE before close paren
                        {state, trailing_newlines} = skip_eoe_count_newlines(state, 0)

                        case TokenAdapter.next(state) do
                          {:ok, %{kind: :")"} = close_tok, state} ->
                            total_newlines = leading_newlines + trailing_newlines
                            close_meta = build_meta(close_tok.metadata)

                            # When followed by parens, convert to call form with proper metadata
                            combined =
                              dot_to_call_with_meta(combined, args, total_newlines, close_meta)

                            # Check for nested calls and do-blocks (foo.bar() do...end)
                            maybe_nested_call_or_do_block(combined, state, log, min_bp, context)

                          {:ok, other, state} ->
                            {:error, {:expected, :")", got: other.kind}, state, log}

                          {:eof, state} ->
                            {:error, :unexpected_eof, state, log}

                          {:error, diag, state} ->
                            {:error, diag, state, log}
                        end
                      end

                    {:ok, %{kind: :"["}, _} ->
                      # Bracket access: foo.bar[:key] - handle via led() which processes [
                      led(combined, state, log, min_bp, context)

                    {:ok, %{kind: :do}, _} ->
                      # Do-block after dot call: foo.bar() do...end
                      # Only handle when rhs was a call (combined has args list)
                      maybe_nested_call_or_do_block(combined, state, log, min_bp, context)

                    {:ok, next_tok, _} ->
                      # Check for no-parens call argument after dot expression
                      if can_be_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok) do
                        with {:ok, args, state, log} <-
                               Calls.parse_no_parens_args([], state, context, log) do
                          combined = dot_to_no_parens_call(combined, args)
                          # Check for do-blocks after no-parens call: foo.bar arg do...end
                          maybe_nested_call_or_do_block(combined, state, log, min_bp, context)
                        end
                      else
                        led(combined, state, log, min_bp, context)
                      end

                    _ ->
                      led(combined, state, log, min_bp, context)
                  end
                end
            end

          {:"[", _} ->
            {:ok, open_tok, state} = TokenAdapter.next(state)

            # Skip leading EOE and count newlines (only leading newlines matter for metadata)
            {state, leading_newlines} = skip_eoe_count_newlines(state, 0)

            with {:ok, indices, state, log} <- parse_access_indices([], state, context, log) do
              # Skip trailing EOE before close bracket (don't count these)
              {state, _trailing_newlines} = skip_eoe_count_newlines(state, 0)

              case TokenAdapter.next(state) do
                {:ok, %{kind: :"]"} = close_tok, state} ->
                  # Build metadata with from_brackets, newlines, closing, line, column
                  open_meta = build_meta(open_tok.metadata)
                  close_meta = build_meta(close_tok.metadata)

                  newlines_meta =
                    if leading_newlines > 0, do: [newlines: leading_newlines], else: []

                  bracket_meta =
                    [from_brackets: true] ++ newlines_meta ++ [closing: close_meta] ++ open_meta

                  combined =
                    {{:., bracket_meta, [Access, :get]}, bracket_meta,
                     [left | Enum.reverse(indices)]}

                  led(combined, state, log, min_bp, context)

                {:ok, other, state} ->
                  {:error, {:expected, :"]", got: other.kind}, state, log}

                {:eof, state} ->
                  {:error, :unexpected_eof, state, log}

                {:error, diag, state} ->
                  {:error, diag, state, log}
              end
            end

          # Special case: ternary_op :"//" following range_op :".."
          # Combines into {:..//, meta, [start, stop, step]}
          {:ternary_op, {bp, _assoc}} when bp >= min_bp ->
            parse_ternary_op(left, state, min_bp, context, log)

          {_, {bp, assoc}} when bp >= min_bp ->
            {:ok, op_token, state} = TokenAdapter.next(state)
            # For right associativity, use same bp to allow chaining at same level
            # For left associativity, use bp+1 to prevent same-level ops from binding to RHS
            rhs_min_bp = if assoc == :right, do: bp, else: bp + 1

            # Skip EOE tokens after operator (allows "1 +\n2")
            {state, newlines} = skip_eoe_after_op(state)

            case TokenAdapter.peek(state) do
              # Special case: when operator followed by keyword list
              # Grammar rule: no_parens_op_expr -> when_op_eol call_args_no_parens_kw
              # This applies regardless of context (not just :no_parens)
              {:ok, rhs_tok, _} when op_token.kind == :when_op ->
                if Keywords.starts_kw?(rhs_tok) do
                  with {:ok, kw_list, state, log} <- Keywords.parse_kw_call(state, context, log) do
                    op = op_token.value
                    meta = build_meta_with_newlines(op_token.metadata, newlines)
                    combined = Builder.Helpers.binary(op, left, kw_list, meta)
                    led(combined, state, log, min_bp, context)
                  end
                else
                  parse_binary_rhs(
                    state,
                    left,
                    op_token,
                    rhs_min_bp,
                    min_bp,
                    newlines,
                    context,
                    log
                  )
                end

              _ ->
                parse_binary_rhs(
                  state,
                  left,
                  op_token,
                  rhs_min_bp,
                  min_bp,
                  newlines,
                  context,
                  log
                )
            end

          _ ->
            {:ok, left, state, log}
        end

      {:eof, state} ->
        {:ok, left, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Handle nested paren calls (foo.()(), foo()()) and do-blocks (foo.() do...end, foo() do...end)
  # Grammar rules:
  #   parens_call -> dot_call_identifier call_args_parens call_args_parens
  #   block_expr -> dot_call_identifier call_args_parens do_block
  #   block_expr -> dot_call_identifier call_args_parens call_args_parens do_block
  defp maybe_nested_call_or_do_block(ast, state, log, min_bp, context) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"("}, _} ->
        # Another paren call - parse it
        {:ok, _open_tok, state} = TokenAdapter.next(state)

        # Skip leading EOE and count newlines
        {state, leading_newlines} = skip_eoe_count_newlines(state, 0)

        with {:ok, args, state, log} <-
               ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, context, log) do
          # Skip trailing EOE before close paren
          {state, trailing_newlines} = skip_eoe_count_newlines(state, 0)

          case TokenAdapter.next(state) do
            {:ok, %{kind: :")"} = close_tok, state} ->
              total_newlines =
                if args == [] do
                  leading_newlines + trailing_newlines
                else
                  leading_newlines
                end

              # Get line/column from callee AST
              callee_meta = extract_meta(ast)
              close_meta = build_meta(close_tok.metadata)

              newlines_meta = if total_newlines > 0, do: [newlines: total_newlines], else: []
              meta = newlines_meta ++ [closing: close_meta] ++ callee_meta

              combined = {ast, meta, Enum.reverse(args)}
              # Recurse for chained calls like foo()()()
              maybe_nested_call_or_do_block(combined, state, log, min_bp, context)

            {:ok, other, state} ->
              {:error, {:expected, :")", got: other.kind}, state, log}

            {:eof, state} ->
              {:error, :unexpected_eof, state, log}

            {:error, diag, state} ->
              {:error, diag, state, log}
          end
        end

      {:ok, %{kind: :do}, _} ->
        # Do-block - parse and attach to call
        with {:ok, {block_meta, sections}, state, log} <-
               Blocks.parse_do_block(state, context, log) do
          combined =
            case ast do
              {name, meta, args} when is_list(args) ->
                # Prepend do/end metadata to the call's existing metadata
                # Remove no_parens: true as it doesn't apply with do-blocks
                clean_meta = Keyword.delete(meta, :no_parens)
                {name, block_meta ++ clean_meta, args ++ [sections]}

              # Bare identifier: {name, meta, nil} - convert to call with do-block
              {name, meta, nil} when is_atom(name) ->
                {name, block_meta ++ meta, [sections]}

              other ->
                Builder.Helpers.call(other, [sections], block_meta)
            end

          # Continue with led() to handle trailing operators
          led(combined, state, log, min_bp, context)
        end

      _ ->
        # No nested call or do-block - continue with led
        led(ast, state, log, min_bp, context)
    end
  end

  # Convert a dot expression to a call when followed by parens, with closing metadata
  # If id_meta already has :closing, this is a nested call - wrap the whole thing
  defp dot_to_call_with_meta(
         {{:., dot_meta, dot_args}, id_meta, _inner_args} = callee,
         args,
         newlines,
         close_meta
       ) do
    if Keyword.has_key?(id_meta, :closing) do
      # Already a call - wrap it as nested call
      callee_meta = Keyword.take(id_meta, [:line, :column])
      newlines_meta = if newlines > 0, do: [newlines: newlines], else: []
      call_meta = newlines_meta ++ [closing: close_meta] ++ callee_meta
      {callee, call_meta, Enum.reverse(args)}
    else
      # First call on dot expression - convert to call form
      base_meta = Keyword.delete(id_meta, :no_parens)
      newlines_meta = if newlines > 0, do: [newlines: newlines], else: []
      call_meta = newlines_meta ++ [closing: close_meta] ++ base_meta
      {{:., dot_meta, dot_args}, call_meta, Enum.reverse(args)}
    end
  end

  defp dot_to_call_with_meta({:., dot_meta, dot_args}, args, newlines, close_meta) do
    newlines_meta = if newlines > 0, do: [newlines: newlines], else: []
    call_meta = newlines_meta ++ [closing: close_meta] ++ dot_meta
    {{:., dot_meta, dot_args}, call_meta, Enum.reverse(args)}
  end

  defp dot_to_call_with_meta(other, args, newlines, close_meta) do
    newlines_meta = if newlines > 0, do: [newlines: newlines], else: []
    meta = newlines_meta ++ [closing: close_meta]
    {other, meta, Enum.reverse(args)}
  end

  # Convert a dot expression to a no-parens call
  # {{:., dot_meta, [left, member]}, [no_parens: true | id_meta], []} + args
  # -> {{:., dot_meta, [left, member]}, id_meta, args}
  defp dot_to_no_parens_call({{:., dot_meta, dot_args}, id_meta, []}, args) do
    # Remove no_parens: true for the call form
    call_meta = Keyword.delete(id_meta, :no_parens)
    {{:., dot_meta, dot_args}, call_meta, args}
  end

  defp dot_to_no_parens_call({:., dot_meta, dot_args}, args) do
    {{:., dot_meta, dot_args}, [], args}
  end

  defp dot_to_no_parens_call(other, args) do
    Builder.Helpers.call(other, args)
  end

  # Check if a token can be the start of a no-parens call argument
  defp can_be_no_parens_arg?(%{kind: kind}) do
    kind in [
      :int,
      :flt,
      :char,
      :atom,
      :string,
      :identifier,
      :do_identifier,
      :alias,
      true,
      false,
      nil,
      :"{",
      :"[",
      :"<<",
      :unary_op,
      :at_op,
      :capture_op,
      :dual_op
    ]
  end

  # Build combined __aliases__ for dot_alias rule (Foo.Bar -> {:__aliases__, meta, [:Foo, :Bar]})
  # When left is already an __aliases__, append the new segment
  defp build_dot_alias({:__aliases__, left_meta, left_segments}, rhs_alias, rhs_meta, _dot_meta) do
    # Extract just the line/column from rhs_meta's :last value (or from rhs_meta itself)
    last_meta = Keyword.get(rhs_meta, :last, rhs_meta)
    new_meta = Keyword.put(left_meta, :last, last_meta)
    {:__aliases__, new_meta, left_segments ++ [rhs_alias]}
  end

  # When left is some other expression, wrap both in __aliases__
  # Meta is [last: alias_location] ++ dot_location
  defp build_dot_alias(left_expr, rhs_alias, rhs_meta, dot_meta) do
    last_meta = Keyword.get(rhs_meta, :last, rhs_meta)
    {:__aliases__, [last: last_meta] ++ dot_meta, [left_expr, rhs_alias]}
  end

  # Parse container args for dot_container: expr.{A, B, ...}
  # Returns {:ok, args, newlines, close_meta, state, log}
  defp parse_dot_container_args(state, ctx, log) do
    # Skip leading EOE and count newlines
    {state, newlines} = skip_eoe_count_newlines(state, 0)

    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"}"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = build_meta(close_tok.metadata)
        {:ok, [], newlines, close_meta, state, log}

      {:ok, _, _} ->
        with {:ok, args, state, log} <- parse_dot_container_args_loop([], state, ctx, log) do
          # Skip trailing EOE before close
          {state, trailing_newlines} = skip_eoe_count_newlines(state, 0)
          total_newlines = newlines + trailing_newlines

          case TokenAdapter.next(state) do
            {:ok, %{kind: :"}"} = close_tok, state} ->
              close_meta = build_meta(close_tok.metadata)
              {:ok, args, total_newlines, close_meta, state, log}

            {:ok, other, state} ->
              {:error, {:expected, :"}", got: other.kind}, state, log}

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

  defp parse_dot_container_args_loop(acc, state, ctx, log) do
    alias ToxicParser.Grammar.Expressions

    # container_args in elixir_parser.yrl allows unmatched_expr
    # Use :unmatched context to allow do-blocks inside container
    with {:ok, expr, state, log} <- Expressions.expr(state, :unmatched, log) do
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state)
          # After comma, check if we hit EOE or closing brace (trailing comma case)
          {state, _newlines} = skip_eoe_count_newlines(state, 0)

          case TokenAdapter.peek(state) do
            {:ok, %{kind: :"}"}, _} ->
              # Trailing comma - stop here
              {:ok, acc ++ [expr], state, log}

            {:ok, tok, _} ->
              # Check if this is keyword data (e.g., foo: x)
              if Keywords.starts_kw?(tok) do
                with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
                  {:ok, acc ++ [expr, kw_list], state, log}
                end
              else
                parse_dot_container_args_loop(acc ++ [expr], state, ctx, log)
              end

            _ ->
              parse_dot_container_args_loop(acc ++ [expr], state, ctx, log)
          end

        _ ->
          {:ok, acc ++ [expr], state, log}
      end
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

  # Helper to parse RHS of binary operator
  defp parse_binary_rhs(state, left, op_token, rhs_min_bp, min_bp, newlines, context, log) do
    case TokenAdapter.next(state) do
      {:ok, rhs_token, state} ->
        with {:ok, right, state, log} <- parse_rhs(rhs_token, state, context, log, rhs_min_bp) do
          combined = build_binary_op(op_token, left, right, newlines)
          led(combined, state, log, min_bp, context)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Build binary operation AST, with special handling for "not in" rewrite
  # "not in" gets rewritten to {:not, NotMeta, [{:in, InMeta, [left, right]}]}
  defp build_binary_op(
         %{kind: :in_op, value: {:"not in", in_location}, metadata: meta},
         left,
         right,
         newlines
       ) do
    not_meta = build_meta_with_newlines(meta, newlines)
    in_meta = build_meta_from_location(in_location)
    {:not, not_meta, [{:in, in_meta, [left, right]}]}
  end

  # Deprecated "not expr1 in expr2" rewrite - when in_op follows {:not, _, [operand]} or {:!, _, [operand]}
  # Rewrites to {:not, InMeta, [{:in, InMeta, [operand, right]}]} or {:!, InMeta, [{:in, InMeta, [operand, right]}]}
  defp build_binary_op(
         %{kind: :in_op, value: :in, metadata: meta},
         {op, _op_meta, [operand]},
         right,
         _newlines
       )
       when op in [:not, :!] do
    in_meta = build_meta(meta)
    {op, in_meta, [{:in, in_meta, [operand, right]}]}
  end

  # Assoc operator (=>) annotates LHS with :assoc metadata
  defp build_binary_op(%{kind: :assoc_op, metadata: meta}, left, right, newlines) do
    op_meta = build_meta_with_newlines(meta, newlines)
    assoc_meta = Keyword.take(op_meta, [:line, :column])
    # Annotate LHS with :assoc metadata
    annotated_left = annotate_assoc(left, assoc_meta)
    Builder.Helpers.binary(:"=>", annotated_left, right, op_meta)
  end

  defp build_binary_op(op_token, left, right, newlines) do
    op = op_token.value
    meta = build_meta_with_newlines(op_token.metadata, newlines)
    Builder.Helpers.binary(op, left, right, meta)
  end

  # Annotate expression with :assoc metadata (for LHS of => operator)
  defp annotate_assoc({name, meta, args}, assoc_meta) when is_list(meta) do
    {name, [assoc: assoc_meta] ++ meta, args}
  end

  defp annotate_assoc(other, _assoc_meta), do: other

  # Build metadata from a raw Toxic location tuple
  defp build_meta_from_location({{line, column}, _, _}) do
    [line: line, column: column]
  end

  defp build_meta_from_location(_), do: []

  # Integer: extract parsed value from raw token metadata
  defp literal_to_ast(%{kind: :int, raw: {:int, {_, _, parsed_value}, _}}) do
    parsed_value
  end

  # Float: extract parsed value from raw token metadata
  defp literal_to_ast(%{kind: :flt, raw: {:flt, {_, _, parsed_value}, _}}) do
    parsed_value
  end

  # Character literal: value is already the codepoint
  defp literal_to_ast(%{kind: :char, value: codepoint}) do
    codepoint
  end

  # Boolean literals: kind is the literal value itself
  defp literal_to_ast(%{kind: true}), do: true
  defp literal_to_ast(%{kind: false}), do: false
  defp literal_to_ast(%{kind: nil}), do: nil

  # Atom: value is already the atom
  defp literal_to_ast(%{kind: :atom, value: atom}) do
    atom
  end

  # Alias: wrap in __aliases__ tuple with :last metadata
  defp literal_to_ast(%{kind: :alias, value: atom, metadata: meta}) do
    m = build_meta(meta)
    {:__aliases__, [last: m] ++ m, [atom]}
  end

  # Identifier: wrap in tuple with nil context (variable reference)
  defp literal_to_ast(%{kind: :identifier, value: atom, metadata: meta}) do
    {atom, build_meta(meta), nil}
  end

  # Paren identifier: same as identifier, ( will be handled by led
  defp literal_to_ast(%{kind: :paren_identifier, value: atom, metadata: meta}) do
    {atom, build_meta(meta), nil}
  end

  # Do identifier: same as identifier
  defp literal_to_ast(%{kind: :do_identifier, value: atom, metadata: meta}) do
    {atom, build_meta(meta), nil}
  end

  # String (for future use)
  defp literal_to_ast(%{kind: :string, value: value}) do
    value
  end

  # Range operator (..) as standalone expression
  defp literal_to_ast(%{kind: :range_op, value: op, metadata: meta}) do
    {op, build_meta(meta), []}
  end

  # Ellipsis operator (...) as standalone expression
  defp literal_to_ast(%{kind: :ellipsis_op, value: op, metadata: meta}) do
    {op, build_meta(meta), []}
  end

  # Fallback for other tokens
  defp literal_to_ast(%{value: value}) do
    value
  end

  # Build metadata for AST nodes
  defp build_meta(%{range: %{start: %{line: line, column: column}}}) do
    [line: line, column: column]
  end

  defp build_meta(_), do: []

  # Extract line/column metadata from an AST node (for nested calls)
  defp extract_meta({_name, meta, _args}) when is_list(meta) do
    Keyword.take(meta, [:line, :column])
  end

  defp extract_meta(_), do: []

  # Build metadata with newlines count if present
  defp build_meta_with_newlines(token_meta, 0) do
    build_meta(token_meta)
  end

  defp build_meta_with_newlines(token_meta, newlines) when newlines > 0 do
    [newlines: newlines] ++ build_meta(token_meta)
  end

  # Skip EOE tokens after an operator and count newlines
  defp skip_eoe_after_op(state, newlines \\ 0) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, value: %{newlines: n}}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe_after_op(state, newlines + n)

      _ ->
        {state, newlines}
    end
  end

  defp parse_access_indices(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"]"}, state} ->
        {:ok, acc, state, log}

      {:ok, _tok, _state} ->
        with {:ok, expr, state, log} <- parse(state, ctx, log) do
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :","}, state} ->
              {:ok, _comma, state} = TokenAdapter.next(state)
              parse_access_indices([expr | acc], state, ctx, log)

            {:ok, %{kind: :"]"}, _} ->
              parse_access_indices([expr | acc], state, ctx, log)

            _ ->
              {:error, {:expected_comma_or, :"]"}, state, log}
          end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Specialized led that ONLY handles dot operator for alias chaining.
  # Used by parse_base_with_dots for struct names like %Foo.Bar{}.
  # Does NOT handle paren calls, no-parens calls, bracket access, or do-blocks.
  defp led_dot_only(left, state, log, context) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :dot_op} = _dot_tok, _} ->
        {:ok, dot_tok, state} = TokenAdapter.next(state)
        dot_meta = build_meta(dot_tok.metadata)

        # Parse the RHS of dot - must be an alias or identifier
        with {:ok, rhs, state, log} <- Dots.parse_member(state, context, log) do
          combined =
            case rhs do
              # Alias on RHS - build combined __aliases__ (dot_alias rule)
              {:__aliases__, rhs_meta, [rhs_alias]} ->
                build_dot_alias(left, rhs_alias, rhs_meta, dot_meta)

              # Simple identifier: {member_atom, member_meta}
              {member, member_meta} when is_atom(member) ->
                {{:., dot_meta, [left, member]}, [no_parens: true] ++ member_meta, []}

              # Other AST node
              other ->
                {:., dot_meta, [left, other]}
            end

          # Continue to handle chained dots: Foo.Bar.Baz
          led_dot_only(combined, state, log, context)
        end

      _ ->
        # Not a dot operator - return what we have
        {:ok, left, state, log}
    end
  end
end
