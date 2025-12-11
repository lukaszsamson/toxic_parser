defmodule ToxicParser.Grammar.Calls do
  @moduledoc """
  Call parsing skeleton for Phase 5.

  This module classifies identifiers and dispatches to paren/no-parens call
  parsers. Full call argument parsing and ambiguity handling will be filled in
  subsequent iterations.
  """

  alias ToxicParser.{Builder, EventLog, Identifiers, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.{Blocks, Expressions, Keywords}

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
      {:ok, %{kind: :"("}, _} ->
        parse_paren_call(tok, state, ctx, log)

      {:ok, %{kind: :"["} = open_tok, _} when kind == :bracket_identifier ->
        # bracket_identifier followed by [ is bracket access: foo[:bar]
        # Grammar: bracket_expr -> dot_bracket_identifier bracket_arg
        parse_bracket_access(tok, open_tok, state, ctx, log)

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
          can_be_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok) ->
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
            maybe_do_block(ast, state, ctx, log)
        end

      # No next token or at terminator - check for do-block
      _ ->
        ast = Builder.Helpers.from_token(tok)
        maybe_do_block(ast, state, ctx, log)
    end
  end

  # Parse op_identifier call like "a -1" where the argument starts with a unary op
  # The tokenizer has determined this is a call, not a binary expression
  defp parse_op_identifier_call(callee_tok, state, ctx, log) do
    with {:ok, arg, state, log} <- Expressions.expr(state, ctx, log) do
      callee = callee_tok.value
      # ambiguous_op: nil comes first, per elixir_parser.yrl build_call for op_identifier
      meta = [ambiguous_op: nil] ++ Builder.Helpers.token_meta(callee_tok.metadata)
      ast = {callee, meta, [arg]}
      maybe_do_block(ast, state, ctx, log)
    end
  end

  # Parse bracket access expression: foo[:bar]
  # Grammar: bracket_expr -> dot_bracket_identifier bracket_arg
  # Produces: {{:., meta, [Access, :get]}, meta, [subject, key]}
  defp parse_bracket_access(ident_tok, _open_tok, state, ctx, log) do
    # Consume the opening bracket
    {:ok, open_tok, state} = TokenAdapter.next(state)

    # Skip leading EOE and count newlines (only leading newlines matter for metadata)
    {state, leading_newlines} = skip_eoe_count_newlines(state, 0)

    # Parse the bracket argument (container_expr or kw_data)
    with {:ok, arg, state, log} <- parse_bracket_arg_no_skip(state, ctx, log) do
      # Skip trailing EOE before close bracket (don't count these)
      {state, _trailing_newlines} = skip_eoe_count_newlines(state, 0)

      case expect_token(state, :"]") do
        {:ok, close_tok, state} ->
          # Build access expression
          subject = Builder.Helpers.from_token(ident_tok)
          bracket_meta = build_bracket_meta_with_newlines(open_tok, close_tok, leading_newlines)
          ast = build_access(subject, arg, bracket_meta)
          # Continue with led() to handle chained access like foo[:a][:b]
          Pratt.led(ast, state, log, 0, ctx)

        {:error, reason, state} ->
          {:error, reason, state, log}
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
                {:ok, expr, state, log}

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
    newlines_meta = if newlines > 0, do: [newlines: newlines], else: []
    [from_brackets: true] ++ newlines_meta ++ [closing: close_meta] ++ open_meta
  end

  # Build Access.get call AST
  defp build_access(subject, key, meta) do
    {{:., meta, [Access, :get]}, meta, [subject, key]}
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

  # Parse a no-parens call like `foo 1`, `foo 1, 2`, or `foo a: 1`.
  defp parse_no_parens_call(callee_tok, state, ctx, log) do
    with {:ok, args, state, log} <- parse_no_parens_args([], state, ctx, log) do
      callee = callee_tok.value
      meta = Builder.Helpers.token_meta(callee_tok.metadata)
      ast = {callee, meta, args}
      maybe_do_block(ast, state, ctx, log)
    end
  end

  defp parse_paren_call(callee_tok, state, ctx, log) do
    {:ok, _open_tok, state} = TokenAdapter.next(state)

    # Skip leading EOE and count newlines
    {state, leading_newlines} = skip_eoe_count_newlines(state, 0)

    with {:ok, args, state, log} <- parse_paren_args([], state, ctx, log),
         # Skip trailing EOE before close paren
         {state, trailing_newlines} = skip_eoe_count_newlines(state, 0),
         {:ok, close_tok, state} <- expect_token(state, :")") do
      # For non-empty calls, only count leading newlines
      # For empty calls, count all newlines
      total_newlines =
        if args == [] do
          leading_newlines + trailing_newlines
        else
          leading_newlines
        end

      callee_meta = Builder.Helpers.token_meta(callee_tok.metadata)
      close_meta = Builder.Helpers.token_meta(close_tok.metadata)

      # Build metadata: [newlines: N, closing: [...], line: L, column: C]
      newlines_meta = if total_newlines > 0, do: [newlines: total_newlines], else: []
      meta = newlines_meta ++ [closing: close_meta] ++ callee_meta

      ast = {callee_tok.value, meta, Enum.reverse(args)}
      # Check for nested call: foo()() - another paren call on the result
      maybe_nested_call(ast, state, ctx, log)
    end
  end

  # Handle nested paren calls: foo()(), foo(1)(2), etc.
  # Rule: parens_call -> dot_call_identifier call_args_parens call_args_parens
  defp maybe_nested_call(ast, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"("}, _} ->
        parse_nested_paren_call(ast, state, ctx, log)

      _ ->
        maybe_do_block(ast, state, ctx, log)
    end
  end

  defp parse_nested_paren_call(callee_ast, state, ctx, log) do
    {:ok, _open_tok, state} = TokenAdapter.next(state)

    # Skip leading EOE and count newlines
    {state, leading_newlines} = skip_eoe_count_newlines(state, 0)

    with {:ok, args, state, log} <- parse_paren_args([], state, ctx, log),
         # Skip trailing EOE before close paren
         {state, trailing_newlines} = skip_eoe_count_newlines(state, 0),
         {:ok, close_tok, state} <- expect_token(state, :")") do
      # For non-empty calls, only count leading newlines
      # For empty calls, count all newlines
      total_newlines =
        if args == [] do
          leading_newlines + trailing_newlines
        else
          leading_newlines
        end

      # Get line/column from callee AST
      callee_meta = extract_meta(callee_ast)
      close_meta = Builder.Helpers.token_meta(close_tok.metadata)

      newlines_meta = if total_newlines > 0, do: [newlines: total_newlines], else: []
      meta = newlines_meta ++ [closing: close_meta] ++ callee_meta

      ast = {callee_ast, meta, Enum.reverse(args)}
      # Recurse for chained calls like foo()()()
      maybe_nested_call(ast, state, ctx, log)
    end
  end

  defp extract_meta({_name, meta, _args}) when is_list(meta) do
    Keyword.take(meta, [:line, :column])
  end

  defp extract_meta(_), do: []

  defp skip_eoe_count_newlines(state, count) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, value: %{newlines: n}}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe_count_newlines(state, count + n)

      _ ->
        {state, count}
    end
  end

  defp expect_token(state, kind) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: ^kind} = token, state} -> {:ok, token, state}
      {:ok, token, state} -> {:error, {:expected, kind, got: token.kind}, state}
      {:eof, state} -> {:error, :unexpected_eof, state}
      {:error, diag, state} -> {:error, diag, state}
    end
  end

  defp parse_paren_args(acc, state, ctx, log), do: ToxicParser.Grammar.CallsPrivate.parse_paren_args(acc, state, ctx, log)

  @doc """
  Parse no-parens call arguments (exported for use by Pratt parser for dot calls).
  """
  def parse_no_parens_args(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: kind}, _} when kind in [:eoe, :")", :"]", :"}", :do] ->
        {:ok, Enum.reverse(acc), state, log}

      {:ok, tok, _} ->
        cond do
          Keywords.starts_kw?(tok) ->
            with {:ok, kw_list, state, log} <- Keywords.parse_kw_call(state, ctx, log) do
              {:ok, Enum.reverse([[kw_list] | acc]), state, log}
            end

          true ->
            # Parse args in :matched context to prevent do-block attachment to arguments.
            # The do-block belongs to the outer call, not to individual args.
            with {:ok, arg, state, log} <- Expressions.expr(state, :matched, log) do
              case TokenAdapter.peek(state) do
                {:ok, %{kind: :","}, state} ->
                  {:ok, _comma, state} = TokenAdapter.next(state)
                  parse_no_parens_args([arg | acc], state, ctx, log)

                _ ->
                  {:ok, Enum.reverse([arg | acc]), state, log}
              end
            end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp maybe_do_block(ast, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :do}, _} ->
        with {:ok, {block_meta, sections}, state, log} <- Blocks.parse_do_block(state, ctx, log) do
          ast =
            case ast do
              {name, meta, args} when is_list(args) ->
                # Prepend do/end metadata to the call's existing metadata
                {name, block_meta ++ meta, args ++ [sections]}

              other ->
                Builder.Helpers.call(other, [sections], block_meta)
            end

          # Continue with Pratt's led() to handle trailing operators like =>
          Pratt.led(ast, state, log, 0, ctx)
        end

      _ ->
        # Continue with Pratt's led() to handle trailing operators
        Pratt.led(ast, state, log, 0, ctx)
    end
  end
end
