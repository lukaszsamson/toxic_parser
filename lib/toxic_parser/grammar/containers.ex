defmodule ToxicParser.Grammar.Containers do
  @moduledoc """
  Container parsing for lists and tuples (Phase 6 scaffolding).
  """

  alias ToxicParser.{Builder, Context, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.{Bitstrings, Delimited, EOE, Expressions, Keywords, Maps, Stabs}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}
          | {:no_container, State.t()}

  @spec parse(State.t(), Pratt.context(), EventLog.t(), non_neg_integer()) :: result()
  def parse(%State{} = state, %Context{} = ctx, %EventLog{} = log, min_bp \\ 0) do
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
  def parse_container_base(%State{} = state, %Context{} = ctx, %EventLog{} = log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"["}, _} ->
        parse_list_base(state, ctx, log)

      {:ok, %{kind: :"{", value: _}, _} ->
        parse_tuple_base(state, ctx, log)

      {:ok, %{kind: :"<<", value: _}, _} ->
        Bitstrings.parse_base(state, ctx, log)

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
      {:ok, tok, _} ->
        if semicolon_eoe?(tok) do
          # Leading semicolon - parse as stab or empty
          {:ok, _semi, state} = TokenAdapter.next(state)
          # Skip any additional EOE
          state = EOE.skip(state)
          Stabs.parse_paren_stab_or_empty(open_meta, state, ctx, log, min_bp)
        else
          # Skip any remaining EOE tokens
          state = EOE.skip(state)
          parse_paren_content(open_meta, state, ctx, log, min_bp)
        end

      _ ->
        # Skip any remaining EOE tokens
        state = EOE.skip(state)
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
        parens_meta = [parens: Meta.closing_meta(open_meta, close_meta, 0, [], base_first: true)]
        ast = {:__block__, parens_meta, []}
        # Continue with Pratt.led to handle trailing operators
        Pratt.led(ast, state, log, min_bp, ctx)

      # Check for stab operator at start: (-> expr)
      {:ok, %{kind: :stab_op}, _} ->
        Stabs.parse_paren_stab(open_meta, state, ctx, log, min_bp)

      # Check for empty inner parens followed by stab/when: (() -> expr) or (() when g -> expr)
      {:ok, %{kind: :"("}, _} ->
        Stabs.try_parse_stab_parens_many(
          open_meta,
          state,
          ctx,
          log,
          min_bp,
          fn new_state, new_log ->
            parse_expr_in_paren_with_meta(open_meta, new_state, ctx, new_log, min_bp)
          end
        )

      # Content that could be expression or stab pattern
      {:ok, _, _} ->
        # Try to parse stab first using checkpoint
        Stabs.try_parse_stab_or_expr(
          open_meta,
          state,
          ctx,
          log,
          min_bp,
          fn new_state, new_log ->
            parse_expr_in_paren_with_meta(open_meta, new_state, ctx, new_log, min_bp)
          end
        )

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_expr_in_paren_with_meta(open_meta, state, ctx, log, min_bp) do
    parse_expr_in_paren_impl(open_meta, [], state, ctx, log, min_bp)
  end

  # Parse expressions in parens, handling semicolons as separators
  # Grammar: paren_expr -> expr (';' expr)* ')'
  # Multiple expressions become a __block__
  # NOTE: Inside parens, we parse full `expr` (matched/unmatched/no_parens).
  # This matches `elixir_parser.yrl` via `stab_expr -> expr` under `stab_eoe`
  # and is required for forms like `x when y: z`.
  defp parse_expr_in_paren_impl(open_meta, acc, state, ctx, log, min_bp) do
    with {:ok, expr, state, log} <- Expressions.expr(state, Context.expr(), log) do
      # Check for EOE (semicolon) or close paren
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :eoe} = eoe_tok, _} ->
          # Annotate expression with EOE metadata before consuming
          annotated_expr = annotate_paren_expr_eoe(expr, eoe_tok)
          # Consume EOE
          {:ok, _eoe, state} = TokenAdapter.next(state)
          state = EOE.skip(state)
          # Check if there's more content or close paren
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :")"} = close_tok, _} ->
              {:ok, _close, state} = TokenAdapter.next(state)
              close_meta = token_meta(close_tok.metadata)

              finish_paren_exprs(
                [annotated_expr | acc],
                open_meta,
                close_meta,
                state,
                log,
                min_bp,
                ctx
              )

            {:ok, _, _} ->
              # More expressions after semicolon
              parse_expr_in_paren_impl(open_meta, [annotated_expr | acc], state, ctx, log, min_bp)

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

  # Annotate expression with end_of_expression metadata from EOE token
  defp annotate_paren_expr_eoe({name, meta, args}, eoe_tok) when is_list(meta) do
    eoe_meta = EOE.build_eoe_meta(eoe_tok)
    {name, [{:end_of_expression, eoe_meta} | meta], args}
  end

  defp annotate_paren_expr_eoe(expr, _eoe_tok), do: expr

  # Finish parsing paren expressions - build appropriate AST
  defp finish_paren_exprs([single], open_meta, close_meta, state, log, min_bp, ctx) do
    # Special cases for single expressions that need to be wrapped in __block__:
    # 1. Single-argument unary expressions with `not` or `!` operators (rearrange_uop)
    # 2. unquote_splicing - always wrapped because the parser assumes a block is being spliced
    #
    # From elixir_parser.yrl:
    #   build_paren_stab(_Before, [{Op, _, [_]}]=Exprs, _After) when ?rearrange_uop(Op) ->
    #     {'__block__', [], Exprs};
    #   build_block([{unquote_splicing, _, [_]}]=Exprs, BeforeAfter) ->
    #     {'__block__', block_meta(BeforeAfter), Exprs};
    expr =
      case single do
        {op, _meta, [_arg]} when op in [:not, :!] ->
          {:__block__, [], [single]}

        {:unquote_splicing, _meta, [_arg]} ->
          # unquote_splicing always gets wrapped in __block__ with closing metadata
          {:__block__, Meta.closing_meta(open_meta, close_meta), [single]}

        _ ->
          # Regular single expression - add parens metadata to 3-tuple AST nodes
          add_parens_meta(single, open_meta, close_meta)
      end

    # Continue with Pratt.led to handle trailing operators like *, /, etc.
    Pratt.led(expr, state, log, min_bp, ctx)
  end

  defp finish_paren_exprs(exprs, open_meta, close_meta, state, log, min_bp, ctx) do
    # Multiple expressions - wrap in __block__
    ast = {:__block__, Meta.closing_meta(open_meta, close_meta), Enum.reverse(exprs)}
    Pratt.led(ast, state, log, min_bp, ctx)
  end

  # Add parens: metadata to 3-tuple AST nodes
  defp add_parens_meta({name, meta, args}, open_meta, close_meta) when is_list(meta) do
    parens_meta = build_parens_meta(open_meta, close_meta)
    {name, [parens: parens_meta] ++ meta, args}
  end

  defp add_parens_meta(literal, _open_meta, _close_meta), do: literal

  defp build_parens_meta(open_meta, close_meta) do
    Meta.closing_meta(open_meta, close_meta, 0, [], base_first: true)
  end

  defp token_meta(meta), do: Builder.Helpers.token_meta(meta)

  defp skip_eoe_not_semicolon(state) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if semicolon_eoe?(tok) do
          state
        else
          case tok.kind do
            :eoe ->
              {:ok, _eoe, state} = TokenAdapter.next(state)
              skip_eoe_not_semicolon(state)

            _ ->
              state
          end
        end

      _ ->
        state
    end
  end

  defp semicolon_eoe?(%{kind: :eoe, value: %{source: :semicolon}}), do: true
  defp semicolon_eoe?(%{kind: :eoe, raw: {:";", _, _}}), do: true
  defp semicolon_eoe?(%{kind: :eoe, raw: {:";", _}}), do: true
  defp semicolon_eoe?(_), do: false

  defp parse_list(state, ctx, log, min_bp) do
    with {:ok, ast, state, log} <- parse_list_base(state, ctx, log) do
      # Continue with Pratt's led() to handle trailing operators
      Pratt.led(ast, state, log, min_bp, ctx)
    end
  end

  # Parse list without calling Pratt.led - used when caller controls led binding
  defp parse_list_base(state, _ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    # In elixir_parser.yrl, list elements use container_expr (not the surrounding ctx).
    container_ctx = Context.container_expr()

    item_fun = fn state, _ctx, log ->
      case Keywords.try_parse_kw_data(state, container_ctx, log) do
        {:ok, kw_list, state, log} ->
          # Keyword data must come last in lists. If it's not followed by a close bracket,
          # raise an error at the next token (typically a comma).
          {state, _newlines} = EOE.skip_count_newlines(state, 0)

          case TokenAdapter.peek(state) do
            {:ok, %{kind: :"]"}, _} ->
              {:ok, {:kw_data, kw_list}, state, log}

            {:ok, %{kind: kind}, state} ->
              {:error, {:expected, :"]", got: kind}, state, log}

            {:eof, state} ->
              {:error, :unexpected_eof, state, log}

            {:error, diag, state} ->
              {:error, diag, state, log}
          end

        {:no_kw, state, log} ->
          with {:ok, expr, state, log} <- Expressions.expr(state, container_ctx, log) do
            {:ok, {:expr, expr}, state, log}
          end

        {:error, reason, state, log} ->
          {:error, reason, state, log}
      end
    end

    with {:ok, tagged_items, state, log} <-
           Delimited.parse_comma_separated(state, container_ctx, log, :"]", item_fun,
             allow_empty?: true
           ) do
      case TokenAdapter.next(state) do
        {:ok, %{kind: :"]"}, state} ->
          {:ok, finalize_list_items(tagged_items), state, log}

        {:ok, tok, state} ->
          {:error, {:expected, :"]", got: tok.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  defp finalize_list_items([]), do: []

  defp finalize_list_items(tagged_items) do
    case List.last(tagged_items) do
      {:kw_data, kw_list} ->
        exprs =
          tagged_items
          |> Enum.drop(-1)
          |> Enum.map(fn {:expr, expr} -> expr end)

        exprs ++ kw_list

      _ ->
        Enum.map(tagged_items, fn {:expr, expr} -> expr end)
    end
  end

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
      meta = Meta.closing_meta(open_meta, close_meta, newlines)

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

  defp parse_tuple_args(state, _ctx, log) do
    # Skip leading EOE and count newlines
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    container_ctx = Context.container_expr()

    item_fun = fn state, _ctx, log ->
      case Keywords.try_parse_kw_data(state, container_ctx, log) do
        {:ok, kw_list, state, log} ->
          # container_args allows a keyword tail as the final element. We only
          # treat this as kw_data if the close curly follows.
          {state, _newlines} = EOE.skip_count_newlines(state, 0)

          case TokenAdapter.peek(state) do
            {:ok, %{kind: :"}"}, _} ->
              {:ok, {:kw_data, kw_list}, state, log}

            {:ok, %{kind: kind}, state} ->
              {:error, {:expected, :"}", got: kind}, state, log}

            {:eof, state} ->
              {:error, :unexpected_eof, state, log}

            {:error, diag, state} ->
              {:error, diag, state, log}
          end

        {:no_kw, state, log} ->
          with {:ok, expr, state, log} <- Expressions.expr(state, container_ctx, log) do
            {:ok, {:expr, expr}, state, log}
          end

        {:error, reason, state, log} ->
          {:error, reason, state, log}
      end
    end

    with {:ok, tagged_items, state, log} <-
           Delimited.parse_comma_separated(state, container_ctx, log, :"}", item_fun,
             allow_empty?: true
           ) do
      case TokenAdapter.next(state) do
        {:ok, %{kind: :"}"} = close_tok, state} ->
          close_meta = token_meta(close_tok.metadata)
          {:ok, finalize_tuple_items(tagged_items), leading_newlines, close_meta, state, log}

        {:ok, tok, state} ->
          {:error, {:expected, :"}", got: tok.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  defp finalize_tuple_items([]), do: []

  defp finalize_tuple_items(tagged_items) do
    case List.last(tagged_items) do
      {:kw_data, kw_list} ->
        exprs =
          tagged_items
          |> Enum.drop(-1)
          |> Enum.map(fn {:expr, expr} -> expr end)

        exprs ++ [kw_list]

      _ ->
        Enum.map(tagged_items, fn {:expr, expr} -> expr end)
    end
  end
end
