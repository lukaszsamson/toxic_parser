defmodule ToxicParser.Grammar.Containers do
  @moduledoc """
  Container parsing for lists and tuples (Phase 6 scaffolding).
  """

  alias ToxicParser.{
    Builder,
    Context,
    Cursor,
    EventLog,
    ExprClass,
    NoParensErrors,
    Pratt,
    State,
    TokenAdapter
  }

  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.{Bitstrings, Delimited, EOE, Expressions, Keywords, Maps, Stabs}

  @type result ::
          {:ok, Macro.t(), State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}
          | {:no_container, State.t(), Cursor.t()}

  @spec parse(State.t(), Cursor.t(), Pratt.context(), EventLog.t(), non_neg_integer(), keyword()) :: result()
  def parse(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log, min_bp \\ 0, opts \\ []) do
    case TokenAdapter.peek(state, cursor) do
      {:ok, {:"(", _meta, _value}, _, cursor} ->
        parse_paren(state, cursor, ctx, log, min_bp, opts)

      {:ok, {:"[", _meta, _value}, _, cursor} ->
        parse_list(state, cursor, ctx, log, min_bp, opts)

      {:ok, {:"{", _meta, _value}, _, cursor} ->
        parse_tuple(state, cursor, ctx, log, min_bp, opts)

      {:ok, {:%{}, _meta, _value}, _, cursor} ->
        Maps.parse_map(state, cursor, ctx, log, min_bp)

      {:ok, {:%, _meta, _value}, _, cursor} ->
        Maps.parse_map(state, cursor, ctx, log, min_bp)

      {:ok, {:"<<", _meta, _value}, _, cursor} ->
        Bitstrings.parse(state, cursor, ctx, log, min_bp)

      {:eof, state, cursor} ->
        {:no_container, state, cursor}

      {:error, _diag, state, cursor} ->
        {:no_container, state, cursor}

      _ ->
        {:no_container, state, cursor}
    end
  end

  @doc """
  Parse a container without calling Pratt.led at the end.
  Used when the caller needs to control operator binding (e.g., stab patterns).
  """
  @spec parse_container_base(State.t(), Cursor.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_container_base(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log) do
    case TokenAdapter.peek(state, cursor) do
      {:ok, {:"[", _meta, _value}, _, cursor} ->
        parse_list_base(state, cursor, ctx, log)

      {:ok, {:"{", _meta, _value}, _, cursor} ->
        parse_tuple_base(state, cursor, ctx, log)

      {:ok, {:"<<", _meta, _value}, _, cursor} ->
        Bitstrings.parse_base(state, cursor, ctx, log)

      {:eof, state, cursor} ->
        {:no_container, state, cursor}

      {:error, _diag, state, cursor} ->
        {:no_container, state, cursor}

      _ ->
        {:no_container, state, cursor}
    end
  end

  # Parse parenthesized expression, empty parens, or stab expression
  # Grammar rules:
  #   access_expr -> open_paren stab_eoe ')'         : build_paren_stab
  #   access_expr -> open_paren ';' stab_eoe ')'     : build_paren_stab
  #   access_expr -> open_paren ';' close_paren      : build_paren_stab with nil
  #   access_expr -> empty_paren                     : wrap in __block__
  defp parse_paren(state, cursor, ctx, log, min_bp, opts) do
    {:ok, open_tok, state, cursor} = TokenAdapter.next(state, cursor)
    open_meta = TokenAdapter.token_meta(open_tok)

    # Skip leading newlines (but not semicolons) and count them
    {state, cursor, newlines} = skip_eoe_not_semicolon_with_count(state, cursor, 0)

    # Check for leading semicolon (forces stab interpretation)
    case TokenAdapter.peek(state, cursor) do
      {:ok, tok, _, cursor} ->
        if semicolon?(tok) do
          # Leading semicolon - parse as stab or empty
          {:ok, _semi, state, cursor} = TokenAdapter.next(state, cursor)
          # Skip any additional EOE and count newlines
          {state, cursor, more_newlines} = EOE.skip_count_newlines(state, cursor, 0)

          Stabs.parse_paren_stab_or_empty(
            open_meta,
            newlines + more_newlines,
            state,
            cursor,
            ctx,
            log,
            min_bp
          )
        else
          # Skip any remaining EOE tokens and count newlines
          {state, cursor, more_newlines} = EOE.skip_count_newlines(state, cursor, 0)
          parse_paren_content(open_meta, newlines + more_newlines, state, cursor, ctx, log, min_bp, opts)
        end

      _ ->
        # Skip any remaining EOE tokens and count newlines
        {state, cursor, more_newlines} = EOE.skip_count_newlines(state, cursor, 0)
        parse_paren_content(open_meta, newlines + more_newlines, state, cursor, ctx, log, min_bp, opts)
    end
  end

  # Parse content after open paren (no leading semicolon)
  defp parse_paren_content(open_meta, newlines, state, cursor, ctx, log, min_bp, opts) do
    case TokenAdapter.peek(state, cursor) do
      # Empty parens: () -> {:__block__, [parens: ...], []}
      # NOTE: parens: metadata doesn't include newlines (only line, column, closing)
      {:ok, {:")", _close_meta, _value} = close_tok, _, cursor} ->
        {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
        close_meta_kw = TokenAdapter.token_meta(close_tok)

        parens_meta = [
          parens: Meta.closing_meta(open_meta, close_meta_kw, 0, [], base_first: true)
        ]

        ast = {:__block__, parens_meta, []}
        Pratt.led(ast, state, cursor, log, min_bp, ctx, opts)

      # Always parse as stab_eoe (YRL-aligned); paren stab builder decides block vs stab.
      {:ok, _, _, cursor} ->
        Stabs.parse_paren_stab(open_meta, newlines, state, cursor, ctx, log, min_bp)

      {:eof, state, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, state, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  defp skip_eoe_not_semicolon_with_count(state, cursor, count) do
    case TokenAdapter.peek(state, cursor) do
      {:ok, tok, _, cursor} ->
        if semicolon?(tok) do
          {state, cursor, count}
        else
          case tok do
            {:eol, {_, _, n}, _value} when is_integer(n) ->
              {:ok, _eol, state, cursor} = TokenAdapter.next(state, cursor)
              skip_eoe_not_semicolon_with_count(state, cursor, count + n)

            {:eol, _meta, _value} ->
              {:ok, _eol, state, cursor} = TokenAdapter.next(state, cursor)
              skip_eoe_not_semicolon_with_count(state, cursor, count)

            _ ->
              {state, cursor, count}
          end
        end

      _ ->
        {state, cursor, count}
    end
  end

  defp semicolon?({:";", _meta, _value}), do: true
  defp semicolon?(_), do: false

  defp parse_list(state, cursor, ctx, log, min_bp, opts) do
    with {:ok, ast, state, cursor, log} <- parse_list_base(state, cursor, ctx, log) do
      # Continue with Pratt's led() to handle trailing operators
      Pratt.led(ast, state, cursor, log, min_bp, ctx, opts)
    end
  end

  # Parse list without calling Pratt.led - used when caller controls led binding
  defp parse_list_base(state, cursor, _ctx, log) do
    {:ok, open_tok, state, cursor} = TokenAdapter.next(state, cursor)
    open_meta = TokenAdapter.token_meta(open_tok)

    # Count leading newlines after [ for literal_encoder metadata
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    # In elixir_parser.yrl, list elements use container_expr (not the surrounding ctx).
    container_ctx = Context.container_expr()

    item_fun = fn state, cursor, _ctx, log ->
      case Keywords.try_parse_kw_data(state, cursor, container_ctx, log) do
        {:ok, kw_list, state, cursor, log} ->
          # Keyword data must come last in lists. If it's not followed by a close bracket,
          # raise an error at the next token (typically a comma).
          {state, cursor, _newlines} = EOE.skip_count_newlines(state, cursor, 0)

          case TokenAdapter.peek(state, cursor) do
            {:ok, {:"]", _meta, _value}, _, cursor} ->
              {:ok, {:kw_data, kw_list}, state, cursor, log}

            {:ok, {kind, _meta, _value}, state, cursor} ->
              {:error, {:expected, :"]", got: kind}, state, cursor, log}

            {:eof, state, cursor} ->
              {:error, :unexpected_eof, state, cursor, log}

            {:error, diag, state, cursor} ->
              {:error, diag, state, cursor, log}
          end

        {:no_kw, state, cursor, log} ->
          with {:ok, expr, state, cursor, log} <- Expressions.expr(state, cursor, container_ctx, log) do
            # Validate no_parens expressions are not allowed in containers
            case ExprClass.classify(expr) do
              :no_parens ->
                {:error, NoParensErrors.error_no_parens_container_strict(expr), state, cursor, log}

              _ ->
                {:ok, {:expr, expr}, state, cursor, log}
            end
          end

        {:error, reason, state, cursor, log} ->
          {:error, reason, state, cursor, log}
      end
    end

    with {:ok, tagged_items, state, cursor, log} <-
           Delimited.parse_comma_separated(state, cursor, container_ctx, log, :"]", item_fun,
             allow_empty?: true,
             skip_eoe_initial?: false
           ) do
      case TokenAdapter.next(state, cursor) do
        {:ok, {:"]", _meta, _value} = close_tok, state, cursor} ->
          close_meta = TokenAdapter.token_meta(close_tok)

          # Build list metadata with closing location and newlines (for token_metadata compatibility)
          list_meta = Meta.closing_meta(open_meta, close_meta, leading_newlines)
          list_ast = finalize_list_items(tagged_items)
          # Pass list through literal_encoder if present
          encoded_list = Builder.Helpers.literal(list_ast, list_meta, state)
          {:ok, encoded_list, state, cursor, log}

        {:ok, {kind, _meta, _value}, state, cursor} ->
          {:error, {:expected, :"]", got: kind}, state, cursor, log}

        {:eof, state, cursor} ->
          {:error, :unexpected_eof, state, cursor, log}

        {:error, diag, state, cursor} ->
          {:error, diag, state, cursor, log}
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

  defp parse_tuple(state, cursor, ctx, log, min_bp, opts) do
    with {:ok, ast, state, cursor, log} <- parse_tuple_base(state, cursor, ctx, log) do
      # Continue with Pratt's led() to handle trailing operators like <-, =, etc.
      Pratt.led(ast, state, cursor, log, min_bp, ctx, opts)
    end
  end

  # Parse tuple without calling Pratt.led - used when caller controls led binding
  defp parse_tuple_base(state, cursor, ctx, log) do
    {:ok, open_tok, state, cursor} = TokenAdapter.next(state, cursor)
    open_meta = TokenAdapter.token_meta(open_tok)

    with {:ok, elements, newlines, close_meta, state, cursor, log} <- parse_tuple_args(state, cursor, ctx, log) do
      meta = Meta.closing_meta(open_meta, close_meta, newlines)

      # 2-element tuples are represented as literal {a, b} and are encodable
      # Other sizes use {:{}, meta, elements} which is NOT a primitive literal
      ast =
        case elements do
          [a, b] ->
            # 2-element tuples are primitive literals - pass through literal_encoder
            tuple = {a, b}
            Builder.Helpers.literal(tuple, meta, state)

          _ ->
            # Other tuples are AST nodes, not primitives - don't encode
            {:{}, meta, elements}
        end

      {:ok, ast, state, cursor, log}
    end
  end

  defp parse_tuple_args(state, cursor, _ctx, log) do
    # Skip leading EOE and count newlines
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    container_ctx = Context.container_expr()

    item_fun = fn state, cursor, _ctx, log ->
      case Keywords.try_parse_kw_data(state, cursor, container_ctx, log) do
        {:ok, kw_list, state, cursor, log} ->
          # container_args allows a keyword tail as the final element. We only
          # treat this as kw_data if the close curly follows.
          {state, cursor, _newlines} = EOE.skip_count_newlines(state, cursor, 0)

          case TokenAdapter.peek(state, cursor) do
            {:ok, {:"}", _meta, _value}, _, cursor} ->
              {:ok, {:kw_data, kw_list}, state, cursor, log}

            {:ok, {kind, _meta, _value}, state, cursor} ->
              {:error, {:expected, :"}", got: kind}, state, cursor, log}

            {:eof, state, cursor} ->
              {:error, :unexpected_eof, state, cursor, log}

            {:error, diag, state, cursor} ->
              {:error, diag, state, cursor, log}
          end

        {:no_kw, state, cursor, log} ->
          with {:ok, expr, state, cursor, log} <- Expressions.expr(state, cursor, container_ctx, log) do
            # Validate no_parens expressions are not allowed in containers
            case ExprClass.classify(expr) do
              :no_parens ->
                {:error, NoParensErrors.error_no_parens_container_strict(expr), state, cursor, log}

              _ ->
                {:ok, {:expr, expr}, state, cursor, log}
            end
          end

        {:error, reason, state, cursor, log} ->
          {:error, reason, state, cursor, log}
      end
    end

    with {:ok, tagged_items, state, cursor, log} <-
           Delimited.parse_comma_separated(state, cursor, container_ctx, log, :"}", item_fun,
             allow_empty?: true
           ) do
      # Check for invalid keyword list at start of tuple
      # {foo: :bar} is invalid - tuples cannot start with keyword data
      case tagged_items do
        [{:kw_data, _} | _] ->
          {:error, :unexpected_keyword_list_in_tuple, state, cursor, log}

        _ ->
          case TokenAdapter.next(state, cursor) do
            {:ok, {:"}", _meta, _value} = close_tok, state, cursor} ->
              close_meta = TokenAdapter.token_meta(close_tok)
              {:ok, finalize_tuple_items(tagged_items), leading_newlines, close_meta, state, cursor, log}

            {:ok, {kind, _meta, _value}, state, cursor} ->
              {:error, {:expected, :"}", got: kind}, state, cursor, log}

            {:eof, state, cursor} ->
              {:error, :unexpected_eof, state, cursor, log}

            {:error, diag, state, cursor} ->
              {:error, diag, state, cursor, log}
          end
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
