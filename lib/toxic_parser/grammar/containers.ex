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
        Pratt.led(ast, state, log, min_bp, ctx)

      # Always parse as stab_eoe (YRL-aligned); paren stab builder decides block vs stab.
      {:ok, _, _} ->
        Stabs.parse_paren_stab(open_meta, state, ctx, log, min_bp)

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
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
      # Check for invalid keyword list at start of tuple
      # {foo: :bar} is invalid - tuples cannot start with keyword data
      case tagged_items do
        [{:kw_data, _} | _] ->
          {:error, :unexpected_keyword_list_in_tuple, state, log}

        _ ->
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
