defmodule ToxicParser.Grammar.Containers do
  @moduledoc """
  Container parsing for lists and tuples (Phase 6 scaffolding).
  """

  alias ToxicParser.{EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.{Bitstrings, Expressions, Keywords, Maps}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}
          | {:no_container, State.t()}

  @spec parse(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse(%State{} = state, ctx, %EventLog{} = log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"("}, _} ->
        parse_paren(state, ctx, log)

      {:ok, %{kind: :"["}, _} ->
        parse_list(state, ctx, log)

      {:ok, %{kind: :"{", value: _}, _} ->
        parse_tuple(state, ctx, log)

      {:ok, %{kind: :%{}}, _} ->
        Maps.parse_map(state, ctx, log)

      {:ok, %{kind: :"%"}, _} ->
        Maps.parse_map(state, ctx, log)

      {:ok, %{kind: :"<<", value: _}, _} ->
        Bitstrings.parse(state, ctx, log)

      {:eof, state} ->
        {:no_container, state}

      {:error, _diag, state} ->
        {:no_container, state}

      _ ->
        {:no_container, state}
    end
  end

  # Parse parenthesized expression or empty parens
  defp parse_paren(state, ctx, log) do
    {:ok, open_tok, state} = TokenAdapter.next(state)
    open_meta = token_meta(open_tok.metadata)

    # Skip any leading EOE tokens (newlines/semicolons)
    state = skip_eoe(state)

    case TokenAdapter.peek(state) do
      # Empty parens: () -> {:__block__, [parens: ...], []}
      {:ok, %{kind: :")"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        parens_meta = [parens: open_meta ++ [closing: close_meta]]
        ast = {:__block__, parens_meta, []}
        {:ok, ast, state, log}

      # Parenthesized expression
      {:ok, _, _} ->
        with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log),
             {:ok, _close, state} <- expect(state, :")") do
          {:ok, expr, state, log}
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp skip_eoe(state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe(state)

      _ ->
        state
    end
  end

  defp token_meta(%{range: %{start: %{line: line, column: column}}}), do: [line: line, column: column]
  defp token_meta(_), do: []

  defp expect(state, kind) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: ^kind}, state} -> {:ok, kind, state}
      {:ok, token, state} -> {:error, {:expected, kind, got: token.kind}, state}
      {:eof, state} -> {:error, :unexpected_eof, state}
      {:error, diag, state} -> {:error, diag, state}
    end
  end

  defp parse_list(state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    with {:ok, elements, state, log} <- parse_container_args(:"]", state, ctx, log) do
      {:ok, elements, state, log}
    end
  end

  defp parse_tuple(state, ctx, log) do
    {:ok, open_tok, state} = TokenAdapter.next(state)
    open_meta = token_meta(open_tok.metadata)

    with {:ok, elements, newlines, close_meta, state, log} <- parse_tuple_args(state, ctx, log) do
      newlines_meta = if newlines > 0, do: [newlines: newlines], else: []
      meta = newlines_meta ++ [closing: close_meta] ++ open_meta

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

  defp parse_tuple_args(state, ctx, log) do
    # Skip leading EOE and count newlines
    {state, leading_newlines} = skip_eoe_count_newlines(state, 0)

    case TokenAdapter.peek(state) do
      # Empty tuple
      {:ok, %{kind: :"}"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        {:ok, [], leading_newlines, close_meta, state, log}

      {:ok, _, _} ->
        with {:ok, elements, close_meta, state, log} <- parse_tuple_elements([], state, ctx, log) do
          {:ok, elements, leading_newlines, close_meta, state, log}
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
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

  # Parse tuple elements, returning close token metadata
  defp parse_tuple_elements(acc, state, ctx, log) do
    # Check if next token starts a keyword list
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          # Parse keyword data and finish
          with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
            # Skip EOE before close
            {state, _newlines} = skip_eoe_count_newlines(state, 0)
            case TokenAdapter.next(state) do
              {:ok, %{kind: :"}"} = close_tok, state} ->
                close_meta = token_meta(close_tok.metadata)
                {:ok, Enum.reverse([kw_list | acc]), close_meta, state, log}

              {:ok, tok, state} ->
                {:error, {:expected, :"}", got: tok.kind}, state, log}

              {:eof, state} ->
                {:error, :unexpected_eof, state, log}

              {:error, diag, state} ->
                {:error, diag, state, log}
            end
          end
        else
          parse_tuple_element(acc, state, ctx, log)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_tuple_element(acc, state, ctx, log) do
    with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
      # Skip EOE after expression
      {state, _newlines} = skip_eoe_count_newlines(state, 0)

      case TokenAdapter.peek(state) do
        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state)
          # Skip EOE after comma
          {state, _newlines} = skip_eoe_count_newlines(state, 0)
          # Check for trailing comma or close
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :"}"} = close_tok, _} ->
              {:ok, _close, state} = TokenAdapter.next(state)
              close_meta = token_meta(close_tok.metadata)
              {:ok, Enum.reverse([expr | acc]), close_meta, state, log}

            _ ->
              parse_tuple_elements([expr | acc], state, ctx, log)
          end

        {:ok, %{kind: :"}"} = close_tok, _} ->
          {:ok, _close, state} = TokenAdapter.next(state)
          close_meta = token_meta(close_tok.metadata)
          {:ok, Enum.reverse([expr | acc]), close_meta, state, log}

        {:ok, tok, state} ->
          {:error, {:expected_comma_or, :"}", got: tok.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  defp parse_container_args(terminator, state, ctx, log) do
    parse_container_args_base([], terminator, state, ctx, log)
    |> append_kw_tail_if_present(terminator, ctx, log)
  end

  defp parse_container_args_base(acc, terminator, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: ^terminator}, state} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        {:ok, acc, state, log}

      {:ok, _tok, _state} ->
        with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :","}, state} ->
              {:ok, _comma, state} = TokenAdapter.next(state)
              parse_container_args_base([expr | acc], terminator, state, ctx, log)

            {:ok, %{kind: ^terminator}, _} ->
              parse_container_args_base([expr | acc], terminator, state, ctx, log)

            _ ->
              {:error, {:expected_comma_or, terminator}, state, log}
          end
        end

      {:eof, state} ->
        {:ok, acc, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp append_kw_tail_if_present({:ok, acc, state, log}, _terminator, ctx, log0) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log0) do
            {:ok, Enum.reverse([kw_list | acc]), state, log}
          end
        else
          {:ok, Enum.reverse(acc), state, log}
        end

      _ ->
        {:ok, Enum.reverse(acc), state, log}
    end
  end

  defp append_kw_tail_if_present({:error, reason, state, log}, _terminator, _ctx, _log0) do
    {:error, reason, state, log}
  end
end
