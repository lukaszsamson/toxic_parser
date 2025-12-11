defmodule ToxicParser.Grammar.Maps do
  @moduledoc """
  Parsing for maps and structs, including updates inside `%{}`.
  """

  alias ToxicParser.{Builder, EventLog, State, TokenAdapter}
  alias ToxicParser.Grammar.{Expressions, Keywords}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @spec parse_map(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_map(%State{} = state, ctx, %EventLog{} = log) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: :%{} = kind, metadata: meta}, state} ->
        meta_kw = Builder.Helpers.token_meta(meta)
        parse_map_body(nil, kind, meta_kw, state, ctx, log)

      {:ok, %{kind: :"%", metadata: meta}, state} ->
        meta_kw = Builder.Helpers.token_meta(meta)
        with {:ok, base, state, log} <- Expressions.expr(state, ctx, log) do
          parse_map_body(base, :"%", meta_kw, state, ctx, log)
        end

      other ->
        other
    end
  end

  defp parse_map_body(base, _kind, meta, state, ctx, log) do
    # Consume optional "{" if present to normalize to brace parsing
    state =
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :"{", metadata: _}, _} ->
          {:ok, _open, state} = TokenAdapter.next(state)
          state

        _ ->
          state
      end

    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"}"}, state} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        {:ok, map_ast(base, [], meta), state, log}

      {:ok, _tok, _} ->
        with {:ok, entries, state, log} <- parse_map_entries([], state, ctx, log),
             {:ok, _close, state} <- expect(state, :"}") do
          {:ok, map_ast(base, Enum.reverse(entries), meta), state, log}
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_map_entries(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"}"}, _} ->
        {:ok, acc, state, log}

      {:ok, _tok, _} ->
        {state_after_entry, entry_result} =
          case try_assoc_update(state, ctx, log) do
            {:ok, update, new_state, new_log} -> {new_state, {:ok, update, new_state, new_log}}
            :no_update -> {state, nil}
          end

        case entry_result do
          {:ok, entry, state, log} ->
            entries =
              if is_list(entry) and Keyword.keyword?(entry) do
                entry
              else
                [entry]
              end

            new_acc = Enum.reduce(entries, acc, fn e, acc -> [e | acc] end)

            case TokenAdapter.peek(state) do
              {:ok, %{kind: :"}"}, _} ->
                {:ok, new_acc, state, log}

              _ ->
                parse_tail(new_acc, state, ctx, log)
            end

          _ ->
            with {:ok, entry, state, log} <- parse_assoc_expr(state_after_entry, ctx, log) do
              entries =
                if is_list(entry) and Keyword.keyword?(entry) do
                  entry
                else
                  [entry]
                end

              new_acc = Enum.reduce(entries, acc, fn e, a -> [e | a] end)
              case TokenAdapter.peek(state) do
                {:ok, %{kind: :"}"}, _} ->
                  {:ok, new_acc, state, log}

                _ ->
                  parse_tail(new_acc, state, ctx, log)
              end
            end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_tail(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :","}, state} ->
        {:ok, _comma, state} = TokenAdapter.next(state)
        parse_map_entries(acc, state, ctx, log)

      _ ->
        parse_map_entries(acc, state, ctx, log)
    end
  end

  defp parse_assoc_expr(state, ctx, log) do
    # assoc_expr -> matched/unmatched with => | kw list
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
            {:ok, kw_list, state, log}
          end
        else
          with {:ok, key_ast, state, log} <- Expressions.expr(state, ctx, log),
               {:ok, _op, state} <- expect_assoc_op(state),
               {:ok, value_ast, state, log} <- Expressions.expr(state, ctx, log) do
            {:ok, {key_ast, value_ast}, state, log}
          end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp try_assoc_update(state, ctx, log) do
    case TokenAdapter.peek_n(state, 2) do
      {:ok, [%{kind: _} = left_tok, %{kind: kind}], _}
      when kind in [:pipe_op, :"|"] ->
        {:ok, _left, state} = TokenAdapter.next(state)
        left_ast = Builder.Helpers.from_token(left_tok)
        {:ok, _pipe, state} = TokenAdapter.next(state)

        case TokenAdapter.peek(state) do
          {:ok, tok, _} ->
            if Keywords.starts_kw?(tok) do
              with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
                {:ok, {:|, [], [left_ast, kw_list]}, state, log}
              end
            else
              with {:ok, key_tok, state} <- TokenAdapter.next(state),
                   key_ast <- Builder.Helpers.from_token(key_tok),
                   {:ok, _op, state} <- expect_assoc_op(state),
                   {:ok, value_ast, state, log} <- Expressions.expr(state, ctx, log) do
                {:ok, {:|, [], [left_ast, [{key_ast, value_ast}]]}, state, log}
              end
            end

          {:eof, state} ->
            {:error, :unexpected_eof, state, log}

          {:error, diag, state} ->
            {:error, diag, state, log}
        end

      _ ->
        :no_update
    end
  end

  defp expect_assoc_op(state) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: :assoc_op, value: _}, state} -> {:ok, :assoc_op, state}
      {:ok, %{kind: :"=>", value: _}, state} -> {:ok, :"=>", state}
      {:ok, %{kind: :":", value: _}, state} -> {:ok, :":", state}
      {:ok, token, state} -> {:error, {:expected, :"=>", got: token.kind}, state}
      {:eof, state} -> {:error, :unexpected_eof, state}
      {:error, diag, state} -> {:error, diag, state}
    end
  end

  defp expect(state, kind) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: ^kind}, state} -> {:ok, kind, state}
      {:ok, token, state} -> {:error, {:expected, kind, got: token.kind}, state}
      {:eof, state} -> {:error, :unexpected_eof, state}
      {:error, diag, state} -> {:error, diag, state}
    end
  end

  defp map_ast(nil, pairs, meta), do: {:%{}, meta, pairs}

  defp map_ast(base, pairs, meta) do
    inner = {:%{}, meta, pairs}
    {:%, meta, [base, inner]}
  end
end
