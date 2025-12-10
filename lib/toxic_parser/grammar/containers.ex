defmodule ToxicParser.Grammar.Containers do
  @moduledoc """
  Container parsing for lists and tuples (Phase 6 scaffolding).
  """

  alias ToxicParser.{EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.Expressions

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}
          | {:no_container, State.t()}

  @spec parse(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse(%State{} = state, ctx, %EventLog{} = log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"["}, _} ->
        parse_list(state, ctx, log)

      {:ok, %{kind: :"{", value: _}, _} ->
        parse_tuple(state, ctx, log)

      {:ok, %{kind: :%{}}, _} ->
        parse_map(state, ctx, log)

      {:eof, state} ->
        {:no_container, state}

      {:error, _diag, state} ->
        {:no_container, state}

      _ ->
        {:no_container, state}
    end
  end

  defp parse_list(state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    with {:ok, elements, state, log} <- parse_elements([], :"]", state, ctx, log) do
      {:ok, Enum.reverse(elements), state, log}
    end
  end

  defp parse_tuple(state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    with {:ok, elements, state, log} <- parse_elements([], :"}", state, ctx, log) do
      ast =
        case elements do
          [] -> {:{}, [], []}
          list -> {:{}, [], Enum.reverse(list)}
        end

      {:ok, ast, state, log}
    end
  end

  defp parse_map(state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    state =
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :"{"}, _} ->
          {:ok, _brace, state} = TokenAdapter.next(state)
          state

        _ ->
          state
      end

    with {:ok, pairs, state, log} <- parse_map_pairs([], state, ctx, log) do
      {:ok, {:%{}, [], Enum.reverse(pairs)}, state, log}
    end
  end

  defp parse_elements(acc, terminator, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: ^terminator}, state} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        {:ok, acc, state, log}

      {:ok, _tok, _state} ->
        with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :","}, state} ->
              {:ok, _comma, state} = TokenAdapter.next(state)
              parse_elements([expr | acc], terminator, state, ctx, log)

            {:ok, %{kind: ^terminator}, _} ->
              parse_elements([expr | acc], terminator, state, ctx, log)

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

  defp parse_map_pairs(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"}"}, state} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        {:ok, acc, state, log}

      {:eof, state} ->
        {:ok, acc, state, log}

      {:ok, _tok, _} ->
        with {:ok, key, state, log} <- Expressions.expr(state, ctx, log),
             {:ok, _sep, state} <- expect_separator(state),
             {:ok, value, state, log} <- Expressions.expr(state, ctx, log) do
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :","}, state} ->
              {:ok, _comma, state} = TokenAdapter.next(state)
              parse_map_pairs([{key, value} | acc], state, ctx, log)

            {:ok, %{kind: :"}"}, _} ->
              parse_map_pairs([{key, value} | acc], state, ctx, log)

            _ ->
              {:error, {:expected_comma_or, :"}"}, state, log}
          end
        end

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp expect_separator(state) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: :"=>", value: _}, state} -> {:ok, :"=>", state}
      {:ok, %{kind: :":", value: _}, state} -> {:ok, :":", state}
      other -> other
    end
  end
end
