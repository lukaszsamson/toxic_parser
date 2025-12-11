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

  defp parse_list(state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    with {:ok, elements, state, log} <- parse_container_args(:"]", state, ctx, log) do
      {:ok, elements, state, log}
    end
  end

  defp parse_tuple(state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    with {:ok, elements, state, log} <- parse_container_args(:"}", state, ctx, log) do
      ast =
        case elements do
          [] -> {:{}, [], []}
          list -> {:{}, [], list}
        end

      {:ok, ast, state, log}
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
end
