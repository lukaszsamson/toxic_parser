defmodule ToxicParser.Grammar.Bitstrings do
  @moduledoc """
  Parsing for bitstring literals (<<>>).
  """

  alias ToxicParser.{Builder, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.{EOE, Expressions, Keywords}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @spec parse(State.t(), Pratt.context(), EventLog.t(), non_neg_integer()) :: result()
  def parse(%State{} = state, ctx, %EventLog{} = log, min_bp \\ 0) do
    with {:ok, ast, state, log} <- parse_base(state, ctx, log) do
      Pratt.led(ast, state, log, min_bp, ctx)
    end
  end

  @doc """
  Parse bitstring base without calling Pratt.led.
  Used when caller controls led binding (e.g., in stab patterns).
  """
  @spec parse_base(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_base(%State{} = state, ctx, %EventLog{} = log) do
    {:ok, open_tok, state} = TokenAdapter.next(state)
    open_meta = token_meta(open_tok.metadata)

    # Skip leading EOE and count newlines
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    case TokenAdapter.peek(state) do
      {:ok, %{kind: :">>"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        newlines_meta = if leading_newlines > 0, do: [newlines: leading_newlines], else: []
        meta = newlines_meta ++ [closing: close_meta] ++ open_meta
        ast = {:<<>>, meta, []}
        {:ok, ast, state, log}

      _ ->
        with {:ok, parts, close_meta, state, log} <- parse_segments([], state, ctx, log) do
          newlines_meta = if leading_newlines > 0, do: [newlines: leading_newlines], else: []
          meta = newlines_meta ++ [closing: close_meta] ++ open_meta
          ast = {:<<>>, meta, parts}
          {:ok, ast, state, log}
        end
    end
  end

  defp token_meta(meta), do: Builder.Helpers.token_meta(meta)

  defp parse_segments(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :">>"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        {:ok, Enum.reverse(acc), close_meta, state, log}

      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          # Parse keyword data and finish
          with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
            # Skip EOE before close
            {state, _newlines} = EOE.skip_count_newlines(state, 0)

            case TokenAdapter.next(state) do
              {:ok, %{kind: :">>"} = close_tok, state} ->
                close_meta = token_meta(close_tok.metadata)
                # Wrap keyword list as a sublist
                {:ok, Enum.reverse(acc) ++ [kw_list], close_meta, state, log}

              {:ok, tok, state} ->
                {:error, {:expected, :">>", got: tok.kind}, state, log}

              {:eof, state} ->
                {:error, :unexpected_eof, state, log}

              {:error, diag, state} ->
                {:error, diag, state, log}
            end
          end
        else
          parse_segment(acc, state, ctx, log)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_segment(acc, state, ctx, log) do
    with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
      # Skip EOE after expression
      {state, _newlines} = EOE.skip_count_newlines(state, 0)

      case TokenAdapter.peek(state) do
        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state)
          # Skip EOE after comma
          {state, _newlines} = EOE.skip_count_newlines(state, 0)
          # Check for trailing comma
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :">>"} = close_tok, _} ->
              {:ok, _close, state} = TokenAdapter.next(state)
              close_meta = token_meta(close_tok.metadata)
              {:ok, Enum.reverse([expr | acc]), close_meta, state, log}

            _ ->
              parse_segments([expr | acc], state, ctx, log)
          end

        {:ok, %{kind: :">>"} = close_tok, _} ->
          {:ok, _close, state} = TokenAdapter.next(state)
          close_meta = token_meta(close_tok.metadata)
          {:ok, Enum.reverse([expr | acc]), close_meta, state, log}

        {:ok, tok, state} ->
          {:error, {:expected_comma_or, :">>", got: tok.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end
end
