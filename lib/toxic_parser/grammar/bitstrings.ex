defmodule ToxicParser.Grammar.Bitstrings do
  @moduledoc """
  Parsing for bitstring literals (<<>>).
  """

  alias ToxicParser.{Builder, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.{EOE, Expressions, Keywords}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @spec parse(State.t(), Pratt.context(), EventLog.t(), non_neg_integer()) :: result()
  def parse(%State{} = state, %Context{} = ctx, %EventLog{} = log, min_bp \\ 0) do
    with {:ok, ast, state, log} <- parse_base(state, ctx, log) do
      Pratt.led(ast, state, log, min_bp, ctx)
    end
  end

  @doc """
  Parse bitstring base without calling Pratt.led.
  Used when caller controls led binding (e.g., in stab patterns).
  """
  @spec parse_base(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_base(%State{} = state, %Context{} = ctx, %EventLog{} = log) do
    {:ok, open_tok, state} = TokenAdapter.next(state)
    open_meta = token_meta(open_tok.metadata)

    # Skip leading EOE and count newlines
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    case TokenAdapter.peek(state) do
      {:ok, %{kind: :">>"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        meta = Meta.closing_meta(open_meta, close_meta, leading_newlines)
        ast = {:<<>>, meta, []}
        {:ok, ast, state, log}

      _ ->
        with {:ok, parts, close_meta, state, log} <- parse_segments([], state, ctx, log) do
          meta = Meta.closing_meta(open_meta, close_meta, leading_newlines)
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

  # Check if result is a keyword list (from quoted keyword parsing)
  defp is_keyword_list(list) when is_list(list) and length(list) > 0, do: true
  defp is_keyword_list(_), do: false

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

            {:ok, tok, _} ->
              # If expr was a keyword list and next is also keyword, merge them
              cond do
                is_keyword_list(expr) and Keywords.starts_kw?(tok) ->
                  # Merge with following keywords
                  with {:ok, more_kw, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
                    {state, _newlines} = EOE.skip_count_newlines(state, 0)

                    case TokenAdapter.next(state) do
                      {:ok, %{kind: :">>"} = close_tok, state} ->
                        close_meta = token_meta(close_tok.metadata)
                        {:ok, Enum.reverse(acc) ++ [expr ++ more_kw], close_meta, state, log}

                      {:ok, tok, state} ->
                        {:error, {:expected, :">>", got: tok.kind}, state, log}

                      {:eof, state} ->
                        {:error, :unexpected_eof, state, log}

                      {:error, diag, state} ->
                        {:error, diag, state, log}
                    end
                  end

                is_keyword_list(expr) and tok.kind in [:list_string_start, :bin_string_start] ->
                  # Another quoted keyword - parse via expression and merge
                  parse_bitstring_quoted_kw_continuation(expr, acc, state, ctx, log)

                true ->
                  parse_segments([expr | acc], state, ctx, log)
              end
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

  # Parse continuation of keyword list in bitstring context
  defp parse_bitstring_quoted_kw_continuation(acc_kw, acc, state, ctx, log) do
    case Expressions.expr(state, ctx, log) do
      {:ok, expr, state, log} when is_list(expr) and length(expr) > 0 ->
        {state, _newlines} = EOE.skip_count_newlines(state, 0)

        case TokenAdapter.peek(state) do
          {:ok, %{kind: :","}, _} ->
            {:ok, _comma, state} = TokenAdapter.next(state)
            {state, _newlines} = EOE.skip_count_newlines(state, 0)

            case TokenAdapter.peek(state) do
              {:ok, %{kind: :">>"} = close_tok, _} ->
                {:ok, _close, state} = TokenAdapter.next(state)
                close_meta = token_meta(close_tok.metadata)
                {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], close_meta, state, log}

              {:ok, tok, _} ->
                cond do
                  Keywords.starts_kw?(tok) ->
                    with {:ok, more_kw, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
                      {state, _newlines} = EOE.skip_count_newlines(state, 0)

                      case TokenAdapter.next(state) do
                        {:ok, %{kind: :">>"} = close_tok, state} ->
                          close_meta = token_meta(close_tok.metadata)

                          {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr ++ more_kw], close_meta,
                           state, log}

                        {:ok, tok, state} ->
                          {:error, {:expected, :">>", got: tok.kind}, state, log}

                        {:eof, state} ->
                          {:error, :unexpected_eof, state, log}

                        {:error, diag, state} ->
                          {:error, diag, state, log}
                      end
                    end

                  tok.kind in [:list_string_start, :bin_string_start] ->
                    parse_bitstring_quoted_kw_continuation(acc_kw ++ expr, acc, state, ctx, log)

                  true ->
                    {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state, log}
                end

              _ ->
                {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], state, log}
            end

          {:ok, %{kind: :">>"} = close_tok, _} ->
            {:ok, _close, state} = TokenAdapter.next(state)
            close_meta = token_meta(close_tok.metadata)
            {:ok, Enum.reverse(acc) ++ [acc_kw ++ expr], close_meta, state, log}

          {:ok, tok, state} ->
            {:error, {:expected_comma_or, :">>", got: tok.kind}, state, log}

          {:eof, state} ->
            {:error, :unexpected_eof, state, log}

          {:error, diag, state} ->
            {:error, diag, state, log}
        end

      {:ok, _expr, state, log} ->
        {:error, {:expected, :keyword}, state, log}

      {:error, _, _, _} = error ->
        error
    end
  end
end
