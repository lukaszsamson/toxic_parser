defmodule ToxicParser.Grammar.Bitstrings do
  @moduledoc """
  Parsing for bitstring literals (<<>>).
  """

  alias ToxicParser.{Builder, Context, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.{Delimited, EOE, Expressions, Keywords}

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
  def parse_base(%State{} = state, %Context{} = _ctx, %EventLog{} = log) do
    {:ok, open_tok, state} = TokenAdapter.next(state)
    open_meta = token_meta(open_tok.metadata)

    # Skip leading EOE and count newlines
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    container_ctx = Context.container_expr()

    item_fun = fn state, _ctx, log ->
      case Keywords.try_parse_kw_data(state, container_ctx, log) do
        {:ok, kw_list, state, log} ->
          # container_args allows a keyword tail as the final element. We only
          # treat this as kw_data if the close bitstring delimiter follows.
          {state, _newlines} = EOE.skip_count_newlines(state, 0)

          case TokenAdapter.peek(state) do
            {:ok, %{kind: :">>"}, _} ->
              {:ok, {:kw_data, kw_list}, state, log}

            {:ok, %{kind: kind}, state} ->
              {:error, {:expected, :">>", got: kind}, state, log}

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
           Delimited.parse_comma_separated(state, container_ctx, log, :">>", item_fun,
             allow_empty?: true
           ) do
      # Check for invalid keyword list at start of bitstring
      # <<foo: :bar>> is invalid - bitstrings cannot start with keyword data
      case tagged_items do
        [{:kw_data, _} | _] ->
          {:error, :unexpected_keyword_list_in_binary, state, log}

        _ ->
          case TokenAdapter.next(state) do
            {:ok, %{kind: :">>"} = close_tok, state} ->
              close_meta = token_meta(close_tok.metadata)
              meta = Meta.closing_meta(open_meta, close_meta, leading_newlines)
              ast = {:<<>>, meta, finalize_bitstring_items(tagged_items)}
              {:ok, ast, state, log}

            {:ok, tok, state} ->
              {:error, {:expected, :">>", got: tok.kind}, state, log}

            {:eof, state} ->
              {:error, :unexpected_eof, state, log}

            {:error, diag, state} ->
              {:error, diag, state, log}
          end
      end
    end
  end

  defp token_meta(meta), do: Builder.Helpers.token_meta(meta)

  defp finalize_bitstring_items([]), do: []

  defp finalize_bitstring_items(tagged_items) do
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
