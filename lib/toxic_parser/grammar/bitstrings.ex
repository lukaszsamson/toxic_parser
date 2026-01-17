defmodule ToxicParser.Grammar.Bitstrings do
  @moduledoc """
  Parsing for bitstring literals (<<>>).
  """

  alias ToxicParser.{
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
  alias ToxicParser.Grammar.{Delimited, EOE, Expressions, Keywords}

  @type result ::
          {:ok, Macro.t(), State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}

  @spec parse(State.t(), Cursor.t(), Pratt.context(), EventLog.t(), non_neg_integer()) :: result()
  def parse(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log, min_bp \\ 0) do
    with {:ok, ast, state, cursor, log} <- parse_base(state, cursor, ctx, log) do
      Pratt.led(ast, state, cursor, log, min_bp, ctx)
    end
  end

  @doc """
  Parse bitstring base without calling Pratt.led.
  Used when caller controls led binding (e.g., in stab patterns).
  """
  @spec parse_base(State.t(), Cursor.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_base(%State{} = state, cursor, %Context{} = _ctx, %EventLog{} = log) do
    {:ok, open_tok, state, cursor} = TokenAdapter.next(state, cursor)
    open_meta = TokenAdapter.token_meta(open_tok)

    # Skip leading EOE and count newlines
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    container_ctx = Context.container_expr()

    item_fun = fn state, cursor, _ctx, log ->
      case Keywords.try_parse_kw_data(state, cursor, container_ctx, log) do
        {:ok, kw_list, state, cursor, log} ->
          # container_args allows a keyword tail as the final element. We only
          # treat this as kw_data if the close bitstring delimiter follows.
          {state, cursor, _newlines} = EOE.skip_count_newlines(state, cursor, 0)

          case Cursor.peek(cursor) do
            {:ok, {:">>", _meta, _value}, cursor} ->
              {:ok, {:kw_data, kw_list}, state, cursor, log}

            {:ok, {kind, _meta, _value}, cursor} ->
              {:error, {:expected, :">>", got: kind}, state, cursor, log}

            {:eof, cursor} ->
              {:error, :unexpected_eof, state, cursor, log}

            {:error, diag, cursor} ->
              {:error, diag, state, cursor, log}
          end

        {:no_kw, state, cursor, log} ->
          with {:ok, expr, state, cursor, log} <-
                 Expressions.expr(state, cursor, container_ctx, log) do
            # Validate no_parens expressions are not allowed in containers
            case ExprClass.classify(expr) do
              :no_parens ->
                {:error, NoParensErrors.error_no_parens_container_strict(expr), state, cursor,
                 log}

              _ ->
                {:ok, {:expr, expr}, state, cursor, log}
            end
          end

        {:error, reason, state, cursor, log} ->
          {:error, reason, state, cursor, log}
      end
    end

    with {:ok, tagged_items, state, cursor, log} <-
           Delimited.parse_comma_separated(state, cursor, container_ctx, log, :">>", item_fun,
             allow_empty?: true
           ) do
      # Check for invalid keyword list at start of bitstring
      # <<foo: :bar>> is invalid - bitstrings cannot start with keyword data
      case tagged_items do
        [{:kw_data, _} | _] ->
          {:error, unexpected_keyword_list_error(:bitstring, open_meta), state, cursor, log}

        _ ->
          case TokenAdapter.next(state, cursor) do
            {:ok, {:">>", _meta, _value} = close_tok, state, cursor} ->
              close_meta = TokenAdapter.token_meta(close_tok)
              meta = Meta.closing_meta(open_meta, close_meta, leading_newlines)
              ast = {:<<>>, meta, finalize_bitstring_items(tagged_items)}
              {:ok, ast, state, cursor, log}

            {:ok, {kind, _meta, _value}, state, cursor} ->
              {:error, {:expected, :">>", got: kind}, state, cursor, log}

            {:eof, state, cursor} ->
              {:error, :unexpected_eof, state, cursor, log}

            {:error, diag, state, cursor} ->
              {:error, diag, state, cursor, log}
          end
      end
    end
  end

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

  defp unexpected_keyword_list_error(context, meta) do
    location = Keyword.take(meta, [:line, :column])
    start_token = if context == :bitstring, do: "'<<'", else: "'{'"

    {location,
     "unexpected keyword list inside #{context}. Did you mean to write a map (using %{...}) or a list (using [...]) instead? Syntax error after: ",
     start_token}
  end
end
