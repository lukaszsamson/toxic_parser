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
  alias ToxicParser.Grammar.{Delimited, EOE, ErrorHelpers, Expressions, Keywords}

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
          {ref, checkpoint_state} = TokenAdapter.checkpoint(state, cursor)

          case Expressions.expr(checkpoint_state, cursor, container_ctx, log) do
            {:ok, expr, state, cursor, log} ->
              # Validate no_parens expressions are not allowed in containers
              case ExprClass.classify(expr) do
                :no_parens ->
                  if state.mode == :tolerant do
                    {state, cursor} = TokenAdapter.rewind(state, ref)
                    reason = NoParensErrors.error_no_parens_container_strict(expr)
                    meta = ErrorHelpers.error_meta_from_reason(reason, cursor)
                    {error_node, state} = ErrorHelpers.build_error_node(:invalid, reason, meta, state, cursor)
                    {state, cursor} = sync_to_bitstring_separator(state, cursor)
                    {:ok, {:expr, error_node}, state, cursor, log}
                  else
                    state = TokenAdapter.drop_checkpoint(state, ref)

                    {:error, NoParensErrors.error_no_parens_container_strict(expr), state, cursor,
                     log}
                  end

                _ ->
                  state = TokenAdapter.drop_checkpoint(state, ref)
                  {:ok, {:expr, expr}, state, cursor, log}
              end

            {:error, reason, state, cursor, log} ->
              if state.mode == :tolerant do
                {state, cursor} = TokenAdapter.rewind(state, ref)
                {:error, reason, state, cursor, log}
              else
                state = TokenAdapter.drop_checkpoint(state, ref)
                {:error, reason, state, cursor, log}
              end
          end

        {:error, reason, state, cursor, log} ->
          {:error, reason, state, cursor, log}
      end
    end

    on_error = fn reason, state, cursor, _ctx, log ->
      meta = error_meta_from_reason(reason, cursor)
      {error_node, state} = build_container_error_node(reason, meta, state, cursor)
      {:ok, {:expr, error_node}, state, cursor, log}
    end

    with {:ok, tagged_items, state, cursor, log} <-
           Delimited.parse_comma_separated(state, cursor, container_ctx, log, :">>", item_fun,
             allow_empty?: true,
             on_error: on_error
           ) do
      # Check for invalid keyword list at start of bitstring
      # <<foo: :bar>> is invalid - bitstrings cannot start with keyword data
      case tagged_items do
        [{:kw_data, _} | _] ->
          if state.mode == :tolerant do
            {error_ast, state} =
              build_container_error_node(
                unexpected_keyword_list_error(:bitstring, open_meta),
                open_meta,
                state,
                cursor
              )

            tagged_items = [{:expr, error_ast}]

            case TokenAdapter.next(state, cursor) do
              {:ok, {:">>", _meta, _value} = close_tok, state, cursor} ->
                close_meta = TokenAdapter.token_meta(close_tok)
                meta = Meta.closing_meta(open_meta, close_meta, leading_newlines)
                ast = {:<<>>, meta, finalize_bitstring_items(tagged_items)}
                {:ok, ast, state, cursor, log}

              {:ok, {kind, _meta, _value} = tok, state, cursor} ->
                if state.mode == :tolerant do
                  {state, cursor} = TokenAdapter.pushback(state, cursor, tok)
                  {error_ast, state} =
                    build_container_error_node({:expected, :">>", got: kind}, open_meta, state, cursor)

                  {:ok, error_ast, state, cursor, log}
                else
                  {:error, {:expected, :">>", got: kind}, state, cursor, log}
                end

              {:eof, state, cursor} ->
                if state.mode == :tolerant do
                  {error_ast, state} =
                    build_container_error_node(:unexpected_eof, open_meta, state, cursor)

                  {:ok, error_ast, state, cursor, log}
                else
                  {:error, :unexpected_eof, state, cursor, log}
                end

              {:error, diag, state, cursor} ->
                if state.mode == :tolerant do
                  {error_ast, state} = build_container_error_node(diag, open_meta, state, cursor)
                  {:ok, error_ast, state, cursor, log}
                else
                  {:error, diag, state, cursor, log}
                end
            end
          else
            {:error, unexpected_keyword_list_error(:bitstring, open_meta), state, cursor, log}
          end

        _ ->
          case TokenAdapter.next(state, cursor) do
            {:ok, {:">>", _meta, _value} = close_tok, state, cursor} ->
              close_meta = TokenAdapter.token_meta(close_tok)
              meta = Meta.closing_meta(open_meta, close_meta, leading_newlines)
              ast = {:<<>>, meta, finalize_bitstring_items(tagged_items)}
              {:ok, ast, state, cursor, log}

            {:ok, {kind, _meta, _value} = tok, state, cursor} ->
              if state.mode == :tolerant do
                {state, cursor} = TokenAdapter.pushback(state, cursor, tok)
                {error_ast, state} =
                  build_container_error_node({:expected, :">>", got: kind}, open_meta, state, cursor)

                {:ok, error_ast, state, cursor, log}
              else
                {:error, {:expected, :">>", got: kind}, state, cursor, log}
              end

            {:eof, state, cursor} ->
              if state.mode == :tolerant do
                {error_ast, state} =
                  build_container_error_node(:unexpected_eof, open_meta, state, cursor)

                {:ok, error_ast, state, cursor, log}
              else
                {:error, :unexpected_eof, state, cursor, log}
              end

            {:error, diag, state, cursor} ->
              if state.mode == :tolerant do
                {error_ast, state} = build_container_error_node(diag, open_meta, state, cursor)
                {:ok, error_ast, state, cursor, log}
              else
                {:error, diag, state, cursor, log}
              end
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

  defp build_container_error_node(reason, meta, %State{} = state, cursor) do
    ErrorHelpers.build_error_node(:unexpected, reason, meta, state, cursor)
  end

  defp error_meta_from_reason(reason, cursor) do
    case reason do
      {meta, _msg, _token} when is_list(meta) ->
        meta

      {meta, _msg} when is_list(meta) ->
        meta

      {{line, column}, _, _} when is_integer(line) and is_integer(column) ->
        [line: line, column: column]

      _ ->
        {line, column} = Cursor.position(cursor)
        [line: line || 1, column: column || 1]
    end
  end

  defp sync_to_bitstring_separator(state, cursor) do
    case Cursor.peek(cursor) do
      {:ok, {kind, _meta, _value}, cursor} when kind in [:">>", :","] ->
        {state, cursor}

      {:ok, _tok, cursor} ->
        case TokenAdapter.next(state, cursor) do
          {:ok, _tok, state, cursor} -> sync_to_bitstring_separator(state, cursor)
          {:eof, state, cursor} -> {state, cursor}
          {:error, _reason, state, cursor} -> {state, cursor}
        end

      {:eof, _cursor} ->
        {state, cursor}

      {:error, _reason, _cursor} ->
        {state, cursor}
    end
  end
end
