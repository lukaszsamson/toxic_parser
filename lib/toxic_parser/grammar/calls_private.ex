defmodule ToxicParser.Grammar.CallsPrivate do
  @moduledoc false
  # Internal helpers exposed for reuse (Pratt dot-call handling).

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

  alias ToxicParser.Grammar.{Delimited, EOE, Expressions, Keywords}

  @spec parse_paren_args([Macro.t()], State.t(), Cursor.t(), Pratt.context(), EventLog.t()) ::
          {:ok, [Macro.t()], State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}
  def parse_paren_args(acc, %State{} = state, cursor, %Context{}, log) do
    container_ctx = Context.container_expr()

    {state, cursor} = EOE.skip(state, cursor)

    case Cursor.peek(cursor) do
      {:ok, {:")", _meta, _value}, _cursor} ->
        {:ok, acc, state, cursor, log}

      {:ok, _tok, _cursor} ->
        with {:ok, args, state, cursor, log} <-
               parse_call_args_parens(state, cursor, container_ctx, log) do
          {:ok, :lists.reverse(args, acc), state, cursor, log}
        end

      {:eof, _cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, _cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  defp parse_call_args_parens(state, cursor, container_ctx, log) do
    case Keywords.try_parse_kw_call(state, cursor, container_ctx, log) do
      {:ok, kw_list, state, cursor, log} ->
        {state, cursor} = EOE.skip(state, cursor)

        case Cursor.peek(cursor) do
          {:ok, {:")", _meta, _value}, _cursor} ->
            {:ok, [kw_list], state, cursor, log}

          {:ok, {kind, _meta, _value}, _cursor} ->
            {:error, {:expected, :")", got: kind}, state, cursor, log}

          {:eof, _cursor} ->
            {:error, :unexpected_eof, state, cursor, log}

          {:error, diag, _cursor} ->
            {:error, diag, state, cursor, log}
        end

      {:no_kw, state, cursor, log} ->
        try_single_no_parens_arg(state, cursor, log, fn state, cursor, log ->
          parse_call_args_parens_list(state, cursor, container_ctx, log)
        end)

      {:error, reason, state, cursor, log} ->
        {:error, reason, state, cursor, log}
    end
  end

  defp try_single_no_parens_arg(state, cursor, log, fallback) do
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state, cursor)

    case Expressions.expr(checkpoint_state, cursor, Context.no_parens_expr(), log) do
      {:ok, expr, state, cursor, log} ->
        {state, cursor} = EOE.skip(state, cursor)

        case Cursor.peek(cursor) do
          {:ok, {:")", _meta, _value}, _cursor} ->
            {:ok, [expr], TokenAdapter.drop_checkpoint(state, ref), cursor, log}

          _ ->
            {state, cursor} = TokenAdapter.rewind(state, cursor, ref)
            fallback.(state, cursor, log)
        end

      {:error, _reason, state, cursor, log} ->
        {state, cursor} = TokenAdapter.rewind(state, cursor, ref)
        fallback.(state, cursor, log)
    end
  end

  defp parse_call_args_parens_list(state, cursor, container_ctx, log) do
    item_fun = fn state, cursor, _ctx, log ->
      case Keywords.try_parse_kw_call(state, cursor, container_ctx, log) do
        {:ok, kw_list, state, cursor, log} ->
          {state, cursor} = EOE.skip(state, cursor)

          case Cursor.peek(cursor) do
            {:ok, {:")", _meta, _value}, _cursor} ->
              {:ok, {:kw_call, kw_list}, state, cursor, log}

            {:ok, {:",", _meta, _value} = comma_tok, _cursor} ->
              meta = TokenAdapter.token_meta(comma_tok)

              {:error, {meta, unexpected_expression_after_kw_call_message(), "','"}, state,
               cursor, log}

            {:ok, {kind, _meta, _value}, _cursor} ->
              {:error, {:expected, :")", got: kind}, state, cursor, log}

            {:eof, _cursor} ->
              {:error, :unexpected_eof, state, cursor, log}

            {:error, diag, _cursor} ->
              {:error, diag, state, cursor, log}
          end

        {:no_kw, state, cursor, log} ->
          with {:ok, expr, state, cursor, log} <-
                 Expressions.expr(state, cursor, container_ctx, log) do
            # Validate no_parens expressions are not allowed inside paren calls
            case ExprClass.classify(expr) do
              :no_parens ->
                {:error, NoParensErrors.error_no_parens_many_strict(expr), state, cursor, log}

              _ ->
                {:ok, {:expr, expr}, state, cursor, log}
            end
          end

        {:error, reason, state, cursor, log} ->
          {:error, reason, state, cursor, log}
      end
    end

    with {:ok, tagged_items, state, cursor, log} <-
           Delimited.parse_comma_separated(state, cursor, container_ctx, log, :")", item_fun,
             allow_empty?: false,
             allow_trailing_comma?: false
           ) do
      {:ok, finalize_call_args_parens_items(tagged_items), state, cursor, log}
    end
  end

  defp finalize_call_args_parens_items(tagged_items) do
    case List.last(tagged_items) do
      {:kw_call, kw_list} ->
        exprs =
          tagged_items
          |> Enum.drop(-1)
          |> Enum.map(fn {:expr, expr} -> expr end)

        exprs ++ [kw_list]

      _ ->
        Enum.map(tagged_items, fn {:expr, expr} -> expr end)
    end
  end

  defp unexpected_expression_after_kw_call_message do
    "unexpected expression after keyword list. Keyword lists must always come as the last argument. " <>
      "Therefore, this is not allowed:\n\n" <>
      "    function_call(1, some: :option, 2)\n\n" <>
      "Instead, wrap the keyword in brackets:\n\n" <>
      "    function_call(1, [some: :option], 2)\n\n" <>
      "Syntax error after: "
  end
end
