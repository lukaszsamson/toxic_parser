defmodule ToxicParser.Grammar.CallsPrivate do
  @moduledoc false
  # Internal helpers exposed for reuse (Pratt dot-call handling).

  alias ToxicParser.{
    Context,
    EventLog,
    ExprClass,
    NoParensErrors,
    Pratt,
    State,
    TokenAdapter
  }

  alias ToxicParser.Grammar.{Delimited, EOE, Expressions, Keywords}

  @spec parse_paren_args([Macro.t()], State.t(), Pratt.context(), EventLog.t()) ::
          {:ok, [Macro.t()], State.t(), EventLog.t()} | {:error, term(), State.t(), EventLog.t()}
  def parse_paren_args(acc, %State{} = state, %Context{}, log) do
    container_ctx = Context.container_expr()

    state = EOE.skip(state)

    case TokenAdapter.peek(state) do
      {:ok, tok, _} when elem(tok, 0) == :")" ->
        {:ok, acc, state, log}

      {:ok, _tok, _} ->
        with {:ok, args, state, log} <- parse_call_args_parens(state, container_ctx, log) do
          {:ok, Enum.reverse(args) ++ acc, state, log}
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_call_args_parens(state, container_ctx, log) do
    case Keywords.try_parse_kw_call(state, container_ctx, log) do
      {:ok, kw_list, state, log} ->
        state = EOE.skip(state)

        case TokenAdapter.peek(state) do
          {:ok, tok, state} when elem(tok, 0) == :")" ->
            {:ok, [kw_list], state, log}

          {:ok, tok, state} ->
            {:error, {:expected, :")", got: TokenAdapter.kind(tok)}, state, log}

          {:eof, state} ->
            {:error, :unexpected_eof, state, log}

          {:error, diag, state} ->
            {:error, diag, state, log}
        end

      {:no_kw, state, log} ->
        try_single_no_parens_arg(state, log, fn state, log ->
          parse_call_args_parens_list(state, container_ctx, log)
        end)

      {:error, reason, state, log} ->
        {:error, reason, state, log}
    end
  end

  defp try_single_no_parens_arg(state, log, fallback) do
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

    case Expressions.expr(checkpoint_state, Context.no_parens_expr(), log) do
      {:ok, expr, state, log} ->
        state = EOE.skip(state)

        case TokenAdapter.peek(state) do
          {:ok, tok, state} when elem(tok, 0) == :")" ->
            {:ok, [expr], TokenAdapter.drop_checkpoint(state, ref), log}

          _ ->
            fallback.(TokenAdapter.rewind(state, ref), log)
        end

      {:error, _reason, state, log} ->
        fallback.(TokenAdapter.rewind(state, ref), log)
    end
  end

  defp parse_call_args_parens_list(state, container_ctx, log) do
    item_fun = fn state, _ctx, log ->
      case Keywords.try_parse_kw_call(state, container_ctx, log) do
        {:ok, kw_list, state, log} ->
          state = EOE.skip(state)

          case TokenAdapter.peek(state) do
            {:ok, tok, state} when elem(tok, 0) == :")" ->
              {:ok, {:kw_call, kw_list}, state, log}

            {:ok, comma_tok, state} when elem(comma_tok, 0) == :"," ->
              meta = TokenAdapter.token_meta(comma_tok)
              {:error, {meta, unexpected_expression_after_kw_call_message(), "','"}, state, log}

            {:ok, tok, state} ->
              {:error, {:expected, :")", got: TokenAdapter.kind(tok)}, state, log}

            {:eof, state} ->
              {:error, :unexpected_eof, state, log}

            {:error, diag, state} ->
              {:error, diag, state, log}
          end

        {:no_kw, state, log} ->
          with {:ok, expr, state, log} <- Expressions.expr(state, container_ctx, log) do
            # Validate no_parens expressions are not allowed inside paren calls
            case ExprClass.classify(expr) do
              :no_parens ->
                {:error, NoParensErrors.error_no_parens_many_strict(expr), state, log}

              _ ->
                {:ok, {:expr, expr}, state, log}
            end
          end

        {:error, reason, state, log} ->
          {:error, reason, state, log}
      end
    end

    with {:ok, tagged_items, state, log} <-
           Delimited.parse_comma_separated(state, container_ctx, log, :")", item_fun,
             allow_empty?: false,
             allow_trailing_comma?: false
           ) do
      {:ok, finalize_call_args_parens_items(tagged_items), state, log}
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
