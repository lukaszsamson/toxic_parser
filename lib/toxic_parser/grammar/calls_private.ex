defmodule ToxicParser.Grammar.CallsPrivate do
  @moduledoc false
  # Internal helpers exposed for reuse (Pratt dot-call handling).

  alias ToxicParser.{
    Builder,
    Context,
    Cursor,
    Error,
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
      {:ok, {:")", _meta, _value}, cursor} ->
        {:ok, acc, state, cursor, log}

      {:ok, _tok, cursor} ->
        with {:ok, args, state, cursor, log} <-
               parse_call_args_parens(state, cursor, container_ctx, log) do
          {:ok, :lists.reverse(args, acc), state, cursor, log}
        end

      {:eof, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  defp parse_call_args_parens(state, cursor, container_ctx, log) do
    case Keywords.try_parse_kw_call(state, cursor, container_ctx, log) do
      {:ok, kw_list, state, cursor, log} ->
        {state, cursor} = EOE.skip(state, cursor)

        case Cursor.peek(cursor) do
          {:ok, {:")", _meta, _value}, cursor} ->
            {:ok, [kw_list], state, cursor, log}

          {:ok, {kind, _meta, _value}, cursor} ->
            {:error, {:expected, :")", got: kind}, state, cursor, log}

          {:eof, cursor} ->
            {:error, :unexpected_eof, state, cursor, log}

          {:error, diag, cursor} ->
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
          {:ok, {:")", _meta, _value}, cursor} ->
            {:ok, [expr], TokenAdapter.drop_checkpoint(state, ref), cursor, log}

          _ ->
            {state, cursor} = TokenAdapter.rewind(state, ref)
            fallback.(state, cursor, log)
        end

      {:error, _reason, state, _cursor, log} ->
        {state, cursor} = TokenAdapter.rewind(state, ref)
        fallback.(state, cursor, log)
    end
  end

  defp parse_call_args_parens_list(state, cursor, container_ctx, log) do
    item_fun = fn state, cursor, _ctx, log ->
      case Keywords.try_parse_kw_call(state, cursor, container_ctx, log) do
        {:ok, kw_list, state, cursor, log} ->
          {state, cursor} = EOE.skip(state, cursor)

          case Cursor.peek(cursor) do
            {:ok, {:")", _meta, _value}, cursor} ->
            {:ok, {:kw_call, kw_list}, state, cursor, log}

            {:ok, {:",", _meta, _value} = comma_tok, cursor} ->
              meta = TokenAdapter.token_meta(comma_tok)

              if state.mode == :tolerant do
                {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)
                {state, cursor} = EOE.skip(state, cursor)

                case Expressions.expr(state, cursor, container_ctx, log) do
                  {:ok, expr, state, cursor, log} ->
                    {error_node, state} =
                      build_kw_tail_error_node(
                        {meta, unexpected_expression_after_kw_call_message(), "','"},
                        meta,
                        state,
                        cursor,
                        [expr]
                      )

                    {:ok, {:many, [{:expr, kw_list}, {:expr, error_node}]}, state, cursor, log}

                  {:error, reason, state, cursor, log} ->
                    {error_node, state} = build_kw_tail_error_node(reason, meta, state, cursor, [])

                    {:ok, {:many, [{:expr, kw_list}, {:expr, error_node}]}, state, cursor, log}
                end
              else
                {:error, {meta, unexpected_expression_after_kw_call_message(), "','"}, state,
                 cursor, log}
              end

            {:ok, {kind, _meta, _value}, cursor} ->
              {:error, {:expected, :")", got: kind}, state, cursor, log}

            {:eof, cursor} ->
              {:error, :unexpected_eof, state, cursor, log}

            {:error, diag, cursor} ->
              {:error, diag, state, cursor, log}
          end

        {:no_kw, state, cursor, log} ->
          with {:ok, expr, state, cursor, log} <-
                 Expressions.expr(state, cursor, container_ctx, log) do
            # Validate no_parens expressions are not allowed inside paren calls
            case ExprClass.classify(expr) do
              :no_parens ->
                if state.mode == :tolerant do
                  meta =
                    case expr do
                      {_, meta, _} -> meta
                      _ -> []
                    end

                  {error_node, state} =
                    build_kw_tail_error_node(
                      NoParensErrors.error_no_parens_many_strict(expr),
                      meta,
                      state,
                      cursor,
                      []
                    )

                  {:ok, {:expr, error_node}, state, cursor, log}
                else
                  {:error, NoParensErrors.error_no_parens_many_strict(expr), state, cursor, log}
                end

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

  defp build_kw_tail_error_node(reason, meta, %State{} = state, cursor, children) do
    {line, column} =
      case meta do
        meta when is_list(meta) ->
          {Keyword.get(meta, :line), Keyword.get(meta, :column)}

        {line, column} when is_integer(line) ->
          {line, column}

        _ ->
          Cursor.position(cursor)
      end

    {line, column} =
      case {line, column} do
        {nil, nil} -> Cursor.position(cursor)
        {line, column} -> {line || 1, column || 1}
      end

    {id, state} = State.next_diagnostic_id(state)

    diagnostic =
      Error.from_parser(nil, reason,
        line_index: state.line_index,
        source: state.source,
        position: {{line, column}, {line, column}}
      )
      |> Error.annotate(%{
        id: id,
        anchor: %{kind: :error_node, path: [], note: nil},
        synthetic?: false,
        lexer_error_code: nil
      })

    diagnostic = %{diagnostic | details: Map.put(diagnostic.details, :source, :grammar)}
    state = %{state | diagnostics: [diagnostic | state.diagnostics]}

    payload =
      Error.error_node_payload(diagnostic,
        kind: :unexpected,
        original: reason,
        children: children,
        synthetic?: false
      )

    error_meta = [line: line, column: column, toxic: %{synthetic?: false, anchor: %{line: line, column: column}}]
    {Builder.Helpers.error(payload, error_meta), state}
  end
end
