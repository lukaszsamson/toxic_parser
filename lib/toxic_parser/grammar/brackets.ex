defmodule ToxicParser.Grammar.Brackets do
  @moduledoc false

  alias ToxicParser.{Context, Cursor, EventLog, State, TokenAdapter}
  alias ToxicParser.Grammar.{Delimited, EOE, ErrorHelpers, Expressions, Keywords}

  @type result ::
          {:ok, Macro.t(), State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}

  @too_many_access_syntax_message "too many arguments when accessing a value. " <>
                                    "The value[key] notation in Elixir expects either a single argument or a keyword list. " <>
                                    "The following examples are allowed:\n\n" <>
                                    "    value[one]\n" <>
                                    "    value[one: 1, two: 2]\n" <>
                                    "    value[[one, two, three]]\n\n" <>
                                    "These are invalid:\n\n" <>
                                    "    value[1, 2, 3]\n" <>
                                    "    value[one, two, three]\n\n" <>
                                    "Syntax error after: "

  @spec parse_bracket_arg_no_skip(State.t(), Cursor.t(), EventLog.t()) :: result()
  def parse_bracket_arg_no_skip(%State{} = state, cursor, %EventLog{} = log) do
    ctx = Context.container_expr()

    case Keywords.try_parse_kw_data(state, cursor, ctx, log) do
      {:ok, kw_list, state, cursor, log} ->
        {:ok, kw_list, state, cursor, log}

      {:no_kw, state, cursor, log} ->
        parse_single_container_expr(state, cursor, ctx, log)

      {:error, reason, state, cursor, log} ->
        {:error, reason, state, cursor, log}
    end
  end

  defp parse_single_container_expr(state, cursor, ctx, log) do
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state, cursor)

    item_fun = fn state, cursor, ctx, log -> Expressions.expr(state, cursor, ctx, log) end

    on_error = fn reason, state, cursor, _ctx, log ->
      meta = ErrorHelpers.error_meta_from_reason(reason, cursor)
      {error_node, state} = ErrorHelpers.build_error_node(:invalid, reason, meta, state, cursor)
      {:ok, error_node, state, cursor, log}
    end

    case Delimited.parse_comma_separated(checkpoint_state, cursor, ctx, log, :"]", item_fun,
           allow_empty?: false,
           allow_trailing_comma?: true,
           skip_eoe?: true,
           on_error: on_error
         ) do
      {:ok, [expr], state, cursor, log} ->
        {:ok, expr, TokenAdapter.drop_checkpoint(state, ref), cursor, log}

      {:ok, [_first, _second | _rest] = items, state, cursor, log} ->
        if state.mode == :tolerant do
          {error_node, state} =
            build_access_error_node(
              {[], @too_many_access_syntax_message, "','"},
              [],
              state,
              cursor,
              items
            )

          {:ok, error_node, TokenAdapter.drop_checkpoint(state, ref), cursor, log}
        else
          {state, cursor} = TokenAdapter.rewind(state, ref)

          with {:ok, _first_expr, state, cursor, log} <- Expressions.expr(state, cursor, ctx, log) do
            {state, cursor, _newlines} = EOE.skip_count_newlines(state, cursor, 0)

            case TokenAdapter.next(state, cursor) do
              {:ok, {:",", _meta, _value} = comma_tok, state, cursor} ->
                meta = TokenAdapter.token_meta(comma_tok)
                {:error, {meta, @too_many_access_syntax_message, "','"}, state, cursor, log}

              {:ok, {kind, _meta, _value}, state, cursor} ->
                {:error, {:expected, :",", got: kind}, state, cursor, log}

              {:eof, state, cursor} ->
                {:error, :unexpected_eof, state, cursor, log}

              {:error, diag, state, cursor} ->
                {:error, diag, state, cursor, log}
            end
          end
        end

      {:error, reason, state, cursor, log} ->
        if state.mode == :tolerant do
          meta = ErrorHelpers.error_meta_from_reason(reason, cursor)

          {error_node, state} =
            ErrorHelpers.build_error_node(:invalid, reason, meta, state, cursor)

          {:ok, error_node, TokenAdapter.drop_checkpoint(state, ref), cursor, log}
        else
          {:error, reason, TokenAdapter.drop_checkpoint(state, ref), cursor, log}
        end
    end
  end

  defp build_access_error_node(reason, meta, %State{} = state, cursor, children) do
    ErrorHelpers.build_error_node(:unexpected, reason, meta, state, cursor, children)
  end
end
