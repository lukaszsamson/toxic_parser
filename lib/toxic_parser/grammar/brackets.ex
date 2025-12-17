defmodule ToxicParser.Grammar.Brackets do
  @moduledoc false

  alias ToxicParser.{Builder, Context, EventLog, State, TokenAdapter}
  alias ToxicParser.Grammar.{Delimited, EOE, Expressions, Keywords}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

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

  @spec parse_bracket_arg_no_skip(State.t(), EventLog.t()) :: result()
  def parse_bracket_arg_no_skip(%State{} = state, %EventLog{} = log) do
    ctx = Context.container_expr()

    case Keywords.try_parse_kw_data(state, ctx, log) do
      {:ok, kw_list, state, log} ->
        {:ok, kw_list, state, log}

      {:no_kw, state, log} ->
        parse_single_container_expr(state, ctx, log)

      {:error, reason, state, log} ->
        {:error, reason, state, log}
    end
  end

  defp parse_single_container_expr(state, ctx, log) do
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

    item_fun = fn state, ctx, log -> Expressions.expr(state, ctx, log) end

    case Delimited.parse_comma_separated(checkpoint_state, ctx, log, :"]", item_fun,
           allow_empty?: false,
           allow_trailing_comma?: true,
           skip_eoe?: true
         ) do
      {:ok, [expr], state, log} ->
        {:ok, expr, TokenAdapter.drop_checkpoint(state, ref), log}

      {:ok, [_first, _second | _rest], state, log} ->
        state = TokenAdapter.rewind(state, ref)

        with {:ok, _first_expr, state, log} <- Expressions.expr(state, ctx, log) do
          {state, _newlines} = EOE.skip_count_newlines(state, 0)

          case TokenAdapter.next(state) do
            {:ok, %{kind: :","} = comma_tok, state} ->
              meta = Builder.Helpers.token_meta(comma_tok.metadata)
              {:error, {meta, @too_many_access_syntax_message, "','"}, state, log}

            {:ok, tok, state} ->
              {:error, {:expected, :",", got: tok.kind}, state, log}

            {:eof, state} ->
              {:error, :unexpected_eof, state, log}

            {:error, diag, state} ->
              {:error, diag, state, log}
          end
        end

      {:error, reason, state, log} ->
        {:error, reason, TokenAdapter.drop_checkpoint(state, ref), log}
    end
  end
end
