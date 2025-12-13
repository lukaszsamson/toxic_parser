defmodule ToxicParser.Grammar.CallsPrivate do
  @moduledoc false
  # Internal helpers exposed for reuse (Pratt dot-call handling).

  alias ToxicParser.{EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.{Expressions, Keywords}

  # Check if an expression result is a keyword list (from quoted keyword parsing)
  defguardp is_keyword_list_result(arg)
            when is_list(arg) and length(arg) > 0

  @spec expect(State.t(), atom()) :: {:ok, atom(), State.t()} | {:error, term(), State.t()}
  def expect(state, kind) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: ^kind}, state} -> {:ok, kind, state}
      {:ok, token, state} -> {:error, {:expected, kind, got: token.kind}, state}
      {:eof, state} -> {:error, :unexpected_eof, state}
      {:error, diag, state} -> {:error, diag, state}
    end
  end

  @spec parse_paren_args([Macro.t()], State.t(), Pratt.context(), EventLog.t()) ::
          {:ok, [Macro.t()], State.t(), EventLog.t()} | {:error, term(), State.t(), EventLog.t()}
  def parse_paren_args(acc, state, _ctx, log) do
    # Skip EOE before checking for close paren or next arg
    state = skip_eoe(state)

    case TokenAdapter.peek(state) do
      {:ok, %{kind: :")"}, _} ->
        {:ok, acc, state, log}

      {:ok, tok, _state} ->
        cond do
          Keywords.starts_kw?(tok) ->
            # Inside parens, use :unmatched so do_identifiers can consume do-blocks
            with {:ok, kw_list, state, log} <- Keywords.parse_kw_call(state, :unmatched, log) do
              {:ok, [kw_list | acc], state, log}
            end

          true ->
            # Inside parens, use :unmatched so do_identifiers (case, if, etc.)
            # can consume their do-blocks. This is correct because when inside parens,
            # the do-block clearly belongs to the inner expression, not an outer call.
            # e.g., foo(case a do x -> y end) - the do belongs to case, not foo
            with {:ok, arg, state, log} <- Expressions.expr(state, :unmatched, log) do
              # Skip EOE after arg before checking for comma
              state = skip_eoe(state)

              case TokenAdapter.peek(state) do
                {:ok, %{kind: :","}, _} ->
                  {:ok, _comma, state} = TokenAdapter.next(state)
                  # Check if arg was a keyword list from quoted key parsing (e.g., "foo": 1)
                  # If so, and next is also a keyword, merge them
                  state = skip_eoe(state)

                  case TokenAdapter.peek(state) do
                    {:ok, next_tok, _} when is_keyword_list_result(arg) ->
                      if Keywords.starts_kw?(next_tok) do
                        # Continue collecting keywords into this list
                        with {:ok, more_kw, state, log} <-
                               Keywords.parse_kw_call(state, :unmatched, log) do
                          merged_kw = arg ++ more_kw
                          {:ok, [merged_kw | acc], state, log}
                        end
                      else
                        parse_paren_args([arg | acc], state, :unmatched, log)
                      end

                    _ ->
                      parse_paren_args([arg | acc], state, :unmatched, log)
                  end

                _ ->
                  {:ok, [arg | acc], state, log}
              end
            end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp skip_eoe(state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe(state)

      _ ->
        state
    end
  end
end
