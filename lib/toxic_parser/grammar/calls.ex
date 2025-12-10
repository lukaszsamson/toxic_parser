defmodule ToxicParser.Grammar.Calls do
  @moduledoc """
  Call parsing skeleton for Phase 5.

  This module classifies identifiers and dispatches to paren/no-parens call
  parsers. Full call argument parsing and ambiguity handling will be filled in
  subsequent iterations.
  """

  alias ToxicParser.{Builder, EventLog, Identifiers, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.Expressions

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @doc """
  Parses a call-or-identifier expression. Currently falls back to Pratt for
  non-call forms and wraps bare identifiers as AST.
  """
  @spec parse(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse(%State{} = state, ctx, %EventLog{} = log) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        case Identifiers.classify(tok.kind) do
          :other ->
            Pratt.parse(state, ctx, log)

          ident_kind ->
            {:ok, _tok, state} = TokenAdapter.next(state)
            parse_identifier(ident_kind, tok, state, ctx, log)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_identifier(_kind, tok, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"("}, _} ->
        parse_paren_call(tok, state, ctx, log)

      _ ->
        ast = Builder.Helpers.literal(tok.value)
        {:ok, ast, state, log}
    end
  end

  defp parse_paren_call(callee_tok, state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    with {:ok, args, state, log} <- parse_paren_args([], state, ctx, log),
         {:ok, _close, state} <- expect(state, :")") do
      ast = Builder.Helpers.call(callee_tok.value, Enum.reverse(args))
      {:ok, ast, state, log}
    end
  end

  defp parse_paren_args(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :")"}, state} ->
        {:ok, acc, state, log}

      {:ok, _tok, _state} ->
        with {:ok, arg, state, log} <- Expressions.expr(state, ctx, log) do
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :","}, state} ->
              {:ok, _comma, state} = TokenAdapter.next(state)
              parse_paren_args([arg | acc], state, ctx, log)

            _ ->
              parse_paren_args([arg | acc], state, ctx, log)
          end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp expect(state, kind) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: ^kind}, state} -> {:ok, kind, state}
      {:ok, token, state} -> {:error, {:expected, kind, got: token.kind}, state}
      {:eof, state} -> {:error, :unexpected_eof, state}
      {:error, diag, state} -> {:error, diag, state}
    end
  end
end
