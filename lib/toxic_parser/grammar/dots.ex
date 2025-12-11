defmodule ToxicParser.Grammar.Dots do
  @moduledoc """
  Dot expression parsing helpers (`foo.bar`, `Foo.Bar`, `foo.(...)`, `foo.()`).
  """

  alias ToxicParser.{Builder, EventLog, Identifiers, State, TokenAdapter}
  alias ToxicParser.Grammar.Expressions

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @doc """
  Parse a dot chain starting from an existing left-hand AST.
  Expects the current peek token to be a dot operator.
  """
  @spec parse_chain(Macro.t(), State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_chain(left, %State{} = state, ctx, %EventLog{} = log) do
    {:ok, _dot, state} = TokenAdapter.next(state)

    with {:ok, rhs, state, log} <- parse_member(state, ctx, log) do
      combined = Builder.Helpers.dot(left, rhs)
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :dot_op}, _} ->
          parse_chain(combined, state, ctx, log)

        _ ->
          {:ok, combined, state, log}
      end
    end
  end

  @doc """
  Parses the RHS of a dot: identifier/alias/call/bracket/paren call.
  Returns `{:ok, {member_value, member_meta}, state, log}` for simple identifiers,
  or `{:ok, call_ast, state, log}` for calls.
  """
  @spec parse_member(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_member(%State{} = state, ctx, %EventLog{} = log) do
    case TokenAdapter.next(state) do
      {:ok, tok, state} ->
        case Identifiers.classify(tok.kind) do
          kind when kind in [:identifier, :do_identifier, :op_identifier, :dot_identifier, :dot_do_identifier, :dot_op_identifier, :bracket_identifier] ->
            # Return {member_atom, member_meta} tuple so caller can build proper AST
            {:ok, {tok.value, Builder.Helpers.token_meta(tok.metadata)}, state, log}

          :alias ->
            # Alias needs to be wrapped as __aliases__
            {:ok, Builder.Helpers.from_token(tok), state, log}

          kind when kind in [:paren_identifier, :dot_call_identifier, :dot_paren_identifier] ->
            parse_paren_call(tok, state, ctx, log)

          _ ->
            {:error, {:expected, :dot_member, got: tok.kind}, state, log}
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_paren_call(tok, state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    # Skip leading EOE and count newlines
    {state, leading_newlines} = skip_eoe_count_newlines(state, 0)

    with {:ok, args, state, log} <- parse_paren_args([], state, ctx, log) do
      # Skip trailing EOE before close paren
      {state, trailing_newlines} = skip_eoe_count_newlines(state, 0)

      case TokenAdapter.next(state) do
        {:ok, %{kind: :")"} = close_tok, state} ->
          total_newlines = leading_newlines + trailing_newlines
          callee_meta = Builder.Helpers.token_meta(tok.metadata)
          close_meta = Builder.Helpers.token_meta(close_tok.metadata)

          # Build metadata: [newlines: N, closing: [...], line: L, column: C]
          newlines_meta = if total_newlines > 0, do: [newlines: total_newlines], else: []
          meta = newlines_meta ++ [closing: close_meta] ++ callee_meta

          callee = Builder.Helpers.from_token(tok)
          {:ok, {callee, meta, Enum.reverse(args)}, state, log}

        {:ok, other, state} ->
          {:error, {:expected, :")", got: other.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state}

        {:error, diag, state} ->
          {:error, diag, state}
      end
    end
  end

  defp skip_eoe_count_newlines(state, count) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, value: %{newlines: n}}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe_count_newlines(state, count + n)

      _ ->
        {state, count}
    end
  end

  defp parse_paren_args(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :")"}, state} ->
        {:ok, acc, state, log}

      {:ok, _tok, _} ->
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
end
