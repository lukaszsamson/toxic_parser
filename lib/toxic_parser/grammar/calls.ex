defmodule ToxicParser.Grammar.Calls do
  @moduledoc """
  Call parsing skeleton for Phase 5.

  This module classifies identifiers and dispatches to paren/no-parens call
  parsers. Full call argument parsing and ambiguity handling will be filled in
  subsequent iterations.
  """

  alias ToxicParser.{Builder, EventLog, Identifiers, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.{Blocks, Expressions, Keywords}

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

      {:ok, next_tok, _} ->
        cond do
          # Binary operator follows - let Pratt handle expression
          Pratt.bp(next_tok.kind) ->
            state = TokenAdapter.pushback(state, tok)
            Pratt.parse(state, ctx, log)

          # Could be no-parens call argument
          can_be_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok) ->
            parse_no_parens_call(tok, state, ctx, log)

          # Just a bare identifier
          true ->
            ast = Builder.Helpers.from_token(tok)
            maybe_do_block(ast, state, ctx, log)
        end

      _ ->
        ast = Builder.Helpers.from_token(tok)
        maybe_do_block(ast, state, ctx, log)
    end
  end

  # Check if a token can be the start of a no-parens call argument
  defp can_be_no_parens_arg?(%{kind: kind}) do
    kind in [
      :int,
      :flt,
      :char,
      :atom,
      :string,
      :identifier,
      :alias,
      true,
      false,
      nil,
      :"{",
      :"[",
      :"<<",
      :unary_op,
      :at_op,
      :capture_op,
      :dual_op
    ]
  end

  # Parse a no-parens call like `foo 1`, `foo 1, 2`, or `foo a: 1`.
  defp parse_no_parens_call(callee_tok, state, ctx, log) do
    with {:ok, args, state, log} <- parse_no_parens_args([], state, ctx, log) do
      callee = callee_tok.value
      meta = Builder.Helpers.token_meta(callee_tok.metadata)
      ast = {callee, meta, args}
      maybe_do_block(ast, state, ctx, log)
    end
  end

  defp parse_paren_call(callee_tok, state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    with {:ok, args, state, log} <- parse_paren_args([], state, ctx, log),
         {:ok, _close, state} <- expect(state, :")") do
      ast = Builder.Helpers.call(callee_tok.value, Enum.reverse(args))
      maybe_do_block(ast, state, ctx, log)
    end
  end

  defp parse_paren_args(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :")"}, state} ->
        {:ok, acc, state, log}

      {:ok, tok, _state} ->
        cond do
          Keywords.starts_kw?(tok) ->
            with {:ok, kw_list, state, log} <- Keywords.parse_kw_call(state, ctx, log) do
              {:ok, [kw_list | acc], state, log}
            end

          true ->
            with {:ok, arg, state, log} <- Expressions.expr(state, ctx, log) do
              case TokenAdapter.peek(state) do
                {:ok, %{kind: :","}, state} ->
                  {:ok, _comma, state} = TokenAdapter.next(state)
                  parse_paren_args([arg | acc], state, ctx, log)

                _ ->
                  parse_paren_args([arg | acc], state, ctx, log)
              end
            end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_no_parens_args(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: kind}, _} when kind in [:eoe, :")", :"]", :"}", :do] ->
        {:ok, Enum.reverse(acc), state, log}

      {:ok, tok, _} ->
        cond do
          Keywords.starts_kw?(tok) ->
            with {:ok, kw_list, state, log} <- Keywords.parse_kw_call(state, ctx, log) do
              {:ok, Enum.reverse([[kw_list] | acc]), state, log}
            end

          true ->
            with {:ok, arg, state, log} <- Expressions.expr(state, ctx, log) do
              case TokenAdapter.peek(state) do
                {:ok, %{kind: :","}, state} ->
                  {:ok, _comma, state} = TokenAdapter.next(state)
                  parse_no_parens_args([arg | acc], state, ctx, log)

                _ ->
                  {:ok, Enum.reverse([arg | acc]), state, log}
              end
            end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp maybe_do_block(ast, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :do}, _} ->
        with {:ok, {block_meta, sections}, state, log} <- Blocks.parse_do_block(state, ctx, log) do
          ast =
            case ast do
              {name, meta, args} when is_list(args) ->
                # Prepend do/end metadata to the call's existing metadata
                {name, block_meta ++ meta, args ++ [sections]}

              other ->
                Builder.Helpers.call(other, [sections], block_meta)
            end

          {:ok, ast, state, log}
        end

      _ ->
        {:ok, ast, state, log}
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
