defmodule ToxicParser.Layout do
  @moduledoc """
  Parser-owned layout view for newline/semicolon separators.

  Provides helpers to peek and skip separator tokens (`:eol` and `:";"`).
  """

  alias ToxicParser.{State, TokenAdapter}

  # Raw separator tokens from lexer:
  # - {:eol, meta} where meta = {{start_line, start_col}, {end_line, end_col}, newline_count}
  # - {:";", meta} where meta = {{start_line, start_col}, {end_line, end_col}, newline_count}
  @type sep_token :: {:eol, tuple()} | {:";", tuple()}

  @doc """
  Peeks at the next separator (`:eol` or `:";"``) without consuming it.
  Returns the raw token.
  """
  @spec peek_sep(State.t(), term()) :: {:ok, sep_token(), State.t()} | :none
  def peek_sep(%State{} = state, _ctx \\ nil) do
    case TokenAdapter.peek(state) do
      {:ok, {:eol, _meta} = tok, state} -> {:ok, tok, state}
      {:ok, {:";", _meta} = tok, state} -> {:ok, tok, state}
      _ -> :none
    end
  end

  @doc """
  Skips all separators (`:eol` and `:";"`), returning the updated state.
  """
  @spec skip_seps(State.t(), term()) :: State.t()
  def skip_seps(%State{} = state, ctx \\ nil) do
    case peek_sep(state, ctx) do
      {:ok, _sep, state} ->
        {:ok, _tok, state} = TokenAdapter.next(state)
        skip_seps(state, ctx)

      :none ->
        state
    end
  end

  @doc """
  Skips newline separators only (`:eol`, not `:";"`).
  Returns the updated state plus total newline count.
  Used for `stab_op_eol` which only allows newlines after `->`.
  """
  @spec skip_newlines_only(State.t(), term(), non_neg_integer()) :: {State.t(), non_neg_integer()}
  def skip_newlines_only(%State{} = state, ctx \\ nil, count \\ 0) when count >= 0 do
    case peek_sep(state, ctx) do
      {:ok, {:";", _meta}, _state} ->
        # Stop at semicolon - don't consume it
        {state, count}

      {:ok, {:eol, {_, _, n}}, state} when is_integer(n) ->
        {:ok, _tok, state} = TokenAdapter.next(state)
        skip_newlines_only(state, ctx, count + n)

      {:ok, {:eol, _meta}, state} ->
        {:ok, _tok, state} = TokenAdapter.next(state)
        skip_newlines_only(state, ctx, count)

      :none ->
        {state, count}
    end
  end
end
