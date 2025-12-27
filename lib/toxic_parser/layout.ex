defmodule ToxicParser.Layout do
  @moduledoc """
  Parser-owned layout view for newline/semicolon separators.

  Provides helpers to peek and skip separator tokens (`:eol` and `:";"`).
  """

  alias ToxicParser.{Cursor, State, TokenAdapter}

  # Raw separator tokens from lexer:
  # - {:eol, meta} where meta = {{start_line, start_col}, {end_line, end_col}, newline_count}
  # - {:";", meta} where meta = {{start_line, start_col}, {end_line, end_col}, newline_count}
  @type sep_token :: {:eol, tuple()} | {:";", tuple()}

  @doc """
  Peeks at the next separator (`:eol` or `:";"``) without consuming it.
  Returns the raw token.
  """
  @spec peek_sep(State.t(), Cursor.t(), term()) :: {:ok, sep_token(), State.t(), Cursor.t()} | :none
  def peek_sep(%State{} = state, cursor, _ctx \\ nil) do
    case TokenAdapter.peek(state, cursor) do
      {:ok, {:eol, _meta} = tok, state, cursor} -> {:ok, tok, state, cursor}
      {:ok, {:";", _meta} = tok, state, cursor} -> {:ok, tok, state, cursor}
      _ -> :none
    end
  end

  @doc """
  Skips all separators (`:eol` and `:";"`), returning the updated state.
  """
  @spec skip_seps(State.t(), Cursor.t(), term()) :: {State.t(), Cursor.t()}
  def skip_seps(%State{} = state, cursor, ctx \\ nil) do
    case peek_sep(state, cursor, ctx) do
      {:ok, _sep, state, cursor} ->
        {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)
        skip_seps(state, cursor, ctx)

      :none ->
        {state, cursor}
    end
  end

  @doc """
  Skips newline separators only (`:eol`, not `:";"`).
  Returns the updated state plus total newline count.
  Used for `stab_op_eol` which only allows newlines after `->`.
  """
  @spec skip_newlines_only(State.t(), Cursor.t(), term(), non_neg_integer()) :: {State.t(), Cursor.t(), non_neg_integer()}
  def skip_newlines_only(%State{} = state, cursor, ctx \\ nil, count \\ 0) when count >= 0 do
    case peek_sep(state, cursor, ctx) do
      {:ok, {:";", _meta}, _state, _cursor} ->
        # Stop at semicolon - don't consume it
        {state, cursor, count}

      {:ok, {:eol, {_, _, n}}, state, cursor} when is_integer(n) ->
        {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)
        skip_newlines_only(state, cursor, ctx, count + n)

      {:ok, {:eol, _meta}, state, cursor} ->
        {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)
        skip_newlines_only(state, cursor, ctx, count)

      :none ->
        {state, cursor, count}
    end
  end
end
