defmodule ToxicParser.Layout do
  @moduledoc """
  Parser-owned layout view for newline/semicolon separators.

  Provides helpers to peek and skip separator tokens without relying on the lexer
  to emit `:eoe` directly.
  """

  alias ToxicParser.{State, TokenAdapter}

  @type layout_token :: {:eoe, map(), %{source: :eol | :semicolon, newlines: non_neg_integer()}}

  @doc """
  Peeks at the next separator (`:eoe`) without consuming it, returning a view token.
  """
  @spec peek_sep(State.t(), term()) :: {:ok, layout_token(), State.t()} | :none
  def peek_sep(%State{} = state, _ctx \\ nil) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, value: %{source: source, newlines: newlines}, metadata: meta}, state} ->
        {:ok, {:eoe, meta, %{source: source, newlines: newlines}}, state}

      _ ->
        :none
    end
  end

  @doc """
  Skips all separators, returning the updated state.
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
  Skips newline separators (not semicolons) and returns the updated state plus newline count.
  """
  @spec skip_newlines_only(State.t(), term(), non_neg_integer()) :: {State.t(), non_neg_integer()}
  def skip_newlines_only(%State{} = state, ctx \\ nil, count \\ 0) when count >= 0 do
    case peek_sep(state, ctx) do
      {:ok, {:eoe, _meta, %{source: :semicolon}}, _state} ->
        {state, count}

      {:ok, {:eoe, _meta, %{newlines: n}}, state} ->
        {:ok, _tok, state} = TokenAdapter.next(state)
        skip_newlines_only(state, ctx, count + n)

      :none ->
        {state, count}
    end
  end
end
