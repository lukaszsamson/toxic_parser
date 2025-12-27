defmodule ToxicParser.Grammar.EOE do
  @moduledoc """
  Helpers for working with separator tokens (`:eol` and `:";"`):
  skipping, counting newlines, and building/annotating metadata.
  """

  alias ToxicParser.{Layout, State, TokenAdapter}
  alias ToxicParser.Builder.Helpers

  @spec skip(State.t(), ToxicParser.Cursor.t()) :: {State.t(), ToxicParser.Cursor.t()}
  def skip(%State{} = state, cursor) do
    case Layout.peek_sep(state, cursor) do
      {:ok, _sep, state, cursor} ->
        {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)
        skip(state, cursor)

      :none ->
        {state, cursor}
    end
  end

  @spec skip_count_newlines(State.t(), ToxicParser.Cursor.t(), non_neg_integer()) ::
          {State.t(), ToxicParser.Cursor.t(), non_neg_integer()}
  def skip_count_newlines(%State{} = state, cursor, count)
      when is_integer(count) and count >= 0 do
    case Layout.peek_sep(state, cursor) do
      {:ok, {_kind, {_, _, n}, _value}, state, cursor} when is_integer(n) ->
        {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)
        skip_count_newlines(state, cursor, count + n)

      {:ok, _sep, state, cursor} ->
        {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)
        skip_count_newlines(state, cursor, count)

      :none ->
        {state, cursor, count}
    end
  end

  @doc """
  Skip ONLY newline tokens (`:eol`), not semicolons.
  Returns the state, cursor, and total newline count.
  This implements Elixir's `stab_op_eol` which only allows newlines after `->`.
  """
  @spec skip_newlines_only(State.t(), ToxicParser.Cursor.t(), non_neg_integer()) ::
          {State.t(), ToxicParser.Cursor.t(), non_neg_integer()}
  def skip_newlines_only(%State{} = state, cursor, count)
      when is_integer(count) and count >= 0 do
    # Delegate to Layout which handles the :eol vs :";" distinction
    Layout.skip_newlines_only(state, cursor, nil, count)
  end

  @doc """
  Build metadata for a separator token.
  Separator tokens are 3-tuples: {:eol, meta, value} or {:";", meta, value}
  where meta = {{start_line, start_col}, {end_line, end_col}, newline_count}
  """
  @spec build_sep_meta(tuple()) :: keyword()
  def build_sep_meta({kind, {_, _, newlines}, _value} = token)
      when kind in [:eol, :";"] and is_integer(newlines) do
    [newlines: newlines] ++ Helpers.token_meta(token)
  end

  def build_sep_meta({kind, _meta, _value} = token) when kind in [:eol, :";"] do
    Helpers.token_meta(token)
  end

  @spec skip_with_meta(State.t(), ToxicParser.Cursor.t(), keyword() | nil) ::
          {State.t(), ToxicParser.Cursor.t(), keyword()}
  def skip_with_meta(%State{} = state, cursor, first_meta \\ nil) do
    case Layout.peek_sep(state, cursor) do
      {:ok, sep_tok, state, cursor} ->
        {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)
        new_meta = first_meta || Helpers.token_meta(sep_tok)
        skip_with_meta(state, cursor, new_meta)

      :none ->
        {state, cursor, first_meta || []}
    end
  end

  @spec annotate_eoe(Macro.t(), keyword()) :: Macro.t()
  def annotate_eoe({:->, stab_meta, [stab_args, {left, meta, right}]}, eoe_meta)
      when is_list(meta) do
    raise "dead code"
    {:->, stab_meta, [stab_args, {left, [{:end_of_expression, eoe_meta} | meta], right}]}
  end

  def annotate_eoe({left, meta, right}, eoe_meta) when is_list(meta) and left != :-> do
    {left, [{:end_of_expression, eoe_meta} | meta], right}
  end

  def annotate_eoe(ast, _eoe_meta), do: ast
end
