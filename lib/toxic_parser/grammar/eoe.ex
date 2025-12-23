defmodule ToxicParser.Grammar.EOE do
  @moduledoc """
  Helpers for working with end-of-expression (EOE) tokens:
  skipping, counting newlines, and building/annotating metadata.
  """

  alias ToxicParser.{Layout, State, TokenAdapter}
  alias ToxicParser.Builder.Helpers

  @spec skip(State.t()) :: State.t()
  def skip(%State{} = state) do
    case Layout.peek_sep(state) do
      {:ok, _eoe, state} ->
        {:ok, _tok, state} = TokenAdapter.next(state)
        skip(state)

      :none ->
        state
    end
  end

  @spec skip_count_newlines(State.t(), non_neg_integer()) :: {State.t(), non_neg_integer()}
  def skip_count_newlines(%State{} = state, count) when is_integer(count) and count >= 0 do
    case Layout.peek_sep(state) do
      {:ok, {:eoe, _meta, %{newlines: n}}, state} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_count_newlines(state, count + n)

      :none ->
        {state, count}
    end
  end

  @doc """
  Skip ONLY newline EOE tokens (not semicolons).
  Returns the state and total newline count.
  This implements Elixir's `stab_op_eol` which only allows newlines after `->`.
  """
  @spec skip_newlines_only(State.t(), non_neg_integer()) :: {State.t(), non_neg_integer()}
  def skip_newlines_only(%State{} = state, count) when is_integer(count) and count >= 0 do
    case Layout.peek_sep(state) do
      {:ok, {:eoe, _meta, %{source: :semicolon}}, _state} ->
        {state, count}

      {:ok, {:eoe, _meta, %{newlines: n}}, state} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_newlines_only(state, count + n)

      :none ->
        {state, count}
    end
  end

  @doc """
  Build metadata for an EOE token.
  EOE tokens are 3-tuples: {:eoe, meta, %{source: :eol | :semicolon, newlines: count}}
  """
  @spec build_eoe_meta(tuple()) :: keyword()
  def build_eoe_meta({:eoe, _meta, %{newlines: newlines}} = token) when is_integer(newlines) do
    [newlines: newlines] ++ Helpers.token_meta(token)
  end

  def build_eoe_meta({:eoe, _meta, _value} = token) do
    Helpers.token_meta(token)
  end

  @spec skip_with_meta(State.t(), keyword() | nil) :: {State.t(), keyword()}
  def skip_with_meta(%State{} = state, first_meta \\ nil) do
    case Layout.peek_sep(state) do
      {:ok, {:eoe, _meta, _value} = eoe_tok, state} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        new_meta = first_meta || Helpers.token_meta(eoe_tok)
        skip_with_meta(state, new_meta)

      :none ->
        {state, first_meta || []}
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
