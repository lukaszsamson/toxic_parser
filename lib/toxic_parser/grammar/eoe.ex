defmodule ToxicParser.Grammar.EOE do
  @moduledoc """
  Helpers for working with end-of-expression (EOE) tokens:
  skipping, counting newlines, and building/annotating metadata.
  """

  alias ToxicParser.{State, TokenAdapter}
  alias ToxicParser.Builder.Helpers

  @spec skip(State.t()) :: State.t()
  def skip(%State{} = state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip(state)

      _ ->
        state
    end
  end

  @spec skip_count_newlines(State.t(), non_neg_integer()) :: {State.t(), non_neg_integer()}
  def skip_count_newlines(%State{} = state, count) when is_integer(count) and count >= 0 do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, value: %{newlines: n}}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_count_newlines(state, count + n)

      _ ->
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
    case TokenAdapter.peek(state) do
      # Only skip if it's an EOE from newlines, not semicolons
      {:ok, %{kind: :eoe, value: %{source: source, newlines: n}}, _} when source != :semicolon ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_newlines_only(state, count + n)

      _ ->
        {state, count}
    end
  end

  @spec build_eoe_meta(map()) :: keyword()
  def build_eoe_meta(%{kind: :eoe, value: %{newlines: newlines}, metadata: meta})
      when is_integer(newlines) do
    [newlines: newlines] ++ Helpers.token_meta(meta)
  end

  def build_eoe_meta(%{kind: :eoe, metadata: meta}) do
    Helpers.token_meta(meta)
  end

  @spec skip_with_meta(State.t(), keyword() | nil) :: {State.t(), keyword()}
  def skip_with_meta(%State{} = state, first_meta \\ nil) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, metadata: meta}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        new_meta = first_meta || Helpers.token_meta(meta)
        skip_with_meta(state, new_meta)

      _ ->
        {state, first_meta || []}
    end
  end

  @spec annotate_eoe(Macro.t(), keyword()) :: Macro.t()
  def annotate_eoe({:->, stab_meta, [stab_args, {left, meta, right}]}, eoe_meta)
      when is_list(meta) do
    {:->, stab_meta, [stab_args, {left, [{:end_of_expression, eoe_meta} | meta], right}]}
  end

  def annotate_eoe({left, meta, right}, eoe_meta) when is_list(meta) and left != :-> do
    {left, [{:end_of_expression, eoe_meta} | meta], right}
  end

  def annotate_eoe(ast, _eoe_meta), do: ast
end
