defmodule ToxicParser.Builder.Meta do
  @moduledoc false

  alias ToxicParser.{State, TokenAdapter}
  alias ToxicParser.Grammar.EOE

  @spec consume_closing(State.t(), ToxicParser.Cursor.t(), atom()) ::
          {:ok, keyword(), non_neg_integer(), State.t(), ToxicParser.Cursor.t()}
          | {:error, term(), State.t(), ToxicParser.Cursor.t()}
  def consume_closing(%State{} = state, cursor, expected_kind) when is_atom(expected_kind) do
    {state, cursor, trailing_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    case TokenAdapter.next(state, cursor) do
      {:ok, {kind, _meta, _value} = tok, state, cursor} ->
        if kind == expected_kind do
          {:ok, TokenAdapter.token_meta(tok), trailing_newlines, state, cursor}
        else
          {:error, {:expected, expected_kind, got: kind}, state, cursor}
        end

      {:eof, state, cursor} ->
        {:error, :unexpected_eof, state, cursor}

      {:error, diag, state, cursor} ->
        {:error, diag, state, cursor}
    end
  end

  @spec closing_meta(keyword(), keyword(), non_neg_integer(), keyword(), keyword()) :: keyword()
  def closing_meta(base_meta, close_meta, newlines \\ 0, prefix \\ [], opts \\ []) do
    base_first? = Keyword.get(opts, :base_first, false)
    newlines_kv = newlines_kv(newlines)

    if base_first? do
      prefix ++ base_meta ++ newlines_kv ++ [closing: close_meta]
    else
      prefix ++ newlines_kv ++ [closing: close_meta] ++ base_meta
    end
  end

  @doc "Builds a `[newlines: n]` keyword when n > 0, otherwise []."
  @spec newlines_meta(non_neg_integer()) :: keyword()
  def newlines_meta(n) when is_integer(n) and n >= 0, do: newlines_kv(n)

  @spec total_newlines(non_neg_integer(), non_neg_integer(), boolean()) :: non_neg_integer()
  def total_newlines(leading, trailing, include_trailing?) do
    leading + if(include_trailing?, do: trailing, else: 0)
  end

  defp newlines_kv(0), do: []
  defp newlines_kv(n), do: [newlines: n]
end
