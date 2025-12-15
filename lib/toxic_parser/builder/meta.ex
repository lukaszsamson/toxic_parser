defmodule ToxicParser.Builder.Meta do
  @moduledoc false

  alias ToxicParser.{State, TokenAdapter}
  alias ToxicParser.Builder.Helpers
  alias ToxicParser.Grammar.EOE

  @spec consume_closing(State.t(), atom()) ::
          {:ok, keyword(), non_neg_integer(), State.t()}
          | {:error, term(), State.t()}
  def consume_closing(%State{} = state, expected_kind) when is_atom(expected_kind) do
    {state, trailing_newlines} = EOE.skip_count_newlines(state, 0)

    case TokenAdapter.next(state) do
      {:ok, %{kind: ^expected_kind} = tok, state} ->
        {:ok, Helpers.token_meta(tok.metadata), trailing_newlines, state}

      {:ok, tok, state} ->
        {:error, {:expected, expected_kind, got: tok.kind}, state}

      {:eof, state} ->
        {:error, :unexpected_eof, state}

      {:error, diag, state} ->
        {:error, diag, state}
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
