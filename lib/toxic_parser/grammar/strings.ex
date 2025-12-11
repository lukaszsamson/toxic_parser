defmodule ToxicParser.Grammar.Strings do
  @moduledoc """
  Parsing for strings, charlists, heredocs, and sigils from Toxic token streams.
  """

  alias ToxicParser.{Builder, EventLog, Pratt, State, TokenAdapter}

  @string_start [:bin_string_start, :list_string_start, :bin_heredoc_start, :list_heredoc_start]
  @sigil_start [:sigil_start]

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}
          | {:no_string, State.t()}

  @spec parse(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse(%State{} = state, _ctx, %EventLog{} = log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: kind}, _} when kind in @string_start ->
        parse_string(state, log)

      {:ok, %{kind: kind}, _} when kind in @sigil_start ->
        parse_sigil(state, log)

      {:ok, _tok, _} ->
        {:no_string, state}

      {:eof, state} ->
        {:no_string, state}

      {:error, _diag, state} ->
        {:no_string, state}
    end
  end

  defp parse_string(state, log) do
    {:ok, start_tok, state} = TokenAdapter.next(state)
    target_end = closing_for(start_tok.kind)
    type = string_type(start_tok.kind)

    with {:ok, fragments, state, log} <- collect_fragments([], state, target_end, log),
         {:ok, _close, state} <- TokenAdapter.next(state) do
      value = build_string_value(Enum.reverse(fragments), type)
      {:ok, Builder.Helpers.literal(value), state, log}
    end
  end

  defp parse_sigil(state, log) do
    {:ok, start_tok, state} = TokenAdapter.next(state)
    {sigil, _delim} = start_tok.value

    with {:ok, fragments, state, log} <- collect_fragments([], state, :sigil_end, log),
         {:ok, end_tok, state} <- TokenAdapter.next(state) do
      {_close, mods} = end_tok.value
      content = build_string_value(Enum.reverse(fragments), :binary)
      ast = {sigil, [], [content, mods || ""]}
      {:ok, ast, state, log}
    end
  end

  defp collect_fragments(acc, state, target_end, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: ^target_end}, _} ->
        {:ok, acc, state, log}

      {:ok, %{kind: :string_fragment, value: fragment}, _} ->
        {:ok, _frag, state} = TokenAdapter.next(state)
        collect_fragments([fragment | acc], state, target_end, log)

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}

      {:ok, token, state} ->
        # Unknown token inside string; treat as error
        {:error, {:unexpected_string_token, token.kind}, state, log}
    end
  end

  defp build_string_value(fragments, :binary), do: Enum.join(fragments, "")
  defp build_string_value(fragments, :charlist), do: fragments |> Enum.join("") |> String.to_charlist()
  defp build_string_value(fragments, :heredoc_binary), do: Enum.join(fragments, "")
  defp build_string_value(fragments, :heredoc_charlist), do: fragments |> Enum.join("") |> String.to_charlist()

  defp string_type(:bin_string_start), do: :binary
  defp string_type(:list_string_start), do: :charlist
  defp string_type(:bin_heredoc_start), do: :heredoc_binary
  defp string_type(:list_heredoc_start), do: :heredoc_charlist

  defp closing_for(:bin_string_start), do: :bin_string_end
  defp closing_for(:list_string_start), do: :list_string_end
  defp closing_for(:bin_heredoc_start), do: :bin_heredoc_end
  defp closing_for(:list_heredoc_start), do: :list_heredoc_end
end
