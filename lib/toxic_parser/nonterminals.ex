defmodule ToxicParser.Nonterminals do
  @moduledoc """
  Canonical mapping of grammar nonterminals to parsing strategy.
  """

  @external_resource Path.expand("../../NONTERMINALS_GPT.md", __DIR__)

  @type parser_style :: :recursive_descent | :pratt
  @type entry :: %{name: String.t(), parser: parser_style()}

  @nonterminals Path.expand("../../NONTERMINALS_GPT.md", __DIR__)
                |> File.read!()
                |> String.split("\n", trim: true)
                |> Enum.reject(&(&1 == "" or String.starts_with?(&1, "#")))
                |> Enum.map(&String.split(&1, ":"))
                |> Enum.map(fn
                  [name, style] ->
                    %{
                      name: String.trim(name),
                      parser:
                        case String.trim(style) do
                          "recursive descend" -> :recursive_descent
                          "pratt" -> :pratt
                          other -> raise ArgumentError, "Unknown parser style: #{inspect(other)}"
                        end
                    }

                  unexpected ->
                    raise ArgumentError, "Invalid nonterminal line: #{inspect(unexpected)}"
                end)

  @doc "Returns the full list of nonterminal entries."
  @spec list() :: [entry()]
  def list, do: @nonterminals

  @doc "Returns the parser style for a given nonterminal name."
  @spec fetch(String.t()) :: {:ok, parser_style()} | :error
  def fetch(name) when is_binary(name) do
    case Enum.find(@nonterminals, fn %{name: entry} -> entry == name end) do
      %{parser: parser} -> {:ok, parser}
      _ -> :error
    end
  end
end
