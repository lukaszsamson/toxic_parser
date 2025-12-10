defmodule ToxicParser.Error do
  @moduledoc """
  Diagnostic payload shared between parser results and event log `:error` events.
  """

  alias ToxicParser.EventLog

  @type phase :: :lexer | :parser
  @type severity :: :error | :warning

  @type t :: %__MODULE__{
          phase: phase(),
          reason: term(),
          range: EventLog.range(),
          severity: severity(),
          token: atom() | nil,
          expected: [atom()] | nil,
          details: map()
        }

  @enforce_keys [:phase, :reason, :range, :severity]
  defstruct [:phase, :reason, :range, :severity, :token, :expected, details: %{}]

  @doc """
  Builds a parser-phase diagnostic from `Code.string_to_quoted*/2` error tuples.
  """
  @spec from_elixir({term(), term(), term()}, keyword()) :: t()
  def from_elixir({line, reason, token}, opts \\ []) do
    %__MODULE__{
      phase: :parser,
      reason: {reason, token},
      token: normalize_token(token),
      expected: Keyword.get(opts, :expected),
      severity: Keyword.get(opts, :severity, :error),
      range: line_only_range(line),
      details: %{source: :elixir_parser}
    }
  end

  defp line_only_range(line) do
    location = %{offset: 0, line: line, column: 0}
    %{start: location, end: location}
  end

  defp normalize_token(atom) when is_atom(atom), do: atom
  defp normalize_token(_), do: nil
end
