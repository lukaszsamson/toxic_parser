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

  @doc """
  Builds a lexer-phase diagnostic from a Toxic error.
  """
  @spec from_toxic(term(), Toxic.Error.t() | term(), keyword()) :: t()
  def from_toxic(meta, reason, opts \\ [])

  def from_toxic(meta, %Toxic.Error{} = err, opts) do
    line_index = Keyword.get(opts, :line_index, [])
    range = meta_to_range(meta, err, line_index)

    %__MODULE__{
      phase: :lexer,
      reason: {err.code, err.details},
      token: normalize_token(err.token_display),
      severity: err.severity,
      expected: Keyword.get(opts, :expected),
      range: range,
      details: %{domain: err.domain, terminators: Keyword.get(opts, :terminators)}
    }
  end

  def from_toxic(meta, reason, opts) do
    line_index = Keyword.get(opts, :line_index, [])
    range = meta_to_range(meta, %{position: Keyword.get(opts, :position)}, line_index)

    reason =
      case reason do
        {loc, msg, token} when is_list(msg) and is_list(token) ->
          {loc, List.to_string(msg), List.to_string(token)}

        other ->
          other
      end

    %__MODULE__{
      phase: :lexer,
      reason: reason,
      token: nil,
      severity: Keyword.get(opts, :severity, :error),
      expected: Keyword.get(opts, :expected),
      range: range,
      details: %{domain: :general, terminators: Keyword.get(opts, :terminators)}
    }
  end

  defp meta_to_range({{_sl, _sc}, {_el, _ec}, _extra} = meta, _err, line_index) do
    ToxicParser.Position.range_from_meta(meta, line_index)
  end

  defp meta_to_range(_meta, err, line_index) do
    case err.position do
      {{_sl, _sc}, {_el, _ec}} = pos -> ToxicParser.Position.range_from_meta(pos, line_index)
      _ -> line_only_range(1)
    end
  end
end
