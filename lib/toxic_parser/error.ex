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
  defp normalize_token(other), do: other

  @doc """
  Builds a lexer-phase diagnostic from a Toxic error.
  """
  @spec from_toxic(term(), Toxic.Error.t() | term(), keyword()) :: t()
  def from_toxic(meta, reason, opts \\ [])

  def from_toxic(meta, %Toxic.Error{} = err, opts) do
    line_index = Keyword.get(opts, :line_index, [])
    range = meta_to_range(meta, err, line_index)

    reason = Toxic.Error.to_reason_tuple(err)

    reason =
      case reason do
        {meta_kv, message, token} ->
          {normalize_meta(meta_kv, opts), normalize_message(message), token}

        other ->
          other
      end

    %__MODULE__{
      phase: :lexer,
      reason: reason,
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

        {loc, {prefix, detail}, token} when is_list(prefix) ->
          {loc, {List.to_string(prefix), List.to_string(detail)}, token}

        {loc, msg, token} when is_list(msg) ->
          {loc, List.to_string(msg), token}

        {loc, msg, token} when is_list(token) and is_list(msg) ->
          {loc, List.to_string(msg), List.to_string(token)}

        {loc, msg, token} when is_list(token) and is_binary(msg) ->
          {loc, msg, List.to_string(token)}

        {loc, msg, token} when is_list(token) ->
          {loc, msg, IO.iodata_to_binary(token)}

        other ->
          other
      end

    reason =
      case reason do
        {meta, message, token} ->
          {normalize_meta(meta, opts), normalize_message(message), token}

        other ->
          normalize_message(other)
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

  defp normalize_meta(meta, opts) when is_list(meta) do
    if Keyword.get(opts, :source) == nil do
      meta
    else
      Keyword.drop(meta, [:end_line, :end_column])
    end
  end

  defp normalize_meta(meta, _opts), do: meta

  defp normalize_message({prefix, detail}) when is_list(prefix) or is_binary(prefix) do
    {to_string(prefix), to_string(detail)}
  end

  defp normalize_message({:unescape_error, message, meta}) when is_binary(message) do
    token = extract_unescape_token(message)
    {meta, "#{message}. Syntax error after: ", token}
  end

  defp normalize_message(message) when is_list(message), do: List.to_string(message)
  defp normalize_message(message), do: message

  defp extract_unescape_token(message) do
    cond do
      String.contains?(message, "\\x") -> "\\x"
      String.contains?(message, "\\u") -> "\\u"
      true -> ""
    end
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
