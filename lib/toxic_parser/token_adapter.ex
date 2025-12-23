defmodule ToxicParser.TokenAdapter do
  @moduledoc """
  Normalizes Toxic streaming tokens into parser-friendly shapes with
  peek/checkpoint/rewind helpers.
  """

  alias ToxicParser.{Error, EventLog, Position, State}

  @type token_kind :: atom()

  @type token :: %{
          kind: token_kind(),
          value: term() | nil,
          metadata: EventLog.metadata(),
          raw: Toxic.token()
        }

  @type result ::
          {:ok, token(), State.t()}
          | {:eof, State.t()}
          | {:error, Error.t(), State.t()}

  @doc """
  Initializes parser state and stream.
  """
  @spec new(binary() | charlist(), keyword()) :: State.t()
  def new(source, opts \\ []), do: State.new(source, opts)

  @doc """
  Returns the current terminator snapshot without consuming tokens.
  """
  @spec current_terminators(State.t()) :: {[term()], State.t()}
  def current_terminators(%State{stream: stream} = state) do
    {terms, stream} = Toxic.current_terminators(stream)
    {terms, %{state | stream: stream, terminators: terms}}
  end

  @doc """
  Consume the next token (with EOE normalization).
  """
  @spec next(State.t()) :: result()
  def next(%State{} = state) do
    with {:ok, state} <- consume_fuel(state) do
      case pop_lookahead(state) do
        {:ok, token, state} ->
          {:ok, token, %{state | terminators: token.metadata.terminators}}

        :empty ->
          case fetch_next(state, cache?: false) do
            {:ok, token, state} ->
              {:ok, token, %{state | terminators: token.metadata.terminators}}

            other ->
              other
          end
      end
    end
  end

  @doc """
  Peek the next token without consuming it.
  """
  @spec peek(State.t()) :: result()
  def peek(%State{} = state) do
    state = normalize_lookahead(state)

    case state.lookahead do
      [next | _] -> {:ok, next, state}
      _ -> fetch_next(state, cache?: true)
    end
  end

  @doc """
  Push a previously consumed token back into the lookahead buffer.
  """
  @spec pushback(State.t(), token()) :: State.t()
  def pushback(%State{} = state, token) when is_map(token) do
    %{state | lookahead: [token | state.lookahead]}
  end

  @doc """
  Push multiple previously consumed tokens back into the lookahead buffer.

  Tokens must be in source order (the first token in the list will be the next token returned).
  """
  @spec pushback_many(State.t(), [token()]) :: State.t()
  def pushback_many(%State{} = state, tokens) when is_list(tokens) do
    front = Enum.reduce(Enum.reverse(tokens), state.lookahead, fn token, acc -> [token | acc] end)
    %{state | lookahead: front}
  end

  @doc """
  Peek at the next N tokens (bounded by `state.max_peek`).
  """
  @spec peek_n(State.t(), pos_integer()) ::
          {:ok, [token()], State.t()}
          | {:eof, [token()], State.t()}
          | {:error, Error.t(), [token()], State.t()}
  def peek_n(%State{max_peek: max} = _state, n) when n > max do
    raise ArgumentError, "peek_n/2 capped at #{max}"
  end

  def peek_n(%State{} = state, n) do
    case ensure_lookahead(state, n) do
      {:ok, state} -> {:ok, take_lookahead(state, n), state}
      {:eof, state} -> {:eof, [], state}
      {:error, err, state} -> {:error, err, [], state}
    end
  end

  defp ensure_lookahead(state, n) do
    state = normalize_lookahead(state)

    cond do
      lookahead_size(state) >= n ->
        {:ok, state}

      true ->
        case fetch_next(state, cache?: true) do
          {:ok, _token, state} -> ensure_lookahead(state, n)
          {:eof, state} -> {:eof, state}
          {:error, err, state} -> {:error, err, state}
        end
    end
  end

  defp normalize_lookahead(%State{lookahead: [], lookahead_back: back} = state) when back != [] do
    %{state | lookahead: Enum.reverse(back), lookahead_back: []}
  end

  defp normalize_lookahead(state), do: state

  defp pop_lookahead(%State{} = state) do
    state = normalize_lookahead(state)

    case state.lookahead do
      [next | rest] -> {:ok, next, %{state | lookahead: rest}}
      [] -> :empty
    end
  end

  defp lookahead_size(%State{lookahead: front, lookahead_back: back}),
    do: length(front) + length(back)

  defp take_lookahead(%State{lookahead: front, lookahead_back: back}, n) do
    front
    |> Stream.concat(back |> Enum.reverse())
    |> Enum.take(n)
  end

  @doc """
  Create a checkpoint for backtracking.
  """
  @spec checkpoint(State.t()) :: {reference(), State.t()}
  def checkpoint(%State{} = state) do
    {ref, stream} = Toxic.checkpoint(state.stream)

    saved = %{
      ref: ref,
      lookahead: state.lookahead,
      lookahead_back: state.lookahead_back,
      diagnostics: state.diagnostics,
      terminators: state.terminators,
      event_log: state.event_log
    }

    {ref, %{state | stream: stream, checkpoints: Map.put(state.checkpoints, ref, saved)}}
  end

  @doc """
  Rewind to a previously created checkpoint.
  """
  @spec rewind(State.t(), reference()) :: State.t()
  def rewind(%State{} = state, ref) do
    checkpoint = Map.fetch!(state.checkpoints, ref)

    stream = Toxic.rewind_to(state.stream, ref)

    %{
      state
      | stream: stream,
        lookahead: checkpoint.lookahead,
        lookahead_back: checkpoint.lookahead_back,
        diagnostics: checkpoint.diagnostics,
        terminators: checkpoint.terminators,
        event_log: checkpoint.event_log,
        checkpoints: Map.delete(state.checkpoints, ref)
    }
  end

  @doc """
  Drop a checkpoint without rewinding.
  """
  @spec drop_checkpoint(State.t(), reference()) :: State.t()
  def drop_checkpoint(%State{} = state, ref) do
    {_checkpoint, checkpoints} = Map.pop!(state.checkpoints, ref)
    :ok = Toxic.drop_checkpoint(ref)
    %{state | checkpoints: checkpoints}
  end

  defp fetch_next(%State{stream: stream} = state, opts) do
    needs_terms? = needs_terminators?(state, opts)

    case Toxic.next(stream) do
      {:ok, raw, stream} ->
        {terminators, stream} = maybe_fetch_terminators(stream, state, needs_terms?)
        {normalized, diagnostics} = normalize_token(raw, state, terminators)
        state = update_state(state, stream, terminators, diagnostics, opts, normalized)
        {:ok, normalized, state}

      {:eof, stream} ->
        {terminators, stream} = maybe_fetch_terminators(stream, state, needs_terms?)
        {:eof, %{state | stream: stream, terminators: terminators}}

      {:error, reason, stream} ->
        {terminators, stream} = refresh_terminators(stream, state)

        diagnostic =
          Error.from_toxic(
            nil,
            reason,
            line_index: state.line_index,
            terminators: terminators
          )

        if state.mode == :tolerant do
          {token, state} = synthetic_error_token(stream, state, terminators, diagnostic, opts)
          {:ok, token, state}
        else
          {:error, diagnostic,
           %{
             state
             | stream: stream,
               terminators: terminators,
               diagnostics: [diagnostic | state.diagnostics]
           }}
        end
    end
  end

  defp maybe_fetch_terminators(stream, _state, true), do: Toxic.current_terminators(stream)
  defp maybe_fetch_terminators(stream, state, false), do: {state.terminators, stream}

  defp refresh_terminators(stream, _state), do: Toxic.current_terminators(stream)

  defp needs_terminators?(state, opts) do
    Keyword.get(opts, :force_terminators?, false) ||
      Keyword.get(opts, :fetch_terminators?, false) ||
      state.mode == :tolerant ||
      Keyword.get(state.opts, :token_metadata, false) ||
      Keyword.get(state.opts, :emit_events, false)
  end

  defp consume_fuel(%State{fuel: :infinity} = state), do: {:ok, state}

  defp consume_fuel(%State{fuel: fuel} = state) when is_integer(fuel) do
    if fuel > 0 do
      {:ok, %{state | fuel: fuel - 1}}
    else
      {:error, :out_of_fuel, state}
    end
  end

  defp update_state(state, stream, terminators, diagnostics, opts, normalized) do
    state =
      state
      |> maybe_cache(opts[:cache?], normalized)
      |> Map.put(:stream, stream)
      |> Map.put(:terminators, terminators)
      |> Map.update!(:diagnostics, &(Enum.reverse(diagnostics) ++ &1))

    state
  end

  defp maybe_cache(state, true, token),
    do: %{state | lookahead_back: [token | state.lookahead_back]}

  defp maybe_cache(state, _cache?, _token), do: state

  defp normalize_token({:eol, meta} = raw, state, terminators) do
    token = %{
      kind: :eoe,
      value: %{source: :eol, newlines: newline_count(raw)},
      metadata: metadata(meta, raw, terminators, state),
      raw: raw
    }

    {token, []}
  end

  defp normalize_token({:";", meta} = raw, state, terminators) do
    token = %{
      kind: :eoe,
      value: %{source: :semicolon, newlines: newline_count(raw)},
      metadata: metadata(meta, raw, terminators, state),
      raw: raw
    }

    {token, []}
  end

  defp normalize_token({:error_token, meta, %Toxic.Error{} = err} = raw, state, terminators) do
    diagnostic =
      Error.from_toxic(
        meta,
        err,
        line_index: state.line_index,
        terminators: terminators
      )

    token = %{
      kind: :error_token,
      value: err,
      metadata: metadata(meta, raw, terminators, state),
      raw: raw
    }

    {token, [diagnostic]}
  end

  defp normalize_token({:., meta} = raw, state, terminators) do
    token = %{
      kind: :dot_op,
      value: :.,
      metadata: metadata(meta, raw, terminators, state),
      raw: raw
    }

    {token, []}
  end

  defp normalize_token({kind, meta, v1, v2} = raw, state, terminators) do
    token = %{
      kind: kind,
      value: {v1, v2},
      metadata: metadata(meta, raw, terminators, state),
      raw: raw
    }

    {token, []}
  end

  defp normalize_token({kind, meta, value} = raw, state, terminators) do
    token = %{
      kind: kind,
      value: value,
      metadata: metadata(meta, raw, terminators, state),
      raw: raw
    }

    {token, []}
  end

  defp normalize_token({kind, meta} = raw, state, terminators) do
    token = %{
      kind: kind,
      value: nil,
      metadata: metadata(meta, raw, terminators, state),
      raw: raw
    }

    {token, []}
  end

  defp metadata({{sl, sc}, {el, ec}, _extra} = meta, raw, terminators, state) do
    %{
      range: %{
        start: Position.to_location(sl, sc, state.line_index),
        end: Position.to_location(el, ec, state.line_index)
      },
      delimiter: delimiter(raw),
      newlines: newline_count(raw),
      synthesized?: synthesized?(meta),
      terminators: terminators,
      role: delimiter_role(raw)
    }
  end

  defp synthesized?({{sl, sc}, {el, ec}, _extra}), do: sl == el and sc == ec

  defp delimiter({token, _meta}) when token in [:"(", :")"], do: {:paren, {"(", ")"}}
  defp delimiter({token, _meta}) when token in [:"[", :"]"], do: {:bracket, {"[", "]"}}
  defp delimiter({token, _meta}) when token in [:"{", :"}"], do: {:curly, {"{", "}"}}
  defp delimiter({token, _meta}) when token in [:"<<", :">>"], do: {:bitstring, {"<<", ">>"}}
  defp delimiter(_), do: nil

  defp delimiter_role({token, _meta}) when token in [:"(", :"[", :"{", :"<<"], do: :open
  defp delimiter_role({token, _meta}) when token in [:")", :"]", :"}", :">>"], do: :close
  defp delimiter_role(_), do: :none

  # Extract newline count from various token formats
  # For EOE tokens: {:eol, {pos, pos, count}}
  # For operators with 3-element tuple: {:arrow_op, {pos, pos, count}, value}
  defp newline_count({:eol, {_, _, count}}) when is_integer(count), do: count
  defp newline_count({:";", {_, _, count}}) when is_integer(count), do: count
  defp newline_count({:",", {_, _, count}}) when is_integer(count), do: count

  # Literals encode parsed values in the 3rd element, not newline counts.
  defp newline_count({:int, _meta, _value}), do: 0
  defp newline_count({:flt, _meta, _value}), do: 0
  defp newline_count({:char, _meta, _value}), do: 0

  # 3-tuple tokens like operators: {kind, {start, end, newlines}, value}
  defp newline_count({_kind, {_, _, count}, _value}) when is_integer(count), do: count
  # 4-tuple tokens like in_op: {kind, {start, end, newlines}, v1, v2}
  defp newline_count({_kind, {_, _, count}, _v1, _v2}) when is_integer(count), do: count
  defp newline_count(_), do: 0

  defp synthetic_error_token(stream, state, terminators, diagnostic, opts) do
    {{line, col}, stream} = Toxic.position(stream)

    meta = {
      {line, col},
      {line, col},
      nil
    }

    token = %{
      kind: :error_token,
      value: diagnostic.reason,
      metadata: metadata(meta, {:error_token, meta}, terminators, state),
      raw: {:error_token, meta, diagnostic.reason}
    }

    state =
      state
      |> Map.put(:stream, %{stream | error: nil})
      |> maybe_cache(opts[:cache?], token)
      |> Map.update!(:diagnostics, &[diagnostic | &1])
      |> Map.put(:terminators, terminators)

    {token, state}
  end
end
