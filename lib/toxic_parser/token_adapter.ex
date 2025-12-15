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
  def next(%State{lookahead: [next | rest]} = state) do
    case consume_fuel(state) do
      {:ok, state} ->
        {:ok, next, %{state | lookahead: rest, terminators: next.metadata.terminators}}

      {:error, reason, state} ->
        {:error, reason, state}
    end
  end

  def next(%State{} = state) do
    with {:ok, state} <- consume_fuel(state) do
      fetch_next(state, cache?: false)
    end
  end

  @doc """
  Peek the next token without consuming it.
  """
  @spec peek(State.t()) :: result()
  def peek(%State{lookahead: [next | _]} = state), do: {:ok, next, state}
  def peek(%State{} = state), do: fetch_next(state, cache?: true)

  @doc """
  Push a previously consumed token back into the lookahead buffer.
  """
  @spec pushback(State.t(), token()) :: State.t()
  def pushback(%State{} = state, token) when is_map(token) do
    %{state | lookahead: [token | state.lookahead]}
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
      {:ok, state} -> {:ok, Enum.take(state.lookahead, n), state}
      {:eof, state} -> {:eof, [], state}
      {:error, err, state} -> {:error, err, [], state}
    end
  end

  defp ensure_lookahead(state, n) when length(state.lookahead) >= n, do: {:ok, state}

  defp ensure_lookahead(state, n) do
    case fetch_next(state, cache?: true) do
      {:ok, _token, state} -> ensure_lookahead(state, n)
      {:eof, state} -> {:eof, state}
      {:error, err, state} -> {:error, err, state}
    end
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
      diagnostics: state.diagnostics,
      terminators: state.terminators
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
        diagnostics: checkpoint.diagnostics,
        terminators: checkpoint.terminators,
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
    {terminators, stream} = Toxic.current_terminators(stream)

    case Toxic.next(stream) do
      {:ok, raw, stream} ->
        {normalized, diagnostics} = normalize_token(raw, state, terminators)
        state = update_state(state, stream, terminators, diagnostics, opts, normalized)
        {:ok, normalized, state}

      {:eof, stream} ->
        {:eof, %{state | stream: stream, terminators: terminators}}

      {:error, reason, stream} ->
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
           %{state | stream: stream, diagnostics: [diagnostic | state.diagnostics]}}
        end
    end
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

  defp maybe_cache(state, true, token), do: %{state | lookahead: state.lookahead ++ [token]}
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

  defp normalize_token({:dot_call_op, meta} = raw, state, terminators) do
    token = %{
      kind: :dot_call_op,
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
  defp delimiter({token, _meta, _}) when token in [:"(", :")"], do: {:paren, {"(", ")"}}
  defp delimiter({token, _meta, _}) when token in [:"[", :"]"], do: {:bracket, {"[", "]"}}
  defp delimiter({token, _meta, _}) when token in [:"{", :"}"], do: {:curly, {"{", "}"}}
  defp delimiter({token, _meta, _}) when token in [:"<<", :">>"], do: {:bitstring, {"<<", ">>"}}
  defp delimiter({token, _meta, _, _}) when token in [:"(", :")"], do: {:paren, {"(", ")"}}
  defp delimiter({token, _meta, _, _}) when token in [:"[", :"]"], do: {:bracket, {"[", "]"}}
  defp delimiter({token, _meta, _, _}) when token in [:"{", :"}"], do: {:curly, {"{", "}"}}

  defp delimiter({token, _meta, _, _}) when token in [:"<<", :">>"],
    do: {:bitstring, {"<<", ">>"}}

  defp delimiter({:sigil_start, _meta, {_, close}}), do: {:sigil, {nil, close}}
  defp delimiter({:sigil_end, _meta, {_, close}}), do: {:sigil, {nil, close}}
  defp delimiter(_), do: nil

  defp delimiter_role({token, _meta}) when token in [:"(", :"[", :"{", :"<<"], do: :open
  defp delimiter_role({token, _meta}) when token in [:")", :"]", :"}", :">>"], do: :close
  defp delimiter_role({token, _meta, _}) when token in [:"(", :"[", :"{", :"<<"], do: :open
  defp delimiter_role({token, _meta, _}) when token in [:")", :"]", :"}", :">>"], do: :close
  defp delimiter_role({token, _meta, _, _}) when token in [:"(", :"[", :"{", :"<<"], do: :open
  defp delimiter_role({token, _meta, _, _}) when token in [:")", :"]", :"}", :">>"], do: :close
  defp delimiter_role(_), do: :none

  # Extract newline count from various token formats
  # For EOE tokens: {:eol, {pos, pos, count}}
  # For operators with 3-element tuple: {:arrow_op, {pos, pos, count}, value}
  defp newline_count({:eol, {_, _, count}}) when is_integer(count), do: count
  defp newline_count({:";", {_, _, count}}) when is_integer(count), do: count
  defp newline_count({:",", {_, _, count}}) when is_integer(count), do: count
  # 3-tuple tokens like operators: {kind, {start, end, newlines}, value}
  defp newline_count({_kind, {_, _, count}, _value}) when is_integer(count), do: count
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
