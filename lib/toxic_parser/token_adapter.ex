defmodule ToxicParser.TokenAdapter do
  @moduledoc """
  Provides peek/checkpoint/rewind helpers for Toxic streaming tokens.

  Phase 3 refactor: Tokens are now stored as raw Toxic tuples, with helper
  functions for accessing kind/value/meta. EOE normalization is handled via
  a view token for backward compatibility during migration.
  """

  alias ToxicParser.{Error, Position, State}

  # Import token macros for pattern matching
  require Toxic.Token

  @doc """
  Guard-safe check if token has the given kind.
  Can be used in guard clauses.
  """
  defguard is_kind(token, expected_kind)
           when is_tuple(token) and elem(token, 0) == expected_kind

  # Raw token types from Toxic lexer:
  # - 2-tuple: {kind, meta} - punctuation, keywords
  # - 3-tuple: {kind, meta, value} - literals, operators, identifiers
  # - 4-tuple: {kind, meta, v1, v2} - special tokens like "not in"
  #
  # EOE view token (parser-owned, not from lexer):
  # - {:eoe, meta, %{source: :eol | :semicolon, newlines: count}}

  @type raw_token :: tuple()
  @type token :: raw_token()

  @type result ::
          {:ok, token(), State.t()}
          | {:eof, State.t()}
          | {:error, Error.t(), State.t()}

  # ============================================================================
  # Token field accessors - work with raw tokens
  # ============================================================================

  @doc "Extract kind from a raw token"
  @spec kind(token()) :: atom()
  def kind({kind, _meta}), do: kind
  def kind({kind, _meta, _value}), do: kind
  def kind({kind, _meta, _v1, _v2}), do: kind

  @doc "Extract value from a raw token. Returns nil for 2-tuple tokens."
  @spec value(token()) :: term()
  def value({_kind, _meta}), do: nil
  def value({_kind, _meta, value}), do: value
  def value({_kind, _meta, v1, _v2}), do: v1

  @doc "Extract second value from a 4-tuple token. Returns nil otherwise."
  @spec value2(token()) :: term()
  def value2({_kind, _meta, _v1, v2}), do: v2
  def value2(_), do: nil

  @doc "Extract meta from a raw token"
  @spec meta(token()) :: tuple()
  def meta({_kind, meta}), do: meta
  def meta({_kind, meta, _value}), do: meta
  def meta({_kind, meta, _v1, _v2}), do: meta

  @doc "Extract start line from token"
  @spec line(token()) :: pos_integer()
  def line(token), do: token |> meta() |> elem(0) |> elem(0)

  @doc "Extract start column from token"
  @spec column(token()) :: pos_integer()
  def column(token), do: token |> meta() |> elem(0) |> elem(1)

  @doc "Extract newline count from token (for :eol, :\";\", operators)"
  @spec newlines(token()) :: non_neg_integer()
  def newlines({kind, {_, _, count}}) when kind in [:eol, :";", :","] and is_integer(count),
    do: count

  # EOE view token has newlines in the value map
  def newlines({:eoe, _meta, %{newlines: n}}), do: n
  # Literals encode parsed values in extra, not newline counts
  def newlines({kind, _meta, _value}) when kind in [:int, :flt, :char], do: 0
  # 3-tuple tokens with newline count in extra
  def newlines({_kind, {_, _, count}, _value}) when is_integer(count), do: count
  # 4-tuple tokens with newline count in extra
  def newlines({_kind, {_, _, count}, _v1, _v2}) when is_integer(count), do: count
  def newlines(_), do: 0

  @doc """
  Build [line: L, column: C] metadata from a token.
  This is the common AST metadata format.
  """
  @spec token_meta(token()) :: keyword()
  def token_meta(token) do
    [line: line(token), column: column(token)]
  end

  @doc """
  Build full metadata map for diagnostics/events.
  Called lazily only when needed - not on the hot path.
  """
  @spec full_metadata(token(), State.t()) :: map()
  def full_metadata(token, %State{} = state) do
    {{sl, sc}, {el, ec}, _extra} = meta(token)

    %{
      range: %{
        start: Position.to_location(sl, sc, state.line_index),
        end: Position.to_location(el, ec, state.line_index)
      },
      delimiter: delimiter(token),
      newlines: newlines(token),
      synthesized?: synthesized?(meta(token)),
      terminators: state.terminators,
      role: delimiter_role(token)
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

  # ============================================================================
  # State management
  # ============================================================================

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

  # ============================================================================
  # Token consumption
  # ============================================================================

  @doc """
  Consume the next token.

  Returns raw tokens from the lexer. EOL (`:eol`) and semicolon (`:\";\"``)
  tokens are converted to `:eoe` view tokens for parser-level separation.
  """
  @spec next(State.t()) :: result()
  def next(%State{} = state) do
    with {:ok, state} <- consume_fuel(state) do
      case pop_lookahead(state) do
        {:ok, token, state} ->
          {:ok, token, state}

        :empty ->
          fetch_next(state, cache?: false)
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
  def pushback(%State{} = state, token) when is_tuple(token) do
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

  # ============================================================================
  # Checkpointing
  # ============================================================================

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

  # ============================================================================
  # Private helpers
  # ============================================================================

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

  defp consume_fuel(%State{fuel: :infinity} = state), do: {:ok, state}

  defp consume_fuel(%State{fuel: fuel} = state) when is_integer(fuel) do
    if fuel > 0 do
      {:ok, %{state | fuel: fuel - 1}}
    else
      {:error, :out_of_fuel, state}
    end
  end

  defp fetch_next(%State{stream: stream} = state, opts) do
    needs_terms? = needs_terminators?(state, opts)

    case Toxic.next(stream) do
      {:ok, raw, stream} ->
        {terminators, stream} = maybe_fetch_terminators(stream, state, needs_terms?)
        {normalized, diagnostics} = normalize_token(raw)
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

  # ============================================================================
  # Token normalization - minimal, only EOE view token conversion
  # ============================================================================

  # Convert :eol to :eoe view token (parser-owned separator)
  defp normalize_token({:eol, meta} = _raw) do
    {{_, _, count}, _} = {meta, nil}
    count = if is_integer(count), do: count, else: 0
    token = {:eoe, meta, %{source: :eol, newlines: count}}
    {token, []}
  end

  # Convert :; to :eoe view token
  defp normalize_token({:";", meta} = _raw) do
    {{_, _, count}, _} = {meta, nil}
    count = if is_integer(count), do: count, else: 0
    token = {:eoe, meta, %{source: :semicolon, newlines: count}}
    {token, []}
  end

  # Error tokens need diagnostic extraction
  defp normalize_token({:error_token, _meta, %Toxic.Error{} = err} = raw) do
    # Diagnostic will be created by fetch_next when state is available
    {raw, [err]}
  end

  # Dot token normalization: {:., meta} -> {:dot_op, meta, :.}
  # This makes it consistent with other operators for pattern matching
  defp normalize_token({:., meta}) do
    {{:dot_op, meta, :.}, []}
  end

  # All other tokens pass through unchanged
  defp normalize_token(raw) do
    {raw, []}
  end

  defp synthetic_error_token(stream, state, terminators, diagnostic, opts) do
    {{line, col}, stream} = Toxic.position(stream)

    meta = {
      {line, col},
      {line, col},
      nil
    }

    token = {:error_token, meta, diagnostic.reason}

    state =
      state
      |> Map.put(:stream, %{stream | error: nil})
      |> maybe_cache(opts[:cache?], token)
      |> Map.update!(:diagnostics, &[diagnostic | &1])
      |> Map.put(:terminators, terminators)

    {token, state}
  end
end
