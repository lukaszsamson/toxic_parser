defmodule ToxicParser.TokenAdapter do
  @moduledoc """
  Provides peek/checkpoint/rewind helpers for Toxic streaming tokens.

  Phase 4: Delegates to ToxicParser.Cursor for token transport.
  Cursor handles lookahead internally with a two-list queue, replacing
  both the Toxic stream batching and the previous TokenAdapter lookahead.

  Token accessors (kind/value/meta/etc.) and EOE normalization are preserved.
  """

  alias ToxicParser.{Cursor, Error, Position, State}

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

  # Only update terminators when we encounter tokens that change the terminator stack.
  # This is a critical performance optimization - terminators only change on delimiters.
  # Per LEXER_REFACTOR_V2.md Section 1.4: "TokenAdapter calls current_terminators/1 on every fetch (wasteful)"
  #
  # Terminator-affecting tokens:
  # - Brackets/blocks: (, ), [, ], {, }, <<, >>, do, end, fn
  # - String/heredoc boundaries: bin_string_start/end, list_string_start/end, bin_heredoc_start/end, list_heredoc_start/end
  # - Sigil boundaries: sigil_start, sigil_end
  # - Interpolation: begin_interpolation, end_interpolation
  # - Quoted atoms: atom_start
  # - Quoted identifiers: quoted_identifier_start, quoted_*_identifier_end variants
  @delimiter_tokens [
    # Brackets and blocks
    :"(",
    :")",
    :"[",
    :"]",
    :"{",
    :"}",
    :"<<",
    :">>",
    :do,
    :end,
    :fn,
    # String/heredoc boundaries
    :bin_string_start,
    :bin_string_end,
    :list_string_start,
    :list_string_end,
    :bin_heredoc_start,
    :bin_heredoc_end,
    :list_heredoc_start,
    :list_heredoc_end,
    # Sigil boundaries
    :sigil_start,
    :sigil_end,
    # Interpolation
    :begin_interpolation,
    :end_interpolation,
    # Quoted atoms
    :atom_start,
    # Quoted identifiers
    :quoted_identifier_start,
    :quoted_identifier_end,
    :quoted_paren_identifier_end,
    :quoted_bracket_identifier_end,
    :quoted_do_identifier_end,
    :quoted_op_identifier_end,
    # Quoted keyword identifiers (:"key":)
    :kw_identifier_safe_end,
    :kw_identifier_unsafe_end
  ]

  # ============================================================================
  # Token field accessors - work with raw tokens
  # ============================================================================

  @doc "Extract kind from a raw token"
  @spec kind(token()) :: atom()
  def kind(tuple), do: elem(tuple, 0)
  # def kind({kind, _meta}), do: kind
  # def kind({kind, _meta, _value}), do: kind
  # def kind({kind, _meta, _v1, _v2}), do: kind

  @compile {:inline, kind: 1}

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
  Pay-for-play: only called when needed, not on every token.
  """
  @spec current_terminators(State.t()) :: {[term()], State.t()}
  def current_terminators(%State{cursor: cursor} = state) do
    {terms, cursor} = Cursor.current_terminators(cursor)
    {terms, %{state | cursor: cursor, terminators: terms}}
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
      fetch_next(state)
    end
  end

  @doc """
  Peek the next token without consuming it.
  """
  @spec peek(State.t()) :: result()
  def peek(%State{cursor: cursor} = state) do
    case Cursor.peek(cursor) do
      {:ok, raw, cursor} ->
        state = %{state | cursor: cursor}
        {normalized, diagnostics} = normalize_token(raw)
        # state = if diagnostics != [], do: add_diagnostics(state, diagnostics), else: state
        # Per LEXER_REFACTOR_V3.md item 6: do NOT update terminators on peek.
        # Terminators only need to be computed on consumption, not lookahead.
        {:ok, normalized, state}

      {:eof, cursor} ->
        {:eof, %{state | cursor: cursor}}

      {:error, reason, cursor} ->
        handle_error(reason, cursor, state)
    end
  end

  @doc """
  Push a previously consumed token back into the lookahead buffer.
  """
  @spec pushback(State.t(), token()) :: State.t()
  def pushback(%State{cursor: cursor} = state, token) when is_tuple(token) do
    %{state | cursor: Cursor.pushback(cursor, token)}
  end

  @doc """
  Push multiple previously consumed tokens back into the lookahead buffer.

  Tokens must be in source order (the first token in the list will be the next token returned).
  """
  @spec pushback_many(State.t(), [token()]) :: State.t()
  def pushback_many(%State{cursor: cursor} = state, tokens) when is_list(tokens) do
    %{state | cursor: Cursor.pushback_many(cursor, tokens)}
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

  def peek_n(%State{cursor: cursor} = state, n) do
    case Cursor.peek_n(cursor, n) do
      {:ok, tokens, cursor} ->
        state = %{state | cursor: cursor}
        {normalized, state} = normalize_tokens(tokens, state)
        {:ok, normalized, state}

      {:eof, tokens, cursor} ->
        state = %{state | cursor: cursor}
        {normalized, state} = normalize_tokens(tokens, state)
        {:eof, normalized, state}

      {:error, reason, tokens, cursor} ->
        state = %{state | cursor: cursor}
        {normalized, state} = normalize_tokens(tokens, state)
        diagnostic = make_diagnostic(reason, state)
        {:error, diagnostic, normalized, %{state | diagnostics: [diagnostic | state.diagnostics]}}
    end
  end

  # ============================================================================
  # Checkpointing
  # ============================================================================

  @doc """
  Create a checkpoint for backtracking.
  Saves cursor state plus diagnostics/event_log for full rollback.
  """
  @spec checkpoint(State.t()) :: {reference(), State.t()}
  def checkpoint(%State{} = state) do
    ref = make_ref()

    saved = %{
      ref: ref,
      cursor: Cursor.mark(state.cursor),
      diagnostics: state.diagnostics,
      terminators: state.terminators,
      event_log: state.event_log
    }

    {ref, %{state | checkpoints: Map.put(state.checkpoints, ref, saved)}}
  end

  @doc """
  Rewind to a previously created checkpoint.
  """
  @spec rewind(State.t(), reference()) :: State.t()
  def rewind(%State{} = state, ref) do
    checkpoint = Map.fetch!(state.checkpoints, ref)

    %{
      state
      | cursor: Cursor.rewind(state.cursor, checkpoint.cursor),
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
    %{state | checkpoints: checkpoints}
  end

  # ============================================================================
  # Private helpers
  # ============================================================================

  defp consume_fuel(%State{fuel: :infinity} = state), do: {:ok, state}

  defp consume_fuel(%State{fuel: fuel} = state) when is_integer(fuel) do
    if fuel > 0 do
      {:ok, %{state | fuel: fuel - 1}}
    else
      {:error, :out_of_fuel, state}
    end
  end



  defp fetch_next(%State{cursor: cursor} = state) do
    case Cursor.next(cursor) do
      {:ok, raw, cursor} ->
        state = %{state | cursor: cursor}
        {normalized, diagnostics} = normalize_token(raw)
        state = if diagnostics != [], do: add_diagnostics(state, diagnostics), else: state
        # Per LEXER_REFACTOR_V3.md item 6: only update terminators when:
        # - tolerant mode recovery needs it, OR
        # - emit_events is enabled (for event metadata)
        # NOT for token_metadata - terminators are only needed for diagnostics/events.
        if elem(normalized, 0) in @delimiter_tokens and
             (state.mode == :tolerant or Keyword.get(state.opts, :emit_events, false)) do
          {:ok, normalized, update_terminators(state)}
        else
          {:ok, normalized, state}
        end

      {:eof, cursor} ->
        {:eof, %{state | cursor: cursor}}

      {:error, reason, cursor} ->
        handle_error(reason, cursor, state)
    end
  end

  defp handle_error(reason, cursor, state) do
    {terminators, cursor} = Cursor.current_terminators(cursor)
    state = %{state | cursor: cursor, terminators: terminators}

    diagnostic = make_diagnostic(reason, state)

    if state.mode == :tolerant do
      {token, state} = synthetic_error_token(state, diagnostic)
      {:ok, token, state}
    else
      {:error, diagnostic, %{state | diagnostics: [diagnostic | state.diagnostics]}}
    end
  end

  defp make_diagnostic(reason, state) do
    Error.from_toxic(
      nil,
      reason,
      line_index: state.line_index,
      terminators: state.terminators
    )
  end

  defp add_diagnostics(state, []), do: state

  defp add_diagnostics(state, diagnostics) do
    %{state | diagnostics: Enum.reverse(diagnostics) ++ state.diagnostics}
  end

  defp update_terminators(state) do
    {terms, cursor} = Cursor.current_terminators(state.cursor)
    %{state | cursor: cursor, terminators: terms}
  end

  defp normalize_tokens(tokens, state) do
    Enum.map_reduce(tokens, state, fn raw, state ->
      {normalized, diagnostics} = normalize_token(raw)
      {normalized, if(diagnostics != [], do: add_diagnostics(state, diagnostics), else: state)}
    end)
  end

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

  defp synthetic_error_token(state, diagnostic) do
    {{line, col}, cursor} = Cursor.position(state.cursor)

    meta = {
      {line, col},
      {line, col},
      nil
    }

    token = {:error_token, meta, diagnostic.reason}

    state = %{
      state
      | cursor: cursor,
        diagnostics: [diagnostic | state.diagnostics]
    }

    {token, state}
  end
end
