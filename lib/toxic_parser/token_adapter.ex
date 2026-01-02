defmodule ToxicParser.TokenAdapter do
  @moduledoc """
  Provides peek/checkpoint/rewind helpers for Toxic streaming tokens.

  Phase 4: Delegates to ToxicParser.Cursor for token transport.
  Cursor handles lookahead internally with a two-list queue, replacing
  both the Toxic stream batching and the previous TokenAdapter lookahead.

  Returns raw tokens from the lexer unchanged. Token accessors
  (kind/value/meta/etc.) are provided for convenience.
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
  # - 3-tuple: {kind, meta, value} - punctuation, keywords, literals, operators, identifiers

  @type raw_token :: tuple()
  @type token :: raw_token()

  @type result ::
          {:ok, token(), State.t(), Cursor.t()}
          | {:eof, State.t(), Cursor.t()}
          | {:error, Error.t(), State.t(), Cursor.t()}

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

  @doc "Extract value from a raw token."
  @spec value(token()) :: term()
  def value({_kind, _meta, value}), do: value
  def value(_), do: nil

  @doc "Extract second value from a tuple payload. Returns nil otherwise."
  @spec value2(token()) :: term()
  def value2({_kind, _meta, {_v1, v2}}), do: v2
  def value2(_), do: nil

  @doc "Extract meta from a raw token"
  @spec meta(token()) :: tuple()
  def meta({_kind, meta, _value}), do: meta

  @doc "Extract start line from token"
  @spec line(token()) :: pos_integer()
  def line({_kind, {{line, _}, _, _}, _value}), do: line

  @doc "Extract start column from token"
  @spec column(token()) :: pos_integer()
  def column({_kind, {{_, column}, _, _}, _value}), do: column

  @doc "Extract newline count from token (for :eol, :\";\", operators)"
  @spec newlines(token()) :: non_neg_integer()
  def newlines({kind, {_, _, count}, _value})
      when kind in [:eol, :";", :","] and is_integer(count),
      do: count

  # Literals encode parsed values in extra, not newline counts
  def newlines({kind, _meta, _value}) when kind in [:int, :flt, :char], do: 0
  # 3-tuple tokens with newline count in meta
  def newlines({_kind, {_, _, count}, _value}) when is_integer(count), do: count
  def newlines(_), do: 0

  @doc """
  Build [line: L, column: C] metadata from a token.
  This is the common AST metadata format.
  """
  @spec token_meta(token()) :: keyword()
  def token_meta({_, {{line, column}, _, _}, _}) do
    [line: line, column: column]
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

  defp delimiter({token, _meta, _value}) when token in [:"(", :")"], do: {:paren, {"(", ")"}}
  defp delimiter({token, _meta, _value}) when token in [:"[", :"]"], do: {:bracket, {"[", "]"}}
  defp delimiter({token, _meta, _value}) when token in [:"{", :"}"], do: {:curly, {"{", "}"}}

  defp delimiter({token, _meta, _value}) when token in [:"<<", :">>"],
    do: {:bitstring, {"<<", ">>"}}

  defp delimiter(_), do: nil

  defp delimiter_role({token, _meta, _value}) when token in [:"(", :"[", :"{", :"<<"], do: :open
  defp delimiter_role({token, _meta, _value}) when token in [:")", :"]", :"}", :">>"], do: :close
  defp delimiter_role(_), do: :none

  # ============================================================================
  # State management
  # ============================================================================

  @doc """
  Initializes parser state and stream.
  """
  @spec new(binary() | charlist(), keyword()) :: {State.t(), Cursor.t()}
  def new(source, opts \\ []), do: State.new(source, opts)

  @doc """
  Returns the current terminator snapshot without consuming tokens.
  Pay-for-play: only called when needed, not on every token.
  """
  @spec current_terminators(State.t(), Cursor.t()) :: {[term()], State.t()}
  def current_terminators(%State{} = state, cursor) do
    terms = Cursor.current_terminators(cursor)
    {terms, %{state | terminators: terms}}
  end

  # ============================================================================
  # Token consumption
  # ============================================================================

  @doc """
  Consume the next token.

  Returns raw tokens from the lexer unchanged.
  """
  @spec next(State.t(), Cursor.t()) :: result()
  def next(%State{} = state, cursor) do
    case state.fuel do
      :infinity ->
        case Cursor.next(cursor) do
          {:ok, raw, cursor} ->
            case raw do
              {:error_token, _meta, %Toxic.Error{} = err} ->
                {:ok, raw, add_diagnostics(state, [err]), cursor}

              {kind, _, _}
              when kind in @delimiter_tokens and (state.mode == :tolerant or state.emit_events?) ->
                {:ok, raw, update_terminators(state, cursor), cursor}

              _ ->
                {:ok, raw, state, cursor}
            end

          {:eof, cursor} ->
            {:eof, state, cursor}

          {:error, reason, cursor} ->
            handle_error(reason, cursor, state)
        end

      fuel when is_integer(fuel) ->
        if fuel > 0 do
          state = %{state | fuel: fuel - 1}

          case Cursor.next(cursor) do
            {:ok, raw, cursor} ->
              case raw do
                {:error_token, _meta, %Toxic.Error{} = err} ->
                  {:ok, raw, add_diagnostics(state, [err]), cursor}

                {kind, _, _}
                when kind in @delimiter_tokens and (state.mode == :tolerant or state.emit_events?) ->
                  {:ok, raw, update_terminators(state, cursor), cursor}

                _ ->
                  {:ok, raw, state, cursor}
              end

            {:eof, cursor} ->
              {:eof, state, cursor}

            {:error, reason, cursor} ->
              handle_error(reason, cursor, state)
          end
        else
          {:error, :out_of_fuel, state, cursor}
        end
    end
  end

  @doc """
  Peek the next token without consuming it.
  """
  @spec peek(State.t(), Cursor.t()) :: result()
  def peek(%State{} = state, cursor) do
    case Cursor.peek(cursor) do
      {:ok, raw, cursor} ->
        # Return raw token unchanged.
        # Per LEXER_REFACTOR_V3.md item 6: do NOT update terminators on peek.
        {:ok, raw, state, cursor}

      {:eof, cursor} ->
        {:eof, state, cursor}

      {:error, reason, cursor} ->
        handle_error(reason, cursor, state)
    end
  end

  @doc """
  Push a previously consumed token back into the lookahead buffer.
  """
  @spec pushback(State.t(), Cursor.t(), token()) :: {State.t(), Cursor.t()}
  def pushback(%State{} = state, cursor, token) when is_tuple(token) do
    {state, Cursor.pushback(cursor, token)}
  end

  @doc """
  Push multiple previously consumed tokens back into the lookahead buffer.

  Tokens must be in source order (the first token in the list will be the next token returned).
  """
  @spec pushback_many(State.t(), Cursor.t(), [token()]) :: {State.t(), Cursor.t()}
  def pushback_many(%State{} = state, cursor, tokens) when is_list(tokens) do
    {state, Cursor.pushback_many(cursor, tokens)}
  end

  # @doc """
  # Peek at the next N tokens (bounded by `state.max_peek`).
  # """
  # @spec peek_n(State.t(), pos_integer()) ::
  #         {:ok, [token()], State.t()}
  #         | {:eof, [token()], State.t()}
  #         | {:error, Error.t(), [token()], State.t()}
  # def peek_n(%State{max_peek: max} = _state, n) when n > max do
  #   raise ArgumentError, "peek_n/2 capped at #{max}"
  # end

  # def peek_n(%State{cursor: cursor} = state, n) do
  #   case Cursor.peek_n(cursor, n) do
  #     {:ok, tokens, cursor} ->
  #       state = %{state | cursor: cursor}
  #       {tokens, state} = process_tokens_for_peek(tokens, state)
  #       {:ok, tokens, state}

  #     {:eof, tokens, cursor} ->
  #       state = %{state | cursor: cursor}
  #       {tokens, state} = process_tokens_for_peek(tokens, state)
  #       {:eof, tokens, state}

  #     {:error, reason, tokens, cursor} ->
  #       state = %{state | cursor: cursor}
  #       {tokens, state} = process_tokens_for_peek(tokens, state)
  #       diagnostic = make_diagnostic(reason, state)
  #       {:error, diagnostic, tokens, %{state | diagnostics: [diagnostic | state.diagnostics]}}
  #   end
  # end

  # ============================================================================
  # Checkpointing
  # ============================================================================

  @doc """
  Create a checkpoint for backtracking.
  Saves cursor state plus diagnostics/event_log for full rollback.
  """
  @spec checkpoint(State.t(), Cursor.t()) :: {reference(), State.t()}
  def checkpoint(%State{} = state, cursor) do
    ref = make_ref()

    saved = %{
      ref: ref,
      cursor: cursor,
      diagnostics: state.diagnostics,
      terminators: state.terminators,
      event_log: state.event_log
    }

    {ref, %{state | checkpoints: Map.put(state.checkpoints, ref, saved)}}
  end

  @doc """
  Rewind to a previously created checkpoint.
  """
  @spec rewind(State.t(), reference()) :: {State.t(), Cursor.t()}
  def rewind(%State{} = state, ref) do
    {checkpoint, checkpoints} = Map.pop!(state.checkpoints, ref)

    {%{
       state
       | diagnostics: checkpoint.diagnostics,
         terminators: checkpoint.terminators,
         event_log: checkpoint.event_log,
         checkpoints: checkpoints
     }, checkpoint.cursor}
  end

  @doc """
  Drop a checkpoint without rewinding.
  """
  @spec drop_checkpoint(State.t(), reference()) :: State.t()
  def drop_checkpoint(%State{} = state, ref) do
    checkpoints = Map.delete(state.checkpoints, ref)
    %{state | checkpoints: checkpoints}
  end

  # ============================================================================
  # Private helpers
  # ============================================================================

  defguard is_delimiter(token) when elem(token, 0) in @delimiter_tokens

  defp handle_error(reason, cursor, state) do
    terminators = Cursor.current_terminators(cursor)
    state = %{state | terminators: terminators}

    diagnostic = make_diagnostic(reason, state)

    if state.mode == :tolerant do
      {token, state} = synthetic_error_token(state, cursor, diagnostic)
      {:ok, token, state, cursor}
    else
      {:error, diagnostic, %{state | diagnostics: [diagnostic | state.diagnostics]}, cursor}
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

  defp add_diagnostics(state, diagnostics) do
    %{state | diagnostics: :lists.reverse(diagnostics, state.diagnostics)}
  end

  defp update_terminators(state, cursor) do
    terms = Cursor.current_terminators(cursor)
    %{state | terminators: terms}
  end

  # Process tokens for peek_n - just extract error diagnostics
  # defp process_tokens_for_peek(tokens, state) do
  #   Enum.map_reduce(tokens, state, fn raw, state ->
  #     {raw, maybe_extract_error_diagnostic(raw, state)}
  #   end)
  # end

  defp synthetic_error_token(state, cursor, diagnostic) do
    {line, col} = Cursor.position(cursor)

    meta = {
      {line, col},
      {line, col},
      nil
    }

    token = {:error_token, meta, diagnostic.reason}

    state = %{
      state
      | diagnostics: [diagnostic | state.diagnostics]
    }

    {token, state}
  end
end
