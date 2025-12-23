defmodule ToxicParser do
  @moduledoc """
  Entry points and contracts for the Toxic parser.

  Phase 0 implements mode handling, result struct wiring, and a conformance
  harness stubbed against `Code.string_to_quoted_with_comments/2`.

  Modes:
  - `:strict` stops on the first lexer/parser error and never synthesizes delimiters.
  - `:tolerant` collects diagnostics, emits synthetic error tokens for lexer fatals,
    and continues parsing with normalized `:eoe` tokens carrying newline counts.
  """

  alias ToxicParser.{Context, Env, Error, EventLog, Grammar, Position, Result, TokenAdapter}

  @type mode :: :strict | :tolerant

  @type option ::
          {:mode, mode()}
          | {:preserve_comments, boolean()}
          | {:emit_events, boolean()}
          | {:emit_env, boolean()}
          | {:token_metadata, boolean()}
          | {:literal_encoder, (term(), Macro.metadata() -> term())}
          | {:existing_atoms_only, boolean()}
          | {:fuel_limit, pos_integer() | :infinity}
          | {:terminators, [atom()]}
          | {:file, String.t()}

  @doc """
  Parses a string and returns a `ToxicParser.Result`.

  Phases 8-10 (strings, recovery, comments) are deferred; the public API is
  wired to the current grammar pipeline and will evolve as later phases land.
  """
  @spec parse_string(String.t(), [option()]) :: {:ok, Result.t()} | {:error, Result.t()}
  def parse_string(source, opts \\ []) when is_binary(source) do
    file = Keyword.get(opts, :file)
    mode = Keyword.get(opts, :mode, :strict)
    emit_events? = Keyword.get(opts, :emit_events, false)
    emit_env? = Keyword.get(opts, :emit_env, false)
    ctx = Keyword.get(opts, :expression_context, Context.expr())

    state = TokenAdapter.new(source, opts)
    log = EventLog.new()

    case Grammar.parse(state, log, ctx) do
      {:ok, ast, state, log} ->
        # In strict mode, ensure all tokens were consumed
        case check_remaining_tokens(state, mode) do
          :ok ->
            result =
              build_result(%{
                ast: ast,
                mode: mode,
                file: file,
                source: source,
                state: state,
                log: log,
                emit_events?: emit_events?,
                emit_env?: emit_env?
              })

            {:ok, result}

          {:error, reason, state} ->
            parser_diag = parser_error(reason, state)

            result =
              build_result(%{
                ast: nil,
                mode: mode,
                file: file,
                source: source,
                state: state,
                log: log,
                emit_events?: emit_events?,
                emit_env?: emit_env?,
                extra_diagnostics: [parser_diag]
              })

            {:error, result}
        end

      {:error, reason, state, log} ->
        parser_diag = parser_error(reason, state)

        result =
          build_result(%{
            ast: nil,
            mode: mode,
            file: file,
            source: source,
            state: state,
            log: log,
            emit_events?: emit_events?,
            emit_env?: emit_env?,
            extra_diagnostics: [parser_diag]
          })

        if mode == :strict, do: {:error, result}, else: {:ok, result}
    end
  end

  @doc """
  Parses a file by path, forwarding to `parse_string/2`.
  """
  @spec parse_file(Path.t(), [option()]) :: {:ok, Result.t()} | {:error, Result.t()}
  def parse_file(path, opts \\ []) do
    with {:ok, source} <- File.read(path) do
      parse_string(source, Keyword.put(opts, :file, path))
    end
  end

  @doc """
  Convenience entry point for already-loaded source; kept for API parity.
  """
  @spec parse(String.t(), [option()]) :: {:ok, Result.t()} | {:error, Result.t()}
  def parse(source, opts \\ []), do: parse_string(source, opts)

  defp build_result(%{
         ast: ast,
         mode: mode,
         file: file,
         source: source,
         state: state,
         log: log,
         emit_events?: emit_events?,
         emit_env?: emit_env?,
         extra_diagnostics: extra_diagnostics
       }) do
    events = if emit_events?, do: EventLog.to_list(log), else: []
    env = if emit_env?, do: Env.from_events(events), else: []

    %Result{
      ast: ast,
      comments: [],
      diagnostics: Enum.reverse(state.diagnostics) ++ extra_diagnostics,
      events: events,
      env: env,
      mode: mode,
      file: file,
      source: source,
      metadata: %{terminators: state.terminators}
    }
  end

  defp build_result(%{
         ast: ast,
         mode: mode,
         file: file,
         source: source,
         state: state,
         log: log,
         emit_events?: emit_events?,
         emit_env?: emit_env?
       }) do
    events = if emit_events?, do: EventLog.to_list(log), else: []
    env = if emit_env?, do: Env.from_events(events), else: []

    %Result{
      ast: ast,
      comments: [],
      diagnostics: Enum.reverse(state.diagnostics),
      events: events,
      env: env,
      mode: mode,
      file: file,
      source: source,
      metadata: %{terminators: state.terminators}
    }
  end

  defp parser_error(reason, state) do
    %Error{
      phase: :parser,
      reason: reason,
      token: nil,
      expected: nil,
      severity: :error,
      range: %{
        start: Position.to_location(1, 1, state.line_index),
        end: Position.to_location(1, 1, state.line_index)
      },
      details: %{source: :grammar}
    }
  end

  # Check for remaining tokens after parsing in strict mode
  defp check_remaining_tokens(state, :strict) do
    case TokenAdapter.peek(state) do
      {:eof, _state} ->
        :ok

      {:ok, token, _state} ->
        {:error, {:syntax_error_before, format_token(token)}, state}

      {:error, _diag, _state} ->
        # Lexer error on remaining input - already an error state
        :ok
    end
  end

  defp check_remaining_tokens(_state, _mode), do: :ok

  # Format a token for error messages
  # Tokens are raw tuples: {kind, meta}, {kind, meta, value}, or {kind, meta, v1, v2}
  defp format_token(token) when is_tuple(token) do
    kind = TokenAdapter.kind(token)
    value = TokenAdapter.value(token)

    case kind do
      :bin_string_start -> format_string_token(value)
      :list_string_start -> format_charlist_token(value)
      :sigil_start -> format_sigil_token(value)
      :alias -> Atom.to_string(value)
      :int when is_integer(value) -> Integer.to_string(value)
      :int when is_list(value) -> List.to_string(value)
      :int -> inspect(value)
      :flt when is_float(value) -> Float.to_string(value)
      :flt -> inspect(value)
      :char -> format_char_token(value)
      :atom -> inspect(value)
      _ when is_atom(value) -> Atom.to_string(value)
      _ -> inspect(kind)
    end
  end

  defp format_string_token(_), do: "\"\""
  defp format_charlist_token(_), do: "''"
  defp format_sigil_token({sigil, _delim}), do: "sigil ~#{sigil}"
  defp format_sigil_token(_), do: "sigil"

  defp format_char_token(char) when is_integer(char) do
    cond do
      char == ?\s -> "?\\s"
      char == ?\n -> "?\\n"
      char == ?\t -> "?\\t"
      char == ?\r -> "?\\r"
      char in 32..126 -> "?#{<<char::utf8>>}"
      true -> "?#{inspect(<<char::utf8>>)}"
    end
  end

  defp format_char_token(value), do: inspect(value)
end
