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

  alias ToxicParser.{
    Builder,
    Context,
    Cursor,
    Env,
    Error,
    EventLog,
    Grammar,
    Position,
    Result,
    State,
    TokenAdapter
  }

  @type mode :: :strict | :tolerant

  @type option ::
          {:mode, mode()}
          | {:preserve_comments, boolean()}
          | {:emit_events, boolean()}
          | {:emit_env, boolean()}
          | {:emit_warnings, boolean()}
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

    {state, cursor} = TokenAdapter.new(source, opts)
    log = EventLog.new()

    case Grammar.parse(state, cursor, log, ctx) do
      {:ok, ast, state, cursor, log} ->
        # In strict mode, ensure all tokens were consumed
        case check_remaining_tokens(state, cursor, mode) do
          :ok ->
            result =
              build_result(%{
                ast: ast,
                mode: mode,
                file: file,
                source: source,
                state: state,
                cursor: cursor,
                log: log,
                emit_events?: emit_events?,
                emit_env?: emit_env?
              })

            {:ok, result}

          {:error, reason, state} ->
            {parser_diag, state} = parser_error(reason, state, cursor)

            result =
              build_result(%{
                ast: maybe_tolerant_error_ast(mode, parser_diag, cursor),
                mode: mode,
                file: file,
                source: source,
                state: state,
                cursor: cursor,
                log: log,
                emit_events?: emit_events?,
                emit_env?: emit_env?,
                extra_diagnostics: [parser_diag]
              })

            {:error, result}
        end

      {:error, reason, state, cursor, log} ->
        {parser_diag, state} = parser_error(reason, state, cursor)

        result =
          build_result(%{
            ast: maybe_tolerant_error_ast(mode, parser_diag, cursor),
            mode: mode,
            file: file,
            source: source,
            state: state,
            cursor: cursor,
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
         cursor: cursor,
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
      diagnostics: annotate_anchor_paths(ast, :lists.reverse(state.diagnostics, extra_diagnostics)),
      warnings: Enum.reverse(state.warnings) ++ Enum.reverse(Cursor.warnings(cursor)),
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
         cursor: cursor,
         log: log,
         emit_events?: emit_events?,
         emit_env?: emit_env?
       }) do
    events = if emit_events?, do: EventLog.to_list(log), else: []
    env = if emit_env?, do: Env.from_events(events), else: []

    %Result{
      ast: ast,
      comments: [],
      diagnostics: annotate_anchor_paths(ast, Enum.reverse(state.diagnostics)),
      warnings: Enum.reverse(state.warnings) ++ Enum.reverse(Cursor.warnings(cursor)),
      events: events,
      env: env,
      mode: mode,
      file: file,
      source: source,
      metadata: %{terminators: state.terminators}
    }
  end

  defp parser_error(reason, %State{} = state, cursor) do
    normalized_reason =
      case reason do
        {loc, {prefix, detail}, token} when is_list(prefix) or is_binary(prefix) ->
          {loc, {to_string(prefix), to_string(detail)}, normalize_token_string(token)}

        {loc, msg, token} when is_list(msg) ->
          normalize_reserved_word({loc, List.to_string(msg), normalize_token_string(token)})

        {loc, msg, token} when is_list(token) ->
          normalize_reserved_word({loc, msg, normalize_token_string(token)})

        {loc, msg, token} ->
          normalize_reserved_word({loc, msg, normalize_token_string(token)})

        other ->
          other
      end

    {line, column} = Cursor.position(cursor)
    {id, state} = State.next_diagnostic_id(state)

    diagnostic = %Error{
      phase: :parser,
      reason: normalized_reason,
      token: nil,
      expected: nil,
      severity: :error,
      range: %{
        start: Position.to_location(line, column, state.line_index),
        end: Position.to_location(line, column, state.line_index)
      },
      details: %{
        source: :grammar,
        id: id,
        anchor: %{kind: :error_node, path: [], note: nil},
        synthetic?: true,
        lexer_error_code: nil
      }
    }

    {diagnostic, state}
  end

  defp maybe_tolerant_error_ast(:tolerant, %Error{} = diagnostic, cursor) do
    {line, column} = Cursor.position(cursor)

    meta = [
      line: line,
      column: column,
      toxic: %{synthetic?: true, anchor: %{line: line, column: column}}
    ]

    payload =
      Error.error_node_payload(diagnostic,
        kind: :invalid,
        original: diagnostic.reason,
        synthetic?: true
      )

    {:__block__, [], [Builder.Helpers.error(payload, meta)]}
  end

  defp maybe_tolerant_error_ast(_mode, _diagnostic, _cursor), do: nil

  defp normalize_token_string(token) when is_list(token) do
    if Enum.all?(token, &is_integer/1) do
      List.to_string(token)
    else
      try do
        IO.iodata_to_binary(token)
      rescue
        ArgumentError -> token
      end
    end
  end

  defp normalize_token_string(token), do: token

  defp normalize_reserved_word({loc, "unexpected reserved word: end", token}) do
    {loc, {"unexpected reserved word: ", ""}, token}
  end

  defp normalize_reserved_word({loc, "unexpected reserved word: end" <> suffix, token}) do
    {loc, {"unexpected reserved word: ", suffix}, token}
  end

  defp normalize_reserved_word(other), do: other

  defp annotate_anchor_paths(nil, diagnostics), do: diagnostics

  defp annotate_anchor_paths(ast, diagnostics) do
    id_paths = collect_error_paths(ast, [:root], %{})

    Enum.map(diagnostics, fn diagnostic ->
      case diagnostic.details do
        %{anchor: %{kind: :error_node} = anchor, id: id} when is_integer(id) ->
          path = Map.get(id_paths, id)
          updated_anchor = Map.put(anchor, :path, path || anchor[:path])
          %{diagnostic | details: Map.put(diagnostic.details, :anchor, updated_anchor)}

        _ ->
          diagnostic
      end
    end)
  end

  defp collect_error_paths({:__error__, _meta, payload}, path, acc) when is_map(payload) do
    case payload do
      %{diag_id: id} when is_integer(id) -> Map.put(acc, id, path)
      _ -> acc
    end
  end

  defp collect_error_paths({:__block__, _meta, exprs}, path, acc) when is_list(exprs) do
    Enum.with_index(exprs)
    |> Enum.reduce(acc, fn {expr, idx}, acc ->
      collect_error_paths(expr, path ++ [{:block, idx}], acc)
    end)
  end

  defp collect_error_paths({:%{}, _meta, kvs}, path, acc) when is_list(kvs) do
    Enum.with_index(kvs)
    |> Enum.reduce(acc, fn
      {{key, value}, idx}, acc ->
        acc = collect_error_paths(key, path ++ [{:map_kv, idx}, :key], acc)
        collect_error_paths(value, path ++ [{:map_kv, idx}, :value], acc)

      {other, idx}, acc ->
        collect_error_paths(other, path ++ [{:map_kv, idx}], acc)
    end)
  end

  defp collect_error_paths({:{}, _meta, elems}, path, acc) when is_list(elems) do
    Enum.with_index(elems)
    |> Enum.reduce(acc, fn {elem, idx}, acc ->
      collect_error_paths(elem, path ++ [{:tuple, idx}], acc)
    end)
  end

  defp collect_error_paths({:<<>>, _meta, parts}, path, acc) when is_list(parts) do
    Enum.with_index(parts)
    |> Enum.reduce(acc, fn {part, idx}, acc ->
      collect_error_paths(part, path ++ [{:bitstring, idx}], acc)
    end)
  end

  defp collect_error_paths({fun, _meta, args}, path, acc) when is_list(args) do
    acc = collect_error_paths(fun, path ++ [:callee], acc)

    Enum.with_index(args)
    |> Enum.reduce(acc, fn {arg, idx}, acc ->
      collect_error_paths(arg, path ++ [{:call_args, idx}], acc)
    end)
  end

  defp collect_error_paths(list, path, acc) when is_list(list) do
    Enum.with_index(list)
    |> Enum.reduce(acc, fn {elem, idx}, acc ->
      collect_error_paths(elem, path ++ [{:list, idx}], acc)
    end)
  end

  defp collect_error_paths(_other, _path, acc), do: acc

  # Check for remaining tokens after parsing in strict mode
  defp check_remaining_tokens(state, cursor, :strict) do
    case Cursor.peek(cursor) do
      {:eof, _cursor} ->
        :ok

      {:ok, {kind, _meta, _value}, _cursor}
      when kind in [:kw_identifier, :kw_identifier_safe, :kw_identifier_unsafe] ->
        {:error, ToxicParser.Grammar.Keywords.invalid_kw_identifier_error(state, cursor, kind),
         state}

      {:ok, token, _cursor} ->
        {:error, {:syntax_error_before, format_token(token)}, state}

      {:error, _reason, _cursor} ->
        # Lexer error on remaining input - already an error state
        :ok
    end
  end

  defp check_remaining_tokens(_state, _cursor, _mode), do: :ok

  # Format a token for error messages
  # Tokens are raw tuples: {kind, meta, value}
  defp format_token({kind, _meta, value}) do
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
