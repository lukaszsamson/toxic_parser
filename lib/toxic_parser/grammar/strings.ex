defmodule ToxicParser.Grammar.Strings do
  @moduledoc """
  Parsing for strings, charlists, heredocs, sigils, and quoted atoms from Toxic token streams.
  """

  alias ToxicParser.{Builder, Context, Cursor, Error, EventLog, ParseOpts, Pratt, State, TokenAdapter}
  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.{ErrorHelpers, Expressions}
  alias ToxicParser.Position

  @simple_string_start [:bin_string_start, :list_string_start]
  @heredoc_start [:bin_heredoc_start, :list_heredoc_start]
  @sigil_start [:sigil_start]
  @atom_start [:atom_unsafe_start, :atom_safe_start]
  # TODO: atom_safe_start

  # Max atom length is 255 bytes
  @max_atom_length 255

  @type result ::
          {:ok, Macro.t(), State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}
          | {:no_string, State.t(), Cursor.t()}
          | {:keyword_key, atom(), keyword(), State.t(), Cursor.t(), EventLog.t()}
          | {:keyword_key_interpolated, list(), atom(), keyword(), String.t(), State.t(),
             Cursor.t(), EventLog.t()}

  @spec parse(
          State.t(),
          Cursor.t(),
          Pratt.context(),
          EventLog.t(),
          non_neg_integer(),
          ParseOpts.t()
        ) :: result()
  def parse(
        %State{} = state,
        cursor,
        %Context{} = ctx,
        %EventLog{} = log,
        min_bp \\ 0,
        %ParseOpts{} = opts \\ %ParseOpts{}
      ) do
    case Cursor.peek(cursor) do
      {:ok, {kind, _meta, _value}, cursor} ->
        case kind do
          kind when kind in @simple_string_start ->
            parse_simple_string(state, cursor, ctx, log, min_bp, opts)

          kind when kind in @heredoc_start ->
            parse_heredoc(state, cursor, ctx, log, min_bp, opts)

          kind when kind in @sigil_start ->
            parse_sigil(state, cursor, ctx, log, min_bp, opts)

          kind when kind in @atom_start ->
            parse_quoted_atom(state, cursor, ctx, kind, log, min_bp, opts)

          _ ->
            {:no_string, state, cursor}
        end

      {:eof, cursor} ->
        {:no_string, state, cursor}

      {:error, _diag, cursor} ->
        {:no_string, state, cursor}
    end
  end

  # Parse simple strings (not heredocs)
  defp parse_simple_string(state, cursor, ctx, log, min_bp, opts) do
    {:ok, {start_tok_kind, _meta, _value} = start_tok, state, cursor} =
      TokenAdapter.next(state, cursor)

    start_meta = TokenAdapter.token_meta(start_tok)
    kind = string_kind(start_tok_kind)
    target_ends = closing_for(start_tok_kind)

    # Get delimiter for metadata
    delimiter = string_delimiter(start_tok_kind)

    case collect_parts([], state, cursor, target_ends, kind, log) do
      {:ok, parts, actual_end, end_tok, state, cursor, log, _recovery} ->
        with {:ok, _close, state, cursor} <- TokenAdapter.next(state, cursor) do
          # If string ended with kw_identifier_unsafe_end, it's a keyword key (atom)
          if actual_end in [:kw_identifier_unsafe_end, :kw_identifier_safe_end] do
            # Check if there's any interpolation in the parts
            if has_interpolations?(parts) do
              # Build binary_to_atom call for interpolated keyword
              {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, cursor, log}
            else
              content = merge_fragments(parts)

              if byte_size(content) > @max_atom_length do
                {:error, {:atom_too_long, content, TokenAdapter.token_meta(start_tok)}, state,
                 cursor, log}
              else
                atom = String.to_atom(content)
                # Include delimiter and format: :keyword for quoted keyword keys
                key_meta = [{:delimiter, delimiter}, {:format, :keyword} | start_meta]
                {:keyword_key, atom, key_meta, state, cursor, log}
              end
            end
          else
            build_string_ast(
              parts,
              kind,
              start_meta,
              delimiter,
              end_tok,
              state,
              cursor,
              ctx,
              log,
              min_bp,
              opts
            )
          end
        end

      {:error, {:missing_terminator, code, error_tok}, state, cursor, log} ->
        if state.mode == :tolerant do
          {state, cursor} = sync_to_string_end(state, cursor, target_ends)
          {state, cursor} = maybe_recover_missing_terminator(state, cursor, start_meta, kind, code)
          error_ast = build_missing_terminator_node(error_tok, start_meta, state, cursor, [])
          normalize_string_errors(error_ast, state, cursor, log, ctx, min_bp, opts)
        else
          {:error, {:missing_terminator, code}, state, cursor, log}
        end

      {:error, reason, state, cursor, log} ->
        if state.mode == :tolerant do
          {state, cursor} = sync_to_string_end(state, cursor, target_ends)
          case missing_terminator_diagnostic(state) do
            {:ok, code, diagnostic} when reason == :unexpected_eof ->
              {state, cursor} = maybe_recover_missing_terminator(state, cursor, start_meta, kind, code)
              error_ast = build_missing_terminator_node_from_diagnostic(diagnostic, start_meta, cursor, [])
              normalize_string_errors(error_ast, state, cursor, log, ctx, min_bp, opts)

            _ ->
              {error_ast, state} = build_string_error_node(reason, start_meta, state, cursor, [])
              normalize_string_errors(error_ast, state, cursor, log, ctx, min_bp, opts)
          end
        else
          {:error, reason, state, cursor, log}
        end
    end
  end

  # Parse heredocs with indentation handling
  defp parse_heredoc(state, cursor, ctx, log, min_bp, opts) do
    {:ok, {start_tok_kind, _meta, _value} = start_tok, state, cursor} =
      TokenAdapter.next(state, cursor)

    start_meta = TokenAdapter.token_meta(start_tok)
    kind = string_kind(start_tok_kind)
    target_ends = closing_for(start_tok_kind)

    case collect_parts([], state, cursor, target_ends, kind, log) do
      {:ok, parts, _actual_end, end_tok, state, cursor, log, _recovery} ->
        with {:ok, _close, state, cursor} <- TokenAdapter.next(state, cursor) do
          # Extract indentation from end token
          indentation = get_heredoc_indentation(end_tok)

          # Trim indentation from parts
          trimmed_parts = trim_heredoc_parts(parts, indentation, line_continuation?: true)

          build_heredoc_ast(
            trimmed_parts,
            kind,
            start_meta,
            indentation,
            state,
            cursor,
            ctx,
            log,
            min_bp,
            opts
          )
        end

      {:error, {:missing_terminator, code, error_tok}, state, cursor, log} ->
        if state.mode == :tolerant do
          {state, cursor} = sync_to_string_end(state, cursor, target_ends)
          {state, cursor} = maybe_recover_missing_terminator(state, cursor, start_meta, kind, code)
          error_ast = build_missing_terminator_node(error_tok, start_meta, state, cursor, [])
          normalize_string_errors(error_ast, state, cursor, log, ctx, min_bp, opts)
        else
          {:error, {:missing_terminator, code}, state, cursor, log}
        end

      {:error, reason, state, cursor, log} ->
        if state.mode == :tolerant do
          {state, cursor} = sync_to_string_end(state, cursor, target_ends)
          case missing_terminator_diagnostic(state) do
            {:ok, code, diagnostic} when reason == :unexpected_eof ->
              {state, cursor} = maybe_recover_missing_terminator(state, cursor, start_meta, kind, code)
              error_ast = build_missing_terminator_node_from_diagnostic(diagnostic, start_meta, cursor, [])
              normalize_string_errors(error_ast, state, cursor, log, ctx, min_bp, opts)

            _ ->
              {error_ast, state} = build_string_error_node(reason, start_meta, state, cursor, [])
              normalize_string_errors(error_ast, state, cursor, log, ctx, min_bp, opts)
          end
        else
          {:error, reason, state, cursor, log}
        end
    end
  end

  defp parse_sigil(state, cursor, ctx, log, min_bp, opts) do
    {:ok, {:sigil_start, _meta, {sigil, delimiter}} = start_tok, state, cursor} =
      TokenAdapter.next(state, cursor)

    start_meta = TokenAdapter.token_meta(start_tok)

    case collect_parts([], state, cursor, [:sigil_end], :sigil, log) do
      {:ok, parts, _actual_end, end_tok, state, cursor, log, _recovery} ->
        with {:ok, _close, state, cursor} <- TokenAdapter.next(state, cursor) do
          # Check for sigil_modifiers token
          {modifiers, state, cursor} =
            case Cursor.peek(cursor) do
              {:ok, {kind, _meta, _value}, cursor} ->
                case kind do
                  :sigil_modifiers ->
                    {:ok, {:sigil_modifiers, _mod_meta, modifiers}, state, cursor} =
                      TokenAdapter.next(state, cursor)

                    {modifiers, state, cursor}

                  _ ->
                    {[], state, cursor}
                end

              {:ok, _, cursor} ->
                {[], state, cursor}

              {:eof, cursor} ->
                {[], state, cursor}
            end

          # Get indentation from end token (for heredoc sigils)
          indentation = get_sigil_indentation(end_tok)

          # If heredoc sigil, trim parts by indentation
          parts =
            if indentation != nil do
              # Sigil heredocs do NOT treat backslash-newline (\\\n) as a line continuation.
              # It must be preserved in the resulting string to match Elixir's AST.
              trim_heredoc_parts(parts, indentation, line_continuation?: false)
            else
              parts
            end

          # Build metadata with delimiter (sigil node)
          meta_with_delimiter = [{:delimiter, delimiter} | start_meta]

          # Build content meta - add indentation if present (for heredoc sigils)
          content_meta =
            if indentation != nil do
              [{:indentation, indentation} | start_meta]
            else
              start_meta
            end

          # Build the sigil AST: {sigil, meta, [{:<<>>, meta, parts}, modifiers]}
          # First arg is always {:<<>>, meta, parts} even for single strings
          content_ast = {:<<>>, content_meta, build_sigil_parts(parts)}

          ast = {sigil, meta_with_delimiter, [content_ast, modifiers]}
          Pratt.led(ast, state, cursor, log, min_bp, ctx, opts)
        end

      {:error, {:missing_terminator, code, error_tok}, state, cursor, log} ->
        if state.mode == :tolerant do
          {state, cursor} = sync_to_string_end(state, cursor, [:sigil_end])
          {state, cursor} = maybe_recover_missing_terminator(state, cursor, start_meta, :sigil, code)
          error_ast = build_missing_terminator_node(error_tok, start_meta, state, cursor, [])
          normalize_string_errors(error_ast, state, cursor, log, ctx, min_bp, opts)
        else
          {:error, {:missing_terminator, code}, state, cursor, log}
        end

      {:error, reason, state, cursor, log} ->
        if state.mode == :tolerant do
          {state, cursor} = sync_to_string_end(state, cursor, [:sigil_end])
          case missing_terminator_diagnostic(state) do
            {:ok, code, diagnostic} when reason == :unexpected_eof ->
              {state, cursor} = maybe_recover_missing_terminator(state, cursor, start_meta, :sigil, code)
              error_ast = build_missing_terminator_node_from_diagnostic(diagnostic, start_meta, cursor, [])
              normalize_string_errors(error_ast, state, cursor, log, ctx, min_bp, opts)

            _ ->
              {error_ast, state} = build_string_error_node(reason, start_meta, state, cursor, [])
              normalize_string_errors(error_ast, state, cursor, log, ctx, min_bp, opts)
          end
        else
          {:error, reason, state, cursor, log}
        end
    end
  end

  # Parse quoted atoms: :"foo", :"foo bar", :""
  defp parse_quoted_atom(state, cursor, ctx, kind, log, min_bp, opts) do
    {:ok, {^kind, _meta, delimiter} = start_tok, state, cursor} =
      TokenAdapter.next(state, cursor)

    start_meta = TokenAdapter.token_meta(start_tok)
    # Use delimiter from token value (39 = ', 34 = ")
    delimiter = if delimiter == 39, do: "'", else: "\""
    meta_with_delimiter = [{:delimiter, delimiter} | start_meta]

    end_token_kind =
      if kind == :atom_unsafe_start do
        :atom_unsafe_end
      else
        :atom_safe_end
      end

    case collect_parts([], state, cursor, [end_token_kind], :atom, log) do
      {:ok, parts, _actual_end, _end_tok, state, cursor, log, _recovery} ->
        with {:ok, _close, state, cursor} <- TokenAdapter.next(state, cursor) do
          # Check if there are any interpolations
          if has_interpolations?(parts) do
            # Build interpolated atom AST with delimiter metadata
            args = build_interpolated_parts(parts, :atom)

            ast =
              {{:., start_meta, [:erlang, :binary_to_atom]}, meta_with_delimiter,
               [{:<<>>, start_meta, args}, :utf8]}

            Pratt.led(ast, state, cursor, log, min_bp, ctx, opts)
          else
            content = merge_fragments(parts)

            if byte_size(content) > @max_atom_length do
              {:error, {:atom_too_long, content, TokenAdapter.token_meta(start_tok)}, state,
               cursor, log}
            else
              atom = String.to_atom(content)
              # Include delimiter in metadata for literal_encoder
              ast = Builder.Helpers.literal(atom, meta_with_delimiter, state)
              Pratt.led(ast, state, cursor, log, min_bp, ctx, opts)
            end
          end
        end

      {:error, {:missing_terminator, code, error_tok}, state, cursor, log} ->
        if state.mode == :tolerant do
          {state, cursor} = sync_to_string_end(state, cursor, [end_token_kind])
          {state, cursor} = maybe_recover_missing_terminator(state, cursor, start_meta, :atom, code)
          error_ast = build_missing_terminator_node(error_tok, start_meta, state, cursor, [])
          normalize_string_errors(error_ast, state, cursor, log, ctx, min_bp, opts)
        else
          {:error, {:missing_terminator, code}, state, cursor, log}
        end

      {:error, reason, state, cursor, log} ->
        if state.mode == :tolerant do
          {state, cursor} = sync_to_string_end(state, cursor, [end_token_kind])
          case missing_terminator_diagnostic(state) do
            {:ok, code, diagnostic} when reason == :unexpected_eof ->
              {state, cursor} = maybe_recover_missing_terminator(state, cursor, start_meta, :atom, code)
              error_ast = build_missing_terminator_node_from_diagnostic(diagnostic, start_meta, cursor, [])
              normalize_string_errors(error_ast, state, cursor, log, ctx, min_bp, opts)

            _ ->
              {error_ast, state} = build_string_error_node(reason, start_meta, state, cursor, [])
              normalize_string_errors(error_ast, state, cursor, log, ctx, min_bp, opts)
          end
        else
          {:error, reason, state, cursor, log}
        end
    end
  end

  # Collect parts (fragments and interpolations)
  # Returns {:ok, parts, actual_end_kind, end_token, state, cursor, log}
  # For sigils, do not unescape content (no_unescape: true)
  defp collect_parts(acc, state, cursor, target_ends, kind, log) do
    # Sigils should not unescape content
    # Heredocs should not unescape here - unescaping happens after indentation trimming
    # (because line continuation \\\n affects indentation stripping)
    should_unescape = kind not in [:sigil, :heredoc_binary, :heredoc_charlist]

    case Cursor.peek(cursor) do
      {:ok, {end_kind, _meta, _value} = tok, cursor} ->
        if end_kind in target_ends do
          {:ok, Enum.reverse(acc), end_kind, tok, state, cursor, log, nil}
        else
          case end_kind do
            :string_fragment ->
              {:ok, {:string_fragment, _frag_meta, fragment} = frag_tok, state, cursor} =
                TokenAdapter.next(state, cursor)

              if should_unescape do
                case safe_unescape(fragment) do
                  {:ok, content} ->
                    collect_parts(
                      [{:fragment, content} | acc],
                      state,
                      cursor,
                      target_ends,
                      kind,
                      log
                    )

                  {:error, reason} ->
                    {:error, {:unescape_error, reason, TokenAdapter.token_meta(frag_tok)}, state,
                     cursor, log}
                end
              else
                collect_parts(
                  [{:fragment, fragment} | acc],
                  state,
                  cursor,
                  target_ends,
                  kind,
                  log
                )
              end

            :begin_interpolation ->
              with {:ok, interp_ast, state, cursor, log} <-
                     parse_interpolation(state, cursor, kind, log) do
              collect_parts(
                [{:interpolation, interp_ast} | acc],
                state,
                cursor,
                target_ends,
                kind,
                log
              )
            end

          :error_token ->
            {:ok, error_tok, state, cursor} = TokenAdapter.next(state, cursor)

            case error_tok do
              {:error_token, _meta, %Toxic.Error{code: code}}
              when code in [:string_missing_terminator, :interpolation_missing_terminator, :heredoc_missing_terminator] and
                     state.mode == :tolerant ->
                {:error, {:missing_terminator, code, error_tok}, state, cursor, log}

              _ ->
                error_ast = build_error_token_node(error_tok, state)

                collect_parts(
                  [{:interpolation, error_ast} | acc],
                  state,
                  cursor,
                  target_ends,
                  kind,
                  log
                )
            end

          _ ->
            if state.mode == :tolerant do
              case TokenAdapter.next(state, cursor) do
                {:ok, _tok, state, cursor} -> collect_parts(acc, state, cursor, target_ends, kind, log)
                {:eof, state, cursor} -> {:error, :unexpected_eof, state, cursor, log}
                {:error, reason, state, cursor} -> {:error, reason, state, cursor, log}
              end
            else
              {:error, {:unexpected_string_token, end_kind}, state, cursor, log}
            end
          end
        end

      {:eof, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, cursor} ->
        if state.mode == :tolerant do
          case TokenAdapter.next(state, cursor) do
            {:ok, _tok, state, cursor} -> collect_parts(acc, state, cursor, target_ends, kind, log)
            {:eof, state, cursor} -> {:error, :unexpected_eof, state, cursor, log}
            {:error, reason, state, cursor} -> {:error, reason, state, cursor, log}
          end
        else
          {:error, diag, state, cursor, log}
        end
    end
  end

  defp normalize_string_errors(ast, %State{} = state, cursor, log, ctx, min_bp, opts) do
    case ast do
      {:__error__, _meta, _payload} = err -> Pratt.led(err, state, cursor, log, min_bp, ctx, opts)
      _ -> Pratt.led(ast, state, cursor, log, min_bp, ctx, opts)
    end
  end

  defp maybe_recover_missing_terminator(state, cursor, start_meta, kind, code) do
    case missing_terminator_suffix(state, start_meta, kind, code) do
      {:ok, suffix, line, column} ->
        tokens = lex_suffix_tokens(state, suffix, line, column)
        TokenAdapter.pushback_many(state, cursor, tokens)

      :none ->
        {state, cursor}
    end
  end

  defp missing_terminator_suffix(%State{} = state, start_meta, kind, code) do
    line = Keyword.get(start_meta, :line, 1)
    column = Keyword.get(start_meta, :column, 1)
    %{offset: offset} = Position.to_location(line, column, state.line_index)
    size = byte_size(state.source)

    if offset >= size do
      :none
    else
      rest = binary_part(state.source, offset, size - offset)

      case missing_terminator_strategy(kind, code) do
        :first_newline ->
          case :binary.match(rest, "\n") do
            {idx, 1} when idx + 1 < byte_size(rest) ->
              suffix = binary_part(rest, idx + 1, byte_size(rest) - idx - 1)
              {:ok, suffix, line + 1, 1}

            _ ->
              :none
          end

        :last_newline ->
          case :binary.matches(rest, "\n") do
            [] ->
              :none

            matches ->
              {idx, 1} = List.last(matches)
              suffix = binary_part(rest, idx + 1, byte_size(rest) - idx - 1)
              {:ok, suffix, line + length(matches), 1}
          end
      end
    end
  end

  defp missing_terminator_strategy(kind, code)
       when code in [:heredoc_missing_terminator] or kind in [:heredoc_binary, :heredoc_charlist] do
    :last_newline
  end

  defp missing_terminator_strategy(_kind, _code), do: :first_newline

  defp lex_suffix_tokens(%State{} = state, suffix, line, column) do
    opts =
      state.opts
      |> Keyword.put(:mode, state.mode)
      |> Keyword.put(:line, line)
      |> Keyword.put(:column, column)

    suffix_cursor = Cursor.new(suffix, opts)
    lex_suffix_tokens([], suffix_cursor)
  end

  defp lex_suffix_tokens(acc, suffix_cursor) do
    case Cursor.next(suffix_cursor) do
      {:ok, tok, suffix_cursor} ->
        lex_suffix_tokens([tok | acc], suffix_cursor)

      {:eof, _suffix_cursor} ->
        Enum.reverse(acc)

      {:error, _reason, _suffix_cursor} ->
        Enum.reverse(acc)
    end
  end

  defp build_missing_terminator_node(error_tok, start_meta, %State{} = state, cursor, children) do
    synthetic? = ErrorHelpers.synthetic_meta?(start_meta, state)

    payload =
      case State.error_token_diagnostic(state, TokenAdapter.meta(error_tok)) do
        %Error{} = diagnostic ->
          Error.error_node_payload(diagnostic,
            kind: :token,
            original: elem(error_tok, 2),
            children: children,
            synthetic?: synthetic?
          )

        _ ->
          elem(error_tok, 2)
      end

    {line, column, _} = ErrorHelpers.error_anchor(start_meta, state, cursor)

    error_meta = ErrorHelpers.build_error_meta(line, column, synthetic?)
    Builder.Helpers.error(payload, error_meta)
  end

  defp build_missing_terminator_node_from_diagnostic(%Error{} = diagnostic, start_meta, cursor, children) do
    synthetic? = synthetic_meta?(start_meta, nil)

    payload =
      Error.error_node_payload(diagnostic,
        kind: :token,
        original: diagnostic.reason,
        children: children,
        synthetic?: synthetic?
      )

    {line, column, _} = error_anchor(start_meta, nil, cursor)

    error_meta = ErrorHelpers.build_error_meta(line, column, synthetic?)
    Builder.Helpers.error(payload, error_meta)
  end

  defp missing_terminator_diagnostic(%State{} = state) do
    case Enum.find(state.diagnostics, fn diag ->
           diag.phase == :lexer and
             diag.details[:lexer_error_code] in [
               :string_missing_terminator,
               :interpolation_missing_terminator,
               :heredoc_missing_terminator
             ]
         end) do
      %Error{} = diagnostic ->
        {:ok, diagnostic.details[:lexer_error_code], diagnostic}

      _ ->
        :none
    end
  end

  defp error_anchor(meta, nil, cursor) do
    {line, column} =
      case meta do
        {{line, column}, _, _} -> {line, column}
        meta when is_list(meta) -> {Keyword.get(meta, :line), Keyword.get(meta, :column)}
        _ -> {nil, nil}
      end

    {line, column} =
      case {line, column} do
        {nil, nil} -> Cursor.position(cursor)
        {line, column} -> {line || 1, column || 1}
      end

    {line, column, synthetic_meta?(meta, nil)}
  end

  defp synthetic_meta?(meta, nil) do
    case meta do
      {{_, _}, _, _} -> false
      meta when is_list(meta) -> Keyword.get(meta, :line) == nil and Keyword.get(meta, :column) == nil
      _ -> true
    end
  end

  # Parse an interpolation: #{expr}
  defp parse_interpolation(state, cursor, kind, log) do
    {:ok, begin_tok, state, cursor} = TokenAdapter.next(state, cursor)
    open_meta = TokenAdapter.token_meta(begin_tok)

    # Check for empty interpolation #{} first
    case Cursor.peek(cursor) do
      {:ok, {peek_kind, _meta, _value} = tok, cursor} ->
        case peek_kind do
          :end_interpolation ->
            # Empty interpolation - use empty block
            {:ok, _end, state, cursor} = TokenAdapter.next(state, cursor)
            close_meta = TokenAdapter.token_meta(tok)
            empty_block = {:__block__, [], []}
            interp_ast = build_interpolation_ast(empty_block, open_meta, close_meta, kind)
            {:ok, interp_ast, state, cursor, log}

          k when k in [:eol, :";"] ->
            # Interpolation starting with separator (e.g., #{;})
            # In Elixir grammar: grammar -> eoe : {'__block__', meta_from_token('$1'), []}.
            {:ok, sep_tok, state, cursor} = TokenAdapter.next(state, cursor)
            sep_meta = TokenAdapter.token_meta(sep_tok)
            # Skip any additional EOE
            alias ToxicParser.Grammar.EOE
            {state, cursor} = EOE.skip(state, cursor)

            case Cursor.peek(cursor) do
              {:ok, {end_kind, _meta, _value} = end_tok, cursor} ->
                case end_kind do
                  :end_interpolation ->
                    # Just EOE before close - empty block with EOE metadata
                    {:ok, _end, state, cursor} = TokenAdapter.next(state, cursor)
                    close_meta = TokenAdapter.token_meta(end_tok)
                    empty_block = {:__block__, sep_meta, []}
                    interp_ast = build_interpolation_ast(empty_block, open_meta, close_meta, kind)
                    {:ok, interp_ast, state, cursor, log}

                  _ ->
                    # EOE followed by content - parse the content as expr_list
                    case Expressions.expr_list(state, cursor, Context.expr(), log) do
                      {:ok, expr, state, cursor, log} ->
                        case Cursor.peek(cursor) do
                          {:ok, {interp_end_kind, _meta, _value} = interp_end_tok, cursor} ->
                            cond do
                              interp_end_kind == :end_interpolation ->
                                {:ok, _end, state, cursor} = TokenAdapter.next(state, cursor)
                                close_meta = TokenAdapter.token_meta(interp_end_tok)

                                interp_ast =
                                  build_interpolation_ast(expr, open_meta, close_meta, kind)

                                {:ok, interp_ast, state, cursor, log}

                              interp_end_kind == :error_token and state.mode == :tolerant ->
                                {:ok, error_tok, state, cursor} = TokenAdapter.next(state, cursor)

                                case error_tok do
                                  {:error_token, _meta, %Toxic.Error{code: :interpolation_missing_terminator}} ->
                                    {:error, {:missing_terminator, :interpolation_missing_terminator, error_tok}, state, cursor, log}

                                  _ ->
                                    {:error, {:expected_end_interpolation, interp_end_kind}, state,
                                     cursor, log}
                                end

                              true ->
                                {:error, {:expected_end_interpolation, interp_end_kind}, state,
                                 cursor, log}
                            end

                          {:eof, cursor} ->
                            {:error, :unexpected_eof, state, cursor, log}

                          {:error, diag, cursor} ->
                            {:error, diag, state, cursor, log}
                        end

                      {:error, reason, state, cursor, log} ->
                        {:error, reason, state, cursor, log}
                    end
                end

              {:eof, cursor} ->
                {:error, :unexpected_eof, state, cursor, log}

              {:error, diag, cursor} ->
                {:error, diag, state, cursor, log}
            end

          _ ->
            # Parse the expression inside interpolation
            # Use :unmatched context so do-blocks are consumed by inner calls
            # (e.g., "foo#{K.'a' do :ok end}bar" - the do belongs to the 'a' call)
            case Expressions.expr_list(state, cursor, Context.expr(), log) do
              {:ok, expr, state, cursor, log} ->
                # Consume end_interpolation
                case Cursor.peek(cursor) do
                  {:ok, {end_kind, _meta, _value} = end_tok, cursor} ->
                    cond do
                      end_kind == :end_interpolation ->
                        {:ok, _end, state, cursor} = TokenAdapter.next(state, cursor)
                        close_meta = TokenAdapter.token_meta(end_tok)

                        # Build interpolation AST based on kind
                        interp_ast = build_interpolation_ast(expr, open_meta, close_meta, kind)
                        {:ok, interp_ast, state, cursor, log}

                      end_kind == :error_token and state.mode == :tolerant ->
                        {:ok, error_tok, state, cursor} = TokenAdapter.next(state, cursor)

                        case error_tok do
                          {:error_token, _meta, %Toxic.Error{code: :interpolation_missing_terminator}} ->
                            {:error, {:missing_terminator, :interpolation_missing_terminator, error_tok}, state, cursor, log}

                          _ ->
                            {:error, {:expected_end_interpolation, end_kind}, state, cursor, log}
                        end

                      true ->
                        {:error, {:expected_end_interpolation, end_kind}, state, cursor, log}
                    end

                  {:eof, cursor} ->
                    {:error, :unexpected_eof, state, cursor, log}

                  {:error, diag, cursor} ->
                    {:error, diag, state, cursor, log}
                end

              {:error, reason, state, cursor, log} ->
                {:error, reason, state, cursor, log}
            end
        end

      {:eof, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  # Build interpolation AST based on string kind
  defp build_interpolation_ast(expr, open_meta, close_meta, kind) do
    call_meta = Meta.closing_meta(open_meta, close_meta, 0, from_interpolation: true)

    case kind do
      k when k in [:binary, :heredoc_binary, :sigil, :atom] ->
        # Binary interpolation: {:"::", meta, [{:to_string, meta, [expr]}, {:binary, meta, nil}]}
        {:"::", open_meta,
         [
           {{:., open_meta, [Kernel, :to_string]}, call_meta, [expr]},
           {:binary, open_meta, nil}
         ]}

      k when k in [:charlist, :heredoc_charlist] ->
        # Charlist interpolation: just Kernel.to_string
        {{:., open_meta, [Kernel, :to_string]}, call_meta, [expr]}
    end
  end

  defp build_string_error_node(reason, meta, %State{} = state, cursor, children) do
    {line, column, synthetic?} = ErrorHelpers.error_anchor(meta, state, cursor)

    {id, state} = State.next_diagnostic_id(state)

    diagnostic =
      Error.from_parser(nil, reason,
        line_index: state.line_index,
        source: state.source,
        position: {{line, column}, {line, column}}
      )
      |> Error.annotate(%{
        id: id,
        anchor: %{kind: :error_node, path: [], note: nil},
        synthetic?: synthetic?,
        lexer_error_code: nil
      })

    diagnostic = %{diagnostic | details: Map.put(diagnostic.details, :source, :grammar)}
    state = %{state | diagnostics: [diagnostic | state.diagnostics]}

    payload =
      Error.error_node_payload(diagnostic,
        kind: :invalid,
        original: reason,
        children: children,
        synthetic?: synthetic?
      )

    error_meta = ErrorHelpers.build_error_meta(line, column, synthetic?)
    {Builder.Helpers.error(payload, error_meta), state}
  end

  defp build_error_token_node(token, %State{} = state) do
    meta = TokenAdapter.token_meta(token)
    synthetic? = ErrorHelpers.synthetic_meta?(meta, state)

    payload =
      case State.error_token_diagnostic(state, TokenAdapter.meta(token)) do
        %Error{} = diagnostic ->
          Error.error_node_payload(diagnostic,
            kind: :token,
            original: elem(token, 2),
            synthetic?: synthetic?
          )

        _ ->
          elem(token, 2)
      end

    Builder.Helpers.error(payload, meta)
  end

  defp sync_to_string_end(state, cursor, target_ends) do
    case Cursor.peek(cursor) do
      {:ok, {kind, _meta, _value}, cursor} ->
        if kind in target_ends do
          case TokenAdapter.next(state, cursor) do
            {:ok, _tok, state, cursor} -> {state, cursor}
            {:eof, state, cursor} -> {state, cursor}
            {:error, _reason, state, cursor} -> {state, cursor}
          end
        else
          case TokenAdapter.next(state, cursor) do
            {:ok, _tok, state, cursor} -> sync_to_string_end(state, cursor, target_ends)
            {:eof, state, cursor} -> {state, cursor}
            {:error, _reason, state, cursor} -> {state, cursor}
          end
        end

      {:eof, cursor} ->
        {state, cursor}

      {:error, _diag, cursor} ->
        case TokenAdapter.next(state, cursor) do
          {:ok, _tok, state, cursor} -> sync_to_string_end(state, cursor, target_ends)
          {:eof, state, cursor} -> {state, cursor}
          {:error, _reason, state, cursor} -> {state, cursor}
        end
    end
  end

  # Build AST for simple strings
  defp build_string_ast(
         parts,
         kind,
         start_meta,
         delimiter,
         _end_tok,
         state,
         cursor,
         ctx,
         log,
         min_bp,
         opts
       ) do
    if has_interpolations?(parts) do
      args = build_interpolated_parts(parts, kind)

      # Add delimiter to metadata for interpolated strings
      meta_with_delimiter = [{:delimiter, delimiter} | start_meta]

      ast =
        case kind do
          :binary ->
            {:<<>>, meta_with_delimiter, args}

          :charlist ->
            # Charlist with interpolation: List.to_charlist(meta, [parts])
            {{:., start_meta, [List, :to_charlist]}, meta_with_delimiter, [args]}
        end

      Pratt.led(ast, state, cursor, log, min_bp, ctx, opts)
    else
      content = merge_fragments(parts)

      value_result =
        case kind do
          :binary -> {:ok, content}
          :charlist -> safe_to_charlist(content, start_meta)
        end

      case value_result do
        {:ok, value} ->
          # Include delimiter in metadata for literal_encoder (token_metadata compatibility)
          meta_with_delimiter = [{:delimiter, delimiter} | start_meta]
          ast = Builder.Helpers.literal(value, meta_with_delimiter, state)
          Pratt.led(ast, state, cursor, log, min_bp, ctx, opts)

        {:error, reason} ->
          {:error, reason, state, cursor, log}
      end
    end
  end

  # Build AST for heredocs
  # Note: For heredocs, parts are NOT unescaped yet (to allow proper line continuation handling)
  defp build_heredoc_ast(
         parts,
         kind,
         start_meta,
         indentation,
         state,
         cursor,
         ctx,
         log,
         min_bp,
         opts
       ) do
    delimiter =
      case kind do
        :heredoc_binary -> ~s|"""|
        :heredoc_charlist -> ~s|'''|
      end

    meta_with_indent = [{:delimiter, delimiter}, {:indentation, indentation} | start_meta]

    if has_interpolations?(parts) do
      # Unescape fragments now (after indentation trimming)
      case build_interpolated_parts_unescape(parts, kind) do
        {:ok, args} ->
          ast =
            case kind do
              :heredoc_binary ->
                {:<<>>, meta_with_indent, args}

              :heredoc_charlist ->
                # Charlist heredoc with interpolation: List.to_charlist(meta, [parts])
                {{:., start_meta, [List, :to_charlist]}, meta_with_indent, [args]}
            end

          Pratt.led(ast, state, cursor, log, min_bp, ctx, opts)

        {:error, reason} ->
          {:error, {:unescape_error, reason, start_meta}, state, cursor, log}
      end
    else
      # Unescape and merge fragments
      case merge_fragments_unescape(parts) do
        {:ok, content} ->
          value =
            case kind do
              :heredoc_binary -> content
              :heredoc_charlist -> String.to_charlist(content)
            end

          # Include delimiter and indentation in metadata for literal_encoder
          ast = Builder.Helpers.literal(value, meta_with_indent, state)
          Pratt.led(ast, state, cursor, log, min_bp, ctx, opts)

        {:error, reason} ->
          {:error, {:unescape_error, reason, start_meta}, state, cursor, log}
      end
    end
  end

  # Build list of parts for interpolated string/heredoc (no unescaping needed)
  defp build_interpolated_parts(parts, _kind) do
    for part <- parts do
      case part do
        {:fragment, content} -> content
        {:interpolation, ast} -> ast
      end
    end
  end

  # Build list of parts for heredoc with interpolation (with unescaping)
  # Returns {:ok, parts} or {:error, reason}
  defp build_interpolated_parts_unescape(parts, _kind) do
    result =
      Enum.reduce_while(parts, {:ok, []}, fn part, {:ok, acc} ->
        case part do
          {:fragment, content} ->
            case safe_unescape(content) do
              {:ok, unescaped} -> {:cont, {:ok, [unescaped | acc]}}
              {:error, reason} -> {:halt, {:error, reason}}
            end

          {:interpolation, ast} ->
            {:cont, {:ok, [ast | acc]}}
        end
      end)

    case result do
      {:ok, reversed} -> {:ok, Enum.reverse(reversed)}
      {:error, reason} -> {:error, reason}
    end
  end

  # Merge all fragments into a single string
  defp merge_fragments(parts) do
    parts
    |> Enum.filter(fn
      {:fragment, _} -> true
      _ -> false
    end)
    |> Enum.map(fn {:fragment, content} -> content end)
    |> Enum.join("")
  end

  # Merge all fragments into a single string with unescaping (for heredocs)
  # Returns {:ok, string} or {:error, reason}
  defp merge_fragments_unescape(parts) do
    fragments =
      parts
      |> Enum.filter(fn
        {:fragment, _} -> true
        _ -> false
      end)

    result =
      Enum.reduce_while(fragments, {:ok, []}, fn {:fragment, content}, {:ok, acc} ->
        case safe_unescape(content) do
          {:ok, unescaped} -> {:cont, {:ok, [unescaped | acc]}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)

    case result do
      {:ok, reversed} -> {:ok, reversed |> Enum.reverse() |> Enum.join("")}
      {:error, reason} -> {:error, reason}
    end
  end

  defp has_interpolations?(parts) do
    Enum.any?(parts, fn
      {:interpolation, _} -> true
      _ -> false
    end)
  end

  # Extract indentation from heredoc end token
  defp get_heredoc_indentation(tok) do
    # heredoc_end is a 3-tuple: {:bin_heredoc_end, meta, {delimiter, indent}}
    case tok do
      {_kind, _meta, {_delim, indentation}} when is_integer(indentation) -> indentation
      _ -> 0
    end
  end

  # Extract indentation from sigil end token (for heredoc sigils)
  # Returns nil for non-heredoc sigils, integer for heredoc sigils
  defp get_sigil_indentation(tok) do
    # sigil_end is a 3-tuple: {:sigil_end, meta, {delim, indent}}
    # Note: indentation of 0 is valid for heredoc sigils and should be included in metadata
    case tok do
      {_kind, _meta, {_delim, indentation}} when is_integer(indentation) and indentation >= 0 ->
        indentation

      _ ->
        nil
    end
  end

  # Build parts for sigil content - always returns list for {:<<>>, meta, parts}
  defp build_sigil_parts([]), do: [""]

  defp build_sigil_parts(parts) do
    for part <- parts do
      case part do
        {:fragment, content} -> content
        {:interpolation, ast} -> ast
      end
    end
  end

  # Trim heredoc parts based on indentation
  defp trim_heredoc_parts(parts, indentation, _opts) when indentation <= 0, do: parts

  defp trim_heredoc_parts(parts, indentation, opts) do
    line_continuation? = Keyword.get(opts, :line_continuation?, true)

    {trimmed, _at_line_start, _spaces_left} =
      trim_parts_loop(parts, indentation, true, indentation, [], line_continuation?)

    Enum.reverse(trimmed)
  end

  defp trim_parts_loop(
         [{:fragment, content} | rest],
         indent,
         at_line_start,
         spaces_left,
         acc,
         line_continuation?
       ) do
    {trimmed, new_at_line_start, new_spaces_left} =
      trim_fragment(content, indent, at_line_start, spaces_left, line_continuation?)

    trim_parts_loop(
      rest,
      indent,
      new_at_line_start,
      new_spaces_left,
      [
        {:fragment, trimmed} | acc
      ],
      line_continuation?
    )
  end

  defp trim_parts_loop(
         [{:interpolation, ast} | rest],
         indent,
         _at_line_start,
         _spaces_left,
         acc,
         line_continuation?
       ) do
    # Interpolation counts as content on this line; no more trimming after it
    trim_parts_loop(rest, indent, false, 0, [{:interpolation, ast} | acc], line_continuation?)
  end

  defp trim_parts_loop([], _indent, at_line_start, spaces_left, acc, _line_continuation?) do
    {acc, at_line_start, spaces_left}
  end

  # Trim indentation from a fragment
  defp trim_fragment(content, indent, at_line_start, spaces_left, line_continuation?) do
    chars = :binary.bin_to_list(content)

    {rev_chars, final_at_line_start, final_spaces_left} =
      trim_chars(chars, indent, at_line_start, spaces_left, [], line_continuation?)

    trimmed = rev_chars |> Enum.reverse() |> :erlang.list_to_binary()
    {trimmed, final_at_line_start, final_spaces_left}
  end

  defp trim_chars([?\\, ?\n | rest], indent, _at_line_start, _spaces_left, acc, true) do
    # Line continuation (backslash-newline) should only trigger when the backslash is not itself
    # escaped by a preceding backslash. For example:
    # - "\\\n"   => continuation (newline removed)
    # - "\\\\\n" => escaped backslash + newline preserved
    if escaped_by_preceding_backslash?(acc) do
      # Preserve the backslash + newline; newline resets indentation trimming.
      trim_chars(rest, indent, true, indent, [?\n, ?\\ | acc], true)
    else
      # Continuation: remove both and strip following indentation.
      trim_chars(rest, indent, true, indent, acc, true)
    end
  end

  defp trim_chars([?\\, ?\r, ?\n | rest], indent, _at_line_start, _spaces_left, acc, true) do
    # Same as above but for CRLF.
    if escaped_by_preceding_backslash?(acc) do
      trim_chars(rest, indent, true, indent, [?\n, ?\r, ?\\ | acc], true)
    else
      trim_chars(rest, indent, true, indent, acc, true)
    end
  end

  defp trim_chars([?\\, ?\n | rest], indent, at_line_start, spaces_left, acc, false) do
    # In sigil heredocs, backslash-newline is preserved.
    trim_chars([?\n | rest], indent, at_line_start, spaces_left, [?\\ | acc], false)
  end

  defp trim_chars([?\n | rest], indent, _at_line_start, _spaces_left, acc, line_continuation?) do
    # Newline resets indentation trimming
    trim_chars(rest, indent, true, indent, [?\n | acc], line_continuation?)
  end

  defp trim_chars([ch | rest], indent, true, spaces_left, acc, line_continuation?)
       when spaces_left > 0 and (ch == ?\s or ch == ?\t) do
    # Trim spaces/tabs at start of line
    trim_chars(rest, indent, true, spaces_left - 1, acc, line_continuation?)
  end

  defp trim_chars([ch | rest], indent, _at_line_start, spaces_left, acc, line_continuation?) do
    # Regular character - keep and mark no longer at line start
    trim_chars(rest, indent, false, spaces_left, [ch | acc], line_continuation?)
  end

  defp trim_chars([], _indent, at_line_start, spaces_left, acc, _line_continuation?) do
    {acc, at_line_start, spaces_left}
  end

  defp escaped_by_preceding_backslash?(acc) when is_list(acc) do
    # `acc` is reversed; leading backslashes in `acc` correspond to immediately-preceding backslashes.
    # If we have an odd number of preceding backslashes, the current backslash is escaped.
    rem(count_leading_backslashes(acc), 2) == 1
  end

  defp count_leading_backslashes([?\\ | rest]), do: 1 + count_leading_backslashes(rest)
  defp count_leading_backslashes(_), do: 0

  # Unescape string escape sequences like \n, \t, etc.
  # Returns {:ok, string} or {:error, reason}
  defp safe_unescape(string) do
    {:ok, :elixir_interpolation.unescape_string(string)}
  rescue
    e in ArgumentError ->
      {:error, Exception.message(e)}
  end

  defp safe_to_charlist(string, meta) do
    {:ok, String.to_charlist(string)}
  rescue
    e in UnicodeConversionError ->
      message = Exception.message(e)
      message = if is_list(message), do: List.to_string(message), else: message
      {:error, {meta, message, "'"}}
  end

  defp string_kind(:bin_string_start), do: :binary
  defp string_kind(:list_string_start), do: :charlist
  defp string_kind(:bin_heredoc_start), do: :heredoc_binary
  defp string_kind(:list_heredoc_start), do: :heredoc_charlist

  defp string_delimiter(:bin_string_start), do: "\""
  defp string_delimiter(:list_string_start), do: "'"

  # Returns list of possible end tokens for a string start
  defp closing_for(:bin_string_start),
    do: [:bin_string_end, :kw_identifier_unsafe_end, :kw_identifier_safe_end]

  defp closing_for(:list_string_start),
    do: [:list_string_end, :kw_identifier_unsafe_end, :kw_identifier_safe_end]

  defp closing_for(:bin_heredoc_start), do: [:bin_heredoc_end]
  defp closing_for(:list_heredoc_start), do: [:list_heredoc_end]
end
