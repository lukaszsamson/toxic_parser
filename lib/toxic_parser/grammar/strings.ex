defmodule ToxicParser.Grammar.Strings do
  @moduledoc """
  Parsing for strings, charlists, heredocs, sigils, and quoted atoms from Toxic token streams.
  """

  alias ToxicParser.{Builder, Context, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.Expressions

  @simple_string_start [:bin_string_start, :list_string_start]
  @heredoc_start [:bin_heredoc_start, :list_heredoc_start]
  @sigil_start [:sigil_start]
  @atom_start [:atom_unsafe_start, :atom_safe_start]
  # TODO: atom_safe_start

  # Max atom length is 255 bytes
  @max_atom_length 255

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}
          | {:no_string, State.t()}
          | {:keyword_key, atom(), keyword(), State.t(), EventLog.t()}
          | {:keyword_key_interpolated, list(), atom(), keyword(), String.t(), State.t(),
             EventLog.t()}

  @spec parse(State.t(), Pratt.context(), EventLog.t(), non_neg_integer()) :: result()
  def parse(%State{} = state, %Context{} = ctx, %EventLog{} = log, min_bp \\ 0, opts \\ []) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: kind}, _} when kind in @simple_string_start ->
        parse_simple_string(state, ctx, log, min_bp, opts)

      {:ok, %{kind: kind}, _} when kind in @heredoc_start ->
        parse_heredoc(state, ctx, log, min_bp, opts)

      {:ok, %{kind: kind}, _} when kind in @sigil_start ->
        parse_sigil(state, ctx, log, min_bp, opts)

      {:ok, %{kind: kind}, _} when kind in @atom_start ->
        parse_quoted_atom(state, ctx, kind, log, min_bp, opts)

      {:ok, _tok, _} ->
        {:no_string, state}

      {:eof, state} ->
        {:no_string, state}

      {:error, _diag, state} ->
        {:no_string, state}
    end
  end

  # Parse simple strings (not heredocs)
  defp parse_simple_string(state, ctx, log, min_bp, opts) do
    {:ok, start_tok, state} = TokenAdapter.next(state)
    start_meta = token_to_meta(start_tok.metadata)
    kind = string_kind(start_tok.kind)
    target_ends = closing_for(start_tok.kind)

    # Get delimiter for metadata
    delimiter = string_delimiter(start_tok.kind)

    with {:ok, parts, actual_end, end_tok, state, log} <-
           collect_parts([], state, target_ends, kind, log),
         {:ok, _close, state} <- TokenAdapter.next(state) do
      # If string ended with kw_identifier_unsafe_end, it's a keyword key (atom)
      if actual_end in [:kw_identifier_unsafe_end, :kw_identifier_safe_end] do
        # Check if there's any interpolation in the parts
        if has_interpolations?(parts) do
          # Build binary_to_atom call for interpolated keyword
          {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, log}
        else
          content = merge_fragments(parts)

          if byte_size(content) > @max_atom_length do
            {:error, {:atom_too_long, content, start_tok.metadata}, state, log}
          else
            atom = String.to_atom(content)
            {:keyword_key, atom, start_meta, state, log}
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
          ctx,
          log,
          min_bp,
          opts
        )
      end
    end
  end

  # Parse heredocs with indentation handling
  defp parse_heredoc(state, ctx, log, min_bp, opts) do
    {:ok, start_tok, state} = TokenAdapter.next(state)
    start_meta = token_to_meta(start_tok.metadata)
    kind = string_kind(start_tok.kind)
    target_ends = closing_for(start_tok.kind)

    with {:ok, parts, _actual_end, end_tok, state, log} <-
           collect_parts([], state, target_ends, kind, log),
         {:ok, _close, state} <- TokenAdapter.next(state) do
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
        ctx,
        log,
        min_bp,
        opts
      )
    end
  end

  defp parse_sigil(state, ctx, log, min_bp, opts) do
    {:ok, start_tok, state} = TokenAdapter.next(state)
    start_meta = token_to_meta(start_tok.metadata)
    {sigil, delimiter} = start_tok.value

    with {:ok, parts, _actual_end, end_tok, state, log} <-
           collect_parts([], state, [:sigil_end], :sigil, log),
         {:ok, _close, state} <- TokenAdapter.next(state) do
      # Check for sigil_modifiers token
      {modifiers, state} =
        case TokenAdapter.peek(state) do
          {:ok, %{kind: :sigil_modifiers, value: mods}, _} ->
            {:ok, _mod_tok, state} = TokenAdapter.next(state)
            {mods, state}

          _ ->
            {[], state}
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
      Pratt.led(ast, state, log, min_bp, ctx, opts)
    end
  end

  # Parse quoted atoms: :"foo", :"foo bar", :""
  defp parse_quoted_atom(state, ctx, kind, log, min_bp, opts) do
    {:ok, start_tok, state} = TokenAdapter.next(state)
    start_meta = token_to_meta(start_tok.metadata)
    # Use delimiter from token value (39 = ', 34 = ")
    delimiter = if start_tok.value == 39, do: "'", else: "\""
    meta_with_delimiter = [{:delimiter, delimiter} | start_meta]

    end_token_kind =
      if kind == :atom_unsafe_start do
        :atom_unsafe_end
      else
        :atom_safe_end
      end

    with {:ok, parts, _actual_end, _end_tok, state, log} <-
           collect_parts([], state, [end_token_kind], :atom, log),
         {:ok, _close, state} <- TokenAdapter.next(state) do
      # Check if there are any interpolations
      if has_interpolations?(parts) do
        # Build interpolated atom AST with delimiter metadata
        args = build_interpolated_parts(parts, :atom)

        ast =
          {{:., start_meta, [:erlang, :binary_to_atom]}, meta_with_delimiter,
           [{:<<>>, start_meta, args}, :utf8]}

        Pratt.led(ast, state, log, min_bp, ctx, opts)
      else
        content = merge_fragments(parts)

        if byte_size(content) > @max_atom_length do
          {:error, {:atom_too_long, content, start_tok.metadata}, state, log}
        else
          atom = String.to_atom(content)
          # Include delimiter in metadata for literal_encoder
          ast = Builder.Helpers.literal(atom, meta_with_delimiter, state)
          Pratt.led(ast, state, log, min_bp, ctx, opts)
        end
      end
    end
  end

  # Collect parts (fragments and interpolations)
  # Returns {:ok, parts, actual_end_kind, end_token, state, log}
  # For sigils, do not unescape content (no_unescape: true)
  defp collect_parts(acc, state, target_ends, kind, log) do
    # Sigils should not unescape content
    # Heredocs should not unescape here - unescaping happens after indentation trimming
    # (because line continuation \\\n affects indentation stripping)
    should_unescape = kind not in [:sigil, :heredoc_binary, :heredoc_charlist]

    case TokenAdapter.peek(state) do
      {:ok, %{kind: end_kind} = end_tok, _} ->
        if end_kind in target_ends do
          {:ok, Enum.reverse(acc), end_kind, end_tok, state, log}
        else
          case end_kind do
            :string_fragment ->
              {:ok, %{value: fragment} = frag_tok, state} = TokenAdapter.next(state)

              if should_unescape do
                case safe_unescape(fragment) do
                  {:ok, content} ->
                    collect_parts([{:fragment, content} | acc], state, target_ends, kind, log)

                  {:error, reason} ->
                    {:error, {:unescape_error, reason, frag_tok.metadata}, state, log}
                end
              else
                collect_parts([{:fragment, fragment} | acc], state, target_ends, kind, log)
              end

            :begin_interpolation ->
              with {:ok, interp_ast, state, log} <- parse_interpolation(state, kind, log) do
                collect_parts([{:interpolation, interp_ast} | acc], state, target_ends, kind, log)
              end

            _ ->
              {:error, {:unexpected_string_token, end_kind}, state, log}
          end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse an interpolation: #{expr}
  defp parse_interpolation(state, kind, log) do
    {:ok, begin_tok, state} = TokenAdapter.next(state)
    open_meta = token_to_meta(begin_tok.metadata)

    # Check for empty interpolation #{} first
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :end_interpolation} = end_tok, _} ->
        # Empty interpolation - use empty block
        {:ok, _end, state} = TokenAdapter.next(state)
        close_meta = token_to_meta(end_tok.metadata)
        empty_block = {:__block__, [], []}
        interp_ast = build_interpolation_ast(empty_block, open_meta, close_meta, kind)
        {:ok, interp_ast, state, log}

      {:ok, %{kind: :eoe} = eoe_tok, _} ->
        # Interpolation starting with EOE (e.g., #{;})
        # In Elixir grammar: grammar -> eoe : {'__block__', meta_from_token('$1'), []}.
        {:ok, _eoe, state} = TokenAdapter.next(state)
        eoe_meta = token_to_meta(eoe_tok.metadata)
        # Skip any additional EOE
        alias ToxicParser.Grammar.EOE
        state = EOE.skip(state)

        case TokenAdapter.peek(state) do
          {:ok, %{kind: :end_interpolation} = end_tok, _} ->
            # Just EOE before close - empty block with EOE metadata
            {:ok, _end, state} = TokenAdapter.next(state)
            close_meta = token_to_meta(end_tok.metadata)
            empty_block = {:__block__, eoe_meta, []}
            interp_ast = build_interpolation_ast(empty_block, open_meta, close_meta, kind)
            {:ok, interp_ast, state, log}

          {:ok, _, _} ->
            # EOE followed by content - parse the content as expr_list
            case Expressions.expr_list(state, Context.expr(), log) do
              {:ok, expr, state, log} ->
                case TokenAdapter.peek(state) do
                  {:ok, %{kind: :end_interpolation} = end_tok, _} ->
                    {:ok, _end, state} = TokenAdapter.next(state)
                    close_meta = token_to_meta(end_tok.metadata)
                    interp_ast = build_interpolation_ast(expr, open_meta, close_meta, kind)
                    {:ok, interp_ast, state, log}

                  {:ok, tok, _} ->
                    {:error, {:expected_end_interpolation, tok.kind}, state, log}

                  {:eof, state} ->
                    {:error, :unexpected_eof, state, log}

                  {:error, diag, state} ->
                    {:error, diag, state, log}
                end

              {:error, reason, state, log} ->
                {:error, reason, state, log}
            end

          {:eof, state} ->
            {:error, :unexpected_eof, state, log}

          {:error, diag, state} ->
            {:error, diag, state, log}
        end

      _ ->
        # Parse the expression inside interpolation
        # Use :unmatched context so do-blocks are consumed by inner calls
        # (e.g., "foo#{K.'a' do :ok end}bar" - the do belongs to the 'a' call)
        case Expressions.expr_list(state, Context.expr(), log) do
          {:ok, expr, state, log} ->
            # Consume end_interpolation
            case TokenAdapter.peek(state) do
              {:ok, %{kind: :end_interpolation} = end_tok, _} ->
                {:ok, _end, state} = TokenAdapter.next(state)
                close_meta = token_to_meta(end_tok.metadata)

                # Build interpolation AST based on kind
                interp_ast = build_interpolation_ast(expr, open_meta, close_meta, kind)
                {:ok, interp_ast, state, log}

              {:ok, tok, _} ->
                {:error, {:expected_end_interpolation, tok.kind}, state, log}

              {:eof, state} ->
                {:error, :unexpected_eof, state, log}

              {:error, diag, state} ->
                {:error, diag, state, log}
            end

          {:error, reason, state, log} ->
            {:error, reason, state, log}
        end
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

  # Build AST for simple strings
  defp build_string_ast(
         parts,
         kind,
         start_meta,
         delimiter,
         _end_tok,
         state,
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

      Pratt.led(ast, state, log, min_bp, ctx, opts)
    else
      content = merge_fragments(parts)

      value =
        case kind do
          :binary -> content
          :charlist -> String.to_charlist(content)
        end

      # Include delimiter in metadata for literal_encoder (token_metadata compatibility)
      meta_with_delimiter = [{:delimiter, delimiter} | start_meta]
      ast = Builder.Helpers.literal(value, meta_with_delimiter, state)
      Pratt.led(ast, state, log, min_bp, ctx, opts)
    end
  end

  # Build AST for heredocs
  # Note: For heredocs, parts are NOT unescaped yet (to allow proper line continuation handling)
  defp build_heredoc_ast(parts, kind, start_meta, indentation, state, ctx, log, min_bp, opts) do
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

          Pratt.led(ast, state, log, min_bp, ctx, opts)

        {:error, reason} ->
          {:error, {:unescape_error, reason, start_meta}, state, log}
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
          Pratt.led(ast, state, log, min_bp, ctx, opts)

        {:error, reason} ->
          {:error, {:unescape_error, reason, start_meta}, state, log}
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
  defp get_heredoc_indentation(%{value: {_delim, indentation}}) when is_integer(indentation) do
    indentation
  end

  defp get_heredoc_indentation(_), do: 0

  # Extract indentation from sigil end token (for heredoc sigils)
  # Returns nil for non-heredoc sigils, integer for heredoc sigils
  defp get_sigil_indentation(%{value: {_delim, indentation}}) when is_integer(indentation) do
    indentation
  end

  defp get_sigil_indentation(_), do: nil

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

  defp token_to_meta(%{range: %{start: %{line: line, column: column}}}) do
    [line: line, column: column]
  end

  defp token_to_meta(_), do: []

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
