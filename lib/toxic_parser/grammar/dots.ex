defmodule ToxicParser.Grammar.Dots do
  @moduledoc """
  Dot expression parsing helpers (`foo.bar`, `Foo.Bar`, `foo.(...)`, `foo.()`).
  """

  alias ToxicParser.{Builder, EventLog, Identifiers, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.{Expressions, Keywords}

  # Check if an expression result is a keyword list (from quoted keyword parsing)
  defguardp is_keyword_list_result(arg)
            when is_list(arg) and length(arg) > 0

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @doc """
  Parse a dot chain starting from an existing left-hand AST.
  Expects the current peek token to be a dot operator.
  """
  @spec parse_chain(Macro.t(), State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_chain(left, %State{} = state, ctx, %EventLog{} = log) do
    {:ok, _dot, state} = TokenAdapter.next(state)

    with {:ok, rhs, state, log} <- parse_member(state, ctx, log) do
      combined = Builder.Helpers.dot(left, rhs)

      case TokenAdapter.peek(state) do
        {:ok, %{kind: :dot_op}, _} ->
          parse_chain(combined, state, ctx, log)

        _ ->
          {:ok, combined, state, log}
      end
    end
  end

  @doc """
  Parses the RHS of a dot: identifier/alias/call/bracket/paren call.
  Returns `{:ok, {member_value, member_meta}, state, log}` for simple identifiers,
  or `{:ok, call_ast, state, log}` for calls.

  For simple identifiers, also returns `:no_parens_call` flag when the identifier
  was classified as :op_identifier or :dot_op_identifier, indicating that a
  no-parens call is expected.
  """
  @spec parse_member(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_member(%State{} = state, ctx, %EventLog{} = log) do
    case TokenAdapter.next(state) do
      {:ok, tok, state} ->
        case Identifiers.classify(tok.kind) do
          kind
          when kind in [
                 :identifier,
                 :do_identifier,
                 :dot_identifier,
                 :dot_do_identifier,
                 :bracket_identifier
               ] ->
            # Return {member_atom, member_meta} tuple so caller can build proper AST
            {:ok, {tok.value, Builder.Helpers.token_meta(tok.metadata)}, state, log}

          # op_identifier or dot_op_identifier indicates no-parens call is expected
          kind when kind in [:op_identifier, :dot_op_identifier] ->
            # Return with :no_parens_call tag to indicate caller should expect no-parens args
            {:ok, {tok.value, Builder.Helpers.token_meta(tok.metadata), :no_parens_call}, state,
             log}

          :alias ->
            # Alias needs to be wrapped as __aliases__
            {:ok, Builder.Helpers.from_token(tok), state, log}

          kind when kind in [:paren_identifier, :dot_call_identifier, :dot_paren_identifier] ->
            parse_paren_call(tok, state, ctx, log)

          _ ->
            # Check for quoted identifier: D."foo"
            if tok.kind == :quoted_identifier_start do
              parse_quoted_identifier(tok, state, ctx, log)
            else
              {:error, {:expected, :dot_member, got: tok.kind}, state, log}
            end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_paren_call(tok, state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    # Skip leading EOE and count newlines
    {state, leading_newlines} = skip_eoe_count_newlines(state, 0)

    with {:ok, args, state, log} <- parse_paren_args([], state, ctx, log) do
      # Skip trailing EOE before close paren
      {state, trailing_newlines} = skip_eoe_count_newlines(state, 0)

      case TokenAdapter.next(state) do
        {:ok, %{kind: :")"} = close_tok, state} ->
          # Only count trailing newlines for empty args
          # (newlines metadata represents leading newlines inside parens)
          total_newlines =
            if args == [] do
              leading_newlines + trailing_newlines
            else
              leading_newlines
            end

          callee_meta = Builder.Helpers.token_meta(tok.metadata)
          close_meta = Builder.Helpers.token_meta(close_tok.metadata)

          # Build metadata: [newlines: N, closing: [...], line: L, column: C]
          newlines_meta = if total_newlines > 0, do: [newlines: total_newlines], else: []
          meta = newlines_meta ++ [closing: close_meta] ++ callee_meta

          callee = Builder.Helpers.from_token(tok)
          {:ok, {callee, meta, Enum.reverse(args)}, state, log}

        {:ok, other, state} ->
          {:error, {:expected, :")", got: other.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state}

        {:error, diag, state} ->
          {:error, diag, state}
      end
    end
  end

  # Parse quoted identifier: D."foo" or D."foo"() or D."foo" arg (no-parens)
  # Token sequence: quoted_identifier_start -> string_fragment* -> quoted_identifier_end/quoted_op_identifier_end
  defp parse_quoted_identifier(start_tok, state, ctx, log) do
    start_meta = Builder.Helpers.token_meta(start_tok.metadata)
    delimiter = delimiter_from_value(start_tok.value)

    with {:ok, fragments, end_kind, state, log} <-
           collect_fragments([], state, :quoted_identifier_end, log),
         {:ok, _close, state} <- TokenAdapter.next(state) do
      content = fragments |> Enum.reverse() |> Enum.join("") |> Macro.unescape_string()
      atom = String.to_atom(content)

      # Build metadata with delimiter
      meta_with_delimiter = [{:delimiter, delimiter} | start_meta]

      cond do
        # quoted_op_identifier_end means there's a space before next token -> no-parens call
        end_kind == :quoted_op_identifier_end ->
          # D."foo" arg - parse no-parens call arguments
          with {:ok, args, state, log} <-
                 ToxicParser.Grammar.Calls.parse_no_parens_args([], state, ctx, log) do
            # Return as call AST: {atom, meta, args}
            {:ok, {atom, meta_with_delimiter, args}, state, log}
          end

        # quoted_bracket_identifier_end: D."foo"[1] - return simple identifier, bracket access handled by caller
        end_kind == :quoted_bracket_identifier_end ->
          {:ok, {atom, meta_with_delimiter}, state, log}

        # quoted_do_identifier_end: D."foo" do...end - return as call AST, do-block parsed by caller
        end_kind == :quoted_do_identifier_end ->
          # Return as call expression {atom, meta, []} so caller can attach do-block
          {:ok, {atom, meta_with_delimiter, []}, state, log}

        # quoted_paren_identifier_end or ( immediately follows: D."foo"()
        end_kind == :quoted_paren_identifier_end or
            match?({:ok, %{kind: :"("}, _}, TokenAdapter.peek(state)) ->
          {:ok, _open, state} = TokenAdapter.next(state)

          # Skip leading EOE and count newlines
          {state, leading_newlines} = skip_eoe_count_newlines(state, 0)

          with {:ok, args, state, log} <- parse_paren_args([], state, :matched, log) do
            # Skip trailing EOE before close paren
            {state, trailing_newlines} = skip_eoe_count_newlines(state, 0)

            case TokenAdapter.next(state) do
              {:ok, %{kind: :")"} = close_tok, state} ->
                total_newlines = leading_newlines + trailing_newlines
                close_meta = Builder.Helpers.token_meta(close_tok.metadata)

                newlines_meta = if total_newlines > 0, do: [newlines: total_newlines], else: []
                call_meta = newlines_meta ++ [closing: close_meta] ++ meta_with_delimiter

                # Return as call AST: {atom, call_meta, args}
                {:ok, {atom, call_meta, Enum.reverse(args)}, state, log}

              {:ok, other, state} ->
                {:error, {:expected, :")", got: other.kind}, state, log}

              {:eof, state} ->
                {:error, :unexpected_eof, state, log}

              {:error, diag, state} ->
                {:error, diag, state, log}
            end
          end

        true ->
          # Simple quoted identifier without parens
          {:ok, {atom, meta_with_delimiter}, state, log}
      end
    end
  end

  # Possible end tokens for quoted identifiers
  # - quoted_identifier_end: D."foo" (no following token)
  # - quoted_op_identifier_end: D."foo" arg (space before next token - no-parens call)
  # - quoted_paren_identifier_end: D."foo"() (immediately followed by parens)
  # - quoted_bracket_identifier_end: D."foo"[1] (immediately followed by bracket)
  # - quoted_do_identifier_end: D."foo" do...end (space before do-block)
  @quoted_id_ends [
    :quoted_identifier_end,
    :quoted_op_identifier_end,
    :quoted_paren_identifier_end,
    :quoted_bracket_identifier_end,
    :quoted_do_identifier_end
  ]

  defp collect_fragments(acc, state, target_end, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: ^target_end}, _} ->
        {:ok, acc, target_end, state, log}

      {:ok, %{kind: kind}, _}
      when kind in @quoted_id_ends and target_end == :quoted_identifier_end ->
        # Accept any quoted identifier end token
        {:ok, acc, kind, state, log}

      {:ok, %{kind: :string_fragment, value: fragment}, _} ->
        {:ok, _frag, state} = TokenAdapter.next(state)
        collect_fragments([fragment | acc], state, target_end, log)

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}

      {:ok, token, _} ->
        {:error, {:unexpected_string_token, token.kind}, state, log}
    end
  end

  defp delimiter_from_value(34), do: "\""
  defp delimiter_from_value(?'), do: "'"
  defp delimiter_from_value(_), do: "\""

  defp skip_eoe_count_newlines(state, count) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, value: %{newlines: n}}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe_count_newlines(state, count + n)

      _ ->
        {state, count}
    end
  end

  defp parse_paren_args(acc, state, _ctx, log) do
    # Skip EOE before checking for close paren or next arg
    state = skip_eoe(state)

    case TokenAdapter.peek(state) do
      {:ok, %{kind: :")"}, _} ->
        {:ok, acc, state, log}

      {:ok, tok, _} ->
        cond do
          Keywords.starts_kw?(tok) ->
            # Keyword argument like foo: 1 - parse entire keyword list
            with {:ok, kw_list, state, log} <- Keywords.parse_kw_call(state, :unmatched, log) do
              {:ok, [kw_list | acc], state, log}
            end

          true ->
            # Inside parentheses, use :unmatched context so do_identifiers (case, if, etc.)
            # can consume their do-blocks. This is correct because when inside parens,
            # the do-block clearly belongs to the inner expression, not an outer call.
            # e.g., foo(case a do x -> y end) - the do belongs to case, not foo
            with {:ok, arg, state, log} <- Expressions.expr(state, :unmatched, log) do
              # Skip EOE after arg before checking for comma
              state = skip_eoe(state)

              case TokenAdapter.peek(state) do
                {:ok, %{kind: :","}, _} ->
                  {:ok, _comma, state} = TokenAdapter.next(state)
                  # Check if arg was a keyword list from quoted key parsing (e.g., "foo": 1)
                  # If so, and next is also a keyword, merge them
                  state = skip_eoe(state)

                  case TokenAdapter.peek(state) do
                    {:ok, next_tok, _} when is_keyword_list_result(arg) ->
                      if Keywords.starts_kw?(next_tok) do
                        # Continue collecting keywords into this list
                        with {:ok, more_kw, state, log} <-
                               Keywords.parse_kw_call(state, :unmatched, log) do
                          merged_kw = arg ++ more_kw
                          {:ok, [merged_kw | acc], state, log}
                        end
                      else
                        parse_paren_args([arg | acc], state, :unmatched, log)
                      end

                    _ ->
                      parse_paren_args([arg | acc], state, :unmatched, log)
                  end

                _ ->
                  # No comma - we're done with args
                  {:ok, [arg | acc], state, log}
              end
            end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp skip_eoe(state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe(state)

      _ ->
        state
    end
  end
end
