defmodule ToxicParser.Grammar.Dots do
  @moduledoc """
  Dot expression parsing helpers (`foo.bar`, `Foo.Bar`, `foo.(...)`, `foo.()`).
  """

  alias ToxicParser.{Builder, EventLog, Identifiers, Pratt, Result, State, TokenAdapter}
  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.{EOE, Expressions, Keywords}

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
  Parse a dot call `expr.(...)` when the current token is `:dot_call_op`.
  """
  @spec parse_dot_call(Macro.t(), State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_dot_call(left, %State{} = state, ctx, %EventLog{} = log) do
    {:ok, dot_tok, state} = TokenAdapter.next(state)
    dot_meta = Builder.Helpers.token_meta(dot_tok.metadata)

    # dot_call_op is immediately followed by (
    {:ok, _open, state} = TokenAdapter.next(state)

    # Skip leading EOE and count newlines
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    with {:ok, args, state, log} <- parse_paren_args([], state, ctx, log),
         {:ok, close_meta, trailing_newlines, state} <- Meta.consume_closing(state, :")") do
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      call_meta = Meta.closing_meta(dot_meta, close_meta, total_newlines)

      {:ok, {{:., dot_meta, [left]}, call_meta, Enum.reverse(args)}, state, log}
    else
      other -> Result.normalize_error(other, log)
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
                 :dot_do_identifier
               ] ->
            # Return {member_atom, member_meta} tuple so caller can build proper AST
            # Regular identifier - bracket access NOT allowed (whitespace before [)
            {:ok, {tok.value, Builder.Helpers.token_meta(tok.metadata)}, state, log}

          :bracket_identifier ->
            # Bracket identifier - bracket access IS allowed (no whitespace before [)
            {:ok, {tok.value, Builder.Helpers.token_meta(tok.metadata), :allows_bracket}, state,
             log}

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
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    with {:ok, args, state, log} <- parse_paren_args([], state, ctx, log),
         {:ok, close_meta, trailing_newlines, state} <- Meta.consume_closing(state, :")") do
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      callee_meta = Builder.Helpers.token_meta(tok.metadata)
      meta = Meta.closing_meta(callee_meta, close_meta, total_newlines)

      callee = Builder.Helpers.from_token(tok)
      {:ok, {callee, meta, Enum.reverse(args)}, state, log}
    else
      other -> Result.normalize_error(other, log)
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

        # quoted_bracket_identifier_end: D."foo"[1] - return with :allows_bracket flag
        # so caller knows bracket access is allowed
        end_kind == :quoted_bracket_identifier_end ->
          {:ok, {atom, meta_with_delimiter, :allows_bracket}, state, log}

        # quoted_do_identifier_end: D."foo" do...end - return as call AST, do-block parsed by caller
        end_kind == :quoted_do_identifier_end ->
          # Return as call expression {atom, meta, []} so caller can attach do-block
          {:ok, {atom, meta_with_delimiter, []}, state, log}

        # quoted_paren_identifier_end or ( immediately follows: D."foo"()
        end_kind == :quoted_paren_identifier_end or
            match?({:ok, %{kind: :"("}, _}, TokenAdapter.peek(state)) ->
          {:ok, _open, state} = TokenAdapter.next(state)

          # Skip leading EOE and count newlines
          {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

          with {:ok, args, state, log} <- parse_paren_args([], state, :matched, log),
               {:ok, close_meta, trailing_newlines, state} <-
                 Meta.consume_closing(state, :")") do
            total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, true)
            call_meta = Meta.closing_meta(meta_with_delimiter, close_meta, total_newlines)

            # Return as call AST: {atom, call_meta, args}
            {:ok, {atom, call_meta, Enum.reverse(args)}, state, log}
          else
            other -> Result.normalize_error(other, log)
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

  defp delimiter_from_value(?"), do: "\""
  defp delimiter_from_value(?'), do: "'"
  defp delimiter_from_value(_), do: "\""

  defp parse_paren_args(acc, state, _ctx, log) do
    # Skip EOE before checking for close paren or next arg
    state = EOE.skip(state)

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
            # Check if this argument starts with [ or { - if so, it's a container literal
            # and should NOT be merged with following keywords even if it's a keyword list
            is_container_literal =
              case tok.kind do
                # NOTE: only [ and ( seem relevant here
                kind when kind in [:"[", :"<<", :"(", :"{"] -> true
                _ -> false
              end

            # Inside parentheses, use :unmatched context so do_identifiers (case, if, etc.)
            # can consume their do-blocks. This is correct because when inside parens,
            # the do-block clearly belongs to the inner expression, not an outer call.
            # e.g., foo(case a do x -> y end) - the do belongs to case, not foo
            with {:ok, arg, state, log} <- Expressions.expr(state, :unmatched, log) do
              # Skip EOE after arg before checking for comma
              state = EOE.skip(state)

              case TokenAdapter.peek(state) do
                {:ok, %{kind: :","}, _} ->
                  {:ok, _comma, state} = TokenAdapter.next(state)
                  # Check if arg was a keyword list from quoted key parsing (e.g., "foo": 1)
                  # If so, and next is also a keyword, merge them
                  # BUT: if it started as a container literal ([...] or {...}), don't merge
                  state = EOE.skip(state)

                  case TokenAdapter.peek(state) do
                    {:ok, next_tok, _}
                    when is_keyword_list_result(arg) and not is_container_literal ->
                      cond do
                        Keywords.starts_kw?(next_tok) ->
                          # Continue collecting keywords into this list
                          with {:ok, more_kw, state, log} <-
                                 Keywords.parse_kw_call(state, :unmatched, log) do
                            merged_kw = arg ++ more_kw
                            {:ok, [merged_kw | acc], state, log}
                          end

                        next_tok.kind in [:list_string_start, :bin_string_start] ->
                          # Another quoted keyword - parse via expression and merge
                          parse_paren_args_quoted_kw_continuation(arg, acc, state, log)

                        true ->
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

  # Parse continuation of keyword list in paren args context when we encounter another quoted keyword
  defp parse_paren_args_quoted_kw_continuation(acc_kw, acc, state, log) do
    # Parse the quoted keyword via expression
    case Expressions.expr(state, :unmatched, log) do
      {:ok, expr, state, log} when is_keyword_list_result(expr) ->
        state = EOE.skip(state)

        case TokenAdapter.peek(state) do
          {:ok, %{kind: :","}, _} ->
            {:ok, _comma, state} = TokenAdapter.next(state)
            state = EOE.skip(state)

            case TokenAdapter.peek(state) do
              {:ok, %{kind: :")"}, _} ->
                # Trailing comma
                {:ok, [acc_kw ++ expr | acc], state, log}

              {:ok, tok, _} ->
                cond do
                  Keywords.starts_kw?(tok) ->
                    with {:ok, more_kw, state, log} <-
                           Keywords.parse_kw_call(state, :unmatched, log) do
                      {:ok, [acc_kw ++ expr ++ more_kw | acc], state, log}
                    end

                  tok.kind in [:list_string_start, :bin_string_start] ->
                    parse_paren_args_quoted_kw_continuation(acc_kw ++ expr, acc, state, log)

                  true ->
                    {:ok, [acc_kw ++ expr | acc], state, log}
                end

              _ ->
                {:ok, [acc_kw ++ expr | acc], state, log}
            end

          _ ->
            {:ok, [acc_kw ++ expr | acc], state, log}
        end

      {:ok, _expr, state, log} ->
        # Not a keyword
        {:error, {:expected, :keyword}, state, log}

      {:error, _, _, _} = error ->
        error
    end
  end
end
