defmodule ToxicParser.Grammar.Blocks do
  @moduledoc """
  Block parsing for `fn`, `case`/`cond`/`with`/`try`/`receive` and `do` blocks
  with clause/guard support and basic environment events.
  """

  alias ToxicParser.{Builder, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.Expressions

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}
          | {:no_block, State.t()}

  @doc """
  Entry point for block constructs that start with reserved words (`fn`,
  `case`, `cond`, `with`, `try`, `receive`).
  """
  @spec parse(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse(%State{} = state, ctx, %EventLog{} = log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :fn} = tok, _} ->
        parse_fn(tok, state, ctx, log)

      {:ok, %{kind: kind, value: value} = tok, _}
      when kind in [:identifier, :do_identifier, :block_identifier] and
             value in [:case, :cond, :with, :try, :receive, :for] ->
        parse_keyword_block(value, tok, state, ctx, log)

      {:eof, state} ->
        {:no_block, state}

      {:error, _diag, state} ->
        {:no_block, state}

      _ ->
        {:no_block, state}
    end
  end

  defp parse_fn(fn_tok, state, ctx, log) do
    alias ToxicParser.Grammar.Containers

    {:ok, _fn, state} = TokenAdapter.next(state)
    fn_meta = token_meta(fn_tok.metadata)
    log = enter_scope(log, :fn, fn_tok.metadata)

    # Skip optional EOE after fn and count newlines
    {state, newlines} = skip_eoe_count_newlines(state, 0)

    # Use the same stab_eoe parsing as paren stabs, but with :end terminator
    with {:ok, clauses, state, log} <- Containers.parse_stab_eoe_until([], state, ctx, log, :end),
         {:ok, end_meta, state} <- expect_kind_with_meta(state, :end) do
      log = exit_scope(log, :fn, fn_tok.metadata)
      end_location = token_meta(end_meta)
      # Build metadata: [newlines: N, closing: [...], line: L, column: C]
      newlines_meta = if newlines > 0, do: [newlines: newlines], else: []
      meta = newlines_meta ++ [closing: end_location] ++ fn_meta
      {:ok, {:fn, meta, clauses}, state, log}
    end
  end

  defp skip_eoe_count_newlines(state, count) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, value: %{newlines: n}}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe_count_newlines(state, count + n)

      _ ->
        {state, count}
    end
  end

  defp parse_keyword_block(kind, kw_tok, state, ctx, log) do
    {:ok, _kw, state} = TokenAdapter.next(state)
    log = enter_scope(log, kind, kw_tok.metadata)

    subject_result =
      case kind do
        :cond -> {:ok, nil, state, log}
        :try -> {:ok, nil, state, log}
        :receive -> {:ok, nil, state, log}
        _ -> Pratt.parse(state, ctx, log)
      end

    kw_meta = Builder.Helpers.token_meta(kw_tok.metadata)

    case kind do
      :for ->
        with {:ok, qualifiers, state, log} <- parse_for_qualifiers([], state, ctx, log),
             {:ok, {block_meta, sections}, state, log} <- parse_do_block(state, ctx, log) do
          # Attach do/end meta to the call, then line/column
          ast = {:for, block_meta ++ kw_meta, Enum.reverse(qualifiers) ++ [sections]}
          log = exit_scope(log, kind, kw_tok.metadata)
          {:ok, ast, state, log}
        end

      :with ->
        with {:ok, qualifiers, state, log} <- parse_with_qualifiers([], state, ctx, log),
             {:ok, {block_meta, sections}, state, log} <- parse_do_block(state, ctx, log) do
          ast = {:with, block_meta ++ kw_meta, Enum.reverse(qualifiers) ++ [sections]}
          log = exit_scope(log, kind, kw_tok.metadata)
          {:ok, ast, state, log}
        end

      _ ->
        with {:ok, subject, state, log} <- subject_result,
             {:ok, {block_meta, sections}, state, log} <- parse_do_block(state, ctx, log) do
          ast =
            case kind do
              :case -> {:case, block_meta ++ kw_meta, [subject, sections]}
              :cond -> {:cond, block_meta ++ kw_meta, [sections]}
              :try -> {:try, block_meta ++ kw_meta, [sections]}
              :receive -> {:receive, block_meta ++ kw_meta, [sections]}
            end

          log = exit_scope(log, kind, kw_tok.metadata)
          {:ok, ast, state, log}
        end
    end
  end

  @doc """
  Parses `do ... end` blocks and returns a tuple with:
  - block_meta: metadata for do/end positions to attach to the call node
  - sections: keyword list of block contents

  Handles labeled sections (`else/catch/rescue/after`).
  """
  @spec parse_do_block(State.t(), Pratt.context(), EventLog.t()) ::
          {:ok, {keyword(), keyword(Macro.t())}, State.t(), EventLog.t()} | {:error, term(), State.t(), EventLog.t()}
  def parse_do_block(state, ctx, log) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: :do, metadata: do_meta}, state} ->
        log = enter_scope(log, :do_block, do_meta)
        do_location = token_meta(do_meta)

        with {:ok, sections, state, log} <-
               parse_labeled_sections([], :do, state, ctx, log),
             {:ok, end_meta, state} <- expect_kind_with_meta(state, :end) do
          log = exit_scope(log, :do_block, do_meta)
          end_location = token_meta(end_meta)
          # Build do/end metadata like elixir_parser.yrl does
          block_meta = [do: do_location, end: end_location]
          {:ok, {block_meta, sections}, state, log}
        end

      {:ok, token, state} ->
        {:error, {:expected, :do, got: token.kind}, state, log}

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp token_meta(%{range: %{start: %{line: line, column: column}}}) do
    [line: line, column: column]
  end
  defp token_meta(_), do: []

  defp expect_kind_with_meta(state, kind) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: ^kind, metadata: meta}, state} -> {:ok, meta, state}
      {:ok, token, state} -> {:error, {:expected, kind, got: token.kind}, state}
      {:eof, state} -> {:error, :unexpected_eof, state}
      {:error, diag, state} -> {:error, diag, state}
    end
  end

  defp parse_labeled_sections(acc, label, state, ctx, log) do
    stop_kinds = [:end, :block_identifier]

    with {:ok, clauses, state, log} <- parse_section_items([], state, ctx, log, stop_kinds) do
      section_value = build_section_value(clauses)
      acc = [{label, section_value} | acc]

      case TokenAdapter.peek(state) do
        {:ok, tok, _} ->
          if block_label?(tok) do
            {:ok, _tok, state} = TokenAdapter.next(state)
            parse_labeled_sections(acc, label_from(tok), state, ctx, log)
          else
            {:ok, Enum.reverse(acc), state, log}
          end

        _ ->
          {:ok, Enum.reverse(acc), state, log}
      end
    end
  end

  defp parse_with_qualifiers(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :do}, _} ->
        {:ok, acc, state, log}

      {:ok, %{kind: :eoe}, state} ->
        {:ok, _tok, state} = TokenAdapter.next(state)
        parse_with_qualifiers(acc, state, ctx, log)

      {:ok, _tok, _} ->
        with {:ok, qualifier, state, log} <- parse_qualifier(state, ctx, log),
             state <- consume_optional_comma(state) do
          parse_with_qualifiers([qualifier | acc], state, ctx, log)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_for_qualifiers(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :do}, _} ->
        {:ok, acc, state, log}

      {:ok, %{kind: :eoe}, state} ->
        {:ok, _tok, state} = TokenAdapter.next(state)
        parse_for_qualifiers(acc, state, ctx, log)

      {:ok, _tok, _} ->
        with {:ok, qualifier, state, log} <- parse_for_qualifier(state, ctx, log),
             state <- consume_optional_comma(state) do
          parse_for_qualifiers([qualifier | acc], state, ctx, log)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_for_qualifier(state, ctx, log) do
    with {:ok, left, state, log} <- Expressions.expr(state, ctx, log) do
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :in_match_op}, _} ->
          {:ok, _op, state} = TokenAdapter.next(state)
          with {:ok, rhs, state, log} <- Expressions.expr(state, ctx, log) do
            {:ok, { :<-, [], [left, rhs]}, state, log}
          end

        _ ->
          {:ok, left, state, log}
      end
    end
  end

  defp parse_qualifier(state, ctx, log) do
    with {:ok, left, state, log} <- Pratt.parse(state, ctx, log) do
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :in_match_op}, _} ->
          {:ok, _op, state} = TokenAdapter.next(state)
          with {:ok, rhs, state, log} <- Pratt.parse(state, ctx, log) do
            {:ok, { :<-, [], [left, rhs]}, state, log}
          end

        _ ->
          {:ok, left, state, log}
      end
    end
  end

  defp parse_section_items(acc, state, ctx, log, stop_kinds) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if stop_token?(tok, stop_kinds) do
          {:ok, Enum.reverse(acc), state, log}
        else
          handle_section_item(tok.kind, acc, state, ctx, log, stop_kinds)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp handle_section_item(:eoe, acc, state, ctx, log, stop_kinds) do
    {:ok, _tok, state} = TokenAdapter.next(state)
    parse_section_items(acc, state, ctx, log, stop_kinds)
  end

  defp handle_section_item(_kind, acc, state, ctx, log, stop_kinds) do
    case try_parse_clause(state, ctx, log, stop_kinds) do
      {:ok, clause, state, log} ->
        parse_section_items([clause | acc], state, ctx, log, stop_kinds)

      {:no_clause, state, log} ->
        with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log),
             state <- consume_optional_eoe(state) do
          transformed =
            case expr do
              {:stab_op, _, [lhs, rhs]} -> {:->, [], [List.wrap(lhs), rhs]}
              other -> other
            end

          parse_section_items([transformed | acc], state, ctx, log, stop_kinds)
        end

      {:error, reason, state, log} ->
        {:error, reason, state, log}
    end
  end

  defp try_parse_clause(state, ctx, log, stop_kinds) do
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

    case parse_clause(checkpoint_state, ctx, log, stop_kinds) do
      {:ok, clause, new_state, log} ->
        {:ok, clause, new_state, log}

      {:error, {:expected, :stab_op, _}, _state, _log} ->
        {:no_clause, TokenAdapter.rewind(checkpoint_state, ref), log}

      {:error, :unexpected_eof, _state, _log} ->
        {:no_clause, TokenAdapter.rewind(checkpoint_state, ref), log}

      {:error, reason, _state, log} ->
        {:error, reason, TokenAdapter.rewind(checkpoint_state, ref), log}
    end
  end

  defp parse_clauses(acc, state, ctx, log, stop_kinds) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: kind}, _} ->
        cond do
          kind in stop_kinds ->
            {:ok, Enum.reverse(acc), state, log}

          kind == :eoe ->
            {:ok, _tok, state} = TokenAdapter.next(state)
            parse_clauses(acc, state, ctx, log, stop_kinds)

          true ->
            with {:ok, clause, state, log} <- parse_clause(state, ctx, log, stop_kinds) do
              parse_clauses([clause | acc], state, ctx, log, stop_kinds)
            end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_clause(state, ctx, log, stop_kinds) do
    head_meta = peek_meta(state)

    with {:ok, patterns, state, log} <- parse_clause_patterns([], state, ctx, log),
         {:ok, head_list, state, log} <- attach_guard(patterns, state, ctx, log) do
      case TokenAdapter.next(state) do
        {:ok, %{kind: :stab_op}, state} ->
          with {:ok, body, state, log} <- parse_clause_body([], state, ctx, log, stop_kinds),
               state <- consume_optional_eoe(state) do
            log =
              Enum.reduce(head_list, log, fn pattern, acc ->
                maybe_bind_env(acc, pattern, head_meta)
              end)

            {:ok, {:->, [], [head_list, body]}, state, log}
          end

        {:ok, token, state} ->
          {:error, {:expected, :stab_op, got: token.kind}, state, log}

        {:eof, state} ->
          {:error, {:expected, :stab_op, got: :eof}, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  defp parse_clause_patterns(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :stab_op}, _} ->
        {:ok, Enum.reverse(acc), state, log}

      _ ->
        with {:ok, pattern, state, log} <- parse_clause_pattern(state, ctx, log) do
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :","}, state} ->
              {:ok, _comma, state} = TokenAdapter.next(state)
              parse_clause_patterns([pattern | acc], state, ctx, log)

            {:ok, %{kind: :when_op}, _} ->
              {:ok, Enum.reverse([pattern | acc]), state, log}

            {:ok, %{kind: :stab_op}, _} ->
              {:ok, Enum.reverse([pattern | acc]), state, log}

            _ ->
              {:ok, Enum.reverse([pattern | acc]), state, log}
          end
        end
    end
  end

  defp parse_clause_pattern(state, ctx, log) do
    case TokenAdapter.peek_n(state, 2) do
      {:ok, [tok, next], state} when tok.kind in [:identifier, :atom, :int, :flt, :string, true, false, :nil] and
                                      next.kind in [:when_op, :stab_op] ->
        {:ok, _tok, state} = TokenAdapter.next(state)
        {:ok, token_literal(tok), state, log}

      _ ->
        Expressions.expr(state, ctx, log)
    end
  end

  defp token_literal(%{kind: :int, value: chars}), do: List.to_integer(chars)
  defp token_literal(%{kind: :flt, value: chars}), do: List.to_float(chars)
  defp token_literal(%{kind: :atom, value: atom}), do: atom
  defp token_literal(%{kind: :string, value: value}), do: value
  defp token_literal(%{kind: :identifier, value: value}), do: value
  defp token_literal(%{kind: true}), do: true
  defp token_literal(%{kind: false}), do: false
  defp token_literal(%{kind: :nil}), do: nil
  defp token_literal(%{value: value}), do: value

  defp attach_guard(patterns, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :when_op}, _} ->
        {:ok, _when, state} = TokenAdapter.next(state)
        {:ok, guard, state, log} = Pratt.parse(state, ctx, log)
        guarded = {:when, [], patterns ++ [guard]}
        {:ok, [guarded], state, log}

      _ ->
        {:ok, patterns, state, log}
    end
  end

  defp parse_clause_body(acc, state, ctx, log, stop_kinds) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: kind}, _} ->
        cond do
          kind in stop_kinds ->
            {:ok, build_block(Enum.reverse(acc)), state, log}

          kind == :eoe ->
            {:ok, _tok, state} = TokenAdapter.next(state)
            {:ok, build_block(Enum.reverse(acc)), state, log}

          true ->
            with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
              parse_clause_body([expr | acc], state, ctx, log, stop_kinds)
            end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp build_section_value([]), do: build_block([])

  defp build_section_value(items) do
    if Enum.all?(items, &match?({:->, _, _}, &1)) do
      items
    else
      build_block(items)
    end
  end

  defp consume_optional_eoe(state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe}, state} ->
        {:ok, _tok, state} = TokenAdapter.next(state)
        state

      _ ->
        state
    end
  end

  defp consume_optional_comma(state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :","}, state} ->
        {:ok, _tok, state} = TokenAdapter.next(state)
        state

      _ ->
        state
    end
  end

  defp build_block([single]), do: single
  defp build_block(items), do: Builder.Helpers.literal({:__block__, [], items})

  defp expect_kind(state, kind) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: ^kind}, state} -> {:ok, kind, state}
      {:ok, token, state} -> {:error, {:expected, kind, got: token.kind}, state}
      {:eof, state} -> {:error, :unexpected_eof, state}
      {:error, diag, state} -> {:error, diag, state}
    end
  end

  defp enter_scope(log, scope, meta) do
    EventLog.env(log, %{action: :enter_scope, scope: scope, name: nil}, meta)
  end

  defp exit_scope(log, scope, meta) do
    EventLog.env(log, %{action: :exit_scope, scope: scope, name: nil}, meta)
  end

  defp maybe_bind_env(log, {:when, [], [pattern, _guard]}, meta), do: maybe_bind_env(log, pattern, meta)
  defp maybe_bind_env(log, {:<-, [], [pattern, _]}, meta), do: maybe_bind_env(log, pattern, meta)
  defp maybe_bind_env(log, {:^, _, [inner]}, meta), do: maybe_bind_env(log, inner, meta)
  defp maybe_bind_env(log, {:{}, _, parts}, meta), do: Enum.reduce(parts, log, &maybe_bind_env(&2, &1, meta))
  defp maybe_bind_env(log, list, meta) when is_list(list), do: Enum.reduce(list, log, &maybe_bind_env(&2, &1, meta))
  defp maybe_bind_env(log, {:%{}, _, pairs}, meta), do: Enum.reduce(pairs, log, fn {k, v}, acc -> acc |> maybe_bind_env(k, meta) |> maybe_bind_env(v, meta) end)
  defp maybe_bind_env(log, {{:., _, _} = struct, _, fields}, meta), do: Enum.reduce(fields, log, &maybe_bind_env(&2, &1, meta)) |> maybe_bind_env(struct, meta)
  defp maybe_bind_env(log, name, meta) when is_atom(name), do: EventLog.env(log, %{action: :bind, scope: nil, name: name}, meta)
  defp maybe_bind_env(log, _other, _meta), do: log

  defp peek_meta(state) do
    case TokenAdapter.peek(state) do
      {:ok, %{metadata: meta}, _} -> meta
      _ -> empty_meta()
    end
  end

  defp empty_meta do
    %{
      range: %{start: %{line: 1, offset: 0, column: 1}, end: %{line: 1, offset: 0, column: 1}},
      delimiter: nil,
      newlines: 0,
      synthesized?: false,
      terminators: [],
      role: :none
    }
  end

  defp stop_token?(%{kind: kind} = tok, stop_kinds) do
    kind in stop_kinds or block_label?(tok)
  end

  defp block_label?(%{kind: :block_identifier, value: value}), do: value in [:else, :catch, :rescue, :after]
  defp block_label?(%{kind: kind}) when kind in [:else, :catch, :rescue, :after], do: true
  defp block_label?(_), do: false

  defp label_from(%{kind: :block_identifier, value: value}), do: value
  defp label_from(%{kind: kind}) when kind in [:else, :catch, :rescue, :after], do: kind
end
