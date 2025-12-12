defmodule ToxicParser.Grammar.Blocks do
  @moduledoc """
  Block parsing for `fn` and `do` blocks with clause/guard support and basic
  environment events.

  Note: Keywords like `case`, `cond`, `with`, `try`, `receive`, `for` are NOT
  handled here - they are normal function calls with do-blocks and are parsed
  by Calls.parse. Only `fn` needs special handling because it has unique syntax
  (fn -> ... end with stab clauses directly, no arguments before do).
  """

  alias ToxicParser.{Builder, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.Containers

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}
          | {:no_block, State.t()}

  @doc """
  Entry point for block constructs. Only `fn` needs special handling here
  because it has unique syntax (fn -> ... end with stab clauses directly).

  Other block keywords like `case`, `cond`, `with`, `try`, `receive`, `for`
  are just normal function calls with do-blocks and are handled by Calls.parse.
  """
  @spec parse(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse(%State{} = state, ctx, %EventLog{} = log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :fn} = tok, _} ->
        parse_fn(tok, state, ctx, log)

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
        # Use min_bp > stab_op (10) to stop before -> so it doesn't get consumed
        # as a binary operator. The -> belongs to the clause parser.
        with {:ok, expr, state, log} <- Pratt.parse_with_min_bp(state, ctx, log, 11) do
          # Annotate with end_of_expression if followed by EOE
          {transformed, state} = maybe_annotate_and_consume_eoe(expr, state)

          transformed =
            case transformed do
              {:stab_op, _, [lhs, rhs]} -> {:->, [], [List.wrap(lhs), rhs]}
              other -> other
            end

          parse_section_items([transformed | acc], state, ctx, log, stop_kinds)
        end
    end
  end

  defp try_parse_clause(state, ctx, log, _stop_kinds) do
    # Use Containers.try_parse_stab_clause which properly handles:
    # - stab_parens_many: (a) -> body, (a, b) -> body, () -> body
    # - stab pattern expressions: a -> body, a, b -> body
    # - guards: a when g -> body
    # Use :end as terminator since block sections end at :end or :block_identifier
    # Checkpoint so we can rewind if not a stab clause
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

    case Containers.try_parse_stab_clause(checkpoint_state, ctx, log, :end) do
      {:ok, clause, state, log} ->
        {:ok, clause, state, log}

      {:not_stab, _state, log} ->
        # Rewind to before we tried to parse stab
        {:no_clause, TokenAdapter.rewind(checkpoint_state, ref), log}

      {:error, _reason, _state, log} ->
        # Rewind on error too - let expression parsing try
        {:no_clause, TokenAdapter.rewind(checkpoint_state, ref), log}
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

  # Annotate expression with end_of_expression metadata if followed by EOE, then consume it
  defp maybe_annotate_and_consume_eoe(expr, state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe} = eoe_tok, _} ->
        {:ok, _tok, state} = TokenAdapter.next(state)
        eoe_meta = build_eoe_meta(eoe_tok)
        annotated = annotate_eoe(expr, eoe_meta)
        {annotated, state}

      _ ->
        {expr, state}
    end
  end

  # Build end_of_expression metadata from an EOE token
  defp build_eoe_meta(%{kind: :eoe, value: %{newlines: newlines}, metadata: meta})
       when is_integer(newlines) do
    case meta do
      %{range: %{start: %{line: line, column: column}}} ->
        [newlines: newlines, line: line, column: column]
      _ ->
        []
    end
  end

  defp build_eoe_meta(%{kind: :eoe, metadata: meta}) do
    case meta do
      %{range: %{start: %{line: line, column: column}}} ->
        [line: line, column: column]
      _ ->
        []
    end
  end

  # Annotate an AST node with end_of_expression metadata
  defp annotate_eoe({left, meta, right}, eoe_meta) when is_list(meta) do
    {left, [{:end_of_expression, eoe_meta} | meta], right}
  end

  defp annotate_eoe(ast, _eoe_meta), do: ast

  defp build_block([single]), do: single
  defp build_block(items), do: Builder.Helpers.literal({:__block__, [], items})

  defp enter_scope(log, scope, meta) do
    EventLog.env(log, %{action: :enter_scope, scope: scope, name: nil}, meta)
  end

  defp exit_scope(log, scope, meta) do
    EventLog.env(log, %{action: :exit_scope, scope: scope, name: nil}, meta)
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
