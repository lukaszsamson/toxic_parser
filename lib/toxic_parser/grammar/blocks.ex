defmodule ToxicParser.Grammar.Blocks do
  @moduledoc """
  Block parsing for `fn` and `do` blocks with clause/guard support and basic
  environment events.

  Note: Keywords like `case`, `cond`, `with`, `try`, `receive`, `for` are NOT
  handled here - they are normal function calls with do-blocks and are parsed
  by Calls.parse. Only `fn` needs special handling because it has unique syntax
  (fn -> ... end with stab clauses directly, no arguments before do).
  """

  alias ToxicParser.{Builder, Context, Cursor, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.{ErrorHelpers, Stabs}

  @type result ::
          {:ok, Macro.t(), State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}
          | {:no_block, State.t(), Cursor.t()}

  @doc """
  Entry point for block constructs. Only `fn` needs special handling here
  because it has unique syntax (fn -> ... end with stab clauses directly).

  Other block keywords like `case`, `cond`, `with`, `try`, `receive`, `for`
  are just normal function calls with do-blocks and are handled by Calls.parse.
  """
  @spec parse(State.t(), Cursor.t(), Pratt.context(), EventLog.t()) :: result()
  def parse(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log) do
    case Cursor.peek(cursor) do
      {:ok, {:fn, _meta, _value} = tok, cursor} ->
        parse_fn(tok, state, cursor, ctx, log)

      {:eof, cursor} ->
        {:no_block, state, cursor}

      {:error, _reason, cursor} ->
        {:no_block, state, cursor}

      {:ok, _tok, cursor} ->
        {:no_block, state, cursor}
    end
  end

  defp parse_fn(fn_tok, state, cursor, ctx, log) do
    {:ok, _fn, state, cursor} = TokenAdapter.next(state, cursor)
    fn_meta = TokenAdapter.token_meta(fn_tok)
    log = enter_scope(log, :fn, TokenAdapter.full_metadata(fn_tok, state))

    # In elixir_parser.yrl, fn_eoe consumes at most one separator token and
    # only that token contributes to fn's `newlines` metadata (via next_is_eol/2).
    {state, cursor, newlines} =
      case Cursor.peek(cursor) do
        {:ok, {:eol, {_, _, n}, _value}, cursor} when is_integer(n) ->
          {:ok, _eol, state, cursor} = TokenAdapter.next(state, cursor)
          {state, cursor, n}

        {:ok, {:eol, _meta, _value}, cursor} ->
          {:ok, _eol, state, cursor} = TokenAdapter.next(state, cursor)
          {state, cursor, 0}

        {:ok, {:";", {_, _, n}, _value}, cursor} when is_integer(n) ->
          {:ok, _semi, state, cursor} = TokenAdapter.next(state, cursor)
          {state, cursor, n}

        {:ok, {:";", _meta, _value}, cursor} ->
          {:ok, _semi, state, cursor} = TokenAdapter.next(state, cursor)
          {state, cursor, 0}

        {:ok, _, cursor} ->
          {state, cursor, 0}

        {:eof, cursor} ->
          {state, cursor, 0}

        {:error, _, cursor} ->
          {state, cursor, 0}
      end

    # Use the same stab_eoe parsing as paren stabs, but with :end terminator
    with {:ok, clauses, state, cursor, log} <-
           Stabs.parse_stab_eoe_until([], state, cursor, ctx, log, :end),
         {:ok, end_meta, state, cursor, log} <- expect_kind_with_meta(state, cursor, :end, log) do
      log = exit_scope(log, :fn, TokenAdapter.full_metadata(fn_tok, state))
      end_location = meta_to_location(end_meta)
      # Build metadata: [newlines: N, closing: [...], line: L, column: C]
      meta = Meta.closing_meta(fn_meta, end_location, newlines)
      {:ok, {:fn, meta, clauses}, state, cursor, log}
    else
      {:error, reason, state, cursor, log} ->
        maybe_recover_fn_error(reason, fn_meta, state, cursor, log)
    end
  end

  @doc """
  Parses `do ... end` blocks and returns a tuple with:
  - block_meta: metadata for do/end positions to attach to the call node
  - sections: keyword list of block contents

  Handles labeled sections (`else/catch/rescue/after`).
  """
  @spec parse_do_block(State.t(), Cursor.t(), Pratt.context(), EventLog.t()) ::
          {:ok, {keyword(), keyword(Macro.t())}, State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}
  def parse_do_block(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log) do
    case TokenAdapter.next(state, cursor) do
      {:ok, {:do, do_meta, _value} = do_tok, state, cursor} ->
        log = enter_scope(log, :do_block, TokenAdapter.full_metadata(do_tok, state))
        do_location = meta_to_location(do_meta)

        with {:ok, sections, state, cursor, log} <-
               parse_labeled_sections([], :do, do_location, state, cursor, ctx, log),
             {:ok, end_meta, state, cursor, log} <-
               expect_kind_with_meta(state, cursor, :end, log) do
          log = exit_scope(log, :do_block, TokenAdapter.full_metadata(do_tok, state))
          end_location = meta_to_location(end_meta)
          # Build do/end metadata like elixir_parser.yrl does
          block_meta = [do: do_location, end: end_location]
          {:ok, {block_meta, sections}, state, cursor, log}
        else
          {:error, reason, state, cursor, log} ->
            maybe_recover_do_block_error(reason, do_location, state, cursor, log)
        end

      {:ok, {got_kind, _meta, _value}, state, cursor} ->
        reason = {:expected, :do, got: got_kind}
        maybe_recover_do_block_error(reason, [], state, cursor, log)

      {:eof, state, cursor} ->
        maybe_recover_do_block_error(:unexpected_eof, [], state, cursor, log)

      {:error, diag, state, cursor} ->
        maybe_recover_do_block_error(diag, [], state, cursor, log)
    end
  end

  # Convert raw meta tuple to [line: L, column: C]
  defp meta_to_location({{line, column}, _, _}), do: [line: line, column: column]

  defp expect_kind_with_meta(state, cursor, kind, log) do
    case TokenAdapter.next(state, cursor) do
      {:ok, {^kind, meta, _value}, state, cursor} ->
        {:ok, meta, state, cursor, log}

      {:ok, {got_kind, _meta, _value}, state, cursor} ->
        {:error, {:expected, kind, got: got_kind}, state, cursor, log}

      {:eof, state, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, state, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  defp parse_labeled_sections(acc, label, label_meta, state, cursor, ctx, log) do
    stop_kinds = [:end, :block_identifier, :else, :catch, :rescue, :after]

    with {:ok, items_rev, state, cursor, log} <-
           Stabs.parse_stab_items_until([], state, cursor, ctx, log, :end, stop_kinds) do
      section_value = Stabs.build_section_value(items_rev)
      # Encode label through literal_encoder if present
      encoded_label = Builder.Helpers.literal(label, label_meta, state)
      acc = [{encoded_label, section_value} | acc]

      case Cursor.peek(cursor) do
        {:ok, {:block_identifier, _meta, value}, cursor} ->
          {:ok, label_tok, state, cursor} = TokenAdapter.next(state, cursor)
          next_label_meta = TokenAdapter.token_meta(label_tok)
          parse_labeled_sections(acc, value, next_label_meta, state, cursor, ctx, log)

        {:ok, _, cursor} ->
          {:ok, Enum.reverse(acc), state, cursor, log}
      end
    end
  end

  defp enter_scope(log, scope, meta) do
    EventLog.env(log, %{action: :enter_scope, scope: scope, name: nil}, meta)
  end

  defp exit_scope(log, scope, meta) do
    EventLog.env(log, %{action: :exit_scope, scope: scope, name: nil}, meta)
  end

  defp maybe_recover_fn_error(reason, fn_meta, %State{mode: :tolerant} = state, cursor, log) do
    {error_ast, state} = build_error_node(reason, fn_meta, state, cursor)
    {:ok, error_ast, state, cursor, log}
  end

  defp maybe_recover_fn_error(reason, _fn_meta, state, cursor, log),
    do: {:error, reason, state, cursor, log}

  defp build_error_node(reason, meta, %State{} = state, cursor) do
    ErrorHelpers.build_error_node(:invalid, reason, meta, state, cursor)
  end

  defp maybe_recover_do_block_error(
         reason,
         do_location,
         %State{mode: :tolerant} = state,
         cursor,
         log
       ) do
    {error_ast, state} = build_error_node(reason, do_location, state, cursor)
    block_meta = build_do_block_meta(do_location, cursor)
    {:ok, {block_meta, [do: error_ast]}, state, cursor, log}
  end

  defp maybe_recover_do_block_error(reason, _do_location, state, cursor, log),
    do: {:error, reason, state, cursor, log}

  defp build_do_block_meta(do_location, cursor) do
    {line, column} = Cursor.position(cursor)
    end_location = [line: line || 1, column: column || 1]

    case do_location do
      [] -> [do: end_location, end: end_location]
      _ -> [do: do_location, end: end_location]
    end
  end
end
