defmodule ToxicParser.Grammar.Blocks do
  @moduledoc """
  Block parsing for `fn` and `do` blocks with clause/guard support and basic
  environment events.

  Note: Keywords like `case`, `cond`, `with`, `try`, `receive`, `for` are NOT
  handled here - they are normal function calls with do-blocks and are parsed
  by Calls.parse. Only `fn` needs special handling because it has unique syntax
  (fn -> ... end with stab clauses directly, no arguments before do).
  """

  alias ToxicParser.{Builder, Context, EventLog, Pratt, Result, State, TokenAdapter}
  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.Stabs

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
  def parse(%State{} = state, %Context{} = ctx, %EventLog{} = log) do
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
    {:ok, _fn, state} = TokenAdapter.next(state)
    fn_meta = Builder.Helpers.token_meta(fn_tok.metadata)
    log = enter_scope(log, :fn, fn_tok.metadata)

    # In elixir_parser.yrl, fn_eoe consumes at most one `eoe` token and
    # only that token contributes to fn's `newlines` metadata (via next_is_eol/2).
    {state, newlines} =
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :eoe, value: %{newlines: n}}, _} ->
          {:ok, _eoe, state} = TokenAdapter.next(state)
          {state, n}

        _ ->
          {state, 0}
      end

    # Use the same stab_eoe parsing as paren stabs, but with :end terminator
    with {:ok, clauses, state, log} <- Stabs.parse_stab_eoe_until([], state, ctx, log, :end),
         {:ok, end_meta, state, log} <- expect_kind_with_meta(state, :end, log) do
      log = exit_scope(log, :fn, fn_tok.metadata)
      end_location = Builder.Helpers.token_meta(end_meta)
      # Build metadata: [newlines: N, closing: [...], line: L, column: C]
      meta = Meta.closing_meta(fn_meta, end_location, newlines)
      {:ok, {:fn, meta, clauses}, state, log}
    end
  end

  @doc """
  Parses `do ... end` blocks and returns a tuple with:
  - block_meta: metadata for do/end positions to attach to the call node
  - sections: keyword list of block contents

  Handles labeled sections (`else/catch/rescue/after`).
  """
  @spec parse_do_block(State.t(), Pratt.context(), EventLog.t()) ::
          {:ok, {keyword(), keyword(Macro.t())}, State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}
  def parse_do_block(%State{} = state, %Context{} = ctx, %EventLog{} = log) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: :do, metadata: do_meta}, state} ->
        log = enter_scope(log, :do_block, do_meta)
        do_location = token_meta(do_meta)

        with {:ok, sections, state, log} <-
               parse_labeled_sections([], :do, do_location, state, ctx, log),
             {:ok, end_meta, state, log} <- expect_kind_with_meta(state, :end, log) do
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

  defp token_meta(meta), do: Builder.Helpers.token_meta(meta)

  defp expect_kind_with_meta(state, kind, log) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: ^kind, metadata: meta}, state} -> {:ok, meta, state, log}
      {:ok, token, state} -> {:error, {:expected, kind, got: token.kind}, state, log}
      {:eof, state} -> {:error, :unexpected_eof, state, log}
      other -> Result.normalize_error(other, log)
    end
  end

  defp parse_labeled_sections(acc, label, label_meta, state, ctx, log) do
    stop_kinds = [:end, :block_identifier, :else, :catch, :rescue, :after]

    with {:ok, items_rev, state, log} <-
           Stabs.parse_stab_items_until([], state, ctx, log, :end, stop_kinds) do
      section_value = Stabs.build_section_value(items_rev)
      # Encode label through literal_encoder if present
      encoded_label = Builder.Helpers.literal(label, label_meta, state)
      acc = [{encoded_label, section_value} | acc]

      case TokenAdapter.peek(state) do
        {:ok, tok, _} ->
          if block_label?(tok) do
            {:ok, label_tok, state} = TokenAdapter.next(state)
            next_label_meta = token_meta(label_tok.metadata)
            parse_labeled_sections(acc, label_from(tok), next_label_meta, state, ctx, log)
          else
            {:ok, Enum.reverse(acc), state, log}
          end

        _ ->
          {:ok, Enum.reverse(acc), state, log}
      end
    end
  end

  defp enter_scope(log, scope, meta) do
    EventLog.env(log, %{action: :enter_scope, scope: scope, name: nil}, meta)
  end

  defp exit_scope(log, scope, meta) do
    EventLog.env(log, %{action: :exit_scope, scope: scope, name: nil}, meta)
  end

  defp block_label?(%{kind: :block_identifier, value: value}),
    do: value in [:else, :catch, :rescue, :after]

  defp block_label?(_), do: false

  defp label_from(%{kind: :block_identifier, value: value}), do: value
end
