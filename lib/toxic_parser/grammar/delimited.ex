defmodule ToxicParser.Grammar.Delimited do
  @moduledoc """
  Helpers for parsing delimited, comma-separated sequences.

  This module is intended to reduce duplication across container-like grammars
  (lists, tuples, bitstrings, dot-container args, bracket args, etc) by
  centralizing the common loop:

  - `item (',' item)*`
  - optional trailing comma before the closing delimiter
  - optional EOE skipping between separators/items

  The closing token is *not* consumed by default; callers usually need to
  measure/attach closing metadata themselves.
  """

  alias ToxicParser.{EventLog, State, TokenAdapter}
  alias ToxicParser.Grammar.EOE

  @type result(item) ::
          {:ok, [item], State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @type item_fun(item) :: (State.t(), term(), EventLog.t() -> {:ok, item, State.t(), EventLog.t()} | result(item))

  @type opts :: [
          separator: atom(),
          skip_eoe?: boolean(),
          allow_empty?: boolean(),
          allow_trailing_comma?: boolean()
        ]

  @default_opts [
    separator: :",",
    skip_eoe?: true,
    allow_empty?: false,
    allow_trailing_comma?: true
  ]

  @doc """
  Parses a comma-separated sequence until `close_kind` is next.

  `item_fun` must be an arity-3 function that parses a single item and returns:

  - `{:ok, item, state, log}` on success
  - `{:error, reason, state, log}` on failure

  The close token is not consumed; parsing stops with `close_kind` still as the
  next token.
  """
  @spec parse_comma_separated(State.t(), term(), EventLog.t(), atom(), item_fun(item), opts()) ::
          result(item)
        when item: term()
  def parse_comma_separated(%State{} = state, ctx, %EventLog{} = log, close_kind, item_fun, opts \\ [])
      when is_atom(close_kind) and is_function(item_fun, 3) do
    opts = Keyword.merge(@default_opts, opts)
    state = maybe_skip_eoe(state, opts)

    case TokenAdapter.peek(state) do
      {:ok, %{kind: ^close_kind}, state} ->
        if opts[:allow_empty?] do
          {:ok, [], state, log}
        else
          {:error, {:expected, :item, got: close_kind}, state, log}
        end

      {:ok, _tok, state} ->
        parse_items_rev([], state, ctx, log, close_kind, item_fun, opts)

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_items_rev(acc_rev, state, ctx, log, close_kind, item_fun, opts) do
    case item_fun.(state, ctx, log) do
      {:ok, item, state, log} ->
        state = maybe_skip_eoe(state, opts)

        separator = opts[:separator]

        case TokenAdapter.peek(state) do
          {:ok, %{kind: ^close_kind}, state} ->
            {:ok, Enum.reverse([item | acc_rev]), state, log}

          {:ok, %{kind: sep_kind}, state} when sep_kind == separator ->
            {:ok, _sep, state} = TokenAdapter.next(state)
            state = maybe_skip_eoe(state, opts)

            case TokenAdapter.peek(state) do
              {:ok, %{kind: ^close_kind}, state} ->
                if opts[:allow_trailing_comma?] do
                  {:ok, Enum.reverse([item | acc_rev]), state, log}
                else
                  {:error, {:unexpected, :trailing_comma}, state, log}
                end

              {:ok, _tok, state} ->
                parse_items_rev([item | acc_rev], state, ctx, log, close_kind, item_fun, opts)

              {:eof, state} ->
                {:error, :unexpected_eof, state, log}

              {:error, diag, state} ->
                {:error, diag, state, log}
            end

          {:ok, %{kind: kind}, state} ->
            {:error, {:expected_comma_or, close_kind, got: kind}, state, log}

          {:eof, state} ->
            {:error, :unexpected_eof, state, log}

          {:error, diag, state} ->
            {:error, diag, state, log}
        end

      {:error, _reason, _state, _log} = err ->
        err
    end
  end

  defp maybe_skip_eoe(state, opts) do
    if opts[:skip_eoe?] do
      {state, _newlines} = EOE.skip_count_newlines(state, 0)
      state
    else
      state
    end
  end
end
