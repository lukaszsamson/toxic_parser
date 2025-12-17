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

  alias ToxicParser.{Context, EventLog, State, TokenAdapter}
  alias ToxicParser.Grammar.EOE

  @type result(item) ::
          {:ok, [item], State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @type item_fun(item) :: (State.t(), term(), EventLog.t() ->
                             {:ok, item, State.t(), EventLog.t()} | result(item))

  @type close_kind :: atom() | [atom()]

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
  @spec parse_comma_separated(
          State.t(),
          term(),
          EventLog.t(),
          close_kind(),
          item_fun(item),
          opts()
        ) ::
          result(item)
        when item: term()
  def parse_comma_separated(
        %State{} = state,
        %Context{} = ctx,
        %EventLog{} = log,
        close_kind_or_kinds,
        item_fun,
        opts \\ []
      )
      when (is_atom(close_kind_or_kinds) or is_list(close_kind_or_kinds)) and
             is_function(item_fun, 3) do
    opts = Keyword.merge(@default_opts, opts)
    close_kinds = List.wrap(close_kind_or_kinds)
    state = maybe_skip_eoe(state, opts)

    case TokenAdapter.peek(state) do
      {:ok, %{kind: kind}, state} ->
        if kind in close_kinds do
          if opts[:allow_empty?] do
            {:ok, [], state, log}
          else
            {:error, {:expected, :item, got: kind}, state, log}
          end
        else
          parse_items_rev([], state, ctx, log, close_kinds, item_fun, opts)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_items_rev(acc_rev, state, ctx, log, close_kinds, item_fun, opts) do
    case item_fun.(state, ctx, log) do
      {:ok, item, state, log} ->
        state = maybe_skip_eoe(state, opts)

        separator = opts[:separator]

        case TokenAdapter.peek(state) do
          {:ok, %{kind: kind}, state} ->
            cond do
              kind in close_kinds ->
                {:ok, Enum.reverse([item | acc_rev]), state, log}

              kind == separator ->
                {:ok, sep_tok, state} = TokenAdapter.next(state)
                state = maybe_skip_eoe(state, opts)

                case TokenAdapter.peek(state) do
                  {:ok, %{kind: kind}, state} ->
                    if kind in close_kinds do
                      if opts[:allow_trailing_comma?] do
                        {:ok, Enum.reverse([item | acc_rev]), state, log}
                      else
                        {:error, {:trailing_comma, sep_tok}, state, log}
                      end
                    else
                      parse_items_rev(
                        [item | acc_rev],
                        state,
                        ctx,
                        log,
                        close_kinds,
                        item_fun,
                        opts
                      )
                    end

                  {:eof, state} ->
                    {:error, :unexpected_eof, state, log}

                  {:error, diag, state} ->
                    {:error, diag, state, log}
                end

              true ->
                expected =
                  case close_kinds do
                    [single] -> single
                    many -> many
                  end

                {:error, {:expected_comma_or, expected, got: kind}, state, log}
            end

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
