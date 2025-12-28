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

  alias ToxicParser.{Context, Cursor, EventLog, State, TokenAdapter}
  # TokenAdapter used for TokenAdapter.next
  alias ToxicParser.Grammar.EOE

  @type result(item) ::
          {:ok, [item], State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}

  @type item_fun(item) :: (State.t(), Cursor.t(), term(), EventLog.t() ->
                             {:ok, item, State.t(), Cursor.t(), EventLog.t()}
                             | {:no_item, State.t(), Cursor.t(), EventLog.t()}
                             | result(item))

  @type close_kind :: atom() | [atom()]

  @type opts :: [
          separator: atom(),
          skip_eoe?: boolean(),
          skip_eoe_initial?: boolean(),
          skip_eoe_after_item?: boolean(),
          skip_eoe_after_separator?: boolean(),
          allow_empty?: boolean(),
          allow_trailing_comma?: boolean(),
          stop_on_unexpected?: boolean(),
          on_no_item_after_separator: (token(), State.t(), Cursor.t(), term(), EventLog.t() ->
                                         {:error, term(), State.t(), Cursor.t(), EventLog.t()})
        ]

  @type token :: tuple()

  @default_opts [
    separator: :",",
    skip_eoe?: true,
    skip_eoe_initial?: nil,
    skip_eoe_after_item?: nil,
    skip_eoe_after_separator?: nil,
    allow_empty?: false,
    allow_trailing_comma?: true,
    stop_on_unexpected?: false,
    on_no_item_after_separator: nil
  ]

  @doc """
  Parses a comma-separated sequence until `close_kind` is next.

  `item_fun` must be an arity-4 function that parses a single item and returns:

  - `{:ok, item, state, cursor, log}` on success
  - `{:error, reason, state, cursor, log}` on failure

  The close token is not consumed; parsing stops with `close_kind` still as the
  next token.
  """
  @spec parse_comma_separated(
          State.t(),
          Cursor.t(),
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
        cursor,
        %Context{} = ctx,
        %EventLog{} = log,
        close_kind_or_kinds,
        item_fun,
        opts \\ []
      )
      when (is_atom(close_kind_or_kinds) or is_list(close_kind_or_kinds)) and
             is_function(item_fun, 4) do
    opts = Keyword.merge(@default_opts, opts)
    close_kinds = List.wrap(close_kind_or_kinds)
    eof_is_close? = :eof in close_kinds
    {state, cursor} = maybe_skip_eoe(state, cursor, opts, :initial)

    case Cursor.peek(cursor) do
      {:ok, {kind, _meta, _value}, _cursor} ->
        if kind in close_kinds do
          if opts[:allow_empty?] do
            {:ok, [], state, cursor, log}
          else
            {:error, {:expected, :item, got: kind}, state, cursor, log}
          end
        else
          parse_items_rev([], state, cursor, ctx, log, close_kinds, item_fun, opts)
        end

      {:eof, _cursor} ->
        if eof_is_close? do
          if opts[:allow_empty?] do
            {:ok, [], state, cursor, log}
          else
            {:error, {:expected, :item, got: :eof}, state, cursor, log}
          end
        else
          {:error, :unexpected_eof, state, cursor, log}
        end

      {:error, diag, _cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  defp parse_items_rev(
         acc_rev,
         state,
         cursor,
         ctx,
         log,
         close_kinds,
         item_fun,
         opts,
         after_sep_tok \\ nil
       ) do
    eof_is_close? = :eof in close_kinds

    case item_fun.(state, cursor, ctx, log) do
      {:ok, item, state, cursor, log} ->
        {state, cursor} = maybe_skip_eoe(state, cursor, opts, :after_item)

        separator = opts[:separator]

        case Cursor.peek(cursor) do
          {:ok, {kind, _meta, _value}, _cursor} ->
            cond do
              kind == separator ->
                {:ok, sep_tok, state, cursor} = TokenAdapter.next(state, cursor)
                {state, cursor} = maybe_skip_eoe(state, cursor, opts, :after_separator)

                case Cursor.peek(cursor) do
                  {:ok, {kind, _meta, _value}, _cursor} ->
                    if kind in close_kinds do
                      if opts[:allow_trailing_comma?] do
                        {:ok, Enum.reverse([item | acc_rev]), state, cursor, log}
                      else
                        {:error, {:trailing_comma, sep_tok}, state, cursor, log}
                      end
                    else
                      parse_items_rev(
                        [item | acc_rev],
                        state,
                        cursor,
                        ctx,
                        log,
                        close_kinds,
                        item_fun,
                        opts,
                        sep_tok
                      )
                    end

                  {:eof, _cursor} ->
                    {:error, :unexpected_eof, state, cursor, log}

                  {:error, diag, _cursor} ->
                    {:error, diag, state, cursor, log}
                end

              kind in close_kinds ->
                {:ok, Enum.reverse([item | acc_rev]), state, cursor, log}

              true ->
                if opts[:stop_on_unexpected?] do
                  {:ok, Enum.reverse([item | acc_rev]), state, cursor, log}
                else
                  expected =
                    case close_kinds do
                      [single] -> single
                      many -> many
                    end

                  {:error, {:expected_comma_or, expected, got: kind}, state, cursor, log}
                end
            end

          {:eof, _cursor} ->
            if eof_is_close? do
              {:ok, Enum.reverse([item | acc_rev]), state, cursor, log}
            else
              {:error, :unexpected_eof, state, cursor, log}
            end

          {:error, diag, _cursor} ->
            {:error, diag, state, cursor, log}
        end

      {:no_item, state, cursor, log} ->
        case after_sep_tok do
          nil ->
            case Cursor.peek(cursor) do
              {:ok, {kind, _meta, _value}, _cursor} ->
                {:error, {:expected, :item, got: kind}, state, cursor, log}

              {:eof, _cursor} ->
                if eof_is_close? do
                  {:error, {:expected, :item, got: :eof}, state, cursor, log}
                else
                  {:error, :unexpected_eof, state, cursor, log}
                end

              {:error, diag, _cursor} ->
                {:error, diag, state, cursor, log}
            end

          sep_tok ->
            case opts[:on_no_item_after_separator] do
              fun when is_function(fun, 5) ->
                fun.(sep_tok, state, cursor, ctx, log)

              _ ->
                {:error, {:expected, :item_after_separator, separator: opts[:separator]}, state,
                 cursor, log}
            end
        end

      {:error, _reason, _state, _cursor, _log} = err ->
        err
    end
  end

  defp maybe_skip_eoe(state, cursor, opts, phase) do
    override =
      case phase do
        :initial -> opts[:skip_eoe_initial?]
        :after_item -> opts[:skip_eoe_after_item?]
        :after_separator -> opts[:skip_eoe_after_separator?]
      end

    enabled? =
      case override do
        true -> true
        false -> false
        _ -> opts[:skip_eoe?]
      end

    if enabled? do
      # Only skip newlines, not semicolons - semicolons are separators
      # and should not be silently skipped in argument lists
      {state, cursor, _count} = EOE.skip_newlines_only(state, cursor, 0)
      {state, cursor}
    else
      {state, cursor}
    end
  end
end
