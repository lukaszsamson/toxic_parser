defmodule ToxicParser.Grammar.Keywords do
  @moduledoc """
  Recursive-descent helpers for keyword lists in call and data contexts.
  """

  alias ToxicParser.{
    Builder,
    Context,
    Cursor,
    EventLog,
    ExprClass,
    NoParensErrors,
    ParseOpts,
    Pratt,
    State,
    TokenAdapter
  }

  # TokenAdapter used for next, checkpoint, rewind, pushback_many, token_meta

  alias ToxicParser.Grammar.{EOE, Expressions, Strings}

  @type result ::
          {:ok, [Macro.t()], State.t(), ToxicParser.Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), ToxicParser.Cursor.t(), EventLog.t()}

  @type try_result ::
          {:ok, [Macro.t()], State.t(), ToxicParser.Cursor.t(), EventLog.t()}
          | {:no_kw, State.t(), ToxicParser.Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), ToxicParser.Cursor.t(), EventLog.t()}

  # Keyword-pair starters (foo:).
  #
  # Note: `do:`/`else:` keyword keys are tokenized as `:kw_identifier` with value
  # `:do`/`:else` (not as the bare `:do`/`:else` block tokens).
  @kw_kinds [:kw_identifier]

  # String start tokens that could be quoted keyword keys like "foo": or 'bar':
  @quoted_kw_start [:bin_string_start, :list_string_start]

  @quoted_kw_keyword_end [:kw_identifier_safe_end, :kw_identifier_unsafe_end]
  @quoted_kw_string_end [:bin_string_end, :list_string_end]
  @quoted_kw_scan_max 512
  @no_parens_kw_terminators [
    :eof,
    :eol,
    :";",
    :")",
    :"]",
    :"}",
    :do,
    :end,
    :end_interpolation,
    :when_op,
    :stab_op
  ]

  @doc "Checks if an expression result is a keyword list (from quoted keyword parsing)"
  # Must check that first element is a tuple to distinguish from charlists like [114]
  defguard is_keyword_list_result(arg) when is_list(arg) and arg != [] and is_tuple(hd(arg))

  @doc "Returns true if the token kind starts a keyword pair."
  @spec starts_kw?(tuple()) :: boolean()
  def starts_kw?({kind, _meta, _value}) do
    # Note: quoted strings like "foo": also start keywords, but we can't tell
    # without lookahead. Use starts_kw_or_quoted_key? in container contexts.
    kind in @kw_kinds
  end

  def starts_kw?(_), do: false

  @doc """
  Returns true if the token kind starts a keyword pair OR could be a quoted keyword key.
  Use this in container contexts (tuples, maps) where quoted keyword keys are valid.
  For call arguments, use starts_kw? instead.
  """
  @spec starts_kw_or_quoted_key?(tuple()) :: boolean()
  def starts_kw_or_quoted_key?({kind, _meta, _value}) do
    kind in @kw_kinds or kind in @quoted_kw_start
  end

  def starts_kw_or_quoted_key?(_), do: false

  @spec quoted_string_is_keyword?(State.t(), ToxicParser.Cursor.t()) ::
          {:keyword | :not_keyword, State.t(), ToxicParser.Cursor.t()}
  defp quoted_string_is_keyword?(%State{} = state, cursor) do
    scan_to_string_end(state, cursor, [], 0, @quoted_kw_scan_max)
  end

  defp scan_to_string_end(state, cursor, consumed, _interp_depth, max) when max <= 0 do
    {state, cursor} = TokenAdapter.pushback_many(state, cursor, Enum.reverse(consumed))
    {:not_keyword, state, cursor}
  end

  defp scan_to_string_end(state, cursor, consumed, interp_depth, max) do
    case TokenAdapter.next(state, cursor) do
      {:ok, {kind, _meta, _value} = tok, state, cursor} ->
        consumed = [tok | consumed]

        cond do
          kind == :begin_interpolation ->
            scan_to_string_end(state, cursor, consumed, interp_depth + 1, max - 1)

          kind == :end_interpolation ->
            scan_to_string_end(state, cursor, consumed, max(interp_depth - 1, 0), max - 1)

          interp_depth > 0 ->
            scan_to_string_end(state, cursor, consumed, interp_depth, max - 1)

          kind in @quoted_kw_keyword_end ->
            {state, cursor} = TokenAdapter.pushback_many(state, cursor, Enum.reverse(consumed))
            {:keyword, state, cursor}

          kind in @quoted_kw_string_end ->
            {state, cursor} = TokenAdapter.pushback_many(state, cursor, Enum.reverse(consumed))
            {:not_keyword, state, cursor}

          true ->
            scan_to_string_end(state, cursor, consumed, interp_depth, max - 1)
        end

      {:eof, state, cursor} ->
        {state, cursor} = TokenAdapter.pushback_many(state, cursor, Enum.reverse(consumed))
        {:not_keyword, state, cursor}

      {:error, _diag, state, cursor} ->
        {state, cursor} = TokenAdapter.pushback_many(state, cursor, Enum.reverse(consumed))
        {:not_keyword, state, cursor}
    end
  end

  @doc "Parses a keyword list usable in call argument position."
  @spec parse_kw_call(State.t(), ToxicParser.Cursor.t(), Pratt.context(), EventLog.t(), keyword()) ::
          result()
  def parse_kw_call(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log, opts \\ []) do
    # elixir_parser.yrl: kw_base -> kw_eol container_expr
    # For stab patterns, allow no_parens values (error_kind = nil skips validation)
    error_kind = if Keyword.get(opts, :allow_no_parens, false), do: nil, else: :many
    parse_kw_list([], state, cursor, ctx, log, 0, Context.container_expr(), error_kind)
  end

  @doc "Parses a keyword list with a minimum binding power constraint."
  @spec parse_kw_call_with_min_bp(
          State.t(),
          ToxicParser.Cursor.t(),
          Pratt.context(),
          EventLog.t(),
          non_neg_integer(),
          keyword()
        ) ::
          result()
  def parse_kw_call_with_min_bp(
        %State{} = state,
        cursor,
        %Context{} = ctx,
        %EventLog{} = log,
        min_bp,
        opts \\ []
      ) do
    # For stab patterns, allow no_parens values (error_kind = nil skips validation)
    error_kind = if Keyword.get(opts, :allow_no_parens, false), do: nil, else: :many
    parse_kw_list([], state, cursor, ctx, log, min_bp, Context.container_expr(), error_kind)
  end

  @doc "Parses a keyword list usable in no-parens call argument position (call_args_no_parens_kw)."
  @spec parse_kw_no_parens_call(State.t(), ToxicParser.Cursor.t(), Pratt.context(), EventLog.t()) ::
          result()
  def parse_kw_no_parens_call(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log) do
    parse_call_args_no_parens_kw(state, cursor, ctx, log, 0)
  end

  @doc "Parses no-parens call keyword list with a minimum binding power constraint."
  @spec parse_kw_no_parens_call_with_min_bp(
          State.t(),
          ToxicParser.Cursor.t(),
          Pratt.context(),
          EventLog.t(),
          non_neg_integer()
        ) ::
          result()
  def parse_kw_no_parens_call_with_min_bp(
        %State{} = state,
        cursor,
        %Context{} = ctx,
        %EventLog{} = log,
        min_bp
      ) do
    parse_call_args_no_parens_kw(state, cursor, ctx, log, min_bp)
  end

  @doc "Parses a keyword list usable in data (container) position."
  @spec parse_kw_data(State.t(), ToxicParser.Cursor.t(), Pratt.context(), EventLog.t()) ::
          result()
  def parse_kw_data(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log) do
    # elixir_parser.yrl: kw_base -> kw_eol container_expr
    parse_kw_list([], state, cursor, ctx, log, 0, Context.container_expr(), :container)
  end

  @doc """
  Tries to parse `kw_data` without consuming tokens if it is not present.

  This is intended for container contexts where quoted keyword keys may be
  ambiguous at the token level (e.g. `'foo': 1` vs `'foo'`).

  Returns:

  - `{:ok, kw_list, state, cursor, log}` when keyword data is present
  - `{:no_kw, state, cursor, log}` when the next token does not start `kw_data`
  - `{:error, reason, state, cursor, log}` for real syntax errors while parsing keyword data
  """
  @spec try_parse_kw_data(
          State.t(),
          ToxicParser.Cursor.t(),
          Pratt.context(),
          EventLog.t(),
          keyword()
        ) ::
          try_result()
  def try_parse_kw_data(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log, opts \\ []) do
    allow_quoted_keys? = Keyword.get(opts, :allow_quoted_keys?, true)

    case Cursor.peek(cursor) do
      {:ok, {kind, _meta, _value} = tok, cursor} ->
        cond do
          starts_kw?(tok) ->
            parse_kw_data(state, cursor, ctx, log)

          allow_quoted_keys? and kind in @quoted_kw_start ->
            case quoted_string_is_keyword?(state, cursor) do
              {:keyword, state, cursor} ->
                parse_kw_data(state, cursor, ctx, log)

              {:not_keyword, state, cursor} ->
                {:no_kw, state, cursor, log}
            end

          true ->
            {:no_kw, state, cursor, log}
        end

      {:eof, cursor} ->
        {:no_kw, state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  @doc """
  Tries to parse `kw_call` without consuming tokens if it is not present.

  Intended for `call_args_parens` contexts, including quoted keys like `'a': 1`.
  """
  @spec try_parse_kw_call(
          State.t(),
          ToxicParser.Cursor.t(),
          Pratt.context(),
          EventLog.t(),
          keyword()
        ) ::
          try_result()
  def try_parse_kw_call(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log, opts \\ []) do
    allow_quoted_keys? = Keyword.get(opts, :allow_quoted_keys?, true)

    case Cursor.peek(cursor) do
      {:ok, {kind, _meta, _value} = tok, cursor} ->
        cond do
          starts_kw?(tok) ->
            parse_kw_call(state, cursor, ctx, log)

          allow_quoted_keys? and kind in @quoted_kw_start ->
            case quoted_string_is_keyword?(state, cursor) do
              {:keyword, state, cursor} ->
                parse_kw_call(state, cursor, ctx, log)

              {:not_keyword, state, cursor} ->
                {:no_kw, state, cursor, log}
            end

          true ->
            {:no_kw, state, cursor, log}
        end

      {:eof, cursor} ->
        {:no_kw, state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  @doc """
  Tries to parse `call_args_no_parens_kw_expr` (a single keyword pair) without consuming tokens
  if it is not present.

  Grammar (elixir_parser.yrl):

  - `call_args_no_parens_kw_expr -> kw_eol matched_expr`
  - `call_args_no_parens_kw_expr -> kw_eol no_parens_expr`

  This context does not allow `unmatched_expr`, but it does allow `no_parens_expr` in the value.
  """
  @type kw_expr_try_result ::
          {:ok, {term(), term()}, State.t(), ToxicParser.Cursor.t(), EventLog.t()}
          | {:no_kw_expr, State.t(), ToxicParser.Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), ToxicParser.Cursor.t(), EventLog.t()}

  @spec try_parse_call_args_no_parens_kw_expr(
          State.t(),
          ToxicParser.Cursor.t(),
          Pratt.context(),
          EventLog.t(),
          keyword()
        ) ::
          kw_expr_try_result()
  def try_parse_call_args_no_parens_kw_expr(
        %State{} = state,
        cursor,
        %Context{} = ctx,
        %EventLog{} = log,
        opts \\ []
      ) do
    allow_quoted_keys? = Keyword.get(opts, :allow_quoted_keys?, true)
    min_bp = Keyword.get(opts, :min_bp, 0)
    value_ctx = Context.kw_no_parens_value()
    # no-parens keyword values allow no_parens_expr, so no validation (error_kind = nil)
    error_kind = nil

    case Cursor.peek(cursor) do
      {:ok, {kind, _meta, _value} = tok, cursor} ->
        cond do
          starts_kw?(tok) ->
            case parse_kw_pair(state, cursor, ctx, log, min_bp, value_ctx, error_kind) do
              {:ok, pair, state, cursor, log} -> {:ok, pair, state, cursor, log}
              {:error, reason, state, cursor, log} -> {:error, reason, state, cursor, log}
            end

          allow_quoted_keys? and kind in @quoted_kw_start ->
            case quoted_string_is_keyword?(state, cursor) do
              {:keyword, state, cursor} ->
                case parse_kw_pair(state, cursor, ctx, log, min_bp, value_ctx, error_kind) do
                  {:ok, pair, state, cursor, log} -> {:ok, pair, state, cursor, log}
                  {:error, reason, state, cursor, log} -> {:error, reason, state, cursor, log}
                end

              {:not_keyword, state, cursor} ->
                {:no_kw_expr, state, cursor, log}
            end

          true ->
            {:no_kw_expr, state, cursor, log}
        end

      {:eof, cursor} ->
        {:no_kw_expr, state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  @doc """
  Tries to parse `call_args_no_parens_kw` without consuming tokens if it is not present.
  """
  @spec try_parse_call_args_no_parens_kw(
          State.t(),
          ToxicParser.Cursor.t(),
          Pratt.context(),
          EventLog.t(),
          keyword()
        ) ::
          try_result()
  def try_parse_call_args_no_parens_kw(
        %State{} = state,
        cursor,
        %Context{} = ctx,
        %EventLog{} = log,
        opts \\ []
      ) do
    allow_quoted_keys? = Keyword.get(opts, :allow_quoted_keys?, true)
    min_bp = Keyword.get(opts, :min_bp, 0)

    case Cursor.peek(cursor) do
      {:ok, {kind, _meta, _value} = tok, cursor} ->
        cond do
          # Definite keyword (kw_identifier) - no checkpoint needed.
          starts_kw?(tok) ->
            case parse_call_args_no_parens_kw(state, cursor, ctx, log, min_bp) do
              {:ok, kw_list, state, cursor, log} -> {:ok, kw_list, state, cursor, log}
              {:error, reason, state, cursor, log} -> {:error, reason, state, cursor, log}
            end

          # Quoted key - only parse if we can prove it ends with a colon.
          allow_quoted_keys? and kind in @quoted_kw_start ->
            case quoted_string_is_keyword?(state, cursor) do
              {:keyword, state, cursor} ->
                case parse_call_args_no_parens_kw(state, cursor, ctx, log, min_bp) do
                  {:ok, kw_list, state, cursor, log} -> {:ok, kw_list, state, cursor, log}
                  {:error, reason, state, cursor, log} -> {:error, reason, state, cursor, log}
                end

              {:not_keyword, state, cursor} ->
                {:no_kw, state, cursor, log}
            end

          true ->
            {:no_kw, state, cursor, log}
        end

      {:eof, cursor} ->
        {:no_kw, state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  @spec parse_call_args_no_parens_kw(
          State.t(),
          ToxicParser.Cursor.t(),
          Pratt.context(),
          EventLog.t(),
          non_neg_integer()
        ) ::
          result()
  defp parse_call_args_no_parens_kw(
         %State{} = state,
         cursor,
         %Context{} = ctx,
         %EventLog{} = log,
         min_bp
       ) do
    item_fun = fn state, cursor, ctx, log ->
      case try_parse_call_args_no_parens_kw_expr(state, cursor, ctx, log, min_bp: min_bp) do
        {:ok, pair, state, cursor, log} -> {:ok, pair, state, cursor, log}
        {:no_kw_expr, state, cursor, log} -> {:no_item, state, cursor, log}
        {:error, reason, state, cursor, log} -> {:error, reason, state, cursor, log}
      end
    end

    on_no_item_after_separator = fn comma_tok, state, cursor, _ctx, log ->
      meta = TokenAdapter.token_meta(comma_tok)
      {:error, {meta, unexpected_expression_after_kw_call_message(), "','"}, state, cursor, log}
    end

    case ToxicParser.Grammar.Delimited.parse_comma_separated(
           state,
           cursor,
           ctx,
           log,
           @no_parens_kw_terminators,
           item_fun,
           allow_trailing_comma?: false,
           skip_eoe_initial?: false,
           skip_eoe_after_item?: false,
           skip_eoe_after_separator?: true,
           stop_on_unexpected?: true,
           on_no_item_after_separator: on_no_item_after_separator
         ) do
      {:ok, kw_list, state, cursor, log} ->
        {:ok, kw_list, state, cursor, log}

      {:error, {:trailing_comma, comma_tok}, state, cursor, log} ->
        meta = TokenAdapter.token_meta(comma_tok)
        {:error, syntax_error_before(meta), state, cursor, log}

      {:error, reason, state, cursor, log} ->
        {:error, reason, state, cursor, log}
    end
  end

  defp syntax_error_before(meta) do
    line = Keyword.get(meta, :line, 1)
    column = Keyword.get(meta, :column, 1)
    {[line: line, column: column], "syntax error before: ", ""}
  end

  defp unexpected_expression_after_kw_call_message do
    "unexpected expression after keyword list. Keyword lists must always come as the last argument. " <>
      "Therefore, this is not allowed:\n\n" <>
      "    function_call(1, some: :option, 2)\n\n" <>
      "Instead, wrap the keyword in brackets:\n\n" <>
      "    function_call(1, [some: :option], 2)\n\n" <>
      "Syntax error after: "
  end

  defp parse_kw_list(acc, state, cursor, ctx, log, min_bp, value_ctx, error_kind) do
    case parse_kw_pair(state, cursor, ctx, log, min_bp, value_ctx, error_kind) do
      {:ok, pair, state, cursor, log} ->
        case Cursor.peek(cursor) do
          {:ok, {:",", _meta, _value}, cursor} ->
            {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)
            # Check for trailing comma - if next is a terminator, stop
            case Cursor.peek(cursor) do
              {:ok, {kind, _meta, _value}, cursor}
              when kind in [:eol, :";", :")", :"]", :"}", :">>"] ->
                {:ok, Enum.reverse([pair | acc]), state, cursor, log}

              {:ok, _, cursor} ->
                parse_kw_list(
                  [pair | acc],
                  state,
                  cursor,
                  ctx,
                  log,
                  min_bp,
                  value_ctx,
                  error_kind
                )
            end

          {:ok, _, cursor} ->
            {:ok, Enum.reverse([pair | acc]), state, cursor, log}
        end

      other ->
        other
    end
  end

  defp parse_kw_pair(state, cursor, _ctx, log, min_bp, value_ctx, error_kind) do
    case Cursor.peek(cursor) do
      {:ok, {kind, _meta, key}, cursor} when kind in @kw_kinds ->
        # Standard keyword like foo:
        # Add format: :keyword for token_metadata compatibility
        {:ok, tok, state, cursor} = TokenAdapter.next(state, cursor)
        key_meta = TokenAdapter.token_meta(tok)
        key_meta_kw = [{:format, :keyword} | key_meta]
        parse_kw_value(key, key_meta_kw, state, cursor, log, min_bp, value_ctx, error_kind)

      {:ok, {kind, _meta, _value}, cursor} when kind in @quoted_kw_start ->
        # Potentially a quoted keyword like "foo": or 'bar':
        # Try parsing as string - if it returns keyword_key, it's a keyword
        case Strings.parse(state, cursor, Context.matched_expr(), log, 10000) do
          {:keyword_key, key_atom, key_meta, state, cursor, log} ->
            parse_kw_value(key_atom, key_meta, state, cursor, log, min_bp, value_ctx, error_kind)

          {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, cursor, log} ->
            parse_kw_value_interpolated(
              parts,
              kind,
              start_meta,
              delimiter,
              state,
              cursor,
              log,
              min_bp,
              value_ctx,
              error_kind
            )

          {:ok, _ast, state, cursor, _log} ->
            # It was a regular string, not a keyword key
            {:error, {:expected, :keyword, got: kind}, state, cursor, log}

          {:error, reason, state, cursor, log} ->
            {:error, reason, state, cursor, log}
        end

      {:ok, {kind, _meta, _value}, cursor} ->
        {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)
        {:error, {:expected, :keyword, got: kind}, state, cursor, log}

      {:eof, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  # Parse keyword value after the key has been determined
  defp parse_kw_value(key, key_meta, state, cursor, log, min_bp, value_ctx, error_kind) do
    {state, cursor} = EOE.skip(state, cursor)

    # Use min_bp if provided to stop parsing at certain operators (e.g., ->)
    # The value context depends on where the keyword list appears in the grammar.
    result =
      if min_bp > 0 do
        Pratt.parse_with_min_bp(state, cursor, value_ctx, log, min_bp)
      else
        Pratt.parse_with_min_bp(state, cursor, value_ctx, log, 0, ParseOpts.stop_at_assoc())
      end

    with {:ok, value_ast, state, cursor, log} <- result do
      # Validate no_parens expressions are not allowed in container/call contexts
      # (error_kind = nil means skip validation, e.g., for no-parens keyword values)
      if error_kind != nil and ExprClass.classify(value_ast) == :no_parens do
        error =
          case error_kind do
            :container -> NoParensErrors.error_no_parens_container_strict(value_ast)
            :many -> NoParensErrors.error_no_parens_many_strict(value_ast)
          end

        {:error, error, state, cursor, log}
      else
        key_ast = Builder.Helpers.literal(key, key_meta, state)
        {:ok, {key_ast, value_ast}, state, cursor, log}
      end
    end
  end

  # Parse keyword value for interpolated key like "foo#{x}":
  defp parse_kw_value_interpolated(
         parts,
         kind,
         start_meta,
         delimiter,
         state,
         cursor,
         log,
         min_bp,
         value_ctx,
         error_kind
       ) do
    {state, cursor} = EOE.skip(state, cursor)

    result =
      if min_bp > 0 do
        raise "dead code"
        Pratt.parse_with_min_bp(state, cursor, value_ctx, log, min_bp)
      else
        Pratt.parse_with_min_bp(state, cursor, value_ctx, log, 0, ParseOpts.stop_at_assoc())
      end

    with {:ok, value_ast, state, cursor, log} <- result do
      # Validate no_parens expressions are not allowed in container/call contexts
      # (error_kind = nil means skip validation, e.g., for no-parens keyword values)
      if error_kind != nil and ExprClass.classify(value_ast) == :no_parens do
        error =
          case error_kind do
            :container -> NoParensErrors.error_no_parens_container_strict(value_ast)
            :many -> NoParensErrors.error_no_parens_many_strict(value_ast)
          end

        {:error, error, state, cursor, log}
      else
        # Build interpolated key AST
        key_ast = Expressions.build_interpolated_keyword_key(parts, kind, start_meta, delimiter)
        {:ok, {key_ast, value_ast}, state, cursor, log}
      end
    end
  end
end
