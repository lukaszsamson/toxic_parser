defmodule ToxicParser.Grammar.Keywords do
  @moduledoc """
  Recursive-descent helpers for keyword lists in call and data contexts.
  """

  alias ToxicParser.{Builder, Context, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.{EOE, Expressions, Strings}

  @type result ::
          {:ok, [Macro.t()], State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @type try_result ::
          {:ok, [Macro.t()], State.t(), EventLog.t()}
          | {:no_kw, State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  # Keyword-pair starters (foo:).
  #
  # Note: `do:`/`else:` keyword keys are tokenized as `:kw_identifier` with value
  # `:do`/`:else` (not as the bare `:do`/`:else` block tokens).
  @kw_kinds [:kw_identifier]

  # String start tokens that could be quoted keyword keys like "foo": or 'bar':
  @quoted_kw_start [:bin_string_start, :list_string_start]
  @no_parens_kw_terminators [
    :eoe,
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
  @spec starts_kw?(map()) :: boolean()
  def starts_kw?(%{kind: kind}) do
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
  @spec starts_kw_or_quoted_key?(map()) :: boolean()
  def starts_kw_or_quoted_key?(%{kind: kind}) do
    kind in @kw_kinds or kind in @quoted_kw_start
  end

  def starts_kw_or_quoted_key?(_), do: false

  @doc "Parses a keyword list usable in call argument position."
  @spec parse_kw_call(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_kw_call(%State{} = state, %Context{} = ctx, %EventLog{} = log) do
    # kw_call in elixir_parser.yrl ultimately parses kw_eol container_expr
    # (container_expr allows do-blocks but not no_parens extension).
    parse_kw_list([], state, ctx, log, 0, Context.container_expr())
  end

  @doc "Parses a keyword list with a minimum binding power constraint."
  @spec parse_kw_call_with_min_bp(State.t(), Pratt.context(), EventLog.t(), non_neg_integer()) ::
          result()
  def parse_kw_call_with_min_bp(%State{} = state, %Context{} = ctx, %EventLog{} = log, min_bp) do
    parse_kw_list([], state, ctx, log, min_bp, Context.container_expr())
  end

  @doc "Parses a keyword list usable in no-parens call argument position (call_args_no_parens_kw)."
  @spec parse_kw_no_parens_call(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_kw_no_parens_call(%State{} = state, %Context{} = ctx, %EventLog{} = log) do
    parse_call_args_no_parens_kw(state, ctx, log, 0)
  end

  @doc "Parses no-parens call keyword list with a minimum binding power constraint."
  @spec parse_kw_no_parens_call_with_min_bp(
          State.t(),
          Pratt.context(),
          EventLog.t(),
          non_neg_integer()
        ) ::
          result()
  def parse_kw_no_parens_call_with_min_bp(
        %State{} = state,
        %Context{} = ctx,
        %EventLog{} = log,
        min_bp
      ) do
    parse_call_args_no_parens_kw(state, ctx, log, min_bp)
  end

  @doc "Parses a keyword list usable in data (container) position."
  @spec parse_kw_data(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_kw_data(%State{} = state, %Context{} = ctx, %EventLog{} = log) do
    # Data/container context allows do-blocks but not no_parens extension.
    parse_kw_list([], state, ctx, log, 0, Context.container_expr())
  end

  @doc """
  Tries to parse `kw_data` without consuming tokens if it is not present.

  This is intended for container contexts where quoted keyword keys may be
  ambiguous at the token level (e.g. `'foo': 1` vs `'foo'`).

  Returns:

  - `{:ok, kw_list, state, log}` when keyword data is present
  - `{:no_kw, state, log}` when the next token does not start `kw_data`
  - `{:error, reason, state, log}` for real syntax errors while parsing keyword data
  """
  @spec try_parse_kw_data(State.t(), Pratt.context(), EventLog.t(), keyword()) :: try_result()
  def try_parse_kw_data(%State{} = state, %Context{} = ctx, %EventLog{} = log, opts \\ []) do
    allow_quoted_keys? = Keyword.get(opts, :allow_quoted_keys?, true)

    case TokenAdapter.peek(state) do
      {:ok, tok, state} ->
        cond do
          starts_kw?(tok) ->
            parse_kw_data(state, ctx, log)

          allow_quoted_keys? and tok.kind in @quoted_kw_start ->
            {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

            case parse_kw_data(checkpoint_state, ctx, log) do
              {:ok, kw_list, state, log} ->
                {:ok, kw_list, TokenAdapter.drop_checkpoint(state, ref), log}

              {:error, {:expected, :keyword, got: got_kind}, state, _attempt_log}
              when got_kind in @quoted_kw_start ->
                {:no_kw, TokenAdapter.rewind(state, ref), log}

              {:error, reason, state, attempt_log} ->
                {:error, reason, TokenAdapter.rewind(state, ref), attempt_log}
            end

          true ->
            {:no_kw, state, log}
        end

      {:eof, state} ->
        {:no_kw, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  @doc """
  Tries to parse `kw_call` without consuming tokens if it is not present.

  Intended for `call_args_parens` contexts, including quoted keys like `'a': 1`.
  """
  @spec try_parse_kw_call(State.t(), Pratt.context(), EventLog.t(), keyword()) :: try_result()
  def try_parse_kw_call(%State{} = state, %Context{} = ctx, %EventLog{} = log, opts \\ []) do
    allow_quoted_keys? = Keyword.get(opts, :allow_quoted_keys?, true)

    case TokenAdapter.peek(state) do
      {:ok, tok, state} ->
        cond do
          starts_kw?(tok) ->
            parse_kw_call(state, ctx, log)

          allow_quoted_keys? and tok.kind in @quoted_kw_start ->
            {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

            case parse_kw_call(checkpoint_state, ctx, log) do
              {:ok, kw_list, state, log} ->
                {:ok, kw_list, TokenAdapter.drop_checkpoint(state, ref), log}

              {:error, {:expected, :keyword, got: got_kind}, state, _attempt_log}
              when got_kind in @quoted_kw_start ->
                {:no_kw, TokenAdapter.rewind(state, ref), log}

              {:error, reason, state, attempt_log} ->
                {:error, reason, TokenAdapter.rewind(state, ref), attempt_log}
            end

          true ->
            {:no_kw, state, log}
        end

      {:eof, state} ->
        {:no_kw, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  @doc """
  Tries to parse `call_args_no_parens_kw` without consuming tokens if it is not present.

  This is used for:

  - `no_parens_op_expr -> when_op_eol call_args_no_parens_kw`
  - no-parens call args (`foo a: 1`)
  - stab parens args (`fn (a: 1) -> ... end`)
  """
  @spec try_parse_kw_no_parens_call(State.t(), Pratt.context(), EventLog.t(), keyword()) ::
          try_result()
  def try_parse_kw_no_parens_call(
        %State{} = state,
        %Context{} = ctx,
        %EventLog{} = log,
        opts \\ []
      ) do
    allow_quoted_keys? = Keyword.get(opts, :allow_quoted_keys?, true)
    min_bp = Keyword.get(opts, :min_bp, 0)

    case TokenAdapter.peek(state) do
      {:ok, tok, state} ->
        cond do
          starts_kw?(tok) ->
            parse_call_args_no_parens_kw(state, ctx, log, min_bp)

          allow_quoted_keys? and tok.kind in @quoted_kw_start ->
            {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

            case parse_call_args_no_parens_kw(checkpoint_state, ctx, log, min_bp) do
              {:ok, kw_list, state, log} ->
                {:ok, kw_list, TokenAdapter.drop_checkpoint(state, ref), log}

              {:error, {:expected, :keyword, got: got_kind}, state, _attempt_log}
              when got_kind in @quoted_kw_start ->
                {:no_kw, TokenAdapter.rewind(state, ref), log}

              {:error, reason, state, attempt_log} ->
                {:error, reason, TokenAdapter.rewind(state, ref), attempt_log}
            end

          true ->
            {:no_kw, state, log}
        end

      {:eof, state} ->
        {:no_kw, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  @spec parse_call_args_no_parens_kw(State.t(), Pratt.context(), EventLog.t(), non_neg_integer()) ::
          result()
  defp parse_call_args_no_parens_kw(%State{} = state, %Context{} = ctx, %EventLog{} = log, min_bp) do
    value_ctx = Context.kw_no_parens_value()

    with {:ok, pair, state, log} <- parse_kw_pair(state, ctx, log, min_bp, value_ctx) do
      parse_call_args_no_parens_kw_pairs([pair], state, ctx, log, min_bp, value_ctx)
    end
  end

  defp parse_call_args_no_parens_kw_pairs(acc, state, ctx, log, min_bp, value_ctx) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :","} = comma_tok, _} ->
        {:ok, _comma, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        case TokenAdapter.peek(state) do
          {:ok, %{kind: kind}, _} when kind in @no_parens_kw_terminators ->
            meta = Builder.Helpers.token_meta(comma_tok.metadata)
            {:error, syntax_error_before(meta), state, log}

          {:ok, _tok, _} ->
            case try_parse_no_parens_kw_pair(state, ctx, log, min_bp, value_ctx) do
              {:ok, pair, state, log} ->
                parse_call_args_no_parens_kw_pairs(
                  [pair | acc],
                  state,
                  ctx,
                  log,
                  min_bp,
                  value_ctx
                )

              {:no_kw, state, log} ->
                meta = Builder.Helpers.token_meta(comma_tok.metadata)
                {:error, {meta, unexpected_expression_after_kw_call_message(), "','"}, state, log}

              {:error, reason, state, log} ->
                {:error, reason, state, log}
            end

          {:eof, state} ->
            meta = Builder.Helpers.token_meta(comma_tok.metadata)
            {:error, syntax_error_before(meta), state, log}

          {:error, diag, state} ->
            {:error, diag, state, log}
        end

      {:ok, _tok, state} ->
        {:ok, Enum.reverse(acc), state, log}

      {:eof, state} ->
        {:ok, Enum.reverse(acc), state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp try_parse_no_parens_kw_pair(state, ctx, log, min_bp, value_ctx) do
    case TokenAdapter.peek(state) do
      {:ok, tok, state} ->
        cond do
          starts_kw?(tok) ->
            parse_kw_pair(state, ctx, log, min_bp, value_ctx)

          tok.kind in @quoted_kw_start ->
            {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

            case parse_kw_pair(checkpoint_state, ctx, log, min_bp, value_ctx) do
              {:ok, pair, state, log} ->
                {:ok, pair, TokenAdapter.drop_checkpoint(state, ref), log}

              {:error, {:expected, :keyword, got: got_kind}, state, _attempt_log}
              when got_kind in @quoted_kw_start ->
                {:no_kw, TokenAdapter.rewind(state, ref), log}

              {:error, reason, state, attempt_log} ->
                {:error, reason, TokenAdapter.rewind(state, ref), attempt_log}
            end

          true ->
            {:no_kw, state, log}
        end

      {:eof, state} ->
        {:no_kw, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
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

  defp parse_kw_list(acc, state, ctx, log, min_bp, value_ctx) do
    case parse_kw_pair(state, ctx, log, min_bp, value_ctx) do
      {:ok, pair, state, log} ->
        case TokenAdapter.peek(state) do
          {:ok, %{kind: :","}, _} ->
            {:ok, _comma, state} = TokenAdapter.next(state)
            # Check for trailing comma - if next is a terminator, stop
            case TokenAdapter.peek(state) do
              {:ok, %{kind: kind}, _} when kind in [:eoe, :")", :"]", :"}", :">>"] ->
                {:ok, Enum.reverse([pair | acc]), state, log}

              _ ->
                parse_kw_list([pair | acc], state, ctx, log, min_bp, value_ctx)
            end

          _ ->
            {:ok, Enum.reverse([pair | acc]), state, log}
        end

      other ->
        other
    end
  end

  defp parse_kw_pair(state, _ctx, log, min_bp, value_ctx) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: kind}, _} when kind in @kw_kinds ->
        # Standard keyword like foo:
        {:ok, %{value: key}, state} = TokenAdapter.next(state)
        parse_kw_value(key, state, log, min_bp, value_ctx)

      {:ok, %{kind: kind}, _} when kind in @quoted_kw_start ->
        # Potentially a quoted keyword like "foo": or 'bar':
        # Try parsing as string - if it returns keyword_key, it's a keyword
        case Strings.parse(state, Context.matched_expr(), log, 10000) do
          {:keyword_key, key_atom, state, log} ->
            parse_kw_value(key_atom, state, log, min_bp, value_ctx)

          {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, log} ->
            parse_kw_value_interpolated(
              parts,
              kind,
              start_meta,
              delimiter,
              state,
              log,
              min_bp,
              value_ctx
            )

          {:ok, _ast, state, _log} ->
            # It was a regular string, not a keyword key
            {:error, {:expected, :keyword, got: kind}, state, log}

          {:error, reason, state, log} ->
            {:error, reason, state, log}
        end

      {:ok, %{kind: kind}, _} ->
        {:ok, _tok, state} = TokenAdapter.next(state)
        {:error, {:expected, :keyword, got: kind}, state, log}

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse keyword value after the key has been determined
  defp parse_kw_value(key, state, log, min_bp, value_ctx) do
    state = EOE.skip(state)

    # Use min_bp if provided to stop parsing at certain operators (e.g., ->)
    # The value context depends on where the keyword list appears in the grammar.
    result =
      if min_bp > 0 do
        Pratt.parse_with_min_bp(state, value_ctx, log, min_bp)
      else
        Expressions.expr(state, value_ctx, log)
      end

    with {:ok, value_ast, state, log} <- result do
      key_ast = Builder.Helpers.literal(key)
      {:ok, {key_ast, value_ast}, state, log}
    end
  end

  # Parse keyword value for interpolated key like "foo#{x}":
  defp parse_kw_value_interpolated(
         parts,
         kind,
         start_meta,
         delimiter,
         state,
         log,
         min_bp,
         value_ctx
       ) do
    state = EOE.skip(state)

    result =
      if min_bp > 0 do
        Pratt.parse_with_min_bp(state, value_ctx, log, min_bp)
      else
        Expressions.expr(state, value_ctx, log)
      end

    with {:ok, value_ast, state, log} <- result do
      # Build interpolated key AST
      key_ast = Expressions.build_interpolated_keyword_key(parts, kind, start_meta, delimiter)
      {:ok, {key_ast, value_ast}, state, log}
    end
  end
end
