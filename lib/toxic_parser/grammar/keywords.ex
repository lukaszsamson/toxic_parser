defmodule ToxicParser.Grammar.Keywords do
  @moduledoc """
  Recursive-descent helpers for keyword lists in call and data contexts.
  """

  alias ToxicParser.{Builder, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.{Expressions, Strings}

  @type result ::
          {:ok, [Macro.t()], State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  # Keyword-pair starters (foo:).
  #
  # Note: `do:`/`else:` keyword keys are tokenized as `:kw_identifier` with value
  # `:do`/`:else` (not as the bare `:do`/`:else` block tokens).
  @kw_kinds [:kw_identifier, :kw_identifier_safe, :kw_identifier_unsafe]

  # String start tokens that could be quoted keyword keys like "foo": or 'bar':
  @quoted_kw_start [:bin_string_start, :list_string_start]

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
  def parse_kw_call(%State{} = state, ctx, %EventLog{} = log) do
    # kw_call in elixir_parser.yrl ultimately parses kw_eol container_expr
    # (container_expr includes unmatched_expr, which includes block_expr).
    parse_kw_list([], state, ctx, log, 0, :unmatched)
  end

  @doc "Parses a keyword list with a minimum binding power constraint."
  @spec parse_kw_call_with_min_bp(State.t(), Pratt.context(), EventLog.t(), non_neg_integer()) ::
          result()
  def parse_kw_call_with_min_bp(%State{} = state, ctx, %EventLog{} = log, min_bp) do
    parse_kw_list([], state, ctx, log, min_bp, :unmatched)
  end

  @doc "Parses a keyword list usable in no-parens call argument position (call_args_no_parens_kw)."
  @spec parse_kw_no_parens_call(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_kw_no_parens_call(%State{} = state, ctx, %EventLog{} = log) do
    # call_args_no_parens_kw_expr in elixir_parser.yrl parses kw_eol matched_expr.
    parse_kw_list([], state, ctx, log, 0, :matched)
  end

  @doc "Parses no-parens call keyword list with a minimum binding power constraint."
  @spec parse_kw_no_parens_call_with_min_bp(State.t(), Pratt.context(), EventLog.t(), non_neg_integer()) ::
          result()
  def parse_kw_no_parens_call_with_min_bp(%State{} = state, ctx, %EventLog{} = log, min_bp) do
    parse_kw_list([], state, ctx, log, min_bp, :matched)
  end

  @doc "Parses a keyword list usable in data (container) position."
  @spec parse_kw_data(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_kw_data(%State{} = state, ctx, %EventLog{} = log) do
    if System.get_env("TP_DEBUG_KW_DATA") == "1" and state.source == "{1, foo: 1, bar: 2}" do
      case TokenAdapter.peek(state) do
        {:ok, tok, _} ->
          IO.puts(
            "TP_DEBUG_KW_DATA enter parse_kw_data next_kind=#{inspect(tok.kind)} next_value=#{inspect(tok.value)}"
          )

        _ ->
          IO.puts("TP_DEBUG_KW_DATA enter parse_kw_data next=<none>")
      end
    end

    parse_kw_list([], state, ctx, log, 0, :unmatched)
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
        case Strings.parse(state, :matched, log, 10000) do
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
    state = skip_eoe(state)

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
    state = skip_eoe(state)

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

  defp skip_eoe(state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe}, _} ->
        {:ok, _tok, state} = TokenAdapter.next(state)
        skip_eoe(state)

      _ ->
        state
    end
  end
end
