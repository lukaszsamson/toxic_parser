defmodule ToxicParser.Grammar.Keywords do
  @moduledoc """
  Recursive-descent helpers for keyword lists in call and data contexts.
  """

  alias ToxicParser.{Builder, Context, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.{EOE, Expressions, Strings}

  @type result ::
          {:ok, [Macro.t()], State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  # Keyword-pair starters (foo:).
  #
  # Note: `do:`/`else:` keyword keys are tokenized as `:kw_identifier` with value
  # `:do`/`:else` (not as the bare `:do`/`:else` block tokens).
  @kw_kinds [:kw_identifier]

  # String start tokens that could be quoted keyword keys like "foo": or 'bar':
  @quoted_kw_start [:bin_string_start, :list_string_start]

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
  def parse_kw_call(%State{} = state, ctx, %EventLog{} = log) do
    # kw_call in elixir_parser.yrl ultimately parses kw_eol container_expr
    # (container_expr allows do-blocks but not no_parens extension).
    parse_kw_list([], state, ctx, log, 0, Context.container_expr())
  end

  @doc "Parses a keyword list with a minimum binding power constraint."
  @spec parse_kw_call_with_min_bp(State.t(), Pratt.context(), EventLog.t(), non_neg_integer()) ::
          result()
  def parse_kw_call_with_min_bp(%State{} = state, ctx, %EventLog{} = log, min_bp) do
    parse_kw_list([], state, ctx, log, min_bp, Context.container_expr())
  end

  @doc "Parses a keyword list usable in no-parens call argument position (call_args_no_parens_kw)."
  @spec parse_kw_no_parens_call(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_kw_no_parens_call(%State{} = state, ctx, %EventLog{} = log) do
    # call_args_no_parens_kw_expr in elixir_parser.yrl parses kw_eol matched_expr.
    # We use kw_no_parens_value to allow no_parens extension but not do-blocks.
    parse_kw_list([], state, ctx, log, 0, Context.kw_no_parens_value())
  end

  @doc "Parses no-parens call keyword list with a minimum binding power constraint."
  @spec parse_kw_no_parens_call_with_min_bp(
          State.t(),
          Pratt.context(),
          EventLog.t(),
          non_neg_integer()
        ) ::
          result()
  def parse_kw_no_parens_call_with_min_bp(%State{} = state, ctx, %EventLog{} = log, min_bp) do
    parse_kw_list([], state, ctx, log, min_bp, Context.kw_no_parens_value())
  end

  @doc "Parses a keyword list usable in data (container) position."
  @spec parse_kw_data(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_kw_data(%State{} = state, ctx, %EventLog{} = log) do
    # Data/container context allows do-blocks but not no_parens extension.
    parse_kw_list([], state, ctx, log, 0, Context.container_expr())
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
