defmodule ToxicParser.Grammar.Keywords do
  @moduledoc """
  Recursive-descent helpers for keyword lists in call and data contexts.
  """

  alias ToxicParser.{Builder, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Grammar.Expressions

  @type result ::
          {:ok, [Macro.t()], State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @kw_kinds [:kw_identifier, :kw_identifier_safe, :kw_identifier_unsafe]

  @doc "Returns true if the token kind starts a keyword pair."
  @spec starts_kw?(map()) :: boolean()
  def starts_kw?(%{kind: kind}) do
    kind in @kw_kinds
  end

  def starts_kw?(_), do: false

  @doc "Parses a keyword list usable in call argument position."
  @spec parse_kw_call(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_kw_call(%State{} = state, ctx, %EventLog{} = log) do
    parse_kw_list([], state, ctx, log, 0)
  end

  @doc "Parses a keyword list with a minimum binding power constraint."
  @spec parse_kw_call_with_min_bp(State.t(), Pratt.context(), EventLog.t(), non_neg_integer()) :: result()
  def parse_kw_call_with_min_bp(%State{} = state, ctx, %EventLog{} = log, min_bp) do
    parse_kw_list([], state, ctx, log, min_bp)
  end

  @doc "Parses a keyword list usable in data (container) position."
  @spec parse_kw_data(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_kw_data(%State{} = state, ctx, %EventLog{} = log) do
    parse_kw_list([], state, ctx, log, 0)
  end

  defp parse_kw_list(acc, state, ctx, log, min_bp) do
    case parse_kw_pair(state, ctx, log, min_bp) do
      {:ok, pair, state, log} ->
        case TokenAdapter.peek(state) do
          {:ok, %{kind: :","}, _} ->
            {:ok, _comma, state} = TokenAdapter.next(state)
            # Check for trailing comma - if next is a terminator, stop
            case TokenAdapter.peek(state) do
              {:ok, %{kind: kind}, _} when kind in [:eoe, :")", :"]", :"}", :">>",:do] ->
                {:ok, Enum.reverse([pair | acc]), state, log}

              _ ->
                parse_kw_list([pair | acc], state, ctx, log, min_bp)
            end

          _ ->
            {:ok, Enum.reverse([pair | acc]), state, log}
        end

      other ->
        other
    end
  end

  defp parse_kw_pair(state, _ctx, log, min_bp) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: kind, value: key}, state} when kind in @kw_kinds ->
        state = skip_eoe(state)

        # Use min_bp if provided to stop parsing at certain operators (e.g., ->)
        # Always use :matched context so keyword values don't consume do-blocks
        # (the do-block belongs to the outer call, not to the keyword value)
        result = if min_bp > 0 do
          Pratt.parse_with_min_bp(state, :matched, log, min_bp)
        else
          Expressions.expr(state, :matched, log)
        end

        with {:ok, value_ast, state, log} <- result do
          key_ast = Builder.Helpers.literal(key)
          {:ok, {key_ast, value_ast}, state, log}
        end

      {:ok, token, state} ->
        {:error, {:expected, :keyword, got: token.kind}, state, log}

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
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
