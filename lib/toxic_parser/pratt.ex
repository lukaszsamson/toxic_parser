defmodule ToxicParser.Pratt do
  @moduledoc """
  Pratt parser skeleton for matched/unmatched/no-parens expressions.

  Phase 3 implements binding power lookup and API shape; full parselets are
  implemented in later phases.

  Dual operator spacing (`dual_op` as unary vs binary) relies on Toxic token
  shapes; spacing-sensitive disambiguation will occur in parselets using token
  metadata.
  """

  alias ToxicParser.{Builder, EventLog, Precedence, State, TokenAdapter}

  @type context :: :matched | :unmatched | :no_parens

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @doc """
  Parses an expression in the given context.
  """
  @spec parse(State.t(), context(), EventLog.t()) :: result()
  def parse(%State{} = state, context, %EventLog{} = log) do
    with {:ok, token, state} <- TokenAdapter.next(state),
         {:ok, left, state, log} <- nud(token, state, context, log) do
      led(left, state, log, 0, context)
    else
      {:eof, state} -> {:error, :unexpected_eof, state, log}
      {:error, diag, state} -> {:error, diag, state, log}
    end
  end

  @doc "Exposes binary binding power."
  @spec bp(atom()) :: Precedence.bp() | nil
  def bp(kind) do
    case Precedence.binary(kind) do
      {bp, _assoc} -> bp
      _ -> nil
    end
  end

  @doc "Exposes unary binding power."
  @spec unary_bp(atom()) :: Precedence.bp() | nil
  def unary_bp(kind) do
    case Precedence.unary(kind) do
      {bp, _assoc} -> bp
      _ -> nil
    end
  end

  defp nud(token, state, _context, log) do
    ast = Builder.Helpers.literal(token.value)
    {:ok, ast, state, log}
  end

  defp led(left, state, log, min_bp, context) do
    case TokenAdapter.peek(state) do
      {:ok, next_token, _} ->
        case Precedence.binary(next_token.kind) do
          {bp, assoc} when bp >= min_bp ->
            {:ok, _op, state} = TokenAdapter.next(state)
            next_min = if assoc == :right, do: bp, else: bp + 1

            case TokenAdapter.next(state) do
              {:ok, rhs_token, state} ->
                with {:ok, right, state, log} <- nud(rhs_token, state, context, log) do
                  combined =
                    Builder.Helpers.binary(
                      next_token.kind,
                      left,
                      right,
                      []
                    )

                  led(combined, state, log, next_min, context)
                end

              {:eof, state} ->
                {:error, :unexpected_eof, state, log}

              {:error, diag, state} ->
                {:error, diag, state, log}
            end

          _ ->
            {:ok, left, state, log}
        end

      {:eof, state} ->
        {:ok, left, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end
end
