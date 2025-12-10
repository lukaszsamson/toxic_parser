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
    ast = literal_to_ast(token)
    {:ok, ast, state, log}
  end

  # Parse RHS of binary operator, handling chained operators with proper precedence
  defp parse_rhs(token, state, context, log, min_bp) do
    with {:ok, right, state, log} <- nud(token, state, context, log) do
      led(right, state, log, min_bp, context)
    end
  end

  defp led(left, state, log, min_bp, context) do
    case TokenAdapter.peek(state) do
      {:ok, next_token, _} ->
        case {next_token.kind, Precedence.binary(next_token.kind)} do
          {:"[", _} ->
            {:ok, _open, state} = TokenAdapter.next(state)

            with {:ok, indices, state, log} <- parse_access_indices([], state, context, log),
                 {:ok, _close, state} <- TokenAdapter.next(state) do
              combined = Builder.Helpers.access(left, Enum.reverse(indices), [])
              led(combined, state, log, min_bp, context)
            end

          {_, {bp, assoc}} when bp >= min_bp ->
            {:ok, op_token, state} = TokenAdapter.next(state)
            # For right associativity, use same bp to allow chaining at same level
            # For left associativity, use bp+1 to prevent same-level ops from binding to RHS
            rhs_min_bp = if assoc == :right, do: bp, else: bp + 1

            # Skip EOE tokens after operator (allows "1 +\n2")
            {state, newlines} = skip_eoe_after_op(state)

            case TokenAdapter.next(state) do
              {:ok, rhs_token, state} ->
                # Parse RHS with appropriate min_bp for associativity
                with {:ok, right, state, log} <- parse_rhs(rhs_token, state, context, log, rhs_min_bp) do
                  # Use the actual operator value (e.g. :=, :+) not the token kind (e.g. :match_op)
                  op = op_token.value
                  # Add newlines metadata if there were newlines after the operator
                  meta = build_meta_with_newlines(op_token.metadata, newlines)

                  combined =
                    Builder.Helpers.binary(
                      op,
                      left,
                      right,
                      meta
                    )

                  # Continue with original min_bp to allow chaining same-precedence left-assoc ops
                  led(combined, state, log, min_bp, context)
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

  # Integer: extract parsed value from raw token metadata
  defp literal_to_ast(%{kind: :int, raw: {:int, {_, _, parsed_value}, _}}) do
    parsed_value
  end

  # Float: extract parsed value from raw token metadata
  defp literal_to_ast(%{kind: :flt, raw: {:flt, {_, _, parsed_value}, _}}) do
    parsed_value
  end

  # Character literal: value is already the codepoint
  defp literal_to_ast(%{kind: :char, value: codepoint}) do
    codepoint
  end

  # Boolean literals: kind is the literal value itself
  defp literal_to_ast(%{kind: true}), do: true
  defp literal_to_ast(%{kind: false}), do: false
  defp literal_to_ast(%{kind: nil}), do: nil

  # Atom: value is already the atom
  defp literal_to_ast(%{kind: :atom, value: atom}) do
    atom
  end

  # Alias: wrap in __aliases__ tuple with :last metadata
  defp literal_to_ast(%{kind: :alias, value: atom, metadata: meta}) do
    m = build_meta(meta)
    {:__aliases__, [last: m] ++ m, [atom]}
  end

  # Identifier: wrap in tuple with nil context (variable reference)
  defp literal_to_ast(%{kind: :identifier, value: atom, metadata: meta}) do
    {atom, build_meta(meta), nil}
  end

  # String (for future use)
  defp literal_to_ast(%{kind: :string, value: value}) do
    value
  end

  # Range operator (..) as standalone expression
  defp literal_to_ast(%{kind: :range_op, value: op, metadata: meta}) do
    {op, build_meta(meta), []}
  end

  # Ellipsis operator (...) as standalone expression
  defp literal_to_ast(%{kind: :ellipsis_op, value: op, metadata: meta}) do
    {op, build_meta(meta), []}
  end

  # Fallback for other tokens
  defp literal_to_ast(%{value: value}) do
    value
  end

  # Build metadata for AST nodes
  defp build_meta(%{range: %{start: %{line: line, column: column}}}) do
    [line: line, column: column]
  end

  defp build_meta(_), do: []

  # Build metadata with newlines count if present
  defp build_meta_with_newlines(token_meta, 0) do
    build_meta(token_meta)
  end

  defp build_meta_with_newlines(token_meta, newlines) when newlines > 0 do
    [newlines: newlines] ++ build_meta(token_meta)
  end

  # Skip EOE tokens after an operator and count newlines
  defp skip_eoe_after_op(state, newlines \\ 0) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, value: %{newlines: n}}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe_after_op(state, newlines + n)

      _ ->
        {state, newlines}
    end
  end

  defp parse_access_indices(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"]"}, state} ->
        {:ok, acc, state, log}

      {:ok, _tok, _state} ->
        with {:ok, expr, state, log} <- parse(state, ctx, log) do
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :","}, state} ->
              {:ok, _comma, state} = TokenAdapter.next(state)
              parse_access_indices([expr | acc], state, ctx, log)

            {:ok, %{kind: :"]"}, _} ->
              parse_access_indices([expr | acc], state, ctx, log)

            _ ->
              {:error, {:expected_comma_or, :"]"}, state, log}
          end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end
end
