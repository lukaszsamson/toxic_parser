defmodule ToxicParser.Grammar.Expressions do
  @moduledoc """
  Expression dispatcher for matched/unmatched/no-parens contexts.
  """

  alias ToxicParser.{Builder, Context, EventLog, Pratt, Recovery, State, TokenAdapter}
  alias ToxicParser.Grammar.{Blocks, Calls, Containers, EOE}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @doc """
  Parses a list of expressions separated by logical EOE.

  Handles the grammar rules:
  - grammar -> '$empty' : {'__block__', [], []}.
  - grammar -> eoe : {'__block__', meta_from_token('$1'), []}.
  - grammar -> expr_list : build_block(reverse('$1')).
  - grammar -> eoe expr_list : build_block(reverse('$2')).
  - grammar -> expr_list eoe : build_block(reverse(annotate_eoe('$2', '$1'))).
  - grammar -> eoe expr_list eoe : build_block(reverse(annotate_eoe('$3', '$2'))).
  """
  @spec expr_list(State.t(), Pratt.context(), EventLog.t()) :: result()
  def expr_list(%State{} = state, ctx, %EventLog{} = log) do
    # If input is empty (or only EOE), return empty block early
    {state, leading_eoe_meta} = EOE.skip_with_meta(state)

    case TokenAdapter.peek(state) do
      {:eof, state} ->
        ast = build_empty_block(leading_eoe_meta)
        {:ok, ast, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}

      _ ->
        # Proceed with expression parsing
        case expr(state, ctx, log) do
          {:ok, first, state, log} ->
            # Check for trailing EOE and annotate expression
            {first, state} = maybe_annotate_eoe(first, state)
            collect_exprs([first], state, ctx, log)

          {:error, :unexpected_eof, state, log} ->
            {:error, :unexpected_eof, state, log}

          {:error, reason, state, log} ->
            recover_expr_error([], reason, state, ctx, log)
        end
    end
  end

  # Build empty block with metadata from first EOE token if present
  defp build_empty_block(meta), do: {:__block__, meta, []}

  @doc """
  Dispatches to the Pratt parser based on expression context.
  """
  @spec expr(State.t(), Pratt.context(), EventLog.t()) :: result()
  def expr(%State{} = state, ctx, %EventLog{} = log) do
    ctx = Context.normalize(ctx)
    parse_with_layers(state, ctx, log)
  end

  @doc "Parses a matched expression (no trailing do-block attachment)."
  @spec matched_expr(State.t(), EventLog.t()) :: result()
  def matched_expr(%State{} = state, %EventLog{} = log) do
    parse_with_layers(state, Context.matched_expr(), log)
  end

  @doc "Parses an unmatched expression (do-block capable context)."
  @spec unmatched_expr(State.t(), EventLog.t()) :: result()
  def unmatched_expr(%State{} = state, %EventLog{} = log) do
    parse_with_layers(state, Context.unmatched_expr(), log)
  end

  @doc "Parses a no-parens expression (call-ambiguous context)."
  @spec no_parens_expr(State.t(), EventLog.t()) :: result()
  def no_parens_expr(%State{} = state, %EventLog{} = log) do
    parse_with_layers(state, Context.no_parens_expr(), log)
  end

  defp parse_with_layers(state, ctx, log) do
    case Blocks.parse(state, ctx, log) do
      {:ok, ast, state, log} ->
        # Continue with led() to handle trailing binary operators like `fn -> a end ** b`
        Pratt.led(ast, state, log, 0, ctx)

      {:error, reason, state, log} ->
        {:error, reason, state, log}

      {:no_block, state} ->
        case Containers.parse(state, ctx, log) do
          {:ok, ast, state, log} ->
            {:ok, ast, state, log}

          {:error, reason, state, log} ->
            {:error, reason, state, log}

          {:no_container, state} ->
            case ToxicParser.Grammar.Strings.parse(state, ctx, log) do
              {:ok, ast, state, log} ->
                {:ok, ast, state, log}

              # String was a quoted keyword key like "a": - parse value and return as keyword pair
              {:keyword_key, key_atom, state, log} ->
                # Skip EOE (newlines) after the colon before parsing value
                state = EOE.skip(state)

                with {:ok, value_ast, state, log} <- expr(state, keyword_value_context(ctx), log) do
                  {:ok, [{key_atom, value_ast}], state, log}
                end

              # Interpolated keyword key like "fo#{1}o": - build binary_to_atom call
              {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, log} ->
                # Skip EOE (newlines) after the colon before parsing value
                state = EOE.skip(state)

                with {:ok, value_ast, state, log} <- expr(state, keyword_value_context(ctx), log) do
                  key_ast = build_interpolated_keyword_key(parts, kind, start_meta, delimiter)
                  {:ok, [{key_ast, value_ast}], state, log}
                end

              {:no_string, state} ->
                case Calls.parse(state, ctx, log) do
                  {:ok, ast, state, log} -> {:ok, ast, state, log}
                  {:error, reason, state, log} -> {:error, reason, state, log}
                end

              {:error, reason, state, log} ->
                {:error, reason, state, log}
            end
        end
    end
  end

  defp collect_exprs(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe}, _} ->
        # Skip all consecutive EOE tokens (handles cases like "1\n;2")
        state = EOE.skip(state)

        # Check if we've reached EOF or terminator after trailing EOE
        case TokenAdapter.peek(state) do
          {:eof, state} ->
            finalize_exprs(acc, state, log)

          # Stop at terminators that shouldn't start new expressions
          {:ok, %{kind: kind}, _} when kind in [:end_interpolation, :"}"] ->
            finalize_exprs(acc, state, log)

          {:ok, _, _} ->
            case expr(state, ctx, log) do
              {:ok, next_expr, state, log} ->
                # Annotate expression with trailing EOE if present
                {next_expr, state} = maybe_annotate_eoe(next_expr, state)
                collect_exprs([next_expr | acc], state, ctx, log)

              {:error, reason, state, log} ->
                recover_expr_error(acc, reason, state, ctx, log)
            end

          {:error, diag, state} ->
            {:error, diag, state, log}
        end

      {:eof, state} ->
        finalize_exprs(acc, state, log)

      {:error, diag, state} ->
        {:error, diag, state, log}

      _ ->
        finalize_exprs(acc, state, log)
    end
  end

  defp finalize_exprs([single], state, log), do: {:ok, single, state, log}

  defp finalize_exprs(many, state, log) do
    block = Builder.Helpers.literal({:__block__, [], Enum.reverse(many)})
    {:ok, block, state, log}
  end

  # Annotate an expression with end_of_expression metadata if followed by EOE.
  # This implements the `annotate_eoe` pattern from elixir_parser.yrl.
  defp maybe_annotate_eoe(ast, state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe} = eoe_token, _} ->
        eoe_meta = EOE.build_eoe_meta(eoe_token)
        annotated = EOE.annotate_eoe(ast, eoe_meta)
        {annotated, state}

      _ ->
        {ast, state}
    end
  end

  defp recover_expr_error(acc, reason, %State{mode: :tolerant} = state, ctx, log) do
    error_ast = build_error_node(reason, state)

    with {:ok, state, log} <- Recovery.sync_expr(state, log) do
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :eoe}, _} ->
          # consume separator to make progress before continuing
          {:ok, _eoe, state} = TokenAdapter.next(state)
          collect_exprs([error_ast | acc], state, ctx, log)

        _ ->
          collect_exprs([error_ast | acc], state, ctx, log)
      end
    end
  end

  defp recover_expr_error(_acc, reason, state, _ctx, log) do
    {:error, reason, state, log}
  end

  defp build_error_node(reason, state) do
    meta =
      case TokenAdapter.peek(state) do
        {:ok, tok, _} -> Builder.Helpers.token_meta(tok.metadata)
        _ -> []
      end

    Builder.Helpers.error(reason, meta)
  end

  @doc """
  Builds an AST node for an interpolated keyword key like "foo\#{x}":
  Returns {:., meta, [:erlang, :binary_to_atom]} call.

  Elixir produces: {{:., meta, [:erlang, :binary_to_atom]}, call_meta, [binary, :utf8]}
  where binary is {:<<>>, meta, [parts...]}
  """
  @spec build_interpolated_keyword_key(list(), atom(), keyword(), String.t()) :: Macro.t()
  def build_interpolated_keyword_key(parts, kind, start_meta, delimiter) do
    # Convert parts to binary parts for {:<<>>, ...}
    binary_parts = parts_to_binary(parts, kind, start_meta)
    binary_ast = {:<<>>, start_meta, binary_parts}

    # Build the :erlang.binary_to_atom call
    dot_ast = {:., start_meta, [:erlang, :binary_to_atom]}
    call_meta = [delimiter: delimiter, format: :keyword] ++ start_meta

    {dot_ast, call_meta, [binary_ast, :utf8]}
  end

  # Convert string parts to binary AST parts
  # Each part is either {:fragment, content} or {:interpolation, ast}
  # The interpolation ast already has the full structure with to_string and ::binary
  defp parts_to_binary(parts, _kind, _start_meta) do
    Enum.flat_map(parts, fn
      {:fragment, content} ->
        if content == "", do: [], else: [content]

      {:interpolation, ast} ->
        # The interpolation ast already contains the full ::binary structure
        [ast]
    end)
  end

  # Determine keyword value context based on whether outer context allows do-blocks.
  # In paren calls (allow_do_block: true), use container_expr which allows do-blocks.
  # In no-parens calls (allow_do_block: false), use kw_no_parens_value to prevent
  # do-blocks from attaching to the keyword value (they belong to outer call).
  defp keyword_value_context(ctx) do
    ctx = Context.normalize(ctx)

    case ctx.allow_do_block do
      true -> Context.container_expr()
      false -> Context.kw_no_parens_value()
    end
  end
end
