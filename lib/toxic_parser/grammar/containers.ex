defmodule ToxicParser.Grammar.Containers do
  @moduledoc """
  Container parsing for lists and tuples (Phase 6 scaffolding).
  """

  alias ToxicParser.{Builder, Context, EventLog, Pratt, State, TokenAdapter}
  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.{Bitstrings, EOE, Expressions, Keywords, Maps, Stabs}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}
          | {:no_container, State.t()}

  @spec parse(State.t(), Pratt.context(), EventLog.t(), non_neg_integer()) :: result()
  def parse(%State{} = state, %Context{} = ctx, %EventLog{} = log, min_bp \\ 0) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"("}, _} ->
        parse_paren(state, ctx, log, min_bp)

      {:ok, %{kind: :"["}, _} ->
        parse_list(state, ctx, log, min_bp)

      {:ok, %{kind: :"{", value: _}, _} ->
        parse_tuple(state, ctx, log, min_bp)

      {:ok, %{kind: :%{}}, _} ->
        Maps.parse_map(state, ctx, log, min_bp)

      {:ok, %{kind: :%}, _} ->
        Maps.parse_map(state, ctx, log, min_bp)

      {:ok, %{kind: :"<<", value: _}, _} ->
        Bitstrings.parse(state, ctx, log, min_bp)

      {:eof, state} ->
        {:no_container, state}

      {:error, _diag, state} ->
        {:no_container, state}

      _ ->
        {:no_container, state}
    end
  end

  @doc """
  Parse a container without calling Pratt.led at the end.
  Used when the caller needs to control operator binding (e.g., stab patterns).
  """
  @spec parse_container_base(State.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_container_base(%State{} = state, %Context{} = ctx, %EventLog{} = log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"["}, _} ->
        parse_list_base(state, ctx, log)

      {:ok, %{kind: :"{", value: _}, _} ->
        parse_tuple_base(state, ctx, log)

      {:ok, %{kind: :"<<", value: _}, _} ->
        Bitstrings.parse_base(state, ctx, log)

      {:eof, state} ->
        {:no_container, state}

      {:error, _diag, state} ->
        {:no_container, state}

      _ ->
        {:no_container, state}
    end
  end

  # Parse parenthesized expression, empty parens, or stab expression
  # Grammar rules:
  #   access_expr -> open_paren stab_eoe ')'         : build_paren_stab
  #   access_expr -> open_paren ';' stab_eoe ')'     : build_paren_stab
  #   access_expr -> open_paren ';' close_paren      : build_paren_stab with nil
  #   access_expr -> empty_paren                     : wrap in __block__
  defp parse_paren(state, ctx, log, min_bp) do
    {:ok, open_tok, state} = TokenAdapter.next(state)
    open_meta = token_meta(open_tok.metadata)

    # Skip leading newlines (but not semicolons)
    state = skip_eoe_not_semicolon(state)

    # Check for leading semicolon (forces stab interpretation)
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if semicolon_eoe?(tok) do
          # Leading semicolon - parse as stab or empty
          {:ok, _semi, state} = TokenAdapter.next(state)
          # Skip any additional EOE
          state = EOE.skip(state)
          Stabs.parse_paren_stab_or_empty(open_meta, state, ctx, log, min_bp)
        else
          # Skip any remaining EOE tokens
          state = EOE.skip(state)
          parse_paren_content(open_meta, state, ctx, log, min_bp)
        end

      _ ->
        # Skip any remaining EOE tokens
        state = EOE.skip(state)
        parse_paren_content(open_meta, state, ctx, log, min_bp)
    end
  end

  # Parse content after open paren (no leading semicolon)
  defp parse_paren_content(open_meta, state, ctx, log, min_bp) do
    case TokenAdapter.peek(state) do
      # Empty parens: () -> {:__block__, [parens: ...], []}
      {:ok, %{kind: :")"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        parens_meta = [parens: Meta.closing_meta(open_meta, close_meta, 0, [], base_first: true)]
        ast = {:__block__, parens_meta, []}
        # Continue with Pratt.led to handle trailing operators
        Pratt.led(ast, state, log, min_bp, ctx)

      # Check for stab operator at start: (-> expr)
      {:ok, %{kind: :stab_op}, _} ->
        Stabs.parse_paren_stab(open_meta, state, ctx, log, min_bp)

      # Check for empty inner parens followed by stab/when: (() -> expr) or (() when g -> expr)
      {:ok, %{kind: :"("}, _} ->
        Stabs.try_parse_stab_parens_many(
          open_meta,
          state,
          ctx,
          log,
          min_bp,
          fn new_state, new_log ->
            parse_expr_in_paren_with_meta(open_meta, new_state, ctx, new_log, min_bp)
          end
        )

      # Content that could be expression or stab pattern
      {:ok, _, _} ->
        # Try to parse stab first using checkpoint
        Stabs.try_parse_stab_or_expr(
          open_meta,
          state,
          ctx,
          log,
          min_bp,
          fn new_state, new_log ->
            parse_expr_in_paren_with_meta(open_meta, new_state, ctx, new_log, min_bp)
          end
        )

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_expr_in_paren_with_meta(open_meta, state, ctx, log, min_bp) do
    parse_expr_in_paren_impl(open_meta, [], state, ctx, log, min_bp)
  end

  # Parse expressions in parens, handling semicolons as separators
  # Grammar: paren_expr -> expr (';' expr)* ')'
  # Multiple expressions become a __block__
  # NOTE: Inside parens, we always use :unmatched context because parens create
  # a new expression boundary - do-blocks inside belong to the inner call.
  defp parse_expr_in_paren_impl(open_meta, acc, state, ctx, log, min_bp) do
    # Use :unmatched inside parens to allow do-blocks on inner calls
    with {:ok, expr, state, log} <- Expressions.expr(state, Context.unmatched_expr(), log) do
      # Check for EOE (semicolon) or close paren
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :eoe} = eoe_tok, _} ->
          # Annotate expression with EOE metadata before consuming
          annotated_expr = annotate_paren_expr_eoe(expr, eoe_tok)
          # Consume EOE
          {:ok, _eoe, state} = TokenAdapter.next(state)
          state = EOE.skip(state)
          # Check if there's more content or close paren
          case TokenAdapter.peek(state) do
            {:ok, %{kind: :")"} = close_tok, _} ->
              {:ok, _close, state} = TokenAdapter.next(state)
              close_meta = token_meta(close_tok.metadata)

              finish_paren_exprs(
                [annotated_expr | acc],
                open_meta,
                close_meta,
                state,
                log,
                min_bp,
                ctx
              )

            {:ok, _, _} ->
              # More expressions after semicolon
              parse_expr_in_paren_impl(open_meta, [annotated_expr | acc], state, ctx, log, min_bp)

            {:eof, state} ->
              {:error, :unexpected_eof, state, log}

            {:error, diag, state} ->
              {:error, diag, state, log}
          end

        {:ok, %{kind: :")"} = close_tok, _} ->
          {:ok, _close, state} = TokenAdapter.next(state)
          close_meta = token_meta(close_tok.metadata)
          finish_paren_exprs([expr | acc], open_meta, close_meta, state, log, min_bp, ctx)

        {:ok, token, state} ->
          {:error, {:expected, :")", got: token.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  # Annotate expression with end_of_expression metadata from EOE token
  defp annotate_paren_expr_eoe({name, meta, args}, eoe_tok) when is_list(meta) do
    eoe_meta = EOE.build_eoe_meta(eoe_tok)
    {name, [{:end_of_expression, eoe_meta} | meta], args}
  end

  defp annotate_paren_expr_eoe(expr, _eoe_tok), do: expr

  # Finish parsing paren expressions - build appropriate AST
  defp finish_paren_exprs([single], open_meta, close_meta, state, log, min_bp, ctx) do
    # Special cases for single expressions that need to be wrapped in __block__:
    # 1. Single-argument unary expressions with `not` or `!` operators (rearrange_uop)
    # 2. unquote_splicing - always wrapped because the parser assumes a block is being spliced
    #
    # From elixir_parser.yrl:
    #   build_paren_stab(_Before, [{Op, _, [_]}]=Exprs, _After) when ?rearrange_uop(Op) ->
    #     {'__block__', [], Exprs};
    #   build_block([{unquote_splicing, _, [_]}]=Exprs, BeforeAfter) ->
    #     {'__block__', block_meta(BeforeAfter), Exprs};
    expr =
      case single do
        {op, _meta, [_arg]} when op in [:not, :!] ->
          {:__block__, [], [single]}

        {:unquote_splicing, _meta, [_arg]} ->
          # unquote_splicing always gets wrapped in __block__ with closing metadata
          {:__block__, Meta.closing_meta(open_meta, close_meta), [single]}

        _ ->
          # Regular single expression - add parens metadata to 3-tuple AST nodes
          add_parens_meta(single, open_meta, close_meta)
      end

    # Continue with Pratt.led to handle trailing operators like *, /, etc.
    Pratt.led(expr, state, log, min_bp, ctx)
  end

  defp finish_paren_exprs(exprs, open_meta, close_meta, state, log, min_bp, ctx) do
    # Multiple expressions - wrap in __block__
    ast = {:__block__, Meta.closing_meta(open_meta, close_meta), Enum.reverse(exprs)}
    Pratt.led(ast, state, log, min_bp, ctx)
  end

  # Add parens: metadata to 3-tuple AST nodes
  defp add_parens_meta({name, meta, args}, open_meta, close_meta) when is_list(meta) do
    parens_meta = build_parens_meta(open_meta, close_meta)
    {name, [parens: parens_meta] ++ meta, args}
  end

  defp add_parens_meta(literal, _open_meta, _close_meta), do: literal

  defp build_parens_meta(open_meta, close_meta) do
    Meta.closing_meta(open_meta, close_meta, 0, [], base_first: true)
  end

  defp token_meta(meta), do: Builder.Helpers.token_meta(meta)

  defp skip_eoe_not_semicolon(state) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if semicolon_eoe?(tok) do
          state
        else
          case tok.kind do
            :eoe ->
              {:ok, _eoe, state} = TokenAdapter.next(state)
              skip_eoe_not_semicolon(state)

            _ ->
              state
          end
        end

      _ ->
        state
    end
  end

  defp semicolon_eoe?(%{kind: :eoe, value: %{source: :semicolon}}), do: true
  defp semicolon_eoe?(%{kind: :eoe, raw: {:";", _, _}}), do: true
  defp semicolon_eoe?(%{kind: :eoe, raw: {:";", _}}), do: true
  defp semicolon_eoe?(_), do: false

  defp parse_list(state, ctx, log, min_bp) do
    with {:ok, ast, state, log} <- parse_list_base(state, ctx, log) do
      # Continue with Pratt's led() to handle trailing operators
      Pratt.led(ast, state, log, min_bp, ctx)
    end
  end

  # Parse list without calling Pratt.led - used when caller controls led binding
  defp parse_list_base(state, ctx, log) do
    {:ok, _open, state} = TokenAdapter.next(state)

    # Skip leading EOE
    {state, _newlines} = EOE.skip_count_newlines(state, 0)

    case TokenAdapter.peek(state) do
      # Empty list
      {:ok, %{kind: :"]"}, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        {:ok, [], state, log}

      {:ok, _, _} ->
        parse_list_elements_base([], state, ctx, log)

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse list elements without calling Pratt.led - used when caller controls led binding
  defp parse_list_elements_base(acc, state, ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, tok, _} ->
        if Keywords.starts_kw?(tok) do
          with {:ok, kw_list, state, log} <- Keywords.parse_kw_data(state, ctx, log) do
            {state, _newlines} = EOE.skip_count_newlines(state, 0)

            case TokenAdapter.next(state) do
              {:ok, %{kind: :"]"}, state} ->
                {:ok, Enum.reverse(acc) ++ kw_list, state, log}

              {:ok, tok, state} ->
                {:error, {:expected, :"]", got: tok.kind}, state, log}

              {:eof, state} ->
                {:error, :unexpected_eof, state, log}

              {:error, diag, state} ->
                {:error, diag, state, log}
            end
          end
        else
          parse_list_element_base(acc, state, ctx, log)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp parse_list_element_base(acc, state, ctx, log) do
    # Check if this element starts with [ or { - if so, it's a list/tuple literal
    # and should NOT be flattened even if it looks like a keyword list
    is_container_literal =
      case TokenAdapter.peek(state) do
        {:ok, %{kind: kind}, _} when kind in [:"[", :"{"] -> true
        _ -> false
      end

    with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
      {state, _newlines} = EOE.skip_count_newlines(state, 0)

      # Check if expr is a keyword list (from quoted keyword parsing like "": 1)
      # If so, we should merge it rather than wrap it as a single element
      # BUT: if it started as a container literal ([...] or {...}), don't merge
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state)
          {state, _newlines} = EOE.skip_count_newlines(state, 0)

          case TokenAdapter.peek(state) do
            {:ok, %{kind: :"]"}, _} ->
              {:ok, _close, state} = TokenAdapter.next(state)
              {:ok, merge_keyword_expr(acc, expr, is_container_literal), state, log}

            _ ->
              parse_list_elements_base(
                prepend_expr(acc, expr, is_container_literal),
                state,
                ctx,
                log
              )
          end

        {:ok, %{kind: :"]"}, _} ->
          {:ok, _close, state} = TokenAdapter.next(state)
          {:ok, merge_keyword_expr(acc, expr, is_container_literal), state, log}

        {:ok, tok, state} ->
          {:error, {:expected_comma_or, :"]", got: tok.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  # Check if expr is a keyword list (list of {atom, value} tuples)
  # If so, merge it into the result; otherwise prepend as single element
  # BUT: if is_container_literal is true, the list came from a [...] or {...}
  # literal and should NOT be flattened
  defp merge_keyword_expr(acc, expr, is_container_literal)

  defp merge_keyword_expr(acc, expr, is_container_literal) when is_list(expr) do
    if not is_container_literal and is_non_empty_keyword_list?(expr) do
      Enum.reverse(acc) ++ expr
    else
      Enum.reverse([expr | acc])
    end
  end

  defp merge_keyword_expr(acc, expr, _is_container_literal) do
    Enum.reverse([expr | acc])
  end

  # Prepend expr to acc, handling keyword lists specially
  # BUT: if is_container_literal is true, the list came from a [...] or {...}
  # literal and should NOT be flattened
  defp prepend_expr(acc, expr, is_container_literal)

  defp prepend_expr(acc, expr, is_container_literal) when is_list(expr) do
    if not is_container_literal and is_non_empty_keyword_list?(expr) do
      # Reverse keyword list and prepend each element to acc
      Enum.reduce(Enum.reverse(expr), acc, fn elem, acc -> [elem | acc] end)
    else
      [expr | acc]
    end
  end

  defp prepend_expr(acc, expr, _is_container_literal), do: [expr | acc]

  # Check if a list is a keyword-like list (list of {key, value} tuples)
  # The key can be an atom (standard keyword) or an AST (interpolated keyword)
  defp is_keyword_list?([{_key, _value} | rest]), do: is_keyword_list?(rest)
  defp is_keyword_list?([]), do: true
  defp is_keyword_list?(_), do: false

  # Check if a list is a NON-EMPTY keyword list (for merging purposes)
  # Empty lists should never be merged - they should be kept as elements (e.g., [''] -> [[]])
  defp is_non_empty_keyword_list?([_ | _] = list), do: is_keyword_list?(list)
  defp is_non_empty_keyword_list?(_), do: false

  defp tuple_element_container_literal?(state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: kind}, state} when kind in [:"[", :"{"] -> {true, state}
      {:ok, _tok, state} -> {false, state}
      {:eof, state} -> {false, state}
      {:error, _diag, state} -> {false, state}
    end
  end

  defp merge_tuple_keyword_tail(tagged_elements) do
    {tail_rev, head_rev} =
      tagged_elements
      |> Enum.reverse()
      |> Enum.split_while(fn {expr, is_container_literal} ->
        not is_container_literal and is_list(expr) and is_keyword_list?(expr)
      end)

    if length(tail_rev) >= 2 do
      merged_kw =
        tail_rev
        |> Enum.reverse()
        |> Enum.flat_map(fn {expr, _flag} -> expr end)

      Enum.reverse(head_rev) ++ [{merged_kw, false}]
    else
      tagged_elements
    end
  end

  defp finalize_tuple_elements(tagged_elements) do
    tagged_elements
    |> merge_tuple_keyword_tail()
    |> Enum.map(fn {expr, _flag} -> expr end)
  end

  defp parse_tuple(state, ctx, log, min_bp) do
    with {:ok, ast, state, log} <- parse_tuple_base(state, ctx, log) do
      # Continue with Pratt's led() to handle trailing operators like <-, =, etc.
      Pratt.led(ast, state, log, min_bp, ctx)
    end
  end

  # Parse tuple without calling Pratt.led - used when caller controls led binding
  defp parse_tuple_base(state, ctx, log) do
    {:ok, open_tok, state} = TokenAdapter.next(state)
    open_meta = token_meta(open_tok.metadata)

    with {:ok, elements, newlines, close_meta, state, log} <- parse_tuple_args(state, ctx, log) do
      meta = Meta.closing_meta(open_meta, close_meta, newlines)

      # 2-element tuples are represented as literal {a, b}
      # Other sizes use {:{}, meta, elements}
      ast =
        case elements do
          [a, b] -> {a, b}
          _ -> {:{}, meta, elements}
        end

      {:ok, ast, state, log}
    end
  end

  defp parse_tuple_args(state, _ctx, log) do
    # Skip leading EOE and count newlines
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    case TokenAdapter.peek(state) do
      # Empty tuple
      {:ok, %{kind: :"}"} = close_tok, _} ->
        {:ok, _close, state} = TokenAdapter.next(state)
        close_meta = token_meta(close_tok.metadata)
        {:ok, [], leading_newlines, close_meta, state, log}

      {:ok, tok, _} ->
        # Tuple args follow container_args in elixir_parser.yrl.
        # Importantly, a keyword tail like `{1, foo: 1, bar: 2}` is a 2-tuple:
        # `{1, [foo: 1, bar: 2]}`.
        # Also, a tuple may start with keyword data, e.g. `{foo: 1, bar: 2}`,
        # which is parsed as a 1-tuple whose sole element is a keyword list.

        # For definite keywords (kw_identifier), parse as keyword list
        # For potential quoted keywords (string_start), try keyword parsing with checkpoint
        cond do
          # TODO: no coverage
          Keywords.starts_kw?(tok) ->
            # Definite keyword - parse as keyword list
            with {:ok, kw_list, state, log} <-
                   Keywords.parse_kw_data(state, Context.container_expr(), log) do
              {state, _newlines} = EOE.skip_count_newlines(state, 0)

              case TokenAdapter.next(state) do
                {:ok, %{kind: :"}"} = close_tok, state} ->
                  close_meta = token_meta(close_tok.metadata)
                  {:ok, [kw_list], leading_newlines, close_meta, state, log}

                {:ok, tok, state} ->
                  {:error, {:expected, :"}", got: tok.kind}, state, log}

                {:eof, state} ->
                  {:error, :unexpected_eof, state, log}

                {:error, diag, state} ->
                  {:error, diag, state, log}
              end
            end

          Keywords.starts_kw_or_quoted_key?(tok) ->
            # Potential quoted keyword - checkpoint and try keyword parsing
            {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

            case Keywords.parse_kw_data(checkpoint_state, Context.container_expr(), log) do
              {:ok, kw_list, state, log} ->
                state = TokenAdapter.drop_checkpoint(state, ref)
                {state, _newlines} = EOE.skip_count_newlines(state, 0)

                case TokenAdapter.next(state) do
                  {:ok, %{kind: :"}"} = close_tok, state} ->
                    close_meta = token_meta(close_tok.metadata)
                    {:ok, [kw_list], leading_newlines, close_meta, state, log}

                  {:ok, tok, state} ->
                    {:error, {:expected, :"}", got: tok.kind}, state, log}

                  {:eof, state} ->
                    {:error, :unexpected_eof, state, log}

                  {:error, diag, state} ->
                    {:error, diag, state, log}
                end

              {:error, _reason, _state, _log} ->
                # Not a keyword - rewind and parse as expression
                state = TokenAdapter.rewind(checkpoint_state, ref)
                parse_tuple_first_element(state, log, leading_newlines)
            end

          true ->
            parse_tuple_first_element(state, log, leading_newlines)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse the first element of a tuple when it's not a keyword-only tuple
  defp parse_tuple_first_element(state, log, leading_newlines) do
    {first_is_container_literal, state} = tuple_element_container_literal?(state)

    with {:ok, first, state, log} <- Expressions.expr(state, Context.container_expr(), log) do
      {state, _newlines} = EOE.skip_count_newlines(state, 0)

      case TokenAdapter.peek(state) do
        {:ok, %{kind: :"}"} = close_tok, _} ->
          {:ok, _close, state} = TokenAdapter.next(state)
          close_meta = token_meta(close_tok.metadata)
          {:ok, [first], leading_newlines, close_meta, state, log}

        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state)
          {state, _newlines} = EOE.skip_count_newlines(state, 0)

          case TokenAdapter.peek(state) do
            {:ok, %{kind: :"}"} = close_tok, _} ->
              # Trailing comma: {1,}
              {:ok, _close, state} = TokenAdapter.next(state)
              close_meta = token_meta(close_tok.metadata)
              {:ok, [first], leading_newlines, close_meta, state, log}

            {:ok, next_tok, _} ->
              # For keyword tail, check both regular and quoted keyword starts
              # For quoted keywords, use checkpoint since it may be a regular string
              cond do
                # TODO: no coverage?
                Keywords.starts_kw?(next_tok) ->
                  # Definite keyword tail
                  parse_tuple_keyword_tail(first, state, log, leading_newlines)

                Keywords.starts_kw_or_quoted_key?(next_tok) ->
                  # Potential quoted keyword - checkpoint and try
                  {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

                  case Keywords.parse_kw_data(checkpoint_state, Context.container_expr(), log) do
                    {:ok, kw_list, state, log} ->
                      state = TokenAdapter.drop_checkpoint(state, ref)
                      {state, _newlines} = EOE.skip_count_newlines(state, 0)

                      case TokenAdapter.next(state) do
                        {:ok, %{kind: :"}"} = close_tok, state} ->
                          close_meta = token_meta(close_tok.metadata)
                          {:ok, [first, kw_list], leading_newlines, close_meta, state, log}

                        {:ok, tok, state} ->
                          {:error, {:expected, :"}", got: tok.kind}, state, log}

                        {:eof, state} ->
                          {:error, :unexpected_eof, state, log}

                        {:error, diag, state} ->
                          {:error, diag, state, log}
                      end

                    {:error, _reason, _state, _log} ->
                      # Not a keyword - rewind and continue as general tuple
                      state = TokenAdapter.rewind(checkpoint_state, ref)

                      parse_tuple_rest(
                        [{first, first_is_container_literal}],
                        state,
                        log,
                        leading_newlines
                      )
                  end

                true ->
                  # General tuple (3+ elements or 2 elements without kw tail)
                  parse_tuple_rest(
                    [{first, first_is_container_literal}],
                    state,
                    log,
                    leading_newlines
                  )
              end

            _ ->
              # General tuple (3+ elements or 2 elements without kw tail)
              parse_tuple_rest(
                [{first, first_is_container_literal}],
                state,
                log,
                leading_newlines
              )
          end

        {:ok, tok, state} ->
          {:error, {:expected_comma_or, :"}", got: tok.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  # Parse keyword tail for tuple like {first, foo: 1, bar: 2}
  defp parse_tuple_keyword_tail(first, state, log, leading_newlines) do
    with {:ok, kw_list, state, log} <-
           Keywords.parse_kw_data(state, Context.container_expr(), log) do
      {state, _newlines} = EOE.skip_count_newlines(state, 0)

      case TokenAdapter.next(state) do
        {:ok, %{kind: :"}"} = close_tok, state} ->
          close_meta = token_meta(close_tok.metadata)
          {:ok, [first, kw_list], leading_newlines, close_meta, state, log}

        {:ok, tok, state} ->
          {:error, {:expected, :"}", got: tok.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  defp parse_tuple_rest(tagged_acc, state, log, leading_newlines) do
    {expr_is_container_literal, state} = tuple_element_container_literal?(state)

    with {:ok, expr, state, log} <- Expressions.expr(state, Context.container_expr(), log) do
      {state, _newlines} = EOE.skip_count_newlines(state, 0)
      new_tagged_acc = [{expr, expr_is_container_literal} | tagged_acc]

      case TokenAdapter.peek(state) do
        {:ok, %{kind: :","}, _} ->
          {:ok, _comma, state} = TokenAdapter.next(state)
          {state, _newlines} = EOE.skip_count_newlines(state, 0)

          case TokenAdapter.peek(state) do
            {:ok, %{kind: :"}"} = close_tok, _} ->
              # Trailing comma
              {:ok, _close, state} = TokenAdapter.next(state)
              close_meta = token_meta(close_tok.metadata)
              elements = Enum.reverse(new_tagged_acc) |> finalize_tuple_elements()
              {:ok, elements, leading_newlines, close_meta, state, log}

            {:ok, next_tok, _} ->
              # Check for keyword tail (e.g., {1, 2, 3, foo: :bar})
              cond do
                # TODO: no coverage?
                Keywords.starts_kw?(next_tok) ->
                  # Definite keyword tail
                  parse_tuple_rest_keyword_tail(new_tagged_acc, state, log, leading_newlines)

                Keywords.starts_kw_or_quoted_key?(next_tok) ->
                  # Potential quoted keyword - checkpoint and try
                  {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

                  case Keywords.parse_kw_data(checkpoint_state, Context.container_expr(), log) do
                    {:ok, kw_list, state, log} ->
                      state = TokenAdapter.drop_checkpoint(state, ref)
                      {state, _newlines} = EOE.skip_count_newlines(state, 0)

                      case TokenAdapter.next(state) do
                        {:ok, %{kind: :"}"} = close_tok, state} ->
                          close_meta = token_meta(close_tok.metadata)
                          elements = Enum.reverse(new_tagged_acc) |> finalize_tuple_elements()
                          {:ok, elements ++ [kw_list], leading_newlines, close_meta, state, log}

                        {:ok, tok, state} ->
                          {:error, {:expected, :"}", got: tok.kind}, state, log}

                        {:eof, state} ->
                          {:error, :unexpected_eof, state, log}

                        {:error, diag, state} ->
                          {:error, diag, state, log}
                      end

                    {:error, _reason, _state, _log} ->
                      # Not a keyword - rewind and continue
                      state = TokenAdapter.rewind(checkpoint_state, ref)
                      parse_tuple_rest(new_tagged_acc, state, log, leading_newlines)
                  end

                true ->
                  parse_tuple_rest(new_tagged_acc, state, log, leading_newlines)
              end

            _ ->
              parse_tuple_rest(new_tagged_acc, state, log, leading_newlines)
          end

        {:ok, %{kind: :"}"} = close_tok, _} ->
          {:ok, _close, state} = TokenAdapter.next(state)
          close_meta = token_meta(close_tok.metadata)
          elements = Enum.reverse(new_tagged_acc) |> finalize_tuple_elements()
          {:ok, elements, leading_newlines, close_meta, state, log}

        {:ok, tok, state} ->
          {:error, {:expected_comma_or, :"}", got: tok.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end

  # Parse keyword tail for 3+ element tuple like {1, 2, 3, foo: :bar}
  defp parse_tuple_rest_keyword_tail(tagged_acc, state, log, leading_newlines) do
    with {:ok, kw_list, state, log} <-
           Keywords.parse_kw_data(state, Context.container_expr(), log) do
      {state, _newlines} = EOE.skip_count_newlines(state, 0)

      case TokenAdapter.next(state) do
        {:ok, %{kind: :"}"} = close_tok, state} ->
          close_meta = token_meta(close_tok.metadata)
          elements = Enum.reverse(tagged_acc) |> finalize_tuple_elements()
          {:ok, elements ++ [kw_list], leading_newlines, close_meta, state, log}

        {:ok, tok, state} ->
          {:error, {:expected, :"}", got: tok.kind}, state, log}

        {:eof, state} ->
          {:error, :unexpected_eof, state, log}

        {:error, diag, state} ->
          {:error, diag, state, log}
      end
    end
  end
end
