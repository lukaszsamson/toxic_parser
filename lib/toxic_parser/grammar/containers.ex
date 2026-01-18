defmodule ToxicParser.Grammar.Containers do
  @moduledoc """
  Container parsing for lists and tuples (Phase 6 scaffolding).
  """

  alias ToxicParser.{
    Builder,
    Context,
    Cursor,
    Error,
    EventLog,
    ExprClass,
    NoParensErrors,
    ParseOpts,
    Pratt,
    State,
    TokenAdapter
  }

  alias ToxicParser.Builder.Meta
  alias ToxicParser.Grammar.{Bitstrings, Delimited, EOE, Expressions, Keywords, Maps, Stabs}

  @type result ::
          {:ok, Macro.t(), State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}
          | {:no_container, State.t(), Cursor.t()}

  @spec parse(
          State.t(),
          Cursor.t(),
          Pratt.context(),
          EventLog.t(),
          non_neg_integer(),
          ParseOpts.t()
        ) ::
          result()
  def parse(
        %State{} = state,
        cursor,
        %Context{} = ctx,
        %EventLog{} = log,
        min_bp \\ 0,
        %ParseOpts{} = opts \\ %ParseOpts{}
      ) do
    case Cursor.peek(cursor) do
      {:ok, {:"(", _meta, _value}, cursor} ->
        parse_paren(state, cursor, ctx, log, min_bp, opts)

      {:ok, {:"[", _meta, _value}, cursor} ->
        parse_list(state, cursor, ctx, log, min_bp, opts)

      {:ok, {:"{", _meta, _value}, cursor} ->
        parse_tuple(state, cursor, ctx, log, min_bp, opts)

      {:ok, {:%{}, _meta, _value}, cursor} ->
        Maps.parse_map(state, cursor, ctx, log, min_bp)

      {:ok, {:%, _meta, _value}, cursor} ->
        Maps.parse_map(state, cursor, ctx, log, min_bp)

      {:ok, {:"<<", _meta, _value}, cursor} ->
        Bitstrings.parse(state, cursor, ctx, log, min_bp)

      {:eof, cursor} ->
        {:no_container, state, cursor}

      {:error, _reason, cursor} ->
        {:no_container, state, cursor}

      {:ok, _next_tok, cursor} ->
        {:no_container, state, cursor}
    end
  end

  @doc """
  Parse a container without calling Pratt.led at the end.
  Used when the caller needs to control operator binding (e.g., stab patterns).
  """
  @spec parse_container_base(State.t(), Cursor.t(), Pratt.context(), EventLog.t()) :: result()
  def parse_container_base(%State{} = state, cursor, %Context{} = ctx, %EventLog{} = log) do
    case Cursor.peek(cursor) do
      {:ok, {:"[", _meta, _value}, cursor} ->
        parse_list_base(state, cursor, ctx, log)

      {:ok, {:"{", _meta, _value}, cursor} ->
        parse_tuple_base(state, cursor, ctx, log)

      {:ok, {:"<<", _meta, _value}, cursor} ->
        Bitstrings.parse_base(state, cursor, ctx, log)

      {:eof, cursor} ->
        {:no_container, state, cursor}

      {:error, _reason, cursor} ->
        {:no_container, state, cursor}

      {:ok, _next_tok, cursor} ->
        {:no_container, state, cursor}
    end
  end

  # Parse parenthesized expression, empty parens, or stab expression
  # Grammar rules:
  #   access_expr -> open_paren stab_eoe ')'         : build_paren_stab
  #   access_expr -> open_paren ';' stab_eoe ')'     : build_paren_stab
  #   access_expr -> open_paren ';' close_paren      : build_paren_stab with nil
  #   access_expr -> empty_paren                     : wrap in __block__
  defp parse_paren(state, cursor, ctx, log, min_bp, opts) do
    {:ok, open_tok, state, cursor} = TokenAdapter.next(state, cursor)
    open_meta = TokenAdapter.token_meta(open_tok)

    # Skip leading newlines (but not semicolons) and count them
    {state, cursor, newlines} = skip_eoe_not_semicolon_with_count(state, cursor, 0)

    # Check for leading semicolon (forces stab interpretation)
    case Cursor.peek(cursor) do
      {:ok, tok, cursor} ->
        if semicolon?(tok) do
          # Leading semicolon - parse as stab or empty
          {:ok, _semi, state, cursor} = TokenAdapter.next(state, cursor)
          # Skip any additional EOE and count newlines
          {state, cursor, more_newlines} = EOE.skip_count_newlines(state, cursor, 0)

          Stabs.parse_paren_stab_or_empty(
            open_meta,
            newlines + more_newlines,
            state,
            cursor,
            ctx,
            log,
            min_bp
          )
        else
          # Skip any remaining EOE tokens and count newlines
          {state, cursor, more_newlines} = EOE.skip_count_newlines(state, cursor, 0)

          parse_paren_content(
            open_meta,
            newlines + more_newlines,
            state,
            cursor,
            ctx,
            log,
            min_bp,
            opts
          )
        end

      {:eof, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  # Parse content after open paren (no leading semicolon)
  defp parse_paren_content(open_meta, newlines, state, cursor, ctx, log, min_bp, opts) do
    case Cursor.peek(cursor) do
      # Empty parens: () -> {:__block__, [parens: ...], []}
      # NOTE: parens: metadata doesn't include newlines (only line, column, closing)
      {:ok, {:")", _close_meta, _value} = close_tok, cursor} ->
        {:ok, _close, state, cursor} = TokenAdapter.next(state, cursor)
        close_meta_kw = TokenAdapter.token_meta(close_tok)

        parens_meta = [
          parens: Meta.closing_meta(open_meta, close_meta_kw, 0, [], base_first: true)
        ]

        ast = {:__block__, parens_meta, []}
        state = maybe_warn_empty_paren(state, open_meta)
        Pratt.led(ast, state, cursor, log, min_bp, ctx, opts)

      # Always parse as stab_eoe (YRL-aligned); paren stab builder decides block vs stab.
      {:ok, _, cursor} ->
        Stabs.parse_paren_stab(open_meta, newlines, state, cursor, ctx, log, min_bp)

      {:eof, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  defp skip_eoe_not_semicolon_with_count(state, cursor, count) do
    case Cursor.peek(cursor) do
      {:ok, tok, cursor} ->
        if semicolon?(tok) do
          {state, cursor, count}
        else
          case tok do
            {:eol, {_, _, n}, _value} when is_integer(n) ->
              {:ok, _eol, state, cursor} = TokenAdapter.next(state, cursor)
              skip_eoe_not_semicolon_with_count(state, cursor, count + n)

            {:eol, _meta, _value} ->
              {:ok, _eol, state, cursor} = TokenAdapter.next(state, cursor)
              skip_eoe_not_semicolon_with_count(state, cursor, count)

            _ ->
              {state, cursor, count}
          end
        end

      _ ->
        {state, cursor, count}
    end
  end

  defp semicolon?({:";", _meta, _value}), do: true
  defp semicolon?(_), do: false

  defp parse_list(state, cursor, ctx, log, min_bp, opts) do
    with {:ok, ast, state, cursor, log} <- parse_list_base(state, cursor, ctx, log) do
      # Continue with Pratt's led() to handle trailing operators
      Pratt.led(ast, state, cursor, log, min_bp, ctx, opts)
    end
  end

  # Parse list without calling Pratt.led - used when caller controls led binding
  defp parse_list_base(state, cursor, _ctx, log) do
    {:ok, open_tok, state, cursor} = TokenAdapter.next(state, cursor)
    open_meta = TokenAdapter.token_meta(open_tok)

    # Count leading newlines after [ for literal_encoder metadata
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    # In elixir_parser.yrl, list elements use container_expr (not the surrounding ctx).
    container_ctx = Context.container_expr()

    item_fun = fn state, cursor, _ctx, log ->
      case Keywords.try_parse_kw_data(state, cursor, container_ctx, log) do
        {:ok, kw_list, state, cursor, log} ->
          # Keyword data must come last in lists. If it's not followed by a close bracket,
          # raise an error at the next token (typically a comma).
          {state, cursor, _newlines} = EOE.skip_count_newlines(state, cursor, 0)

          case Cursor.peek(cursor) do
            {:ok, {:",", comma_meta, _value}, cursor} ->
              if state.mode == :tolerant do
                {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)
                {state, cursor, _} = EOE.skip_count_newlines(state, cursor, 0)

                case Expressions.expr(state, cursor, container_ctx, log) do
                  {:ok, expr, state, cursor, log} ->
                    {error_node, state} =
                      build_kw_tail_error_node(
                        handle_invalid_kw_tail_error_at(comma_meta),
                        comma_meta,
                        state,
                        cursor,
                        [expr]
                      )

                    {:ok, {:many, [{:expr, kw_list}, {:expr, error_node}]}, state, cursor, log}

                  {:error, reason, state, cursor, log} ->
                    {error_node, state} =
                      build_kw_tail_error_node(reason, comma_meta, state, cursor, [])

                    {:ok, {:many, [{:expr, kw_list}, {:expr, error_node}]}, state, cursor, log}
                end
              else
                {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)
                {:error, handle_invalid_kw_tail_error_at(comma_meta), state, cursor, log}
              end

            {:ok, {:"]", _meta, _value}, cursor} ->
              {:ok, {:kw_data, kw_list}, state, cursor, log}

            {:ok, {kind, _meta, _value}, cursor} ->
              {:error, {:expected, :"]", got: kind}, state, cursor, log}

            {:eof, cursor} ->
              {:error, :unexpected_eof, state, cursor, log}

            {:error, diag, cursor} ->
              {:error, diag, state, cursor, log}
          end

        {:no_kw, state, cursor, log} ->
          {ref, checkpoint_state} = TokenAdapter.checkpoint(state, cursor)

          case Expressions.expr(checkpoint_state, cursor, container_ctx, log) do
            {:ok, expr, state, cursor, log} ->
              # Validate no_parens expressions are not allowed in containers
              case ExprClass.classify(expr) do
                :no_parens ->
                  if state.mode == :tolerant do
                    {state, cursor} = TokenAdapter.rewind(state, ref)

                    {:error, NoParensErrors.error_no_parens_container_strict(expr), state,
                     cursor, log}
                  else
                    state = TokenAdapter.drop_checkpoint(state, ref)

                    {:error, NoParensErrors.error_no_parens_container_strict(expr), state, cursor,
                     log}
                  end

                _ ->
                  state = TokenAdapter.drop_checkpoint(state, ref)
                  {:ok, {:expr, expr}, state, cursor, log}
              end

            {:error, reason, state, cursor, log} ->
              state = TokenAdapter.drop_checkpoint(state, ref)
              {:error, reason, state, cursor, log}
          end

        {:error, {:expected, :keyword, _details}, state, cursor, log} ->
          {:error, handle_invalid_kw_tail_error(state, cursor), state, cursor, log}

        {:error, reason, state, cursor, log} ->
          {:error, reason, state, cursor, log}
      end
    end

    on_error = fn reason, state, cursor, _ctx, log ->
      meta = error_meta_from_reason(reason, cursor)
      {error_node, state} = build_container_error_node(reason, meta, state, cursor)
      {:ok, {:expr, error_node}, state, cursor, log}
    end

    with {:ok, tagged_items, state, cursor, log} <-
           Delimited.parse_comma_separated(state, cursor, container_ctx, log, :"]", item_fun,
             allow_empty?: true,
             skip_eoe_initial?: false,
             on_error: on_error
           ) do
      case TokenAdapter.next(state, cursor) do
        {:ok, {:"]", _meta, _value} = close_tok, state, cursor} ->
          close_meta = TokenAdapter.token_meta(close_tok)

          # Build list metadata with closing location and newlines (for token_metadata compatibility)
          list_meta = Meta.closing_meta(open_meta, close_meta, leading_newlines)
          list_ast = finalize_list_items(tagged_items)
          # Pass list through literal_encoder if present
          encoded_list = Builder.Helpers.literal(list_ast, list_meta, state)
          {:ok, encoded_list, state, cursor, log}

        {:ok, {kind, _meta, _value} = tok, state, cursor} ->
          if state.mode == :tolerant do
            {state, cursor} = TokenAdapter.pushback(state, cursor, tok)
            {error_ast, state} =
              build_container_error_node({:expected, :"]", got: kind}, open_meta, state, cursor)

            {:ok, error_ast, state, cursor, log}
          else
            {:error, {:expected, :"]", got: kind}, state, cursor, log}
          end

        {:eof, state, cursor} ->
          if state.mode == :tolerant do
            {error_ast, state} =
              build_container_error_node(:unexpected_eof, open_meta, state, cursor)

            {:ok, error_ast, state, cursor, log}
          else
            {:error, :unexpected_eof, state, cursor, log}
          end

        {:error, diag, state, cursor} ->
          if state.mode == :tolerant do
            {error_ast, state} = build_container_error_node(diag, open_meta, state, cursor)
            {:ok, error_ast, state, cursor, log}
          else
            {:error, diag, state, cursor, log}
          end
      end
    end
  end

  defp finalize_list_items([]), do: []

  defp finalize_list_items(tagged_items) do
    case List.last(tagged_items) do
      {:kw_data, kw_list} ->
        exprs =
          tagged_items
          |> Enum.drop(-1)
          |> Enum.map(fn {:expr, expr} -> expr end)

        exprs ++ kw_list

      _ ->
        Enum.map(tagged_items, fn {:expr, expr} -> expr end)
    end
  end

  defp parse_tuple(state, cursor, ctx, log, min_bp, opts) do
    with {:ok, ast, state, cursor, log} <- parse_tuple_base(state, cursor, ctx, log) do
      # Continue with Pratt's led() to handle trailing operators like <-, =, etc.
      Pratt.led(ast, state, cursor, log, min_bp, ctx, opts)
    end
  end

  defp maybe_warn_empty_paren(%State{} = state, _open_meta) when state.emit_warnings? == false do
    state
  end

  defp maybe_warn_empty_paren(state, open_meta) do
    warning = %ToxicParser.Warning{
      code: :empty_paren,
      message:
        "invalid expression (). If you want to invoke or define a function, make sure there are no spaces between the function name and its arguments. " <>
          "If you wanted to pass an empty block or code, pass a value instead, such as a nil or an atom",
      range: %{
        start: %{
          line: Keyword.get(open_meta, :line, 1),
          column: Keyword.get(open_meta, :column, 1),
          offset: 0
        },
        end: %{
          line: Keyword.get(open_meta, :line, 1),
          column: Keyword.get(open_meta, :column, 1),
          offset: 0
        }
      },
      details: %{}
    }

    %{state | warnings: [warning | state.warnings]}
  end

  # Parse tuple without calling Pratt.led - used when caller controls led binding
  defp parse_tuple_base(state, cursor, ctx, log) do
    {:ok, open_tok, state, cursor} = TokenAdapter.next(state, cursor)
    open_meta = TokenAdapter.token_meta(open_tok)

    with {:ok, elements, newlines, close_meta, state, cursor, log} <-
           parse_tuple_args(state, cursor, ctx, log, open_meta) do
      meta = Meta.closing_meta(open_meta, close_meta, newlines)

      # 2-element tuples are represented as literal {a, b} and are encodable
      # Other sizes use {:{}, meta, elements} which is NOT a primitive literal
      ast =
        case elements do
          [a, b] ->
            # 2-element tuples are primitive literals - pass through literal_encoder
            tuple = {a, b}
            Builder.Helpers.literal(tuple, meta, state)

          _ ->
            # Other tuples are AST nodes, not primitives - don't encode
            {:{}, meta, elements}
        end

      {:ok, ast, state, cursor, log}
    else
      {:error, reason, state, cursor, log} ->
        if state.mode == :tolerant do
          {error_ast, state} = build_container_error_node(reason, open_meta, state, cursor)
          {:ok, error_ast, state, cursor, log}
        else
          {:error, reason, state, cursor, log}
        end
    end
  end

  defp parse_tuple_args(state, cursor, _ctx, log, open_meta) do
    # Skip leading EOE and count newlines
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    container_ctx = Context.container_expr()

    item_fun = fn state, cursor, _ctx, log ->
      case Keywords.try_parse_kw_data(state, cursor, container_ctx, log) do
        {:ok, kw_list, state, cursor, log} ->
          # container_args allows a keyword tail as the final element. We only
          # treat this as kw_data if the close curly follows.
          {state, cursor, _newlines} = EOE.skip_count_newlines(state, cursor, 0)

          case Cursor.peek(cursor) do
            {:ok, {:"}", _meta, _value}, cursor} ->
              {:ok, {:kw_data, kw_list}, state, cursor, log}

            {:ok, {kind, _meta, _value}, cursor} ->
              {:error, {:expected, :"}", got: kind}, state, cursor, log}

            {:eof, cursor} ->
              {:error, :unexpected_eof, state, cursor, log}

            {:error, diag, cursor} ->
              {:error, diag, state, cursor, log}
          end

        {:no_kw, state, cursor, log} ->
          {ref, checkpoint_state} = TokenAdapter.checkpoint(state, cursor)

          case Expressions.expr(checkpoint_state, cursor, container_ctx, log) do
            {:ok, expr, state, cursor, log} ->
              # Validate no_parens expressions are not allowed in containers
              case ExprClass.classify(expr) do
                :no_parens ->
                  if state.mode == :tolerant do
                    {state, cursor} = TokenAdapter.rewind(state, ref)

                    {:error, NoParensErrors.error_no_parens_container_strict(expr), state,
                     cursor, log}
                  else
                    state = TokenAdapter.drop_checkpoint(state, ref)

                    {:error, NoParensErrors.error_no_parens_container_strict(expr), state, cursor,
                     log}
                  end

                _ ->
                  state = TokenAdapter.drop_checkpoint(state, ref)
                  {:ok, {:expr, expr}, state, cursor, log}
              end

            {:error, reason, state, cursor, log} ->
              state = TokenAdapter.drop_checkpoint(state, ref)
              {:error, reason, state, cursor, log}
          end

        {:error, {:expected, :keyword, _details}, state, cursor, log} ->
          {:error, handle_invalid_kw_tail_error(state, cursor), state, cursor, log}

        {:error, reason, state, cursor, log} ->
          {:error, reason, state, cursor, log}
      end
    end

    on_error = fn reason, state, cursor, _ctx, log ->
      meta = error_meta_from_reason(reason, cursor)
      {error_node, state} = build_container_error_node(reason, meta, state, cursor)
      {:ok, {:expr, error_node}, state, cursor, log}
    end

    with {:ok, tagged_items, state, cursor, log} <-
           Delimited.parse_comma_separated(state, cursor, container_ctx, log, :"}", item_fun,
             allow_empty?: true,
             on_error: on_error
           ) do
      # Check for invalid keyword list at start of tuple
      # {foo: :bar} is invalid - tuples cannot start with keyword data
      case tagged_items do
        [{:kw_data, _} | _] ->
          if state.mode == :tolerant do
            {error_ast, state} =
              build_container_error_node(
                unexpected_keyword_list_error(:tuple, open_meta),
                open_meta,
                state,
                cursor
              )

            tagged_items = [{:expr, error_ast}]

            case TokenAdapter.next(state, cursor) do
              {:ok, {:"}", _meta, _value} = close_tok, state, cursor} ->
                close_meta = TokenAdapter.token_meta(close_tok)

                {:ok, finalize_tuple_items(tagged_items), leading_newlines, close_meta, state,
                 cursor, log}

              {:ok, {kind, _meta, _value}, state, cursor} ->
                {:error, {:expected, :"}", got: kind}, state, cursor, log}

              {:eof, state, cursor} ->
                {:error, :unexpected_eof, state, cursor, log}

              {:error, diag, state, cursor} ->
                {:error, diag, state, cursor, log}
            end
          else
            {:error, unexpected_keyword_list_error(:tuple, open_meta), state, cursor, log}
          end

        _ ->
          case TokenAdapter.next(state, cursor) do
            {:ok, {:"}", _meta, _value} = close_tok, state, cursor} ->
              close_meta = TokenAdapter.token_meta(close_tok)

              {:ok, finalize_tuple_items(tagged_items), leading_newlines, close_meta, state,
               cursor, log}

            {:ok, {kind, _meta, _value}, state, cursor} ->
              {:error, {:expected, :"}", got: kind}, state, cursor, log}

            {:eof, state, cursor} ->
              {:error, :unexpected_eof, state, cursor, log}

            {:error, diag, state, cursor} ->
              {:error, diag, state, cursor, log}
          end
      end
    end
  end

  defp finalize_tuple_items([]), do: []

  defp finalize_tuple_items(tagged_items) do
    case List.last(tagged_items) do
      {:kw_data, kw_list} ->
        exprs =
          tagged_items
          |> Enum.drop(-1)
          |> Enum.map(fn {:expr, expr} -> expr end)

        exprs ++ [kw_list]

      _ ->
        Enum.map(tagged_items, fn {:expr, expr} -> expr end)
    end
  end

  defp unexpected_keyword_list_error(context, meta) do
    location = Keyword.take(meta, [:line, :column])
    start_token = if context == :bitstring, do: "'<<'", else: "'{'"

    {location,
     "unexpected keyword list inside #{context}. Did you mean to write a map (using %{...}) or a list (using [...]) instead? Syntax error after: ",
     start_token}
  end

  defp build_container_error_node(reason, meta, %State{} = state, cursor) do
    {line, column} =
      case {Keyword.get(meta, :line), Keyword.get(meta, :column)} do
        {nil, nil} -> Cursor.position(cursor)
        {line, column} -> {line || 1, column || 1}
      end

    {id, state} = State.next_diagnostic_id(state)

    diagnostic =
      Error.from_parser(nil, reason,
        line_index: state.line_index,
        source: state.source,
        position: {{line, column}, {line, column}}
      )
      |> Error.annotate(%{
        id: id,
        anchor: %{kind: :error_node, path: [], note: nil},
        synthetic?: false,
        lexer_error_code: nil
      })

    diagnostic = %{diagnostic | details: Map.put(diagnostic.details, :source, :grammar)}
    state = %{state | diagnostics: [diagnostic | state.diagnostics]}

    payload =
      Error.error_node_payload(diagnostic,
        kind: :unexpected,
        original: reason,
        synthetic?: false
      )

    error_meta = [line: line, column: column, toxic: %{synthetic?: false, anchor: %{line: line, column: column}}]
    {Builder.Helpers.error(payload, error_meta), state}
  end

  defp error_meta_from_reason(reason, cursor) do
    case reason do
      {meta, _msg, _token} when is_list(meta) ->
        meta

      {meta, _msg} when is_list(meta) ->
        meta

      {{line, column}, _, _} when is_integer(line) and is_integer(column) ->
        [line: line, column: column]

      _ ->
        {line, column} = Cursor.position(cursor)
        [line: line || 1, column: column || 1]
    end
  end

  defp handle_invalid_kw_tail_error(_state, cursor) do
    case Cursor.peek(cursor) do
      {:ok, {kind, meta, value}, _cursor} ->
        location = meta_location(meta)

        case kind do
          :kw_identifier ->
            {location,
             "unexpected expression after keyword list. Keyword lists must always come last in lists and maps. Therefore, this is not allowed:\n\n" <>
               "    [some: :value, :another]\n" <>
               "    %{some: :value, another => value}\n\n" <>
               "Instead, reorder it to be the last entry:\n\n" <>
               "    [:another, some: :value]\n" <>
               "    %{another => value, some: :value}\n\n" <>
               "Syntax error after: ", "','"}

          _ ->
            token_display = token_display(kind, value)
            {location, "syntax error before: ", token_display}
        end

      _ ->
        {[], "syntax error before: ", ""}
    end
  end

  defp handle_invalid_kw_tail_error_at(meta) do
    location =
      case meta do
        {{line, column}, _, _} -> [line: line, column: column]
        meta when is_list(meta) -> Keyword.take(meta, [:line, :column])
        _ -> []
      end

    {location,
     "unexpected expression after keyword list. Keyword lists must always come last in lists and maps. Therefore, this is not allowed:\n\n" <>
       "    [some: :value, :another]\n" <>
       "    %{some: :value, another => value}\n\n" <>
       "Instead, reorder it to be the last entry:\n\n" <>
       "    [:another, some: :value]\n" <>
       "    %{another => value, some: :value}\n\n" <>
       "Syntax error after: ", "','"}
  end

  defp build_kw_tail_error_node(reason, meta, %State{} = state, cursor, children) do
    {line, column} =
      case meta do
        {{line, column}, _, _} -> {line, column}
        meta when is_list(meta) -> {Keyword.get(meta, :line), Keyword.get(meta, :column)}
        _ -> Cursor.position(cursor)
      end

    {line, column} =
      case {line, column} do
        {nil, nil} -> Cursor.position(cursor)
        {line, column} -> {line || 1, column || 1}
      end

    {id, state} = State.next_diagnostic_id(state)

    diagnostic =
      Error.from_parser(nil, reason,
        line_index: state.line_index,
        source: state.source,
        position: {{line, column}, {line, column}}
      )
      |> Error.annotate(%{
        id: id,
        anchor: %{kind: :error_node, path: [], note: nil},
        synthetic?: false,
        lexer_error_code: nil
      })

    diagnostic = %{diagnostic | details: Map.put(diagnostic.details, :source, :grammar)}
    state = %{state | diagnostics: [diagnostic | state.diagnostics]}

    payload =
      Error.error_node_payload(diagnostic,
        kind: :unexpected,
        original: reason,
        children: children,
        synthetic?: false
      )

    error_meta = [line: line, column: column, toxic: %{synthetic?: false, anchor: %{line: line, column: column}}]
    {Builder.Helpers.error(payload, error_meta), state}
  end

  defp meta_location({{line, column}, _, _}), do: [line: line, column: column]
  defp meta_location(_), do: []

  defp token_display(:atom, value), do: inspect(value)
  defp token_display(_kind, value) when is_atom(value), do: "'#{value}'"
  defp token_display(_kind, value) when is_binary(value), do: "'#{value}'"
  defp token_display(kind, _value), do: "'#{kind}'"
end
