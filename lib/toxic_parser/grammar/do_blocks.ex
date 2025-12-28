defmodule ToxicParser.Grammar.DoBlocks do
  @moduledoc false

  require ToxicParser.NoParens

  alias ToxicParser.{Builder, Context, Cursor, EventLog, NoParens, State}
  alias ToxicParser.Grammar.{Blocks, Keywords}

  @type result ::
          {:ok, Macro.t(), State.t(), ToxicParser.Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), ToxicParser.Cursor.t(), EventLog.t()}

  @spec maybe_do_block(
          Macro.t(),
          State.t(),
          ToxicParser.Cursor.t(),
          any(),
          EventLog.t(),
          keyword()
        ) ::
          result
  def maybe_do_block(
        ast,
        %State{} = state,
        cursor,
        %Context{} = ctx,
        %EventLog{} = log,
        opts \\ []
      ) do
    allow_do_block? = Context.allow_do_block?(ctx)
    clean_meta? = Keyword.get(opts, :clean_meta?, true)
    token = Keyword.get(opts, :token)
    min_bp = Keyword.get(opts, :min_bp)
    parse_no_parens = Keyword.get(opts, :parse_no_parens)
    after_fn = Keyword.get(opts, :after)

    result =
      maybe_handle(
        ast,
        token,
        state,
        cursor,
        ctx,
        log,
        allow_do_block?,
        min_bp,
        parse_no_parens,
        clean_meta?
      )

    case result do
      {:ok, ast, state, cursor, log} ->
        if is_function(after_fn, 4) do
          after_fn.(ast, state, cursor, log)
        else
          {:ok, ast, state, cursor, log}
        end

      other ->
        other
    end
  end

  defp maybe_handle(
         ast,
         token,
         state,
         cursor,
         ctx,
         log,
         allow_do_block?,
         min_bp,
         parse_no_parens,
         clean_meta?
       ) do
    case token do
      {:do_identifier, _meta, _value} ->
        handle_do_identifier(
          ast,
          token,
          state,
          cursor,
          ctx,
          log,
          allow_do_block?,
          min_bp,
          parse_no_parens,
          clean_meta?
        )

      {:op_identifier, _meta, _value} ->
        raise "dead code"
        maybe_parse_no_parens(ast, token, state, cursor, ctx, log, min_bp, parse_no_parens)

      {:identifier, _meta, _value} ->
        handle_identifier(
          ast,
          token,
          state,
          cursor,
          ctx,
          log,
          allow_do_block?,
          min_bp,
          parse_no_parens,
          clean_meta?
        )

      _ ->
        attach_if_do(ast, state, cursor, ctx, log, allow_do_block?, clean_meta?)
    end
  end

  defp handle_do_identifier(
         ast,
         token,
         state,
         cursor,
         ctx,
         log,
         allow_do_block?,
         min_bp,
         parse_no_parens,
         clean_meta?
       ) do
    case Cursor.peek(cursor) do
      {:ok, {:do, _meta, _value}, _cursor} ->
        if allow_do_block? do
          attach_if_do(ast, state, cursor, ctx, log, allow_do_block?, clean_meta?)
        else
          {:ok, ast, state, cursor, log}
        end

      _ ->
        maybe_parse_no_parens(ast, token, state, cursor, ctx, log, min_bp, parse_no_parens)
    end
  end

  defp handle_identifier(
         ast,
         token,
         state,
         cursor,
         ctx,
         log,
         _allow_do_block?,
         min_bp,
         parse_no_parens,
         _clean_meta?
       ) do
    maybe_parse_no_parens(ast, token, state, cursor, ctx, log, min_bp, parse_no_parens)
  end

  defp attach_if_do(ast, state, cursor, ctx, log, true, clean_meta?) do
    case Cursor.peek(cursor) do
      {:ok, {:do, _meta, _value}, _cursor} ->
        with {:ok, {block_meta, sections}, state, cursor, log} <-
               Blocks.parse_do_block(state, cursor, ctx, log) do
          ast = attach_do_block(ast, block_meta, sections, clean_meta?)
          {:ok, ast, state, cursor, log}
        end

      _ ->
        {:ok, ast, state, cursor, log}
    end
  end

  defp attach_if_do(ast, state, cursor, _ctx, log, false, _clean_meta?) do
    {:ok, ast, state, cursor, log}
  end

  defp attach_do_block(ast, block_meta, sections, clean_meta?) do
    case ast do
      {name, meta, args} when is_list(args) ->
        clean_meta = clean_meta(meta, clean_meta?)
        {name, block_meta ++ clean_meta, args ++ [sections]}

      {name, meta, nil} when is_atom(name) ->
        {name, block_meta ++ meta, [sections]}

      other ->
        raise "dead code"
        Builder.Helpers.call(other, [sections], block_meta)
    end
  end

  defp clean_meta(meta, true),
    do: meta |> Keyword.delete(:ambiguous_op) |> Keyword.delete(:no_parens)

  defp clean_meta(meta, false), do: meta

  defp maybe_parse_no_parens(ast, token, state, cursor, ctx, log, min_bp, parse_no_parens)
       when is_function(parse_no_parens, 6) do
    case Cursor.peek(cursor) do
      {:ok, next_tok, _cursor} ->
        if allow_no_parens?(token, next_tok) do
          # For op_identifier calls, use min_bp=0 to include all operators
          # in the argument per lexer disambiguation.
          # Example: %{c!s|n => 1} should parse c!(s|n) not c!(s)
          raise "dead code"
          effective_min_bp = if match?({:op_identifier, _, _}, token), do: 0, else: min_bp
          parse_no_parens.(token, state, cursor, ctx, log, effective_min_bp)
        else
          {:ok, ast, state, cursor, log}
        end

      _ ->
        {:ok, ast, state, cursor, log}
    end
  end

  defp allow_no_parens?({:identifier, _meta, _value}, next_tok) do
    case next_tok do
      {:dual_op, _next_meta, _next_value} -> false
      _ -> starts_no_parens_arg?(next_tok)
    end
  end

  defp starts_no_parens_arg?(next_tok) do
    NoParens.can_start_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok)
  end
end
