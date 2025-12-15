defmodule ToxicParser.Grammar.DoBlocks do
  @moduledoc false

  alias ToxicParser.{Builder, EventLog, NoParens, State, TokenAdapter}
  alias ToxicParser.Grammar.{Blocks, Keywords}

  @type result ::
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @spec maybe_do_block(Macro.t(), State.t(), any(), EventLog.t(), keyword()) :: result
  def maybe_do_block(ast, %State{} = state, ctx, %EventLog{} = log, opts \\ []) do
    allow_do_block? = allow_do_block?(ctx, opts)
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
        ctx,
        log,
        allow_do_block?,
        min_bp,
        parse_no_parens,
        clean_meta?
      )

    case result do
      {:ok, ast, state, log} ->
        if is_function(after_fn, 3) do
          after_fn.(ast, state, log)
        else
          {:ok, ast, state, log}
        end

      other ->
        other
    end
  end

  defp allow_do_block?(ctx, opts) do
    case Keyword.fetch(opts, :allow_do_block?) do
      {:ok, value} ->
        value

      :error ->
        cond do
          is_map(ctx) and Map.has_key?(ctx, :allow_do_block?) ->
            Map.get(ctx, :allow_do_block?)

          is_map(ctx) and Map.has_key?(ctx, :allow_do_block) ->
            Map.get(ctx, :allow_do_block)

          true ->
            ctx != :matched
        end
    end
  end

  defp maybe_handle(
         ast,
         token,
         state,
         ctx,
         log,
         allow_do_block?,
         min_bp,
         parse_no_parens,
         clean_meta?
       ) do
    case token && token.kind do
      :do_identifier ->
        handle_do_identifier(
          ast,
          token,
          state,
          ctx,
          log,
          allow_do_block?,
          min_bp,
          parse_no_parens,
          clean_meta?
        )

      :op_identifier ->
        maybe_parse_no_parens(ast, token, state, ctx, log, min_bp, parse_no_parens)

      :identifier ->
        handle_identifier(
          ast,
          token,
          state,
          ctx,
          log,
          allow_do_block?,
          min_bp,
          parse_no_parens,
          clean_meta?
        )

      _ ->
        attach_if_do(ast, state, ctx, log, allow_do_block?, clean_meta?)
    end
  end

  defp handle_do_identifier(
         ast,
         token,
         state,
         ctx,
         log,
         allow_do_block?,
         min_bp,
         parse_no_parens,
         clean_meta?
       ) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :do}, _} ->
        if allow_do_block? do
          attach_if_do(ast, state, ctx, log, allow_do_block?, clean_meta?)
        else
          {:ok, ast, state, log}
        end

      _ ->
        maybe_parse_no_parens(ast, token, state, ctx, log, min_bp, parse_no_parens)
    end
  end

  defp handle_identifier(
         ast,
         token,
         state,
         ctx,
         log,
         allow_do_block?,
         min_bp,
         parse_no_parens,
         clean_meta?
       ) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :do}, _} ->
        attach_if_do(ast, state, ctx, log, allow_do_block?, clean_meta?)

      _ ->
        maybe_parse_no_parens(ast, token, state, ctx, log, min_bp, parse_no_parens)
    end
  end

  defp attach_if_do(ast, state, ctx, log, true, clean_meta?) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :do}, _} ->
        with {:ok, {block_meta, sections}, state, log} <- Blocks.parse_do_block(state, ctx, log) do
          ast = attach_do_block(ast, block_meta, sections, clean_meta?)
          {:ok, ast, state, log}
        end

      _ ->
        {:ok, ast, state, log}
    end
  end

  defp attach_if_do(ast, state, _ctx, log, false, _clean_meta?) do
    {:ok, ast, state, log}
  end

  defp attach_do_block(ast, block_meta, sections, clean_meta?) do
    case ast do
      {name, meta, args} when is_list(args) ->
        clean_meta = clean_meta(meta, clean_meta?)
        {name, block_meta ++ clean_meta, args ++ [sections]}

      {name, meta, nil} when is_atom(name) ->
        {name, block_meta ++ meta, [sections]}

      other ->
        Builder.Helpers.call(other, [sections], block_meta)
    end
  end

  defp clean_meta(meta, true),
    do: meta |> Keyword.delete(:ambiguous_op) |> Keyword.delete(:no_parens)

  defp clean_meta(meta, false), do: meta

  defp maybe_parse_no_parens(ast, _token, state, _ctx, log, _min_bp, parse_no_parens)
       when not is_function(parse_no_parens, 5) do
    {:ok, ast, state, log}
  end

  defp maybe_parse_no_parens(ast, token, state, ctx, log, min_bp, parse_no_parens) do
    case TokenAdapter.peek(state) do
      {:ok, next_tok, _} ->
        if allow_no_parens?(token, next_tok) do
          parse_no_parens.(token, state, ctx, log, min_bp)
        else
          {:ok, ast, state, log}
        end

      _ ->
        {:ok, ast, state, log}
    end
  end

  defp allow_no_parens?(%{kind: :identifier}, next_tok) do
    next_tok.kind != :dual_op and starts_no_parens_arg?(next_tok)
  end

  defp allow_no_parens?(%{kind: kind}, next_tok) when kind in [:do_identifier, :op_identifier] do
    starts_no_parens_arg?(next_tok)
  end

  defp starts_no_parens_arg?(next_tok) do
    NoParens.can_start_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok)
  end
end
