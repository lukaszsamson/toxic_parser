defmodule ToxicParser.Cursor do
  @moduledoc """
  Tuple hot-state cursor over `Toxic.Driver`.

  Cursor owns lookahead, emit queue, and lookbehind to avoid per-token driver
  struct churn in the parser loop.
  """

  @type token :: Toxic.token()
  @type input :: [] | [char()] | binary()
  @type driver_hot :: Toxic.Driver.driver_hot()
  @type cfg :: Toxic.Driver.cfg()
  @type lookbehind :: Toxic.Driver.lookbehind()
  @type last_token :: token() | nil
  @type step_result ::
          {:ok, token(), input(), pos_integer(), pos_integer(), driver_hot(), lookbehind()}
          | {:ok_many, [token()], input(), pos_integer(), pos_integer(), driver_hot(), lookbehind()}
          | {:eof, driver_hot()}
          | {:error, term(), input(), pos_integer(), pos_integer(), driver_hot()}

  # {rest, line, column, driver_hot, cfg, lookahead, emitq, last_token}
  @opaque t :: {input(), pos_integer(), pos_integer(), driver_hot(), cfg(), list(token()),
                list(token()), last_token()}

  @spec new(binary() | charlist(), keyword()) :: t()
  def new(source, opts \\ []) when is_binary(source) or is_list(source) do
    mode = Keyword.get(opts, :mode, :tolerant)

    driver_opts =
      opts
      |> Keyword.take([
        :elixir_compatibility,
        :preserve_comments,
        :existing_atoms_only,
        :static_atoms_encoder,
        :line,
        :column,
        :error_sync,
        :error_max_skip,
        :insert_structural_closers,
        :insert_identifier_sanitization,
        :error_token_payload,
        :lexer_backend
      ])
      |> Keyword.put(:error_mode, if(mode == :strict, do: :strict, else: :tolerant))

    cfg = Toxic.Driver.build_cfg(driver_opts)
    driver_hot = Toxic.Driver.build_hot(driver_opts)
    rest = normalize_input(source, cfg.lexer_backend)
    line = Keyword.get(driver_opts, :line, 1)
    column = Keyword.get(driver_opts, :column, 1)
    {rest, line, column, driver_hot, cfg, [], [], nil}
  end

  @spec next(t()) :: {:ok, token(), t()} | {:eof, t()} | {:error, term(), t()}
  def next({rest, line, column, driver_hot, cfg, [tok | lookahead], emitq, _last_token}) do
    {:ok, tok, {rest, line, column, driver_hot, cfg, lookahead, emitq, tok}}
  end

  def next({rest, line, column, driver_hot, cfg, [], [tok | emitq], _last_token}) do
    {:ok, tok, {rest, line, column, driver_hot, cfg, [], emitq, tok}}
  end

  def next(
        {rest, line, column,
         {scope, contexts, deferrals, output, _recent_token} = driver_hot, cfg, [], [],
         last_token}
      ) do
    if empty_input?(rest, cfg.lexer_backend) and deferrals == [] and output == [] and
         cfg.error_mode == :strict do
      strict_eof_or_error(line, column, scope, contexts, driver_hot, cfg, last_token)
    else
      lookbehind = lookbehind_from_token(last_token)

      case Toxic.Driver.step(rest, line, column, driver_hot, cfg, lookbehind) do
        {:ok, tok, rest, line, column, driver_hot, _lookbehind} ->
          {:ok, tok, {rest, line, column, driver_hot, cfg, [], [], tok}}

        {:ok_many, [tok | rest_tokens], rest, line, column, driver_hot, _lookbehind} ->
          {:ok, tok, {rest, line, column, driver_hot, cfg, [], rest_tokens, tok}}

        {:eof, driver_hot} ->
          {:eof, {empty_input(cfg.lexer_backend), line, column, driver_hot, cfg, [], [], last_token}}

        {:error, reason, rest, line, column, driver_hot} ->
          {:error, reason, {rest, line, column, driver_hot, cfg, [], [], last_token}}
      end
    end
  end

  @spec peek(t()) :: {:ok, token(), t()} | {:eof, t()} | {:error, term(), t()}
  def peek({_rest, _line, _column, _driver_hot, _cfg, [tok | _], _emitq, _last_token} = cursor) do
    {:ok, tok, cursor}
  end

  def peek({_rest, _line, _column, _driver_hot, _cfg, [], [tok | _], _last_token} = cursor) do
    {:ok, tok, cursor}
  end

  def peek(
        {rest, line, column,
         {scope, contexts, deferrals, output, _recent_token} = driver_hot, cfg, [], [],
         last_token}
      ) do
    if empty_input?(rest, cfg.lexer_backend) and deferrals == [] and output == [] and
         cfg.error_mode == :strict do
      strict_eof_or_error(line, column, scope, contexts, driver_hot, cfg, last_token)
    else
      lookbehind = lookbehind_from_token(last_token)

      case Toxic.Driver.step(rest, line, column, driver_hot, cfg, lookbehind) do
        {:ok, tok, rest, line, column, driver_hot, _lookbehind} ->
          {:ok, tok, {rest, line, column, driver_hot, cfg, [], [tok], last_token}}

        {:ok_many, [tok | rest_tokens], rest, line, column, driver_hot, _lookbehind} ->
          {:ok, tok, {rest, line, column, driver_hot, cfg, [], [tok | rest_tokens], last_token}}

        {:eof, driver_hot} ->
          {:eof, {empty_input(cfg.lexer_backend), line, column, driver_hot, cfg, [], [], last_token}}

        {:error, reason, rest, line, column, driver_hot} ->
          {:error, reason, {rest, line, column, driver_hot, cfg, [], [], last_token}}
      end
    end
  end

  @doc """
  Push a token back to the front of the lookahead.
  The pushed token will be the next one returned by next/1.
  """
  @spec pushback(t(), token()) :: t()
  def pushback({rest, line, column, driver_hot, cfg, lookahead, emitq, last_token}, token) do
    {rest, line, column, driver_hot, cfg, [token | lookahead], emitq, last_token}
  end

  @doc """
  Push multiple tokens back to the front of the lookahead.
  Tokens must be in source order (first token in list = next to be consumed).
  """
  @spec pushback_many(t(), [token()]) :: t()
  def pushback_many(cursor, []), do: cursor

  def pushback_many({rest, line, column, driver_hot, cfg, lookahead, emitq, last_token}, tokens) do
    {rest, line, column, driver_hot, cfg, tokens ++ lookahead, emitq, last_token}
  end

  @doc """
  Push multiple tokens back to the front of the lookahead.

  Tokens must be in reverse source order (last token in list = next to be consumed).
  This avoids allocating an intermediate reversed list at hot callsites.
  """
  @spec pushback_many_rev(t(), [token()]) :: t()
  def pushback_many_rev(cursor, []), do: cursor

  def pushback_many_rev(
        {rest, line, column, driver_hot, cfg, lookahead, emitq, last_token},
        tokens_rev
      ) do
    {rest, line, column, driver_hot, cfg, :lists.reverse(tokens_rev, lookahead), emitq, last_token}
  end

  @doc "Pay-for-play terminator snapshot (does not run on every token)."
  @spec current_terminators(t()) :: [term()]
  def current_terminators(
        {_rest, _line, _column, {scope, contexts, _deferrals, _output, _recent_token}, _cfg,
         _lookahead, _emitq, _last_token}
      ) do
    Toxic.Driver.current_terminators_from(scope, contexts)
  end

  @doc "Get current position (line, column) from the cursor."
  @spec position(t()) :: {pos_integer(), pos_integer()}
  def position({_rest, line, column, _driver_hot, _cfg, _lookahead, _emitq, _last_token}) do
    {line, column}
  end

  @doc "Check if cursor is at EOF (no more tokens in lookahead or source)."
  @spec eof?(t()) :: boolean()
  def eof?({rest, _line, _column, _driver_hot, cfg, lookahead, emitq, _last_token}) do
    empty? =
      case cfg.lexer_backend do
        :binary -> rest == <<>>
        _ -> rest == []
      end

    empty? and lookahead == [] and emitq == []
  end

  defp normalize_input(source, :binary) when is_binary(source), do: source
  defp normalize_input(source, :binary) when is_list(source), do: IO.iodata_to_binary(source)
  defp normalize_input(source, _backend) when is_binary(source), do: String.to_charlist(source)
  defp normalize_input(source, _backend) when is_list(source), do: source

  defp empty_input(:binary), do: <<>>
  defp empty_input(_backend), do: []

  defp empty_input?(rest, :binary), do: rest == <<>>
  defp empty_input?(rest, _backend), do: rest == []

  defp strict_eof_or_error(line, column, scope, contexts, driver_hot, cfg, last_token) do
    case Toxic.Driver.Contexts.pending_error(contexts, scope) do
      nil ->
        {:eof, {empty_input(cfg.lexer_backend), line, column, driver_hot, cfg, [], [], last_token}}

      error ->
        reason =
          case error do
            {:missing_interpolation, interp_context} ->
              Toxic.Driver.Contexts.missing_interpolation_reason(interp_context, line, column)

            {:missing_context, interp_context} ->
              Toxic.Driver.Contexts.missing_terminator_reason(interp_context, line, column)

            {:missing_scope, entry} ->
              Toxic.Driver.Contexts.missing_scope_terminator_reason(entry, line, column, scope)
          end

        {:error, Toxic.Error.to_reason_tuple(reason),
         {empty_input(cfg.lexer_backend), line, column, driver_hot, cfg, [], [], last_token}}
    end
  end

  defp lookbehind_from_token(nil), do: {nil, false, 0}

  defp lookbehind_from_token({kind, {_, _, count}, _} = token) when kind in [:eol, :";"] do
    {token, true, count}
  end

  defp lookbehind_from_token(token), do: {token, false, 0}

  @compile {:inline, lookbehind_from_token: 1, empty_input?: 2}
end
