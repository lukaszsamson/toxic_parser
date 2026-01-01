defmodule ToxicParser.Cursor do
  @moduledoc """
  Tuple hot-state cursor over `Toxic.Driver`.

  Cursor owns lookahead, emit queue, and lookbehind to avoid per-token driver
  struct churn in the parser loop.
  """

  @type token :: Toxic.token()
  @type input :: charlist() | binary()
  @type driver_hot :: Toxic.Driver.driver_hot()
  @type cfg :: Toxic.Driver.cfg()
  @type lookbehind :: Toxic.Driver.lookbehind()

  # {rest, line, column, driver_hot, cfg, lookahead, emitq, lookbehind}
  @opaque t :: {input(), pos_integer(), pos_integer(), driver_hot(), cfg(), [token()],
                [token()], lookbehind()}

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
    lookbehind = Toxic.Util.lookbehind_from_token(nil)

    {rest, line, column, driver_hot, cfg, [], [], lookbehind}
  end

  @spec next(t()) :: {:ok, token(), t()} | {:eof, t()} | {:error, term(), t()}
  def next({rest, line, column, driver_hot, cfg, [tok | lookahead], emitq, _lookbehind}) do
    lookbehind = Toxic.Util.lookbehind_from_token(tok)
    {:ok, tok, {rest, line, column, driver_hot, cfg, lookahead, emitq, lookbehind}}
  end

  def next({rest, line, column, driver_hot, cfg, [], [tok | emitq], _lookbehind}) do
    lookbehind = Toxic.Util.lookbehind_from_token(tok)
    {:ok, tok, {rest, line, column, driver_hot, cfg, [], emitq, lookbehind}}
  end

  def next({rest, line, column, driver_hot, cfg, [], [], lookbehind}) do
    case Toxic.Driver.step(rest, line, column, driver_hot, cfg, lookbehind) do
      {:ok, tok, rest, line, column, driver_hot, lookbehind} ->
        {:ok, tok, {rest, line, column, driver_hot, cfg, [], [], lookbehind}}

      {:ok_many, [tok | rest_tokens], rest, line, column, driver_hot, _lookbehind} ->
        lookbehind = Toxic.Util.lookbehind_from_token(tok)
        {:ok, tok, {rest, line, column, driver_hot, cfg, [], rest_tokens, lookbehind}}

      {:eof, driver_hot} ->
        {:eof, {empty_input(cfg.lexer_backend), line, column, driver_hot, cfg, [], [], lookbehind}}

      {:error, reason, rest, line, column, driver_hot} ->
        {:error, reason, {rest, line, column, driver_hot, cfg, [], [], lookbehind}}
    end
  end

  @spec peek(t()) :: {:ok, token(), t()} | {:eof, t()} | {:error, term(), t()}
  def peek({_rest, _line, _column, _driver_hot, _cfg, [tok | _], _emitq, _lookbehind} = cursor) do
    {:ok, tok, cursor}
  end

  def peek({_rest, _line, _column, _driver_hot, _cfg, [], [tok | _], _lookbehind} = cursor) do
    {:ok, tok, cursor}
  end

  def peek({rest, line, column, driver_hot, cfg, [], [], lookbehind}) do
    case Toxic.Driver.step(rest, line, column, driver_hot, cfg, lookbehind) do
      {:ok, tok, rest, line, column, driver_hot, _lookbehind} ->
        {:ok, tok, {rest, line, column, driver_hot, cfg, [], [tok], lookbehind}}

      {:ok_many, [tok | _] = tokens, rest, line, column, driver_hot, _lookbehind} ->
        {:ok, tok, {rest, line, column, driver_hot, cfg, [], tokens, lookbehind}}

      {:eof, driver_hot} ->
        {:eof, {empty_input(cfg.lexer_backend), line, column, driver_hot, cfg, [], [], lookbehind}}

      {:error, reason, rest, line, column, driver_hot} ->
        {:error, reason, {rest, line, column, driver_hot, cfg, [], [], lookbehind}}
    end
  end

  @doc """
  Push a token back to the front of the lookahead.
  The pushed token will be the next one returned by next/1.
  """
  @spec pushback(t(), token()) :: t()
  def pushback({rest, line, column, driver_hot, cfg, lookahead, emitq, lookbehind}, token) do
    {rest, line, column, driver_hot, cfg, [token | lookahead], emitq, lookbehind}
  end

  @doc """
  Push multiple tokens back to the front of the lookahead.
  Tokens must be in source order (first token in list = next to be consumed).
  """
  @spec pushback_many(t(), [token()]) :: t()
  def pushback_many(cursor, []), do: cursor

  def pushback_many({rest, line, column, driver_hot, cfg, lookahead, emitq, lookbehind}, tokens) do
    {rest, line, column, driver_hot, cfg, tokens ++ lookahead, emitq, lookbehind}
  end

  @doc """
  Push multiple tokens back to the front of the lookahead.

  Tokens must be in reverse source order (last token in list = next to be consumed).
  This avoids allocating an intermediate reversed list at hot callsites.
  """
  @spec pushback_many_rev(t(), [token()]) :: t()
  def pushback_many_rev(cursor, []), do: cursor

  def pushback_many_rev(
        {rest, line, column, driver_hot, cfg, lookahead, emitq, lookbehind},
        tokens_rev
      ) do
    {rest, line, column, driver_hot, cfg, :lists.reverse(tokens_rev, lookahead), emitq, lookbehind}
  end

  @doc """
  Create a lightweight checkpoint (just saves the cursor tuple).
  Use for backtracking token consumption only.
  """
  @spec mark(t()) :: t()
  def mark(cursor), do: cursor

  @doc """
  Rewind to a previously marked cursor state.
  """
  @spec rewind(t(), t()) :: t()
  def rewind(_cursor, mark), do: mark

  @doc "Pay-for-play terminator snapshot (does not run on every token)."
  @spec current_terminators(t()) :: [term()]
  def current_terminators(
        {_rest, _line, _column, {scope, contexts, _deferrals, _output, _recent_token}, _cfg,
         _lookahead, _emitq, _lookbehind}
      ) do
    Toxic.Driver.current_terminators_from(scope, contexts)
  end

  @doc "Get current position (line, column) from the cursor."
  @spec position(t()) :: {pos_integer(), pos_integer()}
  def position({_rest, line, column, _driver_hot, _cfg, _lookahead, _emitq, _lookbehind}) do
    {line, column}
  end

  @doc "Check if cursor is at EOF (no more tokens in lookahead or source)."
  @spec eof?(t()) :: boolean()
  def eof?({rest, _line, _column, _driver_hot, cfg, lookahead, emitq, _lookbehind}) do
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
end
