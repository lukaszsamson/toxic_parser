defmodule ToxicParser.Cursor do
  @moduledoc """
  Tuple hot-state cursor over `Toxic.Driver`.

  Hot state is kept as a tuple to avoid per-token struct/map updates.
  Uses a simple list for lookahead - tokens are only ever prepended to front.

  This replaces both the Toxic stream batching AND the TokenAdapter lookahead
  with a single unified lookahead mechanism.
  """

  @type token :: Toxic.token()

  # {rest, driver, lookahead}
  # Simplified from 4-tuple - back list was never used
  @opaque t :: {charlist(), Toxic.Driver.t(), [token()]}

  @spec new(binary() | charlist(), keyword()) :: t()
  def new(source, opts \\ []) when is_binary(source) or is_list(source) do
    rest = if is_binary(source), do: String.to_charlist(source), else: source

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
        :error_token_payload
      ])
      |> Keyword.put(:error_mode, if(mode == :strict, do: :strict, else: :tolerant))

    driver = Toxic.Driver.new(driver_opts)

    {rest, driver, []}
  end

  @spec next(t()) :: {:ok, token(), t()} | {:eof, t()} | {:error, term(), t()}
  # Fast path: lookahead has tokens
  def next({rest, driver, [tok | lookahead]}) do
    {:ok, tok, {rest, driver, lookahead}}
  end

  # Fetch path: lookahead empty, get from driver
  def next({rest, driver, []}) do
    case Toxic.Driver.next(rest, driver) do
      {:ok, tok, rest, driver} -> {:ok, tok, {rest, driver, []}}
      {:eof, driver} -> {:eof, {[], driver, []}}
      {:error, reason, rest, driver} -> {:error, reason, {rest, driver, []}}
    end
  end

  @spec peek(t()) :: {:ok, token(), t()} | {:eof, t()} | {:error, term(), t()}
  # Fast path: lookahead has tokens
  def peek({_rest, _driver, [tok | _]} = cursor) do
    {:ok, tok, cursor}
  end

  # Fetch path: lookahead empty, get from driver
  def peek({rest, driver, []}) do
    case Toxic.Driver.next(rest, driver) do
      {:ok, tok, rest, driver} ->
        {:ok, tok, {rest, driver, [tok]}}

      {:eof, driver} ->
        {:eof, {[], driver, []}}

      {:error, reason, rest, driver} ->
        {:error, reason, {rest, driver, []}}
    end
  end

  @doc """
  Push a token back to the front of the lookahead.
  The pushed token will be the next one returned by next/1.
  """
  @spec pushback(t(), token()) :: t()
  def pushback({rest, driver, lookahead}, token) do
    {rest, driver, [token | lookahead]}
  end

  @doc """
  Push multiple tokens back to the front of the lookahead.
  Tokens must be in source order (first token in list = next to be consumed).
  """
  @spec pushback_many(t(), [token()]) :: t()
  def pushback_many(cursor, []), do: cursor

  def pushback_many({rest, driver, lookahead}, tokens) do
    {rest, driver, tokens ++ lookahead}
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
  def current_terminators({_rest, driver, _lookahead}) do
    Toxic.Driver.current_terminators(driver)
  end

  @doc "Get current position (line, column) from the driver."
  @spec position(t()) :: {pos_integer(), pos_integer()}
  def position({_rest, driver, _lookahead}) do
    # Driver has line/column fields directly
    {driver.line, driver.column}
  end

  @doc "Check if cursor is at EOF (no more tokens in lookahead or source)."
  @spec eof?(t()) :: boolean()
  def eof?({[], _driver, []}), do: true
  def eof?(_cursor), do: false
end
