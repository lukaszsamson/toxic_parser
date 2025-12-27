defmodule ToxicParser.Cursor do
  @moduledoc """
  Tuple hot-state cursor over `Toxic.Driver`.

  Hot state is kept as a tuple to avoid per-token struct/map updates.
  Lookahead uses a two-list queue to avoid appends.

  This replaces both the Toxic stream batching AND the TokenAdapter lookahead
  with a single unified lookahead mechanism.
  """

  @type token :: Toxic.token()

  # {rest, driver, la_front, la_back, max_peek, la_size}
  @opaque t ::
            {charlist(), Toxic.Driver.t(), [token()], [token()], pos_integer(), non_neg_integer()}

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

    max_peek = Keyword.get(opts, :max_peek, 4)
    {rest, driver, [], [], max_peek, 0}
  end

  @spec next(t()) :: {:ok, token(), t()} | {:eof, t()} | {:error, term(), t()}
  # Fast path: front has tokens
  def next({rest, driver, [tok | front], back, max_peek, size}) do
    {:ok, tok, {rest, driver, front, back, max_peek, size - 1}}
  end

  # Flip path: front empty, back has tokens
  def next({rest, driver, [], [_ | _] = back, max_peek, size}) do
    [tok | front] = Enum.reverse(back)
    {:ok, tok, {rest, driver, front, [], max_peek, size - 1}}
  end

  # Fetch path: both queues empty, get from driver
  def next({rest, driver, [], [], max_peek, 0}) do
    case Toxic.Driver.next(rest, driver) do
      {:ok, tok, rest, driver} -> {:ok, tok, {rest, driver, [], [], max_peek, 0}}
      {:eof, driver} -> {:eof, {[], driver, [], [], max_peek, 0}}
      {:error, reason, rest, driver} -> {:error, reason, {rest, driver, [], [], max_peek, 0}}
    end
  end

  @spec peek(t()) :: {:ok, token(), t()} | {:eof, t()} | {:error, term(), t()}
  # Fast path: front has tokens
  def peek({_rest, _driver, [tok | _], _back, _max_peek, _size} = cursor) do
    {:ok, tok, cursor}
  end

  # Flip path: front empty, back has tokens
  def peek({rest, driver, [], [_ | _] = back, max_peek, size}) do
    [tok | _] = front = Enum.reverse(back)
    {:ok, tok, {rest, driver, front, [], max_peek, size}}
  end

  # Fetch path: both queues empty, get from driver
  def peek({rest, driver, [], [], max_peek, 0} = _cursor) do
    case Toxic.Driver.next(rest, driver) do
      {:ok, tok, rest, driver} ->
        {:ok, tok, {rest, driver, [tok], [], max_peek, 1}}

      {:eof, driver} ->
        {:eof, {[], driver, [], [], max_peek, 0}}

      {:error, reason, rest, driver} ->
        {:error, reason, {rest, driver, [], [], max_peek, 0}}
    end
  end

  # @spec peek_n(t(), pos_integer()) ::
  #         {:ok, [token()], t()} | {:eof, [token()], t()} | {:error, term(), [token()], t()}
  # def peek_n({_rest, _driver, _front, _back, max, _size} = _cursor, n) when n > max do
  #   raise ArgumentError, "peek_n/2 capped at #{max}"
  # end

  # def peek_n(cursor, n) when is_integer(n) and n > 0 do
  #   case ensure_lookahead(cursor, n) do
  #     {:ok, cursor} -> {:ok, take_queue(cursor, n), cursor}
  #     {:eof, cursor} -> {:eof, take_queue(cursor, n), cursor}
  #     {:error, reason, cursor} -> {:error, reason, take_queue(cursor, n), cursor}
  #   end
  # end

  @doc """
  Push a token back to the front of the lookahead.
  The pushed token will be the next one returned by next/1.
  """
  @spec pushback(t(), token()) :: t()
  def pushback({rest, driver, front, back, max_peek, size}, token) do
    {rest, driver, [token | front], back, max_peek, size + 1}
  end

  @doc """
  Push multiple tokens back to the front of the lookahead.
  Tokens must be in source order (first token in list = next to be consumed).
  """
  @spec pushback_many(t(), [token()]) :: t()
  def pushback_many(cursor, []), do: cursor

  def pushback_many({rest, driver, front, back, max_peek, size}, tokens) do
    new_front = tokens ++ front
    {rest, driver, new_front, back, max_peek, size + length(tokens)}
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
  def current_terminators({_rest, driver, _front, _back, _max_peek, _size}) do
    Toxic.Driver.current_terminators(driver)
  end

  @doc "Get current position (line, column) from the driver."
  @spec position(t()) :: {pos_integer(), pos_integer()}
  def position({_rest, driver, _front, _back, _max_peek, _size}) do
    # Driver has line/column fields directly
    {driver.line, driver.column}
  end

  @doc "Check if cursor is at EOF (no more tokens in lookahead or source)."
  @spec eof?(t()) :: boolean()
  def eof?({[], _driver, [], [], _max_peek, 0}), do: true
  def eof?(_cursor), do: false

  # defp ensure_lookahead(cursor, n) do
  #   cursor = maybe_flip(cursor)

  #   case cursor do
  #     {_rest, _driver, _front, _back, _max_peek, size} when size >= n ->
  #       {:ok, cursor}

  #     _ ->
  #       case fetch_one(cursor) do
  #         {:ok, cursor} -> ensure_lookahead(cursor, n)
  #         {:eof, cursor} -> {:eof, cursor}
  #         {:error, reason, cursor} -> {:error, reason, cursor}
  #       end
  #   end
  # end

  # defp fetch_one({rest, driver, front, back, max_peek, size}) do
  #   case Toxic.Driver.next(rest, driver) do
  #     {:ok, tok, rest, driver} ->
  #       {:ok, {rest, driver, front, [tok | back], max_peek, size + 1}}

  #     {:eof, driver} ->
  #       {:eof, {[], driver, front, back, max_peek, size}}

  #     {:error, reason, rest, driver} ->
  #       {:error, reason, {rest, driver, front, back, max_peek, size}}
  #   end
  # end

  # defp maybe_flip({rest, driver, [], back, max_peek, size}) when back != [] do
  #   {rest, driver, Enum.reverse(back), [], max_peek, size}
  # end

  # defp maybe_flip(cursor), do: cursor

  # defp take_queue(cursor, n) do
  #   {_rest, _driver, front, back, _max_peek, _size} = maybe_flip(cursor)

  #   head = Enum.take(front, n)

  #   if length(head) == n do
  #     head
  #   else
  #     head ++ (back |> Enum.reverse() |> Enum.take(n - length(head)))
  #   end
  # end
end
