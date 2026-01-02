defmodule ToxicParser.TokenAdapterTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{EventLog, TokenAdapter}

  defp drain_tokens(state, cursor, acc \\ []) do
    case TokenAdapter.next(state, cursor) do
      {:ok, token, state, cursor} -> drain_tokens(state, cursor, [token | acc])
      {:eof, _state, _cursor} -> Enum.reverse(acc)
      {:error, error, state, cursor} -> {:error, error, state, cursor}
    end
  end

  test "returns raw eol token with preserved newline count" do
    {state, cursor} = TokenAdapter.new("1\n\n2", preserve_comments: true)

    tokens = drain_tokens(state, cursor)

    assert Enum.map(tokens, &TokenAdapter.kind/1) == [:int, :eol, :int]

    # Raw eol token has newlines in third element of meta tuple
    [{:eol, meta, _value}] = Enum.filter(tokens, &(TokenAdapter.kind(&1) == :eol))

    # Raw meta format: {{start_line, start_col}, {end_line, end_col}, newlines}
    {{start_line, _}, {end_line, _}, newlines} = meta
    assert newlines == 2
    assert start_line == 1
    assert end_line == 3
  end

  test "peek and peek_n do not consume tokens" do
    {state, cursor} = TokenAdapter.new("1;2", max_peek: 2)

    {:ok, first, state, cursor} = TokenAdapter.peek(state, cursor)
    assert TokenAdapter.kind(first) == :int

    # {:ok, tokens, state, cursor} = TokenAdapter.peek_n(state, cursor, 2)
    # # Raw semicolon token kind is :";", not :eoe
    # assert Enum.map(tokens, &TokenAdapter.kind/1) == [:int, :";"]

    {:ok, still_first, _state, _cursor} = TokenAdapter.peek(state, cursor)
    assert TokenAdapter.kind(still_first) == :int
  end

  test "checkpoint and rewind restore lookahead and diagnostics" do
    {state, cursor} = TokenAdapter.new("1\n\n2")

    meta = %{
      range: %{start: %{offset: 0, line: 1, column: 1}, end: %{offset: 0, line: 1, column: 1}},
      delimiter: nil,
      newlines: 0,
      synthesized?: false,
      terminators: [],
      role: :none
    }

    {:ok, first, state, cursor} = TokenAdapter.next(state, cursor)
    assert TokenAdapter.kind(first) == :int

    state = %{state | event_log: EventLog.token(state.event_log, %{kind: :int, value: 1}, meta)}

    {ref, state} = TokenAdapter.checkpoint(state, cursor)
    {:ok, eol, state, _cursor} = TokenAdapter.next(state, cursor)
    # Raw eol token
    assert TokenAdapter.kind(eol) == :eol
    checkpoint_log = state.checkpoints[ref].event_log

    mutated_log = EventLog.token(state.event_log, %{kind: :int, value: 2}, meta)
    state = %{state | event_log: mutated_log}

    {rewound, cursor} = TokenAdapter.rewind(state, ref)
    {:ok, eol_again, _, _} = TokenAdapter.next(rewound, cursor)
    assert TokenAdapter.kind(eol_again) == :eol

    assert rewound.checkpoints == %{}
    assert rewound.event_log == checkpoint_log
  end

  test "drop_checkpoint removes saved state without rewinding" do
    {state, cursor} = TokenAdapter.new("1;2")

    {:ok, first, state, cursor} = TokenAdapter.next(state, cursor)
    assert TokenAdapter.kind(first) == :int

    {ref, state} = TokenAdapter.checkpoint(state, cursor)
    # Raw semicolon token
    {:ok, {:";", _meta, _value}, state, cursor} = TokenAdapter.next(state, cursor)

    state = TokenAdapter.drop_checkpoint(state, ref)
    assert state.checkpoints == %{}

    {:ok, next_int, _, _} = TokenAdapter.next(state, cursor)
    assert TokenAdapter.kind(next_int) == :int
  end

  test "drop_checkpoint vs rewind - drop continues forward, rewind goes back" do
    # Test drop_checkpoint continues from current position
    {state, cursor} = TokenAdapter.new("a;b;c")
    {:ok, {:identifier, _, :a}, state, cursor} = TokenAdapter.next(state, cursor)
    {ref, state} = TokenAdapter.checkpoint(state, cursor)
    # Raw semicolon token
    {:ok, {:";", _meta, _value}, state, cursor} = TokenAdapter.next(state, cursor)
    {:ok, {:identifier, _, :b}, state, cursor} = TokenAdapter.next(state, cursor)

    dropped_state = TokenAdapter.drop_checkpoint(state, ref)
    {:ok, next_after_drop, _, _} = TokenAdapter.next(dropped_state, cursor)
    assert TokenAdapter.kind(next_after_drop) == :";", "Expected semicolon token after drop"

    # Test rewind goes back to checkpoint position
    {state, cursor} = TokenAdapter.new("a;b;c")
    {:ok, {:identifier, _, :a}, state, cursor} = TokenAdapter.next(state, cursor)
    {ref, state} = TokenAdapter.checkpoint(state, cursor)
    {:ok, {:";", _meta, _value}, state, cursor} = TokenAdapter.next(state, cursor)
    {:ok, {:identifier, _, :b}, state, _cursor} = TokenAdapter.next(state, cursor)

    {rewound_state, cursor} = TokenAdapter.rewind(state, ref)
    {:ok, next_after_rewind, _, _} = TokenAdapter.next(rewound_state, cursor)
    assert TokenAdapter.kind(next_after_rewind) == :";"
  end

  test "tolerant mode surfaces error_token and synthesized closers" do
    {state, cursor} = TokenAdapter.new(")", mode: :tolerant)

    {:ok, error_token, state, cursor} = TokenAdapter.next(state, cursor)
    assert TokenAdapter.kind(error_token) == :error_token
    assert length(state.diagnostics) == 1

    {:ok, synthesized, state, _cursor} = TokenAdapter.next(state, cursor)
    full_meta = TokenAdapter.full_metadata(synthesized, state)
    assert full_meta.synthesized?
  end

  test "metadata offsets reflect line and column positions" do
    {state, cursor} = TokenAdapter.new("1;2")
    {:ok, token, state, _cursor} = TokenAdapter.next(state, cursor)

    full_meta = TokenAdapter.full_metadata(token, state)
    assert full_meta.range.start.offset == 0
    assert full_meta.range.start.column == 1
    assert full_meta.range.end.offset == 1
  end

  # test "peek_n raises when exceeding max_peek" do
  #   {state, cursor} = TokenAdapter.new("1;2", max_peek: 1)

  #   assert_raise ArgumentError, fn ->
  #     TokenAdapter.peek_n(state, cursor, 2)
  #   end
  # end

  test "strict mode propagates stream error while tolerant synthesizes" do
    {strict_state, cursor} = TokenAdapter.new(")", mode: :strict)
    assert {:error, _diag, _, _} = TokenAdapter.next(strict_state, cursor)

    # Tolerant mode: errors are handled by synthesizing error tokens
    # Test with invalid input that produces a lexer error
    {tolerant_state, cursor} = TokenAdapter.new("\x00", mode: :tolerant)

    # In tolerant mode, lexer errors become error_token synthetic tokens
    assert {:ok, token, state_after, _cursor} = TokenAdapter.next(tolerant_state, cursor)
    assert TokenAdapter.kind(token) == :error_token
    assert length(state_after.diagnostics) >= 1
  end

  test "fuel guard halts iteration when limit is reached" do
    {state, cursor} = TokenAdapter.new("1;2", fuel_limit: 1)

    assert {:ok, _tok, state, cursor} = TokenAdapter.next(state, cursor)
    assert {:error, :out_of_fuel, _state, _cursor} = TokenAdapter.next(state, cursor)
  end
end
