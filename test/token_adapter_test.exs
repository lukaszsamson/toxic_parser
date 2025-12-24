defmodule ToxicParser.TokenAdapterTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{EventLog, TokenAdapter}

  defp drain_tokens(state, acc \\ []) do
    case TokenAdapter.next(state) do
      {:ok, token, state} -> drain_tokens(state, [token | acc])
      {:eof, _state} -> Enum.reverse(acc)
      {:error, error, state} -> {:error, error, state}
    end
  end

  test "normalizes eol to eoe with preserved newline count" do
    state = TokenAdapter.new("1\n\n2", preserve_comments: true)

    tokens = drain_tokens(state)

    assert Enum.map(tokens, &TokenAdapter.kind/1) == [:int, :eoe, :int]

    [{:eoe, meta, %{newlines: newlines}}] =
      Enum.filter(tokens, &(TokenAdapter.kind(&1) == :eoe))

    assert newlines == 2
    # Raw meta format: {{start_line, start_col}, {end_line, end_col}, extra}
    {{start_line, _}, {end_line, _}, _} = meta
    assert start_line == 1
    assert end_line == 3
  end

  test "peek and peek_n do not consume tokens" do
    state = TokenAdapter.new("1;2", max_peek: 2)

    {:ok, first, state} = TokenAdapter.peek(state)
    assert TokenAdapter.kind(first) == :int

    {:ok, tokens, state} = TokenAdapter.peek_n(state, 2)
    assert Enum.map(tokens, &TokenAdapter.kind/1) == [:int, :eoe]

    {:ok, still_first, _state} = TokenAdapter.peek(state)
    assert TokenAdapter.kind(still_first) == :int
  end

  test "checkpoint and rewind restore lookahead and diagnostics" do
    state = TokenAdapter.new("1\n\n2")

    meta = %{
      range: %{start: %{offset: 0, line: 1, column: 1}, end: %{offset: 0, line: 1, column: 1}},
      delimiter: nil,
      newlines: 0,
      synthesized?: false,
      terminators: [],
      role: :none
    }

    {:ok, first, state} = TokenAdapter.next(state)
    assert TokenAdapter.kind(first) == :int

    state = %{state | event_log: EventLog.token(state.event_log, %{kind: :int, value: 1}, meta)}

    {ref, state} = TokenAdapter.checkpoint(state)
    {:ok, eoe, state} = TokenAdapter.next(state)
    assert TokenAdapter.kind(eoe) == :eoe
    checkpoint_log = state.checkpoints[ref].event_log

    mutated_log = EventLog.token(state.event_log, %{kind: :int, value: 2}, meta)
    state = %{state | event_log: mutated_log}

    rewound = TokenAdapter.rewind(state, ref)
    {:ok, eoe_again, _} = TokenAdapter.next(rewound)
    assert TokenAdapter.kind(eoe_again) == :eoe

    assert rewound.checkpoints == %{}
    assert rewound.event_log == checkpoint_log
  end

  test "drop_checkpoint removes saved state without rewinding" do
    state = TokenAdapter.new("1;2")

    {:ok, first, state} = TokenAdapter.next(state)
    assert TokenAdapter.kind(first) == :int

    {ref, state} = TokenAdapter.checkpoint(state)
    {:ok, {:eoe, _, _}, state} = TokenAdapter.next(state)

    state = TokenAdapter.drop_checkpoint(state, ref)
    assert state.checkpoints == %{}

    {:ok, next_int, _} = TokenAdapter.next(state)
    assert TokenAdapter.kind(next_int) == :int
  end

  test "drop_checkpoint vs rewind - drop continues forward, rewind goes back" do
    # Test drop_checkpoint continues from current position
    state = TokenAdapter.new("a;b;c")
    {:ok, {:identifier, _, :a}, state} = TokenAdapter.next(state)
    {ref, state} = TokenAdapter.checkpoint(state)
    {:ok, {:eoe, _, _}, state} = TokenAdapter.next(state)
    {:ok, {:identifier, _, :b}, state} = TokenAdapter.next(state)

    dropped_state = TokenAdapter.drop_checkpoint(state, ref)
    {:ok, next_after_drop, _} = TokenAdapter.next(dropped_state)
    assert TokenAdapter.kind(next_after_drop) == :eoe

    # Test rewind goes back to checkpoint position
    state = TokenAdapter.new("a;b;c")
    {:ok, {:identifier, _, :a}, state} = TokenAdapter.next(state)
    {ref, state} = TokenAdapter.checkpoint(state)
    {:ok, {:eoe, _, _}, state} = TokenAdapter.next(state)
    {:ok, {:identifier, _, :b}, state} = TokenAdapter.next(state)

    rewound_state = TokenAdapter.rewind(state, ref)
    {:ok, next_after_rewind, _} = TokenAdapter.next(rewound_state)
    assert TokenAdapter.kind(next_after_rewind) == :eoe
  end

  test "tolerant mode surfaces error_token and synthesized closers" do
    state = TokenAdapter.new(")", mode: :tolerant)

    {:ok, error_token, state} = TokenAdapter.next(state)
    assert TokenAdapter.kind(error_token) == :error_token
    assert length(state.diagnostics) == 1

    {:ok, synthesized, state} = TokenAdapter.next(state)
    full_meta = TokenAdapter.full_metadata(synthesized, state)
    assert full_meta.synthesized?
  end

  test "metadata offsets reflect line and column positions" do
    state = TokenAdapter.new("1;2")
    {:ok, token, state} = TokenAdapter.next(state)

    full_meta = TokenAdapter.full_metadata(token, state)
    assert full_meta.range.start.offset == 0
    assert full_meta.range.start.column == 1
    assert full_meta.range.end.offset == 1
  end

  test "peek_n raises when exceeding max_peek" do
    state = TokenAdapter.new("1;2", max_peek: 1)

    assert_raise ArgumentError, fn ->
      TokenAdapter.peek_n(state, 2)
    end
  end

  test "strict mode propagates stream error while tolerant synthesizes" do
    strict_state = TokenAdapter.new(")", mode: :strict)
    assert {:error, _diag, _} = TokenAdapter.next(strict_state)

    # Tolerant mode: errors are handled by synthesizing error tokens
    # Test with invalid input that produces a lexer error
    tolerant_state = TokenAdapter.new("\x00", mode: :tolerant)

    # In tolerant mode, lexer errors become error_token synthetic tokens
    assert {:ok, token, state_after} = TokenAdapter.next(tolerant_state)
    assert TokenAdapter.kind(token) == :error_token
    assert length(state_after.diagnostics) >= 1
  end

  test "fuel guard halts iteration when limit is reached" do
    state = TokenAdapter.new("1;2", fuel_limit: 1)

    assert {:ok, _tok, state} = TokenAdapter.next(state)
    assert {:error, :out_of_fuel, _state} = TokenAdapter.next(state)
  end
end
