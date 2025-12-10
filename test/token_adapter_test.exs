defmodule ToxicParser.TokenAdapterTest do
  use ExUnit.Case, async: true

  alias ToxicParser.TokenAdapter

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

    assert Enum.map(tokens, & &1.kind) == [:int, :eoe, :int]

    [%{metadata: meta}] = Enum.filter(tokens, &(&1.kind == :eoe))
    assert meta.newlines == 2
    assert meta.range.start.line == 1
    assert meta.range.end.line == 3
  end

  test "peek and peek_n do not consume tokens" do
    state = TokenAdapter.new("1;2", max_peek: 2)

    {:ok, first, state} = TokenAdapter.peek(state)
    assert first.kind == :int

    {:ok, tokens, state} = TokenAdapter.peek_n(state, 2)
    assert Enum.map(tokens, & &1.kind) == [:int, :eoe]

    {:ok, still_first, _state} = TokenAdapter.peek(state)
    assert still_first.kind == :int
  end

  test "checkpoint and rewind restore lookahead and diagnostics" do
    state = TokenAdapter.new("1\n\n2")

    {:ok, first, state} = TokenAdapter.next(state)
    assert first.kind == :int

    {ref, state} = TokenAdapter.checkpoint(state)
    {:ok, eoe, state} = TokenAdapter.next(state)
    assert eoe.kind == :eoe

    rewound = TokenAdapter.rewind(state, ref)
    {:ok, eoe_again, _} = TokenAdapter.next(rewound)
    assert eoe_again.kind == :eoe

    assert rewound.checkpoints == %{}
  end

  test "tolerant mode surfaces error_token and synthesized closers" do
    state = TokenAdapter.new(")", mode: :tolerant)

    {:ok, error_token, state} = TokenAdapter.next(state)
    assert error_token.kind == :error_token
    assert length(state.diagnostics) == 1

    {:ok, synthesized, _state} = TokenAdapter.next(state)
    assert synthesized.metadata.synthesized?
  end

  test "metadata offsets reflect line and column positions" do
    state = TokenAdapter.new("1;2")
    {:ok, token, _state} = TokenAdapter.next(state)

    assert token.metadata.range.start.offset == 0
    assert token.metadata.range.start.column == 1
    assert token.metadata.range.end.offset == 1
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

    tolerant_state = TokenAdapter.new("1", mode: :tolerant)
    tolerant_state = %{tolerant_state | stream: %{tolerant_state.stream | error: {:fatal, :boom}}}

    assert {:ok, token, state_after} = TokenAdapter.next(tolerant_state)
    assert token.kind == :error_token
    assert state_after.stream.error == nil
    assert length(state_after.diagnostics) == 1
  end
end
