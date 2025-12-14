defmodule ToxicParser.TupleKeywordMergeRegressionTest do
  use ExUnit.Case, async: true

  defp s2q(code) do
    Code.string_to_quoted(code, columns: true, token_metadata: true, emit_warnings: false)
  end

  defp toxic_parse(code) do
    case ToxicParser.parse_string(code, mode: :strict, token_metadata: true) do
      {:ok, result} -> {:ok, result.ast}
      {:error, result} -> {:error, result.diagnostics}
    end
  end

  test "merges multiple keyword pairs in 2-tuple" do
    code = "{1, foo: 1, bar: 2}"
    assert toxic_parse(code) == s2q(code)
  end

  test "merges keyword tail with quoted keys" do
    code = "{1, foo: 1, 'bar': 2}"

    st0 = ToxicParser.TokenAdapter.new(code, token_metadata: true, max_peek: 64)
    {:ok, _open, st1} = ToxicParser.TokenAdapter.next(st0)
    {:ok, _one, st2} = ToxicParser.TokenAdapter.next(st1)
    {:ok, _comma, st3} = ToxicParser.TokenAdapter.next(st2)

    assert {:ok, [foo: 1, bar: 2], _st4, _log} =
             ToxicParser.Grammar.Keywords.parse_kw_data(st3, :unmatched, ToxicParser.EventLog.new())

    assert toxic_parse(code) == s2q(code)

    code = "{1, 'foo': 1, 'bar': 2}"
    assert toxic_parse(code) == s2q(code)

    code = "{1, 'foo': 1, bar: 2}"
    assert toxic_parse(code) == s2q(code)
  end
end
