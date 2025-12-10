defmodule ToxicParser.EventLogTest do
  use ExUnit.Case, async: true

  alias ToxicParser.EventLog

  test "enforces well-nested start/end nodes" do
    log =
      EventLog.new()
      |> EventLog.start_node(:root, meta())
      |> EventLog.start_node(:expr, meta())
      |> EventLog.end_node(:expr, meta())
      |> EventLog.end_node(:root, meta())

    assert [
             {:start_node, :root, _},
             {:start_node, :expr, _},
             {:end_node, :expr, _},
             {:end_node, :root, _}
           ] = EventLog.to_list(log)
  end

  test "raises on mismatched end_node" do
    log = EventLog.new() |> EventLog.start_node(:root, meta())

    assert_raise ArgumentError, fn ->
      EventLog.end_node(log, :expr, meta())
    end
  end

  test "appends token/error/missing/synthetic/comment events" do
    log =
      EventLog.new()
      |> EventLog.token(%{kind: :int, value: 1}, meta())
      |> EventLog.error(
        %{phase: :parser, reason: :oops, severity: :error, token: nil, expected: []},
        meta()
      )
      |> EventLog.missing(%{expected: :")"}, meta())
      |> EventLog.synthetic(%{kind: :delimiter, value: :")"}, meta())
      |> EventLog.comment(%{text: "# c", inline?: false}, meta())

    kinds = Enum.map(EventLog.to_list(log), fn {kind, _, _} -> kind end)
    assert kinds == [:token, :error, :missing, :synthetic, :comment]
  end

  defp meta do
    %{
      range: %{start: %{offset: 0, line: 1, column: 1}, end: %{offset: 0, line: 1, column: 1}},
      delimiter: nil,
      newlines: 0,
      synthesized?: false,
      terminators: [],
      role: :none
    }
  end
end
