defmodule ToxicParser.EnvTest do
  use ExUnit.Case, async: true

  alias ToxicParser.{Env, EventLog}

  test "builds bindings from env events" do
    meta = %{
      range: %{start: %{line: 1, offset: 0, column: 1}, end: %{line: 1, offset: 0, column: 1}},
      delimiter: nil,
      newlines: 0,
      synthesized?: false,
      terminators: [],
      role: :none
    }

    events =
      EventLog.new()
      |> EventLog.env(%{action: :enter_scope, scope: :fn, name: nil}, meta)
      |> EventLog.env(%{action: :bind, scope: nil, name: :x}, meta)
      |> EventLog.env(%{action: :exit_scope, scope: :fn, name: nil}, meta)
      |> EventLog.to_list()

    env = Env.from_events(events)
    assert Env.bindings(env) == [:x]
  end
end
