defmodule ToxicParser.Builder do
  @moduledoc """
  Behaviour for event-log-driven builders (AST/CST/outline/environment).
  """

  alias ToxicParser.EventLog

  @type error_node :: {:__error__, map(), map()}

  @callback init(keyword()) :: any()
  @callback handle_event(EventLog.event(), any()) :: any()
  @callback finalize(any()) :: term()

  @doc """
  Streams events through a builder implementation.
  """
  @spec reduce([EventLog.event()], module(), keyword()) :: term()
  def reduce(events, builder_mod, opts \\ []) do
    state = builder_mod.init(opts)

    events
    |> Enum.reduce(state, fn event, acc -> builder_mod.handle_event(event, acc) end)
    |> builder_mod.finalize()
  end

  @doc """
  Helper to build the locked error node shape.
  """
  @spec error_node(map(), map()) :: error_node()
  def error_node(meta, payload) when is_map(meta) and is_map(payload) do
    {:__error__, meta, payload}
  end
end
