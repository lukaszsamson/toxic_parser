defmodule ToxicParser.Result do
  @moduledoc """
  Structured output for parser entry points.
  """

  alias ToxicParser.{Error, EventLog}

  @type mode :: :strict | :tolerant

  @type t :: %__MODULE__{
          ast: Macro.t() | nil,
          mode: mode(),
          file: String.t() | nil,
          source: String.t() | nil,
          comments: list(),
          events: [EventLog.event()],
          env: term(),
          diagnostics: [Error.t()],
          metadata: map()
        }

  @enforce_keys [:mode]
  defstruct [
    :ast,
    :mode,
    :file,
    :source,
    comments: [],
    events: [],
    env: [],
    diagnostics: [],
    metadata: %{}
  ]
end
