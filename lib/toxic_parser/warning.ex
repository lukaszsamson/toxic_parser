defmodule ToxicParser.Warning do
  @moduledoc """
  Parser warning payload for ambiguity and style diagnostics (e.g. warn_pipe).
  """

  alias ToxicParser.EventLog

  @type t :: %__MODULE__{
          code: atom(),
          message: String.t(),
          range: EventLog.range(),
          details: map()
        }

  defstruct [:code, :message, :range, details: %{}]
end
