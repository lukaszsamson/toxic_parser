defmodule ToxicParser.Result do
  @moduledoc """
  Structured output for parser entry points.
  """

  alias ToxicParser.{Error, EventLog, State}

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

  @type error_tuple ::
          {:error, term(), State.t()}
          | {:error, term(), State.t(), EventLog.t()}

  @spec normalize_error(error_tuple | term(), EventLog.t()) ::
          {:error, term(), State.t(), EventLog.t()} | term()
  def normalize_error({:error, diag, %State{} = state, %EventLog{} = log}, _),
    do: {:error, diag, state, log}

  def normalize_error({:error, diag, %State{} = state}, %EventLog{} = log),
    do: {:error, diag, state, log}

  def normalize_error(other, _), do: other
end
