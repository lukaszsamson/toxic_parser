defmodule ToxicParser.Result do
  @moduledoc """
  Structured output for parser entry points.
  """

  alias ToxicParser.{Cursor, Error, EventLog, State}

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
          warnings: [ToxicParser.Warning.t() | Toxic.Warning.t()],
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
    warnings: [],
    metadata: %{}
  ]

  @type error_tuple ::
          {:error, term(), State.t()}
          | {:error, term(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}

  @spec normalize_error(error_tuple | term(), Cursor.t(), EventLog.t()) ::
          {:error, term(), State.t(), Cursor.t(), EventLog.t()} | term()
  def normalize_error({:error, diag, %State{} = state, cursor, %EventLog{} = log}, _, _),
    do: {:error, diag, state, cursor, log}

  def normalize_error({:error, diag, %State{} = state, %EventLog{} = log}, cursor, _),
    do: {:error, diag, state, cursor, log}

  def normalize_error({:error, diag, %State{} = state}, cursor, %EventLog{} = log),
    do: {:error, diag, state, cursor, log}

  # Handle 4-element error with cursor but no log (e.g., from expect_token)
  def normalize_error({:error, diag, %State{} = state, cursor}, _, %EventLog{} = log)
      when not is_struct(cursor, EventLog),
      do: {:error, diag, state, cursor, log}

  def normalize_error(other, _, _), do: other
end
