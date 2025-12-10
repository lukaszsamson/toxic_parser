defmodule ToxicParser.Env do
  @moduledoc """
  Minimal environment accumulator that consumes `:env` events from the parser
  and tracks bindings per scope. This is a stub for Phase 7 to make emitted
  environment events usable by tooling.
  """

  alias ToxicParser.EventLog

  defstruct scopes: [[]]

  @type t :: %__MODULE__{scopes: [list(atom())]}

  @doc "Builds an environment from an event list."
  @spec from_events([EventLog.event()]) :: t()
  def from_events(events) when is_list(events) do
    Enum.reduce(events, %__MODULE__{}, fn
      {:env, %{action: :enter_scope}, _meta}, env ->
        %{env | scopes: [[] | env.scopes]}

      {:env, %{action: :exit_scope}, _meta}, %__MODULE__{scopes: [top | rest]} = env ->
        merged =
          case rest do
            [parent | tail] -> [[top | parent] | tail]
            [] -> [top]
          end

        %{env | scopes: merged}

      {:env, %{action: :bind, name: name}, _meta}, %__MODULE__{scopes: [scope | rest]} = env ->
        %{env | scopes: [[name | scope] | rest]}

      _other, env ->
        env
    end)
  end

  @doc "Returns all currently bound names, innermost scope first."
  @spec bindings(t()) :: [atom()]
  def bindings(%__MODULE__{scopes: scopes}) do
    scopes |> List.flatten()
  end
end
