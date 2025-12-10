defmodule ToxicParser.EventLog do
  @moduledoc """
  Event stream contract and helpers for emitting well-ordered parse events.

  Ordering guarantees:
  - Events are emitted in source order.
  - `:start_node`/`:end_node` entries are strictly well-nested; every `:start_node`
    has a corresponding `:end_node` before its parent's `:end_node`.
  - `:missing`/`:synthetic`/`:error` events appear at the position the grammar
    expected the construct or where the lexer reported the problem.
  - `:comment` events precede the node/token they attach to.
  """

  alias ToxicParser.Error

  defstruct events: [], stack: []

  @typedoc "Cursor position in the source."
  @type location :: %{
          offset: non_neg_integer(),
          line: pos_integer(),
          column: non_neg_integer()
        }

  @typedoc "Byte/line span for a token or node."
  @type range :: %{start: location(), end: location()}

  @type delimiter_kind :: :paren | :bracket | :curly | :bitstring | :sigil

  @typedoc "Standardized metadata shared by all events."
  @type metadata :: %{
          range: range(),
          delimiter:
            nil
            | {delimiter_kind(), {open :: String.t(), close :: String.t()}},
          newlines: non_neg_integer(),
          synthesized?: boolean(),
          terminators: [atom()],
          role: :open | :close | :none
        }

  @typedoc "Logical node kinds used across CST builders."
  @type node_kind ::
          :root
          | :expr
          | :matched_expr
          | :unmatched_expr
          | :no_parens_expr
          | :call
          | :list
          | :tuple
          | :map
          | :bitstring
          | :fn
          | :do_block
          | :stab
          | :access
          | :dot
          | :unary_op
          | :binary_op
          | :string
          | :sigil
          | :alias
          | :identifier
          | :keyword
          | :container_args
          | :block
          | :clause
          | :pattern
          | :interpolation
          | :comment_block

  @typedoc "Token payload from the lexer or synthesized by recovery."
  @type token_payload :: %{
          kind: atom(),
          value: term()
        }

  @typedoc "Missing payload identifying the expected construct."
  @type missing_payload :: %{expected: atom()}

  @typedoc "Synthetic payload describing the synthesized artifact."
  @type synthetic_payload :: %{kind: :delimiter | :token, value: term()}

  @typedoc "Comment payload attached to following tokens/nodes."
  @type comment_payload :: %{text: String.t(), inline?: boolean()}

  @typedoc "Unified event representation."
  @type event ::
          {:start_node, node_kind(), metadata()}
          | {:end_node, node_kind(), metadata()}
          | {:token, token_payload(), metadata()}
          | {:error, error_payload(), metadata()}
          | {:missing, missing_payload(), metadata()}
          | {:synthetic, synthetic_payload(), metadata()}
          | {:comment, comment_payload(), metadata()}

  @typedoc "Error payload stored in the event stream."
  @type error_payload :: %{
          phase: Error.phase(),
          reason: term(),
          severity: Error.severity(),
          token: atom() | nil,
          expected: [atom()] | nil
        }

  @type t :: %__MODULE__{events: [event()], stack: [node_kind()]}

  @doc "Returns an empty, ordered event log."
  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc "Returns events in emission order."
  @spec to_list(t()) :: [event()]
  def to_list(%__MODULE__{events: evts}), do: Enum.reverse(evts)

  @doc "Starts a node and pushes it onto the nesting stack."
  @spec start_node(t(), node_kind(), metadata()) :: t()
  def start_node(%__MODULE__{} = log, kind, metadata) do
    %{log | events: [{:start_node, kind, metadata} | log.events], stack: [kind | log.stack]}
  end

  @doc "Ends the current node; raises if the stack is not balanced."
  @spec end_node(t(), node_kind(), metadata()) :: t()
  def end_node(%__MODULE__{stack: [kind | rest]} = log, kind, metadata) do
    %{log | events: [{:end_node, kind, metadata} | log.events], stack: rest}
  end

  def end_node(%__MODULE__{stack: [top | _]}, kind, _metadata) do
    raise ArgumentError, "Mismatched end_node: expected #{inspect(top)}, got #{inspect(kind)}"
  end

  def end_node(%__MODULE__{stack: []}, kind, _metadata) do
    raise ArgumentError, "Unbalanced end_node with empty stack for #{inspect(kind)}"
  end

  @doc "Appends a token event."
  @spec token(t(), token_payload(), metadata()) :: t()
  def token(%__MODULE__{} = log, payload, metadata) do
    %{log | events: [{:token, payload, metadata} | log.events]}
  end

  @doc "Appends an error event."
  @spec error(t(), error_payload(), metadata()) :: t()
  def error(%__MODULE__{} = log, payload, metadata) do
    %{log | events: [{:error, payload, metadata} | log.events]}
  end

  @doc "Appends a missing-token event."
  @spec missing(t(), missing_payload(), metadata()) :: t()
  def missing(%__MODULE__{} = log, payload, metadata) do
    %{log | events: [{:missing, payload, metadata} | log.events]}
  end

  @doc "Appends a synthetic-token event."
  @spec synthetic(t(), synthetic_payload(), metadata()) :: t()
  def synthetic(%__MODULE__{} = log, payload, metadata) do
    %{log | events: [{:synthetic, payload, metadata} | log.events]}
  end

  @doc "Appends a comment event."
  @spec comment(t(), comment_payload(), metadata()) :: t()
  def comment(%__MODULE__{} = log, payload, metadata) do
    %{log | events: [{:comment, payload, metadata} | log.events]}
  end
end
