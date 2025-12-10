defmodule ToxicParser.EventLog do
  @moduledoc """
  Type definitions and documentation for the parser event stream contract.

  Events are emitted in source order. `:start_node`/`:end_node` entries are
  always well-nested. `:missing` and `:synthetic` events are inserted where
  the grammar expected constructs that were absent in the token stream.
  """

  alias ToxicParser.Error

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

  @doc "Returns an empty, ordered event log."
  @spec new :: [event()]
  def new, do: []
end
