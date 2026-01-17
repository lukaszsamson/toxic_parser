defmodule ToxicParser.ParseOpts do
  @moduledoc """
  Struct for parsing options passed through nud/led in the Pratt parser.

  Using a struct instead of a keyword list provides O(1) field access
  instead of O(n) keyword list traversal.
  """

  @type t :: %__MODULE__{
          min_bp: non_neg_integer(),
          allow_containers: boolean(),
          string_min_bp: non_neg_integer(),
          unary_operand: boolean(),
          allow_no_parens_extension?: boolean(),
          allows_bracket: boolean(),
          stop_at_assoc: boolean(),
          stop_at_comma: boolean(),
          emit_warnings?: boolean()
        }

  defstruct min_bp: 0,
            allow_containers: true,
            string_min_bp: 0,
            unary_operand: false,
            allow_no_parens_extension?: true,
            allows_bracket: true,
            stop_at_assoc: false,
            stop_at_comma: false,
            emit_warnings?: false

  @doc """
  Returns ParseOpts with stop_at_assoc: true.
  """
  @spec stop_at_assoc() :: t()
  def stop_at_assoc, do: %__MODULE__{stop_at_assoc: true}
end
