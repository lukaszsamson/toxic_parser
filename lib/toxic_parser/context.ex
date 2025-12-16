defmodule ToxicParser.Context do
  @moduledoc """
  Expression context for Pratt parser.

  Encapsulates two orthogonal axes from the grammar (elixir_parser.yrl):
  - allow_do_block: Whether do...end blocks can attach (unmatched_expr capability)
  - allow_no_parens_expr: Whether operators can extend matched_expr to no_parens_expr

  ## Grammar Union Mapping

  | Grammar Union        | Constructor          | allow_do_block | allow_no_parens_expr |
  |---------------------|----------------------|----------------|----------------------|
  | expr                | expr()               | true           | true                 |
  | container_expr      | container_expr()     | true           | false                |
  | matched_expr        | matched_expr()       | false          | false                |
  | unmatched_expr      | unmatched_expr()     | true           | false                |
  | no_parens_expr      | no_parens_expr()     | false          | true                 |
  | kw value (no-parens)| kw_no_parens_value() | false          | true                 |

  ## Key Grammar Rules

  The rule `no_parens_expr -> matched_expr no_parens_op_expr` means that after parsing
  a matched_expr, operators in no_parens_op_expr can extend it when allow_no_parens_expr
  is true, regardless of outer binding power constraints.
  """

  @type t :: %__MODULE__{
          allow_do_block: boolean(),
          allow_no_parens_expr: boolean()
        }

  defstruct allow_do_block: true, allow_no_parens_expr: true

  @doc """
  expr = matched | no_parens | unmatched (full expression context)

  Used for top-level expressions, stab bodies, and unary operator operands.
  """
  @spec expr() :: t()
  def expr, do: %__MODULE__{allow_do_block: true, allow_no_parens_expr: true}

  @doc """
  container_expr = matched | unmatched (do-blocks allowed, no_parens rejected)

  Used in containers (lists, tuples, maps, bitstrings) where no_parens_expr
  would cause ambiguity. The grammar rejects no_parens_expr in these contexts.
  """
  @spec container_expr() :: t()
  def container_expr, do: %__MODULE__{allow_do_block: true, allow_no_parens_expr: false}

  @doc """
  matched_expr alone (no do-blocks, no no_parens extension)

  Used for operands that bind tightly: literals, parenthesized expressions,
  binary operator operands where precedence must be strictly maintained.
  """
  @spec matched_expr() :: t()
  def matched_expr, do: %__MODULE__{allow_do_block: false, allow_no_parens_expr: false}

  @doc """
  unmatched_expr alone (do-blocks allowed, no no_parens extension)

  Used for contexts that can contain do-blocks but should not allow
  ambiguous no-parens expressions.
  """
  @spec unmatched_expr() :: t()
  def unmatched_expr, do: %__MODULE__{allow_do_block: true, allow_no_parens_expr: false}

  @doc """
  no_parens_expr alone (no do-blocks, allows no_parens extension)

  Used for no-parens call arguments where do-blocks belong to the outer call.
  """
  @spec no_parens_expr() :: t()
  def no_parens_expr, do: %__MODULE__{allow_do_block: false, allow_no_parens_expr: true}

  @doc """
  kw_no_parens_value (for keyword values in no-parens calls)

  Allows no_parens extension but not do-blocks. Used when parsing values
  in keyword lists that appear as no-parens call arguments.
  """
  @spec kw_no_parens_value() :: t()
  def kw_no_parens_value, do: %__MODULE__{allow_do_block: false, allow_no_parens_expr: true}

  @doc """
  Convert legacy atom context to Context struct.

  For backward compatibility during migration.
  """
  @spec from_atom(:matched | :unmatched | :no_parens) :: t()
  def from_atom(:matched), do: matched_expr()
  def from_atom(:unmatched), do: unmatched_expr()
  def from_atom(:no_parens), do: no_parens_expr()
  def from_atom(_), do: matched_expr()
end
