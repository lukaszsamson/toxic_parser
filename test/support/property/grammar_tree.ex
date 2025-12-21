defmodule ToxicParser.Property.GrammarTree do
  @moduledoc """
  Type definitions for grammar tree nodes.

  Grammar trees are intermediate representations that compile to Toxic tokens.
  Each node type corresponds to a syntactic construct in Elixir.
  """

  # ===========================================================================
  # Top-level types
  # ===========================================================================

  @typedoc "A complete grammar tree (program)"
  @type t ::
          {:grammar, [expr_t()]}
          | {:grammar_eoe, [{expr_t(), eoe_t()}]}
          | {:grammar_v2, eoe_t() | nil, [{expr_t(), eoe_t() | nil}], eoe_t() | nil}

  @typedoc "Any expression"
  @type expr_t :: t()

  @typedoc """
  End-of-expression marker (grammar rules 331-333).

  - `:eol` - newline only
  - `:semi` - semicolon only
  - `:eol_semi` - newline followed by semicolon
  """
  @type eoe_t :: :eol | :semi | :eol_semi

  @typedoc "Pattern expression (for fn arguments)"
  @type pattern_t :: :empty | {:single, expr_t()} | {:many, [expr_t()]}

  @typedoc "Guard expression"
  @type guard_t :: nil | expr_t()

  # ===========================================================================
  # Literals
  # ===========================================================================

  @typedoc """
  Integer literal.

  - `value`: the numeric value
  - `format`: `:dec`, `:hex`, `:bin`, or `:oct`
  - `chars`: the original charlist representation (e.g., `~c"123"`, `~c"0xFF"`)
  """
  @type int_t ::
          {:int, value :: integer(), format :: :dec | :hex | :bin | :oct, chars :: charlist()}

  @typedoc """
  Float literal.

  - `value`: the numeric value
  - `chars`: the original charlist representation (e.g., `~c"1.0"`, `~c"1.0e-10"`)
  """
  @type float_t :: {:float, value :: float(), chars :: charlist()}

  @typedoc """
  Character literal.

  - `codepoint`: the character codepoint
  - `chars`: the original charlist representation (e.g., `~c"?a"`, `~c"?\\n"`)
  """
  @type char_t :: {:char, codepoint :: integer(), chars :: charlist()}

  @typedoc "Atom literal (unquoted)"
  @type atom_lit_t :: {:atom_lit, atom()}

  @typedoc "Boolean literal"
  @type bool_lit_t :: {:bool_lit, true | false}

  @typedoc "Nil literal"
  @type nil_lit_t :: :nil_lit

  @typedoc "All literal types"
  @type literal_t :: int_t() | float_t() | char_t() | atom_lit_t() | bool_lit_t() | nil_lit_t()

  # ===========================================================================
  # Identifiers and Aliases
  # ===========================================================================

  @typedoc "Simple identifier (e.g., `foo`)"
  @type identifier_t :: {:identifier, atom()}

  @typedoc "Alias (e.g., `Foo`, `MyApp.Context`)"
  @type alias_t :: {:alias, atom()}

  @typedoc "Paren identifier - identifier immediately followed by `(` (e.g., `foo(`)"
  @type paren_identifier_t :: {:paren_identifier, atom()}

  @typedoc "Bracket identifier - identifier immediately followed by `[` (e.g., `foo[`)"
  @type bracket_identifier_t :: {:bracket_identifier, atom()}

  @typedoc "Do identifier - identifier followed by `do` (e.g., `if`, `case`)"
  @type do_identifier_t :: {:do_identifier, atom()}

  @typedoc "Op identifier - identifier in operator position"
  @type op_identifier_t :: {:op_identifier, atom()}

  @typedoc "All identifier types"
  @type any_identifier_t ::
          identifier_t()
          | alias_t()
          | paren_identifier_t()
          | bracket_identifier_t()
          | do_identifier_t()
          | op_identifier_t()

  # ===========================================================================
  # Operators
  # ===========================================================================

  @typedoc """
  Operator kind and value.

  - First element: operator category (`:dual_op`, `:mult_op`, etc.)
  - Second element: the operator atom (`:+`, `:*`, etc.)
  """
  @type op_kind :: {atom(), atom()}

  @typedoc """
  Operator with optional trailing newlines.

  - `op_kind`: the operator category and value
  - `newlines`: number of newlines after the operator (0 = no newline)
  """
  @type op_eol_t :: {:op_eol, op_kind(), non_neg_integer()}

  @typedoc """
  Matched binary operator expression (both operands are matched expressions).

  Used when the operator appears in a context where unmatched expressions
  would be invalid (e.g., as operands of another operator).

  - `left`: left operand (matched expression)
  - `op`: operator with optional newlines
  - `right`: right operand (matched expression)
  """
  @type matched_op_t :: {:matched_op, matched_expr_t(), op_eol_t(), matched_expr_t()}

  @typedoc """
  Unmatched binary operator expression (right operand is unmatched).

  Used when an operator has a do-block-bearing expression as its right operand.
  Per grammar, the left operand must be matched, but the right can be unmatched.

  - `left`: left operand (matched expression)
  - `op`: operator with optional newlines
  - `right`: right operand (unmatched expression)
  """
  @type unmatched_op_t :: {:unmatched_op, matched_expr_t(), op_eol_t(), unmatched_expr_t()}

  @typedoc """
  Legacy binary operator expression (deprecated, use matched_op_t).

  Kept for backward compatibility during migration.
  """
  @type binary_op_t :: {:binary_op, expr_t(), op_eol_t(), expr_t()}

  @typedoc """
  Matched unary operator expression.

  Per grammar: matched_expr -> unary_op_eol matched_expr
  unary_op_eol -> unary_op | unary_op eol

  - `op`: operator kind
  - `newlines`: number of newlines after operator (0 = no newline)
  - `operand`: the operand expression (matched)
  """
  @type matched_unary_t :: {:matched_unary, op_kind(), non_neg_integer(), matched_expr_t()}

  @typedoc """
  Legacy unary operator expression (deprecated, use matched_unary_t).
  """
  @type unary_op_t :: {:unary_op, op_kind(), expr_t()}

  @typedoc "Nullary range operator (..)"
  @type nullary_range_t :: {:nullary_range, nil}

  @typedoc "Nullary ellipsis operator (...)"
  @type nullary_ellipsis_t :: {:nullary_ellipsis, nil}

  @typedoc """
  At operator expression (@expr for module attributes).

  Per grammar: matched_expr -> at_op_eol matched_expr
  - `newlines`: number of newlines after `@` (0 = no newline)
  - `operand`: the matched expression following `@`
  """
  @type at_op_t :: {:at_op, non_neg_integer(), matched_expr_t()}

  @typedoc """
  Capture operator expression (&expr).

  Per grammar: matched_expr -> capture_op_eol matched_expr
  - `newlines`: number of newlines after `&` (0 = no newline)
  - `operand`: the matched expression following `&`
  """
  @type capture_op_t :: {:capture_op, non_neg_integer(), matched_expr_t()}

  @typedoc "Ellipsis prefix operator (...expr)"
  @type ellipsis_prefix_t :: {:ellipsis_prefix, matched_expr_t()}

  # ===========================================================================
  # Expression Categories (per elixir_parser.yrl)
  # ===========================================================================

  @typedoc """
  Matched expression - safe to use as operands.

  Per grammar lines 155-161, matched expressions include:
  - Binary ops with matched operands
  - Unary ops with matched operands
  - no_parens_one_expr
  - sub_matched_expr (access_expr, nullary ops)
  """
  @type matched_expr_t ::
          matched_op_t()
          | matched_unary_t()
          | call_no_parens_one_t()
          | sub_matched_expr_t()
          # Legacy types for backward compatibility
          | binary_op_t()
          | unary_op_t()

  @typedoc """
  Unmatched expression - has trailing do block.

  Per grammar lines 163-171, unmatched expressions include:
  - Do-block bearing calls (if, case, fn_multi, etc.)
  - Binary ops with unmatched right operand
  """
  @type unmatched_expr_t ::
          call_do_t()
          | unmatched_op_t()

  @typedoc """
  Sub-matched expression - atomic/access expressions.

  Per grammar lines 263-267, includes:
  - access_expr (literals, identifiers, fn, calls, etc.)
  - Nullary range_op (..)
  - Nullary ellipsis_op (...)
  """
  @type sub_matched_expr_t ::
          access_expr_t()
          | nullary_range_t()
          | nullary_ellipsis_t()

  @typedoc """
  Access expression - the leaf nodes.

  Per grammar lines 273-301, includes:
  - Literals (int, float, char, atom, bool, nil)
  - Identifiers and aliases
  - fn expressions
  - Parenthesized calls
  - Captures
  - Parenthesized expressions
  """
  @type access_expr_t ::
          literal_t()
          | identifier_t()
          | alias_t()
          | fn_single_t()
          | fn_multi_t()
          | call_parens_t()
          | capture_int_t()
          | paren_expr_t()
          | empty_paren_t()

  @typedoc "Parenthesized expression (e.g., `(1 + 2)`)"
  @type paren_expr_t :: {:paren_expr, expr_t()}

  @typedoc "Empty parentheses (e.g., `()`)"
  @type empty_paren_t :: {:empty_paren, nil}

  # ===========================================================================
  # Calls and Captures
  # ===========================================================================

  @typedoc "Target of a call (identifier, paren_identifier, or dot expression)"
  @type target_t ::
          identifier_t()
          | paren_identifier_t()
          | {:dot, expr_t(), identifier_t() | op_identifier_t()}
          | {:dot_call, expr_t()}

  @typedoc """
  Parenthesized call expression (e.g., `foo(1, 2)` or `foo.(1)`).

  - `target`: the call target
  - `args`: list of arguments
  """
  @type call_parens_t :: {:call_parens, target_t(), [expr_t()]}

  @typedoc """
  No-parens call with one argument (e.g., `foo bar`).
  """
  @type call_no_parens_one_t :: {:call_no_parens_one, target_t(), expr_t()}

  @typedoc """
  Dot-call expression (e.g., `foo.(1)`).

  The expression is the target to call.
  """
  @type dot_call_t :: {:dot_call, expr_t()}

  @typedoc """
  Capture integer (e.g., `&1`, `&10`).

  Must be a positive integer.
  """
  @type capture_int_t :: {:capture_int, pos_integer()}

  # ===========================================================================
  # Functions (fn_single)
  # ===========================================================================

  @typedoc """
  Stab clause for fn expressions.

  - `pattern`: the pattern (`:empty`, `{:single, expr}`, or `{:many, [expr]}`)
  - `guard`: guard expression
  - `body`: the body expression
  """
  @type stab_clause_t :: {:stab_clause, pattern_t(), guard_t(), expr_t()}

  @typedoc """
  Single-clause fn expression.

  only one clause, no guards, simple patterns (`:empty` or `{:single, expr}`).
  """
  @type fn_single_t :: {:fn_single, [stab_clause_t()]}

  # ===========================================================================
  # Later Phases (placeholders)
  # ===========================================================================

  @typedoc "Multi-clause fn expression"
  @type fn_multi_t :: {:fn_multi, [stab_clause_t()]}

  @typedoc "Do block"
  @type do_block_t :: {:do_block, [stab_clause_t()] | [expr_t()], [block_item_t()]}

  @typedoc """
  Call with do-block (e.g., `if x do y end`).

  - `target`: the call target identifier
  - `args`: list of arguments before the do block
  - `do_block`: the do block itself
  """
  @type call_do_t :: {:call_do, identifier_t(), [expr_t()], do_block_t()}

  @typedoc "Block item (else, rescue, catch, after)"
  @type block_item_t ::
          {:block_item, :after | :else | :catch | :rescue, [stab_clause_t()] | [expr_t()]}

  @typedoc "List container"
  @type list_t :: {:list, [expr_t()]}

  @typedoc "Tuple container"
  @type tuple_t :: {:tuple, [expr_t()]}

  @typedoc "Map container"
  @type map_t :: {:map, [assoc_t()]}

  @typedoc "Association (key => value or key: value)"
  @type assoc_t :: {:assoc, expr_t(), expr_t()} | {:kw, atom(), expr_t()}

  @typedoc "String"
  @type string_t :: {:bin_string, [string_part_t()]}

  @typedoc "String part (fragment or interpolation)"
  @type string_part_t :: {:fragment, binary()} | {:interpolation, [expr_t()]}

  # ===========================================================================
  # Context and Budget
  # ===========================================================================

  @typedoc """
  Generation context flags.

  Controls what constructs are allowed during generation.
  """
  @type context :: %{
          in_do_block: boolean(),
          in_no_parens_many: boolean(),
          in_keyword_value: boolean(),
          in_parens_call_arg: boolean(),
          allow_unmatched: boolean(),
          allow_no_parens: boolean(),
          allow_do_block: boolean(),
          allow_no_parens_many: boolean(),
          allow_ternary_after_range: boolean(),
          interpolation_depth: non_neg_integer()
        }

  @typedoc """
  Generation budget to control tree size and depth.
  """
  @type budget :: %{
          depth: non_neg_integer(),
          nodes_left: non_neg_integer()
        }

  @typedoc """
  Generator state combining budget and context.
  """
  @type state :: %{
          budget: budget(),
          context: context()
        }

  # ===========================================================================
  # Helpers
  # ===========================================================================

  @doc "Create initial generation context for"
  @spec new_context() :: context()
  def new_context do
    %{
      in_do_block: false,
      in_no_parens_many: false,
      in_keyword_value: false,
      in_parens_call_arg: false,
      allow_unmatched: false,
      allow_no_parens: false,
      allow_do_block: false,
      allow_no_parens_many: false,
      allow_ternary_after_range: false,
      interpolation_depth: 0
    }
  end

  @doc "Create initial generation budget"
  @spec initial_budget(non_neg_integer(), non_neg_integer()) :: budget()
  def initial_budget(depth \\ 4, nodes_left \\ 100) do
    %{depth: depth, nodes_left: nodes_left}
  end

  @doc "Create initial generator state"
  @spec initial_state(non_neg_integer(), non_neg_integer()) :: state()
  def initial_state(depth \\ 4, nodes_left \\ 100) do
    %{
      budget: initial_budget(depth, nodes_left),
      context: new_context()
    }
  end

  @doc "Decrement depth in budget"
  @spec decr_depth(state()) :: state()
  def decr_depth(%{budget: budget} = state) do
    %{state | budget: %{budget | depth: max(0, budget.depth - 1)}}
  end

  @doc "Decrement nodes_left in budget"
  @spec decr_nodes(state(), non_neg_integer()) :: state()
  def decr_nodes(%{budget: budget} = state, n \\ 1) do
    %{state | budget: %{budget | nodes_left: max(0, budget.nodes_left - n)}}
  end

  @doc "Check if budget is exhausted (depth or nodes)"
  @spec budget_exhausted?(state()) :: boolean()
  def budget_exhausted?(%{budget: %{depth: depth, nodes_left: nodes_left}}) do
    depth <= 0 or nodes_left <= 0
  end
end
