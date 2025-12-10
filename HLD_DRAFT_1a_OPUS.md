# High-Level Design: Error-Tolerant Elixir Parser

## Executive Summary

This document presents a high-level design for an error-tolerant Elixir parser targeting IDE integration, Language Server Protocol (LSP) implementations, and developer tooling such as type checkers. The design draws from lessons learned in modern parser implementations—particularly Ruby's Prism/YARP and Python's Ruff—while addressing Elixir's unique grammatical challenges.

**Key Design Decisions:**

- Hand-written Pratt parser with recursive descent components
- Streaming integration with Toxic tokenizer
- Multi-strategy error recovery with AST completeness guarantees
- Elixir-compatible AST output with extended metadata

---

## 1. Background and Motivation

### 1.1 Why Hand-Written Over Generated Parsers

Modern language tooling has converged on hand-written parsers over parser generators (LALR, GLR, PEG) for several compelling reasons:

1. **Error Recovery Control**: Generated parsers treat errors as exceptional. Hand-written parsers can encode recovery strategies at every decision point.

2. **Context Sensitivity**: Elixir's grammar, like Ruby's, is context-sensitive. The meaning of tokens depends on surrounding context (e.g., `do:` keyword vs `do` block, unary vs binary operators based on whitespace).

3. **Performance**: Hand-written parsers enable domain-specific optimizations. Ruff achieved 2x+ speedups moving from LALRPOP to hand-written parsing.

4. **Maintainability**: Despite intuition, hand-written parsers are often easier to maintain—changes are localized to specific functions rather than grammar files with non-obvious interactions.

5. **Industry Consensus**: 7 of the top 10 programming languages use hand-written recursive descent parsers. Prism's success with Ruby validates this approach for a language with similar complexity to Elixir.

### 1.2 Why Pratt Parsing for Elixir

Elixir has 30+ operators across 20+ precedence levels (see `elixir_parser.yrl` lines 69-97). A pure recursive descent parser would require one function per precedence level for expression parsing. Pratt parsing elegantly handles:

- Operator precedence and associativity via binding powers
- Prefix, infix, and postfix operators uniformly
- Dynamic precedence adjustments (e.g., `not in` compound operator)
- Easy addition of new operators without restructuring

The hybrid approach—Pratt parsing for expressions, recursive descent for statements and structures—combines the strengths of both techniques.

---

## 2. Architecture Overview

```text
┌─────────────────────────────────────────────────────────────────────────┐
│                          Parser Public API                               │
│   parse/1  parse_expression/1  parse_pattern/1  parse_guard/1           │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                         Parser Core Engine                               │
│  ┌───────────────┐  ┌──────────────────┐  ┌─────────────────────────┐  │
│  │ Pratt Engine  │  │ Recursive Descent │  │ Context Manager         │  │
│  │ (expressions) │  │ (structures)      │  │ (terminator stack,      │  │
│  │               │  │                   │  │  mode tracking)         │  │
│  └───────────────┘  └──────────────────┘  └─────────────────────────┘  │
│                                                                          │
│  ┌───────────────────────────────────────────────────────────────────┐  │
│  │                    Error Recovery Subsystem                        │  │
│  │  • Synchronization point detection                                 │  │
│  │  • Missing token synthesis                                         │  │
│  │  • Panic mode with intelligent resync                              │  │
│  └───────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                         Token Stream Adapter                             │
│   Wraps Toxic tokenizer with:                                           │
│   • Lookahead buffer (configurable depth)                               │
│   • Checkpoint/rewind for speculative parsing                           │
│   • Token classification helpers                                         │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                         Toxic Tokenizer                                  │
│   • Streaming token emission                                             │
│   • Tolerant mode with error tokens                                      │
│   • Linearized interpolation output                                      │
│   • Ranged position metadata                                             │
└─────────────────────────────────────────────────────────────────────────┘
```

### 2.1 Component Responsibilities

| Component | Responsibility |
|-----------|----------------|
| **Parser API** | Public entry points, configuration, result packaging |
| **Pratt Engine** | Expression parsing with operator precedence |
| **Recursive Descent** | Statements, blocks, patterns, special forms |
| **Context Manager** | Track parsing mode, terminator expectations, variable scope |
| **Error Recovery** | Detect errors, synchronize, synthesize missing nodes |
| **Token Adapter** | Buffer management, lookahead, checkpoint/rewind |
| **Toxic** | Lexical analysis, interpolation linearization |

---

## 3. Detailed Design

### 3.1 Parser State

```elixir
defmodule Parser do
  defstruct [
    # Token stream (Toxic stream wrapper)
    tokens: nil,

    # Current token and lookahead
    current: nil,
    peek: nil,

    # Accumulated errors (parsing continues)
    errors: [],

    # Terminator stack for delimiter matching
    # [{:paren, meta}, {:do_block, meta}, ...]
    terminators: [],

    # Parsing context flags
    context: %{
      in_match: false,        # Left of =
      in_guard: false,        # After when
      in_pattern: false,      # Pattern position
      in_type: false,         # Type annotation
      no_parens_allowed: false # Ambiguous call context
    },

    # For variable tracking (like Prism's local table)
    locals: MapSet.new(),

    # Fuel for infinite loop prevention
    fuel: 1000,

    # Source metadata for error reporting
    file: "",
    source: nil
  ]
end
```

### 3.2 Pratt Parser Core

The Pratt parser uses binding powers to handle precedence. Each operator has a left binding power (LBP) and optionally a right binding power (RBP) for associativity.

#### 3.2.1 Precedence Table (derived from elixir_parser.yrl)

```elixir
# Binding powers (higher = tighter binding)
# Format: {left_bp, right_bp} where right_bp > left_bp for right-associative

@precedences %{
  # Block delimiters (lowest)
  do: {5, 6},

  # Stab operator (right-associative)
  :->: {10, 11},

  # Comma
  :",": {20, 21},

  # Match operators (left-associative in matches)
  :<-: {40, 41},
  :\\: {40, 41},

  # When (right-associative)
  :when: {50, 51},

  # Type operator (right-associative)
  :"::": {60, 61},

  # Pipe (right-associative)
  :|: {70, 71},

  # Assoc (right-associative)
  :"=>": {80, 81},

  # Capture/ellipsis (non-associative)
  :&: {90, 90},
  :...: {90, 90},

  # Match (right-associative)
  :=: {100, 101},

  # Boolean OR (left-associative)
  :||: {120, 121},
  :or: {120, 121},

  # Boolean AND (left-associative)
  :&&: {130, 131},
  :and: {130, 131},

  # Comparison (left-associative)
  :==: {140, 141},
  :!=: {140, 141},
  :=~: {140, 141},
  :===: {140, 141},
  :!==: {140, 141},

  # Relational (left-associative)
  :<: {150, 151},
  :>: {150, 151},
  :<=: {150, 151},
  :>=: {150, 151},

  # Arrow/pipe operators (left-associative)
  :|>: {160, 161},
  :~>: {160, 161},
  :<~: {160, 161},
  :<<<: {160, 161},
  :>>>: {160, 161},

  # In operator (left-associative)
  :in: {170, 171},
  :"not in": {170, 171},

  # XOR (left-associative)
  :^^^: {180, 181},

  # Ternary/default (right-associative)
  ://: {190, 191},

  # Concat/range (right-associative)
  :++: {200, 201},
  :--: {200, 201},
  :<>: {200, 201},
  :..: {200, 201},

  # Additive (left-associative)
  :+: {210, 211},
  :-: {210, 211},

  # Multiplicative (left-associative)
  :*: {220, 221},
  :/: {220, 221},

  # Power (left-associative per yrl, often right in other languages)
  :**: {230, 231},

  # Unary (non-associative, prefix only)
  :unary_plus: {300, 300},
  :unary_minus: {300, 300},
  :!: {300, 300},
  :^: {300, 300},
  :not: {300, 300},
  :~~~: {300, 300},

  # Dot call (left-associative)
  :.: {310, 311},

  # At (module attribute, non-associative)
  :@: {320, 320},

  # Access (highest for expressions)
  :access: {330, 331}
}
```

#### 3.2.2 Core Parsing Loop

```elixir
def parse_expression(parser, min_bp \\ 0) do
  # Null denotation: parse prefix/atomic
  {parser, left} = parse_nud(parser)

  parse_led_loop(parser, left, min_bp)
end

defp parse_led_loop(parser, left, min_bp) do
  case peek_operator(parser) do
    nil ->
      {parser, left}

    {op, {lbp, rbp}} when lbp < min_bp ->
      # Operator binds less tightly, return
      {parser, left}

    {op, {lbp, rbp}} ->
      parser = advance(parser)
      {parser, right} = parse_expression(parser, rbp)
      left = build_binary_op(op, left, right)
      parse_led_loop(parser, left, min_bp)
  end
end
```

#### 3.2.3 Null Denotation (Prefix/Atomic)

The `parse_nud` function handles:

```elixir
defp parse_nud(parser) do
  case parser.current do
    # Literals
    {:int, meta, value} -> parse_integer(parser)
    {:flt, meta, value} -> parse_float(parser)
    {:atom, meta, value} -> parse_atom(parser)
    {:bin_string_start, _, _} -> parse_string(parser)
    {:list_string_start, _, _} -> parse_charlist(parser)
    {:sigil_start, _, _, _} -> parse_sigil(parser)

    # Identifiers
    {:identifier, _, _} -> parse_identifier_or_call(parser)
    {:paren_identifier, _, _} -> parse_call(parser)
    {:alias, _, _} -> parse_alias(parser)

    # Prefix operators
    {:unary_op, _, op} -> parse_unary(parser, op)
    {:dual_op, _, op} -> parse_maybe_unary(parser, op)
    {:capture_op, _, _} -> parse_capture(parser)
    {:at_op, _, _} -> parse_at(parser)

    # Grouping/containers
    {:"(", _} -> parse_paren(parser)
    {:"[", _} -> parse_list(parser)
    {:"{", _} -> parse_tuple_or_map(parser)
    {:"<<", _} -> parse_bitstring(parser)
    {:%{}, _} -> parse_map(parser)
    {:%, _} -> parse_struct(parser)

    # Special forms
    {:fn, _} -> parse_fn(parser)
    {:do, _} -> parse_unexpected_do(parser)

    # Range without left operand
    {:range_op, _, :..} -> parse_nullary_range(parser)
    {:ellipsis_op, _, :...} -> parse_nullary_ellipsis(parser)

    # Error case
    token ->
      error = unexpected_token_error(token)
      parser = add_error(parser, error)
      {parser, error_node(token)}
  end
end
```

### 3.3 Recursive Descent Components

Some constructs are better handled with dedicated recursive descent functions:

#### 3.3.1 Block Parsing

```elixir
defp parse_do_block(parser) do
  # Push terminator expectation
  parser = push_terminator(parser, :end, parser.current)
  parser = expect_and_advance(parser, :do)

  # Parse stab clauses or expression list
  {parser, body} =
    if peek_stab?(parser) do
      parse_stab_clauses(parser)
    else
      parse_expression_list(parser, [:end, :else, :catch, :rescue, :after])
    end

  # Parse optional block clauses
  {parser, clauses} = parse_block_clauses(parser)

  # Expect end (with recovery)
  {parser, end_meta} = expect_end(parser)
  parser = pop_terminator(parser)

  {parser, {:__block__, merge_meta(do_meta, end_meta), body ++ clauses}}
end

defp parse_block_clauses(parser, acc \\ []) do
  case parser.current do
    {:block_identifier, _, clause} when clause in [:else, :catch, :rescue, :after] ->
      parser = advance(parser)
      {parser, body} = parse_stab_or_expr_list(parser)
      parse_block_clauses(parser, [{clause, body} | acc])

    _ ->
      {parser, Enum.reverse(acc)}
  end
end
```

#### 3.3.2 Function Calls (No-Parens Ambiguity)

Elixir's no-parens calls require careful handling as documented in `elixir_parser.yrl` lines 116-154:

```elixir
defp parse_identifier_or_call(parser) do
  {id_token, id_meta, id_name} = parser.current
  parser = advance(parser)

  case parser.current do
    # Parenthesized call: foo(args)
    {:"(", _} when id_token == :paren_identifier ->
      parse_paren_call(parser, id_name, id_meta)

    # Access: foo[key]
    {:"[", _} when id_token == :bracket_identifier ->
      parse_access_call(parser, id_name, id_meta)

    # Do block: foo do ... end
    {:do, _} when id_token == :do_identifier ->
      parse_do_block_call(parser, id_name, id_meta, [])

    # No-parens call: check if followed by valid argument
    _ when can_start_no_parens_arg?(parser) ->
      parse_no_parens_call(parser, id_name, id_meta)

    # Bare identifier (variable or nullary call)
    _ ->
      {parser, {id_name, id_meta, nil}}
  end
end

# Determine if current position can start a no-parens argument
# This is context-sensitive and must avoid ambiguity
defp can_start_no_parens_arg?(parser) do
  not parser.context.no_parens_allowed and
  not at_end_of_expression?(parser) and
  not at_operator?(parser) and
  can_start_expression?(parser.current)
end
```

### 3.4 Error Recovery Subsystem

Following Prism's multi-strategy approach:

#### 3.4.1 Recovery Strategies

```elixir
defmodule Parser.Recovery do
  @moduledoc """
  Error recovery strategies ordered by preference.
  """

  @doc """
  Strategy 1: Missing Token Insertion

  When we expect a specific token and find something else,
  synthesize the expected token and continue.
  """
  def insert_missing_token(parser, expected) do
    synthetic_meta = synthetic_meta_at(parser.current)
    error = missing_token_error(expected, parser.current)
    parser = add_error(parser, error)

    # Don't advance - the unexpected token stays for next parse
    {parser, {expected, synthetic_meta}}
  end

  @doc """
  Strategy 2: Missing Node Insertion

  When we need an expression but find something that can't start one,
  insert an error node placeholder.
  """
  def insert_missing_node(parser, context) do
    meta = meta_before(parser.current)
    error = missing_expression_error(context, parser.current)
    parser = add_error(parser, error)

    error_node = {:__error__, meta, [reason: context]}
    {parser, error_node}
  end

  @doc """
  Strategy 3: Synchronization (Panic Mode)

  Skip tokens until we find a synchronization point.
  """
  def synchronize(parser, sync_set) do
    {parser, skipped} = skip_until(parser, sync_set)

    if skipped > 0 do
      error = skipped_tokens_error(skipped, parser.current)
      parser = add_error(parser, error)
    end

    parser
  end

  @doc """
  Strategy 4: Delimiter Matching Recovery

  When we have unclosed delimiters, use terminator stack
  to synthesize closers.
  """
  def close_unclosed_delimiters(parser) do
    Enum.reduce(parser.terminators, parser, fn {type, open_meta}, parser ->
      closer = closing_token_for(type)
      error = unclosed_delimiter_error(type, open_meta)
      parser = add_error(parser, error)

      # Synthesize the closer
      synthetic = {closer, synthetic_meta_at(parser.current)}
      process_synthetic_closer(parser, synthetic)
    end)
  end
end
```

#### 3.4.2 Synchronization Points

Derived from Elixir's grammar structure:

```elixir
@sync_points %{
  # Statement level - recover at expression boundaries
  statement: [:eol, :";", :end, :else, :catch, :rescue, :after, :eof],

  # Expression level - recover at operators or delimiters
  expression: [:eol, :";", :",", :")", :"]", :"}", :">>" :end, :eof],

  # Argument list - recover at comma or closer
  args: [:",", :")", :"]", :"}", :eof],

  # Map/struct - recover at comma, arrow, or closer
  map: [:",", :"=>", :"}", :eof],

  # Block - recover at clause boundaries
  block: [:else, :catch, :rescue, :after, :end, :eof],

  # Pattern - similar to expression but includes match
  pattern: [:=, :<-, :",", :")", :"]", :"}", :when, :do, :->]
}
```

#### 3.4.3 Error Node Representation

```elixir
# Error nodes in the AST allow tools to:
# 1. See where errors occurred
# 2. Access partial information
# 3. Continue processing the rest of the tree

# Missing expression placeholder
{:__error__, meta, [reason: :missing_expression]}

# Unexpected token (preserved for analysis)
{:__error__, meta, [reason: :unexpected, token: token]}

# Incomplete construct
{:__error__, meta, [reason: :incomplete, partial: partial_ast]}
```

### 3.5 Context-Sensitive Parsing

Elixir has several context-sensitive constructs:

#### 3.5.1 Match Context

```elixir
# The = operator triggers match context on its left side
defp parse_match(parser, left) do
  parser = advance(parser)  # consume =

  # Right side is normal expression
  {parser, right} = parse_expression(parser, rbp_for(:=))

  # Reparse left as pattern if needed
  left = to_pattern(left)

  {parser, {:=, meta, [left, right]}}
end

defp to_pattern(ast) do
  # Transform expression AST to pattern AST
  # - Variables remain variables
  # - Calls become tuple patterns
  # - Pin operator marks external variables
  Macro.prewalk(ast, &pattern_transform/1)
end
```

#### 3.5.2 Whitespace Sensitivity

```elixir
# Unary vs binary operators depend on whitespace
defp parse_maybe_unary(parser, op) when op in [:+, :-] do
  op_meta = meta(parser.current)
  parser = advance(parser)

  case {space_before?(op_meta), space_after?(parser.current)} do
    {true, false} ->
      # `a + b` - binary
      :continue_as_binary

    {false, _} ->
      # `a+b` or `a +b` - depends on what follows
      if can_be_unary_operand?(parser.current) do
        # `+1` - unary
        {parser, operand} = parse_expression(parser, @unary_bp)
        {parser, {unary_op(op), op_meta, [operand]}}
      else
        :continue_as_binary
      end

    {true, true} ->
      # `a + b` - binary (most common)
      :continue_as_binary
  end
end
```

#### 3.5.3 `do:` vs `do` Block

```elixir
defp parse_call_args_and_do(parser, name, meta, args) do
  case parser.current do
    {:kw_identifier, _, :do} ->
      # Keyword argument: foo(x, do: y, else: z)
      {parser, kw_args} = parse_keyword_args(parser)
      {parser, {name, meta, args ++ [kw_args]}}

    {:do, _} ->
      # Block syntax: foo(x) do y else z end
      {parser, block} = parse_do_block(parser)
      {parser, {name, add_do_meta(meta, block), args ++ [block]}}

    _ ->
      {parser, {name, meta, args}}
  end
end
```

### 3.6 AST Design

#### 3.6.1 Compatibility with Elixir AST

The parser produces ASTs compatible with `Macro.t()`:

```elixir
# Three-element tuples for calls
{:foo, meta, [arg1, arg2]}

# Two-element tuples for aliases
{:__aliases__, meta, [:Foo, :Bar]}

# Literals pass through
42
:atom
"string"

# Special forms
{:fn, meta, clauses}
{:case, meta, [expr, [do: clauses]]}
```

#### 3.6.2 Extended Metadata

For IDE integration, metadata includes:

```elixir
%{
  # Position (from Toxic ranged meta)
  line: 1,
  column: 5,
  end_line: 1,
  end_column: 10,

  # Token-level detail
  delimiter: "\"",           # For strings/sigils
  format: :keyword,          # For keyword atoms
  closing: %{line: 3, column: 1},  # For blocks

  # Error recovery markers
  synthesized: true,         # If token was inserted
  error: :missing_closer,    # If contains error

  # Semantic hints
  ambiguous_op: true,        # For space-sensitive ops
  from_brackets: true        # For access syntax
}
```

#### 3.6.3 Error Nodes

```elixir
# Error nodes allow partial AST representation
defmodule Parser.AST.Error do
  @type t :: {:__error__, meta(), details()}

  @type details :: [
    reason: atom(),
    token: token() | nil,
    partial: Macro.t() | nil,
    expected: atom() | [atom()] | nil
  ]
end

# Examples:
{:__error__, meta, [reason: :missing_expression]}
{:__error__, meta, [reason: :unexpected_token, token: {:end, _}]}
{:__error__, meta, [reason: :unclosed_delimiter, partial: partial_list]}
```

---

## 4. Token Stream Integration

### 4.1 Toxic Adapter

```elixir
defmodule Parser.TokenStream do
  @moduledoc """
  Adapter wrapping Toxic tokenizer for parser consumption.
  """

  defstruct [
    stream: nil,           # Toxic stream
    buffer: [],            # Lookahead buffer
    checkpoints: %{},      # Saved positions for backtracking
    current_meta: nil      # Tracking for error positions
  ]

  def new(source, opts \\ []) do
    toxic_opts = [
      error_mode: Keyword.get(opts, :error_mode, :tolerant),
      preserve_comments: Keyword.get(opts, :preserve_comments, false)
    ]

    stream = Toxic.new(source, 1, 1, toxic_opts)

    %__MODULE__{stream: stream}
    |> fill_buffer(2)  # Maintain 2-token lookahead
  end

  def current(%__MODULE__{buffer: [token | _]}), do: token
  def current(%__MODULE__{buffer: []}), do: {:eof, {{0, 0}, {0, 0}, nil}}

  def peek(%__MODULE__{buffer: [_, next | _]}), do: next
  def peek(%__MODULE__{buffer: _}), do: {:eof, {{0, 0}, {0, 0}, nil}}

  def advance(%__MODULE__{buffer: [_ | rest]} = ts) do
    ts = %{ts | buffer: rest}
    fill_buffer(ts, 2)
  end

  defp fill_buffer(%__MODULE__{buffer: buffer, stream: stream} = ts, min_size)
       when length(buffer) >= min_size do
    ts
  end

  defp fill_buffer(%__MODULE__{stream: stream} = ts, min_size) do
    case Toxic.next(stream) do
      {:ok, token, new_stream} ->
        ts = %{ts |
          stream: new_stream,
          buffer: ts.buffer ++ [normalize_token(token)]
        }
        fill_buffer(ts, min_size)

      {:eof, new_stream} ->
        %{ts | stream: new_stream}

      {:error, error, new_stream} ->
        # Convert Toxic error to error token
        error_token = {:error_token, error_meta(error), error}
        ts = %{ts |
          stream: new_stream,
          buffer: ts.buffer ++ [error_token]
        }
        fill_buffer(ts, min_size)
    end
  end

  # Normalize Toxic tokens to parser's expected format
  defp normalize_token(token) do
    # Handle EOL coalescing, interpolation markers, etc.
    token
  end
end
```

### 4.2 Handling Interpolation

Toxic linearizes interpolation. The parser reassembles:

```elixir
defp parse_string(parser) do
  start_token = parser.current
  parser = advance(parser)

  {parser, parts} = parse_string_parts(parser, [])

  {parser, end_meta} = expect_string_end(parser)

  case parts do
    [binary] when is_binary(binary) ->
      # Simple string literal
      {parser, binary}

    parts ->
      # Interpolated string
      {parser, {:<<>>, merge_meta(start_token, end_meta), interpolation_to_ast(parts)}}
  end
end

defp parse_string_parts(parser, acc) do
  case parser.current do
    {:string_fragment, _, binary} ->
      parser = advance(parser)
      parse_string_parts(parser, [binary | acc])

    {:begin_interpolation, _, _} ->
      parser = advance(parser)
      {parser, expr} = parse_expression(parser)
      {parser, _} = expect(parser, :end_interpolation)
      parse_string_parts(parser, [{:interpolation, expr} | acc])

    {:bin_string_end, _, _} ->
      {parser, Enum.reverse(acc)}

    _ ->
      # Error recovery - unclosed string
      {parser, Enum.reverse(acc)}
  end
end
```

---

## 5. Performance Considerations

### 5.1 Fuel System

Prevent infinite loops in error recovery:

```elixir
defp consume_fuel(parser) do
  case parser.fuel do
    0 ->
      raise Parser.FuelExhausted, "Parser exceeded maximum iterations"
    n ->
      %{parser | fuel: n - 1}
  end
end

# Called at every loop iteration and recursive call
defp parse_led_loop(parser, left, min_bp) do
  parser = consume_fuel(parser)
  # ... rest of implementation
end
```

### 5.2 Lookahead Optimization

Minimize lookahead where possible:

```elixir
# Use token classification instead of pattern matching
defp at_expression_end?(parser) do
  token_type(parser.current) in @expression_terminators
end

@expression_terminators MapSet.new([
  :eol, :";", :",", :")", :"]", :"}", :">>" :end, :else,
  :catch, :rescue, :after, :eof
])

# Cache operator lookups
@operator_info %{
  :+ => {:dual_op, {210, 211}},
  :- => {:dual_op, {210, 211}},
  # ... pre-computed
}
```

### 5.3 Memory Management

```elixir
# Avoid accumulating large intermediate structures
defp parse_expression_list(parser, terminators, acc \\ []) do
  if at_terminator?(parser, terminators) do
    {parser, Enum.reverse(acc)}  # Single reverse at end
  else
    {parser, expr} = parse_expression(parser)

    parser = case parser.current do
      {:eol, _} -> advance(parser)
      {:";", _} -> advance(parser)
      _ -> parser
    end

    parse_expression_list(parser, terminators, [expr | acc])
  end
end
```

---

## 6. IDE Integration Features

### 6.1 Partial Parse Results

Always return an AST, even with errors:

```elixir
def parse(source, opts \\ []) do
  parser = Parser.new(source, opts)
  {parser, ast} = parse_module_body(parser)

  %ParseResult{
    ast: ast,
    errors: parser.errors,
    warnings: get_warnings(parser),
    comments: get_comments(parser),
    tokens: if(opts[:include_tokens], do: get_tokens(parser))
  }
end
```

### 6.2 Incremental Reparsing Hooks

```elixir
defmodule Parser.Incremental do
  @moduledoc """
  Support for incremental reparsing after edits.
  """

  def reparse_region(prev_result, edit_range, new_text) do
    # 1. Find affected top-level forms
    affected = find_affected_forms(prev_result.ast, edit_range)

    # 2. Determine reparse boundaries
    {start_pos, end_pos} = expand_to_safe_boundaries(affected)

    # 3. Extract region text
    region_text = apply_edit(prev_result.source, edit_range, new_text, start_pos, end_pos)

    # 4. Reparse region
    region_result = parse(region_text, line: start_pos.line, column: start_pos.column)

    # 5. Splice into previous AST
    splice_ast(prev_result.ast, affected, region_result.ast)
  end
end
```

### 6.3 Scope and Reference Information

```elixir
defmodule Parser.Scopes do
  @moduledoc """
  Track variable scopes during parsing for IDE features.
  """

  def collect_scopes(ast) do
    {_, scopes} = Macro.prewalk(ast, %{current: [], stack: []}, &collect_scope/2)
    scopes
  end

  defp collect_scope({:=, _, [pattern, _]} = node, acc) do
    vars = extract_bound_vars(pattern)
    acc = add_bindings(acc, vars)
    {node, acc}
  end

  defp collect_scope({:fn, _, _} = node, acc) do
    # New scope for fn
    acc = push_scope(acc)
    {node, acc}
  end

  # ... more scope rules
end
```

---

## 7. Testing Strategy

### 7.1 Corpus-Based Testing

```elixir
# Parse all .ex files in Elixir stdlib
# Compare AST structure (ignoring metadata) with Code.string_to_quoted
def corpus_test(file_path) do
  source = File.read!(file_path)

  our_ast = Parser.parse(source) |> strip_meta()
  elixir_ast = Code.string_to_quoted!(source) |> strip_meta()

  assert our_ast == elixir_ast,
    "AST mismatch for #{file_path}"
end
```

### 7.2 Error Recovery Testing

```elixir
# Test that errors don't crash and produce usable AST
describe "error recovery" do
  test "missing closing paren" do
    result = Parser.parse("foo(1, 2")

    assert length(result.errors) > 0
    assert match?({:foo, _, [_, _]}, result.ast)
  end

  test "multiple errors reported" do
    result = Parser.parse("foo( bar(")

    # Should report both unclosed parens
    assert length(result.errors) >= 2
  end

  test "recovery at statement boundary" do
    result = Parser.parse("""
    def foo do
      @#$%
      bar()
    end
    """)

    # Should parse the valid parts
    assert {:def, _, _} = result.ast
  end
end
```

### 7.3 Property-Based Testing

```elixir
# Generate random valid Elixir and verify parsing
property "parses valid elixir without errors" do
  check all source <- valid_elixir_generator() do
    result = Parser.parse(source)
    assert result.errors == []
  end
end

# Generate random invalid Elixir and verify graceful handling
property "handles invalid elixir without crashing" do
  check all source <- random_text_generator() do
    result = Parser.parse(source)
    assert is_struct(result, ParseResult)
  end
end
```

---

## 8. Implementation Roadmap

### Phase 1: Core Parser (Foundation)

- [ ] Token stream adapter for Toxic
- [ ] Pratt parser core with precedence table
- [ ] Literal parsing (numbers, atoms, strings)
- [ ] Basic operators (arithmetic, comparison)
- [ ] Grouping (parentheses, lists, tuples, maps)

### Phase 2: Elixir Constructs

- [ ] Function calls (parens and no-parens)
- [ ] Do blocks and block clauses
- [ ] Anonymous functions
- [ ] Pattern matching
- [ ] Guards and `when`
- [ ] Comprehensions

### Phase 3: Error Recovery

- [ ] Missing token insertion
- [ ] Missing node insertion
- [ ] Synchronization points
- [ ] Delimiter matching recovery
- [ ] Error node representation

### Phase 4: Advanced Features

- [ ] String interpolation reassembly
- [ ] Sigils and heredocs
- [ ] Module attributes
- [ ] Macro-aware parsing
- [ ] Bitstring syntax

### Phase 5: IDE Integration

- [ ] Extended metadata
- [ ] Scope tracking
- [ ] Incremental reparse hooks
- [ ] Comment preservation
- [ ] Source mapping

### Phase 6: Optimization & Polish

- [ ] Performance profiling
- [ ] Memory optimization
- [ ] Comprehensive test suite
- [ ] Documentation
- [ ] Benchmark suite

---

## 9. References

1. **Ruby Prism/YARP**: https://github.com/ruby/prism
   - Error recovery strategies
   - Portable C implementation
   - Serialization format

2. **Rails at Scale - Rewriting the Ruby Parser**: https://railsatscale.com/2023-06-12-rewriting-the-ruby-parser/
   - Rationale for hand-written parsers
   - Grammar ambiguity handling

3. **Ruff Python Parser**: https://astral.sh/blog/ruff-v0.4.0
   - Performance benefits of hand-written parsing
   - Migration from generated to hand-written

4. **Elixir Parser Grammar**: `elixir_parser.yrl`
   - Authoritative grammar specification
   - Precedence levels
   - Context sensitivity documentation

5. **Toxic Tokenizer**: `lib/toxic.ex`, `ALL_TOKENS.md`
   - Streaming API
   - Token shapes
   - Error recovery at lexical level

6. **Pratt Parsing**:
   - "Top Down Operator Precedence" - Vaughan Pratt (1973)
   - "Simple but Powerful Pratt Parsing" - Alex Kladov
   - "From Precedence Climbing to Pratt Parsing" - Andy Chu
