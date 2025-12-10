# High-Level Design: Error-Tolerant Elixir Parser

## Executive Summary

This document proposes a new error-tolerant parser for Elixir, designed to power Language Server Protocol (LSP) implementations, type checkers, and other IDE tooling. The design draws lessons from Spitfire's shortcomings and follows successful patterns from modern parsers like Ruby's Prism (YARP), Ruff's Python parser, and rustc.

**Core principles:**
1. **Grammar-faithful**: Mirror `elixir_parser.yrl` nonterminals directly in code
2. **Error-tolerant from the ground up**: Not bolted on as an afterthought
3. **Zero post-processing normalization**: Produce correct AST during parsing
4. **Tight Toxic integration**: Leverage the streaming lexer's full capabilities

---

## 1. Lessons from Spitfire

### 1.1 What Went Wrong

The reviews identified critical architectural failures:

| Problem | Symptom | Impact |
|---------|---------|--------|
| Flattened grammar | Single `parse_expression` with 6 boolean flags | Combinatorial explosion of edge cases |
| Post-parse normalization | 20+ `Macro.prewalk` passes | Errors corrupt during transformation |
| Ad-hoc EOE handling | 7+ different EOL/EOE functions | Fragile expression boundaries |
| State pollution | `pending_newlines`, `stab_state`, etc. | Implicit state machine, hard to reason about |
| Fuel system | Prevents infinite loops | Parser can enter non-consuming loops |
| Dual tokenizer support | Legacy + Toxic paths | Double maintenance, subtle bugs |

### 1.2 Key Insight from YARP/Prism

> "Ruby's grammar cannot be accurately parsed with an LR parser without significant state being stored in the lexer."

Elixir shares this property. The grammar is **context-sensitive** (space sensitivity, optional parentheses, `do` block attachment). Pure Pratt parsing struggles because:
- Pratt handles operator precedence well but not statement-level structure
- `do/end` blocks, `fn` clauses, and `->` expressions need explicit grammar rules
- Expression categories (`matched_expr`, `no_parens_expr`, `unmatched_expr`) require structural enforcement

---

## 2. Parser Architecture

### 2.1 Design Choice: Hybrid Recursive Descent + Pratt

Following the successful patterns from Prism (Ruby) and Ruff (Python):

```
┌─────────────────────────────────────────────────────────────┐
│                    Toxic Token Stream                        │
│  (streaming, error tokens, precise ranges, peek_n)          │
└─────────────────────┬───────────────────────────────────────┘
                      │
┌─────────────────────▼───────────────────────────────────────┐
│              Recursive Descent Layer                         │
│  parse_grammar/1, parse_expr_list/1, parse_block/1          │
│  parse_do_block/1, parse_fn/1, parse_stab/1                 │
│  One function per yrl nonterminal                           │
└─────────────────────┬───────────────────────────────────────┘
                      │ calls into
┌─────────────────────▼───────────────────────────────────────┐
│                  Pratt Expression Parser                     │
│  parse_matched_expr/2, parse_unmatched_expr/2               │
│  parse_no_parens_expr/2, parse_container_expr/2             │
│  Handles ONLY operator precedence within expressions         │
└─────────────────────┬───────────────────────────────────────┘
                      │ produces
┌─────────────────────▼───────────────────────────────────────┐
│                     AST Output                               │
│  Core-compatible, error nodes preserved                     │
│  NO post-processing normalization                           │
└─────────────────────────────────────────────────────────────┘
```

### 2.2 Why Not Pure Pratt?

Pratt parsing is excellent for expressions but poor for:

1. **Statement boundaries (EOE)**: Grammar has explicit `eoe` production (`eol | ;`)
2. **Block structures**: `do/end`, `fn`, `case`, `try` with clause lists
3. **Expression categories**: `matched_expr` vs `no_parens_expr` vs `unmatched_expr`
4. **Context-sensitive ambiguity**: `foo bar, baz` - is it `foo(bar, baz)` or `foo(bar), baz`?

The Elixir grammar encodes these as distinct nonterminals. Trying to encode them as precedence levels (as Spitfire does) leads to boolean flag hell.

### 2.3 Why Not Generated Parser (LALR/PEG)?

1. **Error recovery**: Hand-written parsers allow fine-grained, context-aware recovery
2. **Control**: Can handle ambiguities and special cases precisely
3. **Maintainability**: Easier to debug and modify than generated code
4. **Industry trend**: 7 of top 10 languages use hand-written recursive descent

---

## 3. Grammar Mapping

### 3.1 Direct Nonterminal Translation

Each `elixir_parser.yrl` nonterminal becomes a parser function:

```elixir
# Top-level structure
defp parse_grammar(parser)         # grammar -> eoe? expr_list eoe?
defp parse_expr_list(parser)       # expr_list -> expr (eoe expr)*
defp parse_eoe(parser)             # eoe -> eol | ';' | eol ';'

# Expression categories (critical distinction)
defp parse_expr(parser)            # expr -> matched | no_parens | unmatched
defp parse_matched_expr(parser)    # fully parenthesized expressions
defp parse_unmatched_expr(parser)  # expressions with do-blocks
defp parse_no_parens_expr(parser)  # ambiguous no-parens calls

# Sub-categories from grammar
defp parse_no_parens_one_expr(parser)     # f a
defp parse_no_parens_many_expr(parser)    # f a, b
defp parse_no_parens_one_ambig_expr(parser) # f g a, b

# Structural constructs
defp parse_block_expr(parser)      # calls with do blocks
defp parse_do_block(parser)        # do ... end
defp parse_fn(parser)              # fn ... end
defp parse_stab(parser)            # -> clauses
defp parse_stab_expr(parser)       # individual clause

# Containers
defp parse_list(parser)            # [...]
defp parse_tuple(parser)           # {...}
defp parse_map(parser)             # %{...}
defp parse_bitstring(parser)       # <<...>>
defp parse_container_args(parser)  # items inside containers

# Access and calls
defp parse_access_expr(parser)     # atomic expressions + access
defp parse_bracket_expr(parser)    # expr[key]
defp parse_parens_call(parser)     # f(args)
defp parse_call_args_parens(parser)
defp parse_call_args_no_parens(parser)
```

### 3.2 Expression Category Enforcement

The grammar distinguishes expression types to resolve ambiguity:

```
matched_expr    - Unambiguous: literals, parens calls, operators with matched operands
no_parens_expr  - Ambiguous: f a, b (who owns the args?)
unmatched_expr  - Has do-block: if cond do ... end
```

**Implementation strategy:**

```elixir
defp parse_expr(parser) do
  # Try matched first (most common, unambiguous)
  case parse_matched_expr(parser) do
    {:ok, expr, parser} ->
      # Check if this becomes unmatched (followed by do-block)
      if peek_do_block?(parser) do
        parse_do_block_continuation(expr, parser)
      else
        {:ok, expr, parser}
      end

    :not_matched ->
      # Try no_parens patterns
      parse_no_parens_or_unmatched(parser)
  end
end
```

### 3.3 EOE (End of Expression) Handling

Centralized, not scattered:

```elixir
@doc """
Parse end-of-expression marker.
Grammar: eoe -> eol | ';' | eol ';'
"""
defp parse_eoe(parser) do
  case current_token(parser) do
    {:eol, meta} ->
      parser = advance(parser)
      case current_token(parser) do
        {:";", _} -> {:ok, {:eoe, meta, :eol_semi}, advance(parser)}
        _ -> {:ok, {:eoe, meta, :eol}, parser}
      end

    {:";", meta} ->
      {:ok, {:eoe, meta, :semi}, advance(parser)}

    _ ->
      :no_eoe
  end
end

@doc """
Skip optional EOE (for use in containers, after operators, etc.)
"""
defp skip_eoe(parser) do
  case parse_eoe(parser) do
    {:ok, _, parser} -> parser
    :no_eoe -> parser
  end
end
```

---

## 4. Pratt Layer Design

### 4.1 Minimal Scope

The Pratt parser handles **only** operator expressions within `matched_expr`:

```elixir
@precedences %{
  # From elixir_parser.yrl, directly mapped
  do: 5,
  stab_op: 10,      # ->
  ",": 20,
  in_match_op: 40,  # <-, \\
  when_op: 50,
  type_op: 60,      # ::
  pipe_op: 70,      # |
  assoc_op: 80,     # =>
  capture_op: 90,   # &
  ellipsis_op: 90,  # ...
  match_op: 100,    # =
  or_op: 120,       # ||, |||, or
  and_op: 130,      # &&, &&&, and
  comp_op: 140,     # ==, !=, etc.
  rel_op: 150,      # <, >, etc.
  arrow_op: 160,    # |>, ~>, etc.
  in_op: 170,       # in, not in
  xor_op: 180,      # ^^^
  ternary_op: 190,  # //
  concat_op: 200,   # ++, --, <>
  range_op: 200,    # ..
  dual_op: 210,     # +, -
  mult_op: 220,     # *, /
  power_op: 230,    # **
  unary_op: 300,    # +, -, !, ^, not
  dot_call_op: 310, # .
  dot_op: 310,      # .
  at_op: 320,       # @
}

@right_assoc MapSet.new([
  :stab_op, :when_op, :type_op, :pipe_op, :assoc_op,
  :match_op, :ternary_op, :concat_op, :range_op, :power_op
])
```

### 4.2 Pratt Core Loop

```elixir
defp parse_pratt_expr(parser, min_bp) do
  # NUD (null denotation) - prefix/atomic
  {:ok, left, parser} = parse_nud(parser)

  # LED (left denotation) - infix loop
  parse_led_loop(left, parser, min_bp)
end

defp parse_led_loop(left, parser, min_bp) do
  case peek_operator(parser) do
    {:ok, op, bp} when bp > min_bp ->
      parser = advance(parser)
      parser = skip_eoe(parser)  # op_eol handling

      right_bp = if right_assoc?(op), do: bp, else: bp + 1
      {:ok, right, parser} = parse_pratt_expr(parser, right_bp)

      node = build_binary_op(op, left, right)
      parse_led_loop(node, parser, min_bp)

    _ ->
      {:ok, left, parser}
  end
end
```

### 4.3 NUD Implementations

```elixir
defp parse_nud(parser) do
  case current_token(parser) do
    # Unary operators
    {:unary_op, meta, op} -> parse_unary(parser, meta, op)
    {:dual_op, meta, op} -> parse_unary(parser, meta, op)  # space-sensitive
    {:at_op, meta, :@} -> parse_at_op(parser, meta)
    {:capture_op, meta, :&} -> parse_capture(parser, meta)

    # Literals
    {:int, _, _} -> parse_number(parser)
    {:flt, _, _} -> parse_number(parser)
    {:char, _, _} -> parse_char(parser)
    {:atom, _, _} -> parse_atom(parser)
    {:alias, _, _} -> parse_alias(parser)
    {true, _} -> parse_boolean(parser)
    {false, _} -> parse_boolean(parser)
    {nil, _} -> parse_nil(parser)

    # Strings (delegates to string parser)
    {:bin_string_start, _, _} -> parse_string(parser)
    {:list_string_start, _, _} -> parse_charlist(parser)
    {:bin_heredoc_start, _, _} -> parse_heredoc(parser)
    {:sigil_start, _, _, _} -> parse_sigil(parser)

    # Containers (delegates to container parsers)
    {:"(", _} -> parse_paren_expr(parser)
    {:"[", _} -> parse_list(parser)
    {:"{", _} -> parse_tuple(parser)
    {:%{}, _} -> parse_map(parser)
    {:%, _} -> parse_struct(parser)
    {:"<<", _} -> parse_bitstring(parser)

    # Identifiers
    {:identifier, _, _} -> parse_identifier(parser)
    {:paren_identifier, _, _} -> parse_paren_call(parser)
    {:bracket_identifier, _, _} -> parse_bracket_access(parser)
    {:do_identifier, _, _} -> parse_do_call(parser)

    # Special
    {:fn, _} -> parse_fn(parser)
    {:range_op, meta, :..} -> parse_nullary_range(parser, meta)
    {:ellipsis_op, meta, :...} -> parse_nullary_ellipsis(parser, meta)

    # Error recovery
    {:error_token, meta, error} -> parse_error_token(parser, meta, error)

    _ -> {:error, :unexpected_token, parser}
  end
end
```

---

## 5. Error Recovery Architecture

### 5.1 Design Principles

Following Prism/YARP's layered recovery:

1. **Error tokens are first-class AST nodes** - Not metadata, not exceptions
2. **Subtree isolation** - Errors in one branch don't affect siblings
3. **Synchronization points** - Clear recovery positions per grammar rule
4. **Range preservation** - Error nodes carry their source span

### 5.2 Error Node Structure

```elixir
# Error node in AST
{:__error__, meta, [
  error_type: atom(),           # :missing_closing, :unexpected_token, etc.
  expected: term() | nil,       # What was expected
  recovered_content: [term()],  # Parsed children (possibly partial)
  raw_tokens: [token()]         # Original tokens for the error span
]}

# Meta includes
# - Range: {{start_line, start_col}, {end_line, end_col}}
# - error: true
# - error_code: atom()
```

### 5.3 Synchronization Sets

Each grammar rule defines what tokens can resynchronize parsing:

```elixir
@sync_sets %{
  expr_list: [:eol, :";", :end, :eof],
  container_args: [:",", :"]", :"}", :">>" , :end, :eof],
  do_block: [:else, :catch, :rescue, :after, :end, :eof],
  stab: [:->, :eol, :";", :end, :eof],
  parens: [:")", :eof],
  brackets: [:"]", :eof],
  braces: [:"}", :eof],
  bits: [:">>", :eof]
}

defp synchronize(parser, context) do
  sync_tokens = Map.get(@sync_sets, context, [:eof])
  skip_until(parser, sync_tokens)
end
```

### 5.4 Recovery Strategies

```elixir
defp try_parse(parser, parse_fn, context) do
  try do
    parse_fn.(parser)
  catch
    {:parse_error, error_info, parser} ->
      # Create error node with whatever we parsed
      error_node = build_error_node(error_info, parser)

      # Synchronize to safe point
      parser = synchronize(parser, context)

      {:recovered, error_node, parser}
  end
end

# Missing token insertion
defp expect(parser, expected_token, context) do
  case current_token(parser) do
    {^expected_token, meta} ->
      {:ok, meta, advance(parser)}

    actual ->
      # Insert synthetic token, continue parsing
      synthetic_meta = synthetic_meta_before(parser)
      error = {:missing_token, expected_token, actual}
      parser = add_error(parser, error)
      {:recovered, synthetic_meta, parser}
  end
end

# Missing node insertion
defp parse_required_expr(parser, context) do
  case parse_expr(parser) do
    {:ok, expr, parser} ->
      {:ok, expr, parser}

    {:error, _, parser} ->
      # Insert placeholder node
      placeholder = {:__error__, synthetic_meta(parser), [
        error_type: :missing_expression,
        expected: :expression,
        recovered_content: []
      ]}
      {:recovered, placeholder, parser}
  end
end
```

### 5.5 Toxic Error Token Handling

```elixir
defp parse_error_token(parser, meta, %Toxic.Error{} = error) do
  # Wrap lexer error as parser error node
  node = {:__error__, meta, [
    error_type: :lexer_error,
    error_code: error.code,
    error_domain: error.domain,
    token_display: error.token_display,
    details: error.details,
    recovered_content: []
  ]}

  {:ok, node, advance(parser)}
end
```

---

## 6. Parser State

### 6.1 Minimal State

Unlike Spitfire's 10+ ad-hoc flags, maintain minimal explicit state:

```elixir
defstruct [
  # Token stream
  stream: nil,              # Toxic stream
  current: nil,             # Current token
  peek: nil,                # Next token (from Toxic.peek)

  # Position tracking
  errors: [],               # Accumulated errors
  warnings: [],             # Accumulated warnings

  # Context stack (explicit, not flags)
  context: [],              # [:do_block, :fn, :list, :map, ...]

  # Recovery state
  fuel: 1000,               # Runaway prevention (but shouldn't be needed)
]
```

### 6.2 Context Stack vs Boolean Flags

Instead of `is_list`, `is_map`, `is_stab`, `is_top`:

```elixir
# Push context when entering constructs
defp parse_list(parser) do
  parser = push_context(parser, :list)
  # ... parse list contents ...
  parser = pop_context(parser)
  {:ok, node, parser}
end

# Check context when needed
defp in_context?(parser, ctx) do
  ctx in parser.context
end

# Context affects parsing decisions
defp parse_container_args(parser) do
  # Container args differ from call args
  if in_context?(parser, :map) do
    parse_map_args(parser)
  else
    parse_list_or_tuple_args(parser)
  end
end
```

---

## 7. Toxic Integration

### 7.1 Drop Legacy Tokenizer Entirely

No dual paths. Toxic only:

```elixir
defp init_stream(source, opts) do
  Toxic.new(source, 1, 1, [
    error_mode: :tolerant,
    error_sync: [:semicolon, :newline, :closer, :comma],
    insert_structural_closers: true,
    preserve_comments: opts[:preserve_comments] || false
  ])
end
```

### 7.2 Use `peek_n` for Disambiguation

Many Elixir constructs need 2-3 token lookahead:

```elixir
# Is this `f [key: val]` (call) or `[key: val]` (list)?
defp peek_is_call_with_brackets?(parser) do
  case Toxic.peek_n(parser.stream, 2) do
    {:ok, [{:identifier, _, _}, {:"[", _}], _} -> true
    _ -> false
  end
end

# Is this `x do ... end` or `x.do`?
defp peek_is_do_block?(parser) do
  case Toxic.peek(parser.stream) do
    {:ok, {:do, _}, _} -> true
    _ -> false
  end
end

# Stab vs infix disambiguation: `(a, b -> c)` vs `(a, b) -> c`
defp peek_stab_in_parens?(parser) do
  # Look for -> before closing paren
  scan_for_stab_before_close(parser, 0)
end
```

### 7.3 Terminator Stack for Recovery

```elixir
defp current_expected_closer(parser) do
  {terminators, _} = Toxic.current_terminators(parser.stream)
  case terminators do
    [{opener, _meta, _indent} | _] -> Toxic.Driver.closing_for(opener)
    [] -> nil
  end
end

defp missing_closer_error(parser) do
  case current_expected_closer(parser) do
    nil -> nil
    closer -> {:error, :missing_closer, closer}
  end
end
```

---

## 8. AST Output

### 8.1 No Normalization Passes

The parser **must** produce correct AST directly:

```elixir
# WRONG: Spitfire's approach
# Parse -> malformed AST -> normalize_not_in -> normalize_fn_clauses -> ...

# CORRECT: This design
# Parse -> correct AST (with error nodes for broken parts)
```

### 8.2 Core Compatibility

Output matches `Code.string_to_quoted/2`:

```elixir
# Regular expression
{:+, [line: 1, column: 3], [1, 2]}

# Function call
{:foo, [line: 1], [{:bar, [line: 1], nil}]}

# Do block
{:if, [line: 1, do: [...], end: [...]],
  [{:cond, ...}, [do: {:body, ...}]]}

# Error node (extension)
{:__error__, [line: 1, column: 5, error: true],
  [error_type: :missing_end, recovered_content: [...]]}
```

### 8.3 Metadata Completeness

For IDE tooling, include rich metadata:

```elixir
[
  line: pos_integer(),
  column: pos_integer(),
  # For constructs with closers
  closing: [line: pos_integer(), column: pos_integer()],
  # For do blocks
  do: [...],
  end: [...],
  # For strings
  delimiter: binary(),
  # For errors
  error: true,
  error_code: atom()
]
```

---

## 9. Module Structure

```
lib/
  parser.ex                    # Main entry point
  parser/
    state.ex                   # Parser state struct
    tokens.ex                  # Token helpers, classification
    errors.ex                  # Error types, recovery

    # Recursive descent layer
    grammar.ex                 # parse_grammar, parse_expr_list
    expressions.ex             # parse_expr, parse_matched_expr, etc.
    blocks.ex                  # parse_do_block, parse_fn, parse_stab
    containers.ex              # parse_list, parse_tuple, parse_map, etc.
    calls.ex                   # parse_parens_call, parse_no_parens_call

    # Pratt layer
    pratt.ex                   # Core Pratt loop
    precedence.ex              # Precedence table, associativity
    operators.ex               # Binary/unary operator handling

    # Utilities
    ast.ex                     # AST node builders
    meta.ex                    # Metadata handling
```

---

## 10. Testing Strategy

### 10.1 Correctness Tests

```elixir
# Property: Parser output matches Code.string_to_quoted for valid code
property "valid code parses identically to core" do
  check all code <- valid_elixir_code() do
    our_ast = Parser.parse(code)
    core_ast = Code.string_to_quoted!(code, columns: true, token_metadata: true)
    assert ast_equivalent?(our_ast, core_ast)
  end
end
```

### 10.2 Error Tolerance Tests

```elixir
# Parser always produces output (never crashes)
property "any input produces AST" do
  check all code <- arbitrary_string() do
    result = Parser.parse(code)
    assert match?({:ok, _ast} | {:error, _ast, _errors}, result)
  end
end

# Error recovery preserves sibling nodes
test "error in list element preserves other elements" do
  code = "[1, @@@, 3]"
  {:ok, ast} = Parser.parse(code)
  assert [{:__block__, _, [1]}, {:__error__, _, _}, {:__block__, _, [3]}] =
         get_list_elements(ast)
end
```

### 10.3 Fuzz Testing

Following Prism's approach:
- Parse Elixir stdlib
- Parse top 100 hex packages
- Random mutation testing

---

## 11. Performance Considerations

### 11.1 Single Pass

No AST rewrites means single traversal. The Pratt + recursive descent hybrid typically achieves O(n) parsing.

### 11.2 Streaming from Toxic

Toxic tokenizes on-demand. Combined with our single-pass approach:
- Memory: proportional to max nesting depth, not file size
- Latency: can start producing AST before full file is read

### 11.3 Benchmarks Target

Based on Ruff's improvements:
- Target: 2x faster than current Spitfire (post-normalization)
- Memory: < 2x input size peak footprint

---

## 12. Implementation Phases

### Phase 1: Core Infrastructure
- Parser state, Toxic integration
- Basic error node structure
- `parse_grammar`, `parse_expr_list`, `parse_eoe`

### Phase 2: Pratt Expression Parser
- Precedence table
- Binary/unary operators
- Literals and atoms

### Phase 3: Structural Constructs
- Containers: lists, tuples, maps, bitstrings
- Calls: parens, no-parens, bracket access
- Identifiers: regular, paren, bracket, do, op

### Phase 4: Block Constructs
- `do/end` blocks
- `fn` expressions
- Stab clauses
- Block labels (`else`, `catch`, `rescue`, `after`)

### Phase 5: String Interpolation
- Binary strings, charlists
- Heredocs
- Sigils
- Quoted atoms/identifiers

### Phase 6: Error Recovery
- Synchronization sets refinement
- Missing token insertion
- Missing node insertion
- Toxic error token integration

### Phase 7: Polish & Performance
- Benchmark suite
- Fuzz testing
- Edge case hardening

---

## 13. Open Questions

1. **Comment preservation**: Include as AST nodes or metadata?
   - Recommendation: Configurable, default to metadata for tooling compatibility

2. **Cursor support**: Special `__cursor__` node for IDE completion?
   - Recommendation: Yes, following Elixir's `Code.Fragment` patterns

3. **Incremental parsing**: Worth the complexity?
   - Recommendation: Defer to post-v1; Toxic supports it but parser would need careful design

4. **Compatibility mode**: Strict mode that errors like core parser?
   - Recommendation: Yes, for testing/validation

---

## References

- Ruby Prism (YARP): https://github.com/ruby/prism
- YARP Design Blog: https://railsatscale.com/2023-06-12-rewriting-the-ruby-parser/
- Ruff Parser: https://astral.sh/blog/ruff-v0.4.0
- Elixir Grammar: `lib/elixir/src/elixir_parser.yrl`
- Toxic Tokenizer: `toxic/lib/toxic.ex`
- Spitfire Reviews: `REVIEW_CODEX_1.md`, `REVIEW_G3_1.md`, `REVIEW_OPUS_1.md`
