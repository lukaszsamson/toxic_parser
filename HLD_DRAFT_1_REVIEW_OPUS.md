# HLD_DRAFT_1.md Design Review

**Reviewer:** Claude (Opus)
**Date:** 2025-12-10
**Scope:** Comprehensive review of HLD_DRAFT_1.md against elixir_parser.yrl, Toxic tokenizer, and modern parser best practices (Prism, Ruff)

---

## Executive Summary

HLD_DRAFT_1.md represents a well-considered synthesis of ideas from multiple drafts. It correctly identifies the core architectural approach (hybrid recursive descent + Pratt) and properly prioritizes grammar fidelity over ad-hoc solutions. However, several areas require clarification, correction, or expansion before implementation.

**Overall Assessment:** Strong foundation with specific gaps to address.

| Category | Rating | Notes |
|----------|--------|-------|
| Grammar Fidelity | Good | Correct approach, needs precedence table verification |
| Error Recovery | Good | Sound strategy, needs sync set refinement |
| Toxic Integration | Fair | Missing details on structural token handling |
| IDE Integration | Fair | Environment/event log underspecified |
| Implementation Guidance | Fair | Needs more concrete algorithms |
| Completeness | Good | Covers most concerns, some gaps |

---

## 1. Correct Design Decisions

### 1.1 Hybrid Architecture Choice

The decision to use recursive descent for structural constructs and Pratt for expression precedence is **correct** and aligned with industry best practices:

- **Prism (Ruby):** Hand-written recursive descent with operator precedence parsing
- **Ruff (Python):** Hand-written recursive descent, achieved 2x speedup over LALRPOP
- **rustc:** Recursive descent with Pratt for expressions

The HLD correctly identifies that:
> "If the production's main complexity is **operator precedence**, model it in Pratt. If the production's main complexity is **structural or context-sensitive**, keep it in the recursive-descent grammar layer."

### 1.2 Expression Category Preservation

The HLD correctly maintains the three expression categories from `elixir_parser.yrl`:
- `matched_expr` - Safe, unambiguous expressions
- `unmatched_expr` - Expressions carrying do-blocks
- `no_parens_expr` - Ambiguous no-parentheses calls

This is **critical** because the grammar uses these distinctions to resolve ambiguity. Flattening them (as Spitfire did) leads to combinatorial flag explosion.

### 1.3 Error Node Model

The `{:__error__, meta, payload}` structure is appropriate:
- Preserves location information
- Allows tooling to inspect error details
- Maintains AST walkability
- Compatible with existing Elixir tooling patterns

### 1.4 Token Stream Abstraction

Using Toxic's native API without a legacy fallback is correct. The HLD properly identifies the key APIs:
- `next/1`, `peek/1`, `peek_n/2`
- `checkpoint/1`, `rewind_to/2`
- `current_terminators/1`, `peek_missing_terminator/1`

---

## 2. Issues Requiring Correction

### 2.1 Precedence Table Verification Needed

**Issue:** The HLD references deriving binding powers from `elixir_parser.yrl` but doesn't include the actual table. The reviewer comment notes:
> "_Reviewer comment: peek_n should not be needed to implement LALR(1) grammar_"

**Analysis:** This comment is partially correct but misleading. While the *grammar* is LALR(1), practical parsing decisions benefit from lookahead:
- Distinguishing `f [a: 1]` (call with keyword list) from `[a: 1]` (bare list)
- Detecting do-block attachment
- Resolving `kw_identifier` vs `do` block disambiguation

**Recommendation:**
1. Include explicit binding power table derived from lines 69-97 of `elixir_parser.yrl`
2. Document where `peek_n` is actually needed vs where `peek` suffices
3. Add mapping from yrl precedence numbers to Pratt binding powers

**Proposed Binding Power Table:**

```elixir
# From elixir_parser.yrl precedences (higher = tighter binding)
@binding_powers %{
  # Left 5 do
  do: {5, 6},

  # Right 10 stab_op_eol (->)
  :->: {11, 10},

  # Left 20 ','
  :",": {20, 21},

  # Left 40 in_match_op_eol (<-, \\)
  :<-: {40, 41},
  :\\: {40, 41},

  # Right 50 when_op_eol
  :when: {51, 50},

  # Right 60 type_op_eol (::)
  :"::": {61, 60},

  # Right 70 pipe_op_eol (|)
  :|: {71, 70},

  # Right 80 assoc_op_eol (=>)
  :"=>": {81, 80},

  # Nonassoc 90 capture_op_eol (&), ellipsis_op (...)
  :&: {90, 90},
  :...: {90, 90},

  # Right 100 match_op_eol (=)
  :=: {101, 100},

  # Left 120 or_op_eol (||, |||, or)
  :||: {120, 121},
  :|||: {120, 121},
  :or: {120, 121},

  # Left 130 and_op_eol (&&, &&&, and)
  :&&: {130, 131},
  :&&&: {130, 131},
  :and: {130, 131},

  # Left 140 comp_op_eol (==, !=, =~, ===, !==)
  :==: {140, 141},
  :!=: {140, 141},
  :=~: {140, 141},
  :===: {140, 141},
  :!==: {140, 141},

  # Left 150 rel_op_eol (<, >, <=, >=)
  :<: {150, 151},
  :>: {150, 151},
  :<=: {150, 151},
  :>=: {150, 151},

  # Left 160 arrow_op_eol (|>, ~>, <~, <<<, >>>, etc.)
  :|>: {160, 161},
  :~>: {160, 161},
  :<~: {160, 161},
  :<<<: {160, 161},
  :>>>: {160, 161},
  :~>>: {160, 161},
  :<<~: {160, 161},
  :<~>: {160, 161},
  :"<|>": {160, 161},

  # Left 170 in_op_eol (in, not in)
  :in: {170, 171},
  :"not in": {170, 171},

  # Left 180 xor_op_eol (^^^)
  :^^^: {180, 181},

  # Right 190 ternary_op_eol (//)
  ://: {191, 190},

  # Right 200 concat_op_eol (++, --, +++, ---, <>)
  :++: {201, 200},
  :--: {201, 200},
  :+++: {201, 200},
  :---: {201, 200},
  :<>: {201, 200},

  # Right 200 range_op_eol (..)
  :..: {201, 200},

  # Left 210 dual_op_eol (+, -)
  :+: {210, 211},
  :-: {210, 211},

  # Left 220 mult_op_eol (*, /)
  :*: {220, 221},
  :/: {220, 221},

  # Left 230 power_op_eol (**)
  :**: {230, 231},

  # Nonassoc 300 unary_op_eol (+, -, !, ^, not, ~~~)
  :unary: {300, 300},

  # Left 310 dot_call_op, dot_op (.)
  :.: {310, 311},

  # Nonassoc 320 at_op_eol (@)
  :@: {320, 320},

  # Nonassoc 330 dot_identifier (for access resolution)
  :access: {330, 330},
}
```

### 2.2 Context Mode Clarification

**Issue:** The reviewer comment states:
> "_Reviewer comment: elixir contexts are (none, match, guard) but I'm not sure they belong in the parser. This is higher level concept on the AST macro expand level_"

**Analysis:** This conflates two different "context" concepts:
1. **Grammar context** (`matched`/`unmatched`/`no_parens`) - **Must** be in parser
2. **Semantic context** (`match`/`guard`/`none`) - Can be deferred to later phases

The HLD mentions both but doesn't clearly separate them.

**Recommendation:** Rename or clarify:
- Use "expression category" for `matched`/`unmatched`/`no_parens` (parser-level)
- Use "semantic mode" for `match`/`guard`/`value` (can be post-parse analysis)
- Make clear that expression categories are required for correct parsing, while semantic modes are optional parser hints for better error messages

### 2.3 EOE Handling Underspecified

**Issue:** The HLD mentions EOE abstraction but doesn't fully specify behavior.

**Analysis:** From `elixir_parser.yrl`:
```
eoe -> eol : '$1'.
eoe -> ';' : '$1'.
eoe -> eol ';' : '$1'.
```

And critically, newline handling varies by context:
- Inside containers: newlines are generally insignificant
- After operators (via `*_op_eol`): newlines are absorbed
- Between expressions: newlines act as separators

**Recommendation:** Add explicit EOE consumption rules:

```elixir
# EOE consumption contexts
@eoe_contexts %{
  # Full EOE (newline or semicolon terminates)
  :expr_list => [:eol, :";"],

  # Operator continuation (newline absorbed after operator)
  :after_operator => [:eol],

  # Container (newlines mostly insignificant)
  :container => [],  # no EOE significance

  # Block body (newline or semicolon between expressions)
  :block_body => [:eol, :";"],
}

# After binary operators, consume optional EOL
defp parse_binary_rhs(parser, op, left, rbp) do
  parser = skip_optional_eol(parser)  # Handle *_op_eol
  {parser, right} = parse_expression(parser, rbp, :matched)
  build_binary(op, left, right)
end
```

### 2.4 Stab Parsing Complexity

**Issue:** The HLD doesn't adequately address stab (`->`) parsing complexity.

**Analysis:** From `elixir_parser.yrl`, stab clauses have multiple entry points:
- `stab_expr -> expr` (expression without arrow)
- `stab_expr -> stab_op_eol_and_expr` (bare arrow clause)
- `stab_expr -> empty_paren stab_op_eol_and_expr` (empty args)
- `stab_expr -> call_args_no_parens_all stab_op_eol_and_expr`
- `stab_expr -> stab_parens_many stab_op_eol_and_expr`
- Plus `when` guard variants

**Recommendation:** Add explicit stab parsing section:

```elixir
defp parse_stab_clause(parser) do
  # Check for stab patterns:
  # 1. () -> body
  # 2. (args) -> body
  # 3. (args) when guard -> body
  # 4. args -> body (no parens)
  # 5. args when guard -> body
  # 6. -> body (bare, only in fn)

  case peek_stab_form(parser) do
    :empty_paren_stab -> parse_empty_paren_stab(parser)
    :paren_stab -> parse_paren_stab(parser)
    :no_paren_stab -> parse_no_paren_stab(parser)
    :bare_stab -> parse_bare_stab(parser)
    :expression -> parse_expr(parser)
  end
end
```

---

## 3. Missing Elements

### 3.1 Event Log Architecture

**Gap:** The HLD mentions "Optional Prism-style event log" but doesn't specify the schema or when events are emitted.

**Why It Matters:** Event logs enable:
- Lossless CST construction
- Incremental reparsing
- Multiple tree projections (AST, outline, etc.)
- Better error recovery (events can be replayed)

**Recommendation:** Add event specification:

```elixir
@type event ::
  {:start_node, node_kind :: atom(), start_pos :: position()}
  | {:end_node, node_kind :: atom(), end_pos :: position()}
  | {:token, token()}
  | {:error, error_kind :: atom(), message :: binary(), span :: range()}
  | {:missing, expected :: atom(), at_pos :: position()}
  | {:synthetic, token_kind :: atom(), at_pos :: position()}

# Example event stream for `1 + 2`:
# {:start_node, :binary_op, {1, 1}}
# {:token, {:int, {{1,1},{1,2},1}, '1'}}
# {:token, {:dual_op, {{1,3},{1,4},nil}, :+}}
# {:token, {:int, {{1,5},{1,6},2}, '2'}}
# {:end_node, :binary_op, {1, 6}}
```

### 3.2 Toxic Structural Token Handling

**Gap:** The HLD doesn't specify how to handle Toxic's synthesized structural tokens.

**Why It Matters:** In tolerant mode, Toxic may emit:
- Synthesized closing delimiters (`}`, `)`, `]`, `>>`, `end`)
- Identifier sanitization tokens
- Error tokens with recovery hints

**Recommendation:** Add structural token handling:

```elixir
# When Toxic synthesizes a closer:
defp handle_synthesized_closer(parser, {:synthetic_closer, meta, closer}) do
  # 1. Record that closer was synthesized (for diagnostics)
  parser = add_diagnostic(parser, :missing_closer, closer, meta)

  # 2. Treat it as the real closer for parsing purposes
  # 3. Mark the enclosing node as having synthetic end

  {parser, {:closer, meta, closer, :synthetic}}
end

# Detection helper
defp is_synthetic?(token) do
  case token do
    {_, meta, _} -> Keyword.get(meta, :synthesized, false)
    {_, meta} -> Keyword.get(meta, :synthesized, false)
    _ -> false
  end
end
```

### 3.3 String/Sigil/Heredoc Reconstruction

**Gap:** The HLD mentions "Strings/sigils/heredocs reconstruction" but doesn't specify the algorithm.

**Why It Matters:** Toxic linearizes strings into fragments. The parser must reassemble them correctly:

```elixir
# Toxic emits:
# {:bin_string_start, meta, ?"}
# {:string_fragment, meta, "hello "}
# {:begin_interpolation, meta, :string}
# {:identifier, meta, :name}
# {:end_interpolation, meta, :string}
# {:string_fragment, meta, "!"}
# {:bin_string_end, meta, ?"}

# Parser must produce:
# {:<<>>, meta, [
#   "hello ",
#   {:"::", [], [
#     {{:., [], [Kernel, :to_string]}, [from_interpolation: true], [{:name, [], nil}]},
#     {:binary, [], nil}
#   ]},
#   "!"
# ]}
```

**Recommendation:** Add string parsing specification:

```elixir
defp parse_string(parser, start_token) do
  {:bin_string_start, start_meta, delimiter} = start_token
  parser = advance(parser)

  {parser, parts} = parse_string_parts(parser, delimiter, [])

  {parser, end_meta} = expect_string_end(parser, delimiter)

  case parts do
    [binary] when is_binary(binary) ->
      # Simple string, return as literal
      {parser, binary}

    parts ->
      # Interpolated string, build binary concat AST
      {parser, build_interpolated_string(parts, merge_meta(start_meta, end_meta))}
  end
end

defp parse_string_parts(parser, delimiter, acc) do
  case current_token(parser) do
    {:string_fragment, _meta, binary} ->
      parser = advance(parser)
      parse_string_parts(parser, delimiter, [binary | acc])

    {:begin_interpolation, _meta, _kind} ->
      parser = advance(parser)
      {parser, expr} = parse_expr(parser)
      parser = expect_end_interpolation(parser)
      interp = build_to_string_call(expr)
      parse_string_parts(parser, delimiter, [interp | acc])

    {:bin_string_end, _, ^delimiter} ->
      {parser, Enum.reverse(acc)}

    {:error_token, meta, error} ->
      # Handle unterminated string
      error_node = build_error_node(:unterminated_string, meta, error)
      {parser, Enum.reverse([error_node | acc])}

    _ ->
      # Unexpected token, recover
      {parser, Enum.reverse(acc)}
  end
end
```

### 3.4 `not in` Combined Operator

**Gap:** The HLD mentions combined operators but doesn't specify `not in` handling.

**Why It Matters:** `not in` is a special case where two tokens form one operator. Toxic emits:
```elixir
{:in_op, meta_span, :"not in", in_meta}
```

**Recommendation:** Add combined operator handling:

```elixir
# Toxic handles "not in" as a single token with extra metadata
defp parse_led_in_op(parser, left, {:in_op, meta, :"not in", in_meta}) do
  parser = advance(parser)
  parser = skip_optional_eol(parser)

  {parser, right} = parse_expression(parser, @in_op_rbp, :matched)

  # Build: {:not, not_meta, [{:in, in_meta, [left, right]}]}
  in_node = {:in, in_meta, [left, right]}
  not_node = {:not, meta, [in_node]}

  {parser, not_node}
end
```

### 3.5 Capture and Capture-Int Handling

**Gap:** The HLD doesn't specify `&` and `&N` handling.

**Analysis:** From Toxic tokens:
- `{:capture_op, meta, :&}` - Capture operator
- `{:capture_int, meta, :&}` - Capture-arity marker (followed by `:int` token)

**Recommendation:**

```elixir
defp parse_nud_capture(parser, {:capture_op, meta, :&}) do
  parser = advance(parser)

  case current_token(parser) do
    {:int, int_meta, chars} when is_capture_arity?(parser) ->
      # &1, &2, etc. - Capture argument reference
      parser = advance(parser)
      value = parse_integer_value(chars)
      {parser, {:&, meta, [value]}}

    _ ->
      # &expr - Capture expression
      {parser, expr} = parse_expression(parser, @capture_rbp, :matched)
      {parser, {:&, meta, [expr]}}
  end
end

# Detect capture-arity vs capture-expression
defp is_capture_arity?(parser) do
  # Check if &N where N is integer with no space between
  case current_token(parser) do
    {:int, {{_, col1}, _, _}, _} ->
      {_, {{_, col0}, _, _}, _} = previous_token(parser)
      col1 == col0 + 1  # No space
    _ ->
      false
  end
end
```

### 3.6 Range and Step Operator (`..` and `//`)

**Gap:** The HLD doesn't specify the `..` / `//` interaction.

**Analysis:** From `elixir_parser.yrl`:
```
build_op(AST, {_Kind, Location, '//'}, Right) ->
  case AST of
    {'..', Meta, [Left, Middle]} ->
      {'..//', Meta, [Left, Middle, Right]};
    ...
```

**Recommendation:**

```elixir
# Special handling for range step operator
defp parse_led_ternary(parser, left, {:ternary_op, meta, ://}) do
  case left do
    {:.., range_meta, [start, stop]} ->
      # This is a range step: start..stop//step
      parser = advance(parser)
      parser = skip_optional_eol(parser)
      {parser, step} = parse_expression(parser, @ternary_rbp, :matched)
      {parser, {:"..//", range_meta, [start, stop, step]}}

    _ ->
      # Error: // must follow ..
      error = build_error_node(:invalid_step_operator, meta,
        "// must immediately follow ..")
      parser = add_error(parser, error)
      {parser, {:__error__, meta, [partial: left, operator: ://]}}
  end
end
```

---

## 4. Questions Requiring Answers

### 4.1 Architecture Questions

1. **Event log vs direct AST build:** Should we always emit events, or make it configurable?
   - **Recommendation:** Always emit events internally; tree construction subscribes to event stream. This enables future incremental parsing without API changes.

2. **Green tree / CST:** Should we build a lossless syntax tree alongside the AST?
   - **Recommendation:** Defer to post-v1. Design event log to support it, but don't implement initially.

3. **Comment handling:** Attach to AST nodes or separate list?
   - **Recommendation:** Configurable via Toxic's `preserve_comments` option. When enabled, accumulate comments and attach to nearest following node.

### 4.2 Error Recovery Questions

4. **How deep should recovery go?** When do we stop trying and emit a top-level error?
   - **Recommendation:** Define "recovery budget" per construct. After N failed recovery attempts at same position, escalate to parent.

5. **Synthetic token ranges:** What range should synthesized tokens have?
   - **Recommendation:** Zero-width range at the position where they would have been expected. Set `synthesized: true` in meta.

6. **Multiple errors in same expression:** Report all or just first?
   - **Recommendation:** Report all, but with diminishing detail after first 3-5 in same construct.

### 4.3 Compatibility Questions

7. **Strict mode:** Should we support a mode that errors exactly like core parser?
   - **Recommendation:** Yes, for testing. Default to tolerant mode.

8. **AST metadata compatibility:** Exact match with `Code.string_to_quoted` metadata?
   - **Recommendation:** Superset. Include all core metadata plus extended fields (ranges, closing positions, etc.).

### 4.4 Performance Questions

9. **Lookahead depth:** What's the maximum `peek_n` needed?
   - **Recommendation:** Document actual usage. Likely max 3 tokens. Consider implementing as ring buffer.

10. **Memory budget:** What's acceptable peak memory for large files?
    - **Recommendation:** Target < 3x input size. Event log should be streamable/discardable.

---

## 5. Industry Best Practices Alignment

### 5.1 Prism (Ruby) Alignment

| Prism Feature | HLD Coverage | Gap |
|---------------|--------------|-----|
| Hand-written recursive descent | Yes | - |
| Error tokens as first-class | Yes | - |
| Serialization format | No | Not needed for Elixir |
| Multi-implementation support | No | Single implementation OK |
| Event-based building | Mentioned | Underspecified |
| Comprehensive error recovery | Yes | Needs sync set refinement |

### 5.2 Ruff (Python) Alignment

| Ruff Feature | HLD Coverage | Gap |
|--------------|--------------|-----|
| Hand-written parser | Yes | - |
| Performance focus | Mentioned | No specific targets |
| Better error messages | Implicit | Should be explicit goal |
| Foundation for recovery | Yes | - |
| Single-pass | Yes | - |

### 5.3 railsatscale Lessons

| Lesson | HLD Coverage | Gap |
|--------|--------------|-----|
| Hand-written over generated | Yes | - |
| Context-sensitive grammar | Yes | - |
| Token insertion recovery | Yes | - |
| Maintainability priority | Implicit | Should be explicit |

---

## 6. Concrete Improvements

### 6.1 Add to Section 3 (Token Stream)

Add explicit token classification helpers:

```elixir
# Token predicates for grammar decisions
defp is_expression_start?(token) do
  case token do
    {:int, _, _} -> true
    {:flt, _, _} -> true
    {:atom, _, _} -> true
    {:identifier, _, _} -> true
    {:paren_identifier, _, _} -> true
    # ... complete list
    _ -> false
  end
end

defp is_operator?(token) do
  case token do
    {:dual_op, _, _} -> true
    {:mult_op, _, _} -> true
    # ... complete list
    _ -> false
  end
end

defp is_closer?(token) do
  token_type(token) in [:")", :"]", :"}", :">>" , :end]
end
```

### 6.2 Add to Section 4 (Expression Engine)

Add concrete parselet dispatch:

```elixir
# NUD dispatch table
@nud_parselets %{
  :int => &parse_nud_number/2,
  :flt => &parse_nud_number/2,
  :atom => &parse_nud_atom/2,
  :identifier => &parse_nud_identifier/2,
  :paren_identifier => &parse_nud_paren_call/2,
  :bracket_identifier => &parse_nud_bracket_access/2,
  :do_identifier => &parse_nud_do_call/2,
  :alias => &parse_nud_alias/2,
  :fn => &parse_nud_fn/2,
  :"(" => &parse_nud_paren/2,
  :"[" => &parse_nud_list/2,
  :"{" => &parse_nud_tuple/2,
  :%{} => &parse_nud_map/2,
  :% => &parse_nud_struct/2,
  :"<<" => &parse_nud_bitstring/2,
  :unary_op => &parse_nud_unary/2,
  :dual_op => &parse_nud_unary/2,  # When prefix
  :at_op => &parse_nud_at/2,
  :capture_op => &parse_nud_capture/2,
  :range_op => &parse_nud_nullary_range/2,
  :ellipsis_op => &parse_nud_nullary_ellipsis/2,
  :bin_string_start => &parse_nud_string/2,
  :list_string_start => &parse_nud_charlist/2,
  :sigil_start => &parse_nud_sigil/2,
  # ... etc
}

# LED dispatch (by operator type, not token type)
@led_parselets %{
  :binary_op => &parse_led_binary/3,
  :dot_op => &parse_led_dot/3,
  :dot_call_op => &parse_led_dot_call/3,
  :pipe_op => &parse_led_pipe/3,
  :in_op => &parse_led_in/3,
  :match_op => &parse_led_match/3,
  :range_op => &parse_led_range/3,
  :ternary_op => &parse_led_ternary/3,
  :access => &parse_led_access/3,  # [...] after expression
  :call => &parse_led_call/3,      # (...) after expression
  # ... etc
}
```

### 6.3 Add to Section 6 (Error Tolerance)

Add structured sync sets with examples:

```elixir
@sync_sets %{
  # Expression list: recover at expression boundaries
  expr_list: %{
    tokens: [:eol, :";", :end, :eof],
    skip_eol: false,
    example: "def a do\n  @#@#\n  valid()\nend"
  },

  # Container args: recover at separator or closer
  container_args: %{
    tokens: [:",", :"]", :"}", :">>", :end, :eof],
    skip_eol: true,
    example: "[1, @#@#, 3]"
  },

  # Do block: recover at clause keywords or end
  do_block: %{
    tokens: [:else, :catch, :rescue, :after, :end, :eof],
    skip_eol: true,
    example: "if true do\n  @#@#\nelse\n  ok\nend"
  },

  # Function arguments
  call_args: %{
    tokens: [:",", :")", :eof],
    skip_eol: true,
    example: "foo(1, @#@#, 3)"
  },

  # Stab clauses
  stab: %{
    tokens: [:->, :eol, :";", :end, :eof],
    skip_eol: false,
    example: "fn @#@# -> :ok end"
  },

  # Map/struct
  map_args: %{
    tokens: [:",", :"=>", :"}", :eof],
    skip_eol: true,
    example: "%{a: 1, @#@#, b: 2}"
  },
}
```

### 6.4 Add New Section: IDE Integration API

```elixir
@doc """
IDE-oriented result structure
"""
defmodule ParseResult do
  defstruct [
    :ast,           # Core-compatible AST with error nodes
    :errors,        # List of parser errors
    :warnings,      # List of parser warnings (ambiguity, deprecation)
    :comments,      # Preserved comments (if enabled)
    :tokens,        # Token list (if requested)
    :events,        # Event log (if enabled)
    :terminators,   # Final terminator stack (for completion)
    :source_map,    # Node -> source range mapping
  ]
end

@doc """
Entry points for different use cases
"""
def parse(source, opts \\ [])
def parse_expression(source, opts \\ [])
def parse_pattern(source, opts \\ [])

@doc """
Incremental parsing support
"""
def reparse_region(prev_result, edit, opts \\ [])

@doc """
Completion support
"""
def parse_for_completion(source, cursor_position, opts \\ [])
```

---

## 7. Implementation Priority

Based on the review, recommended implementation order:

### Phase 1: Foundation (Critical Path)
1. Parser state struct with Toxic integration
2. EOE handling (centralized)
3. Expression category framework (`matched`/`unmatched`/`no_parens`)
4. Basic error node structure

### Phase 2: Expression Core
5. Binding power table (verified against yrl)
6. Pratt loop with NUD/LED dispatch
7. Basic binary operators
8. Unary operators (including space-sensitive `+`/`-`)

### Phase 3: Structural Constructs
9. Containers (list, tuple, map, bitstring)
10. Access syntax (`expr[key]`)
11. Call syntax (parens, no-parens, do-block)
12. Dot and dot-call

### Phase 4: Complex Constructs
13. Do-blocks with clause lists
14. fn expressions
15. Stab clauses (all variants)
16. Guards (`when`)

### Phase 5: Strings and Sigils
17. Binary strings with interpolation
18. Charlists
19. Heredocs
20. Sigils
21. Quoted atoms and identifiers

### Phase 6: Error Recovery
22. Sync set implementation
23. Missing token insertion
24. Missing node insertion
25. Toxic error token handling
26. Synthetic closer handling

### Phase 7: Polish
27. Event log (optional)
28. Comprehensive metadata
29. Performance optimization
30. Test coverage

---

## 8. Conclusion

HLD_DRAFT_1.md provides a solid foundation for an error-tolerant Elixir parser. The hybrid recursive descent + Pratt architecture is the correct choice, and the emphasis on grammar fidelity will prevent the issues that plagued Spitfire.

**Key actions needed:**
1. Add explicit binding power table with verification against yrl
2. Clarify expression category vs semantic mode distinction
3. Specify EOE handling rules
4. Add string/sigil reconstruction algorithm
5. Define event log schema
6. Document Toxic structural token handling
7. Add sync set specifications with examples

With these additions, the HLD will be implementation-ready for the building blocks team.

---

*End of Review*
