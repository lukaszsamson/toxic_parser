# ToxicParser Refactoring Analysis (Opus)

This document identifies code quality issues, duplication, unnecessary complexity, and refactoring opportunities in the ToxicParser codebase.

---

## Executive Summary

The ToxicParser implementation is generally well-structured with clear separation of concerns. However, there are significant opportunities for improvement:

1. **High code duplication** - Several patterns are repeated across 5-10 files
2. **Oversized modules** - `pratt.ex` (1940 lines) and `containers.ex` (1751 lines) need splitting
3. **Inconsistent patterns** - Similar operations use different approaches in different files
4. **Dead/redundant code** - Some code branches appear unreachable
5. **Missing abstractions** - Common patterns lack shared helpers

---

## 1. Severe Code Duplication

### 1.1 `skip_eoe_count_newlines/2` Duplicated in 6 Files

**Files containing identical or near-identical implementations:**
- `lib/toxic_parser/pratt.ex` (lines 1463-1472)
- `lib/toxic_parser/grammar/calls.ex` (lines 358-366)
- `lib/toxic_parser/grammar/containers.ex` (lines 1514-1523)
- `lib/toxic_parser/grammar/blocks.ex` (lines 66-74)
- `lib/toxic_parser/grammar/maps.ex` (lines 603-612)

**Identical code:**
```elixir
defp skip_eoe_count_newlines(state, count) do
  case TokenAdapter.peek(state) do
    {:ok, %{kind: :eoe, value: %{newlines: n}}, _} ->
      {:ok, _eoe, state} = TokenAdapter.next(state)
      skip_eoe_count_newlines(state, count + n)
    _ ->
      {state, count}
  end
end
```

**Recommendation:** Extract to `ToxicParser.Helpers` module with `@spec skip_eoe_count_newlines(State.t(), non_neg_integer()) :: {State.t(), non_neg_integer()}`.

---

### 1.2 `skip_eoe/1` Duplicated in 6 Files

**Files containing identical implementations:**
- `lib/toxic_parser/grammar/expressions.ex` (lines 183-191)
- `lib/toxic_parser/grammar/containers.ex` (lines 1280-1288)
- `lib/toxic_parser/grammar/keywords.ex` (lines 146-154)
- `lib/toxic_parser/grammar/maps.ex` (lines 592-600)

**Identical code:**
```elixir
defp skip_eoe(state) do
  case TokenAdapter.peek(state) do
    {:ok, %{kind: :eoe}, _} ->
      {:ok, _eoe, state} = TokenAdapter.next(state)
      skip_eoe(state)
    _ ->
      state
  end
end
```

**Recommendation:** Add to proposed `ToxicParser.Helpers` module.

---

### 1.3 `token_meta/1` Duplicated in 8 Files

**Files containing identical implementations:**
- `lib/toxic_parser/pratt.ex` (multiple variants)
- `lib/toxic_parser/grammar/containers.ex` (lines 1307-1310)
- `lib/toxic_parser/grammar/blocks.ex` (lines 114-117)
- `lib/toxic_parser/grammar/calls.ex` (via Builder.Helpers)
- `lib/toxic_parser/grammar/maps.ex` (lines 623-626)
- `lib/toxic_parser/grammar/strings.ex` (lines 508-512)

**Identical code:**
```elixir
defp token_meta(%{range: %{start: %{line: line, column: column}}}),
  do: [line: line, column: column]
defp token_meta(_), do: []
```

**Recommendation:** Already exists in `Builder.Helpers.token_meta/1` - replace all local definitions with calls to the shared helper.

---

### 1.4 `build_eoe_meta/1` Duplicated in 3 Files

**Files:**
- `lib/toxic_parser/grammar/expressions.ex` (lines 233-251)
- `lib/toxic_parser/grammar/containers.ex` (lines 1044-1063)
- `lib/toxic_parser/grammar/blocks.ex` (lines 244-262)

**Nearly identical code pattern for building end-of-expression metadata.**

**Recommendation:** Extract to shared helper.

---

### 1.5 `annotate_eoe/2` Duplicated in 3 Files

**Files:**
- `lib/toxic_parser/grammar/expressions.ex` (lines 289-297)
- `lib/toxic_parser/grammar/containers.ex` (lines 1066-1070)
- `lib/toxic_parser/grammar/blocks.ex` (lines 266-270)

**Recommendation:** Extract to shared helper.

---

### 1.6 `can_be_no_parens_arg?/1` vs `is_no_parens_arg?/1` - Same Logic, Different Names

**Files:**
- `lib/toxic_parser/pratt.ex`: `is_no_parens_arg?/1` (lines 769-804) and `can_be_no_parens_arg?/1` (lines 1326-1365)
- `lib/toxic_parser/grammar/calls.ex`: `can_be_no_parens_arg?/1` (lines 231-265)

These contain nearly identical token kind lists. The lists differ slightly:
- `pratt.ex:is_no_parens_arg?` includes `:ellipsis_op`
- `pratt.ex:can_be_no_parens_arg?` also includes `:ellipsis_op`
- `calls.ex:can_be_no_parens_arg?` does NOT include `:ellipsis_op`

**Issues:**
1. Code duplication
2. Inconsistent handling of `:ellipsis_op`
3. Confusing naming (is_ vs can_be_)

**Recommendation:** Create single `ToxicParser.Helpers.no_parens_arg_kinds/0` returning the canonical list, with a single `can_be_no_parens_arg?/1` helper.

---

## 2. Oversized Modules - Split Recommendations

### 2.1 `pratt.ex` - 1940 Lines

This module combines too many responsibilities:

| Responsibility | Lines | Suggested Module |
|---------------|-------|------------------|
| Core Pratt loop (`parse`, `led`, `nud`) | ~400 | Keep in `Pratt` |
| Binary operator handling | ~200 | Keep in `Pratt` |
| Unary operator handling | ~150 | Keep in `Pratt` |
| Paren/bracket access handling | ~300 | `Pratt.Access` or move to `Containers` |
| Dot operator handling | ~350 | Move to `Dots` module |
| No-parens call handling | ~200 | Move to `Calls` |
| Helper functions (EOE, meta building) | ~150 | `ToxicParser.Helpers` |
| `parse_access_indices` | ~150 | `Containers` or new module |

**Specific extractions:**

1. **Lines 811-1182** - The `led/5` function is 370+ lines. Extract nested cases to helper functions.

2. **Lines 1668-1815** - `parse_access_indices` and related functions should move to a dedicated module or `Containers`.

3. **Lines 1820-1938** - `led_dot_only` and `led_dots_and_calls` duplicate patterns from the main `led` - extract shared logic.

---

### 2.2 `containers.ex` - 1751 Lines

This module handles parentheses, lists, tuples, AND stab expressions. The stab parsing alone is ~800 lines.

**Split recommendation:**

| Responsibility | Lines | Suggested Module |
|---------------|-------|------------------|
| Paren stab expressions | ~650 | `Grammar.Stabs` |
| List parsing | ~200 | Keep in `Containers` |
| Tuple parsing | ~250 | Keep in `Containers` |
| Stab body parsing | ~150 | `Grammar.Stabs` |
| Container helpers | ~100 | Keep in `Containers` |

**Specific issues:**

1. **`try_parse_stab_clause`** (lines 535-637) - 100+ lines, complex branching
2. **`parse_remaining_stab_clauses`** (lines 1074-1103) - Duplicates logic from `parse_stab_eoe_until`
3. **`parse_tuple_quoted_kw_continuation`** (lines 1655-1736) - 80 lines of deeply nested case statements

---

## 3. Logic Issues and Dubious Code

### 3.1 Unreachable Code Branch in `pratt.ex`

**Location:** `pratt.ex` lines 106-113

```elixir
def parse_with_min_bp(%State{} = state, context, %EventLog{} = log, min_bp) do
  with {:ok, token, state} <- TokenAdapter.next(state),
       {:ok, left, state, log} <- nud_with_min_bp(token, state, context, log, min_bp) do
    led(left, state, log, min_bp, context)
  else
    {:eof, state} -> {:error, :unexpected_eof, state, log}
    {:error, diag, state} -> {:error, diag, state, log}
    # Handle 4-tuple errors from nested calls (e.g., Maps.parse_map)
    {:error, diag, state, log} -> {:error, diag, state, log}  # <-- UNREACHABLE
    ...
  end
end
```

The 4-tuple `{:error, diag, state, log}` will never match because the 3-tuple `{:error, diag, state}` pattern matches first.

**Fix:** Remove the 4-tuple pattern or reorder to make 4-tuple first.

---

### 3.2 Duplicate Pattern Matching in `calls.ex`

**Location:** `calls.ex` lines 57-84

```elixir
cond do
  kind == :op_identifier ->
    parse_op_identifier_call(tok, state, ctx, log)
  Pratt.bp(next_tok.kind) != nil or next_tok.kind in [:dot_op, :dot_call_op] ->
    ...
  can_be_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok) ->
    parse_no_parens_call(tok, state, ctx, log)
  kind == :do_identifier and ctx == :matched ->  # <-- kind already checked above
    ...
```

The check for `kind == :do_identifier` is partly redundant since `op_identifier` was already handled. However, the logic flow is correct - this is more of a readability issue.

**Recommendation:** Add comment explaining the flow or restructure with explicit case matching.

---

### 3.3 Inconsistent Error Tuple Arities

The codebase uses both 3-tuple and 4-tuple error returns inconsistently:

- **3-tuple:** `{:error, diag, state}` - Used by `TokenAdapter`
- **4-tuple:** `{:error, diag, state, log}` - Used by grammar modules

This leads to awkward pattern matching in multiple places:

```elixir
# pratt.ex line 278
{:error, diag, state} ->
  {:error, diag, state, log}
```

**Recommendation:** Standardize on 4-tuple errors throughout, or create a `Result` type that handles both.

---

### 3.4 Magic Numbers for `min_bp`

Several places use magic numbers for binding power:

```elixir
# containers.ex line 13-16
@stab_pattern_min_bp 11  # Good - documented constant

# But elsewhere:
Pratt.parse_with_min_bp(state, :matched, log, 51)  # Why 51?
Pratt.parse_with_min_bp(state, :matched, log, 71)  # Why 71?
```

**Recommendation:** Define named constants for all min_bp values:

```elixir
# In Precedence module:
@stab_op_bp 10
@when_op_bp 50
@pipe_op_bp 70

# Usage:
Pratt.parse_with_min_bp(state, :matched, log, @when_op_bp + 1)
```

---

### 3.5 Keyword List Result Guard is Fragile

**Location:** Multiple files use this guard:

```elixir
defguardp is_keyword_list_result(arg)
          when is_list(arg) and length(arg) > 0
```

This only checks if the result is a non-empty list, not that it's actually a keyword list. The actual check should verify the structure:

```elixir
defp is_keyword_list?([{key, _value} | rest]) when is_atom(key), do: is_keyword_list?(rest)
defp is_keyword_list?([{_ast, _value} | rest]), do: is_keyword_list?(rest)  # For interpolated keys
defp is_keyword_list?([]), do: true
defp is_keyword_list?(_), do: false
```

Note: `containers.ex` does have a proper `is_keyword_list?/1` function (lines 1458-1460), but the guard is still used in other places.

**Recommendation:** Replace guard with function call where type safety matters.

---

## 4. Inconsistent Patterns

### 4.1 Context Parameter Naming

The "context" parameter is named differently across modules:

- `ctx` - Used in `containers.ex`, `calls.ex`, `keywords.ex`
- `context` - Used in `pratt.ex`
- `_ctx` - Unused in many places

**Recommendation:** Standardize on `ctx` (shorter) and remove unused parameters.

---

### 4.2 Checkpoint/Rewind Pattern

The checkpoint pattern is used inconsistently:

```elixir
# Pattern 1 (containers.ex):
{ref, checkpoint_state} = TokenAdapter.checkpoint(state)
...
state = TokenAdapter.rewind(checkpoint_state, ref)

# Pattern 2 (maps.ex):
{checkpoint_id, state} = TokenAdapter.checkpoint(state)
...
state = TokenAdapter.rewind(state, checkpoint_id)
```

The first pattern creates a new binding for the checkpoint state, while the second reuses the same binding.

**Recommendation:** Standardize on Pattern 1 for clarity.

---

### 4.3 EOE Skip Timing

Some functions skip EOE at the start, others at the end:

```elixir
# Pattern 1: Skip at start
state = skip_eoe(state)
with {:ok, expr, state, log} <- parse_something(state, ...) do

# Pattern 2: Skip after parsing
with {:ok, expr, state, log} <- parse_something(state, ...) do
  state = skip_eoe(state)
```

This inconsistency can lead to double-skipping or missed EOE tokens.

**Recommendation:** Document and enforce consistent convention.

---

## 5. Module Structure Issues

### 5.1 Circular-ish Dependencies

The module dependency graph has complex relationships:

```
Pratt ←→ Containers
Pratt ←→ Calls
Pratt ←→ Blocks
Pratt ←→ Maps
Pratt ←→ Strings
Containers → Pratt (for led)
Calls → Pratt (for led)
Blocks → Containers (for stab parsing)
Expressions → All grammar modules
```

The bidirectional dependency between `Pratt` and grammar modules is necessary but creates coupling.

**Recommendation:** Consider a callback-based architecture where grammar modules register parselets with Pratt.

---

### 5.2 `calls_private.ex` Naming

There's a module `calls_private.ex` that exists solely to expose `parse_paren_args/4`:

```elixir
defmodule ToxicParser.Grammar.CallsPrivate do
  ...
  def parse_paren_args(acc, state, ctx, log) do
    ...
  end
end
```

This is called from:
- `pratt.ex` (lines 734, 831, 891, 979, 1205, 1890)
- `calls.ex` (line 379)

**Issue:** The "Private" naming is misleading - it's actually public but meant for internal use.

**Recommendation:** Either:
1. Rename to `CallsInternal` or `CallsShared`
2. Move `parse_paren_args` to `Calls` and export it
3. Move to a general `Args` or `Parameters` module

---

### 5.3 Missing Module for Common AST Patterns

Several AST building patterns are repeated:

```elixir
# Building access expression - appears in 3+ places
{{:., meta, [Access, :get]}, meta, [subject, key]}

# Building do-block metadata - appears in 5+ places
[do: do_location, end: end_location]

# Building newlines metadata - appears in 10+ places
if newlines > 0, do: [newlines: newlines], else: []
```

**Recommendation:** Extend `Builder.Helpers` with:

```elixir
def access_expr(subject, key, meta)
def do_end_meta(do_tok, end_tok)
def maybe_newlines_meta(count)
```

---

## 6. Performance Considerations

### 6.1 Repeated Token Lookups

Several places peek at the same token multiple times:

```elixir
case TokenAdapter.peek(state) do
  {:ok, %{kind: :","}, _} ->
    {:ok, _comma, state} = TokenAdapter.next(state)
    case TokenAdapter.peek(state) do  # Another peek immediately after
      ...
```

While not a major issue, this could be optimized.

---

### 6.2 List Reversals

Accumulator patterns correctly use `[item | acc]` with final `Enum.reverse`, but some places do:

```elixir
acc ++ [expr]  # O(n) - appears in containers.ex
```

**Locations:**
- `containers.ex` line 1447: `Enum.reverse(acc) ++ [expr]`
- `containers.ex` line 1450: `parse_dot_container_args_loop(acc ++ [expr], ...)`

**Recommendation:** Consistently use `[expr | acc]` with `Enum.reverse` at the end.

---

## 7. Documentation Gaps

### 7.1 Missing @moduledoc or Outdated

Some modules have outdated or missing documentation:

- `calls.ex` line 3: "Phase 5 scaffolding" - Phase 5 is complete
- `containers.ex` line 3: "Phase 6 scaffolding" - Phase 6 is complete

**Recommendation:** Update moduledocs to describe current functionality.

---

### 7.2 Complex Functions Lacking Comments

These functions are complex enough to warrant inline comments:

- `pratt.ex:led/5` - 370+ lines with nested cases
- `containers.ex:try_parse_stab_or_expr/5` - Complex checkpoint/rewind logic
- `maps.ex:try_parse_map_update/3` - Multiple fallback paths

---

## 8. Specific Refactoring Recommendations

### 8.1 High Priority - Create `ToxicParser.Helpers` Module

```elixir
defmodule ToxicParser.Helpers do
  alias ToxicParser.{State, TokenAdapter}
  
  @spec skip_eoe(State.t()) :: State.t()
  def skip_eoe(state) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe(state)
      _ ->
        state
    end
  end
  
  @spec skip_eoe_count_newlines(State.t(), non_neg_integer()) :: 
        {State.t(), non_neg_integer()}
  def skip_eoe_count_newlines(state, count) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, value: %{newlines: n}}, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        skip_eoe_count_newlines(state, count + n)
      _ ->
        {state, count}
    end
  end
  
  @spec token_meta(map()) :: keyword()
  def token_meta(%{range: %{start: %{line: line, column: column}}}),
    do: [line: line, column: column]
  def token_meta(_), do: []
  
  @spec build_eoe_meta(map()) :: keyword()
  def build_eoe_meta(%{kind: :eoe, value: %{newlines: n}, metadata: meta}) 
      when is_integer(n) do
    case meta do
      %{range: %{start: %{line: l, column: c}}} -> [newlines: n, line: l, column: c]
      _ -> []
    end
  end
  def build_eoe_meta(%{kind: :eoe, metadata: meta}) do
    case meta do
      %{range: %{start: %{line: l, column: c}}} -> [line: l, column: c]
      _ -> []
    end
  end
  
  @spec annotate_eoe(Macro.t(), keyword()) :: Macro.t()
  def annotate_eoe({left, meta, right}, eoe_meta) when is_list(meta) do
    {left, [{:end_of_expression, eoe_meta} | meta], right}
  end
  def annotate_eoe(ast, _eoe_meta), do: ast
  
  @spec maybe_newlines_meta(non_neg_integer()) :: keyword()
  def maybe_newlines_meta(0), do: []
  def maybe_newlines_meta(n) when n > 0, do: [newlines: n]
  
  @no_parens_arg_kinds [
    :int, :flt, :char, :atom, :string, :identifier, :do_identifier,
    :paren_identifier, :bracket_identifier, :alias, :bin_string_start,
    :list_string_start, :bin_heredoc_start, :list_heredoc_start, :sigil_start,
    :atom_safe_start, :atom_unsafe_start, true, false, nil, :"{", :"[", :"<<",
    :unary_op, :at_op, :capture_op, :dual_op, :%, :%{}, :fn, :ellipsis_op
  ]
  
  @spec can_start_no_parens_arg?(map()) :: boolean()
  def can_start_no_parens_arg?(%{kind: kind}), do: kind in @no_parens_arg_kinds
end
```

**Estimated impact:** Remove ~200 lines of duplicate code across 8 files.

---

### 8.2 High Priority - Extract Stab Parsing to `Grammar.Stabs`

Move stab-related functions from `containers.ex` to a new module:

```elixir
defmodule ToxicParser.Grammar.Stabs do
  @moduledoc """
  Stab clause parsing for fn expressions and paren stabs.
  
  Grammar rules:
    stab_expr -> stab_op_eol_and_expr
    stab_expr -> empty_paren stab_op_eol_and_expr
    stab_expr -> call_args_no_parens_all stab_op_eol_and_expr
    stab_parens_many -> open_paren call_args_parens close_paren
  """
  
  # Move these functions from containers.ex:
  # - try_parse_stab_clause/4
  # - parse_stab_clause_after_patterns/6
  # - parse_stab_patterns/4
  # - parse_stab_body/4
  # - collect_stab_body_exprs/5
  # - parse_stab_eoe_until/5
  # - etc.
end
```

**Estimated impact:** Reduce `containers.ex` by ~700 lines.

---

### 8.3 Medium Priority - Consolidate Dot Handling

Move dot handling from `pratt.ex` to `dots.ex`:

Currently `dots.ex` only has `parse_member/3` (~50 lines). It should also handle:
- Dot alias building (`build_dot_alias`)
- Dot container parsing (`parse_dot_container_args`)
- Dot call conversion (`dot_to_call_with_meta`, `dot_to_no_parens_call`)
- `led_dot_only` and `led_dots_and_calls`

---

### 8.4 Medium Priority - Named Constants for Binding Powers

```elixir
# In precedence.ex:
@doc "Binding power constants for common reference points"
def stab_op_bp, do: 10
def when_op_bp, do: 50
def pipe_op_bp, do: 70
def assoc_op_bp, do: 80
def match_op_bp, do: 100

# Usage:
min_bp = Precedence.when_op_bp() + 1  # Instead of magic 51
```

---

### 8.5 Low Priority - Consistent Error Types

Create a dedicated error module:

```elixir
defmodule ToxicParser.Error do
  @type t :: 
    :unexpected_eof
    | {:expected, atom()}
    | {:expected, atom(), got: atom()}
    | {:expected_comma_or, atom()}
    | {:unexpected_string_token, atom()}
    
  @spec expected(atom()) :: t()
  def expected(kind), do: {:expected, kind}
  
  @spec expected(atom(), atom()) :: t()
  def expected(kind, got), do: {:expected, kind, got: got}
  
  # etc.
end
```

---

## 9. Testing Recommendations

Based on the code analysis, these areas need additional test coverage:

1. **Checkpoint/rewind paths** - Ensure all rewind paths are tested
2. **Edge cases in stab parsing** - Complex guard combinations
3. **Keyword list merging** - Quoted keywords followed by regular keywords
4. **Error recovery** - The tolerant mode paths

---

## Summary Statistics

| Category | Count |
|----------|-------|
| Duplicate Functions | 8 |
| Oversized Modules (>1000 lines) | 2 |
| Logic Issues | 5 |
| Inconsistent Patterns | 5 |
| Missing Abstractions | 4 |
| **Total Refactoring Items** | **24** |

### Priority Matrix

| Priority | Items | Estimated Lines Saved |
|----------|-------|----------------------|
| High | Create Helpers module, Extract Stabs | ~900 |
| Medium | Consolidate dots, Named constants | ~200 |
| Low | Error types, Documentation | ~50 |

---

*Generated by Claude Opus analysis - December 2024*
