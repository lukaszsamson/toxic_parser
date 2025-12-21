# Review of LEXER.md Design

**Reviewer:** Claude Opus 4.5
**Date:** 2025-12-19

## Executive Summary

The LEXER.md design correctly identifies pain points in the Toxic ↔ ToxicParser interface and proposes an elegant solution: a new cursor-based API that eliminates double-buffering and per-token allocation overhead. The "no-buffer fast path" concept is sound and aligns well with the profiling data.

However, the proposal is **ambitious and high-risk**. It requires coordinated changes across two repositories (Toxic and ToxicParser), touches fundamental interfaces, and has a complex migration path. Given current profiling showing token operations at ~7.5% of runtime, the expected speedup (while real) may not justify the engineering effort unless code clarity is also a goal.

**Verdict:** Sound design, but consider a phased approach that delivers incremental value.

---

## What the Design Gets Right

### 1. Correct Identification of Pain Points

The design accurately diagnoses the current issues:

| Pain Point | Description | Evidence from Profiling |
|------------|-------------|------------------------|
| Double buffering | Toxic queue + TokenAdapter lookahead | `queue:in/out` in hot paths |
| Per-token metadata | Map allocation per token | `Position.to_location/3` calls |
| Terminator snapshots | `current_terminators/1` per token | Process dictionary overhead |
| Checkpoint duplication | Both Toxic and TokenAdapter track state | Redundant bookkeeping |

From `PROFILE_RECOMMENDATIONS.md`, the `:queue` operations and metadata creation are visible in call counts:
- `Position.to_location/3` called for every token (start + end)
- Per-token map construction in `TokenAdapter.metadata/4`

### 2. "No-Buffer Fast Path" is the Right Model

The proposed fast path:

```
1. if push != [] ⇒ pop and return
2. else if lookahead != [] ⇒ pop and return
3. else ⇒ call Driver.next(source_tail, driver) directly
```

This eliminates:
- Queue in/out for sequential parsing (most common)
- Batch refill overhead
- Per-token terminator snapshots

The insight that most parsing is sequential (`next/1` dominant, `peek/1` occasional) is correct and aligns with ToxicParser's access patterns.

### 3. Profile-Based Normalization is Elegant

The `profile: :toxic_parser` option that emits:
- `:eoe` instead of `:eol` / `:";"`
- `:dot_op` instead of `:.`
- `:error_token` in tolerant mode

This moves adapter-only normalization into the lexer (where it belongs) without breaking other consumers.

### 4. Cursor-Native Checkpointing is Cleaner

Storing checkpoint state in the cursor struct (vs. process dictionary):

```elixir
%{
  source_tail: source_tail,
  driver: driver,
  lookahead: lookahead,
  push: push,
  eof: eof,
  error: error
}
```

This is simpler, more testable, and avoids the global state issues of process dictionary usage.

---

## What's Missing or Needs Clarification

### 1. Quantified Performance Expectations

The design references "per-token overhead" but doesn't quantify the expected improvement.

From current profiling (`OPTIMIZATION_SUMMARY.md`):

| Operation | % of Runtime |
|-----------|--------------|
| Token operations (peek/next/pushback) | ~7.5% |
| Toxic.Driver.next/2 | 3.24% |
| Position.to_location/3 | <1% (after optimizations) |
| Checkpoint/rewind | <0.5% (16,728 checkpoints) |

**Realistic expectation:** If the cursor design eliminates half of the token operation overhead (~3.75%) and most metadata allocation (~1%), the total speedup might be **5-8%** for the parser. This is meaningful but not transformational.

**Recommendation:** Add quantified targets to validate the design's ROI.

### 2. TokenAdapter Functionality That Must Be Preserved

The current `TokenAdapter` provides:

1. **EOE normalization** - Collapsing `:eol` and `:semicolon` into EOE handling
2. **Metadata extraction** - Range computation with line/column
3. **Lookahead caching** - `maybe_cache/3` for peek results
4. **Checkpoint integration** - Parser-level state restoration
5. **Terminator access** - `terminators/1` for diagnostics

The design addresses (1), (3), (4), and (5), but (2) needs more detail.

**Question:** How does `Builder.Meta` construct AST metadata from raw Toxic metas without per-token `Position.to_location/3`? The design says "compute `range` only at AST construction boundaries" but doesn't specify:

- How to track the line index efficiently
- Whether Toxic metas contain enough information (they're `{start_pos, end_pos, newlines}` tuples)
- How heredoc/sigil special cases are handled

### 3. Cursor State Management During Nested Parsing

The design proposes LIFO checkpoint semantics (`:stack` mode), but ToxicParser has nested parsing contexts:

```elixir
# Example: parsing %{a => fn x -> x end}
# - Enter map context
# - Enter fn context
# - Parse stab clauses
# - Exit fn context
# - Exit map context
```

Each context may have its own checkpoints. The current TokenAdapter tracks these separately.

**Question:** Does the cursor's `:stack` mode handle nested checkpoint scopes correctly? What happens if:
1. Parser checkpoints at position A
2. Enters nested context, checkpoints at position B
3. Nested context rewinds to B
4. Outer context wants to rewind to A

The design implies this works (LIFO), but explicit documentation would help.

### 4. Error Recovery in Tolerant Mode

The design says:

> ToxicParser runs Toxic in `:tolerant` mode and therefore never needs to synthesize error tokens itself.

But current ToxicParser error recovery is more complex:

1. **Synchronization** - After errors, the parser skips to synchronization points (`,`, `)`, `end`, etc.)
2. **Diagnostic accumulation** - Errors are collected with location information
3. **Partial AST construction** - Error nodes inserted into AST

**Question:** How does the cursor expose:
- The error's location (for diagnostics)?
- Enough information to build an error node?
- The ability to continue parsing after an error?

The `:error_token` tuple `{:error_token, meta, %Toxic.Error{...}}` needs to carry all this information.

### 5. Comments and Whitespace

The design mentions comments briefly:

> if ToxicParser wants comment preservation/conformance, the cursor should expose comment tokens or an out-of-band comment channel

But ToxicParser **does** want comment preservation for `Code.string_to_quoted_with_comments/2` conformance. The current implementation relies on Toxic's comment accumulation.

**Missing:** How does `Toxic.cursor/2` handle comments? Options:
1. Emit comment tokens inline (parser filters them)
2. Accumulate comments separately, expose via `Toxic.comments(cursor)`
3. Attach comments to adjacent tokens' metadata

Option 2 seems closest to current behavior, but needs specification.

### 6. Slice/Range Operations

The current Toxic provides `slice/3` for incremental lexing:

```elixir
Toxic.slice(source, start_pos, end_pos)
```

This is used by IDE tooling to re-lex portions of files.

**Question:** Does the cursor API preserve this capability? The design mentions "Preserve incremental lexing support" as a goal but doesn't specify the interface.

### 7. Migration Complexity

The four-phase migration plan is reasonable, but some phases are more complex than described:

**Phase 2 ("Update ToxicParser to use cursor + raw tokens"):**

This requires changing every location that calls `TokenAdapter.peek/next/peek_n`:
- ~50 call sites across Grammar modules
- Token destructuring changes (`%{kind: k, value: v, metadata: m}` → tuple pattern matching)
- Metadata handling in Builder modules

**Estimated scope:** 500-1000 lines of changes across 15+ files.

**Phase 3 ("Delete TokenAdapter"):**

Before deletion, all existing tests must pass with the new interface. This includes:
- Conformance tests against Elixir's parser
- Error recovery tests
- Comment preservation tests

**Risk:** High potential for subtle regressions in edge cases.

---

## Risk Assessment

### High Risk Areas

1. **AST Metadata Conformance** - ToxicParser must produce identical metadata to `Code.string_to_quoted/2`. Any change to how metadata is computed risks conformance failures.

2. **Cross-Repository Coordination** - Changes to Toxic's public API affect both ToxicParser and any other Toxic consumers. Versioning and compatibility become important.

3. **Performance Regression** - The "no-buffer fast path" assumes sequential access dominates. If certain parsing patterns cause frequent buffer rebuilding, performance could regress.

### Medium Risk Areas

1. **Comment Preservation** - The design doesn't fully specify comment handling, which is required for LSP/IDE use cases.

2. **Error Recovery** - Tolerant mode behavior changes could affect error diagnostics quality.

3. **Incremental Lexing** - If the cursor doesn't support slice operations, incremental tooling breaks.

### Low Risk Areas

1. **Profile-based normalization** - Opt-in, backwards compatible.

2. **Checkpoint semantics** - LIFO matches current usage patterns.

---

## Implementation Recommendations

### Option A: Full Implementation (High Effort, High Risk)

Follow the four-phase plan as designed. Expected timeline: significant.

**Pros:**
- Cleanest final architecture
- Maximum performance gain
- Eliminates technical debt

**Cons:**
- High coordination overhead
- Risk of regressions
- Blocks on Toxic changes

### Option B: TokenAdapter Optimization (Low Effort, Low Risk)

Instead of replacing TokenAdapter, optimize it:

1. **Lazy metadata computation** - Don't compute `range` until actually needed
2. **Remove terminator snapshots** - Make `terminators/1` compute on-demand
3. **Simplify lookahead** - Use a simple list instead of `++` patterns

This addresses the main pain points without changing the interface.

**Pros:**
- Minimal code changes
- No cross-repository coordination
- Incremental improvement

**Cons:**
- Doesn't achieve the "no-buffer fast path"
- Leaves double-buffering in place

### Option C: Phased Hybrid (Medium Effort, Medium Risk)

Implement the cursor in Toxic (Phase 1) but keep TokenAdapter as a thin wrapper:

```elixir
# New TokenAdapter (thin wrapper)
defmodule ToxicParser.TokenAdapter do
  def new(source, opts) do
    %State{cursor: Toxic.cursor(source, Keyword.put(opts, :profile, :toxic_parser))}
  end

  def peek(%State{cursor: cursor} = state) do
    case Toxic.peek(cursor) do
      {:ok, token, cursor} ->
        {:ok, normalize_token(token), %{state | cursor: cursor}}
      {:eof, cursor} ->
        {:eof, %{state | cursor: cursor}}
    end
  end

  # ... similar for next, peek_n, checkpoint, rewind
end
```

This:
- Gets the "no-buffer fast path" benefit from the cursor
- Preserves existing ToxicParser interfaces
- Allows gradual migration to raw tokens

**Pros:**
- Delivers cursor benefits incrementally
- Reduces migration risk
- Easier to test and validate

**Cons:**
- Keeps a (thinner) abstraction layer
- Doesn't eliminate all per-token overhead

---

## Suggested Design Improvements

### 1. Add Explicit Token Helpers

The design mentions helper functions but doesn't specify them:

```elixir
defmodule Toxic.Token do
  @compile {:inline, [kind: 1, meta: 1, value: 1, newlines: 1]}

  def kind({k, _}), do: k
  def kind({k, _, _}), do: k
  def kind({k, _, _, _}), do: k

  def meta({_, m}), do: m
  def meta({_, m, _}), do: m
  def meta({_, m, _, _}), do: m

  def value({_, _}), do: nil
  def value({_, _, v}), do: v
  def value({_, _, v, _}), do: v

  # ... etc
end
```

These should be in the design document for completeness.

### 2. Specify Comment Handling

Add a section on comment handling:

```elixir
# Option: separate comment channel
{comments, cursor} = Toxic.comments(cursor)  # drains accumulated comments

# Or: token stream with comments
cursor = Toxic.cursor(source, emit_comments: true)
{:ok, {:comment, meta, text}, cursor} = Toxic.next(cursor)
```

### 3. Define Error Token Structure

The `:error_token` needs a concrete structure:

```elixir
{:error_token, meta, %Toxic.Error{
  message: String.t(),
  position: {line, column},
  snippet: String.t(),       # relevant source context
  recovery: :skip | :insert  # how the lexer recovered
}}
```

### 4. Add Cursor Inspection

For debugging and testing:

```elixir
Toxic.cursor_position(cursor) :: {line, column}
Toxic.cursor_offset(cursor) :: non_neg_integer()
Toxic.cursor_remaining(cursor) :: binary()  # remaining source
```

---

## Conclusion

**The design is sound** in its core concepts:
- No-buffer fast path ✓
- Profile-based normalization ✓
- Cursor-native checkpointing ✓
- Direct token tuple consumption ✓

**What needs improvement:**
1. Quantified performance expectations
2. Complete metadata handling specification
3. Comment preservation strategy
4. Error token structure
5. Migration risk mitigation

**Recommendation:**

Given the current profiling data showing token operations at ~7.5% of runtime, **Option C (Phased Hybrid)** provides the best risk/reward balance:

1. Implement `Toxic.cursor/2` with the no-buffer fast path
2. Keep TokenAdapter as a thin wrapper initially
3. Migrate ToxicParser incrementally to raw tokens
4. Remove TokenAdapter once conformance is verified

This delivers most of the performance benefit while reducing migration risk and allowing validation at each step.

The design document should be updated with:
- Quantified performance targets
- Complete token helper specifications
- Comment handling strategy
- Error token structure definition
- Cursor inspection/debugging API
