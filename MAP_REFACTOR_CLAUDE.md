# Review of MAP_REFACTOR.md Design

**Reviewer:** Claude Opus 4.5
**Date:** 2025-12-18

## Executive Summary

The design in MAP_REFACTOR.md correctly identifies the problem (checkpoint/rewind causing reparse) and proposes a sound two-stage solution. Stage 1 (single-pass prefix + continuation) is practical and low-risk. Stage 2 (delimiter-aware parsing) is more ambitious and closer to YRL but requires careful handling of edge cases.

However, given the current profiling data showing map parsing accounts for only **~0.05% of total time** (~926 checkpoint calls), the ROI of this refactor is questionable unless it's primarily for code clarity rather than performance.

---

## What the Design Gets Right

### 1. Correct Problem Identification

The design accurately describes the current behavior:

```
For the common case %{a => b, c => d}, the first entry is parsed twice:
- Once by try_parse_map_update (discarded after rewind)
- Again by parse_map_close
```

This is exactly what happens in `maps.ex:361-380`.

### 2. Correct Understanding of YRL Grammar

The design correctly identifies the key YRL productions:

```erlang
assoc_expr -> matched_expr assoc_op_eol matched_expr  % key => value
assoc_expr -> map_base_expr                           % just an expression

assoc_update -> matched_expr pipe_op_eol assoc_expr   % base | entry
assoc_update_kw -> matched_expr pipe_op_eol kw_data   % base | a: 1, b: 2

map_args -> open_curly map_close                      % regular entries
map_args -> open_curly assoc_update close_curly       % update form
map_args -> open_curly assoc_update ',' map_close     % update + more
```

### 3. Correct Insight About Delimiters

The design correctly notes:
> Inside `%{...}`, `|` is a *delimiter* in `assoc_update*`, not a normal `|` operator.
> Inside `%{...}`, `=>` is a *delimiter* in `assoc_expr`, not a normal operator.

This is crucial. The YRL comment (line 65-67) confirms:
> Note though the operator => in practice has lower precedence
> than all others, its entry in the table is only to support the
> %{user | foo => bar} syntax.

### 4. Two-Stage Approach is Sensible

Breaking the refactor into:
1. **Stage 1**: Keep existing Pratt-based detection, remove checkpoint/rewind by reusing parsed result
2. **Stage 2**: Make `=>` and `|` true delimiters

This reduces risk by allowing incremental progress.

---

## What's Missing or Needs Clarification

### 1. Profiling Context

The design doesn't mention the current performance impact. From recent profiling:

| Function | Calls | Time % |
|----------|-------|--------|
| `try_parse_map_update/3` | 926 | 0.00% |
| `parse_map_close/3` | 853 | 0.01% |
| `parse_map_args_body/7` | 1,046 | 0.01% |

**Total map parsing: ~0.05% of runtime**

The checkpoint/rewind in maps contributes only ~926 of 16,728 total checkpoints (~5.5%).

**Recommendation:** Unless code clarity is the primary goal, this refactor may not be worth the complexity. The stab/keywords optimizations provided much larger gains.

### 2. Stage 1: The "Prefix + Continuation" Pattern

The design proposes `parse_map_prefix/3` returning one of:
- `{:update, base_ast, pipe_meta, rhs_entries, state, log}`
- `{:entry, first_entry_ast, state, log}`
- `{:kw_only, kw_list, state, log}`

**Missing detail:** How does this avoid the reparse?

The current flow is:
1. `try_parse_map_update` checkpoints
2. Parses expression (may consume multiple entries if `=>` found)
3. If not update → rewind → `parse_map_close` parses again

The Stage 1 fix should be:
1. Parse first "thing" (expression or kw_data)
2. Check for `|` token
3. If `|` found → it's an update, continue with update logic
4. If no `|` → pass the parsed first entry to `parse_map_close_after_first/5`

**Key insight:** The parse result is reusable regardless of whether it's an update or regular entry. The only question is "is there a `|` after it?"

### 3. Stage 2: The `c!s|n` Problem

The design correctly identifies this as a critical edge case:

```elixir
%{c!s|n => 1}  # The | is INSIDE the key expression, not a map update delimiter
```

**Current handling (maps.ex:392-397):**
```elixir
# First, try parsing the full expression without restricting |
# This allows op_identifier calls (like c!) to consume | as part of their argument
case Pratt.parse(state, Context.container_expr(), log) do
```

**Missing from design:** A concrete algorithm for Stage 2 to handle this. The design says:
> Add a Pratt entry point that stops on `|` as a delimiter *only when the `|` is at the current Pratt led-level*

This is vague. A more concrete approach:

```elixir
# Stage 2: Parse map update base with pipe-as-delimiter
def parse_map_update_base(state, ctx, log) do
  # Use a special context that treats | as delimiter at top level
  # but allows | inside nested constructs (parens, calls, etc.)
  Pratt.parse_with_delimiter(state, ctx, log, :pipe_op, @pipe_op_bp)
end
```

The Pratt parser would need to track "delimiter depth" - when inside parens or call args, `|` is a regular operator; at top level, it's a delimiter.

### 4. `extract_assoc` vs Delimiter Parsing

The current implementation uses `extract_assoc/1` to find the rightmost `=>` in the AST:

```elixir
# Parse full expression
{:ok, expr, state, log} <- Pratt.parse(state, Context.unmatched_expr(), log)
# Extract => from AST
case extract_assoc(expr) do
  {:assoc, key, value, assoc_meta} -> ...
```

The design proposes replacing this with delimiter-aware parsing:
```elixir
# Parse key, stopping before =>
{:ok, key, state, log} <- Pratt.parse_with_min_bp(state, ctx, log, assoc_bp + 1)
# Consume => explicitly
{:ok, assoc_tok, state} <- TokenAdapter.next(state)  # must be =>
# Parse value
{:ok, value, state, log} <- Pratt.parse(state, ctx, log)
```

**Trade-off analysis:**

| Approach | Pros | Cons |
|----------|------|------|
| `extract_assoc` | Works with existing Pratt | Post-hoc AST manipulation, diverges from YRL |
| Delimiter parsing | Matches YRL structure | Requires Pratt changes, more complex |

### 5. Error Recovery in Tolerant Mode

The design mentions error-tolerant parsing as a goal but doesn't specify how the refactor affects it.

**Current behavior:** If `try_parse_map_update` fails, we rewind and try `parse_map_close`. This provides implicit fallback.

**Stage 1 risk:** If we don't checkpoint, a parse failure in the "prefix" might leave the parser in an inconsistent state.

**Recommendation:** Keep a "minimal checkpoint" for error recovery, even if we don't rewind for the common case.

### 6. Metadata Preservation

The current implementation carefully preserves metadata:
- `annotate_assoc/2` adds `:assoc` metadata to keys
- `token_meta_with_newlines/2` handles newlines for `|`

**Not addressed in design:** How Stage 2's delimiter parsing preserves this metadata. The YRL's `with_assoc_meta/2` does:

```erlang
with_assoc_meta({Target, Meta, Args}, AssocToken) ->
  case ?token_metadata() of
    true -> {Target, [{assoc, meta_from_token(AssocToken)} | Meta], Args};
    false -> {Target, Meta, Args}
  end.
```

Stage 2 should replicate this behavior.

---

## Risk Assessment

### Stage 1 (Low Risk)
- Minimal changes to control flow
- Preserves existing Pratt/extract_assoc logic
- Easy to test incrementally
- **Estimated complexity:** ~100 lines changed

### Stage 2 (Medium-High Risk)
- Requires Pratt parser changes (delimiter handling)
- Complex interaction with `c!`-style calls
- May affect other uses of `|` and `=>`
- **Estimated complexity:** ~300+ lines changed

---

## Implementation Recommendations

### If Performance is the Goal

**Don't do this refactor.** The 926 map checkpoints contribute negligibly to runtime (~0.05%). Focus on the top hotspots:
- `:lists.keyfind` (3.82%)
- Token operations (7.5%)
- Stab classifier (4.4%)

### If Code Clarity is the Goal

Proceed with **Stage 1 only**:

1. Implement `parse_map_prefix/3` that parses one expression/kw_data
2. Check for `|` token after the prefix
3. Branch without rewinding:
   - `|` found → update path with `parse_map_update_rhs`
   - No `|` → entry path with `parse_map_close_after_first`
4. Remove checkpoint from `try_parse_map_update`
5. Keep `extract_assoc` for now (works correctly)

**Skip Stage 2** unless there's a specific conformance issue that requires delimiter-aware parsing.

### Concrete Stage 1 Implementation Sketch

```elixir
defp parse_map_args_body(base, percent_meta, brace_meta, leading_newlines, state, ctx, log) do
  case TokenAdapter.peek(state) do
    {:ok, %{kind: :"}"} = close_tok, _} ->
      # Empty map
      {:ok, _close, state} = TokenAdapter.next(state)
      close_meta = token_meta(close_tok.metadata)
      map_meta = Meta.closing_meta(brace_meta, close_meta, leading_newlines)
      {:ok, build_map_ast(base, [], percent_meta, map_meta), state, log}

    {:ok, %{kind: :kw_identifier}, _} ->
      # Definite keyword data - can't be update base
      with {:ok, entries, close_meta, state, log} <- parse_map_close(state, ctx, log) do
        map_meta = Meta.closing_meta(brace_meta, close_meta, leading_newlines)
        {:ok, build_map_ast(base, entries, percent_meta, map_meta), state, log}
      end

    {:ok, _, _} ->
      # Parse first element (could be update base or first entry)
      case parse_map_first_element(state, ctx, log) do
        {:ok, {:update, update_ast, close_meta}, state, log} ->
          map_meta = Meta.closing_meta(brace_meta, close_meta, leading_newlines)
          {:ok, build_map_update_ast(base, update_ast, percent_meta, map_meta), state, log}

        {:ok, {:entries, entries, close_meta}, state, log} ->
          map_meta = Meta.closing_meta(brace_meta, close_meta, leading_newlines)
          {:ok, build_map_ast(base, entries, percent_meta, map_meta), state, log}

        {:error, _, _, _} = err ->
          err
      end
  end
end

# Parse first element and determine if update or entries
defp parse_map_first_element(state, ctx, log) do
  case Pratt.parse(state, Context.container_expr(), log) do
    {:ok, expr, state, log} ->
      {state, newlines} = EOE.skip_count_newlines(state, 0)

      case TokenAdapter.peek(state) do
        {:ok, %{kind: :pipe_op, metadata: pipe_meta}, _} ->
          # Found | - this is a map update
          {:ok, _pipe, state} = TokenAdapter.next(state)
          state = EOE.skip(state)
          pipe_meta_kw = token_meta_with_newlines(pipe_meta, newlines)

          with {:ok, update_ast, close_meta, state, log} <-
                 parse_map_update_rhs_and_close(expr, pipe_meta_kw, state, log) do
            {:ok, {:update, update_ast, close_meta}, state, log}
          end

        _ ->
          # Not an update - use expr as first entry
          first_entry = normalize_assoc_expr(expr)
          with {:ok, entries, close_meta, state, log} <-
                 parse_map_close_after_first(first_entry, state, ctx, log) do
            {:ok, {:entries, entries, close_meta}, state, log}
          end
      end

    # Handle embedded pipe expressions (check_embedded_map_update case)
    # ... existing logic for {:|, _, _} detection ...
  end
end
```

---

## Conclusion

**Verdict: Design is sound but implementation priority is questionable.**

The design correctly identifies:
1. The reparse problem ✓
2. The YRL grammar structure ✓
3. The `c!s|n` edge case ✓
4. The two-stage approach ✓

What needs clarification:
1. Profiling context (map parsing is ~0.05% of runtime)
2. Concrete "prefix + continuation" algorithm
3. Error recovery in tolerant mode
4. Metadata preservation in Stage 2

**Recommendation:**
- If pursuing this, implement Stage 1 only
- Stage 2 is high complexity for low gain
- Consider this a "code quality" refactor, not a "performance" refactor
