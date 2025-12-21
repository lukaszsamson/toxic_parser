# Stab Parsing Performance Analysis

## Status: Analysis Complete, Optimization Deferred

After detailed analysis and experimentation, the stab parsing optimization is more complex than initially expected. This document captures the findings for future work.

## Initial Tracing Insight

The tracing reveals valuable data about the double-parsing waste:

| Metric | Value | Notes |
|--------|-------|-------|
| `try_parse_stab_clause` total calls | 31,621 | |
| Returns `:ok` (is stab) | 17,986 (57%) | Successful stab detection |
| Returns `:not_stab` | 13,635 (43%) | **All cause double-parsing** |
| `check_and_collect_stab_body` calls | 11,924 | Called per-EOE in stab bodies |
| - resulted in stab | 10,372 (87%) | Expected case |
| - resulted in not_stab | 1,552 (13%) | Double-parse waste |

**Key insight**: The 43% `:not_stab` rate from `try_parse_stab_clause` represents significant waste. Of these 13,635 cases:
- 1,552 come from `check_and_collect_stab_body` (stab body collection)
- ~12,000 come from other call sites (`parse_stab_eoe_until`, etc.)

## Problem Summary

The `check_and_collect_stab_body` function (stabs.ex:1514) causes O(2n) parsing in stab bodies because it speculatively parses each expression twice:

1. First as potential stab patterns (via `try_parse_stab_clause`)
2. Then again as body expression (if not a stab)

## Call Flow Analysis

```
parse_stab_body
  -> collect_stab_body_exprs (for each expression after EOE)
     -> check_and_collect_stab_body
        -> checkpoint
        -> try_parse_stab_clause
           -> parse_stab_patterns
              -> parse_stab_pattern_exprs  // Parses expression(s)
           -> parse_stab_clause_after_patterns
              -> Checks for '->' or 'when'
              -> Returns {:not_stab, ...} if neither found
        -> IF not_stab: rewind
        -> Pratt.parse_with_min_bp  // Parses SAME expression again!
```

## Why Double Parsing Happens

For code like:
```elixir
fn x ->
  a
  b
  c
end
```

After parsing `a`, the parser encounters EOE (newline), then must decide:
- Is `b` the start of a new stab clause (like `b -> ...`)?
- Or is `b` another body expression?

The current approach:
1. **Checkpoint** state
2. **Try parse** `b` as stab patterns
3. **Check** for `->` (not found)
4. **Rewind** to before `b`
5. **Parse** `b` as expression

This happens for EVERY expression after EOE in a stab body.

## Context Difference Problem

The two parsing passes use different contexts:
- Stab patterns: `Context.matched_expr()` - no do-blocks, no no-parens calls
- Body expressions: `Context.unmatched_expr()` - allows do-blocks and no-parens calls

This means we CAN'T simply reuse the AST from the first parse:
- `foo bar` as pattern → just `foo`
- `foo bar` as body → `foo(bar)` (no-parens call)

## Comparison with elixir_parser.yrl

The Yacc grammar handles this differently:

```yacc
stab_expr -> expr
stab_expr -> call_args_no_parens_all stab_op_eol_and_expr
```

Yacc's LALR parser uses lookahead to decide which production to use BEFORE parsing. It can see that `->` is (or isn't) coming without building AST.

## Proposed Optimizations

### Option 1: Quick Stab Lookahead (Recommended)

Add a lightweight "does `->` appear at stab position?" scan before calling `try_parse_stab_clause`:

```elixir
defp might_be_stab_clause?(state) do
  # Quick scan: skip through balanced parens/brackets,
  # look for '->' at "top level" (not inside containers)
  scan_for_stab_op(state, 0, 0)
end

defp scan_for_stab_op(state, paren_depth, bracket_depth) do
  case TokenAdapter.peek(state) do
    {:ok, %{kind: :stab_op}, _} when paren_depth == 0 and bracket_depth == 0 ->
      true
    {:ok, %{kind: :eoe}, _} when paren_depth == 0 and bracket_depth == 0 ->
      false  # EOE before -> means not a stab
    {:ok, %{kind: kind}, _} when kind in [:")", :end, :"]", :"}", :>>] ->
      false  # Terminator before -> means not a stab
    {:ok, %{kind: :"("}, _} ->
      {:ok, _, state} = TokenAdapter.next(state)
      scan_for_stab_op(state, paren_depth + 1, bracket_depth)
    {:ok, %{kind: :")"}, _} ->
      {:ok, _, state} = TokenAdapter.next(state)
      scan_for_stab_op(state, paren_depth - 1, bracket_depth)
    # ... similar for brackets
    {:ok, _, _} ->
      {:ok, _, state} = TokenAdapter.next(state)
      scan_for_stab_op(state, paren_depth, bracket_depth)
    _ ->
      false
  end
end
```

This is O(n) scan but very cheap (no AST allocation, just token iteration).

### Option 2: Parse-Once with Matched Context

1. Parse expression with `matched_expr` (restrictive)
2. Check what follows:
   - `->`: Build single-pattern stab (AST is correct)
   - `,`: Continue parsing patterns, check for `->`
   - `when`: Parse guard, check for `->`
   - Otherwise: Check if re-parse needed based on next token

```elixir
defp check_and_collect_stab_body(acc, state, ctx, log, terminator) do
  # Parse with matched_expr (same as stab patterns)
  case Pratt.parse_with_min_bp(state, Context.matched_expr(), log, @stab_pattern_min_bp) do
    {:ok, expr, state2, log} ->
      case TokenAdapter.peek(state2) do
        {:ok, %{kind: :stab_op}, _} ->
          # Single pattern stab - we're done collecting body
          build_stab_body(acc, state, log)

        {:ok, %{kind: :","}, _} ->
          # Could be multi-pattern - continue checking
          check_multi_pattern_or_body(expr, acc, state2, ctx, log, terminator)

        {:ok, %{kind: :when_op}, _} ->
          # Could be guard - continue checking
          check_guard_or_body(expr, acc, state2, ctx, log, terminator)

        _ ->
          # Not a stab - but do we need to re-parse?
          if needs_reparse?(state2, ctx) do
            # Re-parse with body context
            with {:ok, expr, state, log} <- Pratt.parse_with_min_bp(state, ctx, log, @stab_pattern_min_bp) do
              collect_stab_body_exprs([expr | acc], state, ctx, log, terminator)
            end
          else
            # Reuse the expression we already parsed
            collect_stab_body_exprs([expr | acc], state2, ctx, log, terminator)
          end
      end
  end
end

defp needs_reparse?(state, ctx) do
  # Check if the next token could cause different parsing in unmatched context
  case TokenAdapter.peek(state) do
    {:ok, %{kind: :do}, _} -> ctx.allow_do_block
    {:ok, tok, _} -> ctx.allow_no_parens_expr and can_start_no_parens_arg?(tok)
    _ -> false
  end
end
```

### Option 3: Token Position Caching

Cache the result of pattern parsing so we don't re-tokenize:

```elixir
# In try_parse_stab_clause, if we determine it's not a stab,
# return the partially parsed result:
{:not_stab, parsed_first_expr, state, log}

# Then in check_and_collect_stab_body:
case try_parse_stab_clause(...) do
  {:not_stab, first_expr, state, log} ->
    # Reuse first_expr if context allows, otherwise re-parse
    ...
end
```

## Estimated Impact

Based on profiling data:
- `check_and_collect_stab_body`: 16,252 calls (4.75x increase after bug fixes)
- `try_parse_stab_clause`: 44,912 calls
- Token traffic increased ~2.7x

With Option 1 (quick lookahead):
- Most body expressions would skip full stab parsing entirely
- Estimated 30-50% reduction in token operations in stab-heavy code
- Should bring performance close to pre-fix levels while keeping correctness

## Optimization Attempts and Why They Failed

### Attempt 1: Quick Scan for `->` (might_be_stab?)

**Idea**: Before calling `try_parse_stab_clause`, do a lightweight token scan looking for `->` at depth 0. If no `->` found, skip the full stab parsing.

**Implementation**:
```elixir
def might_be_stab?(state, terminator) do
  scan_for_stab_op(state, 0, terminator)
end

defp scan_for_stab_op(state, depth, terminator) do
  case TokenAdapter.peek(state) do
    {:ok, %{kind: :stab_op}, _} when depth == 0 -> true
    {:ok, %{kind: :eoe}, _} when depth == 0 -> false  # Stop at EOE
    {:ok, %{kind: :"("}, _} -> scan_for_stab_op(next(state), depth + 1, terminator)
    {:ok, %{kind: :")"}, _} -> scan_for_stab_op(next(state), depth - 1, terminator)
    {:ok, _, _} -> scan_for_stab_op(next(state), depth, terminator)
    _ -> false
  end
end
```

**Problem**: Multi-line expressions have EOE tokens between lines. For example:
```elixir
(e and f) or    # EOE here!
  (g and h) ->
```
The scan stops at the EOE after `or`, missing the `->` that follows.

**Attempted Fix**: Skip ALL EOE tokens in the scan.

**Why That Failed**: The scan now looks across stab clause boundaries:
```elixir
fn x ->
  body_expr    # After body_expr, scan skips EOE and finds -> from NEXT clause
  another_expr
next_pattern ->  # <- This -> is found!
  ...
end
```
This causes body expressions to be incorrectly identified as stab clauses.

### Attempt 2: Quick Check for Obvious Non-Stab Cases

**Idea**: Return `:definitely_not_stab` immediately if the first token is EOE, terminator, or block_identifier.

**Implementation**:
```elixir
defp quick_stab_check(state, terminator) do
  case TokenAdapter.peek(state) do
    {:ok, %{kind: :stab_op}, _} -> :definitely_stab
    {:ok, %{kind: :eoe}, _} -> :definitely_not_stab
    {:ok, %{kind: ^terminator}, _} -> :definitely_not_stab
    _ -> :needs_full_check
  end
end
```

**Problem**: `check_and_collect_stab_body` is called AFTER the EOE has already been consumed:
```elixir
# In collect_stab_body_exprs:
{:ok, %{kind: :eoe}, _} ->
  {:ok, _eoe, state} = TokenAdapter.next(state)  # EOE consumed here
  state = EOE.skip(state)                         # More EOE skipped
  check_and_collect_stab_body(acc, state, ...)    # Called with state AFTER EOE
```

So the first token is always an expression token (identifier, `(`, etc.), which means we always hit `:needs_full_check`.

### The Fundamental Context Mismatch Problem

Even if we could identify stabs vs non-stabs cheaply, we can't reuse the parsed AST because:

- Stab patterns use `Context.matched_expr()` - no do-blocks, no no-parens calls
- Body expressions use `Context.unmatched_expr()` or `Context.expr()` - allows both

The same token sequence produces different ASTs:
```elixir
# "foo bar" as stab pattern (matched_expr):
{:foo, [], nil}  # Just the identifier

# "foo bar" as body expression (unmatched_expr):
{:foo, [], [{:bar, [], nil}]}  # No-parens call foo(bar)
```

**Cannot simply reuse** the parsed expression without knowing which context it was parsed with.

## Viable Optimization Approaches (Future Work)

### Approach A: Smart AST Reuse
1. Parse expression with `matched_expr` context
2. Check what follows (`->`, `,`, `when`, or other)
3. If followed by stab indicators: use as pattern
4. If NOT followed by stab indicators: check if context matters
   - If next token could trigger no-parens call: re-parse with `unmatched_expr`
   - Otherwise: reuse the AST (most common case)

Complexity: Medium. Need to identify when contexts produce different ASTs.

### Approach B: Deferred Pattern Classification
Change parser architecture so patterns and expressions are parsed the same way, with classification deferred to post-processing.

Complexity: High. Would require significant refactoring.

### Approach C: Probabilistic Fast Path
Since 87% of `check_and_collect_stab_body` calls find stabs:
1. Parse aggressively as stab pattern
2. If successful, done
3. If not stab, re-parse as expression (13% of cases)

This is essentially what we already do. No change needed.

## Conclusion

The current implementation, while causing some double-parsing, is architecturally sound. The 13% waste in `check_and_collect_stab_body` is acceptable given:
- Most calls (87%) are successful stab detections
- The overhead is per-EOE-in-stab-body, not per-token
- Fixing it requires complex context analysis

**Recommendation**: Keep current implementation. Focus optimization efforts on higher-impact areas identified in ANALYSIS.md:
- Precedence lookup (easy win)
- Token metadata computation
- Keyword operations
