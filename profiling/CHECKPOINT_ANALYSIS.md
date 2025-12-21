# Checkpoint/Rewind Analysis

**Date:** 2025-12-18
**Total checkpoints:** ~20,307
**Total rewinds:** ~9,597 (47% rewind rate)

## Main Source: Keywords Module

The Keywords module is responsible for most checkpoint/rewind calls:

| Function | Calls | Notes |
|----------|-------|-------|
| `try_parse_call_args_no_parens_kw/4` | 10,502 | **Main opportunity** |
| `try_parse_kw_call/4` | 37,848 | Only checkpoints for quoted keys |
| `try_parse_kw_data/4` | 26,353 | Only checkpoints for quoted keys |
| `starts_kw?/1` | 151,473 | Fast probe, no checkpoint |

## The Main Opportunity: `try_parse_call_args_no_parens_kw`

**Location:** `keywords.ex:304-305`

```elixir
cond do
  starts_kw?(tok) or (allow_quoted_keys? and tok.kind in @quoted_kw_start) ->
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state)  # ALWAYS checkpoints!
    case parse_call_args_no_parens_kw(checkpoint_state, ctx, log, min_bp) do
```

**Problem:** This checkpoints for BOTH:
- Regular `kw_identifier` (like `foo:`) - **doesn't need checkpoint**
- Quoted keys (like `"foo":`) - needs checkpoint

**Why regular kw_identifier doesn't need checkpoint:**

The rewind cases (lines 311-324) only trigger when the error involves quoted string starts:
```elixir
{:error, {:expected, :item, got: got_kind}, ...} when got_kind in @quoted_kw_start ->
  {:no_kw, TokenAdapter.rewind(state, ref), log}

{:error, {:expected, :keyword, got: got_kind}, ...} when got_kind in @quoted_kw_start ->
  {:no_kw, TokenAdapter.rewind(state, ref), log}
```

If `starts_kw?(tok)` is true (regular `kw_identifier`), parsing either:
- Succeeds → no rewind needed
- Fails → real error, not "try something else"

**Proposed Fix:**

```elixir
cond do
  starts_kw?(tok) ->
    # Regular kw_identifier - definite keyword, no checkpoint needed
    parse_call_args_no_parens_kw(state, ctx, log, min_bp)
    |> case do
      {:ok, kw_list, state, log} -> {:ok, kw_list, state, log}
      {:error, reason, state, log} -> {:error, reason, state, log}
    end

  allow_quoted_keys? and tok.kind in @quoted_kw_start ->
    # Quoted key - need checkpoint for fallback
    {ref, checkpoint_state} = TokenAdapter.checkpoint(state)
    case parse_call_args_no_parens_kw(checkpoint_state, ctx, log, min_bp) do
      # ... existing handling
    end

  true ->
    {:no_kw, state, log}
end
```

**Expected Impact:**
- ~90% of `try_parse_call_args_no_parens_kw` calls use regular `kw_identifier`
- Would eliminate ~9,500 checkpoint/rewind pairs
- Reduces total checkpoints by ~47%

## Other Checkpoint Sites (Lower Priority)

### 1. `dots.ex:264` (reject_initial_kw_data)

**Pattern:** Tries parsing keyword data just to check if it exists, then always rewinds.

```elixir
{ref, checkpoint_state} = TokenAdapter.checkpoint(state)
case Keywords.try_parse_kw_data(checkpoint_state, ...) do
  {:ok, kw_list, state, log} ->
    state = TokenAdapter.rewind(state, ref)  # Always rewinds!
    {:error, syntax_error_before(...), state, log}
  {:no_kw, state, log} ->
    {:ok, TokenAdapter.rewind(state, ref), log}  # Always rewinds!
  {:error, ...} ->
    {:error, ..., TokenAdapter.rewind(state, ref), log}  # Always rewinds!
end
```

**Could use lookahead:** Just probe with `starts_kw_or_quoted_key?` instead of full parse.

### 2. `brackets.ex:39` (parse_single_container_expr)

**Pattern:** Parses comma-separated list to detect `value[a, b]` error.

**Not easily optimized:** We need to parse to know if there are multiple elements.

### 3. `calls_private.ex:72` (try_single_no_parens_arg)

**Pattern:** Tries parsing with `no_parens_expr` context, falls back if not followed by `)`.

**Hard to optimize with lookahead:** Would need to simulate full expression parsing.

### 4. `maps.ex:367` (try_parse_map_update)

**Pattern:** Detects `%{expr | ...}` syntax.

**Already optimized:** Only 926 calls, low impact.

### 5. `stabs.ex:120` (parse_paren_stab)

**Already optimized:** Uses classifier with `:unknown` fallback.

## Summary

| Site | Calls | Opportunity | Difficulty |
|------|-------|-------------|------------|
| `keywords.ex:305` | 10,502 | **High - 47% reduction** | Easy |
| `dots.ex:264` | varies | Medium | Easy |
| `calls_private.ex:72` | varies | Low | Hard |
| `brackets.ex:39` | 125 | Low | Hard |
| `maps.ex:367` | 926 | Low | N/A |
| `stabs.ex:120` | ~258 | N/A (done) | N/A |

## Recommendation

1. **Fix `try_parse_call_args_no_parens_kw`** to avoid checkpoint for regular `kw_identifier`
2. **Fix `reject_initial_kw_data`** to use probe instead of full parse
3. Leave other sites as-is (low impact or hard to optimize)

Expected total reduction: **~50% fewer checkpoints/rewinds**
