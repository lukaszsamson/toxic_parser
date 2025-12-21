# ToxicParser Performance Analysis Report

**Date:** 2025-12-18
**Test Corpus:** 50 Elixir files from Elixir standard library
**Total Bytes:** 1,330,661
**Total AST Nodes:** 90,645
**Baseline Time:** ~3,300ms
**Baseline Throughput:** ~27,000 AST nodes/sec, ~390 KB/sec

## Executive Summary

The profiling reveals several performance bottlenecks primarily related to:
1. **O(n) data structure operations** - Heavy use of keyword lists and linear searches
2. **Repeated struct allocation** - Context and metadata structs created repeatedly
3. **Checkpoint/backtracking overhead** - Process dictionary and state copying
4. **Token processing overhead** - Metadata computation per token

## Detailed Findings

### 1. Precedence Lookups (Critical - High Impact)

**Problem:** `Precedence.binary/1` and `Precedence.unary/1` use `List.keyfind/3` which is O(n).

**Stats:**
- `Precedence.binary/1`: 1,111,160 calls, 745,368 words memory
- `:lists.keyfind/3`: 11,445,277 calls

**Location:** `lib/toxic_parser/precedence.ex:46-59`

```elixir
# Current (O(n) per lookup):
def binary(kind) do
  case List.keyfind(@binary_bp, kind, 0) do
    {_, bp, assoc} -> {bp, assoc}
    _ -> nil
  end
end
```

**Recommendation:** Convert to compile-time pattern matching:

```elixir
# Optimized (O(1) pattern match):
for {kind, bp, assoc} <- @binary_bp do
  def binary(unquote(kind)), do: {unquote(bp), unquote(assoc)}
end
def binary(_), do: nil
```

**Estimated Impact:** ~5-10% time reduction

---

### 2. Context Struct Recreation (Medium Impact)

**Problem:** Context constructors create new structs on every call.

**Stats:**
- `Context.matched_expr/0`: 8,945 calls
- `Context.kw_no_parens_value/0`: 7,505 calls
- `Context.expr/0`: thousands of calls

**Location:** `lib/toxic_parser/context.ex:40-84`

```elixir
# Current (new struct every call):
def matched_expr, do: %__MODULE__{allow_do_block: false, allow_no_parens_expr: false}
```

**Recommendation:** Use module attributes:

```elixir
@matched_expr %__MODULE__{allow_do_block: false, allow_no_parens_expr: false}
def matched_expr, do: @matched_expr
```

**Estimated Impact:** ~2-3% time reduction, memory improvement

---

### 3. Keyword List Operations (Critical - High Impact)

**Problem:** Heavy use of O(n) keyword list operations for metadata handling.

**Stats:**
- `Keyword.delete_key/2`: 140,837 calls
- `Keyword.put/3`: 164,551 calls
- `Keyword.take/2`: 206,397 calls
- `Keyword.merge/2`: 171,261 calls
- `Keyword."-take/2-lists^filter/1-1-"/2`: 432,208 calls (internal)

**Key Locations:**
- `lib/toxic_parser/builder/meta.ex:30-38` - `closing_meta/5`
- Token metadata construction throughout

```elixir
# Current (multiple O(n) operations):
def closing_meta(base_meta, close_meta, newlines \\ 0, prefix \\ [], opts \\ []) do
  base_first? = Keyword.get(opts, :base_first, false)
  newlines_kv = newlines_kv(newlines)
  if base_first? do
    prefix ++ base_meta ++ newlines_kv ++ [closing: close_meta]
  else
    prefix ++ newlines_kv ++ [closing: close_meta] ++ base_meta
  end
end
```

**Recommendations:**
1. Use maps internally for metadata, convert to keyword at AST construction
2. Avoid repeated ++ concatenation - build lists in correct order
3. Consider a metadata builder pattern that accumulates efficiently

**Estimated Impact:** ~10-15% time reduction

---

### 4. Checkpoint/Backtracking Overhead (Medium-High Impact)

**Problem:** Heavy checkpoint usage for speculative parsing.

**Stats:**
- `Toxic.checkpoint/1`: 134,062 calls, 2,144,992 words
- `Toxic.rewind_to/3`: 74,439 calls, 805,666 words
- `:erlang.put/2`: 134,062 calls (process dictionary)
- `:erlang.erase/1`: 134,062 calls
- `:maps.take/2`: 59,623 calls for checkpoint management

**Locations:**
- `lib/toxic_parser/token_adapter.ex:106-145` - checkpoint/rewind
- Used heavily in stab clause parsing, keyword list detection

**Recommendations:**
1. Reduce speculative parsing where possible with better lookahead
2. Cache common lookahead patterns
3. Consider lazy checkpoint creation (only save when rewind likely)
4. Profile to identify which parsing paths use most checkpoints

**Estimated Impact:** ~5-8% time reduction

---

### 5. Token Normalization Overhead (Medium Impact)

**Problem:** Each token goes through normalization with metadata computation.

**Stats:**
- Token processing is ~20% of total time
- `Position.to_location` called twice per token
- `metadata/4` creates new map per token

**Location:** `lib/toxic_parser/token_adapter.ex:203-310`

**Recommendations:**
1. Lazy metadata computation - only compute when accessed
2. Cache position calculations
3. Consider streaming metadata through the lexer

**Estimated Impact:** ~3-5% time reduction

---

### 6. EOE (End-of-Expression) Handling (Low-Medium Impact)

**Problem:** EOE tokens are peeked, consumed, and pushed back repeatedly.

**Stats:**
- `EOE.skip_count_newlines/2`: 245,245 calls
- `EOE.skip_newlines_only/2`: 587,702 calls
- `Pratt.peek_past_eoe/1`: Called from every `led` invocation

**Location:** `lib/toxic_parser/grammar/eoe.ex`

**Recommendations:**
1. Batch EOE handling in lexer
2. Pre-compute EOE boundaries
3. Consider EOE-aware token stream

---

### 7. String/Memory Operations (Low-Medium Impact)

**Stats:**
- `:erlang.binary_to_list/1`: 2,520 calls, 949.80 words/call
- `:erlang.list_to_tuple/1`: 100 calls, 427.77 words/call (line index)
- `:unicode.characters_to_list/2`: 1,156 calls, 2,354.56 words/call

**Recommendations:**
1. Keep strings as binaries where possible
2. Avoid list<->binary conversions in hot paths

---

## Architectural Recommendations

### Short-term Optimizations (Low Effort, High Impact)

1. **Compile-time precedence tables** - Convert `List.keyfind` to pattern matching
2. **Constant Context structs** - Use module attributes
3. **Keyword operations** - Minimize merges, build in correct order

### Medium-term Optimizations

4. **Metadata as maps** - Use maps internally, convert at boundaries
5. **Reduce checkpoint usage** - Better lookahead, lazy checkpoints
6. **Batch EOE handling** - Handle at lexer level

### Long-term Architectural Changes

7. **Streaming architecture** - Process tokens without full materialization
8. **Zero-copy parsing** - Reference source slices instead of copying
9. **Incremental parsing** - Cache parsed subtrees for incremental updates

## Implementation Priority

| Priority | Item | Effort | Impact |
|----------|------|--------|--------|
| P0 | Compile-time precedence | Low | High |
| P0 | Constant Context structs | Low | Medium |
| P1 | Keyword → Map for metadata | Medium | High |
| P1 | Optimize closing_meta | Low | Medium |
| P2 | Reduce checkpointing | High | Medium |
| P2 | Lazy metadata | Medium | Medium |
| P3 | Streaming architecture | High | High |

## Benchmark Baseline

For tracking optimization progress:

```
Files: 50 Elixir source files
Bytes: 1,330,661
AST Nodes: 90,645

Baseline Performance:
- Time: 3,300ms
- Throughput: 27,000 AST nodes/sec
- Memory: ~1.5GB words allocated

Target Performance:
- Time: <2,000ms (40% improvement)
- Throughput: >45,000 AST nodes/sec
```

## Appendix: Top Functions by Time

| Function | Calls | % Time |
|----------|-------|--------|
| TokenAdapter.peek/next | 10M+ | ~20% |
| Keyword operations | 1M+ | ~15% |
| Precedence.binary | 1.1M | ~5% |
| EOE.skip_* | 800K+ | ~5% |
| Pratt.led_dispatch | 645K | ~4% |
| Checkpoint ops | 134K | ~5% |

## Appendix: Top Functions by Memory

| Function | Calls | Words/Call |
|----------|-------|------------|
| :unicode.chars_to_list/2 | 1,156 | 2,354 |
| :erlang.binary_to_list/1 | 2,520 | 950 |
| :erlang.list_to_tuple/1 | 100 | 428 |
| State.new/2 | 50 | 33 |
| Toxic.new/4 | 50 | 24 |

---

## Appendix: Module-Specific Profiling (30 files, 779KB, 51K nodes)

### ToxicParser.Pratt Module Breakdown

**Total Time in Pratt:** 2,296,291 µs (100% of matched functions)

| Function | Calls | % Time | µs/Call | Notes |
|----------|-------|--------|---------|-------|
| `parse_no_parens_call_nud_with_min_bp/5` | 5,920 | **26.73%** | 103.68 | Highest per-call cost! |
| `peek_past_eoe/3` | 464,837 | 12.67% | 0.63 | Called for every `led` |
| `led/6` | 400,607 | 10.89% | 0.62 | Main expression loop |
| `led_call/6` | 7,248 | 6.18% | 19.58 | Paren calls expensive |
| `led_dot/6` | 8,844 | 4.91% | 12.75 | Dot access costly |
| `nud/5` | 108,359 | 4.76% | 1.01 | |
| `led_binary/9` | 49,721 | 4.28% | 1.98 | Binary operators |
| `parse_paren_call_base/4` | 10,768 | 4.13% | 8.81 | |
| `maybe_attach_do_block/7` | 89,257 | 3.92% | 1.01 | |
| `nud_identifier_or_literal/6` | 95,177 | 3.19% | 0.77 | |
| `parse_rhs_identifier/6` | 30,753 | 3.04% | 2.27 | |
| `parse_rhs/6` | 56,411 | 2.86% | 1.16 | |
| `maybe_nested_call_or_do_block/6` | 23,330 | 2.21% | 2.17 | |

**Key Findings:**
1. `parse_no_parens_call_nud_with_min_bp` takes 26.73% of Pratt time with only 5,920 calls
   - **103.68 µs/call** is extremely high - this function needs optimization
2. `peek_past_eoe` is called 465K times - EOE handling is a hot path
3. `led_call` and `led_dot` have high per-call costs (19.58 and 12.75 µs)

---

### ToxicParser.TokenAdapter Module Breakdown

**Total Time in TokenAdapter:** 4,345,030 µs

| Function | Calls | % Time | µs/Call | Notes |
|----------|-------|--------|---------|-------|
| `fetch_next/2` | 3,229,497 | **40.31%** | 0.54 | Main token fetching |
| `metadata/4` | 3,229,347 | 14.16% | 0.19 | Metadata computation |
| `update_state/6` | 3,229,347 | 11.38% | 0.15 | State updates |
| `normalize_token/3` | 3,229,347 | 5.78% | 0.08 | Token normalization |
| `maybe_cache/3` | 3,229,347 | 4.56% | 0.06 | Lookahead caching |
| `peek/1` | 3,916,375 | 4.01% | 0.04 | Token peeking |
| `newline_count/1` | 3,378,114 | 3.19% | 0.04 | |
| `delimiter/1` | 3,229,347 | 3.15% | 0.04 | |
| `synthesized?/1` | 3,229,347 | 3.07% | 0.04 | |
| `delimiter_role/1` | 3,229,347 | 3.05% | 0.04 | |
| `next/1` | 863,291 | 1.91% | 0.10 | |

**Key Findings:**
1. `fetch_next/2` dominates at 40.31% - this is where tokens come from Toxic lexer
2. `metadata/4` is 14.16% - metadata computation per token is expensive
3. 3.2M tokens processed for 30 files - ~107K tokens/file average
4. `delimiter/1`, `synthesized?/1`, `delimiter_role/1` each called 3.2M times
   - These compute properties that could be cached or computed lazily

---

### ToxicParser.Precedence Module Breakdown

**Total Time in Precedence:** 55,780 µs

| Function | Calls | % Time | µs/Call |
|----------|-------|--------|---------|
| `binary/1` | 679,740 | **87.01%** | 0.07 |
| `unary/1` | 108,617 | 10.77% | 0.06 |
| `bp_from_binary/1` | 10,562 | 1.43% | 0.08 |
| `stab_op_bp/0` | 10,562 | 0.79% | 0.04 |

**Key Findings:**
1. `Precedence.binary/1` called 680K times - confirms this is critical to optimize
2. Currently using `List.keyfind` which is O(n) for 14 operators
3. Pattern matching would make this O(1)
4. Estimated savings: ~48,000 µs → ~5,000 µs with pattern matching

---

## Updated Recommendations Based on Module Profiling

### Highest Priority Optimizations

1. **Optimize `parse_no_parens_call_nud_with_min_bp`** (Pratt.ex)
   - 103.68 µs/call is 100x higher than typical functions
   - Investigate what makes this so expensive
   - Likely involves checkpoint/backtracking overhead

2. **Reduce EOE overhead** (peek_past_eoe)
   - 465K calls from `led` function
   - Consider caching EOE state or batching

3. **Lazy metadata in TokenAdapter**
   - `metadata/4` at 14.16% is pure overhead
   - Defer computation until metadata is accessed

4. **Pattern-match Precedence.binary**
   - 680K calls with O(n) lookup
   - Easy fix with compile-time code generation

### Call Count Summary (30 files)

| Component | Calls | Note |
|-----------|-------|------|
| Tokens processed | 3.2M | ~107K/file |
| TokenAdapter.peek | 3.9M | |
| Precedence.binary | 680K | |
| Pratt.led | 401K | |
| Checkpoint ops | 84K | |
| EOE handling | 465K | |
