# Post-Stab-Refactor Profiling Analysis

**Date:** 2025-12-18
**Files profiled:** 50 Elixir files
**Baseline time:** ~374-417 ms
**Total calls:** 39,199,555
**Throughput:** ~3.17-3.41 KB/sec

## Executive Summary

The stab refactor significantly reduced checkpoint/rewind overhead. The current profile shows:

1. **Token operations dominate** (12-15% of time)
2. **Checkpoint/rewind is now reasonable** (~0.4% of time)
3. **No single algorithmic issue** - performance is now dominated by the fundamental cost of token-by-token parsing

## Checkpoint/Rewind Analysis

| Metric | Calls | Time % |
|--------|-------|--------|
| `TokenAdapter.checkpoint/1` | 20,307 | 0.12% |
| `TokenAdapter.rewind/2` | 9,597 | 0.07% |
| `TokenAdapter.drop_checkpoint/2` | 10,710 | 0.06% |
| **Total checkpoint overhead** | - | **~0.25%** |

**Success rate:** 53% (10,710 drops vs 9,597 rewinds)

This is a healthy ratio - about half the speculative parses succeed.

### Sources of Checkpoint/Rewind

| Module | Pattern | Calls | Notes |
|--------|---------|-------|-------|
| `Keywords` | Keyword list detection | ~10,000+ | Multiple try_parse sites |
| `Maps` | Map update detection | 926 | `try_parse_map_update` |
| `Stabs` | Paren stab detection | ~258 | Now uses classifier first |
| `Brackets` | Container parsing | ~125 | Single site |
| `Blocks` | Do-block clauses | ~124 | `try_parse_clause` |
| `Dots` | Keyword after dot | varies | In dot call handling |
| `CallsPrivate` | Expression parsing | varies | No-parens ambiguity |

## Token Operation Costs (Main Bottleneck)

| Function | Calls | Time % |
|----------|-------|--------|
| `Toxic.Driver.next/2` | 638,165 | **3.24%** |
| `TokenAdapter.metadata/4` | 285,400 | **2.56%** |
| `TokenAdapter.next/1` | 709,746 | **2.56%** |
| `TokenAdapter.peek/1` | 1,417,348 | **2.42%** |
| `Toxic.NormalTokenizer.next/5` | 353,122 | 1.90% |
| `TokenAdapter.fetch_next/2` | 285,650 | 1.84% |
| `TokenAdapter.update_state/6` | 285,400 | 1.31% |
| `Precedence.binary/1` | 527,981 | 1.32% |
| `TokenAdapter.consume_fuel/1` | 709,746 | 1.15% |
| **Total token operations** | - | **~18%** |

## Maps Module Analysis

User suspected maps might be the next problem. Here's the data:

| Function | Calls | Time % | µs/call |
|----------|-------|--------|---------|
| `try_parse_map_update/3` | 926 | 0.00% | 0.11 |
| `parse_map_update_candidate/3` | 100 | 0.00% | 0.51 |
| `parse_map_close/3` | 853 | 0.01% | 0.27 |
| `parse_map_args_body/7` | 1,046 | 0.01% | 0.29 |
| `parse_map/4` | 1,046 | 0.00% | 0.12 |

**Conclusion:** Maps is NOT the bottleneck. Total Maps time is ~0.05%.

The checkpoint/rewind in `try_parse_map_update/3` is called only 926 times and accounts for negligible overhead.

## Keywords Module Analysis

| Function | Calls | Time % |
|----------|-------|--------|
| `starts_kw?/1` | 151,473 | 0.40% |
| `try_parse_kw_call/4` | 37,848 | 0.26% |
| `try_parse_kw_data/4` | 26,353 | 0.18% |
| `try_parse_call_args_no_parens_kw/4` | 10,502 | 0.09% |
| **Total Keywords overhead** | - | **~0.93%** |

`starts_kw?/1` is the main probe (151k calls) but is cheap at 0.08 µs/call.

## What Could Be Optimized?

### 1. Token Metadata (2.56% of time)

`TokenAdapter.metadata/4` is called 285,400 times (once per token). It builds:
```elixir
%{
  range: %{start: ..., end: ...},
  delimiter: ...,
  newlines: ...,
  synthesized?: ...,
  terminators: ...,
  role: ...
}
```

**Potential optimization:** Lazy metadata - only compute when needed. However, conformance testing may require full metadata.

### 2. Position.to_location (0.94% of time)

Called 570,800 times (~2x per token for start/end positions).

**Potential optimization:** Batch position lookups or cache line index lookups.

### 3. Precedence Lookups (1.32% of time)

`Precedence.binary/1` is called 527,981 times.

**Potential optimization:** Already discussed - compile-time function clause expansion would help, but the impact is limited.

### 4. Fuel Checking (1.15% of time)

`consume_fuel/1` is called 709,746 times. Most runs use `:infinity` fuel.

**Potential optimization:** Fast-path for `:infinity` (already exists but still incurs function call overhead).

## Comparison: Before vs After Stab Refactor

Based on earlier profiling data (ANALYSIS_GPT.md):

| Metric | Before | After | Change |
|--------|--------|-------|--------|
| Token peek/next ratio | ~2.7x higher | Normal | Fixed |
| checkpoint calls | 134,062 | 20,307 | **-85%** |
| rewind calls | 74,439 | 9,597 | **-87%** |
| Stab-related overhead | ~5% | ~0.1% | **Fixed** |

## Conclusions

1. **The stab refactor was successful** - it reduced checkpoint/rewind by 85-87%.

2. **No single algorithmic problem remains** - the parser is now limited by fundamental token-by-token parsing costs.

3. **Maps is NOT the issue** - the map update detection checkpoint/rewind is negligible (~0.01%).

4. **Keywords module is acceptable** - the probe overhead is reasonable (~0.93% total).

5. **Next optimization targets** should be micro-optimizations in hot paths:
   - Token metadata construction (2.56%)
   - Position calculations (0.94%)
   - Precedence lookups (1.32%)
   - State update patterns (1.31%)

6. **The lexer is now the ceiling** - `Toxic.Driver.next/2` at 3.24% is the single hottest function. Parser optimizations have diminishing returns beyond this point.

## Recommendation

The parser is now well-optimized for its architecture. Further significant improvements would require:

1. **Architectural changes** to reduce token traffic (e.g., token batching, lazy parsing)
2. **Lexer optimizations** in the Toxic tokenizer
3. **State representation changes** to reduce map operations

The current ~400ms for 50 files (~1.3MB) translates to about 3.2 MB/sec throughput, which is reasonable for a token-by-token recursive descent parser with full AST conformance.
