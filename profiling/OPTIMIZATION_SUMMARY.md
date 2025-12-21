# Checkpoint/Rewind Optimization Summary

**Date:** 2025-12-18

## Optimization Stages

### Stage 1: Post-Stab Refactor (Baseline)

| Metric | Value |
|--------|-------|
| checkpoint/1 | 20,307 |
| rewind/2 | 9,597 |
| drop_checkpoint/2 | 10,710 |
| Baseline time | ~374-417 ms |

### Stage 2: Keywords + Dots Optimization

Avoided checkpoint for definite `kw_identifier` tokens.

| Metric | Value | Change |
|--------|-------|--------|
| checkpoint/1 | 18,990 | -6.5% |
| rewind/2 | 9,597 | 0% |
| drop_checkpoint/2 | 9,393 | -12.3% |
| Baseline time | ~353-378 ms | ~5% faster |

### Stage 3: String Lookahead

Scan quoted strings to determine if keyword key without checkpointing.

| Metric | Value | Change from Stage 2 | Change from Baseline |
|--------|-------|---------------------|----------------------|
| checkpoint/1 | 16,728 | **-12%** | **-17.6%** |
| rewind/2 | 7,593 | **-21%** | **-20.9%** |
| drop_checkpoint/2 | 9,135 | -3% | -14.7% |
| Baseline time | ~345-378 ms | ~3% faster | **~8% faster** |

## Final Results

| Metric | Before | After | Reduction |
|--------|--------|-------|-----------|
| Checkpoints | 20,307 | 16,728 | **-17.6%** |
| Rewinds | 9,597 | 7,593 | **-20.9%** |
| Drops | 10,710 | 9,135 | **-14.7%** |
| Total calls | 39,199,555 | 38,920,041 | -0.7% |
| Baseline (min) | ~374 ms | ~345 ms | **~8% faster** |

## New Function: String Lookahead

```
quoted_string_is_keyword?/1    1,507 calls    0.00%    84 µs total
scan_to_string_end/4           6,422 calls    0.04%   944 µs total
```

The lookahead scans ~4.3 tokens per string on average (6,422 / 1,507).

## Token Traffic Comparison

| Function | Before | After | Change |
|----------|--------|-------|--------|
| `peek/1` | 1,417,348 | 1,397,456 | -1.4% |
| `next/1` | 709,746 | 709,434 | -0.04% |
| `pushback_many/2` | 15,137 | 16,644 | +10% |

The `pushback_many` increase is expected from the string lookahead restoring scanned tokens.

## Remaining Checkpoint Sources

Current: 16,728 checkpoints

| Source | Estimated | Notes |
|--------|-----------|-------|
| Maps update detection | ~926 | `try_parse_map_update` |
| Stabs fallback | ~258 | Classifier `:unknown` cases |
| Brackets | ~125 | Multi-element detection |
| Remaining quoted keys | varies | Complex interpolations |
| calls_private | varies | No-parens detection |
| Other | varies | - |

## Performance Summary

- **8% faster baseline** (374 → 345 ms best case)
- **21% fewer rewinds** (speculative parsing that fails)
- **18% fewer checkpoints** (less state saving overhead)
- Token traffic essentially unchanged (-0.7%)

## Conclusion

The three-stage optimization successfully reduced checkpoint/rewind overhead:
1. Stab refactor (earlier) - eliminated the main algorithmic issue
2. Keywords optimization - avoided checkpoints for definite keywords
3. String lookahead - avoided checkpoints for quoted strings

The parser is now well-optimized. Remaining checkpoint usage is for genuinely ambiguous cases that require speculative parsing.
