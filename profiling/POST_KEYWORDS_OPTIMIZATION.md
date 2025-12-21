# Post-Keywords Optimization Profiling

**Date:** 2025-12-18

## Checkpoint/Rewind Comparison

| Metric | Before | After | Change |
|--------|--------|-------|--------|
| `checkpoint/1` | 20,307 | 18,990 | **-1,317 (-6.5%)** |
| `rewind/2` | 9,597 | 9,597 | 0 (same) |
| `drop_checkpoint/2` | 10,710 | 9,393 | **-1,317 (-12.3%)** |

## Analysis

The optimization reduced checkpoints by 1,317 calls. Interestingly:
- Rewinds stayed the same (9,597)
- Only drops decreased (-1,317)

This means the optimization only eliminated checkpoints that would have been **successfully dropped** (not rewound). The cases that rewind are still rewinding because they're:
1. Quoted keyword keys that turn out to be regular strings
2. Other checkpoint sites not affected by this optimization

## Remaining Checkpoint Sources

Current checkpoints: 18,990

| Source | Estimated Calls | Notes |
|--------|-----------------|-------|
| `keywords.ex` (quoted keys) | ~9,000 | For `"foo":` style keys |
| `maps.ex:367` | 926 | Map update detection |
| `brackets.ex:39` | 125 | Multi-element bracket detection |
| `stabs.ex:120` | ~258 | Paren stab fallback |
| `calls_private.ex:72` | varies | No-parens arg detection |
| `dots.ex` (quoted keys) | varies | Only for quoted key rejection |
| Other | varies | - |

## Performance

| Metric | Before | After |
|--------|--------|-------|
| Baseline (50 files) | ~374-417 ms | ~353-378 ms |
| Total calls | 39,199,555 | 39,185,068 |

Slight improvement in baseline time (~3-5% faster on best runs).

## Conclusion

The keywords optimization provides a modest improvement (~6.5% fewer checkpoints). The remaining checkpoints are primarily from:
1. Quoted keyword key handling (unavoidable without deeper lookahead)
2. Other speculative parsing sites

Further optimization would require:
1. Deeper lookahead for quoted keys (scan past string to find `:`)
2. Eliminating speculative parsing in other modules
