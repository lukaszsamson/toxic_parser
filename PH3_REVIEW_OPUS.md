# Phase 2 & Phase 3 Implementation Review

**Reviewer:** Claude Opus 4.5
**Date:** 2025-12-10
**Status:** Phase 2 complete. Phase 3 skeleton in place with critical binding power discrepancies.

---

## Summary

Phase 2 (Event Log & Builder Skeletons) is fully implemented with well-tested event emission and builder helpers. Phase 3 (Pratt Expression Core) establishes the API shape but has significant gaps: the binding power table diverges from HLD specifications, and the actual Pratt parsing loop (NUD/LED parselets) is intentionally stubbed.

**Overall Direction:** ✅ Architecturally sound, but binding powers need alignment with HLD

---

## Phase 2: Event Log & Builder Skeletons

### What's Done ✅

| Requirement | Status | Location |
|-------------|--------|----------|
| Event emitters implemented | ✅ | `EventLog.start_node/3`, `end_node/3`, `token/3`, `error/3`, `missing/3`, `synthetic/3`, `comment/3` |
| Ordering guarantees documented | ✅ | `EventLog` moduledoc lines 5-12 |
| Well-nested enforcement | ✅ | `end_node/3` raises on mismatch |
| Event log struct with stack | ✅ | `%EventLog{events: [], stack: []}` |
| Builder helpers for literals | ✅ | `Helpers.literal/1` |
| Builder helpers for unary | ✅ | `Helpers.unary/3` |
| Builder helpers for binary | ✅ | `Helpers.binary/4` |
| Builder helpers for dot | ✅ | `Helpers.dot/3` |
| Builder helpers for access | ✅ | `Helpers.access/3` |
| Dotted alias concatenation | ✅ | `Helpers.alias_segments/2` |
| Tests for event log | ✅ | `event_log_test.exs` |
| Tests for builder helpers | ✅ | `builder_helpers_test.exs` |

### Quality Notes

1. **Event ordering is well-documented** (lines 5-12 of `EventLog`):
   - Events in source order ✅
   - `start_node`/`end_node` strictly well-nested ✅
   - `missing`/`synthetic`/`error` at expected position ✅
   - `comment` precedes attached node ✅

2. **Stack-based nesting validation** ensures CST builders receive balanced events

3. **`to_list/1`** reverses accumulated events for emission-order output

### Minor Gaps 🟡

1. **No event threading through State yet**
   - `EventLog` is standalone; not integrated into `State` struct
   - Parser will need to carry both `State` and `EventLog`
   - **Action:** Consider adding `event_log` field to `State` or establish passing convention

2. **Missing `call` helper**
   - `Helpers` has `unary`, `binary`, `dot`, `access` but no `call/3` or `call/4`
   - Will be needed for function call AST construction
   - **Action:** Add `call(callee, args, meta)` helper

### Phase 2 Status: **COMPLETE** ✅

---

## Phase 3: Pratt Expression Core

### What's Done ✅

| Requirement | Status | Notes |
|-------------|--------|-------|
| Binding power table defined | ✅ | `Precedence` module with `@binary_bp` and `@unary_bp` |
| `not_in` accessor | ✅ | `Precedence.not_in/0` |
| `dot` accessor | ✅ | `Precedence.dot/0` |
| `access` accessor | ✅ | `Precedence.access/0` |
| Pratt context type | ✅ | `@type context :: :matched \| :unmatched \| :no_parens` |
| Pratt result type | ✅ | `{:ok, Macro.t(), State.t(), EventLog.t()} \| {:error, ...}` |
| Pratt `parse/3` stub | ✅ | Returns peeked token value (placeholder) |
| BP exposure functions | ✅ | `Pratt.bp/1`, `Pratt.unary_bp/1` |
| Tests for precedence table | ✅ | `pratt_precedence_test.exs` |

### Critical Gap: Binding Power Table Mismatch 🔴

The HLD specifies binding powers that differ significantly from the implementation:

| Operator | HLD BP | Implemented BP | Status |
|----------|--------|----------------|--------|
| `do` | 5 (L) | **missing** | ❌ |
| `->` (stab) | 10 (R) | **missing** | ❌ |
| `,` | 20 (L) | **missing** | ❌ |
| `when` | 50 (R) | 30 | ⚠️ different |
| `::` (type) | 60 (R) | 40 | ⚠️ different |
| `\|` (pipe sep) | 70 (R) | **missing as pipe_sep** | ❌ |
| `=>` (assoc) | 80 (R) | **missing** | ❌ |
| `&`/`...` | 90 (nonassoc) | 200 (capture_op) | ⚠️ very different |
| `=` (match) | 100 (R) | **missing** | ❌ |
| `or`/`\|\|`/`\|\|\|` | 120 (L) | 50 | ⚠️ different |
| `and`/`&&`/`&&&` | 130 (L) | 60 | ⚠️ different |
| comparisons | 140 (L) | 70 | ⚠️ different |
| relations | 150 (L) | 80 | ⚠️ different |
| arrow family | 160 (L) | 130 | ⚠️ different |
| `in`/`not in` | 170 (L) | 100 | ⚠️ different |
| `^^^` (xor) | 180 (L) | 120 | ⚠️ different |
| `//` (ternary) | 190 (R) | **missing explicit** | ❌ |
| concat/range | 200 (R) | 140/150 | ⚠️ different |
| `+`/`-` (dual) | 210 (L) | 160 | ⚠️ different |
| `*`/`/` (mult) | 220 (L) | 170 | ⚠️ different |
| `**` (power) | 230 (L) | 180 | ⚠️ different |
| unary | 300 (nonassoc) | 190 | ⚠️ different |
| dot/dot-call | 310 (L) | 210 | ⚠️ different |
| `@` | 320 (nonassoc) | **missing** | ❌ |
| access | 330 (nonassoc) | 220 | ⚠️ different |

**Key Issues:**
1. **Missing operators:** `do`, `->`, `,`, `=>`, `=`, `@`, `\\` (default arg)
2. **Relative ordering preserved** but absolute values differ - this is fine
3. **`capture_op` BP (200) is higher than dot (210)** - HLD says `&` is 90, below `=` at 100

**Action Required:**
- Add missing operators to the table
- Verify relative ordering matches `elixir_parser.yrl` precedence
- Document that absolute values don't matter, only relative ordering

### Missing NUD Parselets 🔴

| Parselet | Status | Notes |
|----------|--------|-------|
| Literals (int, float, atom, string) | ❌ | Stub returns raw token |
| Identifiers | ❌ | |
| Unary `@` | ❌ | Module attribute |
| Unary `&` | ❌ | Capture |
| Unary `!` | ❌ | Boolean not |
| Unary `^` | ❌ | Pin |
| Unary `not` | ❌ | Boolean not |
| Unary `+`/`-` | ❌ | Sign |
| Containers (list/tuple/map/bitstring) | ❌ | Deferred to grammar |
| `fn` | ❌ | Deferred to grammar |
| `...` | ❌ | Ellipsis |
| Nullary `..` | ❌ | Unbounded range |
| `capture_int` | ❌ | `&1` etc. |
| Parenthesized expr | ❌ | `(expr)` |

### Missing LED Parselets 🔴

| Parselet | Status | Notes |
|----------|--------|-------|
| Binary operators | ❌ | All `*_op` classes |
| Dot access | ❌ | `foo.bar` |
| Dot call | ❌ | `foo.bar()` |
| Access `[]` | ❌ | `foo[key]` |
| `not in` | ❌ | Combined operator |
| `..//` step | ❌ | Range with step |

### Missing Associativity Handling 🔴

The HLD specifies associativity for each operator class:
- (L) = left-associative
- (R) = right-associative
- (nonassoc) = non-associative

Current `Precedence` module only stores binding powers, not associativity.

**Action:** Add associativity to the precedence table:
```elixir
@binary_bp [
  {:when_op, 50, :right},
  {:type_op, 60, :right},
  {:or_op, 120, :left},
  ...
]
```

### Missing `dual_op` Spacing Rules 🔴

HLD Section 5 mentions: "Honor `dual_op` spacing rules from token shapes"

`dual_op` (`+`/`-`) can be binary or unary depending on spacing:
- `1 + 2` → binary
- `1 +2` → binary (no space before)
- `+ 2` → unary (space after identifier/operator)

No implementation of this disambiguation exists.

**Action:** Document how Toxic provides spacing info and how Pratt will use it

### Phase 3 Status: **SKELETON ONLY** 🟡

The `Pratt.parse/3` stub explicitly states (line 5-6):
> "Phase 3 implements binding power lookup and API shape; full parselets are implemented in later phases."

This is acceptable if the plan is to implement parselets in Phase 4+, but:
1. Binding powers need correction
2. Associativity needs adding
3. The actual Pratt loop structure should be sketched

---

## Cross-Phase Integration Notes

### EventLog + Pratt Integration

The `Pratt.parse/3` signature includes `EventLog.t()`:
```elixir
@spec parse(State.t(), context(), EventLog.t()) :: result()
```

This is correct - events should flow through parsing.

### Builder + Pratt Integration

`Builder.Helpers` functions are ready for use by parselets:
- `Helpers.binary(:+, left, right, meta)` for binary ops
- `Helpers.unary(:-, operand, meta)` for unary ops

### State + Context Threading

The `State` struct has `expression_context` field but `Pratt.parse/3` takes context as parameter. Need to decide:
1. Use `State.expression_context` as default
2. Or always pass explicitly

**Recommendation:** Pass explicitly to `parse/3`, use `State.expression_context` for recursive calls that don't change context.

---

## Test Coverage Analysis

### Phase 2 Tests ✅

| Test File | Coverage |
|-----------|----------|
| `event_log_test.exs` | Well-nested enforcement, all event types |
| `builder_helpers_test.exs` | unary, binary, dot, access, alias_segments |

### Phase 3 Tests 🟡

| Test File | Coverage | Gaps |
|-----------|----------|------|
| `pratt_precedence_test.exs` | BP ordering, stub behavior | No associativity tests, no NUD/LED tests |

**Missing Tests:**
- Associativity behavior
- `dual_op` spacing disambiguation
- Expression context effects
- Error recovery in Pratt loop

---

## Checklist for Phase 2 Completion

- [x] Event emitters implemented
- [x] Well-nested enforcement
- [x] Builder helpers for common AST patterns
- [x] Tests for event log
- [x] Tests for builder helpers
- [ ] Consider adding `call` helper
- [ ] Document State + EventLog threading convention

## Checklist for Phase 3 Completion

- [x] Binding power table defined
- [x] Pratt module with context type
- [x] Parse stub with correct signature
- [ ] **Fix binding power table to match HLD** (critical)
- [ ] **Add missing operators** (`do`, `->`, `,`, `=>`, `=`, `@`, `\\`)
- [ ] **Add associativity to precedence table**
- [ ] Document `dual_op` spacing handling
- [ ] Implement basic Pratt loop structure (even if parselets are stubs)
- [ ] Add tests for precedence ordering against `elixir_parser.yrl`

---

## Recommendations

### Immediate Actions

1. **Reconcile binding power table with HLD/yrl**
   - The current table appears to use different absolute values
   - Missing several operators
   - Need explicit associativity

2. **Implement minimal Pratt loop**
   - Even with stub parselets, the loop structure (`nud` → `led` loop with BP comparison) should exist
   - This validates the architecture before Phase 4

3. **Add `call` builder helper**
   - Will be needed immediately when implementing call parselets

### Deferred to Later Phases (Acceptable)

- Full NUD parselet implementations (Phase 4/5)
- Full LED parselet implementations (Phase 4/5)
- Container parsing (Grammar layer, Phase 4)
- `fn`/blocks (Grammar layer, Phase 4+)

---

## Phase Status Summary

| Phase | Status | Blocking Issues |
|-------|--------|-----------------|
| Phase 2 | ✅ Complete | Minor: add `call` helper |
| Phase 3 | 🟡 Skeleton | Binding power table mismatch, missing associativity |

**Recommendation:** Fix binding powers and add associativity before proceeding to Phase 4. The Pratt loop structure can be implemented alongside Phase 4 grammar work.
