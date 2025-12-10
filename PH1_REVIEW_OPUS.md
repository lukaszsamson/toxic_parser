# Phase 0 & Phase 1 Implementation Review

**Reviewer:** Claude Opus 4.5
**Date:** 2025-12-10
**Status:** Implementation is on track with well-defined contracts. Several gaps need addressing before marking phases complete.

---

## Summary

The implementation establishes a solid foundation for the error-tolerant parser. Core contracts are defined and the architectural separation is clean. The current implementation correctly delegates to `Code.string_to_quoted_with_comments/2` as a placeholder while infrastructure is built.

**Overall Direction:** ✅ Correct - matches HLD and HL_PLAN requirements

---

## Phase 0: Contracts & Harness

### What's Done ✅

| Requirement | Status | Notes |
|-------------|--------|-------|
| Strict vs tolerant mode documented | ✅ | `ToxicParser.option` typespec, State.mode field |
| Event log schema locked | ✅ | `EventLog` module with comprehensive types |
| Metadata policy defined | ✅ | `EventLog.metadata()` type with range, delimiters, newlines, synthesized?, terminators, role |
| Error node shape locked | ✅ | `{:__error__, map(), map()}` in `Builder.error_node/2` |
| Builder behaviour defined | ✅ | `init/1`, `handle_event/2`, `finalize/1` callbacks |
| Conformance harness wired | ✅ | `Conformance.compare/2` compares both modes against reference |
| NONTERMINALS_GPT.md adopted | ✅ | Loaded at compile-time in `Nonterminals` module |

### Gaps to Address 🔴

1. **Strict vs tolerant behavioral documentation incomplete**
   - The `ToxicParser` moduledoc mentions "Phase 0 implements mode handling" but doesn't spell out:
     - What makes lexer errors "fatal" in strict mode
     - How synthesized closers are suppressed in strict mode
     - Diagnostics aggregation policy differences between modes
   - **Action:** Add a `@moduledoc` section or separate doc file detailing mode behaviors

2. **Event ordering guarantees undocumented**
   - `EventLog` moduledoc says events are "emitted in source order" and "well-nested"
   - Missing: explicit contract on `start_node`/`end_node` nesting invariants
   - **Action:** Add formal ordering specification (e.g., "every `start_node` must have corresponding `end_node` before parent's `end_node`")

3. **Comment policy not specified**
   - HLD mentions "Default: dropped. Optional: preserve via Toxic callback"
   - Current impl passes `preserve_comments` to Toxic but policy isn't documented
   - **Action:** Document when comments attach to following vs preceding nodes, and trivia handling

4. **No CI integration yet**
   - HL_PLAN says "wire into CI early"
   - Conformance tests exist but no evidence of CI setup
   - **Action:** Add GitHub Actions or similar CI config

---

## Phase 1: Token Adapter & State

### What's Done ✅

| Requirement | Status | Notes |
|-------------|--------|-------|
| State struct with required fields | ✅ | `stream`, `lookahead`, `diagnostics`, `mode`, `opts`, `fuel`, `expression_context`, `checkpoints`, `line_index`, `terminators`, `max_peek`, `source` |
| Token adapter `next/peek/peek_n/checkpoint/rewind` | ✅ | All implemented with proper signatures |
| Bounded `peek_n` | ✅ | Raises `ArgumentError` when `n > max_peek` |
| `current_terminators` exposed | ✅ | `TokenAdapter.current_terminators/1` |
| EOE abstraction | ✅ | `:eol` and `:;` normalized to `:eoe` with `source` and `newlines` preserved |
| Error token surfacing | ✅ | `{:error_token, meta, %Toxic.Error{}}` → diagnostic + error token |
| Synthesized closer detection | ✅ | `synthesized?` computed from zero-width range (`sl == el and sc == ec`) |
| Line index for offset calculation | ✅ | Precomputed in `State.new/2` |

### Gaps to Address 🔴

1. **`*_op_eol` metadata not explicitly handled**
   - HL_PLAN Phase 1 says: "respect `*_op_eol` metadata for layout-sensitive operators"
   - Current `normalize_token` handles `:eol` and `:;` but doesn't distinguish operator-following EOL
   - Example: after `+`, EOL should behave differently than after `1`
   - **Action:** Document or implement how `*_op_eol` operators affect EOE behavior (may be deferred to Pratt layer but should be noted)

2. **Tolerant mode doesn't fully suppress fatal behavior**
   - In `fetch_next/2`, when `Toxic.next` returns `{:error, reason, stream}`, the adapter returns `{:error, diagnostic, state}`
   - In tolerant mode, this should probably continue parsing rather than propagating error
   - Compare: `error_token` is surfaced as a token and parsing continues, but stream-level errors stop parsing
   - **Action:** Clarify/fix error handling in tolerant mode - should stream errors produce error tokens and continue?

3. **Checkpoint doesn't save stream position explicitly**
   - `checkpoint/1` saves parser state but delegates stream checkpointing to `Toxic.checkpoint`
   - If Toxic checkpoint behavior changes, this could silently break
   - **Action:** Add test verifying stream position restoration after rewind

4. **Missing pushback capability**
   - HLD mentions "pushback" in Token Adapter layer
   - Current implementation has no explicit pushback - only checkpoint/rewind
   - **Action:** Determine if pushback is needed separately from checkpoint/rewind, or if it's subsumed by lookahead management

5. **Identifier flavor classification missing**
   - HLD says adapter "normalizes Toxic tokens into parser views (identifier flavors, operator classes, structural delimiters)"
   - Current `normalize_token` preserves raw token kinds without classification
   - **Action:** Add identifier classification (`identifier`, `paren_identifier`, `bracket_identifier`, `do_identifier`, `op_identifier`) - may be Phase 5 scope but adapter should have hooks

6. **Test coverage gaps**
   - No test for `peek_n` exceeding `max_peek` (raises)
   - No test for stream-level error handling (non-token errors)
   - No test for checkpoint cleanup on successful parse
   - **Action:** Add targeted tests for edge cases

---

## Event Log & Builder (Phase 2 Preview)

### What's Done ✅

- Event types fully specified
- `Builder` behaviour defined
- `Builder.reduce/3` streams events through builder
- `Builder.error_node/2` helper creates locked error shape

### Gaps for Phase 2 🟡

1. **No event emission functions implemented**
   - `EventLog` is just type definitions; no `emit_start_node/3`, `emit_token/3`, etc.
   - These will be needed when grammar/Pratt layers are built

2. **No concrete builder implementation**
   - `Builder` is a behaviour only; no AST builder exists yet
   - This is expected for Phase 2

---

## Conformance Testing

### What's Done ✅

- `Conformance.compare/2` parses in both modes and returns reference AST/comments
- Basic test exercises the harness

### Gaps 🔴

1. **No AST comparison logic**
   - `compare/2` returns both results but doesn't compare them
   - Need: meta-insensitive AST equality check
   - **Action:** Add `Conformance.assert_ast_equal/2` or similar

2. **Test suite is minimal**
   - Only 5 tests total across both test files
   - No corpus tests, no property tests
   - **Action:** Build test infrastructure for later phases

---

## Code Quality Observations

### Strengths

1. **Clean type specifications throughout** - Every module has comprehensive `@type` and `@spec`
2. **Good separation of concerns** - State, TokenAdapter, EventLog, Builder, Error are well-factored
3. **Consistent metadata structure** - Same range/location format everywhere
4. **Defensive design** - `max_peek` bounds, `@enforce_keys` on structs

### Minor Issues

1. **Duplicated `to_location/3`** - Appears in both `TokenAdapter` and `Error` modules
   - **Action:** Extract to shared utility or have Error use TokenAdapter's version

2. **`synthesized?` detection is fragile**
   - Zero-width range check (`sl == el and sc == ec`) may false-positive on actual zero-width tokens
   - **Action:** Consider explicit `synthesized` flag from Toxic instead of inferring

3. **Unused fields in some cases**
   - `Result.env` is always `[]`; `Result.events` is always `[]`
   - Expected for stub implementation, but ensure these get populated

---

## Checklist for Phase 0 Completion

- [ ] Document strict vs tolerant mode behavioral differences
- [ ] Specify event ordering invariants formally
- [ ] Document comment/trivia attachment policy
- [ ] Add CI pipeline configuration
- [ ] Add AST comparison utility to conformance harness

## Checklist for Phase 1 Completion

- [ ] Clarify/document `*_op_eol` handling (defer or implement)
- [ ] Fix/document stream-level error handling in tolerant mode
- [ ] Add pushback documentation (confirm subsumed by checkpoint/rewind or add)
- [ ] Add identifier classification hooks (or defer to Phase 5)
- [ ] Add tests for: peek_n overflow, stream errors, checkpoint cleanup
- [ ] Deduplicate `to_location/3` helper

---

## Recommendation

**Phase 0:** 80% complete - primarily missing documentation and CI. Can be marked done once docs are added.

**Phase 1:** 90% complete - core functionality is solid. The `*_op_eol` and identifier classification may be Phase 3-5 scope. Tolerant mode error handling needs clarification. Mark done after fixing stream error handling and adding tests.

**Next Priority:** Move to Phase 2 (Event Log emission functions + Builder skeletons) in parallel with addressing Phase 0/1 gaps. The token adapter is ready to support parser development.
