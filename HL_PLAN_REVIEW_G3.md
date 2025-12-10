# Implementation Plan Review (G3)

## General Assessment
The plan `HL_PLAN.md` provides a comprehensive roadmap for implementing `ToxicParser`. It correctly identifies the architectural layers (Token Adapter, Pratt, Grammar, Builders) and aligns well with the High-Level Design (`HLD_DRAFT_1.md`) and the canonical grammar (`elixir_parser.yrl`).

However, the ordering of certain foundational elements (Event Log, AST Building, Testing) presents a risk of "big bang" integration at the end. The plan implies implementing the entire grammar (Phases 3-7) before finalizing the Event Log (Phase 8) or AST Builders (Phase 11), which contradicts the HLD's statement that "Event log is the primary parser output".

## Strategic Improvements

### 1. "Event Log First" Approach
**Current:** Phase 8 (Event Log) comes after all grammar phases.
**Risk:** If the grammar layers are implemented without emitting events, they will need significant refactoring to add event emission later.
**Suggestion:** Move **Phase 8 (Event Log & Infrastructure)** to **Phase 1.5** (immediately after Token Adapter). The grammar functions in Phases 3-7 should emit events *as they are written*.

### 2. Continuous AST Verification
**Current:** Phase 11 (AST Building) is near the end.
**Risk:** We won't be able to verify if the parser produces correct Elixir AST until Phase 11.
**Suggestion:** Integrate AST building into the grammar phases.
-   Define `ToxicParser.Builder` in Phase 2.
-   In Phases 3-7, implement the specific builder functions for that feature (e.g., `build_list` in Phase 5).
-   Use Phase 11 only for "Polish & Metadata" or complex rewrites.

### 3. Early Test Harness
**Current:** Phase 12 (Testing) is the last step.
**Risk:** We fly blind until the end.
**Suggestion:** Create a **Phase 0 or 1.5** task to set up a "Conformance Test Harness" that compares `ToxicParser` output against `Code.string_to_quoted/2`. This allows TDD for every subsequent phase.

### 4. Leverage Spitfire Assets
**Context:** `spitfire` contains valuable property test generators (`spitfire/test/spitfire/property`).
**Suggestion:** Explicitly include a task to port or utilize `spitfire`'s property tests and grammar generators to validate `ToxicParser`.

## Specific Phase Adjustments

### Phase 1: Foundation & Token Adapter
*   **Add:** Define `ToxicParser.Event` struct/types here.
*   **Clarify:** Explicitly state that the Token Adapter must handle `Toxic`'s **linear** stream (as per `ALL_TOKENS.md`), especially for handling `eol` coalescing and `identifier` vs `paren_identifier` lookahead.

### Phase 2: Pratt Expression Parser Core
*   **Add:** Implement basic AST builder helpers for literals and ops.
*   **Add:** Setup the "Conformance Test Harness" here to verify simple expressions (e.g., `1 + 2`).

### Phases 3-7 (Grammar Layers)
*   **Modify:** Each phase should include "Implement Event emission and AST construction for [feature]".
*   **Phase 7 (Strings):** Explicitly mention handling `Toxic`'s fragmented string tokens (`:bin_string_start`, `:string_fragment`, etc.) within the grammar or a specialized sub-parser, as they are not pre-collapsed.

### Phase 10: Error Recovery System
*   **Refinement:** While the *logic* for recovery fits here, the *hooks* (sync points) need to be present in the grammar functions from the start.
*   **Suggestion:** In Phases 3-7, ensure grammar functions accept a `context` or `terminators` argument to support future sync logic, even if initially unused.

### Phase 12: Public API & Testing
*   **Refinement:** Focus this phase on "Public API Polish" and "Large Scale Validation" (Hex corpus), assuming unit/conformance testing happened throughout.

## Missing Items / Clarifications Needed
1.  **Spitfire Integration:** Clarify if `ToxicParser` is intended to replace `Spitfire`'s current parser or sit alongside it. The plan mentions `Spitfire.Env` but not the rest of the codebase.
2.  **Token Stream Mode:** Confirm if the parser consumes the raw linear stream (as implied by Phase 7) or if the Token Adapter should collapse strings first. (Recommendation: Consume raw stream for maximum fidelity/error tolerance, but it adds complexity).
