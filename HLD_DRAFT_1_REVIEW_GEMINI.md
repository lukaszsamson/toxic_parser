# Design Review: Error-Tolerant Elixir Parser (HLD Draft 1)

## Summary
The proposed design in `HLD_DRAFT_1.md` is a significant improvement over the previous Spitfire architecture. By adopting a **Hybrid Recursive Descent + Pratt** approach and strictly mirroring the `elixir_parser.yrl` non-terminals, it addresses the core fragility issues of the original implementation. The integration with Toxic and the focus on error tolerance are well-conceived.

However, there are specific areas where the design risks over-complication or deviation from the reference grammar's philosophy.

## Critique & Improvements

### 1. Context Management: Syntactic vs. Semantic
**Current Design:** Proposes tracking both syntactic contexts (`:matched`, `:unmatched`, `:no_parens`) and semantic modes (`:value`, `:pattern`, `:guard`).
**Critique:** The `elixir_parser.yrl` grammar is structural, not semantic. It uses `expr` in patterns and guards alike. Enforcing "guard-safe" operators during parsing is a violation of separation of concerns and adds unnecessary state.
**Improvement:**
*   **Keep Syntactic Contexts:** `:matched`, `:unmatched`, `:no_parens` are essential for resolving `do` block and call ambiguity.
*   **Drop Semantic Modes:** Remove `:value | :pattern | :guard` from the parser state. Parse all expressions generically. Validation of "valid pattern" or "valid guard" should happen in a post-parse pass or be flagged by the AST builder, not prevent parsing.

### 2. Lookahead and `peek_n`
**Current Design:** Uses `peek_n` for disambiguation.
**Critique:** As noted by the reviewer, Elixir is LALR(1), meaning 1 token of lookahead is theoretically sufficient for the grammar structure. Over-reliance on `peek_n` (LL(k)) can lead to fragile, ad-hoc decision logic that drifts from the grammar.
**Improvement:**
*   Restrict `peek_n` usage to exactly where `yrl` uses it (which is implicitly handled by state shifts in LALR).
*   Document the specific ambiguous cases requiring >1 lookahead (e.g., `cmd [key: val]` vs `[key: val]`).
*   Default to `peek(1)`.

### 3. Event Log vs. Direct AST Build
**Current Design:** Mentions "Builders + Event Log" but focuses on AST construction.
**Critique:** Direct AST construction couples the parser to a specific output format. An Event Log (like Prism/YARP) allows the parser to be a pure "structure recognizer" that emits events (`start_call`, `end_call`, `token`). This enables multiple consumers: a Core-AST builder, a CST builder (full fidelity), and an Outline builder (fast indexing).
**Improvement:**
*   Make the **Event Log** the primary output interface of the parser core.
*   Implement the "Core AST Builder" as a consumer of this log.
*   This cleanly separates parsing logic from AST node construction logic.

### 4. Testing Strategy
**Current Design:** Mentions conformance and resilience testing.
**Improvement:**
*   **Property-Based Testing** is critical. Generate random token streams and ensure the parser *never* crashes.
*   **Corpus Testing:** Parse the entire Hex corpus. The goal should be 100% non-crashing on all files, and 100% AST equivalence on valid files.

### 5. Specific Grammar Details
*   **`matched_expr` / `unmatched_expr`**: The design correctly identifies these as the key to Elixir's grammar. This is the most important structural decision and it is correct.
*   **EOE Handling**: Centralizing `eoe` (End of Expression) handling is a massive win over Spitfire's ad-hoc approach.

## Recommendation
**Approve with Modifications.**
Proceed with the Hybrid Recursive Descent + Pratt architecture. Strip out semantic context tracking. Adopt the Event Log pattern as the primary interface to ensure future-proofing for IDE features (CST/Green Trees).
