# Phase 5 Review

## Summary
Phase 5 (Calls & Identifiers) is **INCOMPLETE**. While the module structure (`ToxicParser.Grammar.Calls` and `ToxicParser.Identifiers`) has been created, the actual parsing logic for function calls is missing. The current implementation only consumes bare identifiers and returns them as literals, failing to handle arguments, parentheses, or `do` blocks.

## Status: INCOMPLETE

### Delivered
*   **Module Structure:** `ToxicParser.Grammar.Calls` and `ToxicParser.Identifiers` exist.
*   **Identifier Classification:** `ToxicParser.Identifiers.classify/1` implements the basic mapping of token kinds.
*   **Expression List Logic (Phase 4 Fix):** `ToxicParser.Grammar.Expressions.expr_list/3` has been updated to handle sequences of expressions separated by EOE, addressing a gap from Phase 4.

### Gaps (Critical)
1.  **Call Parsing:** `Calls.parse/3` does not handle function calls. It treats identifiers as literals and returns them immediately. It needs to support:
    *   Paren calls: `foo(1, 2)`
    *   No-parens calls: `foo 1, 2`
    *   Nested paren calls: `foo(bar(1))`
2.  **Do-Block Attachment:** There is no logic to handle `do` blocks attached to calls (e.g., `foo do ... end`).
3.  **Keyword-Last Enforcement:** The requirement to enforce keyword lists being the last argument is not implemented.
4.  **Ambiguity Resolution:** No `peek_n` usage to distinguish between access syntax, list starts, and no-parens ambiguity as required by the plan.
5.  **`do:` vs `do/end`:** No logic to disambiguate or handle `do:` keywords vs `do` blocks.

## Recommendations
To mark Phase 5 as done, `ToxicParser.Grammar.Calls` needs significant expansion to:
1.  Implement `paren_call` parsing (consuming `(`, args, `)`).
2.  Implement `no_parens_call` parsing (consuming args without parens).
3.  Integrate `do_block` parsing (likely requiring a `Blocks` module from Phase 7 or a stub).
4.  Implement the `peek_n` logic to decide between `identifier [ ... ]` (access) and `identifier` followed by a list.
