# Phase 4 Review

## Summary
Phase 4 (Grammar Layer — Expressions) is **INCOMPLETE**. While the module structure (`ToxicParser.Grammar` and `ToxicParser.Grammar.Expressions`) has been created, the actual recursive descent logic required by the plan is almost entirely missing. The current implementation is a skeleton that delegates directly to the Pratt parser without implementing the grammar rules.

## Status: INCOMPLETE

### Delivered
*   **Module Structure:** `ToxicParser.Grammar` and `ToxicParser.Grammar.Expressions` exist.
*   **Dispatcher Stub:** `Grammar.parse_string/2` and `Expressions.expr/3` exist but are mere pass-throughs.
*   **Position Helper:** `ToxicParser.Position` is implemented and used for zero-metadata generation.

### Gaps (Critical)
1.  **Top-Level Grammar:** The `grammar -> eoe? expr_list eoe?` rule is not implemented. `expr_list` currently parses a single expression and returns, ignoring the requirement for EOE-separated lists.
2.  **Expression Families:** The plan requires distinct handling for `matched`, `unmatched`, and `no_parens` families. The current `expr/3` function blindly passes the context to `Pratt.parse/3` without the required dispatch logic or family-specific rules.
3.  **Block Expressions:** The `block_expr` nonterminal (handling `dot_do_identifier`, paren + nested paren + do) is completely missing. There is no logic to handle `do` blocks attached to expressions.
4.  **No-Parens Rules:** The complex `no_parens_one`, `no_parens_many`, and `no_parens_one_ambig` rules are missing. There is no ambiguity resolution logic or warning generation.
5.  **EOE Handling:** Centralized EOE consumption (normalizing `;` and newlines) is not implemented in the grammar layer.

## Recommendations
To mark Phase 4 as done, the `ToxicParser.Grammar.Expressions` module must be expanded to:
1.  Implement the `expr_list` loop that consumes expressions separated by EOEs.
2.  Implement the `block_expr` logic to handle `do` blocks.
3.  Implement the specific recursive descent rules for `no_parens` variants.
4.  Correctly dispatch `matched`/`unmatched` contexts to the appropriate Pratt entry points or sub-rules.
