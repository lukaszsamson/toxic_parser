# Issues and Refactoring Opportunities

## Logic Issues

1.  **Precedence Violation in No-Parens Calls**
    *   **Description**: `Calls.parse_no_parens_args` does not respect the current operator precedence (`min_bp`). It calls `Expressions.expr/3` which defaults to `min_bp=0`.
    *   **Impact**: Expressions like `a + b c + d` are parsed incorrectly as `a + (b (c + d))` instead of `(a + (b c)) + d`. The no-parens call `b c` greedily consumes the trailing `+ d`.
    *   **Fix**: Update `Calls.parse_no_parens_args` (and related functions) to accept a `min_bp` argument and pass it down to `Expressions.expr` (or `Pratt.parse_with_min_bp`).

2.  **Recursion Depth Risk**
    *   **Description**: `Pratt.parse_rhs` and `Pratt.nud` are mutually recursive. Deeply nested structures could potentially cause stack overflows, although Elixir's tail call optimization helps in many cases.
    *   **Impact**: Potential crash on deeply nested inputs.
    *   **Fix**: Verify tail-call optimization where possible or implement an iterative approach for deep nesting if necessary.

3.  **Error Recovery Strategy**
    *   **Description**: `Expressions.recover_expr_error` consumes a single token and checks for EOE.
    *   **Impact**: This might be too aggressive or insufficient for robust error recovery, potentially leading to cascading errors or skipped valid code.
    *   **Fix**: Implement a more robust synchronization strategy (e.g., skipping until a specific set of synchronization tokens).

## Performance Problems

1.  **Token Metadata Overhead**
    *   **Description**: `TokenAdapter.normalize_token` creates a full metadata map (including `range`, `delimiter`, `newlines`) for *every* token during `peek` and `next`.
    *   **Impact**: High allocation rate and CPU usage for metadata that might not always be used.
    *   **Fix**: Optimize metadata creation, possibly by computing it lazily or simplifying the structure.

2.  **List Reversal**
    *   **Description**: Frequent use of `Enum.reverse` in `collect_exprs` and argument parsing loops.
    *   **Impact**: Minor performance cost for very long lists.
    *   **Fix**: Acceptable for now, but keep in mind for hot paths.

## Refactoring Opportunities

1.  **Code Duplication between `Pratt` and `Calls`**
    *   **Description**: `Pratt.ex` and `Calls.ex` share significant logic for parsing identifiers, paren calls, and no-parens calls.
        *   `Pratt.parse_rhs_identifier` vs `Calls.parse_identifier`
        *   `Pratt.parse_no_parens_args_with_min_bp` vs `Calls.parse_no_parens_args`
        *   `Pratt.parse_paren_call_base` vs `Calls.parse_paren_call`
    *   **Rationale**: Maintenance burden and risk of inconsistent behavior (as seen with the precedence bug).
    *   **Action**: Consolidate logic into `Calls` module, ensuring it supports `min_bp` constraints, and have `Pratt` delegate to it.

2.  **Monolithic `Pratt` Module**
    *   **Description**: `lib/toxic_parser/pratt.ex` is over 2000 lines long and handles `nud`, `led`, helper logic, and specific token handling.
    *   **Rationale**: Hard to navigate and maintain.
    *   **Action**: Split `Pratt` into smaller modules:
        *   `ToxicParser.Pratt.Nud`
        *   `ToxicParser.Pratt.Led`
        *   `ToxicParser.Pratt.Helpers` (or move helpers to `Builder` or `Grammar` modules).

3.  **Complex EOE Handling**
    *   **Description**: `Expressions.collect_exprs` and `Pratt.led` have complex logic for skipping/counting EOE tokens.
    *   **Rationale**: Hard to understand and verify correctness.
    *   **Action**: Abstract EOE handling into a dedicated helper module or clearer state transitions.
