# Analysis of Peek_N Benefits for ToxicParser

## Grammar Rules Benefiting from Peek_N

The Elixir grammar is largely LALR(1), but several constructs introduce conflicts or ambiguities that `peek_n` (lookahead > 1) can resolve or simplify.

### 1. Stab Clauses vs. Expressions (`stab` vs `expr`)
The grammar has a known conflict involving `empty_paren` in `stab` clauses:
```erlang
stab_expr -> empty_paren stab_op_eol_and_expr : ...
stab_expr -> expr : ... (where expr -> ... -> empty_paren)
```
This manifests in `Blocks.ex` where `try_parse_stab_clause` uses backtracking (`checkpoint`/`rewind`) to distinguish between a function clause head and a block expression.

**Benefit:** `peek_n` can eliminate backtracking for common cases.
*   **Case `() ->`**: Peeking 2 tokens (`(`, `)`) and seeing `->` confirms a stab clause.
*   **Case `(arg) ->`**: Peeking 4 tokens (`(`, `identifier`, `)`, `->`) confirms a stab clause.
*   **Case `(expr)`**: If `->` is absent after the closing paren, it is an expression.

### 2. Quoted Keyword Keys
In `CallsPrivate.ex`, `can_be_quoted_keyword?` checks if a token is a string start (`"..."` or `'...'`) to decide if it might be a keyword key (e.g., `"foo": 1`). Currently, it returns `true` for any string, potentially leading to false positives or requiring downstream validation.

**Benefit:** `peek_n(2)` can check if the string token is immediately followed by a colon (`:`).
*   `"foo"` followed by `:` -> Keyword key.
*   `"foo"` followed by anything else -> String literal expression.
This would allow `CallsPrivate` to dispatch correctly without tentative parsing.

### 3. Parenthesized Call Arguments vs. No-Parens Args
The ambiguity between `(a, b)` (2 args) and `(a, b)` (1 arg `a(b)`) is a classic Elixir parser issue. While `call_args_parens` rules handle this via precedence and error productions, `peek_n` could help in `Pratt` to distinguish `(a)` from `(a, b)` early.

**Benefit:**
*   If `peek_n` sees a comma inside the parens (e.g., `(`, `id`, `,`), it strongly suggests `call_args_parens_base` (multiple args).
*   This is less critical than the `stab` case but could simplify `CallsPrivate.parse_paren_args`.

## Implementation Simplifications

### `ToxicParser.Grammar.Blocks`
*   **Current:** Uses `try_parse_stab_clause` with `TokenAdapter.checkpoint` and `rewind`. This is expensive as it parses potentially large expressions only to discard them if `->` is missing.
*   **Proposed:** Use `peek_n` to check for `->` after `)` or `identifier` before committing to `parse_stab_clause`.
    ```elixir
    # Pseudo-code
    case peek_n(state, 2) do
      {:ok, [%{kind: :"("}, %{kind: :")"}, %{kind: :stab_op}], _} -> parse_stab(...)
      _ -> parse_expr(...)
    end
    ```

### `ToxicParser.Grammar.CallsPrivate`
*   **Current:** `can_be_quoted_keyword?` is heuristic.
*   **Proposed:** Implement `is_quoted_keyword?(state)` using `peek_n(2)` to verify the presence of `:`.

### `ToxicParser.Pratt`
*   **Current:** `parse_rhs_identifier` uses `peek(1)` and complex logic to handle `foo (expr)` vs `foo(args)`.
*   **Proposed:** `peek_n` could verify `foo ( ... )` structure more reliably, though the tokenizer's `paren_identifier` distinction already does the heavy lifting here.

## Conclusion
The most significant gain from `peek_n` is in **`Blocks.ex`** to remove backtracking for stab clauses. The current implementation's reliance on `checkpoint`/`rewind` is a performance bottleneck for a common construct (`fn`, `case`, `receive`, `try`). `peek_n` allows predictive parsing for these LALR(k) constructs.

## Analysis of `peek_until` for Variable Length Lookahead

The user asked if a `peek_until` operation (accepting a predicate) in the `Toxic` lexer would be superior to the current `checkpoint`/`rewind` strategy, particularly for variable-length cases like stab clauses (`(arg1, arg2, ...) ->`).

### Comparison: `peek_until` vs. `checkpoint`/`rewind`

1.  **Performance**:
    *   **`checkpoint`/`rewind`**: This approach tentatively *parses* the input. It consumes tokens, allocates memory for AST nodes, and performs reduction logic. If the path is incorrect (e.g., it's not a stab clause), all that work is discarded, and the parser must restart from the checkpoint. This is computationally expensive, especially for long argument lists.
    *   **`peek_until`**: This approach only *lexes* the input. It fills the token buffer but does not perform parsing or AST construction. Scanning tokens is significantly cheaper than parsing expressions.

2.  **Memory**:
    *   Both approaches require retaining tokens in memory. `peek_until` fills the `Toxic` buffer, while `checkpoint` retains the buffer state to allow restoration. The memory footprint for the tokens themselves is comparable.

3.  **Complexity & Correctness**:
    *   A simple `peek_until(predicate)` is insufficient for stab clauses because the target token (`->`) can appear inside nested expressions (e.g., `fn (f = fn -> end) -> ...`).
    *   To be effective, `peek_until` must support **balanced scanning** (tracking parenthesis/bracket/brace depth).
    *   If `Toxic` provided a `peek_until` that supports a reducer or stateful predicate (to track nesting), it would allow the parser to look ahead arbitrarily far to find a top-level `->` without the overhead of parsing.

### Recommendation
Implementing `peek_until` in `Toxic` (or `TokenAdapter`) would be **superior** to `checkpoint`/`rewind` for variable-length lookahead scenarios like stab clauses, provided it supports nesting awareness. It converts a backtracking parser (try A, fail, try B) into a predictive one (scan to decide A or B), avoiding the cost of constructing and discarding invalid ASTs.
