# Combined Issues (ordered by priority)

## P0 — Correctness / Logic Issues (Blockers)

1. **`drop_checkpoint/2` rewinds unexpectedly and/or ignores returned stream** — `TokenAdapter.drop_checkpoint/2` calls `Toxic.rewind_to/2` despite docs saying it won't rewind, and it discards the returned stream. Backtracking semantics and memory use are wrong/unclear.
   - **Fix:** Make drop a no-op on position or explicitly update state.stream; add tests for checkpoint semantics.
   DONE

2. **No-parens call precedence violation** — `Calls.parse_no_parens_args` (and related paths) parse without honoring `min_bp`, so expressions like `a + b c + d` parse as `a + (b (c + d))` instead of `(a + (b c)) + d`.
   - **Fix:** Thread `min_bp` through no-parens parsing or delegate to Pratt with the current binding power.

3. **Error fallback to line 1** — `Error.meta_to_range/3` falls back to `line_only_range(1)` when meta is missing, producing misleading diagnostics.
   - **Fix:** Use `Toxic.position/1` or last known token metadata as fallback.

## P1 — Correctness / Contract Issues

4. **Fuel/peek contract holes** — Fuel is decremented on `next/1` but not on `peek/1`/lookahead fill, allowing unbounded peeking. `peek_n/2` returns empty partials on eof/error despite spec claiming partial tokens.
   - **Fix:** Decide intended fuel semantics; align `peek`/`peek_n` behavior and docs.

5. **Event log mismatch raises instead of reporting** — `EventLog.end_node/3` raises `ArgumentError` on mismatched end nodes, crashing tolerant parsing.
   - **Fix:** Return a structured error/diagnostic event instead.

6. **Synthetic error token shape diverges from lexer errors** — `TokenAdapter.synthetic_error_token/5` sets `value` differently from lexer-produced `:error_token`.
   - **Fix:** Standardize `:error_token.value` payload shape.

7. **Duplicated `build_result/1` clauses** — Two nearly identical clauses differ only by `extra_diagnostics` defaulting to `[]`.
   - **Fix:** Consolidate to one implementation with default to avoid drift.

8. **Option/type mismatches for `:expression_context`** — `parse_string/2` reads `:expression_context` but it's not in `@type option`; defaults differ between `parse_string` (`:unmatched`) and `State` (`:matched`).
   - **Fix:** Add to `@type option` and unify defaults.

9. **Inconsistent error normalization** — `Result.normalize_error/2` returns the original term unchanged when it doesn't match expected patterns, potentially masking bugs.
   - **Fix:** Log/raise on unexpected patterns in debug mode or document behavior.

10. **Scope accumulator stores nested lists** — `Env.exit_scope/from_events` wraps child lists inside parent (`[top | parent]`) instead of concatenating, producing nested lists that require later flattening.
    - **Fix:** Keep scopes as flat stacks per frame.

## P2 — Performance Issues

11. **Lookahead caching is O(n²)** — `maybe_cache/3` appends via `lookahead ++ [token]`; repeated peeks incur quadratic copying.
    - **Fix:** Switch to cons+reverse or `:queue`.

12. **Quadratic keyword/arg accumulation** — No-parens arg parsing and keyword merges use `Enum.reverse(acc) ++ …` and `expr ++ more_kw`, leading to O(n²) in long lists.
    - **Fix:** Accumulate in reverse, single reverse at end (or use difference lists).

13. **Token metadata overhead** — `normalize_token` eagerly builds full metadata (range, delimiter, newlines) for every peek/next.
    - **Fix:** Compute metadata lazily or simplify structure.

14. **Diagnostics overhead per token** — `update_state` uses `Enum.reverse(diagnostics) ++ existing` when diagnostics are typically 0/1.
    - **Fix:** Prepend directly instead.

15. **Eager line index building** — `State.new/2` always builds `line_index`, wasting work on large strict parses with no errors.
    - **Fix:** Build lazily on first diagnostic or gate behind option.

16. **O(n) length checks in guards** — `defguardp is_keyword_list_result(...)` uses `length(arg) > 0` which is O(n).
    - **Fix:** Replace with pattern matching (`[_|_]`) or `list != []`.
    DONE

17. **Inefficient `line_index` construction** — Uses `:binary.matches/2` followed by list comprehension and `List.to_tuple/1`, creating multiple intermediate lists.
    - **Fix:** Use single-pass binary traversal.

18. **Excessive checkpoint creation** — Many parse attempts create checkpoints copying parser state including lookahead and diagnostics.
    - **Fix:** Consider lazy checkpoint creation or memoization.

## P3 — Refactoring / Maintainability

19. **Pratt module is too large (~2k lines)** — Handles operators, calls, dots, access, keywords, metadata, error formatting.
    - **Fix:** Split into `Pratt.Nud`, `Pratt.Led`, `Pratt.Access`, `Pratt.Calls` or adopt parselet tables.

20. **Duplicate logic across modules** — Bracket/access parsing exists in both `Pratt` and `Grammar.Calls`; meta extraction exists as both `Pratt.build_meta/1` and `Builder.Helpers.token_meta/1`; keyword continuation logic duplicated 6+ times.
    - **Fix:** Create single-purpose helpers: `ToxicParser.Meta`, `ToxicParser.Grammar.Args`, consolidated keyword continuation.

21. **Complex EOE handling** — `Expressions.collect_exprs` and `Pratt.led` have complex logic for skipping/counting EOE tokens scattered across modules.
    - **Fix:** Abstract into dedicated helper module with consistent patterns.

22. **Large `led_dispatch` function** — Case statement handling many token types; adding operators requires modifying central function.
    - **Fix:** Use table-driven approach or protocol.

23. **Magic numbers for binding power** — Hard-coded values like `300`, `100` scattered without named constants.
    - **Fix:** Define as module attributes in `Precedence` (e.g., `@dual_op_unary_bp 300`).

24. **State mutation via `Map.put`** — `update_state/6` uses `Map.put`/`Map.update!` instead of struct update syntax, bypassing compile-time key validation.
    - **Fix:** Use `%{state | field: value}` consistently.

25. **Unused `ctx` parameters** — Several functions in `keywords.ex`, `stabs.ex`, `dots.ex` receive `ctx` but never use it.
    - **Fix:** Audit each case; use or remove.

26. **Inconsistent `parse_*_base` naming** — Multiple `*_base` functions parse without calling `led()` at end; naming doesn't convey intent.
    - **Fix:** Standardize on `led?:` option or rename to `parse_nud_only`/`parse_with_led`.

27. **Stabs.ex duplication with Pratt.ex** — Both have similar argument parsing logic.
    - **Fix:** Extract into `ToxicParser.Grammar.Args`.

28. **Deeply nested case trees** — `Expressions.parse_with_layers/3` has deep nesting making control flow hard to follow.
    - **Fix:** Use `with` pipeline or list of attempt functions.

29. **Inconsistent error tuple formats** — Error tuples take multiple forms (3-tuple, 4-tuple, structured reason, atom reason).
    - **Fix:** Standardize on structured `%ParseError{}`.

30. **Event logging infrastructure largely unused** — `EventLog`, `Builder`, `State.event_log` exist but rarely emit events.
    - **Fix:** Either wire consistently through parser or remove/park until needed.

## P4 — Testing / Documentation

31. **Coverage gaps** — "TODO: no coverage" branches for op_identifier + unary-arg no-parens calls, keyword list in `[...]` access, quoted/interpolated keyword keys.
    - **Fix:** Add regression tests for each TODO branch.

32. **Checkpoint/backtracking lacks tests** — Many grammar modules rely on checkpoint/rewind/drop without dedicated unit tests.
    - **Fix:** Add tests asserting drop_checkpoint doesn't change position, rewind restores state, nested checkpoints work.

33. **Shallow error recovery** — `recover_expr_error` consumes single token and checks EOE; may be too aggressive or insufficient.
    - **Fix:** Implement robust synchronization strategy (skip until sync tokens).

34. **Recursion depth risk** — `Pratt.parse_rhs` and `Pratt.nud` are mutually recursive; deeply nested inputs could overflow stack.
    - **Fix:** Verify tail-call optimization or implement iterative approach.

35. **Missing module documentation** — `do_blocks.ex` has `@moduledoc false`.
    - **Fix:** Add documentation explaining do-block attachment.

36. **Missing type specs on internal functions** — Only public API functions have `@spec`.
    - **Fix:** Add specs to key internal functions (token manipulation, AST building, parse transformers).

37. **Outdated phase references** — Comments reference "Phase 3", "Phase 4" etc. from incremental development.
    - **Fix:** Remove or consolidate into development history document.

38. **Dead code / TODO comments** — Several code paths marked as no coverage may be dead code.
    - **Fix:** Run coverage analysis; add tests or remove dead code.
