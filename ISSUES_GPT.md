# ToxicParser review: issues & refactoring opportunities (GPT)

This is a code-review style backlog focusing on **correctness**, **performance**, and **maintainability**. Items are phrased as actionable work with rationale; ordering roughly reflects impact/risk.

## P0 — Correctness / logic issues

1. **`TokenAdapter.drop_checkpoint/2` has unclear (and likely incorrect) stream semantics**
   - **Where**: `lib/toxic_parser/token_adapter.ex` (calls `Toxic.rewind_to(state.stream, ref)` but discards the returned stream).
   - **Why it matters**: If `Toxic.rewind_to/2` is functional (returns a new stream), dropping a checkpoint currently does *nothing* to the underlying stream bookkeeping; if it mutates internal state, it may *silently rewind* (or otherwise alter) the stream while the parser state keeps the old `state.stream`. Either way, backtracking correctness and/or memory behavior can be wrong.
   - **Suggested fix**: Replace with a Toxic API that explicitly “commits/drops” a checkpoint, or capture the returned stream and update `state.stream` (plus add tests to lock the intended behavior).

2. **`peek_n/2` return shape is inconsistent with its own spec/docs**
   - **Where**: `TokenAdapter.peek_n/2` returns `{:eof, [], state}` and `{:error, err, [], state}`.
   - **Why it matters**: Spec says `{:eof, [token()], state}` / `{:error, ..., [token()], state}` (suggesting partial results are returned); current implementation always returns an empty list on EOF/error.
   - **Suggested fix**: Either (a) include the already-peeked tokens in the EOF/error tuples, or (b) change the spec/docs to match reality.

3. **Fuel limiting is enforced on `next/1` but not on `peek/1` (and not clearly on lookahead filling)**
   - **Where**: `TokenAdapter.next/1` consumes fuel, `peek/1` does not.
   - **Why it matters**: A parser can potentially spin using `peek`/`peek_n` without reducing fuel, making `:fuel_limit` unreliable as a runaway-protection mechanism.
   - **Suggested fix**: Decide intended semantics (“fuel is tokens consumed” vs “fuel is parsing steps”) and enforce consistently (e.g., decrement on peek lookahead fill, or rename/clarify option).

4. **Synthetic error token shape diverges from lexer error tokens**
   - **Where**: `TokenAdapter.synthetic_error_token/5` builds `%{kind: :error_token, value: diagnostic.reason, ...}` but `normalize_token/3` sets `value: err` for lexer-produced `:error_token`.
   - **Why it matters**: Downstream code that expects `value` to be `%Toxic.Error{}` (or a consistent payload) can break in tolerant mode.
   - **Suggested fix**: Standardize `:error_token.value` (either always `%Toxic.Error{}` or always a normalized error payload).

5. **Error ranges can fall back to line 1 even when position information exists**
   - **Where**: `Error.meta_to_range/3` falls back to `line_only_range(1)` when meta is missing or unrecognized.
   - **Why it matters**: Diagnostics become misleading, especially for recovery/tolerant scenarios where meta may be absent.
   - **Suggested fix**: Prefer using `Toxic.position/1` (or last known token metadata) as a fallback rather than hardcoding line 1.

6. **Option/type mismatches and inconsistent defaults for `:expression_context`**
   - **Where**: `ToxicParser.parse_string/2` reads `:expression_context` but it is not included in `@type option`; also defaults differ (`parse_string` defaults ctx to `:unmatched` while `State` defaults `expression_context` to `:matched`).
   - **Why it matters**: Confusing public API and subtle behavior drift.
   - **Suggested fix**: Add `{:expression_context, ...}` to `@type option` and unify defaults (and/or make `State.expression_context` actually drive parsing).

## P1 — Performance / scaling risks

7. **Quadratic keyword-list merging via repeated `++` in recursive flows**
   - **Where**: examples include `Pratt.parse_access_indices_after_kw/5` (`kw_expr ++ next_kw`), and call/bracket parsing paths that do `expr ++ more_kw`.
   - **Why it matters**: For long keyword lists (or repeated merges), `++` inside recursion becomes O(n²).
   - **Suggested fix**: Accumulate in reverse and `Enum.reverse/1` once, or use difference-list style accumulation.

8. **Repeated `length(list) > 0` checks (including in guards) are O(n)**
   - **Where**: `defguardp is_keyword_list_result(...) when ... length(arg) > 0`, plus many `when is_list(x) and length(x) > 0` clauses.
   - **Why it matters**: Usually small, but it’s an avoidable cost and adds noise.
   - **Suggested fix**: Replace with pattern matching (`[_|_]`) or `list != []`.

9. **Lookahead caching uses `state.lookahead ++ [token]`**
   - **Where**: `TokenAdapter.maybe_cache/3`.
   - **Why it matters**: Appending to a list is O(n). Today `max_peek` defaults to 4, so it’s bounded, but it’s still a hot path and easy to improve.
   - **Suggested fix**: Use `:queue`, or push to head and track reversed order, or store `{front, back}` lists.

10. **Per-token diagnostic list handling does extra work**
   - **Where**: `TokenAdapter.update_state/6` uses `Enum.reverse(diagnostics) ++ existing`.
   - **Why it matters**: Diagnostics per token are typically 0/1; reversing/concatenating is more work than needed.
   - **Suggested fix**: Prepend directly (or ensure `diagnostics` is already built in correct order).

11. **Eager computation of `line_index/1` may be unnecessary for strict parsing**
   - **Where**: `State.new/2` computes line offsets for every parse.
   - **Why it matters**: For large sources in strict mode (no errors), this is wasted work.
   - **Suggested fix**: Compute lazily on first error (store source; build line index only when needed), or gate behind an option.

## P2 — Refactoring / maintainability

12. **`Pratt` is very large and mixes many responsibilities**
   - **Where**: `lib/toxic_parser/pratt.ex` (operators, calls, dots, access, keywords, metadata, error formatting).
   - **Why it matters**: Hard to reason about correctness and to add features without regressions.
   - **Suggested refactor**: Split into submodules (e.g. `Pratt.Nud`, `Pratt.Led`, `Pratt.Access`, `Pratt.Calls`, `Pratt.Meta`), or adopt parselet tables for operator handling.

13. **Duplicate logic across modules (access parsing, keyword continuation, meta extraction)**
   - **Where**: Bracket/access parsing exists in both `Pratt` and `Grammar.Calls`; meta extraction exists as both `Pratt.build_meta/1` and `Builder.Helpers.token_meta/1`.
   - **Why it matters**: Divergence risk (AST shape/metadata differences) and harder maintenance.
   - **Suggested refactor**: Create single-purpose helpers/modules:
     - `ToxicParser.Meta` for token→AST meta conversion
     - `ToxicParser.KeywordMerge` (or extend `Grammar.Keywords`) for quoted-key continuations/merging
     - One canonical implementation of bracket access parsing

14. **Event logging infrastructure is present but largely unused**
   - **Where**: `EventLog`, `Builder`, and `State.event_log` exist, but grammar code rarely emits events (only `Grammar.parse_string/2` wraps a `:root` node).
   - **Why it matters**: Extra surface area and confusion; `emit_events` looks supported but yields little.
   - **Suggested options**: Either (a) wire `EventLog.token/start_node/end_node` through the parser consistently, or (b) remove/park unused fields (`State.event_log`, `warnings`) until needed.

15. **Deeply nested `case` trees in `Expressions.parse_with_layers/3` make control flow hard to follow**
   - **Where**: `Grammar.Expressions.parse_with_layers/3`.
   - **Why it matters**: Adding a new “layer” (strings/comments/etc.) will worsen readability.
   - **Suggested refactor**: Use a `with` pipeline + tagged returns (`{:no_match, state}`) consistently, or a list of “attempt” functions (`Enum.reduce_while`) to linearize the flow.

16. **Public result construction duplicates nearly identical code paths**
   - **Where**: `ToxicParser.build_result/1` has two clauses differing mainly by `extra_diagnostics`.
   - **Why it matters**: Minor, but easy to simplify and prevents drift.
   - **Suggested refactor**: One function with `extra_diagnostics` defaulting to `[]`.

## Testing / coverage gaps

17. **Multiple “TODO: no coverage” branches correspond to known-corpus failures**
   - **Where**: `Pratt` and grammar modules (`calls`, `containers`, `stabs`).
   - **Why it matters**: These are exactly the fragile disambiguation branches (no-parens vs binary ops, keyword-list parsing inside containers/access) most likely to regress.
   - **Suggested tests**: Add minimal regression tests for each TODO branch, especially:
     - `op_identifier` + unary-arg no-parens calls (Pratt/Cals)
     - keyword list parsing inside `[...]` access indices
     - quoted keyword keys (`"a": 1`) and interpolated keyword keys

18. **Checkpoint/backtracking behavior lacks dedicated tests**
   - **Where**: Many grammar modules rely on `checkpoint/rewind/drop_checkpoint`.
   - **Why it matters**: Even tiny semantic mistakes here create non-local parse bugs.
   - **Suggested tests**: Unit tests asserting:
     - `drop_checkpoint` does not change stream position
     - `rewind` restores lookahead/terminators/diagnostics as expected
     - nested checkpoints behave predictably

---

If you want, I can convert the top P0/P1 items into concrete GitHub issues (with reproduction snippets where possible) and/or implement the safest correctness fixes behind tests first.
