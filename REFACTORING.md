# Refactoring priorities (consolidated)

## 1) Centralize EOE/newline + metadata helpers - Done
- **Action:** Create `ToxicParser.Grammar.EOE` (or similar) with `skip/1`, `skip_count_newlines/2`, `build_eoe_meta/1`, `annotate_eoe/2`, and reuse `Builder.Helpers.token_meta/1`.
- **Why:** Removes duplicated skip/build logic across Pratt, Containers, Calls, Maps, Blocks, Strings, Bitstrings; reduces metadata drift and fixes newline/count inconsistencies.

## 2) Unify no-parens argument detection - Done
- **Action:** Define one canonical `can_start_no_parens_arg?/1` (or token kind list) and reuse in Pratt and Calls.
- **Why:** Eliminates divergent token lists (e.g., `:ellipsis_op`), preventing mismatched call parsing behavior.

## 3) Decompose `Pratt.led/6` - Done
- **Action:** Split into focused helpers (`led_call`, `led_dot`, `led_bracket`, `led_binary`, etc.) while preserving behavior.
- **Why:** 350+ lines of mixed responsibilities hinder readability and make fixes risky; smaller units ease testing and future changes.

## 4) Standardize call/container closing meta assembly - Done
- **Action:** Extract helper for the common pattern: skip leading/trailing EOE, gather closing token meta, assemble meta/newlines in a consistent order (possibly in `Builder.Meta`).
- **Why:** Call/container/stub/bitstring/map parsing currently hand-rolls this, causing metadata-only conformance failures and duplication.

## 5) Cleanup checkpoint lifecycle - Done
- **Action:** Add `TokenAdapter.drop_checkpoint/2` (or `with_checkpoint/2`) and ensure speculative parses either rewind or drop on success.
- **Why:** Checkpoints accumulate today on successful speculative parses (blocks, maps, containers), risking latent memory/state leaks.

## 6) Normalize binding-power constants - Done
- **Action:** Expose named precedence constants (`when_op_bp`, `pipe_op_bp`, etc.) and replace magic numbers (e.g., 51, 71) in parse-with-min-bp calls.
- **Why:** Clarifies intent, prevents silent precedence drift, and makes future table changes safer.

## 7) Resolve dot-call normalization - Done
- **Action:** Pick a single strategy: keep `:dot_call_op` distinct or fully normalize to `:dot_op`, then align Pratt vs `grammar/dots.ex` responsibilities accordingly.
- **Why:** Current mixed model leaves dead branches and duplicated logic, increasing risk of dot-call metadata bugs.

## 8) Extract stab parsing from `containers.ex` - Done
- **Action:** Move stab-related functions to `Grammar.Stabs`, keeping containers focused on list/tuple/paren handling.
- **Why:** Containers is ~1700 lines; isolating stab logic (~700 lines) improves navigability and reduces coupling.

## 9) Consolidate map-update heuristic - Done
- **Action:** Refactor `Maps.try_parse_map_update/3` to avoid deep backtracking (peek pipe first or parse with appropriate min_bp) and extract guard/meta helpers.
- **Why:** Current checkpoint-heavy approach is complex and slow; clearer predicates reduce risk of mis-parsing `%{a | b}` vs `%{a => b}`.

## 10) Standardize error tuple arity (optional, scoped) - Done
- **Action:** Prefer 4-tuple `{:error, diag, state, log}` or introduce a `Result` helper to normalize 3- vs 4-tuple paths.
- **Why:** Simplifies pattern matching across Pratt and grammar modules; reduces unreachable branches and error-shape churn.

---

## Additional refactoring opportunities (consolidated from other reviews)

### 11) Consolidate do-block attachment logic - Done
- **Action:** Unify `maybe_do_block/4`, `maybe_do_block_no_led/4`, and `maybe_do_block_with_min_bp/6` into a single function with explicit context/options handling.
- **Why:** These functions duplicate behavior and differ subtly on metadata and context gating, risking regressions.

### 12) Unify nud logic
- **Action:** Refactor `Pratt.nud_base` and `Pratt.nud_with_min_bp` to use a single `nud` function with options/flags to control restricted behavior (no do-blocks, no containers).
- **Why:** Reduces duplication; `nud_base` appears to be a restricted version of `nud_with_min_bp`.

### 13) Decouple strings from keywords
- **Action:** Have `Strings.parse` only return string ASTs; move the `:keyword_key`/`:keyword_key_interpolated` conversion logic to the caller (`Keywords.parse_kw_pair`).
- **Why:** Currently string parsing leaks keyword logic, creating unnecessary coupling.

### 14) Clean up `ToxicParser.ex` entry point
- **Action:** Merge duplicated `build_result` clauses with default values; replace hardcoded `{1, 1}` error position in `parser_error` with actual position from `reason`/`state`.
- **Why:** Reduces duplication and improves error reporting accuracy.

### 15) Rename or restructure `calls_private.ex`
- **Action:** Rename to `CallsInternal`/`CallsShared`, or move `parse_paren_args` to `Calls` (exported), or create a general `Args`/`Parameters` module.
- **Why:** "Private" naming is misleading—it's actually public for internal use.

### 16) Simplify dot member parsing
- **Action:** Separate token classification from call/bracket parsing in `Dots.parse_member/3` to reduce branching and heterogeneous return tuples (`{atom, meta}`, `{atom, meta, :no_parens_call}`, AST).
- **Why:** Intertwined logic is hard to follow and maintain.

### 17) Document and enforce consistent EOE skip timing
- **Action:** Standardize whether functions skip EOE at the start or end; document the convention.
- **Why:** Inconsistency can lead to double-skipping or missed EOE tokens.

### 18) Remove unreachable error pattern in `Pratt.parse_with_min_bp`
- **Action:** Remove or reorder the 4-tuple `{:error, diag, state, log}` pattern that is shadowed by the 3-tuple pattern.
- **Why:** Dead code; the 3-tuple matches first.

### 19) Create shared helpers for common AST patterns
- **Action:** Extend `Builder.Helpers` with: `access_expr/3`, `do_end_meta/2`, `maybe_newlines_meta/1`.
- **Why:** Building access expressions, do-block metadata, and newlines metadata is repeated in 5-10 places.

### 20) Fix O(n) list appending in containers
- **Action:** Replace `acc ++ [expr]` patterns with `[expr | acc]` followed by `Enum.reverse`.
- **Why:** `++` is O(n); prepend + reverse is O(1) per element.

### 21) Reassess precedence table for dead/misleading entries
- **Action:** Audit `Precedence` for unused entries (`unary_not_op`, high `dot_op`, `ellipsis_op` bp); remove or align with actual Pratt usage.
- **Why:** Prevents future misbinding bugs and confusion.

### 22) Standardize context parameter naming
- **Action:** Use `ctx` consistently (not `context`) and remove unused `_ctx` parameters.
- **Why:** Improves readability and consistency.

### 23) Standardize checkpoint/rewind variable pattern
- **Action:** Adopt `{ref, checkpoint_state} = TokenAdapter.checkpoint(state)` consistently.
- **Why:** Clearer than reusing the same binding name.

### 24) Update outdated moduledocs
- **Action:** Replace "Phase X scaffolding" comments in `calls.ex`, `containers.ex` etc. with current functionality descriptions.
- **Why:** Documentation should reflect reality.
