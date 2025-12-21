# Refactoring proposals (ToxicParser)

This document captures refactoring opportunities found during a code scan, with emphasis on areas that affect **Elixir grammar conformance** and **metadata correctness**.

Scope: proposals only (no behavioral changes implemented here).

## 1) Centralize EOE/newline handling + metadata construction

### Why
EOE skipping, newline counting, and EOE-derived metadata are implemented repeatedly across the parser. This duplication is a correctness risk (especially given known metadata-only conformance failures) and makes it easy for edge cases to drift.

### Evidence (duplication)
- `skip_eoe_count_newlines/2` exists in multiple places:
  - [lib/toxic_parser/pratt.ex](lib/toxic_parser/pratt.ex#L1461)
  - [lib/toxic_parser/grammar/calls.ex](lib/toxic_parser/grammar/calls.ex#L358)
  - [lib/toxic_parser/grammar/dots.ex](lib/toxic_parser/grammar/dots.ex#L254)
  - [lib/toxic_parser/grammar/maps.ex](lib/toxic_parser/grammar/maps.ex#L603)
  - [lib/toxic_parser/grammar/blocks.ex](lib/toxic_parser/grammar/blocks.ex#L66)
  - [lib/toxic_parser/grammar/bitstrings.ex](lib/toxic_parser/grammar/bitstrings.ex#L45)
  - [lib/toxic_parser/grammar/containers.ex](lib/toxic_parser/grammar/containers.ex#L1509)
- `skip_eoe/1` is also repeated with small semantic variations:
  - [lib/toxic_parser/grammar/containers.ex](lib/toxic_parser/grammar/containers.ex#L1275)
  - [lib/toxic_parser/grammar/expressions.ex](lib/toxic_parser/grammar/expressions.ex#L183)
  - [lib/toxic_parser/grammar/keywords.ex](lib/toxic_parser/grammar/keywords.ex#L146)
  - [lib/toxic_parser/grammar/maps.ex](lib/toxic_parser/grammar/maps.ex#L592)
- `build_eoe_meta/1` is duplicated (same shape) in at least:
  - [lib/toxic_parser/grammar/blocks.ex](lib/toxic_parser/grammar/blocks.ex#L244)
  - [lib/toxic_parser/grammar/expressions.ex](lib/toxic_parser/grammar/expressions.ex#L233)
  - [lib/toxic_parser/grammar/containers.ex](lib/toxic_parser/grammar/containers.ex#L1041)

### Proposal
Create one shared module for EOE primitives, e.g. `ToxicParser.Grammar.EOE`, and route all call sites through it.

Suggested API (illustrative):
- `EOE.skip(state)`
- `EOE.skip_count_newlines(state) :: {state, non_neg_integer()}`
- `EOE.build_meta(eoe_token) :: keyword()`

Important: keep room for the one real semantic variation that exists today:
- `containers.ex` has `skip_eoe_not_semicolon/1` (semicolons force stab interpretation). That should remain explicit, but could still live in `EOE`.

### Expected payoff
- Reduced risk of metadata drift.
- Easier to reason about (and test) newline/meta rules centrally.

## 2) Standardize “call/container closing meta” assembly

### Why
Call/container metadata often follows a consistent pattern:
- skip/collect leading EOE (newlines)
- parse args/elements
- skip/collect trailing EOE
- read closing token metadata
- build meta list such as `[newlines: N] ++ [closing: close_meta] ++ open/callee_meta`

This logic is repeated across calls, containers, and maps; subtle differences can cause metadata-only mismatches.

### Evidence
- Paren call “empty call counts leading+trailing newlines; non-empty counts leading only” is embedded in:
  - [lib/toxic_parser/grammar/calls.ex](lib/toxic_parser/grammar/calls.ex#L285)
- Similar “skip EOE, take closing meta, then build meta list” patterns appear throughout containers and map parsing:
  - [lib/toxic_parser/grammar/containers.ex](lib/toxic_parser/grammar/containers.ex#L1200)
  - [lib/toxic_parser/grammar/maps.ex](lib/toxic_parser/grammar/maps.ex#L150)

### Proposal
Extract a small helper that produces `{state, args, close_meta, newlines}` plus a helper that assembles the final `meta` keyword list in the canonical order.

Two options:
- Put it in `ToxicParser.Builder.Helpers` (since it’s meta-building), or
- Create `ToxicParser.Builder.Meta` to separate “AST construction helpers” from “token/meta plumbing”.

## 3) Checkpoint lifecycle: avoid accumulating checkpoints on successful parses

### Why
`TokenAdapter.checkpoint/1` stores a checkpoint in `state.checkpoints`, and the checkpoint entry is only removed on `TokenAdapter.rewind/2`.

Several speculative parses create checkpoints and then *succeed* without rewinding. That means the checkpoint entries are never removed, and the underlying token stream may also keep checkpoint state alive (depending on `Toxic.checkpoint/1` implementation).

Even if memory impact is currently small, this is an easy-to-miss correctness/performance footgun.

### Evidence
All checkpoint call sites are here:
- [lib/toxic_parser/grammar/blocks.ex](lib/toxic_parser/grammar/blocks.ex#L203) (`try_parse_clause/4`): on success, the returned `state` retains the checkpoint entry.
- [lib/toxic_parser/grammar/maps.ex](lib/toxic_parser/grammar/maps.ex#L192) (`try_parse_map_update/3`): on success, no checkpoint cleanup occurs.
- [lib/toxic_parser/grammar/containers.ex](lib/toxic_parser/grammar/containers.ex#L186) (`try_parse_stab_parens_many/5`)
- [lib/toxic_parser/grammar/containers.ex](lib/toxic_parser/grammar/containers.ex#L503) (`try_parse_stab_or_expr/5`)
- [lib/toxic_parser/grammar/containers.ex](lib/toxic_parser/grammar/containers.ex#L991) (`check_and_collect_stab_body/5`)

Checkpoint/rewind behavior is implemented in:
- [lib/toxic_parser/token_adapter.ex](lib/toxic_parser/token_adapter.ex#L67)

### Proposal
Add a “commit/drop checkpoint” API and update speculative parse patterns to always end in either:
- rewind (failure), or
- drop/commit (success)

Suggested API shape:
- `TokenAdapter.drop_checkpoint(state, ref) :: state`
- or a structured wrapper: `TokenAdapter.with_checkpoint(state, fn state -> ... end)`

If `Toxic` supports discarding checkpoints, wire it through; otherwise, at minimum clean `state.checkpoints`.

## 4) Dot-call responsibilities + token normalization clarity

### Why
Dot call parsing is inherently tricky in Elixir (due to operator-vs-call ambiguity and metadata rules). Today there are two compounding complexities:

1) `TokenAdapter` normalizes `:dot_call_op` into `:dot_op`:
   - [lib/toxic_parser/token_adapter.ex](lib/toxic_parser/token_adapter.ex#L245)

2) Dot-related logic exists both inside Pratt and in a dedicated grammar module.

This increases the surface area where dot-call metadata and operator disambiguation can diverge.

### Proposal
Pick one of these architectural directions (simplest first):

A) Keep dot-call parsing primarily in Pratt, and reduce `grammar/dots.ex` to *only* the cases Pratt can’t model cleanly.

B) Move dot-call parsing primarily into `grammar/dots.ex` and have Pratt delegate early, keeping Pratt focused on general operator precedence.

Either way, consider preserving `:dot_call_op` as a distinct `token.kind` (so downstream code doesn’t have to reconstruct intent heuristically). That should reduce branching and make conformance debugging easier.

## 5) Consolidate “token metadata” helpers

### Why
There are multiple “extract line/column” helpers (`token_meta/1`, `token_to_meta/1`, `Builder.Helpers.token_meta/1`) scattered across the codebase.

This is small duplication, but it’s also a common source of subtle differences (e.g., sometimes including extra fields, or different ordering).

### Proposal
- Keep exactly one canonical function for “token -> keyword meta”.
- Prefer routing all consumers through it.

## Suggested rollout order (low-risk to high-risk)

1) Introduce shared helper modules (`EOE`, `Builder.Meta`) without changing behavior.
2) Migrate call sites module-by-module (start with the most duplicated: `containers`, `calls`, `maps`).
3) Add checkpoint drop/commit and update all checkpoint call sites.
4) Revisit dot-call architecture once metadata/newlines are standardized.

## Candidate regression tests to add (when implementing refactors)

- Targeted tests for newline metadata ordering and counting around:
  - empty vs non-empty paren calls
  - map literals and map updates with `|`
  - `do ... end` blocks following calls
  - dot calls with and without parens

These tests should assert metadata as well as AST shape to prevent reintroducing the current class of conformance failures.
