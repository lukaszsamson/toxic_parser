# Lexer integration redesign (Toxic ↔ ToxicParser)

This document proposes a new Toxic ↔ ToxicParser integration that removes `ToxicParser.TokenAdapter` entirely and makes ToxicParser operate directly on Toxic’s token stream, while also addressing the major lexer-side performance pain points identified in:

- `profiling/ANALYSIS.md`
- `profiling/ANALYSIS_GPT.md`
- `profiling/POST_STAB_ANALYSIS.md`
- `profiling/OPTIMIZATION_SUMMARY.md`
- `/Users/lukaszsamson/claude_fun/toxic/OPTIMIZATION.md`
- `/Users/lukaszsamson/claude_fun/toxic/PROFILE_RECOMMENDATIONS.md`

The key idea is to **move “adapter responsibilities” into a single, faster lexer cursor** (owned by Toxic), and to make ToxicParser consume **raw Toxic tokens** (tuples) instead of allocating per-token wrapper maps and metadata.

---

## Goals

- Remove `lib/toxic_parser/token_adapter.ex` (no double-buffering, no per-token normalization maps).
- Keep the parsing API error-tolerant and checkpoint-friendly.
- Reduce per-token overhead:
  - avoid per-token `current_terminators/1` snapshots
  - avoid per-token “metadata map” allocation
  - avoid per-token `Position.to_location/3` conversions in hot paths
  - avoid queue churn for “simple next/1 streaming”
- Preserve incremental lexing support (`slice/…`).
- Allow a migration path for other Toxic consumers (don’t break everything at once).

Non-goals:
- Redesign Toxic’s tokenization rules here (only interface/representation and buffering/checkpoint mechanics).
- Change ToxicParser’s grammar semantics.

---

## Current pain points (what profiling is telling us)

### 1) Double buffering and duplicated responsibilities

Today, tokens flow through two layers of buffering/state:

- Toxic’s stream buffering: `buffer: :queue`, `push: []`, `max_batch` refill, plus checkpoint state stored in the process dictionary.
- ToxicParser’s `TokenAdapter` buffering: `State.lookahead` plus separate checkpoint bookkeeping in `State.checkpoints`.

Even when parsing is mostly sequential, we pay overhead for:
- queue in/out per token (Toxic)
- lookahead list operations and `++` (TokenAdapter’s `maybe_cache/3`)
- repeated state updates and metadata building

### 2) Per-token normalization is expensive and mostly avoidable

`TokenAdapter` creates a new map for every token:

```elixir
%{kind: ..., value: ..., metadata: %{range: ..., delimiter: ..., ...}, raw: ...}
```

This allocates and computes:
- `range.start` and `range.end` via `Position.to_location/3` (twice per token)
- delimiter + delimiter role
- newline counts
- terminator snapshots (stored on every token’s metadata)

But the parser largely only needs:
- `kind`
- the token value (when present)
- raw meta range (start/end)
- newline counts (for EOE skipping)

Terminator stacks are *mostly for error reporting / editor features*, not for parsing decisions in ToxicParser itself.

### 3) Terminator snapshots are “always on” in Toxic batching

In `Toxic.fetch_tokens_from_driver/…`, each token is stored as `{token, pre_terms, pre_pos}`. Capturing `pre_terms` per token is costly, and `pre_pos` exists primarily to compensate for “batch drift” (driver has advanced past the head token).

This is a consequence of batching + queue buffering.

### 4) Checkpoint mechanics copy too much state (and use the process dictionary)

Toxic checkpoints currently store `{push, buffer, driver, ...}` in the process dictionary.
TokenAdapter then stores `{lookahead, diagnostics, terminators}` in its own map keyed by the checkpoint ref.

Even though checkpoint overhead is now “reasonable” in the latest profiles, the architecture still forces needless copying and bookkeeping, and it prevents a clean “parser consumes lexer cursor directly” model.

---

## Proposed redesign: Toxic provides a Parser Cursor (no adapter)

### Summary

Introduce a new cursor type in Toxic that is explicitly designed for parsers:

- **No queue by default**: direct `Driver.next/2` for sequential `next/1`.
- **Lookahead buffering only when needed**: `peek/peek_n` fills a small list buffer.
- **Checkpointing is cursor-native** (stored on the cursor, not in the process dictionary).
- **Tolerant mode never returns `{:error, …}`** from `next/1`; it returns an `:error_token` token instead.
- **Optional normalization profile** so the lexer can emit parser-friendly token kinds (e.g. `:eoe`).

This replaces:
- ToxicParser’s `TokenAdapter` module
- the parser-owned lookahead buffer
- the parser-owned checkpoint bookkeeping

ToxicParser becomes “a consumer of a cursor”, not “a wrapper around Toxic”.

---

## New public API (proposal)

### 1) Cursor construction

```elixir
cursor = Toxic.cursor(source, opts)
```

`opts` includes existing tokenizer options plus parser-facing ones:

- `:profile`
  - `:generic` (default): emit today’s token kinds (compat)
  - `:toxic_parser`: emit a parser-optimized surface (see “Token surface”)
- `:capture_terminators`
  - `false` (default for parser throughput)
  - `true` (editor tooling / diagnostics)
- `:checkpoint_storage`
  - `:stack` (default; LIFO, fastest)
  - `:map` (supports arbitrary drop/rewind by id)

Toxic keeps `Toxic.new/…` as the “legacy stream” API for now, but ToxicParser should use `Toxic.cursor/2`.

### 2) Token consumption

```elixir
{:ok, token, cursor} = Toxic.next(cursor)
{:eof, cursor} = Toxic.next(cursor)
```

No `{:error, …}` in tolerant mode:
- `:error_mode == :tolerant` ⇒ `next/1` emits `{:error_token, meta, %Toxic.Error{...}}` and continues.
- `:error_mode == :strict` ⇒ `next/1` may return `{:error, error, cursor}` (for non-parser consumers).

ToxicParser runs Toxic in `:tolerant` mode and therefore never needs to synthesize error tokens itself.

### 3) Lookahead

```elixir
{:ok, token, cursor} = Toxic.peek(cursor)
{:ok, tokens, cursor} = Toxic.peek_n(cursor, n)
```

Implementation detail: `peek_n/2` reads tokens from the driver into `cursor.lookahead` (a list) as needed; it does not convert queues to lists.

### 4) Pushback (optional)

ToxicParser almost never needs pushback if checkpoint/rewind is cheap, but keeping it simplifies migration:

```elixir
cursor = Toxic.pushback(cursor, token)
cursor = Toxic.pushback_many(cursor, [token1, token2])
```

### 5) Checkpoint / rewind

```elixir
{mark, cursor} = Toxic.checkpoint(cursor)
cursor = Toxic.rewind(cursor, mark)
cursor = Toxic.drop_checkpoint(cursor, mark)
```

The cursor stores checkpoint state internally (no process dictionary).

Recommended contract for parser usage:
- checkpoints are used in a nested/LIFO fashion most of the time (good fit for `:stack`)
- but `:map` mode can exist if needed for non-LIFO call sites

### 6) Position and source

```elixir
{{line, col}, cursor} = Toxic.position(cursor)
source_binary = Toxic.source_binary(cursor)
```

For ToxicParser, `source_binary` is needed for line-index generation and error messages.

---

## Token surface (what ToxicParser consumes)

### Keep tuples, avoid wrapper maps

Toxic tokens are already tuples like:

- `{kind, meta}`
- `{kind, meta, value}`
- `{kind, meta, v1, v2}`

ToxicParser should consume these directly, using small helper functions:

```elixir
kind = Toxic.Token.kind(token)
meta = Toxic.Token.meta(token)
value = Toxic.Token.value(token) # nil if none
newlines = Toxic.Token.newlines(token) # 0 if none
```

These helpers should be simple pattern matches (inlined by the compiler).

### Normalization profile: `profile: :toxic_parser`

To eliminate adapter-only “kind rewriting”, Toxic should optionally emit a slightly different surface that matches ToxicParser’s existing expectations:

- `{:eoe, meta, %{source: :newline | :semicolon, newlines: n}}`
  - emitted instead of `{:eol, meta}` and `{:";", meta}`
  - keeps EOE logic out of the parser and makes EOE skipping uniform
- `{:dot_op, meta}` (or `{:dot_op, meta, :.}`) instead of `{:. , meta}`
  - avoids parser-side “dot-kind mapping”
- `{:error_token, meta, %Toxic.Error{...}}` always emitted in tolerant mode

This is intentionally narrow: normalize only the tokens that currently force per-token adapter logic.

If other consumers rely on the old kinds, they keep using `profile: :generic` (or the legacy stream).

---

## Cursor internals (how this hits the perf recommendations)

### 1) “No-buffer fast path” becomes the default

The fastest `next/1` path should be:

1. if `push != []` ⇒ pop and return
2. else if `lookahead != []` ⇒ pop and return
3. else ⇒ call `Toxic.Driver.next(source_tail, driver)` and return token directly

This completely bypasses:
- batching
- queue in/out
- per-token terminator snapshots

Lookahead is only paid for when actually used (`peek/peek_n`) or when buffering for checkpoints is required.

### 2) Bounded “carry window” inside Toxic.Driver (separate but aligned)

From `/Users/lukaszsamson/claude_fun/toxic/OPTIMIZATION.md` / `PROFILE_RECOMMENDATIONS.md`:
- remove per-token `++` patterns in driver/deferrals by keeping a fixed-size recent-context window

This should be implemented in Toxic’s driver/tokenizer internals, independent of the cursor design, but the cursor makes it easier to realize the benefit because it reduces other overhead around token emission.

### 3) Terminators become pay-for-play

For ToxicParser, `capture_terminators: false` should be the default.

When enabled:
- store terminator stack *only for the head position* (or store a small “version id”)
- avoid capturing a full terminator snapshot per token unless a consumer explicitly asks for it

If a consumer wants `current_terminators/1`, compute it from the driver state at the cursor’s current position (which is accurate in the no-batch model).

### 4) Checkpoints store “minimal rewind state”

In the no-batch, list-lookahead model, a checkpoint state can be:

```elixir
%{
  source_tail: source_tail,
  driver: driver,
  lookahead: lookahead,
  push: push,
  eof: eof,
  error: error
}
```

No need for:
- `last_emitted_entry` (that was a queue-era artifact)
- per-token `pre_pos` snapshots

---

## ToxicParser changes (high-level)

### 1) Remove TokenAdapter and consume Toxic tokens directly

Replace calls like:
- `TokenAdapter.peek(state)` / `TokenAdapter.next(state)` / `TokenAdapter.peek_n(state, n)`
- token maps `%{kind: ..., value: ..., metadata: ...}`

with:
- `Toxic.peek(state.lexer)` / `Toxic.next(state.lexer)` / `Toxic.peek_n(state.lexer, n)`
- raw Toxic token tuples

### 2) Stop building per-token metadata maps

`TokenAdapter.metadata/4` currently constructs a per-token metadata map including `range` maps.

Instead:
- carry Toxic `meta` through parsing and AST building
- compute `range` (locations) only at AST construction boundaries (and only when needed)

Practical approach:
- update `ToxicParser.Builder.Meta` and `Builder.Helpers` to accept Toxic meta tuples directly
- keep `line_index` in `State` for converting meta positions to `Position.location()` on demand

This preserves conformance while eliminating millions of small allocations.

### 3) Error handling becomes lexer-native

Because tolerant mode emits `:error_token` tokens, ToxicParser no longer needs:
- to convert `{:error, …}` from `Toxic.next/1` into a synthetic token
- to call `Toxic.position/1` just to place an error token

Parser diagnostics collection becomes:
- “collect error tokens encountered”
- plus “collect Toxic warnings at end” (see below)

### 4) Terminators move out of the hot path

ToxicParser currently refreshes terminators on every token via `Toxic.current_terminators/1` in `fetch_next/2`.

With the new cursor:
- parsing does not request terminators per token
- terminators are only pulled when needed for user-facing error messages or IDE features (optional)

### 5) Warnings and diagnostics interface

Toxic already has “warnings accumulation”.

Proposal:
- Toxic cursor exposes `Toxic.warnings(cursor) :: {[Toxic.Warning.t()], cursor}` (drain or snapshot)
- ToxicParser merges these into its own warning format in one place (end of parse), rather than per token

---

## Migration plan (minimize risk)

### Phase 1: Add cursor API to Toxic (no parser changes yet)

- Implement `Toxic.cursor/2` alongside the existing `Toxic.new/…` stream.
- Implement `next/peek/peek_n/checkpoint/rewind/drop_checkpoint` on the cursor.
- Implement `profile: :toxic_parser` token normalization (EOE + dot + error_token).
- Keep the legacy queue/batch stream behavior untouched.

### Phase 2: Update ToxicParser to use cursor + raw tokens

- Replace `TokenAdapter` usage with direct cursor usage.
- Teach builder/meta helpers to accept Toxic metas.
- Keep `TokenAdapter` around temporarily behind a feature flag to bisect perf regressions.

### Phase 3: Delete TokenAdapter + simplify parser state

- Remove `State.lookahead`, `State.checkpoints`, `State.terminators` (or make them optional).
- Remove `lib/toxic_parser/token_adapter.ex` and its tests; add new “lexer integration” tests that validate cursor behaviors used by the parser (peek/rewind + tolerant errors + EOE).

### Phase 4: Optimize Toxic internals (aligned with Toxic’s own docs)

- Bounded carry window, remove `++` hotspots in driver/deferrals.
- Ensure `max_batch` default remains small (or irrelevant under the cursor model).
- Pay-for-play terminators.

---

## Expected performance impact

Based on the current profiles:

- Remove per-token `TokenAdapter.metadata/4` cost (and most `Position.to_location/3` calls driven by it).
- Remove per-token terminator snapshot calls from the parser’s token fetch path.
- Remove `TokenAdapter.maybe_cache/3` `++` behavior.
- Remove duplicated checkpoint bookkeeping across Toxic + TokenAdapter.
- Reduce overall allocation pressure by avoiding per-token wrapper maps.

The exact speedup depends on corpus and parser behavior (peek frequency), but this targets the existing “lexer/token boundary” ceiling that remains after the stab/checkpoint optimizations.

---

## Open questions / tradeoffs

- **Token normalization compatibility**: do we keep `profile: :toxic_parser` as an opt-in, or switch the default token kinds? The safest is opt-in.
- **Checkpoint semantics**: can ToxicParser guarantee LIFO checkpoint usage everywhere? If yes, `:stack` mode becomes very fast; otherwise keep a `:map` fallback.
- **Metadata conformance**: builder changes must preserve Elixir’s `Code.string_to_quoted_with_comments/2` location expectations. The plan assumes we compute locations from Toxic metas + a line index at AST build time, not per token.
- **Comments**: if ToxicParser wants comment preservation/conformance, the cursor should expose comment tokens or an out-of-band comment channel; this design doesn’t force a choice.

