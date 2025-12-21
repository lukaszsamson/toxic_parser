# Map Parsing Refactor Design (Avoid Reparse)

## Goal

Refactor `lib/toxic_parser/grammar/maps.ex` to:

- Avoid reparsing (especially “parse-as-update then rewind and parse-as-entries”).
- Track `elixir_parser.yrl`’s LALR(1) intent closely (same nonterminal shapes / same decision points).
- Fit ToxicParser’s hybrid design (recursive descent for syntax boundaries + Pratt for expressions).

Non-goals:

- Change AST shapes vs `Code.string_to_quoted/2` compatibility.
- Rework operator precedence globally.
- Perfect Yecc error message parity (but the refactor should make diagnostics easier to localize).

## Canonical reference (Elixir Yecc)

Relevant slice from `elixir_parser.yrl` (Map and structs):

- `assoc_expr -> matched_expr assoc_op_eol matched_expr | ... | map_base_expr`
- `assoc_update -> matched_expr pipe_op_eol assoc_expr | ...`
- `assoc_update_kw -> matched_expr pipe_op_eol kw_data | ...`
- `map_close -> kw_data close_curly | assoc close_curly | assoc_base ',' kw_data close_curly`
- `map_args` alternatives:
  - `{ }`
  - `{ map_close }`
  - `{ assoc_update }`
  - `{ assoc_update , }`
  - `{ assoc_update , map_close }`
  - `{ assoc_update_kw }`

Key intent:

- Inside `%{...}`, `|` is a *delimiter* in `assoc_update*`, not a normal `|` operator.
- Inside `%{...}`, `=>` is a *delimiter* in `assoc_expr`, not a normal operator.
- Keyword data (`kw_data`) is only valid at the end (`map_close`), and is spliced into entries.

## Current ToxicParser implementation (summary)

`parse_map_args_body/7` currently does:

1. If `}`: empty map.
2. Else: `try_parse_map_update/3`:
   - checkpoint
   - `Pratt.parse/3` (pipe allowed) to support cases like `c!s|n` where `|` must be inside an argument/expression
   - classify “embedded update” when Pratt returns a `{:|, ...}` AST
   - otherwise look for a `|` token after the parsed expression
   - on some failures, retry with `Pratt.parse_with_min_bp(..., @pipe_op_bp + 1)`
   - if not an update, rewind to checkpoint
3. If update: build update AST.
4. If not update: parse `map_close` by parsing entries from the beginning again.

This means for the common case `%{a => b, c => d}`, the first entry is parsed twice:

- Once by `try_parse_map_update` (discarded after rewind)
- Again by `parse_map_close`

Additionally, `parse_assoc_expr/3` parses a full expression and then post-processes the AST to “extract the rightmost `=>`” (`extract_assoc/1`), which is correct but diverges from Yecc’s delimiter-style `assoc_expr`.

## Root problem: reparse as control flow

Yecc never “rewinds”; it shifts/reduces deterministically based on lookahead and grammar state.

ToxicParser currently uses a checkpoint as a substitute for a Yecc decision point:

- “Maybe this is `assoc_update` (map update)”
- “If not, rewind and treat it as `map_close`”

The refactor replaces that with a *single-pass prefix parse* that produces enough information to continue without rewinding.

## Proposed refactor (two-stage)

### Stage 1 (minimal, no Pratt changes): single-pass prefix + continuation

Keep the existing Pratt-based “update detection” logic (including `check_embedded_map_update/1` and the `@pipe_op_bp` fallback), but remove the checkpoint/rewind by:

1. Parsing the first map “thing” once.
2. Reusing that result as either:
   - the update prefix (`assoc_update*`), or
   - the first `assoc_expr` in `assoc_base` for `map_close`.

Concretely:

- Replace `try_parse_map_update/3` with `parse_map_prefix/3` that returns one of:
  - `{:update, base_ast, pipe_meta, rhs_entries, state, log}`
  - `{:entry, first_entry_ast, state, log}`
  - `{:kw_only, kw_list, state, log}` (only accepted if followed by `}`)
  - `{:error, ...}`

- Introduce `parse_map_close_after_first/5`:
  - consumes `(',' assoc_expr)*` and optional `',' kw_data` tail
  - expects `}` and returns `{entries, close_meta, state, log}`
  - starts with an already-parsed `first_entry_ast` (no reparse)

- Keep `parse_map_close/3` as a thin wrapper around `parse_map_prefix/3` + `parse_map_close_after_first/5`.

Benefits:

- Eliminates the dominant reparse (checkpoint/rewind).
- Keeps semantics stable (no precedence changes).
- Confines the refactor mostly to `Maps` (lowest risk).

Tradeoffs:

- Still relies on AST post-processing (`extract_assoc/1`, embedded update classification).
- Still carries the “retry with min_bp” path for tricky `|` cases.

### Stage 2 (align to Yecc): delimiter-aware `assoc_expr` and `assoc_update`

After Stage 1 is stable, move closer to `elixir_parser.yrl` by making `=>` and `|` true delimiters during map parsing (instead of “parse whole expr then extract/special-case”).

#### 2.1 `assoc_expr` as a delimiter production

Implement `assoc_expr` the way Yecc expresses it:

1. Parse the key as `matched_expr|unmatched_expr` but *stop before* `=>`:
   - `Pratt.parse_with_min_bp(state, Context.unmatched_expr(), log, assoc_bp + 1)`
2. If next token is `assoc_op` (`=>`), consume it (including EOE for `assoc_op_eol`), parse the value expression, and return `{with_assoc_meta(key, assoc_tok), value}`.
3. If there is no `=>`, return the key AST as `map_base_expr`.

This replaces `extract_assoc/1` entirely (or relegates it to error recovery only).

#### 2.2 `assoc_update` as a delimiter production

Implement `assoc_update` similarly:

1. Parse the update base expression but *stop before* `|` (pipe delimiter):
   - `Pratt.parse_with_min_bp(state, Context.container_expr(), log, pipe_bp + 1)`
2. If next token is `pipe_op`, consume it (including EOE for `pipe_op_eol`).
3. Parse RHS as either:
   - `kw_data` (`assoc_update_kw`), or
   - `assoc_expr` (`assoc_update`)
4. Continue with the `map_args` alternatives:
   - `}` / `, }` / `, map_close`

This eliminates the need for “embedded update” classification because the pipe delimiter is never consumed into the AST in the first place.

#### 2.3 Critical subtlety: `c!s|n` and other “pipe belongs inside expression”

The only reason the current code parses the full expression first is to allow constructs that legitimately consume `|` as part of an argument/expression, e.g.:

- `%{c!s|n => 1}` should treat `|` inside the *key expression* (not as update delimiter).

Stage 2 must preserve this behavior. The clean design is:

- Add a Pratt entry point that stops on `|` as a delimiter *only when the `|` is at the current Pratt led-level*, not inside:
  - parenthesized subexpressions
  - container literals
  - call arguments (paren or no-parens)

In practice this likely means:

- `Pratt.parse_with_min_bp/4` continues to be used, but call/arg parsing must parse its children with their own min_bp (typically `0`) so a `|` can be consumed inside the argument even when the outer “stop-at-pipe” bp is active.
- If current Pratt already does that (common in Pratt implementations), Stage 2 works without further changes.
- If current Pratt propagates the outer `min_bp` into argument parsing, introduce a dedicated function such as:
  - `Pratt.parse_container_expr_for_map_update/3`
  - or `Pratt.parse_with_delimiter/5` (delimiter kind + bp)

This gives map parsing a grammar-accurate delimiter without breaking `c!`-style calls.

## Implementation sketch (grammar-shaped)

Target internal structure (mirrors `elixir_parser.yrl` names):

- `parse_map_args_body/…` implements `map_args` decision tree (including update + close variants).
- `parse_map_close/…` implements `map_close` (assoc list and/or kw tail).
- `parse_assoc_expr/…` implements `assoc_expr` (delimiter `=>` in Stage 2).
- `parse_assoc_update/…` + `parse_assoc_update_kw/…` implement `assoc_update*` (delimiter `|` in Stage 2).

Stage 1 keeps the existing “update detection” but changes control flow to:

- parse prefix once
- branch without rewinding
- continue via `*_after_first` helper(s)

## Edge cases to preserve (tests to add/confirm)

- Empty map: `%{}`.
- Keyword-only map: `%{a: 1, b: 2}` (kw spliced).
- Assoc-only map: `%{a => 1, b => 2}`.
- Assoc + kw tail: `%{a => 1, b: 2}` (kw tail must be last).
- Update + close: `%{user | foo: 1}` and `%{user | foo => 1}`.
- Update + comma + more: `%{user | foo: 1, bar: 2}`, `%{user | foo => 1, bar: 2}`.
- Parenthesized pipe is not update: `%{(user | f())}` should remain a normal pipe expression entry.
- Pipe consumed by argument/call: `%{c!s|n => 1}` should not become an update.
- Quoted keyword keys: `%{"foo": 1}`, `%{'bar': 2}` (via `Keywords.try_parse_kw_data/4`).

## Migration plan

1. Stage 1: remove checkpoint/rewind by introducing “prefix + continuation” helpers; run `mix test`.
2. Add targeted conformance/regression tests for the edge cases above (especially `c!s|n` and parenthesized pipes).
3. Stage 2: refactor `assoc_expr` to delimiter-aware parsing; remove `extract_assoc/1` use from map entry parsing.
4. Stage 2: refactor `assoc_update*` to delimiter-aware parsing; remove embedded-update classification if no longer needed.
5. Optional: move shared “map-close style” parsing onto `Delimited.parse_comma_separated` patterns used by other containers (already present), keeping only map-specific constraints (kw tail must be last, update forms).

## Definition of done

- `%{...}` does not parse the first element twice in the non-update case (verified by instrumentation or log counters during tests).
- Behavior matches `elixir_parser.yrl` for the listed edge cases.
- Map parsing code reads like the Yecc productions: clear `map_args` / `map_close` / `assoc_update*` / `assoc_expr` phases without backtracking.

