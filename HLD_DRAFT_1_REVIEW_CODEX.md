# HLD Draft 1 Review (Codex)

## What works
- Clear split between token adapter, grammar-shaped recursive descent, and Pratt expressions; aligns with Prism/Ruff style hand-written parsers.
- Toxic-first posture (error tokens, synthesized closers, terminator stack) is the right foundation for resilience and IDE use.
- Explicit separation of expression families (`matched` / `unmatched` / `no_parens`) and low-binding `do` handling matches `elixir_parser.yrl` intent.
- Event stream + builder layering is industry-standard for lossless trees and incremental-friendly outputs.
- Per-rule sync sets and missing-node synthesis are the correct recovery primitives for IDE stability.

## Gaps / corrections
- Guard/pattern gating: the upstream parser does **not** enforce guard-only constructs; guard restrictions live in later phases (`elixir_expand`). Keep parser-only contexts minimal (pattern vs value for pin/map-update validation), or gate guard checks behind an optional validator to avoid false negatives in tolerant mode.
- Lookahead: `peek_n` should be scoped to the few ambiguous sites (`no_parens_one_ambig`, `do` attachment, quoted identifier follow-ups). The draft reads like general-purpose multi-token lookahead; codify the exact sites and keep the common path 1-token predictive to avoid speculative rewinds.
- Grammar coverage: add explicit plans for clause-based constructs (`case/cond/receive/try/catch/rescue/after`, `with`, comprehensions/generator qualifiers) and stab handling inside parens (see `build_paren_stab/3` in `elixir_parser.yrl`).
- Keywords/do: spell out the `kw` rules from `yrl` (`maybe_bad_keyword_*` warnings, “keyword list must be last” in calls/containers, `do:` vs `do/end` attachment) and how diagnostics are emitted when those are violated.
- Operators and rewrites: document the `not expr in right` rewrite/warning, `..//` step operator constraint, and space-sensitive `dual_op`/`capture_int` handling so Pratt parselets build the same AST as `build_op/3`.
- EOE semantics: `eoe` normalization must mirror `annotate_eoe/2` and `newlines_pair/2` (e.g., tagging `->` bodies and paren metadata). Current text treats EOE generically; add the exact behaviors needed to preserve `token_metadata` parity.
- Strings/sigils/heredocs: the draft says “rebuild parts lists” but skips the mechanics (linear tokens → parts, indentation stripping, delimiter/modifier metadata, interpolation error recovery, synthesized end tokens). This needs to be made concrete because Toxic exposes raw linear ranges only.
- Recovery/terminators: plan to use the `pre_terms` snapshots from Toxic buffer entries so recovery decisions are made with the same stack the lexer saw. Otherwise refills may drift and resync too far.
- AST/event schema: define the error node payloads, node kinds, and metadata strategy (columns on/off, `closing`, `parens`, `newlines`) so the builder can be audited against `Code.string_to_quoted` in strict mode.
- Fuel: the draft keeps a fuel cap; with deterministic token consumption per rule, this should be a debug guard only (assert progress) rather than a behavior lever that can truncate parses under load.
- Comments/trivia: Toxic can preserve comments via callback; decide whether to surface them as events/trivia or drop them. The HLD should state the default and how formatters/LSPs can opt in.

## Recommended additions
- Map/bitstring specifics: clarify `assoc_update` vs `assoc_update_kw`, kw-only maps, and bitstring segment parsing rules (modifiers, trailing commas) to ensure parity with `container_expr` rules.
- Environment stream: define how scope/import/alias events piggyback on the event log (or a parallel channel) and at which grammar hooks they are emitted for `Spitfire.Env`.
- Strict vs tolerant mode: document how strict parsing aligns with `Code.string_to_quoted/2` (no synthetic closers, lexer errors are fatal) and how tolerant mode annotates synthesized tokens/nodes.
- Incremental parsing: note how checkpoints/slices are used (or avoided) for regional reparses; identify stable subtree boundaries (EOE, block endings) to minimize churn.
- Testing matrix: add plan to cover kw-after-call/container errors, open heredocs/sigils, `no_parens_many` nesting bans, `do`-block collisions, and lexer error tokens flowing through to AST.

## Open questions
- Is the event log the single source of truth (AST built from events only), or will AST be constructed on the fly with an optional event mirror? This affects memory and incremental hooks.
- How will `existing_atoms_only` / identifier sanitization from Toxic be surfaced in nodes or diagnostics?
- What is the chosen shape for error nodes (`{:__error__, meta, payload}`?) and how will diagnostics be aggregated without duplicating lexer errors?
- Will we expose comment/trivia ranges to formatters, or rely solely on token streams for that use case?
