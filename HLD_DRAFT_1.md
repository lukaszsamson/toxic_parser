# Error-Tolerant Elixir Parser — High-Level Design (Draft 1)

## 1) Goals & Constraints
- Faithfully implement the canonical Elixir grammar (`elixir_parser.yrl`), including `matched_expr`, `unmatched_expr`, and `no_parens` families, without relying on post-parse normalization.
- Always produce a usable tree for IDE/LSP tooling (completion, diagnostics, formatting, environment queries) even on malformed input.
- Integrate tightly with the Toxic streaming lexer (error tokens, synthesized closers, terminator stack, ranged metadata, narrowly scoped `peek_n`, checkpoints).
- Keep performance predictable (linear on valid code), avoid unbounded backtracking, and guard with a small fuel/step cap for pathological inputs.
- Preserve lossless source information (ranges, delimiters, indentation, newlines) for formatters and code actions.

## 2) Inputs, Outputs, and External Influences
- **Input:** Toxic stream (`next/1`, `peek/1`, `peek_n/2`, `checkpoint/rewind`), with tolerant-mode error tokens and synthesized structural closers.
- **Output:**
  - Core-compatible AST plus explicit error nodes.
  - Optional Prism-style event log for building CST/green trees and outlines without re-parsing.
  - Diagnostics aggregated from lexer + parser with precise ranges.
  - Environment stream (scope, bindings, aliases, terminators) for `Spitfire.Env`.
- **References:** `elixir_parser.yrl`, Toxic token docs (`ALL_TOKENS.md`, `lib/toxic.ex`), Ruby Prism/YARP (event log, structured recovery), Ruff’s hand-written Python parser (performance, lossless tokens), Rails-at-Scale parser rewrite (maintainability over LR).

## 3) Architectural Layers
- **Token Adapter (Toxic-native):**
  - Normalizes Toxic tokens into parser views (identifier flavors, operator classes, structural delimiters, `eoe` abstraction over `;`/`eol`).
  - Exposes lookahead (`peek`, `peek_n`), checkpoints, pushback, and current terminators; surfaces lexer errors as parser-visible tokens.
  - Honors synthesized closers/identifier sanitization in tolerant mode.
- **Grammar Layer (recursive descent mirroring `yrl`):**
  - One function per key nonterminal: `grammar`, `expr_list`, `expr`, `matched_expr`, `unmatched_expr`, `no_parens_expr`, `block_expr`, `call_args_parens`, `call_args_no_parens_*`, `container_expr`, `stab`, `do_block`, `block_list`, `kw_*`, clause-heavy constructs (`case/cond/receive/try/with` plus `else/catch/rescue/after`), comprehensions/generator qualifiers, `build_paren_stab`-style forms.
  - Required parser context is syntactic expression category only (`:matched | :unmatched | :no_parens`). Semantic validation (guard-only ops, pattern legality) is deferred to an optional validator so tolerant parsing never rejects on semantic grounds.
  - Handles structural constructs (blocks, clauses, containers, strings/sigils, heredocs) that are awkward to force into precedence rules.
- **Expression Layer (Pratt):**
  - Binding powers derived directly from `yrl` precedences (e.g., `@` > dot/dot-call > power > pipe > match > `do`), encoded once with associativity.
  - Small parselets for `nud` (literals, containers, unary ops, attributes, captures, aliases, fn, paren/list/tuple/map/bitstring, dot/parens call) and `led` (binary ops, dot/dot-call, capture arity, `//`, range chaining).
  - `do` blocks are treated as a low-binding suffix only when coming from the grammar layer (`block_expr`), not as a raw operator inside Pratt.
- **Builders + Event Log:**
  - Event log is the primary parser output: emit `start_node`, `end_node`, `token`, `missing`, `synthetic`, `error` with positions/metas. Builders subscribe to events to produce: (a) Core-compatible AST, (b) lossless CST/green tree, (c) outlines.
  - Builders construct correct AST directly (no normalization passes); dotted aliases, `not expr in right` rewrite/warning, pipe, unary `+/-`, `capture_int`, `..//` step, and keyword/do disambiguation are handled in parselets/builders.
  - Error nodes use `{:__error__, meta, payload}` with payload fields like `:kind, :expected, :found, :inner, :synthetic?`; metadata is a superset of `Code.string_to_quoted` (`line`, `column`, `closing`, `parens`, `newlines`, `delimiter`).
- **Recovery Subsystem:**
  - Central helpers for expected-token insertion, missing-node synthesis, and sync scans; consumes lexer error tokens into error nodes.
  - Sync sets are per-nonterminal, using Toxic’s terminator stack to decide whether to synthesize or bail out.

## 4) Grammar Mapping Highlights (from `elixir_parser.yrl`)
- **Top level:** `grammar -> eoe? expr_list eoe?`; `expr_list` separated by `eoe` (`;` or `eol`). Centralize EOE consumption; commas carry EOL counts but stay commas.
- **Expression families:** Keep `matched_expr`, `unmatched_expr`, `no_parens_expr` distinct. No-parens subtypes (`one`, `many`, `one_ambig`) follow the documented arity/ambiguity rules; ambiguous forms prefer outer arity 1 as in `yrl`.
- **Calls:** Support paren calls, nested paren calls, no-parens (one/many/ambig), and do-block carriers (`dot_do_identifier`, `dot_identifier ... do_block`, etc.). Respect restrictions on nested no-parens with arity > 1 and do-block nesting bans; spell out keyword-last rules and `do:` vs `do/end` attachment (`maybe_bad_keyword_*` warnings mirrored in diagnostics).
- **Operators:** Map `*_op_eol` categories to Pratt binding powers with correct associativity; include combined operators (`not in` rewrite to `not(in ...)` with warning), `..//` step constraint, `...`, `capture_int` + `int`, space-sensitive `dual_op`, and pipe/arrow family warnings.
- **Containers:** Lists, tuples, maps/structs (assoc/update vs `assoc_update_kw`, kw-only maps), bitstrings with segment/modifier parsing (including trailing comma rules), kw tails, empty forms, access syntax (`expr[...]`) using bracket identifiers and `capture_int`.
- **Blocks/Clauses:** `do ... end`, anonymous `fn ... end`, stab clauses (paren/no-paren/empty/bare/guard variants), block labels (`else/catch/rescue/after`), and comprehension/with/case/cond/receive/try flows parsed with clause-aware loops and EOE handling.
- **Strings/Sigils/Heredocs/Quoted identifiers & atoms:** Consume Toxic’s linearized tokens, rebuild parts lists with indentation stripping, delimiters/modifiers, synthesized end handling, and interpolation recovery (`start` → fragments/interpolations → `end`, with error nodes on unterminated spans); honor safe/unsafe keyword/identifier variants and `do/paren/bracket/op` rewrites.

## 5) Binding Powers, Lookahead, and EOE
- **Binding power table (from `yrl` precedences):** `do` 5 (L); `->` 10 (R); `,` 20 (L); `<-`/`\\` 40 (L); `when` 50 (R); `::` 60 (R); `|` 70 (R); `=>` 80 (R); `&`/`...` 90 (nonassoc); `=` 100 (R); `or`/`||`/`|||` 120 (L); `and`/`&&`/`&&&` 130 (L); comparisons 140 (L); relations 150 (L); arrow family (`|>`, `~>`, `<~`, shifts) 160 (L); `in` / `not in` 170 (L); `^^^` 180 (L); `//` 190 (R, special with `..`); concat/range `++ -- +++ --- <> ..` 200 (R); `+ -` 210 (L); `* /` 220 (L); `**` 230 (L); unary ops 300 (nonassoc); dot/dot-call 310 (L); `@` 320 (nonassoc); access adornment 330 (nonassoc).
- **Lookahead:** Default to single-token `peek`. Use `peek_n` narrowly for: (1) distinguishing `identifier [ ... ]` call vs bare list start, (2) `no_parens_one_ambig` detection, (3) quoted identifier follow-ups (`(`, `[`, `do`) rewrites, (4) do-block attachment disambiguation. Avoid speculative rewinds elsewhere.
- **EOE/newlines:** Normalize `eol`/`;` into logical EOE in `expr_list`/block bodies. After `*_op_eol`, consume optional newline before RHS. Inside containers, newlines are insignificant separators unless accompanied by commas. Preserve `annotate_eoe`-style metadata (`end_of_expression`, `newlines`, `parens`/`closing`) when token metadata is enabled.

## 6) Error Tolerance & Recovery Strategy
- **Lexer errors:** `{:error_token, meta, %Toxic.Error{}}` become `{:__error__, meta, payload}` nodes immediately; payload preserves lexer code/domain/details and is not double-reported in diagnostics.
- **Missing pieces:** When an expected token/closer is absent, emit a `missing` event and synthetic token node with a diagnostic; prefer Toxic’s synthesized closers when available and mark metas with `synthesized: true` and zero-width ranges.
- **Sync points:** Per rule, e.g., expression (`eoe`, commas, closers, `end`), containers (`]`, `}`, `>>`, commas), blocks (`else/catch/rescue/after/end`), clauses (`->`, `when`), map/struct (`=>`, `}`), call args (`)`/`,`), using `current_terminators/1` and `pre_terms` snapshots from Toxic buffer entries to avoid over-skipping nested constructs.
- **Fuel/loop guard:** Debug-only assert on progress; if tripped, emit one fatal error node and fast-forward to the next high-level sync (EOE or top terminator) to keep IDEs responsive.
- **Ambiguity handling:** No-parens ambiguity resolved per grammar; emit warnings/diagnostics for ambiguous pipes into calls, missing parens after `do`-capable operators, keyword-last violations, `not in` rewrite, and `..//` misuse, mirroring upstream behaviors.

## 7) IDE-Oriented Behaviors
- **Lossless data:** Keep ranged metas, delimiters, indentation, newlines, and tokenizer-provided extras (`from_brackets`, `parens`, `closing`, `delimiter`).
- **Environment stream:** Emit bindings/imports/aliases/module nesting events alongside AST to back `Spitfire.Env` without re-traversal.
- **Terminator awareness:** Expose current terminators/expected closer for completions and structural hints; propagate synthesized closers into AST/events.
- **Incremental-ready:** Event log + Toxic slicing/checkpoints enable regional reparse; stable node kinds and ranges minimize churn for IDE diffs.
- **Comments/trivia:** Default: dropped. Optional: preserve via Toxic callback; surface as comment events/trivia lists or attach to following node for formatters/LSPs.
- **Strict vs tolerant modes:** Strict aligns with `Code.string_to_quoted/2` (lexer errors fatal, no synthetic closers). Tolerant keeps lexer errors as nodes and accepts synthesized structure; diagnostics annotate synthesized pieces.

## 8) Performance & Predictability
- Streaming tokens (on-demand batches) from Toxic; bounded lookahead (rare use of `peek_n`, checkpoints only around truly ambiguous constructs).
- Single-pass parse/build; no normalization walks. Memory proportional to nesting depth + event log (configurable to discard once AST built).
- Avoid recursion explosions by delegating precedence to Pratt and structure to the grammar layer; guard with step cap.

## 9) Testing & Validation
- **Conformance:** Compare AST (meta-insensitive) against `Code.string_to_quoted/2` for valid corpus (stdlib + fixtures) in strict mode; ensure tolerant mode matches shape when errors absent. Include kw-last rules, `maybe_bad_keyword_*`, and `warn_pipe` cases.
- **Resilience:** Fuzz invalid inputs (including Toxic error-mode cases), assert parser returns AST with bounded diagnostics and no crashes/timeouts; property tests on random token streams.
- **Corpus:** Parse large Hex/stdlib corpus; target 100% non-crash, parity on valid files.
- **Targeted fixtures:** No-parens nesting bans, pipe into call ambiguity, missing `end`/closers, open heredocs/sigils, `do:` vs `do/end`, keyword-position errors, `not in` rewrite, `..//` misuse, `capture_int` spacing, map/bitstring modifiers, stab-in-parens forms.
- **Performance checks:** Large-file benchmarks; ensure linear behavior and reasonable allocations.

## 10) Implementation Phases (Suggested)
1. Token adapter + EOE abstraction + diagnostic plumbing for lexer errors/synthesized closers.
2. Pratt core with verified precedence table; `nud`/`led` coverage for literals, unary, dot/dot-call, binary ops, `not in`, `..//`, `capture_int`.
3. Grammar layer for expression families, calls (paren/no-parens/do), containers, access syntax, kw rules/do vs keyword conflicts.
4. Blocks/clauses (`do_block`, `fn`, `case/cond/receive/try/with`, comprehensions), stab parsing variants, strict vs tolerant switches.
5. Strings/sigils/heredocs/quoted identifiers & atoms reconstruction with delimiter/indent metadata and interpolation recovery.
6. Event log + builders (AST/CST/outline) + environment emission + comment handling option.
7. Recovery polish (sync sets, missing-token synthesis, ambiguity warnings) + performance tuning + conformance/resilience test suites.

## 11) Next steps
1. Finalize binding power table and per-rule sync sets against `elixir_parser.yrl`.
2. Lock the event log schema (node kinds, error/missing/synthetic payloads, metadata policy) and AST builder contract.
3. Define strict/tolerant behaviors (including diagnostics for synthesized tokens) and publish the conformance/resilience test corpus and property-based harness.
