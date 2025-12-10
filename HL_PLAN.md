# ToxicParser Implementation Plan

## Overview

Implement an error-tolerant Elixir parser that integrates with the Toxic streaming lexer, producing IDE/LSP-ready AST even on malformed input. The parser faithfully implements the canonical Elixir grammar (`elixir_parser.yrl`) using a hybrid recursive-descent + Pratt parsing approach.

**Key Features:**
- Event log for CST/green tree building and incremental parsing
- Comment preservation (compatible with `Code.string_to_quoted_with_comments`)
- Environment stream for scope/bindings/aliases tracking

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                         ToxicParser (Public API)                     │
├─────────────────────────────────────────────────────────────────────┤
│  Token Adapter Layer                                                 │
│  ├── Normalizes Toxic tokens → parser views                         │
│  ├── Exposes peek/peek_n/checkpoint/rewind                          │
│  └── EOE abstraction (eol/; → logical end-of-expression)            │
├─────────────────────────────────────────────────────────────────────┤
│  Grammar Layer (Recursive Descent)                                   │
│  ├── Expression dispatcher: expr → matched/unmatched/no_parens      │
│  ├── Containers: list/tuple/map/bitstring                           │
│  ├── Blocks: do_block/fn/stab/clauses                               │
│  └── Calls: paren/no-paren/do-block variants                        │
├─────────────────────────────────────────────────────────────────────┤
│  Expression Layer (Pratt Parser)                                     │
│  ├── Binding powers from yrl precedence table                       │
│  ├── Scope: matched_expr/unmatched_expr/no_parens_expr + *_op_expr  │
│  ├── NUD: literals, containers, unary ops, captures                 │
│  └── LED: binary ops, dot/call, access                              │
├─────────────────────────────────────────────────────────────────────┤
│  Event Log & Environment                                             │
│  ├── Event emission: start_node/end_node/token/error/missing        │
│  ├── Comment trivia attachment to following nodes                   │
│  └── Environment stream: scope/bindings/aliases/imports             │
├─────────────────────────────────────────────────────────────────────┤
│  Builders & Recovery                                                 │
│  ├── AST builders (Core-compatible format)                          │
│  ├── Error nodes: {:__error__, meta, payload}                       │
│  └── Recovery: sync points, missing-token synthesis                 │
└─────────────────────────────────────────────────────────────────────┘
```

## Implementation Phases

### Phase 0: Contracts & Harness
- Document strict vs tolerant behavior (lexer errors fatal vs emitted, synthesized closers, diagnostics aggregation, comment policy).
- Lock event log schema (node kinds, ordering guarantees, `error`/`missing`/`synthetic` payloads) and metadata policy (ranges, delimiters, newlines, synthesized flags).
- Define AST builder contract and error node shape (`{:__error__, meta, payload}`) plus how builders subscribe to events for AST/CST/outline/environment.
- Stand up a conformance harness that compares strict-mode AST to `Code.string_to_quoted/2` and exercises tolerant mode on the same cases; wire into CI early.
- Adopt `NONTERMINALS_GPT.md` as the RD vs Pratt mapping (dispatcher vs core vs helpers) and keep it in sync with implementation/tests.

### Phase 1: Token Adapter & State
- Implement state struct (`stream`, `current/peek`, checkpoints, `terminators`, `expression_context`, `fuel`, `opts`, diagnostics).
- Token adapter over Toxic linear stream: `next/peek/peek_n/checkpoint/rewind`, bounded `peek_n` (only for documented ambiguities), surface `current_terminators`.
- EOE abstraction: normalize `eol`/`;` into logical EOE with newline counts preserved; respect `*_op_eol` metadata for layout-sensitive operators.
- Surface tolerant-mode details: pass through `error_token` as parser-visible errors, honor synthesized closers, and keep lexer diagnostics available for error nodes.

### Phase 2: Event Log & Builder Skeletons
- Implement event emitters (`start_node/end_node/token/missing/synthetic/error/comment`) with ordering guarantees; store metadata per Phase 0 policy.
- Add lightweight builder helpers for literals/unary/binary/dot/access so Pratt/grammar code can emit AST as it lands; ensure dotted aliases concatenate.
- Thread event emission through adapter utilities to avoid retrofitting later.

### Phase 3: Pratt Expression Core
- Finalize binding powers from `yrl` (including dot vs dot-call, access adornment as highest, `do` not treated as Pratt operator).
- Parselets: NUD (literals, identifiers, unary `@/&/!/^/not/+/-`, containers, `fn`, `...`, nullary `..`, `capture_int`), LED (binary ops, dot/dot-call, access `[]`, `not in` combined operator, `..//` step).
- Scope Pratt core strictly to `matched_expr`/`unmatched_expr`/`no_parens_expr` and `matched_op_expr`/`unmatched_op_expr`/`no_parens_op_expr`; keep `expr` as an RD dispatcher.
- Treat `*_op_eol` and `dot_op` as operator helpers: parsed via simple RD rules but used only by Pratt parselets.
- Honor `dual_op` spacing rules from token shapes; attach do-blocks only via grammar layer (`block_expr`), not inside Pratt loop.

### Phase 4: Grammar Layer — Expressions
- Top-level `grammar -> eoe? expr_list eoe?` with centralized EOE handling.
- Implement matched/unmatched/no_parens families with context threading; include `block_expr` (`dot_do_identifier`, paren + nested paren + do).
- Implement `expr` as a small RD dispatcher onto the three Pratt contexts, with no precedence logic of its own, matching `NONTERMINALS_GPT.md`.
- Spell out `no_parens_one/many/one_ambig` rules, arity-based nesting bans, and ambiguity resolution (outer arity 1). Emit warnings analogous to `warn_pipe` and `warn_no_parens_after_do_op`.

### Phase 5: Calls & Identifiers
- Identifier classification (`identifier`, `paren/bracket/do/op_identifier`, dot variants) including quoted identifier rewrites (`quoted_paren/bracket/do/op_identifier_end`).
- Call variants: paren calls, nested paren calls, no-parens (one/many/ambig), do-block carriers, keyword-last enforcement, and `do:` vs `do/end` disambiguation (`maybe_bad_keyword_*` parity).
- Limited `peek_n` to distinguish access vs list start and no-parens ambiguity; attach events/builders inline.

### Phase 6: Containers & Access
- Lists/tuples, maps/structs (assoc, update, kw-only), bitstrings with segment/modifier parsing and trailing comma rules, `capture_int` spacing inside segments.
- Container arg handling: kw tails last, EOE normalization inside containers, sync on `]`, `}`, `>>`, commas using terminator stack snapshots.
- Access syntax (`expr[...]`) integrated at Pratt/grammar boundary.

### Phase 7: Blocks, Clauses & Environment
- Do-blocks, `fn`, stab variants (paren/no-paren/empty), block labels (`else/catch/rescue/after`) with clause-aware loops and EOE handling.
- Comprehensions/`with`/`case`/`cond`/`receive`/`try` flows with guard handling and do-block attachment rules.
- Emit environment events as constructs are parsed: enter/exit scopes, bindings in patterns (match LHS, fn args, clause patterns, generators), aliases/imports/uses/requirements.

### Phase 8: Strings, Sigils, Heredocs & Quoted Forms
- Consume Toxic linear tokens (`*_start` → `string_fragment`/interpolations → `*_end`) with indentation stripping for heredocs/sigil heredocs; synthesize missing end tokens in tolerant mode.
- Interpolation recovery via `begin_interpolation`/`end_interpolation` and recursive expression parsing.
- Sigils with modifiers and delimiters; quoted atoms/keywords/identifiers (safe/unsafe) and metadata preservation; ensure keyword identifiers collapse appropriately.

### Phase 9: Recovery & Fuel Guards
- Define per-rule sync sets (expr: EOE/commas/closers/end; containers: `]`/`}`/`>>`/commas; blocks: labels/end) using current terminators.
- Missing-token synthesis vs skip rules; convert lexer `error_token` to `__error__` nodes without double-reporting; emit `missing`/`synthetic` events.
- Fuel/progress guard to prevent infinite loops; fast-forward to top-level sync on stalls.

### Phase 10: Comments & Output Assembly
- Implement comment preservation compatible with `Code.string_to_quoted_with_comments` (trivia attachment vs separate return).
- Finalize how events/env/comments are bundled in return tuples; ensure tolerant vs strict mode toggles control diagnostics and synthesis.

### Phase 11: Public API
- Complete `ToxicParser` public functions (`parse/2`, `parse_string/2`, `parse_file/2`, comment-aware variants).
- Options: `:tolerant` vs `:strict`, `:token_metadata`, `:literal_encoder`, `:existing_atoms_only`, `:preserve_comments`, `:emit_events`, `:emit_env`, `:fuel_limit`, `:terminators` exposure.
- Return contract across combinations of AST/comments/events/env and diagnostic aggregation policy.

### Phase 12: Validation & Performance
- Conformance tests vs `Code.string_to_quoted/2` (strict) and tolerant parity when errors absent.
- Targeted fixtures: no-parens nesting bans, pipe-into-call ambiguity, keyword-last, `maybe_bad_keyword_*`, missing `end`/closers, open heredocs/sigils, `not in` rewrite, `..//` misuse, `capture_int` spacing, lexer `error_token` passthrough.
- Property/fuzz tests (including Toxic error-mode streams) and Spitfire property generators where applicable; large Hex/stdlib corpus and perf benchmarks.

## Critical Files to Create

```
lib/toxic_parser/
├── state.ex              # Parser state struct
├── token_adapter.ex      # Token normalization layer
├── precedence.ex         # Binding power table
├── pratt.ex              # Pratt parser core
├── grammar.ex            # Grammar layer entry
├── grammar/
│   ├── expressions.ex    # Expression families
│   ├── calls.ex          # Call parsing
│   ├── containers.ex     # List/tuple/map/bitstring
│   ├── blocks.ex         # Do blocks and clauses
│   ├── stab.ex           # Stab clause parsing
│   └── strings.ex        # Strings/sigils/heredocs
├── event_log.ex          # Event log infrastructure
├── comments.ex           # Comment handling
├── env.ex                # Environment tracking
├── env/
│   └── scope.ex          # Scope management
├── recovery.ex           # Error recovery logic
├── error.ex              # Error/diagnostic types
└── builder.ex            # AST construction helpers
```

## Dependencies

- Toxic lexer (already configured as path dependency)
- StreamData (test dependency, already configured)

## Key Design Decisions

1. **No normalization passes**: Build correct AST directly in parselets
2. **Single expression context parameter**: Only `:matched | :unmatched | :no_parens`
3. **Semantic validation deferred**: Parser accepts semantically invalid but syntactically correct code
4. **Bounded lookahead**: Default single-token peek, `peek_n` only for specific cases
5. **Event log included**: Events emitted during parsing for CST/incremental support
6. **Comment preservation**: Compatible with `Code.string_to_quoted_with_comments`
7. **Environment stream**: Scope/binding events for IDE integration

## Testing Priorities

1. Expression precedence chains (Pratt vs `yrl` table) in strict mode parity with `Code.string_to_quoted/2`
2. No-parens call nesting rules and `warn_pipe`/`warn_no_parens_after_do_op` equivalents
3. Do-block attachment disambiguation across paren/no-paren/nested paren calls
4. Missing closer recovery and lexer `error_token` passthrough (tolerant) without double-reporting
5. Keyword-last rule enforcement and `maybe_bad_keyword_*` compatibility
6. `not in` rewrite correctness and diagnostics
7. Range + step (`..//`) constraint and `capture_int` spacing rules
8. Strings/sigils/heredocs: open/unterminated handling, interpolation recovery, indentation stripping
9. Comment preservation accuracy vs `Code.string_to_quoted_with_comments`
10. Environment event correctness (scopes, bindings, aliases/imports/uses) including tolerant-mode synthesis effects
11. Property/fuzz suites (including Toxic error-mode streams) and large corpus/performance regressions
