# Spitfire Error-Tolerant Parser — HLD Draft 1a (Codex)

## Goals & Constraints
- Parse full Elixir grammar (see `elixir_parser.yrl`) into a Core-compatible AST while staying useful on malformed input.
- Serve IDE/LSP use-cases: completions, formatting, diagnostics, environment queries, code actions, partial type inference.
- Integrate with the Toxic streaming lexer for tolerant tokenization, positional accuracy, and structural synthesis.
- Maintain predictable performance (fuel/step caps) and memory use; avoid unbounded recursion and pathological backtracking.
- Preserve concrete source details (range, original text where needed) for formatting and fixes.

## Inputs & Outputs
- **Input**: Toxic stream (`Toxic.next/1`, `peek/1`, `checkpoint/1`) emitting ranged tokens, error tokens, and synthesized closers.
- **Output**:
  - **AST**: Core-compatible nodes, plus error/placeholder nodes for gaps.
  - **Events**: Optional Prism-style event log for secondary builders (green tree, CST, outline).
  - **Diagnostics**: Collected lexer and parser errors/warnings with spans.
  - **Environment**: Scope + terminator stacks to support `Spitfire.Env` queries mid-parse.

## Architectural Shape
- **Driver Layer**
  - Wrap Toxic stream; expose lookahead (1–2 tokens), checkpoint/rewind, and terminator introspection.
  - Normalize error tokens into parser diagnostics; keep them in the token flow for recovery.
  - Respect Toxic’s synthesized structural tokens (closers, identifier sanitization) when in tolerant mode.

- **Event Log + Builders**
  - Emit parse events in memory, tree built after (Prism/Ruff style). Facilitates lossless trees and multiple projections (AST, CST, outline) without re-parsing.
  - **Tree Builder**: Consumes events into AST and optional green tree; inserts missing nodes/closers based on recovery hints.

- **Expression Core (Pratt)**
  - Binding powers derived from `elixir_parser.yrl` precedence table (at_op 320–330, dot 310, power 230, … do 5).
  - **nud**: literals, atoms, aliases, sigils/strings, fn, lists/tuples/maps/bitstrings, anonymous calls, capture, unary ops, attribute (`@`).
  - **led**: infix/postfix: dot/dot-call, pipes, ranges, arithmetic, boolean, match/when/in, stab (`->`), ternary, capture arity, `//`, `...`.
  - **Call Forms**: Paren calls, no-parens one/many/ambig, do-block calls, nested paren calls; respect spacing/ambiguity rules from the grammar comments.
  - **Pattern Context**: Pratt entry takes mode (`:value | :pattern | :guard`) to gate constructs (e.g., disallow map update in patterns, restrict pin/unary).

- **Non-Expression Constructs**
  - **Blocks**: do/end, fn/end, case/cond/receive/try/rescue/catch/after, with stab bodies. Align with block precedence (do = 4/5) to bind loosely.
  - **Containers**: lists, tuples, maps/structs, bitstrings; parse elements with eol/comma sensitivity; handle keyword tails.
  - **Clauses**: match, with, case, cond, fn clauses; parse head/guard/body with layout-aware recovery.
  - **Aliases/Remote Calls**: dot chains, quoted identifiers, paren/bracket/do-ident rewrites mirroring Toxic’s identifier flavors.

## Error Tolerance & Recovery
- **Lexer-Level**: Accept `:error_token` inline; treat synthesized closers as authoritative. Record lexer warnings/errors.
- **Parser-Level**:
  - **Sync Tokens**: newline, semicolon, comma, closers (`) ] } >> end`), `do/end`, `fn/end`, pipe boundary, clause keywords, and Toxic sync hints.
  - **Missing Pieces**: Synthesize expected tokens/nodes (e.g., missing `)`/`]`/`}`/`end`, missing `->`, missing `do`) with diagnostics; continue.
  - **Ambiguity Handling**: Prefer narrower no-parens interpretations (`no_parens_one_ambig` → outer arity 1), warn when nested do-blocks would be illegal.
  - **Fuel Guard**: Cap Pratt recursion/steps; on exhaustion emit `{:error, :fuel_exhausted}` node and fast-forward via sync tokens.
  - **Contextual Recovery**: Mode-aware recovery (pattern vs expr vs guard) to avoid cascading operator misuse.
  - **Terminator Stack**: Mirror Toxic’s terminator stack to decide which closers are recoverable vs fatal; expose to IDE queries.

## IDE-Oriented Behaviors
- **Lossless Data**: Store token text for formatters/quick fixes; keep range metadata exact and exclusive.
- **Incremental Hooks**: Checkpoint/rewind around heuristics; allow reparse of slices (align with Toxic.slice/6) using event log and stable node IDs.
- **Environment API**: During parse, emit scope events (bindings, imports, aliases, module nesting) to back `Spitfire.Env`.
- **Completion/Navigation**: Provide current terminators, expected token set, and nearest enclosing construct for interactive features.

## Grammar Coverage Plan (from `elixir_parser.yrl`)
- **Operator Precedence**: Mirror precedence table; map each `_op_eol` to Pratt powers; keep dot/dot-call left-assoc at power 310; at-operator nonassoc high.
- **Calls**: Implement paren/no-parens/do-block permutations (`dot_call_identifier call_args_parens do_block`, `dot_identifier call_args_no_parens_all do_block`, etc.).
- **Containers & Bitstrings**: Lists/tuples/maps/bitstrings with assoc/update, bit modifiers, `%{}` vs `%` struct, kw tails, empty forms.
- **Blocks & Clauses**: do/fn/case/cond/receive/try with clause lists; guard parsing (`when_op_eol`), stab parsing (`stab_op_eol`).
- **Strings/Sigils/Heredocs**: Consume linearized tokens into CST parts, then collapse into AST nodes; preserve indentation and modifiers.
- **Quoted identifiers/atoms**: Respect safe/unsafe/kw flavors, quoting end tokens, and do/paren/bracket rewrites.

## Cross-Paradigm Influences
- **Prism (Ruby)**: Event log + builder; precise error nodes; incremental-friendly; separation of parse and build. Adopt event stream and error marking strategy.
- **Ruff/Rust Python Parser**: Lossless tokens, explicit trivia handling, and bounded parsing to avoid exponential blowups. Use similar fuel and sync heuristics.
- **Rails-at-Scale Ruby Parser rewrite**: Emphasize maintainability and error recovery over LR tables; prefer handwritten Pratt/recursive descent for clarity and IDE needs.

## Key Data Structures
- `Parser.State`: token stream handle, mode, fuel counter, terminator stack mirror, diagnostics accumulator, options.
- `Event`: `:start_node`, `:end_node`, `:token`, `:error`, `:missing`, `:attach_trivia`; includes ranges and recovery tags.
- `Node`: AST node with type, children, range, origin tokens; optional green-tree backing for structural sharing.

## Algorithms (Sketch)
- **parse/2**: Initialize state from Toxic stream; parse `grammar` -> block of expressions separated by `eoe` (newline/semicolon/end-of-input).
- **parse_expr(mode, min_bp)**: Pratt loop over nud/led with binding powers from table; handle `do` precedence by lowering when a `do`-block follows.
- **parse_block(kind)**: Consume `do ... end` with clause-aware bodies; recover missing `end` via terminator stack and Toxic closers.
- **parse_call**: Detect identifier flavor (paren/bracket/do/dot) and call arguments (paren args, no-parens one/many/ambig, kw, nested parens).
- **recover(expected, sync_set)**: Emit error event, optionally synthesize token/node, advance to nearest sync token or terminator boundary.

## Testing & Validation
- **Conformance**: Round-trip valid Elixir corpus comparing against `elixir_parser.yrl` AST; ensure parity for good inputs.
- **Resilience**: Fuzz malformed inputs; assert bounded fuel and presence of AST with error nodes (no crashes).
- **IDE Scenarios**: Incomplete constructs (open heredoc, missing `end`, unfinished pipe) produce stable CST/AST and actionable diagnostics.
- **Performance**: Bench against large files; ensure streaming consumption and limited allocations via event log.

## Delivery Phases
1) **Token plumbing**: Integrate Toxic stream + terminators + error tokens; expose lookahead/rewind utilities.
2) **Pratt core**: Implement operator table + nud/led coverage for expressions and patterns.
3) **Blocks/clauses/containers**: Handwritten recursive descent for non-expression grammar parts; map `yrl` productions to functions.
4) **Events + builders**: Emit event log; build AST and optional green tree; attach diagnostics.
5) **Recovery polish**: Sync heuristics, synthetic nodes, ambiguity warnings; align with IDE behaviors.
6) **Validation**: Conformance + resilience suites; property/fuzz tests; integration with `Spitfire.Env`.
