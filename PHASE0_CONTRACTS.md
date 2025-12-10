# Phase 0: Contracts & Harness

This project will deliver an error-tolerant Elixir parser that mirrors the `elixir_parser.yrl` grammar while emitting a structured event log suitable for AST/CST/environment builders. Phase 0 locks the external contracts and sets up the conformance harness before parser code lands.

## Strict vs Tolerant Modes
- `:mode` option defaults to `:strict`; `:tolerant` keeps parsing past errors.
- Strict: lexer errors are fatal; no synthesized delimiters; diagnostics stop at the first parser error and return `{:error, %ToxicParser.Error{}}`.
- Tolerant: lexer `error_token` values are surfaced as `:error` events; missing closers/openers may be synthesized (`:missing`/`:synthetic` events) to keep the tree well formed; all diagnostics are aggregated and returned alongside the best-effort AST/event log.
- Comments are preserved in both modes; attachment prefers the following syntactic element and falls back to the previous token if at EOF. Ordering of comments in the result matches `Code.string_to_quoted_with_comments/2`.
- Layout-sensitive operators (`*_op_eol` tokens) keep newline counts in metadata; Pratt/grammar layers are responsible for enforcing spacing rules using the `:newlines` metadata.
- Lexer fatal errors are treated as fatal only in strict mode; tolerant mode converts them into diagnostics plus error tokens and resumes where possible.
- Both modes expose lexer diagnostics; tolerant mode downgrades lexer fatal errors into parser-visible events/diagnostics instead of aborting.

## Comment Policy
- Comments are preserved when `:preserve_comments` is true and dropped otherwise.
- Comments attach to the following token/node when possible; if the comment is the last element, it attaches to the preceding token.
- Ordering in comment lists matches `Code.string_to_quoted_with_comments/2`; event log consumers may instead rely on `:comment` events preceding the attached token/node.

## Event Log Schema
- Events are emitted in source order with well-nested `:start_node`/`:end_node` pairs; tokens/comments/errors/missing/synthetic entries appear where they occur in the stream.
- Event tuple shapes:
  - `{:start_node, node_kind(), metadata()}`
  - `{:end_node, node_kind(), metadata()}`
  - `{:token, %{kind: token_kind(), value: term()}, metadata()}`
  - `{:error, %{phase: :lexer | :parser, reason: term(), severity: :error | :warning, token: atom() | nil, expected: [atom()] | nil}, metadata()}`
  - `{:missing, %{expected: atom()}, metadata()}`
  - `{:synthetic, %{kind: :delimiter | :token, value: term()}, metadata()}`
  - `{:comment, %{text: String.t(), inline?: boolean()}, metadata()}`
- Node kinds cover the CST surface (`:root`, `:expr`, `:matched_expr`, `:unmatched_expr`, `:no_parens_expr`, `:call`, `:list`, `:tuple`, `:map`, `:bitstring`, `:fn`, `:do_block`, `:stab`, `:access`, `:dot`, `:unary_op`, `:binary_op`, `:string`, `:sigil`, `:alias`, `:identifier`, `:keyword`, `:container_args`, `:block`, `:clause`, `:pattern`, `:interpolation`, `:comment_block`). Additional node kinds may be added only with corresponding builder/test updates.

## Metadata Policy
- `metadata()` entries carry:
  - `:range` — `%{start: location(), end: location()}` where a location is `%{offset: non_neg_integer(), line: pos_integer(), column: pos_integer()}`.
  - `:delimiter` — `nil | {:paren | :bracket | :curly | :bitstring | :sigil, {open :: String.t(), close :: String.t()}}`.
  - `:newlines` — `non_neg_integer()` counting preceding line breaks for layout-sensitive operators; preserved even when the token collapses into an EOE abstraction.
  - `:synthesized?` — true when the token/node did not originate from the lexer (paired with `:synthetic` or `:missing` events).
  - `:terminators` — snapshot of the active terminator stack for recovery/sync logic.
  - `:role` — `:open | :close | :none` for delimiter-carrying tokens/nodes.
- Ordering guarantees: `:start_node`/`:end_node` pairs are properly nested; `:missing` and `:synthetic` events are placed where the grammar expected the construct; `:comment` events precede the node they attach to.

## AST Builder Contract
- Error node shape is locked to `{:__error__, meta :: Macro.metadata(), payload :: map()}`; `payload` mirrors `:error` event payloads and may include a `:children` key for partial AST fragments.
- Builders subscribe to the event stream via a reducer interface:
  - `c:init/1` → initial builder state from options
  - `c:handle_event/2` → consume one `event()` with the current state
  - `c:finalize/1` → produce the final artifact (AST/CST/outline/environment)
- Builders must tolerate `:missing`/`:synthetic` events and can choose to emit error nodes or drop malformed spans depending on mode. Multiple builders may be run over the same event stream to produce AST plus environment data.

## Conformance Harness
- Strict-mode harness compares `ToxicParser.parse_string/2` output to `Code.string_to_quoted_with_comments/2` on a shared corpus; mismatches report AST and comment diffs.
- Tolerant-mode harness runs the same corpus to ensure parity when no errors are present and to assert diagnostics aggregation is non-empty on intentionally broken fixtures (once added).
- Harness is wired into CI via ExUnit tests so the contract is enforced before parser implementation lands.

## Nonterminal Mapping
- `NONTERMINALS_GPT.md` is the canonical mapping of recursive-descent vs Pratt responsibilities; parser modules and tests MUST stay in sync with this file. Any changes to the mapping require a matching code/test update.
