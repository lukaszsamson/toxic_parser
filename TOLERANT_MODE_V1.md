# ToxicParser Tolerant Mode V1 (Design Spec)

## 0. Scope

This document specifies a **tolerant parsing mode** for ToxicParser that can:

- **Continue past lexer and parser errors**, producing a best-effort AST.
- Record **structured diagnostics** that are constrained to **source regions** and **specific AST subnodes**.
- Consume **tolerant-mode tokens from the Toxic lexer** (including `:error_token` and synthesized structural tokens) **without “fixing” them again**.
- When the parser must synthesize AST shape, those nodes **must not claim real source ranges**.

This is a design/spec only (not an implementation plan), but it is written to map cleanly onto the existing code:

- `ToxicParser.Error` diagnostics (`lib/toxic_parser/error.ex`)
- tolerant lexer transport via `Cursor` and `TokenAdapter` (`lib/toxic_parser/cursor.ex`, `lib/toxic_parser/token_adapter.ex`)
- existing tolerant recovery at the expression-list layer (`lib/toxic_parser/grammar/expressions.ex`)
- existing lexer `:error_token` handling in Pratt `nud/…` (`lib/toxic_parser/pratt.ex`)

## 1. Goals and Non‑Goals

### Goals

1. **Forward progress**: tolerant mode must not get stuck in loops on malformed input.
2. **Stable source mapping**:
   - Valid nodes keep their original `line/column` metadata (no drift).
   - Omitted/bypassed tokens must not change adjacent valid nodes’ ranges.
3. **Error locality**:
   - Every diagnostic has a precise **source range** (or explicit “synthetic/no-range” marker).
   - Every diagnostic is attached to a specific **AST subnode** (an inserted error node or metadata on a nearby node).
4. **Lexer-authoritative recovery**:
   - When the lexer already recovered (emitted `:error_token`, synthesized closers/openers, sanitized identifiers), the parser must **accept the stream** and must not attempt its own “repair” of the same error.

### Non‑Goals (V1)

- Perfect AST equivalence with `Code.string_to_quoted/2` on invalid programs.
- Producing compilable AST for invalid programs (the output is for IDE tooling, linters, and incremental analysis).
- Deep semantic repair (e.g., inventing missing identifiers or operators beyond minimal shape preservation).

## 2. Inputs: Token Stream Contract (Lexer → Parser)

Tolerant mode assumes `Toxic.Driver` runs with `error_mode: :tolerant`. The token stream may contain:

1. **Normal tokens**: `{kind, meta, value}` (as today).
2. **Inline error tokens**:
   - `{:error_token, meta, payload}`
   - `payload` is normally `%Toxic.Error{}` (or equivalent tuple, depending on `error_token_payload`).
3. **Synthesized structural tokens** produced by the lexer during recovery:
   - e.g. inserted openers/closers, `:end_interpolation`, etc.
   - Synthesized tokens use **zero-length meta** (`start == end`) to avoid position drift.

### 2.1 Multi-token error sequences (important)

When the lexer encounters an error in tolerant mode, it may emit **multiple tokens** for a single recovery episode (per `Toxic.Driver.Recovery` ordering rules), e.g.:

1. (sometimes) pre-inserted recovery tokens
2. the `:error_token` itself (with a real span)
3. post-inserted recovery tokens (e.g., sanitized identifier variants)
4. synthesized structural tokens (openers/closers, `:end_interpolation`) with **zero-length meta**
5. (sometimes) the “actual closer” token, also with **zero-length meta**

Parser requirement: tolerant mode MUST be able to **consume and interpret** these sequences without:

- creating duplicate repairs (no “double-fix”), and
- generating additional error nodes solely because a closer/opener was synthesized.

In particular: if a synthesized closer is needed to finish a construct that did start correctly (e.g., missing `)`/`]`/`end`), the parser MUST use it to close the AST node; the missing-terminator diagnostic is sufficient.

**Example (conceptual):** input `"("` (missing closer at EOF) may produce:

1. `{:"(", meta_real, nil}`
2. `{:error_token, meta_realish, %Toxic.Error{code: :terminator_missing_closer, ...}}`
3. `{:")", meta_zero_length, nil}` (synthesized closer)
4. `:eof`

The parser should emit a parenthesized AST node closed by the synthesized `")"` token (no extra parser repair), plus an error node/diagnostic anchored to the `:error_token`.

### Parser obligations

- Treat `:error_token` as **opaque**: do not reinterpret, re-escape, or “improve” it.
- Treat synthesized structural tokens as **real tokens in the stream** (i.e., the parser must not crash when they appear). The parser may omit pure artifacts from the AST, but MUST still accept them for balancing/termination when required by the surrounding construct.

## 3. Output Data Model (Result, Diagnostics, Error Nodes)

### 3.1 Result shape

Tolerant mode returns `{:ok, %ToxicParser.Result{...}}` even when diagnostics exist. In V1:

- `result.ast` is **always non-nil** in tolerant mode.
  - On catastrophic failure, it becomes `{:__block__, [], [error_node]}`.
- `result.diagnostics` is a list of `%ToxicParser.Error{}` (existing struct), extended by V1 conventions below.

### 3.2 Diagnostics: `%ToxicParser.Error{}`

Use the existing `ToxicParser.Error` struct fields as the canonical diagnostic record:

- `phase`: `:lexer | :parser`
- `severity`: `:error | :warning`
- `range`: `%{start: %{offset, line, column}, end: %{offset, line, column}}`
- `reason`: term (existing “reason tuple” or structured error)
- `details`: map (extended)

**V1 additions via `details`:**

```elixir
details: %{
  id: pos_integer(),                 # stable within this parse result
  anchor: %{                         # ties diagnostic to a subnode
    kind: :error_node | :node_meta | :root,
    path: list(),                    # AST path (see 3.4; filled by post-pass)
    note: String.t() | nil
  },
  synthetic?: boolean(),             # true if diagnostic refers to synthetic-only construct
  lexer_error_code: atom() | nil,    # if payload originated from %Toxic.Error{code: ...}
  related: [%{range: EventLog.range(), note: String.t()}] | nil,
  recovery: %{strategy: atom(), synced_to: atom() | :eof} | nil
}
```

**Diagnostic ID rules (V1):**

- `details.id` is allocated by parser state, starting at `1` and incrementing per diagnostic.
- IDs are unique within a single parse result and reset between parses.
- If the AST uses the recommended structured error payload, `payload.diag_id` MUST equal `details.id` for the corresponding diagnostic.

**Range rules (V1):**

- Lexer diagnostics MUST use the lexer token’s `meta` range when available.
- Parser diagnostics MUST use the triggering token’s range when available, and MUST NOT fall back to “line-only” ranges when token metadata exists.
- If a range cannot be derived (e.g., metadata disabled), use a zero-length range at a cursor anchor and set `details.synthetic? = true`.

**Operational definition (V1):** the “triggering token” for a parser diagnostic is:

- the token that the parser just consumed and found invalid for the current production, else
- the current lookahead token (`peek`) at the point the error is reported, else
- a zero-length cursor anchor (synthetic).

### 3.3 Error nodes in the AST

Tolerant AST uses explicit error nodes so every error can be localized to a tree subnode.

**Canonical error node shape (V1):**

```elixir
{:__error__, meta, payload}
```

- `meta` is standard AST metadata; it may include `line/column` as an **anchor point**.
- `payload` is either an opaque term (compatibility) or a structured map (recommended).

**Recommended structured payload (V1):**

```elixir
%{
  diag_id: pos_integer(),
  phase: :lexer | :parser,
  kind: :token | :missing | :unexpected | :invalid | :ambiguous | :internal,
  original: term(),                  # required, but may be nil if truly unavailable
  original_text: String.t() | nil,   # optional best-effort textual form
  children: [Macro.t()],             # optional (default: [])
  synthetic?: boolean()              # required
}
```

**Rules:**

- If the error came from the lexer, `original` is the `:error_token` payload.
- If the error came from the parser, `original` is the parser error “reason”.
- If the parser must synthesize placeholders (see 3.5), it should prefer:
  - `{:__error__, meta, ...}` placeholders over inventing “valid” constructs.

### 3.4 “Constrained to subnodes”: Anchoring errors to the AST

To satisfy “errors must be constrained to source regions and tree subnodes”, every diagnostic must be anchored by one of:

1. **Inserted error node** (`anchor.kind == :error_node`)
2. **Existing AST node metadata** (`anchor.kind == :node_meta`)
3. **Root** (`anchor.kind == :root`) only for errors that are truly file-level trivia (e.g., a malformed trailing comment when comments are not preserved)

**AST path encoding (V1):**

- `path` is a zipper-like navigation list, e.g.:
  - `[:root]`
  - `[:root, {:block, 2}]` (3rd expression inside `__block__`)
  - `[:root, {:call_args, 0}]`
  - `[:root, {:list, 1}]`
  - `[:root, {:map_kv, 0}, :key]`

V1 does not require a globally unique node id. The “path” is sufficient for IDE anchoring and can be built by a post-walk over the produced AST.

**Clarification (V1):** `anchor.path` is produced by a **post-parse traversal** over the final AST (not during streaming parse). The parser core only needs to ensure an error is anchored to a subnode via either an error node insertion or node metadata.

### 3.5 Synthetic elements (parser-created)

When the parser must create placeholders to keep an AST shape usable, those nodes are **synthetic** and must not claim real source ranges.

**V1 rule:** synthetic nodes:

- MUST include standard `line:` / `column:` metadata as an anchor point for compatibility with AST tooling. These anchors are insertion points, not source spans.
- MUST NOT claim real spans via metadata (do not add `:end_line` / `:end_column` or other range-like fields).
- MUST be explicitly marked as synthetic via `meta[:toxic] = %{synthetic?: true, anchor: %{line: L, column: C}}`.
- MUST have a corresponding diagnostic with `details.synthetic? = true` and a **zero-length** diagnostic range (`start == end`).

The lexer may also synthesize tokens (structural closers/openers). Those are **not parser-synthetic** and retain their lexer meta, but are marked “synthesized” by zero-length meta (`start == end`).

**Synthesized token detection (V1):** lexer-synthesized tokens can be identified by zero-length token meta where `start == end` (or by derived metadata such as `TokenAdapter.full_metadata(...).synthesized?` when available).

Consumer guidance (V1): tooling must use `meta[:toxic][:synthetic?]` (when present) to distinguish synthetic nodes; the presence of `line/column` alone is not meaningful.

### 3.6 Event log integration (optional, V1-compatible)

If `emit_events` is enabled, tolerant mode SHOULD emit enough information for tooling to correlate:

- the diagnostic (`details.id`) and
- the AST error node payload (`payload.diag_id` when structured)

Minimum recommendation: for every inserted error node, emit an `:error` event with the same `id`, plus `range` and `synthetic?` flags. This is deliberately additive and does not change the AST contract.

## 4. Tolerant Parsing Strategy

### 4.1 Two classes of errors

1. **Lexer errors** (delivered as `:error_token` or as cursor-level errors converted by `TokenAdapter`):
   - Parser must accept the token and turn it into an error node (or attach to nearest node if it is trivia-only).
2. **Parser errors** (unexpected token sequences, disallowed constructions):
   - Parser emits a parser diagnostic and creates an error node *at the smallest recoverable scope*.

### 4.2 Recovery and synchronization

Tolerant parsing uses synchronization points to resume parsing after emitting an error node.

**V1 sync sets (conceptual):**

- `expr_sync`: `[:eol, :";", :",", :"}", :"]", :")", :end, :eof]` (note `:","` token kind for comma)
- `container_sync`: `[ :",", :"]", :"}", :")", :">>", :end, :eof]`
- `call_args_sync`: `[ :",", :")", :"]", :"}", :end, :eof]`
 
These sync sets should be kept compatible with the lexer’s tolerant recovery defaults (notably `:comma`), so the parser doesn’t “lag behind” lexer recovery decisions.

**Forward progress rule:** after any error, consume at least one token (or rely on the lexer’s recovery which already advanced the cursor) before attempting to parse again.

**Synthesized closer metadata (V1):** when a construct is closed by a lexer-synthesized closer, the parser should not “shrink” or otherwise distort the metadata of the surrounding valid node. Prefer:

- retaining the opener-derived node metadata as-is, and
- optionally annotating `meta[:toxic][:synthetic_closer?] = true` if downstream tooling needs to know a closer was synthesized.

### 4.3 Container-scoped recovery (required for practical tolerant parsing)

When an error occurs while parsing an element inside a container (list/tuple/map/bitstring/call args):

1. Emit an error node for **only the failing element** (not the whole container).
2. Synchronize to the nearest container boundary: `:",", closer, :eof`.
3. If a comma is found, continue parsing subsequent elements.
4. If a closer is found, finish the container with the elements accumulated so far.

Recovery MUST avoid consuming the container’s closing delimiter as “skipped junk” (except when the lexer already advanced to/through it as part of recovery).

Implementation note: V1 requires recovery hooks in container/string/call parsing paths (e.g., delimited element loops), not only at the top-level `expr_list` boundary.

Recovery operates “innermost first”: an error in a nested construct should be contained at the smallest recoverable scope, and the resulting error node becomes the surrounding element/argument. Multiple consecutive failing elements each get their own error node.

### 4.4 String-scoped recovery (required)

String-like constructs (strings, charlists, heredocs, sigils, quoted atoms/identifiers) need dedicated tolerant rules because errors can occur mid-literal.

V1 rules:

1. If the lexer emits `:error_token` while parsing a string-like construct, the parser SHOULD prefer producing a **partial literal** with an embedded `ErrNode` when the lexer provides enough part tokens to continue.
2. Otherwise, replace the entire literal with a single `ErrNode`.
3. If the lexer synthesizes a terminator (`*_end`, `:end_interpolation`, `:sigil_end`), the parser MUST treat it as a valid closing token for the construct and then continue parsing following tokens normally.
4. String-local errors MUST NOT also produce redundant parser-phase “unexpected EOF/missing terminator” diagnostics when the lexer already provided an equivalent lexer diagnostic.

**Example (conceptual):** if the lexer emits `:bin_string` fragments around an `:error_token`, the parser may build:

```elixir
{:<<>>, meta, ["prefix", {:__error__, err_meta, %{diag_id: 1, kind: :token, ...}}, "suffix"]}
```

If the lexer cannot provide recoverable fragments, replace the whole literal with a single `{:__error__, ..., ...}`.

### 4.5 Checkpoints and backtracking

ToxicParser uses checkpoint/rewind for speculative parses. Tolerant mode must preserve these invariants:

- Diagnostics collected during a speculative branch MUST be discarded if that branch is abandoned via rewind.
- Rewinding MUST restore `state.diagnostics` and any diagnostic ids/allocators to the checkpoint state.

### 4.6 Don’t “double-fix”

If the lexer already recovered:

- Do not try to re-synthesize terminators.
- Do not attempt to interpret the error payload and “correct” it.
- Do not re-tokenize the source region.
- Do not emit a second parser-phase diagnostic for a lexer-originated error unless the parser is now blocked at a new, distinct construct boundary.
- If the lexer sanitized an identifier (tolerant sanitization), accept the sanitized token and reuse the lexer diagnostic rather than re-validating and re-diagnosing.

Concrete “no duplicates” cases (V1):

- A `:error_token` in expression position becomes an `ErrNode`; it must not also trigger a generic “syntax error before …” parser diagnostic for the same token.
- A lexer “missing terminator” diagnostic paired with synthesized closers must not also produce a parser `:unexpected_eof` diagnostic for the same construct.
- A lexer unescape/bidi/error inside a string-like construct must not also produce an additional parser diagnostic that merely restates the lexer error.

The parser’s role is to:

- Place an error node at the right subnode location, and
- Continue using the token stream as-is.

## 5. Handling the Strict Error Corpus in Tolerant Mode

This section enumerates every case in `test/errors_strict_test.exs` and specifies how tolerant mode should behave.

Notation:

- **Diag**: a `%ToxicParser.Error{}` appended to `result.diagnostics`.
- **ErrNode**: `{:__error__, ..., payload}` (recommended payload includes `diag_id`)
- `ErrNode(...)` in this document is shorthand for an AST node `{:__error__, meta, payload_map}` anchored at an appropriate token/cursor location.
- Error nodes have the same tuple shape whether they appear as top-level expressions or as container elements/arguments.
- **Keep** means keep already-parsed valid children unchanged.
- **Sync** indicates the intended sync boundary (expression or container level).

### 5.1 Parser-originated errors (grammar/Pratt)

1. **literal encoder error** (`"1"` with failing `literal_encoder`)
   - Diag: `phase: :parser`, range = literal token range, reason = encoder failure
   - AST: keep the literal value (so tooling can continue), but attach error metadata to that literal’s containing node **or** replace literal with `ErrNode(kind: :invalid, original: :literal_encoder_error)`.
   - Sync: none (local).

2. **range step operator must follow range** (`"foo++bar//bat"`)
   - Diag at `//` token.
   - AST: `ErrNode(kind: :unexpected, children: [Keep(foo ++ bar), Keep(bat)])` (preserve the parsed left + step as children; do not invent `..`).
   - Sync: `expr_sync`.

3. **atom cannot be followed by alias** (`":foo.Bar"`)
   - Diag at `.` token.
   - AST: `ErrNode(kind: :invalid, children: [Keep(:foo), Keep(__aliases__(Bar))])`.
   - Sync: `expr_sync`.

4. **fn without -> clauses** (`"fn 1 end"`)
   - Diag anchored at `fn`.
   - AST option A (preferred): build an `{:fn, ..., clauses}` node with one **synthetic** clause:
     - `{:->, meta(toxic.synthetic), [[], [Keep(1)]]}`
     - attach diag to the `fn` node (node-meta anchor) or emit an `ErrNode` as the only clause body.
   - Sync: consume through matching `end` (using lexer-synthesized `end` if needed).

5. **unexpected -> placement** (`fn 1\n2 -> 3 end`)
   - Diag anchored at the first `->` seen (or at `fn` if clause classification fails).
   - AST: prefer an `ErrNode(kind: :ambiguous, children: [Keep(fn ... end as token span)])` inside an enclosing `__block__`, rather than guessing clause boundaries.
   - Sync: to `end` (fn terminator), then `expr_sync`.

6. **keyword list inside tuple** (`"{foo: :bar}"`)
   - Diag anchored at `:` of `foo:`.
   - AST: tuple with a single error element:
     - `{:{}, tuple_meta, [ErrNode(children: [Keep(foo), Keep(:bar)])]}`
   - Sync: `container_sync` to `}`.

7. **keyword list inside bitstring** (`"<<foo: :bar, baz: :bar>>"`)
   - Diag anchored at first `:` token that is interpreted as keyword.
   - AST: bitstring segments list includes `ErrNode` for the invalid keyword-like segment, then continue parsing remaining segments if possible.
   - Sync: `container_sync` to `>>`.

8. **expression after keyword list in call** (`"call foo: 1, :bar"`)
   - Diag anchored at `:bar`.
   - AST: keep call node and its keyword args, append `ErrNode(kind: :unexpected, children: [Keep(:bar)])` as a trailing arg (do not reorder).
   - Sync: `call_args_sync`.

9. **expression after keyword list in list** (`"[foo: 1, :bar]"`)
   - Diag anchored at `:bar`.
   - AST: keep list entries; append `ErrNode(children: [Keep(:bar)])` as an extra element.
   - Sync: `container_sync`.

10. **expression after keyword list in map** (`"%{foo: 1, :bar => :bar}"`)
   - Diag anchored at `:bar =>`.
   - AST: keep map entries; insert `ErrNode` either as a synthetic “entry” node or attach to map node meta while still including the parsed `:bar => :bar` as a normal entry.
   - Sync: `container_sync` to `}`.

11. **unexpected parentheses due to space** (`"foo (hello, world)"`)
   - Diag anchored at `(`.
   - AST: `ErrNode(kind: :unexpected, children: [Keep(foo), Keep(args=[hello, world])])` as a single expression (do not split into two expressions).
   - Sync: `expr_sync`.

12. **unexpected comma in nested calls** (`"foo 1, foo 2, 3"`)
   - Diag anchored at the first ambiguous comma.
   - AST: `ErrNode(kind: :ambiguous, children: [Keep(tokenspan for “foo 1, foo 2, 3”)])` or keep the outer call chosen by the current parser strategy but mark it with the diag.
   - Sync: `expr_sync`.

13. **unexpected comma inside containers** (`"[foo 1, 2]"`)
   - Diag anchored at comma.
   - AST: interpret as two list elements `[Keep(foo(1)), Keep(2)]` but attach diag to the list node meta (because the parse choice is a recovery decision).
   - Sync: `container_sync`.

14. **too many arguments in access syntax** (`"foo[1, 2]"`)
   - Diag anchored at comma.
   - AST: `ErrNode(kind: :invalid, children: [Keep(foo), Keep(indices=[1, 2])])` rather than inventing nested accesses.
   - Sync: `expr_sync` (or `container_sync` to `]` during access parse).

15. **invalid keyword identifier with do** (`"if true do:\\n"`)
   - Diag anchored at `do:`.
   - AST: keep `if true` as a call/head and append `ErrNode(kind: :missing, original: :missing_kw_value)` as the `do:` value.
   - Sync: `expr_sync`.

16. **invalid keyword identifier** (`"if true else: 1"`)
   - Diag anchored at `else:`.
   - AST: keep `if true` as a call/head; attach `ErrNode` for the invalid keyword entry `else: 1` as a trailing argument (do not drop `1`).
   - Sync: `expr_sync`.

17. **unicode conversion error in list string** (`"'\\xFF'"`)
   - Diag anchored at the offending escape span.
   - AST: `ErrNode(kind: :invalid, original: :unicode_conversion_error)` in place of the charlist literal.
   - Sync: `expr_sync`.

### 5.2 Lexer-originated errors (delivered as `:error_token`)

For all items below, the lexer emits `:error_token` with a precise meta span and then recovers to a sync point. The parser must:

- Convert the token into an `ErrNode(kind: :token, original: payload)` at the appropriate AST location, **or**
- If the error occurs in trivia-only positions (e.g., malformed comment when comments are not preserved), attach the diagnostic to `:root` and omit the node from the AST.

18. **unexpected closer** (`")"`)
   - AST: `ErrNode` as a standalone expression; ignore any lexer-synthesized structural artifacts that follow if they form an empty, fully-synthesized pair.

19. **mismatched closer** (`"([)"`)
   - AST: keep the container structure built from the stream; insert `ErrNode` at the mismatch site; accept lexer-synthesized expected closer and the actual closer token ordering without additional repair.

20. **missing closer at eof** (`"("`)
   - AST: build a parenthesized construct if possible; otherwise `ErrNode`. Accept lexer-synthesized closer token (`")"`) with zero-length meta.

21. **unexpected end keyword** (`"end"`)
   - AST: `ErrNode` as a standalone expression.

22. **unexpected end in expression** (`"1 end"`)
   - AST: `__block__` with `Keep(1)` then `ErrNode`.

23. **map invalid open delimiter** (`"%( )"`)
   - AST: `ErrNode` anchored at the `%(` span; any following tokens are parsed normally after lexer recovery.

24. **map unexpected space after percent** (`"% {}"`)
   - AST: `ErrNode` anchored at `%`/space span; then keep `{}` as a normal map/tuple/list if it can be parsed.

25. **keyword missing space after colon** (`"[foo:bar]"`)
   - AST: list with one `ErrNode` element representing the malformed keyword tokenization.

26. **string missing terminator** (`"\"unclosed"`)
   - AST: `ErrNode` replacing the string literal; the lexer may synthesize the closer.

27. **interpolation missing terminator** (`"\"foo #{bar"`)
   - AST: either (A) a string AST that contains `ErrNode` in place of the interpolation tail, or (B) `ErrNode` for the whole string if string parsing cannot proceed.

28. **interpolation missing terminator without metadata** (same input, `token_metadata: false`)
   - AST: same as (27).
   - Diag range: if lexer meta is absent, use cursor anchor with zero-length range and `details.synthetic? = true`.

29. **number trailing garbage** (`"0x"`)
   - AST: `ErrNode` in place of number literal.

30. **invalid float number** (`"1.0e309"`)
   - AST: `ErrNode` in place of number literal.

31. **consecutive semicolons** (`";;"`)
   - AST: prefer `{:__block__, [], []}` (no expression-level error node), with a lexer diagnostic anchored at `:root`. This avoids inventing an expression where none exists.

32. **alias unexpected paren** (`"Foo()"`)
   - AST: `ErrNode` representing the invalid call; keep `Foo` alias as child if lexer still emits it.

33. **alias invalid character** (`"Foöbar"`)
   - AST: `ErrNode`; if lexer sanitizes and still emits an alias token, keep the sanitized alias node but attach diag to it.

34. **heredoc missing terminator**
   - AST: `ErrNode` or partial heredoc node with attached diag, depending on whether the lexer emits synthesized terminator tokens.

35. **heredoc invalid header** (`"\"\"\"invalid"`)
   - AST: `ErrNode` anchored at heredoc header.

36. **sigil invalid lowercase name** (`"~ab()"`)
   - AST: `ErrNode` anchored at sigil name.

37. **sigil invalid mixed case name** (`"~Ab()"`)
   - AST: `ErrNode`.

38. **sigil invalid delimiter** (`"~s$foo$"`)
   - AST: `ErrNode` anchored at delimiter.

39. **identifier mixed script** (`"fooАbar"`)
   - AST: `ErrNode`; if lexer sanitizes and emits an identifier, keep it but attach diag.

40. **identifier invalid char** (`"foo@bar"`)
   - AST: `ErrNode` (same sanitization rule as 39).

41. **nonexistent atom with existing_atoms_only**
   - AST: `ErrNode` in place of atom literal (do not create the atom).

42. **comment invalid bidi**
   - AST: if comments are not preserved, anchor diag at `:root` and do not add an expression-level `ErrNode`.

43. **comment invalid linebreak**
   - AST: same as 42.

44. **version control merge conflict marker** (`"<<<<<<< HEAD"`)
   - AST: `ErrNode` as a standalone expression (or root-anchored if treated as trivia by lexer configuration).

45. **interpolation not allowed in quoted identifier** (`Foo."bar#{baz}"`)
   - AST: `ErrNode` in place of the quoted identifier member; keep `Foo` as left child.

46. **string invalid hex escape** (`"\"\\x\""`)
   - AST: `ErrNode` in place of string literal.

47. **string invalid unicode escape** (`"\"\\u\""`)
   - AST: `ErrNode`.

48. **string invalid unicode codepoint** (`"\"\\u{FFFFFF}\""`)
   - AST: `ErrNode`.

49. **charlist invalid hex escape** (`"'\\x'"`)
   - AST: `ErrNode`.

50. **charlist invalid unicode escape** (`"'\\u'"`)
   - AST: `ErrNode`.

51. **charlist invalid unicode codepoint** (`"'\\u{FFFFFF}'"`)
   - AST: `ErrNode`.

52. **string heredoc invalid hex escape**
   - AST: `ErrNode` or partial heredoc with embedded `ErrNode` for the escape span.

53. **string heredoc invalid unicode escape**
   - AST: same as 52.

54. **charlist heredoc invalid hex escape**
   - AST: same as 52.

55. **charlist heredoc invalid unicode escape**
   - AST: same as 52.

56. **quoted atom invalid hex escape** (`:"\\x"`)
   - AST: `ErrNode` in place of atom literal (do not create an atom).

57. **quoted atom invalid unicode escape** (`:"\\u"`)
   - AST: `ErrNode`.

58. **quoted keyword invalid hex escape** (`"\"\\x\": 1"`)
   - AST: treat as keyword list with an `ErrNode` key and `Keep(1)` value.

59. **quoted keyword invalid unicode escape** (`"\"\\u\": 1"`)
   - AST: same as 58.

60. **quoted call invalid hex escape** (`"Foo.\"\\x\""`) (currently a crash in strict mode)
   - AST (tolerant): must not crash; keep `Foo` as a valid left operand and represent the member as `ErrNode` inside the dot/call structure (so tooling can still “see” `Foo`).

61. **quoted call invalid unicode escape** (`"Foo.\"\\u\""`)
   - AST: same as 60.

62. **quoted call invalid bidi character**
   - AST: `ErrNode` for member; keep `Foo`.

63. **sigil lowercase invalid delimiter hex escape** (`"~s$\\x$"`)
   - AST: `ErrNode`.

64. **sigil uppercase invalid delimiter hex escape** (`"~S$\\x$"`)
   - AST: `ErrNode`.

65. **sigil lowercase invalid delimiter unicode escape** (`"~s$\\u$"`)
   - AST: `ErrNode`.

66. **sigil uppercase invalid delimiter unicode escape** (`"~S$\\u$"`)
   - AST: `ErrNode`.

67. **string invalid bidi character**
   - AST: `ErrNode`.

68. **charlist invalid bidi character**
   - AST: `ErrNode`.

69. **quoted atom invalid bidi character**
   - AST: `ErrNode`.

70. **quoted keyword invalid bidi character**
   - AST: keyword entry with `ErrNode` key.

71. **string heredoc invalid bidi character**
   - AST: `ErrNode` or partial heredoc with embedded `ErrNode`.

72. **charlist heredoc invalid bidi character**
   - AST: same as 71.

73. **sigil lowercase invalid bidi character**
   - AST: `ErrNode`.

74. **sigil uppercase invalid bidi character**
   - AST: `ErrNode`.

75. **sigil lowercase heredoc invalid bidi character**
   - AST: `ErrNode` or partial sigil/heredoc with embedded `ErrNode`.

76. **sigil uppercase heredoc invalid bidi character**
   - AST: same as 75.

77. **sigil lowercase invalid bidi delimiter**
   - AST: `ErrNode` anchored at delimiter span.

78. **sigil uppercase invalid bidi delimiter**
   - AST: `ErrNode`.

## 6. Invariants Checklist (V1)

Tolerant mode V1 is considered correct if:

- It never crashes on inputs from `test/errors_strict_test.exs`.
- It returns a non-nil `result.ast` in tolerant mode.
- Every diagnostic has a bounded range (or is explicitly marked synthetic) and is anchored to a specific AST subnode (error node, node meta, or root).
- Lexer error tokens are treated as authoritative and are not “re-fixed” by parser logic.
- Any parser-synthesized placeholder nodes are clearly marked synthetic and do not claim real source ranges.
- `details.id` values are unique within a single parse result, and can be used to correlate diagnostics with error nodes/events.
- Recovery inside containers preserves valid sibling elements and does not shift their metadata.
- Checkpoint/rewind does not leak diagnostics from abandoned branches.
