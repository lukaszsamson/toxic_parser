# Peek analysis (peek_n) — ToxicParser vs original `elixir_parser.yrl`

## Executive summary

- The upstream Elixir grammar (`elixir_parser.yrl`) is LALR(1) **at the lexer-token level**, and a lot of the “looks like it needs 2+ tokens” ambiguity is resolved by **tokenization choices** (special token kinds) plus **precedence**.
- In this codebase, the Pratt/recursive-descent layers generally only need **one-token lookahead** (`TokenAdapter.peek/1`) because:
  - end-of-expression is normalized into a single `:eoe` token,
  - dot vs dot-call is already distinct (`:dot_op` vs `:dot_call_op`),
  - identifiers are pre-classified (`:paren_identifier`, `:bracket_identifier`, `:do_identifier`, `:op_identifier`, etc.),
  - multi-lexeme operators like `not in` are represented as a single token shape (see `Precedence.not_in/0` + Pratt rewrite).

So: **there are no “must have LL(2)” grammar rules** here. `peek_n/2` is mostly a **readability/tooling** feature for a few localized “check token A then token B” decisions.

## Why LALR(1) doesn’t imply `peek_n` is needed here

It’s useful to distinguish:

1. **Parser lookahead** (LALR(1) means one token of lookahead in the LR automaton)
2. **Lexer power** (the lexer can look arbitrarily far and can emit tokens that encode context)

`elixir_parser.yrl` leans heavily on (2). Examples visible in the grammar:

- Dot call vs dot member is separated by token kinds (`dot_call_op` vs `dot_op`) and by specialized identifier terminals (`paren_identifier`, `bracket_identifier`, `do_identifier`, etc.). This avoids needing the parser to look ahead multiple terminals to decide whether `.` is part of `foo.(...)` or `foo.bar`.
- “`not in`” is treated as a special case via token payloads/locations (`in_op` can carry `not in`), which is then rewritten in the grammar’s Erlang helper code. In this repo, that rewrite is mirrored in `ToxicParser.Pratt`.
- Many spots that look like they’d need peeking past newlines are handled by newline-sensitive tokenization + `:eoe` normalization.

Because ToxicParser uses a tokenizer that similarly pre-classifies tokens, the *parser* rarely needs `peek_n`.

## Where `peek_n/2` *could* simplify implementation

### 1) “two-token pattern checks” (good fit)

These are cases where the code currently does:

- `peek` to decide whether to consume,
- then `next` to consume,
- then another `peek` to decide what follows.

`peek_n/2` can compress those into a single decision without consuming anything.

#### Candidate A — inner-parens stab detection in `Containers`

File: `lib/toxic_parser/grammar/containers.ex`

The `try_parse_stab_parens_many/5` path detects the shape:

- `(() -> ...)`
- `(() when ... -> ...)`

Today it:

1. checkpoints,
2. consumes the inner `(`,
3. skips EOE,
4. peeks for `)`, consumes it,
5. then peeks for `:stab_op` or `:when_op`.

That’s essentially a “peek 2 tokens” decision after `skip_eoe`.

A `peek_n(inner_state, 2)` check could express this directly:

```elixir
case TokenAdapter.peek_n(inner_state, 2) do
  {:ok, [%{kind: :")"} = close, %{kind: :stab_op}], inner_state} ->
    # (() -> ...)

  {:ok, [%{kind: :")"} = close, %{kind: :when_op}], inner_state} ->
    # (() when ... -> ...)

  _ ->
    # not a stab_parens_many
end
```

This doesn’t change correctness, but it *does* make the intent (“pattern match on the next two tokens”) explicit.

#### Candidate B — “comma then terminator” in keyword lists

File: `lib/toxic_parser/grammar/keywords.ex`

The code currently consumes `,` and then peeks again to decide whether it’s a trailing comma:

```elixir
{:ok, %{kind: :","}, _} ->
  {:ok, _comma, state} = TokenAdapter.next(state)
  case TokenAdapter.peek(state) do
    {:ok, %{kind: kind}, _} when kind in terminators -> stop
    _ -> continue
  end
```

`peek_n(state, 2)` can encode “`,)` / `,]` / `,}` / `,>>` / `,eoe`” in one place and potentially reduce repeated peek calls.

This is again a readability improvement; it doesn’t unlock new syntax.

#### Candidate C — bracket-arg trailing comma check

File: `lib/toxic_parser/grammar/calls.ex` (`parse_bracket_arg_no_skip/3`)

After parsing an expression inside `[...]`, it checks for `,` and then checks if the next token is `]`.

`peek_n(state, 2)` could express the “`, ]`” case as a single match.

### 2) “multi-token but variable-length” lookahead (not a good fit)

Some decisions are conceptually “look ahead until …”, but not a fixed N. `peek_n/2` doesn’t help much.

#### Quoted keyword keys: `"foo":` / `'bar':`

File: `lib/toxic_parser/grammar/keywords.ex`

To decide whether a string literal is actually a keyword key, the parser must look beyond:

- string start,
- 0..N `:string_fragment` tokens,
- string end,
- then check for `:` semantics (encoded by `Strings.parse/4` returning `{:keyword_key, ...}` tuples).

That is inherently variable length, so `peek_n` is not sufficient. The current approach (delegate to `Strings.parse`) is the right abstraction.

### 3) Places where “more than one token” isn’t the real problem

Several “hard” ambiguities depend on **structure/precedence**, not on a small fixed window of terminals. In these cases, the code correctly uses:

- `checkpoint/rewind`, or
- a dedicated parse with a binding-power cutoff (`parse_with_min_bp`).

#### Map update vs entry: `%{base | ...}`

File: `lib/toxic_parser/grammar/maps.ex` (`try_parse_map_update/3`)

Whether `|` starts a map update or is part of an expression key can depend on the parsed AST (e.g., “key contains lower-precedence ops”), not the next 2 tokens.

`peek_n` can’t replace that.

#### Stab clauses and guards inside blocks

File: `lib/toxic_parser/grammar/blocks.ex` and `containers.ex`

Detecting whether something is a stab clause involves parsing patterns/guards and seeing if `->` is present in the right place. This is not a fixed lookahead window, so `checkpoint + attempt parse` is appropriate.

## “Do we need peek_n for any LALR(1) rule?”

At the level of terminals in `elixir_parser.yrl`: no.

Where the grammar seems to “want” extra lookahead, Elixir solves it by:

- emitting different tokens for whitespace-sensitive cases,
- pre-classifying identifiers,
- using precedence to keep conflicts manageable,
- and a small amount of AST rewriting (e.g. `not in`).

ToxicParser’s approach mirrors this, so any need for `peek_n` is for **clarity and local refactoring**, not for language coverage.

## Recommendations

1. Keep `peek_n/2` as a utility for *local token-sequence tests*.
2. Prefer `checkpoint/rewind` when the decision boundary is “try parse X, if it doesn’t fit, fall back”.
3. Consider using `peek_n` in:
   - `lib/toxic_parser/grammar/containers.ex` (inner-parens stab detection)
   - `lib/toxic_parser/grammar/keywords.ex` (trailing comma handling)
   - `lib/toxic_parser/grammar/calls.ex` (bracket arg trailing comma / keyword merge checks)

If you want, I can follow up with a small PR that refactors 1–2 of the above spots to use `peek_n/2` (no behavior change), to validate whether it improves readability in practice.

## Would a `peek_until(predicate)` help for variable-length cases?

### Short answer

- It can help **reduce boilerplate** for “scan forward until you see X” patterns.
- It usually does **not** replace `checkpoint/rewind` (it often *implements* it).
- It does **not** replace real parsing when the decision depends on structure (nesting, precedence, interpolation), not just the presence of a token.

### What `peek_until` really is in a streaming lexer

In a streaming setup (like Toxic + `TokenAdapter`), a pure lookahead API must not permanently advance the stream. That means any `peek_until/2` will be implemented as one of:

1. **Checkpoint + advance + rewind** (conceptually):
   - create checkpoint
   - repeatedly call `next` until predicate matches or a limit/terminator occurs
   - rewind

2. **Fill lookahead buffer** until predicate matches (conceptually):
   - repeatedly call `fetch_next(..., cache?: true)` until predicate matches or limit
   - then just return without rewinding because the tokens live in `state.lookahead`

In this repo, (2) is already how `peek_n/2` works; a `peek_until` would be a close cousin.

So the practical benefit is **ergonomics** and sometimes a small reduction in “peek/next/peek” churn—not new language coverage.

### When it *would* make a difference

These are *variable-length* patterns where you’re not sure how many tokens you must peek, but you still only need a **shallow** property (a token kind, or “first non-EOE token”):

- **Skip trivia until next significant token**:
  - Today you have helpers like `skip_eoe/1` and variants.
  - `peek_until(fn t -> t.kind != :eoe end)` could unify a lot of those.

- **Scan until a terminator to decide between two error messages / recovery strategies**:
  - Example: decide whether we’re “inside parens” by scanning for `:')'` before `:eoe`.
  - This is mostly for diagnostics/recovery, not for correct parse decisions.

- **Quoted identifier / quoted keyword key fast-paths** (maybe):
  - A high-level `peek_until` might help detect “this string finishes and is immediately followed by `:`” without building the full string AST.
  - However, see the next section: in practice this tends to leak complexity.

### When it would *not* help (and why parsing/rewind is still better)

#### A) Interpolated/escaped strings and sigils

The classic variable-length example here is the “is this a keyword key?” problem for strings.

Even if you can scan tokens until you see an end token, you still need to know:

- whether interpolation appeared (affects AST and metadata),
- whether escapes apply (affects key atom content),
- how delimiters work across heredocs/sigils,
- whether the lexer emitted an error token partway.

At that point, `peek_until` turns into a partial re-implementation of `Strings.parse/4`.

In other words: **if you must understand structure**, a purpose-built parser (like `Strings.parse`) is the right layer.

#### B) Nesting / balancing decisions

Any scan that needs to “find the next `)` that closes *this* `(`” is not just “until you see `)`”. It needs a nesting counter and awareness of constructs that can contain delimiters (strings, sigils, heredocs).

That’s parser territory, not a predicate.

#### C) Ambiguities that depend on precedence or AST shape

The map-update detection in `Maps.try_parse_map_update/3` is a good example: the decision isn’t “is the next token `|`?”, it’s “is that `|` acting as a map-update separator *given the parsed key/value structure*?”.

No lookahead predicate can replace that reliably.

### Is `peek_until` better than checkpoint/rewind?

It’s better as a **wrapper**, not as a replacement.

Checkpoint/rewind is a general mechanism that preserves correctness when:

- you need to attempt a parse and backtrack,
- you need to build AST to decide,
- you need to preserve accurate diagnostics.

`peek_until` is only “better” in the sense that it:

- can centralize a common pattern,
- can standardize limits/timeouts,
- can reduce repetition and the chance of forgetting to rewind.

### Important design constraints if you add it

If Toxic (or `TokenAdapter`) gains `peek_until`, it should be bounded and explicit about failure modes. Otherwise it becomes an accidental O(n) scan over the whole remainder of the file.

Recommended constraints:

- **Hard cap**: `max_peek` or `max_scan` (tokens), similar to `peek_n/2`.
- **Stop conditions**: stop at `:eoe`, `:end`, close delimiters, or `:eof` depending on the use-case.
- **Return value includes how far it looked**:
  - e.g. `{:ok, tokens, state}` or `{:not_found, tokens, state}` so callers can decide whether to fall back.
- **Implement at the `TokenAdapter` level**, not the raw lexer level, if you want:
  - `:eoe` normalization,
  - tolerant-mode synthetic error tokens,
  - consistent metadata (`terminators`, `newlines`).

### Practical conclusion

- A `peek_until(predicate)` would help for *trivia skipping* and *simple token-sequence scanning*.
- For the truly variable-length “string/sigil determines meaning” cases, it would not eliminate the need for specialized parsing (`Strings.parse`) and/or checkpointing.

If you can paste the relevant part of Toxic’s stream API (or open that file inside this workspace), I can tighten the analysis to the exact semantics of `Toxic.checkpoint/1`, `Toxic.rewind_to/2`, and any existing caching it already does.
