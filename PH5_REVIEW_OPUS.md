# Phase 5 Implementation Review: Calls & Identifiers

**Reviewer:** Claude Opus 4.5
**Date:** 2025-12-10
**Status:** Early skeleton stage (~15% complete). Basic structure exists but no actual call parsing logic.

---

## Summary

Phase 5 requires parsing function calls (paren and no-parens variants), identifier classification with dot variants, keyword handling, and ambiguity resolution using `peek_n`. The current implementation has minimal scaffolding: identifier classification for basic shapes and a call parser that treats all identifiers as bare literals.

**Note:** Phase 4 gaps have been addressed - `expr_list` now properly handles EOE-separated expressions.

---

## Phase 4 Fixes Observed ✅

| Issue from PH4_REVIEW | Status | Notes |
|-----------------------|--------|-------|
| `expr_list` multi-expression | ✅ Fixed | Lines 20-33 in `expressions.ex` |
| EOE skip before first expr | ✅ Fixed | `skip_eoe/2` helper |
| `__block__` wrapping | ✅ Fixed | `finalize_exprs/3` |
| Warning struct | ✅ Added | `Warning` module exists |
| Warnings in State | ✅ Added | `state.warnings` field |
| EventLog in State | ✅ Added | `state.event_log` field |

**Phase 4 can now be considered substantially complete** (minus `block_expr` which is deferred).

---

## Phase 5 Requirements (from HL_PLAN.md)

| Requirement | Status | Notes |
|-------------|--------|-------|
| Identifier classification (basic shapes) | ✅ Done | `Identifiers.classify/1` |
| Dot variant classification | 🔴 Missing | No `dot_identifier`, `dot_paren_identifier`, etc. |
| Quoted identifier rewrites | 🔴 Missing | No `quoted_*_identifier_end` handling |
| Paren calls `foo(args)` | 🔴 Missing | No `(` consumption or arg parsing |
| Nested paren calls `foo(bar())` | 🔴 Missing | No recursive call handling |
| No-parens one `foo arg` | 🔴 Missing | No single-arg no-parens parsing |
| No-parens many `foo a, b` | 🔴 Missing | No multi-arg no-parens parsing |
| No-parens ambig | 🔴 Missing | No ambiguity detection/resolution |
| Do-block carriers | 🔴 Missing | No do-block attachment |
| Keyword-last enforcement | 🔴 Missing | No keyword position validation |
| `do:` vs `do/end` disambiguation | 🔴 Missing | No `maybe_bad_keyword_*` logic |
| `peek_n` for access vs list | 🔴 Missing | No lookahead disambiguation |
| Event/builder attachment | 🟡 Partial | `Builder.Helpers.call/3` exists but unused |

---

## What's Implemented

### Identifiers Module (`identifiers.ex`)

```elixir
def classify(kind) do
  case kind do
    :identifier -> :identifier
    :paren_identifier -> :paren_identifier
    :bracket_identifier -> :bracket_identifier
    :do_identifier -> :do_identifier
    :op_identifier -> :op_identifier
    _ -> :other
  end
end
```

**Good:** Basic 5-way classification exists.

**Missing:**
- Dot variants: `dot_identifier`, `dot_paren_identifier`, `dot_bracket_identifier`, `dot_do_identifier`, `dot_op_identifier`, `dot_call_identifier`
- Quoted forms: `quoted_paren_identifier_end`, `quoted_bracket_identifier_end`, `quoted_do_identifier_end`, `quoted_op_identifier_end`

### Calls Module (`grammar/calls.ex`)

```elixir
def parse(%State{} = state, ctx, %EventLog{} = log) do
  case TokenAdapter.peek(state) do
    {:ok, tok, _} ->
      case Identifiers.classify(tok.kind) do
        :other ->
          Pratt.parse(state, ctx, log)
        _ident_kind ->
          {:ok, _tok, state} = TokenAdapter.next(state)
          ast = Builder.Helpers.literal(tok.value)  # Just returns atom!
          {:ok, ast, state, log}
      end
    # ...
  end
end
```

**Problem:** All identifiers become bare atoms. No check for:
- Following `(` → paren call
- Following args without parens → no-parens call
- Following `do` keyword → do-block carrier
- Following `[` → bracket access (vs bracket call for `bracket_identifier`)

### Infrastructure Ready ✅

| Component | Status | Location |
|-----------|--------|----------|
| `Builder.Helpers.call/3` | ✅ Ready | `builder/helpers.ex:38-41` |
| `Warning` struct | ✅ Ready | `warning.ex` |
| `State.warnings` | ✅ Ready | `state.ex:22,38` |
| `TokenAdapter.peek_n/2` | ✅ Ready | `token_adapter.ex` (bounded by `max_peek`) |
| `EventLog` `:call` node kind | ✅ Ready | `event_log.ex:49` |

---

## Critical Gaps for Phase 5

### 1. Paren Call Parsing 🔴

**Required behavior:**
```elixir
# Input: foo(1, 2)
# Should produce: {:foo, [], [1, 2]}
```

**Needed implementation sketch:**
```elixir
defp parse_identifier(tok, state, ctx, log) do
  case TokenAdapter.peek(state) do
    {:ok, %{kind: :"("}, _} ->
      parse_paren_call(tok, state, ctx, log)
    {:ok, %{kind: :do}, _} when tok.kind == :do_identifier ->
      parse_do_call(tok, state, ctx, log)
    # ... other cases
    _ ->
      # bare identifier
      {:ok, Builder.Helpers.literal(tok.value), state, log}
  end
end

defp parse_paren_call(callee_tok, state, ctx, log) do
  {:ok, _open, state} = TokenAdapter.next(state)  # consume (
  {:ok, args, state, log} = parse_call_args(state, ctx, log)
  {:ok, _close, state} = expect(state, :")"))
  ast = Builder.Helpers.call(callee_tok.value, args)
  {:ok, ast, state, log}
end
```

### 2. No-Parens Call Variants 🔴

**Three distinct cases from `elixir_parser.yrl`:**

1. **`no_parens_one_expr`** - Single unambiguous argument:
   ```elixir
   # foo bar   → {:foo, [], [:bar]}
   # foo 1     → {:foo, [], [1]}
   ```

2. **`no_parens_many_expr`** - Multiple arguments:
   ```elixir
   # foo 1, 2  → {:foo, [], [1, 2]}
   ```
   Subject to arity restrictions on nesting.

3. **`no_parens_one_ambig_expr`** - Ambiguous single argument:
   ```elixir
   # foo bar baz   → Is this foo(bar(baz)) or foo(bar, baz)?
   ```
   Resolved by outer-arity-1 rule.

**Needed:** Argument collection with comma separation, arity tracking, nesting ban enforcement.

### 3. Dot Variant Classification 🔴

**Required forms** (from Toxic token kinds):
```elixir
def classify(kind) do
  case kind do
    # Basic forms
    :identifier -> :identifier
    :paren_identifier -> :paren_identifier
    # ... existing ...

    # Dot forms (MISSING)
    :dot_identifier -> :dot_identifier
    :dot_paren_identifier -> :dot_paren_identifier
    :dot_bracket_identifier -> :dot_bracket_identifier
    :dot_do_identifier -> :dot_do_identifier
    :dot_op_identifier -> :dot_op_identifier
    :dot_call_identifier -> :dot_call_identifier

    _ -> :other
  end
end
```

### 4. `peek_n` Disambiguation 🔴

**Required cases from HLD:**

1. **Access vs list start:**
   ```elixir
   foo[1]   # access - bracket_identifier followed by `[`
   foo [1]  # call with list arg - identifier followed by space then `[`
   ```

2. **No-parens ambiguity:**
   ```elixir
   foo bar baz    # Need to look ahead to resolve
   foo bar, baz   # Comma makes it unambiguous
   ```

**Implementation pattern:**
```elixir
defp disambiguate_bracket(tok, state) do
  case TokenAdapter.peek_n(state, 2) do
    {:ok, [%{kind: :"["}, _], _} when tok.kind == :bracket_identifier ->
      :access
    {:ok, [%{kind: :"["}, _], _} ->
      :call_with_list_arg
    _ ->
      :bare_identifier
  end
end
```

### 5. Keyword-Last Enforcement 🔴

**Rule:** Keyword arguments must appear at the end of argument lists.

```elixir
# Valid
foo(1, 2, key: val)

# Invalid - should warn/error
foo(key: val, 1, 2)
```

**Needed:** Argument list parser that tracks keyword vs positional and validates order.

### 6. `do:` vs `do/end` Disambiguation 🔴

**From HLD:** "spell out keyword-last rules and `do:` vs `do/end` attachment (`maybe_bad_keyword_*` warnings)"

```elixir
# These are different:
if cond, do: expr              # keyword syntax
if cond do expr end            # block syntax

# Ambiguous/problematic:
if cond, do: expr, else: expr  # Should this warn?
```

**Needed:** Warning emission when `do:` keyword appears in contexts expecting `do/end`.

### 7. Do-Block Carriers 🔴

**From `block_expr` in `elixir_parser.yrl`:**
```
block_expr -> dot_do_identifier do_block
block_expr -> dot_call_identifier call_args_parens do_block
block_expr -> dot_identifier call_args_no_parens_all do_block
```

**Depends on:** Do-block parsing (Phase 7), but the carrier detection belongs to Phase 5.

---

## Test Coverage

### Existing Tests

| Test | What it covers |
|------|----------------|
| `calls parser falls back to Pratt` | Single identifier → atom |
| `builds call nodes` | `Builder.Helpers.call/3` works |

### Missing Tests for Phase 5

1. **Paren calls:** `foo(1)`, `foo(1, 2)`, `foo()`
2. **Nested paren calls:** `foo(bar(1))`
3. **No-parens one:** `foo bar`
4. **No-parens many:** `foo 1, 2, 3`
5. **No-parens ambiguous:** `foo bar baz`
6. **Dot calls:** `Foo.bar(1)`, `foo.bar`
7. **Bracket identifier:** `foo[key]`
8. **Do-block carrier:** `foo do ... end`
9. **Keyword-last valid:** `foo(1, key: val)`
10. **Keyword-last invalid:** `foo(key: val, 1)` (should warn)
11. **`do:` vs `do/end`:** warning cases

---

## Dependencies & Blockers

### Depends On (Available)

| Dependency | Status | Notes |
|------------|--------|-------|
| Container parsing (for args) | 🔴 Phase 6 | Need list/tuple parsing for call args |
| Keyword list parsing | 🔴 Phase 6 | For keyword arguments |
| Do-block parsing | 🔴 Phase 7 | For do-block carriers |

### Circular Dependency Issue

Phase 5 (Calls) needs:
- Argument lists → Container parsing (Phase 6)
- Do-blocks → Block parsing (Phase 7)

But containers and blocks may contain calls.

**Resolution:** Implement call argument parsing as a separate concern that can be stubbed initially, then filled in when containers land.

---

## File Structure Assessment

**Exists:**
- `lib/toxic_parser/grammar/calls.ex` ✅
- `lib/toxic_parser/identifiers.ex` ✅

**Missing:**
- `lib/toxic_parser/grammar/args.ex` - Call argument parsing
- `lib/toxic_parser/grammar/keywords.ex` - Keyword list handling

---

## Checklist for Phase 5 Completion

### Must Have (Core Phase 5)

- [ ] Dot variant classification in `Identifiers.classify/1`
- [ ] Paren call detection: check for `(` after identifier
- [ ] Paren call parsing: consume `(`, args, `)`
- [ ] Basic argument list parsing (comma-separated expressions)
- [ ] `Builder.Helpers.call/3` usage in call parsing
- [ ] Bare identifier fallback (current behavior, but after checks)
- [ ] Basic test suite for paren calls

### Should Have (Full Phase 5)

- [ ] No-parens one detection and parsing
- [ ] No-parens many detection and parsing
- [ ] `peek_n` for access vs list disambiguation
- [ ] Keyword-last position validation
- [ ] `warn_pipe` emission for pipe-into-call ambiguity
- [ ] `warn_no_parens_after_do_op` emission
- [ ] Quoted identifier rewrite handling

### Deferred (Phase 6/7 Dependencies)

- [ ] Full keyword list parsing → Phase 6
- [ ] Do-block carriers → Phase 7
- [ ] `do:` vs `do/end` disambiguation → Phase 7
- [ ] `maybe_bad_keyword_*` parity → Phase 7
- [ ] No-parens ambig resolution → needs containers

---

## Recommendations

### Immediate Actions

1. **Add dot variants to `Identifiers.classify/1`**
   - Low effort, high value for Phase 5 foundation

2. **Implement paren call detection and basic parsing**
   - Check for `(` after identifier
   - Stub argument parsing as expression list (will refine with Phase 6)
   - Use `Builder.Helpers.call/3`

3. **Add paren call tests**
   - `foo()` → `{:foo, [], []}`
   - `foo(1)` → `{:foo, [], [1]}`
   - `foo(1, 2)` → `{:foo, [], [1, 2]}`

### Phased Implementation Strategy

Given the circular dependencies, suggest this order:

1. **Phase 5a:** Paren calls with simple expression args (reuse Pratt for args)
2. **Phase 6:** Containers (lists, tuples, maps)
3. **Phase 5b:** No-parens calls using container infrastructure
4. **Phase 7:** Do-blocks
5. **Phase 5c:** Do-block carriers and `do:` disambiguation

---

## Phase Status Summary

| Component | Status | Blocking Issues |
|-----------|--------|-----------------|
| Identifier classification (basic) | ✅ Done | None |
| Identifier classification (dot) | 🔴 Missing | Easy to add |
| Quoted identifier rewrites | 🔴 Missing | Medium effort |
| Paren call detection | 🔴 Missing | Core requirement |
| Paren call parsing | 🔴 Missing | Core requirement |
| Arg list parsing | 🔴 Missing | Needs containers |
| No-parens one | 🔴 Missing | Needs arg parsing |
| No-parens many | 🔴 Missing | Needs arg parsing |
| `peek_n` disambiguation | 🔴 Missing | Infrastructure ready |
| Keyword-last | 🔴 Missing | Needs keyword parsing |
| Do-block carriers | 🔴 Missing | Needs Phase 7 |
| Warnings | 🔴 Missing | Infrastructure ready |
| Tests | 🔴 Minimal | One basic test exists |

**Overall Phase 5: ~15% Complete**

**Recommendation:** Focus on paren call parsing first. This unblocks most other call variants. Consider parallel work on containers (Phase 6) since call args need them.
