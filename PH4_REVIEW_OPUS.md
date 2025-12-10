# Phase 4 Implementation Review: Grammar Layer — Expressions

**Reviewer:** Claude Opus 4.5
**Date:** 2025-12-10
**Status:** Early skeleton stage (~15% complete). Foundation is sound but most Phase 4 requirements remain unimplemented.

---

## Summary

Phase 4 requires the grammar layer to dispatch expressions through matched/unmatched/no_parens contexts, handle EOE-separated expression lists, implement block_expr, and emit ambiguity warnings. The current implementation has minimal scaffolding that correctly delegates to Pratt, but lacks the actual grammar logic.

**Positive Finding:** Phase 3 gaps (binding powers, associativity, Pratt loop) have been addressed since PH3_REVIEW. The Pratt parser now has a working `nud`/`led` loop with proper associativity handling.

---

## Phase 3 Fixes Applied ✅

Before reviewing Phase 4, note these Phase 3 improvements:

| Issue from PH3_REVIEW | Status | Notes |
|-----------------------|--------|-------|
| Missing operators | ✅ Fixed | `do_op`, `stab_op`, `comma_op`, `assoc_op`, `match_op`, `unary_at_op` added |
| No associativity | ✅ Fixed | All operators now have `{bp, assoc}` tuples |
| No Pratt loop | ✅ Fixed | `nud`/`led` loop implemented in `pratt.ex:25-97` |
| Binding power alignment | ✅ Fixed | Values now match HLD (5-330 range) |

**Phase 3 can now be considered complete.**

---

## Phase 4 Requirements (from HL_PLAN.md)

| Requirement | Status | Notes |
|-------------|--------|-------|
| Top-level `grammar -> eoe? expr_list eoe?` | 🔴 Missing | No leading/trailing EOE handling |
| Centralized EOE handling | 🔴 Missing | EOE normalized in TokenAdapter but not consumed in grammar |
| `expr_list` with EOE separation | 🔴 Missing | Currently parses single expression only |
| `matched_expr` context | 🟡 Partial | Context passed to Pratt but no differentiated behavior |
| `unmatched_expr` context | 🟡 Partial | Same as above |
| `no_parens_expr` context | 🟡 Partial | Same as above |
| `expr` as RD dispatcher | ✅ Done | `Expressions.expr/3` dispatches to Pratt |
| `block_expr` variants | 🔴 Missing | None of the 5 variants implemented |
| `no_parens_one_expr` | 🔴 Missing | Not implemented |
| `no_parens_many_expr` | 🔴 Missing | Not implemented |
| `no_parens_one_ambig_expr` | 🔴 Missing | Not implemented |
| Arity-based nesting bans | 🔴 Missing | No nesting restriction logic |
| Ambiguity resolution (outer arity 1) | 🔴 Missing | No ambiguity handling |
| `warn_pipe` emission | 🔴 Missing | No warning infrastructure |
| `warn_no_parens_after_do_op` | 🔴 Missing | No warning infrastructure |

---

## What's Implemented

### Grammar Module (`grammar.ex`)

```elixir
def parse_string(source, opts \\ []) do
  state = TokenAdapter.new(source, opts)
  log = EventLog.new() |> EventLog.start_node(:root, zero_meta())

  with {:ok, ast, state, log} <- Expressions.expr_list(state, :matched, log) do
    log = EventLog.end_node(log, :root, zero_meta())
    {:ok, ast, state, log}
  end
end
```

**Good:**
- Creates parser state and event log
- Wraps parse in `:root` node events
- Passes context to expressions layer

**Missing:**
- No `eoe?` consumption before/after `expr_list`
- No file/source tracking
- Hardcoded `:matched` context (should be configurable)

### Expressions Module (`grammar/expressions.ex`)

```elixir
def expr_list(%State{} = state, ctx, %EventLog{} = log) do
  expr(state, ctx, log)  # Single expression only!
end

def expr(%State{} = state, ctx, %EventLog{} = log) do
  Pratt.parse(state, ctx, log)
end
```

**Good:**
- Clean dispatcher pattern
- Correct signature threading state/context/log

**Missing:**
- No loop for multiple expressions
- No EOE consumption between expressions
- No `__block__` wrapping for multiple expressions
- No early termination on sync tokens

### Pratt Module (`pratt.ex`)

The Pratt implementation has progressed significantly:

```elixir
def parse(%State{} = state, context, %EventLog{} = log) do
  with {:ok, token, state} <- TokenAdapter.next(state),
       {:ok, left, state, log} <- nud(token, state, context, log) do
    led(left, state, log, 0, context)
  else
    {:eof, state} -> {:error, :unexpected_eof, state, log}
    {:error, diag, state} -> {:error, diag, state, log}
  end
end
```

**Good:**
- Proper `nud` → `led` loop structure
- Associativity-aware binding power comparison
- Error propagation

**Limitations (acceptable for Phase 4):**
- `nud` only handles literals (returns `token.value`)
- `led` only handles binary operators (no dot, access, calls)
- No context-specific behavior differentiation
- No unary operator handling

---

## Critical Gaps for Phase 4

### 1. `expr_list` Implementation 🔴

**Required** (from `elixir_parser.yrl` lines 80-84):
```
grammar -> eoe? expr_list : ...
grammar -> eoe? expr_list eoe : ...
grammar -> eoe? : ...
grammar -> '$empty' : ...

expr_list -> expr : ...
expr_list -> expr_list eoe expr : ...
```

**Needed implementation:**
```elixir
def expr_list(state, ctx, log) do
  # Skip leading EOE
  state = skip_eoe(state)

  case expr(state, ctx, log) do
    {:ok, first, state, log} ->
      collect_exprs([first], state, ctx, log)
    {:error, _, _, _} = err ->
      err
  end
end

defp collect_exprs(acc, state, ctx, log) do
  case TokenAdapter.peek(state) do
    {:ok, %{kind: :eoe}, _} ->
      {:ok, _, state} = TokenAdapter.next(state)  # consume EOE
      case expr(state, ctx, log) do
        {:ok, next, state, log} -> collect_exprs([next | acc], state, ctx, log)
        {:error, _, _, _} -> finalize_exprs(acc, state, log)
      end
    _ ->
      finalize_exprs(acc, state, log)
  end
end

defp finalize_exprs([single], state, log), do: {:ok, single, state, log}
defp finalize_exprs(many, state, log) do
  {:ok, {:__block__, [], Enum.reverse(many)}, state, log}
end
```

### 2. `block_expr` Variants 🔴

**Required** (from `elixir_parser.yrl` lines 181-185):
```
block_expr -> dot_call_identifier call_args_parens do_block
block_expr -> dot_call_identifier call_args_parens call_args_parens do_block
block_expr -> dot_do_identifier do_block
block_expr -> dot_op_identifier call_args_no_parens_all do_block
block_expr -> dot_identifier call_args_no_parens_all do_block
```

This depends on:
- Identifier classification (Phase 5)
- Call args parsing (Phase 5)
- Do-block parsing (Phase 7)

**Recommendation:** Defer `block_expr` to Phase 5/7 overlap, but document dependency.

### 3. No-Parens Expression Variants 🔴

**Required** (from NONTERMINALS_GPT.md):
- `no_parens_zero_expr` - Recursive descent
- `no_parens_one_expr` - Recursive descent
- `no_parens_one_ambig_expr` - Recursive descent
- `no_parens_many_expr` - Recursive descent

These interact with call parsing (Phase 5), so partial implementation makes sense.

### 4. Warning Emission Infrastructure 🔴

**Required warnings:**
- `warn_pipe`: Pipe operator feeding into ambiguous no-parens call
- `warn_no_parens_after_do_op`: No-parens after do-capable operator

**Needed infrastructure:**
```elixir
defmodule ToxicParser.Warning do
  @type t :: %__MODULE__{
    code: atom(),
    message: String.t(),
    range: EventLog.range(),
    details: map()
  }

  defstruct [:code, :message, :range, details: %{}]
end
```

Add warnings list to `State` struct alongside diagnostics.

### 5. Context-Differentiated Parsing 🟡

Currently, `context` is passed through but not used to differentiate behavior:

```elixir
# Current: context ignored in nud
defp nud(token, state, _context, log) do
  ast = Builder.Helpers.literal(token.value)
  {:ok, ast, state, log}
end
```

**Needed:** Different parsing paths based on context:
- `:matched` - Standard expression, do-blocks allowed
- `:unmatched` - Inside if/unless/etc., restricted forms
- `:no_parens` - No-parentheses call context, arity restrictions apply

---

## Test Coverage

### Current Tests

| Test | What it covers |
|------|----------------|
| `pratt_precedence_test.exs` | BP ordering, associativity, basic Pratt parsing |
| `grammar expr_list dispatches to pratt` | Single expression via grammar |

### Missing Tests for Phase 4

1. **Multi-expression parsing**: `"1\n2\n3"` → `{:__block__, [], [1, 2, 3]}`
2. **EOE handling**: Leading/trailing newlines, semicolons
3. **Empty input**: `""` and `"\n\n"` cases
4. **Context threading**: Verify context reaches Pratt
5. **Warning emission**: `warn_pipe` and `warn_no_parens_after_do_op` triggers

---

## File Structure Assessment

**Exists:**
- `lib/toxic_parser/grammar.ex` ✅
- `lib/toxic_parser/grammar/expressions.ex` ✅

**Missing (per HL_PLAN.md):**
- `lib/toxic_parser/grammar/calls.ex` - Needed for `block_expr` and no-parens
- `lib/toxic_parser/warning.ex` - Needed for `warn_*` emissions

---

## Dependencies on Other Phases

| Dependency | Blocking Phase 4? | Notes |
|------------|-------------------|-------|
| Identifier classification | Partially | Needed for `block_expr`, defer that part |
| Call args parsing | Partially | Needed for `block_expr` and no-parens calls |
| Do-block parsing | Yes for `block_expr` | Defer `block_expr` to Phase 7 |
| Container parsing | No | Phase 6, independent |

---

## Checklist for Phase 4 Completion

### Must Have (Core Phase 4)

- [ ] `expr_list` parses multiple expressions separated by EOE
- [ ] `expr_list` wraps multiple expressions in `{:__block__, [], [...]}`
- [ ] Leading EOE skipped before first expression
- [ ] Trailing EOE handled (not an error)
- [ ] Empty input handled gracefully
- [ ] `Warning` struct defined
- [ ] Warnings list added to `State`
- [ ] Basic `warn_pipe` infrastructure (even if triggers incomplete)
- [ ] Tests for multi-expression parsing
- [ ] Tests for EOE handling edge cases

### Should Have (Context Foundation)

- [ ] Context affects parsing behavior (even minimally)
- [ ] `matched_expr` event emission with `:matched_expr` node kind
- [ ] `no_parens_expr` event emission with `:no_parens_expr` node kind
- [ ] Tests for context-specific behavior

### Deferred (Phase 5/7 Dependencies)

- [ ] `block_expr` variants → Phase 7
- [ ] `no_parens_one_expr` → Phase 5
- [ ] `no_parens_many_expr` → Phase 5
- [ ] `no_parens_one_ambig_expr` → Phase 5
- [ ] Arity-based nesting bans → Phase 5
- [ ] Full `warn_pipe` trigger conditions → Phase 5
- [ ] Full `warn_no_parens_after_do_op` triggers → Phase 5

---

## Recommendations

### Immediate Actions

1. **Implement `expr_list` with EOE loop** - This is the core Phase 4 deliverable and has no external dependencies

2. **Add `Warning` type and state field** - Infrastructure needed for ambiguity warnings

3. **Add multi-expression tests** - Validate the core grammar behavior

### Architectural Clarification Needed

The split between what's "Phase 4 Grammar" vs "Phase 5 Calls" is blurry:

- `block_expr` requires identifier classification and do-blocks
- No-parens variants require call argument parsing

**Suggestion:** Rename Phase 4 scope to "Grammar Layer — Expression Lists & Dispatch" and move `block_expr` and no-parens variants explicitly to Phase 5.

---

## Phase Status Summary

| Component | Status | Blocking Issues |
|-----------|--------|-----------------|
| `expr` dispatcher | ✅ Complete | None |
| `expr_list` multi-expression | 🔴 Missing | Core requirement |
| EOE handling | 🔴 Missing | Core requirement |
| Context threading | 🟡 Partial | Passes through but not used |
| `block_expr` | 🔴 Missing | Depends on Phase 5/7 |
| No-parens variants | 🔴 Missing | Depends on Phase 5 |
| Warning infrastructure | 🔴 Missing | Core requirement |
| Tests | 🔴 Minimal | Need multi-expr tests |

**Overall Phase 4: ~15% Complete**

**Recommendation:** Focus on `expr_list` EOE handling and warning infrastructure. Defer `block_expr` and no-parens variants to their natural phases (5/7). This keeps Phase 4 focused and achievable.
