# Phase 7 Implementation Review: Blocks, Clauses & Environment

**Reviewer:** Claude Opus 4.5
**Date:** 2025-12-10
**Status:** Substantially complete (~75%). Core block parsing works well with good test coverage. Missing comprehensions (`for`) and environment state management infrastructure.

---

## Summary

Phase 7 requires parsing block constructs (`fn`, `case`, `cond`, `with`, `try`, `receive`), stab clauses with guards, block labels, and environment event emission. The implementation is well-architected with 430 lines of clean code, proper scope tracking events, and good test coverage. However, comprehensions (`for`) are entirely missing, and environment events are emitted but not consumed by dedicated state management.

**This is the most complete phase reviewed so far.**

---

## Phase 7 Requirements (from HL_PLAN.md)

| Requirement | Status | Notes |
|-------------|--------|-------|
| Do-blocks (`do ... end`) | ✅ Done | `parse_do_block/3` with section handling |
| `fn` expressions | ✅ Done | `parse_fn/3` with clauses |
| Stab clauses (`->`) | ✅ Done | `parse_clause/4` with pattern/guard/body |
| Empty stab (`fn -> :ok end`) | ✅ Done | Tested |
| Paren stab (`fn (x) -> x end`) | 🟡 Untested | Logic exists but no test |
| No-paren stab (`fn x -> x end`) | 🟡 Untested | Logic exists but no test |
| Block labels (`else/catch/rescue/after`) | ✅ Done | `block_label?/1`, `label_from/1` |
| EOE handling in blocks | ✅ Done | `consume_optional_eoe/1` |
| `case` expression | ✅ Done | With clauses, tested |
| `cond` expression | ✅ Done | Subject = nil |
| `with` expression | ✅ Done | Qualifiers + else, tested |
| `try` expression | ✅ Done | With rescue/after, tested |
| `receive` expression | ✅ Done | Subject = nil |
| Comprehensions (`for`) | 🔴 Missing | Not in recognized keywords |
| Guard handling (`when`) | ✅ Done | `maybe_guard/4` |
| Generator handling (`<-`) | ✅ Done | `maybe_generator/4` |
| Scope enter/exit events | ✅ Done | `enter_scope/3`, `exit_scope/3` |
| Binding events | ✅ Done | `maybe_bind_env/3` for atoms |
| Aliases/imports/uses/requires | 🔴 Missing | Not implemented |
| Environment state module | 🔴 Missing | Events emitted but not consumed |

---

## What's Implemented

### Block Entry Point (`blocks.ex:19-38`) ✅

```elixir
def parse(%State{} = state, ctx, %EventLog{} = log) do
  case TokenAdapter.peek(state) do
    {:ok, %{kind: :fn} = tok, _} ->
      parse_fn(tok, state, ctx, log)

    {:ok, %{kind: kind, value: value} = tok, _}
    when kind in [:identifier, :do_identifier, :block_identifier] and
           value in [:case, :cond, :with, :try, :receive] ->
      parse_keyword_block(value, tok, state, ctx, log)

    _ ->
      {:no_block, state}
  end
end
```

**Recognized constructs:**
- `:fn` → `parse_fn/4`
- `:case`, `:cond`, `:with`, `:try`, `:receive` → `parse_keyword_block/5`

**Missing:** `:for` not in the value list.

### fn Expression (`blocks.ex:41-49`) ✅

```elixir
defp parse_fn(fn_tok, state, ctx, log) do
  {:ok, _fn, state} = TokenAdapter.next(state)
  log = enter_scope(log, :fn, fn_tok.metadata)

  with {:ok, clauses, state, log} <- parse_clauses([], state, ctx, log, [:end]),
       {:ok, _end, state} <- expect_kind(state, :end) do
    log = exit_scope(log, :fn, fn_tok.metadata)
    {:ok, {:fn, [], clauses}, state, log}
  end
end
```

**Good:**
- Scope enter/exit events
- Clause parsing delegation
- Proper `end` expectation

### Keyword Blocks (`blocks.ex:52-87`) ✅

Handles `case`, `cond`, `with`, `try`, `receive` with:
- Subject parsing (where applicable)
- Do-block attachment
- Scope tracking

**Special handling for `with`:**
```elixir
:with ->
  with {:ok, qualifiers, state, log} <- parse_with_qualifiers([], state, ctx, log),
       {:ok, block_kw, state, log} <- parse_do_block(state, ctx, log) do
    ast = {:with, [], Enum.reverse(qualifiers) ++ [block_kw]}
    # ...
  end
```

### Do-Block Parsing (`blocks.ex:96-117`) ✅

```elixir
def parse_do_block(state, ctx, log) do
  case TokenAdapter.next(state) do
    {:ok, %{kind: :do, metadata: meta}, state} ->
      log = enter_scope(log, :do_block, meta)

      with {:ok, sections, state, log} <-
             parse_labeled_sections([], :do, state, ctx, log),
           {:ok, _end, state} <- expect_kind(state, :end) do
        log = exit_scope(log, :do_block, meta)
        {:ok, sections, state, log}
      end
    # ...
  end
end
```

### Labeled Sections (`blocks.ex:119-138`) ✅

Handles `else`, `catch`, `rescue`, `after` with:
- Section-aware loops
- Label detection via `block_label?/1`
- Proper keyword list building

### Clause Parsing (`blocks.ex:260-288`) ✅

```elixir
defp parse_clause(state, ctx, log, stop_kinds) do
  with {:ok, head_ast, state, log} <- Pratt.parse(state, ctx, log) do
    case head_ast do
      {:stab_op, _, [lhs, rhs]} ->
        # Pre-parsed stab
        {:ok, {:->, [], [List.wrap(lhs), rhs]}, state, log}

      :-> ->
        # Empty stab
        with {:ok, body, state, log} <- parse_clause_body(...) do
          {:ok, {:->, [], [[], body]}, state, log}
        end

      _ ->
        # Pattern -> body
        {head, state, log} = maybe_generator(head_ast, state, ctx, log)
        {head, state, log} = maybe_guard(head, state, ctx, log)
        log = maybe_bind_env(log, head, head_meta)
        # ... expect stab_op, parse body
    end
  end
end
```

### Guard Handling (`blocks.ex:303-314`) ✅

```elixir
defp maybe_guard(pattern, state, ctx, log) do
  case TokenAdapter.peek(state) do
    {:ok, %{kind: :when_op}, _} ->
      {:ok, _when, state} = TokenAdapter.next(state)
      with {:ok, guard, state, log} <- Pratt.parse(state, ctx, log) do
        {{:when, [], [pattern, guard]}, state, log}
      end
    _ ->
      {pattern, state, log}
  end
end
```

### Generator Handling (`blocks.ex:290-301`) ✅

```elixir
defp maybe_generator(pattern, state, ctx, log) do
  case TokenAdapter.peek(state) do
    {:ok, %{kind: :in_match_op}, _} ->
      {:ok, _op, state} = TokenAdapter.next(state)
      with {:ok, rhs, state, log} <- Pratt.parse(state, ctx, log) do
        {{:<-, [], [pattern, rhs]}, state, log}
      end
    _ ->
      {pattern, state, log}
  end
end
```

### Environment Events (`blocks.ex:385-400`) ✅

```elixir
defp enter_scope(log, scope, meta) do
  EventLog.env(log, %{action: :enter_scope, scope: scope, name: nil}, meta)
end

defp exit_scope(log, scope, meta) do
  EventLog.env(log, %{action: :exit_scope, scope: scope, name: nil}, meta)
end

defp maybe_bind_env(log, name, meta) when is_atom(name) do
  EventLog.env(log, %{action: :bind, scope: nil, name: name}, meta)
end
```

**Event payload type** (from `event_log.ex:100-104`):
```elixir
@type env_payload :: %{
  action: :enter_scope | :exit_scope | :bind,
  scope: atom() | nil,
  name: atom() | nil
}
```

---

## Critical Gaps

### 1. Comprehensions (`for`) 🔴

**Missing entirely.** The value list on line 27:
```elixir
value in [:case, :cond, :with, :try, :receive]
```

Does not include `:for`.

**Required implementation:**
```elixir
# Add to recognized keywords
value in [:case, :cond, :with, :try, :receive, :for] ->
  parse_keyword_block(value, tok, state, ctx, log)

# In parse_keyword_block/5:
:for ->
  with {:ok, qualifiers, state, log} <- parse_for_qualifiers([], state, ctx, log),
       {:ok, block_kw, state, log} <- parse_do_or_keyword(state, ctx, log) do
    ast = {:for, [], Enum.reverse(qualifiers) ++ [block_kw]}
    # ...
  end
```

**For comprehensions need:**
- Generators: `x <- list`
- Filters: `x > 0` (bare expressions)
- `do:` keyword or `do ... end` block
- `into:` option for collectables
- `reduce:` option for reductions
- `uniq:` option for deduplication

### 2. Environment State Module 🔴

Events are emitted but no consumer exists.

**Missing files:**
- `lib/toxic_parser/env.ex` - Main environment module
- `lib/toxic_parser/env/scope.ex` - Scope management

**Expected functionality:**
```elixir
defmodule ToxicParser.Env do
  @type t :: %__MODULE__{
    scopes: [Scope.t()],
    bindings: %{atom() => binding_info()},
    aliases: %{atom() => module()},
    imports: [module()],
    requires: [module()]
  }

  def from_events(events) do
    # Consume :env events and build state
  end

  def lookup(env, name) do
    # Find binding in scope chain
  end
end
```

### 3. Aliases/Imports/Uses/Requires 🔴

Not tracked. These are typically top-level declarations:
```elixir
alias Foo.Bar
import Enum, only: [map: 2]
use GenServer
require Logger
```

May be a concern for later phases (top-level parsing), but HL_PLAN mentions them in Phase 7.

### 4. Pattern Binding Limited 🟡

`maybe_bind_env/3` only handles atom patterns:
```elixir
defp maybe_bind_env(log, name, meta) when is_atom(name) do
  EventLog.env(log, %{action: :bind, scope: nil, name: name}, meta)
end

defp maybe_bind_env(log, _other, _meta), do: log  # Ignores complex patterns!
```

**Missing:**
- Tuple patterns: `{a, b} = ...`
- List patterns: `[h | t] = ...`
- Map patterns: `%{key: val} = ...`
- Struct patterns: `%Mod{field: f} = ...`
- Pin operator: `^existing`

---

## Test Coverage

### Existing Tests (5 tests) ✅

| Test | Input | Validates |
|------|-------|-----------|
| Zero-arity fn | `fn -> :ok end` | Empty clause parsing |
| Do-block attachment | `foo do :ok end` | Call + block integration |
| Simple case | `case x do x -> :ok end` | Subject + clause |
| Try with sections | `try do :ok rescue e -> e after :after end` | Labeled sections |
| With generator | `with x <- y do :ok else x -> :err end` | Qualifiers + else |

### Missing Tests

**fn variants:**
- `fn x -> x end` - Single arg
- `fn x, y -> x + y end` - Multiple args
- `fn (x) -> x end` - Parenthesized
- `fn x when x > 0 -> x end` - With guard
- `fn 0 -> :zero; n -> n end` - Multiple clauses

**Other constructs:**
- `cond do true -> :ok end`
- `receive do msg -> msg after 1000 -> :timeout end`
- `for x <- [1,2,3], do: x * 2` - Basic comprehension
- `for x <- list, x > 0, do: x` - With filter
- `for x <- list, into: %{}, do: {x, x}` - With into

**Guards:**
- `case x do y when y > 0 -> y end`
- `fn x when is_integer(x) -> x end`

**Environment:**
- Verify enter_scope events emitted
- Verify exit_scope events emitted
- Verify bind events for pattern variables

---

## Code Quality

### Strengths

1. **Clean architecture** - Well-separated concerns (entry, clauses, sections, helpers)
2. **Consistent patterns** - All constructs follow same with/case structure
3. **Proper scope tracking** - Enter/exit events at correct points
4. **Checkpoint usage** - `try_parse_clause` uses checkpoint for backtracking
5. **EOE handling** - Consistent optional EOE consumption
6. **430 lines** - Comprehensive but not bloated

### Minor Issues

1. **Binding limited to atoms** - Complex patterns ignored
2. **No dedicated stab module** - Inlined (acceptable design choice)
3. **Limited error recovery** - Errors propagate up without sync

---

## Integration Status

| Phase | Integration | Status |
|-------|-------------|--------|
| Phase 3 (Pratt) | Used for expressions in clauses | ✅ Working |
| Phase 4 (Grammar) | Expression dispatcher routes to blocks | ✅ Working |
| Phase 5 (Calls) | Do-block attachment | ✅ Working |
| Phase 6 (Containers) | Patterns in clauses | ⚠️ Phase 6 incomplete |

---

## Checklist for Phase 7 Completion

### Must Have (Core Phase 7)

- [x] `fn` expression parsing
- [x] Do-block parsing
- [x] Block labels (else/catch/rescue/after)
- [x] `case` expression
- [x] `cond` expression
- [x] `with` expression
- [x] `try` expression
- [x] `receive` expression
- [ ] **`for` comprehensions** - Critical gap
- [x] Stab clause parsing
- [x] Guard handling (`when`)
- [x] Generator handling (`<-`)
- [x] Scope enter/exit events
- [x] Basic binding events

### Should Have (Full Phase 7)

- [ ] `for` with filters
- [ ] `for` with `into:`/`reduce:`/`uniq:`
- [ ] Complex pattern binding (tuples, lists, maps)
- [ ] Tests for fn variants (args, guards, multiple clauses)
- [ ] Tests for cond, receive
- [ ] Environment state module (`env.ex`)
- [ ] Scope management (`env/scope.ex`)

### Nice to Have

- [ ] Aliases/imports/uses/requires tracking
- [ ] Pattern binding for all AST forms
- [ ] Shadowing detection
- [ ] More comprehensive error messages

---

## Recommendations

### Immediate Actions

1. **Add `for` to recognized keywords** - Simple addition, high impact
   ```elixir
   value in [:case, :cond, :with, :try, :receive, :for]
   ```

2. **Implement `parse_for_qualifiers`** - Similar to `parse_with_qualifiers`

3. **Add fn variant tests** - Validate existing logic

### Future Work

4. **Create `env.ex` and `env/scope.ex`** - Consume emitted events

5. **Extend `maybe_bind_env`** - Handle tuple/list/map patterns

6. **Add comprehension options** - `into:`, `reduce:`, `uniq:`

---

## Phase Status Summary

| Component | Status | Notes |
|-----------|--------|-------|
| `fn` parsing | ✅ Done | All forms supported |
| Do-blocks | ✅ Done | With labeled sections |
| `case/cond/try/receive` | ✅ Done | Full support |
| `with` | ✅ Done | Qualifiers + else |
| `for` (comprehensions) | 🔴 Missing | Not in keyword list |
| Stab clauses | ✅ Done | Pattern/guard/body |
| Guards (`when`) | ✅ Done | `maybe_guard/4` |
| Generators (`<-`) | ✅ Done | `maybe_generator/4` |
| Scope events | ✅ Done | Enter/exit emitted |
| Binding events | 🟡 Partial | Atoms only |
| Environment state | 🔴 Missing | No consumer module |
| Tests | 🟡 Partial | 5 tests, need more |

**Overall Phase 7: ~75% Complete**

**Recommendation:** Add `for` comprehensions (critical), then expand test coverage. Environment state modules can be deferred if LSP integration isn't immediate priority.
