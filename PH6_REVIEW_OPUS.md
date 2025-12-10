# Phase 6 Implementation Review: Containers & Access

**Reviewer:** Claude Opus 4.5
**Date:** 2025-12-10
**Status:** Early scaffolding stage (~20% complete). Basic list/tuple structure exists but most Phase 6 requirements unimplemented.

---

## Summary

Phase 6 requires parsing all container types (lists, tuples, maps, structs, bitstrings), access syntax, keyword tail handling, and error recovery using terminator stacks. The current implementation has basic list and tuple parsing with a critical bug (doesn't enforce commas), and explicitly defers maps. Bitstrings and access syntax are not implemented.

---

## Phase 6 Requirements (from HL_PLAN.md)

| Requirement | Status | Notes |
|-------------|--------|-------|
| Lists | 🟡 Partial | Basic structure, bug in element parsing |
| Tuples | 🟡 Partial | Basic structure, same bug |
| Maps (assoc) | 🔴 Missing | Explicitly deferred in code comment |
| Maps (update) | 🔴 Missing | `%{map \| key: val}` |
| Structs | 🔴 Missing | `%Module{...}` |
| Keyword-only maps | 🔴 Missing | `%{a: 1, b: 2}` |
| Bitstrings | 🔴 Missing | No `<<>>` parsing |
| Segment/modifier parsing | 🔴 Missing | `<<x::8-little>>` |
| Trailing comma rules | 🔴 Missing | `[1, 2,]` handling |
| `capture_int` in segments | 🔴 Missing | `<<&1::8>>` |
| Keyword tails last | 🔴 Missing | `[1, 2, key: val]` |
| EOE inside containers | 🔴 Missing | Newlines in lists |
| Sync on `]`, `}`, `>>` | 🔴 Missing | Recovery not implemented |
| Access syntax `expr[...]` | 🔴 Missing | No LED parselet for `[` |

---

## What's Implemented

### Container Entry Point Integration ✅

`Expressions.expr/3` correctly routes to containers first:
```elixir
def expr(%State{} = state, ctx, %EventLog{} = log) do
  case Containers.parse(state, ctx, log) do
    {:ok, ast, state, log} -> {:ok, ast, state, log}
    {:no_container, state} ->
      Calls.parse(state, ctx, log)  # fallback
  end
end
```

### List Parsing (`containers.ex:33-38`) 🟡

```elixir
defp parse_list(state, ctx, log) do
  {:ok, _open, state} = TokenAdapter.next(state)
  with {:ok, elements, state, log} <- parse_elements([], :"]", state, ctx, log) do
    {:ok, Enum.reverse(elements), state, log}
  end
end
```

**Works:** `[1, 2, 3]` → `[1, 2, 3]`

**Missing:**
- Empty list `[]` handling (works but untested)
- Trailing comma `[1, 2,]`
- Keyword tail `[1, key: val]`
- Nested containers `[[1], [2]]`
- EOE inside `[1\n2]`

### Tuple Parsing (`containers.ex:41-52`) 🟡

```elixir
defp parse_tuple(state, ctx, log) do
  {:ok, _open, state} = TokenAdapter.next(state)
  with {:ok, elements, state, log} <- parse_elements([], :"}", state, ctx, log) do
    ast = case elements do
      [] -> {:{}, [], []}
      list -> {:{}, [], Enum.reverse(list)}
    end
    {:ok, ast, state, log}
  end
end
```

**Works:** `{1, 2}` → `{:{}, [], [1, 2]}`

**Note:** Two-element tuples should be `{1, 2}` not `{:{}, [], [1, 2]}` for Elixir AST compatibility. This may need fixing.

### Element Parsing (`containers.ex:55-79`) ⚠️ BUG

```elixir
defp parse_elements(acc, terminator, state, ctx, log) do
  case TokenAdapter.peek(state) do
    {:ok, %{kind: ^terminator}, state} ->
      {:ok, _close, state} = TokenAdapter.next(state)
      {:ok, acc, state, log}

    {:ok, _tok, _state} ->
      with {:ok, expr, state, log} <- Expressions.expr(state, ctx, log) do
        case TokenAdapter.peek(state) do
          {:ok, %{kind: :","}, state} ->
            {:ok, _comma, state} = TokenAdapter.next(state)
            parse_elements([expr | acc], terminator, state, ctx, log)

          _ ->
            parse_elements([expr | acc], terminator, state, ctx, log)  # BUG!
        end
      end
    # ...
  end
end
```

**BUG:** When no comma is found after an expression, it continues parsing without requiring one. This means `[1 2 3]` would be accepted as valid, which is incorrect.

**Fix needed:**
```elixir
_ ->
  # After expression, must see comma or terminator
  case TokenAdapter.peek(state) do
    {:ok, %{kind: ^terminator}, _} ->
      parse_elements([expr | acc], terminator, state, ctx, log)
    _ ->
      {:error, {:expected, [:comma, terminator]}, state, log}
  end
```

### Map Parsing (Explicitly Deferred) 🔴

```elixir
{:ok, %{kind: :%{}}, _} ->
  # Map parsing is deferred to a later phase; for now treat as empty map literal.
  {:ok, _tok, state} = TokenAdapter.next(state)
  {:ok, %{}, state, log}
```

This is a placeholder that returns an empty map for any `%{...}` input.

---

## Critical Gaps for Phase 6

### 1. Map Parsing 🔴

**Required forms:**

```elixir
# Keyword-only map
%{a: 1, b: 2}

# Assoc syntax (arrow)
%{:a => 1, "b" => 2}

# Mixed (arrow keys with keyword values - invalid, should error)
%{:a => 1, b: 2}

# Update syntax
%{existing_map | key: new_value}

# Struct
%MyModule{field: value}

# Struct update
%MyModule{existing | field: value}
```

**Implementation needed:**
- Detect `%{` vs `%Module{` (struct)
- Parse assoc pairs (`:key => value`)
- Parse keyword pairs (`key: value`)
- Parse update syntax (`| field: value`)
- Validate no mixing of assoc and keyword in same map

### 2. Bitstring Parsing 🔴

**Required forms:**

```elixir
# Basic
<<1, 2, 3>>

# With size
<<x::8>>

# With type
<<x::integer>>

# With multiple modifiers
<<x::size(8)-little-unsigned-integer>>

# With unit
<<x::size(n)-unit(8)>>

# Binary segment
<<binary::binary>>

# UTF-8
<<char::utf8>>
```

**Implementation needed:**
- Recognize `<<` and `>>`
- Parse segments separated by commas
- Parse segment modifiers after `::`
- Handle size expressions `size(expr)`
- Handle type/signedness/endianness modifiers
- Handle `unit(n)` specifier
- Validate modifier combinations

### 3. Access Syntax in Pratt 🔴

**Required:** `expr[key]` should parse as access.

Current Pratt `led/5` only handles binary operators:
```elixir
defp led(left, state, log, min_bp, context) do
  case TokenAdapter.peek(state) do
    {:ok, next_token, _} ->
      case Precedence.binary(next_token.kind) do
        {bp, assoc} when bp >= min_bp ->
          # ... binary operator handling
```

**Needed:** Add `[` detection in LED:
```elixir
{:ok, %{kind: :"["}, _} ->
  # Access syntax - highest precedence
  parse_access(left, state, log, context)
```

**Also needed:** Disambiguation between:
- `foo[1]` → access (bracket_identifier)
- `foo [1]` → call with list arg (space before `[`)

### 4. Keyword Tail Handling 🔴

**Required:** Keywords must come last in argument/element lists.

```elixir
# Valid
[1, 2, a: 3, b: 4]

# Invalid
[a: 1, 2, 3]
```

**Implementation needed:**
- Track whether we've seen a keyword pair
- After seeing keyword, only allow more keywords
- Emit error/warning for positional after keyword

### 5. Trailing Comma Support 🔴

**Required:** Elixir allows trailing commas in containers.

```elixir
[1, 2,]      # Valid
{1, 2,}      # Valid
%{a: 1,}     # Valid
<<1, 2,>>    # Valid
```

**Implementation needed:**
- After consuming comma, check if next token is terminator
- If so, don't try to parse another element

### 6. EOE Inside Containers 🔴

**Required:** Newlines are allowed inside containers.

```elixir
[
  1,
  2,
  3
]
```

**Implementation needed:**
- Skip EOE tokens between elements (after comma)
- Or before first element
- But NOT in place of commas

### 7. Sync/Recovery on Delimiters 🔴

**Required:** Error recovery should sync on container terminators.

```elixir
[1, @#$%, 3]  # Should recover at `]`
```

**Implementation needed:**
- Use `State.terminators` to know expected closers
- On error, scan forward to terminator
- Emit error node for skipped content
- Continue parsing after sync point

---

## Test Coverage

### Existing Tests (2 total)

| Test | Input | Expected |
|------|-------|----------|
| List literal | `[1, 2]` | `[1, 2]` |
| Tuple literal | `{1, 2}` | `{:{}, [], [1, 2]}` |

### Missing Tests

**Lists:**
- Empty: `[]`
- Single element: `[1]`
- Trailing comma: `[1,]`
- Nested: `[[1], [2]]`
- Keyword tail: `[1, a: 2]`
- With newlines: `[\n1,\n2\n]`
- Invalid (no comma): `[1 2]` → should error

**Tuples:**
- Empty: `{}`
- Single: `{1}`
- Two-element: `{1, 2}` → should be `{1, 2}` not `{:{}, ...}`

**Maps:**
- Empty: `%{}`
- Keyword: `%{a: 1}`
- Assoc: `%{:a => 1}`
- Update: `%{m | a: 1}`

**Structs:**
- Basic: `%Mod{a: 1}`
- Nested: `%Mod.Sub{a: 1}`
- Update: `%Mod{s | a: 1}`

**Bitstrings:**
- Empty: `<<>>`
- Basic: `<<1, 2>>`
- With size: `<<x::8>>`
- With modifiers: `<<x::little-unsigned>>`

**Access:**
- Basic: `foo[1]`
- Chained: `foo[1][2]`
- On expression: `(foo)[1]`

---

## Infrastructure Status

### Ready ✅

| Component | Location | Notes |
|-----------|----------|-------|
| `Builder.Helpers.access/3` | `builder/helpers.ex:32-35` | Unused |
| `Precedence.access/0` | `precedence.ex:78-79` | BP 330 |
| `State.terminators` | `state.ex:29,45` | Available |
| `TokenAdapter.current_terminators/1` | `token_adapter.ex:32-36` | Available |
| Event node kinds | `event_log.ex` | `:list`, `:tuple`, `:map`, `:bitstring` |

### Missing

- Map AST builder helpers
- Bitstring segment builder
- Keyword pair detection
- Recovery/sync helpers

---

## Checklist for Phase 6 Completion

### Must Have (Core Phase 6)

- [ ] **Fix `parse_elements` comma bug** - Critical correctness issue
- [ ] Empty container handling (`[]`, `{}`, `%{}`, `<<>>`)
- [ ] Trailing comma support
- [ ] Map parsing (keyword and assoc forms)
- [ ] Struct parsing (`%Module{...}`)
- [ ] Bitstring parsing (basic segments)
- [ ] Access syntax in Pratt LED
- [ ] Tests for all container types

### Should Have (Full Phase 6)

- [ ] Map update syntax (`%{m | k: v}`)
- [ ] Struct update syntax
- [ ] Bitstring modifiers (size, type, signedness, endianness)
- [ ] Keyword tail enforcement
- [ ] EOE handling inside containers
- [ ] `capture_int` in bitstring segments
- [ ] Terminator-based sync/recovery
- [ ] Comprehensive edge case tests

### Nice to Have

- [ ] Detailed error messages for invalid syntax
- [ ] Warning for deprecated/ambiguous forms
- [ ] Performance optimization for large containers

---

## Recommendations

### Immediate Actions

1. **Fix the comma bug in `parse_elements`** - This is a correctness blocker

2. **Implement map parsing** - Most common container after lists
   - Start with keyword-only maps `%{a: 1}`
   - Add assoc syntax `%{:a => 1}`
   - Then update syntax

3. **Add access syntax to Pratt LED** - Simple addition, high value

4. **Add more list/tuple tests** - Validate edge cases before expanding

### Implementation Order Suggestion

1. Fix `parse_elements` comma requirement
2. Add trailing comma support
3. Add empty container tests
4. Implement keyword-only maps
5. Add access syntax to Pratt
6. Implement assoc maps
7. Implement map update
8. Implement structs
9. Implement basic bitstrings
10. Add bitstring modifiers
11. Add keyword tail enforcement
12. Add EOE handling
13. Add recovery/sync

---

## Phase Status Summary

| Component | Status | Blocking Issues |
|-----------|--------|-----------------|
| List parsing | 🟡 Partial | Comma bug, no trailing comma |
| Tuple parsing | 🟡 Partial | Comma bug, AST format question |
| Map parsing | 🔴 Missing | Explicitly deferred |
| Struct parsing | 🔴 Missing | Depends on map |
| Bitstring parsing | 🔴 Missing | No code exists |
| Access syntax | 🔴 Missing | No LED for `[` |
| Keyword tails | 🔴 Missing | No tracking |
| EOE in containers | 🔴 Missing | Not implemented |
| Recovery/sync | 🔴 Missing | Infrastructure ready but unused |
| Tests | 🔴 Minimal | 2 basic tests |

**Overall Phase 6: ~20% Complete**

**Recommendation:** Fix the comma bug first, then implement maps, then access syntax. Bitstrings are complex and could be a later priority if needed for unblocking Phase 7+.
