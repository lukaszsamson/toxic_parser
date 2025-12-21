# ToxicParser Code Review Issues

This document captures logic issues, performance concerns, and refactoring opportunities identified in the ToxicParser codebase.

---

## 1. Logic Issues

### 1.1 Duplicated `build_result` function clauses (ToxicParser)

**Location:** `lib/toxic_parser.ex` lines 100-151

**Issue:** Two nearly identical `build_result/1` function clauses exist, differing only in whether `extra_diagnostics` is present. This can lead to subtle bugs if one is modified without the other.

**Recommendation:** Consolidate into a single function clause that defaults `extra_diagnostics` to `[]`:

```elixir
defp build_result(%{extra_diagnostics: extra_diagnostics} = params) do
  # Use extra_diagnostics from params or default to []
end

defp build_result(params) do
  build_result(Map.put(params, :extra_diagnostics, []))
end
```

---

### 1.2 Inconsistent error normalization (Result module)

**Location:** `lib/toxic_parser/result.ex` lines 39-44

**Issue:** `normalize_error/2` returns the original term unchanged when it doesn't match expected patterns. This could mask bugs where unexpected error shapes propagate through the system.

**Recommendation:** Log or raise on unexpected patterns in debug mode, or at least document the expected behavior clearly.

---

### 1.3 Unused `ctx` parameter in multiple functions

**Location:** Multiple files:
- `lib/toxic_parser/grammar/keywords.ex` line 108 (`parse_kw_pair`)
- `lib/toxic_parser/grammar/stabs.ex` line 964 (`parse_stab_pattern_exprs`)
- `lib/toxic_parser/grammar/dots.ex` line 248 (`parse_paren_args`)

**Issue:** Several functions have `ctx` parameters that are received but never used (or only passed through without effect). This indicates either dead code or missing context-sensitive behavior.

**Recommendation:** Audit each case to determine if `ctx` should be used or removed.

---

### 1.4 Magic numbers for binding power thresholds

**Location:** Multiple files:
- `lib/toxic_parser/pratt.ex` line 249 (`300` for dual_op unary)
- `lib/toxic_parser/pratt.ex` line 566 (`100` for ellipsis operand min_bp)
- `lib/toxic_parser/pratt.ex` line 654 (`300` for ternary unary operand)

**Issue:** Hard-coded binding power values are scattered throughout the code without clear documentation or named constants.

**Recommendation:** Define these as module attributes in `Precedence` with descriptive names:
```elixir
@dual_op_unary_bp 300
@matched_expr_min_bp 100
```

---

### 1.5 State mutation through map direct updates

**Location:** `lib/toxic_parser/token_adapter.ex` lines 189-197

**Issue:** `update_state/6` uses `Map.put` and `Map.update!` instead of struct update syntax, which bypasses compile-time key validation.

**Recommendation:** Use struct update syntax `%{state | field: value}` consistently.

---

## 2. Performance Issues

### 2.1 Repeated list reversals in argument accumulation

**Location:** Multiple files:
- `lib/toxic_parser/grammar/calls.ex` lines 353, 389
- `lib/toxic_parser/grammar/containers.ex` lines 348, 395, 408
- `lib/toxic_parser/grammar/stabs.ex` lines 424, 551, 884

**Issue:** Argument lists are accumulated in reverse order (prepending with `[x | acc]`), then reversed with `Enum.reverse` at the end. However, some code paths do `Enum.reverse(acc) ++ [kw_list]` which is O(n) twice.

**Recommendation:** Consider using a consistent pattern:
- Accumulate in reverse order
- Single `Enum.reverse` at final return
- Avoid `Enum.reverse(acc) ++ [item]`; instead: `Enum.reverse([item | acc])`

---

### 2.2 Inefficient `line_index` construction

**Location:** `lib/toxic_parser/state.ex` lines 87-91

**Issue:** `line_index/1` uses `:binary.matches/2` followed by list comprehension and `List.to_tuple/1`. For large files, this creates multiple intermediate lists.

**Recommendation:** Use a single-pass binary traversal to build the index more efficiently.

---

### 2.3 Excessive checkpoint creation

**Location:** Multiple files in grammar parsing

**Issue:** Many parse attempts create checkpoints for speculative parsing, then either rewind or drop. Each checkpoint copies parser state including lookahead buffer and diagnostics list.

**Recommendation:** Consider:
- Lazy checkpoint creation (only copy state when rewind is actually needed)
- Memoization for common parse patterns
- Better lookahead to reduce speculative parsing

---

### 2.4 Repeated `TokenAdapter.peek` calls

**Location:** Throughout grammar modules

**Issue:** Many code paths call `TokenAdapter.peek(state)` multiple times in quick succession, even when the result hasn't changed.

**Recommendation:** Cache the peek result in local variables when multiple conditionals need to examine the same token.

---

### 2.5 String concatenation in `unescape`

**Location:** `lib/toxic_parser/grammar/strings.ex` line 564

**Issue:** `unescape/1` delegates to `Macro.unescape_string/1` but string unescaping happens frequently. Consider whether caching or batching could help for large files with many strings.

**Recommendation:** Profile string-heavy inputs to determine if this is actually a bottleneck.

---

## 3. Refactoring Opportunities

### 3.1 Extract common token consumption patterns

**Location:** Throughout grammar modules

**Issue:** The pattern `{:ok, token, state} = TokenAdapter.next(state)` followed by metadata extraction and error handling is repeated hundreds of times with slight variations.

**Recommendation:** Create helper macros or functions:
```elixir
defmacro consume!(state, kind) do
  # Pattern match and extract, raise on mismatch
end

def consume_meta(state, kind) do
  # Returns {:ok, meta, state} or {:error, reason, state}
end
```

---

### 3.2 Consolidate keyword parsing continuation patterns

**Location:** 
- `lib/toxic_parser/grammar/calls.ex` lines 539-631 (`parse_no_parens_quoted_kw_continuation`, `parse_quoted_kw_continuation`)
- `lib/toxic_parser/grammar/dots.ex` lines 330-376 (`parse_paren_args_quoted_kw_continuation`)
- `lib/toxic_parser/grammar/stabs.ex` lines 482-533, 911-962 (multiple continuation functions)
- `lib/toxic_parser/grammar/containers.ex` lines 1439-1490 (`parse_dot_container_quoted_kw_continuation`)
- `lib/toxic_parser/pratt.ex` lines 1579-1641 (`handle_when_keyword_continuation`)

**Issue:** Nearly identical "quoted keyword continuation" logic is duplicated across 6+ locations. Each handles:
1. Parse expression
2. Check if keyword list
3. Check for comma
4. Check for more keywords or terminator
5. Merge or return

**Recommendation:** Extract a generic `parse_keyword_continuation/5` that takes:
- Accumulated keywords
- Accumulator for non-keyword items
- State/log
- Terminator kinds
- Parse function for next item

---

### 3.3 Unify `parse_*_base` vs `parse_*` pattern

**Location:**
- `lib/toxic_parser/grammar/containers.ex` (`parse_list` vs `parse_list_base`, `parse_tuple` vs `parse_tuple_base`)
- `lib/toxic_parser/pratt.ex` (`parse` vs `parse_base` vs `parse_base_with_dots` vs `parse_base_with_dots_and_calls`)

**Issue:** Multiple `*_base` functions exist to parse without calling `led()` at the end. The pattern is inconsistent and the naming doesn't clearly convey intent.

**Recommendation:** Standardize on a single approach:
- Option 1: Add `led?: boolean` option to parse functions
- Option 2: Always return AST without led(), caller decides to continue
- Option 3: Rename to `parse_nud_only` / `parse_with_led` for clarity

---

### 3.4 Large `led_dispatch` function needs decomposition

**Location:** `lib/toxic_parser/pratt.ex` lines 842-890

**Issue:** `led_dispatch/8` is a large case statement that handles many different token types. Adding new operators or changing behavior requires modifying this central function.

**Recommendation:** Use a table-driven approach or protocol:
```elixir
@led_handlers %{
  :"(" => &led_call/6,
  :dot_call_op => &led_dot_call/6,
  :dot_op => &led_dot/6,
  # ...
}
```

---

### 3.5 Extract `ToxicParser.Grammar.Strings` heredoc handling

**Location:** `lib/toxic_parser/grammar/strings.ex` lines 81-150, 325-365, 444-560

**Issue:** Heredoc parsing, indentation trimming, and line continuation handling is complex and interleaved with simple string parsing. This makes both harder to understand and modify.

**Recommendation:** Extract heredoc-specific logic into a separate module `ToxicParser.Grammar.Heredocs`.

---

### 3.6 Consolidate EOE handling

**Location:**
- `lib/toxic_parser/grammar/eoe.ex` (likely)
- Inline EOE skipping throughout grammar modules

**Issue:** EOE (end-of-expression) handling has multiple patterns:
- `EOE.skip(state)`
- `EOE.skip_count_newlines(state, 0)`
- `{state, _newlines} = EOE.skip_count_newlines(state, 0)` (ignoring newlines)
- Inline `case TokenAdapter.peek(state) do {:ok, %{kind: :eoe}, _} -> ...`

**Recommendation:** Standardize on `EOE.skip_count_newlines` and provide `EOE.skip(state)` as a convenience wrapper. Remove inline EOE checking.

---

### 3.7 Pratt.ex is too large (2097 lines)

**Location:** `lib/toxic_parser/pratt.ex`

**Issue:** At over 2000 lines, `pratt.ex` handles too many concerns:
- Core Pratt algorithm (nud/led)
- Binary operator parsing
- Unary operator parsing
- Ternary operator parsing
- Dot expressions
- Bracket access
- Call parsing
- Keyword handling

**Recommendation:** Split into focused modules:
- `ToxicParser.Pratt.Core` - main parse/led/nud dispatch
- `ToxicParser.Pratt.Binary` - binary operator handling
- `ToxicParser.Pratt.Unary` - unary operator handling
- `ToxicParser.Pratt.Access` - dot and bracket access

---

### 3.8 Stabs.ex duplication with Pratt.ex

**Location:** `lib/toxic_parser/grammar/stabs.ex` and `lib/toxic_parser/pratt.ex`

**Issue:** Both modules have similar argument parsing logic (`parse_stab_parens_args`, `parse_call_args_parens`, `parse_paren_args`). Stabs.ex duplicates call argument parsing that already exists in Calls/Pratt.

**Recommendation:** Extract shared argument parsing into a dedicated module `ToxicParser.Grammar.Args` that both can use.

---

### 3.9 Missing type specs on internal functions

**Location:** Throughout all grammar modules

**Issue:** Only public API functions have `@spec` annotations. Internal helper functions lack type specifications, making it harder to understand data flow and catch type errors.

**Recommendation:** Add `@spec` to key internal functions, especially:
- Token manipulation functions
- AST building functions
- Parse result transformers

---

### 3.10 Inconsistent error tuple formats

**Location:** Throughout codebase

**Issue:** Error tuples take multiple forms:
- `{:error, reason, state, log}` (4-tuple)
- `{:error, reason, state}` (3-tuple)
- `{:error, {:expected, kind, got: actual}, state, log}` (structured reason)
- `{:error, :unexpected_eof, state, log}` (atom reason)

**Recommendation:** Standardize on structured errors with consistent format:
```elixir
%ParseError{
  kind: :unexpected_token | :eof | :expected,
  expected: term(),
  got: term(),
  location: location()
}
```

---

### 3.11 Dead code / TODO comments

**Location:** Multiple files have `# TODO: no coverage?` comments

**Issue:** Several code paths are marked as having no test coverage or only failing corpus tests. These may be dead code or represent edge cases that need investigation.

**Recommendation:** 
1. Run coverage analysis
2. Investigate each `TODO: no coverage` comment
3. Either add tests or remove dead code

---

## 4. Documentation Issues

### 4.1 Missing module documentation

**Location:** `lib/toxic_parser/grammar/do_blocks.ex`

**Issue:** Module has `@moduledoc false`. As a key component of block expression handling, it should be documented.

**Recommendation:** Add documentation explaining the module's role in do-block attachment.

---

### 4.2 Outdated phase references in comments

**Location:** Multiple files reference "Phase 3", "Phase 4", "Phase 5" etc.

**Issue:** These phase references from incremental development may no longer be meaningful to maintainers.

**Recommendation:** Either remove phase references or consolidate them into a development history document.

---

## Summary

| Category | Count | Priority |
|----------|-------|----------|
| Logic Issues | 5 | High |
| Performance Issues | 5 | Medium |
| Refactoring Opportunities | 11 | Medium |
| Documentation Issues | 2 | Low |

**Top Priority Items:**
1. **3.7** Split `pratt.ex` into focused modules (maintainability)
2. **3.2** Consolidate keyword continuation patterns (DRY)
3. **1.1** Fix duplicated `build_result` (bug risk)
4. **2.1** Fix inefficient list operations (performance)
5. **3.4** Decompose `led_dispatch` (extensibility)
