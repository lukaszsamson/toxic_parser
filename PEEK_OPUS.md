# Peek_N Analysis for ToxicParser

## Executive Summary

After analyzing both the original Elixir grammar (`elixir_parser.yrl`) and the ToxicParser implementation, I conclude that:

1. **The grammar is LALR(1) at the token level** - no grammar rules inherently require multi-token lookahead
2. **The tokenizer does the heavy lifting** - context-sensitive tokenization (`:paren_identifier`, `:bracket_identifier`, `:op_identifier`, `:do_identifier`, etc.) resolves most ambiguities that would otherwise need lookahead
3. **`peek_n` exists and is already implemented** in `TokenAdapter.peek_n/2` with a configurable `max_peek` (default 4)
4. **Current uses of `checkpoint/rewind` are appropriate** for variable-length decisions (stab clause detection)
5. **`peek_n` would improve code clarity** in 3-5 specific locations as a refactoring opportunity, not a functional requirement

## Grammar Analysis

### LALR(1) Design Principles in elixir_parser.yrl

The Elixir grammar explicitly declares:

```erlang
%% Two shift/reduce conflicts coming from call_args_parens and
%% one coming from empty_paren on stab.
Expect 3.
```

These 3 conflicts are managed via precedence declarations, not extra lookahead. The grammar achieves LALR(1) through:

1. **Token pre-classification**: Identifiers tokenized as different kinds based on what follows
   - `paren_identifier`: identifier immediately followed by `(`
   - `bracket_identifier`: identifier immediately followed by `[`
   - `do_identifier`: identifier followed by `do` keyword
   - `op_identifier`: identifier followed by unary operator-like token

2. **Operator precedence tables**: Lines 69-97 define 23 precedence levels

3. **Separate nonterminals for matched/unmatched/no_parens contexts**: The tripartite expression classification (lines 112-179) avoids grammar conflicts

### Would Any Grammar Rule Benefit from peek_n(2+)?

No grammar rule *requires* peek_n. Here's why each potentially ambiguous construct works with peek_n(1):

| Construct | Why peek(1) suffices |
|-----------|---------------------|
| `foo(...)` vs `foo (...)` | Tokenizer emits `:paren_identifier` vs `:identifier` |
| `foo[...]` vs `foo [...]` | Tokenizer emits `:bracket_identifier` vs `:identifier` |
| `foo do...end` vs `foo` + `do...end` | Tokenizer emits `:do_identifier` + explicit spacing rules |
| `foo -1` (no-parens call) vs `foo - 1` (binary) | Tokenizer emits `:op_identifier` based on whitespace |
| `not in` (compound operator) | Tokenizer emits `:in_op` with value `{:"not in", in_location}` |
| `"foo":` (quoted kw) vs `"foo"` (string) | String parser returns special `:keyword_key` tuple |
| `() ->` vs `()` | After consuming `()`, peek for `:stab_op` |

## Current ToxicParser Implementation

### TokenAdapter.peek_n/2 Already Exists

```elixir
# lib/toxic_parser/token_adapter.ex lines 76-90
@spec peek_n(State.t(), pos_integer()) ::
        {:ok, [token()], State.t()}
        | {:eof, [token()], State.t()}
        | {:error, Error.t(), [token()], State.t()}
def peek_n(%State{max_peek: max} = _state, n) when n > max do
  raise ArgumentError, "peek_n/2 capped at #{max}"
end

def peek_n(%State{} = state, n) do
  case ensure_lookahead(state, n) do
    {:ok, state} -> {:ok, Enum.take(state.lookahead, n), state}
    {:eof, state} -> {:eof, [], state}
    {:error, err, state} -> {:error, err, [], state}
  end
end
```

### Where checkpoint/rewind Is Currently Used

The parser uses `checkpoint/rewind` in these locations:

1. **`Blocks.try_parse_clause/4`** (blocks.ex:203-216)
   - Attempts to parse stab clause, rewinds if not a stab
   - Variable-length decision (pattern + optional guard + `->`)

2. **`Containers.try_parse_stab_or_expr/5`** (containers.ex:501-518)
   - Tries stab clause parsing in parentheses
   - Falls back to expression if no `->` found

3. **`Containers.try_parse_stab_parens_many/5`** (containers.ex:183-246)
   - Detects `(() -> ...)`, `(() when g -> ...)`, `((args) -> ...)`
   - Checkpoints to rewind if inner parens aren't a stab pattern

4. **`Containers.parse_stab_patterns/4`** (containers.ex:706-754)
   - Distinguishes `(args) -> ...` from `(parenthesized_expr) op ...`
   - Checkpoints to try stab_parens_many interpretation

5. **`Containers.check_and_collect_stab_body/5`** (containers.ex:1011-1042)
   - Determines if content after EOE starts a new stab clause
   - Used for multi-clause fn/case bodies

6. **`Containers.parse_tuple_args/3`** (containers.ex:1599-1626)
   - Tries quoted keyword parsing for tuple elements
   - Falls back to expression if not a keyword

## Locations Where peek_n Would Improve Clarity

### 1. Inner-Parens Stab Detection (containers.ex:183-246)

**Current code pattern:**
```elixir
# Checkpoint
{ref, checkpoint_state} = TokenAdapter.checkpoint(state)

# Consume inner (
{:ok, inner_open_tok, inner_state} = TokenAdapter.next(checkpoint_state)
# Skip EOE
inner_state = skip_eoe(inner_state)

case TokenAdapter.peek(inner_state) do
  {:ok, %{kind: :")"} = inner_close_tok, _} ->
    {:ok, _close, inner_state} = TokenAdapter.next(inner_state)
    # Check what follows
    case TokenAdapter.peek(inner_state) do
      {:ok, %{kind: :stab_op}, _} -> # (() -> ...)
      {:ok, %{kind: :when_op}, _} -> # (() when ...)
      _ -> rewind and parse as expression
    end
```

**Could be simplified with peek_n(2):**
```elixir
# After consuming inner (, skip EOE, then:
case TokenAdapter.peek_n(inner_state, 2) do
  {:ok, [%{kind: :")"}, %{kind: :stab_op}], _} ->
    # Empty paren stab: () ->
  {:ok, [%{kind: :")"}, %{kind: :when_op}], _} ->
    # Empty paren with guard: () when ...
  _ ->
    # Not a stab pattern, continue as expression
end
```

**Benefit**: Eliminates one checkpoint/rewind cycle for the common case of empty-paren stabs.

### 2. Trailing Comma Detection (keywords.ex:99-116)

**Current code pattern:**
```elixir
{:ok, %{kind: :","}, _} ->
  {:ok, _comma, state} = TokenAdapter.next(state)
  case TokenAdapter.peek(state) do
    {:ok, %{kind: kind}, _} when kind in [:eoe, :")", :"]", :"}", :">>"] ->
      # Trailing comma - stop
    _ ->
      # More keywords
  end
```

**Could use peek_n(2) to avoid consuming comma before checking:**
```elixir
case TokenAdapter.peek_n(state, 2) do
  {:ok, [%{kind: :","}, %{kind: kind}], _} when kind in [:eoe, :")", :"]", :"}", :">>"] ->
    # Trailing comma - consume and stop
  {:ok, [%{kind: :","}, _], _} ->
    # Comma with more content - consume and continue
  _ ->
    # No comma - stop
end
```

**Benefit**: Clearer intent, single decision point.

### 3. Bracket Arg Trailing Comma (calls.ex:161-209)

Similar pattern in `parse_bracket_arg_no_skip/3` where comma is consumed before checking if it's trailing.

### 4. Map Update vs Entry Detection (Not Currently Using checkpoint)

The `Maps.parse_map/4` module doesn't currently use checkpoint for map update detection because it parses the first expression, then checks for `|`. This is already optimal, but demonstrates that not all ambiguities need checkpoint.

## What peek_n Cannot Solve

### Variable-Length Pattern + Guard + Stab Detection

The core stab clause ambiguity cannot be solved by any fixed N:

```elixir
# These are all valid stab patterns:
fn -> :ok end                    # 0 patterns
fn x -> x end                    # 1 pattern  
fn x, y -> x + y end             # 2 patterns
fn x, y, z -> x + y + z end      # N patterns
fn x when is_atom(x) -> x end    # pattern + guard
```

To detect "is this a stab clause?", we must parse patterns until we see `->` or determine it's not a stab. This requires checkpoint/rewind, not fixed lookahead.

### Quoted Keyword Keys with Interpolation

```elixir
["#{key}": value]  # Variable-length interpolation
```

The string must be fully parsed to determine if it ends with `:`. The `Strings.parse/4` module handles this by returning special `:keyword_key` or `:keyword_key_interpolated` tuples.

## Performance Considerations

### peek_n vs checkpoint/rewind

| Aspect | peek_n(N) | checkpoint/rewind |
|--------|-----------|-------------------|
| Time complexity | O(N) to fill lookahead | O(N) tokens to parse |
| Memory | Tokens buffered | AST nodes + tokens |
| Backtrack cost | Already buffered | Must re-parse |
| Best use case | Fixed-size patterns | Variable-length decisions |

For the stab clause detection in `Blocks.try_parse_clause/4`, checkpoint/rewind is actually the right choice because:
1. The decision boundary is variable ("parse until you see `->` or don't")
2. If it IS a stab clause, we need the parsed patterns anyway
3. If it ISN'T, we rewind a typically short distance

### Recommendation for peek_until/peek_while

A `peek_until(predicate, limit)` or `peek_while(predicate, limit)` API could help for bounded scans like:

```elixir
# Skip EOE and count, returning first non-EOE token
peek_while(state, fn tok -> tok.kind == :eoe end, 10)
```

This would unify the many `skip_eoe_count_newlines` helpers, but isn't a priority.

## Recommendations

### Immediate Actions (Low Effort, High Clarity)

1. **Do nothing** - The current implementation is correct and performant. The checkpoint/rewind pattern is appropriate for the variable-length decisions it handles.

### Optional Refactoring Opportunities

If code clarity is prioritized over minimal changes:

1. **Inner-parens stab detection** in `Containers.try_parse_stab_parens_many/5`: Use peek_n(2) to check for `() ->` or `() when` pattern before checkpointing. This eliminates one checkpoint cycle for empty-paren stabs.

2. **Trailing comma checks**: Replace consume-then-peek with peek_n(2) in keyword and container parsing for clearer control flow.

3. **Add `skip_eoe_peek/1` helper**: A single function that skips EOE tokens and returns the next non-EOE token would simplify many locations.

### Non-Recommendations

1. **Don't add peek_until/peek_while** unless there's a concrete performance or correctness issue. The current helpers are sufficient.

2. **Don't replace checkpoint/rewind for stab detection** with multi-token lookahead. The variable-length nature of patterns makes checkpoint/rewind the correct approach.

3. **Don't increase max_peek beyond 4** - no grammar pattern requires it, and larger buffers add memory pressure.

## Would `peek_until(predicate)` Help for Variable-Length Cases?

### The Question

For variable-length lookahead decisions (like stab clause detection), would a `peek_until(predicate)` operation in Toxic be better than the current `checkpoint/rewind` strategy?

### Toxic's Current Architecture

From examining `/Users/lukaszsamson/claude_fun/toxic/lib/toxic.ex`, Toxic provides:

```elixir
# Checkpoint/rewind (lines 353-408)
@spec checkpoint(t()) :: {reference(), t()}
def checkpoint(%__MODULE__{} = stream) do
  ref = make_ref()
  Process.put({__MODULE__, :checkpoint, ref},
    {stream.push, stream.buffer, stream.driver, stream.error, stream.eof,
     stream.last_emitted_entry, stream.source, stream.driver_source})
  {ref, stream}
end

@spec rewind_to(t(), reference(), boolean()) :: t()
def rewind_to(%__MODULE__{} = stream, ref, delete_checkpoint? \\ true)
```

Key characteristics:
1. **Stateful tokenizer**: The `driver` maintains mutable state (terminator stack, indentation, string context)
2. **Buffer-based streaming**: Uses `:queue` for token buffer with configurable `max_batch` (default 16)
3. **Terminator tracking**: Each buffered token includes `{token, pre_terms, pre_pos}` where `pre_terms` is the terminator stack snapshot
4. **Process dictionary storage**: Checkpoints stored in process dictionary for simplicity

### Analysis: `peek_until` vs `checkpoint/rewind`

#### What `peek_until` Would Do

A hypothetical `peek_until(stream, predicate, opts)` would:
1. Scan tokens forward without consuming them
2. Fill the lookahead buffer until `predicate` returns true or a limit is reached
3. Return the scanned tokens and updated stream

```elixir
# Hypothetical API
@spec peek_until(t(), (token() -> boolean()), keyword()) :: 
  {:found, [token()], t()} | {:not_found, [token()], t()} | {:limit, [token()], t()}
```

#### Comparison Table

| Aspect | `peek_until(predicate)` | `checkpoint/rewind` |
|--------|------------------------|---------------------|
| **Time complexity** | O(N) tokens scanned | O(N) tokens parsed + O(N) rewound |
| **Memory** | Tokens in buffer only | Tokens + AST nodes + checkpoint state |
| **What's preserved** | Buffer only | Buffer + driver state + terminators |
| **Backtrack cost** | Zero (tokens already buffered) | Restore from process dict |
| **Nesting awareness** | Requires depth tracking in predicate | Free (driver maintains terminator stack) |
| **Best for** | Simple token patterns | Structural decisions requiring parsing |

#### The Critical Problem: Nesting Awareness

For stab clause detection, we need to find `->` at the *correct nesting level*. Consider:

```elixir
fn (x, fn y -> y end) -> x end
#           ^-- inner stab, NOT what we're looking for
#                        ^-- outer stab, THIS is what we want
```

A naive `peek_until(fn tok -> tok.kind == :stab_op end)` would incorrectly stop at the inner `->`.

**Solution requires depth tracking:**

```elixir
# Would need a stateful predicate or reducer
peek_until(stream, fn tok, depth ->
  case tok.kind do
    :"(" -> {:continue, depth + 1}
    :")" -> {:continue, depth - 1}
    :fn -> {:continue, depth + 1}
    :end -> {:continue, depth - 1}
    :stab_op when depth == 0 -> {:stop, :found}
    _ -> {:continue, depth}
  end
end, initial_depth: 0, limit: 100)
```

But this is incomplete! It doesn't handle:
- String interpolation containing `->`: `"#{a -> b}"`
- Heredocs and sigils with `->` in content
- Comments containing `->`
- `when` operators that are part of guards vs binary operators

**The terminator stack in Toxic already tracks this**, but it's maintained incrementally as tokens are consumed, not just scanned.

#### Why `checkpoint/rewind` Is Actually Better for Stab Detection

1. **Terminator stack is accurate**: Toxic's driver maintains `pre_terms` with each token, capturing the exact nesting context. `peek_until` would need to replicate this logic.

2. **Parser context matters**: The decision "is this a stab clause?" depends on *parsed structure*, not just token kinds:
   ```elixir
   fn x = y -> x end    # Valid: pattern x = y, then ->
   fn x = -> x end      # Invalid: = expects RHS
   ```
   You can't determine validity from token scan alone.

3. **Work reuse**: If checkpoint/rewind determines "yes, it's a stab", the parsed patterns are already available. With `peek_until`, you'd scan tokens, make a decision, then parse them again.

4. **Error recovery**: If parsing fails, checkpoint/rewind can restore to a known good state. `peek_until` provides no parsed state to fall back to.

### When Would `peek_until` Be Useful?

Despite the above, `peek_until` would help in these scenarios:

#### 1. Quoted Keyword Key Detection

Currently, `Strings.parse/4` must fully parse a string to determine if it ends with `:`. A `peek_until` could scan for:
- String end token
- Followed by `:` token (indicating keyword key)

This is simpler than stab detection because strings don't contain arbitrary nesting.

```elixir
case peek_until(state, fn tok -> tok.kind in [:bin_string_end, :list_string_end] end, limit: 50) do
  {:found, tokens, state} ->
    # Check if next token after string end is `:` 
    # (indicating keyword key syntax like "foo":)
    ...
end
```

**Caveat**: Interpolated strings have variable-length content, so this still needs careful handling.

#### 2. EOE Skipping with Lookahead

Many places in the parser have:
```elixir
state = skip_eoe(state)
case TokenAdapter.peek(state) do
  ...
end
```

A `peek_past(predicate)` could combine these:
```elixir
case peek_past(state, fn tok -> tok.kind == :eoe end) do
  {:ok, next_non_eoe_token, state} -> ...
end
```

This would be simpler than `peek_until` and immediately useful.

#### 3. Bounded Delimiter Matching

For constructs like `%{}` (empty map) vs `%{...}` (map with content), `peek_n(2)` already suffices. But for checking "is there a `|` before the first `}`", `peek_until` could help:

```elixir
# Map update detection: %{expr | ...}
peek_until(state, fn tok -> tok.kind in [:|, :"}"] end, limit: 20)
```

### Implementation Recommendation

If `peek_until` is added to Toxic, it should:

1. **Support a reducer, not just a predicate**, to track depth:
   ```elixir
   @spec peek_until(t(), (token(), acc -> {:continue, acc} | {:stop, result}), acc, keyword()) ::
     {:found, result, [token()], t()} | {:limit, acc, [token()], t()}
   ```

2. **Use the existing buffer**, not re-lex:
   ```elixir
   def peek_until(%__MODULE__{} = stream, reducer, initial_acc, opts) do
     limit = Keyword.get(opts, :limit, 100)
     stream = ensure_buffer_size(stream, limit)
     # Scan buffer without consuming...
   end
   ```

3. **Respect terminator stack** by including `pre_terms` in tokens passed to reducer.

4. **Be bounded** with a hard limit (default 100) to prevent runaway scans.

### Practical Conclusion

| Use Case | Best Approach |
|----------|---------------|
| Stab clause detection | `checkpoint/rewind` (needs parsed structure) |
| Empty-paren stab `() ->` | `peek_n(2)` (fixed pattern) |
| Quoted keyword key | `peek_until` could help, but `Strings.parse` works |
| EOE skipping with lookahead | `peek_past` helper (specialized) |
| Map update `%{x \| ...}` | Parse first expr, then check for `\|` |

**Recommendation**: 

1. **Don't add `peek_until` for stab detection** - `checkpoint/rewind` is correct and doesn't waste work.

2. **Consider adding a simpler `peek_past_eoe/1`** helper in `TokenAdapter` that skips EOE tokens and peeks the next non-EOE token. This would clean up many call sites.

3. **If `peek_until` is added**, make it a reducer-based API with depth tracking support, and document that it's for token-level decisions only, not structural parsing.

## Conclusion

The Elixir grammar is cleverly designed to be LALR(1) through:
1. Context-sensitive tokenization (whitespace affects token kinds)
2. Operator precedence declarations
3. Separate expression nonterminals for matched/unmatched/no_parens contexts

ToxicParser correctly mirrors this design. The existing `peek_n/2` API is sufficient for any fixed-lookahead needs. The checkpoint/rewind mechanism handles variable-length decisions appropriately.

**For the stab clause detection problem specifically**, `peek_until` would NOT be better than `checkpoint/rewind` because:
- Stab detection requires nesting awareness (parentheses, fn/end blocks, strings)
- The decision depends on parsed structure, not just token patterns
- Parsed patterns are reused when the clause is valid
- Toxic's terminator stack is maintained incrementally during consumption, not scanning

**No changes are required.** Optional refactoring could improve code clarity in 2-3 locations by using `peek_n(2)` for two-token pattern matching, but this is a stylistic preference, not a functional requirement.
