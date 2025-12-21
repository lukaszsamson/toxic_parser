# String Lookahead Proposal for Quoted Keyword Detection

## Problem

When parsing, we encounter quoted strings that might be keyword keys:
- `"foo": 1` - keyword key (ends with `:kw_identifier_safe_end`)
- `"foo"` - regular string (ends with `:bin_string_end`)

Currently, we checkpoint and try to parse, then rewind if it's not a keyword.

## Token Structure

| Start Token | Regular End | Keyword End |
|-------------|-------------|-------------|
| `:bin_string_start` | `:bin_string_end` | `:kw_identifier_safe_end` / `:kw_identifier_unsafe_end` |
| `:list_string_start` | `:list_string_end` | `:kw_identifier_safe_end` / `:kw_identifier_unsafe_end` |

For interpolated strings, we also see `:begin_interpolation` ... `:end_interpolation` pairs.

## Proposed Lookahead

```elixir
@doc """
Checks if a quoted string (starting at current position) is a keyword key.
Returns {:keyword, state} or {:not_keyword, state} without consuming tokens.

Uses pushback to restore all scanned tokens.
"""
@spec quoted_string_is_keyword?(State.t()) :: {:keyword | :not_keyword, State.t()}
def quoted_string_is_keyword?(state) do
  scan_to_string_end(state, [], 0)
end

# Scan until we find the string end token, tracking interpolation depth
defp scan_to_string_end(state, consumed, interp_depth) do
  case TokenAdapter.next(state) do
    {:ok, %{kind: kind} = tok, state} ->
      consumed = [tok | consumed]

      cond do
        # Entering interpolation
        kind == :begin_interpolation ->
          scan_to_string_end(state, consumed, interp_depth + 1)

        # Exiting interpolation
        kind == :end_interpolation ->
          scan_to_string_end(state, consumed, interp_depth - 1)

        # Inside interpolation - keep scanning
        interp_depth > 0 ->
          scan_to_string_end(state, consumed, interp_depth)

        # Found keyword end (with colon) at top level
        kind in [:kw_identifier_safe_end, :kw_identifier_unsafe_end] ->
          state = TokenAdapter.pushback_many(state, Enum.reverse(consumed))
          {:keyword, state}

        # Found regular string end at top level
        kind in [:bin_string_end, :list_string_end] ->
          state = TokenAdapter.pushback_many(state, Enum.reverse(consumed))
          {:not_keyword, state}

        # Other tokens (string content, heredoc parts, etc.) - keep scanning
        true ->
          scan_to_string_end(state, consumed, interp_depth)
      end

    {:eof, state} ->
      state = TokenAdapter.pushback_many(state, Enum.reverse(consumed))
      {:not_keyword, state}

    {:error, _diag, state} ->
      state = TokenAdapter.pushback_many(state, Enum.reverse(consumed))
      {:not_keyword, state}
  end
end
```

## Usage in Keywords Module

Replace checkpoint-based detection with lookahead:

```elixir
# In try_parse_kw_data/4, try_parse_kw_call/4, etc.

case TokenAdapter.peek(state) do
  {:ok, tok, state} ->
    cond do
      starts_kw?(tok) ->
        # Regular kw_identifier - parse directly
        parse_kw_data(state, ctx, log)

      tok.kind in @quoted_kw_start ->
        # Check if quoted string is a keyword key
        case quoted_string_is_keyword?(state) do
          {:keyword, state} ->
            # Definitely a keyword - parse directly, no checkpoint
            parse_kw_data(state, ctx, log)

          {:not_keyword, state} ->
            # Not a keyword - skip without parsing
            {:no_kw, state, log}
        end

      true ->
        {:no_kw, state, log}
    end
```

## Complexity Analysis

- **Simple strings** (`"foo":`): 2-3 token lookahead (start, content, end)
- **Empty strings** (`"":`): 2 token lookahead (start, end)
- **Interpolated** (`"foo#{x}bar":`): Scan through interpolation, ~5-10 tokens typical
- **Heredocs**: Larger but still linear

## Expected Impact

- Eliminates ~9,000 checkpoints for quoted key detection
- Total checkpoint reduction: ~50% (from 18,990 to ~10,000)
- No more rewinds for quoted key false positives

## Edge Cases

1. **Nested interpolations**: `"a#{b#{c}d}e":` - handled by depth tracking
2. **Heredocs**: Same token structure, just more content tokens
3. **Sigils**: Not valid as keyword keys, handled elsewhere
4. **Errors in string**: Return `:not_keyword` on error

## Alternative: Lexer-Level Solution

The lexer could emit a flag on string start tokens indicating whether the string ends with a colon. This would be even faster (no lookahead needed) but requires lexer changes.
