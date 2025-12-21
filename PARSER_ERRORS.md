# Parser errors emitted by `elixir_parser.yrl`

This document lists **parser errors (i.e. errors explicitly emitted by the grammar actions in** `lib/elixir/src/elixir_parser.yrl`**) and minimal snippets that reproduce them**.

Notes:

* Most entries can be reproduced with `Code.string_to_quoted/2`.
* “Covered by” references existing tests in `lib/elixir/test/elixir/kernel/parser_test.exs`.

---

## 1) `literal_encoder` error while encoding a literal

**Grammar site:** `handle_literal/3` (`return_error(..., Reason ++ ": ", "literal")`)

**Error tuple shape (example):**

```elixir
{:error, {[line: 1, column: 1], "ENCODER_FAIL: ", "literal"}}
```

**Repro (terminal):**

```elixir
Code.string_to_quoted("1",
  literal_encoder: fn _literal, _meta ->
    {:error, "ENCODER_FAIL"}
  end
)
```

**Covered by:** none found (reproduced via terminal).

---

## 2) Misplaced range step operator (`//` must follow `..`)

**Grammar site:** `build_op/3` (special-case for `//`)

**Message starts with:**

> the range step operator (//) must immediately follow the range definition operator (..)

**Repro:**

```elixir
Code.string_to_quoted!("foo++bar//bat")
```

**Covered by:** `parser_test.exs` “ternary ops / errors” (around line 100)

Other covered examples:

```elixir
Code.string_to_quoted!("foo..bar baz//bat")
Code.string_to_quoted!("foo..(bar//bat)")
```

---

## 3) Atom cannot be followed by an alias (`:foo.Bar`)

**Grammar site:** `build_dot_alias/3` → `error_bad_atom/1`

**Message:**

> atom cannot be followed by an alias. If the '.' was meant to be part of the atom's name, the atom name must be quoted. Syntax error before: '.'

**Repro:**

```elixir
Code.string_to_quoted!(":foo.Bar")
```

**Covered by:** `parser_test.exs` “invalid atom dot alias” (around line 1342)

---

## 4) `fn` without `->` clauses

**Grammar site:** `build_fn/3` (block form where `->` is expected)

**Message:**

> expected anonymous functions to be defined with -> inside: 'fn'

**Repro:**

```elixir
Code.string_to_quoted!("fn 1 end")
```

**Covered by:** `parser_test.exs` “invalid fn” (around line 983)

---

## 5) Unexpected `->` (stab) operator placement

**Grammar site:** `check_stab/2` → `error_invalid_stab/1`

**Message starts with:**

> unexpected operator ->. If you want to define multiple clauses, the first expression must use ->.

**Repro:**

```elixir
Code.string_to_quoted!("""
fn 1
2 -> 3 end
""")
```

**Covered by:** `parser_test.exs` “invalid fn” (around line 983)

---

## 6) Unexpected keyword list inside a tuple

**Grammar site:** `bad_keyword/3`

**Message starts with:**

> unexpected keyword list inside tuple.

**Repro:**

```elixir
Code.string_to_quoted!("{foo: :bar}")
```

**Covered by:** `parser_test.exs` “invalid keyword list in tuple/binary” (around line 1167)

---

## 7) Unexpected keyword list inside a bitstring

**Grammar site:** `bad_keyword/3`

**Message starts with:**

> unexpected keyword list inside bitstring.

**Repro:**

```elixir
Code.string_to_quoted!("<<foo: :bar, baz: :bar>>")
```

**Covered by:** `parser_test.exs` “invalid keyword list in tuple/binary” (around line 1167)

---

## 8) Expression after keyword list (keyword list must be last argument in a call)

**Grammar site:** `maybe_bad_keyword_call_follow_up/3`

**Message starts with:**

> unexpected expression after keyword list. Keyword lists must always come as the last argument.

**Repro:**

```elixir
Code.string_to_quoted!("call foo: 1, :bar")
```

**Covered by:** `parser_test.exs` “expression after keyword lists” (around line 1184)

Other covered example:

```elixir
Code.string_to_quoted!("call(foo: 1, :bar)")
```

---

## 9) Expression after keyword list in lists/maps (keyword list must be last entry)

**Grammar site:** `maybe_bad_keyword_data_follow_up/3`

**Message starts with:**

> unexpected expression after keyword list. Keyword lists must always come last in lists and maps.

**Repro:**

```elixir
Code.string_to_quoted!("[foo: 1, :bar]")
```

**Covered by:** `parser_test.exs` “expression after keyword lists” (around line 1184)

Other covered example:

```elixir
Code.string_to_quoted!("%{foo: 1, :bar => :bar}")
```

---

## 10) Unexpected parentheses due to space before `(`

**Grammar site:** `error_no_parens_strict/1`

**Message starts with:**

> unexpected parentheses. If you are making a function call, do not insert spaces between the function name and the opening parentheses.

**Repro:**

```elixir
Code.string_to_quoted!("foo (hello, world)")
```

**Covered by:** `parser_test.exs` “invalid parens call” (around line 1309)

---

## 11) Unexpected comma: nested calls without parentheses are ambiguous

**Grammar site:** `error_no_parens_many_strict/1`

**Message starts with:**

> unexpected comma. Parentheses are required to solve ambiguity in nested calls.

**Repro:**

```elixir
Code.string_to_quoted!("foo 1, foo 2, 3")
```

**Covered by:** `parser_test.exs` “invalid nested no parens call” (around line 1321)

---

## 12) Unexpected comma inside containers (lists/tuples/etc)

**Grammar site:** `error_no_parens_container_strict/1`

**Message starts with:**

> unexpected comma. Parentheses are required to solve ambiguity inside containers.

**Repro:**

```elixir
Code.string_to_quoted!("[foo 1, 2]")
```

**Covered by:** `parser_test.exs` “invalid nested no parens call” (around line 1321)

---

## 13) Too many arguments in access syntax (`value[...]`)

**Grammar site:** `error_too_many_access_syntax/1`

**Message starts with:**

> too many arguments when accessing a value.

**Repro:**

```elixir
Code.string_to_quoted!("foo[1, 2]")
```

**Covered by:** `parser_test.exs` “invalid access” (around line 1223)

---

## 14) Invalid keyword identifier (`do:` special case)

**Grammar site:** `error_invalid_kw_identifier/1` (special-cases `do`)

**Message:**

> unexpected keyword: do:.

**Repro:**

```elixir
Code.string_to_quoted!("if true do:\n")
```

**Covered by:** `parser_test.exs` “invalid do” (around line 1300)

---

## 15) Invalid keyword identifier (generic `...:`)

**Grammar site:** `error_invalid_kw_identifier/1` (generic branch)

**Message shape:**

> syntax error before: 'else:'

**Repro (terminal):**

```elixir
Code.string_to_quoted("if true else: 1")
```

**Covered by:** none found (reproduced via terminal).

---

## 16) Unicode conversion error while building a list string

**Grammar site:** `build_list_string/2` (catches `Elixir.UnicodeConversionError` and emits `return_error(..., Message, "'" )`)

**Notes:** this is defensive against invalid encodings during charlist literal processing; a direct reproduction via `Code.string_to_quoted/2` was not found because `Code.string_to_quoted/2` rejects non-UTF-8 source binaries before parsing.
