# Parser warnings emitted by `elixir_parser.yrl`

This document lists **warnings explicitly emitted by the grammar/actions** in `lib/elixir/src/elixir_parser.yrl` and minimal snippets that reproduce them.

Notes:

* You can reproduce warnings via `Code.with_diagnostics/1` (recommended) so warnings are captured rather than printed.
* “Covered by” references existing tests in:
  * `lib/elixir/test/elixir/kernel/warning_test.exs`
  * `lib/elixir/test/elixir/kernel/parser_test.exs`

---

## 1) Deprecation: `not expr1 in expr2`

**Grammar site:** `build_op/3` (special-case rearrangement for unary `not` + `in`)

**Warning message:**

> "not expr1 in expr2" is deprecated, use "expr1 not in expr2" instead

**Repro (terminal, captures diagnostics):**

```elixir
Code.with_diagnostics(fn ->
  Code.string_to_quoted("not left in right")
end)
```

**Covered by:** `warning_test.exs` "deprecated not left in right" (around line 1128)

---

## 2) Ambiguous pipe: missing parentheses when piping into a function call

**Grammar site:** `warn_pipe/2` (called from `matched_op_expr` / `no_parens_op_expr` for `|>` and friends)

**Warning message starts with:**

> parentheses are required when piping into a function call.

**Repro:**

```elixir
Code.string_to_quoted!("""
[5, 6, 7, 3]
|> Enum.map_join \"\", &(Integer.to_string(&1))
|> String.to_integer
""")
```

**Covered by:** `warning_test.exs` “pipe without explicit parentheses” (around line 1922)

---

## 3) Missing parentheses after operator (do/end + operator + no-parens call)

**Grammar site:** `warn_no_parens_after_do_op/1`

**Warning message starts with:**

> missing parentheses on expression following operator "...", you must add parentheses to avoid ambiguities

**Repro:**

```elixir
Code.string_to_quoted!("""
quote do
  case do
  end || raise 1, 2
end
""")
```

**Covered by:** `warning_test.exs` “do+end with operator without explicit parentheses” (around line 1944)

---

## 4) Missing parentheses inside keywords (keyword value is a no-parens call)

**Grammar site:** `warn_nested_no_parens_keyword/2`

**Warning message starts with:**

> missing parentheses for expression following "...:" keyword. Parentheses are required to solve ambiguity inside keywords.

**Repro:**

```elixir
Code.string_to_quoted!("""
quote do
  IO.inspect arg, label: if true, do: \"foo\", else: \"baz\"
end
""")
```

**Covered by:** `warning_test.exs` “keywords without explicit parentheses” (around line 1933)

---

## 5) Trailing comma inside call arguments

**Grammar site:** `warn_trailing_comma/1` (triggered by `kw_call -> kw_base ','`)

**Warning message:**

> trailing commas are not allowed inside function/macro call arguments

**Repro:**

```elixir
Code.string_to_quoted!("Keyword.merge([], foo: 1,)")
```

**Covered by:** `warning_test.exs` “warnings on trailing comma on call” (around line 2188)

---

## 6) Empty parentheses expression `()`

**Grammar site:** `warn_empty_paren/1` (triggered by `access_expr -> empty_paren`)

**Warning message starts with:**

> invalid expression (). If you want to invoke or define a function, make sure there are no spaces between the function name and its arguments.

**Repro (terminal, captures diagnostics):**

```elixir
Code.with_diagnostics(fn ->
  Code.string_to_quoted("()")
end)
```

**Covered by:** none found.

---

## 7) Empty `->` clause body (right side of `->` is required)

**Grammar site:** `warn_empty_stab_clause/1` (triggered by `stab_op_eol_and_expr -> stab_op_eol`)

**Warning message starts with:**

> an expression is always required on the right side of ->. Please provide a value after ->

**Repro (terminal, captures diagnostics):**

```elixir
Code.with_diagnostics(fn ->
  Code.string_to_quoted("fn x -> end")
end)
```

**Covered by:** none found.
