# Conformance Tests Expansion Plan

Goal: extend `test/conformance_test.exs` to cover more of `elixir_parser.yrl`, including error/warning branches and under-tested valid combinations. Prioritize high-signal additions that guard parser parity.

## Harness updates (prereqs)
- Add `assert_error/2` (compare `{:error, reason}` between reference and ToxicParser) and use `ToxicParser.parse_string/2` with `mode: :strict`. Normalize error shapes if needed.
- Add `assert_warns/3` (or option on `assert_conforms/2`) that runs reference with `emit_warnings: true` and checks warning messages/presence, not just AST.
- Fill the placeholder `do_block -> do_eoe stab_eoe 'end'` test; add at least one canonical case.

## High-priority new tests (errors)
- `error_no_parens_strict` / `error_no_parens_many_strict`: `foo (a: 1)`, `foo (1, 2)`, `foo (1, a: 2)`, `foo(bar 1, 2)`, `foo(bar 1, 2, baz 3)`.
- `error_no_parens_container_strict`: `[foo 1, 2, 3]`, `{foo 1, 2, 3}`, `%{foo 1, 2 => 3}`, `<<foo 1, 2>>`.
- `error_too_many_access_syntax`: `foo[1, 2]`, `foo[1, 2, 3]`, `foo.bar[1, 2]`, `@foo[1, 2]`.
- `bad_keyword` in tuple/bitstring: `{a: 1}`, `{a: 1, b: 2}`, `<<a: 1>>`, `<<a: 1, b: 2>>`.
- `error_invalid_kw_identifier`: `foo do:`, `foo.bar do:`, `(1) do:`, `[1] do:`.
- `error_bad_atom`: `:foo.Bar`, `:foo.Bar.Baz`.
- `error_invalid_stab`: `fn 1\n2 -> 3 end`, `(1; 2 -> 3)`.
- Range step misuse: `1 // 2`, `a // b`, `foo() // bar()`.
- `maybe_bad_keyword_*` follow-ups: `foo(a: 1, 2)`, `foo a: 1, 2`, `[a: 1, 2]`, `%{a: 1, 2}`.
- `not in` rewrite edge: `!a in b`, `not foo() in bar()`, `!x in [1, 2, 3]`, `not (a + b) in c`.

## Warning coverage (requires `emit_warnings: true`)
- `warn_pipe`: `1 |> foo bar 1, 2`, `1 ~> foo bar 1, 2`, `1 |> foo bar 1, 2 |> baz`.
- `warn_no_parens_after_do_op`: `if a do b end == foo 1, 2`, `case x do _ -> y end + bar 1, 2`.
- `warn_nested_no_parens_keyword`: `foo(a: bar 1, 2)`, `[a: bar 1, 2]`, `%{a: bar 1, 2}`, `foo a: bar 1, 2`.
- `warn_trailing_comma`: `foo a: 1,`, `fn (a: 1,) -> a end`, `[a: 1,]`.
- `warn_empty_paren`: `foo((), ())`, `[()]`, `{()}`, `1 + ()`.
- `warn_empty_stab_clause`: `fn a -> end`, `fn a, b -> end`, `(->)`, `(() ->)`, `case x do _ -> end`.

## Valid edge combinations to add
- `stab_parens_many` with guards/nesting: `fn (a, b, c: 1) when guard -> body end`, `fn ((nested)) -> body end`, `fn ({:ok, x}, {:error, y}) when x > y -> x end`.
- `call_args_no_parens_kw_expr` nested calls: `foo a: bar 1, 2`, `foo a: bar 1, 2, b: baz 3`, `if a: bar 1, 2 do end`.
- Arrow ops with no-parens RHS: `a ~> foo 1`, `a <~ foo 1`, `a <<< foo 1`, `a >>> foo 1`, `a <~> foo 1`.
- Map update bases (`assoc_update`/`assoc_update_kw`): `%{foo() | a => b}`, `%{Mod.func() | a => b}`, `%{@attr | a: 1}`, `%{x.y.z | a: 1}`, `%{if foo do bar end | a: 1}`, `%{struct | a => 1, b: 2, c => 3}`.
- `map_base_expr` with chained unary/dot: `%@!foo{}`, `%-@foo{}`, `%...@foo{}`.
- `dot_container` variants: `Foo.{A, b: 1}`, `Foo.{if a do b end}`, `Foo.{A, B, c: 1, d: 2}`, `foo.bar.{A, B, C}`, `1.{A}`, `(a + b).{C}`.
- Quoted identifiers: `foo."bar baz"()`, `foo."bar\nbaz" 1, 2`, `foo."bar"[:key]`, `foo."bar" do end`, `@"attr"`, `&"func"/1`.
- Bitstring modifiers: `<<x::size(if a do 8 else 16 end)>>`, `<<x::size(foo 1, 2)>>`, `<<x::(if a do binary else integer end)>>`, `<<@attr::binary>>`, `<<foo()::8-unit(4)>>`.
- `container_args_base` with multiple `unmatched_expr`: `[if a do 1 end, if b do 2 end]`, `{if a do 1 end, if b do 2 end, if c do 3 end}`, `%{if a do :key end => if b do :val end}`, `<<if a do 1 end>>`.
- Block/keyword forms: `if true, do: :ok`, `if true, do: :ok, else: :error`, `with {:ok, x} <- foo(), do: x, else: (_ -> :error)`.
- Block list combos: `try do :ok rescue e -> e catch :throw, x -> x else _ -> :default after :cleanup end`, `receive do :msg -> :ok after 100 -> :timeout end`, `with {:ok, a} <- foo(), {:ok, b} <- bar() do {a, b} else {:error, e} -> e end`.
- `stab_eoe` annotations: `fn x -> x; end`, `fn x -> x\n;\nend`, `(x -> x;)`, `(x -> x\n;)`.
- Dot-call newline boundaries: `foo.\n()`, `foo.\n(1, 2)`.
- Sigil delimiters: `~s(...)`, `~s[...]`, `~s{...}`, `~s<...>`, `~s/.../`, `~s|...|`, `~s'...'`.
- Unicode identifiers/atoms: `fóó()`, `:bår`, `def módule? do end`-style samples.
- Operator edge cases: `& &1 + &2`, `<-` / `\\` in matched contexts outside `with/for`, precedence around `//` special-casing.
- Whitespace/comments: comments between callee and `(`, between `->` and body, multiple blank lines acting as EOE.

## Metadata checks
- Add targeted assertions (possibly with dedicated helper) that `end_of_expression` and `:newlines` metadata are preserved in known hotspots: `"1;2"`, `"1\n2"`, `fn x -> 1;2 end`, multi-line remote calls (`IO.warn(\n...)\n`), and dot-container calls before normalization.

## Placement guidance in `conformance_test.exs`
- Add new `describe` blocks near existing sections: errors/warnings near keyword/access sections; block/EOE additions in "unmatched_expr - block_expr"; operator/warning cases near operator sections; sigils/identifiers near terminal literal sections.
- Keep using `assert_conforms/2` for valid cases; use new helpers for error/warning cases to avoid changing existing expectations.
