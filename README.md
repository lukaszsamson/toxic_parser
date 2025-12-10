# ToxicParser

ToxicParser is an error-tolerant Elixir parser that integrates with the Toxic streaming lexer and emits an event log suitable for AST/CST/environment builders. Phase 0 focuses on contracts and conformance harnessing; the current implementation defers to `Code.string_to_quoted_with_comments/2` while locking the public surface.

## Modes
- `:strict` (default): lexer/parser errors are fatal and returned as `{:error, %ToxicParser.Result{}}`.
- `:tolerant`: lexer/parser errors are collected in diagnostics and returned alongside the best-effort result; no exception is raised.
- Tolerant mode downgrades lexer fatals into diagnostics and emits synthetic error tokens; strict mode stops on the first lexer error and never synthesizes delimiters.
- Newline counts from `eol`/`;` tokens are preserved on the normalized `:eoe` token for layout-sensitive operators.

See `PHASE0_CONTRACTS.md` for detailed behavior, event log schema, metadata policy, and builder contracts.

## Entry Points
- `ToxicParser.parse_string/2`
- `ToxicParser.parse_file/2`
- `ToxicParser.parse/2` (alias of `parse_string/2`)

All entry points return `{:ok, %ToxicParser.Result{}}` on success or `{:error, %Result{}}` in strict mode failures.

## Conformance Harness
`ToxicParser.Conformance.compare/2` runs the parser in strict and tolerant modes and returns the reference AST/comments from Elixir's parser for CI assertions. ExUnit tests exercise this harness and keep `NONTERMINALS_GPT.md` in sync via `ToxicParser.Nonterminals`.

## Development
- Run `mix test` to execute the conformance harness and contract checks.
- `NONTERMINALS_GPT.md` is treated as the canonical nonterminal mapping and is loaded at compile time.
- Set `TOXIC_PATH` to point at a checkout of the Toxic lexer when running CI (see `.github/workflows/ci.yml`); falls back to `/Users/lukaszsamson/claude_fun/toxic` for local development.
- Phase 3 adds Pratt skeleton and precedence table; Phase 4 introduces a minimal grammar dispatcher. Phase 5 begins call/identifier scaffolding with paren call parsing and identifier classification. Public API still delegates to `Code.string_to_quoted_with_comments/2` until parser completeness.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `toxic_parser` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:toxic_parser, "~> 0.1.0"}
  ]
end
```
