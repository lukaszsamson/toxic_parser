# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ToxicParser is an error-tolerant Elixir parser that uses a Pratt parsing algorithm. It integrates with the Toxic streaming lexer (separate repository) and produces ASTs conformant with Elixir's `Code.string_to_quoted/2`.

## Commands

```bash
# Run all tests
mix test

# Run a single test file
mix test test/conformance_test.exs

# Run a specific test by line number
mix test test/conformance_test.exs:123

# Run tests matching a pattern
mix test --only describe:"terminals - integers"

# Run dialyzer for type checking
mix dialyzer

# Compile
mix compile

# Format
mix format
```

## Environment Setup

Set `TOXIC_PATH` to the Toxic lexer checkout path (falls back to `/Users/lukaszsamson/claude_fun/toxic`).

## Architecture

### Core Parsing Pipeline

1. **TokenAdapter** (`lib/toxic_parser/token_adapter.ex`) - Wraps Toxic lexer, provides peek/next/pushback interface with EOE normalization
2. **Pratt** (`lib/toxic_parser/pratt.ex`) - Core Pratt parser with binding power-based precedence. Entry points:
   - `parse/3` - Full expression parsing
   - `parse_with_min_bp/4` - Parse with minimum binding power (stops before lower-precedence operators)
   - `parse_base/3` - Base expression without trailing operators
   - `led/5` - Left denotation (infix/postfix operators)
3. **Grammar** (`lib/toxic_parser/grammar.ex`) - Entry dispatcher, calls `Expressions.expr_list`
4. **Grammar modules** (`lib/toxic_parser/grammar/*.ex`) - Domain-specific parsers:
   - `Calls` - Paren calls, no-parens calls, identifier classification
   - `Containers` - Lists, tuples, maps, bitstrings, parenthesized expressions
   - `Blocks` - `fn` expressions, stab clauses
   - `DoBlocks` - `do/end` block handling
   - `Strings` - String/heredoc/sigil parsing with interpolation
   - `Keywords` - Keyword list parsing
   - `Stabs` - Stab clause (`->`) parsing
   - `Dots` - Dot operator and remote calls

### Key Design Patterns

**Binding Power (Precedence)**: Defined in `Precedence` module. Higher numbers bind tighter. Associativity handled via left/right bp adjustment in `led`.

**Expression Contexts**:
- `:matched` - do-blocks NOT attached to identifiers. Includes literals, parens calls, fn-end, stabs, no parens one argument calls and operators with `matched_expr` operands
- `:unmatched` - do-blocks attached to identifiers. Includes do-blocks and operators with various kinds of operands
- `:no_parens` - Defined but rarely used directly, falls back to `:matched`. Allows nested ambiguous calls like `f g a, b`, multi-argument calls like `f a, b` and operators with `no_parens_expr` operands

**No-parens Call Handling**: `NoParens.can_start_no_parens_arg?/1` determines if a token can start a no-parens argument.

### Test Organization

- `test/conformance_test.exs` - Main conformance tests against Elixir's parser (organized by grammar nonterminals)
- `test/elixir_source_repros_test.exs` - Regression tests from real Elixir source failures
- `test/conformance_corpus_test.exs` / `conformance_large_test.exs` - Tests against Elixir source corpus
- Domain-specific tests: `calls_test.exs`, `operators_test.exs`, `strings_test.exs`, etc.

### Conformance Testing

Tests use `assert_conforms/1` helper which compares ToxicParser output against `Code.string_to_quoted_with_comments/2` using `Macro.to_string` normalization.

## Reference Documents

- `elixir_parser.yrl` in the Elixir source is the canonical grammar reference
- Token kinds come from the Toxic lexer (e.g., `:dual_op` for `+`/`-`, `:op_identifier` for ambiguous no-parens calls)
- Token documentation in `/Users/lukaszsamson/claude_fun/toxic/ALL_TOKENS.md`
- Toxic lexer source code in `/Users/lukaszsamson/claude_fun/toxic`
