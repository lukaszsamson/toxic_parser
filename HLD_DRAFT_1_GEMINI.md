# High Level Design: Error-Tolerant Elixir Parser (Gemini Draft 1)

## 1. Executive Summary

This document outlines the design for a new, error-tolerant Elixir parser intended to power IDE tools (Language Servers, type checkers). It addresses the architectural flaws of the previous `spitfire` parser by adopting a **Hybrid Recursive Descent + Pratt** architecture that strictly adheres to the canonical `elixir_parser.yrl` grammar structure. It leverages the **Toxic** tokenizer's advanced capabilities (streaming, error tokens, precise ranges) to achieve robustness without fragile post-processing.

## 2. Design Principles

1.  **Grammar Fidelity**: The parser structure must mirror `elixir_parser.yrl` non-terminals. If the grammar has a rule for `matched_expr`, the parser has a function `parse_matched_expr`.
2.  **Error Tolerance**: The parser must never crash. It must recover from errors by synchronizing on safe tokens and producing "Error Nodes" in the AST, preserving as much valid structure as possible.
3.  **Zero Post-Processing**: The parser must produce correct ASTs during the parse phase. Normalization passes (like those in `spitfire`) are prohibited.
4.  **Toxic-Native**: The parser is built specifically for the `Toxic` tokenizer, utilizing `peek_n`, error tokens, and range data directly.

## 3. Architecture

### 3.1. Tokenizer Integration (Toxic)
The parser will consume a `Toxic` stream directly.
*   **Input**: `Toxic.t` stream.
*   **Lookahead**: Use `Toxic.peek_n/2` for disambiguation (e.g., distinguishing `kw_list` from `block`).
*   **Error Handling**:
    *   **Lexer Errors**: `{:error_token, meta, details}` from Toxic are accepted as valid tokens and wrapped in `ErrorAST` nodes.
    *   **Synthesized Tokens**: Toxic's ability to synthesize missing closers (in tolerant mode) will be utilized.

### 3.2. Parser Structure: Hybrid Recursive Descent + Pratt

The parser will use **Recursive Descent** for high-level statements and structure, and **Pratt Parsing** for expressions, controlled by context-specific binding powers.

#### 3.2.1. The "Context" Concept
Instead of boolean flags (`is_list`, `is_map`), the parser will maintain a `Context` that mirrors the `yrl` hierarchy:
*   `:matched` (Parenthesized/Atomic)
*   `:unmatched` (Block-allowing)
*   `:no_parens` (Ambiguous calls)

#### 3.2.2. Core Functions (Mapping to YRL)

*   `parse_program(stream)`: Entry point. Parses a list of expressions (`expr_list`).
*   `parse_expr_list(stream)`: Loop that consumes expressions separated by `eoe` (End of Expression).
*   `parse_expr(stream, context)`: The main dispatch function.
    *   Corresponds to `expr` in YRL.
    *   Dispatches to `parse_pratt_expr(stream, min_binding_power, context)`.

#### 3.2.3. Pratt Parser Implementation
The Pratt loop (`parse_pratt_expr`) handles operator precedence. Crucially, the `context` determines which operators are allowed and what the Right-Hand Side (RHS) can be.

*   **NUD (Null Denotation)**: Handles prefix ops, literals, identifiers, `if/case/try` (as prefix operators or structural keywords).
*   **LED (Left Denotation)**: Handles infix/postfix ops (`+`, `|>` , `.`).
*   **Context Enforcement**:
    *   If context is `:matched`, the LED for `do` (blocks) is disabled or unreachable.
    *   If context is `:matched`, the RHS of an infix op must be parsed with `:matched` context.
    *   If context is `:unmatched`, the RHS can be `:unmatched`.

### 3.3. Handling Specific Constructs

#### 3.3.1. `do` Blocks
Handled as a suffix operator (LED) with very low binding power, available only in `:unmatched` context.
*   **Grammar**: `block_expr -> ... do_block`
*   **Implementation**: When `do` is encountered in `parse_pratt_expr` (and context allows), it triggers `parse_do_block`.

#### 3.3.2. No-Parens Calls (`foo 1, 2`)
This is the most complex part of Elixir parsing.
*   **Detection**: `identifier` followed by space + start of expression.
*   **Resolution**: Use `Toxic.peek_n` to distinguish `foo(1)` (paren_identifier) from `foo (1)` (identifier + space + paren).
*   **Precedence**: Treated as a high-precedence prefix operator in the Pratt sense, or handled explicitly in the NUD of the identifier.

#### 3.3.3. Containers (Lists, Maps, Tuples)
Handled in NUD of opening tokens (`[`, `{`, `%{`).
*   Recursive call to `parse_container_args`.
*   Uses synchronization on closing tokens (`]`, `}`, `)`) to recover from malformed elements.

## 4. Error Recovery Strategy

Recovery is integrated into the recursive descent functions, not patched later.

### 4.1. Synchronization
When an unexpected token is encountered:
1.  **Emit Error Node**: Create an AST node `{:__error__, meta, ...}` containing the unexpected token.
2.  **Panic Mode**: Advance stream until a **Synchronization Token** is found.
    *   Sync Tokens: `;`, `eol` (for statements), `end`, `]`, `}`, `)` (for blocks/containers).
3.  **Resume**: Continue parsing from the sync token.

### 4.2. Error AST Nodes
Errors are first-class citizens in the AST.
```elixir
{:__error__, meta, [
  kind: :unexpected_token,
  found: token,
  expected: [:identifier, :integer],
  context: :parsing_function_args
]}
```
This allows the IDE to display red squiggles while still understanding the surrounding code.

## 5. Comparison with Spitfire

| Feature | Spitfire | New Design |
| :--- | :--- | :--- |
| **Architecture** | Pure Pratt (forced) | Hybrid (Recursive Descent + Pratt) |
| **Grammar** | Ad-hoc, loose approximation | Strict mapping to `elixir_parser.yrl` |
| **State** | Complex flags (`is_list`, `is_stab`) | Explicit Grammar Contexts |
| **Tokenizer** | Dual (Legacy + Toxic) | Toxic Only |
| **Lookahead** | Single token (`peek`) | Multi-token (`peek_n`) |
| **AST** | Requires 20+ normalization passes | Correct by construction |
| **Error Recovery** | Fragile, state-dependent | Robust, synchronization-based |

## 6. Implementation Plan

1.  **Scaffold**: Create `Parser` struct and basic loop.
2.  **Literals & Atoms**: Implement NUDs for basic types.
3.  **Operators**: Implement Precedence Table and Pratt loop.
4.  **Structure**: Implement `parse_block`, `parse_fn`, `parse_container`.
5.  **No-Parens**: Implement ambiguity resolution logic.
6.  **Recovery**: Add synchronization points to all container/block parsers.
