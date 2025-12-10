# Error-Tolerant Elixir Parser – High-Level Design (Draft 1)

This document proposes a new error-tolerant Elixir parser intended for IDE / LSP / tooling use. It is informed by:

- The canonical Elixir LALR(1) grammar in `elixir_parser.yrl`
- The Toxic streaming tokenizer and its token model
- Prior reviews of Spitfire (`REVIEW_CODEX_1.md`, `REVIEW_G3_1.md`, `REVIEW_OPUS_1.md`)
- Modern parser work in Ruby (Prism/YARP) and Python (Ruff’s hand-written parser)

Goals:

- Faithfully implement the Elixir grammar, including all `matched_expr` / `unmatched_expr` / `no_parens_expr` distinctions.
- Be robust and predictable in the presence of syntax errors, producing a best-effort AST rather than failing early.
- Avoid Spitfire’s architectural pitfalls (monolithic Pratt, post-hoc AST normalization, state flags everywhere).
- Integrate cleanly with Toxic’s streaming architecture, error tokens, and terminator stack.

Non-goals (for this design phase):

- Concrete module layout and naming.
- Exact AST shape for every construct beyond “Core-compatible + error nodes”.
- Incremental parsing and green-tree persistence (we can design for compatibility, but not implement it here).

---

## 1. Top-Level Architecture

### 1.1 Overview

The new parser is a **hand-written recursive-descent parser with an embedded Pratt expression engine**, structured to mirror the canonical `elixir_parser.yrl` nonterminals as closely as possible.

Layers:

1. **Token Stream Layer (Toxic)**  
   - Single source of tokens, with:
     - `Toxic.next/1`, `Toxic.peek/1`, `Toxic.peek_n/2`, `Toxic.checkpoint/1`, `Toxic.rewind_to/2`
     - Ranged metas and error tokens (`{:error_token, meta, %Toxic.Error{}}`)
     - Terminator stack (`Toxic.current_terminators/1`) and missing closer hints (`Toxic.peek_missing_terminator/1`)
   - **No legacy tokenizer**. The parser assumes Toxic’s streaming token model.

2. **Grammar Layer (Recursive Descent)**  
   - One function per important nonterminal from `elixir_parser.yrl`, e.g.:
     - `parse_grammar/1`, `parse_expr_list/1`, `parse_expr/1`
     - `parse_matched_expr/1`, `parse_unmatched_expr/1`, `parse_no_parens_expr/1`
     - `parse_container_expr/1`, `parse_block_expr/1`, `parse_call_args_parens/1`, etc.
   - These functions:
     - Use a small, fixed set of helpers for token consumption and error recovery.
     - Call into the **expression layer** when precedence/associativity is the main concern.

3. **Expression Layer (Pratt)**  
   - Responsible only for operator precedence and associativity within the `*_expr` nonterminals where `yrl` uses `*_op_eol`.
   - Exposes a `parse_expression(parser, min_bp, category)` entry, where:
     - `min_bp` – binding power lower bound.
     - `category` – expression category (`:matched`, `:unmatched`, `:no_parens`) to enforce grammar constraints.
   - Keeps null denotation (`nud`) and left denotation (`led`) logic in **small parselets**, not a monolithic `parse_expression/…` function.

4. **Error-Recovery Layer**  
   - Cross-cutting helper module providing:
     - Uniform handling of Toxic `:error_token`s.
     - Structured error nodes in the AST.
     - Synchronization strategies (e.g., skip until `;`, `:eol`, closers, `end`, or other rule-specific sync tokens).
   - Recovery decisions are made **per nonterminal** using its follow set, not via ad-hoc loops scattered across the parser.

5. **AST Construction Layer**  
   - Small, focused builders to mirror `Code.string_to_quoted/2` shape:
     - `build_block/1`, `build_call/3`, `build_fn/3`, `build_list/3`, `build_map/…`, etc.
   - **Normalization passes are minimized or eliminated**:
     - Operators like `not in`, pipe, unary `+`/`-` are handled correctly in the parser, not “fixed in post”.
   - AST nodes for syntax errors and lexer errors:
     - `{:__error__, meta, error_payload}` with structured payloads that tools can inspect.

### 1.2 Fundamental Constraints

- **Grammar Fidelity First**: The parser is derived from `elixir_parser.yrl`. For each nonterminal we decide:
  - Direct recursive-descent translation, or
  - Expression-level treatment within the Pratt engine.
- **Token Consumption Invariants**:
  - Every successful parse function either:
    - Consumes at least one token, or
    - Explicitly documents that it can be called on EOF and returns gracefully.
  - Error recovery always either:
    - Consumes at least one token, or
    - Terminates the current nonterminal with an error node.
  - No “fuel counter” is needed; infinite loops are prevented structurally.
- **IDEs and LSPs as Primary Consumers**:
  - AST must be usable even when incomplete or partially wrong.
  - Error nodes must preserve ranges and as much local structure as possible.
  - AST must have a reasonably stable shape between valid and invalid code.

---

## 2. Mapping the Elixir Grammar

### 2.1 Direct Mapping Strategy

We define **parser functions that intentionally mirror** key `yrl` nonterminals. Examples:

- `grammar` → `parse_grammar/1`
- `expr_list` → `parse_expr_list/1`
- `expr` → `parse_expr/1`
- `matched_expr` / `unmatched_expr` / `no_parens_expr` → `parse_matched_expr/1`, `parse_unmatched_expr/1`, `parse_no_parens_expr/1`
- `container_expr` → `parse_container_expr/1`
- `block_expr` → `parse_block_expr/1`
- `call_args_parens` / `call_args_no_parens_*` → matching `parse_*` functions
- `stab` / `stab_expr` / `do_block` / `block_list` → `parse_stab_expr/1`, `parse_do_block/1`, `parse_block_list/1`

Where `yrl` uses helper nonterminals like `*_op_eol`, we:

- Model them as either:
  - Specialized Pratt operators with precise binding powers, or
  - Micro nonterminals inside the grammar layer that call into Pratt.

The design rule:

- If the production’s main complexity is **operator precedence**, model it in Pratt.
- If the production’s main complexity is **structural or context-sensitive** (e.g., optional `do` blocks, no-parens rules), keep it in the recursive-descent grammar layer.

### 2.2 Expression Categories: matched / unmatched / no_parens

We keep the three major categories as first-class concepts:

- `matched_expr` – Expressions that are syntactically “safe” (parens or other disambiguation).
- `unmatched_expr` – Expressions that can carry `do/end` blocks.
- `no_parens_expr` – Expressions relying on spacing and arity rules for disambiguation.

Rather than encoding these as boolean flags (`is_top`, `is_stab`, `is_list`, etc.), the parser:

- Exposes **separate entry points**:
  - `parse_matched_expr(parser)`
  - `parse_unmatched_expr(parser)`
  - `parse_no_parens_expr(parser)`
- Each entry point calls into the Pratt engine with a `category` parameter indicating:
  - Which operators are legal.
  - How to interpret EOL/EOE.
  - Whether `do` blocks may attach to the result.

Examples:

- `parse_unmatched_expr/1`:
  - Accepts `block_expr` productions (e.g., `if … do … end`).
  - Allows matched/unmatched/no-parens operators as defined in the grammar.
- `parse_no_parens_expr/1`:
  - Applies the strict nested no-parens call rules from `yrl` (`no_parens_one_ambig_expr`, `no_parens_many_expr`, etc.).
  - Errors/warns on invalid nesting patterns via error nodes, not ad-hoc checks.

### 2.3 EOE / EOL / Terminators

Instead of random `eat_eol` calls, we mirror `elixir_parser.yrl`’s `eoe` nonterminal and its usage:

- Introduce an internal “EOE token view” over Toxic tokens:
  - `;` and `:eol` tokens are normalized into a logical `:eoe` concept where appropriate.
  - Commas that also coalesce newline info (via `meta.extra`) are **not** treated as EOE; they stay as commas but can act as sync points for error recovery.

In grammar functions that reference `eoe`:

- We implement helpers like:
  - `maybe_parse_eoe(parser)` → `{:ok, parser}` or `{:missing, parser}`
  - `parse_eoe(parser)` → consumes one or more `;` / `:eol` tokens forming an expression separator.
- We leverage Toxic’s `:eol` meta (coalesced newlines) and `current_terminators/1` to distinguish:
  - Expression separators inside containers/blocks.
  - Formatting-only newlines inside expressions (e.g., line breaks in long pipelines).

The goal is a **single model of “expression end”** instead of a proliferation of pending states.

---

## 3. Token Stream & Lookahead Strategy

### 3.1 Token Stream API

The parser uses only the Toxic API:

- `next/1` – consume next token.
- `peek/1` – single-token lookahead.
- `peek_n/2` – multi-token lookahead for ambiguity resolution.
- `checkpoint/1` / `rewind_to/2` – speculative parsing for rare ambiguous constructs.
- `current_terminators/1` – track open `(`, `[`, `{`, `do`, heredocs, etc.
- `peek_missing_terminator/1` – detect likely missing closers for recovery hints.

No legacy backend, no custom token shapes – all rules are expressed in terms of Toxic’s streaming tokens described in `ALL_TOKENS.md`.

### 3.2 Lookahead Usage

Elixir’s grammar is LALR(1), but some practical decisions benefit from explicit lookahead:

- Distinguish `cmd [key: val]` (call) vs `[key: val]` (list).
- Attach `do` blocks to the right call expression:
  - `foo bar do: baz` vs `foo(bar, do: baz) do … end`.
- Disambiguate no-parens call chains (`no_parens_one_ambig_expr`, `no_parens_many_expr`).

Design:

- Use `peek_n` only where `yrl` requires nontrivial lookahead; encapsulate those checks in small helpers:
  - `looks_like_no_parens_call/1`
  - `looks_like_kw_list/1`
  - `looks_like_block_expr/1` (for `do`/`end` carriers)
- Avoid general speculative parsing; prefer **predictive decisions** using lookahead.
- Use `checkpoint/rewind` only where the grammar is truly ambiguous or where Toxic’s tokenization granularity creates a practical ambiguity (expected to be rare).

### 3.3 Token Categories & Adaptation to `yrl`

We introduce internal “views” over Toxic tokens to match the `yrl` grammar’s categories:

- `identifier`, `paren_identifier`, `bracket_identifier`, `do_identifier`, `op_identifier`, `alias` map straightforwardly.
- Operators are grouped into:
  - `*_op_eol` categories based on `ALL_TOKENS.md` (`:dual_op`, `:mult_op`, `:pipe_op`, `:in_op` / combined `"not in"`, etc.).
- Structural tokens (`'('`, `')'`, `'['`, `']'`, `'{'`, `'}'`, `'<<'`, `'>>'`, `:%{`, `:%`) are recognized directly.

These views are pure and local; we avoid making the parser context-dependent on token shape beyond what `yrl` models.

---

## 4. Expression Engine (Pratt) Design

### 4.1 Responsibility Scope

The Pratt engine is responsible for:

- Implementing precedence and associativity for:
  - `match_op_eol`, `dual_op_eol`, `mult_op_eol`, `power_op_eol`, `concat_op_eol`, `range_op_eol`, `ternary_op_eol`,
    `xor_op_eol`, `and_op_eol`, `or_op_eol`, `in_op_eol`, `in_match_op_eol`, `type_op_eol`, `when_op_eol`,
    `pipe_op_eol`, `comp_op_eol`, `rel_op_eol`, `arrow_op_eol`, and unary ops (`unary_op_eol`, `at_op_eol`, `capture_op_eol`, `ellipsis_op`).
- Handling dotted forms and call expressions that are naturally expression-like:
  - `dot_identifier`, `dot_alias`, `dot_call_identifier`, `parens_call` (where the grammar treats them as expressions).

It is **not** responsible for:

- `do` blocks, `fn` blocks, `case`, `try`, `receive`, `with`, etc.
- No-parens arity/ambiguity rules.
- EOE / block boundaries.

Those remain in the grammar layer.

### 4.2 Parselet Structure

Each token type that can start an expression or extend one has a small, focused parselet:

- `nud` parselets (null denotation):
  - Literals (`int`, `flt`, `char`, string tokens collapsed into `bin_string`, `list_string`, etc.).
  - Containers (`list`, `tuple`, `map`, `bitstring`), via grammar helpers that call into Pratt for elements when needed.
  - Unary operators (`unary_op`, `dual_op` when unary, `at_op`, `capture_op`, `ellipsis_op`).
  - Dot forms (`dot_identifier`, `dot_alias`, `parens_call`) when they appear in positions allowed by the grammar.

- `led` parselets (left denotation):
  - Binary operators with appropriate binding powers and associativity, including:
    - Combined operators like `not in` as a single operator token (`:in_op` with `"not in"`).
    - `?:`-like ternary is expressed as binary `ternary_op` per `yrl`.
  - Dot operator (`.`) and `dot_call_op`:
    - `foo.bar`, `foo.(args)`, etc.

Each parselet:

- Returns a correctly shaped AST node **directly**, not an intermediate form to be normalized later.
- Encodes corner cases like `not in` and `|>` as part of its semantics.

### 4.3 Binding Power Table

We derive the binding power table directly from `elixir_parser.yrl`’s precedence declarations:

- Map each `*_op_eol` priority level to a `left_bp` / `right_bp` pair.
- Right-associative operators (`match_op_eol`, `when_op_eol`, `type_op_eol`, etc.) get `right_bp = left_bp - 1`.
- The Pratt engine has a **single** source of truth for operator precedence that mirrors the LALR table.

This makes the precedence/associativity story both:

- Faithful to the reference grammar.
- Easy to audit and cross-check with upstream Elixir changes.

---

## 5. Statement / Block / Do-End Parsing

### 5.1 High-Level Blocks

High-level constructs (`defmodule`, `def`, `if`, `case`, `try`, `with`, `receive`, etc.) are handled by specialized grammar functions:

- Example: `parse_block_expr/1` mirrors the `block_expr` nonterminal:
  - `dot_call_identifier call_args_parens do_block`
  - `dot_do_identifier do_block`
  - `dot_identifier call_args_no_parens_all do_block`
  - etc.

Design:

- Recognize these forms using Toxic’s transformed identifiers (`do_identifier`, `block_identifier`) and lookahead.
- Use dedicated functions to parse:
  - `parse_do_block/1` – `do` + `block_list` + `end` with proper terminator handling.
  - `parse_block_list/1` – repeated `block_item`s separated by `eoe` / semicolons / newlines.
  - `parse_block_item/1` – uses `parse_expr/1` or specific forms like `rescue`/`after` sub-blocks.

### 5.2 Clause Lists (`stab_expr`, `fn`, `case`, `try`, `with`)

Clause-based structures are parsed with grammar-layer loops:

- Anonymous functions: `fn_eoe stab_eoe 'end'` → `parse_fn/1`.
- `case`, `with`, `receive` clauses:
  - `stab_eoe` nonterminals map to `parse_stab_expr/1` and `parse_clause_list/1`.

These functions:

- Use EOE-aware helpers to respect newline/separator rules.
- Use expression entry points (`parse_unmatched_expr`, `parse_matched_expr`, etc.) per grammar requirement.

Avoided pitfalls:

- No global `:stab_state` or `:allow_do_in_args` flags leaking through the parser.
- Clause parsing is scoped to clause-oriented functions, with clear entry/exit invariants.

### 5.3 Containers and Keyword Lists

Containers (`list`, `tuple`, `bitstring`, `map`) are handled using the `container_expr` rules:

- For items, we call `parse_container_expr/1`, which:
  - Accepts `matched_expr` / `unmatched_expr` / `no_parens_expr` according to `yrl`.
  - Enforces `no_parens` container restrictions by producing error nodes when violated (`error_no_parens_container_strict` equivalent).
- Keyword lists in:
  - Call arguments (`kw_call`, `call_args_no_parens_kw`, etc.).
  - Maps / bitstrings (`kw_data`).

These are implemented as dedicated functions mirroring `kw_*` productions, with:

- Clear separation of “keyword pair” vs “expression” roles.
- Error nodes for invalid combinations (`maybe_bad_keyword_call_follow_up`, etc.) instead of silent re-shaping.

---

## 6. Error Tolerance & Recovery

### 6.1 Error Node Model

Instead of signaling all failures via metadata or empty blocks, the parser introduces **explicit error AST nodes**:

- `{:__error__, meta, payload}`

Where `payload` includes:

- `:kind` – e.g. `:missing_closer`, `:unexpected_token`, `:invalid_kw_identifier`, `:unterminated_expression`, `:lexer_error`.
- `:token` / `:expected` / `:found` – small structured details.
- `:inner` – optional partially parsed subtree or tokens list.
- `:message` – human-friendly description (for debugging / logs, not necessarily for end users).

Error nodes:

- Are used both for **syntactic** errors (parser-level) and for **lexical** errors (Toxic’s `:error_token`s).
- Are placed in the AST where the expression or construct would normally be.
- Preserve the widest possible range from the first offending token to the chosen sync point.

### 6.2 Interaction with Toxic Error Tokens

When `Toxic.next/1` yields `{:error_token, meta, %Toxic.Error{} = err}`:

- Parser wraps it into an error node immediately:
  - `{:__error__, meta, %{kind: :lexer_error, error: err}}`
- The grammar function decides whether to:
  - Treat this as a standalone expression/element (e.g., inside a list).
  - Skip ahead to a sync token (like `;`, newline, closing delimiter) and attach any skipped tokens as `:inner` to the error node.

We **never discard** Toxic error information; we surface it in the AST.

### 6.3 Synchronization Strategy

Each grammar function defines its own **sync set**:

- Example: `parse_expr_list/1` can sync on:
  - EOE (`;` or newline)
  - EOF
- Example: `parse_do_block/1` can sync on:
  - `:block_identifier` for `rescue`, `after`, `else`, `catch`
  - `:end`
  - EOF / closers from Toxic’s terminators stack
- Example: `parse_list/1` can sync on:
  - `]`
  - `;`, newline, `,` as element separators

Implementation:

- Central helper, e.g., `recover(parser, sync_tokens, context)`:
  - Consumes tokens until:
    - Sync token encountered, or
    - EOF or a high-level boundary is reached (determined via terminator stack).
  - Returns `{error_node, parser}` where `error_node` includes:
    - `:inner_tokens` (or sub-AST fragments) gathered during recovery.
    - `:sync_token` describing where we stopped.

The sync decisions are **local and explicit** per nonterminal, not scattered as ad-hoc loops.

### 6.4 Partial Structures & Synthesis

When Toxic synthesizes structural closers (due to `insert_structural_closers: true`):

- The parser sees synthetic closing tokens with proper ranges.
- For incomplete constructs (e.g., `fn ->`, `[1, 2`, `%{a: 1`, `if true do`), we:
  - Create an error node attached to the AST for the partially parsed structure.
  - Use the synthetic closer (if present) to bound the error range.
  - When closers are missing even after Toxic recovery, we may:
    - Rely on terminator hints (`peek_missing_terminator/1`) to annotate the error.

This ensures that IDEs see:

- A structurally reasonable AST fragment.
- A clearly marked error on the truncated construct.

---

## 7. AST Shape and Normalization Philosophy

### 7.1 Core-Compatible AST

The target AST should be:

- As close as possible to `Code.string_to_quoted/2` output.
- Differences:
  - Presence of `{:__error__, …}` nodes.
  - Additional metadata fields for error tolerance (e.g., terminator hints, missing-closer hints).

AST builders are organized around the same conceptual helpers that `elixir_parser.yrl` uses internally:

- `build_block/1`, `build_op/2`, `build_unary_op/2`, `build_list/3`, `build_tuple/3`, `build_map/…`, `build_bin_string/…`, etc.

### 7.2 Avoiding Post-Parse Normalization

Spitfire’s design relied on dozens of `Macro.prewalk` passes to “fix” the AST after parsing. This design explicitly avoids that by:

- Implementing semantics like:
  - `not in` as a combined operator at parse time.
  - `fn`, pipe, unary ranges, `not` over `in`, etc., directly in parselets.
  - `keyword` vs `do` block disambiguation using grammar-guided rules.
- Restricting post-processing to:
  - Simple, local cleanups (e.g., stripping transient parser-only metadata).
  - Optional conversions from error nodes to the user’s preferred error representation, if needed for compatibility.

The default pipeline is:

1. `Toxic` tokenization.
2. Parser → AST (with errors).
3. Optional tooling-specific passes (e.g., LSP projection, type-checker-specific annotations).

No general-purpose normalization pass is required to get a correct AST for valid programs.

---

## 8. Performance & Tooling Considerations

### 8.1 Complexity

- Recursive descent + Pratt yields **linear-time behavior** on valid code.
- Error recovery may skip tokens, but each token is visited a bounded number of times.
- No backtracking in the common case; speculative parsing is rare and bounded.

### 8.2 Memory and Streaming

- Parser operates over a streaming Toxic token source:
  - Avoids eager tokenization of the entire file when not needed.
  - Compatible with large files and incremental parsing strategies.
- Ranges are kept in Toxic’s `meta`; parser does not duplicate position tracking.

### 8.3 IDE / LSP Integration

The design supports:

- Per-node ranges for diagnostics and highlighting.
- Error nodes with structured payloads for:
  - Squiggles, quick-fixes, and code actions.
  - “Explain this syntax error” features.
- Future incremental parsing:
  - The grammar and AST are designed to be decomposable into **stable subtrees**.
  - We can later introduce a green-tree or lossless syntax tree representation, inspired by Prism/YARP and Ruff.

---

## 9. Avoiding Spitfire’s Pitfalls

This design deliberately addresses the main issues raised in the Spitfire reviews:

- **Monolithic `parse_expression`** → replaced with:
  - Clear recursive-descent grammar functions per nonterminal.
  - A Pratt engine with small, decomposed parselets.
- **AST fixed “in post”** → replaced with:
  - Correct AST construction in parselets and grammar functions.
  - Minimal or no post-parsing normalization.
- **State flags everywhere** → replaced with:
  - Grammar structure guiding allowed constructs (e.g., no-parens categories).
  - Small, explicit enums / parameters (like `category`) instead of boolean flag soup.
- **Ad-hoc EOE handling** → replaced with:
  - Centralized EOE handling mirroring `eoe` nonterminal.
  - Toxic `:eol` meta and terminator stack for precise expression boundaries.
- **Dual tokenizer backends** → replaced with:
  - Toxic-only tokenization.
  - Direct use of error tokens, peek_n, and structural recovery.
- **Error tolerance bolted on** → replaced with:
  - First-class error nodes.
  - Per-nonterminal sync sets and structured recovery.

---

## 10. Next Steps

Implementation-oriented follow-up work (out of scope for this HLD, but implied):

- Define the concrete Elixir modules and public API:
  - `Spitfire.Parser`-like module with `parse/1` and `parse_with_errors/1` entry points.
- Map the entire `elixir_parser.yrl` nonterminal set to parser functions:
  - Start from top-level (`grammar`, `expr_list`, `expr`) and drill down.
- Implement the Pratt engine and operator table directly from the `yrl` precedence section.
- Implement error nodes and recovery helpers, starting with:
  - Closers/terminators (`(`, `[`, `{`, `do`, heredocs).
  - Expression separators (`;`, newline).
- Validate:
  - Golden tests comparing against `Code.string_to_quoted/2` on valid code.
  - Fuzzing / property tests exercising Toxic error modes and error-node stability.

This design gives a cleaner, grammar-derived foundation for an error-tolerant Elixir parser suitable for IDEs, LSP servers, and advanced tooling, while explicitly avoiding the structural problems identified in the existing Spitfire implementation.

