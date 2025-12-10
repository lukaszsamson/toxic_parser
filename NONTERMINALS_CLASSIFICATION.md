# Comparison of Parsing Approaches

## Overview

All three models (G3, GPT, OPUS) agree on the hybrid strategy:

- Use **Pratt parsing** for the operator/precedence-heavy expression families.
- Use **Recursive Descent (RD)** for structural constructs (blocks, containers, calls, argument lists, etc.).
- Treat simple operator-token rules (`*_op_eol`) and token wrappers (`open_*`, `close_*`, `eoe`, etc.) as trivial helpers that feed the main strategies.

GPT’s final classification keeps the file `NONTERMINALS_GPT.md` as a two-way tag (`pratt` vs `recursive descend`), but conceptually we recognize a third role (“helper”) where OPUS does.

---

## 1. Core Expression Rules

**Final stance:**

- `expr`: **RD (dispatcher)**
- `matched_expr`: **PRATT**
- `unmatched_expr`: **PRATT**
- `no_parens_expr`: **PRATT**
- `matched_op_expr`: **PRATT (integrated)**
- `unmatched_op_expr`: **PRATT (integrated)**
- `no_parens_op_expr`: **PRATT (integrated)**

### Comparison Table

| Nonterminal       | OPUS               | G3    | GPT   | Final view |
|-------------------|--------------------|-------|-------|-----------|
| `expr`            | RD (dispatcher)    | pratt | RD    | **RD**    |
| `matched_expr`    | PRATT              | pratt | pratt | **PRATT** |
| `unmatched_expr`  | PRATT              | pratt | pratt | **PRATT** |
| `no_parens_expr`  | PRATT              | pratt | pratt | **PRATT** |
| `matched_op_expr` | PRATT (integrated) | pratt | pratt | **PRATT** |
| `unmatched_op_expr`| PRATT (integrated)| pratt | pratt | **PRATT** |
| `no_parens_op_expr`| PRATT (integrated)| pratt | pratt | **PRATT** |

### Reasoning

- `expr` is syntactically just:
  - `expr -> matched_expr`
  - `expr -> no_parens_expr`
  - `expr -> unmatched_expr`
- It is a dispatcher that *selects* which expression family to parse; it does not itself implement precedence.
- The Pratt loop lives inside `matched_expr`, `unmatched_expr`, and `no_parens_expr` (via their `*_op_expr` rules).
- So:
  - Marking `expr` as **RD (dispatcher)** (OPUS/GPT) is more precise.
  - G3 calling `expr` “Pratt” reflects that it *enters* the Pratt-based groups, but that’s a semantic shortcut rather than the literal shape of the rule.

Implementation implication: you implement a small RD `parse_expr/1` that chooses which Pratt context to call (`parse_matched_expr_pratt`, `parse_unmatched_expr_pratt`, `parse_no_parens_expr_pratt`).

---

## 2. Operator Definitions: `*_op_expr`

These are the nonterminals that wrap operators plus their RHS and feed into Pratt’s infix handling.

| Nonterminal         | OPUS               | G3    | GPT   | Final view |
|---------------------|--------------------|-------|-------|-----------|
| `matched_op_expr`   | PRATT (integrated) | pratt | pratt | PRATT     |
| `unmatched_op_expr` | PRATT (integrated) | pratt | pratt | PRATT     |
| `no_parens_op_expr` | PRATT (integrated) | pratt | pratt | PRATT     |

**Consensus / reasoning:**

- All three treat these as part of the Pratt layer.
- In `elixir_parser.yrl`, you see patterns like:
  - `matched_expr -> matched_expr matched_op_expr : build_op('$1', '$2').`
- In Pratt terms, this is the *infix* side: once you have a left expression and see an operator, you parse the operator+RHS as a single Pratt infix step.
- Final classification: keep them **PRATT**, and don’t try to parse them as independent RD routines.

---

## 3. Operator Tokens and Simple Helpers

Representative helper rules:

- `unary_op_eol`, `capture_op_eol`, `at_op_eol`
- `match_op_eol`, `dual_op_eol`, `mult_op_eol`, `power_op_eol`, `concat_op_eol`, `range_op_eol`, `ternary_op_eol`, `xor_op_eol`, `pipe_op_eol`, `and_op_eol`, `or_op_eol`, `in_op_eol`, `in_match_op_eol`, `type_op_eol`, `when_op_eol`, `stab_op_eol`, `comp_op_eol`, `rel_op_eol`, `arrow_op_eol`
- `dot_op`
- `open_paren`, `close_paren`, `empty_paren`
- `open_bracket`, `close_bracket`
- `open_bit`, `close_bit`
- `open_curly`, `close_curly`
- `eoe`, `fn_eoe`, `do_eoe`, `block_eoe`

### Comparison Table

| Category           | OPUS                            | G3  | GPT | Final view                    |
|--------------------|----------------------------------|-----|-----|-------------------------------|
| `*_op_eol`         | HELPER (PRATT prefix/infix)     | RD  | RD  | RD **helpers feeding Pratt**  |
| `dot_op`           | HELPER (PRATT infix/postfix)    | RD  | RD  | RD **helper feeding Pratt**   |
| `open_*`/`close_*` | HELPER                          | RD  | RD  | RD **token helpers**          |
| `eoe`/`fn_eoe`/... | HELPER                          | RD  | RD  | RD **token helpers**          |

### Reasoning

- These rules are syntactically trivial: match a token (sometimes with optional `eol`) and return it.
- They are *consumed by* Pratt or RD code but do not themselves perform precedence logic or structural recursion.
- OPUS makes this explicit via a distinct **HELPER** category; G3/GPT fold them into “recursive descend”.

Final classification:

- In `NONTERMINALS_GPT.md` they remain tagged as `recursive descend`.
- Conceptually, they are **RD helpers for Pratt and RD** (operator-token and delimiter wrappers, not top-level parse functions).

---

## 4. Structural Rules

These include:

- Blocks and stabs:
  - `do_block`, `stab`, `stab_eoe`, `stab_expr`, `stab_op_eol_and_expr`, `stab_parens_many`, `block_item`, `block_list`, `block_eoe`.
- Containers:
  - `list`, `list_args`, `tuple`, `bitstring`, `map`, `map_op`, `map_base_expr`, `map_close`, `map_args`,
  - `container_expr`, `container_args_base`, `container_args`.
- Calls and arguments:
  - `block_expr`, `parens_call`,
  - `call_args_parens_expr`, `call_args_parens_base`, `call_args_parens`,
  - `call_args_no_parens_expr`, `call_args_no_parens_comma_expr`, `call_args_no_parens_all`,
  - `call_args_no_parens_one`, `call_args_no_parens_ambig`, `call_args_no_parens_many`, `call_args_no_parens_many_strict`,
  - `call_args_no_parens_kw_expr`, `call_args_no_parens_kw`,
  - `kw_eol`, `kw_base`, `kw_data`, `kw_call`.
- Access and dot:
  - `access_expr`, `sub_matched_expr`, `bracket_arg`, `bracket_expr`, `bracket_at_expr`,
  - `dot_identifier`, `dot_alias`, `dot_op_identifier`, `dot_do_identifier`,
  - `dot_bracket_identifier`, `dot_paren_identifier`, `dot_call_identifier`.

**Consensus:**

- All three analyses keep these in **Recursive Descent**.
- They have clear syntactic structure (specific sequences, lists, bracketed/curly contexts, clause lists) and are not solved by precedence climbing.
- Some of them *contain* expressions (which use Pratt internally), but the outer shell is best written as RD.

Final classification: all of these remain `recursive descend` in `NONTERMINALS_GPT.md`.

---

## 5. Summary of Final Classification

At the level of individual nonterminals (as encoded in `NONTERMINALS_GPT.md`):

- **PRATT**:
  - `matched_expr`
  - `unmatched_expr`
  - `no_parens_expr`
  - `matched_op_expr`
  - `unmatched_op_expr`
  - `no_parens_op_expr`
- **Recursive Descent**:
  - `expr` (dispatcher)
  - All `*_op_eol`, `dot_op`, `open_*`/`close_*`, `eoe`/`fn_eoe`/`do_eoe`/`block_eoe`
  - All structural constructs (blocks, containers, calls, args, keywords, assoc, dot forms, access, etc.)

Conceptually (OPUS-style):

- **Pratt core**: the three `*_expr` families plus their `*_op_expr` companions.
- **RD**: everything else.
- **Helpers** (a conceptual subcategory of RD): operator-token rules (`*_op_eol`, `dot_op`) and token wrappers (`open_*`, `close_*`, `eoe`, etc.) that exist primarily to support Pratt and RD entry points.

This matches the intent of all three documents while being explicit about where Pratt actually runs and how the helper rules support it.

