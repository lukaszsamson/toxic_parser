# Nonterminals Parsing Strategy Analysis

This document categorizes each nonterminal from the Elixir grammar by recommended parsing approach for a hybrid recursive descent/Pratt parser.

## Legend
- **PRATT**: Use Pratt parsing (precedence climbing) - ideal for operators and expressions
- **RD**: Use Recursive Descent - ideal for structured constructs
- **HELPER**: Simple helper rule, typically inlined or trivial token consumption

---

## 1. grammar (6 rules)
**Approach: RD**

Entry point of the parser. Handles top-level expression lists with optional EOL markers. Recursive descent is natural here as it dispatches to expr_list parsing.

## 2. expr_list (2 rules)
**Approach: RD**

A list of expressions separated by EOE (end of expression). Simple iterative/recursive collection.

## 3. expr (3 rules)
**Approach: RD (dispatcher)**

Acts as a dispatcher to matched_expr, no_parens_expr, or unmatched_expr. The actual expression parsing happens in those rules.

## 4. matched_expr (7 rules)
**Approach: PRATT**

Core expression rule with operators. Contains:
- Binary operators via matched_op_expr
- Unary operators (unary_op_eol, at_op_eol, capture_op_eol, ellipsis_op)
- Operands (no_parens_one_expr, sub_matched_expr)

This is a prime candidate for Pratt parsing with precedence levels.

## 5. unmatched_expr (9 rules)
**Approach: PRATT**

Similar to matched_expr but for expressions containing do-blocks. Contains operator chains and unary operators. Pratt parsing handles the precedence naturally.

## 6. no_parens_expr (7 rules)
**Approach: PRATT**

Expressions without parentheses in call context. Contains operators and unary prefixes. Use Pratt parsing.

## 7. block_expr (5 rules)
**Approach: RD**

Function calls with do-blocks. These are structured constructs:
- `identifier(args) do ... end`
- `identifier args do ... end`

Recursive descent handles the sequential structure well.

## 8. matched_op_expr (19 rules)
**Approach: PRATT (integrated)**

These are the RHS of binary operator rules. In Pratt parsing, these become the infix parsing functions. Each rule corresponds to an operator with specific precedence.

Not parsed standalone - integrated into the Pratt expression parser.

## 9. unmatched_op_expr (18 rules)
**Approach: PRATT (integrated)**

Same as matched_op_expr but for unmatched context. Integrated into Pratt parser.

## 10. no_parens_op_expr (19 rules)
**Approach: PRATT (integrated)**

Same pattern, for no_parens context. Includes special case for `when_op_eol call_args_no_parens_kw`.

## 11. no_parens_one_ambig_expr (2 rules)
**Approach: RD**

Ambiguous single-argument calls without parens: `f g a, b`. This is a specific call form, not an operator expression.

## 12. no_parens_many_expr (2 rules)
**Approach: RD**

Multi-argument calls without parens: `f a, b, c`. Structured call syntax.

## 13. no_parens_one_expr (2 rules)
**Approach: RD**

Single-argument calls without parens: `f a`. Part of call syntax, not operator parsing.

## 14. no_parens_zero_expr (2 rules)
**Approach: RD**

Zero-argument identifiers (variables, nullary calls). Direct identifier handling.

## 15. sub_matched_expr (5 rules)
**Approach: RD**

Leaf expressions for matched context:
- no_parens_zero_expr
- range_op (nullary)
- ellipsis_op (nullary)
- access_expr
- error case for invalid kw_identifier

These are atomic or base expressions fed to the Pratt parser.

## 16. access_expr (29 rules)
**Approach: RD**

Primary/atomic expressions - the "atoms" of the expression language:
- Literals (int, flt, char, strings, atoms, booleans, nil)
- Containers (list, map, tuple, bitstring)
- Brackets and parens expressions
- fn expressions
- capture expressions

Recursive descent naturally handles these diverse constructs.

## 17. parens_call (2 rules)
**Approach: RD**

Function calls with parentheses: `f(args)` or `f(args)(args)`. Sequential structure.

## 18. bracket_arg (4 rules)
**Approach: RD**

Access bracket contents: `[key]` or `[key: value]`. Container-like parsing.

## 19. bracket_expr (2 rules)
**Approach: RD**

Access expressions: `expr[key]`. Postfix operation, can be handled in Pratt as postfix or in RD.

## 20. bracket_at_expr (2 rules)
**Approach: RD**

`@attr[key]` - combination of @ prefix and bracket access.

## 21. do_block (4 rules)
**Approach: RD**

`do ... end` blocks with optional stab clauses and block_list. Clearly structured.

## 22. eoe (3 rules)
**Approach: HELPER**

End-of-expression markers: eol, ';', or both. Simple token consumption.

## 23. fn_eoe (2 rules)
**Approach: HELPER**

`fn` keyword with optional eoe. Token consumption.

## 24. do_eoe (2 rules)
**Approach: HELPER**

`do` keyword with optional eoe. Token consumption.

## 25. block_eoe (2 rules)
**Approach: HELPER**

Block identifier (else, catch, rescue, after) with optional eoe.

## 26. stab (2 rules)
**Approach: RD**

List of stab expressions (clauses separated by eoe). Iterative collection.

## 27. stab_eoe (2 rules)
**Approach: RD**

Stab list with optional trailing eoe.

## 28. stab_expr (7 rules)
**Approach: RD with PRATT for expressions**

Stab clauses: `pattern -> body` or just expressions. The pattern parsing may use Pratt, but the `->` structure is RD.

## 29. stab_op_eol_and_expr (2 rules)
**Approach: RD**

`-> expr` - the arrow and RHS of a stab clause.

## 30. block_item (2 rules)
**Approach: RD**

Block clause: `else stab_eoe` or `else`. Structured.

## 31. block_list (2 rules)
**Approach: RD**

List of block items. Iterative collection.

## 32. open_paren (2 rules)
**Approach: HELPER**

`(` with optional eol. Token consumption.

## 33. close_paren (2 rules)
**Approach: HELPER**

`)` with optional preceding eol. Token consumption.

## 34. empty_paren (1 rules)
**Approach: HELPER**

`()` - open followed by close.

## 35. open_bracket (2 rules)
**Approach: HELPER**

`[` with optional eol.

## 36. close_bracket (2 rules)
**Approach: HELPER**

`]` with optional preceding eol.

## 37. open_bit (2 rules)
**Approach: HELPER**

`<<` with optional eol.

## 38. close_bit (2 rules)
**Approach: HELPER**

`>>` with optional preceding eol.

## 39. open_curly (2 rules)
**Approach: HELPER**

`{` with optional eol.

## 40. close_curly (2 rules)
**Approach: HELPER**

`}` with optional preceding eol.

## 41. unary_op_eol (6 rules)
**Approach: HELPER (PRATT prefix)**

Unary operators with optional eol: unary_op, dual_op (as unary), ternary_op (as unary).
In Pratt parsing, these define prefix parselets.

## 42. capture_op_eol (2 rules)
**Approach: HELPER (PRATT prefix)**

`&` operator with optional eol. Prefix parselet.

## 43. at_op_eol (2 rules)
**Approach: HELPER (PRATT prefix)**

`@` operator with optional eol. Prefix parselet.

## 44. match_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`=` operator with optional eol. Infix parselet, right associative, precedence 100.

## 45. dual_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`+`, `-` operators with optional eol. Infix parselet, left associative, precedence 210.

## 46. mult_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`*`, `/` operators with optional eol. Infix parselet, left associative, precedence 220.

## 47. power_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`**` operator with optional eol. Infix parselet, left associative, precedence 230.

## 48. concat_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`++`, `--`, `<>`, etc. with optional eol. Infix parselet, right associative, precedence 200.

## 49. range_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`..` operator with optional eol. Infix parselet, right associative, precedence 200.

## 50. ternary_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`//` operator with optional eol. Infix parselet, right associative, precedence 190.

## 51. xor_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`^^^` operator with optional eol. Infix parselet, left associative, precedence 180.

## 52. pipe_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`|` operator with optional eol. Infix parselet, right associative, precedence 70.

## 53. and_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`&&`, `and`, `&&&` with optional eol. Infix parselet, left associative, precedence 130.

## 54. or_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`||`, `or`, `|||` with optional eol. Infix parselet, left associative, precedence 120.

## 55. in_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`in`, `not in` with optional eol. Infix parselet, left associative, precedence 170.

## 56. in_match_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`<-`, `\\\\` with optional eol. Infix parselet, left associative, precedence 40.

## 57. type_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`::` operator with optional eol. Infix parselet, right associative, precedence 60.

## 58. when_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`when` operator with optional eol. Infix parselet, right associative, precedence 50.

## 59. stab_op_eol (2 rules)
**Approach: HELPER (special)**

`->` operator with optional eol. Precedence 10, but typically handled specially in stab context rather than general expression parsing.

## 60. comp_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`==`, `!=`, `=~`, `===`, `!==` with optional eol. Infix parselet, left associative, precedence 140.

## 61. rel_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`<`, `>`, `<=`, `>=` with optional eol. Infix parselet, left associative, precedence 150.

## 62. arrow_op_eol (2 rules)
**Approach: HELPER (PRATT infix)**

`|>`, `<<<`, `>>>`, etc. with optional eol. Infix parselet, left associative, precedence 160.

## 63. dot_op (2 rules)
**Approach: HELPER (PRATT infix/postfix)**

`.` operator with optional eol. Very high precedence (310), left associative. Can be handled as infix in Pratt.

## 64. dot_identifier (2 rules)
**Approach: RD with PRATT for dot**

Plain identifier or `expr.identifier`. The dot is high-precedence infix.

## 65. dot_alias (4 rules)
**Approach: RD with PRATT for dot**

Alias or `expr.Alias` or `expr.{...}`. Module path construction.

## 66. dot_op_identifier (2 rules)
**Approach: RD with PRATT for dot**

Op identifier or `expr.op_identifier`.

## 67. dot_do_identifier (2 rules)
**Approach: RD with PRATT for dot**

Do identifier or `expr.do_identifier`.

## 68. dot_bracket_identifier (2 rules)
**Approach: RD with PRATT for dot**

Bracket identifier or `expr.bracket_identifier`.

## 69. dot_paren_identifier (2 rules)
**Approach: RD with PRATT for dot**

Paren identifier or `expr.paren_identifier`.

## 70. dot_call_identifier (2 rules)
**Approach: RD with PRATT for dot**

Paren identifier or `expr.()` (anonymous function call).

## 71. call_args_no_parens_expr (2 rules)
**Approach: RD**

Arguments allowed in no-parens calls. Validates against ambiguous nesting.

## 72. call_args_no_parens_comma_expr (2 rules)
**Approach: RD**

Comma-separated args for no-parens calls. Iterative collection.

## 73. call_args_no_parens_all (3 rules)
**Approach: RD**

Union of all no-parens argument forms. Dispatcher.

## 74. call_args_no_parens_one (2 rules)
**Approach: RD**

Single argument (keyword or matched_expr) for no-parens calls.

## 75. call_args_no_parens_ambig (1 rules)
**Approach: RD**

Single no_parens_expr argument (ambiguous nesting).

## 76. call_args_no_parens_many (3 rules)
**Approach: RD**

Multiple arguments for no-parens calls.

## 77. call_args_no_parens_many_strict (3 rules)
**Approach: RD**

call_args_no_parens_many with error handling for space-before-parens.

## 78. stab_parens_many (2 rules)
**Approach: RD**

Parenthesized arguments in stab clause patterns.

## 79. container_expr (3 rules)
**Approach: RD**

Expressions allowed in containers (list, tuple, etc.). Validates nesting.

## 80. container_args_base (2 rules)
**Approach: RD**

Base comma-separated container args. Iterative.

## 81. container_args (3 rules)
**Approach: RD**

Container args with optional trailing comma and keywords.

## 82. call_args_parens_expr (3 rules)
**Approach: RD**

Expressions allowed in parenthesized call args.

## 83. call_args_parens_base (2 rules)
**Approach: RD**

Base comma-separated call args. Iterative.

## 84. call_args_parens (5 rules)
**Approach: RD**

Full parenthesized argument list: `(args)`.

## 85. kw_eol (6 rules)
**Approach: HELPER**

Keyword key (identifier with colon) with optional eol.

## 86. kw_base (2 rules)
**Approach: RD**

Base keyword pairs. Iterative collection.

## 87. kw_call (3 rules)
**Approach: RD**

Keyword list in call context with validation.

## 88. kw_data (3 rules)
**Approach: RD**

Keyword list in data context with validation.

## 89. call_args_no_parens_kw_expr (2 rules)
**Approach: RD**

Single keyword pair for no-parens calls.

## 90. call_args_no_parens_kw (3 rules)
**Approach: RD**

Keyword list for no-parens calls.

## 91. list_args (4 rules)
**Approach: RD**

Arguments inside list brackets.

## 92. list (2 rules)
**Approach: RD**

List literal: `[]` or `[args]`.

## 93. tuple (3 rules)
**Approach: RD**

Tuple literal: `{}` or `{args}`.

## 94. bitstring (3 rules)
**Approach: RD**

Bitstring literal: `<<>>` or `<<args>>`.

## 95. map_base_expr (4 rules)
**Approach: RD with PRATT for unary**

Expressions allowed as map/struct base (before `%`).

## 96. assoc_op_eol (2 rules)
**Approach: HELPER**

`=>` operator with optional eol.

## 97. assoc_expr (5 rules)
**Approach: RD**

Association expression: `key => value` or just expression.

## 98. assoc_update (2 rules)
**Approach: RD**

Map update: `expr | assoc`.

## 99. assoc_update_kw (2 rules)
**Approach: RD**

Map update with keywords: `expr | kw_data`.

## 100. assoc_base (2 rules)
**Approach: RD**

Base association pairs. Iterative.

## 101. assoc (2 rules)
**Approach: RD**

Association list with optional trailing comma.

## 102. map_op (2 rules)
**Approach: HELPER**

`%{}` token with optional eol.

## 103. map_close (3 rules)
**Approach: RD**

Map closing contents (associations or keywords).

## 104. map_args (6 rules)
**Approach: RD**

Map contents including updates.

## 105. map (3 rules)
**Approach: RD**

Map literal: `%{}`, `%{args}`, or struct `%Name{args}`.

---

## Summary

### PRATT Parsing (Expression/Operator Parsing)
Core expression nonterminals that involve operator precedence:
- **matched_expr** (4)
- **unmatched_expr** (5)
- **no_parens_expr** (6)
- **matched_op_expr** (8) - integrated as infix rules
- **unmatched_op_expr** (9) - integrated as infix rules
- **no_parens_op_expr** (10) - integrated as infix rules

### HELPER (Operator Token Rules for Pratt)
These define the operators and their precedence/associativity:
- All `*_op_eol` rules (41-62) - define prefix/infix parselets
- **dot_op** (63) - very high precedence infix

### Recursive Descent
Everything else - structured constructs:
- Entry/dispatch: grammar, expr_list, expr
- Calls: block_expr, parens_call, no_parens_*_expr (11-14)
- Atomics: sub_matched_expr, access_expr
- Containers: list, tuple, bitstring, map, bracket_*
- Blocks: do_block, stab*, block_*
- Arguments: call_args_*, container_args*, kw_*
- Dot constructs: dot_identifier, dot_alias, etc. (64-70)

### Token Helpers
Simple token consumption, often inlined:
- open_*/close_* (32-40)
- eoe, fn_eoe, do_eoe, block_eoe (22-25)
- map_op, assoc_op_eol (96, 102)

---

## Recommended Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Parser Entry (RD)                        │
│                       grammar                               │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   Expression Dispatch (RD)                  │
│              expr → matched/unmatched/no_parens             │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    Pratt Expression Parser                  │
│  ┌─────────────────────────────────────────────────────┐   │
│  │ Prefix parselets:                                    │   │
│  │   - unary_op, dual_op (as unary), ternary_op        │   │
│  │   - capture_op (&), at_op (@), ellipsis_op (...)    │   │
│  │   - Atoms/primaries → call RD for access_expr       │   │
│  └─────────────────────────────────────────────────────┘   │
│  ┌─────────────────────────────────────────────────────┐   │
│  │ Infix parselets (by precedence):                     │   │
│  │   310: dot_op, dot_call_op                          │   │
│  │   320: at_op (postfix?)                             │   │
│  │   300: unary_op (n/a for infix)                     │   │
│  │   230: power_op (**)                                │   │
│  │   220: mult_op (*, /)                               │   │
│  │   210: dual_op (+, -)                               │   │
│  │   200: concat_op, range_op                          │   │
│  │   190: ternary_op (//)                              │   │
│  │   180: xor_op (^^^)                                 │   │
│  │   170: in_op                                        │   │
│  │   160: arrow_op (|>, etc.)                          │   │
│  │   150: rel_op (<, >, etc.)                          │   │
│  │   140: comp_op (==, etc.)                           │   │
│  │   130: and_op                                       │   │
│  │   120: or_op                                        │   │
│  │   100: match_op (=)                                 │   │
│  │    90: capture_op (&), ellipsis_op (...)            │   │
│  │    80: assoc_op (=>)                                │   │
│  │    70: pipe_op (|)                                  │   │
│  │    60: type_op (::)                                 │   │
│  │    50: when_op                                      │   │
│  │    40: in_match_op (<-, \\)                         │   │
│  │    10: stab_op (->)                                 │   │
│  │     5: do                                           │   │
│  └─────────────────────────────────────────────────────┘   │
│  ┌─────────────────────────────────────────────────────┐   │
│  │ Postfix parselets:                                   │   │
│  │   - bracket_arg (access: expr[key])                 │   │
│  │   - call_args_parens (call: expr(...))              │   │
│  │   - do_block (call: expr do...end)                  │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                 Primary/Atomic Parser (RD)                  │
│                      access_expr                            │
│  - Literals: int, flt, char, atom, string, etc.            │
│  - Containers: list, tuple, map, bitstring                 │
│  - Constructs: fn...end, (stab), parens_call               │
│  - Identifiers: various dot_* forms                        │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│               Container/Block Parsers (RD)                  │
│  - list, tuple, bitstring, map                             │
│  - do_block, stab, block_list                              │
│  - call_args_*, container_args, kw_*                       │
└─────────────────────────────────────────────────────────────┘
```

## Key Implementation Notes

1. **Context-Sensitive Parsing**: The grammar distinguishes matched/unmatched/no_parens expressions. The Pratt parser needs to be aware of context to know which RHS rules apply.

2. **Do-Block Ambiguity**: The `do` keyword has very low precedence (5) to handle `if cond do ... end`. This needs special handling.

3. **Stab Operator**: The `->` is tricky as it's used both as an operator and as clause separator in `fn`/`case`/etc. Handle specially in stab context.

4. **No-Parens Calls**: These require careful lookahead to distinguish `f a, b` from `f(a), b`. The tokenizer helps by marking identifier types.

5. **Newline Handling**: Many `*_eol` rules allow optional newlines after operators. The Pratt parser should consume these transparently.

6. **Error Recovery**: Many rules exist for better error messages (e.g., error_no_parens_strict). Integrate these into the parser.
