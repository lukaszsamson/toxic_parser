# Refactoring Opportunities (G3)

This document outlines refactoring opportunities identified during the G3 code review of `ToxicParser`.

## 1. Reduce Duplication in Call Parsing

There is significant logic duplication between `lib/toxic_parser/pratt.ex` and `lib/toxic_parser/grammar/calls.ex`.

*   **`can_be_no_parens_arg?/1`**: This function (checking if a token can start a no-parens argument) is duplicated in both modules. It should be centralized, likely in `ToxicParser.Identifiers` or a dedicated helper module.
*   **`parse_without_led/3`**: `Calls.parse_without_led` duplicates the dispatch logic of `Calls.parse` and `Pratt.parse` but without the `led` call. This parallel hierarchy is hard to maintain. A better approach would be to have a single parse entry point that accepts an option to skip `led`, or to restructure the recursion so `led` is always called at the appropriate level.

## 2. Centralize EOE Skipping Logic

Logic for skipping end-of-expression (EOE) tokens and counting newlines is scattered across multiple modules:
*   `Pratt.skip_eoe_after_op/2`
*   `Pratt.skip_eoe_count_newlines/2`
*   `CallsPrivate.skip_eoe/1`
*   `Keywords.skip_eoe/1`
*   `Maps.skip_eoe/1`
*   `Maps.skip_eoe_count_newlines/2`

These should be consolidated into `ToxicParser.TokenAdapter` or a `Grammar.Helpers` module to ensure consistent behavior and reduce code volume.

## 3. Decompose `Pratt.led/6`

The `Pratt.led/6` function is over 350 lines long and handles a wide variety of constructs:
*   Parenthesized calls `foo()`
*   Dot calls `foo.()`
*   Dot access `foo.bar`
*   Bracket access `foo[bar]`
*   Binary operators `a + b`
*   Ternary operator `..//`

This function should be decomposed into smaller, specialized functions (e.g., `led_call`, `led_dot`, `led_bracket`, `led_binary`) to improve readability and maintainability.

## 4. Improve Map Update Parsing Strategy

`Maps.try_parse_map_update/3` relies on backtracking (`checkpoint`/`rewind`) to distinguish between map updates (`%{a | b}`) and map entries (`%{a => b}`). It parses an expression and then checks for a pipe `|`. If the pipe is absent or if the parsed expression contained operators with lower precedence than pipe (meaning the pipe should have bound tighter), it rewinds.

This is inefficient and complex. A better approach might be to peek ahead for the pipe before parsing the base expression, or to parse the base expression with a specific binding power that naturally stops at the pipe if present, avoiding the need for deep backtracking.

## 5. Decouple Strings from Keywords

`Strings.parse` currently returns `{:keyword_key, ...}` or `{:keyword_key_interpolated, ...}` when it encounters a string followed by a colon (e.g., `"foo":`). This couples string parsing with keyword list parsing and leaks keyword logic into the string parser.

It would be cleaner if `Strings.parse` only returned string ASTs, and the caller (e.g., `Keywords.parse_kw_pair`) handled the conversion of a string AST followed by a colon into a keyword pair.

## 6. Clean Up `ToxicParser.ex`

*   **`build_result` duplication**: There are two nearly identical clauses for `build_result` in `lib/toxic_parser.ex`. These should be merged into a single function with default values.
*   **Hardcoded Error Position**: `parser_error` uses a hardcoded `{1, 1}` position. It should attempt to extract the actual error position from the `reason` or `state` if available.

## 7. Unify `nud` Logic

`Pratt.nud_base` and `Pratt.nud_with_min_bp` share significant logic for handling literals and unary operators. `nud_base` appears to be a restricted version of `nud_with_min_bp`. Refactoring to use a single `nud` function with options or flags to control the restricted behavior (no do-blocks, no containers) would reduce duplication.
