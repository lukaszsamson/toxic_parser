Parser Review: Conformance with Design Documents

  Overall Assessment: The parser is following the high-level design, but with some deviations and incomplete areas.

  ---
  1. Hybrid RD/Pratt Approach (per NONTERMINALS_OPUS.md)

  ✅ Correctly Applied:

  | Component                                  | Design Doc         | Implementation                                                   | Status |
  |--------------------------------------------|--------------------|------------------------------------------------------------------|--------|
  | grammar                                    | RD                 | Grammar.parse_string/2 → Expressions.expr_list/3                 | ✅      |
  | expr_list                                  | RD                 | Expressions.expr_list/3                                          | ✅      |
  | expr                                       | RD (dispatcher)    | Expressions.expr/3 → dispatches to Blocks/Containers/Calls/Pratt | ✅      |
  | matched_expr/unmatched_expr/no_parens_expr | PRATT              | Pratt.parse/3 with context parameter                             | ✅      |
  | *_op_expr (binary ops)                     | PRATT (integrated) | LED in Pratt.led/5                                               | ✅      |
  | block_expr                                 | RD                 | Blocks.parse/3 + Calls.maybe_do_block/4                          | ✅      |
  | access_expr                                | RD                 | Pratt.led/5 handles [ + parse_access_indices                     | ✅      |
  | list/tuple/map                             | RD                 | Containers.parse/3                                               | ✅      |
  | do_block                                   | RD                 | Blocks.parse_do_block/3                                          | ✅      |
  | stab/clauses                               | RD                 | Blocks.parse_clauses/5, parse_clause/4                           | ✅      |
  | fn                                         | RD                 | Blocks.parse_fn/4                                                | ✅      |
  | case/cond/with/try/receive/for             | RD                 | Blocks.parse_keyword_block/5                                     | ✅      |
  | Operator helpers (*_op_eol)                | HELPER             | Integrated into Pratt + skip_eoe_after_op                        | ✅      |

  ⚠️ Deviations/Issues:

  1. Context not fully threaded: The context parameter (:matched | :unmatched | :no_parens) is passed through but not always semantically used. Per HLD §4.26:
  "Required parser context is syntactic expression category only (:matched | :unmatched | :no_parens)"

  1. Currently the context is threaded but many functions treat all contexts identically.
  2. no_parens_one/many/one_ambig not fully distinguished: Per NONTERMINALS_OPUS.md §11-13, these should be separate RD rules. Currently Calls.parse_no_parens_call/5 only handles single-argument no-parens
   calls (foo arg), not multi-arg (foo a, b) or ambiguous (f g a, b) forms.
  3. sub_matched_expr not explicitly separated: The design calls for this as the "leaf expressions for matched context" but it's absorbed into literal_to_ast/1 in Pratt.

  ---
  2. Precedence Table (per HLD §5)

  ✅ Mostly Correct:

  The Precedence module matches HLD §5 binding powers:

  | Operator     | HLD BP         | Implementation | Status |
  |--------------|----------------|----------------|--------|
  | do           | 5 (L)          | 5 (left)       | ✅      |
  | ->           | 10 (R)         | 10 (right)     | ✅      |
  | ,            | 20 (L)         | 20 (left)      | ✅      |
  | <-/\\        | 40 (L)         | 40 (left)      | ✅      |
  | when         | 50 (R)         | 50 (right)     | ✅      |
  | ::           | 60 (R)         | 60 (right)     | ✅      |
  | |            | 70 (R)         | 70 (right)     | ✅      |
  | =>           | 80 (R)         | 80 (right)     | ✅      |
  | &/...        | 90 (nonassoc)  | 90 (nonassoc)  | ✅      |
  | =            | 100 (R)        | 100 (right)    | ✅      |
  | or/||/|||    | 120 (L)        | 120 (left)     | ✅      |
  | and/&&/&&&   | 130 (L)        | 130 (left)     | ✅      |
  | comparisons  | 140 (L)        | 140 (left)     | ✅      |
  | relations    | 150 (L)        | 150 (left)     | ✅      |
  | arrow family | 160 (L)        | 160 (left)     | ✅      |
  | in/not in    | 170 (L)        | 170 (left)     | ✅      |
  | ^^^          | 180 (L)        | 180 (left)     | ✅      |
  | //           | 190 (R)        | 190 (right)    | ✅      |
  | concat/range | 200 (R)        | 200 (right)    | ✅      |
  | +/-          | 210 (L)        | 210 (left)     | ✅      |
  | *//          | 220 (L)        | 220 (left)     | ✅      |
  | **           | 230 (L)        | 230 (left)     | ✅      |
  | unary        | 300 (nonassoc) | 300 (nonassoc) | ✅      |
  | dot          | 310 (L)        | 310 (left)     | ✅      |
  | @            | 320 (nonassoc) | 320 (nonassoc) | ✅      |
  | access       | 330 (nonassoc) | 330 (nonassoc) | ✅      |

  ⚠️ Missing/Issues:

  1. not in combined operator: HLD mentions "combined operators (not in rewrite to not(in ...) with warning)". The Precedence.not_in/0 function exists but the actual parsing/rewrite logic in Pratt doesn't
   appear to handle not in as a combined operator.
  2. ..// step constraint: HLD §5 mentions "190 (R, special with `..")". The special step handling isn't implemented.

  ---
  3. Grammar Layer Structure (per HL_PLAN.md)

  ✅ Correct Structure:

  Grammar (entry) → Expressions.expr_list
                        ↓
                    Expressions.expr (dispatcher)
                        ↓
             ┌─────────┴─────────┬──────────────┐
          Blocks.parse    Containers.parse    Calls.parse
                                                 ↓
                                            Pratt.parse

  This matches the architecture diagram in HL_PLAN.md exactly.

  ⚠️ Issues:

  1. expr isn't dispatching to Pratt directly for operators: Per NONTERMINALS_OPUS.md, expr should be "RD (dispatcher)" that routes to matched_expr/unmatched_expr/no_parens_expr. Currently it routes
  through Blocks → Containers → Calls → Pratt, which is mostly correct but the semantic distinction of expression families is lost.

  ---
  4. Token Adapter (per HLD §3 and HL_PLAN Phase 1)

  ✅ Implemented:

  - EOE normalization (eol/; → :eoe) ✅
  - peek/next/peek_n ✅
  - Checkpoint/rewind ✅
  - Terminator tracking ✅
  - Tolerant mode error token passthrough ✅
  - Synthesized token detection ✅
  - Ranged metadata ✅

  ⚠️ Missing:

  - pushback exists but the design talks about "pre_terms snapshots" which isn't fully utilized
  - Bounded peek_n is enforced (max_peek) but the specific ambiguity cases from HLD §5 aren't clearly using it

  ---
  5. Event Log & Builders (per HLD §3 and HL_PLAN Phase 2)

  ⚠️ Partially Implemented:

  Per Blocks.ex, environment events are emitted (enter_scope/exit_scope/bind), and EventLog module exists. However:

  1. Event log not systematically used: start_node/end_node events aren't emitted during parsing - only at the top level in Grammar.parse_string/2
  2. Builder pattern exists but actual builders for CST/outline aren't implemented
  3. Comments not preserved (Phase 10 per HL_PLAN)

  ---
  6. Error Recovery (per HLD §6 and HL_PLAN Phase 9)

  ⚠️ Minimal Implementation:

  - Lexer error_token → diagnostic in tolerant mode ✅
  - Per-rule sync sets NOT implemented - no sync_to/3 or similar helper
  - Missing-token synthesis NOT implemented - no missing events emitted
  - Fuel/loop guard exists in State but not enforced during parsing

  ---
  7. Missing Components (per HL_PLAN)

  | Component                    | Phase    | Status              |
  |------------------------------|----------|---------------------|
  | Strings/sigils/heredocs      | Phase 8  | ❌ Not implemented   |
  | Bitstring patterns           | Phase 6  | ❌ Not in Containers |
  | Comment preservation         | Phase 10 | ❌ Not implemented   |
  | Pattern matching semantics   | Phase 7  | ⚠️ Basic only       |
  | not in rewrite               | Phase 3  | ❌ Not implemented   |
  | ..// step constraint         | Phase 3  | ❌ Not implemented   |
  | capture_int handling         | Phase 3  | ❌ Not implemented   |
  | Quoted identifiers/atoms     | Phase 8  | ❌ Not implemented   |
  | maybe_bad_keyword_* warnings | Phase 5  | ❌ Not implemented   |
  | warn_pipe                    | Phase 4  | ❌ Not implemented   |

  ---
  Summary

  Strengths:
  1. Core architecture correctly follows the hybrid RD/Pratt design
  2. Precedence table is accurate
  3. Grammar dispatcher structure matches the plan
  4. Token adapter is well-implemented
  5. Basic block/clause/container parsing works

  Areas Needing Work:
  1. Expression context (:matched/:unmatched/:no_parens) isn't semantically distinguished
  2. No-parens call parsing is incomplete (only single-arg)
  3. Error recovery infrastructure is minimal
  4. Event log integration is shallow
  5. Strings/sigils/heredocs/bitstrings not implemented
  6. Several operator-specific behaviors missing (not in, ..//, capture_int)

  Verdict: The parser is at approximately Phase 6 completion per HL_PLAN.md, with solid foundations but significant gaps in string handling, recovery, and advanced call forms.


  Grammar Rules Visibility Analysis

  ✅ Clearly Visible & Well-Mapped

  | YRL Rule                                               | Implementation Location                | Notes                                           |
  |--------------------------------------------------------|----------------------------------------|-------------------------------------------------|
  | grammar -> eoe? expr_list eoe?                         | Grammar.Expressions.expr_list/3        | Lines 25-40 handle leading/trailing EOE         |
  | expr_list -> expr | expr_list eoe expr                 | Expressions.collect_exprs/4            | Lines 76-109                                    |
  | expr -> matched_expr | no_parens_expr | unmatched_expr | Expressions.expr/3 dispatches to Pratt | Context parameter exists but not fully utilized |
  | eoe -> eol | ';' | eol ';'                             | TokenAdapter.normalize_token/3         | Lines 175-195 normalize to :eoe                 |
  | do_block -> do_eoe stab_eoe 'end'                      | Blocks.parse_do_block/3                | Lines 110-135                                   |
  | block_list -> block_item+                              | Blocks.parse_labeled_sections/5        | Lines 151-170                                   |
  | stab -> stab_expr | stab eoe stab_expr                 | Blocks.parse_clauses/5                 | Lines 311-334                                   |
  | stab_expr -> ... stab_op_eol_and_expr                  | Blocks.parse_clause/4                  | Lines 336-363                                   |
  | fn_eoe -> 'fn' eoe?                                    | Blocks.parse_fn/4                      | Line 42                                         |
  | list -> '[' list_args? ']'                             | Containers.parse_list/4                | Lines 37-43                                     |
  | tuple -> '{' container_args? '}'                       | Containers.parse_tuple/4               | Lines 45-57                                     |
  | map -> map_op map_args                                 | Containers.parse_map/4                 | Lines 59-74                                     |
  | matched_expr -> matched_expr matched_op_expr           | Pratt.led/5                            | Binary op chaining                              |
  | matched_expr -> unary_op_eol matched_expr              | Pratt.nud/4 + parse_unary/5            | Lines 55-73, 97-123                             |
  | *_op_eol -> op | op eol                                | Pratt.skip_eoe_after_op/2              | Lines 278-287                                   |
  | Precedence table (lines 69-97)                         | Precedence module                      | Exact match                                     |

  ⚠️ Partially Visible / Incomplete

  | YRL Rule                                                                              | Issue                                                                                                          |
  |---------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------|
  | no_parens_one_expr -> dot_identifier call_args_no_parens_one (line 258-259)           | Calls.parse_no_parens_call/5 only handles single arg, not full call_args_no_parens_one which includes keywords |
  | no_parens_many_expr -> dot_identifier call_args_no_parens_many_strict (lines 255-256) | Not implemented - no multi-arg no-parens calls                                                                 |
  | no_parens_one_ambig_expr (lines 252-253)                                              | Not implemented - ambiguous nesting like f g a, b                                                              |
  | call_args_no_parens_all (lines 508-510)                                               | Only call_args_no_parens_one partially done                                                                    |
  | block_expr variants (lines 181-185)                                                   | Only basic dot_do_identifier do_block works via Calls.maybe_do_block                                           |
  | sub_matched_expr (lines 263-267)                                                      | Absorbed into Pratt.literal_to_ast/1, no explicit separation                                                   |
  | container_expr -> no_parens_expr : error_no_parens_container_strict (line 535)        | No validation that no-parens is rejected in containers                                                         |

  ❌ Not Visible / Missing

  | YRL Rule                                                                                     | Status                                              |
  |----------------------------------------------------------------------------------------------|-----------------------------------------------------|
  | access_expr -> bin_string | list_string | bin_heredoc | list_heredoc | sigil (lines 290-295) | Strings/sigils not implemented                      |
  | access_expr -> atom_quoted | atom_safe | atom_unsafe (lines 297-299)                         | Quoted atoms not implemented                        |
  | access_expr -> bitstring (line 294)                                                          | Bitstring not in Containers                         |
  | bracket_expr -> access_expr bracket_arg (line 313)                                           | Access is in Pratt but bracket_arg rules incomplete |
  | bracket_at_expr (lines 315-318)                                                              | @foo[key] pattern not implemented                   |
  | parens_call -> dot_call_identifier call_args_parens call_args_parens (line 305)              | Nested parens calls not implemented                 |
  | dot_alias -> matched_expr dot_op alias (line 481)                                            | Dotted aliases not properly chained                 |
  | dot_alias -> matched_expr dot_op '{' container_args '}' (lines 482-483)                      | Multi-alias Foo.{Bar, Baz} not implemented          |
  | build_op for // (lines 739-746)                                                              | Range step ..// constraint not implemented          |
  | build_op for not in (lines 748-757)                                                          | not in rewrite not implemented                      |
  | kw_eol, kw_base, kw_call, kw_data (lines 566-582)                                            | Keyword list parsing incomplete                     |
  | assoc_expr, assoc_update, assoc_update_kw (lines 623-633)                                    | Map update syntax %{map | key: val} incomplete      |
  | stab_parens_many (lines 528-529)                                                             | Parenthesized stab args like fn (a, b) -> ... end   |
  | warn_pipe (lines 1285-1298)                                                                  | Pipe ambiguity warning not implemented              |
  | warn_no_parens_after_do_op (lines 1301-1308)                                                 | Missing parens warning not implemented              |
  | maybe_bad_keyword_* (lines 1209-1229)                                                        | Keyword-last enforcement not implemented            |
  | error_no_parens_strict (line 1231)                                                           | Space-before-parens error not implemented           |

  Structural Comparison

  The YRL has 105 nonterminals organized into clear sections:

  YRL Structure:
  ├── Main flow (grammar, expr_list, expr)
  ├── Expression families (matched_expr, unmatched_expr, no_parens_expr)  
  ├── Operator expressions (*_op_expr - 3 variants × 19 operators)
  ├── Call variants (no_parens_one/many/ambig, parens_call, block_expr)
  ├── Sub-expressions (sub_matched_expr, access_expr - 29 rules!)
  ├── Containers (list, tuple, bitstring, map with assoc/update)
  ├── Blocks (do_block, stab, block_list, fn)
  ├── Helpers (open/close_*, *_op_eol)
  ├── Keywords (kw_eol, kw_base, kw_call, kw_data)
  └── Dot constructs (dot_identifier, dot_alias, dot_*_identifier)

  The implementation mirrors this but with gaps:

  Implementation Structure:
  ├── Grammar.Expressions (grammar, expr_list, expr) ✅
  ├── Pratt (matched/unmatched/no_parens via context) ⚠️ context underutilized
  ├── Pratt.*_op handling (binary ops) ✅
  ├── Calls (partial - only simple no-parens) ⚠️
  ├── Pratt.literal_to_ast (sub_matched_expr, partial access_expr) ⚠️
  ├── Containers (list, tuple, map - no bitstring) ⚠️
  ├── Blocks (do_block, stab, fn, keyword blocks) ✅
  ├── TokenAdapter EOE helpers ✅
  ├── Keywords ❌ mostly missing
  └── Dot constructs ❌ mostly missing

  Key Gaps Summary

  1. No-parens call family: Only no_parens_one with single non-keyword arg works. no_parens_many, no_parens_ambig, keyword args all missing.
  2. access_expr is incomplete: Only 10 of 29 rules are implemented (literals, some containers). Missing: strings, sigils, heredocs, quoted atoms, bitstrings.
  3. Dot constructs: dot_identifier works for simple cases but dot_alias chaining, dot_bracket_identifier, dot_op_identifier behaviors are incomplete.
  4. Keyword lists: kw_eol, kw_base, kw_call, kw_data parsing not properly structured.
  5. Warnings/errors: warn_pipe, warn_no_parens_after_do_op, maybe_bad_keyword_*, error_no_parens_* not implemented.
  6. Special operators: // step constraint, not in rewrite, capture_int spacing not handled.

  Verdict: The core expression parsing structure from YRL is visible, but approximately 40% of the grammar rules are not yet implemented, primarily in strings/sigils, full no-parens call handling, keyword
   lists, and validation/warning logic.
