1) Alignment with HLD_DRAFT_1 / HL_PLAN

  - The core architecture matches the HLD/plan: there is a Toxic-facing TokenAdapter + State (lib/toxic_parser/token_adapter.ex, state.ex), a grammar dispatcher (Grammar.Expressions / Grammar), a
    Pratt core with a binding‑power table (Pratt, Precedence), an event log + env stream (EventLog, Env), a builder contract (Builder, Builder.Helpers), and a conformance harness (Conformance, test/
    conformance_test.exs).
  - Token adapter behavior is on spec: it normalizes eol/; into :eoe with newline counts, tracks terminators via Toxic.current_terminators/1, supports peek/peek_n/checkpoint/rewind, and converts :error_token
    from Toxic into diagnostics and synthetic tokens in tolerant mode.
  - The public API in ToxicParser.parse_string/2 now does go through the new grammar pipeline (Grammar.parse/3), and returns AST + optional events/env, which is exactly what the HLD describes (the README/
    Phase‑0 docs about delegating to Code.string_to_quoted* are stale).
  - Significant HLD items are still only partially implemented: tokens/nodes are barely emitted into the event log (root node + env events only; no systematic :token/start_node/end_node usage yet), there is
    no real recovery module (:missing/:synthetic events, sync sets, fuel guard) beyond the error_token path, and strings/sigils/heredocs/quoted identifiers + comment preservation are not wired into the parser
    yet.
  - Environment events and a minimal env accumulator exist and are used by block parsing (Blocks emits :enter_scope, :exit_scope, and :bind events; Env.from_events/1 accumulates them), which is directly in
    line with the HLD “environment stream” section.

  2) RD/Pratt hybrid vs NONTERMINALS_OPUS

  - At a coarse level, the split matches NONTERMINALS_OPUS.md and NONTERMINALS_GPT.md:
      - RD: grammar/expr_list dispatch (Grammar.Expressions.expr_list/3 and expr/3), block constructs (Blocks for fn, case/cond/with/try/receive/for, do_block, stab), containers (Containers for lists/tuples/
        maps/bitstrings), and call dispatch (Calls for paren vs no‑parens vs bare identifier).
      - Pratt: operator chains across matched/unmatched/no_parens contexts (Pratt + Precedence), including unary prefixes and binary infix operators.
  - The binding‑power/associativity table in Precedence is a faithful lift of the elixir_parser.yrl precedence section (same numeric levels and assoc for match_op, and_op, or_op, in_op, pipe_op, range_op,
    concat_op, unary_op, at_op, etc.), and Pratt.led/5 uses it correctly (left vs right assoc via bp+1 vs bp).
  - RD is correctly used for structured constructs: Blocks mirrors stab clauses and labeled sections; Containers iteratively parses list/tuple/map elements; Calls decides between paren and (very simple)
    no‑parens calls and only falls back to Pratt when an identifier is not actually a call.
  - Deviations / gaps relative to NONTERMINALS_OPUS:
      - matched_expr / unmatched_expr / no_parens_expr are not implemented as distinct RD wrappers; the Pratt context parameter is threaded through but almost never inspected, so context‑sensitive behavior
        (especially for no‑parens families) is still largely missing.
      - The no_parens_* and call_args_* nonterminal families are heavily collapsed: Calls.parse_no_parens_call/4 just parses a single Expressions.expr as an argument, without distinguishing no_parens_one/
        one_ambig/many/zero or enforcing all the ambiguity rules described in NONTERMINALS_OPUS.md.
      - Access/bracket rules are partly handled as Pratt postfix (Pratt.parse_access_indices/4 from led/5) instead of a full RD access_expr/bracket_expr/bracket_at_expr layer, and bracket_at_expr semantics
        are not present yet.
      - Some operator helper nonterminals are only partly represented: e.g. unary_not_op is referenced in tests but not present in Precedence.unary/1; do is handled as a structural block via
        Blocks.parse_do_block/3, not as a true low‑precedence “do_op” binding power.
  - So: the pattern of “RD for structure, Pratt for operator expressions” is applied correctly for implemented areas, but the nonterminal‑by‑nonterminal mapping from NONTERMINALS_OPUS.md is only partially
    realized and still quite approximate for no‑parens calls, dot/access, and some helpers.

  3) Visibility of elixir_parser.yrl grammar rules

  - Many top‑level and operator rules from elixir_parser.yrl are clearly mirrored:
      - The grammar/expr_list/eoe rules are explicitly described and implemented in Grammar.Expressions.expr_list/3 (docstring quotes the yrl rules, and the code handles leading/trailing EOEs exactly as in
        the grammar).
      - The precedence section is reified in Precedence and exercised by Pratt and test/pratt_precedence_test.exs.
      - Block/stab/do‑block structure (do_block, stab, stab_eoe, guards with when, labeled else/catch/rescue/after) is clearly visible in Blocks and in the corresponding portions of test/conformance_test.exs.
      - Basic containers (list, tuple, map, %{map | update}) and bitstrings are visible in Containers and the conformance tests.
  - The conformance suite is organized by yrl sections and names (each group of tests cites the original rule comments), which makes the intended mapping from elixir_parser.yrl → ToxicParser explicit even
    when the implementation is not 1:1.
  - However, there is not a complete “one function per nonterminal” mirroring: a lot of detailed rules are either collapsed or not yet implemented in code—particularly the no_parens_* call families, keyword/
    kw_* rules, the full assoc_*/map update matrix, dot alias/call families, sigils/heredocs/quoted atoms and identifiers, and recovery/error‑production rules.
  - Overall: the major structural and precedence rules from elixir_parser.yrl are visible in the implementation and tests, but many of the finer‑grained grammar rules are still only present in the design/
    docs/tests and haven’t been turned into explicit parser code yet.


Here’s a coarse status map for the nonterminals in NONTERMINALS_GPT.md, with an eye toward what to do next.

  Legend (how I’m using the statuses)

  - Implemented – There is dedicated parsing code that covers the mainline behavior for that nonterminal family. Edge-case errors/recovery may still be missing.
  - Partially – Some semantics are present (often via generalized Pratt/containers/blocks), but the yrl rule is only loosely approximated or restricted cases are missing.
  - Missing – No dedicated implementation yet; behavior is largely absent or only incidentally covered.

  ———

  Implemented

  - grammar, expr_list, expr, eoe
  - parens_call
  - stab, stab_eoe, stab_expr, stab_op_eol_and_expr, stab_parens_many
  - do_block, fn_eoe, do_eoe, block_eoe, block_item, block_list

  These are backed by real modules (Grammar, Grammar.Expressions, Blocks) and heavily exercised in blocks_test.exs, grammar_expr_list_test.exs, calls_test.exs, and the conformance suite.

  ———

  Partially implemented

  These have some coverage (often generic) but don’t yet follow the yrl rule shape or constraints closely:

  - Expression / context core
      - container_expr, block_expr, access_expr
      - no_parens_expr, no_parens_zero_expr, no_parens_one_expr
      - matched_expr, unmatched_expr, sub_matched_expr
      - matched_op_expr, unmatched_op_expr, no_parens_op_expr

    (All driven by Pratt, Calls, Containers, Blocks, but context‑sensitive behavior and no‑parens semantics are incomplete.)
  - Operator helpers (*_op_eol)
      - comp_op_eol, at_op_eol, unary_op_eol, and_op_eol, or_op_eol, capture_op_eol, dual_op_eol, mult_op_eol, power_op_eol, concat_op_eol, xor_op_eol, pipe_op_eol, stab_op_eol, arrow_op_eol, match_op_eol,
        when_op_eol, in_op_eol, in_match_op_eol, type_op_eol, rel_op_eol, range_op_eol, ternary_op_eol
        (Operators are normalized by Toxic + TokenAdapter, and precedence/associativity live in Precedence; Pratt parses them, but there are no explicit *_op_eol RD wrappers and special behaviors like ..//
        rewriting and some warnings are missing.)
  - Parens / separators
      - open_paren, close_paren, empty_paren (tokens + delimiter metadata exist, used by Calls/Pratt, but not as separate RD helpers)
      - list, list_args, open_bracket, close_bracket (lists parsed by Containers.parse_list/3, but keyword-tail rules and strictness matching yrl are not implemented)
      - tuple, open_curly, close_curly (tuples via Containers.parse_tuple/3, but no kw-specific error paths)
  - Maps / assoc / container args
      - map, map_op, map_close, map_args (handled in Containers.parse_map/3 + parse_map_pairs/4 for %{} and basic %{key: value} / %{:a => 1}; struct/update/base‑expr forms are not there)
      - assoc_op_eol, assoc_expr, assoc_base, assoc (essentially approximated by parse_map_pairs/4)
      - container_args_base, container_args (approximated by parse_elements/6 in lists/tuples, but without strict yrl split between expr/kw tails)
  - Call arguments (paren)
      - call_args_parens_expr, call_args_parens_base, call_args_parens
        (Implemented generically in Calls.parse_paren_args/4 using Expressions.expr/3 and comma termination, but without kw‑vs‑non‑kw splits, error rules (error_no_parens_*), or no_parens_expr bans in certain
        positions.)
  - No‑parens call surface
      - call_args_no_parens_one
      - call_args_no_parens_expr

        (Simple foo 1 / foo x style calls handled by Calls.parse_no_parens_call/4, but there is no modeling of the full call_args_no_parens_* lattice, ambiguity rules, or kw‑only forms.)
  - Misc syntax helpers
      - bracket_expr (indexing via Pratt.led/5 + parse_access_indices/4 exists for expr[...], but no alignment with bracket_expr/open_bracket/close_bracket rule variants or error branches)
      - bracket_arg (we parse expressions inside brackets, but don’t distinguish kw vs container forms as yrl does)
      - dot_op, dot_identifier, dot_alias
        (Dot is present as a high‑bp binary op in Precedence and is parsed generically by Pratt; Builder.Helpers.from_token/1 handles aliases, and conformance tests for simple foo.bar/Foo.Bar pass, but the
        full dot nonterminal family is not explicitly modeled.)

  ———

  Missing (or effectively not started)

  These either have no dedicated implementation yet, or only token‑level plumbing without real yrl‑style rule semantics:

  - No‑parens, ambiguity, and strict variants
      - no_parens_one_ambig_expr, no_parens_many_expr
      - call_args_no_parens_ambig, call_args_no_parens_comma_expr, call_args_no_parens_all, call_args_no_parens_many, call_args_no_parens_many_strict

    (There is no logic for the “one/many/ambig” no‑parens families, nested call disambiguation, or the strict “no parens in this context” error variants; current code only does the simplest single‑argument
    no‑parens call.)
  - Bitstrings
      - bitstring, open_bit, close_bit

    (Only delimiter metadata is present in TokenAdapter; there’s no parser for <<>>, <<x::size(8)>>, etc. Current conformance bitstring tests will not be satisfied by the new parser.)
  - Map base / updates / kw‑driven variants
      - map_base_expr
      - assoc_update, assoc_update_kw

    (No %{map | a: 1}, %Foo{struct | ...} or % base_expr %{...} support; only fully literal %{...} is parsed.)
  - Keywords and kw‑based call/data rules
      - kw_eol, kw_base, kw_data, kw_call
      - call_args_no_parens_kw_expr, call_args_no_parens_kw

    (There’s currently no dedicated keyword list parsing; function and container keyword tails are not distinguished from general map syntax.)
  - Bracket + @ forms
      - bracket_at_expr

    (No dedicated @foo[bar] handling; @ exists as a unary op in precedence and Identifiers.classify/1, but the specific access form is missing.)
  - Dot “identifier kinds”
      - dot_bracket_identifier, dot_call_identifier, dot_op_identifier, dot_do_identifier, dot_paren_identifier

    (These appear only as token classifications; there’s no RD layer that builds yrl’s dot_* nonterminals and the associated alias/call/access behaviors. Current dot behavior is purely via generic Pratt
    binary ..)

  ———

  Suggested next-phase focus (based on gaps)

  - 1) No‑parens and keyword arguments
    Implement no_parens_expr + no_parens_* + call_args_no_parens_* + kw_* as per NONTERMINALS_OPUS.md, wired through Calls and Pratt.context/0. This unblocks a large fraction of the conformance suite (no-
    parens calls, kw last, ambiguity handling).
  - 2) Maps, structs, and updates
    Add map_base_expr, assoc_update, assoc_update_kw, and the % base_expr map_args / %{map | updates} / %Alias{struct | updates} variants on top of Containers.parse_map/3.
  - 3) Bitstrings and advanced containers
    Introduce a real bitstring parser (reusing container_args* style) and tighten list/tuple/map container rules to respect container_expr/container_args_* and kw-tail behaviors.
  - 4) Dot families
    Layer dot_* nonterminals (identifier/alias/op/bracket/paren/call) over the existing Pratt . core so dot expressions behave like elixir_parser.yrl (including remote calls, foo.+, foo.(...), etc.).




  1) No‑parens calls + keyword arguments

  Targets: lib/toxic_parser/grammar/calls.ex, grammar/expressions.ex, identifiers.ex, new grammar/keywords.ex (optional), pratt.ex, precedence.ex, tests.

  - 1.1 Context‑aware entry points
      - In Grammar.Expressions:
          - Add explicit matched_expr/3, unmatched_expr/3, no_parens_expr/3 that call Pratt.parse/3 with the right context, instead of the current single expr/3 → Blocks/Containers/Calls chain.
          - Update expr/3 to dispatch to those three per NONTERMINALS_OPUS and elixir_parser.yrl expr -> matched/no_parens/unmatched.
  - 1.2 Identifier + dot classification
      - In Identifiers:
          - Expand classify/1 return types to distinguish :dot_identifier, :dot_paren_identifier, :dot_bracket_identifier, :dot_op_identifier, :dot_call_identifier, :dot_do_identifier as separate tags (not
            just :other).
      - In TokenAdapter.normalize_token/3:
          - Ensure Toxic’s identifier/paren_identifier/bracket_identifier/do_identifier/op_identifier/dot_* tokens are preserved at the kind level so Identifiers.classify/1 can work reliably.
  - 1.3 No‑parens call surface in Grammar.Calls
      - In Grammar.Calls:
          - Split parse_identifier/5 into:
              - parse_paren_call/5 (already there, refine later for kw).
              - parse_no_parens_call/5 that:
                  - Uses TokenAdapter.checkpoint/1 + peek_n/2 to detect the three no_parens_* families: no_parens_one, no_parens_one_ambig, no_parens_many.
                  - For dot_* kinds, dispatches to helpers mirroring yrl rules:
                      - parse_no_parens_one/5
                      - parse_no_parens_one_ambig/5
                      - parse_no_parens_many/5
                  - Attaches do blocks only where allowed (block_expr equivalents).
          - Model return shapes like yrl’s build_no_parens forms but directly as AST via Builder.Helpers.call/3.
  - 1.4 Keyword argument parsing
      - Add new ToxicParser.Grammar.Keywords module:
          - Implement kw_eol, kw_base, kw_data, kw_call, call_args_no_parens_kw_expr, call_args_no_parens_kw as RD functions returning AST key/value pairs.
          - Provide:
              - parse_kw_call/4 – keyword list usable in paren call context.
              - parse_kw_data/4 – keyword list usable in data/map/list contexts.
      - Wire into:
          - Grammar.Calls.parse_paren_args/4:
              - Look ahead to detect kw_eol start; when found, call Keywords.parse_kw_call/4.
          - Grammar.Calls.parse_no_parens_call/5:
              - After first arg, allow handoff to Keywords.parse_kw_call/4 to finish arg list where yrl allows keywords.
  - 1.5 Context rules + when/no_parens_op_expr
      - In Pratt:
          - Respect context for no_parens_expr:
              - Disallow certain RHS forms (like block_expr) and special‑case when in no_parens_op_expr (e.g. when followed by kw‑only).
          - Implement the when_op_eol call_args_no_parens_kw split:
              - When context == :no_parens and operator kind == :when_op, if RHS starts with kw, treat it as that special yrl rule.
      - In Precedence (if needed):
          - Verify :when_op, :in_op, :in_match_op, etc. align with the yrl table and adjust if missing.
  - 1.6 Tests
      - Gradually un‑@tag :skip relevant sections of test/conformance_test.exs under:
          - “expr dispatch” → no_parens_expr.
          - “no_parens_expr – no_parens_one_expr/one_ambig/many_expr”, “keyword arguments”.
      - Extend test/calls_test.exs with targeted fixtures:
          - foo 1, foo 1, 2, foo a: 1, foo 1, a: 1, foo bar 1, 2, foo -bar 1, 2.

  ———

  2) Maps, structs, updates

  Targets: grammar/containers.ex, new grammar/maps.ex (optional), pratt.ex (for % as unary/base), tests.

  - 2.1 Map nonterminals
      - Create new ToxicParser.Grammar.Maps module, or extend Containers:
          - Functions mirroring yrl:
              - map/3 – top‑level, recognizes %{} and % base_expr map_args.
              - map_args/3 – parses everything inside {} including assoc, assoc_update, assoc_update_kw.
              - assoc_expr/3, assoc_base/3, assoc/3 – parse key => value and key being matched/unmatched/container expressions.
              - assoc_update/3, assoc_update_kw/3 – handle %{map | ...} and %{map | kw_data}.
          - Reuse Grammar.Expressions for matched_expr / unmatched_expr / container_expr on both sides where yrl does.
      - Update Containers.parse/3:
          - For %{} / % tokens, delegate to Maps.parse_map/3 rather than inline parse_map/3.
  - 2.2 Map base expressions
      - Implement map_base_expr semantics:
          - Allow sub_matched_expr, @ prefixes, unary op prefixes, ... prefix as per yrl.
          - Use Pratt for the RHS where appropriate, but ensure the leading % attaches correctly to the base AST.
  - 2.3 Structs
      - Within Maps:
          - Recognize %Alias{...} and %identifier{...} by:
              - Parsing an access_expr/matched_expr after % before {.
              - Distinguishing struct vs plain map by AST or token kind.
          - Build {:%{}, meta, [struct_base | pairs]} vs plain {:%{}, meta, pairs}.
  - 2.4 Tests
      - Unskip and extend map/struct sections in test/conformance_test.exs:
          - “containers – maps”, “containers – structs”, and “map update syntax”.
      - Add focused tests in test/containers_test.exs for:
          - %{a: 1}, %{map | a: 1}, %Foo{a: 1}, %Foo{struct | a: 1}.

  ———

  3) Bitstrings and advanced containers

  Targets: grammar/containers.ex, token_adapter.ex (metadata already there), tests.

  - 3.1 Bitstring parser
      - In Containers (or a Grammar.Bitstrings module):
          - Add parse_bitstring/3 that:
              - Recognizes <<" (open_bit) and >> (close_bit) from tokens.
              - Parses container_args inside, respecting:
                  - Plain expressions (container_expr).
                  - Bit modifiers like ::size(...), ::integer, ::binary, etc. (initially treat :: and modifier identifiers via existing Pratt expression parsing, then refine).
          - Hook into parse/3 dispatch: when peek kind is :"<<"; delegate to parse_bitstring/3.
  - 3.2 Tighten list/tuple behavior
      - Refine parse_list/3 and parse_tuple/3:
          - Split inner parsing into:
              - parse_container_args_base/5 (for container_args_base).
              - parse_container_args/5 – adds kw tails (kw_data) and optional trailing comma.
          - Use Keywords.parse_kw_data/4 for tail kw lists where allowed.
  - 3.3 Tests
      - Enable the “containers – bitstrings” section in test/conformance_test.exs.
      - Extend containers_test.exs with bitstring literals plus simple modifiers:
          - <<>>, <<1>>, <<x::8>>, <<x::size(8)-integer>>.

  ———

  4) Dot families

  Targets: pratt.ex, new grammar/dots.ex, grammar/calls.ex, builder/helpers.ex, tests.

  - 4.1 Dedicated dot RD layer
      - Create new ToxicParser.Grammar.Dots:
          - Implement functions corresponding to dot_identifier, dot_alias, dot_op_identifier, dot_bracket_identifier, dot_paren_identifier, dot_call_identifier:
              - Use Identifiers.classify/1 to recognize the correct starting token.
              - Build AST with Builder.Helpers.dot/3 or Builder.Helpers.alias_segments/2 where appropriate.
              - Support foo.Bar, Foo.Bar, foo.{A, B}, foo.(), foo.(1, 2).
          - Handle dot_call_identifier specially for expr.(...) and expr.().
  - 4.2 Integrate dot with Pratt
      - In Pratt.led/5:
          - When next_token.kind == :dot_op (or specific dot tokens from Toxic):
              - Treat . as a very high‑precedence infix:
                  - Parse RHS using Dots.parse_member/4, not generic parse_rhs/5.
                  - Allow chaining foo.bar.baz by looping in led/5.
              - Keep Precedence.binary(:dot_op) as the highest binding power.
      - In Builder.Helpers:
          - Make sure there is a helper for “dot call” vs “dot field access” if needed (dot/3 + call/3 combos).
  - 4.3 Calls + dot
      - In Grammar.Calls:
          - After Dots exists, adjust:
              - Paren call parsing to handle foo.bar(...) where the callee is a Dots AST, not just a bare identifier token.
              - no_parens_* logic to recognize dot_identifier variants as in yrl: foo.bar 1, 2, a.b c 1, 2.
  - 4.4 Tests
      - Unskip and extend the “dot expressions” section in test/conformance_test.exs:
          - Dot identifiers, dot alias, dot call, dot op identifier, dot bracket identifier, dot do identifier, anonymous function call (foo.(), foo.(1)).
      - Add unit tests in a new test/dots_test.exs for the Grammar.Dots module.
