PH1_G3 raised no open issues; nothing new surfaced. From PH1_OPUS, most items are addressed (mode docs tightened, event ordering documented, tolerant stream errors handled, pushback/identifier
   classification added, peek_n/stream-error tests present, Position helper deduped), but two gaps remain: there’s still no documented comment/trivia attachment policy and no CI pipeline/config present. If
   you want to fully close Phase 0/1, add the comment policy doc and wire CI for the existing test/conformance suites.

PH3 issues are now addressed: the precedence table matches the HLD with associativity and missing operators added, Pratt is fully implemented with NUD/LED parselets and dual-op spacing handled via token
   shapes, and Builder.Helpers includes the call helper. Tests cover precedence and associativity, and State carries the event log. No PH3 review items remain relevant.

Most PH4 gaps are fixed: the grammar now skips leading/trailing EOE, parses multi-expression lists into __block__, threads contexts through matched/unmatched/no-parens dispatch, handles do-blocks via 
   Calls/Blocks, and implements no-parens/paren calls and bracket access. A Warning struct and state slot exist, but no warnings are emitted—warn_pipe/warn_no_parens_after_do_op infrastructure and triggers
   are still absent. So the only remaining PH4 issue is the missing warning emission.

Most Phase 5 gaps are now closed: identifier classification includes dot/quoted forms, paren and no‑parens calls (with do-block carriers) are parsed, bracket access works, and args/keyword parsing
   infrastructure is in place. Still relevant:

     - No warning emission: warn_pipe, warn_no_parens_after_do_op, and maybe_bad_keyword_*/keyword-last checks are not implemented (warnings list stays empty).
     - do: vs do/end disambiguation and keyword-last enforcement are absent.
     - No-parens ambiguity/nesting bans and peek_n-based disambiguation are still not enforced; calls accept any arg shapes without the outer-arity-1 rules.


Phase 6 gaps are mostly closed: lists/tuples enforce commas, allow trailing commas, keyword tails, and EOE skipping; access syntax is handled in Pratt LED; maps/structs (incl. update) are implemented;
   basic bitstrings (<<>> with comma-separated segments/keyword tails) are parsed; parens/containers integrate with Pratt and stab parsing. Remaining PH6 issues: bitstring segment modifiers
   (size/type/unit/signedness/endianness, capture_int) are not parsed—segments are plain expressions only—and there’s no dedicated sync/recovery on container terminators beyond normal error returns. If you
   need full bitstring modifier support and robust recovery, those items are still outstanding.

Yes. for comprehensions are still not parsed anywhere. Environment tracking remains minimal: only enter/exit scope events on fn/do-blocks, no bind events for patterns, and no alias/import/use/require
   handling (Env is a stub with scopes only).
