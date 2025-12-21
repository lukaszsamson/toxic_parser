# Response to `profiling/ANALYSIS.md` (GPT)

I read `profiling/ANALYSIS.md` and cross-checked it against the attached profiler outputs (`profiling/time_parser_50files.txt`, `profiling/memory_parser_50files.txt`, `profiling/fprof_callers_20files.txt`, `profiling/eprof_parser_20files.txt`).

## Is the analysis correct?

Mostly yes: the *direction* of the hotspots is correct (lots of tiny operations multiplied by millions of tokens/steps). The concrete counts called out in `ANALYSIS.md` match what `tprof` shows for the 50-file run:

- `ToxicParser.Precedence.binary/1`: `1,111,160` calls (`profiling/time_parser_50files.txt`)
- `:lists.keyfind/3`: `11,445,277` calls
- `Toxic.checkpoint/1`: `134,062` calls
- `Toxic.rewind_to/3`: `74,439` calls
- `EOE.skip_newlines_only/2`: `587,702` calls
- `EOE.skip_count_newlines/2`: `245,245` calls
- `ToxicParser.TokenAdapter.metadata/4`: `5,241,328` calls (~`4.41%` of time profile)

So the “what is hot” list is grounded in the data.

## Where the writeup likely overstates impact

### 1) Precedence lookup optimization is real, but probably not 5–10%

`ToxicParser.Precedence.binary/1` is only ~`0.28%` of total time in the 50-file `tprof` run. Even a perfect O(1) rewrite only saves a fraction of that. The larger bucket is *all* `:lists.keyfind/3` callers (Keyword functions, etc.), and `Precedence.binary/1` is only one contributor.

I’d still do the compile-time function clause expansion (it’s clean and free), but I would expect “low single digit %” at best, not 10%.

### 2) “Keyword operations ~15%” is not supported by these profiles as written

There are a lot of Keyword calls, but in `profiling/time_parser_50files.txt` the biggest “pure Keyword” entries are roughly:

- `Keyword.get/3` ~`0.94%`
- `Keyword.get/2` ~`0.18%`
- plus smaller `Keyword.merge/2`, `Keyword.take/2`, etc.

The bigger costs that *feel like* “keyword/metadata handling” are actually:

- `:erlang.++/2` (~`5.04%` time, `30,552,244` calls)
- `:lists.keyfind/3` (~`1.98%` time)

So the right framing is: “list operations / keyword-as-list representation is expensive at scale”, not “Keyword module itself dominates”.

## Are there worse problems (O(n²), inefficient algorithms)?

Yes: the profiles point at a few places that can become quadratic-ish or simply “death by a million cuts”. The biggest red flag in the time profile is `:erlang.++/2`:

- `:erlang.++/2` is ~`5%` of runtime and ~`3.6%` of allocations (`profiling/time_parser_50files.txt`, `profiling/memory_parser_50files.txt`).
- That doesn’t *prove* O(n²), but it strongly suggests many “append to list” patterns in hot paths.

Concrete candidates:

1) **`TokenAdapter.maybe_cache/3` uses `lookahead ++ [token]`**
   - This is O(length(lookahead)) per append.
   - If you ever “build up lookahead” via repeated `peek`/`peek_n` before consuming, the total cost becomes quadratic in the peek window size.
   - It may be bounded in practice by `max_peek`, but even bounded quadratic work can show up as high `++` call counts when done constantly.
   - A queue/ring-buffer representation for `lookahead` would eliminate this class of cost.

2) **Metadata building uses repeated concatenation**
   - `Meta.closing_meta/5` and similar patterns are intentionally small lists, so the *asymptotic* risk is low, but the constant-factor cost is high because this happens everywhere.
   - The fix is less about “avoid Keyword.merge” and more “avoid repeated `++` and avoid constructing metadata you don’t need”.

3) **Delimiter parsing helpers**
   - Your new `Delimited.parse_comma_separated/…` is algorithmically fine (reverse-accumulate, no obvious O(n²)), but it does extra per-call setup (`List.wrap/1`, repeated `kind in close_kinds` → `Enum.member?/2`).
   - It’s not a correctness problem; just be aware it can become a large constant-factor tax when used pervasively.

## The biggest missing headline: lexer + token normalization dominate

The largest allocation buckets are not in your grammar code at all:

- `Toxic.Driver.handle_tokenize_result/2`: ~`9.28%` of allocations
- `Toxic.Driver.return_token/3`: ~`7.26%`
- `ToxicParser.TokenAdapter.update_state/6`: ~`7.33%`
- `ToxicParser.TokenAdapter.metadata/4`: ~`4.66%`
- `ToxicParser.TokenAdapter.maybe_cache/3`: ~`5.88%`

And in the time profile, the biggest single line item is the lexer driver:

- `Toxic.Driver.next/2`: ~`5.54%` time

So even perfect parser-grammar micro-optimizations will have a ceiling unless you reduce:

- token count / token churn
- per-token metadata cost (`Position.to_location/3` is called ~10M times; “twice per token” matches the profile)
- list churn (`++`, `reverse`, keyfind) while building metadata/terminators/lookahead

## One correctness/measurement issue in the appendix

`profiling/time_pratt_30files.txt` shows “Profile done over 0 matching functions”. That means the “Matching: ToxicParser.Pratt/*” selector didn’t match anything for `tprof` in that run, so any conclusions relying on that file aren’t actionable yet. It’s worth fixing the matcher (or using `--matching` syntax that `tprof` actually supports) before attributing costs specifically to Pratt internals.

## Suggested priority tweaks (based on these numbers)

- **P0: cut `++` in hot paths**
  - Especially `TokenAdapter.maybe_cache/3` and any metadata builders that append repeatedly.
- **P0/P1: reduce per-token metadata work**
  - Avoid building full `%{range: ..., delimiter: ..., terminators: ...}` maps eagerly if only some consumers need them.
  - If `token_metadata: true` is required for conformance, consider storing positions as integers/slices and lazily materializing `{line, column}` maps.
- **P1: precedence dispatch as function clauses**
  - Easy win, but small.
- **P1: reduce checkpointing where it’s truly speculative**
  - Checkpoints are *not* currently the main time hog, but they do allocate and can interact badly with speculative parsing regressions.

If the goal is “back to 8s from 30s” for the full suite, I would focus first on anything that increases token churn and list churn in the hottest loops (especially around call-args parsing and stab/paren ambiguity), because the baseline profiles already show you are close to the ceiling set by tokenization + metadata.

## What changed in `d1fb9a5` and `ff0d887` that explains the slowdown?

You’re right that there were no lexer / TokenAdapter / metadata-table changes in those commits, so the regression has to come from **parser control-flow changes causing more token churn** (more `peek/next`, more backtracking, more speculative parsing).

I made this concrete by running the exact same profiling script (`profile_toxic.exs`, `--limit 50`, `--type time`, `--profiler tprof`) against three git states via detached worktrees:

- `e32c4db` (parent of `d1fb9a5`)
- `d1fb9a5`
- `ff0d887`

### Baseline throughput regression shows up immediately

From the script’s own baseline measurement (same 50 files, same bytes):

- `e32c4db`: baseline `1329.63ms`, throughput `~68k AST nodes/s`, `~977 KB/s` (`/tmp/time_e32c4db.txt`)
- `d1fb9a5`: baseline `1264.41ms`, throughput `~72k AST nodes/s`, `~1028 KB/s` (`/tmp/time_d1fb9a5.txt`)
- `ff0d887`: baseline `3423.93ms`, throughput `~26k AST nodes/s`, `~380 KB/s` (`/tmp/time_ff0d887.txt`)

So the big jump is **between `d1fb9a5` → `ff0d887`**, not (for this corpus) between `e32c4db` → `d1fb9a5`.

### The core symptom: ~2.7–2.9× more token traffic (backtracking / re-reading)

Comparing `tprof` counters (`e32c4db` → `ff0d887`):

- `ToxicParser.TokenAdapter.peek/1`: `2,296,900` → `6,377,905` (~2.78×)
- `ToxicParser.TokenAdapter.next/1`: `519,655` → `1,398,003` (~2.69×)
- `Toxic.next/1`: `2,130,845` → `5,891,216` (~2.76×)
- `ToxicParser.Grammar.Expressions.parse_with_layers/3`: `119,247` → `349,867` (~2.93×)

That’s the smoking gun: even though the lexer/tokenizer implementation is unchanged, the parser is now *asking it for tokens* ~2.7× more often.

### Checkpoint / rewind usage explodes proportionally

Same comparison (`e32c4db` → `ff0d887`):

- `ToxicParser.TokenAdapter.checkpoint/1`: `47,616` → `134,062` (~2.82×)
- `ToxicParser.TokenAdapter.rewind/2`: `29,629` → `74,439` (~2.51×)
- `ToxicParser.TokenAdapter.drop_checkpoint/2`: `17,987` → `59,623` (~3.31×)

This lines up with “parser does more speculative parsing and rewinds more”, which *necessarily* multiplies token reads.

### Where is that extra work happening? Stabs + delimited + keyword probing

The biggest *parser-owned* multipliers in `ff0d887` are inside stab parsing and comma/kw loops:

- `ToxicParser.Grammar.Stabs.try_parse_stab_clause/4`: `16,090` → `44,912` (~2.79×)
- `ToxicParser.Grammar.Stabs.parse_stab_patterns/4`: `16,090` → `44,912` (~2.79×)
- `ToxicParser.Grammar.Stabs.parse_stab_clause_after_patterns/6`: `16,090` → `44,912` (~2.79×)
- `ToxicParser.Grammar.Stabs.check_and_collect_stab_body/5`: `3,420` → `16,252` (~4.75×)

On the comma/keyword side:

- `ToxicParser.Grammar.Delimited.parse_comma_separated/6`: `37,493` → `100,729` (~2.69×)
- `ToxicParser.Grammar.Delimited.maybe_skip_eoe/3`: `168,772` → `480,098` (~2.85×)
- `ToxicParser.Grammar.Keywords.starts_kw?/1`: `256,085` → `726,313` (~2.84×)
- `ToxicParser.Grammar.EOE.skip_count_newlines/2`: `277,844` → `792,943` (~2.85×)

These are exactly the kinds of operations that multiply when you:

1) probe a construct speculatively (checkpoint)
2) partially parse it
3) discover it’s “not that construct”
4) rewind and parse it again in a different mode/context

### Likely specific behavioral change in `ff0d887`

Looking at the diffs, `ff0d887` does two things that plausibly cause this multiplication:

1) It makes stab bodies/guards parse as `Context.expr()` (more permissive) in more places.
   - That increases the surface area where no-parens / kw-call forms are attempted, which triggers more `Keywords.*` probing and more Delimited loops.
2) It increases ambiguity handling around `when` inside stab-related parsing.
   - Even when the “keyword RHS after `when`” case is rare in source, the *check* and the extra peeks/checkpoints around it are now on a very hot path (guards and clause detection).

The key takeaway is: the regression isn’t “metadata got slower”; it’s that `ff0d887` made the parser **speculate more / rewind more / re-parse more**, and you can see that directly in `peek/next`, `checkpoint/rewind`, and stab-clause probing counters.

### What to do with this

If you want to restore performance without losing correctness, the data suggests you should target:

- `ToxicParser.Grammar.Stabs.check_and_collect_stab_body/5` (avoid “probe parse then reparse”)
- Any “try-parse then rewind” in hot paths (especially around `when`/kw ambiguity)
- Reducing `Delimited.maybe_skip_eoe/3` / `Keywords.starts_kw?/1` invocations caused by speculative paths

Those are exactly the spots where the parser can accidentally do 2–3× the work with no lexer changes.
