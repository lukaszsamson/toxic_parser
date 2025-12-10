# ToxicParser Implementation Plan

## Overview

Implement an error-tolerant Elixir parser that integrates with the Toxic streaming lexer, producing IDE/LSP-ready AST even on malformed input. The parser faithfully implements the canonical Elixir grammar (`elixir_parser.yrl`) using a hybrid recursive-descent + Pratt parsing approach.

**Key Features:**
- Event log for CST/green tree building and incremental parsing
- Comment preservation (compatible with `Code.string_to_quoted_with_comments`)
- Environment stream for scope/bindings/aliases tracking

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                         ToxicParser (Public API)                     │
├─────────────────────────────────────────────────────────────────────┤
│  Token Adapter Layer                                                 │
│  ├── Normalizes Toxic tokens → parser views                         │
│  ├── Exposes peek/peek_n/checkpoint/rewind                          │
│  └── EOE abstraction (eol/; → logical end-of-expression)            │
├─────────────────────────────────────────────────────────────────────┤
│  Grammar Layer (Recursive Descent)                                   │
│  ├── Expression families: matched/unmatched/no_parens               │
│  ├── Containers: list/tuple/map/bitstring                           │
│  ├── Blocks: do_block/fn/stab/clauses                               │
│  └── Calls: paren/no-paren/do-block variants                        │
├─────────────────────────────────────────────────────────────────────┤
│  Expression Layer (Pratt Parser)                                     │
│  ├── Binding powers from yrl precedence table                       │
│  ├── NUD: literals, containers, unary ops, captures                 │
│  └── LED: binary ops, dot/call, access, do-block attachment         │
├─────────────────────────────────────────────────────────────────────┤
│  Event Log & Environment                                             │
│  ├── Event emission: start_node/end_node/token/error/missing        │
│  ├── Comment trivia attachment to following nodes                   │
│  └── Environment stream: scope/bindings/aliases/imports             │
├─────────────────────────────────────────────────────────────────────┤
│  Builders & Recovery                                                 │
│  ├── AST builders (Core-compatible format)                          │
│  ├── Error nodes: {:__error__, meta, payload}                       │
│  └── Recovery: sync points, missing-token synthesis                 │
└─────────────────────────────────────────────────────────────────────┘
```

## Implementation Phases

### Phase 1: Foundation & Token Adapter

**Files to create:**
- `lib/toxic_parser.ex` - Main module, public API
- `lib/toxic_parser/state.ex` - Parser state struct
- `lib/toxic_parser/token_adapter.ex` - Token normalization layer

**Key tasks:**
1. Define parser state struct:
   ```elixir
   %ToxicParser.State{
     stream: Toxic.t(),              # Toxic stream
     current_token: token(),         # Current token
     peek_token: token(),            # Lookahead token
     errors: [error()],              # Accumulated diagnostics
     terminators: [atom()],          # Current terminator stack
     expression_context: :matched | :unmatched | :no_parens,
     fuel: integer(),                # Progress guard (debug)
     opts: keyword()                 # Configuration options
   }
   ```

2. Implement token adapter functions:
   - `next_token/1` - Advance and consume token
   - `peek_token/1` - Lookahead without consuming
   - `peek_n/2` - Multi-token lookahead (narrow use cases)
   - `checkpoint/1`, `rewind/2` - Backtracking support
   - `current_terminators/1` - Terminator stack access

3. Implement EOE abstraction:
   - Coalesce `eol`/`;` into logical end-of-expression markers
   - Track newline counts for metadata
   - Handle optional EOE consumption after operators

4. Token classification helpers:
   - `token_type/1` - Extract token type atom
   - `is_operator?/1` - Check if binary/unary operator
   - `operator_info/1` - Get precedence and associativity
   - `is_structural?/1` - Check if delimiter/keyword

### Phase 2: Pratt Expression Parser Core

**Files to create:**
- `lib/toxic_parser/pratt.ex` - Pratt parser core
- `lib/toxic_parser/precedence.ex` - Binding power table

**Key tasks:**
1. Define binding power table (from yrl lines 69-97):
   ```elixir
   @precedence %{
     do:          {5, :left},
     stab_op:     {10, :right},     # ->
     comma:       {20, :left},
     in_match_op: {40, :left},      # <-, \\
     when_op:     {50, :right},
     type_op:     {60, :right},     # ::
     pipe_op:     {70, :right},     # |
     assoc_op:    {80, :right},     # =>
     capture_op:  {90, :nonassoc},  # &
     ellipsis_op: {90, :nonassoc},  # ...
     match_op:    {100, :right},    # =
     or_op:       {120, :left},     # ||, |||, or
     and_op:      {130, :left},     # &&, &&&, and
     comp_op:     {140, :left},     # ==, !=, =~, ===, !==
     rel_op:      {150, :left},     # <, >, <=, >=
     arrow_op:    {160, :left},     # |>, ~>, <~, etc.
     in_op:       {170, :left},     # in, not in
     xor_op:      {180, :left},     # ^^^
     ternary_op:  {190, :right},    # //
     concat_op:   {200, :right},    # ++, --, <>, etc.
     range_op:    {200, :right},    # ..
     dual_op:     {210, :left},     # +, -
     mult_op:     {220, :left},     # *, /
     power_op:    {230, :left},     # **
     unary_op:    {300, :nonassoc}, # +, -, !, ^, not, ~~~
     dot_call_op: {310, :left},     # .
     at_op:       {320, :nonassoc}, # @
     access:      {330, :nonassoc}  # []
   }
   ```

2. Implement Pratt parser loop:
   ```elixir
   def parse_expression(state, min_bp) do
     {left, state} = parse_nud(state)
     parse_led_loop(state, left, min_bp)
   end

   defp parse_led_loop(state, left, min_bp) do
     case get_led_bp(peek_token(state)) do
       {bp, _assoc} when bp > min_bp ->
         {left, state} = parse_led(state, left)
         parse_led_loop(state, left, min_bp)
       _ ->
         {left, state}
     end
   end
   ```

3. Implement NUD (null denotation) parselets:
   - Literals: `:int`, `:flt`, `:char`, `:atom`, `true/false/nil`
   - Identifiers: all identifier variants
   - Unary operators: `@`, `&`, `!`, `^`, `not`, `+/-` (unary)
   - Containers: `(`, `[`, `{`, `%{`, `%`, `<<`
   - Special: `fn`, `...`, `..` (nullary range), `&N` (capture_int)

4. Implement LED (left denotation) parselets:
   - Binary operators: all `*_op` types
   - Dot/call: `.`, `.()`
   - Access: `[]`
   - Special: `not in` rewrite, `..//` step

### Phase 3: Grammar Layer - Expression Families

**Files to create:**
- `lib/toxic_parser/grammar.ex` - Grammar rules
- `lib/toxic_parser/grammar/expressions.ex` - Expression family logic

**Key tasks:**
1. Implement top-level grammar:
   ```elixir
   # grammar -> eoe? expr_list eoe?
   def parse_grammar(state) do
     state = maybe_consume_eoe(state)
     {exprs, state} = parse_expr_list(state)
     state = maybe_consume_eoe(state)
     build_block(exprs, state)
   end
   ```

2. Implement expression families (yrl lines 112-180):
   - `parse_expr/1` - Dispatch to matched/unmatched/no_parens
   - `parse_matched_expr/1` - Complete expressions
   - `parse_unmatched_expr/1` - Expressions with do blocks
   - `parse_no_parens_expr/1` - No-parens calls

3. Implement no-parens subtypes:
   - `no_parens_one` - Single unambiguous argument
   - `no_parens_many` - Multiple arguments
   - `no_parens_one_ambig` - Ambiguous nested call (outer arity 1)

4. Handle expression context threading:
   - Pass context `:matched | :unmatched | :no_parens` through parsing
   - Enforce restrictions (e.g., no nested no_parens_many)

### Phase 4: Grammar Layer - Calls & Identifiers

**Files to create:**
- `lib/toxic_parser/grammar/calls.ex` - Call parsing

**Key tasks:**
1. Implement call variants:
   - `parse_paren_call/2` - `foo(args)`
   - `parse_no_parens_call/2` - `foo args`
   - `parse_nested_paren_call/2` - `foo(args)(more_args)`
   - `parse_do_block_call/2` - `foo args do ... end`

2. Implement identifier handling:
   - Classify by token type: `identifier`, `paren_identifier`,
     `bracket_identifier`, `do_identifier`, `op_identifier`
   - Handle dot-prefixed forms: `dot_identifier`, `dot_op_identifier`, etc.
   - Build identifier/call AST nodes

3. Implement call argument parsing:
   - `parse_call_args_parens/1` - Parenthesized args
   - `parse_call_args_no_parens/1` - No-parens args
   - Keyword argument handling (must be last)
   - `do:` vs `do/end` disambiguation

### Phase 5: Grammar Layer - Containers

**Files to create:**
- `lib/toxic_parser/grammar/containers.ex` - Container parsing

**Key tasks:**
1. Implement list parsing:
   ```elixir
   # list -> '[' ']' | '[' list_args ']'
   def parse_list(state) do
     state = expect(state, :"[")
     {args, state} = parse_list_args(state)
     state = expect(state, :"]")
     {args, state}
   end
   ```

2. Implement tuple parsing:
   - Handle 2-tuple special case (literal tuple)
   - Handle N-tuple (wrapped in `{}`)

3. Implement map/struct parsing:
   - `%{}` - empty map
   - `%{k => v}` - assoc map
   - `%{map | k => v}` - map update
   - `%Struct{...}` - struct literal

4. Implement bitstring parsing:
   - `<<>>` - empty bitstring
   - `<<expr, expr::type, ...>>` - with segments
   - Segment modifier parsing (size, unit, type)

5. Container argument rules:
   - Keyword lists must be last
   - Trailing comma handling
   - `container_expr` vs `matched_expr` distinction

### Phase 6: Grammar Layer - Blocks & Clauses

**Files to create:**
- `lib/toxic_parser/grammar/blocks.ex` - Block parsing
- `lib/toxic_parser/grammar/stab.ex` - Stab clause parsing

**Key tasks:**
1. Implement do_block parsing:
   ```elixir
   # do_block -> 'do' stab_eoe? block_list? 'end'
   def parse_do_block(state) do
     state = expect(state, :do)
     {stab, state} = maybe_parse_stab(state)
     {blocks, state} = parse_block_list(state)
     state = expect(state, :end)
     build_do_block(stab, blocks, state)
   end
   ```

2. Implement block_list parsing:
   - Block labels: `else`, `catch`, `rescue`, `after`
   - Each section has its own stab clauses

3. Implement fn parsing:
   - `fn stab end` - anonymous function
   - Validate stab structure

4. Implement stab clause parsing:
   - `args -> body`
   - `args when guard -> body`
   - `(args) -> body` (parenthesized)
   - Empty stab clause warning

5. Implement paren-stab forms:
   - `(-> body)` - empty args
   - `(args -> body)` - stab in parens
   - `(; ...)` - semicolon-separated

### Phase 7: Strings, Sigils, Heredocs

**Files to create:**
- `lib/toxic_parser/grammar/strings.ex` - String/sigil parsing

**Key tasks:**
1. Consume Toxic's linearized string tokens:
   - `:bin_string_start` → `:string_fragment`* → `:bin_string_end`
   - Build parts list with interpolations

2. Handle interpolation:
   - `:begin_interpolation` → parse expression → `:end_interpolation`
   - Recursive parser call for interpolated content

3. Handle heredocs:
   - Indentation stripping based on closing delimiter position
   - Preserve indentation metadata

4. Handle sigils:
   - `:sigil_start` → content → `:sigil_end` → `:sigil_modifiers`
   - Interpolating vs. non-interpolating sigils
   - Build `{:sigil_X, meta, [content, modifiers]}`

5. Handle quoted atoms/identifiers:
   - `:atom_safe_start`/`:atom_unsafe_start`
   - Quoted keyword identifiers

### Phase 8: Event Log & Comment Handling

**Files to create:**
- `lib/toxic_parser/event_log.ex` - Event log infrastructure
- `lib/toxic_parser/comments.ex` - Comment handling

**Key tasks:**
1. Define event types:
   ```elixir
   @type event ::
     {:start_node, node_kind(), position()}
     | {:end_node, node_kind(), position()}
     | {:token, token_type(), position(), value()}
     | {:missing, expected(), position()}
     | {:synthetic, token_type(), position()}
     | {:error, error_payload(), position()}
     | {:comment, text(), position()}
   ```

2. Implement event emission during parsing:
   - `emit_start_node/2` - Before parsing a construct
   - `emit_end_node/2` - After completing a construct
   - `emit_token/2` - For each consumed token
   - Events enable CST reconstruction without re-parsing

3. Implement comment preservation (compatible with `Code.string_to_quoted_with_comments`):
   - Configure Toxic with `preserve_comments` callback
   - Accumulate comments during parsing
   - Attach to following node as trivia or return separately
   - Support both inline `# comment` and doc comments

4. Comment attachment strategy:
   ```elixir
   # Option 1: Attach to AST node metadata
   {:def, [comments: [%{text: "...", line: 1}], line: 2], ...}

   # Option 2: Return separately (like Code.string_to_quoted_with_comments)
   {:ok, ast, comments}
   ```

### Phase 9: Environment Stream

**Files to create:**
- `lib/toxic_parser/env.ex` - Environment tracking
- `lib/toxic_parser/env/scope.ex` - Scope management

**Key tasks:**
1. Define environment events:
   ```elixir
   @type env_event ::
     {:enter_scope, scope_type()}      # module, function, block, comprehension
     | {:exit_scope, scope_type()}
     | {:binding, name(), position()}   # Variable binding
     | {:alias, alias_info()}           # alias Foo.Bar
     | {:import, import_info()}         # import Module
     | {:require, require_info()}       # require Module
     | {:use, use_info()}               # use Module
   ```

2. Emit environment events during parsing:
   - Track `defmodule` → `:enter_scope, :module`
   - Track `def/defp/defmacro` → `:enter_scope, :function`
   - Track `for/with` → `:enter_scope, :comprehension`
   - Track variable bindings in patterns (left side of `=`, function args)

3. Binding detection in patterns:
   - Match operator left-hand side
   - Function clause arguments
   - Case/cond/receive clause patterns
   - For/with generator patterns
   - Destructuring in assignments

4. Integration with Spitfire.Env:
   - Events can be consumed to build environment state
   - No re-traversal needed for scope queries
   - Supports incremental updates

### Phase 10: Error Recovery System

**Files to create:**
- `lib/toxic_parser/recovery.ex` - Error recovery logic
- `lib/toxic_parser/error.ex` - Error/diagnostic types

**Key tasks:**
1. Define error node structure:
   ```elixir
   {:__error__, meta, %{
     kind: :missing_token | :unexpected_token | :lexer_error | ...,
     expected: atom() | [atom()],
     found: token() | nil,
     inner: ast() | nil,
     synthesized?: boolean()
   }}
   ```

2. Implement lexer error integration:
   - Convert `{:error_token, meta, %Toxic.Error{}}` to error nodes
   - Preserve lexer error details in payload

3. Implement missing token synthesis:
   - When expected token absent, emit diagnostic
   - Use Toxic's synthesized closers when available
   - Mark with `synthesized: true` metadata

4. Implement sync point scanning:
   - Per-rule sync sets using terminator stack
   - Expression sync: EOE, commas, closers, `end`
   - Container sync: `]`, `}`, `>>`, commas
   - Block sync: `else/catch/rescue/after/end`

5. Implement fuel/progress guard:
   - Debug assertion on forward progress
   - Emit fatal error node if stuck
   - Fast-forward to next high-level sync point

### Phase 11: AST Building & Metadata

**Files to create:**
- `lib/toxic_parser/builder.ex` - AST construction helpers

**Key tasks:**
1. Implement metadata construction:
   ```elixir
   def build_meta(token) do
     {{sl, sc}, {el, ec}, extra} = token_meta(token)
     [line: sl, column: sc, closing: ..., newlines: ...]
   end
   ```

2. Implement Core-compatible AST builders:
   - `build_block/2` - `{:__block__, meta, exprs}`
   - `build_op/3` - `{op, meta, [left, right]}`
   - `build_call/3` - `{name, meta, args}`
   - `build_fn/2` - `{:fn, meta, clauses}`

3. Handle special rewrites:
   - `not expr in right` → `not(expr in right)` with warning
   - `..//` step operator constraint
   - Dotted aliases concatenation
   - Pipe/arrow warnings

4. Preserve lossless metadata:
   - Range positions (start/end line/col)
   - Delimiters for strings/atoms
   - Newline counts
   - `parens`/`closing` markers

### Phase 12: Public API & Testing

**Files to modify:**
- `lib/toxic_parser.ex` - Public API completion

**Key tasks:**
1. Implement public API:
   ```elixir
   def parse(source, opts \\ [])
   def parse_string(string, opts \\ [])
   def parse_file(path, opts \\ [])
   def parse_string_with_comments(string, opts \\ [])
   ```

2. Configuration options:
   - `:tolerant` (default) vs `:strict` mode
   - `:token_metadata` - include/exclude metadata
   - `:literal_encoder` - custom literal handling
   - `:existing_atoms_only` - atom safety
   - `:preserve_comments` - comment handling (true/false or callback)
   - `:emit_events` - enable event log output
   - `:emit_env` - enable environment stream

3. Return format:
   ```elixir
   {:ok, ast}                                    # Success (no comments)
   {:ok, ast, comments}                          # Success with comments
   {:ok, ast, comments, events}                  # Success with events
   {:ok, ast, comments, events, env_events}      # Full output
   {:error, ast, diagnostics}                    # Tolerant mode with errors
   {:error, reason}                              # Fatal error / strict mode
   ```

4. Testing strategy:
   - Conformance tests vs `Code.string_to_quoted/2`
   - Resilience tests with malformed input
   - Property-based tests with StreamData
   - Large corpus parsing (Hex packages)
   - Comment preservation tests vs `Code.string_to_quoted_with_comments`

## Critical Files to Create

```
lib/toxic_parser/
├── state.ex              # Parser state struct
├── token_adapter.ex      # Token normalization layer
├── precedence.ex         # Binding power table
├── pratt.ex              # Pratt parser core
├── grammar.ex            # Grammar layer entry
├── grammar/
│   ├── expressions.ex    # Expression families
│   ├── calls.ex          # Call parsing
│   ├── containers.ex     # List/tuple/map/bitstring
│   ├── blocks.ex         # Do blocks and clauses
│   ├── stab.ex           # Stab clause parsing
│   └── strings.ex        # Strings/sigils/heredocs
├── event_log.ex          # Event log infrastructure
├── comments.ex           # Comment handling
├── env.ex                # Environment tracking
├── env/
│   └── scope.ex          # Scope management
├── recovery.ex           # Error recovery logic
├── error.ex              # Error/diagnostic types
└── builder.ex            # AST construction helpers
```

## Dependencies

- Toxic lexer (already configured as path dependency)
- StreamData (test dependency, already configured)

## Key Design Decisions

1. **No normalization passes**: Build correct AST directly in parselets
2. **Single expression context parameter**: Only `:matched | :unmatched | :no_parens`
3. **Semantic validation deferred**: Parser accepts semantically invalid but syntactically correct code
4. **Bounded lookahead**: Default single-token peek, `peek_n` only for specific cases
5. **Event log included**: Events emitted during parsing for CST/incremental support
6. **Comment preservation**: Compatible with `Code.string_to_quoted_with_comments`
7. **Environment stream**: Scope/binding events for IDE integration

## Testing Priorities

1. Expression precedence chains
2. No-parens call nesting rules
3. Do-block attachment disambiguation
4. Missing closer recovery
5. Keyword-last rule enforcement
6. `not in` rewrite correctness
7. Range + step (`..//`) constraint
8. Comment preservation accuracy
9. Environment event correctness
