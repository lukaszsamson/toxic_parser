defmodule ToxicParser.Pratt do
  @moduledoc """
  Pratt parser skeleton for matched/unmatched/no-parens expressions.

  Phase 3 implements binding power lookup and API shape; full parselets are
  implemented in later phases.

  Dual operator spacing (`dual_op` as unary vs binary) relies on Toxic token
  shapes; spacing-sensitive disambiguation will occur in parselets using token
  metadata.
  """

  alias ToxicParser.{
    Builder,
    Context,
    Cursor,
    EventLog,
    Identifiers,
    NoParens,
    Precedence,
    Result,
    State,
    TokenAdapter
  }

  alias ToxicParser.Builder.Meta

  alias ToxicParser.Grammar.{
    Blocks,
    Brackets,
    Calls,
    Delimited,
    DoBlocks,
    Dots,
    EOE,
    Expressions,
    Keywords
  }

  import Keywords, only: [{:is_keyword_list_result, 1}]
  import TokenAdapter, only: [{:is_kind, 2}]

  # Binary-only operators that cannot start an expression
  # These have binary precedence but no unary form
  @binary_only_ops [
    :stab_op,
    :in_match_op,
    :when_op,
    :type_op,
    :pipe_op,
    :assoc_op,
    :match_op,
    :or_op,
    :and_op,
    :comp_op,
    :rel_op,
    :arrow_op,
    :in_op,
    :xor_op,
    :ternary_op,
    :concat_op,
    :mult_op,
    :power_op,
    :.,
    :dot_call_op
  ]

  defguardp is_binary_only_op(kind) when kind in @binary_only_ops

  @type context :: Context.t()

  # Operators that can extend matched_expr to no_parens_expr
  # From elixir_parser.yrl no_parens_op_expr rules (lines 230-250)
  # EXCLUDED: :., :dot_call_op (via access_expr), :stab_op (handled separately), :assoc_op (map-only)
  @no_parens_op_expr_operators [
    :match_op,
    :dual_op,
    :mult_op,
    :power_op,
    :concat_op,
    :range_op,
    :ternary_op,
    :xor_op,
    :and_op,
    :or_op,
    :in_op,
    :in_match_op,
    :type_op,
    :when_op,
    :pipe_op,
    :comp_op,
    :rel_op,
    :arrow_op
  ]
  @do_block_keywords [:case, :cond, :with, :try, :receive, :if, :unless, :for, :quote]

  @type result ::
          {:ok, Macro.t(), State.t(), Cursor.t(), EventLog.t()}
          | {:error, term(), State.t(), Cursor.t(), EventLog.t()}
          | {:keyword_key, term(), term(), term(), term(), term()}
          | {:keyword_key_interpolated, term(), term(), term(), term(), term(), term(), term()}

  defp ensure_no_parens_extension_opt(opts) do
    allow? =
      Keyword.get(opts, :allow_no_parens_extension?, not Keyword.get(opts, :unary_operand, false))

    Keyword.put(opts, :allow_no_parens_extension?, allow?)
  end

  defp is_no_parens_op_expr?(kind), do: kind in @no_parens_op_expr_operators

  defp do_block_expr?({_, meta, _}) when is_list(meta) do
    Keyword.has_key?(meta, :do) or Keyword.has_key?(meta, :end)
  end

  defp do_block_expr?(_), do: false

  @doc """
  Parses an expression in the given context.
  """
  @spec parse(State.t(), Cursor.t(), context(), EventLog.t()) :: result()
  def parse(%State{} = state, cursor, %Context{} = context, %EventLog{} = log) do
    with {:ok, token, state, cursor} <- TokenAdapter.next(state, cursor),
         {:ok, left, state, cursor, log} <- nud(token, state, cursor, context, log) do
      led(left, state, cursor, log, 0, context)
    else
      {:eof, state, cursor} -> {:error, :unexpected_eof, state, cursor, log}
      # Handle keyword_key from Strings.parse - bubble up to caller
      {:keyword_key, _, _, _, _, _} = keyword_key -> keyword_key
      {:keyword_key_interpolated, _, _, _, _, _, _, _} = keyword_key -> keyword_key
      other -> Result.normalize_error(other, cursor, log)
    end
  end

  @doc """
  Parses a base expression (nud only, no trailing operators via led).
  Used for sub_matched_expr in map_base_expr grammar rule.
  This version does NOT check for do-blocks or no-parens call arguments.
  """
  @spec parse_base(State.t(), Cursor.t(), context(), EventLog.t()) :: result()
  def parse_base(%State{} = state, cursor, %Context{} = context, %EventLog{} = log) do
    with {:ok, token, state, cursor} <- TokenAdapter.next(state, cursor),
         {:ok, ast, state, cursor, log} <-
           nud(token, state, cursor, context, log,
             min_bp: 10000,
             allow_containers: false
           ) do
      {:ok, ast, state, cursor, log}
    else
      {:eof, state, cursor} -> {:error, :unexpected_eof, state, cursor, log}
      other -> Result.normalize_error(other, cursor, log)
    end
  end

  @doc """
  Parses an expression base with dots and paren calls, stopping at `{`.
  Used for struct names like `%Foo.Bar{}` and `%unquote(struct){}`.
  """
  @spec parse_base_with_dots_and_calls(State.t(), Cursor.t(), context(), EventLog.t()) :: result()
  def parse_base_with_dots_and_calls(
        %State{} = state,
        cursor,
        %Context{} = context,
        %EventLog{} = log
      ) do
    with {:ok, token, state, cursor} <- TokenAdapter.next(state, cursor),
         {:ok, ast, state, cursor, log} <-
           nud(token, state, cursor, context, log,
             min_bp: 10000,
             allow_containers: false
           ) do
      led_dots_and_calls(ast, state, cursor, log, context)
    else
      {:eof, state, cursor} -> {:error, :unexpected_eof, state, cursor, log}
      other -> Result.normalize_error(other, cursor, log)
    end
  end

  @doc """
  Parses an expression with a minimum binding power.
  Used for map updates where we need to stop before the pipe operator.
  Also used for guard expressions in stab clauses where we need to stop before ->.
  """
  @spec parse_with_min_bp(
          State.t(),
          Cursor.t(),
          context(),
          EventLog.t(),
          non_neg_integer(),
          keyword()
        ) ::
          result()
  def parse_with_min_bp(
        %State{} = state,
        cursor,
        %Context{} = context,
        %EventLog{} = log,
        min_bp,
        opts \\ []
      ) do
    # Merge opts with min_bp for nud - preserve stop_at_assoc for string parsing
    nud_opts = Keyword.merge([min_bp: min_bp], opts)

    with {:ok, token, state, cursor} <- TokenAdapter.next(state, cursor),
         {:ok, left, state, cursor, log} <- nud(token, state, cursor, context, log, nud_opts) do
      led(left, state, cursor, log, min_bp, context, opts)
    else
      {:eof, state, cursor} -> {:error, :unexpected_eof, state, cursor, log}
      # Handle keyword_key from Strings.parse - bubble up to caller
      {:keyword_key, _, _, _, _, _} = keyword_key -> keyword_key
      {:keyword_key_interpolated, _, _, _, _, _, _, _} = keyword_key -> keyword_key
      other -> Result.normalize_error(other, cursor, log)
    end
  end

  @doc "Exposes binary binding power."
  @spec bp(atom()) :: Precedence.bp() | nil
  def bp(kind) do
    case Precedence.binary(kind) do
      {bp, _assoc} -> bp
      _ -> nil
    end
  end

  @doc "Exposes unary binding power."
  @spec unary_bp(atom()) :: Precedence.bp() | nil
  def unary_bp(kind) do
    case Precedence.unary(kind) do
      {bp, _assoc} -> bp
      _ -> nil
    end
  end

  # Handle unary operators in nud (null denotation)
  defp nud(token, state, cursor, context, log) do
    nud(token, state, cursor, context, log, [])
  end

  defp nud({token_kind, _meta, token_value} = token, state, cursor, context, log, opts) do
    min_bp = Keyword.get(opts, :min_bp, 0)
    allow_containers = Keyword.get(opts, :allow_containers, true)
    string_min_bp = Keyword.get(opts, :string_min_bp, min_bp)
    # Pass through options like stop_at_assoc to string parsing
    led_opts = Keyword.take(opts, [:stop_at_assoc])

    case token_kind do
      :error_token ->
        meta = TokenAdapter.token_meta(token)
        ast = Builder.Helpers.error(token_value, meta)
        {:ok, ast, state, cursor, log}

      :capture_int ->
        parse_capture_int(token, state, cursor, log)

      # Container tokens need to be handled by Containers.parse
      kind
      when kind in [
             :"[",
             :"{",
             :"(",
             :"<<",
             :%,
             :%{},
             :"%]"
           ] ->
        if allow_containers do
          {state, cursor} = TokenAdapter.pushback(state, cursor, token)
          alias ToxicParser.Grammar.Containers
          Containers.parse(state, cursor, context, log, min_bp, led_opts)
        else
          nud_literal_or_unary(token, state, cursor, context, log, min_bp, false, opts)
        end

      # Quoted atoms need to be handled by Strings.parse even in restricted mode
      kind when kind in [:atom_unsafe_start, :atom_safe_start] ->
        {state, cursor} = TokenAdapter.pushback(state, cursor, token)
        alias ToxicParser.Grammar.Strings
        Strings.parse(state, cursor, context, log, string_min_bp, led_opts)

      # Other string/sigil tokens - always parse strings, they're not containers
      # This allows %''{}` (struct with empty charlist) to work
      kind
      when kind in [
             :bin_string_start,
             :list_string_start,
             :bin_heredoc_start,
             :list_heredoc_start,
             :sigil_start
           ] ->
        {state, cursor} = TokenAdapter.pushback(state, cursor, token)
        alias ToxicParser.Grammar.Strings
        Strings.parse(state, cursor, context, log, min_bp, led_opts)

      # fn tokens need to be handled by Blocks.parse unless restricted
      :fn ->
        if allow_containers do
          {state, cursor} = TokenAdapter.pushback(state, cursor, token)
          alias ToxicParser.Grammar.Blocks
          Blocks.parse(state, cursor, context, log)
        else
          nud_literal_or_unary(token, state, cursor, context, log, min_bp, false, opts)
        end

      # kw_identifier at expression position is a syntax error
      :kw_identifier ->
        {:error, syntax_error_before(TokenAdapter.token_meta(token), token_value), state, cursor,
         log}

      _ ->
        nud_literal_or_unary(token, state, cursor, context, log, min_bp, allow_containers, opts)
    end
  end

  # allow_containers: when false (e.g., struct base parsing), don't parse no-parens calls
  # The grammar's map_base_expr only produces bare identifiers via sub_matched_expr
  defp nud_literal_or_unary(
         {token_kind, _meta, token_value} = token,
         state,
         cursor,
         context,
         log,
         min_bp,
         allow_containers,
         opts
       ) do
    case Precedence.unary(token_kind) do
      {_bp, _assoc} ->
        # Pass outer min_bp for trailing led(), not the unary's own precedence.
        # The operand_min_bp is computed inside parse_unary from the operator's precedence.
        parse_unary(token, state, cursor, context, log)

      nil ->
        cond do
          token_kind == :dual_op ->
            # Pass outer min_bp for trailing led()
            parse_unary(token, state, cursor, context, log)

          # ternary_op ://" used as unary (e.g., //foo) becomes {:/,_,[{:/,_,nil},rhs]}
          token_kind == :ternary_op and token_value == :"//" ->
            parse_ternary_unary(token, state, cursor, context, log)

          # Binary-only operators cannot start an expression
          # e.g., "1 + * 3" should error because * cannot follow +
          is_binary_only_op(token_kind) ->
            meta = TokenAdapter.token_meta(token)
            {:error, syntax_error_before(meta, token_value), state, cursor, log}

          # Semicolons cannot start an expression
          # e.g., "1+;\n2" should error because ; cannot follow +
          # Note: newlines are handled separately by EOE.skip_newlines_only
          token_kind == :";" ->
            meta = TokenAdapter.token_meta(token)
            {:error, syntax_error_before(meta, "';'"), state, cursor, log}

          # Comma cannot start an expression
          # e.g., "if true do\n  foo = [],\n  baz\nend" - comma after [] is invalid
          token_kind == :"," ->
            meta = TokenAdapter.token_meta(token)
            {:error, syntax_error_before(meta, "','"), state, cursor, log}

          # Block identifiers (after, else, catch, rescue) are only valid as block labels
          # They cannot be used as variables or expressions outside block context
          # e.g., "after = 1" is invalid
          token_kind == :block_identifier ->
            meta = TokenAdapter.token_meta(token)
            {:error, syntax_error_before(meta, "'#{token_value}'"), state, cursor, log}

          true ->
            nud_identifier_or_literal(
              token,
              state,
              cursor,
              context,
              log,
              min_bp,
              allow_containers,
              opts
            )
        end
    end
  end

  defp nud_identifier_or_literal(
         {token_kind, _meta, _value} = token,
         state,
         cursor,
         context,
         log,
         min_bp,
         allow_containers,
         opts
       ) do
    allow_do_blocks = Context.allow_do_block?(context)

    case TokenAdapter.peek(state, cursor) do
      {:ok, {next_kind, _meta, _value} = next_tok, _state, _cursor} ->
        binary_bp = bp(next_kind)

        allow_no_parens =
          token_kind == :op_identifier or
            (is_nil(binary_bp) and
               (NoParens.can_start_no_parens_arg?(next_tok) or
                  Keywords.starts_kw?(next_tok)))

        # Container tokens ({, [, <<, %{}) CAN be no-parens call arguments
        # e.g., `foo {:ok, a}` or `with {:ok, a} <- b, do: c`
        # Aliases cannot have no-parens call syntax - `Foo x` is invalid
        # bracket_identifier means tokenizer determined this is bracket access (foo[x])
        # not a no-parens call - let led_bracket handle it
        # Literals cannot be function calls - only identifiers can:
        # - nil, true, false are boolean/nil literals
        # - :int, :flt, :char, :atom are other literal token kinds
        # When allow_containers is false, don't parse no-parens calls
        literal_kinds = [:int, :flt, :char, :atom]

        excluded_kinds = [
          :paren_identifier,
          :alias,
          :bracket_identifier,
          nil,
          true,
          false | literal_kinds
        ]

        if token_kind not in excluded_kinds and
             allow_no_parens and
             allow_containers do
          call_min_bp = if Keyword.get(opts, :unary_operand, false), do: 0, else: min_bp

          parse_no_parens_call_nud_with_min_bp(
            token,
            state,
            cursor,
            context,
            log,
            call_min_bp,
            opts
          )
        else
          ast = literal_to_ast(token, state)

          maybe_attach_do_block(
            ast,
            token,
            state,
            cursor,
            context,
            log,
            min_bp,
            allow_do_blocks,
            opts
          )
        end

      _ ->
        ast = literal_to_ast(token, state)

        maybe_attach_do_block(
          ast,
          token,
          state,
          cursor,
          context,
          log,
          min_bp,
          allow_do_blocks,
          opts
        )
    end
  end

  defp maybe_attach_do_block(ast, token, state, cursor, context, log, min_bp, true, opts) do
    DoBlocks.maybe_do_block(ast, state, cursor, context, log,
      token: token,
      min_bp: min_bp,
      parse_no_parens: fn tok, st, cur, ctx, lg, min_bp ->
        parse_no_parens_call_nud_with_min_bp(tok, st, cur, ctx, lg, min_bp, opts)
      end
    )
  end

  defp maybe_attach_do_block(ast, _token, state, cursor, _context, log, _min_bp, false, _opts) do
    {:ok, ast, state, cursor, log}
  end

  # Parse capture_int followed by int (e.g., &1, &2)
  # Rule: access_expr -> capture_int int : build_unary_op('$1', number_value('$2'))
  # NOTE: For capture arguments without whitespace (&1, &2, etc.), the integer
  # is NOT passed through literal_encoder. Only `& 1` (with space) gets encoded.
  defp parse_capture_int(capture_token, state, cursor, log) do
    case TokenAdapter.next(state, cursor) do
      {:ok, {:int, {_, _, parsed_value}, _raw_string}, state, cursor} ->
        meta = TokenAdapter.token_meta(capture_token)
        # For capture arguments (&1, &2, etc.), DON'T encode the integer
        # This matches Elixir's behavior where &1 is NOT encoded but & 1 IS
        ast = {:&, meta, [parsed_value]}
        {:ok, ast, state, cursor, log}

      {:ok, {got_kind, _meta, _value}, state, cursor} ->
        {:error, {:expected, :int, got: got_kind}, state, cursor, log}

      {:eof, state, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, state, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  # Parse a no-parens call in nud context (for identifiers followed by args)
  # This is similar to Calls.parse_no_parens_call but doesn't call led at the end
  # This version uses min_bp to stop argument parsing before certain operators
  defp parse_no_parens_call_nud_with_min_bp(
         {callee_kind, _meta, callee} = callee_tok,
         state,
         cursor,
         context,
         log,
         min_bp,
         opts
       ) do
    with {:ok, args, state, cursor, log} <-
           Calls.parse_no_parens_args([], state, cursor, context, log, min_bp, opts) do
      base_meta = TokenAdapter.token_meta(callee_tok)

      # Check if there's a do-block following that will attach to this call.
      # Only counts as "has do-block" if context allows do-block attachment.
      # When context has allow_do_block: false, the do-block belongs to an outer call.
      has_do_block =
        Context.allow_do_block?(context) and
          match?({:ok, {:do, _, _}, _state, _cursor}, TokenAdapter.peek(state, cursor))

      # For op_identifier with a single argument AND no do-block, add ambiguous_op: nil
      # This matches elixir_parser.yrl behavior for no_parens_one_ambig
      # Don't add it when there's a do-block because then there's no ambiguity
      meta =
        if callee_kind == :op_identifier and length(args) == 1 and
             not has_do_block do
          [ambiguous_op: nil] ++ base_meta
        else
          base_meta
        end

      ast = {callee, meta, args}
      # Check for do-block ONLY, don't call led (caller will do that)
      DoBlocks.maybe_do_block(ast, state, cursor, context, log)
    end
  end

  # Parse unary operator expression
  defp parse_unary({op_kind, _meta, op_value} = op_token, state, cursor, context, log) do
    # For ellipsis/range operators, check for EOE terminator BEFORE skipping
    # This ensures "x...;" correctly sees the ; as a terminator
    # and doesn't skip it, allowing `led` to see the EOE separation
    if op_kind in [:ellipsis_op, :range_op] do
      case TokenAdapter.peek(state, cursor) do
        {:ok, {kind, _meta, _value}, _state, _cursor} when kind in [:eol, :";"] ->
          # Separator is a terminator for ellipsis/range - return standalone
          ast = {op_value, TokenAdapter.token_meta(op_token), []}
          {:ok, ast, state, cursor, log}

        _ ->
          # Not a separator, proceed with normal parsing
          parse_unary_operand_or_standalone(op_token, state, cursor, context, log)
      end
    else
      # Skip EOE after unary operator (allows "!\ntrue")
      # Note: Unlike binary ops, unary ops don't include newlines metadata
      {state, cursor, _newlines} = EOE.skip_count_newlines(state, cursor, 0)
      parse_unary_after_eoe_skip(op_token, state, cursor, context, log)
    end
  end

  # Parse unary operand for ellipsis/range after confirming no EOE terminator
  defp parse_unary_operand_or_standalone(op_token, state, cursor, context, log) do
    # Skip EOE after operator
    {state, cursor, _newlines} = EOE.skip_count_newlines(state, cursor, 0)
    parse_unary_after_eoe_skip(op_token, state, cursor, context, log)
  end

  # Common unary parsing logic after EOE is handled
  defp parse_unary_after_eoe_skip(
         {op_kind, _meta, op_value} = op_token,
         state,
         cursor,
         context,
         log
       ) do
    case TokenAdapter.next(state, cursor) do
      {:ok, {operand_kind, operand_meta, operand_value} = operand_token, state, cursor} ->
        # Special case: ... and .. followed by pure binary operator or terminator should be standalone
        # e.g., "... * 1" should parse as (* (...) 1), not (... *)
        # e.g., "fn -> ... end" - the ... before end should be standalone
        # BUT: dual_op (+/-) after ellipsis should be unary, not binary
        # e.g., "... + 1 * 2" should parse as "...((+1) * 2)" not "(... + 1) * 2"
        ellipsis_terminators = [
          :end,
          :do,
          :eol,
          :";",
          :")",
          :"]",
          :"}",
          :">>",
          :end_interpolation,
          :",",
          :when_op,
          :stab_op,
          :block_identifier
        ]

        # dual_op can be unary (+/-), ternary_op can be unary (//)
        # so they should NOT trigger standalone ellipsis
        is_pure_binary_op =
          operand_kind not in [:dual_op, :ternary_op] and
            Precedence.binary(operand_kind) != nil

        if op_kind in [:ellipsis_op, :range_op] and
             (is_pure_binary_op or operand_kind in ellipsis_terminators) do
          ast = {op_value, TokenAdapter.token_meta(op_token), []}
          {state, cursor} = TokenAdapter.pushback(state, cursor, operand_token)
          {:ok, ast, state, cursor, log}
          # Special case: at_op + bracket_identifier (e.g., @foo[1])
          # Grammar: bracket_at_expr -> at_op_eol dot_bracket_identifier bracket_arg
          # The @foo becomes the subject, bracket access is handled at outer level
        else
          if op_kind == :at_op do
            at_bp =
              case Precedence.unary(:at_op) do
                {bp, _assoc} -> bp
                _ -> 320
              end

            if operand_kind == :bracket_identifier do
              op = op_value
              meta = TokenAdapter.token_meta(op_token)
              # Use raw meta tuple (not keyword list) for from_token compatibility
              operand =
                Builder.Helpers.from_token({:identifier, operand_meta, operand_value})

              ast = Builder.Helpers.unary(op, operand, meta)

              case TokenAdapter.peek(state, cursor) do
                {:ok, tok, _state, _cursor} when is_kind(tok, :"[") ->
                  led_bracket(ast, state, cursor, log, at_bp, context, [])

                _ ->
                  {:ok, ast, state, cursor, log}
              end
            else
              operand_token_for_parse = operand_token

              # Follow elixir_parser.yrl at_op rules:
              # - matched_expr -> at_op_eol matched_expr
              # - unmatched_expr -> at_op_eol expr
              # - no_parens_expr -> at_op_eol no_parens_expr
              {operand_ctx, operand_min_bp0} =
                cond do
                  # no_parens_expr -> at_op_eol no_parens_expr
                  Context.allow_no_parens_expr?(context) and not Context.allow_do_block?(context) ->
                    {Context.no_parens_expr(), at_bp}

                  # unmatched_expr -> at_op_eol expr
                  Context.allow_do_block?(context) ->
                    {Context.expr(), at_bp}

                  # matched_expr -> at_op_eol matched_expr
                  true ->
                    {Context.matched_expr(), at_bp}
                end

              operand_min_bp =
                if operand_ctx.allow_do_block == false and
                     operand_ctx.allow_no_parens_expr == false do
                  10_000
                else
                  operand_min_bp0
                end

              {ref, checkpoint_state} = TokenAdapter.checkpoint(state, cursor)

              with {:ok, operand_base, state_after_base, cursor_after_base, log} <-
                     parse_rhs(
                       operand_token_for_parse,
                       checkpoint_state,
                       cursor,
                       operand_ctx,
                       log,
                       operand_min_bp,
                       unary_operand: true
                     ) do
                operand_result =
                  case TokenAdapter.peek(state_after_base, cursor_after_base) do
                    # Allow bracket access to bind inside the operand when the operand itself is an @-expr.
                    # This matches cases like `@ @ (case ... end)[x]` where the bracket applies to the inner @,
                    # and the outer @ wraps the whole access_expr.
                    {:ok, tok, _state, _cursor} when is_kind(tok, :"[") ->
                      if match?({:@, _, [_]}, operand_base) do
                        with {:ok, operand_with_bracket, state_after_bracket,
                              cursor_after_bracket, log} <-
                               led_bracket(
                                 operand_base,
                                 state_after_base,
                                 cursor_after_base,
                                 log,
                                 at_bp,
                                 operand_ctx,
                                 []
                               ) do
                          {:ok, operand_with_bracket,
                           TokenAdapter.drop_checkpoint(state_after_bracket, ref),
                           cursor_after_bracket, log}
                        end
                      else
                        {:ok, operand_base, TokenAdapter.drop_checkpoint(state_after_base, ref),
                         cursor_after_base, log}
                      end

                    {:ok, {next_tok_kind, _meta, _value}, _state, _cursor} ->
                      if operand_ctx.allow_do_block and do_block_expr?(operand_base) do
                        case {next_tok_kind, bp(next_tok_kind)} do
                          {_, nil} ->
                            {:ok, operand_base,
                             TokenAdapter.drop_checkpoint(state_after_base, ref),
                             cursor_after_base, log}

                          {kind, next_bp}
                          when kind in [:., :dot_call_op] and next_bp < at_bp ->
                            {:ok, operand_base,
                             TokenAdapter.drop_checkpoint(state_after_base, ref),
                             cursor_after_base, log}

                          {_, next_bp} when next_bp < at_bp ->
                            {state_full, cursor_full} =
                              TokenAdapter.rewind(state_after_base, cursor_after_base, ref)

                            with {:ok, operand_full, state_full, cursor_full, log} <-
                                   parse_rhs(
                                     operand_token_for_parse,
                                     state_full,
                                     cursor_full,
                                     operand_ctx,
                                     log,
                                     0,
                                     unary_operand: true
                                   ) do
                              {:ok, operand_full, state_full, cursor_full, log}
                            end

                          _ ->
                            {:ok, operand_base,
                             TokenAdapter.drop_checkpoint(state_after_base, ref),
                             cursor_after_base, log}
                        end
                      else
                        {:ok, operand_base, TokenAdapter.drop_checkpoint(state_after_base, ref),
                         cursor_after_base, log}
                      end

                    _ ->
                      {:ok, operand_base, TokenAdapter.drop_checkpoint(state_after_base, ref),
                       cursor_after_base, log}
                  end

                with {:ok, operand, state, cursor, log} <- operand_result do
                  op = op_value
                  meta = TokenAdapter.token_meta(op_token)
                  ast = Builder.Helpers.unary(op, operand, meta)
                  {:ok, ast, state, cursor, log}
                end
              end
            end
          else
            # In Elixir's grammar, unary operators take different operand types:
            # - ellipsis_op always takes matched_expr (min_bp=100)
            # - @/& take call expressions (identifier with optional args/do-block)
            #   but NOT trailing binary operators
            #
            # For @/&, we use Calls.parse_without_led to get the call without led(),
            # then led() is called at the END of parse_unary to handle trailing ops.
            # This ensures "@foo.bar" parses as "(@foo).bar" not "@(foo.bar)"
            # Elixir's yecc grammar uses operator precedence for unary operands:
            # - unary_op_eol (300): "+ a ** b" -> "(+ a) ** b" since 300 > 230
            # - capture_op_eol (90): "& a ** b" -> "& (a ** b)" since 90 < 230
            # - ellipsis_op (90): "... a ** b" -> "... (a ** b)" since 90 < 230
            #
            # For ellipsis/range, operand is always matched_expr (min_bp=100):
            # - "... a ** b" parses as "...(a ** b)" (power_op bp=230 >= 100)
            # - "... a | b" parses as "(... a) | b" (pipe_op bp=70 < 100)
            #
            # For other unary ops, use the unary op's precedence as min_bp.
            # This implements the standard Pratt parser precedence model.
            # - unary_op (300): "+a[e]" -> "+(a[e])" since bracket (310) > 300
            # Exception: @ (at_op) should capture full expressions including low-precedence
            # operators like `when` for module attributes (@spec foo() when ...).
            # Bracket access for @ is handled separately in led_bracket.
            {_next_token, next_token_kind, next_token_value} =
              case TokenAdapter.peek(state, cursor) do
                {:ok, {kind, _meta, value} = tok, _state, _cursor} -> {tok, kind, value}
                _ -> {nil, nil, nil}
              end

            operand_min_bp =
              cond do
                operand_kind == :do_identifier and Context.allow_do_block?(context) ->
                  0

                operand_kind == :identifier and next_token_kind == :do_identifier and
                    Context.allow_do_block?(context) ->
                  0

                operand_kind == :identifier and next_token_kind == :identifier and
                  next_token_value in @do_block_keywords and Context.allow_do_block?(context) ->
                  0

                op_kind in [:ellipsis_op, :range_op] ->
                  100

                true ->
                  case Precedence.unary(op_kind) do
                    {bp, _assoc} -> bp
                    nil -> 300
                  end
              end

            # Most unary operator operands must NOT allow no_parens_expr extension.
            # The extension bypasses min_bp which would break "-1 + 2" (making it "-(1+2)").
            # Exception: ellipsis_op follows yecc rules and can take expr/no_parens_expr operands.
            # The outer context is preserved for led() after the unary is built.
            operand_context =
              if op_kind == :ellipsis_op do
                cond do
                  # no_parens_expr -> ellipsis_op no_parens_expr
                  Context.allow_no_parens_expr?(context) and not Context.allow_do_block?(context) ->
                    Context.no_parens_expr()

                  # unmatched_expr -> ellipsis_op expr
                  Context.allow_do_block?(context) ->
                    Context.expr()

                  # matched_expr -> ellipsis_op matched_expr
                  true ->
                    Context.matched_expr()
                end
              else
                restrict_no_parens_extension(context)
              end

            {ref, checkpoint_state} = TokenAdapter.checkpoint(state, cursor)

            with {:ok, operand_base, state_after_base, cursor_after_base, log} <-
                   parse_rhs(
                     operand_token,
                     checkpoint_state,
                     cursor,
                     operand_context,
                     log,
                     operand_min_bp,
                     unary_operand: true
                   ) do
              operand_result =
                case TokenAdapter.peek(state_after_base, cursor_after_base) do
                  {:ok, {next_tok_kind, _meta, _value}, _state, _cursor} ->
                    # elixir_parser.yrl: unmatched_expr -> unary_op_eol expr
                    # When the operand is a do-block expression, allow lower-precedence operators
                    # to bind *inside* the unary operand by reparsing with min_bp=0.
                    if operand_context.allow_do_block and
                         (do_block_expr?(operand_base) or
                            (op_kind in [:ellipsis_op, :range_op] and
                               contains_do_block?(operand_base))) and
                         not (op_kind == :dual_op and has_parens_meta?(operand_base)) do
                      case bp(next_tok_kind) do
                        next_bp when is_integer(next_bp) and next_bp < operand_min_bp ->
                          {state_full, cursor_full} =
                            TokenAdapter.rewind(state_after_base, cursor_after_base, ref)

                          with {:ok, operand_full, state_full, cursor_full, log} <-
                                 parse_rhs(
                                   operand_token,
                                   state_full,
                                   cursor_full,
                                   operand_context,
                                   log,
                                   0, unary_operand: true) do
                            {:ok, operand_full, state_full, cursor_full, log}
                          end

                        _ ->
                          {:ok, operand_base, TokenAdapter.drop_checkpoint(state_after_base, ref),
                           cursor_after_base, log}
                      end
                    else
                      if op_kind == :capture_op do
                        function_capture? = function_capture_operand?(operand_base)

                        case {function_capture?, contains_do_block?(operand_base),
                              bp(next_tok_kind)} do
                          {false, true, next_bp}
                          when is_integer(next_bp) and next_bp < operand_min_bp ->
                            {state_full, cursor_full} =
                              TokenAdapter.rewind(state_after_base, cursor_after_base, ref)

                            with {:ok, operand_full, state_full, cursor_full, log} <-
                                   parse_rhs(
                                     operand_token,
                                     state_full,
                                     cursor_full,
                                     operand_context,
                                     log,
                                     0, unary_operand: true) do
                              {:ok, operand_full, state_full, cursor_full, log}
                            end

                          _ ->
                            {:ok, operand_base,
                             TokenAdapter.drop_checkpoint(state_after_base, ref),
                             cursor_after_base, log}
                        end
                      else
                        {:ok, operand_base, TokenAdapter.drop_checkpoint(state_after_base, ref),
                         cursor_after_base, log}
                      end
                    end

                  _ ->
                    {:ok, operand_base, TokenAdapter.drop_checkpoint(state_after_base, ref),
                     cursor_after_base, log}
                end

              with {:ok, operand, state, cursor, log} <- operand_result do
                op = op_value
                meta = TokenAdapter.token_meta(op_token)
                ast = Builder.Helpers.unary(op, operand, meta)

                # After building the unary, continue with led() to handle trailing operators.
                # Use the unary's precedence (operand_min_bp) so only higher-precedence operators
                # can attach.
                led(ast, state, cursor, log, operand_min_bp, context)
              end
            end
          end
        end

      {:eof, state, cursor} ->
        # Special case: ... and .. can be standalone at EOF
        if op_kind in [:ellipsis_op, :range_op] do
          ast = {op_value, TokenAdapter.token_meta(op_token), []}
          {:ok, ast, state, cursor, log}
        else
          {:error, :unexpected_eof, state, cursor, log}
        end

      {:error, diag, state, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  # Parse ternary_op :"//" as binary operator (e.g., 1..10//2)
  # When left is a range {:.., meta, [start, stop]}, combines into {:..//, meta, [start, stop, step]}
  # Otherwise, produces an error (// must follow ..)
  defp parse_ternary_op(left, state, cursor, min_bp, context, log) do
    {:ok, {_op_kind, _op_meta, op_value} = op_token, state, cursor} =
      TokenAdapter.next(state, cursor)

    {bp, _assoc} = Precedence.binary(:ternary_op)

    # Skip EOE after operator
    {state, cursor, _newlines} = EOE.skip_count_newlines(state, cursor, 0)

    case TokenAdapter.next(state, cursor) do
      {:ok, rhs_token, state, cursor} ->
        with {:ok, step, state, cursor, log} <-
               parse_rhs(rhs_token, state, cursor, context, log, bp) do
          case left do
            # Range expression: {:.., meta, [start, stop]} -> {:..//, meta, [start, stop, step]}
            {:.., range_meta, [start, stop]} ->
              combined = {:..//, range_meta, [start, stop, step]}
              led(combined, state, cursor, log, min_bp, context)

            # Not a range - this is an error but we still parse it as {:"//", meta, [left, step]}
            _ ->
              op_meta = TokenAdapter.token_meta(op_token)
              combined = {op_value, op_meta, [left, step]}
              led(combined, state, cursor, log, min_bp, context)
          end
        end

      {:eof, state, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, state, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  # Parse ternary_op :"//" as unary operator (e.g., //foo)
  # This produces: {:/, outer_meta, [{:/, inner_meta, nil}, rhs]}
  # where outer has column+1 and inner has original column
  defp parse_ternary_unary(op_token, state, cursor, context, log) do
    meta = TokenAdapter.token_meta(op_token)

    # Calculate inner/outer metadata with column adjustment
    {outer_meta, inner_meta} =
      case {Keyword.get(meta, :line), Keyword.get(meta, :column)} do
        {line, col} when is_integer(line) and is_integer(col) ->
          rest_meta = Keyword.drop(meta, [:line, :column])
          {[line: line, column: col + 1] ++ rest_meta, [line: line, column: col] ++ rest_meta}

        _ ->
          {meta, meta}
      end

    # Skip EOE after operator
    {state, cursor, _newlines} = EOE.skip_count_newlines(state, cursor, 0)

    {ref, checkpoint_state} = TokenAdapter.checkpoint(state, cursor)

    case TokenAdapter.next(checkpoint_state, cursor) do
      {:ok, operand_token, checkpoint_state, cursor_after_next} ->
        # Base case: unary_op_eol precedence is 300 in elixir_parser.yrl (higher than power_op_eol=230).
        with {:ok, operand_base, state_after_base, cursor_after_base, log} <-
               parse_rhs(operand_token, checkpoint_state, cursor_after_next, context, log, 300,
                 unary_operand: true
               ) do
          operand_result =
            case TokenAdapter.peek(state_after_base, cursor_after_base) do
              {:ok, {next_kind, _meta, _value}, _state, _cursor} ->
                if do_block_expr?(operand_base) do
                  case bp(next_kind) do
                    nil ->
                      {:ok, operand_base, TokenAdapter.drop_checkpoint(state_after_base, ref),
                       cursor_after_base, log}

                    next_bp when next_bp < 300 ->
                      # Elixir parses `// foo do ... end ** 2` as `// (foo do ... end ** 2)`.
                      # Reparse the operand allowing trailing operators when the base is a do-block expr.
                      {state_full, cursor_full} =
                        TokenAdapter.rewind(state_after_base, cursor_after_base, ref)

                      with {:ok, tok, state_full, cursor_full} <-
                             TokenAdapter.next(state_full, cursor_full),
                           {:ok, operand_full, state_full, cursor_full, log} <-
                             parse_rhs(tok, state_full, cursor_full, context, log, 0,
                               unary_operand: true
                             ) do
                        {:ok, operand_full, state_full, cursor_full, log}
                      end

                    _ ->
                      {:ok, operand_base, TokenAdapter.drop_checkpoint(state_after_base, ref),
                       cursor_after_base, log}
                  end
                else
                  {:ok, operand_base, TokenAdapter.drop_checkpoint(state_after_base, ref),
                   cursor_after_base, log}
                end

              _ ->
                {:ok, operand_base, TokenAdapter.drop_checkpoint(state_after_base, ref),
                 cursor_after_base, log}
            end

          with {:ok, operand, state, cursor, log} <- operand_result do
            ast = {:/, outer_meta, [{:/, inner_meta, nil}, operand]}
            {:ok, ast, state, cursor, log}
          end
        end

      {:eof, checkpoint_state, cursor_after_next} ->
        state = TokenAdapter.drop_checkpoint(checkpoint_state, ref)
        {:error, :unexpected_eof, state, cursor_after_next, log}

      {:error, diag, checkpoint_state, cursor_after_next} ->
        state = TokenAdapter.drop_checkpoint(checkpoint_state, ref)
        {:error, diag, state, cursor_after_next, log}
    end
  end

  # Parse RHS of binary operator, handling chained operators with proper precedence
  # For identifiers that could be calls with do-blocks (like `if true do...end`),
  # we need special handling but must preserve min_bp for associativity
  # opts can contain :unary_operand to indicate we're parsing a unary operator's operand
  defp parse_rhs(
         {token_kind, _meta, _value} = token,
         state,
         cursor,
         context,
         log,
         min_bp,
         opts \\ []
       ) do
    # Check if this is an identifier that could be a call with arguments
    cond do
      Identifiers.classify(token_kind) != :other ->
        # Handle identifier specially to preserve min_bp
        parse_rhs_identifier(token, state, cursor, context, log, min_bp, opts)

      # Container tokens need special handling to preserve min_bp
      token_kind in [:"[", :"{", :"(", :"<<", :%, :%{}, :%] ->
        {state, cursor} = TokenAdapter.pushback(state, cursor, token)
        alias ToxicParser.Grammar.Containers

        # Call Containers.parse with min_bp to preserve operator precedence
        Containers.parse(state, cursor, context, log, min_bp, opts)

      # String and quoted atom tokens need special handling to preserve min_bp
      token_kind in [
        :bin_string_start,
        :list_string_start,
        :bin_heredoc_start,
        :list_heredoc_start,
        :sigil_start,
        :atom_unsafe_start,
        :atom_safe_start
      ] ->
        {state, cursor} = TokenAdapter.pushback(state, cursor, token)
        alias ToxicParser.Grammar.Strings

        # Call Strings.parse with min_bp to preserve operator precedence
        Strings.parse(state, cursor, context, log, min_bp, opts)

      true ->
        nud_opts = Keyword.put(opts, :min_bp, min_bp)

        with {:ok, right, state, cursor, log} <- nud(token, state, cursor, context, log, nud_opts) do
          opts = ensure_no_parens_extension_opt(opts)
          led(right, state, cursor, log, min_bp, context, opts)
        end
    end
  end

  # Parse identifier on RHS, handling calls while preserving min_bp for associativity
  # opts can contain :unary_operand to indicate we're parsing a unary operator's operand
  defp parse_rhs_identifier(
         {token_kind, _meta, _value} = token,
         state,
         cursor,
         context,
         log,
         min_bp,
         opts
       ) do
    case TokenAdapter.peek(state, cursor) do
      {:ok, {:"(", _meta, _value}, _state, _cursor} when is_kind(token, :paren_identifier) ->
        # Paren call (no space before '(') - parse directly to preserve min_bp.
        # Don't use Calls.parse because it calls led(_, 0, _) which loses min_bp.
        # Pass opts through so do-blocks after paren call know the context.
        with {:ok, ast, state, cursor, log} <-
               parse_paren_call_base(token, state, cursor, context, log) do
          opts = ensure_no_parens_extension_opt(opts)
          maybe_nested_call_or_do_block(ast, state, cursor, log, min_bp, context, opts)
        end

      {:ok, tok, _state, _cursor} when is_kind(tok, :"(") ->
        # `foo (expr)` is a no-parens call where the `(` begins a parenthesized
        # expression argument (it is NOT a paren-call like `foo(expr)`).
        # This matters because trailing operators (e.g. `foo (1 <<< 7) - 1`)
        # belong to the argument expression in Elixir.
        {state, cursor} = TokenAdapter.pushback(state, cursor, token)

        with {:ok, right, state, cursor, log} <-
               Calls.parse_without_led(state, cursor, context, log, min_bp, opts) do
          # Preserve min_bp so unary operands do not swallow lower-precedence operators.
          led_min_bp = min_bp
          opts = ensure_no_parens_extension_opt(opts)
          led(right, state, cursor, log, led_min_bp, context, opts)
        end

      {:ok, {:do, _meta, _value}, _state, _cursor} when context.allow_do_block ->
        # Do-block - delegate to Calls.parse_without_led
        {state, cursor} = TokenAdapter.pushback(state, cursor, token)

        with {:ok, right, state, cursor, log} <-
               Calls.parse_without_led(state, cursor, context, log, min_bp, opts) do
          # Preserve min_bp so unary operands do not swallow lower-precedence operators.
          led_min_bp = min_bp
          opts = ensure_no_parens_extension_opt(opts)
          led(right, state, cursor, log, led_min_bp, context, opts)
        end

      {:ok, tok, _state, _cursor} when is_kind(tok, :do) ->
        # Do-blocks not allowed in this context (matched_expr); leave `do` for the caller.
        ast = Builder.Helpers.from_token(token)
        opts = ensure_no_parens_extension_opt(opts)
        led(ast, state, cursor, log, min_bp, context, opts)

      {:ok, {next_tok_kind, _meta, _value} = next_tok, _state, _cursor} ->
        cond do
          # Alias followed by [ is bracket access, not a no-parens call
          # Grammar: bracket_expr -> access_expr bracket_arg
          # Build alias AST and let led handle the [ as bracket access
          token_kind == :alias and next_tok_kind == :"[" ->
            ast = Builder.Helpers.from_token(token)
            opts = ensure_no_parens_extension_opt(opts)
            led(ast, state, cursor, log, min_bp, context, opts)

          # op_identifier means tokenizer determined this is a no-parens call
          # (e.g., `a -2` where `-2` is unary argument, not binary subtraction)
          # This must be checked BEFORE binary operator check
          # TODO: no coverage? only failing corpus
          token_kind == :op_identifier and
              (NoParens.can_start_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok)) ->
            {state, cursor} = TokenAdapter.pushback(state, cursor, token)

            with {:ok, right, state, cursor, log} <-
                   Calls.parse_without_led(state, cursor, context, log, min_bp, opts) do
              # Preserve the original min_bp for proper associativity
              opts = ensure_no_parens_extension_opt(opts)
              led(right, state, cursor, log, min_bp, context, opts)
            end

          # Binary operator follows - just return identifier, led will handle with min_bp
          bp(next_tok_kind) ->
            ast = Builder.Helpers.from_token(token)
            opts = ensure_no_parens_extension_opt(opts)
            led(ast, state, cursor, log, min_bp, context, opts)

          # Could be no-parens call argument - delegate to Calls
          # TODO: no coverage? only failing corpus
          NoParens.can_start_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok) ->
            {state, cursor} = TokenAdapter.pushback(state, cursor, token)

            call_min_bp = if Keyword.get(opts, :unary_operand, false), do: 0, else: min_bp

            with {:ok, right, state, cursor, log} <-
                   Calls.parse_without_led(state, cursor, context, log, call_min_bp, opts) do
              # Preserve min_bp so unary operands do not swallow lower-precedence operators.
              led_min_bp = min_bp

              opts = ensure_no_parens_extension_opt(opts)
              led(right, state, cursor, log, led_min_bp, context, opts)
            end

          # Just a bare identifier
          true ->
            ast = Builder.Helpers.from_token(token)
            opts = ensure_no_parens_extension_opt(opts)
            led(ast, state, cursor, log, min_bp, context, opts)
        end

      _ ->
        # EOF or error - just return identifier
        ast = Builder.Helpers.from_token(token)
        opts = ensure_no_parens_extension_opt(opts)
        led(ast, state, cursor, log, min_bp, context, opts)
    end
  end

  @doc """
  Parse a paren call without calling led at the end.
  Used by parse_rhs_identifier to preserve min_bp for associativity.
  Also used by Maps.parse_unary_operand for %@i(){} patterns.
  """
  @spec parse_paren_call_base(tuple(), State.t(), Cursor.t(), context(), EventLog.t()) :: result()
  def parse_paren_call_base(
        {_, _, callee} = callee_tok,
        %State{} = state,
        cursor,
        %Context{} = context,
        %EventLog{} = log
      ) do
    {:ok, _open_tok, state, cursor} = TokenAdapter.next(state, cursor)

    # Skip leading EOE and count newlines
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    with {:ok, args, state, cursor, log} <-
           ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, cursor, context, log),
         {:ok, close_meta, trailing_newlines, state, cursor} <-
           Meta.consume_closing(state, cursor, :")") do
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      callee_meta = TokenAdapter.token_meta(callee_tok)
      meta = Meta.closing_meta(callee_meta, close_meta, total_newlines)

      ast = {callee, meta, Enum.reverse(args)}
      {:ok, ast, state, cursor, log}
    else
      other -> Result.normalize_error(other, cursor, log)
    end
  end

  @doc """
  Continue parsing with left-hand expression, looking for trailing operators.
  This is exposed for modules like Calls that need to continue parsing after
  building a call expression.
  """
  def led(
        left,
        %State{} = state,
        cursor,
        %EventLog{} = log,
        min_bp,
        %Context{} = context,
        opts \\ []
      ) do
    # Check if there's EOE followed by a continuation operator
    # Only skip EOE when it precedes an operator that can continue the expression
    # This preserves EOE as separators for expr_list (e.g., "1;2" should not skip the ";")
    {state, cursor, sep_tokens, newlines_before_op} = peek_past_sep(state, cursor)

    case TokenAdapter.peek(state, cursor) do
      {:ok, next_token, _state, _cursor} ->
        led_dispatch(
          left,
          next_token,
          state,
          cursor,
          log,
          min_bp,
          context,
          opts,
          sep_tokens,
          newlines_before_op
        )

      {:eof, state, cursor} ->
        # At EOF, push back EOE tokens so they're available for expr_list
        {state, cursor} = pushback_sep_tokens(state, cursor, sep_tokens)
        {:ok, left, state, cursor, log}

      {:error, diag, state, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  defp led_dispatch(
         left,
         {next_token_kind, _meta, _value} = _next_token,
         state,
         cursor,
         log,
         min_bp,
         context,
         opts,
         sep_tokens,
         newlines_before_op
       ) do
    precedence = Precedence.binary(next_token_kind)

    case {next_token_kind, precedence} do
      # Parens call: identifier(args) or expr()(args) (nested call)
      # Rule: parens_call -> dot_call_identifier call_args_parens call_args_parens
      # NOTE: Only treat as call if there's NO EOE between left and (
      # `foo()` is a call, but `foo\n()` is not (the newline separates them)
      # Also, for bare identifiers like `if (a)`, only treat as call if
      # the identifier came from a :paren_identifier token (no space before `(`)
      {:"(", _} when sep_tokens == [] ->
        led_call(left, state, cursor, log, min_bp, context, opts)

      # dot_call_op: expr.(args) - anonymous function call
      # Must check bp >= min_bp to respect operator precedence
      {:dot_call_op, {bp, _}} when bp >= min_bp ->
        led_dot_call(left, state, cursor, log, min_bp, context, opts)

      {:., {bp, _}} when bp >= min_bp ->
        led_dot(left, state, cursor, log, min_bp, context, opts)

      # Bracket access: expr[key]
      # Bracket access has precedence 310 (same as .)
      # NOTE: Only treat as bracket access if:
      # 1. Bracket precedence (310) >= min_bp
      # 2. NO EOE between left and [ (`foo[x]` is bracket access, `foo\n[x]` is not)
      # 3. allows_bracket flag is true (for dot members: `foo.bar[x]` yes, `foo.bar [x]` no)
      {:"[", _} when 310 >= min_bp and sep_tokens == [] ->
        led_bracket(left, state, cursor, log, min_bp, context, opts)

      # Special case: ternary_op :"//" following range_op :".."
      # Combines into {:..//, meta, [start, stop, step]}
      # BUT: If there's EOE between range and //, don't combine - // starts a new expression
      # e.g., "x..0;//y" should be two expressions: x..0 and //y (not x..0//y)
      {:ternary_op, {bp, _assoc}} when bp >= min_bp and sep_tokens == [] ->
        parse_ternary_op(left, state, cursor, min_bp, context, log)

      # ternary_op after EOE is NOT a ternary continuation - it's unary // starting a new expr
      {:ternary_op, {bp, _assoc}} when bp >= min_bp and sep_tokens != [] ->
        {state, cursor} = pushback_sep_tokens(state, cursor, sep_tokens)
        {:ok, left, state, cursor, log}

      # dual_op (+/-) after EOE is NOT a binary operator - it starts a new expression
      # e.g., "x()\n\n-1" should be two expressions: x() and -1 (unary minus)
      # This matches Elixir's behavior where dual_op after newlines is unary
      {:dual_op, {bp, _assoc}} when bp >= min_bp and sep_tokens != [] ->
        # Push back EOE tokens and return left - dual_op after EOE is not continuation
        {state, cursor} = pushback_sep_tokens(state, cursor, sep_tokens)
        {:ok, left, state, cursor, log}

      # range_op (..) or ellipsis_op (...) after separator:
      # - After semicolon, it starts a new expression (e.g. "t;..<e").
      # - After newline, it can continue the previous expression (e.g. "a\n..b").
      {kind, {bp, assoc}}
      when kind in [:range_op, :ellipsis_op] and bp >= min_bp and sep_tokens != [] ->
        # Check if any separator token was a semicolon
        if Enum.any?(sep_tokens, fn {kind, _meta, _value} -> kind == :";" end) do
          {state, cursor} = pushback_sep_tokens(state, cursor, sep_tokens)
          {:ok, left, state, cursor, log}
        else
          led_binary(left, state, cursor, log, min_bp, context, opts, bp, assoc)
        end

      # stab_op (->) is NOT a general binary operator - it only appears in stab clause contexts
      # Parsing it here would wrongly parse "1 -> 2" as a binary expression
      # Stab clauses are handled by the Stabs module which knows when -> is valid
      {:stab_op, _} ->
        {state, cursor} = pushback_sep_tokens(state, cursor, sep_tokens)
        {:ok, left, state, cursor, log}

      # assoc_op (=>) is map-only in elixir_parser.yrl (assoc_expr / map grammar),
      # so it must NOT behave like a general binary operator in Pratt parsing.
      {:assoc_op, {_bp, _assoc}} ->
        {state, cursor} = pushback_sep_tokens(state, cursor, sep_tokens)
        {:ok, left, state, cursor, log}

      {_, {bp, assoc}} when bp >= min_bp ->
        led_binary(left, state, cursor, log, min_bp, context, opts, bp, assoc, newlines_before_op)

      # no_parens_expr extension: when context allows it, operators in @no_parens_op_expr_operators
      # can extend matched_expr to no_parens_expr REGARDLESS of min_bp.
      # This implements the grammar rule: no_parens_expr -> matched_expr no_parens_op_expr
      # Example: @spec foo() when a: term - the `when` (bp=50) can extend `spec foo()`
      # even though @ has bp=320, because the operand context allows no_parens extension.
      {kind, {bp, assoc}} when is_tuple({bp, assoc}) ->
        allow_extension? = Keyword.get(opts, :allow_no_parens_extension?, true)

        if allow_extension? and min_bp == 0 and is_no_parens_op_expr?(kind) and
             Context.allow_no_parens_expr?(context) do
          led_binary(
            left,
            state,
            cursor,
            log,
            min_bp,
            context,
            opts,
            bp,
            assoc,
            newlines_before_op
          )
        else
          # No continuation operator found - push back EOE tokens
          {state, cursor} = pushback_sep_tokens(state, cursor, sep_tokens)
          {:ok, left, state, cursor, log}
        end

      _ ->
        # No continuation operator found - push back EOE tokens
        {state, cursor} = pushback_sep_tokens(state, cursor, sep_tokens)
        {:ok, left, state, cursor, log}
    end
  end

  defp led_call(left, state, cursor, log, min_bp, context, opts) do
    if is_paren_call_valid(left) do
      {:ok, _open_tok, state, cursor} = TokenAdapter.next(state, cursor)

      # Skip leading EOE and count newlines
      {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

      with {:ok, args, state, cursor, log} <-
             ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, cursor, context, log),
           {:ok, close_meta, trailing_newlines, state, cursor} <-
             Meta.consume_closing(state, cursor, :")") do
        total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
        # Get line/column from left AST (the callee)
        callee_meta = extract_meta(left)
        call_meta = Meta.closing_meta(callee_meta, close_meta, total_newlines)

        # If left is a simple identifier {name, meta, nil}, convert to call {name, call_meta, args}
        # Otherwise it's a nested call: {left, call_meta, args}
        combined =
          case left do
            {name, _meta, nil} when is_atom(name) ->
              {name, call_meta, Enum.reverse(args)}

            _ ->
              {left, call_meta, Enum.reverse(args)}
          end

        # Check for nested calls and do-blocks (foo() do...end, foo()() do...end)
        maybe_nested_call_or_do_block(combined, state, cursor, log, min_bp, context, opts)
      else
        other -> Result.normalize_error(other, cursor, log)
      end
    else
      # Not a valid paren call (e.g., `if (a)` with space - `if` is :identifier, not :paren_identifier)
      # Return left as-is, the `(` will be handled as a separate parenthesized expression
      {:ok, left, state, cursor, log}
    end
  end

  defp led_dot_call(left, state, cursor, log, min_bp, context, opts) do
    with {:ok, combined, state, cursor, log} <-
           Dots.parse_dot_call(left, state, cursor, context, log) do
      # Check for nested calls (foo.()()) and do-blocks (foo.() do ... end)
      maybe_nested_call_or_do_block(combined, state, cursor, log, min_bp, context, opts)
    end
  end

  defp led_dot(left, state, cursor, log, min_bp, context, opts) do
    {:ok, dot_tok, state, cursor} = TokenAdapter.next(state, cursor)
    dot_meta = TokenAdapter.token_meta(dot_tok)

    # Skip EOE after dot (allows newlines between dot and member: foo.\n bar)
    {state, cursor, _newlines} = EOE.skip_count_newlines(state, cursor, 0)

    # Check for dot_container: expr.{...}
    case TokenAdapter.peek(state, cursor) do
      {:ok, tok, _state, _cursor} when is_kind(tok, :"{") ->
        {:ok, _open, state, cursor} = TokenAdapter.next(state, cursor)

        with {:ok, args, newlines, close_meta, state, cursor, log} <-
               parse_dot_container_args(state, cursor, context, log) do
          combined =
            {{:., dot_meta, [left, :{}]}, Meta.closing_meta(dot_meta, close_meta, newlines), args}

          led(combined, state, cursor, log, min_bp, context, opts)
        end

      _ ->
        # Pass dot_meta for curly calls where the call metadata uses the dot's position
        with {:ok, rhs, state, cursor, log} <-
               Dots.parse_member(state, cursor, context, log, dot_meta) do
          # Check if rhs indicates a no-parens call (from op_identifier/dot_op_identifier)
          # or bracket access (from bracket_identifier)
          combined_result =
            case rhs do
              # Simple identifier with :no_parens_call flag: {member_atom, member_meta, :no_parens_call}
              # This indicates a no-parens call is expected (from op_identifier/dot_op_identifier)
              {member, member_meta, :no_parens_call} when is_atom(member) ->
                {:ok,
                 {{{:., dot_meta, [left, member]}, [no_parens: true] ++ member_meta, []}, true,
                  false}}

              # Bracket identifier with :allows_bracket flag: {member_atom, member_meta, :allows_bracket}
              # This indicates bracket access is allowed (no whitespace before [)
              {member, member_meta, :allows_bracket} when is_atom(member) ->
                {:ok,
                 {{{:., dot_meta, [left, member]}, [no_parens: true] ++ member_meta, []}, false,
                  true}}

              # Simple identifier: {member_atom, member_meta}
              # Build: {{:., dot_meta, [left, member]}, [no_parens: true | member_meta], []}
              # Bracket access NOT allowed (whitespace before [)
              {member, member_meta} when is_atom(member) ->
                {:ok,
                 {{{:., dot_meta, [left, member]}, [no_parens: true] ++ member_meta, []}, false,
                  false}}

              # Alias on RHS with bracket access allowed (no whitespace before [)
              {{:__aliases__, rhs_meta, [rhs_alias]}, :allows_bracket} ->
                case build_dot_alias(left, rhs_alias, rhs_meta, dot_meta) do
                  {:ok, combined} -> {:ok, {combined, false, true}}
                  {:error, reason} -> {:error, reason}
                end

              # Alias on RHS - build combined __aliases__ (dot_alias rule)
              # Must come before the general {name, meta, args} pattern
              {:__aliases__, rhs_meta, [rhs_alias]} ->
                case build_dot_alias(left, rhs_alias, rhs_meta, dot_meta) do
                  {:ok, combined} -> {:ok, {combined, false, false}}
                  {:error, reason} -> {:error, reason}
                end

              # Call with args: {name, meta, args}
              # Bracket access is always allowed after a call result (no whitespace check needed)
              {name, meta, args} when is_list(args) ->
                {:ok, {{{:., dot_meta, [left, name]}, meta, args}, false, true}}

              # Other AST node
              other ->
                {:ok, {{:., dot_meta, [left, other]}, false, false}}
            end

          # Handle error from build_dot_alias (e.g., atom.Alias)
          case combined_result do
            {:ok, {combined, expects_no_parens_call, allows_bracket}} ->
              case TokenAdapter.peek(state, cursor) do
                # When ( follows a dot member that was already a call (allows_bracket = true),
                # it's a nested paren call: foo.bar()()
                # When ( follows a simple identifier (allows_bracket = false), it means there's
                # whitespace before (, so ( starts a no-parens argument: foo.e ()
                {:ok, {:"(", _meta, _value}, _state, _cursor} when not allows_bracket ->
                  with {:ok, args, state, cursor, log} <-
                         Calls.parse_no_parens_args([], state, cursor, context, log) do
                    combined = dot_to_no_parens_call(combined, args)
                    # Check for do-blocks after no-parens call: foo.bar () do...end
                    maybe_nested_call_or_do_block(
                      combined,
                      state,
                      cursor,
                      log,
                      min_bp,
                      context,
                      opts
                    )
                  end

                # Nested paren call: foo.bar()() - rhs was already a call (allows_bracket = true)
                {:ok, tok, _state, _cursor} when is_kind(tok, :"(") ->
                  {:ok, _open, state, cursor} = TokenAdapter.next(state, cursor)
                  # Skip leading EOE and count newlines
                  {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

                  with {:ok, args, state, cursor, log} <-
                         ToxicParser.Grammar.CallsPrivate.parse_paren_args(
                           [],
                           state,
                           cursor,
                           context,
                           log
                         ) do
                    # Skip trailing EOE before close paren
                    {state, cursor, trailing_newlines} = EOE.skip_count_newlines(state, cursor, 0)

                    case TokenAdapter.next(state, cursor) do
                      {:ok, {:")", _meta, _value} = close_tok, state, cursor} ->
                        # Only count trailing newlines for empty args
                        # (matches behavior in maybe_nested_call_or_do_block)
                        total_newlines =
                          if args == [] do
                            leading_newlines + trailing_newlines
                          else
                            leading_newlines
                          end

                        close_meta = TokenAdapter.token_meta(close_tok)

                        # When followed by parens, convert to call form with proper metadata
                        combined =
                          dot_to_call_with_meta(combined, args, total_newlines, close_meta)

                        # Check for nested calls and do-blocks (foo.bar() do...end)
                        maybe_nested_call_or_do_block(
                          combined,
                          state,
                          cursor,
                          log,
                          min_bp,
                          context,
                          opts
                        )

                      {:ok, {got_kind, _meta, _value}, state, cursor} ->
                        {:error, {:expected, :")", got: got_kind}, state, cursor, log}

                      {:eof, state, cursor} ->
                        {:error, :unexpected_eof, state, cursor, log}

                      {:error, diag, state, cursor} ->
                        {:error, diag, state, cursor, log}
                    end
                  end

                {:ok, {:"[", _meta, _value}, _state, _cursor} when allows_bracket ->
                  # Bracket access: foo.bar[:key] - only when bracket access is allowed
                  # (no whitespace before [)
                  led(combined, state, cursor, log, min_bp, context, opts)

                {:ok, tok, _state, _cursor} when is_kind(tok, :do) ->
                  # Do-block after dot call: foo.bar() do...end
                  # Only handle when rhs was a call (combined has args list)
                  maybe_nested_call_or_do_block(
                    combined,
                    state,
                    cursor,
                    log,
                    min_bp,
                    context,
                    opts
                  )

                {:ok, {next_kind, _meta, _value} = next_tok, _state, _cursor} ->
                  # Check for no-parens call argument after dot expression
                  # Only parse as no-parens call if:
                  # 1. The member was tokenized as op_identifier/dot_op_identifier (expects_no_parens_call)
                  # 2. OR next token can start a no-parens arg (excluding dual_op when expects_no_parens_call is false)
                  # 3. OR next token starts a keyword list
                  should_parse_no_parens =
                    expects_no_parens_call or Keywords.starts_kw?(next_tok) or
                      (NoParens.can_start_no_parens_arg?(next_tok) and next_kind != :dual_op)

                  if should_parse_no_parens do
                    with {:ok, args, state, cursor, log} <-
                           Calls.parse_no_parens_args([], state, cursor, context, log) do
                      combined = dot_to_no_parens_call(combined, args)
                      # Check for do-blocks after no-parens call: foo.bar arg do...end
                      maybe_nested_call_or_do_block(
                        combined,
                        state,
                        cursor,
                        log,
                        min_bp,
                        context,
                        opts
                      )
                    end
                  else
                    led(combined, state, cursor, log, min_bp, context, opts)
                  end

                _ ->
                  led(combined, state, cursor, log, min_bp, context, opts)
              end

            {:error, reason} ->
              {:error, reason, state, cursor, log}
          end
        else
          {:error, :unexpected_eof, state, cursor, log} ->
            {:error, syntax_error_before(dot_meta), state, cursor, log}

          {:error, reason, state, cursor, log} ->
            {:error, reason, state, cursor, log}
        end
    end
  end

  defp led_bracket(left, state, cursor, log, min_bp, context, opts) do
    # Check allows_bracket flag (defaults to true for backward compatibility)
    allows_bracket = Keyword.get(opts, :allows_bracket, true)

    if not allows_bracket do
      # Space before [ means this is NOT bracket access, but a no-parens call with list arg
      # Return left as-is (no-parens call parsing will be handled by caller)
      {:ok, left, state, cursor, log}
    else
      {:ok, open_tok, state, cursor} = TokenAdapter.next(state, cursor)

      # Skip leading EOE and count newlines (only leading newlines matter for metadata)
      {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

      with {:ok, indices, state, cursor, log} <-
             parse_access_indices([], state, cursor, context, log) do
        # Skip trailing EOE before close bracket (don't count these)
        {state, cursor, _trailing_newlines} = EOE.skip_count_newlines(state, cursor, 0)

        case TokenAdapter.next(state, cursor) do
          {:ok, {:"]", _meta, _value} = close_tok, state, cursor} ->
            # Build metadata with from_brackets, newlines, closing, line, column
            open_meta = TokenAdapter.token_meta(open_tok)
            close_meta = TokenAdapter.token_meta(close_tok)

            bracket_meta =
              Meta.closing_meta(open_meta, close_meta, leading_newlines, from_brackets: true)

            combined =
              {{:., bracket_meta, [Access, :get]}, bracket_meta, [left | Enum.reverse(indices)]}

            led(combined, state, cursor, log, min_bp, context, opts)

          {:ok, {got_kind, _meta, _value}, state, cursor} ->
            {:error, {:expected, :"]", got: got_kind}, state, cursor, log}

          {:eof, state, cursor} ->
            {:error, :unexpected_eof, state, cursor, log}

          {:error, diag, state, cursor} ->
            {:error, diag, state, cursor, log}
        end
      end
    end
  end

  defp led_binary(
         left,
         state,
         cursor,
         log,
         min_bp,
         context,
         opts,
         bp,
         assoc,
         newlines_before_op \\ 0
       ) do
    {:ok, {op_kind, _meta, op_value} = op_token, state, cursor} = TokenAdapter.next(state, cursor)
    # For right associativity, use same bp to allow chaining at same level
    # For left associativity, use bp+1 to prevent same-level ops from binding to RHS
    rhs_min_bp = if assoc == :right, do: bp, else: bp + 1

    # Skip newlines after operator (allows "1 +\n2")
    # Note: We only skip newlines, not semicolons. "1 +;\n2" should be a syntax error.
    #
    # IMPORTANT: Elixir's parser attaches `newlines` metadata to many operators based on
    # the EOL *after* the operator (see `*_op_eol` + `next_is_eol/2` in elixir_parser.yrl).
    # For pipe_op specifically, we use newlines *before* the operator (for map update syntax).
    {state, cursor, newlines_after_op} = EOE.skip_newlines_only(state, cursor, 0)

    # Toxic encodes some line breaks as explicit :eoe tokens (after the operator)
    # and others as leading newline counts on the operator token itself.
    token_leading_newlines = TokenAdapter.newlines(op_token)

    # For pipe_op, prefer newlines before the operator (for map update syntax)
    # For other operators, use newlines after the operator
    effective_newlines =
      if op_kind == :pipe_op and newlines_before_op > 0 do
        newlines_before_op
      else
        if newlines_after_op > 0, do: newlines_after_op, else: token_leading_newlines
      end

    case TokenAdapter.peek(state, cursor) do
      # Special case: when operator followed by keyword list
      # Grammar rule: no_parens_op_expr -> when_op_eol call_args_no_parens_kw
      # Only valid when no-parens extension is enabled.
      {:ok, _rhs_tok, _state, _cursor}
      when is_kind(op_token, :when_op) and context.allow_no_parens_expr ->
        case Keywords.try_parse_call_args_no_parens_kw(state, cursor, context, log) do
          {:ok, kw_list, state, cursor, log} ->
            op = op_value
            meta = build_meta_with_newlines(TokenAdapter.token_meta(op_token), effective_newlines)
            combined = Builder.Helpers.binary(op, left, kw_list, meta)
            led(combined, state, cursor, log, min_bp, context, opts)

          {:no_kw, state, cursor, log} ->
            # Parse RHS normally - handle keyword_key from quoted keywords
            parse_when_binary_rhs(
              state,
              cursor,
              left,
              op_token,
              rhs_min_bp,
              min_bp,
              effective_newlines,
              context,
              log
            )

          {:error, reason, state, cursor, log} ->
            {:error, reason, state, cursor, log}
        end

      # Special case: when operator in contexts without no-parens extension
      {:ok, _rhs_tok, _state, _cursor} when is_kind(op_token, :when_op) ->
        parse_when_binary_rhs(
          state,
          cursor,
          left,
          op_token,
          rhs_min_bp,
          min_bp,
          effective_newlines,
          context,
          log
        )

      # Special case: assoc_op (=>) in map context needs to parse full expression as value
      # Grammar rule: assoc_expr -> container_expr assoc_op_eol container_expr
      # The value of => should be a container_expr which includes pipe_op
      # So use min_bp=0 to allow | and other low-precedence operators in the value
      {:ok, _rhs_tok, _state, _cursor} when is_kind(op_token, :assoc_op) ->
        parse_binary_rhs(
          state,
          cursor,
          left,
          op_token,
          # min_bp=0 to allow full expression as value
          0,
          min_bp,
          effective_newlines,
          context,
          log,
          opts
        )

      _ ->
        parse_binary_rhs(
          state,
          cursor,
          left,
          op_token,
          rhs_min_bp,
          min_bp,
          effective_newlines,
          context,
          log,
          opts
        )
    end
  end

  # Handle nested paren calls (foo.()(), foo()()) and do-blocks (foo.() do...end, foo() do...end)
  # Grammar rules:
  #   parens_call -> dot_call_identifier call_args_parens call_args_parens
  #   block_expr -> dot_call_identifier call_args_parens do_block
  #   block_expr -> dot_call_identifier call_args_parens call_args_parens do_block
  # When unary_operand: true, this is parsing a unary operator's operand.
  # In that context, do-blocks should allow ALL binary ops to attach (min_bp=0).
  # Otherwise (binary RHS), preserve min_bp for proper associativity.
  defp maybe_nested_call_or_do_block(ast, state, cursor, log, min_bp, context, opts) do
    unary_operand = Keyword.get(opts, :unary_operand, false)

    case TokenAdapter.peek(state, cursor) do
      {:ok, tok, _state, _cursor} when is_kind(tok, :"(") ->
        # Another paren call - parse it
        {:ok, _open_tok, state, cursor} = TokenAdapter.next(state, cursor)

        # Skip leading EOE and count newlines
        {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

        with {:ok, args, state, cursor, log} <-
               ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, cursor, context, log),
             {:ok, close_meta, trailing_newlines, state, cursor} <-
               Meta.consume_closing(state, cursor, :")") do
          total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
          # Get line/column from callee AST
          callee_meta = extract_meta(ast)
          meta = Meta.closing_meta(callee_meta, close_meta, total_newlines)

          combined = {ast, meta, Enum.reverse(args)}
          # Recurse for chained calls like foo()()()
          maybe_nested_call_or_do_block(combined, state, cursor, log, min_bp, context, opts)
        else
          other -> Result.normalize_error(other, cursor, log)
        end

      {:ok, tok, _state, _cursor} when is_kind(tok, :do) ->
        # When allow_do_block is false (e.g. parsing no-parens call arguments), do-blocks
        # belong to the outer call (e.g. `foo bar do ... end`) and must NOT attach to
        # the final argument expression (e.g. `bar do ... end`).
        if not Context.allow_do_block?(context) do
          led(ast, state, cursor, log, min_bp, context)
        else
          # Do-block - parse and attach to call
          # Attach do-block if the AST is a call form (has args list).
          # Do NOT attach to:
          # - Bare identifiers like `(b)` in `if (a) or (b) do` - these have `:parens` metadata
          #   but args=nil, not a call form
          # - Variables/atoms that aren't calls
          #
          # Per Elixir's grammar, block_expr (call + do-block) can appear as operand
          # in unmatched_expr, so min_bp doesn't affect do-block attachment.
          should_attach =
            case ast do
              # Call with args list (including empty args [])
              {_name, meta, args} when is_list(args) ->
                # Don't attach to parenthesized expressions like `(x)` which have
                # :parens metadata.
                not Keyword.has_key?(meta, :parens)

              # Bare identifier, variable, literal - not a call
              _ ->
                false
            end

          if should_attach do
            with {:ok, {block_meta, sections}, state, cursor, log} <-
                   Blocks.parse_do_block(state, cursor, context, log) do
              combined =
                case ast do
                  {name, meta, args} when is_list(args) ->
                    # Prepend do/end metadata to the call's existing metadata
                    # Remove no_parens: true as it doesn't apply with do-blocks
                    clean_meta = Keyword.delete(meta, :no_parens)
                    {name, block_meta ++ clean_meta, args ++ [sections]}

                  # Bare identifier: {name, meta, nil} - convert to call with do-block
                  {name, meta, nil} when is_atom(name) ->
                    {name, block_meta ++ meta, [sections]}

                  other ->
                    Builder.Helpers.call(other, [sections], block_meta)
                end

              # After attaching do-block, decide min_bp based on context:
              # - For unary operands: use min_bp=0 so trailing operators are consumed.
              #   E.g., `+ a.b do x end ** c` -> `+ (a.b do x end ** c)`
              #   BUT: stop at assoc_op (=>) so map key-value pairs work correctly.
              #   E.g., `%{A | + B do x end => C}` should have `+B do x end` as key, not `+(B do x end => C)`
              # - For binary RHS: preserve min_bp for proper associativity.
              #   E.g., `A ** B do x end ** C` -> `(A ** B do x end) ** C` (left-assoc)
              {led_min_bp, led_opts} =
                if unary_operand do
                  {0, [stop_at_assoc: true]}
                else
                  {min_bp, []}
                end

              led(combined, state, cursor, log, led_min_bp, context, led_opts)
            end
          else
            led(ast, state, cursor, log, min_bp, context)
          end
        end

      _ ->
        # No nested call or do-block - continue with led
        led(ast, state, cursor, log, min_bp, context)
    end
  end

  # Convert a dot expression to a call when followed by parens, with closing metadata
  # If id_meta already has :closing, this is a nested call - wrap the whole thing
  defp dot_to_call_with_meta(
         {{:., dot_meta, dot_args}, id_meta, _inner_args} = callee,
         args,
         newlines,
         close_meta
       ) do
    if Keyword.has_key?(id_meta, :closing) do
      # Already a call - wrap it as nested call
      callee_meta = Keyword.take(id_meta, [:line, :column])
      call_meta = Meta.closing_meta(callee_meta, close_meta, newlines)
      {callee, call_meta, Enum.reverse(args)}
    else
      # First call on dot expression - convert to call form
      base_meta = Keyword.delete(id_meta, :no_parens)
      call_meta = Meta.closing_meta(base_meta, close_meta, newlines)
      {{:., dot_meta, dot_args}, call_meta, Enum.reverse(args)}
    end
  end

  defp dot_to_call_with_meta({:., dot_meta, dot_args}, args, newlines, close_meta) do
    call_meta = Meta.closing_meta(dot_meta, close_meta, newlines)
    {{:., dot_meta, dot_args}, call_meta, Enum.reverse(args)}
  end

  defp dot_to_call_with_meta(other, args, newlines, close_meta) do
    meta = Meta.closing_meta([], close_meta, newlines)
    {other, meta, Enum.reverse(args)}
  end

  # Convert a dot expression to a no-parens call
  # {{:., dot_meta, [left, member]}, [no_parens: true | id_meta], []} + args
  # -> {{:., dot_meta, [left, member]}, id_meta, args}
  defp dot_to_no_parens_call({{:., dot_meta, dot_args}, id_meta, []}, args) do
    # Remove no_parens: true for the call form
    call_meta = Keyword.delete(id_meta, :no_parens)
    {{:., dot_meta, dot_args}, call_meta, args}
  end

  defp dot_to_no_parens_call({:., dot_meta, dot_args}, args) do
    {{:., dot_meta, dot_args}, [], args}
  end

  defp dot_to_no_parens_call(other, args) do
    Builder.Helpers.call(other, args)
  end

  # Build combined __aliases__ for dot_alias rule (Foo.Bar -> {:__aliases__, meta, [:Foo, :Bar]})
  # When left is already an __aliases__, append the new segment
  defp build_dot_alias({:__aliases__, left_meta, left_segments}, rhs_alias, rhs_meta, _dot_meta) do
    # Extract just the line/column from rhs_meta's :last value (or from rhs_meta itself)
    last_meta = Keyword.get(rhs_meta, :last, rhs_meta)
    new_meta = Keyword.put(left_meta, :last, last_meta)
    {:ok, {:__aliases__, new_meta, left_segments ++ [rhs_alias]}}
  end

  # Error case: literal atom followed by alias
  # From elixir_parser.yrl: build_dot_alias(_Dot, Atom, Right) when is_atom(Atom) -> error_bad_atom(Right)
  defp build_dot_alias(left_expr, _rhs_alias, _rhs_meta, dot_meta)
       when is_atom(left_expr) do
    {:error, {:atom_followed_by_alias, dot_meta}}
  end

  # When left is some other expression, wrap both in __aliases__
  # Meta is [last: alias_location] ++ dot_location
  defp build_dot_alias(left_expr, rhs_alias, rhs_meta, dot_meta) do
    last_meta = Keyword.get(rhs_meta, :last, rhs_meta)
    {:ok, {:__aliases__, [last: last_meta] ++ dot_meta, [left_expr, rhs_alias]}}
  end

  # Parse container args for dot_container: expr.{...}
  # Grammar (elixir_parser.yrl):
  #   dot_alias -> matched_expr dot_op open_curly '}'.
  #   dot_alias -> matched_expr dot_op open_curly container_args close_curly.
  #
  # Returns {:ok, args, leading_newlines, close_meta, state, cursor, log}.
  defp parse_dot_container_args(state, cursor, _ctx, log) do
    # Skip leading EOE and count newlines (only leading newlines matter for metadata).
    {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

    container_ctx = Context.container_expr()

    # `container_args` does NOT allow starting with kw_data (including quoted keys).
    # Ensure we preserve the legacy behavior for `a: 1` (kw_identifier), and fix
    # the quoted-key case where `'a': 1` previously parsed as a keyword list.
    with {:ok, state, cursor, log} <- reject_initial_kw_data(state, cursor, container_ctx, log) do
      item_fun = fn state, cursor, _ctx, log ->
        case Keywords.try_parse_kw_data(state, cursor, container_ctx, log) do
          {:ok, kw_list, state, cursor, log} ->
            {state, cursor, _newlines} = EOE.skip_count_newlines(state, cursor, 0)

            case TokenAdapter.peek(state, cursor) do
              {:ok, tok, _state, _cursor} when is_kind(tok, :"}") ->
                {:ok, {:kw_data, kw_list}, state, cursor, log}

              {:ok, {got_kind, _meta, _value}, state, cursor} ->
                {:error, {:expected, :"}", got: got_kind}, state, cursor, log}

              {:eof, state, cursor} ->
                {:error, :unexpected_eof, state, cursor, log}

              {:error, diag, state, cursor} ->
                {:error, diag, state, cursor, log}
            end

          {:no_kw, state, cursor, log} ->
            with {:ok, expr, state, cursor, log} <-
                   Expressions.expr(state, cursor, container_ctx, log) do
              {:ok, {:expr, expr}, state, cursor, log}
            end

          {:error, reason, state, cursor, log} ->
            {:error, reason, state, cursor, log}
        end
      end

      with {:ok, tagged_items, state, cursor, log} <-
             Delimited.parse_comma_separated(state, cursor, container_ctx, log, :"}", item_fun,
               allow_empty?: true
             ) do
        # Skip trailing EOE before close curly (don't count these).
        {state, cursor, _trailing_newlines} = EOE.skip_count_newlines(state, cursor, 0)

        case TokenAdapter.next(state, cursor) do
          {:ok, {:"}", _meta, _value} = close_tok, state, cursor} ->
            close_meta = TokenAdapter.token_meta(close_tok)

            {:ok, finalize_dot_container_items(tagged_items), leading_newlines, close_meta, state,
             cursor, log}

          {:ok, {got_kind, _meta, _value}, state, cursor} ->
            {:error, {:expected, :"}", got: got_kind}, state, cursor, log}

          {:eof, state, cursor} ->
            {:error, :unexpected_eof, state, cursor, log}

          {:error, diag, state, cursor} ->
            {:error, diag, state, cursor, log}
        end
      end
    end
  end

  defp reject_initial_kw_data(state, cursor, container_ctx, log) do
    case TokenAdapter.peek(state, cursor) do
      {:ok, tok, _state, _cursor} when is_kind(tok, :"}") ->
        {:ok, state, cursor, log}

      {:ok, tok, _state, _cursor} ->
        {ref, checkpoint_state} = TokenAdapter.checkpoint(state, cursor)

        case Keywords.try_parse_kw_data(checkpoint_state, cursor, container_ctx, log) do
          {:ok, kw_list, state, cursor, log} ->
            {state, cursor} = TokenAdapter.rewind(state, cursor, ref)
            meta = TokenAdapter.token_meta(tok)
            token_display = kw_data_first_key_display(kw_list)
            {:error, syntax_error_before(meta, token_display), state, cursor, log}

          {:no_kw, state, cursor, log} ->
            {state, cursor} = TokenAdapter.rewind(state, cursor, ref)
            {:ok, state, cursor, log}

          {:error, reason, state, cursor, log} ->
            {state, cursor} = TokenAdapter.rewind(state, cursor, ref)
            {:error, reason, state, cursor, log}
        end

      {:eof, state, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, state, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  defp kw_data_first_key_display([{key, _} | _]) when is_atom(key), do: Atom.to_string(key)
  defp kw_data_first_key_display([{key, _} | _]), do: Macro.to_string(key)
  defp kw_data_first_key_display(_), do: ""

  defp finalize_dot_container_items([]), do: []

  defp finalize_dot_container_items(tagged_items) do
    case List.last(tagged_items) do
      {:kw_data, kw_list} ->
        exprs =
          tagged_items
          |> Enum.drop(-1)
          |> Enum.map(fn {:expr, expr} -> expr end)

        exprs ++ [kw_list]

      _ ->
        Enum.map(tagged_items, fn {:expr, expr} -> expr end)
    end
  end

  # Peek past separator tokens without consuming them permanently
  # Returns {state_after_sep, cursor_after_sep, sep_tokens_list, newlines_count}
  # The sep_tokens_list can be used to push back if we don't find a continuation operator
  defp peek_past_sep(state, cursor, sep_tokens \\ [], newlines \\ 0) do
    case TokenAdapter.peek(state, cursor) do
      {:ok, {:eol, {_, _, n}, _value} = sep_tok, _state, _cursor} when is_integer(n) ->
        {:ok, _sep, state, cursor} = TokenAdapter.next(state, cursor)
        peek_past_sep(state, cursor, [sep_tok | sep_tokens], newlines + n)

      {:ok, {:eol, _meta, _value} = sep_tok, _state, _cursor} ->
        {:ok, _sep, state, cursor} = TokenAdapter.next(state, cursor)
        peek_past_sep(state, cursor, [sep_tok | sep_tokens], newlines)

      {:ok, {:";", {_, _, n}, _value} = sep_tok, _state, _cursor} when is_integer(n) ->
        {:ok, _sep, state, cursor} = TokenAdapter.next(state, cursor)
        peek_past_sep(state, cursor, [sep_tok | sep_tokens], newlines + n)

      {:ok, {:";", _meta, _value} = sep_tok, _state, _cursor} ->
        {:ok, _sep, state, cursor} = TokenAdapter.next(state, cursor)
        peek_past_sep(state, cursor, [sep_tok | sep_tokens], newlines)

      _ ->
        {state, cursor, Enum.reverse(sep_tokens), newlines}
    end
  end

  # Push back a list of separator tokens so the first token read is first in lookahead.
  # Since pushback adds to the FRONT of lookahead, we need to push in reverse order:
  # For [newline, semicolon], push semicolon first, then newline, so lookahead is [newline, semicolon].
  defp pushback_sep_tokens(state, cursor, []), do: {state, cursor}

  defp pushback_sep_tokens(state, cursor, sep_tokens) do
    Enum.reduce(Enum.reverse(sep_tokens), {state, cursor}, fn tok, {acc_state, acc_cursor} ->
      TokenAdapter.pushback(acc_state, acc_cursor, tok)
    end)
  end

  # Helper to parse RHS of `when` binary operator
  # Handles both regular RHS and keyword_key returns from quoted keywords
  defp parse_when_binary_rhs(
         state,
         cursor,
         left,
         op_token,
         rhs_min_bp,
         min_bp,
         newlines,
         context,
         log
       ) do
    # Follow elixir_parser.yrl context-dependent RHS:
    # - matched_op_expr -> when_op_eol matched_expr
    # - unmatched_op_expr -> when_op_eol unmatched_expr
    # - no_parens_op_expr -> when_op_eol no_parens_expr
    rhs_context =
      cond do
        # expr union: allow both do-blocks and no-parens extension
        context.allow_do_block and context.allow_no_parens_expr ->
          Context.expr()

        # unmatched_expr: allow do-blocks, forbid no-parens extension
        context.allow_do_block ->
          Context.unmatched_expr()

        # no_parens_expr: allow extension, forbid do-blocks
        context.allow_no_parens_expr ->
          Context.no_parens_expr()

        # matched_expr
        true ->
          Context.matched_expr()
      end

    case TokenAdapter.next(state, cursor) do
      {:ok, rhs_token, state, cursor} ->
        if Keywords.starts_kw?(rhs_token) do
          {state, cursor} = TokenAdapter.pushback(state, cursor, rhs_token)

          with {:ok, kw_list, state, cursor, log} <-
                 Keywords.parse_kw_no_parens_call(state, cursor, rhs_context, log) do
            combined = build_binary_op(op_token, left, kw_list, newlines)
            led(combined, state, cursor, log, min_bp, context)
          end
        else
          case parse_rhs(rhs_token, state, cursor, rhs_context, log, rhs_min_bp) do
            {:ok, right, state, cursor, log} ->
              combined = build_binary_op(op_token, left, right, newlines)
              # Continuation uses original context
              led(combined, state, cursor, log, min_bp, context)

            # String was a quoted keyword key - parse value and handle as keyword list RHS
            {:keyword_key, key_atom, key_meta, state, cursor, log} ->
              {state, cursor} = EOE.skip(state, cursor)

              with {:ok, value_ast, state, cursor, log} <-
                     parse_with_min_bp(state, cursor, rhs_context, log, rhs_min_bp) do
                key_ast = Builder.Helpers.literal(key_atom, key_meta, state)
                kw_list = [{key_ast, value_ast}]
                # Check for more keywords after comma
                handle_when_keyword_continuation(
                  kw_list,
                  left,
                  op_token,
                  min_bp,
                  newlines,
                  state,
                  cursor,
                  context,
                  log
                )
              end

            {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, cursor, log} ->
              {state, cursor} = EOE.skip(state, cursor)

              with {:ok, value_ast, state, cursor, log} <-
                     parse_with_min_bp(state, cursor, rhs_context, log, rhs_min_bp) do
                key_ast =
                  Expressions.build_interpolated_keyword_key(parts, kind, start_meta, delimiter)

                kw_list = [{key_ast, value_ast}]

                handle_when_keyword_continuation(
                  kw_list,
                  left,
                  op_token,
                  min_bp,
                  newlines,
                  state,
                  cursor,
                  context,
                  log
                )
              end

            {:error, reason, state, cursor, log} ->
              {:error, reason, state, cursor, log}
          end
        end

      {:eof, state, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, state, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  # Handle keyword continuation for `when` RHS - check for more keywords after comma
  defp handle_when_keyword_continuation(
         kw_list,
         left,
         op_token,
         min_bp,
         newlines,
         state,
         cursor,
         context,
         log
       ) do
    case TokenAdapter.peek(state, cursor) do
      {:ok, tok, _state, _cursor} when is_kind(tok, :",") ->
        {:ok, _comma, state, cursor} = TokenAdapter.next(state, cursor)
        {state, cursor} = EOE.skip(state, cursor)

        case TokenAdapter.peek(state, cursor) do
          {:ok, {kind, _meta, _value} = tok, _state, _cursor} ->
            cond do
              Keywords.starts_kw?(tok) ->
                with {:ok, more_kw, state, cursor, log} <-
                       Keywords.parse_kw_no_parens_call(state, cursor, context, log) do
                  combined = build_binary_op(op_token, left, kw_list ++ more_kw, newlines)
                  led(combined, state, cursor, log, min_bp, context)
                end

              kind in [:list_string_start, :bin_string_start] ->
                # Potentially another quoted keyword - parse via expression
                with {:ok, expr, state, cursor, log} <-
                       Expressions.expr(state, cursor, context, log) do
                  if is_keyword_list_result(expr) do
                    # It was a keyword list - merge and continue
                    handle_when_keyword_continuation(
                      kw_list ++ expr,
                      left,
                      op_token,
                      min_bp,
                      newlines,
                      state,
                      cursor,
                      context,
                      log
                    )
                  else
                    # Not a keyword - finalize current kw_list
                    combined = build_binary_op(op_token, left, kw_list, newlines)
                    led(combined, state, cursor, log, min_bp, context)
                  end
                end

              true ->
                # Not a keyword - finalize
                combined = build_binary_op(op_token, left, kw_list, newlines)
                led(combined, state, cursor, log, min_bp, context)
            end

          _ ->
            combined = build_binary_op(op_token, left, kw_list, newlines)
            led(combined, state, cursor, log, min_bp, context)
        end

      _ ->
        combined = build_binary_op(op_token, left, kw_list, newlines)
        led(combined, state, cursor, log, min_bp, context)
    end
  end

  # Helper to parse RHS of binary operator
  defp parse_binary_rhs(
         state,
         cursor,
         left,
         op_token,
         rhs_min_bp,
         min_bp,
         newlines,
         context,
         log,
         opts
       ) do
    # Follow elixir_parser.yrl: RHS can be a no_parens_expr depending on the operator class.
    # In Pratt, that means we must preserve allow_no_parens_expr on RHS when the surrounding
    # context allows it (e.g. `<-` RHS can contain `when foo: bar` keyword args).
    rhs_context = context

    case TokenAdapter.next(state, cursor) do
      {:ok, rhs_token, state, cursor} ->
        rhs_opts =
          opts
          |> Keyword.put(:allow_no_parens_extension?, context.allow_no_parens_expr)
          |> Keyword.put(:unary_operand, false)

        with {:ok, right, state, cursor, log} <-
               parse_rhs(rhs_token, state, cursor, rhs_context, log, rhs_min_bp, rhs_opts) do
          combined = build_binary_op(op_token, left, right, newlines)
          # Continuation uses original context to allow further extension at outer level
          led(combined, state, cursor, log, min_bp, context, opts)
        end

      {:eof, state, cursor} ->
        {:error, :unexpected_eof, state, cursor, log}

      {:error, diag, state, cursor} ->
        {:error, diag, state, cursor, log}
    end
  end

  # Restrict context to prevent no_parens_expr extension in nested operands
  defp restrict_no_parens_extension(%Context{} = ctx), do: %{ctx | allow_no_parens_expr: false}

  defp has_parens_meta?({_, meta, _}) when is_list(meta), do: Keyword.has_key?(meta, :parens)
  defp has_parens_meta?(_), do: false

  # Build binary operation AST, with special handling for "not in" rewrite
  # "not in" gets rewritten to {:not, NotMeta, [{:in, InMeta, [left, right]}]}
  # Token format: {:in_op, meta, {:"not in", in_location}}
  defp build_binary_op(
         {:in_op, _meta, {:"not in", in_location}} = op_token,
         left,
         right,
         newlines
       ) do
    not_meta = build_meta_with_newlines(TokenAdapter.token_meta(op_token), newlines)
    in_meta = build_meta_from_location(in_location)
    {:not, not_meta, [{:in, in_meta, [left, right]}]}
  end

  # Deprecated "not expr1 in expr2" rewrite - when in_op follows {:not, _, [operand]} or {:!, _, [operand]}
  # Rewrites to {:not, InMeta, [{:in, InMeta, [operand, right]}]} or {:!, InMeta, [{:in, InMeta, [operand, right]}]}
  # Token format: {:in_op, meta, :in}
  defp build_binary_op(
         {:in_op, _meta, :in} = op_token,
         {op, _op_meta, [operand]},
         right,
         _newlines
       )
       when op in [:not, :!] do
    in_meta = TokenAdapter.token_meta(op_token)
    {op, in_meta, [{:in, in_meta, [operand, right]}]}
  end

  # Range ternary rewrite: a .. (b // c) -> a ..// b // c
  # Token format: {:range_op, meta, :..}
  defp build_binary_op(
         {:range_op, _meta, _op} = op_token,
         left,
         {:"//", _meta2, [stop, step]},
         newlines
       ) do
    range_meta = build_meta_with_newlines(TokenAdapter.token_meta(op_token), newlines)
    {:..//, range_meta, [left, stop, step]}
  end

  # Assoc operator (=>) - just build as normal binary op
  # Note: Elixir doesn't add any special metadata for => operator
  # Token format: {:assoc_op, meta, :"=>"}
  defp build_binary_op({:assoc_op, _meta, _op} = op_token, left, right, newlines) do
    op_meta = build_meta_with_newlines(TokenAdapter.token_meta(op_token), newlines)
    Builder.Helpers.binary(:"=>", left, right, op_meta)
  end

  defp build_binary_op({_kind, _meta, op} = op_token, left, right, newlines) do
    meta = build_meta_with_newlines(TokenAdapter.token_meta(op_token), newlines)
    Builder.Helpers.binary(op, left, right, meta)
  end

  # Build metadata from a raw Toxic location tuple
  defp build_meta_from_location({{line, column}, _, _}) do
    [line: line, column: column]
  end

  defp build_meta_from_location(_), do: []

  # Encode a literal value using the literal_encoder if provided
  # The encoder receives (literal, metadata) and returns {:ok, encoded} or {:error, reason}
  @doc false
  def encode_literal(value, meta, state) do
    case Keyword.get(state.opts, :literal_encoder) do
      nil ->
        value

      encoder when is_function(encoder, 2) ->
        case encoder.(value, meta) do
          {:ok, encoded} -> encoded
          # For error case, we just return the value unchanged
          # The error should be handled by the caller in strict mode
          {:error, _reason} -> value
        end
    end
  end

  # Integer: extract parsed value from raw token metadata
  # Token format: {:int, {{line, col}, {end_line, end_col}, parsed_value}, raw_string}
  defp literal_to_ast({:int, {_, _, parsed_value}, raw_string} = token, state) do
    # Add token metadata for literal_encoder (original string representation)
    token_meta = [{:token, to_string(raw_string)} | Builder.Helpers.token_meta(token)]
    encode_literal(parsed_value, token_meta, state)
  end

  # Float: extract parsed value from raw token metadata
  # Token format: {:flt, {{line, col}, {end_line, end_col}, parsed_value}, raw_string}
  defp literal_to_ast({:flt, {_, _, parsed_value}, raw_string} = token, state) do
    # Add token metadata for literal_encoder (original string representation)
    token_meta = [{:token, to_string(raw_string)} | Builder.Helpers.token_meta(token)]
    encode_literal(parsed_value, token_meta, state)
  end

  # Character literal: value is the codepoint
  # Token format: {:char, {{line, col}, {end_line, end_col}, raw_charlist}, codepoint}
  defp literal_to_ast({:char, {_, _, raw_charlist}, codepoint} = token, state) do
    # Add token metadata for literal_encoder (original string representation)
    token_meta = [{:token, to_string(raw_charlist)} | Builder.Helpers.token_meta(token)]
    encode_literal(codepoint, token_meta, state)
  end

  # Boolean literals: 3-tuple tokens like {true, meta, nil}
  defp literal_to_ast({true, _meta, _value} = token, state) do
    encode_literal(true, Builder.Helpers.token_meta(token), state)
  end

  defp literal_to_ast({false, _meta, _value} = token, state) do
    encode_literal(false, Builder.Helpers.token_meta(token), state)
  end

  defp literal_to_ast({nil, _meta, _value} = token, state) do
    encode_literal(nil, Builder.Helpers.token_meta(token), state)
  end

  # Atom: 3-tuple token {:atom, meta, atom_value}
  # Include format: :atom for special atoms :true/:false/:nil (distinguishes :true from true)
  defp literal_to_ast({:atom, _meta, atom} = token, state)
       when atom in [true, false, nil] do
    atom_meta = [{:format, :atom} | Builder.Helpers.token_meta(token)]
    encode_literal(atom, atom_meta, state)
  end

  defp literal_to_ast({:atom, _meta, atom} = token, state) do
    encode_literal(atom, Builder.Helpers.token_meta(token), state)
  end

  # Alias: wrap in __aliases__ tuple with :last metadata
  # NOT encoded - aliases are AST nodes, not primitive literals
  defp literal_to_ast({:alias, _meta, atom} = token, _state) do
    m = Builder.Helpers.token_meta(token)
    {:__aliases__, [last: m] ++ m, [atom]}
  end

  # Identifier: wrap in tuple with nil context (variable reference)
  # NOT encoded - identifiers are AST nodes, not primitive literals
  defp literal_to_ast({:identifier, _meta, atom} = token, _state) do
    {atom, Builder.Helpers.token_meta(token), nil}
  end

  # Paren identifier: same as identifier, but mark it so led() knows to handle (
  # Add paren_call: true marker so led() can distinguish if(a) from if (a)
  defp literal_to_ast({:paren_identifier, _meta, atom} = token, _state) do
    {atom, [paren_call: true] ++ Builder.Helpers.token_meta(token), nil}
  end

  # Do identifier: same as identifier
  defp literal_to_ast({:do_identifier, _meta, atom} = token, _state) do
    {atom, Builder.Helpers.token_meta(token), nil}
  end

  # Bracket identifier: same as identifier, [ will be handled by led
  defp literal_to_ast({:bracket_identifier, _meta, atom} = token, _state) do
    {atom, Builder.Helpers.token_meta(token), nil}
  end

  # Op identifier: same as identifier, for no-parens calls
  defp literal_to_ast({:op_identifier, _meta, atom} = token, _state) do
    {atom, Builder.Helpers.token_meta(token), nil}
  end

  # String (for future use)
  defp literal_to_ast({:string, _meta, value} = token, state) do
    encode_literal(value, Builder.Helpers.token_meta(token), state)
  end

  # Range operator (..) as standalone expression
  # NOT encoded - operators are AST nodes, not primitive literals
  defp literal_to_ast({:range_op, _meta, op} = token, _state) do
    {op, Builder.Helpers.token_meta(token), []}
  end

  # Ellipsis operator (...) as standalone expression
  # NOT encoded - operators are AST nodes, not primitive literals
  defp literal_to_ast({:ellipsis_op, _meta, op} = token, _state) do
    {op, Builder.Helpers.token_meta(token), []}
  end

  # Fallback for 3-tuple tokens - return value
  defp literal_to_ast({_kind, _meta, value}, _state) do
    value
  end

  defp syntax_error_before(meta) do
    line = Keyword.get(meta, :line, 1)
    column = Keyword.get(meta, :column, 1)
    {[line: line, column: column], "syntax error before: ", ""}
  end

  defp syntax_error_before(meta, token_value) when is_atom(token_value) do
    line = Keyword.get(meta, :line, 1)
    column = Keyword.get(meta, :column, 1)
    {[line: line, column: column], "syntax error before: ", to_string(token_value)}
  end

  defp syntax_error_before(meta, token_value) when is_binary(token_value) do
    line = Keyword.get(meta, :line, 1)
    column = Keyword.get(meta, :column, 1)
    {[line: line, column: column], "syntax error before: ", token_value}
  end

  # Extract line/column metadata from an AST node (for nested calls)
  defp extract_meta({_name, meta, _args}) when is_list(meta) do
    Keyword.take(meta, [:line, :column])
  end

  defp extract_meta(_), do: []

  # Check if AST can receive a paren call in led()
  # Bare identifiers need paren_call: true marker (from :paren_identifier token)
  # Other expressions (calls, dot expressions, etc.) can always receive paren calls
  defp is_paren_call_valid({_name, meta, nil}) when is_list(meta) do
    Keyword.get(meta, :paren_call, false)
  end

  defp is_paren_call_valid(_), do: true

  # Build metadata with newlines count if present
  # token_meta is already [line: L, column: C]
  defp build_meta_with_newlines(token_meta, 0) do
    token_meta
  end

  defp build_meta_with_newlines(token_meta, newlines) when newlines > 0 do
    [newlines: newlines] ++ token_meta
  end

  defp function_capture_operand?({:/, _meta, [callee, arity]}) when is_integer(arity) do
    case callee do
      {name, _meta, nil} when is_atom(name) -> true
      {{:., _meta, _args}, _meta2, []} -> true
      _ -> false
    end
  end

  defp function_capture_operand?(_), do: false

  defp contains_do_block?({_, meta, args}) when is_list(meta) and is_list(args) do
    Keyword.has_key?(meta, :do) or Enum.any?(args, &contains_do_block?/1)
  end

  defp contains_do_block?(list) when is_list(list), do: Enum.any?(list, &contains_do_block?/1)
  defp contains_do_block?(_), do: false

  defp parse_access_indices(acc, state, cursor, _ctx, log) do
    case Brackets.parse_bracket_arg_no_skip(state, cursor, log) do
      {:ok, arg, state, cursor, log} -> {:ok, [arg | acc], state, cursor, log}
      {:error, reason, state, cursor, log} -> {:error, reason, state, cursor, log}
    end
  end

  @doc """
  Specialized led that handles dot operators AND paren calls, but stops at `{`.
  Used by parse_base_with_dots_and_calls for struct names like %Foo.Bar{} and %unquote(struct){}.
  Also used by Maps.parse_map_base_expr for unary operators in struct context.
  """
  @spec led_dots_and_calls(Macro.t(), State.t(), Cursor.t(), EventLog.t(), context()) :: result()
  def led_dots_and_calls(left, %State{} = state, cursor, %EventLog{} = log, %Context{} = context) do
    case TokenAdapter.peek(state, cursor) do
      # Handle dot-call: foo.() (tokenized as dot_call_op followed by parens)
      # dot_call_op is a 3-tuple: {:dot_call_op, meta, :.}
      {:ok, {:dot_call_op, _, _} = _dot_tok, _state, _cursor} ->
        {:ok, dot_tok, state, cursor} = TokenAdapter.next(state, cursor)
        dot_meta = TokenAdapter.token_meta(dot_tok)

        case TokenAdapter.next(state, cursor) do
          {:ok, tok, state, cursor} when is_kind(tok, :"(") ->
            {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

            with {:ok, args, state, cursor, log} <-
                   ToxicParser.Grammar.CallsPrivate.parse_paren_args(
                     [],
                     state,
                     cursor,
                     context,
                     log
                   ),
                 {:ok, close_meta, trailing_newlines, state, cursor} <-
                   Meta.consume_closing(state, cursor, :")") do
              total_newlines =
                Meta.total_newlines(leading_newlines, trailing_newlines, args == [])

              call_meta = Meta.closing_meta(dot_meta, close_meta, total_newlines)
              callee = {:., dot_meta, [left]}
              combined = {callee, call_meta, Enum.reverse(args)}
              led_dots_and_calls(combined, state, cursor, log, context)
            else
              other -> Result.normalize_error(other, cursor, log)
            end

          {:ok, {got_kind, _meta, _value}, state, cursor} ->
            {:error, {:expected, :"(", got: got_kind}, state, cursor, log}

          {:eof, state, cursor} ->
            {:error, :unexpected_eof, state, cursor, log}

          {:error, diag, state, cursor} ->
            {:error, diag, state, cursor, log}
        end

      {:ok, tok, _state, _cursor} when is_kind(tok, :.) ->
        {:ok, dot_tok, state, cursor} = TokenAdapter.next(state, cursor)
        dot_meta = TokenAdapter.token_meta(dot_tok)

        # Parse the RHS of dot - must be an alias or identifier
        # Pass dot_meta for curly calls where the call metadata uses the dot's position
        with {:ok, rhs, state, cursor, log} <-
               Dots.parse_member(state, cursor, context, log, dot_meta) do
          combined_result =
            case rhs do
              # Alias on RHS with bracket access (unwrap the flag, bracket handled separately)
              {{:__aliases__, rhs_meta, [rhs_alias]}, :allows_bracket} ->
                build_dot_alias(left, rhs_alias, rhs_meta, dot_meta)

              # Alias on RHS - build combined __aliases__ (dot_alias rule)
              {:__aliases__, rhs_meta, [rhs_alias]} ->
                build_dot_alias(left, rhs_alias, rhs_meta, dot_meta)

              # Paren call on RHS (tokenized as dot_paren_identifier)
              # Build as a call on the dotted target: Foo.bar() => {{:., ..., [Foo, :bar]}, meta, args}
              {name, meta, args} when is_atom(name) and is_list(args) ->
                {:ok, {{:., dot_meta, [left, name]}, meta, args}}

              # Simple identifier with :no_parens_call flag - treat same as simple identifier
              # In struct base context, { is the struct body, not a no-parens call argument
              {member, member_meta, :no_parens_call} when is_atom(member) ->
                {:ok, {{:., dot_meta, [left, member]}, [no_parens: true] ++ member_meta, []}}

              # Simple identifier with :allows_bracket flag
              {member, member_meta, :allows_bracket} when is_atom(member) ->
                {:ok, {{:., dot_meta, [left, member]}, [no_parens: true] ++ member_meta, []}}

              # Simple identifier: {member_atom, member_meta}
              {member, member_meta} when is_atom(member) ->
                {:ok, {{:., dot_meta, [left, member]}, [no_parens: true] ++ member_meta, []}}

              # Other AST node
              other ->
                {:ok, {:., dot_meta, [left, other]}}
            end

          # Handle error from build_dot_alias (e.g., atom.Alias)
          case combined_result do
            {:ok, combined} ->
              # Continue to handle chained dots and calls
              led_dots_and_calls(combined, state, cursor, log, context)

            {:error, reason} ->
              {:error, reason, state, cursor, log}
          end
        end

      # Handle paren calls: foo(args) or unquote(struct)
      {:ok, tok, _state, _cursor} when is_kind(tok, :"(") ->
        {:ok, _open_tok, state, cursor} = TokenAdapter.next(state, cursor)

        # Skip leading EOE and count newlines
        {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

        with {:ok, args, state, cursor, log} <-
               ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, cursor, context, log) do
          with {:ok, close_meta, trailing_newlines, state, cursor} <-
                 Meta.consume_closing(state, cursor, :")") do
            total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
            callee_meta = extract_meta(left)
            call_meta = Meta.closing_meta(callee_meta, close_meta, total_newlines)

            combined =
              case left do
                {name, _meta, nil} when is_atom(name) ->
                  {name, call_meta, Enum.reverse(args)}

                _ ->
                  {left, call_meta, Enum.reverse(args)}
              end

            # Continue to handle more dots and calls
            led_dots_and_calls(combined, state, cursor, log, context)
          else
            other -> Result.normalize_error(other, cursor, log)
          end
        end

      # Handle bracket access: 0[l] for structs like %0[l]{}
      # Grammar: bracket_expr -> access_expr bracket_arg
      {:ok, tok, _state, _cursor} when is_kind(tok, :"[") ->
        {:ok, open_tok, state, cursor} = TokenAdapter.next(state, cursor)

        # Skip leading EOE and count newlines
        {state, cursor, leading_newlines} = EOE.skip_count_newlines(state, cursor, 0)

        with {:ok, indices, state, cursor, log} <-
               parse_access_indices([], state, cursor, context, log) do
          # Skip trailing EOE before close bracket
          {state, cursor, _trailing_newlines} = EOE.skip_count_newlines(state, cursor, 0)

          case TokenAdapter.next(state, cursor) do
            {:ok, {:"]", _meta, _value} = close_tok, state, cursor} ->
              # Build bracket access AST like led_bracket does
              open_meta = TokenAdapter.token_meta(open_tok)
              close_meta = TokenAdapter.token_meta(close_tok)

              bracket_meta =
                Meta.closing_meta(open_meta, close_meta, leading_newlines, from_brackets: true)

              combined =
                {{:., bracket_meta, [Access, :get]}, bracket_meta, [left | Enum.reverse(indices)]}

              # Continue to handle more chained operations
              led_dots_and_calls(combined, state, cursor, log, context)

            {:ok, {got_kind, _meta, _value}, state, cursor} ->
              {:error, {:expected, :"]", got: got_kind}, state, cursor, log}

            {:eof, state, cursor} ->
              {:error, :unexpected_eof, state, cursor, log}

            {:error, diag, state, cursor} ->
              {:error, diag, state, cursor, log}
          end
        end

      _ ->
        # Not a dot, paren, or bracket - return what we have
        {:ok, left, state, cursor, log}
    end
  end
end
