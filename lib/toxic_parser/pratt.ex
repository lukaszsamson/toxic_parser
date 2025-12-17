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
    :dot_op,
    :dot_call_op
  ]

  defp is_binary_only_op(kind), do: kind in @binary_only_ops

  @type context :: Context.t()

  # Operators that can extend matched_expr to no_parens_expr
  # From elixir_parser.yrl no_parens_op_expr rules (lines 230-250)
  # EXCLUDED: :dot_op, :dot_call_op (via access_expr), :stab_op (handled separately), :assoc_op (map-only)
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
          {:ok, Macro.t(), State.t(), EventLog.t()}
          | {:error, term(), State.t(), EventLog.t()}
          | {:keyword_key, term(), term(), term()}
          | {:keyword_key_interpolated, term(), term(), term(), term(), term(), term()}

  defp ensure_no_parens_extension_opt(opts) do
    allow? =
      Keyword.get(opts, :allow_no_parens_extension?, not Keyword.get(opts, :unary_operand, false))

    Keyword.put(opts, :allow_no_parens_extension?, allow?)
  end

  defp is_no_parens_op_expr?(kind), do: kind in @no_parens_op_expr_operators

  @doc """
  Parses an expression in the given context.
  """
  @spec parse(State.t(), context(), EventLog.t()) :: result()
  def parse(%State{} = state, %Context{} = context, %EventLog{} = log) do
    with {:ok, token, state} <- TokenAdapter.next(state),
         {:ok, left, state, log} <- nud(token, state, context, log) do
      led(left, state, log, 0, context)
    else
      {:eof, state} -> {:error, :unexpected_eof, state, log}
      # Handle keyword_key from Strings.parse - bubble up to caller
      {:keyword_key, _, _, _} = keyword_key -> keyword_key
      {:keyword_key_interpolated, _, _, _, _, _, _} = keyword_key -> keyword_key
      other -> Result.normalize_error(other, log)
    end
  end

  @doc """
  Parses a base expression (nud only, no trailing operators via led).
  Used for sub_matched_expr in map_base_expr grammar rule.
  This version does NOT check for do-blocks or no-parens call arguments.
  """
  @spec parse_base(State.t(), context(), EventLog.t()) :: result()
  def parse_base(%State{} = state, %Context{} = context, %EventLog{} = log) do
    with {:ok, token, state} <- TokenAdapter.next(state),
         {:ok, ast, state, log} <-
           nud(token, state, context, log,
             min_bp: 10000,
             allow_containers: false
           ) do
      {:ok, ast, state, log}
    else
      {:eof, state} -> {:error, :unexpected_eof, state, log}
      other -> Result.normalize_error(other, log)
    end
  end

  @doc """
  Parses a base expression with dot operator support for dotted aliases.
  Used for struct names like `%Foo.Bar{}` where we need to parse the full
  alias chain (Foo.Bar) but NOT consume {} as tuple/call arguments.

  Only processes dot_op for alias chaining. Does not handle:
  - Paren calls: foo.bar()
  - No-parens calls: foo.bar arg
  - Bracket access: foo.bar[x]
  - Do-blocks: foo.bar do...end
  """
  @spec parse_base_with_dots(State.t(), context(), EventLog.t()) :: result()
  def parse_base_with_dots(%State{} = state, %Context{} = context, %EventLog{} = log) do
    with {:ok, token, state} <- TokenAdapter.next(state),
         {:ok, ast, state, log} <-
           nud(token, state, context, log,
             min_bp: 10000,
             allow_containers: false
           ) do
      led_dot_only(ast, state, log, context)
    else
      {:eof, state} -> {:error, :unexpected_eof, state, log}
      other -> Result.normalize_error(other, log)
    end
  end

  @doc """
  Parses an expression base with dots and paren calls, stopping at `{`.
  Used for struct names like `%Foo.Bar{}` and `%unquote(struct){}`.
  """
  @spec parse_base_with_dots_and_calls(State.t(), context(), EventLog.t()) :: result()
  def parse_base_with_dots_and_calls(%State{} = state, %Context{} = context, %EventLog{} = log) do
    with {:ok, token, state} <- TokenAdapter.next(state),
         {:ok, ast, state, log} <-
           nud(token, state, context, log,
             min_bp: 10000,
             allow_containers: false
           ) do
      led_dots_and_calls(ast, state, log, context)
    else
      {:eof, state} -> {:error, :unexpected_eof, state, log}
      other -> Result.normalize_error(other, log)
    end
  end

  @doc """
  Parses an expression with a minimum binding power.
  Used for map updates where we need to stop before the pipe operator.
  Also used for guard expressions in stab clauses where we need to stop before ->.
  """
  @spec parse_with_min_bp(State.t(), context(), EventLog.t(), non_neg_integer(), keyword()) ::
          result()
  def parse_with_min_bp(
        %State{} = state,
        %Context{} = context,
        %EventLog{} = log,
        min_bp,
        opts \\ []
      ) do
    # Merge opts with min_bp for nud - preserve stop_at_assoc for string parsing
    nud_opts = Keyword.merge([min_bp: min_bp], opts)

    with {:ok, token, state} <- TokenAdapter.next(state),
         {:ok, left, state, log} <- nud(token, state, context, log, nud_opts) do
      led(left, state, log, min_bp, context, opts)
    else
      {:eof, state} -> {:error, :unexpected_eof, state, log}
      # Handle keyword_key from Strings.parse - bubble up to caller
      {:keyword_key, _, _, _} = keyword_key -> keyword_key
      {:keyword_key_interpolated, _, _, _, _, _, _} = keyword_key -> keyword_key
      other -> Result.normalize_error(other, log)
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
  defp nud(token, state, context, log) do
    nud(token, state, context, log, [])
  end

  defp nud(token, state, context, log, opts) do
    min_bp = Keyword.get(opts, :min_bp, 0)
    allow_containers = Keyword.get(opts, :allow_containers, true)
    string_min_bp = Keyword.get(opts, :string_min_bp, min_bp)
    # Pass through options like stop_at_assoc to string parsing
    led_opts = Keyword.take(opts, [:stop_at_assoc])

    case token.kind do
      :error_token ->
        meta = build_meta(token.metadata)
        ast = Builder.Helpers.error(token.value, meta)
        {:ok, ast, state, log}

      :capture_int ->
        parse_capture_int(token, state, log)

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
          state = TokenAdapter.pushback(state, token)
          alias ToxicParser.Grammar.Containers
          Containers.parse(state, context, log, min_bp)
        else
          nud_literal_or_unary(token, state, context, log, min_bp, false)
        end

      # Quoted atoms need to be handled by Strings.parse even in restricted mode
      kind when kind in [:atom_unsafe_start, :atom_safe_start] ->
        state = TokenAdapter.pushback(state, token)
        alias ToxicParser.Grammar.Strings
        Strings.parse(state, context, log, string_min_bp, led_opts)

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
        state = TokenAdapter.pushback(state, token)
        alias ToxicParser.Grammar.Strings
        Strings.parse(state, context, log, min_bp, led_opts)

      # fn tokens need to be handled by Blocks.parse unless restricted
      :fn ->
        if allow_containers do
          state = TokenAdapter.pushback(state, token)
          alias ToxicParser.Grammar.Blocks
          Blocks.parse(state, context, log)
        else
          nud_literal_or_unary(token, state, context, log, min_bp, false)
        end

      # kw_identifier at expression position is a syntax error
      :kw_identifier ->
        {:error, syntax_error_before(build_meta(token.metadata), token.value), state, log}

      _ ->
        nud_literal_or_unary(token, state, context, log, min_bp, allow_containers)
    end
  end

  # allow_containers: when false (e.g., struct base parsing), don't parse no-parens calls
  # The grammar's map_base_expr only produces bare identifiers via sub_matched_expr
  defp nud_literal_or_unary(token, state, context, log, min_bp, allow_containers) do
    case Precedence.unary(token.kind) do
      {_bp, _assoc} ->
        # Pass outer min_bp for trailing led(), not the unary's own precedence.
        # The operand_min_bp is computed inside parse_unary from the operator's precedence.
        parse_unary(token, state, context, log)

      nil ->
        cond do
          token.kind == :dual_op ->
            # Pass outer min_bp for trailing led()
            parse_unary(token, state, context, log)

          # ternary_op ://" used as unary (e.g., //foo) becomes {:/,_,[{:/,_,nil},rhs]}
          token.kind == :ternary_op and token.value == :"//" ->
            parse_ternary_unary(token, state, context, log)

          # Binary-only operators cannot start an expression
          # e.g., "1 + * 3" should error because * cannot follow +
          is_binary_only_op(token.kind) ->
            meta = build_meta(token.metadata)
            {:error, syntax_error_before(meta, token.value), state, log}

          # EOE tokens (semicolons) cannot start an expression
          # e.g., "1+;\n2" should error because ; cannot follow +
          # Note: newlines are handled separately by EOE.skip_newlines_only
          token.kind == :eoe and token.value.source == :semicolon ->
            meta = build_meta(token.metadata)
            {:error, syntax_error_before(meta, "';'"), state, log}

          true ->
            nud_identifier_or_literal(token, state, context, log, min_bp, allow_containers)
        end
    end
  end

  defp nud_identifier_or_literal(token, state, context, log, min_bp, allow_containers) do
    allow_do_blocks = Context.allow_do_block?(context)

    case TokenAdapter.peek(state) do
      {:ok, next_tok, _} ->
        binary_bp = bp(next_tok.kind)

        allow_no_parens =
          token.kind == :op_identifier or
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
        excluded_kinds = [:paren_identifier, :alias, :bracket_identifier, nil, true, false | literal_kinds]

        if token.kind not in excluded_kinds and
             allow_no_parens and
             allow_containers do
          parse_no_parens_call_nud_with_min_bp(token, state, context, log, min_bp)
        else
          ast = literal_to_ast(token)
          maybe_attach_do_block(ast, token, state, context, log, min_bp, allow_do_blocks)
        end

      _ ->
        ast = literal_to_ast(token)
        maybe_attach_do_block(ast, token, state, context, log, min_bp, allow_do_blocks)
    end
  end

  defp maybe_attach_do_block(ast, token, state, context, log, min_bp, true) do
    DoBlocks.maybe_do_block(ast, state, context, log,
      token: token,
      min_bp: min_bp,
      parse_no_parens: &parse_no_parens_call_nud_with_min_bp/5
    )
  end

  defp maybe_attach_do_block(ast, _token, state, _context, log, _min_bp, false) do
    {:ok, ast, state, log}
  end

  # Parse capture_int followed by int (e.g., &1, &2)
  # Rule: access_expr -> capture_int int : build_unary_op('$1', number_value('$2'))
  defp parse_capture_int(capture_token, state, log) do
    case TokenAdapter.next(state) do
      {:ok, %{kind: :int} = int_token, state} ->
        meta = build_meta(capture_token.metadata)
        # Extract the integer value from the int token
        int_value = literal_to_ast(int_token)
        ast = {:&, meta, [int_value]}
        {:ok, ast, state, log}

      {:ok, other_token, state} ->
        {:error, {:expected, :int, got: other_token.kind}, state, log}

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse a no-parens call in nud context (for identifiers followed by args)
  # This is similar to Calls.parse_no_parens_call but doesn't call led at the end
  # This version uses min_bp to stop argument parsing before certain operators
  defp parse_no_parens_call_nud_with_min_bp(callee_tok, state, context, log, min_bp) do
    with {:ok, args, state, log} <-
           Calls.parse_no_parens_args([], state, context, log, min_bp) do
      callee = callee_tok.value
      base_meta = Builder.Helpers.token_meta(callee_tok.metadata)

      # Check if there's a do-block following that will attach to this call.
      # Only counts as "has do-block" if context allows do-block attachment.
      # When context has allow_do_block: false, the do-block belongs to an outer call.
      has_do_block =
        match?({:ok, %{kind: :do}, _}, TokenAdapter.peek(state)) and
          Context.allow_do_block?(context)

      # For op_identifier with a single argument AND no do-block, add ambiguous_op: nil
      # This matches elixir_parser.yrl behavior for no_parens_one_ambig
      # Don't add it when there's a do-block because then there's no ambiguity
      meta =
        if callee_tok.kind == :op_identifier and length(args) == 1 and not has_do_block do
          [ambiguous_op: nil] ++ base_meta
        else
          base_meta
        end

      ast = {callee, meta, args}
      # Check for do-block ONLY, don't call led (caller will do that)
      DoBlocks.maybe_do_block(ast, state, context, log)
    end
  end

  # Parse unary operator expression
  defp parse_unary(op_token, state, context, log) do
    # For ellipsis/range operators, check for EOE terminator BEFORE skipping
    # This ensures "x...;" correctly sees the ; as a terminator
    # and doesn't skip it, allowing `led` to see the EOE separation
    if op_token.kind in [:ellipsis_op, :range_op] do
      case TokenAdapter.peek(state) do
        {:ok, %{kind: :eoe}, _} ->
          # EOE is a terminator for ellipsis/range - return standalone
          ast = {op_token.value, build_meta(op_token.metadata), []}
          {:ok, ast, state, log}

        _ ->
          # Not EOE, proceed with normal parsing
          parse_unary_operand_or_standalone(op_token, state, context, log)
      end
    else
      # Skip EOE after unary operator (allows "!\ntrue")
      # Note: Unlike binary ops, unary ops don't include newlines metadata
      {state, _newlines} = EOE.skip_count_newlines(state, 0)
      parse_unary_after_eoe_skip(op_token, state, context, log)
    end
  end

  # Parse unary operand for ellipsis/range after confirming no EOE terminator
  defp parse_unary_operand_or_standalone(op_token, state, context, log) do
    # Skip EOE after operator
    {state, _newlines} = EOE.skip_count_newlines(state, 0)
    parse_unary_after_eoe_skip(op_token, state, context, log)
  end

  # Common unary parsing logic after EOE is handled
  defp parse_unary_after_eoe_skip(op_token, state, context, log) do
    case TokenAdapter.next(state) do
      {:ok, operand_token, state} ->
        # Special case: ... and .. followed by pure binary operator or terminator should be standalone
        # e.g., "... * 1" should parse as (* (...) 1), not (... *)
        # e.g., "fn -> ... end" - the ... before end should be standalone
        # BUT: dual_op (+/-) after ellipsis should be unary, not binary
        # e.g., "... + 1 * 2" should parse as "...((+1) * 2)" not "(... + 1) * 2"
        ellipsis_terminators = [
          :end,
          :do,
          :eoe,
          :")",
          :"]",
          :"}",
          :end_interpolation,
          :",",
          :when_op,
          :stab_op,
          :block_identifier
        ]

        # dual_op can be unary (+/-), ternary_op can be unary (//)
        # so they should NOT trigger standalone ellipsis
        is_pure_binary_op =
          operand_token.kind not in [:dual_op, :ternary_op] and
            Precedence.binary(operand_token.kind) != nil

        if op_token.kind in [:ellipsis_op, :range_op] and
             (is_pure_binary_op or operand_token.kind in ellipsis_terminators) do
          ast = {op_token.value, build_meta(op_token.metadata), []}
          state = TokenAdapter.pushback(state, operand_token)
          {:ok, ast, state, log}
          # Special case: at_op + bracket_identifier (e.g., @foo[1])
          # Grammar: bracket_at_expr -> at_op_eol dot_bracket_identifier bracket_arg
          # The @foo becomes the subject, bracket access is handled at outer level
        else
          if op_token.kind == :at_op and operand_token.kind == :bracket_identifier do
            op = op_token.value
            meta = build_meta(op_token.metadata)
            operand = Builder.Helpers.from_token(operand_token)
            ast = Builder.Helpers.unary(op, operand, meta)
            {:ok, ast, state, log}
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
            {next_token, next_token_kind, next_token_value} =
              case TokenAdapter.peek(state) do
                {:ok, tok, _} -> {tok, tok.kind, tok.value}
                _ -> {nil, nil, nil}
              end

            operand_min_bp =
              cond do
                operand_token.kind == :do_identifier and Context.allow_do_block?(context) ->
                  0

                operand_token.kind == :identifier and next_token_kind == :do_identifier and
                    Context.allow_do_block?(context) ->
                  0

                operand_token.kind == :identifier and next_token_kind == :identifier and
                  next_token_value in @do_block_keywords and Context.allow_do_block?(context) ->
                  0

                # Only lower precedence for do-block keywords if a do-block or argument follows
                # This prevents @for | x from parsing as @(for | x) when | is a binary operator
                operand_token.kind in [:identifier, :dot_identifier] and
                  operand_token.value in @do_block_keywords and Context.allow_do_block?(context) and
                    (next_token_kind == :do or
                       (next_token != nil and NoParens.can_start_no_parens_arg?(next_token))) ->
                  0

                op_token.kind in [:ellipsis_op, :range_op] ->
                  100

                true ->
                  case Precedence.unary(op_token.kind) do
                    {bp, _assoc} -> bp
                    nil -> 300
                  end
              end

            # Unary operator operands must NOT allow no_parens_expr extension.
            # The extension bypasses min_bp which would break "-1 + 2" (making it "-(1+2)").
            # Restrict the context to disable extension inside the operand.
            # The outer context is preserved for led() after the unary is built.
            operand_context = restrict_no_parens_extension(context)

            with {:ok, operand, state, log} <-
                   parse_rhs(operand_token, state, operand_context, log, operand_min_bp,
                     unary_operand: true
                   ) do
              op = op_token.value
              meta = build_meta(op_token.metadata)
              ast = Builder.Helpers.unary(op, operand, meta)
              # After building the unary, continue with led() to handle trailing operators.
              # Use the unary's precedence (operand_min_bp) so only higher-precedence operators
              # can attach. E.g., `+ a ** b` parses as `(+a) ** b` since ** bp=230 < + bp=300.
              # For ellipsis/range with operand_min_bp=100, `... a ** b` -> `... (a ** b)`.
              led(ast, state, log, operand_min_bp, context)
            end
          end
        end

      {:eof, state} ->
        # Special case: ... and .. can be standalone at EOF
        if op_token.kind in [:ellipsis_op, :range_op] do
          ast = {op_token.value, build_meta(op_token.metadata), []}
          {:ok, ast, state, log}
        else
          {:error, :unexpected_eof, state, log}
        end

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse ternary_op :"//" as binary operator (e.g., 1..10//2)
  # When left is a range {:.., meta, [start, stop]}, combines into {:..//, meta, [start, stop, step]}
  # Otherwise, produces an error (// must follow ..)
  defp parse_ternary_op(left, state, min_bp, context, log) do
    {:ok, op_token, state} = TokenAdapter.next(state)
    {bp, _assoc} = Precedence.binary(:ternary_op)

    # Skip EOE after operator
    {state, _newlines} = EOE.skip_count_newlines(state, 0)

    case TokenAdapter.next(state) do
      {:ok, rhs_token, state} ->
        with {:ok, step, state, log} <- parse_rhs(rhs_token, state, context, log, bp) do
          case left do
            # Range expression: {:.., meta, [start, stop]} -> {:..//, meta, [start, stop, step]}
            {:.., range_meta, [start, stop]} ->
              combined = {:..//, range_meta, [start, stop, step]}
              led(combined, state, log, min_bp, context)

            # Not a range - this is an error but we still parse it as {:"//", meta, [left, step]}
            # The error handling follows spitfire's pattern
            _ ->
              op_meta = build_meta(op_token.metadata)
              combined = {op_token.value, op_meta, [left, step]}
              led(combined, state, log, min_bp, context)
          end
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse ternary_op :"//" as unary operator (e.g., //foo)
  # This produces: {:/, outer_meta, [{:/, inner_meta, nil}, rhs]}
  # where outer has column+1 and inner has original column
  defp parse_ternary_unary(op_token, state, context, log) do
    meta = build_meta(op_token.metadata)

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
    {state, _newlines} = EOE.skip_count_newlines(state, 0)

    # Parse the operand with unary precedence
    case TokenAdapter.next(state) do
      {:ok, operand_token, state} ->
        with {:ok, operand, state, log} <-
               parse_rhs(operand_token, state, context, log, 300, unary_operand: true) do
          # Build: {:/, outer_meta, [{:/, inner_meta, nil}, operand]}
          ast = {:/, outer_meta, [{:/, inner_meta, nil}, operand]}
          {:ok, ast, state, log}
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Parse RHS of binary operator, handling chained operators with proper precedence
  # For identifiers that could be calls with do-blocks (like `if true do...end`),
  # we need special handling but must preserve min_bp for associativity
  # opts can contain :unary_operand to indicate we're parsing a unary operator's operand
  defp parse_rhs(token, state, context, log, min_bp, opts \\ []) do
    # Check if this is an identifier that could be a call with arguments
    cond do
      Identifiers.classify(token.kind) != :other ->
        # Handle identifier specially to preserve min_bp
        parse_rhs_identifier(token, state, context, log, min_bp, opts)

      # Container tokens need special handling to preserve min_bp
      token.kind in [:"[", :"{", :"(", :"<<", :%, :%{}, :%] ->
        state = TokenAdapter.pushback(state, token)
        alias ToxicParser.Grammar.Containers

        # Call Containers.parse with min_bp to preserve operator precedence
        Containers.parse(state, context, log, min_bp)

      # String and quoted atom tokens need special handling to preserve min_bp
      token.kind in [
        :bin_string_start,
        :list_string_start,
        :bin_heredoc_start,
        :list_heredoc_start,
        :sigil_start,
        :atom_unsafe_start,
        :atom_safe_start
      ] ->
        state = TokenAdapter.pushback(state, token)
        alias ToxicParser.Grammar.Strings

        # Call Strings.parse with min_bp to preserve operator precedence
        Strings.parse(state, context, log, min_bp, opts)

      true ->
        nud_opts = Keyword.put(opts, :min_bp, min_bp)

        with {:ok, right, state, log} <- nud(token, state, context, log, nud_opts) do
          opts = ensure_no_parens_extension_opt(opts)
          led(right, state, log, min_bp, context, opts)
        end
    end
  end

  # Parse identifier on RHS, handling calls while preserving min_bp for associativity
  # opts can contain :unary_operand to indicate we're parsing a unary operator's operand
  defp parse_rhs_identifier(token, state, context, log, min_bp, opts) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"("}, _} when token.kind == :paren_identifier ->
        # Paren call (no space before '(') - parse directly to preserve min_bp.
        # Don't use Calls.parse because it calls led(_, 0, _) which loses min_bp.
        # Pass opts through so do-blocks after paren call know the context.
        with {:ok, ast, state, log} <- parse_paren_call_base(token, state, context, log) do
          opts = ensure_no_parens_extension_opt(opts)
          maybe_nested_call_or_do_block(ast, state, log, min_bp, context, opts)
        end

      {:ok, %{kind: :"("}, _} ->
        # `foo (expr)` is a no-parens call where the `(` begins a parenthesized
        # expression argument (it is NOT a paren-call like `foo(expr)`).
        # This matters because trailing operators (e.g. `foo (1 <<< 7) - 1`)
        # belong to the argument expression in Elixir.
        state = TokenAdapter.pushback(state, token)

        with {:ok, right, state, log} <- Calls.parse_without_led(state, context, log, min_bp) do
          # Preserve min_bp so unary operands do not swallow lower-precedence operators.
          led_min_bp = min_bp
          opts = ensure_no_parens_extension_opt(opts)
          led(right, state, log, led_min_bp, context, opts)
        end

      {:ok, %{kind: :do}, _} ->
        # Do-block - delegate to Calls.parse_without_led
        state = TokenAdapter.pushback(state, token)

        with {:ok, right, state, log} <- Calls.parse_without_led(state, context, log, min_bp) do
          # Preserve min_bp so unary operands do not swallow lower-precedence operators.
          led_min_bp = min_bp
          opts = ensure_no_parens_extension_opt(opts)
          led(right, state, log, led_min_bp, context, opts)
        end

      {:ok, next_tok, _} ->
        cond do
          # Alias followed by [ is bracket access, not a no-parens call
          # Grammar: bracket_expr -> access_expr bracket_arg
          # Build alias AST and let led handle the [ as bracket access
          token.kind == :alias and next_tok.kind == :"[" ->
            ast = Builder.Helpers.from_token(token)
            opts = ensure_no_parens_extension_opt(opts)
            led(ast, state, log, min_bp, context, opts)

          # op_identifier means tokenizer determined this is a no-parens call
          # (e.g., `a -2` where `-2` is unary argument, not binary subtraction)
          # This must be checked BEFORE binary operator check
          # TODO: no coverage? only failing corpus
          token.kind == :op_identifier and
              (NoParens.can_start_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok)) ->
            state = TokenAdapter.pushback(state, token)

            with {:ok, right, state, log} <- Calls.parse_without_led(state, context, log, min_bp) do
              # Preserve the original min_bp for proper associativity
              opts = ensure_no_parens_extension_opt(opts)
              led(right, state, log, min_bp, context, opts)
            end

          # Binary operator follows - just return identifier, led will handle with min_bp
          bp(next_tok.kind) ->
            ast = Builder.Helpers.from_token(token)
            opts = ensure_no_parens_extension_opt(opts)
            led(ast, state, log, min_bp, context, opts)

          # Could be no-parens call argument - delegate to Calls
          # TODO: no coverage? only failing corpus
          NoParens.can_start_no_parens_arg?(next_tok) or Keywords.starts_kw?(next_tok) ->
            state = TokenAdapter.pushback(state, token)

            with {:ok, right, state, log} <- Calls.parse_without_led(state, context, log, min_bp) do
              # Preserve min_bp so unary operands do not swallow lower-precedence operators.
              led_min_bp = min_bp

              opts = ensure_no_parens_extension_opt(opts)
              led(right, state, log, led_min_bp, context, opts)
            end

          # Just a bare identifier
          true ->
            ast = Builder.Helpers.from_token(token)
            opts = ensure_no_parens_extension_opt(opts)
            led(ast, state, log, min_bp, context, opts)
        end

      _ ->
        # EOF or error - just return identifier
        ast = Builder.Helpers.from_token(token)
        opts = ensure_no_parens_extension_opt(opts)
        led(ast, state, log, min_bp, context, opts)
    end
  end

  @doc """
  Parse a paren call without calling led at the end.
  Used by parse_rhs_identifier to preserve min_bp for associativity.
  Also used by Maps.parse_unary_operand for %@i(){} patterns.
  """
  @spec parse_paren_call_base(map(), State.t(), context(), EventLog.t()) :: result()
  def parse_paren_call_base(callee_tok, %State{} = state, %Context{} = context, %EventLog{} = log) do
    {:ok, _open_tok, state} = TokenAdapter.next(state)

    # Skip leading EOE and count newlines
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    with {:ok, args, state, log} <-
           ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, context, log),
         {:ok, close_meta, trailing_newlines, state} <- Meta.consume_closing(state, :")") do
      total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
      callee_meta = build_meta(callee_tok.metadata)
      meta = Meta.closing_meta(callee_meta, close_meta, total_newlines)

      ast = {callee_tok.value, meta, Enum.reverse(args)}
      {:ok, ast, state, log}
    else
      other -> Result.normalize_error(other, log)
    end
  end

  @doc """
  Continue parsing with left-hand expression, looking for trailing operators.
  This is exposed for modules like Calls that need to continue parsing after
  building a call expression.
  """
  def led(left, %State{} = state, %EventLog{} = log, min_bp, %Context{} = context, opts \\ []) do
    # Check if there's EOE followed by a continuation operator
    # Only skip EOE when it precedes an operator that can continue the expression
    # This preserves EOE as separators for expr_list (e.g., "1;2" should not skip the ";")
    {state, eoe_tokens, newlines_before_op} = peek_past_eoe(state)

    case TokenAdapter.peek(state) do
      {:ok, next_token, _} ->
        led_dispatch(
          left,
          next_token,
          state,
          log,
          min_bp,
          context,
          opts,
          eoe_tokens,
          newlines_before_op
        )

      {:eof, state} ->
        # At EOF, push back EOE tokens so they're available for expr_list
        state = pushback_eoe_tokens(state, eoe_tokens)
        {:ok, left, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  defp led_dispatch(
         left,
         next_token,
         state,
         log,
         min_bp,
         context,
         opts,
         eoe_tokens,
         newlines_before_op
       ) do
    precedence = Precedence.binary(next_token.kind)

    case {next_token.kind, precedence} do
      # Parens call: identifier(args) or expr()(args) (nested call)
      # Rule: parens_call -> dot_call_identifier call_args_parens call_args_parens
      # NOTE: Only treat as call if there's NO EOE between left and (
      # `foo()` is a call, but `foo\n()` is not (the newline separates them)
      # Also, for bare identifiers like `if (a)`, only treat as call if
      # the identifier came from a :paren_identifier token (no space before `(`)
      {:"(", _} when eoe_tokens == [] ->
        led_call(left, state, log, min_bp, context, opts)

      # dot_call_op: expr.(args) - anonymous function call
      # Must check bp >= min_bp to respect operator precedence
      {:dot_call_op, {bp, _}} when bp >= min_bp ->
        led_dot_call(left, state, log, min_bp, context, opts)

      {:dot_op, {bp, _}} when bp >= min_bp ->
        led_dot(left, state, log, min_bp, context, opts)

      # Bracket access: expr[key]
      # Bracket access has precedence 310 (same as dot_op)
      # NOTE: Only treat as bracket access if:
      # 1. Bracket precedence (310) >= min_bp
      # 2. NO EOE between left and [ (`foo[x]` is bracket access, `foo\n[x]` is not)
      # 3. allows_bracket flag is true (for dot members: `foo.bar[x]` yes, `foo.bar [x]` no)
      {:"[", _} when 310 >= min_bp and eoe_tokens == [] ->
        led_bracket(left, state, log, min_bp, context, opts)

      # Special case: ternary_op :"//" following range_op :".."
      # Combines into {:..//, meta, [start, stop, step]}
      # BUT: If there's EOE between range and //, don't combine - // starts a new expression
      # e.g., "x..0;//y" should be two expressions: x..0 and //y (not x..0//y)
      {:ternary_op, {bp, _assoc}} when bp >= min_bp and eoe_tokens == [] ->
        parse_ternary_op(left, state, min_bp, context, log)

      # ternary_op after EOE is NOT a ternary continuation - it's unary // starting a new expr
      {:ternary_op, {bp, _assoc}} when bp >= min_bp and eoe_tokens != [] ->
        state = pushback_eoe_tokens(state, eoe_tokens)
        {:ok, left, state, log}

      # dual_op (+/-) after EOE is NOT a binary operator - it starts a new expression
      # e.g., "x()\n\n-1" should be two expressions: x() and -1 (unary minus)
      # This matches Elixir's behavior where dual_op after newlines is unary
      {:dual_op, {bp, _assoc}} when bp >= min_bp and eoe_tokens != [] ->
        # Push back EOE tokens and return left - dual_op after EOE is not continuation
        state = pushback_eoe_tokens(state, eoe_tokens)
        {:ok, left, state, log}

      # range_op (..) or ellipsis_op (...) after EOE is NOT a binary operator
      # e.g., "t;..<e" should be two expressions: t and (..<e), not (t..<) e
      # The range/ellipsis starts a new expression (unary/nullary)
      {kind, {bp, _assoc}}
      when kind in [:range_op, :ellipsis_op] and bp >= min_bp and eoe_tokens != [] ->
        state = pushback_eoe_tokens(state, eoe_tokens)
        {:ok, left, state, log}

      # stab_op (->) is NOT a general binary operator - it only appears in stab clause contexts
      # Parsing it here would wrongly parse "1 -> 2" as a binary expression
      # Stab clauses are handled by the Stabs module which knows when -> is valid
      {:stab_op, _} ->
        state = pushback_eoe_tokens(state, eoe_tokens)
        {:ok, left, state, log}

      # assoc_op (=>) is NOT a general binary operator - it only appears in map contexts
      # In no-parens call context, stop at => to avoid parsing n(&n=>1) as n(&(n=>1))
      # In normal context (inside maps), allow => to be consumed
      {:assoc_op, {bp, assoc}} when bp >= min_bp ->
        if Keyword.get(opts, :stop_at_assoc, false) do
          state = pushback_eoe_tokens(state, eoe_tokens)
          {:ok, left, state, log}
        else
          led_binary(left, state, log, min_bp, context, opts, bp, assoc, newlines_before_op)
        end

      {_, {bp, assoc}} when bp >= min_bp ->
        led_binary(left, state, log, min_bp, context, opts, bp, assoc, newlines_before_op)

      # no_parens_expr extension: when context allows it, operators in @no_parens_op_expr_operators
      # can extend matched_expr to no_parens_expr REGARDLESS of min_bp.
      # This implements the grammar rule: no_parens_expr -> matched_expr no_parens_op_expr
      # Example: @spec foo() when a: term - the `when` (bp=50) can extend `spec foo()`
      # even though @ has bp=320, because the operand context allows no_parens extension.
      {kind, {bp, assoc}} when is_tuple({bp, assoc}) ->
        allow_extension? = Keyword.get(opts, :allow_no_parens_extension?, true)

        if allow_extension? and min_bp == 0 and
             is_no_parens_op_expr?(kind) and Context.allow_no_parens_expr?(context) do
          led_binary(left, state, log, min_bp, context, opts, bp, assoc, newlines_before_op)
        else
          # No continuation operator found - push back EOE tokens
          state = pushback_eoe_tokens(state, eoe_tokens)
          {:ok, left, state, log}
        end

      _ ->
        # No continuation operator found - push back EOE tokens
        state = pushback_eoe_tokens(state, eoe_tokens)
        {:ok, left, state, log}
    end
  end

  defp led_call(left, state, log, min_bp, context, opts) do
    if is_paren_call_valid(left) do
      {:ok, _open_tok, state} = TokenAdapter.next(state)

      # Skip leading EOE and count newlines
      {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

      with {:ok, args, state, log} <-
             ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, context, log),
           {:ok, close_meta, trailing_newlines, state} <- Meta.consume_closing(state, :")") do
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
        maybe_nested_call_or_do_block(combined, state, log, min_bp, context, opts)
      else
        other -> Result.normalize_error(other, log)
      end
    else
      # Not a valid paren call (e.g., `if (a)` with space - `if` is :identifier, not :paren_identifier)
      # Return left as-is, the `(` will be handled as a separate parenthesized expression
      {:ok, left, state, log}
    end
  end

  defp led_dot_call(left, state, log, min_bp, context, opts) do
    with {:ok, combined, state, log} <-
           Dots.parse_dot_call(left, state, context, log) do
      # Check for nested calls (foo.()()) and do-blocks (foo.() do ... end)
      maybe_nested_call_or_do_block(combined, state, log, min_bp, context, opts)
    end
  end

  defp led_dot(left, state, log, min_bp, context, opts) do
    {:ok, dot_tok, state} = TokenAdapter.next(state)
    dot_meta = build_meta(dot_tok.metadata)

    # Skip EOE after dot (allows newlines between dot and member: foo.\n bar)
    {state, _newlines} = EOE.skip_count_newlines(state, 0)

    # Check for dot_container: expr.{...}
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"{"}, _} ->
        {:ok, _open, state} = TokenAdapter.next(state)

        with {:ok, args, newlines, close_meta, state, log} <-
               parse_dot_container_args(state, context, log) do
          combined =
            {{:., dot_meta, [left, :{}]}, Meta.closing_meta(dot_meta, close_meta, newlines), args}

          led(combined, state, log, min_bp, context, opts)
        end

      _ ->
        # Pass dot_meta for curly calls where the call metadata uses the dot's position
        with {:ok, rhs, state, log} <- Dots.parse_member(state, context, log, dot_meta) do
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
              case TokenAdapter.peek(state) do
            # When ( follows a dot member that was already a call (allows_bracket = true),
            # it's a nested paren call: foo.bar()()
            # When ( follows a simple identifier (allows_bracket = false), it means there's
            # whitespace before (, so ( starts a no-parens argument: foo.e ()
            {:ok, %{kind: :"("}, _} when not allows_bracket ->
              with {:ok, args, state, log} <-
                     Calls.parse_no_parens_args([], state, context, log) do
                combined = dot_to_no_parens_call(combined, args)
                # Check for do-blocks after no-parens call: foo.bar () do...end
                maybe_nested_call_or_do_block(combined, state, log, min_bp, context, opts)
              end

            # Nested paren call: foo.bar()() - rhs was already a call (allows_bracket = true)
            {:ok, %{kind: :"("}, _} ->
              {:ok, _open, state} = TokenAdapter.next(state)
              # Skip leading EOE and count newlines
              {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

              with {:ok, args, state, log} <-
                     ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, context, log) do
                # Skip trailing EOE before close paren
                {state, trailing_newlines} = EOE.skip_count_newlines(state, 0)

                case TokenAdapter.next(state) do
                  {:ok, %{kind: :")"} = close_tok, state} ->
                    # Only count trailing newlines for empty args
                    # (matches behavior in maybe_nested_call_or_do_block)
                    total_newlines =
                      if args == [] do
                        leading_newlines + trailing_newlines
                      else
                        leading_newlines
                      end

                    close_meta = build_meta(close_tok.metadata)

                    # When followed by parens, convert to call form with proper metadata
                    combined =
                      dot_to_call_with_meta(combined, args, total_newlines, close_meta)

                    # Check for nested calls and do-blocks (foo.bar() do...end)
                    maybe_nested_call_or_do_block(combined, state, log, min_bp, context, opts)

                  {:ok, other, state} ->
                    {:error, {:expected, :")", got: other.kind}, state, log}

                  {:eof, state} ->
                    {:error, :unexpected_eof, state, log}

                  {:error, diag, state} ->
                    {:error, diag, state, log}
                end
              end

            {:ok, %{kind: :"["}, _} when allows_bracket ->
              # Bracket access: foo.bar[:key] - only when bracket access is allowed
              # (no whitespace before [)
              led(combined, state, log, min_bp, context, opts)

            {:ok, %{kind: :do}, _} ->
              # Do-block after dot call: foo.bar() do...end
              # Only handle when rhs was a call (combined has args list)
              maybe_nested_call_or_do_block(combined, state, log, min_bp, context, opts)

            {:ok, next_tok, _} ->
              # Check for no-parens call argument after dot expression
              # Only parse as no-parens call if:
              # 1. The member was tokenized as op_identifier/dot_op_identifier (expects_no_parens_call)
              # 2. OR next token can start a no-parens arg (excluding dual_op when expects_no_parens_call is false)
              # 3. OR next token starts a keyword list
              should_parse_no_parens =
                expects_no_parens_call or Keywords.starts_kw?(next_tok) or
                  (NoParens.can_start_no_parens_arg?(next_tok) and next_tok.kind != :dual_op)

              if should_parse_no_parens do
                with {:ok, args, state, log} <-
                       Calls.parse_no_parens_args([], state, context, log) do
                  combined = dot_to_no_parens_call(combined, args)
                  # Check for do-blocks after no-parens call: foo.bar arg do...end
                  maybe_nested_call_or_do_block(combined, state, log, min_bp, context, opts)
                end
              else
                led(combined, state, log, min_bp, context, opts)
              end

            _ ->
              led(combined, state, log, min_bp, context, opts)
          end

            {:error, reason} ->
              {:error, reason, state, log}
          end
        else
          {:error, :unexpected_eof, state, log} ->
            {:error, syntax_error_before(dot_meta), state, log}

          {:error, reason, state, log} ->
            {:error, reason, state, log}
        end
    end
  end

  defp led_bracket(left, state, log, min_bp, context, opts) do
    # Check allows_bracket flag (defaults to true for backward compatibility)
    allows_bracket = Keyword.get(opts, :allows_bracket, true)

    if not allows_bracket do
      # Space before [ means this is NOT bracket access, but a no-parens call with list arg
      # Return left as-is (no-parens call parsing will be handled by caller)
      {:ok, left, state, log}
    else
      {:ok, open_tok, state} = TokenAdapter.next(state)

      # Skip leading EOE and count newlines (only leading newlines matter for metadata)
      {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

      with {:ok, indices, state, log} <- parse_access_indices([], state, context, log) do
        # Skip trailing EOE before close bracket (don't count these)
        {state, _trailing_newlines} = EOE.skip_count_newlines(state, 0)

        case TokenAdapter.next(state) do
          {:ok, %{kind: :"]"} = close_tok, state} ->
            # Build metadata with from_brackets, newlines, closing, line, column
            open_meta = build_meta(open_tok.metadata)
            close_meta = build_meta(close_tok.metadata)

            bracket_meta =
              Meta.closing_meta(open_meta, close_meta, leading_newlines, from_brackets: true)

            combined =
              {{:., bracket_meta, [Access, :get]}, bracket_meta, [left | Enum.reverse(indices)]}

            led(combined, state, log, min_bp, context, opts)

          {:ok, other, state} ->
            {:error, {:expected, :"]", got: other.kind}, state, log}

          {:eof, state} ->
            {:error, :unexpected_eof, state, log}

          {:error, diag, state} ->
            {:error, diag, state, log}
        end
      end
    end
  end

  defp led_binary(left, state, log, min_bp, context, opts, bp, assoc, newlines_before_op) do
    {:ok, op_token, state} = TokenAdapter.next(state)
    # For right associativity, use same bp to allow chaining at same level
    # For left associativity, use bp+1 to prevent same-level ops from binding to RHS
    rhs_min_bp = if assoc == :right, do: bp, else: bp + 1

    # Skip newlines after operator (allows "1 +\n2")
    # Note: We only skip newlines, not semicolons. "1 +;\n2" should be a syntax error.
    # Newlines can come from:
    # 1. EOE after the operator (continuation: "1 +\n2")
    # 2. The operator token itself (continuation operator: "a\n|> b")
    # 3. EOE before the operator (for pipe_op in map update: "%{base\n| key: val}")
    # We take the max of all sources
    {state, newlines_after_op} = EOE.skip_newlines_only(state, 0)
    token_newlines = Map.get(op_token.metadata, :newlines, 0)
    effective_newlines = Enum.max([newlines_after_op, token_newlines, newlines_before_op])

    case TokenAdapter.peek(state) do
      # Special case: when operator followed by keyword list
      # Grammar rule: no_parens_op_expr -> when_op_eol call_args_no_parens_kw
      # This only applies when `no_parens_expr` extension is enabled.
      {:ok, _rhs_tok, _} when op_token.kind == :when_op and context.allow_no_parens_expr ->
        case Keywords.try_parse_call_args_no_parens_kw(state, context, log) do
          {:ok, kw_list, state, log} ->
            op = op_token.value
            meta = build_meta_with_newlines(op_token.metadata, effective_newlines)
            combined = Builder.Helpers.binary(op, left, kw_list, meta)
            led(combined, state, log, min_bp, context, opts)

          {:no_kw, state, log} ->
            # Parse RHS normally - handle keyword_key from quoted keywords
            parse_when_binary_rhs(
              state,
              left,
              op_token,
              rhs_min_bp,
              min_bp,
              effective_newlines,
              context,
              log
            )

          {:error, reason, state, log} ->
            {:error, reason, state, log}
        end

      # Special case: assoc_op (=>) in map context needs to parse full expression as value
      # Grammar rule: assoc_expr -> container_expr assoc_op_eol container_expr
      # The value of => should be a container_expr which includes pipe_op
      # So use min_bp=0 to allow | and other low-precedence operators in the value
      {:ok, _rhs_tok, _} when op_token.kind == :assoc_op ->
        parse_binary_rhs(
          state,
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
  defp maybe_nested_call_or_do_block(ast, state, log, min_bp, context, opts) do
    unary_operand = Keyword.get(opts, :unary_operand, false)

    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"("}, _} ->
        # Another paren call - parse it
        {:ok, _open_tok, state} = TokenAdapter.next(state)

        # Skip leading EOE and count newlines
        {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

        with {:ok, args, state, log} <-
               ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, context, log),
             {:ok, close_meta, trailing_newlines, state} <- Meta.consume_closing(state, :")") do
          total_newlines = Meta.total_newlines(leading_newlines, trailing_newlines, args == [])
          # Get line/column from callee AST
          callee_meta = extract_meta(ast)
          meta = Meta.closing_meta(callee_meta, close_meta, total_newlines)

          combined = {ast, meta, Enum.reverse(args)}
          # Recurse for chained calls like foo()()()
          maybe_nested_call_or_do_block(combined, state, log, min_bp, context, opts)
        else
          other -> Result.normalize_error(other, log)
        end

      {:ok, %{kind: :do}, _} ->
        # When allow_do_block is false (e.g. parsing no-parens call arguments), do-blocks
        # belong to the outer call (e.g. `foo bar do ... end`) and must NOT attach to
        # the final argument expression (e.g. `bar do ... end`).
        if not Context.allow_do_block?(context) do
          led(ast, state, log, min_bp, context)
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
            with {:ok, {block_meta, sections}, state, log} <-
                   Blocks.parse_do_block(state, context, log) do
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

              led(combined, state, log, led_min_bp, context, led_opts)
            end
          else
            led(ast, state, log, min_bp, context)
          end
        end

      _ ->
        # No nested call or do-block - continue with led
        led(ast, state, log, min_bp, context)
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
  # Returns {:ok, args, leading_newlines, close_meta, state, log}.
  defp parse_dot_container_args(state, _ctx, log) do
    # Skip leading EOE and count newlines (only leading newlines matter for metadata).
    {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

    container_ctx = Context.container_expr()

    # `container_args` does NOT allow starting with kw_data (including quoted keys).
    # Ensure we preserve the legacy behavior for `a: 1` (kw_identifier), and fix
    # the quoted-key case where `'a': 1` previously parsed as a keyword list.
    with {:ok, state, log} <- reject_initial_kw_data(state, container_ctx, log) do
      item_fun = fn state, _ctx, log ->
        case Keywords.try_parse_kw_data(state, container_ctx, log) do
          {:ok, kw_list, state, log} ->
            {state, _newlines} = EOE.skip_count_newlines(state, 0)

            case TokenAdapter.peek(state) do
              {:ok, %{kind: :"}"}, _} ->
                {:ok, {:kw_data, kw_list}, state, log}

              {:ok, %{kind: kind}, state} ->
                {:error, {:expected, :"}", got: kind}, state, log}

              {:eof, state} ->
                {:error, :unexpected_eof, state, log}

              {:error, diag, state} ->
                {:error, diag, state, log}
            end

          {:no_kw, state, log} ->
            with {:ok, expr, state, log} <- Expressions.expr(state, container_ctx, log) do
              {:ok, {:expr, expr}, state, log}
            end

          {:error, reason, state, log} ->
            {:error, reason, state, log}
        end
      end

      with {:ok, tagged_items, state, log} <-
             Delimited.parse_comma_separated(state, container_ctx, log, :"}", item_fun,
               allow_empty?: true
             ) do
        # Skip trailing EOE before close curly (don't count these).
        {state, _trailing_newlines} = EOE.skip_count_newlines(state, 0)

        case TokenAdapter.next(state) do
          {:ok, %{kind: :"}"} = close_tok, state} ->
            close_meta = build_meta(close_tok.metadata)

            {:ok, finalize_dot_container_items(tagged_items), leading_newlines, close_meta, state,
             log}

          {:ok, other, state} ->
            {:error, {:expected, :"}", got: other.kind}, state, log}

          {:eof, state} ->
            {:error, :unexpected_eof, state, log}

          {:error, diag, state} ->
            {:error, diag, state, log}
        end
      end
    end
  end

  defp reject_initial_kw_data(state, container_ctx, log) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :"}"}, state} ->
        {:ok, state, log}

      {:ok, tok, state} ->
        {ref, checkpoint_state} = TokenAdapter.checkpoint(state)

        case Keywords.try_parse_kw_data(checkpoint_state, container_ctx, log) do
          {:ok, kw_list, state, log} ->
            state = TokenAdapter.rewind(state, ref)
            meta = build_meta(tok.metadata)
            token_display = kw_data_first_key_display(kw_list)
            {:error, syntax_error_before(meta, token_display), state, log}

          {:no_kw, state, log} ->
            {:ok, TokenAdapter.rewind(state, ref), log}

          {:error, reason, state, log} ->
            {:error, reason, TokenAdapter.rewind(state, ref), log}
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
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

  # Peek past EOE tokens without consuming them permanently
  # Returns {state_after_eoe, eoe_tokens_list, newlines_count}
  # The eoe_tokens_list can be used to push back if we don't find a continuation operator
  defp peek_past_eoe(state, eoe_tokens \\ [], newlines \\ 0) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :eoe, value: %{newlines: n}} = eoe_tok, _} ->
        {:ok, _eoe, state} = TokenAdapter.next(state)
        peek_past_eoe(state, [eoe_tok | eoe_tokens], newlines + n)

      _ ->
        {state, Enum.reverse(eoe_tokens), newlines}
    end
  end

  # Push back a list of EOE tokens so the first token read is first in lookahead.
  # Since pushback adds to the FRONT of lookahead, we need to push in reverse order:
  # For [newline, semicolon], push semicolon first, then newline, so lookahead is [newline, semicolon].
  defp pushback_eoe_tokens(state, []), do: state

  defp pushback_eoe_tokens(state, eoe_tokens) do
    Enum.reduce(Enum.reverse(eoe_tokens), state, fn tok, acc ->
      TokenAdapter.pushback(acc, tok)
    end)
  end

  # Helper to parse RHS of `when` binary operator
  # Handles both regular RHS and keyword_key returns from quoted keywords
  defp parse_when_binary_rhs(state, left, op_token, rhs_min_bp, min_bp, newlines, context, log) do
    # Restrict context for RHS parsing to prevent nested operator bypass
    rhs_context = restrict_no_parens_extension(context)

    case TokenAdapter.next(state) do
      {:ok, rhs_token, state} ->
        case parse_rhs(rhs_token, state, rhs_context, log, rhs_min_bp) do
          {:ok, right, state, log} ->
            combined = build_binary_op(op_token, left, right, newlines)
            # Continuation uses original context
            led(combined, state, log, min_bp, context)

          # String was a quoted keyword key - parse value and handle as keyword list RHS
          {:keyword_key, key_atom, state, log} ->
            state = EOE.skip(state)

            with {:ok, value_ast, state, log} <-
                   parse_with_min_bp(state, rhs_context, log, rhs_min_bp) do
              kw_list = [{key_atom, value_ast}]
              # Check for more keywords after comma
              handle_when_keyword_continuation(
                kw_list,
                left,
                op_token,
                min_bp,
                newlines,
                state,
                context,
                log
              )
            end

          {:keyword_key_interpolated, parts, kind, start_meta, delimiter, state, log} ->
            state = EOE.skip(state)

            with {:ok, value_ast, state, log} <-
                   parse_with_min_bp(state, rhs_context, log, rhs_min_bp) do
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
                context,
                log
              )
            end

          {:error, reason, state, log} ->
            {:error, reason, state, log}
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
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
         context,
         log
       ) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :","}, _} ->
        {:ok, _comma, state} = TokenAdapter.next(state)
        state = EOE.skip(state)

        case TokenAdapter.peek(state) do
          {:ok, tok, _} ->
            cond do
              Keywords.starts_kw?(tok) ->
                with {:ok, more_kw, state, log} <-
                       Keywords.parse_kw_no_parens_call(state, context, log) do
                  combined = build_binary_op(op_token, left, kw_list ++ more_kw, newlines)
                  led(combined, state, log, min_bp, context)
                end

              tok.kind in [:list_string_start, :bin_string_start] ->
                # Potentially another quoted keyword - parse via expression
                with {:ok, expr, state, log} <- Expressions.expr(state, context, log) do
                  if is_keyword_list_result(expr) do
                    # It was a keyword list - merge and continue
                    handle_when_keyword_continuation(
                      kw_list ++ expr,
                      left,
                      op_token,
                      min_bp,
                      newlines,
                      state,
                      context,
                      log
                    )
                  else
                    # Not a keyword - finalize current kw_list
                    combined = build_binary_op(op_token, left, kw_list, newlines)
                    led(combined, state, log, min_bp, context)
                  end
                end

              true ->
                # Not a keyword - finalize
                combined = build_binary_op(op_token, left, kw_list, newlines)
                led(combined, state, log, min_bp, context)
            end

          _ ->
            combined = build_binary_op(op_token, left, kw_list, newlines)
            led(combined, state, log, min_bp, context)
        end

      _ ->
        combined = build_binary_op(op_token, left, kw_list, newlines)
        led(combined, state, log, min_bp, context)
    end
  end

  # Helper to parse RHS of binary operator
  defp parse_binary_rhs(
         state,
         left,
         op_token,
         rhs_min_bp,
         min_bp,
         newlines,
         context,
         log,
         opts
       ) do
    # For RHS parsing, we don't want to allow no_parens_expr extension.
    # The allow_no_parens_expr flag should only affect the TOP level of expression parsing,
    # not nested operands. This ensures proper precedence handling in chains like
    # "for x when is_integer(x) <- xs" where <- should NOT be consumed inside when's RHS.
    rhs_context = restrict_no_parens_extension(context)

    case TokenAdapter.next(state) do
      {:ok, rhs_token, state} ->
        rhs_opts = Keyword.put(opts, :allow_no_parens_extension?, false)

        with {:ok, right, state, log} <-
               parse_rhs(rhs_token, state, rhs_context, log, rhs_min_bp, rhs_opts) do
          combined = build_binary_op(op_token, left, right, newlines)
          # Continuation uses original context to allow further extension at outer level
          led(combined, state, log, min_bp, context, opts)
        end

      {:eof, state} ->
        {:error, :unexpected_eof, state, log}

      {:error, diag, state} ->
        {:error, diag, state, log}
    end
  end

  # Restrict context to prevent no_parens_expr extension in nested operands
  defp restrict_no_parens_extension(%Context{} = ctx), do: %{ctx | allow_no_parens_expr: false}

  # Build binary operation AST, with special handling for "not in" rewrite
  # "not in" gets rewritten to {:not, NotMeta, [{:in, InMeta, [left, right]}]}
  defp build_binary_op(
         %{kind: :in_op, value: {:"not in", in_location}, metadata: meta},
         left,
         right,
         newlines
       ) do
    not_meta = build_meta_with_newlines(meta, newlines)
    in_meta = build_meta_from_location(in_location)
    {:not, not_meta, [{:in, in_meta, [left, right]}]}
  end

  # Deprecated "not expr1 in expr2" rewrite - when in_op follows {:not, _, [operand]} or {:!, _, [operand]}
  # Rewrites to {:not, InMeta, [{:in, InMeta, [operand, right]}]} or {:!, InMeta, [{:in, InMeta, [operand, right]}]}
  defp build_binary_op(
         %{kind: :in_op, value: :in, metadata: meta},
         {op, _op_meta, [operand]},
         right,
         _newlines
       )
       when op in [:not, :!] do
    in_meta = build_meta(meta)
    {op, in_meta, [{:in, in_meta, [operand, right]}]}
  end

  # Range ternary rewrite: a .. (b // c) -> a ..// b // c
  defp build_binary_op(
         %{kind: :range_op, metadata: meta},
         left,
         {:"//", _meta, [stop, step]},
         newlines
       ) do
    range_meta = build_meta_with_newlines(meta, newlines)
    {:..//, range_meta, [left, stop, step]}
  end

  # Assoc operator (=>) - just build as normal binary op
  # Note: Elixir doesn't add any special metadata for => operator
  defp build_binary_op(%{kind: :assoc_op, metadata: meta}, left, right, newlines) do
    op_meta = build_meta_with_newlines(meta, newlines)
    Builder.Helpers.binary(:"=>", left, right, op_meta)
  end

  defp build_binary_op(op_token, left, right, newlines) do
    op = op_token.value
    meta = build_meta_with_newlines(op_token.metadata, newlines)
    Builder.Helpers.binary(op, left, right, meta)
  end

  # Build metadata from a raw Toxic location tuple
  defp build_meta_from_location({{line, column}, _, _}) do
    [line: line, column: column]
  end

  defp build_meta_from_location(_), do: []

  # Integer: extract parsed value from raw token metadata
  defp literal_to_ast(%{kind: :int, raw: {:int, {_, _, parsed_value}, _}}) do
    parsed_value
  end

  # Float: extract parsed value from raw token metadata
  defp literal_to_ast(%{kind: :flt, raw: {:flt, {_, _, parsed_value}, _}}) do
    parsed_value
  end

  # Character literal: value is already the codepoint
  defp literal_to_ast(%{kind: :char, value: codepoint}) do
    codepoint
  end

  # Boolean literals: kind is the literal value itself
  defp literal_to_ast(%{kind: true}), do: true
  defp literal_to_ast(%{kind: false}), do: false
  defp literal_to_ast(%{kind: nil}), do: nil

  # Atom: value is already the atom
  defp literal_to_ast(%{kind: :atom, value: atom}) do
    atom
  end

  # Alias: wrap in __aliases__ tuple with :last metadata
  defp literal_to_ast(%{kind: :alias, value: atom, metadata: meta}) do
    m = build_meta(meta)
    {:__aliases__, [last: m] ++ m, [atom]}
  end

  # Identifier: wrap in tuple with nil context (variable reference)
  defp literal_to_ast(%{kind: :identifier, value: atom, metadata: meta}) do
    {atom, build_meta(meta), nil}
  end

  # Paren identifier: same as identifier, but mark it so led() knows to handle (
  # Add paren_call: true marker so led() can distinguish if(a) from if (a)
  defp literal_to_ast(%{kind: :paren_identifier, value: atom, metadata: meta}) do
    {atom, [paren_call: true] ++ build_meta(meta), nil}
  end

  # Do identifier: same as identifier
  defp literal_to_ast(%{kind: :do_identifier, value: atom, metadata: meta}) do
    {atom, build_meta(meta), nil}
  end

  # Bracket identifier: same as identifier, [ will be handled by led
  defp literal_to_ast(%{kind: :bracket_identifier, value: atom, metadata: meta}) do
    {atom, build_meta(meta), nil}
  end

  # Op identifier: same as identifier, for no-parens calls
  defp literal_to_ast(%{kind: :op_identifier, value: atom, metadata: meta}) do
    {atom, build_meta(meta), nil}
  end

  # String (for future use)
  defp literal_to_ast(%{kind: :string, value: value}) do
    value
  end

  # Range operator (..) as standalone expression
  defp literal_to_ast(%{kind: :range_op, value: op, metadata: meta}) do
    {op, build_meta(meta), []}
  end

  # Ellipsis operator (...) as standalone expression
  defp literal_to_ast(%{kind: :ellipsis_op, value: op, metadata: meta}) do
    {op, build_meta(meta), []}
  end

  # Fallback for other tokens
  defp literal_to_ast(%{value: value}) do
    value
  end

  # Build metadata for AST nodes
  defp build_meta(%{range: %{start: %{line: line, column: column}}}) do
    [line: line, column: column]
  end

  defp build_meta(_), do: []

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
  defp build_meta_with_newlines(token_meta, 0) do
    build_meta(token_meta)
  end

  defp build_meta_with_newlines(token_meta, newlines) when newlines > 0 do
    [newlines: newlines] ++ build_meta(token_meta)
  end

  defp parse_access_indices(acc, state, _ctx, log) do
    case Brackets.parse_bracket_arg_no_skip(state, log) do
      {:ok, arg, state, log} -> {:ok, [arg | acc], state, log}
      {:error, reason, state, log} -> {:error, reason, state, log}
    end
  end

  # Specialized led that ONLY handles dot operator for alias chaining.
  # Used by parse_base_with_dots for struct names like %Foo.Bar{}.
  # Does NOT handle paren calls, no-parens calls, bracket access, or do-blocks.
  defp led_dot_only(left, state, log, context) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :dot_op} = _dot_tok, _} ->
        {:ok, dot_tok, state} = TokenAdapter.next(state)
        dot_meta = build_meta(dot_tok.metadata)

        # Parse the RHS of dot - must be an alias or identifier
        # Pass dot_meta for curly calls where the call metadata uses the dot's position
        with {:ok, rhs, state, log} <- Dots.parse_member(state, context, log, dot_meta) do
          combined_result =
            case rhs do
              # Alias on RHS with bracket access (ignored here, just unwrap)
              {{:__aliases__, rhs_meta, [rhs_alias]}, :allows_bracket} ->
                build_dot_alias(left, rhs_alias, rhs_meta, dot_meta)

              # Alias on RHS - build combined __aliases__ (dot_alias rule)
              {:__aliases__, rhs_meta, [rhs_alias]} ->
                build_dot_alias(left, rhs_alias, rhs_meta, dot_meta)

              # Paren call on RHS (tokenized as dot_paren_identifier)
              # Build as a call on the dotted target: Foo.bar() => {{:., ..., [Foo, :bar]}, meta, args}
              {name, meta, args} when is_atom(name) and is_list(args) ->
                {:ok, {{:., dot_meta, [left, name]}, meta, args}}

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
              # Continue to handle chained dots: Foo.Bar.Baz
              led_dot_only(combined, state, log, context)

            {:error, reason} ->
              {:error, reason, state, log}
          end
        end

      _ ->
        # Not a dot operator - return what we have
        {:ok, left, state, log}
    end
  end

  @doc """
  Specialized led that handles dot operators AND paren calls, but stops at `{`.
  Used by parse_base_with_dots_and_calls for struct names like %Foo.Bar{} and %unquote(struct){}.
  Also used by Maps.parse_map_base_expr for unary operators in struct context.
  """
  @spec led_dots_and_calls(Macro.t(), State.t(), EventLog.t(), context()) :: result()
  def led_dots_and_calls(left, %State{} = state, %EventLog{} = log, %Context{} = context) do
    case TokenAdapter.peek(state) do
      {:ok, %{kind: :dot_op} = _dot_tok, _} ->
        {:ok, dot_tok, state} = TokenAdapter.next(state)
        dot_meta = build_meta(dot_tok.metadata)

        # Parse the RHS of dot - must be an alias or identifier
        # Pass dot_meta for curly calls where the call metadata uses the dot's position
        with {:ok, rhs, state, log} <- Dots.parse_member(state, context, log, dot_meta) do
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
              led_dots_and_calls(combined, state, log, context)

            {:error, reason} ->
              {:error, reason, state, log}
          end
        end

      # Handle paren calls: foo(args) or unquote(struct)
      {:ok, %{kind: :"("}, _} ->
        {:ok, _open_tok, state} = TokenAdapter.next(state)

        # Skip leading EOE and count newlines
        {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

        with {:ok, args, state, log} <-
               ToxicParser.Grammar.CallsPrivate.parse_paren_args([], state, context, log) do
          with {:ok, close_meta, trailing_newlines, state} <- Meta.consume_closing(state, :")") do
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
            led_dots_and_calls(combined, state, log, context)
          else
            other -> Result.normalize_error(other, log)
          end
        end

      # Handle bracket access: 0[l] for structs like %0[l]{}
      # Grammar: bracket_expr -> access_expr bracket_arg
      {:ok, %{kind: :"["}, _} ->
        {:ok, open_tok, state} = TokenAdapter.next(state)

        # Skip leading EOE and count newlines
        {state, leading_newlines} = EOE.skip_count_newlines(state, 0)

        with {:ok, indices, state, log} <- parse_access_indices([], state, context, log) do
          # Skip trailing EOE before close bracket
          {state, _trailing_newlines} = EOE.skip_count_newlines(state, 0)

          case TokenAdapter.next(state) do
            {:ok, %{kind: :"]"} = close_tok, state} ->
              # Build bracket access AST like led_bracket does
              open_meta = build_meta(open_tok.metadata)
              close_meta = build_meta(close_tok.metadata)

              bracket_meta =
                Meta.closing_meta(open_meta, close_meta, leading_newlines, from_brackets: true)

              combined =
                {{:., bracket_meta, [Access, :get]}, bracket_meta, [left | Enum.reverse(indices)]}

              # Continue to handle more chained operations
              led_dots_and_calls(combined, state, log, context)

            {:ok, other, state} ->
              {:error, {:expected, :"]", got: other.kind}, state, log}

            {:eof, state} ->
              {:error, :unexpected_eof, state, log}

            {:error, diag, state} ->
              {:error, diag, state, log}
          end
        end

      _ ->
        # Not a dot, paren, or bracket - return what we have
        {:ok, left, state, log}
    end
  end
end
