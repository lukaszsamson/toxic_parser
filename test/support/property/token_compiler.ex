defmodule ToxicParser.Property.TokenCompiler do
  @moduledoc """
  Compiles grammar trees to Toxic token lists.

  This module takes grammar tree nodes (defined in `GrammarTree`) and produces
  linear Toxic streaming tokens that can be rendered to source code.
  """

  alias ToxicParser.Property.TokenLayout

  @doc """
  Compile a grammar tree to a list of Toxic tokens.
  """
  @spec to_tokens(term(), keyword()) :: [Toxic.token()]
  def to_tokens(tree, opts \\ []) do
    layout = TokenLayout.new()

    {tokens, _layout} = do_to_tokens(tree, layout, opts)
    tokens
  end

  # ===========================================================================
  # Token compiler: do_to_tokens/3
  # ===========================================================================

  # Top-level grammar (legacy format with implicit newline separators)
  defp do_to_tokens({:grammar, forms}, layout, opts) do
    compile_forms(forms, layout, opts)
  end

  # Top-level grammar with explicit eoe markers (legacy format)
  defp do_to_tokens({:grammar_eoe, forms_with_eoe}, layout, opts) do
    compile_forms_with_eoe(forms_with_eoe, layout, opts)
  end

  # Top-level grammar v2 format per elixir_parser.yrl grammar rules:
  # - leading_eoe: optional eoe before expr_list
  # - exprs: list of {expr, eoe | nil} where eoe is between exprs (last has nil)
  # - trailing_eoe: optional eoe after expr_list
  defp do_to_tokens({:grammar_v2, leading_eoe, exprs, trailing_eoe}, layout, opts) do
    # Emit leading eoe if present
    {leading_tokens, layout} =
      if leading_eoe do
        compile_eoe(leading_eoe, layout)
      else
        {[], layout}
      end

    # Compile expressions with eoe between them
    {expr_tokens, layout} = compile_expr_list(exprs, layout, opts)

    # Emit trailing eoe if present
    {trailing_tokens, layout} =
      if trailing_eoe do
        compile_eoe(trailing_eoe, layout)
      else
        {[], layout}
      end

    {leading_tokens ++ expr_tokens ++ trailing_tokens, layout}
  end

  # ---------------------------------------------------------------------------
  # Literals
  # ---------------------------------------------------------------------------

  defp do_to_tokens({:int, value, _format, chars}, layout, _opts) do
    lexeme = List.to_string(chars)
    {meta, layout} = TokenLayout.space_before(layout, lexeme, value)
    {[{:int, meta, chars}], layout}
  end

  defp do_to_tokens({:float, value, chars}, layout, _opts) do
    lexeme = List.to_string(chars)
    {meta, layout} = TokenLayout.space_before(layout, lexeme, value)
    {[{:flt, meta, chars}], layout}
  end

  defp do_to_tokens({:char, codepoint, chars}, layout, _opts) do
    lexeme = List.to_string(chars)
    {meta, layout} = TokenLayout.space_before(layout, lexeme, chars)
    {[{:char, meta, codepoint}], layout}
  end

  defp do_to_tokens({:atom_lit, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    lexeme = ":" <> name
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.space_before(layout, lexeme, chars)
    {[{:atom, meta, atom}], layout}
  end

  defp do_to_tokens({:bool_lit, true}, layout, _opts) do
    {meta, layout} = TokenLayout.space_before(layout, "true", nil)
    {[{true, meta}], layout}
  end

  defp do_to_tokens({:bool_lit, false}, layout, _opts) do
    {meta, layout} = TokenLayout.space_before(layout, "false", nil)
    {[{false, meta}], layout}
  end

  defp do_to_tokens(:nil_lit, layout, _opts) do
    {meta, layout} = TokenLayout.space_before(layout, "nil", nil)
    {[{nil, meta}], layout}
  end

  # ---------------------------------------------------------------------------
  # Identifiers and Aliases
  # ---------------------------------------------------------------------------

  defp do_to_tokens({:identifier, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.space_before(layout, name, chars)
    {[{:identifier, meta, atom}], layout}
  end

  # Do identifier (if, unless, case, etc.) used as bare identifier
  # Per grammar: dot_do_identifier -> do_identifier
  defp do_to_tokens({:do_identifier, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.space_before(layout, name, chars)
    {[{:do_identifier, meta, atom}], layout}
  end

  # Op identifier (operator used as identifier, e.g., in `def +/2`)
  defp do_to_tokens({:op_identifier, op}, layout, _opts) do
    name = Atom.to_string(op)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.space_before(layout, name, chars)
    {[{:op_identifier, meta, op}], layout}
  end

  defp do_to_tokens({:alias, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.space_before(layout, name, chars)
    {[{:alias, meta, atom}], layout}
  end

  # Dotted alias: Foo.Bar.Baz
  # Per grammar: dot_alias -> matched_expr dot_op alias
  defp do_to_tokens({:dot_alias, segments}, layout, _opts) when is_list(segments) do
    compile_alias_segments(segments, layout, true)
  end

  # Dotted identifier: expr.identifier (e.g., foo.bar, Mod.func)
  # Per grammar: dot_identifier -> matched_expr dot_op identifier
  defp do_to_tokens({:dot_identifier, left, right_name}, layout, opts) do
    # Compile left expression
    {left_tokens, layout} = do_to_tokens(left, layout, opts)

    # Compile dot (stuck to left for adhesion)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile right identifier (stuck to dot for adhesion)
    right_str = Atom.to_string(right_name)
    chars = String.to_charlist(right_str)
    {right_meta, layout} = TokenLayout.stick_right(layout, right_str, chars)
    right_token = {:identifier, right_meta, right_name}

    {left_tokens ++ [dot_token, right_token], layout}
  end

  # Dotted operator identifier: matched_expr . op_identifier
  defp do_to_tokens({:dot_op_identifier, left, op}, layout, opts) do
    # Compile left expression
    {left_tokens, layout} = do_to_tokens(left, layout, opts)

    # Compile dot (stuck to left for adhesion)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile op identifier (stuck to dot for adhesion)
    name = Atom.to_string(op)
    chars = String.to_charlist(name)
    {op_meta, layout} = TokenLayout.stick_right(layout, name, chars)
    op_token = {:op_identifier, op_meta, op}

    {left_tokens ++ [dot_token, op_token], layout}
  end

  # Dotted do_identifier: expr.do_identifier (e.g., Foo.if, Mod.case)
  # Per grammar: dot_do_identifier -> matched_expr dot_op do_identifier
  defp do_to_tokens({:dot_do_identifier, left, do_id}, layout, opts) do
    # Compile left expression
    {left_tokens, layout} = do_to_tokens(left, layout, opts)

    # Compile dot (stuck to left for adhesion)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile do_identifier (stuck to dot for adhesion)
    id_str = Atom.to_string(do_id)
    chars = String.to_charlist(id_str)
    {id_meta, layout} = TokenLayout.stick_right(layout, id_str, chars)
    id_token = {:do_identifier, id_meta, do_id}

    {left_tokens ++ [dot_token, id_token], layout}
  end

  # ---------------------------------------------------------------------------
  # Binary Operators
  # ---------------------------------------------------------------------------

  # Matched binary operator: left op right (both operands are matched)
  defp do_to_tokens({:matched_op, left, op_eol, right}, layout, opts) do
    compile_binary_op(left, op_eol, right, layout, opts)
  end

  # Unmatched binary operator: left op right (right is unmatched/do-block-bearing)
  defp do_to_tokens({:unmatched_op, left, op_eol, right}, layout, opts) do
    compile_binary_op(left, op_eol, right, layout, opts)
  end

  # Legacy binary operator (backward compatibility)
  # Per V7 Section 2: operators never render newlines from extra,
  # we emit :eol token if newlines > 0
  defp do_to_tokens({:binary_op, left, op_eol, right}, layout, opts) do
    compile_binary_op(left, op_eol, right, layout, opts)
  end

  # ---------------------------------------------------------------------------
  # Unary Operators
  # ---------------------------------------------------------------------------

  # Matched unary operator: op operand (with adhesion)
  # Per grammar: matched_expr -> unary_op_eol matched_expr
  # unary_op_eol -> unary_op | unary_op eol
  defp do_to_tokens({:matched_unary, {op_kind, op}, newlines, operand}, layout, opts)
       when is_integer(newlines) do
    op_lexeme = op_to_lexeme(op)
    {op_meta, layout} = TokenLayout.space_before(layout, op_lexeme, nil)
    op_token = {op_kind, op_meta, op}

    # Emit newlines if any (per unary_op_eol -> unary_op eol)
    {eol_tokens, layout} =
      if newlines > 0 do
        eol_meta = TokenLayout.meta(layout, "\n", newlines)
        layout = TokenLayout.newlines(layout, newlines)
        {[{:eol, eol_meta}], layout}
      else
        {[], layout}
      end

    # Compile operand with adhesion (stuck to operator for matched_unary)
    {operand_tokens, layout} = compile_arg_with_adhesion(operand, layout, opts)

    {[op_token] ++ eol_tokens ++ operand_tokens, layout}
  end

  # Legacy matched_unary without newlines (backward compatibility)
  defp do_to_tokens({:matched_unary, {op_kind, op}, operand}, layout, opts) do
    op_lexeme = op_to_lexeme(op)
    {op_meta, layout} = TokenLayout.space_before(layout, op_lexeme, nil)
    op_token = {op_kind, op_meta, op}

    # Compile operand with adhesion (stuck to operator for matched_unary)
    {operand_tokens, layout} = compile_arg_with_adhesion(operand, layout, opts)

    {[op_token] ++ operand_tokens, layout}
  end

  # Legacy unary operator (backward compatibility - keeps original space behavior)
  defp do_to_tokens({:unary_op, {op_kind, op}, operand}, layout, opts) do
    op_lexeme = op_to_lexeme(op)
    {op_meta, layout} = TokenLayout.space_before(layout, op_lexeme, nil)
    op_token = {op_kind, op_meta, op}

    # Compile operand with space (original behavior)
    {operand_tokens, layout} = do_to_tokens(operand, layout, opts)

    {[op_token] ++ operand_tokens, layout}
  end

  # ---------------------------------------------------------------------------
  # Nullary Operators
  # ---------------------------------------------------------------------------

  # Nullary range operator: ..
  defp do_to_tokens({:nullary_range, nil}, layout, _opts) do
    {meta, layout} = TokenLayout.space_before(layout, "..", nil)
    {[{:range_op, meta, :..}], layout}
  end

  # Nullary ellipsis operator: ...
  defp do_to_tokens({:nullary_ellipsis, nil}, layout, _opts) do
    {meta, layout} = TokenLayout.space_before(layout, "...", nil)
    {[{:ellipsis_op, meta, :...}], layout}
  end

  # ---------------------------------------------------------------------------
  # Range with Step (ternary_op)
  # ---------------------------------------------------------------------------

  # Range with step: left..middle//step
  # Per grammar lines 739-746: ternary_op (//) is only valid immediately after range_op (..)
  # Produces: left .. middle // step
  defp do_to_tokens(
         {:range_step, left, range_newlines, middle, step_newlines, step},
         layout,
         opts
       ) do
    # Compile left operand
    {left_tokens, layout} = do_to_tokens(left, layout, opts)

    # Compile range operator (..)
    {range_meta, layout} = TokenLayout.space_before(layout, "..", nil)
    range_token = {:range_op, range_meta, :..}

    # Handle newlines after range operator
    {range_eol_tokens, layout} =
      if range_newlines > 0 do
        eol_meta = TokenLayout.meta(layout, "\n", range_newlines)
        layout = TokenLayout.newlines(layout, range_newlines)
        {[{:eol, eol_meta}], layout}
      else
        {[], layout}
      end

    # Compile middle operand
    {middle_tokens, layout} = do_to_tokens(middle, layout, opts)

    # Compile ternary operator (//)
    {ternary_meta, layout} = TokenLayout.space_before(layout, "//", nil)
    ternary_token = {:ternary_op, ternary_meta, :"//"}

    # Handle newlines after ternary operator
    {ternary_eol_tokens, layout} =
      if step_newlines > 0 do
        eol_meta = TokenLayout.meta(layout, "\n", step_newlines)
        layout = TokenLayout.newlines(layout, step_newlines)
        {[{:eol, eol_meta}], layout}
      else
        {[], layout}
      end

    # Compile step operand
    {step_tokens, layout} = do_to_tokens(step, layout, opts)

    {left_tokens ++
       [range_token] ++
       range_eol_tokens ++
       middle_tokens ++
       [ternary_token] ++ ternary_eol_tokens ++ step_tokens, layout}
  end

  # ---------------------------------------------------------------------------
  # Matched Op with warn_pipe pattern
  # ---------------------------------------------------------------------------

  # Matched op with warn_pipe: left arrow_op no_parens_one_expr
  # Per grammar line 209: matched_op_expr -> arrow_op_eol no_parens_one_expr : warn_pipe('$1', '$2')
  # This is a valid expression that generates a warning at parse time.
  # We compile it like a regular matched_op - the warning is emitted by the parser, not us.
  defp do_to_tokens({:matched_op_warn_pipe, left, op_eol, right}, layout, opts) do
    compile_binary_op(left, op_eol, right, layout, opts)
  end

  # ---------------------------------------------------------------------------
  # At operator (@expr)
  # ---------------------------------------------------------------------------

  # At operator with optional newline: @\n expr
  # Per grammar: at_op_eol -> at_op | at_op eol
  defp do_to_tokens({:at_op, newlines, operand}, layout, opts) when is_integer(newlines) do
    {at_meta, layout} = TokenLayout.space_before(layout, "@", nil)
    at_token = {:at_op, at_meta, :@}

    # Emit newlines if any (per at_op_eol -> at_op eol)
    {eol_tokens, layout} =
      if newlines > 0 do
        eol_meta = TokenLayout.meta(layout, "\n", newlines)
        layout = TokenLayout.newlines(layout, newlines)
        {[{:eol, eol_meta}], layout}
      else
        {[], layout}
      end

    # Compile operand
    {operand_tokens, layout} = compile_arg_with_adhesion(operand, layout, opts)

    {[at_token] ++ eol_tokens ++ operand_tokens, layout}
  end

  # ---------------------------------------------------------------------------
  # Capture operator (&expr)
  # ---------------------------------------------------------------------------

  # Capture operator with optional newline: &\n expr
  # Per grammar: capture_op_eol -> capture_op | capture_op eol
  defp do_to_tokens({:capture_op, newlines, operand}, layout, opts) when is_integer(newlines) do
    {amp_meta, layout} = TokenLayout.space_before(layout, "&", nil)
    amp_token = {:capture_op, amp_meta, :&}

    # Emit newlines if any (per capture_op_eol -> capture_op eol)
    {eol_tokens, layout} =
      if newlines > 0 do
        eol_meta = TokenLayout.meta(layout, "\n", newlines)
        layout = TokenLayout.newlines(layout, newlines)
        {[{:eol, eol_meta}], layout}
      else
        {[], layout}
      end

    # Compile operand with adhesion
    {operand_tokens, layout} = compile_arg_with_adhesion(operand, layout, opts)

    {[amp_token] ++ eol_tokens ++ operand_tokens, layout}
  end

  # ---------------------------------------------------------------------------
  # Ellipsis prefix operator (...expr)
  # ---------------------------------------------------------------------------

  # Ellipsis as prefix operator: ...expr
  defp do_to_tokens({:ellipsis_prefix, expr}, layout, opts) do
    {ellipsis_meta, layout} = TokenLayout.space_before(layout, "...", nil)
    ellipsis_token = {:ellipsis_op, ellipsis_meta, :...}

    # Compile expression with adhesion
    {expr_tokens, layout} = compile_arg_with_adhesion(expr, layout, opts)

    {[ellipsis_token] ++ expr_tokens, layout}
  end

  # ---------------------------------------------------------------------------
  # Parenthesized Expressions
  # ---------------------------------------------------------------------------

  # Parenthesized expression: (expr)
  defp do_to_tokens({:paren_expr, expr}, layout, opts) do
    # Opening paren
    {open_meta, layout} = TokenLayout.space_before(layout, "(", nil)
    open_token = {:"(", open_meta}

    # Compile expression (stuck to open paren)
    {expr_tokens, layout} = compile_arg_with_adhesion(expr, layout, opts)

    # Closing paren (stuck to expression)
    {close_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    close_token = {:")", close_meta}

    {[open_token] ++ expr_tokens ++ [close_token], layout}
  end

  # Empty parentheses: ()
  defp do_to_tokens({:empty_paren, nil}, layout, _opts) do
    # Opening paren
    {open_meta, layout} = TokenLayout.space_before(layout, "(", nil)
    open_token = {:"(", open_meta}

    # Closing paren (stuck to open paren)
    {close_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    close_token = {:")", close_meta}

    {[open_token, close_token], layout}
  end

  # Handle keyword-only no-parens arguments appearing as an operator-right RHS
  defp do_to_tokens({:kw_args, pairs}, layout, opts) do
    {tokens, layout} = compile_no_parens_kw_pairs(pairs, layout, opts)
    {tokens, layout}
  end

  # ---------------------------------------------------------------------------
  # Bracket Access Expressions
  # ---------------------------------------------------------------------------

  # Bracket access with identifier: foo[bar]
  defp do_to_tokens({:bracket_expr, {:bracket_identifier, name}, arg}, layout, opts) do
    # Compile bracket_identifier
    name_str = Atom.to_string(name)
    chars = String.to_charlist(name_str)
    {id_meta, layout} = TokenLayout.space_before(layout, name_str, chars)
    id_token = {:bracket_identifier, id_meta, name}

    # Compile bracket arg: [key]
    {bracket_tokens, layout} = compile_bracket_arg(arg, layout, opts)

    {[id_token] ++ bracket_tokens, layout}
  end

  # Bracket access with expression: expr[key]
  defp do_to_tokens({:bracket_expr, {:expr, expr}, arg}, layout, opts) do
    # Compile the expression
    {expr_tokens, layout} = do_to_tokens(expr, layout, opts)

    # Compile bracket arg (stuck to expression)
    {bracket_tokens, layout} = compile_bracket_arg_stuck(arg, layout, opts)

    {expr_tokens ++ bracket_tokens, layout}
  end

  # Bracket access with dotted bracket identifier: expr.foo[bar]
  # Represented as {:bracket_expr, {:dot_bracket_identifier, left, name}, arg}
  defp do_to_tokens({:bracket_expr, {:dot_bracket_identifier, left, name}, arg}, layout, opts) do
    # Compile left expression
    {left_tokens, layout} = do_to_tokens(left, layout, opts)

    # Compile dot (stuck to left)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile bracket_identifier (stuck to dot)
    name_str = Atom.to_string(name)
    chars = String.to_charlist(name_str)
    {id_meta, layout} = TokenLayout.stick_right(layout, name_str, chars)
    id_token = {:bracket_identifier, id_meta, name}

    # Compile bracket arg (stuck to identifier)
    {bracket_tokens, layout} = compile_bracket_arg(arg, layout, opts)

    {left_tokens ++ [dot_token, id_token] ++ bracket_tokens, layout}
  end

  # ---------------------------------------------------------------------------
  # Bracket At Expressions (@foo[bar])
  # ---------------------------------------------------------------------------

  # Bracket at with identifier: @foo[bar]
  # Per grammar: bracket_at_expr -> at_op_eol dot_bracket_identifier bracket_arg
  defp do_to_tokens({:bracket_at_expr, newlines, {:bracket_identifier, name}, arg}, layout, opts)
       when is_integer(newlines) do
    # Compile @ operator
    {at_meta, layout} = TokenLayout.space_before(layout, "@", nil)
    at_token = {:at_op, at_meta, :@}

    # Emit newlines if any (per at_op_eol -> at_op eol)
    {eol_tokens, layout} =
      if newlines > 0 do
        eol_meta = TokenLayout.meta(layout, "\n", newlines)
        layout = TokenLayout.newlines(layout, newlines)
        {[{:eol, eol_meta}], layout}
      else
        {[], layout}
      end

    # Compile identifier (stuck to @)
    name_str = Atom.to_string(name)
    chars = String.to_charlist(name_str)
    {id_meta, layout} = TokenLayout.stick_right(layout, name_str, chars)
    id_token = {:bracket_identifier, id_meta, name}

    # Compile bracket arg: [key]
    {bracket_tokens, layout} = compile_bracket_arg(arg, layout, opts)

    {[at_token] ++ eol_tokens ++ [id_token] ++ bracket_tokens, layout}
  end

  # Bracket at with expression: @(expr)[bar]
  # Per grammar: bracket_at_expr -> at_op_eol access_expr bracket_arg
  defp do_to_tokens({:bracket_at_expr, newlines, {:expr, expr}, arg}, layout, opts)
       when is_integer(newlines) do
    # Compile @ operator
    {at_meta, layout} = TokenLayout.space_before(layout, "@", nil)
    at_token = {:at_op, at_meta, :@}

    # Emit newlines if any (per at_op_eol -> at_op eol)
    {eol_tokens, layout} =
      if newlines > 0 do
        eol_meta = TokenLayout.meta(layout, "\n", newlines)
        layout = TokenLayout.newlines(layout, newlines)
        {[{:eol, eol_meta}], layout}
      else
        {[], layout}
      end

    # Compile expression (stuck to @)
    {expr_tokens, layout} = compile_arg_with_adhesion(expr, layout, opts)

    # Compile bracket arg (stuck to expression)
    {bracket_tokens, layout} = compile_bracket_arg_stuck(arg, layout, opts)

    {[at_token] ++ eol_tokens ++ expr_tokens ++ bracket_tokens, layout}
  end

  # Bracket at with dotted bracket identifier: @expr.foo[bar]
  defp do_to_tokens(
         {:bracket_at_expr, newlines, {:dot_bracket_identifier, left, name}, arg},
         layout,
         opts
       )
       when is_integer(newlines) do
    # Compile @ operator
    {at_meta, layout} = TokenLayout.space_before(layout, "@", nil)
    at_token = {:at_op, at_meta, :@}

    # Emit newlines if any (per at_op_eol -> at_op eol)
    {eol_tokens, layout} =
      if newlines > 0 do
        eol_meta = TokenLayout.meta(layout, "\n", newlines)
        layout = TokenLayout.newlines(layout, newlines)
        {[{:eol, eol_meta}], layout}
      else
        {[], layout}
      end

    # Compile left expression
    {left_tokens, layout} = do_to_tokens(left, layout, opts)

    # Compile dot (stuck to left)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile bracket_identifier (stuck to dot)
    name_str = Atom.to_string(name)
    chars = String.to_charlist(name_str)
    {id_meta, layout} = TokenLayout.stick_right(layout, name_str, chars)
    id_token = {:bracket_identifier, id_meta, name}

    # Compile bracket arg (stuck to identifier)
    {bracket_tokens, layout} = compile_bracket_arg(arg, layout, opts)

    {[at_token] ++ eol_tokens ++ left_tokens ++ [dot_token, id_token] ++ bracket_tokens, layout}
  end

  # ---------------------------------------------------------------------------
  # Lists, Tuples, Maps
  # ---------------------------------------------------------------------------

  # List: [elem1, elem2, ...]
  defp do_to_tokens({:list, []}, layout, _opts) do
    # Empty list: []
    {open_meta, layout} = TokenLayout.space_before(layout, "[", nil)
    open_token = {:"[", open_meta}

    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token, close_token], layout}
  end

  defp do_to_tokens({:list, args}, layout, opts) do
    # Opening bracket
    {open_meta, layout} = TokenLayout.space_before(layout, "[", nil)
    open_token = {:"[", open_meta}

    # Compile arguments (first stuck to open bracket)
    {args_tokens, layout} = compile_args_in_brackets(args, layout, opts)

    # Closing bracket (stuck to last arg)
    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token] ++ args_tokens ++ [close_token], layout}
  end

  # Tuple: {elem1, elem2, ...}
  defp do_to_tokens({:tuple, []}, layout, _opts) do
    # Empty tuple: {}
    {open_meta, layout} = TokenLayout.space_before(layout, "{", nil)
    open_token = {:"{", open_meta}

    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[open_token, close_token], layout}
  end

  defp do_to_tokens({:tuple, args}, layout, opts) do
    # Opening curly
    {open_meta, layout} = TokenLayout.space_before(layout, "{", nil)
    open_token = {:"{", open_meta}

    # Compile arguments (first stuck to open curly)
    {args_tokens, layout} = compile_args_in_brackets(args, layout, opts)

    # Closing curly (stuck to last arg)
    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[open_token] ++ args_tokens ++ [close_token], layout}
  end

  # Map: %{key: value, ...} or %{key => value, ...}
  defp do_to_tokens({:map, []}, layout, _opts) do
    # Empty map: %{}
    {map_meta, layout} = TokenLayout.space_before(layout, "%{}", nil)
    map_token = {:%{}, map_meta}

    {[map_token], layout}
  end

  defp do_to_tokens({:map, {:kw, pairs}}, layout, opts) do
    # Map with keyword syntax: %{foo: 1, bar: 2}
    {map_meta, layout} = TokenLayout.space_before(layout, "%{", nil)
    map_token = {:%{}, map_meta}

    # Compile keyword pairs
    {pairs_tokens, layout} = compile_kw_pairs(pairs, layout, opts)

    # Closing curly (stuck to last value)
    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[map_token] ++ pairs_tokens ++ [close_token], layout}
  end

  defp do_to_tokens({:map, {:assoc, pairs}}, layout, opts) do
    # Map with arrow syntax: %{:foo => 1, :bar => 2}
    {map_meta, layout} = TokenLayout.space_before(layout, "%{", nil)
    map_token = {:%{}, map_meta}

    # Compile association pairs
    {pairs_tokens, layout} = compile_assoc_pairs(pairs, layout, opts)

    # Closing curly (stuck to last value)
    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[map_token] ++ pairs_tokens ++ [close_token], layout}
  end

  # Map update with keyword syntax: %{base | key: value}
  defp do_to_tokens({:map_update, {:kw, base_expr, pipe_newlines, pairs}}, layout, opts) do
    # Opening %{
    {map_meta, layout} = TokenLayout.space_before(layout, "%{", nil)
    map_token = {:%{}, map_meta}

    # Base expression (stuck to %{)
    {base_tokens, layout} = compile_arg_with_adhesion(base_expr, layout, opts)

    # Pipe operator with optional newlines
    {pipe_meta, layout} = TokenLayout.space_before(layout, "|", nil)
    pipe_token = {:pipe_op, pipe_meta, :|}
    {eol_tokens, layout} = compile_newlines(pipe_newlines, layout)

    # Keyword pairs
    {pairs_tokens, layout} = compile_kw_pairs(pairs, layout, opts)

    # Closing curly
    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[map_token] ++ base_tokens ++ [pipe_token] ++ eol_tokens ++ pairs_tokens ++ [close_token], layout}
  end

  # Map update with arrow syntax: %{base | key => value}
  defp do_to_tokens({:map_update, {:assoc, base_expr, pipe_newlines, pairs}}, layout, opts) do
    # Opening %{
    {map_meta, layout} = TokenLayout.space_before(layout, "%{", nil)
    map_token = {:%{}, map_meta}

    # Base expression (stuck to %{)
    {base_tokens, layout} = compile_arg_with_adhesion(base_expr, layout, opts)

    # Pipe operator with optional newlines
    {pipe_meta, layout} = TokenLayout.space_before(layout, "|", nil)
    pipe_token = {:pipe_op, pipe_meta, :|}
    {eol_tokens, layout} = compile_newlines(pipe_newlines, layout)

    # Association pairs
    {pairs_tokens, layout} = compile_assoc_pairs(pairs, layout, opts)

    # Closing curly
    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[map_token] ++ base_tokens ++ [pipe_token] ++ eol_tokens ++ pairs_tokens ++ [close_token], layout}
  end

  # ---------------------------------------------------------------------------
  # Struct Literals
  # ---------------------------------------------------------------------------

  # Empty struct: %Foo{}
  defp do_to_tokens({:struct, struct_name, eol_count, []}, layout, opts) do
    # % token
    {percent_meta, layout} = TokenLayout.space_before(layout, "%", nil)
    percent_token = {:%, percent_meta}

    # Struct name (stuck to %)
    {name_tokens, layout} = compile_arg_with_adhesion(struct_name, layout, opts)

    # Optional eol after name
    {eol_tokens, layout} = compile_newlines(eol_count, layout)

    # Empty curly braces
    {open_meta, layout} =
      if eol_count > 0, do: TokenLayout.space_before(layout, "{", nil), else: TokenLayout.stick_right(layout, "{", nil)
    open_token = {:"{", open_meta}

    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[percent_token] ++ name_tokens ++ eol_tokens ++ [open_token, close_token], layout}
  end

  # Struct with keyword syntax: %Foo{key: value}
  defp do_to_tokens({:struct, struct_name, eol_count, {:kw, pairs}}, layout, opts) do
    # % token
    {percent_meta, layout} = TokenLayout.space_before(layout, "%", nil)
    percent_token = {:%, percent_meta}

    # Struct name (stuck to %)
    {name_tokens, layout} = compile_arg_with_adhesion(struct_name, layout, opts)

    # Optional eol after name
    {eol_tokens, layout} = compile_newlines(eol_count, layout)

    # Opening curly
    {open_meta, layout} =
      if eol_count > 0, do: TokenLayout.space_before(layout, "{", nil), else: TokenLayout.stick_right(layout, "{", nil)
    open_token = {:"{", open_meta}

    # Keyword pairs
    {pairs_tokens, layout} = compile_kw_pairs(pairs, layout, opts)

    # Closing curly
    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[percent_token] ++ name_tokens ++ eol_tokens ++ [open_token] ++ pairs_tokens ++ [close_token], layout}
  end

  # Struct with arrow syntax: %Foo{key => value}
  defp do_to_tokens({:struct, struct_name, eol_count, {:assoc, pairs}}, layout, opts) do
    # % token
    {percent_meta, layout} = TokenLayout.space_before(layout, "%", nil)
    percent_token = {:%, percent_meta}

    # Struct name (stuck to %)
    {name_tokens, layout} = compile_arg_with_adhesion(struct_name, layout, opts)

    # Optional eol after name
    {eol_tokens, layout} = compile_newlines(eol_count, layout)

    # Opening curly
    {open_meta, layout} =
      if eol_count > 0, do: TokenLayout.space_before(layout, "{", nil), else: TokenLayout.stick_right(layout, "{", nil)
    open_token = {:"{", open_meta}

    # Association pairs
    {pairs_tokens, layout} = compile_assoc_pairs(pairs, layout, opts)

    # Closing curly
    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[percent_token] ++ name_tokens ++ eol_tokens ++ [open_token] ++ pairs_tokens ++ [close_token], layout}
  end

  # Struct update with keyword: %Foo{s | key: value}
  defp do_to_tokens({:struct, struct_name, eol_count, {:update_kw, base_expr, pipe_newlines, pairs}}, layout, opts) do
    # % token
    {percent_meta, layout} = TokenLayout.space_before(layout, "%", nil)
    percent_token = {:%, percent_meta}

    # Struct name (stuck to %)
    {name_tokens, layout} = compile_arg_with_adhesion(struct_name, layout, opts)

    # Optional eol after name
    {eol_tokens, layout} = compile_newlines(eol_count, layout)

    # Opening curly
    {open_meta, layout} =
      if eol_count > 0, do: TokenLayout.space_before(layout, "{", nil), else: TokenLayout.stick_right(layout, "{", nil)
    open_token = {:"{", open_meta}

    # Base expression (stuck to {)
    {base_tokens, layout} = compile_arg_with_adhesion(base_expr, layout, opts)

    # Pipe operator with optional newlines
    {pipe_meta, layout} = TokenLayout.space_before(layout, "|", nil)
    pipe_token = {:pipe_op, pipe_meta, :|}
    {pipe_eol_tokens, layout} = compile_newlines(pipe_newlines, layout)

    # Keyword pairs
    {pairs_tokens, layout} = compile_kw_pairs(pairs, layout, opts)

    # Closing curly
    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[percent_token] ++ name_tokens ++ eol_tokens ++ [open_token] ++ base_tokens ++ [pipe_token] ++ pipe_eol_tokens ++ pairs_tokens ++ [close_token], layout}
  end

  # Struct update with arrow: %Foo{s | key => value}
  defp do_to_tokens({:struct, struct_name, eol_count, {:update_assoc, base_expr, pipe_newlines, pairs}}, layout, opts) do
    # % token
    {percent_meta, layout} = TokenLayout.space_before(layout, "%", nil)
    percent_token = {:%, percent_meta}

    # Struct name (stuck to %)
    {name_tokens, layout} = compile_arg_with_adhesion(struct_name, layout, opts)

    # Optional eol after name
    {eol_tokens, layout} = compile_newlines(eol_count, layout)

    # Opening curly
    {open_meta, layout} =
      if eol_count > 0, do: TokenLayout.space_before(layout, "{", nil), else: TokenLayout.stick_right(layout, "{", nil)
    open_token = {:"{", open_meta}

    # Base expression (stuck to {)
    {base_tokens, layout} = compile_arg_with_adhesion(base_expr, layout, opts)

    # Pipe operator with optional newlines
    {pipe_meta, layout} = TokenLayout.space_before(layout, "|", nil)
    pipe_token = {:pipe_op, pipe_meta, :|}
    {pipe_eol_tokens, layout} = compile_newlines(pipe_newlines, layout)

    # Association pairs
    {pairs_tokens, layout} = compile_assoc_pairs(pairs, layout, opts)

    # Closing curly
    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[percent_token] ++ name_tokens ++ eol_tokens ++ [open_token] ++ base_tokens ++ [pipe_token] ++ pipe_eol_tokens ++ pairs_tokens ++ [close_token], layout}
  end

  # ---------------------------------------------------------------------------
  # Anonymous Struct Maps (expr.%{})
  # ---------------------------------------------------------------------------

  # Empty anonymous struct: expr.%{}
  defp do_to_tokens({:anon_struct, base_expr, dot_newlines, []}, layout, opts) do
    # Base expression
    {base_tokens, layout} = do_to_tokens(base_expr, layout, opts)

    # Dot (stuck to base)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Optional eol after dot
    {eol_tokens, layout} = compile_newlines(dot_newlines, layout)

    # %{} (empty map token)
    {map_meta, layout} =
      if dot_newlines > 0, do: TokenLayout.space_before(layout, "%{}", nil), else: TokenLayout.stick_right(layout, "%{}", nil)
    map_token = {:%{}, map_meta}

    {base_tokens ++ [dot_token] ++ eol_tokens ++ [map_token], layout}
  end

  # Anonymous struct with keyword syntax: expr.%{key: value}
  defp do_to_tokens({:anon_struct, base_expr, dot_newlines, {:kw, pairs}}, layout, opts) do
    # Base expression
    {base_tokens, layout} = do_to_tokens(base_expr, layout, opts)

    # Dot (stuck to base)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Optional eol after dot
    {eol_tokens, layout} = compile_newlines(dot_newlines, layout)

    # %{ (map open)
    {map_meta, layout} =
      if dot_newlines > 0, do: TokenLayout.space_before(layout, "%{", nil), else: TokenLayout.stick_right(layout, "%{", nil)
    map_token = {:%{}, map_meta}

    # Keyword pairs
    {pairs_tokens, layout} = compile_kw_pairs(pairs, layout, opts)

    # Closing curly
    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {base_tokens ++ [dot_token] ++ eol_tokens ++ [map_token] ++ pairs_tokens ++ [close_token], layout}
  end

  # Anonymous struct with arrow syntax: expr.%{key => value}
  defp do_to_tokens({:anon_struct, base_expr, dot_newlines, {:assoc, pairs}}, layout, opts) do
    # Base expression
    {base_tokens, layout} = do_to_tokens(base_expr, layout, opts)

    # Dot (stuck to base)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Optional eol after dot
    {eol_tokens, layout} = compile_newlines(dot_newlines, layout)

    # %{ (map open)
    {map_meta, layout} =
      if dot_newlines > 0, do: TokenLayout.space_before(layout, "%{", nil), else: TokenLayout.stick_right(layout, "%{", nil)
    map_token = {:%{}, map_meta}

    # Association pairs
    {pairs_tokens, layout} = compile_assoc_pairs(pairs, layout, opts)

    # Closing curly
    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {base_tokens ++ [dot_token] ++ eol_tokens ++ [map_token] ++ pairs_tokens ++ [close_token], layout}
  end

  # ---------------------------------------------------------------------------
  # Bitstrings
  # ---------------------------------------------------------------------------

  # Bitstring: <<elem1, elem2, ...>>
  defp do_to_tokens({:bitstring, []}, layout, _opts) do
    # Empty bitstring: <<>>
    {open_meta, layout} = TokenLayout.space_before(layout, "<<", nil)
    open_token = {:"<<", open_meta}

    {close_meta, layout} = TokenLayout.stick_right(layout, ">>", nil)
    close_token = {:">>", close_meta}

    {[open_token, close_token], layout}
  end

  defp do_to_tokens({:bitstring, args}, layout, opts) do
    # Opening <<
    {open_meta, layout} = TokenLayout.space_before(layout, "<<", nil)
    open_token = {:"<<", open_meta}

    # Compile arguments (first stuck to open <<)
    {args_tokens, layout} = compile_args_in_brackets(args, layout, opts)

    # Closing >> (stuck to last arg)
    {close_meta, layout} = TokenLayout.stick_right(layout, ">>", nil)
    close_token = {:">>", close_meta}

    {[open_token] ++ args_tokens ++ [close_token], layout}
  end

  # ---------------------------------------------------------------------------
  # Strings
  # ---------------------------------------------------------------------------

  # Binary string: "hello"
  defp do_to_tokens({:bin_string, ""}, layout, _opts) do
    # Empty string: ""
    {str_meta, layout} = TokenLayout.space_before(layout, "\"\"", nil)
    str_token = {:bin_string, str_meta, [""]}

    {[str_token], layout}
  end

  defp do_to_tokens({:bin_string, content}, layout, _opts) when is_binary(content) do
    # Simple string with content (no interpolation)
    lexeme = "\"" <> content <> "\""
    {str_meta, layout} = TokenLayout.space_before(layout, lexeme, nil)
    str_token = {:bin_string, str_meta, [content]}

    {[str_token], layout}
  end

  # ---------------------------------------------------------------------------
  # Calls and Captures
  # ---------------------------------------------------------------------------

  # Paren identifier: foo (used before `(` in calls)
  defp do_to_tokens({:paren_identifier, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.space_before(layout, name, chars)
    {[{:paren_identifier, meta, atom}], layout}
  end

  # Call with parentheses: foo(1, 2) or expr.(1)
  # Grammar: parens_call -> dot_call_identifier call_args_parens
  defp do_to_tokens({:call_parens, target, args}, layout, opts) do
    # Compile target
    {target_tokens, layout} = compile_call_target(target, layout, opts)

    # Compile opening paren (stuck to target for adhesion)
    {open_meta, layout} = TokenLayout.stick_right(layout, "(", nil)
    open_token = {:"(", open_meta}

    # Compile arguments using call_args_parens handler
    {args_tokens, layout} = compile_call_args_parens(args, layout, opts)

    # Compile closing paren (stuck to last arg or open paren)
    {close_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    close_token = {:")", close_meta}

    {target_tokens ++ [open_token] ++ args_tokens ++ [close_token], layout}
  end

  # Nested parens call without do: foo()()
  # Grammar: parens_call -> dot_call_identifier call_args_parens call_args_parens
  defp do_to_tokens({:call_parens_nested, target, args1, args2}, layout, opts) do
    # Compile target
    {target_tokens, layout} = compile_call_target(target, layout, opts)

    # First call
    {lparen1_meta, layout} = TokenLayout.stick_right(layout, "(", nil)
    lparen1_token = {:"(", lparen1_meta}

    {args1_tokens, layout} = compile_call_args_parens(args1, layout, opts)

    {rparen1_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    rparen1_token = {:")", rparen1_meta}

    # Second call (stuck to first close paren)
    {lparen2_meta, layout} = TokenLayout.stick_right(layout, "(", nil)
    lparen2_token = {:"(", lparen2_meta}

    {args2_tokens, layout} = compile_call_args_parens(args2, layout, opts)

    {rparen2_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    rparen2_token = {:")", rparen2_meta}

    {target_tokens ++
       [lparen1_token] ++
       args1_tokens ++
       [rparen1_token, lparen2_token] ++
       args2_tokens ++ [rparen2_token], layout}
  end

  # No-parens call with one argument: foo bar
  defp do_to_tokens({:call_no_parens_one, {:identifier, name}, arg}, layout, opts) do
    # Compile identifier
    name_str = Atom.to_string(name)
    chars = String.to_charlist(name_str)
    {id_meta, layout} = TokenLayout.space_before(layout, name_str, chars)
    id_token = {:identifier, id_meta, name}

    # Compile argument(s)
    {arg_tokens, layout} = compile_no_parens_args(arg, layout, opts)

    {[id_token] ++ arg_tokens, layout}
  end

  # No-parens call with op identifier: +/2 used as identifier
  defp do_to_tokens({:call_no_parens_one, {:op_identifier, op}, arg}, layout, opts) do
    # Compile op_identifier as a standalone identifier
    name_str = Atom.to_string(op)
    chars = String.to_charlist(name_str)
    {id_meta, layout} = TokenLayout.space_before(layout, name_str, chars)
    id_token = {:op_identifier, id_meta, op}

    # Compile argument(s)
    {arg_tokens, layout} = compile_no_parens_args(arg, layout, opts)

    {[id_token] ++ arg_tokens, layout}
  end

  # No-parens call with dotted target: Mod.fun bar
  defp do_to_tokens({:call_no_parens_one, {:dot_identifier, left, right_name}, arg}, layout, opts) do
    # Compile the dotted identifier target
    {target_tokens, layout} = do_to_tokens({:dot_identifier, left, right_name}, layout, opts)

    # Compile argument(s)
    {arg_tokens, layout} = compile_no_parens_args(arg, layout, opts)

    {target_tokens ++ arg_tokens, layout}
  end

  # No-parens call with dotted op-identifier target: Expr.+ bar
  defp do_to_tokens({:call_no_parens_one, {:dot_op_identifier, left, op}, arg}, layout, opts) do
    # Compile the dotted op_identifier target
    {target_tokens, layout} = do_to_tokens({:dot_op_identifier, left, op}, layout, opts)

    # Compile argument(s)
    {arg_tokens, layout} = compile_no_parens_args(arg, layout, opts)

    {target_tokens ++ arg_tokens, layout}
  end

  # ---------------------------------------------------------------------------
  # no_parens_expr nodes (Increment 9)
  # ---------------------------------------------------------------------------

  # no_parens_many: foo a, b, c (multi-arg call without parentheses)
  # Per grammar lines 255-256
  defp do_to_tokens({:no_parens_many, target, args}, layout, opts) when is_list(args) do
    # Compile target (identifier or dot_identifier)
    {target_tokens, layout} = compile_no_parens_target(target, layout, opts)

    # Compile arguments with comma separators
    {args_tokens, layout} = compile_no_parens_many_args(args, layout, opts)

    {target_tokens ++ args_tokens, layout}
  end

  # no_parens_one_ambig: foo bar 1, 2 (nested ambiguous call)
  # Per grammar lines 252-253
  defp do_to_tokens({:no_parens_one_ambig, target, arg}, layout, opts) do
    # Compile target (identifier or dot_identifier)
    {target_tokens, layout} = compile_no_parens_target(target, layout, opts)

    # Compile the single argument (which is a no_parens_expr)
    {arg_tokens, layout} = do_to_tokens(arg, layout, opts)

    {target_tokens ++ arg_tokens, layout}
  end

  # no_parens_op: matched_expr op no_parens_expr
  # Per grammar line 173
  defp do_to_tokens(
         {:no_parens_op, left, {:op_eol, {op_kind, op}, newlines}, right},
         layout,
         opts
       ) do
    # Compile left side (matched_expr)
    {left_tokens, layout} = do_to_tokens(left, layout, opts)

    # Compile operator
    {op_meta, layout} = TokenLayout.space_before(layout, Atom.to_string(op), nil)
    op_token = {op_kind, op_meta, op}

    # Add newlines after operator if specified
    {newline_tokens, layout} = compile_newlines(newlines, layout)

    # Compile right side (no_parens_expr)
    {right_tokens, layout} = do_to_tokens(right, layout, opts)

    {left_tokens ++ [op_token] ++ newline_tokens ++ right_tokens, layout}
  end

  # no_parens_unary: unary_op no_parens_expr
  # Per grammar line 174
  defp do_to_tokens({:no_parens_unary, {op_kind, op}, newlines, operand}, layout, opts) do
    # Compile operator
    {op_meta, layout} = TokenLayout.space_before(layout, Atom.to_string(op), nil)
    op_token = {op_kind, op_meta, op}

    # Add newlines after operator if specified
    {newline_tokens, layout} = compile_newlines(newlines, layout)

    # Compile operand (no_parens_expr)
    {operand_tokens, layout} = do_to_tokens(operand, layout, opts)

    {[op_token] ++ newline_tokens ++ operand_tokens, layout}
  end

  # no_parens_at_op: @ no_parens_expr
  # Per grammar line 175
  defp do_to_tokens({:no_parens_at_op, newlines, operand}, layout, opts) do
    # Compile @ operator
    {at_meta, layout} = TokenLayout.space_before(layout, "@", nil)
    at_token = {:at_op, at_meta, :@}

    # Add newlines after operator if specified
    {newline_tokens, layout} = compile_newlines(newlines, layout)

    # Compile operand (no_parens_expr)
    {operand_tokens, layout} = do_to_tokens(operand, layout, opts)

    {[at_token] ++ newline_tokens ++ operand_tokens, layout}
  end

  # no_parens_capture_op: & no_parens_expr
  # Per grammar line 176
  defp do_to_tokens({:no_parens_capture_op, newlines, operand}, layout, opts) do
    # Compile & operator
    {amp_meta, layout} = TokenLayout.space_before(layout, "&", nil)
    amp_token = {:capture_op, amp_meta, :&}

    # Add newlines after operator if specified
    {newline_tokens, layout} = compile_newlines(newlines, layout)

    # Compile operand (no_parens_expr)
    {operand_tokens, layout} = do_to_tokens(operand, layout, opts)

    {[amp_token] ++ newline_tokens ++ operand_tokens, layout}
  end

  # no_parens_ellipsis: ... no_parens_expr
  # Per grammar line 177
  defp do_to_tokens({:no_parens_ellipsis, operand}, layout, opts) do
    # Compile ... operator
    {ellipsis_meta, layout} = TokenLayout.space_before(layout, "...", nil)
    ellipsis_token = {:ellipsis_op, ellipsis_meta, :...}

    # Compile operand (no_parens_expr)
    {operand_tokens, layout} = do_to_tokens(operand, layout, opts)

    {[ellipsis_token] ++ operand_tokens, layout}
  end

  # Dot call: expr.(args) - the expr part with the dot
  defp do_to_tokens({:dot_call, expr}, layout, opts) do
    # Compile expression
    {expr_tokens, layout} = do_to_tokens(expr, layout, opts)

    # Compile dot (stuck to expression)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    {expr_tokens ++ [dot_token], layout}
  end

  # Capture integer: &1, &10 (with adhesion)
  defp do_to_tokens({:capture_int, n}, layout, _opts) when is_integer(n) and n > 0 do
    # Compile & operator
    {amp_meta, layout} = TokenLayout.space_before(layout, "&", nil)
    amp_token = {:capture_op, amp_meta, :&}

    # Compile integer (stuck to & for adhesion)
    int_str = Integer.to_string(n)
    chars = String.to_charlist(int_str)
    {int_meta, layout} = TokenLayout.stick_right(layout, int_str, n)
    int_token = {:int, int_meta, chars}

    {[amp_token, int_token], layout}
  end

  # fn_single with fn_eoe and stab_eoe: fn fn_eoe stab_eoe end
  # Per grammar line 276: access_expr -> fn_eoe stab_eoe 'end'
  defp do_to_tokens({:fn_single, fn_eoe, {:stab_eoe, clauses, trailing_eoe}}, layout, opts) do
    # Compile 'fn' keyword
    {fn_meta, layout} = TokenLayout.space_before(layout, "fn", nil)
    fn_token = {:fn, fn_meta}

    # Compile fn_eoe (what comes after 'fn')
    {fn_eoe_tokens, layout} = compile_fn_eoe(fn_eoe, layout, opts)

    # Compile stab_eoe (clauses with optional trailing eoe)
    {clauses_tokens, layout} = compile_stab_eoe(clauses, trailing_eoe, layout, opts)

    # Compile 'end' keyword
    {end_meta, layout} = TokenLayout.space_before(layout, "end", nil)
    end_token = {:end, end_meta}

    {[fn_token] ++ fn_eoe_tokens ++ clauses_tokens ++ [end_token], layout}
  end

  # fn_single with stab_eoe (no fn_eoe): fn stab_eoe end (backward compat)
  defp do_to_tokens({:fn_single, {:stab_eoe, clauses, trailing_eoe}}, layout, opts) do
    # Compile 'fn' keyword
    {fn_meta, layout} = TokenLayout.space_before(layout, "fn", nil)
    fn_token = {:fn, fn_meta}

    # Compile stab_eoe (clauses with optional trailing eoe)
    {clauses_tokens, layout} = compile_stab_eoe(clauses, trailing_eoe, layout, opts)

    # Compile 'end' keyword
    {end_meta, layout} = TokenLayout.space_before(layout, "end", nil)
    end_token = {:end, end_meta}

    {[fn_token] ++ clauses_tokens ++ [end_token], layout}
  end

  # fn_single: fn clause end (backward compat)
  defp do_to_tokens({:fn_single, [clause]}, layout, opts) do
    # Compile 'fn' keyword
    {fn_meta, layout} = TokenLayout.space_before(layout, "fn", nil)
    fn_token = {:fn, fn_meta}

    # Compile the stab clause
    {clause_tokens, layout} = compile_stab_clause(clause, layout, opts)

    # Compile 'end' keyword
    {end_meta, layout} = TokenLayout.space_before(layout, "end", nil)
    end_token = {:end, end_meta}

    {[fn_token] ++ clause_tokens ++ [end_token], layout}
  end

  # fn_multi with fn_eoe and stab_eoe: fn fn_eoe stab_eoe end
  # Per grammar line 276: access_expr -> fn_eoe stab_eoe 'end'
  defp do_to_tokens({:fn_multi, fn_eoe, {:stab_eoe, clauses, trailing_eoe}}, layout, opts) do
    # Compile 'fn' keyword
    {fn_meta, layout} = TokenLayout.space_before(layout, "fn", nil)
    fn_token = {:fn, fn_meta}

    # Compile fn_eoe (what comes after 'fn')
    {fn_eoe_tokens, layout} = compile_fn_eoe(fn_eoe, layout, opts)

    # Compile stab_eoe (clauses with optional trailing eoe)
    {clauses_tokens, layout} = compile_stab_eoe(clauses, trailing_eoe, layout, opts)

    # Compile 'end' keyword
    {end_meta, layout} = TokenLayout.space_before(layout, "end", nil)
    end_token = {:end, end_meta}

    {[fn_token] ++ fn_eoe_tokens ++ clauses_tokens ++ [end_token], layout}
  end

  # fn_multi with stab_eoe (no fn_eoe): fn stab_eoe end (backward compat)
  defp do_to_tokens({:fn_multi, {:stab_eoe, clauses, trailing_eoe}}, layout, opts) do
    # Compile 'fn' keyword
    {fn_meta, layout} = TokenLayout.space_before(layout, "fn", nil)
    fn_token = {:fn, fn_meta}

    # Compile stab_eoe (clauses with optional trailing eoe)
    {clauses_tokens, layout} = compile_stab_eoe(clauses, trailing_eoe, layout, opts)

    # Compile 'end' keyword
    {end_meta, layout} = TokenLayout.space_before(layout, "end", nil)
    end_token = {:end, end_meta}

    {[fn_token] ++ clauses_tokens ++ [end_token], layout}
  end

  # fn_multi: fn clause1; clause2; ... end (backward compat)
  defp do_to_tokens({:fn_multi, clauses}, layout, opts) when length(clauses) >= 2 do
    # Compile 'fn' keyword
    {fn_meta, layout} = TokenLayout.space_before(layout, "fn", nil)
    fn_token = {:fn, fn_meta}

    # Compile stab clauses with semicolon separators
    {clauses_tokens, layout} = compile_stab_clauses(clauses, layout, opts)

    # Compile 'end' keyword
    {end_meta, layout} = TokenLayout.space_before(layout, "end", nil)
    end_token = {:end, end_meta}

    {[fn_token] ++ clauses_tokens ++ [end_token], layout}
  end

  # ---------------------------------------------------------------------------
  # paren_stab expressions (Increment 7)
  # ---------------------------------------------------------------------------

  # Per grammar lines 277-279:
  #   access_expr -> open_paren stab_eoe ')' : build_paren_stab('$1', '$2', '$3').
  #   access_expr -> open_paren ';' stab_eoe ')' : build_paren_stab('$1', '$3', '$4').
  #   access_expr -> open_paren ';' close_paren : build_paren_stab('$1', [], '$3').

  # paren_stab with stab_eoe: (stab_eoe)
  defp do_to_tokens({:paren_stab, {:stab_eoe, clauses, trailing_eoe}}, layout, opts) do
    # Compile '(' - with space before
    {open_meta, layout} = TokenLayout.space_before(layout, "(", nil)
    open_token = {:"(", open_meta}

    # Compile stab_eoe (clauses with optional trailing eoe)
    {clauses_tokens, layout} = compile_stab_eoe(clauses, trailing_eoe, layout, opts)

    # Compile ')' - stuck to last token
    {close_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    close_token = {:")", close_meta}

    {[open_token] ++ clauses_tokens ++ [close_token], layout}
  end

  # Variant where open_paren had a trailing newline: '(' eol stab_eoe ')'
  # Grammar action next_is_eol would attach newline count to open paren; at
  # token level we emit '(' then an :eol token and continue.
  defp do_to_tokens({:paren_stab_nl, {:stab_eoe, clauses, trailing_eoe}, newlines}, layout, opts)
       when is_integer(newlines) and newlines > 0 do
    # Compile '(' - with space before
    {open_meta, layout} = TokenLayout.space_before(layout, "(", nil)
    open_token = {:"(", open_meta}

    # Emit eol token representing the newline after '('
    eol_meta = TokenLayout.meta(layout, "\n", newlines)
    layout = TokenLayout.newlines(layout, newlines)
    eol_token = {:eol, eol_meta}

    # Compile stab_eoe (clauses with optional trailing eoe)
    {clauses_tokens, layout} = compile_stab_eoe(clauses, trailing_eoe, layout, opts)

    # Compile ')' - stuck to last token
    {close_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    close_token = {:")", close_meta}

    {[open_token, eol_token] ++ clauses_tokens ++ [close_token], layout}
  end

  # paren_stab: (clause) or (clause1; clause2) (backward compat)
  # Grammar: open_paren stab_eoe ')' : build_paren_stab
  defp do_to_tokens({:paren_stab, clauses}, layout, opts)
       when is_list(clauses) and length(clauses) >= 1 do
    # Compile '(' - with space before
    {open_meta, layout} = TokenLayout.space_before(layout, "(", nil)
    open_token = {:"(", open_meta}

    # Compile stab clauses with semicolon separators
    {clauses_tokens, layout} = compile_stab_clauses(clauses, layout, opts)

    # Compile ')' - stuck to last token
    {close_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    close_token = {:")", close_meta}

    {[open_token] ++ clauses_tokens ++ [close_token], layout}
  end

  # paren_stab_semi with stab_eoe: (; stab_eoe)
  defp do_to_tokens({:paren_stab_semi, {:stab_eoe, clauses, trailing_eoe}}, layout, opts) do
    # Compile '(' - with space before
    {open_meta, layout} = TokenLayout.space_before(layout, "(", nil)
    open_token = {:"(", open_meta}

    # Compile leading ';' - stuck to open paren
    {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
    semi_token = {:";", semi_meta}

    # Compile stab_eoe (clauses with optional trailing eoe)
    {clauses_tokens, layout} = compile_stab_eoe(clauses, trailing_eoe, layout, opts)

    # Compile ')' - stuck to last token
    {close_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    close_token = {:")", close_meta}

    {[open_token, semi_token] ++ clauses_tokens ++ [close_token], layout}
  end

  # Variant where open_paren had a trailing newline and leading semicolon
  defp do_to_tokens(
         {:paren_stab_nl_semi, {:stab_eoe, clauses, trailing_eoe}, newlines},
         layout,
         opts
       )
       when is_integer(newlines) and newlines > 0 do
    # Compile '(' - with space before
    {open_meta, layout} = TokenLayout.space_before(layout, "(", nil)
    open_token = {:"(", open_meta}

    # Emit eol token representing the newline after '('
    eol_meta = TokenLayout.meta(layout, "\n", newlines)
    layout = TokenLayout.newlines(layout, newlines)
    eol_token = {:eol, eol_meta}

    # Compile leading ';' - stuck to open paren (but after newline per grammar)
    {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
    semi_token = {:";", semi_meta}

    # Compile stab_eoe (clauses with optional trailing eoe)
    {clauses_tokens, layout} = compile_stab_eoe(clauses, trailing_eoe, layout, opts)

    # Compile ')' - stuck to last token
    {close_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    close_token = {:")", close_meta}

    {[open_token, eol_token, semi_token] ++ clauses_tokens ++ [close_token], layout}
  end

  # paren_stab_semi: (; clause) or (; clause1; clause2) (backward compat)
  # Grammar: open_paren ';' stab_eoe ')' : build_paren_stab
  defp do_to_tokens({:paren_stab_semi, clauses}, layout, opts)
       when is_list(clauses) and length(clauses) >= 1 do
    # Compile '(' - with space before
    {open_meta, layout} = TokenLayout.space_before(layout, "(", nil)
    open_token = {:"(", open_meta}

    # Compile leading ';' - stuck to open paren
    {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
    semi_token = {:";", semi_meta}

    # Compile stab clauses with semicolon separators
    {clauses_tokens, layout} = compile_stab_clauses(clauses, layout, opts)

    # Compile ')' - stuck to last token
    {close_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    close_token = {:")", close_meta}

    {[open_token, semi_token] ++ clauses_tokens ++ [close_token], layout}
  end

  # paren_stab_empty: (;)
  # Grammar: open_paren ';' close_paren : build_paren_stab
  defp do_to_tokens({:paren_stab_empty}, layout, _opts) do
    # Compile '(' - with space before
    {open_meta, layout} = TokenLayout.space_before(layout, "(", nil)
    open_token = {:"(", open_meta}

    # Compile ';' - stuck to open paren
    {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
    semi_token = {:";", semi_meta}

    # Compile ')' - stuck to semicolon
    {close_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    close_token = {:")", close_meta}

    {[open_token, semi_token, close_token], layout}
  end

  # Backward compatibility: convert old 3-element do_block to new 4-element format
  # Old format: {:do_block, body, extras}
  # New format: {:do_block, do_eoe, body, extras}
  defp do_to_tokens({:call_do, target, args, {:do_block, body, extras}}, layout, opts)
       when is_list(body) do
    do_to_tokens({:call_do, target, args, {:do_block, :eol, body, extras}}, layout, opts)
  end

  defp do_to_tokens({:block_parens, target, args, {:do_block, body, extras}}, layout, opts)
       when is_list(body) do
    do_to_tokens({:block_parens, target, args, {:do_block, :eol, body, extras}}, layout, opts)
  end

  defp do_to_tokens(
         {:block_parens_nested, target, args1, args2, {:do_block, body, extras}},
         layout,
         opts
       )
       when is_list(body) do
    do_to_tokens(
      {:block_parens_nested, target, args1, args2, {:do_block, :eol, body, extras}},
      layout,
      opts
    )
  end

  defp do_to_tokens({:block_no_parens_op, target, args, {:do_block, body, extras}}, layout, opts)
       when is_list(body) do
    do_to_tokens(
      {:block_no_parens_op, target, args, {:do_block, :eol, body, extras}},
      layout,
      opts
    )
  end

  defp do_to_tokens({:block_no_parens, target, args, {:do_block, body, extras}}, layout, opts)
       when is_list(body) do
    do_to_tokens({:block_no_parens, target, args, {:do_block, :eol, body, extras}}, layout, opts)
  end

  # call_do: identifier do body end (e.g., if true do :yes end)
  defp do_to_tokens(
         {:call_do, {:identifier, name}, args, {:do_block, do_eoe, body, extras}},
         layout,
         opts
       ) do
    # Compile identifier as do_identifier
    name_str = Atom.to_string(name)
    chars = String.to_charlist(name_str)
    {id_meta, layout} = TokenLayout.space_before(layout, name_str, chars)
    id_token = {:do_identifier, id_meta, name}

    # Compile arguments (if any)
    {args_tokens, layout} = compile_do_args(args, layout, opts)

    # Compile 'do' keyword
    {do_meta, layout} = TokenLayout.space_before(layout, "do", nil)
    do_token = {:do, do_meta}

    # Compile do_eoe (newline, semicolon, or inline)
    {eoe_tokens, layout} = compile_do_eoe(do_eoe, layout, opts)

    # Compile body expressions
    {body_tokens, layout} = compile_do_body(body, layout, opts)

    # Compile extras (else, rescue, etc.)
    {extras_tokens, layout} = compile_block_items(extras, layout, opts)

    # Compile 'end' keyword
    {end_meta, layout} = TokenLayout.space_before(layout, "end", nil)
    end_token = {:end, end_meta}

    {[id_token] ++
       args_tokens ++ [do_token] ++ eoe_tokens ++ body_tokens ++ extras_tokens ++ [end_token],
     layout}
  end

  # call_do with dot_do_identifier target: Mod.if true do :yes end
  # Per grammar: matched_expr dot_op do_identifier
  defp do_to_tokens(
         {:call_do, {:dot_do_identifier, left, name}, args, {:do_block, do_eoe, body, extras}},
         layout,
         opts
       ) do
    # Compile left side (matched_expr)
    {left_tokens, layout} = do_to_tokens(left, layout, opts)

    # Compile dot (stuck to left)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile right side as do_identifier (stuck to dot)
    name_str = Atom.to_string(name)
    chars = String.to_charlist(name_str)
    {id_meta, layout} = TokenLayout.stick_right(layout, name_str, chars)
    id_token = {:do_identifier, id_meta, name}

    # Compile arguments (if any)
    {args_tokens, layout} = compile_do_args(args, layout, opts)

    # Compile 'do' keyword
    {do_meta, layout} = TokenLayout.space_before(layout, "do", nil)
    do_token = {:do, do_meta}

    # Compile do_eoe (newline, semicolon, or inline)
    {eoe_tokens, layout} = compile_do_eoe(do_eoe, layout, opts)

    # Compile body expressions
    {body_tokens, layout} = compile_do_body(body, layout, opts)

    # Compile extras (else, rescue, etc.)
    {extras_tokens, layout} = compile_block_items(extras, layout, opts)

    # Compile 'end' keyword
    {end_meta, layout} = TokenLayout.space_before(layout, "end", nil)
    end_token = {:end, end_meta}

    {left_tokens ++
       [dot_token, id_token] ++
       args_tokens ++ [do_token] ++ eoe_tokens ++ body_tokens ++ extras_tokens ++ [end_token],
     layout}
  end

  # ---------------------------------------------------------------------------
  # block_expr: Rule 1 - dot_call_identifier call_args_parens do_block
  # Examples: foo() do end, Mod.func() do end, expr.() do end
  # ---------------------------------------------------------------------------

  defp do_to_tokens(
         {:block_parens, target, args, {:do_block, do_eoe, body, extras}},
         layout,
         opts
       ) do
    # Compile target (paren_identifier, dot_paren_identifier, or dot_call)
    {target_tokens, layout} = compile_block_parens_target(target, layout, opts)

    # Compile open paren (stuck to target)
    {lparen_meta, layout} = TokenLayout.stick_right(layout, "(", nil)
    lparen_token = {:"(", lparen_meta}

    # Compile arguments using call_args_parens handler
    {args_tokens, layout} = compile_call_args_parens(args, layout, opts)

    # Compile close paren (stuck to args)
    {rparen_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    rparen_token = {:")", rparen_meta}

    # Compile 'do' keyword
    {do_meta, layout} = TokenLayout.space_before(layout, "do", nil)
    do_token = {:do, do_meta}

    # Compile do_eoe (newline, semicolon, or inline)
    {eoe_tokens, layout} = compile_do_eoe(do_eoe, layout, opts)

    # Compile body expressions
    {body_tokens, layout} = compile_do_body(body, layout, opts)

    # Compile extras (else, rescue, etc.)
    {extras_tokens, layout} = compile_block_items(extras, layout, opts)

    # Compile 'end' keyword
    {end_meta, layout} = TokenLayout.space_before(layout, "end", nil)
    end_token = {:end, end_meta}

    {target_tokens ++
       [lparen_token] ++
       args_tokens ++
       [rparen_token, do_token] ++
       eoe_tokens ++
       body_tokens ++ extras_tokens ++ [end_token], layout}
  end

  # ---------------------------------------------------------------------------
  # block_expr: Rule 2 - nested paren calls with do block
  # Examples: foo()() do end, Mod.func()() do end
  # ---------------------------------------------------------------------------

  defp do_to_tokens(
         {:block_parens_nested, target, args1, args2, {:do_block, do_eoe, body, extras}},
         layout,
         opts
       ) do
    # Compile target (paren_identifier or dot_paren_identifier)
    {target_tokens, layout} = compile_block_parens_target(target, layout, opts)

    # Compile first call args
    {lparen1_meta, layout} = TokenLayout.stick_right(layout, "(", nil)
    lparen1_token = {:"(", lparen1_meta}

    {args1_tokens, layout} = compile_call_args_parens(args1, layout, opts)

    {rparen1_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    rparen1_token = {:")", rparen1_meta}

    # Compile second call args (stuck to first close paren)
    {lparen2_meta, layout} = TokenLayout.stick_right(layout, "(", nil)
    lparen2_token = {:"(", lparen2_meta}

    {args2_tokens, layout} = compile_call_args_parens(args2, layout, opts)

    {rparen2_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    rparen2_token = {:")", rparen2_meta}

    # Compile 'do' keyword
    {do_meta, layout} = TokenLayout.space_before(layout, "do", nil)
    do_token = {:do, do_meta}

    # Compile do_eoe (newline, semicolon, or inline)
    {eoe_tokens, layout} = compile_do_eoe(do_eoe, layout, opts)

    # Compile body expressions
    {body_tokens, layout} = compile_do_body(body, layout, opts)

    # Compile extras
    {extras_tokens, layout} = compile_block_items(extras, layout, opts)

    # Compile 'end' keyword
    {end_meta, layout} = TokenLayout.space_before(layout, "end", nil)
    end_token = {:end, end_meta}

    {target_tokens ++
       [lparen1_token] ++
       args1_tokens ++
       [rparen1_token, lparen2_token] ++
       args2_tokens ++
       [rparen2_token, do_token] ++
       eoe_tokens ++
       body_tokens ++ extras_tokens ++ [end_token], layout}
  end

  # ---------------------------------------------------------------------------
  # block_expr: Rule 4 - op_identifier with no-parens args and do block
  # Examples: .+ 1 do end, expr.* arg do end
  # ---------------------------------------------------------------------------

  defp do_to_tokens(
         {:block_no_parens_op, target, args, {:do_block, do_eoe, body, extras}},
         layout,
         opts
       ) do
    # Compile target (op_identifier or dot_op_identifier)
    {target_tokens, layout} = compile_op_identifier_target(target, layout, opts)

    # Compile args (single_arg or kw_args)
    {args_tokens, layout} = compile_no_parens_args(args, layout, opts)

    # Compile 'do' keyword
    {do_meta, layout} = TokenLayout.space_before(layout, "do", nil)
    do_token = {:do, do_meta}

    # Compile do_eoe (newline, semicolon, or inline)
    {eoe_tokens, layout} = compile_do_eoe(do_eoe, layout, opts)

    # Compile body expressions
    {body_tokens, layout} = compile_do_body(body, layout, opts)

    # Compile extras
    {extras_tokens, layout} = compile_block_items(extras, layout, opts)

    # Compile 'end' keyword
    {end_meta, layout} = TokenLayout.space_before(layout, "end", nil)
    end_token = {:end, end_meta}

    {target_tokens ++
       args_tokens ++
       [do_token] ++
       eoe_tokens ++
       body_tokens ++ extras_tokens ++ [end_token], layout}
  end

  # ---------------------------------------------------------------------------
  # block_expr: Rule 5 - identifier with no-parens args and do block
  # Examples: foo 1 do end, Mod.func arg do end
  # ---------------------------------------------------------------------------

  defp do_to_tokens(
         {:block_no_parens, target, args, {:do_block, do_eoe, body, extras}},
         layout,
         opts
       ) do
    # Compile target (identifier or dot_identifier)
    {target_tokens, layout} = compile_identifier_target(target, layout, opts)

    # Compile args (single_arg or kw_args)
    {args_tokens, layout} = compile_no_parens_args(args, layout, opts)

    # Compile 'do' keyword
    {do_meta, layout} = TokenLayout.space_before(layout, "do", nil)
    do_token = {:do, do_meta}

    # Compile do_eoe (newline, semicolon, or inline)
    {eoe_tokens, layout} = compile_do_eoe(do_eoe, layout, opts)

    # Compile body expressions
    {body_tokens, layout} = compile_do_body(body, layout, opts)

    # Compile extras
    {extras_tokens, layout} = compile_block_items(extras, layout, opts)

    # Compile 'end' keyword
    {end_meta, layout} = TokenLayout.space_before(layout, "end", nil)
    end_token = {:end, end_meta}

    {target_tokens ++
       args_tokens ++
       [do_token] ++
       eoe_tokens ++
       body_tokens ++ extras_tokens ++ [end_token], layout}
  end

  # ---------------------------------------------------------------------------
  # Access expression followed by keyword identifier (invalid kw identifier)
  # ---------------------------------------------------------------------------

  defp do_to_tokens({:access_expr_kw_identifier, access_expr, key}, layout, opts) do
    # Compile the access expression normally
    {access_tokens, layout} = do_to_tokens(access_expr, layout, opts)

    # Emit the kw_identifier token (e.g., "key:") with a space before it
    key_str = Atom.to_string(key)
    key_lexeme = key_str <> ":"
    {meta, layout} = TokenLayout.space_before(layout, key_lexeme, nil)
    kw_token = {:kw_identifier, meta, key}

    {access_tokens ++ [kw_token], layout}
  end

  # ---------------------------------------------------------------------------
  # Catch-all for unimplemented nodes
  # ---------------------------------------------------------------------------

  defp do_to_tokens(node, _layout, _opts) do
    raise "Unimplemented grammar tree node: #{inspect(node)}"
  end

  # Common binary operator compilation
  defp compile_binary_op(left, {:op_eol, {op_kind, op}, newlines}, right, layout, opts) do
    # Compile left operand
    {left_tokens, layout} = do_to_tokens(left, layout, opts)

    # Compile operator
    op_lexeme = op_to_lexeme(op)
    {op_meta, layout} = TokenLayout.space_before(layout, op_lexeme, nil)
    op_token = {op_kind, op_meta, op}

    # Handle newlines after operator
    {eol_tokens, layout} =
      if newlines > 0 do
        eol_meta = TokenLayout.meta(layout, "\n", newlines)
        layout = TokenLayout.newlines(layout, newlines)
        {[{:eol, eol_meta}], layout}
      else
        {[], layout}
      end

    # Compile right operand
    {right_tokens, layout} = do_to_tokens(right, layout, opts)

    {left_tokens ++ [op_token] ++ eol_tokens ++ right_tokens, layout}
  end

  # Helper: compile target for no_parens calls (identifier or dot_identifier)
  defp compile_no_parens_target({:identifier, name}, layout, _opts) do
    name_str = Atom.to_string(name)
    chars = String.to_charlist(name_str)
    {id_meta, layout} = TokenLayout.space_before(layout, name_str, chars)
    id_token = {:identifier, id_meta, name}
    {[id_token], layout}
  end

  defp compile_no_parens_target({:dot_identifier, left, right_name}, layout, opts) do
    # Compile left side
    {left_tokens, layout} = do_to_tokens(left, layout, opts)

    # Compile dot (stuck to left)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile right identifier (stuck to dot)
    name_str = Atom.to_string(right_name)
    chars = String.to_charlist(name_str)
    {id_meta, layout} = TokenLayout.stick_right(layout, name_str, chars)
    id_token = {:identifier, id_meta, right_name}

    {left_tokens ++ [dot_token, id_token], layout}
  end

  defp compile_no_parens_target({:op_identifier, op}, layout, _opts) do
    name_str = Atom.to_string(op)
    chars = String.to_charlist(name_str)
    {id_meta, layout} = TokenLayout.space_before(layout, name_str, chars)
    id_token = {:op_identifier, id_meta, op}
    {[id_token], layout}
  end

  defp compile_no_parens_target({:dot_op_identifier, left, op}, layout, opts) do
    # Compile left side
    {left_tokens, layout} = do_to_tokens(left, layout, opts)

    # Compile dot (stuck to left)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile op identifier (stuck to dot)
    name = Atom.to_string(op)
    chars = String.to_charlist(name)
    {op_meta, layout} = TokenLayout.stick_right(layout, name, chars)
    op_token = {:op_identifier, op_meta, op}

    {left_tokens ++ [dot_token, op_token], layout}
  end

  # Helper: compile multiple arguments for no_parens_many (comma-separated)
  defp compile_no_parens_many_args([], layout, _opts), do: {[], layout}

  defp compile_no_parens_many_args([arg], layout, opts) do
    # Single arg - compile with space before
    case arg do
      {:kw_args, _} -> compile_no_parens_args(arg, layout, opts)
      _ -> do_to_tokens(arg, layout, opts)
    end
  end

  defp compile_no_parens_many_args([arg | rest], layout, opts) do
    # First arg with space before
    {arg_tokens, layout} =
      case arg do
        {:kw_args, _} -> compile_no_parens_args(arg, layout, opts)
        _ -> do_to_tokens(arg, layout, opts)
      end

    # Compile comma
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    # Compile rest
    {rest_tokens, layout} = compile_no_parens_many_args_rest(rest, layout, opts)

    {arg_tokens ++ [comma_token] ++ rest_tokens, layout}
  end

  defp compile_no_parens_many_args_rest([], layout, _opts), do: {[], layout}

  defp compile_no_parens_many_args_rest([arg], layout, opts) do
    # Last arg with space before
    case arg do
      {:kw_args, _} -> compile_no_parens_args(arg, layout, opts)
      _ -> do_to_tokens(arg, layout, opts)
    end
  end

  defp compile_no_parens_many_args_rest([arg | rest], layout, opts) do
    # Arg with space before
    {arg_tokens, layout} =
      case arg do
        {:kw_args, _} -> compile_no_parens_args(arg, layout, opts)
        _ -> do_to_tokens(arg, layout, opts)
      end

    # Compile comma
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    # Compile rest
    {rest_tokens, layout} = compile_no_parens_many_args_rest(rest, layout, opts)

    {arg_tokens ++ [comma_token] ++ rest_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_call_target
  # ===========================================================================

  # Target is a paren_identifier (most common case): foo(...)
  defp compile_call_target({:paren_identifier, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.space_before(layout, name, chars)
    {[{:paren_identifier, meta, atom}], layout}
  end

  # Target is a dot_call: expr.(...)
  defp compile_call_target({:dot_call, expr}, layout, opts) do
    # Compile expression
    {expr_tokens, layout} = do_to_tokens(expr, layout, opts)

    # Compile dot (stuck to expression for adhesion)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    {expr_tokens ++ [dot_token], layout}
  end

  # Target is a dot_paren_identifier (matched_expr.paren_identifier)
  defp compile_call_target({:dot_paren_identifier, left, right_name}, layout, opts) do
    # Compile left side (matched_expr)
    {left_tokens, layout} = do_to_tokens(left, layout, opts)

    # Compile dot (stuck to left)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile right side as paren_identifier (stuck to dot)
    right_str = Atom.to_string(right_name)
    chars = String.to_charlist(right_str)
    {right_meta, layout} = TokenLayout.stick_right(layout, right_str, chars)
    right_token = {:paren_identifier, right_meta, right_name}

    {left_tokens ++ [dot_token, right_token], layout}
  end

  # Target is an identifier (shouldn't happen for call_parens, but handle it)
  defp compile_call_target({:identifier, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.space_before(layout, name, chars)
    {[{:identifier, meta, atom}], layout}
  end

  # ===========================================================================
  # Helper: compile_block_parens_target (for block_expr rule 1 & 2)
  # ===========================================================================

  # Compile target for paren calls with do blocks
  # Target types: paren_identifier, dot_paren_identifier, dot_call

  # Simple paren_identifier: foo
  defp compile_block_parens_target({:paren_identifier, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.space_before(layout, name, chars)
    {[{:paren_identifier, meta, atom}], layout}
  end

  # dot_paren_identifier: matched_expr.paren_identifier
  defp compile_block_parens_target({:dot_paren_identifier, left, right_name}, layout, opts) do
    # Compile left side (matched_expr)
    {left_tokens, layout} = do_to_tokens(left, layout, opts)

    # Compile dot (stuck to left)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile right side as paren_identifier (stuck to dot)
    right_str = Atom.to_string(right_name)
    chars = String.to_charlist(right_str)
    {right_meta, layout} = TokenLayout.stick_right(layout, right_str, chars)
    right_token = {:paren_identifier, right_meta, right_name}

    {left_tokens ++ [dot_token, right_token], layout}
  end

  # dot_call: expr. (for anonymous function calls)
  defp compile_block_parens_target({:dot_call, expr}, layout, opts) do
    # Compile expression
    {expr_tokens, layout} = do_to_tokens(expr, layout, opts)

    # Compile dot (stuck to expression)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    {expr_tokens ++ [dot_token], layout}
  end

  # ===========================================================================
  # Helper: compile_op_identifier_target (for block_expr rule 4)
  # ===========================================================================

  # Simple op_identifier: + (as function reference)
  defp compile_op_identifier_target({:op_identifier, op}, layout, _opts) do
    name = Atom.to_string(op)
    {meta, layout} = TokenLayout.space_before(layout, name, nil)
    {[{:op_identifier, meta, op}], layout}
  end

  # dot_op_identifier: matched_expr.+
  defp compile_op_identifier_target({:dot_op_identifier, left, op}, layout, opts) do
    # Compile left side (matched_expr)
    {left_tokens, layout} = do_to_tokens(left, layout, opts)

    # Compile dot (stuck to left)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile operator as op_identifier (stuck to dot)
    op_str = Atom.to_string(op)
    {op_meta, layout} = TokenLayout.stick_right(layout, op_str, nil)
    op_token = {:op_identifier, op_meta, op}

    {left_tokens ++ [dot_token, op_token], layout}
  end

  # ===========================================================================
  # Helper: compile_identifier_target (for block_expr rule 5)
  # ===========================================================================

  # Simple identifier: foo
  defp compile_identifier_target({:identifier, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.space_before(layout, name, chars)
    {[{:identifier, meta, atom}], layout}
  end

  # dot_identifier: matched_expr.identifier
  defp compile_identifier_target({:dot_identifier, left, right_name}, layout, opts) do
    # Compile left side (matched_expr)
    {left_tokens, layout} = do_to_tokens(left, layout, opts)

    # Compile dot (stuck to left)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile right side as identifier (stuck to dot)
    right_str = Atom.to_string(right_name)
    chars = String.to_charlist(right_str)
    {right_meta, layout} = TokenLayout.stick_right(layout, right_str, chars)
    right_token = {:identifier, right_meta, right_name}

    {left_tokens ++ [dot_token, right_token], layout}
  end

  # ===========================================================================
  # Helper: compile_call_args_parens
  # ===========================================================================
  #
  # Per grammar lines 553-562, call_args_parens can be:
  # - `{:call_args_parens, :empty}` - empty args ()
  # - `{:call_args_parens, {:no_parens_expr, expr}}` - single no_parens_expr (foo bar)
  # - `{:call_args_parens, {:kw_only, pairs}}` - keyword only (a: 1, b: 2)
  # - `{:call_args_parens, {:positional, exprs}}` - positional args only (1, 2, 3)
  # - `{:call_args_parens, {:positional_with_kw, exprs, kw_pairs}}` - positional + kw

  # Empty args: ()
  defp compile_call_args_parens({:call_args_parens, :empty}, layout, _opts), do: {[], layout}

  # Single no_parens_expr: (foo bar)
  defp compile_call_args_parens({:call_args_parens, {:no_parens_expr, expr}}, layout, opts) do
    # Compile the no_parens_expr - it gets space handling from its own compilation
    {expr_tokens, layout} = do_to_tokens(expr, layout, opts)
    {expr_tokens, layout}
  end

  # Keyword only: (a: 1, b: 2)
  # New format: {:kw_call, pairs} or {:kw_call_trailing, pairs}
  defp compile_call_args_parens({:call_args_parens, {:kw_only, {:kw_call, pairs}}}, layout, opts) do
    compile_kw_base(pairs, layout, opts, :first)
  end

  defp compile_call_args_parens(
         {:call_args_parens, {:kw_only, {:kw_call_trailing, pairs}}},
         layout,
         opts
       ) do
    {kw_tokens, layout} = compile_kw_base(pairs, layout, opts, :first)
    # Add trailing comma
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}
    {kw_tokens ++ [comma_token], layout}
  end

  # Old format for backward compatibility: [{key, value}, ...]
  defp compile_call_args_parens({:call_args_parens, {:kw_only, pairs}}, layout, opts)
       when is_list(pairs) do
    compile_kw_call_args(pairs, layout, opts, :first)
  end

  # Positional args only: (1, 2, 3)
  defp compile_call_args_parens({:call_args_parens, {:positional, exprs}}, layout, opts) do
    compile_args_in_parens(exprs, layout, opts)
  end

  # Positional + trailing kw: (1, 2, a: 3)
  # New format: {:kw_call, pairs} or {:kw_call_trailing, pairs}
  defp compile_call_args_parens(
         {:call_args_parens, {:positional_with_kw, exprs, {:kw_call, kw_pairs}}},
         layout,
         opts
       ) do
    # Compile positional args first
    {pos_tokens, layout} = compile_args_in_parens(exprs, layout, opts)

    # Add comma before keywords
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    # Compile keyword args (not first since we have positional)
    {kw_tokens, layout} = compile_kw_base(kw_pairs, layout, opts, :rest)

    {pos_tokens ++ [comma_token] ++ kw_tokens, layout}
  end

  defp compile_call_args_parens(
         {:call_args_parens, {:positional_with_kw, exprs, {:kw_call_trailing, kw_pairs}}},
         layout,
         opts
       ) do
    # Compile positional args first
    {pos_tokens, layout} = compile_args_in_parens(exprs, layout, opts)

    # Add comma before keywords
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    # Compile keyword args (not first since we have positional)
    {kw_tokens, layout} = compile_kw_base(kw_pairs, layout, opts, :rest)

    # Add trailing comma
    {trailing_comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    trailing_comma_token = {:",", trailing_comma_meta}

    {pos_tokens ++ [comma_token] ++ kw_tokens ++ [trailing_comma_token], layout}
  end

  # Old format for backward compatibility: plain list of pairs
  defp compile_call_args_parens(
         {:call_args_parens, {:positional_with_kw, exprs, kw_pairs}},
         layout,
         opts
       )
       when is_list(kw_pairs) do
    # Compile positional args first
    {pos_tokens, layout} = compile_args_in_parens(exprs, layout, opts)

    # Add comma before keywords
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    # Compile keyword args (not first since we have positional)
    {kw_tokens, layout} = compile_kw_call_args(kw_pairs, layout, opts, :rest)

    {pos_tokens ++ [comma_token] ++ kw_tokens, layout}
  end

  # Backward compatibility: handle plain list (old format)
  defp compile_call_args_parens(args, layout, opts) when is_list(args) do
    compile_args_in_parens(args, layout, opts)
  end

  # Helper: compile keyword args for call_args_parens
  # :first means first arg is stuck to open paren, :rest means space before
  defp compile_kw_call_args([], layout, _opts, _position), do: {[], layout}

  defp compile_kw_call_args([{key, value} | rest], layout, opts, position) do
    # Compile keyword key (as kw_identifier)
    key_str = Atom.to_string(key)
    key_lexeme = key_str <> ":"
    chars = String.to_charlist(key_str)

    {key_meta, layout} =
      case position do
        :first -> TokenLayout.stick_right(layout, key_lexeme, chars)
        :rest -> TokenLayout.space_before(layout, key_lexeme, chars)
      end

    key_token = {:kw_identifier, key_meta, key}

    # Compile value with space
    {value_tokens, layout} = do_to_tokens(value, layout, opts)

    # Compile remaining pairs
    {rest_tokens, layout} = compile_kw_call_args_rest(rest, layout, opts)

    {[key_token] ++ value_tokens ++ rest_tokens, layout}
  end

  defp compile_kw_call_args_rest([], layout, _opts), do: {[], layout}

  defp compile_kw_call_args_rest([{key, value} | rest], layout, opts) do
    # Add comma
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    # Compile key with space
    key_str = Atom.to_string(key)
    key_lexeme = key_str <> ":"
    chars = String.to_charlist(key_str)
    {key_meta, layout} = TokenLayout.space_before(layout, key_lexeme, chars)
    key_token = {:kw_identifier, key_meta, key}

    # Compile value
    {value_tokens, layout} = do_to_tokens(value, layout, opts)

    # Compile remaining
    {rest_tokens, layout} = compile_kw_call_args_rest(rest, layout, opts)

    {[comma_token, key_token] ++ value_tokens ++ rest_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_kw_base (new format with kw_eol)
  # ===========================================================================
  #
  # Handles the new kw_base format where pairs are:
  #   {{:kw_eol, key, has_eol}, container_expr}
  #
  # :first means first arg is stuck to open paren, :rest means space before

  defp compile_kw_base([], layout, _opts, _position), do: {[], layout}

  defp compile_kw_base([{{:kw_eol, key, has_eol}, value} | rest], layout, opts, position) do
    # Compile keyword key (as kw_identifier)
    key_str = Atom.to_string(key)
    key_lexeme = key_str <> ":"
    chars = String.to_charlist(key_str)

    {key_meta, layout} =
      case position do
        :first -> TokenLayout.stick_right(layout, key_lexeme, chars)
        :rest -> TokenLayout.space_before(layout, key_lexeme, chars)
      end

    key_token = {:kw_identifier, key_meta, key}

    # Add optional eol after key
    {eol_tokens, layout} =
      if has_eol do
        eol_meta = TokenLayout.meta(layout, "\n", 1)
        eol_token = {:eol, eol_meta}
        layout = TokenLayout.newline(layout)
        {[eol_token], layout}
      else
        {[], layout}
      end

    # Compile value with space (or after newline)
    {value_tokens, layout} = do_to_tokens(value, layout, opts)

    # Compile remaining pairs
    {rest_tokens, layout} = compile_kw_base_rest(rest, layout, opts)

    {[key_token] ++ eol_tokens ++ value_tokens ++ rest_tokens, layout}
  end

  defp compile_kw_base_rest([], layout, _opts), do: {[], layout}

  defp compile_kw_base_rest([{{:kw_eol, key, has_eol}, value} | rest], layout, opts) do
    # Add comma
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    # Compile key with space
    key_str = Atom.to_string(key)
    key_lexeme = key_str <> ":"
    chars = String.to_charlist(key_str)
    {key_meta, layout} = TokenLayout.space_before(layout, key_lexeme, chars)
    key_token = {:kw_identifier, key_meta, key}

    # Add optional eol after key
    {eol_tokens, layout} =
      if has_eol do
        eol_meta = TokenLayout.meta(layout, "\n", 1)
        eol_token = {:eol, eol_meta}
        layout = TokenLayout.newline(layout)
        {[eol_token], layout}
      else
        {[], layout}
      end

    # Compile value
    {value_tokens, layout} = do_to_tokens(value, layout, opts)

    # Compile remaining
    {rest_tokens, layout} = compile_kw_base_rest(rest, layout, opts)

    {[comma_token, key_token] ++ eol_tokens ++ value_tokens ++ rest_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_kw_data (for bracket_arg)
  # ===========================================================================
  #
  # Handles {:kw_data, pairs} and {:kw_data_trailing, pairs} from bracket_arg

  defp compile_kw_data({:kw_data, pairs}, layout, opts) do
    compile_kw_base(pairs, layout, opts, :first)
  end

  defp compile_kw_data({:kw_data_trailing, pairs}, layout, opts) do
    {kw_tokens, layout} = compile_kw_base(pairs, layout, opts, :first)
    # Add trailing comma
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}
    {kw_tokens ++ [comma_token], layout}
  end

  # ===========================================================================
  # Helper: compile_args_in_parens
  # ===========================================================================

  # Compile arguments inside parentheses (first arg stuck to open paren)
  defp compile_args_in_parens([], layout, _opts), do: {[], layout}

  defp compile_args_in_parens([arg | rest], layout, opts) do
    # First argument is stuck to opening paren (no space)
    {first_tokens, layout} = compile_arg_stuck(arg, layout, opts)

    # Remaining args have commas and spaces
    {rest_tokens, layout} = compile_remaining_args(rest, layout, opts)

    {first_tokens ++ rest_tokens, layout}
  end

  # Compile an argument stuck to previous token (no leading space)
  defp compile_arg_stuck(arg, layout, opts) do
    compile_arg_with_adhesion(arg, layout, opts)
  end

  # Compile remaining arguments with comma separators
  defp compile_remaining_args([], layout, _opts), do: {[], layout}

  defp compile_remaining_args([arg | rest], layout, opts) do
    # Add comma token (stuck to previous)
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    # Compile arg with space before
    {arg_tokens, layout} = do_to_tokens(arg, layout, opts)

    # Continue with remaining args
    {rest_tokens, layout} = compile_remaining_args(rest, layout, opts)

    {[comma_token] ++ arg_tokens ++ rest_tokens, layout}
  end

  # Helper to compile an expression with adhesion (stuck to previous token)
  # This handles various expression types by emitting them without leading space
  defp compile_arg_with_adhesion({:int, value, _format, chars}, layout, _opts) do
    lexeme = List.to_string(chars)
    {meta, layout} = TokenLayout.stick_right(layout, lexeme, value)
    {[{:int, meta, chars}], layout}
  end

  defp compile_arg_with_adhesion({:float, value, chars}, layout, _opts) do
    lexeme = List.to_string(chars)
    {meta, layout} = TokenLayout.stick_right(layout, lexeme, value)
    {[{:flt, meta, chars}], layout}
  end

  defp compile_arg_with_adhesion({:char, codepoint, chars}, layout, _opts) do
    lexeme = List.to_string(chars)
    {meta, layout} = TokenLayout.stick_right(layout, lexeme, chars)
    {[{:char, meta, codepoint}], layout}
  end

  defp compile_arg_with_adhesion({:atom_lit, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    lexeme = ":" <> name
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.stick_right(layout, lexeme, chars)
    {[{:atom, meta, atom}], layout}
  end

  defp compile_arg_with_adhesion({:bool_lit, true}, layout, _opts) do
    {meta, layout} = TokenLayout.stick_right(layout, "true", nil)
    {[{true, meta}], layout}
  end

  defp compile_arg_with_adhesion({:bool_lit, false}, layout, _opts) do
    {meta, layout} = TokenLayout.stick_right(layout, "false", nil)
    {[{false, meta}], layout}
  end

  defp compile_arg_with_adhesion(:nil_lit, layout, _opts) do
    {meta, layout} = TokenLayout.stick_right(layout, "nil", nil)
    {[{nil, meta}], layout}
  end

  defp compile_arg_with_adhesion({:identifier, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.stick_right(layout, name, chars)
    {[{:identifier, meta, atom}], layout}
  end

  # Do identifier stuck to previous token
  defp compile_arg_with_adhesion({:do_identifier, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.stick_right(layout, name, chars)
    {[{:do_identifier, meta, atom}], layout}
  end

  # Op identifier stuck to previous token
  defp compile_arg_with_adhesion({:op_identifier, op}, layout, _opts) do
    name = Atom.to_string(op)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.stick_right(layout, name, chars)
    {[{:op_identifier, meta, op}], layout}
  end

  defp compile_arg_with_adhesion({:alias, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.stick_right(layout, name, chars)
    {[{:alias, meta, atom}], layout}
  end

  defp compile_arg_with_adhesion({:capture_int, n}, layout, _opts) when is_integer(n) and n > 0 do
    # & stuck to position, int stuck to &
    {amp_meta, layout} = TokenLayout.stick_right(layout, "&", nil)
    amp_token = {:capture_op, amp_meta, :&}

    int_str = Integer.to_string(n)
    chars = String.to_charlist(int_str)
    {int_meta, layout} = TokenLayout.stick_right(layout, int_str, n)
    int_token = {:int, int_meta, chars}

    {[amp_token, int_token], layout}
  end

  # For complex expressions like nested calls, emit with adhesion
  defp compile_arg_with_adhesion({:call_parens, target, args}, layout, opts) do
    # Compile target stuck to current position
    {target_tokens, layout} = compile_call_target_stuck(target, layout, opts)

    # Opening paren stuck to target
    {open_meta, layout} = TokenLayout.stick_right(layout, "(", nil)
    open_token = {:"(", open_meta}

    # Args inside parens (using call_args_parens handler for new format)
    {args_tokens, layout} = compile_call_args_parens(args, layout, opts)

    # Closing paren stuck to args
    {close_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    close_token = {:")", close_meta}

    {target_tokens ++ [open_token] ++ args_tokens ++ [close_token], layout}
  end

  defp compile_arg_with_adhesion({:paren_identifier, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.stick_right(layout, name, chars)
    {[{:paren_identifier, meta, atom}], layout}
  end

  # fn_single stuck to previous token
  defp compile_arg_with_adhesion({:fn_single, [clause]}, layout, opts) do
    # Compile 'fn' keyword stuck to previous
    {fn_meta, layout} = TokenLayout.stick_right(layout, "fn", nil)
    fn_token = {:fn, fn_meta}

    # Compile the stab clause
    {clause_tokens, layout} = compile_stab_clause(clause, layout, opts)

    # Compile 'end' keyword
    {end_meta, layout} = TokenLayout.space_before(layout, "end", nil)
    end_token = {:end, end_meta}

    {[fn_token] ++ clause_tokens ++ [end_token], layout}
  end

  # Matched binary operator stuck to previous token
  defp compile_arg_with_adhesion({:matched_op, left, op_eol, right}, layout, opts) do
    compile_binary_op_stuck(left, op_eol, right, layout, opts)
  end

  # Unmatched binary operator stuck to previous token
  defp compile_arg_with_adhesion({:unmatched_op, left, op_eol, right}, layout, opts) do
    compile_binary_op_stuck(left, op_eol, right, layout, opts)
  end

  # Legacy binary operator stuck to previous token
  defp compile_arg_with_adhesion({:binary_op, left, op_eol, right}, layout, opts) do
    compile_binary_op_stuck(left, op_eol, right, layout, opts)
  end

  # Matched unary operator stuck to previous token (with newlines)
  defp compile_arg_with_adhesion({:matched_unary, op_kind, newlines, operand}, layout, opts)
       when is_integer(newlines) do
    compile_unary_op_stuck_with_newlines(op_kind, newlines, operand, layout, opts)
  end

  # Matched unary operator stuck to previous token (legacy, no newlines)
  defp compile_arg_with_adhesion({:matched_unary, op_kind, operand}, layout, opts) do
    compile_unary_op_stuck(op_kind, operand, layout, opts)
  end

  # Legacy unary operator stuck to previous token
  defp compile_arg_with_adhesion({:unary_op, op_kind, operand}, layout, opts) do
    compile_unary_op_stuck(op_kind, operand, layout, opts)
  end

  # Dotted identifier stuck to previous token
  defp compile_arg_with_adhesion({:dot_identifier, left, right_name}, layout, opts) do
    # Compile left expression stuck to current position
    {left_tokens, layout} = compile_arg_with_adhesion(left, layout, opts)

    # Compile dot (stuck to left)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile right identifier (stuck to dot)
    right_str = Atom.to_string(right_name)
    chars = String.to_charlist(right_str)
    {right_meta, layout} = TokenLayout.stick_right(layout, right_str, chars)
    right_token = {:identifier, right_meta, right_name}

    {left_tokens ++ [dot_token, right_token], layout}
  end

  # Dotted operator identifier stuck to previous token
  defp compile_arg_with_adhesion({:dot_op_identifier, left, op}, layout, opts) do
    # Compile left expression stuck to current position
    {left_tokens, layout} = compile_arg_with_adhesion(left, layout, opts)

    # Compile dot (stuck to left)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile op identifier (stuck to dot)
    name = Atom.to_string(op)
    chars = String.to_charlist(name)
    {op_meta, layout} = TokenLayout.stick_right(layout, name, chars)
    op_token = {:op_identifier, op_meta, op}

    {left_tokens ++ [dot_token, op_token], layout}
  end

  # Dotted do_identifier stuck to previous token
  defp compile_arg_with_adhesion({:dot_do_identifier, left, do_id}, layout, opts) do
    # Compile left expression stuck to current position
    {left_tokens, layout} = compile_arg_with_adhesion(left, layout, opts)

    # Compile dot (stuck to left)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile do_identifier (stuck to dot)
    id_str = Atom.to_string(do_id)
    chars = String.to_charlist(id_str)
    {id_meta, layout} = TokenLayout.stick_right(layout, id_str, chars)
    id_token = {:do_identifier, id_meta, do_id}

    {left_tokens ++ [dot_token, id_token], layout}
  end

  # List stuck to previous token
  defp compile_arg_with_adhesion({:list, []}, layout, _opts) do
    {open_meta, layout} = TokenLayout.stick_right(layout, "[", nil)
    open_token = {:"[", open_meta}

    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token, close_token], layout}
  end

  defp compile_arg_with_adhesion({:list, args}, layout, opts) do
    {open_meta, layout} = TokenLayout.stick_right(layout, "[", nil)
    open_token = {:"[", open_meta}

    {args_tokens, layout} = compile_args_in_brackets(args, layout, opts)

    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token] ++ args_tokens ++ [close_token], layout}
  end

  # Tuple stuck to previous token
  defp compile_arg_with_adhesion({:tuple, []}, layout, _opts) do
    {open_meta, layout} = TokenLayout.stick_right(layout, "{", nil)
    open_token = {:"{", open_meta}

    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[open_token, close_token], layout}
  end

  defp compile_arg_with_adhesion({:tuple, args}, layout, opts) do
    {open_meta, layout} = TokenLayout.stick_right(layout, "{", nil)
    open_token = {:"{", open_meta}

    {args_tokens, layout} = compile_args_in_brackets(args, layout, opts)

    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[open_token] ++ args_tokens ++ [close_token], layout}
  end

  # Map stuck to previous token
  defp compile_arg_with_adhesion({:map, []}, layout, _opts) do
    {map_meta, layout} = TokenLayout.stick_right(layout, "%{}", nil)
    map_token = {:%{}, map_meta}

    {[map_token], layout}
  end

  defp compile_arg_with_adhesion({:map, {:kw, pairs}}, layout, opts) do
    {map_meta, layout} = TokenLayout.stick_right(layout, "%{", nil)
    map_token = {:%{}, map_meta}

    {pairs_tokens, layout} = compile_kw_pairs(pairs, layout, opts)

    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[map_token] ++ pairs_tokens ++ [close_token], layout}
  end

  defp compile_arg_with_adhesion({:map, {:assoc, pairs}}, layout, opts) do
    {map_meta, layout} = TokenLayout.stick_right(layout, "%{", nil)
    map_token = {:%{}, map_meta}

    {pairs_tokens, layout} = compile_assoc_pairs(pairs, layout, opts)

    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[map_token] ++ pairs_tokens ++ [close_token], layout}
  end

  # Map update stuck to previous token
  defp compile_arg_with_adhesion({:map_update, {:kw, base_expr, pipe_newlines, pairs}}, layout, opts) do
    {map_meta, layout} = TokenLayout.stick_right(layout, "%{", nil)
    map_token = {:%{}, map_meta}

    {base_tokens, layout} = compile_arg_with_adhesion(base_expr, layout, opts)

    {pipe_meta, layout} = TokenLayout.space_before(layout, "|", nil)
    pipe_token = {:pipe_op, pipe_meta, :|}
    {eol_tokens, layout} = compile_newlines(pipe_newlines, layout)

    {pairs_tokens, layout} = compile_kw_pairs(pairs, layout, opts)

    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[map_token] ++ base_tokens ++ [pipe_token] ++ eol_tokens ++ pairs_tokens ++ [close_token], layout}
  end

  defp compile_arg_with_adhesion({:map_update, {:assoc, base_expr, pipe_newlines, pairs}}, layout, opts) do
    {map_meta, layout} = TokenLayout.stick_right(layout, "%{", nil)
    map_token = {:%{}, map_meta}

    {base_tokens, layout} = compile_arg_with_adhesion(base_expr, layout, opts)

    {pipe_meta, layout} = TokenLayout.space_before(layout, "|", nil)
    pipe_token = {:pipe_op, pipe_meta, :|}
    {eol_tokens, layout} = compile_newlines(pipe_newlines, layout)

    {pairs_tokens, layout} = compile_assoc_pairs(pairs, layout, opts)

    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[map_token] ++ base_tokens ++ [pipe_token] ++ eol_tokens ++ pairs_tokens ++ [close_token], layout}
  end

  # Struct literals stuck to previous token
  defp compile_arg_with_adhesion({:struct, struct_name, eol_count, body}, layout, opts) do
    {percent_meta, layout} = TokenLayout.stick_right(layout, "%", nil)
    percent_token = {:%, percent_meta}

    {name_tokens, layout} = compile_arg_with_adhesion(struct_name, layout, opts)

    {eol_tokens, layout} = compile_newlines(eol_count, layout)

    {open_meta, layout} =
      if eol_count > 0, do: TokenLayout.space_before(layout, "{", nil), else: TokenLayout.stick_right(layout, "{", nil)
    open_token = {:"{", open_meta}

    {body_tokens, layout} = compile_struct_body(body, layout, opts)

    {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
    close_token = {:"}", close_meta}

    {[percent_token] ++ name_tokens ++ eol_tokens ++ [open_token] ++ body_tokens ++ [close_token], layout}
  end

  # Anonymous struct stuck to previous token
  defp compile_arg_with_adhesion({:anon_struct, base_expr, dot_newlines, body}, layout, opts) do
    {base_tokens, layout} = compile_arg_with_adhesion(base_expr, layout, opts)

    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    {eol_tokens, layout} = compile_newlines(dot_newlines, layout)

    case body do
      [] ->
        {map_meta, layout} =
          if dot_newlines > 0, do: TokenLayout.space_before(layout, "%{}", nil), else: TokenLayout.stick_right(layout, "%{}", nil)
        map_token = {:%{}, map_meta}
        {base_tokens ++ [dot_token] ++ eol_tokens ++ [map_token], layout}

      {:kw, pairs} ->
        {map_meta, layout} =
          if dot_newlines > 0, do: TokenLayout.space_before(layout, "%{", nil), else: TokenLayout.stick_right(layout, "%{", nil)
        map_token = {:%{}, map_meta}
        {pairs_tokens, layout} = compile_kw_pairs(pairs, layout, opts)
        {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
        close_token = {:"}", close_meta}
        {base_tokens ++ [dot_token] ++ eol_tokens ++ [map_token] ++ pairs_tokens ++ [close_token], layout}

      {:assoc, pairs} ->
        {map_meta, layout} =
          if dot_newlines > 0, do: TokenLayout.space_before(layout, "%{", nil), else: TokenLayout.stick_right(layout, "%{", nil)
        map_token = {:%{}, map_meta}
        {pairs_tokens, layout} = compile_assoc_pairs(pairs, layout, opts)
        {close_meta, layout} = TokenLayout.stick_right(layout, "}", nil)
        close_token = {:"}", close_meta}
        {base_tokens ++ [dot_token] ++ eol_tokens ++ [map_token] ++ pairs_tokens ++ [close_token], layout}
    end
  end

  # Bitstring stuck to previous token
  defp compile_arg_with_adhesion({:bitstring, []}, layout, _opts) do
    {open_meta, layout} = TokenLayout.stick_right(layout, "<<", nil)
    open_token = {:"<<", open_meta}

    {close_meta, layout} = TokenLayout.stick_right(layout, ">>", nil)
    close_token = {:">>", close_meta}

    {[open_token, close_token], layout}
  end

  defp compile_arg_with_adhesion({:bitstring, args}, layout, opts) do
    {open_meta, layout} = TokenLayout.stick_right(layout, "<<", nil)
    open_token = {:"<<", open_meta}

    {args_tokens, layout} = compile_args_in_brackets(args, layout, opts)

    {close_meta, layout} = TokenLayout.stick_right(layout, ">>", nil)
    close_token = {:">>", close_meta}

    {[open_token] ++ args_tokens ++ [close_token], layout}
  end

  # String stuck to previous token
  defp compile_arg_with_adhesion({:bin_string, ""}, layout, _opts) do
    {str_meta, layout} = TokenLayout.stick_right(layout, "\"\"", nil)
    str_token = {:bin_string, str_meta, [""]}

    {[str_token], layout}
  end

  defp compile_arg_with_adhesion({:bin_string, content}, layout, _opts) when is_binary(content) do
    lexeme = "\"" <> content <> "\""
    {str_meta, layout} = TokenLayout.stick_right(layout, lexeme, nil)
    str_token = {:bin_string, str_meta, [content]}

    {[str_token], layout}
  end

  # Bracket access stuck to previous token
  defp compile_arg_with_adhesion({:bracket_expr, {:bracket_identifier, name}, arg}, layout, opts) do
    name_str = Atom.to_string(name)
    chars = String.to_charlist(name_str)
    {id_meta, layout} = TokenLayout.stick_right(layout, name_str, chars)
    id_token = {:bracket_identifier, id_meta, name}

    {bracket_tokens, layout} = compile_bracket_arg(arg, layout, opts)

    {[id_token] ++ bracket_tokens, layout}
  end

  defp compile_arg_with_adhesion({:bracket_expr, {:expr, expr}, arg}, layout, opts) do
    {expr_tokens, layout} = compile_arg_with_adhesion(expr, layout, opts)
    {bracket_tokens, layout} = compile_bracket_arg_stuck(arg, layout, opts)

    {expr_tokens ++ bracket_tokens, layout}
  end

  # Bracket at with identifier stuck to previous token: @foo[bar]
  defp compile_arg_with_adhesion(
         {:bracket_at_expr, newlines, {:bracket_identifier, name}, arg},
         layout,
         opts
       )
       when is_integer(newlines) do
    # @ stuck to previous
    {at_meta, layout} = TokenLayout.stick_right(layout, "@", nil)
    at_token = {:at_op, at_meta, :@}

    # Emit newlines if any
    {eol_tokens, layout} =
      if newlines > 0 do
        eol_meta = TokenLayout.meta(layout, "\n", newlines)
        layout = TokenLayout.newlines(layout, newlines)
        {[{:eol, eol_meta}], layout}
      else
        {[], layout}
      end

    # Identifier stuck to @
    name_str = Atom.to_string(name)
    chars = String.to_charlist(name_str)
    {id_meta, layout} = TokenLayout.stick_right(layout, name_str, chars)
    id_token = {:bracket_identifier, id_meta, name}

    # Bracket arg
    {bracket_tokens, layout} = compile_bracket_arg(arg, layout, opts)

    {[at_token] ++ eol_tokens ++ [id_token] ++ bracket_tokens, layout}
  end

  # Bracket at with expression stuck to previous token: @(expr)[bar]
  defp compile_arg_with_adhesion({:bracket_at_expr, newlines, {:expr, expr}, arg}, layout, opts)
       when is_integer(newlines) do
    # @ stuck to previous
    {at_meta, layout} = TokenLayout.stick_right(layout, "@", nil)
    at_token = {:at_op, at_meta, :@}

    # Emit newlines if any
    {eol_tokens, layout} =
      if newlines > 0 do
        eol_meta = TokenLayout.meta(layout, "\n", newlines)
        layout = TokenLayout.newlines(layout, newlines)
        {[{:eol, eol_meta}], layout}
      else
        {[], layout}
      end

    # Expression stuck to @
    {expr_tokens, layout} = compile_arg_with_adhesion(expr, layout, opts)

    # Bracket arg stuck to expression
    {bracket_tokens, layout} = compile_bracket_arg_stuck(arg, layout, opts)

    {[at_token] ++ eol_tokens ++ expr_tokens ++ bracket_tokens, layout}
  end

  # At operator stuck to previous token: used in contexts like `foo @bar` or
  # in operator adhesion positions. This mirrors do_to_tokens/3 for at_op but
  # preserves stickiness when compiling into a larger stuck expression.
  defp compile_arg_with_adhesion({:at_op, newlines, operand}, layout, opts)
       when is_integer(newlines) do
    # @ stuck to previous
    {at_meta, layout} = TokenLayout.stick_right(layout, "@", nil)
    at_token = {:at_op, at_meta, :@}

    # Emit newlines if any
    {eol_tokens, layout} =
      if newlines > 0 do
        eol_meta = TokenLayout.meta(layout, "\n", newlines)
        layout = TokenLayout.newlines(layout, newlines)
        {[{:eol, eol_meta}], layout}
      else
        {[], layout}
      end

    # Compile operand stuck to @ (adhesion)
    {operand_tokens, layout} = compile_arg_with_adhesion(operand, layout, opts)

    {[at_token] ++ eol_tokens ++ operand_tokens, layout}
  end

  defp compile_arg_with_adhesion(other, layout, opts) do
    # Fallback: use do_to_tokens (may add unwanted space in some cases)
    do_to_tokens(other, layout, opts)
  end

  # Compile binary operator with left operand stuck to current position
  defp compile_binary_op_stuck(left, {:op_eol, {op_kind, op}, newlines}, right, layout, opts) do
    # Compile left operand stuck to current position
    {left_tokens, layout} = compile_arg_with_adhesion(left, layout, opts)

    # Compile operator
    op_lexeme = op_to_lexeme(op)
    {op_meta, layout} = TokenLayout.space_before(layout, op_lexeme, nil)
    op_token = {op_kind, op_meta, op}

    # Handle newlines after operator
    {eol_tokens, layout} =
      if newlines > 0 do
        eol_meta = TokenLayout.meta(layout, "\n", newlines)
        layout = TokenLayout.newlines(layout, newlines)
        {[{:eol, eol_meta}], layout}
      else
        {[], layout}
      end

    # Compile right operand
    {right_tokens, layout} = do_to_tokens(right, layout, opts)

    {left_tokens ++ [op_token] ++ eol_tokens ++ right_tokens, layout}
  end

  # Compile unary operator stuck to current position
  defp compile_unary_op_stuck({op_kind, op}, operand, layout, opts) do
    op_lexeme = op_to_lexeme(op)
    {op_meta, layout} = TokenLayout.stick_right(layout, op_lexeme, nil)
    op_token = {op_kind, op_meta, op}

    # Compile operand with adhesion (stuck to operator)
    {operand_tokens, layout} = compile_arg_with_adhesion(operand, layout, opts)

    {[op_token] ++ operand_tokens, layout}
  end

  # Compile unary operator stuck to current position with newlines
  defp compile_unary_op_stuck_with_newlines({op_kind, op}, newlines, operand, layout, opts) do
    op_lexeme = op_to_lexeme(op)
    {op_meta, layout} = TokenLayout.stick_right(layout, op_lexeme, nil)
    op_token = {op_kind, op_meta, op}

    # Emit newlines if any
    {eol_tokens, layout} =
      if newlines > 0 do
        eol_meta = TokenLayout.meta(layout, "\n", newlines)
        layout = TokenLayout.newlines(layout, newlines)
        {[{:eol, eol_meta}], layout}
      else
        {[], layout}
      end

    # Compile operand with adhesion (stuck to operator or after newline)
    {operand_tokens, layout} = compile_arg_with_adhesion(operand, layout, opts)

    {[op_token] ++ eol_tokens ++ operand_tokens, layout}
  end

  # Compile call target stuck to current position (no leading space)
  defp compile_call_target_stuck({:paren_identifier, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.stick_right(layout, name, chars)
    {[{:paren_identifier, meta, atom}], layout}
  end

  defp compile_call_target_stuck({:dot_call, expr}, layout, opts) do
    # Compile expression stuck
    {expr_tokens, layout} = compile_arg_with_adhesion(expr, layout, opts)

    # Compile dot stuck to expression
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    {expr_tokens ++ [dot_token], layout}
  end

  # dot_paren_identifier stuck: matched_expr.paren_identifier
  defp compile_call_target_stuck({:dot_paren_identifier, left, right_name}, layout, opts) do
    # Compile left expression stuck
    {left_tokens, layout} = compile_arg_with_adhesion(left, layout, opts)

    # Compile dot (stuck to left)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile right side as paren_identifier (stuck to dot)
    right_str = Atom.to_string(right_name)
    chars = String.to_charlist(right_str)
    {right_meta, layout} = TokenLayout.stick_right(layout, right_str, chars)
    right_token = {:paren_identifier, right_meta, right_name}

    {left_tokens ++ [dot_token, right_token], layout}
  end

  defp compile_call_target_stuck({:identifier, atom}, layout, _opts) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)
    {meta, layout} = TokenLayout.stick_right(layout, name, chars)
    {[{:identifier, meta, atom}], layout}
  end

  # ===========================================================================
  # Helper: op_to_lexeme
  # ===========================================================================

  defp op_to_lexeme(op) when is_atom(op), do: Atom.to_string(op)

  # ===========================================================================
  # Helper: compile_forms
  # ===========================================================================

  # Compile a list of forms with EOL separators
  defp compile_forms([], layout, _opts), do: {[], layout}

  defp compile_forms([form], layout, opts) do
    do_to_tokens(form, layout, opts)
  end

  defp compile_forms([form | rest], layout, opts) do
    {form_tokens, layout} = do_to_tokens(form, layout, opts)

    # Add EOL token between forms
    # The EOL token's meta spans from current position to next line
    # stick_right doesn't add the newline to position, we do it explicitly after
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)

    {rest_tokens, layout} = compile_forms(rest, layout, opts)

    {form_tokens ++ [eol_token] ++ rest_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_forms_with_eoe (explicit eoe markers - legacy)
  # ===========================================================================

  # Compile forms with explicit eoe markers (legacy format where every form has eoe)
  defp compile_forms_with_eoe([], layout, _opts), do: {[], layout}

  defp compile_forms_with_eoe([{form, eoe}], layout, opts) do
    {form_tokens, layout} = do_to_tokens(form, layout, opts)
    {eoe_tokens, layout} = compile_eoe(eoe, layout)
    {form_tokens ++ eoe_tokens, layout}
  end

  defp compile_forms_with_eoe([{form, eoe} | rest], layout, opts) do
    {form_tokens, layout} = do_to_tokens(form, layout, opts)
    {eoe_tokens, layout} = compile_eoe(eoe, layout)
    {rest_tokens, layout} = compile_forms_with_eoe(rest, layout, opts)
    {form_tokens ++ eoe_tokens ++ rest_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_expr_list (grammar v2 format)
  # ===========================================================================

  # Compile expr_list per grammar rules:
  #   expr_list -> expr
  #   expr_list -> expr_list eoe expr
  #
  # The eoe goes BETWEEN expressions. Last expr has eoe = nil.
  # Format: [{expr, eoe | nil}, ...]

  defp compile_expr_list([], layout, _opts), do: {[], layout}

  defp compile_expr_list([{expr, nil}], layout, opts) do
    # Last expression, no eoe after it
    do_to_tokens(expr, layout, opts)
  end

  defp compile_expr_list([{expr, eoe} | rest], layout, opts) when eoe != nil do
    # Expression with eoe after it (between this and next)
    {expr_tokens, layout} = do_to_tokens(expr, layout, opts)
    {eoe_tokens, layout} = compile_eoe(eoe, layout)
    {rest_tokens, layout} = compile_expr_list(rest, layout, opts)
    {expr_tokens ++ eoe_tokens ++ rest_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_eoe (end-of-expression markers)
  # ===========================================================================

  # eoe -> eol (newline only)
  defp compile_eoe(:eol, layout) do
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    layout = TokenLayout.newline(layout)
    {[{:eol, eol_meta}], layout}
  end

  # eoe -> ';' (semicolon only)
  defp compile_eoe(:semi, layout) do
    {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
    {[{:";", semi_meta}], layout}
  end

  # eoe -> eol ';' (newline followed by semicolon)
  defp compile_eoe(:eol_semi, layout) do
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    layout = TokenLayout.newline(layout)
    {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
    {[{:eol, eol_meta}, {:";", semi_meta}], layout}
  end

  # ===========================================================================
  # Helper: compile_stab_clause
  # ===========================================================================

  # Compile a stab clause: pattern -> body (or pattern when guard -> body)
  # Pattern can be :empty, {:single, expr}, or {:many, [expr]}
  # Guard can be nil or an expression
  # stab_op_eol is the number of newlines after -> (0 = inline)
  #
  # New 5-element format: {:stab_clause, pattern, guard, stab_op_eol, body}
  # Old 4-element format: {:stab_clause, pattern, guard, body} (backward compat)

  # Backward compatibility: convert old 4-element format to new 5-element format
  defp compile_stab_clause({:stab_clause, pattern, guard, body}, layout, opts) do
    # Default stab_op_eol to 0 (inline) for backward compatibility
    compile_stab_clause({:stab_clause, pattern, guard, 0, body}, layout, opts)
  end

  # Stab clause with guard: pattern when guard -> body
  defp compile_stab_clause({:stab_clause, pattern, guard, stab_op_eol, body}, layout, opts)
       when guard != nil do
    # Compile pattern (if any)
    {pattern_tokens, layout} = compile_pattern(pattern, layout, opts)

    # Compile 'when' keyword
    {when_meta, layout} = TokenLayout.space_before(layout, "when", nil)
    when_token = {:when_op, when_meta, :when}

    # Compile guard expression
    {guard_tokens, layout} = do_to_tokens(guard, layout, opts)

    # Compile stab operator ->
    {stab_meta, layout} = TokenLayout.space_before(layout, "->", nil)
    stab_token = {:stab_op, stab_meta, :->}

    # Compile stab_op_eol (newlines after ->)
    {eol_tokens, layout} = compile_stab_op_eol(stab_op_eol, layout)

    # Compile body (may be :empty_body per grammar line 366)
    {body_tokens, layout} = compile_stab_body_expr(body, layout, opts)

    {pattern_tokens ++ [when_token] ++ guard_tokens ++ [stab_token] ++ eol_tokens ++ body_tokens,
     layout}
  end

  # Stab clause without guard: pattern -> body
  defp compile_stab_clause({:stab_clause, pattern, nil, stab_op_eol, body}, layout, opts) do
    # Compile pattern (if any)
    {pattern_tokens, layout} = compile_pattern(pattern, layout, opts)

    # Compile stab operator ->
    {stab_meta, layout} = TokenLayout.space_before(layout, "->", nil)
    stab_token = {:stab_op, stab_meta, :->}

    # Compile stab_op_eol (newlines after ->)
    {eol_tokens, layout} = compile_stab_op_eol(stab_op_eol, layout)

    # Compile body (may be :empty_body per grammar line 366)
    {body_tokens, layout} = compile_stab_body_expr(body, layout, opts)

    {pattern_tokens ++ [stab_token] ++ eol_tokens ++ body_tokens, layout}
  end

  # Compile stab body expression - handles :empty_body
  # Per grammar lines 365-366:
  #   stab_op_eol_and_expr -> stab_op_eol expr : {'$1', '$2'}.
  #   stab_op_eol_and_expr -> stab_op_eol : warn_empty_stab_clause('$1'), {'$1', handle_literal(nil, '$1')}.
  #
  # When body is :empty_body, we emit no tokens (producing source like "x ->" with no body).
  # The parser will warn and synthesize a nil value. We intentionally do NOT emit a "nil"
  # token here because:
  # 1. We want to generate the actual problematic source that triggers the warning
  # 2. This tests that both parsers (Oracle and Toxic) handle the edge case correctly
  # 3. Emitting "nil" would produce different source ("x -> nil") which is normal valid code
  defp compile_stab_body_expr(:empty_body, layout, _opts), do: {[], layout}

  defp compile_stab_body_expr(body, layout, opts) do
    do_to_tokens(body, layout, opts)
  end

  # Compile stab_op_eol: newlines after -> operator
  # Per grammar lines 460-461:
  #   stab_op_eol -> stab_op : '$1'.
  #   stab_op_eol -> stab_op eol : next_is_eol('$1', '$2').
  #
  # The grammar's `next_is_eol` action (lines 834-836) copies the newline count
  # from the eol token to the operator token's location for AST metadata. At the
  # token level, we emit a separate eol token with the newline count. The count
  # can be > 1 for multiple consecutive newlines (tokenized as single eol token).
  defp compile_stab_op_eol(0, layout), do: {[], layout}

  defp compile_stab_op_eol(newlines, layout) when newlines > 0 do
    # Emit eol token with newline count in metadata
    eol_meta = TokenLayout.meta(layout, "\n", newlines)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newlines(layout, newlines)
    {[eol_token], layout}
  end

  # ===========================================================================
  # Helper: compile_pattern
  # ===========================================================================

  # Empty pattern (no arguments): fn -> ... end
  # Per grammar: stab_op_eol_and_expr (no pattern before ->)
  defp compile_pattern(:empty, layout, _opts), do: {[], layout}

  # Empty parens pattern: fn () -> ... end
  # Per grammar: empty_paren stab_op_eol_and_expr
  defp compile_pattern(:empty_parens, layout, _opts) do
    # Compile '('
    {open_meta, layout} = TokenLayout.space_before(layout, "(", nil)
    open_token = {:"(", open_meta}

    # Compile ')' - stuck to open paren
    {close_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    close_token = {:")", close_meta}

    {[open_token, close_token], layout}
  end

  # Single pattern: fn x -> ... end
  defp compile_pattern({:single, expr}, layout, opts) do
    do_to_tokens(expr, layout, opts)
  end

  # Multiple patterns without parens: fn x, y -> ... end
  # Per grammar: call_args_no_parens_all stab_op_eol_and_expr
  defp compile_pattern({:many, exprs}, layout, opts) do
    compile_pattern_list(exprs, layout, opts)
  end

  # Multiple patterns with parens: fn (x, y) -> ... end
  # Per grammar lines 528-529 (stab_parens_many):
  #   stab_parens_many -> open_paren call_args_no_parens_kw close_paren
  #   stab_parens_many -> open_paren call_args_no_parens_many close_paren
  defp compile_pattern({:many_parens, exprs}, layout, opts) do
    # Compile '('
    {open_meta, layout} = TokenLayout.space_before(layout, "(", nil)
    open_token = {:"(", open_meta}

    # Compile patterns inside parens (first stuck to open paren)
    {patterns_tokens, layout} = compile_pattern_list_in_parens(exprs, layout, opts)

    # Compile ')' - stuck to last pattern
    {close_meta, layout} = TokenLayout.stick_right(layout, ")", nil)
    close_token = {:")", close_meta}

    {[open_token] ++ patterns_tokens ++ [close_token], layout}
  end

  # Compile a list of patterns with comma separators
  defp compile_pattern_list([], layout, _opts), do: {[], layout}

  defp compile_pattern_list([expr], layout, opts) do
    do_to_tokens(expr, layout, opts)
  end

  defp compile_pattern_list([expr | rest], layout, opts) do
    {expr_tokens, layout} = do_to_tokens(expr, layout, opts)

    # Add comma token
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    # Compile remaining patterns
    {rest_tokens, layout} = compile_pattern_list(rest, layout, opts)

    {expr_tokens ++ [comma_token] ++ rest_tokens, layout}
  end

  # Compile a list of patterns inside parens (first stuck to open paren)
  defp compile_pattern_list_in_parens([], layout, _opts), do: {[], layout}

  defp compile_pattern_list_in_parens([expr], layout, opts) do
    # Single expr stuck to open paren (no space)
    compile_arg_with_adhesion(expr, layout, opts)
  end

  defp compile_pattern_list_in_parens([expr | rest], layout, opts) do
    # First expr stuck to open paren
    {expr_tokens, layout} = compile_arg_with_adhesion(expr, layout, opts)

    # Add comma token
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    # Compile remaining patterns with normal spacing
    {rest_tokens, layout} = compile_pattern_list(rest, layout, opts)

    {expr_tokens ++ [comma_token] ++ rest_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_stab_clauses (multiple clauses with semicolon separators)
  # ===========================================================================

  # Compile multiple stab clauses with semicolon separators
  defp compile_stab_clauses([], layout, _opts), do: {[], layout}

  defp compile_stab_clauses([clause], layout, opts) do
    case clause do
      {:stab_expr_bare, expr} ->
        # Bare expr as stab entry: compile the expr and add trailing newline
        {expr_tokens, layout} = do_to_tokens(expr, layout, opts)

        # Add trailing newline
        eol_meta = TokenLayout.meta(layout, "\n", 1)
        eol_token = {:eol, eol_meta}
        layout = TokenLayout.newline(layout)

        {expr_tokens ++ [eol_token], layout}

      _ ->
        compile_stab_clause(clause, layout, opts)
    end
  end

  defp compile_stab_clauses([clause | rest], layout, opts) do
    # Compile first clause (supports bare expr variant)
    {clause_tokens, layout} =
      case clause do
        {:stab_expr_bare, expr} ->
          {tokens, layout} = do_to_tokens(expr, layout, opts)

          # Add semicolon separator after bare expr when followed by more clauses
          {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
          semi_token = {:";", semi_meta}

          {List.flatten([tokens]) ++ [semi_token], layout}

        _ ->
          {clause_toks, layout} = compile_stab_clause(clause, layout, opts)

          # Add semicolon separator after stab clause when followed by more clauses
          {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
          semi_token = {:";", semi_meta}

          {clause_toks ++ [semi_token], layout}
      end

    # Compile remaining clauses
    {rest_tokens, layout} = compile_stab_clauses(rest, layout, opts)

    {clause_tokens ++ rest_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_stab_eoe (stab clauses with trailing eoe)
  # ===========================================================================

  # Per grammar lines 347-348:
  #   stab_eoe -> stab : '$1'.
  #   stab_eoe -> stab eoe : annotate_eoe('$2', '$1').
  #
  # stab_eoe is a list of stab clauses with an optional trailing eoe
  # Structure: clauses (list), trailing_eoe (:none, :eol, :semi)
  defp compile_stab_eoe(clauses, trailing_eoe, layout, opts) do
    # Compile the stab clauses with semicolon separators
    {clauses_tokens, layout} = compile_stab_clauses(clauses, layout, opts)

    # Add trailing eoe if needed
    {trailing_tokens, layout} = compile_trailing_eoe(trailing_eoe, layout)

    {clauses_tokens ++ trailing_tokens, layout}
  end

  # Compile trailing eoe (end-of-expression) for stab_eoe
  defp compile_trailing_eoe(:none, layout), do: {[], layout}

  defp compile_trailing_eoe(:eol, layout) do
    # Trailing newline
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)
    {[eol_token], layout}
  end

  defp compile_trailing_eoe(:semi, layout) do
    # Trailing semicolon
    {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
    semi_token = {:";", semi_meta}
    {[semi_token], layout}
  end

  # Per grammar line 333: eoe -> eol ';' (newline followed by semicolon)
  defp compile_trailing_eoe(:eol_semi, layout) do
    # Trailing newline then semicolon
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)
    {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
    semi_token = {:";", semi_meta}
    {[eol_token, semi_token], layout}
  end

  # ===========================================================================
  # Helper: compile_do_args (arguments for do blocks)
  # ===========================================================================

  # Compile arguments for do block (if/unless take one arg, case takes an expression)
  defp compile_do_args([], layout, _opts), do: {[], layout}

  defp compile_do_args([arg], layout, opts) do
    do_to_tokens(arg, layout, opts)
  end

  defp compile_do_args([arg | rest], layout, opts) do
    {arg_tokens, layout} = do_to_tokens(arg, layout, opts)
    {rest_tokens, layout} = compile_do_args(rest, layout, opts)
    {arg_tokens ++ rest_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_fn_eoe (what comes after 'fn')
  # ===========================================================================

  # Compile fn_eoe (what comes after 'fn')
  # Per grammar lines 335-336:
  #   fn_eoe -> 'fn'      (inline - no eol)
  #   fn_eoe -> 'fn' eoe  (eoe = eol | ';' | eol ';')
  defp compile_fn_eoe(:none, layout, _opts) do
    # Inline form: fn x -> x end (just a space, no eol)
    {[], layout}
  end

  defp compile_fn_eoe(:eol, layout, _opts) do
    # Newline form: fn\nx -> x\nend
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)
    {[eol_token], layout}
  end

  defp compile_fn_eoe(:semi, layout, _opts) do
    # Semicolon form: fn; x -> x end
    {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
    semi_token = {:";", semi_meta}
    {[semi_token], layout}
  end

  defp compile_fn_eoe(:eol_semi, layout, _opts) do
    # eol followed by semicolon form: fn\n; x -> x end
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)
    {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
    semi_token = {:";", semi_meta}
    {[eol_token, semi_token], layout}
  end

  # ===========================================================================
  # Helper: compile_do_body (body of do block)
  # ===========================================================================

  # Compile do block body - can be a list of expressions or stab clauses
  # Compile do_eoe (what comes after 'do')
  # Per grammar lines 338-339:
  #   do_eoe -> 'do'      (inline - no eol)
  #   do_eoe -> 'do' eoe  (eoe = eol | ';' | eol ';')
  defp compile_do_eoe(:none, layout, _opts) do
    # Inline form: do expr end (just a space, no eol)
    {[], layout}
  end

  defp compile_do_eoe(:eol, layout, _opts) do
    # Newline form: do\nexpr\nend
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)
    {[eol_token], layout}
  end

  defp compile_do_eoe(:semi, layout, _opts) do
    # Semicolon form: do; expr end
    {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
    semi_token = {:";", semi_meta}
    {[semi_token], layout}
  end

  defp compile_do_eoe(:eol_semi, layout, _opts) do
    # eol followed by semicolon form: do\n; expr end
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)
    {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
    semi_token = {:";", semi_meta}
    {[eol_token, semi_token], layout}
  end

  defp compile_do_body([], layout, _opts), do: {[], layout}

  # Handle parser-style empty body {:__block__, [], []} - produces no tokens
  # This provides literal parity with parser output for empty block items
  defp compile_do_body({:__block__, [], []}, layout, _opts), do: {[], layout}

  # Handle stab_eoe structure: {:stab_eoe, clauses, trailing_eoe}
  defp compile_do_body({:stab_eoe, clauses, trailing_eoe}, layout, opts) do
    compile_stab_eoe_body(clauses, trailing_eoe, layout, opts)
  end

  # Handle stab clauses (for case expressions)
  # 5-element format: {:stab_clause, pattern, guard, stab_op_eol, body}
  defp compile_do_body([{:stab_clause, _, _, _, _} = clause | rest], layout, opts) do
    compile_stab_body([clause | rest], layout, opts)
  end

  # 4-element format (backward compat): {:stab_clause, pattern, guard, body}
  defp compile_do_body([{:stab_clause, _, _, _} = clause | rest], layout, opts) do
    compile_stab_body([clause | rest], layout, opts)
  end

  defp compile_do_body([expr], layout, opts) do
    {expr_tokens, layout} = do_to_tokens(expr, layout, opts)

    # Add trailing newline
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)

    {expr_tokens ++ [eol_token], layout}
  end

  defp compile_do_body([expr | rest], layout, opts) do
    {expr_tokens, layout} = do_to_tokens(expr, layout, opts)

    # Add EOL between expressions
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)

    {rest_tokens, layout} = compile_do_body(rest, layout, opts)

    {expr_tokens ++ [eol_token] ++ rest_tokens, layout}
  end

  # Compile stab clauses in do block body (for case expressions)
  defp compile_stab_body([], layout, _opts), do: {[], layout}

  defp compile_stab_body([clause], layout, opts) do
    {clause_tokens, layout} = compile_stab_clause(clause, layout, opts)

    # Add trailing newline
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)

    {clause_tokens ++ [eol_token], layout}
  end

  defp compile_stab_body([clause | rest], layout, opts) do
    {clause_tokens, layout} = compile_stab_clause(clause, layout, opts)

    # Add newline between clauses
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)

    {rest_tokens, layout} = compile_stab_body(rest, layout, opts)

    {clause_tokens ++ [eol_token] ++ rest_tokens, layout}
  end

  # Compile stab_eoe in do block body
  # This handles {:stab_eoe, clauses, trailing_eoe} structure
  defp compile_stab_eoe_body(clauses, trailing_eoe, layout, opts) do
    # Compile the stab clauses with newline separators (for do blocks)
    {clauses_tokens, layout} = compile_stab_body_clauses(clauses, layout, opts)

    # Add trailing eoe based on type
    {trailing_tokens, layout} =
      case trailing_eoe do
        :none ->
          # No trailing eoe, but still add final newline before 'end'
          eol_meta = TokenLayout.meta(layout, "\n", 1)
          eol_token = {:eol, eol_meta}
          layout = TokenLayout.newline(layout)
          {[eol_token], layout}

        :eol ->
          # Trailing newline (same as :none for do body)
          eol_meta = TokenLayout.meta(layout, "\n", 1)
          eol_token = {:eol, eol_meta}
          layout = TokenLayout.newline(layout)
          {[eol_token], layout}

        :semi ->
          # Trailing semicolon
          {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
          semi_token = {:";", semi_meta}
          # Still need newline before 'end'
          eol_meta = TokenLayout.meta(layout, "\n", 1)
          eol_token = {:eol, eol_meta}
          layout = TokenLayout.newline(layout)
          {[semi_token, eol_token], layout}

        :eol_semi ->
          # Trailing newline followed by semicolon, then newline before 'end'
          eol_meta1 = TokenLayout.meta(layout, "\n", 1)
          eol_token1 = {:eol, eol_meta1}
          layout = TokenLayout.newline(layout)
          {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
          semi_token = {:";", semi_meta}
          eol_meta2 = TokenLayout.meta(layout, "\n", 1)
          eol_token2 = {:eol, eol_meta2}
          layout = TokenLayout.newline(layout)
          {[eol_token1, semi_token, eol_token2], layout}
      end

    {clauses_tokens ++ trailing_tokens, layout}
  end

  # Helper to compile stab clauses in do body with newline separators
  defp compile_stab_body_clauses([], layout, _opts), do: {[], layout}

  defp compile_stab_body_clauses([clause], layout, opts) do
    compile_stab_clause(clause, layout, opts)
  end

  defp compile_stab_body_clauses([clause | rest], layout, opts) do
    {clause_tokens, layout} = compile_stab_clause(clause, layout, opts)

    # Add newline between clauses
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)

    {rest_tokens, layout} = compile_stab_body_clauses(rest, layout, opts)

    {clause_tokens ++ [eol_token] ++ rest_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_block_items (else, rescue, catch, after)
  # Per grammar lines 368-374:
  #   block_item -> block_eoe stab_eoe    (with stab clauses)
  #   block_item -> block_eoe             (empty body)
  #   block_list -> block_item | block_item block_list
  # block_eoe (grammar lines 341-342):
  #   block_eoe -> block_identifier       (inline)
  #   block_eoe -> block_identifier eoe   (eoe = eol | ';' | eol ';')
  # ===========================================================================

  # Compile block items (empty for basic do blocks)
  defp compile_block_items([], layout, _opts), do: {[], layout}

  # New 4-element format with block_eoe: {:block_item, block_type, block_eoe, body}
  defp compile_block_items([{:block_item, block_type, block_eoe, body} | rest], layout, opts) do
    block_name = Atom.to_string(block_type)

    # Compile block_identifier keyword
    {kw_meta, layout} = TokenLayout.space_before(layout, block_name, nil)
    kw_token = {:block_identifier, kw_meta, block_type}

    # Compile block_eoe (what comes after block_identifier)
    {eoe_tokens, layout} = compile_block_eoe(block_eoe, layout, opts)

    # Compile block body
    {body_tokens, layout} = compile_do_body(body, layout, opts)

    # Continue with remaining block items
    {rest_tokens, layout} = compile_block_items(rest, layout, opts)

    {[kw_token] ++ eoe_tokens ++ body_tokens ++ rest_tokens, layout}
  end

  # Backward compat: 3-element format {:block_item, :else, body}
  defp compile_block_items([{:block_item, :else, body} | rest], layout, opts) do
    # Compile 'else' keyword
    {else_meta, layout} = TokenLayout.space_before(layout, "else", nil)
    else_token = {:block_identifier, else_meta, :else}

    # Newline after else
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)

    # Compile else body
    {body_tokens, layout} = compile_do_body(body, layout, opts)

    # Continue with remaining block items
    {rest_tokens, layout} = compile_block_items(rest, layout, opts)

    {[else_token, eol_token] ++ body_tokens ++ rest_tokens, layout}
  end

  # Backward compat: 3-element format {:block_item, :rescue, body}
  defp compile_block_items([{:block_item, :rescue, body} | rest], layout, opts) do
    # Compile 'rescue' keyword
    {rescue_meta, layout} = TokenLayout.space_before(layout, "rescue", nil)
    rescue_token = {:block_identifier, rescue_meta, :rescue}

    # Newline after rescue
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)

    # Compile rescue body (stab clauses)
    {body_tokens, layout} = compile_do_body(body, layout, opts)

    # Continue with remaining block items
    {rest_tokens, layout} = compile_block_items(rest, layout, opts)

    {[rescue_token, eol_token] ++ body_tokens ++ rest_tokens, layout}
  end

  # Backward compat: 3-element format {:block_item, :catch, body}
  defp compile_block_items([{:block_item, :catch, body} | rest], layout, opts) do
    # Compile 'catch' keyword
    {catch_meta, layout} = TokenLayout.space_before(layout, "catch", nil)
    catch_token = {:block_identifier, catch_meta, :catch}

    # Newline after catch
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)

    # Compile catch body (stab clauses)
    {body_tokens, layout} = compile_do_body(body, layout, opts)

    # Continue with remaining block items
    {rest_tokens, layout} = compile_block_items(rest, layout, opts)

    {[catch_token, eol_token] ++ body_tokens ++ rest_tokens, layout}
  end

  # Backward compat: 3-element format {:block_item, :after, body}
  defp compile_block_items([{:block_item, :after, body} | rest], layout, opts) do
    # Compile 'after' keyword
    {after_meta, layout} = TokenLayout.space_before(layout, "after", nil)
    after_token = {:block_identifier, after_meta, :after}

    # Newline after after
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)

    # Compile after body (expressions)
    {body_tokens, layout} = compile_do_body(body, layout, opts)

    # Continue with remaining block items
    {rest_tokens, layout} = compile_block_items(rest, layout, opts)

    {[after_token, eol_token] ++ body_tokens ++ rest_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_block_eoe (what comes after block_identifier)
  # ===========================================================================

  # Compile block_eoe (what comes after block_identifier like else, rescue, catch, after)
  # Per grammar lines 341-342:
  #   block_eoe -> block_identifier       (inline - no eol)
  #   block_eoe -> block_identifier eoe   (eoe = eol | ';' | eol ';')
  defp compile_block_eoe(:none, layout, _opts) do
    # Inline form: else expr (just a space, no eol)
    {[], layout}
  end

  defp compile_block_eoe(:eol, layout, _opts) do
    # Newline form: else\nexpr
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)
    {[eol_token], layout}
  end

  defp compile_block_eoe(:semi, layout, _opts) do
    # Semicolon form: else; expr
    {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
    semi_token = {:";", semi_meta}
    {[semi_token], layout}
  end

  defp compile_block_eoe(:eol_semi, layout, _opts) do
    # eol followed by semicolon form: else\n; expr
    eol_meta = TokenLayout.meta(layout, "\n", 1)
    eol_token = {:eol, eol_meta}
    layout = TokenLayout.newline(layout)
    {semi_meta, layout} = TokenLayout.stick_right(layout, ";", nil)
    semi_token = {:";", semi_meta}
    {[eol_token, semi_token], layout}
  end

  # ===========================================================================
  # Helper: compile_args_in_brackets (for lists and tuples)
  # ===========================================================================

  # Compile arguments inside brackets (first arg stuck to open bracket)
  defp compile_args_in_brackets([], layout, _opts), do: {[], layout}

  defp compile_args_in_brackets([arg | rest], layout, opts) do
    # First argument is stuck to opening bracket (no space)
    {first_tokens, layout} = compile_arg_with_adhesion(arg, layout, opts)

    # Remaining args have commas and spaces
    {rest_tokens, layout} = compile_remaining_bracket_args(rest, layout, opts)

    {first_tokens ++ rest_tokens, layout}
  end

  # Compile remaining arguments with comma separators
  defp compile_remaining_bracket_args([], layout, _opts), do: {[], layout}

  defp compile_remaining_bracket_args([arg | rest], layout, opts) do
    # Add comma token (stuck to previous)
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    # Compile arg with space before
    {arg_tokens, layout} = do_to_tokens(arg, layout, opts)

    # Continue with remaining args
    {rest_tokens, layout} = compile_remaining_bracket_args(rest, layout, opts)

    {[comma_token] ++ arg_tokens ++ rest_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_kw_pairs (for maps with keyword syntax)
  # ===========================================================================

  # Compile keyword pairs: key: value, key2: value2
  defp compile_kw_pairs([], layout, _opts), do: {[], layout}

  defp compile_kw_pairs([{key, value}], layout, opts) do
    # Single pair: key: value (stuck to %{)
    compile_kw_pair_stuck(key, value, layout, opts)
  end

  defp compile_kw_pairs([{key, value} | rest], layout, opts) do
    # First pair stuck to %{
    {first_tokens, layout} = compile_kw_pair_stuck(key, value, layout, opts)

    # Remaining pairs have commas
    {rest_tokens, layout} = compile_remaining_kw_pairs(rest, layout, opts)

    {first_tokens ++ rest_tokens, layout}
  end

  # Compile a single keyword pair stuck to previous token
  defp compile_kw_pair_stuck(key, value, layout, opts) do
    # Key (atom) stuck to previous
    key_str = Atom.to_string(key)
    key_lexeme = key_str <> ":"
    {key_meta, layout} = TokenLayout.stick_right(layout, key_lexeme, nil)
    key_token = {:kw_identifier, key_meta, key}

    # Value with space after colon
    {value_tokens, layout} = do_to_tokens(value, layout, opts)

    {[key_token] ++ value_tokens, layout}
  end

  defp compile_remaining_kw_pairs([], layout, _opts), do: {[], layout}

  defp compile_remaining_kw_pairs([{key, value} | rest], layout, opts) do
    # Comma stuck to previous
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    # Key with space after comma
    key_str = Atom.to_string(key)
    key_lexeme = key_str <> ":"
    {key_meta, layout} = TokenLayout.space_before(layout, key_lexeme, nil)
    key_token = {:kw_identifier, key_meta, key}

    # Value with space after colon
    {value_tokens, layout} = do_to_tokens(value, layout, opts)

    # Continue with remaining pairs
    {rest_tokens, layout} = compile_remaining_kw_pairs(rest, layout, opts)

    {[comma_token, key_token] ++ value_tokens ++ rest_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_assoc_pairs (for maps with arrow syntax)
  # ===========================================================================

  # Compile association pairs: key => value, key2 => value2
  defp compile_assoc_pairs([], layout, _opts), do: {[], layout}

  defp compile_assoc_pairs([{key, value}], layout, opts) do
    # Single pair: key => value (stuck to %{)
    compile_assoc_pair_stuck(key, value, layout, opts)
  end

  defp compile_assoc_pairs([{key, value} | rest], layout, opts) do
    # First pair stuck to %{
    {first_tokens, layout} = compile_assoc_pair_stuck(key, value, layout, opts)

    # Remaining pairs have commas
    {rest_tokens, layout} = compile_remaining_assoc_pairs(rest, layout, opts)

    {first_tokens ++ rest_tokens, layout}
  end

  # Compile a single association pair stuck to previous token
  defp compile_assoc_pair_stuck(key, value, layout, opts) do
    # Key stuck to previous
    {key_tokens, layout} = compile_arg_with_adhesion(key, layout, opts)

    # => operator with space
    {assoc_meta, layout} = TokenLayout.space_before(layout, "=>", nil)
    assoc_token = {:assoc_op, assoc_meta, :"=>"}

    # Value with space
    {value_tokens, layout} = do_to_tokens(value, layout, opts)

    {key_tokens ++ [assoc_token] ++ value_tokens, layout}
  end

  defp compile_remaining_assoc_pairs([], layout, _opts), do: {[], layout}

  defp compile_remaining_assoc_pairs([{key, value} | rest], layout, opts) do
    # Comma stuck to previous
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    # Key with space
    {key_tokens, layout} = do_to_tokens(key, layout, opts)

    # => operator with space
    {assoc_meta, layout} = TokenLayout.space_before(layout, "=>", nil)
    assoc_token = {:assoc_op, assoc_meta, :"=>"}

    # Value with space
    {value_tokens, layout} = do_to_tokens(value, layout, opts)

    # Continue with remaining pairs
    {rest_tokens, layout} = compile_remaining_assoc_pairs(rest, layout, opts)

    {[comma_token] ++ key_tokens ++ [assoc_token] ++ value_tokens ++ rest_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_struct_body (for struct literals)
  # ===========================================================================

  # Compile struct body based on body type
  defp compile_struct_body([], layout, _opts), do: {[], layout}

  defp compile_struct_body({:kw, pairs}, layout, opts) do
    compile_kw_pairs(pairs, layout, opts)
  end

  defp compile_struct_body({:assoc, pairs}, layout, opts) do
    compile_assoc_pairs(pairs, layout, opts)
  end

  defp compile_struct_body({:update_kw, base_expr, pipe_newlines, pairs}, layout, opts) do
    {base_tokens, layout} = compile_arg_with_adhesion(base_expr, layout, opts)

    {pipe_meta, layout} = TokenLayout.space_before(layout, "|", nil)
    pipe_token = {:pipe_op, pipe_meta, :|}
    {eol_tokens, layout} = compile_newlines(pipe_newlines, layout)

    {pairs_tokens, layout} = compile_kw_pairs(pairs, layout, opts)

    {base_tokens ++ [pipe_token] ++ eol_tokens ++ pairs_tokens, layout}
  end

  defp compile_struct_body({:update_assoc, base_expr, pipe_newlines, pairs}, layout, opts) do
    {base_tokens, layout} = compile_arg_with_adhesion(base_expr, layout, opts)

    {pipe_meta, layout} = TokenLayout.space_before(layout, "|", nil)
    pipe_token = {:pipe_op, pipe_meta, :|}
    {eol_tokens, layout} = compile_newlines(pipe_newlines, layout)

    {pairs_tokens, layout} = compile_assoc_pairs(pairs, layout, opts)

    {base_tokens ++ [pipe_token] ++ eol_tokens ++ pairs_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_bracket_arg (for bracket access expressions)
  # ===========================================================================

  # Compile bracket argument: [key]
  # Supports multiple generator shapes produced by gen_bracket_arg/1:
  # - simple expression (e.g., {:identifier, meta, name})
  # - {:kw_args, pairs} for keyword lists (old format)
  # - {:kw_data, pairs} / {:kw_data_trailing, pairs} for keyword data (new format)
  # - list of elements for container args
  # - {:trailing, expr} for trailing-comma form
  defp compile_bracket_arg({:kw_args, pairs}, layout, opts) do
    # Opening bracket (stuck to identifier for adhesion)
    {open_meta, layout} = TokenLayout.stick_right(layout, "[", nil)
    open_token = {:"[", open_meta}

    # Compile keyword pairs inside bracket
    {pairs_tokens, layout} = compile_no_parens_kw_pairs(pairs, layout, opts)

    # Closing bracket (stuck to last value)
    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token] ++ pairs_tokens ++ [close_token], layout}
  end

  # New format: kw_data with kw_eol pairs
  defp compile_bracket_arg({:kw_data, _pairs} = kw_data, layout, opts) do
    {open_meta, layout} = TokenLayout.stick_right(layout, "[", nil)
    open_token = {:"[", open_meta}

    {kw_tokens, layout} = compile_kw_data(kw_data, layout, opts)

    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token] ++ kw_tokens ++ [close_token], layout}
  end

  defp compile_bracket_arg({:kw_data_trailing, _pairs} = kw_data, layout, opts) do
    {open_meta, layout} = TokenLayout.stick_right(layout, "[", nil)
    open_token = {:"[", open_meta}

    {kw_tokens, layout} = compile_kw_data(kw_data, layout, opts)

    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token] ++ kw_tokens ++ [close_token], layout}
  end

  defp compile_bracket_arg(arg, layout, opts) when is_list(arg) do
    # Opening bracket (stuck to identifier for adhesion)
    {open_meta, layout} = TokenLayout.stick_right(layout, "[", nil)
    open_token = {:"[", open_meta}

    # Compile multiple args in brackets
    {args_tokens, layout} = compile_args_in_brackets(arg, layout, opts)

    # Closing bracket (stuck to last arg)
    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token] ++ args_tokens ++ [close_token], layout}
  end

  defp compile_bracket_arg({:trailing, expr}, layout, opts) do
    # Opening bracket (stuck to identifier for adhesion)
    {open_meta, layout} = TokenLayout.stick_right(layout, "[", nil)
    open_token = {:"[", open_meta}

    # Compile single expr (stuck to bracket)
    {arg_tokens, layout} = compile_arg_with_adhesion(expr, layout, opts)

    # Trailing comma before closing bracket
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    # Closing bracket (stuck to comma)
    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token] ++ arg_tokens ++ [comma_token, close_token], layout}
  end

  defp compile_bracket_arg(arg, layout, opts) do
    # Fallback: single key expression
    {open_meta, layout} = TokenLayout.stick_right(layout, "[", nil)
    open_token = {:"[", open_meta}

    # Key expression (stuck to bracket)
    {arg_tokens, layout} = compile_arg_with_adhesion(arg, layout, opts)

    # Closing bracket (stuck to key)
    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token] ++ arg_tokens ++ [close_token], layout}
  end

  # Compile bracket argument stuck to expression: expr[key]
  # Mirrors compile_bracket_arg variants but opening bracket is stuck to expr
  defp compile_bracket_arg_stuck({:kw_args, pairs}, layout, opts) do
    {open_meta, layout} = TokenLayout.stick_right(layout, "[", nil)
    open_token = {:"[", open_meta}

    {pairs_tokens, layout} = compile_no_parens_kw_pairs(pairs, layout, opts)

    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token] ++ pairs_tokens ++ [close_token], layout}
  end

  # New format: kw_data with kw_eol pairs
  defp compile_bracket_arg_stuck({:kw_data, _pairs} = kw_data, layout, opts) do
    {open_meta, layout} = TokenLayout.stick_right(layout, "[", nil)
    open_token = {:"[", open_meta}

    {kw_tokens, layout} = compile_kw_data(kw_data, layout, opts)

    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token] ++ kw_tokens ++ [close_token], layout}
  end

  defp compile_bracket_arg_stuck({:kw_data_trailing, _pairs} = kw_data, layout, opts) do
    {open_meta, layout} = TokenLayout.stick_right(layout, "[", nil)
    open_token = {:"[", open_meta}

    {kw_tokens, layout} = compile_kw_data(kw_data, layout, opts)

    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token] ++ kw_tokens ++ [close_token], layout}
  end

  defp compile_bracket_arg_stuck(arg, layout, opts) when is_list(arg) do
    {open_meta, layout} = TokenLayout.stick_right(layout, "[", nil)
    open_token = {:"[", open_meta}

    {args_tokens, layout} = compile_args_in_brackets(arg, layout, opts)

    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token] ++ args_tokens ++ [close_token], layout}
  end

  defp compile_bracket_arg_stuck({:trailing, expr}, layout, opts) do
    {open_meta, layout} = TokenLayout.stick_right(layout, "[", nil)
    open_token = {:"[", open_meta}

    {arg_tokens, layout} = compile_arg_with_adhesion(expr, layout, opts)

    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token] ++ arg_tokens ++ [comma_token, close_token], layout}
  end

  defp compile_bracket_arg_stuck(arg, layout, opts) do
    {open_meta, layout} = TokenLayout.stick_right(layout, "[", nil)
    open_token = {:"[", open_meta}

    {arg_tokens, layout} = compile_arg_with_adhesion(arg, layout, opts)

    {close_meta, layout} = TokenLayout.stick_right(layout, "]", nil)
    close_token = {:"]", close_meta}

    {[open_token] ++ arg_tokens ++ [close_token], layout}
  end

  # ===========================================================================
  # Helper: compile_no_parens_args (for no-parens call arguments)
  # ===========================================================================

  # Compile arguments for no-parens calls
  # Can be a single expression, keyword arguments, or call_args_no_parens_all variants

  # call_args_no_parens_all format (new)
  # Per grammar lines 508-510:
  # call_args_no_parens_all -> call_args_no_parens_one   : {:call_args_one, arg}
  # call_args_no_parens_all -> call_args_no_parens_ambig : {:call_args_ambig, no_parens_expr}
  # call_args_no_parens_all -> call_args_no_parens_many  : {:call_args_many, [exprs]}

  defp compile_no_parens_args({:call_args_one, arg}, layout, opts) do
    # call_args_no_parens_one: single arg or keyword args
    compile_no_parens_args(arg, layout, opts)
  end

  defp compile_no_parens_args({:call_args_ambig, no_parens_expr}, layout, opts) do
    # call_args_no_parens_ambig: nested no_parens call (the argument IS a no_parens_expr)
    do_to_tokens(no_parens_expr, layout, opts)
  end

  defp compile_no_parens_args({:call_args_many, exprs}, layout, opts) when is_list(exprs) do
    # call_args_no_parens_many: 2+ positional args, optionally with trailing kw
    compile_no_parens_many_args(exprs, layout, opts)
  end

  # Legacy format: direct expression
  defp compile_no_parens_args({:single_arg, expr}, layout, opts) do
    do_to_tokens(expr, layout, opts)
  end

  # Keyword arguments: foo a: 1, b: 2
  defp compile_no_parens_args({:kw_args, pairs}, layout, opts) do
    compile_no_parens_kw_pairs(pairs, layout, opts)
  end

  # Fallback for legacy format (direct expression without wrapper)
  defp compile_no_parens_args(expr, layout, opts) do
    do_to_tokens(expr, layout, opts)
  end

  # Compile keyword pairs for no-parens call: a: 1 or a: 1, b: 2
  defp compile_no_parens_kw_pairs([], layout, _opts), do: {[], layout}

  defp compile_no_parens_kw_pairs([{key, value}], layout, opts) do
    # Single pair: key: value
    compile_no_parens_kw_pair(key, value, layout, opts)
  end

  defp compile_no_parens_kw_pairs([{key, value} | rest], layout, opts) do
    # First pair
    {first_tokens, layout} = compile_no_parens_kw_pair(key, value, layout, opts)

    # Remaining pairs have commas
    {rest_tokens, layout} = compile_remaining_no_parens_kw_pairs(rest, layout, opts)

    {first_tokens ++ rest_tokens, layout}
  end

  # Compile a single keyword pair for no-parens call
  defp compile_no_parens_kw_pair(key, value, layout, opts) do
    # Key (identifier as keyword) with space before
    key_str = Atom.to_string(key)
    key_lexeme = key_str <> ":"
    {key_meta, layout} = TokenLayout.space_before(layout, key_lexeme, nil)
    key_token = {:kw_identifier, key_meta, key}

    # Value with space after colon
    {value_tokens, layout} = do_to_tokens(value, layout, opts)

    {[key_token] ++ value_tokens, layout}
  end

  defp compile_remaining_no_parens_kw_pairs([], layout, _opts), do: {[], layout}

  defp compile_remaining_no_parens_kw_pairs([{key, value} | rest], layout, opts) do
    # Comma stuck to previous
    {comma_meta, layout} = TokenLayout.stick_right(layout, ",", nil)
    comma_token = {:",", comma_meta}

    # Key with space after comma
    key_str = Atom.to_string(key)
    key_lexeme = key_str <> ":"
    {key_meta, layout} = TokenLayout.space_before(layout, key_lexeme, nil)
    key_token = {:kw_identifier, key_meta, key}

    # Value with space after colon
    {value_tokens, layout} = do_to_tokens(value, layout, opts)

    # Continue with remaining pairs
    {rest_tokens, layout} = compile_remaining_no_parens_kw_pairs(rest, layout, opts)

    {[comma_token, key_token] ++ value_tokens ++ rest_tokens, layout}
  end

  # ===========================================================================
  # Helper: compile_alias_segments (for dotted aliases like Foo.Bar.Baz)
  # ===========================================================================

  # Compile alias segments with dots between them
  defp compile_alias_segments([], layout, _is_first), do: {[], layout}

  defp compile_alias_segments([{:alias, atom}], layout, is_first) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)

    {meta, layout} =
      if is_first do
        TokenLayout.space_before(layout, name, chars)
      else
        TokenLayout.stick_right(layout, name, chars)
      end

    {[{:alias, meta, atom}], layout}
  end

  defp compile_alias_segments([{:alias, atom} | rest], layout, is_first) do
    name = Atom.to_string(atom)
    chars = String.to_charlist(name)

    {meta, layout} =
      if is_first do
        TokenLayout.space_before(layout, name, chars)
      else
        TokenLayout.stick_right(layout, name, chars)
      end

    alias_token = {:alias, meta, atom}

    # Add dot (stuck to alias)
    {dot_meta, layout} = TokenLayout.stick_right(layout, ".", nil)
    dot_token = {:., dot_meta}

    # Compile remaining segments (not first anymore)
    {rest_tokens, layout} = compile_alias_segments(rest, layout, false)

    {[alias_token, dot_token] ++ rest_tokens, layout}
  end

  # ---------------------------------------------------------------------------
  # Helper: compile_newlines
  # ---------------------------------------------------------------------------

  # Compile newlines after an operator (for op_eol patterns)
  defp compile_newlines(0, layout), do: {[], layout}

  defp compile_newlines(newlines, layout) when is_integer(newlines) and newlines > 0 do
    eol_meta = TokenLayout.meta(layout, "\n", newlines)
    layout = TokenLayout.newlines(layout, newlines)
    {[{:eol, eol_meta}], layout}
  end
end
