defmodule ToxicParser.ExprClass do
  @moduledoc """
  Recursive AST classifier for no-parens ambiguity detection.

  Classifies expressions into three kinds matching elixir_parser.yrl's grammar unions:
  - `:matched` - Literals, paren calls, fn-end, operators with matched operands
  - `:unmatched` - Has attached do...end block
  - `:no_parens` - Multi-arg no-parens calls, ambiguous nested calls, operator extensions

  Used to detect and error on ambiguous no-parens expressions in containers and paren calls.
  """

  @type kind :: :matched | :unmatched | :no_parens

  @doc """
  Recursively classifies an AST node.

  ## Examples

      iex> classify(1)
      :matched

      iex> classify({:foo, [closing: [...]], [1, 2]})  # foo(1, 2)
      :matched

      iex> classify({:foo, [], [1, 2]})  # foo 1, 2
      :no_parens

      iex> classify({:if, [do: [...]], [true]})  # if true do ... end
      :unmatched
  """
  @spec classify(Macro.t()) :: kind()

  # Literals are always matched
  def classify(literal) when is_number(literal), do: :matched
  def classify(literal) when is_binary(literal), do: :matched
  def classify(literal) when is_atom(literal), do: :matched
  def classify(literal) when is_list(literal), do: :matched

  # Tuples that are literals (2-tuples without metadata)
  def classify({_left, _right}) do
    # A plain 2-tuple literal is matched
    # Both elements could be any expression, but the tuple itself is a container
    :matched
  end

  # Maps/structs are matched (they're containers)
  def classify(%{} = _map), do: :matched

  # AST nodes: {name, meta, args}
  def classify({name, meta, args}) when is_atom(name) and is_list(meta) do
    classify_call(name, meta, args)
  end

  # AST nodes with non-atom name (e.g., {{:., _, [...]}, _, args} for remote calls)
  def classify({_callee, meta, args}) when is_list(meta) and is_list(args) do
    # Non-atom callees (remote/dot calls, anonymous function calls, etc.)
    # can still have do-blocks attached and can still be ambiguous if they have
    # multiple args without parentheses.
    #
    # Interpolation helpers (e.g. :erlang.binary_to_atom/2 in interpolated atoms)
    # carry `:delimiter` metadata and must be treated as matched.
    cond do
      Keyword.has_key?(meta, :do) -> :unmatched
      Keyword.has_key?(meta, :parens) -> :matched
      Keyword.has_key?(meta, :delimiter) -> :matched
      Keyword.has_key?(meta, :closing) -> :matched
      length(args) > 1 -> :no_parens
      length(args) == 1 -> classify(hd(args))
      true -> :matched
    end
  end

  # Catch-all for other expressions
  def classify(_), do: :matched

  # Operators - these are not function calls, they're syntax
  # Binary operators have 2 args, unary have 1 - they don't make things no_parens by themselves
  @binary_operators [
    # Match operators
    :=,
    # Type annotation
    :"::",
    # Pipe and stab
    :|,
    :->,
    :<-,
    # Boolean operators
    :and,
    :or,
    :not,
    :in,
    # Comparison
    :==,
    :!=,
    :===,
    :!==,
    :<,
    :>,
    :<=,
    :>=,
    # Arithmetic
    :+,
    :-,
    :*,
    :/,
    :div,
    :rem,
    # Binary
    :band,
    :bor,
    :bxor,
    :bnot,
    :bsl,
    :bsr,
    # List/binary
    :++,
    :--,
    :<>,
    # Range
    :..,
    :..//,
    # Pipe operator
    :|>,
    # Boolean
    :&&,
    :||,
    :!,
    # Access
    :.,
    # When
    :when,
    # Capture
    :&,
    # Match
    :^,
    # String concat at compile time
    :<>,
    # Ternary
    :"//",
    # Module attribute, @ is unary but can look like binary with args
    :@,
    # Arrow operators
    :<~,
    :~>,
    :<~>,
    :<<~,
    :~>>,
    # Regex match
    :=~,
    # Triple boolean operators
    :&&&,
    :|||,
    # Default argument operator
    :\\,
    # Shift operators
    :>>>,
    :<<<
  ]

  @unary_operators [:+, :-, :!, :^, :not, :bnot, :&, :@]

  # Classify a call node {name, meta, args}
  defp classify_call(name, meta, args) do
    cond do
      # Do-block attached -> unmatched
      # Check for :do in meta (indicates do...end block)
      Keyword.has_key?(meta, :do) ->
        :unmatched

      # Has closing paren metadata -> paren call -> matched
      Keyword.has_key?(meta, :closing) ->
        :matched

      # Has parens metadata -> parenthesized expression like (foo 1, 2) -> matched
      Keyword.has_key?(meta, :parens) ->
        :matched

      # fn ... end has :end but not :do in standard calls
      # fn expressions are matched (they're self-contained)
      name == :fn ->
        :matched

      # __block__ is matched (it's a container for multiple expressions)
      name == :__block__ ->
        :matched

      # __aliases__ is matched (module name)
      name == :__aliases__ ->
        :matched

      # Containers (list, tuple, map, bitstring) are matched
      name in [:{}, :%{}, :<<>>] ->
        :matched

      # Special forms that are always matched
      name in [:%, :__cursor__, :__error__] ->
        :matched

      # Sigils are matched (they have explicit delimiters)
      is_sigil_name?(name) ->
        :matched

      # args is nil -> bare identifier, matched
      is_nil(args) ->
        :matched

      # Empty args list -> zero-arg call, matched
      args == [] ->
        :matched

      # Operators with 1 arg - check if operand propagates no_parens
      name in @unary_operators and length(args) == 1 ->
        classify_operands(args)

      # Operators with 2 args - check if operands propagate no_parens
      # Includes both known operators and custom operators (symbol-based names)
      length(args) == 2 and (name in @binary_operators or is_operator_name?(name)) ->
        classify_operands(args)

      # Ternary operator :..// (range with step) has 3 args
      name == :..// and length(args) == 3 ->
        classify_operands(args)

      # Multi-arg call without closing -> no_parens_many
      length(args) > 1 ->
        :no_parens

      # Single-arg call without closing -> check if arg is no_parens
      # This handles no_parens_one_ambig_expr: f g a, b
      length(args) == 1 ->
        case classify(hd(args)) do
          # no_parens_one (safe)
          :matched -> :matched
          # propagate unmatched
          :unmatched -> :unmatched
          # no_parens_one_ambig (ambiguous)
          :no_parens -> :no_parens
        end

      true ->
        :matched
    end
  end

  # Classify operator based on its operands
  # If any operand is :no_parens, the operator expression is :no_parens
  # If any operand is :unmatched, the result is :unmatched
  # Otherwise :matched
  defp classify_operands(args) do
    results = Enum.map(args, &classify/1)

    cond do
      :unmatched in results -> :unmatched
      :no_parens in results -> :no_parens
      true -> :matched
    end
  end

  # Check if a name is a sigil (like :sigil_S, :sigil_r, etc.)
  defp is_sigil_name?(name) when is_atom(name) do
    name_str = Atom.to_string(name)
    String.starts_with?(name_str, "sigil_")
  end

  # Check if a name looks like an operator (symbols made of special characters)
  # This handles custom operators like :---, :^^^, etc.
  @operator_chars ~c"+-*/<>=|&!^~@%"

  defp is_operator_name?(name) when is_atom(name) do
    name_str = Atom.to_string(name)
    # Operators are made entirely of special characters
    name_str != "" and String.to_charlist(name_str) |> Enum.all?(&(&1 in @operator_chars))
  end
end
