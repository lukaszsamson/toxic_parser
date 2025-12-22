defmodule ToxicParser.NoParensErrors do
  @moduledoc """
  Error messages for no-parens ambiguity errors.

  These match the error messages from elixir_parser.yrl lines 1231-1261.
  """

  @doc """
  Error for space before parentheses: `foo (a, b)`

  From elixir_parser.yrl lines 1231-1234:
  error_no_parens_strict(Token) ->
    return_error(?location(Token), "unexpected parentheses. If you are making a "
      "function call, do not insert spaces between the function name and the "
      "opening parentheses. Syntax error before: ", "'('").
  """
  @spec error_no_parens_strict(keyword()) :: {keyword(), String.t(), String.t()}
  def error_no_parens_strict(meta) do
    location = Keyword.take(meta, [:line, :column])

    {location,
     "unexpected parentheses. If you are making a " <>
       "function call, do not insert spaces between the function name and the " <>
       "opening parentheses. Syntax error before: ", "'('"}
  end

  @doc """
  Error for nested no-parens call inside paren call: `foo(bar 1, 2)`

  From elixir_parser.yrl lines 1236-1248:
  error_no_parens_many_strict(Node) ->
    return_error_with_meta(?meta(Node),
      "unexpected comma. Parentheses are required to solve ambiguity in nested calls.\\n\\n"
      "This error happens when you have nested function calls without parentheses. "
      "For example:\\n\\n"
      "    parent_call a, nested_call b, c, d\\n\\n"
      "In the example above, we don't know if the parameters \\"c\\" and \\"d\\" apply "
      "to the function \\"parent_call\\" or \\"nested_call\\". You can solve this by "
      "explicitly adding parentheses:\\n\\n"
      "    parent_call a, nested_call(b, c, d)\\n\\n"
      "Or by adding commas (in case a nested call is not intended):\\n\\n"
      "    parent_call a, nested_call, b, c, d\\n\\n"
      "Elixir cannot compile otherwise. Syntax error before: ", "','").
  """
  @spec error_no_parens_many_strict(Macro.t()) :: {keyword(), String.t(), String.t()}
  def error_no_parens_many_strict({_, meta, _}) do
    location = Keyword.take(meta, [:line, :column])

    {location,
     "unexpected comma. Parentheses are required to solve ambiguity in nested calls.\n\n" <>
       "This error happens when you have nested function calls without parentheses. " <>
       "For example:\n\n" <>
       "    parent_call a, nested_call b, c, d\n\n" <>
       "In the example above, we don't know if the parameters \"c\" and \"d\" apply " <>
       "to the function \"parent_call\" or \"nested_call\". You can solve this by " <>
       "explicitly adding parentheses:\n\n" <>
       "    parent_call a, nested_call(b, c, d)\n\n" <>
       "Or by adding commas (in case a nested call is not intended):\n\n" <>
       "    parent_call a, nested_call, b, c, d\n\n" <>
       "Elixir cannot compile otherwise. Syntax error before: ", "','"}
  end

  # Handle non-call expressions (e.g., operators with no_parens operands)
  def error_no_parens_many_strict(expr) when is_tuple(expr) and tuple_size(expr) >= 2 do
    meta = elem(expr, 1)
    location = if is_list(meta), do: Keyword.take(meta, [:line, :column]), else: []

    {location,
     "unexpected comma. Parentheses are required to solve ambiguity in nested calls.\n\n" <>
       "This error happens when you have nested function calls without parentheses. " <>
       "For example:\n\n" <>
       "    parent_call a, nested_call b, c, d\n\n" <>
       "In the example above, we don't know if the parameters \"c\" and \"d\" apply " <>
       "to the function \"parent_call\" or \"nested_call\". You can solve this by " <>
       "explicitly adding parentheses:\n\n" <>
       "    parent_call a, nested_call(b, c, d)\n\n" <>
       "Or by adding commas (in case a nested call is not intended):\n\n" <>
       "    parent_call a, nested_call, b, c, d\n\n" <>
       "Elixir cannot compile otherwise. Syntax error before: ", "','"}
  end

  @doc """
  Error for no-parens call inside container: `[foo 1, 2]`

  From elixir_parser.yrl lines 1250-1261:
  error_no_parens_container_strict(Node) ->
    return_error_with_meta(?meta(Node),
      "unexpected comma. Parentheses are required to solve ambiguity inside containers.\\n\\n"
      "This error may happen when you forget a comma in a list or other container:\\n\\n"
      "    [a, b c, d]\\n\\n"
      "Or when you have ambiguous calls:\\n\\n"
      "    [function a, b, c]\\n\\n"
      "In the example above, we don't know if the values \\"b\\" and \\"c\\" "
      "belongs to the list or the function \\"function\\". You can solve this by explicitly "
      "adding parentheses:\\n\\n"
      "    [one, function(a, b, c)]\\n\\n"
      "Elixir cannot compile otherwise. Syntax error before: ", "','").
  """
  @spec error_no_parens_container_strict(Macro.t()) :: {keyword(), String.t(), String.t()}
  def error_no_parens_container_strict({_, meta, _}) do
    location = Keyword.take(meta, [:line, :column])

    {location,
     "unexpected comma. Parentheses are required to solve ambiguity inside containers.\n\n" <>
       "This error may happen when you forget a comma in a list or other container:\n\n" <>
       "    [a, b c, d]\n\n" <>
       "Or when you have ambiguous calls:\n\n" <>
       "    [function a, b, c]\n\n" <>
       "In the example above, we don't know if the values \"b\" and \"c\" " <>
       "belongs to the list or the function \"function\". You can solve this by explicitly " <>
       "adding parentheses:\n\n" <>
       "    [one, function(a, b, c)]\n\n" <>
       "Elixir cannot compile otherwise. Syntax error before: ", "','"}
  end

  # Handle non-call expressions
  def error_no_parens_container_strict(expr) when is_tuple(expr) and tuple_size(expr) >= 2 do
    meta = elem(expr, 1)
    location = if is_list(meta), do: Keyword.take(meta, [:line, :column]), else: []

    {location,
     "unexpected comma. Parentheses are required to solve ambiguity inside containers.\n\n" <>
       "This error may happen when you forget a comma in a list or other container:\n\n" <>
       "    [a, b c, d]\n\n" <>
       "Or when you have ambiguous calls:\n\n" <>
       "    [function a, b, c]\n\n" <>
       "In the example above, we don't know if the values \"b\" and \"c\" " <>
       "belongs to the list or the function \"function\". You can solve this by explicitly " <>
       "adding parentheses:\n\n" <>
       "    [one, function(a, b, c)]\n\n" <>
       "Elixir cannot compile otherwise. Syntax error before: ", "','"}
  end
end
