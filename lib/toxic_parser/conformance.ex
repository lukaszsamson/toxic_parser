defmodule ToxicParser.Conformance do
  @moduledoc """
  Utilities for comparing parser output against Elixir's reference parser.
  """

  alias ToxicParser.Result

  @doc """
  Parses `source` with ToxicParser in both strict and tolerant modes and returns
  the reference AST/comments from `Code.string_to_quoted_with_comments/2`.
  """
  @spec compare(String.t(), keyword()) ::
          {:ok,
           %{
             strict: Result.t(),
             tolerant: Result.t(),
             reference_ast: Macro.t(),
             reference_comments: list()
           }}
          | {:error, term()}
  def compare(source, opts \\ []) when is_binary(source) do
    reference_opts = to_code_opts(opts)

    with {:ok, reference_ast, reference_comments} <-
           Code.string_to_quoted_with_comments(source, reference_opts),
         {:ok, strict} <- ToxicParser.parse_string(source, Keyword.put(opts, :mode, :strict)),
         {:ok, tolerant} <- ToxicParser.parse_string(source, Keyword.put(opts, :mode, :tolerant)) do
      {:ok,
       %{
         strict: strict,
         tolerant: tolerant,
         reference_ast: reference_ast,
         reference_comments: reference_comments
       }}
    end
  end

  @doc """
  Compares two ASTs ignoring metadata by stringifying them.
  """
  @spec ast_equal?(Macro.t(), Macro.t()) :: boolean()
  def ast_equal?(left, right) do
    Macro.to_string(left) == Macro.to_string(right)
  end

  defp to_code_opts(opts) do
    opts
    |> Keyword.take([:existing_atoms_only, :token_metadata, :literal_encoder, :file])
    |> Keyword.put_new(:existing_atoms_only, false)
  end
end
