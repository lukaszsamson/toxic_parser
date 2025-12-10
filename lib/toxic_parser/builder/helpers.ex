defmodule ToxicParser.Builder.Helpers do
  @moduledoc """
  Lightweight helpers for building Elixir AST nodes directly from parser events.

  These helpers mirror common patterns so Pratt/grammar code can emit AST as it
  parses without a post-pass.
  """

  @doc "Returns a literal AST (unchanged)."
  @spec literal(term()) :: term()
  def literal(value), do: value

  @doc "Builds a unary op AST node."
  @spec unary(atom(), Macro.t(), keyword()) :: Macro.t()
  def unary(op, operand, meta \\ []) do
    {op, meta, [operand]}
  end

  @doc "Builds a binary op AST node."
  @spec binary(atom(), Macro.t(), Macro.t(), keyword()) :: Macro.t()
  def binary(op, left, right, meta \\ []) do
    {op, meta, [left, right]}
  end

  @doc "Builds a dotted access/call target AST (`foo.bar`)."
  @spec dot(Macro.t(), Macro.t(), keyword()) :: Macro.t()
  def dot(base, member, meta \\ []) do
    {:., meta, [base, member]}
  end

  @doc "Builds an access AST (`expr[...]`) with indices."
  @spec access(Macro.t(), [Macro.t()], keyword()) :: Macro.t()
  def access(subject, indices, meta \\ []) do
    {{:., meta, [Access, :get]}, meta, [subject | indices]}
  end

  @doc "Concatenates alias segments into an `__aliases__` node."
  @spec alias_segments([atom()], keyword()) :: Macro.t()
  def alias_segments(segments, meta \\ []) when is_list(segments) do
    {:__aliases__, meta, segments}
  end
end
