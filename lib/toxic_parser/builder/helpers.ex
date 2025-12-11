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

  @doc "Builds a dot call AST (`expr.(...)`)."
  @spec dot_call(Macro.t(), [Macro.t()], keyword()) :: Macro.t()
  def dot_call(base, args, meta \\ []) do
    {{:., meta, [base, :call]}, meta, args}
  end

  @doc "Builds an access AST (`expr[...]`) with indices."
  @spec access(Macro.t(), [Macro.t()], keyword()) :: Macro.t()
  def access(subject, indices, meta \\ []) do
    {{:., meta, [Access, :get]}, meta, [subject | indices]}
  end

  @doc "Builds a function call AST node."
  @spec call(Macro.t(), [Macro.t()], keyword()) :: Macro.t()
  def call(callee, args, meta \\ []) do
    {callee, meta, args}
  end

  @doc "Concatenates alias segments into an `__aliases__` node."
  @spec alias_segments([atom()], keyword()) :: Macro.t()
  def alias_segments(segments, meta \\ []) when is_list(segments) do
    {:__aliases__, meta, segments}
  end

  @doc "Builds an identifier/variable reference AST."
  @spec identifier(atom(), keyword()) :: Macro.t()
  def identifier(name, meta \\ []) when is_atom(name) do
    {name, meta, nil}
  end

  @doc "Builds an alias AST from a single atom."
  @spec alias_single(atom(), keyword()) :: Macro.t()
  def alias_single(name, meta \\ []) when is_atom(name) do
    {:__aliases__, meta, [name]}
  end

  @doc "Builds AST from a token based on its kind."
  @spec from_token(map()) :: Macro.t()
  def from_token(%{kind: :identifier, value: name, metadata: meta}) do
    identifier(name, token_meta(meta))
  end

  def from_token(%{kind: :alias, value: name, metadata: meta}) do
    # Aliases need both :last and regular location metadata
    m = token_meta(meta)
    {:__aliases__, [last: m] ++ m, [name]}
  end

  def from_token(%{kind: kind, value: _}) when kind in [true, false, nil] do
    kind
  end

  def from_token(%{kind: :atom, value: atom}) do
    atom
  end

  def from_token(%{kind: :int, raw: {:int, {_, _, parsed}, _}}) do
    parsed
  end

  def from_token(%{kind: :flt, raw: {:flt, {_, _, parsed}, _}}) do
    parsed
  end

  def from_token(%{kind: :char, value: codepoint}) do
    codepoint
  end

  def from_token(%{value: value}) do
    value
  end

  @doc "Extracts line/column metadata from a token's metadata."
  @spec token_meta(map()) :: keyword()
  def token_meta(%{range: %{start: %{line: line, column: column}}}) do
    [line: line, column: column]
  end

  def token_meta(_), do: []
end
