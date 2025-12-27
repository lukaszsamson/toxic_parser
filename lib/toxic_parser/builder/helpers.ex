defmodule ToxicParser.Builder.Helpers do
  @moduledoc """
  Lightweight helpers for building Elixir AST nodes directly from parser events.

  These helpers mirror common patterns so Pratt/grammar code can emit AST as it
  parses without a post-pass.
  """

  @doc "Returns a literal AST, optionally encoded using literal_encoder."
  @spec literal(term(), keyword(), ToxicParser.State.t() | nil) :: term()
  def literal(value, meta \\ [], state \\ nil)

  def literal(value, meta, %{opts: opts} = _state) do
    case Keyword.get(opts, :literal_encoder) do
      nil ->
        value

      encoder when is_function(encoder, 2) ->
        case encoder.(value, meta) do
          {:ok, encoded} -> encoded
          {:error, _reason} -> value
        end
    end
  end

  def literal(value, _meta, nil), do: value

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

  @doc "Builds an error node payload."
  @spec error(term(), keyword()) :: Macro.t()
  def error(payload, meta \\ []) do
    {:__error__, meta, payload}
  end

  @doc """
  Builds AST from a raw token based on its kind.

  Tokens are raw tuples from the lexer:
  - {kind, meta} for simple tokens
  - {kind, meta, value} for tokens with values
  - {kind, meta, v1, v2} for special tokens like "not in"
  """
  @spec from_token(tuple()) :: Macro.t()
  def from_token({:identifier, _meta, name} = token) do
    identifier(name, token_meta(token))
  end

  def from_token({:do_identifier, _meta, name} = token) do
    identifier(name, token_meta(token))
  end

  def from_token({:bracket_identifier, _meta, name} = token) do
    identifier(name, token_meta(token))
  end

  def from_token({:paren_identifier, _meta, name} = token) do
    identifier(name, token_meta(token))
  end

  def from_token({:op_identifier, _meta, name} = token) do
    identifier(name, token_meta(token))
  end

  def from_token({:alias, _meta, name} = token) do
    # Aliases need both :last and regular location metadata
    m = token_meta(token)
    {:__aliases__, [last: m] ++ m, [name]}
  end

  def from_token({kind, _meta, _value}) when kind in [true, false, nil] do
    kind
  end

  def from_token({:atom, _meta, atom}) do
    atom
  end

  # For int/flt tokens, the parsed value is in meta.extra (3rd element of meta tuple)
  def from_token({:int, {_, _, parsed}, _repr}) do
    parsed
  end

  def from_token({:flt, {_, _, parsed}, _repr}) do
    parsed
  end

  def from_token({:char, _meta, codepoint}) do
    codepoint
  end

  # Fallback for other 3-tuple tokens - return the value
  def from_token({_kind, _meta, value}) do
    value
  end

  @doc """
  Extracts [line: L, column: C] metadata from a raw token.

  Token meta format: {{start_line, start_col}, {end_line, end_col}, extra}
  """
  @spec token_meta(tuple()) :: keyword()
  def token_meta({_kind, {{line, column}, _, _}, _value}) do
    [line: line, column: column]
  end

  def token_meta(_), do: []
end
