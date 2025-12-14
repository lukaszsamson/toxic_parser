defmodule ToxicParser.Precedence do
  @moduledoc """
  Binding power table derived from `elixir_parser.yrl` for Pratt parsing.

  Binding powers use larger numbers for tighter binding; associativity is
  included to drive the Pratt loop.
  """

  @type bp :: pos_integer()
  @type assoc :: :left | :right | :nonassoc

  @binary_bp [
    {:stab_op, 10, :right},
    {:in_match_op, 40, :left},
    {:when_op, 50, :right},
    {:type_op, 60, :right},
    {:pipe_op, 70, :right},
    {:assoc_op, 80, :right},
    {:match_op, 100, :right},
    {:or_op, 120, :left},
    {:and_op, 130, :left},
    {:comp_op, 140, :left},
    {:rel_op, 150, :left},
    {:arrow_op, 160, :left},
    {:in_op, 170, :left},
    {:xor_op, 180, :left},
    {:ternary_op, 190, :right},
    {:concat_op, 200, :right},
    {:range_op, 200, :right},
    {:dual_op, 210, :left},
    {:mult_op, 220, :left},
    {:power_op, 230, :left},
    {:dot_op, 310, :left}
  ]

  @unary_bp [
    {:capture_op, 90, :nonassoc},
    {:unary_not_op, 135, :nonassoc},
    {:ellipsis_op, 300, :nonassoc},
    {:unary_op, 300, :nonassoc},
    {:at_op, 320, :nonassoc}
  ]

  @doc "Returns {binding power, associativity} for a binary operator class."
  @spec binary(atom()) :: {bp(), assoc()} | nil
  def binary(kind) do
    case List.keyfind(@binary_bp, kind, 0) do
      {_, bp, assoc} -> {bp, assoc}
      _ -> nil
    end
  end

  @doc "Returns {binding power, associativity} for a unary operator class."
  @spec unary(atom()) :: {bp(), assoc()} | nil
  def unary(kind) do
    case List.keyfind(@unary_bp, kind, 0) do
      {_, bp, assoc} -> {bp, assoc}
      _ -> nil
    end
  end

  @doc "Returns binding power for `not in` combined operator."
  @spec not_in() :: bp() | nil
  def not_in do
    case binary(:in_op) do
      {bp, _} -> bp
      _ -> nil
    end
  end

  @doc "Returns the binding power for dot vs dot-call handling."
  @spec dot() :: {bp(), assoc()} | nil
  def dot, do: binary(:dot_op)

  @doc "Returns the full binary precedence list."
  @spec binary_table() :: [{atom(), bp(), assoc()}]
  def binary_table, do: @binary_bp

  @doc "Returns the full unary precedence list."
  @spec unary_table() :: [{atom(), bp(), assoc()}]
  def unary_table, do: @unary_bp
end
