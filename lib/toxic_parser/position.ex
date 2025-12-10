defmodule ToxicParser.Position do
  @moduledoc """
  Helpers for translating Toxic positions into parser metadata.
  """

  @type location :: %{offset: non_neg_integer(), line: pos_integer(), column: non_neg_integer()}

  @doc """
  Builds a location map from 1-based line/column and a precomputed line index.
  """
  @spec to_location(pos_integer(), pos_integer(), [non_neg_integer()]) :: location()
  def to_location(line, column, line_index) do
    offset =
      case Enum.fetch(line_index, line - 1) do
        {:ok, base} -> base + max(column - 1, 0)
        :error -> 0
      end

    %{offset: offset, line: line, column: column}
  end

  @doc """
  Converts Toxic meta or error positions into a range map.
  """
  @spec range_from_meta(term(), [non_neg_integer()]) :: %{
          start: location(),
          end: location()
        }
  def range_from_meta({{sl, sc}, {el, ec}, _extra}, line_index) do
    %{
      start: to_location(sl, sc, line_index),
      end: to_location(el, ec, line_index)
    }
  end

  def range_from_meta({{sl, sc}, {el, ec}}, line_index) do
    %{
      start: to_location(sl, sc, line_index),
      end: to_location(el, ec, line_index)
    }
  end

  def range_from_meta(_meta, line_index) do
    # Fallback: zero-length at start of file
    %{
      start: to_location(1, 1, line_index),
      end: to_location(1, 1, line_index)
    }
  end
end
