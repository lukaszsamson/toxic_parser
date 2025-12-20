defmodule ToxicParser.Property.TokenLayout do
  @moduledoc """
  Position tracking for token generation.

  Generators never see line/col directly - this module handles all position math.
  Tokens use ranged metas: `{{start_line, start_col}, {end_line, end_col}, extra}`.

  Key behaviors:
  - `advance/2` counts newlines in lexeme, updates line/col
  - `stick_right/2` emits token with no extra space (for adhesion: `%{`, `&1`, `foo.(`)
  - `space_before/2` ensures space before lexeme
  """

  @type t :: %__MODULE__{
          line: pos_integer(),
          col: pos_integer()
        }

  defstruct line: 1, col: 1

  @doc """
  Create a new layout starting at the given position.
  """
  @spec new(pos_integer(), pos_integer()) :: t()
  def new(line \\ 1, col \\ 1) do
    %__MODULE__{line: line, col: col}
  end

  @doc """
  Create a meta tuple for a token at the current position.

  Returns `{{start_line, start_col}, {end_line, end_col}, extra}` where
  end position is computed by advancing over the lexeme.
  """
  @spec meta(t(), String.t() | charlist(), term()) ::
          {{pos_integer(), pos_integer()}, {pos_integer(), pos_integer()}, term()}
  def meta(%__MODULE__{line: line, col: col} = _layout, lexeme, extra) do
    lexeme_str = lexeme_to_string(lexeme)
    {end_line, end_col} = compute_end_position(line, col, lexeme_str)
    {{line, col}, {end_line, end_col}, extra}
  end

  @doc """
  Advance the layout position over a lexeme.

  Counts newlines in the lexeme and updates line/col accordingly.
  """
  @spec advance(t(), String.t() | charlist()) :: t()
  def advance(%__MODULE__{line: line, col: col}, lexeme) do
    lexeme_str = lexeme_to_string(lexeme)
    {new_line, new_col} = compute_end_position(line, col, lexeme_str)
    %__MODULE__{line: new_line, col: new_col}
  end

  @doc """
  Emit a token stuck to the previous token (no extra space).

  Used for adhesion cases like `%{`, `&10`, `foo.(`.
  Returns `{meta, new_layout}`.
  """
  @spec stick_right(t(), String.t() | charlist(), term()) ::
          {{{pos_integer(), pos_integer()}, {pos_integer(), pos_integer()}, term()}, t()}
  def stick_right(%__MODULE__{} = layout, lexeme, extra) do
    m = meta(layout, lexeme, extra)
    new_layout = advance(layout, lexeme)
    {m, new_layout}
  end

  @doc """
  Emit a token with a space before it (if not at start of line).

  Returns `{meta, new_layout}`.
  """
  @spec space_before(t(), String.t() | charlist(), term()) ::
          {{{pos_integer(), pos_integer()}, {pos_integer(), pos_integer()}, term()}, t()}
  def space_before(%__MODULE__{line: line, col: col} = _layout, lexeme, extra) do
    # Add space if not at column 1
    new_col = if col == 1, do: 1, else: col + 1
    layout_with_space = %__MODULE__{line: line, col: new_col}

    m = meta(layout_with_space, lexeme, extra)
    new_layout = advance(layout_with_space, lexeme)
    {m, new_layout}
  end

  @doc """
  Emit a newline and move to the next line at the given column.

  Returns updated layout positioned at start of new line.
  """
  @spec newline(t(), pos_integer()) :: t()
  def newline(%__MODULE__{line: line}, col \\ 1) do
    %__MODULE__{line: line + 1, col: col}
  end

  @doc """
  Emit multiple newlines and move to the target line at the given column.
  """
  @spec newlines(t(), pos_integer(), pos_integer()) :: t()
  def newlines(%__MODULE__{line: line}, count, col \\ 1) when count > 0 do
    %__MODULE__{line: line + count, col: col}
  end

  @doc """
  Move to a specific column on the current line.
  """
  @spec to_column(t(), pos_integer()) :: t()
  def to_column(%__MODULE__{line: line}, col) do
    %__MODULE__{line: line, col: col}
  end

  @doc """
  Get the current position as {line, col}.
  """
  @spec position(t()) :: {pos_integer(), pos_integer()}
  def position(%__MODULE__{line: line, col: col}), do: {line, col}

  # Compute end position after traversing a string
  defp compute_end_position(line, col, "") do
    {line, col}
  end

  defp compute_end_position(line, col, str) do
    lines = String.split(str, "\n", parts: :infinity)

    case lines do
      [single] ->
        {line, col + String.length(single)}

      [_first | rest] ->
        new_line = line + length(rest)
        last = List.last(rest)
        new_col = 1 + String.length(last)
        {new_line, new_col}
    end
  end

  # Convert to string if charlist
  defp lexeme_to_string(s) when is_binary(s), do: s
  defp lexeme_to_string(s) when is_list(s), do: List.to_string(s)
end
