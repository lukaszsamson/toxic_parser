defmodule ToxicParser.State do
  @moduledoc """
  Parser state carrier for streaming tokens and parser options.
  """

  alias ToxicParser.{Context, Error}

  @type mode :: :strict | :tolerant
  @type expression_context :: Context.t()

  @type checkpoint :: %{
          ref: reference(),
          lookahead: [map()],
          diagnostics: [Error.t()],
          terminators: [term()]
        }

  @type t :: %__MODULE__{
          stream: Toxic.t(),
          lookahead: [map()],
          diagnostics: [Error.t()],
          warnings: [ToxicParser.Warning.t()],
          mode: mode(),
          opts: keyword(),
          fuel: pos_integer() | :infinity,
          expression_context: expression_context(),
          checkpoints: %{reference() => checkpoint()},
          line_index: tuple(),
          terminators: [term()],
          max_peek: pos_integer(),
          source: binary(),
          event_log: ToxicParser.EventLog.t()
        }

  defstruct stream: nil,
            lookahead: [],
            diagnostics: [],
            warnings: [],
            mode: :strict,
            opts: [],
            fuel: :infinity,
             expression_context: Context.expr(),
            checkpoints: %{},
            line_index: {},
            terminators: [],
            max_peek: 4,
            source: "",
            event_log: ToxicParser.EventLog.new()

  @doc """
  Builds an initial state from source with parser options.
  """
  @spec new(binary() | charlist(), keyword()) :: t()
  def new(source, opts \\ []) when is_binary(source) or is_list(source) do
    mode = Keyword.get(opts, :mode, :strict)
    source_bin = if is_binary(source), do: source, else: List.to_string(source)

    stream =
      Toxic.new(source, 1, 1,
        error_mode: if(mode == :tolerant, do: :tolerant, else: :strict),
        preserve_comments: Keyword.get(opts, :preserve_comments, false),
        existing_atoms_only: Keyword.get(opts, :existing_atoms_only, false),
        max_batch: 8
      )

    {terminators, stream} = Toxic.current_terminators(stream)

    ctx = Keyword.get(opts, :expression_context, Context.expr())

    %__MODULE__{
      stream: stream,
      mode: mode,
      opts: opts,
      fuel: Keyword.get(opts, :fuel_limit, :infinity),
      expression_context: ctx,
      line_index: line_index(source_bin),
      terminators: terminators,
      max_peek: Keyword.get(opts, :max_peek, 4),
      source: source_bin,
      event_log: ToxicParser.EventLog.new()
    }
  end

  @doc """
  Returns the precomputed line offset table for the given source.
  Returns a tuple for O(1) lookup by line number.
  """
  @spec line_index(binary()) :: tuple()
  def line_index(source) when is_binary(source) do
    starts =
      for {pos, 1} <- :binary.matches(source, "\n"), do: pos + 1

    [0 | starts] |> List.to_tuple()
  end
end
