defmodule ToxicParser do
  @moduledoc """
  Entry points and contracts for the Toxic parser.

  Phase 0 implements mode handling, result struct wiring, and a conformance
  harness stubbed against `Code.string_to_quoted_with_comments/2`.
  """

  alias ToxicParser.{Error, EventLog, Result}

  @type mode :: :strict | :tolerant

  @type option ::
          {:mode, mode()}
          | {:preserve_comments, boolean()}
          | {:emit_events, boolean()}
          | {:emit_env, boolean()}
          | {:token_metadata, boolean()}
          | {:literal_encoder, (term() -> term())}
          | {:existing_atoms_only, boolean()}
          | {:fuel_limit, pos_integer() | :infinity}
          | {:terminators, [atom()]}
          | {:file, String.t()}

  @doc """
  Parses a string and returns a `ToxicParser.Result`.
  """
  @spec parse_string(String.t(), [option()]) :: {:ok, Result.t()} | {:error, Result.t()}
  def parse_string(source, opts \\ []) when is_binary(source) do
    file = Keyword.get(opts, :file)
    mode = Keyword.get(opts, :mode, :strict)
    preserve_comments? = Keyword.get(opts, :preserve_comments, false)

    code_opts = to_code_opts(opts)

    case Code.string_to_quoted_with_comments(source, code_opts) do
      {:ok, ast, comments} ->
        {:ok,
         %Result{
           ast: ast,
           comments: maybe_preserve_comments(comments, preserve_comments?),
           diagnostics: [],
           events: EventLog.new(),
           env: [],
           mode: mode,
           file: file,
           source: source,
           metadata: %{code_opts: code_opts}
         }}

      {:error, error_tuple} ->
        diagnostic = Error.from_elixir(error_tuple)
        result = error_result(mode, file, source, diagnostic)

        if mode == :strict, do: {:error, result}, else: {:ok, result}
    end
  end

  @doc """
  Parses a file by path, forwarding to `parse_string/2`.
  """
  @spec parse_file(Path.t(), [option()]) :: {:ok, Result.t()} | {:error, Result.t()}
  def parse_file(path, opts \\ []) do
    with {:ok, source} <- File.read(path) do
      parse_string(source, Keyword.put(opts, :file, path))
    end
  end

  @doc """
  Convenience entry point for already-loaded source; kept for API parity.
  """
  @spec parse(String.t(), [option()]) :: {:ok, Result.t()} | {:error, Result.t()}
  def parse(source, opts \\ []), do: parse_string(source, opts)

  defp to_code_opts(opts) do
    opts
    |> Keyword.take([:existing_atoms_only, :token_metadata, :literal_encoder, :file])
    |> Keyword.put_new(:existing_atoms_only, false)
  end

  defp maybe_preserve_comments(comments, true), do: comments
  defp maybe_preserve_comments(_comments, false), do: []

  defp error_result(mode, file, source, diagnostic) do
    %Result{
      ast: nil,
      comments: [],
      diagnostics: [diagnostic],
      events: EventLog.new(),
      env: [],
      mode: mode,
      file: file,
      source: source,
      metadata: %{source_status: :failed}
    }
  end
end
