#!/usr/bin/env elixir

# Profiling script for ToxicParser on real-world Elixir code
# Usage examples:
#   elixir profile_toxic.exs --type time
#   elixir profile_toxic.exs --type memory
#   elixir profile_toxic.exs --type calls
#   elixir profile_toxic.exs --type time --matching ToxicParser
#   elixir profile_toxic.exs --type time --project phoenix
#   elixir profile_toxic.exs --type time --limit 100
#   elixir profile_toxic.exs --type time --target tokenizer  # profile only tokenizer
#   elixir profile_toxic.exs --type time --target parser     # profile full parser (default)
#
# Profiler selection:
#   elixir profile_toxic.exs --profiler fprof              # use fprof (detailed caller/callee)
#   elixir profile_toxic.exs --profiler fprof --callers    # show caller/callee relationships
#   elixir profile_toxic.exs --profiler fprof --details    # per-process breakdown
#   elixir profile_toxic.exs --profiler fprof --sort own   # sort by own time (not accumulated)
#   elixir profile_toxic.exs --profiler eprof              # force eprof
#   elixir profile_toxic.exs --profiler cprof              # force cprof (calls only)

Mix.install([
  {:toxic_parser, path: Path.expand(".", __DIR__)}
])

defmodule ToxicProfiler do
  @moduledoc """
  Profiles ToxicParser on real-world Elixir code corpus.
  Supports profiling just the tokenizer or the full parser.
  """

  @projects_dir "/Users/lukaszsamson/claude_fun/elixir_oss/projects/elixir"
  @ignored_dirs ["_build", "deps", ".git", "tmp", "priv", "rel", "cover", "doc", "logs"]
  @ignored_files ["mix.lock"]

  def run(args \\ []) do
    opts = parse_args(args)

    IO.puts("ToxicParser Profiler")
    IO.puts("====================")
    IO.puts("Target: #{opts.target}")
    IO.puts("Profiler: #{opts.profiler}")
    IO.puts("Profile type: #{opts.type}")
    IO.puts("Matching: #{format_matching(opts.matching)}")
    if opts.project, do: IO.puts("Project filter: #{opts.project}")
    if opts.limit, do: IO.puts("File limit: #{opts.limit}")
    if opts.profiler == :fprof do
      if opts.callers, do: IO.puts("fprof: --callers enabled")
      if opts.details, do: IO.puts("fprof: --details enabled")
      if opts.trace_to_file, do: IO.puts("fprof: --trace-to-file enabled")
    end
    IO.puts("")

    files = collect_files(opts)
    IO.puts("Collected #{length(files)} files to profile")
    IO.puts("")

    if length(files) == 0 do
      IO.puts("No files found. Exiting.")
      System.halt(1)
    end

    # Pre-warmup: ensure all code is loaded
    IO.puts("Loading code...")
    _ = process_file(hd(files), opts.target)

    # Run the profiler
    run_profile(files, opts)
  end

  defp parse_args(args) do
    {parsed, _rest, _invalid} =
      OptionParser.parse(args,
        strict: [
          type: :string,
          profiler: :string,
          matching: :string,
          project: :string,
          limit: :integer,
          no_warmup: :boolean,
          sort: :string,
          target: :string,
          # fprof-specific options
          callers: :boolean,
          details: :boolean,
          trace_to_file: :boolean
        ]
      )

    %{
      type: parse_type(Keyword.get(parsed, :type, "time")),
      profiler: parse_profiler(Keyword.get(parsed, :profiler)),
      matching: parse_matching(Keyword.get(parsed, :matching)),
      project: Keyword.get(parsed, :project),
      limit: Keyword.get(parsed, :limit),
      warmup: not Keyword.get(parsed, :no_warmup, false),
      sort: parse_sort(Keyword.get(parsed, :sort)),
      target: parse_target(Keyword.get(parsed, :target, "parser")),
      # fprof options
      callers: Keyword.get(parsed, :callers, false),
      details: Keyword.get(parsed, :details, false),
      trace_to_file: Keyword.get(parsed, :trace_to_file, false)
    }
  end

  defp parse_target("parser"), do: :parser
  defp parse_target("tokenizer"), do: :tokenizer
  defp parse_target(other), do: raise("Invalid target: #{other}. Use: parser, tokenizer")

  defp parse_type("time"), do: :time
  defp parse_type("memory"), do: :memory
  defp parse_type("calls"), do: :calls
  defp parse_type(other), do: raise("Invalid type: #{other}. Use: time, memory, calls")

  defp parse_profiler(nil), do: :auto
  defp parse_profiler("auto"), do: :auto
  defp parse_profiler("tprof"), do: :tprof
  defp parse_profiler("eprof"), do: :eprof
  defp parse_profiler("cprof"), do: :cprof
  defp parse_profiler("fprof"), do: :fprof
  defp parse_profiler(other), do: raise("Invalid profiler: #{other}. Use: auto, tprof, eprof, cprof, fprof")

  defp parse_sort(nil), do: nil
  defp parse_sort("time"), do: :time
  defp parse_sort("calls"), do: :calls
  defp parse_sort("memory"), do: :memory
  defp parse_sort("per_call"), do: :per_call
  # fprof-specific sort options
  defp parse_sort("acc"), do: :acc
  defp parse_sort("own"), do: :own
  defp parse_sort(other), do: raise("Invalid sort: #{other}")

  defp parse_matching(nil), do: {:_, :_, :_}

  defp parse_matching(str) do
    # Handle arity suffix like "Module.function/2"
    {str, arity} =
      case String.split(str, "/") do
        [s, a] -> {s, String.to_integer(a)}
        [s] -> {s, :_}
      end

    parts = String.split(str, ".")

    # Find where module ends and function begins
    # Module parts start with uppercase, function names start with lowercase
    {mod_parts, func_parts} =
      Enum.split_while(parts, fn part ->
        first_char = String.first(part)
        first_char == String.upcase(first_char)
      end)

    cond do
      mod_parts == [] ->
        raise "Invalid matching pattern: #{str}. Must start with a module name."

      func_parts == [] ->
        # All uppercase = just module name
        mod = String.to_atom("Elixir." <> Enum.join(mod_parts, "."))
        {mod, :_, :_}

      length(func_parts) == 1 ->
        # Module.function
        mod = String.to_atom("Elixir." <> Enum.join(mod_parts, "."))
        func = String.to_atom(hd(func_parts))
        {mod, func, arity}

      true ->
        raise "Invalid matching pattern: #{str}. Use: Module, Module.Submodule, or Module.function"
    end
  end

  defp format_matching({:_, :_, :_}), do: "all functions"
  defp format_matching({mod, :_, :_}), do: "#{inspect(mod)}.*"
  defp format_matching({mod, func, :_}), do: "#{inspect(mod)}.#{func}/*"
  defp format_matching({mod, func, arity}), do: "#{inspect(mod)}.#{func}/#{arity}"

  defp collect_files(opts) do
    projects =
      case File.ls(@projects_dir) do
        {:ok, dirs} ->
          dirs
          |> Enum.filter(&File.dir?(Path.join(@projects_dir, &1)))
          |> Enum.map(&Path.join(@projects_dir, &1))

        {:error, _} ->
          IO.puts("Error: projects directory '#{@projects_dir}' not found")
          System.halt(1)
      end

    projects =
      if opts.project do
        Enum.filter(projects, &(Path.basename(&1) == opts.project))
      else
        projects
      end

    IO.puts("Projects: #{Enum.map(projects, &Path.basename/1) |> Enum.join(", ")}")

    files =
      projects
      |> Enum.flat_map(&collect_elixir_files/1)

    files =
      if opts.limit do
        Enum.take(files, opts.limit)
      else
        files
      end

    files
  end

  defp collect_elixir_files(root_path) do
    Path.wildcard(Path.join(root_path, "**/*.{ex,exs}"))
    |> Enum.reject(&should_ignore_file?/1)
  end

  defp should_ignore_file?(file_path) do
    path_parts = Path.split(file_path)
    filename = Path.basename(file_path)

    # Skip Phoenix installer templates
    phoenix_installer_templates =
      String.contains?(file_path, "projects/phoenix/installer/templates")

    phoenix_installer_templates or
      Enum.any?(@ignored_dirs, &(&1 in path_parts)) or
      filename in @ignored_files
  end

  def process_file(file_path, :parser) do
    content = File.read!(file_path)
    byte_size = byte_size(content)

    case ToxicParser.parse_string(content, mode: :strict, token_metadata: true) do
      {:ok, result} ->
        # Count AST nodes as a proxy for complexity
        node_count = count_ast_nodes(result.ast)
        {node_count, byte_size}

      {:error, _result} ->
        # Still count bytes even on error
        {0, byte_size}
    end
  end

  def process_file(file_path, :tokenizer) do
    content = File.read!(file_path)
    byte_size = byte_size(content)
    stream = Toxic.new(content, 1, 1, [])
    {token_count, _stream} = count_tokens(stream, 0)
    {token_count, byte_size}
  end

  defp count_tokens(stream, count) do
    case Toxic.next(stream) do
      {:ok, _token, new_stream} ->
        count_tokens(new_stream, count + 1)

      {:eof, _final_stream} ->
        {count, nil}

      {:error, _reason, new_stream} ->
        # Continue on errors in tolerant mode
        count_tokens(new_stream, count + 1)
    end
  end

  defp count_ast_nodes(nil), do: 0
  defp count_ast_nodes(ast) when is_list(ast), do: Enum.sum(Enum.map(ast, &count_ast_nodes/1))
  defp count_ast_nodes({_form, _meta, args}) when is_list(args), do: 1 + count_ast_nodes(args)
  defp count_ast_nodes({left, right}), do: count_ast_nodes(left) + count_ast_nodes(right)
  defp count_ast_nodes(_), do: 1

  defp run_profile(files, opts) do
    target = opts.target

    # Prepare the workload function
    workload = fn ->
      Enum.reduce(files, {0, 0}, fn file, {total_units, total_bytes} ->
        {units, bytes} = process_file(file, target)
        {total_units + units, total_bytes + bytes}
      end)
    end

    # First, run without profiling to get baseline timing and stats
    IO.puts("Running baseline measurement...")
    {baseline_time, {total_units, total_bytes}} = :timer.tc(workload)
    baseline_ms = baseline_time / 1000

    unit_label = if target == :parser, do: "AST nodes", else: "tokens"

    IO.puts("")
    IO.puts("Baseline Results:")
    IO.puts("-----------------")
    IO.puts("Files processed: #{length(files)}")
    IO.puts("Total #{unit_label}: #{total_units}")
    IO.puts("Total bytes: #{total_bytes}")
    IO.puts("Baseline time: #{Float.round(baseline_ms, 2)} ms")
    IO.puts("Throughput: #{Float.round(total_units / (baseline_ms / 1000), 0)} #{unit_label}/sec")
    IO.puts("Throughput: #{Float.round(total_bytes / (baseline_ms / 1000) / 1024, 2)} KB/sec")
    IO.puts("")

    # Now run with profiling
    IO.puts("Running profiler (#{opts.profiler}, #{opts.type})...")
    IO.puts("")

    run_selected_profiler(workload, opts)
  end

  defp run_selected_profiler(workload, %{profiler: :fprof} = opts) do
    IO.puts("Using fprof")
    run_fprof(workload, opts)
  end

  defp run_selected_profiler(workload, %{profiler: :eprof} = opts) do
    IO.puts("Using eprof")
    profile_opts = build_eprof_opts(opts)
    run_eprof(workload, profile_opts)
  end

  defp run_selected_profiler(workload, %{profiler: :cprof} = opts) do
    IO.puts("Using cprof")
    profile_opts = [matching: opts.matching, warmup: opts.warmup]
    run_cprof(workload, profile_opts)
  end

  defp run_selected_profiler(workload, %{profiler: :tprof} = opts) do
    if Code.ensure_loaded?(:tprof) do
      IO.puts("Using tprof (OTP 27+)")
      profile_opts = build_tprof_opts(opts)
      run_tprof(workload, profile_opts)
    else
      IO.puts("tprof not available (requires OTP 27+), falling back to eprof")
      profile_opts = build_eprof_opts(opts)
      run_eprof(workload, profile_opts)
    end
  end

  defp run_selected_profiler(workload, %{profiler: :auto} = opts) do
    # Auto-select based on type and availability
    case opts.type do
      :time ->
        run_tprof_or_eprof(workload, opts)

      :memory ->
        if Code.ensure_loaded?(:tprof) do
          IO.puts("Using tprof for memory profiling (OTP 27+)")
          profile_opts = build_tprof_opts(opts)
          run_tprof(workload, profile_opts)
        else
          IO.puts("Memory profiling requires tprof (OTP 27+)")
          System.halt(1)
        end

      :calls ->
        run_cprof_or_tprof(workload, opts)
    end
  end

  defp build_tprof_opts(opts) do
    [type: opts.type, matching: opts.matching, warmup: opts.warmup]
    |> maybe_add_sort(opts)
  end

  defp build_eprof_opts(opts) do
    [matching: opts.matching, warmup: opts.warmup]
    |> maybe_add_sort(opts)
  end

  defp maybe_add_sort(opts_list, %{sort: nil}), do: opts_list
  defp maybe_add_sort(opts_list, %{sort: sort}), do: [{:sort, sort} | opts_list]

  defp run_tprof_or_eprof(workload, opts) do
    if Code.ensure_loaded?(:tprof) do
      IO.puts("Using tprof (OTP 27+)")
      profile_opts = build_tprof_opts(opts)
      run_tprof(workload, profile_opts)
    else
      IO.puts("Using eprof (tprof not available)")
      profile_opts = build_eprof_opts(opts)
      run_eprof(workload, profile_opts)
    end
  end

  defp run_cprof_or_tprof(workload, opts) do
    if Code.ensure_loaded?(:tprof) do
      IO.puts("Using tprof for call counting (OTP 27+)")
      profile_opts = build_tprof_opts(%{opts | type: :calls})
      run_tprof(workload, profile_opts)
    else
      IO.puts("Using cprof for call counting")
      run_cprof(workload, [matching: opts.matching, warmup: opts.warmup])
    end
  end

  defp run_tprof(workload, opts) do
    Mix.ensure_application!(:tools)
    Mix.Tasks.Profile.Tprof.profile(workload, opts)
  end

  defp run_eprof(workload, opts) do
    Mix.ensure_application!(:tools)
    Mix.Tasks.Profile.Eprof.profile(workload, opts)
  end

  defp run_cprof(workload, opts) do
    Mix.ensure_application!(:tools)
    Mix.Tasks.Profile.Cprof.profile(workload, opts)
  end

  defp run_fprof(workload, opts) do
    Mix.ensure_application!(:tools)
    Mix.ensure_application!(:runtime_tools)

    fprof_opts =
      [warmup: opts.warmup]
      |> maybe_add_fprof_callers(opts)
      |> maybe_add_fprof_details(opts)
      |> maybe_add_fprof_sort(opts)
      |> maybe_add_fprof_trace_to_file(opts)

    Mix.Tasks.Profile.Fprof.profile(workload, fprof_opts)
  end

  defp maybe_add_fprof_callers(opts_list, %{callers: true}), do: [{:callers, true} | opts_list]
  defp maybe_add_fprof_callers(opts_list, _), do: opts_list

  defp maybe_add_fprof_details(opts_list, %{details: true}), do: [{:details, true} | opts_list]
  defp maybe_add_fprof_details(opts_list, _), do: opts_list

  defp maybe_add_fprof_sort(opts_list, %{sort: sort}) when sort in [:acc, :own],
    do: [{:sort, sort} | opts_list]
  defp maybe_add_fprof_sort(opts_list, _), do: opts_list

  defp maybe_add_fprof_trace_to_file(opts_list, %{trace_to_file: true}),
    do: [{:trace_to_file, true} | opts_list]
  defp maybe_add_fprof_trace_to_file(opts_list, _), do: opts_list
end

# Run the profiler
ToxicProfiler.run(System.argv())
