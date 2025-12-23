#!/usr/bin/env elixir
# Phase 0 benchmark: Strict mode parser throughput
#
# Measures ToxicParser performance in strict mode with token_metadata.
# Tracks: wall time, reductions, GC counts, memory.
#
# Usage:
#   mix run scripts/bench_parse_strict.exs
#   mix run scripts/bench_parse_strict.exs --project elixir
#   mix run scripts/bench_parse_strict.exs --limit 100
#   mix run scripts/bench_parse_strict.exs --runs 10

defmodule BenchParseStrict do
  @projects_dir "/Users/lukaszsamson/claude_fun/elixir_oss/projects/elixir"
  @ignored_dirs ["_build", "deps", ".git", "tmp", "priv", "rel", "cover", "doc", "logs"]
  @default_runs 5

  def run(args) do
    opts = parse_args(args)

    IO.puts("=== Bench: Parser Strict Mode ===")
    IO.puts("Project filter: #{opts.project || "all"}")
    if opts.limit, do: IO.puts("File limit: #{opts.limit}")
    IO.puts("Runs: #{opts.runs}")
    IO.puts("")

    files = collect_files(opts)
    IO.puts("Files: #{length(files)}")

    if files == [] do
      IO.puts("No files found. Exiting.")
      System.halt(1)
    end

    # Pre-load file contents to exclude I/O from measurement
    contents = Enum.map(files, &File.read!/1)
    total_bytes = Enum.reduce(contents, 0, fn c, acc -> acc + byte_size(c) end)
    IO.puts("Total bytes: #{total_bytes}")
    IO.puts("")

    # Warmup
    IO.puts("Warming up...")
    _ = parse_all(contents)

    # Run benchmarks
    IO.puts("Running #{opts.runs} measurements...\n")

    results =
      for run <- 1..opts.runs do
        # Collect GC stats before
        gc_before = :erlang.statistics(:garbage_collection)
        {gcs_before, words_before, _} = gc_before
        # Reset reductions counter by calling it before workload
        _ = :erlang.statistics(:reductions)

        # Run workload
        {time_us, {node_count, success_count, error_count}} = :timer.tc(fn -> parse_all(contents) end)

        # Collect GC stats after
        gc_after = :erlang.statistics(:garbage_collection)
        {gcs_after, words_after, _} = gc_after
        # elem(1) gives reductions since last call (i.e., during workload)
        reductions = :erlang.statistics(:reductions) |> elem(1)

        time_ms = time_us / 1000
        gc_count = gcs_after - gcs_before
        gc_words = words_after - words_before

        status = if error_count > 0, do: " (#{error_count} errors)", else: ""

        IO.puts(
          "Run #{run}: #{Float.round(time_ms, 2)} ms | " <>
            "#{node_count} nodes | " <>
            "#{div(reductions, 1000)}k reds | " <>
            "#{gc_count} GCs | " <>
            "#{div(gc_words * 8, 1024)} KB reclaimed#{status}"
        )

        %{
          time_ms: time_ms,
          nodes: node_count,
          success: success_count,
          errors: error_count,
          reductions: reductions,
          gc_count: gc_count,
          gc_words: gc_words
        }
      end

    # Summary statistics
    times = Enum.map(results, & &1.time_ms)
    avg_time = Enum.sum(times) / length(times)
    min_time = Enum.min(times)
    max_time = Enum.max(times)
    node_count = hd(results).nodes
    success_count = hd(results).success
    error_count = hd(results).errors

    avg_reds = Enum.sum(Enum.map(results, & &1.reductions)) / length(results)
    avg_gcs = Enum.sum(Enum.map(results, & &1.gc_count)) / length(results)
    avg_gc_words = Enum.sum(Enum.map(results, & &1.gc_words)) / length(results)

    IO.puts("")
    IO.puts("=== Summary ===")
    IO.puts("Time:       avg=#{Float.round(avg_time, 2)} ms, min=#{Float.round(min_time, 2)} ms, max=#{Float.round(max_time, 2)} ms")
    IO.puts("AST nodes:  #{node_count}")
    IO.puts("Files:      #{success_count} ok, #{error_count} errors")
    IO.puts("Throughput: #{Float.round(node_count / (avg_time / 1000), 0)} nodes/sec")
    IO.puts("Throughput: #{Float.round(total_bytes / (avg_time / 1000) / 1024, 2)} KB/sec")
    IO.puts("Reductions: #{Float.round(avg_reds / 1000, 1)}k avg per run")
    IO.puts("Reds/node:  #{Float.round(avg_reds / max(node_count, 1), 1)}")
    IO.puts("GC count:   #{Float.round(avg_gcs, 1)} avg per run")
    IO.puts("GC reclaim: #{Float.round(avg_gc_words * 8 / 1024, 1)} KB avg per run")
  end

  defp parse_all(contents) do
    Enum.reduce(contents, {0, 0, 0}, fn content, {nodes, success, errors} ->
      case ToxicParser.parse_string(content, mode: :strict, token_metadata: true) do
        {:ok, result} ->
          count = count_ast_nodes(result.ast)
          {nodes + count, success + 1, errors}

        {:error, _} ->
          {nodes, success, errors + 1}
      end
    end)
  end

  defp count_ast_nodes(nil), do: 0
  defp count_ast_nodes(ast) when is_list(ast), do: Enum.sum(Enum.map(ast, &count_ast_nodes/1))
  defp count_ast_nodes({_form, _meta, args}) when is_list(args), do: 1 + count_ast_nodes(args)
  defp count_ast_nodes({left, right}), do: count_ast_nodes(left) + count_ast_nodes(right)
  defp count_ast_nodes(_), do: 1

  defp parse_args(args) do
    {parsed, _rest, _invalid} =
      OptionParser.parse(args,
        strict: [
          project: :string,
          limit: :integer,
          runs: :integer
        ]
      )

    %{
      project: Keyword.get(parsed, :project),
      limit: Keyword.get(parsed, :limit),
      runs: Keyword.get(parsed, :runs, @default_runs)
    }
  end

  defp collect_files(opts) do
    base_path =
      if opts.project do
        Path.join(@projects_dir, opts.project)
      else
        @projects_dir
      end

    unless File.exists?(base_path) do
      IO.puts("Error: path '#{base_path}' not found")
      System.halt(1)
    end

    files =
      Path.wildcard(Path.join(base_path, "**/*.{ex,exs}"))
      |> Enum.reject(&should_ignore_file?/1)

    if opts.limit do
      Enum.take(files, opts.limit)
    else
      files
    end
  end

  defp should_ignore_file?(file_path) do
    path_parts = Path.split(file_path)

    phoenix_templates = String.contains?(file_path, "installer/templates")

    phoenix_templates or Enum.any?(@ignored_dirs, &(&1 in path_parts))
  end
end

BenchParseStrict.run(System.argv())
