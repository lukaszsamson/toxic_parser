#!/usr/bin/env elixir
# Benchmark: Compare ToxicParser vs Code.string_to_quoted
#
# Runs both parsers with comparable options and compares performance.
# Options used:
#   - ToxicParser: mode: :strict, token_metadata: true
#   - Code.string_to_quoted: columns: true, token_metadata: true
#
# Usage:
#   mix run scripts/bench_compare.exs
#   mix run scripts/bench_compare.exs --project elixir
#   mix run scripts/bench_compare.exs --limit 100
#   mix run scripts/bench_compare.exs --runs 5

defmodule BenchCompare do
  @projects_dir "/Users/lukaszsamson/claude_fun/elixir_oss/projects/elixir"
  @ignored_dirs ["_build", "deps", ".git", "tmp", "priv", "rel", "cover", "doc", "logs"]
  @default_runs 5

  def run(args) do
    opts = parse_args(args)

    IO.puts("=== Bench: ToxicParser vs Code.string_to_quoted ===")
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

    # Warmup both parsers
    IO.puts("Warming up...")
    _ = parse_all_toxic(contents)
    _ = parse_all_elixir(contents)
    IO.puts("")

    # Benchmark ToxicParser
    IO.puts("=== ToxicParser (mode: :strict, token_metadata: true) ===")
    toxic_results = run_benchmark(opts.runs, fn -> parse_all_toxic(contents) end, "ToxicParser")

    IO.puts("")

    # Benchmark Code.string_to_quoted
    IO.puts("=== Code.string_to_quoted (columns: true, token_metadata: true) ===")
    elixir_results = run_benchmark(opts.runs, fn -> parse_all_elixir(contents) end, "Elixir")

    # Comparison summary
    IO.puts("")
    IO.puts("=== Comparison ===")

    toxic_avg = Enum.sum(Enum.map(toxic_results, & &1.time_ms)) / length(toxic_results)
    elixir_avg = Enum.sum(Enum.map(elixir_results, & &1.time_ms)) / length(elixir_results)
    toxic_min = Enum.min(Enum.map(toxic_results, & &1.time_ms))
    elixir_min = Enum.min(Enum.map(elixir_results, & &1.time_ms))

    toxic_reds = Enum.sum(Enum.map(toxic_results, & &1.reductions)) / length(toxic_results)
    elixir_reds = Enum.sum(Enum.map(elixir_results, & &1.reductions)) / length(elixir_results)

    toxic_nodes = hd(toxic_results).nodes
    elixir_nodes = hd(elixir_results).nodes

    ratio_avg = toxic_avg / elixir_avg
    ratio_min = toxic_min / elixir_min

    IO.puts("")
    IO.puts("| Metric | ToxicParser | Elixir | Ratio |")
    IO.puts("|--------|-------------|--------|-------|")
    IO.puts("| Avg time | #{Float.round(toxic_avg, 2)} ms | #{Float.round(elixir_avg, 2)} ms | #{Float.round(ratio_avg, 2)}x |")
    IO.puts("| Min time | #{Float.round(toxic_min, 2)} ms | #{Float.round(elixir_min, 2)} ms | #{Float.round(ratio_min, 2)}x |")
    IO.puts("| Throughput | #{Float.round(toxic_nodes / (toxic_avg / 1000), 0)} nodes/s | #{Float.round(elixir_nodes / (elixir_avg / 1000), 0)} nodes/s | #{Float.round(1 / ratio_avg, 2)}x |")
    IO.puts("| Throughput | #{Float.round(total_bytes / (toxic_avg / 1000) / 1024, 2)} KB/s | #{Float.round(total_bytes / (elixir_avg / 1000) / 1024, 2)} KB/s | #{Float.round(1 / ratio_avg, 2)}x |")
    IO.puts("| Reductions | #{Float.round(toxic_reds / 1_000_000, 2)}M | #{Float.round(elixir_reds / 1_000_000, 2)}M | #{Float.round(toxic_reds / elixir_reds, 2)}x |")
    IO.puts("| AST nodes | #{toxic_nodes} | #{elixir_nodes} | - |")
    IO.puts("")

    if ratio_avg < 1.0 do
      IO.puts("ToxicParser is #{Float.round((1 - ratio_avg) * 100, 1)}% FASTER than Elixir")
    else
      IO.puts("ToxicParser is #{Float.round((ratio_avg - 1) * 100, 1)}% SLOWER than Elixir")
    end
  end

  defp run_benchmark(runs, workload_fn, _label) do
    results =
      for run <- 1..runs do
        # Collect GC stats before
        gc_before = :erlang.statistics(:garbage_collection)
        {gcs_before, words_before, _} = gc_before
        # Reset reductions counter
        _ = :erlang.statistics(:reductions)

        # Run workload
        {time_us, {node_count, success_count, error_count}} = :timer.tc(workload_fn)

        # Collect stats after
        gc_after = :erlang.statistics(:garbage_collection)
        {gcs_after, words_after, _} = gc_after
        reductions = :erlang.statistics(:reductions) |> elem(1)

        time_ms = time_us / 1000
        gc_count = gcs_after - gcs_before
        gc_words = words_after - words_before

        status = if error_count > 0, do: " (#{error_count} errors)", else: ""

        IO.puts(
          "Run #{run}: #{Float.round(time_ms, 2)} ms | " <>
            "#{node_count} nodes | " <>
            "#{div(reductions, 1000)}k reds | " <>
            "#{gc_count} GCs#{status}"
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

    # Summary
    times = Enum.map(results, & &1.time_ms)
    avg_time = Enum.sum(times) / length(times)
    min_time = Enum.min(times)
    max_time = Enum.max(times)

    IO.puts("Summary: avg=#{Float.round(avg_time, 2)} ms, min=#{Float.round(min_time, 2)} ms, max=#{Float.round(max_time, 2)} ms")

    results
  end

  defp parse_all_toxic(contents) do
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

  defp parse_all_elixir(contents) do
    Enum.reduce(contents, {0, 0, 0}, fn content, {nodes, success, errors} ->
      case Code.string_to_quoted(content, columns: true, token_metadata: true) do
        {:ok, ast} ->
          count = count_ast_nodes(ast)
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

BenchCompare.run(System.argv())
