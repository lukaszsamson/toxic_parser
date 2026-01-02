#!/usr/bin/env elixir

# Benchmark script to find optimal scan limit for stab classification
# Tests different limit values against the Elixir OSS corpus
# Run: elixir benchmark_scan_limit.exs [--limits "50,100,200,300"] [--runs 3] [--file-limit 100]

defmodule ScanLimitBenchmark do
  @stabs_file "lib/toxic_parser/grammar/stabs.ex"
  @projects_dir "/Users/lukaszsamson/claude_fun/elixir_oss/projects/elixir"
  @ignored_dirs ["_build", "deps", ".git", "tmp", "priv", "rel", "cover", "doc", "logs"]
  @default_limits [50, 100, 150, 200, 250, 300]

  def run(args \\ []) do
    {opts, _, _} = OptionParser.parse(args,
      strict: [limits: :string, runs: :integer, file_limit: :integer]
    )

    limits = parse_limits(Keyword.get(opts, :limits))
    runs = Keyword.get(opts, :runs, 3)
    file_limit = Keyword.get(opts, :file_limit)

    IO.puts("Scan Limit Benchmark")
    IO.puts("====================")
    IO.puts("Limits to test: #{inspect(limits)}")
    IO.puts("Runs per limit: #{runs}")
    if file_limit, do: IO.puts("File limit: #{file_limit}")
    IO.puts("")

    files = collect_files(file_limit)
    IO.puts("Files: #{length(files)}")
    total_bytes = Enum.reduce(files, 0, fn f, acc -> acc + byte_size(File.read!(f)) end)
    IO.puts("Total bytes: #{total_bytes}")
    IO.puts("")

    original_content = File.read!(@stabs_file)
    file_list_path = "/tmp/benchmark_files.txt"
    File.write!(file_list_path, Enum.join(files, "\n"))

    # Create benchmark runner script
    runner_script = "/tmp/benchmark_runner.exs"
    File.write!(runner_script, """
    files = File.read!("#{file_list_path}") |> String.split("\\n", trim: true)
    # Warmup
    hd(files) |> File.read!() |> ToxicParser.parse_string(mode: :strict, token_metadata: true)
    :erlang.garbage_collect()
    # Timed run
    {time_us, _} = :timer.tc(fn ->
      Enum.each(files, fn file ->
        content = File.read!(file)
        ToxicParser.parse_string(content, mode: :strict, token_metadata: true)
      end)
    end)
    IO.puts(time_us)
    """)

    results =
      try do
        Enum.map(limits, fn limit ->
          IO.puts("Testing limit = #{limit}...")

          modify_scan_limit(original_content, limit)

          IO.puts("  Compiling...")
          {_, 0} = System.cmd("mix", ["compile", "--force"],
            cd: Path.expand(".", __DIR__), stderr_to_stdout: true)

          timings = for run <- 1..runs do
            IO.puts("  Run #{run}/#{runs}...")

            {output, exit_code} = System.cmd("mix", ["run", runner_script],
              cd: Path.expand(".", __DIR__), stderr_to_stdout: true)

            if exit_code != 0 do
              IO.puts("Error: #{output}")
              raise "Benchmark run failed"
            end

            time_us =
              output
              |> String.split("\n", trim: true)
              |> List.last()
              |> String.to_integer()

            time_ms = time_us / 1000
            IO.puts("    Time: #{Float.round(time_ms, 2)} ms")
            time_ms
          end

          avg_ms = Enum.sum(timings) / length(timings)
          min_ms = Enum.min(timings)
          max_ms = Enum.max(timings)
          throughput_kb = total_bytes / (avg_ms / 1000) / 1024

          IO.puts("  Avg: #{Float.round(avg_ms, 2)} ms, Min: #{Float.round(min_ms, 2)} ms, Max: #{Float.round(max_ms, 2)} ms")
          IO.puts("  Throughput: #{Float.round(throughput_kb, 2)} KB/sec")
          IO.puts("")

          %{limit: limit, avg_ms: avg_ms, min_ms: min_ms, max_ms: max_ms, throughput_kb: throughput_kb}
        end)
      after
        IO.puts("Restoring original stabs.ex...")
        File.write!(@stabs_file, original_content)
        System.cmd("mix", ["compile", "--force"],
          cd: Path.expand(".", __DIR__), stderr_to_stdout: true)
        File.rm(file_list_path)
        File.rm(runner_script)
      end

    print_summary(results)
  end

  defp parse_limits(nil), do: @default_limits
  defp parse_limits(str), do: str |> String.split(",") |> Enum.map(&String.trim/1) |> Enum.map(&String.to_integer/1)

  defp collect_files(limit) do
    files = Path.wildcard(Path.join(@projects_dir, "**/*.{ex,exs}")) |> Enum.reject(&should_ignore_file?/1)
    if limit, do: Enum.take(files, limit), else: files
  end

  defp should_ignore_file?(path) do
    parts = Path.split(path)
    String.contains?(path, "projects/phoenix/installer/templates") or Enum.any?(@ignored_dirs, &(&1 in parts))
  end

  defp modify_scan_limit(content, limit) do
    new = String.replace(content,
      ~r/scan_classify\(state, cursor, \[\], ctx, terminator, stop_kinds, \d+, false\)/,
      "scan_classify(state, cursor, [], ctx, terminator, stop_kinds, #{limit}, false)")
    File.write!(@stabs_file, new)
  end

  defp print_summary(results) do
    IO.puts("")
    IO.puts(String.duplicate("=", 70))
    IO.puts("SUMMARY")
    IO.puts(String.duplicate("=", 70))
    IO.puts("")

    best = Enum.min_by(results, & &1.avg_ms)

    IO.puts(String.pad_trailing("Limit", 8) <>
            String.pad_leading("Avg (ms)", 12) <>
            String.pad_leading("Min (ms)", 12) <>
            String.pad_leading("Max (ms)", 12) <>
            String.pad_leading("KB/sec", 12) <>
            String.pad_leading("vs Best", 10))
    IO.puts(String.duplicate("-", 66))

    Enum.each(results, fn r ->
      delta = (r.avg_ms - best.avg_ms) / best.avg_ms * 100
      delta_str = if r.limit == best.limit, do: "BEST", else: "+#{Float.round(delta, 1)}%"

      IO.puts(
        String.pad_trailing("#{r.limit}", 8) <>
        String.pad_leading("#{Float.round(r.avg_ms, 2)}", 12) <>
        String.pad_leading("#{Float.round(r.min_ms, 2)}", 12) <>
        String.pad_leading("#{Float.round(r.max_ms, 2)}", 12) <>
        String.pad_leading("#{Float.round(r.throughput_kb, 1)}", 12) <>
        String.pad_leading(delta_str, 10)
      )
    end)

    IO.puts("")
    IO.puts("Optimal scan limit: #{best.limit} (#{Float.round(best.avg_ms, 2)} ms average)")
    IO.puts("")
  end
end

ScanLimitBenchmark.run(System.argv())
