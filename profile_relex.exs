#!/usr/bin/env elixir

# Re-lex analysis script for ToxicParser
# Measures how many times the lexer re-lexes the same source positions due to
# parser checkpoints and rewinds, and estimates the time cost.
#
# Usage:
#   elixir profile_relex.exs                      # Analyze all files
#   elixir profile_relex.exs --project phoenix    # Analyze specific project
#   elixir profile_relex.exs --limit 100          # Limit to first N files
#   elixir profile_relex.exs --target tokenizer   # Analyze tokenizer only
#   elixir profile_relex.exs --verbose            # Show per-file details
#   elixir profile_relex.exs --top 20             # Show top N files by relex ratio
#   elixir profile_relex.exs --time-analysis      # Include re-lex time estimation

Mix.install([
  {:toxic_parser, path: Path.expand(".", __DIR__)}
])

defmodule RelexProfiler do
  @moduledoc """
  Profiles re-lexing overhead in ToxicParser.

  Measures how many times Toxic.Driver.step is called with the same
  input position, indicating the parser rewound and re-lexed tokens.
  """

  @projects_dir "/Users/lukaszsamson/claude_fun/elixir_oss/projects/elixir"
  @ignored_dirs ["_build", "deps", ".git", "tmp", "priv", "rel", "cover", "doc", "logs"]
  @ignored_files ["mix.lock"]

  def run(args \\ []) do
    opts = parse_args(args)

    IO.puts("ToxicParser Re-lex Analyzer")
    IO.puts("===========================")
    IO.puts("Target: #{opts.target}")
    if opts.project, do: IO.puts("Project filter: #{opts.project}")
    if opts.limit, do: IO.puts("File limit: #{opts.limit}")
    IO.puts("")

    files = collect_files(opts)
    IO.puts("Collected #{length(files)} files to analyze")
    IO.puts("")

    if length(files) == 0 do
      IO.puts("No files found. Exiting.")
      System.halt(1)
    end

    # Warmup - ensure all code is loaded
    IO.puts("Loading code...")
    _ = profile_file(hd(files), opts)

    # Profile all files
    IO.puts("Analyzing re-lex overhead...")
    IO.puts("")

    {time_us, results} = :timer.tc(fn ->
      files
      |> Enum.with_index(1)
      |> Enum.map(fn {file, idx} ->
        if opts.verbose do
          IO.write("\r  [#{idx}/#{length(files)}] #{Path.basename(file)}#{String.duplicate(" ", 40)}")
        end
        profile_file(file, opts)
      end)
    end)

    if opts.verbose, do: IO.puts("")

    print_summary(results, time_us, opts)
  end

  defp parse_args(args) do
    {parsed, _rest, _invalid} =
      OptionParser.parse(args,
        strict: [
          project: :string,
          limit: :integer,
          target: :string,
          verbose: :boolean,
          top: :integer,
          time_analysis: :boolean
        ]
      )

    %{
      project: Keyword.get(parsed, :project),
      limit: Keyword.get(parsed, :limit),
      target: parse_target(Keyword.get(parsed, :target, "parser")),
      verbose: Keyword.get(parsed, :verbose, false),
      top: Keyword.get(parsed, :top, 10),
      time_analysis: Keyword.get(parsed, :time_analysis, false)
    }
  end

  defp parse_target("parser"), do: :parser
  defp parse_target("tokenizer"), do: :tokenizer
  defp parse_target(other), do: raise("Invalid target: #{other}. Use: parser, tokenizer")

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

  defp profile_file(path, opts) do
    content = File.read!(path)
    bytes = byte_size(content)
    target = opts.target

    # Enable tracing
    Process.put(:toxic_trace_lexing, true)
    Process.put({Toxic.Driver, :step_calls}, %{})

    # Run the target with tracing (this adds overhead but gives us call counts)
    {time_with_trace_us, result} = :timer.tc(fn ->
      case target do
        :parser -> parse_file(content)
        :tokenizer -> tokenize_file(content)
      end
    end)

    # Collect stats
    calls = Process.get({Toxic.Driver, :step_calls}, %{})

    # Disable tracing
    Process.put(:toxic_trace_lexing, false)

    total = Enum.sum(Map.values(calls))
    unique = map_size(calls)
    relex = total - unique

    # Find positions that were re-lexed the most
    hot_positions =
      calls
      |> Enum.filter(fn {_pos, count} -> count > 1 end)
      |> Enum.sort_by(fn {_pos, count} -> count end, :desc)
      |> Enum.take(5)

    # Time analysis: measure clean times without tracing overhead
    time_analysis =
      if opts.time_analysis do
        # Run without tracing for accurate timing
        {parse_time_us, _} = :timer.tc(fn -> parse_file(content) end)

        # Run tokenizer-only to measure pure lexing time
        {tokenize_time_us, _} = :timer.tc(fn -> tokenize_file(content) end)

        # Estimate re-lex time:
        # - tokenize_time is proportional to unique positions (no backtracking in tokenizer)
        # - During parsing, each relex is roughly the cost of one lex operation
        # - relex_time_estimate = (relex_calls / unique_positions) * tokenize_time
        relex_time_estimate_us =
          if unique > 0 do
            (relex / unique) * tokenize_time_us
          else
            0
          end

        # Parser overhead = parse_time - tokenize_time (roughly)
        parser_overhead_us = max(0, parse_time_us - tokenize_time_us)

        %{
          parse_time_us: parse_time_us,
          tokenize_time_us: tokenize_time_us,
          relex_time_estimate_us: relex_time_estimate_us,
          parser_overhead_us: parser_overhead_us,
          relex_pct_of_parse: if(parse_time_us > 0, do: relex_time_estimate_us / parse_time_us * 100, else: 0.0),
          relex_pct_of_lexing: if(tokenize_time_us > 0, do: relex_time_estimate_us / tokenize_time_us * 100, else: 0.0)
        }
      else
        nil
      end

    %{
      path: path,
      bytes: bytes,
      time_us: time_with_trace_us,
      total_step_calls: total,
      unique_positions: unique,
      relex_calls: relex,
      relex_ratio: if(total > 0, do: relex / total * 100, else: 0.0),
      hot_positions: hot_positions,
      success: match?({:ok, _}, result) or match?({:ok, _, _}, result),
      time_analysis: time_analysis
    }
  end

  defp parse_file(content) do
    ToxicParser.parse_string(content, mode: :strict, token_metadata: true)
  end

  defp tokenize_file(content) do
    stream = Toxic.new(content, 1, 1, [])
    count_tokens(stream, 0)
  end

  defp count_tokens(stream, count) do
    case Toxic.next(stream) do
      {:ok, _token, new_stream} ->
        count_tokens(new_stream, count + 1)

      {:eof, _final_stream} ->
        {:ok, count}

      {:error, _reason, new_stream} ->
        count_tokens(new_stream, count + 1)
    end
  end

  defp print_summary(results, total_time_us, opts) do
    successful = Enum.filter(results, & &1.success)
    failed = length(results) - length(successful)

    totals = Enum.reduce(successful, %{bytes: 0, calls: 0, unique: 0, relex: 0, time: 0}, fn r, acc ->
      %{
        bytes: acc.bytes + r.bytes,
        calls: acc.calls + r.total_step_calls,
        unique: acc.unique + r.unique_positions,
        relex: acc.relex + r.relex_calls,
        time: acc.time + r.time_us
      }
    end)

    IO.puts("=" |> String.duplicate(60))
    IO.puts("RE-LEX ANALYSIS RESULTS")
    IO.puts("=" |> String.duplicate(60))
    IO.puts("")
    IO.puts("Files analyzed: #{length(results)} (#{failed} failed)")
    IO.puts("Total bytes: #{format_bytes(totals.bytes)}")
    IO.puts("Total time: #{Float.round(total_time_us / 1000, 2)} ms")
    IO.puts("")
    IO.puts("-" |> String.duplicate(60))
    IO.puts("LEXER STEP CALLS")
    IO.puts("-" |> String.duplicate(60))
    IO.puts("Total Driver.step calls: #{format_number(totals.calls)}")
    IO.puts("Unique positions: #{format_number(totals.unique)}")
    IO.puts("Re-lex calls: #{format_number(totals.relex)}")
    IO.puts("")
    IO.puts("RE-LEX RATIO: #{Float.round(totals.relex / max(totals.calls, 1) * 100, 2)}%")
    IO.puts("")

    if totals.relex > 0 do
      IO.puts("Interpretation:")
      IO.puts("  - #{Float.round(totals.relex / max(totals.calls, 1) * 100, 2)}% of lexer calls re-lex previously seen positions")
      IO.puts("  - This overhead comes from parser checkpoints/rewinds")
      avg_relex_per_file = totals.relex / max(length(successful), 1)
      IO.puts("  - Average re-lex calls per file: #{Float.round(avg_relex_per_file, 1)}")
    end

    # Time analysis section
    if opts.time_analysis do
      print_time_analysis(successful)
    end

    IO.puts("")
    IO.puts("-" |> String.duplicate(60))
    IO.puts("TOP #{opts.top} FILES BY RE-LEX RATIO")
    IO.puts("-" |> String.duplicate(60))

    successful
    |> Enum.filter(& &1.relex_calls > 0)
    |> Enum.sort_by(& &1.relex_ratio, :desc)
    |> Enum.take(opts.top)
    |> Enum.with_index(1)
    |> Enum.each(fn {r, idx} ->
      ratio = Float.round(r.relex_ratio, 1)
      IO.puts("#{String.pad_leading(to_string(idx), 2)}. #{String.pad_leading("#{ratio}%", 6)} (#{r.relex_calls}/#{r.total_step_calls}) #{truncate_path(r.path, 45)}")

      # Show hot positions for top files
      if idx <= 3 and length(r.hot_positions) > 0 do
        IO.puts("      Hot positions (line:col -> count):")
        Enum.each(r.hot_positions, fn {{line, col, _rest_size}, count} ->
          IO.puts("        #{line}:#{col} -> #{count}x")
        end)
      end
    end)

    IO.puts("")
    IO.puts("-" |> String.duplicate(60))
    IO.puts("TOP #{opts.top} FILES BY ABSOLUTE RE-LEX COUNT")
    IO.puts("-" |> String.duplicate(60))

    successful
    |> Enum.filter(& &1.relex_calls > 0)
    |> Enum.sort_by(& &1.relex_calls, :desc)
    |> Enum.take(opts.top)
    |> Enum.with_index(1)
    |> Enum.each(fn {r, idx} ->
      IO.puts("#{String.pad_leading(to_string(idx), 2)}. #{String.pad_leading(to_string(r.relex_calls), 6)} re-lexes in #{truncate_path(r.path, 45)}")
    end)

    # Distribution analysis
    IO.puts("")
    IO.puts("-" |> String.duplicate(60))
    IO.puts("RE-LEX DISTRIBUTION")
    IO.puts("-" |> String.duplicate(60))

    bins = [
      {0, 0, "0% (no re-lex)"},
      {0.01, 1, "0-1%"},
      {1, 5, "1-5%"},
      {5, 10, "5-10%"},
      {10, 20, "10-20%"},
      {20, 100, "20%+"}
    ]

    Enum.each(bins, fn {min, max, label} ->
      count = Enum.count(successful, fn r ->
        r.relex_ratio >= min and r.relex_ratio < max
      end)
      pct = count / max(length(successful), 1) * 100
      bar = String.duplicate("█", round(pct / 2))
      IO.puts("  #{String.pad_trailing(label, 15)} #{String.pad_leading(to_string(count), 5)} files #{bar}")
    end)

    IO.puts("")
  end

  defp print_time_analysis(results) do
    # Only include results that have time_analysis data
    with_timing = Enum.filter(results, & &1.time_analysis)

    if length(with_timing) == 0 do
      IO.puts("")
      IO.puts("No timing data collected.")
    else
      print_time_analysis_data(with_timing)
    end
  end

  defp print_time_analysis_data(with_timing) do
    # Aggregate timing data
    totals = Enum.reduce(with_timing, %{
      parse_time: 0,
      tokenize_time: 0,
      relex_time_estimate: 0,
      parser_overhead: 0
    }, fn r, acc ->
      ta = r.time_analysis
      %{
        parse_time: acc.parse_time + ta.parse_time_us,
        tokenize_time: acc.tokenize_time + ta.tokenize_time_us,
        relex_time_estimate: acc.relex_time_estimate + ta.relex_time_estimate_us,
        parser_overhead: acc.parser_overhead + ta.parser_overhead_us
      }
    end)

    IO.puts("")
    IO.puts("-" |> String.duplicate(60))
    IO.puts("RE-LEX TIME ANALYSIS")
    IO.puts("-" |> String.duplicate(60))
    IO.puts("")
    IO.puts("Methodology:")
    IO.puts("  1. Run tokenizer-only to measure lexer baseline (no backtracking)")
    IO.puts("  2. Estimate re-lex time = (re-lex calls / unique positions) * tokenizer time")
    IO.puts("  Note: This is an approximation; actual re-lex cost may vary per token.")
    IO.puts("")

    parse_ms = totals.parse_time / 1000
    tokenize_ms = totals.tokenize_time / 1000
    relex_ms = totals.relex_time_estimate / 1000
    parser_overhead_ms = totals.parser_overhead / 1000

    IO.puts("Total parse time:        #{Float.round(parse_ms, 2)} ms")
    IO.puts("Total tokenize time:     #{Float.round(tokenize_ms, 2)} ms (lexer-only baseline)")
    IO.puts("Parser overhead:         #{Float.round(parser_overhead_ms, 2)} ms (parse - tokenize)")
    IO.puts("")
    IO.puts("Estimated re-lex time:   #{Float.round(relex_ms, 2)} ms")
    IO.puts("")

    # Calculate percentages
    relex_pct_of_parse = if totals.parse_time > 0, do: totals.relex_time_estimate / totals.parse_time * 100, else: 0
    relex_pct_of_tokenize = if totals.tokenize_time > 0, do: totals.relex_time_estimate / totals.tokenize_time * 100, else: 0
    tokenize_pct_of_parse = if totals.parse_time > 0, do: totals.tokenize_time / totals.parse_time * 100, else: 0

    IO.puts("RE-LEX AS % OF TOTAL PARSE TIME: #{Float.round(relex_pct_of_parse, 2)}%")
    IO.puts("RE-LEX AS % OF LEXER TIME:       #{Float.round(relex_pct_of_tokenize, 2)}%")
    IO.puts("LEXER AS % OF TOTAL PARSE TIME:  #{Float.round(tokenize_pct_of_parse, 2)}%")
    IO.puts("")

    # Time breakdown visualization
    IO.puts("Time breakdown:")
    total = totals.parse_time
    lexer_first_pass = totals.tokenize_time - totals.relex_time_estimate
    lexer_first_pass = max(0, lexer_first_pass)

    bar_width = 50
    if total > 0 do
      first_pass_pct = lexer_first_pass / total
      relex_pct = totals.relex_time_estimate / total
      parser_pct = totals.parser_overhead / total

      first_pass_chars = round(first_pass_pct * bar_width)
      relex_chars = round(relex_pct * bar_width)
      parser_chars = bar_width - first_pass_chars - relex_chars

      bar = String.duplicate("L", first_pass_chars) <>
            String.duplicate("R", relex_chars) <>
            String.duplicate("P", max(0, parser_chars))

      IO.puts("  [#{bar}]")
      IO.puts("  L = Lexer (first pass): #{Float.round(first_pass_pct * 100, 1)}%")
      IO.puts("  R = Re-lex overhead:    #{Float.round(relex_pct * 100, 1)}%")
      IO.puts("  P = Parser overhead:    #{Float.round(parser_pct * 100, 1)}%")
    end

    IO.puts("")
    IO.puts("Interpretation:")
    IO.puts("  - Eliminating re-lexing would save ~#{Float.round(relex_ms, 1)} ms (#{Float.round(relex_pct_of_parse, 1)}% of parse time)")

    potential_speedup = if totals.parse_time > totals.relex_time_estimate do
      totals.parse_time / (totals.parse_time - totals.relex_time_estimate)
    else
      1.0
    end
    IO.puts("  - Potential speedup: #{Float.round(potential_speedup, 2)}x")

    # Show top files by re-lex time
    IO.puts("")
    IO.puts("Top 10 files by estimated re-lex time:")

    with_timing
    |> Enum.sort_by(& &1.time_analysis.relex_time_estimate_us, :desc)
    |> Enum.take(10)
    |> Enum.with_index(1)
    |> Enum.each(fn {r, idx} ->
      ta = r.time_analysis
      relex_ms = ta.relex_time_estimate_us / 1000
      # Cap at 99% to avoid confusing >100% values from measurement noise
      pct = min(99.0, ta.relex_pct_of_parse)
      IO.puts("  #{String.pad_leading(to_string(idx), 2)}. #{Float.round(relex_ms, 1)} ms (~#{Float.round(pct, 0)}% of parse) #{truncate_path(r.path, 35)}")
    end)
  end

  defp format_bytes(bytes) when bytes < 1024, do: "#{bytes} B"
  defp format_bytes(bytes) when bytes < 1024 * 1024, do: "#{Float.round(bytes / 1024, 1)} KB"
  defp format_bytes(bytes), do: "#{Float.round(bytes / 1024 / 1024, 2)} MB"

  defp format_number(n) when n < 1000, do: to_string(n)
  defp format_number(n) when n < 1_000_000, do: "#{Float.round(n / 1000, 1)}K"
  defp format_number(n), do: "#{Float.round(n / 1_000_000, 2)}M"

  defp truncate_path(path, max_len) do
    basename = Path.basename(path)
    if String.length(path) <= max_len do
      path
    else
      # Show .../<parent>/<file>
      parent = path |> Path.dirname() |> Path.basename()
      short = ".../" <> parent <> "/" <> basename
      if String.length(short) <= max_len do
        short
      else
        "..." <> String.slice(basename, -(max_len - 3), max_len - 3)
      end
    end
  end
end

# Run the profiler
RelexProfiler.run(System.argv())
