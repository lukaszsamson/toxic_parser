#!/usr/bin/env elixir
# Benchmark: Scaling comparison - ToxicParser/Toxic vs Elixir
#
# Tests how parsers and lexers scale with input size by concatenating
# corpus files into increasingly larger inputs.
#
# Compares:
#   - Lexer: Toxic.tokenize vs :elixir_tokenizer.tokenize
#   - Parser: ToxicParser vs Code.string_to_quoted
#
# Usage:
#   mix run scripts/bench_scaling.exs
#   mix run scripts/bench_scaling.exs --max-size 500  # KB
#   mix run scripts/bench_scaling.exs --steps 8

defmodule BenchScaling do
  @projects_dir "/Users/lukaszsamson/claude_fun/elixir_oss/projects/elixir"
  @ignored_dirs ["_build", "deps", ".git", "tmp", "priv", "rel", "cover", "doc", "logs"]
  @default_max_size_kb 200
  @default_steps 6
  @warmup_runs 2
  @bench_runs 3

  def run(args) do
    opts = parse_args(args)

    IO.puts("=== Scaling Benchmark: ToxicParser/Toxic vs Elixir ===")
    IO.puts("Max size: #{opts.max_size_kb} KB")
    IO.puts("Steps: #{opts.steps}")
    IO.puts("Runs per step: #{@bench_runs}")
    IO.puts("")

    # Collect source files
    files = collect_files()
    IO.puts("Corpus files: #{length(files)}")

    # Load and concatenate content
    all_content = files |> Enum.map(&File.read!/1) |> Enum.join("\n\n")
    IO.puts("Total corpus size: #{div(byte_size(all_content), 1024)} KB")
    IO.puts("")

    # Load individual files
    file_contents = Enum.map(files, &File.read!/1)

    # Generate test inputs by concatenating complete files
    max_bytes = opts.max_size_kb * 1024
    inputs = build_inputs(file_contents, opts.steps, max_bytes)

    IO.puts("Test sizes: #{Enum.map(inputs, fn {s, _} -> "#{div(s, 1024)}KB" end) |> Enum.join(", ")}")
    IO.puts("")

    # Run benchmarks
    IO.puts("=== Lexer Comparison ===")
    IO.puts("")
    lexer_results = run_lexer_benchmarks(inputs)

    IO.puts("")
    IO.puts("=== Parser Comparison ===")
    IO.puts("")
    parser_results = run_parser_benchmarks(inputs)

    # Write results
    write_results(lexer_results, parser_results, opts)

    IO.puts("")
    IO.puts("Results written to benchmarks/comparison/")
  end

  defp build_inputs(file_contents, steps, max_bytes) do
    # Build increasingly larger inputs by concatenating complete files
    step_size = max_bytes / steps

    {inputs, _} =
      Enum.reduce(1..steps, {[], {0, ""}}, fn step, {acc, {current_size, current_content}} ->
        target_size = round(step_size * step)

        # Add files until we reach target size
        {new_size, new_content} =
          Enum.reduce_while(file_contents, {current_size, current_content}, fn file, {size, content} ->
            if size >= target_size do
              {:halt, {size, content}}
            else
              new_content = content <> "\n\n" <> file
              {:cont, {byte_size(new_content), new_content}}
            end
          end)

        if new_size > current_size do
          {[{new_size, new_content} | acc], {new_size, new_content}}
        else
          {acc, {current_size, current_content}}
        end
      end)

    inputs
    |> Enum.reverse()
    |> Enum.uniq_by(&elem(&1, 0))
  end

  defp run_lexer_benchmarks(inputs) do
    IO.puts("Warming up lexers...")
    {_, sample} = hd(inputs)
    for _ <- 1..@warmup_runs do
      _ = lex_toxic(sample)
      _ = lex_elixir(sample)
    end

    IO.puts("")
    IO.puts("| Size (KB) | Toxic (ms) | Elixir (ms) | Ratio | Toxic tok/s | Elixir tok/s |")
    IO.puts("|-----------|------------|-------------|-------|-------------|--------------|")

    Enum.map(inputs, fn {size, content} ->
      size_kb = div(size, 1024)

      # Benchmark Toxic
      toxic_times =
        for _ <- 1..@bench_runs do
          {time_us, token_count} = :timer.tc(fn -> lex_toxic(content) end)
          {time_us / 1000, token_count}
        end

      toxic_avg = Enum.sum(Enum.map(toxic_times, &elem(&1, 0))) / @bench_runs
      toxic_tokens = elem(hd(toxic_times), 1)

      # Benchmark Elixir
      elixir_times =
        for _ <- 1..@bench_runs do
          {time_us, token_count} = :timer.tc(fn -> lex_elixir(content) end)
          {time_us / 1000, token_count}
        end

      elixir_avg = Enum.sum(Enum.map(elixir_times, &elem(&1, 0))) / @bench_runs
      elixir_tokens = elem(hd(elixir_times), 1)

      ratio = toxic_avg / max(elixir_avg, 0.001)
      toxic_tps = toxic_tokens / (toxic_avg / 1000)
      elixir_tps = elixir_tokens / (elixir_avg / 1000)

      IO.puts(
        "| #{pad(size_kb, 9)} | #{pad(Float.round(toxic_avg, 2), 10)} | " <>
          "#{pad(Float.round(elixir_avg, 2), 11)} | #{pad(Float.round(ratio, 2), 5)}x | " <>
          "#{pad(round(toxic_tps), 11)} | #{pad(round(elixir_tps), 12)} |"
      )

      %{
        size_kb: size_kb,
        size_bytes: size,
        toxic_ms: toxic_avg,
        elixir_ms: elixir_avg,
        ratio: ratio,
        toxic_tokens: toxic_tokens,
        elixir_tokens: elixir_tokens
      }
    end)
  end

  defp run_parser_benchmarks(inputs) do
    IO.puts("Warming up parsers...")
    {_, sample} = hd(inputs)
    for _ <- 1..@warmup_runs do
      _ = parse_toxic(sample)
      _ = parse_elixir(sample)
    end

    IO.puts("")
    IO.puts("| Size (KB) | Toxic (ms) | Elixir (ms) | Ratio | Toxic node/s | Elixir node/s |")
    IO.puts("|-----------|------------|-------------|-------|--------------|---------------|")

    Enum.map(inputs, fn {size, content} ->
      size_kb = div(size, 1024)

      # Benchmark ToxicParser
      toxic_times =
        for _ <- 1..@bench_runs do
          {time_us, node_count} = :timer.tc(fn -> parse_toxic(content) end)
          {time_us / 1000, node_count}
        end

      toxic_avg = Enum.sum(Enum.map(toxic_times, &elem(&1, 0))) / @bench_runs
      toxic_nodes = elem(hd(toxic_times), 1)

      # Benchmark Elixir
      elixir_times =
        for _ <- 1..@bench_runs do
          {time_us, node_count} = :timer.tc(fn -> parse_elixir(content) end)
          {time_us / 1000, node_count}
        end

      elixir_avg = Enum.sum(Enum.map(elixir_times, &elem(&1, 0))) / @bench_runs
      elixir_nodes = elem(hd(elixir_times), 1)

      ratio = toxic_avg / max(elixir_avg, 0.001)
      toxic_nps = toxic_nodes / (toxic_avg / 1000)
      elixir_nps = elixir_nodes / (elixir_avg / 1000)

      IO.puts(
        "| #{pad(size_kb, 9)} | #{pad(Float.round(toxic_avg, 2), 10)} | " <>
          "#{pad(Float.round(elixir_avg, 2), 11)} | #{pad(Float.round(ratio, 2), 5)}x | " <>
          "#{pad(round(toxic_nps), 12)} | #{pad(round(elixir_nps), 13)} |"
      )

      %{
        size_kb: size_kb,
        size_bytes: size,
        toxic_ms: toxic_avg,
        elixir_ms: elixir_avg,
        ratio: ratio,
        toxic_nodes: toxic_nodes,
        elixir_nodes: elixir_nodes
      }
    end)
  end

  defp lex_toxic(content) do
    rest = String.to_charlist(content)
    driver = Toxic.Driver.new([])
    count_tokens_toxic(rest, driver, 0)
  end

  defp count_tokens_toxic(rest, driver, count) do
    case Toxic.Driver.next(rest, driver) do
      {:ok, {:eof, _, _}, _rest, _driver} -> count
      {:ok, _tok, rest, driver} -> count_tokens_toxic(rest, driver, count + 1)
      {:eof, _driver} -> count
      {:error, _reason, rest, driver} -> count_tokens_toxic(rest, driver, count)
    end
  end

  defp lex_elixir(content) do
    charlist = String.to_charlist(content)
    case :elixir_tokenizer.tokenize(charlist, 1, 1, []) do
      {:ok, _line, _col, _warnings, tokens, _terminators} -> length(tokens)
      {:error, _, _, _, _, _} -> 0
    end
  end

  defp parse_toxic(content) do
    # Use tolerant mode for partial content
    case ToxicParser.parse_string(content, mode: :tolerant, token_metadata: true) do
      {:ok, result} -> count_ast_nodes(result.ast)
      {:error, _} -> 0
    end
  end

  defp parse_elixir(content) do
    # Wrap in try to handle partial content parse errors
    try do
      case Code.string_to_quoted(content, columns: true, token_metadata: true) do
        {:ok, ast} -> count_ast_nodes(ast)
        {:error, _} -> 0
      end
    rescue
      _ -> 0
    end
  end

  defp count_ast_nodes(nil), do: 0
  defp count_ast_nodes(ast) when is_list(ast), do: Enum.sum(Enum.map(ast, &count_ast_nodes/1))
  defp count_ast_nodes({_form, _meta, args}) when is_list(args), do: 1 + count_ast_nodes(args)
  defp count_ast_nodes({left, right}), do: count_ast_nodes(left) + count_ast_nodes(right)
  defp count_ast_nodes(_), do: 1

  defp pad(value, width) do
    str = to_string(value)
    String.pad_leading(str, width)
  end

  defp write_results(lexer_results, parser_results, _opts) do
    timestamp = DateTime.utc_now() |> DateTime.to_iso8601()

    # Write lexer results
    lexer_csv = [
      "size_kb,toxic_ms,elixir_ms,ratio,toxic_tokens,elixir_tokens",
      Enum.map(lexer_results, fn r ->
        "#{r.size_kb},#{r.toxic_ms},#{r.elixir_ms},#{r.ratio},#{r.toxic_tokens},#{r.elixir_tokens}"
      end)
    ]
    |> List.flatten()
    |> Enum.join("\n")

    File.write!("benchmarks/comparison/lexer_scaling.csv", lexer_csv)

    # Write parser results
    parser_csv = [
      "size_kb,toxic_ms,elixir_ms,ratio,toxic_nodes,elixir_nodes",
      Enum.map(parser_results, fn r ->
        "#{r.size_kb},#{r.toxic_ms},#{r.elixir_ms},#{r.ratio},#{r.toxic_nodes},#{r.elixir_nodes}"
      end)
    ]
    |> List.flatten()
    |> Enum.join("\n")

    File.write!("benchmarks/comparison/parser_scaling.csv", parser_csv)

    # Write summary markdown
    summary = """
    # Scaling Benchmark Results

    Generated: #{timestamp}

    ## Lexer Comparison: Toxic vs :elixir_tokenizer

    | Size (KB) | Toxic (ms) | Elixir (ms) | Ratio |
    |-----------|------------|-------------|-------|
    #{Enum.map(lexer_results, fn r -> "| #{r.size_kb} | #{Float.round(r.toxic_ms, 2)} | #{Float.round(r.elixir_ms, 2)} | #{Float.round(r.ratio, 2)}x |" end) |> Enum.join("\n")}

    ## Parser Comparison: ToxicParser vs Code.string_to_quoted

    | Size (KB) | Toxic (ms) | Elixir (ms) | Ratio |
    |-----------|------------|-------------|-------|
    #{Enum.map(parser_results, fn r -> "| #{r.size_kb} | #{Float.round(r.toxic_ms, 2)} | #{Float.round(r.elixir_ms, 2)} | #{Float.round(r.ratio, 2)}x |" end) |> Enum.join("\n")}

    ## Analysis

    ### Lexer
    - Average ratio: #{Float.round(Enum.sum(Enum.map(lexer_results, & &1.ratio)) / length(lexer_results), 2)}x slower
    - Scaling: #{if linear_scaling?(lexer_results), do: "Linear", else: "Non-linear"}

    ### Parser
    - Average ratio: #{Float.round(Enum.sum(Enum.map(parser_results, & &1.ratio)) / length(parser_results), 2)}x slower
    - Scaling: #{if linear_scaling?(parser_results), do: "Linear", else: "Non-linear"}
    """

    File.write!("benchmarks/comparison/RESULTS.md", summary)
  end

  defp linear_scaling?(results) do
    # Check if time grows roughly linearly with size
    # by comparing ratio of time to ratio of size
    if length(results) < 2 do
      true
    else
      first = hd(results)
      last = List.last(results)

      size_ratio = last.size_kb / max(first.size_kb, 1)
      time_ratio = last.toxic_ms / max(first.toxic_ms, 0.001)

      # If time_ratio is within 2x of size_ratio, consider it linear
      time_ratio / size_ratio < 2.0
    end
  end

  defp parse_args(args) do
    {parsed, _rest, _invalid} =
      OptionParser.parse(args,
        strict: [
          max_size: :integer,
          steps: :integer
        ],
        aliases: [s: :steps, m: :max_size]
      )

    %{
      max_size_kb: Keyword.get(parsed, :max_size, @default_max_size_kb),
      steps: Keyword.get(parsed, :steps, @default_steps)
    }
  end

  defp collect_files do
    Path.wildcard(Path.join(@projects_dir, "**/*.{ex,exs}"))
    |> Enum.reject(&should_ignore_file?/1)
    |> Enum.take(200)  # Limit to avoid huge concatenation
  end

  defp should_ignore_file?(file_path) do
    path_parts = Path.split(file_path)
    phoenix_templates = String.contains?(file_path, "installer/templates")
    phoenix_templates or Enum.any?(@ignored_dirs, &(&1 in path_parts))
  end
end

BenchScaling.run(System.argv())
