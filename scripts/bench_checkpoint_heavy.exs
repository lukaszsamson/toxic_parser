#!/usr/bin/env elixir
# Phase 0 benchmark: Checkpoint-heavy parsing
#
# Measures ToxicParser performance on code patterns that stress checkpoint/rewind.
# These patterns include:
# - Stab clauses (fn expressions, case/cond blocks)
# - Bracket expressions with potential stabs (calls with fn args)
# - Map updates (update vs literal ambiguity)
# - Complex call expressions with keywords
#
# Tracks: wall time, reductions, GC counts, memory.
#
# Usage:
#   mix run scripts/bench_checkpoint_heavy.exs
#   mix run scripts/bench_checkpoint_heavy.exs --runs 10
#   mix run scripts/bench_checkpoint_heavy.exs --scale 2  # larger synthetic corpus

defmodule BenchCheckpointHeavy do
  @default_runs 5
  @default_scale 1

  def run(args) do
    opts = parse_args(args)

    IO.puts("=== Bench: Checkpoint-Heavy Parsing ===")
    IO.puts("Runs: #{opts.runs}")
    IO.puts("Scale: #{opts.scale}x")
    IO.puts("")

    # Generate synthetic checkpoint-heavy code
    contents = generate_checkpoint_heavy_code(opts.scale)
    total_bytes = Enum.reduce(contents, 0, fn c, acc -> acc + byte_size(c) end)
    IO.puts("Generated #{length(contents)} synthetic snippets")
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
    IO.puts("Snippets:   #{success_count} ok, #{error_count} errors")
    IO.puts("Throughput: #{Float.round(node_count / (avg_time / 1000), 0)} nodes/sec")
    IO.puts("Throughput: #{Float.round(total_bytes / (avg_time / 1000) / 1024, 2)} KB/sec")
    IO.puts("Reductions: #{Float.round(avg_reds / 1000, 1)}k avg per run")
    IO.puts("Reds/node:  #{Float.round(avg_reds / max(node_count, 1), 1)}")
    IO.puts("GC count:   #{Float.round(avg_gcs, 1)} avg per run")
    IO.puts("GC reclaim: #{Float.round(avg_gc_words * 8 / 1024, 1)} KB avg per run")
  end

  defp generate_checkpoint_heavy_code(scale) do
    base_count = 50 * scale

    # Pattern 1: fn expressions with multiple clauses (stab checkpoints)
    fn_exprs =
      for i <- 1..base_count do
        clauses =
          for j <- 1..5 do
            "  {#{j}, x#{i}} -> x#{i} + #{j}"
          end
          |> Enum.join("\n")

        """
        fn
        #{clauses}
        end
        """
      end

    # Pattern 2: case expressions with guards (stab + guard checkpoints)
    case_exprs =
      for i <- 1..base_count do
        clauses =
          for j <- 1..5 do
            "  x when is_integer(x) and x > #{j} -> x + #{i}"
          end
          |> Enum.join("\n")

        """
        case value#{i} do
        #{clauses}
        end
        """
      end

    # Pattern 3: calls with fn arguments (bracket + stab checkpoints)
    call_with_fn =
      for i <- 1..base_count do
        """
        Enum.map(list#{i}, fn
          {a, b} -> a + b
          x when is_number(x) -> x * 2
          _ -> nil
        end)
        """
      end

    # Pattern 4: map updates vs literals (map update checkpoint)
    map_updates =
      for i <- 1..base_count do
        """
        %{map#{i} | key: value#{i}, other: #{i}}
        """
      end

    # Pattern 5: keyword lists in calls (keyword checkpoint)
    keyword_calls =
      for i <- 1..base_count do
        """
        SomeModule.function#{i}(arg#{i}, key1: val1, key2: val2, key3: fn x -> x end)
        """
      end

    # Pattern 6: nested stabs in comprehensions
    comprehensions =
      for i <- 1..base_count do
        """
        for x <- list#{i}, y <- other#{i}, reduce: %{} do
          acc when is_map(acc) -> Map.put(acc, x, y)
          acc -> acc
        end
        """
      end

    # Pattern 7: cond expressions (multiple clause checkpoints)
    cond_exprs =
      for i <- 1..base_count do
        clauses =
          for j <- 1..5 do
            "  x#{i} > #{j} and y#{i} < #{j * 10} -> #{j}"
          end
          |> Enum.join("\n")

        """
        cond do
        #{clauses}
          true -> :default
        end
        """
      end

    # Pattern 8: complex nested expressions with brackets
    nested_brackets =
      for i <- 1..base_count do
        """
        [
          {a#{i}, b#{i}},
          %{key: [1, 2, {3, 4}], other: fn -> :ok end},
          Enum.map([#{i}], &(&1 * 2))
        ]
        """
      end

    # Pattern 9: quoted string keys in keyword lists (checkpoint for string key detection)
    quoted_keys =
      for i <- 1..base_count do
        """
        [
          "string_key_#{i}": value#{i},
          normal_key: other#{i},
          "another_string": fn x -> x end
        ]
        """
      end

    # Pattern 10: dot access with potential keyword continuation
    dot_patterns =
      for i <- 1..base_count do
        """
        module#{i}.function#{i}(arg).nested.field
        """
      end

    Enum.concat([
      fn_exprs,
      case_exprs,
      call_with_fn,
      map_updates,
      keyword_calls,
      comprehensions,
      cond_exprs,
      nested_brackets,
      quoted_keys,
      dot_patterns
    ])
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
          runs: :integer,
          scale: :integer
        ]
      )

    %{
      runs: Keyword.get(parsed, :runs, @default_runs),
      scale: Keyword.get(parsed, :scale, @default_scale)
    }
  end
end

BenchCheckpointHeavy.run(System.argv())
