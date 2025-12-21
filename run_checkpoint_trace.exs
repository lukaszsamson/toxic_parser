# Trace checkpoint/rewind calls to understand their sources

defmodule CheckpointTracer do
  def run do
    files = Path.wildcard("/Users/lukaszsamson/claude_fun/elixir_oss/projects/elixir/**/*.{ex,exs}")
    |> Enum.reject(fn f ->
      String.contains?(f, "_build") or
      String.contains?(f, "deps") or
      String.contains?(f, ".git") or
      String.contains?(f, "installer/templates")
    end)
    |> Enum.take(50)

    IO.puts("Tracing checkpoint/rewind calls for #{length(files)} files...")

    # Use process dictionary to track calls
    Process.put(:checkpoint_sites, %{})
    Process.put(:rewind_sites, %{})

    # Warmup
    hd(files) |> File.read!() |> ToxicParser.parse_string(mode: :strict, token_metadata: true)

    # Reset counters
    Process.put(:checkpoint_sites, %{})
    Process.put(:rewind_sites, %{})

    # Parse all files
    Enum.each(files, fn file ->
      content = File.read!(file)
      ToxicParser.parse_string(content, mode: :strict, token_metadata: true)
    end)

    checkpoint_sites = Process.get(:checkpoint_sites)
    rewind_sites = Process.get(:rewind_sites)

    IO.puts("\n=== Checkpoint Sites ===")
    checkpoint_sites
    |> Enum.sort_by(fn {_, count} -> -count end)
    |> Enum.each(fn {site, count} ->
      IO.puts("  #{count}\t#{site}")
    end)

    IO.puts("\n=== Rewind Sites ===")
    rewind_sites
    |> Enum.sort_by(fn {_, count} -> -count end)
    |> Enum.each(fn {site, count} ->
      IO.puts("  #{count}\t#{site}")
    end)

    total_checkpoints = Enum.sum(Map.values(checkpoint_sites))
    total_rewinds = Enum.sum(Map.values(rewind_sites))
    IO.puts("\nTotal checkpoints: #{total_checkpoints}")
    IO.puts("Total rewinds: #{total_rewinds}")
    IO.puts("Rewind rate: #{Float.round(total_rewinds / max(total_checkpoints, 1) * 100, 1)}%")
  end
end

# We need to trace at runtime - let's use :dbg instead
defmodule RuntimeTracer do
  def run do
    files = Path.wildcard("/Users/lukaszsamson/claude_fun/elixir_oss/projects/elixir/**/*.{ex,exs}")
    |> Enum.reject(fn f ->
      String.contains?(f, "_build") or
      String.contains?(f, "deps") or
      String.contains?(f, ".git") or
      String.contains?(f, "installer/templates")
    end)
    |> Enum.take(20)

    IO.puts("Analyzing checkpoint/rewind patterns for #{length(files)} files...")

    # Collect stack traces for checkpoint and rewind calls
    checkpoint_callers = :ets.new(:checkpoint_callers, [:set, :public])
    rewind_callers = :ets.new(:rewind_callers, [:set, :public])

    tracer_pid = spawn(fn -> tracer_loop(checkpoint_callers, rewind_callers) end)

    :erlang.trace(:all, true, [:call, {:tracer, tracer_pid}])

    :erlang.trace_pattern({ToxicParser.TokenAdapter, :checkpoint, 1}, [{:_, [], [{:return_trace}]}], [:local])
    :erlang.trace_pattern({ToxicParser.TokenAdapter, :rewind, 2}, [{:_, [], [{:return_trace}]}], [:local])

    # Parse files
    Enum.each(files, fn file ->
      content = File.read!(file)
      ToxicParser.parse_string(content, mode: :strict, token_metadata: true)
    end)

    # Stop tracing
    :erlang.trace(:all, false, [:call])
    send(tracer_pid, :stop)
    Process.sleep(100)

    # Analyze results
    IO.puts("\n=== Checkpoint Callers ===")
    :ets.tab2list(checkpoint_callers)
    |> Enum.sort_by(fn {_, count} -> -count end)
    |> Enum.take(20)
    |> Enum.each(fn {caller, count} ->
      IO.puts("  #{count}\t#{inspect(caller)}")
    end)

    IO.puts("\n=== Rewind Callers ===")
    :ets.tab2list(rewind_callers)
    |> Enum.sort_by(fn {_, count} -> -count end)
    |> Enum.take(20)
    |> Enum.each(fn {caller, count} ->
      IO.puts("  #{count}\t#{inspect(caller)}")
    end)

    :ets.delete(checkpoint_callers)
    :ets.delete(rewind_callers)
  end

  defp tracer_loop(checkpoint_callers, rewind_callers) do
    receive do
      :stop ->
        :ok

      {:trace, _pid, :call, {ToxicParser.TokenAdapter, :checkpoint, _args}} ->
        # Get caller from stack
        {:current_stacktrace, stack} = Process.info(self(), :current_stacktrace)
        caller = extract_caller(stack)
        :ets.update_counter(checkpoint_callers, caller, 1, {caller, 0})
        tracer_loop(checkpoint_callers, rewind_callers)

      {:trace, _pid, :call, {ToxicParser.TokenAdapter, :rewind, _args}} ->
        {:current_stacktrace, stack} = Process.info(self(), :current_stacktrace)
        caller = extract_caller(stack)
        :ets.update_counter(rewind_callers, caller, 1, {caller, 0})
        tracer_loop(checkpoint_callers, rewind_callers)

      {:trace, _pid, :return_from, _, _} ->
        tracer_loop(checkpoint_callers, rewind_callers)

      _other ->
        tracer_loop(checkpoint_callers, rewind_callers)
    end
  end

  defp extract_caller(stack) do
    # Find the first non-TokenAdapter caller
    stack
    |> Enum.drop(2)  # Skip tracer frames
    |> Enum.find(fn {mod, _fun, _arity, _loc} ->
      mod != ToxicParser.TokenAdapter
    end)
    |> case do
      {mod, fun, arity, _loc} -> {mod, fun, arity}
      nil -> :unknown
    end
  end
end

# Simpler approach: just count by grepping the source
IO.puts("=== Checkpoint/Rewind Call Sites in Source ===\n")

checkpoint_sites = [
  {"keywords.ex:133", "try_parse_kw_data (quoted string key)"},
  {"keywords.ex:175", "try_parse_kw_call (quoted string key)"},
  {"keywords.ex:239", "try_parse_call_args_no_parens_kw_expr"},
  {"keywords.ex:305", "try_parse_call_args_no_parens_kw"},
  {"maps.ex:367", "try_parse_map_update"},
  {"stabs.ex:120", "parse_paren_stab (inner paren check)"},
  {"brackets.ex:39", "parse (bracket container)"},
  {"blocks.ex:187", "try_parse_clause"},
  {"dots.ex:264", "check_dot_kw (keyword after dot)"},
  {"calls_private.ex:72", "parse_call_args_parens_item_maybe_stab"},
]

IO.puts("Checkpoint sites:")
Enum.each(checkpoint_sites, fn {loc, desc} ->
  IO.puts("  #{loc}: #{desc}")
end)

IO.puts("\nNow running profiling to count actual calls...")

# Simple profiling with call counting
files = Path.wildcard("/Users/lukaszsamson/claude_fun/elixir_oss/projects/elixir/**/*.{ex,exs}")
|> Enum.reject(fn f ->
  String.contains?(f, "_build") or
  String.contains?(f, "deps") or
  String.contains?(f, ".git") or
  String.contains?(f, "installer/templates")
end)
|> Enum.take(50)

IO.puts("\nProfiling #{length(files)} files with call counting...")

Mix.ensure_application!(:tools)

workload = fn ->
  Enum.each(files, fn file ->
    content = File.read!(file)
    ToxicParser.parse_string(content, mode: :strict, token_metadata: true)
  end)
end

# Profile with call counts
Mix.Tasks.Profile.Tprof.profile(workload, [type: :calls, matching: {ToxicParser.Grammar.Keywords, :_, :_}])
