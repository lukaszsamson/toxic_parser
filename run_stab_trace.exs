# Stab Tracing Script
# Run with: mix run run_stab_trace.exs

alias ToxicParser.Grammar.StabsTrace

# Get Elixir lib path
elixir_lib = System.get_env("ELIXIR_LIB_PATH", "/Users/lukaszsamson/elixir/lib/elixir/lib")

# Collect files
files =
  Path.wildcard("#{elixir_lib}/**/*.ex")
  |> Enum.take(30)

IO.puts("Tracing stab parsing for #{length(files)} files...")
IO.puts("")

# Start tracing
StabsTrace.start_trace()

# Parse files
start_time = System.monotonic_time(:millisecond)

results = Enum.map(files, fn file ->
  code = File.read!(file)
  case ToxicParser.parse_string(code, mode: :strict, token_metadata: true) do
    {:ok, _ast} -> :ok
    {:error, _} -> :error
  end
end)

end_time = System.monotonic_time(:millisecond)
elapsed = end_time - start_time

# Get trace stats
stats = StabsTrace.stop_trace()

# Calculate success rate
ok_count = Enum.count(results, &(&1 == :ok))
error_count = Enum.count(results, &(&1 == :error))

IO.puts("Parse results: #{ok_count} ok, #{error_count} errors")
IO.puts("Total time: #{elapsed}ms")
IO.puts("")

IO.puts("Stab Trace Statistics:")
IO.puts("=" |> String.duplicate(50))

if map_size(stats) == 0 do
  IO.puts("No trace data collected (tracing may not be working)")
else
  StabsTrace.print_stats(stats)

  IO.puts("")
  IO.puts("Key Metrics:")
  IO.puts("-" |> String.duplicate(50))

  # Calculate key ratios
  check_calls = Map.get(stats, :check_and_collect_stab_body, 0)
  was_stab = Map.get(stats, :check_stab_body_was_stab, 0)
  not_stab = Map.get(stats, :check_stab_body_not_stab, 0)
  error_stab = Map.get(stats, :check_stab_body_error, 0)

  try_stab_ok = Map.get(stats, {:try_parse_stab_clause, :ok}, 0)
  try_stab_not = Map.get(stats, {:try_parse_stab_clause, :not_stab}, 0)
  try_stab_err = Map.get(stats, {:try_parse_stab_clause, :error}, 0)

  parse_body = Map.get(stats, :parse_stab_body, 0)
  parse_eoe = Map.get(stats, :parse_stab_eoe_until, 0)

  IO.puts("check_and_collect_stab_body calls: #{check_calls}")
  IO.puts("  - resulted in stab: #{was_stab} (#{if check_calls > 0, do: Float.round(was_stab / check_calls * 100, 1), else: 0}%)")
  IO.puts("  - resulted in not_stab: #{not_stab} (#{if check_calls > 0, do: Float.round(not_stab / check_calls * 100, 1), else: 0}%)")
  IO.puts("  - resulted in error: #{error_stab}")
  IO.puts("")
  IO.puts("try_parse_stab_clause outcomes:")
  IO.puts("  - :ok (was stab): #{try_stab_ok}")
  IO.puts("  - :not_stab: #{try_stab_not}")
  IO.puts("  - :error: #{try_stab_err}")
  IO.puts("")
  IO.puts("parse_stab_body calls: #{parse_body}")
  IO.puts("parse_stab_eoe_until calls: #{parse_eoe}")

  # Double-parse waste calculation
  double_parses = not_stab
  if double_parses > 0 do
    IO.puts("")
    IO.puts("WASTE ANALYSIS:")
    IO.puts("  Expressions double-parsed: #{double_parses}")
    IO.puts("  This is the optimization target!")
  end
end
