files = Path.wildcard("/Users/lukaszsamson/claude_fun/elixir_oss/projects/elixir/**/*.{ex,exs}")
|> Enum.reject(fn f ->
  String.contains?(f, "_build") or
  String.contains?(f, "deps") or
  String.contains?(f, ".git") or
  String.contains?(f, "installer/templates")
end)
|> Enum.take(50)

IO.puts("Files: #{length(files)}")

# Warmup
hd(files) |> File.read!() |> ToxicParser.parse_string(mode: :strict, token_metadata: true)

# Run 5 measurements
times = for run <- 1..5 do
  {time, _} = :timer.tc(fn ->
    Enum.each(files, fn file ->
      content = File.read!(file)
      ToxicParser.parse_string(content, mode: :strict, token_metadata: true)
    end)
  end)
  time_ms = time / 1000
  IO.puts("Run #{run}: #{Float.round(time_ms, 2)} ms")
  time_ms
end

avg = Enum.sum(times) / length(times)
min_time = Enum.min(times)
max_time = Enum.max(times)

IO.puts("")
IO.puts("Average: #{Float.round(avg, 2)} ms")
IO.puts("Min: #{Float.round(min_time, 2)} ms")
IO.puts("Max: #{Float.round(max_time, 2)} ms")
