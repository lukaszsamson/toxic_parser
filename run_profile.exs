Mix.ensure_application!(:tools)

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

# Baseline measurement
{baseline_time, stats} = :timer.tc(fn ->
  Enum.reduce(files, {0, 0}, fn file, {nodes, bytes} ->
    content = File.read!(file)
    case ToxicParser.parse_string(content, mode: :strict, token_metadata: true) do
      {:ok, result} ->
        node_count = :erlang.term_to_binary(result.ast) |> byte_size()
        {nodes + node_count, bytes + byte_size(content)}
      _ -> {nodes, bytes + byte_size(content)}
    end
  end)
end)

{total_nodes, total_bytes} = stats
IO.puts("Baseline: #{Float.round(baseline_time / 1000, 2)} ms")
IO.puts("Bytes: #{total_bytes}, AST size: #{total_nodes}")
IO.puts("Throughput: #{Float.round(total_bytes / (baseline_time / 1000) / 1024, 2)} KB/sec")
IO.puts("")
IO.puts("Running tprof...")

workload = fn ->
  Enum.each(files, fn file ->
    content = File.read!(file)
    ToxicParser.parse_string(content, mode: :strict, token_metadata: true)
  end)
end

Mix.Tasks.Profile.Tprof.profile(workload, [type: :time])
