defmodule ToxicParser.Grammar.StabsTrace do
  @moduledoc """
  Tracing wrapper for Stabs module to understand call patterns.
  """

  # Counter process to track call statistics
  def start_trace do
    if Process.whereis(:stabs_trace) do
      Process.whereis(:stabs_trace) |> Process.exit(:kill)
      Process.sleep(10)
    end

    pid = spawn(fn -> trace_loop(%{}) end)
    Process.register(pid, :stabs_trace)
  end

  def stop_trace do
    if pid = Process.whereis(:stabs_trace) do
      send(pid, {:get_stats, self()})

      receive do
        {:stats, stats} -> stats
      after
        1000 -> %{}
      end
    else
      %{}
    end
  end

  def trace(key) do
    if pid = Process.whereis(:stabs_trace) do
      send(pid, {:inc, key})
    end
  end

  def trace_result(key, result) do
    outcome =
      case result do
        {:ok, _, _, _} -> :ok
        {:ok, _, _, _, _} -> :ok_with_meta
        {:not_stab, _, _} -> :not_stab
        {:error, _, _, _} -> :error
        {:no_kw, _, _} -> :no_kw
        _ -> :other
      end

    trace({key, outcome})
    result
  end

  defp trace_loop(stats) do
    receive do
      {:inc, key} ->
        trace_loop(Map.update(stats, key, 1, &(&1 + 1)))

      {:get_stats, from} ->
        send(from, {:stats, stats})
        trace_loop(stats)
    end
  end

  def print_stats(stats) do
    stats
    |> Enum.sort_by(fn {_k, v} -> -v end)
    |> Enum.each(fn {k, v} ->
      IO.puts("  #{inspect(k)}: #{v}")
    end)
  end
end
