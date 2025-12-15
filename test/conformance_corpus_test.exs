defmodule ToxicParser.ConformanceCorpusTest do
  use ExUnit.Case, async: true

  @projects_dir "/Users/lukaszsamson/claude_fun/elixir_oss/projects"
  @ignored_dirs ["_build", "deps", ".git", "tmp", "priv", "rel", "cover", "doc", "logs"]

  @tag timeout: :infinity
  @tag :skip
  test "elixir sources" do
    # files = @regressions
    files = collect_files(nil)

    for file <- files do
      code = file |> File.read!()
      # lines = String.split(source, "\n")
      # assert toxic_parse(code) == s2q(code)
      # IO.puts("Parsing: #{file}")
      res = toxic_parse(code) == s2q(code)

      if not res do
        IO.puts("Failed: #{file}")
      end

      # assert res
    end
  end

  @selected [
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/livebook/lib/livebook/runtime/evaluator.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/phoenix/installer/test/phx_new_ecto_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/phoenix/installer/test/phx_new_web_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/phoenix/lib/mix/tasks/phx.digest.clean.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/phoenix/lib/mix/tasks/phx.digest.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/phoenix/test/mix/tasks/phx_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ecto/integration_test/cases/joins.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ecto/lib/mix/tasks/ecto.gen.repo.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ecto/test/ecto/query/builder/dynamic_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ecto/test/ecto/query/builder/select_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ecto/test/ecto/query/inspect_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ecto/test/ecto/query_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ecto/test/mix/tasks/ecto.create_drop_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ecto/test/mix/tasks/ecto_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/oban/test/oban/backoff_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/oban/test/oban/cron/expression_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/oban/test/oban/cron_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/oban/test/oban/worker_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/makeup/test/html_formatter_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/makeup/test/lexer_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/jason/test/property_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ash/lib/ash/action_input.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ash/lib/ash/actions/action.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ash/lib/ash/actions/aggregate.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ash/lib/ash/actions/create/create.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ash/lib/ash/actions/destroy/destroy.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ash/lib/ash/actions/read/calculations.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ash/lib/ash/actions/read/read.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ash/lib/ash/actions/update/update.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ash/lib/ash/changeset/changeset.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ash/lib/ash/notifier/notifier.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/ash/lib/ash/query/query.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/absinthe/test/absinthe/schema/notation/experimental/macro_extensions_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/absinthe/test/absinthe/type/built_ins/scalars_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/credo/lib/credo/check/warning/unused_operation.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/credo/test/credo/priority_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/timex/test/comparable_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/timex/test/timex_test.exs",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/phoenix_live_view/lib/phoenix_component.ex",
    "/Users/lukaszsamson/claude_fun/elixir_oss/projects/phoenix_live_view/lib/phoenix_live_view/engine.ex"
  ]

  test "selected files" do
    files = @selected

    for file <- files do
      code = file |> File.read!()
      # lines = String.split(source, "\n")
      # assert toxic_parse(code) == s2q(code)
      # IO.puts("Parsing: #{file}")
      res = toxic_parse(code) == s2q(code)

      if not res do
        IO.puts("Failed: #{file}")
      end

      assert res
    end
  end

  defp collect_files(limit) do
    projects =
      case File.ls(@projects_dir) do
        {:ok, dirs} ->
          dirs
          |> Enum.filter(&File.dir?(Path.join(@projects_dir, &1)))
          |> Enum.map(&Path.join(@projects_dir, &1))

        {:error, _} ->
          IO.puts("Error: projects directory not found")
          System.halt(1)
      end

    files =
      projects
      |> Enum.flat_map(&collect_elixir_files/1)

    if limit, do: Enum.take(files, limit), else: files
  end

  defp collect_elixir_files(root_path) do
    Path.wildcard(Path.join(root_path, "**/*.{ex,exs}"))
    |> Enum.reject(&should_ignore_file?/1)
  end

  defp should_ignore_file?(file_path) do
    path_parts = Path.split(file_path)
    phoenix_templates = String.contains?(file_path, "projects/phoenix/installer/templates")
    phoenix_templates or Enum.any?(@ignored_dirs, &(&1 in path_parts))
  end

  defp s2q(code, opts \\ []) do
    Code.string_to_quoted(
      code,
      Keyword.merge([columns: true, token_metadata: true, emit_warnings: false], opts)
    )
  end

  defp toxic_parse(code, options \\ []) do
    case ToxicParser.parse_string(
           code,
           [mode: :strict, token_metadata: true] |> Keyword.merge(options)
         ) do
      {:ok, result} -> {:ok, result.ast}
      {:error, result} -> {:error, format_error(result)}
    end
  end

  defp format_error(result) do
    case result.diagnostics do
      [%{reason: reason} | _] -> reason
      _ -> :unknown_error
    end
  end
end
