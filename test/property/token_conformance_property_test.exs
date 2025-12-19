defmodule ToxicParser.TokenConformancePropertyTest do
  @moduledoc """
  Property tests that generate random token sequences and validate
  that ToxicParser produces ASTs conformant with the oracle parser.
  """
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias ToxicParser.Generator

  # Options used by both oracle and Toxic for consistency
  @oracle_opts [
    columns: true,
    token_metadata: true,
    emit_warnings: false,
    existing_atoms_only: false
  ]

  # ===========================================================================
  # Property Tests
  # ===========================================================================

  describe "token conformance" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "generated tokens produce conformant ASTs" do
      check all(
              tokens <- Generator.tokens_gen(max_forms: 3, depth: 2),
              max_runs: 100,
              max_shrinking_steps: 50
            ) do
        code = Toxic.to_string(tokens)
        IO.puts(code)
        run_comparison(code)
      end
    end
  end

  describe "token conformance - shallow" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "shallow generated tokens produce conformant ASTs" do
      check all(
              tokens <- Generator.tokens_gen(max_forms: 1, depth: 1),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = Toxic.to_string(tokens)
        run_comparison(code)
      end
    end
  end

  describe "token conformance - deep" do
    @tag :skip
    @tag :property
    @tag timeout: 120_000
    property "deep generated tokens produce conformant ASTs" do
      check all(
              tokens <- Generator.tokens_gen(max_forms: 5, depth: 4),
              max_runs: 50_000,
              max_shrinking_steps: 50
            ) do
        code = Toxic.to_string(tokens)
        run_comparison(code)
      end
    end
  end

  # ===========================================================================
  # Comparison Helper
  # ===========================================================================

  defp run_comparison(code) do
    # Use Code.with_diagnostics to capture warnings
    {result, _diagnostics} =
      Code.with_diagnostics(fn ->
        Code.string_to_quoted(code, @oracle_opts)
      end)

    case result do
      {:ok, {:__block__, _, []}} ->
        # Empty block, skip
        :ok

      {:ok, oracle_ast} ->
        # Parse with Toxic using same options as oracle
        # try do
        case toxic_parse(code) do
          {:ok, toxic_ast} ->
            # Normalize and compare ASTs
            oracle_normalized = normalize_ast(oracle_ast)
            toxic_normalized = normalize_ast(toxic_ast)

            assert oracle_normalized == toxic_normalized,
                  """
                  AST mismatch for code: #{inspect(code)}

                  Oracle:
                  #{inspect(oracle_normalized, pretty: true)}

                  Toxic:
                  #{inspect(toxic_normalized, pretty: true)}
                  """
            {:error, reason} ->
              flunk """
                  Parser error for: #{inspect(code)}

                  Toxic:
                  #{inspect(reason, pretty: true)}
                  """
          end
        # rescue
        #   error ->
        #     flunk """
        #           Parser crash for: #{inspect(code)}

        #           Toxic:
        #           #{Exception.format(:error, error, __STACKTRACE__)}
        #           """
        # end

      {:error, _} ->
        # Oracle rejected, skip this sample
        :ok
    end
  end

  defp toxic_parse(code) do
    case ToxicParser.parse_string(code,
           mode: :strict,
           token_metadata: true,
           existing_atoms_only: false
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

  # ===========================================================================
  # AST Normalization
  # ===========================================================================

  @ignored_meta_keys [
    :range
  ]

  defp normalize_ast(ast) do
    ast
    |> Macro.postwalk(fn
      # Empty blocks - strip all metadata (Oracle omits it, Toxic includes it)
      {:__block__, _meta, []} ->
        {:__block__, [], []}

      {tag, meta, args} when is_list(meta) ->
        {tag, Keyword.drop(meta, @ignored_meta_keys), args}

      keyword when is_list(keyword) ->
        if Keyword.keyword?(keyword) do
          Keyword.drop(keyword, @ignored_meta_keys)
        else
          keyword
        end

      node ->
        node
    end)
  end
end
