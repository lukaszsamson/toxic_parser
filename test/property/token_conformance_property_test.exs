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
    @tag :property
    @tag timeout: 120_000
    property "generated tokens produce conformant ASTs" do
      check all(
              tokens <- Generator.tokens_gen(max_forms: 3, depth: 3),
              max_runs: 5_000,
              max_shrinking_steps: 50
            ) do
        code = Toxic.to_string(tokens)
        # IO.puts(code)
        run_comparison(code)
      end
    end
  end

  describe "token conformance - shallow" do
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
    @tag :property
    @tag timeout: 120_000
    property "deep generated tokens produce conformant ASTs" do
      check all(
              tokens <- Generator.tokens_gen(max_forms: 4, depth: 5),
              max_runs: 5_000,
              max_shrinking_steps: 50
            ) do
        code = Toxic.to_string(tokens)
        run_comparison(code)
      end
    end
  end

  describe "token conformance - random flags" do
    @tag :property
    @tag timeout: 120_000
    property "generated tokens with random feature flags produce conformant ASTs" do
      check all(
              flags <- Generator.flags_gen(),
              tokens <- Generator.tokens_gen(flags: flags, max_forms: 3, depth: 3),
              max_runs: 5_000,
              max_shrinking_steps: 50
            ) do
        code = Toxic.to_string(tokens)
        run_comparison(code)
      end
    end
  end

  describe "token conformance - minimal features" do
    @tag :property
    @tag timeout: 120_000
    property "generated tokens with all optional features disabled produce conformant ASTs" do
      flags =
        Generator.default_flags()
        |> Map.merge(%{
          enable_lists: false,
          enable_maps: false,
          enable_do_blocks: false,
          enable_tuples: false,
          enable_bitstrings: false,
          enable_fn: false,
          enable_parens_stab: false,
          enable_binary_op: false,
          enable_unary_op: false,
          enable_kw: false,
          enable_parens_calls: false,
          enable_no_parens_calls: false
        })

      check all(
              tokens <- Generator.tokens_gen(flags: flags, max_forms: 3, depth: 3),
              max_runs: 5_000,
              max_shrinking_steps: 50
            ) do
        code = Toxic.to_string(tokens)
        run_comparison(code)
      end
    end
  end

  describe "token conformance - lists only" do
    @tag :property
    @tag timeout: 120_000
    property "generated tokens with only lists enabled produce conformant ASTs" do
      flags =
        Generator.default_flags()
        |> Map.merge(%{
          enable_lists: true,
          enable_maps: false,
          enable_do_blocks: false,
          enable_tuples: false,
          enable_bitstrings: false,
          enable_fn: false,
          enable_parens_stab: false,
          enable_binary_op: false,
          enable_unary_op: false,
          enable_kw: true,
          enable_parens_calls: false,
          enable_no_parens_calls: false
        })

      check all(
              tokens <- Generator.tokens_gen(flags: flags, max_forms: 3, depth: 3),
              max_runs: 5_000,
              max_shrinking_steps: 50
            ) do
        code = Toxic.to_string(tokens)
        run_comparison(code)
      end
    end
  end

  describe "token conformance - calls only" do
    @tag :property
    @tag timeout: 120_000
    property "generated tokens with only calls enabled produce conformant ASTs" do
      flags =
        Generator.default_flags()
        |> Map.merge(%{
          enable_lists: false,
          enable_maps: false,
          enable_do_blocks: false,
          enable_tuples: false,
          enable_bitstrings: false,
          enable_fn: false,
          enable_parens_stab: false,
          enable_binary_op: false,
          enable_unary_op: false,
          enable_kw: true,
          enable_parens_calls: true,
          enable_no_parens_calls: true
        })

      check all(
              tokens <- Generator.tokens_gen(flags: flags, max_forms: 3, depth: 3),
              max_runs: 5_000,
              max_shrinking_steps: 50
            ) do
        code = Toxic.to_string(tokens)
        run_comparison(code)
      end
    end
  end

  describe "token conformance - operators only" do
    @tag :property
    @tag timeout: 120_000
    property "generated tokens with only operators enabled produce conformant ASTs" do
      flags =
        Generator.default_flags()
        |> Map.merge(%{
          enable_lists: false,
          enable_maps: false,
          enable_do_blocks: false,
          enable_tuples: false,
          enable_bitstrings: false,
          enable_fn: false,
          enable_parens_stab: false,
          enable_binary_op: true,
          enable_unary_op: true,
          enable_kw: false,
          enable_parens_calls: false,
          enable_no_parens_calls: false
        })

      check all(
              tokens <- Generator.tokens_gen(flags: flags, max_forms: 3, depth: 3),
              max_runs: 5_000,
              max_shrinking_steps: 50
            ) do
        code = Toxic.to_string(tokens)
        run_comparison(code)
      end
    end
  end

  describe "token conformance - fn and stabs" do
    @tag :property
    @tag timeout: 120_000
    property "generated tokens with fn and stabs enabled produce conformant ASTs" do
      flags =
        Generator.default_flags()
        |> Map.merge(%{
          enable_lists: false,
          enable_maps: false,
          enable_do_blocks: false,
          enable_tuples: true,
          enable_bitstrings: false,
          enable_fn: true,
          enable_parens_stab: true,
          enable_binary_op: false,
          enable_unary_op: false,
          enable_kw: false,
          enable_parens_calls: false,
          enable_no_parens_calls: false
        })

      check all(
              tokens <- Generator.tokens_gen(flags: flags, max_forms: 3, depth: 3),
              max_runs: 5_000,
              max_shrinking_steps: 50
            ) do
        code = Toxic.to_string(tokens)
        run_comparison(code)
      end
    end
  end

  describe "token conformance - do blocks" do
    @tag :property
    @tag timeout: 120_000
    property "generated tokens with do blocks enabled produce conformant ASTs" do
      flags =
        Generator.default_flags()
        |> Map.merge(%{
          enable_lists: false,
          enable_maps: false,
          enable_do_blocks: true,
          enable_tuples: false,
          enable_bitstrings: false,
          enable_fn: false,
          enable_parens_stab: false,
          enable_binary_op: false,
          enable_unary_op: false,
          enable_kw: true,
          enable_parens_calls: true,
          enable_no_parens_calls: true
        })

      check all(
              tokens <- Generator.tokens_gen(flags: flags, max_forms: 3, depth: 3),
              max_runs: 5_000,
              max_shrinking_steps: 50
            ) do
        code = Toxic.to_string(tokens)
        run_comparison(code)
      end
    end
  end

  describe "token conformance - maps and structs" do
    @tag :property
    @tag timeout: 120_000
    property "generated tokens with maps enabled produce conformant ASTs" do
      flags =
        Generator.default_flags()
        |> Map.merge(%{
          enable_lists: false,
          enable_maps: true,
          enable_do_blocks: false,
          enable_tuples: false,
          enable_bitstrings: false,
          enable_fn: false,
          enable_parens_stab: false,
          enable_binary_op: false,
          enable_unary_op: false,
          enable_kw: true,
          enable_parens_calls: false,
          enable_no_parens_calls: false
        })

      check all(
              tokens <- Generator.tokens_gen(flags: flags, max_forms: 3, depth: 3),
              max_runs: 5_000,
              max_shrinking_steps: 50
            ) do
        code = Toxic.to_string(tokens)
        run_comparison(code)
      end
    end
  end

  describe "token conformance - bitstrings" do
    @tag :property
    @tag timeout: 120_000
    property "generated tokens with bitstrings enabled produce conformant ASTs" do
      flags =
        Generator.default_flags()
        |> Map.merge(%{
          enable_lists: false,
          enable_maps: false,
          enable_do_blocks: false,
          enable_tuples: false,
          enable_bitstrings: true,
          enable_fn: false,
          enable_parens_stab: false,
          enable_binary_op: false,
          enable_unary_op: false,
          enable_kw: true,
          enable_parens_calls: false,
          enable_no_parens_calls: false
        })

      check all(
              tokens <- Generator.tokens_gen(flags: flags, max_forms: 3, depth: 3),
              max_runs: 5_000,
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
    result = try do
        Code.string_to_quoted(code, @oracle_opts)
    rescue
      _ -> {:error, :oracle_crash}
    end

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
