defmodule ToxicParser.GrammarTokenPropertyTest do
  @moduledoc """
  Property tests for token-driven grammar trees.

  These tests generate random grammar trees, compile them to Toxic tokens,
  render to source code, and verify they round-trip through both the
  Elixir oracle (Code.string_to_quoted) and Spitfire.
  """
  use ExUnit.Case, async: false
  use ExUnitProperties

  alias ToxicParser.Property.TokenGrammarGenerators, as: Gen
  alias ToxicParser.Property.TokenCompiler

  # Note: emit_warnings is NOT set to false here because we need
  # Code.with_diagnostics to capture deprecation warnings for workarounds
  @oracle_opts [
    columns: true,
    token_metadata: true,
    existing_atoms_only: true
  ]

  setup_all do
    # Touch atom pools to ensure atoms exist
    touch_atom_pools()
    :ok
  end

  defp touch_atom_pools do
    _ = Gen.atom_pool()
    _ = Gen.identifier_pool()

    Enum.each(Gen.alias_pool(), fn alias_atom ->
      _ = Module.concat([alias_atom])
    end)

    :ok
  end

  describe "grammar trees" do
    @tag :property
    @tag timeout: 120_000
    property "grammar trees round-trip through Toxic" do
      check all(
              tree <- Gen.grammar(max_depth: 2, max_forms: 2),
              max_runs: 5000,
              max_shrinks: 50
            ) do
        tokens = TokenCompiler.to_tokens(tree)
        code = Toxic.ToString.to_string(tokens)
        # IO.puts(">>>>>\n" <> code <> "\n<<<<<")
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
