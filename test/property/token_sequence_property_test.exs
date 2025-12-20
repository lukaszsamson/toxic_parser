defmodule ToxicParser.TokenSequencePropertyTest do
  @moduledoc """
  Property tests that generate programs from a closed list of identifiers,
  keywords, operators, and container tokens glued by separators.
  """
  use ExUnit.Case, async: true
  use ExUnitProperties

  # Options used by both oracle and Toxic for consistency
  @oracle_opts [
    columns: true,
    token_metadata: true,
    emit_warnings: false,
    existing_atoms_only: false
  ]

  # ===========================================================================
  # Token Lists
  # ===========================================================================

  # Identifiers
  @identifiers ~w(a b c x y z foo bar baz qux name value key item)

  # Elixir reserved keywords
  @keywords ~w(
    true false nil
    when and or not in
    fn do end catch rescue after else
    case cond for if unless try receive with
    defmodule def defp defmacro defmacrop defguard defguardp defdelegate defstruct defexception defimpl defprotocol defoverridable
    import require alias use
    quote unquote unquote_splicing
    super __MODULE__ __DIR__ __ENV__ __CALLER__ __STACKTRACE__
  )

  # Operators (binary and unary)
  @operators ~w(
    + - * / ++ -- ** .. ... <> |> <<< >>> <<~ ~>> <~ ~> <~>
    < > <= >= == != === !== =~
    && || !
    & | ^ ~
    = <- -> => ::
    \\
    @
    .
  )

  # Container openers/closers
  @containers ~w( ( \) [ ] { } << >> %{ %  )

  # Special tokens
  @special ~w( : ; , \n | _ ? ! )

  # Atoms and strings
  @atoms [":ok", ":error", ":foo", ":bar", ":_"]

  # Numbers
  @numbers ~w(0 1 2 42 100 0x10 0b101 0o17 1.0 1.5e10)

  # Strings
  @strings ["\"hello\"", "\"\"", "'c'"]

  # All tokens combined
  @all_tokens @identifiers ++
                @keywords ++
                @operators ++
                @containers ++
                @special ++
                @atoms ++
                @numbers ++
                @strings

  # Separators for gluing tokens
  @separators ["", " ", "\n", "  ", " \n", "\n ", " \n "]

  # ===========================================================================
  # Generators
  # ===========================================================================

  defp token_gen do
    StreamData.member_of(@all_tokens)
  end

  defp separator_gen do
    StreamData.member_of(@separators)
  end

  defp token_sequence_gen(opts \\ []) do
    min_length = Keyword.get(opts, :min_length, 1)
    max_length = Keyword.get(opts, :max_length, 10)

    StreamData.bind(
      StreamData.integer(min_length..max_length),
      fn length ->
        StreamData.bind(
          StreamData.list_of(token_gen(), length: length),
          fn tokens ->
            StreamData.bind(
              StreamData.list_of(separator_gen(), length: max(0, length - 1)),
              fn separators ->
                code = interleave(tokens, separators)
                StreamData.constant(code)
              end
            )
          end
        )
      end
    )
  end

  defp interleave([token], []), do: token
  defp interleave([token | rest_tokens], [sep | rest_seps]) do
    token <> sep <> interleave(rest_tokens, rest_seps)
  end
  defp interleave([], []), do: ""

  # ===========================================================================
  # Property Tests
  # ===========================================================================

  describe "token sequence conformance" do
    @tag :property
    @tag timeout: 120_000
    property "generated token sequences produce conformant ASTs" do
      check all(
              code <- token_sequence_gen(min_length: 1, max_length: 12),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - short" do
    @tag :property
    @tag timeout: 120_000
    property "short token sequences produce conformant ASTs" do
      check all(
              code <- token_sequence_gen(min_length: 1, max_length: 5),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - long" do
    @tag :property
    @tag timeout: 120_000
    property "long token sequences produce conformant ASTs" do
      check all(
              code <- token_sequence_gen(min_length: 8, max_length: 20),
              max_runs: 50_000,
              max_shrinking_steps: 50
            ) do
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - identifiers only" do
    @tag :property
    @tag timeout: 120_000
    property "identifier-only sequences produce conformant ASTs" do
      identifier_gen = StreamData.member_of(@identifiers)

      check all(
              tokens <- StreamData.list_of(identifier_gen, min_length: 1, max_length: 8),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - keywords only" do
    @tag :property
    @tag timeout: 120_000
    property "keyword-only sequences produce conformant ASTs" do
      keyword_gen = StreamData.member_of(@keywords)

      check all(
              tokens <- StreamData.list_of(keyword_gen, min_length: 1, max_length: 8),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - operators only" do
    @tag :property
    @tag timeout: 120_000
    property "operator-only sequences produce conformant ASTs" do
      operator_gen = StreamData.member_of(@operators)

      check all(
              tokens <- StreamData.list_of(operator_gen, min_length: 1, max_length: 8),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - containers only" do
    @tag :property
    @tag timeout: 120_000
    property "container-only sequences produce conformant ASTs" do
      container_gen = StreamData.member_of(@containers)

      check all(
              tokens <- StreamData.list_of(container_gen, min_length: 1, max_length: 12),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - identifiers and operators" do
    @tag :property
    @tag timeout: 120_000
    property "identifier and operator sequences produce conformant ASTs" do
      token_gen = StreamData.member_of(@identifiers ++ @operators)

      check all(
              tokens <- StreamData.list_of(token_gen, min_length: 1, max_length: 10),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - identifiers and containers" do
    @tag :property
    @tag timeout: 120_000
    property "identifier and container sequences produce conformant ASTs" do
      token_gen = StreamData.member_of(@identifiers ++ @containers)

      check all(
              tokens <- StreamData.list_of(token_gen, min_length: 1, max_length: 10),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - keywords and containers" do
    @tag :property
    @tag timeout: 120_000
    property "keyword and container sequences produce conformant ASTs" do
      token_gen = StreamData.member_of(@keywords ++ @containers)

      check all(
              tokens <- StreamData.list_of(token_gen, min_length: 1, max_length: 10),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - no containers" do
    @tag :property
    @tag timeout: 120_000
    property "sequences without containers produce conformant ASTs" do
      token_gen = StreamData.member_of(@identifiers ++ @keywords ++ @operators ++ @atoms ++ @numbers)

      check all(
              tokens <- StreamData.list_of(token_gen, min_length: 1, max_length: 10),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  describe "token sequence conformance - basic expressions" do
    @tag :property
    @tag timeout: 120_000
    property "basic expression-like sequences produce conformant ASTs" do
      # More likely to produce valid expressions
      basic_tokens = @identifiers ++ @atoms ++ @numbers ++ ["(", ")", "[", "]", "{", "}", ","]
      token_gen = StreamData.member_of(basic_tokens)

      check all(
              tokens <- StreamData.list_of(token_gen, min_length: 1, max_length: 12),
              separators <- StreamData.list_of(separator_gen(), length: max(0, length(tokens) - 1)),
              max_runs: 100_000,
              max_shrinking_steps: 50
            ) do
        code = interleave(tokens, separators)
        run_comparison(code)
      end
    end
  end

  # ===========================================================================
  # Comparison Helper
  # ===========================================================================

  defp run_comparison(code) do
    result =
      try do
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
            flunk("""
            Parser error for: #{inspect(code)}

            Toxic:
            #{inspect(reason, pretty: true)}
            """)
        end

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
