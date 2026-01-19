defmodule ToxicParser.Grammar.ErrorHelpers do
  @moduledoc false

  alias ToxicParser.{Builder, Cursor, Error, State}

  def error_anchor(meta, %State{} = state, cursor) do
    {line, column} =
      case meta do
        {{line, column}, _, _} -> {line, column}
        meta when is_list(meta) -> {Keyword.get(meta, :line), Keyword.get(meta, :column)}
        {line, column} when is_integer(line) -> {line, column}
        _ -> {nil, nil}
      end

    synthetic? = synthetic_meta?(meta, state)

    {line, column} =
      case {line, column} do
        {nil, nil} -> Cursor.position(cursor)
        {line, column} -> {line || 1, column || 1}
      end

    {line, column, synthetic?}
  end

  def error_anchor_from_reason(reason, %State{} = state, cursor) do
    meta = error_meta_from_reason(reason, cursor)
    {line, column, synthetic?} = error_anchor(meta, state, cursor)
    {line, column, synthetic?}
  end

  def build_error_meta(line, column, synthetic?) do
    [
      line: line,
      column: column,
      toxic: %{synthetic?: synthetic?, anchor: %{line: line, column: column}}
    ]
  end

  def build_parser_diagnostic(reason, %State{} = state, line, column, id, synthetic?) do
    diagnostic =
      Error.from_parser(nil, reason,
        line_index: state.line_index,
        source: state.source,
        position: {{line, column}, {line, column}}
      )
      |> Error.annotate(%{
        id: id,
        anchor: %{kind: :error_node, path: [], note: nil},
        synthetic?: synthetic?,
        lexer_error_code: nil
      })

    %{diagnostic | details: Map.put(diagnostic.details, :source, :grammar)}
  end

  def build_error_node(kind, reason, meta, %State{} = state, cursor, children \\ []) do
    {line, column, synthetic?} = error_anchor(meta, state, cursor)
    {id, state} = State.next_diagnostic_id(state)
    diagnostic = build_parser_diagnostic(reason, state, line, column, id, synthetic?)
    state = %{state | diagnostics: [diagnostic | state.diagnostics]}

    payload =
      Error.error_node_payload(diagnostic,
        kind: kind,
        original: reason,
        children: children,
        synthetic?: synthetic?
      )

    error_meta = build_error_meta(line, column, synthetic?)
    {Builder.Helpers.error(payload, error_meta), state}
  end

  def build_error_node_from_reason(kind, reason, %State{} = state, cursor, children \\ []) do
    {line, column, synthetic?} = error_anchor_from_reason(reason, state, cursor)
    {id, state} = State.next_diagnostic_id(state)
    diagnostic = build_parser_diagnostic(reason, state, line, column, id, synthetic?)
    state = %{state | diagnostics: [diagnostic | state.diagnostics]}

    payload =
      Error.error_node_payload(diagnostic,
        kind: kind,
        original: reason,
        children: children,
        synthetic?: synthetic?
      )

    error_meta = build_error_meta(line, column, synthetic?)
    {Builder.Helpers.error(payload, error_meta), state}
  end

  def error_meta_from_reason(reason, cursor) do
    case reason do
      {meta, _msg, _token} when is_list(meta) ->
        meta

      {meta, _msg} when is_list(meta) ->
        meta

      {{line, column}, _, _} when is_integer(line) and is_integer(column) ->
        [line: line, column: column]

      {line, column} when is_integer(line) and is_integer(column) ->
        [line: line, column: column]

      _ ->
        {line, column} = Cursor.position(cursor)
        [line: line || 1, column: column || 1]
    end
  end

  def synthetic_meta?(meta, %State{} = state) do
    missing? =
      case meta do
        {{_, _}, _, _} ->
          false

        meta when is_list(meta) ->
          Keyword.get(meta, :line) == nil and Keyword.get(meta, :column) == nil

        {line, column} when is_integer(line) and is_integer(column) ->
          false

        _ ->
          true
      end

    missing? or not Keyword.get(state.opts, :token_metadata, true)
  end
end
