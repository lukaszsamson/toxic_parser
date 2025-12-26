defmodule ToxicParser.Identifiers do
  @moduledoc """
  Identifier classification utilities.

  Phase 5 requires distinguishing identifier shapes (`identifier`, `paren_identifier`,
  `bracket_identifier`, `do_identifier`, `op_identifier`) from Toxic tokens.
  This module provides lightweight helpers to recognize these token kinds.
  """

  @spec classify(atom()) ::
          :identifier
          | :paren_identifier
          | :bracket_identifier
          | :do_identifier
          | :op_identifier
          | :alias
          | :other
  def classify(kind) do
    case kind do
      :identifier -> :identifier
      :paren_identifier -> :paren_identifier
      :bracket_identifier -> :bracket_identifier
      :do_identifier -> :do_identifier
      :op_identifier -> :op_identifier
      :alias -> :alias
      :quoted_paren_identifier_end -> :paren_identifier
      :quoted_bracket_identifier_end -> :bracket_identifier
      :quoted_do_identifier_end -> :do_identifier
      :quoted_op_identifier_end -> :op_identifier
      _ -> :other
    end
  end
end
