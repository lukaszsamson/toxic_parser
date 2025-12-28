defmodule ToxicParser.NoParens do
  @moduledoc false

  @no_parens_arg_kinds [
    :int,
    :flt,
    :char,
    :atom,
    :string,
    :identifier,
    :op_identifier,
    :do_identifier,
    :paren_identifier,
    :bracket_identifier,
    :alias,
    :bin_string_start,
    :list_string_start,
    :bin_heredoc_start,
    :list_heredoc_start,
    :sigil_start,
    :atom_safe_start,
    :atom_unsafe_start,
    true,
    false,
    nil,
    :"{",
    :"[",
    :"(",
    :"<<",
    :unary_op,
    :at_op,
    :capture_int,
    :capture_op,
    :dual_op,
    :%,
    :%{},
    :fn,
    :ellipsis_op
  ]

  defguard can_start_no_parens_arg?(token) when elem(token, 0) in @no_parens_arg_kinds
end
