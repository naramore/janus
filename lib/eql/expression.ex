defmodule EQL.Expression do
  @moduledoc false

  alias EQL.AST

  @callback to_ast(term) :: AST.t() | nil

  @spec to_ast([module] | module, term) :: AST.t() | nil
  def to_ast([], _), do: nil

  def to_ast([module | t], term) do
    case to_ast(module, term) do
      nil -> to_ast(t, term)
      otherwise -> otherwise
    end
  end

  def to_ast(module, term) do
    module.to_ast(term)
  end
end
