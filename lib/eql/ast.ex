defprotocol EQL.AST do
  @moduledoc false

  @spec to_expr(t) :: term
  def to_expr(ast)

  @spec get_key(t) :: EQL.AST.Prop.expr() | nil
  def get_key(ast)
end
