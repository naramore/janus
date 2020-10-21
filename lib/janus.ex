defmodule Janus do
  @moduledoc false
  use Boundary, deps: [Digraph, EQL, Interceptor, Rails], exports: []

  @type env :: map
  @type attr :: EQL.AST.Prop.expr()
  @type shape_descriptor(x) :: %{optional(x) => shape_descriptor(x)}
  @type shape_descriptor :: shape_descriptor(attr)
end
