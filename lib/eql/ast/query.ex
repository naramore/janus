defmodule EQL.AST.Query do
  @moduledoc false

  @behaviour EQL.Expression

  alias EQL.Expression
  alias EQL.AST.{Ident, Join, Mutation, Params, Prop}

  defstruct children: []

  @type t :: %__MODULE__{
          children: [Prop.t() | Join.t() | Ident.t() | Mutation.t() | Params.t() | special]
        }

  @type expr :: [query_expr]
  @type query_expr ::
          Prop.expr()
          | Join.expr()
          | Ident.expr()
          | Mutation.expr()
          | Params.expr()
          | special
  @type special :: :*

  @spec new([Prop.t() | Join.t() | Ident.t() | Mutation.t() | Params.t() | special]) :: t
  def new(children \\ []) do
    %__MODULE__{
      children: children
    }
  end

  defguard is_query(x) when is_list(x) and length(x) >= 0

  @impl Expression
  def to_ast(query) when is_query(query) do
    children =
      Enum.map(query, fn
        :* -> :*
        x -> Expression.to_ast([Prop, Ident, Join, Params, Mutation], x)
      end)

    unless Enum.any?(children, &is_nil/1) do
      new(children)
    end
  end

  def to_ast(_), do: nil

  defimpl EQL.AST do
    @impl @protocol
    def to_expr(query) do
      Enum.map(query.children, fn
        :* -> :*
        x -> @protocol.to_expr(x)
      end)
    end

    @impl @protocol
    def get_key(_), do: nil
  end
end
