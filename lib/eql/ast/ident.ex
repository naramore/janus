defmodule EQL.AST.Ident do
  @moduledoc false

  @behaviour EQL.Expression

  alias EQL.{AST.Prop, Expression}
  import EQL.AST.Prop, only: [is_prop: 1]

  defstruct key: nil,
            value: nil

  @type t :: %__MODULE__{
          key: Prop.t(),
          value: term
        }

  @type expr :: {Prop.expr(), term}

  @spec new(Prop.t(), term) :: t
  def new(key, value) do
    %__MODULE__{
      key: key,
      value: value
    }
  end

  defguard is_ident(x) when is_tuple(x) and tuple_size(x) == 2 and is_prop(elem(x, 0))

  @impl Expression
  def to_ast({key, value} = expr) when is_ident(expr) do
    case Expression.to_ast(Prop, key) do
      %Prop{} = prop -> new(prop, value)
      _ -> nil
    end
  end

  def to_ast(_), do: nil

  defimpl EQL.AST do
    @impl @protocol
    def to_expr(ident) do
      {@protocol.to_expr(ident.key), ident.value}
    end

    @impl @protocol
    def get_key(ident) do
      @protocol.to_expr(ident.key)
    end
  end
end
