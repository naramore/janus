defmodule EQL.AST.Prop do
  @moduledoc false

  @behaviour EQL.Expression

  defstruct module: nil,
            key: nil

  @type t :: %__MODULE__{
          module: module,
          key: atom
        }

  @type expr :: {module, atom}

  @spec new(module, atom) :: t
  def new(module, key) do
    %__MODULE__{
      module: module,
      key: key
    }
  end

  defguard is_prop(x)
           when is_tuple(x) and tuple_size(x) == 2 and is_atom(elem(x, 0)) and is_atom(elem(x, 1))

  @impl EQL.Expression
  def to_ast({module, key} = expr) when is_prop(expr) do
    new(module, key)
  end

  def to_ast(_), do: nil

  defimpl EQL.AST do
    @impl @protocol
    def to_expr(prop) do
      {prop.module, prop.key}
    end

    @impl @protocol
    def get_key(prop) do
      to_expr(prop)
    end
  end
end
