defmodule EQL.AST.Mutation do
  @moduledoc false

  @behaviour EQL.Expression

  alias EQL.{AST.Query, Expression}

  defstruct module: nil,
            fun: nil,
            args: []

  @type t :: %__MODULE__{
          module: module,
          fun: atom,
          args: [term]
        }

  @type expr :: call | %{required(call) => Query.expr()}
  @type call :: {module, atom, args :: [term]}

  @spec new(module, atom, [term]) :: t
  def new(module, fun, args \\ []) do
    %__MODULE__{
      module: module,
      fun: fun,
      args: args
    }
  end

  defguard is_mutation(x)
           when is_tuple(x) and tuple_size(x) == 3 and
                  is_atom(elem(x, 0)) and is_atom(elem(x, 1)) and is_list(elem(x, 2))

  @impl Expression
  def to_ast({module, fun, args} = mut) when is_mutation(mut) do
    new(module, fun, args)
  end

  def to_ast(_), do: nil

  defimpl EQL.AST do
    @impl @protocol
    def to_expr(mut) do
      {mut.module, mut.fun, mut.args}
    end

    @impl @protocol
    def get_key(mut) do
      {mut.module, mut.fun}
    end
  end
end
