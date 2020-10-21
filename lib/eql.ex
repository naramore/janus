defmodule EQL do
  @moduledoc false

  use Boundary, deps: [], exports: []
  alias EQL.AST

  @type query :: AST.Query.expr()

  @spec to_ast(term) :: AST.t() | nil
  def to_ast(term) do
    AST.Query.to_ast(term)
  end

  @spec to_expr(AST.t()) :: term
  def to_expr(ast) do
    AST.to_expr(ast)
  end

  @spec get_key(AST.t()) :: AST.Prop.expr() | nil
  def get_key(ast) do
    AST.get_key(ast)
  end

  defdelegate query(children), to: AST.Query, as: :new

  defdelegate prop(module, key), to: AST.Prop, as: :new

  defdelegate ident(prop, value), to: AST.Ident, as: :new

  defdelegate join(prop, query), to: AST.Join, as: :new

  defdelegate union(children), to: AST.Union, as: :new

  defdelegate union_entry(key, query), to: AST.Union.Entry, as: :new

  defdelegate params(expr, params), to: AST.Params, as: :new

  defdelegate mutation(module, fun, args), to: AST.Mutation, as: :new
end

defprotocol EQL.AST do
  @moduledoc false

  @spec to_expr(t) :: term
  def to_expr(ast)

  @spec get_key(t) :: EQL.AST.Prop.expr() | nil
  def get_key(ast)
end

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

defmodule EQL.AST.Join do
  @moduledoc false

  @behaviour EQL.Expression

  alias EQL.Expression
  alias EQL.AST.{Ident, Mutation, Params, Prop, Query, Union}

  defstruct key: nil,
            query: nil

  @type t(p, q) :: %__MODULE__{
          key: p,
          query: q
        }
  @type t ::
          t(
            Prop.t() | Ident.t() | Params.t(Prop.t() | Ident.t()),
            Query.t() | Union.t() | recursion
          )

  @type expr :: %{
          required(prop_or_ident | Params.expr(prop_or_ident)) =>
            Query.expr() | Union.expr() | recursion
        }
  @type mutation_join :: t(Mutation.t(), Query.t())
  @type prop_or_ident :: Prop.expr() | Ident.expr()
  @type recursion :: non_neg_integer | :infinity

  @spec new(
          Prop.t() | Ident.t() | Params.t(Prop.t() | Ident.t()),
          Query.t() | Union.t() | recursion
        ) :: t
  def new(key, query) do
    %__MODULE__{
      key: key,
      query: query
    }
  end

  defguard is_join(x) when is_map(x) and map_size(x) == 1

  @impl Expression
  def to_ast(join) when is_join(join) do
    with {key, val} <- extract_key_query(join),
         key when not is_nil(key) <- Expression.to_ast([Prop, Ident, Params], key),
         query when not is_nil(query) <- subquery_to_ast(val) do
      new(key, query)
    else
      _ -> nil
    end
  end

  def to_ast(_), do: nil

  @spec extract_key_query(map) :: {term, term}
  defp extract_key_query(join) do
    [key | _] = Map.keys(join)
    {key, Map.get(join, key)}
  end

  @spec subquery_to_ast(term) :: Query.t() | Union.t() | recursion | nil
  defp subquery_to_ast(:infinity), do: :infinity
  defp subquery_to_ast(subquery) when is_integer(subquery), do: subquery

  defp subquery_to_ast(subquery) do
    Expression.to_ast([Query, Union], subquery)
  end

  defimpl EQL.AST do
    @impl @protocol
    def to_expr(join) do
      %{@protocol.to_expr(join.key) => @protocol.to_expr(join.query)}
    end

    @impl @protocol
    def get_key(join) do
      @protocol.to_expr(join.key)
    end
  end
end

defmodule EQL.AST.Union do
  @moduledoc false

  @behaviour EQL.Expression

  alias EQL.Expression
  alias EQL.AST.{Prop, Query, Union.Entry}

  defstruct children: []

  @type t :: %__MODULE__{
          children: [Entry.t()]
        }

  @type expr :: %{required(Prop.expr()) => Query.expr()}

  @spec new([Entry.t()]) :: t
  def new(children \\ []) do
    %__MODULE__{
      children: children
    }
  end

  defguard is_union(x) when is_map(x) and map_size(x) > 0

  @impl Expression
  def to_ast(union) when is_union(union) do
    children =
      Enum.map(union, fn {p, q} -> {Expression.to_ast(Prop, p), Expression.to_ast(Query, q)} end)

    unless Enum.any?(children, fn {p, q} -> is_nil(p) or is_nil(q) end) do
      children
      |> Enum.map(fn {p, q} -> __MODULE__.Entry.new(p, q) end)
      |> new()
    end
  end

  def to_ast(_), do: nil

  defimpl EQL.AST do
    @impl @protocol
    def to_expr(union) do
      union.children
      |> Enum.map(&@protocol.to_expr/1)
      |> Enum.into(%{})
    end

    @impl @protocol
    def get_key(_), do: nil
  end

  defmodule Entry do
    @moduledoc false

    defstruct key: nil,
              query: nil

    @type t :: %__MODULE__{
            key: Prop.t(),
            query: Query.t()
          }

    @spec new(Prop.t(), Query.t()) :: t
    def new(key, query) do
      %__MODULE__{
        key: key,
        query: query
      }
    end

    defimpl EQL.AST do
      @impl @protocol
      def to_expr(entry) do
        {@protocol.to_expr(entry.key), @protocol.to_expr(entry.query)}
      end

      @impl @protocol
      def get_key(entry) do
        @protocol.to_expr(entry.key)
      end
    end
  end
end

defmodule EQL.AST.Params do
  @moduledoc false

  @behaviour EQL.Expression

  alias EQL.AST.{Ident, Join, Prop}
  alias EQL.Expression

  defstruct params: %{},
            expr: nil

  @type t(x) :: %__MODULE__{
          params: params,
          expr: x
        }
  @type t :: t(Prop.t() | Ident.t() | Join.t())

  @type expr(x) :: nonempty_improper_list(x, params)
  @type expr :: expr(Prop.expr() | Ident.expr() | Join.expr())
  @type params :: map

  @spec new(Prop.t() | Ident.t() | Join.t(), params) :: t
  def new(expr, params \\ %{}) do
    %__MODULE__{
      expr: expr,
      params: params
    }
  end

  defguard is_params(x) when is_list(x) and is_map(tl(x))

  @impl EQL.Expression
  def to_ast([expr | params] = x) when is_params(x) do
    case Expression.to_ast([Prop, Ident, Join], expr) do
      nil -> nil
      expr -> new(expr, params)
    end
  end

  def to_ast(_), do: nil

  defimpl EQL.AST do
    @impl @protocol
    def to_expr(params) do
      [@protocol.to_expr(params.expr) | params.params]
    end

    @impl @protocol
    def get_key(params) do
      @protocol.get_key(params.expr)
    end
  end
end

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
