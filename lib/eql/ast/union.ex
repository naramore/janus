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

  @spec to_query(t) :: Query.t()
  def to_query(union) do
    union.children
    |> Enum.map(fn e -> e.query.children end)
    |> Enum.concat()
    |> Query.new()
  end

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
