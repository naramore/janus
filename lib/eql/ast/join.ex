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
