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
