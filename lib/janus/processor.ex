defmodule Janus.Processor do
  @moduledoc false

  @type entity :: Janus.response_form()

  @callback process_ast(Janus.env(), EQL.AST.Query.t()) ::
              {:ok, entity} | {:error, reason :: term}

  @spec process_expr(module, Janus.env(), EQL.query()) :: {:ok, entity} | {:error, reason :: term}
  def process_expr(module, env, query) do
    case EQL.to_ast(query) do
      nil -> {:error, :invalid_ast}
      query -> module.process_ast(env, query)
    end
  end

  @doc false
  defmacro __using__(_opts) do
    quote do
      alias Janus.Processor
      @behaviour Processor

      @spec process_expr(Janus.env(), EQL.query()) ::
              {:ok, Processor.entity()} | {:error, reason :: term}
      def process_expr(env, query) do
        Processor.process_expr(__MODULE__, env, query)
      end
    end
  end
end

defmodule Janus.Processor.Serial do
  @moduledoc false

  use Janus.Processor

  @impl Processor
  def process_ast(_env, _ast) do
    {:error, :not_implemented}
  end

  # change env from map -> struct?
  #   graph: Janus.Graph.t
  #   resolvers: %{optional(Resolver.id) => Resolver.t}
  #   planner: Janus.Planner.t
  #   entity: Janus.Processor.entity
  #   context: map

  # graph = Janus.Graph.new(resolvers)
  # planner = Janus.Planner.new(available_data)
  # {:ok, {graph, planner}} = Janus.Planner.walk_ast(graph, query_ast, planner)

  # process_root()
  #   Enum.reduce(root.branches, entity, &process_node/2)

  # process_node(node, env)
  #   resolver -> process_resolver_node(node, env)
  #   and -> process_and_node(node, env)
  #   or -> process_or_node(node, env)
  #   join -> process_join_node(node, env)

  # process_resolver_node(node, env)
  #   if any outputs not in entity -> lookup_resolver -> execute -> merge output w/ entity -> process_next_node()
  #   otherwise -> process_next_node()

  # process_and_node(node, env)
  #   Enum.reduce(node.branches, env, &process_node/2)

  # process_or_node(node, env)
  #   recur until one succeeds (USE WEIGHT BEHAVIOUR HERE!)

  # process_join_node(node, env)
  #   root_end? -> noop
  #   once 1 of each attr of in_neighbours has succeeded -> process_next_node()
  #   otherwise -> noop
end
