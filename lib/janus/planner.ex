defmodule Janus.Planner do
  @moduledoc false

  alias Janus.{Graph, Plan, Utils}
  require Logger

  @behaviour Janus.Graph
  @behaviour Access

  defstruct available_data: %{},
            resolver_trail: [],
            # change to %{optional(Janus.attr) => Plan.path} ???
            paths: [],
            plan: nil,
            current_attr: nil

  @type t :: %__MODULE__{
          available_data: Janus.shape_descriptor(),
          resolver_trail: Plan.path(),
          paths: [Plan.path()],
          plan: Digraph.t(),
          current_attr: Janus.attr() | nil
        }

  @impl Access
  def fetch(planner, key) do
    Map.fetch(planner, key)
  end

  @impl Access
  def get_and_update(planner, key, fun) do
    Map.get_and_update(planner, key, fun)
  end

  @impl Access
  def pop(planner, key) do
    Map.pop(planner, key)
  end

  # :digraph.in_edges(graph.dg, EQL.get_key(key)) |> (( UNIQUE RESOLVERS )) |> Graph.output(graph, &1, [key | subquery_path]) |> Enum.map(&Map.keys/1)
  # ^ that will be the available_data for each subquery

  # :pre_walk, %Prop{}              -> set current_attr + reset trail
  # {:post_walk, r}, %Prop{}        -> noop
  # :ident, %Ident{}                -> add to (subquery?) available_data
  # :params, %Params{}              -> store params
  # {:recursion, depth}, %Join{}    -> ???
  # {:pre_subquery, rid}, %Join{}   -> ???
  # {:post_subquery, rid}, %Join{}  -> ???

  # NOTE: need to update this struct to better support subqueries...

  @impl Graph
  def ast_walker(_type, _ast, _graph, _acc) do
    {:error, :not_implemented}
  end

  @impl Graph
  def attr_walker(:cyclic, _edge, graph, planner) do
    {:unreachable, graph, planner}
  end

  def attr_walker(:pre_walk, {_, i, o, %{id: nil}}, graph, planner) when is_list(o) do
    attr_walker(:pre_walk, {nil, i, o, %{id: {:and, o, i}}}, graph, planner)
  end

  def attr_walker(:pre_walk, {_, i, _, %{id: id}}, graph, planner) do
    if available?(planner, i) do
      {:found, graph, add_path(planner, id)}
    else
      {:reachable, graph, add_trail(planner, id)}
    end
  end

  def attr_walker({:post_walk, r}, _edge, graph, planner) do
    {r, graph, tail_trail(planner)}
  end

  @spec new([Janus.attr()]) :: t
  def new(attrs) do
    %__MODULE__{
      available_data: Utils.to_shape_descriptor(attrs),
      plan: Plan.init_graph(Digraph.new())
    }
  end

  @spec available?(t, Graph.node_id()) :: boolean
  defp available?(_planner, []), do: true

  defp available?(planner, input) when is_list(input) do
    Enum.all?(input, &(&1 in Map.keys(planner.available_data)))
  end

  defp available?(planner, input) do
    input in Map.keys(planner.available_data)
  end

  @spec add_trail(t, Plan.landmark()) :: t
  defp add_trail(planner, id), do: Map.update!(planner, :resolver_trail, &[id | &1])

  @spec tail_trail(t) :: t
  defp tail_trail(planner), do: Map.update!(planner, :resolver_trail, &tl/1)

  @spec get_trail(t) :: Plan.path()
  defp get_trail(%{resolver_trail: trail}), do: trail

  @spec add_path(t, Plan.landmark()) :: t
  defp add_path(planner, id) do
    planner
    |> Map.update!(:paths, &[[id | get_trail(planner)] | &1])
    |> update_plan()
    |> case do
      {:ok, planner} ->
        planner

      {:error, reason} ->
        raise %RuntimeError{message: "#{inspect(reason)}"}
    end
  end

  @spec update_plan(t) :: {:ok, t} | {:error, reason :: term}
  defp update_plan(%{paths: [path | _], plan: graph, current_attr: attr} = planner) do
    with {:ok, {origin_id, graph}} <- Plan.create_attr_root_node(graph, attr),
         path = Plan.pre_process_path(path),
         {:ok, graph} <- Plan.follow(graph, origin_id, path, attr) do
      {:ok, %{planner | plan: graph}}
    else
      {:error, reason} -> {:error, reason}
    end
  end
end
