defmodule Janus.Planner do
  @moduledoc false

  use Janus.Graph
  alias Digraph.Vertex
  alias EQL.AST.{Ident, Join, Params, Prop}
  alias Janus.{Graph, Plan, Utils}
  require Logger

  @behaviour Access

  defstruct available_data: %{},
            resolver_trail: [],
            paths: %{},
            plan: nil,
            current_attr: nil,
            params: nil

  @type t :: %__MODULE__{
          available_data: Janus.shape_descriptor(),
          resolver_trail: [{Plan.landmark(), Graph.node_id(), Janus.attr()}],
          paths: %{optional(Janus.attr()) => [{Plan.landmark(), Graph.node_id(), Janus.attr()}]},
          plan: Digraph.t(),
          current_attr: Janus.attr() | nil,
          params: map | nil
        }

  @type graph :: %{optional(Vertex.t()) => graph}

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

  @impl Graph
  def ast_walker(:pre_walk, %Prop{} = prop, graph, %{plan: plan} = planner) do
    with attr when not is_nil(attr) <- EQL.get_key(prop),
         {attr, [_ | _] = vs} <- {attr, Plan.find_attr_resolvers(plan, attr)},
         {:ok, {_, plan}} <- Plan.create_attr_root_node(plan, attr),
         {:ok, planner} <- backtrack_and_mark(%{planner | plan: plan}, attr, vs) do
      {:skip, graph, planner}
    else
      {:error, reason} ->
        {:error, reason}

      nil ->
        {:error, {:invalid_prop, prop}}

      {attr, []} ->
        if attr in Map.keys(planner.available_data) do
          {:skip, graph, planner}
        else
          {:cont, graph, reset(planner, attr)}
        end
    end
  end

  def ast_walker({:post_walk, :unreachable}, %Prop{} = prop, _graph, _planner) do
    {:error, {:unreachable_prop, prop}}
  end

  def ast_walker({:post_walk, _}, %Prop{}, graph, planner) do
    {:cont, graph, reset(planner)}
  end

  def ast_walker({:pre_walk, :params}, %Params{params: params}, graph, planner) do
    {:cont, graph, %{planner | params: params}}
  end

  def ast_walker({:post_walk, :params}, %Params{}, graph, planner) do
    {:cont, graph, %{planner | params: nil}}
  end

  def ast_walker(:ident, %Ident{}, _graph, _planner) do
    # TODO: add to (subquery?) available_data?
    {:error, :not_implemented}
  end

  def ast_walker({:pre_subquery, _rid}, %Join{}, _graph, _planner) do
    # TODO: implement
    {:error, :not_implemented}
  end

  def ast_walker({:post_subquery, _rid}, %Join{}, _graph, _planner) do
    # TODO: implement
    {:error, :not_implemented}
  end

  def ast_walker({:recursion, _depth}, %Join{}, _graph, _planner) do
    # TODO: implement
    {:error, :not_implemented}
  end

  @impl Graph
  def attr_walker(:cyclic, _edge, graph, planner) do
    {:unreachable, graph, planner}
  end

  def attr_walker(:pre_walk, {_, i, o, %{id: nil}}, graph, planner) when is_list(o) do
    attr_walker(:pre_walk, {nil, i, o, %{id: {:and, o, i}}}, graph, planner)
  end

  def attr_walker(:pre_walk, {_, i, o, %{id: id}}, graph, planner) do
    if available?(planner, i) do
      {:found, graph, add_path(planner, id, i, o)}
    else
      {:reachable, graph, add_trail(planner, id, i, o)}
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

  @spec reset(t, Janus.attr() | nil) :: t
  def reset(plan, attr \\ nil)

  def reset(plan, nil) do
    %{plan | resolver_trail: [], current_attr: nil}
  end

  def reset(plan, attr) do
    %{plan | resolver_trail: [], current_attr: attr}
    |> update_in([:paths, attr], fn
      nil -> []
      x -> x
    end)
  end

  @spec show(Digraph.t(), Vertex.t() | Vertex.id()) :: graph | nil
  def show(graph, vertex_or_id \\ [:root | :start]) do
    case show_impl(graph, vertex_or_id) do
      {v, g} -> %{v => g}
      _ -> nil
    end
  end

  @spec show_impl(Digraph.t(), Vertex.t() | Vertex.id() | nil) :: {Vertex.t(), graph} | nil
  defp show_impl(_graph, nil), do: nil

  defp show_impl(graph, %Vertex{} = vertex) do
    graph
    |> Digraph.out_neighbours(vertex.id)
    |> Enum.map(&show_impl(graph, &1))
    |> Enum.reject(&is_nil/1)
    |> Enum.into(%{})
    |> (&{vertex, &1}).()
  end

  defp show_impl(graph, vertex_id) do
    show_impl(graph, Digraph.vertex(graph, vertex_id))
  end

  @spec available?(t, Graph.node_id()) :: boolean
  defp available?(_planner, []), do: true

  defp available?(planner, input) when is_list(input) do
    Enum.all?(input, &(&1 in Map.keys(planner.available_data)))
  end

  defp available?(planner, input) do
    input in Map.keys(planner.available_data)
  end

  @spec add_trail(t, Plan.landmark(), Graph.node_id(), Janus.attr()) :: t
  defp add_trail(planner, id, input, output) do
    Map.update!(planner, :resolver_trail, &[{id, input, output} | &1])
  end

  @spec tail_trail(t) :: t
  defp tail_trail(planner), do: Map.update!(planner, :resolver_trail, &tl/1)

  @spec add_path(t, Plan.landmark(), Graph.node_id(), Janus.attr(), keyword) :: t
  defp add_path(planner, id, input, output, opts \\ []) do
    planner
    |> update_in(
      [:paths, planner.current_attr],
      &[[{id, input, output} | planner.resolver_trail] | &1]
    )
    |> update_plan(update_opts(planner, opts))
    |> case do
      {:ok, planner} ->
        planner

      {:error, reason} ->
        raise %RuntimeError{message: "#{inspect(reason)}"}
    end
  end

  @spec update_opts(t, keyword) :: keyword
  defp update_opts(%{params: nil}, opts), do: opts
  defp update_opts(%{params: params}, opts), do: Keyword.put(opts, :params, params)

  @spec update_plan(t, keyword) :: {:ok, t} | {:error, reason :: term}
  defp update_plan(%{plan: plan, current_attr: attr} = planner, opts) do
    with [path | _] <- get_in(planner, [:paths, attr]),
         {:ok, {origin_id, plan}} <- Plan.create_attr_root_node(plan, attr),
         path = pre_process_path(path),
         {:ok, plan} <- Plan.follow(plan, origin_id, path, attr, opts) do
      {:ok, %{planner | plan: plan}}
    else
      {:error, reason} -> {:error, reason}
      _ -> {:error, :unknown}
    end
  end

  # NOTE: this 'path' is getting so complicated as is, convert to a struct?
  @spec pre_process_path([{Plan.landmark(), Graph.node_id(), Janus.attr()}]) :: [
          {Plan.landmark(), Graph.node_id(), Janus.attr(), Plan.scope()}
        ]
  defp pre_process_path(path) do
    {top_ands, new_path} =
      path
      |> :lists.reverse()
      |> Enum.reduce({[], []}, fn
        {{:and, bs, b}, _, _}, {t, []} ->
          {[{{:and, bs, b}, [], [], [{:and, bs}]} | t], [{{:join, bs}, bs, [], [b, {:and, bs}]}]}

        {{:and, bs, b}, _, _}, {t, [{_, _, _, s} | _] = p} ->
          {[{{:and, bs, b}, [], [], [{:and, bs} | s]} | t],
           [{{:join, bs}, bs, [], [b, {:and, bs} | s]} | p]}

        {id, i, o}, {t, []} ->
          {t, [{id, i, o, []}]}

        {id, i, o}, {t, [{_, _, _, s} | _] = p} ->
          {t, [{id, i, o, s} | p]}
      end)

    top_ands
    |> :lists.reverse(new_path)
    |> Enum.map(fn
      {{:join, _} = j, i, o, [_ | s]} -> {j, i, o, s}
      x -> x
    end)
  end

  @spec backtrack_and_mark(t, Janus.attr(), [Vertex.t()]) :: {:ok, t} | {:error, reason :: term}
  defp backtrack_and_mark(planner, attr, vertices) do
    case backtrack(planner, attr, vertices) do
      {:error, reason} ->
        {:error, reason}

      {:ok, planner} ->
        Enum.reduce(vertices, {:ok, planner}, &backtrack_and_marker(&1, &2, attr))
    end
  end

  @spec backtrack_and_marker(Vertex.t(), acc, Janus.attr()) :: acc
        when acc: {:ok, t} | {:error, reason :: term}
  defp backtrack_and_marker(vertex, planner_result, attr)
  defp backtrack_and_marker(_, {:error, reason}, _), do: {:error, reason}

  defp backtrack_and_marker(vertex, {:ok, planner}, attr) do
    planner
    |> update_output(vertex.id, attr)
    |> connect_to_root_end(vertex.id)
  end

  @spec update_output(t, Vertex.id(), Janus.attr()) :: t
  defp update_output(planner, vertex_id, attr) do
    update_in(
      planner,
      [:plan, :vertices, vertex_id, :label, :output],
      &Utils.deep_merge(&1, %{attr => %{}})
    )
  end

  @spec connect_to_root_end(t, Vertex.id()) :: {:ok, t} | {:error, reason :: term}
  defp connect_to_root_end(planner, vertex_id) do
    case Plan.connect(planner.plan, vertex_id, Plan.root_end()) do
      {:error, reason} -> {:error, reason}
      {:ok, {_, plan}} -> {:ok, %{planner | plan: plan}}
    end
  end

  @spec backtrack(t, Janus.attr(), [Vertex.t()]) :: {:ok, t} | {:error, reason :: term}
  defp backtrack(planner, attr, vertices) do
    Enum.reduce(vertices, {:ok, planner}, &backtracker(&1, &2, attr))
  end

  @spec backtracker(Vertex.t(), acc, Janus.attr()) :: acc
        when acc: {:ok, t} | {:error, reason :: term}
  defp backtracker(vertex, planner_result, attr)
  defp backtracker(_, {:error, reason}, _), do: {:error, reason}

  defp backtracker(vertex, {:ok, planner}, attr) do
    planner = update_attrs(planner, vertex.id, attr)

    planner.plan
    |> Digraph.in_neighbours(vertex.id)
    |> Enum.filter(&match?(%{id: [:"$v" | _]}, &1))
    |> case do
      [] ->
        case Plan.connect(planner.plan, attr, vertex.id) do
          {:ok, {_, plan}} -> {:ok, %{planner | plan: plan}}
          {:error, reason} -> {:error, reason}
        end

      next ->
        backtrack(planner, attr, next)
    end
  end

  @spec update_attrs(t, Vertex.id(), Janus.attr()) :: t
  defp update_attrs(planner, vertex_id, attr) do
    update_in(planner, [:plan, :vertices, vertex_id, :label, :attrs], fn
      nil ->
        [attr]

      attrs ->
        if attr in attrs do
          attrs
        else
          [attr | attrs]
        end
    end)
  end
end
