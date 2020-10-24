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

  @spec backtrack_and_mark(t, Janus.attr(), [Vertex.t()]) :: {:ok, t} | {:error, reason :: term}
  def backtrack_and_mark(planner, attr, vertices) do
    case backtrack(planner, attr, vertices) do
      {:error, reason} ->
        {:error, reason}

      {:ok, planner} ->
        Enum.reduce(vertices, {:ok, planner}, fn
          _, {:error, reason} ->
            {:error, reason}

          v, {:ok, p} ->
            update_in(
              p,
              [:plan, :vertices, v.id, :label, :output],
              &Utils.deep_merge(&1, %{attr => %{}})
            )
            |> (&Plan.connect(&1.plan, v.id, [:root | :end])).()
            # credo:disable-for-next-line Credo.Check.Refactor.Nesting
            |> case do
              {:error, reason} -> {:error, reason}
              {:ok, {_, plan}} -> {:ok, %{planner | plan: plan}}
            end
        end)
    end
  end

  @spec backtrack(t, Janus.attr(), [Vertex.t()]) :: {:ok, t} | {:error, reason :: term}
  def backtrack(planner, attr, vertices) do
    Enum.reduce(vertices, {:ok, planner}, fn
      _, {:error, reason} ->
        {:error, reason}

      v, {:ok, p} ->
        planner =
          update_in(p, [:plan, :vertices, v.id, :label, :attrs], fn
            nil ->
              [attr]

            attrs ->
              if attr in attrs do
                attrs
              else
                [attr | attrs]
              end
          end)

        planner.plan
        |> Digraph.in_neighbours(v.id)
        |> Enum.filter(&match?(%{id: [:"$v" | _]}, &1))
        |> case do
          [] ->
            # credo:disable-for-next-line Credo.Check.Refactor.Nesting
            case Plan.connect(planner.plan, attr, v.id) do
              {:ok, {_, plan}} -> {:ok, %{planner | plan: plan}}
              {:error, reason} -> {:error, reason}
            end

          next ->
            backtrack(planner, attr, next)
        end
    end)
  end

  @impl Graph
  def ast_walker(:pre_walk, %Prop{} = prop, graph, planner) do
    # TODO: root_start and root_end need their attrs updated
    # TODO: fix this to look for available attrs as well and NOT backtrack in those situations
    # TODO: refactor -> with
    case EQL.get_key(prop) do
      nil ->
        {:error, {:invalid_prop, prop}}

      attr ->
        case Plan.find_attr_resolvers(planner.plan, attr) do
          [] ->
            {:cont, graph, reset(planner, attr)}

          vertices ->
            # credo:disable-for-next-line Credo.Check.Refactor.Nesting
            case Plan.create_attr_root_node(planner.plan, attr) do
              {:error, reason} ->
                {:error, reason}

              {:ok, {_, plan}} ->
                # credo:disable-for-next-line Credo.Check.Refactor.Nesting
                case backtrack_and_mark(%{planner | plan: plan}, attr, vertices) do
                  {:error, reason} -> {:error, reason}
                  {:ok, planner} -> {:skip, graph, planner}
                end
            end
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
end
