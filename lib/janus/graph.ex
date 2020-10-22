defmodule Janus.Graph do
  @moduledoc false

  alias Janus.{Resolver, Utils}
  alias EQL.AST.{Ident, Join, Params, Prop, Query, Union, Union.Entry}

  @behaviour Access

  defstruct unreachable: MapSet.new([]),
            attr_trail: [],
            dg: nil

  @type t :: %__MODULE__{
          unreachable: MapSet.t(node_id),
          attr_trail: [node_id],
          dg: :digraph.graph()
        }

  @type node_id :: Janus.attr() | [Janus.attr()]
  @type vertex :: :digraph.vertex()
  @type edge :: :digraph.edge()
  @type acc :: term
  @type reachability :: :reachable | :unreachable | :found
  @type walker(x) :: (type :: term, x, t, acc -> {reachability, t, acc})
  @type walker :: walker(edge)
  @type depth :: non_neg_integer
  @type label_tuple(id) :: {id, depth, [Janus.attr()], Janus.attr() | nil, leaf? :: boolean}
  @type label(id) :: %{
          required(:id) => id,
          required(:depth) => depth,
          required(:path) => [Janus.attr()],
          required(:union_key) => Janus.attr() | nil,
          required(:leaf?) => boolean
        }
  @type ast_type ::
          :pre_walk
          | :ident
          | :params
          | {:pre_subquery | :post_subquery, Resolver.id()}
          | {:recursion, depth :: timeout}
          | {:post_walk, reachability}
  @type attr_type :: :pre_walk | :cyclic | {:post_walk, reachability}

  @callback ast_walker(ast_type, EQL.AST.t(), t, acc) ::
              {:ok, {t, acc}} | {:error, reason :: term}
  @callback attr_walker(attr_type, edge, t, acc) :: {reachability, t, acc}

  @impl Access
  def fetch(graph, key) do
    Map.fetch(graph, key)
  end

  @impl Access
  def get_and_update(graph, key, fun) do
    Map.get_and_update(graph, key, fun)
  end

  @impl Access
  def pop(graph, key) do
    Map.pop(graph, key)
  end

  @spec new([Resolver.t()]) :: t
  def new(resolvers) do
    %__MODULE__{
      dg: from_resolvers(resolvers)
    }
  end

  @spec new([Resolver.t()], :digraph.graph()) :: t
  def new(resolvers, dg) do
    %__MODULE__{
      dg: from_resolvers(resolvers, dg)
    }
  end

  @spec reset(t) :: t
  def reset(graph) do
    %{graph | attr_trail: [], unreachable: MapSet.new([])}
  end

  @spec ast_walker(module, ast_type, EQL.AST.t(), t, acc) ::
          {:ok, {t, acc}} | {:error, reason :: term}
  def ast_walker(module, type, ast, graph, acc) do
    module.ast_walker(type, ast, graph, acc)
  end

  @spec attr_walker(module, attr_type, edge, t, acc) :: {reachability, t, acc}
  def attr_walker(module, type, edge, graph, acc) do
    module.attr_walker(type, edge, graph, acc)
  end

  @spec output(t, Resolver.id(), [Janus.attr()]) :: Janus.shape_descriptor()
  def output(graph, resolver_id, subquery_path \\ []) do
    # TODO: unions (at the moment, this 'flattens' all unions at their respective logical depth)
    graph.dg
    |> :digraph.edges()
    |> Enum.filter(&match?({_, _, _, %{id: ^resolver_id}}, &1))
    |> Enum.group_by(fn {_, _, _, %{parent: x}} -> x end)
    |> Enum.filter(fn {k, _} -> List.starts_with?(k, subquery_path) end)
    |> Enum.map(fn {k, v} -> {Enum.drop(k, length(subquery_path)), v} end)
    |> Enum.sort_by(fn {k, _} -> length(k) end)
    |> Enum.reduce(%{}, fn
      {[], attrs}, _ ->
        Utils.to_shape_descriptor(attrs)

      {p, attrs}, acc ->
        update_in(acc, :lists.reverse(p), &Map.merge(&1, Utils.to_shape_descriptor(attrs)))
    end)
  end

  @spec walk_ast(t, EQL.AST.t(), acc, module) :: {:ok, {t, acc}} | {:error, reason :: term}
  def walk_ast(graph, %Prop{} = prop, acc, module) do
    with {:ok, {graph, acc}} <- ast_walker(module, :pre_walk, prop, graph, acc),
         {r, g, a} <- walk_attr(graph, EQL.get_key(prop), acc, module) do
      ast_walker(module, {:post_walk, r}, prop, g, a)
    else
      {:error, reason} -> {:error, reason}
    end
  end

  def walk_ast(graph, %Ident{} = ident, acc, module) do
    ast_walker(module, :ident, ident, acc, graph)
  end

  def walk_ast(graph, %Params{expr: expr} = params, acc, module) do
    case ast_walker(module, :params, params, graph, acc) do
      {:ok, {graph, acc}} ->
        walk_ast(graph, expr, acc, module)

      {:error, reason} ->
        {:error, reason}
    end
  end

  def walk_ast(graph, %Join{key: key, query: %Query{} = q} = join, acc, module) do
    case walk_ast(graph, key, acc, module) do
      {:error, reason} ->
        {:error, reason}

      {:ok, {graph, acc}} ->
        graph
        |> subqueries(key)
        |> Enum.reduce({:ok, {graph, acc}}, fn
          _, {:error, reason} ->
            {:error, reason}

          r, {:ok, {g, a}} ->
            with {:ok, {g, a}} <- ast_walker(module, {:pre_subquery, r}, join, g, a),
                 {:ok, {g, a}} <- walk_ast(g, q, a, module) do
              ast_walker(module, {:post_subquery, r}, join, g, a)
            else
              {:error, reason} -> {:error, reason}
            end
        end)
    end
  end

  def walk_ast(graph, %Join{key: key} = join, acc, module)
      when is_integer(key) or key == :infinity do
    case walk_ast(graph, key, acc, module) do
      {:error, reason} ->
        {:error, reason}

      {:ok, {graph, acc}} ->
        ast_walker(module, {:recursion, key}, join, graph, acc)
    end
  end

  def walk_ast(graph, %Union{} = union, acc, module) do
    walk_ast(graph, Union.to_query(union), acc, module)
  end

  def walk_ast(graph, %Query{children: children}, acc, module) do
    Enum.reduce_while(children, {:ok, {graph, acc}}, fn
      _, {:error, reason} -> {:halt, {:error, reason}}
      ast, {:ok, {g, a}} -> {:cont, walk_ast(g, ast, a, module)}
    end)
  end

  def walk_ast(_graph, ast, _acc, _module) do
    {:error, {:invalid_ast, ast}}
  end

  @spec walk_attr(t, node_id, acc, module) :: {reachability, t, acc}
  def walk_attr(graph, node_ids, acc, module) when is_list(node_ids) do
    Enum.reduce_while(node_ids, {:reachable, graph, acc}, fn
      _, {:unreachable, g, a} ->
        {:halt, {:unreachable, g, a}}

      nid, {r, g, a} when r in [:reachable, :found] ->
        fake_edge = {nil, nid, node_ids, %{id: nil}}
        walk_attr_reducer(fake_edge, {:reachable, g, a}, module)
    end)
  end

  def walk_attr(graph, node_id, acc, module) do
    graph.dg
    |> :digraph.in_edges(node_id)
    |> Enum.map(&:digraph.edge(graph.dg, &1))
    |> Enum.filter(&direct_edge?/1)
    |> Enum.reduce_while({:unreachable, graph, acc}, &walk_attr_reducer(&1, &2, module))
  end

  @spec walk_attr_reducer(edge, {reachability, t, acc}, module) ::
          {:cont | :halt, {reachability, t, acc}}
  defp walk_attr_reducer(edge, {r, graph, acc}, module) do
    cond do
      unreachable?(graph, edge) ->
        {:unreachable, graph, acc}

      cyclic?(graph, edge) ->
        {:unreachable, graph, acc}
        |> wrap_walker(edge, module, :cyclic)
        |> update_unreachable(edge, graph, r)

      true ->
        {graph, acc}
        |> continue_walk_attr(edge, module)
        |> update_unreachable(edge, graph, r)
    end
    |> cont_or_halt(edge)
  end

  @spec continue_walk_attr({t, acc}, edge, module) :: {reachability, t, acc}
  defp continue_walk_attr({g, a}, {_, i, o, _} = edge, module) do
    case attr_walker(module, :pre_walk, edge, g, a) do
      {:reachable, graph, acc} ->
        graph
        |> Map.update!(:attr_trail, &[o | &1])
        |> walk_attr(i, acc, module)
        |> wrap_walker(edge, module)

      otherwise ->
        otherwise
    end
  end

  @spec wrap_walker({reachability, t, acc}, edge, module) :: {reachability, t, acc}
  defp wrap_walker(rga, edge, module, type \\ nil)

  defp wrap_walker({reach, graph, acc}, edge, module, nil) do
    attr_walker(module, {:post_walk, reach}, edge, graph, acc)
  end

  defp wrap_walker({_, graph, acc}, edge, module, type) do
    attr_walker(module, type, edge, graph, acc)
  end

  @spec cont_or_halt({reachability, t, acc}, edge) :: {:cont | :halt, {reachability, t, acc}}
  defp cont_or_halt({:unreachable, _, _} = rga, {_, _, o, _}) when is_list(o), do: {:halt, rga}
  defp cont_or_halt(rga, _edge), do: {:cont, rga}

  @spec update_unreachable({reachability, t, acc}, edge, t, reachability) ::
          {reachability, t, acc}
  defp update_unreachable({:unreachable, g, a}, {_, i, o, _}, graph, _) when is_list(o) do
    {:unreachable, mark_unreachable(graph, g, i), a}
  end

  defp update_unreachable({:unreachable, g, a}, {_, i, _, _}, graph, r) do
    {r, mark_unreachable(graph, g, i), a}
  end

  defp update_unreachable({r, g, a}, _edge, graph, _) do
    {r, %{graph | unreachable: g.unreachable}, a}
  end

  @spec mark_unreachable(t, t, Janus.attr()) :: t
  defp mark_unreachable(previous_graph, graph, attr) do
    if attr in graph.attr_trail do
      %{previous_graph | unreachable: graph.unreachable}
    else
      %{previous_graph | unreachable: MapSet.put(graph.unreachable, attr)}
    end
  end

  @spec direct_edge?(edge) :: boolean
  defp direct_edge?({_, _, _, %{depth: 0}}), do: true
  defp direct_edge?(_), do: false

  @spec unreachable?(t, edge) :: boolean
  defp unreachable?(graph, {_, i, _, _}) when is_list(i) do
    i in graph.unreachable or Enum.any?(i, &(&1 in graph.unreachable))
  end

  defp unreachable?(graph, {_, i, _, _}) do
    i in graph.unreachable
  end

  @spec cyclic?(t, edge) :: boolean
  defp cyclic?(graph, {_, i, _, _}), do: i in graph.attr_trail

  @spec from_resolvers([Resolver.t()]) :: :digraph.graph()
  defp from_resolvers(resolvers) do
    from_resolvers(resolvers, :digraph.new([]))
  end

  @spec from_resolvers([Resolver.t()], :digraph.graph()) :: :digraph.graph()
  defp from_resolvers([], dg), do: dg

  defp from_resolvers([res | t], dg) do
    i = extract_input_name(res)
    labels = output_info(res)
    output = Enum.map(labels, &Map.get(&1, :id))
    _ = Enum.each([i | res.input], &:digraph.add_vertex(dg, &1))
    _ = Enum.each(output, &:digraph.add_vertex(dg, &1))
    _ = Enum.each(labels, &:digraph.add_edge(dg, i, &1.id, %{&1 | id: res.id}))

    from_resolvers(t, dg)
  end

  @spec extract_input_name(Resolver.t()) :: id | [id] when id: {module, atom}
  defp extract_input_name(%Resolver{input: [id]}), do: id
  defp extract_input_name(%Resolver{input: [_ | _] = ids}), do: ids

  @spec output_info(Resolver.t()) :: [label(id)] when id: Resolver.id()
  defp output_info(resolver) do
    case EQL.to_ast(resolver.output) do
      nil ->
        []

      ast ->
        ast
        |> output_info(0, [], nil)
        |> Enum.map(fn {id, depth, path, union_key, leaf?} ->
          %{id: id, depth: depth, parent: path, union_key: union_key, leaf?: leaf?}
        end)
    end
  end

  @spec output_info([EQL.AST.t()] | EQL.AST.t(), depth, path, union_key) ::
          [{id, depth, path, union_key, leaf?}]
        when id: Janus.attr(), path: [id], union_key: id | nil, leaf?: boolean
  defp output_info([], _, _, _), do: []
  defp output_info([h | t], d, p, u), do: output_info(h, d, p, u) ++ output_info(t, d, p, u)
  defp output_info(%Prop{module: m, key: k}, d, p, u), do: [{{m, k}, d, p, u, true}]

  defp output_info(%Join{key: %Prop{module: m, key: k}, query: q}, d, p, u),
    do: [{{m, k}, d, p, u, false} | output_info(q, d + 1, [{m, k} | p], nil)]

  defp output_info(%Union{children: cs}, d, p, _), do: output_info(cs, d, p, nil)

  defp output_info(%Entry{key: %Prop{module: m, key: k}, query: q}, d, p, _),
    do: output_info(q, d, p, {m, k})

  defp output_info(%Query{children: cs}, d, p, _), do: output_info(cs, d, p, nil)

  @spec subqueries(t, Prop.t()) :: [Resolver.id()]
  defp subqueries(graph, prop) do
    graph.dg
    |> :digraph.in_edges(EQL.get_key(prop))
    |> Enum.map(fn
      {_, _, _, %{id: id}} -> id
      _ -> nil
    end)
    |> Enum.reject(&is_nil/1)
    |> Enum.dedup()
  end
end
