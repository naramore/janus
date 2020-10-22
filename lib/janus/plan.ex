defmodule Janus.Plan do
  @moduledoc false

  alias Digraph.{Edge, Vertex}
  alias Janus.{Graph, Resolver, Utils}
  require Logger

  @type t :: Digraph.t()
  @type path :: [landmark]
  @type landmark :: Resolver.id() | {:and, [Janus.attr()], Janus.attr()} | {:join, [Janus.attr()]}
  @type scope :: [Janus.attr() | {:and, [Janus.attr()]}]
  @type type :: :resolver | :and | :or | :join
  @type node_label :: %{
          required(:scope) => scope,
          required(:attrs) => [Janus.attr()],
          required(:type) => type,
          optional(:resolver) => Resolver.id(),
          optional(:params) => map,
          optional(:input) => [Janus.attr()]
        }

  @root_start [:root | :start]
  @root_end [:root | :end]

  @spec init_graph(t) :: t
  def init_graph(graph) do
    {_, graph} = create_node(graph, :and, [], [], id: @root_start)
    {_, graph} = create_node(graph, :join, [], [], id: @root_end)
    graph
  end

  @spec create_attr_root_node(t, Janus.attr()) ::
          {:ok, {Vertex.id(), t}} | {:error, reason :: term}
  def create_attr_root_node(graph, attr) do
    case lookup(graph, :or, [], attr) do
      nil ->
        {attr_root, graph} = create_node(graph, :or, [], attr)

        case connect(graph, @root_start, attr_root, attr) do
          {:ok, {_, g}} -> {:ok, {attr_root, g}}
          {:error, reason} -> {:error, reason}
        end

      id ->
        {:ok, {id, graph}}
    end
  end

  @spec required_of(t, Vertex.t() | Vertex.id()) :: Janus.shape_descriptor()
  def required_of(graph, %Vertex{id: id}) do
    required_of(graph, id)
  end

  def required_of(graph, id) do
    # TODO: recursively traverse until finding one or more resolver nodes
    graph
    |> Digraph.out_neighbours(id)
    |> get_in([Access.all(), :label, :input])
    |> Enum.reject(&is_nil/1)
    |> Enum.concat()
    |> Utils.to_shape_descriptor()
  end

  @spec follow(
          t,
          Vertex.t() | Vertex.id() | nil,
          [{landmark, Graph.node_id(), scope}],
          Janus.attr(),
          keyword
        ) :: {:ok, t} | {:error, reason :: term}
  def follow(graph, current, path, attr, opts \\ [])

  def follow(graph, nil, path, attr, opts) do
    # TODO: eventually remove/modify these logs
    _ = Logger.error(fn -> "#{Utils.inspect(graph)}" end)
    _ = Logger.error(fn -> "#{Utils.inspect(path)}" end)
    _ = Logger.error(fn -> "#{Utils.inspect(attr)}" end)
    _ = Logger.error(fn -> "#{Utils.inspect(opts)}" end)
    {:error, :invalid_current}
  end

  def follow(graph, %Vertex{} = current, [], _attr, _opts) do
    graph
    |> Digraph.out_neighbours(current.id)
    |> Enum.filter(&match?(%{id: @root_end}, &1))
    |> case do
      [] ->
        case Digraph.add_edge(graph, current.id, @root_end) do
          {:error, reason} -> {:error, reason}
          {:ok, {_, graph}} -> {:ok, graph}
        end

      _ ->
        {:ok, graph}
    end
  end

  def follow(graph, %Vertex{} = current, path, attr, opts) do
    follow_impl(graph, current, next(graph, current, attr), path, attr, opts)
  end

  def follow(graph, id, path, attr, opts) do
    follow(graph, Digraph.vertex(graph, id), path, attr, opts)
  end

  @spec follow_impl(
          t,
          Vertex.t(),
          [Vertex.t()] | Vertex.t() | nil,
          [{landmark, Graph.node_id(), scope}],
          Janus.attr(),
          keyword
        ) ::
          {:ok, t} | {:error, reason :: term}
  defp follow_impl(graph, current, [], path, attr, opts) do
    follow_impl(graph, current, nil, path, attr, opts)
  end

  defp follow_impl(graph, current, nil, [{l, i, s} | t], attr, opts) do
    graph
    |> locate_and_connect(current.id, l, s, attr, Keyword.put(opts, :input, i))
    |> Rails.bind(fn {v, g} -> follow(g, v, t, attr, opts) end)
  end

  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
  defp follow_impl(graph, current, next, [{l, i, s} | t] = p, attr, opts) do
    if match_path_next?(next, l) do
      follow(graph, next, t, attr, opts)
    else
      case {current, next} do
        {_, %{label: %{resolver: _}}} ->
          graph
          |> inject_or_node(current, next)
          |> Rails.bind(fn {oid, g} ->
            locate_and_connect(g, oid, l, s, attr, Keyword.put(opts, :input, i))
          end)
          |> Rails.bind(fn {v, g} -> follow(g, v, t, attr, opts) end)

        {%{label: %{type: :and}}, branches} ->
          case Enum.find(branches, &match?({%{label: %{scope: [b | _]}}, [b | _]}, {&1, s})) do
            nil ->
              graph
              |> locate_and_connect(current.id, l, s, attr, Keyword.put(opts, :input, i))
              |> Rails.bind(fn {v, g} -> follow(g, v, t, attr, opts) end)

            branch ->
              if match_path_next?(branch, hd(t)) do
                follow(graph, branch, t, attr, opts)
              else
                graph
                |> inject_or_node(current, branch, s)
                |> Rails.bind(fn {v, g} -> follow(g, v, p, attr, opts) end)
              end
          end

        {%{label: %{type: :or}}, branches} ->
          case Enum.find(branches, &match_path_next?(&1, l)) do
            nil ->
              graph
              |> locate_and_connect(current.id, l, s, attr, Keyword.put(opts, :input, i))
              |> Rails.bind(fn {v, g} -> follow(g, v, t, attr, opts) end)

            branch ->
              follow(graph, branch, t, attr, opts)
          end

        {_, %{label: %{type: :or}}} ->
          follow(graph, next, p, attr, opts)

        _ ->
          # TODO: eventually remove/modify these logs
          # require IEx; IEx.pry()
          _ = Logger.error(fn -> "graph: #{Utils.inspect(graph)}" end)
          _ = Logger.error(fn -> "current: #{Utils.inspect(current)}" end)
          _ = Logger.error(fn -> "next: #{Utils.inspect(next)}" end)
          _ = Logger.error(fn -> "path: #{Utils.inspect(p)}" end)
          _ = Logger.error(fn -> "attr: #{Utils.inspect(attr)}" end)
          _ = Logger.error(fn -> "opts: #{Utils.inspect(opts)}" end)
          {:error, :unexpected_form}
      end
    end
  end

  @spec next(t, Vertex.t(), Janus.attr()) :: [Vertex.t()] | Vertex.t() | nil
  defp next(graph, %{label: %{type: type}} = current, attr) do
    graph
    |> Digraph.out_neighbours(current.id)
    |> Enum.filter(&attr_match?(&1, attr))
    |> Utils.cond_pipe(type in [:resolver, :join], &List.first/1)
  end

  @spec inject_or_node(t, Vertex.t(), Vertex.t()) ::
          {:ok, {Vertex.id(), t}} | {:error, reason :: term}
  defp inject_or_node(graph, current, next) do
    inject_or_node(graph, current, next, current.label.scope)
  end

  @spec inject_or_node(t, Vertex.t(), Vertex.t(), scope) ::
          {:ok, {Vertex.id(), t}} | {:error, reason :: term}
  defp inject_or_node(graph, %{label: %{attrs: [attr]}} = current, next, scope) do
    inject_node(graph, current, next, &create_node(&1, :or, scope, attr))
  end

  defp inject_or_node(graph, %{label: %{attrs: attrs}} = current, next, scope) do
    inject_node(graph, current, next, &create_node(&1, :or, scope, attrs))
  end

  @spec inject_node(
          t,
          Vertex.t(),
          Vertex.t(),
          (t -> {Vertex.id(), t})
        ) :: {:ok, {Vertex.id(), t}} | {:error, reason :: term}
  defp inject_node(graph, current, next, fun) do
    with %{id: id, label: label} <-
           Enum.find(Digraph.out_edges(graph, current.id), &(&1.v2 == next.id)),
         {injected_id, g} <- fun.(graph),
         {:ok, {_, g}} <- Digraph.add_edge(g, current.id, injected_id, label),
         {:ok, {_, g}} <- Digraph.add_edge(g, injected_id, next.id) do
      {:ok, {injected_id, Digraph.del_edge(g, id)}}
    else
      _ -> {:error, {:edge_not_found, current.id, next.id}}
    end
  end

  @spec locate_and_connect(
          t,
          Vertex.id(),
          landmark,
          scope,
          [Janus.attr()] | Janus.attr(),
          keyword
        ) :: {:ok, {Vertex.id(), t}} | {:error, reason :: term}
  defp locate_and_connect(graph, previous_id, landmark, scope, attr, opts) do
    with {next_id, graph} <- find_or_create(graph, landmark, scope, attr, opts),
         {:ok, {_, graph}} <- connect(graph, previous_id, next_id, attr) do
      {:ok, {next_id, graph}}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @spec connect(t, Vertex.id(), Vertex.id(), [Janus.attr()] | Janus.attr() | nil) ::
          {:ok, {Edge.t(), t}} | {:error, reason :: term}
  defp connect(graph, v1, v2, attr) do
    graph
    |> Digraph.out_neighbours(v1)
    |> Enum.find(fn v -> v.id == v2 and attr_match?(v, attr) end)
    |> case do
      nil -> Digraph.add_edge(graph, v1, v2)
      id -> {:ok, {id, graph}}
    end
  end

  @spec find_or_create(t, landmark | :or, scope, [Janus.attr()] | Janus.attr(), keyword) ::
          {Vertex.id(), t}
  defp find_or_create(graph, landmark, scope, attr, opts) do
    case lookup(graph, landmark, scope) do
      nil ->
        create_node(graph, landmark, scope, attr, opts)

      id ->
        graph
        |> update_attrs(id, attr)
        |> update_via_opts(id, opts)
        |> (&{id, &1}).()
    end
  end

  @spec update_attrs(t, Vertex.id(), [Janus.attr()] | Janus.attr()) :: t
  defp update_attrs(graph, id, attr) do
    update_in(graph, [:vertices, id, :label, :attrs], fn
      nil ->
        if is_list(attr), do: attr, else: [attr]

      attrs ->
        if is_list(attr) do
          Enum.dedup(:lists.reverse(attr, attrs))
        else
          Enum.dedup([attr | attrs])
        end
    end)
  end

  @spec update_via_opts(t, Vertex.t() | Vertex.id(), keyword) :: t
  defp update_via_opts(graph, %Vertex{} = vertex, opts) do
    if Keyword.has_key?(opts, :params) do
      update_in(graph, [:vertices, vertex.id, :label, :params], fn
        nil ->
          Keyword.get(opts, :params)

        params ->
          Map.merge(params, Keyword.get(opts, :params), fn _, m1, m2 -> Map.merge(m1, m2) end)
      end)
    else
      graph
    end
  end

  defp update_via_opts(graph, id, opts) do
    case Digraph.vertex(graph, id) do
      nil -> graph
      vertex -> update_via_opts(graph, vertex, opts)
    end
  end

  @spec lookup(t, landmark | :or, scope, [Janus.attr()] | Janus.attr() | nil) ::
          Vertex.id() | nil
  defp lookup(graph, landmark, scope, attr \\ nil) do
    graph
    |> Digraph.vertices()
    |> Enum.find_value(fn vertex ->
      if vertex_match?(vertex, landmark, scope, attr) do
        vertex.id
      end
    end)
  end

  @spec vertex_match?(Vertex.t(), landmark | :or, scope, [Janus.attr()] | Janus.attr() | nil) ::
          boolean
  defp vertex_match?(v, id, scope, attr) do
    type_match?(v, id) and scope_match?(v, scope) and attr_match?(v, attr)
  end

  @spec type_match?(Vertex.t(), landmark | :or) :: boolean
  defp type_match?(%{label: %{type: :and}}, {:and, _, _}), do: true
  defp type_match?(%{label: %{type: :join}}, {:join, _}), do: true
  defp type_match?(%{label: %{type: :or}}, :or), do: true
  defp type_match?(%{label: %{resolver: id}}, id), do: true
  defp type_match?(_, _), do: false

  @spec scope_match?(Vertex.t(), scope) :: boolean
  defp scope_match?(%{label: %{scope: scope}}, scope), do: true
  defp scope_match?(_, _), do: false

  @spec attr_match?(Vertex.t(), [Janus.attr()] | Janus.attr() | nil) :: boolean
  defp attr_match?(_, nil), do: true
  defp attr_match?(%{label: %{attrs: []}}, []), do: true
  defp attr_match?(%{label: %{attrs: attrs}}, [_ | _] = attr), do: Enum.all?(attr, &(&1 in attrs))
  defp attr_match?(%{label: %{attrs: attrs}}, attr), do: attr in attrs
  defp attr_match?(_, _), do: false

  @spec create_node(
          t,
          landmark | :or | :and | :join,
          scope,
          [Janus.attr()] | Janus.attr(),
          keyword
        ) :: {Vertex.id(), t}
  defp create_node(graph, landmark, scope, attr, opts \\ []) do
    label = create_node_label(landmark, scope, attr, opts)

    if Keyword.has_key?(opts, :id) do
      Digraph.add_vertex(graph, Keyword.get(opts, :id), label)
    else
      Digraph.add_next_vertex(graph, label)
    end
  end

  @spec create_node_label(
          landmark | :or | :and | :join,
          scope,
          [Janus.attr()] | Janus.attr(),
          keyword
        ) :: node_label
  defp create_node_label(landmark, scope, attr, opts) do
    input =
      case Keyword.get(opts, :input, []) do
        i when is_list(i) -> i
        i -> [i]
      end

    %{
      scope: scope,
      attrs: if(is_list(attr), do: attr, else: [attr]),
      type: get_node_type(landmark)
    }
    |> Utils.cond_pipe(
      &match?(%{type: :resolver}, &1),
      &Map.merge(&1, %{resolver: landmark, input: input})
    )
    |> Utils.cond_pipe(&add_params?(&1, opts), &Map.put(&1, :params, Keyword.get(opts, :params)))
  end

  @spec add_params?(node_label, keyword) :: boolean
  defp add_params?(%{type: :resolver}, opts) do
    Keyword.has_key?(opts, :params)
  end

  defp add_params?(_, _), do: false

  @spec get_node_type(landmark | :or | :and | :join) :: type
  defp get_node_type({:and, _, _}), do: :and
  defp get_node_type({:join, _}), do: :join
  defp get_node_type(type) when type in [:or, :and, :join], do: type
  defp get_node_type(_), do: :resolver

  @spec match_path_next?(Vertex.t(), landmark) :: boolean
  defp match_path_next?(%{label: %{type: :and, scope: [{:and, bs} | _]}}, {:and, bs, _}), do: true
  defp match_path_next?(%{label: %{type: :join, scope: [{:and, bs} | _]}}, {:join, bs}), do: true
  defp match_path_next?(%{label: %{resolver: id}}, id), do: true
  defp match_path_next?(_, _), do: false
end
