defmodule Janus.Plan do
  @moduledoc false

  alias Digraph.{Edge, Vertex}
  alias Janus.{Resolver, Utils}
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
          # optional(:requires) => Janus.shape_descriptor,
          optional(:params) => map
        }

  @root_start [:root | :start]
  @root_end [:root | :end]

  @spec init_graph(Digraph.t()) :: Digraph.t()
  def init_graph(graph) do
    {_, graph} = create_node(graph, :and, [], [], id: @root_start)
    {_, graph} = create_node(graph, :join, [], [], id: @root_end)
    graph
  end

  @spec create_attr_root_node(Digraph.t(), Janus.attr()) ::
          {:ok, {Vertex.id(), Digraph.t()}} | {:error, reason :: term}
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

  @spec pre_process_path(path) :: [{landmark, scope}]
  def pre_process_path(path) do
    {top_ands, new_path} =
      path
      |> :lists.reverse()
      |> Enum.reduce({[], []}, fn
        {:and, bs, b}, {t, []} ->
          {[{{:and, bs, b}, [{:and, bs}]} | t], [{{:join, bs}, [b, {:and, bs}]}]}

        {:and, bs, b}, {t, [{_, s} | _] = p} ->
          {[{{:and, bs, b}, [{:and, bs} | s]} | t], [{{:join, bs}, [b, {:and, bs} | s]} | p]}

        id, {t, []} ->
          {t, [{id, []}]}

        id, {t, [{_, s} | _] = p} ->
          {t, [{id, s} | p]}
      end)

    top_ands
    |> :lists.reverse(new_path)
    |> Enum.map(fn
      {{:join, _} = j, [_ | s]} -> {j, s}
      x -> x
    end)
  end

  @spec follow(
          Digraph.t(),
          Vertex.t() | Vertex.id() | nil,
          [{landmark, scope}],
          Janus.attr(),
          keyword
        ) :: {:ok, Digraph.t()} | {:error, reason :: term}
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
          Digraph.t(),
          Vertex.t(),
          [Vertex.t()] | Vertex.t() | nil,
          [{landmark, scope}],
          Janus.attr(),
          keyword
        ) ::
          {:ok, Digraph.t()} | {:error, reason :: term}
  defp follow_impl(graph, current, [], path, attr, opts) do
    follow_impl(graph, current, nil, path, attr, opts)
  end

  defp follow_impl(graph, current, nil, [{l, s} | t], attr, opts) do
    graph
    |> locate_and_connect(current.id, l, s, attr, opts)
    |> Rails.bind(fn {v, g} -> follow(g, v, t, attr, opts) end)
  end

  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
  defp follow_impl(graph, current, next, [{l, s} | t] = p, attr, opts) do
    if match_path_next?(next, l) do
      follow(graph, next, t, attr, opts)
    else
      case {current, next} do
        {_, %{label: %{resolver: _}}} ->
          graph
          |> inject_or_node(current, next)
          |> Rails.bind(fn {oid, g} -> locate_and_connect(g, oid, l, s, attr, opts) end)
          |> Rails.bind(fn {v, g} -> follow(g, v, t, attr, opts) end)

        {%{label: %{type: :and}}, branches} ->
          case Enum.find(branches, &match?({%{label: %{scope: [b | _]}}, [b | _]}, {&1, s})) do
            nil ->
              graph
              |> locate_and_connect(current.id, l, s, attr, opts)
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
              |> locate_and_connect(current.id, l, s, attr, opts)
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

  # TODO: filter out all out_neighbours w/o the current attr
  @spec next(Digraph.t(), Vertex.t(), Janus.attr()) :: [Vertex.t()] | Vertex.t() | nil
  defp next(graph, %{label: %{type: type}} = current, attr) do
    graph
    |> Digraph.out_neighbours(current.id)
    |> Enum.filter(&attr_match?(&1, attr))
    |> Utils.cond_pipe(type in [:resolver, :join], &List.first/1)
  end

  @spec inject_or_node(Digraph.t(), Vertex.t(), Vertex.t()) ::
          {:ok, {Vertex.id(), Digraph.t()}} | {:error, reason :: term}
  defp inject_or_node(graph, current, next) do
    inject_or_node(graph, current, next, current.label.scope)
  end

  @spec inject_or_node(Digraph.t(), Vertex.t(), Vertex.t(), scope) ::
          {:ok, {Vertex.id(), Digraph.t()}} | {:error, reason :: term}
  defp inject_or_node(graph, %{label: %{attrs: [attr]}} = current, next, scope) do
    inject_node(graph, current, next, &create_node(&1, :or, scope, attr))
  end

  defp inject_or_node(graph, %{label: %{attrs: attrs}} = current, next, scope) do
    inject_node(graph, current, next, &create_node(&1, :or, scope, attrs))
  end

  @spec inject_node(
          Digraph.t(),
          Vertex.t(),
          Vertex.t(),
          (Digraph.t() -> {Vertex.id(), Digraph.t()})
        ) :: {:ok, {Vertex.id(), Digraph.t()}} | {:error, reason :: term}
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
          Digraph.t(),
          Vertex.id(),
          landmark,
          scope,
          [Janus.attr()] | Janus.attr(),
          keyword
        ) :: {:ok, {Vertex.id(), Digraph.t()}} | {:error, reason :: term}
  defp locate_and_connect(graph, previous_id, landmark, scope, attr, opts) do
    with {next_id, graph} <- find_or_create(graph, landmark, scope, attr, opts),
         {:ok, {_, graph}} <- connect(graph, previous_id, next_id, attr) do
      {:ok, {next_id, graph}}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  @spec connect(Digraph.t(), Vertex.id(), Vertex.id(), [Janus.attr()] | Janus.attr() | nil) ::
          {:ok, {Edge.t(), Digraph.t()}} | {:error, reason :: term}
  defp connect(graph, v1, v2, attr) do
    graph
    |> Digraph.out_neighbours(v1)
    |> Enum.find(fn v -> v.id == v2 and attr_match?(v, attr) end)
    |> case do
      nil -> Digraph.add_edge(graph, v1, v2)
      id -> {:ok, {id, graph}}
    end
  end

  @spec find_or_create(Digraph.t(), landmark | :or, scope, [Janus.attr()] | Janus.attr(), keyword) ::
          {Vertex.id(), Digraph.t()}
  defp find_or_create(graph, landmark, scope, attr, opts) do
    case lookup(graph, landmark, scope) do
      nil ->
        create_node(graph, landmark, scope, attr, opts)

      id ->
        graph
        |> update_attrs(id, attr)
        |> (&{id, &1}).()
    end
  end

  @spec update_attrs(Digraph.t(), Vertex.id(), [Janus.attr()] | Janus.attr()) :: Digraph.t()
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

  @spec lookup(Digraph.t(), landmark | :or, scope, [Janus.attr()] | Janus.attr() | nil) ::
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
          Digraph.t(),
          landmark | :or | :and | :join,
          scope,
          [Janus.attr()] | Janus.attr(),
          keyword
        ) :: {Vertex.id(), Digraph.t()}
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
    %{
      scope: scope,
      attrs: if(is_list(attr), do: attr, else: [attr]),
      type: get_node_type(landmark)
    }
    |> Utils.cond_pipe(&match?(%{type: :resolver}, &1), &Map.put(&1, :resolver, landmark))
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
