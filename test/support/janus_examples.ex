defmodule JanusExamples do
  @moduledoc false
  use Boundary, check: [in: true, out: false]
  alias Janus.{Planner, Resolver}

  # {:ok, {g, a}} = JanusExamples.test_ast_alt()
  def test_ast_alt do
    extra_resolvers = [
      resolver({Complex, :r40}, [], [{Complex, :c}], &lookup/3),
      resolver({Complex, :r41}, [], [{Complex, :q}], &lookup/3),
      resolver({Complex, :r42}, [], [{Complex, :t}], &lookup/3),
      resolver({Complex, :r43}, [], [{Complex, :u}], &lookup/3),
      resolver({Complex, :r44}, [], [{Complex, :ae}], &lookup/3)
    ]

    ast = EQL.to_ast([{Complex, :p}])

    complex_resolvers()
    |> :lists.reverse(extra_resolvers)
    |> Janus.Graph.new()
    |> Planner.walk_ast(ast, Planner.new([]))
  end

  # {:ok, {g, a}} = JanusExamples.test_ast()
  def test_ast do
    planner =
      Planner.new([
        {Complex, :c},
        {Complex, :q},
        {Complex, :t},
        {Complex, :u},
        {Complex, :ae}
      ])

    ast =
      EQL.to_ast([
        {Complex, :p},
        {Complex, :m},
        # {Complex, :ae},
        {Complex, :n}
      ])

    JanusExamples.complex_resolvers()
    |> Janus.Graph.new()
    |> Planner.walk_ast(ast, planner)
  end

  # {_, g, a} = JanusExamples.test_attr()
  def test_attr do
    planner = Planner.new(complex_source())

    JanusExamples.complex_resolvers()
    |> Janus.Graph.new()
    |> Planner.walk_attr(complex_attr(), Planner.reset(planner, complex_attr()))
  end

  def complex_source do
    [
      {Complex, :c},
      {Complex, :q},
      {Complex, :t},
      {Complex, :u},
      {Complex, :ae}
    ]
  end

  def complex_attr, do: {Complex, :p}

  @spec complex_resolvers() :: [Resolver.t()]
  def complex_resolvers do
    [
      resolver({Complex, :r1}, [{Complex, :a}], [{Complex, :b}], &lookup/3),
      resolver({Complex, :r2}, [{Complex, :c}], [{Complex, :d}], &lookup/3),
      resolver({Complex, :r3}, [{Complex, :c}], [{Complex, :e}], &lookup/3),
      resolver({Complex, :r4}, [{Complex, :e}], [{Complex, :l}], &lookup/3),
      resolver({Complex, :r5}, [{Complex, :l}], [{Complex, :m}], &lookup/3),
      resolver({Complex, :r6}, [{Complex, :l}], [{Complex, :n}], &lookup/3),
      resolver({Complex, :r7}, [{Complex, :n}], [{Complex, :o}], &lookup/3),
      resolver({Complex, :r8}, [{Complex, :m}], [{Complex, :p}], &lookup/3),
      resolver({Complex, :r9}, [{Complex, :o}], [{Complex, :p}], &lookup/3),
      resolver({Complex, :r10}, [{Complex, :g}], [{Complex, :k}], &lookup/3),
      resolver({Complex, :r11}, [{Complex, :h}], [{Complex, :g}], &lookup/3),
      resolver({Complex, :r12}, [{Complex, :i}], [{Complex, :h}], &lookup/3),
      resolver({Complex, :r13}, [{Complex, :j}], [{Complex, :i}], &lookup/3),
      resolver({Complex, :r14}, [{Complex, :g}], [{Complex, :j}], &lookup/3),
      resolver({Complex, :r15}, [{Complex, :b}, {Complex, :d}], [{Complex, :f}], &lookup/3),
      resolver({Complex, :r16}, [{Complex, :q}], [{Complex, :r}], &lookup/3),
      resolver({Complex, :r17}, [{Complex, :t}], [{Complex, :v}], &lookup/3),
      resolver({Complex, :r18}, [{Complex, :u}], [{Complex, :v}], &lookup/3),
      resolver({Complex, :r19}, [{Complex, :v}], [{Complex, :w}], &lookup_and/3),
      resolver({Complex, :r20}, [{Complex, :r}, {Complex, :w}], [{Complex, :s}], &lookup/3),
      resolver({Complex, :r21}, [{Complex, :s}], [{Complex, :y}], &lookup/3),
      resolver({Complex, :r22}, [{Complex, :y}], [{Complex, :z}], &lookup/3),
      resolver({Complex, :r23}, [{Complex, :z}], [{Complex, :o}], &lookup/3),
      resolver({Complex, :r24}, [{Complex, :aa}], [{Complex, :ab}], &lookup/3),
      resolver({Complex, :r25}, [{Complex, :ab}], [{Complex, :z}], &lookup/3),
      resolver({Complex, :r26}, [{Complex, :ac}], [{Complex, :y}], &lookup/3),
      resolver({Complex, :r27}, [{Complex, :ad}], [{Complex, :ac}], &lookup/3),
      resolver({Complex, :r28}, [{Complex, :ae}], [{Complex, :ad}], &lookup/3),
      resolver({Complex, :r29}, [{Complex, :ae}], [{Complex, :af}], &lookup/3),
      resolver({Complex, :r30}, [{Complex, :af}], [{Complex, :ab}], &lookup/3),
      resolver({Complex, :r31}, [{Complex, :ad}], [{Complex, :ab}], &lookup/3),
      resolver({Complex, :r32}, [{Complex, :f}], [{Complex, :k}], &lookup/3),
      resolver({Complex, :r33}, [{Complex, :k}], [{Complex, :p}], &lookup/3)
    ]
  end

  @available %{
    {Complex, :c} => 3,
    {Complex, :q} => 17,
    {Complex, :t} => 20,
    {Complex, :u} => 21,
    {Complex, :ae} => 31
  }

  def available_data, do: @available

  @complex_database %{
    {Complex, :a} => %{},
    {Complex, :b} => %{},
    {Complex, :c} => %{3 => %{{Complex, :e} => 5}},
    {Complex, :d} => %{},
    {Complex, :e} => %{5 => %{{Complex, :l} => 12}},
    {Complex, :f} => %{},
    {Complex, :g} => %{},
    {Complex, :h} => %{},
    {Complex, :i} => %{},
    {Complex, :j} => %{},
    {Complex, :k} => %{},
    {Complex, :l} => %{12 => %{{Complex, :n} => 14, {Complex, :m} => 13}},
    {Complex, :m} => %{13 => %{{Complex, :p} => 16}},
    {Complex, :n} => %{14 => %{{Complex, :o} => 15}},
    {Complex, :o} => %{15 => %{{Complex, :p} => 16}},
    {Complex, :p} => %{16 => :ANSWER!},
    {Complex, :q} => %{17 => %{{Complex, :r} => 18}},
    {Complex, :r} => %{18 => %{{Complex, :s} => {:and, 8}}},
    {Complex, :s} => %{19 => %{{Complex, :y} => 25}},
    {Complex, :t} => %{20 => %{{Complex, :v} => 22}},
    {Complex, :u} => %{21 => %{{Complex, :v} => 22}},
    {Complex, :v} => %{22 => %{{Complex, :w} => 23}},
    {Complex, :w} => %{23 => %{{Complex, :s} => {:and, 11}}},
    {Complex, :x} => %{24 => %{}},
    {Complex, :y} => %{25 => %{{Complex, :y} => 26}},
    {Complex, :z} => %{26 => %{{Complex, :o} => 15}},
    {Complex, :aa} => %{27 => %{}},
    {Complex, :ab} => %{28 => %{{Complex, :z} => 26}},
    {Complex, :ac} => %{29 => %{{Complex, :y} => 25}},
    {Complex, :ad} => %{30 => %{{Complex, :ab} => 28, {Complex, :ac} => 29}},
    {Complex, :ae} => %{31 => %{{Complex, :ad} => 30, {Complex, :af} => 32}},
    {Complex, :af} => %{32 => %{{Complex, :ab} => 28}}
  }

  def lookup(input, _env, output) do
    [key | _] = Map.keys(input)

    @complex_database
    |> Map.get(key)
    |> Map.get(Map.get(input, key))
    |> Enum.filter(fn {k, _} -> k in output end)
    |> Enum.into(%{})
  end

  def lookup_and(input, _env, output) do
    input
    |> Enum.map(fn {k, v} -> get_in(@complex_database, [k, v]) end)
    |> Enum.map(&Enum.filter(&1, fn {k, _} -> k in output end))
    |> Enum.concat()
    |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
    |> Enum.map(fn {k, vs} ->
      {k, Enum.reduce(vs, 0, fn {:and, x}, acc -> acc + x end)}
    end)
    |> Enum.into(%{})
  end

  @spec nested_resolvers() :: [Resolver.t()]
  def nested_resolvers do
    [
      resolver({Nested, :r1}, [{Nested, :a}], [
        {Nested, :b},
        {Nested, :c},
        %{{Nested, :d} => [{Nested, :e}]}
      ]),
      resolver({Nested, :r2}, [{Nested, :e}], [
        {Nested, :f},
        {Nested, :g},
        %{{Nested, :h} => [{Nested, :i}]}
      ]),
      resolver({Nested, :r3}, [{Nested, :i}], [
        {Nested, :j},
        {Nested, :k},
        %{{Nested, :l} => [{Nested, :m}]}
      ])
    ]
  end

  @spec resolver(Resolver.id(), Resolver.input(), Resolver.output(), (... -> term) | nil) ::
          Resolver.t()
  def resolver(id, input, output, resolve \\ nil)

  def resolver(id, input, output, nil) do
    Resolver.new(id, input, output, &default_resolver/2)
  end

  def resolver(id, input, output, resolve) when is_function(resolve, 2) do
    Resolver.new(id, input, output, resolve)
  end

  def resolver(id, input, output, resolve) when is_function(resolve, 3) do
    Resolver.new(id, input, output, &resolve.(&1, &2, output))
  end

  def default_resolver(_inputs, _env) do
    %{}
  end
end
