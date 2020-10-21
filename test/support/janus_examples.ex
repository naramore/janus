defmodule JanusExamples do
  @moduledoc false
  use Boundary, check: [in: true, out: false]
  alias Janus.{Graph, Planner, Resolver}

  def test do
    planner = Planner.new(complex_source())

    JanusExamples.complex_resolvers()
    |> Janus.Graph.new()
    |> Graph.walk_attr(complex_attr(), %{planner | current_attr: complex_attr()}, Planner)
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
      resolver({Complex, :r1}, [{Complex, :a}], [{Complex, :b}]),
      resolver({Complex, :r2}, [{Complex, :c}], [{Complex, :d}]),
      resolver({Complex, :r3}, [{Complex, :c}], [{Complex, :e}]),
      resolver({Complex, :r4}, [{Complex, :e}], [{Complex, :l}]),
      resolver({Complex, :r5}, [{Complex, :l}], [{Complex, :m}]),
      resolver({Complex, :r6}, [{Complex, :l}], [{Complex, :n}]),
      resolver({Complex, :r7}, [{Complex, :n}], [{Complex, :o}]),
      resolver({Complex, :r8}, [{Complex, :m}], [{Complex, :p}]),
      resolver({Complex, :r9}, [{Complex, :o}], [{Complex, :p}]),
      resolver({Complex, :r10}, [{Complex, :g}], [{Complex, :k}]),
      resolver({Complex, :r11}, [{Complex, :h}], [{Complex, :g}]),
      resolver({Complex, :r12}, [{Complex, :i}], [{Complex, :h}]),
      resolver({Complex, :r13}, [{Complex, :j}], [{Complex, :i}]),
      resolver({Complex, :r14}, [{Complex, :g}], [{Complex, :j}]),
      resolver({Complex, :r15}, [{Complex, :b}, {Complex, :d}], [{Complex, :f}]),
      resolver({Complex, :r16}, [{Complex, :q}], [{Complex, :r}]),
      resolver({Complex, :r17}, [{Complex, :t}], [{Complex, :v}]),
      resolver({Complex, :r18}, [{Complex, :u}], [{Complex, :v}]),
      resolver({Complex, :r19}, [{Complex, :v}], [{Complex, :w}]),
      resolver({Complex, :r20}, [{Complex, :r}, {Complex, :w}], [{Complex, :s}]),
      resolver({Complex, :r21}, [{Complex, :s}], [{Complex, :y}]),
      resolver({Complex, :r22}, [{Complex, :y}], [{Complex, :z}]),
      resolver({Complex, :r23}, [{Complex, :z}], [{Complex, :o}]),
      resolver({Complex, :r24}, [{Complex, :aa}], [{Complex, :ab}]),
      resolver({Complex, :r25}, [{Complex, :ab}], [{Complex, :z}]),
      resolver({Complex, :r26}, [{Complex, :ac}], [{Complex, :y}]),
      resolver({Complex, :r27}, [{Complex, :ad}], [{Complex, :ac}]),
      resolver({Complex, :r28}, [{Complex, :ae}], [{Complex, :ad}]),
      resolver({Complex, :r29}, [{Complex, :ae}], [{Complex, :af}]),
      resolver({Complex, :r30}, [{Complex, :af}], [{Complex, :ab}]),
      resolver({Complex, :r31}, [{Complex, :ad}], [{Complex, :ab}]),
      resolver({Complex, :r32}, [{Complex, :f}], [{Complex, :k}]),
      resolver({Complex, :r33}, [{Complex, :k}], [{Complex, :p}])
    ]
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

  @spec resolver(Resolver.id(), Resolver.input(), Resolver.output()) :: Resolver.t()
  def resolver(id, input, output) do
    Resolver.new(id, input, output, &default_resolver/2)
  end

  def default_resolver(_inputs, _env) do
    %{}
  end
end
