defmodule Janus do
  @moduledoc """
  Core public API for `Janus`.

  There are two foundational components this graph query language
  is built upon: fully namespaced property names, and resolving
  functions with specified inputs and outputs (i.e. resolvers).

  ## Fully Namespaced Property Names

  Let's look at an example to get a glimpse of the significance:

      %{
        id: 123,
        name: "Joe Smith",
        address: "456 Lambda Ln"
      }

  From this map, it *could* be difficult to understand to what
  entity (or logical group) the properties `:id`, `:name`, and
  `:address` belong to. Because of the specific values, we can
  intuit that it's likely a person, but is it a customer, or an
  employee? Something else? Hard to tell...

      customers: [
        %{
          id: 123,
          name: "Joe Smith",
          address: "456 Lambda Ln"
        }
      ]

  Much more obvious now, but what if we're talking about multiple
  organizations, regions, offerings, etc. There is still
  ambiguity...

  And this is where fully namespaced properties come in:

      %{
        {Company.Sales.Customer, :id} => 123,
        {Company.Sales.Customer, :name} => "Joe Smith",
        {Company.Sales.Customer, :address} => "456 Lambda Ln"
      }

  Now, even without knowing the values, or the associated grouped
  properties the meaning/context of `{Company.Sales.Customer, :id}`
  is understood, purely by it's name.

  In Elixir we'll be using `{module, atom}` tuples as that seems
  more idiomatic (similar to mfa tuples).

  See [Clojure's Keywords](https://clojuredocs.org/clojure.core/keyword),
  for an environment that supports fully namespaced keywords/atoms
  natively.

  ## Resolvers

  Resolvers are functions bundled with a list of fully namespaced
  input properties, and fully namespaced output properties. The
  expectation of the function being that: given the named inputs,
  it will return the outputs. Similiar to the following:

      %{
        name: {Foo, :get_by_id},
        input: [{Foo, :id}],
        output: [
          {Foo, :bar},
          {Foo, :baz}
        ],
        function: fn _env, %{id: id} ->
          {bar, baz} = get_foo_by_id(id)
          %{
            {Foo, :bar} => bar,
            {Foo, :baz} => baz
          }
        end
      }

  Resolvers connect properties/attributes. If properties are the
  nodes, then resolvers are the edges of a directed graph,
  drawing the edges from the input nodes to point at their output
  nodes.

  Resolvers and namespaced properties are all you need to
  construct a graph-based query system similiar to
  [GraphQL](https://graphql.org/), but without the overly
  restrictive nature of a type system.
  """
  use Boundary, deps: [Digraph, EQL, Interceptor, Rails], exports: []

  @type env :: map
  @type attr :: EQL.AST.Prop.expr()
  @type shape_descriptor(x) :: %{optional(x) => shape_descriptor(x)}
  @type shape_descriptor :: shape_descriptor(attr)
  @type response_form(x) :: %{optional(x) => [response_form(x)] | any}
  @type response_form :: response_form(attr)
end
