defmodule EQL do
  @moduledoc """
  EDN Query Language (`EQL`) is a reimplementation of
  [EQL](https://github.com/edn-query-language/eql) in Elixir.

  Unlike Clojure, Elixir does not possess an [Extensible Data
  Notation (EDN)](https://github.com/edn-format/edn) equivalent
  at the time of this writing.

  ## Query / Transaction

  An `EQL` transaction is represented by a `List`.

  A transaction that only contains reads is commonly called a query,
  but notice that at the syntax level, it has no difference.

  ### Examples

  An empty transaction / query:

    iex> empty_txn = []
    iex> EQL.to_ast(empty_txn)
    %EQL.AST.Query{children: []}

  ## Properties

  Properties are expressed as Elixir `{module, atom}` tuples;
  they express the property been requested.

  ### Examples

    iex> query = [{Album, :name}, {Album, :year}]
    iex> EQL.to_ast(query)
    %EQL.AST.Query{children: [%EQL.AST.Prop{module: Album, key: :name},
                              %EQL.AST.Prop{module: Album, key: :year}]}

  ## Joins

  Joins are used to describe nesting in the request transaction.
  They are represented as Elixir `Map`s, always with a single entry,
  the key is the property to join on, and the value is a sub-query to run.

  ### Examples

  Simple join

    iex> join = [%{{Favorite, :albums} => [{Album, :name}, {Album, :year}]}]
    iex> EQL.to_ast(join)
    %EQL.AST.Query{children: [
      %EQL.AST.Join{
        key: %EQL.AST.Prop{module: Favorite, key: :albums},
        query: %EQL.AST.Query{children: [
          %EQL.AST.Prop{module: Album, key: :name},
          %EQL.AST.Prop{module: Album, key: :year}
        ]}
      }
    ]}

  Nested joins

    iex> join = [%{{Favorite, :albums} => [
    ...>   {Album, :name},
    ...>   {Album, :year},
    ...>   %{{Album, :tracks} => [
    ...>     {Track, :name},
    ...>     {Track, :duration}
    ...>   ]}
    ...> ]}]
    iex> EQL.to_ast(join)
    %EQL.AST.Query{children: [
      %EQL.AST.Join{
        key: %EQL.AST.Prop{module: Favorite, key: :albums},
        query: %EQL.AST.Query{children: [
          %EQL.AST.Prop{module: Album, key: :name},
          %EQL.AST.Prop{module: Album, key: :year},
          %EQL.AST.Join{
            key: %EQL.AST.Prop{module: Album, key: :tracks},
            query: %EQL.AST.Query{children: [
              %EQL.AST.Prop{module: Track, key: :name},
              %EQL.AST.Prop{module: Track, key: :duration}
            ]}
          }
        ]}
      }
    ]}

  ## Idents

  Idents are represented by a `{property, value}` tuple, where the first
  is a property and the second can be anything. They are like (lookup refs
  on Datomic)[https://blog.datomic.com/2014/02/datomic-lookup-refs.html], in
  general, they can provide an address-like thing, and their use and
  semantics might vary from system to system.

  ### Examples

      iex> ident = [{{Customer, :id}, 123}]
      iex> EQL.to_ast(ident)
      %EQL.AST.Query{children: [
        %EQL.AST.Ident{
          key: %EQL.AST.Prop{module: Customer, key: :id},
          value: 123
        }
      ]}

  It’s common to use an ident as a join key to start a query for some entity:

    iex> join = [%{{{Customer, :id}, 123} => [
    ...>   {Customer, :name},
    ...>   {Customer, :email}
    ...> ]}]
    iex> EQL.to_ast(join)
    %EQL.AST.Query{children: [
      %EQL.AST.Join{
        key: %EQL.AST.Ident{
          key: %EQL.AST.Prop{module: Customer, key: :id},
          value: 123
        },
        query: %EQL.AST.Query{children: [
          %EQL.AST.Prop{module: Customer, key: :name},
          %EQL.AST.Prop{module: Customer, key: :email}
        ]}
      }
    ]}

  ## Parameters

  `EQL` properties, joins, and idents have support for parametrization.
  This allows the query to provide an extra dimension of information about the requested data.
  A parameter is expressed by wrapping the thing with an improper list, like so:

    iex> # without params
    iex> EQL.to_ast([{Foo, :bar}])
    %EQL.AST.Query{children: [%EQL.AST.Prop{module: Foo, key: :bar}]}

    iex> # with params
    iex> EQL.to_ast([[{Foo, :bar} | %{with: "params"}]])
    %EQL.AST.Query{children: [
      %EQL.AST.Params{
        expr: %EQL.AST.Prop{module: Foo, key: :bar},
        params: %{with: "params"}
      }
    ]}

  Params must always be maps, the map values can be anything.

  ## Unions

  In `EQL` unions are used to specify polymorphic requirements.
  For example, a messaging app may have a single list, and each
  entry on the chat log can be a message, audio or photo, each
  having its own query requirement.

      # message query
      [{Message, :id}, {Message, :text}, {Chat.Entry, :timestamp}]

      # audio query
      [{Audio, :id}, {Audio, :url}, {Audio, :duration}, {Chat.Entry, :timestamp}]

      # photo query
      [{Photo, :id}, {Photo, :url}, {Photo, :width}, {Photo, :height}, {Chat.Entry, :timestamp}]

      # list query
      [%{{Chat, :entries} => "???"}]

  Now to express this polymorphic requirement as the sub-query of the
  `{Chat, :entries}` list we can use a map as the join value, and each
  entry on this map represents a possible sub-query.

  The way this information is used is up to the parser implementation;
  EQL only defines the syntax.

  ### Examples

    iex> union = [%{{Chat, :entries} => %{
    ...>   {Message, :id} => [{Message, :id}, {Message, :text}, {Chat.Entry, :timestamp}],
    ...>   {Audio, :id} => [{Audio, :id}, {Audio, :url}, {Audio, :duration}, {Chat.Entry, :timestamp}],
    ...>   {Photo, :id} => [{Photo, :id}, {Photo, :url}, {Photo, :width}, {Photo, :height}, {Chat.Entry, :timestamp}]
    ...> }}]
    iex> EQL.to_ast(union)
    %EQL.AST.Query{children: [
      %EQL.AST.Join{
        key: %EQL.AST.Prop{module: Chat, key: :entries},
        query: %EQL.AST.Union{children: [
          %EQL.AST.Union.Entry{
            key: %EQL.AST.Prop{module: Audio, key: :id},
            query: %EQL.AST.Query{children: [
              %EQL.AST.Prop{module: Audio, key: :id},
              %EQL.AST.Prop{module: Audio, key: :url},
              %EQL.AST.Prop{module: Audio, key: :duration},
              %EQL.AST.Prop{module: Chat.Entry, key: :timestamp},
            ]}
          },
          %EQL.AST.Union.Entry{
            key: %EQL.AST.Prop{module: Message, key: :id},
            query: %EQL.AST.Query{children: [
              %EQL.AST.Prop{module: Message, key: :id},
              %EQL.AST.Prop{module: Message, key: :text},
              %EQL.AST.Prop{module: Chat.Entry, key: :timestamp},
            ]}
          },
          %EQL.AST.Union.Entry{
            key: %EQL.AST.Prop{module: Photo, key: :id},
            query: %EQL.AST.Query{children: [
              %EQL.AST.Prop{module: Photo, key: :id},
              %EQL.AST.Prop{module: Photo, key: :url},
              %EQL.AST.Prop{module: Photo, key: :width},
              %EQL.AST.Prop{module: Photo, key: :height},
              %EQL.AST.Prop{module: Chat.Entry, key: :timestamp},
            ]}
          },
        ]}
      }
    ]}

  ## Mutations

  Mutations in `EQL` are used to represent operation calls,
  usually to do something that will cause a side effect.
  Mutations as data allows that operation to behave much like event sourcing,
  and can be transparently applied locally, across a network, onto an event bus, etc.

  ### Examples

  A mutation is represented by a list of two elements; the first is the symbol that names the mutation, and the second is a map with input data.

    iex> mutation = [{Call.Some, :operation, ["input"]}]
    iex> EQL.to_ast(mutation)
    %EQL.AST.Query{children: [
      %EQL.AST.Mutation{
        module: Call.Some,
        fun: :operation,
        args: ["input"]
      }
    ]}
  """

  use Boundary, deps: [], exports: []
  alias EQL.AST

  @type query :: AST.Query.expr()

  @spec to_ast(term) :: AST.t() | nil
  def to_ast(term) do
    AST.Query.to_ast(term)
  end

  @spec to_expr(AST.t()) :: term
  def to_expr(ast) do
    AST.to_expr(ast)
  end

  @spec get_key(AST.t()) :: AST.Prop.expr() | nil
  def get_key(ast) do
    AST.get_key(ast)
  end

  defdelegate union_to_query(union), to: AST.Union, as: :to_query

  defdelegate query(children), to: AST.Query, as: :new

  defdelegate prop(module, key), to: AST.Prop, as: :new

  defdelegate ident(prop, value), to: AST.Ident, as: :new

  defdelegate join(prop, query), to: AST.Join, as: :new

  defdelegate union(children), to: AST.Union, as: :new

  defdelegate union_entry(key, query), to: AST.Union.Entry, as: :new

  defdelegate params(expr, params), to: AST.Params, as: :new

  defdelegate mutation(module, fun, args), to: AST.Mutation, as: :new
end
