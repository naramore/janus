> **HIGHLY EXPERIMENTAL AND IN ACTIVE DEVELOPEMENT: USE AT YOUR OWN RISK**

# Janus

> Roman God of beginnings, gates, transitions, time,
> duality, doorways, passages, and ending
>
> [Wikipedia](https://en.wikipedia.org/wiki/Janus)

I've been a huge fan of GraphQL and everything that specification
has accomplished. When compared to other similar wire standards,
GraphQL's combination of power, tooling, community support,
extensibility and other amazing tools like
[Apollo Federation](https://www.apollographql.com/docs/federation/)
blow other tools out of the water (in *my* opinion).

But when I saw [The Maximal Graph](https://www.youtube.com/watch?v=IS3i3DTUnAI)
by [Wilker Lúcio](https://github.com/wilkerlucio), the simplicity and
power of [Pathom](https://github.com/wilkerlucio/pathom)'s model converted me.
Thanks to Wilker Lúcio for all the great work, it's only because
of his work (and presentations) on Pathom that this library exists!

Janus is basically Pathom in Elixir. See the following for more details:
- [Pathom 3 is Comming](https://blog.wsscode.com/pathom-3-is-coming/)
- [Pathom 3 GitHub](https://github.com/wilkerlucio/pathom3)
- [Pathom GitHub](https://github.com/wilkerlucio/pathom)
- [EDN Query Language (EQL)](https://github.com/edn-query-language/eql)
- [Extensible Data Notation (EDN)](https://github.com/edn-format/edn)
- [Tranist](https://github.com/cognitect/transit-cljs)

## Musings

Should properties emulate the Pathom form or follow a more idiomatically
Erlang/Elixir form?

  ```elixir
  # more like Pathom/EQL
  prop = :"foo.bar/baz"
  
  # more Elixir/Erlang-y?
  prop = {Foo.Bar, :baz}
  ```
  
The former looks less like Elixir, but using 2-tuples as map keys doesn't
look super Elixir-y either...

  ```elixir
  # more like Pathom/EQL
  map = %{"foo.bar/baz": :test}
  
  # more Elixir/Erlang-y???
  map = %{{Foo.Bar, :baz} => :test}
  ```
  
I (personally) don't find either of these options truly satifying...

Maybe create a Map-like substitute struct that behaves like a map, but
presents itself differently to seem more idiomatic?

> As an aside, I understand that Clojure (and I would assumes Lisps in general)
> are built intentionally with this type of thing in mind, while Erlang
> and Elixir were not, hence the friction. I still find the core idea of
> namespaced attributes + resolvers compelling enough to attempt in this
> (amazing) language regardless.

At the moment, I think I still prefer the 2-tuple option for Elixir (although
I'm open to being persuaded on this point). 

Or maybe use a more struct-based approach for query expression, rather than the
data-oriented apporoach that Clojure's EDN and EQL uses? Which would potentially
avoid this entire issue? I'm a huge fan of the whole query-as-data premise though
because it allows for 'straight-forward' query manipulation using existing core
functions.

## Roadmap

- support subqueries (i.e. ident + join + recursion)
- `Janus.Runner` + `Janus.Resolver` (i.e. batching, async, transforming, etc.)
- weight alorithm behaviour (i.e. OR branches) for `Janus.Runner`
- `Janus.Plugin` (e.g. middleware, interceptor, behaviour, walker / planner / runner lifecycle hooks)
  - `:telemetry`
  - error handling
  - tracing (can we just use a `:telemetry` handler for this?)
  - placeholders
  - caching: request, query, plan, resolver
  - complexity analysis (see https://hexdocs.pm/absinthe/complexity-analysis.html#content)
- `Janus` API (i.e. process/parse, index)
- index 
- docs and typespecs for attributes (docs for resolvers?)
- mutation
- subscriptions
- export, defer, live, stream
- dynamic resolvers / GraphQL integration
- `Inspect`
- Error Struct(s) instead of `{:error, reason :: term}`
- resolver diplomats for `Tesla`, `Ecto`, `:ets`

## Related Next-Steps

- EQL (rename?) wire format
- Janus `Plug`
- Generic Digraph `Phoenix.LiveView` `Plug`
- graphiql / graphql playground / visualization `Plug` (via `Phoenix.LiveView`)
- Janus `Phoenix` templates
