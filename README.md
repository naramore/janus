> **HIGHLY EXPERIMENTAL AND IN ACTIVE DEVELOPEMENT: USE AT YOUR OWN RISK**

# Janus

> Roman God of beginnings, gates, transitions, time,
> duality, doorways, passages, and ending
>
> [Wikipedia](https://en.wikipedia.org/wiki/Janus)

Basically, Pathom in Elixir. See the following for more details:
- [The Maximal Graph](https://www.youtube.com/watch?v=IS3i3DTUnAI)
- [Pathom 3 is Comming](https://blog.wsscode.com/pathom-3-is-coming/)
- [Pathom 3 GitHub](https://github.com/wilkerlucio/pathom3)
- [Pathom GitHub](https://github.com/wilkerlucio/pathom)
- [EDN Query Language (EQL)](https://github.com/edn-query-language/eql)
- [Extensible Data Notation (EDN)](https://github.com/edn-format/edn)
- [Tranist](https://github.com/cognitect/transit-cljs)

## Roadmap

- support subqueries (i.e. ident + join + recursion)
- `Janus.Runner` + `Janus.Resolver` (i.e. batching, async, transforming, etc.)
- weight alorithm behaviour (i.e. OR branches) for `Janus.Runner`
- `Janus` API (i.e. process/parse, index)
- `Janus.Plugin` (e.g. middleware, interceptor, behaviour, walker / planner / runner lifecycle hooks)
  - `:telemetry`
  - error handling
  - tracing (can we just use a `:telemetry` handler for this?)
  - placeholders
  - caching: request, query, plan, resolver
  - complexity analysis (see https://hexdocs.pm/absinthe/complexity-analysis.html#content)
- index 
- docs and typespecs for attributes (docs for resolvers?)
- mutation
- subscriptions
- export, defer, live, stream
- dynamic resolvers / GraphQL integration
- `Inspect`

### Misc

- support alternative property expressions (e.g. formatted atoms, strings)
- add checks to Janus.Graph.from_resolvers/1 for resolver uniqueness
- EQL + Janus.Plugin -> add variables? (i.e. pattern matching some/all of a query, and able to 'use' that within subsequent parts of the query)

## Related Next-Steps

- EQL (rename?) wire format
- Janus `Plug`
- Generic Digraph `Phoenix.LiveView` `Plug`
- graphiql / graphql playground / visualization `Plug` (via `Phoenix.LiveView`)
- Janus `Phoenix` templates
