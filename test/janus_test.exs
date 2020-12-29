defmodule JanusTest do
  use ExUnit.Case
  use ExUnitProperties
  doctest Janus
end

defmodule Janus.GraphTest do
  use ExUnit.Case
  use ExUnitProperties
  doctest Janus.Graph
end

defmodule Janus.PlanTest do
  use ExUnit.Case
  use ExUnitProperties
  doctest Janus.Plan
end

defmodule Janus.PlannerTest do
  use ExUnit.Case
  use ExUnitProperties
  doctest Janus.Planner
end

defmodule Janus.PluginTest do
  use ExUnit.Case
  use ExUnitProperties
  doctest Janus.Plugin
end

defmodule Janus.ResolverTest do
  use ExUnit.Case
  use ExUnitProperties
  doctest Janus.Resolver
end

defmodule Janus.ProcessorTest do
  use ExUnit.Case
  use ExUnitProperties
  doctest Janus.Processor
end

defmodule Janus.UtilsTest do
  use ExUnit.Case
  use ExUnitProperties
  doctest Janus.Utils
end
