defmodule Interceptor do
  @moduledoc """
  See Eric Normand's LispCast Blog article:
  [A Model of Interceptors](https://lispcast.com/a-model-of-interceptors/)
  and the [interceptor](https://github.com/exoscale/interceptor)
  library as inspirations.

  The idea of interceptors is similar to that of middleware, they define
  the transform-on-the-way-in (`:enter` function) and transform-on-the-way-out
  (`:leave` function) as two separate operations. They basically reify the
  two uses of middleware into a distinct object.

  ## Interceptor Pattern

  `execute/2` takes a context (map) and chains it to the interceptors
  that can modify it and ultimately returns the modified context.

  An interceptor contains the `:enter` function and optionally the `:leave`
  and/or `:error` functions. The `:enter` and `:leave` functions take
  the context as input and return it as output, while `:error` takes
  the context and the error that triggered it and potentially returns
  and new context.

  When executed, it will pass the context to the `:enter` handlers in
  order, then to all the `:leave` handlers in reverse.

  Something like this:

      enter A -> enter B -> enter C -> leave C -> leave B -> leave A
  """

  use Boundary, deps: [], exports: []
  alias Interceptor.{Coercible, Hook, Queue, Stage}

  @type t :: Coercible.t()
  @type err :: Exception.t() | (reason :: term)
  @type stage :: :enter | :leave | {:error, err}
  @type ctx :: %{
          :__queue__ => Queue.t() | nil,
          :__stage__ => stage | nil,
          optional(any) => any
        }

  @spec execute(ctx) :: {:ok, ctx} | {:error, err}
  def execute(ctx) do
    ctx.__queue__
    |> Enum.reduce(ctx, &Hook.invoke_with_hooks(&2, Coercible.coerce(&1)))
    |> case do
      %{__stage__: {:error, reason}} -> {:error, reason}
      ctx -> {:ok, ctx}
    end
  end

  @spec execute(ctx, [t]) :: {:ok, ctx} | {:error, err}
  def execute(ctx, interceptors) do
    ctx
    |> start()
    |> enqueue(interceptors)
    |> execute()
  end

  @spec stage(ctx, Stage.stage_fun() | nil, Stage.stage_fun() | nil, Stage.stage_fun() | nil) ::
          ctx
  def stage(ctx, enter, leave \\ nil, error \\ nil) do
    enqueue(ctx, [Stage.new(enter, leave, error)])
  end

  @spec start(map) :: ctx
  def start(ctx) do
    Map.merge(
      %{
        __queue__: Queue.new(),
        __stage__: :enter
      },
      ctx
    )
  end

  @spec error(ctx, err) :: ctx
  def error(ctx, err) do
    Map.put(ctx, :__stage__, {:error, err})
  end

  @spec terminate(ctx) :: ctx
  def terminate(ctx) do
    Map.update(
      ctx,
      :__queue__,
      Queue.new(),
      &Queue.terminate(&1)
    )
  end

  @spec halt(ctx) :: ctx
  def halt(ctx) do
    Map.update(
      ctx,
      :__queue__,
      Queue.new(),
      &Queue.halt(&1)
    )
  end

  @spec enqueue(ctx, [t]) :: ctx
  def enqueue(ctx, interceptors) do
    Map.update(
      ctx,
      :__queue__,
      Queue.new(interceptors),
      &Queue.enqueue(&1, interceptors)
    )
  end

  @spec transform(Stage.stage_fun(), (ctx, any -> ctx)) :: Stage.stage_fun()
  def transform(f, g) do
    fn ctx ->
      g.(ctx, f.(ctx))
    end
  end

  @spec take_in(Stage.stage_fun(), path :: [term, ...]) :: Stage.stage_fun()
  def take_in(f, path) do
    fn ctx ->
      f.(get_in(ctx, path))
    end
  end

  @spec return_at(Stage.stage_fun(), path :: [term, ...]) :: Stage.stage_fun()
  def return_at(f, path) do
    transform(f, &put_in(&1, path, &2))
  end

  @spec whenever(Stage.stage_fun(), (ctx -> boolean)) :: Stage.stage_fun()
  def whenever(f, pred) do
    fn ctx ->
      if pred.(ctx) do
        f.(ctx)
      else
        ctx
      end
    end
  end

  @spec lens(Stage.stage_fun(), path :: [term, ...]) :: Stage.stage_fun()
  def lens(f, path) do
    f
    |> take_in(path)
    |> return_at(path)
  end

  @spec discard(Stage.stage_fun()) :: Stage.stage_fun()
  def discard(f) do
    transform(f, fn ctx, _ -> ctx end)
  end
end
