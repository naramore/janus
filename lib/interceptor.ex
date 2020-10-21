defmodule Interceptor do
  @moduledoc false

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

defprotocol Interceptor.Invokable do
  @moduledoc false

  @spec invoke(t, Interceptor.ctx()) :: Interceptor.ctx()
  def invoke(interceptor, ctx)
end

defprotocol Interceptor.Coercible do
  @moduledoc false

  alias Interceptor.{Invokable, Queue}

  @fallback_to_any true

  @spec coerce(t) :: Invokable.t() | Queue.t()
  def coerce(interceptor)
end

defmodule Interceptor.Stage do
  @moduledoc false

  defstruct enter: nil,
            leave: nil,
            error: nil

  @type t :: %__MODULE__{
          enter: stage_fun,
          leave: stage_fun,
          error: error_fun
        }

  @type stage_fun :: (Interceptor.ctx() -> Interceptor.ctx())

  @type error_fun :: (Interceptor.ctx(), Interceptor.err() -> Interceptor.ctx())

  @type opt ::
          {:enter, stage_fun | nil}
          | {:leave, stage_fun | nil}
          | {:error, error_fun | nil}

  @callback enter(Interceptor.ctx()) :: Interceptor.ctx()

  @callback leave(Interceptor.ctx()) :: Interceptor.ctx()

  @callback error(Interceptor.ctx(), Interceptor.err()) :: Interceptor.ctx()

  @spec new([opt]) :: t
  def new(opts \\ []) do
    new(
      Keyword.get(opts, :enter),
      Keyword.get(opts, :leave),
      Keyword.get(opts, :error)
    )
  end

  @spec new(stage_fun | nil, stage_fun | nil, error_fun | nil) :: t
  def new(enter, leave, error) do
    %__MODULE__{
      enter: enter,
      leave: leave,
      error: error
    }
  end

  @doc false
  defmacro __using__(_opts) do
    quote do
      @behaviour Interceptor.Stage

      @impl Interceptor.Stage
      def enter(ctx), do: ctx

      @impl Interceptor.Stage
      def leave(ctx), do: ctx

      @impl Interceptor.Stage
      def error(ctx, err),
        do: Interceptor.error(ctx, err)

      defoverridable enter: 1, leave: 1, error: 2
    end
  end

  defimpl Interceptor.Invokable do
    @impl @protocol
    def invoke(%{__stage__: {:error, err}} = ctx, interceptor) do
      if is_function(interceptor.error, 2) do
        safe_invoke(interceptor.error, [Map.put(ctx, :__stage__, :leave), err])
      else
        ctx
      end
    end

    def invoke(%{__stage__: stage} = ctx, interceptor) do
      case Map.get(interceptor, stage) do
        f when is_function(f, 1) ->
          safe_invoke(f, [ctx])

        _ ->
          ctx
      end
    end

    @spec safe_invoke(fun, [any, ...]) :: Interceptor.ctx()
    defp safe_invoke(f, [ctx | _] = args) do
      apply(f, args)
    rescue
      e -> Interceptor.error(ctx, e)
    catch
      :exit, reason -> Interceptor.error(ctx, {:exit, reason})
      x -> Interceptor.error(ctx, {:caught, x})
    end
  end

  defimpl Interceptor.Coercible do
    @impl @protocol
    def coerce(stage) do
      stage
    end
  end
end

defimpl Interceptor.Coercible, for: Function do
  alias Interceptor.Stage

  @impl @protocol
  def coerce(f) do
    Stage.new(f, nil, nil)
  end
end

defimpl Interceptor.Coercible, for: Map do
  alias Interceptor.Stage

  @impl @protocol
  def coerce(interceptor) do
    interceptor
    |> Enum.into([])
    |> Stage.new()
  end
end

defimpl Interceptor.Coercible, for: Atom do
  alias Interceptor.Stage

  @impl @protocol
  def coerce(interceptor) do
    if Code.ensure_loaded?(interceptor) do
      Stage.new(
        enter: &interceptor.enter/1,
        leave: &interceptor.leave/1,
        error: &interceptor.error/2
      )
    else
      Stage.new([])
    end
  end
end

defimpl Interceptor.Coercible, for: List do
  alias Interceptor.{Invokable, Queue}

  @impl @protocol
  def coerce(list) do
    Queue.new(coerce_impl(list))
  end

  @spec coerce_impl([list | any]) :: [Invokable.t() | Queue.t()]
  defp coerce_impl([]), do: []
  defp coerce_impl([h | t]), do: [@protocol.coerce(h) | coerce_impl(t)]
end

defimpl Interceptor.Coercible, for: Any do
  alias Interceptor.Stage

  @impl @protocol
  def coerce(_) do
    Stage.new(nil, nil, nil)
  end
end

defmodule Interceptor.Hook do
  @moduledoc false

  alias Interceptor.Invokable

  @type t ::
          module
          | {:pre_hook, hook_fun}
          | {:post_hook, hook_fun}
          | %{
              optional(:pre_hook) => hook_fun,
              optional(:post_hook) => hook_fun
            }

  @type hook_fun :: (Interceptor.stage(), Interceptor.t(), Interceptor.ctx() -> Interceptor.ctx())

  @callback pre_hook(Interceptor.stage(), Interceptor.t(), Interceptor.ctx()) :: Interceptor.ctx()
  @callback post_hook(Interceptor.stage(), Interceptor.t(), Interceptor.ctx()) ::
              Interceptor.ctx()

  @spec invoke_with_hooks(Interceptor.ctx(), Interceptor.t()) :: Interceptor.ctx()
  def invoke_with_hooks(ctx, interceptor) do
    invoke_with_hooks(ctx, interceptor, Map.get(ctx, :__hooks__, []))
  end

  @spec invoke_with_hooks(Interceptor.ctx(), Interceptor.t(), [t]) :: Interceptor.ctx()
  def invoke_with_hooks(ctx, interceptor, hooks) do
    ctx
    |> apply_hooks(interceptor, :pre_hook, hooks)
    |> (&Invokable.invoke(interceptor, &1)).()
    |> apply_hooks(interceptor, :post_hook, hooks)
  end

  @spec apply_hooks(Interceptor.ctx(), Interceptor.t(), :pre_hook | :post_hook, [t]) ::
          Interceptor.ctx()
  defp apply_hooks(ctx, _interceptor, _key, []), do: ctx

  defp apply_hooks(ctx, interceptor, key, [h | hs]) do
    ctx
    |> apply_hook(interceptor, key, h)
    |> apply_hooks(interceptor, key, hs)
  end

  @spec apply_hook(Interceptor.ctx(), Interceptor.t(), :pre_hook | :post_hook, t) ::
          Interceptor.ctx()
  defp apply_hook(ctx, interceptor, key, hook) do
    case extract_hook(key, hook) do
      nil ->
        ctx

      hook ->
        run_hook(ctx, interceptor, hook)
    end
  end

  @spec extract_hook(:pre_hook | :post_hook, t) :: hook_fun | nil
  defp extract_hook(:pre_hook, module) when is_atom(module), do: &module.pre_hook/3
  defp extract_hook(:post_hook, module) when is_atom(module), do: &module.post_hook/3
  defp extract_hook(:pre_hook, %{pre_hook: hook}), do: hook
  defp extract_hook(:post_hook, %{post_hook: hook}), do: hook
  defp extract_hook(key, {key, hook}), do: hook
  defp extract_hook(_key, _hook), do: nil

  @spec run_hook(Interceptor.ctx(), Interceptor.t(), hook_fun) :: Interceptor.ctx()
  defp run_hook(%{__stage__: stg} = ctx, i, hook) when is_function(hook, 3) do
    hook.(stg, i, ctx)
  end

  defp run_hook(ctx, _, _), do: ctx

  @doc false
  defmacro __using__(_opts) do
    quote do
      @behaviour Interceptor.Hook

      @impl Interceptor.Hook
      def pre_hook(_stage, _interceptor, ctx), do: ctx

      @impl Interceptor.Hook
      def post_hook(_stage, _interceptor, ctx), do: ctx

      defoverridable pre_hook: 3, post_hook: 3
    end
  end
end

defmodule Interceptor.Queue do
  @moduledoc false

  alias Interceptor.Coercible

  defstruct queue: :queue.new(),
            stack: []

  @type t :: %__MODULE__{
          queue: :queue.queue(),
          stack: list
        }

  @type direction :: :forwards | :backwards

  @spec new(list) :: t
  def new(items \\ []) do
    %__MODULE__{
      queue: :queue.from_list(items)
    }
  end

  @spec terminate(t) :: t
  def terminate(%{stack: [%__MODULE__{} = iqq | s]} = iq) do
    terminate(%{iq | stack: [terminate(iqq) | s]})
  end

  def terminate(iq) do
    %{iq | queue: :queue.new()}
  end

  @spec halt(t) :: t
  def halt(queue) do
    %{queue | queue: :queue.new(), stack: []}
  end

  @spec enqueue(t, [Coercible.t()]) :: t
  def enqueue(queue, interceptors) do
    Map.update(
      queue,
      :queue,
      :queue.from_list(interceptors),
      &enqueue_impl(&1, interceptors)
    )
  end

  @spec pop(t, direction) :: {any, t} | :empty
  def pop(queue, direction \\ :forwards)

  def pop(%{queue: q, stack: [%__MODULE__{} = iqq | s]} = iq, dir) do
    case {pop(iqq, dir), dir} do
      {{x, iqq}, _} ->
        {x, %{iq | stack: [iqq | s]}}

      {:empty, :backwards} ->
        pop(%{iq | queue: :queue.cons(iqq, q), stack: s}, dir)

      {:empty, :forwards} ->
        pop_next(iq, :forwards)
    end
  end

  def pop(iq, dir) do
    pop_next(iq, dir)
  end

  @spec pop_next(t, direction) :: {any, t} | :empty
  defp pop_next(iq, dir) do
    case pop_impl(iq, dir) do
      {%__MODULE__{}, iq} ->
        pop(iq, dir)

      {_, %{stack: [x | s]} = iq} when is_list(x) ->
        pop(%{iq | stack: [new(x) | s]}, dir)

      otherwise ->
        otherwise
    end
  end

  @spec pop_impl(t, direction) :: {any, t} | :empty
  defp pop_impl(%{queue: q, stack: s} = iq, :forwards) do
    case :queue.out(q) do
      {{:value, x}, q} -> {x, %{iq | queue: q, stack: [x | s]}}
      _ -> :empty
    end
  end

  defp pop_impl(%{queue: q, stack: [x | xs]} = iq, :backwards) do
    {x, %{iq | queue: :queue.cons(x, q), stack: xs}}
  end

  defp pop_impl(%{stack: []}, :backwards), do: :empty

  @spec enqueue_impl(:queue.queue() | nil, [Interceptor.t()]) :: :queue.queue()
  defp enqueue_impl(nil, interceptors) do
    enqueue_impl(:queue.new(), interceptors)
  end

  defp enqueue_impl(queue, interceptors) do
    Enum.reduce(interceptors, queue, fn i, q ->
      :queue.in(i, q)
    end)
  end

  defimpl Enumerable do
    @impl @protocol
    def count(%{queue: q, stack: s}) do
      {:ok, :queue.len(q) + length(s)}
    end

    @impl @protocol
    def member?(%{queue: q, stack: s}, item) do
      {:ok, :queue.member(item, q) or item in s}
    end

    @impl @protocol
    def reduce(_queue, {:halt, acc}, _fun), do: {:halted, acc}
    def reduce(queue, {:suspend, acc}, fun), do: {:suspended, acc, &reduce(queue, &1, fun)}

    def reduce(queue, {:cont, acc}, fun) do
      {acc, dir} = extract_direction(acc)

      case {@for.pop(queue, dir), dir} do
        {{x, queue}, _} ->
          reduce(queue, inject_direction(fun.(x, acc), dir), fun)

        {:empty, :forwards} ->
          reduce(queue, inject_direction({:cont, acc}, :backwards, true), fun)

        {:empty, :backwards} ->
          {:done, acc}
      end
    end

    @impl @protocol
    def slice(_queue) do
      {:error, @for}
    end

    @spec extract_direction(any) :: {any, @for.direction}
    defp extract_direction(%{__stage__: :enter} = ctx), do: {ctx, :forwards}
    defp extract_direction(%{__stage__: _} = ctx), do: {ctx, :backwards}

    defp extract_direction([acc | dir])
         when dir in [:forwards, :backwards],
         do: {acc, dir}

    defp extract_direction(acc), do: {acc, :forwards}

    @spec inject_direction(@protocol.acc, @for.direction, boolean) :: @protocol.acc
    defp inject_direction(acc, direction, overwrite? \\ false)

    defp inject_direction({type, %{__stage__: _} = ctx}, dir, true),
      do: {type, %{ctx | __stage__: to_stage(dir)}}

    defp inject_direction({_, %{__stage__: _}} = acc, _, false), do: acc
    defp inject_direction({type, acc}, dir, _), do: {type, [acc | dir]}

    @spec to_stage(@for.direction) :: Interceptor.stage()
    defp to_stage(:forwards), do: :enter
    defp to_stage(:backwards), do: :leave
  end

  defimpl Interceptor.Coercible do
    alias Interceptor.Invokable

    @impl @protocol
    def coerce(%{queue: q, stack: s} = iq) do
      %{iq | queue: :queue.from_list(coerce_impl(:queue.to_list(q))), stack: coerce_impl(s)}
    end

    @spec coerce_impl([@protocol.t]) :: [Invokable.t() | @for.t]
    defp coerce_impl([]), do: []
    defp coerce_impl([h | t]), do: [@protocol.coerce(h) | coerce_impl(t)]
  end
end
