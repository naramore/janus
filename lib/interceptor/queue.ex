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
