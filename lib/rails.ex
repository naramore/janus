defmodule Rails do
  @moduledoc """
  Partial Railway Oriented Programming implementation in Elixir.

  See this [blog article](https://fsharpforfunandprofit.com/rop/)
  for more details.
  """

  use Boundary, deps: [], exports: []
  require Logger

  @type success(x) :: {:ok, x}
  @type success :: success(any)
  @type failure(reason) :: {:error, reason}
  @type failure :: failure(any)
  @type two_track(x, reason) :: success(x) | failure(reason)
  @type two_track(x) :: two_track(x, any)
  @type two_track_fun(a, b, c, d) :: (two_track(a, c) -> two_track(b, d))
  @type two_track_fun(a, b) :: two_track_fun(a, b, any, any)
  @type switch_fun(a, b) :: (a -> two_track(b))
  @type chardata_or_fun :: Logger.message() | (() -> Logger.message())

  @spec success(a) :: success(a) when a: any
  def success(a), do: {:ok, a}

  @spec failure(reason) :: failure(reason) when reason: term
  def failure(reason), do: {:error, reason}

  @spec switch((a -> b)) :: switch_fun(a, b) when a: any, b: any
  def switch(f), do: &{:ok, f.(&1)}

  @spec bind(switch_fun(a, b)) :: two_track_fun(a, b) when a: any, b: any
  def bind(f), do: &bind(&1, f)

  @spec bind(two_track(a), switch_fun(a, b)) :: two_track(b) when a: any, b: any
  def bind({:error, reason}, _), do: {:error, reason}
  def bind({:ok, a}, f), do: f.(a)

  @spec map((a -> b)) :: two_track_fun(a, b) when a: any, b: any
  def map(f), do: &map(&1, f)

  @spec map(two_track(a), (a -> b)) :: two_track(b) when a: any, b: any
  def map({:error, reason}, _), do: {:error, reason}
  def map({:ok, a}, f), do: {:ok, f.(a)}

  @spec tee((a -> :ok)) :: (a -> a) when a: any
  def tee(f), do: &tee(&1, f)

  @spec tee(a, (a -> :ok)) :: a when a: any
  def tee(a, f) do
    _ = f.(a)
    a
  end

  @spec safe((a -> b | no_return)) :: switch_fun(a, b) when a: any, b: any
  def safe(f), do: &safe(&1, f)

  @spec safe(a, (a -> b | no_return)) :: two_track(b) when a: any, b: any
  def safe(a, f) do
    f.(a)
  rescue
    reason -> {:error, reason}
  catch
    :exit, reason -> {:error, {:exit, reason}}
    x -> {:error, {:caught, x}}
  end

  @spec supervise((a -> b), (c -> d)) :: two_track_fun(a, b, c, d)
        when a: any, b: any, c: any, d: any
  def supervise(on_success, on_fail), do: &supervise(&1, on_success, on_fail)

  @spec supervise(two_track(a, c), (a -> b), (c -> d)) :: two_track(b, d)
        when a: any, b: any, c: any, d: any
  def supervise({:error, reason}, _on_success, on_fail), do: map({:error, reason}, on_fail)
  def supervise({:ok, a}, on_success, _on_fail), do: map({:ok, a}, on_success)

  @spec curry((... -> any), [...]) :: (any -> any)
  def curry(f, _) when is_function(f, 0), do: fn _ -> f.() end
  def curry(f, _) when is_function(f, 1), do: f
  def curry(f, args) when is_function(f), do: &apply(f, [&1 | args])

  @spec log(two_track(a), (a -> chardata_or_fun), keyword) :: two_track(a) when a: any
  def log(two_track, message_fun, metadata \\ []) do
    supervise(
      two_track,
      tee(&Logger.info(message_fun.(&1), metadata)),
      tee(&Logger.error(fn -> error_message(&1) end, metadata))
    )
  end

  @spec error_message(reason :: term) :: Logger.message()
  def error_message(%{__exception__: true} = error) do
    Exception.format(:error, error, [])
  end

  def error_message(reason) do
    case String.Chars.impl_for(reason) do
      nil -> inspect(reason)
      otherwise -> to_string(otherwise)
    end
  end
end
