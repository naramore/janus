defmodule Janus.Utils do
  @moduledoc false
  import Kernel, except: [inspect: 1, inspect: 2]
  require Logger

  @spec inspect_opts() :: keyword
  def inspect_opts,
    do: [pretty: true, structs: false, limit: :infinity, printable_limit: :infinity]

  @spec inspect(term, keyword) :: Inspect.t()
  def inspect(term, opts \\ inspect_opts()) do
    Kernel.inspect(term, opts)
  end

  @spec to_shape_descriptor([x]) :: Janus.shape_descriptor(x) when x: any
  def to_shape_descriptor(terms) do
    terms
    |> Enum.zip(Stream.repeatedly(fn -> %{} end))
    |> Enum.into(%{})
  end

  @spec trace(Janus.Graph.edge(), term) :: :ok
  def trace({_, i, o, %{id: id}}, term) do
    Logger.debug(
      "#{Kernel.inspect(short_form(id))}[#{Kernel.inspect(short_form(o))} <- #{
        Kernel.inspect(short_form(i))
      }] :: #{Kernel.inspect(short_form(term))}"
    )
  end

  def trace(edge, term) do
    Logger.warn(
      "unknown edge format: #{Kernel.inspect(edge)} :: #{Kernel.inspect(short_form(term))}"
    )
  end

  def short_form([]), do: []
  def short_form([h | t]), do: [short_form(h) | short_form(t)]
  def short_form({:and, ids}), do: {:and, short_form(ids)}
  def short_form({:and, ids, id}), do: {:and, short_form(ids), short_form(id)}
  def short_form({_, id}), do: id
  def short_form(id), do: id

  @spec cond_pipe(x, boolean | (x -> boolean), (x -> y)) :: y when x: any, y: any
  def cond_pipe(x, true, fun), do: fun.(x)
  def cond_pipe(x, false, _), do: x

  def cond_pipe(x, pred, fun) do
    if pred.(x) do
      fun.(x)
    else
      x
    end
  end
end
