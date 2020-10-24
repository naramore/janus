defprotocol Interceptor.Coercible do
  @moduledoc false

  alias Interceptor.{Invokable, Queue}

  @fallback_to_any true

  @spec coerce(t) :: Invokable.t() | Queue.t()
  def coerce(interceptor)
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
