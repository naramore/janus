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
