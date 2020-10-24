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
