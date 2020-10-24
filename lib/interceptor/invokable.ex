defprotocol Interceptor.Invokable do
  @moduledoc false

  @spec invoke(t, Interceptor.ctx()) :: Interceptor.ctx()
  def invoke(interceptor, ctx)
end
