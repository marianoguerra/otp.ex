defmodule :cowboy_handler do
  use Bitwise
  @behaviour :cowboy_middleware
  def execute(
        req,
        env = %{:handler => handler, :handler_opts => handlerOpts}
      ) do
    try do
      handler.init(req, handlerOpts)
    catch
      class, reason ->
        terminate({:crash, class, reason}, req, handlerOpts, handler)
        :erlang.raise(class, reason, __STACKTRACE__)
    else
      {:ok, req2, state} ->
        result = terminate(:normal, req2, state, handler)
        {:ok, req2, %{env | :result => result}}

      {mod, req2, state} ->
        mod.upgrade(req2, env, handler, state)

      {mod, req2, state, opts} ->
        mod.upgrade(req2, env, handler, state, opts)
    end
  end

  def terminate(reason, req, state, handler) do
    case :erlang.function_exported(handler, :terminate, 3) do
      true ->
        handler.terminate(reason, req, state)

      false ->
        :ok
    end
  end
end
