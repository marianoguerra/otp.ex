defmodule :cowboy_loop do
  use Bitwise
  @behaviour :cowboy_sub_protocol
  def upgrade(req, env, handler, handlerState) do
    loop(req, env, handler, handlerState)
  end

  def upgrade(req, env, handler, handlerState, :hibernate) do
    suspend(req, env, handler, handlerState)
  end

  def loop(req = %{:pid => parent}, env, handler, handlerState) do
    receive do
      {:EXIT, ^parent, reason} ->
        terminate(req, env, handler, handlerState, reason)

      {:system, from, request} ->
        :sys.handle_system_msg(
          request,
          from,
          parent,
          :cowboy_loop,
          [],
          {req, env, handler, handlerState}
        )

      {:"$gen_call", from, call} ->
        :cowboy_children.handle_supervisor_call(call, from, [], :cowboy_loop)
        loop(req, env, handler, handlerState)

      message ->
        call(req, env, handler, handlerState, message)
    end
  end

  defp call(req0, env, handler, handlerState0, message) do
    try do
      handler.info(message, req0, handlerState0)
    catch
      class, reason ->
        :cowboy_handler.terminate({:crash, class, reason}, req0, handlerState0, handler)
        :erlang.raise(class, reason, __STACKTRACE__)
    else
      {:ok, req, handlerState} ->
        loop(req, env, handler, handlerState)

      {:ok, req, handlerState, :hibernate} ->
        suspend(req, env, handler, handlerState)

      {:stop, req, handlerState} ->
        terminate(req, env, handler, handlerState, :stop)
    end
  end

  defp suspend(req, env, handler, handlerState) do
    {:suspend, :cowboy_loop, :loop, [req, env, handler, handlerState]}
  end

  defp terminate(req, env, handler, handlerState, reason) do
    result = :cowboy_handler.terminate(reason, req, handlerState, handler)
    {:ok, req, %{env | :result => result}}
  end

  def system_continue(_, _, {req, env, handler, handlerState}) do
    loop(req, env, handler, handlerState)
  end

  def system_terminate(reason, _, _, {req, env, handler, handlerState}) do
    terminate(req, env, handler, handlerState, reason)
  end

  def system_code_change(misc, _, _, _) do
    {:ok, misc}
  end
end
