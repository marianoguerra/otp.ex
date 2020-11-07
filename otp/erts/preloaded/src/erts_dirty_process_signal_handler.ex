defmodule :m_erts_dirty_process_signal_handler do
  use Bitwise

  def start() do
    :erlang.process_flag(:trap_exit, true)
    msg_loop()
  end

  defp msg_loop() do
    _ =
      receive do
        request ->
          try do
            handle_request(request)
          catch
            _, _ ->
              :ok
          end
      end

    msg_loop()
  end

  defp handle_request(pid) when is_pid(pid) do
    handle_incoming_signals(pid, 0)
  end

  defp handle_request({requester, target, prio, {sysTaskOp, reqId, arg} = op} = request) do
    case handle_sys_task(requester, target, sysTaskOp, reqId, arg, 0) do
      :done ->
        :ok

      :busy ->
        send(self(), request)

      :normal ->
        case :erts_internal.request_system_task(requester, target, prio, op) do
          :ok ->
            :ok

          :dirty_execution ->
            handle_request(request)
        end
    end
  end

  defp handle_request(_Garbage) do
    :ignore
  end

  defp handle_incoming_signals(pid, 5) do
    send(self(), pid)
  end

  defp handle_incoming_signals(pid, n) do
    case :erts_internal.dirty_process_handle_signals(pid) do
      :more ->
        handle_incoming_signals(pid, n + 1)

      _Res ->
        :ok
    end
  end

  defp handle_sys_task(requester, target, :check_process_code, reqId, module, n) do
    case :erts_internal.check_dirty_process_code(
           target,
           module
         ) do
      bool when bool == true or bool == false ->
        send(requester, {:check_process_code, reqId, bool})
        :done

      :busy ->
        case n > 5 do
          true ->
            :busy

          false ->
            handle_sys_task(requester, target, :check_process_code, reqId, module, n + 1)
        end

      res ->
        res
    end
  end
end
