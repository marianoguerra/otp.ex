defmodule :m_user_sup do
  use Bitwise
  @behaviour :supervisor_bridge
  def start() do
    :supervisor_bridge.start_link(:user_sup, [])
  end

  def init([]) do
    case get_user() do
      :nouser ->
        :ignore

      {:master, master} ->
        pid = start_slave(master)
        {:ok, pid, pid}

      {m, f, a} ->
        case start_user(m, f, a) do
          {:ok, pid} ->
            {:ok, pid, pid}

          error ->
            error
        end
    end
  end

  defp start_slave(master) do
    case :rpc.call(master, :erlang, :whereis, [:user]) do
      user when is_pid(user) ->
        spawn(:user_sup, :relay, [user])

      _ ->
        :error_logger.error_msg('Cannot get remote user', [])

        receive do
        after
          1000 ->
            true
        end

        :erlang.halt()
    end
  end

  def relay(pid) do
    :erlang.register(:user, self())
    relay1(pid)
  end

  defp relay1(pid) do
    receive do
      x ->
        send(pid, x)
        relay1(pid)
    end
  end

  def terminate(_Reason, userPid) do
    receive do
    after
      1000 ->
        :ok
    end

    :erlang.exit(userPid, :kill)
    :ok
  end

  defp start_user(mod, func, a) do
    apply(mod, func, a)
    wait_for_user_p(100)
  end

  defp wait_for_user_p(0) do
    {:error, :nouser}
  end

  defp wait_for_user_p(n) do
    case :erlang.whereis(:user) do
      pid when is_pid(pid) ->
        :erlang.link(pid)
        {:ok, pid}

      _ ->
        receive do
        after
          100 ->
            :ok
        end

        wait_for_user_p(n - 1)
    end
  end

  defp get_user() do
    flags = :init.get_arguments()
    check_flags(flags, {:user_drv, :start, []})
  end

  defp check_flags([{:nouser, []} | t], _) do
    check_flags(t, :nouser)
  end

  defp check_flags([{:user, [user]} | t], _) do
    check_flags(t, {:erlang.list_to_atom(user), :start, []})
  end

  defp check_flags([{:noshell, []} | t], _) do
    check_flags(t, {:user, :start, []})
  end

  defp check_flags([{:oldshell, []} | t], _) do
    check_flags(t, {:user, :start, []})
  end

  defp check_flags([{:noinput, []} | t], _) do
    check_flags(t, {:user, :start_out, []})
  end

  defp check_flags([{:master, [node]} | t], _) do
    check_flags(t, {:master, :erlang.list_to_atom(node)})
  end

  defp check_flags([_H | t], user) do
    check_flags(t, user)
  end

  defp check_flags([], user) do
    user
  end
end
