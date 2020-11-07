defmodule :m_kernel_config do
  use Bitwise
  @behaviour :gen_server
  def start_link() do
    :gen_server.start_link(:kernel_config, [], [])
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)

    case sync_nodes() do
      :ok ->
        case :erlang.whereis(:dist_ac) do
          dAC when is_pid(dAC) ->
            send(dAC, {:go, self()})

            receive do
              :dist_ac_took_control ->
                :ok
            end

          _ ->
            :ok
        end

        {:ok, []}

      {:error, error} ->
        {:stop, error}
    end
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def handle_call(:__not_used, _From, state) do
    {:reply, :ok, state}
  end

  def handle_cast(:__not_used, state) do
    {:noreply, state}
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp sync_nodes() do
    case (try do
            get_sync_data()
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, reason} = error ->
        :error_logger.format('~tp', [reason])
        error

      {:infinity, mandatoryNodes, optionalNodes} ->
        case wait_nodes(mandatoryNodes, optionalNodes) do
          :ok ->
            :ok

          error ->
            error
        end

      {timeout, mandatoryNodes, optionalNodes} ->
        spawn_link(:kernel_config, :send_timeout, [timeout, self()])

        case wait_nodes(mandatoryNodes, optionalNodes) do
          :ok ->
            :ok

          error ->
            error
        end

      :undefined ->
        :ok
    end
  end

  def send_timeout(timeout, pid) do
    receive do
    after
      timeout ->
        send(pid, :timeout)
    end
  end

  defp wait_nodes(mandatory, optional) do
    :ok = :net_kernel.monitor_nodes(true)

    :lists.foreach(
      fn node ->
        case :net_adm.ping(node) do
          :pong ->
            send(self(), {:nodeup, node})

          _ ->
            :ok
        end
      end,
      mandatory ++ optional
    )

    r = rec_nodes(mandatory, optional)
    :ok = :net_kernel.monitor_nodes(false)
    r
  end

  defp rec_nodes([], []) do
    :ok
  end

  defp rec_nodes(mandatory, optional) do
    receive do
      {:nodeup, node} ->
        check_up(node, mandatory, optional)

      :timeout when mandatory === [] ->
        :ok

      :timeout ->
        {:error, {:mandatory_nodes_down, mandatory}}
    end
  end

  defp check_up(node, mandatory, optional) do
    case :lists.member(node, mandatory) do
      true ->
        rec_nodes(:lists.delete(node, mandatory), optional)

      false ->
        case :lists.member(node, optional) do
          true ->
            rec_nodes(mandatory, :lists.delete(node, optional))

          false ->
            rec_nodes(mandatory, optional)
        end
    end
  end

  defp get_sync_data() do
    timeout = get_sync_timeout()
    mandatoryNodes = get_sync_mandatory_nodes()
    optionalNodes = get_sync_optional_nodes()
    {timeout, mandatoryNodes, optionalNodes}
  end

  defp get_sync_timeout() do
    case :application.get_env(:sync_nodes_timeout) do
      {:ok, timeout}
      when is_integer(timeout) and
             timeout > 0 ->
        timeout

      {:ok, :infinity} ->
        :infinity

      :undefined ->
        throw(:undefined)

      {:ok, else__} ->
        throw({:error, {:badopt, {:sync_nodes_timeout, else__}}})
    end
  end

  defp get_sync_mandatory_nodes() do
    case :application.get_env(:sync_nodes_mandatory) do
      {:ok, nodes} when is_list(nodes) ->
        nodes

      :undefined ->
        []

      {:ok, else__} ->
        throw({:error, {:badopt, {:sync_nodes_mandatory, else__}}})
    end
  end

  defp get_sync_optional_nodes() do
    case :application.get_env(:sync_nodes_optional) do
      {:ok, nodes} when is_list(nodes) ->
        nodes

      :undefined ->
        []

      {:ok, else__} ->
        throw({:error, {:badopt, {:sync_nodes_optional, else__}}})
    end
  end
end
