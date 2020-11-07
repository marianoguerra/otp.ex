defmodule :m_global_search do
  use Bitwise

  def start(flag, arg) do
    case flag do
      :send ->
        spawn_link(:global_search, :init_send, [arg])

      :whereis ->
        spawn_link(:global_search, :init_whereis, [arg])

      :names ->
        spawn_link(:global_search, :init_names, [arg])

      :send_test ->
        spawn_link(:global_search, :send_test, [arg])

      :whereis_test ->
        spawn_link(:global_search, :whereis_test, [arg])

      :names_test ->
        spawn_link(:global_search, :names_test, [arg])
    end
  end

  def init_send({:any, nodesList, name, msg, from}) do
    case whereis_any_loop(nodesList, name) do
      :undefined ->
        res = {:badarg, {name, msg}}

        :gen_server.cast(
          :global_group,
          {:send_res, res, name, msg, self(), from}
        )

      pid ->
        :gen_server.cast(
          :global_group,
          {:send_res, pid, name, msg, self(), from}
        )
    end

    end_loop()
  end

  def init_send({:group, nodes, name, msg, from}) do
    case whereis_group_loop(nodes, name) do
      :group_down ->
        res = {:badarg, {name, msg}}

        :gen_server.cast(
          :global_group,
          {:send_res, res, name, msg, self(), from}
        )

      :undefined ->
        res = {:badarg, {name, msg}}

        :gen_server.cast(
          :global_group,
          {:send_res, res, name, msg, self(), from}
        )

      pid ->
        :gen_server.cast(
          :global_group,
          {:send_res, pid, name, msg, self(), from}
        )
    end

    end_loop()
  end

  def init_send({:node, node, name, msg, from}) do
    case whereis_check_node(node, name) do
      :node_down ->
        res = {:badarg, {name, msg}}

        :gen_server.cast(
          :global_group,
          {:send_res, res, name, msg, self(), from}
        )

      :undefined ->
        res = {:badarg, {name, msg}}

        :gen_server.cast(
          :global_group,
          {:send_res, res, name, msg, self(), from}
        )

      pid ->
        :gen_server.cast(
          :global_group,
          {:send_res, pid, name, msg, self(), from}
        )
    end

    end_loop()
  end

  def init_whereis({:any, nodesList, name, from}) do
    r = whereis_any_loop(nodesList, name)

    :gen_server.cast(
      :global_group,
      {:find_name_res, r, self(), from}
    )

    end_loop()
  end

  def init_whereis({:group, nodes, name, from}) do
    case whereis_group_loop(nodes, name) do
      :group_down ->
        :gen_server.cast(
          :global_group,
          {:find_name_res, :undefined, self(), from}
        )

      r ->
        :gen_server.cast(
          :global_group,
          {:find_name_res, r, self(), from}
        )
    end

    end_loop()
  end

  def init_whereis({:node, node, name, from}) do
    case whereis_check_node(node, name) do
      :node_down ->
        :gen_server.cast(
          :global_group,
          {:find_name_res, :undefined, self(), from}
        )

      r ->
        :gen_server.cast(
          :global_group,
          {:find_name_res, r, self(), from}
        )
    end

    end_loop()
  end

  def init_names({:group, nodes, from}) do
    case names_group_loop(nodes) do
      :group_down ->
        :gen_server.cast(
          :global_group,
          {:registered_names_res, [], self(), from}
        )

      r ->
        :gen_server.cast(
          :global_group,
          {:registered_names_res, r, self(), from}
        )
    end

    end_loop()
  end

  def init_names({:node, node, from}) do
    case names_check_node(node) do
      :node_down ->
        :gen_server.cast(
          :global_group,
          {:registered_names_res, [], self(), from}
        )

      r ->
        :gen_server.cast(
          :global_group,
          {:registered_names_res, r, self(), from}
        )
    end

    end_loop()
  end

  defp end_loop() do
    receive do
      :kill ->
        exit(:normal)
    end
  end

  defp whereis_any_loop([], _Name) do
    :undefined
  end

  defp whereis_any_loop([{_Group_name, nodes} | t], name) do
    case whereis_group_loop(nodes, name) do
      :group_down ->
        whereis_any_loop(t, name)

      :undefined ->
        whereis_any_loop(t, name)

      r ->
        r
    end
  end

  defp whereis_group_loop([], _Name) do
    :group_down
  end

  defp whereis_group_loop([node | t], name) do
    case whereis_check_node(node, name) do
      :node_down ->
        whereis_group_loop(t, name)

      r ->
        r
    end
  end

  defp whereis_check_node(node, name) do
    case :net_adm.ping(node) do
      :pang ->
        :node_down

      :pong ->
        :erlang.monitor_node(node, true)

        :gen_server.cast(
          {:global_group, node},
          {:find_name, self(), name}
        )

        receive do
          {:nodedown, ^node} ->
            :node_down

          {:find_name_res, result} ->
            :erlang.monitor_node(node, false)
            result
        end
    end
  end

  defp names_group_loop([]) do
    :group_down
  end

  defp names_group_loop([node | t]) do
    case names_check_node(node) do
      :node_down ->
        names_group_loop(t)

      r ->
        r
    end
  end

  defp names_check_node(node) do
    case :net_adm.ping(node) do
      :pang ->
        :node_down

      :pong ->
        :erlang.monitor_node(node, true)

        :gen_server.cast(
          {:global_group, node},
          {:registered_names, self()}
        )

        receive do
          {:nodedown, ^node} ->
            :node_down

          {:registered_names_res, result} ->
            :erlang.monitor_node(node, false)
            result
        end
    end
  end

  def send_test(_Args) do
    :timer.sleep(5000)
    exit(:testing_exit)
  end

  def whereis_test(_Args) do
    :timer.sleep(5000)
    exit(:testing_exit)
  end

  def names_test(_Args) do
    :timer.sleep(5000)
    exit(:testing_exit)
  end
end
