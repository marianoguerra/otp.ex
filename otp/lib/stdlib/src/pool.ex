defmodule :m_pool do
  use Bitwise

  def start(name) do
    start(name, [])
  end

  def start(name, args) when is_atom(name) do
    _ = :gen_server.start({:global, :pool_master}, :pool, [], [])
    hosts = :net_adm.host_file()
    nodes = start_nodes(hosts, name, args)
    :lists.foreach(&attach/1, nodes)
    nodes
  end

  def get_nodes() do
    get_elements(2, get_nodes_and_load())
  end

  def attach(node) do
    :gen_server.call(
      {:global, :pool_master},
      {:attach, node}
    )
  end

  def get_nodes_and_load() do
    :gen_server.call({:global, :pool_master}, :get_nodes)
  end

  def get_node() do
    :gen_server.call({:global, :pool_master}, :get_node)
  end

  def pspawn(m, f, a) do
    :gen_server.call(
      {:global, :pool_master},
      {:spawn, :erlang.group_leader(), m, f, a}
    )
  end

  def pspawn_link(m, f, a) do
    :erlang.spawn_link(get_node(), m, f, a)
  end

  defp start_nodes([], _, _) do
    []
  end

  defp start_nodes([host | tail], name, args) do
    case :slave.start(host, name, args) do
      {:error, {:already_running, node}} ->
        :io.format('Can\'t start node on host ~w due to ~w~n', [host, {:already_running, node}])
        [node | start_nodes(tail, name, args)]

      {:error, r} ->
        :io.format('Can\'t start node on host ~w due to ~w~n', [host, r])
        start_nodes(tail, name, args)

      {:ok, node} ->
        [node | start_nodes(tail, name, args)]
    end
  end

  def stop() do
    :gen_server.call({:global, :pool_master}, :stop)
  end

  defp get_elements(_Pos, []) do
    []
  end

  defp get_elements(pos, [e | t]) do
    [:erlang.element(pos, e) | get_elements(pos, t)]
  end

  defp stop_em([]) do
    :stopped
  end

  defp stop_em([n | tail]) do
    :rpc.cast(n, :erlang, :halt, [])
    stop_em(tail)
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    spawn_link(:pool, :statistic_collector, [])
    {:ok, [{0, node()}]}
  end

  def handle_call(:get_nodes, _From, nodes) do
    {:reply, nodes, nodes}
  end

  def handle_call(:get_node, _From, [{load, n} | tail]) do
    {:reply, n, tail ++ [{load + 1, n}]}
  end

  def handle_call({:attach, node}, _From, nodes) do
    case :lists.keymember(node, 2, nodes) do
      true ->
        {:reply, :already_attached, nodes}

      false ->
        :erlang.monitor_node(node, true)
        :erlang.spawn_link(node, :pool, :statistic_collector, [])
        {:reply, :attached, nodes ++ [{999_999, node}]}
    end
  end

  def handle_call({:spawn, gl, m, f, a}, _From, nodes) do
    {:reply, n, newNodes} = handle_call(:get_node, _From, nodes)
    pid = :erlang.spawn(n, :pool, :do_spawn, [gl, m, f, a])
    {:reply, pid, newNodes}
  end

  def handle_call(:stop, _From, nodes) do
    {:stop, :normal, :stopped, nodes}
  end

  def handle_cast(_, nodes) do
    {:noreply, nodes}
  end

  def handle_info({node, :load, load}, nodes) do
    nodes2 = insert_node({load, node}, nodes)
    {:noreply, nodes2}
  end

  def handle_info({:nodedown, node}, nodes) do
    {:noreply, :lists.keydelete(node, 2, nodes)}
  end

  def handle_info(_, nodes) do
    {:noreply, nodes}
  end

  def terminate(_Reason, nodes) do
    n = :lists.delete(node(), get_elements(2, nodes))
    stop_em(n)
    :ok
  end

  def do_spawn(gl, m, f, a) do
    :erlang.group_leader(gl, self())
    apply(m, f, a)
  end

  defp insert_node({load, node}, [{l, node} | tail])
       when load > l do
    pure_insert({load, node}, tail)
  end

  defp insert_node({load, node}, [{l, n} | tail]) when load <= l do
    t = :lists.keydelete(node, 2, [{l, n} | tail])
    [{load, node} | t]
  end

  defp insert_node(ln, [h | t]) do
    [h | insert_node(ln, t)]
  end

  defp insert_node(x, []) do
    :error_logger.error_msg('Pool_master: Bad node list X=~w\n', [x])
    exit(:crash)
  end

  defp pure_insert({load, node}, []) do
    [{load, node}]
  end

  defp pure_insert({load, node}, [{l, n} | tail]) when load < l do
    [[{load, node}, {l, n}] | tail]
  end

  defp pure_insert(l, [h | t]) do
    [h | pure_insert(l, t)]
  end

  def statistic_collector() do
    statistic_collector(5)
  end

  defp statistic_collector(0) do
    exit(:normal)
  end

  defp statistic_collector(i) do
    :timer.sleep(300)

    case :global.whereis_name(:pool_master) do
      :undefined ->
        statistic_collector(i - 1)

      m ->
        stat_loop(m, 999_999)
    end
  end

  defp stat_loop(m, old) do
    :timer.sleep(2000)

    case :erlang.statistics(:run_queue) do
      ^old ->
        stat_loop(m, old)

      newLoad ->
        send(m, {node(), :load, newLoad})
        stat_loop(m, newLoad)
    end
  end
end
