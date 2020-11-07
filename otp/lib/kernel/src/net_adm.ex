defmodule :m_net_adm do
  use Bitwise

  def host_file() do
    home =
      case :init.get_argument(:home) do
        {:ok, [[h]]} ->
          [h]

        _ ->
          []
      end

    case :file.path_consult(
           ['.'] ++ home ++ [:code.root_dir()],
           '.hosts.erlang'
         ) do
      {:ok, hosts, _} ->
        hosts

      error ->
        error
    end
  end

  def ping(node) when is_atom(node) do
    case (try do
            :gen.call({:net_kernel, node}, :"$gen_call", {:is_auth, node()}, :infinity)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, :yes} ->
        :pong

      _ ->
        :erlang.disconnect_node(node)
        :pang
    end
  end

  def localhost() do
    {:ok, host} = :inet.gethostname()

    case :inet_db.res_option(:domain) do
      '' ->
        host

      domain ->
        host ++ '.' ++ domain
    end
  end

  def names() do
    names(localhost())
  end

  def names(hostname) do
    erlEpmd = :net_kernel.epmd_module()
    erlEpmd.names(hostname)
  end

  def dns_hostname(hostname) do
    case :inet.gethostbyname(hostname) do
      {:ok, {:hostent, name, _, _Af, _Size, _Addr}} ->
        {:ok, name}

      _ ->
        {:error, hostname}
    end
  end

  def ping_list(nodelist) do
    :ok = :net_kernel.monitor_nodes(true)
    sofar = ping_first(nodelist, :erlang.nodes())
    collect_new(sofar, nodelist)
  end

  defp ping_first([], _S) do
    []
  end

  defp ping_first([node | nodes], s) do
    case :lists.member(node, s) do
      true ->
        [node | ping_first(nodes, s)]

      false ->
        case ping(node) do
          :pong ->
            [node]

          :pang ->
            ping_first(nodes, s)
        end
    end
  end

  defp collect_new(sofar, nodelist) do
    receive do
      {:nodeup, node} ->
        case :lists.member(node, nodelist) do
          true ->
            collect_new(sofar, nodelist)

          false ->
            collect_new([node | sofar], nodelist)
        end
    after
      3000 ->
        :ok = :net_kernel.monitor_nodes(false)
        sofar
    end
  end

  def world() do
    world(:silent)
  end

  def world(verbose) do
    case :net_adm.host_file() do
      {:error, r} ->
        exit({:error, r})

      hosts ->
        expand_hosts(hosts, verbose)
    end
  end

  def world_list(hosts) when is_list(hosts) do
    expand_hosts(hosts, :silent)
  end

  def world_list(hosts, verbose) when is_list(hosts) do
    expand_hosts(hosts, verbose)
  end

  defp expand_hosts(hosts, verbose) do
    :lists.flatten(collect_nodes(hosts, verbose))
  end

  defp collect_nodes([], _) do
    []
  end

  defp collect_nodes([host | tail], verbose) do
    case collect_host_nodes(host, verbose) do
      nil ->
        collect_nodes(tail, verbose)

      l ->
        [l | collect_nodes(tail, verbose)]
    end
  end

  defp collect_host_nodes(host, verbose) do
    case names(host) do
      {:ok, namelist} ->
        do_ping(namelist, :erlang.atom_to_list(host), verbose)

      _ ->
        nil
    end
  end

  defp do_ping(names, host0, verbose) do
    case longshort(host0) do
      :ignored ->
        []

      host ->
        do_ping_1(names, host, verbose)
    end
  end

  defp do_ping_1([], _Host, _Verbose) do
    []
  end

  defp do_ping_1([{name, _} | rest], host, verbose) do
    node = :erlang.list_to_atom(name ++ '@' ++ longshort(host))
    verbose(verbose, 'Pinging ~w -> ', [node])
    result = ping(node)
    verbose(verbose, '~p\n', [result])

    case result do
      :pong ->
        [node | do_ping_1(rest, host, verbose)]

      :pang ->
        do_ping_1(rest, host, verbose)
    end
  end

  defp verbose(:verbose, format, args) do
    :io.format(format, args)
  end

  defp verbose(_, _, _) do
    :ok
  end

  defp longshort(host) do
    case :net_kernel.longnames() do
      false ->
        uptodot(host)

      true ->
        host

      :ignored ->
        :ignored
    end
  end

  defp uptodot([?. | _]) do
    []
  end

  defp uptodot([]) do
    []
  end

  defp uptodot([h | t]) do
    [h | uptodot(t)]
  end
end
