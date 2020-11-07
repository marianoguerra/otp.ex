defmodule :m_inet_tcp_dist do
  use Bitwise
  import :error_logger, only: [error_msg: 2]
  require Record

  Record.defrecord(:r_net_address, :net_address,
    address: :undefined,
    host: :undefined,
    protocol: :undefined,
    family: :undefined
  )

  Record.defrecord(:r_hs_data, :hs_data,
    kernel_pid: :undefined,
    other_node: :undefined,
    this_node: :undefined,
    socket: :undefined,
    timer: :undefined,
    this_flags: :undefined,
    allowed: :undefined,
    other_version: :undefined,
    other_flags: :undefined,
    other_started: :undefined,
    f_send: :undefined,
    f_recv: :undefined,
    f_setopts_pre_nodeup: :undefined,
    f_setopts_post_nodeup: :undefined,
    f_getll: :undefined,
    f_address: :undefined,
    mf_tick: :undefined,
    mf_getstat: :undefined,
    request_type: :normal,
    mf_setopts: :undefined,
    mf_getopts: :undefined,
    f_handshake_complete: :undefined,
    add_flags: :undefined,
    reject_flags: :undefined,
    require_flags: :undefined,
    this_creation: :undefined,
    other_creation: :undefined
  )

  def select(node) do
    gen_select(:inet_tcp, node)
  end

  def gen_select(driver, node) do
    case split_node(:erlang.atom_to_list(node), ?@, []) do
      [_, host] ->
        case :inet.getaddr(host, driver.family()) do
          {:ok, _} ->
            true

          _ ->
            false
        end

      _ ->
        false
    end
  end

  def address() do
    gen_address(:inet_tcp)
  end

  def gen_address(driver) do
    get_tcp_address(driver)
  end

  def listen(name, host) do
    gen_listen(:inet_tcp, name, host)
  end

  def listen(name) do
    {:ok, host} = :inet.gethostname()
    listen(name, host)
  end

  def gen_listen(driver, name, host) do
    erlEpmd = :net_kernel.epmd_module()

    case gen_listen(erlEpmd, name, host, driver, [
           {:active, false},
           {:packet, 2},
           {:reuseaddr, true}
         ]) do
      {:ok, socket} ->
        tcpAddress = get_tcp_address(driver, socket)
        {_, port} = r_net_address(tcpAddress, :address)

        case erlEpmd.register_node(name, port, driver) do
          {:ok, creation} ->
            {:ok, {socket, tcpAddress, creation}}

          error ->
            error
        end

      error ->
        error
    end
  end

  defp gen_listen(erlEpmd, name, host, driver, options) do
    listenOptions = listen_options(options)

    case call_epmd_function(erlEpmd, :listen_port_please, [name, host]) do
      {:ok, 0} ->
        {first, last} = get_port_range()
        do_listen(driver, first, last, listenOptions)

      {:ok, prt} ->
        do_listen(driver, prt, prt, listenOptions)
    end
  end

  defp get_port_range() do
    case :application.get_env(
           :kernel,
           :inet_dist_listen_min
         ) do
      {:ok, n} when is_integer(n) ->
        case :application.get_env(
               :kernel,
               :inet_dist_listen_max
             ) do
          {:ok, m} when is_integer(m) ->
            {n, m}

          _ ->
            {n, n}
        end

      _ ->
        {0, 0}
    end
  end

  defp do_listen(_Driver, first, last, _) when first > last do
    {:error, :eaddrinuse}
  end

  defp do_listen(driver, first, last, options) do
    case driver.listen(first, options) do
      {:error, :eaddrinuse} ->
        do_listen(driver, first + 1, last, options)

      other ->
        other
    end
  end

  defp listen_options(opts0) do
    opts1 =
      case :application.get_env(
             :kernel,
             :inet_dist_use_interface
           ) do
        {:ok, ip} ->
          [{:ip, ip} | opts0]

        _ ->
          opts0
      end

    case :application.get_env(
           :kernel,
           :inet_dist_listen_options
         ) do
      {:ok, listenOpts} ->
        case :proplists.is_defined(:backlog, listenOpts) do
          true ->
            listenOpts ++ opts1

          false ->
            listenOpts ++ [{:backlog, 128} | opts1]
        end

      _ ->
        [{:backlog, 128} | opts1]
    end
  end

  def accept(listen) do
    gen_accept(:inet_tcp, listen)
  end

  def gen_accept(driver, listen) do
    :erlang.spawn_opt(:inet_tcp_dist, :accept_loop, [driver, self(), listen], [
      :link,
      {:priority, :max}
    ])
  end

  def accept_loop(driver, kernel, listen) do
    case driver.accept(listen) do
      {:ok, socket} ->
        send(kernel, {:accept, self(), socket, driver.family(), :tcp})
        _ = controller(driver, kernel, socket)
        accept_loop(driver, kernel, listen)

      error ->
        exit(error)
    end
  end

  defp controller(driver, kernel, socket) do
    receive do
      {^kernel, :controller, pid} ->
        flush_controller(pid, socket)
        driver.controlling_process(socket, pid)
        flush_controller(pid, socket)
        send(pid, {self(), :controller})

      {^kernel, :unsupported_protocol} ->
        exit(:unsupported_protocol)
    end
  end

  defp flush_controller(pid, socket) do
    receive do
      {:tcp, ^socket, data} ->
        send(pid, {:tcp, socket, data})
        flush_controller(pid, socket)

      {:tcp_closed, ^socket} ->
        send(pid, {:tcp_closed, socket})
        flush_controller(pid, socket)
    after
      0 ->
        :ok
    end
  end

  def accept_connection(acceptPid, socket, myNode, allowed, setupTime) do
    gen_accept_connection(:inet_tcp, acceptPid, socket, myNode, allowed, setupTime)
  end

  def gen_accept_connection(driver, acceptPid, socket, myNode, allowed, setupTime) do
    :erlang.spawn_opt(
      :inet_tcp_dist,
      :do_accept,
      [driver, self(), acceptPid, socket, myNode, allowed, setupTime],
      [:link, {:priority, :max}]
    )
  end

  def do_accept(driver, kernel, acceptPid, socket, myNode, allowed, setupTime) do
    receive do
      {^acceptPid, :controller} ->
        timer = :dist_util.start_timer(setupTime)

        case check_ip(driver, socket) do
          true ->
            hSData =
              r_hs_data(
                kernel_pid: kernel,
                this_node: myNode,
                socket: socket,
                timer: timer,
                this_flags: 0,
                allowed: allowed,
                f_send: Function.capture(driver, :send, 2),
                f_recv: Function.capture(driver, :recv, 3),
                f_setopts_pre_nodeup: fn s ->
                  :inet.setopts(
                    s,
                    [{:active, false}, {:packet, 4}, nodelay()]
                  )
                end,
                f_setopts_post_nodeup: fn s ->
                  :inet.setopts(
                    s,
                    [{:active, true}, {:deliver, :port}, {:packet, 4}, :binary, nodelay()]
                  )
                end,
                f_getll: fn s ->
                  :inet.getll(s)
                end,
                f_address: fn s, node ->
                  get_remote_id(driver, s, node)
                end,
                mf_tick: fn s ->
                  :inet_tcp_dist.tick(driver, s)
                end,
                mf_getstat: &:inet_tcp_dist.getstat/1,
                mf_setopts: &:inet_tcp_dist.setopts/2,
                mf_getopts: &:inet_tcp_dist.getopts/2
              )

            :dist_util.handshake_other_started(hSData)

          {false, iP} ->
            error_msg('** Connection attempt from disallowed IP ~w ** ~n', [iP])
            :dist_util.shutdown(:inet_tcp_dist, 257, :no_node)
        end
    end
  end

  defp nodelay() do
    case :application.get_env(:kernel, :dist_nodelay) do
      :undefined ->
        {:nodelay, true}

      {:ok, true} ->
        {:nodelay, true}

      {:ok, false} ->
        {:nodelay, false}

      _ ->
        {:nodelay, true}
    end
  end

  defp get_remote_id(driver, socket, node) do
    case :inet.peername(socket) do
      {:ok, address} ->
        case split_node(:erlang.atom_to_list(node), ?@, []) do
          [_, host] ->
            r_net_address(address: address, host: host, protocol: :tcp, family: driver.family())

          _ ->
            :dist_util.shutdown(:inet_tcp_dist, 290, :no_node)
        end

      {:error, _Reason} ->
        :dist_util.shutdown(:inet_tcp_dist, 293, :no_node)
    end
  end

  def setup(node, type, myNode, longOrShortNames, setupTime) do
    gen_setup(:inet_tcp, node, type, myNode, longOrShortNames, setupTime)
  end

  def gen_setup(driver, node, type, myNode, longOrShortNames, setupTime) do
    :erlang.spawn_opt(
      :inet_tcp_dist,
      :do_setup,
      [driver, self(), node, type, myNode, longOrShortNames, setupTime],
      [:link, {:priority, :max}]
    )
  end

  def do_setup(driver, kernel, node, type, myNode, longOrShortNames, setupTime) do
    :ok
    [name, address] = splitnode(driver, node, longOrShortNames)
    addressFamily = driver.family()
    erlEpmd = :net_kernel.epmd_module()
    timer = :dist_util.start_timer(setupTime)

    case call_epmd_function(erlEpmd, :address_please, [name, address, addressFamily]) do
      {:ok, ip, tcpPort, version} ->
        :ok

        do_setup_connect(
          driver,
          kernel,
          node,
          address,
          addressFamily,
          ip,
          tcpPort,
          version,
          type,
          myNode,
          timer
        )

      {:ok, ip} ->
        case erlEpmd.port_please(name, ip) do
          {:port, tcpPort, version} ->
            :ok

            do_setup_connect(
              driver,
              kernel,
              node,
              address,
              addressFamily,
              ip,
              tcpPort,
              version,
              type,
              myNode,
              timer
            )

          _ ->
            :ok
            :dist_util.shutdown(:inet_tcp_dist, 330, node)
        end

      _Other ->
        :ok
        :dist_util.shutdown(:inet_tcp_dist, 335, node)
    end
  end

  defp do_setup_connect(
         driver,
         kernel,
         node,
         address,
         addressFamily,
         ip,
         tcpPort,
         version,
         type,
         myNode,
         timer
       ) do
    :dist_util.reset_timer(timer)

    case driver.connect(ip, tcpPort, connect_options([{:active, false}, {:packet, 2}])) do
      {:ok, socket} ->
        hSData =
          r_hs_data(
            kernel_pid: kernel,
            other_node: node,
            this_node: myNode,
            socket: socket,
            timer: timer,
            this_flags: 0,
            other_version: version,
            f_send: Function.capture(driver, :send, 2),
            f_recv: Function.capture(driver, :recv, 3),
            f_setopts_pre_nodeup: fn s ->
              :inet.setopts(
                s,
                [{:active, false}, {:packet, 4}, nodelay()]
              )
            end,
            f_setopts_post_nodeup: fn s ->
              :inet.setopts(
                s,
                [{:active, true}, {:deliver, :port}, {:packet, 4}, nodelay()]
              )
            end,
            f_getll: &:inet.getll/1,
            f_address: fn _, _ ->
              r_net_address(
                address: {ip, tcpPort},
                host: address,
                protocol: :tcp,
                family: addressFamily
              )
            end,
            mf_tick: fn s ->
              :inet_tcp_dist.tick(driver, s)
            end,
            mf_getstat: &:inet_tcp_dist.getstat/1,
            request_type: type,
            mf_setopts: &:inet_tcp_dist.setopts/2,
            mf_getopts: &:inet_tcp_dist.getopts/2
          )

        :dist_util.handshake_we_started(hSData)

      _ ->
        :ok
        :dist_util.shutdown(:inet_tcp_dist, 400, node)
    end
  end

  defp connect_options(opts) do
    case :application.get_env(
           :kernel,
           :inet_dist_connect_options
         ) do
      {:ok, connectOpts} ->
        connectOpts ++ opts

      _ ->
        opts
    end
  end

  def close(socket) do
    :inet_tcp.close(socket)
  end

  defp splitnode(driver, node, longOrShortNames) do
    case split_node(:erlang.atom_to_list(node), ?@, []) do
      [name | tail] when tail !== [] ->
        host = :lists.append(tail)

        case split_node(host, ?., []) do
          [_] when longOrShortNames === :longnames ->
            case driver.parse_address(host) do
              {:ok, _} ->
                [name, host]

              _ ->
                error_msg(
                  '** System running to use fully qualified hostnames **~n** Hostname ~ts is illegal **~n',
                  [host]
                )

                :dist_util.shutdown(:inet_tcp_dist, 434, node)
            end

          l
          when length(l) > 1 and
                 longOrShortNames === :shortnames ->
            error_msg(
              '** System NOT running to use fully qualified hostnames **~n** Hostname ~ts is illegal **~n',
              [host]
            )

            :dist_util.shutdown(:inet_tcp_dist, 441, node)

          _ ->
            [name, host]
        end

      [_] ->
        error_msg('** Nodename ~p illegal, no \'@\' character **~n', [node])
        :dist_util.shutdown(:inet_tcp_dist, 448, node)

      _ ->
        error_msg('** Nodename ~p illegal **~n', [node])
        :dist_util.shutdown(:inet_tcp_dist, 451, node)
    end
  end

  defp split_node([chr | t], chr, ack) do
    [:lists.reverse(ack) | split_node(t, chr, [])]
  end

  defp split_node([h | t], chr, ack) do
    split_node(t, chr, [h | ack])
  end

  defp split_node([], _, ack) do
    [:lists.reverse(ack)]
  end

  defp get_tcp_address(driver, socket) do
    {:ok, address} = :inet.sockname(socket)
    netAddr = get_tcp_address(driver)
    r_net_address(netAddr, address: address)
  end

  defp get_tcp_address(driver) do
    {:ok, host} = :inet.gethostname()
    r_net_address(host: host, protocol: :tcp, family: driver.family())
  end

  defp call_epmd_function(mod, fun, args) do
    case :erlang.function_exported(mod, fun, length(args)) do
      true ->
        apply(mod, fun, args)

      _ ->
        apply(:erl_epmd, fun, args)
    end
  end

  defp check_ip(driver, socket) do
    case :application.get_env(:check_ip) do
      {:ok, true} ->
        case get_ifs(socket) do
          {:ok, iFs, iP} ->
            check_ip(driver, iFs, iP)

          _ ->
            :dist_util.shutdown(:inet_tcp_dist, 494, :no_node)
        end

      _ ->
        true
    end
  end

  defp get_ifs(socket) do
    case :inet.peername(socket) do
      {:ok, {iP, _}} ->
        case :inet.getif(socket) do
          {:ok, iFs} ->
            {:ok, iFs, iP}

          error ->
            error
        end

      error ->
        error
    end
  end

  defp check_ip(driver, [{ownIP, _, netmask} | iFs], peerIP) do
    case {driver.mask(netmask, peerIP), driver.mask(netmask, ownIP)} do
      {m, m} ->
        true

      _ ->
        check_ip(driver, iFs, peerIP)
    end
  end

  defp check_ip(_Driver, [], peerIP) do
    {false, peerIP}
  end

  def is_node_name(node) when is_atom(node) do
    case split_node(:erlang.atom_to_list(node), ?@, []) do
      [_, _Host] ->
        true

      _ ->
        false
    end
  end

  def is_node_name(_Node) do
    false
  end

  def tick(driver, socket) do
    case driver.send(socket, [], [:force]) do
      {:error, :closed} ->
        send(self(), {:tcp_closed, socket})
        {:error, :closed}

      r ->
        r
    end
  end

  def getstat(socket) do
    case :inet.getstat(
           socket,
           [:recv_cnt, :send_cnt, :send_pend]
         ) do
      {:ok, stat} ->
        split_stat(stat, 0, 0, 0)

      error ->
        error
    end
  end

  defp split_stat([{:recv_cnt, r} | stat], _, w, p) do
    split_stat(stat, r, w, p)
  end

  defp split_stat([{:send_cnt, w} | stat], r, _, p) do
    split_stat(stat, r, w, p)
  end

  defp split_stat([{:send_pend, p} | stat], r, w, _) do
    split_stat(stat, r, w, p)
  end

  defp split_stat([], r, w, p) do
    {:ok, r, w, p}
  end

  def setopts(s, opts) do
    case (for {k, _} = opt <- opts,
              k === :active or k === :deliver or k === :packet do
            opt
          end) do
      [] ->
        :inet.setopts(s, opts)

      opts1 ->
        {:error, {:badopts, opts1}}
    end
  end

  def getopts(s, opts) do
    :inet.getopts(s, opts)
  end
end
