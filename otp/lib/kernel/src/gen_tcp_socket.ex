defmodule :m_gen_tcp_socket do
  use Bitwise
  import Kernel, except: [send: 2]
  @behaviour :gen_statem
  require Record
  Record.defrecord(:r_connect_opts, :connect_opts, ifaddr: :any, port: 0, fd: -1, opts: [])

  Record.defrecord(:r_listen_opts, :listen_opts,
    ifaddr: :any,
    port: 0,
    backlog: 5,
    fd: -1,
    opts: []
  )

  Record.defrecord(:r_udp_opts, :udp_opts, ifaddr: :any, port: 0, fd: -1, opts: [{:active, true}])

  Record.defrecord(:r_sctp_opts, :sctp_opts,
    ifaddr: :undefined,
    port: 0,
    fd: -1,
    type: :seqpacket,
    opts: [
      {:mode, :binary},
      {:buffer, 65536},
      {:sndbuf, 65536},
      {:recbuf, 1024},
      {:sctp_events, :undefined}
    ]
  )

  def connect(address, port, opts, timeout) do
    timer = :inet.start_timer(timeout)

    try do
      connect_lookup(address, port, opts, timer)
    after
      _ = :inet.stop_timer(timer)
    end
  end

  defp connect_lookup(address, port, opts, timer) do
    {einvalOpts, opts_1} = setopts_split(:einval, opts)
    einvalOpts === [] or exit(:badarg)
    {mod, opts_2} = :inet.tcp_module(opts_1, address)
    domain = domain(mod)
    {startOpts, opts_3} = setopts_split(:start, opts_2)
    errRef = make_ref()

    try do
      iPs = val(errRef, mod.getaddrs(address, timer))
      tP = val(errRef, mod.getserv(port))
      cO = val(errRef, :inet.connect_options(opts_3, mod))
      {sockaddrs(iPs, tP, domain), cO}
    catch
      {^errRef, reason} ->
        case {:error, reason} do
          {:error, :badarg} ->
            exit(:badarg)

          oTHER__ ->
            oTHER__
        end
    else
      {addrs, r_connect_opts(fd: fd, ifaddr: bindIP, port: bindPort, opts: connectOpts)} ->
        bindAddr = bind_addr(domain, bindIP, bindPort)
        connect_open(addrs, domain, connectOpts, startOpts, fd, timer, bindAddr)
    end
  end

  defp connect_open(addrs, domain, connectOpts, opts, fd, timer, bindAddr) do
    extraOpts =
      cond do
        fd === -1 ->
          []

        is_integer(fd) ->
          [{:fd, fd}]

        is_list(fd) ->
          fd
      end

    {socketOpts, startOpts} = setopts_split(:socket, opts)

    case start_server(domain, extraOpts, [
           {:timeout, :inet.timeout(timer)}
           | start_opts(startOpts)
         ]) do
      {:ok, server} ->
        {setopts, _} =
          setopts_split(
            %{:socket => [], :server_read => [], :server_write => []},
            connectOpts
          )

        errRef = make_ref()

        try do
          ok(
            errRef,
            call(server, {:setopts, socketOpts ++ setopts})
          )

          ok(errRef, call_bind(server, bindAddr))
          defaultError = {:error, :einval}

          socket =
            val(
              errRef,
              connect_loop(addrs, server, defaultError, timer)
            )

          {:ok, {:"$inet", :gen_tcp_socket, {server, socket}}}
        catch
          {^errRef, reason} ->
            close_server(server)

            case {:error, reason} do
              {:error, :badarg} ->
                exit(:badarg)

              oTHER__ ->
                oTHER__
            end
        end

      {:error, _} = error ->
        case error do
          {:error, :badarg} ->
            exit(:badarg)

          oTHER__ ->
            oTHER__
        end
    end
  end

  defp connect_loop([], _Server, error, _Timer) do
    error
  end

  defp connect_loop([addr | addrs], server, _Error, timer) do
    result =
      call(
        server,
        {:connect, addr, :inet.timeout(timer)}
      )

    case result do
      {:ok, _Socket} ->
        result

      {:error, :badarg} ->
        result

      {:error, :einval} ->
        result

      {:error, :timeout} ->
        result

      {:error, _} ->
        connect_loop(addrs, server, result, timer)
    end
  end

  defp bind_addr(domain, bindIP, bindPort) do
    case domain do
      :local ->
        case bindIP do
          :any ->
            :undefined

          {:local, path} ->
            %{:family => domain, :path => path}
        end

      _ when domain === :inet or domain === :inet6 ->
        %{:family => domain, :addr => bindIP, :port => bindPort}
    end
  end

  defp call_bind(_Server, :undefined) do
    :ok
  end

  defp call_bind(server, bindAddr) do
    call(server, {:bind, bindAddr})
  end

  def listen(port, opts) do
    {einvalOpts, opts_1} = setopts_split(:einval, opts)
    einvalOpts === [] or exit(:badarg)
    {mod, opts_2} = :inet.tcp_module(opts_1)
    {startOpts, opts_3} = setopts_split(:start, opts_2)

    case mod.getserv(port) do
      {:ok, tP} ->
        case :inet.listen_options(
               [{:port, tP} | opts_3],
               mod
             ) do
          {:error, :badarg} ->
            exit(:badarg)

          {:ok,
           r_listen_opts(
             fd: fd,
             ifaddr: bindIP,
             port: bindPort,
             opts: listenOpts,
             backlog: backlog
           )} ->
            domain = domain(mod)
            bindAddr = bind_addr(domain, bindIP, bindPort)
            listen_open(domain, listenOpts, startOpts, fd, backlog, bindAddr)
        end

      {:error, _} = error ->
        case error do
          {:error, :badarg} ->
            exit(:badarg)

          oTHER__ ->
            oTHER__
        end
    end
  end

  defp listen_open(domain, listenOpts, opts, fd, backlog, bindAddr) do
    extraOpts =
      cond do
        fd === -1 ->
          []

        is_integer(fd) ->
          [{:fd, fd}]

        is_list(fd) ->
          fd
      end

    {socketOpts, startOpts} = setopts_split(:socket, opts)

    case start_server(domain, extraOpts, [{:timeout, :infinity} | start_opts(startOpts)]) do
      {:ok, server} ->
        {setopts, _} =
          setopts_split(
            %{:socket => [], :server_read => [], :server_write => []},
            listenOpts
          )

        errRef = make_ref()

        try do
          ok(
            errRef,
            call(
              server,
              {:setopts, [{:start_opts, startOpts}] ++ socketOpts ++ setopts}
            )
          )

          ok(errRef, call_bind(server, bindAddr))
          socket = val(errRef, call(server, {:listen, backlog}))
          {:ok, {:"$inet", :gen_tcp_socket, {server, socket}}}
        catch
          {^errRef, reason} ->
            close_server(server)

            case {:error, reason} do
              {:error, :badarg} ->
                exit(:badarg)

              oTHER__ ->
                oTHER__
            end
        end

      {:error, {:shutdown, reason}} ->
        case {:error, reason} do
          {:error, :badarg} ->
            exit(:badarg)

          oTHER__ ->
            oTHER__
        end

      {:error, _} = error ->
        case error do
          {:error, :badarg} ->
            exit(:badarg)

          oTHER__ ->
            oTHER__
        end
    end
  end

  def accept(
        {:"$inet", :gen_tcp_socket, {listenServer, listenSocket}},
        timeout
      ) do
    timer = :inet.start_timer(timeout)
    errRef = make_ref()

    try do
      %{:start_opts => startOpts} =
        serverData =
        val(
          errRef,
          call(
            listenServer,
            :get_server_opts
          )
        )

      server =
        val(
          errRef,
          start_server(
            serverData,
            [
              {:timeout, :inet.timeout(timer)}
              | start_opts(startOpts)
            ]
          )
        )

      socket =
        val(
          {errRef, server},
          call(
            server,
            {:accept, listenSocket, :inet.timeout(timer)}
          )
        )

      {:ok, {:"$inet", :gen_tcp_socket, {server, socket}}}
    catch
      {{^errRef, srv}, reason} ->
        stop_server(srv)

        case {:error, reason} do
          {:error, :badarg} ->
            exit(:badarg)

          oTHER__ ->
            oTHER__
        end

      {^errRef, reason} ->
        case {:error, reason} do
          {:error, :badarg} ->
            exit(:badarg)

          oTHER__ ->
            oTHER__
        end
    after
      _ = :inet.stop_timer(timer)
    end
  end

  def send({:"$inet", :gen_tcp_socket, {server, socket}}, data) do
    case :socket.getopt(socket, {:otp, :meta}) do
      {:ok, %{:packet => packet, :send_timeout => sendTimeout} = meta} ->
        cond do
          packet === 1 or packet === 2 or packet === 4 ->
            size = :erlang.iolist_size(data)
            header = <<size::size(packet)-unit(8)-integer-big-unsigned>>
            result = socket_send(socket, [header, data], sendTimeout)
            send_result(server, meta, result)

          true ->
            result = socket_send(socket, data, sendTimeout)
            send_result(server, meta, result)
        end

      {:ok, _BadMeta} ->
        exit(:badarg)

      {:error, _} = error ->
        error
    end
  end

  defp send_result(server, meta, result) do
    case result do
      {:error, {reason, _RestData}} ->
        case reason do
          :econnreset ->
            case :maps.get(:show_econnreset, meta) do
              true ->
                {:error, :econnreset}

              false ->
                {:error, :closed}
            end

          :timeout ->
            _ =
              :maps.get(
                :send_timeout_close,
                meta
              ) and close_server(server)

            {:error, reason}

          _ ->
            case {:error, reason} do
              {:error, :badarg} ->
                exit(:badarg)

              oTHER__ ->
                oTHER__
            end
        end

      :ok ->
        :ok
    end
  end

  def recv({:"$inet", :gen_tcp_socket, {server, _Socket}}, length, timeout) do
    case call(server, {:recv, length, timeout}) do
      {:error, :badarg} ->
        exit(:badarg)

      oTHER__ ->
        oTHER__
    end
  end

  def shutdown({:"$inet", :gen_tcp_socket, {server, socket}}, how) do
    result =
      case how do
        :write ->
          :socket.shutdown(socket, how)

        :read ->
          call(server, :shutdown_read)

        :read_write ->
          close_server(server)
      end

    case result do
      {:error, :badarg} ->
        exit(:badarg)

      oTHER__ ->
        oTHER__
    end
  end

  def close({:"$inet", :gen_tcp_socket, {server, _Socket}}) do
    case close_server(server) do
      {:error, :badarg} ->
        exit(:badarg)

      oTHER__ ->
        oTHER__
    end
  end

  defp close_server(server) do
    result = call(server, :close)
    stop_server(server)
    result
  end

  def controlling_process(
        {:"$inet", :gen_tcp_socket, {server, _Socket}} = s,
        newOwner
      )
      when is_pid(newOwner) do
    case call(server, {:controlling_process, newOwner}) do
      :ok ->
        :ok

      :transfer ->
        controlling_process(s, newOwner, server)

      {:error, _} = error ->
        error
    end
  end

  defp controlling_process(s, newOwner, server) do
    receive do
      {:tcp, ^s, _Data} = msg ->
        controlling_process(s, newOwner, server, msg)

      {:tcp_closed, ^s} = msg ->
        controlling_process(s, newOwner, server, msg)

      {^s, {:data, _Data}} = msg ->
        controlling_process(s, newOwner, server, msg)
    after
      0 ->
        call(server, :controlling_process)
    end
  end

  defp controlling_process(s, newOwner, server, msg) do
    send(newOwner, msg)
    controlling_process(s, newOwner, server)
  end

  def setopts({:"$inet", :gen_tcp_socket, {server, _Socket}}, opts)
      when is_list(opts) do
    call(server, {:setopts, opts})
  end

  def getopts({:"$inet", :gen_tcp_socket, {server, _Socket}}, opts)
      when is_list(opts) do
    call(server, {:getopts, opts})
  end

  def sockname({:"$inet", :gen_tcp_socket, {_Server, socket}}) do
    case :socket.sockname(socket) do
      {:ok, sockAddr} ->
        {:ok, address(sockAddr)}

      {:error, _} = error ->
        error
    end
  end

  def peername({:"$inet", :gen_tcp_socket, {_Server, socket}}) do
    case :socket.peername(socket) do
      {:ok, sockAddr} ->
        {:ok, address(sockAddr)}

      {:error, _} = error ->
        error
    end
  end

  def getstat({:"$inet", :gen_tcp_socket, {server, _Socket}}, what)
      when is_list(what) do
    call(server, {:getstat, what})
  end

  defp socket_send(socket, opts, timeout) do
    result = :socket.send(socket, opts, timeout)

    case result do
      {:error, {:epipe, rest}} ->
        {:error, {:econnreset, rest}}

      {:error, {_Reason, _Rest}} ->
        result

      {:select, _} ->
        result

      {:ok, _} ->
        result

      :ok ->
        :ok
    end
  end

  defp socket_recv_peek(socket, length) do
    options = [:peek]
    result = :socket.recv(socket, length, options, :nowait)
    result
  end

  defp socket_recv(socket, length) do
    result = :socket.recv(socket, length, :nowait)
    result
  end

  defp socket_close(socket) do
    case :socket.close(socket) do
      :ok ->
        :ok

      {:error, :closed} ->
        :ok
    end
  end

  defp socket_cancel(socket, selectInfo) do
    case :socket.cancel(socket, selectInfo) do
      :ok ->
        :ok

      {:error, :closed} ->
        :ok
    end
  end

  defp ok(_ErrRef, :ok) do
    :ok
  end

  defp ok(errRef, {:error, reason}) do
    throw({errRef, reason})
  end

  defp val(_ErrRef, {:ok, val}) do
    val
  end

  defp val(errRef, {:error, reason}) do
    throw({errRef, reason})
  end

  defp address(sockAddr) do
    case sockAddr do
      %{:family => family, :addr => iP, :port => port}
      when family === :inet or family === :inet6 ->
        {iP, port}

      %{:family => :local, :path => path} ->
        {:local, path}
    end
  end

  defp domain(mod) do
    case mod do
      :inet_tcp ->
        :inet

      :inet6_tcp ->
        :inet6

      :local_tcp ->
        :local
    end
  end

  defp sockaddrs([], _TP, _Domain) do
    []
  end

  defp sockaddrs([{:local, path} | iPs], tP, domain)
       when domain === :local do
    [%{:family => domain, :path => path} | sockaddrs(iPs, tP, domain)]
  end

  defp sockaddrs([iP | iPs], tP, domain) do
    [
      %{:family => domain, :addr => iP, :port => tP}
      | sockaddrs(iPs, tP, domain)
    ]
  end

  defp setopts_split(filterTags, opts) do
    setopts_split(filterTags, opts, [], [])
  end

  defp setopts_split(_FilterTags, [], true__, false__) do
    {reverse(true__), reverse(false__)}
  end

  defp setopts_split(filterTags, [opt | opts], true__, false__) do
    opt_1 = conv_setopt(opt)

    case member(filterTags, setopt_categories(opt_1)) do
      true ->
        setopts_split(filterTags, opts, [opt_1 | true__], false__)

      false ->
        setopts_split(filterTags, opts, true__, [opt_1 | false__])
    end
  end

  defp member(x, y) when is_atom(x) and is_map(y) do
    case y do
      %{^x => _} ->
        true

      %{} ->
        false
    end
  end

  defp member(x, y) when is_map(x) and is_map(y) do
    :maps.fold(
      fn
        _, _, true ->
          true

        key, _, false ->
          :maps.is_key(key, y)
      end,
      false,
      x
    )
  end

  defp conv_setopt(:binary) do
    {:mode, :binary}
  end

  defp conv_setopt(:list) do
    {:mode, :list}
  end

  defp conv_setopt(:inet) do
    {:tcp_module, :inet_tcp}
  end

  defp conv_setopt(:inet6) do
    {:tcp_module, :inet6_tcp}
  end

  defp conv_setopt(:local) do
    {:tcp_module, :local_tcp}
  end

  defp conv_setopt(other) do
    other
  end

  defp socket_setopt(socket, {:raw, level, key, value}) do
    :socket.setopt_native(socket, {level, key}, value)
  end

  defp socket_setopt(socket, {:raw, {level, key, value}}) do
    :socket.setopt_native(socket, {level, key}, value)
  end

  defp socket_setopt(socket, {tag, value}) do
    case socket_opt() do
      %{^tag => opt} ->
        :socket.setopt(socket, opt, socket_setopt_value(tag, value))

      %{} ->
        {:error, :einval}
    end
  end

  defp socket_setopt_value(:linger, {onOff, linger}) do
    %{:onoff => onOff, :linger => linger}
  end

  defp socket_setopt_value(_Tag, value) do
    value
  end

  defp socket_getopt(socket, {:raw, level, key, valueSpec}) do
    :socket.getopt_native(socket, {level, key}, valueSpec)
  end

  defp socket_getopt(socket, {:raw, {level, key, valueSpec}}) do
    :socket.getopt_native(socket, {level, key}, valueSpec)
  end

  defp socket_getopt(socket, tag) when is_atom(tag) do
    case socket_opt() do
      %{^tag => opt} ->
        socket_getopt_value(tag, :socket.getopt(socket, opt))

      %{} ->
        {:error, :einval}
    end
  end

  defp socket_getopt_value(
         :linger,
         {:ok, %{:onoff => onOff, :linger => linger}}
       ) do
    {:ok, {onOff, linger}}
  end

  defp socket_getopt_value(_Tag, {:ok, _Value} = ok) do
    ok
  end

  defp socket_getopt_value(_Tag, {:error, _} = error) do
    error
  end

  defp socket_copy_opt(socket, tag, targetSocket) when is_atom(tag) do
    case socket_opt() do
      %{^tag => opt} ->
        case :socket.is_supported(:options, opt) do
          true ->
            case :socket.getopt(socket, opt) do
              {:ok, value} ->
                :socket.setopt(targetSocket, opt, value)

              {:error, _Reason} = error ->
                error
            end

          false ->
            :ok
        end

      %{} = _X ->
        {:error, :einval}
    end
  end

  defp start_opts([{:sys_debug, d} | opts]) do
    [{:debug, d} | start_opts(opts)]
  end

  defp start_opts([opt | opts]) do
    [opt | start_opts(opts)]
  end

  defp start_opts([]) do
    []
  end

  defp setopt_categories(opt) do
    case opt do
      {:raw, _, _, _} ->
        %{:socket => []}

      {:raw, {_, _, _}} ->
        %{:socket => []}

      {tag, _} ->
        opt_categories(tag)

      _ ->
        :ignore
    end
  end

  defp getopt_categories(opt) do
    case opt do
      {:raw, _, _, _} ->
        %{:socket => []}

      {:raw, {_, _, _}} ->
        %{:socket => []}

      _ ->
        opt_categories(opt)
    end
  end

  defp opt_categories(tag) when is_atom(tag) do
    case tag do
      :sys_debug ->
        %{:start => []}

      :debug ->
        %{:socket => [], :start => []}

      _ ->
        case :maps.is_key(tag, socket_opt()) do
          true ->
            %{:socket => []}

          false ->
            case :maps.is_key(tag, ignore_opt()) do
              true ->
                %{:ignore => []}

              false ->
                :maps.merge(
                  case :maps.is_key(
                         tag,
                         server_read_opts()
                       ) do
                    true ->
                      %{:server_read => []}

                    false ->
                      %{}
                  end,
                  case :maps.is_key(tag, server_write_opts()) do
                    true ->
                      %{:server_write => []}

                    false ->
                      %{}
                  end
                )
            end
        end
    end
  end

  defp ignore_opt() do
    %{
      :tcp_module => [],
      :ip => [],
      :backlog => [],
      :high_msgq_watermark => [],
      :high_watermark => [],
      :low_msgq_watermark => [],
      :nopush => []
    }
  end

  defp socket_opt() do
    %{
      :buffer => {:otp, :rcvbuf},
      :debug => {:otp, :debug},
      :fd => {:otp, :fd},
      :bind_to_device => {:socket, :bindtodevice},
      :dontroute => {:socket, :dontroute},
      :keepalive => {:socket, :keepalive},
      :linger => {:socket, :linger},
      :low_watermark => {:socket, :rcvlowat},
      :priority => {:socket, :priority},
      :recbuf => {:socket, :rcvbuf},
      :reuseaddr => {:socket, :reuseaddr},
      :sndbuf => {:socket, :sndbuf},
      :nodelay => {:tcp, :nodelay},
      :recvtos => {:ip, :recvtos},
      :recvttl => {:ip, :recvttl},
      :tos => {:ip, :tos},
      :ttl => {:ip, :ttl},
      :recvtclass => {:ipv6, :recvtclass},
      :ipv6_v6only => {:ipv6, :v6only}
    }
  end

  defp socket_inherit_opts() do
    [:priority]
  end

  defp server_read_write_opts() do
    %{:packet => :raw, :packet_size => 67_108_864, :show_econnreset => false}
  end

  defp server_read_opts() do
    :maps.merge(
      %{
        :active => true,
        :mode => :list,
        :header => 0,
        :deliver => :term,
        :start_opts => [],
        :exit_on_close => true,
        :line_delimiter => ?\n
      },
      server_read_write_opts()
    )
  end

  defp server_write_opts() do
    :maps.merge(
      %{:send_timeout => :infinity, :send_timeout_close => false, :delay_send => false},
      server_read_write_opts()
    )
  end

  defp server_opts() do
    :maps.merge(server_read_opts(), server_write_opts())
  end

  defp meta(d) do
    :maps.with(:maps.keys(server_write_opts()), d)
  end

  defp start_server(domain, extraOpts, startOpts) do
    owner = self()
    arg = {:open, domain, extraOpts, owner}

    case :gen_statem.start(:gen_tcp_socket, arg, startOpts) do
      {:ok, server} ->
        {:ok, server}

      {:error, _} = error ->
        error
    end
  end

  defp start_server(serverData, startOpts) do
    owner = self()
    arg = {:prepare, serverData, owner}

    case :gen_statem.start(:gen_tcp_socket, arg, startOpts) do
      {:ok, server} ->
        {:ok, server}

      {:error, _} = error ->
        error
    end
  end

  defp call(server, call) do
    try do
      :gen_statem.call(server, call)
    catch
      :exit, {:noproc, {:gen_statem, :call, _Args}} ->
        {:error, :closed}
    end
  end

  defp stop_server(server) do
    try do
      :gen_statem.stop(server)
    catch
      _, _ ->
        :ok
    else
      _ ->
        :ok
    end
  end

  def callback_mode() do
    :handle_event_function
  end

  Record.defrecord(:r_controlling_process, :controlling_process,
    owner: :undefined,
    state: :undefined
  )

  Record.defrecord(:r_accept, :accept,
    info: :undefined,
    from: :undefined,
    listen_socket: :undefined
  )

  Record.defrecord(:r_connect, :connect, info: :undefined, from: :undefined, addr: :undefined)
  Record.defrecord(:r_recv, :recv, info: :undefined)

  Record.defrecord(:r_params, :params,
    socket: :undefined,
    owner: :undefined,
    owner_mon: :undefined
  )

  def init({:open, domain, extraOpts, owner}) do
    :erlang.process_flag(:trap_exit, true)
    ownerMon = :erlang.monitor(:process, owner)
    extra = :maps.from_list(extraOpts)

    proto =
      cond do
        domain === :local ->
          :default

        true ->
          :tcp
      end

    case :socket.open(domain, :stream, proto, extra) do
      {:ok, socket} ->
        d = server_opts()
        :ok = :socket.setopt(socket, {:otp, :iow}, true)
        :ok = :socket.setopt(socket, {:otp, :meta}, meta(d))
        p = r_params(socket: socket, owner: owner, owner_mon: ownerMon)
        {:ok, :connect, {p, %{d | :buffer => <<>>}}}

      {:error, reason} ->
        {:stop, {:shutdown, reason}}
    end
  end

  def init({:prepare, d, owner}) do
    :erlang.process_flag(:trap_exit, true)
    ownerMon = :erlang.monitor(:process, owner)
    p = r_params(owner: owner, owner_mon: ownerMon)
    {:ok, :accept, {p, %{d | :buffer => <<>>}}}
  end

  def init(arg) do
    :error_logger.error_report([{:badarg, {:gen_tcp_socket, :init, [arg]}}])
    :erlang.error(:badarg, [arg])
  end

  def terminate(_Reason, state, p_D) do
    case state do
      r_controlling_process(state: oldState) ->
        terminate(oldState, p_D)

      _ ->
        terminate(state, p_D)
    end
  end

  defp terminate(state, {r_params(socket: socket) = p, d}) do
    case state do
      :closed ->
        :ok

      :closed_read ->
        _ = socket_close(socket)
        :ok

      _ ->
        case state do
          :accept ->
            :ok

          r_accept() ->
            :ok

          _ ->
            _ = socket_close(socket)
            :ok
        end

        {_D_1, actionsR} =
          case state do
            r_controlling_process(state: oldState) ->
              cleanup_close_read(p, d, oldState, :closed)

            _ ->
              cleanup_close_read(p, d, state, :closed)
          end

        for {:reply, _From, _Msg} = reply <- reverse(actionsR) do
          :gen_statem.reply(reply)
        end

        :ok
    end

    :void
  end

  defp module_socket(r_params(socket: socket)) do
    {:"$inet", :gen_tcp_socket, {self(), socket}}
  end

  defp is_packet_option_value(value) do
    case value do
      0 ->
        true

      1 ->
        true

      2 ->
        true

      4 ->
        true

      :raw ->
        true

      :sunrm ->
        true

      :asn1 ->
        true

      :cdr ->
        true

      :fcgi ->
        true

      :line ->
        true

      :tpkt ->
        true

      :http ->
        true

      :httph ->
        true

      :http_bin ->
        true

      :httph_bin ->
        true

      _ ->
        false
    end
  end

  def handle_event({:call, from}, :get_server_opts, _State, {_P, d}) do
    serverData = :maps.with(:maps.keys(server_opts()), d)
    {:keep_state_and_data, [{:reply, from, {:ok, serverData}}]}
  end

  def handle_event(
        :info,
        {:DOWN, ownerMon, _, _, reason},
        _State,
        {r_params(owner_mon: ownerMon) = _P, _D} = p_D
      ) do
    {:stop, {:shutdown, reason}, p_D}
  end

  def handle_event(
        :info,
        {:"$socket", socket, :counter_wrap, counter},
        :connected = _State,
        {r_params(socket: socket) = p, d}
      ) do
    {:keep_state, {p, wrap_counter(counter, d)}}
  end

  def handle_event(
        :info,
        {:"$socket", socket, :counter_wrap, counter},
        r_recv() = _State,
        {r_params(socket: socket) = p, d}
      ) do
    {:keep_state, {p, wrap_counter(counter, d)}}
  end

  def handle_event(:info, {:"$socket", _Socket, :counter_wrap, _Counter}, _State, _P_D) do
    {:keep_state_and_data, [:postpone]}
  end

  def handle_event(
        {:call, {caller, _} = from},
        {:controlling_process, newOwner},
        state,
        {p, _D} = p_D
      ) do
    case p do
      r_params(owner: ^newOwner) ->
        {:keep_state_and_data, [{:reply, from, :ok}]}

      r_params(owner: ^caller) ->
        {:next_state, r_controlling_process(owner: newOwner, state: state), p_D,
         [{:reply, from, :transfer}]}

      r_params() ->
        {:keep_state_and_data, [{:reply, from, {:error, :not_owner}}]}
    end
  end

  def handle_event(
        {:call, {owner, _} = from},
        :controlling_process,
        r_controlling_process(owner: newOwner, state: state),
        {r_params(owner: owner, owner_mon: ownerMon) = p, d}
      ) do
    newOwnerMon = :erlang.monitor(:process, newOwner)
    true = :erlang.demonitor(ownerMon, [:flush])

    {:next_state, state, {r_params(p, owner: newOwner, owner_mon: newOwnerMon), d},
     [{:reply, from, :ok}]}
  end

  def handle_event(_Type, _Content, r_controlling_process(), _StateData) do
    {:keep_state_and_data, [:postpone]}
  end

  def handle_event({:call, from}, :close, state, {p, d} = p_D) do
    case state do
      :closed_read ->
        {:next_state, :closed, p_D, [{:reply, from, socket_close(r_params(p, :socket))}]}

      :closed ->
        {:keep_state_and_data, [{:reply, from, :ok}]}

      _ ->
        next_state(p, cleanup_close_read(p, %{d | :active => false}, state, :closed), :closed, [
          {:reply, from, socket_close(r_params(p, :socket))}
        ])
    end
  end

  def handle_event({:call, from}, {:getopts, opts}, state, {p, d}) do
    result = state_getopts(p, d, state, opts)
    {:keep_state_and_data, [{:reply, from, result}]}
  end

  def handle_event({:call, from}, {:setopts, opts}, state, {p, d}) do
    {result, d_1} = state_setopts(p, d, state, opts)
    :ok = :socket.setopt(r_params(p, :socket), {:otp, :meta}, meta(d_1))
    reply = {:reply, from, result}

    case state do
      :connected ->
        handle_connected(p, d_1, [reply])

      _ ->
        {:keep_state, {p, d_1}, [reply]}
    end
  end

  def handle_event({:call, from}, {:getstat, what}, state, {p, d}) do
    case state do
      :closed ->
        {:keep_state_and_data, [{:reply, from, {:error, :closed}}]}

      _ ->
        {d_1, result} = getstat(r_params(p, :socket), d, what)
        {:keep_state, {p, d_1}, [{:reply, from, {:ok, result}}]}
    end
  end

  def handle_event(type, content, :closed = state, p_D) do
    handle_closed(type, content, state, p_D)
  end

  def handle_event({:call, from}, :shutdown_read, state, {p, d}) do
    case state do
      :closed_read ->
        {:keep_state_and_data, [{:reply, from, :ok}]}

      _ ->
        next_state(
          p,
          cleanup_close_read(p, %{d | :active => false}, state, :closed),
          :closed_read,
          [{:reply, from, :socket.shutdown(r_params(p, :socket), :read)}]
        )
    end
  end

  def handle_event(type, content, :closed_read = state, p_D) do
    handle_closed(type, content, state, p_D)
  end

  def handle_event({:call, from}, {:accept, listenSocket, timeout}, :accept = _State, {p, d}) do
    handle_accept(p, d, from, listenSocket, timeout)
  end

  def handle_event(type, content, :accept = state, p_D) do
    handle_unexpected(type, content, state, p_D)
  end

  def handle_event(
        :info,
        {:"$socket", listenSocket, :select, selectRef},
        r_accept(info: {:select_info, _, selectRef}, from: from, listen_socket: listenSocket),
        {p, d}
      ) do
    handle_accept(p, d, from, listenSocket, :update)
  end

  def handle_event(
        :info,
        {:"$socket", listenSocket, :abort, {selectRef, reason}},
        r_accept(info: {:select_info, _, selectRef}, from: from, listen_socket: listenSocket),
        {p, d}
      ) do
    {:next_state, :closed, {p, d}, [{:reply, from, {:error, reason}}]}
  end

  def handle_event(
        {:timeout, :accept},
        :accept,
        r_accept(info: selectInfo, from: from, listen_socket: listenSocket),
        {p, d}
      ) do
    socket_cancel(listenSocket, selectInfo)
    {:next_state, :closed, {p, d}, [{:reply, from, {:error, :timeout}}]}
  end

  def handle_event(type, content, r_accept() = state, p_D) do
    handle_unexpected(type, content, state, p_D)
  end

  def handle_event({:call, from}, {:bind, bindAddr}, _State, {p, _D}) do
    result = :socket.bind(r_params(p, :socket), bindAddr)
    {:keep_state_and_data, [{:reply, from, result}]}
  end

  def handle_event({:call, from}, {:listen, backlog}, _State, {r_params(socket: socket) = _P, _D}) do
    result =
      case :socket.listen(socket, backlog) do
        :ok ->
          {:ok, socket}

        {:error, _} = error ->
          error
      end

    {:keep_state_and_data, [{:reply, from, result}]}
  end

  def handle_event(
        {:call, from},
        {:recv, _Length, _Timeout},
        _State,
        {_P, %{:active => active} = _D}
      )
      when active !== false do
    {:keep_state_and_data, [{:reply, from, {:error, :einval}}]}
  end

  def handle_event({:call, from}, {:connect, addr, timeout}, :connect = _State, {p, d}) do
    handle_connect(p, d, from, addr, timeout)
  end

  def handle_event({:call, from}, {:recv, _Length, _Timeout}, :connect = _State, _P_D) do
    {:keep_state_and_data, [{:reply, from, {:error, :enotconn}}]}
  end

  def handle_event(type, content, :connect = state, p_D) do
    handle_unexpected(type, content, state, p_D)
  end

  def handle_event(
        :info,
        {:"$socket", socket, :select, selectRef},
        r_connect(info: {:select_info, _, selectRef}, from: from, addr: addr) = _State,
        {r_params(socket: socket) = p, d}
      ) do
    handle_connect(p, d, from, addr, :update)
  end

  def handle_event(
        :info,
        {:"$socket", socket, :abort, {selectRef, reason}},
        r_connect(
          info: {:select_info, _, selectRef},
          from: from
        ) = _State,
        {r_params(socket: socket) = _P, _D} = p_D
      ) do
    _ = socket_close(socket)
    {:next_state, :closed, p_D, [{:reply, from, {:error, reason}}]}
  end

  def handle_event(
        {:timeout, :connect},
        :connect,
        r_connect(info: selectInfo, from: from),
        {r_params(socket: socket) = _P, _D} = p_D
      ) do
    socket_cancel(socket, selectInfo)
    _ = socket_close(socket)
    {:next_state, :closed, p_D, [{:reply, from, {:error, :timeout}}]}
  end

  def handle_event({:call, from}, {:recv, _Length, _Timeout}, r_connect() = _State, _P_D) do
    {:keep_state_and_data, [{:reply, from, {:error, :enotconn}}]}
  end

  def handle_event(type, content, r_connect() = state, p_D) do
    handle_unexpected(type, content, state, p_D)
  end

  def handle_event({:call, from}, {:recv, length, timeout}, state, {p, d}) do
    case state do
      :connected ->
        handle_recv_start(p, d, from, length, timeout)

      r_recv() ->
        {:keep_state_and_data, [:postpone]}
    end
  end

  def handle_event(
        :info,
        {:"$socket", socket, :select, selectRef},
        r_recv(info: {:select_info, _, selectRef}),
        {r_params(socket: socket) = p, d}
      ) do
    handle_recv(p, d, [])
  end

  def handle_event(
        :info,
        {:"$socket", socket, :abort, {selectRef, reason}},
        r_recv(info: {:select_info, _, selectRef}),
        {r_params(socket: socket) = p, d}
      ) do
    handle_connected(
      p,
      cleanup_recv_reply(p, d, [], reason)
    )
  end

  def handle_event({:timeout, :recv}, :recv, r_recv() = state, {p, d}) do
    handle_connected(p, cleanup_recv(p, d, state, :timeout))
  end

  def handle_event(type, content, state, p_D) do
    handle_unexpected(type, content, state, p_D)
  end

  defp handle_unexpected(type, content, state, {p, _D}) do
    :error_logger.warning_report([
      {:module, :gen_tcp_socket},
      {:socket, r_params(p, :socket)},
      {:unknown_event, {type, content}},
      {:state, state}
    ])

    case type do
      {:call, from} ->
        {:keep_state_and_data, [{:reply, from, {:error, :einval}}]}

      _ ->
        :keep_state_and_data
    end
  end

  defp handle_closed(type, content, state, {p, _D}) do
    case type do
      {:call, from} ->
        {:keep_state_and_data, [{:reply, from, {:error, :closed}}]}

      _ ->
        :error_logger.warning_report([
          {:module, :gen_tcp_socket},
          {:socket, r_params(p, :socket)},
          {:unknown_event, {type, content}},
          {:state, state}
        ])

        :keep_state_and_data
    end
  end

  defp handle_connect(r_params(socket: socket) = p, d, from, addr, timeout) do
    case :socket.connect(socket, addr, :nowait) do
      :ok ->
        handle_connected(p, d, [{{:timeout, :connect}, :cancel}, {:reply, from, {:ok, socket}}])

      {:select, selectInfo} ->
        {:next_state, r_connect(info: selectInfo, from: from, addr: addr), {p, d},
         [{{:timeout, :connect}, timeout, :connect}]}

      {:error, _} = error ->
        {:next_state, :connect, {p, d}, [{{:timeout, :connect}, :cancel}, {:reply, from, error}]}
    end
  end

  defp handle_accept(p, d, from, listenSocket, timeout) do
    case :socket.accept(listenSocket, :nowait) do
      {:ok, socket} ->
        :ok = :socket.setopt(socket, {:otp, :iow}, true)
        :ok = :socket.setopt(socket, {:otp, :meta}, meta(d))

        for opt <- socket_inherit_opts() do
          :ok = socket_copy_opt(listenSocket, opt, socket)
        end

        handle_connected(r_params(p, socket: socket), d, [
          {{:timeout, :accept}, :cancel},
          {:reply, from, {:ok, socket}}
        ])

      {:select, selectInfo} ->
        {:next_state, r_accept(info: selectInfo, from: from, listen_socket: listenSocket), {p, d},
         [{{:timeout, :accept}, timeout, :accept}]}

      {:error, _} = error ->
        {:next_state, :accept, {p, d}, [{{:timeout, :accept}, :cancel}, {:reply, from, error}]}
    end
  end

  defp handle_connected(p, {d, actionsR}) do
    handle_connected(p, d, actionsR)
  end

  defp handle_connected(p, d, actionsR) do
    case d do
      %{:active => false} ->
        {:next_state, :connected, {p, d}, reverse(actionsR)}

      %{:active => _} ->
        handle_recv(p, recv_start(d), actionsR)
    end
  end

  defp handle_recv_start(p, %{:packet => packet, :buffer => buffer} = d, from, length, timeout)
       when (packet === :raw and 0 < length) or
              (packet === 0 and 0 < length) do
    size = :erlang.iolist_size(buffer)

    cond do
      length <= size ->
        {data, newBuffer} =
          :erlang.split_binary(
            condense_buffer(buffer),
            length
          )

        handle_recv_deliver(
          p,
          %{d | :recv_length => length, :recv_from => from, :buffer => newBuffer},
          [],
          data
        )

      true ->
        n = length - size

        handle_recv(
          p,
          %{d | :recv_length => n, :recv_from => from},
          [{{:timeout, :recv}, timeout, :recv}]
        )
    end
  end

  defp handle_recv_start(p, d, from, _Length, timeout) do
    handle_recv(
      p,
      %{d | :recv_length => 0, :recv_from => from},
      [{{:timeout, :recv}, timeout, :recv}]
    )
  end

  defp handle_recv(
         p,
         %{:packet => packet, :recv_length => length} = d,
         actionsR
       ) do
    cond do
      0 < length ->
        handle_recv_length(p, d, actionsR, length)

      packet === :raw or packet === 0 ->
        handle_recv_length(p, d, actionsR, length)

      packet === 1 or packet === 2 or packet === 4 ->
        handle_recv_peek(p, d, actionsR, packet)

      true ->
        handle_recv_packet(p, d, actionsR)
    end
  end

  defp handle_recv_peek(p, d, actionsR, packet) do
    case d do
      %{:buffer => buffer} when is_list(buffer) ->
        data = condense_buffer(buffer)
        handle_recv_peek(p, %{d | :buffer => data}, actionsR, packet)

      %{:buffer => <<data::size(packet)-binary, _Rest::binary>>} ->
        handle_recv_peek(p, d, actionsR, packet, data)

      %{:buffer => <<shortData::binary>>} ->
        n = packet - byte_size(shortData)

        case socket_recv_peek(r_params(p, :socket), n) do
          {:ok, <<finalData::binary>>} ->
            handle_recv_peek(p, d, actionsR, packet, <<shortData::binary, finalData::binary>>)

          {:ok, {_, selectInfo}} ->
            {:next_state, r_recv(info: selectInfo), {p, d}, reverse(actionsR)}

          {:select, selectInfo} ->
            {:next_state, r_recv(info: selectInfo), {p, d}, reverse(actionsR)}

          {:error, {reason, <<_Data::binary>>}} ->
            handle_recv_error(p, d, actionsR, reason)

          {:error, reason} ->
            handle_recv_error(p, d, actionsR, reason)
        end
    end
  end

  defp handle_recv_peek(p, d, actionsR, packet, data) do
    <<n::size(packet)-unit(8)-integer-big-unsigned>> = data
    %{:packet_size => packetSize} = d

    cond do
      0 < packetSize and packetSize < n ->
        handle_recv_error(p, d, actionsR, :emsgsize)

      true ->
        handle_recv_length(p, d, actionsR, packet + n)
    end
  end

  defp handle_recv_packet(p, d, actionsR) do
    case d do
      %{:buffer => buffer} when is_list(buffer) ->
        data = condense_buffer(buffer)
        handle_recv_decode(p, d, actionsR, data)

      %{:buffer => data} when is_binary(data) ->
        handle_recv_more(p, d, actionsR, data)
    end
  end

  defp handle_recv_length(p, %{:buffer => buffer} = d, actionsR, length) do
    handle_recv_length(p, d, actionsR, length, buffer)
  end

  defp handle_recv_length(p, d, actionsR, length, buffer)
       when 0 < length do
    case socket_recv(r_params(p, :socket), length) do
      {:ok, <<data::binary>>} ->
        handle_recv_deliver(p, %{d | :buffer => <<>>}, actionsR, condense_buffer([data | buffer]))

      {:ok, {data, selectInfo}} ->
        n = length - byte_size(data)

        {:next_state, r_recv(info: selectInfo),
         {p, %{d | :buffer => [data | buffer], :recv_length => n}}, reverse(actionsR)}

      {:select, selectInfo} ->
        {:next_state, r_recv(info: selectInfo), {p, %{d | :buffer => buffer}}, reverse(actionsR)}

      {:error, {reason, <<data::binary>>}} ->
        handle_recv_error(p, %{d | :buffer => [data | buffer]}, actionsR, reason)

      {:error, reason} ->
        handle_recv_error(p, %{d | :buffer => buffer}, actionsR, reason)
    end
  end

  defp handle_recv_length(p, d, actionsR, _0, buffer) do
    case buffer do
      <<>> ->
        socket = r_params(p, :socket)

        case socket_recv(socket, 0) do
          {:ok, <<data::binary>>} ->
            handle_recv_deliver(p, d, actionsR, data)

          {:ok, {data, selectInfo}} ->
            case :socket.cancel(socket, selectInfo) do
              :ok ->
                handle_recv_deliver(p, d, actionsR, data)

              {:error, reason} ->
                handle_recv_error(p, d, actionsR, reason, data)
            end

          {:select, selectInfo} ->
            {:next_state, r_recv(info: selectInfo), {p, d}, reverse(actionsR)}

          {:error, {reason, <<data::binary>>}} ->
            handle_recv_error(p, d, actionsR, reason, data)

          {:error, reason} ->
            handle_recv_error(p, d, actionsR, reason)
        end

      <<data::binary>> ->
        handle_recv_deliver(p, %{d | :buffer => <<>>}, actionsR, data)

      _ when is_list(buffer) ->
        data = condense_buffer(buffer)
        handle_recv_deliver(p, %{d | :buffer => <<>>}, actionsR, data)
    end
  end

  defp handle_recv_decode(p, %{:packet_size => packetSize} = d, actionsR, data) do
    case :erlang.decode_packet(decode_packet(d), data, [
           {:packet_size, packetSize},
           {:line_length, packetSize}
         ]) do
      {:ok, decoded, rest} ->
        buffer =
          case rest do
            <<>> ->
              rest

            <<_::binary>> ->
              [rest]
          end

        handle_recv_deliver(p, %{d | :buffer => buffer}, actionsR, decoded)

      {:more, :undefined} ->
        handle_recv_more(p, d, actionsR, data)

      {:more, length} ->
        n = length - byte_size(data)
        handle_recv_length(p, d, actionsR, n, data)

      {:error, reason} ->
        handle_recv_error(
          p,
          %{d | :buffer => data},
          actionsR,
          case reason do
            :invalid ->
              :emsgsize

            _ ->
              reason
          end
        )
    end
  end

  defp handle_recv_error_decode(p, %{:packet_size => packetSize} = d, actionsR, reason, data) do
    case :erlang.decode_packet(decode_packet(d), data, [
           {:packet_size, packetSize},
           {:line_length, packetSize}
         ]) do
      {:ok, decoded, rest} ->
        buffer =
          case rest do
            <<>> ->
              rest

            <<_::binary>> ->
              [rest]
          end

        handle_recv_error(p, %{d | :buffer => buffer}, actionsR, reason, decoded)

      {:more, _} ->
        handle_recv_error(p, %{d | :buffer => data}, actionsR, reason)

      {:error, ^reason} ->
        handle_recv_error(
          p,
          %{d | :buffer => data},
          actionsR,
          case reason do
            :invalid ->
              :emsgsize

            _ ->
              reason
          end
        )
    end
  end

  defp handle_recv_more(p, d, actionsR, bufferedData) do
    case socket_recv(r_params(p, :socket), 0) do
      {:ok, <<moreData::binary>>} ->
        data = catbin(bufferedData, moreData)
        handle_recv_decode(p, d, actionsR, data)

      {:select, selectInfo} ->
        {:next_state, r_recv(info: selectInfo), {p, %{d | :buffer => bufferedData}},
         reverse(actionsR)}

      {:error, {reason, <<moreData::binary>>}} ->
        data = catbin(bufferedData, moreData)
        handle_recv_error_decode(p, d, actionsR, reason, data)

      {:error, reason} ->
        handle_recv_error(p, %{d | :buffer => bufferedData}, actionsR, reason)
    end
  end

  defp handle_recv_deliver(p, d, actionsR, data) do
    handle_connected(
      p,
      recv_data_deliver(p, d, actionsR, data)
    )
  end

  defp handle_recv_error(p, d, actionsR, reason, data) do
    {d_1, actionsR_1} = recv_data_deliver(p, d, actionsR, data)
    handle_recv_error(p, d_1, actionsR_1, reason)
  end

  defp handle_recv_error(p, d, actionsR, reason) do
    {d_1, actionsR_1} = cleanup_recv_reply(p, %{d | :buffer => <<>>}, actionsR, reason)

    case reason do
      :closed ->
        {:next_state, :closed_read, {p, d_1}, reverse(actionsR_1)}

      :econnreset ->
        _ = socket_close(r_params(p, :socket))
        {:next_state, :closed, {p, d_1}, reverse(actionsR_1)}

      :emsgsize ->
        {:next_state, :connected, {p, recv_stop(%{d | :active => false})}, reverse(actionsR_1)}
    end
  end

  defp next_state(p, {d, actionsR}, state, actions) do
    {:next_state, state, {p, d}, reverse(actionsR, actions)}
  end

  defp cleanup_close_read(p, d, state, reason) do
    case state do
      r_accept(info: selectInfo, from: from, listen_socket: listenSocket) ->
        socket_cancel(listenSocket, selectInfo)
        {d, [{:reply, from, {:error, reason}}]}

      r_connect(info: selectInfo, from: from) ->
        socket_cancel(r_params(p, :socket), selectInfo)
        {d, [{:reply, from, {:error, reason}}]}

      _ ->
        cleanup_recv(p, d, state, reason)
    end
  end

  defp cleanup_recv(p, d, state, reason) do
    case state do
      r_recv(info: selectInfo) ->
        socket_cancel(r_params(p, :socket), selectInfo)
        cleanup_recv_reply(p, d, [], reason)

      _ ->
        cleanup_recv_reply(p, d, [], reason)
    end
  end

  defp cleanup_recv_reply(p, %{:show_econnreset => showEconnreset} = d, actionsR, reason) do
    case d do
      %{:active => false} ->
        :ok

      %{:active => _} ->
        moduleSocket = module_socket(p)
        owner = r_params(p, :owner)

        case reason do
          :timeout ->
            send(owner, {:tcp_error, moduleSocket, reason})
            :ok

          :closed ->
            send(owner, {:tcp_closed, moduleSocket})
            :ok

          :emsgsize ->
            send(owner, {:tcp_error, moduleSocket, reason})
            :ok

          :econnreset when showEconnreset === false ->
            send(owner, {:tcp_closed, moduleSocket})
            :ok

          _ ->
            send(owner, {:tcp_error, moduleSocket, reason})
            send(owner, {:tcp_closed, moduleSocket})
            :ok
        end
    end

    {recv_stop(%{d | :active => false}),
     case d do
       %{:recv_from => from} ->
         reason_1 =
           case reason do
             :econnreset when showEconnreset === false ->
               :closed

             _ ->
               reason
           end

         [
           [{:reply, from, {:error, reason_1}}, {{:timeout, :recv}, :cancel}]
           | actionsR
         ]

       %{} ->
         actionsR
     end}
  end

  defp recv_start(d) do
    %{d | :recv_length => 0}
  end

  defp recv_stop(d) do
    :maps.without([:recv_from, :recv_length], d)
  end

  defp decode_packet(%{:packet => packet} = d) do
    case d do
      %{:packet => :http, :recv_httph => true} ->
        :httph

      %{:packet => :http_bin, :recv_httph => true} ->
        :httph_bin

      %{:packet => ^packet} ->
        packet
    end
  end

  defp recv_data_deliver(
         r_params(owner: owner) = p,
         %{:mode => mode, :header => header, :deliver => deliver, :packet => packet} = d,
         actionsR,
         data
       ) do
    deliverData = deliver_data(data, mode, header, packet)

    case d do
      %{:recv_from => from} ->
        {recv_stop(next_packet(d, packet, data)),
         [
           [{:reply, from, {:ok, deliverData}}, {{:timeout, :recv}, :cancel}]
           | actionsR
         ]}

      %{:active => false} ->
        d_1 = %{d | :buffer => unrecv_buffer(data, :maps.get(:buffer, d))}
        {recv_stop(next_packet(d_1, packet, data)), actionsR}

      %{:active => active} ->
        moduleSocket = module_socket(p)

        send(
          owner,
          case deliver do
            :term ->
              {tag(packet), moduleSocket, deliverData}

            :port ->
              {moduleSocket, {:data, deliverData}}
          end
        )

        case active do
          true ->
            {recv_start(next_packet(d, packet, data)), actionsR}

          :once ->
            {recv_stop(next_packet(d, packet, data, false)), actionsR}

          1 ->
            send(owner, {:tcp_passive, moduleSocket})
            {recv_stop(next_packet(d, packet, data, false)), actionsR}

          n when is_integer(n) ->
            {recv_start(next_packet(d, packet, data, active - 1)), actionsR}
        end
    end
  end

  defp next_packet(d, packet, data) do
    cond do
      packet === :http or packet === :http_bin ->
        case data do
          {:http_request, _HttpMethod, _HttpUri, _HttpVersion} ->
            %{d | :recv_httph => true}

          {:http_response, _HttpVersion, _Integer, _HttpString} ->
            %{d | :recv_httph => true}

          {:http_header, _Integer, _HttpField, _Reserver, _Value} ->
            d

          :http_eoh ->
            %{d | :recv_httph => false}

          {:http_error, _HttpString} ->
            d
        end

      true ->
        d
    end
  end

  defp next_packet(d, packet, data, active) do
    cond do
      packet === :http or packet === :http_bin ->
        case data do
          {:http_request, _HttpMethod, _HttpUri, _HttpVersion} ->
            %{d | :recv_httph => true, :active => active}

          {:http_response, _HttpVersion, _Integer, _HttpString} ->
            %{d | :recv_httph => true, :active => active}

          {:http_header, _Integer, _HttpField, _Reserver, _Value} ->
            %{d | :active => active}

          :http_eoh ->
            %{d | :recv_httph => false, :active => active}

          {:http_error, _HttpString} ->
            %{d | :active => active}
        end

      true ->
        %{d | :active => active}
    end
  end

  defp catbin(<<>>, bin) when is_binary(bin) do
    bin
  end

  defp catbin(bin, <<>>) when is_binary(bin) do
    bin
  end

  defp catbin(bin1, bin2)
       when is_binary(bin1) and
              is_binary(bin2) do
    <<bin1::binary, bin2::binary>>
  end

  defp unrecv_buffer(data, buffer) do
    case buffer do
      <<>> ->
        data

      _ when is_binary(buffer) ->
        [data, buffer]

      _ ->
        [data | buffer]
    end
  end

  defp condense_buffer([bin]) when is_binary(bin) do
    bin
  end

  defp condense_buffer(buffer) do
    :erlang.iolist_to_binary(reverse_improper(buffer, []))
  end

  defp deliver_data(data, mode, header, packet) do
    cond do
      packet === 1 or packet === 2 or packet === 4 ->
        <<_Size::size(packet)-unit(8)-integer-big-unsigned, payload::binary>> = data
        deliver_data(payload, mode, header)

      packet === :http or packet === :http_bin or
        packet === :httph or packet === :httph_bin ->
        data

      true ->
        deliver_data(data, mode, header)
    end
  end

  defp deliver_data(data, :list, _N) do
    :erlang.binary_to_list(data)
  end

  defp deliver_data(data, :binary, 0) do
    data
  end

  defp deliver_data(data, :binary, n) do
    case data do
      <<_::size(n)-binary>> ->
        :erlang.binary_to_list(data)

      <<header::size(n)-binary, payload::binary>> ->
        :erlang.binary_to_list(header) ++ payload
    end
  end

  defp tag(packet) do
    cond do
      packet === :http or packet === :http_bin or
        packet === :httph or packet === :httph_bin ->
        :http

      true ->
        :tcp
    end
  end

  defp state_setopts(_P, d, _State, []) do
    {:ok, d}
  end

  defp state_setopts(p, d, state, [opt | opts]) do
    opt_1 = conv_setopt(opt)

    case setopt_categories(opt_1) do
      %{:socket => _} ->
        case r_params(p, :socket) do
          :undefined ->
            {{:error, :closed}, d}

          socket ->
            case socket_setopt(socket, opt_1) do
              :ok ->
                state_setopts(p, d, state, opts)

              {:error, _} = error ->
                {error, d}
            end
        end

      %{:server_write => _} when state === :closed ->
        {{:error, :einval}, d}

      %{:server_write => _} ->
        state_setopts_server(p, d, state, opts, opt_1)

      %{:server_read => _} when state === :closed ->
        {{:error, :einval}, d}

      %{:server_read => _} when state === :closed_read ->
        {{:error, :einval}, d}

      %{:server_read => _} ->
        state_setopts_server(p, d, state, opts, opt_1)

      %{:ignore => _} ->
        state_setopts(p, d, state, opts)

      %{} ->
        {{:error, :einval}, d}
    end
  end

  defp state_setopts_server(p, d, state, opts, {tag, value}) do
    case tag do
      :active ->
        state_setopts_active(p, d, state, opts, value)

      :packet ->
        case is_packet_option_value(value) do
          true ->
            case d do
              %{:recv_httph => _} ->
                state_setopts(
                  p,
                  :maps.remove(
                    :recv_httph,
                    %{d | :packet => value}
                  ),
                  state,
                  opts
                )

              %{} ->
                state_setopts(p, %{d | :packet => value}, state, opts)
            end

          false ->
            {{:error, :einval}, d}
        end

      _ ->
        state_setopts(p, %{d | tag => value}, state, opts)
    end
  end

  defp state_setopts_active(p, d, state, opts, active) do
    cond do
      active === :once or active === true ->
        state_setopts(p, %{d | :active => active}, state, opts)

      active === false ->
        case d do
          %{:active => oldActive} when is_integer(oldActive) ->
            send(r_params(p, :owner), {:tcp_passive, module_socket(p)})
            :ok

          %{:active => _OldActive} ->
            :ok
        end

        state_setopts(p, %{d | :active => active}, state, opts)

      is_integer(active) and -32768 <= active and
          active <= 32767 ->
        n =
          case d do
            %{:active => oldActive} when is_integer(oldActive) ->
              oldActive + active

            %{:active => _OldActive} ->
              active
          end

        cond do
          32767 < n ->
            {{:error, :einval}, d}

          n <= 0 ->
            send(r_params(p, :owner), {:tcp_passive, module_socket(p)})
            state_setopts(p, %{d | :active => false}, state, opts)

          true ->
            state_setopts(p, %{d | :active => n}, state, opts)
        end

      true ->
        {{:error, :einval}, d}
    end
  end

  defp state_getopts(p, d, state, opts) do
    state_getopts(p, d, state, opts, [])
  end

  defp state_getopts(_P, _D, _State, [], acc) do
    {:ok, reverse(acc)}
  end

  defp state_getopts(p, d, state, [tag | tags], acc) do
    case getopt_categories(tag) do
      %{:socket => _} ->
        case r_params(p, :socket) do
          :undefined ->
            {:error, :closed}

          socket ->
            case socket_getopt(socket, tag) do
              {:ok, value} ->
                state_getopts(p, d, state, tags, [{tag, value} | acc])

              {:error, _} ->
                state_getopts(p, d, state, tags, acc)
            end
        end

      %{:server_write => _} when state === :closed ->
        {:error, :einval}

      %{:server_write => _} ->
        value = :maps.get(tag, d)
        state_getopts(p, d, state, tags, [{tag, value} | acc])

      %{:server_read => _} when state === :closed ->
        {:error, :einval}

      %{:server_read => _} when state === :closed_read ->
        {:error, :einval}

      %{:server_read => _} ->
        value = :maps.get(tag, d)
        state_getopts(p, d, state, tags, [{tag, value} | acc])

      %{} ->
        {:error, :einval}
    end
  end

  defp getstat(socket, d, what) do
    counters_1 = socket_info_counters(socket)
    {d_1, wrapped} = receive_counter_wrap(socket, d, [])
    counters_2 = socket_info_counters(socket)

    counters_3 =
      :maps.merge(
        counters_1,
        :maps.with(wrapped, counters_2)
      )

    {d_1, getstat_what(what, d_1, counters_3)}
  end

  defp getstat_what([], _D, _C) do
    []
  end

  defp getstat_what([tag | what], d, c) do
    val =
      case tag do
        :recv_oct ->
          counter_value(:read_byte, d, c)

        :recv_cnt ->
          counter_value(:read_pkg, d, c)

        :recv_max ->
          getstat_avg(:read_byte, d, c, :read_pkg)

        :recv_avg ->
          getstat_avg(:read_byte, d, c, :read_pkg)

        :recv_dvi ->
          0

        :send_oct ->
          counter_value(:write_byte, d, c)

        :send_cnt ->
          counter_value(:write_pkg, d, c)

        :send_max ->
          getstat_avg(:write_byte, d, c, :write_pkg)

        :send_avg ->
          getstat_avg(:write_byte, d, c, :write_pkg)

        :send_pend ->
          0
      end

    [{tag, val} | getstat_what(what, d, c)]
  end

  defp getstat_avg(sumTag, d, c, cntTag) do
    cnt = counter_value(cntTag, d, c)

    cond do
      cnt === 0 ->
        counter_value(sumTag, d, c)

      true ->
        round(counter_value(sumTag, d, c) / cnt)
    end
  end

  defp socket_info_counters(socket) do
    %{:counters => counters} = :socket.info(socket)
    counters
  end

  defp receive_counter_wrap(socket, d, wrapped) do
    receive do
      {:"$socket", ^socket, :counter_wrap, counter} ->
        receive_counter_wrap(socket, wrap_counter(counter, d), [counter | wrapped])
    after
      0 ->
        {d, wrapped}
    end
  end

  defp wrap_counter(counter, d) do
    case d do
      %{^counter => n} ->
        %{d | counter => n + 1}

      %{} ->
        %{d | counter => 1}
    end
  end

  defp counter_value(counter, d, counters) do
    case d do
      %{^counter => wraps} ->
        wraps <<< (32 + :maps.get(counter, counters))

      %{} ->
        :maps.get(counter, counters)
    end
  end

  defp reverse([]) do
    []
  end

  defp reverse([_] = l) do
    l
  end

  defp reverse([a, b]) do
    [b, a]
  end

  defp reverse(l) do
    :lists.reverse(l)
  end

  defp reverse([], l) do
    l
  end

  defp reverse([a], l) do
    [a | l]
  end

  defp reverse([a, b], l) do
    [[b, a] | l]
  end

  defp reverse(l1, l2) do
    :lists.reverse(l1, l2)
  end

  defp reverse_improper([h | t], acc) do
    reverse_improper(t, [h | acc])
  end

  defp reverse_improper([], acc) do
    acc
  end

  defp reverse_improper(t, acc) do
    [t | acc]
  end
end
