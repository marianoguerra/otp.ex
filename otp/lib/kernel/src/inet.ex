defmodule :m_inet do
  use Bitwise
  import Kernel, except: [send: 2]
  import :lists, only: [append: 1, duplicate: 2, filter: 2, foldl: 3]
  require Record

  Record.defrecord(:r_hostent, :hostent,
    h_name: :undefined,
    h_aliases: [],
    h_addrtype: :undefined,
    h_length: :undefined,
    h_addr_list: []
  )

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

  Record.defrecord(:r_sctp_initmsg, :sctp_initmsg,
    num_ostreams: :undefined,
    max_instreams: :undefined,
    max_attempts: :undefined,
    max_init_timeo: :undefined
  )

  Record.defrecord(:r_sctp_sndrcvinfo, :sctp_sndrcvinfo,
    stream: :undefined,
    ssn: :undefined,
    flags: :undefined,
    ppid: :undefined,
    context: :undefined,
    timetolive: :undefined,
    tsn: :undefined,
    cumtsn: :undefined,
    assoc_id: :undefined
  )

  Record.defrecord(:r_sctp_assoc_change, :sctp_assoc_change,
    state: :cant_assoc,
    error: 0,
    outbound_streams: 0,
    inbound_streams: 0,
    assoc_id: 0
  )

  Record.defrecord(:r_sctp_paddr_change, :sctp_paddr_change,
    addr: [0, 0, 0, 0],
    state: :addr_available,
    error: 0,
    assoc_id: 0
  )

  Record.defrecord(:r_sctp_remote_error, :sctp_remote_error, error: 0, assoc_id: 0, data: [])

  Record.defrecord(:r_sctp_send_failed, :sctp_send_failed,
    flags: false,
    error: 0,
    info: :EFE_TODO_NESTED_RECORD,
    assoc_id: 0,
    data: <<>>
  )

  Record.defrecord(:r_sctp_shutdown_event, :sctp_shutdown_event, assoc_id: 0)

  Record.defrecord(:r_sctp_adaptation_event, :sctp_adaptation_event,
    adaptation_ind: 0,
    assoc_id: 0
  )

  Record.defrecord(:r_sctp_pdapi_event, :sctp_pdapi_event,
    indication: :partial_delivery_aborted,
    assoc_id: 0
  )

  Record.defrecord(:r_sctp_rtoinfo, :sctp_rtoinfo,
    assoc_id: :undefined,
    initial: :undefined,
    max: :undefined,
    min: :undefined
  )

  Record.defrecord(:r_sctp_assocparams, :sctp_assocparams,
    assoc_id: :undefined,
    asocmaxrxt: :undefined,
    number_peer_destinations: :undefined,
    peer_rwnd: :undefined,
    local_rwnd: :undefined,
    cookie_life: :undefined
  )

  Record.defrecord(:r_sctp_prim, :sctp_prim,
    assoc_id: :undefined,
    addr: :undefined
  )

  Record.defrecord(:r_sctp_setpeerprim, :sctp_setpeerprim,
    assoc_id: :undefined,
    addr: :undefined
  )

  Record.defrecord(:r_sctp_setadaptation, :sctp_setadaptation, adaptation_ind: :undefined)

  Record.defrecord(:r_sctp_paddrparams, :sctp_paddrparams,
    assoc_id: :undefined,
    address: :undefined,
    hbinterval: :undefined,
    pathmaxrxt: :undefined,
    pathmtu: :undefined,
    sackdelay: :undefined,
    flags: :undefined
  )

  Record.defrecord(:r_sctp_event_subscribe, :sctp_event_subscribe,
    data_io_event: :undefined,
    association_event: :undefined,
    address_event: :undefined,
    send_failure_event: :undefined,
    peer_error_event: :undefined,
    shutdown_event: :undefined,
    partial_delivery_event: :undefined,
    adaptation_layer_event: :undefined,
    authentication_event: :undefined
  )

  Record.defrecord(:r_sctp_assoc_value, :sctp_assoc_value,
    assoc_id: :undefined,
    assoc_value: :undefined
  )

  Record.defrecord(:r_sctp_paddrinfo, :sctp_paddrinfo,
    assoc_id: :undefined,
    address: :undefined,
    state: :undefined,
    cwnd: :undefined,
    srtt: :undefined,
    rto: :undefined,
    mtu: :undefined
  )

  Record.defrecord(:r_sctp_status, :sctp_status,
    assoc_id: :undefined,
    state: :undefined,
    rwnd: :undefined,
    unackdata: :undefined,
    penddata: :undefined,
    instrms: :undefined,
    outstrms: :undefined,
    fragmentation_point: :undefined,
    primary: :undefined
  )

  def get_rc() do
    :inet_db.get_rc()
  end

  def close(socket) do
    :prim_inet.close(socket)

    receive do
      {closed, ^socket}
      when closed === :tcp_closed or
             closed === :udp_closed ->
        :ok
    after
      0 ->
        :ok
    end
  end

  def peername({:"$inet", genSocketMod, _} = socket)
      when is_atom(genSocketMod) do
    genSocketMod.peername(socket)
  end

  def peername(socket) do
    :prim_inet.peername(socket)
  end

  def setpeername(socket, {iP, port}) do
    :prim_inet.setpeername(socket, {iP, port})
  end

  def setpeername(socket, :undefined) do
    :prim_inet.setpeername(socket, :undefined)
  end

  def peernames(socket) do
    :prim_inet.peernames(socket)
  end

  def peernames(socket, assoc) do
    :prim_inet.peernames(socket, assoc)
  end

  def sockname({:"$inet", genSocketMod, _} = socket)
      when is_atom(genSocketMod) do
    genSocketMod.sockname(socket)
  end

  def sockname(socket) do
    :prim_inet.sockname(socket)
  end

  def setsockname(socket, {iP, port}) do
    :prim_inet.setsockname(socket, {iP, port})
  end

  def setsockname(socket, :undefined) do
    :prim_inet.setsockname(socket, :undefined)
  end

  def socknames(socket) do
    :prim_inet.socknames(socket)
  end

  def socknames(socket, assoc) do
    :prim_inet.socknames(socket, assoc)
  end

  def port({:"$inet", genSocketMod, _} = socket)
      when is_atom(genSocketMod) do
    case genSocketMod.sockname(socket) do
      {:ok, {_, port}} ->
        {:ok, port}

      {:error, _} = error ->
        error
    end
  end

  def port(socket) do
    case :prim_inet.sockname(socket) do
      {:ok, {_, port}} ->
        {:ok, port}

      error ->
        error
    end
  end

  def send(socket, packet) do
    :prim_inet.send(socket, packet)
  end

  def setopts({:"$inet", genSocketMod, _} = socket, opts)
      when is_atom(genSocketMod) do
    genSocketMod.setopts(socket, opts)
  end

  def setopts(socket, opts) do
    socketOpts =
      for opt <- opts do
        case opt do
          {:netns, nS} ->
            {:netns, filename2binary(nS)}

          _ ->
            opt
        end
      end

    :prim_inet.setopts(socket, socketOpts)
  end

  def getopts({:"$inet", genSocketMod, _} = socket, opts)
      when is_atom(genSocketMod) do
    genSocketMod.getopts(socket, opts)
  end

  def getopts(socket, opts) do
    case :prim_inet.getopts(socket, opts) do
      {:ok, optionValues} ->
        {:ok,
         for optionValue <- optionValues do
           case optionValue do
             {:netns, bin} ->
               {:netns, binary2filename(bin)}

             _ ->
               optionValue
           end
         end}

      other ->
        other
    end
  end

  def getifaddrs(opts) when is_list(opts) do
    withsocket(
      fn s ->
        :prim_inet.getifaddrs(s)
      end,
      opts
    )
  end

  def getifaddrs(socket) do
    :prim_inet.getifaddrs(socket)
  end

  def getifaddrs() do
    withsocket(fn s ->
      :prim_inet.getifaddrs(s)
    end)
  end

  def getiflist(opts) when is_list(opts) do
    withsocket(
      fn s ->
        :prim_inet.getiflist(s)
      end,
      opts
    )
  end

  def getiflist(socket) do
    :prim_inet.getiflist(socket)
  end

  def getiflist() do
    withsocket(fn s ->
      :prim_inet.getiflist(s)
    end)
  end

  def ifget(socket, name, opts) do
    :prim_inet.ifget(socket, name, opts)
  end

  def ifget(name, opts) do
    {nSOpts, iFOpts} =
      :lists.partition(
        fn
          {:netns, _} ->
            true

          _ ->
            false
        end,
        opts
      )

    withsocket(
      fn s ->
        :prim_inet.ifget(s, name, iFOpts)
      end,
      nSOpts
    )
  end

  def ifset(socket, name, opts) do
    :prim_inet.ifset(socket, name, opts)
  end

  def ifset(name, opts) do
    {nSOpts, iFOpts} =
      :lists.partition(
        fn
          {:netns, _} ->
            true

          _ ->
            false
        end,
        opts
      )

    withsocket(
      fn s ->
        :prim_inet.ifset(s, name, iFOpts)
      end,
      nSOpts
    )
  end

  def getif() do
    withsocket(fn s ->
      getif(s)
    end)
  end

  def getif(opts) when is_list(opts) do
    withsocket(
      fn s ->
        getif(s)
      end,
      opts
    )
  end

  def getif(socket) do
    case :prim_inet.getiflist(socket) do
      {:ok, ifList} ->
        {:ok,
         :lists.foldl(
           fn name, acc ->
             case :prim_inet.ifget(socket, name, [:addr, :broadaddr, :netmask]) do
               {:ok, [{:addr, a}, {:broadaddr, b}, {:netmask, m}]} ->
                 [{a, b, m} | acc]

               {:ok, [{:addr, a}, {:netmask, m}]} ->
                 [{a, :undefined, m} | acc]

               _ ->
                 acc
             end
           end,
           [],
           ifList
         )}

      error ->
        error
    end
  end

  defp withsocket(fun) do
    withsocket(fun, [])
  end

  defp withsocket(fun, opts) do
    case :inet_udp.open(0, opts) do
      {:ok, socket} ->
        res = fun.(socket)
        :inet_udp.close(socket)
        res

      error ->
        error
    end
  end

  def pushf(_Socket, fun, _State) when is_function(fun) do
    {:error, :einval}
  end

  def popf(_Socket) do
    {:error, :einval}
  end

  def gethostname() do
    case :inet_udp.open(0, []) do
      {:ok, u} ->
        {:ok, res} = gethostname(u)
        :inet_udp.close(u)

        {res2, _} =
          :lists.splitwith(
            fn
              ?. ->
                false

              _ ->
                true
            end,
            res
          )

        {:ok, res2}

      _ ->
        {:ok, 'nohost.nodomain'}
    end
  end

  def gethostname(socket) do
    :prim_inet.gethostname(socket)
  end

  def getstat(socket) do
    getstat(socket, stats())
  end

  def getstat({:"$inet", genSocketMod, _} = socket, what)
      when is_atom(genSocketMod) do
    genSocketMod.getstat(socket, what)
  end

  def getstat(socket, what) do
    :prim_inet.getstat(socket, what)
  end

  def gethostbyname(name) do
    case :inet_db.res_option(:inet6) do
      true ->
        gethostbyname_tm(name, :inet6, false)

      false ->
        gethostbyname_tm(name, :inet, false)
    end
  end

  def gethostbyname(name, family) do
    gethostbyname_tm(name, family, false)
  end

  def gethostbyname(name, family, timeout) do
    timer = start_timer(timeout)
    res = gethostbyname_tm(name, family, timer)
    _ = stop_timer(timer)
    res
  end

  def gethostbyname_tm(name, family, timer) do
    opts0 = :inet_db.res_option(:lookup)

    opts =
      case :lists.member(
             :native,
             opts0
           ) or
             :lists.member(
               :string,
               opts0
             ) or
             :lists.member(
               :nostring,
               opts0
             ) do
        true ->
          opts0

        false ->
          [:string | opts0]
      end

    gethostbyname_tm(name, family, timer, opts)
  end

  def gethostbyaddr(address) do
    gethostbyaddr_tm(address, false)
  end

  def gethostbyaddr(address, timeout) do
    timer = start_timer(timeout)
    res = gethostbyaddr_tm(address, timer)
    _ = stop_timer(timer)
    res
  end

  def gethostbyaddr_tm(address, timer) do
    gethostbyaddr_tm(address, timer, :inet_db.res_option(:lookup))
  end

  def ip({a, b, c, d}) when (a ||| b ||| c ||| d) &&& ~~~255 === 0 do
    {:ok, {a, b, c, d}}
  end

  def ip(name) do
    case gethostbyname(name) do
      {:ok, ent} ->
        {:ok, hd(r_hostent(ent, :h_addr_list))}

      error ->
        error
    end
  end

  def getll(socket) when is_port(socket) do
    {:ok, socket}
  end

  def getfd(socket) do
    :prim_inet.getfd(socket)
  end

  def getaddr(address, family) do
    getaddr(address, family, :infinity)
  end

  def getaddr(address, family, timeout) do
    timer = start_timer(timeout)
    res = getaddr_tm(address, family, timer)
    _ = stop_timer(timer)
    res
  end

  def getaddr_tm(address, family, timer) do
    case getaddrs_tm(address, family, timer) do
      {:ok, [iP | _]} ->
        {:ok, iP}

      error ->
        error
    end
  end

  def getaddrs(address, family) do
    getaddrs(address, family, :infinity)
  end

  def getaddrs(address, family, timeout) do
    timer = start_timer(timeout)
    res = getaddrs_tm(address, family, timer)
    _ = stop_timer(timer)
    res
  end

  def getservbyport(port, proto) do
    case :inet_udp.open(0, []) do
      {:ok, u} ->
        res = :prim_inet.getservbyport(u, port, proto)
        :inet_udp.close(u)
        res

      error ->
        error
    end
  end

  def getservbyname(name, protocol) when is_atom(name) do
    case :inet_udp.open(0, []) do
      {:ok, u} ->
        res = :prim_inet.getservbyname(u, name, protocol)
        :inet_udp.close(u)
        res

      error ->
        error
    end
  end

  def ntoa(addr) do
    :inet_parse.ntoa(addr)
  end

  def parse_ipv4_address(addr) do
    :inet_parse.ipv4_address(addr)
  end

  def parse_ipv6_address(addr) do
    :inet_parse.ipv6_address(addr)
  end

  def parse_ipv4strict_address(addr) do
    :inet_parse.ipv4strict_address(addr)
  end

  def parse_ipv6strict_address(addr) do
    :inet_parse.ipv6strict_address(addr)
  end

  def parse_address(addr) do
    :inet_parse.address(addr)
  end

  def parse_strict_address(addr) do
    :inet_parse.strict_address(addr)
  end

  def ipv4_mapped_ipv6_address({d1, d2, d3, d4})
      when d1 ||| d2 ||| d3 ||| d4 < 256 do
    {0, 0, 0, 0, 0, 65535, d1 <<< 8 ||| d2, d3 <<< 8 ||| d4}
  end

  def ipv4_mapped_ipv6_address({d1, d2, d3, d4, d5, d6, d7, d8})
      when d1 ||| d2 ||| d3 ||| d4 ||| d5 ||| d6 ||| d7 ||| d8 < 65536 do
    {d7 >>> 8, d7 &&& 255, d8 >>> 8, d8 &&& 255}
  end

  def options() do
    [
      :tos,
      :tclass,
      :priority,
      :reuseaddr,
      :keepalive,
      :dontroute,
      :linger,
      :broadcast,
      :sndbuf,
      :recbuf,
      :nodelay,
      :ipv6_v6only,
      :buffer,
      :header,
      :active,
      :packet,
      :deliver,
      :mode,
      :multicast_if,
      :multicast_ttl,
      :multicast_loop,
      :exit_on_close,
      :high_watermark,
      :low_watermark,
      :high_msgq_watermark,
      :low_msgq_watermark,
      :send_timeout,
      :send_timeout_close,
      :show_econnreset
    ]
  end

  def stats() do
    [
      :recv_oct,
      :recv_cnt,
      :recv_max,
      :recv_avg,
      :recv_dvi,
      :send_oct,
      :send_cnt,
      :send_max,
      :send_avg,
      :send_pend
    ]
  end

  defp connect_options() do
    [
      :tos,
      :tclass,
      :priority,
      :reuseaddr,
      :keepalive,
      :linger,
      :sndbuf,
      :recbuf,
      :nodelay,
      :recvtos,
      :recvtclass,
      :ttl,
      :recvttl,
      :header,
      :active,
      :packet,
      :packet_size,
      :buffer,
      :mode,
      :deliver,
      :line_delimiter,
      :exit_on_close,
      :high_watermark,
      :low_watermark,
      :high_msgq_watermark,
      :low_msgq_watermark,
      :send_timeout,
      :send_timeout_close,
      :delay_send,
      :raw,
      :show_econnreset,
      :bind_to_device
    ]
  end

  def connect_options(opts, mod) do
    baseOpts =
      case :application.get_env(
             :kernel,
             :inet_default_connect_options
           ) do
        {:ok, list} when is_list(list) ->
          nList = [{:active, true} | :lists.keydelete(:active, 1, list)]
          r_connect_opts(opts: nList)

        {:ok, {:active, _Bool}} ->
          r_connect_opts(opts: [{:active, true}])

        {:ok, option} ->
          r_connect_opts(opts: [{:active, true}, option])

        _ ->
          r_connect_opts(opts: [{:active, true}])
      end

    case con_opt(opts, baseOpts, connect_options()) do
      {:ok, r} ->
        {:ok,
         r_connect_opts(r,
           opts: :lists.reverse(r_connect_opts(r, :opts)),
           ifaddr: mod.translate_ip(r_connect_opts(r, :ifaddr))
         )}

      error ->
        error
    end
  end

  defp con_opt([{:raw, a, b, c} | opts], r_connect_opts() = r, as) do
    con_opt([{:raw, {a, b, c}} | opts], r, as)
  end

  defp con_opt([opt | opts], r_connect_opts() = r, as) do
    case opt do
      {:ip, iP} ->
        con_opt(opts, r_connect_opts(r, ifaddr: iP), as)

      {:ifaddr, iP} ->
        con_opt(opts, r_connect_opts(r, ifaddr: iP), as)

      {:port, p} ->
        con_opt(opts, r_connect_opts(r, port: p), as)

      {:fd, fd} ->
        con_opt(opts, r_connect_opts(r, fd: fd), as)

      :binary ->
        con_add(:mode, :binary, r, opts, as)

      :list ->
        con_add(:mode, :list, r, opts, as)

      {:netns, nS} ->
        binNS = filename2binary(nS)

        case :prim_inet.is_sockopt_val(:netns, binNS) do
          true ->
            con_opt(opts, r_connect_opts(r, fd: [{:netns, binNS}]), as)

          false ->
            {:error, :badarg}
        end

      {:active, n}
      when is_integer(n) and n < 32768 and
             n >= -32768 ->
        nOpts = :lists.keydelete(:active, 1, r_connect_opts(r, :opts))
        con_opt(opts, r_connect_opts(r, opts: [{:active, n} | nOpts]), as)

      {:line_delimiter, c}
      when is_integer(c) and c >= 0 and
             c <= 255 ->
        con_add(:line_delimiter, c, r, opts, as)

      {name, val} when is_atom(name) ->
        con_add(name, val, r, opts, as)

      _ ->
        {:error, :badarg}
    end
  end

  defp con_opt([], r_connect_opts() = r, _) do
    {:ok, r}
  end

  defp con_add(name, val, r_connect_opts() = r, opts, allOpts) do
    case add_opt(name, val, r_connect_opts(r, :opts), allOpts) do
      {:ok, sOpts} ->
        con_opt(opts, r_connect_opts(r, opts: sOpts), allOpts)

      error ->
        error
    end
  end

  defp listen_options() do
    [
      :tos,
      :tclass,
      :priority,
      :reuseaddr,
      :keepalive,
      :linger,
      :sndbuf,
      :recbuf,
      :nodelay,
      :recvtos,
      :recvtclass,
      :ttl,
      :recvttl,
      :header,
      :active,
      :packet,
      :buffer,
      :mode,
      :deliver,
      :backlog,
      :ipv6_v6only,
      :exit_on_close,
      :high_watermark,
      :low_watermark,
      :high_msgq_watermark,
      :low_msgq_watermark,
      :send_timeout,
      :send_timeout_close,
      :delay_send,
      :packet_size,
      :raw,
      :show_econnreset,
      :bind_to_device
    ]
  end

  def listen_options(opts, mod) do
    baseOpts =
      case :application.get_env(
             :kernel,
             :inet_default_listen_options
           ) do
        {:ok, list} when is_list(list) ->
          nList = [{:active, true} | :lists.keydelete(:active, 1, list)]
          r_listen_opts(opts: nList)

        {:ok, {:active, _Bool}} ->
          r_listen_opts(opts: [{:active, true}])

        {:ok, option} ->
          r_listen_opts(opts: [{:active, true}, option])

        _ ->
          r_listen_opts(opts: [{:active, true}])
      end

    case list_opt(opts, baseOpts, listen_options()) do
      {:ok, r} ->
        {:ok,
         r_listen_opts(r,
           opts: :lists.reverse(r_listen_opts(r, :opts)),
           ifaddr: mod.translate_ip(r_listen_opts(r, :ifaddr))
         )}

      error ->
        error
    end
  end

  defp list_opt([{:raw, a, b, c} | opts], r_listen_opts() = r, as) do
    list_opt([{:raw, {a, b, c}} | opts], r, as)
  end

  defp list_opt([opt | opts], r_listen_opts() = r, as) do
    case opt do
      {:ip, iP} ->
        list_opt(opts, r_listen_opts(r, ifaddr: iP), as)

      {:ifaddr, iP} ->
        list_opt(opts, r_listen_opts(r, ifaddr: iP), as)

      {:port, p} ->
        list_opt(opts, r_listen_opts(r, port: p), as)

      {:fd, fd} ->
        list_opt(opts, r_listen_opts(r, fd: fd), as)

      {:backlog, bL} ->
        list_opt(opts, r_listen_opts(r, backlog: bL), as)

      :binary ->
        list_add(:mode, :binary, r, opts, as)

      :list ->
        list_add(:mode, :list, r, opts, as)

      {:netns, nS} ->
        binNS = filename2binary(nS)

        case :prim_inet.is_sockopt_val(:netns, binNS) do
          true ->
            list_opt(opts, r_listen_opts(r, fd: [{:netns, binNS}]), as)

          false ->
            {:error, :badarg}
        end

      {:active, n}
      when is_integer(n) and n < 32768 and
             n >= -32768 ->
        nOpts = :lists.keydelete(:active, 1, r_listen_opts(r, :opts))
        list_opt(opts, r_listen_opts(r, opts: [{:active, n} | nOpts]), as)

      {name, val} when is_atom(name) ->
        list_add(name, val, r, opts, as)

      _ ->
        {:error, :badarg}
    end
  end

  defp list_opt([], r_listen_opts() = r, _SockOpts) do
    {:ok, r}
  end

  defp list_add(name, val, r_listen_opts() = r, opts, as) do
    case add_opt(name, val, r_listen_opts(r, :opts), as) do
      {:ok, sOpts} ->
        list_opt(opts, r_listen_opts(r, opts: sOpts), as)

      error ->
        error
    end
  end

  def tcp_module(opts) do
    tcp_module_1(opts, :undefined)
  end

  def tcp_module(opts, addr) do
    address = {:undefined, addr}
    tcp_module_1(opts, address)
  end

  defp tcp_module_1(opts, address) do
    mod(opts, :tcp_module, address, %{inet: :inet_tcp, inet6: :inet6_tcp, local: :local_tcp})
  end

  def gen_tcp_module([{:inet_backend, flag} | opts]) do
    gen_tcp_module(opts, flag)
  end

  def gen_tcp_module(opts) do
    gen_tcp_module(
      opts,
      :persistent_term.get({:kernel, :inet_backend}, :inet)
    )
  end

  defp gen_tcp_module(opts, :inet) do
    {:gen_tcp, opts}
  end

  defp gen_tcp_module(opts, :socket) do
    {:gen_tcp_socket, opts}
  end

  defp udp_options() do
    [
      :tos,
      :tclass,
      :priority,
      :reuseaddr,
      :sndbuf,
      :recbuf,
      :header,
      :active,
      :buffer,
      :mode,
      :recvtos,
      :recvtclass,
      :ttl,
      :recvttl,
      :deliver,
      :ipv6_v6only,
      :broadcast,
      :dontroute,
      :multicast_if,
      :multicast_ttl,
      :multicast_loop,
      :add_membership,
      :drop_membership,
      :read_packets,
      :raw,
      :high_msgq_watermark,
      :low_msgq_watermark,
      :bind_to_device
    ]
  end

  def udp_options(opts, mod) do
    case udp_opt(opts, r_udp_opts(), udp_options()) do
      {:ok, r} ->
        {:ok,
         r_udp_opts(r,
           opts: :lists.reverse(r_udp_opts(r, :opts)),
           ifaddr: mod.translate_ip(r_udp_opts(r, :ifaddr))
         )}

      error ->
        error
    end
  end

  defp udp_opt([{:raw, a, b, c} | opts], r_udp_opts() = r, as) do
    udp_opt([{:raw, {a, b, c}} | opts], r, as)
  end

  defp udp_opt([opt | opts], r_udp_opts() = r, as) do
    case opt do
      {:ip, iP} ->
        udp_opt(opts, r_udp_opts(r, ifaddr: iP), as)

      {:ifaddr, iP} ->
        udp_opt(opts, r_udp_opts(r, ifaddr: iP), as)

      {:port, p} ->
        udp_opt(opts, r_udp_opts(r, port: p), as)

      {:fd, fd} ->
        udp_opt(opts, r_udp_opts(r, fd: fd), as)

      :binary ->
        udp_add(:mode, :binary, r, opts, as)

      :list ->
        udp_add(:mode, :list, r, opts, as)

      {:netns, nS} ->
        binNS = filename2binary(nS)

        case :prim_inet.is_sockopt_val(:netns, binNS) do
          true ->
            udp_opt(opts, r_udp_opts(r, fd: [{:netns, binNS}]), as)

          false ->
            {:error, :badarg}
        end

      {:active, n}
      when is_integer(n) and n < 32768 and
             n >= -32768 ->
        nOpts = :lists.keydelete(:active, 1, r_udp_opts(r, :opts))
        udp_opt(opts, r_udp_opts(r, opts: [{:active, n} | nOpts]), as)

      {name, val} when is_atom(name) ->
        udp_add(name, val, r, opts, as)

      _ ->
        {:error, :badarg}
    end
  end

  defp udp_opt([], r_udp_opts() = r, _SockOpts) do
    {:ok, r}
  end

  defp udp_add(name, val, r_udp_opts() = r, opts, as) do
    case add_opt(name, val, r_udp_opts(r, :opts), as) do
      {:ok, sOpts} ->
        udp_opt(opts, r_udp_opts(r, opts: sOpts), as)

      error ->
        error
    end
  end

  def udp_module(opts) do
    mod(opts, :udp_module, :undefined, %{inet: :inet_udp, inet6: :inet6_udp, local: :local_udp})
  end

  defp sctp_options() do
    [
      :mode,
      :active,
      :buffer,
      :tos,
      :tclass,
      :ttl,
      :priority,
      :dontroute,
      :reuseaddr,
      :linger,
      :recvtos,
      :recvtclass,
      :recvttl,
      :sndbuf,
      :recbuf,
      :ipv6_v6only,
      :high_msgq_watermark,
      :low_msgq_watermark,
      :bind_to_device,
      :sctp_rtoinfo,
      :sctp_associnfo,
      :sctp_initmsg,
      :sctp_autoclose,
      :sctp_nodelay,
      :sctp_disable_fragments,
      :sctp_i_want_mapped_v4_addr,
      :sctp_maxseg,
      :sctp_primary_addr,
      :sctp_set_peer_primary_addr,
      :sctp_adaptation_layer,
      :sctp_peer_addr_params,
      :sctp_default_send_param,
      :sctp_events,
      :sctp_delayed_ack_time,
      :sctp_status,
      :sctp_get_peer_addr_info
    ]
  end

  def sctp_options(opts, mod) do
    case sctp_opt(opts, mod, r_sctp_opts(), sctp_options()) do
      {:ok, r_sctp_opts(ifaddr: :undefined) = sO} ->
        {:ok,
         r_sctp_opts(sO,
           opts: :lists.reverse(r_sctp_opts(sO, :opts)),
           ifaddr: mod.translate_ip(:any)
         )}

      {:ok, sO} ->
        {:ok, r_sctp_opts(sO, opts: :lists.reverse(r_sctp_opts(sO, :opts)))}

      error ->
        error
    end
  end

  defp sctp_opt([opt | opts], mod, r_sctp_opts() = r, as) do
    case opt do
      {:ip, iP} ->
        sctp_opt_ifaddr(opts, mod, r, as, iP)

      {:ifaddr, iP} ->
        sctp_opt_ifaddr(opts, mod, r, as, iP)

      {:port, port} ->
        case mod.getserv(port) do
          {:ok, p} ->
            sctp_opt(opts, mod, r_sctp_opts(r, port: p), as)

          error ->
            error
        end

      {:type, type}
      when type === :seqpacket or
             type === :stream ->
        sctp_opt(opts, mod, r_sctp_opts(r, type: type), as)

      :binary ->
        sctp_opt(opts, mod, r, as, :mode, :binary)

      :list ->
        sctp_opt(opts, mod, r, as, :mode, :list)

      {:netns, nS} ->
        binNS = filename2binary(nS)

        case :prim_inet.is_sockopt_val(:netns, binNS) do
          true ->
            sctp_opt(opts, mod, r_sctp_opts(r, fd: [{:netns, binNS}]), as)

          false ->
            {:error, :badarg}
        end

      {:active, n}
      when is_integer(n) and n < 32768 and
             n >= -32768 ->
        nOpts = :lists.keydelete(:active, 1, r_sctp_opts(r, :opts))
        sctp_opt(opts, mod, r_sctp_opts(r, opts: [{:active, n} | nOpts]), as)

      {name, val} ->
        sctp_opt(opts, mod, r, as, name, val)

      _ ->
        {:error, :badarg}
    end
  end

  defp sctp_opt([], _Mod, r_sctp_opts(ifaddr: ifAddr) = r, _SockOpts) do
    cond do
      is_list(ifAddr) ->
        {:ok, r_sctp_opts(r, ifaddr: :lists.reverse(ifAddr))}

      true ->
        {:ok, r}
    end
  end

  defp sctp_opt(opts, mod, r_sctp_opts() = r, as, name, val) do
    case add_opt(name, val, r_sctp_opts(r, :opts), as) do
      {:ok, socketOpts} ->
        sctp_opt(opts, mod, r_sctp_opts(r, opts: socketOpts), as)

      error ->
        error
    end
  end

  defp sctp_opt_ifaddr(opts, mod, r_sctp_opts(ifaddr: ifAddr) = r, as, addr) do
    iP = mod.translate_ip(addr)

    sctp_opt(
      opts,
      mod,
      r_sctp_opts(r,
        ifaddr:
          case ifAddr do
            :undefined ->
              iP

            _ when is_list(ifAddr) ->
              [iP | ifAddr]

            _ ->
              [iP, ifAddr]
          end
      ),
      as
    )
  end

  def sctp_module(opts) do
    mod(opts, :sctp_module, :undefined, %{inet: :inet_sctp, inet6: :inet6_sctp})
  end

  defp add_opt(name, val, opts, as) do
    case :lists.member(name, as) do
      true ->
        case :prim_inet.is_sockopt_val(name, val) do
          true when name === :raw ->
            {:ok, [{name, val} | opts]}

          true ->
            opts1 = :lists.keydelete(name, 1, opts)
            {:ok, [{name, val} | opts1]}

          false ->
            {:error, :badarg}
        end

      false ->
        {:error, :badarg}
    end
  end

  defp filename2binary(list) when is_list(list) do
    outEncoding = :file.native_name_encoding()

    try do
      :unicode.characters_to_binary(list, :unicode, outEncoding)
    catch
      :error, :badarg ->
        list
    else
      bin when is_binary(bin) ->
        bin

      _ ->
        list
    end
  end

  defp filename2binary(bin) do
    bin
  end

  defp binary2filename(bin) do
    inEncoding = :file.native_name_encoding()

    case :unicode.characters_to_list(bin, inEncoding) do
      filename when is_list(filename) ->
        filename

      _ ->
        bin
    end
  end

  def translate_ip(:any, :inet) do
    {0, 0, 0, 0}
  end

  def translate_ip(:loopback, :inet) do
    {127, 0, 0, 1}
  end

  def translate_ip(:any, :inet6) do
    {0, 0, 0, 0, 0, 0, 0, 0}
  end

  def translate_ip(:loopback, :inet6) do
    {0, 0, 0, 0, 0, 0, 0, 1}
  end

  def translate_ip(iP, _) do
    iP
  end

  defp mod(opts, tag, address, map) do
    mod(opts, tag, address, map, :undefined, [])
  end

  defp mod([{tag, m} | opts], tag, address, map, mod, acc) do
    mod(opts, tag, address, map, mod, acc, m)
  end

  defp mod([{t, _} = opt | opts], tag, _Address, map, mod, acc)
       when t === :ip or t === :ifaddr do
    mod(opts, tag, opt, map, mod, [opt | acc])
  end

  defp mod([family | opts], tag, address, map, mod, acc)
       when is_atom(family) do
    case map do
      %{^family => m} ->
        mod(opts, tag, address, map, mod, acc, m)

      %{} ->
        mod(opts, tag, address, map, mod, [family | acc])
    end
  end

  defp mod([opt | opts], tag, address, map, mod, acc) do
    mod(opts, tag, address, map, mod, [opt | acc])
  end

  defp mod([], tag, address, map, :undefined, acc) do
    {case address do
       {_, {:local, _}} ->
         case map do
           %{local: mod} ->
             mod

           %{} ->
             apply(:inet_db, tag, [])
         end

       {_, iP} when tuple_size(iP) === 8 ->
         %{inet: iPv4Mod} = map

         case apply(:inet_db, tag, []) do
           ^iPv4Mod ->
             %{inet6: iPv6Mod} = map
             iPv6Mod

           mod ->
             mod
         end

       _ ->
         apply(:inet_db, tag, [])
     end, :lists.reverse(acc)}
  end

  defp mod([], _Tag, _Address, _Map, mod, acc) do
    {mod, :lists.reverse(acc)}
  end

  defp mod(opts, tag, address, map, :undefined, acc, m) do
    mod(opts, tag, address, map, m, acc)
  end

  defp mod(opts, tag, address, map, mod, acc, _M) do
    mod(opts, tag, address, map, mod, acc)
  end

  def getaddrs_tm({a, b, c, d} = iP, fam, _) do
    cond do
      (a ||| b ||| c ||| d) &&& ~~~255 === 0 ->
        cond do
          fam === :inet ->
            {:ok, [iP]}

          true ->
            {:error, :eafnosupport}
        end

      true ->
        {:error, :einval}
    end
  end

  def getaddrs_tm({a, b, c, d, e, f, g, h} = iP, fam, _) do
    cond do
      (a ||| b ||| c ||| d ||| e ||| f ||| g ||| h) &&& ~~~65535 === 0 ->
        cond do
          fam === :inet6 ->
            {:ok, [iP]}

          true ->
            {:error, :eafnosupport}
        end

      true ->
        {:error, :einval}
    end
  end

  def getaddrs_tm(address, family, timer) when is_atom(address) do
    getaddrs_tm(:erlang.atom_to_list(address), family, timer)
  end

  def getaddrs_tm(address, family, timer) do
    case :inet_parse.visible_string(address) do
      false ->
        {:error, :einval}

      true ->
        case gethostbyname_tm(address, family, timer) do
          {:ok, ent} ->
            {:ok, r_hostent(ent, :h_addr_list)}

          error ->
            error
        end
    end
  end

  defp gethostbyname_tm(name, type, timer, [:string | _] = opts) do
    result = gethostbyname_string(name, type)
    gethostbyname_tm(name, type, timer, opts, result)
  end

  defp gethostbyname_tm(name, type, timer, [:dns | _] = opts) do
    result = :inet_res.gethostbyname_tm(name, type, timer)
    gethostbyname_tm(name, type, timer, opts, result)
  end

  defp gethostbyname_tm(name, type, timer, [:file | _] = opts) do
    result = :inet_hosts.gethostbyname(name, type)
    gethostbyname_tm(name, type, timer, opts, result)
  end

  defp gethostbyname_tm(name, type, timer, [:yp | _] = opts) do
    gethostbyname_tm_native(name, type, timer, opts)
  end

  defp gethostbyname_tm(name, type, timer, [:nis | _] = opts) do
    gethostbyname_tm_native(name, type, timer, opts)
  end

  defp gethostbyname_tm(name, type, timer, [:nisplus | _] = opts) do
    gethostbyname_tm_native(name, type, timer, opts)
  end

  defp gethostbyname_tm(name, type, timer, [:wins | _] = opts) do
    gethostbyname_tm_native(name, type, timer, opts)
  end

  defp gethostbyname_tm(name, type, timer, [:native | _] = opts) do
    gethostbyname_tm_native(name, type, timer, opts)
  end

  defp gethostbyname_tm(name, type, timer, [_ | opts]) do
    gethostbyname_tm(name, type, timer, opts)
  end

  defp gethostbyname_tm(name, type, timer, []) do
    result = gethostbyname_self(name, type)
    gethostbyname_tm(name, type, timer, [], result)
  end

  defp gethostbyname_tm(name, type, timer, opts, result) do
    case result do
      {:ok, _} ->
        result

      {:error, :formerr} ->
        {:error, :einval}

      {:error, _} when opts === [] ->
        {:error, :nxdomain}

      {:error, _} ->
        gethostbyname_tm(name, type, timer, tl(opts))
    end
  end

  defp gethostbyname_tm_native(name, type, timer, opts) do
    result = :inet_gethost_native.gethostbyname(name, type)
    gethostbyname_tm(name, type, timer, opts, result)
  end

  def gethostbyname_self(name, type) when is_atom(name) do
    gethostbyname_self(:erlang.atom_to_list(name), type)
  end

  def gethostbyname_self(name, type)
      when (is_list(name) and
              type === :inet) or
             (is_list(name) and type === :inet6) do
    n = :inet_db.tolower(name)
    self = :inet_db.gethostname()

    case :inet_db.tolower(self) do
      ^n ->
        {:ok, make_hostent(self, [translate_ip(:loopback, type)], [], type)}

      _ ->
        case :inet_db.res_option(:domain) do
          '' ->
            {:error, :nxdomain}

          domain ->
            fQDN = :lists.append([self, '.', domain])

            case :inet_db.tolower(fQDN) do
              ^n ->
                {:ok, make_hostent(fQDN, [translate_ip(:loopback, type)], [], type)}

              _ ->
                {:error, :nxdomain}
            end
        end
    end
  end

  def gethostbyname_self(_, _) do
    {:error, :formerr}
  end

  def gethostbyname_string(name, type) when is_atom(name) do
    gethostbyname_string(:erlang.atom_to_list(name), type)
  end

  def gethostbyname_string(name, type)
      when (is_list(name) and
              type === :inet) or
             (is_list(name) and type === :inet6) do
    case (case type do
            :inet ->
              :inet_parse.ipv4_address(name)

            :inet6 ->
              :inet_parse.ipv6strict_address(name)
          end) do
      {:ok, iP} ->
        {:ok, make_hostent(name, [iP], [], type)}

      {:error, :einval} ->
        {:error, :nxdomain}
    end
  end

  def gethostbyname_string(_, _) do
    {:error, :formerr}
  end

  defp make_hostent(name, addrs, aliases, type) do
    r_hostent(
      h_name: name,
      h_aliases: aliases,
      h_addrtype: type,
      h_length:
        case type do
          :inet ->
            4

          :inet6 ->
            16
        end,
      h_addr_list: addrs
    )
  end

  defp gethostbyaddr_tm(addr, timer, [:dns | opts]) do
    res = :inet_res.gethostbyaddr_tm(addr, timer)

    case res do
      {:ok, _} ->
        res

      {:error, :timeout} ->
        res

      {:error, :formerr} ->
        {:error, :einval}

      {:error, _} ->
        gethostbyaddr_tm(addr, timer, opts)
    end
  end

  defp gethostbyaddr_tm(addr, timer, [:file | opts]) do
    case :inet_hosts.gethostbyaddr(addr) do
      {:error, :formerr} ->
        {:error, :einval}

      {:error, _} ->
        gethostbyaddr_tm(addr, timer, opts)

      result ->
        result
    end
  end

  defp gethostbyaddr_tm(addr, timer, [:yp | opts]) do
    gethostbyaddr_tm_native(addr, timer, opts)
  end

  defp gethostbyaddr_tm(addr, timer, [:nis | opts]) do
    gethostbyaddr_tm_native(addr, timer, opts)
  end

  defp gethostbyaddr_tm(addr, timer, [:nisplus | opts]) do
    gethostbyaddr_tm_native(addr, timer, opts)
  end

  defp gethostbyaddr_tm(addr, timer, [:wins | opts]) do
    gethostbyaddr_tm_native(addr, timer, opts)
  end

  defp gethostbyaddr_tm(addr, timer, [:native | opts]) do
    gethostbyaddr_tm_native(addr, timer, opts)
  end

  defp gethostbyaddr_tm(addr, timer, [_ | opts]) do
    gethostbyaddr_tm(addr, timer, opts)
  end

  defp gethostbyaddr_tm({127, 0, 0, 1} = iP, _Timer, []) do
    gethostbyaddr_self(iP, :inet)
  end

  defp gethostbyaddr_tm({0, 0, 0, 0, 0, 0, 0, 1} = iP, _Timer, []) do
    gethostbyaddr_self(iP, :inet6)
  end

  defp gethostbyaddr_tm(_Addr, _Timer, []) do
    {:error, :nxdomain}
  end

  defp gethostbyaddr_self(iP, type) do
    name = :inet_db.gethostname()

    case :inet_db.res_option(:domain) do
      '' ->
        {:ok, make_hostent(name, [iP], [], type)}

      domain ->
        {:ok, make_hostent(name ++ '.' ++ domain, [iP], [name], type)}
    end
  end

  defp gethostbyaddr_tm_native(addr, timer, opts) do
    case :inet_gethost_native.gethostbyaddr(addr) do
      {:error, :formerr} ->
        {:error, :einval}

      {:error, _} ->
        gethostbyaddr_tm(addr, timer, opts)

      result ->
        result
    end
  end

  def open(fdO, addr, port, opts, protocol, family, type, module)
      when (is_integer(fdO) and fdO < 0) or is_list(fdO) do
    openOpts =
      cond do
        is_list(fdO) ->
          fdO

        true ->
          []
      end

    case :prim_inet.open(protocol, family, type, openOpts) do
      {:ok, s} ->
        case :prim_inet.setopts(s, opts) do
          :ok when addr === :undefined ->
            :inet_db.register_socket(s, module)
            {:ok, s}

          :ok ->
            case bind(s, addr, port) do
              {:ok, _} ->
                :inet_db.register_socket(s, module)
                {:ok, s}

              error ->
                :prim_inet.close(s)
                error
            end

          error ->
            :prim_inet.close(s)
            error
        end

      error ->
        error
    end
  end

  def open(fd, addr, port, opts, protocol, family, type, module)
      when is_integer(fd) do
    fdopen(fd, addr, port, opts, protocol, family, type, module)
  end

  defp bind(s, addr, port) when is_list(addr) do
    bindx(s, addr, port)
  end

  defp bind(s, addr, port) do
    :prim_inet.bind(s, addr, port)
  end

  defp bindx(s, [addr], port0) do
    {iP, port} = set_bindx_port(addr, port0)
    :prim_inet.bind(s, iP, port)
  end

  defp bindx(s, addrs, port0) do
    [{iP, port} | rest] =
      for addr <- addrs do
        set_bindx_port(addr, port0)
      end

    case :prim_inet.bind(s, iP, port) do
      {:ok, assignedPort} when port === 0 ->
        rest2 =
          for addr <- rest do
            change_bindx_0_port(addr, assignedPort)
          end

        :prim_inet.bind(s, :add, rest2)

      {:ok, _} ->
        :prim_inet.bind(s, :add, rest)

      error ->
        error
    end
  end

  defp set_bindx_port({_IP, _Port} = addr, _OtherPort) do
    addr
  end

  defp set_bindx_port(iP, port) do
    {iP, port}
  end

  defp change_bindx_0_port({iP, 0}, assignedPort) do
    {iP, assignedPort}
  end

  defp change_bindx_0_port({_IP, _Port} = addr, _AssignedPort) do
    addr
  end

  def fdopen(fd, opts, protocol, family, type, module) do
    fdopen(fd, :any, 0, opts, protocol, family, type, module)
  end

  defp fdopen(fd, addr, port, opts, protocol, family, type, module) do
    bound =
      case addr do
        {0, 0, 0, 0} when port === 0 ->
          true

        {0, 0, 0, 0, 0, 0, 0, 0} when port === 0 ->
          true

        :any when port === 0 ->
          true

        _ ->
          false
      end

    case :prim_inet.fdopen(protocol, family, type, fd, bound) do
      {:ok, s} ->
        case :prim_inet.setopts(s, opts) do
          :ok when addr === :undefined or bound ->
            :inet_db.register_socket(s, module)
            {:ok, s}

          :ok ->
            case bind(s, addr, port) do
              {:ok, _} ->
                :inet_db.register_socket(s, module)
                {:ok, s}

              error ->
                :prim_inet.close(s)
                error
            end

          error ->
            :prim_inet.close(s)
            error
        end

      error ->
        error
    end
  end

  def i() do
    i(:tcp)
    i(:udp)
    i(:sctp)
  end

  def i(proto) do
    i(
      proto,
      [:port, :module, :recv, :sent, :owner, :local_address, :foreign_address, :state, :type]
    )
  end

  def i(:tcp, fs) do
    ii(tcp_sockets(), fs, :tcp)
  end

  def i(:udp, fs) do
    ii(udp_sockets(), fs, :udp)
  end

  def i(:sctp, fs) do
    ii(sctp_sockets(), fs, :sctp)
  end

  defp ii(ss, fs, proto) do
    lLs =
      case info_lines(ss, fs, proto) do
        [] ->
          []

        infoLines ->
          [h_line(fs) | infoLines]
      end

    maxs =
      foldl(
        fn line, max0 ->
          smax(max0, line)
        end,
        duplicate(length(fs), 0),
        lLs
      )

    fmt =
      append(
        for n <- maxs do
          '~-' ++ :erlang.integer_to_list(n) ++ 's '
        end
      ) ++ '\n'

    :lists.foreach(
      fn line ->
        :io.format(fmt, line)
      end,
      lLs
    )
  end

  defp smax([max | ms], [str | strs]) do
    n = length(str)

    [
      cond do
        n > max ->
          n

        true ->
          max
      end
      | smax(ms, strs)
    ]
  end

  defp smax([], []) do
    []
  end

  defp info_lines(ss, fs, proto) do
    for s <- ss do
      i_line(s, fs, proto)
    end
  end

  defp i_line(s, fs, proto) do
    for f <- fs do
      info(s, f, proto)
    end
  end

  defp h_line(fs) do
    for f <- fs do
      h_field(:erlang.atom_to_list(f))
    end
  end

  defp h_field([c | cs]) do
    [upper(c) | hh_field(cs)]
  end

  defp hh_field([?_, c | cs]) do
    [?\s, upper(c) | hh_field(cs)]
  end

  defp hh_field([c | cs]) do
    [c | hh_field(cs)]
  end

  defp hh_field([]) do
    []
  end

  defp upper(c) when c >= ?a and c <= ?z do
    c - ?a + ?A
  end

  defp upper(c) do
    c
  end

  defp info(s, f, proto) do
    case f do
      :owner ->
        case :erlang.port_info(s, :connected) do
          {:connected, owner} ->
            :erlang.pid_to_list(owner)

          _ ->
            ' '
        end

      :port ->
        case :erlang.port_info(s, :id) do
          {:id, id} ->
            :erlang.integer_to_list(id)

          :undefined ->
            ' '
        end

      :sent ->
        case :prim_inet.getstat(s, [:send_oct]) do
          {:ok, [{:send_oct, n}]} ->
            :erlang.integer_to_list(n)

          _ ->
            ' '
        end

      :recv ->
        case :prim_inet.getstat(s, [:recv_oct]) do
          {:ok, [{:recv_oct, n}]} ->
            :erlang.integer_to_list(n)

          _ ->
            ' '
        end

      :local_address ->
        fmt_addr(:prim_inet.sockname(s), proto)

      :foreign_address ->
        fmt_addr(:prim_inet.peername(s), proto)

      :state ->
        case :prim_inet.getstatus(s) do
          {:ok, status} ->
            fmt_status(status)

          _ ->
            ' '
        end

      :packet ->
        case :prim_inet.getopt(s, :packet) do
          {:ok, type} when is_atom(type) ->
            :erlang.atom_to_list(type)

          {:ok, type} when is_integer(type) ->
            :erlang.integer_to_list(type)

          _ ->
            ' '
        end

      :type ->
        case :prim_inet.gettype(s) do
          {:ok, {_, :stream}} ->
            'STREAM'

          {:ok, {_, :dgram}} ->
            'DGRAM'

          {:ok, {_, :seqpacket}} ->
            'SEQPACKET'

          _ ->
            ' '
        end

      :fd ->
        case :prim_inet.getfd(s) do
          {:ok, fd} ->
            :erlang.integer_to_list(fd)

          _ ->
            ' '
        end

      :module ->
        case :inet_db.lookup_socket(s) do
          {:ok, mod} ->
            :erlang.atom_to_list(mod)

          _ ->
            'prim_inet'
        end
    end
  end

  defp fmt_status(flags) do
    case :lists.sort(flags) do
      [:accepting | _] ->
        'ACCEPTING'

      [:bound, :busy, :connected | _] ->
        'CONNECTED(BB)'

      [:bound, :connected | _] ->
        'CONNECTED(B)'

      [:bound, :listen, :listening | _] ->
        'LISTENING'

      [:bound, :listen | _] ->
        'LISTEN'

      [:bound, :connecting | _] ->
        'CONNECTING'

      [:bound, :open] ->
        'BOUND'

      [:connected, :open] ->
        'CONNECTED(O)'

      [:open] ->
        'IDLE'

      [] ->
        'CLOSED'

      sorted ->
        fmt_status2(sorted)
    end
  end

  defp fmt_status2([h]) do
    fmt_status3(h)
  end

  defp fmt_status2([h | t]) do
    fmt_status3(h) ++ ':' ++ fmt_status2(t)
  end

  defp fmt_status3(:accepting) do
    'A'
  end

  defp fmt_status3(:bound) do
    'BD'
  end

  defp fmt_status3(:busy) do
    'BY'
  end

  defp fmt_status3(:connected) do
    'CD'
  end

  defp fmt_status3(:connecting) do
    'CG'
  end

  defp fmt_status3(:listen) do
    'LN'
  end

  defp fmt_status3(:listening) do
    'LG'
  end

  defp fmt_status3(:open) do
    'O'
  end

  defp fmt_status3(x) when is_atom(x) do
    :string.uppercase(:erlang.atom_to_list(x))
  end

  defp fmt_addr({:error, :enotconn}, _) do
    '*:*'
  end

  defp fmt_addr({:error, _}, _) do
    ' '
  end

  defp fmt_addr({:ok, addr}, proto) do
    case addr do
      {{0, 0, 0, 0}, port} ->
        '*:' ++ fmt_port(port, proto)

      {{0, 0, 0, 0, 0, 0, 0, 0}, port} ->
        '*:' ++ fmt_port(port, proto)

      {{127, 0, 0, 1}, port} ->
        'localhost:' ++ fmt_port(port, proto)

      {{0, 0, 0, 0, 0, 0, 0, 1}, port} ->
        'localhost:' ++ fmt_port(port, proto)

      {:local, path} ->
        'local:' ++ :erlang.binary_to_list(path)

      {iP, port} ->
        :inet_parse.ntoa(iP) ++ ':' ++ fmt_port(port, proto)
    end
  end

  defp fmt_port(n, proto) do
    case :inet.getservbyport(n, proto) do
      {:ok, name} ->
        name

      _ ->
        :erlang.integer_to_list(n)
    end
  end

  defp tcp_sockets() do
    port_list('tcp_inet')
  end

  defp udp_sockets() do
    port_list('udp_inet')
  end

  defp sctp_sockets() do
    port_list('sctp_inet')
  end

  defp port_list(name) do
    filter(
      fn port ->
        case :erlang.port_info(port, :name) do
          {:name, ^name} ->
            true

          _ ->
            false
        end
      end,
      :erlang.ports()
    )
  end

  def format_error(:exbadport) do
    'invalid port state'
  end

  def format_error(:exbadseq) do
    'bad command sequence'
  end

  def format_error(:system_limit) do
    'a system limit was hit, probably not enough ports'
  end

  def format_error(tag) do
    :erl_posix_msg.message(tag)
  end

  def tcp_close(s) when is_port(s) do
    :prim_inet.close(s)

    receive do
      {:tcp_closed, ^s} ->
        :ok
    after
      0 ->
        :ok
    end
  end

  def udp_close(s) when is_port(s) do
    receive do
      {:udp_closed, ^s} ->
        :ok
    after
      0 ->
        :prim_inet.close(s)

        receive do
          {:udp_closed, ^s} ->
            :ok
        after
          0 ->
            :ok
        end
    end
  end

  def tcp_controlling_process(s, newOwner)
      when is_port(s) and
             is_pid(newOwner) do
    case :erlang.port_info(s, :connected) do
      {:connected, ^newOwner} ->
        :ok

      {:connected, pid} when pid !== self() ->
        {:error, :not_owner}

      :undefined ->
        {:error, :einval}

      _ ->
        case :prim_inet.getopt(s, :active) do
          {:ok, a0} ->
            setOptRes =
              case a0 do
                false ->
                  :ok

                _ ->
                  :prim_inet.setopt(s, :active, false)
              end

            case {tcp_sync_input(s, newOwner, false), setOptRes} do
              {true, _} ->
                :ok

              {false, :ok} ->
                try do
                  :erlang.port_connect(s, newOwner)
                catch
                  :error, reason ->
                    {:error, reason}
                else
                  true ->
                    :erlang.unlink(s)

                    case a0 do
                      false ->
                        :ok

                      _ ->
                        :prim_inet.setopt(s, :active, a0)
                    end
                end

              {false, error} ->
                error
            end

          error ->
            error
        end
    end
  end

  defp tcp_sync_input(s, owner, flag) do
    receive do
      {:tcp, ^s, data} ->
        send(owner, {:tcp, s, data})
        tcp_sync_input(s, owner, flag)

      {:tcp_closed, ^s} ->
        send(owner, {:tcp_closed, s})
        tcp_sync_input(s, owner, true)

      {^s, {:data, data}} ->
        send(owner, {s, {:data, data}})
        tcp_sync_input(s, owner, flag)

      {:inet_async, ^s, ref, status} ->
        send(owner, {:inet_async, s, ref, status})
        tcp_sync_input(s, owner, flag)

      {:inet_reply, ^s, status} ->
        send(owner, {:inet_reply, s, status})
        tcp_sync_input(s, owner, flag)
    after
      0 ->
        flag
    end
  end

  def udp_controlling_process(s, newOwner)
      when is_port(s) and
             is_pid(newOwner) do
    case :erlang.port_info(s, :connected) do
      {:connected, ^newOwner} ->
        :ok

      {:connected, pid} when pid !== self() ->
        {:error, :not_owner}

      _ ->
        {:ok, a0} = :prim_inet.getopt(s, :active)
        :ok = :prim_inet.setopt(s, :active, false)
        udp_sync_input(s, newOwner)

        try do
          :erlang.port_connect(s, newOwner)
        catch
          :error, reason ->
            {:error, reason}
        else
          true ->
            :erlang.unlink(s)
            :ok = :prim_inet.setopt(s, :active, a0)
        end
    end
  end

  defp udp_sync_input(s, owner) do
    receive do
      {:sctp, ^s, _, _, _} = msg ->
        udp_sync_input(s, owner, msg)

      {:udp, ^s, _, _, _} = msg ->
        udp_sync_input(s, owner, msg)

      {:udp_closed, ^s} = msg ->
        udp_sync_input(s, owner, msg)

      {^s, {:data, _}} = msg ->
        udp_sync_input(s, owner, msg)

      {:inet_async, ^s, _, _} = msg ->
        udp_sync_input(s, owner, msg)

      {:inet_reply, ^s, _} = msg ->
        udp_sync_input(s, owner, msg)
    after
      0 ->
        :ok
    end
  end

  defp udp_sync_input(s, owner, msg) do
    send(owner, msg)
    udp_sync_input(s, owner)
  end

  def start_timer(:infinity) do
    false
  end

  def start_timer(timeout) do
    :erlang.start_timer(timeout, self(), :inet)
  end

  def timeout(false) do
    :infinity
  end

  def timeout(timer) do
    case :erlang.read_timer(timer) do
      false ->
        0

      time ->
        time
    end
  end

  def timeout(time, false) do
    time
  end

  def timeout(time, timer) do
    timerTime = timeout(timer)

    cond do
      timerTime < time ->
        timerTime

      true ->
        time
    end
  end

  def stop_timer(false) do
    false
  end

  def stop_timer(timer) do
    case :erlang.cancel_timer(timer) do
      false ->
        receive do
          {:timeout, ^timer, _} ->
            false
        after
          0 ->
            false
        end

      t ->
        t
    end
  end

  def lock_socket(s, val) do
    case :erlang.port_info(s, :connected) do
      {:connected, pid} when pid !== self() ->
        {:error, :not_owner}

      :undefined ->
        {:error, :einval}

      _ ->
        :prim_inet.ignorefd(s, val)
    end
  end
end
