defmodule :m_prim_inet do
  use Bitwise
  import Kernel, except: [send: 2]

  def fdopen(protocol, family, type, fd)
      when is_integer(fd) do
    fdopen(protocol, family, type, fd, true)
  end

  defp open(protocol, family, type, opts, req, data) do
    drv = protocol2drv(protocol)
    aF = enc_family(family)
    t = enc_type(type)

    try do
      :erlang.open_port({:spawn_driver, drv}, [:binary])
    catch
      :error, :badarg ->
        {:error, :eprotonosupport}

      :error, :system_limit ->
        {:error, :system_limit}
    else
      s ->
        case setopts(s, opts) do
          :ok ->
            case ctl_cmd(s, req, [aF, t, data]) do
              {:ok, _} ->
                {:ok, s}

              {:error, _} = e1 ->
                close(s)
                e1
            end

          {:error, _} = e2 ->
            close(s)
            e2
        end
    end
  end

  defp protocol2drv(:tcp) do
    'tcp_inet'
  end

  defp protocol2drv(:udp) do
    'udp_inet'
  end

  defp protocol2drv(:sctp) do
    'sctp_inet'
  end

  defp drv2protocol('tcp_inet') do
    :tcp
  end

  defp drv2protocol('udp_inet') do
    :udp
  end

  defp drv2protocol('sctp_inet') do
    :sctp
  end

  defp drv2protocol(_) do
    :undefined
  end

  def shutdown(s, :read) when is_port(s) do
    shutdown_1(s, 0)
  end

  def shutdown(s, :write) when is_port(s) do
    shutdown_1(s, 1)
  end

  def shutdown(s, :read_write) when is_port(s) do
    shutdown_1(s, 2)
  end

  def close(s) when is_port(s) do
    :ok

    case getopt(s, :linger) do
      {:ok, {true, 0}} ->
        close_port(s)

      {:ok, {true, t}} ->
        case subscribe(s, [:subs_empty_out_q]) do
          {:ok, [{:subs_empty_out_q, 0}]} ->
            close_port(s)

          {:ok, [{:subs_empty_out_q, n}]} when n > 0 ->
            tref = :erlang.start_timer(t * 1000, self(), :close_port)
            close_pend_loop(s, tref, n)

          _ ->
            tref = :erlang.start_timer(t * 1000, self(), :close_port)
            close_pend_loop(s, tref, :undefined)
        end

      _ ->
        case subscribe(s, [:subs_empty_out_q]) do
          {:ok, [{:subs_empty_out_q, n}]} when n > 0 ->
            defaultT = 180_000
            tref = :erlang.start_timer(defaultT, self(), :close_port)
            close_pend_loop(s, tref, n)

          _ ->
            close_port(s)
        end
    end
  end

  defp close_port(s, tref) do
    :ok

    case :erlang.cancel_timer(tref) do
      false ->
        receive do
          {:timeout, ^tref, _} ->
            :ok
        end

      _N ->
        :ok
    end

    close_port(s)
  end

  defp close_port(s) do
    :ok

    _Closed =
      try do
        :erlang.port_close(s)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    receive do
      {:EXIT, ^s, _} ->
        :ok
    after
      0 ->
        :ok
    end

    :ok
    :ok
  end

  defp bindx_check_addrs([addr | addrs]) do
    type_value(:set, :addr, addr) and bindx_check_addrs(addrs)
  end

  defp bindx_check_addrs([]) do
    true
  end

  def connect(s, iP, port) do
    connect(s, iP, port, :infinity)
  end

  def connect(s, addr, _, time)
      when is_port(s) and
             tuple_size(addr) === 2 do
    case type_value(:set, :addr, addr) do
      true when time === :infinity ->
        connect0(s, addr, -1)

      true when is_integer(time) ->
        connect0(s, addr, time)

      false ->
        {:error, :einval}
    end
  end

  def connect(s, iP, port, time) do
    connect(s, {iP, port}, 0, time)
  end

  defp connect0(s, addr, time) do
    case async_connect0(s, addr, time) do
      {:ok, ^s, ref} ->
        receive do
          {:inet_async, ^s, ^ref, status} ->
            status
        end

      error ->
        error
    end
  end

  def async_connect(s, addr, _, time)
      when is_port(s) and
             tuple_size(addr) === 2 do
    case type_value(:set, :addr, addr) do
      true when time === :infinity ->
        async_connect0(s, addr, -1)

      true when is_integer(time) ->
        async_connect0(s, addr, time)

      false ->
        {:error, :einval}
    end
  end

  def async_connect(s, iP, port, time) do
    async_connect(s, {iP, port}, 0, time)
  end

  def accept(l) do
    accept0(l, -1, [])
  end

  def accept(l, :infinity) do
    accept0(l, -1, [])
  end

  def accept(l, familyOpts) when is_list(familyOpts) do
    accept0(l, -1, familyOpts)
  end

  def accept(l, time) do
    accept0(l, time, [])
  end

  def accept(l, :infinity, familyOpts) do
    accept0(l, -1, familyOpts)
  end

  def accept(l, time, familyOpts) do
    accept0(l, time, familyOpts)
  end

  defp accept0(l, time, familyOpts)
       when is_port(l) and
              is_integer(time) and
              is_list(familyOpts) do
    case async_accept(l, time) do
      {:ok, ref} ->
        receive do
          {:inet_async, ^l, ^ref, {:ok, s}} ->
            accept_opts(l, s, familyOpts)

          {:inet_async, ^l, ^ref, error} ->
            error
        end

      error ->
        error
    end
  end

  defp accept_opts(l, s, familyOpts) do
    case getopts(
           l,
           [:active, :nodelay, :keepalive, :delay_send, :priority, :linger] ++ familyOpts
         ) do
      {:ok, opts} ->
        case setopts(s, opts) do
          :ok ->
            {:ok, s}

          error1 ->
            close(s)
            error1
        end

      error2 ->
        close(s)
        error2
    end
  end

  def send(s, data, optList)
      when is_port(s) and
             is_list(optList) do
    :ok

    try do
      :erlang.port_command(s, data, optList)
    catch
      :error, _Error ->
        :ok
        {:error, :einval}
    else
      false ->
        :ok
        {:error, :busy}

      true ->
        send_recv_reply(s, :undefined)
    end
  end

  def send(s, data) do
    send(s, data, [])
  end

  def sendto(s, {_, _} = address, ancOpts, data)
      when is_port(s) and is_list(ancOpts) do
    case encode_opt_val(ancOpts) do
      {:ok, ancData} ->
        ancDataLen = :erlang.iolist_size(ancData)

        case type_value(:set, :addr, address) and type_value(:set, :uint32, ancDataLen) do
          true ->
            :ok

            portCommandData = [
              enc_value(:set, :addr, address),
              enc_value(:set, :uint32, ancDataLen),
              ancData,
              data
            ]

            try do
              :erlang.port_command(s, portCommandData)
            catch
              _, _ ->
                :ok
                {:error, :einval}
            else
              true ->
                receive do
                  {:inet_reply, ^s, reply} ->
                    :ok
                    reply
                end
            end

          false ->
            :ok
            {:error, :einval}
        end

      {:error, _} ->
        :ok
        {:error, :einval}
    end
  end

  def sendto(s, iP, port, data)
      when is_port(s) and
             is_integer(port) do
    sendto(s, {iP, port}, [], data)
  end

  def sendmsg(s, r_sctp_sndrcvinfo() = sRI, data) when is_port(s) do
    type = type_opt(:set, :sctp_default_send_param)

    try do
      type_value(:set, type, sRI)
    catch
      reason ->
        {:error, reason}
    else
      true ->
        send(s, [enc_value(:set, type, sRI) | data])

      false ->
        {:error, :einval}
    end
  end

  def sendfile(s, fileHandle, offset, length)
      when not is_port(s) or
             not is_binary(fileHandle) or
             not is_integer(offset) or
             not is_integer(length) do
    {:error, :badarg}
  end

  def sendfile(s, fileHandle, offset, length) do
    case :erlang.port_info(s, :connected) do
      {:connected, pid} when pid === self() ->
        uncork = sendfile_maybe_cork(s)
        result = sendfile_1(s, fileHandle, offset, length)
        sendfile_maybe_uncork(s, uncork)
        result

      {:connected, pid} when pid !== self() ->
        {:error, :not_owner}

      _Other ->
        {:error, :einval}
    end
  end

  defp sendfile_maybe_cork(s) do
    case getprotocol(s) do
      :tcp ->
        case getopts(s, [:nopush]) do
          {:ok, [{:nopush, false}]} ->
            _ = setopts(s, [{:nopush, true}])
            true

          _ ->
            false
        end

      _ ->
        false
    end
  end

  defp sendfile_maybe_uncork(s, true) do
    _ = setopts(s, [{:nopush, false}])
    :ok
  end

  defp sendfile_maybe_uncork(_, false) do
    :ok
  end

  def recv(s, length) do
    recv0(s, length, -1)
  end

  def recv(s, length, :infinity) do
    recv0(s, length, -1)
  end

  def recv(s, length, time) when is_integer(time) do
    recv0(s, length, time)
  end

  defp recv0(s, length, time)
       when is_port(s) and
              is_integer(length) and length >= 0 do
    case async_recv(s, length, time) do
      {:ok, ref} ->
        receive do
          {:inet_async, ^s, ^ref, status} ->
            status

          {:EXIT, ^s, _Reason} ->
            {:error, :closed}
        end

      error ->
        error
    end
  end

  def recvfrom(s, length) do
    recvfrom(s, length, :infinity)
  end

  def recvfrom(s, length, :infinity) when is_port(s) do
    recvfrom0(s, length, -1)
  end

  def recvfrom(s, length, time) when is_port(s) do
    cond do
      is_integer(time) and 0 <= time and
          time < 4_294_967_295 ->
        recvfrom0(s, length, time)

      true ->
        {:error, :einval}
    end
  end

  def peernames(s) when is_port(s) do
    peernames(s, :undefined)
  end

  def socknames(s) when is_port(s) do
    socknames(s, :undefined)
  end

  def setopt(s, opt, value) when is_port(s) do
    setopts(s, [{opt, value}])
  end

  def getopt(s, opt) when is_port(s) and is_atom(opt) do
    case getopts(s, [opt]) do
      {:ok, [{_, value}]} ->
        {:ok, value}

      error ->
        error
    end
  end

  def chgopt(s, opt, value) when is_port(s) do
    chgopts(s, [{opt, value}])
  end

  def chgopts(s, opts) when is_port(s) and is_list(opts) do
    case getopts(s, need_template(opts)) do
      {:ok, templates} ->
        try do
          merge_options(opts, templates)
        catch
          reason ->
            {:error, reason}
        else
          newOpts ->
            setopts(s, newOpts)
        end

      error ->
        error
    end
  end

  defp comp_ifaddrs(ifOpts) do
    comp_ifaddrs(ifOpts, ktree_empty())
  end

  defp comp_ifaddrs(
         [{if__, [{:flags, flags} | opts]} | ifOpts],
         ifT
       ) do
    case ktree_is_defined(if__, ifT) do
      true ->
        comp_ifaddrs(
          ifOpts,
          ktree_update(
            if__,
            comp_ifaddrs_flags(
              flags,
              opts,
              ktree_get(
                if__,
                ifT
              )
            ),
            ifT
          )
        )

      false ->
        comp_ifaddrs(
          ifOpts,
          ktree_insert(
            if__,
            comp_ifaddrs_flags(flags, opts, ktree_empty()),
            ifT
          )
        )
    end
  end

  defp comp_ifaddrs([], ifT) do
    comp_ifaddrs_2(ktree_keys(ifT), ifT)
  end

  defp comp_ifaddrs_flags(flags, opts, flagsT) do
    case ktree_is_defined(flags, flagsT) do
      true ->
        ktree_update(flags, rev(opts, ktree_get(flags, flagsT)), flagsT)

      false ->
        ktree_insert(flags, rev(opts), flagsT)
    end
  end

  defp comp_ifaddrs_2([if__ | ifs], ifT) do
    flagsT = ktree_get(if__, ifT)

    [
      {if__, comp_ifaddrs_3(ktree_keys(flagsT), flagsT)}
      | comp_ifaddrs_2(ifs, ifT)
    ]
  end

  defp comp_ifaddrs_2([], _IfT) do
    []
  end

  defp comp_ifaddrs_3([flags | flagsL], flagsT) do
    [
      {:flags, flags}
      | hwaddr_last(
          rev(
            ktree_get(
              flags,
              flagsT
            )
          )
        )
    ] ++
      hwaddr_last(
        comp_ifaddrs_3(
          flagsL,
          flagsT
        )
      )
  end

  defp comp_ifaddrs_3([], _FlagsT) do
    []
  end

  defp hwaddr_last(opts) do
    hwaddr_last(opts, opts, [])
  end

  defp hwaddr_last([{:hwaddr, _} = opt | opts], l, r) do
    hwaddr_last(opts, l, [opt | r])
  end

  defp hwaddr_last([_ | opts], l, r) do
    hwaddr_last(opts, l, r)
  end

  defp hwaddr_last([], l, []) do
    l
  end

  defp hwaddr_last([], l, r) do
    rev(hwaddr_last(l, []), rev(r))
  end

  defp hwaddr_last([{:hwaddr, _} | opts], r) do
    hwaddr_last(opts, r)
  end

  defp hwaddr_last([opt | opts], r) do
    hwaddr_last(opts, [opt | r])
  end

  defp hwaddr_last([], r) do
    r
  end

  defp getifaddrs_ifget(_, []) do
    []
  end

  defp getifaddrs_ifget(s, [iF | iFs]) do
    case ifget(s, iF, [:flags]) do
      {:ok, [{:flags, flags}] = flagsVals} ->
        getOpts =
          case member(:pointtopoint, flags) do
            true ->
              [:dstaddr, :hwaddr]

            false ->
              case member(:broadcast, flags) do
                true ->
                  [:broadaddr, :hwaddr]

                false ->
                  [:hwaddr]
              end
          end

        getifaddrs_ifget(s, iFs, iF, flagsVals, [[:addr, :netmask] | getOpts])

      _ ->
        getifaddrs_ifget(s, iFs, iF, [], [:addr, :netmask, :hwaddr])
    end
  end

  defp getifaddrs_ifget(s, iFs, iF, flagsVals, opts) do
    optVals =
      case ifget(s, iF, opts) do
        {:ok, oVs} ->
          oVs

        _ ->
          []
      end

    [{iF, flagsVals ++ optVals} | getifaddrs_ifget(s, iFs)]
  end

  def getindex(s) when is_port(s) do
    {:error, :einval}
  end

  defp getprotocol(s) when is_port(s) do
    {:name, drv} = :erlang.port_info(s, :name)
    drv2protocol(drv)
  end

  def getservbyname(s, name, proto)
      when is_port(s) and
             is_atom(name) and is_atom(proto) do
    getservbyname1(s, :erlang.atom_to_list(name), :erlang.atom_to_list(proto))
  end

  def getservbyname(s, name, proto)
      when is_port(s) and
             is_atom(name) and is_list(proto) do
    getservbyname1(s, :erlang.atom_to_list(name), proto)
  end

  def getservbyname(s, name, proto)
      when is_port(s) and
             is_list(name) and is_atom(proto) do
    getservbyname1(s, name, :erlang.atom_to_list(proto))
  end

  def getservbyname(s, name, proto)
      when is_port(s) and
             is_list(name) and is_list(proto) do
    getservbyname1(s, name, proto)
  end

  def getservbyname(_, _, _) do
    {:error, :einval}
  end

  def getservbyport(s, port, proto)
      when is_port(s) and
             is_atom(proto) do
    getservbyport1(s, port, :erlang.atom_to_list(proto))
  end

  def getservbyport(s, port, proto)
      when is_port(s) and
             is_list(proto) do
    getservbyport1(s, port, proto)
  end

  def getservbyport(_, _, _) do
    {:error, :einval}
  end

  def detach(s) when is_port(s) do
    :erlang.unlink(s)
    :ok
  end

  def attach(s) when is_port(s) do
    try do
      :erlang.port_connect(s, self())
    catch
      :error, reason ->
        {:error, reason}
    else
      true ->
        :erlang.link(s)
        :ok
    end
  end

  def is_sockopt_val(opt, val) do
    type = type_opt(:set, opt)

    try do
      type_value(:set, type, val)
    catch
      _ ->
        false
    end
  end

  defp type_opt(:get, :raw) do
    [{[:int], [:int], [:binary_or_uint]}]
  end

  defp type_opt(_, :raw) do
    {:int, :int, :binary}
  end

  defp type_opt(:get, :sctp_status) do
    [{:record, r_sctp_status(assoc_id: [:sctp_assoc_id], _: [])}]
  end

  defp type_opt(:get, :sctp_get_peer_addr_info) do
    [
      {:record,
       r_sctp_paddrinfo(assoc_id: [[:sctp_assoc_id, 0]], address: [[:addr, {:any, 0}]], _: [])}
    ]
  end

  defp type_opt(_, opt) do
    type_opt_1(opt)
  end

  defp type_value(:get, :undefined) do
    false
  end

  defp type_value(:get, [{:record, types}]) do
    type_value_record(:get, types, :erlang.make_tuple(tuple_size(types), :undefined), 2)
  end

  defp type_value(:get, [_]) do
    false
  end

  defp type_value(:get, _) do
    true
  end

  defp type_value(_, :undefined, _) do
    false
  end

  defp type_value(_, [], :undefined) do
    true
  end

  defp type_value(_, [], _) do
    false
  end

  defp type_value(q, [type], value) do
    type_value_default(q, type, value)
  end

  defp type_value(:set, type, value) do
    type_value_default(:set, type, value)
  end

  defp type_value(_, _, :undefined) do
    true
  end

  defp type_value(_, _, _) do
    false
  end

  defp type_value_default(q, [type, default], :undefined) do
    type_value_1(q, type, default)
  end

  defp type_value_default(q, [type, _], value) do
    type_value_1(q, type, value)
  end

  defp type_value_default(q, type, value) do
    type_value_1(q, type, value)
  end

  defp type_value_1(q, {:record, types}, :undefined) do
    type_value_record(q, types, :erlang.make_tuple(tuple_size(types), :undefined), 2)
  end

  defp type_value_1(q, {:record, types}, values)
       when tuple_size(types) === tuple_size(values) do
    type_value_record(q, types, values, 2)
  end

  defp type_value_1(q, types, values)
       when tuple_size(types) === tuple_size(values) do
    type_value_tuple(q, types, values, 1)
  end

  defp type_value_1(_, type, value) do
    type_value_2(type, value)
  end

  defp type_value_tuple(q, types, values, n)
       when is_integer(n) and
              n <= tuple_size(types) do
    type_value(q, :erlang.element(n, types), :erlang.element(n, values)) and
      type_value_tuple(
        q,
        types,
        values,
        n + 1
      )
  end

  defp type_value_tuple(_, _, _, _) do
    true
  end

  defp type_value_record(q, types, values, n)
       when is_integer(n) and
              n <= tuple_size(types) do
    case type_value(q, :erlang.element(n, types), :erlang.element(n, values)) do
      true ->
        type_value_record(q, types, values, n + 1)

      false ->
        :erlang.throw({:type, {:record, q, types, values, n}})
    end
  end

  defp type_value_record(_, _, _, _) do
    true
  end

  defp enc_value(:get, [{:record, types}]) do
    enc_value_tuple(:get, types, :erlang.make_tuple(tuple_size(types), :undefined), 2)
  end

  defp enc_value(:get, _) do
    []
  end

  defp enc_value(_, [], _) do
    []
  end

  defp enc_value(q, [type], value) do
    enc_value_default(q, type, value)
  end

  defp enc_value(:set, type, value) do
    enc_value_default(:set, type, value)
  end

  defp enc_value(_, _, _) do
    []
  end

  defp enc_value_default(q, [type, default], :undefined) do
    enc_value_1(q, type, default)
  end

  defp enc_value_default(q, [type, _], value) do
    enc_value_1(q, type, value)
  end

  defp enc_value_default(q, type, value) do
    enc_value_1(q, type, value)
  end

  defp enc_value_1(q, {:record, types}, :undefined) do
    enc_value_tuple(q, types, :erlang.make_tuple(tuple_size(types), :undefined), 2)
  end

  defp enc_value_1(q, {:record, types}, values)
       when tuple_size(types) === tuple_size(values) do
    enc_value_tuple(q, types, values, 2)
  end

  defp enc_value_1(q, types, values)
       when tuple_size(types) === tuple_size(values) do
    enc_value_tuple(q, types, values, 1)
  end

  defp enc_value_1(_, type, value) do
    enc_value_2(type, value)
  end

  defp enc_value_tuple(q, types, values, n)
       when is_integer(n) and
              n <= tuple_size(types) do
    [
      enc_value(q, :erlang.element(n, types), :erlang.element(n, values))
      | enc_value_tuple(q, types, values, n + 1)
    ]
  end

  defp enc_value_tuple(_, _, _, _) do
    []
  end

  defp dec_value_tuple(types, list, n, acc)
       when is_integer(n) and
              n <= tuple_size(types) do
    {term, tail} =
      dec_value(
        :erlang.element(n, types),
        list
      )

    dec_value_tuple(types, tail, n + 1, [term | acc])
  end

  defp dec_value_tuple(_, list, _, acc) do
    {rev(acc), list}
  end

  defp borlist([v | vs], value) do
    borlist(vs, v ||| value)
  end

  defp borlist([], value) do
    value
  end

  defp enum_vals([enum | es], list) do
    case enum_val(enum, list) do
      false ->
        false

      {:value, value} ->
        [value | enum_vals(es, list)]
    end
  end

  defp enum_vals([], _) do
    []
  end

  defp enum_names(val, [{enum, bitVal} | list]) do
    cond do
      val &&& bitVal === bitVal ->
        [enum | enum_names(val, list)]

      true ->
        enum_names(val, list)
    end
  end

  defp enum_names(_, []) do
    []
  end

  defp enum_val(enum, [{enum, value} | _]) do
    {:value, value}
  end

  defp enum_val(enum, [_ | list]) do
    enum_val(enum, list)
  end

  defp enum_val(_, []) do
    false
  end

  defp enum_name(val, [{enum, val} | _]) do
    {:name, enum}
  end

  defp enum_name(val, [_ | list]) do
    enum_name(val, list)
  end

  defp enum_name(_, []) do
    false
  end

  defp encode_opt_val(opts) do
    try do
      {:ok, enc_opt_val(opts, [])}
    catch
      reason ->
        {:error, reason}
    end
  end

  defp enc_opt_val(opts, acc, opt, val) when is_atom(opt) do
    type = type_opt(:set, opt)

    case type_value(:set, type, val) do
      true ->
        enc_opt_val(
          opts,
          [[enc_opt(opt), enc_value(:set, type, val)] | acc]
        )

      false ->
        throw(:einval)
    end
  end

  defp enc_opt_val(_, _, _, _) do
    throw(:einval)
  end

  defp encode_opts(opts) do
    try do
      enc_opts(opts)
    catch
      error ->
        {:error, error}
    else
      buf ->
        {:ok, buf}
    end
  end

  defp enc_opts([{:raw, p, o, s} | opts]) do
    enc_opts(opts, :raw, {p, o, s})
  end

  defp enc_opts([{opt, val} | opts]) do
    enc_opts(opts, opt, val)
  end

  defp enc_opts([opt | opts]) do
    enc_opts(opts, opt)
  end

  defp enc_opts([]) do
    []
  end

  defp enc_opts(opts, opt) when is_atom(opt) do
    type = type_opt(:get, opt)

    case type_value(:get, type) do
      true ->
        [[enc_opt(opt), enc_value(:get, type)] | enc_opts(opts)]

      false ->
        throw(:einval)
    end
  end

  defp enc_opts(_, _) do
    throw(:einval)
  end

  defp enc_opts(opts, opt, val) when is_atom(opt) do
    type = type_opt(:get, opt)

    case type_value(:get, type, val) do
      true ->
        [
          [enc_opt(opt), enc_value(:get, type, val)]
          | enc_opts(opts)
        ]

      false ->
        throw(:einval)
    end
  end

  defp enc_opts(_, _, _) do
    throw(:einval)
  end

  defp decode_opt_val(buf) do
    try do
      dec_opt_val(buf)
    catch
      error ->
        {:error, error}
    else
      result ->
        {:ok, result}
    end
  end

  defp dec_opt_val([b | buf] = bBuf) do
    case dec_opt(b) do
      :undefined ->
        :erlang.error({:decode, bBuf})

      opt ->
        type = type_opt(:dec, opt)
        dec_opt_val(buf, opt, type)
    end
  end

  defp dec_opt_val([]) do
    []
  end

  defp dec_opt_val(buf, :raw, type) do
    {{p, o, b}, t} = dec_value(type, buf)
    [{:raw, p, o, b} | dec_opt_val(t)]
  end

  defp dec_opt_val(buf, :active, type) do
    case dec_value(type, buf) do
      {:multi, [[m0, m1] | t]} ->
        <<n::size(16)>> = :erlang.list_to_binary([m0, m1])
        [{:active, n} | dec_opt_val(t)]

      {val, t} ->
        [{:active, val} | dec_opt_val(t)]
    end
  end

  defp dec_opt_val(buf, opt, type) do
    {val, t} = dec_value(type, buf)
    [{opt, val} | dec_opt_val(t)]
  end

  defp need_template([{opt, :undefined} = oV | opts])
       when is_atom(opt) do
    [oV | need_template(opts)]
  end

  defp need_template([{opt, val} | opts]) when is_atom(opt) do
    case need_template(val, 2) do
      true ->
        [{opt, :undefined} | need_template(opts)]

      false ->
        need_template(opts)
    end
  end

  defp need_template([_ | opts]) do
    need_template(opts)
  end

  defp need_template([]) do
    []
  end

  defp need_template(t, n)
       when is_integer(n) and
              n <= tuple_size(t) do
    case :erlang.element(n, t) do
      :undefined ->
        true

      _ ->
        need_template(t, n + 1)
    end
  end

  defp need_template(_, _) do
    false
  end

  defp merge_options(
         [{opt, :undefined} | opts],
         [{opt, _} = t | templates]
       ) do
    [t | merge_options(opts, templates)]
  end

  defp merge_options(
         [{opt, val} | opts],
         [{opt, template} | templates]
       )
       when is_atom(opt) and tuple_size(val) >= 2 do
    key = :erlang.element(1, val)
    size = tuple_size(val)

    cond do
      size === tuple_size(template) and
          key === :erlang.element(1, template) ->
        [
          {opt, :erlang.list_to_tuple([key | merge_fields(val, template, 2)])}
          | merge_options(opts, templates)
        ]

      true ->
        throw({:merge, val, template})
    end
  end

  defp merge_options([optVal | opts], templates) do
    [optVal | merge_options(opts, templates)]
  end

  defp merge_options([], []) do
    []
  end

  defp merge_options(opts, templates) do
    throw({:merge, opts, templates})
  end

  defp merge_fields(opt, template, n)
       when is_integer(n) and
              n <= tuple_size(opt) do
    case :erlang.element(n, opt) do
      :undefined ->
        [:erlang.element(n, template) | merge_fields(opt, template, n + 1)]

      val ->
        [val | merge_fields(opt, template, n + 1)]
    end
  end

  defp merge_fields(_, _, _) do
    []
  end

  defp decode_ifopts([b | buf], acc) do
    case dec_ifopt(b) do
      :undefined ->
        {:error, :einval}

      opt ->
        {val, t} = dec_value(type_ifopt(opt), buf)
        decode_ifopts(t, [{opt, val} | acc])
    end
  end

  defp decode_ifopts(_, acc) do
    {:ok, acc}
  end

  defp encode_ifopts([opt | opts], acc) do
    case enc_ifopt(opt) do
      -1 ->
        {:error, :einval}

      b ->
        encode_ifopts(opts, [b | acc])
    end
  end

  defp encode_ifopts([], acc) do
    {:ok, acc}
  end

  defp encode_ifopt_val([{opt, val} | opts], buf) do
    type = type_ifopt(opt)

    try do
      type_value(:set, type, val)
    catch
      reason ->
        {:error, reason}
    else
      true ->
        encode_ifopt_val(
          opts,
          [buf, enc_ifopt(opt), enc_value(:set, type, val)]
        )

      false ->
        {:error, :einval}
    end
  end

  defp encode_ifopt_val([], buf) do
    {:ok, buf}
  end

  defp encode_subs(l) do
    try do
      enc_subs(l)
    catch
      error ->
        {:error, error}
    else
      result ->
        {:ok, result}
    end
  end

  defp decode_subs(bytes) do
    try do
      dec_subs(bytes)
    catch
      error ->
        {:error, error}
    else
      result ->
        {:ok, result}
    end
  end

  defp encode_stats(l) do
    try do
      enc_stats(l)
    catch
      error ->
        {:error, error}
    else
      result ->
        {:ok, result}
    end
  end

  defp decode_stats(bytes) do
    try do
      dec_stats(bytes)
    catch
      error ->
        {:error, error}
    else
      result ->
        {:ok, result}
    end
  end

  defp encode_ifname(name) when is_atom(name) do
    encode_ifname(:erlang.atom_to_list(name))
  end

  defp encode_ifname(name) do
    n = length(name)

    cond do
      n > 255 ->
        {:error, :einval}

      true ->
        {:ok, [n | name]}
    end
  end

  defp build_ifaddrs(cs) do
    build_ifaddrs(cs, [])
  end

  defp build_ifaddrs([], []) do
    []
  end

  defp build_ifaddrs([0 | cs], acc) do
    name = utf8_to_characters(rev(acc))
    {opts, rest} = build_ifaddrs_opts(cs, [])
    [{name, opts} | build_ifaddrs(rest)]
  end

  defp build_ifaddrs([c | cs], acc) do
    build_ifaddrs(cs, [c | acc])
  end

  defp build_ifaddrs_opts([0 | cs], acc) do
    {rev(acc), cs}
  end

  defp build_ifaddrs_opts([c | cs] = cCs, acc) do
    case dec_ifopt(c) do
      :undefined ->
        :erlang.error(:badarg, [cCs, acc])

      opt ->
        type = type_ifopt(opt)
        {val, rest} = dec_value(type, cs)
        build_ifaddrs_opts(rest, [{opt, val} | acc])
    end
  end

  defp build_iflist(cs) do
    build_iflist(cs, [], [])
  end

  defp build_iflist([0 | l], acc, [h | t]) do
    case rev(acc) do
      ^h ->
        build_iflist(l, [], [h | t])

      n ->
        build_iflist(l, [], [[n, h] | t])
    end
  end

  defp build_iflist([0 | l], acc, []) do
    build_iflist(l, [], [rev(acc)])
  end

  defp build_iflist([c | l], acc, list) do
    build_iflist(l, [c | acc], list)
  end

  defp build_iflist([], [], list) do
    rev(list)
  end

  defp build_iflist([], acc, list) do
    build_iflist([0], acc, list)
  end

  defp rev(l) do
    rev(l, [])
  end

  defp rev([c | l], acc) do
    rev(l, [c | acc])
  end

  defp rev([], acc) do
    acc
  end

  defp split(n, l) do
    split(n, l, [])
  end

  defp split(0, l, r) when is_list(l) do
    {rev(r), l}
  end

  defp split(n, [h | t], r) when is_integer(n) and n > 0 do
    split(n - 1, t, [h | r])
  end

  defp len(l, n) do
    len(l, n, 0)
  end

  defp len([], n, c) when is_integer(n) and n >= 0 do
    c
  end

  defp len(l, 0, _) when is_list(l) do
    :undefined
  end

  defp len([_ | l], n, c)
       when is_integer(n) and
              n >= 0 do
    len(l, n - 1, c + 1)
  end

  defp member(x, [x | _]) do
    true
  end

  defp member(x, [_ | xs]) do
    member(x, xs)
  end

  defp member(_, []) do
    false
  end

  defp ktree_empty() do
    {[], tree()}
  end

  defp ktree_is_defined(key, {_, t}) do
    tree(t, key, :is_defined)
  end

  defp ktree_get(key, {_, t}) do
    tree(t, key, :get)
  end

  defp ktree_insert(key, v, {keys, t}) do
    {[key | keys], tree(t, key, {:insert, v})}
  end

  defp ktree_update(key, v, {keys, t}) do
    {keys, tree(t, key, {:update, v})}
  end

  defp ktree_keys({keys, _}) do
    rev(keys)
  end

  defp tree() do
    nil
  end

  defp tree(t, key, op) do
    tree(t, key, op, :erlang.phash2(key))
  end

  defp tree(nil, _, :is_defined, _) do
    false
  end

  defp tree(nil, k, {:insert, v}, _) do
    {k, v, nil, nil}
  end

  defp tree({k, _, _, _}, k, :is_defined, _) do
    true
  end

  defp tree({k, v, _, _}, k, :get, _) do
    v
  end

  defp tree({k, _, l, r}, k, {:update, v}, _) do
    {k, v, l, r}
  end

  defp tree({k0, v0, l, r}, k, op, h) do
    h0 = :erlang.phash2(k0)

    cond do
      h0 < h or (h0 === h and k0 < k) ->
        cond do
          is_tuple(op) ->
            {k0, v0, tree(l, k, op, h), r}

          true ->
            tree(l, k, op, h)
        end

      true ->
        cond do
          is_tuple(op) ->
            {k0, v0, l, tree(r, k, op, h)}

          true ->
            tree(r, k, op, h)
        end
    end
  end

  defp utf8_to_characters([]) do
    []
  end

  defp utf8_to_characters([b | bs] = arg) when b &&& 255 === b do
    cond do
      248 <= b ->
        :erlang.error(:badarg, [arg])

      240 <= b ->
        utf8_to_characters(bs, b &&& 7, 3)

      224 <= b ->
        utf8_to_characters(bs, b &&& 15, 2)

      192 <= b ->
        utf8_to_characters(bs, b &&& 31, 1)

      128 <= b ->
        :erlang.error(:badarg, [arg])

      true ->
        [b | utf8_to_characters(bs)]
    end
  end

  defp utf8_to_characters(bs, u, 0) do
    [u | utf8_to_characters(bs)]
  end

  defp utf8_to_characters([b | bs], u, n) when (b &&& 63) ||| 128 === b do
    utf8_to_characters(bs, u <<< 6 ||| (b &&& 63), n - 1)
  end

  defp ip4_to_bytes({a, b, c, d}) do
    [a &&& 255, b &&& 255, c &&& 255, d &&& 255]
  end

  defp get_addrs([]) do
    []
  end

  defp get_addrs([f | addrs]) do
    {addr, rest} = get_addr(f, addrs)
    [addr | get_addrs(rest)]
  end

  defp get_ip4([[a, b, c, d] | t]) do
    {{a, b, c, d}, t}
  end
end
