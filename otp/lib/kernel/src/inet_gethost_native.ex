defmodule :m_inet_gethost_native do
  use Bitwise
  @behaviour :supervisor_bridge
  require Record

  Record.defrecord(:r_hostent, :hostent,
    h_name: :undefined,
    h_aliases: [],
    h_addrtype: :undefined,
    h_length: :undefined,
    h_addr_list: []
  )

  Record.defrecord(:r_request, :request,
    rid: :undefined,
    op: :undefined,
    proto: :undefined,
    rdata: :undefined,
    clients: []
  )

  Record.defrecord(:r_statistics, :statistics,
    netdb_timeout: 0,
    netdb_internal: 0,
    port_crash: 0,
    notsup: 0,
    host_not_found: 0,
    try_again: 0,
    no_recovery: 0,
    no_data: 0
  )

  Record.defrecord(:r_state, :state,
    port: :noport,
    timeout: 8000,
    requests: :undefined,
    req_index: :undefined,
    parent: :undefined,
    pool_size: 4,
    statistics: :undefined
  )

  def init([]) do
    ref = make_ref()
    saveTE = :erlang.process_flag(:trap_exit, true)
    pid = spawn_link(:inet_gethost_native, :server_init, [self(), ref])

    receive do
      ^ref ->
        :erlang.process_flag(:trap_exit, saveTE)
        {:ok, pid, pid}

      {:EXIT, ^pid, message} ->
        :erlang.process_flag(:trap_exit, saveTE)
        {:error, message}
    after
      10000 ->
        :erlang.process_flag(:trap_exit, saveTE)
        {:error, {:timeout, :inet_gethost_native}}
    end
  end

  def start_link() do
    :supervisor_bridge.start_link({:local, :inet_gethost_native_sup}, :inet_gethost_native, [])
  end

  def start_raw() do
    spawn(:inet_gethost_native, :run_once, [])
  end

  def run_once() do
    port = do_open_port(get_poolsize(), get_extra_args())
    timeout = :inet_db.res_option(:timeout) * 4

    {pid, r, request} =
      receive do
        {{pid0, r0}, {1, proto0, name0}} ->
          {pid0, r0, [<<1::size(32), 1::size(8), proto0::size(8)>>, name0, 0]}

        {{pid1, r1}, {2, proto1, data1}} ->
          {pid1, r1, <<1::size(32), 2::size(8), proto1::size(8), data1::binary>>}
      after
        timeout ->
          exit(:normal)
      end

    try do
      :erlang.port_command(port, request)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    receive do
      {^port, {:data, <<1::size(32), binReply::binary>>}} ->
        send(pid, {r, {:ok, binReply}})
    after
      timeout ->
        send(pid, {r, {:error, :timeout}})
    end
  end

  def terminate(_Reason, pid) do
    try do
      :erlang.exit(pid, :kill)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def server_init(starter, ref) do
    :erlang.process_flag(:trap_exit, true)

    case :erlang.whereis(:inet_gethost_native) do
      :undefined ->
        case (try do
                :erlang.register(:inet_gethost_native, self())
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          true ->
            send(starter, ref)

          _ ->
            exit({:already_started, :erlang.whereis(:inet_gethost_native)})
        end

      winner ->
        exit({:already_started, winner})
    end

    poolsize = get_poolsize()
    port = do_open_port(poolsize, get_extra_args())
    timeout = :inet_db.res_option(:timeout) * 4
    :erlang.put(:rid, 0)
    :erlang.put(:num_requests, 0)

    requestTab =
      :ets.new(
        :ign_requests,
        [{:keypos, r_request(:rid)}, :set, :protected]
      )

    requestIndex =
      :ets.new(
        :ign_req_index,
        [:set, :protected]
      )

    state =
      r_state(
        port: port,
        timeout: timeout,
        requests: requestTab,
        req_index: requestIndex,
        pool_size: poolsize,
        statistics: r_statistics(),
        parent: starter
      )

    main_loop(state)
  end

  def main_loop(state) do
    receive do
      any ->
        handle_message(any, state)
    end
  end

  defp handle_message(
         {{pid, _} = client, {1, proto, name} = r},
         state
       )
       when is_pid(pid) do
    newState = do_handle_call(r, client, state, [<<1::size(8), proto::size(8)>>, name, 0])
    main_loop(newState)
  end

  defp handle_message(
         {{pid, _} = client, {2, proto, data} = r},
         state
       )
       when is_pid(pid) do
    newState = do_handle_call(r, client, state, <<2::size(8), proto::size(8), data::binary>>)
    main_loop(newState)
  end

  defp handle_message({{pid, ref}, {4, ctl, data}}, state)
       when is_pid(pid) do
    try do
      :erlang.port_command(
        r_state(state, :port),
        <<4_294_967_295::size(32), 4::size(8), ctl::size(8), data::binary>>
      )
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    send(pid, {ref, :ok})
    main_loop(state)
  end

  defp handle_message({{pid, ref}, :restart_port}, state)
       when is_pid(pid) do
    newPort = restart_port(state)
    send(pid, {ref, :ok})
    main_loop(r_state(state, port: newPort))
  end

  defp handle_message({port, {:data, data}}, state = r_state(port: port)) do
    newState =
      case data do
        <<rID::size(32), binReply::binary>> ->
          case binReply do
            <<unit, _::binary>>
            when unit === 0 or unit === 4 or
                   unit === 16 ->
              case pick_request(state, rID) do
                false ->
                  state

                req ->
                  :lists.foreach(
                    fn {p, r, tR} ->
                      _ = :erlang.cancel_timer(tR)
                      send(p, {r, {:ok, binReply}})
                    end,
                    r_request(req, :clients)
                  )

                  state
              end

            _UnitError ->
              newPort = restart_port(state)
              r_state(state, port: newPort)
          end

        _BasicFormatError ->
          newPort = restart_port(state)
          r_state(state, port: newPort)
      end

    main_loop(newState)
  end

  defp handle_message(
         {:EXIT, port, _Reason},
         state = r_state(port: port)
       ) do
    :noop
    newPort = restart_port(state)
    main_loop(r_state(state, port: newPort))
  end

  defp handle_message({port, :eof}, state = r_state(port: port)) do
    :noop
    newPort = restart_port(state)
    main_loop(r_state(state, port: newPort))
  end

  defp handle_message({:timeout, pid, rID}, state) do
    case pick_client(state, rID, pid) do
      false ->
        false

      {:more, {p, r, _}} ->
        send(p, {r, {:error, :timeout}})

      {:last, {lP, lR, _}} ->
        send(lP, {lR, {:error, :timeout}})
        _ = pick_request(state, rID)

        try do
          :erlang.port_command(
            r_state(state, :port),
            <<rID::size(32), 3>>
          )
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end
    end

    main_loop(state)
  end

  defp handle_message({:system, from, req}, state) do
    :sys.handle_system_msg(req, from, r_state(state, :parent), :inet_gethost_native, [], state)
  end

  defp handle_message(_, state) do
    main_loop(state)
  end

  defp do_handle_call(r, client0, state, rData) do
    req = find_request(state, r)
    timeout = r_state(state, :timeout)
    {p, ref} = client0
    tR = :erlang.send_after(timeout, self(), {:timeout, p, r_request(req, :rid)})
    client = {p, ref, tR}

    case r_request(req, :clients) do
      [] ->
        realRData = [<<r_request(req, :rid)::size(32)>> | rData]

        try do
          :erlang.port_command(r_state(state, :port), realRData)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        :ets.insert(
          r_state(state, :requests),
          r_request(req, clients: [client])
        )

      tail ->
        :ets.insert(
          r_state(state, :requests),
          r_request(req, clients: [client | tail])
        )
    end

    state
  end

  defp find_request(state, r = {op, proto, data}) do
    case :ets.lookup(r_state(state, :req_index), r) do
      [{^r, rid}] ->
        [ret] = :ets.lookup(r_state(state, :requests), rid)
        ret

      [] ->
        nRid = get_rid()
        req = r_request(rid: nRid, op: op, proto: proto, rdata: data)
        :ets.insert(r_state(state, :requests), req)
        :ets.insert(r_state(state, :req_index), {r, nRid})

        :erlang.put(
          :num_requests,
          :erlang.get(:num_requests) + 1
        )

        req
    end
  end

  defp pick_request(state, rID) do
    case :ets.lookup(r_state(state, :requests), rID) do
      [] ->
        false

      [r_request(rid: ^rID, op: op, proto: proto, rdata: data) = r] ->
        :ets.delete(r_state(state, :requests), rID)
        :ets.delete(r_state(state, :req_index), {op, proto, data})

        :erlang.put(
          :num_requests,
          :erlang.get(:num_requests) - 1
        )

        r
    end
  end

  defp pick_client(state, rID, clid) do
    case :ets.lookup(r_state(state, :requests), rID) do
      [] ->
        false

      [r] ->
        case r_request(r, :clients) do
          [soleClient] ->
            {:last, soleClient}

          cList ->
            case :lists.keyfind(clid, 1, cList) do
              false ->
                false

              client ->
                nCList = :lists.keydelete(clid, 1, cList)
                :ets.insert(r_state(state, :requests), r_request(r, clients: nCList))
                {:more, client}
            end
        end
    end
  end

  defp get_rid() do
    new = rem(:erlang.get(:rid) + 1, 134_217_727)
    :erlang.put(:rid, new)
    new
  end

  defp foreach(fun, table) do
    foreach(fun, table, :ets.first(table))
  end

  defp foreach(_Fun, _Table, :"$end_of_table") do
    :ok
  end

  defp foreach(fun, table, key) do
    [object] = :ets.lookup(table, key)
    fun.(object)
    foreach(fun, table, :ets.next(table, key))
  end

  defp restart_port(r_state(port: port, requests: requests)) do
    try do
      :erlang.port_close(port)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    newPort = do_open_port(get_poolsize(), get_extra_args())

    foreach(
      fn r_request(rid: rid, op: op, proto: proto, rdata: rdata) ->
        case op do
          1 ->
            :erlang.port_command(
              newPort,
              [<<rid::size(32), 1::size(8), proto::size(8)>>, rdata, 0]
            )

          2 ->
            :erlang.port_command(
              newPort,
              <<rid::size(32), 2::size(8), proto::size(8), rdata::binary>>
            )
        end
      end,
      requests
    )

    newPort
  end

  defp do_open_port(poolsize, extraArgs) do
    try do
      :erlang.open_port(
        {:spawn, 'inet_gethost' ++ ' ' ++ :erlang.integer_to_list(poolsize) ++ ' ' ++ extraArgs},
        [{:packet, 4}, :eof, :binary, :overlapped_io]
      )
    catch
      :error, _ ->
        :erlang.open_port(
          {:spawn,
           'inet_gethost' ++ ' ' ++ :erlang.integer_to_list(poolsize) ++ ' ' ++ extraArgs},
          [{:packet, 4}, :eof, :binary]
        )
    end
  end

  defp get_extra_args() do
    firstPart =
      case :application.get_env(
             :kernel,
             :gethost_prioritize
           ) do
        {:ok, false} ->
          ' -ng'

        _ ->
          ''
      end

    case :application.get_env(
           :kernel,
           :gethost_extra_args
         ) do
      {:ok, l} when is_list(l) ->
        firstPart ++ ' ' ++ l

      _ ->
        firstPart ++ ''
    end
  end

  defp get_poolsize() do
    case :application.get_env(
           :kernel,
           :gethost_poolsize
         ) do
      {:ok, i} when is_integer(i) ->
        i

      _ ->
        4
    end
  end

  def system_continue(_Parent, _, state) do
    main_loop(state)
  end

  def system_terminate(reason, _Parent, _, _State) do
    exit(reason)
  end

  def system_code_change(state, _Module, _OldVsn, _Extra) do
    {:ok, state}
  end

  def gethostbyname(name) do
    gethostbyname(name, :inet)
  end

  def gethostbyname(name, :inet) when is_list(name) do
    getit(1, 1, name, name)
  end

  def gethostbyname(name, :inet6) when is_list(name) do
    getit(1, 2, name, name)
  end

  def gethostbyname(name, type) when is_atom(name) do
    gethostbyname(:erlang.atom_to_list(name), type)
  end

  def gethostbyname(_, _) do
    {:error, :formerr}
  end

  def gethostbyaddr({a, b, c, d} = addr)
      when is_integer(a) and
             a < 256 and is_integer(b) and b < 256 and
             is_integer(c) and c < 256 and
             is_integer(d) and d < 256 do
    getit(2, 1, <<a, b, c, d>>, addr)
  end

  def gethostbyaddr({a, b, c, d, e, f, g, h} = addr)
      when is_integer(a) and a < 65536 and is_integer(b) and
             b < 65536 and is_integer(c) and c < 65536 and
             is_integer(d) and d < 65536 and is_integer(e) and
             e < 65536 and is_integer(f) and f < 65536 and
             is_integer(g) and g < 65536 and is_integer(h) and
             h < 65536 do
    getit(
      2,
      2,
      <<a::size(16), b::size(16), c::size(16), d::size(16), e::size(16), f::size(16), g::size(16),
        h::size(16)>>,
      addr
    )
  end

  def gethostbyaddr(addr) when is_list(addr) do
    case :inet_parse.address(addr) do
      {:ok, iP} ->
        gethostbyaddr(iP)

      _Error ->
        {:error, :formerr}
    end
  end

  def gethostbyaddr(addr) when is_atom(addr) do
    gethostbyaddr(:erlang.atom_to_list(addr))
  end

  def gethostbyaddr(_) do
    {:error, :formerr}
  end

  def control({:debug_level, level}) when is_integer(level) do
    getit(4, 0, <<level::size(32)>>, :undefined)
  end

  def control(:soft_restart) do
    getit(:restart_port, :undefined)
  end

  def control(_) do
    {:error, :formerr}
  end

  defp getit(op, proto, data, defaultName) do
    getit({op, proto, data}, defaultName)
  end

  defp getit(req, defaultName) do
    pid = ensure_started()
    ref = make_ref()
    send(pid, {{self(), ref}, req})

    receive do
      {^ref, {:ok, binHostent}} ->
        parse_address(binHostent, defaultName)

      {^ref, result} ->
        result
    after
      5000 ->
        ref2 = :erlang.monitor(:process, pid)

        res2 =
          receive do
            {^ref, {:ok, binHostent}} ->
              parse_address(binHostent, defaultName)

            {^ref, result} ->
              result

            {:DOWN, ^ref2, :process, ^pid, reason} ->
              {:error, reason}
          end

        try do
          :erlang.demonitor(ref2, [:flush])
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        res2
    end
  end

  defp do_start(sup, c) do
    {child, _, _, _, _, _} = c

    case :supervisor.start_child(sup, c) do
      {:ok, _} ->
        :ok

      {:error, {:already_started, pid}} when is_pid(pid) ->
        :ok

      {:error, {{:already_started, pid}, _Child}}
      when is_pid(pid) ->
        :ok

      {:error, :already_present} ->
        _ = :supervisor.delete_child(sup, child)
        do_start(sup, c)
    end
  end

  defp ensure_started() do
    case :erlang.whereis(:inet_gethost_native) do
      :undefined ->
        c =
          {:inet_gethost_native_sup, {:inet_gethost_native, :start_link, []}, :temporary, 1000,
           :worker, [:inet_gethost_native]}

        case :erlang.whereis(:kernel_safe_sup) do
          :undefined ->
            case :erlang.whereis(:net_sup) do
              :undefined ->
                start_raw()

              _ ->
                do_start(:net_sup, c)

                case :erlang.whereis(:inet_gethost_native) do
                  :undefined ->
                    exit({:could_not_start_server, :inet_gethost_native})

                  pid0 ->
                    pid0
                end
            end

          _ ->
            do_start(:kernel_safe_sup, c)

            case :erlang.whereis(:inet_gethost_native) do
              :undefined ->
                exit({:could_not_start_server, :inet_gethost_native})

              pid1 ->
                pid1
            end
        end

      pid ->
        pid
    end
  end

  defp parse_address(binHostent, defaultName) do
    case (try do
            case binHostent do
              <<0, errstring::binary>> ->
                {:error, :erlang.list_to_atom(listify(errstring))}

              <<4, naddr::size(32), t0::binary>> ->
                {t1, addresses} = pick_addresses_v4(naddr, t0)

                {name, names} =
                  expand_default_name(
                    pick_names(t1),
                    defaultName
                  )

                {:ok,
                 r_hostent(
                   h_addr_list: addresses,
                   h_addrtype: :inet,
                   h_aliases: names,
                   h_length: 4,
                   h_name: name
                 )}

              <<16, naddr::size(32), t0::binary>> ->
                {t1, addresses} = pick_addresses_v6(naddr, t0)

                {name, names} =
                  expand_default_name(
                    pick_names(t1),
                    defaultName
                  )

                {:ok,
                 r_hostent(
                   h_addr_list: addresses,
                   h_addrtype: :inet6,
                   h_aliases: names,
                   h_length: 16,
                   h_name: name
                 )}

              _Else ->
                {:error, {:internal_error, {:malformed_response, binHostent}}}
            end
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        reason

      normal ->
        normal
    end
  end

  defp expand_default_name([], defaultName) when is_list(defaultName) do
    {defaultName, []}
  end

  defp expand_default_name([], defaultName) when is_tuple(defaultName) do
    {:inet_parse.ntoa(defaultName), []}
  end

  defp expand_default_name([name | names], defaultName)
       when is_list(defaultName) or is_tuple(defaultName) do
    {name, names}
  end

  defp listify(bin) do
    n = byte_size(bin) - 1
    <<bin2::size(n)-binary, ch>> = bin

    case ch do
      0 ->
        listify(bin2)

      _ ->
        :erlang.binary_to_list(bin)
    end
  end

  defp pick_addresses_v4(0, tail) do
    {tail, []}
  end

  defp pick_addresses_v4(n, <<a, b, c, d, tail::binary>>) do
    {nTail, oList} = pick_addresses_v4(n - 1, tail)
    {nTail, [{a, b, c, d} | oList]}
  end

  defp pick_addresses_v6(0, tail) do
    {tail, []}
  end

  defp pick_addresses_v6(
         num,
         <<a::size(16), b::size(16), c::size(16), d::size(16), e::size(16), f::size(16),
           g::size(16), h::size(16), tail::binary>>
       ) do
    {nTail, oList} = pick_addresses_v6(num - 1, tail)
    {nTail, [{a, b, c, d, e, f, g, h} | oList]}
  end

  defp ndx(ch, bin) do
    ndx(ch, 0, byte_size(bin), bin)
  end

  defp ndx(_, n, n, _) do
    :undefined
  end

  defp ndx(ch, i, n, bin) do
    case bin do
      <<_::size(i)-binary, ^ch, _::binary>> ->
        i

      _ ->
        ndx(ch, i + 1, n, bin)
    end
  end

  defp pick_names(<<length::size(32), namelist::binary>>) do
    pick_names(length, namelist)
  end

  defp pick_names(0, <<>>) do
    []
  end

  defp pick_names(0, _) do
    exit({:error, :format_error})
  end

  defp pick_names(_N, <<>>) do
    exit({:error, :format_error})
  end

  defp pick_names(n, bin) do
    ndx = ndx(0, bin)
    <<str::size(ndx)-binary, 0, rest::binary>> = bin
    [:erlang.binary_to_list(str) | pick_names(n - 1, rest)]
  end
end
