defmodule :m_erl_epmd do
  use Bitwise
  import Kernel, except: [to_string: 1]
  import :lists, only: [reverse: 1]
  @behaviour :gen_server
  require Record
  Record.defrecord(:r_state, :state, socket: :undefined, port_no: -1, name: '')
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

  Record.defrecord(:r_hostent, :hostent,
    h_name: :undefined,
    h_aliases: [],
    h_addrtype: :undefined,
    h_length: :undefined,
    h_addr_list: []
  )

  def start() do
    :gen_server.start({:local, :erl_epmd}, :erl_epmd, [], [])
  end

  def start_link() do
    :gen_server.start_link({:local, :erl_epmd}, :erl_epmd, [], [])
  end

  def stop() do
    :gen_server.call(:erl_epmd, :stop, :infinity)
  end

  def port_please(node, host) do
    port_please(node, host, :infinity)
  end

  def port_please(node, hostName, timeout) do
    case listen_port_please(node, hostName) do
      {:ok, 0} ->
        case getepmdbyname(hostName, timeout) do
          {:ok, epmdAddr} ->
            get_port(node, epmdAddr, timeout)

          error ->
            :noop
            error
        end

      {:ok, prt} ->
        {:port, prt, 5}
    end
  end

  defp getepmdbyname(hostName, timeout) when is_atom(hostName) do
    getepmdbyname(:erlang.atom_to_list(hostName), timeout)
  end

  defp getepmdbyname(hostName, timeout) when is_list(hostName) do
    family =
      case :inet_db.res_option(:inet6) do
        true ->
          :inet6

        false ->
          :inet
      end

    case :inet.gethostbyname(hostName, family, timeout) do
      {:ok, r_hostent(h_addr_list: [epmdAddr | _])} ->
        {:ok, epmdAddr}

      _Else ->
        :noport
    end
  end

  defp getepmdbyname(hostName, _Timeout) do
    {:ok, hostName}
  end

  def listen_port_please(_Name, _Host) do
    try do
      {:ok, [[stringPort]]} = :init.get_argument(:erl_epmd_port)
      port = :erlang.list_to_integer(stringPort)
      {:ok, port}
    catch
      :error, _ ->
        {:ok, 0}
    end
  end

  def names() do
    {:ok, h} = :inet.gethostname()
    names(h)
  end

  def names(hostName) do
    case getepmdbyname(hostName, :infinity) do
      {:ok, epmdAddr} ->
        get_names(epmdAddr)

      else__ ->
        else__
    end
  end

  def register_node(name, portNo) do
    register_node(name, portNo, :inet)
  end

  def register_node(name, portNo, :inet_tcp) do
    register_node(name, portNo, :inet)
  end

  def register_node(name, portNo, :inet6_tcp) do
    register_node(name, portNo, :inet6)
  end

  def register_node(name, portNo, family) do
    :gen_server.call(:erl_epmd, {:register, name, portNo, family}, :infinity)
  end

  def address_please(_Name, host, addressFamily) do
    :inet.getaddr(host, addressFamily)
  end

  def init(_) do
    {:ok, r_state(socket: -1)}
  end

  def handle_call({:register, name, portNo, family}, _From, state) do
    case r_state(state, :socket) do
      p when p < 0 ->
        case do_register_node(name, portNo, family) do
          {:alive, socket, creation} ->
            s = r_state(state, socket: socket, port_no: portNo, name: name)
            {:reply, {:ok, creation}, s}

          error ->
            case :init.get_argument(:erl_epmd_port) do
              {:ok, _} ->
                {:reply, {:ok, -1}, r_state(state, socket: -1, port_no: portNo, name: name)}

              :error ->
                {:reply, error, state}
            end
        end

      _ ->
        {:reply, {:error, :already_registered}, state}
    end
  end

  def handle_call(:client_info_req, _From, state) do
    reply = {:ok, {:r4, r_state(state, :name), r_state(state, :port_no)}}
    {:reply, reply, state}
  end

  def handle_call(:stop, _From, state) do
    {:stop, :shutdown, :ok, state}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, state)
      when r_state(state, :socket) === socket do
    {:noreply, r_state(state, socket: -1)}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def terminate(_, r_state(socket: socket)) when socket > 0 do
    close(socket)
    :ok
  end

  def terminate(_, _) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp get_epmd_port() do
    case :init.get_argument(:epmd_port) do
      {:ok, [[portStr | _] | _]} when is_list(portStr) ->
        :erlang.list_to_integer(portStr)

      :error ->
        4369
    end
  end

  def open() do
    open({127, 0, 0, 1})
  end

  def open({a, b, c, d} = epmdAddr)
      when (a ||| b ||| c ||| d) &&& ~~~255 === 0 do
    :gen_tcp.connect(epmdAddr, get_epmd_port(), [:inet])
  end

  def open({a, b, c, d, e, f, g, h} = epmdAddr)
      when (a ||| b ||| c ||| d ||| e ||| f ||| g ||| h) &&& ~~~65535 === 0 do
    :gen_tcp.connect(epmdAddr, get_epmd_port(), [:inet6])
  end

  def open({a, b, c, d} = epmdAddr, timeout)
      when (a ||| b ||| c ||| d) &&& ~~~255 === 0 do
    :gen_tcp.connect(epmdAddr, get_epmd_port(), [:inet], timeout)
  end

  def open({a, b, c, d, e, f, g, h} = epmdAddr, timeout)
      when (a ||| b ||| c ||| d ||| e ||| f ||| g ||| h) &&& ~~~65535 === 0 do
    :gen_tcp.connect(epmdAddr, get_epmd_port(), [:inet6], timeout)
  end

  defp close(socket) do
    :gen_tcp.close(socket)
  end

  defp do_register_node(nodeName, tcpPort, family) do
    localhost =
      case family do
        :inet ->
          open({127, 0, 0, 1})

        :inet6 ->
          open({0, 0, 0, 0, 0, 0, 0, 1})
      end

    case localhost do
      {:ok, socket} ->
        name = to_string(nodeName)
        extra = ''
        elen = length(extra)
        len = 1 + 2 + 1 + 1 + 2 + 2 + 2 + length(name) + 2 + elen

        packet = [
          [len >>> 8 &&& 255, len &&& 255],
          ?x,
          [tcpPort >>> 8 &&& 255, tcpPort &&& 255],
          ?M,
          0,
          [epmd_dist_high() >>> 8 &&& 255, epmd_dist_high() &&& 255],
          [epmd_dist_low() >>> 8 &&& 255, epmd_dist_low() &&& 255],
          [length(name) >>> 8 &&& 255, length(name) &&& 255],
          name,
          [elen >>> 8 &&& 255, elen &&& 255],
          extra
        ]

        case :gen_tcp.send(socket, packet) do
          :ok ->
            wait_for_reg_reply(socket, [])

          error ->
            close(socket)
            error
        end

      error ->
        error
    end
  end

  defp epmd_dist_high() do
    case :os.getenv('ERL_EPMD_DIST_HIGH') do
      false ->
        6

      version ->
        case (try do
                :erlang.list_to_integer(version)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          n when is_integer(n) and n < 6 ->
            n

          _ ->
            6
        end
    end
  end

  defp epmd_dist_low() do
    case :os.getenv('ERL_EPMD_DIST_LOW') do
      false ->
        5

      version ->
        case (try do
                :erlang.list_to_integer(version)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          n when is_integer(n) and n > 5 ->
            n

          _ ->
            5
        end
    end
  end

  defp wait_for_reg_reply(socket, soFar) do
    receive do
      {:tcp, ^socket, data0} ->
        case soFar ++ data0 do
          [?v, result, a, b, c, d] ->
            case result do
              0 ->
                {:alive, socket, a <<< 24 ||| b <<< 16 ||| c <<< 8 ||| d}

              _ ->
                {:error, :duplicate_name}
            end

          [?y, result, a, b] ->
            case result do
              0 ->
                {:alive, socket, a <<< 8 ||| b}

              _ ->
                {:error, :duplicate_name}
            end

          data when length(data) < 4 ->
            wait_for_reg_reply(socket, data)

          garbage ->
            {:error, {:garbage_from_epmd, garbage}}
        end

      {:tcp_closed, ^socket} ->
        {:error, :epmd_close}
    after
      10000 ->
        :gen_tcp.close(socket)
        {:error, :no_reg_reply_from_epmd}
    end
  end

  defp get_port(node, epmdAddress, timeout) do
    case open(epmdAddress, timeout) do
      {:ok, socket} ->
        name = to_string(node)
        len = 1 + length(name)
        msg = [[len >>> 8 &&& 255, len &&& 255], ?z, name]

        case :gen_tcp.send(socket, msg) do
          :ok ->
            wait_for_port_reply(socket, [])

          _Error ->
            :noop
            :noport
        end

      _Error ->
        :noport
    end
  end

  defp wait_for_port_reply(socket, soFar) do
    receive do
      {:tcp, ^socket, data0} ->
        case soFar ++ data0 do
          [[?w, result] | rest] ->
            case result do
              0 ->
                wait_for_port_reply_cont(socket, rest)

              _ ->
                :noop
                wait_for_close(socket, :noport)
            end

          data when length(data) < 2 ->
            wait_for_port_reply(socket, data)

          garbage ->
            :noop
            {:error, {:garbage_from_epmd, garbage}}
        end

      {:tcp_closed, ^socket} ->
        :noop
        :closed
    after
      10000 ->
        :noop
        :gen_tcp.close(socket)
        :noport
    end
  end

  defp wait_for_port_reply_cont(socket, soFar) when length(soFar) >= 10 do
    wait_for_port_reply_cont2(socket, soFar)
  end

  defp wait_for_port_reply_cont(socket, soFar) do
    receive do
      {:tcp, ^socket, data0} ->
        case soFar ++ data0 do
          data when length(data) >= 10 ->
            wait_for_port_reply_cont2(socket, data)

          data when length(data) < 10 ->
            wait_for_port_reply_cont(socket, data)

          garbage ->
            :noop
            {:error, {:garbage_from_epmd, garbage}}
        end

      {:tcp_closed, ^socket} ->
        :noop
        :noport
    after
      10000 ->
        :noop
        :gen_tcp.close(socket)
        :noport
    end
  end

  defp wait_for_port_reply_cont2(socket, data) do
    [
      [a, b, _Type, _Proto, highA, highB, lowA, lowB, nLenA, nLenB]
      | rest
    ] = data

    wait_for_port_reply_name(socket, nLenA <<< 8 ||| nLenB, rest)
    low = lowA <<< 8 ||| lowB
    high = highA <<< 8 ||| highB
    version = best_version(low, high)
    {:port, a <<< 8 ||| b, version}
  end

  defp wait_for_port_reply_name(socket, len, sofar) do
    receive do
      {:tcp, ^socket, _Data} ->
        wait_for_port_reply_name(socket, len, sofar)

      {:tcp_closed, ^socket} ->
        :ok
    end
  end

  defp best_version(low, high) do
    ourLow = epmd_dist_low()
    ourHigh = epmd_dist_high()
    select_best_version(ourLow, ourHigh, low, high)
  end

  defp select_best_version(l1, _H1, _L2, h2) when l1 > h2 do
    0
  end

  defp select_best_version(_L1, h1, l2, _H2) when l2 > h1 do
    0
  end

  defp select_best_version(_L1, h1, _L2, h2) do
    :erlang.min(h1, h2)
  end

  defp wait_for_close(socket, reply) do
    receive do
      {:tcp_closed, ^socket} ->
        reply
    after
      10000 ->
        :gen_tcp.close(socket)
        reply
    end
  end

  defp to_string(s) when is_atom(s) do
    :erlang.atom_to_list(s)
  end

  defp to_string(s) when is_list(s) do
    s
  end

  defp get_names(epmdAddress) do
    case open(epmdAddress) do
      {:ok, socket} ->
        do_get_names(socket)

      _Error ->
        {:error, :address}
    end
  end

  defp do_get_names(socket) do
    case :gen_tcp.send(
           socket,
           [[1 >>> 8 &&& 255, 1 &&& 255], ?n]
         ) do
      :ok ->
        receive do
          {:tcp, ^socket, [[p0, p1, p2, p3] | t]} ->
            epmdPort = p0 <<< 24 ||| p1 <<< 16 ||| p2 <<< 8 ||| p3

            case get_epmd_port() do
              ^epmdPort ->
                names_loop(socket, t, [])

              _ ->
                close(socket)
                {:error, :address}
            end

          {:tcp_closed, ^socket} ->
            {:ok, []}
        end

      _ ->
        close(socket)
        {:error, :address}
    end
  end

  defp names_loop(socket, acc, ps) do
    receive do
      {:tcp, ^socket, bytes} ->
        {nAcc, nPs} = scan_names(acc ++ bytes, ps)
        names_loop(socket, nAcc, nPs)

      {:tcp_closed, ^socket} ->
        {_, nPs} = scan_names(acc, ps)
        {:ok, nPs}
    end
  end

  defp scan_names(buf, ps) do
    case scan_line(buf, []) do
      {line, nBuf} ->
        case parse_line(line) do
          {:ok, entry} ->
            scan_names(nBuf, [entry | ps])

          :error ->
            scan_names(nBuf, ps)
        end

      [] ->
        {buf, ps}
    end
  end

  defp scan_line([?\n | buf], line) do
    {reverse(line), buf}
  end

  defp scan_line([c | buf], line) do
    scan_line(buf, [c | line])
  end

  defp scan_line([], _) do
    []
  end

  defp parse_line('name ' ++ buf0) do
    case parse_name(buf0, []) do
      {name, buf1} ->
        case buf1 do
          'at port ' ++ buf2 ->
            case (try do
                    :erlang.list_to_integer(buf2)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:EXIT, _} ->
                :error

              port ->
                {:ok, {name, port}}
            end

          _ ->
            :error
        end

      :error ->
        :error
    end
  end

  defp parse_line(_) do
    :error
  end

  defp parse_name([?\s | buf], name) do
    {reverse(name), buf}
  end

  defp parse_name([c | buf], name) do
    parse_name(buf, [c | name])
  end

  defp parse_name([], _Name) do
    :error
  end
end
