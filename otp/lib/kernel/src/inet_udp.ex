defmodule :m_inet_udp do
  use Bitwise
  import Kernel, except: [send: 2]
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

  def getserv(port) when is_integer(port) do
    {:ok, port}
  end

  def getserv(name) when is_atom(name) do
    :inet.getservbyname(name, :udp)
  end

  def getaddr(address) do
    :inet.getaddr(address, :inet)
  end

  def getaddr(address, timer) do
    :inet.getaddr(address, :inet, timer)
  end

  def translate_ip(iP) do
    :inet.translate_ip(iP, :inet)
  end

  def open(port) do
    open(port, [])
  end

  def open(port, opts) do
    case :inet.udp_options(
           [
             [{:port, port}, {:recbuf, 8 * 1024}]
             | opts
           ],
           :inet_udp
         ) do
      {:error, reason} ->
        exit(reason)

      {:ok, r_udp_opts(fd: fd, ifaddr: bAddr = {a, b, c, d}, port: bPort, opts: sockOpts)}
      when (a ||| b ||| c ||| d) &&& ~~~255 === 0 and
             bPort &&& ~~~65535 === 0 ->
        :inet.open(fd, bAddr, bPort, sockOpts, :udp, :inet, :dgram, :inet_udp)

      {:ok, _} ->
        exit(:badarg)
    end
  end

  def send(s, {a, b, c, d} = iP, port, data)
      when (a ||| b ||| c ||| d) &&& ~~~255 === 0 and
             port &&& ~~~65535 === 0 do
    :prim_inet.sendto(s, {iP, port}, [], data)
  end

  def send(s, {{a, b, c, d}, port} = addr, ancData, data)
      when (a ||| b ||| c ||| d) &&& ~~~255 === 0 and
             port &&& ~~~65535 === 0 and is_list(ancData) do
    :prim_inet.sendto(s, addr, ancData, data)
  end

  def send(s, {:inet, {{a, b, c, d}, port}} = address, ancData, data)
      when (a ||| b ||| c ||| d) &&& ~~~255 === 0 and
             port &&& ~~~65535 === 0 and is_list(ancData) do
    :prim_inet.sendto(s, address, ancData, data)
  end

  def send(s, {:inet, {:loopback, port}} = address, ancData, data)
      when port &&& ~~~65535 === 0 and is_list(ancData) do
    :prim_inet.sendto(s, address, ancData, data)
  end

  def send(s, data) do
    :prim_inet.sendto(s, {:any, 0}, [], data)
  end

  def connect(s, addr = {a, b, c, d}, port)
      when (a ||| b ||| c ||| d) &&& ~~~255 === 0 and
             port &&& ~~~65535 === 0 do
    :prim_inet.connect(s, addr, port)
  end

  def recv(s, len) do
    :prim_inet.recvfrom(s, len)
  end

  def recv(s, len, time) do
    :prim_inet.recvfrom(s, len, time)
  end

  def close(s) do
    :inet.udp_close(s)
  end

  def controlling_process(socket, newOwner) do
    :inet.udp_controlling_process(socket, newOwner)
  end

  def fdopen(fd, opts) do
    :inet.fdopen(fd, optuniquify([{:recbuf, 8 * 1024} | opts]), :udp, :inet, :dgram, :inet_udp)
  end

  defp optuniquify(list) do
    optuniquify(:lists.reverse(list), [])
  end

  defp optuniquify([], result) do
    result
  end

  defp optuniquify([opt | tail], result) do
    optuniquify(opt, tail, [], result)
  end

  defp optuniquify(opt, [], rest, result) do
    optuniquify(:lists.reverse(rest), [opt | result])
  end

  defp optuniquify(opt0, [opt1 | tail], rest, result)
       when tuple_size(opt0) === tuple_size(opt1) and
              :erlang.element(1, opt0) ===
                :erlang.element(
                  1,
                  opt1
                ) do
    optuniquify(opt0, tail, rest, result)
  end

  defp optuniquify(opt, [opt | tail], rest, result) do
    optuniquify(opt, tail, rest, result)
  end

  defp optuniquify(opt, [x | tail], rest, result) do
    optuniquify(opt, tail, [x | rest], result)
  end
end
