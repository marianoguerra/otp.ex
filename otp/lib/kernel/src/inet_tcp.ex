defmodule :m_inet_tcp do
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

  def family() do
    :inet
  end

  def mask({m1, m2, m3, m4}, {iP1, iP2, iP3, iP4}) do
    {m1 &&& iP1, m2 &&& iP2, m3 &&& iP3, m4 &&& iP4}
  end

  def parse_address(host) do
    :inet_parse.ipv4strict_address(host)
  end

  def getserv(port) when is_integer(port) do
    {:ok, port}
  end

  def getserv(name) when is_atom(name) do
    :inet.getservbyname(name, :tcp)
  end

  def getaddr(address) do
    :inet.getaddr(address, :inet)
  end

  def getaddr(address, timer) do
    :inet.getaddr_tm(address, :inet, timer)
  end

  def getaddrs(address) do
    :inet.getaddrs(address, :inet)
  end

  def getaddrs(address, timer) do
    :inet.getaddrs_tm(address, :inet, timer)
  end

  def translate_ip(iP) do
    :inet.translate_ip(iP, :inet)
  end

  def send(socket, packet, opts) do
    :prim_inet.send(socket, packet, opts)
  end

  def send(socket, packet) do
    :prim_inet.send(socket, packet, [])
  end

  def recv(socket, length) do
    :prim_inet.recv(socket, length)
  end

  def recv(socket, length, timeout) do
    :prim_inet.recv(socket, length, timeout)
  end

  def unrecv(socket, data) do
    :prim_inet.unrecv(socket, data)
  end

  def shutdown(socket, how) do
    :prim_inet.shutdown(socket, how)
  end

  def close(socket) do
    :inet.tcp_close(socket)
  end

  def controlling_process(socket, newOwner) do
    :inet.tcp_controlling_process(socket, newOwner)
  end

  def connect(address, port, opts) do
    do_connect(address, port, opts, :infinity)
  end

  def connect(address, port, opts, :infinity) do
    do_connect(address, port, opts, :infinity)
  end

  def connect(address, port, opts, timeout)
      when is_integer(timeout) and timeout >= 0 do
    do_connect(address, port, opts, timeout)
  end

  defp do_connect(addr = {a, b, c, d}, port, opts, time)
       when (a ||| b ||| c ||| d) &&& ~~~255 === 0 and
              port &&& ~~~65535 === 0 do
    case :inet.connect_options(opts, :inet_tcp) do
      {:error, reason} ->
        exit(reason)

      {:ok, r_connect_opts(fd: fd, ifaddr: bAddr = {ab, bb, cb, db}, port: bPort, opts: sockOpts)}
      when (ab ||| bb ||| cb ||| db) &&& ~~~255 === 0 and
             bPort &&& ~~~65535 === 0 ->
        case :inet.open(fd, bAddr, bPort, sockOpts, :tcp, :inet, :stream, :inet_tcp) do
          {:ok, s} ->
            case :prim_inet.connect(s, addr, port, time) do
              :ok ->
                {:ok, s}

              error ->
                :prim_inet.close(s)
                error
            end

          error ->
            error
        end

      {:ok, _} ->
        exit(:badarg)
    end
  end

  def listen(port, opts) do
    case :inet.listen_options(
           [{:port, port} | opts],
           :inet_tcp
         ) do
      {:error, reason} ->
        exit(reason)

      {:ok, r_listen_opts(fd: fd, ifaddr: bAddr = {a, b, c, d}, port: bPort, opts: sockOpts) = r}
      when (a ||| b ||| c ||| d) &&& ~~~255 === 0 and
             bPort &&& ~~~65535 === 0 ->
        case :inet.open(fd, bAddr, bPort, sockOpts, :tcp, :inet, :stream, :inet_tcp) do
          {:ok, s} ->
            case :prim_inet.listen(s, r_listen_opts(r, :backlog)) do
              :ok ->
                {:ok, s}

              error ->
                :prim_inet.close(s)
                error
            end

          error ->
            error
        end

      {:ok, _} ->
        exit(:badarg)
    end
  end

  def accept(l) do
    case :prim_inet.accept(l, accept_family_opts()) do
      {:ok, s} ->
        :inet_db.register_socket(s, :inet_tcp)
        {:ok, s}

      error ->
        error
    end
  end

  def accept(l, timeout) do
    case :prim_inet.accept(l, timeout, accept_family_opts()) do
      {:ok, s} ->
        :inet_db.register_socket(s, :inet_tcp)
        {:ok, s}

      error ->
        error
    end
  end

  defp accept_family_opts() do
    [:tos, :ttl, :recvtos, :recvttl]
  end

  def fdopen(fd, opts) do
    :inet.fdopen(fd, opts, :tcp, :inet, :stream, :inet_tcp)
  end
end
