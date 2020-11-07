defmodule :m_local_tcp do
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

  def getserv(0) do
    {:ok, 0}
  end

  def getaddr({:local, _} = address) do
    {:ok, address}
  end

  def getaddr({:local, _} = address, _Timer) do
    {:ok, address}
  end

  def getaddrs({:local, _} = address) do
    {:ok, [address]}
  end

  def getaddrs({:local, _} = address, _Timer) do
    {:ok, [address]}
  end

  def translate_ip(iP) do
    iP
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

  defp do_connect(addr = {:local, _}, 0, opts, time) do
    case :inet.connect_options(opts, :local_tcp) do
      {:error, reason} ->
        exit(reason)

      {:ok, r_connect_opts(fd: fd, ifaddr: bAddr, port: 0, opts: sockOpts)}
      when (tuple_size(bAddr) === 2 and
              :erlang.element(1, bAddr) === :local) or
             bAddr === :any ->
        case :inet.open(
               fd,
               case bAddr do
                 :any ->
                   :undefined

                 _ ->
                   bAddr
               end,
               0,
               sockOpts,
               :tcp,
               :local,
               :stream,
               :local_tcp
             ) do
          {:ok, s} ->
            case :prim_inet.connect(s, addr, 0, time) do
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

  def listen(0, opts) do
    case :inet.listen_options(
           [{:port, 0} | opts],
           :local_tcp
         ) do
      {:error, reason} ->
        exit(reason)

      {:ok, r_listen_opts(fd: fd, ifaddr: bAddr, port: 0, opts: sockOpts) = r}
      when (tuple_size(bAddr) === 2 and
              :erlang.element(1, bAddr) === :local) or
             bAddr === :any ->
        case :inet.open(fd, bAddr, 0, sockOpts, :tcp, :local, :stream, :local_tcp) do
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
    case :prim_inet.accept(l) do
      {:ok, s} ->
        :inet_db.register_socket(s, :local_tcp)
        {:ok, s}

      error ->
        error
    end
  end

  def accept(l, timeout) do
    case :prim_inet.accept(l, timeout) do
      {:ok, s} ->
        :inet_db.register_socket(s, :local_tcp)
        {:ok, s}

      error ->
        error
    end
  end

  def fdopen(fd, opts) do
    :inet.open(fd, :undefined, 0, opts, :tcp, :local, :stream, :local_tcp)
  end
end
