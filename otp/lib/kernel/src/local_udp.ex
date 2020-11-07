defmodule :m_local_udp do
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

  def translate_ip(iP) do
    iP
  end

  def open(0) do
    open(0, [])
  end

  def open(0, opts) do
    case :inet.udp_options(
           [{:port, 0} | opts],
           :local_udp
         ) do
      {:error, reason} ->
        exit(reason)

      {:ok, r_udp_opts(fd: fd, ifaddr: bAddr, port: 0, opts: sockOpts)}
      when (tuple_size(bAddr) === 2 and
              :erlang.element(1, bAddr) === :local) or
             bAddr === :any ->
        :inet.open(
          fd,
          case bAddr do
            :any ->
              :undefined

            _ ->
              bAddr
          end,
          0,
          sockOpts,
          :udp,
          :local,
          :dgram,
          :local_udp
        )

      {:ok, _} ->
        exit(:badarg)
    end
  end

  def send(s, {:local, _} = addr, 0, data) do
    :prim_inet.sendto(s, addr, [], data)
  end

  def send(s, {:local, _} = addr, ancData, data)
      when is_list(ancData) do
    :prim_inet.sendto(s, addr, ancData, data)
  end

  def send(s, data) do
    :prim_inet.sendto(s, {:local, <<>>}, [], data)
  end

  def connect(s, addr = {:local, _}, 0) do
    :prim_inet.connect(s, addr, 0)
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
    :inet.fdopen(fd, opts, :udp, :local, :dgram, :local_udp)
  end
end
