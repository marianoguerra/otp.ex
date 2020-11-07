defmodule :m_gen_udp do
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

  def open(port) do
    open(port, [])
  end

  def open(port, opts0) do
    {mod, opts} = :inet.udp_module(opts0)
    {:ok, uP} = mod.getserv(port)
    mod.open(uP, opts)
  end

  def close(s) do
    :inet.udp_close(s)
  end

  def send(socket, destination, packet) do
    send(socket, destination, [], packet)
  end

  def send(s, {_, _} = destination, portZero = ancData, packet)
      when is_port(s) do
    cond do
      portZero === 0 ->
        case :inet_db.lookup_socket(s) do
          {:ok, mod} ->
            mod.send(s, destination, [], packet)

          error ->
            error
        end

      is_integer(portZero) ->
        {:error, :einval}

      is_list(ancData) ->
        case :inet_db.lookup_socket(s) do
          {:ok, mod} ->
            mod.send(s, destination, ancData, packet)

          error ->
            error
        end
    end
  end

  def send(s, host, port, packet) when is_port(s) do
    send(s, host, port, [], packet)
  end

  def send(s, host, port, ancData, packet)
      when is_port(s) and is_list(ancData) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        case mod.getaddr(host) do
          {:ok, iP} ->
            case mod.getserv(port) do
              {:ok, p} ->
                mod.send(s, {iP, p}, ancData, packet)

              {:error, :einval} ->
                exit(:badarg)

              error ->
                error
            end

          {:error, :einval} ->
            exit(:badarg)

          error ->
            error
        end

      error ->
        error
    end
  end

  def send(s, packet) when is_port(s) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.send(s, packet)

      error ->
        error
    end
  end

  def recv(s, len) when is_port(s) and is_integer(len) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.recv(s, len)

      error ->
        error
    end
  end

  def recv(s, len, time) when is_port(s) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.recv(s, len, time)

      error ->
        error
    end
  end

  def connect(s, address, port) when is_port(s) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        case mod.getaddr(address) do
          {:ok, iP} ->
            mod.connect(s, iP, port)

          error ->
            error
        end

      error ->
        error
    end
  end

  def controlling_process(s, newOwner) do
    :inet.udp_controlling_process(s, newOwner)
  end

  def fdopen(fd, opts0) do
    {mod, opts} = :inet.udp_module(opts0)
    mod.fdopen(fd, opts)
  end
end
