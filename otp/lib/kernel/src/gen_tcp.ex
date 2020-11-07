defmodule :m_gen_tcp do
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

  Record.defrecord(:r_file_info, :file_info,
    size: :undefined,
    type: :undefined,
    access: :undefined,
    atime: :undefined,
    mtime: :undefined,
    ctime: :undefined,
    mode: :undefined,
    links: :undefined,
    major_device: :undefined,
    minor_device: :undefined,
    inode: :undefined,
    uid: :undefined,
    gid: :undefined
  )

  Record.defrecord(:r_file_descriptor, :file_descriptor,
    module: :undefined,
    data: :undefined
  )

  def connect(address, port, opts) do
    connect(address, port, opts, :infinity)
  end

  def connect(address, port, opts0, time) do
    case :inet.gen_tcp_module(opts0) do
      {:gen_tcp, opts} ->
        timer = :inet.start_timer(time)

        res =
          try do
            connect1(address, port, opts, timer)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        _ = :inet.stop_timer(timer)

        case res do
          {:ok, s} ->
            {:ok, s}

          {:error, :einval} ->
            exit(:badarg)

          {:EXIT, reason} ->
            exit(reason)

          error ->
            error
        end

      {genTcpMod, opts} ->
        genTcpMod.connect(address, port, opts, time)
    end
  end

  defp connect1(address, port, opts0, timer) do
    {mod, opts} = :inet.tcp_module(opts0, address)

    case mod.getaddrs(address, timer) do
      {:ok, iPs} ->
        case mod.getserv(port) do
          {:ok, tP} ->
            try_connect(iPs, tP, opts, timer, mod, {:error, :einval})

          error ->
            error
        end

      error ->
        error
    end
  end

  defp try_connect([iP | iPs], port, opts, timer, mod, _) do
    time = :inet.timeout(timer)

    case mod.connect(iP, port, opts, time) do
      {:ok, s} ->
        {:ok, s}

      {:error, :einval} ->
        {:error, :einval}

      {:error, :timeout} ->
        {:error, :timeout}

      err1 ->
        try_connect(iPs, port, opts, timer, mod, err1)
    end
  end

  defp try_connect([], _Port, _Opts, _Timer, _Mod, err) do
    err
  end

  def listen(port, opts0) do
    case :inet.gen_tcp_module(opts0) do
      {:gen_tcp, opts1} ->
        {mod, opts} = :inet.tcp_module(opts1)

        case mod.getserv(port) do
          {:ok, tP} ->
            mod.listen(tP, opts)

          {:error, :einval} ->
            exit(:badarg)

          other ->
            other
        end

      {genTcpMod, opts} ->
        genTcpMod.listen(port, opts)
    end
  end

  def accept({:"$inet", genTcpMod, _} = s)
      when is_atom(genTcpMod) do
    genTcpMod.accept(s, :infinity)
  end

  def accept(s) when is_port(s) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.accept(s)

      error ->
        error
    end
  end

  def accept({:"$inet", genTcpMod, _} = s, time)
      when is_atom(genTcpMod) do
    genTcpMod.accept(s, time)
  end

  def accept(s, time) when is_port(s) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.accept(s, time)

      error ->
        error
    end
  end

  def shutdown({:"$inet", genTcpMod, _} = s, how)
      when is_atom(genTcpMod) do
    genTcpMod.shutdown(s, how)
  end

  def shutdown(s, how) when is_port(s) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.shutdown(s, how)

      error ->
        error
    end
  end

  def close({:"$inet", genTcpMod, _} = s)
      when is_atom(genTcpMod) do
    genTcpMod.close(s)
  end

  def close(s) do
    :inet.tcp_close(s)
  end

  def send({:"$inet", genTcpMod, _} = s, packet)
      when is_atom(genTcpMod) do
    genTcpMod.send(s, packet)
  end

  def send(s, packet) when is_port(s) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.send(s, packet)

      error ->
        error
    end
  end

  def recv({:"$inet", genTcpMod, _} = s, length)
      when is_atom(genTcpMod) do
    genTcpMod.recv(s, length, :infinity)
  end

  def recv(s, length) when is_port(s) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.recv(s, length)

      error ->
        error
    end
  end

  def recv({:"$inet", genTcpMod, _} = s, length, time)
      when is_atom(genTcpMod) do
    genTcpMod.recv(s, length, time)
  end

  def recv(s, length, time) when is_port(s) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.recv(s, length, time)

      error ->
        error
    end
  end

  def unrecv({:"$inet", genTcpMod, _} = s, data)
      when is_atom(genTcpMod) do
    genTcpMod.unrecv(s, data)
  end

  def unrecv(s, data) when is_port(s) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.unrecv(s, data)

      error ->
        error
    end
  end

  def controlling_process({:"$inet", genTcpMod, _} = s, newOwner)
      when is_atom(genTcpMod) do
    genTcpMod.controlling_process(s, newOwner)
  end

  def controlling_process(s, newOwner) do
    case :inet_db.lookup_socket(s) do
      {:ok, _Mod} ->
        :inet.tcp_controlling_process(s, newOwner)

      error ->
        error
    end
  end

  def fdopen(fd, opts0) do
    case :inet.gen_tcp_module(opts0) do
      {:gen_tcp, opts1} ->
        {mod, opts} = :inet.tcp_module(opts1)
        mod.fdopen(fd, opts)

      {genTcpMod, opts} ->
        genTcpMod.fdopen(fd, opts)
    end
  end
end
