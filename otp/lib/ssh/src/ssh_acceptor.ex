defmodule :m_ssh_acceptor do
  use Bitwise
  require Record

  Record.defrecord(:r_ssh, :ssh,
    role: :undefined,
    peer: :undefined,
    local: :undefined,
    c_vsn: :undefined,
    s_vsn: :undefined,
    c_version: :undefined,
    s_version: :undefined,
    c_keyinit: :undefined,
    s_keyinit: :undefined,
    send_ext_info: :undefined,
    recv_ext_info: :undefined,
    algorithms: :undefined,
    send_mac: :none,
    send_mac_key: :undefined,
    send_mac_size: 0,
    recv_mac: :none,
    recv_mac_key: :undefined,
    recv_mac_size: 0,
    encrypt: :none,
    encrypt_cipher: :undefined,
    encrypt_keys: :undefined,
    encrypt_block_size: 8,
    encrypt_ctx: :undefined,
    decrypt: :none,
    decrypt_cipher: :undefined,
    decrypt_keys: :undefined,
    decrypt_block_size: 8,
    decrypt_ctx: :undefined,
    compress: :none,
    compress_ctx: :undefined,
    decompress: :none,
    decompress_ctx: :undefined,
    c_lng: :none,
    s_lng: :none,
    user_ack: true,
    timeout: :infinity,
    shared_secret: :undefined,
    exchanged_hash: :undefined,
    session_id: :undefined,
    opts: [],
    send_sequence: 0,
    recv_sequence: 0,
    keyex_key: :undefined,
    keyex_info: :undefined,
    random_length_padding: 15,
    user: :undefined,
    service: :undefined,
    userauth_quiet_mode: :undefined,
    userauth_methods: :undefined,
    userauth_supported_methods: :undefined,
    userauth_pubkeys: :undefined,
    kb_tries_left: 0,
    userauth_preference: :undefined,
    available_host_keys: :undefined,
    pwdfun_user_state: :undefined,
    authenticated: false
  )

  Record.defrecord(:r_alg, :alg,
    kex: :undefined,
    hkey: :undefined,
    send_mac: :undefined,
    recv_mac: :undefined,
    encrypt: :undefined,
    decrypt: :undefined,
    compress: :undefined,
    decompress: :undefined,
    c_lng: :undefined,
    s_lng: :undefined,
    send_ext_info: :undefined,
    recv_ext_info: :undefined
  )

  Record.defrecord(:r_ssh_pty, :ssh_pty,
    term: '',
    width: 80,
    height: 25,
    pixel_width: 1024,
    pixel_height: 768,
    modes: <<>>
  )

  Record.defrecord(:r_circ_buf_entry, :circ_buf_entry,
    module: :undefined,
    line: :undefined,
    function: :undefined,
    pid: self(),
    value: :undefined
  )

  @behaviour :ssh_dbg
  def start_link(port, address, options, acceptTimeout) do
    args = [self(), port, address, options, acceptTimeout]
    :proc_lib.start_link(:ssh_acceptor, :acceptor_init, args)
  end

  def number_of_connections(systemSup) do
    length(
      for {r, x, :supervisor, [:ssh_subsystem_sup]} <- :supervisor.which_children(systemSup),
          is_pid(x),
          is_reference(r) do
        x
      end
    )
  end

  def listen(port, options) do
    {_, callback, _} =
      :ssh_options.get_value(:user_options, :transport, options, :ssh_acceptor, 58)

    sockOpts = [
      [{:active, false}, {:reuseaddr, true}]
      | :ssh_options.get_value(:user_options, :socket_options, options, :ssh_acceptor, 59)
    ]

    case callback.listen(port, sockOpts) do
      {:error, :nxdomain} ->
        callback.listen(port, :lists.delete(:inet6, sockOpts))

      {:error, :enetunreach} ->
        callback.listen(port, :lists.delete(:inet6, sockOpts))

      {:error, :eafnosupport} ->
        callback.listen(port, :lists.delete(:inet6, sockOpts))

      other ->
        other
    end
  end

  def handle_established_connection(address, port, options, socket) do
    {_, callback, _} =
      :ssh_options.get_value(:user_options, :transport, options, :ssh_acceptor, 73)

    handle_connection(callback, address, port, options, socket)
  end

  def acceptor_init(parent, port, address, opts, acceptTimeout) do
    try do
      :ssh_options.get_value(:internal_options, :lsocket, opts, :ssh_acceptor, 81)
    catch
      _, _ ->
        {:error, :use_existing_socket_failed}
    else
      {lSock, sockOwner} ->
        case :inet.sockname(lSock) do
          {:ok, {_, ^port}} ->
            :proc_lib.init_ack(parent, {:ok, self()})
            request_ownership(lSock, sockOwner)

            {_, callback, _} =
              :ssh_options.get_value(:user_options, :transport, opts, :ssh_acceptor, 88)

            acceptor_loop(callback, port, address, opts, lSock, acceptTimeout)

          {:error, _} ->
            {:ok, newLSock} = try_listen(port, opts, 4)
            :proc_lib.init_ack(parent, {:ok, self()})
            opts1 = :ssh_options.delete_key(:internal_options, :lsocket, opts, :ssh_acceptor, 95)

            {_, callback, _} =
              :ssh_options.get_value(:user_options, :transport, opts1, :ssh_acceptor, 96)

            acceptor_loop(callback, port, address, opts1, newLSock, acceptTimeout)
        end
    end
  end

  defp try_listen(port, opts, ntriesLeft) do
    try_listen(port, opts, 1, ntriesLeft)
  end

  defp try_listen(port, opts, n, nmax) do
    case listen(port, opts) do
      {:error, :eaddrinuse} when n < nmax ->
        :timer.sleep(10 * n)
        try_listen(port, opts, n + 1, nmax)

      other ->
        other
    end
  end

  defp request_ownership(lSock, sockOwner) do
    send(sockOwner, {:request_control, lSock, self()})

    receive do
      {:its_yours, ^lSock} ->
        :ok
    end
  end

  def acceptor_loop(callback, port, address, opts, listenSocket, acceptTimeout) do
    case (try do
            callback.accept(listenSocket, acceptTimeout)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, socket} ->
        handle_connection(callback, address, port, opts, socket)
        :ssh_acceptor.acceptor_loop(callback, port, address, opts, listenSocket, acceptTimeout)

      {:error, reason} ->
        handle_error(reason)
        :ssh_acceptor.acceptor_loop(callback, port, address, opts, listenSocket, acceptTimeout)

      {:EXIT, reason} ->
        handle_error(reason)
        :ssh_acceptor.acceptor_loop(callback, port, address, opts, listenSocket, acceptTimeout)
    end
  end

  defp handle_connection(callback, address, port, options, socket) do
    profile = :ssh_options.get_value(:user_options, :profile, options, :ssh_acceptor, 143)
    systemSup = :ssh_system_sup.system_supervisor(address, port, profile)

    maxSessions =
      :ssh_options.get_value(:user_options, :max_sessions, options, :ssh_acceptor, 146)

    case number_of_connections(systemSup) < maxSessions do
      true ->
        {:ok, subSysSup} =
          :ssh_system_sup.start_subsystem(systemSup, :server, address, port, profile, options)

        connectionSup = :ssh_subsystem_sup.connection_supervisor(subSysSup)

        negTimeout =
          :ssh_options.get_value(:user_options, :negotiation_timeout, options, :ssh_acceptor, 152)

        :ssh_connection_handler.start_connection(
          :server,
          socket,
          :ssh_options.put_value(
            :internal_options,
            {:supervisors,
             [
               {:system_sup, systemSup},
               {:subsystem_sup, subSysSup},
               {:connection_sup, connectionSup}
             ]},
            options,
            :ssh_acceptor,
            158
          ),
          negTimeout
        )

      false ->
        callback.close(socket)

        iPstr =
          cond do
            is_tuple(address) ->
              :inet.ntoa(address)

            true ->
              address
          end

        str =
          try do
            :io_lib.format(:"~s:~p", [iPstr, port])
          catch
            _, _ ->
              'port ' ++ :erlang.integer_to_list(port)
          end

        :error_logger.info_report(
          'Ssh login attempt to ' ++
            str ++
            ' denied due to option max_sessions limits to ' ++
            :io_lib.write(maxSessions) ++ ' sessions.'
        )

        {:error, :max_sessions}
    end
  end

  defp handle_error(:timeout) do
    :ok
  end

  defp handle_error(:enfile) do
    :timer.sleep(200)
  end

  defp handle_error(:emfile) do
    :timer.sleep(200)
  end

  defp handle_error(:closed) do
    :error_logger.info_report(
      'The ssh accept socket was closed by a third party. This will not have an impact on ssh that will open a new accept socket and go on as nothing happened. It does however indicate that some other software is behaving badly.'
    )

    exit(:normal)
  end

  defp handle_error(reason) do
    string = :lists.flatten(:io_lib.format('Accept error: ~p', [reason]))
    :error_logger.error_report(string)
    exit({:accept_failed, string})
  end

  def ssh_dbg_trace_points() do
    [:connections]
  end

  def ssh_dbg_flags(:connections) do
    [:c]
  end

  def ssh_dbg_on(:connections) do
    :dbg.tp(:ssh_acceptor, :acceptor_init, 5, :x)
    :dbg.tpl(:ssh_acceptor, :handle_connection, 5, :x)
  end

  def ssh_dbg_off(:connections) do
    :dbg.ctp(:ssh_acceptor, :acceptor_init, 5)
    :dbg.ctp(:ssh_acceptor, :handle_connection, 5)
  end

  def ssh_dbg_format(
        :connections,
        {:call, {:ssh_acceptor, :acceptor_init, [_Parent, port, address, _Opts, _AcceptTimeout]}}
      ) do
    [:io_lib.format('Starting LISTENER on ~s:~p\n', [ntoa(address), port])]
  end

  def ssh_dbg_format(
        :connections,
        {:return_from, {:ssh_acceptor, :acceptor_init, 5}, _Ret}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :connections,
        {:call, {:ssh_acceptor, :handle_connection, [_, _, _, _, _]}}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :connections,
        {:return_from, {:ssh_acceptor, :handle_connection, 5}, {:error, error}}
      ) do
    ['Starting connection to server failed:\n', :io_lib.format('Error = ~p', [error])]
  end

  defp ntoa(a) do
    try do
      :inet.ntoa(a)
    catch
      _, _ when is_list(a) ->
        a

      _, _ ->
        :io_lib.format(:"~p", [a])
    end
  end
end
