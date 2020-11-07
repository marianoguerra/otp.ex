defmodule :m_ftp do
  use Bitwise
  import Kernel, except: [send: 2]
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_state, :state,
    csock: :undefined,
    dsock: :undefined,
    tls_options: :undefined,
    verbose: false,
    ldir: :undefined,
    type: :ftp_server_default,
    chunk: false,
    mode: :passive,
    timeout: 60 * 1000,
    data: <<>>,
    ctrl_data: {<<>>, [], :start},
    latest_ctrl_response: '',
    owner: :undefined,
    client: :undefined,
    caller: :undefined,
    ipfamily: :undefined,
    sockopts_ctrl: [],
    sockopts_data_passive: [],
    sockopts_data_active: [],
    progress: :ignore,
    dtimeout: :infinity,
    tls_upgrading_data_connection: false,
    ftp_extension: false
  )

  Record.defrecord(:r_recv_chunk_closing, :recv_chunk_closing,
    dconn_closed: false,
    pos_compl_received: false,
    client_called_us: false
  )

  def start() do
    :application.start(:ftp)
  end

  def start_standalone(options) do
    try do
      {:ok, startOptions} = start_options(options)
      {:ok, openOptions} = open_options(options)
      {:ok, socketOptions} = socket_options(options)

      case start_link(startOptions, []) do
        {:ok, pid} ->
          call(pid, {:open, :ip_comm, openOptions, socketOptions}, :plain)

        error1 ->
          error1
      end
    catch
      error2 ->
        error2
    end
  end

  def start_service(options) do
    try do
      {:ok, startOptions} = start_options(options)
      {:ok, openOptions} = open_options(options)
      {:ok, socketOptions} = socket_options(options)

      case :ftp_sup.start_child([
             [
               [
                 {:client, self()}
                 | startOptions
               ],
               []
             ]
           ]) do
        {:ok, pid} ->
          call(pid, {:open, :ip_comm, openOptions, socketOptions}, :plain)

        error1 ->
          error1
      end
    catch
      error2 ->
        error2
    end
  end

  def stop() do
    :application.stop(:ftp)
  end

  def stop_service(pid) do
    close(pid)
  end

  def services() do
    for {_, pid, _, _} <- :supervisor.which_children(:ftp_sup) do
      {:ftpc, pid}
    end
  end

  def service_info(pid) do
    {:ok, info} = call(pid, :info, :list)

    {:ok,
     [
       :proplists.lookup(:mode, info),
       :proplists.lookup(:local_port, info),
       :proplists.lookup(:peer, info),
       :proplists.lookup(:peer_port, info)
     ]}
  end

  def open({:option_list, options}) when is_list(options) do
    try do
      {:ok, startOptions} = start_options(options)
      {:ok, openOptions} = open_options(options)
      {:ok, sockOpts} = socket_options(options)

      case :ftp_sup.start_child([
             [
               [
                 {:client, self()}
                 | startOptions
               ],
               []
             ]
           ]) do
        {:ok, pid} ->
          call(pid, {:open, :ip_comm, openOptions, sockOpts}, :plain)

        error1 ->
          error1
      end
    catch
      error2 ->
        error2
    end
  end

  def open(host) do
    open(host, [])
  end

  def open(host, port) when is_integer(port) do
    open(host, [{:port, port}])
  end

  def open(host, opts) when is_list(opts) do
    try do
      {:ok, startOptions} = start_options(opts)

      {:ok, openOptions} =
        open_options([
          {:host, host}
          | opts
        ])

      {:ok, socketOptions} = socket_options(opts)

      case start_link(startOptions, []) do
        {:ok, pid} ->
          do_open(pid, openOptions, socketOptions, tls_options(opts))

        error1 ->
          error1
      end
    catch
      error2 ->
        error2
    end
  end

  defp do_open(pid, openOptions, socketOptions, tLSOpts) do
    case call(
           pid,
           {:open, :ip_comm, openOptions, socketOptions},
           :plain
         ) do
      {:ok, ^pid} ->
        maybe_tls_upgrade(pid, tLSOpts)

      error ->
        error
    end
  end

  def user(pid, user, pass) do
    case {is_name_sane(user), is_name_sane(pass)} do
      {true, true} ->
        call(pid, {:user, user, pass}, :atom)

      _ ->
        {:error, :euser}
    end
  end

  def user(pid, user, pass, acc) do
    case {is_name_sane(user), is_name_sane(pass), is_name_sane(acc)} do
      {true, true, true} ->
        call(pid, {:user, user, pass, acc}, :atom)

      _ ->
        {:error, :euser}
    end
  end

  def account(pid, acc) do
    case is_name_sane(acc) do
      true ->
        call(pid, {:account, acc}, :atom)

      _ ->
        {:error, :eacct}
    end
  end

  def pwd(pid) do
    call(pid, :pwd, :ctrl)
  end

  def lpwd(pid) do
    call(pid, :lpwd, :string)
  end

  def cd(pid, dir) do
    case is_name_sane(dir) do
      true ->
        call(pid, {:cd, dir}, :atom)

      _ ->
        {:error, :efnamena}
    end
  end

  def lcd(pid, dir) do
    call(pid, {:lcd, dir}, :string)
  end

  def ls(pid) do
    ls(pid, '')
  end

  def ls(pid, dir) do
    case is_name_sane(dir) do
      true ->
        call(pid, {:dir, :long, dir}, :string)

      _ ->
        {:error, :efnamena}
    end
  end

  def nlist(pid) do
    nlist(pid, '')
  end

  def nlist(pid, dir) do
    case is_name_sane(dir) do
      true ->
        call(pid, {:dir, :short, dir}, :string)

      _ ->
        {:error, :efnamena}
    end
  end

  def rename(pid, old, new) do
    case {is_name_sane(old), is_name_sane(new)} do
      {true, true} ->
        call(pid, {:rename, old, new}, :string)

      _ ->
        {:error, :efnamena}
    end
  end

  def delete(pid, file) do
    case is_name_sane(file) do
      true ->
        call(pid, {:delete, file}, :string)

      _ ->
        {:error, :efnamena}
    end
  end

  def mkdir(pid, dir) do
    case is_name_sane(dir) do
      true ->
        call(pid, {:mkdir, dir}, :atom)

      _ ->
        {:error, :efnamena}
    end
  end

  def rmdir(pid, dir) do
    case is_name_sane(dir) do
      true ->
        call(pid, {:rmdir, dir}, :atom)

      _ ->
        {:error, :efnamena}
    end
  end

  def type(pid, type) do
    call(pid, {:type, type}, :atom)
  end

  def recv(pid, remotFileName) do
    recv(pid, remotFileName, remotFileName)
  end

  def recv(pid, remotFileName, localFileName) do
    case is_name_sane(remotFileName) do
      true ->
        call(pid, {:recv, remotFileName, localFileName}, :atom)

      _ ->
        {:error, :efnamena}
    end
  end

  def recv_bin(pid, remoteFile) do
    case is_name_sane(remoteFile) do
      true ->
        call(pid, {:recv_bin, remoteFile}, :bin)

      _ ->
        {:error, :efnamena}
    end
  end

  def recv_chunk_start(pid, remoteFile) do
    case is_name_sane(remoteFile) do
      true ->
        call(pid, {:recv_chunk_start, remoteFile}, :atom)

      _ ->
        {:error, :efnamena}
    end
  end

  def recv_chunk(pid) do
    call(pid, :recv_chunk, :atom)
  end

  def send(pid, localFileName) do
    send(pid, localFileName, localFileName)
  end

  def send(pid, localFileName, remotFileName) do
    case is_name_sane(remotFileName) do
      true ->
        call(pid, {:send, localFileName, remotFileName}, :atom)

      _ ->
        {:error, :efnamena}
    end
  end

  def send_bin(pid, bin, remoteFile) when is_binary(bin) do
    case is_name_sane(remoteFile) do
      true ->
        call(pid, {:send_bin, bin, remoteFile}, :atom)

      _ ->
        {:error, :efnamena}
    end
  end

  def send_bin(_Pid, _Bin, _RemoteFile) do
    {:error, :enotbinary}
  end

  def send_chunk_start(pid, remoteFile) do
    case is_name_sane(remoteFile) do
      true ->
        call(pid, {:send_chunk_start, remoteFile}, :atom)

      _ ->
        {:error, :efnamena}
    end
  end

  def append_chunk_start(pid, remoteFile) do
    case is_name_sane(remoteFile) do
      true ->
        call(pid, {:append_chunk_start, remoteFile}, :atom)

      _ ->
        {:error, :efnamena}
    end
  end

  def send_chunk(pid, bin) when is_binary(bin) do
    call(pid, {:transfer_chunk, bin}, :atom)
  end

  def send_chunk(_Pid, _Bin) do
    {:error, :enotbinary}
  end

  def append_chunk(pid, bin) when is_binary(bin) do
    call(pid, {:transfer_chunk, bin}, :atom)
  end

  def append_chunk(_Pid, _Bin) do
    {:error, :enotbinary}
  end

  def send_chunk_end(pid) do
    call(pid, :chunk_end, :atom)
  end

  def append_chunk_end(pid) do
    call(pid, :chunk_end, :atom)
  end

  def append(pid, localFileName) do
    append(pid, localFileName, localFileName)
  end

  def append(pid, localFileName, remotFileName) do
    case is_name_sane(remotFileName) do
      true ->
        call(pid, {:append, localFileName, remotFileName}, :atom)

      _ ->
        {:error, :efnamena}
    end
  end

  def append_bin(pid, bin, remoteFile) when is_binary(bin) do
    case is_name_sane(remoteFile) do
      true ->
        call(pid, {:append_bin, bin, remoteFile}, :atom)

      _ ->
        {:error, :efnamena}
    end
  end

  def append_bin(_Pid, _Bin, _RemoteFile) do
    {:error, :enotbinary}
  end

  def quote(pid, cmd) when is_list(cmd) do
    call(pid, {:quote, cmd}, :atom)
  end

  def close(pid) do
    cast(pid, :close)
    :ok
  end

  def formaterror(tag) do
    :ftp_response.error_string(tag)
  end

  def info(pid) do
    call(pid, :info, :list)
  end

  def latest_ctrl_response(pid) do
    call(pid, :latest_ctrl_response, :string)
  end

  def init(options) do
    :erlang.process_flag(:trap_exit, true)
    {:value, {:client, client}} = :lists.keysearch(:client, 1, options)
    :erlang.monitor(:process, client)
    _ = :inet_db.start()
    {:ok, dir} = :file.get_cwd()

    case key_search(:debug, options, :disable) do
      :trace ->
        :dbg.tracer()
        :dbg.p(:all, [:call])
        {:ok, _} = :dbg.tpl(:ftp, [{:_, [], [{:return_trace}]}])

        {:ok, _} =
          :dbg.tpl(
            :ftp_response,
            [{:_, [], [{:return_trace}]}]
          )

        {:ok, _} =
          :dbg.tpl(
            :ftp_progress,
            [{:_, [], [{:return_trace}]}]
          )

        :ok

      :debug ->
        :dbg.tracer()
        :dbg.p(:all, [:call])
        {:ok, _} = :dbg.tp(:ftp, [{:_, [], [{:return_trace}]}])

        {:ok, _} =
          :dbg.tp(
            :ftp_response,
            [{:_, [], [{:return_trace}]}]
          )

        {:ok, _} =
          :dbg.tp(
            :ftp_progress,
            [{:_, [], [{:return_trace}]}]
          )

        :ok

      _ ->
        :ok
    end

    verbose = key_search(:verbose, options, false)
    ipFamily = key_search(:ipfamily, options, :inet)
    state = r_state(owner: client, verbose: verbose, ipfamily: ipFamily, ldir: dir)
    priority = key_search(:priority, options, :low)
    :erlang.process_flag(:priority, priority)
    {:ok, state}
  end

  def handle_call(
        {_, :info},
        _,
        r_state(
          verbose: verbose,
          mode: mode,
          timeout: timeout,
          ipfamily: ipFamily,
          csock: socket,
          progress: progress
        ) = state
      ) do
    {:ok, {_, localPort}} = sockname(socket)
    {:ok, {address, port}} = peername(socket)

    options = [
      {:verbose, verbose},
      {:ipfamily, ipFamily},
      {:mode, mode},
      {:peer, address},
      {:peer_port, port},
      {:local_port, localPort},
      {:timeout, timeout},
      {:progress, progress}
    ]

    {:reply, {:ok, options}, state}
  end

  def handle_call({_, :latest_ctrl_response}, _, r_state(latest_ctrl_response: resp) = state) do
    {:reply, {:ok, resp}, state}
  end

  def handle_call({pid, _}, _, r_state(owner: owner) = state)
      when owner !== pid do
    {:reply, {:error, :not_connection_owner}, state}
  end

  def handle_call(
        {_, {:open, :ip_comm, opts, {ctrlOpts, dataPassOpts, dataActOpts}}},
        from,
        state
      ) do
    case key_search(:host, opts, :undefined) do
      :undefined ->
        {:stop, :normal, {:error, :ehost}, state}

      host ->
        mode = key_search(:mode, opts, :passive)
        port = key_search(:port, opts, 21)
        timeout = key_search(:timeout, opts, 60 * 1000)
        dTimeout = key_search(:dtimeout, opts, :infinity)
        progress = key_search(:progress, opts, :ignore)
        ipFamily = key_search(:ipfamily, opts, :inet)
        ftpExt = key_search(:ftp_extension, opts, false)

        state2 =
          r_state(state,
            client: from,
            mode: mode,
            progress: progress(progress),
            ipfamily: ipFamily,
            sockopts_ctrl: ctrlOpts,
            sockopts_data_passive: dataPassOpts,
            sockopts_data_active: dataActOpts,
            dtimeout: dTimeout,
            ftp_extension: ftpExt
          )

        case setup_ctrl_connection(host, port, timeout, state2) do
          {:ok, state3, waitTimeout} ->
            {:noreply, state3, waitTimeout}

          {:error, _Reason} ->
            :gen_server.reply(from, {:error, :ehost})
            {:stop, :normal, r_state(state2, client: :undefined)}
        end
    end
  end

  def handle_call({_, {:open, :tls_upgrade, tLSOptions}}, from, state0) do
    _ = send_ctrl_message(state0, mk_cmd('AUTH TLS', []))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, client: from, caller: :open, tls_options: tLSOptions)}
  end

  def handle_call({_, {:user, user, password}}, from, r_state(csock: cSock) = state)
      when cSock !== :undefined do
    handle_user(user, password, '', r_state(state, client: from))
  end

  def handle_call({_, {:user, user, password, acc}}, from, r_state(csock: cSock) = state)
      when cSock !== :undefined do
    handle_user(user, password, acc, r_state(state, client: from))
  end

  def handle_call({_, {:account, acc}}, from, state) do
    handle_user_account(acc, r_state(state, client: from))
  end

  def handle_call({_, :pwd}, from, r_state(chunk: false) = state0) do
    _ = send_ctrl_message(state0, mk_cmd('PWD', []))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, client: from, caller: :pwd)}
  end

  def handle_call({_, :lpwd}, from, r_state(ldir: lDir) = state) do
    {:reply, {:ok, lDir}, r_state(state, client: from)}
  end

  def handle_call({_, {:cd, dir}}, from, r_state(chunk: false) = state0) do
    _ = send_ctrl_message(state0, mk_cmd('CWD ~s', [dir]))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, client: from, caller: :cd)}
  end

  def handle_call({_, {:lcd, dir}}, _From, r_state(ldir: lDir0) = state) do
    lDir = :filename.absname(dir, lDir0)

    case :file.read_file_info(lDir) do
      {:ok, _} ->
        {:reply, :ok, r_state(state, ldir: lDir)}

      _ ->
        {:reply, {:error, :epath}, state}
    end
  end

  def handle_call({_, {:dir, len, dir}}, {_Pid, _} = from, r_state(chunk: false) = state) do
    setup_data_connection(
      r_state(state,
        caller: {:dir, dir, len},
        client: from
      )
    )
  end

  def handle_call({_, {:rename, currFile, newFile}}, from, r_state(chunk: false) = state0) do
    _ = send_ctrl_message(state0, mk_cmd('RNFR ~s', [currFile]))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: {:rename, newFile}, client: from)}
  end

  def handle_call({_, {:delete, file}}, {_Pid, _} = from, r_state(chunk: false) = state0) do
    _ = send_ctrl_message(state0, mk_cmd('DELE ~s', [file]))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, client: from)}
  end

  def handle_call({_, {:mkdir, dir}}, from, r_state(chunk: false) = state0) do
    _ = send_ctrl_message(state0, mk_cmd('MKD ~s', [dir]))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, client: from)}
  end

  def handle_call({_, {:rmdir, dir}}, from, r_state(chunk: false) = state0) do
    _ = send_ctrl_message(state0, mk_cmd('RMD ~s', [dir]))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, client: from)}
  end

  def handle_call({_, {:type, type}}, from, r_state(chunk: false) = state0) do
    case type do
      :ascii ->
        _ = send_ctrl_message(state0, mk_cmd('TYPE A', []))
        state = activate_ctrl_connection(state0)
        {:noreply, r_state(state, caller: :type, type: :ascii, client: from)}

      :binary ->
        _ = send_ctrl_message(state0, mk_cmd('TYPE I', []))
        state = activate_ctrl_connection(state0)
        {:noreply, r_state(state, caller: :type, type: :binary, client: from)}

      _ ->
        {:reply, {:error, :etype}, state0}
    end
  end

  def handle_call(
        {_, {:recv, remoteFile, localFile}},
        from,
        r_state(chunk: false, ldir: localDir) = state
      ) do
    progress_report({:remote_file, remoteFile}, state)
    newLocalFile = :filename.absname(localFile, localDir)

    case file_open(newLocalFile, :write) do
      {:ok, fd} ->
        setup_data_connection(
          r_state(state,
            client: from,
            caller: {:recv_file, remoteFile, fd}
          )
        )

      {:error, _What} ->
        {:reply, {:error, :epath}, state}
    end
  end

  def handle_call({_, {:recv_bin, remoteFile}}, from, r_state(chunk: false) = state) do
    setup_data_connection(
      r_state(state,
        caller: {:recv_bin, remoteFile},
        client: from
      )
    )
  end

  def handle_call({_, {:recv_chunk_start, remoteFile}}, from, r_state(chunk: false) = state) do
    setup_data_connection(
      r_state(state,
        caller: {:start_chunk_transfer, 'RETR', remoteFile},
        client: from
      )
    )
  end

  def handle_call({_, :recv_chunk}, _, r_state(chunk: false) = state) do
    {:reply, {:error, 'ftp:recv_chunk_start/2 not called'}, state}
  end

  def handle_call(
        {_, :recv_chunk},
        _From,
        r_state(
          chunk: true,
          caller:
            r_recv_chunk_closing(
              dconn_closed: true,
              pos_compl_received: true
            )
        ) = state0
      ) do
    :"n/a"
    state = activate_ctrl_connection(state0)
    {:reply, :ok, r_state(state, caller: :undefined, chunk: false, client: :undefined)}
  end

  def handle_call(
        {_, :recv_chunk},
        from,
        r_state(chunk: true, caller: r_recv_chunk_closing() = r) = state
      ) do
    :"n/a"

    {:noreply,
     r_state(state,
       client: from,
       caller: r_recv_chunk_closing(r, client_called_us: true)
     )}
  end

  def handle_call({_, :recv_chunk}, from, r_state(chunk: true) = state0) do
    state = activate_data_connection(state0)
    {:noreply, r_state(state, client: from, caller: :recv_chunk)}
  end

  def handle_call(
        {_, {:send, localFile, remoteFile}},
        from,
        r_state(chunk: false, ldir: localDir) = state
      ) do
    progress_report(
      {:local_file, :filename.absname(localFile, localDir)},
      state
    )

    setup_data_connection(
      r_state(state,
        caller: {:transfer_file, {'STOR', localFile, remoteFile}},
        client: from
      )
    )
  end

  def handle_call({_, {:append, localFile, remoteFile}}, from, r_state(chunk: false) = state) do
    setup_data_connection(
      r_state(state,
        caller: {:transfer_file, {'APPE', localFile, remoteFile}},
        client: from
      )
    )
  end

  def handle_call({_, {:send_bin, bin, remoteFile}}, from, r_state(chunk: false) = state) do
    setup_data_connection(
      r_state(state,
        caller: {:transfer_data, {'STOR', bin, remoteFile}},
        client: from
      )
    )
  end

  def handle_call({_, {:append_bin, bin, remoteFile}}, from, r_state(chunk: false) = state) do
    setup_data_connection(
      r_state(state,
        caller: {:transfer_data, {'APPE', bin, remoteFile}},
        client: from
      )
    )
  end

  def handle_call({_, {:send_chunk_start, remoteFile}}, from, r_state(chunk: false) = state) do
    setup_data_connection(
      r_state(state,
        caller: {:start_chunk_transfer, 'STOR', remoteFile},
        client: from
      )
    )
  end

  def handle_call({_, {:append_chunk_start, remoteFile}}, from, r_state(chunk: false) = state) do
    setup_data_connection(
      r_state(state,
        caller: {:start_chunk_transfer, 'APPE', remoteFile},
        client: from
      )
    )
  end

  def handle_call({_, {:transfer_chunk, bin}}, _, r_state(chunk: true) = state) do
    send_data_message(state, bin)
    {:reply, :ok, state}
  end

  def handle_call({_, {:transfer_chunk, _}}, _, r_state(chunk: false) = state) do
    {:reply, {:error, :echunk}, state}
  end

  def handle_call({_, :chunk_end}, from, r_state(chunk: true) = state0) do
    close_data_connection(state0)
    state = activate_ctrl_connection(state0)

    {:noreply,
     r_state(state, client: from, dsock: :undefined, caller: :end_chunk_transfer, chunk: false)}
  end

  def handle_call({_, :chunk_end}, _, r_state(chunk: false) = state) do
    {:reply, {:error, :echunk}, state}
  end

  def handle_call({_, {:quote, cmd}}, from, r_state(chunk: false) = state0) do
    _ = send_ctrl_message(state0, mk_cmd(cmd, []))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, client: from, caller: :quote)}
  end

  def handle_call({_, _Req}, _From, r_state(csock: cSock) = state)
      when cSock === :undefined do
    {:reply, {:error, :not_connected}, state}
  end

  def handle_call(_, _, r_state(chunk: true) = state) do
    {:reply, {:error, :echunk}, state}
  end

  def handle_call(request, _Timeout, state) do
    {:stop, {:API_violation_connection_closed, request},
     {:error, {:connection_terminated, :API_violation}}, state}
  end

  def handle_cast({pid, :close}, r_state(owner: pid) = state) do
    _ = send_ctrl_message(state, mk_cmd('QUIT', []))
    close_ctrl_connection(state)
    close_data_connection(state)
    {:stop, :normal, r_state(state, csock: :undefined, dsock: :undefined)}
  end

  def handle_cast({pid, :close}, state) do
    report = :io_lib.format('A none owner process ~p tried to close an ftp connection: ~n', [pid])
    :error_logger.info_report(report)
    {:noreply, state}
  end

  def handle_cast(msg, state) do
    {:stop, {:API_violation_connection_closed, msg}, state}
  end

  def handle_info(:timeout, r_state(caller: :open) = state) do
    {:stop, :timeout, state}
  end

  def handle_info(:timeout, state) do
    {:noreply, state}
  end

  def handle_info(
        {trpt, socket, data},
        r_state(
          dsock: {trpt, socket},
          caller: {:recv_file, fd}
        ) = state0
      )
      when trpt == :tcp or trpt == :ssl do
    :"n/a"
    :ok = file_write(:erlang.binary_to_list(data), fd)
    progress_report({:binary, data}, state0)
    state = activate_data_connection(state0)
    {:noreply, state}
  end

  def handle_info(
        {trpt, socket, data},
        r_state(dsock: {trpt, socket}, client: from, caller: :recv_chunk) = state
      )
      when trpt == :tcp or trpt == :ssl do
    :"n/a"
    :gen_server.reply(from, {:ok, data})
    {:noreply, r_state(state, client: :undefined, data: <<>>)}
  end

  def handle_info(
        {trpt, socket, data},
        r_state(dsock: {trpt, socket}) = state0
      )
      when trpt == :tcp or trpt == :ssl do
    :"n/a"
    state = activate_data_connection(state0)
    {:noreply, r_state(state, data: <<r_state(state, :data)::binary, data::binary>>)}
  end

  def handle_info(
        {cls, socket},
        r_state(
          dsock: {trpt, socket},
          caller: {:recv_file, fd}
        ) = state0
      )
      when {cls, trpt} == {:tcp_closed, :tcp} or
             {cls, trpt} == {:ssl_closed, :ssl} do
    file_close(fd)
    progress_report({:transfer_size, 0}, state0)
    state = activate_ctrl_connection(state0)
    :"n/a"
    {:noreply, r_state(state, dsock: :undefined, data: <<>>)}
  end

  def handle_info(
        {cls, socket},
        r_state(dsock: {trpt, socket}, client: client, caller: :recv_chunk) = state0
      )
      when {cls, trpt} == {:tcp_closed, :tcp} or
             {cls, trpt} == {:ssl_closed, :ssl} do
    :"n/a"
    state = activate_ctrl_connection(state0)

    {:noreply,
     r_state(state,
       dsock: :undefined,
       caller:
         r_recv_chunk_closing(
           dconn_closed: true,
           client_called_us: client !== :undefined
         )
     )}
  end

  def handle_info(
        {cls, socket},
        r_state(dsock: {trpt, socket}, caller: :recv_bin, data: data) = state0
      )
      when {cls, trpt} == {:tcp_closed, :tcp} or
             {cls, trpt} == {:ssl_closed, :ssl} do
    :"n/a"
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, dsock: :undefined, data: <<>>, caller: {:recv_bin, data})}
  end

  def handle_info(
        {cls, socket},
        r_state(dsock: {trpt, socket}, data: data, caller: {:handle_dir_result, dir}) = state0
      )
      when {cls, trpt} == {:tcp_closed, :tcp} or
             {cls, trpt} == {:ssl_closed, :ssl} do
    :"n/a"
    state = activate_ctrl_connection(state0)

    {:noreply,
     r_state(state, dsock: :undefined, caller: {:handle_dir_result, dir, data}, data: <<>>)}
  end

  def handle_info(
        {err, socket, reason},
        r_state(dsock: {trpt, socket}, client: from) = state
      )
      when {err, trpt} == {:tcp_error, :tcp} or
             {err, trpt} == {:ssl_error, :ssl} do
    :gen_server.reply(from, {:error, reason})
    close_data_connection(state)

    {:noreply,
     r_state(state,
       dsock: :undefined,
       client: :undefined,
       data: <<>>,
       caller: :undefined,
       chunk: false
     )}
  end

  def handle_info(
        {transport, socket, data},
        r_state(
          csock: {transport, socket},
          verbose: verbose,
          caller: caller,
          client: from,
          ctrl_data: {binCtrlData, accLines, lineStatus}
        ) = state0
      ) do
    :"n/a"

    case :ftp_response.parse_lines(<<binCtrlData::binary, data::binary>>, accLines, lineStatus) do
      {:ok, lines, nextMsgData} ->
        verbose(lines, verbose, :receive)
        ctrlResult = :ftp_response.interpret(lines)

        case caller do
          :quote ->
            :gen_server.reply(
              from,
              :string.tokens(lines, [?\r, ?\n])
            )

            {:noreply,
             r_state(state0,
               client: :undefined,
               caller: :undefined,
               latest_ctrl_response: lines,
               ctrl_data: {nextMsgData, [], :start}
             )}

          _ ->
            :"n/a"

            handle_ctrl_result(
              ctrlResult,
              r_state(state0,
                latest_ctrl_response: lines,
                ctrl_data: {nextMsgData, [], :start}
              )
            )
        end

      {:continue, ctrlData}
      when ctrlData !== r_state(state0, :ctrl_data) ->
        :"n/a"
        state1 = r_state(state0, ctrl_data: ctrlData)
        state = activate_ctrl_connection(state1)
        {:noreply, state}

      {:continue, _CtrlData} ->
        :"n/a"
        {:noreply, state0}
    end
  end

  def handle_info({cls, socket}, r_state(csock: {trpt, socket}))
      when {cls, trpt} == {:tcp_closed, :tcp} or
             {cls, trpt} == {:ssl_closed, :ssl} do
    exit(:normal)
  end

  def handle_info({err, socket, reason}, _)
      when err == :tcp_error or err == :ssl_error do
    report = :io_lib.format('~p on socket: ~p  for reason: ~p~n', [err, socket, reason])
    :error_logger.error_report(report)
    exit(:normal)
  end

  def handle_info(
        {:DOWN, _Ref, _Type, _Process, :normal},
        state
      ) do
    {:stop, :normal, r_state(state, client: :undefined)}
  end

  def handle_info(
        {:DOWN, _Ref, _Type, _Process, :shutdown},
        state
      ) do
    {:stop, :normal, r_state(state, client: :undefined)}
  end

  def handle_info(
        {:DOWN, _Ref, _Type, _Process, :timeout},
        state
      ) do
    {:stop, :normal, r_state(state, client: :undefined)}
  end

  def handle_info({:DOWN, _Ref, _Type, process, reason}, state) do
    {:stop, {:stopped, {:EXIT, process, reason}}, r_state(state, client: :undefined)}
  end

  def handle_info(
        {:EXIT, pid, reason},
        r_state(progress: pid) = state
      ) do
    report = :io_lib.format('Progress reporting stopped for reason ~p~n', [reason])
    :error_logger.info_report(report)
    {:noreply, r_state(state, progress: :ignore)}
  end

  def handle_info(info, state) do
    report =
      :io_lib.format('ftp : ~p : Unexpected message: ~p~nState: ~p~n', [self(), info, state])

    :error_logger.info_report(report)
    {:noreply, state}
  end

  def terminate(:normal, state) do
    progress_report(:stop, state)
    do_terminate({:error, :econn}, state)
  end

  def terminate(reason, state) do
    report = :io_lib.format('Ftp connection closed due to: ~p~n', [reason])
    :error_logger.error_report(report)
    do_terminate({:error, :eclosed}, state)
  end

  defp do_terminate(errorMsg, state) do
    close_data_connection(state)
    close_ctrl_connection(state)

    case r_state(state, :client) do
      :undefined ->
        :ok

      from ->
        :gen_server.reply(from, errorMsg)
    end

    :ok
  end

  def code_change(_Vsn, state1, :upgrade_from_pre_5_12) do
    {:state, cSock, dSock, verbose, lDir, type, chunk, mode, timeout, data, ctrlData, owner,
     client, caller, iPv6Disable, progress} = state1

    ipFamily =
      cond do
        iPv6Disable === true ->
          :inet

        true ->
          :inet6fb4
      end

    state2 =
      r_state(
        csock: cSock,
        dsock: dSock,
        verbose: verbose,
        ldir: lDir,
        type: type,
        chunk: chunk,
        mode: mode,
        timeout: timeout,
        data: data,
        ctrl_data: ctrlData,
        owner: owner,
        client: client,
        caller: caller,
        ipfamily: ipFamily,
        progress: progress
      )

    {:ok, state2}
  end

  def code_change(_Vsn, state1, :downgrade_to_pre_5_12) do
    r_state(
      csock: cSock,
      dsock: dSock,
      verbose: verbose,
      ldir: lDir,
      type: type,
      chunk: chunk,
      mode: mode,
      timeout: timeout,
      data: data,
      ctrl_data: ctrlData,
      owner: owner,
      client: client,
      caller: caller,
      ipfamily: ipFamily,
      progress: progress
    ) = state1

    iPv6Disable =
      cond do
        ipFamily === :inet ->
          true

        true ->
          false
      end

    state2 =
      {:state, cSock, dSock, verbose, lDir, type, chunk, mode, timeout, data, ctrlData, owner,
       client, caller, iPv6Disable, progress}

    {:ok, state2}
  end

  def code_change(_Vsn, state, _Extra) do
    {:ok, state}
  end

  def start_link([opts, genServerOptions]) do
    start_link(opts, genServerOptions)
  end

  def start_link(opts, genServerOptions) do
    case :lists.keysearch(:client, 1, opts) do
      {:value, _} ->
        :gen_server.start_link(:ftp, opts, genServerOptions)

      false ->
        opts2 = [{:client, self()} | opts]
        :gen_server.start_link(:ftp, opts2, genServerOptions)
    end
  end

  defp handle_user(user, password, acc, state0) do
    _ = send_ctrl_message(state0, mk_cmd('USER ~s', [user]))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: {:handle_user, password, acc})}
  end

  defp handle_user_passwd(password, acc, state0) do
    _ = send_ctrl_message(state0, mk_cmd('PASS ~s', [password]))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: {:handle_user_passwd, acc})}
  end

  defp handle_user_account(acc, state0) do
    _ = send_ctrl_message(state0, mk_cmd('ACCT ~s', [acc]))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: :handle_user_account)}
  end

  defp handle_ctrl_result(
         {:tls_upgrade, _},
         r_state(
           csock: {:tcp, socket},
           tls_options: tLSOptions,
           timeout: timeout,
           caller: :open,
           client: from
         ) = state0
       ) do
    :"n/a"

    case :ssl.connect(socket, tLSOptions, timeout) do
      {:ok, tLSSocket} ->
        state1 = r_state(state0, csock: {:ssl, tLSSocket})
        _ = send_ctrl_message(state1, mk_cmd('PBSZ 0', []))
        state = activate_ctrl_connection(state1)
        {:noreply, r_state(state, tls_upgrading_data_connection: {true, :pbsz})}

      {:error, _} = error ->
        :gen_server.reply(from, {error, self()})

        {:stop, :normal,
         r_state(state0,
           client: :undefined,
           caller: :undefined,
           tls_upgrading_data_connection: false
         )}
    end
  end

  defp handle_ctrl_result(
         {:pos_compl, _},
         r_state(tls_upgrading_data_connection: {true, :pbsz}) = state0
       ) do
    _ = send_ctrl_message(state0, mk_cmd('PROT P', []))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, tls_upgrading_data_connection: {true, :prot})}
  end

  defp handle_ctrl_result(
         {:pos_compl, _},
         r_state(
           tls_upgrading_data_connection: {true, :prot},
           client: from
         ) = state
       ) do
    :gen_server.reply(from, {:ok, self()})

    {:noreply,
     r_state(state, client: :undefined, caller: :undefined, tls_upgrading_data_connection: false)}
  end

  defp handle_ctrl_result(
         {:pos_compl, _},
         r_state(caller: :open, client: from) = state
       ) do
    :gen_server.reply(from, {:ok, self()})
    {:noreply, r_state(state, client: :undefined, caller: :undefined)}
  end

  defp handle_ctrl_result({_, lines}, r_state(caller: :open) = state) do
    ctrl_result_response(:econn, state, {:error, lines})
  end

  defp handle_ctrl_result(
         {:pos_compl, _Lines},
         r_state(
           mode: :active,
           caller: {:setup_data_connection, {lSock, caller}}
         ) = state
       ) do
    handle_caller(
      r_state(state,
        caller: caller,
        dsock: {:lsock, lSock}
      )
    )
  end

  defp handle_ctrl_result(
         {status, _Lines},
         r_state(
           mode: :active,
           caller: {:setup_data_connection, {lSock, _}}
         ) = state
       ) do
    close_connection({:tcp, lSock})
    ctrl_result_response(status, state, {:error, status})
  end

  defp handle_ctrl_result(
         {:pos_compl, lines},
         r_state(
           mode: :passive,
           ipfamily: :inet6,
           client: from,
           caller: {:setup_data_connection, caller},
           csock: cSock,
           sockopts_data_passive: sockOpts,
           timeout: timeout
         ) = state
       ) do
    [
      [_, portStr]
      | _
    ] = :lists.reverse(:string.tokens(lines, '|'))

    {:ok, {iP, _}} = peername(cSock)

    case connect(iP, :erlang.list_to_integer(portStr), sockOpts, timeout, state) do
      {:ok, _, socket} ->
        handle_caller(
          r_state(state,
            caller: caller,
            dsock: {:tcp, socket}
          )
        )

      {:error, _Reason} = error ->
        :gen_server.reply(from, error)
        {:noreply, r_state(state, client: :undefined, caller: :undefined)}
    end
  end

  defp handle_ctrl_result(
         {:pos_compl, lines},
         r_state(
           mode: :passive,
           ipfamily: :inet,
           client: from,
           caller: {:setup_data_connection, caller},
           timeout: timeout,
           sockopts_data_passive: sockOpts,
           ftp_extension: false
         ) = state
       ) do
    {_, [?( | rest]} =
      :lists.splitwith(
        fn
          ?( ->
            false

          _ ->
            true
        end,
        lines
      )

    {newPortAddr, _} =
      :lists.splitwith(
        fn
          ?) ->
            false

          _ ->
            true
        end,
        rest
      )

    [a1, a2, a3, a4, p1, p2] =
      :lists.map(
        fn x ->
          :erlang.list_to_integer(x)
        end,
        :string.tokens(newPortAddr, [?,])
      )

    iP = {a1, a2, a3, a4}
    port = p1 * 256 + p2
    :"n/a"

    case connect(iP, port, sockOpts, timeout, state) do
      {:ok, _, socket} ->
        handle_caller(
          r_state(state,
            caller: caller,
            dsock: {:tcp, socket}
          )
        )

      {:error, _Reason} = error ->
        :gen_server.reply(from, error)
        {:noreply, r_state(state, client: :undefined, caller: :undefined)}
    end
  end

  defp handle_ctrl_result(
         {:pos_compl, lines},
         r_state(
           mode: :passive,
           ipfamily: :inet,
           client: from,
           caller: {:setup_data_connection, caller},
           csock: cSock,
           timeout: timeout,
           sockopts_data_passive: sockOpts,
           ftp_extension: true
         ) = state
       ) do
    [
      [_, portStr]
      | _
    ] = :lists.reverse(:string.tokens(lines, '|'))

    {:ok, {iP, _}} = peername(cSock)
    :"n/a"

    case connect(iP, :erlang.list_to_integer(portStr), sockOpts, timeout, state) do
      {:ok, _, socket} ->
        handle_caller(
          r_state(state,
            caller: caller,
            dsock: {:tcp, socket}
          )
        )

      {:error, _Reason} = error ->
        :gen_server.reply(from, error)
        {:noreply, r_state(state, client: :undefined, caller: :undefined)}
    end
  end

  defp handle_ctrl_result(
         _,
         r_state(
           mode: :passive,
           caller: {:setup_data_connection, caller}
         ) = state
       ) do
    setup_data_connection(
      r_state(state,
        mode: :active,
        caller: caller
      )
    )
  end

  defp handle_ctrl_result(
         {:pos_interm, _},
         r_state(caller: {:handle_user, passWord, acc}) = state
       ) do
    handle_user_passwd(passWord, acc, state)
  end

  defp handle_ctrl_result(
         {status, _},
         r_state(caller: {:handle_user, _, _}) = state
       ) do
    ctrl_result_response(status, state, {:error, :euser})
  end

  defp handle_ctrl_result(
         {:pos_interm_acct, _},
         r_state(caller: {:handle_user_passwd, acc}) = state
       )
       when acc !== '' do
    handle_user_account(acc, state)
  end

  defp handle_ctrl_result(
         {status, _},
         r_state(caller: {:handle_user_passwd, _}) = state
       ) do
    ctrl_result_response(status, state, {:error, :euser})
  end

  defp handle_ctrl_result(
         {:pos_compl, lines},
         r_state(caller: :pwd, client: from) = state
       ) do
    dir = pwd_result(lines)
    :gen_server.reply(from, {:ok, dir})
    {:noreply, r_state(state, client: :undefined, caller: :undefined)}
  end

  defp handle_ctrl_result(
         {:pos_prel, _},
         r_state(caller: {:dir, dir}) = state0
       ) do
    case accept_data_connection(state0) do
      {:ok, state1} ->
        state = activate_data_connection(state1)
        {:noreply, r_state(state, caller: {:handle_dir_result, dir})}

      {:error, _Reason} = eRROR ->
        case r_state(state0, :client) do
          :undefined ->
            {:stop, eRROR, state0}

          from ->
            :gen_server.reply(from, eRROR)
            {:stop, :normal, r_state(state0, client: :undefined)}
        end
    end
  end

  defp handle_ctrl_result(
         {:pos_compl, _},
         r_state(
           caller: {:handle_dir_result, dir, data},
           client: from
         ) = state
       ) do
    case dir do
      '' ->
        :gen_server.reply(from, {:ok, data})
        {:noreply, r_state(state, client: :undefined, caller: :undefined)}

      _ ->
        :gen_server.reply(from, {:ok, data})
        {:noreply, r_state(state, client: :undefined, caller: :undefined)}
    end
  end

  defp handle_ctrl_result(
         {:pos_compl, lines},
         r_state(caller: {:handle_dir_data, dir, dirData}) = state0
       ) do
    oldDir = pwd_result(lines)
    _ = send_ctrl_message(state0, mk_cmd('CWD ~s', [dir]))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: {:handle_dir_data_second_phase, oldDir, dirData})}
  end

  defp handle_ctrl_result(
         {status, _},
         r_state(caller: {:handle_dir_data, _, _}) = state
       ) do
    ctrl_result_response(status, state, {:error, :epath})
  end

  defp handle_ctrl_result(
         s = {_Status, _},
         r_state(caller: {:handle_dir_result, _, _}) = state
       ) do
    ctrl_result_response(s, state, {:error, :epath})
  end

  defp handle_ctrl_result(
         {:pos_compl, _},
         r_state(caller: {:handle_dir_data_second_phase, oldDir, dirData}) = state0
       ) do
    _ = send_ctrl_message(state0, mk_cmd('CWD ~s', [oldDir]))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: {:handle_dir_data_third_phase, dirData})}
  end

  defp handle_ctrl_result(
         {status, _},
         r_state(caller: {:handle_dir_data_second_phase, _, _}) = state
       ) do
    ctrl_result_response(status, state, {:error, :epath})
  end

  defp handle_ctrl_result(
         _,
         r_state(
           caller: {:handle_dir_data_third_phase, dirData},
           client: from
         ) = state
       ) do
    :gen_server.reply(from, {:ok, dirData})
    {:noreply, r_state(state, client: :undefined, caller: :undefined)}
  end

  defp handle_ctrl_result({status, _}, r_state(caller: :cd) = state) do
    ctrl_result_response(status, state, {:error, status})
  end

  defp handle_ctrl_result(
         status = {:epath, _},
         r_state(caller: {:dir, _}) = state
       ) do
    ctrl_result_response(status, state, {:error, :epath})
  end

  defp handle_ctrl_result(
         {:pos_interm, _},
         r_state(caller: {:rename, newFile}) = state0
       ) do
    _ = send_ctrl_message(state0, mk_cmd('RNTO ~s', [newFile]))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: :rename_second_phase)}
  end

  defp handle_ctrl_result({status, _}, r_state(caller: {:rename, _}) = state) do
    ctrl_result_response(status, state, {:error, status})
  end

  defp handle_ctrl_result(
         {status, _},
         r_state(caller: :rename_second_phase) = state
       ) do
    ctrl_result_response(status, state, {:error, status})
  end

  defp handle_ctrl_result(
         {:pos_prel, _},
         r_state(caller: :recv_bin) = state0
       ) do
    case accept_data_connection(state0) do
      {:ok, state1} ->
        state = activate_data_connection(state1)
        {:noreply, state}

      {:error, _Reason} = eRROR ->
        case r_state(state0, :client) do
          :undefined ->
            {:stop, eRROR, state0}

          from ->
            :gen_server.reply(from, eRROR)
            {:stop, :normal, r_state(state0, client: :undefined)}
        end
    end
  end

  defp handle_ctrl_result(
         {:pos_compl, _},
         r_state(caller: {:recv_bin, data}, client: from) = state
       ) do
    :gen_server.reply(from, {:ok, data})
    close_data_connection(state)
    {:noreply, r_state(state, client: :undefined, caller: :undefined)}
  end

  defp handle_ctrl_result({status, _}, r_state(caller: :recv_bin) = state) do
    close_data_connection(state)
    ctrl_result_response(status, r_state(state, dsock: :undefined), {:error, :epath})
  end

  defp handle_ctrl_result(
         {status, _},
         r_state(caller: {:recv_bin, _}) = state
       ) do
    close_data_connection(state)
    ctrl_result_response(status, r_state(state, dsock: :undefined), {:error, :epath})
  end

  defp handle_ctrl_result(
         {:pos_prel, _},
         r_state(
           client: from,
           caller: :start_chunk_transfer
         ) = state0
       ) do
    case accept_data_connection(state0) do
      {:ok, state1} ->
        state = start_chunk(state1)
        {:noreply, state}

      {:error, _Reason} = eRROR ->
        case r_state(state0, :client) do
          :undefined ->
            {:stop, eRROR, state0}

          ^from ->
            :gen_server.reply(from, eRROR)
            {:stop, :normal, r_state(state0, client: :undefined)}
        end
    end
  end

  defp handle_ctrl_result(
         {:pos_compl, _},
         r_state(
           client: from,
           caller:
             r_recv_chunk_closing(
               dconn_closed: true,
               client_called_us: true,
               pos_compl_received: false
             )
         ) = state0
       )
       when from !== :undefined do
    :"n/a"
    :gen_server.reply(from, :ok)
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: :undefined, chunk: false, client: :undefined)}
  end

  defp handle_ctrl_result({:pos_compl, _}, r_state(caller: r_recv_chunk_closing() = r) = state0) do
    :"n/a"
    {:noreply, r_state(state0, caller: r_recv_chunk_closing(r, pos_compl_received: true))}
  end

  defp handle_ctrl_result(
         {:pos_prel, _},
         r_state(caller: {:recv_file, _}) = state0
       ) do
    case accept_data_connection(state0) do
      {:ok, state1} ->
        state = activate_data_connection(state1)
        {:noreply, state}

      {:error, _Reason} = eRROR ->
        case r_state(state0, :client) do
          :undefined ->
            {:stop, eRROR, state0}

          from ->
            :gen_server.reply(from, eRROR)
            {:stop, :normal, r_state(state0, client: :undefined)}
        end
    end
  end

  defp handle_ctrl_result(
         {status, _},
         r_state(caller: {:recv_file, fd}) = state
       ) do
    file_close(fd)
    close_data_connection(state)
    ctrl_result_response(status, r_state(state, dsock: :undefined), {:error, :epath})
  end

  defp handle_ctrl_result(
         {:pos_prel, _},
         r_state(caller: {:transfer_file, fd}) = state0
       ) do
    case accept_data_connection(state0) do
      {:ok, state1} ->
        send_file(state1, fd)

      {:error, _Reason} = eRROR ->
        case r_state(state0, :client) do
          :undefined ->
            {:stop, eRROR, state0}

          from ->
            :gen_server.reply(from, eRROR)
            {:stop, :normal, r_state(state0, client: :undefined)}
        end
    end
  end

  defp handle_ctrl_result(
         {:pos_prel, _},
         r_state(caller: {:transfer_data, bin}) = state0
       ) do
    case accept_data_connection(state0) do
      {:ok, state} ->
        send_bin(state, bin)

      {:error, _Reason} = eRROR ->
        case r_state(state0, :client) do
          :undefined ->
            {:stop, eRROR, state0}

          from ->
            :gen_server.reply(from, eRROR)
            {:stop, :normal, r_state(state0, client: :undefined)}
        end
    end
  end

  defp handle_ctrl_result({status, _Lines}, r_state(client: from) = state)
       when from !== :undefined do
    ctrl_result_response(status, state, {:error, status})
  end

  defp ctrl_result_response(:pos_compl, r_state(client: from) = state, _) do
    :gen_server.reply(from, :ok)
    {:noreply, r_state(state, client: :undefined, caller: :undefined)}
  end

  defp ctrl_result_response(:enofile, r_state(client: from) = state, _) do
    :gen_server.reply(from, {:error, :enofile})
    {:noreply, r_state(state, client: :undefined, caller: :undefined)}
  end

  defp ctrl_result_response(status, r_state(client: from) = state, _)
       when status === :etnospc or status === :epnospc or status === :efnamena or
              status === :econn do
    :gen_server.reply(from, {:error, status})
    {:stop, :normal, r_state(state, client: :undefined)}
  end

  defp ctrl_result_response(_, r_state(client: from) = state, errorMsg) do
    :gen_server.reply(from, errorMsg)
    {:noreply, r_state(state, client: :undefined, caller: :undefined)}
  end

  defp handle_caller(r_state(caller: {:dir, dir, len}) = state0) do
    cmd =
      case len do
        :short ->
          'NLST'

        :long ->
          'LIST'
      end

    _ =
      case dir do
        '' ->
          send_ctrl_message(state0, mk_cmd(cmd, ''))

        _ ->
          send_ctrl_message(state0, mk_cmd(cmd ++ ' ~s', [dir]))
      end

    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: {:dir, dir})}
  end

  defp handle_caller(r_state(caller: {:recv_bin, remoteFile}) = state0) do
    _ = send_ctrl_message(state0, mk_cmd('RETR ~s', [remoteFile]))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: :recv_bin)}
  end

  defp handle_caller(r_state(caller: {:start_chunk_transfer, cmd, remoteFile}) = state0) do
    _ =
      send_ctrl_message(
        state0,
        mk_cmd('~s ~s', [cmd, remoteFile])
      )

    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: :start_chunk_transfer)}
  end

  defp handle_caller(r_state(caller: {:recv_file, remoteFile, fd}) = state0) do
    _ = send_ctrl_message(state0, mk_cmd('RETR ~s', [remoteFile]))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: {:recv_file, fd})}
  end

  defp handle_caller(
         r_state(
           caller: {:transfer_file, {cmd, localFile, remoteFile}},
           ldir: localDir,
           client: from
         ) = state0
       ) do
    case file_open(
           :filename.absname(localFile, localDir),
           :read
         ) do
      {:ok, fd} ->
        _ =
          send_ctrl_message(
            state0,
            mk_cmd('~s ~s', [cmd, remoteFile])
          )

        state = activate_ctrl_connection(state0)
        {:noreply, r_state(state, caller: {:transfer_file, fd})}

      {:error, _} ->
        :gen_server.reply(from, {:error, :epath})
        {:noreply, r_state(state0, client: :undefined, caller: :undefined, dsock: :undefined)}
    end
  end

  defp handle_caller(r_state(caller: {:transfer_data, {cmd, bin, remoteFile}}) = state0) do
    _ =
      send_ctrl_message(
        state0,
        mk_cmd('~s ~s', [cmd, remoteFile])
      )

    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: {:transfer_data, bin})}
  end

  defp setup_ctrl_connection(host, port, timeout, r_state(sockopts_ctrl: sockOpts) = state0) do
    msTime = :erlang.monotonic_time()

    case connect(host, port, sockOpts, timeout, state0) do
      {:ok, ipFam, cSock} ->
        state1 =
          r_state(state0,
            csock: {:tcp, cSock},
            ipfamily: ipFam
          )

        state = activate_ctrl_connection(state1)

        case timeout - millisec_passed(msTime) do
          timeout2 when timeout2 >= 0 ->
            {:ok, r_state(state, caller: :open), timeout2}

          _ ->
            {:ok, r_state(state, caller: :open), 0}
        end

      error ->
        error
    end
  end

  defp setup_data_connection(
         r_state(
           mode: :active,
           caller: caller,
           csock: cSock,
           sockopts_data_active: sockOpts,
           ftp_extension: ftpExt
         ) = state0
       ) do
    case (try do
            sockname(cSock)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, {{_, _, _, _, _, _, _, _} = iP0, _}} ->
        iP = :proplists.get_value(:ip, sockOpts, iP0)

        {:ok, lSock} =
          :gen_tcp.listen(
            0,
            [
              [{:ip, iP}, {:active, false}, :inet6, :binary, {:packet, 0}]
              | :lists.keydelete(:ip, 1, sockOpts)
            ]
          )

        {:ok, {_, port}} = sockname({:tcp, lSock})
        ipAddress = :inet_parse.ntoa(iP)
        cmd = mk_cmd('EPRT |2|~s|~p|', [ipAddress, port])
        _ = send_ctrl_message(state0, cmd)
        state = activate_ctrl_connection(state0)
        {:noreply, r_state(state, caller: {:setup_data_connection, {lSock, caller}})}

      {:ok, {{_, _, _, _} = iP0, _}} ->
        iP = :proplists.get_value(:ip, sockOpts, iP0)

        {:ok, lSock} =
          :gen_tcp.listen(
            0,
            [
              [{:ip, iP}, {:active, false}, :binary, {:packet, 0}]
              | :lists.keydelete(:ip, 1, sockOpts)
            ]
          )

        {:ok, port} = :inet.port(lSock)

        _ =
          case ftpExt do
            false ->
              {iP1, iP2, iP3, iP4} = iP
              {port1, port2} = {div(port, 256), rem(port, 256)}

              send_ctrl_message(
                state0,
                mk_cmd(
                  'PORT ~w,~w,~w,~w,~w,~w',
                  [iP1, iP2, iP3, iP4, port1, port2]
                )
              )

            true ->
              ipAddress = :inet_parse.ntoa(iP)
              cmd = mk_cmd('EPRT |1|~s|~p|', [ipAddress, port])
              send_ctrl_message(state0, cmd)
          end

        state = activate_ctrl_connection(state0)
        {:noreply, r_state(state, caller: {:setup_data_connection, {lSock, caller}})}
    end
  end

  defp setup_data_connection(r_state(mode: :passive, ipfamily: :inet6, caller: caller) = state0) do
    _ = send_ctrl_message(state0, mk_cmd('EPSV', []))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: {:setup_data_connection, caller})}
  end

  defp setup_data_connection(
         r_state(mode: :passive, ipfamily: :inet, caller: caller, ftp_extension: false) = state0
       ) do
    _ = send_ctrl_message(state0, mk_cmd('PASV', []))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: {:setup_data_connection, caller})}
  end

  defp setup_data_connection(
         r_state(mode: :passive, ipfamily: :inet, caller: caller, ftp_extension: true) = state0
       ) do
    _ = send_ctrl_message(state0, mk_cmd('EPSV', []))
    state = activate_ctrl_connection(state0)
    {:noreply, r_state(state, caller: {:setup_data_connection, caller})}
  end

  defp connect(host, port, sockOpts, timeout, r_state(ipfamily: :inet = ipFam)) do
    connect2(host, port, ipFam, sockOpts, timeout)
  end

  defp connect(host, port, sockOpts, timeout, r_state(ipfamily: :inet6 = ipFam)) do
    connect2(host, port, ipFam, sockOpts, timeout)
  end

  defp connect(host, port, sockOpts, timeout, r_state(ipfamily: :inet6fb4)) do
    case :inet.getaddr(host, :inet6) do
      {:ok, {0, 0, 0, 0, 0, 65535, _, _} = iPv6} ->
        case :inet.getaddr(host, :inet) do
          {:ok, iPv4} ->
            ipFam = :inet
            connect2(iPv4, port, ipFam, sockOpts, timeout)

          _ ->
            ipFam = :inet6
            connect2(iPv6, port, ipFam, sockOpts, timeout)
        end

      {:ok, iPv6} ->
        ipFam = :inet6
        connect2(iPv6, port, ipFam, sockOpts, timeout)

      _ ->
        case :inet.getaddr(host, :inet) do
          {:ok, iPv4} ->
            ipFam = :inet
            connect2(iPv4, port, ipFam, sockOpts, timeout)

          error ->
            error
        end
    end
  end

  defp connect2(host, port, ipFam, sockOpts, timeout) do
    opts = [
      [ipFam, :binary, {:packet, 0}, {:active, false}]
      | sockOpts
    ]

    case :gen_tcp.connect(host, port, opts, timeout) do
      {:ok, sock} ->
        {:ok, ipFam, sock}

      error ->
        error
    end
  end

  defp accept_data_connection(
         r_state(
           mode: :active,
           dtimeout: dTimeout,
           tls_options: tLSOptions,
           dsock: {:lsock, lSock}
         ) = state0
       ) do
    case :gen_tcp.accept(lSock, dTimeout) do
      {:ok, socket} when is_list(tLSOptions) ->
        :gen_tcp.close(lSock)
        :"n/a"

        case :ssl.connect(socket, tLSOptions, dTimeout) do
          {:ok, tLSSocket} ->
            {:ok, r_state(state0, dsock: {:ssl, tLSSocket})}

          {:error, reason} ->
            {:error, {:ssl_connect_failed, reason}}
        end

      {:ok, socket} ->
        :gen_tcp.close(lSock)
        {:ok, r_state(state0, dsock: {:tcp, socket})}

      {:error, reason} ->
        {:error, {:data_connect_failed, reason}}
    end
  end

  defp accept_data_connection(
         r_state(
           mode: :passive,
           dtimeout: dTimeout,
           dsock: {:tcp, socket},
           tls_options: tLSOptions
         ) = state
       )
       when is_list(tLSOptions) do
    :"n/a"

    case :ssl.connect(socket, tLSOptions, dTimeout) do
      {:ok, tLSSocket} ->
        {:ok, r_state(state, dsock: {:ssl, tLSSocket})}

      {:error, reason} ->
        {:error, {:ssl_connect_failed, reason}}
    end
  end

  defp accept_data_connection(r_state(mode: :passive) = state) do
    {:ok, state}
  end

  defp send_ctrl_message(
         _S = r_state(csock: socket, verbose: verbose),
         message
       ) do
    verbose(:lists.flatten(message), verbose, :send)
    :"n/a"
    _ = send_message(socket, message)
  end

  defp send_data_message(_S = r_state(dsock: socket), message) do
    :"n/a"

    case send_message(socket, message) do
      :ok ->
        :ok

      {:error, reason} ->
        report = :io_lib.format('send/2 for socket ~p failed with reason ~p~n', [socket, reason])
        :error_logger.error_report(report)
        exit(:normal)
    end
  end

  defp send_message({:tcp, socket}, message) do
    :gen_tcp.send(socket, message)
  end

  defp send_message({:ssl, socket}, message) do
    :ssl.send(socket, message)
  end

  defp activate_ctrl_connection(
         r_state(
           csock: cSock,
           ctrl_data: {<<>>, _, _}
         ) = state
       ) do
    _ = activate_connection(cSock)
    state
  end

  defp activate_ctrl_connection(r_state(csock: cSock) = state0) do
    _ = activate_connection(cSock)

    {:noreply, state} =
      handle_info(
        {socket_type(cSock), unwrap_socket(cSock), <<>>},
        state0
      )

    state
  end

  defp activate_data_connection(r_state(dsock: dSock) = state) do
    _ = activate_connection(dSock)
    state
  end

  defp activate_connection(socket) do
    case socket_type(socket) do
      :tcp ->
        _ = activate_connection(:inet, :tcp_closed, socket)

      :ssl ->
        _ = activate_connection(:ssl, :ssl_closed, socket)
    end
  end

  defp activate_connection(aPI, closeTag, socket0) do
    socket = unwrap_socket(socket0)

    case aPI.setopts(socket, [{:active, :once}]) do
      :ok ->
        :ok

      {:error, _} ->
        send(self(), {closeTag, socket})
    end
  end

  defp ignore_return_value(_) do
    :ok
  end

  defp unwrap_socket({:tcp, socket}) do
    socket
  end

  defp unwrap_socket({:ssl, socket}) do
    socket
  end

  defp socket_type({:tcp, _Socket}) do
    :tcp
  end

  defp socket_type({:ssl, _Socket}) do
    :ssl
  end

  defp close_ctrl_connection(r_state(csock: :undefined)) do
    :ok
  end

  defp close_ctrl_connection(r_state(csock: socket)) do
    close_connection(socket)
  end

  defp close_data_connection(r_state(dsock: :undefined)) do
    :ok
  end

  defp close_data_connection(r_state(dsock: socket)) do
    close_connection(socket)
  end

  defp close_connection({:lsock, socket}) do
    ignore_return_value(:gen_tcp.close(socket))
  end

  defp close_connection({:tcp, socket}) do
    ignore_return_value(:gen_tcp.close(socket))
  end

  defp close_connection({:ssl, socket}) do
    ignore_return_value(:ssl.close(socket))
  end

  defp send_file(
         r_state(tls_upgrading_data_connection: {true, cTRL, _}) = state,
         fd
       ) do
    {:noreply, r_state(state, tls_upgrading_data_connection: {true, cTRL, :ftp, :send_file, fd})}
  end

  defp send_file(state0, fd) do
    case file_read(fd) do
      {:ok, n, bin} when n > 0 ->
        send_data_message(state0, bin)
        progress_report({:binary, bin}, state0)
        send_file(state0, fd)

      {:ok, _, _} ->
        file_close(fd)
        close_data_connection(state0)
        progress_report({:transfer_size, 0}, state0)
        state = activate_ctrl_connection(state0)

        {:noreply,
         r_state(state,
           caller: :transfer_file_second_phase,
           dsock: :undefined
         )}

      {:error, reason} ->
        :gen_server.reply(r_state(state0, :client), {:error, reason})
        {:stop, :normal, r_state(state0, client: :undefined)}
    end
  end

  defp file_open(file, option) do
    :file.open(file, [:raw, :binary, option])
  end

  defp file_close(fd) do
    ignore_return_value(:file.close(fd))
  end

  defp file_read(fd) do
    case :file.read(fd, 4096) do
      {:ok, bytes} ->
        {:ok, :erlang.size(bytes), bytes}

      :eof ->
        {:ok, 0, []}

      other ->
        other
    end
  end

  defp file_write(bytes, fd) do
    :file.write(fd, bytes)
  end

  defp call(genServer, msg, format) do
    call(genServer, msg, format, :infinity)
  end

  defp call(genServer, msg, format, timeout) do
    req = {self(), msg}

    case (try do
            :gen_server.call(genServer, req, timeout)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, bin} when is_binary(bin) and format === :string ->
        {:ok, :erlang.binary_to_list(bin)}

      {:EXIT, _} ->
        {:error, :eclosed}

      result ->
        result
    end
  end

  defp cast(genServer, msg) do
    :gen_server.cast(genServer, {self(), msg})
  end

  defp send_bin(
         r_state(tls_upgrading_data_connection: {true, cTRL, _}) = state,
         bin
       ) do
    r_state(state, tls_upgrading_data_connection: {true, cTRL, :ftp, :send_bin, bin})
  end

  defp send_bin(state0, bin) do
    send_data_message(state0, bin)
    close_data_connection(state0)
    state = activate_ctrl_connection(state0)

    {:noreply,
     r_state(state,
       caller: :transfer_data_second_phase,
       dsock: :undefined
     )}
  end

  defp mk_cmd(fmt, args) do
    [:io_lib.format(fmt, args), ?\r, ?\n]
  end

  defp is_name_sane([]) do
    true
  end

  defp is_name_sane([?\r | _]) do
    false
  end

  defp is_name_sane([?\n | _]) do
    false
  end

  defp is_name_sane([_ | rest]) do
    is_name_sane(rest)
  end

  defp pwd_result(lines) do
    {_, [?" | rest]} =
      :lists.splitwith(
        fn
          ?" ->
            false

          _ ->
            true
        end,
        lines
      )

    {dir, _} =
      :lists.splitwith(
        fn
          ?" ->
            false

          _ ->
            true
        end,
        rest
      )

    dir
  end

  defp key_search(key, list, default) do
    case :lists.keysearch(key, 1, list) do
      {:value, {_, val}} ->
        val

      false ->
        default
    end
  end

  defp verbose(lines, true, direction) do
    dirStr =
      case direction do
        :send ->
          'Sending: '

        _ ->
          'Receiving: '
      end

    str = :string.strip(:string.strip(lines, :right, ?\n), :right, ?\r)
    :erlang.display(dirStr ++ str)
  end

  defp verbose(_, false, _) do
    :ok
  end

  defp progress(options) do
    :ftp_progress.start_link(options)
  end

  defp progress_report(_, r_state(progress: :ignore)) do
    :ok
  end

  defp progress_report(:stop, r_state(progress: progressPid)) do
    :ftp_progress.stop(progressPid)
  end

  defp progress_report({:binary, data}, r_state(progress: progressPid)) do
    :ftp_progress.report(
      progressPid,
      {:transfer_size, :erlang.size(data)}
    )
  end

  defp progress_report(report, r_state(progress: progressPid)) do
    :ftp_progress.report(progressPid, report)
  end

  defp peername({:tcp, socket}) do
    :inet.peername(socket)
  end

  defp peername({:ssl, socket}) do
    :ssl.peername(socket)
  end

  defp sockname({:tcp, socket}) do
    :inet.sockname(socket)
  end

  defp sockname({:ssl, socket}) do
    :ssl.sockname(socket)
  end

  defp maybe_tls_upgrade(pid, :undefined) do
    {:ok, pid}
  end

  defp maybe_tls_upgrade(pid, tLSOptions) do
    try do
      :ssl.start()
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    call(pid, {:open, :tls_upgrade, tLSOptions}, :plain)
  end

  defp start_chunk(r_state(tls_upgrading_data_connection: {true, cTRL, _}) = state) do
    r_state(state, tls_upgrading_data_connection: {true, cTRL, :ftp, :start_chunk, :undefined})
  end

  defp start_chunk(r_state(client: from) = state) do
    :gen_server.reply(from, :ok)
    r_state(state, chunk: true, client: :undefined, caller: :undefined)
  end

  defp start_options(options) do
    case :lists.keysearch(:flags, 1, options) do
      {:value, {:flags, flags}} ->
        verbose = :lists.member(:verbose, flags)
        isTrace = :lists.member(:trace, flags)
        isDebug = :lists.member(:debug, flags)

        debugLevel =
          cond do
            isTrace === true ->
              :trace

            isDebug === true ->
              :debug

            true ->
              :disable
          end

        {:ok, [{:verbose, verbose}, {:debug, debugLevel}, {:priority, :low}]}

      false ->
        validateVerbose = fn
          true ->
            true

          false ->
            true

          _ ->
            false
        end

        validateDebug = fn
          :trace ->
            true

          :debug ->
            true

          :disable ->
            true

          _ ->
            false
        end

        validatePriority = fn
          :low ->
            true

          :normal ->
            true

          :high ->
            true

          _ ->
            false
        end

        validOptions = [
          {:verbose, validateVerbose, false, false},
          {:debug, validateDebug, false, :disable},
          {:priority, validatePriority, false, :low}
        ]

        validate_options(options, validOptions, [])
    end
  end

  defp open_options(options) do
    validateMode = fn
      :active ->
        true

      :passive ->
        true

      _ ->
        false
    end

    validateHost = fn
      host when is_list(host) ->
        true

      host
      when is_tuple(host) and (:erlang.size(host) === 4 or :erlang.size(host) === 8) ->
        true

      _ ->
        false
    end

    validatePort = fn
      port
      when is_integer(port) and port > 0 ->
        true

      _ ->
        false
    end

    validateIpFamily = fn
      :inet ->
        true

      :inet6 ->
        true

      :inet6fb4 ->
        true

      _ ->
        false
    end

    validateTimeout = fn
      timeout
      when is_integer(timeout) and timeout >= 0 ->
        true

      _ ->
        false
    end

    validateDTimeout = fn
      dTimeout
      when is_integer(dTimeout) and dTimeout >= 0 ->
        true

      :infinity ->
        true

      _ ->
        false
    end

    validateProgress = fn
      :ignore ->
        true

      {mod, func, _InitProgress}
      when is_atom(mod) and is_atom(func) ->
        true

      _ ->
        false
    end

    validateFtpExtension = fn
      true ->
        true

      false ->
        true

      _ ->
        false
    end

    validOptions = [
      {:mode, validateMode, false, :passive},
      {:host, validateHost, true, :ehost},
      {:port, validatePort, false, 21},
      {:ipfamily, validateIpFamily, false, :inet},
      {:timeout, validateTimeout, false, 60 * 1000},
      {:dtimeout, validateDTimeout, false, :infinity},
      {:progress, validateProgress, false, :ignore},
      {:ftp_extension, validateFtpExtension, false, false}
    ]

    validate_options(options, validOptions, [])
  end

  defp socket_options(options) do
    ctrlOpts = :proplists.get_value(:sock_ctrl, options, [])
    dataActOpts = :proplists.get_value(:sock_data_act, options, ctrlOpts)
    dataPassOpts = :proplists.get_value(:sock_data_pass, options, ctrlOpts)

    case (for o <- :lists.usort(ctrlOpts ++ dataPassOpts ++ dataActOpts),
              not valid_socket_option(o) do
            o
          end) do
      [] ->
        {:ok, {ctrlOpts, dataPassOpts, dataActOpts}}

      invalid ->
        throw({:error, {:sock_opts, invalid}})
    end
  end

  defp valid_socket_option(:inet) do
    false
  end

  defp valid_socket_option(:inet6) do
    false
  end

  defp valid_socket_option({:ipv6_v6only, _}) do
    false
  end

  defp valid_socket_option({:active, _}) do
    false
  end

  defp valid_socket_option({:packet, _}) do
    false
  end

  defp valid_socket_option({:mode, _}) do
    false
  end

  defp valid_socket_option(:binary) do
    false
  end

  defp valid_socket_option(:list) do
    false
  end

  defp valid_socket_option({:header, _}) do
    false
  end

  defp valid_socket_option({:packet_size, _}) do
    false
  end

  defp valid_socket_option(_) do
    true
  end

  defp tls_options(options) do
    :proplists.get_value(:tls, options, :undefined)
  end

  defp validate_options([], [], acc) do
    {:ok, :lists.reverse(acc)}
  end

  defp validate_options([], validOptions, acc) do
    case (for {key, _, true, reason} <- validOptions do
            {key, reason}
          end) do
      [] ->
        defaults =
          for {key, _, _, default} <- validOptions do
            {key, default}
          end

        {:ok, :lists.reverse(defaults ++ acc)}

      [{_, reason} | _Missing] ->
        throw({:error, reason})
    end
  end

  defp validate_options([{key, value} | options], validOptions, acc) do
    case :lists.keysearch(key, 1, validOptions) do
      {:value, {^key, validate, _, default}} ->
        case (try do
                validate.(value)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          true ->
            newValidOptions = :lists.keydelete(key, 1, validOptions)
            validate_options(options, newValidOptions, [{key, value} | acc])

          _ ->
            newValidOptions = :lists.keydelete(key, 1, validOptions)
            validate_options(options, newValidOptions, [{key, default} | acc])
        end

      false ->
        validate_options(options, validOptions, acc)
    end
  end

  defp validate_options([_ | options], validOptions, acc) do
    validate_options(options, validOptions, acc)
  end

  defp millisec_passed(t0) do
    div(
      :erlang.convert_time_unit(:erlang.monotonic_time() - t0, :native, :micro_seconds),
      1000
    )
  end
end
