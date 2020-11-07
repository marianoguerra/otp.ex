defmodule :m_ct_ftp do
  use Bitwise
  import Kernel, except: [send: 2]
  require Record

  Record.defrecord(:r_conn, :conn,
    handle: :undefined,
    targetref: :undefined,
    address: :undefined,
    callback: :undefined
  )

  Record.defrecord(:r_testspec, :testspec,
    spec_dir: :undefined,
    nodes: [],
    init: [],
    label: [],
    profile: [],
    logdir: ['.'],
    logopts: [],
    basic_html: [],
    esc_chars: [],
    verbosity: [],
    silent_connections: [],
    cover: [],
    cover_stop: [],
    config: [],
    userconfig: [],
    event_handler: [],
    ct_hooks: [],
    enable_builtin_hooks: true,
    release_shell: false,
    include: [],
    auto_compile: [],
    abort_if_missing_suites: [],
    stylesheet: [],
    multiply_timetraps: [],
    scale_timetraps: [],
    create_priv_dir: [],
    alias: [],
    tests: [],
    unknown: [],
    merge_tests: true
  )

  Record.defrecord(:r_cover, :cover,
    app: :none,
    local_only: false,
    level: :details,
    excl_mods: [],
    incl_mods: [],
    cross: [],
    src: []
  )

  Record.defrecord(:r_conn_log, :conn_log,
    header: true,
    client: :undefined,
    name: :undefined,
    address: :undefined,
    conn_pid: :undefined,
    action: :undefined,
    module: :undefined
  )

  Record.defrecord(:r_state, :state,
    ftp_pid: :undefined,
    target_name: :undefined
  )

  def put(keyOrName, localFile, remoteFile) do
    fun = fn ftp ->
      send(ftp, localFile, remoteFile)
    end

    open_and_do(keyOrName, fun)
  end

  def get(keyOrName, remoteFile, localFile) do
    fun = fn ftp ->
      recv(ftp, remoteFile, localFile)
    end

    open_and_do(keyOrName, fun)
  end

  def open(keyOrName) do
    case :ct_util.get_key_from_name(keyOrName) do
      {:ok, :node} ->
        open(keyOrName, 'erlang', 'x')

      _ ->
        case :ct.get_config(keyOrName) do
          :undefined ->
            log(heading(:open, keyOrName), 'Failed: ~tp\n', [{:not_available, keyOrName}])
            {:error, {:not_available, keyOrName}}

          _ ->
            case :ct.get_config({keyOrName, :username}) do
              :undefined ->
                log(heading(:open, keyOrName), 'Failed: ~tp\n', [
                  {:not_available, {keyOrName, :username}}
                ])

                {:error, {:not_available, {keyOrName, :username}}}

              username ->
                case :ct.get_config({keyOrName, :password}) do
                  :undefined ->
                    log(heading(:open, keyOrName), 'Failed: ~tp\n', [
                      {:not_available, {keyOrName, :password}}
                    ])

                    {:error, {:not_available, {keyOrName, :password}}}

                  password ->
                    open(keyOrName, username, password)
                end
            end
        end
    end
  end

  defp open(keyOrName, username, password) do
    log(heading(:open, keyOrName), '', [])

    case :ct.get_config({keyOrName, :ftp}) do
      :undefined ->
        log(heading(:open, keyOrName), 'Failed: ~tp\n', [{:not_available, {keyOrName, :ftp}}])
        {:error, {:not_available, {keyOrName, :ftp}}}

      addr ->
        :ct_gen_conn.start(keyOrName, full_addr(addr), {username, password}, :ct_ftp)
    end
  end

  def send(connection, localFile) do
    send(connection, localFile, :filename.basename(localFile))
  end

  def send(connection, localFile, remoteFile) do
    case get_handle(connection) do
      {:ok, pid} ->
        call(pid, {:send, localFile, remoteFile})

      error ->
        error
    end
  end

  def recv(connection, remoteFile) do
    recv(connection, remoteFile, :filename.basename(remoteFile))
  end

  def recv(connection, remoteFile, localFile) do
    case get_handle(connection) do
      {:ok, pid} ->
        call(pid, {:recv, remoteFile, localFile})

      error ->
        error
    end
  end

  def cd(connection, dir) do
    case get_handle(connection) do
      {:ok, pid} ->
        call(pid, {:cd, dir})

      error ->
        error
    end
  end

  def ls(connection, dir) do
    case get_handle(connection) do
      {:ok, pid} ->
        call(pid, {:ls, dir})

      error ->
        error
    end
  end

  def type(connection, type) do
    case get_handle(connection) do
      {:ok, pid} ->
        call(pid, {:type, type})

      error ->
        error
    end
  end

  def delete(connection, file) do
    case get_handle(connection) do
      {:ok, pid} ->
        call(pid, {:delete, file})

      error ->
        error
    end
  end

  def close(connection) do
    case get_handle(connection) do
      {:ok, pid} ->
        :ct_gen_conn.stop(pid)

      error ->
        error
    end
  end

  def init(keyOrName, {iP, port}, {username, password}) do
    case ftp_connect(iP, port, username, password) do
      {:ok, ftpPid} ->
        log(
          heading(:init, keyOrName),
          'Opened ftp connection:\nIP: ~tp\nUsername: ~tp\nPassword: ~p\n',
          [iP, username, :lists.duplicate(:string.length(password), ?*)]
        )

        {:ok, ftpPid, r_state(ftp_pid: ftpPid, target_name: keyOrName)}

      error ->
        error
    end
  end

  defp ftp_connect(iP, port, username, password) do
    _ = :ftp.start()

    case :ftp.start_service([{:host, iP}, {:port, port}]) do
      {:ok, ftpPid} ->
        case :ftp.user(ftpPid, username, password) do
          :ok ->
            {:ok, ftpPid}

          {:error, reason} ->
            {:error, {:user, reason}}
        end

      {:error, reason} ->
        {:error, {:open, reason}}
    end
  end

  def handle_msg({:send, localFile, remoteFile}, state) do
    log(heading(:send, r_state(state, :target_name)), 'LocalFile: ~tp\nRemoteFile: ~tp\n', [
      localFile,
      remoteFile
    ])

    result = :ftp.send(r_state(state, :ftp_pid), localFile, remoteFile)
    {result, state}
  end

  def handle_msg({:recv, remoteFile, localFile}, state) do
    log(heading(:recv, r_state(state, :target_name)), 'RemoteFile: ~tp\nLocalFile: ~tp\n', [
      remoteFile,
      localFile
    ])

    result = :ftp.recv(r_state(state, :ftp_pid), remoteFile, localFile)
    {result, state}
  end

  def handle_msg({:cd, dir}, state) do
    log(heading(:cd, r_state(state, :target_name)), 'Dir: ~tp\n', [dir])
    result = :ftp.cd(r_state(state, :ftp_pid), dir)
    {result, state}
  end

  def handle_msg({:ls, dir}, state) do
    log(heading(:ls, r_state(state, :target_name)), 'Dir: ~tp\n', [dir])
    result = :ftp.ls(r_state(state, :ftp_pid), dir)
    {result, state}
  end

  def handle_msg({:type, type}, state) do
    log(heading(:type, r_state(state, :target_name)), 'Type: ~tp\n', [type])
    result = :ftp.type(r_state(state, :ftp_pid), type)
    {result, state}
  end

  def handle_msg({:delete, file}, state) do
    log(heading(:delete, r_state(state, :target_name)), 'Delete file: ~tp\n', [file])
    result = :ftp.delete(r_state(state, :ftp_pid), file)
    {result, state}
  end

  def reconnect(_Addr, _State) do
    {:error, :no_reconnection_of_ftp}
  end

  def terminate(ftpPid, state) do
    log(
      heading(:terminate, r_state(state, :target_name)),
      'Closing FTP connection.\nHandle: ~p\n',
      [ftpPid]
    )

    :ftp.stop_service(ftpPid)
  end

  defp get_handle(pid) when is_pid(pid) do
    {:ok, pid}
  end

  defp get_handle(name) do
    case :ct_util.get_connection(name, :ct_ftp) do
      {:ok, {pid, _}} ->
        {:ok, pid}

      {:error, :no_registered_connection} ->
        open(name)

      error ->
        error
    end
  end

  defp full_addr({ip, port}) do
    {ip, port}
  end

  defp full_addr(ip) do
    {ip, 21}
  end

  defp call(pid, msg) do
    :ct_gen_conn.call(pid, msg)
  end

  defp heading(function, name) do
    :io_lib.format('ct_ftp:~tw ~tp', [function, name])
  end

  defp log(heading, str, args) do
    :ct_gen_conn.log(heading, str, args)
  end

  defp open_and_do(name, fun) do
    case open(name) do
      {:ok, ftp} ->
        r = fun.(ftp)
        close(ftp)
        r

      error ->
        error
    end
  end
end
