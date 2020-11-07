defmodule :m_ssh_sftp do
  use Bitwise
  @behaviour :ssh_client_channel
  require Record

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

  Record.defrecord(:r_ssh_xfer_attr, :ssh_xfer_attr,
    type: :undefined,
    size: :undefined,
    owner: :undefined,
    group: :undefined,
    permissions: :undefined,
    atime: :undefined,
    atime_nseconds: :undefined,
    createtime: :undefined,
    createtime_nseconds: :undefined,
    mtime: :undefined,
    mtime_nseconds: :undefined,
    acl: :undefined,
    attrib_bits: :undefined,
    extensions: :undefined
  )

  Record.defrecord(:r_ssh_xfer_ace, :ssh_xfer_ace,
    type: :undefined,
    flag: :undefined,
    mask: :undefined,
    who: :undefined
  )

  Record.defrecord(:r_ssh_xfer, :ssh_xfer,
    vsn: :undefined,
    ext: :undefined,
    cm: :undefined,
    channel: :undefined
  )

  @behaviour :ssh_dbg
  Record.defrecord(:r_state, :state,
    xf: :undefined,
    rep_buf: <<>>,
    req_id: :undefined,
    req_list: [],
    inf: :undefined,
    opts: :undefined
  )

  Record.defrecord(:r_fileinf, :fileinf,
    handle: :undefined,
    offset: :undefined,
    size: :undefined,
    mode: :undefined
  )

  Record.defrecord(:r_bufinf, :bufinf,
    mode: :undefined,
    crypto_state: :undefined,
    crypto_fun: :undefined,
    size: 0,
    chunksize: :undefined,
    enc_text_buf: <<>>,
    plain_text_buf: <<>>
  )

  def start_channel(cm) when is_pid(cm) do
    start_channel(cm, [])
  end

  def start_channel(socket) when is_port(socket) do
    start_channel(socket, [])
  end

  def start_channel(host) do
    start_channel(host, [])
  end

  def start_channel(socket, userOptions) when is_port(socket) do
    {sshOpts, chanOpts, sftpOpts} = handle_options(userOptions)

    timeout =
      :proplists.get_value(
        :connect_timeout,
        sshOpts,
        :proplists.get_value(:timeout, sftpOpts, :infinity)
      )

    case :ssh.connect(socket, sshOpts, timeout) do
      {:ok, cm} ->
        case start_channel(cm, chanOpts ++ sftpOpts) do
          {:ok, pid} ->
            {:ok, pid, cm}

          error ->
            error
        end

      error ->
        error
    end
  end

  def start_channel(cm, userOptions) when is_pid(cm) do
    timeout = :proplists.get_value(:timeout, userOptions, :infinity)
    {_SshOpts, chanOpts, sftpOpts} = handle_options(userOptions)
    windowSize = :proplists.get_value(:window_size, chanOpts, 20 * 65536)
    packetSize = :proplists.get_value(:packet_size, chanOpts, 65536)

    case :ssh_connection.session_channel(cm, windowSize, packetSize, timeout) do
      {:ok, channelId} ->
        case :ssh_connection_handler.start_channel(
               cm,
               :ssh_sftp,
               channelId,
               [cm, channelId, sftpOpts],
               :undefined
             ) do
          {:ok, pid} ->
            case wait_for_version_negotiation(pid, timeout) do
              :ok ->
                {:ok, pid}

              timeOut ->
                timeOut
            end

          {:error, reason} ->
            {:error, format_channel_start_error(reason)}
        end

      error ->
        error
    end
  end

  def start_channel(host, userOptions) do
    start_channel(host, 22, userOptions)
  end

  def start_channel(host, port, userOptions) do
    {sshOpts, _ChanOpts, _SftpOpts} = handle_options(userOptions)

    timeout =
      case :proplists.get_value(
             :connect_timeout,
             userOptions
           ) do
        :undefined ->
          :proplists.get_value(:timeout, userOptions, :infinity)

        tO ->
          tO
      end

    case :ssh.connect(host, port, sshOpts, timeout) do
      {:ok, cm} ->
        case start_channel(cm, userOptions) do
          {:ok, pid} ->
            {:ok, pid, cm}

          error ->
            error
        end

      {:error, ^timeout} ->
        {:error, :timeout}

      error ->
        error
    end
  end

  defp wait_for_version_negotiation(pid, timeout) do
    call(pid, :wait_for_version_negotiation, timeout)
  end

  def stop_channel(pid) do
    case :erlang.is_process_alive(pid) do
      true ->
        monRef = :erlang.monitor(:process, pid)
        :erlang.unlink(pid)
        :erlang.exit(pid, :ssh_sftp_stop_channel)

        receive do
          {:DOWN, ^monRef, _, _, _} ->
            :ok
        after
          1000 ->
            :erlang.exit(pid, :kill)
            :erlang.demonitor(monRef, [:flush])
            :ok
        end

      false ->
        :ok
    end
  end

  def open(pid, file, mode) do
    open(pid, file, mode, :infinity)
  end

  def open(pid, file, mode, fileOpTimeout) do
    call(pid, {:open, false, file, mode}, fileOpTimeout)
  end

  def open_tar(pid, file, mode) do
    open_tar(pid, file, mode, :infinity)
  end

  def open_tar(pid, file, mode, fileOpTimeout) do
    case {:lists.member(:write, mode), :lists.member(:read, mode), mode -- [:write, :read]} do
      {true, false, []} ->
        {:ok, handle} = open(pid, file, [:write], fileOpTimeout)

        :erl_tar.init(pid, :write, fn
          :write, {_, data} ->
            write_to_remote_tar(pid, handle, to_bin(data), fileOpTimeout)

          :position, {_, pos} ->
            position(pid, handle, pos, fileOpTimeout)

          :close, _ ->
            close(pid, handle, fileOpTimeout)
        end)

      {true, false, [{:crypto, {cryptoInitFun, cryptoEncryptFun, cryptoEndFun}}]} ->
        {:ok, sftpHandle} = open(pid, file, [:write], fileOpTimeout)
        bI = r_bufinf(mode: :write, crypto_fun: cryptoEncryptFun)
        {:ok, bufHandle} = open_buf(pid, cryptoInitFun, bI, fileOpTimeout)

        :erl_tar.init(pid, :write, fn
          :write, {_, data} ->
            write_buf(pid, sftpHandle, bufHandle, to_bin(data), fileOpTimeout)

          :position, {_, pos} ->
            position_buf(pid, sftpHandle, bufHandle, pos, fileOpTimeout)

          :close, _ ->
            {:ok,
             r_bufinf(
               plain_text_buf: plainBuf0,
               enc_text_buf: encBuf0,
               crypto_state: cState0
             )} =
              call(
                pid,
                {:get_bufinf, bufHandle},
                fileOpTimeout
              )

            {:ok, encTextTail} =
              cryptoEndFun.(
                plainBuf0,
                cState0
              )

            encTextBuf = <<encBuf0::binary, encTextTail::binary>>

            case write(pid, sftpHandle, encTextBuf, fileOpTimeout) do
              :ok ->
                call(pid, {:erase_bufinf, bufHandle}, fileOpTimeout)
                close(pid, sftpHandle, fileOpTimeout)

              other ->
                other
            end
        end)

      {false, true, []} ->
        {:ok, handle} = open(pid, file, [:read, :binary], fileOpTimeout)

        :erl_tar.init(pid, :read, fn
          :read2, {_, len} ->
            read_repeat(pid, handle, len, fileOpTimeout)

          :position, {_, pos} ->
            position(pid, handle, pos, fileOpTimeout)

          :close, _ ->
            close(pid, handle, fileOpTimeout)
        end)

      {false, true, [{:crypto, {cryptoInitFun, cryptoDecryptFun}}]} ->
        {:ok, sftpHandle} = open(pid, file, [:read, :binary], fileOpTimeout)
        bI = r_bufinf(mode: :read, crypto_fun: cryptoDecryptFun)
        {:ok, bufHandle} = open_buf(pid, cryptoInitFun, bI, fileOpTimeout)

        :erl_tar.init(pid, :read, fn
          :read2, {_, len} ->
            read_buf(pid, sftpHandle, bufHandle, len, fileOpTimeout)

          :position, {_, pos} ->
            position_buf(pid, sftpHandle, bufHandle, pos, fileOpTimeout)

          :close, _ ->
            call(pid, {:erase_bufinf, bufHandle}, fileOpTimeout)
            close(pid, sftpHandle, fileOpTimeout)
        end)

      _ ->
        {:error, {:illegal_mode, mode}}
    end
  end

  def opendir(pid, path) do
    opendir(pid, path, :infinity)
  end

  def opendir(pid, path, fileOpTimeout) do
    call(pid, {:opendir, false, path}, fileOpTimeout)
  end

  def close(pid, handle) do
    close(pid, handle, :infinity)
  end

  def close(pid, handle, fileOpTimeout) do
    call(pid, {:close, false, handle}, fileOpTimeout)
  end

  def readdir(pid, handle) do
    readdir(pid, handle, :infinity)
  end

  def readdir(pid, handle, fileOpTimeout) do
    call(pid, {:readdir, false, handle}, fileOpTimeout)
  end

  def pread(pid, handle, offset, len) do
    pread(pid, handle, offset, len, :infinity)
  end

  def pread(pid, handle, offset, len, fileOpTimeout) do
    call(pid, {:pread, false, handle, offset, len}, fileOpTimeout)
  end

  def read(pid, handle, len) do
    read(pid, handle, len, :infinity)
  end

  def read(pid, handle, len, fileOpTimeout) do
    call(pid, {:read, false, handle, len}, fileOpTimeout)
  end

  def apread(pid, handle, offset, len) do
    call(pid, {:pread, true, handle, offset, len}, :infinity)
  end

  def aread(pid, handle, len) do
    call(pid, {:read, true, handle, len}, :infinity)
  end

  def pwrite(pid, handle, offset, data) do
    pwrite(pid, handle, offset, data, :infinity)
  end

  def pwrite(pid, handle, offset, data, fileOpTimeout) do
    call(pid, {:pwrite, false, handle, offset, data}, fileOpTimeout)
  end

  def write(pid, handle, data) do
    write(pid, handle, data, :infinity)
  end

  def write(pid, handle, data, fileOpTimeout) do
    call(pid, {:write, false, handle, data}, fileOpTimeout)
  end

  def apwrite(pid, handle, offset, data) do
    call(pid, {:pwrite, true, handle, offset, data}, :infinity)
  end

  def awrite(pid, handle, data) do
    call(pid, {:write, true, handle, data}, :infinity)
  end

  def position(pid, handle, pos) do
    position(pid, handle, pos, :infinity)
  end

  def position(pid, handle, pos, fileOpTimeout) do
    call(pid, {:position, handle, pos}, fileOpTimeout)
  end

  def real_path(pid, path) do
    real_path(pid, path, :infinity)
  end

  def real_path(pid, path, fileOpTimeout) do
    call(pid, {:real_path, false, path}, fileOpTimeout)
  end

  def read_file_info(pid, name) do
    read_file_info(pid, name, :infinity)
  end

  def read_file_info(pid, name, fileOpTimeout) do
    call(pid, {:read_file_info, false, name}, fileOpTimeout)
  end

  def get_file_info(pid, handle) do
    get_file_info(pid, handle, :infinity)
  end

  def get_file_info(pid, handle, fileOpTimeout) do
    call(pid, {:get_file_info, false, handle}, fileOpTimeout)
  end

  def write_file_info(pid, name, info) do
    write_file_info(pid, name, info, :infinity)
  end

  def write_file_info(pid, name, info, fileOpTimeout) do
    call(pid, {:write_file_info, false, name, info}, fileOpTimeout)
  end

  def read_link_info(pid, name) do
    read_link_info(pid, name, :infinity)
  end

  def read_link_info(pid, name, fileOpTimeout) do
    call(pid, {:read_link_info, false, name}, fileOpTimeout)
  end

  def read_link(pid, linkName) do
    read_link(pid, linkName, :infinity)
  end

  def read_link(pid, linkName, fileOpTimeout) do
    case call(pid, {:read_link, false, linkName}, fileOpTimeout) do
      {:ok, [{name, _Attrs}]} ->
        {:ok, name}

      errMsg ->
        errMsg
    end
  end

  def make_symlink(pid, name, target) do
    make_symlink(pid, name, target, :infinity)
  end

  def make_symlink(pid, name, target, fileOpTimeout) do
    call(pid, {:make_symlink, false, name, target}, fileOpTimeout)
  end

  def rename(pid, fromFile, toFile) do
    rename(pid, fromFile, toFile, :infinity)
  end

  def rename(pid, fromFile, toFile, fileOpTimeout) do
    call(pid, {:rename, false, fromFile, toFile}, fileOpTimeout)
  end

  def delete(pid, name) do
    delete(pid, name, :infinity)
  end

  def delete(pid, name, fileOpTimeout) do
    call(pid, {:delete, false, name}, fileOpTimeout)
  end

  def make_dir(pid, name) do
    make_dir(pid, name, :infinity)
  end

  def make_dir(pid, name, fileOpTimeout) do
    call(pid, {:make_dir, false, name}, fileOpTimeout)
  end

  def del_dir(pid, name) do
    del_dir(pid, name, :infinity)
  end

  def del_dir(pid, name, fileOpTimeout) do
    call(pid, {:del_dir, false, name}, fileOpTimeout)
  end

  def send_window(pid) do
    send_window(pid, :infinity)
  end

  def send_window(pid, fileOpTimeout) do
    call(pid, :send_window, fileOpTimeout)
  end

  def recv_window(pid) do
    recv_window(pid, :infinity)
  end

  def recv_window(pid, fileOpTimeout) do
    call(pid, :recv_window, fileOpTimeout)
  end

  def list_dir(pid, name) do
    list_dir(pid, name, :infinity)
  end

  def list_dir(pid, name, fileOpTimeout) do
    case opendir(pid, name, fileOpTimeout) do
      {:ok, handle} ->
        res = do_list_dir(pid, handle, fileOpTimeout, [])
        close(pid, handle, fileOpTimeout)

        case res do
          {:ok, list} ->
            nList =
              :lists.foldl(
                fn {nm, _Info}, acc ->
                  [nm | acc]
                end,
                [],
                list
              )

            {:ok, nList}

          error ->
            error
        end

      error ->
        error
    end
  end

  defp do_list_dir(pid, handle, fileOpTimeout, acc) do
    case readdir(pid, handle, fileOpTimeout) do
      {:ok, []} ->
        {:ok, acc}

      {:ok, names} ->
        do_list_dir(pid, handle, fileOpTimeout, acc ++ names)

      :eof ->
        {:ok, acc}

      error ->
        error
    end
  end

  def read_file(pid, name) do
    read_file(pid, name, :infinity)
  end

  def read_file(pid, name, fileOpTimeout) do
    case open(pid, name, [:read, :binary], fileOpTimeout) do
      {:ok, handle} ->
        {:ok, {_WindowSz, packetSz}} =
          recv_window(
            pid,
            fileOpTimeout
          )

        res = read_file_loop(pid, handle, packetSz, fileOpTimeout, [])
        close(pid, handle)
        res

      error ->
        error
    end
  end

  defp read_file_loop(pid, handle, packetSz, fileOpTimeout, acc) do
    case read(pid, handle, packetSz, fileOpTimeout) do
      {:ok, data} ->
        read_file_loop(pid, handle, packetSz, fileOpTimeout, [data | acc])

      :eof ->
        {:ok, :erlang.list_to_binary(:lists.reverse(acc))}

      error ->
        error
    end
  end

  def write_file(pid, name, list) do
    write_file(pid, name, list, :infinity)
  end

  def write_file(pid, name, list, fileOpTimeout)
      when is_list(list) do
    write_file(pid, name, to_bin(list), fileOpTimeout)
  end

  def write_file(pid, name, bin, fileOpTimeout) do
    case open(pid, name, [:write, :binary], fileOpTimeout) do
      {:ok, handle} ->
        {:ok, {_Window, packet}} =
          send_window(
            pid,
            fileOpTimeout
          )

        res = write_file_loop(pid, handle, 0, bin, :erlang.size(bin), packet, fileOpTimeout)
        close(pid, handle, fileOpTimeout)
        res

      error ->
        error
    end
  end

  defp write_file_loop(_Pid, _Handle, _Pos, _Bin, 0, _PacketSz, _FileOpTimeout) do
    :ok
  end

  defp write_file_loop(pid, handle, pos, bin, remain, packetSz, fileOpTimeout) do
    cond do
      remain >= packetSz ->
        <<_::size(pos)-binary, data::size(packetSz)-binary, _::binary>> = bin

        case write(pid, handle, data, fileOpTimeout) do
          :ok ->
            write_file_loop(
              pid,
              handle,
              pos + packetSz,
              bin,
              remain - packetSz,
              packetSz,
              fileOpTimeout
            )

          error ->
            error
        end

      true ->
        <<_::size(pos)-binary, data::binary>> = bin
        write(pid, handle, data, fileOpTimeout)
    end
  end

  def init([cm, channelId, options]) do
    timeout = :proplists.get_value(:timeout, options, :infinity)
    :erlang.monitor(:process, cm)

    case :ssh_connection.subsystem(cm, channelId, 'sftp', timeout) do
      :success ->
        xf = r_ssh_xfer(cm: cm, channel: channelId)
        {:ok, r_state(xf: xf, req_id: 0, rep_buf: <<>>, inf: new_inf(), opts: options)}

      :failure ->
        {:stop, {:shutdown, 'server failed to start sftp subsystem'}}

      error ->
        {:stop, {:shutdown, error}}
    end
  end

  def handle_call(
        {{:timeout, :infinity}, :wait_for_version_negotiation},
        from,
        r_state(xf: r_ssh_xfer(vsn: :undefined) = xf) = state
      ) do
    {:noreply, r_state(state, xf: r_ssh_xfer(xf, vsn: {:wait, from, :undefined}))}
  end

  def handle_call(
        {{:timeout, timeout}, :wait_for_version_negotiation},
        from,
        r_state(xf: r_ssh_xfer(vsn: :undefined) = xf) = state
      ) do
    tRef = :erlang.send_after(timeout, self(), {:timeout, :undefined, from})
    {:noreply, r_state(state, xf: r_ssh_xfer(xf, vsn: {:wait, from, tRef}))}
  end

  def handle_call({_, :wait_for_version_negotiation}, _, state) do
    {:reply, :ok, state}
  end

  def handle_call({{:timeout, :infinity}, msg}, from, state) do
    do_handle_call(msg, from, state)
  end

  def handle_call({{:timeout, timeout}, msg}, from, r_state(req_id: id) = state) do
    :timer.send_after(timeout, {:timeout, id, from})
    do_handle_call(msg, from, state)
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp do_handle_call({:get_bufinf, bufHandle}, _From, s = r_state(inf: i0)) do
    {:reply, :maps.find(bufHandle, i0), s}
  end

  defp do_handle_call({:put_bufinf, bufHandle, b}, _From, s = r_state(inf: i0)) do
    {:reply, :ok, r_state(s, inf: :maps.put(bufHandle, b, i0))}
  end

  defp do_handle_call({:erase_bufinf, bufHandle}, _From, s = r_state(inf: i0)) do
    {:reply, :ok, r_state(s, inf: :maps.remove(bufHandle, i0))}
  end

  defp do_handle_call({:open, async, fileName, mode}, from, r_state(xf: xF) = state) do
    {access, flags, attrs} = open_mode(r_ssh_xfer(xF, :vsn), mode)
    reqID = r_state(state, :req_id)
    :ssh_xfer.open(xF, reqID, fileName, access, flags, attrs)

    case async do
      true ->
        {:reply, {:async, reqID},
         update_request_info(reqID, state, fn
           {:ok, handle}, state1 ->
             open2(reqID, fileName, handle, mode, async, from, state1)

           rep, state1 ->
             async_reply(reqID, rep, from, state1)
         end)}

      false ->
        {:noreply,
         update_request_info(reqID, state, fn
           {:ok, handle}, state1 ->
             open2(reqID, fileName, handle, mode, async, from, state1)

           rep, state1 ->
             sync_reply(rep, from, state1)
         end)}
    end
  end

  defp do_handle_call({:opendir, async, path}, from, state) do
    reqID = r_state(state, :req_id)
    :ssh_xfer.opendir(r_state(state, :xf), reqID, path)
    make_reply(reqID, async, from, state)
  end

  defp do_handle_call({:readdir, async, handle}, from, state) do
    reqID = r_state(state, :req_id)
    :ssh_xfer.readdir(r_state(state, :xf), reqID, handle)
    make_reply(reqID, async, from, state)
  end

  defp do_handle_call({:close, _Async, handle}, from, state) do
    case get_size(handle, state) do
      :undefined ->
        reqID = r_state(state, :req_id)
        :ssh_xfer.close(r_state(state, :xf), reqID, handle)

        make_reply_post(reqID, false, from, state, fn rep, state1 ->
          {rep, erase_handle(handle, state1)}
        end)

      _ ->
        case lseek_position(handle, :cur, state) do
          {:ok, _} ->
            reqID = r_state(state, :req_id)
            :ssh_xfer.close(r_state(state, :xf), reqID, handle)

            make_reply_post(reqID, false, from, state, fn rep, state1 ->
              {rep, erase_handle(handle, state1)}
            end)

          error ->
            {:reply, error, state}
        end
    end
  end

  defp do_handle_call({:pread, async, handle, at, length}, from, state) do
    case lseek_position(handle, at, state) do
      {:ok, offset} ->
        reqID = r_state(state, :req_id)
        :ssh_xfer.read(r_state(state, :xf), reqID, handle, offset, length)
        state1 = update_offset(handle, offset + length, state)

        make_reply_post(reqID, async, from, state1, fn
          {:ok, data}, state2 ->
            case get_mode(handle, state2) do
              :binary ->
                {{:ok, data}, state2}

              :text ->
                {{:ok, :erlang.binary_to_list(data)}, state2}
            end

          rep, state2 ->
            {rep, state2}
        end)

      error ->
        {:reply, error, state}
    end
  end

  defp do_handle_call({:read, async, handle, length}, from, state) do
    case lseek_position(handle, :cur, state) do
      {:ok, offset} ->
        reqID = r_state(state, :req_id)
        :ssh_xfer.read(r_state(state, :xf), reqID, handle, offset, length)
        state1 = update_offset(handle, offset + length, state)

        make_reply_post(reqID, async, from, state1, fn
          {:ok, data}, state2 ->
            case get_mode(handle, state2) do
              :binary ->
                {{:ok, data}, state2}

              :text ->
                {{:ok, :erlang.binary_to_list(data)}, state2}
            end

          rep, state2 ->
            {rep, state2}
        end)

      error ->
        {:reply, error, state}
    end
  end

  defp do_handle_call({:pwrite, async, handle, at, data0}, from, state) do
    case lseek_position(handle, at, state) do
      {:ok, offset} ->
        data = to_bin(data0)
        reqID = r_state(state, :req_id)
        size = :erlang.size(data)
        :ssh_xfer.write(r_state(state, :xf), reqID, handle, offset, data)
        state1 = update_size(handle, offset + size, state)
        make_reply(reqID, async, from, state1)

      error ->
        {:reply, error, state}
    end
  end

  defp do_handle_call({:write, async, handle, data0}, from, state) do
    case lseek_position(handle, :cur, state) do
      {:ok, offset} ->
        data = to_bin(data0)
        reqID = r_state(state, :req_id)
        size = :erlang.size(data)
        :ssh_xfer.write(r_state(state, :xf), reqID, handle, offset, data)
        state1 = update_offset(handle, offset + size, state)
        make_reply(reqID, async, from, state1)

      error ->
        {:reply, error, state}
    end
  end

  defp do_handle_call({:position, handle, at}, _From, state) do
    case lseek_position(handle, at, state) do
      {:ok, offset} ->
        {:reply, {:ok, offset}, update_offset(handle, offset, state)}

      error ->
        {:reply, error, state}
    end
  end

  defp do_handle_call({:rename, async, fromFile, toFile}, from, state) do
    reqID = r_state(state, :req_id)
    :ssh_xfer.rename(r_state(state, :xf), reqID, fromFile, toFile, [:overwrite])
    make_reply(reqID, async, from, state)
  end

  defp do_handle_call({:delete, async, name}, from, state) do
    reqID = r_state(state, :req_id)
    :ssh_xfer.remove(r_state(state, :xf), reqID, name)
    make_reply(reqID, async, from, state)
  end

  defp do_handle_call({:make_dir, async, name}, from, state) do
    reqID = r_state(state, :req_id)
    :ssh_xfer.mkdir(r_state(state, :xf), reqID, name, r_ssh_xfer_attr(type: :directory))
    make_reply(reqID, async, from, state)
  end

  defp do_handle_call({:del_dir, async, name}, from, state) do
    reqID = r_state(state, :req_id)
    :ssh_xfer.rmdir(r_state(state, :xf), reqID, name)
    make_reply(reqID, async, from, state)
  end

  defp do_handle_call({:real_path, async, name}, from, state) do
    reqID = r_state(state, :req_id)
    :ssh_xfer.realpath(r_state(state, :xf), reqID, name)
    make_reply(reqID, async, from, state)
  end

  defp do_handle_call({:read_file_info, async, name}, from, state) do
    reqID = r_state(state, :req_id)
    :ssh_xfer.stat(r_state(state, :xf), reqID, name, :all)
    make_reply(reqID, async, from, state)
  end

  defp do_handle_call({:get_file_info, async, name}, from, state) do
    reqID = r_state(state, :req_id)
    :ssh_xfer.fstat(r_state(state, :xf), reqID, name, :all)
    make_reply(reqID, async, from, state)
  end

  defp do_handle_call({:read_link_info, async, name}, from, state) do
    reqID = r_state(state, :req_id)
    :ssh_xfer.lstat(r_state(state, :xf), reqID, name, :all)
    make_reply(reqID, async, from, state)
  end

  defp do_handle_call({:read_link, async, name}, from, state) do
    reqID = r_state(state, :req_id)
    :ssh_xfer.readlink(r_state(state, :xf), reqID, name)
    make_reply(reqID, async, from, state)
  end

  defp do_handle_call({:make_symlink, async, path, targetPath}, from, state) do
    reqID = r_state(state, :req_id)
    :ssh_xfer.symlink(r_state(state, :xf), reqID, path, targetPath)
    make_reply(reqID, async, from, state)
  end

  defp do_handle_call({:write_file_info, async, name, info}, from, state) do
    reqID = r_state(state, :req_id)
    a = info_to_attr(info)
    :ssh_xfer.setstat(r_state(state, :xf), reqID, name, a)
    make_reply(reqID, async, from, state)
  end

  defp do_handle_call(:send_window, _From, state) do
    xF = r_state(state, :xf)

    [{:send_window, {{:win_size, size0}, {:packet_size, size1}}}] =
      :ssh.channel_info(
        r_ssh_xfer(xF, :cm),
        r_ssh_xfer(xF, :channel),
        [:send_window]
      )

    {:reply, {:ok, {size0, size1}}, state}
  end

  defp do_handle_call(:recv_window, _From, state) do
    xF = r_state(state, :xf)

    [{:recv_window, {{:win_size, size0}, {:packet_size, size1}}}] =
      :ssh.channel_info(
        r_ssh_xfer(xF, :cm),
        r_ssh_xfer(xF, :channel),
        [:recv_window]
      )

    {:reply, {:ok, {size0, size1}}, state}
  end

  defp do_handle_call(:stop, _From, state) do
    {:stop, :shutdown, :ok, state}
  end

  defp do_handle_call(call, _From, state) do
    {:reply, {:error, :bad_call, call, state}, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, _ConnectionManager, {:data, _ChannelId, 0, data}},
        r_state(rep_buf: data0) = state0
      ) do
    state =
      handle_reply(
        state0,
        <<data0::binary, data::binary>>
      )

    {:ok, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, _ConnectionManager, {:data, _ChannelId, 1, data}},
        state
      ) do
    :error_logger.format('ssh: STDERR: ~s\n', [:erlang.binary_to_list(data)])
    {:ok, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, _ConnectionManager, {:eof, _ChannelId}},
        state
      ) do
    {:ok, state}
  end

  def handle_ssh_msg({:ssh_cm, _, {:signal, _, _}}, state) do
    {:ok, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, _, {:exit_signal, channelId, signal, error0, _}},
        state0
      ) do
    error =
      case error0 do
        '' ->
          signal

        _ ->
          error0
      end

    state = reply_all(state0, {:error, error})
    {:stop, channelId, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, _, {:exit_status, channelId, status}},
        state0
      ) do
    state =
      case state0 do
        0 ->
          state0

        _ ->
          reply_all(state0, {:error, {:exit_status, status}})
      end

    {:stop, channelId, state}
  end

  def handle_msg(
        {:ssh_channel_up, _, _},
        r_state(opts: options, xf: xf) = state
      ) do
    version = :proplists.get_value(:sftp_vsn, options, 6)
    :ssh_xfer.protocol_version_request(xf, version)
    {:ok, state}
  end

  def handle_msg(
        {:timeout, :undefined, from},
        r_state(xf: r_ssh_xfer(channel: channelId)) = state
      ) do
    :ssh_client_channel.reply(from, {:error, :timeout})
    {:stop, channelId, state}
  end

  def handle_msg(
        {:timeout, id, from},
        r_state(req_list: reqList0) = state
      ) do
    case :lists.keysearch(id, 1, reqList0) do
      false ->
        {:ok, state}

      _ ->
        reqList = :lists.keydelete(id, 1, reqList0)
        :ssh_client_channel.reply(from, {:error, :timeout})
        {:ok, r_state(state, req_list: reqList)}
    end
  end

  def handle_msg(
        {:DOWN, _Ref, _Type, _Process, _},
        r_state(xf: r_ssh_xfer(channel: channelId)) = state
      ) do
    {:stop, channelId, state}
  end

  def handle_msg(
        {:EXIT, _, :ssh_sftp_stop_channel},
        r_state(xf: r_ssh_xfer(channel: channelId)) = state
      ) do
    {:stop, channelId, state}
  end

  def handle_msg(_, state) do
    {:ok, state}
  end

  def terminate(:shutdown, r_state(xf: r_ssh_xfer(cm: cm)) = state) do
    reply_all(state, {:error, :closed})
    :ssh.close(cm)
  end

  def terminate(_Reason, state) do
    reply_all(state, {:error, :closed})
  end

  defp handle_options(userOptions) do
    handle_options(userOptions, [], [], [])
  end

  defp handle_options([], sftp, chan, ssh) do
    {ssh, chan, sftp}
  end

  defp handle_options([{:timeout, _} = opt | rest], sftp, chan, ssh) do
    handle_options(rest, [opt | sftp], chan, ssh)
  end

  defp handle_options([{:sftp_vsn, _} = opt | rest], sftp, chan, ssh) do
    handle_options(rest, [opt | sftp], chan, ssh)
  end

  defp handle_options([{:window_size, _} = opt | rest], sftp, chan, ssh) do
    handle_options(rest, sftp, [opt | chan], ssh)
  end

  defp handle_options([{:packet_size, _} = opt | rest], sftp, chan, ssh) do
    handle_options(rest, sftp, [opt | chan], ssh)
  end

  defp handle_options([opt | rest], sftp, chan, ssh) do
    handle_options(rest, sftp, chan, [opt | ssh])
  end

  defp call(pid, msg, timeOut) do
    :ssh_client_channel.call(pid, {{:timeout, timeOut}, msg}, :infinity)
  end

  defp handle_reply(
         state,
         <<len::size(32)-unsigned-big-integer, reply::size(len)-binary, rest::binary>>
       ) do
    do_handle_reply(state, reply, rest)
  end

  defp handle_reply(state, data) do
    r_state(state, rep_buf: data)
  end

  defp do_handle_reply(
         r_state(xf: xf) = state,
         <<2, version::size(32)-unsigned-big-integer, binExt::binary>>,
         rest
       ) do
    ext = :ssh_xfer.decode_ext(binExt)

    case r_ssh_xfer(xf, :vsn) do
      :undefined ->
        :ok

      {:wait, from, tRef} ->
        cond do
          is_reference(tRef) ->
            :erlang.cancel_timer(tRef)

          true ->
            :ok
        end

        :ssh_client_channel.reply(from, :ok)
    end

    r_state(state,
      xf: r_ssh_xfer(xf, vsn: version, ext: ext),
      rep_buf: rest
    )
  end

  defp do_handle_reply(state0, data, rest) do
    case (try do
            :ssh_xfer.xf_reply(r_state(state0, :xf), data)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _Reason} ->
        handle_reply(state0, rest)

      xfReply ->
        state = handle_req_reply(state0, xfReply)
        handle_reply(state, rest)
    end
  end

  defp handle_req_reply(state0, {_, reqID, _} = xfReply) do
    case :lists.keysearch(reqID, 1, r_state(state0, :req_list)) do
      false ->
        state0

      {:value, {_, fun}} ->
        list = :lists.keydelete(reqID, 1, r_state(state0, :req_list))
        state1 = r_state(state0, req_list: list)

        case (try do
                fun.(xreply(xfReply), state1)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            state1

          state ->
            state
        end
    end
  end

  defp xreply({:handle, _, h}) do
    {:ok, h}
  end

  defp xreply({:data, _, data}) do
    {:ok, data}
  end

  defp xreply({:name, _, names}) do
    {:ok, names}
  end

  defp xreply({:attrs, _, a}) do
    {:ok, attr_to_info(a)}
  end

  defp xreply({:extended_reply, _, x}) do
    {:ok, x}
  end

  defp xreply({:status, _, {:ok, _Err, _Lang, _Rep}}) do
    :ok
  end

  defp xreply({:status, _, {:eof, _Err, _Lang, _Rep}}) do
    :eof
  end

  defp xreply({:status, _, {stat, _Err, _Lang, _Rep}}) do
    {:error, stat}
  end

  defp xreply({code, _, reply}) do
    {code, reply}
  end

  defp update_request_info(reqID, state, fun) do
    list = [{reqID, fun} | r_state(state, :req_list)]
    iD = r_state(state, :req_id) + 1 &&& 4_294_967_295
    r_state(state, req_list: list, req_id: iD)
  end

  defp async_reply(reqID, reply, _From = {to, _}, state) do
    send(to, {:async_reply, reqID, reply})
    state
  end

  defp sync_reply(reply, from, state) do
    try do
      :ssh_client_channel.reply(from, reply)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    state
  end

  defp open2(origReqID, fileName, handle, mode, async, from, state) do
    i0 = r_state(state, :inf)

    fileMode =
      case :lists.member(
             :binary,
             mode
           ) or :lists.member(:raw, mode) do
        true ->
          :binary

        false ->
          :text
      end

    i1 = add_new_handle(handle, fileMode, i0)
    state0 = r_state(state, inf: i1)
    reqID = r_state(state0, :req_id)
    :ssh_xfer.stat(r_state(state0, :xf), reqID, fileName, [:size])

    case async do
      true ->
        update_request_info(reqID, state0, fn
          {:ok, fI}, state1 ->
            size = r_file_info(fI, :size)

            state2 =
              cond do
                is_integer(size) ->
                  put_size(handle, size, state1)

                true ->
                  state1
              end

            async_reply(origReqID, {:ok, handle}, from, state2)

          _, state1 ->
            async_reply(origReqID, {:ok, handle}, from, state1)
        end)

      false ->
        update_request_info(reqID, state0, fn
          {:ok, fI}, state1 ->
            size = r_file_info(fI, :size)

            state2 =
              cond do
                is_integer(size) ->
                  put_size(handle, size, state1)

                true ->
                  state1
              end

            sync_reply({:ok, handle}, from, state2)

          _, state1 ->
            sync_reply({:ok, handle}, from, state1)
        end)
    end
  end

  defp reply_all(state, reply) do
    list = r_state(state, :req_list)

    :lists.foreach(
      fn {_ReqID, fun} ->
        try do
          fun.(reply, state)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end
      end,
      list
    )

    r_state(state, req_list: [])
  end

  defp make_reply(reqID, true, from, state) do
    {:reply, {:async, reqID},
     update_request_info(reqID, state, fn reply, state1 ->
       async_reply(reqID, reply, from, state1)
     end)}
  end

  defp make_reply(reqID, false, from, state) do
    {:noreply,
     update_request_info(reqID, state, fn reply, state1 ->
       sync_reply(reply, from, state1)
     end)}
  end

  defp make_reply_post(reqID, true, from, state, postFun) do
    {:reply, {:async, reqID},
     update_request_info(reqID, state, fn reply, state1 ->
       case (try do
               postFun.(reply, state1)
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end) do
         {:EXIT, _} ->
           async_reply(reqID, reply, from, state1)

         {reply1, state2} ->
           async_reply(reqID, reply1, from, state2)
       end
     end)}
  end

  defp make_reply_post(reqID, false, from, state, postFun) do
    {:noreply,
     update_request_info(reqID, state, fn reply, state1 ->
       case (try do
               postFun.(reply, state1)
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end) do
         {:EXIT, _} ->
           sync_reply(reply, from, state1)

         {reply1, state2} ->
           sync_reply(reply1, from, state2)
       end
     end)}
  end

  def info_to_attr(i) when elem(i, 0) === :file_info do
    r_ssh_xfer_attr(
      permissions: r_file_info(i, :mode),
      size: r_file_info(i, :size),
      type: r_file_info(i, :type),
      owner: r_file_info(i, :uid),
      group: r_file_info(i, :gid),
      atime: datetime_to_unix(r_file_info(i, :atime)),
      mtime: datetime_to_unix(r_file_info(i, :mtime)),
      createtime: datetime_to_unix(r_file_info(i, :ctime))
    )
  end

  def attr_to_info(a) when elem(a, 0) === :ssh_xfer_attr do
    r_file_info(
      size: r_ssh_xfer_attr(a, :size),
      type: r_ssh_xfer_attr(a, :type),
      access: file_mode_to_owner_access(r_ssh_xfer_attr(a, :permissions)),
      atime: unix_to_datetime(r_ssh_xfer_attr(a, :atime)),
      mtime: unix_to_datetime(r_ssh_xfer_attr(a, :mtime)),
      ctime: unix_to_datetime(r_ssh_xfer_attr(a, :createtime)),
      mode: r_ssh_xfer_attr(a, :permissions),
      links: 1,
      major_device: 0,
      minor_device: 0,
      inode: 0,
      uid: r_ssh_xfer_attr(a, :owner),
      gid: r_ssh_xfer_attr(a, :group)
    )
  end

  defp file_mode_to_owner_access(fileMode) when is_integer(fileMode) do
    readPermission = fileMode >>> 8 &&& 1
    writePermission = fileMode >>> 7 &&& 1

    case {readPermission, writePermission} do
      {1, 1} ->
        :read_write

      {1, 0} ->
        :read

      {0, 1} ->
        :write

      {0, 0} ->
        :none

      _ ->
        :undefined
    end
  end

  defp file_mode_to_owner_access(_) do
    :undefined
  end

  defp unix_to_datetime(:undefined) do
    :undefined
  end

  defp unix_to_datetime(uTCSecs) do
    uTCDateTime = :calendar.gregorian_seconds_to_datetime(uTCSecs + 62_167_219_200)
    :erlang.universaltime_to_localtime(uTCDateTime)
  end

  defp datetime_to_unix(:undefined) do
    :undefined
  end

  defp datetime_to_unix(localDateTime) do
    uTCDateTime = :erlang.localtime_to_universaltime(localDateTime)
    :calendar.datetime_to_gregorian_seconds(uTCDateTime) - 62_167_219_200
  end

  defp open_mode(vsn, modes) when vsn >= 5 do
    open_mode5(modes)
  end

  defp open_mode(_Vsn, modes) do
    open_mode3(modes)
  end

  defp open_mode5(modes) do
    a = r_ssh_xfer_attr(type: :regular)

    {fl, ac} =
      case {:lists.member(:write, modes), :lists.member(:read, modes),
            :lists.member(:append, modes)} do
        {_, _, true} ->
          {[:append_data], [:read_attributes, :append_data, :write_attributes]}

        {true, false, false} ->
          {[:create_truncate], [:write_data, :write_attributes]}

        {true, true, _} ->
          {[:open_or_create], [:read_data, :read_attributes, :write_data, :write_attributes]}

        {false, true, _} ->
          {[:open_existing], [:read_data, :read_attributes]}
      end

    {ac, fl, a}
  end

  defp open_mode3(modes) do
    a = r_ssh_xfer_attr(type: :regular)

    fl =
      case {:lists.member(:write, modes), :lists.member(:read, modes),
            :lists.member(:append, modes)} do
        {_, _, true} ->
          [:append]

        {true, false, false} ->
          [:write, :creat, :trunc]

        {true, true, _} ->
          [:read, :write]

        {false, true, _} ->
          [:read]
      end

    {[], fl, a}
  end

  defp new_inf() do
    %{}
  end

  defp add_new_handle(handle, fileMode, inf) do
    :maps.put(handle, r_fileinf(offset: 0, size: 0, mode: fileMode), inf)
  end

  defp update_size(handle, newSize, state) do
    oldSize = get_size(handle, state)

    cond do
      newSize > oldSize ->
        put_size(handle, newSize, state)

      true ->
        state
    end
  end

  defp update_offset(handle, newOffset, state0) do
    state1 = put_offset(handle, newOffset, state0)
    update_size(handle, newOffset, state1)
  end

  defp put_size(handle, size, state) do
    inf0 = r_state(state, :inf)

    case :maps.find(handle, inf0) do
      {:ok, fI} ->
        r_state(state, inf: :maps.put(handle, r_fileinf(fI, size: size), inf0))

      _ ->
        r_state(state, inf: :maps.put(handle, r_fileinf(size: size, offset: 0), inf0))
    end
  end

  defp put_offset(handle, offset, state) do
    inf0 = r_state(state, :inf)

    case :maps.find(handle, inf0) do
      {:ok, fI} ->
        r_state(state, inf: :maps.put(handle, r_fileinf(fI, offset: offset), inf0))

      _ ->
        r_state(state, inf: :maps.put(handle, r_fileinf(size: offset, offset: offset), inf0))
    end
  end

  defp get_size(handle, state) do
    case :maps.find(handle, r_state(state, :inf)) do
      {:ok, fI} ->
        r_fileinf(fI, :size)

      _ ->
        :undefined
    end
  end

  defp get_mode(handle, state) do
    case :maps.find(handle, r_state(state, :inf)) do
      {:ok, fI} ->
        r_fileinf(fI, :mode)

      _ ->
        :undefined
    end
  end

  defp erase_handle(handle, state) do
    fI = :maps.remove(handle, r_state(state, :inf))
    r_state(state, inf: fI)
  end

  defp lseek_position(handle, pos, state) do
    case :maps.find(handle, r_state(state, :inf)) do
      {:ok, r_fileinf(offset: o, size: s)} ->
        lseek_pos(pos, o, s)

      _ ->
        {:error, :einval}
    end
  end

  defp lseek_pos(_Pos, :undefined, _) do
    {:error, :einval}
  end

  defp lseek_pos(pos, _CurOffset, _CurSize)
       when is_integer(pos) and 0 <= pos and pos < 1 <<< 63 do
    {:ok, pos}
  end

  defp lseek_pos(:bof, _CurOffset, _CurSize) do
    {:ok, 0}
  end

  defp lseek_pos(:cur, curOffset, _CurSize) do
    {:ok, curOffset}
  end

  defp lseek_pos(:eof, _CurOffset, curSize) do
    {:ok, curSize}
  end

  defp lseek_pos({:bof, offset}, _CurOffset, _CurSize)
       when is_integer(offset) and 0 <= offset and offset < 1 <<< 63 do
    {:ok, offset}
  end

  defp lseek_pos({:cur, offset}, curOffset, _CurSize)
       when is_integer(offset) and -(1 <<< 63) <= offset and offset < 1 <<< 63 do
    newOffset = curOffset + offset

    cond do
      newOffset < 0 ->
        {:ok, 0}

      true ->
        {:ok, newOffset}
    end
  end

  defp lseek_pos({:eof, offset}, _CurOffset, curSize)
       when is_integer(offset) and -(1 <<< 63) <= offset and offset < 1 <<< 63 do
    newOffset = curSize + offset

    cond do
      newOffset < 0 ->
        {:ok, 0}

      true ->
        {:ok, newOffset}
    end
  end

  defp lseek_pos(_, _, _) do
    {:error, :einval}
  end

  defp to_bin(data) when is_list(data) do
    try do
      :erlang.iolist_to_binary(data)
    catch
      _, _ ->
        :unicode.characters_to_binary(data)
    end
  end

  defp to_bin(data) when is_binary(data) do
    data
  end

  defp read_repeat(pid, handle, len, fileOpTimeout) do
    {:ok, {_WindowSz, packetSz}} =
      recv_window(
        pid,
        fileOpTimeout
      )

    read_rpt(pid, handle, len, packetSz, fileOpTimeout, <<>>)
  end

  defp read_rpt(pid, handle, wantedLen, packetSz, fileOpTimeout, acc)
       when wantedLen > 0 do
    case read(pid, handle, min(wantedLen, packetSz), fileOpTimeout) do
      {:ok, data} ->
        read_rpt(
          pid,
          handle,
          wantedLen - :erlang.size(data),
          packetSz,
          fileOpTimeout,
          <<acc::binary, data::binary>>
        )

      :eof ->
        {:ok, acc}

      error ->
        error
    end
  end

  defp read_rpt(_Pid, _Handle, wantedLen, _PacketSz, _FileOpTimeout, acc)
       when wantedLen >= 0 do
    {:ok, acc}
  end

  defp write_to_remote_tar(_Pid, _SftpHandle, <<>>, _FileOpTimeout) do
    :ok
  end

  defp write_to_remote_tar(pid, sftpHandle, bin, fileOpTimeout) do
    {:ok, {_Window, packet}} =
      send_window(
        pid,
        fileOpTimeout
      )

    write_file_loop(pid, sftpHandle, 0, bin, :erlang.size(bin), packet, fileOpTimeout)
  end

  defp position_buf(pid, sftpHandle, bufHandle, pos, fileOpTimeout) do
    {:ok, r_bufinf(mode: mode, plain_text_buf: buf0, size: size)} =
      call(pid, {:get_bufinf, bufHandle}, fileOpTimeout)

    case pos do
      {:cur, 0} when mode == :write ->
        {:ok, size + :erlang.size(buf0)}

      {:cur, 0} when mode == :read ->
        {:ok, size}

      _ when mode == :read and is_integer(pos) ->
        skip = pos - size

        cond do
          skip < 0 ->
            {:error, :cannot_rewind}

          skip == 0 ->
            {:ok, pos}

          skip > 0 ->
            case read_buf(pid, sftpHandle, bufHandle, skip, fileOpTimeout) do
              {:ok, _} ->
                {:ok, pos}

              other ->
                other
            end
        end

      _ ->
        {:error, {:not_yet_implemented, {:pos, pos}}}
    end
  end

  defp read_buf(pid, sftpHandle, bufHandle, wantedLen, fileOpTimeout) do
    {:ok, {_Window, packet}} =
      send_window(
        pid,
        fileOpTimeout
      )

    {:ok, b0} = call(pid, {:get_bufinf, bufHandle}, fileOpTimeout)

    case do_the_read_buf(pid, sftpHandle, wantedLen, packet, fileOpTimeout, b0) do
      {:ok, resultBin, b} ->
        call(pid, {:put_bufinf, bufHandle, b}, fileOpTimeout)
        {:ok, resultBin}

      {:error, error} ->
        {:error, error}

      {:eof, b} ->
        call(pid, {:put_bufinf, bufHandle, b}, fileOpTimeout)
        :eof
    end
  end

  defp do_the_read_buf(
         _Pid,
         _SftpHandle,
         wantedLen,
         _Packet,
         _FileOpTimeout,
         b = r_bufinf(plain_text_buf: plainBuf0, size: size)
       )
       when :erlang.size(plainBuf0) >= wantedLen do
    <<resultBin::size(wantedLen)-binary, plainBuf::binary>> = plainBuf0
    {:ok, resultBin, r_bufinf(b, plain_text_buf: plainBuf, size: size + wantedLen)}
  end

  defp do_the_read_buf(
         pid,
         sftpHandle,
         wantedLen,
         packet,
         fileOpTimeout,
         b0 = r_bufinf(plain_text_buf: plainBuf0, enc_text_buf: encBuf0, chunksize: :undefined)
       )
       when :erlang.size(encBuf0) > 0 do
    {:ok, decodedBin, b} = apply_crypto(encBuf0, b0)

    do_the_read_buf(
      pid,
      sftpHandle,
      wantedLen,
      packet,
      fileOpTimeout,
      r_bufinf(b,
        plain_text_buf: <<plainBuf0::binary, decodedBin::binary>>,
        enc_text_buf: <<>>
      )
    )
  end

  defp do_the_read_buf(
         pid,
         sftpHandle,
         wantedLen,
         packet,
         fileOpTimeout,
         b0 = r_bufinf(plain_text_buf: plainBuf0, enc_text_buf: encBuf0, chunksize: chunkSize0)
       )
       when :erlang.size(encBuf0) >= chunkSize0 do
    <<toDecode::size(chunkSize0)-binary, encBuf::binary>> = encBuf0
    {:ok, decodedBin, b} = apply_crypto(toDecode, b0)

    do_the_read_buf(
      pid,
      sftpHandle,
      wantedLen,
      packet,
      fileOpTimeout,
      r_bufinf(b,
        plain_text_buf: <<plainBuf0::binary, decodedBin::binary>>,
        enc_text_buf: encBuf
      )
    )
  end

  defp do_the_read_buf(
         pid,
         sftpHandle,
         wantedLen,
         packet,
         fileOpTimeout,
         b = r_bufinf(enc_text_buf: encBuf0)
       ) do
    case read(pid, sftpHandle, packet, fileOpTimeout) do
      {:ok, encryptedBin} ->
        do_the_read_buf(
          pid,
          sftpHandle,
          wantedLen,
          packet,
          fileOpTimeout,
          r_bufinf(b, enc_text_buf: <<encBuf0::binary, encryptedBin::binary>>)
        )

      :eof ->
        {:eof, b}

      other ->
        other
    end
  end

  defp write_buf(pid, sftpHandle, bufHandle, plainBin, fileOpTimeout) do
    {:ok, {_Window, packet}} =
      send_window(
        pid,
        fileOpTimeout
      )

    {:ok, b0 = r_bufinf(plain_text_buf: pTB)} =
      call(
        pid,
        {:get_bufinf, bufHandle},
        fileOpTimeout
      )

    case do_the_write_buf(
           pid,
           sftpHandle,
           packet,
           fileOpTimeout,
           r_bufinf(b0, plain_text_buf: <<pTB::binary, plainBin::binary>>)
         ) do
      {:ok, b} ->
        call(pid, {:put_bufinf, bufHandle, b}, fileOpTimeout)
        :ok

      {:error, error} ->
        {:error, error}
    end
  end

  defp do_the_write_buf(
         pid,
         sftpHandle,
         packet,
         fileOpTimeout,
         b = r_bufinf(enc_text_buf: encBuf0, size: size)
       )
       when :erlang.size(encBuf0) >= packet do
    <<binToWrite::size(packet)-binary, encBuf::binary>> = encBuf0

    case write(pid, sftpHandle, binToWrite, fileOpTimeout) do
      :ok ->
        do_the_write_buf(
          pid,
          sftpHandle,
          packet,
          fileOpTimeout,
          r_bufinf(b, enc_text_buf: encBuf, size: size + packet)
        )

      other ->
        other
    end
  end

  defp do_the_write_buf(
         pid,
         sftpHandle,
         packet,
         fileOpTimeout,
         b0 = r_bufinf(plain_text_buf: plainBuf0, enc_text_buf: encBuf0, chunksize: :undefined)
       )
       when :erlang.size(plainBuf0) > 0 do
    {:ok, encodedBin, b} = apply_crypto(plainBuf0, b0)

    do_the_write_buf(
      pid,
      sftpHandle,
      packet,
      fileOpTimeout,
      r_bufinf(b,
        plain_text_buf: <<>>,
        enc_text_buf: <<encBuf0::binary, encodedBin::binary>>
      )
    )
  end

  defp do_the_write_buf(
         pid,
         sftpHandle,
         packet,
         fileOpTimeout,
         b0 = r_bufinf(plain_text_buf: plainBuf0, enc_text_buf: encBuf0, chunksize: chunkSize0)
       )
       when :erlang.size(plainBuf0) >= chunkSize0 do
    <<toEncode::size(chunkSize0)-binary, plainBuf::binary>> = plainBuf0
    {:ok, encodedBin, b} = apply_crypto(toEncode, b0)

    do_the_write_buf(
      pid,
      sftpHandle,
      packet,
      fileOpTimeout,
      r_bufinf(b,
        plain_text_buf: plainBuf,
        enc_text_buf: <<encBuf0::binary, encodedBin::binary>>
      )
    )
  end

  defp do_the_write_buf(_Pid, _SftpHandle, _Packet, _FileOpTimeout, b) do
    {:ok, b}
  end

  defp apply_crypto(
         in__,
         b = r_bufinf(crypto_state: cState0, crypto_fun: f)
       ) do
    case f.(in__, cState0) do
      {:ok, encodedBin, cState} ->
        {:ok, encodedBin, r_bufinf(b, crypto_state: cState)}

      {:ok, encodedBin, cState, chunkSize} ->
        {:ok, encodedBin, r_bufinf(b, crypto_state: cState, chunksize: chunkSize)}
    end
  end

  defp open_buf(pid, cryptoInitFun, bufInfo0, fileOpTimeout) do
    case cryptoInitFun.() do
      {:ok, cryptoState} ->
        open_buf1(pid, bufInfo0, fileOpTimeout, cryptoState, :undefined)

      {:ok, cryptoState, chunkSize} ->
        open_buf1(pid, bufInfo0, fileOpTimeout, cryptoState, chunkSize)

      other ->
        other
    end
  end

  defp open_buf1(pid, bufInfo0, fileOpTimeout, cryptoState, chunkSize) do
    bufInfo =
      r_bufinf(bufInfo0,
        crypto_state: cryptoState,
        chunksize: chunkSize
      )

    bufHandle = make_ref()
    call(pid, {:put_bufinf, bufHandle, bufInfo}, fileOpTimeout)
    {:ok, bufHandle}
  end

  defp format_channel_start_error({:shutdown, reason}) do
    reason
  end

  defp format_channel_start_error(reason) do
    reason
  end

  def ssh_dbg_trace_points() do
    [:terminate]
  end

  def ssh_dbg_flags(:terminate) do
    [:c]
  end

  def ssh_dbg_on(:terminate) do
    :dbg.tp(:ssh_sftp, :terminate, 2, :x)
  end

  def ssh_dbg_off(:terminate) do
    :dbg.ctpg(:ssh_sftp, :terminate, 2)
  end

  def ssh_dbg_format(
        :terminate,
        {:call, {:ssh_sftp, :terminate, [reason, state]}}
      ) do
    ['Sftp Terminating:\n', :io_lib.format('Reason: ~p,~nState:~n~s', [reason, wr_record(state)])]
  end

  def ssh_dbg_format(
        :terminate,
        {:return_from, {:ssh_sftp, :terminate, 2}, _Ret}
      ) do
    :skip
  end

  defp wr_record(r = r_state()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_state(r_state())), [])
  end
end
