defmodule :m_ssh_sftpd do
  use Bitwise
  @behaviour :ssh_server_channel
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

  Record.defrecord(:r_ssh_msg_global_request, :ssh_msg_global_request,
    name: :undefined,
    want_reply: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_request_success, :ssh_msg_request_success, data: :undefined)
  Record.defrecord(:r_ssh_msg_request_failure, :ssh_msg_request_failure, [])

  Record.defrecord(:r_ssh_msg_channel_open, :ssh_msg_channel_open,
    channel_type: :undefined,
    sender_channel: :undefined,
    initial_window_size: :undefined,
    maximum_packet_size: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_open_confirmation, :ssh_msg_channel_open_confirmation,
    recipient_channel: :undefined,
    sender_channel: :undefined,
    initial_window_size: :undefined,
    maximum_packet_size: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_open_failure, :ssh_msg_channel_open_failure,
    recipient_channel: :undefined,
    reason: :undefined,
    description: :undefined,
    lang: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_window_adjust, :ssh_msg_channel_window_adjust,
    recipient_channel: :undefined,
    bytes_to_add: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_data, :ssh_msg_channel_data,
    recipient_channel: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_extended_data, :ssh_msg_channel_extended_data,
    recipient_channel: :undefined,
    data_type_code: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_eof, :ssh_msg_channel_eof, recipient_channel: :undefined)

  Record.defrecord(:r_ssh_msg_channel_close, :ssh_msg_channel_close, recipient_channel: :undefined)

  Record.defrecord(:r_ssh_msg_channel_request, :ssh_msg_channel_request,
    recipient_channel: :undefined,
    request_type: :undefined,
    want_reply: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_success, :ssh_msg_channel_success,
    recipient_channel: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_failure, :ssh_msg_channel_failure,
    recipient_channel: :undefined
  )

  Record.defrecord(:r_channel, :channel,
    type: :undefined,
    sys: :undefined,
    user: :undefined,
    flow_control: :undefined,
    local_id: :undefined,
    recv_window_size: :undefined,
    recv_window_pending: 0,
    recv_packet_size: :undefined,
    recv_close: false,
    remote_id: :undefined,
    send_window_size: :undefined,
    send_packet_size: :undefined,
    sent_close: false,
    send_buf: []
  )

  Record.defrecord(:r_connection, :connection,
    requests: [],
    channel_cache: :undefined,
    channel_id_seed: :undefined,
    cli_spec: :undefined,
    options: :undefined,
    exec: :undefined,
    system_supervisor: :undefined,
    sub_system_supervisor: :undefined,
    connection_supervisor: :undefined
  )

  @behaviour :ssh_dbg
  Record.defrecord(:r_state, :state,
    xf: :undefined,
    cwd: :undefined,
    root: :undefined,
    remote_channel: :undefined,
    pending: :undefined,
    file_handler: :undefined,
    file_state: :undefined,
    max_files: :undefined,
    options: :undefined,
    handles: :undefined
  )

  def subsystem_spec(options) do
    {'sftp', {:ssh_sftpd, options}}
  end

  def init(options) do
    {fileMod, fS0} =
      case :proplists.get_value(:file_handler, options, {:ssh_sftpd_file, []}) do
        {f, s} ->
          {f, s}

        f ->
          {f, []}
      end

    {{:ok, default}, fS1} = fileMod.get_cwd(fS0)
    cWD = :proplists.get_value(:cwd, options, default)
    root0 = :proplists.get_value(:root, options, '')

    {root, state} =
      case resolve_symlinks(
             root0,
             r_state(
               root: root0,
               file_handler: fileMod,
               file_state: fS1
             )
           ) do
        {{:ok, root1}, state0} ->
          {root1, state0}

        {{:error, _}, state0} ->
          {root0, state0}
      end

    maxLength = :proplists.get_value(:max_files, options, 0)
    vsn = :proplists.get_value(:sftpd_vsn, options, 5)

    {:ok,
     r_state(state,
       cwd: cWD,
       root: root,
       max_files: maxLength,
       options: options,
       handles: [],
       pending: <<>>,
       xf: r_ssh_xfer(vsn: vsn, ext: [])
     )}
  end

  def handle_ssh_msg(
        {:ssh_cm, _ConnectionManager, {:data, _ChannelId, type, data}},
        state
      ) do
    state1 = handle_data(type, data, state)
    {:ok, state1}
  end

  def handle_ssh_msg({:ssh_cm, _, {:eof, channelId}}, state) do
    {:stop, channelId, state}
  end

  def handle_ssh_msg({:ssh_cm, _, {:signal, _, _}}, state) do
    {:ok, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, _, {:exit_signal, channelId, signal, error, _}},
        state
      ) do
    report = :io_lib.format('Connection closed by peer signal ~p~n Error ~p~n', [signal, error])
    :error_logger.error_report(report)
    {:stop, channelId, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, _, {:exit_status, channelId, 0}},
        state
      ) do
    {:stop, channelId, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, _, {:exit_status, channelId, status}},
        state
      ) do
    report = :io_lib.format('Connection closed by peer ~n Status ~p~n', [status])
    :error_logger.error_report(report)
    {:stop, channelId, state}
  end

  def handle_msg(
        {:ssh_channel_up, channelId, connectionManager},
        r_state(xf: xf, options: options) = state
      ) do
    maybe_increase_recv_window(connectionManager, channelId, options)

    {:ok,
     r_state(state,
       xf:
         r_ssh_xfer(xf,
           cm: connectionManager,
           channel: channelId
         )
     )}
  end

  def terminate(
        _,
        r_state(handles: handles, file_handler: fileMod, file_state: fS)
      ) do
    closeFun = fn
      {_, :file, {_, fd}}, fS0 ->
        {_Res, fS1} = fileMod.close(fd, fS0)
        fS1

      _, fS0 ->
        fS0
    end

    :lists.foldl(closeFun, fS, handles)
    :ok
  end

  defp handle_data(
         0,
         <<len::size(32)-unsigned-big-integer, msg::size(len)-binary, rest::binary>>,
         state = r_state(pending: <<>>)
       ) do
    <<op, reqId::size(32)-unsigned-big-integer, data::binary>> = msg
    newState = handle_op(op, reqId, data, state)

    case rest do
      <<>> ->
        newState

      _ ->
        handle_data(0, rest, newState)
    end
  end

  defp handle_data(0, data, state = r_state(pending: <<>>)) do
    r_state(state, pending: data)
  end

  defp handle_data(type, data, state = r_state(pending: pending)) do
    handle_data(type, <<pending::binary, data::binary>>, r_state(state, pending: <<>>))
  end

  defp handle_op(1, version, b, state) when is_binary(b) do
    xF = r_state(state, :xf)
    vsn = :lists.min([r_ssh_xfer(xF, :vsn), version])
    xF1 = r_ssh_xfer(xF, vsn: vsn)
    :ssh_xfer.xf_send_reply(xF1, 2, <<vsn::size(32)-unsigned-big-integer>>)
    r_state(state, xf: xF1)
  end

  defp handle_op(
         16,
         reqId,
         <<rlen::size(32)-unsigned-big-integer, rPath::size(rlen)-binary>>,
         state0
       ) do
    relPath = relate_file_name(rPath, state0, _Canonicalize = false)
    {res, state} = resolve_symlinks(relPath, state0)

    case res do
      {:ok, absPath} ->
        newAbsPath = chroot_filename(absPath, state)
        xF = r_state(state, :xf)
        attr = r_ssh_xfer_attr(type: :directory)
        :ssh_xfer.xf_send_name(xF, reqId, newAbsPath, attr)
        state

      {:error, _} = error ->
        send_status(error, reqId, state)
    end
  end

  defp handle_op(
         11,
         reqId,
         <<rLen::size(32)-unsigned-big-integer, rPath::size(rLen)-binary>>,
         state0 = r_state(xf: r_ssh_xfer(vsn: vsn), file_handler: fileMod, file_state: fS0)
       ) do
    relPath = :unicode.characters_to_list(rPath)
    absPath = relate_file_name(relPath, state0)
    xF = r_state(state0, :xf)
    {isDir, fS1} = fileMod.is_dir(absPath, fS0)
    state1 = r_state(state0, file_state: fS1)

    case isDir do
      false when vsn > 5 ->
        :ssh_xfer.xf_send_status(xF, reqId, 19, 'Not a directory')
        state1

      false ->
        :ssh_xfer.xf_send_status(xF, reqId, 4, 'Not a directory')
        state1

      true ->
        add_handle(state1, xF, reqId, :directory, {relPath, :unread})
    end
  end

  defp handle_op(
         12,
         reqId,
         <<hLen::size(32)-unsigned-big-integer, binHandle::size(hLen)-binary>>,
         state
       ) do
    xF = r_state(state, :xf)

    case get_handle(r_state(state, :handles), binHandle) do
      {_Handle, :directory, {_RelPath, :eof}} ->
        :ssh_xfer.xf_send_status(xF, reqId, 1)
        state

      {handle, :directory, {relPath, status}} ->
        read_dir(state, xF, reqId, handle, relPath, status)

      _ ->
        :ssh_xfer.xf_send_status(xF, reqId, 9)
        state
    end
  end

  defp handle_op(
         4,
         reqId,
         <<hLen::size(32)-unsigned-big-integer, binHandle::size(hLen)-binary>>,
         state = r_state(handles: handles, xf: xF, file_handler: fileMod, file_state: fS0)
       ) do
    case get_handle(handles, binHandle) do
      {handle, type, t} ->
        fS1 =
          case type do
            :file ->
              close_our_file(t, fileMod, fS0)

            _ ->
              fS0
          end

        :ssh_xfer.xf_send_status(xF, reqId, 0)

        r_state(state,
          handles: :lists.keydelete(handle, 1, handles),
          file_state: fS1
        )

      _ ->
        :ssh_xfer.xf_send_status(xF, reqId, 9)
        state
    end
  end

  defp handle_op(7, reqId, data, state) do
    stat(r_ssh_xfer(r_state(state, :xf), :vsn), reqId, data, state, :read_link_info)
  end

  defp handle_op(17, reqId, data, state) do
    stat(r_ssh_xfer(r_state(state, :xf), :vsn), reqId, data, state, :read_file_info)
  end

  defp handle_op(8, reqId, data, state) do
    fstat(r_ssh_xfer(r_state(state, :xf), :vsn), reqId, data, state)
  end

  defp handle_op(3, reqId, data, state) do
    open(r_ssh_xfer(r_state(state, :xf), :vsn), reqId, data, state)
  end

  defp handle_op(
         5,
         reqId,
         <<hLen::size(32)-unsigned-big-integer, binHandle::size(hLen)-binary,
           offset::size(64)-unsigned-big-integer, len::size(32)-unsigned-big-integer>>,
         state
       ) do
    case get_handle(r_state(state, :handles), binHandle) do
      {_Handle, :file, {_Path, ioDevice}} ->
        read_file(reqId, ioDevice, offset, len, state)

      _ ->
        :ssh_xfer.xf_send_status(r_state(state, :xf), reqId, 9)
        state
    end
  end

  defp handle_op(
         6,
         reqId,
         <<hLen::size(32)-unsigned-big-integer, binHandle::size(hLen)-binary,
           offset::size(64)-unsigned-big-integer, len::size(32)-unsigned-big-integer,
           data::size(len)-binary>>,
         state
       ) do
    case get_handle(r_state(state, :handles), binHandle) do
      {_Handle, :file, {_Path, ioDevice}} ->
        write_file(reqId, ioDevice, offset, data, state)

      _ ->
        :ssh_xfer.xf_send_status(r_state(state, :xf), reqId, 9)
        state
    end
  end

  defp handle_op(
         19,
         reqId,
         <<pLen::size(32)-unsigned-big-integer, relPath::size(pLen)-binary>>,
         state = r_state(file_handler: fileMod, file_state: fS0)
       ) do
    absPath = relate_file_name(relPath, state)
    {res, fS1} = fileMod.read_link(absPath, fS0)

    case res do
      {:ok, newPath} ->
        :ssh_xfer.xf_send_name(
          r_state(state, :xf),
          reqId,
          newPath,
          r_ssh_xfer_attr(type: :regular)
        )

      {:error, error} ->
        :ssh_xfer.xf_send_status(
          r_state(state, :xf),
          reqId,
          :ssh_xfer.encode_erlang_status(error)
        )
    end

    r_state(state, file_state: fS1)
  end

  defp handle_op(
         9,
         reqId,
         <<pLen::size(32)-unsigned-big-integer, bPath::size(pLen)-binary, attr::binary>>,
         state0
       ) do
    path = relate_file_name(bPath, state0)
    {status, state1} = set_stat(attr, path, state0)
    send_status(status, reqId, state1)
  end

  defp handle_op(
         14,
         reqId,
         <<pLen::size(32)-unsigned-big-integer, bPath::size(pLen)-binary, attr::binary>>,
         state0 = r_state(file_handler: fileMod, file_state: fS0)
       ) do
    path = relate_file_name(bPath, state0)
    {res, fS1} = fileMod.make_dir(path, fS0)
    state1 = r_state(state0, file_state: fS1)

    case res do
      :ok ->
        {_, state2} = set_stat(attr, path, state1)
        send_status(:ok, reqId, state2)

      {:error, error} ->
        send_status({:error, error}, reqId, state1)
    end
  end

  defp handle_op(
         10,
         reqId,
         <<hLen::size(32)-unsigned-big-integer, binHandle::size(hLen)-binary, attr::binary>>,
         state0 = r_state(handles: handles)
       ) do
    case get_handle(handles, binHandle) do
      {_Handle, _Type, {path, _}} ->
        {status, state1} = set_stat(attr, path, state0)
        send_status(status, reqId, state1)

      _ ->
        :ssh_xfer.xf_send_status(r_state(state0, :xf), reqId, 9)
        state0
    end
  end

  defp handle_op(
         13,
         reqId,
         <<pLen::size(32)-unsigned-big-integer, bPath::size(pLen)-binary>>,
         state0 = r_state(file_handler: fileMod, file_state: fS0, xf: r_ssh_xfer(vsn: vsn))
       ) do
    path = relate_file_name(bPath, state0)
    {isDir, _FS1} = fileMod.is_dir(path, fS0)

    case isDir do
      true when vsn > 5 ->
        :ssh_xfer.xf_send_status(r_state(state0, :xf), reqId, 24, 'File is a directory')
        state0

      true ->
        :ssh_xfer.xf_send_status(r_state(state0, :xf), reqId, 4, 'File is a directory')
        state0

      false ->
        {status, fS1} = fileMod.delete(path, fS0)
        state1 = r_state(state0, file_state: fS1)
        send_status(status, reqId, state1)
    end
  end

  defp handle_op(
         15,
         reqId,
         <<pLen::size(32)-unsigned-big-integer, bPath::size(pLen)-binary>>,
         state0 = r_state(file_handler: fileMod, file_state: fS0)
       ) do
    path = relate_file_name(bPath, state0)
    {status, fS1} = fileMod.del_dir(path, fS0)
    state1 = r_state(state0, file_state: fS1)
    send_status(status, reqId, state1)
  end

  defp handle_op(
         18,
         reqId,
         bin =
           <<pLen::size(32)-unsigned-big-integer, _::size(pLen)-binary,
             pLen2::size(32)-unsigned-big-integer, _::size(pLen2)-binary>>,
         state = r_state(xf: r_ssh_xfer(vsn: vsn))
       )
       when vsn == 3 or vsn == 4 do
    handle_op(18, reqId, <<bin::binary, 0::size(32)>>, state)
  end

  defp handle_op(
         18,
         reqId,
         <<pLen::size(32)-unsigned-big-integer, bPath::size(pLen)-binary,
           pLen2::size(32)-unsigned-big-integer, bPath2::size(pLen2)-binary,
           flags::size(32)-unsigned-big-integer>>,
         state0 = r_state(file_handler: fileMod, file_state: fS0)
       ) do
    path = relate_file_name(bPath, state0)
    path2 = relate_file_name(bPath2, state0)

    case flags &&& 2 do
      0 ->
        case flags &&& 1 do
          0 ->
            {res, fS1} = fileMod.read_link_info(path2, fS0)
            state1 = r_state(state0, file_state: fS1)

            case res do
              {:ok, _Info} ->
                :ssh_xfer.xf_send_status(r_state(state1, :xf), reqId, 11)
                state1

              _ ->
                rename(path, path2, reqId, state1)
            end

          _ ->
            rename(path, path2, reqId, state0)
        end

      _ ->
        :ssh_xfer.xf_send_status(r_state(state0, :xf), reqId, 8)
        state0
    end
  end

  defp handle_op(
         20,
         reqId,
         <<pLen::size(32)-unsigned-big-integer, link::size(pLen)-binary,
           pLen2::size(32)-unsigned-big-integer, target::size(pLen2)-binary>>,
         state0 = r_state(file_handler: fileMod, file_state: fS0)
       ) do
    linkPath = relate_file_name(link, state0)
    targetPath = relate_file_name(target, state0)
    {status, fS1} = fileMod.make_symlink(targetPath, linkPath, fS0)
    state1 = r_state(state0, file_state: fS1)
    send_status(status, reqId, state1)
  end

  defp new_handle([], h) do
    h
  end

  defp new_handle([{n, _, _} | rest], h) when n <= h do
    new_handle(rest, n + 1)
  end

  defp new_handle([_ | rest], h) do
    new_handle(rest, h)
  end

  defp add_handle(state, xF, reqId, type, dirFileTuple) do
    handles = r_state(state, :handles)
    handle = new_handle(handles, 0)
    :ssh_xfer.xf_send_handle(xF, reqId, :erlang.integer_to_list(handle))

    r_state(state,
      handles: [
        {handle, type, dirFileTuple}
        | handles
      ]
    )
  end

  defp get_handle(handles, binHandle) do
    case (try do
            :erlang.list_to_integer(:erlang.binary_to_list(binHandle))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      i when is_integer(i) ->
        case :lists.keysearch(i, 1, handles) do
          {:value, t} ->
            t

          false ->
            :error
        end

      _ ->
        :error
    end
  end

  defp read_dir(
         state0 = r_state(file_handler: fileMod, max_files: maxLength, file_state: fS0),
         xF,
         reqId,
         handle,
         relPath,
         {:cache, files}
       ) do
    absPath = relate_file_name(relPath, state0)

    cond do
      length(files) > maxLength ->
        {toSend, newCache} = :lists.split(maxLength, files)
        {namesAndAttrs, fS1} = get_attrs(absPath, toSend, fileMod, fS0)
        :ssh_xfer.xf_send_names(xF, reqId, namesAndAttrs)

        handles =
          :lists.keyreplace(
            handle,
            1,
            r_state(state0, :handles),
            {handle, :directory, {relPath, {:cache, newCache}}}
          )

        r_state(state0, handles: handles, file_state: fS1)

      true ->
        {namesAndAttrs, fS1} = get_attrs(absPath, files, fileMod, fS0)
        :ssh_xfer.xf_send_names(xF, reqId, namesAndAttrs)

        handles =
          :lists.keyreplace(
            handle,
            1,
            r_state(state0, :handles),
            {handle, :directory, {relPath, :eof}}
          )

        r_state(state0, handles: handles, file_state: fS1)
    end
  end

  defp read_dir(
         state0 = r_state(file_handler: fileMod, max_files: maxLength, file_state: fS0),
         xF,
         reqId,
         handle,
         relPath,
         _Status
       ) do
    absPath = relate_file_name(relPath, state0)
    {res, fS1} = fileMod.list_dir(absPath, fS0)

    case res do
      {:ok, files}
      when maxLength == 0 or maxLength > length(files) ->
        {namesAndAttrs, fS2} = get_attrs(absPath, files, fileMod, fS1)
        :ssh_xfer.xf_send_names(xF, reqId, namesAndAttrs)

        handles =
          :lists.keyreplace(
            handle,
            1,
            r_state(state0, :handles),
            {handle, :directory, {relPath, :eof}}
          )

        r_state(state0, handles: handles, file_state: fS2)

      {:ok, files} ->
        {toSend, cache} = :lists.split(maxLength, files)
        {namesAndAttrs, fS2} = get_attrs(absPath, toSend, fileMod, fS1)
        :ssh_xfer.xf_send_names(xF, reqId, namesAndAttrs)

        handles =
          :lists.keyreplace(
            handle,
            1,
            r_state(state0, :handles),
            {handle, :directory, {relPath, {:cache, cache}}}
          )

        r_state(state0, handles: handles, file_state: fS2)

      {:error, error} ->
        state1 = r_state(state0, file_state: fS1)
        send_status({:error, error}, reqId, state1)
    end
  end

  defp get_attrs(relPath, files, fileMod, fS) do
    get_attrs(relPath, files, fileMod, fS, [])
  end

  defp get_attrs(_RelPath, [], _FileMod, fS, acc) do
    {:lists.reverse(acc), fS}
  end

  defp get_attrs(relPath, [f | rest], fileMod, fS0, acc) do
    path = :filename.absname(f, relPath)

    case fileMod.read_link_info(path, fS0) do
      {{:ok, info}, fS1} ->
        attrs = :ssh_sftp.info_to_attr(info)
        get_attrs(relPath, rest, fileMod, fS1, [{f, attrs} | acc])

      {{:error, :enoent}, fS1} ->
        get_attrs(relPath, rest, fileMod, fS1, acc)

      {error, fS1} ->
        {error, fS1}
    end
  end

  defp close_our_file({_, fd}, fileMod, fS0) do
    {_Res, fS1} = fileMod.close(fd, fS0)
    fS1
  end

  defp stat(_Vsn, reqId, data, state, f) do
    <<bLen::size(32)-unsigned-big-integer, bPath::size(bLen)-binary, _::binary>> = data
    stat(reqId, :unicode.characters_to_list(bPath), state, f)
  end

  defp fstat(vsn, reqId, data, state) when vsn <= 3 do
    <<hLen::size(32)-unsigned-big-integer, handle::size(hLen)-binary>> = data
    fstat(reqId, handle, state)
  end

  defp fstat(vsn, reqId, data, state) when vsn >= 4 do
    <<hLen::size(32)-unsigned-big-integer, handle::size(hLen)-binary,
      _Flags::size(32)-unsigned-big-integer>> = data

    fstat(reqId, handle, state)
  end

  defp fstat(reqId, binHandle, state) do
    case get_handle(r_state(state, :handles), binHandle) do
      {_Handle, _Type, {path, _}} ->
        stat(reqId, path, state, :read_file_info)

      _ ->
        :ssh_xfer.xf_send_status(r_state(state, :xf), reqId, 9)
        state
    end
  end

  defp stat(reqId, relPath, state0 = r_state(file_handler: fileMod, file_state: fS0), f) do
    absPath = relate_file_name(relPath, state0)
    xF = r_state(state0, :xf)
    {res, fS1} = apply(fileMod, f, [absPath, fS0])
    state1 = r_state(state0, file_state: fS1)

    case res do
      {:ok, fileInfo} ->
        :ssh_xfer.xf_send_attr(xF, reqId, :ssh_sftp.info_to_attr(fileInfo))
        state1

      {:error, e} ->
        send_status({:error, e}, reqId, state1)
    end
  end

  defp sftp_to_erlang_flag(:read, vsn) when vsn == 3 or vsn == 4 do
    :read
  end

  defp sftp_to_erlang_flag(:write, vsn) when vsn == 3 or vsn == 4 do
    :write
  end

  defp sftp_to_erlang_flag(:append, vsn) when vsn == 3 or vsn == 4 do
    :append
  end

  defp sftp_to_erlang_flag(:creat, vsn) when vsn == 3 or vsn == 4 do
    :write
  end

  defp sftp_to_erlang_flag(:trunc, vsn) when vsn == 3 or vsn == 4 do
    :write
  end

  defp sftp_to_erlang_flag(:excl, vsn) when vsn == 3 or vsn == 4 do
    :read
  end

  defp sftp_to_erlang_flag(:create_new, vsn) when vsn > 4 do
    :write
  end

  defp sftp_to_erlang_flag(:create_truncate, vsn) when vsn > 4 do
    :write
  end

  defp sftp_to_erlang_flag(:open_existing, vsn) when vsn > 4 do
    :read
  end

  defp sftp_to_erlang_flag(:open_or_create, vsn) when vsn > 4 do
    :write
  end

  defp sftp_to_erlang_flag(:truncate_existing, vsn) when vsn > 4 do
    :write
  end

  defp sftp_to_erlang_flag(:append_data, vsn) when vsn > 4 do
    :append
  end

  defp sftp_to_erlang_flag(:append_data_atomic, vsn) when vsn > 4 do
    :append
  end

  defp sftp_to_erlang_flag(_, _) do
    :read
  end

  defp sftp_to_erlang_flags(flags, vsn) do
    :lists.map(
      fn flag ->
        sftp_to_erlang_flag(flag, vsn)
      end,
      flags
    )
  end

  defp sftp_to_erlang_access_flag(:read_data, _) do
    :read
  end

  defp sftp_to_erlang_access_flag(:list_directory, _) do
    :read
  end

  defp sftp_to_erlang_access_flag(:write_data, _) do
    :write
  end

  defp sftp_to_erlang_access_flag(:append_data, _) do
    :append
  end

  defp sftp_to_erlang_access_flag(:add_subdirectory, _) do
    :read
  end

  defp sftp_to_erlang_access_flag(:add_file, _) do
    :write
  end

  defp sftp_to_erlang_access_flag(:write_attributes, _) do
    :write
  end

  defp sftp_to_erlang_access_flag(_, _) do
    :read
  end

  defp sftp_to_erlang_access_flags(flags, vsn) do
    :lists.map(
      fn flag ->
        sftp_to_erlang_access_flag(flag, vsn)
      end,
      flags
    )
  end

  defp open(vsn, reqId, data, state) when vsn <= 3 do
    <<bLen::size(32)-unsigned-big-integer, bPath::size(bLen)-binary,
      pFlags::size(32)-unsigned-big-integer, _Attrs::binary>> = data

    path = :unicode.characters_to_list(bPath)
    flagBits = :ssh_xfer.decode_open_flags(vsn, pFlags)

    flags =
      :lists.usort(
        sftp_to_erlang_flags(
          flagBits,
          vsn
        )
      )

    do_open(reqId, state, path, flags)
  end

  defp open(vsn, reqId, data, state) when vsn >= 4 do
    <<bLen::size(32)-unsigned-big-integer, bPath::size(bLen)-binary,
      access::size(32)-unsigned-big-integer, pFlags::size(32)-unsigned-big-integer,
      _Attrs::binary>> = data

    path = :unicode.characters_to_list(bPath)
    flagBits = :ssh_xfer.decode_open_flags(vsn, pFlags)
    acessBits = :ssh_xfer.decode_ace_mask(access)
    acessFlags = sftp_to_erlang_access_flags(acessBits, vsn)

    flags =
      :lists.usort(
        sftp_to_erlang_flags(
          flagBits,
          vsn
        ) ++ acessFlags
      )

    do_open(reqId, state, path, flags)
  end

  defp do_open(reqId, state0, path, flags) do
    r_state(file_handler: fileMod, file_state: fS0, xf: r_ssh_xfer(vsn: vsn)) = state0
    absPath = relate_file_name(path, state0)
    {isDir, _FS1} = fileMod.is_dir(absPath, fS0)

    case isDir do
      true when vsn > 5 ->
        :ssh_xfer.xf_send_status(r_state(state0, :xf), reqId, 24, 'File is a directory')
        state0

      true ->
        :ssh_xfer.xf_send_status(r_state(state0, :xf), reqId, 4, 'File is a directory')
        state0

      false ->
        openFlags = [:binary | flags]
        {res, fS1} = fileMod.open(absPath, openFlags, fS0)
        state1 = r_state(state0, file_state: fS1)

        case res do
          {:ok, ioDevice} ->
            add_handle(state1, r_state(state0, :xf), reqId, :file, {path, ioDevice})

          {:error, error} ->
            :ssh_xfer.xf_send_status(
              r_state(state1, :xf),
              reqId,
              :ssh_xfer.encode_erlang_status(error)
            )

            state1
        end
    end
  end

  defp resolve_symlinks(path, state) do
    resolve_symlinks(path, _LinkCnt = 32, state)
  end

  defp resolve_symlinks(path, linkCnt, state0) do
    resolve_symlinks_2(:filename.split(path), state0, linkCnt, [])
  end

  defp resolve_symlinks_2(_Path, state, linkCnt, _AccPath)
       when linkCnt === 0 do
    {{:error, :emlink}, state}
  end

  defp resolve_symlinks_2(['.' | restPath], state0, linkCnt, accPath) do
    resolve_symlinks_2(restPath, state0, linkCnt, accPath)
  end

  defp resolve_symlinks_2(['..' | restPath], state0, linkCnt, accPath) do
    accPathComps0 = :filename.split(accPath)

    path =
      case :lists.droplast(accPathComps0) do
        [] ->
          ''

        accPathComps ->
          :filename.join(accPathComps)
      end

    resolve_symlinks_2(restPath, state0, linkCnt, path)
  end

  defp resolve_symlinks_2([pathComp | restPath], state0, linkCnt, accPath0) do
    r_state(file_handler: fileMod, file_state: fS0) = state0
    accPath1 = :filename.join(accPath0, pathComp)
    {res, fS1} = fileMod.read_link(accPath1, fS0)
    state1 = r_state(state0, file_state: fS1)

    case res do
      {:ok, target0} ->
        target1 = :filename.absname(target0, accPath0)
        {followRes, state2} = resolve_symlinks(target1, linkCnt - 1, state1)

        case followRes do
          {:ok, target} ->
            resolve_symlinks_2(restPath, state2, linkCnt - 1, target)

          {:error, _} = error ->
            {error, state2}
        end

      {:error, :einval} ->
        resolve_symlinks_2(restPath, state1, linkCnt, accPath1)

      {:error, _} = error ->
        {error, state1}
    end
  end

  defp resolve_symlinks_2([], state, _LinkCnt, accPath) do
    {{:ok, accPath}, state}
  end

  defp relate_file_name(file, state) do
    relate_file_name(file, state, _Canonicalize = true)
  end

  defp relate_file_name(file, state, canonicalize)
       when is_binary(file) do
    relate_file_name(:unicode.characters_to_list(file), state, canonicalize)
  end

  defp relate_file_name(file, r_state(cwd: cWD, root: ''), canonicalize) do
    relate_filename_to_path(file, cWD, canonicalize)
  end

  defp relate_file_name(file, r_state(cwd: cWD, root: root), canonicalize) do
    cWD1 =
      case is_within_root(root, cWD) do
        true ->
          cWD

        false ->
          root
      end

    absFile =
      case make_relative_filename(file) do
        ^file ->
          relate_filename_to_path(file, cWD1, canonicalize)

        relFile ->
          relate_filename_to_path(relFile, root, canonicalize)
      end

    case is_within_root(root, absFile) do
      true ->
        absFile

      false ->
        root
    end
  end

  defp is_within_root(root, file) do
    :lists.prefix(root, file)
  end

  defp make_relative_filename('/') do
    './'
  end

  defp make_relative_filename('/' ++ file) do
    file
  end

  defp make_relative_filename(file) do
    file
  end

  defp relate_filename_to_path(file0, path, canonicalize) do
    file1 = :filename.absname(file0, path)

    file2 =
      cond do
        canonicalize ->
          canonicalize_filename(file1)

        true ->
          file1
      end

    ensure_trailing_slash_is_preserved(file0, file2)
  end

  defp ensure_trailing_slash_is_preserved(file0, file1) do
    case {:lists.suffix('/', file0), :lists.suffix('/', file1)} do
      {true, false} ->
        file1 ++ '/'

      _Other ->
        file1
    end
  end

  defp canonicalize_filename(file0) do
    file =
      :filename.join(
        canonicalize_filename_2(
          :filename.split(file0),
          []
        )
      )

    ensure_trailing_slash_is_preserved(file0, file)
  end

  defp canonicalize_filename_2(['..' | rest], ['/'] = acc) do
    canonicalize_filename_2(rest, acc)
  end

  defp canonicalize_filename_2(['..' | rest], [_Dir | paths]) do
    canonicalize_filename_2(rest, paths)
  end

  defp canonicalize_filename_2(['.' | rest], acc) do
    canonicalize_filename_2(rest, acc)
  end

  defp canonicalize_filename_2([a | rest], acc) do
    canonicalize_filename_2(rest, [a | acc])
  end

  defp canonicalize_filename_2([], acc) do
    :lists.reverse(acc)
  end

  defp chroot_filename(filename, r_state(root: root)) do
    filenameComps0 = :filename.split(filename)
    rootComps = :filename.split(root)

    :filename.join(
      chroot_filename_2(
        filenameComps0,
        rootComps
      )
    )
  end

  defp chroot_filename_2(
         [pathComp | filenameRest],
         [pathComp | rootRest]
       ) do
    chroot_filename_2(filenameRest, rootRest)
  end

  defp chroot_filename_2(filenameComps, [])
       when length(filenameComps) > 0 do
    ['/' | filenameComps]
  end

  defp chroot_filename_2(_FilenameComps, _RootComps) do
    ['/']
  end

  defp read_file(
         reqId,
         ioDevice,
         offset,
         len,
         state0 = r_state(file_handler: fileMod, file_state: fS0)
       ) do
    {res1, fS1} = fileMod.position(ioDevice, {:bof, offset}, fS0)

    case res1 do
      {:ok, _NewPos} ->
        {res2, fS2} = fileMod.read(ioDevice, len, fS1)
        state1 = r_state(state0, file_state: fS2)

        case res2 do
          {:ok, data} ->
            :ssh_xfer.xf_send_data(r_state(state1, :xf), reqId, data)
            state1

          {:error, error} ->
            send_status({:error, error}, reqId, state1)

          :eof ->
            send_status(:eof, reqId, state1)
        end

      {:error, error} ->
        state1 = r_state(state0, file_state: fS1)
        send_status({:error, error}, reqId, state1)
    end
  end

  defp write_file(
         reqId,
         ioDevice,
         offset,
         data,
         state0 = r_state(file_handler: fileMod, file_state: fS0)
       ) do
    {res, fS1} = fileMod.position(ioDevice, {:bof, offset}, fS0)

    case res do
      {:ok, _NewPos} ->
        {status, fS2} = fileMod.write(ioDevice, data, fS1)
        state1 = r_state(state0, file_state: fS2)
        send_status(status, reqId, state1)

      {:error, error} ->
        state1 = r_state(state0, file_state: fS1)
        send_status({:error, error}, reqId, state1)
    end
  end

  defp get_status(:ok) do
    0
  end

  defp get_status(:eof) do
    1
  end

  defp get_status({:error, error}) do
    :ssh_xfer.encode_erlang_status(error)
  end

  defp send_status(status, reqId, state) do
    :ssh_xfer.xf_send_status(r_state(state, :xf), reqId, get_status(status))
    state
  end

  defp set_stat(<<>>, _Path, state) do
    {:ok, state}
  end

  defp set_stat(attr, path, state0 = r_state(file_handler: fileMod, file_state: fS0)) do
    {decodedAttr, _Rest} =
      :ssh_xfer.decode_ATTR(
        r_ssh_xfer(r_state(state0, :xf), :vsn),
        attr
      )

    info = :ssh_sftp.attr_to_info(decodedAttr)
    {res1, fS1} = fileMod.read_link_info(path, fS0)

    case res1 do
      {:ok, oldInfo} ->
        newInfo = set_file_info(info, oldInfo)
        {res2, fS2} = fileMod.write_file_info(path, newInfo, fS1)
        state1 = r_state(state0, file_state: fS2)
        {res2, state1}

      {:error, error} ->
        state1 = r_state(state0, file_state: fS1)
        {{:error, error}, state1}
    end
  end

  defp set_file_info_sel(:undefined, f) do
    f
  end

  defp set_file_info_sel(f, _) do
    f
  end

  defp set_file_info(
         r_file_info(
           atime: dst_atime,
           mtime: dst_mtime,
           ctime: dst_ctime,
           mode: dst_mode,
           uid: dst_uid,
           gid: dst_gid
         ),
         r_file_info(
           atime: src_atime,
           mtime: src_mtime,
           ctime: src_ctime,
           mode: src_mode,
           uid: src_uid,
           gid: src_gid
         )
       ) do
    r_file_info(
      atime: set_file_info_sel(dst_atime, src_atime),
      mtime: set_file_info_sel(dst_mtime, src_mtime),
      ctime: set_file_info_sel(dst_ctime, src_ctime),
      mode: set_file_info_sel(dst_mode, src_mode),
      uid: set_file_info_sel(dst_uid, src_uid),
      gid: set_file_info_sel(dst_gid, src_gid)
    )
  end

  defp rename(path, path2, reqId, state0) do
    r_state(file_handler: fileMod, file_state: fS0) = state0
    {status, fS1} = fileMod.rename(path, path2, fS0)
    state1 = r_state(state0, file_state: fS1)
    send_status(status, reqId, state1)
  end

  defp maybe_increase_recv_window(connectionManager, channelId, options) do
    wantedRecvWindowSize = :proplists.get_value(:recv_window_size, options, 1_000_000)
    numPkts = div(wantedRecvWindowSize, 65536)
    increment = numPkts * 65536 - 10 * 65536

    cond do
      increment > 0 ->
        :ssh_connection.adjust_window(connectionManager, channelId, increment)

      increment <= 0 ->
        :do_nothing
    end
  end

  def ssh_dbg_trace_points() do
    [:terminate]
  end

  def ssh_dbg_flags(:terminate) do
    [:c]
  end

  def ssh_dbg_on(:terminate) do
    :dbg.tp(:ssh_sftpd, :terminate, 2, :x)
  end

  def ssh_dbg_off(:terminate) do
    :dbg.ctpg(:ssh_sftpd, :terminate, 2)
  end

  def ssh_dbg_format(
        :terminate,
        {:call, {:ssh_sftpd, :terminate, [reason, state]}}
      ) do
    [
      'SftpD Terminating:\n',
      :io_lib.format('Reason: ~p,~nState:~n~s', [reason, wr_record(state)])
    ]
  end

  def ssh_dbg_format(
        :terminate,
        {:return_from, {:ssh_sftpd, :terminate, 2}, _Ret}
      ) do
    :skip
  end

  defp wr_record(r = r_state()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_state(r_state())), [])
  end
end
