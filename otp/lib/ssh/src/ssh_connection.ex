defmodule :m_ssh_connection do
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

  Record.defrecord(:r_ssh_msg_disconnect, :ssh_msg_disconnect,
    code: :undefined,
    description: :undefined,
    language: :undefined
  )

  Record.defrecord(:r_ssh_msg_ignore, :ssh_msg_ignore, data: :undefined)
  Record.defrecord(:r_ssh_msg_unimplemented, :ssh_msg_unimplemented, sequence: :undefined)

  Record.defrecord(:r_ssh_msg_debug, :ssh_msg_debug,
    always_display: :undefined,
    message: :undefined,
    language: :undefined
  )

  Record.defrecord(:r_ssh_msg_service_request, :ssh_msg_service_request, name: :undefined)
  Record.defrecord(:r_ssh_msg_service_accept, :ssh_msg_service_accept, name: :undefined)

  Record.defrecord(:r_ssh_msg_ext_info, :ssh_msg_ext_info,
    nr_extensions: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_kexinit, :ssh_msg_kexinit,
    cookie: :undefined,
    kex_algorithms: :undefined,
    server_host_key_algorithms: :undefined,
    encryption_algorithms_client_to_server: :undefined,
    encryption_algorithms_server_to_client: :undefined,
    mac_algorithms_client_to_server: :undefined,
    mac_algorithms_server_to_client: :undefined,
    compression_algorithms_client_to_server: :undefined,
    compression_algorithms_server_to_client: :undefined,
    languages_client_to_server: :undefined,
    languages_server_to_client: :undefined,
    first_kex_packet_follows: false,
    reserved: 0
  )

  Record.defrecord(:r_ssh_msg_kexdh_init, :ssh_msg_kexdh_init, e: :undefined)

  Record.defrecord(:r_ssh_msg_kexdh_reply, :ssh_msg_kexdh_reply,
    public_host_key: :undefined,
    f: :undefined,
    h_sig: :undefined
  )

  Record.defrecord(:r_ssh_msg_newkeys, :ssh_msg_newkeys, [])

  Record.defrecord(:r_ssh_msg_kex_dh_gex_request, :ssh_msg_kex_dh_gex_request,
    min: :undefined,
    n: :undefined,
    max: :undefined
  )

  Record.defrecord(:r_ssh_msg_kex_dh_gex_request_old, :ssh_msg_kex_dh_gex_request_old,
    n: :undefined
  )

  Record.defrecord(:r_ssh_msg_kex_dh_gex_group, :ssh_msg_kex_dh_gex_group,
    p: :undefined,
    g: :undefined
  )

  Record.defrecord(:r_ssh_msg_kex_dh_gex_init, :ssh_msg_kex_dh_gex_init, e: :undefined)

  Record.defrecord(:r_ssh_msg_kex_dh_gex_reply, :ssh_msg_kex_dh_gex_reply,
    public_host_key: :undefined,
    f: :undefined,
    h_sig: :undefined
  )

  Record.defrecord(:r_ssh_msg_kex_ecdh_init, :ssh_msg_kex_ecdh_init, q_c: :undefined)

  Record.defrecord(:r_ssh_msg_kex_ecdh_reply, :ssh_msg_kex_ecdh_reply,
    public_host_key: :undefined,
    q_s: :undefined,
    h_sig: :undefined
  )

  def dummy(_) do
    false
  end

  def session_channel(connectionHandler, timeout) do
    session_channel(connectionHandler, 10 * 65536, 65536, timeout)
  end

  def session_channel(connectionHandler, initialWindowSize, maxPacketSize, timeout) do
    open_channel(connectionHandler, 'session', <<>>, initialWindowSize, maxPacketSize, timeout)
  end

  def open_channel(connectionHandler, type, chanData, timeout) do
    open_channel(connectionHandler, type, chanData, 10 * 65536, 65536, timeout)
  end

  defp open_channel(connectionHandler, type, chanData, initialWindowSize, maxPacketSize, timeout) do
    case :ssh_connection_handler.open_channel(
           connectionHandler,
           type,
           chanData,
           initialWindowSize,
           maxPacketSize,
           timeout
         ) do
      {:open, channel} ->
        {:ok, channel}

      error ->
        error
    end
  end

  def exec(connectionHandler, channelId, command, timeOut) do
    :ssh_connection_handler.request(
      connectionHandler,
      self(),
      channelId,
      'exec',
      true,
      [
        <<:erlang.size(:unicode.characters_to_binary(command))::size(32)-unsigned-big-integer,
          :unicode.characters_to_binary(command)::binary>>
      ],
      timeOut
    )
  end

  def shell(connectionHandler, channelId) do
    :ssh_connection_handler.request(connectionHandler, self(), channelId, 'shell', false, <<>>, 0)
  end

  def subsystem(connectionHandler, channelId, subSystem, timeOut) do
    :ssh_connection_handler.request(
      connectionHandler,
      self(),
      channelId,
      'subsystem',
      true,
      [
        <<:erlang.size(:unicode.characters_to_binary(subSystem))::size(32)-unsigned-big-integer,
          :unicode.characters_to_binary(subSystem)::binary>>
      ],
      timeOut
    )
  end

  def send(connectionHandler, channelId, data) do
    send(connectionHandler, channelId, 0, data, :infinity)
  end

  def send(connectionHandler, channelId, data, timeOut)
      when is_integer(timeOut) do
    send(connectionHandler, channelId, 0, data, timeOut)
  end

  def send(connectionHandler, channelId, data, :infinity) do
    send(connectionHandler, channelId, 0, data, :infinity)
  end

  def send(connectionHandler, channelId, type, data) do
    send(connectionHandler, channelId, type, data, :infinity)
  end

  def send(connectionHandler, channelId, type, data, timeOut) do
    :ssh_connection_handler.send(connectionHandler, channelId, type, data, timeOut)
  end

  def send_eof(connectionHandler, channel) do
    :ssh_connection_handler.send_eof(
      connectionHandler,
      channel
    )
  end

  def adjust_window(connectionHandler, channel, bytes) do
    :ssh_connection_handler.adjust_window(connectionHandler, channel, bytes)
  end

  def setenv(connectionHandler, channelId, var, value, timeOut) do
    setenv(connectionHandler, channelId, true, var, value, timeOut)
  end

  defp setenv(connectionHandler, channelId, wantReply, var, value, timeOut) do
    case :ssh_connection_handler.request(
           connectionHandler,
           channelId,
           'env',
           wantReply,
           [
             <<:erlang.size(:unicode.characters_to_binary(var))::size(32)-unsigned-big-integer,
               :unicode.characters_to_binary(var)::binary>>,
             <<:erlang.size(:unicode.characters_to_binary(value))::size(32)-unsigned-big-integer,
               :unicode.characters_to_binary(value)::binary>>
           ],
           timeOut
         ) do
      :ok when wantReply == false ->
        :success

      reply ->
        reply
    end
  end

  def close(connectionHandler, channelId) do
    :ssh_connection_handler.close(
      connectionHandler,
      channelId
    )
  end

  def reply_request(connectionHandler, true, status, channelId) do
    :ssh_connection_handler.reply_request(connectionHandler, status, channelId)
  end

  def reply_request(_, false, _, _) do
    :ok
  end

  def ptty_alloc(connectionHandler, channel, options) do
    ptty_alloc(connectionHandler, channel, options, :infinity)
  end

  def ptty_alloc(connectionHandler, channel, options0, timeOut) do
    termData = backwards_compatible(options0, [])

    {width, pixWidth} =
      pty_default_dimensions(
        :width,
        termData
      )

    {height, pixHeight} =
      pty_default_dimensions(
        :height,
        termData
      )

    pty_req(
      connectionHandler,
      channel,
      :proplists.get_value(:term, termData, :os.getenv('TERM', 'vt100')),
      :proplists.get_value(:width, termData, width),
      :proplists.get_value(:height, termData, height),
      :proplists.get_value(:pixel_widh, termData, pixWidth),
      :proplists.get_value(:pixel_height, termData, pixHeight),
      :proplists.get_value(:pty_opts, termData, []),
      timeOut
    )
  end

  def window_change(connectionHandler, channel, width, height) do
    window_change(connectionHandler, channel, width, height, 0, 0)
  end

  def window_change(connectionHandler, channel, width, height, pixWidth, pixHeight) do
    :ssh_connection_handler.request(
      connectionHandler,
      channel,
      'window-change',
      false,
      [
        <<width::size(32)-unsigned-big-integer>>,
        <<height::size(32)-unsigned-big-integer>>,
        <<pixWidth::size(32)-unsigned-big-integer>>,
        <<pixHeight::size(32)-unsigned-big-integer>>
      ],
      0
    )
  end

  def signal(connectionHandler, channel, sig) do
    :ssh_connection_handler.request(
      connectionHandler,
      channel,
      'signal',
      false,
      [
        <<:erlang.size(:unicode.characters_to_binary(sig))::size(32)-unsigned-big-integer,
          :unicode.characters_to_binary(sig)::binary>>
      ],
      0
    )
  end

  def exit_status(connectionHandler, channel, status) do
    :ssh_connection_handler.request(
      connectionHandler,
      channel,
      'exit-status',
      false,
      [<<status::size(32)-unsigned-big-integer>>],
      0
    )
  end

  def channel_data(
        channelId,
        dataType,
        data0,
        r_connection(channel_cache: cache) = connection,
        from
      ) do
    case :ssh_client_channel.cache_lookup(
           cache,
           channelId
         ) do
      r_channel(remote_id: id, sent_close: false) = channel0 ->
        data =
          try do
            :erlang.iolist_to_binary(data0)
          catch
            _, _ ->
              :unicode.characters_to_binary(data0)
          end

        {sendList, channel} =
          update_send_window(r_channel(channel0, flow_control: from), dataType, data, connection)

        replies =
          :lists.map(
            fn {sendDataType, sendData} ->
              {:connection_reply, channel_data_msg(id, sendDataType, sendData)}
            end,
            sendList
          )

        flowCtrlMsgs = flow_control(replies, channel, cache)
        {replies ++ flowCtrlMsgs, connection}

      _ ->
        {[{:channel_request_reply, from, {:error, :closed}}], connection}
    end
  end

  def handle_msg(
        r_ssh_msg_channel_open_confirmation(
          recipient_channel: channelId,
          sender_channel: remoteId,
          initial_window_size: windowSz,
          maximum_packet_size: packetSz
        ),
        r_connection(channel_cache: cache) = connection0,
        _
      ) do
    r_channel(remote_id: :undefined) =
      channel =
      :ssh_client_channel.cache_lookup(
        cache,
        channelId
      )

    :ssh_client_channel.cache_update(
      cache,
      r_channel(channel,
        remote_id: remoteId,
        recv_packet_size:
          max(
            32768,
            min(
              packetSz,
              r_channel(channel, :recv_packet_size)
            )
          ),
        send_window_size: windowSz,
        send_packet_size: packetSz
      )
    )

    reply_msg(channel, connection0, {:open, channelId})
  end

  def handle_msg(
        r_ssh_msg_channel_open_failure(
          recipient_channel: channelId,
          reason: reason,
          description: descr,
          lang: lang
        ),
        r_connection(channel_cache: cache) = connection0,
        _
      ) do
    channel =
      :ssh_client_channel.cache_lookup(
        cache,
        channelId
      )

    :ssh_client_channel.cache_delete(cache, channelId)
    reply_msg(channel, connection0, {:open_error, reason, descr, lang})
  end

  def handle_msg(r_ssh_msg_channel_success(recipient_channel: channelId), connection, _) do
    reply_msg(channelId, connection, :success)
  end

  def handle_msg(r_ssh_msg_channel_failure(recipient_channel: channelId), connection, _) do
    reply_msg(channelId, connection, :failure)
  end

  def handle_msg(r_ssh_msg_channel_eof(recipient_channel: channelId), connection, _) do
    reply_msg(channelId, connection, {:eof, channelId})
  end

  def handle_msg(
        r_ssh_msg_channel_close(recipient_channel: channelId),
        r_connection(channel_cache: cache) = connection0,
        _
      ) do
    case :ssh_client_channel.cache_lookup(
           cache,
           channelId
         ) do
      r_channel(sent_close: closed, remote_id: remoteId, flow_control: flowControl) = channel ->
        :ssh_client_channel.cache_delete(cache, channelId)
        {closeMsg, connection} = reply_msg(channel, connection0, {:closed, channelId})

        connReplyMsgs =
          case closed do
            true ->
              []

            false ->
              remoteCloseMsg = channel_close_msg(remoteId)
              [{:connection_reply, remoteCloseMsg}]
          end

        sendReplyMsgs =
          case flowControl do
            :undefined ->
              []

            from ->
              [{:flow_control, from, {:error, :closed}}]
          end

        replies = connReplyMsgs ++ closeMsg ++ sendReplyMsgs
        {replies, connection}

      :undefined ->
        {[], connection0}
    end
  end

  def handle_msg(r_ssh_msg_channel_data(recipient_channel: channelId, data: data), connection, _) do
    channel_data_reply_msg(channelId, connection, 0, data)
  end

  def handle_msg(
        r_ssh_msg_channel_extended_data(
          recipient_channel: channelId,
          data_type_code: dataType,
          data: data
        ),
        connection,
        _
      ) do
    channel_data_reply_msg(channelId, connection, dataType, data)
  end

  def handle_msg(
        r_ssh_msg_channel_window_adjust(
          recipient_channel: channelId,
          bytes_to_add: add
        ),
        r_connection(channel_cache: cache) = connection,
        _
      ) do
    r_channel(
      send_window_size: size,
      remote_id: remoteId
    ) =
      channel0 =
      :ssh_client_channel.cache_lookup(
        cache,
        channelId
      )

    {sendList, channel} =
      update_send_window(
        r_channel(channel0, send_window_size: size + add),
        0,
        :undefined,
        connection
      )

    replies =
      :lists.map(
        fn {type, data} ->
          {:connection_reply, channel_data_msg(remoteId, type, data)}
        end,
        sendList
      )

    flowCtrlMsgs = flow_control(channel, cache)
    {replies ++ flowCtrlMsgs, connection}
  end

  def handle_msg(
        r_ssh_msg_channel_open(
          channel_type: 'session' = type,
          sender_channel: remoteId,
          initial_window_size: windowSz,
          maximum_packet_size: packetSz
        ),
        r_connection(options: sSHopts) = connection0,
        :server
      ) do
    minAcceptedPackSz =
      :ssh_options.get_value(
        :user_options,
        :minimal_remote_max_packet_size,
        sSHopts,
        :ssh_connection,
        573
      )

    cond do
      minAcceptedPackSz <= packetSz ->
        try do
          setup_session(connection0, remoteId, type, windowSz, packetSz)
        catch
          _, _ ->
            failMsg = channel_open_failure_msg(remoteId, 2, 'Connection refused', 'en')
            {[{:connection_reply, failMsg}], connection0}
        else
          result ->
            result
        end

      minAcceptedPackSz > packetSz ->
        failMsg =
          channel_open_failure_msg(
            remoteId,
            1,
            :lists.concat(['Maximum packet size below ', minAcceptedPackSz, ' not supported']),
            'en'
          )

        {[{:connection_reply, failMsg}], connection0}
    end
  end

  def handle_msg(
        r_ssh_msg_channel_open(
          channel_type: 'forwarded-tcpip',
          sender_channel: remoteId,
          initial_window_size: windowSize,
          maximum_packet_size: packetSize,
          data:
            <<_L1::size(32)-unsigned-big-integer, connectedHost::size(_L1)-binary,
              connectedPort::size(32)-unsigned-big-integer, _L2::size(32)-unsigned-big-integer,
              _OriginHost::size(_L2)-binary, _OriginPort::size(32)-unsigned-big-integer>>
        ),
        r_connection(
          channel_cache: cache,
          channel_id_seed: chId,
          options: options,
          sub_system_supervisor: subSysSup
        ) = c,
        :client
      ) do
    {replyMsg, nextChId} =
      case :ssh_connection_handler.retrieve(
             c,
             {:tcpip_forward, connectedHost, connectedPort}
           ) do
        {:ok, {connectToHost, connectToPort}} ->
          case :gen_tcp.connect(connectToHost, connectToPort, [{:active, false}, :binary]) do
            {:ok, sock} ->
              {:ok, pid} =
                :ssh_subsystem_sup.start_channel(
                  :client,
                  subSysSup,
                  self(),
                  :ssh_tcpip_forward_client,
                  chId,
                  [sock],
                  :undefined,
                  options
                )

              :ssh_client_channel.cache_update(
                cache,
                r_channel(
                  type: 'forwarded-tcpip',
                  sys: 'none',
                  local_id: chId,
                  remote_id: remoteId,
                  user: pid,
                  recv_window_size: 10 * 65536,
                  recv_packet_size: 65536,
                  send_window_size: windowSize,
                  send_packet_size: packetSize,
                  send_buf: :queue.new()
                )
              )

              :gen_tcp.controlling_process(sock, pid)
              :inet.setopts(sock, [{:active, :once}])
              {channel_open_confirmation_msg(remoteId, chId, 10 * 65536, 65536), chId + 1}

            {:error, error} ->
              {channel_open_failure_msg(
                 remoteId,
                 2,
                 :io_lib.format(
                   'Forwarded connection refused: ~p',
                   [error]
                 ),
                 'en'
               ), chId}
          end

        :undefined ->
          {channel_open_failure_msg(
             remoteId,
             2,
             :io_lib.format('No forwarding ordered', []),
             'en'
           ), chId}
      end

    {[{:connection_reply, replyMsg}], r_connection(c, channel_id_seed: nextChId)}
  end

  def handle_msg(
        r_ssh_msg_channel_open(
          channel_type: 'direct-tcpip',
          sender_channel: remoteId,
          initial_window_size: windowSize,
          maximum_packet_size: packetSize,
          data:
            <<_L1::size(32)-unsigned-big-integer, hostToConnect::size(_L1)-binary,
              portToConnect::size(32)-unsigned-big-integer, _L2::size(32)-unsigned-big-integer,
              _OriginatorIPaddress::size(_L2)-binary,
              _OrignatorPort::size(32)-unsigned-big-integer>>
        ),
        r_connection(
          channel_cache: cache,
          channel_id_seed: chId,
          options: options,
          sub_system_supervisor: subSysSup
        ) = c,
        :server
      ) do
    {replyMsg, nextChId} =
      case :ssh_options.get_value(:user_options, :tcpip_tunnel_in, options, :ssh_connection, 669) do
        false ->
          {channel_open_failure_msg(remoteId, 2, 'Forwarding disabled', 'en'), chId}

        true ->
          case :gen_tcp.connect(
                 :erlang.binary_to_list(hostToConnect),
                 portToConnect,
                 [{:active, false}, :binary]
               ) do
            {:ok, sock} ->
              {:ok, pid} =
                :ssh_subsystem_sup.start_channel(
                  :server,
                  subSysSup,
                  self(),
                  :ssh_tcpip_forward_srv,
                  chId,
                  [sock],
                  :undefined,
                  options
                )

              :ssh_client_channel.cache_update(
                cache,
                r_channel(
                  type: 'direct-tcpip',
                  sys: 'none',
                  local_id: chId,
                  remote_id: remoteId,
                  user: pid,
                  recv_window_size: 10 * 65536,
                  recv_packet_size: 65536,
                  send_window_size: windowSize,
                  send_packet_size: packetSize,
                  send_buf: :queue.new()
                )
              )

              :gen_tcp.controlling_process(sock, pid)
              :inet.setopts(sock, [{:active, :once}])
              {channel_open_confirmation_msg(remoteId, chId, 10 * 65536, 65536), chId + 1}

            {:error, error} ->
              {channel_open_failure_msg(
                 remoteId,
                 2,
                 :io_lib.format(
                   'Forwarded connection refused: ~p',
                   [error]
                 ),
                 'en'
               ), chId}
          end
      end

    {[{:connection_reply, replyMsg}], r_connection(c, channel_id_seed: nextChId)}
  end

  def handle_msg(
        r_ssh_msg_channel_open(channel_type: 'session', sender_channel: remoteId),
        connection,
        :client
      ) do
    failMsg = channel_open_failure_msg(remoteId, 2, 'Connection refused', 'en')
    {[{:connection_reply, failMsg}], connection}
  end

  def handle_msg(r_ssh_msg_channel_open(sender_channel: remoteId), connection, _) do
    failMsg = channel_open_failure_msg(remoteId, 1, 'Not allowed', 'en')
    {[{:connection_reply, failMsg}], connection}
  end

  def handle_msg(
        r_ssh_msg_channel_request(
          recipient_channel: channelId,
          request_type: 'exit-status',
          data: data
        ),
        connection,
        _
      ) do
    <<status::size(32)-unsigned-big-integer>> = data
    reply_msg(channelId, connection, {:exit_status, channelId, status})
  end

  def handle_msg(
        r_ssh_msg_channel_request(
          recipient_channel: channelId,
          request_type: 'exit-signal',
          want_reply: false,
          data: data
        ),
        r_connection(channel_cache: cache) = connection0,
        _
      ) do
    <<_SigLen::size(32)-unsigned-big-integer, sigName::size(_SigLen)-binary,
      _Core::size(8)-unsigned-big-integer, _ErrLen::size(32)-unsigned-big-integer,
      err::size(_ErrLen)-binary, _LangLen::size(32)-unsigned-big-integer,
      lang::size(_LangLen)-binary>> = data

    channel =
      :ssh_client_channel.cache_lookup(
        cache,
        channelId
      )

    remoteId = r_channel(channel, :remote_id)

    {reply, connection} =
      reply_msg(
        channel,
        connection0,
        {:exit_signal, channelId, :erlang.binary_to_list(sigName), :erlang.binary_to_list(err),
         :erlang.binary_to_list(lang)}
      )

    closeMsg = channel_close_msg(remoteId)
    {[{:connection_reply, closeMsg} | reply], connection}
  end

  def handle_msg(
        r_ssh_msg_channel_request(
          recipient_channel: channelId,
          request_type: 'xon-xoff',
          want_reply: false,
          data: data
        ),
        connection,
        _
      ) do
    <<cDo::size(8)-unsigned-big-integer>> = data
    reply_msg(channelId, connection, {:xon_xoff, channelId, cDo !== 0})
  end

  def handle_msg(
        r_ssh_msg_channel_request(
          recipient_channel: channelId,
          request_type: 'window-change',
          want_reply: false,
          data: data
        ),
        connection0,
        _
      ) do
    <<width::size(32)-unsigned-big-integer, height::size(32)-unsigned-big-integer,
      pixWidth::size(32)-unsigned-big-integer, pixHeight::size(32)-unsigned-big-integer>> = data

    reply_msg(
      channelId,
      connection0,
      {:window_change, channelId, width, height, pixWidth, pixHeight}
    )
  end

  def handle_msg(
        r_ssh_msg_channel_request(
          recipient_channel: channelId,
          request_type: 'signal',
          data: data
        ),
        connection0,
        _
      ) do
    <<_SigLen::size(32)-unsigned-big-integer, sigName::size(_SigLen)-binary>> = data
    reply_msg(channelId, connection0, {:signal, channelId, :erlang.binary_to_list(sigName)})
  end

  def handle_msg(
        r_ssh_msg_channel_request(
          recipient_channel: channelId,
          request_type: 'subsystem',
          want_reply: wantReply,
          data: data
        ),
        r_connection(channel_cache: cache) = connection,
        :server
      ) do
    <<_SsLen::size(32)-unsigned-big-integer, ssName::size(_SsLen)-binary>> = data

    r_channel(remote_id: remoteId) =
      channel =
      :ssh_client_channel.cache_lookup(
        cache,
        channelId
      )

    reply =
      case start_subsystem(
             ssName,
             connection,
             channel,
             {:subsystem, channelId, wantReply, :erlang.binary_to_list(ssName)}
           ) do
        {:ok, pid} ->
          :erlang.monitor(:process, pid)

          :ssh_client_channel.cache_update(
            cache,
            r_channel(channel, user: pid)
          )

          channel_success_msg(remoteId)

        {:error, _Error} ->
          channel_failure_msg(remoteId)
      end

    {[{:connection_reply, reply}], connection}
  end

  def handle_msg(r_ssh_msg_channel_request(request_type: 'subsystem'), connection, :client) do
    {[], connection}
  end

  def handle_msg(
        r_ssh_msg_channel_request(
          recipient_channel: channelId,
          request_type: 'pty-req',
          want_reply: wantReply,
          data: data
        ),
        connection,
        :server
      ) do
    <<_TermLen::size(32)-unsigned-big-integer, bTermName::size(_TermLen)-binary,
      width::size(32)-unsigned-big-integer, height::size(32)-unsigned-big-integer,
      pixWidth::size(32)-unsigned-big-integer, pixHeight::size(32)-unsigned-big-integer,
      modes::binary>> = data

    termName = :erlang.binary_to_list(bTermName)
    ptyRequest = {termName, width, height, pixWidth, pixHeight, decode_pty_opts(modes)}
    handle_cli_msg(connection, channelId, {:pty, channelId, wantReply, ptyRequest})
  end

  def handle_msg(r_ssh_msg_channel_request(request_type: 'pty-req'), connection, :client) do
    {[], connection}
  end

  def handle_msg(
        r_ssh_msg_channel_request(
          recipient_channel: channelId,
          request_type: 'shell',
          want_reply: wantReply
        ),
        connection,
        :server
      ) do
    handle_cli_msg(connection, channelId, {:shell, channelId, wantReply})
  end

  def handle_msg(r_ssh_msg_channel_request(request_type: 'shell'), connection, :client) do
    {[], connection}
  end

  def handle_msg(
        r_ssh_msg_channel_request(
          recipient_channel: channelId,
          request_type: 'exec',
          want_reply: wantReply,
          data: data
        ),
        connection,
        :server
      ) do
    <<_Len::size(32)-unsigned-big-integer, command::size(_Len)-binary>> = data

    handle_cli_msg(
      connection,
      channelId,
      {:exec, channelId, wantReply, :erlang.binary_to_list(command)}
    )
  end

  def handle_msg(r_ssh_msg_channel_request(request_type: 'exec'), connection, :client) do
    {[], connection}
  end

  def handle_msg(
        r_ssh_msg_channel_request(
          recipient_channel: channelId,
          request_type: 'env',
          want_reply: wantReply,
          data: data
        ),
        connection,
        :server
      ) do
    <<_VarLen::size(32)-unsigned-big-integer, var::size(_VarLen)-binary,
      _ValLen::size(32)-unsigned-big-integer, value::size(_ValLen)-binary>> = data

    handle_cli_msg(connection, channelId, {:env, channelId, wantReply, var, value})
  end

  def handle_msg(r_ssh_msg_channel_request(request_type: 'env'), connection, :client) do
    {[], connection}
  end

  def handle_msg(
        r_ssh_msg_channel_request(recipient_channel: channelId),
        r_connection(channel_cache: cache) = connection,
        _
      ) do
    case :ssh_client_channel.cache_lookup(
           cache,
           channelId
         ) do
      r_channel(remote_id: remoteId) ->
        failMsg = channel_failure_msg(remoteId)
        {[{:connection_reply, failMsg}], connection}

      :undefined ->
        {[], connection}
    end
  end

  def handle_msg(
        r_ssh_msg_global_request(
          name: "tcpip-forward",
          want_reply: wantReply,
          data:
            <<_Len::size(32)-unsigned-big-integer, listenAddrStr::size(_Len)-binary,
              listenPort::size(32)-unsigned-big-integer>>
        ),
        r_connection(options: opts) = connection,
        :server
      ) do
    case :ssh_options.get_value(:user_options, :tcpip_tunnel_out, opts, :ssh_connection, 893) do
      false ->
        {[{:connection_reply, request_failure_msg()}], connection}

      true ->
        sups = :ssh_options.get_value(:internal_options, :supervisors, opts, :ssh_connection, 899)
        subSysSup = :proplists.get_value(:subsystem_sup, sups)
        fwdSup = :ssh_subsystem_sup.tcpip_fwd_supervisor(subSysSup)
        connPid = self()

        case :ssh_tcpip_forward_acceptor.supervised_start(
               fwdSup,
               {listenAddrStr, listenPort},
               :undefined,
               'forwarded-tcpip',
               :ssh_tcpip_forward_srv,
               connPid
             ) do
          {:ok, ^listenPort} when wantReply == true ->
            {[{:connection_reply, request_success_msg(<<>>)}], connection}

          {:ok, lPort} when wantReply == true ->
            {[{:connection_reply, request_success_msg(<<lPort::size(32)-unsigned-big-integer>>)}],
             connection}

          {:error, _} when wantReply == true ->
            {[{:connection_reply, request_failure_msg()}], connection}

          _ when wantReply == true ->
            {[{:connection_reply, request_failure_msg()}], connection}

          _ ->
            {[], connection}
        end
    end
  end

  def handle_msg(
        r_ssh_msg_global_request(name: _Type, want_reply: wantReply, data: _Data),
        connection,
        _Role
      ) do
    cond do
      wantReply == true ->
        failMsg = request_failure_msg()
        {[{:connection_reply, failMsg}], connection}

      true ->
        {[], connection}
    end
  end

  def handle_msg(
        r_ssh_msg_request_failure(),
        r_connection(requests: [{_, from} | rest]) = connection,
        _
      ) do
    {[{:channel_request_reply, from, {:failure, <<>>}}], r_connection(connection, requests: rest)}
  end

  def handle_msg(
        r_ssh_msg_request_failure(),
        r_connection(requests: [{_, from, _} | rest]) = connection,
        _
      ) do
    {[{:channel_request_reply, from, {:failure, <<>>}}], r_connection(connection, requests: rest)}
  end

  def handle_msg(
        r_ssh_msg_request_success(data: data),
        r_connection(requests: [{_, from} | rest]) = connection,
        _
      ) do
    {[{:channel_request_reply, from, {:success, data}}], r_connection(connection, requests: rest)}
  end

  def handle_msg(
        r_ssh_msg_request_success(data: data),
        r_connection(requests: [{_, from, fun} | rest]) = connection0,
        _
      ) do
    connection = fun.({:success, data}, connection0)
    {[{:channel_request_reply, from, {:success, data}}], r_connection(connection, requests: rest)}
  end

  def handle_msg(r_ssh_msg_disconnect(code: code, description: description), connection, _) do
    {:disconnect, {code, description}, handle_stop(connection)}
  end

  def handle_stop(r_connection(channel_cache: cache) = connection0) do
    {connection, replies} =
      :ssh_client_channel.cache_foldl(
        fn channel, {connection1, acc} ->
          {reply, connection2} =
            reply_msg(
              channel,
              connection1,
              {:closed, r_channel(channel, :local_id)}
            )

          {connection2, reply ++ acc}
        end,
        {connection0, []},
        cache
      )

    :ssh_client_channel.cache_delete(cache)
    {replies, connection}
  end

  def channel_adjust_window_msg(channelId, bytes) do
    r_ssh_msg_channel_window_adjust(recipient_channel: channelId, bytes_to_add: bytes)
  end

  def channel_close_msg(channelId) do
    r_ssh_msg_channel_close(recipient_channel: channelId)
  end

  def channel_data_msg(channelId, 0, data) do
    r_ssh_msg_channel_data(recipient_channel: channelId, data: data)
  end

  def channel_data_msg(channelId, type, data) do
    r_ssh_msg_channel_extended_data(
      recipient_channel: channelId,
      data_type_code: type,
      data: data
    )
  end

  def channel_eof_msg(channelId) do
    r_ssh_msg_channel_eof(recipient_channel: channelId)
  end

  def channel_failure_msg(channelId) do
    r_ssh_msg_channel_failure(recipient_channel: channelId)
  end

  def channel_open_msg(type, channelId, windowSize, maxPacketSize, data) do
    r_ssh_msg_channel_open(
      channel_type: type,
      sender_channel: channelId,
      initial_window_size: windowSize,
      maximum_packet_size: maxPacketSize,
      data: data
    )
  end

  def channel_open_confirmation_msg(remoteId, lID, windowSize, packetSize) do
    r_ssh_msg_channel_open_confirmation(
      recipient_channel: remoteId,
      sender_channel: lID,
      initial_window_size: windowSize,
      maximum_packet_size: packetSize
    )
  end

  def channel_open_failure_msg(remoteId, reason, description, lang) do
    r_ssh_msg_channel_open_failure(
      recipient_channel: remoteId,
      reason: reason,
      description: description,
      lang: lang
    )
  end

  def channel_status_msg({:success, channelId}) do
    channel_success_msg(channelId)
  end

  def channel_status_msg({:failure, channelId}) do
    channel_failure_msg(channelId)
  end

  def channel_request_msg(channelId, type, wantReply, data) do
    r_ssh_msg_channel_request(
      recipient_channel: channelId,
      request_type: type,
      want_reply: wantReply,
      data: data
    )
  end

  def channel_success_msg(channelId) do
    r_ssh_msg_channel_success(recipient_channel: channelId)
  end

  def request_global_msg(name, wantReply, data) do
    r_ssh_msg_global_request(name: name, want_reply: wantReply, data: data)
  end

  def request_failure_msg() do
    r_ssh_msg_request_failure()
  end

  def request_success_msg(data) do
    r_ssh_msg_request_success(data: data)
  end

  def encode_ip(addr) when is_tuple(addr) do
    case (try do
            :inet_parse.ntoa(addr)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        false

      a ->
        a
    end
  end

  def encode_ip(addr) when is_list(addr) do
    case :inet_parse.address(addr) do
      {:ok, _} ->
        addr

      error ->
        case :inet.getaddr(addr, :inet) do
          {:ok, a} ->
            :inet_parse.ntoa(a)

          ^error ->
            false
        end
    end
  end

  defp setup_session(
         r_connection(
           channel_cache: cache,
           channel_id_seed: newChannelID
         ) = c,
         remoteId,
         type,
         windowSize,
         packetSize
       ) do
    nextChannelID = newChannelID + 1

    channel =
      r_channel(
        type: type,
        sys: 'ssh',
        local_id: newChannelID,
        recv_window_size: 10 * 65536,
        recv_packet_size: 65536,
        send_window_size: windowSize,
        send_packet_size: packetSize,
        send_buf: :queue.new(),
        remote_id: remoteId
      )

    :ssh_client_channel.cache_update(cache, channel)
    openConfMsg = channel_open_confirmation_msg(remoteId, newChannelID, 10 * 65536, 65536)
    reply = {:connection_reply, openConfMsg}
    {[reply], r_connection(c, channel_id_seed: nextChannelID)}
  end

  defp start_cli(
         r_connection(
           options: options,
           cli_spec: cliSpec,
           exec: exec,
           sub_system_supervisor: subSysSup
         ),
         channelId
       ) do
    case cliSpec do
      :no_cli ->
        {:error, :cli_disabled}

      {cbModule, args} ->
        :ssh_subsystem_sup.start_channel(
          :server,
          subSysSup,
          self(),
          cbModule,
          channelId,
          args,
          exec,
          options
        )
    end
  end

  defp start_subsystem(
         binName,
         r_connection(options: options, sub_system_supervisor: subSysSup),
         r_channel(local_id: channelId),
         _ReplyMsg
       ) do
    name = :erlang.binary_to_list(binName)

    case check_subsystem(name, options) do
      {callback, opts}
      when is_atom(callback) and
             callback !== :none ->
        :ssh_subsystem_sup.start_channel(
          :server,
          subSysSup,
          self(),
          callback,
          channelId,
          opts,
          :undefined,
          options
        )

      {:none, _} ->
        {:error, :bad_subsystem}

      {_, _} ->
        {:error, :legacy_option_not_supported}
    end
  end

  defp check_subsystem('sftp' = ssName, options) do
    case :ssh_options.get_value(:user_options, :subsystems, options, :ssh_connection, 1137) do
      :no_subsys ->
        {^ssName, {cb, opts}} = :ssh_sftpd.subsystem_spec([])
        {cb, opts}

      subSystems ->
        :proplists.get_value(ssName, subSystems, {:none, []})
    end
  end

  defp check_subsystem(ssName, options) do
    subsystems =
      :ssh_options.get_value(:user_options, :subsystems, options, :ssh_connection, 1146)

    case :proplists.get_value(ssName, subsystems, {:none, []}) do
      fun when is_function(fun) ->
        {fun, []}

      {_, _} = value ->
        value
    end
  end

  defp update_send_window(channel, _, :undefined, r_connection(channel_cache: cache)) do
    do_update_send_window(channel, cache)
  end

  defp update_send_window(
         r_channel(send_buf: sendBuffer) = channel,
         dataType,
         data,
         r_connection(channel_cache: cache)
       ) do
    do_update_send_window(
      r_channel(channel,
        send_buf:
          :queue.in(
            {dataType, data},
            sendBuffer
          )
      ),
      cache
    )
  end

  defp do_update_send_window(channel0, cache) do
    {sendMsgs, channel} = get_window(channel0, [])
    :ssh_client_channel.cache_update(cache, channel)
    {sendMsgs, channel}
  end

  defp get_window(r_channel(send_window_size: 0) = channel, acc) do
    {:lists.reverse(acc), channel}
  end

  defp get_window(r_channel(send_packet_size: 0) = channel, acc) do
    {:lists.reverse(acc), channel}
  end

  defp get_window(
         r_channel(
           send_buf: buffer,
           send_packet_size: packetSize,
           send_window_size: windowSize0
         ) = channel,
         acc0
       ) do
    case :queue.out(buffer) do
      {{:value, {_, data} = msg}, newBuffer} ->
        case handle_send_window(msg, :erlang.size(data), packetSize, windowSize0, acc0) do
          {windowSize, acc, {_, <<>>}} ->
            {:lists.reverse(acc),
             r_channel(channel,
               send_window_size: windowSize,
               send_buf: newBuffer
             )}

          {windowSize, acc, rest} ->
            get_window(
              r_channel(channel,
                send_window_size: windowSize,
                send_buf: :queue.in_r(rest, newBuffer)
              ),
              acc
            )
        end

      {:empty, newBuffer} ->
        {[], r_channel(channel, send_buf: newBuffer)}
    end
  end

  defp handle_send_window(msg = {type, data}, size, packetSize, windowSize, acc)
       when size <= windowSize do
    case size <= packetSize do
      true ->
        {windowSize - size, [msg | acc], {type, <<>>}}

      false ->
        <<msg1::size(packetSize)-binary, msg2::binary>> = data
        {windowSize - packetSize, [{type, msg1} | acc], {type, msg2}}
    end
  end

  defp handle_send_window({type, data}, _, packetSize, windowSize, acc)
       when windowSize <= packetSize do
    <<msg1::size(windowSize)-binary, msg2::binary>> = data
    {windowSize - windowSize, [{type, msg1} | acc], {type, msg2}}
  end

  defp handle_send_window({type, data}, _, packetSize, windowSize, acc) do
    <<msg1::size(packetSize)-binary, msg2::binary>> = data
    {windowSize - packetSize, [{type, msg1} | acc], {type, msg2}}
  end

  defp flow_control(channel, cache) do
    flow_control([:window_adjusted], channel, cache)
  end

  defp flow_control([], channel, cache) do
    :ssh_client_channel.cache_update(cache, channel)
    []
  end

  defp flow_control(
         [_ | _],
         r_channel(flow_control: from, send_buf: buffer) = channel,
         cache
       )
       when from !== :undefined do
    case :queue.is_empty(buffer) do
      true ->
        :ssh_client_channel.cache_update(
          cache,
          r_channel(channel, flow_control: :undefined)
        )

        [{:flow_control, cache, channel, from, :ok}]

      false ->
        []
    end
  end

  defp flow_control(_, _, _) do
    []
  end

  defp pty_req(
         connectionHandler,
         channel,
         term,
         width,
         height,
         pixWidth,
         pixHeight,
         ptyOpts,
         timeOut
       ) do
    :ssh_connection_handler.request(
      connectionHandler,
      channel,
      'pty-req',
      true,
      [
        <<:erlang.size(:unicode.characters_to_binary(term))::size(32)-unsigned-big-integer,
          :unicode.characters_to_binary(term)::binary>>,
        <<width::size(32)-unsigned-big-integer>>,
        <<height::size(32)-unsigned-big-integer>>,
        <<pixWidth::size(32)-unsigned-big-integer>>,
        <<pixHeight::size(32)-unsigned-big-integer>>,
        encode_pty_opts(ptyOpts)
      ],
      timeOut
    )
  end

  defp pty_default_dimensions(dimension, termData) do
    case :proplists.get_value(dimension, termData, 0) do
      n when is_integer(n) and n > 0 ->
        {n, 0}

      _ ->
        pixelDim = :erlang.list_to_atom('pixel_' ++ :erlang.atom_to_list(dimension))

        case :proplists.get_value(pixelDim, termData, 0) do
          n when is_integer(n) and n > 0 ->
            {0, n}

          _ ->
            {80, 0}
        end
    end
  end

  def encode_pty_opts(opts) do
    bin = :erlang.list_to_binary(encode_pty_opts2(opts))
    <<:erlang.size(bin)::size(32)-unsigned-big-integer, bin::binary>>
  end

  defp encode_pty_opts2([]) do
    [0]
  end

  defp encode_pty_opts2([{:vintr, value} | opts]) do
    [
      [1, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:vquit, value} | opts]) do
    [
      [2, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:verase, value} | opts]) do
    [
      [3, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:vkill, value} | opts]) do
    [
      [4, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:veof, value} | opts]) do
    [
      [5, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:veol, value} | opts]) do
    [
      [6, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:veol2, value} | opts]) do
    [
      [7, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:vstart, value} | opts]) do
    [
      [8, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:vstop, value} | opts]) do
    [
      [9, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:vsusp, value} | opts]) do
    [
      [10, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:vdsusp, value} | opts]) do
    [
      [11, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:vreprint, value} | opts]) do
    [
      [12, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:vwerase, value} | opts]) do
    [
      [13, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:vlnext, value} | opts]) do
    [
      [14, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:vflush, value} | opts]) do
    [
      [15, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:vswtch, value} | opts]) do
    [
      [16, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:vstatus, value} | opts]) do
    [
      [17, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:vdiscard, value} | opts]) do
    [
      [18, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:ignpar, value} | opts]) do
    [
      [30, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:parmrk, value} | opts]) do
    [
      [31, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:inpck, value} | opts]) do
    [
      [32, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:istrip, value} | opts]) do
    [
      [33, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:inlcr, value} | opts]) do
    [
      [34, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:igncr, value} | opts]) do
    [
      [35, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:icrnl, value} | opts]) do
    [
      [36, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:iuclc, value} | opts]) do
    [
      [37, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:ixon, value} | opts]) do
    [
      [38, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:ixany, value} | opts]) do
    [
      [39, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:ixoff, value} | opts]) do
    [
      [40, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:imaxbel, value} | opts]) do
    [
      [41, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:isig, value} | opts]) do
    [
      [50, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:icanon, value} | opts]) do
    [
      [51, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:xcase, value} | opts]) do
    [
      [52, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:echo, value} | opts]) do
    [
      [53, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:echoe, value} | opts]) do
    [
      [54, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:echok, value} | opts]) do
    [
      [55, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:echonl, value} | opts]) do
    [
      [56, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:noflsh, value} | opts]) do
    [
      [57, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:tostop, value} | opts]) do
    [
      [58, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:iexten, value} | opts]) do
    [
      [59, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:echoctl, value} | opts]) do
    [
      [60, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:echoke, value} | opts]) do
    [
      [61, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:pendin, value} | opts]) do
    [
      [62, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:opost, value} | opts]) do
    [
      [70, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:olcuc, value} | opts]) do
    [
      [71, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:onlcr, value} | opts]) do
    [
      [72, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:ocrnl, value} | opts]) do
    [
      [73, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:onocr, value} | opts]) do
    [
      [74, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:onlret, value} | opts]) do
    [
      [75, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:cs7, value} | opts]) do
    [
      [90, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:cs8, value} | opts]) do
    [
      [91, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:parenb, value} | opts]) do
    [
      [92, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:parodd, value} | opts]) do
    [
      [93, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:tty_op_ispeed, value} | opts]) do
    [
      [128, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  defp encode_pty_opts2([{:tty_op_ospeed, value} | opts]) do
    [
      [129, <<value::size(32)-unsigned-big-integer>>]
      | encode_pty_opts2(opts)
    ]
  end

  def decode_pty_opts(<<>>) do
    []
  end

  def decode_pty_opts(<<0, 0, 0, 0>>) do
    []
  end

  def decode_pty_opts(<<_Len::size(32)-unsigned-big-integer, modes::size(_Len)-binary>>) do
    decode_pty_opts2(modes)
  end

  def decode_pty_opts(binary) do
    decode_pty_opts2(binary)
  end

  defp decode_pty_opts2(<<0>>) do
    []
  end

  defp decode_pty_opts2(<<code, value::size(32)-unsigned-big-integer, tail::binary>>) do
    op =
      case code do
        1 ->
          :vintr

        2 ->
          :vquit

        3 ->
          :verase

        4 ->
          :vkill

        5 ->
          :veof

        6 ->
          :veol

        7 ->
          :veol2

        8 ->
          :vstart

        9 ->
          :vstop

        10 ->
          :vsusp

        11 ->
          :vdsusp

        12 ->
          :vreprint

        13 ->
          :vwerase

        14 ->
          :vlnext

        15 ->
          :vflush

        16 ->
          :vswtch

        17 ->
          :vstatus

        18 ->
          :vdiscard

        30 ->
          :ignpar

        31 ->
          :parmrk

        32 ->
          :inpck

        33 ->
          :istrip

        34 ->
          :inlcr

        35 ->
          :igncr

        36 ->
          :icrnl

        37 ->
          :iuclc

        38 ->
          :ixon

        39 ->
          :ixany

        40 ->
          :ixoff

        41 ->
          :imaxbel

        50 ->
          :isig

        51 ->
          :icanon

        52 ->
          :xcase

        53 ->
          :echo

        54 ->
          :echoe

        55 ->
          :echok

        56 ->
          :echonl

        57 ->
          :noflsh

        58 ->
          :tostop

        59 ->
          :iexten

        60 ->
          :echoctl

        61 ->
          :echoke

        62 ->
          :pendin

        70 ->
          :opost

        71 ->
          :olcuc

        72 ->
          :onlcr

        73 ->
          :ocrnl

        74 ->
          :onocr

        75 ->
          :onlret

        90 ->
          :cs7

        91 ->
          :cs8

        92 ->
          :parenb

        93 ->
          :parodd

        128 ->
          :tty_op_ispeed

        129 ->
          :tty_op_ospeed

        _ ->
          code
      end

    [{op, value} | decode_pty_opts2(tail)]
  end

  defp backwards_compatible([], acc) do
    acc
  end

  defp backwards_compatible([{:hight, value} | rest], acc) do
    backwards_compatible(rest, [{:height, value} | acc])
  end

  defp backwards_compatible([{:pixel_hight, value} | rest], acc) do
    backwards_compatible(rest, [{:height, value} | acc])
  end

  defp backwards_compatible([value | rest], acc) do
    backwards_compatible(rest, [value | acc])
  end

  defp handle_cli_msg(c0, chId, reply0) do
    cache = r_connection(c0, :channel_cache)
    ch0 = :ssh_client_channel.cache_lookup(cache, chId)

    case r_channel(ch0, :user) do
      :undefined ->
        case start_cli(c0, chId) do
          {:ok, pid} ->
            :erlang.monitor(:process, pid)
            ch = r_channel(ch0, user: pid)
            :ssh_client_channel.cache_update(cache, ch)
            reply_msg(ch, c0, reply0)

          {:error, _Error} ->
            reply = {:connection_reply, channel_failure_msg(r_channel(ch0, :remote_id))}
            {[reply], c0}
        end

      _ ->
        reply_msg(ch0, c0, reply0)
    end
  end

  defp channel_data_reply_msg(channelId, connection, dataType, data) do
    case :ssh_client_channel.cache_lookup(
           r_connection(connection, :channel_cache),
           channelId
         ) do
      r_channel(recv_window_size: size) = channel ->
        wantedSize = size - :erlang.size(data)

        :ssh_client_channel.cache_update(
          r_connection(connection, :channel_cache),
          r_channel(channel, recv_window_size: wantedSize)
        )

        reply_msg(channel, connection, {:data, channelId, dataType, data})

      :undefined ->
        {[], connection}
    end
  end

  defp reply_msg(chId, c, reply) when is_integer(chId) do
    reply_msg(
      :ssh_client_channel.cache_lookup(
        r_connection(c, :channel_cache),
        chId
      ),
      c,
      reply
    )
  end

  defp reply_msg(channel, connection, {:open, _} = reply) do
    request_reply_or_data(channel, connection, reply)
  end

  defp reply_msg(channel, connection, {:open_error, _, _, _} = reply) do
    request_reply_or_data(channel, connection, reply)
  end

  defp reply_msg(channel, connection, :success = reply) do
    request_reply_or_data(channel, connection, reply)
  end

  defp reply_msg(channel, connection, :failure = reply) do
    request_reply_or_data(channel, connection, reply)
  end

  defp reply_msg(channel, connection, {:closed, _} = reply) do
    request_reply_or_data(channel, connection, reply)
  end

  defp reply_msg(:undefined, connection, _Reply) do
    {[], connection}
  end

  defp reply_msg(r_channel(user: channelPid), connection, reply) do
    {[{:channel_data, channelPid, reply}], connection}
  end

  defp request_reply_or_data(
         r_channel(local_id: channelId, user: channelPid),
         r_connection(requests: requests) = connection,
         reply
       ) do
    case :lists.keysearch(channelId, 1, requests) do
      {:value, {^channelId, from}} ->
        {[{:channel_request_reply, from, reply}],
         r_connection(connection, requests: :lists.keydelete(channelId, 1, requests))}

      false when reply == :success or reply == :failure ->
        {[], connection}

      false ->
        {[{:channel_data, channelPid, reply}], connection}
    end
  end

  def send_environment_vars(connectionHandler, channel, varNames) do
    :lists.foldl(
      fn var, :success ->
        case :os.getenv(var) do
          false ->
            :success

          value ->
            setenv(connectionHandler, channel, false, var, value, :infinity)
        end
      end,
      :success,
      varNames
    )
  end
end
