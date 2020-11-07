defmodule :m_ssh_cli do
  use Bitwise
  @behaviour :ssh_server_channel
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

  @behaviour :ssh_dbg
  Record.defrecord(:r_state, :state,
    cm: :undefined,
    channel: :undefined,
    pty: :undefined,
    encoding: :undefined,
    deduced_encoding: :undefined,
    group: :undefined,
    buf: :undefined,
    shell: :undefined,
    exec: :undefined
  )

  def init([shell, exec]) do
    {:ok, r_state(shell: shell, exec: exec)}
  end

  def init([shell]) do
    {:ok, r_state(shell: shell)}
  end

  def handle_ssh_msg(
        {:ssh_cm, _ConnectionHandler, {:data, _ChannelId, _Type, data}},
        r_state(group: group) = state0
      ) do
    {enc, state} = guess_encoding(data, state0)
    list = :unicode.characters_to_list(data, enc)
    to_group(list, group)
    {:ok, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, connectionHandler,
         {:pty, channelId, wantReply, {termName, width, height, pixWidth, pixHeight, modes}}},
        state0
      ) do
    state =
      r_state(state0,
        pty:
          r_ssh_pty(
            term: termName,
            width: not_zero(width, 80),
            height: not_zero(height, 24),
            pixel_width: pixWidth,
            pixel_height: pixHeight,
            modes: modes
          ),
        buf: empty_buf()
      )

    set_echo(state)
    :ssh_connection.reply_request(connectionHandler, wantReply, :success, channelId)
    {:ok, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, connectionHandler, {:env, channelId, wantReply, var, value}},
        state = r_state(encoding: enc0)
      ) do
    :ssh_connection.reply_request(connectionHandler, wantReply, :failure, channelId)

    enc =
      case var do
        "LANG" when enc0 == :undefined ->
          case claim_encoding(value) do
            {:ok, enc1} ->
              enc1

            _ ->
              enc0
          end

        "LC_ALL" ->
          case claim_encoding(value) do
            {:ok, enc1} ->
              enc1

            _ ->
              enc0
          end

        _ ->
          enc0
      end

    {:ok, r_state(state, encoding: enc)}
  end

  def handle_ssh_msg(
        {:ssh_cm, connectionHandler,
         {:window_change, channelId, width, height, pixWidth, pixHeight}},
        r_state(buf: buf, pty: pty0) = state
      ) do
    pty =
      r_ssh_pty(pty0, width: width, height: height, pixel_width: pixWidth, pixel_height: pixHeight)

    {chars, newBuf} = io_request({:window_change, pty0}, buf, pty, :undefined)
    write_chars(connectionHandler, channelId, chars)
    {:ok, r_state(state, pty: pty, buf: newBuf)}
  end

  def handle_ssh_msg(
        {:ssh_cm, connectionHandler, {:shell, channelId, wantReply}},
        r_state(shell: :disabled) = state
      ) do
    write_chars(connectionHandler, channelId, 1, 'Prohibited.')
    :ssh_connection.reply_request(connectionHandler, wantReply, :success, channelId)
    :ssh_connection.exit_status(connectionHandler, channelId, 255)
    :ssh_connection.send_eof(connectionHandler, channelId)
    {:stop, channelId, r_state(state, channel: channelId, cm: connectionHandler)}
  end

  def handle_ssh_msg(
        {:ssh_cm, connectionHandler, {:shell, channelId, wantReply}},
        state0
      ) do
    state =
      case r_state(state0, :encoding) do
        :undefined ->
          r_state(state0, encoding: :utf8)

        _ ->
          state0
      end

    newState = start_shell(connectionHandler, state)
    :ssh_connection.reply_request(connectionHandler, wantReply, :success, channelId)
    {:ok, r_state(newState, channel: channelId, cm: connectionHandler)}
  end

  def handle_ssh_msg(
        {:ssh_cm, connectionHandler, {:exec, channelId, wantReply, cmd0}},
        s0
      ) do
    {enc, s1} = guess_encoding(cmd0, s0)
    cmd = :unicode.characters_to_list(cmd0, enc)

    case (case r_state(s1, :exec) do
            :disabled ->
              {'Prohibited.', 255, 1}

            {:direct, f} ->
              exec_direct(connectionHandler, channelId, cmd, f, wantReply, s1)

            :undefined
            when r_state(s0, :shell) == {:shell, :start, []} or
                   r_state(s0, :shell) == :disabled ->
              exec_in_erlang_default_shell(connectionHandler, channelId, cmd, wantReply, s1)

            :undefined ->
              {'Prohibited.', 255, 1}

            _ ->
              s2 = start_exec_shell(connectionHandler, cmd, s1)
              :ssh_connection.reply_request(connectionHandler, wantReply, :success, channelId)
              {:ok, s2}
          end) do
      {reply, status, type} ->
        write_chars(
          connectionHandler,
          channelId,
          type,
          :unicode.characters_to_binary(reply, :utf8, out_enc(s1))
        )

        :ssh_connection.reply_request(connectionHandler, wantReply, :success, channelId)
        :ssh_connection.exit_status(connectionHandler, channelId, status)
        :ssh_connection.send_eof(connectionHandler, channelId)
        {:stop, channelId, r_state(s1, channel: channelId, cm: connectionHandler)}

      {:ok, s} ->
        {:ok, r_state(s, channel: channelId, cm: connectionHandler)}
    end
  end

  def handle_ssh_msg(
        {:ssh_cm, _ConnectionHandler, {:eof, _ChannelId}},
        state
      ) do
    {:ok, state}
  end

  def handle_ssh_msg({:ssh_cm, _, {:signal, _, _}}, state) do
    {:ok, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, _, {:exit_signal, channelId, _, error, _}},
        state
      ) do
    report = :io_lib.format('Connection closed by peer ~n Error ~p~n', [error])
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
        {:ssh_channel_up, channelId, connectionHandler},
        r_state(channel: channelId, cm: connectionHandler) = state
      ) do
    {:ok, state}
  end

  def handle_msg({group, :set_unicode_state, _Arg}, state) do
    send(group, {self(), :set_unicode_state, false})
    {:ok, state}
  end

  def handle_msg({group, :get_unicode_state}, state) do
    send(group, {self(), :get_unicode_state, false})
    {:ok, state}
  end

  def handle_msg(
        {group, :tty_geometry},
        r_state(group: group, pty: pty) = state
      ) do
    case pty do
      r_ssh_pty(width: width, height: height) ->
        send(group, {self(), :tty_geometry, {width, height}})

      _ ->
        send(group, {self(), :tty_geometry, {0, 0}})
    end

    {:ok, state}
  end

  def handle_msg(
        {group, req},
        r_state(group: group, buf: buf, pty: pty, cm: connectionHandler, channel: channelId) =
          state
      ) do
    {chars0, newBuf} = io_request(req, buf, pty, group)
    chars = :unicode.characters_to_binary(chars0, :utf8, out_enc(state))
    write_chars(connectionHandler, channelId, chars)
    {:ok, r_state(state, buf: newBuf)}
  end

  def handle_msg(
        {:EXIT, group, reason},
        r_state(group: group, cm: connectionHandler, channel: channelId) = state
      ) do
    :ssh_connection.send_eof(connectionHandler, channelId)

    exitStatus =
      case reason do
        :normal ->
          0

        {:exit_status, v} when is_integer(v) ->
          v

        _ ->
          255
      end

    :ssh_connection.exit_status(connectionHandler, channelId, exitStatus)
    {:stop, channelId, state}
  end

  def handle_msg(_, state) do
    {:ok, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  defp claim_encoding(<<"/", _::binary>>) do
    :undefined
  end

  defp claim_encoding(envValue) do
    try do
      :string.tokens(:erlang.binary_to_list(envValue), '.')
    catch
      _, _ ->
        :undefined
    else
      [_, 'UTF-8'] ->
        {:ok, :utf8}

      [_, 'ISO-8859-1'] ->
        {:ok, :latin1}

      _ ->
        :undefined
    end
  end

  defp guess_encoding(
         data0,
         r_state(
           encoding: peerEnc0,
           deduced_encoding: testEnc0
         ) = state
       ) do
    enc =
      case {peerEnc0, testEnc0} do
        {:latin1, _} ->
          :latin1

        {_, :latin1} ->
          :latin1

        _ ->
          case :unicode.characters_to_binary(data0, :utf8, :utf8) do
            ^data0 ->
              :utf8

            _ ->
              :latin1
          end
      end

    case testEnc0 do
      ^enc ->
        {enc, state}

      :latin1 ->
        {enc, state}

      :utf8 when enc == :latin1 ->
        {enc, r_state(state, deduced_encoding: :latin1)}

      :undefined ->
        {enc, r_state(state, deduced_encoding: enc)}
    end
  end

  defp out_enc(
         r_state(
           encoding: peerEnc,
           deduced_encoding: deducedEnc
         )
       ) do
    case deducedEnc do
      :undefined ->
        peerEnc

      _ ->
        deducedEnc
    end
  end

  defp to_group([], _Group) do
    :ok
  end

  defp to_group([3 | tail], group) do
    :erlang.exit(group, :interrupt)
    to_group(tail, group)
  end

  defp to_group(data, group) do
    func = fn c ->
      c != 3
    end

    tail =
      case :lists.splitwith(func, data) do
        {[], right} ->
          right

        {left, right} ->
          send(group, {self(), {:data, left}})
          right
      end

    to_group(tail, group)
  end

  defp io_request({:window_change, oldTty}, buf, tty, _Group) do
    window_change(tty, oldTty, buf)
  end

  defp io_request({:put_chars, cs}, buf, tty, _Group) do
    put_chars(bin_to_list(cs), buf, tty)
  end

  defp io_request({:put_chars, :unicode, cs}, buf, tty, _Group) do
    put_chars(:unicode.characters_to_list(cs, :unicode), buf, tty)
  end

  defp io_request({:insert_chars, cs}, buf, tty, _Group) do
    insert_chars(bin_to_list(cs), buf, tty)
  end

  defp io_request({:insert_chars, :unicode, cs}, buf, tty, _Group) do
    insert_chars(:unicode.characters_to_list(cs, :unicode), buf, tty)
  end

  defp io_request({:move_rel, n}, buf, tty, _Group) do
    move_rel(n, buf, tty)
  end

  defp io_request({:delete_chars, n}, buf, tty, _Group) do
    delete_chars(n, buf, tty)
  end

  defp io_request(:beep, buf, _Tty, _Group) do
    {[7], buf}
  end

  defp io_request({:get_geometry, :columns}, buf, tty, _Group) do
    {:ok, r_ssh_pty(tty, :width), buf}
  end

  defp io_request({:get_geometry, :rows}, buf, tty, _Group) do
    {:ok, r_ssh_pty(tty, :height), buf}
  end

  defp io_request({:requests, rs}, buf, tty, group) do
    io_requests(rs, buf, tty, [], group)
  end

  defp io_request(:tty_geometry, buf, tty, group) do
    io_requests([{:move_rel, 0}, {:put_chars, :unicode, [10]}], buf, tty, [], group)
  end

  defp io_request({:put_chars_sync, class, cs, reply}, buf, tty, group) do
    send(group, {:reply, reply})
    io_request({:put_chars, class, cs}, buf, tty, group)
  end

  defp io_request(_R, buf, _Tty, _Group) do
    {[], buf}
  end

  defp io_requests([r | rs], buf, tty, acc, group) do
    {chars, newBuf} = io_request(r, buf, tty, group)
    io_requests(rs, newBuf, tty, [acc | chars], group)
  end

  defp io_requests([], buf, _Tty, acc, _Group) do
    {acc, buf}
  end

  defp ansi_tty(n, l) do
    ['\e[', :erlang.integer_to_list(n), l]
  end

  defp get_tty_command(:up, n, _TerminalType) do
    ansi_tty(n, ?A)
  end

  defp get_tty_command(:down, n, _TerminalType) do
    ansi_tty(n, ?B)
  end

  defp get_tty_command(:right, n, _TerminalType) do
    ansi_tty(n, ?C)
  end

  defp get_tty_command(:left, n, _TerminalType) do
    ansi_tty(n, ?D)
  end

  defp conv_buf([], accBuf, accBufTail, accWrite, col) do
    {accBuf, accBufTail, :lists.reverse(accWrite), col}
  end

  defp conv_buf([[13, 10] | rest], _AccBuf, accBufTail, accWrite, _Col) do
    conv_buf(rest, [], tl2(accBufTail), [[10, 13] | accWrite], 0)
  end

  defp conv_buf([13 | rest], _AccBuf, accBufTail, accWrite, _Col) do
    conv_buf(rest, [], tl1(accBufTail), [13 | accWrite], 0)
  end

  defp conv_buf([10 | rest], _AccBuf, accBufTail, accWrite, _Col) do
    conv_buf(rest, [], tl1(accBufTail), [[10, 13] | accWrite], 0)
  end

  defp conv_buf([c | rest], accBuf, accBufTail, accWrite, col) do
    conv_buf(rest, [c | accBuf], tl1(accBufTail), [c | accWrite], col + 1)
  end

  defp put_chars(chars, {buf, bufTail, col}, _Tty) do
    {newBuf, newBufTail, writeBuf, newCol} = conv_buf(chars, buf, bufTail, [], col)
    {writeBuf, {newBuf, newBufTail, newCol}}
  end

  defp insert_chars([], {buf, bufTail, col}, _Tty) do
    {[], {buf, bufTail, col}}
  end

  defp insert_chars(chars, {buf, bufTail, col}, tty) do
    {newBuf, _NewBufTail, writeBuf, newCol} = conv_buf(chars, buf, [], [], col)

    m =
      move_cursor(
        special_at_width(
          newCol + length(bufTail),
          tty
        ),
        newCol,
        tty
      )

    {[[writeBuf, bufTail] | m], {newBuf, bufTail, newCol}}
  end

  defp delete_chars(0, {buf, bufTail, col}, _Tty) do
    {[], {buf, bufTail, col}}
  end

  defp delete_chars(n, {buf, bufTail, col}, tty) when n > 0 do
    newBufTail = nthtail(n, bufTail)
    m = move_cursor(col + length(newBufTail) + n, col, tty)
    {[[newBufTail, :lists.duplicate(n, ?\s)] | m], {buf, newBufTail, col}}
  end

  defp delete_chars(n, {buf, bufTail, col}, tty) do
    newBuf = nthtail(-n, buf)

    newCol =
      case col + n do
        v when v >= 0 ->
          v

        _ ->
          0
      end

    m1 = move_cursor(col, newCol, tty)

    m2 =
      move_cursor(
        special_at_width(
          newCol + length(bufTail) - n,
          tty
        ),
        newCol,
        tty
      )

    {[[m1, bufTail, :lists.duplicate(-n, ?\s)] | m2], {newBuf, bufTail, newCol}}
  end

  defp window_change(tty, oldTty, buf)
       when r_ssh_pty(oldTty, :width) == r_ssh_pty(tty, :width) do
    {[], buf}
  end

  defp window_change(tty, oldTty, {buf, bufTail, col}) do
    case r_ssh_pty(oldTty, :width) - r_ssh_pty(tty, :width) do
      0 ->
        {[], {buf, bufTail, col}}

      deltaW0 when deltaW0 < 0 and bufTail == [] ->
        {[], {buf, bufTail, col}}

      deltaW0 when deltaW0 < 0 and bufTail !== [] ->
        {[], {buf, bufTail, col}}

      deltaW0 when deltaW0 > 0 ->
        {[], {buf, bufTail, col}}
    end
  end

  defp step_over(0, buf, [10 | bufTail], col) do
    {[10 | buf], bufTail, col + 1}
  end

  defp step_over(0, buf, bufTail, col) do
    {buf, bufTail, col}
  end

  defp step_over(n, [c | buf], bufTail, col) when n < 0 do
    n1 = ifelse(c == 10, n, n + 1)
    step_over(n1, buf, [c | bufTail], col - 1)
  end

  defp step_over(n, buf, [c | bufTail], col) when n > 0 do
    n1 = ifelse(c == 10, n, n - 1)
    step_over(n1, [c | buf], bufTail, col + 1)
  end

  defp empty_buf() do
    {[], [], 0}
  end

  defp col(n, w) do
    rem(n, w)
  end

  defp row(n, w) do
    div(n, w)
  end

  defp move_rel(n, {buf, bufTail, col}, tty) do
    {newBuf, newBufTail, newCol} = step_over(n, buf, bufTail, col)
    m = move_cursor(col, newCol, tty)
    {m, {newBuf, newBufTail, newCol}}
  end

  defp move_cursor(a, a, _Tty) do
    []
  end

  defp move_cursor(from, to, r_ssh_pty(width: width, term: type)) do
    tcol =
      case col(to, width) - col(from, width) do
        0 ->
          ''

        i when i < 0 ->
          get_tty_command(:left, -i, type)

        i ->
          get_tty_command(:right, i, type)
      end

    trow =
      case row(to, width) - row(from, width) do
        0 ->
          ''

        j when j < 0 ->
          get_tty_command(:up, -j, type)

        j ->
          get_tty_command(:down, j, type)
      end

    [tcol | trow]
  end

  defp special_at_width(from0, r_ssh_pty(width: width))
       when rem(from0, width) == 0 do
    from0 - 1
  end

  defp special_at_width(from0, _) do
    from0
  end

  defp write_chars(connectionHandler, channelId, chars) do
    write_chars(connectionHandler, channelId, 0, chars)
  end

  defp write_chars(connectionHandler, channelId, type, chars) do
    case has_chars(chars) do
      false ->
        :ok

      true ->
        :ssh_connection.send(connectionHandler, channelId, type, chars)
    end
  end

  defp has_chars([c | _]) when is_integer(c) do
    true
  end

  defp has_chars([h | t]) when is_list(h) or is_binary(h) do
    has_chars(h) or has_chars(t)
  end

  defp has_chars(<<_::size(8), _::binary>>) do
    true
  end

  defp has_chars(_) do
    false
  end

  defp tl1([_ | a]) do
    a
  end

  defp tl1(_) do
    []
  end

  defp tl2([[_, _] | a]) do
    a
  end

  defp tl2(_) do
    []
  end

  defp nthtail(0, a) do
    a
  end

  defp nthtail(n, [_ | a]) when n > 0 do
    nthtail(n - 1, a)
  end

  defp nthtail(_, _) do
    []
  end

  defp ifelse(cond__, a, b) do
    case cond__ do
      true ->
        a

      _ ->
        b
    end
  end

  defp bin_to_list(b) when is_binary(b) do
    :erlang.binary_to_list(b)
  end

  defp bin_to_list(l) when is_list(l) do
    :lists.flatten(
      for a <- l do
        bin_to_list(a)
      end
    )
  end

  defp bin_to_list(i) when is_integer(i) do
    i
  end

  defp start_shell(connectionHandler, state) do
    shellSpawner =
      case r_state(state, :shell) do
        shell when is_function(shell, 1) ->
          [{:user, user}] =
            :ssh_connection_handler.connection_info(
              connectionHandler,
              [:user]
            )

          fn ->
            shell.(user)
          end

        shell when is_function(shell, 2) ->
          connectionInfo =
            :ssh_connection_handler.connection_info(
              connectionHandler,
              [:peer, :user]
            )

          user = :proplists.get_value(:user, connectionInfo)

          {_, peerAddr} =
            :proplists.get_value(
              :peer,
              connectionInfo
            )

          fn ->
            shell.(user, peerAddr)
          end

        {_, _, _} = shell ->
          shell
      end

    r_state(state,
      group: :group.start(self(), shellSpawner, [{:echo, get_echo(r_state(state, :pty))}]),
      buf: empty_buf()
    )
  end

  defp start_exec_shell(connectionHandler, cmd, state) do
    execShellSpawner =
      case r_state(state, :exec) do
        execShell when is_function(execShell, 1) ->
          fn ->
            execShell.(cmd)
          end

        execShell when is_function(execShell, 2) ->
          [{:user, user}] =
            :ssh_connection_handler.connection_info(
              connectionHandler,
              [:user]
            )

          fn ->
            execShell.(cmd, user)
          end

        execShell when is_function(execShell, 3) ->
          connectionInfo =
            :ssh_connection_handler.connection_info(
              connectionHandler,
              [:peer, :user]
            )

          user = :proplists.get_value(:user, connectionInfo)

          {_, peerAddr} =
            :proplists.get_value(
              :peer,
              connectionInfo
            )

          fn ->
            execShell.(cmd, user, peerAddr)
          end

        {m, f, a} ->
          {m, f, a ++ [cmd]}
      end

    r_state(state,
      group: :group.start(self(), execShellSpawner, [{:echo, false}]),
      buf: empty_buf()
    )
  end

  defp exec_in_erlang_default_shell(connectionHandler, channelId, cmd, wantReply, state) do
    exec_in_self_group(connectionHandler, channelId, wantReply, state, fn ->
      eval(parse(scan(cmd)))
    end)
  end

  defp scan(cmd) do
    :erl_scan.string(cmd)
  end

  defp parse({:ok, tokens, _}) do
    :erl_parse.parse_exprs(tokens)
  end

  defp parse({:error, {_, :erl_scan, cause}, _}) do
    {:error, :erl_scan.format_error(cause)}
  end

  defp eval({:ok, expr_list}) do
    {:value, value, _NewBindings} =
      :erl_eval.exprs(
        expr_list,
        :erl_eval.new_bindings()
      )

    {:ok, value}
  end

  defp eval({:error, {_, :erl_parse, cause}}) do
    {:error, :erl_parse.format_error(cause)}
  end

  defp eval({:error, error}) do
    {:error, error}
  end

  defp exec_direct(connectionHandler, channelId, cmd, execSpec, wantReply, state) do
    fun = fn ->
      cond do
        is_function(execSpec, 1) ->
          execSpec.(cmd)

        is_function(execSpec, 2) ->
          [{:user, user}] =
            :ssh_connection_handler.connection_info(
              connectionHandler,
              [:user]
            )

          execSpec.(cmd, user)

        is_function(execSpec, 3) ->
          connectionInfo =
            :ssh_connection_handler.connection_info(
              connectionHandler,
              [:peer, :user]
            )

          user = :proplists.get_value(:user, connectionInfo)

          {_, peerAddr} =
            :proplists.get_value(
              :peer,
              connectionInfo
            )

          execSpec.(cmd, user, peerAddr)

        true ->
          {:error, 'Bad exec fun in server'}
      end
    end

    exec_in_self_group(connectionHandler, channelId, wantReply, state, fun)
  end

  defp exec_in_self_group(connectionHandler, channelId, wantReply, state, fun) do
    exec = fn ->
      spawn(fn ->
        case (try do
                :ssh_connection.reply_request(
                  connectionHandler,
                  wantReply,
                  :success,
                  channelId
                )

                fun.()
              catch
                :error, err ->
                  {:error, err}

                cls, exp ->
                  {:error, {cls, exp}}
              else
                {:ok, result} ->
                  {:ok, result}

                {:error, error} ->
                  {:error, error}

                x ->
                  {:error, 'Bad exec fun in server. Invalid return value: ' ++ t2str(x)}
              end) do
          {:ok, str} ->
            write_chars(connectionHandler, channelId, t2str(str))

          {:error, str} ->
            write_chars(connectionHandler, channelId, 1, '**Error** ' ++ t2str(str))
            exit({:exit_status, 255})
        end
      end)
    end

    {:ok,
     r_state(state,
       group: :group.start(self(), exec, [{:echo, false}]),
       buf: empty_buf()
     )}
  end

  defp t2str(t) do
    try do
      :io_lib.format('~s', [t])
    catch
      _, _ ->
        :io_lib.format('~p', [t])
    end
  end

  defp get_echo(:undefined) do
    true
  end

  defp get_echo(r_ssh_pty(modes: modes)) do
    case :proplists.get_value(:echo, modes, 1) do
      0 ->
        false

      _ ->
        true
    end
  end

  defp set_echo(r_state(group: :undefined)) do
    :ok
  end

  defp set_echo(r_state(group: group, pty: pty)) do
    echo = get_echo(pty)
    send(group, {self(), :echo, echo})
  end

  defp not_zero(0, b) do
    b
  end

  defp not_zero(a, _) do
    a
  end

  def ssh_dbg_trace_points() do
    [:terminate, :cli, :cli_details]
  end

  def ssh_dbg_flags(:cli) do
    [:c]
  end

  def ssh_dbg_flags(:terminate) do
    [:c]
  end

  def ssh_dbg_on(:cli) do
    :dbg.tp(:ssh_cli, :handle_ssh_msg, 2, :x)
    :dbg.tp(:ssh_cli, :write_chars, 4, :x)
  end

  def ssh_dbg_on(:cli_details) do
    :dbg.tp(:ssh_cli, :handle_msg, 2, :x)
  end

  def ssh_dbg_on(:terminate) do
    :dbg.tp(:ssh_cli, :terminate, 2, :x)
  end

  def ssh_dbg_off(:cli) do
    :dbg.ctpg(:ssh_cli, :handle_ssh_msg, 2)
    :dbg.ctpg(:ssh_cli, :write_chars, 4)
  end

  def ssh_dbg_off(:cli_details) do
    :dbg.ctpg(:ssh_cli, :handle_msg, 2)
  end

  def ssh_dbg_off(:terminate) do
    :dbg.ctpg(:ssh_cli, :terminate, 2)
  end

  def ssh_dbg_format(
        :cli,
        {:call,
         {:ssh_cli, :handle_ssh_msg,
          [{:ssh_cm, _ConnectionHandler, request}, s = r_state(channel: ch)]}}
      )
      when is_tuple(request) do
    [
      :io_lib.format(
        'CLI conn ~p chan ~p, req ~p',
        [self(), ch, :erlang.element(1, request)]
      ),
      case request do
        {:window_change, channelId, width, height, pixWidth, pixHeight} ->
          fmt_kv([
            {:channel_id, channelId},
            {:width, width},
            {:height, height},
            {:pix_width, pixWidth},
            {:pixel_hight, pixHeight}
          ])

        {:env, channelId, wantReply, var, value} ->
          fmt_kv([{:channel_id, channelId}, {:want_reply, wantReply}, {var, value}])

        {:exec, channelId, wantReply, cmd} ->
          fmt_kv([{:channel_id, channelId}, {:want_reply, wantReply}, {:command, cmd}])

        {:pty, channelId, wantReply, {termName, width, height, pixWidth, pixHeight, modes}} ->
          fmt_kv([
            {:channel_id, channelId},
            {:want_reply, wantReply},
            {:term, termName},
            {:width, width},
            {:height, height},
            {:pix_width, pixWidth},
            {:pixel_hight, pixHeight},
            {:pty_opts, modes}
          ])

        {:data, channelId, type, data} ->
          fmt_kv([
            {:channel_id, channelId},
            {:type, type(type)},
            {:data, :us, :ssh_dbg.shrink_bin(data)},
            {:hex, :h, data}
          ])

        {:shell, channelId, wantReply} ->
          fmt_kv([
            {:channel_id, channelId},
            {:want_reply, wantReply},
            {:encoding, r_state(s, :encoding)},
            {:pty, r_state(s, :pty)}
          ])

        _ ->
          :io_lib.format('~nunder construction:~nRequest = ~p', [request])
      end
    ]
  end

  def ssh_dbg_format(:cli, {:call, {:ssh_cli, :handle_ssh_msg, _}}) do
    :skip
  end

  def ssh_dbg_format(
        :cli,
        {:return_from, {:ssh_cli, :handle_ssh_msg, 2}, _Result}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :cli,
        {:call, {:ssh_cli, :write_chars, [c, ch, type, chars]}}
      ) do
    [
      :io_lib.format('CLI conn ~p chan ~p reply', [c, ch]),
      fmt_kv([
        {:channel_id, ch},
        {:type, type(type)},
        {:data, :us, :ssh_dbg.shrink_bin(chars)},
        {:hex, :h, chars}
      ])
    ]
  end

  def ssh_dbg_format(
        :cli,
        {:return_from, {:ssh_cli, :write_chars, 4}, _Result}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :cli_details,
        {:call, {:ssh_cli, :handle_msg, [{group, arg}, r_state(channel: ch)]}}
      ) do
    [
      :io_lib.format('CLI detail conn ~p chan ~p group ~p', [:"?", ch, group]),
      case arg do
        {:put_chars_sync, class, cs, reply} ->
          fmt_kv([
            {:op, :put_chars_sync},
            {:class, class},
            {:data, :us, :ssh_dbg.shrink_bin(cs)},
            {:hex, :h, cs},
            {:reply, reply}
          ])

        _ ->
          :io_lib.format('~nunder construction:~nRequest = ~p', [arg])
      end
    ]
  end

  def ssh_dbg_format(
        :cli_details,
        {:call, {:ssh_cli, :handle_msg, _}}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :cli_details,
        {:return_from, {:ssh_cli, :handle_msg, 2}, _Result}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :terminate,
        {:call, {:ssh_cli, :terminate, [reason, state]}}
      ) do
    ['Cli Terminating:\n', :io_lib.format('Reason: ~p,~nState:~n~s', [reason, wr_record(state)])]
  end

  def ssh_dbg_format(
        :terminate,
        {:return_from, {:ssh_cli, :terminate, 2}, _Ret}
      ) do
    :skip
  end

  defp wr_record(r = r_state()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_state(r_state())), [])
  end

  defp fmt_kv(kVs) do
    :lists.map(&fmt_kv1/1, kVs)
  end

  defp fmt_kv1({k, v}) do
    :io_lib.format('~n~p: ~p', [k, v])
  end

  defp fmt_kv1({k, :s, v}) do
    :io_lib.format('~n~p: ~s', [k, v])
  end

  defp fmt_kv1({k, :us, v}) do
    :io_lib.format('~n~p: ~ts', [k, v])
  end

  defp fmt_kv1({k, :h, v}) do
    :io_lib.format('~n~p: ~s', [k, [?\n | :ssh_dbg.hex_dump(v)]])
  end

  defp type(0) do
    '0 (normal data)'
  end

  defp type(1) do
    '1 (extended data, i.e. errors)'
  end

  defp type(t) do
    t
  end
end
