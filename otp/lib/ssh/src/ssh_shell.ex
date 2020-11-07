defmodule :m_ssh_shell do
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

  @behaviour :ssh_server_channel
  @behaviour :ssh_dbg
  Record.defrecord(:r_state, :state, io: :undefined, channel: :undefined, cm: :undefined)

  def init([connectionManager, channelId] = args) do
    case :erlang.get(:"$initial_call") do
      :undefined ->
        me = get_my_name()
        ancestors = get_ancestors()
        :erlang.put(:"$ancestors", [me | ancestors])
        :erlang.put(:"$initial_call", {:ssh_shell, :init, args})

      _ ->
        :ok
    end

    case :ssh_connection.shell(
           connectionManager,
           channelId
         ) do
      :ok ->
        {:group_leader, gIO} =
          :erlang.process_info(
            self(),
            :group_leader
          )

        ioPid = spawn_link(:ssh_shell, :input_loop, [gIO, self()])
        {:ok, r_state(io: ioPid, channel: channelId, cm: connectionManager)}

      error ->
        {:stop, error}
    end
  end

  def handle_ssh_msg(
        {:ssh_cm, _, {:data, _ChannelId, 0, data}},
        state
      ) do
    :io.format('~ts', [data])
    {:ok, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, _, {:data, _ChannelId, 1, data}},
        state
      ) do
    :io.format('~ts', [data])
    {:ok, state}
  end

  def handle_ssh_msg({:ssh_cm, _, {:eof, _ChannelId}}, state) do
    {:ok, state}
  end

  def handle_ssh_msg({:ssh_cm, _, {:signal, _, _}}, state) do
    {:ok, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, _, {:exit_signal, channelId, _, error, _}},
        state
      ) do
    :io.put_chars('Connection closed by peer')
    :io.put_chars(error)
    {:stop, channelId, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, _, {:exit_status, channelId, 0}},
        state
      ) do
    :io.put_chars('logout')
    :io.put_chars('Connection closed')
    {:stop, channelId, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, _, {:exit_status, channelId, status}},
        state
      ) do
    :io.put_chars('Connection closed by peer')
    :io.put_chars('Status: ' ++ :erlang.integer_to_list(status))
    {:stop, channelId, state}
  end

  def handle_msg(
        {:ssh_channel_up, channelId, connectionManager},
        r_state(channel: channelId, cm: connectionManager) = state
      ) do
    {:ok, state}
  end

  def handle_msg(
        {:input, ioPid, :eof},
        r_state(io: ioPid, channel: channelId, cm: connectionManager) = state
      ) do
    :ssh_connection.send_eof(connectionManager, channelId)
    {:ok, state}
  end

  def handle_msg(
        {:input, ioPid, line0},
        r_state(io: ioPid, channel: channelId, cm: connectionManager) = state
      ) do
    line =
      case encoding(line0) do
        :utf8 ->
          line0

        :unicode ->
          :unicode.characters_to_binary(line0)

        :latin1 ->
          :unicode.characters_to_binary(line0, :latin1, :utf8)
      end

    :ssh_connection.send(connectionManager, channelId, line)
    {:ok, state}
  end

  def terminate(_Reason, r_state(io: ioPid)) do
    :erlang.exit(ioPid, :kill)
  end

  defp encoding(bin) do
    case :unicode.characters_to_binary(bin, :utf8, :utf8) do
      ^bin ->
        :utf8

      bin2 when is_binary(bin2) ->
        :unicode

      _ ->
        :latin1
    end
  end

  def input_loop(fd, pid) do
    case :io.get_line(fd, :"") do
      :eof ->
        send(pid, {:input, self(), :eof})
        :ok

      line ->
        send(pid, {:input, self(), line})
        input_loop(fd, pid)
    end
  end

  defp get_my_name() do
    case :erlang.process_info(self(), :registered_name) do
      {:registered_name, name} ->
        name

      _ ->
        self()
    end
  end

  defp get_ancestors() do
    case :erlang.get(:"$ancestors") do
      a when is_list(a) ->
        a

      _ ->
        []
    end
  end

  def ssh_dbg_trace_points() do
    [:terminate, :shell]
  end

  def ssh_dbg_flags(:shell) do
    [:c]
  end

  def ssh_dbg_flags(:terminate) do
    [:c]
  end

  def ssh_dbg_on(:shell) do
    :dbg.tp(:ssh_shell, :handle_ssh_msg, 2, :x)
  end

  def ssh_dbg_on(:terminate) do
    :dbg.tp(:ssh_shell, :terminate, 2, :x)
  end

  def ssh_dbg_off(:shell) do
    :dbg.ctpg(:ssh_shell, :handle_ssh_msg, 2)
  end

  def ssh_dbg_off(:terminate) do
    :dbg.ctpg(:ssh_shell, :terminate, 2)
  end

  def ssh_dbg_format(
        :shell,
        {:call,
         {:ssh_shell, :handle_ssh_msg,
          [{:ssh_cm, _ConnectionHandler, request}, r_state(channel: ch)]}}
      )
      when is_tuple(request) do
    [
      :io_lib.format(
        'SHELL conn ~p chan ~p, req ~p',
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
            {:type,
             case type do
               0 ->
                 '0 (normal data)'

               1 ->
                 '1 (extended data, i.e. errors)'

               _ ->
                 type
             end},
            {:data, :ssh_dbg.shrink_bin(data)},
            {:hex, :h, data}
          ])

        _ ->
          :io_lib.format('~nunder construction:~nRequest = ~p', [request])
      end
    ]
  end

  def ssh_dbg_format(
        :shell,
        {:call, {:ssh_shell, :handle_ssh_msg, _}}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :shell,
        {:return_from, {:ssh_shell, :handle_ssh_msg, 2}, _Result}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :terminate,
        {:call, {:ssh_shell, :terminate, [reason, state]}}
      ) do
    [
      'Shell Terminating:\n',
      :io_lib.format('Reason: ~p,~nState:~n~s', [reason, wr_record(state)])
    ]
  end

  def ssh_dbg_format(
        :terminate,
        {:return_from, {:ssh_shell, :terminate, 2}, _Ret}
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

  defp fmt_kv1({k, :h, v}) do
    :io_lib.format('~n~p: ~s', [k, [?\n | :ssh_dbg.hex_dump(v)]])
  end
end
