defmodule :m_ssh_client_channel do
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

  @behaviour :gen_server
  @behaviour :ssh_dbg
  Record.defrecord(:r_state, :state,
    cm: :undefined,
    channel_cb: :undefined,
    channel_state: :undefined,
    channel_id: :undefined,
    close_sent: false
  )

  def call(channelPid, msg) do
    call(channelPid, msg, :infinity)
  end

  def call(channelPid, msg, timeOute) do
    try do
      :gen_server.call(channelPid, msg, timeOute)
    catch
      :exit, {:noproc, _} ->
        {:error, :closed}

      :exit, {:normal, _} ->
        {:error, :closed}

      :exit, {:shutdown, _} ->
        {:error, :closed}

      :exit, {{:shutdown, _}, _} ->
        {:error, :closed}

      :exit, {:timeout, _} ->
        {:error, :timeout}
    else
      result ->
        result
    end
  end

  def cast(channelPid, msg) do
    :gen_server.cast(channelPid, msg)
  end

  def reply(from, msg) do
    :gen_server.reply(from, msg)
  end

  def start(connectionManager, channelId, callBack, cbInitArgs) do
    start(connectionManager, channelId, callBack, cbInitArgs, :undefined)
  end

  def start(connectionManager, channelId, callBack, cbInitArgs, exec) do
    options = [
      {:channel_cb, callBack},
      {:channel_id, channelId},
      {:init_args, cbInitArgs},
      {:cm, connectionManager},
      {:exec, exec}
    ]

    :gen_server.start(:ssh_client_channel, [options], [])
  end

  def start_link(connectionManager, channelId, callBack, cbInitArgs) do
    start_link(connectionManager, channelId, callBack, cbInitArgs, :undefined)
  end

  def start_link(connectionManager, channelId, callBack, cbInitArgs, exec) do
    options = [
      {:channel_cb, callBack},
      {:channel_id, channelId},
      {:init_args, cbInitArgs},
      {:cm, connectionManager},
      {:exec, exec}
    ]

    :gen_server.start_link(:ssh_client_channel, [options], [])
  end

  def enter_loop(state) do
    :gen_server.enter_loop(:ssh_client_channel, [], state)
  end

  def init([options]) do
    cb = :proplists.get_value(:channel_cb, options)
    connectionManager = :proplists.get_value(:cm, options)
    channelId = :proplists.get_value(:channel_id, options)
    :erlang.process_flag(:trap_exit, true)

    try do
      cb.init(channel_cb_init_args(options))
    catch
      _, :undef ->
        {:stop, {:bad_channel_callback_module, cb}}

      _, reason ->
        {:stop, reason}
    else
      {:ok, channelState} ->
        state =
          r_state(
            cm: connectionManager,
            channel_cb: cb,
            channel_id: channelId,
            channel_state: channelState
          )

        send(self(), {:ssh_channel_up, channelId, connectionManager})
        {:ok, state}

      {:ok, channelState, timeout} ->
        state =
          r_state(
            cm: connectionManager,
            channel_cb: cb,
            channel_id: channelId,
            channel_state: channelState
          )

        send(self(), {:ssh_channel_up, channelId, connectionManager})
        {:ok, state, timeout}

      {:stop, why} ->
        {:stop, why}
    end
  end

  defp channel_cb_init_args(options) do
    case :proplists.get_value(:exec, options) do
      :undefined ->
        :proplists.get_value(:init_args, options)

      exec ->
        :proplists.get_value(:init_args, options) ++ [exec]
    end
  end

  def handle_call(:get_print_info, _From, state) do
    reply =
      {{r_state(state, :cm), r_state(state, :channel_id)},
       :io_lib.format(:"CB=~p", [r_state(state, :channel_cb)])}

    {:reply, reply, state}
  end

  def handle_call(
        request,
        from,
        r_state(
          channel_cb: module,
          channel_state: channelState
        ) = state
      ) do
    try do
      module.handle_call(request, from, channelState)
    catch
      :error, {:undef, _} ->
        {:noreply, state}
    else
      result ->
        handle_cb_result(result, state)
    end
  end

  def handle_cast(
        msg,
        r_state(
          channel_cb: module,
          channel_state: channelState
        ) = state
      ) do
    try do
      module.handle_cast(msg, channelState)
    catch
      :error, {:undef, _} ->
        {:noreply, state}
    else
      result ->
        handle_cb_result(result, state)
    end
  end

  def handle_info(
        {:ssh_cm, connectionManager, {:closed, _ChannelId}},
        r_state(cm: connectionManager, close_sent: true) = state
      ) do
    {:stop, :normal, state}
  end

  def handle_info(
        {:ssh_cm, connectionManager, {:closed, channelId}},
        r_state(cm: connectionManager, close_sent: false) = state
      ) do
    try do
      :ssh_connection.close(connectionManager, channelId)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    {:stop, :normal, r_state(state, close_sent: true)}
  end

  def handle_info(
        {:ssh_cm, _, _} = msg,
        r_state(cm: connectionManager, channel_cb: module, channel_state: channelState0) = state
      ) do
    case module.handle_ssh_msg(msg, channelState0) do
      {:ok, channelState} ->
        adjust_window(msg)
        {:noreply, r_state(state, channel_state: channelState)}

      {:ok, channelState, timeout} ->
        adjust_window(msg)
        {:noreply, r_state(state, channel_state: channelState), timeout}

      {:stop, channelId, channelState} ->
        try do
          :ssh_connection.close(connectionManager, channelId)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        {:stop, :normal,
         r_state(state,
           close_sent: true,
           channel_state: channelState
         )}
    end
  end

  def handle_info(
        msg,
        r_state(cm: connectionManager, channel_cb: module, channel_state: channelState0) = state
      ) do
    case module.handle_msg(msg, channelState0) do
      {:ok, channelState} ->
        {:noreply, r_state(state, channel_state: channelState)}

      {:ok, channelState, timeout} ->
        {:noreply, r_state(state, channel_state: channelState), timeout}

      {:stop, reason, channelState} when is_atom(reason) ->
        {:stop, reason,
         r_state(state,
           close_sent: true,
           channel_state: channelState
         )}

      {:stop, channelId, channelState} ->
        reason =
          case msg do
            {:EXIT, _Pid, :shutdown} ->
              :shutdown

            _ ->
              :normal
          end

        try do
          :ssh_connection.close(connectionManager, channelId)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        {:stop, reason,
         r_state(state,
           close_sent: true,
           channel_state: channelState
         )}
    end
  end

  def terminate(
        reason,
        r_state(cm: connectionManager, channel_id: channelId, close_sent: false) = state
      ) do
    try do
      :ssh_connection.close(connectionManager, channelId)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    terminate(reason, r_state(state, close_sent: true))
  end

  def terminate(
        reason,
        r_state(channel_cb: cb, channel_state: channelState)
      ) do
    try do
      cb.terminate(reason, channelState)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def code_change(
        oldVsn,
        r_state(
          channel_cb: module,
          channel_state: channelState0
        ) = state,
        extra
      ) do
    {:ok, channelState} = module.code_change(oldVsn, channelState0, extra)
    {:ok, r_state(state, channel_state: channelState)}
  end

  def cache_create() do
    :ets.new(:cm_tab, [:set, {:keypos, r_channel(:local_id)}])
  end

  def cache_lookup(cache, key) do
    case :ets.lookup(cache, key) do
      [channel] ->
        channel

      [] ->
        :undefined
    end
  end

  def cache_update(cache, r_channel(local_id: id) = entry)
      when id !== :undefined do
    :ets.insert(cache, entry)
  end

  def cache_delete(cache, key) do
    :ets.delete(cache, key)
  end

  def cache_delete(cache) do
    :ets.delete(cache)
  end

  def cache_foldl(fun, acc, cache) do
    :ets.foldl(fun, acc, cache)
  end

  def cache_info(:num_entries, cache) do
    :proplists.get_value(:size, :ets.info(cache))
  end

  def cache_find(channelPid, cache) do
    case :ets.match_object(cache, r_channel(user: channelPid)) do
      [] ->
        :undefined

      [channel] ->
        channel
    end
  end

  def get_print_info(pid) do
    call(pid, :get_print_info, 1000)
  end

  defp handle_cb_result({:reply, reply, channelState}, state) do
    {:reply, reply, r_state(state, channel_state: channelState)}
  end

  defp handle_cb_result(
         {:reply, reply, channelState, timeout},
         state
       ) do
    {:reply, reply, r_state(state, channel_state: channelState), timeout}
  end

  defp handle_cb_result({:noreply, channelState}, state) do
    {:noreply, r_state(state, channel_state: channelState)}
  end

  defp handle_cb_result({:noreply, channelState, timeout}, state) do
    {:noreply, r_state(state, channel_state: channelState), timeout}
  end

  defp handle_cb_result({:stop, reason, reply, channelState}, state) do
    {:stop, reason, reply, r_state(state, channel_state: channelState)}
  end

  defp handle_cb_result({:stop, reason, channelState}, state) do
    {:stop, reason, r_state(state, channel_state: channelState)}
  end

  defp adjust_window({:ssh_cm, connectionManager, {:data, channelId, _, data}}) do
    :ssh_connection.adjust_window(connectionManager, channelId, :erlang.size(data))
  end

  defp adjust_window(_) do
    :ok
  end

  def ssh_dbg_trace_points() do
    [:terminate, :channels, :channel_events]
  end

  def ssh_dbg_flags(:channels) do
    [:c]
  end

  def ssh_dbg_flags(:terminate) do
    [:c]
  end

  def ssh_dbg_flags(:channel_events) do
    [:c]
  end

  def ssh_dbg_on(:terminate) do
    :dbg.tp(:ssh_client_channel, :terminate, 2, :x)
  end

  def ssh_dbg_on(:channels) do
    :dbg.tp(:ssh_client_channel, :init, 1, :x)
    ssh_dbg_on(:terminate)
  end

  def ssh_dbg_on(:channel_events) do
    :dbg.tp(:ssh_client_channel, :handle_call, 3, :x)
    :dbg.tp(:ssh_client_channel, :handle_cast, 2, :x)
    :dbg.tp(:ssh_client_channel, :handle_info, 2, :x)
  end

  def ssh_dbg_off(:terminate) do
    :dbg.ctpg(:ssh_client_channel, :terminate, 2)
  end

  def ssh_dbg_off(:channels) do
    :dbg.ctpg(:ssh_client_channel, :init, 1)
    ssh_dbg_off(:terminate)
  end

  def ssh_dbg_off(:channel_events) do
    :dbg.ctpg(:ssh_client_channel, :handle_call, 3)
    :dbg.ctpg(:ssh_client_channel, :handle_cast, 2)
    :dbg.ctpg(:ssh_client_channel, :handle_info, 2)
  end

  def ssh_dbg_format(
        :channels,
        {:call, {:ssh_client_channel, :init, [[kVs]]}}
      ) do
    [
      'Server Channel Starting:\n',
      :io_lib.format(
        'Connection: ~p, ChannelId: ~p, CallBack: ~p\nCallBack init args = ~p',
        for k <- [:cm, :channel_id, :channel_cb] do
          :proplists.get_value(k, kVs)
        end ++ [channel_cb_init_args(kVs)]
      )
    ]
  end

  def ssh_dbg_format(
        :channels,
        {:return_from, {:ssh_client_channel, :init, 1}, {:stop, reason}}
      ) do
    ['Server Channel Start FAILED!\n', :io_lib.format('Reason = ~p', [reason])]
  end

  def ssh_dbg_format(:channels, f) do
    ssh_dbg_format(:terminate, f)
  end

  def ssh_dbg_format(
        :terminate,
        {:call, {:ssh_client_channel, :terminate, [reason, state]}}
      ) do
    [
      'Server Channel Terminating:\n',
      :io_lib.format('Reason: ~p,~nState:~n~s', [reason, wr_record(state)])
    ]
  end

  def ssh_dbg_format(
        :terminate,
        {:return_from, {:ssh_client_channel, :terminate, 2}, _Ret}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :channel_events,
        {:call, {:ssh_client_channel, :handle_call, [call, from, state]}}
      ) do
    [hdr('is called', state), :io_lib.format('From: ~p~nCall: ~p~n', [from, call])]
  end

  def ssh_dbg_format(
        :channel_events,
        {:return_from, {:ssh_client_channel, :handle_call, 3}, ret}
      ) do
    [
      'Server Channel call returned:\n',
      :io_lib.format('~p~n', [:ssh_dbg.reduce_state(ret, r_state())])
    ]
  end

  def ssh_dbg_format(
        :channel_events,
        {:call, {:ssh_client_channel, :handle_cast, [cast, state]}}
      ) do
    [hdr('got cast', state), :io_lib.format('Cast: ~p~n', [cast])]
  end

  def ssh_dbg_format(
        :channel_events,
        {:return_from, {:ssh_client_channel, :handle_cast, 2}, ret}
      ) do
    [
      'Server Channel cast returned:\n',
      :io_lib.format('~p~n', [:ssh_dbg.reduce_state(ret, r_state())])
    ]
  end

  def ssh_dbg_format(
        :channel_events,
        {:call, {:ssh_client_channel, :handle_info, [info, state]}}
      ) do
    [hdr('got info', state), :io_lib.format('Info: ~p~n', [info])]
  end

  def ssh_dbg_format(
        :channel_events,
        {:return_from, {:ssh_client_channel, :handle_info, 2}, ret}
      ) do
    [
      'Server Channel info returned:\n',
      :io_lib.format('~p~n', [:ssh_dbg.reduce_state(ret, r_state())])
    ]
  end

  defp hdr(title, s) do
    :io_lib.format(
      'Server Channel (Id=~p, CB=~p) ~s:\n',
      [r_state(s, :channel_id), r_state(s, :channel_cb), title]
    )
  end

  defp wr_record(r = r_state()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_state(r_state())), [])
  end
end
