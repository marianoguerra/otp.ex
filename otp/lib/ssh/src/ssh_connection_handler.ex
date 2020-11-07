defmodule :m_ssh_connection_handler do
  use Bitwise
  @behaviour :gen_statem
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

  Record.defrecord(:r_ssh_msg_userauth_request, :ssh_msg_userauth_request,
    user: :undefined,
    service: :undefined,
    method: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_failure, :ssh_msg_userauth_failure,
    authentications: :undefined,
    partial_success: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_success, :ssh_msg_userauth_success, [])

  Record.defrecord(:r_ssh_msg_userauth_banner, :ssh_msg_userauth_banner,
    message: :undefined,
    language: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_passwd_changereq, :ssh_msg_userauth_passwd_changereq,
    prompt: :undefined,
    languge: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_pk_ok, :ssh_msg_userauth_pk_ok,
    algorithm_name: :undefined,
    key_blob: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_info_request, :ssh_msg_userauth_info_request,
    name: :undefined,
    instruction: :undefined,
    language_tag: :undefined,
    num_prompts: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_info_response, :ssh_msg_userauth_info_response,
    num_responses: :undefined,
    data: :undefined
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
  def start_link(role, socket, options) do
    {:ok,
     :proc_lib.spawn_opt(
       :ssh_connection_handler,
       :init_connection_handler,
       [role, socket, options],
       [:link, {:message_queue_data, :off_heap}]
     )}
  end

  def stop(connectionHandler) do
    case call(connectionHandler, :stop) do
      {:error, :closed} ->
        :ok

      other ->
        other
    end
  end

  def start_connection(role, socket, options, timeout) do
    try do
      case role do
        :client ->
          childPid = start_the_connection_child(self(), role, socket, options)
          handshake(childPid, :erlang.monitor(:process, childPid), timeout)

        :server ->
          case :ssh_options.get_value(
                 :user_options,
                 :parallel_login,
                 options,
                 :ssh_connection_handler,
                 144
               ) do
            true ->
              handshakerPid =
                spawn_link(fn ->
                  receive do
                    {:do_handshake, pid} ->
                      handshake(
                        pid,
                        :erlang.monitor(
                          :process,
                          pid
                        ),
                        timeout
                      )
                  end
                end)

              childPid = start_the_connection_child(handshakerPid, role, socket, options)
              send(handshakerPid, {:do_handshake, childPid})

            false ->
              childPid = start_the_connection_child(self(), role, socket, options)
              handshake(childPid, :erlang.monitor(:process, childPid), timeout)
          end
      end
    catch
      :exit, {:noproc, _} ->
        {:error, :ssh_not_started}

      _, error ->
        {:error, error}
    end
  end

  def disconnect(code, detailedText, module, line) do
    throw(
      {:keep_state_and_data,
       [{:next_event, :internal, {:send_disconnect, code, detailedText, module, line}}]}
    )
  end

  def open_channel(
        connectionHandler,
        channelType,
        channelSpecificData,
        initialWindowSize,
        maxPacketSize,
        timeout
      ) do
    call(
      connectionHandler,
      {:open, self(), channelType, initialWindowSize, maxPacketSize, channelSpecificData, timeout}
    )
  end

  def start_channel(connectionHandler, callbackModule, channelId, args, exec) do
    {:ok, {subSysSup, role, opts}} =
      call(
        connectionHandler,
        :get_misc
      )

    :ssh_subsystem_sup.start_channel(
      role,
      subSysSup,
      connectionHandler,
      callbackModule,
      channelId,
      args,
      exec,
      opts
    )
  end

  def handle_direct_tcpip(
        connectionHandler,
        listenHost,
        listenPort,
        connectToHost,
        connectToPort,
        timeout
      ) do
    call(
      connectionHandler,
      {:handle_direct_tcpip, listenHost, listenPort, connectToHost, connectToPort, timeout}
    )
  end

  def request(connectionHandler, channelPid, channelId, type, true, data, timeout) do
    call(
      connectionHandler,
      {:request, channelPid, channelId, type, data, timeout}
    )
  end

  def request(connectionHandler, channelPid, channelId, type, false, data, _) do
    cast(
      connectionHandler,
      {:request, channelPid, channelId, type, data}
    )
  end

  def request(connectionHandler, channelId, type, true, data, timeout) do
    call(
      connectionHandler,
      {:request, channelId, type, data, timeout}
    )
  end

  def request(connectionHandler, channelId, type, false, data, _) do
    cast(
      connectionHandler,
      {:request, channelId, type, data}
    )
  end

  def reply_request(connectionHandler, status, channelId) do
    cast(
      connectionHandler,
      {:reply_request, status, channelId}
    )
  end

  def global_request(connectionHandler, type, true, data, timeout) do
    call(
      connectionHandler,
      {:global_request, type, data, timeout}
    )
  end

  def global_request(connectionHandler, type, false, data, _) do
    cast(connectionHandler, {:global_request, type, data})
  end

  def send(connectionHandler, channelId, type, data, timeout) do
    call(
      connectionHandler,
      {:data, channelId, type, data, timeout}
    )
  end

  def send_eof(connectionHandler, channelId) do
    call(connectionHandler, {:eof, channelId})
  end

  def info(connectionHandler) do
    info(connectionHandler, :all)
  end

  def info(connectionHandler, channelProcess) do
    call(connectionHandler, {:info, channelProcess})
  end

  def get_print_info(connectionHandler) do
    call(connectionHandler, :get_print_info, 1000)
  end

  def connection_info(connectionHandler, []) do
    connection_info(connectionHandler, conn_info_keys())
  end

  def connection_info(connectionHandler, key) when is_atom(key) do
    case connection_info(connectionHandler, [key]) do
      [{^key, val}] ->
        {key, val}

      other ->
        other
    end
  end

  def connection_info(connectionHandler, options) do
    call(connectionHandler, {:connection_info, options})
  end

  def channel_info(connectionHandler, channelId, options) do
    call(
      connectionHandler,
      {:channel_info, channelId, options}
    )
  end

  def adjust_window(connectionHandler, channel, bytes) do
    cast(
      connectionHandler,
      {:adjust_window, channel, bytes}
    )
  end

  def close(connectionHandler, channelId) do
    case call(connectionHandler, {:close, channelId}) do
      :ok ->
        :ok

      {:error, :closed} ->
        :ok
    end
  end

  def store(connectionHandler, key, value) do
    cast(connectionHandler, {:store, key, value})
  end

  def retrieve(r_connection(options: opts), key) do
    try do
      :ssh_options.get_value(:internal_options, key, opts, :ssh_connection_handler, 355)
    catch
      :error, {:badkey, ^key} ->
        :undefined
    else
      value ->
        {:ok, value}
    end
  end

  def retrieve(connectionHandler, key) do
    call(connectionHandler, {:retrieve, key})
  end

  def set_sock_opts(connectionRef, socketOptions) do
    try do
      :lists.foldr(
        fn {name, _Val}, acc ->
          case prohibited_sock_option(name) do
            true ->
              [name | acc]

            false ->
              acc
          end
        end,
        [],
        socketOptions
      )
    catch
      _, _ ->
        {:error, :badarg}
    else
      [] ->
        call(connectionRef, {:set_sock_opts, socketOptions})

      bad ->
        {:error, {:not_allowed, bad}}
    end
  end

  def prohibited_sock_option(:active) do
    true
  end

  def prohibited_sock_option(:deliver) do
    true
  end

  def prohibited_sock_option(:mode) do
    true
  end

  def prohibited_sock_option(:packet) do
    true
  end

  def prohibited_sock_option(_) do
    false
  end

  def get_sock_opts(connectionRef, socketGetOptions) do
    call(connectionRef, {:get_sock_opts, socketGetOptions})
  end

  def renegotiate(connectionHandler) do
    cast(connectionHandler, :force_renegotiate)
  end

  def alg(connectionHandler) do
    call(connectionHandler, :get_alg)
  end

  Record.defrecord(:r_data, :data,
    starter: :undefined,
    auth_user: :undefined,
    connection_state: :undefined,
    latest_channel_id: 0,
    transport_protocol: :undefined,
    transport_cb: :undefined,
    transport_close_tag: :undefined,
    ssh_params: :undefined,
    socket: :undefined,
    decrypted_data_buffer: <<>>,
    encrypted_data_buffer: <<>>,
    aead_data: <<>>,
    undecrypted_packet_length: :undefined,
    key_exchange_init_msg: :undefined,
    last_size_rekey: 0,
    event_queue: [],
    inet_initial_recbuf_size: :undefined
  )

  def init_connection_handler(role, socket, opts) do
    case init([role, socket, opts]) do
      {:ok, startState, d} when role == :server ->
        :erlang.process_flag(:trap_exit, true)
        :gen_statem.enter_loop(:ssh_connection_handler, [], startState, d)

      {:ok, startState, d0 = r_data(connection_state: c)}
      when role == :client ->
        :erlang.process_flag(:trap_exit, true)

        sups =
          :ssh_options.get_value(
            :internal_options,
            :supervisors,
            opts,
            :ssh_connection_handler,
            466
          )

        d =
          r_data(d0,
            connection_state:
              r_connection(c,
                system_supervisor:
                  :proplists.get_value(
                    :system_sup,
                    sups
                  ),
                sub_system_supervisor:
                  :proplists.get_value(
                    :subsystem_sup,
                    sups
                  ),
                connection_supervisor:
                  :proplists.get_value(
                    :connection_sup,
                    sups
                  )
              )
          )

        :gen_statem.enter_loop(:ssh_connection_handler, [], startState, d)

      {:stop, error} ->
        d =
          try do
            sups =
              :ssh_options.get_value(
                :internal_options,
                :supervisors,
                opts,
                :ssh_connection_handler,
                480
              )

            r_connection(
              system_supervisor:
                :proplists.get_value(
                  :system_sup,
                  sups
                ),
              sub_system_supervisor:
                :proplists.get_value(
                  :subsystem_sup,
                  sups
                ),
              connection_supervisor:
                :proplists.get_value(
                  :connection_sup,
                  sups
                )
            )
          catch
            _, _ ->
              r_data(connection_state: r_connection())
          else
            c ->
              r_data(connection_state: c)
          end

        :gen_statem.enter_loop(
          :ssh_connection_handler,
          [],
          {:init_error, error},
          r_data(d, socket: socket)
        )
    end
  end

  def init([role, socket, opts]) do
    case :inet.peername(socket) do
      {:ok, peerAddr} ->
        {protocol, callback, closeTag} =
          :ssh_options.get_value(:user_options, :transport, opts, :ssh_connection_handler, 503)

        c =
          r_connection(
            channel_cache: :ssh_client_channel.cache_create(),
            channel_id_seed: 0,
            requests: [],
            options: opts
          )

        d0 =
          r_data(
            starter:
              :ssh_options.get_value(
                :internal_options,
                :user_pid,
                opts,
                :ssh_connection_handler,
                508
              ),
            connection_state: c,
            socket: socket,
            transport_protocol: protocol,
            transport_cb: callback,
            transport_close_tag: closeTag,
            ssh_params: init_ssh_record(role, socket, peerAddr, opts)
          )

        d =
          case role do
            :client ->
              d0

            :server ->
              sups =
                :ssh_options.get_value(
                  :internal_options,
                  :supervisors,
                  opts,
                  :ssh_connection_handler,
                  520
                )

              r_data(d0,
                connection_state:
                  r_connection(c,
                    cli_spec:
                      :ssh_options.get_value(
                        :user_options,
                        :ssh_cli,
                        opts,
                        fn ->
                          {:ssh_cli,
                           [
                             :ssh_options.get_value(
                               :user_options,
                               :shell,
                               opts,
                               :ssh_connection_handler,
                               522
                             )
                           ]}
                        end,
                        :ssh_connection_handler,
                        522
                      ),
                    exec:
                      :ssh_options.get_value(
                        :user_options,
                        :exec,
                        opts,
                        :ssh_connection_handler,
                        523
                      ),
                    system_supervisor:
                      :proplists.get_value(
                        :system_sup,
                        sups
                      ),
                    sub_system_supervisor:
                      :proplists.get_value(
                        :subsystem_sup,
                        sups
                      ),
                    connection_supervisor:
                      :proplists.get_value(
                        :connection_sup,
                        sups
                      )
                  )
              )
          end

        {:ok, {:hello, role}, d}

      {:error, error} ->
        {:stop, error}
    end
  end

  def init_ssh_record(role, socket, opts) do
    {:ok, peerAddr} = :inet.peername(socket)
    init_ssh_record(role, socket, peerAddr, opts)
  end

  defp init_ssh_record(role, socket, peerAddr, opts) do
    authMethods =
      :ssh_options.get_value(:user_options, :auth_methods, opts, :ssh_connection_handler, 544)

    s0 =
      r_ssh(
        role: role,
        opts: opts,
        userauth_supported_methods: authMethods,
        available_host_keys:
          available_hkey_algorithms(
            role,
            opts
          ),
        random_length_padding:
          :ssh_options.get_value(
            :user_options,
            :max_random_length_padding,
            opts,
            :ssh_connection_handler,
            549
          )
      )

    {vsn, version} = :ssh_transport.versions(role, opts)

    localName =
      case :inet.sockname(socket) do
        {:ok, local} ->
          local

        _ ->
          :undefined
      end

    case role do
      :client ->
        peerName =
          case :ssh_options.get_value(
                 :internal_options,
                 :host,
                 opts,
                 fn ->
                   :erlang.element(
                     1,
                     peerAddr
                   )
                 end,
                 :ssh_connection_handler,
                 559
               ) do
            peerIP when is_tuple(peerIP) ->
              :inet_parse.ntoa(peerIP)

            peerName0 when is_atom(peerName0) ->
              :erlang.atom_to_list(peerName0)

            peerName0 when is_list(peerName0) ->
              peerName0
          end

        s1 =
          r_ssh(s0,
            c_vsn: vsn,
            c_version: version,
            opts:
              :ssh_options.put_value(
                :internal_options,
                {:io_cb,
                 case :ssh_options.get_value(
                        :user_options,
                        :user_interaction,
                        opts,
                        :ssh_connection_handler,
                        570
                      ) do
                   true ->
                     :ssh_io

                   false ->
                     :ssh_no_io
                 end},
                opts,
                :ssh_connection_handler,
                574
              ),
            userauth_quiet_mode:
              :ssh_options.get_value(
                :user_options,
                :quiet_mode,
                opts,
                :ssh_connection_handler,
                575
              ),
            peer: {peerName, peerAddr},
            local: localName
          )

        r_ssh(s1,
          userauth_pubkeys:
            for k <-
                  :ssh_options.get_value(
                    :user_options,
                    :pref_public_key_algs,
                    opts,
                    :ssh_connection_handler,
                    579
                  ),
                is_usable_user_pubkey(k, s1) do
              k
            end
        )

      :server ->
        r_ssh(s0,
          s_vsn: vsn,
          s_version: version,
          userauth_methods: :string.tokens(authMethods, ','),
          kb_tries_left: 3,
          peer: {:undefined, peerAddr},
          local: localName
        )
    end
  end

  defp role({_, role}) do
    role
  end

  defp role({_, role, _}) do
    role
  end

  defp renegotiation({_, _, reNeg}) do
    reNeg == :renegotiate
  end

  defp renegotiation(_) do
    false
  end

  def callback_mode() do
    [:handle_event_function, :state_enter]
  end

  def handle_event(_, _Event, {:init_error, error} = stateName, d) do
    case error do
      :enotconn ->
        call_disconnectfun_and_log_cond(
          'Protocol Error',
          'TCP connenction to server was prematurely closed by the client',
          :ssh_connection_handler,
          662,
          stateName,
          d
        )

        {:stop, {:shutdown, 'TCP connenction to server was prematurely closed by the client'}}

      otherError ->
        {:stop, {:shutdown, {:init, otherError}}}
    end
  end

  def handle_event(_, :socket_control, {:hello, _} = stateName, r_data(ssh_params: ssh0) = d) do
    vsnMsg = :ssh_transport.hello_version_msg(string_version(ssh0))
    send_bytes(vsnMsg, d)

    case :inet.getopts(
           socket = r_data(d, :socket),
           [:recbuf]
         ) do
      {:ok, [{:recbuf, size}]} ->
        :inet.setopts(
          socket,
          [{:packet, :line}, {:active, :once}, {:recbuf, 255}, {:nodelay, true}]
        )

        time =
          :ssh_options.get_value(
            :user_options,
            :hello_timeout,
            r_ssh(ssh0, :opts),
            fn ->
              :infinity
            end,
            :ssh_connection_handler,
            684
          )

        {:keep_state, r_data(d, inet_initial_recbuf_size: size),
         [{:state_timeout, time, :no_hello_received}]}

      other ->
        call_disconnectfun_and_log_cond(
          'Option return',
          :io_lib.format('Unexpected getopts return:~n  ~p', [other]),
          :ssh_connection_handler,
          689,
          stateName,
          d
        )

        {:stop, {:shutdown, {:unexpected_getopts_return, other}}}
    end
  end

  def handle_event(_, {:info_line, line}, {:hello, role} = stateName, d) do
    case role do
      :client ->
        :inet.setopts(r_data(d, :socket), [{:active, :once}])
        :keep_state_and_data

      :server ->
        send_bytes('Protocol mismatch.', d)

        msg =
          :io_lib.format('Protocol mismatch in version exchange. Client sent info lines.~n~s', [
            :ssh_dbg.hex_dump(line, 64)
          ])

        call_disconnectfun_and_log_cond(
          'Protocol mismatch.',
          msg,
          :ssh_connection_handler,
          707,
          stateName,
          d
        )

        {:stop, {:shutdown, 'Protocol mismatch in version exchange. Client sent info lines.'}}
    end
  end

  def handle_event(_, {:version_exchange, version}, {:hello, role}, d0) do
    {numVsn, strVsn} = :ssh_transport.handle_hello_version(version)

    case handle_version(numVsn, strVsn, r_data(d0, :ssh_params)) do
      {:ok, ssh1} ->
        :inet.setopts(
          r_data(d0, :socket),
          [
            {:packet, 0},
            {:mode, :binary},
            {:active, :once},
            {:recbuf, r_data(d0, :inet_initial_recbuf_size)}
          ]
        )

        {keyInitMsg, sshPacket, ssh} = :ssh_transport.key_exchange_init_msg(ssh1)
        send_bytes(sshPacket, d0)

        {:next_state, {:kexinit, role, :init},
         r_data(d0,
           ssh_params: ssh,
           key_exchange_init_msg: keyInitMsg
         )}

      :not_supported ->
        {shutdown, d} =
          send_disconnect(
            8,
            :io_lib.format(
              'Offending version is ~p',
              [:string.chomp(version)]
            ),
            :ssh_connection_handler,
            728,
            {:hello, role},
            d0
          )

        {:stop, shutdown, d}
    end
  end

  def handle_event(_, :no_hello_received, {:hello, _Role} = stateName, d0) do
    {shutdown, d} =
      send_disconnect(2, 'No HELLO recieved', :ssh_connection_handler, 736, stateName, d0)

    {:stop, shutdown, d}
  end

  def handle_event(
        _,
        {r_ssh_msg_kexinit() = kex, payload},
        {:kexinit, role, reNeg},
        d = r_data(key_exchange_init_msg: ownKex)
      ) do
    ssh1 = :ssh_transport.key_init(peer_role(role), r_data(d, :ssh_params), payload)

    ssh =
      case :ssh_transport.handle_kexinit_msg(kex, ownKex, ssh1) do
        {:ok, nextKexMsg, ssh2} when role == :client ->
          send_bytes(nextKexMsg, d)
          ssh2

        {:ok, ssh2} when role == :server ->
          ssh2
      end

    {:next_state, {:key_exchange, role, reNeg}, r_data(d, ssh_params: ssh)}
  end

  def handle_event(_, r_ssh_msg_kexdh_init() = msg, {:key_exchange, :server, reNeg}, d) do
    {:ok, kexdhReply, ssh1} =
      :ssh_transport.handle_kexdh_init(
        msg,
        r_data(d, :ssh_params)
      )

    send_bytes(kexdhReply, d)
    {:ok, newKeys, ssh2} = :ssh_transport.new_keys_message(ssh1)
    send_bytes(newKeys, d)
    {:ok, extInfo, ssh} = :ssh_transport.ext_info_message(ssh2)
    send_bytes(extInfo, d)
    {:next_state, {:new_keys, :server, reNeg}, r_data(d, ssh_params: ssh)}
  end

  def handle_event(_, r_ssh_msg_kexdh_reply() = msg, {:key_exchange, :client, reNeg}, d) do
    {:ok, newKeys, ssh1} =
      :ssh_transport.handle_kexdh_reply(
        msg,
        r_data(d, :ssh_params)
      )

    send_bytes(newKeys, d)
    {:ok, extInfo, ssh} = :ssh_transport.ext_info_message(ssh1)
    send_bytes(extInfo, d)
    {:next_state, {:new_keys, :client, reNeg}, r_data(d, ssh_params: ssh)}
  end

  def handle_event(_, r_ssh_msg_kex_dh_gex_request() = msg, {:key_exchange, :server, reNeg}, d) do
    {:ok, gexGroup, ssh1} =
      :ssh_transport.handle_kex_dh_gex_request(
        msg,
        r_data(d, :ssh_params)
      )

    send_bytes(gexGroup, d)
    ssh = :ssh_transport.parallell_gen_key(ssh1)
    {:next_state, {:key_exchange_dh_gex_init, :server, reNeg}, r_data(d, ssh_params: ssh)}
  end

  def handle_event(
        _,
        r_ssh_msg_kex_dh_gex_request_old() = msg,
        {:key_exchange, :server, reNeg},
        d
      ) do
    {:ok, gexGroup, ssh1} =
      :ssh_transport.handle_kex_dh_gex_request(
        msg,
        r_data(d, :ssh_params)
      )

    send_bytes(gexGroup, d)
    ssh = :ssh_transport.parallell_gen_key(ssh1)
    {:next_state, {:key_exchange_dh_gex_init, :server, reNeg}, r_data(d, ssh_params: ssh)}
  end

  def handle_event(_, r_ssh_msg_kex_dh_gex_group() = msg, {:key_exchange, :client, reNeg}, d) do
    {:ok, kexGexInit, ssh} =
      :ssh_transport.handle_kex_dh_gex_group(
        msg,
        r_data(d, :ssh_params)
      )

    send_bytes(kexGexInit, d)
    {:next_state, {:key_exchange_dh_gex_reply, :client, reNeg}, r_data(d, ssh_params: ssh)}
  end

  def handle_event(_, r_ssh_msg_kex_ecdh_init() = msg, {:key_exchange, :server, reNeg}, d) do
    {:ok, kexEcdhReply, ssh1} =
      :ssh_transport.handle_kex_ecdh_init(
        msg,
        r_data(d, :ssh_params)
      )

    send_bytes(kexEcdhReply, d)
    {:ok, newKeys, ssh2} = :ssh_transport.new_keys_message(ssh1)
    send_bytes(newKeys, d)
    {:ok, extInfo, ssh} = :ssh_transport.ext_info_message(ssh2)
    send_bytes(extInfo, d)
    {:next_state, {:new_keys, :server, reNeg}, r_data(d, ssh_params: ssh)}
  end

  def handle_event(_, r_ssh_msg_kex_ecdh_reply() = msg, {:key_exchange, :client, reNeg}, d) do
    {:ok, newKeys, ssh1} =
      :ssh_transport.handle_kex_ecdh_reply(
        msg,
        r_data(d, :ssh_params)
      )

    send_bytes(newKeys, d)
    {:ok, extInfo, ssh} = :ssh_transport.ext_info_message(ssh1)
    send_bytes(extInfo, d)
    {:next_state, {:new_keys, :client, reNeg}, r_data(d, ssh_params: ssh)}
  end

  def handle_event(
        _,
        r_ssh_msg_kex_dh_gex_init() = msg,
        {:key_exchange_dh_gex_init, :server, reNeg},
        d
      ) do
    {:ok, kexGexReply, ssh1} =
      :ssh_transport.handle_kex_dh_gex_init(
        msg,
        r_data(d, :ssh_params)
      )

    send_bytes(kexGexReply, d)
    {:ok, newKeys, ssh2} = :ssh_transport.new_keys_message(ssh1)
    send_bytes(newKeys, d)
    {:ok, extInfo, ssh} = :ssh_transport.ext_info_message(ssh2)
    send_bytes(extInfo, d)
    {:next_state, {:new_keys, :server, reNeg}, r_data(d, ssh_params: ssh)}
  end

  def handle_event(
        _,
        r_ssh_msg_kex_dh_gex_reply() = msg,
        {:key_exchange_dh_gex_reply, :client, reNeg},
        d
      ) do
    {:ok, newKeys, ssh1} =
      :ssh_transport.handle_kex_dh_gex_reply(
        msg,
        r_data(d, :ssh_params)
      )

    send_bytes(newKeys, d)
    {:ok, extInfo, ssh} = :ssh_transport.ext_info_message(ssh1)
    send_bytes(extInfo, d)
    {:next_state, {:new_keys, :client, reNeg}, r_data(d, ssh_params: ssh)}
  end

  def handle_event(_, r_ssh_msg_newkeys() = msg, {:new_keys, :client, :init}, d0) do
    {:ok, ssh1} =
      :ssh_transport.handle_new_keys(
        msg,
        r_data(d0, :ssh_params)
      )

    {msgReq, ssh} = :ssh_auth.service_request_msg(ssh1)
    d = send_msg(msgReq, r_data(d0, ssh_params: ssh))
    {:next_state, {:ext_info, :client, :init}, d}
  end

  def handle_event(_, r_ssh_msg_newkeys() = msg, {:new_keys, :server, :init}, d) do
    {:ok, ssh} =
      :ssh_transport.handle_new_keys(
        msg,
        r_data(d, :ssh_params)
      )

    {:next_state, {:ext_info, :server, :init}, r_data(d, ssh_params: ssh)}
  end

  def handle_event(_, r_ssh_msg_newkeys() = msg, {:new_keys, role, :renegotiate}, d) do
    {:ok, ssh} =
      :ssh_transport.handle_new_keys(
        msg,
        r_data(d, :ssh_params)
      )

    {:next_state, {:ext_info, role, :renegotiate}, r_data(d, ssh_params: ssh)}
  end

  def handle_event(_, r_ssh_msg_ext_info() = msg, {:ext_info, role, :init}, d0) do
    d = handle_ssh_msg_ext_info(msg, d0)
    {:next_state, {:service_request, role}, d}
  end

  def handle_event(_, r_ssh_msg_ext_info() = msg, {:ext_info, role, :renegotiate}, d0) do
    d = handle_ssh_msg_ext_info(msg, d0)
    {:next_state, {:connected, role}, d}
  end

  def handle_event(_, r_ssh_msg_newkeys() = msg, {:ext_info, _Role, :renegotiate}, d) do
    {:ok, ssh} =
      :ssh_transport.handle_new_keys(
        msg,
        r_data(d, :ssh_params)
      )

    {:keep_state, r_data(d, ssh_params: ssh)}
  end

  def handle_event(:internal, msg, {:ext_info, role, :init}, d)
      when is_tuple(msg) do
    {:next_state, {:service_request, role}, d, [:postpone]}
  end

  def handle_event(:internal, msg, {:ext_info, role, _ReNegFlag}, d)
      when is_tuple(msg) do
    {:next_state, {:connected, role}, d, [:postpone]}
  end

  def handle_event(
        _,
        msg = r_ssh_msg_service_request(name: serviceName),
        stateName = {:service_request, :server},
        d0
      ) do
    case serviceName do
      'ssh-userauth' ->
        ssh0 = r_ssh(session_id: sessionId) = r_data(d0, :ssh_params)
        {:ok, {reply, ssh}} = :ssh_auth.handle_userauth_request(msg, sessionId, ssh0)
        d = send_msg(reply, r_data(d0, ssh_params: ssh))
        {:next_state, {:userauth, :server}, d}

      _ ->
        {shutdown, d} =
          send_disconnect(
            7,
            :io_lib.format('Unknown service: ~p', [serviceName]),
            :ssh_connection_handler,
            892,
            stateName,
            d0
          )

        {:stop, shutdown, d}
    end
  end

  def handle_event(
        _,
        r_ssh_msg_service_accept(name: 'ssh-userauth'),
        {:service_request, :client},
        r_data(ssh_params: r_ssh(service: 'ssh-userauth') = ssh0) = d0
      ) do
    {msg, ssh} = :ssh_auth.init_userauth_request_msg(ssh0)

    d =
      send_msg(
        msg,
        r_data(d0, ssh_params: ssh, auth_user: r_ssh(ssh, :user))
      )

    {:next_state, {:userauth, :client}, d}
  end

  def handle_event(
        _,
        msg = r_ssh_msg_userauth_request(service: serviceName, method: method),
        stateName = {:userauth, :server},
        d0 = r_data(ssh_params: ssh0)
      ) do
    case {serviceName, r_ssh(ssh0, :service), method} do
      {'ssh-connection', 'ssh-connection', 'none'} ->
        {:not_authorized, _, {reply, ssh}} =
          :ssh_auth.handle_userauth_request(
            msg,
            r_ssh(ssh0, :session_id),
            ssh0
          )

        d = send_msg(reply, r_data(d0, ssh_params: ssh))
        {:keep_state, d}

      {'ssh-connection', 'ssh-connection', ^method} ->
        case :lists.member(
               method,
               r_ssh(ssh0, :userauth_methods)
             ) do
          true ->
            case :ssh_auth.handle_userauth_request(
                   msg,
                   r_ssh(ssh0, :session_id),
                   ssh0
                 ) do
              {:authorized, user, {reply, ssh1}} ->
                d =
                  r_data(ssh_params: ssh) =
                  send_msg(
                    reply,
                    r_data(d0, ssh_params: ssh1)
                  )

                send(r_data(d, :starter), :ssh_connected)
                connected_fun(user, method, d)

                {:next_state, {:connected, :server},
                 r_data(d,
                   auth_user: user,
                   ssh_params: r_ssh(ssh, authenticated: true)
                 )}

              {:not_authorized, {user, reason}, {reply, ssh}}
              when method == 'keyboard-interactive' ->
                retry_fun(user, reason, d0)
                d = send_msg(reply, r_data(d0, ssh_params: ssh))
                {:next_state, {:userauth_keyboard_interactive, :server}, d}

              {:not_authorized, {user, reason}, {reply, ssh}} ->
                retry_fun(user, reason, d0)
                d = send_msg(reply, r_data(d0, ssh_params: ssh))
                {:keep_state, d}
            end

          false ->
            {:keep_state_and_data,
             [{:next_event, :internal, r_ssh_msg_userauth_request(msg, method: 'none')}]}
        end

      {^serviceName, _, _} when serviceName !== 'ssh-connection' ->
        {shutdown, d} =
          send_disconnect(
            7,
            :io_lib.format('Unknown service: ~p', [serviceName]),
            :ssh_connection_handler,
            961,
            stateName,
            d0
          )

        {:stop, shutdown, d}
    end
  end

  def handle_event(_, r_ssh_msg_ext_info() = msg, {:userauth, :client}, d0) do
    d = handle_ssh_msg_ext_info(msg, d0)
    {:keep_state, d}
  end

  def handle_event(
        _,
        r_ssh_msg_userauth_success(),
        {:userauth, :client},
        d = r_data(ssh_params: ssh)
      ) do
    :ssh_auth.ssh_msg_userauth_result(:success)
    send(r_data(d, :starter), :ssh_connected)
    {:next_state, {:connected, :client}, r_data(d, ssh_params: r_ssh(ssh, authenticated: true))}
  end

  def handle_event(
        _,
        r_ssh_msg_userauth_failure(),
        {:userauth, :client} = stateName,
        r_data(ssh_params: r_ssh(userauth_methods: [])) = d0
      ) do
    {shutdown, d} =
      send_disconnect(
        14,
        :io_lib.format('User auth failed for: ~p', [r_data(d0, :auth_user)]),
        :ssh_connection_handler,
        983,
        stateName,
        d0
      )

    {:stop, shutdown, d}
  end

  def handle_event(
        _,
        r_ssh_msg_userauth_failure(authentications: methods),
        stateName = {:userauth, :client},
        d0 = r_data(ssh_params: ssh0)
      ) do
    ssh1 =
      case r_ssh(ssh0, :userauth_methods) do
        :none ->
          r_ssh(ssh0, userauth_methods: :string.tokens(methods, ','))

        _ ->
          ssh0
      end

    case :ssh_auth.userauth_request_msg(ssh1) do
      {:send_disconnect, code, ssh} ->
        {shutdown, d} =
          send_disconnect(
            code,
            :io_lib.format(
              'User auth failed for: ~p',
              [r_data(d0, :auth_user)]
            ),
            :ssh_connection_handler,
            1001,
            stateName,
            r_data(d0, ssh_params: ssh)
          )

        {:stop, shutdown, d}

      {'keyboard-interactive', {msg, ssh}} ->
        d = send_msg(msg, r_data(d0, ssh_params: ssh))
        {:next_state, {:userauth_keyboard_interactive, :client}, d}

      {_Method, {msg, ssh}} ->
        d = send_msg(msg, r_data(d0, ssh_params: ssh))
        {:keep_state, d}
    end
  end

  def handle_event(_, r_ssh_msg_userauth_banner(message: msg), {:userauth, :client}, d) do
    case r_ssh(r_data(d, :ssh_params), :userauth_quiet_mode) do
      false ->
        :io.format('~s', [msg])

      true ->
        :ok
    end

    :keep_state_and_data
  end

  def handle_event(
        _,
        r_ssh_msg_userauth_info_request() = msg,
        {:userauth_keyboard_interactive, :client},
        r_data(ssh_params: ssh0) = d0
      ) do
    case :ssh_auth.handle_userauth_info_request(
           msg,
           ssh0
         ) do
      {:ok, {reply, ssh}} ->
        d = send_msg(reply, r_data(d0, ssh_params: ssh))
        {:next_state, {:userauth_keyboard_interactive_info_response, :client}, d}

      :not_ok ->
        {:next_state, {:userauth, :client}, d0, [:postpone]}
    end
  end

  def handle_event(
        _,
        r_ssh_msg_userauth_info_response() = msg,
        {:userauth_keyboard_interactive, :server},
        d0
      ) do
    case :ssh_auth.handle_userauth_info_response(
           msg,
           r_data(d0, :ssh_params)
         ) do
      {:authorized, user, {reply, ssh1}} ->
        d =
          r_data(ssh_params: ssh) =
          send_msg(
            reply,
            r_data(d0, ssh_params: ssh1)
          )

        send(r_data(d, :starter), :ssh_connected)
        connected_fun(user, 'keyboard-interactive', d)

        {:next_state, {:connected, :server},
         r_data(d,
           auth_user: user,
           ssh_params: r_ssh(ssh, authenticated: true)
         )}

      {:not_authorized, {user, reason}, {reply, ssh}} ->
        retry_fun(user, reason, d0)
        d = send_msg(reply, r_data(d0, ssh_params: ssh))
        {:next_state, {:userauth, :server}, d}

      {:authorized_but_one_more, _User, {reply, ssh}} ->
        d = send_msg(reply, r_data(d0, ssh_params: ssh))
        {:next_state, {:userauth_keyboard_interactive_extra, :server}, d}
    end
  end

  def handle_event(
        _,
        r_ssh_msg_userauth_info_response() = msg,
        {:userauth_keyboard_interactive_extra, :server},
        d0
      ) do
    {:authorized, user, {reply, ssh1}} =
      :ssh_auth.handle_userauth_info_response(
        {:extra, msg},
        r_data(d0, :ssh_params)
      )

    d =
      r_data(ssh_params: ssh) =
      send_msg(
        reply,
        r_data(d0, ssh_params: ssh1)
      )

    send(r_data(d, :starter), :ssh_connected)
    connected_fun(user, 'keyboard-interactive', d)

    {:next_state, {:connected, :server},
     r_data(d,
       auth_user: user,
       ssh_params: r_ssh(ssh, authenticated: true)
     )}
  end

  def handle_event(
        _,
        r_ssh_msg_userauth_failure(),
        {:userauth_keyboard_interactive, :client},
        r_data(ssh_params: ssh0) = d0
      ) do
    prefs =
      for {method, m, f, a} <- r_ssh(ssh0, :userauth_preference),
          method !== 'keyboard-interactive' do
        {method, m, f, a}
      end

    d = r_data(d0, ssh_params: r_ssh(ssh0, userauth_preference: prefs))
    {:next_state, {:userauth, :client}, d, [:postpone]}
  end

  def handle_event(
        _,
        r_ssh_msg_userauth_failure(),
        {:userauth_keyboard_interactive_info_response, :client},
        r_data(ssh_params: ssh0) = d0
      ) do
    opts = r_ssh(ssh0, :opts)

    d =
      case :ssh_options.get_value(:user_options, :password, opts, :ssh_connection_handler, 1076) do
        :undefined ->
          d0

        _ ->
          r_data(d0,
            ssh_params:
              r_ssh(ssh0,
                opts:
                  :ssh_options.put_value(
                    :user_options,
                    {:password, :not_ok},
                    opts,
                    :ssh_connection_handler,
                    1081
                  )
              )
          )
      end

    {:next_state, {:userauth, :client}, d, [:postpone]}
  end

  def handle_event(
        _,
        r_ssh_msg_ext_info() = msg,
        {:userauth_keyboard_interactive_info_response, :client},
        d0
      ) do
    d = handle_ssh_msg_ext_info(msg, d0)
    {:keep_state, d}
  end

  def handle_event(
        _,
        r_ssh_msg_userauth_success(),
        {:userauth_keyboard_interactive_info_response, :client},
        d
      ) do
    {:next_state, {:userauth, :client}, d, [:postpone]}
  end

  def handle_event(
        _,
        r_ssh_msg_userauth_info_request(),
        {:userauth_keyboard_interactive_info_response, :client},
        d
      ) do
    {:next_state, {:userauth_keyboard_interactive, :client}, d, [:postpone]}
  end

  def handle_event(_, r_ssh_msg_ext_info(), {:connected, _Role}, d) do
    {:keep_state, d}
  end

  def handle_event(_, {r_ssh_msg_kexinit(), _}, {:connected, role}, d0) do
    {keyInitMsg, sshPacket, ssh} = :ssh_transport.key_exchange_init_msg(r_data(d0, :ssh_params))

    d =
      r_data(d0,
        ssh_params: ssh,
        key_exchange_init_msg: keyInitMsg
      )

    send_bytes(sshPacket, d)
    {:next_state, {:kexinit, role, :renegotiate}, d, [:postpone]}
  end

  def handle_event(_, r_ssh_msg_disconnect(description: desc) = msg, stateName, d0) do
    {:disconnect, _, repliesCon} =
      :ssh_connection.handle_msg(
        msg,
        r_data(d0, :connection_state),
        role(stateName)
      )

    {actions, d} = send_replies(repliesCon, d0)
    disconnect_fun('Received disconnect: ' ++ desc, d)
    {:stop_and_reply, {:shutdown, desc}, actions, d}
  end

  def handle_event(_, r_ssh_msg_ignore(), _, _) do
    :keep_state_and_data
  end

  def handle_event(_, r_ssh_msg_unimplemented(), _, _) do
    :keep_state_and_data
  end

  def handle_event(_, r_ssh_msg_debug() = msg, _, d) do
    debug_fun(msg, d)
    :keep_state_and_data
  end

  def handle_event(
        :internal,
        {:conn_msg, msg},
        stateName,
        r_data(starter: user, connection_state: connection0, event_queue: qev0) = d0
      ) do
    role = role(stateName)
    rengotation = renegotiation(stateName)

    try do
      :ssh_connection.handle_msg(msg, connection0, role)
    catch
      class, error ->
        {repls, d1} =
          send_replies(
            :ssh_connection.handle_stop(connection0),
            d0
          )

        {shutdown, d} =
          send_disconnect(
            11,
            :io_lib.format('Internal error: ~p:~p', [class, error]),
            :ssh_connection_handler,
            1167,
            stateName,
            d1
          )

        {:stop_and_reply, shutdown, repls, d}
    else
      {:disconnect, reason0, repliesConn} ->
        {repls, d} = send_replies(repliesConn, d0)

        case {reason0, role} do
          {{_, reason}, :client} when stateName !== {:connected, :client} and not rengotation ->
            send(user, {self(), :not_connected, reason})

          _ ->
            :ok
        end

        {:stop_and_reply, {:shutdown, :normal}, repls, d}

      {replies, connection} when is_list(replies) ->
        {repls, d} =
          case stateName do
            {:connected, _} ->
              send_replies(
                replies,
                r_data(d0, connection_state: connection)
              )

            _ ->
              {connReplies, nonConnReplies} =
                :lists.splitwith(
                  &not_connected_filter/1,
                  replies
                )

              send_replies(
                nonConnReplies,
                r_data(d0, event_queue: qev0 ++ connReplies)
              )
          end

        case {msg, stateName} do
          {r_ssh_msg_channel_close(), {:connected, _}} ->
            {:keep_state, d, [cond_set_idle_timer(d) | repls]}

          {r_ssh_msg_channel_success(), _} ->
            update_inet_buffers(r_data(d, :socket))
            {:keep_state, d, repls}

          _ ->
            {:keep_state, d, repls}
        end
    end
  end

  def handle_event(:enter, oldState, {:connected, _} = newState, d) do
    init_renegotiate_timers(oldState, newState, d)
  end

  def handle_event(:enter, oldState, {:ext_info, _, :renegotiate} = newState, d) do
    init_renegotiate_timers(oldState, newState, d)
  end

  def handle_event(:enter, {:connected, _} = oldState, newState, d) do
    pause_renegotiate_timers(oldState, newState, d)
  end

  def handle_event(:cast, :force_renegotiate, stateName, d) do
    handle_event({:timeout, :renegotiate}, :undefined, stateName, d)
  end

  def handle_event({:timeout, :renegotiate}, _, stateName, d0) do
    case stateName do
      {:connected, role} ->
        start_rekeying(role, d0)

      {:ext_info, role, :renegotiate} ->
        start_rekeying(role, d0)

      _ ->
        :keep_state_and_data
    end
  end

  def handle_event({:timeout, :check_data_size}, _, stateName, d0) do
    case stateName do
      {:connected, role} ->
        check_data_rekeying(role, d0)

      _ ->
        :keep_state_and_data
    end
  end

  def handle_event({:call, from}, :get_alg, _, d) do
    r_ssh(algorithms: algs) = r_data(d, :ssh_params)
    {:keep_state_and_data, [{:reply, from, algs}]}
  end

  def handle_event(:cast, _, stateName, _)
      when not (:erlang.element(
                  1,
                  stateName
                ) == :connected or
                  :erlang.element(
                    1,
                    stateName
                  ) == :ext_info) do
    {:keep_state_and_data, [:postpone]}
  end

  def handle_event(:cast, {:adjust_window, channelId, bytes}, stateName, d)
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    case :ssh_client_channel.cache_lookup(
           cache(d),
           channelId
         ) do
      r_channel(
        recv_window_size: winSize,
        recv_window_pending: pending,
        recv_packet_size: pktSize
      ) = channel
      when winSize - bytes >= 2 * pktSize ->
        :ssh_client_channel.cache_update(
          cache(d),
          r_channel(channel, recv_window_pending: pending + bytes)
        )

        :keep_state_and_data

      r_channel(
        recv_window_size: winSize,
        recv_window_pending: pending,
        remote_id: id
      ) = channel ->
        :ssh_client_channel.cache_update(
          cache(d),
          r_channel(channel,
            recv_window_size: winSize + bytes + pending,
            recv_window_pending: 0
          )
        )

        msg =
          :ssh_connection.channel_adjust_window_msg(
            id,
            bytes + pending
          )

        {:keep_state, send_msg(msg, d)}

      :undefined ->
        :keep_state_and_data
    end
  end

  def handle_event(:cast, {:reply_request, resp, channelId}, stateName, d)
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    case :ssh_client_channel.cache_lookup(
           cache(d),
           channelId
         ) do
      r_channel(remote_id: remoteId)
      when resp == :success or
             resp == :failure ->
        msg =
          case resp do
            :success ->
              :ssh_connection.channel_success_msg(remoteId)

            :failure ->
              :ssh_connection.channel_failure_msg(remoteId)
          end

        update_inet_buffers(r_data(d, :socket))
        {:keep_state, send_msg(msg, d)}

      r_channel() ->
        details = :io_lib.format('Unhandled reply in state ~p:~n~p', [stateName, resp])
        send_disconnect(2, details, :ssh_connection_handler, 1255, stateName, d)

      :undefined ->
        :keep_state_and_data
    end
  end

  def handle_event(:cast, {:request, channelPid, channelId, type, data}, stateName, d)
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    {:keep_state, handle_request(channelPid, channelId, type, data, false, :none, d)}
  end

  def handle_event(:cast, {:request, channelId, type, data}, stateName, d)
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    {:keep_state, handle_request(channelId, type, data, false, :none, d)}
  end

  def handle_event(:cast, {:unknown, data}, stateName, d)
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    msg = r_ssh_msg_unimplemented(sequence: data)
    {:keep_state, send_msg(msg, d)}
  end

  def handle_event(:cast, {:global_request, type, data}, stateName, d)
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    {:keep_state,
     send_msg(
       :ssh_connection.request_global_msg(type, false, data),
       d
     )}
  end

  def handle_event({:call, from}, :get_print_info, stateName, d) do
    reply =
      try do
        {:inet.sockname(r_data(d, :socket)), :inet.peername(r_data(d, :socket))}
      catch
        _, _ ->
          {{'?', 0}, '?'}
      else
        {{:ok, local}, {:ok, remote}} ->
          {{local, remote}, :io_lib.format('statename=~p', [stateName])}

        _ ->
          {{'-', 0}, '-'}
      end

    {:keep_state_and_data, [{:reply, from, reply}]}
  end

  def handle_event({:call, from}, {:connection_info, options}, _, d) do
    info = fold_keys(options, &conn_info/2, d)
    {:keep_state_and_data, [{:reply, from, info}]}
  end

  def handle_event({:call, from}, {:channel_info, channelId, options}, _, d) do
    case :ssh_client_channel.cache_lookup(
           cache(d),
           channelId
         ) do
      r_channel() = channel ->
        info = fold_keys(options, &chann_info/2, channel)
        {:keep_state_and_data, [{:reply, from, info}]}

      :undefined ->
        {:keep_state_and_data, [{:reply, from, []}]}
    end
  end

  def handle_event({:call, from}, {:info, :all}, _, d) do
    result =
      :ssh_client_channel.cache_foldl(
        fn channel, acc ->
          [channel | acc]
        end,
        [],
        cache(d)
      )

    {:keep_state_and_data, [{:reply, from, {:ok, result}}]}
  end

  def handle_event({:call, from}, {:info, channelPid}, _, d) do
    result =
      :ssh_client_channel.cache_foldl(
        fn
          channel, acc
          when r_channel(channel, :user) == channelPid ->
            [channel | acc]

          _, acc ->
            acc
        end,
        [],
        cache(d)
      )

    {:keep_state_and_data, [{:reply, from, {:ok, result}}]}
  end

  def handle_event({:call, from}, {:set_sock_opts, socketOptions}, _StateName, d) do
    result =
      try do
        :inet.setopts(r_data(d, :socket), socketOptions)
      catch
        _, _ ->
          {:error, :badarg}
      end

    {:keep_state_and_data, [{:reply, from, result}]}
  end

  def handle_event({:call, from}, {:get_sock_opts, socketGetOptions}, _StateName, d) do
    result =
      try do
        :inet.getopts(r_data(d, :socket), socketGetOptions)
      catch
        _, _ ->
          {:error, :badarg}
      end

    {:keep_state_and_data, [{:reply, from, result}]}
  end

  def handle_event({:call, from}, :stop, _StateName, d0) do
    {repls, d} =
      send_replies(
        :ssh_connection.handle_stop(r_data(d0, :connection_state)),
        d0
      )

    {:stop_and_reply, :normal, [{:reply, from, :ok} | repls], d}
  end

  def handle_event({:call, _}, _, stateName, _)
      when not (:erlang.element(
                  1,
                  stateName
                ) == :connected or
                  :erlang.element(
                    1,
                    stateName
                  ) == :ext_info) do
    {:keep_state_and_data, [:postpone]}
  end

  def handle_event(
        {:call, from},
        {:request, channelPid, channelId, type, data, timeout},
        stateName,
        d0
      )
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    case handle_request(channelPid, channelId, type, data, true, from, d0) do
      {:error, error} ->
        {:keep_state, d0, {:reply, from, {:error, error}}}

      d ->
        start_channel_request_timer(channelId, from, timeout)
        {:keep_state, d, cond_set_idle_timer(d)}
    end
  end

  def handle_event({:call, from}, {:request, channelId, type, data, timeout}, stateName, d0)
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    case handle_request(channelId, type, data, true, from, d0) do
      {:error, error} ->
        {:keep_state, d0, {:reply, from, {:error, error}}}

      d ->
        start_channel_request_timer(channelId, from, timeout)
        {:keep_state, d, cond_set_idle_timer(d)}
    end
  end

  def handle_event(
        {:call, from},
        {:global_request, 'tcpip-forward' = type,
         {listenHost, listenPort, connectToHost, connectToPort}, timeout},
        stateName,
        d0
      )
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    id = make_ref()

    data =
      <<:erlang.size(listenHost)::size(32)-unsigned-big-integer, listenHost::binary,
        listenPort::size(32)-unsigned-big-integer>>

    fun = fn
      {:success, <<port::size(32)-unsigned-integer>>}, c ->
        key = {:tcpip_forward, listenHost, port}
        value = {connectToHost, connectToPort}

        r_connection(c,
          options:
            :ssh_options.put_value(
              :internal_options,
              {key, value},
              r_connection(c, :options),
              :ssh_connection_handler,
              1374
            )
        )

      {:success, <<>>}, c ->
        key = {:tcpip_forward, listenHost, listenPort}
        value = {connectToHost, connectToPort}

        r_connection(c,
          options:
            :ssh_options.put_value(
              :internal_options,
              {key, value},
              r_connection(c, :options),
              :ssh_connection_handler,
              1378
            )
        )

      _, c ->
        c
    end

    d =
      send_msg(
        :ssh_connection.request_global_msg(type, true, data),
        add_request(fun, id, from, d0)
      )

    start_channel_request_timer(id, from, timeout)
    {:keep_state, d, cond_set_idle_timer(d)}
  end

  def handle_event({:call, from}, {:global_request, type, data, timeout}, stateName, d0)
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    id = make_ref()

    d =
      send_msg(
        :ssh_connection.request_global_msg(type, true, data),
        add_request(true, id, from, d0)
      )

    start_channel_request_timer(id, from, timeout)
    {:keep_state, d, cond_set_idle_timer(d)}
  end

  def handle_event({:call, from}, {:data, channelId, type, data, timeout}, stateName, d0)
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    {repls, d} =
      send_replies(
        :ssh_connection.channel_data(channelId, type, data, r_data(d0, :connection_state), from),
        d0
      )

    start_channel_request_timer(channelId, from, timeout)
    {:keep_state, d, repls}
  end

  def handle_event({:call, from}, {:eof, channelId}, stateName, d0)
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    case :ssh_client_channel.cache_lookup(
           cache(d0),
           channelId
         ) do
      r_channel(remote_id: id, sent_close: false) ->
        d = send_msg(:ssh_connection.channel_eof_msg(id), d0)
        {:keep_state, d, [{:reply, from, :ok}]}

      _ ->
        {:keep_state, d0, [{:reply, from, {:error, :closed}}]}
    end
  end

  def handle_event(
        {:call, from},
        :get_misc,
        stateName,
        r_data(connection_state: r_connection(options: opts)) = d
      )
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    sups =
      :ssh_options.get_value(:internal_options, :supervisors, opts, :ssh_connection_handler, 1413)

    subSysSup = :proplists.get_value(:subsystem_sup, sups)
    reply = {:ok, {subSysSup, role(stateName), opts}}
    {:keep_state, d, [{:reply, from, reply}]}
  end

  def handle_event(
        {:call, from},
        {:open, channelPid, type, initialWindowSize, maxPacketSize, data, timeout},
        stateName,
        d0
      )
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    :erlang.monitor(:process, channelPid)
    {channelId, d1} = new_channel_id(d0)

    d2 =
      send_msg(
        :ssh_connection.channel_open_msg(type, channelId, initialWindowSize, maxPacketSize, data),
        d1
      )

    :ssh_client_channel.cache_update(
      cache(d2),
      r_channel(
        type: type,
        sys: 'none',
        user: channelPid,
        local_id: channelId,
        recv_window_size: initialWindowSize,
        recv_packet_size: maxPacketSize,
        send_buf: :queue.new()
      )
    )

    d = add_request(true, channelId, from, d2)
    start_channel_request_timer(channelId, from, timeout)
    {:keep_state, d, cond_set_idle_timer(d)}
  end

  def handle_event({:call, from}, {:send_window, channelId}, stateName, d)
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    reply =
      case :ssh_client_channel.cache_lookup(
             cache(d),
             channelId
           ) do
        r_channel(
          send_window_size: winSize,
          send_packet_size: packsize
        ) ->
          {:ok, {winSize, packsize}}

        :undefined ->
          {:error, :einval}
      end

    {:keep_state_and_data, [{:reply, from, reply}]}
  end

  def handle_event({:call, from}, {:recv_window, channelId}, stateName, d)
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    reply =
      case :ssh_client_channel.cache_lookup(
             cache(d),
             channelId
           ) do
        r_channel(
          recv_window_size: winSize,
          recv_packet_size: packsize
        ) ->
          {:ok, {winSize, packsize}}

        :undefined ->
          {:error, :einval}
      end

    {:keep_state_and_data, [{:reply, from, reply}]}
  end

  def handle_event({:call, from}, {:close, channelId}, stateName, d0)
      when :erlang.element(
             1,
             stateName
           ) == :connected or
             :erlang.element(
               1,
               stateName
             ) == :ext_info do
    case :ssh_client_channel.cache_lookup(
           cache(d0),
           channelId
         ) do
      r_channel(remote_id: id) = channel ->
        d1 = send_msg(:ssh_connection.channel_close_msg(id), d0)

        :ssh_client_channel.cache_update(
          cache(d1),
          r_channel(channel, sent_close: true)
        )

        {:keep_state, d1, [cond_set_idle_timer(d1), {:reply, from, :ok}]}

      :undefined ->
        {:keep_state_and_data, [{:reply, from, :ok}]}
    end
  end

  def handle_event(:cast, {:store, key, value}, _StateName, r_data(connection_state: c0) = d) do
    c =
      r_connection(c0,
        options:
          :ssh_options.put_value(
            :internal_options,
            {key, value},
            r_connection(c0, :options),
            :ssh_connection_handler,
            1475
          )
      )

    {:keep_state, r_data(d, connection_state: c)}
  end

  def handle_event({:call, from}, {:retrieve, key}, _StateName, r_data(connection_state: c)) do
    case retrieve(c, key) do
      {:ok, value} ->
        {:keep_state_and_data, [{:reply, from, {:ok, value}}]}

      _ ->
        {:keep_state_and_data, [{:reply, from, :undefined}]}
    end
  end

  def handle_event(
        :info,
        {proto, sock, info},
        {:hello, _},
        r_data(socket: sock, transport_protocol: proto)
      ) do
    case info do
      'SSH-' ++ _ ->
        {:keep_state_and_data, [{:next_event, :internal, {:version_exchange, info}}]}

      _ ->
        {:keep_state_and_data, [{:next_event, :internal, {:info_line, info}}]}
    end
  end

  def handle_event(
        :info,
        {proto, sock, newData},
        stateName,
        d0 = r_data(socket: sock, transport_protocol: proto)
      ) do
    try do
      :ssh_transport.handle_packet_part(
        r_data(d0, :decrypted_data_buffer),
        <<r_data(d0, :encrypted_data_buffer)::binary, newData::binary>>,
        r_data(d0, :aead_data),
        r_data(d0, :undecrypted_packet_length),
        r_data(d0, :ssh_params)
      )
    catch
      c, e ->
        {shutdown, d} =
          send_disconnect(
            2,
            :io_lib.format(
              'Bad packet: Couldn\'t decrypt~n~p:~p~n~p',
              [c, e, __STACKTRACE__]
            ),
            :ssh_connection_handler,
            1578,
            stateName,
            d0
          )

        {:stop, shutdown, d}
    else
      {:packet_decrypted, decryptedBytes, encryptedDataRest, ssh1} ->
        d1 =
          r_data(d0,
            ssh_params:
              r_ssh(ssh1, recv_sequence: :ssh_transport.next_seqnum(r_ssh(ssh1, :recv_sequence))),
            decrypted_data_buffer: <<>>,
            undecrypted_packet_length: :undefined,
            aead_data: <<>>,
            encrypted_data_buffer: encryptedDataRest
          )

        try do
          :ssh_message.decode(
            set_kex_overload_prefix(
              decryptedBytes,
              d1
            )
          )
        catch
          c, e ->
            {shutdown, d} =
              send_disconnect(
                2,
                :io_lib.format(
                  'Bad packet: Decrypted, but can\'t decode~n~p:~p~n~p',
                  [c, e, __STACKTRACE__]
                ),
                :ssh_connection_handler,
                1545,
                stateName,
                d1
              )

            {:stop, shutdown, d}
        else
          r_ssh_msg_kexinit() = msg ->
            {:keep_state, d1,
             [
               {:next_event, :internal, :prepare_next_packet},
               {:next_event, :internal, {msg, decryptedBytes}}
             ]}

          r_ssh_msg_global_request() = msg ->
            {:keep_state, d1,
             [
               {:next_event, :internal, :prepare_next_packet},
               {:next_event, :internal, {:conn_msg, msg}}
             ]}

          r_ssh_msg_request_success() = msg ->
            {:keep_state, d1,
             [
               {:next_event, :internal, :prepare_next_packet},
               {:next_event, :internal, {:conn_msg, msg}}
             ]}

          r_ssh_msg_request_failure() = msg ->
            {:keep_state, d1,
             [
               {:next_event, :internal, :prepare_next_packet},
               {:next_event, :internal, {:conn_msg, msg}}
             ]}

          r_ssh_msg_channel_open() = msg ->
            {:keep_state, d1,
             [
               {:next_event, :internal, :prepare_next_packet},
               {:next_event, :internal, {:conn_msg, msg}}
             ]}

          r_ssh_msg_channel_open_confirmation() = msg ->
            {:keep_state, d1,
             [
               {:next_event, :internal, :prepare_next_packet},
               {:next_event, :internal, {:conn_msg, msg}}
             ]}

          r_ssh_msg_channel_open_failure() = msg ->
            {:keep_state, d1,
             [
               {:next_event, :internal, :prepare_next_packet},
               {:next_event, :internal, {:conn_msg, msg}}
             ]}

          r_ssh_msg_channel_window_adjust() = msg ->
            {:keep_state, d1,
             [
               {:next_event, :internal, :prepare_next_packet},
               {:next_event, :internal, {:conn_msg, msg}}
             ]}

          r_ssh_msg_channel_data() = msg ->
            {:keep_state, d1,
             [
               {:next_event, :internal, :prepare_next_packet},
               {:next_event, :internal, {:conn_msg, msg}}
             ]}

          r_ssh_msg_channel_extended_data() = msg ->
            {:keep_state, d1,
             [
               {:next_event, :internal, :prepare_next_packet},
               {:next_event, :internal, {:conn_msg, msg}}
             ]}

          r_ssh_msg_channel_eof() = msg ->
            {:keep_state, d1,
             [
               {:next_event, :internal, :prepare_next_packet},
               {:next_event, :internal, {:conn_msg, msg}}
             ]}

          r_ssh_msg_channel_close() = msg ->
            {:keep_state, d1,
             [
               {:next_event, :internal, :prepare_next_packet},
               {:next_event, :internal, {:conn_msg, msg}}
             ]}

          r_ssh_msg_channel_request() = msg ->
            {:keep_state, d1,
             [
               {:next_event, :internal, :prepare_next_packet},
               {:next_event, :internal, {:conn_msg, msg}}
             ]}

          r_ssh_msg_channel_failure() = msg ->
            {:keep_state, d1,
             [
               {:next_event, :internal, :prepare_next_packet},
               {:next_event, :internal, {:conn_msg, msg}}
             ]}

          r_ssh_msg_channel_success() = msg ->
            {:keep_state, d1,
             [
               {:next_event, :internal, :prepare_next_packet},
               {:next_event, :internal, {:conn_msg, msg}}
             ]}

          msg ->
            {:keep_state, d1,
             [{:next_event, :internal, :prepare_next_packet}, {:next_event, :internal, msg}]}
        end

      {:get_more, decryptedBytes, encryptedDataRest, aeadData, remainingSshPacketLen, ssh1} ->
        :inet.setopts(sock, [{:active, :once}])

        {:keep_state,
         r_data(d0,
           encrypted_data_buffer: encryptedDataRest,
           decrypted_data_buffer: decryptedBytes,
           undecrypted_packet_length: remainingSshPacketLen,
           aead_data: aeadData,
           ssh_params: ssh1
         )}

      {:bad_mac, ssh1} ->
        {shutdown, d} =
          send_disconnect(
            2,
            'Bad packet: bad mac',
            :ssh_connection_handler,
            1563,
            stateName,
            r_data(d0, ssh_params: ssh1)
          )

        {:stop, shutdown, d}

      {:error, {:exceeds_max_size, packetLen}} ->
        {shutdown, d} =
          send_disconnect(
            2,
            :io_lib.format('Bad packet: Size (~p bytes) exceeds max size', [packetLen]),
            :ssh_connection_handler,
            1571,
            stateName,
            d0
          )

        {:stop, shutdown, d}
    end
  end

  def handle_event(:internal, :prepare_next_packet, _, d) do
    enough =
      :erlang.max(
        8,
        r_ssh(r_data(d, :ssh_params), :decrypt_block_size)
      )

    case :erlang.size(r_data(d, :encrypted_data_buffer)) do
      sz when sz >= enough ->
        send(self(), {r_data(d, :transport_protocol), r_data(d, :socket), <<>>})

      _ ->
        :ok
    end

    :inet.setopts(r_data(d, :socket), [{:active, :once}])
    :keep_state_and_data
  end

  def handle_event(
        :info,
        {closeTag, socket},
        _StateName,
        d0 = r_data(socket: socket, transport_close_tag: closeTag, connection_state: c0)
      ) do
    {repls, d} = send_replies(:ssh_connection.handle_stop(c0), d0)
    disconnect_fun('Received a transport close', d)
    {:stop_and_reply, {:shutdown, 'Connection closed'}, repls, d}
  end

  def handle_event(
        :info,
        {:timeout, {_, from} = request},
        _,
        r_data(connection_state: r_connection(requests: requests) = c0) = d
      ) do
    case :lists.member(request, requests) do
      true ->
        c = r_connection(c0, requests: :lists.delete(request, requests))
        {:keep_state, r_data(d, connection_state: c), [{:reply, from, {:error, :timeout}}]}

      false ->
        :keep_state_and_data
    end
  end

  def handle_event(:info, {:DOWN, _Ref, :process, channelPid, _Reason}, _, d) do
    cache = cache(d)

    :ssh_client_channel.cache_foldl(
      fn
        r_channel(
          user: u,
          local_id: id
        ),
        acc
        when u == channelPid ->
          :ssh_client_channel.cache_delete(
            cache,
            id
          )

          acc

        _, acc ->
          acc
      end,
      [],
      cache
    )

    {:keep_state, d, cond_set_idle_timer(d)}
  end

  def handle_event({:timeout, :idle_time}, _Data, _StateName, _D) do
    {:stop, {:shutdown, 'Timeout'}}
  end

  def handle_event(:info, {:EXIT, _Sup, reason}, stateName, _) do
    role = role(stateName)

    cond do
      role == :client ->
        {:stop, {:shutdown, reason}}

      reason == :normal ->
        :keep_state_and_data

      true ->
        {:stop, {:shutdown, reason}}
    end
  end

  def handle_event(:info, :check_cache, _, d) do
    {:keep_state, d, cond_set_idle_timer(d)}
  end

  def handle_event(
        :info,
        {:fwd_connect_received, sock, chId, chanCB},
        stateName,
        r_data(connection_state: connection)
      ) do
    r_connection(options: options, channel_cache: cache, sub_system_supervisor: subSysSup) =
      connection

    channel = :ssh_client_channel.cache_lookup(cache, chId)

    {:ok, pid} =
      :ssh_subsystem_sup.start_channel(
        role(stateName),
        subSysSup,
        self(),
        chanCB,
        chId,
        [sock],
        :undefined,
        options
      )

    :ssh_client_channel.cache_update(
      cache,
      r_channel(channel, user: pid)
    )

    :gen_tcp.controlling_process(sock, pid)
    :inet.setopts(sock, [{:active, :once}])
    :keep_state_and_data
  end

  def handle_event(
        {:call, from},
        {:handle_direct_tcpip, listenHost, listenPort, connectToHost, connectToPort, _Timeout},
        _StateName,
        r_data(connection_state: r_connection(sub_system_supervisor: subSysSup))
      ) do
    case :ssh_tcpip_forward_acceptor.supervised_start(
           :ssh_subsystem_sup.tcpip_fwd_supervisor(subSysSup),
           {listenHost, listenPort},
           {connectToHost, connectToPort},
           'direct-tcpip',
           :ssh_tcpip_forward_client,
           self()
         ) do
      {:ok, lPort} ->
        {:keep_state_and_data, [{:reply, from, {:ok, lPort}}]}

      {:error, error} ->
        {:keep_state_and_data, [{:reply, from, {:error, error}}]}
    end
  end

  def handle_event(:info, unexpectedMessage, stateName, d = r_data(ssh_params: ssh)) do
    case unexpected_fun(unexpectedMessage, d) do
      :report ->
        msg =
          :lists.flatten(
            :io_lib.format(
              '*** SSH: Unexpected message \'~p\' received in state \'~p\'\nRole: ~p\nPeer: ~p\nLocal Address: ~p\n',
              [
                unexpectedMessage,
                stateName,
                r_ssh(ssh, :role),
                r_ssh(ssh, :peer),
                :ssh_options.get_value(
                  :internal_options,
                  :address,
                  r_ssh(ssh, :opts),
                  fn ->
                    :undefined
                  end,
                  :ssh_connection_handler,
                  1693
                )
              ]
            )
          )

        :error_logger.info_report(msg)
        :keep_state_and_data

      :skip ->
        :keep_state_and_data

      other ->
        msg =
          :lists.flatten(
            :io_lib.format(
              '*** SSH: Call to fun in \'unexpectedfun\' failed:~nReturn: ~p\nMessage: ~p\nRole: ~p\nPeer: ~p\nLocal Address: ~p\n',
              [
                other,
                unexpectedMessage,
                r_ssh(ssh, :role),
                r_ssh(ssh, :peer),
                :ssh_options.get_value(
                  :internal_options,
                  :address,
                  r_ssh(ssh, :opts),
                  fn ->
                    :undefined
                  end,
                  :ssh_connection_handler,
                  1713
                )
              ]
            )
          )

        :error_logger.error_report(msg)
        :keep_state_and_data
    end
  end

  def handle_event(:internal, {:send_disconnect, code, detailedText, module, line}, stateName, d0) do
    {shutdown, d} = send_disconnect(code, detailedText, module, line, stateName, d0)
    {:stop, shutdown, d}
  end

  def handle_event(:enter, _OldState, state, d) do
    {:next_state, state, d}
  end

  def handle_event(_Type, _Msg, {:ext_info, role, _ReNegFlag}, d) do
    {:next_state, {:connected, role}, d, [:postpone]}
  end

  def handle_event(type, ev, stateName, d0) do
    details =
      case (try do
              :erlang.atom_to_list(:erlang.element(1, ev))
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        'ssh_msg_' ++ _ when type == :internal ->
          :lists.flatten(
            :io_lib.format(
              'Message ~p in wrong state (~p)',
              [:erlang.element(1, ev), stateName]
            )
          )

        _ ->
          :io_lib.format('Unhandled event in state ~p:~n~p', [stateName, ev])
      end

    {shutdown, d} = send_disconnect(2, details, :ssh_connection_handler, 1742, stateName, d0)
    {:stop, shutdown, d}
  end

  def terminate(:normal, _StateName, d) do
    stop_subsystem(d)
    close_transport(d)
  end

  def terminate({:shutdown, 'Connection closed'}, _StateName, d) do
    stop_subsystem(d)
    close_transport(d)
  end

  def terminate({:shutdown, {:init, reason}}, stateName, d) do
    log(:error, d, 'Shutdown in init (StateName=~p): ~p~n', [stateName, reason])
    stop_subsystem(d)
    close_transport(d)
  end

  def terminate({:shutdown, _R}, _StateName, d) do
    stop_subsystem(d)
    close_transport(d)
  end

  def terminate(:shutdown, _StateName, d0) do
    d =
      send_msg(
        r_ssh_msg_disconnect(code: 11, description: 'Terminated (shutdown) by supervisor'),
        d0
      )

    close_transport(d)
  end

  def terminate(:kill, _StateName, d) do
    stop_subsystem(d)
    close_transport(d)
  end

  def terminate(reason, stateName, d0) do
    log(:error, d0, reason)

    {_ShutdownReason, d} =
      send_disconnect(
        11,
        'Internal error',
        :io_lib.format('Reason: ~p', [reason]),
        :ssh_connection_handler,
        1792,
        stateName,
        d0
      )

    stop_subsystem(d)
    close_transport(d)
  end

  def format_status(:normal, [_, _StateName, d]) do
    [{:data, [{'State', d}]}]
  end

  def format_status(:terminate, [_, _StateName, d]) do
    [{:data, [{'State', state_data2proplist(d)}]}]
  end

  defp state_data2proplist(d) do
    dataPropList0 =
      fmt_stat_rec(Keyword.keys(r_data(r_data())), d, [
        :decrypted_data_buffer,
        :encrypted_data_buffer,
        :key_exchange_init_msg,
        :user_passwords,
        :opts,
        :inet_initial_recbuf_size
      ])

    sshPropList =
      fmt_stat_rec(
        Keyword.keys(r_ssh(r_ssh())),
        r_data(d, :ssh_params),
        [
          :c_keyinit,
          :s_keyinit,
          :send_mac_key,
          :send_mac_size,
          :recv_mac_key,
          :recv_mac_size,
          :encrypt_keys,
          :encrypt_ctx,
          :decrypt_keys,
          :decrypt_ctx,
          :compress_ctx,
          :decompress_ctx,
          :shared_secret,
          :exchanged_hash,
          :session_id,
          :keyex_key,
          :keyex_info,
          :available_host_keys
        ]
      )

    :lists.keyreplace(:ssh_params, 1, dataPropList0, {:ssh_params, sshPropList})
  end

  defp fmt_stat_rec(fieldNames, rec, exclude) do
    values = tl(:erlang.tuple_to_list(rec))

    for {k, _} = p <- :lists.zip(fieldNames, values),
        not :lists.member(k, exclude) do
      p
    end
  end

  def code_change(_OldVsn, stateName, state, _Extra) do
    {:ok, stateName, state}
  end

  defp start_the_connection_child(userPid, role, socket, options0) do
    sups =
      :ssh_options.get_value(
        :internal_options,
        :supervisors,
        options0,
        :ssh_connection_handler,
        1866
      )

    connectionSup =
      :proplists.get_value(
        :connection_sup,
        sups
      )

    options =
      :ssh_options.put_value(
        :internal_options,
        {:user_pid, userPid},
        options0,
        :ssh_connection_handler,
        1868
      )

    initArgs = [role, socket, options]

    {:ok, pid} =
      :ssh_connection_sup.start_child(
        connectionSup,
        initArgs
      )

    :ok = socket_control(socket, pid, options)
    pid
  end

  defp stop_subsystem(
         r_data(
           ssh_params: r_ssh(role: role),
           connection_state:
             r_connection(
               system_supervisor: sysSup,
               sub_system_supervisor: subSysSup
             )
         )
       )
       when is_pid(sysSup) and is_pid(subSysSup) do
    :erlang.process_flag(:trap_exit, false)
    c = self()

    spawn(fn ->
      mref = :erlang.monitor(:process, c)

      receive do
        {:DOWN, ^mref, :process, ^c, _Info} ->
          :ok
      after
        10000 ->
          :ok
      end

      case role do
        :client ->
          :ssh_system_sup.stop_system(role, sysSup)

        _ ->
          :ssh_system_sup.stop_subsystem(sysSup, subSysSup)
      end
    end)
  end

  defp stop_subsystem(_) do
    :ok
  end

  defp close_transport(r_data(transport_cb: transport, socket: socket)) do
    try do
      transport.close(socket)
    catch
      _, _ ->
        :ok
    else
      _ ->
        :ok
    end
  end

  defp peer_role(:client) do
    :server
  end

  defp peer_role(:server) do
    :client
  end

  def available_hkey_algorithms(:client, options) do
    case available_hkey_algos(options) do
      [] ->
        :erlang.error({:shutdown, 'No public key algs'})

      algs ->
        for a <- algs do
          :erlang.atom_to_list(a)
        end
    end
  end

  def available_hkey_algorithms(:server, options) do
    case (for a <- available_hkey_algos(options),
              is_usable_host_key(a, options) do
            a
          end) do
      [] ->
        :erlang.error({:shutdown, 'No host key available'})

      algs ->
        for a <- algs do
          :erlang.atom_to_list(a)
        end
    end
  end

  defp available_hkey_algos(options) do
    supAlgos = :ssh_transport.supported_algorithms(:public_key)

    hKeys =
      :proplists.get_value(
        :public_key,
        :ssh_options.get_value(
          :user_options,
          :preferred_algorithms,
          options,
          :ssh_connection_handler,
          1940
        )
      )

    nonSupported = hKeys -- supAlgos
    availableAndSupported = hKeys -- nonSupported
    availableAndSupported
  end

  defp send_msg(msg, state = r_data(ssh_params: ssh0))
       when is_tuple(msg) do
    {bytes, ssh} = :ssh_transport.ssh_packet(msg, ssh0)
    send_bytes(bytes, state)
    r_data(state, ssh_params: ssh)
  end

  defp send_bytes('', _D) do
    :ok
  end

  defp send_bytes(
         bytes,
         r_data(socket: socket, transport_cb: transport)
       ) do
    _ = transport.send(socket, bytes)
    :ok
  end

  defp handle_version({2, 0} = numVsn, strVsn, ssh0) do
    ssh = counterpart_versions(numVsn, strVsn, ssh0)
    {:ok, ssh}
  end

  defp handle_version(_, _, _) do
    :not_supported
  end

  defp string_version(r_ssh(role: :client, c_version: vsn)) do
    vsn
  end

  defp string_version(r_ssh(role: :server, s_version: vsn)) do
    vsn
  end

  defp cast(fsmPid, event) do
    :gen_statem.cast(fsmPid, event)
  end

  defp call(fsmPid, event) do
    call(fsmPid, event, :infinity)
  end

  defp call(fsmPid, event, timeout) do
    try do
      :gen_statem.call(fsmPid, event, timeout)
    catch
      :exit, {:noproc, _R} ->
        {:error, :closed}

      :exit, {:normal, _R} ->
        {:error, :closed}

      :exit, {{:shutdown, _R}, _} ->
        {:error, :closed}

      :exit, {:shutdown, _R} ->
        {:error, :closed}
    else
      {:closed, _R} ->
        {:error, :closed}

      {:killed, _R} ->
        {:error, :closed}

      result ->
        result
    end
  end

  defp set_kex_overload_prefix(
         msg = <<op::size(8)-unsigned-big-integer, _::binary>>,
         r_data(ssh_params: sshParams)
       )
       when op == 30 or op == 31 do
    case (try do
            :erlang.atom_to_list(kex(sshParams))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      'ecdh-sha2-' ++ _ ->
        <<"ecdh", msg::binary>>

      'curve25519-' ++ _ ->
        <<"ecdh", msg::binary>>

      'curve448-' ++ _ ->
        <<"ecdh", msg::binary>>

      'diffie-hellman-group-exchange-' ++ _ ->
        <<"dh_gex", msg::binary>>

      'diffie-hellman-group' ++ _ ->
        <<"dh", msg::binary>>

      _ ->
        msg
    end
  end

  defp set_kex_overload_prefix(msg, _) do
    msg
  end

  defp kex(r_ssh(algorithms: r_alg(kex: kex))) do
    kex
  end

  defp kex(_) do
    :undefined
  end

  defp cache(r_data(connection_state: c)) do
    r_connection(c, :channel_cache)
  end

  defp handle_ssh_msg_ext_info(
         r_ssh_msg_ext_info(),
         d = r_data(ssh_params: r_ssh(recv_ext_info: false))
       ) do
    d
  end

  defp handle_ssh_msg_ext_info(r_ssh_msg_ext_info(data: data), d0) do
    :lists.foldl(&ext_info/2, d0, data)
  end

  defp ext_info(
         {'server-sig-algs', sigAlgsStr},
         d0 =
           r_data(
             ssh_params:
               r_ssh(
                 role: :client,
                 userauth_pubkeys: clientSigAlgs
               ) = ssh0
           )
       ) do
    sigAlgs =
      for astr <- :string.tokens(sigAlgsStr, ','),
          a <-
            (try do
               [:erlang.list_to_existing_atom(astr)]
             catch
               _, _ ->
                 []
             end) do
        a
      end

    commonAlgs =
      for a <- sigAlgs,
          :lists.member(a, clientSigAlgs) do
        a
      end

    r_data(d0,
      ssh_params: r_ssh(ssh0, userauth_pubkeys: commonAlgs ++ (clientSigAlgs -- commonAlgs))
    )
  end

  defp ext_info(_, d0) do
    d0
  end

  defp is_usable_user_pubkey(alg, ssh) do
    try do
      :ssh_auth.get_public_key(alg, ssh)
    catch
      _, _ ->
        false
    else
      {:ok, _} ->
        true

      _ ->
        false
    end
  end

  defp is_usable_host_key(alg, opts) do
    try do
      :ssh_transport.get_host_key(alg, opts)
    catch
      _, _ ->
        false
    else
      _PrivHostKey ->
        true
    end
  end

  defp handle_request(channelPid, channelId, type, data, wantReply, from, d) do
    case :ssh_client_channel.cache_lookup(
           cache(d),
           channelId
         ) do
      r_channel(remote_id: id, sent_close: false) = channel ->
        update_sys(cache(d), channel, type, channelPid)

        send_msg(
          :ssh_connection.channel_request_msg(id, type, wantReply, data),
          add_request(wantReply, channelId, from, d)
        )

      _ when wantReply == true ->
        {:error, :closed}

      _ ->
        d
    end
  end

  defp handle_request(channelId, type, data, wantReply, from, d) do
    case :ssh_client_channel.cache_lookup(
           cache(d),
           channelId
         ) do
      r_channel(remote_id: id, sent_close: false) ->
        send_msg(
          :ssh_connection.channel_request_msg(id, type, wantReply, data),
          add_request(wantReply, channelId, from, d)
        )

      _ when wantReply == true ->
        {:error, :closed}

      _ ->
        d
    end
  end

  defp update_sys(cache, channel, type, channelPid) do
    :ssh_client_channel.cache_update(
      cache,
      r_channel(channel, sys: type, user: channelPid)
    )
  end

  defp add_request(false, _ChannelId, _From, state) do
    state
  end

  defp add_request(
         true,
         channelId,
         from,
         r_data(connection_state: r_connection(requests: requests0) = connection) = state
       ) do
    requests = [{channelId, from} | requests0]
    r_data(state, connection_state: r_connection(connection, requests: requests))
  end

  defp add_request(
         fun,
         channelId,
         from,
         r_data(connection_state: r_connection(requests: requests0) = connection) = state
       )
       when is_function(fun) do
    requests = [{channelId, from, fun} | requests0]
    r_data(state, connection_state: r_connection(connection, requests: requests))
  end

  defp new_channel_id(
         r_data(connection_state: r_connection(channel_id_seed: id) = connection) = state
       ) do
    {id, r_data(state, connection_state: r_connection(connection, channel_id_seed: id + 1))}
  end

  defp start_rekeying(role, d0) do
    {keyInitMsg, sshPacket, ssh} = :ssh_transport.key_exchange_init_msg(r_data(d0, :ssh_params))
    send_bytes(sshPacket, d0)

    d =
      r_data(d0,
        ssh_params: ssh,
        key_exchange_init_msg: keyInitMsg
      )

    {:next_state, {:kexinit, role, :renegotiate}, d}
  end

  defp init_renegotiate_timers(_OldState, newState, d) do
    {rekeyTimeout, _MaxSent} =
      :ssh_options.get_value(
        :user_options,
        :rekey_limit,
        r_ssh(r_data(d, :ssh_params), :opts),
        :ssh_connection_handler,
        2150
      )

    {:next_state, newState, d,
     [
       {{:timeout, :renegotiate}, rekeyTimeout, :none},
       {{:timeout, :check_data_size}, 60000, :none}
     ]}
  end

  defp pause_renegotiate_timers(_OldState, newState, d) do
    {:next_state, newState, d,
     [
       {{:timeout, :renegotiate}, :infinity, :none},
       {{:timeout, :check_data_size}, :infinity, :none}
     ]}
  end

  defp check_data_rekeying(role, d) do
    case :inet.getstat(r_data(d, :socket), [:send_oct]) do
      {:ok, [{:send_oct, socketSentTotal}]} ->
        sentSinceRekey = socketSentTotal - r_data(d, :last_size_rekey)

        {_RekeyTimeout, maxSent} =
          :ssh_options.get_value(
            :user_options,
            :rekey_limit,
            r_ssh(r_data(d, :ssh_params), :opts),
            :ssh_connection_handler,
            2163
          )

        case check_data_rekeying_dbg(
               sentSinceRekey,
               maxSent
             ) do
          true ->
            start_rekeying(
              role,
              r_data(d, last_size_rekey: socketSentTotal)
            )

          _ ->
            {:keep_state, d, {{:timeout, :check_data_size}, 60000, :none}}
        end

      {:error, _} ->
        {:keep_state, d, {{:timeout, :check_data_size}, 60000, :none}}
    end
  end

  defp check_data_rekeying_dbg(sentSinceRekey, maxSent) do
    sentSinceRekey >= maxSent
  end

  defp send_disconnect(code, detailedText, module, line, stateName, d) do
    send_disconnect(code, default_text(code), detailedText, module, line, stateName, d)
  end

  defp send_disconnect(code, reason, detailedText, module, line, stateName, d0) do
    msg = r_ssh_msg_disconnect(code: code, description: reason)
    d = send_msg(msg, d0)
    logMsg = :io_lib.format('Disconnects with code = ~p [RFC4253 11.1]: ~s', [code, reason])
    call_disconnectfun_and_log_cond(logMsg, detailedText, module, line, stateName, d)
    {{:shutdown, reason}, d}
  end

  defp call_disconnectfun_and_log_cond(logMsg, detailedText, module, line, stateName, d) do
    case disconnect_fun(logMsg, d) do
      :void ->
        log(:info, d, '~s~nState = ~p~nModule = ~p, Line = ~p.~nDetails:~n  ~s~n', [
          logMsg,
          stateName,
          module,
          line,
          detailedText
        ])

      _ ->
        :ok
    end
  end

  defp default_text(1) do
    'Host not allowed to connect'
  end

  defp default_text(2) do
    'Protocol error'
  end

  defp default_text(3) do
    'Key exchange failed'
  end

  defp default_text(4) do
    'Reserved'
  end

  defp default_text(5) do
    'Mac error'
  end

  defp default_text(6) do
    'Compression error'
  end

  defp default_text(7) do
    'Service not available'
  end

  defp default_text(8) do
    'Protocol version not supported'
  end

  defp default_text(9) do
    'Host key not verifiable'
  end

  defp default_text(10) do
    'Connection lost'
  end

  defp default_text(11) do
    'By application'
  end

  defp default_text(12) do
    'Too many connections'
  end

  defp default_text(13) do
    'Auth cancelled by user'
  end

  defp default_text(14) do
    'Unable to connect using the available authentication methods'
  end

  defp default_text(15) do
    'Illegal user name'
  end

  defp counterpart_versions(numVsn, strVsn, r_ssh(role: :server) = ssh) do
    r_ssh(ssh, c_vsn: numVsn, c_version: strVsn)
  end

  defp counterpart_versions(numVsn, strVsn, r_ssh(role: :client) = ssh) do
    r_ssh(ssh, s_vsn: numVsn, s_version: strVsn)
  end

  defp conn_info_keys() do
    [:client_version, :server_version, :peer, :user, :sockname, :options, :algorithms, :channels]
  end

  defp conn_info(:client_version, r_data(ssh_params: s)) do
    {r_ssh(s, :c_vsn), r_ssh(s, :c_version)}
  end

  defp conn_info(:server_version, r_data(ssh_params: s)) do
    {r_ssh(s, :s_vsn), r_ssh(s, :s_version)}
  end

  defp conn_info(:peer, r_data(ssh_params: s)) do
    r_ssh(s, :peer)
  end

  defp conn_info(:user, d) do
    r_data(d, :auth_user)
  end

  defp conn_info(:sockname, r_data(ssh_params: s)) do
    r_ssh(s, :local)
  end

  defp conn_info(:options, r_data(ssh_params: r_ssh(opts: opts))) do
    :lists.sort(
      :maps.to_list(
        :ssh_options.keep_set_options(
          :client,
          :ssh_options.keep_user_options(
            :client,
            opts
          )
        )
      )
    )
  end

  defp conn_info(:algorithms, r_data(ssh_params: r_ssh(algorithms: a))) do
    conn_info_alg(a)
  end

  defp conn_info(:channels, d) do
    try do
      conn_info_chans(:ets.tab2list(cache(d)))
    catch
      _, _ ->
        :undefined
    end
  end

  defp conn_info(:socket, d) do
    r_data(d, :socket)
  end

  defp conn_info(:chan_ids, d) do
    :ssh_client_channel.cache_foldl(
      fn r_channel(local_id: id), acc ->
        [id | acc]
      end,
      [],
      cache(d)
    )
  end

  defp conn_info_chans(chs) do
    fs = Keyword.keys(r_channel(r_channel()))

    for ch = r_channel() <- chs do
      :lists.zip(fs, tl(:erlang.tuple_to_list(ch)))
    end
  end

  defp conn_info_alg(algTup) do
    [:alg | vs] = :erlang.tuple_to_list(algTup)
    fs = Keyword.keys(r_alg(r_alg()))

    for {k, v} <- :lists.zip(fs, vs),
        :lists.member(
          k,
          [
            :kex,
            :hkey,
            :encrypt,
            :decrypt,
            :send_mac,
            :recv_mac,
            :compress,
            :decompress,
            :send_ext_info,
            :recv_ext_info
          ]
        ) do
      {k, v}
    end
  end

  defp chann_info(:recv_window, c) do
    {{:win_size, r_channel(c, :recv_window_size)},
     {:packet_size, r_channel(c, :recv_packet_size)}}
  end

  defp chann_info(:send_window, c) do
    {{:win_size, r_channel(c, :send_window_size)},
     {:packet_size, r_channel(c, :send_packet_size)}}
  end

  defp chann_info(:pid, c) do
    r_channel(c, :user)
  end

  defp fold_keys(keys, fun, extra) do
    :lists.foldr(
      fn key, acc ->
        try do
          fun.(key, extra)
        catch
          _, _ ->
            acc
        else
          value ->
            [{key, value} | acc]
        end
      end,
      [],
      keys
    )
  end

  defp log(tag, d, format, args) do
    log(tag, d, :io_lib.format(format, args))
  end

  defp log(tag, d, reason) do
    case :erlang.atom_to_list(tag) do
      'error' ->
        do_log(:error_msg, reason, d)

      'warning' ->
        do_log(:warning_msg, reason, d)

      'info' ->
        do_log(:info_msg, reason, d)
    end
  end

  defp do_log(f, reason0, r_data(ssh_params: s)) do
    reason =
      try do
        :io_lib.format('~s', [reason0])
      catch
        _, _ ->
          :io_lib.format('~p', [reason0])
      else
        _ ->
          reason0
      end

    case s do
      r_ssh(role: role) when role == :server or role == :client ->
        {peerRole, peerVersion} =
          case role do
            :server ->
              {'Client', r_ssh(s, :c_version)}

            :client ->
              {'Server', r_ssh(s, :s_version)}
          end

        apply(:error_logger, f, [
          'Erlang SSH ~p ~s ~s.~n~s: ~p~n~s~n',
          [role, ssh_log_version(), crypto_log_info(), peerRole, peerVersion, reason]
        ])

      _ ->
        apply(:error_logger, f, [
          'Erlang SSH ~s ~s.~n~s~n',
          [ssh_log_version(), crypto_log_info(), reason]
        ])
    end
  end

  defp crypto_log_info() do
    try do
      [{_, _, cI}] = :crypto.info_lib()

      case :crypto.info_fips() do
        :enabled ->
          <<"(", cI::binary, ". FIPS enabled)">>

        :not_enabled ->
          <<"(", cI::binary, ". FIPS available but not enabled)">>

        _ ->
          <<"(", cI::binary, ")">>
      end
    catch
      _, _ ->
        ''
    end
  end

  defp ssh_log_version() do
    case :application.get_key(:ssh, :vsn) do
      {:ok, vsn} ->
        vsn

      :undefined ->
        ''
    end
  end

  defp not_connected_filter({:connection_reply, _Data}) do
    true
  end

  defp not_connected_filter(_) do
    false
  end

  defp send_replies({repls, c = r_connection()}, d) when is_list(repls) do
    send_replies(repls, r_data(d, connection_state: c))
  end

  defp send_replies(repls, state) do
    :lists.foldl(&get_repl/2, {[], state}, repls)
  end

  defp get_repl({:connection_reply, msg}, {callRepls, s}) do
    cond do
      elem(msg, 0) === :ssh_msg_channel_success ->
        update_inet_buffers(r_data(s, :socket))

      true ->
        :ok
    end

    {callRepls, send_msg(msg, s)}
  end

  defp get_repl({:channel_data, :undefined, _Data}, acc) do
    acc
  end

  defp get_repl({:channel_data, pid, data}, acc) do
    send(pid, {:ssh_cm, self(), data})
    acc
  end

  defp get_repl(
         {:channel_request_reply, from, data},
         {callRepls, s}
       ) do
    {[{:reply, from, data} | callRepls], s}
  end

  defp get_repl(
         {:flow_control, cache, channel, from, msg},
         {callRepls, s}
       ) do
    :ssh_client_channel.cache_update(
      cache,
      r_channel(channel, flow_control: :undefined)
    )

    {[{:reply, from, msg} | callRepls], s}
  end

  defp get_repl({:flow_control, from, msg}, {callRepls, s}) do
    {[{:reply, from, msg} | callRepls], s}
  end

  defp get_repl(x, acc) do
    exit({:get_repl, x, acc})
  end

  defp disconnect_fun(reason, d) do
    try do
      :ssh_options.get_value(
        :user_options,
        :disconnectfun,
        r_ssh(r_data(d, :ssh_params), :opts),
        :ssh_connection_handler,
        2410
      ).(reason)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp unexpected_fun(
         unexpectedMessage,
         r_data(ssh_params: r_ssh(peer: {_, peer})) = d
       ) do
    try do
      :ssh_options.get_value(
        :user_options,
        :unexpectedfun,
        r_ssh(r_data(d, :ssh_params), :opts),
        :ssh_connection_handler,
        2413
      ).(unexpectedMessage, peer)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp debug_fun(
         r_ssh_msg_debug(always_display: display, message: dbgMsg, language: lang),
         d
       ) do
    try do
      :ssh_options.get_value(
        :user_options,
        :ssh_msg_debug_fun,
        r_ssh(r_data(d, :ssh_params), :opts),
        :ssh_connection_handler,
        2419
      ).(self(), display, dbgMsg, lang)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp connected_fun(user, method, r_data(ssh_params: r_ssh(peer: {_, peer})) = d) do
    try do
      :ssh_options.get_value(
        :user_options,
        :connectfun,
        r_ssh(r_data(d, :ssh_params), :opts),
        :ssh_connection_handler,
        2423
      ).(user, peer, method)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp retry_fun(_, :undefined, _) do
    :ok
  end

  defp retry_fun(user, reason, r_data(ssh_params: r_ssh(opts: opts, peer: {_, peer}))) do
    {tag, info} =
      case reason do
        {:error, error} ->
          {:failfun, error}

        _ ->
          {:infofun, reason}
      end

    fun = :ssh_options.get_value(:user_options, tag, opts, :ssh_connection_handler, 2438)

    try do
      :erlang.fun_info(fun, :arity)
    catch
      _, _ ->
        :ok
    else
      {:arity, 2} ->
        try do
          fun.(user, info)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      {:arity, 3} ->
        try do
          fun.(user, peer, info)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      _ ->
        :ok
    end
  end

  defp cond_set_idle_timer(d) do
    case :ssh_client_channel.cache_info(
           :num_entries,
           cache(d)
         ) do
      0 ->
        {{:timeout, :idle_time},
         :ssh_options.get_value(
           :user_options,
           :idle_time,
           r_ssh(r_data(d, :ssh_params), :opts),
           :ssh_connection_handler,
           2458
         ), :none}

      _ ->
        {{:timeout, :idle_time}, :infinity, :none}
    end
  end

  defp start_channel_request_timer(_, _, :infinity) do
    :ok
  end

  defp start_channel_request_timer(channel, from, time) do
    :erlang.send_after(time, self(), {:timeout, {channel, from}})
  end

  defp socket_control(socket, pid, options) do
    {_, callback, _} =
      :ssh_options.get_value(:user_options, :transport, options, :ssh_connection_handler, 2472)

    case callback.controlling_process(socket, pid) do
      :ok ->
        :gen_statem.cast(pid, :socket_control)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp handshake(pid, ref, timeout) do
    receive do
      :ssh_connected ->
        :erlang.demonitor(ref)
        {:ok, pid}

      {^pid, :not_connected, reason} ->
        {:error, reason}

      {^pid, :user_password} ->
        pass = :io.get_password()
        send(pid, pass)
        handshake(pid, ref, timeout)

      {^pid, :question} ->
        answer = :io.get_line('')
        send(pid, answer)
        handshake(pid, ref, timeout)

      {:DOWN, _, :process, ^pid, {:shutdown, reason}} ->
        {:error, reason}

      {:DOWN, _, :process, ^pid, reason} ->
        {:error, reason}
    after
      timeout ->
        stop(pid)
        {:error, :timeout}
    end
  end

  defp update_inet_buffers(socket) do
    try do
      {:ok, bufSzs0} =
        :inet.getopts(
          socket,
          [:sndbuf, :recbuf]
        )

      minVal = 655_360

      for {tag, val} <- bufSzs0, val < minVal do
        {tag, minVal}
      end
    catch
      _, _ ->
        :ok
    else
      [] ->
        :ok

      newOpts ->
        :inet.setopts(socket, newOpts)
    end
  end

  def ssh_dbg_trace_points() do
    [:terminate, :disconnect, :connections, :connection_events, :renegotiation]
  end

  def ssh_dbg_flags(:connections) do
    [:c | ssh_dbg_flags(:terminate)]
  end

  def ssh_dbg_flags(:renegotiation) do
    [:c]
  end

  def ssh_dbg_flags(:connection_events) do
    [:c]
  end

  def ssh_dbg_flags(:terminate) do
    [:c]
  end

  def ssh_dbg_flags(:disconnect) do
    [:c]
  end

  def ssh_dbg_on(:connections) do
    :dbg.tp(:ssh_connection_handler, :init_connection_handler, 3, :x)
    ssh_dbg_on(:terminate)
  end

  def ssh_dbg_on(:connection_events) do
    :dbg.tp(:ssh_connection_handler, :handle_event, 4, :x)
  end

  def ssh_dbg_on(:renegotiation) do
    :dbg.tpl(:ssh_connection_handler, :init_renegotiate_timers, 3, :x)
    :dbg.tpl(:ssh_connection_handler, :pause_renegotiate_timers, 3, :x)
    :dbg.tpl(:ssh_connection_handler, :check_data_rekeying_dbg, 2, :x)
    :dbg.tpl(:ssh_connection_handler, :start_rekeying, 2, :x)
    :dbg.tp(:ssh_connection_handler, :renegotiate, 1, :x)
  end

  def ssh_dbg_on(:terminate) do
    :dbg.tp(:ssh_connection_handler, :terminate, 3, :x)
  end

  def ssh_dbg_on(:disconnect) do
    :dbg.tpl(:ssh_connection_handler, :send_disconnect, 7, :x)
  end

  def ssh_dbg_off(:disconnect) do
    :dbg.ctpl(:ssh_connection_handler, :send_disconnect, 7)
  end

  def ssh_dbg_off(:terminate) do
    :dbg.ctpg(:ssh_connection_handler, :terminate, 3)
  end

  def ssh_dbg_off(:renegotiation) do
    :dbg.ctpl(:ssh_connection_handler, :init_renegotiate_timers, 3)
    :dbg.ctpl(:ssh_connection_handler, :pause_renegotiate_timers, 3)
    :dbg.ctpl(:ssh_connection_handler, :check_data_rekeying_dbg, 2)
    :dbg.ctpl(:ssh_connection_handler, :start_rekeying, 2)
    :dbg.ctpg(:ssh_connection_handler, :renegotiate, 1)
  end

  def ssh_dbg_off(:connection_events) do
    :dbg.ctpg(:ssh_connection_handler, :handle_event, 4)
  end

  def ssh_dbg_off(:connections) do
    :dbg.ctpg(:ssh_connection_handler, :init_connection_handler, 3)
    ssh_dbg_off(:terminate)
  end

  def ssh_dbg_format(
        :connections,
        {:call, {:ssh_connection_handler, :init_connection_handler, [role, sock, opts]}}
      ) do
    defaultOpts = :ssh_options.handle_options(role, [])
    excludedKeys = [:internal_options, :user_options]

    nonDefaultOpts =
      :maps.filter(
        fn k, v ->
          case :lists.member(k, excludedKeys) do
            true ->
              false

            false ->
              v !==
                try do
                  :maps.get(k, defaultOpts)
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end
          end
        end,
        opts
      )

    {:ok, {iPp, portp}} = :inet.peername(sock)
    {:ok, {iPs, ports}} = :inet.sockname(sock)

    [
      :io_lib.format('Starting ~p connection:\n', [role]),
      :io_lib.format(
        'Socket = ~p, Peer = ~s:~p, Local = ~s:~p,~nNon-default options:~n~p',
        [sock, :inet.ntoa(iPp), portp, :inet.ntoa(iPs), ports, nonDefaultOpts]
      )
    ]
  end

  def ssh_dbg_format(:connections, f) do
    ssh_dbg_format(:terminate, f)
  end

  def ssh_dbg_format(
        :connection_events,
        {:call, {:ssh_connection_handler, :handle_event, [eventType, eventContent, state, _Data]}}
      ) do
    [
      'Connection event\n',
      :io_lib.format('EventType: ~p~nEventContent: ~p~nState: ~p~n', [
        eventType,
        eventContent,
        state
      ])
    ]
  end

  def ssh_dbg_format(
        :connection_events,
        {:return_from, {:ssh_connection_handler, :handle_event, 4}, ret}
      ) do
    ['Connection event result\n', :io_lib.format('~p~n', [:ssh_dbg.reduce_state(ret, r_data())])]
  end

  def ssh_dbg_format(
        :renegotiation,
        {:call, {:ssh_connection_handler, :init_renegotiate_timers, [oldState, newState, d]}}
      ) do
    [
      'Renegotiation: start timer (init_renegotiate_timers)\n',
      :io_lib.format(
        'State: ~p  -->  ~p~nrekey_limit: ~p ({ms,bytes})~ncheck_data_size: ~p (ms)~n',
        [
          oldState,
          newState,
          :ssh_options.get_value(
            :user_options,
            :rekey_limit,
            r_ssh(r_data(d, :ssh_params), :opts),
            :ssh_connection_handler,
            2593
          ),
          60000
        ]
      )
    ]
  end

  def ssh_dbg_format(
        :renegotiation,
        {:return_from, {:ssh_connection_handler, :init_renegotiate_timers, 3}, _Ret}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :renegotiation,
        {:call, {:ssh_connection_handler, :renegotiate, [connectionHandler]}}
      ) do
    [
      'Renegotiation: renegotiation forced\n',
      :io_lib.format(
        '~p:renegotiate(~p) called~n',
        [:ssh_connection_handler, connectionHandler]
      )
    ]
  end

  def ssh_dbg_format(
        :renegotiation,
        {:return_from, {:ssh_connection_handler, :renegotiate, 1}, _Ret}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :renegotiation,
        {:call, {:ssh_connection_handler, :pause_renegotiate_timers, [oldState, newState, _D]}}
      ) do
    [
      'Renegotiation: pause timers\n',
      :io_lib.format('State: ~p  -->  ~p~n', [oldState, newState])
    ]
  end

  def ssh_dbg_format(
        :renegotiation,
        {:return_from, {:ssh_connection_handler, :pause_renegotiate_timers, 3}, _Ret}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :renegotiation,
        {:call, {:ssh_connection_handler, :start_rekeying, [_Role, _D]}}
      ) do
    ['Renegotiation: start rekeying\n']
  end

  def ssh_dbg_format(
        :renegotiation,
        {:return_from, {:ssh_connection_handler, :start_rekeying, 2}, _Ret}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :renegotiation,
        {:call, {:ssh_connection_handler, :check_data_rekeying_dbg, [sentSinceRekey, maxSent]}}
      ) do
    [
      'Renegotiation: check size of data sent\n',
      :io_lib.format(
        'TotalSentSinceRekey: ~p~nMaxBeforeRekey: ~p~nStartRekey: ~p~n',
        [sentSinceRekey, maxSent, sentSinceRekey >= maxSent]
      )
    ]
  end

  def ssh_dbg_format(
        :renegotiation,
        {:return_from, {:ssh_connection_handler, :check_data_rekeying_dbg, 2}, _Ret}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :terminate,
        {:call, {:ssh_connection_handler, :terminate, [reason, stateName, d]}}
      ) do
    extraInfo =
      try do
        {conn_info(:peer, d), conn_info(:user, d), conn_info(:sockname, d)}
      catch
        _, _ ->
          ''
      else
        {{_, {iPp, portp}}, usr, {iPs, ports}}
        when is_tuple(iPp) and is_tuple(iPs) and
               is_integer(portp) and is_integer(ports) ->
          :io_lib.format(
            'Peer=~s:~p, Local=~s:~p, User=~p',
            [:inet.ntoa(iPp), portp, :inet.ntoa(iPs), ports, usr]
          )

        {peer, usr, sockname} ->
          :io_lib.format('Peer=~p, Local=~p, User=~p', [peer, sockname, usr])
      end

    cond do
      reason == :normal or reason == :shutdown or
          :erlang.element(1, reason) == :shutdown ->
        [
          'Connection Terminating:\n',
          :io_lib.format('Reason: ~p, StateName: ~p~n~s', [reason, stateName, extraInfo])
        ]

      true ->
        [
          'Connection Terminating:\n',
          :io_lib.format(
            'Reason: ~p, StateName: ~p~n~s~nStateData = ~p',
            [reason, stateName, extraInfo, state_data2proplist(d)]
          )
        ]
    end
  end

  def ssh_dbg_format(
        :renegotiation,
        {:return_from, {:ssh_connection_handler, :terminate, 3}, _Ret}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :disconnect,
        {:call,
         {:ssh_connection_handler, :send_disconnect,
          [code, reason, detailedText, module, line, stateName, _D]}}
      ) do
    [
      'Disconnecting:\n',
      :io_lib.format(
        ' Module = ~p, Line = ~p, StateName = ~p,~n Code = ~p, Reason = ~p,~n DetailedText =~n ~p',
        [module, line, stateName, code, reason, :lists.flatten(detailedText)]
      )
    ]
  end

  def ssh_dbg_format(
        :renegotiation,
        {:return_from, {:ssh_connection_handler, :send_disconnect, 7}, _Ret}
      ) do
    :skip
  end
end
