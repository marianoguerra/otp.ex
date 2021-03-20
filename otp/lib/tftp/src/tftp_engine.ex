defmodule :m_tftp_engine do
  use Bitwise
  require Record

  Record.defrecord(:r_tftp_msg_req, :tftp_msg_req,
    access: :undefined,
    filename: :undefined,
    mode: :undefined,
    options: :undefined,
    local_filename: :undefined
  )

  Record.defrecord(:r_tftp_msg_data, :tftp_msg_data,
    block_no: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_tftp_msg_ack, :tftp_msg_ack, block_no: :undefined)

  Record.defrecord(:r_tftp_msg_error, :tftp_msg_error,
    code: :undefined,
    text: :undefined,
    details: :undefined
  )

  Record.defrecord(:r_tftp_msg_oack, :tftp_msg_oack, options: :undefined)

  Record.defrecord(:r_config, :config,
    parent_pid: self(),
    udp_socket: :undefined,
    udp_options: [:binary, {:reuseaddr, true}, {:active, :once}],
    udp_host: 'localhost',
    udp_port: 69,
    port_policy: :random,
    use_tsize: false,
    max_tsize: :infinity,
    max_conn: :infinity,
    rejected: [],
    polite_ack: false,
    debug_level: :none,
    timeout: :undefined,
    user_options: [],
    callbacks: [],
    logger: :tftp_logger,
    max_retries: 5
  )

  Record.defrecord(:r_callback, :callback,
    regexp: :undefined,
    internal: :undefined,
    module: :undefined,
    state: :undefined,
    block_no: :undefined,
    count: :undefined
  )

  Record.defrecord(:r_daemon_state, :daemon_state,
    config: :undefined,
    n_servers: :undefined,
    server_tab: :undefined,
    file_tab: :undefined
  )

  Record.defrecord(:r_server_info, :server_info,
    pid: :undefined,
    req: :undefined,
    peer: :undefined
  )

  Record.defrecord(:r_file_info, :file_info,
    peer_req: :undefined,
    pid: :undefined
  )

  Record.defrecord(:r_sys_misc, :sys_misc,
    module: :undefined,
    function: :undefined,
    arguments: :undefined
  )

  Record.defrecord(:r_error, :error,
    where: :undefined,
    code: :undefined,
    text: :undefined,
    filename: :undefined
  )

  Record.defrecord(:r_prepared, :prepared,
    status: :undefined,
    result: :undefined,
    block_no: :undefined,
    next_data: :undefined,
    prev_data: :undefined
  )

  Record.defrecord(:r_transfer_res, :transfer_res,
    status: :undefined,
    decoded_msg: :undefined,
    prepared: :undefined
  )

  def info(:daemons) do
    daemons = :supervisor.which_children(:tftp_sup)

    for {_, pid, _, _} <- daemons do
      {pid, info(pid)}
    end
  end

  def info(:servers) do
    for {_, {:ok, deamonInfo}} <- info(:daemons),
        {:server, pid} <- deamonInfo do
      {pid, info(pid)}
    end
  end

  def info(toPid) when is_pid(toPid) do
    call(:info, toPid, :timer.seconds(10))
  end

  def change_config(:daemons, options) do
    daemons = :supervisor.which_children(:tftp_sup)

    for {_, pid, _, _} <- daemons do
      {pid, change_config(pid, options)}
    end
  end

  def change_config(:servers, options) do
    for {_, {:ok, deamonInfo}} <- info(:daemons),
        {:server, pid} <- deamonInfo do
      {pid, change_config(pid, options)}
    end
  end

  def change_config(toPid, options) when is_pid(toPid) do
    badKeys = [:host, :port, :udp]

    badOptions =
      for {key, val} <- options, badKey <- badKeys, key === badKey do
        {key, val}
      end

    case badOptions do
      [] ->
        call({:change_config, options}, toPid, :timer.seconds(10))

      [{key, val} | _] ->
        {:error, {:badarg, {key, val}}}
    end
  end

  defp call(req, toPid, timeout) when is_pid(toPid) do
    type = :process
    ref = :erlang.monitor(type, toPid)
    send(toPid, {req, ref, self()})

    receive do
      {reply, ^ref, fromPid} when fromPid === toPid ->
        :erlang.demonitor(ref, [:flush])
        reply

      {:DOWN, ^ref, ^type, fromPid, _Reason}
      when fromPid === toPid ->
        {:error, :timeout}
    after
      timeout ->
        {:error, :timeout}
    end
  end

  defp reply(reply, ref, toPid) do
    send(toPid, {reply, ref, self()})
  end

  def daemon_start(options) when is_list(options) do
    config = :tftp_lib.parse_config(options)
    :proc_lib.start_link(:tftp_engine, :daemon_init, [config], :infinity)
  end

  def daemon_init(config)
      when elem(config, 0) === :config and
             is_pid(r_config(config, :parent_pid)) do
    :erlang.process_flag(:trap_exit, true)
    {port, udpOptions} = prepare_daemon_udp(config)

    case (try do
            :gen_udp.open(port, udpOptions)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, socket} ->
        {:ok, actualPort} = :inet.port(socket)
        :proc_lib.init_ack({:ok, self()})

        config2 =
          r_config(config,
            udp_socket: socket,
            udp_port: actualPort
          )

        print_debug_info(config2, :daemon, :open, r_tftp_msg_req(filename: ''))

        serverTab =
          :ets.new(
            :tftp_daemon_servers,
            [{:keypos, 2}]
          )

        fileTab = :ets.new(:tftp_daemon_files, [{:keypos, 2}])

        state =
          r_daemon_state(config: config2, n_servers: 0, server_tab: serverTab, file_tab: fileTab)

        daemon_loop(state)

      {:error, reason} ->
        text =
          :lists.flatten(
            :io_lib.format(
              'UDP open ~p -> ~p',
              [udpOptions, reason]
            )
          )

        print_debug_info(
          config,
          :daemon,
          :open,
          r_error(where: :open, code: :undef, text: text, filename: '')
        )

        exit({:gen_udp_open, udpOptions, reason})

      reason ->
        text =
          :lists.flatten(
            :io_lib.format(
              'UDP open ~p -> ~p',
              [udpOptions, reason]
            )
          )

        print_debug_info(
          config,
          :daemon,
          :open,
          r_error(where: :open, code: :undef, text: text, filename: '')
        )

        exit({:gen_udp_open, udpOptions, reason})
    end
  end

  defp prepare_daemon_udp(
         r_config(
           udp_port: port,
           udp_options: udpOptions
         ) = config
       ) do
    case :lists.keymember(:fd, 1, udpOptions) do
      true ->
        {port, udpOptions}

      false ->
        initArg = :erlang.list_to_atom('tftpd_' ++ :erlang.integer_to_list(port))

        case :init.get_argument(initArg) do
          {:ok, [[fdStr]] = badarg} when is_list(fdStr) ->
            case (try do
                    :erlang.list_to_integer(fdStr)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              fd when is_integer(fd) ->
                {0, [{:fd, fd} | :lists.keydelete(:ip, 1, udpOptions)]}

              {:EXIT, _} ->
                text =
                  :lists.flatten(
                    :io_lib.format(
                      'Illegal prebound fd ~p: ~p',
                      [initArg, badarg]
                    )
                  )

                print_debug_info(
                  config,
                  :daemon,
                  :open,
                  r_error(where: :open, code: :undef, text: text, filename: '')
                )

                exit({:badarg, {:prebound_fd, initArg, badarg}})
            end

          {:ok, badarg} ->
            text =
              :lists.flatten(
                :io_lib.format(
                  'Illegal prebound fd ~p: ~p',
                  [initArg, badarg]
                )
              )

            print_debug_info(
              config,
              :daemon,
              :open,
              r_error(where: :open, code: :undef, text: text, filename: '')
            )

            exit({:badarg, {:prebound_fd, initArg, badarg}})

          :error ->
            {port, udpOptions}
        end
    end
  end

  def daemon_loop(daemonConfig, n, servers)
      when is_list(servers) do
    serverTab =
      :ets.new(
        :tftp_daemon_servers,
        [{:keypos, 2}]
      )

    fileTab = :ets.new(:tftp_daemon_files, [{:keypos, 2}])

    state =
      r_daemon_state(config: daemonConfig, n_servers: n, server_tab: serverTab, file_tab: fileTab)

    req = r_tftp_msg_req(filename: :dummy)

    for pid <- servers do
      :ets.insert(
        serverTab,
        r_server_info(pid: pid, req: req, peer: :dummy)
      )
    end

    daemon_loop(state)
  end

  def daemon_loop(
        r_daemon_state(
          config: daemonConfig,
          n_servers: n,
          server_tab: serverTab,
          file_tab: fileTab
        ) = state
      )
      when elem(daemonConfig, 0) === :config do
    receive do
      {:info, ref, fromPid} when is_pid(fromPid) ->
        fun = fn r_server_info(pid: pid), acc ->
          [{:server, pid} | acc]
        end

        serverInfo = :ets.foldl(fun, [], serverTab)

        info =
          internal_info(
            daemonConfig,
            :daemon
          ) ++ [{:n_conn, n}] ++ serverInfo

        _ = reply({:ok, info}, ref, fromPid)
        :tftp_engine.daemon_loop(state)

      {{:change_config, options}, ref, fromPid}
      when is_pid(fromPid) ->
        case (try do
                :tftp_lib.parse_config(options, daemonConfig)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, reason} ->
            _ = reply({:error, reason}, ref, fromPid)
            :tftp_engine.daemon_loop(state)

          daemonConfig2 when elem(daemonConfig2, 0) === :config ->
            _ = reply(:ok, ref, fromPid)
            :tftp_engine.daemon_loop(r_daemon_state(state, config: daemonConfig2))
        end

      {:udp, socket, remoteHost, remotePort, bin}
      when is_binary(bin) ->
        _ = :inet.setopts(socket, [{:active, :once}])

        serverConfig =
          r_config(daemonConfig,
            parent_pid: self(),
            udp_host: remoteHost,
            udp_port: remotePort
          )

        msg =
          try do
            :tftp_lib.decode_msg(bin)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        print_debug_info(serverConfig, :daemon, :recv, msg)

        case msg do
          req
          when elem(req, 0) === :tftp_msg_req and
                 n <= r_config(daemonConfig, :max_conn) ->
            peer = peer_info(serverConfig)
            peerReq = {peer, req}
            peerInfo = :lists.flatten(:io_lib.format('~p', [peer]))

            case :ets.lookup(fileTab, peerReq) do
              [] ->
                args = [serverConfig, req]
                pid = :proc_lib.spawn_link(:tftp_engine, :server_init, args)

                :ets.insert(
                  serverTab,
                  r_server_info(pid: pid, req: req, peer: peer)
                )

                :ets.insert(fileTab, r_file_info(peer_req: peerReq, pid: pid))
                :tftp_engine.daemon_loop(r_daemon_state(state, n_servers: n + 1))

              [r_file_info(pid: pid)] ->
                warning_msg(daemonConfig, '~p Reuse connection for ~s\n\t~p\n', [
                  pid,
                  peerInfo,
                  r_tftp_msg_req(req, :filename)
                ])

                :tftp_engine.daemon_loop(state)
            end

          req when elem(req, 0) === :tftp_msg_req ->
            reply = r_tftp_msg_error(code: :enospc, text: 'Too many connections')
            peer = peer_info(serverConfig)
            peerInfo = :lists.flatten(:io_lib.format('~p', [peer]))

            warning_msg(
              daemonConfig,
              'Daemon has too many connections (~p).\n\tRejecting request from ~s\n',
              [n, peerInfo]
            )

            send_msg(serverConfig, :daemon, reply)
            :tftp_engine.daemon_loop(state)

          {:EXIT, reply} when elem(reply, 0) === :tftp_msg_error ->
            send_msg(serverConfig, :daemon, reply)
            :tftp_engine.daemon_loop(state)

          req ->
            reply = r_tftp_msg_error(code: :badop, text: 'Illegal TFTP operation')

            warning_msg(daemonConfig, 'Daemon received: ~p.\n\tfrom ~p:~p', [
              req,
              remoteHost,
              remotePort
            ])

            send_msg(serverConfig, :daemon, reply)
            :tftp_engine.daemon_loop(state)
        end

      {:system, from, msg} ->
        misc = r_sys_misc(module: :tftp_engine, function: :daemon_loop, arguments: [state])

        :sys.handle_system_msg(
          msg,
          from,
          r_config(daemonConfig, :parent_pid),
          :tftp_engine,
          [],
          misc
        )

      {:EXIT, pid, reason}
      when r_config(daemonConfig, :parent_pid) === pid ->
        close_port(daemonConfig, :daemon, r_tftp_msg_req(filename: ''))
        exit(reason)

      {:EXIT, pid, _Reason} = info ->
        case :ets.lookup(serverTab, pid) do
          [] ->
            warning_msg(daemonConfig, 'Daemon received: ~p', [info])
            :tftp_engine.daemon_loop(state)

          [r_server_info(req: req, peer: peer)] ->
            peerReq = {peer, req}
            :ets.delete(fileTab, peerReq)
            :ets.delete(serverTab, pid)
            :tftp_engine.daemon_loop(r_daemon_state(state, n_servers: n - 1))
        end

      info ->
        warning_msg(daemonConfig, 'Daemon received: ~p', [info])
        :tftp_engine.daemon_loop(state)
    end
  end

  def daemon_loop(r_daemon_state(config: config) = state) do
    config2 = upgrade_config(config)
    daemon_loop(r_daemon_state(state, config: config2))
  end

  defp upgrade_config(
         {:config, parentPid, udpSocket, udpOptions, udpHost, udpPort, portPolicy, useTsize,
          maxTsize, maxConn, rejected, politeAck, debugLevel, timeout, userOptions, callbacks}
       ) do
    callbacks2 = :tftp_lib.add_default_callbacks(callbacks)
    logger = :tftp_logger
    maxRetries = 5

    {:config, parentPid, udpSocket, udpOptions, udpHost, udpPort, portPolicy, useTsize, maxTsize,
     maxConn, rejected, politeAck, debugLevel, timeout, userOptions, callbacks2, logger,
     maxRetries}
  end

  def server_init(config, req)
      when elem(config, 0) === :config and
             is_pid(r_config(config, :parent_pid)) and
             elem(req, 0) === :tftp_msg_req do
    :erlang.process_flag(:trap_exit, true)
    suggestedOptions = r_tftp_msg_req(req, :options)
    udpOptions = r_config(config, :udp_options)
    udpOptions2 = :lists.keydelete(:fd, 1, udpOptions)
    config1 = r_config(config, udp_options: udpOptions2)

    config2 =
      :tftp_lib.parse_config(
        suggestedOptions,
        config1
      )

    suggestedOptions2 = r_config(config2, :user_options)
    req2 = r_tftp_msg_req(req, options: suggestedOptions2)

    case open_free_port(config2, :server, req2) do
      {:ok, config3} ->
        filename = r_tftp_msg_req(req, :filename)

        case match_callback(
               filename,
               r_config(config3, :callbacks)
             ) do
          {:ok, callback} ->
            print_debug_info(config3, :server, :match, callback)

            case pre_verify_options(config3, req2) do
              :ok ->
                case callback({:open, :server_open}, config3, callback, req2) do
                  {callback2, {:ok, acceptedOptions}} ->
                    {localAccess, _} = local_file_access(req2)
                    optText = 'Internal error. Not allowed to add new options.'

                    case post_verify_options(config3, req2, acceptedOptions, optText) do
                      {:ok, config4, req3} when acceptedOptions !== [] ->
                        reply = r_tftp_msg_oack(options: acceptedOptions)

                        blockNo =
                          case localAccess do
                            :read ->
                              0

                            :write ->
                              1
                          end

                        {config5, callback3, transferRes} =
                          transfer(
                            config4,
                            callback2,
                            req3,
                            reply,
                            localAccess,
                            blockNo,
                            r_prepared()
                          )

                        common_loop(config5, callback3, req3, transferRes, localAccess, blockNo)

                      {:ok, config4, req3} when localAccess === :write ->
                        blockNo = 0
                        common_ack(config4, callback2, req3, localAccess, blockNo, r_prepared())

                      {:ok, config4, req3} when localAccess === :read ->
                        blockNo = 0

                        common_read(
                          config4,
                          callback2,
                          req3,
                          localAccess,
                          blockNo,
                          blockNo,
                          r_prepared()
                        )

                      {:error, {code, text}} ->
                        {:undefined, error} =
                          callback({:abort, {code, text}}, config3, callback2, req2)

                        send_msg(config3, req, error)

                        terminate(
                          config3,
                          req2,
                          r_error(
                            where: :post_verify_options,
                            code: code,
                            text: text,
                            filename: r_tftp_msg_req(req2, :filename)
                          )
                        )
                    end

                  {:undefined, r_tftp_msg_error(code: code, text: text) = error} ->
                    send_msg(config3, req, error)

                    terminate(
                      config3,
                      req,
                      r_error(
                        where: :server_open,
                        code: code,
                        text: text,
                        filename: r_tftp_msg_req(req2, :filename)
                      )
                    )
                end

              {:error, {code, text}} ->
                {:undefined, error} = callback({:abort, {code, text}}, config2, callback, req2)
                send_msg(config2, req, error)

                terminate(
                  config2,
                  req2,
                  r_error(
                    where: :pre_verify_options,
                    code: code,
                    text: text,
                    filename: r_tftp_msg_req(req2, :filename)
                  )
                )
            end

          {:error, r_tftp_msg_error(code: code, text: text) = error} ->
            send_msg(config3, req, error)

            terminate(
              config3,
              req,
              r_error(
                where: :match_callback,
                code: code,
                text: text,
                filename: r_tftp_msg_req(req2, :filename)
              )
            )
        end

      r_error() = error ->
        terminate(config2, req, error)
    end
  end

  def server_init(config, req)
      when elem(req, 0) === :tftp_msg_req do
    config2 = upgrade_config(config)
    server_init(config2, req)
  end

  def client_start(access, remoteFilename, localFilename, options) do
    config = :tftp_lib.parse_config(options)

    config2 =
      r_config(config,
        parent_pid: self(),
        udp_socket: :undefined
      )

    req =
      r_tftp_msg_req(
        access: access,
        filename: remoteFilename,
        mode: lookup_mode(r_config(config2, :user_options)),
        options: r_config(config2, :user_options),
        local_filename: localFilename
      )

    args = [config2, req]

    case :proc_lib.start_link(:tftp_engine, :client_init, args, :infinity) do
      {:ok, lastCallbackState} ->
        {:ok, lastCallbackState}

      {:error, error} ->
        {:error, error}
    end
  end

  def client_init(config, req)
      when elem(config, 0) === :config and
             is_pid(r_config(config, :parent_pid)) and
             elem(req, 0) === :tftp_msg_req do
    :erlang.process_flag(:trap_exit, true)

    case open_free_port(config, :client, req) do
      {:ok, config2} ->
        req2 =
          case r_config(config2, :use_tsize) do
            true ->
              suggestedOptions = r_tftp_msg_req(req, :options)
              suggestedOptions2 = :tftp_lib.replace_val('tsize', '0', suggestedOptions)
              r_tftp_msg_req(req, options: suggestedOptions2)

            false ->
              req
          end

        localFilename = r_tftp_msg_req(req2, :local_filename)

        case match_callback(
               localFilename,
               r_config(config2, :callbacks)
             ) do
          {:ok, callback} ->
            print_debug_info(config2, :client, :match, callback)
            client_prepare(config2, callback, req2)

          {:error, r_tftp_msg_error(code: code, text: text)} ->
            terminate(
              config,
              req,
              r_error(
                where: :match,
                code: code,
                text: text,
                filename: r_tftp_msg_req(req, :filename)
              )
            )
        end

      r_error() = error ->
        terminate(config, req, error)
    end
  end

  defp client_prepare(config, callback, req)
       when elem(req, 0) === :tftp_msg_req do
    case pre_verify_options(config, req) do
      :ok ->
        case callback({:open, :client_prepare}, config, callback, req) do
          {callback2, {:ok, acceptedOptions}} ->
            optText = 'Internal error. Not allowed to add new options.'

            case post_verify_options(config, req, acceptedOptions, optText) do
              {:ok, config2, req2} ->
                {localAccess, _} = local_file_access(req2)
                blockNo = 0

                {config3, callback3, transferRes} =
                  transfer(config2, callback2, req2, req2, localAccess, blockNo, r_prepared())

                client_open(config3, callback3, req2, blockNo, transferRes)

              {:error, {code, text}} ->
                _ = callback({:abort, {code, text}}, config, callback2, req)

                terminate(
                  config,
                  req,
                  r_error(
                    where: :post_verify_options,
                    code: code,
                    text: text,
                    filename: r_tftp_msg_req(req, :filename)
                  )
                )
            end

          {:undefined, r_tftp_msg_error(code: code, text: text)} ->
            terminate(
              config,
              req,
              r_error(
                where: :client_prepare,
                code: code,
                text: text,
                filename: r_tftp_msg_req(req, :filename)
              )
            )
        end

      {:error, {code, text}} ->
        _ = callback({:abort, {code, text}}, config, callback, req)

        terminate(
          config,
          req,
          r_error(
            where: :pre_verify_options,
            code: code,
            text: text,
            filename: r_tftp_msg_req(req, :filename)
          )
        )
    end
  end

  defp client_open(
         config,
         callback,
         req,
         blockNo,
         r_transfer_res(status: status, decoded_msg: decodedMsg, prepared: prepared)
       ) do
    {localAccess, _} = local_file_access(req)

    case status do
      :ok when elem(prepared, 0) === :prepared ->
        case decodedMsg do
          msg when elem(msg, 0) === :tftp_msg_oack ->
            serverOptions = r_tftp_msg_oack(msg, :options)
            optText = 'Protocol violation. Server is not allowed new options'

            case post_verify_options(config, req, serverOptions, optText) do
              {:ok, config2, req2} ->
                {config3, callback2, req3} = do_client_open(config2, callback, req2)

                case localAccess do
                  :read ->
                    common_read(config3, callback2, req3, localAccess, blockNo, blockNo, prepared)

                  :write ->
                    common_ack(config3, callback2, req3, localAccess, blockNo, prepared)
                end

              {:error, {code, text}} ->
                {:undefined, error} = callback({:abort, {code, text}}, config, callback, req)
                send_msg(config, req, error)

                terminate(
                  config,
                  req,
                  r_error(
                    where: :verify_server_options,
                    code: code,
                    text: text,
                    filename: r_tftp_msg_req(req, :filename)
                  )
                )
            end

          r_tftp_msg_ack(block_no: actualBlockNo) when localAccess === :read ->
            req2 = r_tftp_msg_req(req, options: [])
            {config2, callback2, ^req2} = do_client_open(config, callback, req2)
            expectedBlockNo = 0

            common_read(
              config2,
              callback2,
              req2,
              localAccess,
              expectedBlockNo,
              actualBlockNo,
              prepared
            )

          r_tftp_msg_data(block_no: actualBlockNo, data: data)
          when localAccess === :write ->
            req2 = r_tftp_msg_req(req, options: [])
            {config2, callback2, ^req2} = do_client_open(config, callback, req2)
            expectedBlockNo = 1

            common_write(
              config2,
              callback2,
              req2,
              localAccess,
              expectedBlockNo,
              actualBlockNo,
              data,
              prepared
            )

          r_tftp_msg_error(code: code, text: text) ->
            _ = callback({:abort, {code, text}}, config, callback, req)

            terminate(
              config,
              req,
              r_error(
                where: :client_open,
                code: code,
                text: text,
                filename: r_tftp_msg_req(req, :filename)
              )
            )

          {:EXIT, r_tftp_msg_error(code: code, text: text)} ->
            _ = callback({:abort, {code, text}}, config, callback, req)

            terminate(
              config,
              req,
              r_error(
                where: :client_open,
                code: code,
                text: text,
                filename: r_tftp_msg_req(req, :filename)
              )
            )

          msg when is_tuple(msg) ->
            code = :badop
            text = 'Illegal TFTP operation'
            {:undefined, error} = callback({:abort, {code, text}}, config, callback, req)
            send_msg(config, req, error)

            text2 =
              :lists.flatten([
                text,
                '. ',
                :io_lib.format(
                  '~p',
                  [
                    :erlang.element(
                      1,
                      msg
                    )
                  ]
                )
              ])

            terminate(
              config,
              req,
              r_error(
                where: :client_open,
                code: code,
                text: text2,
                filename: r_tftp_msg_req(req, :filename)
              )
            )
        end

      :error when elem(prepared, 0) === :tftp_msg_error ->
        r_tftp_msg_error(code: code, text: text) = prepared
        _ = callback({:abort, {code, text}}, config, callback, req)

        terminate(
          config,
          req,
          r_error(
            where: :client_open,
            code: code,
            text: text,
            filename: r_tftp_msg_req(req, :filename)
          )
        )
    end
  end

  defp do_client_open(config, callback, req) do
    case callback({:open, :client_open}, config, callback, req) do
      {callback2, {:ok, finalOptions}} ->
        optText = 'Internal error. Not allowed to change options.'

        case post_verify_options(config, req, finalOptions, optText) do
          {:ok, config2, req2} ->
            {config2, callback2, req2}

          {:error, {code, text}} ->
            {:undefined, error} = callback({:abort, {code, text}}, config, callback2, req)
            send_msg(config, req, error)

            terminate(
              config,
              req,
              r_error(
                where: :post_verify_options,
                code: code,
                text: text,
                filename: r_tftp_msg_req(req, :filename)
              )
            )
        end

      {:undefined, r_tftp_msg_error(code: code, text: text) = error} ->
        send_msg(config, req, error)

        terminate(
          config,
          req,
          r_error(
            where: :client_open,
            code: code,
            text: text,
            filename: r_tftp_msg_req(req, :filename)
          )
        )
    end
  end

  def common_loop(
        config,
        callback,
        req,
        r_transfer_res(status: status, decoded_msg: decodedMsg, prepared: prepared),
        localAccess,
        expectedBlockNo
      )
      when elem(config, 0) === :config do
    case status do
      :ok when elem(prepared, 0) === :prepared ->
        case decodedMsg do
          r_tftp_msg_ack(block_no: actualBlockNo) when localAccess === :read ->
            common_read(
              config,
              callback,
              req,
              localAccess,
              expectedBlockNo,
              actualBlockNo,
              prepared
            )

          r_tftp_msg_data(block_no: actualBlockNo, data: data)
          when localAccess === :write ->
            common_write(
              config,
              callback,
              req,
              localAccess,
              expectedBlockNo,
              actualBlockNo,
              data,
              prepared
            )

          r_tftp_msg_error(code: code, text: text) ->
            _ = callback({:abort, {code, text}}, config, callback, req)

            terminate(
              config,
              req,
              r_error(
                where: :common_loop,
                code: code,
                text: text,
                filename: r_tftp_msg_req(req, :filename)
              )
            )

          {:EXIT, r_tftp_msg_error(code: code, text: text) = error} ->
            _ = callback({:abort, {code, text}}, config, callback, req)
            send_msg(config, req, error)

            terminate(
              config,
              req,
              r_error(
                where: :common_loop,
                code: code,
                text: text,
                filename: r_tftp_msg_req(req, :filename)
              )
            )

          msg when is_tuple(msg) ->
            code = :badop
            text = 'Illegal TFTP operation'
            {:undefined, error} = callback({:abort, {code, text}}, config, callback, req)
            send_msg(config, req, error)

            text2 =
              :lists.flatten([
                text,
                '. ',
                :io_lib.format(
                  '~p',
                  [
                    :erlang.element(
                      1,
                      msg
                    )
                  ]
                )
              ])

            terminate(
              config,
              req,
              r_error(
                where: :common_loop,
                code: code,
                text: text2,
                filename: r_tftp_msg_req(req, :filename)
              )
            )
        end

      :error when elem(prepared, 0) === :tftp_msg_error ->
        r_tftp_msg_error(code: code, text: text) = prepared
        send_msg(config, req, prepared)

        terminate(
          config,
          req,
          r_error(
            where: :transfer,
            code: code,
            text: text,
            filename: r_tftp_msg_req(req, :filename)
          )
        )
    end
  end

  def common_loop(config, callback, req, transferRes, localAccess, expectedBlockNo) do
    config2 = upgrade_config(config)
    common_loop(config2, callback, req, transferRes, localAccess, expectedBlockNo)
  end

  defp common_read(config, _, req, _, _, _, r_prepared(status: :terminate, result: result)) do
    terminate(config, req, {:ok, result})
  end

  defp common_read(config, callback, req, localAccess, expectedBlockNo, actualBlockNo, prepared)
       when actualBlockNo === expectedBlockNo and
              elem(prepared, 0) === :prepared do
    case early_read(config, callback, req, localAccess, actualBlockNo, prepared) do
      {callback2, r_prepared(status: :more, next_data: data) = prepared2}
      when is_binary(data) ->
        prepared3 =
          r_prepared(prepared2,
            prev_data: data,
            next_data: :undefined
          )

        do_common_read(config, callback2, req, localAccess, actualBlockNo, data, prepared3)

      {:undefined, r_prepared(status: :last, next_data: data) = prepared2}
      when is_binary(data) ->
        prepared3 = r_prepared(prepared2, status: :terminate)
        do_common_read(config, :undefined, req, localAccess, actualBlockNo, data, prepared3)

      {:undefined, r_prepared(status: :error, result: error)} ->
        r_tftp_msg_error(code: code, text: text) = error
        send_msg(config, req, error)

        terminate(
          config,
          req,
          r_error(where: :read, code: code, text: text, filename: r_tftp_msg_req(req, :filename))
        )
    end
  end

  defp common_read(config, callback, req, localAccess, expectedBlockNo, actualBlockNo, prepared)
       when actualBlockNo === expectedBlockNo - 1 and
              elem(prepared, 0) === :prepared do
    case prepared do
      r_prepared(status: :more, prev_data: data) when is_binary(data) ->
        do_common_read(config, callback, req, localAccess, actualBlockNo, data, prepared)

      r_prepared(status: :last, prev_data: data) when is_binary(data) ->
        do_common_read(config, callback, req, localAccess, actualBlockNo, data, prepared)

      r_prepared(status: :error, result: error) ->
        r_tftp_msg_error(code: code, text: text) = error
        send_msg(config, req, error)

        terminate(
          config,
          req,
          r_error(where: :read, code: code, text: text, filename: r_tftp_msg_req(req, :filename))
        )
    end
  end

  defp common_read(config, callback, req, localAccess, expectedBlockNo, actualBlockNo, prepared)
       when actualBlockNo <= expectedBlockNo and
              elem(prepared, 0) === :prepared do
    case prepared do
      r_prepared(status: :more, prev_data: data) when is_binary(data) ->
        reply = r_tftp_msg_data(block_no: expectedBlockNo, data: data)

        {config2, callback2, transferRes} =
          wait_for_msg_and_handle_timeout(
            config,
            callback,
            req,
            reply,
            localAccess,
            expectedBlockNo,
            prepared
          )

        :tftp_engine.common_loop(
          config2,
          callback2,
          req,
          transferRes,
          localAccess,
          expectedBlockNo
        )

      r_prepared(status: :last, prev_data: data) when is_binary(data) ->
        reply = r_tftp_msg_data(block_no: expectedBlockNo, data: data)

        {config2, callback2, transferRes} =
          wait_for_msg_and_handle_timeout(
            config,
            callback,
            req,
            reply,
            localAccess,
            expectedBlockNo,
            prepared
          )

        :tftp_engine.common_loop(
          config2,
          callback2,
          req,
          transferRes,
          localAccess,
          expectedBlockNo
        )

      r_prepared(status: :error, result: error) ->
        r_tftp_msg_error(code: code, text: text) = error
        send_msg(config, req, error)

        terminate(
          config,
          req,
          r_error(where: :read, code: code, text: text, filename: r_tftp_msg_req(req, :filename))
        )
    end
  end

  defp common_read(config, callback, req, _LocalAccess, expectedBlockNo, actualBlockNo, prepared)
       when elem(prepared, 0) === :prepared do
    code = :badblk

    text =
      'Unknown transfer ID = ' ++
        :erlang.integer_to_list(actualBlockNo) ++
        ' (' ++ :erlang.integer_to_list(expectedBlockNo) ++ ')'

    {:undefined, error} = callback({:abort, {code, text}}, config, callback, req)
    send_msg(config, req, error)

    terminate(
      config,
      req,
      r_error(where: :read, code: code, text: text, filename: r_tftp_msg_req(req, :filename))
    )
  end

  defp do_common_read(config, callback, req, localAccess, blockNo, data, prepared)
       when is_binary(data) and
              elem(prepared, 0) === :prepared do
    nextBlockNo = rem(blockNo + 1, 65536)
    reply = r_tftp_msg_data(block_no: nextBlockNo, data: data)

    {config2, callback2, transferRes} =
      transfer(config, callback, req, reply, localAccess, nextBlockNo, prepared)

    :tftp_engine.common_loop(config2, callback2, req, transferRes, localAccess, nextBlockNo)
  end

  defp common_write(config, _, req, _, _, _, _, r_prepared(status: :terminate, result: result)) do
    terminate(config, req, {:ok, result})
  end

  defp common_write(
         config,
         callback,
         req,
         localAccess,
         expectedBlockNo,
         actualBlockNo,
         data,
         prepared
       )
       when actualBlockNo === expectedBlockNo and
              is_binary(data) and elem(prepared, 0) === :prepared do
    case callback({:write, data}, config, callback, req) do
      {callback2, r_prepared(status: :more) = prepared2} ->
        common_ack(config, callback2, req, localAccess, actualBlockNo, prepared2)

      {:undefined, r_prepared(status: :last, result: result) = prepared2} ->
        config2 = pre_terminate(config, req, {:ok, result})
        prepared3 = r_prepared(prepared2, status: :terminate)
        common_ack(config2, :undefined, req, localAccess, actualBlockNo, prepared3)

      {:undefined, r_prepared(status: :error, result: error)} ->
        r_tftp_msg_error(code: code, text: text) = error
        send_msg(config, req, error)

        terminate(
          config,
          req,
          r_error(where: :write, code: code, text: text, filename: r_tftp_msg_req(req, :filename))
        )
    end
  end

  defp common_write(
         config,
         callback,
         req,
         localAccess,
         expectedBlockNo,
         actualBlockNo,
         data,
         prepared
       )
       when actualBlockNo === expectedBlockNo - 1 and
              is_binary(data) and elem(prepared, 0) === :prepared do
    common_ack(config, callback, req, localAccess, expectedBlockNo - 1, prepared)
  end

  defp common_write(
         config,
         callback,
         req,
         localAccess,
         expectedBlockNo,
         actualBlockNo,
         data,
         prepared
       )
       when actualBlockNo <= expectedBlockNo and
              is_binary(data) and elem(prepared, 0) === :prepared do
    reply = r_tftp_msg_ack(block_no: expectedBlockNo)

    {config2, callback2, transferRes} =
      wait_for_msg_and_handle_timeout(
        config,
        callback,
        req,
        reply,
        localAccess,
        expectedBlockNo,
        prepared
      )

    :tftp_engine.common_loop(config2, callback2, req, transferRes, localAccess, expectedBlockNo)
  end

  defp common_write(config, callback, req, _, expectedBlockNo, actualBlockNo, data, prepared)
       when is_binary(data) and
              elem(prepared, 0) === :prepared do
    code = :badblk

    text =
      'Unknown transfer ID = ' ++
        :erlang.integer_to_list(actualBlockNo) ++
        ' (' ++ :erlang.integer_to_list(expectedBlockNo) ++ ')'

    {:undefined, error} = callback({:abort, {code, text}}, config, callback, req)
    send_msg(config, req, error)

    terminate(
      config,
      req,
      r_error(where: :write, code: code, text: text, filename: r_tftp_msg_req(req, :filename))
    )
  end

  defp common_ack(config, callback, req, localAccess, blockNo, prepared)
       when elem(prepared, 0) === :prepared do
    reply = r_tftp_msg_ack(block_no: blockNo)
    nextBlockNo = rem(blockNo + 1, 65536)

    {config2, callback2, transferRes} =
      transfer(config, callback, req, reply, localAccess, nextBlockNo, prepared)

    :tftp_engine.common_loop(config2, callback2, req, transferRes, localAccess, nextBlockNo)
  end

  defp pre_terminate(config, req, result) do
    cond do
      r_tftp_msg_req(req, :local_filename) !== :undefined and
          r_config(config, :parent_pid) !== :undefined ->
        :proc_lib.init_ack(result)
        :erlang.unlink(r_config(config, :parent_pid))
        r_config(config, parent_pid: :undefined, polite_ack: true)

      true ->
        r_config(config, polite_ack: true)
    end
  end

  defp terminate(config, req, result) do
    result2 =
      case result do
        {:ok, _} ->
          result

        r_error(where: where, code: code, text: text) = error ->
          print_debug_info(
            config,
            req,
            where,
            r_error(error, filename: r_tftp_msg_req(req, :filename))
          )

          {:error, {where, code, text}}
      end

    cond do
      r_config(config, :parent_pid) === :undefined ->
        close_port(config, :client, req)
        exit(:normal)

      r_tftp_msg_req(req, :local_filename) !== :undefined ->
        close_port(config, :client, req)
        :proc_lib.init_ack(result2)
        :erlang.unlink(r_config(config, :parent_pid))
        exit(:normal)

      true ->
        close_port(config, :server, req)
        exit(:shutdown)
    end
  end

  defp close_port(config, who, req)
       when elem(req, 0) === :tftp_msg_req do
    case r_config(config, :udp_socket) do
      :undefined ->
        :ignore

      socket ->
        print_debug_info(config, who, :close, req)
        :gen_udp.close(socket)
    end
  end

  defp open_free_port(config, who, req)
       when elem(config, 0) === :config and
              elem(req, 0) === :tftp_msg_req do
    udpOptions = r_config(config, :udp_options)

    case r_config(config, :port_policy) do
      :random ->
        case (try do
                :gen_udp.open(0, udpOptions)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:ok, socket} ->
            config2 = r_config(config, udp_socket: socket)
            print_debug_info(config2, who, :open, req)
            {:ok, config2}

          {:error, reason} ->
            text =
              :lists.flatten(
                :io_lib.format(
                  'UDP open ~p -> ~p',
                  [[0 | udpOptions], reason]
                )
              )

            r_error(
              where: :open,
              code: :undef,
              text: text,
              filename: r_tftp_msg_req(req, :filename)
            )

          {:EXIT, _} = reason ->
            text =
              :lists.flatten(
                :io_lib.format(
                  'UDP open ~p -> ~p',
                  [[0 | udpOptions], reason]
                )
              )

            r_error(
              where: :open,
              code: :undef,
              text: text,
              filename: r_tftp_msg_req(req, :filename)
            )
        end

      {:range, port, max} when port <= max ->
        case (try do
                :gen_udp.open(port, udpOptions)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:ok, socket} ->
            config2 = r_config(config, udp_socket: socket)
            print_debug_info(config2, who, :open, req)
            {:ok, config2}

          {:error, :eaddrinuse} ->
            portPolicy = {:range, port + 1, max}
            config2 = r_config(config, port_policy: portPolicy)
            open_free_port(config2, who, req)

          {:error, reason} ->
            text =
              :lists.flatten(
                :io_lib.format(
                  'UDP open ~p -> ~p',
                  [[port | udpOptions], reason]
                )
              )

            r_error(
              where: :open,
              code: :undef,
              text: text,
              filename: r_tftp_msg_req(req, :filename)
            )

          {:EXIT, _} = reason ->
            text =
              :lists.flatten(
                :io_lib.format(
                  'UDP open ~p -> ~p',
                  [[port | udpOptions], reason]
                )
              )

            r_error(
              where: :open,
              code: :undef,
              text: text,
              filename: r_tftp_msg_req(req, :filename)
            )
        end

      {:range, port, _Max} ->
        reason = 'Port range exhausted'

        text =
          :lists.flatten(
            :io_lib.format(
              'UDP open ~p -> ~p',
              [[port | udpOptions], reason]
            )
          )

        r_error(where: who, code: :undef, text: text, filename: r_tftp_msg_req(req, :filename))
    end
  end

  defp transfer(config, callback, req, msg, localAccess, nextBlockNo, prepared)
       when elem(prepared, 0) === :prepared do
    ioList = :tftp_lib.encode_msg(msg)
    retries = r_config(config, :max_retries) + 1
    do_transfer(config, callback, req, msg, ioList, localAccess, nextBlockNo, prepared, retries)
  end

  defp do_transfer(
         config,
         callback,
         req,
         msg,
         ioList,
         localAccess,
         nextBlockNo,
         prepared,
         retries
       )
       when elem(prepared, 0) === :prepared and
              is_integer(retries) and retries >= 0 do
    case do_send_msg(config, req, msg, ioList) do
      :ok ->
        {callback2, prepared2} =
          early_read(config, callback, req, localAccess, nextBlockNo, prepared)

        do_wait_for_msg_and_handle_timeout(
          config,
          callback2,
          req,
          msg,
          ioList,
          localAccess,
          nextBlockNo,
          prepared2,
          retries
        )

      {:error, _Reason} when retries > 0 ->
        retries2 = 0

        do_transfer(
          config,
          callback,
          req,
          msg,
          ioList,
          localAccess,
          nextBlockNo,
          prepared,
          retries2
        )

      {:error, reason} ->
        code = :undef
        text = :lists.flatten(:io_lib.format('Transfer failed - giving up -> ~p', [reason]))
        error = r_tftp_msg_error(code: code, text: text)
        {config, callback, r_transfer_res(status: :error, prepared: error)}
    end
  end

  defp wait_for_msg_and_handle_timeout(
         config,
         callback,
         req,
         msg,
         localAccess,
         nextBlockNo,
         prepared
       ) do
    ioList = :tftp_lib.encode_msg(msg)
    retries = r_config(config, :max_retries) + 1

    do_wait_for_msg_and_handle_timeout(
      config,
      callback,
      req,
      msg,
      ioList,
      localAccess,
      nextBlockNo,
      prepared,
      retries
    )
  end

  defp do_wait_for_msg_and_handle_timeout(
         config,
         callback,
         req,
         msg,
         ioList,
         localAccess,
         nextBlockNo,
         prepared,
         retries
       ) do
    code = :undef
    text = 'Transfer timed out.'

    case wait_for_msg(config, callback, req) do
      :timeout when r_config(config, :polite_ack) === true ->
        do_send_msg(config, req, msg, ioList)

        case prepared do
          r_prepared(status: :terminate, result: result) ->
            terminate(config, req, {:ok, result})

          r_prepared() ->
            terminate(
              config,
              req,
              r_error(
                where: :transfer,
                code: code,
                text: text,
                filename: r_tftp_msg_req(req, :filename)
              )
            )
        end

      :timeout when retries > 0 ->
        retries2 = retries - 1

        do_transfer(
          config,
          callback,
          req,
          msg,
          ioList,
          localAccess,
          nextBlockNo,
          prepared,
          retries2
        )

      :timeout ->
        error = r_tftp_msg_error(code: code, text: text)
        {config, callback, r_transfer_res(status: :error, prepared: error)}

      {config2, decodedMsg} ->
        {config2, callback,
         r_transfer_res(status: :ok, decoded_msg: decodedMsg, prepared: prepared)}
    end
  end

  defp send_msg(config, req, msg) do
    case (try do
            :tftp_lib.encode_msg(msg)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        code = :undef
        text = 'Internal error. Encode failed'
        msg2 = r_tftp_msg_error(code: code, text: text, details: reason)
        send_msg(config, req, msg2)

      ioList ->
        do_send_msg(config, req, msg, ioList)
    end
  end

  defp do_send_msg(
         r_config(udp_socket: socket, udp_host: remoteHost, udp_port: remotePort) = config,
         req,
         msg,
         ioList
       ) do
    print_debug_info(config, req, :send, msg)

    try do
      :ok = :gen_udp.send(socket, remoteHost, remotePort, ioList)
    catch
      :error, {:badmatch, {:error, :einval = reason}} ->
        error_msg(config, 'Stacktrace; ~p\n gen_udp:send(~p, ~p, ~p, ~p) -> ~p\n', [
          __STACKTRACE__,
          socket,
          remoteHost,
          remotePort,
          ioList,
          {:error, reason}
        ])

      :error, {:badmatch, {:error, reason}} ->
        {:error, reason}
    end
  end

  def wait_for_msg(config, callback, req) do
    receive do
      {:udp, socket, remoteHost, remotePort, bin}
      when is_binary(bin) and
             r_callback(callback, :block_no) === :undefined ->
        _ = :inet.setopts(socket, [{:active, :once}])

        config2 =
          r_config(config,
            udp_host: remoteHost,
            udp_port: remotePort
          )

        decodedMsg =
          try do
            :tftp_lib.decode_msg(bin)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        print_debug_info(config2, req, :recv, decodedMsg)
        {config2, decodedMsg}

      {:udp, socket, host, port, bin}
      when is_binary(bin) and
             r_config(config, :udp_host) === host and
             r_config(config, :udp_port) === port ->
        _ = :inet.setopts(socket, [{:active, :once}])

        decodedMsg =
          try do
            :tftp_lib.decode_msg(bin)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        print_debug_info(config, req, :recv, decodedMsg)
        {config, decodedMsg}

      {:info, ref, fromPid} when is_pid(fromPid) ->
        type =
          case r_tftp_msg_req(req, :local_filename) !== :undefined do
            true ->
              :client

            false ->
              :server
          end

        info = internal_info(config, type)
        _ = reply({:ok, info}, ref, fromPid)
        wait_for_msg(config, callback, req)

      {{:change_config, options}, ref, fromPid}
      when is_pid(fromPid) ->
        case (try do
                :tftp_lib.parse_config(options, config)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, reason} ->
            _ = reply({:error, reason}, ref, fromPid)
            wait_for_msg(config, callback, req)

          config2 when elem(config2, 0) === :config ->
            _ = reply(:ok, ref, fromPid)
            wait_for_msg(config2, callback, req)
        end

      {:system, from, msg} ->
        misc =
          r_sys_misc(
            module: :tftp_engine,
            function: :wait_for_msg,
            arguments: [config, callback, req]
          )

        :sys.handle_system_msg(msg, from, r_config(config, :parent_pid), :tftp_engine, [], misc)

      {:EXIT, pid, _Reason}
      when r_config(config, :parent_pid) === pid ->
        code = :undef
        text = 'Parent exited.'

        terminate(
          config,
          req,
          r_error(
            where: :wait_for_msg,
            code: code,
            text: text,
            filename: r_tftp_msg_req(req, :filename)
          )
        )

      msg when r_tftp_msg_req(req, :local_filename) !== :undefined ->
        warning_msg(config, 'Client received : ~p', [msg])
        wait_for_msg(config, callback, req)

      msg when r_tftp_msg_req(req, :local_filename) === :undefined ->
        warning_msg(config, 'Server received : ~p', [msg])
        wait_for_msg(config, callback, req)
    after
      r_config(config, :timeout) * 1000 ->
        print_debug_info(config, req, :recv, :timeout)
        :timeout
    end
  end

  defp early_read(
         config,
         callback,
         req,
         localAccess,
         _NextBlockNo,
         r_prepared(status: status, next_data: nextData, prev_data: prevData) = prepared
       ) do
    cond do
      status !== :terminate and localAccess === :read and
        r_callback(callback, :block_no) !== :undefined and
          nextData === :undefined ->
        case callback(:read, config, callback, req) do
          {:undefined, error}
          when elem(error, 0) === :tftp_msg_error ->
            {:undefined, error}

          {callback2, prepared2}
          when elem(prepared2, 0) === :prepared ->
            {callback2, r_prepared(prepared2, prev_data: prevData)}
        end

      true ->
        {callback, prepared}
    end
  end

  def callback(access, config, callback, req) do
    {callback2, result} = do_callback(access, config, callback, req)
    print_debug_info(config, req, :call, {callback2, result})
    {callback2, result}
  end

  defp do_callback(:read = fun, config, callback, req)
       when elem(config, 0) === :config and
              elem(callback, 0) === :callback and
              elem(req, 0) === :tftp_msg_req do
    args = [r_callback(callback, :state)]
    nextBlockNo = r_callback(callback, :block_no) + 1

    case (try do
            safe_apply(r_callback(callback, :module), fun, args)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:more, bin, newState} when is_binary(bin) ->
        count = r_callback(callback, :count) + :erlang.size(bin)
        callback2 = r_callback(callback, state: newState, block_no: nextBlockNo, count: count)

        prepared =
          r_prepared(status: :more, result: :undefined, block_no: nextBlockNo, next_data: bin)

        verify_count(config, callback2, req, prepared)

      {:last, bin, result} when is_binary(bin) ->
        prepared =
          r_prepared(status: :last, result: result, block_no: nextBlockNo, next_data: bin)

        {:undefined, prepared}

      {:error, {code, text}} ->
        error = r_tftp_msg_error(code: code, text: text)
        prepared = r_prepared(status: :error, result: error)
        {:undefined, prepared}

      illegal ->
        code = :undef
        text = 'Internal error. File handler error.'
        callback({:abort, {code, text, illegal}}, config, callback, req)
    end
  end

  defp do_callback({:write = fun, bin}, config, callback, req)
       when elem(config, 0) === :config and
              elem(callback, 0) === :callback and
              elem(req, 0) === :tftp_msg_req and is_binary(bin) do
    args = [bin, r_callback(callback, :state)]
    nextBlockNo = r_callback(callback, :block_no) + 1

    case (try do
            safe_apply(r_callback(callback, :module), fun, args)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:more, newState} ->
        count = r_callback(callback, :count) + :erlang.size(bin)
        callback2 = r_callback(callback, state: newState, block_no: nextBlockNo, count: count)
        prepared = r_prepared(status: :more, block_no: nextBlockNo)
        verify_count(config, callback2, req, prepared)

      {:last, result} ->
        prepared = r_prepared(status: :last, result: result, block_no: nextBlockNo)
        {:undefined, prepared}

      {:error, {code, text}} ->
        error = r_tftp_msg_error(code: code, text: text)
        prepared = r_prepared(status: :error, result: error)
        {:undefined, prepared}

      illegal ->
        code = :undef
        text = 'Internal error. File handler error.'
        callback({:abort, {code, text, illegal}}, config, callback, req)
    end
  end

  defp do_callback({:open, type}, config, callback, req)
       when elem(config, 0) === :config and
              elem(callback, 0) === :callback and
              elem(req, 0) === :tftp_msg_req do
    {access, filename} = local_file_access(req)

    {fun, blockNo} =
      case type do
        :client_prepare ->
          {:prepare, :undefined}

        :client_open ->
          {:open, 0}

        :server_open ->
          {:open, 0}
      end

    mod = r_callback(callback, :module)

    args = [
      access,
      filename,
      r_tftp_msg_req(req, :mode),
      r_tftp_msg_req(req, :options),
      r_callback(callback, :state)
    ]

    peerInfo = peer_info(config)
    _ = fast_ensure_loaded(mod)

    args2 =
      case :erlang.function_exported(mod, fun, length(args)) do
        true ->
          args

        false ->
          [peerInfo | args]
      end

    case (try do
            safe_apply(mod, fun, args2)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, acceptedOptions, newState} ->
        callback2 = r_callback(callback, state: newState, block_no: blockNo, count: 0)
        {callback2, {:ok, acceptedOptions}}

      {:error, {code, text}} ->
        {:undefined, r_tftp_msg_error(code: code, text: text)}

      illegal ->
        code = :undef
        text = 'Internal error. File handler error.'
        callback({:abort, {code, text, illegal}}, config, callback, req)
    end
  end

  defp do_callback({:abort, {code, text}}, config, callback, req) do
    error = r_tftp_msg_error(code: code, text: text)
    do_callback({:abort, error}, config, callback, req)
  end

  defp do_callback({:abort, {code, text, details}}, config, callback, req) do
    error = r_tftp_msg_error(code: code, text: text, details: details)
    do_callback({:abort, error}, config, callback, req)
  end

  defp do_callback(
         {:abort = fun, r_tftp_msg_error(code: code, text: text) = error},
         config,
         callback,
         req
       )
       when elem(config, 0) === :config and
              elem(callback, 0) === :callback and
              elem(req, 0) === :tftp_msg_req do
    args = [code, text, r_callback(callback, :state)]

    try do
      safe_apply(r_callback(callback, :module), fun, args)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    {:undefined, error}
  end

  defp do_callback({:abort, error}, _Config, :undefined, _Req)
       when elem(error, 0) === :tftp_msg_error do
    {:undefined, error}
  end

  defp peer_info(r_config(udp_host: host, udp_port: port)) do
    cond do
      is_tuple(host) and :erlang.size(host) === 4 ->
        {:inet, :tftp_lib.host_to_string(host), port}

      is_tuple(host) and :erlang.size(host) === 8 ->
        {:inet6, :tftp_lib.host_to_string(host), port}

      true ->
        {:undefined, host, port}
    end
  end

  defp match_callback(filename, callbacks) do
    cond do
      filename === :binary ->
        lookup_callback_mod(:tftp_binary, callbacks)

      is_binary(filename) ->
        lookup_callback_mod(:tftp_binary, callbacks)

      true ->
        do_match_callback(filename, callbacks)
    end
  end

  defp do_match_callback(filename, [c | tail])
       when elem(c, 0) === :callback do
    case (try do
            :re.run(filename, r_callback(c, :internal), [{:capture, :none}])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :match ->
        {:ok, c}

      :nomatch ->
        do_match_callback(filename, tail)

      details ->
        code = :baduser
        text = 'Internal error. File handler not found'
        {:error, r_tftp_msg_error(code: code, text: text, details: details)}
    end
  end

  defp do_match_callback(filename, []) do
    code = :baduser
    text = 'Internal error. File handler not found'
    {:error, r_tftp_msg_error(code: code, text: text, details: filename)}
  end

  defp lookup_callback_mod(mod, callbacks) do
    {:value, c} = :lists.keysearch(mod, r_callback(:module), callbacks)
    {:ok, c}
  end

  defp verify_count(config, callback, req, result) do
    case r_config(config, :max_tsize) do
      :infinity ->
        {callback, result}

      max when r_callback(callback, :count) <= max ->
        {callback, result}

      _Max ->
        code = :enospc
        text = 'Too large file.'
        callback({:abort, {code, text}}, config, callback, req)
    end
  end

  defp internal_info(config, type)
       when elem(config, 0) === :config do
    {:ok, actualPort} = :inet.port(r_config(config, :udp_socket))

    [
      {:type, type},
      {:host, :tftp_lib.host_to_string(r_config(config, :udp_host))},
      {:port, r_config(config, :udp_port)},
      {:local_port, actualPort},
      {:port_policy, r_config(config, :port_policy)},
      {:udp, r_config(config, :udp_options)},
      {:use_tsize, r_config(config, :use_tsize)},
      {:max_tsize, r_config(config, :max_tsize)},
      {:max_conn, r_config(config, :max_conn)},
      {:rejected, r_config(config, :rejected)},
      {:timeout, r_config(config, :timeout)},
      {:polite_ack, r_config(config, :polite_ack)},
      {:debug, r_config(config, :debug_level)},
      {:parent_pid, r_config(config, :parent_pid)}
    ] ++ r_config(config, :user_options) ++ r_config(config, :callbacks)
  end

  defp local_file_access(
         r_tftp_msg_req(access: access, local_filename: local, filename: filename)
       ) do
    case local === :undefined do
      true ->
        {access, filename}

      false ->
        case access do
          :read ->
            {:write, local}

          :write ->
            {:read, local}
        end
    end
  end

  defp pre_verify_options(config, req) do
    options = r_tftp_msg_req(req, :options)

    case (try do
            verify_reject(config, req, options)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :ok ->
        case verify_integer('tsize', 0, r_config(config, :max_tsize), options) do
          true ->
            case verify_integer('blksize', 0, 65464, options) do
              true ->
                :ok

              false ->
                {:error, {:badopt, 'Too large blksize'}}
            end

          false ->
            {:error, {:badopt, 'Too large tsize'}}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp post_verify_options(config, req, newOptions, text) do
    oldOptions = r_tftp_msg_req(req, :options)

    badOptions =
      for {key, _Val} <- newOptions,
          not :lists.keymember(key, 1, oldOptions) do
        key
      end

    case badOptions === [] do
      true ->
        config2 = r_config(config, timeout: lookup_timeout(newOptions))
        req2 = r_tftp_msg_req(req, options: newOptions)
        {:ok, config2, req2}

      false ->
        {:error, {:badopt, text}}
    end
  end

  defp verify_reject(config, req, options) do
    access = r_tftp_msg_req(req, :access)
    rejected = r_config(config, :rejected)

    case :lists.member(access, rejected) do
      true ->
        {:error, {:eacces, :erlang.atom_to_list(access) ++ ' mode not allowed'}}

      false ->
        for {key, _} <- options, :lists.member(key, rejected) do
          throw({:error, {:badopt, key ++ ' not allowed'}})
        end

        :ok
    end
  end

  defp lookup_timeout(options) do
    case :lists.keysearch('timeout', 1, options) do
      {:value, {_, val}} ->
        :erlang.list_to_integer(val)

      false ->
        3
    end
  end

  defp lookup_mode(options) do
    case :lists.keysearch('mode', 1, options) do
      {:value, {_, val}} ->
        val

      false ->
        'octet'
    end
  end

  defp verify_integer(key, min, max, options) do
    case :lists.keysearch(key, 1, options) do
      {:value, {_, val}} when is_list(val) ->
        case (try do
                :erlang.list_to_integer(val)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            false

          int
          when int >= min and is_integer(min) and
                 max === :infinity ->
            true

          int
          when int >= min and is_integer(min) and
                 int <= max and is_integer(max) ->
            true

          _ ->
            false
        end

      false ->
        true
    end
  end

  defp error_msg(r_config(logger: logger, debug_level: _Level), f, a) do
    safe_apply(logger, :error_msg, [f, a])
  end

  defp warning_msg(r_config(logger: logger, debug_level: level), f, a) do
    case level do
      :none ->
        :ok

      :error ->
        :ok

      _ ->
        safe_apply(logger, :warning_msg, [f, a])
    end
  end

  defp info_msg(r_config(logger: logger), f, a) do
    safe_apply(logger, :info_msg, [f, a])
  end

  defp safe_apply(mod, fun, args) do
    _ = fast_ensure_loaded(mod)
    apply(mod, fun, args)
  end

  defp fast_ensure_loaded(mod) do
    case :erlang.function_exported(mod, :module_info, 0) do
      true ->
        :ok

      false ->
        res = :code.load_file(mod)
        res
    end
  end

  defp print_debug_info(r_config(debug_level: level) = config, who, where, data) do
    cond do
      level === :none ->
        :ok

      elem(data, 0) === :error ->
        do_print_debug_info(config, who, where, data)

      level === :warning ->
        :ok

      level === :error ->
        :ok

      level === :all ->
        do_print_debug_info(config, who, where, data)

      where === :open ->
        do_print_debug_info(config, who, where, data)

      where === :close ->
        do_print_debug_info(config, who, where, data)

      level === :brief ->
        :ok

      where !== :recv and where !== :send ->
        :ok

      elem(data, 0) === :tftp_msg_data and
          level === :normal ->
        :ok

      elem(data, 0) === :tftp_msg_ack and
          level === :normal ->
        :ok

      true ->
        do_print_debug_info(config, who, where, data)
    end
  end

  defp do_print_debug_info(config, who, where, r_tftp_msg_data(data: bin) = msg)
       when is_binary(bin) do
    msg2 = r_tftp_msg_data(msg, data: {:bytes, :erlang.size(bin)})
    do_print_debug_info(config, who, where, msg2)
  end

  defp do_print_debug_info(config, who, where, r_tftp_msg_req(local_filename: filename) = msg)
       when is_binary(filename) do
    msg2 = r_tftp_msg_req(msg, local_filename: :binary)
    do_print_debug_info(config, who, where, msg2)
  end

  defp do_print_debug_info(config, who, where, data) do
    local =
      case (try do
              :inet.port(r_config(config, :udp_socket))
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:EXIT, _Reason} ->
          0

        {:ok, port} ->
          port
      end

    peerInfo =
      :lists.flatten(
        :io_lib.format(
          '~p',
          [peer_info(config)]
        )
      )

    side =
      cond do
        elem(who, 0) === :tftp_msg_req and
            r_tftp_msg_req(who, :local_filename) !== :undefined ->
          :client

        elem(who, 0) === :tftp_msg_req and
            r_tftp_msg_req(who, :local_filename) === :undefined ->
          :server

        is_atom(who) ->
          who
      end

    case {where, data} do
      {_, r_error(where: ^where, code: code, text: text, filename: filename)} ->
        do_format(config, side, local, 'error ~s ->\n\t~p ~p\n\t~p ~p: ~s\n', [
          peerInfo,
          self(),
          filename,
          where,
          code,
          text
        ])

      {:open, r_tftp_msg_req(filename: filename)} ->
        do_format(config, side, local, 'open  ~s ->\n\t~p ~p\n', [peerInfo, self(), filename])

      {:close, r_tftp_msg_req(filename: filename)} ->
        do_format(config, side, local, 'close ~s ->\n\t~p ~p\n', [peerInfo, self(), filename])

      {:recv, _} ->
        do_format(config, side, local, 'recv  ~s <-\n\t~p\n', [peerInfo, data])

      {:send, _} ->
        do_format(config, side, local, 'send  ~s ->\n\t~p\n', [peerInfo, data])

      {:match, _} when elem(data, 0) === :callback ->
        mod = r_callback(data, :module)
        state = r_callback(data, :state)
        do_format(config, side, local, 'match ~s ~p =>\n\t~p\n', [peerInfo, mod, state])

      {:call, _} ->
        case data do
          {callback, _Result} when elem(callback, 0) === :callback ->
            mod = r_callback(callback, :module)
            state = r_callback(callback, :state)
            do_format(config, side, local, 'call ~s ~p =>\n\t~p\n', [peerInfo, mod, state])

          {:undefined, result} ->
            do_format(config, side, local, 'call ~s result =>\n\t~p\n', [peerInfo, result])
        end
    end
  end

  defp do_format(config, side, local, format, args) do
    info_msg(config, '~p(~p): ' ++ format, [side, local | args])
  end

  def system_continue(_Parent, _Debug, r_sys_misc(module: mod, function: fun, arguments: args)) do
    apply(mod, fun, args)
  end

  def system_continue(parent, debug, {fun, args}) do
    system_continue(
      parent,
      debug,
      r_sys_misc(module: :tftp_engine, function: fun, arguments: args)
    )
  end

  def system_terminate(reason, _Parent, _Debug, r_sys_misc()) do
    exit(reason)
  end

  def system_terminate(reason, parent, debug, {fun, args}) do
    system_terminate(
      reason,
      parent,
      debug,
      r_sys_misc(module: :tftp_engine, function: fun, arguments: args)
    )
  end

  def system_code_change({fun, args}, _Module, _OldVsn, _Extra) do
    {:ok, {fun, args}}
  end
end
