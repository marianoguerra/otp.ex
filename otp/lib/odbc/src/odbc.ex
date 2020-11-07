defmodule :m_odbc do
  use Bitwise
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_state, :state,
    erlang_port: :undefined,
    reply_to: :undefined,
    owner: :undefined,
    result_set: :undefined,
    auto_commit_mode: :on,
    absolute_pos: :undefined,
    relative_pos: :undefined,
    scrollable_cursors: :undefined,
    state: :connecting,
    pending_request: :undefined,
    num_timeouts: 0,
    listen_sockets: :undefined,
    sup_socket: :undefined,
    odbc_socket: :undefined
  )

  def start() do
    :application.start(:odbc)
  end

  def start(type) do
    :application.start(:odbc, type)
  end

  def stop() do
    :application.stop(:odbc)
  end

  def connect(connectionStr, options)
      when is_list(connectionStr) and is_list(options) do
    try do
      :supervisor.start_child(
        :odbc_sup,
        [[{:client, self()}]]
      )
    catch
      :exit, {:noproc, _} ->
        {:error, :odbc_not_started}
    else
      {:ok, pid} ->
        connect(pid, connectionStr, options)

      {:error, reason} ->
        {:error, reason}
    end
  end

  def disconnect(connectionReference)
      when is_pid(connectionReference) do
    oDBCCmd = [2]

    case call(connectionReference, {:disconnect, oDBCCmd}, 5000) do
      {:error, :connection_closed} ->
        :ok

      :ok ->
        :ok

      other ->
        other
    end
  end

  def commit(connectionReference, commitMode) do
    commit(connectionReference, commitMode, :infinity)
  end

  def commit(connectionReference, :commit, :infinity)
      when is_pid(connectionReference) do
    oDBCCmd = [3, 4]
    call(connectionReference, {:commit, oDBCCmd}, :infinity)
  end

  def commit(connectionReference, :commit, timeOut)
      when is_pid(connectionReference) and
             is_integer(timeOut) and timeOut > 0 do
    oDBCCmd = [3, 4]
    call(connectionReference, {:commit, oDBCCmd}, timeOut)
  end

  def commit(connectionReference, :rollback, :infinity)
      when is_pid(connectionReference) do
    oDBCCmd = [3, 5]
    call(connectionReference, {:commit, oDBCCmd}, :infinity)
  end

  def commit(connectionReference, :rollback, timeOut)
      when is_pid(connectionReference) and
             is_integer(timeOut) and timeOut > 0 do
    oDBCCmd = [3, 5]
    call(connectionReference, {:commit, oDBCCmd}, timeOut)
  end

  def sql_query(connectionReference, sQLQuery) do
    sql_query(connectionReference, sQLQuery, :infinity)
  end

  def sql_query(connectionReference, sQLQuery, :infinity)
      when is_pid(connectionReference) and
             is_list(sQLQuery) do
    oDBCCmd = [6, sQLQuery]
    call(connectionReference, {:sql_query, oDBCCmd}, :infinity)
  end

  def sql_query(connectionReference, sQLQuery, timeOut)
      when is_pid(connectionReference) and
             is_list(sQLQuery) and is_integer(timeOut) and
             timeOut > 0 do
    oDBCCmd = [6, sQLQuery]
    call(connectionReference, {:sql_query, oDBCCmd}, timeOut)
  end

  def select_count(connectionReference, sQLQuery) do
    select_count(connectionReference, sQLQuery, :infinity)
  end

  def select_count(connectionReference, sQLQuery, :infinity)
      when is_pid(connectionReference) and
             is_list(sQLQuery) do
    oDBCCmd = [7, sQLQuery]
    call(connectionReference, {:select_count, oDBCCmd}, :infinity)
  end

  def select_count(connectionReference, sQLQuery, timeOut)
      when is_pid(connectionReference) and
             is_list(sQLQuery) and is_integer(timeOut) and
             timeOut > 0 do
    oDBCCmd = [7, sQLQuery]
    call(connectionReference, {:select_count, oDBCCmd}, timeOut)
  end

  def first(connectionReference) do
    first(connectionReference, :infinity)
  end

  def first(connectionReference, :infinity)
      when is_pid(connectionReference) do
    oDBCCmd = [12, 8]
    call(connectionReference, {:select_cmd, :absolute, oDBCCmd}, :infinity)
  end

  def first(connectionReference, timeOut)
      when is_pid(connectionReference) and
             is_integer(timeOut) and timeOut > 0 do
    oDBCCmd = [12, 8]
    call(connectionReference, {:select_cmd, :absolute, oDBCCmd}, timeOut)
  end

  def last(connectionReference) do
    last(connectionReference, :infinity)
  end

  def last(connectionReference, :infinity)
      when is_pid(connectionReference) do
    oDBCCmd = [12, 9]
    call(connectionReference, {:select_cmd, :absolute, oDBCCmd}, :infinity)
  end

  def last(connectionReference, timeOut)
      when is_pid(connectionReference) and
             is_integer(timeOut) and timeOut > 0 do
    oDBCCmd = [12, 9]
    call(connectionReference, {:select_cmd, :absolute, oDBCCmd}, timeOut)
  end

  def next(connectionReference) do
    next(connectionReference, :infinity)
  end

  def next(connectionReference, :infinity)
      when is_pid(connectionReference) do
    oDBCCmd = [12, 10]
    call(connectionReference, {:select_cmd, :next, oDBCCmd}, :infinity)
  end

  def next(connectionReference, timeOut)
      when is_pid(connectionReference) and
             is_integer(timeOut) and timeOut > 0 do
    oDBCCmd = [12, 10]
    call(connectionReference, {:select_cmd, :next, oDBCCmd}, timeOut)
  end

  def prev(connectionReference) do
    prev(connectionReference, :infinity)
  end

  def prev(connectionReference, :infinity)
      when is_pid(connectionReference) do
    oDBCCmd = [12, 11]
    call(connectionReference, {:select_cmd, :relative, oDBCCmd}, :infinity)
  end

  def prev(connectionReference, timeOut)
      when is_pid(connectionReference) and
             is_integer(timeOut) and timeOut > 0 do
    oDBCCmd = [12, 11]
    call(connectionReference, {:select_cmd, :relative, oDBCCmd}, timeOut)
  end

  def select(connectionReference, position, n) do
    select(connectionReference, position, n, :infinity)
  end

  def select(connectionReference, :next, n, :infinity)
      when is_pid(connectionReference) and is_integer(n) and
             n > 0 do
    oDBCCmd = [12, 15, :erlang.integer_to_list(0), ';', :erlang.integer_to_list(n), ';']
    call(connectionReference, {:select_cmd, :next, oDBCCmd}, :infinity)
  end

  def select(connectionReference, :next, n, timeOut)
      when is_pid(connectionReference) and is_integer(n) and
             n > 0 and is_integer(timeOut) and timeOut > 0 do
    oDBCCmd = [12, 15, :erlang.integer_to_list(0), ';', :erlang.integer_to_list(n), ';']
    call(connectionReference, {:select_cmd, :next, oDBCCmd}, timeOut)
  end

  def select(connectionReference, {:relative, pos}, n, :infinity)
      when is_pid(connectionReference) and
             is_integer(pos) and pos > 0 and is_integer(n) and
             n > 0 do
    oDBCCmd = [12, 13, :erlang.integer_to_list(pos), ';', :erlang.integer_to_list(n), ';']
    call(connectionReference, {:select_cmd, :relative, oDBCCmd}, :infinity)
  end

  def select(connectionReference, {:relative, pos}, n, timeOut)
      when is_pid(connectionReference) and
             is_integer(pos) and pos > 0 and is_integer(n) and
             n > 0 and is_integer(timeOut) and timeOut > 0 do
    oDBCCmd = [12, 13, :erlang.integer_to_list(pos), ';', :erlang.integer_to_list(n), ';']
    call(connectionReference, {:select_cmd, :relative, oDBCCmd}, timeOut)
  end

  def select(connectionReference, {:absolute, pos}, n, :infinity)
      when is_pid(connectionReference) and
             is_integer(pos) and pos > 0 and is_integer(n) and
             n > 0 do
    oDBCCmd = [12, 14, :erlang.integer_to_list(pos), ';', :erlang.integer_to_list(n), ';']
    call(connectionReference, {:select_cmd, :absolute, oDBCCmd}, :infinity)
  end

  def select(connectionReference, {:absolute, pos}, n, timeOut)
      when is_pid(connectionReference) and
             is_integer(pos) and pos > 0 and is_integer(n) and
             n > 0 and is_integer(timeOut) and timeOut > 0 do
    oDBCCmd = [12, 14, :erlang.integer_to_list(pos), ';', :erlang.integer_to_list(n), ';']
    call(connectionReference, {:select_cmd, :absolute, oDBCCmd}, timeOut)
  end

  def param_query(connectionReference, sQLQuery, params) do
    param_query(connectionReference, sQLQuery, params, :infinity)
  end

  def param_query(connectionReference, sQLQuery, params, :infinity)
      when is_pid(connectionReference) and
             is_list(sQLQuery) and is_list(params) do
    values = param_values(params)
    noRows = length(values)
    newParams = :lists.map(&fix_params/1, params)
    oDBCCmd = [16, :erlang.term_to_binary({sQLQuery ++ [0], noRows, newParams})]
    call(connectionReference, {:param_query, oDBCCmd}, :infinity)
  end

  def param_query(connectionReference, sQLQuery, params, timeOut)
      when is_pid(connectionReference) and
             is_list(sQLQuery) and is_list(params) and
             is_integer(timeOut) and timeOut > 0 do
    values = param_values(params)
    noRows = length(values)
    newParams = :lists.map(&fix_params/1, params)
    oDBCCmd = [16, :erlang.term_to_binary({sQLQuery ++ [0], noRows, newParams})]
    call(connectionReference, {:param_query, oDBCCmd}, timeOut)
  end

  def describe_table(connectionReference, table) do
    describe_table(connectionReference, table, :infinity)
  end

  def describe_table(connectionReference, table, :infinity)
      when is_pid(connectionReference) and is_list(table) do
    oDBCCmd = [17, 'SELECT * FROM ' ++ table]
    call(connectionReference, {:describe_table, oDBCCmd}, :infinity)
  end

  def describe_table(connectionReference, table, timeOut)
      when is_pid(connectionReference) and is_list(table) and
             is_integer(timeOut) and timeOut > 0 do
    oDBCCmd = [17, 'SELECT * FROM ' ++ table]
    call(connectionReference, {:describe_table, oDBCCmd}, timeOut)
  end

  def start_link_sup(args) do
    :gen_server.start_link(:odbc, args, [])
  end

  def init(args) do
    :erlang.process_flag(:trap_exit, true)
    {:value, {:client, clientPid}} = :lists.keysearch(:client, 1, args)
    :erlang.monitor(:process, clientPid)

    inet =
      case :gen_tcp.listen(
             0,
             [:inet6, {:ip, :loopback}]
           ) do
        {:ok, dummyport} ->
          :gen_tcp.close(dummyport)
          :inet6

        _ ->
          :inet
      end

    {:ok, listenSocketSup} =
      :gen_tcp.listen(
        0,
        [inet, :binary, {:packet, 4}, {:active, false}, {:nodelay, true}, {:ip, :loopback}]
      )

    {:ok, listenSocketOdbc} =
      :gen_tcp.listen(
        0,
        [inet, :binary, {:packet, 4}, {:active, false}, {:nodelay, true}, {:ip, :loopback}]
      )

    case :os.find_executable(
           'odbcserver',
           :filename.nativename(
             :filename.join(
               :code.priv_dir(:odbc),
               'bin'
             )
           )
         ) do
      fileName when is_list(fileName) ->
        port =
          :erlang.open_port(
            {:spawn, '"' ++ fileName ++ '"'},
            [{:packet, 4}, :binary, :exit_status]
          )

        state =
          r_state(
            listen_sockets: [listenSocketSup, listenSocketOdbc],
            erlang_port: port,
            owner: clientPid
          )

        {:ok, state}

      false ->
        {:stop, :port_program_executable_not_found}
    end
  end

  def handle_call(
        {client, msg, timeout},
        from,
        state = r_state(owner: client, reply_to: :undefined)
      ) do
    handle_msg(msg, timeout, r_state(state, reply_to: from))
  end

  def handle_call(
        request = {client, _, timeout},
        from,
        state = r_state(owner: client, reply_to: :skip, num_timeouts: n)
      )
      when n < 10 do
    {:noreply, r_state(state, pending_request: {request, from}), timeout}
  end

  def handle_call({client, _, _}, from, state = r_state(owner: client, num_timeouts: n))
      when n >= 10 do
    :gen_server.reply(from, {:error, :connection_closed})
    {:stop, :too_many_sequential_timeouts, r_state(state, reply_to: :undefined)}
  end

  def handle_call(_, _, state) do
    {:reply, {:error, :process_not_owner_of_odbc_connection},
     r_state(state, reply_to: :undefined)}
  end

  defp handle_msg({:connect, oDBCCmd, autoCommitMode, srollableCursors}, timeout, state) do
    [listenSocketSup, listenSocketOdbc] = r_state(state, :listen_sockets)
    {:ok, inetPortSup} = :inet.port(listenSocketSup)
    {:ok, inetPortOdbc} = :inet.port(listenSocketOdbc)

    :erlang.port_command(
      r_state(state, :erlang_port),
      [:erlang.integer_to_list(inetPortSup), ';', :erlang.integer_to_list(inetPortOdbc), 0]
    )

    newState =
      r_state(state,
        auto_commit_mode: autoCommitMode,
        scrollable_cursors: srollableCursors
      )

    case :gen_tcp.accept(
           listenSocketSup,
           port_timeout()
         ) do
      {:ok, supSocket} ->
        :gen_tcp.close(listenSocketSup)

        case :gen_tcp.accept(
               listenSocketOdbc,
               port_timeout()
             ) do
          {:ok, odbcSocket} ->
            :gen_tcp.close(listenSocketOdbc)
            odbc_send(odbcSocket, oDBCCmd)

            {:noreply,
             r_state(newState,
               odbc_socket: odbcSocket,
               sup_socket: supSocket
             ), timeout}

          {:error, reason} ->
            {:stop, reason, {:error, :connection_closed}, newState}
        end

      {:error, reason} ->
        {:stop, reason, {:error, :connection_closed}, newState}
    end
  end

  defp handle_msg({:disconnect, oDBCCmd}, timeout, state) do
    odbc_send(r_state(state, :odbc_socket), oDBCCmd)
    {:noreply, r_state(state, state: :disconnecting), timeout}
  end

  defp handle_msg({:commit, _ODBCCmd}, timeout, state = r_state(auto_commit_mode: :on)) do
    {:reply, {:error, :not_an_explicit_commit_connection}, r_state(state, reply_to: :undefined),
     timeout}
  end

  defp handle_msg({:commit, oDBCCmd}, timeout, state = r_state(auto_commit_mode: :off)) do
    odbc_send(r_state(state, :odbc_socket), oDBCCmd)
    {:noreply, state, timeout}
  end

  defp handle_msg({:sql_query, oDBCCmd}, timeout, state) do
    odbc_send(r_state(state, :odbc_socket), oDBCCmd)
    {:noreply, r_state(state, result_set: :undefined), timeout}
  end

  defp handle_msg({:param_query, oDBCCmd}, timeout, state) do
    odbc_send(r_state(state, :odbc_socket), oDBCCmd)
    {:noreply, r_state(state, result_set: :undefined), timeout}
  end

  defp handle_msg({:describe_table, oDBCCmd}, timeout, state) do
    odbc_send(r_state(state, :odbc_socket), oDBCCmd)
    {:noreply, r_state(state, result_set: :undefined), timeout}
  end

  defp handle_msg({:select_count, oDBCCmd}, timeout, state) do
    odbc_send(r_state(state, :odbc_socket), oDBCCmd)
    {:noreply, r_state(state, result_set: :exists), timeout}
  end

  defp handle_msg(
         {:select_cmd, :absolute, oDBCCmd},
         timeout,
         state = r_state(result_set: :exists, absolute_pos: true)
       ) do
    odbc_send(r_state(state, :odbc_socket), oDBCCmd)
    {:noreply, state, timeout}
  end

  defp handle_msg(
         {:select_cmd, :relative, oDBCCmd},
         timeout,
         state = r_state(result_set: :exists, relative_pos: true)
       ) do
    odbc_send(r_state(state, :odbc_socket), oDBCCmd)
    {:noreply, state, timeout}
  end

  defp handle_msg({:select_cmd, :next, oDBCCmd}, timeout, state = r_state(result_set: :exists)) do
    odbc_send(r_state(state, :odbc_socket), oDBCCmd)
    {:noreply, state, timeout}
  end

  defp handle_msg(
         {:select_cmd, _Type, _ODBCCmd},
         _Timeout,
         state = r_state(result_set: :undefined)
       ) do
    {:reply, {:error, :result_set_does_not_exist}, r_state(state, reply_to: :undefined)}
  end

  defp handle_msg({:select_cmd, _Type, _ODBCCmd}, _Timeout, state) do
    reply =
      case r_state(state, :scrollable_cursors) do
        :on ->
          {:error, :driver_does_not_support_function}

        :off ->
          {:error, :scrollable_cursors_disabled}
      end

    {:reply, reply, r_state(state, reply_to: :undefined)}
  end

  defp handle_msg(request, _Timeout, state) do
    {:stop, {:API_violation_connection_colsed, request}, {:error, :connection_closed},
     r_state(state, reply_to: :undefined)}
  end

  def handle_cast(msg, state) do
    {:stop, {:API_violation_connection_colsed, msg}, state}
  end

  def handle_info(
        {:tcp, socket, binData},
        state = r_state(state: :connecting, reply_to: from, odbc_socket: socket)
      ) do
    case :erlang.binary_to_term(binData) do
      {:ok, absolutSupport, relativeSupport} ->
        newState =
          r_state(state,
            absolute_pos: absolutSupport,
            relative_pos: relativeSupport
          )

        :gen_server.reply(from, :ok)
        {:noreply, r_state(newState, state: :connected, reply_to: :undefined)}

      error ->
        :gen_server.reply(from, error)
        {:stop, :normal, r_state(state, reply_to: :undefined)}
    end
  end

  def handle_info(
        {:tcp, socket, _},
        state =
          r_state(
            state: :connected,
            odbc_socket: socket,
            reply_to: :skip,
            pending_request: :undefined
          )
      ) do
    {:noreply, r_state(state, reply_to: :undefined)}
  end

  def handle_info(
        {:tcp, socket, _},
        state = r_state(state: :connected, odbc_socket: socket, reply_to: :skip)
      ) do
    {{_, msg, timeout}, from} = r_state(state, :pending_request)
    handle_msg(msg, timeout, r_state(state, pending_request: :undefined, reply_to: from))
  end

  def handle_info(
        {:tcp, socket, binData},
        state = r_state(state: :connected, reply_to: from, odbc_socket: socket)
      ) do
    :gen_server.reply(from, binData)
    {:noreply, r_state(state, reply_to: :undefined, num_timeouts: 0)}
  end

  def handle_info(
        {:tcp, socket, binData},
        state = r_state(state: :disconnecting, reply_to: from, odbc_socket: socket)
      ) do
    :gen_server.reply(from, :ok)

    case :erlang.binary_to_term(binData) do
      :ok ->
        :ok

      {:error, reason} ->
        report = :io_lib.format('ODBC could not end connection gracefully due to ~p~n', [reason])
        :error_logger.error_report(report)
    end

    {:stop, :normal, r_state(state, reply_to: :undefined)}
  end

  def handle_info(
        :timeout,
        state = r_state(state: :disconnecting, reply_to: from)
      )
      when from != :undefined do
    :gen_server.reply(from, :ok)
    {:stop, {:timeout, 'Port program is not responding to disconnect, will be killed'}, state}
  end

  def handle_info(
        :timeout,
        state = r_state(state: :connecting, reply_to: from)
      )
      when from != :undefined do
    :gen_server.reply(from, :timeout)
    {:stop, :normal, r_state(state, reply_to: :undefined)}
  end

  def handle_info(
        :timeout,
        state = r_state(state: :connected, pending_request: :undefined, reply_to: from)
      )
      when from != :undefined do
    :gen_server.reply(from, :timeout)

    {:noreply,
     r_state(state,
       reply_to: :skip,
       num_timeouts: r_state(state, :num_timeouts) + 1
     )}
  end

  def handle_info(
        :timeout,
        state =
          r_state(
            state: :connected,
            pending_request: {{_, {:disconnect, _}, _}, pendingFrom}
          )
      ) do
    :gen_server.reply(pendingFrom, :ok)

    {:stop, {:timeout, 'Port-program busy when trying to disconnect,  will be killed'},
     r_state(state,
       pending_request: :undefined,
       reply_to: :undefined,
       num_timeouts: r_state(state, :num_timeouts) + 1
     )}
  end

  def handle_info(
        :timeout,
        state =
          r_state(
            state: :connected,
            pending_request: {_, pendingFrom}
          )
      ) do
    :gen_server.reply(pendingFrom, :timeout)

    {:noreply,
     r_state(state,
       pending_request: :undefined,
       num_timeouts: r_state(state, :num_timeouts) + 1
     )}
  end

  def handle_info(
        {port, {:exit_status, 0}},
        state = r_state(erlang_port: port, state: :disconnecting)
      ) do
    {:noreply, state}
  end

  def handle_info(
        {port, {:exit_status, status}},
        state = r_state(erlang_port: port)
      ) do
    {:stop,
     {:port_exit,
      (fn
         0 ->
           :normal_exit

         1 ->
           :abnormal_exit

         2 ->
           :memory_allocation_failed

         3 ->
           :setting_of_environment_attributes_failed

         4 ->
           :setting_of_connection_attributes_faild

         5 ->
           :freeing_of_memory_failed

         6 ->
           :receiving_port_message_header_failed

         7 ->
           :receiving_port_message_body_failed

         8 ->
           :retrieving_of_binary_data_failed

         9 ->
           :failed_to_create_thread

         10 ->
           :does_not_support_param_arrays

         11 ->
           :too_old_verion_of_winsock

         12 ->
           :socket_connect_failed

         13 ->
           :socket_send_message_header_failed

         14 ->
           :socket_send_message_body_failed

         15 ->
           :socket_received_too_large_message

         16 ->
           :too_large_message_in_socket_send

         17 ->
           :socket_receive_message_header_failed

         18 ->
           :socket_receive_message_body_failed

         19 ->
           :could_not_access_column_count

         20 ->
           :could_not_access_row_count

         21 ->
           :could_not_access_table_description

         22 ->
           :could_not_bind_data_buffers

         23 ->
           :collecting_of_driver_information_faild

         _ ->
           :killed
       end).(status)}, state}
  end

  def handle_info(
        {:EXIT, port, _},
        state = r_state(erlang_port: port, state: :disconnecting)
      ) do
    {:noreply, state}
  end

  def handle_info(
        {:EXIT, port, reason},
        state = r_state(erlang_port: port)
      ) do
    {:stop, reason, state}
  end

  def handle_info(
        {:DOWN, _Ref, _Type, _Process, :normal},
        state
      ) do
    {:stop, :normal, r_state(state, reply_to: :undefined)}
  end

  def handle_info(
        {:DOWN, _Ref, _Type, _Process, :timeout},
        state
      ) do
    {:stop, :normal, r_state(state, reply_to: :undefined)}
  end

  def handle_info(
        {:DOWN, _Ref, _Type, _Process, :shutdown},
        state
      ) do
    {:stop, :normal, r_state(state, reply_to: :undefined)}
  end

  def handle_info({:DOWN, _Ref, _Type, process, reason}, state) do
    {:stop, {:stopped, {:EXIT, process, reason}}, r_state(state, reply_to: :undefined)}
  end

  def handle_info(
        {:tcp_closed, socket},
        state =
          r_state(
            odbc_socket: socket,
            state: :disconnecting
          )
      ) do
    {:stop, :normal, state}
  end

  def handle_info(info, state) do
    report = :io_lib.format('ODBC: received unexpected info: ~p~n', [info])
    :error_logger.error_report(report)
    {:noreply, state}
  end

  def terminate(
        {:port_exit, _Reason},
        state = r_state(reply_to: :undefined)
      ) do
    :gen_tcp.close(r_state(state, :odbc_socket))
    :gen_tcp.close(r_state(state, :sup_socket))
    :ok
  end

  def terminate(_Reason, state = r_state(reply_to: :undefined)) do
    try do
      :gen_tcp.send(r_state(state, :sup_socket), [18, 0])
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    try do
      :gen_tcp.close(r_state(state, :odbc_socket))
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    try do
      :gen_tcp.close(r_state(state, :sup_socket))
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    try do
      :erlang.port_close(r_state(state, :erlang_port))
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def terminate(reason, state = r_state(reply_to: from)) do
    :gen_server.reply(from, {:error, :connection_closed})
    terminate(reason, r_state(state, reply_to: :undefined))
  end

  def code_change(_Vsn, state, _Extra) do
    {:ok, state}
  end

  defp connect(connectionReferense, connectionStr, options) do
    {c_AutoCommitMode, eRL_AutoCommitMode} =
      connection_config(
        :auto_commit,
        options
      )

    timeOut = connection_config(:timeout, options)

    {c_TraceDriver, _} =
      connection_config(
        :trace_driver,
        options
      )

    {c_SrollableCursors, eRL_SrollableCursors} =
      connection_config(
        :scrollable_cursors,
        options
      )

    {c_TupleRow, _} = connection_config(:tuple_row, options)

    {binaryStrings, _} =
      connection_config(
        :binary_strings,
        options
      )

    {extendedErrors, _} = connection_config(:extended_errors, options)

    oDBCCmd = [
      1,
      c_AutoCommitMode,
      c_TraceDriver,
      c_SrollableCursors,
      c_TupleRow,
      binaryStrings,
      extendedErrors,
      connectionStr
    ]

    case call(
           connectionReferense,
           {:connect, oDBCCmd, eRL_AutoCommitMode, eRL_SrollableCursors},
           timeOut
         ) do
      :ok ->
        {:ok, connectionReferense}

      error ->
        error
    end
  end

  defp odbc_send(socket, msg) do
    newMsg = msg ++ [0]
    :ok = :gen_tcp.send(socket, newMsg)
    :ok = :inet.setopts(socket, [{:active, :once}])
  end

  defp connection_config(key, options) do
    case :lists.keysearch(key, 1, options) do
      {:value, {^key, :on}} ->
        {1, :on}

      {:value, {^key, :off}} ->
        {2, :off}

      {:value, {^key, value}} ->
        value

      _ ->
        connection_default(key)
    end
  end

  defp connection_default(:auto_commit) do
    {1, :on}
  end

  defp connection_default(:timeout) do
    :infinity
  end

  defp connection_default(:tuple_row) do
    {1, :on}
  end

  defp connection_default(:trace_driver) do
    {2, :off}
  end

  defp connection_default(:scrollable_cursors) do
    {1, :on}
  end

  defp connection_default(:binary_strings) do
    {2, :off}
  end

  defp connection_default(:extended_errors) do
    {2, :off}
  end

  defp call(connectionReference, msg, timeout) do
    result =
      try do
        :gen_server.call(connectionReference, {self(), msg, timeout}, :infinity)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    case result do
      binary when is_binary(binary) ->
        decode(binary)

      :timeout ->
        exit(:timeout)

      {:EXIT, _} ->
        {:error, :connection_closed}

      term ->
        term
    end
  end

  defp decode(binary) do
    case :erlang.binary_to_term(binary) do
      [resultSet] ->
        resultSet

      :param_badarg ->
        exit({:badarg, :odbc, :param_query, :Params})

      multipleResultSets_or_Other ->
        multipleResultSets_or_Other
    end
  end

  defp param_values(params) do
    case params do
      [{_, values} | _] ->
        values

      [{_, _, values} | _] ->
        values

      [] ->
        []
    end
  end

  defp fix_params({:sql_integer, inOut, values}) do
    {2, fix_inout(inOut), [256 | values]}
  end

  defp fix_params({:sql_smallint, inOut, values}) do
    {1, fix_inout(inOut), [256 | values]}
  end

  defp fix_params({:sql_tinyint, inOut, values}) do
    {11, fix_inout(inOut), [256 | values]}
  end

  defp fix_params({{:sql_decimal, precision, 0}, inOut, values})
       when precision >= 0 and precision <= 9 do
    {3, precision, 0, fix_inout(inOut), [256 | values]}
  end

  defp fix_params({{:sql_decimal, precision, scale}, inOut, values}) do
    {3, precision, scale, fix_inout(inOut), values}
  end

  defp fix_params({{:sql_numeric, precision, 0}, inOut, values})
       when precision >= 0 and precision <= 9 do
    {4, precision, 0, fix_inout(inOut), [256 | values]}
  end

  defp fix_params({{:sql_numeric, precision, scale}, inOut, values}) do
    {4, precision, scale, fix_inout(inOut), values}
  end

  defp fix_params({{:sql_char, max}, inOut, values}) do
    newValues = string_terminate(values)
    {5, max, fix_inout(inOut), newValues}
  end

  defp fix_params({{:sql_varchar, max}, inOut, values}) do
    newValues = string_terminate(values)
    {6, max, fix_inout(inOut), newValues}
  end

  defp fix_params({{:sql_wchar, max}, inOut, values}) do
    newValues = string_terminate(values)
    {12, max, fix_inout(inOut), newValues}
  end

  defp fix_params({{:sql_wvarchar, max}, inOut, values}) do
    newValues = string_terminate(values)
    {13, max, fix_inout(inOut), newValues}
  end

  defp fix_params({{:sql_wlongvarchar, max}, inOut, values}) do
    newValues = string_terminate(values)
    {15, max, fix_inout(inOut), newValues}
  end

  defp fix_params({{:sql_float, precision}, inOut, values}) do
    {7, precision, fix_inout(inOut), values}
  end

  defp fix_params({:sql_real, inOut, values}) do
    {8, fix_inout(inOut), values}
  end

  defp fix_params({:sql_double, inOut, values}) do
    {9, fix_inout(inOut), values}
  end

  defp fix_params({:sql_bit, inOut, values}) do
    {10, fix_inout(inOut), values}
  end

  defp fix_params({:sql_timestamp, inOut, values}) do
    newValues =
      case (try do
              :lists.map(
                fn
                  {{year, month, day}, {hour, minute, second}} ->
                    {year, month, day, hour, minute, second}

                  :null ->
                    :null
                end,
                values
              )
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        result ->
          result
      end

    {14, fix_inout(inOut), newValues}
  end

  defp fix_params({type, values}) do
    fix_params({type, :in, values})
  end

  defp fix_inout(:in) do
    0
  end

  defp fix_inout(:out) do
    1
  end

  defp fix_inout(:inout) do
    2
  end

  defp string_terminate(values) do
    case (try do
            :lists.map(&string_terminate_value/1, values)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      result ->
        result
    end
  end

  defp string_terminate_value(string) when is_list(string) do
    string ++ [0]
  end

  defp string_terminate_value(binary) when is_binary(binary) do
    <<binary::binary, 0::size(16)>>
  end

  defp string_terminate_value(:null) do
    :null
  end

  defp port_timeout() do
    :application.get_env(:odbc, :port_timeout, 5000)
  end
end
