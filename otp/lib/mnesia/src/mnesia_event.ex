defmodule :m_mnesia_event do
  use Bitwise
  @behaviour :gen_event
  require Record
  Record.defrecord(:r_state, :state, nodes: [], dumped_core: false, args: :undefined)

  Record.defrecord(:r_tid, :tid,
    counter: :undefined,
    pid: :undefined
  )

  Record.defrecord(:r_tidstore, :tidstore, store: :undefined, up_stores: [], level: 1)

  Record.defrecord(:r_cstruct, :cstruct,
    name: :undefined,
    type: :set,
    ram_copies: [],
    disc_copies: [],
    disc_only_copies: [],
    external_copies: [],
    load_order: 0,
    access_mode: :read_write,
    majority: false,
    index: [],
    snmp: [],
    local_content: false,
    record_name: {:bad_record_name},
    attributes: [:key, :val],
    user_properties: [],
    frag_properties: [],
    storage_properties: [],
    cookie:
      {{:erlang.monotonic_time() + :erlang.time_offset(), :erlang.unique_integer(), 1}, node()},
    version: {{2, 0}, []}
  )

  Record.defrecord(:r_log_header, :log_header,
    log_kind: :undefined,
    log_version: :undefined,
    mnesia_version: :undefined,
    node: :undefined,
    now: :undefined
  )

  Record.defrecord(:r_commit, :commit,
    node: :undefined,
    decision: :undefined,
    ram_copies: [],
    disc_copies: [],
    disc_only_copies: [],
    ext: [],
    schema_ops: []
  )

  Record.defrecord(:r_decision, :decision,
    tid: :undefined,
    outcome: :undefined,
    disc_nodes: :undefined,
    ram_nodes: :undefined
  )

  Record.defrecord(:r_cyclic, :cyclic,
    node: node(),
    oid: :undefined,
    op: :undefined,
    lock: :undefined,
    lucky: :undefined
  )

  def init(args) do
    {:ok, r_state(args: args)}
  end

  def handle_event(event, state) do
    handle_any_event(event, state)
  end

  def handle_info(msg, state) do
    {:ok, _} = handle_any_event(msg, state)
    {:ok, state}
  end

  def handle_call(msg, state) do
    reply = :ok
    {:ok, newState} = handle_any_event(msg, state)
    {:ok, reply, newState}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp handle_any_event({:mnesia_system_event, event}, state) do
    handle_system_event(event, state)
  end

  defp handle_any_event({:mnesia_table_event, event}, state) do
    handle_table_event(event, state)
  end

  defp handle_any_event(msg, state) do
    report_error('~p got unexpected event: ~tp~n', [:mnesia_event, msg])
    {:ok, state}
  end

  defp handle_table_event({oper, record, transId}, state) do
    report_info('~p performed by ~p on record:~n\t~tp~n', [oper, transId, record])
    {:ok, state}
  end

  defp handle_system_event(
         {:mnesia_checkpoint_activated, _Checkpoint},
         state
       ) do
    {:ok, state}
  end

  defp handle_system_event(
         {:mnesia_checkpoint_deactivated, checkpoint},
         state
       ) do
    report_error('Checkpoint \'~p\' has been deactivated, last table copy deleted.\n', [
      checkpoint
    ])

    {:ok, state}
  end

  defp handle_system_event({:mnesia_up, node}, state) do
    nodes = [node | r_state(state, :nodes)]
    {:ok, r_state(state, nodes: nodes)}
  end

  defp handle_system_event({:mnesia_down, node}, state) do
    case :mnesia.system_info(:fallback_activated) and node !== node() do
      true ->
        case :mnesia_monitor.get_env(:fallback_error_function) do
          {:mnesia, :lkill} ->
            msg =
              'A fallback is installed and Mnesia must be restarted. Forcing shutdown after mnesia_down from ~p...~n'

            report_fatal(msg, [node], :nocore, r_state(state, :dumped_core))

            try do
              :erlang.exit(:erlang.whereis(:mnesia_monitor), :fatal)
            catch
              :error, _ ->
                :ok
            end

            {:ok, state}

          {userMod, userFunc} ->
            msg = 'Warning: A fallback is installed and Mnesia got mnesia_down from ~p. ~n'
            report_info(msg, [node])

            case (try do
                    apply(userMod, userFunc, [node])
                  catch
                    _, _Reason ->
                      {:EXIT, _Reason}
                  end) do
              {:EXIT, {:undef, _R}} ->
                apply(userMod, userFunc, [])

              {:EXIT, reason} ->
                exit(reason)

              _ ->
                :ok
            end

            nodes = :lists.delete(node, r_state(state, :nodes))
            {:ok, r_state(state, nodes: nodes)}
        end

      false ->
        nodes = :lists.delete(node, r_state(state, :nodes))
        {:ok, r_state(state, nodes: nodes)}
    end
  end

  defp handle_system_event({:mnesia_overload, details}, state) do
    report_warning('Mnesia is overloaded: ~tw~n', [details])
    {:ok, state}
  end

  defp handle_system_event({:mnesia_info, format, args}, state) do
    report_info(format, args)
    {:ok, state}
  end

  defp handle_system_event({:mnesia_warning, format, args}, state) do
    report_warning(format, args)
    {:ok, state}
  end

  defp handle_system_event({:mnesia_error, format, args}, state) do
    report_error(format, args)
    {:ok, state}
  end

  defp handle_system_event(
         {:mnesia_fatal, format, args, binaryCore},
         state
       ) do
    report_fatal(format, args, binaryCore, r_state(state, :dumped_core))
    {:ok, r_state(state, dumped_core: true)}
  end

  defp handle_system_event(
         {:inconsistent_database, reason, node},
         state
       ) do
    report_error('mnesia_event got {inconsistent_database, ~tw, ~w}~n', [reason, node])
    {:ok, state}
  end

  defp handle_system_event({:mnesia_user, event}, state) do
    report_info('User event: ~tp~n', [event])
    {:ok, state}
  end

  defp handle_system_event(msg, state) do
    report_error('mnesia_event got unexpected system event: ~tp~n', [msg])
    {:ok, state}
  end

  defp report_info(format0, args0) do
    format = 'Mnesia(~p): ' ++ format0
    args = [node() | args0]

    case :global.whereis_name(:mnesia_global_logger) do
      :undefined ->
        :io.format(format, args)

      pid ->
        :io.format(pid, format, args)
    end
  end

  defp report_warning(format0, args0) do
    format = 'Mnesia(~p): ** WARNING ** ' ++ format0
    args = [node() | args0]

    case :erlang.function_exported(:error_logger, :warning_msg, 2) do
      true ->
        :error_logger.warning_msg(format, args)

      false ->
        :error_logger.format(format, args)
    end

    case :global.whereis_name(:mnesia_global_logger) do
      :undefined ->
        :ok

      pid ->
        :io.format(pid, format, args)
    end
  end

  defp report_error(format0, args0) do
    format = 'Mnesia(~p): ** ERROR ** ' ++ format0
    args = [node() | args0]
    :error_logger.format(format, args)

    case :global.whereis_name(:mnesia_global_logger) do
      :undefined ->
        :ok

      pid ->
        :io.format(pid, format, args)
    end
  end

  defp report_fatal(format, args, binaryCore, coreDumped) do
    useDir = :mnesia_monitor.use_dir()
    coreDir = :mnesia_monitor.get_env(:core_dir)

    cond do
      is_list(coreDir) and coreDumped == false and
          is_binary(binaryCore) ->
        core_file(coreDir, binaryCore, format, args)

      useDir == true and coreDumped == false and
          is_binary(binaryCore) ->
        core_file(coreDir, binaryCore, format, args)

      true ->
        report_error('(ignoring core) ** FATAL ** ' ++ format, args)
    end
  end

  defp core_file(coreDir, binaryCore, format, args) do
    integers = :erlang.tuple_to_list(:erlang.timestamp())

    fun = fn
      i when i < 10 ->
        ['_0', i]

      i ->
        ['_', i]
    end

    list =
      :lists.append(
        for i <- integers do
          fun.(i)
        end
      )

    coreFile =
      cond do
        is_list(coreDir) ->
          :filename.absname(
            :lists.concat(['MnesiaCore.', node()] ++ list),
            coreDir
          )

        true ->
          :filename.absname(:lists.concat(['MnesiaCore.', node()] ++ list))
      end

    case :file.write_file(coreFile, binaryCore) do
      :ok ->
        report_error('(core dumped to file: ~p)~n ** FATAL ** ' ++ format, [coreFile] ++ args)

      {:error, reason} ->
        report_error('(could not write core file: ~p)~n ** FATAL ** ' ++ format, [reason] ++ args)
    end
  end
end
