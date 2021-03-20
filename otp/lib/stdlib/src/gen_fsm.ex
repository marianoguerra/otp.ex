defmodule :m_gen_fsm do
  use Bitwise

  def start(mod, args, options) do
    :gen.start(:gen_fsm, :nolink, mod, args, options)
  end

  def start(name, mod, args, options) do
    :gen.start(:gen_fsm, :nolink, name, mod, args, options)
  end

  def start_link(mod, args, options) do
    :gen.start(:gen_fsm, :link, mod, args, options)
  end

  def start_link(name, mod, args, options) do
    :gen.start(:gen_fsm, :link, name, mod, args, options)
  end

  def stop(name) do
    :gen.stop(name)
  end

  def stop(name, reason, timeout) do
    :gen.stop(name, reason, timeout)
  end

  def send_event({:global, name}, event) do
    try do
      :global.send(name, {:"$gen_event", event})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def send_event({:via, mod, name}, event) do
    try do
      mod.send(name, {:"$gen_event", event})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def send_event(name, event) do
    send(name, {:"$gen_event", event})
    :ok
  end

  def sync_send_event(name, event) do
    case (try do
            :gen.call(name, :"$gen_sync_event", event)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, res} ->
        res

      {:EXIT, reason} ->
        exit({reason, {:gen_fsm, :sync_send_event, [name, event]}})
    end
  end

  def sync_send_event(name, event, timeout) do
    case (try do
            :gen.call(name, :"$gen_sync_event", event, timeout)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, res} ->
        res

      {:EXIT, reason} ->
        exit({reason, {:gen_fsm, :sync_send_event, [name, event, timeout]}})
    end
  end

  def send_all_state_event({:global, name}, event) do
    try do
      :global.send(name, {:"$gen_all_state_event", event})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def send_all_state_event({:via, mod, name}, event) do
    try do
      mod.send(name, {:"$gen_all_state_event", event})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def send_all_state_event(name, event) do
    send(name, {:"$gen_all_state_event", event})
    :ok
  end

  def sync_send_all_state_event(name, event) do
    case (try do
            :gen.call(name, :"$gen_sync_all_state_event", event)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, res} ->
        res

      {:EXIT, reason} ->
        exit({reason, {:gen_fsm, :sync_send_all_state_event, [name, event]}})
    end
  end

  def sync_send_all_state_event(name, event, timeout) do
    case (try do
            :gen.call(name, :"$gen_sync_all_state_event", event, timeout)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, res} ->
        res

      {:EXIT, reason} ->
        exit({reason, {:gen_fsm, :sync_send_all_state_event, [name, event, timeout]}})
    end
  end

  def start_timer(time, msg) do
    :erlang.start_timer(time, self(), {:"$gen_timer", msg})
  end

  def send_event_after(time, event) do
    :erlang.start_timer(time, self(), {:"$gen_event", event})
  end

  def cancel_timer(ref) do
    case :erlang.cancel_timer(ref) do
      false ->
        receive do
          {:timeout, ^ref, _} ->
            0
        after
          0 ->
            false
        end

      remainingTime ->
        remainingTime
    end
  end

  def enter_loop(mod, options, stateName, stateData) do
    enter_loop(mod, options, stateName, stateData, self(), :infinity)
  end

  def enter_loop(mod, options, stateName, stateData, {scope, _} = serverName)
      when scope == :local or scope == :global do
    enter_loop(mod, options, stateName, stateData, serverName, :infinity)
  end

  def enter_loop(mod, options, stateName, stateData, {:via, _, _} = serverName) do
    enter_loop(mod, options, stateName, stateData, serverName, :infinity)
  end

  def enter_loop(mod, options, stateName, stateData, timeout) do
    enter_loop(mod, options, stateName, stateData, self(), timeout)
  end

  def enter_loop(mod, options, stateName, stateData, serverName, timeout) do
    name = :gen.get_proc_name(serverName)
    parent = :gen.get_parent()
    debug = :gen.debug_options(name, options)
    hibernateAfterTimeout = :gen.hibernate_after(options)
    loop(parent, name, stateName, stateData, mod, timeout, hibernateAfterTimeout, debug)
  end

  def init_it(starter, :self, name, mod, args, options) do
    init_it(starter, self(), name, mod, args, options)
  end

  def init_it(starter, parent, name0, mod, args, options) do
    name = :gen.name(name0)
    debug = :gen.debug_options(name, options)
    hibernateAfterTimeout = :gen.hibernate_after(options)

    case (try do
            mod.init(args)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, stateName, stateData} ->
        :proc_lib.init_ack(starter, {:ok, self()})
        loop(parent, name, stateName, stateData, mod, :infinity, hibernateAfterTimeout, debug)

      {:ok, stateName, stateData, timeout} ->
        :proc_lib.init_ack(starter, {:ok, self()})
        loop(parent, name, stateName, stateData, mod, timeout, hibernateAfterTimeout, debug)

      {:stop, reason} ->
        :gen.unregister_name(name0)
        :proc_lib.init_ack(starter, {:error, reason})
        exit(reason)

      :ignore ->
        :gen.unregister_name(name0)
        :proc_lib.init_ack(starter, :ignore)
        exit(:normal)

      {:EXIT, reason} ->
        :gen.unregister_name(name0)
        :proc_lib.init_ack(starter, {:error, reason})
        exit(reason)

      else__ ->
        error = {:bad_return_value, else__}
        :proc_lib.init_ack(starter, {:error, error})
        exit(error)
    end
  end

  defp loop(parent, name, stateName, stateData, mod, :hibernate, hibernateAfterTimeout, debug) do
    :proc_lib.hibernate(:gen_fsm, :wake_hib, [
      parent,
      name,
      stateName,
      stateData,
      mod,
      hibernateAfterTimeout,
      debug
    ])
  end

  defp loop(parent, name, stateName, stateData, mod, :infinity, hibernateAfterTimeout, debug) do
    receive do
      msg ->
        decode_msg(
          msg,
          parent,
          name,
          stateName,
          stateData,
          mod,
          :infinity,
          hibernateAfterTimeout,
          debug,
          false
        )
    after
      hibernateAfterTimeout ->
        loop(parent, name, stateName, stateData, mod, :hibernate, hibernateAfterTimeout, debug)
    end
  end

  defp loop(parent, name, stateName, stateData, mod, time, hibernateAfterTimeout, debug) do
    msg =
      receive do
        input ->
          input
      after
        time ->
          {:"$gen_event", :timeout}
      end

    decode_msg(
      msg,
      parent,
      name,
      stateName,
      stateData,
      mod,
      time,
      hibernateAfterTimeout,
      debug,
      false
    )
  end

  def wake_hib(parent, name, stateName, stateData, mod, hibernateAfterTimeout, debug) do
    msg =
      receive do
        input ->
          input
      end

    decode_msg(
      msg,
      parent,
      name,
      stateName,
      stateData,
      mod,
      :hibernate,
      hibernateAfterTimeout,
      debug,
      true
    )
  end

  defp decode_msg(
         msg,
         parent,
         name,
         stateName,
         stateData,
         mod,
         time,
         hibernateAfterTimeout,
         debug,
         hib
       ) do
    case msg do
      {:system, from, req} ->
        :sys.handle_system_msg(
          req,
          from,
          parent,
          :gen_fsm,
          debug,
          [name, stateName, stateData, mod, time, hibernateAfterTimeout],
          hib
        )

      {:EXIT, ^parent, reason} ->
        terminate(reason, name, :undefined, msg, mod, stateName, stateData, debug)

      _Msg when debug === [] ->
        handle_msg(msg, parent, name, stateName, stateData, mod, time, hibernateAfterTimeout)

      _Msg ->
        debug1 = :sys.handle_debug(debug, &print_event/3, name, {:in, msg, stateName})

        handle_msg(
          msg,
          parent,
          name,
          stateName,
          stateData,
          mod,
          time,
          hibernateAfterTimeout,
          debug1
        )
    end
  end

  def system_continue(parent, debug, [
        name,
        stateName,
        stateData,
        mod,
        time,
        hibernateAfterTimeout
      ]) do
    loop(parent, name, stateName, stateData, mod, time, hibernateAfterTimeout, debug)
  end

  def system_terminate(reason, _Parent, debug, [
        name,
        stateName,
        stateData,
        mod,
        _Time,
        _HibernateAfterTimeout
      ]) do
    terminate(reason, name, :undefined, [], mod, stateName, stateData, debug)
  end

  def system_code_change(
        [name, stateName, stateData, mod, time, hibernateAfterTimeout],
        _Module,
        oldVsn,
        extra
      ) do
    case (try do
            mod.code_change(oldVsn, stateName, stateData, extra)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newStateName, newStateData} ->
        {:ok, [name, newStateName, newStateData, mod, time, hibernateAfterTimeout]}

      else__ ->
        else__
    end
  end

  def system_get_state([_Name, stateName, stateData, _Mod, _Time, _HibernateAfterTimeout]) do
    {:ok, {stateName, stateData}}
  end

  def system_replace_state(
        stateFun,
        [name, stateName, stateData, mod, time, hibernateAfterTimeout]
      ) do
    result = {nStateName, nStateData} = stateFun.({stateName, stateData})
    {:ok, result, [name, nStateName, nStateData, mod, time, hibernateAfterTimeout]}
  end

  defp print_event(dev, {:in, msg, stateName}, name) do
    case msg do
      {:"$gen_event", event} ->
        :io.format(dev, '*DBG* ~tp got event ~tp in state ~tw~n', [name, event, stateName])

      {:"$gen_all_state_event", event} ->
        :io.format(dev, '*DBG* ~tp got all_state_event ~tp in state ~tw~n', [
          name,
          event,
          stateName
        ])

      {:"$gen_sync_event", {from, _Tag}, event} ->
        :io.format(dev, '*DBG* ~tp got sync_event ~tp from ~tw in state ~tw~n', [
          name,
          event,
          from,
          stateName
        ])

      {:"$gen_sync_all_state_event", {from, _Tag}, event} ->
        :io.format(dev, '*DBG* ~tp got sync_all_state_event ~tp from ~tw in state ~tw~n', [
          name,
          event,
          from,
          stateName
        ])

      {:timeout, ref, {:"$gen_timer", message}} ->
        :io.format(dev, '*DBG* ~tp got timer ~tp in state ~tw~n', [
          name,
          {:timeout, ref, message},
          stateName
        ])

      {:timeout, _Ref, {:"$gen_event", event}} ->
        :io.format(dev, '*DBG* ~tp got timer ~tp in state ~tw~n', [name, event, stateName])

      _ ->
        :io.format(dev, '*DBG* ~tp got ~tp in state ~tw~n', [name, msg, stateName])
    end
  end

  defp print_event(dev, {:out, msg, {to, _Tag}, stateName}, name) do
    :io.format(dev, '*DBG* ~tp sent ~tp to ~tw~n      and switched to state ~tw~n', [
      name,
      msg,
      to,
      stateName
    ])
  end

  defp print_event(dev, {:noreply, stateName}, name) do
    :io.format(dev, '*DBG* ~tp switched to state ~tw~n', [name, stateName])
  end

  defp handle_msg(msg, parent, name, stateName, stateData, mod, _Time, hibernateAfterTimeout) do
    from = from(msg)

    case (try do
            dispatch(msg, mod, stateName, stateData)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:next_state, nStateName, nStateData} ->
        loop(parent, name, nStateName, nStateData, mod, :infinity, hibernateAfterTimeout, [])

      {:next_state, nStateName, nStateData, time1} ->
        loop(parent, name, nStateName, nStateData, mod, time1, hibernateAfterTimeout, [])

      {:reply, reply, nStateName, nStateData}
      when from !== :undefined ->
        reply(from, reply)
        loop(parent, name, nStateName, nStateData, mod, :infinity, hibernateAfterTimeout, [])

      {:reply, reply, nStateName, nStateData, time1}
      when from !== :undefined ->
        reply(from, reply)
        loop(parent, name, nStateName, nStateData, mod, time1, hibernateAfterTimeout, [])

      {:stop, reason, nStateData} ->
        terminate(reason, name, from, msg, mod, stateName, nStateData, [])

      {:stop, reason, reply, nStateData}
      when from !== :undefined ->
        {:EXIT, r} =
          try do
            terminate(reason, name, from, msg, mod, stateName, nStateData, [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        reply(from, reply)
        exit(r)

      {:EXIT, {:undef, [{^mod, :handle_info, [_, _, _], _} | _]}} ->
        case :logger.allow(:warning, :gen_fsm) do
          true ->
            :erlang.apply(:logger, :macro_log, [
              %{
                mfa: {:gen_fsm, :handle_msg, 8},
                line: 498,
                file: 'otp/lib/stdlib/src/gen_fsm.erl'
              },
              :warning,
              %{label: {:gen_fsm, :no_handle_info}, module: mod, message: msg},
              %{
                domain: [:otp],
                report_cb: &:gen_fsm.format_log/2,
                error_logger: %{tag: :warning_msg, report_cb: &:gen_fsm.format_log/1}
              }
            ])

          false ->
            :ok
        end

        loop(parent, name, stateName, stateData, mod, :infinity, hibernateAfterTimeout, [])

      {:EXIT, what} ->
        terminate(what, name, from, msg, mod, stateName, stateData, [])

      reply ->
        terminate({:bad_return_value, reply}, name, from, msg, mod, stateName, stateData, [])
    end
  end

  defp handle_msg(
         msg,
         parent,
         name,
         stateName,
         stateData,
         mod,
         _Time,
         hibernateAfterTimeout,
         debug
       ) do
    from = from(msg)

    case (try do
            dispatch(msg, mod, stateName, stateData)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:next_state, nStateName, nStateData} ->
        debug1 = :sys.handle_debug(debug, &print_event/3, name, {:noreply, nStateName})
        loop(parent, name, nStateName, nStateData, mod, :infinity, hibernateAfterTimeout, debug1)

      {:next_state, nStateName, nStateData, time1} ->
        debug1 = :sys.handle_debug(debug, &print_event/3, name, {:noreply, nStateName})
        loop(parent, name, nStateName, nStateData, mod, time1, hibernateAfterTimeout, debug1)

      {:reply, reply, nStateName, nStateData}
      when from !== :undefined ->
        debug1 = reply(name, from, reply, debug, nStateName)
        loop(parent, name, nStateName, nStateData, mod, :infinity, hibernateAfterTimeout, debug1)

      {:reply, reply, nStateName, nStateData, time1}
      when from !== :undefined ->
        debug1 = reply(name, from, reply, debug, nStateName)
        loop(parent, name, nStateName, nStateData, mod, time1, hibernateAfterTimeout, debug1)

      {:stop, reason, nStateData} ->
        terminate(reason, name, from, msg, mod, stateName, nStateData, debug)

      {:stop, reason, reply, nStateData}
      when from !== :undefined ->
        {:EXIT, r} =
          try do
            terminate(reason, name, from, msg, mod, stateName, nStateData, debug)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        _ = reply(name, from, reply, debug, stateName)
        exit(r)

      {:EXIT, what} ->
        terminate(what, name, from, msg, mod, stateName, stateData, debug)

      reply ->
        terminate({:bad_return_value, reply}, name, from, msg, mod, stateName, stateData, debug)
    end
  end

  defp dispatch({:"$gen_event", event}, mod, stateName, stateData) do
    apply(mod, stateName, [event, stateData])
  end

  defp dispatch({:"$gen_all_state_event", event}, mod, stateName, stateData) do
    mod.handle_event(event, stateName, stateData)
  end

  defp dispatch({:"$gen_sync_event", from, event}, mod, stateName, stateData) do
    apply(mod, stateName, [event, from, stateData])
  end

  defp dispatch({:"$gen_sync_all_state_event", from, event}, mod, stateName, stateData) do
    mod.handle_sync_event(event, from, stateName, stateData)
  end

  defp dispatch({:timeout, ref, {:"$gen_timer", msg}}, mod, stateName, stateData) do
    apply(mod, stateName, [{:timeout, ref, msg}, stateData])
  end

  defp dispatch({:timeout, _Ref, {:"$gen_event", event}}, mod, stateName, stateData) do
    apply(mod, stateName, [event, stateData])
  end

  defp dispatch(info, mod, stateName, stateData) do
    mod.handle_info(info, stateName, stateData)
  end

  defp from({:"$gen_sync_event", from, _Event}) do
    from
  end

  defp from({:"$gen_sync_all_state_event", from, _Event}) do
    from
  end

  defp from(_) do
    :undefined
  end

  def reply({to, tag}, reply) do
    try do
      send(to, {tag, reply})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp reply(name, from, reply, debug, stateName) do
    reply(from, reply)
    :sys.handle_debug(debug, &print_event/3, name, {:out, reply, from, stateName})
  end

  defp terminate(reason, name, from, msg, mod, stateName, stateData, debug) do
    case :erlang.function_exported(mod, :terminate, 3) do
      true ->
        case (try do
                mod.terminate(reason, stateName, stateData)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, r} ->
            fmtStateData = format_status(:terminate, mod, :erlang.get(), stateData)
            error_info(r, name, from, msg, stateName, fmtStateData, debug)
            exit(r)

          _ ->
            :ok
        end

      false ->
        :ok
    end

    case reason do
      :normal ->
        exit(:normal)

      :shutdown ->
        exit(:shutdown)

      {:shutdown, _} = shutdown ->
        exit(shutdown)

      _ ->
        fmtStateData1 = format_status(:terminate, mod, :erlang.get(), stateData)
        error_info(reason, name, from, msg, stateName, fmtStateData1, debug)
        exit(reason)
    end
  end

  defp error_info(reason, name, from, msg, stateName, stateData, debug) do
    log = :sys.get_log(debug)

    case :logger.allow(:error, :gen_fsm) do
      true ->
        :erlang.apply(:logger, :macro_log, [
          %{mfa: {:gen_fsm, :error_info, 7}, line: 612, file: 'otp/lib/stdlib/src/gen_fsm.erl'},
          :error,
          %{
            label: {:gen_fsm, :terminate},
            name: name,
            last_message: msg,
            state_name: stateName,
            state_data: stateData,
            log: log,
            reason: reason,
            client_info: client_stacktrace(from)
          },
          %{
            domain: [:otp],
            report_cb: &:gen_fsm.format_log/2,
            error_logger: %{tag: :error, report_cb: &:gen_fsm.format_log/1}
          }
        ])

      false ->
        :ok
    end

    :ok
  end

  defp client_stacktrace(:undefined) do
    :undefined
  end

  defp client_stacktrace({pid, _Tag}) do
    client_stacktrace(pid)
  end

  defp client_stacktrace(pid)
       when is_pid(pid) and
              node(pid) === node() do
    case :erlang.process_info(
           pid,
           [:current_stacktrace, :registered_name]
         ) do
      :undefined ->
        {pid, :dead}

      [{:current_stacktrace, stacktrace}, {:registered_name, []}] ->
        {pid, {pid, stacktrace}}

      [{:current_stacktrace, stacktrace}, {:registered_name, name}] ->
        {pid, {name, stacktrace}}
    end
  end

  defp client_stacktrace(pid) when is_pid(pid) do
    {pid, :remote}
  end

  def format_log(report) do
    depth = :error_logger.get_format_depth()
    formatOpts = %{chars_limit: :unlimited, depth: depth, single_line: false, encoding: :utf8}

    format_log_multi(
      limit_report(report, depth),
      formatOpts
    )
  end

  defp limit_report(report, :unlimited) do
    report
  end

  defp limit_report(
         %{
           label: {:gen_fsm, :terminate},
           last_message: msg,
           state_data: stateData,
           log: log,
           reason: reason,
           client_info: clientInfo
         } = report,
         depth
       ) do
    Map.merge(report, %{
      last_message: :io_lib.limit_term(msg, depth),
      state_data: :io_lib.limit_term(stateData, depth),
      log:
        for l <- log do
          :io_lib.limit_term(l, depth)
        end,
      reason: :io_lib.limit_term(reason, depth),
      client_info: limit_client_report(clientInfo, depth)
    })
  end

  defp limit_report(
         %{label: {:gen_fsm, :no_handle_info}, message: msg} = report,
         depth
       ) do
    Map.put(report, :message, :io_lib.limit_term(msg, depth))
  end

  defp limit_client_report({from, {name, stacktrace}}, depth) do
    {from, {name, :io_lib.limit_term(stacktrace, depth)}}
  end

  defp limit_client_report(client, _) do
    client
  end

  def format_log(report, formatOpts0) do
    default = %{chars_limit: :unlimited, depth: :unlimited, single_line: false, encoding: :utf8}
    formatOpts = :maps.merge(default, formatOpts0)

    ioOpts =
      case formatOpts do
        %{chars_limit: :unlimited} ->
          []

        %{chars_limit: limit} ->
          [{:chars_limit, limit}]
      end

    {format, args} = format_log_single(report, formatOpts)
    :io_lib.format(format, args, ioOpts)
  end

  defp format_log_single(
         %{
           label: {:gen_fsm, :terminate},
           name: name,
           last_message: msg,
           state_name: stateName,
           state_data: stateData,
           log: log,
           reason: reason,
           client_info: clientInfo
         },
         %{single_line: true, depth: depth} = formatOpts
       ) do
    p = p(formatOpts)
    fixedReason = fix_reason(reason)
    {clientFmt, clientArgs} = format_client_log_single(clientInfo, p, depth)

    format =
      :lists.append([
        'State machine ',
        p,
        ' terminating. Reason: ',
        p,
        '. Last event: ',
        p,
        '. State: ',
        p,
        '. Data: ',
        p,
        case log do
          [] ->
            ''

          _ ->
            '. Log: ' ++ p
        end,
        '.'
      ])

    args0 =
      [name, fixedReason, get_msg(msg), stateName, stateData] ++
        case log do
          [] ->
            []

          _ ->
            [log]
        end

    args =
      case depth do
        :unlimited ->
          args0

        _ ->
          :lists.flatmap(
            fn a ->
              [a, depth]
            end,
            args0
          )
      end

    {format ++ clientFmt, args ++ clientArgs}
  end

  defp format_log_single(
         %{label: {:gen_fsm, :no_handle_info}, module: mod, message: msg},
         %{single_line: true, depth: depth} = formatOpts
       ) do
    p = p(formatOpts)
    format = :lists.append(['Undefined handle_info in ', p, '. Unhandled message: ', p, '.'])

    args =
      case depth do
        :unlimited ->
          [mod, msg]

        _ ->
          [mod, depth, msg, depth]
      end

    {format, args}
  end

  defp format_log_single(report, formatOpts) do
    format_log_multi(report, formatOpts)
  end

  defp format_log_multi(
         %{
           label: {:gen_fsm, :terminate},
           name: name,
           last_message: msg,
           state_name: stateName,
           state_data: stateData,
           log: log,
           reason: reason,
           client_info: clientInfo
         },
         %{depth: depth} = formatOpts
       ) do
    p = p(formatOpts)
    fixedReason = fix_reason(reason)
    {clientFmt, clientArgs} = format_client_log(clientInfo, p, depth)

    format =
      :lists.append([
        '** State machine ',
        p,
        ' terminating \n' ++
          get_msg_str(
            msg,
            p
          ) ++ '** When State == ',
        p,
        '~n',
        '**      Data  == ',
        p,
        '~n',
        '** Reason for termination ==~n** ',
        p,
        '~n',
        case log do
          [] ->
            []

          _ ->
            '** Log ==~n**' ++ p ++ '~n'
        end
      ])

    args0 =
      [name | get_msg(msg)] ++
        [
          stateName,
          stateData,
          fixedReason
          | case log do
              [] ->
                []

              _ ->
                [log]
            end
        ]

    args =
      case depth do
        :unlimited ->
          args0

        _ ->
          :lists.flatmap(
            fn a ->
              [a, depth]
            end,
            args0
          )
      end

    {format ++ clientFmt, args ++ clientArgs}
  end

  defp format_log_multi(
         %{label: {:gen_fsm, :no_handle_info}, module: mod, message: msg},
         %{depth: depth} = formatOpts
       ) do
    p = p(formatOpts)
    format = '** Undefined handle_info in ~p~n** Unhandled message: ' ++ p ++ '~n'

    args =
      case depth do
        :unlimited ->
          [mod, msg]

        _ ->
          [mod, msg, depth]
      end

    {format, args}
  end

  defp fix_reason({:undef, [{m, f, a, l} | mFAs]} = reason) do
    case :code.is_loaded(m) do
      false ->
        {:"module could not be loaded", [{m, f, a, l} | mFAs]}

      _ ->
        case :erlang.function_exported(m, f, length(a)) do
          true ->
            reason

          false ->
            {:"function not exported", [{m, f, a, l} | mFAs]}
        end
    end
  end

  defp fix_reason(reason) do
    reason
  end

  defp get_msg_str({:"$gen_event", _Event}, p) do
    '** Last event in was ' ++ p ++ '~n'
  end

  defp get_msg_str({:"$gen_sync_event", _From, _Event}, p) do
    '** Last sync event in was ' ++ p ++ ' from ~tw~n'
  end

  defp get_msg_str({:"$gen_all_state_event", _Event}, p) do
    '** Last event in was ' ++ p ++ ' (for all states)~n'
  end

  defp get_msg_str({:"$gen_sync_all_state_event", _From, _Event}, p) do
    '** Last sync event in was ' ++ p ++ ' (for all states) from ' ++ p ++ '~n'
  end

  defp get_msg_str({:timeout, _Ref, {:"$gen_timer", _Msg}}, p) do
    '** Last timer event in was ' ++ p ++ '~n'
  end

  defp get_msg_str({:timeout, _Ref, {:"$gen_event", _Msg}}, p) do
    '** Last timer event in was ' ++ p ++ '~n'
  end

  defp get_msg_str(_Msg, p) do
    '** Last message in was ' ++ p ++ '~n'
  end

  defp get_msg({:"$gen_event", event}) do
    [event]
  end

  defp get_msg({:"$gen_sync_event", {from, _Tag}, event}) do
    [event, from]
  end

  defp get_msg({:"$gen_all_state_event", event}) do
    [event]
  end

  defp get_msg({:"$gen_sync_all_state_event", {from, _Tag}, event}) do
    [event, from]
  end

  defp get_msg({:timeout, ref, {:"$gen_timer", msg}}) do
    [{:timeout, ref, msg}]
  end

  defp get_msg({:timeout, _Ref, {:"$gen_event", event}}) do
    [event]
  end

  defp get_msg(msg) do
    [msg]
  end

  defp format_client_log_single(:undefined, _, _) do
    {'', []}
  end

  defp format_client_log_single({pid, :dead}, _, _) do
    {' Client ~0p is dead.', [pid]}
  end

  defp format_client_log_single({pid, :remote}, _, _) do
    {' Client ~0p is remote on node ~0p.', [pid, node(pid)]}
  end

  defp format_client_log_single({_Pid, {name, stacktrace0}}, p, depth) do
    stacktrace = :lists.sublist(stacktrace0, 4)
    format = :lists.append([' Client ', p, ' stacktrace: ', p, '.'])

    args =
      case depth do
        :unlimited ->
          [name, stacktrace]

        _ ->
          [name, depth, stacktrace, depth]
      end

    {format, args}
  end

  defp format_client_log(:undefined, _, _) do
    {'', []}
  end

  defp format_client_log({pid, :dead}, _, _) do
    {'** Client ~p is dead~n', [pid]}
  end

  defp format_client_log({pid, :remote}, _, _) do
    {'** Client ~p is remote on node ~p~n', [pid, node(pid)]}
  end

  defp format_client_log({_Pid, {name, stacktrace}}, p, depth) do
    format = :lists.append(['** Client ', p, ' stacktrace~n** ', p, '~n'])

    args =
      case depth do
        :unlimited ->
          [name, stacktrace]

        _ ->
          [name, depth, stacktrace, depth]
      end

    {format, args}
  end

  defp p(%{single_line: single, depth: depth, encoding: enc}) do
    '~' ++ single(single) ++ mod(enc) ++ p(depth)
  end

  defp p(:unlimited) do
    'p'
  end

  defp p(_Depth) do
    'P'
  end

  defp single(true) do
    '0'
  end

  defp single(false) do
    ''
  end

  defp mod(:latin1) do
    ''
  end

  defp mod(_) do
    't'
  end

  def format_status(opt, statusData) do
    [
      pDict,
      sysState,
      parent,
      debug,
      [name, stateName, stateData, mod, _Time, _HibernateAfterTimeout]
    ] = statusData

    header = :gen.format_status_header('Status for state machine', name)
    log = :sys.get_log(debug)

    specific =
      case format_status(opt, mod, pDict, stateData) do
        s when is_list(s) ->
          s

        s ->
          [s]
      end

    [
      {:header, header},
      {:data,
       [
         {'Status', sysState},
         {'Parent', parent},
         {'Logged events', log},
         {'StateName', stateName}
       ]}
      | specific
    ]
  end

  defp format_status(opt, mod, pDict, state) do
    defStatus =
      case opt do
        :terminate ->
          state

        _ ->
          [{:data, [{'StateData', state}]}]
      end

    case :erlang.function_exported(mod, :format_status, 2) do
      true ->
        case (try do
                mod.format_status(opt, [pDict, state])
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            defStatus

          else__ ->
            else__
        end

      _ ->
        defStatus
    end
  end
end
