defmodule :m_sys do
  use Bitwise

  def suspend(name) do
    send_system_msg(name, :suspend)
  end

  def suspend(name, timeout) do
    send_system_msg(name, :suspend, timeout)
  end

  def resume(name) do
    send_system_msg(name, :resume)
  end

  def resume(name, timeout) do
    send_system_msg(name, :resume, timeout)
  end

  def get_status(name) do
    send_system_msg(name, :get_status)
  end

  def get_status(name, timeout) do
    send_system_msg(name, :get_status, timeout)
  end

  def get_state(name) do
    case send_system_msg(name, :get_state) do
      {:error, reason} ->
        :erlang.error(reason)

      state ->
        state
    end
  end

  def get_state(name, timeout) do
    case send_system_msg(name, :get_state, timeout) do
      {:error, reason} ->
        :erlang.error(reason)

      state ->
        state
    end
  end

  def replace_state(name, stateFun) do
    case send_system_msg(
           name,
           {:replace_state, stateFun}
         ) do
      {:error, reason} ->
        :erlang.error(reason)

      state ->
        state
    end
  end

  def replace_state(name, stateFun, timeout) do
    case send_system_msg(name, {:replace_state, stateFun}, timeout) do
      {:error, reason} ->
        :erlang.error(reason)

      state ->
        state
    end
  end

  def change_code(name, mod, vsn, extra) do
    send_system_msg(name, {:change_code, mod, vsn, extra})
  end

  def change_code(name, mod, vsn, extra, timeout) do
    send_system_msg(name, {:change_code, mod, vsn, extra}, timeout)
  end

  def terminate(name, reason) do
    send_system_msg(name, {:terminate, reason})
  end

  def terminate(name, reason, timeout) do
    send_system_msg(name, {:terminate, reason}, timeout)
  end

  def log(name, flag) do
    send_system_msg(name, {:debug, {:log, flag}})
  end

  def log(name, flag, timeout) do
    send_system_msg(name, {:debug, {:log, flag}}, timeout)
  end

  def trace(name, flag) do
    send_system_msg(name, {:debug, {:trace, flag}})
  end

  def trace(name, flag, timeout) do
    send_system_msg(name, {:debug, {:trace, flag}}, timeout)
  end

  def log_to_file(name, fileName) do
    send_system_msg(
      name,
      {:debug, {:log_to_file, fileName}}
    )
  end

  def log_to_file(name, fileName, timeout) do
    send_system_msg(name, {:debug, {:log_to_file, fileName}}, timeout)
  end

  def statistics(name, flag) do
    send_system_msg(name, {:debug, {:statistics, flag}})
  end

  def statistics(name, flag, timeout) do
    send_system_msg(name, {:debug, {:statistics, flag}}, timeout)
  end

  def no_debug(name) do
    send_system_msg(name, {:debug, :no_debug})
  end

  def no_debug(name, timeout) do
    send_system_msg(name, {:debug, :no_debug}, timeout)
  end

  def install(name, {func, funcState}) do
    send_system_msg(
      name,
      {:debug, {:install, {func, funcState}}}
    )
  end

  def install(name, {funcId, func, funcState}) do
    send_system_msg(
      name,
      {:debug, {:install, {funcId, func, funcState}}}
    )
  end

  def install(name, {func, funcState}, timeout) do
    send_system_msg(name, {:debug, {:install, {func, funcState}}}, timeout)
  end

  def install(name, {funcId, func, funcState}, timeout) do
    send_system_msg(
      name,
      {:debug, {:install, {funcId, func, funcState}}},
      timeout
    )
  end

  def remove(name, funcOrFuncId) do
    send_system_msg(name, {:debug, {:remove, funcOrFuncId}})
  end

  def remove(name, funcOrFuncId, timeout) do
    send_system_msg(name, {:debug, {:remove, funcOrFuncId}}, timeout)
  end

  defp send_system_msg(name, request) do
    case (try do
            :gen.call(name, :system, request)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, res} ->
        res

      {:EXIT, reason} ->
        exit({reason, mfa(name, request)})
    end
  end

  defp send_system_msg(name, request, timeout) do
    case (try do
            :gen.call(name, :system, request, timeout)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, res} ->
        res

      {:EXIT, reason} ->
        exit({reason, mfa(name, request, timeout)})
    end
  end

  defp mfa(name, {:debug, {func, arg2}}) do
    {:sys, func, [name, arg2]}
  end

  defp mfa(name, {:change_code, mod, vsn, extra}) do
    {:sys, :change_code, [name, mod, vsn, extra]}
  end

  defp mfa(name, {:terminate, reason}) do
    {:sys, :terminate, [name, reason]}
  end

  defp mfa(name, atom) do
    {:sys, atom, [name]}
  end

  defp mfa(name, req, timeout) do
    {m, f, a} = mfa(name, req)
    {m, f, a ++ [timeout]}
  end

  def handle_system_msg(msg, from, parent, module, debug, misc) do
    handle_system_msg(:running, msg, from, parent, module, debug, misc, false)
  end

  def handle_system_msg(msg, from, parent, mod, debug, misc, hib) do
    handle_system_msg(:running, msg, from, parent, mod, debug, misc, hib)
  end

  defp handle_system_msg(sysState, msg, from, parent, mod, debug, misc, hib) do
    case do_cmd(sysState, msg, parent, mod, debug, misc) do
      {:suspended, reply, nDebug, nMisc} ->
        _ = :gen.reply(from, reply)
        suspend_loop(:suspended, parent, mod, nDebug, nMisc, hib)

      {:running, reply, nDebug, nMisc} ->
        _ = :gen.reply(from, reply)
        mod.system_continue(parent, nDebug, nMisc)

      {{:terminating, reason}, reply, nDebug, nMisc} ->
        _ = :gen.reply(from, reply)
        mod.system_terminate(reason, parent, nDebug, nMisc)
    end
  end

  def handle_debug([{:trace, true} = dbgOpt | t], formFunc, state, event) do
    print_event({event, state, formFunc})
    [dbgOpt | handle_debug(t, formFunc, state, event)]
  end

  def handle_debug([{:log, nLog} | t], formFunc, state, event) do
    item = {event, state, formFunc}
    [{:log, nlog_put(item, nLog)} | handle_debug(t, formFunc, state, event)]
  end

  def handle_debug([{:log_to_file, fd} = dbgOpt | t], formFunc, state, event) do
    print_event(fd, {event, state, formFunc})
    [dbgOpt | handle_debug(t, formFunc, state, event)]
  end

  def handle_debug([{:statistics, statData} | t], formFunc, state, event) do
    nStatData = stat(event, statData)
    [{:statistics, nStatData} | handle_debug(t, formFunc, state, event)]
  end

  def handle_debug([{funcId, {func, funcState}} | t], formFunc, state, event) do
    try do
      func.(funcState, event, state)
    catch
      :done ->
        handle_debug(t, formFunc, state, event)

      nFuncState ->
        [{funcId, {func, nFuncState}} | handle_debug(t, formFunc, state, event)]

      _, _ ->
        handle_debug(t, formFunc, state, event)
    else
      :done ->
        handle_debug(t, formFunc, state, event)

      nFuncState ->
        [{funcId, {func, nFuncState}} | handle_debug(t, formFunc, state, event)]
    end
  end

  def handle_debug([{func, funcState} | t], formFunc, state, event) do
    try do
      func.(funcState, event, state)
    catch
      :done ->
        handle_debug(t, formFunc, state, event)

      nFuncState ->
        [{func, nFuncState} | handle_debug(t, formFunc, state, event)]

      _, _ ->
        handle_debug(t, formFunc, state, event)
    else
      :done ->
        handle_debug(t, formFunc, state, event)

      nFuncState ->
        [{func, nFuncState} | handle_debug(t, formFunc, state, event)]
    end
  end

  def handle_debug([], _FormFunc, _State, _Event) do
    []
  end

  defp suspend_loop(sysState, parent, mod, debug, misc, hib) do
    case hib do
      true ->
        suspend_loop_hib(sysState, parent, mod, debug, misc, hib)

      _ ->
        receive do
          {:system, from, msg} ->
            handle_system_msg(sysState, msg, from, parent, mod, debug, misc, hib)

          {:EXIT, ^parent, reason} ->
            mod.system_terminate(reason, parent, debug, misc)
        end
    end
  end

  def suspend_loop_hib(sysState, parent, mod, debug, misc, hib) do
    receive do
      {:system, from, msg} ->
        handle_system_msg(sysState, msg, from, parent, mod, debug, misc, hib)

      {:EXIT, ^parent, reason} ->
        mod.system_terminate(reason, parent, debug, misc)
    after
      0 ->
        :proc_lib.hibernate(:sys, :suspend_loop_hib, [sysState, parent, mod, debug, misc, hib])
    end
  end

  defp do_cmd(_, :suspend, _Parent, _Mod, debug, misc) do
    {:suspended, :ok, debug, misc}
  end

  defp do_cmd(_, :resume, _Parent, _Mod, debug, misc) do
    {:running, :ok, debug, misc}
  end

  defp do_cmd(sysState, :get_state, _Parent, mod, debug, misc) do
    {sysState, do_get_state(mod, misc), debug, misc}
  end

  defp do_cmd(sysState, {:replace_state, stateFun}, _Parent, mod, debug, misc) do
    {res, nMisc} = do_replace_state(stateFun, mod, misc)
    {sysState, res, debug, nMisc}
  end

  defp do_cmd(sysState, :get_status, parent, mod, debug, misc) do
    res = get_status(sysState, parent, mod, debug, misc)
    {sysState, res, debug, misc}
  end

  defp do_cmd(sysState, {:debug, what}, _Parent, _Mod, debug, misc) do
    {res, nDebug} = debug_cmd(what, debug)
    {sysState, res, nDebug, misc}
  end

  defp do_cmd(_, {:terminate, reason}, _Parent, _Mod, debug, misc) do
    {{:terminating, reason}, :ok, debug, misc}
  end

  defp do_cmd(:suspended, {:change_code, module, vsn, extra}, _Parent, mod, debug, misc) do
    {res, nMisc} = do_change_code(mod, module, vsn, extra, misc)
    {:suspended, res, debug, nMisc}
  end

  defp do_cmd(sysState, other, _Parent, _Mod, debug, misc) do
    {sysState, {:error, {:unknown_system_msg, other}}, debug, misc}
  end

  defp do_get_state(mod, misc) do
    case :erlang.function_exported(mod, :system_get_state, 1) do
      true ->
        try do
          {:ok, state} = mod.system_get_state(misc)
          state
        catch
          cl, exc ->
            {:error, {:callback_failed, {mod, :system_get_state}, {cl, exc}}}
        end

      false ->
        misc
    end
  end

  defp do_replace_state(stateFun, mod, misc) do
    case :erlang.function_exported(mod, :system_replace_state, 2) do
      true ->
        try do
          {:ok, state, nMisc} =
            mod.system_replace_state(
              stateFun,
              misc
            )

          {state, nMisc}
        catch
          cl, exc ->
            {{:error, {:callback_failed, {mod, :system_replace_state}, {cl, exc}}}, misc}
        end

      false ->
        try do
          nMisc = stateFun.(misc)
          {nMisc, nMisc}
        catch
          cl, exc ->
            {{:error, {:callback_failed, stateFun, {cl, exc}}}, misc}
        end
    end
  end

  defp get_status(sysState, parent, mod, debug, misc) do
    pDict = :erlang.get()

    fmtMisc =
      case :erlang.function_exported(mod, :format_status, 2) do
        true ->
          fmtArgs = [pDict, sysState, parent, debug, misc]
          mod.format_status(:normal, fmtArgs)

        _ ->
          misc
      end

    {:status, self(), {:module, mod}, [pDict, sysState, parent, debug, fmtMisc]}
  end

  defp debug_cmd({:trace, true}, debug) do
    {:ok, install_debug(:trace, true, debug)}
  end

  defp debug_cmd({:trace, false}, debug) do
    {:ok, remove_debug(:trace, debug)}
  end

  defp debug_cmd({:log, true}, debug) do
    nLog = get_debug(:log, debug, nlog_new())
    {:ok, install_debug(:log, nlog_new(nLog), debug)}
  end

  defp debug_cmd({:log, {true, n}}, debug)
       when is_integer(n) and 1 <= n do
    nLog = get_debug(:log, debug, nlog_new(n))
    {:ok, install_debug(:log, nlog_new(n, nLog), debug)}
  end

  defp debug_cmd({:log, false}, debug) do
    {:ok, remove_debug(:log, debug)}
  end

  defp debug_cmd({:log, :print}, debug) do
    print_log(debug)
    {:ok, debug}
  end

  defp debug_cmd({:log, :get}, debug) do
    nLog = get_debug(:log, debug, nlog_new())

    {{:ok,
      for {event, _State, _FormFunc} <- nlog_get(nLog) do
        event
      end}, debug}
  end

  defp debug_cmd({:log_to_file, false}, debug) do
    nDebug = close_log_file(debug)
    {:ok, nDebug}
  end

  defp debug_cmd({:log_to_file, fileName}, debug) do
    nDebug = close_log_file(debug)

    case :file.open(
           fileName,
           [:write, {:encoding, :utf8}]
         ) do
      {:ok, fd} ->
        {:ok, install_debug(:log_to_file, fd, nDebug)}

      _Error ->
        {{:error, :open_file}, nDebug}
    end
  end

  defp debug_cmd({:statistics, true}, debug) do
    {:ok, install_debug(:statistics, init_stat(), debug)}
  end

  defp debug_cmd({:statistics, false}, debug) do
    {:ok, remove_debug(:statistics, debug)}
  end

  defp debug_cmd({:statistics, :get}, debug) do
    {{:ok, get_stat(get_debug(:statistics, debug, []))}, debug}
  end

  defp debug_cmd(:no_debug, debug) do
    close_log_file(debug)
    {:ok, []}
  end

  defp debug_cmd({:install, {func, funcState}}, debug) do
    {:ok, install_debug(func, funcState, debug)}
  end

  defp debug_cmd({:install, {funcId, func, funcState}}, debug) do
    {:ok, install_debug(funcId, {func, funcState}, debug)}
  end

  defp debug_cmd({:remove, funcOrFuncId}, debug) do
    {:ok, remove_debug(funcOrFuncId, debug)}
  end

  defp debug_cmd(_Unknown, debug) do
    {:unknown_debug, debug}
  end

  defp do_change_code(mod, module, vsn, extra, misc) do
    case (try do
            mod.system_code_change(misc, module, vsn, extra)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, nMisc} ->
        {:ok, nMisc}

      else__ ->
        {{:error, else__}, misc}
    end
  end

  defp print_event(x) do
    print_event(:standard_io, x)
  end

  defp print_event(dev, {event, state, formFunc}) do
    formFunc.(dev, event, state)
  end

  defp init_stat() do
    {:erlang.localtime(), :erlang.process_info(self(), :reductions), 0, 0}
  end

  defp get_stat({time, {:reductions, reds}, in__, out}) do
    {:reductions, reds2} =
      :erlang.process_info(
        self(),
        :reductions
      )

    [
      {:start_time, time},
      {:current_time, :erlang.localtime()},
      {:reductions, reds2 - reds},
      {:messages_in, in__},
      {:messages_out, out}
    ]
  end

  defp get_stat(_) do
    :no_statistics
  end

  defp stat({:in, _Msg}, {time, reds, in__, out}) do
    {time, reds, in__ + 1, out}
  end

  defp stat({:in, _Msg, _From}, {time, reds, in__, out}) do
    {time, reds, in__ + 1, out}
  end

  defp stat({:out, _Msg, _To}, {time, reds, in__, out}) do
    {time, reds, in__, out + 1}
  end

  defp stat(
         {:out, _Msg, _To, _State},
         {time, reds, in__, out}
       ) do
    {time, reds, in__, out + 1}
  end

  defp stat(_, statData) do
    statData
  end

  defp install_debug(item, data, debug) do
    case :lists.keysearch(item, 1, debug) do
      false ->
        [{item, data} | debug]

      _ ->
        debug
    end
  end

  defp remove_debug(item, debug) do
    :lists.keydelete(item, 1, debug)
  end

  def get_debug(item, debug, default) do
    get_debug2(item, debug, default)
  end

  defp get_debug2(item, debug, default) do
    case :lists.keysearch(item, 1, debug) do
      {:value, {^item, data}} ->
        data

      _ ->
        default
    end
  end

  def print_log(debug) do
    nLog = get_debug(:log, debug, nlog_new())
    :lists.foreach(&print_event/1, nlog_get(nLog))
  end

  def get_log(debug) do
    nLog = get_debug(:log, debug, nlog_new())

    for {event, _State, _FormFunc} <- nlog_get(nLog) do
      event
    end
  end

  defp close_log_file(debug) do
    case get_debug2(:log_to_file, debug, []) do
      [] ->
        debug

      fd ->
        :ok = :file.close(fd)
        remove_debug(:log_to_file, debug)
    end
  end

  defp nlog_new() do
    nlog_new(10)
  end

  defp nlog_new([_ | _] = nLog) do
    nlog_new(10, nLog)
  end

  defp nlog_new(n) do
    [n]
  end

  defp nlog_new(n, nLog) do
    :lists.foldl(
      fn item, nL ->
        nlog_put(item, nL)
      end,
      nlog_new(n),
      nlog_get(nLog)
    )
  end

  defp nlog_put(item, nLog) do
    case nLog do
      [r | fF] when is_list(r) ->
        case fF do
          [_ | f] ->
            [[item | r] | f]

          [] ->
            [_ | f] = :lists.reverse(r, [item])
            [[] | f]
        end

      [1 | r] ->
        [[item | r]]

      [j | r] ->
        [[j - 1, item] | r]
    end
  end

  defp nlog_get([[] | f]) do
    f
  end

  defp nlog_get([[_ | _] = r | f]) do
    f ++ :lists.reverse(r)
  end

  defp nlog_get([_J | r]) do
    :lists.reverse(r)
  end

  def debug_options(options) do
    debug_options(options, [])
  end

  defp debug_options([:trace | t], debug) do
    debug_options(t, install_debug(:trace, true, debug))
  end

  defp debug_options([:log | t], debug) do
    debug_options(t, install_debug(:log, nlog_new(), debug))
  end

  defp debug_options([{:log, n} | t], debug)
       when is_integer(n) and
              n > 0 do
    debug_options(
      t,
      install_debug(:log, nlog_new(n), debug)
    )
  end

  defp debug_options([:statistics | t], debug) do
    debug_options(
      t,
      install_debug(:statistics, init_stat(), debug)
    )
  end

  defp debug_options([{:log_to_file, fileName} | t], debug) do
    case :file.open(
           fileName,
           [:write, {:encoding, :utf8}]
         ) do
      {:ok, fd} ->
        debug_options(t, install_debug(:log_to_file, fd, debug))

      _Error ->
        debug_options(t, debug)
    end
  end

  defp debug_options([{:install, {func, funcState}} | t], debug) do
    debug_options(t, install_debug(func, funcState, debug))
  end

  defp debug_options(
         [{:install, {funcId, func, funcState}} | t],
         debug
       ) do
    debug_options(
      t,
      install_debug(funcId, {func, funcState}, debug)
    )
  end

  defp debug_options([_ | t], debug) do
    debug_options(t, debug)
  end

  defp debug_options([], debug) do
    debug
  end
end
