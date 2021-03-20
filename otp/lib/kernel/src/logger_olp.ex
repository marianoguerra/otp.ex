defmodule :m_logger_olp do
  use Bitwise
  @behaviour :gen_server
  def start_link(name, module, args, options0)
      when is_map(options0) do
    options = :maps.merge(get_default_opts(), options0)

    case check_opts(options) do
      :ok ->
        :proc_lib.start_link(:logger_olp, :init, [[name, module, args, options]])

      error ->
        error
    end
  end

  def load({_Name, pid, modeRef}, msg) do
    case get_mode(modeRef) do
      :async ->
        :gen_server.cast(pid, {:"$olp_load", msg})

      :sync ->
        case call(pid, {:"$olp_load", msg}) do
          :ok ->
            :ok

          _Other ->
            :ok
        end

      :drop ->
        :ok
    end
  end

  def info(olp) do
    call(olp, :info)
  end

  def reset(olp) do
    call(olp, :reset)
  end

  def stop({_Name, pid, _ModRef}) do
    stop(pid)
  end

  def stop(pid) do
    _ = :gen_server.call(pid, :stop)
    :ok
  end

  def set_opts(olp, opts) do
    call(olp, {:set_opts, opts})
  end

  def get_opts(olp) do
    call(olp, :get_opts)
  end

  def get_default_opts() do
    %{
      sync_mode_qlen: 10,
      drop_mode_qlen: 200,
      flush_qlen: 1000,
      burst_limit_enable: true,
      burst_limit_max_count: 500,
      burst_limit_window_time: 1000,
      overload_kill_enable: false,
      overload_kill_qlen: 20000,
      overload_kill_mem_size: 3_000_000,
      overload_kill_restart_after: 5000
    }
  end

  def restart(fun) do
    result =
      try do
        fun.()
      catch
        c, r ->
          {:error, {:restart_failed, fun, c, r, __STACKTRACE__}}
      end

    case :logger.allow(:debug, :logger_olp) do
      true ->
        _ =
          :logger_server.do_internal_log(
            :debug,
            %{
              mfa: {:logger_olp, :restart, 1},
              line: 144,
              file: 'otp/lib/kernel/src/logger_olp.erl'
            },
            %{},
            [[{:logger_olp, :restart}, {:result, result}]]
          )

        :ok

      false ->
        :ok
    end

    :ok
  end

  def get_ref() do
    :erlang.get(:olp_ref)
  end

  def get_ref(pidOrName) do
    call(pidOrName, :get_ref)
  end

  def get_pid({_Name, pid, _ModeRef}) do
    pid
  end

  def init([name, module, args, options]) do
    :erlang.register(name, self())
    :erlang.process_flag(:message_queue_data, :off_heap)
    :ok
    modeRef = {:logger_olp, name}
    olpRef = {name, self(), modeRef}
    :erlang.put(:olp_ref, olpRef)

    try do
      module.init(args)
    catch
      _, error ->
        :erlang.unregister(name)
        :proc_lib.init_ack(error)
    else
      {:ok, cBState} ->
        set_mode(modeRef, :async)
        t0 = :erlang.monotonic_time(:microsecond)
        :proc_lib.init_ack({:ok, self(), olpRef})

        state0 =
          Map.merge(options, %{
            id: name,
            idle: true,
            module: module,
            mode_ref: modeRef,
            mode: :async,
            last_qlen: 0,
            last_load_ts: t0,
            burst_win_ts: t0,
            burst_msg_count: 0,
            cb_state: cBState
          })

        state = reset_restart_flag(state0)
        :gen_server.enter_loop(:logger_olp, [], state)

      error ->
        :erlang.unregister(name)
        :proc_lib.init_ack(error)
    end
  end

  def handle_call({:"$olp_load", msg}, _From, state) do
    {result, state1} = do_load(msg, :call, Map.put(state, :idle, false))
    reply_return(result, state1)
  end

  def handle_call(:get_ref, _From, %{id: name, mode_ref: modeRef} = state) do
    reply_return({name, self(), modeRef}, state)
  end

  def handle_call({:set_opts, opts0}, _From, state) do
    opts =
      :maps.merge(
        :maps.with(
          [
            :sync_mode_qlen,
            :drop_mode_qlen,
            :flush_qlen,
            :burst_limit_enable,
            :burst_limit_max_count,
            :burst_limit_window_time,
            :overload_kill_enable,
            :overload_kill_qlen,
            :overload_kill_mem_size,
            :overload_kill_restart_after
          ],
          state
        ),
        opts0
      )

    case check_opts(opts) do
      :ok ->
        reply_return(:ok, :maps.merge(state, opts))

      error ->
        reply_return(error, state)
    end
  end

  def handle_call(:get_opts, _From, state) do
    reply_return(
      :maps.with(
        [
          :sync_mode_qlen,
          :drop_mode_qlen,
          :flush_qlen,
          :burst_limit_enable,
          :burst_limit_max_count,
          :burst_limit_window_time,
          :overload_kill_enable,
          :overload_kill_qlen,
          :overload_kill_mem_size,
          :overload_kill_restart_after
        ],
        state
      ),
      state
    )
  end

  def handle_call(:info, _From, state) do
    reply_return(state, state)
  end

  def handle_call(:reset, _From, %{module: module, cb_state: cBState} = state) do
    state1 = state
    cBState1 = try_callback_call(module, :reset_state, [cBState], cBState)

    reply_return(
      :ok,
      Map.merge(state1, %{
        idle: true,
        last_qlen: 0,
        last_load_ts: :erlang.monotonic_time(:microsecond),
        cb_state: cBState1
      })
    )
  end

  def handle_call(:stop, _From, state) do
    {:stop, {:shutdown, :stopped}, :ok, state}
  end

  def handle_call(msg, from, %{module: module, cb_state: cBState} = state) do
    case try_callback_call(module, :handle_call, [msg, from, cBState]) do
      {:reply, reply, cBState1} ->
        reply_return(reply, Map.put(state, :cb_state, cBState1))

      {:noreply, cBState1} ->
        noreply_return(Map.put(state, :cb_state, cBState1))

      {:stop, reason, reply, cBState1} ->
        {:stop, reason, reply, Map.put(state, :cb_state, cBState1)}

      {:stop, reason, cBState1} ->
        {:stop, reason, Map.put(state, :cb_state, cBState1)}
    end
  end

  def handle_cast({:"$olp_load", msg}, state) do
    {_Result, state1} = do_load(msg, :cast, Map.put(state, :idle, false))
    noreply_return(state1)
  end

  def handle_cast(
        msg,
        %{module: module, cb_state: cBState} = state
      ) do
    case try_callback_call(module, :handle_cast, [msg, cBState]) do
      {:noreply, cBState1} ->
        noreply_return(Map.put(state, :cb_state, cBState1))

      {:stop, reason, cBState1} ->
        {:stop, reason, Map.put(state, :cb_state, cBState1)}
    end
  end

  def handle_info(:timeout, %{mode_ref: modeRef} = state) do
    state1 = notify(:idle, state)
    state2 = maybe_notify_mode_change(:async, state1)

    {:noreply,
     Map.merge(state2, %{idle: true, mode: set_mode(modeRef, :async), burst_msg_count: 0})}
  end

  def handle_info(
        msg,
        %{module: module, cb_state: cBState} = state
      ) do
    case try_callback_call(module, :handle_info, [msg, cBState]) do
      {:noreply, cBState1} ->
        noreply_return(Map.put(state, :cb_state, cBState1))

      {:stop, reason, cBState1} ->
        {:stop, reason, Map.put(state, :cb_state, cBState1)}

      {:load, cBState1} ->
        {_, state1} = do_load(msg, :cast, Map.merge(state, %{idle: false, cb_state: cBState1}))
        noreply_return(state1)
    end
  end

  def terminate(
        {:shutdown, {:overloaded, _QLen, _Mem}},
        %{id: name, module: module, cb_state: cBState, overload_kill_restart_after: restartAfter} =
          state
      ) do
    :erlang.unregister(name)

    case try_callback_call(module, :terminate, [:overloaded, cBState], :ok) do
      {:ok, fun}
      when is_function(fun, 0) and
             is_integer(restartAfter) ->
        set_restart_flag(state)
        _ = :timer.apply_after(restartAfter, :logger_olp, :restart, [fun])
        :ok

      _ ->
        :ok
    end
  end

  def terminate(
        reason,
        %{id: name, module: module, cb_state: cBState}
      ) do
    _ = try_callback_call(module, :terminate, [reason, cBState], :ok)
    :erlang.unregister(name)
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  def call({_Name, pid, _ModeRef}, msg) do
    call(pid, msg)
  end

  def call(server, msg) do
    try do
      :gen_server.call(server, msg)
    catch
      _, {:timeout, _} ->
        {:error, :busy}
    end
  end

  def cast({_Name, pid, _ModeRef}, msg) do
    :gen_server.cast(pid, msg)
  end

  defp do_load(msg, callOrCast, state) do
    t1 = :erlang.monotonic_time(:microsecond)
    state1 = state
    {mode1, qLen, mem, state2} = check_load(state1)
    kill_if_choked(qLen, mem, state2)

    cond do
      mode1 == :flush ->
        flush(t1, state2)

      true ->
        handle_load(mode1, t1, msg, callOrCast, state2)
    end
  end

  defp flush(
         t1,
         state = %{id: _Name, last_load_ts: _T0, mode_ref: modeRef}
       ) do
    newFlushed = flush_load(5000)
    state1 = notify({:flushed, newFlushed}, state)

    {_, qLen1} =
      :erlang.process_info(
        self(),
        :message_queue_len
      )

    :ok
    :ok
    state2 = state1
    state3 = state2
    state4 = maybe_notify_mode_change(:async, state3)

    {:dropped,
     Map.merge(state4, %{mode: set_mode(modeRef, :async), last_qlen: qLen1, last_load_ts: t1})}
  end

  defp handle_load(
         mode,
         t1,
         msg,
         _CallOrCast,
         state = %{
           id: _Name,
           module: module,
           cb_state: cBState,
           last_qlen: lastQLen,
           last_load_ts: _T0
         }
       ) do
    {doWrite, state1} = limit_burst(state)

    {result, lastQLen1, cBState1} =
      cond do
        doWrite ->
          :ok

          cBS =
            try_callback_call(
              module,
              :handle_load,
              [msg, cBState]
            )

          {:ok,
           :erlang.element(
             2,
             :erlang.process_info(
               self(),
               :message_queue_len
             )
           ), cBS}

        true ->
          :ok
          {:dropped, lastQLen, cBState}
      end

    state2 = Map.put(state1, :cb_state, cBState1)
    state3 = Map.put(state2, :mode, mode)
    state4 = state3
    state5 = state4
    state6 = Map.put(%{state5 | last_qlen: lastQLen1}, :last_load_ts, t1)

    state7 =
      case result do
        :ok ->
          s = state6
          s

        _ ->
          state6
      end

    {result, state7}
  end

  defp check_opts(options) when is_map(options) do
    case do_check_opts(:maps.to_list(options)) do
      :ok ->
        case overload_levels_ok(options) do
          true ->
            :ok

          false ->
            faulty =
              :maps.with(
                [:sync_mode_qlen, :drop_mode_qlen, :flush_qlen],
                options
              )

            {:error, {:invalid_olp_levels, faulty}}
        end

      {:error, key, value} ->
        {:error, {:invalid_olp_config, %{key => value}}}
    end
  end

  defp do_check_opts([{:sync_mode_qlen, n} | options])
       when is_integer(n) do
    do_check_opts(options)
  end

  defp do_check_opts([{:drop_mode_qlen, n} | options])
       when is_integer(n) do
    do_check_opts(options)
  end

  defp do_check_opts([{:flush_qlen, n} | options])
       when is_integer(n) do
    do_check_opts(options)
  end

  defp do_check_opts([{:burst_limit_enable, bool} | options])
       when is_boolean(bool) do
    do_check_opts(options)
  end

  defp do_check_opts([{:burst_limit_max_count, n} | options])
       when is_integer(n) do
    do_check_opts(options)
  end

  defp do_check_opts([{:burst_limit_window_time, n} | options])
       when is_integer(n) do
    do_check_opts(options)
  end

  defp do_check_opts([{:overload_kill_enable, bool} | options])
       when is_boolean(bool) do
    do_check_opts(options)
  end

  defp do_check_opts([{:overload_kill_qlen, n} | options])
       when is_integer(n) do
    do_check_opts(options)
  end

  defp do_check_opts([{:overload_kill_mem_size, n} | options])
       when is_integer(n) do
    do_check_opts(options)
  end

  defp do_check_opts([
         {:overload_kill_restart_after, norA}
         | options
       ])
       when is_integer(norA) or norA == :infinity do
    do_check_opts(options)
  end

  defp do_check_opts([{key, value} | _]) do
    {:error, key, value}
  end

  defp do_check_opts([]) do
    :ok
  end

  defp set_restart_flag(%{id: name, module: module}) do
    flag = :erlang.list_to_atom(:lists.concat([module, '_', name, '_restarting']))

    spawn(fn ->
      :erlang.register(flag, self())
      :timer.sleep(:infinity)
    end)

    :ok
  end

  defp reset_restart_flag(%{id: name, module: module} = state) do
    flag = :erlang.list_to_atom(:lists.concat([module, '_', name, '_restarting']))

    case :erlang.whereis(flag) do
      :undefined ->
        state

      pid ->
        :erlang.exit(pid, :kill)
        notify(:restart, state)
    end
  end

  defp check_load(
         state = %{
           id: _Name,
           mode_ref: modeRef,
           mode: mode,
           sync_mode_qlen: syncModeQLen,
           drop_mode_qlen: dropModeQLen,
           flush_qlen: flushQLen
         }
       ) do
    {_, mem} = :erlang.process_info(self(), :memory)
    :ok

    {_, qLen} =
      :erlang.process_info(
        self(),
        :message_queue_len
      )

    :ok

    {mode1, _NewDrops, _NewFlushes} =
      cond do
        qLen >= flushQLen ->
          {:flush, 0, 1}

        qLen >= dropModeQLen ->
          incDrops =
            cond do
              mode == :drop ->
                0

              true ->
                1
            end

          {set_mode(modeRef, :drop), incDrops, 0}

        qLen >= syncModeQLen ->
          {set_mode(modeRef, :sync), 0, 0}

        true ->
          {set_mode(modeRef, :async), 0, 0}
      end

    state1 = state
    state2 = state1
    state3 = state2
    state4 = maybe_notify_mode_change(mode1, state3)
    {mode1, qLen, mem, Map.put(state4, :last_qlen, qLen)}
  end

  defp limit_burst(%{burst_limit_enable: false} = state) do
    {true, state}
  end

  defp limit_burst(
         %{
           burst_win_ts: burstWinT0,
           burst_msg_count: burstMsgCount,
           burst_limit_window_time: burstLimitWinTime,
           burst_limit_max_count: burstLimitMaxCnt
         } = state
       ) do
    cond do
      burstMsgCount >= burstLimitMaxCnt ->
        burstWinT1 = :erlang.monotonic_time(:microsecond)

        case burstWinT1 - burstWinT0 do
          burstCheckTime
          when burstCheckTime < burstLimitWinTime * 1000 ->
            {false, state}

          _BurstCheckTime ->
            {true, Map.merge(state, %{burst_win_ts: burstWinT1, burst_msg_count: 0})}
        end

      true ->
        {true, Map.merge(state, %{burst_win_ts: burstWinT0, burst_msg_count: burstMsgCount + 1})}
    end
  end

  defp kill_if_choked(qLen, mem, %{
         overload_kill_enable: killIfOL,
         overload_kill_qlen: oLKillQLen,
         overload_kill_mem_size: oLKillMem
       }) do
    cond do
      killIfOL and (qLen > oLKillQLen or mem > oLKillMem) ->
        exit({:shutdown, {:overloaded, qLen, mem}})

      true ->
        :ok
    end
  end

  defp flush_load(limit) do
    :erlang.process_flag(:priority, :high)
    flushed = flush_load(0, limit)
    :erlang.process_flag(:priority, :normal)
    flushed
  end

  defp flush_load(limit, limit) do
    limit
  end

  defp flush_load(n, limit) do
    receive do
      {:"$gen_cast", {:"$olp_load", _}} ->
        flush_load(n + 1, limit)

      {:"$gen_call", {pid, mRef}, {:"$olp_load", _}} ->
        send(pid, {mRef, :dropped})
        flush_load(n + 1, limit)

      {:log, _, _, _, _} ->
        flush_load(n + 1, limit)

      {:log, _, _, _} ->
        flush_load(n + 1, limit)
    after
      0 ->
        n
    end
  end

  defp overload_levels_ok(options) do
    sMQL = :maps.get(:sync_mode_qlen, options, 10)
    dMQL = :maps.get(:drop_mode_qlen, options, 200)
    fQL = :maps.get(:flush_qlen, options, 1000)
    dMQL > 1 and sMQL <= dMQL and dMQL <= fQL
  end

  defp get_mode(ref) do
    :persistent_term.get(ref, :async)
  end

  defp set_mode(ref, m) do
    true = is_atom(m)
    :persistent_term.put(ref, m)
    m
  end

  defp maybe_notify_mode_change(:drop, %{mode: mode0} = state)
       when mode0 !== :drop do
    notify({:mode_change, mode0, :drop}, state)
  end

  defp maybe_notify_mode_change(mode1, %{mode: :drop} = state)
       when mode1 == :async or mode1 == :sync do
    notify({:mode_change, :drop, mode1}, state)
  end

  defp maybe_notify_mode_change(_, state) do
    state
  end

  defp notify(
         note,
         %{module: module, cb_state: cBState} = state
       ) do
    cBState1 = try_callback_call(module, :notify, [note, cBState], cBState)
    Map.put(state, :cb_state, cBState1)
  end

  defp try_callback_call(module, function, args) do
    try_callback_call(module, function, args, :"$no_default_return")
  end

  defp try_callback_call(module, function, args, defRet) do
    try do
      apply(module, function, args)
    catch
      r ->
        r

      :error, :undef when defRet !== :"$no_default_return" ->
        case __STACKTRACE__ do
          [{^module, ^function, ^args, _} | _] ->
            defRet

          _ ->
            :erlang.raise(:error, :undef, __STACKTRACE__)
        end
    end
  end

  defp noreply_return(%{idle: true} = state) do
    {:noreply, state}
  end

  defp noreply_return(%{idle: false} = state) do
    {:noreply, state, 100}
  end

  defp reply_return(reply, %{idle: true} = state) do
    {:reply, reply, state}
  end

  defp reply_return(reply, %{idle: false} = state) do
    {:reply, reply, state, 100}
  end
end
