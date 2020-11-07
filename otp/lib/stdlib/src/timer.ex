defmodule :m_timer do
  use Bitwise

  def apply_after(time, m, f, a) do
    req(:apply_after, {time, {m, f, a}})
  end

  def send_after(time, pid, message) do
    req(
      :apply_after,
      {time, {:timer, :send, [pid, message]}}
    )
  end

  def send_after(time, message) do
    send_after(time, self(), message)
  end

  def exit_after(time, pid, reason) do
    req(
      :apply_after,
      {time, {:erlang, :exit, [pid, reason]}}
    )
  end

  def exit_after(time, reason) do
    exit_after(time, self(), reason)
  end

  def kill_after(time, pid) do
    exit_after(time, pid, :kill)
  end

  def kill_after(time) do
    exit_after(time, self(), :kill)
  end

  def apply_interval(time, m, f, a) do
    req(:apply_interval, {time, self(), {m, f, a}})
  end

  def send_interval(time, pid, message) do
    req(
      :apply_interval,
      {time, pid, {:timer, :send, [pid, message]}}
    )
  end

  def send_interval(time, message) do
    send_interval(time, self(), message)
  end

  def cancel(bRef) do
    req(:cancel, bRef)
  end

  def sleep(t) do
    receive do
    after
      t ->
        :ok
    end
  end

  def tc(f) do
    t1 = :erlang.monotonic_time()
    val = f.()
    t2 = :erlang.monotonic_time()
    time = :erlang.convert_time_unit(t2 - t1, :native, :microsecond)
    {time, val}
  end

  def tc(f, a) do
    t1 = :erlang.monotonic_time()
    val = apply(f, a)
    t2 = :erlang.monotonic_time()
    time = :erlang.convert_time_unit(t2 - t1, :native, :microsecond)
    {time, val}
  end

  def tc(m, f, a) do
    t1 = :erlang.monotonic_time()
    val = apply(m, f, a)
    t2 = :erlang.monotonic_time()
    time = :erlang.convert_time_unit(t2 - t1, :native, :microsecond)
    {time, val}
  end

  def now_diff({a2, b2, c2}, {a1, b1, c1}) do
    ((a2 - a1) * 1_000_000 + b2 - b1) * 1_000_000 + c2 - c1
  end

  def seconds(seconds) do
    1000 * seconds
  end

  def minutes(minutes) do
    1000 * 60 * minutes
  end

  def hours(hours) do
    1000 * 60 * 60 * hours
  end

  def hms(h, m, s) do
    hours(h) + minutes(m) + seconds(s)
  end

  def start() do
    ensure_started()
  end

  def start_link() do
    :gen_server.start_link({:local, :timer_server}, :timer, [], [])
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)

    :timer_tab =
      :ets.new(
        :timer_tab,
        [:named_table, :ordered_set, :protected]
      )

    :timer_interval_tab =
      :ets.new(
        :timer_interval_tab,
        [:named_table, :protected]
      )

    {:ok, [], :infinity}
  end

  defp ensure_started() do
    case :erlang.whereis(:timer_server) do
      :undefined ->
        c = {:timer_server, {:timer, :start_link, []}, :permanent, 1000, :worker, [:timer]}
        _ = :supervisor.start_child(:kernel_safe_sup, c)
        :ok

      _ ->
        :ok
    end
  end

  defp req(req, arg) do
    sysTime = system_time()
    ensure_started()
    :gen_server.call(:timer_server, {req, arg, sysTime}, :infinity)
  end

  def handle_call({:apply_after, {time, op}, started}, _From, _Ts)
      when is_integer(time) and time >= 0 do
    bRef = {started + 1000 * time, make_ref()}
    timer = {bRef, :timeout, op}
    :ets.insert(:timer_tab, timer)
    timeout = timer_timeout(system_time())
    {:reply, {:ok, bRef}, [], timeout}
  end

  def handle_call({:apply_interval, {time, to, mFA}, started}, _From, _Ts)
      when is_integer(time) and time >= 0 do
    case get_pid(to) do
      pid when is_pid(pid) ->
        try do
          :erlang.link(pid)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        sysTime = system_time()
        ref = make_ref()
        bRef1 = {:interval, ref}
        interval = time * 1000
        bRef2 = {started + interval, ref}
        timer = {bRef2, {:repeat, interval, pid}, mFA}
        :ets.insert(:timer_interval_tab, {bRef1, bRef2, pid})
        :ets.insert(:timer_tab, timer)
        timeout = timer_timeout(sysTime)
        {:reply, {:ok, bRef1}, [], timeout}

      _ ->
        {:reply, {:error, :badarg}, [], next_timeout()}
    end
  end

  def handle_call({:cancel, bRef = {_Time, ref}, _}, _From, ts)
      when is_reference(ref) do
    delete_ref(bRef)
    {:reply, {:ok, :cancel}, ts, next_timeout()}
  end

  def handle_call({:cancel, _BRef, _}, _From, ts) do
    {:reply, {:error, :badarg}, ts, next_timeout()}
  end

  def handle_call({:apply_after, _, _}, _From, ts) do
    {:reply, {:error, :badarg}, ts, next_timeout()}
  end

  def handle_call({:apply_interval, _, _}, _From, ts) do
    {:reply, {:error, :badarg}, ts, next_timeout()}
  end

  def handle_call(_Else, _From, ts) do
    {:noreply, ts, next_timeout()}
  end

  def handle_info(:timeout, ts) do
    timeout = timer_timeout(system_time())
    {:noreply, ts, timeout}
  end

  def handle_info({:EXIT, pid, _Reason}, ts) do
    pid_delete(pid)
    {:noreply, ts, next_timeout()}
  end

  def handle_info(_OtherMsg, ts) do
    {:noreply, ts, next_timeout()}
  end

  def handle_cast(_Req, ts) do
    {:noreply, ts, next_timeout()}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp timer_timeout(sysTime) do
    case :ets.first(:timer_tab) do
      :"$end_of_table" ->
        :infinity

      {time, _Ref} when time > sysTime ->
        timeout = div(time - sysTime + 999, 1000)
        :erlang.min(timeout, 8_388_608)

      key ->
        case :ets.lookup(:timer_tab, key) do
          [{^key, :timeout, mFA}] ->
            :ets.delete(:timer_tab, key)
            do_apply(mFA)
            timer_timeout(sysTime)

          [{{time, ref}, repeat = {:repeat, interv, to}, mFA}] ->
            :ets.delete(:timer_tab, key)
            newTime = time + interv

            :ets.insert(
              :timer_interval_tab,
              {{:interval, ref}, {newTime, ref}, to}
            )

            do_apply(mFA)
            :ets.insert(:timer_tab, {{newTime, ref}, repeat, mFA})
            timer_timeout(sysTime)
        end
    end
  end

  defp delete_ref(bRef = {:interval, _}) do
    case :ets.lookup(:timer_interval_tab, bRef) do
      [{_, bRef2, _Pid}] ->
        :ets.delete(:timer_interval_tab, bRef)
        :ets.delete(:timer_tab, bRef2)

      _ ->
        :ok
    end
  end

  defp delete_ref(bRef) do
    :ets.delete(:timer_tab, bRef)
  end

  defp pid_delete(pid) do
    intervalTimerList =
      :ets.select(
        :timer_interval_tab,
        [{{:_, :_, :"$1"}, [{:==, :"$1", pid}], [:"$_"]}]
      )

    :lists.foreach(
      fn {intKey, timerKey, _} ->
        :ets.delete(:timer_interval_tab, intKey)
        :ets.delete(:timer_tab, timerKey)
      end,
      intervalTimerList
    )
  end

  defp next_timeout() do
    case :ets.first(:timer_tab) do
      :"$end_of_table" ->
        :infinity

      {time, _} ->
        :erlang.min(
          positive(div(time - system_time() + 999, 1000)),
          8_388_608
        )
    end
  end

  defp do_apply({m, f, a}) do
    case {m, f, a} do
      {:timer, :send, ^a} ->
        try do
          send(a)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      {:erlang, :exit, [name, reason]} ->
        try do
          :erlang.exit(get_pid(name), reason)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      _ ->
        try do
          spawn(m, f, a)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end
    end
  end

  defp positive(x) do
    :erlang.max(x, 0)
  end

  defp system_time() do
    :erlang.monotonic_time(1_000_000)
  end

  defp send([pid, msg]) do
    send(pid, msg)
  end

  defp get_pid(name) when is_pid(name) do
    name
  end

  defp get_pid(:undefined) do
    :undefined
  end

  defp get_pid(name) when is_atom(name) do
    get_pid(:erlang.whereis(name))
  end

  defp get_pid(_) do
    :undefined
  end

  def get_status() do
    info1 = :ets.info(:timer_tab)
    {:size, totalNumTimers} = :lists.keyfind(:size, 1, info1)
    info2 = :ets.info(:timer_interval_tab)
    {:size, numIntervalTimers} = :lists.keyfind(:size, 1, info2)
    {{:timer_tab, totalNumTimers}, {:timer_interval_tab, numIntervalTimers}}
  end
end
