defmodule :m_eprof do
  use Bitwise
  @behaviour :gen_server
  require Record
  Record.defrecord(:r_bpd, :bpd, n: 0, us: 0, p: :gb_trees.empty(), mfa: [])

  Record.defrecord(:r_state, :state,
    profiling: false,
    pattern: {:_, :_, :_},
    rootset: [],
    trace_opts: [],
    fd: :undefined,
    start_ts: :undefined,
    reply: :undefined,
    bpd: :EFE_TODO_NESTED_RECORD
  )

  def start() do
    :gen_server.start({:local, :eprof}, :eprof, [], [])
  end

  def stop() do
    :gen_server.call(:eprof, :stop, :infinity)
  end

  def analyze() do
    analyze(:procs)
  end

  def analyze(type) when is_atom(type) do
    analyze(type, [])
  end

  def analyze(opts) when is_list(opts) do
    analyze(:procs, opts)
  end

  def analyze(type, opts) when is_list(opts) do
    :gen_server.call(:eprof, {:analyze, type, opts}, :infinity)
  end

  def profile(rootset) when is_list(rootset) do
    start_profiling(rootset)
  end

  def profile(fun) when is_function(fun) do
    profile([], fun)
  end

  def profile(fun, opts)
      when is_function(fun) and
             is_list(opts) do
    profile([], :erlang, :apply, [fun, []], {:_, :_, :_}, opts)
  end

  def profile(rootset, fun)
      when is_list(rootset) and
             is_function(fun) do
    profile(rootset, fun, {:_, :_, :_})
  end

  def profile(rootset, fun, pattern)
      when is_list(rootset) and
             is_function(fun) do
    profile(rootset, fun, pattern, [{:set_on_spawn, true}])
  end

  def profile(rootset, fun, pattern, options)
      when is_list(rootset) and is_function(fun) and
             is_list(options) do
    profile(rootset, :erlang, :apply, [fun, []], pattern, options)
  end

  def profile(rootset, m, f, a)
      when is_list(rootset) and
             is_atom(m) and is_atom(f) and is_list(a) do
    profile(rootset, m, f, a, {:_, :_, :_})
  end

  def profile(rootset, m, f, a, pattern)
      when is_list(rootset) and is_atom(m) and is_atom(f) and
             is_list(a) do
    profile(rootset, m, f, a, pattern, [{:set_on_spawn, true}])
  end

  def profile(rootset, m, f, a, pattern, options) do
    :ok = start_internal()

    :gen_server.call(
      :eprof,
      {:profile_start, rootset, pattern, {m, f, a}, options},
      :infinity
    )
  end

  def dump() do
    :gen_server.call(:eprof, :dump, :infinity)
  end

  def dump_data() do
    :gen_server.call(:eprof, :dump_data, :infinity)
  end

  def log(file) do
    :gen_server.call(:eprof, {:logfile, file}, :infinity)
  end

  def start_profiling(rootset) do
    start_profiling(rootset, {:_, :_, :_})
  end

  def start_profiling(rootset, pattern) do
    start_profiling(rootset, pattern, [{:set_on_spawn, true}])
  end

  def start_profiling(rootset, pattern, options) do
    :ok = start_internal()

    :gen_server.call(
      :eprof,
      {:profile_start, rootset, pattern, :undefined, options},
      :infinity
    )
  end

  def stop_profiling() do
    :gen_server.call(:eprof, :profile_stop, :infinity)
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    {:ok, r_state()}
  end

  def handle_call({:analyze, _, _}, _, r_state(bpd: r_bpd(p: {0, nil}, us: 0, n: 0)) = s) do
    {:reply, :nothing_to_analyze, s}
  end

  def handle_call({:analyze, :procs, opts}, _, r_state(bpd: bpd, fd: fd) = s)
      when elem(bpd, 0) === :bpd do
    {:reply, analyze(fd, :procs, opts, bpd), s}
  end

  def handle_call({:analyze, :total, opts}, _, r_state(bpd: bpd, fd: fd) = s)
      when elem(bpd, 0) === :bpd do
    {:reply, analyze(fd, :total, opts, bpd), s}
  end

  def handle_call({:analyze, type, _Opts}, _, s) do
    {:reply, {:error, {:undefined, type}}, s}
  end

  def handle_call(
        {:profile_start, _Rootset, _Pattern, _MFA, _Opts},
        _From,
        r_state(profiling: true) = s
      ) do
    {:reply, {:error, :already_profiling}, s}
  end

  def handle_call({:profile_start, rootset, pattern, {m, f, a}, opts}, from, r_state(fd: fd) = s) do
    :ok = set_pattern_trace(false, r_state(s, :pattern))
    _ = set_process_trace(false, r_state(s, :rootset), r_state(s, :trace_opts))
    topts = get_trace_options(opts)
    pid = setup_profiling(m, f, a)

    case set_process_trace(true, [pid | rootset], topts) do
      true ->
        :ok = set_pattern_trace(true, pattern)
        t0 = :erlang.timestamp()
        :ok = execute_profiling(pid)

        {:noreply,
         r_state(
           profiling: true,
           rootset: [pid | rootset],
           start_ts: t0,
           reply: from,
           fd: fd,
           trace_opts: topts,
           pattern: pattern
         )}

      false ->
        :erlang.exit(pid, :eprof_kill)
        {:reply, :error, r_state(fd: fd)}
    end
  end

  def handle_call({:profile_start, rootset, pattern, :undefined, opts}, from, r_state(fd: fd) = s) do
    :ok = set_pattern_trace(false, r_state(s, :pattern))
    true = set_process_trace(false, r_state(s, :rootset), r_state(s, :trace_opts))
    topts = get_trace_options(opts)

    case set_process_trace(true, rootset, topts) do
      true ->
        t0 = :erlang.timestamp()
        :ok = set_pattern_trace(true, pattern)

        {:reply, :profiling,
         r_state(
           profiling: true,
           rootset: rootset,
           start_ts: t0,
           reply: from,
           fd: fd,
           trace_opts: topts,
           pattern: pattern
         )}

      false ->
        {:reply, :error, r_state(fd: fd)}
    end
  end

  def handle_call(:profile_stop, _From, r_state(profiling: false) = s) do
    {:reply, :profiling_already_stopped, s}
  end

  def handle_call(:profile_stop, _From, r_state(profiling: true) = s) do
    :ok = set_pattern_trace(:pause, r_state(s, :pattern))
    bpd = collect_bpd()
    _ = set_process_trace(false, r_state(s, :rootset), r_state(s, :trace_opts))
    :ok = set_pattern_trace(false, r_state(s, :pattern))

    {:reply, :profiling_stopped,
     r_state(s, profiling: false, rootset: [], trace_opts: [], pattern: {:_, :_, :_}, bpd: bpd)}
  end

  def handle_call({:logfile, file}, _From, r_state(fd: oldFd) = s) do
    case :file.open(file, [:write, {:encoding, :utf8}]) do
      {:ok, fd} ->
        case oldFd do
          :undefined ->
            :ok

          ^oldFd ->
            :ok = :file.close(oldFd)
        end

        {:reply, :ok, r_state(s, fd: fd)}

      error ->
        {:reply, error, s}
    end
  end

  def handle_call(:dump, _From, r_state(bpd: bpd) = s)
      when elem(bpd, 0) === :bpd do
    {:reply, :gb_trees.to_list(r_bpd(bpd, :p)), s}
  end

  def handle_call(:dump_data, _, r_state(bpd: r_bpd() = bpd) = s)
      when elem(bpd, 0) === :bpd do
    {:reply, bpd, s}
  end

  def handle_call(:stop, _FromTag, s) do
    {:stop, :normal, :stopped, s}
  end

  def handle_cast(_Msg, state) do
    {:noreply, state}
  end

  def handle_info({:EXIT, _, :normal}, s) do
    {:noreply, s}
  end

  def handle_info({:EXIT, _, :eprof_kill}, s) do
    {:noreply, s}
  end

  def handle_info({:EXIT, _, reason}, r_state(reply: fromTag) = s) do
    _ = set_process_trace(false, r_state(s, :rootset), r_state(s, :trace_opts))
    :ok = set_pattern_trace(false, r_state(s, :pattern))
    :gen_server.reply(fromTag, {:error, reason})
    {:noreply, r_state(s, profiling: false, rootset: [], trace_opts: [], pattern: {:_, :_, :_})}
  end

  def handle_info(
        {_Pid, {:answer, result}},
        r_state(reply: {from, _} = fromTag) = s
      ) do
    :ok = set_pattern_trace(:pause, r_state(s, :pattern))
    bpd = collect_bpd()
    _ = set_process_trace(false, r_state(s, :rootset), r_state(s, :trace_opts))
    :ok = set_pattern_trace(false, r_state(s, :pattern))

    try do
      :erlang.unlink(from)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :gen_server.reply(fromTag, {:ok, result})

    {:noreply,
     r_state(s, profiling: false, rootset: [], trace_opts: [], pattern: {:_, :_, :_}, bpd: bpd)}
  end

  def terminate(_Reason, r_state(fd: :undefined)) do
    :ok = set_pattern_trace(false, {:_, :_, :_})
    :ok
  end

  def terminate(_Reason, r_state(fd: fd)) do
    :ok = :file.close(fd)
    :ok = set_pattern_trace(false, {:_, :_, :_})
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp setup_profiling(m, f, a) do
    spawn_link(fn ->
      spin_profile(m, f, a)
    end)
  end

  defp spin_profile(m, f, a) do
    receive do
      {pid, :execute} ->
        send(pid, {self(), {:answer, :erlang.apply(m, f, a)}})
    end
  end

  defp execute_profiling(pid) do
    send(pid, {self(), :execute})
    :ok
  end

  defp get_trace_options([]) do
    [:call]
  end

  defp get_trace_options([{:set_on_spawn, true} | opts]) do
    [:set_on_spawn | get_trace_options(opts)]
  end

  defp get_trace_options([:set_on_spawn | opts]) do
    [:set_on_spawn | get_trace_options(opts)]
  end

  defp get_trace_options([_ | opts]) do
    get_trace_options(opts)
  end

  defp set_pattern_trace(flag, pattern) do
    :erlang.system_flag(:multi_scheduling, :block)
    :erlang.trace_pattern(:on_load, flag, [:call_time])
    :erlang.trace_pattern(pattern, flag, [:call_time])
    :erlang.system_flag(:multi_scheduling, :unblock)
    :ok
  end

  defp set_process_trace(_, [], _) do
    true
  end

  defp set_process_trace(flag, [pid | pids], options) when is_pid(pid) do
    try do
      :erlang.trace(pid, flag, options)
      set_process_trace(flag, pids, options)
    catch
      _, _ ->
        false
    end
  end

  defp set_process_trace(flag, [name | pids], options)
       when is_atom(name) do
    case :erlang.whereis(name) do
      :undefined ->
        set_process_trace(flag, pids, options)

      pid ->
        set_process_trace(flag, [pid | pids], options)
    end
  end

  defp collect_bpd() do
    collect_bpd(
      for m <-
            (for mi <- :code.all_loaded() do
               :erlang.element(1, mi)
             end),
          m !== :eprof do
        m
      end
    )
  end

  defp collect_bpd(ms) when is_list(ms) do
    collect_bpdf(collect_mfas(ms))
  end

  defp collect_mfas(ms) do
    :lists.foldl(
      fn m, mfas ->
        mfas ++
          for {f, a} <- m.module_info(:functions) do
            {m, f, a}
          end
      end,
      [],
      ms
    )
  end

  defp collect_bpdf(mfas) do
    collect_bpdf(mfas, r_bpd())
  end

  defp collect_bpdf([], bpd) do
    bpd
  end

  defp collect_bpdf(
         [mfa | mfas],
         r_bpd(n: n, us: us, p: tree, mfa: code) = bpd
       ) do
    case :erlang.trace_info(mfa, :call_time) do
      {:call_time, []} ->
        collect_bpdf(mfas, bpd)

      {:call_time, data} when is_list(data) ->
        {cTn, cTus, cTree} = collect_bpdfp(mfa, tree, data)

        collect_bpdf(
          mfas,
          r_bpd(bpd, n: cTn + n, us: cTus + us, p: cTree, mfa: [{mfa, {cTn, cTus}} | code])
        )

      {:call_time, false} ->
        collect_bpdf(mfas, bpd)

      {:call_time, _Other} ->
        collect_bpdf(mfas, bpd)
    end
  end

  defp collect_bpdfp(mfa, tree, data) do
    :lists.foldl(
      fn {pid, ni, si, usi}, {pTno, pTuso, to} ->
        time = si * 1_000_000 + usi

        ti1 =
          case :gb_trees.lookup(pid, to) do
            :none ->
              :gb_trees.enter(pid, [{mfa, {ni, time}}], to)

            {:value, pmfas} ->
              :gb_trees.enter(
                pid,
                [{mfa, {ni, time}} | pmfas],
                to
              )
          end

        {pTno + ni, pTuso + time, ti1}
      end,
      {0, 0, tree},
      data
    )
  end

  def analyze(fd, :procs, opts, r_bpd(p: ps, us: tus)) do
    :lists.foreach(
      fn {pid, mfas} ->
        {pn, pus} = sum_bp_total_n_us(mfas)

        format(fd, '~n****** Process ~w    -- ~s % of profiled time *** ~n', [
          pid,
          s('~.2f', [100.0 * divide(pus, tus)])
        ])

        print_bp_mfa(mfas, {pn, pus}, fd, opts)
        :ok
      end,
      :gb_trees.to_list(ps)
    )
  end

  def analyze(fd, :total, opts, r_bpd(mfa: mfas, n: tn, us: tus)) do
    print_bp_mfa(mfas, {tn, tus}, fd, opts)
  end

  defp sort_mfa(bpfs, :mfa) when is_list(bpfs) do
    :lists.sort(
      fn
        {a, _}, {b, _} when a < b ->
          true

        _, _ ->
          false
      end,
      bpfs
    )
  end

  defp sort_mfa(bpfs, :time) when is_list(bpfs) do
    :lists.sort(
      fn
        {_, {_, a}}, {_, {_, b}} when a < b ->
          true

        _, _ ->
          false
      end,
      bpfs
    )
  end

  defp sort_mfa(bpfs, :calls) when is_list(bpfs) do
    :lists.sort(
      fn
        {_, {a, _}}, {_, {b, _}} when a < b ->
          true

        _, _ ->
          false
      end,
      bpfs
    )
  end

  defp sort_mfa(bpfs, _) when is_list(bpfs) do
    sort_mfa(bpfs, :time)
  end

  defp filter_mfa(bpfs, ts) when is_list(ts) do
    filter_mfa(bpfs, [], :proplists.get_value(:calls, ts, 0), :proplists.get_value(:time, ts, 0))
  end

  defp filter_mfa(bpfs, _) do
    bpfs
  end

  defp filter_mfa([], out, _, _) do
    :lists.reverse(out)
  end

  defp filter_mfa([{_, {c, t}} = bpf | bpfs], out, ct, tt)
       when c >= ct and t >= tt do
    filter_mfa(bpfs, [bpf | out], ct, tt)
  end

  defp filter_mfa([_ | bpfs], out, ct, tt) do
    filter_mfa(bpfs, out, ct, tt)
  end

  defp sum_bp_total_n_us(mfas) do
    :lists.foldl(
      fn {_, {ci, usi}}, {co, uso} ->
        {co + ci, uso + usi}
      end,
      {0, 0},
      mfas
    )
  end

  defp string_bp_mfa(mfas, tus) do
    string_bp_mfa(mfas, tus, {0, 0, 0, 0, 0}, [])
  end

  defp string_bp_mfa([], _, ws, strings) do
    {ws, :lists.reverse(strings)}
  end

  defp string_bp_mfa(
         [{mfa, {count, time}} | mfas],
         tus,
         {mfaW, countW, percW, timeW, tpCW},
         strings
       ) do
    smfa = s(mfa)
    scount = s(count)
    stime = s(time)
    sperc = s('~.2f', [100 * divide(time, tus)])
    stpc = s('~.2f', [divide(time, count)])

    string_bp_mfa(
      mfas,
      tus,
      {:erlang.max(mfaW, :string.length(smfa)), :erlang.max(countW, :string.length(scount)),
       :erlang.max(percW, :string.length(sperc)), :erlang.max(timeW, :string.length(stime)),
       :erlang.max(tpCW, :string.length(stpc))},
      [[smfa, scount, sperc, stime, stpc] | strings]
    )
  end

  defp print_bp_mfa(mfas, {tn, tus}, fd, opts) do
    fmfas =
      filter_mfa(
        sort_mfa(
          mfas,
          :proplists.get_value(:sort, opts)
        ),
        :proplists.get_value(:filter, opts)
      )

    {{mfaW, countW, percW, timeW, tpCW}, strs} = string_bp_mfa(fmfas, tus)
    tnStr = s(tn)
    tusStr = s(tus)
    tuspcStr = s('~.2f', [divide(tus, tn)])

    ws =
      {:erlang.max(:string.length('FUNCTION'), mfaW),
       :lists.max([:string.length('CALLS'), countW, :string.length(tnStr)]),
       :erlang.max(:string.length('      %'), percW),
       :lists.max([:string.length('TIME'), timeW, :string.length(tusStr)]),
       :lists.max([:string.length('uS / CALLS'), tpCW, :string.length(tuspcStr)])}

    format(fd, ws, ['FUNCTION', 'CALLS', '      %', 'TIME', 'uS / CALLS'])
    format(fd, ws, ['--------', '-----', '-------', '----', '----------'])

    :lists.foreach(
      fn string ->
        format(fd, ws, string)
      end,
      strs
    )

    format(
      fd,
      ws,
      for n <- :erlang.tuple_to_list(ws) do
        :lists.duplicate(n, ?-)
      end
    )

    format(fd, ws, ['Total:', tnStr, '100.00%', tusStr, tuspcStr])
    :ok
  end

  defp s({m, f, a}) do
    s('~w:~tw/~w', [m, f, a])
  end

  defp s(term) do
    s('~tp', [term])
  end

  defp s(format, terms) do
    :lists.flatten(:io_lib.format(format, terms))
  end

  defp format(fd, {mfaW, countW, percW, timeW, tpCW}, strings) do
    format(
      fd,
      s('~~.~wts  ~~~ws  ~~~ws  ~~~ws  [~~~ws]~~n', [mfaW, countW, percW, timeW, tpCW]),
      strings
    )
  end

  defp format(:undefined, format, strings) do
    :io.format(format, strings)
    :ok
  end

  defp format(fd, format, strings) do
    :io.format(fd, format, strings)
    :io.format(format, strings)
    :ok
  end

  defp divide(_, 0) do
    0.0
  end

  defp divide(t, n) do
    t / n
  end

  defp start_internal() do
    case start() do
      {:ok, _} ->
        :ok

      {:error, {:already_started, _}} ->
        :ok

      error ->
        error
    end
  end
end
