defmodule :m_ttb_et do
  use Bitwise
  @author :"siri@erix.ericsson.se"
  require Record

  Record.defrecord(:r_event, :event,
    detail_level: :undefined,
    trace_ts: :undefined,
    event_ts: :undefined,
    from: :undefined,
    to: :undefined,
    label: :undefined,
    contents: :undefined
  )

  def handler(out, trace, traci, :initial) do
    s = self()

    spawn(fn ->
      init_et(s)
    end)

    receive do
      {:et_started, collector} ->
        :ok
    end

    handler(out, trace, traci, collector)
  end

  def handler(_, :end_of_trace, _Traci, col) do
    get_returns(col)
    :ok
  end

  def handler(_, trace, _Traci, col) do
    {:ok, newCol} = :et_collector.report(col, trace)
    newCol
  end

  defp collector(event) when elem(event, 0) === :event do
    true
  end

  defp collector(trace) do
    :et_selector.parse_event(:undefined, trace)
  end

  defp get_returns(col) do
    fun = fn event, acc ->
      collect_return_info(event, acc, col)
    end

    :et_collector.iterate(col, :last, :"-infinity", fun, :dict.new())
  end

  defp collect_return_info(r_event(label: l, from: pid) = e, acc, _Col)
       when l == :return_to or l == :return_from do
    :dict.update(
      pid,
      fn old ->
        [e | old]
      end,
      [e],
      acc
    )
  end

  defp collect_return_info(r_event(label: :call, from: pid, contents: contents) = e, acc, col) do
    mFA = get_mfarity(contents)

    {caller, newAcc} =
      case :dict.find(pid, acc) do
        {:ok, [r_event(label: :return_from) = retFrom, r_event(label: :return_to) = retTo | rets]} ->
          retToCont = r_event(retTo, :contents)
          c = get_mfarity(retToCont)
          newRetTo = r_event(retTo, contents: retToCont ++ [{:return_from, mFA}])
          retFromCont = r_event(retFrom, :contents)
          newRetFrom = r_event(retFrom, contents: retFromCont ++ [{:return_to, c}])
          :et_collector.report(col, newRetTo)
          :et_collector.report(col, newRetFrom)
          {c, :dict.store(pid, rets, acc)}

        {:ok, [r_event(label: :return_to) = retTo | rets]} ->
          retToCont = r_event(retTo, :contents)
          c = get_mfarity(retToCont)
          newRetTo = r_event(retTo, contents: retToCont ++ [{:return_from, mFA}])
          :et_collector.report(col, newRetTo)
          {c, :dict.store(pid, rets, acc)}

        {:ok, [r_event(label: :return_from) = retFrom | rets]} ->
          c = get_caller(contents)
          retFromCont = r_event(retFrom, :contents)
          newRetFrom = r_event(retFrom, contents: retFromCont ++ [{:return_to, c}])
          :et_collector.report(col, newRetFrom)
          {c, :dict.store(pid, rets, acc)}

        _noreturn ->
          {:nocaller, acc}
      end

    newE = r_event(e, contents: contents ++ [{:caller, caller}])
    :et_collector.report(col, newE)
    newAcc
  end

  defp collect_return_info(_E, acc, _Col) do
    acc
  end

  defp init_et(parent) do
    :erlang.process_flag(:trap_exit, true)

    etOpt = [
      {:active_filter, :processes},
      {:dict_insert, {:filter, :collector}, &collector/1},
      {:dict_insert, {:filter, :processes}, &processes/1},
      {:dict_insert, {:filter, :modules}, &modules/1},
      {:dict_insert, {:filter, :mods_and_procs}, &mods_and_procs/1},
      {:dict_insert, {:filter, :functions}, &functions/1},
      {:dict_insert, {:filter, :funcs_and_procs}, &funcs_and_procs/1},
      {:hide_actions, false},
      {:max_events, :infinity},
      {:max_actors, :infinity}
    ]

    {:ok, viewer} = :et_viewer.start_link(etOpt)
    collector = :et_viewer.get_collector_pid(viewer)
    send(parent, {:et_started, collector})

    receive do
      {:EXIT, ^viewer, :shutdown} ->
        :ok
    end
  end

  defp processes(e0) do
    e = label(e0)
    {{fromProc, fromNode}, {toProc, toNode}} = get_actors(r_event(e, :from), r_event(e, :to))

    {true,
     r_event(e,
       from: :io_lib.format('~tw~n~w', [fromProc, fromNode]),
       to: :io_lib.format('~tw~n~w', [toProc, toNode])
     )}
  end

  defp mods_and_procs(e) do
    actorFun = fn {m, _F, _A}, {proc, node} ->
      :io_lib.format('~w~n~tw~n~w', [m, proc, node])
    end

    calltrace_filter(e, actorFun)
  end

  defp modules(e) do
    actorFun = fn {m, _F, _A}, {_Proc, node} ->
      :io_lib.format('~w~n~w', [m, node])
    end

    calltrace_filter(e, actorFun)
  end

  defp funcs_and_procs(e) do
    actorFun = fn {m, f, a}, {proc, node} ->
      :io_lib.format('~ts~n~tw~n~w', [mfa(m, f, a), proc, node])
    end

    calltrace_filter(e, actorFun)
  end

  defp functions(e) do
    actorFun = fn {m, f, a}, {_Proc, node} ->
      :io_lib.format('~ts~n~w', [mfa(m, f, a), node])
    end

    calltrace_filter(e, actorFun)
  end

  defp calltrace_filter(e, actorFun) do
    {from, to} = get_actors(r_event(e, :from), r_event(e, :to))
    calltrace_filter(e, from, to, actorFun)
  end

  defp calltrace_filter(r_event(label: :call) = e, from, to, actorFun) do
    cont = r_event(e, :contents)
    mFA = get_mfarity(cont)

    case :lists.keysearch(:caller, 1, cont) do
      {:value, {_, {_CM, _CF, _CA} = caller}} ->
        {true,
         r_event(e,
           label: label(:call, mFA),
           from: actorFun.(caller, from),
           to: actorFun.(mFA, to)
         )}

      {:value, {_, _}} ->
        {true,
         r_event(e, label: label(:call, mFA), from: actorFun.(mFA, from), to: actorFun.(mFA, to))}
    end
  end

  defp calltrace_filter(r_event(label: :return_from) = e, from, to, actorFun) do
    cont = r_event(e, :contents)
    mFA = get_mfarity(cont)

    case :lists.keysearch(:return_to, 1, cont) do
      {:value, {_, {_M2, _F2, _A2} = mFA2}} ->
        {true,
         r_event(e,
           label: label(:return_from, mFA),
           from: actorFun.(mFA, from),
           to: actorFun.(mFA2, to)
         )}

      {:value, {_, _}} ->
        {true,
         r_event(e,
           label: label(:return_from, mFA),
           from: actorFun.(mFA, from),
           to: actorFun.(mFA, to)
         )}
    end
  end

  defp calltrace_filter(r_event(label: :return_to) = e, from, to, actorFun) do
    cont = r_event(e, :contents)
    {:value, {_, {_M2, _F2, _A2} = mFA2}} = :lists.keysearch(:return_from, 1, cont)

    case get_mfarity(cont) do
      {_M, _F, _A} = mFA ->
        {true,
         r_event(e,
           label: label(:return_to, mFA),
           from: actorFun.(mFA2, from),
           to: actorFun.(mFA, to)
         )}

      :undefined ->
        {true,
         r_event(e,
           label: 'return_to unknown',
           from: actorFun.(mFA2, from),
           to: actorFun.(mFA2, to)
         )}
    end
  end

  defp calltrace_filter(_E, _From, _To, _ActorFun) do
    false
  end

  defp label(event = r_event(label: l, contents: c)) do
    case :lists.keysearch(:mfa, 1, c) do
      {:value, {:mfa, mFA}} ->
        r_event(event, label: label(l, mFA))

      false ->
        event
    end
  end

  defp label(l, {m, f, a}) do
    label(l, m, f, a)
  end

  defp label(l, other) do
    :io_lib.format('~w ~tw', [l, other])
  end

  defp label(:call, m, f, a) do
    'call ' ++ mfa(m, f, a)
  end

  defp label(:return_from, m, f, a) do
    'return_from ' ++ mfa(m, f, a)
  end

  defp label(:return_to, m, f, a) do
    'return_to ' ++ mfa(m, f, a)
  end

  defp label(:spawn, m, f, a) do
    'spawn ' ++ mfa(m, f, a)
  end

  defp label(:out, m, f, a) do
    'out ' ++ mfa(m, f, a)
  end

  defp label(:in, m, f, a) do
    'in ' ++ mfa(m, f, a)
  end

  defp mfa(m, f, a) do
    :erlang.atom_to_list(m) ++ ':' ++ fa(f, a)
  end

  defp fa(f, a) do
    :erlang.atom_to_list(f) ++ '/' ++ :erlang.integer_to_list(arity(a))
  end

  defp arity(l) when is_list(l) do
    length(l)
  end

  defp arity(i) when is_integer(i) do
    i
  end

  defp get_actors(from, to) do
    case {get_proc(from), get_proc(to)} do
      {{_FP, _FN}, {_TP, _TN}} = r ->
        r

      {{fP, fN}, t} ->
        {{fP, fN}, {t, fN}}

      {f, {tP, tN}} ->
        {{f, tN}, {tP, tN}}

      {f, t} ->
        {{f, :unknown}, {t, :unknown}}
    end
  end

  defp get_proc({_Pid, name, node}) when is_atom(name) do
    {name, node}
  end

  defp get_proc({pid, _initfunc, node}) do
    {pid, node}
  end

  defp get_proc(p) when is_pid(p) or is_port(p) do
    {p, node(p)}
  end

  defp get_proc(p) do
    p
  end

  defp get_mfarity(list) do
    case get_mfa(list) do
      {m, f, a} ->
        {m, f, arity(a)}

      other ->
        other
    end
  end

  defp get_mfa(list) do
    {:value, {:mfa, mFA}} = :lists.keysearch(:mfa, 1, list)
    mFA
  end

  defp get_caller(list) do
    case :lists.keysearch(:pam_result, 1, list) do
      {:value, {:pam_result, {m, f, a}}} ->
        {m, f, arity(a)}

      {:value, {:pam_result, :undefined}} ->
        :undefined

      {:value, {:pam_result, _Other}} ->
        :nocaller

      false ->
        :nocaller
    end
  end
end
