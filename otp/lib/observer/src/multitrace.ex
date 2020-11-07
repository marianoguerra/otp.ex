defmodule :m_multitrace do
  use Bitwise
  @author :"siri@erix.ericsson.se"
  def debug(func) do
    case running() do
      false ->
        {:ok, _} =
          :ttb.tracer(
            :all,
            [{:file, 'debug_trace'}, {:handler, {{:multitrace, :handle_debug}, :initial}}]
          )

        init()
        {:ok, _} = :ttb.p(:all, [:timestamp, :c])
        tp(func)
        :ok

      true ->
        {:error, :tracer_already_running}
    end
  end

  defp tp([func | funcs]) do
    tp(func)
    tp(funcs)
  end

  defp tp([]) do
    :ok
  end

  defp tp({m, f, a}) do
    do_tp(m, f, a)
  end

  defp tp({m, f}) do
    do_tp(m, f, :_)
  end

  defp tp(m) do
    do_tp(m, :_, :_)
  end

  defp do_tp(m, f, a) do
    {:ok, _} = :ttb.tp(m, f, a, [{:_, [], [{:message, {:caller}}, {:return_trace}]}])
  end

  def gc(proc) do
    case running() do
      false ->
        {:ok, _} =
          :ttb.tracer(
            :all,
            [
              {:file, 'gc_trace'},
              {:handler, {{:multitrace, :handle_gc}, :initial}},
              {:process_info, false}
            ]
          )

        init()

        {:ok, _} =
          :ttb.p(
            proc,
            [:timestamp, :garbage_collection]
          )

        :ok

      true ->
        {:error, :tracer_already_running}
    end
  end

  def schedule(proc) do
    case running() do
      false ->
        {:ok, _} =
          :ttb.tracer(
            :all,
            [
              {:file, 'schedule_trace'},
              {:handler, {{:multitrace, :handle_schedule}, :initial}},
              {:process_info, false}
            ]
          )

        init()
        {:ok, _} = :ttb.p(proc, [:timestamp, :running])
        :ok

      true ->
        {:error, :tracer_already_running}
    end
  end

  def stop() do
    :ttb.stop()
  end

  def format(file) do
    :ttb.format(file)
  end

  def format(file, out) do
    :ttb.format(file, [{:out, out}])
  end

  def handle_debug(out, trace, tI, :initial) do
    print_header(out, tI)
    handle_debug(out, trace, tI, 0)
  end

  def handle_debug(_Out, :end_of_trace, _TI, n) do
    n
  end

  def handle_debug(out, trace, _TI, n) do
    print_func(out, trace, n)
    n + 1
  end

  defp print_func(out, {:trace_ts, p, :call, {m, f, a}, c, ts}, n) do
    :io.format(
      out,
      '~w: ~s~nProcess   : ~w~nCall      : ~w:~tw/~w~nArguments : ~tp~nCaller    : ~tw~n~n',
      [n, ts(ts), p, m, f, length(a), a, c]
    )
  end

  defp print_func(out, {:trace_ts, p, :return_from, {m, f, a}, r, ts}, n) do
    :io.format(
      out,
      '~w: ~s~nProcess      : ~w~nReturn from  : ~w:~tw/~w~nReturn value : ~tp~n~n',
      [n, ts(ts), p, m, f, a, r]
    )
  end

  def handle_gc(_Out, :end_of_trace, _TI, s) do
    s
  end

  def handle_gc(out, trace, tI, :initial) do
    print_header(out, tI)
    print_gc_header(out)
    handle_gc(out, trace, tI, :dict.new())
  end

  def handle_gc(_Out, {:trace_ts, p, :gc_start, info, ts}, _TI, s) do
    :dict.store(p, {info, ts}, s)
  end

  def handle_gc(out, {:trace_ts, p, :gc_end, info, ts}, _TI, s) do
    case :dict.find(p, s) do
      {:ok, {startInfo, startTime}} ->
        {eM, eR, eS, eO, eH, eOB, eB} = sort(info)
        {sM, sR, sS, sO, sH, sOB, sB} = sort(startInfo)

        :io.format(
          out,
          'start\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w~nend\t~w\t~w\t~w\t~w\t~w\t~w\t~w\t~w~n~n',
          [sM, sR, sS, sO, sH, sOB, sB, p, eM, eR, eS, eO, eH, eOB, eB, diff(startTime, ts)]
        )

        :dict.erase(p, s)

      :error ->
        s
    end
  end

  defp print_gc_header(out) do
    :io.format(
      out,
      '\tMBuf\tRecent\tStack\tOldHeap\tHeap\tOldBL\tBlock\tProcess/Time(micro sec)~n=========================================================================================~n',
      []
    )
  end

  defp sort(gC) do
    sort(gC, {0, 0, 0, 0, 0, :_, :_})
  end

  defp sort(
         [{:mbuf_size, m} | rest],
         {_, r, s, o, h, oB, b}
       ) do
    sort(rest, {m, r, s, o, h, oB, b})
  end

  defp sort(
         [{:recent_size, r} | rest],
         {m, _, s, o, h, oB, b}
       ) do
    sort(rest, {m, r, s, o, h, oB, b})
  end

  defp sort(
         [{:stack_size, s} | rest],
         {m, r, _, o, h, oB, b}
       ) do
    sort(rest, {m, r, s, o, h, oB, b})
  end

  defp sort(
         [{:old_heap_size, o} | rest],
         {m, r, s, _, h, oB, b}
       ) do
    sort(rest, {m, r, s, o, h, oB, b})
  end

  defp sort(
         [{:heap_size, h} | rest],
         {m, r, s, o, _, oB, b}
       ) do
    sort(rest, {m, r, s, o, h, oB, b})
  end

  defp sort(
         [{:old_heap_block_size, oB} | rest],
         {m, r, s, o, h, _, b}
       ) do
    sort(rest, {m, r, s, o, h, oB, b})
  end

  defp sort(
         [{:heap_block_size, b} | rest],
         {m, r, s, o, h, oB, _}
       ) do
    sort(rest, {m, r, s, o, h, oB, b})
  end

  defp sort([], gC) do
    gC
  end

  def handle_schedule(out, trace, tI, :initial) do
    print_header(out, tI)
    handle_schedule(out, trace, tI, [])
  end

  def handle_schedule(out, :end_of_trace, _TI, s) do
    summary(out, s)
  end

  def handle_schedule(out, {:trace_ts, p, :out, info, ts}, _TI, s) do
    :io.format(out, 'out:~nProcess  : ~w~nTime     : ~s~nFunction : ~tw~n~n', [p, ts(ts), info])

    case :lists.keysearch(p, 1, s) do
      {:value, {^p, list}} ->
        :lists.keyreplace(p, 1, s, {p, [{:out, ts} | list]})

      false ->
        [{p, [{:out, ts}]} | s]
    end
  end

  def handle_schedule(out, {:trace_ts, p, :in, info, ts}, _TI, s) do
    :io.format(out, 'in:~nProcess  : ~w~nTime     : ~s~nFunction : ~tw~n~n', [p, ts(ts), info])

    case :lists.keysearch(p, 1, s) do
      {:value, {^p, list}} ->
        :lists.keyreplace(p, 1, s, {p, [{:in, ts} | list]})

      false ->
        [{p, [{:in, ts}]} | s]
    end
  end

  defp summary(out, [{p, list} | rest]) do
    sum = proc_summary(list, 0)
    :io.format(out, 'Total time \'in\' for process ~w: ~w micro seconds~n', [p, sum])
    summary(out, rest)
  end

  defp summary(_Out, []) do
    :ok
  end

  defp proc_summary([{:in, _Start} | rest], acc) do
    proc_summary(rest, acc)
  end

  defp proc_summary([[{:out, end__}, {:in, start}] | rest], acc) do
    diff = diff(start, end__)
    proc_summary(rest, acc + diff)
  end

  defp proc_summary([], acc) do
    acc
  end

  defp ts({_, _, micro} = now) do
    {{y, m, d}, {h, min, s}} = :calendar.now_to_local_time(now)

    :io_lib.format('~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w,~6.6.0w', [
      y,
      m,
      d,
      h,
      min,
      s,
      micro
    ])
  end

  defp diff({sMeg, sS, sMic}, {eMeg, eS, eMic}) do
    (eMeg - sMeg) * 1_000_000_000_000 + (eS - sS) * 1_000_000 + (eMic - sMic)
  end

  defp init() do
    :ttb.write_trace_info(
      :start_time,
      fn ->
        :erlang.now()
      end
    )
  end

  defp print_header(out, tI) do
    {:value, {:node, [node]}} = :lists.keysearch(:node, 1, tI)
    {:value, {:flags, flags}} = :lists.keysearch(:flags, 1, tI)

    case :lists.keysearch(:start_time, 1, tI) do
      {:value, {:start_time, [sT]}} ->
        :io.format(out, '~nTracing started on node ~w at ~s~nFlags: ~p~n~n~n', [
          node,
          ts(sT),
          flags
        ])

      false ->
        :io.format(out, '~nTracing from node ~w~nFlags: ~p~n~n~n', [node, flags])
    end
  end

  defp running() do
    case :erlang.whereis(:ttb) do
      :undefined ->
        false

      _Pid ->
        true
    end
  end
end
