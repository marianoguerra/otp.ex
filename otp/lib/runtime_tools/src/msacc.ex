defmodule :m_msacc do
  use Bitwise
  def available() do
    try do
      [_ | _] = :erlang.statistics(:microstate_accounting)
      true
    catch
      _, _ ->
        false
    end
  end

  def start() do
    :erlang.system_flag(:microstate_accounting, true)
  end

  def stop() do
    :erlang.system_flag(:microstate_accounting, false)
  end

  def reset() do
    :erlang.system_flag(:microstate_accounting, :reset)
  end

  def start(tmo) do
    stop()
    reset()
    start()
    :timer.sleep(tmo)
    stop()
  end

  def to_file(filename) do
    :file.write_file(filename, :io_lib.format('~p.~n', [stats()]))
  end

  def from_file(filename) do
    {:ok, [stats]} = :file.consult(filename)
    stats
  end

  def print() do
    print(stats())
  end

  def print(stats) do
    print(stats, %{})
  end

  def print(stats, options) do
    print(:erlang.group_leader(), stats, options)
  end

  def print(filename, stats, options)
      when is_list(filename) do
    case (:file.open(filename, [:write])) do
      {:ok, d} ->
        print(d, stats, options)
        :file.close(d)
      error ->
        error
    end
  end

  def print(device, stats, options) do
    defaultOpts = %{:system => false}
    print_int(device, stats,
                :maps.merge(defaultOpts, options))
  end

  defp print_int(device,
            [%{:"$type" => :msacc_data, :id => _Id} | _] = stats,
            options) do
    typeStats = stats(:type, stats)
    :io.format(device, '~s',
                 [print_stats_overview(stats, options)])
    :io.format(device, '~s',
                 [print_stats_header(stats, options)])
    :io.format(device, '~s',
                 [print_stats_threads(stats(:realtime, stats), options)])
    :io.format(device, '~s',
                 [print_stats_type(stats(:realtime, typeStats),
                                     options)])
  end

  defp print_int(device, [%{:"$type" => :msacc_data} | _] = stats,
            options) do
    :io.format(device, '~s',
                 [print_stats_header(stats, options)])
    :io.format(device, '~s',
                 [print_stats_type(stats(:realtime, stats), options)])
  end

  defp print_int(device,
            [%{:"$type" => :msacc_stats, :id => _Id} | _] = stats,
            options) do
    :io.format(device, '~s',
                 [print_stats_header(stats, options)])
    :io.format(device, '~s',
                 [print_stats_threads(stats, options)])
    :io.format(device, '~s',
                 [print_stats_type(:msacc.stats(:type, stats), options)])
  end

  defp print_int(device, [%{:"$type" => :msacc_stats} | _] = stats,
            options) do
    :io.format(device, '~s',
                 [print_stats_header(stats, options)])
    :io.format(device, '~s',
                 [print_stats_type(stats, options)])
  end

  def stats() do
    fun = fn f
          k, {perfCount, stateCount} ->
            {f.(k, perfCount), stateCount}
          _K, perfCount ->
            :erlang.convert_time_unit(perfCount, :perf_counter,
                                        1000000)
          end
    usStats = :lists.map(fn %{:counters => cnt} = m ->
                              usCnt = :maps.map(fun, cnt)
                              %{m | :"$type" => :msacc_data, :counters => usCnt}
                         end,
                           :erlang.statistics(:microstate_accounting))
    statssort(usStats)
  end

  def stats(:system_realtime, stats) do
    :lists.foldl(fn %{:counters => cnt}, acc ->
                      get_total(cnt, acc)
                 end,
                   0, stats)
  end

  def stats(:system_runtime, stats) do
    :lists.foldl(fn %{:counters => cnt}, acc ->
                      get_total(:maps.remove(:sleep, cnt), acc)
                 end,
                   0, stats)
  end

  def stats(:realtime, stats) do
    realTime = stats(:system_realtime, stats)
    statssort(for thread <- stats do
                get_thread_perc(thread, realTime)
              end)
  end

  def stats(:runtime, stats) do
    runTime = stats(:system_runtime, stats)
    statssort(for (t = %{:counters => cnt}) <- stats do
                get_thread_perc(%{t
                                  |
                                  :counters => :maps.remove(:sleep, cnt)},
                                  runTime)
              end)
  end

  def stats(:type, stats) do
    statssort(merge_threads(stats, []))
  end

  defp print_stats_overview(stats, _Options) do
    runTime = stats(:system_runtime, stats)
    realTime = div(stats(:system_realtime,
                           stats), length(stats))
    schedStats = (for (%{:type
                         =>
                         :scheduler} = s) <- stats do
                    s
                  end)
    avgSchedRunTime = div(stats(:system_runtime,
                                  schedStats), length(schedStats))
    numSize = (cond do
                 realTime > runTime ->
                   length(:erlang.integer_to_list(realTime))
                 true ->
                   length(:erlang.integer_to_list(runTime))
               end)
    [:io_lib.format('Average thread real-time    : ~*B us~n', [numSize, realTime]),
       :io_lib.format('Accumulated system run-time : ~*B us~n', [numSize, runTime]),
       :io_lib.format('Average scheduler run-time  : ~*B us~n', [numSize, avgSchedRunTime]),
       :io_lib.format('~n', [])]
  end

  defp print_stats_threads(stats, options) do
    [:io_lib.format('~nStats per thread:~n', []),
       for thread <- stats do
         print_thread_info(thread, options)
       end]
  end

  defp print_stats_type(stats, options) do
    [:io_lib.format('~nStats per type:~n', []),
       for thread <- stats do
         print_thread_info(thread, options)
       end]
  end

  defp print_stats_header([%{:counters => cnt} | _],
            %{:system => printSys}) do
    [:io_lib.format('~14s', ['Thread']),
       map(fn counter, _ when printSys ->
                :io_lib.format('~9s     ', [:erlang.atom_to_list(counter)])
              counter, _ ->
                :io_lib.format('~9s', [:erlang.atom_to_list(counter)])
           end,
             cnt),
       :io_lib.format('~n', [])]
  end

  defp print_thread_info(%{:"$type" => :msacc_stats,
              :counters => cnt} = thread,
            %{:system => printSys}) do
    [case (:maps.find(:id, thread)) do
       :error ->
         :io_lib.format('~14s',
                          [:erlang.atom_to_list(:maps.get(:type, thread))])
       {:ok, id} ->
         :io_lib.format('~10s(~2B)',
                          [:erlang.atom_to_list(:maps.get(:type, thread)), id])
     end,
       map(fn _Key,
                %{:thread => threadPerc, :system => systemPerc}
                  when printSys ->
                :io_lib.format('~6.2f%(~4.1f%)', [threadPerc, systemPerc])
              _Key, %{:thread => threadPerc} ->
                :io_lib.format('~8.2f%', [threadPerc])
           end,
             cnt),
       :io_lib.format('~n', [])]
  end

  defp get_total(cnt, base) do
    :maps.fold(fn _, {val, _}, time ->
                    time + val
                  _, val, time ->
                    time + val
               end,
                 base, cnt)
  end

  defp get_thread_perc(%{:"$type" => :msacc_data, :counters => cnt} = thread,
            systemTime) do
    threadTime = get_total(cnt, 0)
    %{thread
      |
      :"$type" => :msacc_stats,
        :system => percentage(threadTime, systemTime),
        :counters
        =>
        get_thread_perc(cnt, threadTime, systemTime)}
  end

  defp get_thread_perc(cnt, threadTime, systemTime) do
    :maps.map(fn f
              key, {val, c} ->
                m = f.(key, val)
                %{m | :cnt => c}
              _Key, val ->
                %{:thread => percentage(val, threadTime),
                    :system => percentage(val, systemTime)}
              end,
                cnt)
  end

  defp merge_threads([%{:"$type" => :msacc_stats, :type => type,
               :counters => cnt} = m0 |
               r],
            acc) do
    case (keyfind(:type, type, acc)) do
      false ->
        merge_threads(r,
                        [:maps.remove(:id, %{m0 | :threads => 1}) | acc])
      %{:"$type" => :msacc_stats, :counters => cnt0,
          :threads => threads, :system => system} = m ->
        newMap = %{m
                   |
                   :counters => add_counters(cnt, cnt0),
                     :system => system + :maps.get(:system, m0),
                     :threads => threads + 1}
        newAcc = keyreplace(:type, type, newMap, acc)
        merge_threads(r, newAcc)
    end
  end

  defp merge_threads([],
            [%{:"$type" => :msacc_stats, :system => system,
                 :threads => threads, :counters => cnt} = m0 |
                 r]) do
    counters = :maps.map(fn _, %{:thread => thr} = map ->
                              %{map | :thread => thr / threads}
                         end,
                           cnt)
    m = :maps.remove(:threads, m0)
    [%{m | :system => system, :counters => counters} |
         merge_threads([], r)]
  end

  defp merge_threads([], []) do
    []
  end

  defp merge_threads([%{:"$type" => :msacc_data, :type => type,
               :counters => cnt} = m0 |
               r],
            acc) do
    case (keyfind(:type, type, acc)) do
      false ->
        merge_threads(r, [:maps.remove(:id, m0) | acc])
      %{:"$type" => :msacc_data, :counters => cnt0} = m ->
        newMap = %{m | :counters => add_counters(cnt, cnt0)}
        newAcc = keyreplace(:type, type, newMap, acc)
        merge_threads(r, newAcc)
    end
  end

  defp merge_threads([], acc) do
    acc
  end

  defp add_counters(m1, m2) do
    :maps.map(fn key,
                   %{:thread => thr1, :system => sys1, :cnt => cnt1} ->
                   %{:thread => thr2, :system => sys2,
                       :cnt => cnt2} = :maps.get(key, m2)
                   %{:thread => thr1 + thr2, :system => sys1 + sys2,
                       :cnt => cnt1 + cnt2}
                 key, %{:thread => thr1, :system => sys1} ->
                   %{:thread => thr2, :system => sys2} = :maps.get(key, m2)
                   %{:thread => thr1 + thr2, :system => sys1 + sys2}
                 key, {v1, c1} ->
                   {v2, c2} = :maps.get(key, m2)
                   {v1 + v2, c1 + c2}
                 key, v1 ->
                   :maps.get(key, m2) + v1
              end,
                m1)
  end

  defp percentage(divident, divisor) do
    cond do
      divisor == 0 and divident != 0 ->
        100.0
      divisor == 0 ->
        0.0
      true ->
        divident / divisor * 100
    end
  end

  defp keyfind(key, value, [h | t]) do
    case (:maps.find(key, h)) do
      {:ok, ^value} ->
        h
      _ ->
        keyfind(key, value, t)
    end
  end

  defp keyfind(_, _, []) do
    false
  end

  defp keyreplace(key, value, newMap, [h | t]) do
    case (:maps.find(key, h)) do
      {:ok, ^value} ->
        [newMap | t]
      _ ->
        [h | keyreplace(key, value, newMap, t)]
    end
  end

  defp keyreplace(_, _, _, []) do
    []
  end

  defp statssort(stats) do
    :lists.sort(fn %{:type => type1, :id => id1},
                     %{:type => type2, :id => id2} ->
                     {type1, id1} < {type2, id2}
                   %{:type => type1}, %{:type => type2} ->
                     type1 < type2
                end,
                  stats)
  end

  defp map(fun, map) do
    for {k, v} <- :maps.to_list(map) do
      fun.(k, v)
    end
  end

end