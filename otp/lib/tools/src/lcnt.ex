defmodule :m_lcnt do
  use Bitwise
  import Kernel, except: [inspect: 2, apply: 3, apply: 2]
  @behaviour :gen_server
  @author "BjÃ¶rn-Egil Dahlberg"
  require Record
  Record.defrecord(:r_state, :state, locks: [], duration: 0)

  Record.defrecord(:r_stats, :stats,
    file: :undefined,
    line: :undefined,
    tries: :undefined,
    colls: :undefined,
    time: :undefined,
    nt: :undefined,
    hist: :undefined
  )

  Record.defrecord(:r_lock, :lock, name: :undefined, id: :undefined, type: :undefined, stats: [])

  Record.defrecord(:r_print, :print,
    name: :undefined,
    id: :undefined,
    type: :undefined,
    entry: :undefined,
    tries: :undefined,
    colls: :undefined,
    cr: :undefined,
    time: :undefined,
    dtr: :undefined,
    hist: :undefined
  )

  def start() do
    :gen_server.start({:local, :lcnt}, :lcnt, [], [])
  end

  def stop() do
    :gen_server.stop(:lcnt, :normal, :infinity)
  end

  def init([]) do
    {:ok, r_state(locks: [], duration: 0)}
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

  def rt_mask(node, categories)
      when is_atom(node) and
             is_list(categories) do
    :rpc.call(node, :lcnt, :rt_mask, [categories])
  end

  def rt_mask(node) when is_atom(node) do
    :rpc.call(node, :lcnt, :rt_mask, [])
  end

  def rt_mask(categories) when is_list(categories) do
    case :erts_debug.lcnt_control(:copy_save) do
      false ->
        :erts_debug.lcnt_control(:mask, categories)

      true ->
        {:error, :copy_save_enabled}
    end
  end

  def rt_mask() do
    :erts_debug.lcnt_control(:mask)
  end

  def rt_collect(node) do
    :rpc.call(node, :lcnt, :rt_collect, [])
  end

  def rt_collect() do
    :erts_debug.lcnt_collect()
  end

  def rt_clear(node) do
    :rpc.call(node, :lcnt, :rt_clear, [])
  end

  def rt_clear() do
    :erts_debug.lcnt_clear()
  end

  def rt_opt(node, arg) do
    :rpc.call(node, :lcnt, :rt_opt, [arg])
  end

  def rt_opt({:process_locks, enable}) do
    toggle_category(:process, enable)
  end

  def rt_opt({:port_locks, enable}) do
    toggle_category(:io, enable)
  end

  def rt_opt({type, newVal}) do
    previousVal = :erts_debug.lcnt_control(type)
    :erts_debug.lcnt_control(type, newVal)
    previousVal
  end

  defp toggle_category(category, true) do
    previousMask = :erts_debug.lcnt_control(:mask)

    :erts_debug.lcnt_control(
      :mask,
      [category | previousMask]
    )

    :lists.member(category, previousMask)
  end

  defp toggle_category(category, false) do
    previousMask = :erts_debug.lcnt_control(:mask)

    :erts_debug.lcnt_control(
      :mask,
      :lists.delete(category, previousMask)
    )

    :lists.member(category, previousMask)
  end

  def clear() do
    rt_clear()
  end

  def clear(node) do
    rt_clear(node)
  end

  def collect() do
    call({:collect, rt_collect()})
  end

  def collect(node) do
    call({:collect, rt_collect(node)})
  end

  def locations() do
    call({:locations, []})
  end

  def locations(opts) do
    call({:locations, opts})
  end

  def conflicts() do
    call({:conflicts, []})
  end

  def conflicts(opts) do
    call({:conflicts, opts})
  end

  def inspect(lock) do
    call({:inspect, lock, []})
  end

  def inspect(lock, opts) do
    call({:inspect, lock, opts})
  end

  def histogram(lock) do
    call({:histogram, lock, []})
  end

  def histogram(lock, opts) do
    call({:histogram, lock, opts})
  end

  def information() do
    call(:information)
  end

  def swap_pid_keys() do
    call(:swap_pid_keys)
  end

  def raw() do
    call(:raw)
  end

  def set(option, value) do
    call({:set, option, value})
  end

  def set({option, value}) do
    call({:set, option, value})
  end

  def save(filename) do
    call({:save, filename})
  end

  def load(filename) do
    call({:load, filename})
  end

  defp call(msg) do
    :ok = start_internal()
    :gen_server.call(:lcnt, msg, :infinity)
  end

  def apply(m, f, as)
      when is_atom(m) and is_atom(f) and
             is_list(as) do
    apply(fn ->
      :erlang.apply(m, f, as)
    end)
  end

  def apply(fun) when is_function(fun) do
    :lcnt.apply(fun, [])
  end

  def apply(fun, as) when is_function(fun) do
    opt = :lcnt.rt_opt({:copy_save, true})
    :lcnt.clear()
    res = :erlang.apply(fun, as)
    :lcnt.collect()
    _ = :lcnt.rt_opt({:copy_save, opt})
    res
  end

  def all_conflicts() do
    all_conflicts(:time)
  end

  def all_conflicts(sort) do
    conflicts([
      {:max_locks, :none},
      {:thresholds, []},
      {:combine, false},
      {:sort, sort},
      {:reverse, true}
    ])
  end

  def pid(id, serial) do
    pid(node(), id, serial)
  end

  def pid(node, id, serial) when is_atom(node) do
    header = <<131, 103, 100>>
    string = :erlang.atom_to_list(node)
    l = length(string)

    :erlang.binary_to_term(
      :erlang.list_to_binary([header, bytes16(l), string, bytes32(id), bytes32(serial), 0])
    )
  end

  def port(id) do
    port(node(), id)
  end

  def port(node, id) when is_atom(node) do
    header = <<131, 102, 100>>
    string = :erlang.atom_to_list(node)
    l = length(string)
    :erlang.binary_to_term(:erlang.list_to_binary([header, bytes16(l), string, bytes32(id), 0]))
  end

  def handle_call({:conflicts, inOpts}, _From, r_state(locks: locks) = state)
      when is_list(inOpts) do
    default = [
      {:sort, :time},
      {:reverse, false},
      {:print, [:name, :id, :tries, :colls, :ratio, :time, :duration]},
      {:max_locks, 20},
      {:combine, true},
      {:thresholds, [{:tries, 0}, {:colls, 0}, {:time, 0}]},
      {:locations, false}
    ]

    opts = options(inOpts, default)

    flocks =
      filter_locks_type(
        locks,
        :proplists.get_value(:type, opts)
      )

    combos =
      combine_classes(
        flocks,
        :proplists.get_value(:combine, opts)
      )

    printables = locks2print(combos, r_state(state, :duration))
    filtered = filter_print(printables, opts)

    print_lock_information(
      filtered,
      :proplists.get_value(:print, opts)
    )

    {:reply, :ok, state}
  end

  def handle_call(:information, _From, state) do
    print_state_information(state)
    {:reply, :ok, state}
  end

  def handle_call({:locations, inOpts}, _From, r_state(locks: locks) = state)
      when is_list(inOpts) do
    default = [
      {:sort, :time},
      {:reverse, false},
      {:print, [:name, :entry, :tries, :colls, :ratio, :time, :duration]},
      {:max_locks, 20},
      {:combine, true},
      {:thresholds, [{:tries, 0}, {:colls, 0}, {:time, 0}]},
      {:locations, true}
    ]

    opts = options(inOpts, default)

    printables =
      filter_print(
        for {stats, names} <- combine_locations(locks) do
          r_print(
            name: string_names(names),
            entry:
              term2string(
                '~tp:~p',
                [r_stats(stats, :file), r_stats(stats, :line)]
              ),
            colls: r_stats(stats, :colls),
            tries: r_stats(stats, :tries),
            cr:
              percent(
                r_stats(stats, :colls),
                r_stats(stats, :tries)
              ),
            time: r_stats(stats, :time),
            dtr:
              percent(
                r_stats(stats, :time),
                r_state(state, :duration)
              )
          )
        end,
        opts
      )

    print_lock_information(
      printables,
      :proplists.get_value(:print, opts)
    )

    {:reply, :ok, state}
  end

  def handle_call(
        {:inspect, lockname, inOpts},
        _From,
        r_state(duration: duration, locks: locks) = state
      )
      when is_list(inOpts) do
    default = [
      {:sort, :time},
      {:reverse, false},
      {:print, [:name, :id, :tries, :colls, :ratio, :time, :duration, :histogram]},
      {:max_locks, 20},
      {:combine, false},
      {:thresholds, []},
      {:locations, false}
    ]

    opts = options(inOpts, default)
    filtered = filter_locks(locks, lockname)

    iDs =
      case {:proplists.get_value(:full_id, opts), :proplists.get_value(:combine, opts)} do
        {true, true} ->
          locks_ids(filtered)

        _ ->
          []
      end

    combos =
      combine_classes(
        filtered,
        :proplists.get_value(:combine, opts)
      )

    case :proplists.get_value(:locations, opts) do
      true ->
        :lists.foreach(
          fn r_lock(name: name, id: id, type: type, stats: stats) ->
            idString =
              case :proplists.get_value(
                     :full_id,
                     opts
                   ) do
                true ->
                  term2string(
                    :proplists.get_value(
                      name,
                      iDs,
                      id
                    )
                  )

                _ ->
                  term2string(id)
              end

            combined =
              for {cStats, _} <- combine_locations(stats) do
                cStats
              end

            case combined do
              [] ->
                :ok

              _ ->
                print('lock: ' ++ term2string(name))
                print('id:   ' ++ idString)
                print('type: ' ++ term2string(type))
                ps = stats2print(combined, duration)

                opts1 =
                  options(
                    [
                      {:print, [:entry, :tries, :colls, :ratio, :time, :duration, :histogram]},
                      {:thresholds, [{:tries, -1}, {:colls, -1}, {:time, -1}]}
                    ],
                    opts
                  )

                print_lock_information(
                  filter_print(ps, opts1),
                  :proplists.get_value(
                    :print,
                    opts1
                  )
                )
            end
          end,
          combos
        )

      _ ->
        print =
          filter_print(
            locks2print(combos, duration),
            opts
          )

        print_lock_information(
          print,
          :proplists.get_value(:print, opts)
        )
    end

    {:reply, :ok, state}
  end

  def handle_call(
        {:histogram, lockname, inOpts},
        _From,
        r_state(duration: duration, locks: locks) = state
      ) do
    default = [
      {:sort, :time},
      {:reverse, false},
      {:print, [:name, :id, :tries, :colls, :ratio, :time, :duration, :histogram]},
      {:max_locks, 20},
      {:combine, true},
      {:thresholds, []},
      {:locations, false}
    ]

    opts = options(inOpts, default)
    filtered = filter_locks(locks, lockname)

    combos =
      combine_classes(
        filtered,
        :proplists.get_value(:combine, opts)
      )

    :lists.foreach(
      fn r_lock(stats: stats) = l ->
        sumStats = summate_stats(stats)

        opts1 =
          options(
            [
              {:print, [:name, :id, :tries, :colls, :ratio, :time, :duration]},
              {:thresholds, [{:tries, -1}, {:colls, -1}, {:time, -1}]}
            ],
            opts
          )

        prints = locks2print([l], duration)

        print_lock_information(
          prints,
          :proplists.get_value(
            :print,
            opts1
          )
        )

        print_full_histogram(r_stats(sumStats, :hist))
      end,
      combos
    )

    {:reply, :ok, state}
  end

  def handle_call(:raw, _From, r_state(locks: locks) = state) do
    {:reply, locks, state}
  end

  def handle_call({:collect, data}, _From, state) do
    {:reply, :ok, data2state(data, state)}
  end

  def handle_call(:swap_pid_keys, _From, r_state(locks: locks) = state) do
    swappedLocks =
      :lists.map(
        fn
          l
          when r_lock(l, :name) === :port_lock or
                 r_lock(l, :type) === :proclock ->
            r_lock(l, id: r_lock(l, :name), name: r_lock(l, :id))

          l ->
            l
        end,
        locks
      )

    {:reply, :ok, r_state(state, locks: swappedLocks)}
  end

  def handle_call({:set, :data, data}, _From, state) do
    {:reply, :ok, data2state(data, state)}
  end

  def handle_call({:set, :duration, duration}, _From, state) do
    {:reply, :ok, r_state(state, duration: duration)}
  end

  def handle_call({:load, filename}, _From, state) do
    case :file.read_file(filename) do
      {:ok, binary} ->
        case :erlang.binary_to_term(binary) do
          {'1.0', statelist} ->
            {:reply, :ok, list2state(statelist)}

          {version, _} ->
            {:reply, {:error, {:mismatch, version, '1.0'}}, state}
        end

      error ->
        {:reply, {:error, error}, state}
    end
  end

  def handle_call({:save, filename}, _From, state) do
    binary = :erlang.term_to_binary({'1.0', state2list(state)})

    case :file.write_file(filename, binary) do
      :ok ->
        {:reply, :ok, state}

      error ->
        {:reply, {:error, error}, state}
    end
  end

  def handle_call(command, _From, state) do
    {:reply, {:error, {:undefined, command}}, state}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  def handle_info(_Info, state) do
    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp summate_locks(locks) do
    summate_locks(
      locks,
      r_stats(tries: 0, colls: 0, time: 0, nt: 0)
    )
  end

  defp summate_locks([], stats) do
    stats
  end

  defp summate_locks(
         [l | ls],
         r_stats(tries: tries, colls: colls, time: time, nt: nt, hist: hist)
       ) do
    s = summate_stats(r_lock(l, :stats))

    summate_locks(
      ls,
      r_stats(
        tries: tries + r_stats(s, :tries),
        colls: colls + r_stats(s, :colls),
        time: time + r_stats(s, :time),
        nt: nt + r_stats(s, :nt),
        hist: summate_histogram(hist, r_stats(s, :hist))
      )
    )
  end

  defp summate_stats(stats) do
    summate_stats(
      stats,
      r_stats(tries: 0, colls: 0, time: 0, nt: 0)
    )
  end

  defp summate_stats([], stats) do
    stats
  end

  defp summate_stats(
         [s | ss],
         r_stats(tries: tries, colls: colls, time: time, nt: nt, hist: hist)
       ) do
    summate_stats(
      ss,
      r_stats(
        tries: tries + r_stats(s, :tries),
        colls: colls + r_stats(s, :colls),
        time: time + r_stats(s, :time),
        nt: nt + r_stats(s, :nt),
        hist: summate_histogram(hist, r_stats(s, :hist))
      )
    )
  end

  defp summate_histogram(tup, :undefined) when is_tuple(tup) do
    tup
  end

  defp summate_histogram(:undefined, tup) when is_tuple(tup) do
    tup
  end

  defp summate_histogram(hs1, hs2) do
    :erlang.list_to_tuple(
      for {a, b} <-
            :lists.zip(
              :erlang.tuple_to_list(hs1),
              :erlang.tuple_to_list(hs2)
            ) do
        a + b
      end
    )
  end

  defp filter_locks_type(locks, :undefined) do
    locks
  end

  defp filter_locks_type(locks, :all) do
    locks
  end

  defp filter_locks_type(locks, types) when is_list(types) do
    for l <- locks, :lists.member(r_lock(l, :type), types) do
      l
    end
  end

  defp filter_locks_type(locks, type) do
    for l <- locks, r_lock(l, :type) === type do
      l
    end
  end

  defp filter_locks(locks, {lockname, ids}) when is_list(ids) do
    for l <- locks, r_lock(l, :name) === lockname, :lists.member(r_lock(l, :id), ids) do
      l
    end
  end

  defp filter_locks(locks, {lockname, id}) do
    for l <- locks, r_lock(l, :name) === lockname, r_lock(l, :id) === id do
      l
    end
  end

  defp filter_locks(locks, lockname) do
    for l <- locks, r_lock(l, :name) === lockname do
      l
    end
  end

  defp filter_print(pLs, opts) do
    tLs =
      threshold_locks(
        pLs,
        :proplists.get_value(:thresholds, opts, [])
      )

    sLs =
      sort_locks(
        tLs,
        :proplists.get_value(:sort, opts, :time)
      )

    cLs =
      cut_locks(
        sLs,
        :proplists.get_value(:max_locks, opts, :none)
      )

    reverse_locks(
      cLs,
      :proplists.get_value(:reverse, opts, false)
    )
  end

  defp sort_locks(locks, :name) do
    reverse_sort_locks(r_print(:name), locks)
  end

  defp sort_locks(locks, :id) do
    reverse_sort_locks(r_print(:id), locks)
  end

  defp sort_locks(locks, :type) do
    reverse_sort_locks(r_print(:type), locks)
  end

  defp sort_locks(locks, :tries) do
    reverse_sort_locks(r_print(:tries), locks)
  end

  defp sort_locks(locks, :colls) do
    reverse_sort_locks(r_print(:colls), locks)
  end

  defp sort_locks(locks, :ratio) do
    reverse_sort_locks(r_print(:cr), locks)
  end

  defp sort_locks(locks, :time) do
    reverse_sort_locks(r_print(:time), locks)
  end

  defp sort_locks(locks, _) do
    sort_locks(locks, :time)
  end

  defp reverse_sort_locks(ix, locks) do
    :lists.reverse(:lists.keysort(ix, locks))
  end

  defp threshold_locks(locks, thresholds) do
    tries = :proplists.get_value(:tries, thresholds, -1)
    colls = :proplists.get_value(:colls, thresholds, -1)
    time = :proplists.get_value(:time, thresholds, -1)

    for l <- locks,
        r_print(l, :tries) > tries,
        r_print(l, :colls) > colls,
        r_print(l, :time) > time do
      l
    end
  end

  defp cut_locks(locks, n) when is_integer(n) and n > 0 do
    :lists.sublist(locks, n)
  end

  defp cut_locks(locks, _) do
    locks
  end

  defp reverse_locks(locks, true) do
    :lists.reverse(locks)
  end

  defp reverse_locks(locks, _) do
    locks
  end

  defp string_names([]) do
    ''
  end

  defp string_names(names) do
    string_names(names, [])
  end

  defp string_names([name], strings) do
    strings(:lists.reverse([term2string(name) | strings]))
  end

  defp string_names([name | names], strings) do
    string_names(names, [term2string(name) ++ ',' | strings])
  end

  defp combine_locations(locations) do
    :gb_trees.values(
      combine_locations(
        locations,
        :gb_trees.empty()
      )
    )
  end

  defp combine_locations([], tree) do
    tree
  end

  defp combine_locations([s | _] = stats, tree)
       when elem(s, 0) === :stats do
    combine_locations(stats, :undefined, tree)
  end

  defp combine_locations([r_lock(stats: stats, name: name) | ls], tree) do
    combine_locations(
      ls,
      combine_locations(stats, name, tree)
    )
  end

  defp combine_locations([], _, tree) do
    tree
  end

  defp combine_locations([s | ss], name, tree)
       when elem(s, 0) === :stats do
    key = {r_stats(s, :file), r_stats(s, :line)}

    tree1 =
      case :gb_trees.lookup(key, tree) do
        :none ->
          :gb_trees.insert(key, {s, [name]}, tree)

        {:value, {c, names}} ->
          newNames =
            case :lists.member(name, names) do
              true ->
                names

              _ ->
                [name | names]
            end

          :gb_trees.update(
            key,
            {r_stats(c,
               tries: r_stats(c, :tries) + r_stats(s, :tries),
               colls: r_stats(c, :colls) + r_stats(s, :colls),
               time: r_stats(c, :time) + r_stats(s, :time),
               nt: r_stats(c, :nt) + r_stats(s, :nt)
             ), newNames},
            tree
          )
      end

    combine_locations(ss, name, tree1)
  end

  defp combine_classes(locks, true) do
    combine_classes1(locks, :gb_trees.empty())
  end

  defp combine_classes(locks, _) do
    locks
  end

  defp combine_classes1([], tree) do
    :gb_trees.values(tree)
  end

  defp combine_classes1([l | ls], tree) do
    key = r_lock(l, :name)

    case :gb_trees.lookup(key, tree) do
      :none ->
        combine_classes1(
          ls,
          :gb_trees.insert(key, r_lock(l, id: 1), tree)
        )

      {:value, c} ->
        combine_classes1(
          ls,
          :gb_trees.update(
            key,
            r_lock(c,
              id: r_lock(c, :id) + 1,
              stats: r_lock(l, :stats) ++ r_lock(c, :stats)
            ),
            tree
          )
        )
    end
  end

  defp locks_ids(locks) do
    locks_ids(locks, [])
  end

  defp locks_ids([], out) do
    out
  end

  defp locks_ids([r_lock(name: key) = l | ls], out) do
    case :proplists.get_value(key, out) do
      :undefined ->
        locks_ids(ls, [{key, [r_lock(l, :id)]} | out])

      ids ->
        locks_ids(
          ls,
          [
            {key, [r_lock(l, :id) | ids]}
            | :proplists.delete(
                key,
                out
              )
          ]
        )
    end
  end

  defp stats2print(stats, duration) do
    :lists.map(
      fn s ->
        r_print(
          entry: term2string('~tp:~p', [r_stats(s, :file), r_stats(s, :line)]),
          colls: r_stats(s, :colls),
          tries: r_stats(s, :tries),
          cr: percent(r_stats(s, :colls), r_stats(s, :tries)),
          time: r_stats(s, :time),
          dtr: percent(r_stats(s, :time), duration),
          hist: format_histogram(r_stats(s, :hist))
        )
      end,
      stats
    )
  end

  defp locks2print(locks, duration) do
    :lists.map(
      fn l ->
        r_stats(tries: tries, colls: colls, time: time, hist: hist) =
          summate_stats(r_lock(l, :stats))

        cr = percent(colls, tries)
        dtr = percent(time, duration)

        r_print(
          name: r_lock(l, :name),
          id: r_lock(l, :id),
          type: r_lock(l, :type),
          tries: tries,
          colls: colls,
          hist: format_histogram(hist),
          cr: cr,
          time: time,
          dtr: dtr
        )
      end,
      locks
    )
  end

  defp format_histogram(tup) when is_tuple(tup) do
    vs = :erlang.tuple_to_list(tup)
    max = :lists.max(vs)

    case max do
      0 ->
        string_histogram(vs)

      _ ->
        string_histogram(
          for v <- vs do
            case v do
              0 ->
                0

              _ ->
                v / max
            end
          end
        )
    end
  end

  defp string_histogram(vs) do
    [?| | histogram_values_to_string(vs, ?|)]
  end

  defp histogram_values_to_string([0 | vs], end__) do
    [?\s | histogram_values_to_string(vs, end__)]
  end

  defp histogram_values_to_string([v | vs], end__) when v > 0.66 do
    [?X | histogram_values_to_string(vs, end__)]
  end

  defp histogram_values_to_string([v | vs], end__) when v > 0.33 do
    [?x | histogram_values_to_string(vs, end__)]
  end

  defp histogram_values_to_string([_ | vs], end__) do
    [?. | histogram_values_to_string(vs, end__)]
  end

  defp histogram_values_to_string([], end__) do
    [end__]
  end

  defp data2state(data, state) do
    duration =
      time2us(
        :proplists.get_value(
          :duration,
          data
        )
      )

    rawlocks = :proplists.get_value(:locks, data)
    locks = locks2records(rawlocks)
    r_state(state, duration: duration, locks: locks)
  end

  defp locks2records([{name, id, type, stats} | locks]) do
    [
      r_lock(name: name, id: clean_id_creation(id), type: type, stats: stats2record(stats))
      | locks2records(locks)
    ]
  end

  defp locks2records([]) do
    []
  end

  defp stats2record([
         {{file, line}, {tries, colls, {s, ns, n}}, hist}
         | stats
       ]) do
    [
      r_stats(
        file: file,
        line: line,
        hist: hist,
        tries: tries,
        colls: colls,
        time: time2us({s, ns}),
        nt: n
      )
      | stats2record(stats)
    ]
  end

  defp stats2record([
         {{file, line}, {tries, colls, {s, ns, n}}}
         | stats
       ]) do
    [
      r_stats(
        file: file,
        line: line,
        hist: {},
        tries: tries,
        colls: colls,
        time: time2us({s, ns}),
        nt: n
      )
      | stats2record(stats)
    ]
  end

  defp stats2record([]) do
    []
  end

  defp clean_id_creation(id) when is_pid(id) do
    bin = :erlang.term_to_binary(id)
    <<h::size(3)-binary, rest::binary>> = bin
    <<131, pidTag, atomTag>> = h
    lL = atomlen_bits(atomTag)
    cL = creation_bits(pidTag)
    <<l::size(lL), node::size(l)-binary, ids::size(8)-binary, _Creation::binary>> = rest
    bin2 = :erlang.list_to_binary([h, <<l::size(lL)>>, node, ids, <<0::size(cL)>>])
    :erlang.binary_to_term(bin2)
  end

  defp clean_id_creation(id) when is_port(id) do
    bin = :erlang.term_to_binary(id)
    <<h::size(3)-binary, rest::binary>> = bin
    <<131, portTag, atomTag>> = h
    lL = atomlen_bits(atomTag)
    cL = creation_bits(portTag)
    <<l::size(lL), node::size(l)-binary, ids::size(4)-binary, _Creation::binary>> = rest
    bin2 = :erlang.list_to_binary([h, <<l::size(lL)>>, node, ids, <<0::size(cL)>>])
    :erlang.binary_to_term(bin2)
  end

  defp clean_id_creation(id) do
    id
  end

  defp atomlen_bits(?d) do
    16
  end

  defp atomlen_bits(?s) do
    8
  end

  defp atomlen_bits(?v) do
    16
  end

  defp atomlen_bits(?w) do
    8
  end

  defp creation_bits(?g) do
    8
  end

  defp creation_bits(?X) do
    32
  end

  defp creation_bits(?f) do
    8
  end

  defp creation_bits(?Y) do
    32
  end

  defp state_default(field) do
    :proplists.get_value(field, state2list(r_state()))
  end

  defp state2list(state) do
    [_ | values] = :erlang.tuple_to_list(state)

    :lists.zipwith(
      fn
        :locks, locks ->
          {:locks,
           for lock <- locks do
             lock2list(lock)
           end}

        x, y ->
          {x, y}
      end,
      Keyword.keys(r_state(r_state())),
      values
    )
  end

  defp lock_default(field) do
    :proplists.get_value(field, lock2list(r_lock()))
  end

  defp lock2list(lock) do
    [_ | values] = :erlang.tuple_to_list(lock)
    :lists.zip(Keyword.keys(r_lock(r_lock())), values)
  end

  defp list2state(list) do
    :erlang.list_to_tuple([
      :state
      | list2state(Keyword.keys(r_state(r_state())), list)
    ])
  end

  defp list2state([], _) do
    []
  end

  defp list2state([:locks | fs], list) do
    locks =
      for lock <- :proplists.get_value(:locks, list, []) do
        list2lock(lock)
      end

    [locks | list2state(fs, list)]
  end

  defp list2state([f | fs], list) do
    [
      :proplists.get_value(f, list, state_default(f))
      | list2state(fs, list)
    ]
  end

  defp list2lock(ls) do
    :erlang.list_to_tuple([
      :lock
      | list2lock(Keyword.keys(r_lock(r_lock())), ls)
    ])
  end

  defp list2lock([], _) do
    []
  end

  defp list2lock([:stats = f | fs], ls) do
    stats = stats2stats(:proplists.get_value(f, ls, lock_default(f)))
    [stats | list2lock(fs, ls)]
  end

  defp list2lock([f | fs], ls) do
    [
      :proplists.get_value(f, ls, lock_default(f))
      | list2lock(fs, ls)
    ]
  end

  defp stats2stats([]) do
    []
  end

  defp stats2stats([stat | stats]) do
    sz = length(r_stats(r_stats()))
    [stat2stat(stat, sz) | stats2stats(stats)]
  end

  defp stat2stat(stat, sz) when tuple_size(stat) === sz do
    stat
  end

  defp stat2stat(stat, _) do
    :erlang.list_to_tuple(:erlang.tuple_to_list(stat) ++ [{0}])
  end

  defp auto_print_width(locks, print) do
    r =
      :lists.foldl(
        fn l, max ->
          :erlang.list_to_tuple(
            :lists.reverse(
              :lists.foldl(
                fn
                  {:print, :print}, out ->
                    [
                      :print
                      | out
                    ]

                  {str, len}, out ->
                    [
                      :erlang.min(
                        :erlang.max(
                          length(s(str)) + 1,
                          len
                        ),
                        80
                      )
                      | out
                    ]
                end,
                [],
                :lists.zip(
                  :erlang.tuple_to_list(l),
                  :erlang.tuple_to_list(max)
                )
              )
            )
          )
        end,
        r_print(
          id: 4,
          type: 5,
          entry: 5,
          name: 6,
          tries: 8,
          colls: 13,
          cr: 16,
          time: 11,
          dtr: 14,
          hist: 20
        ),
        locks
      )

    offsets = [
      {:id, r_print(r, :id)},
      {:name, r_print(r, :name)},
      {:type, r_print(r, :type)},
      {:entry, r_print(r, :entry)},
      {:tries, r_print(r, :tries)},
      {:colls, r_print(r, :colls)},
      {:ratio, r_print(r, :cr)},
      {:time, r_print(r, :time)},
      {:duration, r_print(r, :dtr)},
      {:histogram, r_print(r, :hist)}
    ]

    :lists.foldr(
      fn
        {type, w}, out ->
          [{type, w} | out]

        type, out ->
          [:proplists.lookup(type, offsets) | out]
      end,
      [],
      print
    )
  end

  defp print_lock_information(locks, print) do
    autoPrint = auto_print_width(locks, print)
    print_header(autoPrint)

    :lists.foreach(
      fn l ->
        print_lock(l, autoPrint)
      end,
      locks
    )

    :ok
  end

  defp print_header(opts) do
    header =
      r_print(
        name: 'lock',
        id: 'id',
        type: 'type',
        entry: 'location',
        tries: '#tries',
        colls: '#collisions',
        cr: 'collisions [%]',
        time: 'time [us]',
        dtr: 'duration [%]',
        hist: 'histogram [log2(us)]'
      )

    divider =
      r_print(
        name:
          :lists.duplicate(
            1 + length(r_print(header, :name)),
            45
          ),
        id: :lists.duplicate(1 + length(r_print(header, :id)), 45),
        type:
          :lists.duplicate(
            1 + length(r_print(header, :type)),
            45
          ),
        entry:
          :lists.duplicate(
            1 + length(r_print(header, :entry)),
            45
          ),
        tries:
          :lists.duplicate(
            1 + length(r_print(header, :tries)),
            45
          ),
        colls:
          :lists.duplicate(
            1 + length(r_print(header, :colls)),
            45
          ),
        cr: :lists.duplicate(1 + length(r_print(header, :cr)), 45),
        time:
          :lists.duplicate(
            1 + length(r_print(header, :time)),
            45
          ),
        dtr: :lists.duplicate(1 + length(r_print(header, :dtr)), 45),
        hist:
          :lists.duplicate(
            1 + length(r_print(header, :hist)),
            45
          )
      )

    print_lock(header, opts)
    print_lock(divider, opts)
    :ok
  end

  defp print_lock(l, opts) do
    print(strings(format_lock(l, opts)))
  end

  defp format_lock(_, []) do
    []
  end

  defp format_lock(l, [opt | opts]) do
    case opt do
      :id ->
        [{:space, 25, s(r_print(l, :id))} | format_lock(l, opts)]

      {:id, w} ->
        [{:space, w, s(r_print(l, :id))} | format_lock(l, opts)]

      :type ->
        [{:space, 18, s(r_print(l, :type))} | format_lock(l, opts)]

      {:type, w} ->
        [{:space, w, s(r_print(l, :type))} | format_lock(l, opts)]

      :entry ->
        [{:space, 30, s(r_print(l, :entry))} | format_lock(l, opts)]

      {:entry, w} ->
        [{:space, w, s(r_print(l, :entry))} | format_lock(l, opts)]

      :name ->
        [{:space, 22, s(r_print(l, :name))} | format_lock(l, opts)]

      {:name, w} ->
        [{:space, w, s(r_print(l, :name))} | format_lock(l, opts)]

      :tries ->
        [{:space, 12, s(r_print(l, :tries))} | format_lock(l, opts)]

      {:tries, w} ->
        [{:space, w, s(r_print(l, :tries))} | format_lock(l, opts)]

      :colls ->
        [{:space, 14, s(r_print(l, :colls))} | format_lock(l, opts)]

      {:colls, w} ->
        [{:space, w, s(r_print(l, :colls))} | format_lock(l, opts)]

      :ratio ->
        [{:space, 20, s(r_print(l, :cr))} | format_lock(l, opts)]

      {:ratio, w} ->
        [{:space, w, s(r_print(l, :cr))} | format_lock(l, opts)]

      :time ->
        [{:space, 15, s(r_print(l, :time))} | format_lock(l, opts)]

      {:time, w} ->
        [{:space, w, s(r_print(l, :time))} | format_lock(l, opts)]

      :duration ->
        [{:space, 20, s(r_print(l, :dtr))} | format_lock(l, opts)]

      {:duration, w} ->
        [{:space, w, s(r_print(l, :dtr))} | format_lock(l, opts)]

      :histogram ->
        [{:space, 20, s(r_print(l, :hist))} | format_lock(l, opts)]

      {:histogram, w} ->
        [
          {:left, w - length(s(r_print(l, :hist))) - 1, s(r_print(l, :hist))}
          | format_lock(l, opts)
        ]

      _ ->
        format_lock(l, opts)
    end
  end

  defp print_state_information(r_state(locks: locks) = state) do
    stats = summate_locks(locks)
    print('information:')
    print(kv('#locks', s(length(locks))))

    print(
      kv(
        'duration',
        s(r_state(state, :duration)) ++
          ' us' ++ ' (' ++ s(r_state(state, :duration) / 1_000_000) ++ ' s)'
      )
    )

    print('\nsummated stats:')
    print(kv('#tries', s(r_stats(stats, :tries))))
    print(kv('#colls', s(r_stats(stats, :colls))))

    print(
      kv(
        'wait time',
        s(r_stats(stats, :time)) ++
          ' us' ++ ' ( ' ++ s(r_stats(stats, :time) / 1_000_000) ++ ' s)'
      )
    )

    print(
      kv(
        'percent of duration',
        s(percent(r_stats(stats, :time), r_state(state, :duration))) ++ ' %'
      )
    )

    :ok
  end

  defp print_full_histogram(t) when is_tuple(t) do
    vs = :erlang.tuple_to_list(t)
    max = :lists.max(vs)
    w = 60
    print_full_histogram(0, vs, max, w)
  end

  defp print_full_histogram(_, [], _, _) do
    :ok
  end

  defp print_full_histogram(ix, [v | vs], 0, w) do
    :io.format('~2w = log2 : ~8w |~n', [ix, v])
    print_full_histogram(ix + 1, vs, 0, w)
  end

  defp print_full_histogram(ix, [v | vs], max, w) do
    :io.format(
      '~2w = log2 : ~8w | ~s~n',
      [ix, v, :lists.duplicate(trunc(w * (v / max)), ?#)]
    )

    print_full_histogram(ix + 1, vs, max, w)
  end

  defp time2us({s, ns}) do
    s * 1_000_000 + div(ns, 1000)
  end

  defp percent(_, 0) do
    0.0
  end

  defp percent(t, n) do
    t / n * 100
  end

  defp options(opts, default) when is_list(default) do
    options1(:proplists.unfold(opts), default)
  end

  defp options1([], defaults) do
    defaults
  end

  defp options1([{key, value} | opts], defaults) do
    case :proplists.get_value(key, defaults) do
      :undefined ->
        options1(opts, [{key, value} | defaults])

      _ ->
        options1(
          opts,
          [{key, value} | :proplists.delete(key, defaults)]
        )
    end
  end

  defp print(string) do
    :io.format('~ts~n', [string])
  end

  defp kv(key, value) do
    kv(key, value, 20)
  end

  defp kv(key, value, offset) do
    term2string(term2string('~~~ps : ~~s', [offset]), [key, value])
  end

  defp s(t) when is_float(t) do
    term2string('~.4f', [t])
  end

  defp s(t) when is_list(t) do
    term2string('~ts', [t])
  end

  defp s(t) do
    term2string(t)
  end

  defp strings(strings) do
    strings(strings, [])
  end

  defp strings([], out) do
    out
  end

  defp strings([{:space, n, s} | ss], out) do
    strings(
      ss,
      out ++ term2string(term2string('~~~ws', [n]), [s])
    )
  end

  defp strings([{:left, n, s} | ss], out) do
    strings(
      ss,
      out ++ term2string(term2string(' ~~s~~~ws', [n]), [s, ''])
    )
  end

  defp strings([s | ss], out) do
    strings(ss, out ++ term2string('~ts', [s]))
  end

  defp term2string({m, f, a})
       when is_atom(m) and is_atom(f) and
              is_integer(a) do
    term2string('~p:~p/~p', [m, f, a])
  end

  defp term2string(term) when is_port(term) do
    case :erlang.term_to_binary(term) do
      <<_::size(2)-binary, ?w, l::size(8), node::size(l)-binary, ids::size(32), _::binary>> ->
        term2string('#Port<~ts.~w>', [node, ids])

      <<_::size(2)-binary, ?v, l::size(16), node::size(l)-binary, ids::size(32), _::binary>> ->
        term2string('#Port<~ts.~w>', [node, ids])

      <<_::size(2)-binary, ?d, l::size(16), node::size(l)-binary, ids::size(32), _::binary>> ->
        term2string('#Port<~s.~w>', [node, ids])
    end
  end

  defp term2string(term) when is_pid(term) do
    case :erlang.term_to_binary(term) do
      <<_::size(2)-binary, ?w, l::size(8), node::size(l)-binary, ids::size(32), serial::size(32),
        _::binary>> ->
        term2string('<~ts.~w.~w>', [node, ids, serial])

      <<_::size(2)-binary, ?v, l::size(16), node::size(l)-binary, ids::size(32), serial::size(32),
        _::binary>> ->
        term2string('<~ts.~w.~w>', [node, ids, serial])

      <<_::size(2)-binary, ?d, l::size(16), node::size(l)-binary, ids::size(32), serial::size(32),
        _::binary>> ->
        term2string('<~s.~w.~w>', [node, ids, serial])
    end
  end

  defp term2string(term) do
    term2string('~w', [term])
  end

  defp term2string(format, terms) do
    :lists.flatten(:io_lib.format(format, terms))
  end

  defp bytes16(value) do
    b0 = value &&& 255
    b1 = value >>> 8 &&& 255
    <<b1, b0>>
  end

  defp bytes32(value) do
    b0 = value &&& 255
    b1 = value >>> 8 &&& 255
    b2 = value >>> 16 &&& 255
    b3 = value >>> 24 &&& 255
    <<b3, b2, b1, b0>>
  end
end
