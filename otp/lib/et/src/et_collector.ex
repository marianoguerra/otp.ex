defmodule :m_et_collector do
  use Bitwise
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_filter, :filter,
    name: :undefined,
    function: :undefined
  )

  Record.defrecord(:r_event, :event,
    detail_level: :undefined,
    trace_ts: :undefined,
    event_ts: :undefined,
    from: :undefined,
    to: :undefined,
    label: :undefined,
    contents: :undefined
  )

  Record.defrecord(:r_state, :state,
    parent_pid: :undefined,
    auto_shutdown: :undefined,
    event_tab_size: :undefined,
    event_tab: :undefined,
    dict_tab: :undefined,
    event_order: :undefined,
    subscribers: :undefined,
    file: :undefined,
    trace_pattern: :undefined,
    trace_port: :undefined,
    trace_max_queue: :undefined,
    trace_nodes: :undefined,
    trace_global: :undefined
  )

  Record.defrecord(:r_file, :file,
    name: :undefined,
    desc: :undefined,
    event_opt: :undefined,
    file_opt: :undefined,
    table_opt: :undefined
  )

  Record.defrecord(:r_table_handle, :table_handle,
    collector_pid: :undefined,
    event_tab: :undefined,
    event_order: :undefined,
    filter: :undefined
  )

  Record.defrecord(:r_trace_ts, :trace_ts,
    trace_ts: :undefined,
    event_ts: :undefined
  )

  Record.defrecord(:r_event_ts, :event_ts,
    event_ts: :undefined,
    trace_ts: :undefined
  )

  def start_link(options) do
    case parse_opt(options, default_state(), [], []) do
      {:ok, s, dict2, clients} ->
        res =
          case r_state(s, :trace_global) do
            false ->
              :gen_server.start_link(:et_collector, [s, dict2], [])

            true ->
              :gen_server.start_link({:global, :et_collector}, :et_collector, [s, dict2], [])
          end

        case res do
          {:ok, pid} when r_state(s, :parent_pid) !== self() ->
            :erlang.unlink(pid)
            start_clients(pid, clients)

          {:ok, pid} ->
            start_clients(pid, clients)

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp default_state() do
    r_state(
      parent_pid: self(),
      auto_shutdown: false,
      event_order: :trace_ts,
      subscribers: [],
      trace_global: false,
      trace_pattern: :undefined,
      trace_nodes: [],
      trace_port: 4711,
      trace_max_queue: 50
    )
  end

  defp parse_opt([], s, dict, clients) do
    {mod, pattern} = :et_selector.make_pattern(r_state(s, :trace_pattern))

    fun = fn e ->
      :et_selector.parse_event(mod, e)
    end

    default = {:dict_insert, {:filter, :all}, fun}
    {:ok, r_state(s, trace_pattern: {mod, pattern}), [default | dict], clients}
  end

  defp parse_opt([h | t], s, dict, clients) do
    case h do
      {:parent_pid, parent} when parent === :undefined ->
        parse_opt(t, r_state(s, parent_pid: parent), dict, clients)

      {:parent_pid, parent} when is_pid(parent) ->
        parse_opt(t, r_state(s, parent_pid: parent), dict, clients)

      {:auto_shutdown, bool}
      when bool === true or
             bool === false ->
        parse_opt(t, r_state(s, auto_shutdown: bool), dict, clients)

      {:event_order, order} when order === :trace_ts ->
        parse_opt(t, r_state(s, event_order: order), dict, clients)

      {:event_order, order} when order === :event_ts ->
        parse_opt(t, r_state(s, event_order: order), dict, clients)

      {:dict_insert, {:filter, name}, fun} ->
        cond do
          is_atom(name) and is_function(fun) ->
            parse_opt(t, s, dict ++ [h], clients)

          true ->
            {:error, {:bad_option, h}}
        end

      {:dict_insert, {:subscriber, pid}, _Val} ->
        cond do
          is_pid(pid) ->
            parse_opt(t, s, dict ++ [h], clients)

          true ->
            {:error, {:bad_option, h}}
        end

      {:dict_insert, _Key, _Val} ->
        parse_opt(t, s, dict ++ [h], clients)

      {:dict_delete, _Key} ->
        parse_opt(t, s, dict ++ [h], clients)

      {:trace_client, client = {_, _}} ->
        parse_opt(t, s, dict, clients ++ [client])

      {:trace_global, bool} when bool === false ->
        parse_opt(t, r_state(s, trace_global: bool), dict, clients)

      {:trace_global, bool} when bool === true ->
        parse_opt(t, r_state(s, trace_global: bool), dict, clients)

      {:trace_pattern, {mod, _} = pattern} when is_atom(mod) ->
        parse_opt(t, r_state(s, trace_pattern: pattern), dict, clients)

      {:trace_pattern, :undefined = pattern} ->
        parse_opt(t, r_state(s, trace_pattern: pattern), dict, clients)

      {:trace_port, port} when is_integer(port) ->
        parse_opt(t, r_state(s, trace_port: port), dict, clients)

      {:trace_max_queue, maxQueue} when is_integer(maxQueue) ->
        parse_opt(t, r_state(s, trace_port: maxQueue), dict, clients)

      bad ->
        {:error, {:bad_option, bad}}
    end
  end

  defp parse_opt(badList, _S, _Dict, _Clients) do
    {:error, {:bad_option_list, badList}}
  end

  defp start_clients(collectorPid, [{type, parameters} | t]) do
    _ = start_trace_client(collectorPid, type, parameters)
    start_clients(collectorPid, t)
  end

  defp start_clients(collectorPid, []) do
    {:ok, collectorPid}
  end

  def stop(collectorPid) do
    call(collectorPid, :stop)
  end

  def save_event_file(collectorPid, fileName, options) do
    call(
      collectorPid,
      {:save_event_file, fileName, options}
    )
  end

  defp load_event_file(collectorPid, fileName) do
    fd = make_ref()
    args = [{:file, fileName}, {:name, fd}, {:repair, true}, {:mode, :read_only}]

    fun = fn event, {:ok, tH} ->
      report(tH, event)
    end

    case :disk_log.open(args) do
      {:ok, _} ->
        do_load_event_file(fun, fd, :start, {:ok, collectorPid}, fileName, 0)

      {:repaired, _, _, badBytes} ->
        do_load_event_file(fun, fd, :start, {:ok, collectorPid}, fileName, badBytes)

      {:error, reason} ->
        exit({:disk_log_open, fileName, reason})
    end
  end

  defp do_load_event_file(fun, fd, cont, acc, fileName, badBytes) do
    case :disk_log.chunk(fd, cont) do
      :eof ->
        {:ok, badBytes}

      {:error, reason} ->
        exit({:bad_disk_log_chunk, fileName, reason})

      {cont2, events} ->
        acc2 = :lists.foldl(fun, acc, events)
        do_load_event_file(fun, fd, cont2, acc2, fileName, badBytes)

      {cont2, events, more} ->
        acc2 = :lists.foldl(fun, acc, events)
        do_load_event_file(fun, fd, cont2, acc2, fileName, badBytes + more)
    end
  end

  def report(collectorPid, traceOrEvent)
      when is_pid(collectorPid) do
    case get_table_handle(collectorPid) do
      {:ok, tH} when elem(tH, 0) === :table_handle ->
        report(tH, traceOrEvent)

      {:error, reason} ->
        exit(reason)
    end
  end

  def report(tH, traceOrEvent)
      when elem(tH, 0) === :table_handle do
    fun = r_table_handle(tH, :filter)

    case fun.(traceOrEvent) do
      false ->
        {:ok, tH}

      true when elem(traceOrEvent, 0) === :event ->
        key = make_key(tH, traceOrEvent)

        case (try do
                :ets.insert(r_table_handle(tH, :event_tab), {key, traceOrEvent})
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          true ->
            {:ok, tH}

          {:EXIT, _Reason} ->
            report(r_table_handle(tH, :collector_pid), traceOrEvent)
        end

      {true, event} when elem(event, 0) === :event ->
        key = make_key(tH, event)

        case (try do
                :ets.insert(r_table_handle(tH, :event_tab), {key, event})
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          true ->
            {:ok, tH}

          {:EXIT, _Reason} ->
            report(r_table_handle(tH, :collector_pid), traceOrEvent)
        end

      badEvent ->
        tS = :erlang.now()
        contents = [{:trace, traceOrEvent}, {:reason, badEvent}, {:filter, fun}]

        event =
          r_event(
            detail_level: 0,
            trace_ts: tS,
            event_ts: tS,
            from: :bad_filter,
            to: :bad_filter,
            label: :bad_filter,
            contents: contents
          )

        key = make_key(tH, event)

        case (try do
                :ets.insert(r_table_handle(tH, :event_tab), {key, event})
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          true ->
            {:ok, tH}

          {:EXIT, _Reason} ->
            report(r_table_handle(tH, :collector_pid), traceOrEvent)
        end
    end
  end

  def report(_, bad) do
    exit({:bad_event, bad})
  end

  def report_event(collectorPid, detailLevel, fromTo, label, contents) do
    report_event(collectorPid, detailLevel, fromTo, fromTo, label, contents)
  end

  def report_event(collectorPid, detailLevel, from, to, label, contents)
      when is_integer(detailLevel) and detailLevel >= 0 and
             detailLevel <= 100 do
    tS = :erlang.now()

    e =
      r_event(
        detail_level: detailLevel,
        trace_ts: tS,
        event_ts: tS,
        from: from,
        to: to,
        label: label,
        contents: contents
      )

    report(collectorPid, e)
  end

  def make_key(tH, stuff) when elem(tH, 0) === :table_handle do
    make_key(r_table_handle(tH, :event_order), stuff)
  end

  def make_key(:trace_ts, stuff) do
    cond do
      elem(stuff, 0) === :event ->
        r_event(trace_ts: r, event_ts: p) = stuff
        r_trace_ts(trace_ts: r, event_ts: p)

      elem(stuff, 0) === :trace_ts ->
        stuff

      elem(stuff, 0) === :event_ts ->
        r_event_ts(trace_ts: r, event_ts: p) = stuff
        r_trace_ts(trace_ts: r, event_ts: p)
    end
  end

  def make_key(:event_ts, stuff) do
    cond do
      elem(stuff, 0) === :event ->
        r_event(trace_ts: r, event_ts: p) = stuff
        r_event_ts(trace_ts: r, event_ts: p)

      elem(stuff, 0) === :event_ts ->
        stuff

      elem(stuff, 0) === :trace_ts ->
        r_trace_ts(trace_ts: r, event_ts: p) = stuff
        r_event_ts(trace_ts: r, event_ts: p)
    end
  end

  def get_table_size(collectorPid) when is_pid(collectorPid) do
    call(collectorPid, :get_table_size)
  end

  defp get_table_handle(collectorPid) when is_pid(collectorPid) do
    call(collectorPid, :get_table_handle)
  end

  def get_global_pid() do
    case :global.whereis_name(:et_collector) do
      collectorPid when is_pid(collectorPid) ->
        collectorPid

      :undefined ->
        exit(:global_collector_not_started)
    end
  end

  def change_pattern(collectorPid, rawPattern) do
    pattern = :et_selector.make_pattern(rawPattern)
    call(collectorPid, {:change_pattern, pattern})
  end

  def dict_insert(collectorPid, key = {:filter, name}, fun) do
    cond do
      is_atom(name) and is_function(fun) ->
        call(collectorPid, {:dict_insert, key, fun})

      true ->
        exit({:badarg, key})
    end
  end

  def dict_insert(collectorPid, key = {:subscriber, pid}, val) do
    cond do
      is_pid(pid) ->
        call(collectorPid, {:dict_insert, key, val})

      true ->
        exit({:badarg, key})
    end
  end

  def dict_insert(collectorPid, key, val) do
    call(collectorPid, {:dict_insert, key, val})
  end

  def dict_lookup(collectorPid, key) do
    call(collectorPid, {:dict_lookup, key})
  end

  def dict_delete(collectorPid, key) do
    call(collectorPid, {:dict_delete, key})
  end

  def dict_match(collectorPid, pattern) do
    call(collectorPid, {:dict_match, pattern})
  end

  def multicast(
        _CollectorPid,
        msg = {:dict_insert, _Key, _Val}
      ) do
    exit({:badarg, msg})
  end

  def multicast(_CollectorPid, msg = {:dict_delete, _Key}) do
    exit({:badarg, msg})
  end

  def multicast(collectorPid, msg) do
    call(collectorPid, {:multicast, msg})
  end

  def start_trace_client(collectorPid, type, fileName)
      when type === :event_file do
    load_event_file(collectorPid, fileName)
  end

  def start_trace_client(collectorPid, type, fileName)
      when type === :file do
    waitFor = {make_ref(), :end_of_trace}

    eventFun = fn e, {replyTo, {:ok, tH}} ->
      {replyTo, report(tH, e)}
    end

    endFun = fn {replyTo, {:ok, _TH}} ->
      send(replyTo, waitFor)
      replyTo
    end

    spec = trace_spec_wrapper(eventFun, endFun, {self(), {:ok, collectorPid}})
    pid = :dbg.trace_client(type, fileName, spec)
    :erlang.unlink(pid)
    ref = :erlang.monitor(:process, pid)

    receive do
      ^waitFor ->
        :erlang.demonitor(ref, [:flush])
        :file_loaded

      {:DOWN, ^ref, _, _, reason} ->
        exit(reason)
    end
  end

  def start_trace_client(collectorPid, type, parameters) do
    eventFun = fn event, {:ok, tH} ->
      report(tH, event)
    end

    endFun = fn acc ->
      acc
    end

    spec = trace_spec_wrapper(eventFun, endFun, {:ok, collectorPid})
    pid = :dbg.trace_client(type, parameters, spec)
    send(collectorPid, {:register_trace_client, pid})
    :erlang.unlink(pid)
    {:trace_client_pid, pid}
  end

  defp trace_spec_wrapper(eventFun, endFun, eventInitialAcc)
       when is_function(eventFun) and is_function(endFun) do
    {fn trace, acc ->
       case trace === :end_of_trace do
         true ->
           endFun.(acc)

         false ->
           eventFun.(trace, acc)
       end
     end, eventInitialAcc}
  end

  def start_trace_port(parameters) do
    :dbg.tracer(:port, :dbg.trace_port(:ip, parameters))
  end

  def monitor_trace_port(collectorPid, parameters) do
    res = start_trace_port(parameters)

    spawn(fn ->
      monitorRef = :erlang.monitor(:process, collectorPid)

      receive do
        {:DOWN, ^monitorRef, _, _, _} ->
          :dbg.stop_clear()
      end
    end)

    res
  end

  def iterate(handle, prev, limit) do
    iterate(handle, prev, limit, :undefined, prev)
  end

  def iterate(_, _, limit, _, acc) when limit === 0 do
    acc
  end

  def iterate(collectorPid, prev, limit, fun, acc)
      when is_pid(collectorPid) do
    case get_table_handle(collectorPid) do
      {:ok, tH} when elem(tH, 0) === :table_handle ->
        iterate(tH, prev, limit, fun, acc)

      {:error, reason} ->
        exit(reason)
    end
  end

  def iterate(tH, prev, limit, fun, acc)
      when elem(tH, 0) === :table_handle do
    cond do
      limit === :infinity ->
        next_iterate(tH, prev, limit, fun, acc)

      is_integer(limit) and limit > 0 ->
        next_iterate(tH, prev, limit, fun, acc)

      limit === :"-infinity" ->
        prev_iterate(tH, prev, limit, fun, acc)

      is_integer(limit) and limit < 0 ->
        prev_iterate(tH, prev, limit, fun, acc)
    end
  end

  defp next_iterate(tH, prev = :first, limit, fun, acc) do
    tab = r_table_handle(tH, :event_tab)

    case (try do
            :ets.first(tab)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :"$end_of_table" ->
        acc

      {:EXIT, _} = error ->
        :io.format('~p(~p): First ~tp~n', [:et_collector, 751, error])
        iterate(r_table_handle(tH, :collector_pid), prev, limit, fun, acc)

      first ->
        lookup_and_apply(tH, prev, first, limit, -1, fun, acc)
    end
  end

  defp next_iterate(tH, prev = :last, limit, fun, acc) do
    tab = r_table_handle(tH, :event_tab)

    case (try do
            :ets.last(tab)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :"$end_of_table" ->
        acc

      {:EXIT, _} = error ->
        :io.format('~p(~p): Last ~tp~n', [:et_collector, 762, error])
        iterate(r_table_handle(tH, :collector_pid), prev, limit, fun, acc)

      last ->
        lookup_and_apply(tH, prev, last, limit, -1, fun, acc)
    end
  end

  defp next_iterate(tH, prev, limit, fun, acc) do
    tab = r_table_handle(tH, :event_tab)
    key = make_key(tH, prev)

    case (try do
            :ets.next(tab, key)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :"$end_of_table" ->
        acc

      {:EXIT, _} = error ->
        :io.format('~p(~p): Next ~tp -> ~tp~n', [:et_collector, 774, key, error])
        iterate(r_table_handle(tH, :collector_pid), prev, limit, fun, acc)

      next ->
        lookup_and_apply(tH, prev, next, limit, -1, fun, acc)
    end
  end

  defp prev_iterate(tH, prev = :first, limit, fun, acc) do
    tab = r_table_handle(tH, :event_tab)

    case (try do
            :ets.first(tab)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :"$end_of_table" ->
        acc

      {:EXIT, _} = error ->
        :io.format('~p(~p): First ~tp~n', [:et_collector, 786, error])
        iterate(r_table_handle(tH, :collector_pid), prev, limit, fun, acc)

      first ->
        lookup_and_apply(tH, prev, first, limit, 1, fun, acc)
    end
  end

  defp prev_iterate(tH, prev = :last, limit, fun, acc) do
    tab = r_table_handle(tH, :event_tab)

    case (try do
            :ets.last(tab)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :"$end_of_table" ->
        acc

      {:EXIT, _} = error ->
        :io.format('~p(~p): Last ~tp~n', [:et_collector, 797, error])
        iterate(r_table_handle(tH, :collector_pid), prev, limit, fun, acc)

      last ->
        lookup_and_apply(tH, prev, last, limit, 1, fun, acc)
    end
  end

  defp prev_iterate(tH, prev, limit, fun, acc) do
    tab = r_table_handle(tH, :event_tab)
    key = make_key(tH, prev)

    case (try do
            :ets.prev(tab, key)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :"$end_of_table" ->
        acc

      {:EXIT, _} = error ->
        :io.format('~p(~p): Prev ~tp -> ~tp~n', [:et_collector, 809, key, error])
        iterate(r_table_handle(tH, :collector_pid), prev, limit, fun, acc)

      next ->
        lookup_and_apply(tH, prev, next, limit, 1, fun, acc)
    end
  end

  defp lookup_and_apply(tH, _Prev, next, limit, incr, fun, _Acc)
       when fun === :undefined do
    limit2 = incr(limit, incr)
    iterate(tH, next, limit2, fun, next)
  end

  defp lookup_and_apply(tH, prev, next, limit, incr, fun, acc) do
    tab = r_table_handle(tH, :event_tab)

    case (try do
            :ets.lookup_element(tab, next, 2)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        iterate(r_table_handle(tH, :collector_pid), prev, limit, fun, acc)

      e when elem(e, 0) === :event ->
        acc2 = fun.(e, acc)
        limit2 = incr(limit, incr)
        iterate(tH, next, limit2, fun, acc2)
    end
  end

  def lookup(collectorPid, key) when is_pid(collectorPid) do
    case get_table_handle(collectorPid) do
      {:ok, tH} when elem(tH, 0) === :table_handle ->
        lookup(tH, key)

      {:error, reason} ->
        {:error, reason}
    end
  end

  def lookup(tH, key) when elem(tH, 0) === :table_handle do
    tab = r_table_handle(tH, :event_tab)

    case (try do
            :ets.lookup_element(tab, key, 2)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, :enoent}

      e when elem(e, 0) === :event ->
        {:ok, e}
    end
  end

  defp incr(val, incr) do
    cond do
      val === :infinity ->
        val

      val === :"-infinity" ->
        val

      is_integer(val) ->
        val + incr
    end
  end

  def clear_table(collectorPid) when is_pid(collectorPid) do
    call(collectorPid, :clear_table)
  end

  def clear_table(tH) when elem(tH, 0) === :table_handle do
    clear_table(r_table_handle(tH, :collector_pid))
  end

  defp call(collectorPid, request) do
    try do
      :gen_server.call(collectorPid, request, :infinity)
    catch
      :exit, {:noproc, _} ->
        {:error, :no_collector}
    end
  end

  def init([initialS, dict]) do
    :erlang.process_flag(:trap_exit, true)

    case r_state(initialS, :parent_pid) do
      :undefined ->
        :ok

      pid when is_pid(pid) ->
        :erlang.link(pid)
    end

    funs = [
      &init_tables/1,
      &init_global/1,
      fn s ->
        :lists.foldl(&do_dict_insert/2, s, dict)
      end
    ]

    {:ok,
     :lists.foldl(
       fn f, s ->
         f.(s)
       end,
       initialS,
       funs
     )}
  end

  defp init_tables(s) do
    eventTab =
      :ets.new(
        :et_events,
        [:ordered_set, {:keypos, 1}, :public]
      )

    dictTab =
      :ets.new(
        :et_dict,
        [:ordered_set, {:keypos, 1}, :public]
      )

    r_state(s, event_tab: eventTab, dict_tab: dictTab, event_tab_size: 0)
  end

  defp init_global(s) do
    case r_state(s, :trace_global) do
      true ->
        eventFun = fn event, {:ok, tH} ->
          report(tH, event)
        end

        endFun = fn acc ->
          acc
        end

        spec = trace_spec_wrapper(eventFun, endFun, {:ok, self()})
        :dbg.tracer(:process, spec)
        :et_selector.change_pattern(r_state(s, :trace_pattern))
        :ok = :net_kernel.monitor_nodes(true)

        :lists.foreach(
          fn n ->
            send(self(), {:nodeup, n})
          end,
          :erlang.nodes()
        )

        r_state(s, trace_nodes: [node()])

      false ->
        s
    end
  end

  def handle_call({:multicast, msg}, _From, s) do
    do_multicast(r_state(s, :subscribers), msg)
    reply(:ok, s)
  end

  def handle_call(msg = {:dict_insert, _Key, _Val}, _From, s) do
    s2 = do_dict_insert(msg, s)
    reply(:ok, s2)
  end

  def handle_call(msg = {:dict_delete, _Key}, _From, s) do
    try do
      s2 = do_dict_delete(msg, s)
      reply(:ok, s2)
    catch
      {:stop, r} ->
        opt_unlink(r_state(s, :parent_pid))
        {:stop, r, s}
    end
  end

  def handle_call({:dict_lookup, key}, _From, s) do
    reply = :ets.lookup(r_state(s, :dict_tab), key)
    reply(reply, s)
  end

  def handle_call({:dict_match, pattern}, _From, s) do
    case (try do
            :ets.match_object(r_state(s, :dict_tab), pattern)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _Reason} ->
        reply([], s)

      matching ->
        reply(matching, s)
    end
  end

  def handle_call(:get_table_handle, _From, s) do
    [{_, tableFilter}] =
      :ets.lookup(
        r_state(s, :dict_tab),
        {:filter, :all}
      )

    tH =
      r_table_handle(
        collector_pid: self(),
        event_tab: r_state(s, :event_tab),
        event_order: r_state(s, :event_order),
        filter: tableFilter
      )

    reply({:ok, tH}, s)
  end

  def handle_call(:get_table_size, _From, s) do
    size = :ets.info(r_state(s, :event_tab), :size)
    reply({:ok, size}, s)
  end

  def handle_call(:close, _From, s) do
    case r_state(s, :file) do
      :undefined ->
        reply({:error, :file_not_open}, s)

      f ->
        reply = :disk_log.close(r_file(f, :desc))
        s2 = r_state(s, file: :undefined)
        reply(reply, s2)
    end
  end

  def handle_call({:save_event_file, fileName, options}, _From, s) do
    default = r_file(name: fileName, event_opt: :existing, file_opt: :write, table_opt: :keep)

    case parse_file_options(default, options) do
      {:ok, f} when elem(f, 0) === :file ->
        case file_open(f) do
          {:ok, fd} ->
            f2 = r_file(f, desc: fd)

            {reply2, s3} =
              case r_file(f2, :event_opt) do
                :existing ->
                  fun = fn {_, e}, a ->
                    :ok = :disk_log.log(fd, e)
                    a
                  end

                  tab = r_state(s, :event_tab)
                  reply = tab_iterate(fun, tab, :ets.first(tab), :ok)
                  :ok = :disk_log.close(fd)
                  {reply, s}
              end

            case r_file(f2, :table_opt) do
              :keep ->
                reply(reply2, s3)

              :clear ->
                s4 = do_clear_table(s3)
                reply(reply2, s4)
            end

          {:error, reason} ->
            reply({:error, {:file_open, reason}}, s)
        end

      {:error, reason} ->
        reply({:error, reason}, s)
    end
  end

  def handle_call({:change_pattern, pattern}, _From, s) do
    ns = r_state(s, :trace_nodes)
    {_, []} = :rpc.multicall(ns, :et_selector, :change_pattern, [pattern])
    reply = {:old_pattern, r_state(s, :trace_pattern)}
    s2 = r_state(s, trace_pattern: pattern)
    reply(reply, s2)
  end

  def handle_call(:clear_table, _From, s) do
    s2 = do_clear_table(s)
    reply(:ok, s2)
  end

  def handle_call(:stop, _From, s) do
    do_multicast(r_state(s, :subscribers), :close)

    case r_state(s, :trace_global) do
      true ->
        {_, []} = :rpc.multicall(r_state(s, :trace_nodes), :dbg, :stop_clear, [])
        :ok

      false ->
        :ok
    end

    {:stop, :shutdown, :ok, s}
  end

  def handle_call(request, from, s) do
    :ok =
      :error_logger.format(
        '~p(~p): handle_call(~tp, ~tp, ~tp)~n',
        [:et_collector, self(), request, from, s]
      )

    reply({:error, {:bad_request, request}}, s)
  end

  def handle_cast(msg, s) do
    :ok =
      :error_logger.format(
        '~p(~p): handle_cast(~tp, ~tp)~n',
        [:et_collector, self(), msg, s]
      )

    noreply(s)
  end

  def handle_info(:timeout, s) do
    s2 = check_size(s)
    noreply(s2)
  end

  def handle_info({:nodeup, node}, s) do
    port = r_state(s, :trace_port)
    maxQueue = r_state(s, :trace_max_queue)

    case :rpc.call(node, :et_collector, :monitor_trace_port, [self(), {port, maxQueue}]) do
      {:ok, _} ->
        s2 = listen_on_trace_port(node, port, s)
        noreply(s2)

      {:error, reason} when reason === :already_started ->
        :ok =
          :error_logger.format(
            '~p(~p): producer ignored(~p:~p):~n    ~tp~n',
            [:et_collector, self(), node, port, reason]
          )

        s2 = r_state(s, trace_port: port + 1)
        noreply(s2)

      {:badrpc, reason} ->
        :ok =
          :error_logger.format(
            '~p(~p): producer ignored(~p:~p):~n    ~tp~n',
            [:et_collector, self(), node, port, reason]
          )

        s2 = r_state(s, trace_port: port + 1)
        noreply(s2)

      {:error, reason} ->
        send(self(), {:nodeup, node})

        :ok =
          :error_logger.format(
            '~p(~p): producer retry(~p:~p):~n     ~tp~n',
            [:et_collector, self(), node, port, reason]
          )

        s2 = r_state(s, trace_port: port + 1)
        noreply(s2)
    end
  end

  def handle_info({:nodedown, node}, s) do
    noreply(r_state(s, trace_nodes: r_state(s, :trace_nodes) -- [node]))
  end

  def handle_info({:register_trace_client, pid}, s) do
    :erlang.link(pid)
    noreply(s)
  end

  def handle_info({:EXIT, pid, reason}, s)
      when pid === r_state(s, :parent_pid) do
    {:stop, reason, s}
  end

  def handle_info(info = {:EXIT, pid, reason}, s) do
    oldSubscribers = r_state(s, :subscribers)

    case :lists.member(pid, oldSubscribers) do
      true when reason === :shutdown ->
        try do
          s2 =
            do_dict_delete(
              {:dict_delete, {:subscriber, pid}},
              s
            )

          noreply(s2)
        catch
          {:stop, r} ->
            opt_unlink(r_state(s, :parent_pid))
            {:stop, r, s}
        end

      true ->
        opt_unlink(r_state(s, :parent_pid))
        {:stop, reason, s}

      false ->
        :ok =
          :error_logger.format(
            '~p(~p): handle_info(~tp, ~tp)~n',
            [:et_collector, self(), info, s]
          )

        noreply(s)
    end
  end

  def handle_info(info, s) do
    :ok =
      :error_logger.format(
        '~p(~p): handle_info(~tp, ~tp)~n',
        [:et_collector, self(), info, s]
      )

    noreply(s)
  end

  defp listen_on_trace_port(node, port, s) do
    [_Name, host] =
      :string.lexemes(
        :erlang.atom_to_list(node),
        [?@]
      )

    case (try do
            start_trace_client(self(), :ip, {host, port})
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:trace_client_pid, remotePid} ->
        :rpc.call(node, :et_selector, :change_pattern, [r_state(s, :trace_pattern)])
        :erlang.link(remotePid)

        r_state(s,
          trace_nodes: [node | r_state(s, :trace_nodes)],
          trace_port: port + 1
        )

      {:EXIT, reason} when reason === :already_started ->
        :ok =
          :error_logger.format(
            '~p(~p): consumer ignored(~p:~p): ~tp~n',
            [:et_collector, self(), node, port, reason]
          )

        r_state(s, trace_port: port + 1)

      {:EXIT, reason} ->
        send(self(), {:nodeup, node})

        :ok =
          :error_logger.format(
            '~p(~p): consumer retry(~p:~p):~n     ~tp~n',
            [:et_collector, self(), node, port, reason]
          )

        r_state(s, trace_port: port + 1)
    end
  end

  def terminate(reason, s) do
    fun = fn pid ->
      :erlang.exit(pid, reason)
    end

    :lists.foreach(fun, r_state(s, :subscribers))
  end

  def code_change(_OldVsn, s, _Extra) do
    {:ok, s}
  end

  defp do_clear_table(s) do
    oldTab = r_state(s, :event_tab)
    :ets.delete(oldTab)

    newTab =
      :ets.new(
        :et_events,
        [:ordered_set, {:keypos, 1}, :public]
      )

    r_state(s, event_tab: newTab)
  end

  defp do_dict_insert(
         msg = {:dict_insert, key = {:subscriber, pid}, val},
         s
       )
       when is_pid(pid) do
    oldSubscribers = r_state(s, :subscribers)

    newSubscribers =
      case :lists.member(
             pid,
             oldSubscribers
           ) do
        true ->
          oldSubscribers

        false ->
          :erlang.link(pid)
          all = :ets.match_object(r_state(s, :dict_tab), :_)

          :lists.foreach(
            fn {k, v} ->
              send(pid, {:et, {:dict_insert, k, v}})
            end,
            all
          )

          [pid | oldSubscribers]
      end

    do_multicast(newSubscribers, msg)
    size = :ets.info(r_state(s, :event_tab), :size)
    do_multicast(newSubscribers, {:more_events, size})
    :ets.insert(r_state(s, :dict_tab), {key, val})
    r_state(s, subscribers: newSubscribers)
  end

  defp do_dict_insert(msg = {:dict_insert, key, val}, s) do
    do_multicast(r_state(s, :subscribers), msg)
    :ets.insert(r_state(s, :dict_tab), {key, val})
    s
  end

  defp do_dict_delete(
         msg = {:dict_delete, key = {:subscriber, pid}},
         s
       ) do
    oldSubscribers = r_state(s, :subscribers)
    do_multicast(oldSubscribers, msg)
    :ets.delete(r_state(s, :dict_tab), key)

    case :lists.member(pid, oldSubscribers) do
      true ->
        :erlang.unlink(pid)
        s2 = r_state(s, subscribers: oldSubscribers -- [pid])

        cond do
          r_state(s2, :auto_shutdown) and
              r_state(s2, :subscribers) === [] ->
            throw({:stop, :shutdown})

          true ->
            s2
        end

      false ->
        s
    end
  end

  defp do_dict_delete({:dict_delete, {:filter, :all}}, s) do
    s
  end

  defp do_dict_delete(msg = {:dict_delete, key}, s) do
    do_multicast(r_state(s, :subscribers), msg)
    :ets.delete(r_state(s, :dict_tab), key)
    s
  end

  defp tab_iterate(_Fun, _Tab, :"$end_of_table", acc) do
    acc
  end

  defp tab_iterate(fun, tab, key, acc) do
    acc2 = :lists.foldl(fun, acc, :ets.lookup(tab, key))
    tab_iterate(fun, tab, :ets.next(tab, key), acc2)
  end

  defp file_open(f) do
    fd = make_ref()

    case r_file(f, :file_opt) do
      :write ->
        :ok = :file.rename(r_file(f, :name), r_file(f, :name) ++ '.OLD')

      :append ->
        :ok
    end

    args = [{:file, r_file(f, :name)}, {:name, fd}, {:repair, true}, {:mode, :read_write}]

    case :disk_log.open(args) do
      {:ok, _} ->
        {:ok, fd}

      {:repaired, _, _, badBytes} ->
        :ok =
          :error_logger.format(
            '~p: Skipped ~p bad bytes in file: ~tp~n',
            [:et_collector, badBytes, r_file(f, :name)]
          )

        {:ok, fd}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp parse_file_options(f, [h | t]) do
    case h do
      :existing ->
        parse_file_options(r_file(f, event_opt: :existing), t)

      :all ->
        parse_file_options(r_file(f, event_opt: :all), t)

      :write ->
        parse_file_options(r_file(f, file_opt: :write), t)

      :append ->
        parse_file_options(r_file(f, file_opt: :append), t)

      :keep ->
        parse_file_options(r_file(f, table_opt: :keep), t)

      :clear ->
        parse_file_options(r_file(f, table_opt: :clear), t)

      bad ->
        {:error, {:bad_file_option, bad}}
    end
  end

  defp parse_file_options(f, []) do
    {:ok, f}
  end

  defp do_multicast([pid | pids], msg) do
    send(pid, {:et, msg})
    do_multicast(pids, msg)
  end

  defp do_multicast([], _Msg) do
    :ok
  end

  defp opt_unlink(pid) do
    cond do
      pid === :undefined ->
        :ok

      true ->
        :erlang.unlink(pid)
    end
  end

  defp reply(reply, r_state(subscribers: []) = s) do
    {:reply, reply, s}
  end

  defp reply(reply, s) do
    {:reply, reply, s, 500}
  end

  defp noreply(r_state(subscribers: []) = s) do
    {:noreply, s}
  end

  defp noreply(s) do
    {:noreply, s, 500}
  end

  defp check_size(s) do
    size = :ets.info(r_state(s, :event_tab), :size)

    cond do
      size === r_state(s, :event_tab_size) ->
        s

      true ->
        msg = {:more_events, size}
        do_multicast(r_state(s, :subscribers), msg)
        r_state(s, event_tab_size: size)
    end
  end
end
