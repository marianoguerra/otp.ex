defmodule :m_appmon_info do
  use Bitwise
  @behaviour :gen_server
  require Record
  Record.defrecord(:r_state, :state, starter: :undefined, opts: [], work: [], clients: [])

  Record.defrecord(:r_db, :db, q: :undefined, p: :undefined, links: :undefined, links2: :undefined)

  def start_link(node, client, opts) do
    :rpc.call(node, :appmon_info, :start_link2, [self(), client, opts])
  end

  def start_link2(starter, client, opts) do
    name = {:local, :appmon_info}
    args = {starter, opts, client}

    case :gen_server.start(name, :appmon_info, args, []) do
      {:ok, pid} ->
        {:ok, pid}

      {:error, {:already_started, pid}} ->
        register_client(pid, client)
        {:ok, pid}
    end
  end

  def app_ctrl(serv, aux, onOff, opts) do
    :gen_server.cast(
      serv,
      {self(), :app_ctrl, aux, onOff, opts}
    )
  end

  def load(serv, aux, onOff, opts) do
    :gen_server.cast(
      serv,
      {self(), :load, aux, onOff, opts}
    )
  end

  def app(serv, appName, onOff, opts) do
    :gen_server.cast(
      serv,
      {self(), :app, appName, onOff, opts}
    )
  end

  def pinfo(serv, pid, onOff, opt) do
    :gen_server.cast(
      serv,
      {self(), :pinfo, pid, onOff, opt}
    )
  end

  defp register_client(serv, p) do
    :erlang.link(serv)
    :gen_server.call(serv, {:register_client, p})
  end

  def status() do
    :gen_server.cast(:appmon_info, :status)
  end

  def init({starter, opts, pid}) do
    :erlang.link(pid)
    :erlang.process_flag(:trap_exit, true)
    workStore = :ets.new(:workstore, [:set, :public])
    {:ok, r_state(starter: starter, opts: opts, work: workStore, clients: [pid])}
  end

  def terminate(_Reason, state) do
    :ets.delete(r_state(state, :work))
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  def handle_call({:register_client, pid}, _From, state) do
    newState =
      case :lists.member(
             pid,
             r_state(state, :clients)
           ) do
        true ->
          state

        _ ->
          r_state(state, clients: [pid | r_state(state, :clients)])
      end

    {:reply, :ok, newState}
  end

  def handle_call(_Other, _From, state) do
    {:reply, :ok, state}
  end

  def handle_cast({from, cmd, aux, onOff, opts}, state) do
    newState = update_worklist(cmd, aux, from, onOff, opts, state)
    {:noreply, newState}
  end

  def handle_cast(:status, state) do
    print_state(state)
    {:noreply, state}
  end

  def handle_cast(_Other, state) do
    {:noreply, state}
  end

  def handle_info({:do_it, key}, state) do
    :ok = do_work(key, state)
    {:noreply, state}
  end

  def handle_info({:EXIT, pid, reason}, state) do
    case r_state(state, :starter) do
      ^pid ->
        {:stop, reason, state}

      _Other ->
        work = r_state(state, :work)
        del_work(:ets.match(work, {{:"$1", :"$2", pid}, :_, :_, :_}), pid, work)

        case :lists.delete(pid, r_state(state, :clients)) do
          [] ->
            case get_opt(:stay_resident, r_state(state, :opts)) do
              true ->
                {:noreply, r_state(state, clients: [])}

              _ ->
                {:stop, :normal, state}
            end

          newClients ->
            {:noreply, r_state(state, clients: newClients)}
        end
    end
  end

  def handle_info(_Other, state) do
    {:noreply, state}
  end

  defp do_work(key, state) do
    workStore = r_state(state, :work)
    {cmd, aux, from, _OldRef, old, opts} = retrieve(workStore, key)
    {:ok, result} = do_work2(cmd, aux, from, old, opts)

    cond do
      result == old ->
        :ok

      true ->
        send(from, {:delivery, self(), cmd, aux, result})
        :ok
    end

    case get_opt(:timeout, opts) do
      :at_most_once ->
        del_task(key, workStore)

      t when is_integer(t) ->
        {:ok, ref} = :timer.send_after(t, {:do_it, key})
        store(workStore, key, ref, result, opts)
    end

    :ok
  end

  defp do_work2(:load, _Aux, _From, old, opts) do
    calc_load(old, opts)
  end

  defp do_work2(:app_ctrl, _Aux, _From, _Old, _Opts) do
    calc_app_on_node()
  end

  defp do_work2(:app, aux, _From, _Old, opts) do
    calc_app_tree(aux, opts)
  end

  defp do_work2(:pinfo, aux, _From, _Old, _Opts) do
    calc_pinfo(:pinfo, aux)
  end

  defp do_work2(cmd, aux, _From, _Old, _Opts) do
    {cmd, aux}
  end

  defp retrieve(tab, key) do
    case :ets.lookup(tab, key) do
      [{{cmd, aux, from}, ref, old, opts}] ->
        {cmd, aux, from, ref, old, opts}

      _Other ->
        false
    end
  end

  defp store(tab, key, ref, old, opts) do
    :ets.insert(tab, {key, ref, old, opts})
    key
  end

  defp update_worklist(cmd, aux, from, true, opts, state) do
    add_task(cmd, aux, from, opts, state)
    state
  end

  defp update_worklist(cmd, aux, from, _Other, _Opts, state) do
    del_task(cmd, aux, from, r_state(state, :work))
    state
  end

  defp add_task(cmd, aux, from, opts, state) do
    workStore = r_state(state, :work)
    key = {cmd, aux, from}
    oldOpts = del_task(key, workStore)
    store(workStore, key, nil, nil, ins_opts(opts, oldOpts))

    try do
      do_work(key, state)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  defp del_work([[cmd, aux] | ws], pid, work) do
    del_task(cmd, aux, pid, work)
    del_work(ws, pid, work)
  end

  defp del_work([], _Pid, _Work) do
    :ok
  end

  defp del_task(cmd, aux, from, workStore) do
    del_task({cmd, aux, from}, workStore)
  end

  defp del_task(key, workStore) do
    oldStuff = retrieve(workStore, key)
    :ets.delete(workStore, key)

    case oldStuff do
      {_Cmd, _Aux, _From, ref, _Old, opts} ->
        cond do
          ref != nil ->
            {:ok, _} = :timer.cancel(ref)

            receive do
              {:do_it, ^key} ->
                opts
            after
              10 ->
                opts
            end

          true ->
            opts
        end

      _ ->
        []
    end
  end

  defp calc_app_tree(name, opts) do
    mode = get_opt(:info_type, opts)

    case :application_controller.get_master(name) do
      pid when is_pid(pid) ->
        dB = new_db(mode, pid)
        gL = groupl(pid)

        r =
          case (try do
                  do_find_proc(mode, dB, gL, find_avoid())
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            {:ok, dB2} ->
              {:ok,
               {format(pid), format(:ets.tab2list(r_db(dB2, :p))),
                format(:ets.tab2list(r_db(dB2, :links))),
                format(:ets.tab2list(r_db(dB2, :links2)))}}

            {:error, reason} ->
              {:error, reason}

            other ->
              {:error, other}
          end

        :ets.delete(r_db(dB, :p))
        :ets.delete(r_db(dB, :links))
        :ets.delete(r_db(dB, :links2))
        r

      _ ->
        {:ok, {[], [], [], []}}
    end
  end

  defp get_pid(p) when is_pid(p) do
    p
  end

  defp get_pid(p) when is_port(p) do
    p
  end

  defp get_pid(x) when is_tuple(x) do
    :erlang.element(2, x)
  end

  defp do_find_proc(mode, dB, gL, avoid) do
    case get_next(dB) do
      {{:value, v}, dB2} ->
        do_find_proc2(v, mode, dB2, gL, avoid)

      {:empty, dB2} ->
        {:ok, dB2}
    end
  end

  defp do_find_proc2(x, mode, dB, gL, avoid) when is_port(x) do
    do_find_proc(mode, dB, gL, avoid)
  end

  defp do_find_proc2(x, mode, dB, gL, avoid) do
    xpid = get_pid(x)

    dB2 =
      case is_proc(dB, xpid) do
        false ->
          add_proc(dB, xpid)
          c1 = find_children(x, mode)
          add_children(c1, xpid, dB, gL, avoid, mode)

        _ ->
          dB
      end

    do_find_proc(mode, dB2, gL, avoid)
  end

  defp find_children(x, :sup) when is_pid(x) do
    :supervisor.which_children(x)
  end

  defp find_children(x, :link)
       when is_pid(x) and
              node(x) != node() do
    []
  end

  defp find_children(x, :link) when is_pid(x) do
    case :erlang.process_info(x, :links) do
      {:links, links} ->
        :lists.reverse(links)

      _ ->
        []
    end
  end

  defp find_children({:master, x}, :sup) do
    case :application_master.get_child(x) do
      {pid, _Name} when is_pid(pid) ->
        [pid]

      pid when is_pid(pid) ->
        [pid]
    end
  end

  defp find_children({_, _X, :worker, _}, :sup) do
    []
  end

  defp find_children({_, x, :supervisor, _}, :sup) do
    :lists.filter(
      fn thing ->
        pid = get_pid(thing)

        cond do
          is_pid(pid) ->
            true

          true ->
            false
        end
      end,
      :supervisor.which_children(x)
    )
  end

  defp add_children(cList, paren, dB, _GL, _Avoid, :sup) do
    :lists.foldr(
      fn c, dB2 ->
        case get_pid(c) do
          p when is_pid(p) ->
            add_prim(c, paren, dB2)

          _ ->
            dB2
        end
      end,
      dB,
      cList
    )
  end

  defp add_children(cList, paren, dB, gL, avoid, _Mode) do
    :lists.foldr(
      fn c, dB2 ->
        maybe_add_child(c, paren, dB2, gL, avoid)
      end,
      dB,
      cList
    )
  end

  defp maybe_add_child(c, paren, dB, gL, avoid) do
    case is_proc(dB, c) do
      false ->
        maybe_add_child_node(c, paren, dB, gL, avoid)

      _ ->
        dB
    end
  end

  defp maybe_add_child_node(c, paren, dB, gL, avoid) do
    cond do
      node(c) != node() ->
        add_foreign(c, paren, dB)

      true ->
        maybe_add_child_avoid(c, paren, dB, gL, avoid)
    end
  end

  defp maybe_add_child_avoid(c, paren, dB, gL, avoid) do
    case :lists.member(c, avoid) do
      true ->
        dB

      false ->
        maybe_add_child_port(c, paren, dB, gL)
    end
  end

  defp maybe_add_child_port(c, paren, dB, gL) do
    cond do
      is_port(c) ->
        add_prim(c, paren, dB)

      true ->
        maybe_add_child_sasl(c, paren, dB, gL)
    end
  end

  defp maybe_add_child_sasl(c, paren, dB, gL) do
    case check_sasl_ancestor(paren, c) do
      :yes ->
        add_prim(c, paren, dB)

      :no ->
        add_sec(c, paren, dB)

      :dont_know ->
        maybe_add_child_gl(c, paren, dB, gL)
    end
  end

  defp maybe_add_child_gl(c, paren, dB, gL) do
    case cmp_groupl(gL, groupl(c)) do
      true ->
        maybe_add_child_sec(c, paren, dB)

      _ ->
        dB
    end
  end

  defp maybe_add_child_sec(c, paren, dB) do
    case is_in_queue(dB, c) do
      true ->
        add_sec(c, paren, dB)

      _ ->
        add_prim(c, paren, dB)
    end
  end

  defp check_sasl_ancestor(paren, c) do
    case :lists.keysearch(
           :"$ancestors",
           1,
           :erlang.element(
             2,
             :erlang.process_info(
               c,
               :dictionary
             )
           )
         ) do
      {:value, {_, l}} when is_list(l) ->
        h =
          cond do
            is_atom(hd(l)) ->
              :erlang.whereis(hd(l))

            true ->
              hd(l)
          end

        cond do
          h == paren ->
            :yes

          true ->
            :no
        end

      _ ->
        :dont_know
    end
  end

  defp new_db(mode, pid) do
    p = :ets.new(:processes, [:set, :public])
    l1 = :ets.new(:links, [:bag, :public])
    l2 = :ets.new(:extralinks, [:bag, :public])

    q =
      cond do
        mode === :sup ->
          :queue.in({:master, pid}, :queue.new())

        true ->
          :queue.in(pid, :queue.new())
      end

    r_db(q: q, p: p, links: l1, links2: l2)
  end

  defp get_next(dB) do
    {x, q} = :queue.out(r_db(dB, :q))
    {x, r_db(dB, q: q)}
  end

  defp add_proc(dB, p) do
    :ets.insert(r_db(dB, :p), {p})
  end

  defp add_prim(c, paren, dB) do
    :ets.insert(r_db(dB, :links), {paren, get_pid(c)})
    r_db(dB, q: :queue.in(c, r_db(dB, :q)))
  end

  defp add_foreign(c, paren, dB) do
    :ets.insert(r_db(dB, :links2), {paren, c})
    r_db(dB, q: :queue.in(c, r_db(dB, :q)))
  end

  defp add_sec(c, paren, dB) do
    :ets.insert(r_db(dB, :links2), {paren, c})
    dB
  end

  defp is_proc(r_db(p: tab), p) do
    :ets.member(tab, p)
  end

  defp is_in_queue(r_db(q: q), p) do
    :queue.member(p, q)
  end

  defp groupl(p) do
    case :erlang.process_info(p, :group_leader) do
      {:group_leader, gL} ->
        gL

      _Other ->
        nil
    end
  end

  defp cmp_groupl(_GL1, nil) do
    true
  end

  defp cmp_groupl(gL1, gL1) do
    true
  end

  defp cmp_groupl(_, _) do
    false
  end

  defp find_avoid() do
    :lists.foldr(
      fn x, accu ->
        case :erlang.whereis(x) do
          p when is_pid(p) ->
            [p | accu]

          _ ->
            accu
        end
      end,
      [:undefined],
      [:application_controller, :init, :gs, :node_serv, :appmon, :appmon_a, :appmon_info]
    )
  end

  defp format([{p} | fs]) do
    [{p, format(p)} | format(fs)]
  end

  defp format([{p1, p2} | fs]) do
    [{format(p1), format(p2)} | format(fs)]
  end

  defp format([]) do
    []
  end

  defp format(p) when is_pid(p) and node(p) != node() do
    :erlang.pid_to_list(p) ++ ' ' ++ :erlang.atom_to_list(node(p))
  end

  defp format(p) when is_pid(p) do
    case :erlang.process_info(p, :registered_name) do
      {:registered_name, name} ->
        :erlang.atom_to_list(name)

      _ ->
        :erlang.pid_to_list(p)
    end
  end

  defp format(p) when is_port(p) do
    case :erlang.port_info(p, :id) do
      :undefined ->
        'port closed'

      {_, pid} ->
        'port ' ++ :erlang.integer_to_list(pid)
    end
  end

  defp format(x) do
    :io.format('What: ~p~n', [x])
    '???'
  end

  defp calc_app_on_node() do
    newApps = reality_check(:application.which_applications())
    {:ok, newApps}
  end

  defp reality_check([e | es]) do
    n = :erlang.element(1, e)

    case (try do
            :application_controller.get_master(n)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      p when is_pid(p) ->
        [{p, n, e} | reality_check(es)]

      _ ->
        reality_check(es)
    end
  end

  defp reality_check([]) do
    []
  end

  defp calc_load(old, opts) do
    l = load(opts)

    case get_opt(:load_average, opts) do
      true ->
        case old do
          {_, ^l} ->
            {:ok, {l, l}}

          {_, o2} when abs(l - o2) < 3 ->
            {:ok, {o2, l}}

          {_, o2} ->
            {:ok, {o2, trunc((2 * l + o2) / 3)}}

          _ ->
            {:ok, {0, l}}
        end

      _ ->
        case old do
          {_, o2} ->
            {:ok, {o2, l}}

          _ ->
            {:ok, {0, l}}
        end
    end
  end

  defp load(opts) do
    q = get_sample(:queue)

    case get_opt(:load_method, opts) do
      :time ->
        td = get_sample(:runtime)
        tot = get_sample(:tot_time)

        case get_opt(:load_scale, opts) do
          :linear ->
            :erlang.min(
              trunc(load_range() * (td / tot + q / 6)),
              load_range()
            )

          :prog ->
            :erlang.min(
              trunc(load_range() * prog(td / tot + q / 6)),
              load_range()
            )
        end

      :queue ->
        case get_opt(:load_scale, opts) do
          :linear ->
            :erlang.min(trunc(load_range() * q / 6), load_range())

          :prog ->
            :erlang.min(
              trunc(load_range() * prog(q / 6)),
              load_range()
            )
        end
    end
  end

  defp prog(t) do
    :math.sqrt(abs(t) / 0.9)
  end

  defp get_sample(:queue) do
    :erlang.statistics(:run_queue)
  end

  defp get_sample(:runtime) do
    {rt, rd} = :erlang.statistics(:runtime)
    delta(:runtime, rt, rd)
  end

  defp get_sample(:tot_time) do
    {rt, rd} = :erlang.statistics(:wall_clock)
    delta(:tot_time, rt, rd)
  end

  defp delta(keyWord, val, cheatDelta) do
    retVal =
      case :erlang.get(keyWord) do
        :undefined ->
          val

        other ->
          cond do
            other > val ->
              cheatDelta

            true ->
              val - other
          end
      end

    :erlang.put(keyWord, val)
    retVal
  end

  defp load_range() do
    16
  end

  defp calc_pinfo(:pinfo, pid) when is_pid(pid) do
    info = :erlang.process_info(pid)
    {:ok, :io_lib.format('Node: ~p, Process: ~p~n~p~n~n', [node(), pid, info])}
  end

  defp calc_pinfo(:pinfo, pid) when is_port(pid) do
    info =
      :lists.map(
        fn key ->
          :erlang.port_info(pid, key)
        end,
        [:id, :name, :connected, :links, :input, :output]
      )

    {:ok,
     :io_lib.format(
       'Node: ~p, Port: ~p~n~p~n~n',
       [
         node(),
         :erlang.element(
           2,
           :erlang.port_info(pid, :id)
         ),
         info
       ]
     )}
  end

  defp calc_pinfo(:pinfo, _Pid) do
    {:ok, ''}
  end

  defp print_state(state) do
    :io.format('Status:~n    Opts: ~p~nClients: ~p~n    WorkStore:~n', [
      r_state(state, :opts),
      r_state(state, :clients)
    ])

    print_work(:ets.tab2list(r_state(state, :work)))
  end

  defp print_work([w | ws]) do
    :io.format('        ~p~n', [w])
    print_work(ws)
  end

  defp print_work([]) do
    :ok
  end

  defp get_opt(name, opts) do
    case :lists.keysearch(name, 1, opts) do
      {:value, val} ->
        :erlang.element(2, val)

      false ->
        default(name)
    end
  end

  defp default(:info_type) do
    :link
  end

  defp default(:load_average) do
    true
  end

  defp default(:load_method) do
    :time
  end

  defp default(:load_scale) do
    :prog
  end

  defp default(:stay_resident) do
    false
  end

  defp default(:timeout) do
    2000
  end

  defp ins_opts([opt | opts], opts2) do
    ins_opts(opts, ins_opt(opt, opts2))
  end

  defp ins_opts([], opts2) do
    opts2
  end

  defp ins_opt({opt, val}, [{opt, _} | os]) do
    [{opt, val} | os]
  end

  defp ins_opt(opt, [opt2 | os]) do
    [opt2 | ins_opt(opt, os)]
  end

  defp ins_opt(opt, []) do
    [opt]
  end
end
