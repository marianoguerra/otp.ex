defmodule :m_mnesia_checkpoint do
  use Bitwise
  import :mnesia_lib, only: [add: 2, dbg_out: 2, del: 2, set: 2, unset: 1]
  require Record

  Record.defrecord(:r_tid, :tid,
    counter: :undefined,
    pid: :undefined
  )

  Record.defrecord(:r_tidstore, :tidstore, store: :undefined, up_stores: [], level: 1)

  Record.defrecord(:r_cstruct, :cstruct,
    name: :undefined,
    type: :set,
    ram_copies: [],
    disc_copies: [],
    disc_only_copies: [],
    external_copies: [],
    load_order: 0,
    access_mode: :read_write,
    majority: false,
    index: [],
    snmp: [],
    local_content: false,
    record_name: {:bad_record_name},
    attributes: [:key, :val],
    user_properties: [],
    frag_properties: [],
    storage_properties: [],
    cookie:
      {{:erlang.monotonic_time() + :erlang.time_offset(), :erlang.unique_integer(), 1}, node()},
    version: {{2, 0}, []}
  )

  Record.defrecord(:r_log_header, :log_header,
    log_kind: :undefined,
    log_version: :undefined,
    mnesia_version: :undefined,
    node: :undefined,
    now: :undefined
  )

  Record.defrecord(:r_commit, :commit,
    node: :undefined,
    decision: :undefined,
    ram_copies: [],
    disc_copies: [],
    disc_only_copies: [],
    ext: [],
    schema_ops: []
  )

  Record.defrecord(:r_decision, :decision,
    tid: :undefined,
    outcome: :undefined,
    disc_nodes: :undefined,
    ram_nodes: :undefined
  )

  Record.defrecord(:r_cyclic, :cyclic,
    node: node(),
    oid: :undefined,
    op: :undefined,
    lock: :undefined,
    lucky: :undefined
  )

  Record.defrecord(:r_checkpoint_args, :checkpoint_args,
    name: {:erlang.unique_integer([:positive]), node()},
    allow_remote: true,
    ram_overrides_dump: false,
    nodes: [],
    node: node(),
    now: :undefined,
    cookie:
      {{:erlang.monotonic_time() + :erlang.time_offset(), :erlang.unique_integer(), 1}, node()},
    min: [],
    max: [],
    pending_tab: :undefined,
    wait_for_old: :undefined,
    is_activated: false,
    ignore_new: [],
    retainers: [],
    iterators: [],
    supervisor: :undefined,
    pid: :undefined
  )

  Record.defrecord(:r_retainer, :retainer,
    cp_name: :undefined,
    tab_name: :undefined,
    store: :undefined,
    writers: [],
    really_retain: true
  )

  Record.defrecord(:r_iter, :iter,
    tab_name: :undefined,
    oid_tab: :undefined,
    main_tab: :undefined,
    retainer_tab: :undefined,
    source: :undefined,
    val: :undefined,
    pid: :undefined
  )

  Record.defrecord(:r_pending, :pending, tid: :undefined, disc_nodes: [], ram_nodes: [])

  def stop() do
    :lists.foreach(
      fn name ->
        call(name, :stop)
      end,
      checkpoints()
    )

    :ok
  end

  def tm_prepare(cp) when elem(cp, 0) === :checkpoint_args do
    name = r_checkpoint_args(cp, :name)

    case :lists.member(name, checkpoints()) do
      false ->
        start_retainer(cp)

      true ->
        {:error, {:already_exists, name, node()}}
    end
  end

  def tm_mnesia_down(node) do
    :lists.foreach(
      fn name ->
        cast(name, {:mnesia_down, node})
      end,
      checkpoints()
    )
  end

  def tm_enter_pending(tid, discNs, ramNs) do
    pending = r_pending(tid: tid, disc_nodes: discNs, ram_nodes: ramNs)
    tm_enter_pending(pending)
  end

  def tm_enter_pending(pending) do
    pendingTabs = val(:pending_checkpoints)
    tm_enter_pending(pendingTabs, pending)
  end

  defp tm_enter_pending([], pending) do
    pending
  end

  defp tm_enter_pending([tab | tabs], pending) do
    try do
      :ets.insert(tab, pending)
    catch
      :error, _ ->
        :ok
    end

    tm_enter_pending(tabs, pending)
  end

  def tm_exit_pending(tid) do
    pids = val(:pending_checkpoint_pids)
    tm_exit_pending(pids, tid)
  end

  defp tm_exit_pending([], tid) do
    tid
  end

  defp tm_exit_pending([pid | pids], tid) do
    send(pid, {self(), {:exit_pending, tid}})
    tm_exit_pending(pids, tid)
  end

  defp enter_still_pending([tid | tids], tab) do
    :ets.insert(tab, r_pending(tid: tid))
    enter_still_pending(tids, tab)
  end

  defp enter_still_pending([], _Tab) do
    :ok
  end

  def tm_retain(tid, tab, key, op) do
    case val({tab, :commit_work}) do
      [{:checkpoints, checkpoints} | _] ->
        tm_retain(tid, tab, key, op, checkpoints)

      _ ->
        :undefined
    end
  end

  def tm_retain(tid, tab, key, op, checkpoints) do
    case op do
      :clear_table ->
        oldRecs = :mnesia_lib.db_match_object(tab, :_)
        send_group_retain(oldRecs, checkpoints, tid, tab, [])
        oldRecs

      _ ->
        oldRecs = :mnesia_lib.db_get(tab, key)

        send_retain(
          checkpoints,
          {:retain, tid, tab, key, oldRecs}
        )

        oldRecs
    end
  end

  defp send_group_retain([rec | recs], checkpoints, tid, tab, [prevRec | prevRecs])
       when :erlang.element(2, rec) !=
              :erlang.element(
                2,
                prevRec
              ) do
    key = :erlang.element(2, prevRec)
    oldRecs = :lists.reverse([prevRec | prevRecs])

    send_retain(
      checkpoints,
      {:retain, tid, tab, key, oldRecs}
    )

    send_group_retain(recs, checkpoints, tid, tab, [rec])
  end

  defp send_group_retain([rec | recs], checkpoints, tid, tab, acc) do
    send_group_retain(recs, checkpoints, tid, tab, [rec | acc])
  end

  defp send_group_retain([], checkpoints, tid, tab, [prevRec | prevRecs]) do
    key = :erlang.element(2, prevRec)
    oldRecs = :lists.reverse([prevRec | prevRecs])

    send_retain(
      checkpoints,
      {:retain, tid, tab, key, oldRecs}
    )

    :ok
  end

  defp send_group_retain([], _Checkpoints, _Tid, _Tab, []) do
    :ok
  end

  defp send_retain([name | names], msg) do
    cast(name, msg)
    send_retain(names, msg)
  end

  defp send_retain([], _Msg) do
    :ok
  end

  def tm_add_copy(tab, node) when node != node() do
    case val({tab, :commit_work}) do
      [{:checkpoints, checkpoints} | _] ->
        fun = fn name ->
          call(name, {:add_copy, tab, node})
        end

        map_call(fun, checkpoints, :ok)

      _ ->
        :ok
    end
  end

  def tm_del_copy(tab, node) when node == node() do
    :mnesia_subscr.unsubscribe_table(tab)

    case val({tab, :commit_work}) do
      [{:checkpoints, checkpoints} | _] ->
        fun = fn name ->
          call(name, {:del_copy, tab, node})
        end

        map_call(fun, checkpoints, :ok)

      _ ->
        :ok
    end
  end

  def tm_change_table_copy_type(tab, from, to) do
    case val({tab, :commit_work}) do
      [{:checkpoints, checkpoints} | _] ->
        fun = fn name ->
          call(name, {:change_copy, tab, from, to})
        end

        map_call(fun, checkpoints, :ok)

      _ ->
        :ok
    end
  end

  defp map_call(fun, [name | names], res) do
    case fun.(name) do
      :ok ->
        map_call(fun, names, res)

      {:error, {:no_exists, ^name}} ->
        map_call(fun, names, res)

      {:error, reason} ->
        map_call(fun, names, {:error, reason})
    end
  end

  defp map_call(_Fun, [], res) do
    res
  end

  def deactivate(name) do
    case call(name, :get_checkpoint) do
      {:error, reason} ->
        {:error, reason}

      cp ->
        deactivate(r_checkpoint_args(cp, :nodes), name)
    end
  end

  def deactivate(nodes, name) do
    :rpc.multicall(nodes, :mnesia_checkpoint, :remote_deactivate, [name])
    :ok
  end

  def remote_deactivate(name) do
    call(name, :deactivate)
  end

  def checkpoints() do
    val(:checkpoints)
  end

  def tables_and_cookie(name) do
    case call(name, :get_checkpoint) do
      {:error, reason} ->
        {:error, reason}

      cp ->
        tabs = r_checkpoint_args(cp, :min) ++ r_checkpoint_args(cp, :max)
        cookie = r_checkpoint_args(cp, :cookie)
        {:ok, tabs, cookie}
    end
  end

  def most_local_node(name, tab) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, {:retainer, name}}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        {:error, {'No retainer attached to table', [tab, name]}}

      r ->
        writers = r_retainer(r, :writers)
        localWriter = :lists.member(node(), writers)

        cond do
          localWriter == true ->
            {:ok, node()}

          writers != [] ->
            {:ok, hd(writers)}

          true ->
            {:error, {'No retainer attached to table', [tab, name]}}
        end
    end
  end

  def really_retain(name, tab) do
    r = val({tab, {:retainer, name}})
    r_retainer(r, :really_retain)
  end

  def activate(args) do
    case args2cp(args) do
      {:ok, cp} ->
        do_activate(cp)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp args2cp(args) when is_list(args) do
    try do
      :lists.foldl(&check_arg/2, r_checkpoint_args(), args)
    catch
      :exit, reason ->
        {:error, reason}

      :error, reason ->
        {:error, reason}
    else
      cp ->
        case check_tables(cp) do
          {:error, reason} ->
            {:error, reason}

          {:ok, overriders, allTabs} ->
            arrange_retainers(cp, overriders, allTabs)
        end
    end
  end

  defp args2cp(args) do
    {:error, {:badarg, args}}
  end

  defp check_arg({:name, name}, cp) do
    case :lists.member(name, checkpoints()) do
      true ->
        exit({:already_exists, name})

      false ->
        try do
          [_ | _] = tab2retainer({:foo, name})
          r_checkpoint_args(cp, name: name)
        catch
          _, _ ->
            exit({:badarg, name})
        end
    end
  end

  defp check_arg({:allow_remote, true}, cp) do
    r_checkpoint_args(cp, allow_remote: true)
  end

  defp check_arg({:allow_remote, false}, cp) do
    r_checkpoint_args(cp, allow_remote: false)
  end

  defp check_arg({:ram_overrides_dump, true}, cp) do
    r_checkpoint_args(cp, ram_overrides_dump: true)
  end

  defp check_arg({:ram_overrides_dump, false}, cp) do
    r_checkpoint_args(cp, ram_overrides_dump: false)
  end

  defp check_arg({:ram_overrides_dump, tabs}, cp)
       when is_list(tabs) do
    r_checkpoint_args(cp, ram_overrides_dump: tabs)
  end

  defp check_arg({:min, tabs}, cp) when is_list(tabs) do
    r_checkpoint_args(cp, min: tabs)
  end

  defp check_arg({:max, tabs}, cp) when is_list(tabs) do
    r_checkpoint_args(cp, max: tabs)
  end

  defp check_arg({:ignore_new, tids}, cp) when is_list(tids) do
    r_checkpoint_args(cp, ignore_new: tids)
  end

  defp check_arg(arg, _) do
    exit({:badarg, arg})
  end

  defp check_tables(cp) do
    min = r_checkpoint_args(cp, :min)
    max = r_checkpoint_args(cp, :max)
    allTabs = min ++ max

    doubleTabs =
      for t <- min, :lists.member(t, max) do
        t
      end

    overriders = r_checkpoint_args(cp, :ram_overrides_dump)

    cond do
      doubleTabs != [] ->
        {:error,
         {:combine_error, r_checkpoint_args(cp, :name), [{:min, doubleTabs}, {:max, doubleTabs}]}}

      min == [] and max == [] ->
        {:error, {:combine_error, r_checkpoint_args(cp, :name), [{:min, min}, {:max, max}]}}

      overriders == false ->
        {:ok, [], allTabs}

      overriders == true ->
        {:ok, allTabs, allTabs}

      is_list(overriders) ->
        case (for t <- overriders, not :lists.member(t, min) do
                t
              end) do
          [] ->
            case (for t <- overriders, not :lists.member(t, max) do
                    t
                  end) do
              [] ->
                {:ok, overriders, allTabs}

              outsiders ->
                {:error,
                 {:combine_error, r_checkpoint_args(cp, :name),
                  [{:ram_overrides_dump, outsiders}, {:max, outsiders}]}}
            end

          outsiders ->
            {:error,
             {:combine_error, r_checkpoint_args(cp, :name),
              [{:ram_overrides_dump, outsiders}, {:min, outsiders}]}}
        end
    end
  end

  defp arrange_retainers(cp, overriders, allTabs) do
    r = r_retainer(cp_name: r_checkpoint_args(cp, :name))

    try do
      for tab <- allTabs do
        r_retainer(r, tab_name: tab, writers: select_writers(cp, tab))
      end
    catch
      reason ->
        {:error, reason}
    else
      retainers ->
        {:ok,
         r_checkpoint_args(cp,
           ram_overrides_dump: overriders,
           retainers: retainers,
           nodes: writers(retainers)
         )}
    end
  end

  defp select_writers(cp, tab) do
    case filter_remote(
           cp,
           val({tab, :active_replicas})
         ) do
      [] ->
        throw(
          {'Cannot prepare checkpoint (replica not available)',
           [tab, r_checkpoint_args(cp, :name)]}
        )

      writers ->
        this = node()

        case {:lists.member(tab, r_checkpoint_args(cp, :max)), :lists.member(this, writers)} do
          {true, _} ->
            writers

          {false, true} ->
            [this]

          {false, false} ->
            [hd(writers)]
        end
    end
  end

  defp filter_remote(cp, writers)
       when r_checkpoint_args(cp, :allow_remote) == true do
    writers
  end

  defp filter_remote(_Cp, writers) do
    this = node()

    case :lists.member(this, writers) do
      true ->
        [this]

      false ->
        []
    end
  end

  defp writers(retainers) do
    fun = fn r, acc ->
      r_retainer(r, :writers) ++ acc
    end

    writers = :lists.foldl(fun, [], retainers)
    :mnesia_lib.uniq(writers)
  end

  defp do_activate(cp) do
    name = r_checkpoint_args(cp, :name)
    nodes = r_checkpoint_args(cp, :nodes)

    case :mnesia_tm.prepare_checkpoint(nodes, cp) do
      {replies, []} ->
        check_prep(replies, name, nodes, r_checkpoint_args(cp, :ignore_new))

      {_, badNodes} ->
        {:error, {'Cannot prepare checkpoint (bad nodes)', [name, badNodes]}}
    end
  end

  defp check_prep([{:ok, name, ignoreNew, _Node} | replies], name, nodes, ignoreNew) do
    check_prep(replies, name, nodes, ignoreNew)
  end

  defp check_prep([{:error, reason} | _Replies], name, _Nodes, _IgnoreNew) do
    {:error, {'Cannot prepare checkpoint (bad reply)', [name, reason]}}
  end

  defp check_prep([{:badrpc, reason} | _Replies], name, _Nodes, _IgnoreNew) do
    {:error, {'Cannot prepare checkpoint (badrpc)', [name, reason]}}
  end

  defp check_prep([], name, nodes, ignoreNew) do
    collect_pending(name, nodes, ignoreNew)
  end

  defp collect_pending(name, nodes, ignoreNew) do
    case :rpc.multicall(nodes, :mnesia_checkpoint, :call, [name, :collect_pending]) do
      {replies, []} ->
        try do
          unionTab = _ = :ets.new(:mnesia_union, [:bag])
          compute_union(replies, nodes, name, unionTab, ignoreNew)
        catch
          :error, reason ->
            msg = 'Cannot create an ets table pending union'
            {:error, {:system_limit, msg, reason}}
        end

      {_, badNodes} ->
        deactivate(nodes, name)
        {:error, {'Cannot collect from pending checkpoint', name, badNodes}}
    end
  end

  defp compute_union([{:ok, pending} | replies], nodes, name, unionTab, ignoreNew) do
    add_pending(pending, unionTab)
    compute_union(replies, nodes, name, unionTab, ignoreNew)
  end

  defp compute_union([{:error, reason} | _Replies], nodes, name, unionTab, _IgnoreNew) do
    deactivate(nodes, name)
    :ets.delete(unionTab)
    {:error, reason}
  end

  defp compute_union([{:badrpc, reason} | _Replies], nodes, name, unionTab, _IgnoreNew) do
    deactivate(nodes, name)
    :ets.delete(unionTab)
    {:error, {:badrpc, reason}}
  end

  defp compute_union([], nodes, name, unionTab, ignoreNew) do
    send_activate(nodes, nodes, name, unionTab, ignoreNew)
  end

  defp add_pending([p | pending], unionTab) do
    add_pending_node(r_pending(p, :disc_nodes), r_pending(p, :tid), unionTab)
    add_pending_node(r_pending(p, :ram_nodes), r_pending(p, :tid), unionTab)
    add_pending(pending, unionTab)
  end

  defp add_pending([], _UnionTab) do
    :ok
  end

  defp add_pending_node([node | nodes], tid, unionTab) do
    :ets.insert(unionTab, {node, tid})
    add_pending_node(nodes, tid, unionTab)
  end

  defp add_pending_node([], _Tid, _UnionTab) do
    :ok
  end

  defp send_activate([node | nodes], allNodes, name, unionTab, ignoreNew) do
    pending =
      for {_, tid} <- :ets.lookup(unionTab, node),
          not :lists.member(tid, ignoreNew) do
        tid
      end

    case :rpc.call(node, :mnesia_checkpoint, :call, [name, {:activate, pending}]) do
      :activated ->
        send_activate(nodes, allNodes, name, unionTab, ignoreNew)

      {:badrpc, reason} ->
        deactivate(nodes, name)
        :ets.delete(unionTab)
        {:error, {'Activation failed (bad node)', name, node, reason}}

      {:error, reason} ->
        deactivate(nodes, name)
        :ets.delete(unionTab)
        {:error, {'Activation failed', name, node, reason}}
    end
  end

  defp send_activate([], allNodes, name, unionTab, _IgnoreNew) do
    :ets.delete(unionTab)
    {:ok, name, allNodes}
  end

  def cast(name, msg) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {:checkpoint, name}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        {:error, {:no_exists, name}}

      pid when is_pid(pid) ->
        send(pid, {self(), msg})
        {:ok, pid}
    end
  end

  def call(name, msg) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {:checkpoint, name}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        {:error, {:no_exists, name}}

      pid when is_pid(pid) ->
        monitor = :erlang.monitor(:process, pid)
        send(pid, {self(), msg})
        self = self()

        receive do
          {:EXIT, ^pid, reason} ->
            {:error, {'Got exit', [name, reason]}}

          {:DOWN, ^monitor, _, ^pid, reason} ->
            {:error, {'Got exit', [name, reason]}}

          {^name, ^self, reply} ->
            :erlang.demonitor(monitor)
            reply
        end

      error ->
        error
    end
  end

  defp abcast(nodes, name, msg) do
    :rpc.eval_everywhere(nodes, :mnesia_checkpoint, :cast, [name, msg])
  end

  defp reply(:nopid, _Name, _Reply) do
    :ignore
  end

  defp reply(replyTo, name, reply) do
    send(replyTo, {name, replyTo, reply})
  end

  defp start_retainer(cp) do
    name = r_checkpoint_args(cp, :name)

    case :supervisor.start_child(
           :mnesia_checkpoint_sup,
           [cp]
         ) do
      {:ok, _Pid} ->
        {:ok, name, r_checkpoint_args(cp, :ignore_new), node()}

      {:error, reason} ->
        {:error, {'Cannot create checkpoint retainer', name, node(), reason}}
    end
  end

  def start(cp) do
    name = r_checkpoint_args(cp, :name)
    args = [r_checkpoint_args(cp, supervisor: self())]
    :mnesia_monitor.start_proc({:mnesia_checkpoint, name}, :mnesia_checkpoint, :init, args)
  end

  def init(cp) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.process_flag(:priority, :high)
    name = r_checkpoint_args(cp, :name)
    props = [:set, :public, {:keypos, 2}]

    try do
      _ = :ets.new(:mnesia_pending_checkpoint, props)
    catch
      :error, reason ->
        msg = 'Cannot create an ets table for pending transactions'
        error = {:error, {:system_limit, name, msg, reason}}
        :proc_lib.init_ack(r_checkpoint_args(cp, :supervisor), error)
    else
      pendingTab ->
        rs =
          for r <- r_checkpoint_args(cp, :retainers) do
            prepare_tab(cp, r)
          end

        cp2 = r_checkpoint_args(cp, retainers: rs, pid: self(), pending_tab: pendingTab)
        add(:pending_checkpoint_pids, self())
        add(:pending_checkpoints, pendingTab)
        set({:checkpoint, name}, self())
        add(:checkpoints, name)
        dbg_out('Checkpoint ~p (~p) started~n', [name, self()])
        :proc_lib.init_ack(r_checkpoint_args(cp2, :supervisor), {:ok, self()})
        retainer_loop(cp2)
    end
  end

  defp prepare_tab(cp, r) do
    tab = r_retainer(r, :tab_name)
    prepare_tab(cp, r, val({tab, :storage_type}))
  end

  defp prepare_tab(cp, r, storage) do
    tab = r_retainer(r, :tab_name)
    name = r_retainer(r, :cp_name)

    case :lists.member(node(), r_retainer(r, :writers)) do
      true ->
        r2 = retainer_create(cp, r, tab, name, storage)
        set({tab, {:retainer, name}}, r2)
        add({tab, :checkpoints}, name)
        add_chkp_info(tab, name)
        r2

      false ->
        set({tab, {:retainer, name}}, r_retainer(r, store: :undefined))
        r
    end
  end

  defp add_chkp_info(tab, name) do
    case val({tab, :commit_work}) do
      [{:checkpoints, oldList} | commitList] ->
        case :lists.member(name, oldList) do
          true ->
            :ok

          false ->
            newC = [{:checkpoints, [name | oldList]} | commitList]
            :mnesia_lib.set({tab, :commit_work}, newC)
        end

      commitList ->
        chkp = {:checkpoints, [name]}

        :mnesia_lib.set(
          {tab, :commit_work},
          [chkp | commitList]
        )
    end
  end

  defp tab2retainer({tab, name}) do
    flatName = :lists.flatten(:io_lib.write(name))
    :mnesia_lib.dir(:lists.concat([:mnesia_checkpoint, '_', tab, '_', flatName, '.RET']))
  end

  defp retainer_create(_Cp, r, tab, name, ext = {:ext, alias, mod}) do
    t = {tab, :retainer, name}
    p = :mnesia_schema.cs2list(val({tab, :cstruct}))
    mod.delete_table(alias, t)
    :ok = mod.create_table(alias, t, p)
    cs = val({tab, :cstruct})
    mod.load_table(alias, t, {:retainer, :create_table}, :mnesia_schema.cs2list(cs))
    dbg_out('Checkpoint retainer created ~p ~tp~n', [name, tab])
    r_retainer(r, store: {ext, t}, really_retain: true)
  end

  defp retainer_create(_Cp, r, tab, name, :disc_only_copies) do
    fname = tab2retainer({tab, name})
    :file.delete(fname)
    args = [{:file, fname}, {:type, :set}, {:keypos, 2}, {:repair, false}]
    {:ok, _} = :mnesia_lib.dets_sync_open({tab, name}, args)
    dbg_out('Checkpoint retainer created ~p ~tp~n', [name, tab])
    r_retainer(r, store: {:dets, {tab, name}}, really_retain: true)
  end

  defp retainer_create(cp, r, tab, name, storage) do
    t =
      _ =
      :ets.new(
        :mnesia_retainer,
        [:set, :public, {:keypos, 2}]
      )

    overriders = r_checkpoint_args(cp, :ram_overrides_dump)
    reallyR = r_retainer(r, :really_retain)
    reallyCp = :lists.member(tab, overriders)
    reallyR2 = prepare_ram_tab(tab, t, storage, reallyR, reallyCp)
    dbg_out('Checkpoint retainer created ~p ~tp~n', [name, tab])
    r_retainer(r, store: {:ets, t}, really_retain: reallyR2)
  end

  defp prepare_ram_tab(tab, t, :ram_copies, true, false) do
    fname = :mnesia_lib.tab2dcd(tab)

    case :mnesia_lib.exists(fname) do
      true ->
        log =
          :mnesia_log.open_log(
            :prepare_ram_tab,
            :mnesia_log.dcd_log_header(),
            fname,
            true,
            :mnesia_monitor.get_env(:auto_repair),
            :read_only
          )

        add = fn rec ->
          key = :erlang.element(2, rec)

          recs =
            case :ets.lookup(t, key) do
              [] ->
                []

              [{_, _, old}] ->
                old
            end

          :ets.insert(t, {tab, key, [rec | recs]})
          :continue
        end

        traverse_dcd(:mnesia_log.chunk_log(log, :start), log, add)
        :mnesia_log.close_log(log)

      false ->
        :ok
    end

    false
  end

  defp prepare_ram_tab(_, _, _, reallyRetain, _) do
    reallyRetain
  end

  defp traverse_dcd({cont, [logH | rest]}, log, fun)
       when elem(logH, 0) === :log_header and
              r_log_header(logH, :log_kind) == :dcd_log and
              r_log_header(logH, :log_version) >= '1.0' do
    traverse_dcd({cont, rest}, log, fun)
  end

  defp traverse_dcd({cont, recs}, log, fun) do
    :lists.foreach(fun, recs)
    traverse_dcd(:mnesia_log.chunk_log(log, cont), log, fun)
  end

  defp traverse_dcd(:eof, _Log, _Fun) do
    :ok
  end

  defp retainer_get({{:ext, alias, mod}, store}, key) do
    mod.lookup(alias, store, key)
  end

  defp retainer_get({:ets, store}, key) do
    :ets.lookup(store, key)
  end

  defp retainer_get({:dets, store}, key) do
    :dets.lookup(store, key)
  end

  defp retainer_put({{:ext, alias, mod}, store}, val) do
    mod.insert(alias, store, val)
  end

  defp retainer_put({:ets, store}, val) do
    :ets.insert(store, val)
  end

  defp retainer_put({:dets, store}, val) do
    :dets.insert(store, val)
  end

  defp retainer_first({{:ext, alias, mod}, store}) do
    mod.first(alias, store)
  end

  defp retainer_first({:ets, store}) do
    :ets.first(store)
  end

  defp retainer_first({:dets, store}) do
    :dets.first(store)
  end

  defp retainer_next({{:ext, alias, mod}, store}, key) do
    mod.next(alias, store, key)
  end

  defp retainer_next({:ets, store}, key) do
    :ets.next(store, key)
  end

  defp retainer_next({:dets, store}, key) do
    :dets.next(store, key)
  end

  defp retainer_fixtable(tab, bool) when is_atom(tab) do
    :mnesia_lib.db_fixtable(val({tab, :storage_type}), tab, bool)
  end

  defp retainer_fixtable({ext = {:ext, _, _}, tab}, bool) do
    :mnesia_lib.db_fixtable(ext, tab, bool)
  end

  defp retainer_fixtable({:ets, tab}, bool) do
    :mnesia_lib.db_fixtable(:ram_copies, tab, bool)
  end

  defp retainer_fixtable({:dets, tab}, bool) do
    :mnesia_lib.db_fixtable(:disc_only_copies, tab, bool)
  end

  defp retainer_delete({{:ext, alias, mod}, store}) do
    mod.close_table(alias, store)
    mod.delete_table(alias, store)
  end

  defp retainer_delete({:ets, store}) do
    :ets.delete(store)
  end

  defp retainer_delete({:dets, store}) do
    :mnesia_lib.dets_sync_close(store)
    fname = tab2retainer(store)
    :file.delete(fname)
  end

  defp retainer_loop(cp = r_checkpoint_args(is_activated: false, name: name)) do
    receive do
      {from, {:activate, pending}} ->
        stillPending = :mnesia_recover.still_pending(pending)
        enter_still_pending(stillPending, r_checkpoint_args(cp, :pending_tab))

        local =
          for r_tid(pid: pid) = tid <- stillPending,
              node(pid) !== node() do
            tid
          end

        cp2 = maybe_activate(r_checkpoint_args(cp, wait_for_old: local))
        reply(from, name, :activated)
        retainer_loop(cp2)

      {_From, {:exit_pending, tid}}
      when is_list(r_checkpoint_args(cp, :wait_for_old)) ->
        stillPending = :lists.delete(tid, r_checkpoint_args(cp, :wait_for_old))
        cp2 = r_checkpoint_args(cp, wait_for_old: stillPending)
        cp3 = maybe_activate(cp2)
        retainer_loop(cp3)

      {from, :deactivate} ->
        do_stop(cp)
        reply(from, name, :deactivated)
        :erlang.unlink(from)
        exit(:shutdown)

      {from, :get_checkpoint} ->
        reply(from, name, cp)
        retainer_loop(cp)

      {_From, {:add_retainer, r, node}} ->
        cp2 = do_add_retainer(cp, r, node)
        retainer_loop(cp2)

      {from, :collect_pending} ->
        pendingTab = r_checkpoint_args(cp, :pending_tab)
        del(:pending_checkpoints, pendingTab)
        pending = :ets.match_object(pendingTab, :_)
        reply(from, name, {:ok, pending})
        retainer_loop(cp)

      {_From, {:mnesia_down, node}} ->
        cp2 = do_del_retainers(cp, node)
        retainer_loop(cp2)

      {:EXIT, parent, _} when parent == r_checkpoint_args(cp, :supervisor) ->
        exit(:shutdown)

      {:EXIT, from, _Reason} ->
        iters =
          for iter <- r_checkpoint_args(cp, :iterators),
              check_iter(from, iter) do
            iter
          end

        retainer_loop(r_checkpoint_args(cp, iterators: iters))

      {:system, from, msg} ->
        dbg_out('~p got {system, ~p, ~tp}~n', [:mnesia_checkpoint, from, msg])

        :sys.handle_system_msg(
          msg,
          from,
          r_checkpoint_args(cp, :supervisor),
          :mnesia_checkpoint,
          [],
          cp
        )
    end
  end

  defp retainer_loop(cp = r_checkpoint_args(name: name)) do
    receive do
      {_From, {:retain, tid, tab, key, oldRecs}} ->
        r =
          try do
            :ets.lookup_element(:mnesia_gvar, {tab, {:retainer, name}}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end

        pendingTab = r_checkpoint_args(cp, :pending_tab)

        case elem(r, 0) === :retainer and r_retainer(r, :really_retain) do
          true ->
            store = r_retainer(r, :store)

            try do
              true = :ets.member(pendingTab, tid)

              case retainer_get(store, key) do
                [] ->
                  :ignore

                _ ->
                  :ets.delete(:erlang.element(2, store), key)
              end
            catch
              _, _ ->
                case retainer_get(store, key) do
                  [] ->
                    retainer_put(store, {tab, key, oldRecs})

                  _ ->
                    :already_retained
                end
            end

          false ->
            :ignore
        end

        retainer_loop(cp)

      {from, :get_checkpoint} ->
        reply(from, name, cp)
        retainer_loop(cp)

      {from, {:add_copy, tab, node}} ->
        {res, cp2} = do_add_copy(cp, tab, node)
        reply(from, name, res)
        retainer_loop(cp2)

      {from, {:del_copy, tab, node}} ->
        cp2 = do_del_copy(cp, tab, node)
        reply(from, name, :ok)
        retainer_loop(cp2)

      {from, {:change_copy, tab, from, to}} ->
        cp2 = do_change_copy(cp, tab, from, to)
        reply(from, name, :ok)
        retainer_loop(cp2)

      {_From, {:add_retainer, r, node}} ->
        cp2 = do_add_retainer(cp, r, node)
        retainer_loop(cp2)

      {_From, {:del_retainer, r, node}} ->
        cp2 = do_del_retainer(cp, r, node)
        retainer_loop(cp2)

      {from, {:iter_begin, iter}} ->
        cp2 = iter_begin(cp, from, iter)
        retainer_loop(cp2)

      {from, {:iter_end, iter}} ->
        try do
          retainer_fixtable(r_iter(iter, :oid_tab), false)
        catch
          :error, _ ->
            :ok
        end

        iters = r_checkpoint_args(cp, :iterators) -- [iter]
        reply(from, name, :ok)
        retainer_loop(r_checkpoint_args(cp, iterators: iters))

      {_From, {:exit_pending, _Tid}} ->
        retainer_loop(cp)

      {from, :deactivate} ->
        do_stop(cp)
        reply(from, name, :deactivated)
        :erlang.unlink(from)
        exit(:shutdown)

      {_From, {:mnesia_down, node}} ->
        cp2 = do_del_retainers(cp, node)
        retainer_loop(cp2)

      {:EXIT, parent, _} when parent == r_checkpoint_args(cp, :supervisor) ->
        exit(:shutdown)

      {:EXIT, from, _Reason} ->
        iters =
          for iter <- r_checkpoint_args(cp, :iterators),
              check_iter(from, iter) do
            iter
          end

        retainer_loop(r_checkpoint_args(cp, iterators: iters))

      {:system, from, msg} ->
        dbg_out('~p got {system, ~p, ~tp}~n', [:mnesia_checkpoint, from, msg])

        :sys.handle_system_msg(
          msg,
          from,
          r_checkpoint_args(cp, :supervisor),
          :mnesia_checkpoint,
          [],
          cp
        )

      msg ->
        dbg_out('~p got ~tp~n', [:mnesia_checkpoint, msg])
    end
  end

  defp maybe_activate(cp)
       when r_checkpoint_args(cp, :wait_for_old) == [] and
              r_checkpoint_args(cp, :is_activated) == false do
    r_checkpoint_args(cp, is_activated: true)
  end

  defp maybe_activate(cp) do
    cp
  end

  defp iter_begin(cp, from, iter) do
    name = r_checkpoint_args(cp, :name)
    r = val({r_iter(iter, :tab_name), {:retainer, name}})
    iter2 = init_tabs(r, iter)
    iter3 = r_iter(iter2, pid: from)
    retainer_fixtable(r_iter(iter3, :oid_tab), true)
    iters = [iter3 | r_checkpoint_args(cp, :iterators)]
    reply(from, name, {:ok, iter3, self()})
    r_checkpoint_args(cp, iterators: iters)
  end

  defp do_stop(cp) do
    name = r_checkpoint_args(cp, :name)
    del(:pending_checkpoints, r_checkpoint_args(cp, :pending_tab))
    del(:pending_checkpoint_pids, self())
    del(:checkpoints, name)
    unset({:checkpoint, name})
    :lists.foreach(&deactivate_tab/1, r_checkpoint_args(cp, :retainers))
    iters = r_checkpoint_args(cp, :iterators)

    for r_iter(main_tab: tab) <- iters do
      try do
        retainer_fixtable(tab, false)
      catch
        :error, _ ->
          :ok
      end
    end

    :ok
  end

  defp deactivate_tab(r) do
    name = r_retainer(r, :cp_name)
    tab = r_retainer(r, :tab_name)

    try do
      active = :lists.member(node(), r_retainer(r, :writers))

      case r_retainer(r, :store) do
        :undefined ->
          :ignore

        store when active == true ->
          retainer_delete(store)

        _ ->
          :ignore
      end

      unset({tab, {:retainer, name}})
      del({tab, :checkpoints}, name)
      del_chkp_info(tab, name)
    catch
      _, _ ->
        :ignore
    end
  end

  defp del_chkp_info(tab, name) do
    case val({tab, :commit_work}) do
      [{:checkpoints, chkList} | rest] ->
        case :lists.delete(name, chkList) do
          [] ->
            :mnesia_lib.set({tab, :commit_work}, rest)

          newList ->
            :mnesia_lib.set(
              {tab, :commit_work},
              [{:checkpoints, newList} | rest]
            )
        end

      _ ->
        :ignore
    end
  end

  defp do_del_retainers(cp, node) do
    rs =
      for r <- r_checkpoint_args(cp, :retainers) do
        do_del_retainer2(cp, r, node)
      end

    r_checkpoint_args(cp, retainers: rs, nodes: writers(rs))
  end

  defp do_del_retainer2(cp, r, node) do
    writers = r_retainer(r, :writers) -- [node]
    r2 = r_retainer(r, writers: writers)

    set(
      {r_retainer(r2, :tab_name), {:retainer, r_retainer(r2, :cp_name)}},
      r2
    )

    cond do
      writers == [] ->
        event = {:mnesia_checkpoint_deactivated, r_checkpoint_args(cp, :name)}
        :mnesia_lib.report_system_event(event)
        do_stop(cp)
        exit(:shutdown)

      node == node() ->
        deactivate_tab(r)

        set(
          {r_retainer(r2, :tab_name), {:retainer, r_retainer(r2, :cp_name)}},
          r2
        )

        r2

      true ->
        r2
    end
  end

  defp do_del_retainer(cp, r0, node) do
    {r, rest} = find_retainer(r0, r_checkpoint_args(cp, :retainers), [])
    r2 = do_del_retainer2(cp, r, node)
    rs = [r2 | rest]
    r_checkpoint_args(cp, retainers: rs, nodes: writers(rs))
  end

  defp do_del_copy(cp, tab, thisNode) when thisNode == node() do
    name = r_checkpoint_args(cp, :name)
    others = r_checkpoint_args(cp, :nodes) -- [thisNode]
    r = val({tab, {:retainer, name}})
    abcast(others, name, {:del_retainer, r, thisNode})
    do_del_retainer(cp, r, thisNode)
  end

  defp do_add_copy(cp, tab, node) when node != node() do
    case :lists.member(tab, r_checkpoint_args(cp, :max)) do
      false ->
        {:ok, cp}

      true ->
        name = r_checkpoint_args(cp, :name)
        r0 = val({tab, {:retainer, name}})
        w = r_retainer(r0, :writers)
        r = r_retainer(r0, writers: w ++ [node])

        case :lists.member(node, r_checkpoint_args(cp, :nodes)) do
          true ->
            send_retainer(cp, r, node)

          false ->
            case tm_remote_prepare(node, cp) do
              {:ok, ^name, _IgnoreNew, ^node} ->
                case :lists.member(:schema, r_checkpoint_args(cp, :max)) do
                  true ->
                    rS0 = val({:schema, {:retainer, name}})
                    wS = r_retainer(rS0, :writers)
                    rS1 = r_retainer(rS0, writers: wS ++ [node])
                    {:ok, cp1} = send_retainer(cp, rS1, node)
                    send_retainer(cp1, r, node)

                  false ->
                    send_retainer(cp, r, node)
                end

              {:badrpc, reason} ->
                {{:error, {:badrpc, reason}}, cp}

              {:error, reason} ->
                {{:error, reason}, cp}
            end
        end
    end
  end

  defp tm_remote_prepare(node, cp) do
    :rpc.call(node, :mnesia_checkpoint, :tm_prepare, [cp])
  end

  defp do_add_retainer(cp, r0, node) do
    writers = r_retainer(r0, :writers)
    {r, rest} = find_retainer(r0, r_checkpoint_args(cp, :retainers), [])

    newRet =
      cond do
        node == node() ->
          prepare_tab(cp, r_retainer(r, writers: writers))

        true ->
          r_retainer(r, writers: writers)
      end

    rs = [newRet | rest]

    set(
      {r_retainer(newRet, :tab_name), {:retainer, r_retainer(newRet, :cp_name)}},
      newRet
    )

    r_checkpoint_args(cp, retainers: rs, nodes: writers(rs))
  end

  defp find_retainer(
         r_retainer(cp_name: cP, tab_name: tab),
         [ret = r_retainer(cp_name: cP, tab_name: tab) | r],
         acc
       ) do
    {ret, r ++ acc}
  end

  defp find_retainer(ret, [h | r], acc) do
    find_retainer(ret, r, [h | acc])
  end

  defp send_retainer(cp, r, node) do
    name = r_checkpoint_args(cp, :name)
    nodes0 = r_checkpoint_args(cp, :nodes) -- [node]
    nodes = nodes0 -- [node()]
    msg = {:add_retainer, r, node}
    abcast(nodes, name, msg)
    {:ok, _} = :rpc.call(node, :mnesia_checkpoint, :cast, [name, msg])
    store = r_retainer(r, :store)
    send_retainer2(node, name, store, retainer_first(store))
    cp2 = do_add_retainer(cp, r, node)
    {:ok, cp2}
  end

  defp send_retainer2(_, _, _, :"$end_of_table") do
    :ok
  end

  defp send_retainer2(node, name, store, key) do
    [{tab, _, records}] = retainer_get(store, key)
    abcast([node], name, {:retain, {:dirty, :send_retainer}, tab, key, records})
    send_retainer2(node, name, store, retainer_next(store, key))
  end

  defp do_change_copy(cp, tab, fromType, toType) do
    name = r_checkpoint_args(cp, :name)
    r = val({tab, {:retainer, name}})
    r2 = prepare_tab(cp, r, toType)
    {_, old} = r_retainer(r, :store)
    {_, new} = r_retainer(r2, :store)
    fname = tab2retainer({tab, name})

    cond do
      fromType == :disc_only_copies ->
        :mnesia_lib.dets_sync_close(old)
        :loaded = :mnesia_lib.dets_to_ets(old, new, fname, :set, :no, :yes)
        :ok = :file.delete(fname)

      toType == :disc_only_copies ->
        tabSize = :ets.info(old, :size)

        props = [
          {:file, fname},
          {:type, :set},
          {:keypos, 2},
          {:estimated_no_objects, tabSize + 256},
          {:repair, false}
        ]

        {:ok, _} = :mnesia_lib.dets_sync_open(new, props)
        :ok = :mnesia_dumper.raw_dump_table(new, old)
        :ets.delete(old)

      true ->
        :ignore
    end

    pos = r_retainer(:tab_name)
    rs = :lists.keyreplace(tab, pos, r_checkpoint_args(cp, :retainers), r2)
    r_checkpoint_args(cp, retainers: rs, nodes: writers(rs))
  end

  defp check_iter(from, iter) when r_iter(iter, :pid) == from do
    try do
      retainer_fixtable(r_iter(iter, :oid_tab), false)
    catch
      :error, _ ->
        :ok
    end

    false
  end

  defp check_iter(_From, _Iter) do
    true
  end

  defp init_tabs(r, iter) do
    {kind, _} = store = r_retainer(r, :store)
    main = {kind, r_iter(iter, :tab_name)}
    ret = store
    iter2 = r_iter(iter, main_tab: main, retainer_tab: ret)

    case r_iter(iter, :source) do
      :table ->
        r_iter(iter2, oid_tab: main)

      :retainer ->
        r_iter(iter2, oid_tab: ret)
    end
  end

  def iterate(name, tab, fun, acc, source, val) do
    iter0 = r_iter(tab_name: tab, source: source, val: val)

    case call(name, {:iter_begin, iter0}) do
      {:error, reason} ->
        {:error, reason}

      {:ok, iter, pid} ->
        :erlang.link(pid)

        res =
          try do
            iter(fun, acc, iter)
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end

        :erlang.unlink(pid)
        call(name, {:iter_end, iter})

        case res do
          {:EXIT, reason} ->
            {:error, reason}

          {:error, reason} ->
            {:error, reason}

          acc2 ->
            {:ok, acc2}
        end
    end
  end

  defp iter(fun, acc, iter) do
    iter(fun, acc, iter, retainer_first(r_iter(iter, :oid_tab)))
  end

  defp iter(fun, acc, iter, key) do
    case get_records(iter, key) do
      {:"$end_of_table", []} ->
        fun.([], acc)

      {:"$end_of_table", records} ->
        acc2 = fun.(records, acc)
        fun.([], acc2)

      {next, records} ->
        acc2 = fun.(records, acc)
        iter(fun, acc2, iter, next)
    end
  end

  def stop_iteration(reason) do
    throw({:error, {:stopped, reason}})
  end

  defp get_records(iter, key) do
    get_records(iter, key, 500, [])
  end

  defp get_records(_Iter, key, 0, acc) do
    {key, :lists.append(:lists.reverse(acc))}
  end

  defp get_records(_Iter, :"$end_of_table", _I, acc) do
    {:"$end_of_table", :lists.append(:lists.reverse(acc))}
  end

  defp get_records(iter, key, i, acc) do
    recs = get_val(iter, key)
    next = retainer_next(r_iter(iter, :oid_tab), key)
    get_records(iter, next, i - 1, [recs | acc])
  end

  defp get_val(iter, key) when r_iter(iter, :val) == :latest do
    get_latest_val(iter, key)
  end

  defp get_val(iter, key) when r_iter(iter, :val) == :checkpoint do
    get_checkpoint_val(iter, key)
  end

  defp get_latest_val(iter, key) when r_iter(iter, :source) == :table do
    retainer_get(r_iter(iter, :main_tab), key)
  end

  defp get_latest_val(iter, key) when r_iter(iter, :source) == :retainer do
    deleteOid = {r_iter(iter, :tab_name), key}
    [deleteOid | retainer_get(r_iter(iter, :main_tab), key)]
  end

  defp get_checkpoint_val(iter, key) when r_iter(iter, :source) == :table do
    retainer_get(r_iter(iter, :main_tab), key)
  end

  defp get_checkpoint_val(iter, key) when r_iter(iter, :source) == :retainer do
    deleteOid = {r_iter(iter, :tab_name), key}

    case retainer_get(r_iter(iter, :retainer_tab), key) do
      [{_, _, []}] ->
        [deleteOid]

      [{_, _, records}] ->
        [deleteOid | records]
    end
  end

  def system_continue(_Parent, _Debug, cp) do
    retainer_loop(cp)
  end

  def system_terminate(_Reason, _Parent, _Debug, cp) do
    do_stop(cp)
  end

  def system_code_change(cp, _Module, _OldVsn, _Extra) do
    {:ok, cp}
  end

  defp val(var) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, var, 2)
          catch
            :error, _ ->
              {:EXIT, __STACKTRACE__}
          end) do
      {:EXIT, stacktrace} ->
        :mnesia_lib.other_val(var, stacktrace)

      value ->
        value
    end
  end
end
