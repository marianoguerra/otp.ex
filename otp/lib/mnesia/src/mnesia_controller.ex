defmodule :m_mnesia_controller do
  use Bitwise
  import :mnesia_lib, only: [add: 2, dbg_out: 2, error: 2, fatal: 2, set: 2, verbose: 2]
  @behaviour :gen_server
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

  Record.defrecord(:r_state, :state,
    supervisor: :undefined,
    schema_is_merged: false,
    early_msgs: [],
    loader_pid: [],
    loader_queue: :undefined,
    sender_pid: [],
    sender_queue: [],
    late_loader_queue: :undefined,
    dumper_pid: :undefined,
    dumper_queue: [],
    others: [],
    dump_log_timer_ref: :undefined,
    is_stopping: false
  )

  defp get_senders(r_state(sender_pid: pids)) when is_list(pids) do
    pids
  end

  defp get_loaders(r_state(loader_pid: pids)) when is_list(pids) do
    pids
  end

  defp max_loaders() do
    case (try do
            :ets.lookup_element(:mnesia_gvar, :no_table_loaders, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        :mnesia_lib.set(:no_table_loaders, 1)
        1

      val ->
        val
    end
  end

  Record.defrecord(:r_schema_commit_lock, :schema_commit_lock, owner: :undefined)
  Record.defrecord(:r_block_controller, :block_controller, owner: :undefined)

  Record.defrecord(:r_dump_log, :dump_log,
    initiated_by: :undefined,
    opt_reply_to: :undefined,
    operation: :dump_log
  )

  Record.defrecord(:r_net_load, :net_load,
    table: :undefined,
    reason: :undefined,
    opt_reply_to: :undefined,
    cstruct: :unknown
  )

  Record.defrecord(:r_send_table, :send_table,
    table: :undefined,
    receiver_pid: :undefined,
    remote_storage: :undefined
  )

  Record.defrecord(:r_disc_load, :disc_load,
    table: :undefined,
    reason: :undefined,
    opt_reply_to: :undefined
  )

  Record.defrecord(:r_late_load, :late_load,
    table: :undefined,
    reason: :undefined,
    opt_reply_to: :undefined,
    loaders: :undefined
  )

  Record.defrecord(:r_loader_done, :loader_done,
    worker_pid: :undefined,
    is_loaded: :undefined,
    table_name: :undefined,
    needs_announce: :undefined,
    needs_sync: :undefined,
    needs_reply: :undefined,
    reply_to: :undefined,
    reply: :undefined
  )

  Record.defrecord(:r_sender_done, :sender_done,
    worker_pid: :undefined,
    worker_res: :undefined,
    table_name: :undefined
  )

  Record.defrecord(:r_dumper_done, :dumper_done,
    worker_pid: :undefined,
    worker_res: :undefined
  )

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

  def start() do
    :gen_server.start_link({:local, :mnesia_controller}, :mnesia_controller, [self()], [
      {:timeout, :infinity}
    ])
  end

  def sync_dump_log(initBy) do
    call({:sync_dump_log, initBy})
  end

  def async_dump_log(initBy) do
    send(:mnesia_controller, {:async_dump_log, initBy})
    :ok
  end

  def snapshot_dcd(tables) when is_list(tables) do
    case (for t <- tables,
              :mnesia_lib.storage_type_at_node(
                node(),
                t
              ) !== :disc_copies do
            t
          end) do
      [] ->
        call({:snapshot_dcd, tables})

      badTabs ->
        {:error, {:not_disc_copies, badTabs}}
    end
  end

  def wait_for_tables(tabs, timeout)
      when is_list(tabs) and
             timeout == :infinity do
    do_wait_for_tables(tabs, timeout)
  end

  def wait_for_tables(tabs, timeout)
      when is_list(tabs) and
             is_integer(timeout) and timeout >= 0 do
    do_wait_for_tables(tabs, timeout)
  end

  def wait_for_tables(tabs, timeout) do
    {:error, {:badarg, tabs, timeout}}
  end

  defp do_wait_for_tables(tabs, 0) do
    reply_wait(tabs)
  end

  defp do_wait_for_tables(tabs, timeout) do
    pid = spawn_link(:mnesia_controller, :wait_for_tables_init, [self(), tabs])

    receive do
      {:mnesia_controller, ^pid, res} ->
        res

      {:EXIT, ^pid, _} ->
        reply_wait(tabs)
    after
      timeout ->
        :erlang.unlink(pid)
        :erlang.exit(pid, :timeout)
        reply_wait(tabs)
    end
  end

  defp reply_wait(tabs) do
    try do
      :mnesia_lib.active_tables()
    catch
      :exit, _ ->
        {:error, {:node_not_running, node()}}
    else
      active when is_list(active) ->
        case tabs -- active do
          [] ->
            :ok

          badTabs ->
            {:timeout, badTabs}
        end
    end
  end

  def wait_for_tables_init(from, tabs) do
    :erlang.process_flag(:trap_exit, true)
    res = wait_for_init(from, tabs, :erlang.whereis(:mnesia_controller))
    send(from, {:mnesia_controller, self(), res})
    :erlang.unlink(from)
    exit(:normal)
  end

  defp wait_for_init(from, tabs, init) do
    try do
      :erlang.link(init)
    catch
      :error, _ ->
        {:error, {:node_not_running, node()}}
    else
      true when is_pid(init) ->
        cast({:sync_tabs, tabs, self()})
        rec_tabs(tabs, tabs, from, init)
    end
  end

  defp sync_reply(waiter, tab) do
    send(waiter, {:mnesia_controller, {:tab_synced, tab}})
  end

  defp rec_tabs([tab | tabs], allTabs, from, init) do
    receive do
      {:mnesia_controller, {:tab_synced, ^tab}} ->
        rec_tabs(tabs, allTabs, from, init)

      {:EXIT, ^from, _} ->
        exit(:wait_for_tables_timeout)

      {:EXIT, ^init, _} ->
        exit(:mnesia_stopped)
    end
  end

  defp rec_tabs([], _, _, init) do
    :erlang.unlink(init)
    :ok
  end

  def get_remote_cstructs() do
    get_cstructs()
  end

  def get_cstructs() do
    {:cstructs, cstructs, running} = call(:get_cstructs)
    node = node(:erlang.group_leader())
    {:cstructs, :mnesia_schema.normalize_cs(cstructs, node), running}
  end

  def update(fun) do
    call({:update, fun})
  end

  def mnesia_down(node) do
    case :erlang.whereis(:mnesia_controller) do
      :undefined ->
        :mnesia_monitor.mnesia_down(:mnesia_controller, node)

      pid ->
        :gen_server.cast(pid, {:mnesia_down, node})
    end
  end

  def wait_for_schema_commit_lock() do
    try do
      pid = :erlang.whereis(:mnesia_controller)
      :erlang.link(pid)
      :gen_server.call(pid, :wait_for_schema_commit_lock, :infinity)
    catch
      _, _ ->
        :mnesia.abort({:node_not_running, node()})
    end
  end

  def block_controller() do
    call(:block_controller)
  end

  def unblock_controller() do
    cast(:unblock_controller)
  end

  def release_schema_commit_lock() do
    cast({:release_schema_commit_lock, self()})
    :erlang.unlink(:erlang.whereis(:mnesia_controller))
  end

  def get_network_copy(tid, tab, cs) do
    case call({:add_other, self()}) do
      :ok ->
        reason = {:dumper, {:add_table_copy, tid}}
        work = r_net_load(table: tab, reason: reason, cstruct: cs)
        :erlang.process_flag(:trap_exit, true)
        load = load_table_fun(work)

        res =
          try do
            load.()
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end

        :erlang.process_flag(:trap_exit, false)
        call({:del_other, self()})

        case res do
          r_loader_done(is_loaded: true) ->
            ^tab = r_loader_done(res, :table_name)

            case r_loader_done(res, :needs_announce) do
              true ->
                i_have_tab(tab)

              false ->
                :ignore
            end

            r_loader_done(res, :reply)

          r_loader_done() ->
            r_loader_done(res, :reply)

          else__ ->
            {:not_loaded, else__}
        end

      {:error, else__} ->
        {:not_loaded, else__}
    end
  end

  def create_table(tab) do
    {:loaded, :ok} =
      :mnesia_loader.disc_load_table(
        tab,
        {:dumper, :create_table}
      )
  end

  def get_disc_copy(tab) do
    disc_load_table(tab, {:dumper, :change_table_copy_type}, :undefined)
  end

  def force_load_table(tab) when is_atom(tab) and tab != :schema do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :storage_type}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      :ram_copies ->
        do_force_load_table(tab)

      :disc_copies ->
        do_force_load_table(tab)

      :disc_only_copies ->
        do_force_load_table(tab)

      {:ext, _, _} ->
        do_force_load_table(tab)

      :unknown ->
        set({tab, :load_by_force}, true)
        cast({:force_load_updated, tab})
        wait_for_tables([tab], :infinity)

      {:EXIT, _} ->
        {:error, {:no_exists, tab}}
    end
  end

  def force_load_table(tab) do
    {:error, {:bad_type, tab}}
  end

  defp do_force_load_table(tab) do
    loaded =
      try do
        :ets.lookup_element(:mnesia_gvar, {tab, :load_reason}, 2)
      catch
        :error, _ ->
          {:EXIT, {:badarg, []}}
      end

    case loaded do
      :unknown ->
        set({tab, :load_by_force}, true)
        :mnesia_late_loader.async_late_disc_load(node(), [tab], :forced_by_user)
        wait_for_tables([tab], :infinity)

      {:EXIT, _} ->
        set({tab, :load_by_force}, true)
        :mnesia_late_loader.async_late_disc_load(node(), [tab], :forced_by_user)
        wait_for_tables([tab], :infinity)

      _ ->
        :ok
    end
  end

  def master_nodes_updated(:schema, _Masters) do
    :ignore
  end

  def master_nodes_updated(tab, masters) do
    cast({:master_nodes_updated, tab, masters})
  end

  def schedule_late_disc_load(tabs, reason) do
    msgTag = :late_disc_load
    try_schedule_late_disc_load(tabs, reason, msgTag)
  end

  defp try_schedule_late_disc_load(tabs, _Reason, msgTag)
       when tabs == [] and
              msgTag != :schema_is_merged do
    :ignore
  end

  defp try_schedule_late_disc_load(tabs, reason, msgTag) do
    getIntents = fn ->
      item = :mnesia_late_disc_load
      nodes = val({:current, :db_nodes})
      :mnesia.lock({:global, item, nodes}, :write)

      case multicall(
             nodes -- [node()],
             :disc_load_intents
           ) do
        {replies, []} ->
          call({msgTag, tabs, reason, replies})
          :done

        {_, badNodes} ->
          {:retry, badNodes}
      end
    end

    case :mnesia.transaction(getIntents) do
      {:atomic, :done} ->
        :done

      {:atomic, {:retry, badNodes}} ->
        verbose('Retry late_load_tables because bad nodes: ~p~n', [badNodes])
        try_schedule_late_disc_load(tabs, reason, msgTag)

      {:aborted, abortReason} ->
        fatal('Cannot late_load_tables ~tp: ~tp~n', [[tabs, reason, msgTag], abortReason])
    end
  end

  def connect_nodes(ns) do
    connect_nodes(ns, &default_merge/1)
  end

  def connect_nodes(ns, userFun) do
    case :mnesia.system_info(:is_running) do
      :no ->
        {:error, {:node_not_running, node()}}

      :yes ->
        pid = spawn_link(:mnesia_controller, :connect_nodes2, [self(), ns, userFun])

        receive do
          {:mnesia_controller, ^pid, res, new} ->
            case res do
              :ok ->
                :mnesia_lib.add_list(:extra_db_nodes, new)
                {:ok, new}

              {:aborted, {:throw, str}} when is_list(str) ->
                {:error, {:merge_schema_failed, :lists.flatten(str)}}

              else__ ->
                {:error, else__}
            end

          {:EXIT, ^pid, reason} ->
            {:error, reason}
        end
    end
  end

  def connect_nodes2(father, ns, userFun) do
    current = val({:current, :db_nodes})
    abcast([node() | ns], {:merging_schema, node()})
    {newC, oldC} = :mnesia_recover.connect_nodes(ns)
    connected = newC ++ oldC
    new1 = :mnesia_lib.intersect(ns, connected)
    new = new1 -- current
    :erlang.process_flag(:trap_exit, true)
    res = try_merge_schema(new, [], userFun)
    msg = {:schema_is_merged, [], :late_merge, []}
    _ = multicall([node() | ns], msg)
    after__ = val({:current, :db_nodes})
    send(father, {:mnesia_controller, self(), res, :mnesia_lib.intersect(ns, after__)})
    :erlang.unlink(father)
    :ok
  end

  def merge_schema() do
    allNodes = :mnesia_lib.all_nodes()

    case try_merge_schema(allNodes, [node()], &default_merge/1) do
      :ok ->
        schema_is_merged()

      {:aborted, {:throw, str}} when is_list(str) ->
        fatal('Failed to merge schema: ~s~n', [str])

      else__ ->
        fatal('Failed to merge schema: ~p~n', [else__])
    end
  end

  defp default_merge(f) do
    f.([])
  end

  defp try_merge_schema(nodes, told0, userFun) do
    case :mnesia_schema.merge_schema(userFun) do
      {:atomic, :not_merged} ->
        case val({:current, :db_nodes}) -- :mnesia_lib.uniq(told0) do
          [] ->
            :ok

          tell ->
            im_running(tell, [node()])
            :ok
        end

      {:atomic, {:merged, oldFriends, newFriends}} ->
        diff = :mnesia_lib.all_nodes() -- [node() | nodes]
        :mnesia_recover.connect_nodes(diff)
        im_running(oldFriends, newFriends)
        im_running(newFriends, oldFriends)

        told =
          case :lists.member(node(), newFriends) do
            true ->
              told0 ++ oldFriends

            false ->
              told0 ++ newFriends
          end

        try_merge_schema(nodes, told, userFun)

      {:atomic, {'Cannot get cstructs', node, reason}} ->
        dbg_out('Cannot get cstructs, Node ~p ~tp~n', [node, reason])
        :timer.sleep(300)
        try_merge_schema(nodes, told0, userFun)

      {:aborted, {:shutdown, _}} ->
        :timer.sleep(300)
        try_merge_schema(nodes, told0, userFun)

      other ->
        other
    end
  end

  defp im_running(oldFriends, newFriends) do
    abcast(oldFriends, {:im_running, node(), newFriends})
  end

  defp schema_is_merged() do
    msgTag = :schema_is_merged
    safeLoads = initial_safe_loads()
    try_schedule_late_disc_load(safeLoads, :initial, msgTag)
  end

  def cast(msg) do
    case :erlang.whereis(:mnesia_controller) do
      :undefined ->
        :ok

      pid ->
        :gen_server.cast(pid, msg)
    end
  end

  defp abcast(nodes, msg) do
    :gen_server.abcast(nodes, :mnesia_controller, msg)
  end

  def call(msg) do
    case :erlang.whereis(:mnesia_controller) do
      :undefined ->
        {:error, {:node_not_running, node()}}

      pid ->
        :erlang.link(pid)
        res = :gen_server.call(pid, msg, :infinity)
        :erlang.unlink(pid)

        receive do
          {:EXIT, ^pid, _Reason} ->
            {:error, {:node_not_running, node()}}
        after
          0 ->
            res
        end
    end
  end

  defp remote_call(node, func, args) do
    try do
      :gen_server.call({:mnesia_controller, node}, {func, args, self()}, :infinity)
    catch
      :exit, error ->
        {:error, error}
    end
  end

  defp multicall(nodes, msg) do
    {good, bad} = :gen_server.multi_call(nodes, :mnesia_controller, msg, :infinity)

    patchedGood =
      for {_Node, reply} <- good do
        reply
      end

    {patchedGood, bad}
  end

  defp next_async_dump_log() do
    interval = :mnesia_monitor.get_env(:dump_log_time_threshold)
    msg = {:next_async_dump_log, :time_threshold}
    ref = :erlang.send_after(interval, self(), msg)
    ref
  end

  def init([parent]) do
    :erlang.process_flag(:trap_exit, true)
    :mnesia_lib.verbose('~p starting: ~p~n', [:mnesia_controller, self()])
    all = :mnesia_lib.all_nodes()
    diff = all -- [node() | val(:original_nodes)]
    :mnesia_lib.unset(:original_nodes)
    :mnesia_recover.connect_nodes(diff)
    ref = next_async_dump_log()
    :mnesia_dumper.start_regulator()
    empty = :gb_trees.empty()

    {:ok,
     r_state(
       supervisor: parent,
       dump_log_timer_ref: ref,
       loader_queue: empty,
       late_loader_queue: empty
     )}
  end

  def handle_call({:sync_dump_log, initBy}, from, state) do
    worker = r_dump_log(initiated_by: initBy, opt_reply_to: from)
    state2 = add_worker(worker, state)
    noreply(state2)
  end

  def handle_call({:snapshot_dcd, tables}, from, state) do
    worker =
      r_dump_log(
        initiated_by: :user,
        opt_reply_to: from,
        operation: fn ->
          :mnesia_dumper.snapshot_dcd(tables)
        end
      )

    state2 = add_worker(worker, state)
    noreply(state2)
  end

  def handle_call(:wait_for_schema_commit_lock, from, state) do
    worker = r_schema_commit_lock(owner: from)
    state2 = add_worker(worker, state)
    noreply(state2)
  end

  def handle_call(:block_controller, from, state) do
    worker = r_block_controller(owner: from)
    state2 = add_worker(worker, state)
    noreply(state2)
  end

  def handle_call({:update, fun}, from, state) do
    res =
      try do
        fun.()
      catch
        _, _Reason ->
          {:EXIT, _Reason}
      end

    reply(from, res)
    noreply(state)
  end

  def handle_call(:get_cstructs, from, state) do
    tabs = val({:schema, :tables})

    cstructs =
      for t <- tabs do
        val({t, :cstruct})
      end

    running = val({:current, :db_nodes})
    reply(from, {:cstructs, cstructs, running})
    noreply(state)
  end

  def handle_call(
        {:schema_is_merged, [], :late_merge, []},
        from,
        state = r_state(schema_is_merged: merged)
      ) do
    case merged do
      {false, node} when node == node(from) ->
        msgs = r_state(state, :early_msgs)

        state1 =
          r_state(state,
            early_msgs: [],
            schema_is_merged: true
          )

        handle_early_msgs(:lists.reverse(msgs), state1)

      _ ->
        reply(from, :ignore)
        noreply(state)
    end
  end

  def handle_call({:schema_is_merged, tabsR, reason, remoteLoaders}, from, state) do
    state2 = late_disc_load(tabsR, reason, remoteLoaders, from, state)
    msgs = r_state(state2, :early_msgs)

    state3 =
      r_state(state2,
        early_msgs: [],
        schema_is_merged: true
      )

    handle_early_msgs(:lists.reverse(msgs), state3)
  end

  def handle_call(
        :disc_load_intents,
        from,
        state = r_state(loader_queue: lQ, late_loader_queue: lLQ)
      ) do
    lQTabs = :gb_trees.keys(lQ)
    lLQTabs = :gb_trees.keys(lLQ)
    activeTabs = :lists.sort(:mnesia_lib.local_active_tables())

    reply(
      from,
      {:ok, node(), :ordsets.union([lQTabs, lLQTabs, activeTabs])}
    )

    noreply(state)
  end

  def handle_call({:update_where_to_write, [:add, tab, addNode], _From}, _Dummy, state) do
    current = val({:current, :db_nodes})

    res =
      case :erlang.and(
             :lists.member(
               addNode,
               current
             ),
             r_state(state, :schema_is_merged) == true
           ) do
        true ->
          :mnesia_lib.add_lsort({tab, :where_to_write}, addNode)
          update_where_to_wlock(tab)

        false ->
          :ignore
      end

    {:reply, res, state}
  end

  def handle_call({:add_active_replica, [tab, toNode, remoteS, accessMode], from}, replyTo, state) do
    knownNode =
      :lists.member(
        toNode,
        val({:current, :db_nodes})
      )

    merged = r_state(state, :schema_is_merged)

    cond do
      knownNode == false ->
        reply(replyTo, :ignore)
        noreply(state)

      merged == true ->
        res =
          case (try do
                  :ets.lookup_element(:mnesia_gvar, {tab, :cstruct}, 2)
                catch
                  :error, _ ->
                    {:EXIT, {:badarg, []}}
                end) do
            {:EXIT, _} ->
              :deleted

            _ ->
              add_active_replica(tab, toNode, remoteS, accessMode)
          end

        reply(replyTo, res)
        noreply(state)

      true ->
        msg = {:add_active_replica, [tab, toNode, remoteS, accessMode], from}
        msgs = r_state(state, :early_msgs)
        reply(replyTo, :ignore)

        noreply(
          r_state(state,
            early_msgs: [
              {:call, msg, :undefined}
              | msgs
            ]
          )
        )
    end
  end

  def handle_call({:unannounce_add_table_copy, [tab, node], from}, replyTo, state) do
    knownNode =
      :lists.member(
        node(from),
        val({:current, :db_nodes})
      )

    merged = r_state(state, :schema_is_merged)

    cond do
      knownNode == false ->
        reply(replyTo, :ignore)
        noreply(state)

      merged == true ->
        res = unannounce_add_table_copy(tab, node)
        reply(replyTo, res)
        noreply(state)

      true ->
        msg = {:unannounce_add_table_copy, [tab, node], from}
        msgs = r_state(state, :early_msgs)
        reply(replyTo, :ignore)

        noreply(
          r_state(state,
            early_msgs: [
              {:call, msg, :undefined}
              | msgs
            ]
          )
        )
    end
  end

  def handle_call(
        {:add_other, who},
        _From,
        state = r_state(others: others0, schema_is_merged: sM)
      ) do
    case sM do
      true ->
        others = [who | others0]
        {:reply, :ok, r_state(state, others: others)}

      false ->
        {:reply, {:error, {:not_active, :schema, node()}}, state}
    end
  end

  def handle_call({:del_other, who}, _From, state = r_state(others: others0)) do
    others = :lists.delete(who, others0)
    {:reply, :ok, r_state(state, others: others)}
  end

  def handle_call(msg, from, state)
      when r_state(state, :schema_is_merged) != true do
    msgs = r_state(state, :early_msgs)

    noreply(
      r_state(state,
        early_msgs: [
          {:call, msg, from}
          | msgs
        ]
      )
    )
  end

  def handle_call({:late_disc_load, tabs, reason, remoteLoaders}, from, state) do
    state2 = late_disc_load(tabs, reason, remoteLoaders, from, state)
    noreply(state2)
  end

  def handle_call({:unblock_table, tab}, _Dummy, state) do
    var = {tab, :where_to_commit}

    case val(var) do
      {:blocked, list} ->
        set(var, list)

      _ ->
        :ignore
    end

    {:reply, :ok, state}
  end

  def handle_call({:block_table, [tab], from}, _Dummy, state) do
    case :lists.member(
           node(from),
           val({:current, :db_nodes})
         ) do
      true ->
        block_table(tab)

      false ->
        :ignore
    end

    {:reply, :ok, state}
  end

  def handle_call({:check_w2r, _Node, tab}, _From, state) do
    {:reply, val({tab, :where_to_read}), state}
  end

  def handle_call(msg, _From, state) do
    :erlang.error('~p got unexpected call: ~tp~n', [:mnesia_controller, msg])
    noreply(state)
  end

  defp late_disc_load(
         tabsR,
         reason,
         remoteLoaders,
         from,
         state = r_state(loader_queue: lQ, late_loader_queue: lLQ)
       ) do
    verbose('Intend to load tables: ~tp~n', [tabsR])
    :ok
    reply(from, :queued)
    localTabs = :gb_sets.from_ordset(:lists.sort(:mnesia_lib.val({:schema, :local_tables})))

    filter = fn tabInfo0, acc ->
      tabInfo =
        {tab, _} =
        case tabInfo0 do
          {_, _} ->
            tabInfo0

          tabN ->
            {tabN, reason}
        end

      case :gb_sets.is_member(tab, localTabs) do
        true ->
          case (try do
                  :ets.lookup_element(:mnesia_gvar, {tab, :where_to_read}, 2)
                catch
                  :error, _ ->
                    {:EXIT, {:badarg, []}}
                end) == node() do
            true ->
              acc

            false ->
              case :gb_trees.is_defined(tab, lQ) do
                true ->
                  acc

                false ->
                  [tabInfo | acc]
              end
          end

        false ->
          acc
      end
    end

    tabs = :lists.foldl(filter, [], tabsR)
    nodes = val({:current, :db_nodes})
    lateQueue = late_loaders(tabs, remoteLoaders, nodes, lLQ)
    r_state(state, late_loader_queue: lateQueue)
  end

  defp late_loaders([{tab, reason} | tabs], remoteLoaders, nodes, lLQ) do
    case :gb_trees.is_defined(tab, lLQ) do
      false ->
        loadNodes = late_load_filter(remoteLoaders, tab, nodes, [])

        case loadNodes do
          [] ->
            cast({:disc_load, tab, reason})

          _ ->
            :ignore
        end

        lateLoad = r_late_load(table: tab, loaders: loadNodes, reason: reason)
        late_loaders(tabs, remoteLoaders, nodes, :gb_trees.insert(tab, lateLoad, lLQ))

      true ->
        late_loaders(tabs, remoteLoaders, nodes, lLQ)
    end
  end

  defp late_loaders([], _RemoteLoaders, _Nodes, lLQ) do
    lLQ
  end

  defp late_load_filter([{:error, _} | remoteLoaders], tab, nodes, acc) do
    late_load_filter(remoteLoaders, tab, nodes, acc)
  end

  defp late_load_filter([{:badrpc, _} | remoteLoaders], tab, nodes, acc) do
    late_load_filter(remoteLoaders, tab, nodes, acc)
  end

  defp late_load_filter([rL | remoteLoaders], tab, nodes, acc) do
    {:ok, node, intents} = rL
    access = val({tab, :access_mode})
    localC = val({tab, :local_content})
    stillActive = :lists.member(node, nodes)
    remoteIntent = :lists.member(tab, intents)

    cond do
      access == :read_write and localC == false and
        stillActive == true and remoteIntent == true ->
        masters = :mnesia_recover.get_master_nodes(tab)

        case :lists.member(node, masters) do
          true ->
            late_load_filter(remoteLoaders, tab, nodes, [node | acc])

          false when masters == [] ->
            late_load_filter(remoteLoaders, tab, nodes, [node | acc])

          false ->
            late_load_filter(remoteLoaders, tab, nodes, acc)
        end

      true ->
        late_load_filter(remoteLoaders, tab, nodes, acc)
    end
  end

  defp late_load_filter([], _Tab, _Nodes, acc) do
    acc
  end

  def handle_cast({:release_schema_commit_lock, _Owner}, state) do
    cond do
      r_state(state, :is_stopping) == true ->
        {:stop, :shutdown, state}

      true ->
        case r_state(state, :dumper_queue) do
          [r_schema_commit_lock() | rest] ->
            [_Worker | ^rest] = r_state(state, :dumper_queue)

            state2 =
              r_state(state,
                dumper_pid: :undefined,
                dumper_queue: rest
              )

            state3 = opt_start_worker(state2)
            noreply(state3)

          _ ->
            noreply(state)
        end
    end
  end

  def handle_cast(:unblock_controller, state) do
    cond do
      r_state(state, :is_stopping) == true ->
        {:stop, :shutdown, state}

      elem(hd(r_state(state, :dumper_queue)), 0) === :block_controller ->
        [_Worker | rest] = r_state(state, :dumper_queue)

        state2 =
          r_state(state,
            dumper_pid: :undefined,
            dumper_queue: rest
          )

        state3 = opt_start_worker(state2)
        noreply(state3)
    end
  end

  def handle_cast({:mnesia_down, node}, state) do
    maybe_log_mnesia_down(node)
    :mnesia_lib.del({:current, :db_nodes}, node)
    :mnesia_lib.unset({:node_up, node})
    :mnesia_checkpoint.tm_mnesia_down(node)
    alltabs = val({:schema, :tables})
    reconfigure_tables(node, alltabs)
    :mnesia_monitor.mnesia_down(:mnesia_controller, node)

    case r_state(state, :schema_is_merged) do
      {false, ^node} ->
        spawn(:mnesia_controller, :call, [{:schema_is_merged, [], :late_merge, []}])

      _ ->
        :ignore
    end

    lateQ = remove_loaders(alltabs, node, r_state(state, :late_loader_queue))

    case get_senders(state) ++ get_loaders(state) do
      [] ->
        :ignore

      senders ->
        :lists.foreach(
          fn {pid, _} ->
            send(pid, {:copier_done, node})
          end,
          senders
        )
    end

    :lists.foreach(
      fn pid ->
        send(pid, {:copier_done, node})
      end,
      r_state(state, :others)
    )

    remove = fn sT ->
      node(r_send_table(sT, :receiver_pid)) != node
    end

    newSenders =
      :lists.filter(
        remove,
        r_state(state, :sender_queue)
      )

    early =
      remove_early_messages(
        r_state(state, :early_msgs),
        node
      )

    noreply(r_state(state, sender_queue: newSenders, early_msgs: early, late_loader_queue: lateQ))
  end

  def handle_cast({:merging_schema, node}, state) do
    case r_state(state, :schema_is_merged) do
      false ->
        imANewKidInTheBlock =
          val({:schema, :storage_type}) == :ram_copies and
            :mnesia_lib.val({:schema, :local_tables}) == [:schema]

        case imANewKidInTheBlock do
          true ->
            noreply(r_state(state, schema_is_merged: {false, node}))

          false ->
            noreply(state)
        end

      _ ->
        noreply(state)
    end
  end

  def handle_cast(msg, state)
      when r_state(state, :schema_is_merged) != true do
    msgs = r_state(state, :early_msgs)
    noreply(r_state(state, early_msgs: [{:cast, msg} | msgs]))
  end

  def handle_cast({:im_running, node, newFriends}, state) do
    localTabs = :mnesia_lib.local_active_tables() -- [:schema]

    removeLocalOnly = fn tab ->
      not val({tab, :local_content})
    end

    tabs = :lists.filter(removeLocalOnly, localTabs)

    nodes =
      :mnesia_lib.union(
        [node],
        val({:current, :db_nodes})
      )

    ns = :mnesia_lib.intersect(newFriends, nodes)
    abcast(ns, {:adopt_orphans, node(), tabs})
    noreply(state)
  end

  def handle_cast({:disc_load, tab, reason}, state) do
    worker = r_disc_load(table: tab, reason: reason)
    state2 = add_worker(worker, state)
    noreply(state2)
  end

  def handle_cast(worker = r_send_table(), state) do
    state2 = add_worker(worker, state)
    noreply(state2)
  end

  def handle_cast({:sync_tabs, tabs, from}, state) do
    handle_sync_tabs(tabs, from)
    noreply(state)
  end

  def handle_cast({:i_have_tab, tab, node}, state) do
    case :lists.member(
           node,
           val({:current, :db_nodes})
         ) do
      true ->
        state2 = node_has_tabs([tab], node, state)
        noreply(state2)

      false ->
        noreply(state)
    end
  end

  def handle_cast({:force_load_updated, tab}, state) do
    case val({tab, :active_replicas}) do
      [] ->
        noreply(state)

      [someNode | _] ->
        state2 = node_has_tabs([tab], someNode, state)
        noreply(state2)
    end
  end

  def handle_cast({:master_nodes_updated, tab, masters}, state) do
    active = val({tab, :active_replicas})

    valid =
      case val({tab, :load_by_force}) do
        true ->
          active

        false ->
          cond do
            masters == [] ->
              active

            true ->
              :mnesia_lib.intersect(masters, active)
          end
      end

    case valid do
      [] ->
        noreply(state)

      [someNode | _] ->
        state2 = node_has_tabs([tab], someNode, state)
        noreply(state2)
    end
  end

  def handle_cast({:adopt_orphans, node, tabs}, state) do
    state2 = node_has_tabs(tabs, node, state)

    case (try do
            :ets.lookup_element(:mnesia_gvar, {:node_up, node}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      true ->
        :ignore

      _ ->
        set({:node_up, node}, true)
        :mnesia_recover.log_mnesia_up(node)
        verbose('Logging mnesia_up ~w~n', [node])
        :mnesia_lib.report_system_event({:mnesia_up, node})
        localTabs = val({:schema, :local_tables}) -- [:schema]
        nodes = val({:current, :db_nodes})
        {localOrphans, remoteMasters} = orphan_tables(localTabs, node, nodes, [], [])
        reason = {:adopt_orphan, node()}
        :mnesia_late_loader.async_late_disc_load(node(), localOrphans, reason)

        fun = fn n ->
          remoteOrphans =
            for {tab, ns} <- remoteMasters,
                :lists.member(n, ns) do
              tab
            end

          :mnesia_late_loader.maybe_async_late_disc_load(
            n,
            remoteOrphans,
            reason
          )
        end

        :lists.foreach(fun, nodes)
    end

    noreply(state2)
  end

  def handle_cast(msg, state) do
    :erlang.error('~p got unexpected cast: ~tp~n', [:mnesia_controller, msg])
    noreply(state)
  end

  defp handle_sync_tabs([tab | tabs], from) do
    case val({tab, :where_to_read}) do
      :nowhere ->
        case :erlang.get({:sync_tab, tab}) do
          :undefined ->
            :erlang.put({:sync_tab, tab}, [from])

          pids ->
            :erlang.put({:sync_tab, tab}, [from | pids])
        end

      _ ->
        sync_reply(from, tab)
    end

    handle_sync_tabs(tabs, from)
  end

  defp handle_sync_tabs([], _From) do
    :ok
  end

  def handle_info({:next_async_dump_log, initBy}, state) do
    async_dump_log(initBy)
    ref = next_async_dump_log()
    noreply(r_state(state, dump_log_timer_ref: ref))
  end

  def handle_info({:async_dump_log, initBy}, state) do
    worker = r_dump_log(initiated_by: initBy)
    state2 = add_worker(worker, state)
    noreply(state2)
  end

  def handle_info(r_dumper_done(worker_pid: pid, worker_res: res), state) do
    cond do
      r_state(state, :is_stopping) == true ->
        {:stop, :shutdown, state}

      res == :dumped and pid == r_state(state, :dumper_pid) ->
        [worker | rest] = r_state(state, :dumper_queue)
        reply(r_dump_log(worker, :opt_reply_to), res)

        state2 =
          r_state(state,
            dumper_pid: :undefined,
            dumper_queue: rest
          )

        state3 = opt_start_worker(state2)
        noreply(state3)

      true ->
        fatal('Dumper failed: ~p~n state: ~tp~n', [res, state])
        {:stop, :fatal, state}
    end
  end

  def handle_info(
        done = r_loader_done(worker_pid: wPid, table_name: tab),
        state0
      ) do
    lateQueue0 = r_state(state0, :late_loader_queue)
    state1 = r_state(state0, loader_pid: :lists.keydelete(wPid, 1, get_loaders(state0)))

    state2 =
      case r_loader_done(done, :is_loaded) do
        true ->
          cond do
            r_loader_done(done, :needs_announce) == true and
                r_loader_done(done, :needs_reply) == true ->
              i_have_tab(tab)
              reply(r_loader_done(done, :reply_to), r_loader_done(done, :reply))

            r_loader_done(done, :needs_reply) == true ->
              reply(r_loader_done(done, :reply_to), r_loader_done(done, :reply))

            r_loader_done(done, :needs_announce) == true and tab == :schema ->
              i_have_tab(tab)

            r_loader_done(done, :needs_announce) == true ->
              i_have_tab(tab)
              ns = val({:current, :db_nodes})
              abcast(ns, {:i_have_tab, tab, node()})

            tab == :schema ->
              :ignore

            true ->
              ns = val({:current, :db_nodes})
              alreadyKnows = val({tab, :active_replicas})
              abcast(ns -- alreadyKnows, {:i_have_tab, tab, node()})
          end

          case r_loader_done(done, :needs_sync) do
            true ->
              user_sync_tab(tab)

            false ->
              :ignore
          end

          r_state(state1,
            late_loader_queue:
              :gb_trees.delete_any(
                tab,
                lateQueue0
              )
          )

        false ->
          case r_loader_done(done, :needs_reply) do
            true ->
              reply(r_loader_done(done, :reply_to), r_loader_done(done, :reply))

            false ->
              :ignore
          end

          case (try do
                  :ets.lookup_element(:mnesia_gvar, {tab, :active_replicas}, 2)
                catch
                  :error, _ ->
                    {:EXIT, {:badarg, []}}
                end) do
            [_ | _] ->
              {:value, {_, worker}} = :lists.keysearch(wPid, 1, get_loaders(state0))
              add_loader(tab, worker, state1)

            _ ->
              delState =
                r_state(state1,
                  late_loader_queue:
                    :gb_trees.delete_any(
                      tab,
                      lateQueue0
                    )
                )

              case (try do
                      :ets.lookup_element(:mnesia_gvar, {tab, :storage_type}, 2)
                    catch
                      :error, _ ->
                        {:EXIT, {:badarg, []}}
                    end) do
                :ram_copies ->
                  cast({:disc_load, tab, :ram_only})
                  delState

                _ ->
                  delState
              end
          end
      end

    state3 = opt_start_worker(state2)
    noreply(state3)
  end

  def handle_info(r_sender_done(worker_pid: pid, worker_res: res), state) do
    senders = get_senders(state)
    {:value, {^pid, _Worker}} = :lists.keysearch(pid, 1, senders)

    cond do
      res == :ok ->
        state2 = r_state(state, sender_pid: :lists.keydelete(pid, 1, senders))
        state3 = opt_start_worker(state2)
        noreply(state3)

      true ->
        fatal('Sender failed: ~p~n state: ~tp~n', [res, state])
        {:stop, :fatal, state}
    end
  end

  def handle_info({:EXIT, pid, r}, state)
      when pid == r_state(state, :supervisor) do
    try do
      set(:mnesia_status, :stopping)
    catch
      :error, _ ->
        :ok
    end

    case r_state(state, :dumper_pid) do
      :undefined ->
        dbg_out('~p was ~tp~n', [:mnesia_controller, r])
        {:stop, :shutdown, state}

      _ ->
        noreply(r_state(state, is_stopping: true))
    end
  end

  def handle_info({:EXIT, pid, r}, state)
      when pid == r_state(state, :dumper_pid) do
    case r_state(state, :dumper_queue) do
      [r_schema_commit_lock() | workers] ->
        dbg_out('WARNING: Dumper ~p exited ~tp~n', [pid, r])

        state2 =
          r_state(state,
            dumper_queue: workers,
            dumper_pid: :undefined
          )

        state3 = opt_start_worker(state2)
        noreply(state3)

      _Other ->
        fatal('Dumper or schema commit crashed: ~p~n state: ~tp~n', [r, state])
        {:stop, :fatal, state}
    end
  end

  def handle_info(msg = {:EXIT, pid, r}, state)
      when r != :wait_for_tables_timeout do
    case :lists.keymember(pid, 1, get_senders(state)) do
      true ->
        fatal('Sender crashed: ~p~n state: ~tp~n', [{pid, r}, state])
        {:stop, :fatal, state}

      false ->
        case :lists.keymember(pid, 1, get_loaders(state)) do
          true ->
            fatal('Loader crashed: ~p~n state: ~tp~n', [r, state])
            {:stop, :fatal, state}

          false ->
            :erlang.error('~p got unexpected info: ~tp~n', [:mnesia_controller, msg])
            noreply(state)
        end
    end
  end

  def handle_info({from, :get_state}, state) do
    send(from, {:mnesia_controller, state})
    noreply(state)
  end

  def handle_info(msg, state)
      when r_state(state, :schema_is_merged) != true do
    msgs = r_state(state, :early_msgs)
    noreply(r_state(state, early_msgs: [{:info, msg} | msgs]))
  end

  def handle_info({:EXIT, pid, :wait_for_tables_timeout}, state) do
    sync_tab_timeout(pid, :erlang.get())
    noreply(state)
  end

  def handle_info(msg, state) do
    :erlang.error('~p got unexpected info: ~tp~n', [:mnesia_controller, msg])
    noreply(state)
  end

  defp sync_tab_timeout(pid, [{{:sync_tab, tab}, pids} | tail]) do
    case :lists.delete(pid, pids) do
      [] ->
        :erlang.erase({:sync_tab, tab})

      pids2 ->
        :erlang.put({:sync_tab, tab}, pids2)
    end

    sync_tab_timeout(pid, tail)
  end

  defp sync_tab_timeout(pid, [_ | tail]) do
    sync_tab_timeout(pid, tail)
  end

  defp sync_tab_timeout(_Pid, []) do
    :ok
  end

  defp pick_next(queue) do
    list = :gb_trees.values(queue)

    case pick_next(list, :none, :none) do
      :none ->
        {:none, :gb_trees.empty()}

      {tab, worker} ->
        {worker, :gb_trees.delete(tab, queue)}
    end
  end

  defp pick_next([head = r_net_load(table: tab) | tail], load, order) do
    select_best(
      head,
      tail,
      try do
        :ets.lookup_element(:mnesia_gvar, {tab, :load_order}, 2)
      catch
        :error, _ ->
          {:EXIT, {:badarg, []}}
      end,
      load,
      order
    )
  end

  defp pick_next([head = r_disc_load(table: tab) | tail], load, order) do
    select_best(
      head,
      tail,
      try do
        :ets.lookup_element(:mnesia_gvar, {tab, :load_order}, 2)
      catch
        :error, _ ->
          {:EXIT, {:badarg, []}}
      end,
      load,
      order
    )
  end

  defp pick_next([], :none, _Order) do
    :none
  end

  defp pick_next([], load, _Order) do
    {:erlang.element(2, load), load}
  end

  defp select_best(_Head, tail, {:EXIT, _WHAT}, load, order) do
    pick_next(tail, load, order)
  end

  defp select_best(load, tail, order, :none, :none) do
    pick_next(tail, load, order)
  end

  defp select_best(load, tail, order, _OldLoad, oldOrder)
       when order > oldOrder do
    pick_next(tail, load, order)
  end

  defp select_best(_Load, tail, _Order, oldLoad, oldOrder) do
    pick_next(tail, oldLoad, oldOrder)
  end

  def terminate(reason, state) do
    :mnesia_monitor.terminate_proc(:mnesia_controller, reason, state)
  end

  def code_change(_OldVsn, state0, _Extra) do
    state1 =
      case r_state(state0, :loader_pid) do
        pids when is_list(pids) ->
          state0

        :undefined ->
          r_state(state0,
            loader_pid: [],
            loader_queue: :gb_trees.empty()
          )

        pid when is_pid(pid) ->
          [loader | rest] = r_state(state0, :loader_queue)

          lQ0 =
            for rec <- rest do
              {:erlang.element(2, rec), rec}
            end

          lQ1 = :lists.sort(lQ0)
          lQ = :gb_trees.from_orddict(lQ1)

          r_state(state0,
            loader_pid: [{pid, loader}],
            loader_queue: lQ
          )
      end

    state =
      cond do
        is_list(r_state(state1, :late_loader_queue)) ->
          lLQ0 = r_state(state1, :late_loader_queue)

          lLQ1 =
            :lists.sort(
              for rec <- lLQ0 do
                {:erlang.element(2, rec), rec}
              end
            )

          lLQ = :gb_trees.from_orddict(lLQ1)
          r_state(state1, late_loader_queue: lLQ)

        true ->
          state1
      end

    {:ok, state}
  end

  defp maybe_log_mnesia_down(n) do
    case :mnesia_lib.is_running() do
      :yes ->
        verbose('Logging mnesia_down ~w~n', [n])
        :mnesia_recover.log_mnesia_down(n)
        :ok

      _ ->
        filter = fn tab ->
          inactive_copy_holders(tab, n)
        end

        halfLoadedTabs =
          :lists.any(
            filter,
            val({:schema, :local_tables}) -- [:schema]
          )

        cond do
          halfLoadedTabs == true ->
            verbose('Logging mnesia_down ~w~n', [n])
            :mnesia_recover.log_mnesia_down(n)
            :ok

          true ->
            :log_later
        end
    end
  end

  defp inactive_copy_holders(tab, node) do
    cs = val({tab, :cstruct})

    case :mnesia_lib.cs_to_storage_type(node, cs) do
      :unknown ->
        false

      _Storage ->
        :mnesia_lib.not_active_here(tab)
    end
  end

  defp orphan_tables([tab | tabs], node, ns, local, remote) do
    cs = val({tab, :cstruct})
    copyHolders = :mnesia_lib.copy_holders(cs)
    ramCopyHolders = r_cstruct(cs, :ram_copies)
    discCopyHolders = copyHolders -- ramCopyHolders
    discNodes = val({:schema, :disc_copies})
    localContent = r_cstruct(cs, :local_content)

    ramCopyHoldersOnDiscNodes =
      :mnesia_lib.intersect(
        ramCopyHolders,
        discNodes
      )

    active = val({tab, :active_replicas})

    beingCreated =
      try do
        :ets.lookup_element(:mnesia_gvar, {tab, :create_table}, 2)
      catch
        :error, _ ->
          {:EXIT, {:badarg, []}}
      end == true

    read = val({tab, :where_to_read})

    case :lists.member(node, discCopyHolders) do
      _ when beingCreated == true ->
        orphan_tables(tabs, node, ns, local, remote)

      _ when read == node() ->
        orphan_tables(tabs, node, ns, local, remote)

      true when active == [] ->
        case discCopyHolders -- ns do
          [] ->
            case :lists.min(discCopyHolders) do
              min when min == node() ->
                case :mnesia_recover.get_master_nodes(tab) do
                  [] ->
                    l = [tab | local]
                    orphan_tables(tabs, node, ns, l, remote)

                  masters ->
                    r = [{tab, masters -- ramCopyHolders} | remote]
                    orphan_tables(tabs, node, ns, local, r)
                end

              _ ->
                orphan_tables(tabs, node, ns, local, remote)
            end

          _ ->
            orphan_tables(tabs, node, ns, local, remote)
        end

      false
      when active == [] and discCopyHolders == [] and
             ramCopyHoldersOnDiscNodes == [] ->
        orphan_tables(tabs, node, ns, [tab | local], remote)

      _ when localContent == true ->
        orphan_tables(tabs, node, ns, [tab | local], remote)

      _ ->
        orphan_tables(tabs, node, ns, local, remote)
    end
  end

  defp orphan_tables([], _, _, localOrphans, remoteMasters) do
    {localOrphans, remoteMasters}
  end

  defp node_has_tabs([tab | tabs], node, state)
       when node != node() do
    state2 =
      try do
        update_whereabouts(tab, node, state)
      catch
        :exit, r ->
          case (try do
                  :ets.lookup_element(:mnesia_gvar, {tab, :cstruct}, 2)
                catch
                  :error, _ ->
                    {:EXIT, {:badarg, []}}
                end) do
            {:EXIT, _} ->
              state

            _ ->
              :erlang.error(r)
          end
      else
        state1 = r_state() ->
          state1
      end

    node_has_tabs(tabs, node, state2)
  end

  defp node_has_tabs([tab | tabs], node, state) do
    user_sync_tab(tab)
    node_has_tabs(tabs, node, state)
  end

  defp node_has_tabs([], _Node, state) do
    state
  end

  defp update_whereabouts(tab, node, state) do
    storage = val({tab, :storage_type})
    read = val({tab, :where_to_read})
    localC = val({tab, :local_content})

    beingCreated =
      try do
        :ets.lookup_element(:mnesia_gvar, {tab, :create_table}, 2)
      catch
        :error, _ ->
          {:EXIT, {:badarg, []}}
      end == true

    masters = :mnesia_recover.get_master_nodes(tab)
    byForce = val({tab, :load_by_force})

    goGetIt =
      cond do
        byForce == true ->
          true

        masters == [] ->
          true

        true ->
          :lists.member(node, masters)
      end

    dbg_out(
      'Table ~w is loaded on ~w. s=~w, r=~w, lc=~w, f=~w, m=~w~n',
      [tab, node, storage, read, localC, byForce, goGetIt]
    )

    cond do
      localC == true ->
        state

      beingCreated == true ->
        state

      storage == :unknown and read == :nowhere ->
        add_active_replica(tab, node)

        case goGetIt do
          true ->
            set({tab, :where_to_read}, node)
            user_sync_tab(tab)
            state

          false ->
            state
        end

      storage == :unknown ->
        add_active_replica(tab, node)

        nodeST =
          :mnesia_lib.semantics(
            :mnesia_lib.storage_type_at_node(
              node,
              tab
            ),
            :storage
          )

        readST =
          :mnesia_lib.semantics(
            :mnesia_lib.storage_type_at_node(
              read,
              tab
            ),
            :storage
          )

        cond do
          nodeST == :disc_only_copies ->
            :ignore

          readST == :disc_only_copies ->
            :mnesia_lib.set_remote_where_to_read(tab)

          true ->
            :ignore
        end

        user_sync_tab(tab)
        state

      read == :nowhere ->
        add_active_replica(tab, node)

        case goGetIt do
          true ->
            worker = r_net_load(table: tab, reason: {:active_remote, node})
            add_worker(worker, state)

          false ->
            state
        end

      true ->
        add_active_replica(tab, node)
        user_sync_tab(tab)
        state
    end
  end

  defp initial_safe_loads() do
    case val({:schema, :storage_type}) do
      :ram_copies ->
        downs = []
        tabs = val({:schema, :local_tables}) -- [:schema]

        lastC = fn t ->
          last_consistent_replica(t, downs)
        end

        :lists.zf(lastC, tabs)

      :disc_copies ->
        downs = :mnesia_recover.get_mnesia_downs()
        dbg_out('mnesia_downs = ~p~n', [downs])
        tabs = val({:schema, :local_tables}) -- [:schema]

        lastC = fn t ->
          last_consistent_replica(t, downs)
        end

        :lists.zf(lastC, tabs)
    end
  end

  defp last_consistent_replica(tab, downs) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :cstruct}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      r_cstruct() = cs ->
        last_consistent_replica(cs, tab, downs)

      _ ->
        false
    end
  end

  defp last_consistent_replica(cs, tab, downs) do
    storage = :mnesia_lib.cs_to_storage_type(node(), cs)
    ram = r_cstruct(cs, :ram_copies)
    disc = r_cstruct(cs, :disc_copies)
    discOnly = r_cstruct(cs, :disc_only_copies)
    ext = r_cstruct(cs, :external_copies)
    betterCopies0 = :mnesia_lib.remote_copy_holders(cs) -- downs
    betterCopies = betterCopies0 -- ram
    accessMode = r_cstruct(cs, :access_mode)
    copies = :mnesia_lib.copy_holders(cs)
    masters = :mnesia_recover.get_master_nodes(tab)
    localMaster0 = :lists.member(node(), masters)
    localContent = r_cstruct(cs, :local_content)

    remoteMaster =
      cond do
        masters == [] ->
          false

        true ->
          not localMaster0
      end

    localMaster =
      cond do
        masters == [] ->
          false

        true ->
          localMaster0
      end

    cond do
      copies == [node()] ->
        {true, {tab, :local_only}}

      localContent == true ->
        {true, {tab, :local_content}}

      localMaster == true ->
        {true, {tab, :local_master}}

      remoteMaster == true ->
        false

      storage == :ram_copies ->
        cond do
          disc == [] and discOnly == [] and ext == [] ->
            {true, {tab, :ram_only}}

          true ->
            false
        end

      accessMode == :read_only ->
        {true, {tab, :read_only}}

      betterCopies != [] and masters != [node()] ->
        false

      true ->
        {true, {tab, :initial}}
    end
  end

  defp reconfigure_tables(n, [tab | tail]) do
    del_active_replica(tab, n)

    case val({tab, :where_to_read}) do
      ^n ->
        :mnesia_lib.set_remote_where_to_read(tab)

      _ ->
        :ignore
    end

    reconfigure_tables(n, tail)
  end

  defp reconfigure_tables(_, []) do
    :ok
  end

  defp remove_loaders([tab | tabs], n, loaders) do
    lateQ = drop_loaders(tab, n, loaders)
    remove_loaders(tabs, n, lateQ)
  end

  defp remove_loaders([], _, lateQ) do
    lateQ
  end

  defp remove_early_messages([], _Node) do
    []
  end

  defp remove_early_messages(
         [
           {:call, {:add_active_replica, [_, node, _, _], _}, _}
           | r
         ],
         node
       ) do
    remove_early_messages(r, node)
  end

  defp remove_early_messages(
         [{:call, {:block_table, _, from}, replyTo} | r],
         node
       )
       when node(from) == node do
    reply(replyTo, :ok)
    remove_early_messages(r, node)
  end

  defp remove_early_messages(
         [{:cast, {:i_have_tab, _Tab, node}} | r],
         node
       ) do
    remove_early_messages(r, node)
  end

  defp remove_early_messages(
         [{:cast, {:adopt_orphans, node, _Tabs}} | r],
         node
       ) do
    remove_early_messages(r, node)
  end

  defp remove_early_messages([m | r], node) do
    [m | remove_early_messages(r, node)]
  end

  defp drop_loaders(tab, node, lLQ) do
    case :gb_trees.lookup(tab, lLQ) do
      :none ->
        lLQ

      {:value, h} ->
        case r_late_load(h, :loaders) do
          [^node] ->
            reason = {r_late_load(h, :reason), :last_loader_down, node}
            cast({:disc_load, tab, reason})

          _ ->
            :ignore
        end

        h2 = r_late_load(h, loaders: r_late_load(h, :loaders) -- [node])
        :gb_trees.update(tab, h2, lLQ)
    end
  end

  def add_active_replica(tab, node) do
    add_active_replica(tab, node, val({tab, :cstruct}))
  end

  def add_active_replica(tab, node, cs = r_cstruct()) do
    storage =
      :mnesia_lib.schema_cs_to_storage_type(
        node,
        cs
      )

    accessMode = r_cstruct(cs, :access_mode)
    add_active_replica(tab, node, storage, accessMode)
  end

  def block_table(tab) do
    var = {tab, :where_to_commit}

    case is_tab_blocked(val(var)) do
      {true, _} ->
        :ok

      {false, w2C} ->
        set(var, mark_blocked_tab(true, w2C))
    end
  end

  def unblock_table(tab) do
    call({:unblock_table, tab})
  end

  defp is_tab_blocked(w2C) when is_list(w2C) do
    {false, w2C}
  end

  defp is_tab_blocked({:blocked, w2C}) when is_list(w2C) do
    {true, w2C}
  end

  defp mark_blocked_tab(true, value) do
    {:blocked, value}
  end

  defp mark_blocked_tab(false, value) do
    value
  end

  def add_active_replica(tab, node, storage, accessMode) do
    var = {tab, :where_to_commit}
    {blocked, old} = is_tab_blocked(val(var))
    del = :lists.keydelete(node, 1, old)

    case accessMode do
      :read_write ->
        new = :lists.sort([{node, storage} | del])
        set(var, mark_blocked_tab(blocked, new))
        :mnesia_lib.add_lsort({tab, :where_to_write}, node)

      :read_only ->
        set(var, mark_blocked_tab(blocked, del))
        :mnesia_lib.del({tab, :where_to_write}, node)
    end

    update_where_to_wlock(tab)
    add({tab, :active_replicas}, node)
  end

  def del_active_replica(tab, node) do
    var = {tab, :where_to_commit}
    {blocked, old} = is_tab_blocked(val(var))
    del = :lists.keydelete(node, 1, old)
    new = :lists.sort(del)
    set(var, mark_blocked_tab(blocked, new))
    :mnesia_lib.del({tab, :active_replicas}, node)
    :mnesia_lib.del({tab, :where_to_write}, node)
    update_where_to_wlock(tab)
  end

  def change_table_access_mode(cs) do
    w = fn ->
      tab = r_cstruct(cs, :name)

      :lists.foreach(
        fn n ->
          add_active_replica(tab, n, cs)
        end,
        val({tab, :active_replicas})
      )
    end

    update(w)
  end

  def change_table_majority(cs) do
    w = fn ->
      tab = r_cstruct(cs, :name)
      set({tab, :majority}, r_cstruct(cs, :majority))
      update_where_to_wlock(tab)
    end

    update(w)
  end

  defp update_where_to_wlock(tab) do
    wNodes = val({tab, :where_to_write})

    majority =
      try do
        :ets.lookup_element(:mnesia_gvar, {tab, :majority}, 2)
      catch
        :error, _ ->
          {:EXIT, {:badarg, []}}
      end == true

    set({tab, :where_to_wlock}, {wNodes, majority})
  end

  def unannounce_add_table_copy(tab, to) do
    try do
      del_active_replica(tab, to)
    catch
      :error, _ ->
        :ok
    end

    try do
      ^to = val({tab, :where_to_read})
      :mnesia_lib.set_remote_where_to_read(tab)
    catch
      _, _ ->
        :ignore
    end
  end

  defp user_sync_tab(tab) do
    case val(:debug) do
      :trace ->
        :mnesia_subscr.subscribe(
          :erlang.whereis(:mnesia_event),
          {:table, tab}
        )

      _ ->
        :ignore
    end

    case :erlang.erase({:sync_tab, tab}) do
      :undefined ->
        :ok

      pids ->
        :lists.foreach(
          fn pid ->
            sync_reply(pid, tab)
          end,
          pids
        )
    end
  end

  def i_have_tab(tab) do
    case val({tab, :local_content}) do
      true ->
        :mnesia_lib.set_local_content_whereabouts(tab)

      false ->
        set({tab, :where_to_read}, node())
    end

    add_active_replica(tab, node())
  end

  def sync_and_block_table_whereabouts(tab, toNode, remoteS, accessMode)
      when tab != :schema do
    current = val({:current, :db_nodes})

    ns =
      case :lists.member(toNode, current) do
        true ->
          current -- [toNode]

        false ->
          current
      end

    _ = remote_call(toNode, :block_table, [tab])

    for node <- [toNode | ns] do
      remote_call(node, :add_active_replica, [tab, toNode, remoteS, accessMode])
    end

    :ok
  end

  def sync_del_table_copy_whereabouts(tab, toNode) when tab != :schema do
    current = val({:current, :db_nodes})

    ns =
      case :lists.member(toNode, current) do
        true ->
          current

        false ->
          [toNode | current]
      end

    args = [tab, toNode]

    for node <- ns do
      remote_call(node, :unannounce_add_table_copy, args)
    end

    :ok
  end

  def get_info(timeout) do
    case :erlang.whereis(:mnesia_controller) do
      :undefined ->
        {:timeout, timeout}

      pid ->
        send(pid, {self(), :get_state})

        receive do
          {:mnesia_controller, state = r_state(loader_queue: lQ, late_loader_queue: lLQ)} ->
            {:info,
             r_state(state,
               loader_queue: :gb_trees.to_list(lQ),
               late_loader_queue: :gb_trees.to_list(lLQ)
             )}
        after
          timeout ->
            {:timeout, timeout}
        end
    end
  end

  def get_workers(timeout) do
    case :erlang.whereis(:mnesia_controller) do
      :undefined ->
        {:timeout, timeout}

      pid ->
        send(pid, {self(), :get_state})

        receive do
          {:mnesia_controller, state = r_state()} ->
            {:workers, get_loaders(state), get_senders(state), r_state(state, :dumper_pid)}
        after
          timeout ->
            {:timeout, timeout}
        end
    end
  end

  def info() do
    tabs = :mnesia_lib.local_active_tables()
    :io.format('---> Active tables <--- ~n', [])
    info(tabs)
  end

  defp info([tab | tail]) do
    case val({tab, :storage_type}) do
      :disc_only_copies ->
        info_format(tab, :dets.info(tab, :size), :dets.info(tab, :file_size), 'bytes on disc')

      {:ext, alias, mod} ->
        info_format(
          tab,
          mod.info(alias, tab, :size),
          mod.info(alias, tab, :memory),
          'words of mem'
        )

      _ ->
        info_format(tab, :ets.info(tab, :size), :ets.info(tab, :memory), 'words of mem')
    end

    info(tail)
  end

  defp info([]) do
    :ok
  end

  defp info_format(tab, size, mem, media) do
    strT = :mnesia_lib.pad_name(:erlang.atom_to_list(tab), 15, [])
    strS = :mnesia_lib.pad_name(:erlang.integer_to_list(size), 8, [])
    strM = :mnesia_lib.pad_name(:erlang.integer_to_list(mem), 8, [])
    :io.format('~ts: with ~s records occupying ~s ~s~n', [strT, strS, strM, media])
  end

  defp handle_early_msgs([msg | msgs], state) do
    case handle_early_msg(msg, state) do
      {:stop, reason, state2} ->
        {:stop, reason, state2}

      {:noreply, state2} ->
        handle_early_msgs(msgs, state2)

      {:reply, reply, state2} ->
        {:call, _Call, from} = msg
        reply(from, reply)
        handle_early_msgs(msgs, state2)
    end
  end

  defp handle_early_msgs([], state) do
    noreply(state)
  end

  defp handle_early_msg({:call, msg, from}, state) do
    handle_call(msg, from, state)
  end

  defp handle_early_msg({:cast, msg}, state) do
    handle_cast(msg, state)
  end

  defp handle_early_msg({:info, msg}, state) do
    handle_info(msg, state)
  end

  defp noreply(state) do
    {:noreply, state}
  end

  defp reply(:undefined, reply) do
    reply
  end

  defp reply(replyTo, reply) do
    :gen_server.reply(replyTo, reply)
    reply
  end

  defp add_worker(worker = r_dump_log(), state) do
    initBy = r_dump_log(worker, :initiated_by)
    queue = r_state(state, :dumper_queue)

    status =
      case :lists.keymember(initBy, r_dump_log(:initiated_by), queue) do
        true when r_dump_log(worker, :opt_reply_to) == :undefined ->
          detectedBy = {:dump_log, initBy}
          event = {:mnesia_overload, detectedBy}
          :mnesia_lib.report_system_event(event)
          true

        _ ->
          false
      end

    :mnesia_recover.log_dump_overload(status)
    queue2 = queue ++ [worker]
    state2 = r_state(state, dumper_queue: queue2)
    opt_start_worker(state2)
  end

  defp add_worker(worker = r_schema_commit_lock(), state) do
    queue = r_state(state, :dumper_queue)
    queue2 = queue ++ [worker]
    state2 = r_state(state, dumper_queue: queue2)
    opt_start_worker(state2)
  end

  defp add_worker(worker = r_net_load(), state) do
    opt_start_worker(add_loader(r_net_load(worker, :table), worker, state))
  end

  defp add_worker(worker = r_send_table(), state) do
    queue = r_state(state, :sender_queue)
    state2 = r_state(state, sender_queue: queue ++ [worker])
    opt_start_worker(state2)
  end

  defp add_worker(worker = r_disc_load(), state) do
    opt_start_worker(add_loader(r_disc_load(worker, :table), worker, state))
  end

  defp add_worker(worker = r_block_controller(), state) do
    queue = r_state(state, :dumper_queue)
    queue2 = [worker | queue]
    state2 = r_state(state, dumper_queue: queue2)
    opt_start_worker(state2)
  end

  defp add_loader(tab, worker, state = r_state(loader_queue: lQ0)) do
    case :gb_trees.is_defined(tab, lQ0) do
      true ->
        state

      false ->
        lQ = :gb_trees.insert(tab, worker, lQ0)
        r_state(state, loader_queue: lQ)
    end
  end

  defp opt_start_worker(state) when r_state(state, :is_stopping) == true do
    state
  end

  defp opt_start_worker(state) do
    case r_state(state, :dumper_queue) do
      [worker | _Rest]
      when r_state(state, :dumper_pid) == :undefined ->
        cond do
          elem(worker, 0) === :schema_commit_lock ->
            replyTo = r_schema_commit_lock(worker, :owner)
            reply(replyTo, :granted)
            {owner, _Tag} = replyTo
            opt_start_loader(r_state(state, dumper_pid: owner))

          elem(worker, 0) === :dump_log ->
            pid = spawn_link(:mnesia_controller, :dump_and_reply, [self(), worker])
            state2 = r_state(state, dumper_pid: pid)
            state3 = opt_start_sender(state2)
            opt_start_loader(state3)

          elem(worker, 0) === :block_controller ->
            case {get_senders(state), get_loaders(state)} do
              {[], []} ->
                replyTo = r_block_controller(worker, :owner)
                reply(replyTo, :granted)
                {owner, _Tag} = replyTo
                r_state(state, dumper_pid: owner)

              _ ->
                state
            end
        end

      _ ->
        state2 = opt_start_sender(state)
        opt_start_loader(state2)
    end
  end

  defp opt_start_sender(state) do
    case r_state(state, :sender_queue) do
      [] ->
        state

      senderQ ->
        {newS, kept} = opt_start_sender2(senderQ, get_senders(state), [], get_loaders(state))
        r_state(state, sender_pid: newS, sender_queue: kept)
    end
  end

  defp opt_start_sender2([], pids, kept, _) do
    {pids, kept}
  end

  defp opt_start_sender2([sender | r], pids, kept, loaderQ) do
    tab = r_send_table(sender, :table)
    active = val({tab, :active_replicas})
    igotIt = :lists.member(node(), active)

    isLoading =
      :lists.any(
        fn {_Pid, loader} ->
          tab == :erlang.element(r_net_load(:table), loader)
        end,
        loaderQ
      )

    cond do
      igotIt and isLoading ->
        opt_start_sender2(r, pids, [sender | kept], loaderQ)

      igotIt ->
        pid = spawn_link(:mnesia_controller, :send_and_reply, [self(), sender])
        opt_start_sender2(r, [{pid, sender} | pids], kept, loaderQ)

      true ->
        verbose('Send table failed ~tp not active on this node ~n', [tab])
        send(r_send_table(sender, :receiver_pid), {:copier_done, node()})
        opt_start_sender2(r, pids, kept, loaderQ)
    end
  end

  defp opt_start_loader(state = r_state(loader_queue: loaderQ)) do
    current = get_loaders(state)
    max = max_loaders()

    case :gb_trees.is_empty(loaderQ) do
      true ->
        state

      _ when length(current) >= max ->
        state

      false ->
        schemaQueue = r_state(state, :dumper_queue)

        case :lists.keymember(:schema_commit_lock, 1, schemaQueue) do
          false ->
            case pick_next(loaderQ) do
              {:none, rest} ->
                r_state(state, loader_queue: rest)

              {worker, rest} ->
                case already_loading(worker, get_loaders(state)) do
                  true ->
                    opt_start_loader(r_state(state, loader_queue: rest))

                  false ->
                    pid = load_and_reply(self(), worker)

                    r_state(state,
                      loader_pid: [
                        {pid, worker}
                        | get_loaders(state)
                      ],
                      loader_queue: rest
                    )
                end
            end

          true ->
            state
        end
    end
  end

  defp already_loading(r_net_load(table: tab), loaders) do
    already_loading2(tab, loaders)
  end

  defp already_loading(r_disc_load(table: tab), loaders) do
    already_loading2(tab, loaders)
  end

  defp already_loading2(tab, [{_, r_net_load(table: tab)} | _]) do
    true
  end

  defp already_loading2(tab, [{_, r_disc_load(table: tab)} | _]) do
    true
  end

  defp already_loading2(tab, [_ | rest]) do
    already_loading2(tab, rest)
  end

  defp already_loading2(_, []) do
    false
  end

  def start_remote_sender(node, tab, receiver, storage) do
    msg = r_send_table(table: tab, receiver_pid: receiver, remote_storage: storage)
    :gen_server.cast({:mnesia_controller, node}, msg)
  end

  def dump_and_reply(replyTo, worker) do
    res =
      case r_dump_log(worker, :operation) do
        :dump_log ->
          :mnesia_dumper.opt_dump_log(r_dump_log(worker, :initiated_by))

        f when is_function(f, 0) ->
          f.()
      end

    send(replyTo, r_dumper_done(worker_pid: self(), worker_res: res))
    :erlang.unlink(replyTo)
    exit(:normal)
  end

  def send_and_reply(replyTo, worker) do
    res =
      :mnesia_loader.send_table(
        r_send_table(worker, :receiver_pid),
        r_send_table(worker, :table),
        r_send_table(worker, :remote_storage)
      )

    send(replyTo, r_sender_done(worker_pid: self(), worker_res: res))
    :erlang.unlink(replyTo)
    exit(:normal)
  end

  def load_and_reply(replyTo, worker) do
    load = load_table_fun(worker)

    sendAndReply = fn ->
      :erlang.process_flag(:trap_exit, true)
      done = load.()
      send(replyTo, r_loader_done(done, worker_pid: self()))
      :erlang.unlink(replyTo)
      exit(:normal)
    end

    spawn_link(sendAndReply)
  end

  defp load_table_fun(r_net_load(cstruct: cs, table: tab, reason: reason, opt_reply_to: replyTo)) do
    localC = val({tab, :local_content})
    accessMode = val({tab, :access_mode})
    readNode = val({tab, :where_to_read})
    active = filter_active(tab)

    done =
      r_loader_done(
        is_loaded: true,
        table_name: tab,
        needs_announce: false,
        needs_sync: false,
        needs_reply: replyTo != :undefined,
        reply_to: replyTo,
        reply: {:loaded, :ok}
      )

    addTableCopy =
      case reason do
        {:dumper, {:add_table_copy, _}} ->
          true

        _ ->
          false
      end

    onlyRamCopies =
      case cs do
        r_cstruct(disc_copies: dC, disc_only_copies: dOC, external_copies: ext) ->
          [] === (dC ++ dOC ++ ext) -- [node()]

        _ ->
          false
      end

    cond do
      readNode == node() ->
        fn ->
          done
        end

      localC == true ->
        fn ->
          res =
            :mnesia_loader.disc_load_table(
              tab,
              :load_local_content
            )

          r_loader_done(done, reply: res, needs_announce: true, needs_sync: true)
        end

      accessMode == :read_only and not addTableCopy ->
        fn ->
          disc_load_table(tab, reason, replyTo)
        end

      active === [] and addTableCopy and onlyRamCopies ->
        fn ->
          disc_load_table(tab, reason, replyTo)
        end

      true ->
        fn ->
          res = :mnesia_loader.net_load_table(tab, reason, active, cs)

          case res do
            {:loaded, :ok} ->
              r_loader_done(done, needs_sync: true, reply: res)

            {:not_loaded, _} ->
              r_loader_done(done, is_loaded: false, reply: res)
          end
        end
    end
  end

  defp load_table_fun(r_disc_load(table: tab, reason: reason, opt_reply_to: replyTo)) do
    readNode = val({tab, :where_to_read})
    active = filter_active(tab)

    done =
      r_loader_done(
        is_loaded: true,
        table_name: tab,
        needs_announce: false,
        needs_sync: false,
        needs_reply: false
      )

    cond do
      active == [] and readNode == :nowhere ->
        fn ->
          disc_load_table(tab, reason, replyTo)
        end

      readNode == :nowhere ->
        cs = val({tab, :cstruct})

        fn ->
          case :mnesia_loader.net_load_table(tab, reason, active, cs) do
            {:loaded, :ok} ->
              r_loader_done(done, needs_sync: true)

            {:not_loaded, :storage_unknown} ->
              r_loader_done(done, is_loaded: false)

            {:not_loaded, errReason} ->
              r_loader_done(done,
                is_loaded: false,
                reply: {:not_loaded, errReason}
              )
          end
        end

      true ->
        fn ->
          done
        end
    end
  end

  defp disc_load_table(tab, reason, replyTo) do
    done =
      r_loader_done(
        is_loaded: true,
        table_name: tab,
        needs_announce: false,
        needs_sync: false,
        needs_reply: replyTo != :undefined,
        reply_to: replyTo,
        reply: {:loaded, :ok}
      )

    res = :mnesia_loader.disc_load_table(tab, reason)

    cond do
      res == {:loaded, :ok} ->
        r_loader_done(done, needs_announce: true, needs_sync: true, reply: res)

      replyTo != :undefined ->
        r_loader_done(done, is_loaded: false, reply: res)

      true ->
        fatal('Cannot load table ~tp from disc: ~tp~n', [tab, res])
    end
  end

  defp filter_active(tab) do
    byForce = val({tab, :load_by_force})
    active = val({tab, :active_replicas})
    masters = :mnesia_recover.get_master_nodes(tab)
    ns = do_filter_active(byForce, active, masters)

    lS =
      try do
        :ets.lookup_element(:mnesia_gvar, {tab, :storage_type}, 2)
      catch
        :error, _ ->
          {:EXIT, {:badarg, []}}
      end

    dOC = val({tab, :disc_only_copies})

    {good, worse} =
      case lS do
        :disc_only_copies ->
          g = :mnesia_lib.intersect(ns, dOC)
          {g, ns -- g}

        _ ->
          g = ns -- dOC
          {g, ns -- g}
      end

    len = length(good)

    cond do
      len > 0 ->
        r = :erlang.phash(node(), len + 1)
        random(r - 1, good, worse)

      true ->
        worse
    end
  end

  defp random(n, [h | r], acc) when n > 0 do
    random(n - 1, r, [h | acc])
  end

  defp random(0, l, acc) do
    l ++ acc
  end

  defp do_filter_active(true, active, _Masters) do
    active
  end

  defp do_filter_active(false, active, []) do
    active
  end

  defp do_filter_active(false, active, masters) do
    :mnesia_lib.intersect(active, masters)
  end
end
