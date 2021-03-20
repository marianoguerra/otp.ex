defmodule :m_mnesia_tm do
  use Bitwise
  import :mnesia_lib, only: [dbg_out: 2, fatal: 2, set: 2, verbose: 2]
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
    coordinators: :gb_trees.empty(),
    participants: :gb_trees.empty(),
    supervisor: :undefined,
    blocked_tabs: [],
    dirty_queue: [],
    fixed_tabs: []
  )

  Record.defrecord(:r_prep, :prep,
    protocol: :sym_trans,
    records: [],
    prev_tab: [],
    prev_types: :undefined,
    prev_snmp: :undefined,
    types: :undefined,
    majority: [],
    sync: false
  )

  Record.defrecord(:r_participant, :participant,
    tid: :undefined,
    pid: :undefined,
    commit: :undefined,
    disc_nodes: [],
    ram_nodes: [],
    protocol: :sym_trans
  )

  def start() do
    :mnesia_monitor.start_proc(:mnesia_tm, :mnesia_tm, :init, [self()])
  end

  def init(parent) do
    :erlang.register(:mnesia_tm, self())
    :erlang.process_flag(:trap_exit, true)
    :erlang.process_flag(:message_queue_data, :off_heap)
    ignoreFallback = :mnesia_monitor.get_env(:ignore_fallback_at_startup)
    :mnesia_bup.tm_fallback_start(ignoreFallback)
    :mnesia_schema.init(ignoreFallback)
    :mnesia_recover.init()
    early = :mnesia_monitor.init()
    allOthers = :mnesia_lib.uniq(early ++ :mnesia_lib.all_nodes()) -- [node()]
    set(:original_nodes, allOthers)
    :mnesia_recover.connect_nodes(allOthers)

    case :mnesia_monitor.use_dir() do
      true ->
        p = :mnesia_dumper.opt_dump_log(:startup)
        l = :mnesia_dumper.opt_dump_log(:startup)
        msg = 'Initial dump of log during startup: ~p~n'
        :mnesia_lib.verbose(msg, [[p, l]])
        :mnesia_log.init()

      false ->
        :ignore
    end

    :mnesia_schema.purge_tmp_files()
    :mnesia_recover.next_garb()
    :mnesia_recover.next_check_overload()
    :ok

    case val(:debug) do
      debug when debug != :debug and debug != :trace ->
        :ignore

      _ ->
        :mnesia_subscr.subscribe(
          :erlang.whereis(:mnesia_event),
          {:table, :schema}
        )
    end

    :proc_lib.init_ack(parent, {:ok, self()})
    doit_loop(r_state(supervisor: parent))
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

  defp reply({from, ref}, r) do
    send(from, {:mnesia_tm, ref, r})
  end

  defp reply(from, r) do
    send(from, {:mnesia_tm, node(), r})
  end

  defp reply(from, r, state) do
    reply(from, r)
    doit_loop(state)
  end

  defp req(r) do
    case :erlang.whereis(:mnesia_tm) do
      :undefined ->
        {:error, {:node_not_running, node()}}

      pid ->
        ref = make_ref()
        send(pid, {{self(), ref}, r})
        rec(pid, ref)
    end
  end

  defp rec() do
    rec(:erlang.whereis(:mnesia_tm))
  end

  defp rec(pid) when is_pid(pid) do
    receive do
      {:mnesia_tm, _, reply} ->
        reply

      {:EXIT, ^pid, _} ->
        {:error, {:node_not_running, node()}}
    end
  end

  defp rec(:undefined) do
    {:error, {:node_not_running, node()}}
  end

  defp rec(pid, ref) do
    receive do
      {:mnesia_tm, ^ref, reply} ->
        reply

      {:EXIT, ^pid, _} ->
        {:error, {:node_not_running, node()}}
    end
  end

  defp tmlink({from, ref}) when is_reference(ref) do
    :erlang.link(from)
  end

  defp tmlink(from) do
    :erlang.link(from)
  end

  defp tmpid({pid, _Ref}) when is_pid(pid) do
    pid
  end

  defp tmpid(pid) do
    pid
  end

  def mnesia_down(node) do
    case :erlang.whereis(:mnesia_tm) do
      :undefined ->
        :mnesia_monitor.mnesia_down(:mnesia_tm, node)

      pid ->
        send(pid, {:mnesia_down, node})
        :ok
    end
  end

  def prepare_checkpoint(nodes, cp) do
    :rpc.multicall(nodes, :mnesia_tm, :prepare_checkpoint, [cp])
  end

  def prepare_checkpoint(cp) do
    req({:prepare_checkpoint, cp})
  end

  def block_tab(tab) do
    req({:block_tab, tab})
  end

  def unblock_tab(tab) do
    req({:unblock_tab, tab})
  end

  defp doit_loop(
         r_state(coordinators: coordinators, participants: participants, supervisor: sup) = state
       ) do
    receive do
      {_From, {:async_dirty, tid, commit, tab}} ->
        case :lists.member(tab, r_state(state, :blocked_tabs)) do
          false ->
            do_async_dirty(tid, new_cr_format(commit), tab)
            doit_loop(state)

          true ->
            item = {:async_dirty, tid, new_cr_format(commit), tab}

            state2 =
              r_state(state,
                dirty_queue: [
                  item
                  | r_state(state, :dirty_queue)
                ]
              )

            doit_loop(state2)
        end

      {from, {:sync_dirty, tid, commit, tab}} ->
        case :lists.member(tab, r_state(state, :blocked_tabs)) do
          false ->
            do_sync_dirty(from, tid, new_cr_format(commit), tab)
            doit_loop(state)

          true ->
            item = {:sync_dirty, from, tid, new_cr_format(commit), tab}

            state2 =
              r_state(state,
                dirty_queue: [
                  item
                  | r_state(state, :dirty_queue)
                ]
              )

            doit_loop(state2)
        end

      {from, :start_outer} ->
        try do
          _ = :ets.new(:mnesia_trans_store, [:bag, :public])
        catch
          :error, reason ->
            msg = 'Cannot create an ets table for the local transaction store'
            reply(from, {:error, {:system_limit, msg, reason}}, state)
        else
          etab ->
            tmlink(from)
            c = :mnesia_recover.incr_trans_tid_serial()
            :ets.insert(etab, {:nodes, node()})
            tid = r_tid(pid: tmpid(from), counter: c)
            a2 = :gb_trees.insert(tid, [etab], coordinators)
            s2 = r_state(state, coordinators: a2)
            reply(from, {:new_tid, tid, etab}, s2)
        end

      {from, {:ask_commit, protocol, tid, commit0, discNs, ramNs}} ->
        :ok
        :mnesia_checkpoint.tm_enter_pending(tid, discNs, ramNs)
        commit = new_cr_format(commit0)

        pid =
          cond do
            node(r_tid(tid, :pid)) === node() ->
              :erlang.error({:internal_error, :local_node})

            protocol === :asym_trans or protocol === :sync_asym_trans ->
              args = [protocol, tmpid(from), tid, commit, discNs, ramNs]
              spawn_link(:mnesia_tm, :commit_participant, args)

            true ->
              reply(from, {:vote_yes, tid})
              :nopid
          end

        p =
          r_participant(
            tid: tid,
            pid: pid,
            commit: commit,
            disc_nodes: discNs,
            ram_nodes: ramNs,
            protocol: protocol
          )

        state2 = r_state(state, participants: :gb_trees.insert(tid, p, participants))
        doit_loop(state2)

      {tid, :do_commit} ->
        case :gb_trees.lookup(tid, participants) do
          :none ->
            verbose('Tried to commit a non participant transaction ~p~n', [tid])
            doit_loop(state)

          {:value, p} ->
            :ok

            case r_participant(p, :pid) do
              :nopid ->
                commit = r_participant(p, :commit)
                member = :lists.member(node(), r_participant(p, :disc_nodes))

                cond do
                  member == false ->
                    :ignore

                  r_participant(p, :protocol) == :sym_trans ->
                    :mnesia_log.log(commit)

                  r_participant(p, :protocol) == :sync_sym_trans ->
                    :mnesia_log.slog(commit)
                end

                :mnesia_recover.note_decision(tid, :committed)
                do_commit(tid, commit)

                cond do
                  r_participant(p, :protocol) == :sync_sym_trans ->
                    send(r_tid(tid, :pid), {:mnesia_tm, node(), {:committed, tid}})

                  true ->
                    :ignore
                end

                :mnesia_locker.release_tid(tid)
                transaction_terminated(tid)
                :ok

                doit_loop(
                  r_state(state,
                    participants:
                      :gb_trees.delete(
                        tid,
                        participants
                      )
                  )
                )

              pid when is_pid(pid) ->
                send(pid, {tid, :committed})
                :ok
                doit_loop(state)
            end
        end

      {tid, :simple_commit} ->
        :mnesia_recover.note_decision(tid, :committed)
        :mnesia_locker.release_tid(tid)
        transaction_terminated(tid)
        doit_loop(state)

      {tid, {:do_abort, reason}} ->
        :ok

        case :gb_trees.lookup(tid, participants) do
          :none ->
            verbose('Tried to abort a non participant transaction ~p: ~tp~n', [tid, reason])
            :mnesia_locker.release_tid(tid)
            doit_loop(state)

          {:value, p} ->
            case r_participant(p, :pid) do
              :nopid ->
                commit = r_participant(p, :commit)
                :mnesia_recover.note_decision(tid, :aborted)
                do_abort(tid, commit)

                cond do
                  r_participant(p, :protocol) == :sync_sym_trans ->
                    send(r_tid(tid, :pid), {:mnesia_tm, node(), {:aborted, tid}})

                  true ->
                    :ignore
                end

                transaction_terminated(tid)
                :mnesia_locker.release_tid(tid)
                :ok

                doit_loop(
                  r_state(state,
                    participants:
                      :gb_trees.delete(
                        tid,
                        participants
                      )
                  )
                )

              pid when is_pid(pid) ->
                send(pid, {tid, {:do_abort, reason}})
                :ok
                doit_loop(state)
            end
        end

      {from, {:add_store, tid}} ->
        try do
          _ = :ets.new(:mnesia_trans_store, [:bag, :public])
        catch
          :error, reason ->
            msg = 'Cannot create an ets table for a nested local transaction store'
            reply(from, {:error, {:system_limit, msg, reason}}, state)
        else
          etab ->
            a2 = add_coord_store(coordinators, tid, etab)
            reply(from, {:new_store, etab}, r_state(state, coordinators: a2))
        end

      {from, {:del_store, tid, current, obsolete, propagateStore}} ->
        opt_propagate_store(current, obsolete, propagateStore)
        a2 = del_coord_store(coordinators, tid, current, obsolete)
        reply(from, :store_erased, r_state(state, coordinators: a2))

      {:EXIT, pid, reason} ->
        handle_exit(pid, reason, state)

      {from, {:restart, tid, store}} ->
        a2 = restore_stores(coordinators, tid, store)
        clear_fixtable([store])
        :ets.match_delete(store, :_)
        :ets.insert(store, {:nodes, node()})
        reply(from, {:restarted, tid}, r_state(state, coordinators: a2))

      {:delete_transaction, tid} ->
        case :gb_trees.is_defined(tid, participants) do
          false ->
            case :gb_trees.lookup(tid, coordinators) do
              :none ->
                verbose('** ERROR ** Tried to delete a non transaction ~p~n', [tid])
                doit_loop(state)

              {:value, etabs} ->
                clear_fixtable(etabs)
                erase_ets_tabs(etabs)
                transaction_terminated(tid)

                doit_loop(
                  r_state(state,
                    coordinators:
                      :gb_trees.delete(
                        tid,
                        coordinators
                      )
                  )
                )
            end

          true ->
            transaction_terminated(tid)

            state2 =
              r_state(state,
                participants:
                  :gb_trees.delete(
                    tid,
                    participants
                  )
              )

            doit_loop(state2)
        end

      {:sync_trans_serial, tid} ->
        :mnesia_recover.sync_trans_tid_serial(tid)
        doit_loop(state)

      {from, :info} ->
        reply(
          from,
          {:info, :gb_trees.values(participants), :gb_trees.to_list(coordinators)},
          state
        )

      {:mnesia_down, n} ->
        verbose('Got mnesia_down from ~p, reconfiguring...~n', [n])

        reconfigure_coordinators(
          n,
          :gb_trees.to_list(coordinators)
        )

        tids = :gb_trees.keys(participants)

        reconfigure_participants(
          n,
          :gb_trees.values(participants)
        )

        newState = clear_fixtable(n, state)
        :mnesia_locker.mnesia_down(n, tids)
        :mnesia_monitor.mnesia_down(:mnesia_tm, n)
        doit_loop(newState)

      {from, {:unblock_me, tab}} ->
        case :lists.member(tab, r_state(state, :blocked_tabs)) do
          false ->
            verbose('Wrong dirty Op blocked on ~p ~tp ~p', [node(), tab, from])
            reply(from, :unblocked)
            doit_loop(state)

          true ->
            item = {tab, :unblock_me, from}

            state2 =
              r_state(state,
                dirty_queue: [
                  item
                  | r_state(state, :dirty_queue)
                ]
              )

            doit_loop(state2)
        end

      {from, {:block_tab, tab}} ->
        state2 =
          r_state(state,
            blocked_tabs: [
              tab
              | r_state(state, :blocked_tabs)
            ]
          )

        reply(from, :ok, state2)

      {from, {:unblock_tab, tab}} ->
        blockedTabs2 = r_state(state, :blocked_tabs) -- [tab]

        case :lists.member(tab, blockedTabs2) do
          false ->
            :mnesia_controller.unblock_table(tab)
            queue = process_dirty_queue(tab, r_state(state, :dirty_queue))

            state2 =
              r_state(state,
                blocked_tabs: blockedTabs2,
                dirty_queue: queue
              )

            reply(from, :ok, state2)

          true ->
            state2 = r_state(state, blocked_tabs: blockedTabs2)
            reply(from, :ok, state2)
        end

      {from, {:prepare_checkpoint, cp}} ->
        res = :mnesia_checkpoint.tm_prepare(cp)

        case res do
          {:ok, _Name, ignoreNew, _Node} ->
            prepare_pending_coordinators(
              :gb_trees.to_list(coordinators),
              ignoreNew
            )

            prepare_pending_participants(
              :gb_trees.values(participants),
              ignoreNew
            )

          {:error, _Reason} ->
            :ignore
        end

        reply(from, res, state)

      {from, {:fixtable, [tab, lock, requester]}} ->
        case (try do
                :ets.lookup_element(:mnesia_gvar, {tab, :storage_type}, 2)
              catch
                :error, _ ->
                  {:EXIT, {:badarg, []}}
              end) do
          {:EXIT, _} ->
            reply(from, :error, state)

          storage ->
            :mnesia_lib.db_fixtable(storage, tab, lock)
            newState = manage_fixtable(tab, lock, requester, state)
            reply(from, node(), newState)
        end

      {:system, from, msg} ->
        dbg_out('~p got {system, ~p, ~tp}~n', [:mnesia_tm, from, msg])
        :sys.handle_system_msg(msg, from, sup, :mnesia_tm, [], state)

      msg ->
        verbose('** ERROR ** ~p got unexpected message: ~tp~n', [:mnesia_tm, msg])
        doit_loop(state)
    end
  end

  defp do_sync_dirty(from, tid, commit, _Tab) do
    :ok
    res = do_dirty(tid, commit)
    :ok
    send(from, {:mnesia_tm, node(), {:dirty_res, res}})
  end

  defp do_async_dirty(tid, commit, _Tab) do
    :ok
    do_dirty(tid, commit)
    :ok
  end

  defp process_dirty_queue(tab, [item | queue]) do
    queue2 = process_dirty_queue(tab, queue)

    case item do
      {:async_dirty, tid, commit, ^tab} ->
        do_async_dirty(tid, commit, tab)
        queue2

      {:sync_dirty, from, tid, commit, ^tab} ->
        do_sync_dirty(from, tid, commit, tab)
        queue2

      {^tab, :unblock_me, from} ->
        reply(from, :unblocked)
        queue2

      _ ->
        [item | queue2]
    end
  end

  defp process_dirty_queue(_Tab, []) do
    []
  end

  defp prepare_pending_coordinators(
         [{tid, [store | _Etabs]} | coords],
         ignoreNew
       ) do
    try do
      :ets.lookup(store, :pending)
    catch
      :error, _ ->
        prepare_pending_coordinators(coords, ignoreNew)
    else
      [] ->
        prepare_pending_coordinators(coords, ignoreNew)

      [pending] ->
        case :lists.member(tid, ignoreNew) do
          false ->
            :mnesia_checkpoint.tm_enter_pending(pending)

          true ->
            :ignore
        end

        prepare_pending_coordinators(coords, ignoreNew)
    end
  end

  defp prepare_pending_coordinators([], _IgnoreNew) do
    :ok
  end

  defp prepare_pending_participants([part | parts], ignoreNew) do
    tid = r_participant(part, :tid)
    d = r_participant(part, :disc_nodes)
    r = r_participant(part, :ram_nodes)

    case :lists.member(tid, ignoreNew) do
      false ->
        :mnesia_checkpoint.tm_enter_pending(tid, d, r)

      true ->
        :ignore
    end

    prepare_pending_participants(parts, ignoreNew)
  end

  defp prepare_pending_participants([], _IgnoreNew) do
    :ok
  end

  defp handle_exit(pid, _Reason, state) when node(pid) != node() do
    doit_loop(state)
  end

  defp handle_exit(pid, _Reason, state)
       when pid == r_state(state, :supervisor) do
    do_stop(state)
  end

  defp handle_exit(pid, reason, state) do
    case pid_search_delete(
           pid,
           :gb_trees.to_list(r_state(state, :coordinators))
         ) do
      {:none, _} ->
        ps = :gb_trees.values(r_state(state, :participants))

        case :mnesia_lib.key_search_delete(pid, r_participant(:pid), ps) do
          {:none, _} ->
            doit_loop(state)

          {p = r_participant(), _RestP} ->
            fatal('Participant ~p in transaction ~p died ~tp~n', [
              r_participant(p, :pid),
              r_participant(p, :tid),
              reason
            ])

            newPs =
              :gb_trees.delete(
                r_participant(p, :tid),
                r_state(state, :participants)
              )

            doit_loop(r_state(state, participants: newPs))
        end

      {{tid, etabs}, restC} ->
        recover_coordinator(tid, etabs)
        doit_loop(r_state(state, coordinators: restC))
    end
  end

  defp recover_coordinator(tid, etabs) do
    verbose('Coordinator ~p in transaction ~p died.~n', [r_tid(tid, :pid), tid])
    store = hd(etabs)
    checkNodes = get_elements(:nodes, store)
    tellNodes = checkNodes -- [node()]

    try do
      arrange(tid, store, :async)
    catch
      _, reason ->
        dbg_out('Recovery of coordinator ~p failed: ~tp~n', [tid, {reason, __STACKTRACE__}])
        protocol = :asym_trans
        tell_outcome(tid, protocol, node(), checkNodes, tellNodes)
    else
      {_N, prep} ->
        protocol = r_prep(prep, :protocol)
        outcome = tell_outcome(tid, protocol, node(), checkNodes, tellNodes)
        cR = r_prep(prep, :records)
        {discNs, ramNs} = commit_nodes(cR, [], [])

        case :lists.keysearch(node(), r_commit(:node), cR) do
          {:value, local} ->
            :ok
            recover_coordinator(tid, protocol, outcome, local, discNs, ramNs)
            :ok

          false ->
            :ok
        end
    end

    erase_ets_tabs(etabs)
    transaction_terminated(tid)
    :mnesia_locker.release_tid(tid)
  end

  defp recover_coordinator(tid, :sym_trans, :committed, local, _, _) do
    :mnesia_recover.note_decision(tid, :committed)
    do_dirty(tid, local)
  end

  defp recover_coordinator(tid, :sym_trans, :aborted, _Local, _, _) do
    :mnesia_recover.note_decision(tid, :aborted)
  end

  defp recover_coordinator(tid, :sync_sym_trans, :committed, local, _, _) do
    :mnesia_recover.note_decision(tid, :committed)
    do_dirty(tid, local)
  end

  defp recover_coordinator(tid, :sync_sym_trans, :aborted, _Local, _, _) do
    :mnesia_recover.note_decision(tid, :aborted)
  end

  defp recover_coordinator(tid, protocol, :committed, local, discNs, ramNs)
       when protocol === :asym_trans or
              protocol === :sync_asym_trans do
    d = r_decision(tid: tid, outcome: :committed, disc_nodes: discNs, ram_nodes: ramNs)
    :mnesia_recover.log_decision(d)
    do_commit(tid, local)
  end

  defp recover_coordinator(tid, protocol, :aborted, local, discNs, ramNs)
       when protocol === :asym_trans or
              protocol === :sync_asym_trans do
    d = r_decision(tid: tid, outcome: :aborted, disc_nodes: discNs, ram_nodes: ramNs)
    :mnesia_recover.log_decision(d)
    do_abort(tid, local)
  end

  defp restore_stores(coords, tid, store) do
    etstabs = :gb_trees.get(tid, coords)
    remaining = :lists.delete(store, etstabs)
    erase_ets_tabs(remaining)
    :gb_trees.update(tid, [store], coords)
  end

  defp add_coord_store(coords, tid, etab) do
    stores = :gb_trees.get(tid, coords)
    :gb_trees.update(tid, [etab | stores], coords)
  end

  defp del_coord_store(coords, tid, current, obsolete) do
    stores = :gb_trees.get(tid, coords)

    rest =
      case stores do
        [^obsolete, ^current | tail] ->
          tail

        [^current, ^obsolete | tail] ->
          tail
      end

    :ets.delete(obsolete)
    :gb_trees.update(tid, [current | rest], coords)
  end

  defp erase_ets_tabs([h | t]) do
    :ets.delete(h)
    erase_ets_tabs(t)
  end

  defp erase_ets_tabs([]) do
    :ok
  end

  defp clear_fixtable([store | _]) do
    fixed = get_elements(:fixtable, store)

    :lists.foreach(
      fn {tab, node} ->
        :rpc.cast(node, :mnesia_tm, :fixtable, [tab, false, self()])
      end,
      fixed
    )
  end

  defp clear_fixtable(node, state = r_state(fixed_tabs: fT0)) do
    case :mnesia_lib.key_search_delete(node, 1, fT0) do
      {:none, _Ft} ->
        state

      {{^node, tabs}, fT} ->
        :lists.foreach(
          fn tab ->
            case (try do
                    :ets.lookup_element(
                      :mnesia_gvar,
                      {tab, :storage_type},
                      2
                    )
                  catch
                    :error, _ ->
                      {:EXIT, {:badarg, []}}
                  end) do
              {:EXIT, _} ->
                :ignore

              storage ->
                :mnesia_lib.db_fixtable(storage, tab, false)
            end
          end,
          tabs
        )

        r_state(state, fixed_tabs: fT)
    end
  end

  defp manage_fixtable(tab, true, requester, state = r_state(fixed_tabs: fT0)) do
    node = node(requester)

    case :mnesia_lib.key_search_delete(node, 1, fT0) do
      {:none, fT} ->
        r_state(state, fixed_tabs: [{node, [tab]} | fT])

      {{^node, tabs}, fT} ->
        r_state(state, fixed_tabs: [{node, [tab | tabs]} | fT])
    end
  end

  defp manage_fixtable(tab, false, requester, state = r_state(fixed_tabs: fT0)) do
    node = node(requester)

    case :mnesia_lib.key_search_delete(node, 1, fT0) do
      {:none, _FT} ->
        state

      {{^node, tabs0}, fT} ->
        case :lists.delete(tab, tabs0) do
          [] ->
            r_state(state, fixed_tabs: fT)

          tabs ->
            r_state(state, fixed_tabs: [{node, tabs} | fT])
        end
    end
  end

  defp pid_search_delete(pid, trs) do
    pid_search_delete(pid, trs, :none, [])
  end

  defp pid_search_delete(pid, [tr = {tid, _Ts} | trs], _Val, ack)
       when r_tid(tid, :pid) == pid do
    pid_search_delete(pid, trs, tr, ack)
  end

  defp pid_search_delete(pid, [tr | trs], val, ack) do
    pid_search_delete(pid, trs, val, [tr | ack])
  end

  defp pid_search_delete(_Pid, [], val, ack) do
    {val, :gb_trees.from_orddict(:lists.reverse(ack))}
  end

  defp transaction_terminated(tid) do
    :mnesia_checkpoint.tm_exit_pending(tid)
    pid = r_tid(tid, :pid)

    cond do
      node(pid) == node() ->
        :erlang.unlink(pid)

      true ->
        :mnesia_recover.sync_trans_tid_serial(tid)
    end
  end

  def non_transaction(oldState = {_, _, trans}, fun, args, activityKind, mod)
      when trans != :non_transaction do
    kind =
      case activityKind do
        :sync_dirty ->
          :sync

        _ ->
          :async
      end

    case transaction(oldState, fun, args, :infinity, mod, kind) do
      {:atomic, res} ->
        res

      {:aborted, res} ->
        exit(res)
    end
  end

  def non_transaction(oldState, fun, args, activityKind, mod) do
    id = {activityKind, self()}
    newState = {mod, id, :non_transaction}
    :erlang.put(:mnesia_activity_state, newState)

    try do
      apply(fun, args)
    catch
      throw ->
        throw(throw)

      :error, reason ->
        exit({reason, __STACKTRACE__})

      :exit, reason ->
        exit(reason)
    else
      {:EXIT, reason} ->
        exit(reason)

      {:aborted, reason} ->
        :mnesia.abort(reason)

      res ->
        res
    after
      case oldState do
        :undefined ->
          :erlang.erase(:mnesia_activity_state)

        _ ->
          :erlang.put(:mnesia_activity_state, oldState)
      end
    end
  end

  def transaction(oldTidTs, fun, args, retries, mod, type) do
    factor = 1

    case oldTidTs do
      :undefined ->
        execute_outer(mod, fun, args, factor, retries, type)

      {_, _, :non_transaction} ->
        res = execute_outer(mod, fun, args, factor, retries, type)
        :erlang.put(:mnesia_activity_state, oldTidTs)
        res

      {oldMod, tid, ts} ->
        execute_inner(mod, tid, oldMod, ts, fun, args, factor, retries, type)

      _ ->
        {:aborted, :nested_transaction}
    end
  end

  defp execute_outer(mod, fun, args, factor, retries, type) do
    case req(:start_outer) do
      {:error, reason} ->
        {:aborted, reason}

      {:new_tid, tid, store} ->
        ts = r_tidstore(store: store)
        newTidTs = {mod, tid, ts}
        :erlang.put(:mnesia_activity_state, newTidTs)
        execute_transaction(fun, args, factor, retries, type)
    end
  end

  defp execute_inner(mod, tid, oldMod, ts, fun, args, factor, retries, type) do
    case req({:add_store, tid}) do
      {:error, reason} ->
        {:aborted, reason}

      {:new_store, ets} ->
        copy_ets(r_tidstore(ts, :store), ets)
        up = [{oldMod, r_tidstore(ts, :store)} | r_tidstore(ts, :up_stores)]
        newTs = r_tidstore(ts, level: 1 + r_tidstore(ts, :level), store: ets, up_stores: up)
        newTidTs = {mod, tid, newTs}
        :erlang.put(:mnesia_activity_state, newTidTs)
        execute_transaction(fun, args, factor, retries, type)
    end
  end

  defp copy_ets(from, to) do
    do_copy_ets(:ets.first(from), from, to)
  end

  defp do_copy_ets(:"$end_of_table", _, _) do
    :ok
  end

  defp do_copy_ets(k, from, to) do
    objs = :ets.lookup(from, k)
    insert_objs(objs, to)
    do_copy_ets(:ets.next(from, k), from, to)
  end

  defp insert_objs([h | t], tab) do
    :ets.insert(tab, h)
    insert_objs(t, tab)
  end

  defp insert_objs([], _Tab) do
    :ok
  end

  defp execute_transaction(fun, args, factor, retries, type) do
    try do
      apply_fun(fun, args, type)
    catch
      value ->
        reason = {:aborted, {:throw, value}}
        return_abort(fun, args, reason)

      :error, reason ->
        check_exit(fun, args, factor, retries, {reason, __STACKTRACE__}, type)

      _, reason ->
        check_exit(fun, args, factor, retries, reason, type)
    else
      {:atomic, value} ->
        :mnesia_lib.incr_counter(:trans_commits)
        :erlang.erase(:mnesia_activity_state)
        flush_downs()

        try do
          :erlang.unlink(:erlang.whereis(:mnesia_tm))
        catch
          :error, _ ->
            :ok
        end

        {:atomic, value}

      {:do_abort, reason} ->
        check_exit(fun, args, factor, retries, {:aborted, reason}, type)

      {:nested_atomic, value} ->
        :mnesia_lib.incr_counter(:trans_commits)
        {:atomic, value}
    end
  end

  defp apply_fun(fun, args, type) do
    result = apply(fun, args)

    case t_commit(type) do
      :do_commit ->
        {:atomic, result}

      :do_commit_nested ->
        {:nested_atomic, result}

      {:do_abort, {:aborted, reason}} ->
        {:do_abort, reason}

      {:do_abort, _} = abort ->
        abort
    end
  end

  defp check_exit(fun, args, factor, retries, reason, type) do
    case reason do
      {:aborted, c = r_cyclic()} ->
        maybe_restart(fun, args, factor, retries, type, c)

      {:aborted, {:node_not_running, n}} ->
        maybe_restart(fun, args, factor, retries, type, {:node_not_running, n})

      {:aborted, {:bad_commit, n}} ->
        maybe_restart(fun, args, factor, retries, type, {:bad_commit, n})

      _ ->
        return_abort(fun, args, reason)
    end
  end

  defp maybe_restart(fun, args, factor, retries, type, why) do
    {mod, tid, ts} = :erlang.get(:mnesia_activity_state)

    case try_again(retries) do
      :yes when r_tidstore(ts, :level) == 1 ->
        restart(mod, tid, ts, fun, args, factor, retries, type, why)

      :yes ->
        return_abort(fun, args, why)

      :no ->
        return_abort(fun, args, {:aborted, :nomore})
    end
  end

  defp try_again(:infinity) do
    :yes
  end

  defp try_again(x) when is_number(x) and x > 1 do
    :yes
  end

  defp try_again(_) do
    :no
  end

  defp restart(mod, tid, ts, fun, args, factor0, retries0, type, why) do
    :mnesia_lib.incr_counter(:trans_restarts)
    retries = decr(retries0)

    case why do
      {:bad_commit, _N} ->
        return_abort(fun, args, why)
        factor = 1

        sleepTime =
          :mnesia_lib.random_time(
            factor,
            r_tid(tid, :counter)
          )

        dbg_out('Restarting transaction ~w: in ~wms ~w~n', [tid, sleepTime, why])
        :timer.sleep(sleepTime)
        execute_outer(mod, fun, args, factor, retries, type)

      {:node_not_running, _N} ->
        return_abort(fun, args, why)
        factor = 1

        sleepTime =
          :mnesia_lib.random_time(
            factor,
            r_tid(tid, :counter)
          )

        dbg_out('Restarting transaction ~w: in ~wms ~w~n', [tid, sleepTime, why])
        :timer.sleep(sleepTime)
        execute_outer(mod, fun, args, factor, retries, type)

      _ ->
        sleepTime =
          :mnesia_lib.random_time(
            factor0,
            r_tid(tid, :counter)
          )

        dbg_out('Restarting transaction ~w: in ~wms ~w~n', [tid, sleepTime, why])

        cond do
          factor0 != 10 ->
            :ignore

          true ->
            allNodes = val({:current, :db_nodes})
            verbose('Sync serial ~p~n', [tid])
            :rpc.abcast(allNodes, :mnesia_tm, {:sync_trans_serial, tid})
        end

        intercept_friends(tid, ts)
        store = r_tidstore(ts, :store)
        nodes = get_elements(:nodes, store)
        send(:mnesia_tm, {self(), {:restart, tid, store}})
        :mnesia_locker.send_release_tid(nodes, tid)
        :timer.sleep(sleepTime)
        :mnesia_locker.receive_release_tid_acc(nodes, tid)

        case get_restarted(tid) do
          {:restarted, ^tid} ->
            execute_transaction(fun, args, factor0 + 1, retries, type)

          {:error, reason} ->
            :mnesia.abort(reason)
        end
    end
  end

  defp get_restarted(tid) do
    case res = rec() do
      {:restarted, ^tid} ->
        res

      {:error, _} ->
        res

      _ ->
        get_restarted(tid)
    end
  end

  defp decr(:infinity) do
    :infinity
  end

  defp decr(x) when is_integer(x) and x > 1 do
    x - 1
  end

  defp decr(_X) do
    0
  end

  defp return_abort(fun, args, reason) do
    {_Mod, tid, ts} = :erlang.get(:mnesia_activity_state)
    dbg_out('Transaction ~p calling ~tp with ~tp failed: ~n ~tp~n', [tid, fun, args, reason])
    oldStore = r_tidstore(ts, :store)
    nodes = get_elements(:nodes, oldStore)
    intercept_friends(tid, ts)

    try do
      :mnesia_lib.incr_counter(:trans_failures)
    catch
      :error, _ ->
        :ok
    end

    level = r_tidstore(ts, :level)

    cond do
      level == 1 ->
        :mnesia_locker.async_release_tid(nodes, tid)

        try do
          send(:mnesia_tm, {:delete_transaction, tid})
        catch
          :error, _ ->
            :ok
        end

        :erlang.erase(:mnesia_activity_state)
        flush_downs()

        try do
          :erlang.unlink(:erlang.whereis(:mnesia_tm))
        catch
          :error, _ ->
            :ok
        end

        {:aborted, :mnesia_lib.fix_error(reason)}

      true ->
        [{oldMod, newStore} | tail] = r_tidstore(ts, :up_stores)
        req({:del_store, tid, newStore, oldStore, true})
        ts2 = r_tidstore(ts, store: newStore, up_stores: tail, level: level - 1)
        newTidTs = {oldMod, tid, ts2}
        :erlang.put(:mnesia_activity_state, newTidTs)

        case reason do
          r_cyclic() ->
            exit({:aborted, reason})

          {:node_not_running, _N} ->
            exit({:aborted, reason})

          {:bad_commit, _N} ->
            exit({:aborted, reason})

          _ ->
            {:aborted, :mnesia_lib.fix_error(reason)}
        end
    end
  end

  defp flush_downs() do
    receive do
      {:mnesia_tm, _, _} ->
        flush_downs()

      {:mnesia_down, _} ->
        flush_downs()
    after
      0 ->
        :flushed
    end
  end

  def put_activity_id(mTT) do
    put_activity_id(mTT, :undefined)
  end

  def put_activity_id(:undefined, _) do
    erase_activity_id()
  end

  def put_activity_id({mod, tid = r_tid(), ts = r_tidstore()}, fun) do
    flush_downs()
    store = r_tidstore(ts, :store)

    cond do
      is_function(fun) ->
        :ets.insert(store, {:friends, {:stop, fun}})

      true ->
        :ets.insert(store, {:friends, self()})
    end

    newTidTs = {mod, tid, ts}
    :erlang.put(:mnesia_activity_state, newTidTs)
  end

  def put_activity_id(simpleState, _) do
    :erlang.put(:mnesia_activity_state, simpleState)
  end

  defp erase_activity_id() do
    flush_downs()
    :erlang.erase(:mnesia_activity_state)
  end

  defp get_elements(type, store) do
    try do
      :ets.lookup(store, type)
    catch
      :error, _ ->
        []
    else
      [] ->
        []

      [{_, val}] ->
        [val]

      vals ->
        for {_, val} <- vals do
          val
        end
    end
  end

  defp opt_propagate_store(_Current, _Obsolete, false) do
    :ok
  end

  defp opt_propagate_store(current, obsolete, true) do
    propagate_store(current, :nodes, get_elements(:nodes, obsolete))
    propagate_store(current, :fixtable, get_elements(:fixtable, obsolete))
    propagate_store(current, :friends, get_elements(:friends, obsolete))
  end

  defp propagate_store(store, var, [val | vals]) do
    :ets.insert(store, {var, val})
    propagate_store(store, var, vals)
  end

  defp propagate_store(_Store, _Var, []) do
    :ok
  end

  defp intercept_friends(_Tid, ts) do
    friends = get_elements(:friends, r_tidstore(ts, :store))
    intercept_best_friend(friends, false)
  end

  defp intercept_best_friend([], _) do
    :ok
  end

  defp intercept_best_friend([{:stop, fun} | r], ignore) do
    try do
      fun.()
    catch
      _, _Reason ->
        {:EXIT, _Reason}
    end

    intercept_best_friend(r, ignore)
  end

  defp intercept_best_friend([pid | r], false) do
    send(pid, {:activity_ended, :undefined, self()})
    wait_for_best_friend(pid, 0)
    intercept_best_friend(r, true)
  end

  defp intercept_best_friend([_ | r], true) do
    intercept_best_friend(r, true)
  end

  defp wait_for_best_friend(pid, timeout) do
    receive do
      {:EXIT, ^pid, _} ->
        :ok

      {:activity_ended, _, ^pid} ->
        :ok
    after
      timeout ->
        case :erlang.is_process_alive(pid) do
          true ->
            wait_for_best_friend(pid, 1000)

          false ->
            :ok
        end
    end
  end

  def dirty(protocol, item) do
    {{tab, key}, _Val, _Op} = item
    tid = {:dirty, self()}
    prep = prepare_items(tid, tab, key, [item], r_prep(protocol: protocol))
    cR = r_prep(prep, :records)

    case protocol do
      :async_dirty ->
        readNode = val({tab, :where_to_read})
        {waitFor, firstRes} = async_send_dirty(tid, cR, tab, readNode)
        rec_dirty(waitFor, firstRes)

      :sync_dirty ->
        {waitFor, firstRes} = sync_send_dirty(tid, cR, tab, [])
        rec_dirty(waitFor, firstRes)

      _ ->
        :mnesia.abort({:bad_activity, protocol})
    end
  end

  defp t_commit(type) do
    {_Mod, tid, ts} = :erlang.get(:mnesia_activity_state)
    store = r_tidstore(ts, :store)

    cond do
      r_tidstore(ts, :level) == 1 ->
        intercept_friends(tid, ts)

        case arrange(tid, store, type) do
          {n, prep} when n > 0 ->
            multi_commit(
              r_prep(prep, :protocol),
              majority_attr(prep),
              tid,
              r_prep(prep, :records),
              store
            )

          {0, prep} ->
            multi_commit(:read_only, majority_attr(prep), tid, r_prep(prep, :records), store)
        end

      true ->
        level = r_tidstore(ts, :level)
        [{oldMod, obsolete} | tail] = r_tidstore(ts, :up_stores)
        req({:del_store, tid, store, obsolete, false})
        newTs = r_tidstore(ts, store: store, up_stores: tail, level: level - 1)
        newTidTs = {oldMod, tid, newTs}
        :erlang.put(:mnesia_activity_state, newTidTs)
        :do_commit_nested
    end
  end

  defp majority_attr(r_prep(majority: m)) do
    m
  end

  defp arrange(tid, store, type) do
    nodes = get_elements(:nodes, store)
    recs = prep_recs(nodes, [])
    key = :ets.first(store)
    n = 0

    prep =
      case type do
        :async ->
          r_prep(protocol: :sym_trans, records: recs)

        :sync ->
          r_prep(protocol: :sync_sym_trans, records: recs)
      end

    {new, prepared} = do_arrange(tid, store, key, prep, n)
    {new, r_prep(prepared, records: reverse(r_prep(prepared, :records)))}
  end

  defp reverse([]) do
    []
  end

  defp reverse([
         h = r_commit(ram_copies: ram, disc_copies: dC, disc_only_copies: dOC, ext: ext)
         | r
       ]) do
    [
      r_commit(h,
        ram_copies: :lists.reverse(ram),
        disc_copies: :lists.reverse(dC),
        disc_only_copies: :lists.reverse(dOC),
        ext:
          for {type, e} <- ext do
            {type, :lists.reverse(e)}
          end
      )
      | reverse(r)
    ]
  end

  defp prep_recs([n | nodes], recs) do
    prep_recs(
      nodes,
      [r_commit(decision: :presume_commit, node: n) | recs]
    )
  end

  defp prep_recs([], recs) do
    recs
  end

  defp do_arrange(tid, store, {tab, key}, prep, n) do
    oid = {tab, key}
    items = :ets.lookup(store, oid)
    p2 = prepare_items(tid, tab, key, items, prep)
    do_arrange(tid, store, :ets.next(store, oid), p2, n + 1)
  end

  defp do_arrange(tid, store, schemaKey, prep, n)
       when schemaKey == :op do
    items = :ets.lookup(store, schemaKey)
    p2 = prepare_schema_items(tid, items, prep)
    do_arrange(tid, store, :ets.next(store, schemaKey), p2, n + 1)
  end

  defp do_arrange(tid, store, restoreKey, prep, n)
       when restoreKey == :restore_op do
    [{:restore_op, r}] = :ets.lookup(store, restoreKey)

    fun = fn
      {tab, key}, commitRecs, _RecName, where, snmp ->
        item = [{{tab, key}, {tab, key}, :delete}]
        do_prepare_items(tid, tab, key, where, snmp, item, commitRecs)

      bupRec, commitRecs, recName, where, snmp ->
        tab = :erlang.element(1, bupRec)
        key = :erlang.element(2, bupRec)

        item =
          cond do
            tab == recName ->
              [{{tab, key}, bupRec, :write}]

            true ->
              bupRec2 = :erlang.setelement(1, bupRec, recName)
              [{{tab, key}, bupRec2, :write}]
          end

        do_prepare_items(tid, tab, key, where, snmp, item, commitRecs)
    end

    recs2 = :mnesia_schema.arrange_restore(r, fun, r_prep(prep, :records))
    p2 = r_prep(prep, protocol: :asym_trans, records: recs2)
    do_arrange(tid, store, :ets.next(store, restoreKey), p2, n + 1)
  end

  defp do_arrange(_Tid, _Store, :"$end_of_table", prep, n) do
    case prep do
      r_prep(sync: true, protocol: :asym_trans) ->
        {n, r_prep(prep, protocol: :sync_asym_trans)}

      _ ->
        {n, prep}
    end
  end

  defp do_arrange(tid, store, :sticky, prep, n) do
    p2 = r_prep(prep, sync: true)
    do_arrange(tid, store, :ets.next(store, :sticky), p2, n)
  end

  defp do_arrange(tid, store, ignoredKey, prep, n) do
    do_arrange(tid, store, :ets.next(store, ignoredKey), prep, n)
  end

  defp prepare_schema_items(tid, items, prep) do
    types =
      for n <- val({:current, :db_nodes}) do
        {n, :schema_ops}
      end

    recs = prepare_nodes(tid, types, items, r_prep(prep, :records), :schema)
    r_prep(prep, protocol: :asym_trans, records: recs)
  end

  defp prepare_items(tid, tab, key, items, prep)
       when r_prep(prep, :prev_tab) == tab do
    types = r_prep(prep, :prev_types)
    snmp = r_prep(prep, :prev_snmp)
    recs = r_prep(prep, :records)
    recs2 = do_prepare_items(tid, tab, key, types, snmp, items, recs)
    r_prep(prep, records: recs2)
  end

  defp prepare_items(tid, tab, key, items, prep) do
    types = val({tab, :where_to_commit})

    case types do
      [] ->
        :mnesia.abort({:no_exists, tab})

      {:blocked, _} ->
        :unblocked = req({:unblock_me, tab})
        prepare_items(tid, tab, key, items, prep)

      _ ->
        majority = needs_majority(tab, prep)
        snmp = val({tab, :snmp})
        recs2 = do_prepare_items(tid, tab, key, types, snmp, items, r_prep(prep, :records))

        prep2 =
          r_prep(prep,
            records: recs2,
            prev_tab: tab,
            majority: majority,
            prev_types: types,
            prev_snmp: snmp
          )

        check_prep(prep2, types)
    end
  end

  defp do_prepare_items(tid, tab, key, types, snmp, items, recs) do
    recs2 = prepare_snmp(tid, tab, key, types, snmp, items, recs)
    prepare_nodes(tid, types, items, recs2, :normal)
  end

  defp needs_majority(tab, r_prep(majority: m)) do
    case :lists.keymember(tab, 1, m) do
      true ->
        m

      false ->
        case (try do
                :ets.lookup_element(:mnesia_gvar, {tab, :majority}, 2)
              catch
                :error, _ ->
                  {:EXIT, {:badarg, []}}
              end) do
          {:EXIT, _} ->
            m

          false ->
            m

          true ->
            copyHolders = val({tab, :all_nodes})
            [{tab, copyHolders} | m]
        end
    end
  end

  defp have_majority([], _) do
    :ok
  end

  defp have_majority([{tab, allNodes} | rest], nodes) do
    case :mnesia_lib.have_majority(tab, allNodes, nodes) do
      true ->
        have_majority(rest, nodes)

      false ->
        {:error, tab}
    end
  end

  def prepare_snmp(tab, key, items) do
    case val({tab, :snmp}) do
      [] ->
        []

      ustruct when key != :_ ->
        {_Oid, _Val, op} = hd(items)
        snmpOid = :mnesia_snmp_hook.key_to_oid(tab, key, ustruct)
        [{op, tab, key, snmpOid}]

      _ ->
        [{:clear_table, tab}]
    end
  end

  defp prepare_snmp(_Tid, _Tab, _Key, _Types, [], _Items, recs) do
    recs
  end

  defp prepare_snmp(tid, tab, key, types, us, items, recs) do
    cond do
      key != :_ ->
        {_Oid, _Val, op} = hd(items)
        snmpOid = :mnesia_snmp_hook.key_to_oid(tab, key, us)
        prepare_nodes(tid, types, [{op, tab, key, snmpOid}], recs, :snmp)

      key == :_ ->
        prepare_nodes(tid, types, [{:clear_table, tab}], recs, :snmp)
    end
  end

  defp check_prep(r_prep(majority: [], types: types) = prep, types) do
    prep
  end

  defp check_prep(
         r_prep(majority: m, types: :undefined) = prep,
         types
       ) do
    protocol =
      cond do
        m == [] ->
          r_prep(prep, :protocol)

        true ->
          :asym_trans
      end

    r_prep(prep, protocol: protocol, types: types)
  end

  defp check_prep(prep, _Types) do
    r_prep(prep, protocol: :asym_trans)
  end

  defp prepare_nodes(tid, [{node, storage} | rest], items, c, kind) do
    {rec, c2} = pick_node(tid, node, c, [])
    rec2 = prepare_node(node, storage, items, rec, kind)
    [rec2 | prepare_nodes(tid, rest, items, c2, kind)]
  end

  defp prepare_nodes(_Tid, [], _Items, commitRecords, _Kind) do
    commitRecords
  end

  defp pick_node(tid, node, [rec | rest], done) do
    cond do
      r_commit(rec, :node) == node ->
        {rec, done ++ rest}

      true ->
        pick_node(tid, node, rest, [rec | done])
    end
  end

  defp pick_node({:dirty, _}, node, [], done) do
    {r_commit(decision: :presume_commit, node: node), done}
  end

  defp pick_node(_Tid, node, [], _Done) do
    :mnesia.abort({:bad_commit, {:missing_lock, node}})
  end

  defp prepare_node(node, storage, [item | items], r_commit(ext: ext0) = rec, kind)
       when kind == :snmp do
    rec2 =
      case :lists.keytake(:snmp, 1, ext0) do
        false ->
          r_commit(rec, ext: [{:snmp, [item]} | ext0])

        {_, {:snmp, snmp}, ext} ->
          r_commit(rec, ext: [{:snmp, [item | snmp]} | ext])
      end

    prepare_node(node, storage, items, rec2, kind)
  end

  defp prepare_node(node, storage, [item | items], rec, kind)
       when kind != :schema do
    rec2 =
      case storage do
        :ram_copies ->
          r_commit(rec, ram_copies: [item | r_commit(rec, :ram_copies)])

        :disc_copies ->
          r_commit(rec, disc_copies: [item | r_commit(rec, :disc_copies)])

        :disc_only_copies ->
          r_commit(rec,
            disc_only_copies: [
              item
              | r_commit(rec, :disc_only_copies)
            ]
          )

        {:ext, alias, mod} ->
          ext0 = r_commit(rec, :ext)

          case :lists.keytake(:ext_copies, 1, ext0) do
            false ->
              r_commit(rec,
                ext: [
                  {:ext_copies, [{{:ext, alias, mod}, item}]}
                  | ext0
                ]
              )

            {_, {_, eC}, ext} ->
              r_commit(rec,
                ext: [
                  {:ext_copies, [{{:ext, alias, mod}, item} | eC]}
                  | ext
                ]
              )
          end
      end

    prepare_node(node, storage, items, rec2, kind)
  end

  defp prepare_node(_Node, _Storage, items, rec, kind)
       when kind == :schema and r_commit(rec, :schema_ops) == [] do
    r_commit(rec, schema_ops: items)
  end

  defp prepare_node(_Node, _Storage, [], rec, _Kind) do
    rec
  end

  defp multi_commit(:read_only, _Maj = [], tid, cR, _Store) do
    {discNs, ramNs} = commit_nodes(cR, [], [])
    msg = {tid, :simple_commit}
    :rpc.abcast(discNs -- [node()], :mnesia_tm, msg)
    :rpc.abcast(ramNs -- [node()], :mnesia_tm, msg)
    :mnesia_recover.note_decision(tid, :committed)
    :mnesia_locker.release_tid(tid)
    send(:mnesia_tm, {:delete_transaction, tid})
    :do_commit
  end

  defp multi_commit(:sym_trans, _Maj = [], tid, cR, store) do
    {discNs, ramNs} = commit_nodes(cR, [], [])
    pending = :mnesia_checkpoint.tm_enter_pending(tid, discNs, ramNs)
    :ets.insert(store, pending)
    {waitFor, local} = ask_commit(:sym_trans, tid, cR, discNs, ramNs)
    {outcome, []} = rec_all(waitFor, tid, :do_commit, [])
    :ok
    :rpc.abcast(discNs -- [node()], :mnesia_tm, {tid, outcome})
    :rpc.abcast(ramNs -- [node()], :mnesia_tm, {tid, outcome})

    case outcome do
      :do_commit ->
        :mnesia_recover.note_decision(tid, :committed)
        do_dirty(tid, local)
        :mnesia_locker.release_tid(tid)
        send(:mnesia_tm, {:delete_transaction, tid})

      {:do_abort, _Reason} ->
        :mnesia_recover.note_decision(tid, :aborted)
    end

    :ok
    outcome
  end

  defp multi_commit(:sync_sym_trans, _Maj = [], tid, cR, store) do
    {discNs, ramNs} = commit_nodes(cR, [], [])
    pending = :mnesia_checkpoint.tm_enter_pending(tid, discNs, ramNs)
    :ets.insert(store, pending)
    {waitFor, local} = ask_commit(:sync_sym_trans, tid, cR, discNs, ramNs)
    {outcome, []} = rec_all(waitFor, tid, :do_commit, [])
    :ok

    for node <- waitFor do
      :ets.insert(store, {:waiting_for_commit_ack, node})
    end

    :rpc.abcast(discNs -- [node()], :mnesia_tm, {tid, outcome})
    :rpc.abcast(ramNs -- [node()], :mnesia_tm, {tid, outcome})

    case outcome do
      :do_commit ->
        :mnesia_recover.note_decision(tid, :committed)
        :mnesia_log.slog(local)
        do_commit(tid, local)
        rec_all(waitFor, tid, :ignore, [])
        :mnesia_locker.release_tid(tid)
        send(:mnesia_tm, {:delete_transaction, tid})

      {:do_abort, _Reason} ->
        :mnesia_recover.note_decision(tid, :aborted)
    end

    :ok
    outcome
  end

  defp multi_commit(protocol, majority, tid, cR, store)
       when protocol === :asym_trans or
              protocol === :sync_asym_trans do
    d = r_decision(tid: tid, outcome: :presume_abort)
    {d2, cR2} = commit_decision(d, cR, [], [])
    discNs = r_decision(d2, :disc_nodes)
    ramNs = r_decision(d2, :ram_nodes)

    case have_majority(majority, discNs ++ ramNs) do
      :ok ->
        :ok

      {:error, tab} ->
        :mnesia.abort({:no_majority, tab})
    end

    pending = :mnesia_checkpoint.tm_enter_pending(tid, discNs, ramNs)
    :ets.insert(store, pending)
    {waitFor, local} = ask_commit(protocol, tid, cR2, discNs, ramNs)

    schemaPrep =
      try do
        :mnesia_schema.prepare_commit(tid, local, {:coord, waitFor})
      catch
        _, _Reason ->
          {:EXIT, _Reason}
      end

    {votes, pids} = rec_all(waitFor, tid, :do_commit, [])
    :ok

    case votes do
      :do_commit ->
        case schemaPrep do
          {_Modified, c = r_commit(), dumperMode} ->
            :mnesia_log.log(c)
            :ok
            d3 = r_commit(c, :decision)
            d4 = r_decision(d3, outcome: :unclear)
            :mnesia_recover.log_decision(d4)
            :ok
            tell_participants(pids, {tid, :pre_commit})
            rec_acc_pre_commit(pids, tid, store, {c, local}, :do_commit, dumperMode, [], [])

          {:EXIT, reason} ->
            :mnesia_recover.note_decision(tid, :aborted)
            :ok
            tell_participants(pids, {tid, {:do_abort, reason}})
            do_abort(tid, local)
            {:do_abort, reason}
        end

      {:do_abort, reason} ->
        :mnesia_recover.note_decision(tid, :aborted)
        :ok
        tell_participants(pids, {tid, {:do_abort, reason}})
        do_abort(tid, local)
        {:do_abort, reason}
    end
  end

  defp rec_acc_pre_commit([pid | tail], tid, store, commit, res, dumperMode, goodPids, ackPids) do
    receive do
      {:mnesia_tm, _, {:acc_pre_commit, ^tid, ^pid, true}} ->
        rec_acc_pre_commit(tail, tid, store, commit, res, dumperMode, [pid | goodPids], [
          pid | ackPids
        ])

      {:mnesia_tm, _, {:acc_pre_commit, ^tid, ^pid, false}} ->
        rec_acc_pre_commit(tail, tid, store, commit, res, dumperMode, [pid | goodPids], ackPids)

      {:mnesia_tm, _, {:acc_pre_commit, ^tid, ^pid}} ->
        rec_acc_pre_commit(tail, tid, store, commit, res, dumperMode, [pid | goodPids], [
          pid | ackPids
        ])

      {:mnesia_tm, _, {:do_abort, ^tid, ^pid, _Reason}} ->
        abortRes = {:do_abort, {:bad_commit, node(pid)}}
        rec_acc_pre_commit(tail, tid, store, commit, abortRes, dumperMode, goodPids, ackPids)

      {:mnesia_down, node} when node == node(pid) ->
        abortRes = {:do_abort, {:bad_commit, node}}

        try do
          send(pid, {tid, abortRes})
        catch
          :error, _ ->
            :ok
        end

        rec_acc_pre_commit(tail, tid, store, commit, abortRes, dumperMode, goodPids, ackPids)
    end
  end

  defp rec_acc_pre_commit([], tid, store, {commit, origC}, res, dumperMode, goodPids, ackPids) do
    d = r_commit(commit, :decision)

    case res do
      :do_commit ->
        prepare_sync_schema_commit(store, ackPids)
        tell_participants(goodPids, {tid, :committed})
        d2 = r_decision(d, outcome: :committed)
        :mnesia_recover.log_decision(d2)
        :ok
        do_commit(tid, commit, dumperMode)
        :ok
        sync_schema_commit(tid, store, ackPids)
        :mnesia_locker.release_tid(tid)
        send(:mnesia_tm, {:delete_transaction, tid})

      {:do_abort, reason} ->
        tell_participants(goodPids, {tid, {:do_abort, reason}})
        d2 = r_decision(d, outcome: :aborted)
        :mnesia_recover.log_decision(d2)
        :ok
        do_abort(tid, origC)
        :ok
    end

    res
  end

  defp prepare_sync_schema_commit(_Store, []) do
    :ok
  end

  defp prepare_sync_schema_commit(store, [pid | pids]) do
    :ets.insert(store, {:waiting_for_commit_ack, node(pid)})
    prepare_sync_schema_commit(store, pids)
  end

  defp sync_schema_commit(_Tid, _Store, []) do
    :ok
  end

  defp sync_schema_commit(tid, store, [pid | tail]) do
    receive do
      {:mnesia_tm, _, {:schema_commit, ^tid, ^pid}} ->
        :ets.match_delete(
          store,
          {:waiting_for_commit_ack, node(pid)}
        )

        sync_schema_commit(tid, store, tail)

      {:mnesia_down, node} when node == node(pid) ->
        :ets.match_delete(
          store,
          {:waiting_for_commit_ack, node}
        )

        sync_schema_commit(tid, store, tail)
    end
  end

  defp tell_participants([pid | pids], msg) do
    send(pid, msg)
    tell_participants(pids, msg)
  end

  defp tell_participants([], _Msg) do
    :ok
  end

  def commit_participant(protocol, coord, tid, bin, discNs, ramNs)
      when is_binary(bin) do
    :erlang.process_flag(:trap_exit, true)
    commit = :erlang.binary_to_term(bin)
    commit_participant(protocol, coord, tid, bin, commit, discNs, ramNs)
  end

  def commit_participant(protocol, coord, tid, c = r_commit(), discNs, ramNs) do
    :erlang.process_flag(:trap_exit, true)
    commit_participant(protocol, coord, tid, c, c, discNs, ramNs)
  end

  defp commit_participant(protocol, coord, tid, bin, c0, discNs, _RamNs) do
    :ok

    try do
      :mnesia_schema.prepare_commit(tid, c0, {:part, coord})
    catch
      _, reason ->
        :ok
        reply(coord, {:vote_no, tid, reason})
        :mnesia_schema.undo_prepare_commit(tid, c0)
    else
      {modified, c = r_commit(), dumperMode} ->
        case :lists.member(node(), discNs) do
          false ->
            :ignore

          true ->
            case modified do
              false ->
                :mnesia_log.log(bin)

              true ->
                :mnesia_log.log(c)
            end
        end

        :ok
        reply(coord, {:vote_yes, tid, self()})

        receive do
          {^tid, :pre_commit} ->
            d = r_commit(c, :decision)
            :mnesia_recover.log_decision(r_decision(d, outcome: :unclear))
            :ok
            expectAck = r_commit(c, :schema_ops) != [] or protocol === :sync_asym_trans
            reply(coord, {:acc_pre_commit, tid, self(), expectAck})

            receive do
              {^tid, :committed} ->
                :mnesia_recover.log_decision(r_decision(d, outcome: :committed))
                :ok
                do_commit(tid, c, dumperMode)

                case expectAck do
                  false ->
                    :ignore

                  true ->
                    reply(coord, {:schema_commit, tid, self()})
                end

                :ok

              {^tid, {:do_abort, _Reason}} ->
                :mnesia_recover.log_decision(r_decision(d, outcome: :aborted))
                :ok
                :mnesia_schema.undo_prepare_commit(tid, c0)
                :ok

              {:EXIT, _MnesiaTM, reason} ->
                reply(
                  coord,
                  {:do_abort, tid, self(), {:bad_commit, reason}}
                )

                :mnesia_recover.log_decision(r_decision(d, outcome: :aborted))
                :mnesia_schema.undo_prepare_commit(tid, c0)

              msg ->
                verbose('** ERROR ** commit_participant ~p, got unexpected msg: ~tp~n', [tid, msg])
            end

          {^tid, {:do_abort, reason}} ->
            reply(coord, {:do_abort, tid, self(), reason})
            :mnesia_schema.undo_prepare_commit(tid, c0)
            :ok

          {:EXIT, _, reason} ->
            reply(
              coord,
              {:do_abort, tid, self(), {:bad_commit, reason}}
            )

            :mnesia_schema.undo_prepare_commit(tid, c0)
            :ok

          msg ->
            reply(
              coord,
              {:do_abort, tid, self(), {:bad_commit, :internal}}
            )

            verbose('** ERROR ** commit_participant ~p, got unexpected msg: ~tp~n', [tid, msg])
        end
    end

    :mnesia_locker.release_tid(tid)
    send(:mnesia_tm, {:delete_transaction, tid})
    :erlang.unlink(:erlang.whereis(:mnesia_tm))
    exit(:normal)
  end

  defp do_abort(tid, bin) when is_binary(bin) do
    do_abort(tid, :erlang.binary_to_term(bin))
  end

  defp do_abort(tid, commit) do
    :mnesia_schema.undo_prepare_commit(tid, commit)
    commit
  end

  defp do_dirty(tid, commit)
       when r_commit(commit, :schema_ops) == [] do
    :mnesia_log.log(commit)
    do_commit(tid, commit)
  end

  defp do_commit(tid, bin) when is_binary(bin) do
    do_commit(tid, :erlang.binary_to_term(bin))
  end

  defp do_commit(tid, c) do
    do_commit(tid, c, :optional)
  end

  defp do_commit(tid, bin, dumperMode) when is_binary(bin) do
    do_commit(tid, :erlang.binary_to_term(bin), dumperMode)
  end

  defp do_commit(tid, c, dumperMode) do
    :mnesia_dumper.update(tid, r_commit(c, :schema_ops), dumperMode)

    r =
      do_snmp(
        tid,
        :proplists.get_value(:snmp, r_commit(c, :ext), [])
      )

    r2 = do_update(tid, :ram_copies, r_commit(c, :ram_copies), r)
    r3 = do_update(tid, :disc_copies, r_commit(c, :disc_copies), r2)
    r4 = do_update(tid, :disc_only_copies, r_commit(c, :disc_only_copies), r3)
    r5 = do_update_ext(tid, r_commit(c, :ext), r4)
    :mnesia_subscr.report_activity(tid)
    r5
  end

  defp do_update_ext(_Tid, [], oldRes) do
    oldRes
  end

  defp do_update_ext(tid, ext, oldRes) do
    case :lists.keyfind(:ext_copies, 1, ext) do
      false ->
        oldRes

      {_, ops} ->
        do__ = fn {{:ext, _, _} = storage, op}, r ->
          do_update(tid, storage, [op], r)
        end

        :lists.foldl(do__, oldRes, ops)
    end
  end

  defp do_update(tid, storage, [op | ops], oldRes) do
    try do
      do_update_op(tid, storage, op)
    catch
      _, reason ->
        verbose('do_update in ~w failed: ~tp -> {\'EXIT\', ~tp}~n', [
          tid,
          op,
          {reason, __STACKTRACE__}
        ])

        do_update(tid, storage, ops, oldRes)
    else
      :ok ->
        do_update(tid, storage, ops, oldRes)

      newRes ->
        do_update(tid, storage, ops, newRes)
    end
  end

  defp do_update(_Tid, _Storage, [], res) do
    res
  end

  def do_update_op(tid, storage, {{tab, k}, obj, :write}) do
    commit_write(
      try do
        :ets.lookup_element(:mnesia_gvar, {tab, :commit_work}, 2)
      catch
        :error, _ ->
          {:EXIT, {:badarg, []}}
      end,
      tid,
      storage,
      tab,
      k,
      obj,
      :undefined
    )

    :mnesia_lib.db_put(storage, tab, obj)
  end

  def do_update_op(tid, storage, {{tab, k}, val, :delete}) do
    commit_delete(
      try do
        :ets.lookup_element(:mnesia_gvar, {tab, :commit_work}, 2)
      catch
        :error, _ ->
          {:EXIT, {:badarg, []}}
      end,
      tid,
      storage,
      tab,
      k,
      val,
      :undefined
    )

    :mnesia_lib.db_erase(storage, tab, k)
  end

  def do_update_op(tid, storage, {{tab, k}, {recName, incr}, :update_counter}) do
    {newObj, oldObjs} =
      try do
        newVal = :mnesia_lib.db_update_counter(storage, tab, k, incr)
        true = is_integer(newVal) and newVal >= 0
        {{recName, k, newVal}, [{recName, k, newVal - incr}]}
      catch
        :error, _ when incr > 0 ->
          new = {recName, k, incr}
          :mnesia_lib.db_put(storage, tab, new)
          {new, []}

        :error, _ ->
          zero = {recName, k, 0}
          :mnesia_lib.db_put(storage, tab, zero)
          {zero, []}
      end

    commit_update(
      try do
        :ets.lookup_element(:mnesia_gvar, {tab, :commit_work}, 2)
      catch
        :error, _ ->
          {:EXIT, {:badarg, []}}
      end,
      tid,
      storage,
      tab,
      k,
      newObj,
      oldObjs
    )

    :erlang.element(3, newObj)
  end

  def do_update_op(tid, storage, {{tab, key}, obj, :delete_object}) do
    commit_del_object(
      try do
        :ets.lookup_element(:mnesia_gvar, {tab, :commit_work}, 2)
      catch
        :error, _ ->
          {:EXIT, {:badarg, []}}
      end,
      tid,
      storage,
      tab,
      key,
      obj
    )

    :mnesia_lib.db_match_erase(storage, tab, obj)
  end

  def do_update_op(tid, storage, {{tab, key}, obj, :clear_table}) do
    commit_clear(
      try do
        :ets.lookup_element(:mnesia_gvar, {tab, :commit_work}, 2)
      catch
        :error, _ ->
          {:EXIT, {:badarg, []}}
      end,
      tid,
      storage,
      tab,
      key,
      obj
    )

    :mnesia_lib.db_match_erase(storage, tab, obj)
  end

  defp commit_write([], _, _, _, _, _, _) do
    :ok
  end

  defp commit_write([{:checkpoints, cpList} | r], tid, storage, tab, k, obj, old) do
    :mnesia_checkpoint.tm_retain(tid, tab, k, :write, cpList)
    commit_write(r, tid, storage, tab, k, obj, old)
  end

  defp commit_write([h | r], tid, storage, tab, k, obj, old)
       when :erlang.element(1, h) == :subscribers do
    :mnesia_subscr.report_table_event(h, tab, tid, obj, :write, old)
    commit_write(r, tid, storage, tab, k, obj, old)
  end

  defp commit_write([h | r], tid, storage, tab, k, obj, old)
       when :erlang.element(1, h) == :index do
    :mnesia_index.add_index(h, storage, tab, k, obj, old)
    commit_write(r, tid, storage, tab, k, obj, old)
  end

  defp commit_update([], _, _, _, _, _, _) do
    :ok
  end

  defp commit_update([{:checkpoints, cpList} | r], tid, storage, tab, k, obj, _) do
    old = :mnesia_checkpoint.tm_retain(tid, tab, k, :write, cpList)
    commit_update(r, tid, storage, tab, k, obj, old)
  end

  defp commit_update([h | r], tid, storage, tab, k, obj, old)
       when :erlang.element(1, h) == :subscribers do
    :mnesia_subscr.report_table_event(h, tab, tid, obj, :write, old)
    commit_update(r, tid, storage, tab, k, obj, old)
  end

  defp commit_update([h | r], tid, storage, tab, k, obj, old)
       when :erlang.element(1, h) == :index do
    :mnesia_index.add_index(h, storage, tab, k, obj, old)
    commit_update(r, tid, storage, tab, k, obj, old)
  end

  defp commit_delete([], _, _, _, _, _, _) do
    :ok
  end

  defp commit_delete([{:checkpoints, cpList} | r], tid, storage, tab, k, obj, _) do
    old = :mnesia_checkpoint.tm_retain(tid, tab, k, :delete, cpList)
    commit_delete(r, tid, storage, tab, k, obj, old)
  end

  defp commit_delete([h | r], tid, storage, tab, k, obj, old)
       when :erlang.element(1, h) == :subscribers do
    :mnesia_subscr.report_table_event(h, tab, tid, obj, :delete, old)
    commit_delete(r, tid, storage, tab, k, obj, old)
  end

  defp commit_delete([h | r], tid, storage, tab, k, obj, old)
       when :erlang.element(1, h) == :index do
    :mnesia_index.delete_index(h, storage, tab, k)
    commit_delete(r, tid, storage, tab, k, obj, old)
  end

  defp commit_del_object([], _, _, _, _, _) do
    :ok
  end

  defp commit_del_object([{:checkpoints, cpList} | r], tid, storage, tab, k, obj) do
    :mnesia_checkpoint.tm_retain(tid, tab, k, :delete_object, cpList)
    commit_del_object(r, tid, storage, tab, k, obj)
  end

  defp commit_del_object([h | r], tid, storage, tab, k, obj)
       when :erlang.element(1, h) == :subscribers do
    :mnesia_subscr.report_table_event(h, tab, tid, obj, :delete_object)
    commit_del_object(r, tid, storage, tab, k, obj)
  end

  defp commit_del_object([h | r], tid, storage, tab, k, obj)
       when :erlang.element(1, h) == :index do
    :mnesia_index.del_object_index(h, storage, tab, k, obj)
    commit_del_object(r, tid, storage, tab, k, obj)
  end

  defp commit_clear([], _, _, _, _, _) do
    :ok
  end

  defp commit_clear([{:checkpoints, cpList} | r], tid, storage, tab, k, obj) do
    :mnesia_checkpoint.tm_retain(tid, tab, k, :clear_table, cpList)
    commit_clear(r, tid, storage, tab, k, obj)
  end

  defp commit_clear([h | r], tid, storage, tab, k, obj)
       when :erlang.element(1, h) == :subscribers do
    :mnesia_subscr.report_table_event(h, tab, tid, obj, :clear_table, :undefined)
    commit_clear(r, tid, storage, tab, k, obj)
  end

  defp commit_clear([h | r], tid, storage, tab, k, obj)
       when :erlang.element(1, h) == :index do
    :mnesia_index.clear_index(h, tab, k, obj)
    commit_clear(r, tid, storage, tab, k, obj)
  end

  def do_snmp(_, []) do
    :ok
  end

  def do_snmp(tid, [head | tail]) do
    try do
      :mnesia_snmp_hook.update(head)
    catch
      _, reason ->
        verbose('do_snmp in ~w failed: ~tp -> {\'EXIT\', ~tp}~n', [
          tid,
          head,
          {reason, __STACKTRACE__}
        ])
    end

    do_snmp(tid, tail)
  end

  defp commit_nodes([c | tail], accD, accR) do
    case c do
      r_commit(disc_copies: [], disc_only_copies: [], schema_ops: [], ext: ext) ->
        case :lists.keyfind(:ext_copies, 1, ext) do
          false ->
            commit_nodes(tail, accD, [r_commit(c, :node) | accR])

          _ ->
            commit_nodes(tail, [r_commit(c, :node) | accD], accR)
        end

      _ ->
        commit_nodes(tail, [r_commit(c, :node) | accD], accR)
    end
  end

  defp commit_nodes([], accD, accR) do
    {accD, accR}
  end

  defp commit_decision(d, [c | tail], accD, accR) do
    n = r_commit(c, :node)

    {d2, tail2} =
      case c do
        r_commit(disc_copies: [], disc_only_copies: [], schema_ops: [], ext: ext) ->
          case :lists.keyfind(:ext_copies, 1, ext) do
            false ->
              commit_decision(d, tail, accD, [n | accR])

            _ ->
              commit_decision(d, tail, [n | accD], accR)
          end

        r_commit(schema_ops: []) ->
          commit_decision(d, tail, [n | accD], accR)

        r_commit(schema_ops: ops) ->
          case ram_only_ops(n, ops) do
            true ->
              commit_decision(d, tail, accD, [n | accR])

            false ->
              commit_decision(d, tail, [n | accD], accR)
          end
      end

    {d2, [r_commit(c, decision: d2) | tail2]}
  end

  defp commit_decision(d, [], accD, accR) do
    {r_decision(d, disc_nodes: accD, ram_nodes: accR), []}
  end

  defp ram_only_ops(
         n,
         [
           {:op, :change_table_copy_type, n, _FromS, _ToS, cs}
           | _Ops
         ]
       ) do
    case :lists.member({:name, :schema}, cs) do
      true ->
        false

      false ->
        not :lists.member(n, val({:schema, :disc_copies}))
    end
  end

  defp ram_only_ops(n, _Ops) do
    not :lists.member(n, val({:schema, :disc_copies}))
  end

  defp sync_send_dirty(tid, [head | tail], tab, waitFor) do
    node = r_commit(head, :node)

    cond do
      node == node() ->
        {wF, _} = sync_send_dirty(tid, tail, tab, waitFor)
        res = do_dirty(tid, head)
        {wF, res}

      true ->
        send({:mnesia_tm, node}, {self(), {:sync_dirty, tid, head, tab}})
        sync_send_dirty(tid, tail, tab, [node | waitFor])
    end
  end

  defp sync_send_dirty(_Tid, [], _Tab, waitFor) do
    {waitFor, {:EXIT, {:aborted, {:node_not_running, waitFor}}}}
  end

  defp async_send_dirty(_Tid, _Nodes, tab, :nowhere) do
    {[], {:EXIT, {:aborted, {:no_exists, tab}}}}
  end

  defp async_send_dirty(tid, nodes, tab, readNode) do
    async_send_dirty(tid, nodes, tab, readNode, [], :ok)
  end

  defp async_send_dirty(tid, [head | tail], tab, readNode, waitFor, res) do
    node = r_commit(head, :node)

    cond do
      readNode == node and node == node() ->
        newRes = do_dirty(tid, head)
        async_send_dirty(tid, tail, tab, readNode, waitFor, newRes)

      readNode == node ->
        send({:mnesia_tm, node}, {self(), {:sync_dirty, tid, head, tab}})
        newRes = {:EXIT, {:aborted, {:node_not_running, node}}}
        async_send_dirty(tid, tail, tab, readNode, [node | waitFor], newRes)

      true ->
        send({:mnesia_tm, node}, {self(), {:async_dirty, tid, head, tab}})
        async_send_dirty(tid, tail, tab, readNode, waitFor, res)
    end
  end

  defp async_send_dirty(_Tid, [], _Tab, _ReadNode, waitFor, res) do
    {waitFor, res}
  end

  defp rec_dirty([node | tail], res) when node != node() do
    newRes = get_dirty_reply(node, res)
    rec_dirty(tail, newRes)
  end

  defp rec_dirty([], res) do
    res
  end

  defp get_dirty_reply(node, res) do
    receive do
      {:mnesia_tm, ^node, {:EXIT, reason}} ->
        {:EXIT, {:aborted, {:badarg, reason}}}

      {:mnesia_tm, ^node, {:dirty_res, :ok}} ->
        case res do
          {:EXIT, {:aborted, {:node_not_running, _Node}}} ->
            :ok

          _ ->
            res
        end

      {:mnesia_tm, ^node, {:dirty_res, reply}} ->
        reply

      {:mnesia_down, ^node} ->
        case :erlang.get(:mnesia_activity_state) do
          {_, tid, _Ts} when :erlang.element(1, tid) == :tid ->
            :mnesia.abort({:node_not_running, node})

          _ ->
            res
        end
    after
      1000 ->
        case :lists.member(
               node,
               val({:current, :db_nodes})
             ) do
          true ->
            get_dirty_reply(node, res)

          false ->
            res
        end
    end
  end

  defp ask_commit(protocol, tid, cR, discNs, ramNs) do
    ask_commit(protocol, tid, cR, discNs, ramNs, [], :no_local)
  end

  defp ask_commit(protocol, tid, [head | tail], discNs, ramNs, waitFor, local) do
    node = r_commit(head, :node)

    cond do
      node == node() ->
        ask_commit(protocol, tid, tail, discNs, ramNs, waitFor, head)

      true ->
        msg = {:ask_commit, convert_old(protocol, node), tid, head, discNs, ramNs}
        send({:mnesia_tm, node}, {self(), msg})
        ask_commit(protocol, tid, tail, discNs, ramNs, [node | waitFor], local)
    end
  end

  defp ask_commit(_Protocol, _Tid, [], _DiscNs, _RamNs, waitFor, local) do
    {waitFor, local}
  end

  defp convert_old(:sync_asym_trans, node) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {:protocol, node}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {{8, 3}, _} ->
        :asym_trans

      _ ->
        :sync_asym_trans
    end
  end

  defp convert_old(protocol, _) do
    protocol
  end

  def new_cr_format(r_commit(ext: []) = cr) do
    cr
  end

  def new_cr_format(r_commit(ext: [{_, _} | _]) = cr) do
    cr
  end

  def new_cr_format(r_commit(ext: snmp) = cr) do
    r_commit(cr, ext: [{:snmp, snmp}])
  end

  defp rec_all([node | tail], tid, res, pids) do
    receive do
      {:mnesia_tm, ^node, {:vote_yes, ^tid}} ->
        rec_all(tail, tid, res, pids)

      {:mnesia_tm, ^node, {:vote_yes, ^tid, pid}} ->
        rec_all(tail, tid, res, [pid | pids])

      {:mnesia_tm, ^node, {:vote_no, ^tid, reason}} ->
        rec_all(tail, tid, {:do_abort, reason}, pids)

      {:mnesia_tm, ^node, {:committed, ^tid}} ->
        rec_all(tail, tid, res, pids)

      {:mnesia_tm, ^node, {:aborted, ^tid}} ->
        rec_all(tail, tid, res, pids)

      {:mnesia_down, ^node} ->
        abort = {:do_abort, {:bad_commit, node}}

        try do
          send({:mnesia_tm, node}, {tid, abort})
        catch
          :error, _ ->
            :ok
        end

        rec_all(tail, tid, abort, pids)
    end
  end

  defp rec_all([], _Tid, res, pids) do
    {res, pids}
  end

  def get_transactions() do
    {:info, participant, coordinator} = req(:info)

    :lists.map(
      fn {tid, _Tabs} ->
        status = tr_status(tid, participant)
        {r_tid(tid, :counter), r_tid(tid, :pid), status}
      end,
      coordinator
    )
  end

  defp tr_status(tid, participant) do
    case :lists.keymember(tid, 1, participant) do
      true ->
        :participant

      false ->
        :coordinator
    end
  end

  def get_info(timeout) do
    case :erlang.whereis(:mnesia_tm) do
      :undefined ->
        {:timeout, timeout}

      pid ->
        send(pid, {self(), :info})

        receive do
          {:mnesia_tm, _, {:info, part, coord}} ->
            {:info, part, coord}
        after
          timeout ->
            {:timeout, timeout}
        end
    end
  end

  def display_info(stream, {:timeout, t}) do
    :io.format(
      stream,
      '---> No info about coordinator and participant transactions, timeout ~p <--- ~n',
      [t]
    )
  end

  def display_info(stream, {:info, part, coord}) do
    :io.format(stream, '---> Participant transactions <--- ~n', [])

    :lists.foreach(
      fn p ->
        pr_participant(stream, p)
      end,
      part
    )

    :io.format(stream, '---> Coordinator transactions <---~n', [])

    :lists.foreach(
      fn {tid, _Tabs} ->
        pr_tid(stream, tid)
      end,
      coord
    )
  end

  defp pr_participant(stream, p) do
    commit0 = r_participant(p, :commit)

    commit =
      cond do
        is_binary(commit0) ->
          :erlang.binary_to_term(commit0)

        true ->
          commit0
      end

    pr_tid(stream, r_participant(p, :tid))
    :io.format(stream, 'with participant objects ~tp~n', [commit])
  end

  defp pr_tid(stream, tid) do
    :io.format(stream, 'Tid: ~p (owned by ~p) ~n', [r_tid(tid, :counter), r_tid(tid, :pid)])
  end

  def info(serial) do
    :io.format('Info about transaction with serial == ~p~n', [serial])
    {:info, participant, trs} = req(:info)
    search_pr_participant(serial, participant)
    search_pr_coordinator(serial, trs)
  end

  defp search_pr_coordinator(_S, []) do
    :no
  end

  defp search_pr_coordinator(s, [{tid, _Ts} | tail]) do
    case r_tid(tid, :counter) do
      ^s ->
        :io.format('Tid is coordinator, owner == \n', [])
        display_pid_info(r_tid(tid, :pid))
        search_pr_coordinator(s, tail)

      _ ->
        search_pr_coordinator(s, tail)
    end
  end

  defp search_pr_participant(_S, []) do
    false
  end

  defp search_pr_participant(s, [p | tail]) do
    tid = r_participant(p, :tid)
    commit0 = r_participant(p, :commit)

    cond do
      r_tid(tid, :counter) == s ->
        :io.format('Tid is participant to commit, owner == \n', [])
        pid = r_tid(tid, :pid)
        display_pid_info(pid)
        :io.format('Tid wants to write objects \n', [])

        commit =
          cond do
            is_binary(commit0) ->
              :erlang.binary_to_term(commit0)

            true ->
              commit0
          end

        :io.format('~tp~n', [commit])
        search_pr_participant(s, tail)

      true ->
        search_pr_participant(s, tail)
    end
  end

  defp display_pid_info(pid) do
    case :rpc.pinfo(pid) do
      :undefined ->
        :io.format('Dead process \n')

      info ->
        call = fetch(:initial_call, info)

        curr =
          case fetch(:current_function, info) do
            {mod, f, args} when is_list(args) ->
              {mod, f, length(args)}

            other ->
              other
          end

        reds = fetch(:reductions, info)
        lM = fetch(:message_queue_len, info)

        pformat(
          :io_lib.format('~p', [pid]),
          :io_lib.format('~tp', [call]),
          :io_lib.format('~tp', [curr]),
          reds,
          lM
        )
    end
  end

  defp pformat(a1, a2, a3, a4, a5) do
    :io.format('~-12s ~-21ts ~-21ts ~9w ~4w~n', [a1, a2, a3, a4, a5])
  end

  defp fetch(key, info) do
    case :lists.keysearch(key, 1, info) do
      {:value, {_, val}} ->
        val

      _ ->
        0
    end
  end

  defp reconfigure_coordinators(n, [{tid, [store | _]} | coordinators]) do
    case :mnesia_recover.outcome(tid, :unknown) do
      :committed ->
        waitingNodes =
          :ets.lookup(
            store,
            :waiting_for_commit_ack
          )

        case :lists.keymember(n, 2, waitingNodes) do
          false ->
            :ignore

          true ->
            send_mnesia_down(tid, store, n)
        end

      _ ->
        send_mnesia_down(tid, store, n)
    end

    reconfigure_coordinators(n, coordinators)
  end

  defp reconfigure_coordinators(_N, []) do
    :ok
  end

  defp send_mnesia_down(tid, store, node) do
    msg = {:mnesia_down, node}

    send_to_pids(
      [
        r_tid(tid, :pid)
        | get_elements(
            :friends,
            store
          )
      ],
      msg
    )
  end

  defp send_to_pids([pid | pids], msg) when is_pid(pid) do
    send(pid, msg)
    send_to_pids(pids, msg)
  end

  defp send_to_pids([_ | pids], msg) do
    send_to_pids(pids, msg)
  end

  defp send_to_pids([], _Msg) do
    :ok
  end

  defp reconfigure_participants(n, [p | tail]) do
    case :erlang.or(
           :lists.member(n, r_participant(p, :disc_nodes)),
           :lists.member(n, r_participant(p, :ram_nodes))
         ) do
      false ->
        reconfigure_participants(n, tail)

      true ->
        tid = r_participant(p, :tid)

        cond do
          node(r_tid(tid, :pid)) != n ->
            reconfigure_participants(n, tail)

          true ->
            verbose('Coordinator ~p in transaction ~p died~n', [r_tid(tid, :pid), tid])
            nodes = r_participant(p, :disc_nodes) ++ r_participant(p, :ram_nodes)
            aliveNodes = nodes -- [n]
            protocol = r_participant(p, :protocol)
            tell_outcome(tid, protocol, n, aliveNodes, aliveNodes)
            reconfigure_participants(n, tail)
        end
    end
  end

  defp reconfigure_participants(_, []) do
    []
  end

  defp tell_outcome(tid, protocol, node, checkNodes, tellNodes) do
    outcome = :mnesia_recover.what_happened(tid, proto(protocol), checkNodes)

    case outcome do
      :aborted ->
        :rpc.abcast(tellNodes, :mnesia_tm, {tid, {:do_abort, {:mnesia_down, node}}})

      :committed ->
        :rpc.abcast(tellNodes, :mnesia_tm, {tid, :do_commit})
    end

    outcome
  end

  defp proto(:sync_asym_trans) do
    :asym_trans
  end

  defp proto(proto) do
    proto
  end

  defp do_stop(r_state(coordinators: coordinators)) do
    msg = {:mnesia_down, node()}

    :lists.foreach(
      fn {tid, _} ->
        send(r_tid(tid, :pid), msg)
      end,
      :gb_trees.to_list(coordinators)
    )

    :mnesia_checkpoint.stop()
    :mnesia_log.stop()
    exit(:shutdown)
  end

  def fixtable(tab, lock, me) do
    case req({:fixtable, [tab, lock, me]}) do
      :error ->
        exit({:no_exists, tab})

      else__ ->
        else__
    end
  end

  def system_continue(_Parent, _Debug, state) do
    doit_loop(state)
  end

  def system_terminate(_Reason, _Parent, _Debug, state) do
    do_stop(state)
  end

  def system_code_change(
        state = r_state(coordinators: cs0, participants: ps0),
        _Module,
        _OldVsn,
        :downgrade
      ) do
    case is_tuple(cs0) do
      true ->
        cs = :gb_trees.to_list(cs0)
        ps = :gb_trees.values(ps0)
        {:ok, r_state(state, coordinators: cs, participants: ps)}

      false ->
        {:ok, state}
    end
  end

  def system_code_change(
        state = r_state(coordinators: cs0, participants: ps0),
        _Module,
        _OldVsn,
        _Extra
      ) do
    case is_list(cs0) do
      true ->
        cs = :gb_trees.from_orddict(:lists.sort(cs0))

        ps1 =
          for p <- ps0 do
            {r_participant(p, :tid), p}
          end

        ps = :gb_trees.from_orddict(:lists.sort(ps1))
        {:ok, r_state(state, coordinators: cs, participants: ps)}

      false ->
        {:ok, state}
    end
  end
end
