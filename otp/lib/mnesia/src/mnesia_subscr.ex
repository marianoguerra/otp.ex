defmodule :m_mnesia_subscr do
  use Bitwise
  import :mnesia_lib, only: [error: 2]
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
    pid_tab: :undefined
  )

  def start() do
    :gen_server.start_link({:local, :mnesia_subscr}, :mnesia_subscr, [self()], [
      {:timeout, :infinity}
    ])
  end

  def set_debug_level(level) do
    oldEnv = :application.get_env(:mnesia, :debug)

    case :mnesia_monitor.patch_env(:debug, level) do
      {:error, reason} ->
        {:error, reason}

      newLevel ->
        set_debug_level(newLevel, oldEnv)
    end
  end

  defp set_debug_level(level, oldEnv) do
    case :mnesia.system_info(:is_running) do
      :no when oldEnv == :undefined ->
        :none

      :no ->
        {:ok, e} = oldEnv
        e

      _ ->
        old = :mnesia_lib.val(:debug)
        local = :mnesia.system_info(:local_tables)
        e = :erlang.whereis(:mnesia_event)

        sub = fn tab ->
          subscribe(e, {:table, tab})
        end

        unSub = fn tab ->
          unsubscribe(e, {:table, tab})
        end

        case level do
          :none ->
            :lists.foreach(unSub, local)

          :verbose ->
            :lists.foreach(unSub, local)

          :debug ->
            :lists.foreach(unSub, local -- [:schema])
            sub.(:schema)

          :trace ->
            :lists.foreach(sub, local)
        end

        :mnesia_lib.set(:debug, level)
        old
    end
  end

  def subscribe(clientPid, :system) do
    change_subscr(:activate, clientPid, :system)
  end

  def subscribe(clientPid, :activity) do
    change_subscr(:activate, clientPid, :activity)
  end

  def subscribe(clientPid, {:table, tab}) do
    change_subscr(:activate, clientPid, {:table, tab, :simple})
  end

  def subscribe(clientPid, {:table, tab, :simple}) do
    change_subscr(:activate, clientPid, {:table, tab, :simple})
  end

  def subscribe(clientPid, {:table, tab, :detailed}) do
    change_subscr(:activate, clientPid, {:table, tab, :detailed})
  end

  def subscribe(_ClientPid, what) do
    {:error, {:badarg, what}}
  end

  def unsubscribe(clientPid, :system) do
    change_subscr(:deactivate, clientPid, :system)
  end

  def unsubscribe(clientPid, :activity) do
    change_subscr(:deactivate, clientPid, :activity)
  end

  def unsubscribe(clientPid, {:table, tab}) do
    change_subscr(:deactivate, clientPid, {:table, tab, :simple})
  end

  def unsubscribe(clientPid, {:table, tab, :simple}) do
    change_subscr(:deactivate, clientPid, {:table, tab, :simple})
  end

  def unsubscribe(clientPid, {:table, tab, :detailed}) do
    change_subscr(:deactivate, clientPid, {:table, tab, :detailed})
  end

  def unsubscribe(_ClientPid, what) do
    {:error, {:badarg, what}}
  end

  def unsubscribe_table(tab) do
    call({:change, {:deactivate_table, tab}})
  end

  defp change_subscr(kind, clientPid, what) do
    call({:change, {kind, clientPid, what}})
  end

  def subscribers() do
    [
      :erlang.whereis(:mnesia_event)
      | :mnesia_lib.val(:subscribers)
    ]
  end

  def report_activity({:dirty, _pid}) do
    :ok
  end

  def report_activity(tid) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, :activity_subscribers, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        :ok

      subscribers ->
        deliver(
          subscribers,
          {:mnesia_activity_event, {:complete, tid}}
        )
    end
  end

  def report_table_event(tab, tid, obj, op) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :commit_work}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        :ok

      commit ->
        case :lists.keysearch(:subscribers, 1, commit) do
          false ->
            :ok

          {:value, subs} ->
            report_table_event(subs, tab, tid, obj, op, :undefined)
        end
    end
  end

  def report_table_event(subscr, tab, tid, obj, op) do
    report_table_event(subscr, tab, tid, obj, op, :undefined)
  end

  def report_table_event({:subscribers, s1, s2}, tab, tid, _Obj, :clear_table, _Old) do
    what = {:delete, {:schema, tab}, tid}
    deliver(s1, {:mnesia_table_event, what})

    tabDef =
      :mnesia_schema.cs2list(
        try do
          :ets.lookup_element(:mnesia_gvar, {tab, :cstruct}, 2)
        catch
          :error, _ ->
            {:EXIT, {:badarg, []}}
        end
      )

    what2 = {:write, {:schema, tab, tabDef}, tid}
    deliver(s1, {:mnesia_table_event, what2})
    what3 = {:delete, :schema, {:schema, tab}, [{:schema, tab, tabDef}], tid}
    deliver(s2, {:mnesia_table_event, what3})
    what4 = {:write, :schema, {:schema, tab, tabDef}, [], tid}
    deliver(s2, {:mnesia_table_event, what4})
  end

  def report_table_event({:subscribers, subscr, []}, tab, tid, obj, op, _Old) do
    what = {op, patch_record(tab, obj), tid}
    deliver(subscr, {:mnesia_table_event, what})
  end

  def report_table_event({:subscribers, s1, s2}, tab, tid, obj, op, old) do
    standard = {op, patch_record(tab, obj), tid}
    deliver(s1, {:mnesia_table_event, standard})
    extended = what(tab, tid, obj, op, old)
    deliver(s2, extended)
  end

  def report_table_event({:subscribers, subscr}, tab, tid, obj, op, old) do
    report_table_event({:subscribers, subscr, []}, tab, tid, obj, op, old)
  end

  defp patch_record(tab, obj) do
    case tab == :erlang.element(1, obj) do
      true ->
        obj

      false ->
        :erlang.setelement(1, obj, tab)
    end
  end

  defp what(tab, tid, {recName, key}, :delete, :undefined) do
    try do
      :mnesia_lib.db_get(tab, key)
    catch
      :error, _ ->
        :ignore
    else
      old ->
        {:mnesia_table_event, {:delete, tab, {recName, key}, old, tid}}
    end
  end

  defp what(tab, tid, obj, :delete, old) do
    {:mnesia_table_event, {:delete, tab, obj, old, tid}}
  end

  defp what(tab, tid, obj, :delete_object, _Old) do
    {:mnesia_table_event, {:delete, tab, obj, [obj], tid}}
  end

  defp what(tab, tid, obj, :write, :undefined) do
    try do
      :mnesia_lib.db_get(tab, :erlang.element(2, obj))
    catch
      :error, _ ->
        :ignore
    else
      old ->
        {:mnesia_table_event, {:write, tab, obj, old, tid}}
    end
  end

  defp what(tab, tid, obj, :write, old) do
    {:mnesia_table_event, {:write, tab, obj, old, tid}}
  end

  defp deliver(_, :ignore) do
    :ok
  end

  defp deliver([pid | pids], msg) do
    send(pid, msg)
    deliver(pids, msg)
  end

  defp deliver([], _Msg) do
    :ok
  end

  defp call(msg) do
    pid = :erlang.whereis(:mnesia_subscr)

    case pid do
      :undefined ->
        {:error, {:node_not_running, node()}}

      ^pid ->
        res = :gen_server.call(pid, msg, :infinity)

        receive do
          {:EXIT, ^pid, _Reason} ->
            {:error, {:node_not_running, node()}}
        after
          0 ->
            res
        end
    end
  end

  def init([parent]) do
    :erlang.process_flag(:trap_exit, true)
    clientPid = :erlang.whereis(:mnesia_event)
    :erlang.link(clientPid)
    :mnesia_lib.verbose('~p starting: ~p~n', [:mnesia_subscr, self()])

    tab =
      _ =
      :ets.new(
        :mnesia_subscr,
        [:duplicate_bag, :private]
      )

    :ets.insert(tab, {clientPid, :system})
    {:ok, r_state(supervisor: parent, pid_tab: tab)}
  end

  def handle_call({:change, how}, _From, state) do
    reply = do_change(how, r_state(state, :pid_tab))
    {:reply, reply, state}
  end

  def handle_call(msg, _From, state) do
    :erlang.error('~p got unexpected call: ~tp~n', [:mnesia_subscr, msg])
    {:noreply, state}
  end

  def handle_cast(msg, state) do
    :erlang.error('~p got unexpected cast: ~tp~n', [:mnesia_subscr, msg])
    {:noreply, state}
  end

  def handle_info({:EXIT, pid, _R}, state)
      when pid == r_state(state, :supervisor) do
    {:stop, :shutdown, state}
  end

  def handle_info({:EXIT, pid, _Reason}, state) do
    handle_exit(pid, r_state(state, :pid_tab))
    {:noreply, state}
  end

  def handle_info(msg, state) do
    :erlang.error('~p got unexpected info: ~tp~n', [:mnesia_subscr, msg])
    {:noreply, state}
  end

  def terminate(reason, state) do
    prepare_stop(r_state(state, :pid_tab))
    :mnesia_monitor.terminate_proc(:mnesia_subscr, reason, state)
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp do_change({:activate, clientPid, :system}, subscrTab)
       when is_pid(clientPid) do
    var = :subscribers
    activate(clientPid, :system, var, subscribers(), subscrTab)
  end

  defp do_change({:activate, clientPid, :activity}, subscrTab)
       when is_pid(clientPid) do
    var = :activity_subscribers
    activate(clientPid, :activity, var, :mnesia_lib.val(var), subscrTab)
  end

  defp do_change(
         {:activate, clientPid, {:table, tab, how}},
         subscrTab
       )
       when is_pid(clientPid) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :where_to_read}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      node when node == node() ->
        var = {tab, :commit_work}
        activate(clientPid, {:table, tab, how}, var, :mnesia_lib.val(var), subscrTab)

      {:EXIT, _} ->
        {:error, {:no_exists, tab}}

      _Node ->
        {:error, {:not_active_local, tab}}
    end
  end

  defp do_change({:deactivate, clientPid, :system}, subscrTab) do
    var = :subscribers
    deactivate(clientPid, :system, var, subscrTab)
  end

  defp do_change(
         {:deactivate, clientPid, :activity},
         subscrTab
       ) do
    var = :activity_subscribers
    deactivate(clientPid, :activity, var, subscrTab)
  end

  defp do_change(
         {:deactivate, clientPid, {:table, tab, how}},
         subscrTab
       ) do
    var = {tab, :commit_work}
    deactivate(clientPid, {:table, tab, how}, var, subscrTab)
  end

  defp do_change({:deactivate_table, tab}, subscrTab) do
    var = {tab, :commit_work}

    case (try do
            :ets.lookup_element(:mnesia_gvar, var, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        {:error, {:no_exists, tab}}

      commitWork ->
        case :lists.keysearch(:subscribers, 1, commitWork) do
          false ->
            :ok

          {:value, subs} ->
            simple = {:table, tab, :simple}
            detailed = {:table, tab, :detailed}

            fs = fn c ->
              deactivate(c, simple, var, subscrTab)
            end

            fd = fn c ->
              deactivate(c, detailed, var, subscrTab)
            end

            case subs do
              {:subscribers, l1, l2} ->
                :lists.foreach(fs, l1)
                :lists.foreach(fd, l2)

              {:subscribers, l1} ->
                :lists.foreach(fs, l1)
            end
        end

        {:ok, node()}
    end
  end

  defp do_change(_, _) do
    {:error, :badarg}
  end

  defp activate(clientPid, what, var, oldSubscribers, subscrTab) do
    old =
      cond do
        var == :subscribers or var == :activity_subscribers ->
          oldSubscribers

        true ->
          case :lists.keysearch(:subscribers, 1, oldSubscribers) do
            false ->
              []

            {:value, subs} ->
              case subs do
                {:subscribers, l1, l2} ->
                  l1 ++ l2

                {:subscribers, l1} ->
                  l1
              end
          end
      end

    case :lists.member(clientPid, old) do
      false ->
        try do
          :erlang.link(clientPid)
        catch
          :error, _ ->
            {:error, {:no_exists, clientPid}}
        else
          true ->
            :ets.insert(subscrTab, {clientPid, what})
            add_subscr(var, what, clientPid)
            {:ok, node()}
        end

      true ->
        {:error, {:already_exists, what}}
    end
  end

  defp add_subscr(:subscribers, _What, pid) do
    :mnesia_lib.add(:subscribers, pid)
    {:ok, node()}
  end

  defp add_subscr(:activity_subscribers, _What, pid) do
    :mnesia_lib.add(:activity_subscribers, pid)
    {:ok, node()}
  end

  defp add_subscr({tab, :commit_work}, what, pid) do
    commit = :mnesia_lib.val({tab, :commit_work})

    case :lists.keysearch(:subscribers, 1, commit) do
      false ->
        subscr =
          case what do
            {:table, _, :simple} ->
              {:subscribers, [pid], []}

            {:table, _, :detailed} ->
              {:subscribers, [], [pid]}
          end

        :mnesia_lib.add({tab, :subscribers}, pid)

        :mnesia_lib.set(
          {tab, :commit_work},
          :mnesia_lib.sort_commit([subscr | commit])
        )

      {:value, old} ->
        {l1, l2} =
          case old do
            {:subscribers, l} ->
              {l, []}

            {:subscribers, sL1, sL2} ->
              {sL1, sL2}
          end

        subscr =
          case what do
            {:table, _, :simple} ->
              {:subscribers, [pid | l1], l2}

            {:table, _, :detailed} ->
              {:subscribers, l1, [pid | l2]}
          end

        newC = :lists.keyreplace(:subscribers, 1, commit, subscr)

        :mnesia_lib.set(
          {tab, :commit_work},
          :mnesia_lib.sort_commit(newC)
        )

        :mnesia_lib.add({tab, :subscribers}, pid)
    end
  end

  defp deactivate(clientPid, what, var, subscrTab) do
    :ets.match_delete(subscrTab, {clientPid, what})

    try do
      :ets.lookup_element(subscrTab, clientPid, 1)
      :ignore
    catch
      :error, _ ->
        :erlang.unlink(clientPid)
    end

    try do
      del_subscr(var, what, clientPid)
      {:ok, node()}
    catch
      _, _ ->
        {:error, :badarg}
    end
  end

  defp del_subscr(:subscribers, _What, pid) do
    :mnesia_lib.del(:subscribers, pid)
  end

  defp del_subscr(:activity_subscribers, _What, pid) do
    :mnesia_lib.del(:activity_subscribers, pid)
  end

  defp del_subscr({tab, :commit_work}, what, pid) do
    commit = :mnesia_lib.val({tab, :commit_work})

    case :lists.keysearch(:subscribers, 1, commit) do
      false ->
        false

      {:value, old} ->
        {l1, l2} =
          case old do
            {:subscribers, l} ->
              {l, []}

            {:subscribers, sL1, sL2} ->
              {sL1, sL2}
          end

        subscr =
          case what do
            {:table, _, :simple} ->
              newL1 = :lists.delete(pid, l1)
              newL2 = :lists.delete(pid, l2)
              {:subscribers, newL1, newL2}

            {:table, _, :detailed} ->
              newL1 = :lists.delete(pid, l1)
              newL2 = :lists.delete(pid, l2)
              {:subscribers, newL1, newL2}
          end

        case subscr do
          {:subscribers, [], []} ->
            newC = :lists.keydelete(:subscribers, 1, commit)
            :mnesia_lib.del({tab, :subscribers}, pid)

            :mnesia_lib.set(
              {tab, :commit_work},
              :mnesia_lib.sort_commit(newC)
            )

          _ ->
            newC = :lists.keyreplace(:subscribers, 1, commit, subscr)
            :mnesia_lib.del({tab, :subscribers}, pid)

            :mnesia_lib.set(
              {tab, :commit_work},
              :mnesia_lib.sort_commit(newC)
            )
        end
    end
  end

  defp handle_exit(clientPid, subscrTab) do
    do_handle_exit(:ets.lookup(subscrTab, clientPid))
    :ets.delete(subscrTab, clientPid)
  end

  defp do_handle_exit([{clientPid, what} | tail]) do
    case what do
      :system ->
        del_subscr(:subscribers, what, clientPid)

      :activity ->
        del_subscr(:activity_subscribers, what, clientPid)

      {_, tab, _Level} ->
        del_subscr({tab, :commit_work}, what, clientPid)
    end

    do_handle_exit(tail)
  end

  defp do_handle_exit([]) do
    :ok
  end

  defp prepare_stop(subscrTab) do
    :mnesia_lib.report_system_event({:mnesia_down, node()})
    do_prepare_stop(:ets.first(subscrTab), subscrTab)
  end

  defp do_prepare_stop(:"$end_of_table", _SubscrTab) do
    :ok
  end

  defp do_prepare_stop(clientPid, subscrTab) do
    next = :ets.next(subscrTab, clientPid)
    handle_exit(clientPid, subscrTab)
    :erlang.unlink(clientPid)
    do_prepare_stop(next, subscrTab)
  end
end
