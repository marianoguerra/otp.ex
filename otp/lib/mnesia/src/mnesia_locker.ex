defmodule :m_mnesia_locker do
  use Bitwise
  import :mnesia_lib, only: [dbg_out: 2, error: 2, verbose: 2]
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

  Record.defrecord(:r_state, :state, supervisor: :undefined)

  Record.defrecord(:r_queue, :queue,
    oid: :undefined,
    tid: :undefined,
    op: :undefined,
    pid: :undefined,
    lucky: :undefined
  )

  def start() do
    :mnesia_monitor.start_proc(:mnesia_locker, :mnesia_locker, :init, [self()])
  end

  def init(parent) do
    :erlang.register(:mnesia_locker, self())
    :erlang.process_flag(:trap_exit, true)

    _ =
      :ets.new(
        :mnesia_held_locks,
        [:ordered_set, :private, :named_table]
      )

    _ =
      :ets.new(
        :mnesia_tid_locks,
        [:ordered_set, :private, :named_table]
      )

    _ =
      :ets.new(
        :mnesia_sticky_locks,
        [:set, :private, :named_table]
      )

    _ =
      :ets.new(
        :mnesia_lock_queue,
        [:bag, :private, :named_table, {:keypos, 2}]
      )

    :proc_lib.init_ack(parent, {:ok, self()})

    case (try do
            :ets.lookup_element(:mnesia_gvar, :pid_sort_order, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      :r9b_plain ->
        :erlang.put(:pid_sort_order, :r9b_plain)

      :standard ->
        :erlang.put(:pid_sort_order, :standard)

      _ ->
        :ignore
    end

    loop(r_state(supervisor: parent))
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

  defp reply(from, r) do
    send(from, {:mnesia_locker, node(), r})
    true
  end

  defp l_request(node, x, store) do
    send({:mnesia_locker, node}, {self(), x})
    l_req_rec(node, store)
  end

  defp l_req_rec(node, store) do
    :ets.insert(store, {:nodes, node})

    receive do
      {:mnesia_locker, ^node, reply} ->
        reply

      {:mnesia_down, ^node} ->
        {:not_granted, {:node_not_running, node}}
    end
  end

  def release_tid(tid) do
    send(:mnesia_locker, {:release_tid, tid})
  end

  def async_release_tid(nodes, tid) do
    :rpc.abcast(nodes, :mnesia_locker, {:release_tid, tid})
  end

  def send_release_tid(nodes, tid) do
    :rpc.abcast(nodes, :mnesia_locker, {self(), {:sync_release_tid, tid}})
  end

  def receive_release_tid_acc([node | nodes], tid) do
    receive do
      {:mnesia_locker, ^node, {:tid_released, ^tid}} ->
        receive_release_tid_acc(nodes, tid)
    after
      0 ->
        receive do
          {:mnesia_locker, ^node, {:tid_released, ^tid}} ->
            receive_release_tid_acc(nodes, tid)

          {:mnesia_down, ^node} ->
            receive_release_tid_acc(nodes, tid)
        end
    end
  end

  def receive_release_tid_acc([], _Tid) do
    :ok
  end

  def mnesia_down(node, pending) do
    case :erlang.whereis(:mnesia_locker) do
      :undefined ->
        {:error, :node_not_running}

      pid ->
        ref = make_ref()
        send(pid, {{self(), ref}, {:release_remote_non_pending, node, pending}})

        receive do
          {^ref, :ok} ->
            :ok
        end
    end
  end

  defp loop(state) do
    receive do
      {from, {:write, tid, oid}} ->
        try_sticky_lock(tid, :write, from, oid)
        loop(state)

      {from, {:read, tid, oid}} ->
        try_sticky_lock(tid, :read, from, oid)
        loop(state)

      {from, {:read_write, tid, oid}} ->
        try_sticky_lock(tid, :read_write, from, oid)
        loop(state)

      {:release_tid, tid} ->
        do_release_tid(tid)
        loop(state)

      {from, {:test_set_sticky, tid, {tab, _} = oid, lock}} ->
        case :ets.lookup(:mnesia_sticky_locks, tab) do
          [] ->
            reply(from, :not_stuck)
            loop(state)

          [{_, node}] when node == node() ->
            try_lock(tid, lock, from, oid)
            loop(state)

          [{_, node}] ->
            reply(from, {:stuck_elsewhere, node})
            loop(state)
        end

      {:stick, {tab, _}, n} ->
        :ets.insert(:mnesia_sticky_locks, {tab, n})
        loop(state)

      {:unstick, tab} ->
        :ets.delete(:mnesia_sticky_locks, tab)
        loop(state)

      {from, {:ix_read, tid, tab, ixKey, pos}} ->
        case :ets.lookup(:mnesia_sticky_locks, tab) do
          [] ->
            set_read_lock_on_all_keys(tid, from, tab, ixKey, pos)
            loop(state)

          [{_, n}] when n == node() ->
            set_read_lock_on_all_keys(tid, from, tab, ixKey, pos)
            loop(state)

          [{_, n}] ->
            req = {from, {:ix_read, tid, tab, ixKey, pos}}
            send(from, {:mnesia_locker, node(), {:switch, n, req}})
            loop(state)
        end

      {from, {:sync_release_tid, tid}} ->
        do_release_tid(tid)
        reply(from, {:tid_released, tid})
        loop(state)

      {{from, ref}, {:release_remote_non_pending, node, pending}} ->
        release_remote_non_pending(node, pending)
        send(from, {ref, :ok})
        loop(state)

      {from, {:is_locked, oid}} ->
        held = :ets.lookup(:mnesia_held_locks, oid)
        reply(from, held)
        loop(state)

      {:EXIT, pid, _} when pid == r_state(state, :supervisor) ->
        do_stop()

      {:system, from, msg} ->
        verbose('~p got {system, ~p, ~tp}~n', [:mnesia_locker, from, msg])
        parent = r_state(state, :supervisor)
        :sys.handle_system_msg(msg, from, parent, :mnesia_locker, [], state)

      {:get_table, from, lockTable} ->
        send(from, {lockTable, :ets.match_object(lockTable, :_)})
        loop(state)

      msg ->
        :erlang.error('~p got unexpected message: ~tp~n', [:mnesia_locker, msg])
        loop(state)
    end
  end

  defp set_lock(tid, oid, op, []) do
    :ets.insert(:mnesia_tid_locks, {{tid, oid, op}})
    :ets.insert(:mnesia_held_locks, {oid, op, [{op, tid}]})
  end

  defp set_lock(tid, oid, :read, [{oid, prev, items}]) do
    :ets.insert(:mnesia_tid_locks, {{tid, oid, :read}})

    :ets.insert(
      :mnesia_held_locks,
      {oid, prev, [{:read, tid} | items]}
    )
  end

  defp set_lock(tid, oid, :write, [{oid, _Prev, items}]) do
    :ets.insert(:mnesia_tid_locks, {{tid, oid, :write}})

    :ets.insert(
      :mnesia_held_locks,
      {oid, :write, [{:write, tid} | items]}
    )
  end

  defp set_lock(tid, oid, op, :undefined) do
    set_lock(tid, oid, op, :ets.lookup(:mnesia_held_locks, oid))
  end

  defp try_sticky_lock(tid, op, pid, {tab, _} = oid) do
    case :ets.lookup(:mnesia_sticky_locks, tab) do
      [] ->
        try_lock(tid, op, pid, oid)

      [{_, n}] when n == node() ->
        try_lock(tid, op, pid, oid)

      [{_, n}] ->
        req = {pid, {op, tid, oid}}
        send(pid, {:mnesia_locker, node(), {:switch, n, req}})
        true
    end
  end

  defp try_lock(tid, :read_write, pid, oid) do
    try_lock(tid, :read_write, :read, :write, pid, oid)
  end

  defp try_lock(tid, op, pid, oid) do
    try_lock(tid, op, op, op, pid, oid)
  end

  defp try_lock(tid, op, simpleOp, lock, pid, oid) do
    case can_lock(tid, lock, oid, {:no, :bad_luck}) do
      {:yes, default} ->
        reply = grant_lock(tid, simpleOp, lock, oid, default)
        reply(pid, reply)

      {{:no, lucky}, _} ->
        c = r_cyclic(op: simpleOp, lock: lock, oid: oid, lucky: lucky)
        :ok
        reply(pid, {:not_granted, c})

      {{:queue, lucky}, _} ->
        :ok

        :ets.insert(
          :mnesia_lock_queue,
          r_queue(oid: oid, tid: tid, op: op, pid: pid, lucky: lucky)
        )

        :ets.insert(
          :mnesia_tid_locks,
          {{tid, oid, {:queued, op}}}
        )
    end
  end

  defp grant_lock(tid, :read, lock, oid = {tab, key}, default)
       when key != :______WHOLETABLE_____ and
              tab != :______GLOBAL_____ do
    case node(r_tid(tid, :pid)) == node() do
      true ->
        set_lock(tid, oid, lock, default)
        {:granted, :lookup_in_client}

      false ->
        try do
          val = :mnesia_lib.db_get(tab, key)
          set_lock(tid, oid, lock, default)
          {:granted, val}
        catch
          _, _Reason ->
            c = r_cyclic(op: :read, lock: lock, oid: oid, lucky: :nowhere)
            {:not_granted, c}
        end
    end
  end

  defp grant_lock(tid, {:ix_read, ixKey, pos}, lock, oid = {tab, _}, default) do
    try do
      res = ix_read_res(tab, ixKey, pos)
      set_lock(tid, oid, lock, default)
      {:granted, res, [:______WHOLETABLE_____]}
    catch
      _, _ ->
        {:not_granted, {:no_exists, tab, {:index, [pos]}}}
    end
  end

  defp grant_lock(tid, :read, lock, oid, default) do
    set_lock(tid, oid, lock, default)
    {:granted, :ok}
  end

  defp grant_lock(tid, :write, lock, oid, default) do
    set_lock(tid, oid, lock, default)
    :granted
  end

  defp can_lock(tid, :read, oid = {tab, key}, alreadyQ)
       when key != :______WHOLETABLE_____ do
    objLocks = :ets.lookup(:mnesia_held_locks, oid)

    tabLocks =
      :ets.lookup(
        :mnesia_held_locks,
        {tab, :______WHOLETABLE_____}
      )

    {check_lock(tid, oid, filter_write(objLocks), filter_write(tabLocks), :yes, alreadyQ, :read),
     objLocks}
  end

  defp can_lock(tid, :read, oid, alreadyQ) do
    tab = :erlang.element(1, oid)

    objLocks =
      :ets.match_object(
        :mnesia_held_locks,
        {{tab, :_}, :write, :_}
      )

    {check_lock(tid, oid, objLocks, [], :yes, alreadyQ, :read), :undefined}
  end

  defp can_lock(tid, :write, oid = {tab, key}, alreadyQ)
       when key != :______WHOLETABLE_____ do
    objLocks = :ets.lookup(:mnesia_held_locks, oid)

    tabLocks =
      :ets.lookup(
        :mnesia_held_locks,
        {tab, :______WHOLETABLE_____}
      )

    {check_lock(tid, oid, objLocks, tabLocks, :yes, alreadyQ, :write), objLocks}
  end

  defp can_lock(tid, :write, oid, alreadyQ) do
    tab = :erlang.element(1, oid)

    objLocks =
      :ets.match_object(
        :mnesia_held_locks,
        {{tab, :_}, :_, :_}
      )

    {check_lock(tid, oid, objLocks, [], :yes, alreadyQ, :write), :undefined}
  end

  defp filter_write([{_, :read, _}]) do
    []
  end

  defp filter_write(res) do
    res
  end

  defp check_lock(tid, oid, [{_, _, lock} | locks], tabLocks, _X, alreadyQ, type) do
    case can_queue(lock, tid, oid, _X) do
      {:no, _} = res ->
        res

      res ->
        check_lock(tid, oid, locks, tabLocks, res, alreadyQ, type)
    end
  end

  defp check_lock(_, _, [], [], x, {:queue, :bad_luck}, _) do
    x
  end

  defp check_lock(_, _, [], [], x = {:queue, _Tid}, _AlreadyQ, _) do
    x
  end

  defp check_lock(tid, oid = {tab, key}, [], [], x, alreadyQ, type) do
    cond do
      type == :write ->
        check_queue(tid, tab, x, alreadyQ)

      key == :______WHOLETABLE_____ ->
        check_queue(tid, tab, x, alreadyQ)

      true ->
        objLocks = :ets.lookup(:mnesia_lock_queue, oid)

        case max(objLocks) do
          :empty ->
            check_queue(tid, tab, x, alreadyQ)

          objL ->
            case allowed_to_be_queued(objL, tid) do
              false ->
                {:no, objL}

              true ->
                check_queue(tid, tab, {:queue, objL}, alreadyQ)
            end
        end
    end
  end

  defp check_lock(tid, oid, [], tabLocks, x, alreadyQ, type) do
    check_lock(tid, oid, tabLocks, [], x, alreadyQ, type)
  end

  defp can_queue([{_Op, tid} | locks], tid, oid, res) do
    can_queue(locks, tid, oid, res)
  end

  defp can_queue([{op, waitForTid} | locks], tid, oid = {tab, _}, _) do
    case allowed_to_be_queued(waitForTid, tid) do
      true when r_tid(tid, :pid) == r_tid(waitForTid, :pid) ->
        dbg_out('Spurious lock conflict ~w ~w: ~w -> ~w~n', [oid, op, tid, waitForTid])

        haveQ =
          :ets.lookup(
            :mnesia_lock_queue,
            oid
          ) != [] or
            :ets.lookup(
              :mnesia_lock_queue,
              {tab, :______WHOLETABLE_____}
            ) != []

        case haveQ do
          true ->
            {:no, waitForTid}

          false ->
            can_queue(locks, tid, oid, {:queue, waitForTid})
        end

      true ->
        can_queue(locks, tid, oid, {:queue, waitForTid})

      false ->
        {:no, waitForTid}
    end
  end

  defp can_queue([], _, _, res) do
    res
  end

  defp allowed_to_be_queued(waitForTid, tid) do
    case :erlang.get(:pid_sort_order) do
      :undefined ->
        waitForTid > tid

      :r9b_plain ->
        cmp_tid(true, waitForTid, tid) === 1

      :standard ->
        cmp_tid(false, waitForTid, tid) === 1
    end
  end

  defp check_queue(tid, tab, x, alreadyQ) do
    tabLocks =
      :ets.lookup(
        :mnesia_lock_queue,
        {tab, :______WHOLETABLE_____}
      )

    greatest = max(tabLocks)

    case greatest do
      :empty ->
        x

      ^tid ->
        x

      waitForTid ->
        case allowed_to_be_queued(waitForTid, tid) do
          true ->
            {:queue, waitForTid}

          false when alreadyQ === {:no, :bad_luck} ->
            {:no, waitForTid}
        end
    end
  end

  defp sort_queue(qL) do
    case :erlang.get(:pid_sort_order) do
      :undefined ->
        :lists.reverse(:lists.keysort(r_queue(:tid), qL))

      :r9b_plain ->
        :lists.sort(
          fn r_queue(tid: x), r_queue(tid: y) ->
            cmp_tid(true, x, y) == 1
          end,
          qL
        )

      :standard ->
        :lists.sort(
          fn r_queue(tid: x), r_queue(tid: y) ->
            cmp_tid(false, x, y) == 1
          end,
          qL
        )
    end
  end

  defp max([]) do
    :empty
  end

  defp max([r_queue(tid: max)]) do
    max
  end

  defp max(l) do
    [r_queue(tid: max) | _] = sort_queue(l)
    max
  end

  defp set_read_lock_on_all_keys(tid, from, tab, ixKey, pos) do
    oid = {tab, :______WHOLETABLE_____}
    op = {:ix_read, ixKey, pos}
    lock = :read

    case can_lock(tid, lock, oid, {:no, :bad_luck}) do
      {:yes, default} ->
        reply = grant_lock(tid, op, lock, oid, default)
        reply(from, reply)

      {{:no, lucky}, _} ->
        c = r_cyclic(op: op, lock: lock, oid: oid, lucky: lucky)
        :ok
        reply(from, {:not_granted, c})

      {{:queue, lucky}, _} ->
        :ok

        :ets.insert(
          :mnesia_lock_queue,
          r_queue(oid: oid, tid: tid, op: op, pid: from, lucky: lucky)
        )

        :ets.insert(
          :mnesia_tid_locks,
          {{tid, oid, {:queued, op}}}
        )
    end
  end

  defp release_remote_non_pending(node, pending) do
    :ets.match_delete(:mnesia_sticky_locks, {:_, node})
    allTids0 = :ets.match(:mnesia_tid_locks, {{:"$1", :_, :_}})
    allTids = :lists.usort(allTids0)

    tids =
      for [t] <- allTids, node == node(r_tid(t, :pid)), not :lists.member(t, pending) do
        t
      end

    do_release_tids(tids)
  end

  defp do_release_tids([tid | tids]) do
    do_release_tid(tid)
    do_release_tids(tids)
  end

  defp do_release_tids([]) do
    :ok
  end

  defp do_release_tid(tid) do
    objects =
      :ets.select(
        :mnesia_tid_locks,
        [{{{tid, :_, :_}}, [], [:"$_"]}]
      )

    locks =
      :lists.map(
        fn {l} ->
          l
        end,
        objects
      )

    :ok

    for l <- locks do
      :ets.delete(:mnesia_tid_locks, l)
    end

    release_locks(locks)
    uniqueLocks = keyunique(:lists.sort(locks), [])
    rearrange_queue(uniqueLocks)
  end

  defp keyunique(
         [{_Tid, oid, _Op} | r],
         acc = [{_, oid, _} | _]
       ) do
    keyunique(r, acc)
  end

  defp keyunique([h | r], acc) do
    keyunique(r, [h | acc])
  end

  defp keyunique([], acc) do
    acc
  end

  defp release_locks([lock | locks]) do
    release_lock(lock)
    release_locks(locks)
  end

  defp release_locks([]) do
    :ok
  end

  defp release_lock({tid, oid, {:queued, _}}) do
    :ets.match_delete(
      :mnesia_lock_queue,
      r_queue(oid: oid, tid: tid, op: :_, pid: :_, lucky: :_)
    )
  end

  defp release_lock({_Tid, oid, :write}) do
    :ets.delete(:mnesia_held_locks, oid)
  end

  defp release_lock({tid, oid, :read}) do
    case :ets.lookup(:mnesia_held_locks, oid) do
      [{^oid, prev, locks0}] ->
        case remove_tid(locks0, tid, []) do
          [] ->
            :ets.delete(:mnesia_held_locks, oid)

          locks ->
            :ets.insert(:mnesia_held_locks, {oid, prev, locks})
        end

      [] ->
        :ok
    end
  end

  defp remove_tid([{_Op, tid} | ls], tid, acc) do
    remove_tid(ls, tid, acc)
  end

  defp remove_tid([keep | ls], tid, acc) do
    remove_tid(ls, tid, [keep | acc])
  end

  defp remove_tid([], _, acc) do
    acc
  end

  defp rearrange_queue([{_Tid, {tab, key}, _} | locks]) do
    cond do
      key != :______WHOLETABLE_____ ->
        queue =
          :ets.lookup(
            :mnesia_lock_queue,
            {tab, :______WHOLETABLE_____}
          ) ++
            :ets.lookup(
              :mnesia_lock_queue,
              {tab, key}
            )

        case queue do
          [] ->
            :ok

          _ ->
            sorted = sort_queue(queue)
            try_waiters_obj(sorted)
        end

      true ->
        pat = r_queue(oid: {tab, :_}, tid: :_, op: :_, pid: :_, lucky: :_)
        queue = :ets.match_object(:mnesia_lock_queue, pat)
        sorted = sort_queue(queue)
        try_waiters_tab(sorted)
    end

    :ok
    rearrange_queue(locks)
  end

  defp rearrange_queue([]) do
    :ok
  end

  defp try_waiters_obj([w | waiters]) do
    case try_waiter(w) do
      :queued ->
        :no

      _ ->
        try_waiters_obj(waiters)
    end
  end

  defp try_waiters_obj([]) do
    :ok
  end

  defp try_waiters_tab([w | waiters]) do
    case r_queue(w, :oid) do
      {_Tab, :______WHOLETABLE_____} ->
        case try_waiter(w) do
          :queued ->
            :no

          _ ->
            try_waiters_tab(waiters)
        end

      oid ->
        case try_waiter(w) do
          :queued ->
            rest = key_delete_all(oid, r_queue(:oid), waiters)
            try_waiters_tab(rest)

          _ ->
            try_waiters_tab(waiters)
        end
    end
  end

  defp try_waiters_tab([]) do
    :ok
  end

  defp try_waiter({:queue, oid, tid, :read_write, replyTo, _}) do
    try_waiter(oid, :read_write, :read, :write, replyTo, tid)
  end

  defp try_waiter({:queue, oid, tid, iXR = {:ix_read, _, _}, replyTo, _}) do
    try_waiter(oid, iXR, iXR, :read, replyTo, tid)
  end

  defp try_waiter({:queue, oid, tid, op, replyTo, _}) do
    try_waiter(oid, op, op, op, replyTo, tid)
  end

  defp try_waiter(oid, op, simpleOp, lock, replyTo, tid) do
    case can_lock(tid, lock, oid, {:queue, :bad_luck}) do
      {:yes, default} ->
        :ets.match_delete(
          :mnesia_lock_queue,
          r_queue(oid: oid, tid: tid, op: op, pid: replyTo, lucky: :_)
        )

        reply = grant_lock(tid, simpleOp, lock, oid, default)
        reply(replyTo, reply)
        :locked

      {{:queue, _Why}, _} ->
        :ok
        :queued

      {{:no, lucky}, _} ->
        c = r_cyclic(op: simpleOp, lock: lock, oid: oid, lucky: lucky)

        verbose(
          '** WARNING ** Restarted transaction, possible deadlock in lock queue ~w: cyclic = ~w~n',
          [tid, c]
        )

        :ets.match_delete(
          :mnesia_lock_queue,
          r_queue(oid: oid, tid: tid, op: op, pid: replyTo, lucky: :_)
        )

        reply = {:not_granted, c}
        reply(replyTo, reply)
        :removed
    end
  end

  defp key_delete_all(key, pos, tupleList) do
    key_delete_all(key, pos, tupleList, [])
  end

  defp key_delete_all(key, pos, [h | t], ack)
       when :erlang.element(pos, h) == key do
    key_delete_all(key, pos, t, ack)
  end

  defp key_delete_all(key, pos, [h | t], ack) do
    key_delete_all(key, pos, t, [h | ack])
  end

  defp key_delete_all(_, _, [], ack) do
    :lists.reverse(ack)
  end

  defp ix_read_res(tab, ixKey, pos) do
    index = :mnesia_index.get_index_table(tab, pos)

    rks =
      :mnesia_lib.elems(
        2,
        :mnesia_index.db_get(index, ixKey)
      )

    :lists.append(
      :lists.map(
        fn real ->
          :mnesia_lib.db_get(tab, real)
        end,
        rks
      )
    )
  end

  def rwlock(tid, store, oid) do
    {tab, key} = oid

    case val({tab, :where_to_read}) do
      :nowhere ->
        :mnesia.abort({:no_exists, tab})

      node ->
        lock = :write

        case need_lock(store, tab, key, lock) do
          :yes ->
            {ns0, majority} = w_nodes(tab)
            ns = [node | :lists.delete(node, ns0)]
            check_majority(majority, tab, ns)
            res = get_rwlocks_on_nodes(ns, make_ref(), store, tid, oid)
            :ets.insert(store, {{:locks, tab, key}, lock})
            res

          :no ->
            cond do
              key == :______WHOLETABLE_____ ->
                :erlang.element(2, w_nodes(tab))

              tab == :______GLOBAL_____ ->
                :erlang.element(2, w_nodes(tab))

              true ->
                dirty_rpc(node, tab, key, lock)
            end
        end
    end
  end

  defp w_nodes(tab) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :where_to_wlock}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {[_ | _], _} = where ->
        where

      _ ->
        :mnesia.abort({:no_exists, tab})
    end
  end

  defp check_majority(true, tab, haveNs) do
    check_majority(tab, haveNs)
  end

  defp check_majority(false, _, _) do
    :ok
  end

  defp check_majority(tab, haveNs) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :majority}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      true ->
        case :mnesia_lib.have_majority(tab, haveNs) do
          true ->
            :ok

          false ->
            :mnesia.abort({:no_majority, tab})
        end

      _ ->
        :ok
    end
  end

  def sticky_wlock(tid, store, oid) do
    sticky_lock(tid, store, oid, :write)
  end

  def sticky_rwlock(tid, store, oid) do
    sticky_lock(tid, store, oid, :read_write)
  end

  defp sticky_lock(tid, store, {tab, key} = oid, lock) do
    n = val({tab, :where_to_read})

    cond do
      node() == n ->
        case need_lock(store, tab, key, :write) do
          :yes ->
            do_sticky_lock(tid, store, oid, lock)

          :no ->
            dirty_sticky_lock(tab, key, [n], lock)
        end

      true ->
        :mnesia.abort({:not_local, tab})
    end
  end

  defp do_sticky_lock(tid, store, {tab, key} = oid, lock) do
    {wNodes, majority} = w_nodes(tab)
    sticky_check_majority(lock, tab, majority, wNodes)
    send(:mnesia_locker, {self(), {:test_set_sticky, tid, oid, lock}})
    n = node()

    receive do
      {:mnesia_locker, ^n, :granted} ->
        :ets.insert(store, {:sticky, true})
        :ets.insert(store, {{:locks, tab, key}, :write})

        for node <- wNodes do
          :ets.insert(store, {:nodes, node})
        end

        :granted

      {:mnesia_locker, ^n, {:granted, val}} ->
        :ets.insert(store, {:sticky, true})

        case opt_lookup_in_client(val, oid, :write) do
          c = r_cyclic() ->
            exit({:aborted, c})

          val2 ->
            :ets.insert(store, {{:locks, tab, key}, :write})

            for node <- wNodes do
              :ets.insert(store, {:nodes, node})
            end

            val2
        end

      {:mnesia_locker, ^n, {:not_granted, reason}} ->
        exit({:aborted, reason})

      {:mnesia_locker, ^n, :not_stuck} ->
        not_stuck(tid, store, tab, key, oid, lock, n)
        dirty_sticky_lock(tab, key, [n], lock)

      {:mnesia_down, node} ->
        eMsg = {:aborted, {:node_not_running, node}}
        flush_remaining([n], node, eMsg)

      {:mnesia_locker, ^n, {:stuck_elsewhere, _N2}} ->
        stuck_elsewhere(tid, store, tab, key, oid, lock)
        dirty_sticky_lock(tab, key, [n], lock)
    end
  end

  defp sticky_check_majority(w, tab, true, wNodes)
       when w == :write or
              w == :read_write do
    case :mnesia_lib.have_majority(tab, wNodes) do
      true ->
        :ok

      false ->
        :mnesia.abort({:no_majority, tab})
    end
  end

  defp sticky_check_majority(_, _, _, _) do
    :ok
  end

  defp not_stuck(tid, store, tab, _Key, oid, _Lock, n) do
    rlock(tid, store, {tab, :______WHOLETABLE_____})
    wlock(tid, store, oid)
    wlock(tid, store, {tab, :______STICK_____})
    ns = val({tab, :where_to_write})
    :rpc.abcast(ns, :mnesia_locker, {:stick, oid, n})
  end

  defp stuck_elsewhere(tid, store, tab, _Key, oid, _Lock) do
    rlock(tid, store, {tab, :______WHOLETABLE_____})
    wlock(tid, store, oid)
    wlock(tid, store, {tab, :______STICK_____})
    ns = val({tab, :where_to_write})
    :rpc.abcast(ns, :mnesia_locker, {:unstick, tab})
  end

  defp dirty_sticky_lock(tab, key, nodes, lock) do
    cond do
      lock == :read_write ->
        :mnesia_lib.db_get(tab, key)

      key == :______WHOLETABLE_____ ->
        nodes

      tab == :______GLOBAL_____ ->
        nodes

      true ->
        :ok
    end
  end

  def sticky_wlock_table(tid, store, tab) do
    sticky_lock(tid, store, {tab, :______WHOLETABLE_____}, :write)
  end

  def wlock(tid, store, oid) do
    wlock(tid, store, oid, _CheckMajority = true)
  end

  defp wlock(tid, store, oid, checkMajority) do
    {tab, key} = oid

    case need_lock(store, tab, key, :write) do
      :yes ->
        {ns, majority} = w_nodes(tab)

        cond do
          checkMajority ->
            check_majority(majority, tab, ns)

          true ->
            :ignore
        end

        op = {self(), {:write, tid, oid}}
        :ets.insert(store, {{:locks, tab, key}, :write})
        get_wlocks_on_nodes(ns, ns, store, op, oid)

      :no
      when key != :______WHOLETABLE_____ and
             tab != :______GLOBAL_____ ->
        []

      :no ->
        :erlang.element(2, w_nodes(tab))
    end
  end

  def wlock_table(tid, store, tab) do
    wlock(tid, store, {tab, :______WHOLETABLE_____})
  end

  def load_lock_table(tid, store, tab) do
    wlock(tid, store, {tab, :______WHOLETABLE_____}, _CheckMajority = false)
  end

  def wlock_no_exist(tid, store, tab, ns) do
    oid = {tab, :______WHOLETABLE_____}
    op = {self(), {:write, tid, oid}}
    get_wlocks_on_nodes(ns, ns, store, op, oid)
  end

  defp need_lock(store, tab, key, lockPattern) do
    tabL =
      :ets.match_object(
        store,
        {{:locks, tab, :______WHOLETABLE_____}, lockPattern}
      )

    cond do
      tabL == [] ->
        keyL =
          :ets.match_object(
            store,
            {{:locks, tab, key}, lockPattern}
          )

        cond do
          keyL == [] ->
            :yes

          true ->
            :no
        end

      true ->
        :no
    end
  end

  defp add_debug(nodes) do
    :erlang.put(:mnesia_wlock_nodes, nodes)
  end

  defp del_debug() do
    :erlang.erase(:mnesia_wlock_nodes)
  end

  defp get_wlocks_on_nodes([node | tail], orig, store, request, oid) do
    send({:mnesia_locker, node}, request)
    :ets.insert(store, {:nodes, node})
    receive_wlocks([node], :undefined, store, oid)

    case node() do
      ^node ->
        get_wlocks_on_nodes(tail, orig, store, request, oid)

      _ ->
        get_wlocks_on_nodes(tail, store, request)
        receive_wlocks(tail, orig, store, oid)
    end
  end

  defp get_wlocks_on_nodes([], orig, _Store, _Request, _Oid) do
    orig
  end

  defp get_wlocks_on_nodes([node | tail], store, request) do
    send({:mnesia_locker, node}, request)
    :ets.insert(store, {:nodes, node})
    get_wlocks_on_nodes(tail, store, request)
  end

  defp get_wlocks_on_nodes([], _, _) do
    :ok
  end

  defp get_rwlocks_on_nodes([readNode | tail], ref, store, tid, oid) do
    op = {self(), {:read_write, tid, oid}}
    send({:mnesia_locker, readNode}, op)
    :ets.insert(store, {:nodes, readNode})

    case receive_wlocks([readNode], ref, store, oid) do
      ^ref ->
        get_rwlocks_on_nodes(tail, ref, store, tid, oid)

      res ->
        get_wlocks_on_nodes(tail, res, store, {self(), {:write, tid, oid}}, oid)
    end
  end

  defp get_rwlocks_on_nodes([], res, _, _, _) do
    res
  end

  defp receive_wlocks([], res, _Store, _Oid) do
    del_debug()
    res
  end

  defp receive_wlocks(nodes = [this | ns], res, store, oid) do
    add_debug(nodes)

    receive do
      {:mnesia_locker, node, :granted} ->
        receive_wlocks(:lists.delete(node, nodes), res, store, oid)

      {:mnesia_locker, node, {:granted, val}} ->
        case opt_lookup_in_client(val, oid, :write) do
          c = r_cyclic() ->
            flush_remaining(nodes, node, {:aborted, c})

          val2 ->
            receive_wlocks(:lists.delete(node, nodes), val2, store, oid)
        end

      {:mnesia_locker, node, {:not_granted, reason}} ->
        reason1 = {:aborted, reason}
        flush_remaining(nodes, node, reason1)

      {:mnesia_locker, node, {:switch, sticky, _Req}} ->
        tail = :lists.delete(node, nodes)
        nonstuck = :lists.delete(sticky, tail)

        for nSNode <- nonstuck do
          :ets.insert(store, {:nodes, nSNode})
        end

        case :lists.member(sticky, tail) do
          true ->
            sticky_flush(nonstuck, store)
            receive_wlocks([sticky], res, store, oid)

          false ->
            sticky_flush(nonstuck, store)
            res
        end

      {:mnesia_down, ^this} ->
        reason1 = {:aborted, {:node_not_running, this}}
        flush_remaining(ns, this, reason1)
    end
  end

  defp sticky_flush([], _) do
    del_debug()
    :ok
  end

  defp sticky_flush(ns = [node | tail], store) do
    add_debug(ns)

    receive do
      {:mnesia_locker, ^node, _} ->
        sticky_flush(tail, store)

      {:mnesia_down, ^node} ->
        reason1 = {:aborted, {:node_not_running, node}}
        flush_remaining(tail, node, reason1)
    end
  end

  defp flush_remaining([], _SkipNode, res) do
    del_debug()
    exit(res)
  end

  defp flush_remaining(ns = [skipNode | tail], skipNode, res) do
    add_debug(ns)

    receive do
      {:mnesia_locker, ^skipNode, _} ->
        flush_remaining(tail, skipNode, res)
    after
      0 ->
        flush_remaining(tail, skipNode, res)
    end
  end

  defp flush_remaining(ns = [node | tail], skipNode, res) do
    add_debug(ns)

    receive do
      {:mnesia_locker, ^node, _} ->
        flush_remaining(tail, skipNode, res)

      {:mnesia_down, ^node} ->
        flush_remaining(tail, skipNode, {:aborted, {:node_not_running, node}})
    end
  end

  defp opt_lookup_in_client(:lookup_in_client, oid, lock) do
    {tab, key} = oid

    try do
      :mnesia_lib.db_get(tab, key)
    catch
      :error, _ ->
        r_cyclic(op: :read, lock: lock, oid: oid, lucky: :nowhere)
    end
  end

  defp opt_lookup_in_client(val, _Oid, _Lock) do
    val
  end

  defp return_granted_or_nodes({_, :______WHOLETABLE_____}, nodes) do
    nodes
  end

  defp return_granted_or_nodes({:______GLOBAL_____, _}, nodes) do
    nodes
  end

  defp return_granted_or_nodes(_, _Nodes) do
    :granted
  end

  def rlock(tid, store, oid) do
    {tab, key} = oid

    case val({tab, :where_to_read}) do
      :nowhere ->
        :mnesia.abort({:no_exists, tab})

      node ->
        case need_lock(store, tab, key, :_) do
          :yes ->
            r = l_request(node, {:read, tid, oid}, store)
            rlock_get_reply(node, store, oid, r)

          :no ->
            cond do
              key == :______WHOLETABLE_____ ->
                [node]

              tab == :______GLOBAL_____ ->
                [node]

              true ->
                dirty_rpc(node, tab, key, :read)
            end
        end
    end
  end

  defp dirty_rpc(:nowhere, tab, key, _Lock) do
    :mnesia.abort({:no_exists, {tab, key}})
  end

  defp dirty_rpc(node, _Tab, :______WHOLETABLE_____, _Lock) do
    [node]
  end

  defp dirty_rpc(node, :______GLOBAL_____, _Key, _Lock) do
    [node]
  end

  defp dirty_rpc(node, tab, key, lock) do
    args = [tab, key]

    case :rpc.call(node, :mnesia_lib, :db_get, args) do
      {:badrpc, reason} ->
        case val({tab, :where_to_read}) do
          ^node ->
            errorTag = :mnesia_lib.dirty_rpc_error_tag(reason)
            :mnesia.abort({errorTag, args})

          _NewNode ->
            c = r_cyclic(op: :read, lock: lock, oid: {tab, key}, lucky: :nowhere)
            exit({:aborted, c})
        end

      other ->
        other
    end
  end

  defp rlock_get_reply(node, store, oid, {:granted, v}) do
    {tab, key} = oid
    :ets.insert(store, {{:locks, tab, key}, :read})
    :ets.insert(store, {:nodes, node})

    case opt_lookup_in_client(v, oid, :read) do
      c = r_cyclic() ->
        :mnesia.abort(c)

      val ->
        val
    end
  end

  defp rlock_get_reply(node, store, oid, :granted) do
    {tab, key} = oid
    :ets.insert(store, {{:locks, tab, key}, :read})
    :ets.insert(store, {:nodes, node})
    return_granted_or_nodes(oid, [node])
  end

  defp rlock_get_reply(node, store, tab, {:granted, v, realKeys}) do
    l = fn k ->
      :ets.insert(store, {{:locks, tab, k}, :read})
    end

    :lists.foreach(l, realKeys)
    :ets.insert(store, {:nodes, node})
    v
  end

  defp rlock_get_reply(_Node, _Store, _Oid, {:not_granted, reason}) do
    exit({:aborted, reason})
  end

  defp rlock_get_reply(_Node, store, oid, {:switch, n2, req}) do
    :ets.insert(store, {:nodes, n2})
    send({:mnesia_locker, n2}, req)
    rlock_get_reply(n2, store, oid, l_req_rec(n2, store))
  end

  def rlock_table(tid, store, tab) do
    rlock(tid, store, {tab, :______WHOLETABLE_____})
  end

  def ixrlock(tid, store, tab, ixKey, pos) do
    case val({tab, :where_to_read}) do
      :nowhere ->
        :mnesia.abort({:no_exists, tab})

      node ->
        case need_lock(store, tab, :______WHOLETABLE_____, :read) do
          :no when node === node() ->
            ix_read_res(tab, ixKey, pos)

          _ ->
            r = l_request(node, {:ix_read, tid, tab, ixKey, pos}, store)
            rlock_get_reply(node, store, tab, r)
        end
    end
  end

  def global_lock(tid, store, item, :write, ns) do
    oid = {:______GLOBAL_____, item}
    op = {self(), {:write, tid, oid}}
    get_wlocks_on_nodes(ns, ns, store, op, oid)
  end

  def global_lock(tid, store, item, :read, ns) do
    oid = {:______GLOBAL_____, item}
    send_requests(ns, {:read, tid, oid})
    rec_requests(ns, oid, store)
    ns
  end

  defp send_requests([node | nodes], x) do
    send({:mnesia_locker, node}, {self(), x})
    send_requests(nodes, x)
  end

  defp send_requests([], _X) do
    :ok
  end

  defp rec_requests([node | nodes], oid, store) do
    res = l_req_rec(node, store)

    try do
      rlock_get_reply(node, store, oid, res)
    catch
      _, reason ->
        flush_remaining(nodes, node, reason)
    else
      _ ->
        rec_requests(nodes, oid, store)
    end
  end

  defp rec_requests([], _Oid, _Store) do
    :ok
  end

  def get_held_locks() do
    send(:mnesia_locker, {:get_table, self(), :mnesia_held_locks})

    locks =
      receive do
        {:mnesia_held_locks, ls} ->
          ls
      after
        5000 ->
          []
      end

    rewrite_locks(locks, [])
  end

  def get_held_locks(tab) when is_atom(tab) do
    oid = {tab, :______WHOLETABLE_____}
    send(:mnesia_locker, {self(), {:is_locked, oid}})

    receive do
      {:mnesia_locker, _Node, locks} ->
        case locks do
          [] ->
            []

          [{^oid, _Prev, what}] ->
            what
        end
    end
  end

  defp rewrite_locks([{oid, _, ls} | locks], acc0) do
    acc = rewrite_locks(ls, oid, acc0)
    rewrite_locks(locks, acc)
  end

  defp rewrite_locks([], acc) do
    :lists.reverse(acc)
  end

  defp rewrite_locks([{op, tid} | ls], oid, acc) do
    rewrite_locks(ls, oid, [{oid, op, tid} | acc])
  end

  defp rewrite_locks([], _, acc) do
    acc
  end

  def get_lock_queue() do
    send(:mnesia_locker, {:get_table, self(), :mnesia_lock_queue})

    q =
      receive do
        {:mnesia_lock_queue, locks} ->
          locks
      after
        5000 ->
          []
      end

    for {:queue, oid, tid, op, pid, wFT} <- q do
      {oid, op, pid, tid, wFT}
    end
  end

  defp do_stop() do
    exit(:shutdown)
  end

  def system_continue(_Parent, _Debug, state) do
    loop(state)
  end

  def system_terminate(_Reason, _Parent, _Debug, _State) do
    do_stop()
  end

  def system_code_change(state, _Module, _OldVsn, _Extra) do
    {:ok, state}
  end

  Record.defrecord(:r_pid_info, :pid_info,
    serial: :undefined,
    number: :undefined,
    nodename: :undefined,
    creation: :undefined
  )

  defp cmp_tid(r9B, r_tid() = t, r_tid() = t)
       when r9B == true or
              r9B == false do
    0
  end

  defp cmp_tid(r9B, r_tid(counter: c, pid: pid1), r_tid(counter: c, pid: pid2))
       when r9B == true or r9B == false do
    cmp_pid_info(r9B, pid_to_pid_info(pid1), pid_to_pid_info(pid2))
  end

  defp cmp_tid(r9B, r_tid(counter: c1), r_tid(counter: c2))
       when r9B == true or r9B == false do
    cmp(c1, c2)
  end

  defp cmp_pid_info(_, r_pid_info() = pI, r_pid_info() = pI) do
    0
  end

  defp cmp_pid_info(
         false,
         r_pid_info(serial: s, number: n, nodename: nN, creation: c1),
         r_pid_info(serial: s, number: n, nodename: nN, creation: c2)
       ) do
    cmp(c1, c2)
  end

  defp cmp_pid_info(
         false,
         r_pid_info(serial: s, number: n, nodename: nN1),
         r_pid_info(serial: s, number: n, nodename: nN2)
       ) do
    cmp(nN1, nN2)
  end

  defp cmp_pid_info(false, r_pid_info(serial: s, number: n1), r_pid_info(serial: s, number: n2)) do
    cmp(n1, n2)
  end

  defp cmp_pid_info(false, r_pid_info(serial: s1), r_pid_info(serial: s2)) do
    cmp(s1, s2)
  end

  defp cmp_pid_info(
         true,
         r_pid_info(nodename: nN, creation: c, serial: s, number: n1),
         r_pid_info(nodename: nN, creation: c, serial: s, number: n2)
       ) do
    cmp(n1, n2)
  end

  defp cmp_pid_info(
         true,
         r_pid_info(nodename: nN, creation: c, serial: s1),
         r_pid_info(nodename: nN, creation: c, serial: s2)
       ) do
    cmp(s1, s2)
  end

  defp cmp_pid_info(
         true,
         r_pid_info(nodename: nN, creation: c1),
         r_pid_info(nodename: nN, creation: c2)
       ) do
    cmp(c1, c2)
  end

  defp cmp_pid_info(true, r_pid_info(nodename: nN1), r_pid_info(nodename: nN2)) do
    cmp(nN1, nN2)
  end

  defp cmp(x, x) do
    0
  end

  defp cmp(x1, x2) when x1 < x2 do
    -1
  end

  defp cmp(_X1, _X2) do
    1
  end

  defp pid_to_pid_info(pid) when is_pid(pid) do
    [
      [131, 103, 100, nNL1, nNL0]
      | rest
    ] = :erlang.binary_to_list(:erlang.term_to_binary(pid))

    [n3, n2, n1, n0, s3, s2, s1, s0, creation] = drop(bytes2int(nNL1, nNL0), rest)

    r_pid_info(
      serial: bytes2int(s3, s2, s1, s0),
      number: bytes2int(n3, n2, n1, n0),
      nodename: node(pid),
      creation: creation
    )
  end

  defp drop(0, l) do
    l
  end

  defp drop(n, [_ | l]) when is_integer(n) and n > 0 do
    drop(n - 1, l)
  end

  defp drop(n, []) when is_integer(n) and n > 0 do
    []
  end

  defp bytes2int(n1, n0)
       when 0 <= n1 and n1 <= 255 and
              0 <= n0 and n0 <= 255 do
    n1 <<< 8 ||| n0
  end

  defp bytes2int(n3, n2, n1, n0)
       when 0 <= n3 and n3 <= 255 and
              0 <= n2 and n2 <= 255 and 0 <= n1 and
              n1 <= 255 and 0 <= n0 and n0 <= 255 do
    n3 <<< 24 ||| n2 <<< 16 ||| n1 <<< 8 ||| n0
  end
end
