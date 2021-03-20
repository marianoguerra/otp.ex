defmodule :m_mnesia do
  use Bitwise
  import :mnesia_lib, only: [verbose: 2]
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

  defp is_dollar_digits(var) do
    case :erlang.atom_to_list(var) do
      [?$ | digs] ->
        is_digits(digs)

      _ ->
        false
    end
  end

  defp is_digits([dig | tail]) do
    cond do
      ?0 <= dig and dig <= ?9 ->
        is_digits(tail)

      true ->
        false
    end
  end

  defp is_digits([]) do
    true
  end

  def has_var(x) when is_atom(x) do
    cond do
      x == :_ ->
        true

      is_atom(x) ->
        is_dollar_digits(x)

      true ->
        false
    end
  end

  def has_var(x) when is_tuple(x) do
    e_has_var(x, tuple_size(x))
  end

  def has_var([h | t]) do
    case has_var(h) do
      false ->
        has_var(t)

      other ->
        other
    end
  end

  def has_var(_) do
    false
  end

  defp e_has_var(_, 0) do
    false
  end

  defp e_has_var(x, pos) do
    case has_var(:erlang.element(pos, x)) do
      false ->
        e_has_var(x, pos - 1)

      other ->
        other
    end
  end

  def start() do
    start([])
  end

  defp start_() do
    {time, res} = :timer.tc(:application, :start, [:mnesia, :temporary])
    secs = div(time, 1_000_000)

    case res do
      :ok ->
        verbose('Mnesia started, ~p seconds~n', [secs])
        :ok

      {:error, {:already_started, :mnesia}} ->
        verbose('Mnesia already started, ~p seconds~n', [secs])
        :ok

      {:error, r} ->
        verbose('Mnesia failed to start, ~p seconds: ~p~n', [secs, r])
        {:error, r}
    end
  end

  def start(extraEnv) when is_list(extraEnv) do
    case :mnesia_lib.ensure_loaded(:mnesia) do
      :ok ->
        patched_start(extraEnv)

      error ->
        error
    end
  end

  def start(extraEnv) do
    {:error, {:badarg, extraEnv}}
  end

  defp patched_start([{env, val} | tail]) when is_atom(env) do
    case :mnesia_monitor.patch_env(env, val) do
      {:error, reason} ->
        {:error, reason}

      _NewVal ->
        patched_start(tail)
    end
  end

  defp patched_start([head | _]) do
    {:error, {:bad_type, head}}
  end

  defp patched_start([]) do
    start_()
  end

  def stop() do
    case :application.stop(:mnesia) do
      :ok ->
        :stopped

      {:error, {:not_started, :mnesia}} ->
        :stopped

      other ->
        other
    end
  end

  def change_config(:extra_db_nodes, ns) when is_list(ns) do
    :mnesia_controller.connect_nodes(ns)
  end

  def change_config(:dc_dump_limit, n)
      when is_number(n) and
             n > 0 do
    case :mnesia_lib.is_running() do
      :yes ->
        :mnesia_lib.set(:dc_dump_limit, n)
        {:ok, n}

      _ ->
        {:error, {:not_started, :mnesia}}
    end
  end

  def change_config(badKey, _BadVal) do
    {:error, {:badarg, badKey}}
  end

  def set_debug_level(level) do
    :mnesia_subscr.set_debug_level(level)
  end

  def lkill() do
    :mnesia_sup.kill()
  end

  def kill() do
    :rpc.multicall(:mnesia_sup, :kill, [])
  end

  def ms() do
    [
      :mnesia,
      :mnesia_app,
      :mnesia_backup,
      :mnesia_bup,
      :mnesia_checkpoint,
      :mnesia_checkpoint_sup,
      :mnesia_controller,
      :mnesia_dumper,
      :mnesia_loader,
      :mnesia_frag,
      :mnesia_frag_hash,
      :mnesia_index,
      :mnesia_kernel_sup,
      :mnesia_late_loader,
      :mnesia_lib,
      :mnesia_log,
      :mnesia_registry,
      :mnesia_schema,
      :mnesia_snmp_hook,
      :mnesia_snmp_sup,
      :mnesia_subscr,
      :mnesia_sup,
      :mnesia_text,
      :mnesia_tm,
      :mnesia_recover,
      :mnesia_locker,
      :mnesia_ext_sup,
      :mnesia_monitor,
      :mnesia_event
    ]
  end

  def abort(reason = {:aborted, _}) do
    exit(reason)
  end

  def abort(reason) do
    exit({:aborted, reason})
  end

  def is_transaction() do
    case :erlang.get(:mnesia_activity_state) do
      {_, tid, _Ts} when :erlang.element(1, tid) == :tid ->
        true

      _ ->
        false
    end
  end

  def transaction(fun) do
    transaction(:erlang.get(:mnesia_activity_state), fun, [], :infinity, :mnesia, :async)
  end

  def transaction(fun, retries)
      when is_integer(retries) and
             retries >= 0 do
    transaction(:erlang.get(:mnesia_activity_state), fun, [], retries, :mnesia, :async)
  end

  def transaction(fun, retries) when retries == :infinity do
    transaction(:erlang.get(:mnesia_activity_state), fun, [], retries, :mnesia, :async)
  end

  def transaction(fun, args) do
    transaction(:erlang.get(:mnesia_activity_state), fun, args, :infinity, :mnesia, :async)
  end

  def transaction(fun, args, retries) do
    transaction(:erlang.get(:mnesia_activity_state), fun, args, retries, :mnesia, :async)
  end

  def sync_transaction(fun) do
    transaction(:erlang.get(:mnesia_activity_state), fun, [], :infinity, :mnesia, :sync)
  end

  def sync_transaction(fun, retries)
      when is_integer(retries) and
             retries >= 0 do
    transaction(:erlang.get(:mnesia_activity_state), fun, [], retries, :mnesia, :sync)
  end

  def sync_transaction(fun, retries) when retries == :infinity do
    transaction(:erlang.get(:mnesia_activity_state), fun, [], retries, :mnesia, :sync)
  end

  def sync_transaction(fun, args) do
    transaction(:erlang.get(:mnesia_activity_state), fun, args, :infinity, :mnesia, :sync)
  end

  def sync_transaction(fun, args, retries) do
    transaction(:erlang.get(:mnesia_activity_state), fun, args, retries, :mnesia, :sync)
  end

  defp transaction(state, fun, args, retries, mod, kind)
       when is_function(fun) and is_list(args) and
              retries == :infinity and is_atom(mod) do
    :mnesia_tm.transaction(state, fun, args, retries, mod, kind)
  end

  defp transaction(state, fun, args, retries, mod, kind)
       when is_function(fun) and is_list(args) and
              is_integer(retries) and retries >= 0 and
              is_atom(mod) do
    :mnesia_tm.transaction(state, fun, args, retries, mod, kind)
  end

  defp transaction(_State, fun, args, retries, mod, _Kind) do
    {:aborted, {:badarg, fun, args, retries, mod}}
  end

  defp non_transaction(state, fun, args, activityKind, mod)
       when is_function(fun) and is_list(args) and
              is_atom(mod) do
    :mnesia_tm.non_transaction(state, fun, args, activityKind, mod)
  end

  defp non_transaction(_State, fun, args, _ActivityKind, _Mod) do
    {:aborted, {:badarg, fun, args}}
  end

  def async_dirty(fun) do
    async_dirty(fun, [])
  end

  def async_dirty(fun, args) do
    non_transaction(:erlang.get(:mnesia_activity_state), fun, args, :async_dirty, :mnesia)
  end

  def sync_dirty(fun) do
    sync_dirty(fun, [])
  end

  def sync_dirty(fun, args) do
    non_transaction(:erlang.get(:mnesia_activity_state), fun, args, :sync_dirty, :mnesia)
  end

  def ets(fun) do
    ets(fun, [])
  end

  def ets(fun, args) do
    non_transaction(:erlang.get(:mnesia_activity_state), fun, args, :ets, :mnesia)
  end

  def activity(kind, fun) do
    activity(kind, fun, [])
  end

  def activity(kind, fun, args) when is_list(args) do
    activity(kind, fun, args, :mnesia_monitor.get_env(:access_module))
  end

  def activity(kind, fun, mod) do
    activity(kind, fun, [], mod)
  end

  def activity(kind, fun, args, mod) do
    state = :erlang.get(:mnesia_activity_state)

    case kind do
      :ets ->
        non_transaction(state, fun, args, kind, mod)

      :async_dirty ->
        non_transaction(state, fun, args, kind, mod)

      :sync_dirty ->
        non_transaction(state, fun, args, kind, mod)

      :transaction ->
        wrap_trans(state, fun, args, :infinity, mod, :async)

      {:transaction, retries} ->
        wrap_trans(state, fun, args, retries, mod, :async)

      :sync_transaction ->
        wrap_trans(state, fun, args, :infinity, mod, :sync)

      {:sync_transaction, retries} ->
        wrap_trans(state, fun, args, retries, mod, :sync)

      _ ->
        {:aborted, {:bad_type, kind}}
    end
  end

  defp wrap_trans(state, fun, args, retries, mod, kind) do
    case transaction(state, fun, args, retries, mod, kind) do
      {:atomic, goodRes} ->
        goodRes

      badRes ->
        exit(badRes)
    end
  end

  def lock(lockItem, lockKind) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        lock(tid, ts, lockItem, lockKind)

      {mod, tid, ts} ->
        mod.lock(tid, ts, lockItem, lockKind)

      _ ->
        abort(:no_transaction)
    end
  end

  def lock_table(tab, lockKind) do
    lock({:table, tab}, lockKind)
  end

  def lock(tid, ts, lockItem, lockKind) do
    case :erlang.element(1, tid) do
      :tid ->
        case lockItem do
          {:record, tab, key} ->
            lock_record(tid, ts, tab, key, lockKind)

          {:table, tab} ->
            lock_table(tid, ts, tab, lockKind)

          {:global, globalKey, nodes} ->
            global_lock(tid, ts, globalKey, lockKind, nodes)

          _ ->
            abort({:bad_type, lockItem})
        end

      _Protocol ->
        []
    end
  end

  def read_lock_table(tab) do
    lock({:table, tab}, :read)
    :ok
  end

  def write_lock_table(tab) do
    lock({:table, tab}, :write)
    :ok
  end

  defp lock_record(tid, ts, tab, key, lockKind)
       when is_atom(tab) do
    store = r_tidstore(ts, :store)
    oid = {tab, key}

    case lockKind do
      :read ->
        :mnesia_locker.rlock(tid, store, oid)

      :write ->
        :mnesia_locker.wlock(tid, store, oid)

      :sticky_write ->
        :mnesia_locker.sticky_wlock(tid, store, oid)

      :none ->
        []

      _ ->
        abort({:bad_type, tab, lockKind})
    end
  end

  defp lock_record(_Tid, _Ts, tab, _Key, _LockKind) do
    abort({:bad_type, tab})
  end

  defp lock_table(tid, ts, tab, lockKind) when is_atom(tab) do
    store = r_tidstore(ts, :store)

    case lockKind do
      :read ->
        :mnesia_locker.rlock_table(tid, store, tab)

      :write ->
        :mnesia_locker.wlock_table(tid, store, tab)

      :load ->
        :mnesia_locker.load_lock_table(tid, store, tab)

      :sticky_write ->
        :mnesia_locker.sticky_wlock_table(tid, store, tab)

      :none ->
        []

      _ ->
        abort({:bad_type, tab, lockKind})
    end
  end

  defp lock_table(_Tid, _Ts, tab, _LockKind) do
    abort({:bad_type, tab})
  end

  defp global_lock(tid, ts, item, kind, nodes)
       when is_list(nodes) do
    case :erlang.element(1, tid) do
      :tid ->
        store = r_tidstore(ts, :store)
        goodNs = good_global_nodes(nodes)

        cond do
          kind != :read and kind != :write ->
            abort({:bad_type, kind})

          true ->
            :mnesia_locker.global_lock(tid, store, item, kind, goodNs)
        end

      _Protocol ->
        []
    end
  end

  defp global_lock(_Tid, _Ts, _Item, _Kind, nodes) do
    abort({:bad_type, nodes})
  end

  defp good_global_nodes(nodes) do
    recover = [node() | val(:recover_nodes)]
    :mnesia_lib.intersect(nodes, recover)
  end

  def write(val)
      when is_tuple(val) and
             tuple_size(val) > 2 do
    tab = :erlang.element(1, val)
    write(tab, val, :write)
  end

  def write(val) do
    abort({:bad_type, val})
  end

  def s_write(val)
      when is_tuple(val) and
             tuple_size(val) > 2 do
    tab = :erlang.element(1, val)
    write(tab, val, :sticky_write)
  end

  def write(tab, val, lockKind) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        write(tid, ts, tab, val, lockKind)

      {mod, tid, ts} ->
        mod.write(tid, ts, tab, val, lockKind)

      _ ->
        abort(:no_transaction)
    end
  end

  def write(tid, ts, tab, val, lockKind)
      when is_atom(tab) and tab != :schema and
             is_tuple(val) and tuple_size(val) > 2 do
    case :erlang.element(1, tid) do
      :ets ->
        :ets.insert(tab, val)
        :ok

      :tid ->
        store = r_tidstore(ts, :store)
        oid = {tab, :erlang.element(2, val)}

        case lockKind do
          :write ->
            :mnesia_locker.wlock(tid, store, oid)

          :sticky_write ->
            :mnesia_locker.sticky_wlock(tid, store, oid)

          _ ->
            abort({:bad_type, tab, lockKind})
        end

        write_to_store(tab, store, oid, val)

      protocol ->
        do_dirty_write(protocol, tab, val)
    end
  end

  def write(_Tid, _Ts, tab, val, lockKind) do
    abort({:bad_type, tab, val, lockKind})
  end

  defp write_to_store(tab, store, oid, val) do
    {_, _, type} = :mnesia_lib.validate_record(tab, val)
    ^oid = {tab, :erlang.element(2, val)}

    case type do
      :bag ->
        :ets.insert(store, {oid, val, :write})

      _ ->
        :ets.delete(store, oid)
        :ets.insert(store, {oid, val, :write})
    end

    :ok
  end

  def delete({tab, key}) do
    delete(tab, key, :write)
  end

  def delete(oid) do
    abort({:bad_type, oid})
  end

  def s_delete({tab, key}) do
    delete(tab, key, :sticky_write)
  end

  def s_delete(oid) do
    abort({:bad_type, oid})
  end

  def delete(tab, key, lockKind) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        delete(tid, ts, tab, key, lockKind)

      {mod, tid, ts} ->
        mod.delete(tid, ts, tab, key, lockKind)

      _ ->
        abort(:no_transaction)
    end
  end

  def delete(tid, ts, tab, key, lockKind)
      when is_atom(tab) and tab != :schema do
    case :erlang.element(1, tid) do
      :ets ->
        :ets.delete(tab, key)
        :ok

      :tid ->
        store = r_tidstore(ts, :store)
        oid = {tab, key}

        case lockKind do
          :write ->
            :mnesia_locker.wlock(tid, store, oid)

          :sticky_write ->
            :mnesia_locker.sticky_wlock(tid, store, oid)

          _ ->
            abort({:bad_type, tab, lockKind})
        end

        :ets.delete(store, oid)
        :ets.insert(store, {oid, oid, :delete})
        :ok

      protocol ->
        do_dirty_delete(protocol, tab, key)
    end
  end

  def delete(_Tid, _Ts, tab, _Key, _LockKind) do
    abort({:bad_type, tab})
  end

  def delete_object(val)
      when is_tuple(val) and
             tuple_size(val) > 2 do
    tab = :erlang.element(1, val)
    delete_object(tab, val, :write)
  end

  def delete_object(val) do
    abort({:bad_type, val})
  end

  def s_delete_object(val)
      when is_tuple(val) and
             tuple_size(val) > 2 do
    tab = :erlang.element(1, val)
    delete_object(tab, val, :sticky_write)
  end

  def s_delete_object(val) do
    abort({:bad_type, val})
  end

  def delete_object(tab, val, lockKind) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        delete_object(tid, ts, tab, val, lockKind)

      {mod, tid, ts} ->
        mod.delete_object(tid, ts, tab, val, lockKind)

      _ ->
        abort(:no_transaction)
    end
  end

  def delete_object(tid, ts, tab, val, lockKind)
      when is_atom(tab) and tab != :schema and
             is_tuple(val) and tuple_size(val) > 2 do
    case has_var(val) do
      false ->
        do_delete_object(tid, ts, tab, val, lockKind)

      true ->
        abort({:bad_type, tab, val})
    end
  end

  def delete_object(_Tid, _Ts, tab, _Key, _LockKind) do
    abort({:bad_type, tab})
  end

  defp do_delete_object(tid, ts, tab, val, lockKind) do
    case :erlang.element(1, tid) do
      :ets ->
        :ets.match_delete(tab, val)
        :ok

      :tid ->
        store = r_tidstore(ts, :store)
        oid = {tab, :erlang.element(2, val)}

        case lockKind do
          :write ->
            :mnesia_locker.wlock(tid, store, oid)

          :sticky_write ->
            :mnesia_locker.sticky_wlock(tid, store, oid)

          _ ->
            abort({:bad_type, tab, lockKind})
        end

        case val({tab, :setorbag}) do
          :bag ->
            :ets.match_delete(store, {oid, val, :_})
            :ets.insert(store, {oid, val, :delete_object})

          _ ->
            case :ets.match_object(
                   store,
                   {oid, :_, :write}
                 ) ++
                   :ets.match_object(
                     store,
                     {oid, :_, :delete}
                   ) do
              [] ->
                :ets.match_delete(store, {oid, val, :_})
                :ets.insert(store, {oid, val, :delete_object})

              ops ->
                case :lists.member({oid, val, :write}, ops) do
                  true ->
                    :ets.delete(store, oid)
                    :ets.insert(store, {oid, oid, :delete})

                  false ->
                    :ok
                end
            end
        end

        :ok

      protocol ->
        do_dirty_delete_object(protocol, tab, val)
    end
  end

  def read(tab, key) do
    read(tab, key, :read)
  end

  def read({tab, key}) do
    read(tab, key, :read)
  end

  def read(oid) do
    abort({:bad_type, oid})
  end

  def wread({tab, key}) do
    read(tab, key, :write)
  end

  def wread(oid) do
    abort({:bad_type, oid})
  end

  def read(tab, key, lockKind) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        read(tid, ts, tab, key, lockKind)

      {mod, tid, ts} ->
        mod.read(tid, ts, tab, key, lockKind)

      _ ->
        abort(:no_transaction)
    end
  end

  def read(tid, ts, tab, key, lockKind)
      when is_atom(tab) and tab != :schema do
    case :erlang.element(1, tid) do
      :ets ->
        :ets.lookup(tab, key)

      :tid ->
        store = r_tidstore(ts, :store)
        oid = {tab, key}

        objsFun = fn ->
          case lockKind do
            :read ->
              :mnesia_locker.rlock(tid, store, oid)

            :write ->
              :mnesia_locker.rwlock(tid, store, oid)

            :sticky_write ->
              :mnesia_locker.sticky_rwlock(tid, store, oid)

            _ ->
              abort({:bad_type, tab, lockKind})
          end
        end

        add_written(:ets.lookup(store, oid), tab, objsFun, lockKind)

      _Protocol ->
        dirty_read(tab, key)
    end
  end

  def read(_Tid, _Ts, tab, _Key, _LockKind) do
    abort({:bad_type, tab})
  end

  def first(tab) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        first(tid, ts, tab)

      {mod, tid, ts} ->
        mod.first(tid, ts, tab)

      _ ->
        abort(:no_transaction)
    end
  end

  def first(tid, ts, tab)
      when is_atom(tab) and
             tab != :schema do
    case :erlang.element(1, tid) do
      :ets ->
        :ets.first(tab)

      :tid ->
        lock_table(tid, ts, tab, :read)
        do_fixtable(tab, ts)
        key = dirty_first(tab)
        stored_keys(tab, key, :"$end_of_table", ts, :next, val({tab, :setorbag}))

      _Protocol ->
        dirty_first(tab)
    end
  end

  def first(_Tid, _Ts, tab) do
    abort({:bad_type, tab})
  end

  def last(tab) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        last(tid, ts, tab)

      {mod, tid, ts} ->
        mod.last(tid, ts, tab)

      _ ->
        abort(:no_transaction)
    end
  end

  def last(tid, ts, tab)
      when is_atom(tab) and
             tab != :schema do
    case :erlang.element(1, tid) do
      :ets ->
        :ets.last(tab)

      :tid ->
        lock_table(tid, ts, tab, :read)
        do_fixtable(tab, ts)
        key = dirty_last(tab)
        stored_keys(tab, key, :"$end_of_table", ts, :prev, val({tab, :setorbag}))

      _Protocol ->
        dirty_last(tab)
    end
  end

  def last(_Tid, _Ts, tab) do
    abort({:bad_type, tab})
  end

  def next(tab, key) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        next(tid, ts, tab, key)

      {mod, tid, ts} ->
        mod.next(tid, ts, tab, key)

      _ ->
        abort(:no_transaction)
    end
  end

  def next(tid, ts, tab, key)
      when is_atom(tab) and
             tab != :schema do
    case :erlang.element(1, tid) do
      :ets ->
        :ets.next(tab, key)

      :tid ->
        lock_table(tid, ts, tab, :read)
        do_fixtable(tab, ts)

        new =
          try do
            dirty_next(tab, key)
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end

        stored_keys(tab, new, key, ts, :next, val({tab, :setorbag}))

      _Protocol ->
        dirty_next(tab, key)
    end
  end

  def next(_Tid, _Ts, tab, _) do
    abort({:bad_type, tab})
  end

  def prev(tab, key) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        prev(tid, ts, tab, key)

      {mod, tid, ts} ->
        mod.prev(tid, ts, tab, key)

      _ ->
        abort(:no_transaction)
    end
  end

  def prev(tid, ts, tab, key)
      when is_atom(tab) and
             tab != :schema do
    case :erlang.element(1, tid) do
      :ets ->
        :ets.prev(tab, key)

      :tid ->
        lock_table(tid, ts, tab, :read)
        do_fixtable(tab, ts)

        new =
          try do
            dirty_prev(tab, key)
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end

        stored_keys(tab, new, key, ts, :prev, val({tab, :setorbag}))

      _Protocol ->
        dirty_prev(tab, key)
    end
  end

  def prev(_Tid, _Ts, tab, _) do
    abort({:bad_type, tab})
  end

  defp stored_keys(tab, :"$end_of_table", prev, ts, op, type) do
    case ts_keys(r_tidstore(ts, :store), tab, op, type, []) do
      [] ->
        :"$end_of_table"

      keys when type == :ordered_set ->
        get_ordered_tskey(prev, keys, op)

      keys ->
        get_next_tskey(prev, keys, tab)
    end
  end

  defp stored_keys(
         tab,
         {:EXIT, {:aborted, r = {:badarg, [tab, key]}}},
         key,
         r_tidstore(store: store),
         op,
         type
       ) do
    case :ets.match(store, {{tab, key}, :_, :"$1"}) do
      [] ->
        abort(r)

      ops ->
        case :lists.last(ops) do
          [:delete] ->
            abort(r)

          _ ->
            case ts_keys(store, tab, op, type, []) do
              [] ->
                :"$end_of_table"

              keys ->
                get_next_tskey(key, keys, tab)
            end
        end
    end
  end

  defp stored_keys(_, {:EXIT, {:aborted, r}}, _, _, _, _) do
    abort(r)
  end

  defp stored_keys(tab, key, prev, r_tidstore(store: store), op, :ordered_set) do
    case :ets.match(store, {{tab, key}, :_, :"$1"}) do
      [] ->
        keys = ts_keys(store, tab, op, :ordered_set, [key])
        get_ordered_tskey(prev, keys, op)

      ops ->
        case :lists.last(ops) do
          [:delete] ->
            apply(:mnesia, op, [tab, key])

          _ ->
            keys = ts_keys(store, tab, op, :ordered_set, [key])
            get_ordered_tskey(prev, keys, op)
        end
    end
  end

  defp stored_keys(tab, key, _, r_tidstore(store: store), op, _) do
    case :ets.match(store, {{tab, key}, :_, :"$1"}) do
      [] ->
        key

      ops ->
        case :lists.last(ops) do
          [:delete] ->
            apply(:mnesia, op, [tab, key])

          _ ->
            key
        end
    end
  end

  defp get_ordered_tskey(:"$end_of_table", [first | _], _) do
    first
  end

  defp get_ordered_tskey(prev, [first | _], :next) when prev < first do
    first
  end

  defp get_ordered_tskey(prev, [first | _], :prev) when prev > first do
    first
  end

  defp get_ordered_tskey(prev, [_ | r], op) do
    get_ordered_tskey(prev, r, op)
  end

  defp get_ordered_tskey(_, [], _) do
    :"$end_of_table"
  end

  defp get_next_tskey(key, keys, tab) do
    next =
      cond do
        key == :"$end_of_table" ->
          hd(keys)

        true ->
          case :lists.dropwhile(
                 fn a ->
                   a != key
                 end,
                 keys
               ) do
            [] ->
              hd(keys)

            [^key] ->
              :"$end_of_table"

            [^key, next2 | _] ->
              next2
          end
      end

    case next do
      :"$end_of_table" ->
        :"$end_of_table"

      _ ->
        case dirty_read(tab, next) do
          [] ->
            next

          _ ->
            get_next_tskey(next, keys, tab)
        end
    end
  end

  defp ts_keys(store, tab, op, type, def__) do
    all = :ets.match(store, {{tab, :"$1"}, :_, :"$2"})
    keys = ts_keys_1(all, def__)

    cond do
      type == :ordered_set and op == :prev ->
        :lists.reverse(:lists.sort(keys))

      type == :ordered_set ->
        :lists.sort(keys)

      op == :next ->
        :lists.reverse(keys)

      true ->
        keys
    end
  end

  defp ts_keys_1([[key, :write] | r], []) do
    ts_keys_1(r, [key])
  end

  defp ts_keys_1([[key, :write] | r], acc = [key | _]) do
    ts_keys_1(r, acc)
  end

  defp ts_keys_1([[key, :write] | r], acc) do
    ts_keys_1(r, [key | acc])
  end

  defp ts_keys_1([[key, :delete] | r], [key | acc]) do
    ts_keys_1(r, acc)
  end

  defp ts_keys_1([_ | r], acc) do
    ts_keys_1(r, acc)
  end

  defp ts_keys_1([], acc) do
    acc
  end

  def foldl(fun, acc, tab) do
    foldl(fun, acc, tab, :read)
  end

  def foldl(fun, acc, tab, lockKind) when is_function(fun) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        foldl(tid, ts, fun, acc, tab, lockKind)

      {mod, tid, ts} ->
        mod.foldl(tid, ts, fun, acc, tab, lockKind)

      _ ->
        abort(:no_transaction)
    end
  end

  def foldl(activityId, opaque, fun, acc, tab, lockKind) do
    {type, prev} = init_iteration(activityId, opaque, tab, lockKind)

    res =
      try do
        do_foldl(activityId, opaque, tab, dirty_first(tab), fun, acc, type, prev)
      catch
        _, _Reason ->
          {:EXIT, _Reason}
      end

    close_iteration(res, tab)
  end

  defp do_foldl(a, o, tab, :"$end_of_table", fun, rAcc, _Type, stored) do
    :lists.foldl(
      fn key, acc ->
        :lists.foldl(fun, acc, read(a, o, tab, key, :read))
      end,
      rAcc,
      stored
    )
  end

  defp do_foldl(a, o, tab, key, fun, acc, :ordered_set, [h | stored])
       when h == key do
    newAcc = :lists.foldl(fun, acc, read(a, o, tab, key, :read))
    {_, tid, ts} = :erlang.get(:mnesia_activity_state)
    do_foldl(tid, ts, tab, dirty_next(tab, key), fun, newAcc, :ordered_set, stored)
  end

  defp do_foldl(a, o, tab, key, fun, acc, :ordered_set, [h | stored])
       when h < key do
    newAcc = :lists.foldl(fun, acc, read(a, o, tab, h, :read))
    {_, tid, ts} = :erlang.get(:mnesia_activity_state)
    do_foldl(tid, ts, tab, key, fun, newAcc, :ordered_set, stored)
  end

  defp do_foldl(a, o, tab, key, fun, acc, :ordered_set, [h | stored])
       when h > key do
    newAcc = :lists.foldl(fun, acc, read(a, o, tab, key, :read))
    {_, tid, ts} = :erlang.get(:mnesia_activity_state)
    do_foldl(tid, ts, tab, dirty_next(tab, key), fun, newAcc, :ordered_set, [h | stored])
  end

  defp do_foldl(a, o, tab, key, fun, acc, type, stored) do
    newAcc = :lists.foldl(fun, acc, read(a, o, tab, key, :read))
    newStored = :ordsets.del_element(key, stored)
    {_, tid, ts} = :erlang.get(:mnesia_activity_state)
    do_foldl(tid, ts, tab, dirty_next(tab, key), fun, newAcc, type, newStored)
  end

  def foldr(fun, acc, tab) do
    foldr(fun, acc, tab, :read)
  end

  def foldr(fun, acc, tab, lockKind) when is_function(fun) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        foldr(tid, ts, fun, acc, tab, lockKind)

      {mod, tid, ts} ->
        mod.foldr(tid, ts, fun, acc, tab, lockKind)

      _ ->
        abort(:no_transaction)
    end
  end

  def foldr(activityId, opaque, fun, acc, tab, lockKind) do
    {type, tempPrev} = init_iteration(activityId, opaque, tab, lockKind)

    prev =
      cond do
        type == :ordered_set ->
          :lists.reverse(tempPrev)

        true ->
          tempPrev
      end

    res =
      try do
        do_foldr(activityId, opaque, tab, dirty_last(tab), fun, acc, type, prev)
      catch
        _, _Reason ->
          {:EXIT, _Reason}
      end

    close_iteration(res, tab)
  end

  defp do_foldr(a, o, tab, :"$end_of_table", fun, rAcc, _Type, stored) do
    :lists.foldl(
      fn key, acc ->
        :lists.foldl(fun, acc, read(a, o, tab, key, :read))
      end,
      rAcc,
      stored
    )
  end

  defp do_foldr(a, o, tab, key, fun, acc, :ordered_set, [h | stored])
       when h == key do
    newAcc = :lists.foldl(fun, acc, read(a, o, tab, key, :read))
    {_, tid, ts} = :erlang.get(:mnesia_activity_state)
    do_foldr(tid, ts, tab, dirty_prev(tab, key), fun, newAcc, :ordered_set, stored)
  end

  defp do_foldr(a, o, tab, key, fun, acc, :ordered_set, [h | stored])
       when h > key do
    newAcc = :lists.foldl(fun, acc, read(a, o, tab, h, :read))
    {_, tid, ts} = :erlang.get(:mnesia_activity_state)
    do_foldr(tid, ts, tab, key, fun, newAcc, :ordered_set, stored)
  end

  defp do_foldr(a, o, tab, key, fun, acc, :ordered_set, [h | stored])
       when h < key do
    newAcc = :lists.foldl(fun, acc, read(a, o, tab, key, :read))
    {_, tid, ts} = :erlang.get(:mnesia_activity_state)
    do_foldr(tid, ts, tab, dirty_prev(tab, key), fun, newAcc, :ordered_set, [h | stored])
  end

  defp do_foldr(a, o, tab, key, fun, acc, type, stored) do
    newAcc = :lists.foldl(fun, acc, read(a, o, tab, key, :read))
    newStored = :ordsets.del_element(key, stored)
    {_, tid, ts} = :erlang.get(:mnesia_activity_state)
    do_foldr(tid, ts, tab, dirty_prev(tab, key), fun, newAcc, type, newStored)
  end

  defp init_iteration(activityId, opaque, tab, lockKind) do
    lock(activityId, opaque, {:table, tab}, lockKind)
    type = val({tab, :setorbag})
    previous = add_previous(activityId, opaque, type, tab)
    st = val({tab, :storage_type})

    cond do
      st == :unknown ->
        :ignore

      true ->
        :mnesia_lib.db_fixtable(st, tab, true)
    end

    {type, previous}
  end

  defp close_iteration(res, tab) do
    case val({tab, :storage_type}) do
      :unknown ->
        :ignore

      st ->
        :mnesia_lib.db_fixtable(st, tab, false)
    end

    case res do
      {:EXIT, {:aborted, what}} ->
        abort(what)

      {:EXIT, what} ->
        abort(what)

      _ ->
        res
    end
  end

  defp add_previous(_ActivityId, :non_transaction, _Type, _Tab) do
    []
  end

  defp add_previous(_Tid, ts, _Type, tab) do
    previous =
      :ets.match(
        r_tidstore(ts, :store),
        {{tab, :"$1"}, :_, :write}
      )

    :lists.sort(:lists.concat(previous))
  end

  defp add_written([], _Tab, objsFun, _LockKind) do
    objsFun.()
  end

  defp add_written(written, tab, objsFun, lockKind) do
    case val({tab, :setorbag}) do
      :bag ->
        add_written_to_bag(written, objsFun.(), [])

      _ when lockKind == :read or lockKind == :write ->
        add_written_to_set(written, objsFun)

      _ ->
        add_written_to_set(written, objsFun.())
    end
  end

  defp add_written_to_set(ws, objsOrFun) do
    case :lists.last(ws) do
      {_, _, :delete} ->
        []

      {_, val, :write} ->
        [val]

      {oid, _, :delete_object} ->
        for val <- get_objs(objsOrFun),
            not :lists.member({oid, val, :delete_object}, ws) do
          val
        end
    end
  end

  defp get_objs(objsFun) when is_function(objsFun) do
    objsFun.()
  end

  defp get_objs(objs) when is_list(objs) do
    objs
  end

  defp add_written_to_bag([{_, val, :write} | tail], objs, ack) do
    add_written_to_bag(tail, :lists.delete(val, objs), [val | ack])
  end

  defp add_written_to_bag([], objs, ack) do
    objs ++ :lists.reverse(ack)
  end

  defp add_written_to_bag([{_, _, :delete} | tail], _Objs, _Ack) do
    add_written_to_bag(tail, [], [])
  end

  defp add_written_to_bag([{_, val, :delete_object} | tail], objs, ack) do
    add_written_to_bag(tail, :lists.delete(val, objs), :lists.delete(val, ack))
  end

  def match_object(pat)
      when is_tuple(pat) and
             tuple_size(pat) > 2 do
    tab = :erlang.element(1, pat)
    match_object(tab, pat, :read)
  end

  def match_object(pat) do
    abort({:bad_type, pat})
  end

  def match_object(tab, pat, lockKind) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        match_object(tid, ts, tab, pat, lockKind)

      {mod, tid, ts} ->
        mod.match_object(tid, ts, tab, pat, lockKind)

      _ ->
        abort(:no_transaction)
    end
  end

  def match_object(tid, ts, tab, pat, lockKind)
      when is_atom(tab) and tab != :schema and
             is_tuple(pat) and tuple_size(pat) > 2 do
    case :erlang.element(1, tid) do
      :ets ->
        :mnesia_lib.db_match_object(:ram_copies, tab, pat)

      :tid ->
        key = :erlang.element(2, pat)

        case has_var(key) do
          false ->
            lock_record(tid, ts, tab, key, lockKind)

          true ->
            lock_table(tid, ts, tab, lockKind)
        end

        objs = dirty_match_object(tab, pat)
        add_written_match(r_tidstore(ts, :store), pat, tab, objs)

      _Protocol ->
        dirty_match_object(tab, pat)
    end
  end

  def match_object(_Tid, _Ts, tab, pat, _LockKind) do
    abort({:bad_type, tab, pat})
  end

  defp add_written_index(store, pos, tab, key, objs)
       when is_integer(pos) do
    pat = :erlang.setelement(pos, val({tab, :wild_pattern}), key)
    add_written_match(store, pat, tab, objs)
  end

  defp add_written_index(store, pos, tab, key, objs)
       when is_tuple(pos) do
    ixF = :mnesia_index.index_vals_f(val({tab, :storage_type}), tab, pos)
    ops = find_ops(store, tab, :_)
    add_ix_match(ops, objs, ixF, key, val({tab, :setorbag}))
  end

  defp add_written_match(s, pat, tab, objs) do
    ops = find_ops(s, tab, pat)
    fixedRes = add_match(ops, objs, val({tab, :setorbag}))
    mS = :ets.match_spec_compile([{pat, [], [:"$_"]}])
    :ets.match_spec_run(fixedRes, mS)
  end

  defp find_ops(s, tab, pat) do
    getWritten = [
      {{{tab, :_}, :_, :write}, [], [:"$_"]},
      {{{tab, :_}, :_, :delete}, [], [:"$_"]},
      {{{tab, :_}, pat, :delete_object}, [], [:"$_"]}
    ]

    :ets.select(s, getWritten)
  end

  defp add_match([], objs, _Type) do
    objs
  end

  defp add_match(written, objs, :ordered_set) do
    add_ordered_match(:lists.keysort(1, written), objs, [])
  end

  defp add_match([{oid, _, :delete} | r], objs, type) do
    add_match(r, deloid(oid, objs), type)
  end

  defp add_match([{_Oid, val, :delete_object} | r], objs, type) do
    add_match(r, :lists.delete(val, objs), type)
  end

  defp add_match([{_Oid, val, :write} | r], objs, :bag) do
    add_match(r, [val | :lists.delete(val, objs)], :bag)
  end

  defp add_match([{oid, val, :write} | r], objs, :set) do
    add_match(r, [val | deloid(oid, objs)], :set)
  end

  defp add_ix_match([], objs, _IxF, _Key, _Type) do
    objs
  end

  defp add_ix_match(written, objs, ixF, key, :ordered_set) do
    add_ordered_match(
      :lists.keysort(
        1,
        ix_filter_ops(ixF, key, written)
      ),
      objs,
      []
    )
  end

  defp add_ix_match([{oid, _, :delete} | r], objs, ixF, key, type) do
    add_ix_match(r, deloid(oid, objs), ixF, key, type)
  end

  defp add_ix_match([{_Oid, val, :delete_object} | r], objs, ixF, key, type) do
    case ix_match(val, ixF, key) do
      true ->
        add_ix_match(r, :lists.delete(val, objs), ixF, key, type)

      false ->
        add_ix_match(r, objs, ixF, key, type)
    end
  end

  defp add_ix_match([{_Oid, val, :write} | r], objs, ixF, key, :bag) do
    case ix_match(val, ixF, key) do
      true ->
        add_ix_match(r, [val | :lists.delete(val, objs)], ixF, key, :bag)

      false ->
        add_ix_match(r, objs, ixF, key, :bag)
    end
  end

  defp add_ix_match([{oid, val, :write} | r], objs, ixF, key, :set) do
    case ix_match(val, ixF, key) do
      true ->
        add_ix_match(r, [val | deloid(oid, objs)], ixF, key, :set)

      false ->
        add_ix_match(r, objs, ixF, key, :set)
    end
  end

  defp ix_match(val, ixF, key) do
    :lists.member(key, ixF.(val))
  end

  defp ix_filter_ops(ixF, key, ops) do
    :lists.filter(
      fn
        {_Oid, obj, :write} ->
          ix_match(obj, ixF, key)

        _ ->
          true
      end,
      ops
    )
  end

  defp add_ordered_match(written = [{{_, key}, _, _} | _], [obj | objs], acc)
       when key > :erlang.element(2, obj) do
    add_ordered_match(written, objs, [obj | acc])
  end

  defp add_ordered_match([{{_, key}, val, :write} | rest], objs = [obj | _], acc)
       when key < :erlang.element(2, obj) do
    add_ordered_match(rest, [val | objs], acc)
  end

  defp add_ordered_match([{{_, key}, _, _DelOP} | rest], objs = [obj | _], acc)
       when key < :erlang.element(2, obj) do
    add_ordered_match(rest, objs, acc)
  end

  defp add_ordered_match([{_, val, :write} | rest], [], acc) do
    add_ordered_match(rest, [val], acc)
  end

  defp add_ordered_match([_ | rest], [], acc) do
    add_ordered_match(rest, [], acc)
  end

  defp add_ordered_match([{_, val, :write} | rest], [_Obj | objs], acc) do
    add_ordered_match(rest, [val | objs], acc)
  end

  defp add_ordered_match([{_, _Val, :delete} | rest], [_Obj | objs], acc) do
    add_ordered_match(rest, objs, acc)
  end

  defp add_ordered_match([{_, val, :delete_object} | rest], [val | objs], acc) do
    add_ordered_match(rest, objs, acc)
  end

  defp add_ordered_match([{_, _, :delete_object} | rest], objs, acc) do
    add_ordered_match(rest, objs, acc)
  end

  defp add_ordered_match([], objs, acc) do
    :lists.reverse(acc, objs)
  end

  defp add_sel_match(sorted, objs, :ordered_set) do
    add_sel_ordered_match(sorted, objs, [])
  end

  defp add_sel_match(written, objs, type) do
    add_sel_match(written, objs, type, [])
  end

  defp add_sel_match([], objs, _Type, acc) do
    {objs, :lists.reverse(acc)}
  end

  defp add_sel_match([op = {oid, _, :delete} | r], objs, type, acc) do
    case deloid(oid, objs) do
      ^objs ->
        add_sel_match(r, objs, type, [op | acc])

      newObjs when type == :set ->
        add_sel_match(r, newObjs, type, acc)

      newObjs ->
        add_sel_match(r, newObjs, type, [op | acc])
    end
  end

  defp add_sel_match([op = {_Oid, val, :delete_object} | r], objs, type, acc) do
    case :lists.delete(val, objs) do
      ^objs ->
        add_sel_match(r, objs, type, [op | acc])

      newObjs when type == :set ->
        add_sel_match(r, newObjs, type, acc)

      newObjs ->
        add_sel_match(r, newObjs, type, [op | acc])
    end
  end

  defp add_sel_match([op = {oid = {_, key}, val, :write} | r], objs, :bag, acc) do
    case :lists.keymember(key, 2, objs) do
      true ->
        add_sel_match(r, [val | :lists.delete(val, objs)], :bag, [
          {oid, val, :delete_object} | acc
        ])

      false ->
        add_sel_match(r, objs, :bag, [op | acc])
    end
  end

  defp add_sel_match([op = {oid, val, :write} | r], objs, :set, acc) do
    case deloid(oid, objs) do
      ^objs ->
        add_sel_match(r, objs, :set, [op | acc])

      newObjs ->
        add_sel_match(r, [val | newObjs], :set, acc)
    end
  end

  defp add_sel_ordered_match(written = [{{_, key}, _, _} | _], [obj | objs], acc)
       when key > :erlang.element(2, obj) do
    add_sel_ordered_match(written, objs, [obj | acc])
  end

  defp add_sel_ordered_match([{{_, key}, val, :write} | rest], objs = [obj | _], acc)
       when key < :erlang.element(2, obj) do
    add_sel_ordered_match(rest, [val | objs], acc)
  end

  defp add_sel_ordered_match([{{_, key}, _, _DelOP} | rest], objs = [obj | _], acc)
       when key < :erlang.element(2, obj) do
    add_sel_ordered_match(rest, objs, acc)
  end

  defp add_sel_ordered_match(ops1, [], acc) do
    {:lists.reverse(acc), ops1}
  end

  defp add_sel_ordered_match([{_, val, :write} | rest], [_Obj | objs], acc) do
    add_sel_ordered_match(rest, [val | objs], acc)
  end

  defp add_sel_ordered_match([{_, _Val, :delete} | rest], [_Obj | objs], acc) do
    add_sel_ordered_match(rest, objs, acc)
  end

  defp add_sel_ordered_match([{_, val, :delete_object} | rest], [val | objs], acc) do
    add_sel_ordered_match(rest, objs, acc)
  end

  defp add_sel_ordered_match([{_, _, :delete_object} | rest], objs, acc) do
    add_sel_ordered_match(rest, objs, acc)
  end

  defp add_sel_ordered_match([], objs, acc) do
    {:lists.reverse(acc, objs), []}
  end

  defp deloid(_Oid, []) do
    []
  end

  defp deloid({tab, key}, [h | t])
       when :erlang.element(
              2,
              h
            ) == key do
    deloid({tab, key}, t)
  end

  defp deloid(oid, [h | t]) do
    [h | deloid(oid, t)]
  end

  def select(tab, pat) do
    select(tab, pat, :read)
  end

  def select(tab, pat, lockKind)
      when is_atom(tab) and
             tab != :schema and is_list(pat) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        select(tid, ts, tab, pat, lockKind)

      {mod, tid, ts} ->
        mod.select(tid, ts, tab, pat, lockKind)

      _ ->
        abort(:no_transaction)
    end
  end

  def select(tab, pat, _Lock) do
    abort({:badarg, tab, pat})
  end

  def select(tid, ts, tab, spec, lockKind) do
    selectFun = fn fixedSpec ->
      dirty_select(tab, fixedSpec)
    end

    fun_select(tid, ts, tab, spec, lockKind, tab, selectFun)
  end

  def fun_select(tid, ts, tab, spec, lockKind, tabPat, selectFun) do
    case :erlang.element(1, tid) do
      :ets ->
        :mnesia_lib.db_select(:ram_copies, tab, spec)

      :tid ->
        select_lock(tid, ts, lockKind, spec, tab)
        store = r_tidstore(ts, :store)

        written =
          :ets.match_object(
            store,
            {{tabPat, :_}, :_, :_}
          )

        case written do
          [] ->
            selectFun.(spec)

          _ ->
            type = val({tab, :setorbag})
            fixedSpec = get_record_pattern(spec)
            tabRecs = selectFun.(fixedSpec)
            fixedRes = add_match(written, tabRecs, type)
            cMS = :ets.match_spec_compile(spec)
            :ets.match_spec_run(fixedRes, cMS)
        end

      _Protocol ->
        selectFun.(spec)
    end
  end

  defp select_lock(tid, ts, lockKind, spec, tab) do
    case spec do
      [{headPat, _, _}]
      when is_tuple(headPat) and
             tuple_size(headPat) > 2 ->
        key = :erlang.element(2, headPat)

        case has_var(key) do
          false ->
            lock_record(tid, ts, tab, key, lockKind)

          true ->
            lock_table(tid, ts, tab, lockKind)
        end

      _ ->
        lock_table(tid, ts, tab, lockKind)
    end
  end

  def select(tab, pat, nObjects, lockKind)
      when is_atom(tab) and tab != :schema and
             is_list(pat) and is_integer(nObjects) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        select(tid, ts, tab, pat, nObjects, lockKind)

      {mod, tid, ts} ->
        mod.select(tid, ts, tab, pat, nObjects, lockKind)

      _ ->
        abort(:no_transaction)
    end
  end

  def select(tab, pat, nObjects, _Lock) do
    abort({:badarg, tab, pat, nObjects})
  end

  def select(tid, ts, tab, spec, nObjects, lockKind) do
    where = val({tab, :where_to_read})
    type = :mnesia_lib.storage_type_at_node(where, tab)

    initFun = fn fixedSpec ->
      dirty_sel_init(where, tab, fixedSpec, nObjects, type)
    end

    fun_select(tid, ts, tab, spec, lockKind, tab, initFun, nObjects, where, type)
  end

  Record.defrecord(:r_mnesia_select, :mnesia_select,
    tab: :undefined,
    tid: :undefined,
    node: :undefined,
    storage: :undefined,
    cont: :undefined,
    written: [],
    spec: :undefined,
    type: :undefined,
    orig: :undefined
  )

  def fun_select(tid, ts, tab, spec, lockKind, tabPat, init, nObjects, node, storage) do
    def__ = r_mnesia_select(tid: tid, node: node, storage: storage, tab: tab, orig: spec)

    case :erlang.element(1, tid) do
      :ets ->
        select_state(
          :mnesia_lib.db_select_init(:ram_copies, tab, spec, nObjects),
          def__
        )

      :tid ->
        select_lock(tid, ts, lockKind, spec, tab)
        store = r_tidstore(ts, :store)
        do_fixtable(tab, store)

        written0 =
          :ets.match_object(
            store,
            {{tabPat, :_}, :_, :_}
          )

        case written0 do
          [] ->
            select_state(init.(spec), def__)

          _ ->
            type = val({tab, :setorbag})

            written =
              cond do
                type == :ordered_set ->
                  :lists.keysort(1, written0)

                true ->
                  written0
              end

            fixedSpec = get_record_pattern(spec)
            cMS = :ets.match_spec_compile(spec)

            trans_select(
              init.(fixedSpec),
              r_mnesia_select(def__, written: written, spec: cMS, type: type, orig: fixedSpec)
            )
        end

      _Protocol ->
        select_state(init.(spec), def__)
    end
  end

  def select(cont) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        select_cont(tid, ts, cont)

      {mod, tid, ts} ->
        mod.select_cont(tid, ts, cont)

      _ ->
        abort(:no_transaction)
    end
  end

  def select_cont(_Tid, _Ts, :"$end_of_table") do
    :"$end_of_table"
  end

  def select_cont(tid, _Ts, state = r_mnesia_select(tid: tid, cont: cont, orig: ms))
      when :erlang.element(1, tid) == :ets do
    case cont do
      :"$end_of_table" ->
        :"$end_of_table"

      _ ->
        select_state(
          :mnesia_lib.db_select_cont(:ram_copies, cont, ms),
          state
        )
    end
  end

  def select_cont(tid, _, state = r_mnesia_select(tid: tid, written: [])) do
    select_state(dirty_sel_cont(state), state)
  end

  def select_cont(tid, _Ts, state = r_mnesia_select(tid: tid)) do
    trans_select(dirty_sel_cont(state), state)
  end

  def select_cont(tid2, _, r_mnesia_select(tid: _Tid1))
      when :erlang.element(
             1,
             tid2
           ) == :tid do
    abort(:wrong_transaction)
  end

  def select_cont(tid, ts, state = r_mnesia_select()) do
    repairedState =
      r_mnesia_select(state, tid: tid, written: [], spec: :undefined, type: :undefined)

    select_cont(tid, ts, repairedState)
  end

  def select_cont(_, _, cont) do
    abort({:badarg, cont})
  end

  defp trans_select(
         :"$end_of_table",
         r_mnesia_select(written: written0, spec: cMS, type: type)
       ) do
    written = add_match(written0, [], type)
    {:ets.match_spec_run(written, cMS), :"$end_of_table"}
  end

  defp trans_select(
         {tabRecs, cont},
         state = r_mnesia_select(written: written0, spec: cMS, type: type)
       ) do
    {fixedRes, written} = add_sel_match(written0, tabRecs, type)

    select_state(
      {:ets.match_spec_run(fixedRes, cMS), cont},
      r_mnesia_select(state, written: written)
    )
  end

  defp select_state({matches, cont}, mS) do
    {matches, r_mnesia_select(mS, cont: cont)}
  end

  defp select_state(:"$end_of_table", _) do
    :"$end_of_table"
  end

  defp get_record_pattern([]) do
    []
  end

  defp get_record_pattern([{m, c, _B} | r]) do
    [{m, c, [:"$_"]} | get_record_pattern(r)]
  end

  def all_keys(tab) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        all_keys(tid, ts, tab, :read)

      {mod, tid, ts} ->
        mod.all_keys(tid, ts, tab, :read)

      _ ->
        abort(:no_transaction)
    end
  end

  def all_keys(tid, ts, tab, lockKind)
      when is_atom(tab) and
             tab != :schema do
    pat0 = val({tab, :wild_pattern})
    pat = :erlang.setelement(2, pat0, :"$1")
    keys = select(tid, ts, tab, [{pat, [], [:"$1"]}], lockKind)

    case val({tab, :setorbag}) do
      :bag ->
        :mnesia_lib.uniq(keys)

      _ ->
        keys
    end
  end

  def all_keys(_Tid, _Ts, tab, _LockKind) do
    abort({:bad_type, tab})
  end

  def index_match_object(pat, attr)
      when is_tuple(pat) and
             tuple_size(pat) > 2 do
    tab = :erlang.element(1, pat)
    index_match_object(tab, pat, attr, :read)
  end

  def index_match_object(pat, _Attr) do
    abort({:bad_type, pat})
  end

  def index_match_object(tab, pat, attr, lockKind) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        index_match_object(tid, ts, tab, pat, attr, lockKind)

      {mod, tid, ts} ->
        mod.index_match_object(tid, ts, tab, pat, attr, lockKind)

      _ ->
        abort(:no_transaction)
    end
  end

  def index_match_object(tid, ts, tab, pat, attr, lockKind)
      when is_atom(tab) and tab != :schema and
             is_tuple(pat) and tuple_size(pat) > 2 do
    case :erlang.element(1, tid) do
      :ets ->
        dirty_index_match_object(tab, pat, attr)

      :tid ->
        case :mnesia_schema.attr_tab_to_pos(tab, attr) do
          {_} ->
            case lockKind do
              :read ->
                store = r_tidstore(ts, :store)
                :mnesia_locker.rlock_table(tid, store, tab)
                objs = dirty_match_object(tab, pat)
                add_written_match(store, pat, tab, objs)

              _ ->
                abort({:bad_type, tab, lockKind})
            end

          pos when pos <= tuple_size(pat) ->
            case lockKind do
              :read ->
                store = r_tidstore(ts, :store)
                :mnesia_locker.rlock_table(tid, store, tab)
                objs = dirty_index_match_object(tab, pat, attr)
                add_written_match(store, pat, tab, objs)

              _ ->
                abort({:bad_type, tab, lockKind})
            end

          badPos ->
            abort({:bad_type, tab, badPos})
        end

      _Protocol ->
        dirty_index_match_object(tab, pat, attr)
    end
  end

  def index_match_object(_Tid, _Ts, tab, pat, _Attr, _LockKind) do
    abort({:bad_type, tab, pat})
  end

  def index_read(tab, key, attr) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        index_read(tid, ts, tab, key, attr, :read)

      {mod, tid, ts} ->
        mod.index_read(tid, ts, tab, key, attr, :read)

      _ ->
        abort(:no_transaction)
    end
  end

  def index_read(tid, ts, tab, key, attr, lockKind)
      when is_atom(tab) and tab != :schema do
    case :erlang.element(1, tid) do
      :ets ->
        dirty_index_read(tab, key, attr)

      :tid ->
        pos = :mnesia_schema.attr_tab_to_pos(tab, attr)

        case lockKind do
          :read ->
            case has_var(key) do
              false ->
                store = r_tidstore(ts, :store)
                objs = :mnesia_index.read(tid, store, tab, key, pos)
                add_written_index(r_tidstore(ts, :store), pos, tab, key, objs)

              true ->
                abort({:bad_type, tab, attr, key})
            end

          _ ->
            abort({:bad_type, tab, lockKind})
        end

      _Protocol ->
        dirty_index_read(tab, key, attr)
    end
  end

  def index_read(_Tid, _Ts, tab, _Key, _Attr, _LockKind) do
    abort({:bad_type, tab})
  end

  def dirty_write(val)
      when is_tuple(val) and
             tuple_size(val) > 2 do
    tab = :erlang.element(1, val)
    dirty_write(tab, val)
  end

  def dirty_write(val) do
    abort({:bad_type, val})
  end

  def dirty_write(tab, val) do
    do_dirty_write(:async_dirty, tab, val)
  end

  defp do_dirty_write(syncMode, tab, val)
       when is_atom(tab) and
              tab != :schema and is_tuple(val) and
              tuple_size(val) > 2 do
    {_, _, _} = :mnesia_lib.validate_record(tab, val)
    oid = {tab, :erlang.element(2, val)}
    :mnesia_tm.dirty(syncMode, {oid, val, :write})
  end

  defp do_dirty_write(_SyncMode, tab, val) do
    abort({:bad_type, tab, val})
  end

  def dirty_delete({tab, key}) do
    dirty_delete(tab, key)
  end

  def dirty_delete(oid) do
    abort({:bad_type, oid})
  end

  def dirty_delete(tab, key) do
    do_dirty_delete(:async_dirty, tab, key)
  end

  defp do_dirty_delete(syncMode, tab, key)
       when is_atom(tab) and
              tab != :schema do
    oid = {tab, key}
    :mnesia_tm.dirty(syncMode, {oid, oid, :delete})
  end

  defp do_dirty_delete(_SyncMode, tab, _Key) do
    abort({:bad_type, tab})
  end

  def dirty_delete_object(val)
      when is_tuple(val) and
             tuple_size(val) > 2 do
    tab = :erlang.element(1, val)
    dirty_delete_object(tab, val)
  end

  def dirty_delete_object(val) do
    abort({:bad_type, val})
  end

  def dirty_delete_object(tab, val) do
    do_dirty_delete_object(:async_dirty, tab, val)
  end

  defp do_dirty_delete_object(syncMode, tab, val)
       when is_atom(tab) and
              tab != :schema and is_tuple(val) and
              tuple_size(val) > 2 do
    oid = {tab, :erlang.element(2, val)}

    case has_var(val) do
      false ->
        :mnesia_tm.dirty(syncMode, {oid, val, :delete_object})

      true ->
        abort({:bad_type, tab, val})
    end
  end

  defp do_dirty_delete_object(_SyncMode, tab, val) do
    abort({:bad_type, tab, val})
  end

  def dirty_update_counter({tab, key}, incr) do
    dirty_update_counter(tab, key, incr)
  end

  def dirty_update_counter(counter, _Incr) do
    abort({:bad_type, counter})
  end

  def dirty_update_counter(tab, key, incr) do
    do_dirty_update_counter(:async_dirty, tab, key, incr)
  end

  defp do_dirty_update_counter(syncMode, tab, key, incr)
       when is_atom(tab) and
              tab != :schema and
              is_integer(incr) do
    case :mnesia_lib.validate_key(tab, key) do
      {recName, 3, type}
      when type == :set or
             type == :ordered_set ->
        oid = {tab, key}

        :mnesia_tm.dirty(
          syncMode,
          {oid, {recName, incr}, :update_counter}
        )

      _ ->
        abort({:combine_error, tab, :update_counter})
    end
  end

  defp do_dirty_update_counter(_SyncMode, tab, _Key, incr) do
    abort({:bad_type, tab, incr})
  end

  def dirty_read({tab, key}) do
    dirty_read(tab, key)
  end

  def dirty_read(oid) do
    abort({:bad_type, oid})
  end

  def dirty_read(tab, key)
      when is_atom(tab) and
             tab != :schema do
    dirty_rpc(tab, :mnesia_lib, :db_get, [tab, key])
  end

  def dirty_read(tab, _Key) do
    abort({:bad_type, tab})
  end

  def dirty_match_object(pat)
      when is_tuple(pat) and
             tuple_size(pat) > 2 do
    tab = :erlang.element(1, pat)
    dirty_match_object(tab, pat)
  end

  def dirty_match_object(pat) do
    abort({:bad_type, pat})
  end

  def dirty_match_object(tab, pat)
      when is_atom(tab) and
             tab != :schema and is_tuple(pat) and
             tuple_size(pat) > 2 do
    dirty_rpc(tab, :mnesia, :remote_dirty_match_object, [tab, pat])
  end

  def dirty_match_object(tab, pat) do
    abort({:bad_type, tab, pat})
  end

  def remote_dirty_match_object(tab, pat) do
    key = :erlang.element(2, pat)

    case has_var(key) do
      false ->
        :mnesia_lib.db_match_object(tab, pat)

      true ->
        posList = regular_indexes(tab)
        remote_dirty_match_object(tab, pat, posList)
    end
  end

  defp remote_dirty_match_object(tab, pat, [pos | tail])
       when pos <= tuple_size(pat) do
    ixKey = :erlang.element(pos, pat)

    case has_var(ixKey) do
      false ->
        :mnesia_index.dirty_match_object(tab, pat, pos)

      true ->
        remote_dirty_match_object(tab, pat, tail)
    end
  end

  defp remote_dirty_match_object(tab, pat, []) do
    :mnesia_lib.db_match_object(tab, pat)
  end

  defp remote_dirty_match_object(tab, pat, _PosList) do
    abort({:bad_type, tab, pat})
  end

  def dirty_select(tab, spec)
      when is_atom(tab) and
             tab != :schema and is_list(spec) do
    dirty_rpc(tab, :mnesia, :remote_dirty_select, [tab, spec])
  end

  def dirty_select(tab, spec) do
    abort({:bad_type, tab, spec})
  end

  def remote_dirty_select(tab, spec) do
    case spec do
      [{headPat, _, _}]
      when is_tuple(headPat) and
             tuple_size(headPat) > 2 ->
        key = :erlang.element(2, headPat)

        case has_var(key) do
          false ->
            :mnesia_lib.db_select(tab, spec)

          true ->
            posList = regular_indexes(tab)
            remote_dirty_select(tab, spec, posList)
        end

      _ ->
        :mnesia_lib.db_select(tab, spec)
    end
  end

  defp remote_dirty_select(tab, [{headPat, _, _}] = spec, [pos | tail])
       when is_tuple(headPat) and tuple_size(headPat) > 2 and
              pos <= tuple_size(headPat) do
    key = :erlang.element(pos, headPat)

    case has_var(key) do
      false ->
        recs = :mnesia_index.dirty_select(tab, headPat, pos)
        cMS = :ets.match_spec_compile(spec)

        case val({tab, :setorbag}) do
          :ordered_set ->
            :ets.match_spec_run(:lists.sort(recs), cMS)

          _ ->
            :ets.match_spec_run(recs, cMS)
        end

      true ->
        remote_dirty_select(tab, spec, tail)
    end
  end

  defp remote_dirty_select(tab, spec, _) do
    :mnesia_lib.db_select(tab, spec)
  end

  def dirty_sel_init(node, tab, spec, nObjects, type) do
    do_dirty_rpc(tab, node, :mnesia_lib, :db_select_init, [type, tab, spec, nObjects])
  end

  defp dirty_sel_cont(r_mnesia_select(cont: :"$end_of_table")) do
    :"$end_of_table"
  end

  defp dirty_sel_cont(r_mnesia_select(node: node, tab: tab, storage: type, cont: cont, orig: ms)) do
    do_dirty_rpc(tab, node, :mnesia_lib, :db_select_cont, [type, cont, ms])
  end

  def dirty_all_keys(tab) when is_atom(tab) and tab != :schema do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :wild_pattern}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        abort({:no_exists, tab})

      pat0 ->
        pat = :erlang.setelement(2, pat0, :"$1")
        keys = dirty_select(tab, [{pat, [], [:"$1"]}])

        case val({tab, :setorbag}) do
          :bag ->
            :mnesia_lib.uniq(keys)

          _ ->
            keys
        end
    end
  end

  def dirty_all_keys(tab) do
    abort({:bad_type, tab})
  end

  def dirty_index_match_object(pat, attr)
      when is_tuple(pat) and
             tuple_size(pat) > 2 do
    tab = :erlang.element(1, pat)
    dirty_index_match_object(tab, pat, attr)
  end

  def dirty_index_match_object(pat, _Attr) do
    abort({:bad_type, pat})
  end

  def dirty_index_match_object(tab, pat, attr)
      when is_atom(tab) and
             tab != :schema and is_tuple(pat) and
             tuple_size(pat) > 2 do
    case :mnesia_schema.attr_tab_to_pos(tab, attr) do
      {_} ->
        dirty_match_object(tab, pat)

      pos when pos <= tuple_size(pat) ->
        case has_var(:erlang.element(2, pat)) do
          false ->
            dirty_match_object(tab, pat)

          true ->
            elem = :erlang.element(pos, pat)

            case has_var(elem) do
              false ->
                dirty_rpc(tab, :mnesia_index, :dirty_match_object, [tab, pat, pos])

              true ->
                abort({:bad_type, tab, attr, elem})
            end
        end

      badPos ->
        abort({:bad_type, tab, badPos})
    end
  end

  def dirty_index_match_object(tab, pat, _Attr) do
    abort({:bad_type, tab, pat})
  end

  def dirty_index_read(tab, key, attr)
      when is_atom(tab) and
             tab != :schema do
    pos = :mnesia_schema.attr_tab_to_pos(tab, attr)

    case has_var(key) do
      false ->
        :mnesia_index.dirty_read(tab, key, pos)

      true ->
        abort({:bad_type, tab, attr, key})
    end
  end

  def dirty_index_read(tab, _Key, _Attr) do
    abort({:bad_type, tab})
  end

  def dirty_slot(tab, slot)
      when is_atom(tab) and
             tab != :schema and is_integer(slot) do
    dirty_rpc(tab, :mnesia_lib, :db_slot, [tab, slot])
  end

  def dirty_slot(tab, slot) do
    abort({:bad_type, tab, slot})
  end

  def dirty_first(tab) when is_atom(tab) and tab != :schema do
    dirty_rpc(tab, :mnesia_lib, :db_first, [tab])
  end

  def dirty_first(tab) do
    abort({:bad_type, tab})
  end

  def dirty_last(tab) when is_atom(tab) and tab != :schema do
    dirty_rpc(tab, :mnesia_lib, :db_last, [tab])
  end

  def dirty_last(tab) do
    abort({:bad_type, tab})
  end

  def dirty_next(tab, key)
      when is_atom(tab) and
             tab != :schema do
    dirty_rpc(tab, :mnesia_lib, :db_next_key, [tab, key])
  end

  def dirty_next(tab, _Key) do
    abort({:bad_type, tab})
  end

  def dirty_prev(tab, key)
      when is_atom(tab) and
             tab != :schema do
    dirty_rpc(tab, :mnesia_lib, :db_prev_key, [tab, key])
  end

  def dirty_prev(tab, _Key) do
    abort({:bad_type, tab})
  end

  def dirty_rpc(tab, m, f, args) do
    node = val({tab, :where_to_read})
    do_dirty_rpc(tab, node, m, f, args)
  end

  defp do_dirty_rpc(_Tab, :nowhere, _, _, args) do
    :mnesia.abort({:no_exists, args})
  end

  defp do_dirty_rpc(_Tab, local, m, f, args)
       when local === node() do
    try do
      apply(m, f, args)
    catch
      res ->
        res

      _, _ ->
        :mnesia.abort({:badarg, args})
    end
  end

  defp do_dirty_rpc(tab, node, m, f, args) do
    case :mnesia_rpc.call(node, m, f, args) do
      {:badrpc, reason} ->
        :timer.sleep(20)

        _ =
          try do
            :sys.get_status(:mnesia_monitor)
          catch
            _, _ ->
              :ok
          end

        case :mnesia_controller.call({:check_w2r, node, tab}) do
          newNode when newNode === node ->
            errorTag = :mnesia_lib.dirty_rpc_error_tag(reason)
            :mnesia.abort({errorTag, args})

          newNode ->
            case :erlang.get(:mnesia_activity_state) do
              {_Mod, tid, _Ts} when elem(tid, 0) === :tid ->
                :mnesia.abort({:node_not_running, node})

              {:error, {:node_not_running, _}} ->
                :mnesia.abort({:no_exists, args})

              _ ->
                do_dirty_rpc(tab, newNode, m, f, args)
            end
        end

      other ->
        other
    end
  end

  def table_info(tab, item) do
    case :erlang.get(:mnesia_activity_state) do
      :undefined ->
        any_table_info(tab, item)

      {:mnesia, _Tid, _Ts} ->
        any_table_info(tab, item)

      {mod, tid, ts} ->
        mod.table_info(tid, ts, tab, item)

      _ ->
        abort(:no_transaction)
    end
  end

  def table_info(_Tid, _Ts, tab, item) do
    any_table_info(tab, item)
  end

  defp any_table_info(tab, item) when is_atom(tab) do
    case item do
      :master_nodes ->
        :mnesia_recover.get_master_nodes(tab)

      :size ->
        raw_table_info(tab, item)

      :memory ->
        raw_table_info(tab, item)

      :type ->
        case (try do
                :ets.lookup_element(:mnesia_gvar, {tab, :setorbag}, 2)
              catch
                :error, _ ->
                  {:EXIT, {:badarg, []}}
              end) do
          {:EXIT, _} ->
            abort({:no_exists, tab, item})

          val ->
            val
        end

      :all ->
        case :mnesia_schema.get_table_properties(tab) do
          [] ->
            abort({:no_exists, tab, item})

          props ->
            rename = fn
              {:setorbag, type} ->
                {:type, type}

              prop ->
                prop
            end

            :lists.sort(:lists.map(rename, props))
        end

      :name ->
        tab

      _ ->
        case (try do
                :ets.lookup_element(:mnesia_gvar, {tab, item}, 2)
              catch
                :error, _ ->
                  {:EXIT, {:badarg, []}}
              end) do
          {:EXIT, _} ->
            abort({:no_exists, tab, item})

          val ->
            val
        end
    end
  end

  defp any_table_info(tab, _Item) do
    abort({:bad_type, tab})
  end

  def raw_table_info(tab, item) do
    try do
      case :ets.lookup_element(:mnesia_gvar, {tab, :storage_type}, 2) do
        :ram_copies ->
          info_reply(:ets.info(tab, item), tab, item)

        :disc_copies ->
          info_reply(:ets.info(tab, item), tab, item)

        :disc_only_copies ->
          info_reply(:dets.info(tab, item), tab, item)

        {:ext, alias, mod} ->
          info_reply(mod.info(alias, tab, item), tab, item)

        :unknown ->
          bad_info_reply(tab, item)
      end
    catch
      :error, _ ->
        bad_info_reply(tab, item)
    end
  end

  defp info_reply({:error, _Reason}, tab, item) do
    bad_info_reply(tab, item)
  end

  defp info_reply(val, _Tab, _Item) do
    val
  end

  defp bad_info_reply(_Tab, :size) do
    0
  end

  defp bad_info_reply(_Tab, :memory) do
    0
  end

  defp bad_info_reply(tab, item) do
    abort({:no_exists, tab, item})
  end

  def schema() do
    :mnesia_schema.info()
  end

  def schema(tab) do
    :mnesia_schema.info(tab)
  end

  def error_description(err) do
    :mnesia_lib.error_desc(err)
  end

  def info() do
    case :mnesia_lib.is_running() do
      :yes ->
        tmInfo = :mnesia_tm.get_info(10000)
        held = system_info(:held_locks)
        queued = system_info(:lock_queue)
        :io.format('---> Processes holding locks <--- ~n', [])

        :lists.foreach(
          fn l ->
            :io.format('Lock: ~p~n', [l])
          end,
          held
        )

        :io.format('---> Processes waiting for locks <--- ~n', [])

        :lists.foreach(
          fn {oid, op, _Pid, tid, ownerTid} ->
            :io.format('Tid ~p waits for ~p lock on oid ~p owned by ~p ~n', [
              tid,
              op,
              oid,
              ownerTid
            ])
          end,
          queued
        )

        :mnesia_tm.display_info(:erlang.group_leader(), tmInfo)
        pat = {:_, :unclear, :_}
        uncertain = :ets.match_object(:mnesia_decision, pat)
        :io.format('---> Uncertain transactions <--- ~n', [])

        :lists.foreach(
          fn {tid, _, nodes} ->
            :io.format('Tid ~w waits for decision from ~w~n', [tid, nodes])
          end,
          uncertain
        )

        :mnesia_controller.info()
        display_system_info(held, queued, tmInfo, uncertain)

      _ ->
        mini_info()
    end

    :ok
  end

  defp mini_info() do
    :io.format(
      '===> System info in version ~p, debug level = ~p <===~n',
      [system_info(:version), system_info(:debug)]
    )

    not__ =
      case system_info(:use_dir) do
        true ->
          ''

        false ->
          'NOT '
      end

    :io.format(
      '~w. Directory ~p is ~sused.~n',
      [system_info(:schema_location), system_info(:directory), not__]
    )

    :io.format('use fallback at restart = ~w~n', [system_info(:fallback_activated)])
    running = system_info(:running_db_nodes)
    :io.format('running db nodes   = ~w~n', [running])
    all = :mnesia_lib.all_nodes()
    :io.format('stopped db nodes   = ~w ~n', [all -- running])
  end

  defp display_system_info(held, queued, tmInfo, uncertain) do
    mini_info()
    display_tab_info()

    s = fn items ->
      for i <- items do
        system_info(i)
      end
    end

    :io.format(
      '~w transactions committed, ~w aborted, ~w restarted, ~w logged to disc~n',
      s.([
        :transaction_commits,
        :transaction_failures,
        :transaction_restarts,
        :transaction_log_writes
      ])
    )

    {active, pending} =
      case tmInfo do
        {:timeout, _} ->
          {:infinity, :infinity}

        {:info, p, a} ->
          {length(a), length(p)}
      end

    :io.format(
      '~w held locks, ~w in queue; ~w local transactions, ~w remote~n',
      [length(held), length(queued), active, pending]
    )

    ufold = fn {_, _, ns}, {c, old} ->
      new =
        for n <- ns, not :lists.member(n, old) do
          n
        end

      {c + 1, new ++ old}
    end

    {ucount, unodes} = :lists.foldl(ufold, {0, []}, uncertain)
    :io.format('~w transactions waits for other nodes: ~p~n', [ucount, unodes])
  end

  defp display_tab_info() do
    masterTabs = :mnesia_recover.get_master_node_tables()
    :io.format('master node tables = ~p~n', [:lists.sort(masterTabs)])

    case get_backend_types() do
      [] ->
        :ok

      ts ->
        list_backend_types(ts, 'backend types      = ')
    end

    case get_index_plugins() do
      [] ->
        :ok

      ps ->
        list_index_plugins(ps, 'index plugins      = ')
    end

    tabs = system_info(:tables)

    {unknown, ram, disc, discOnly, ext} =
      :lists.foldl(&storage_count/2, {[], [], [], [], []}, tabs)

    :io.format('remote             = ~p~n', [:lists.sort(unknown)])
    :io.format('ram_copies         = ~p~n', [:lists.sort(ram)])
    :io.format('disc_copies        = ~p~n', [:lists.sort(disc)])
    :io.format('disc_only_copies   = ~p~n', [:lists.sort(discOnly)])

    for {a, ts} <- ext do
      :io.format('~-19s= ~p~n', [:erlang.atom_to_list(a), ts])
    end

    rfoldl = fn t, acc ->
      rpat =
        case val({t, :access_mode}) do
          :read_only ->
            :lists.sort(
              for a <- val({t, :active_replicas}) do
                {a, :read_only}
              end
            )

          :read_write ->
            for w <- table_info(t, :where_to_commit) do
              fix_wtc(w)
            end
        end

      case :lists.keysearch(rpat, 1, acc) do
        {:value, {_Rpat, rtabs}} ->
          :lists.keyreplace(rpat, 1, acc, {rpat, [t | rtabs]})

        false ->
          [{rpat, [t]} | acc]
      end
    end

    repl = :lists.foldl(rfoldl, [], tabs)

    rdisp = fn {rpat, rtabs} ->
      :io.format('~p = ~p~n', [rpat, rtabs])
    end

    :lists.foreach(rdisp, :lists.sort(repl))
  end

  defp get_backend_types() do
    case (try do
            :ets.lookup_element(
              :mnesia_gvar,
              {:schema, :user_property, :mnesia_backend_types},
              2
            )
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        []

      {:mnesia_backend_types, ts} ->
        :lists.sort(ts)
    end
  end

  defp get_index_plugins() do
    case (try do
            :ets.lookup_element(
              :mnesia_gvar,
              {:schema, :user_property, :mnesia_index_plugins},
              2
            )
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        []

      {:mnesia_index_plugins, ps} ->
        :lists.sort(ps)
    end
  end

  defp list_backend_types([{a, m} | t] = ts, legend) do
    indent =
      for _ <- legend do
        ?\s
      end

    w =
      :erlang.integer_to_list(
        :lists.foldl(
          fn {alias, _}, wa ->
            :erlang.max(
              wa,
              length(:erlang.atom_to_list(alias))
            )
          end,
          0,
          ts
        )
      )

    :io.fwrite(
      legend ++ '~-' ++ w ++ 's - ~s~n',
      [:erlang.atom_to_list(a), :erlang.atom_to_list(m)]
    )

    for {a1, m1} <- t do
      :io.fwrite(
        indent ++ '~-' ++ w ++ 's - ~s~n',
        [:erlang.atom_to_list(a1), :erlang.atom_to_list(m1)]
      )
    end
  end

  defp list_index_plugins([{n, m, f} | t] = ps, legend) do
    indent =
      for _ <- legend do
        ?\s
      end

    w =
      :erlang.integer_to_list(
        :lists.foldl(
          fn {n1, _, _}, wa ->
            :erlang.max(
              wa,
              length(pp_ix_name(n1))
            )
          end,
          0,
          ps
        )
      )

    :io.fwrite(
      legend ++ '~-' ++ w ++ 's - ~s:~ts~n',
      [pp_ix_name(n), :erlang.atom_to_list(m), :erlang.atom_to_list(f)]
    )

    for {n1, m1, f1} <- t do
      :io.fwrite(
        indent ++ '~-' ++ w ++ 's - ~s:~ts~n',
        [pp_ix_name(n1), :erlang.atom_to_list(m1), :erlang.atom_to_list(f1)]
      )
    end
  end

  defp pp_ix_name(n) do
    :lists.flatten(:io_lib.fwrite('~w', [n]))
  end

  defp fix_wtc({n, {:ext, a, _}}) do
    {n, a}
  end

  defp fix_wtc({n, a}) when is_atom(a) do
    {n, a}
  end

  defp storage_count(t, {u, r, d, dO, ext}) do
    case table_info(t, :storage_type) do
      :unknown ->
        {[t | u], r, d, dO, ext}

      :ram_copies ->
        {u, [t | r], d, dO, ext}

      :disc_copies ->
        {u, r, [t | d], dO, ext}

      :disc_only_copies ->
        {u, r, d, [t | dO], ext}

      {:ext, a, _} ->
        {u, r, d, dO, :orddict.append(a, t, ext)}
    end
  end

  def system_info(item) do
    try do
      system_info2(item)
    catch
      _, error ->
        abort(error)
    end
  end

  defp system_info2(:all) do
    items = system_info_items(:mnesia_lib.is_running())

    for i <- items do
      {i, system_info(i)}
    end
  end

  defp system_info2(:db_nodes) do
    discNs =
      try do
        :ets.lookup_element(:mnesia_gvar, {:schema, :disc_copies}, 2)
      catch
        :error, _ ->
          {:EXIT, {:badarg, []}}
      end

    ramNs =
      try do
        :ets.lookup_element(:mnesia_gvar, {:schema, :ram_copies}, 2)
      catch
        :error, _ ->
          {:EXIT, {:badarg, []}}
      end

    extNs =
      try do
        :ets.lookup_element(:mnesia_gvar, {:schema, :external_copies}, 2)
      catch
        :error, _ ->
          {:EXIT, {:badarg, []}}
      end

    cond do
      is_list(discNs) and is_list(ramNs) and
          is_list(extNs) ->
        discNs ++ ramNs ++ extNs

      true ->
        case :mnesia_schema.read_nodes() do
          {:ok, nodes} ->
            nodes

          {:error, reason} ->
            exit(reason)
        end
    end
  end

  defp system_info2(:running_db_nodes) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {:current, :db_nodes}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        load_mnesia_or_abort()
        :mnesia_lib.running_nodes()

      other ->
        other
    end
  end

  defp system_info2(:extra_db_nodes) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, :extra_db_nodes, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        load_mnesia_or_abort()
        :mnesia_monitor.get_env(:extra_db_nodes)

      other ->
        other
    end
  end

  defp system_info2(:directory) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, :directory, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        load_mnesia_or_abort()
        :mnesia_monitor.get_env(:dir)

      other ->
        other
    end
  end

  defp system_info2(:use_dir) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, :use_dir, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        load_mnesia_or_abort()
        :mnesia_monitor.use_dir()

      other ->
        other
    end
  end

  defp system_info2(:schema_location) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, :schema_location, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        load_mnesia_or_abort()
        :mnesia_monitor.get_env(:schema_location)

      other ->
        other
    end
  end

  defp system_info2(:fallback_activated) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, :fallback_activated, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        load_mnesia_or_abort()
        :mnesia_bup.fallback_exists()

      other ->
        other
    end
  end

  defp system_info2(:version) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, :version, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        apps = :application.loaded_applications()

        case :lists.keysearch(:mnesia, 1, apps) do
          {:value, {_Name, _Desc, version}} ->
            version

          false ->
            {:mnesia_not_loaded, node(), :erlang.timestamp()}
        end

      version ->
        version
    end
  end

  defp system_info2(:access_module) do
    :mnesia_monitor.get_env(:access_module)
  end

  defp system_info2(:auto_repair) do
    :mnesia_monitor.get_env(:auto_repair)
  end

  defp system_info2(:is_running) do
    :mnesia_lib.is_running()
  end

  defp system_info2(:backup_module) do
    :mnesia_monitor.get_env(:backup_module)
  end

  defp system_info2(:backend_types) do
    :mnesia_schema.backend_types()
  end

  defp system_info2(:event_module) do
    :mnesia_monitor.get_env(:event_module)
  end

  defp system_info2(:debug) do
    :mnesia_monitor.get_env(:debug)
  end

  defp system_info2(:dump_log_load_regulation) do
    :mnesia_monitor.get_env(:dump_log_load_regulation)
  end

  defp system_info2(:dump_log_write_threshold) do
    :mnesia_monitor.get_env(:dump_log_write_threshold)
  end

  defp system_info2(:dump_log_time_threshold) do
    :mnesia_monitor.get_env(:dump_log_time_threshold)
  end

  defp system_info2(:dump_log_update_in_place) do
    :mnesia_monitor.get_env(:dump_log_update_in_place)
  end

  defp system_info2(:max_wait_for_decision) do
    :mnesia_monitor.get_env(:max_wait_for_decision)
  end

  defp system_info2(:ignore_fallback_at_startup) do
    :mnesia_monitor.get_env(:ignore_fallback_at_startup)
  end

  defp system_info2(:fallback_error_function) do
    :mnesia_monitor.get_env(:fallback_error_function)
  end

  defp system_info2(:log_version) do
    :mnesia_log.version()
  end

  defp system_info2(:protocol_version) do
    :mnesia_monitor.protocol_version()
  end

  defp system_info2(:schema_version) do
    :mnesia_schema.version()
  end

  defp system_info2(:tables) do
    val({:schema, :tables})
  end

  defp system_info2(:local_tables) do
    val({:schema, :local_tables})
  end

  defp system_info2(:master_node_tables) do
    :mnesia_recover.get_master_node_tables()
  end

  defp system_info2(:subscribers) do
    :mnesia_subscr.subscribers()
  end

  defp system_info2(:checkpoints) do
    :mnesia_checkpoint.checkpoints()
  end

  defp system_info2(:held_locks) do
    :mnesia_locker.get_held_locks()
  end

  defp system_info2(:lock_queue) do
    :mnesia_locker.get_lock_queue()
  end

  defp system_info2(:transactions) do
    :mnesia_tm.get_transactions()
  end

  defp system_info2(:transaction_failures) do
    :mnesia_lib.read_counter(:trans_failures)
  end

  defp system_info2(:transaction_commits) do
    :mnesia_lib.read_counter(:trans_commits)
  end

  defp system_info2(:transaction_restarts) do
    :mnesia_lib.read_counter(:trans_restarts)
  end

  defp system_info2(:transaction_log_writes) do
    :mnesia_dumper.get_log_writes()
  end

  defp system_info2(:core_dir) do
    :mnesia_monitor.get_env(:core_dir)
  end

  defp system_info2(:no_table_loaders) do
    :mnesia_monitor.get_env(:no_table_loaders)
  end

  defp system_info2(:dc_dump_limit) do
    :mnesia_monitor.get_env(:dc_dump_limit)
  end

  defp system_info2(:send_compressed) do
    :mnesia_monitor.get_env(:send_compressed)
  end

  defp system_info2(item) do
    exit({:badarg, item})
  end

  defp system_info_items(:yes) do
    [
      :access_module,
      :auto_repair,
      :backend_types,
      :backup_module,
      :checkpoints,
      :db_nodes,
      :debug,
      :directory,
      :dump_log_load_regulation,
      :dump_log_time_threshold,
      :dump_log_update_in_place,
      :dump_log_write_threshold,
      :event_module,
      :extra_db_nodes,
      :fallback_activated,
      :held_locks,
      :ignore_fallback_at_startup,
      :fallback_error_function,
      :is_running,
      :local_tables,
      :lock_queue,
      :log_version,
      :master_node_tables,
      :max_wait_for_decision,
      :protocol_version,
      :running_db_nodes,
      :schema_location,
      :schema_version,
      :subscribers,
      :tables,
      :transaction_commits,
      :transaction_failures,
      :transaction_log_writes,
      :transaction_restarts,
      :transactions,
      :use_dir,
      :core_dir,
      :no_table_loaders,
      :dc_dump_limit,
      :send_compressed,
      :version
    ]
  end

  defp system_info_items(:no) do
    [
      :auto_repair,
      :backup_module,
      :db_nodes,
      :debug,
      :directory,
      :dump_log_load_regulation,
      :dump_log_time_threshold,
      :dump_log_update_in_place,
      :dump_log_write_threshold,
      :event_module,
      :extra_db_nodes,
      :ignore_fallback_at_startup,
      :fallback_error_function,
      :is_running,
      :log_version,
      :max_wait_for_decision,
      :protocol_version,
      :running_db_nodes,
      :schema_location,
      :schema_version,
      :use_dir,
      :core_dir,
      :version
    ]
  end

  def system_info() do
    isRunning = :mnesia_lib.is_running()

    case isRunning do
      :yes ->
        tmInfo = :mnesia_tm.get_info(10000)
        held = system_info(:held_locks)
        queued = system_info(:lock_queue)
        pat = {:_, :unclear, :_}
        uncertain = :ets.match_object(:mnesia_decision, pat)
        display_system_info(held, queued, tmInfo, uncertain)

      _ ->
        mini_info()
    end

    isRunning
  end

  defp load_mnesia_or_abort() do
    case :mnesia_lib.ensure_loaded(:mnesia) do
      :ok ->
        :ok

      {:error, reason} ->
        abort(reason)
    end
  end

  def create_schema(ns) do
    create_schema(ns, [])
  end

  def create_schema(ns, properties) do
    :mnesia_bup.create_schema(ns, properties)
  end

  def delete_schema(ns) do
    :mnesia_schema.delete_schema(ns)
  end

  def add_backend_type(alias, module) do
    :mnesia_schema.add_backend_type(alias, module)
  end

  def backup(opaque) do
    :mnesia_log.backup(opaque)
  end

  def backup(opaque, mod) do
    :mnesia_log.backup(opaque, mod)
  end

  def traverse_backup(s, t, fun, acc) do
    :mnesia_bup.traverse_backup(s, t, fun, acc)
  end

  def traverse_backup(s, sM, t, tM, f, a) do
    :mnesia_bup.traverse_backup(s, sM, t, tM, f, a)
  end

  def install_fallback(opaque) do
    :mnesia_bup.install_fallback(opaque)
  end

  def install_fallback(opaque, mod) do
    :mnesia_bup.install_fallback(opaque, mod)
  end

  def uninstall_fallback() do
    :mnesia_bup.uninstall_fallback()
  end

  def uninstall_fallback(args) do
    :mnesia_bup.uninstall_fallback(args)
  end

  def activate_checkpoint(args) do
    :mnesia_checkpoint.activate(args)
  end

  def deactivate_checkpoint(name) do
    :mnesia_checkpoint.deactivate(name)
  end

  def backup_checkpoint(name, opaque) do
    :mnesia_log.backup_checkpoint(name, opaque)
  end

  def backup_checkpoint(name, opaque, mod) do
    :mnesia_log.backup_checkpoint(name, opaque, mod)
  end

  def restore(opaque, args) do
    :mnesia_schema.restore(opaque, args)
  end

  def create_table(arg) do
    :mnesia_schema.create_table(arg)
  end

  def create_table(name, arg) when is_list(arg) do
    :mnesia_schema.create_table([{:name, name} | arg])
  end

  def create_table(name, arg) do
    {:aborted, {:badarg, name, arg}}
  end

  def delete_table(tab) do
    :mnesia_schema.delete_table(tab)
  end

  def add_table_copy(tab, n, s) do
    :mnesia_schema.add_table_copy(tab, n, s)
  end

  def del_table_copy(tab, n) do
    :mnesia_schema.del_table_copy(tab, n)
  end

  def move_table_copy(tab, from, to) do
    :mnesia_schema.move_table(tab, from, to)
  end

  def add_table_index(tab, ix) do
    :mnesia_schema.add_table_index(tab, ix)
  end

  def del_table_index(tab, ix) do
    :mnesia_schema.del_table_index(tab, ix)
  end

  def transform_table(tab, fun, newA) do
    try do
      val({tab, :record_name})
    catch
      :exit, reason ->
        :mnesia.abort(reason)
    else
      oldRN ->
        :mnesia_schema.transform_table(tab, fun, newA, oldRN)
    end
  end

  def transform_table(tab, fun, newA, newRN) do
    :mnesia_schema.transform_table(tab, fun, newA, newRN)
  end

  def change_table_copy_type(t, n, s) do
    :mnesia_schema.change_table_copy_type(t, n, s)
  end

  def clear_table(tab) do
    case :erlang.get(:mnesia_activity_state) do
      state = {mod, tid, _Ts}
      when :erlang.element(
             1,
             tid
           ) !== :tid ->
        transaction(
          state,
          fn ->
            do_clear_table(tab)
          end,
          [],
          :infinity,
          mod,
          :sync
        )

      :undefined ->
        transaction(
          :undefined,
          fn ->
            do_clear_table(tab)
          end,
          [],
          :infinity,
          :mnesia,
          :sync
        )

      _ ->
        :mnesia.abort({:aborted, :nested_transaction})
    end
  end

  defp do_clear_table(tab) do
    case :erlang.get(:mnesia_activity_state) do
      {:mnesia, tid, ts} ->
        clear_table(tid, ts, tab, :_)

      {mod, tid, ts} ->
        mod.clear_table(tid, ts, tab, :_)

      _ ->
        abort(:no_transaction)
    end
  end

  def clear_table(tid, ts, tab, obj)
      when :erlang.element(
             1,
             tid
           ) === :tid do
    store = r_tidstore(ts, :store)
    :mnesia_locker.wlock_table(tid, store, tab)
    oid = {tab, :_}
    :ets.insert(store, {oid, obj, :clear_table})
    :ok
  end

  def read_table_property(tab, propKey) do
    val({tab, :user_property, propKey})
  end

  def write_table_property(tab, prop) do
    :mnesia_schema.write_table_property(tab, prop)
  end

  def delete_table_property(tab, propKey) do
    :mnesia_schema.delete_table_property(tab, propKey)
  end

  def change_table_frag(tab, fragProp) do
    :mnesia_schema.change_table_frag(tab, fragProp)
  end

  def dump_tables(tabs) do
    :mnesia_schema.dump_tables(tabs)
  end

  def wait_for_tables(tabs, timeout) do
    :mnesia_controller.wait_for_tables(tabs, timeout)
  end

  def force_load_table(tab) do
    case :mnesia_controller.force_load_table(tab) do
      :ok ->
        :yes

      other ->
        other
    end
  end

  def change_table_access_mode(t, access) do
    :mnesia_schema.change_table_access_mode(t, access)
  end

  def change_table_load_order(t, o) do
    :mnesia_schema.change_table_load_order(t, o)
  end

  def change_table_majority(t, m) do
    :mnesia_schema.change_table_majority(t, m)
  end

  def set_master_nodes(nodes) when is_list(nodes) do
    useDir = system_info(:use_dir)
    isRunning = system_info(:is_running)

    case isRunning do
      :yes ->
        csPat = {{:_, :cstruct}, :_}
        cstructs0 = :ets.match_object(:mnesia_gvar, csPat)

        cstructs =
          for {_, cs} <- cstructs0 do
            cs
          end

        log_valid_master_nodes(cstructs, nodes, useDir, isRunning)

      _NotRunning ->
        case useDir do
          true ->
            :mnesia_lib.lock_table(:schema)

            res =
              case :mnesia_schema.read_cstructs_from_disc() do
                {:ok, cstructs} ->
                  log_valid_master_nodes(cstructs, nodes, useDir, isRunning)

                {:error, reason} ->
                  {:error, reason}
              end

            :mnesia_lib.unlock_table(:schema)
            res

          false ->
            :ok
        end
    end
  end

  def set_master_nodes(nodes) do
    {:error, {:bad_type, nodes}}
  end

  defp log_valid_master_nodes(cstructs, nodes, useDir, isRunning) do
    fun = fn cs ->
      copies = :mnesia_lib.copy_holders(cs)
      valid = :mnesia_lib.intersect(nodes, copies)
      {r_cstruct(cs, :name), valid}
    end

    args = :lists.map(fun, cstructs)
    :mnesia_recover.log_master_nodes(args, useDir, isRunning)
  end

  def set_master_nodes(tab, nodes) when is_list(nodes) do
    useDir = system_info(:use_dir)
    isRunning = system_info(:is_running)

    case isRunning do
      :yes ->
        case (try do
                :ets.lookup_element(:mnesia_gvar, {tab, :cstruct}, 2)
              catch
                :error, _ ->
                  {:EXIT, {:badarg, []}}
              end) do
          {:EXIT, _} ->
            {:error, {:no_exists, tab}}

          cs ->
            case nodes -- :mnesia_lib.copy_holders(cs) do
              [] ->
                args = [{tab, nodes}]
                :mnesia_recover.log_master_nodes(args, useDir, isRunning)

              badNodes ->
                {:error, {:no_exists, tab, badNodes}}
            end
        end

      _NotRunning ->
        case useDir do
          true ->
            :mnesia_lib.lock_table(:schema)

            res =
              case :mnesia_schema.read_cstructs_from_disc() do
                {:ok, cstructs} ->
                  case :lists.keysearch(tab, 2, cstructs) do
                    {:value, cs} ->
                      case nodes -- :mnesia_lib.copy_holders(cs) do
                        [] ->
                          args = [{tab, nodes}]
                          :mnesia_recover.log_master_nodes(args, useDir, isRunning)

                        badNodes ->
                          {:error, {:no_exists, tab, badNodes}}
                      end

                    false ->
                      {:error, {:no_exists, tab}}
                  end

                {:error, reason} ->
                  {:error, reason}
              end

            :mnesia_lib.unlock_table(:schema)
            res

          false ->
            :ok
        end
    end
  end

  def set_master_nodes(tab, nodes) do
    {:error, {:bad_type, tab, nodes}}
  end

  def dump_log() do
    :mnesia_controller.sync_dump_log(:user)
  end

  def sync_log() do
    :mnesia_monitor.sync_log(:latest_log)
  end

  def subscribe(what) do
    :mnesia_subscr.subscribe(self(), what)
  end

  def unsubscribe(what) do
    :mnesia_subscr.unsubscribe(self(), what)
  end

  def report_event(event) do
    :mnesia_lib.report_system_event({:mnesia_user, event})
  end

  def snmp_open_table(tab, us) do
    :mnesia_schema.add_snmp(tab, us)
  end

  def snmp_close_table(tab) do
    :mnesia_schema.del_snmp(tab)
  end

  def snmp_get_row(tab, rowIndex)
      when is_atom(tab) and
             tab != :schema and is_list(rowIndex) do
    case :erlang.get(:mnesia_activity_state) do
      {mod, tid, ts = r_tidstore(store: store)}
      when :erlang.element(
             1,
             tid
           ) === :tid ->
        case snmp_oid_to_mnesia_key(rowIndex, tab) do
          :unknown ->
            ops = find_ops(store, tab, val({tab, :wild_pattern}))
            snmpType = val({tab, :snmp})

            fix = fn {{_, key}, row, op}, res ->
              case :mnesia_snmp_hook.key_to_oid(tab, key, snmpType) do
                ^rowIndex ->
                  case op do
                    :write ->
                      {:ok, row}

                    _ ->
                      :undefined
                  end

                _ ->
                  res
              end
            end

            :lists.foldl(fix, :undefined, ops)

          key ->
            case mod.read(tid, ts, tab, key, :read) do
              [row] ->
                {:ok, row}

              _ ->
                :undefined
            end
        end

      _ ->
        dirty_rpc(tab, :mnesia_snmp_hook, :get_row, [tab, rowIndex])
    end
  end

  def snmp_get_row(tab, _RowIndex) do
    abort({:bad_type, tab})
  end

  def snmp_get_next_index(tab, rowIndex)
      when is_atom(tab) and
             tab != :schema and is_list(rowIndex) do
    {next, origKey} = dirty_rpc(tab, :mnesia_snmp_hook, :get_next_index, [tab, rowIndex])

    case :erlang.get(:mnesia_activity_state) do
      {_Mod, tid, r_tidstore(store: store)}
      when :erlang.element(
             1,
             tid
           ) === :tid ->
        case origKey do
          :undefined ->
            snmp_order_keys(store, tab, rowIndex, [])

          _ ->
            case :ets.match(store, {{tab, origKey}, :_, :"$1"}) do
              [] ->
                snmp_order_keys(store, tab, rowIndex, [origKey])

              ops ->
                case :lists.last(ops) do
                  [:delete] ->
                    snmp_get_next_index(tab, next)

                  _ ->
                    snmp_order_keys(store, tab, rowIndex, [origKey])
                end
            end
        end

      _ ->
        case next do
          :endOfTable ->
            :endOfTable

          _ ->
            {:ok, next}
        end
    end
  end

  def snmp_get_next_index(tab, _RowIndex) do
    abort({:bad_type, tab})
  end

  defp snmp_order_keys(store, tab, rowIndex, def__) do
    all = :ets.match(store, {{tab, :"$1"}, :_, :"$2"})
    snmpType = val({tab, :snmp})

    keys0 =
      for key <- ts_keys_1(all, def__) do
        :mnesia_snmp_hook.key_to_oid(tab, key, snmpType)
      end

    keys = :lists.sort(keys0)
    get_ordered_snmp_key(rowIndex, keys)
  end

  defp get_ordered_snmp_key(prev, [first | _]) when prev < first do
    {:ok, first}
  end

  defp get_ordered_snmp_key(prev, [_ | r]) do
    get_ordered_snmp_key(prev, r)
  end

  defp get_ordered_snmp_key(_, []) do
    :endOfTable
  end

  def snmp_get_mnesia_key(tab, rowIndex)
      when is_atom(tab) and
             tab != :schema and is_list(rowIndex) do
    case :erlang.get(:mnesia_activity_state) do
      {_Mod, tid, ts} when :erlang.element(1, tid) === :tid ->
        res = dirty_rpc(tab, :mnesia_snmp_hook, :get_mnesia_key, [tab, rowIndex])
        snmp_filter_key(res, rowIndex, tab, r_tidstore(ts, :store))

      _ ->
        dirty_rpc(tab, :mnesia_snmp_hook, :get_mnesia_key, [tab, rowIndex])
    end
  end

  def snmp_get_mnesia_key(tab, _RowIndex) do
    abort({:bad_type, tab})
  end

  defp snmp_oid_to_mnesia_key(rowIndex, tab) do
    case :mnesia_snmp_hook.oid_to_key(rowIndex, tab) do
      :unknown ->
        case dirty_rpc(tab, :mnesia_snmp_hook, :get_mnesia_key, [tab, rowIndex]) do
          {:ok, mnesiaKey} ->
            mnesiaKey

          :undefined ->
            :unknown
        end

      mnesiaKey ->
        mnesiaKey
    end
  end

  defp snmp_filter_key(res = {:ok, key}, _RowIndex, tab, store) do
    case :ets.lookup(store, {tab, key}) do
      [] ->
        res

      ops ->
        case :lists.last(ops) do
          {_, _, :write} ->
            res

          _ ->
            :undefined
        end
    end
  end

  defp snmp_filter_key(:undefined, rowIndex, tab, store) do
    case :mnesia_snmp_hook.oid_to_key(rowIndex, tab) do
      :unknown ->
        ops = find_ops(store, tab, val({tab, :wild_pattern}))
        snmpType = val({tab, :snmp})

        fix = fn {{_, key}, _, op}, res ->
          case :mnesia_snmp_hook.key_to_oid(tab, key, snmpType) do
            ^rowIndex ->
              case op do
                :write ->
                  {:ok, key}

                _ ->
                  :undefined
              end

            _ ->
              res
          end
        end

        :lists.foldl(fix, :undefined, ops)

      key ->
        case :ets.lookup(store, {tab, key}) do
          [] ->
            :undefined

          ops ->
            case :lists.last(ops) do
              {_, _, :write} ->
                {:ok, key}

              _ ->
                :undefined
            end
        end
    end
  end

  def load_textfile(f) do
    :mnesia_text.load_textfile(f)
  end

  def dump_to_textfile(f) do
    :mnesia_text.dump_to_textfile(f)
  end

  def table(tab) do
    table(tab, [])
  end

  def table(tab, opts) do
    {[trav, lock, nObjects], qlcOptions0} =
      qlc_opts(
        opts,
        [{:traverse, :select}, {:lock, :read}, {:n_objects, 100}]
      )

    tF =
      case trav do
        {:select, ms} ->
          fn ->
            qlc_select(select(tab, ms, nObjects, lock))
          end

        :select ->
          fn ms ->
            qlc_select(select(tab, ms, nObjects, lock))
          end

        _ ->
          :erlang.error({:badarg, {trav, [tab, opts]}})
      end

    pre = fn arg ->
      pre_qlc(arg, tab)
    end

    post = fn ->
      post_qlc(tab)
    end

    info = fn tag ->
      qlc_info(tab, tag)
    end

    parentFun = fn ->
      {:mnesia_activity, :mnesia.get_activity_id()}
    end

    lookup =
      case trav do
        {:select, _} ->
          []

        _ ->
          lFun = fn
            2, keys ->
              read = fn key ->
                read(tab, key, lock)
              end

              :lists.flatmap(read, keys)

            index, keys ->
              idxRead = fn key ->
                index_read(tab, key, index)
              end

              :lists.flatmap(idxRead, keys)
          end

          [{:lookup_fun, lFun}]
      end

    mFA = fn type ->
      qlc_format(type, tab, nObjects, lock, opts)
    end

    qlcOptions =
      [
        {:pre_fun, pre},
        {:post_fun, post},
        {:info_fun, info},
        {:parent_fun, parentFun},
        {:format_fun, mFA}
        | lookup
      ] ++ qlcOptions0

    :qlc.table(tF, qlcOptions)
  end

  defp pre_qlc(opts, tab) do
    {_, tid, _} =
      case :erlang.get(:mnesia_activity_state) do
        :undefined ->
          case :lists.keysearch(:parent_value, 1, opts) do
            {:value, {:parent_value, {:mnesia_activity, :undefined}}} ->
              abort(:no_transaction)

            {:value, {:parent_value, {:mnesia_activity, aid}}} ->
              {:value, {:stop_fun, stop}} = :lists.keysearch(:stop_fun, 1, opts)
              put_activity_id(aid, stop)
              aid

            _ ->
              abort(:no_transaction)
          end

        else__ ->
          else__
      end

    case :erlang.element(1, tid) do
      :tid ->
        :ok

      _ ->
        case (try do
                :ets.lookup_element(:mnesia_gvar, {tab, :setorbag}, 2)
              catch
                :error, _ ->
                  {:EXIT, {:badarg, []}}
              end) do
          :ordered_set ->
            :ok

          _ ->
            dirty_rpc(tab, :mnesia_tm, :fixtable, [tab, true, self()])
            :ok
        end
    end
  end

  defp post_qlc(tab) do
    case :erlang.get(:mnesia_activity_state) do
      {_, r_tid(), _} ->
        :ok

      _ ->
        case (try do
                :ets.lookup_element(:mnesia_gvar, {tab, :setorbag}, 2)
              catch
                :error, _ ->
                  {:EXIT, {:badarg, []}}
              end) do
          :ordered_set ->
            :ok

          _ ->
            dirty_rpc(tab, :mnesia_tm, :fixtable, [tab, false, self()])
            :ok
        end
    end
  end

  defp qlc_select(:"$end_of_table") do
    []
  end

  defp qlc_select({[], cont}) do
    qlc_select(select(cont))
  end

  defp qlc_select({objects, cont}) do
    objects ++
      fn ->
        qlc_select(select(cont))
      end
  end

  defp qlc_opts(opts, keys) when is_list(opts) do
    qlc_opts(opts, keys, [])
  end

  defp qlc_opts(option, keys) do
    qlc_opts([option], keys, [])
  end

  defp qlc_opts(opts, [{key, def__} | keys], acc) do
    opt =
      case :lists.keysearch(key, 1, opts) do
        {:value, {^key, value}} ->
          value

        false ->
          def__
      end

    qlc_opts(:lists.keydelete(key, 1, opts), keys, [opt | acc])
  end

  defp qlc_opts(opts, [], acc) do
    {:lists.reverse(acc), opts}
  end

  defp qlc_info(tab, :num_of_objects) do
    dirty_rpc(tab, :mnesia, :raw_table_info, [tab, :size])
  end

  defp qlc_info(_, :keypos) do
    2
  end

  defp qlc_info(_, :is_unique_objects) do
    true
  end

  defp qlc_info(tab, :is_unique_keys) do
    case val({tab, :type}) do
      :set ->
        true

      :ordered_set ->
        true

      _ ->
        false
    end
  end

  defp qlc_info(tab, :is_sorted_objects) do
    case val({tab, :type}) do
      :ordered_set ->
        case (try do
                :ets.lookup_element(:mnesia_gvar, {tab, :frag_hash}, 2)
              catch
                :error, _ ->
                  {:EXIT, {:badarg, []}}
              end) do
          {:EXIT, _} ->
            :ascending

          _ ->
            :no
        end

      _ ->
        :no
    end
  end

  defp qlc_info(tab, :indices) do
    val({tab, :index})
  end

  defp qlc_info(_Tab, _) do
    :undefined
  end

  defp qlc_format(:all, tab, nObjects, lock, opts) do
    {:mnesia, :table, [tab, [{:n_objects, nObjects}, {:lock, lock} | opts]]}
  end

  defp qlc_format({:match_spec, ms}, tab, nObjects, lock, opts) do
    {:mnesia, :table,
     [tab, [{:traverse, {:select, ms}}, {:n_objects, nObjects}, {:lock, lock} | opts]]}
  end

  defp qlc_format({:lookup, 2, keys}, tab, _, lock, _) do
    :io_lib.format('lists:flatmap(fun(V) -> ~w:read(~w, V, ~w) end, ~w)', [
      :mnesia,
      tab,
      lock,
      keys
    ])
  end

  defp qlc_format({:lookup, index, keys}, tab, _, _, _) do
    :io_lib.format('lists:flatmap(fun(V) -> ~w:index_read(~w, V, ~w) end, ~w)', [
      :mnesia,
      tab,
      index,
      keys
    ])
  end

  defp do_fixtable(tab, r_tidstore(store: store)) do
    do_fixtable(tab, store)
  end

  defp do_fixtable(tab, store) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :setorbag}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      :ordered_set ->
        :ok

      _ ->
        case :ets.match_object(
               store,
               {:fixtable, {tab, :_}}
             ) do
          [] ->
            node = dirty_rpc(tab, :mnesia_tm, :fixtable, [tab, true, self()])
            :ets.insert(store, {:fixtable, {tab, node}})

          _ ->
            :ignore
        end

        :ok
    end
  end

  def get_activity_id() do
    :erlang.get(:mnesia_activity_state)
  end

  def put_activity_id(activity) do
    :mnesia_tm.put_activity_id(activity)
  end

  defp put_activity_id(activity, fun) do
    :mnesia_tm.put_activity_id(activity, fun)
  end

  defp regular_indexes(tab) do
    posList = val({tab, :index})

    for p <- posList, is_integer(p) do
      p
    end
  end
end
