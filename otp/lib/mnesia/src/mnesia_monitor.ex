defmodule :m_mnesia_monitor do
  use Bitwise
  import :mnesia_lib, only: [dbg_out: 2, error: 2, fatal: 2, set: 2, verbose: 2]
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
    pending_negotiators: [],
    going_down: [],
    tm_started: false,
    early_connects: [],
    connecting: :undefined,
    mq: [],
    remote_node_status: []
  )

  def start() do
    :gen_server.start_link({:local, :mnesia_monitor}, :mnesia_monitor, [self()], [
      {:timeout, :infinity}
    ])
  end

  def init() do
    call(:init)
  end

  def mnesia_down(from, node) do
    cast({:mnesia_down, from, node})
  end

  def mktab(tab, args) do
    unsafe_call({:mktab, tab, args})
  end

  def unsafe_mktab(tab, args) do
    unsafe_call({:unsafe_mktab, tab, args})
  end

  def open_dets(tab, args) do
    unsafe_call({:open_dets, tab, args})
  end

  def unsafe_open_dets(tab, args) do
    unsafe_call({:unsafe_open_dets, tab, args})
  end

  def close_dets(tab) do
    unsafe_call({:close_dets, tab})
  end

  def unsafe_close_dets(name) do
    unsafe_call({:unsafe_close_dets, name})
  end

  def open_log(args) do
    unsafe_call({:open_log, args})
  end

  def reopen_log(name, fname, head) do
    unsafe_call({:reopen_log, name, fname, head})
  end

  def sync_log(name) do
    unsafe_call({:sync_log, name})
  end

  def close_log(name) do
    unsafe_call({:close_log, name})
  end

  def unsafe_close_log(name) do
    unsafe_call({:unsafe_close_log, name})
  end

  def unsafe_create_external(tab, alias, mod, cs) do
    unsafe_call({:unsafe_create_external, tab, alias, mod, cs})
  end

  def disconnect(node) do
    cast({:disconnect, node})
  end

  def negotiate_protocol([]) do
    []
  end

  def negotiate_protocol(nodes) do
    call({:negotiate_protocol, nodes})
  end

  def negotiate_protocol_impl(nodes, requester) do
    version = :mnesia.system_info(:version)
    protocols = acceptable_protocol_versions()
    monitorPid = :erlang.whereis(:mnesia_monitor)
    msg = {:negotiate_protocol, monitorPid, version, protocols}
    {replies, _BadNodes} = multicall(nodes, msg)
    res = check_protocol(replies, protocols)
    send(:mnesia_monitor, {:protocol_negotiated, requester, res})
    :erlang.unlink(:erlang.whereis(:mnesia_monitor))
    :ok
  end

  defp check_protocol(
         [
           {node, {:accept, mon, version, protocol}}
           | tail
         ],
         protocols
       ) do
    case :lists.member(protocol, protocols) do
      true ->
        case protocol == protocol_version() do
          true ->
            set({:protocol, node}, {protocol, false})

          false ->
            set({:protocol, node}, {protocol, true})
        end

        [node(mon) | check_protocol(tail, protocols)]

      false ->
        verbose(
          'Failed to connect with ~p. ~p protocols rejected. expected version = ~p, expected protocol = ~p~n',
          [node, protocols, version, protocol]
        )

        :erlang.unlink(mon)
        check_protocol(tail, protocols)
    end
  end

  defp check_protocol(
         [
           {node, {:reject, _Mon, version, protocol}}
           | tail
         ],
         protocols
       ) do
    verbose(
      'Failed to connect with ~p. ~p protocols rejected. expected version = ~p, expected protocol = ~p~n',
      [node, protocols, version, protocol]
    )

    check_protocol(tail, protocols)
  end

  defp check_protocol([{:error, _Reason} | tail], protocols) do
    dbg_out('~p connect failed error: ~tp~n', [:mnesia_monitor, _Reason])
    check_protocol(tail, protocols)
  end

  defp check_protocol([{:badrpc, _Reason} | tail], protocols) do
    dbg_out('~p connect failed badrpc: ~tp~n', [:mnesia_monitor, _Reason])
    check_protocol(tail, protocols)
  end

  defp check_protocol([], [protocol | _Protocols]) do
    set(:protocol_version, protocol)
    []
  end

  def protocol_version() do
    case (try do
            :ets.lookup_element(:mnesia_gvar, :protocol_version, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        {8, 5}

      version ->
        version
    end
  end

  defp acceptable_protocol_versions() do
    [protocol_version(), {8, 4}, {8, 3}]
  end

  def needs_protocol_conversion(node) do
    case {try do
            :ets.lookup_element(:mnesia_gvar, {:protocol, node}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end, protocol_version()} do
      {{:EXIT, _}, _} ->
        false

      {{_, bool}, {8, 5}} ->
        bool

      {{_, bool}, _} ->
        not bool
    end
  end

  def cast(msg) do
    case :erlang.whereis(:mnesia_monitor) do
      :undefined ->
        :ok

      pid ->
        :gen_server.cast(pid, msg)
    end
  end

  defp unsafe_call(msg) do
    case :erlang.whereis(:mnesia_monitor) do
      :undefined ->
        {:error, {:node_not_running, node()}}

      pid ->
        :gen_server.call(pid, msg, :infinity)
    end
  end

  def call(msg) do
    case :erlang.whereis(:mnesia_monitor) do
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

  defp multicall(nodes, msg) do
    :rpc.multicall(nodes, :mnesia_monitor, :call, [msg])
  end

  def start_proc(who, mod, fun, args) do
    args2 = [who, mod, fun, args]
    :proc_lib.start_link(:mnesia_sp, :init_proc, args2, :infinity)
  end

  def terminate_proc(who, r, state)
      when r != :shutdown and
             r != :killed do
    fatal('~p crashed: ~p state: ~tp~n', [who, r, state])
  end

  def terminate_proc(who, reason, _State) do
    :mnesia_lib.verbose('~p terminated: ~tp~n', [who, reason])
    :ok
  end

  def init([parent]) do
    :erlang.process_flag(:trap_exit, true)

    _ =
      :ets.new(
        :mnesia_gvar,
        [:set, :public, :named_table]
      )

    _ =
      :ets.new(
        :mnesia_stats,
        [:set, :public, :named_table]
      )

    set(:subscribers, [])
    set(:activity_subscribers, [])
    :mnesia_lib.verbose('~p starting: ~p~n', [:mnesia_monitor, self()])
    version = :mnesia.system_info(:version)
    set(:version, version)
    dbg_out('Version: ~p~n', [version])

    try do
      process_config_args(env())
    catch
      _, reason ->
        :mnesia_lib.report_fatal('Bad configuration: ~tp~n', [reason])
        {:stop, {:bad_config, reason}}
    else
      :ok ->
        :mnesia_lib.set({:"$$$_report", :current_pos}, 0)
        level = :mnesia_lib.val(:debug)
        :mnesia_lib.verbose('Mnesia debug level set to ~p\n', [level])
        set(:mnesia_status, :starting)
        set({:current, :db_nodes}, [node()])
        set(:use_dir, use_dir())
        :mnesia_lib.create_counter(:trans_aborts)
        :mnesia_lib.create_counter(:trans_commits)
        :mnesia_lib.create_counter(:trans_log_writes)
        left = get_env(:dump_log_write_threshold)
        :mnesia_lib.set_counter(:trans_log_writes_left, left)
        :mnesia_lib.create_counter(:trans_log_writes_prev)
        :mnesia_lib.create_counter(:trans_restarts)
        :mnesia_lib.create_counter(:trans_failures)
        set(:checkpoints, [])
        set(:pending_checkpoints, [])
        set(:pending_checkpoint_pids, [])
        {:ok, r_state(supervisor: parent)}
    end
  end

  def use_dir() do
    case (try do
            :ets.lookup_element(:mnesia_gvar, :use_dir, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        case get_env(:schema_location) do
          :disc ->
            true

          :opt_disc ->
            non_empty_dir()

          :ram ->
            false
        end

      bool ->
        bool
    end
  end

  defp non_empty_dir() do
    :erlang.or(
      :erlang.or(
        :mnesia_lib.exists(:mnesia_bup.fallback_bup()),
        :mnesia_lib.exists(:mnesia_lib.tab2dmp(:schema))
      ),
      :mnesia_lib.exists(:mnesia_lib.tab2dat(:schema))
    )
  end

  def handle_call({:mktab, tab, args}, _From, state) do
    try do
      _ = :ets.new(tab, args)
    catch
      :error, exitReason ->
        msg = 'Cannot create ets table'
        reason = {:system_limit, msg, tab, args, exitReason}
        fatal('~tp~n', [reason])
        {:noreply, state}
    else
      reply ->
        {:reply, reply, state}
    end
  end

  def handle_call({:unsafe_mktab, tab, args}, _From, state) do
    try do
      _ = :ets.new(tab, args)
    catch
      :error, exitReason ->
        {:reply, {:error, exitReason}, state}
    else
      reply ->
        {:reply, reply, state}
    end
  end

  def handle_call({:open_dets, tab, args}, _From, state) do
    case :mnesia_lib.dets_sync_open(tab, args) do
      {:ok, ^tab} ->
        {:reply, {:ok, tab}, state}

      {:error, reason} ->
        msg = 'Cannot open dets table'
        error = {:error, {msg, tab, args, reason}}
        fatal('~tp~n', [error])
        {:noreply, state}
    end
  end

  def handle_call({:unsafe_open_dets, tab, args}, _From, state) do
    case :mnesia_lib.dets_sync_open(tab, args) do
      {:ok, ^tab} ->
        {:reply, {:ok, tab}, state}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:close_dets, tab}, _From, state) do
    :ok = :mnesia_lib.dets_sync_close(tab)
    {:reply, :ok, state}
  end

  def handle_call({:unsafe_close_dets, tab}, _From, state) do
    :mnesia_lib.dets_sync_close(tab)
    {:reply, :ok, state}
  end

  def handle_call({:open_log, args}, _From, state) do
    res = :disk_log.open([{:notify, true} | args])
    {:reply, res, state}
  end

  def handle_call({:reopen_log, name, fname, head}, _From, state) do
    case :disk_log.reopen(name, fname, head) do
      :ok ->
        {:reply, :ok, state}

      {:error, reason} ->
        msg = 'Cannot rename disk_log file'
        error = {:error, {msg, name, fname, head, reason}}
        fatal('~tp~n', [error])
        {:noreply, state}
    end
  end

  def handle_call({:sync_log, name}, _From, state) do
    {:reply, :disk_log.sync(name), state}
  end

  def handle_call({:close_log, name}, _From, state) do
    case :disk_log.close(name) do
      :ok ->
        {:reply, :ok, state}

      {:error, reason} ->
        msg = 'Cannot close disk_log file'
        error = {:error, {msg, name, reason}}
        fatal('~tp~n', [error])
        {:noreply, state}
    end
  end

  def handle_call({:unsafe_close_log, name}, _From, state) do
    _ = :disk_log.close(name)
    {:reply, :ok, state}
  end

  def handle_call({:unsafe_create_external, tab, alias, mod, cs}, _From, state) do
    case (try do
            mod.create_table(alias, tab, :mnesia_schema.cs2list(cs))
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end) do
      {:EXIT, exitReason} ->
        {:reply, {:error, exitReason}, state}

      reply ->
        {:reply, reply, state}
    end
  end

  def handle_call({:negotiate_protocol, mon, _Version, _Protocols}, _From, state)
      when r_state(state, :tm_started) == false do
    state2 =
      r_state(state,
        early_connects: [
          node(mon)
          | r_state(state, :early_connects)
        ]
      )

    {:reply, {node(), {:reject, self(), :uninitialized, :uninitialized}}, state2}
  end

  def handle_call({:negotiate_protocol, mon, version, protocols}, from, state)
      when node(mon) != node() do
    protocol = protocol_version()
    myVersion = :mnesia.system_info(:version)

    case :lists.member(protocol, protocols) do
      true ->
        accept_protocol(mon, myVersion, protocol, from, state)

      false ->
        case hd(protocols) do
          {8, 4} ->
            accept_protocol(mon, myVersion, {8, 4}, from, state)

          _ ->
            verbose(
              'Connection with ~p rejected. version = ~p, protocols = ~p, expected version = ~p, expected protocol = ~p~n',
              [node(mon), version, protocols, myVersion, protocol]
            )

            {:reply, {node(), {:reject, self(), myVersion, protocol}}, state}
        end
    end
  end

  def handle_call({:negotiate_protocol, nodes}, from, state) do
    case :mnesia_lib.intersect(
           r_state(state, :going_down),
           nodes
         ) do
      [] ->
        spawn_link(:mnesia_monitor, :negotiate_protocol_impl, [nodes, from])
        {:noreply, r_state(state, connecting: {from, nodes})}

      _ ->
        {:reply, :busy, state}
    end
  end

  def handle_call(:init, _From, state) do
    _ = :net_kernel.monitor_nodes(true)
    earlyNodes = r_state(state, :early_connects)
    state2 = r_state(state, tm_started: true)
    {:reply, earlyNodes, state2}
  end

  def handle_call(msg, _From, state) do
    :erlang.error('~p got unexpected call: ~tp~n', [:mnesia_monitor, msg])
    {:noreply, state}
  end

  defp accept_protocol(mon, version, protocol, from, state) do
    reply = {node(), {:accept, self(), version, protocol}}
    node = node(mon)
    pending0 = r_state(state, :pending_negotiators)
    pending = :lists.keydelete(node, 1, pending0)

    case :lists.member(node, r_state(state, :going_down)) do
      true ->
        p = pending ++ [{node, mon, from, reply}]
        {:noreply, r_state(state, pending_negotiators: p)}

      false ->
        :erlang.link(mon)

        case protocol == protocol_version() do
          true ->
            set({:protocol, node}, {protocol, false})

          false ->
            set({:protocol, node}, {protocol, true})
        end

        {:reply, reply, r_state(state, pending_negotiators: pending)}
    end
  end

  def handle_cast(
        {:mnesia_down, :mnesia_controller, node},
        state
      ) do
    :mnesia_tm.mnesia_down(node)
    {:noreply, state}
  end

  def handle_cast({:mnesia_down, :mnesia_tm, node}, state) do
    down = {:mnesia_down, node}
    :mnesia_lib.report_system_event(down)
    goingDown = :lists.delete(node, r_state(state, :going_down))
    state2 = r_state(state, going_down: goingDown)
    pending = r_state(state, :pending_negotiators)
    state3 = check_raise_conditon_nodeup(node, state2)

    case :lists.keysearch(node, 1, pending) do
      {:value, {^node, mon, replyTo, reply}} ->
        :erlang.link(mon)
        :gen_server.reply(replyTo, reply)
        p2 = :lists.keydelete(node, 1, pending)
        state4 = r_state(state3, pending_negotiators: p2)
        process_q(state4)

      false ->
        process_q(state3)
    end
  end

  def handle_cast({:disconnect, node}, state) do
    case :rpc.call(node, :erlang, :whereis, [:mnesia_monitor]) do
      {:badrpc, _} ->
        :ignore

      :undefined ->
        :ignore

      remoteMon when is_pid(remoteMon) ->
        :erlang.unlink(remoteMon)
    end

    {:noreply, state}
  end

  def handle_cast(
        {:inconsistent_database, context, node},
        state
      ) do
    msg = {:inconsistent_database, context, node}
    :mnesia_lib.report_system_event(msg)
    {:noreply, state}
  end

  def handle_cast(msg, state) do
    :erlang.error('~p got unexpected cast: ~tp~n', [:mnesia_monitor, msg])
    {:noreply, state}
  end

  def handle_info({:EXIT, pid, r}, state)
      when pid == r_state(state, :supervisor) do
    dbg_out('~p was ~p by supervisor~n', [:mnesia_monitor, r])
    {:stop, r, state}
  end

  def handle_info({:EXIT, pid, :fatal}, state)
      when node(pid) == node() do
    dbg_out('~p got FATAL ERROR from: ~p~n', [:mnesia_monitor, pid])

    try do
      :erlang.exit(:erlang.whereis(:mnesia_locker), :kill)
    catch
      :error, _ ->
        :ok
    end

    {:noreply, state}
  end

  def handle_info(msg = {:EXIT, pid, _}, state) do
    node = node(pid)

    cond do
      node != node() and
          r_state(state, :connecting) == :undefined ->
        :mnesia_recover.mnesia_down(node)
        :mnesia_controller.mnesia_down(node)
        {:noreply, r_state(state, going_down: [node | r_state(state, :going_down)])}

      node != node() ->
        {:noreply, r_state(state, mq: r_state(state, :mq) ++ [{:info, msg}])}

      true ->
        hint = 'Hint: check that the disk still is writable'
        fatal('~p got unexpected info: ~tp; ~p~n', [:mnesia_monitor, msg, hint])
    end
  end

  def handle_info({:protocol_negotiated, from, res}, state) do
    ^from = :erlang.element(1, r_state(state, :connecting))
    :gen_server.reply(from, res)
    process_q(r_state(state, connecting: :undefined))
  end

  def handle_info({:check_nodeup, node}, state) do
    state2 = check_mnesia_down(node, state)
    {:noreply, state2}
  end

  def handle_info({:nodeup, node}, state) do
    state2 = remote_node_status(node, :up, state)
    state3 = check_mnesia_down(node, state2)
    {:noreply, state3}
  end

  def handle_info({:nodedown, node}, state) do
    state2 = remote_node_status(node, :down, state)
    {:noreply, state2}
  end

  def handle_info({:disk_log, _Node, log, info}, state) do
    case info do
      {:truncated, _No} ->
        :ok

      _ ->
        :mnesia_lib.important(
          'Warning Log file ~tp error reason ~ts~n',
          [log, :disk_log.format_error(info)]
        )
    end

    {:noreply, state}
  end

  def handle_info(msg, state) do
    :erlang.error('~p got unexpected info (~tp): ~tp~n', [:mnesia_monitor, state, msg])
  end

  defp process_q(state = r_state(mq: [])) do
    {:noreply, state}
  end

  defp process_q(state = r_state(mq: [{:info, msg} | r])) do
    handle_info(msg, r_state(state, mq: r))
  end

  defp process_q(state = r_state(mq: [{:cast, msg} | r])) do
    handle_cast(msg, r_state(state, mq: r))
  end

  defp process_q(state = r_state(mq: [{:call, from, msg} | r])) do
    handle_call(msg, from, r_state(state, mq: r))
  end

  def terminate(reason, state) do
    terminate_proc(:mnesia_monitor, reason, state)
  end

  def code_change(_, {:state, sUP, pN, gD, tMS, eC}, _) do
    {:ok,
     r_state(
       supervisor: sUP,
       pending_negotiators: pN,
       going_down: gD,
       tm_started: tMS,
       early_connects: eC
     )}
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp process_config_args([]) do
    :ok
  end

  defp process_config_args([c | t]) do
    v = get_env(c)
    dbg_out('Env ~p: ~p~n', [c, v])
    :mnesia_lib.set(c, v)
    process_config_args(t)
  end

  def set_env(e, val) do
    :mnesia_lib.set(e, check_type(e, val))
    :ok
  end

  def get_env(e) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, e, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        case :application.get_env(:mnesia, e) do
          {:ok, val} ->
            check_type(e, val)

          :undefined ->
            check_type(e, default_env(e))
        end

      val ->
        val
    end
  end

  defp env() do
    [
      :access_module,
      :allow_index_on_key,
      :auto_repair,
      :backup_module,
      :debug,
      :dir,
      :dump_disc_copies_at_startup,
      :dump_log_load_regulation,
      :dump_log_time_threshold,
      :dump_log_update_in_place,
      :dump_log_write_threshold,
      :event_module,
      :extra_db_nodes,
      :ignore_fallback_at_startup,
      :fallback_error_function,
      :fold_chunk_size,
      :max_wait_for_decision,
      :schema_location,
      :core_dir,
      :pid_sort_order,
      :no_table_loaders,
      :dc_dump_limit,
      :send_compressed,
      :schema
    ]
  end

  defp default_env(:access_module) do
    :mnesia
  end

  defp default_env(:auto_repair) do
    true
  end

  defp default_env(:allow_index_on_key) do
    false
  end

  defp default_env(:backup_module) do
    :mnesia_backup
  end

  defp default_env(:debug) do
    :none
  end

  defp default_env(:dir) do
    name = :lists.concat(['Mnesia.', node()])
    :filename.absname(name)
  end

  defp default_env(:dump_disc_copies_at_startup) do
    true
  end

  defp default_env(:dump_log_load_regulation) do
    false
  end

  defp default_env(:dump_log_time_threshold) do
    :timer.minutes(3)
  end

  defp default_env(:dump_log_update_in_place) do
    true
  end

  defp default_env(:dump_log_write_threshold) do
    1000
  end

  defp default_env(:event_module) do
    :mnesia_event
  end

  defp default_env(:extra_db_nodes) do
    []
  end

  defp default_env(:ignore_fallback_at_startup) do
    false
  end

  defp default_env(:fallback_error_function) do
    {:mnesia, :lkill}
  end

  defp default_env(:fold_chunk_size) do
    100
  end

  defp default_env(:max_wait_for_decision) do
    :infinity
  end

  defp default_env(:schema_location) do
    :opt_disc
  end

  defp default_env(:core_dir) do
    false
  end

  defp default_env(:pid_sort_order) do
    false
  end

  defp default_env(:no_table_loaders) do
    2
  end

  defp default_env(:dc_dump_limit) do
    4
  end

  defp default_env(:send_compressed) do
    0
  end

  defp default_env(:schema) do
    []
  end

  defp check_type(env, val) do
    try do
      do_check_type(env, val)
    catch
      :error, _ ->
        exit({:bad_config, env, val})
    end
  end

  def do_check_type(:access_module, a) when is_atom(a) do
    a
  end

  def do_check_type(:allow_index_on_key, b) do
    bool(b)
  end

  def do_check_type(:auto_repair, b) do
    bool(b)
  end

  def do_check_type(:backup_module, b) when is_atom(b) do
    b
  end

  def do_check_type(:debug, :debug) do
    :debug
  end

  def do_check_type(:debug, false) do
    :none
  end

  def do_check_type(:debug, :none) do
    :none
  end

  def do_check_type(:debug, :trace) do
    :trace
  end

  def do_check_type(:debug, true) do
    :debug
  end

  def do_check_type(:debug, :verbose) do
    :verbose
  end

  def do_check_type(:dir, v) do
    :filename.absname(v)
  end

  def do_check_type(:dump_disc_copies_at_startup, b) do
    bool(b)
  end

  def do_check_type(:dump_log_load_regulation, b) do
    bool(b)
  end

  def do_check_type(:dump_log_time_threshold, i)
      when is_integer(i) and i > 0 do
    i
  end

  def do_check_type(:dump_log_update_in_place, b) do
    bool(b)
  end

  def do_check_type(:dump_log_write_threshold, i)
      when is_integer(i) and i > 0 do
    i
  end

  def do_check_type(:event_module, a) when is_atom(a) do
    a
  end

  def do_check_type(:ignore_fallback_at_startup, b) do
    bool(b)
  end

  def do_check_type(:fallback_error_function, {mod, func})
      when is_atom(mod) and is_atom(func) do
    {mod, func}
  end

  def do_check_type(:extra_db_nodes, l) when is_list(l) do
    fun = fn
      n when n == node() ->
        false

      a when is_atom(a) ->
        true
    end

    :lists.filter(fun, l)
  end

  def do_check_type(:fold_chunk_size, i)
      when (is_integer(i) and
              i > 0) or
             i === :infinity do
    i
  end

  def do_check_type(:max_wait_for_decision, :infinity) do
    :infinity
  end

  def do_check_type(:max_wait_for_decision, i)
      when is_integer(i) and i > 0 do
    i
  end

  def do_check_type(:schema_location, m) do
    media(m)
  end

  def do_check_type(:core_dir, 'false') do
    false
  end

  def do_check_type(:core_dir, false) do
    false
  end

  def do_check_type(:core_dir, dir) when is_list(dir) do
    dir
  end

  def do_check_type(:pid_sort_order, :r9b_plain) do
    :r9b_plain
  end

  def do_check_type(:pid_sort_order, 'r9b_plain') do
    :r9b_plain
  end

  def do_check_type(:pid_sort_order, :standard) do
    :standard
  end

  def do_check_type(:pid_sort_order, 'standard') do
    :standard
  end

  def do_check_type(:pid_sort_order, _) do
    false
  end

  def do_check_type(:no_table_loaders, n)
      when is_integer(n) and
             n > 0 do
    n
  end

  def do_check_type(:dc_dump_limit, n)
      when is_number(n) and
             n > 0 do
    n
  end

  def do_check_type(:send_compressed, l)
      when is_integer(l) and
             l >= 0 and l <= 9 do
    l
  end

  def do_check_type(:schema, l) when is_list(l) do
    l
  end

  defp bool(true) do
    true
  end

  defp bool(false) do
    false
  end

  defp media(:disc) do
    :disc
  end

  defp media(:opt_disc) do
    :opt_disc
  end

  defp media(:ram) do
    :ram
  end

  def patch_env(env, val) do
    try do
      do_check_type(env, val)
    catch
      :error, _ ->
        {:error, {:bad_type, env, val}}
    else
      newVal ->
        :application_controller.set_env(:mnesia, env, newVal)
        newVal
    end
  end

  def detect_partitioned_network(mon, node) do
    detect_inconcistency(
      [node],
      :running_partitioned_network
    )

    :erlang.unlink(mon)
    exit(:normal)
  end

  def detect_inconcistency([], _Context) do
    :ok
  end

  def detect_inconcistency(nodes, context) do
    downs =
      for n <- nodes,
          :mnesia_recover.has_mnesia_down(n) do
        n
      end

    {replies, _BadNodes} =
      :rpc.multicall(downs, :mnesia_monitor, :has_remote_mnesia_down, [node()])

    report_inconsistency(replies, context, :ok)
  end

  def has_remote_mnesia_down(node) do
    hasDown = :mnesia_recover.has_mnesia_down(node)
    master = :mnesia_recover.get_master_nodes(:schema)

    cond do
      hasDown == true and master == [] ->
        {true, node()}

      true ->
        {false, node()}
    end
  end

  defp report_inconsistency([{true, node} | replies], context, _Status) do
    msg = {:inconsistent_database, context, node}
    :mnesia_lib.report_system_event(msg)
    report_inconsistency(replies, context, :inconsistent_database)
  end

  defp report_inconsistency([{false, _Node} | replies], context, status) do
    report_inconsistency(replies, context, status)
  end

  defp report_inconsistency([{:badrpc, _Reason} | replies], context, status) do
    report_inconsistency(replies, context, status)
  end

  defp report_inconsistency([], _Context, status) do
    status
  end

  defp remote_node_status(node, status, state) do
    {:ok, nodes} = :mnesia_schema.read_nodes()

    case :lists.member(node, nodes) do
      true ->
        update_node_status({node, status}, state)

      _ ->
        state
    end
  end

  defp update_node_status(
         {node, :down},
         state = r_state(remote_node_status: rNodeS)
       ) do
    rNodeS2 = :lists.ukeymerge(1, [{node, :down}], rNodeS)
    r_state(state, remote_node_status: rNodeS2)
  end

  defp update_node_status(
         {node, :up},
         state = r_state(remote_node_status: rNodeS)
       ) do
    case :lists.keyfind(node, 1, rNodeS) do
      {^node, :down} ->
        rNodeS2 = :lists.ukeymerge(1, [{node, :up}], rNodeS)
        r_state(state, remote_node_status: rNodeS2)

      _ ->
        state
    end
  end

  defp check_raise_conditon_nodeup(node, state = r_state(remote_node_status: rNodeS)) do
    case :lists.keyfind(node, 1, rNodeS) do
      {^node, :up} ->
        send(self(), {:check_nodeup, node})

      _ ->
        :ignore
    end

    r_state(state, remote_node_status: :lists.keydelete(node, 1, rNodeS))
  end

  defp check_mnesia_down(node, state = r_state(remote_node_status: rNodeS)) do
    hasDown = :mnesia_recover.has_mnesia_down(node)
    imRunning = :mnesia_lib.is_running()

    cond do
      hasDown == true and imRunning == :yes ->
        spawn_link(:mnesia_monitor, :detect_partitioned_network, [self(), node])
        r_state(state, remote_node_status: :lists.keydelete(node, 1, rNodeS))

      true ->
        state
    end
  end
end
