defmodule :m_mnesia_recover do
  use Bitwise
  import :mnesia_lib, only: [error: 2, fatal: 2, set: 2, verbose: 2]
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
    unclear_pid: :undefined,
    unclear_decision: :undefined,
    unclear_waitfor: :undefined,
    tm_queue_len: 0,
    log_dump_overload: false,
    initiated: false,
    early_msgs: []
  )

  Record.defrecord(:r_transient_decision, :transient_decision,
    tid: :undefined,
    outcome: :undefined
  )

  def start() do
    :gen_server.start_link({:local, :mnesia_recover}, :mnesia_recover, [self()], [
      {:timeout, :infinity}
    ])
  end

  def init() do
    call(:init)
  end

  def next_garb() do
    pid = :erlang.whereis(:mnesia_recover)
    :erlang.send_after(:timer.minutes(2), pid, :garb_decisions)
  end

  def next_check_overload() do
    pid = :erlang.whereis(:mnesia_recover)
    :erlang.send_after(:timer.seconds(10), pid, :check_overload)
  end

  defp do_check_overload(s) do
    case :erlang.whereis(:mnesia_tm) do
      pid when is_pid(pid) ->
        threshold = 100
        prev = r_state(s, :tm_queue_len)

        {:message_queue_len, len} =
          :erlang.process_info(
            pid,
            :message_queue_len
          )

        cond do
          len > threshold and prev > threshold ->
            what = {:mnesia_tm, :message_queue_len, [prev, len]}
            :mnesia_lib.report_system_event({:mnesia_overload, what})
            :mnesia_lib.overload_set(:mnesia_tm, true)
            r_state(s, tm_queue_len: 0)

          len > threshold ->
            r_state(s, tm_queue_len: len)

          true ->
            :mnesia_lib.overload_set(:mnesia_tm, false)
            r_state(s, tm_queue_len: 0)
        end

      :undefined ->
        s
    end
  end

  def allow_garb() do
    cast(:allow_garb)
  end

  defp do_allow_garb() do
    curr = val(:latest_transient_decision)

    case :ets.info(curr, :size) > 20 do
      true ->
        old = val(:previous_transient_decisions)
        next = create_transient_decision()
        {prev, reallyOld} = sublist([curr | old], 10, [])

        for tab <- reallyOld do
          :ets.delete(tab)
        end

        set(:previous_transient_decisions, prev)
        set(:latest_transient_decision, next)

      false ->
        :ignore
    end
  end

  defp sublist([h | r], n, acc) when n > 0 do
    sublist(r, n - 1, [h | acc])
  end

  defp sublist(list, _N, acc) do
    {:lists.reverse(acc), list}
  end

  defp do_garb_decisions() do
    case val(:previous_transient_decisions) do
      [first, second | rest] ->
        set(:previous_transient_decisions, [first, second])

        for tab <- rest do
          :ets.delete(tab)
        end

      _ ->
        :ignore
    end
  end

  def connect_nodes(ns) do
    call({:connect_nodes, ns})
  end

  def disconnect(node) do
    call({:disconnect, node})
  end

  def log_decision(d) do
    cast({:log_decision, d})
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

  def call(msg) do
    pid = :erlang.whereis(:mnesia_recover)

    case pid do
      :undefined ->
        {:error, {:node_not_running, node()}}

      ^pid ->
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
    :rpc.multicall(nodes, :mnesia_recover, :call, [msg])
  end

  defp cast(msg) do
    case :erlang.whereis(:mnesia_recover) do
      :undefined ->
        :ignore

      pid ->
        :gen_server.cast(pid, msg)
    end
  end

  defp abcast(nodes, msg) do
    :gen_server.abcast(nodes, :mnesia_recover, msg)
  end

  def note_decision(tid, outcome) do
    tab = val(:latest_transient_decision)
    :ets.insert(tab, r_transient_decision(tid: tid, outcome: outcome))
  end

  defp note_up(node, _Date, _Time) do
    :ets.delete(:mnesia_decision, node)
  end

  defp note_down(node, date, time) do
    :ets.insert(
      :mnesia_decision,
      {:mnesia_down, node, date, time}
    )
  end

  defp note_master_nodes(tab, []) do
    :ets.delete(:mnesia_decision, tab)
  end

  defp note_master_nodes(tab, nodes) when is_list(nodes) do
    master = {:master_nodes, tab, nodes}
    :ets.insert(:mnesia_decision, master)
  end

  defp note_outcome(d) when r_decision(d, :disc_nodes) == [] do
    note_decision(
      r_decision(d, :tid),
      filter_outcome(r_decision(d, :outcome))
    )

    :ets.delete(:mnesia_decision, r_decision(d, :tid))
  end

  defp note_outcome(d) when r_decision(d, :disc_nodes) != [] do
    :ets.insert(:mnesia_decision, d)
  end

  defp do_log_decision(d) when r_decision(d, :outcome) != :unclear do
    oldD = decision(r_decision(d, :tid))
    mergedD = merge_decisions(node(), oldD, d)
    do_log_decision(mergedD, true, d)
  end

  defp do_log_decision(d) do
    do_log_decision(d, false, :undefined)
  end

  defp do_log_decision(d, doTell, nodeD) do
    discNs = r_decision(d, :disc_nodes) -- [node()]
    outcome = r_decision(d, :outcome)

    d2 =
      case outcome do
        :aborted ->
          r_decision(d, disc_nodes: discNs)

        :committed ->
          r_decision(d, disc_nodes: discNs)

        _ ->
          d
      end

    note_outcome(d2)

    case :mnesia_monitor.use_dir() do
      true ->
        cond do
          doTell == true and outcome != :unclear ->
            tell_im_certain(r_decision(nodeD, :disc_nodes) -- [node()], d2)
            tell_im_certain(r_decision(nodeD, :ram_nodes) -- [node()], d2)
            :mnesia_log.log(d2)

          outcome != :unclear ->
            :mnesia_log.log(d2)

          true ->
            :ignore
        end

      false ->
        :ignore
    end
  end

  defp tell_im_certain([], _D) do
    :ignore
  end

  defp tell_im_certain(nodes, d) do
    msg = {:im_certain, node(), d}
    abcast(nodes, msg)
  end

  def sync() do
    call(:sync)
  end

  def log_mnesia_up(node) do
    call({:log_mnesia_up, node})
  end

  def log_mnesia_down(node) do
    call({:log_mnesia_down, node})
  end

  def get_mnesia_downs() do
    tab = :mnesia_decision
    pat = {:mnesia_down, :_, :_, :_}
    downs = :ets.match_object(tab, pat)

    for {:mnesia_down, node, _Date, _Time} <- downs do
      node
    end
  end

  def has_mnesia_down(node) do
    case :ets.lookup(:mnesia_decision, node) do
      [{:mnesia_down, ^node, _Date, _Time}] ->
        true

      [] ->
        false
    end
  end

  def mnesia_down(node) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, :recover_nodes, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        :ignore

      _ ->
        :mnesia_lib.del(:recover_nodes, node)
        cast({:mnesia_down, node})
    end
  end

  def log_dump_overload(flag) when is_boolean(flag) do
    cast({:log_dump_overload, flag})
  end

  def log_master_nodes(args, useDir, isRunning) do
    cond do
      isRunning == :yes ->
        log_master_nodes2(args, useDir, isRunning, :ok)

      useDir == false ->
        :ok

      true ->
        name = :latest_log
        fname = :mnesia_log.latest_log_file()
        exists = :mnesia_lib.exists(fname)
        repair = :mnesia.system_info(:auto_repair)
        openArgs = [{:file, fname}, {:name, name}, {:repair, repair}]

        case :disk_log.open(openArgs) do
          {:ok, ^name} ->
            log_master_nodes2(args, useDir, isRunning, :ok)

          {:repaired, ^name, {:recovered, _R}, {:badbytes, _B}}
          when exists == true ->
            log_master_nodes2(args, useDir, isRunning, :ok)

          {:repaired, ^name, {:recovered, _R}, {:badbytes, _B}}
          when exists == false ->
            :mnesia_log.write_trans_log_header()
            log_master_nodes2(args, useDir, isRunning, :ok)

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp log_master_nodes2([{tab, nodes} | tail], useDir, isRunning, worstRes) do
    res =
      case isRunning do
        :yes ->
          r = call({:log_master_nodes, tab, nodes, useDir, isRunning})
          :mnesia_controller.master_nodes_updated(tab, nodes)
          r

        _ ->
          do_log_master_nodes(tab, nodes, useDir, isRunning)
      end

    case res do
      :ok ->
        log_master_nodes2(tail, useDir, isRunning, worstRes)

      {:error, reason} ->
        log_master_nodes2(tail, useDir, isRunning, {:error, reason})
    end
  end

  defp log_master_nodes2([], _UseDir, isRunning, worstRes) do
    case isRunning do
      :yes ->
        worstRes

      _ ->
        :disk_log.close(:latest_log)
        worstRes
    end
  end

  def get_master_node_info() do
    tab = :mnesia_decision
    pat = {:master_nodes, :_, :_}

    try do
      :mnesia_lib.db_match_object(:ram_copies, tab, pat)
    catch
      :error, _ ->
        []
    end
  end

  def get_master_node_tables() do
    masters = get_master_node_info()

    for {:master_nodes, tab, _Nodes} <- masters do
      tab
    end
  end

  def get_master_nodes(tab) do
    try do
      :ets.lookup_element(:mnesia_decision, tab, 3)
    catch
      :error, _ ->
        []
    end
  end

  def what_happened(tid, protocol, nodes) do
    default =
      case protocol do
        :asym_trans ->
          :aborted

        _ ->
          :unclear
      end

    this = node()

    case :lists.member(this, nodes) do
      true ->
        {:ok, outcome} = call({:what_happened, default, tid})
        others = nodes -- [this]

        case filter_outcome(outcome) do
          :unclear ->
            what_happened_remotely(tid, default, others)

          :aborted ->
            :aborted

          :committed ->
            :committed
        end

      false ->
        what_happened_remotely(tid, default, nodes)
    end
  end

  defp what_happened_remotely(tid, default, nodes) do
    {replies, _} =
      multicall(
        nodes,
        {:what_happened, default, tid}
      )

    check_what_happened(replies, 0, 0)
  end

  defp check_what_happened([h | t], aborts, commits) do
    case h do
      {:ok, r} ->
        case filter_outcome(r) do
          :committed ->
            check_what_happened(t, aborts, commits + 1)

          :aborted ->
            check_what_happened(t, aborts + 1, commits)

          :unclear ->
            check_what_happened(t, aborts, commits)
        end

      {:error, _} ->
        check_what_happened(t, aborts, commits)

      {:badrpc, _} ->
        check_what_happened(t, aborts, commits)
    end
  end

  defp check_what_happened([], aborts, commits) do
    cond do
      aborts == 0 and commits == 0 ->
        :aborted

      aborts > 0 ->
        :aborted

      aborts == 0 and commits > 0 ->
        :committed
    end
  end

  def wait_for_decision(:presume_commit, _InitBy) do
    {{:presume_commit, self()}, :committed}
  end

  def wait_for_decision(d, initBy)
      when r_decision(d, :outcome) == :presume_abort do
    wait_for_decision(d, initBy, 0)
  end

  defp wait_for_decision(d, initBy, n) do
    tid = r_decision(d, :tid)
    max = 10
    outcome = outcome(tid, r_decision(d, :outcome))

    cond do
      outcome === :committed ->
        {tid, :committed}

      outcome === :aborted ->
        {tid, :aborted}

      initBy == :startup ->
        {:ok, res} = call({:wait_for_decision, d})
        {tid, res}

      outcome === :presume_abort ->
        case n > max do
          true ->
            {tid, :aborted}

          false ->
            :timer.sleep(10)
            wait_for_decision(d, initBy, n + 1)
        end

      initBy != :startup ->
        :timer.sleep(100)
        wait_for_decision(d, initBy, n)
    end
  end

  def still_pending([tid | pending]) do
    case filter_outcome(outcome(tid, :unclear)) do
      :unclear ->
        [tid | still_pending(pending)]

      _ ->
        still_pending(pending)
    end
  end

  def still_pending([]) do
    []
  end

  defp load_decision_tab() do
    cont = :mnesia_log.open_decision_tab()
    load_decision_tab(cont, :load_decision_tab)
    :mnesia_log.close_decision_tab()
  end

  defp load_decision_tab(cont, initBy) do
    case :mnesia_log.chunk_decision_tab(cont) do
      {cont2, decisions} ->
        note_log_decisions(decisions, initBy)
        load_decision_tab(cont2, initBy)

      :eof ->
        :ok
    end
  end

  defp convert_old() do
    hasOldStuff =
      :erlang.or(
        :mnesia_lib.exists(:mnesia_log.previous_decision_log_file()),
        :mnesia_lib.exists(:mnesia_log.decision_log_file())
      )

    case hasOldStuff do
      true ->
        :mnesia_log.open_decision_log()
        dump_decision_log(:startup)
        dump_decision_log(:startup)
        :mnesia_log.close_decision_log()
        latest = :mnesia_log.decision_log_file()
        :ok = :file.delete(latest)

      false ->
        :ignore
    end
  end

  defp dump_decision_log(initBy) do
    cont = :mnesia_log.prepare_decision_log_dump()
    perform_dump_decision_log(cont, initBy)
  end

  defp perform_dump_decision_log(cont, initBy) when initBy == :startup do
    case :mnesia_log.chunk_decision_log(cont) do
      {cont2, decisions} ->
        note_log_decisions(decisions, initBy)
        perform_dump_decision_log(cont2, initBy)

      :eof ->
        confirm_decision_log_dump()
    end
  end

  defp perform_dump_decision_log(_Cont, _InitBy) do
    confirm_decision_log_dump()
  end

  defp confirm_decision_log_dump() do
    dump_decision_tab()
    :mnesia_log.confirm_decision_log_dump()
  end

  def dump_decision_tab() do
    tab = :mnesia_decision
    all = :mnesia_lib.db_match_object(:ram_copies, tab, :_)
    :mnesia_log.save_decision_tab({:decision_list, all})
  end

  defp note_log_decisions([what | tail], initBy) do
    note_log_decision(what, initBy)
    note_log_decisions(tail, initBy)
  end

  defp note_log_decisions([], _InitBy) do
    :ok
  end

  def note_log_decision(newD, initBy)
      when r_decision(newD, :outcome) == :pre_commit do
    note_log_decision(r_decision(newD, outcome: :unclear), initBy)
  end

  def note_log_decision(newD, _InitBy)
      when elem(newD, 0) === :decision do
    tid = r_decision(newD, :tid)
    sync_trans_tid_serial(tid)
    note_outcome(newD)
  end

  def note_log_decision({:trans_tid, :serial, _Serial}, :startup) do
    :ignore
  end

  def note_log_decision({:trans_tid, :serial, serial}, _InitBy) do
    sync_trans_tid_serial(serial)
  end

  def note_log_decision({:mnesia_up, node, date, time}, _InitBy) do
    note_up(node, date, time)
  end

  def note_log_decision({:mnesia_down, node, date, time}, _InitBy) do
    note_down(node, date, time)
  end

  def note_log_decision({:master_nodes, tab, nodes}, _InitBy) do
    note_master_nodes(tab, nodes)
  end

  def note_log_decision(h, _InitBy)
      when r_log_header(h, :log_kind) == :decision_log do
    v = :mnesia_log.decision_log_version()

    cond do
      r_log_header(h, :log_version) == v ->
        :ok

      r_log_header(h, :log_version) == '2.0' ->
        verbose('Accepting an old version format of decision log: ~p~n', [v])
        :ok

      true ->
        fatal('Bad version of decision log: ~p~n', [h])
    end
  end

  def note_log_decision(h, _InitBy)
      when r_log_header(h, :log_kind) == :decision_tab do
    v = :mnesia_log.decision_tab_version()

    cond do
      v == r_log_header(h, :log_version) ->
        :ok

      true ->
        fatal('Bad version of decision tab: ~p~n', [h])
    end
  end

  def note_log_decision({:decision_list, itemList}, initBy) do
    note_log_decisions(itemList, initBy)
  end

  def note_log_decision(badItem, initBy) do
    exit({'Bad decision log item', badItem, initBy})
  end

  defp trans_tid_serial() do
    :ets.lookup_element(:mnesia_decision, :serial, 3)
  end

  defp set_trans_tid_serial(val) do
    :ets.insert(
      :mnesia_decision,
      {:trans_tid, :serial, val}
    )
  end

  def incr_trans_tid_serial() do
    :ets.update_counter(:mnesia_decision, :serial, 1)
  end

  def sync_trans_tid_serial(thatCounter) when is_integer(thatCounter) do
    thisCounter = trans_tid_serial()

    cond do
      thatCounter > thisCounter ->
        set_trans_tid_serial(thatCounter + 1)

      true ->
        :ignore
    end
  end

  def sync_trans_tid_serial(tid) do
    sync_trans_tid_serial(r_tid(tid, :counter))
  end

  def init([parent]) do
    :erlang.process_flag(:trap_exit, true)
    :mnesia_lib.verbose('~p starting: ~p~n', [:mnesia_recover, self()])

    set(
      :latest_transient_decision,
      create_transient_decision()
    )

    set(:previous_transient_decisions, [])
    set(:recover_nodes, [])
    state = r_state(supervisor: parent)
    {:ok, state}
  end

  defp create_transient_decision() do
    _ =
      :ets.new(
        :mnesia_transient_decision,
        [{:keypos, 2}, :set, :public]
      )
  end

  def handle_call(:init, from, state)
      when r_state(state, :initiated) == false do
    args = [{:keypos, 2}, :set, :public, :named_table]

    case :mnesia_monitor.use_dir() do
      true ->
        _ = :ets.new(:mnesia_decision, args)
        set_trans_tid_serial(0)
        tabFile = :mnesia_log.decision_tab_file()

        case :mnesia_lib.exists(tabFile) do
          true ->
            load_decision_tab()

          false ->
            :ignore
        end

        convert_old()
        :mnesia_dumper.opt_dump_log(:scan_decisions)

      false ->
        _ = :ets.new(:mnesia_decision, args)
        set_trans_tid_serial(0)
    end

    handle_early_msgs(state, from)
  end

  def handle_call(msg, from, state)
      when r_state(state, :initiated) == false do
    msgs = r_state(state, :early_msgs)
    {:noreply, r_state(state, early_msgs: [{:call, msg, from} | msgs])}
  end

  def handle_call({:disconnect, node}, _From, state) do
    :mnesia_monitor.disconnect(node)
    :mnesia_lib.del(:recover_nodes, node)
    {:reply, :ok, state}
  end

  def handle_call({:connect_nodes, ns}, from, state) do
    alreadyConnected = val(:recover_nodes)
    {_, nodes} = :mnesia_lib.search_delete(node(), ns)
    check = nodes -- alreadyConnected

    case :mnesia_monitor.negotiate_protocol(check) do
      :busy ->
        :erlang.send_after(2, self(), {:connect_nodes, ns, from})
        {:noreply, state}

      [] ->
        :gen_server.reply(from, {[], alreadyConnected})
        {:noreply, state}

      probablyGoodNodes ->
        verify = fn n ->
          run = :mnesia_lib.is_running(n)
          run === :yes or run === :starting
        end

        goodNodes =
          for n <- probablyGoodNodes, verify.(n) do
            n
          end

        :mnesia_lib.add_list(:recover_nodes, goodNodes)
        cast({:announce_all, goodNodes})

        case get_master_nodes(:schema) do
          [] ->
            context = :starting_partitioned_network
            :mnesia_monitor.detect_inconcistency(goodNodes, context)

          _ ->
            :ignore
        end

        :gen_server.reply(from, {goodNodes, alreadyConnected})
        {:noreply, state}
    end
  end

  def handle_call({:what_happened, default, tid}, _From, state) do
    sync_trans_tid_serial(tid)
    outcome = outcome(tid, default)
    {:reply, {:ok, outcome}, state}
  end

  def handle_call({:wait_for_decision, d}, from, state) do
    recov = val(:recover_nodes)

    aliveRam =
      :mnesia_lib.intersect(
        r_decision(d, :ram_nodes),
        recov
      ) -- [node()]

    remoteDisc = r_decision(d, :disc_nodes) -- [node()]

    cond do
      aliveRam == [] and remoteDisc == [] ->
        {:reply, {:ok, :aborted}, state}

      true ->
        verbose('Transaction ~p is unclear. Wait for disc nodes: ~w ram: ~w~n', [
          r_decision(d, :tid),
          remoteDisc,
          aliveRam
        ])

        aliveDisc = :mnesia_lib.intersect(remoteDisc, recov)
        msg = {:what_decision, node(), d}
        abcast(aliveRam, msg)
        abcast(aliveDisc, msg)

        case val(:max_wait_for_decision) do
          :infinity ->
            :ignore

          maxWait ->
            forceMsg = {:force_decision, r_decision(d, :tid)}
            {:ok, _} = :timer.send_after(maxWait, forceMsg)
        end

        state2 =
          r_state(state,
            unclear_pid: from,
            unclear_decision: d,
            unclear_waitfor: remoteDisc ++ aliveRam
          )

        {:noreply, state2}
    end
  end

  def handle_call({:log_mnesia_up, node}, _From, state) do
    do_log_mnesia_up(node)
    {:reply, :ok, state}
  end

  def handle_call({:log_mnesia_down, node}, _From, state) do
    do_log_mnesia_down(node)
    {:reply, :ok, state}
  end

  def handle_call({:log_master_nodes, tab, nodes, useDir, isRunning}, _From, state) do
    do_log_master_nodes(tab, nodes, useDir, isRunning)
    {:reply, :ok, state}
  end

  def handle_call(:sync, _From, state) do
    {:reply, :ok, state}
  end

  def handle_call(msg, _From, state) do
    :erlang.error('~p got unexpected call: ~tp~n', [:mnesia_recover, msg])
    {:noreply, state}
  end

  defp do_log_mnesia_up(node) do
    yoyo = {:mnesia_up, node, date = :erlang.date(), time = :erlang.time()}

    case :mnesia_monitor.use_dir() do
      true ->
        :mnesia_log.append(:latest_log, yoyo)
        :disk_log.sync(:latest_log)

      false ->
        :ignore
    end

    note_up(node, date, time)
  end

  defp do_log_mnesia_down(node) do
    yoyo = {:mnesia_down, node, date = :erlang.date(), time = :erlang.time()}

    case :mnesia_monitor.use_dir() do
      true ->
        :mnesia_log.append(:latest_log, yoyo)
        :disk_log.sync(:latest_log)

      false ->
        :ignore
    end

    note_down(node, date, time)
  end

  defp do_log_master_nodes(tab, nodes, useDir, isRunning) do
    master = {:master_nodes, tab, nodes}

    res =
      case useDir do
        true ->
          logRes = :mnesia_log.append(:latest_log, master)
          :disk_log.sync(:latest_log)
          logRes

        false ->
          :ok
      end

    case isRunning do
      :yes ->
        note_master_nodes(tab, nodes)

      _NotRunning ->
        :ignore
    end

    res
  end

  def handle_cast(msg, state) when r_state(state, :initiated) == false do
    msgs = r_state(state, :early_msgs)
    {:noreply, r_state(state, early_msgs: [{:cast, msg} | msgs])}
  end

  def handle_cast({:im_certain, node, newD}, state) do
    oldD = decision(r_decision(newD, :tid))
    mergedD = merge_decisions(node, oldD, newD)
    do_log_decision(mergedD, false, :undefined)
    {:noreply, state}
  end

  def handle_cast({:log_decision, d}, state) do
    do_log_decision(d)
    {:noreply, state}
  end

  def handle_cast(:allow_garb, state) do
    do_allow_garb()
    {:noreply, state}
  end

  def handle_cast({:decisions, node, decisions}, state) do
    :mnesia_lib.add(:recover_nodes, node)
    state2 = add_remote_decisions(node, decisions, state)
    {:noreply, state2}
  end

  def handle_cast({:what_decision, node, otherD}, state) do
    tid = r_decision(otherD, :tid)
    sync_trans_tid_serial(tid)

    decision =
      case decision(tid) do
        :no_decision ->
          otherD

        myD when elem(myD, 0) === :decision ->
          myD
      end

    announce([node], [decision], [], true)
    {:noreply, state}
  end

  def handle_cast({:mnesia_down, node}, state) do
    :mnesia_lib.del(:recover_nodes, node)

    case r_state(state, :unclear_decision) do
      :undefined ->
        {:noreply, state}

      d ->
        case :lists.member(node, r_decision(d, :ram_nodes)) do
          false ->
            {:noreply, state}

          true ->
            state2 = add_remote_decision(node, d, state)
            {:noreply, state2}
        end
    end
  end

  def handle_cast({:announce_all, nodes}, state) do
    announce_all(nodes)
    {:noreply, state}
  end

  def handle_cast({:log_dump_overload, flag}, state)
      when is_boolean(flag) do
    prev = r_state(state, :log_dump_overload)
    overload = prev or flag
    :mnesia_lib.overload_set(:mnesia_dump_log, overload)
    {:noreply, r_state(state, log_dump_overload: flag)}
  end

  def handle_cast(msg, state) do
    :erlang.error('~p got unexpected cast: ~tp~n', [:mnesia_recover, msg])
    {:noreply, state}
  end

  def handle_info({:connect_nodes, ns, from}, state) do
    handle_call({:connect_nodes, ns}, from, state)
  end

  def handle_info(:check_overload, s) do
    state2 = do_check_overload(s)
    next_check_overload()
    {:noreply, state2}
  end

  def handle_info(:garb_decisions, state) do
    do_garb_decisions()
    next_garb()
    {:noreply, state}
  end

  def handle_info({:force_decision, tid}, state) do
    case r_state(state, :unclear_decision) do
      u when r_decision(u, :tid) == tid ->
        verbose(
          'Decided to abort transaction ~p since max_wait_for_decision has been exceeded~n',
          [tid]
        )

        d = r_decision(u, outcome: :aborted)
        state2 = add_remote_decision(node(), d, state)
        {:noreply, state2}

      _ ->
        {:noreply, state}
    end
  end

  def handle_info({:EXIT, pid, r}, state)
      when pid == r_state(state, :supervisor) do
    :mnesia_lib.dbg_out('~p was ~tp~n', [:mnesia_recover, r])
    {:stop, :shutdown, state}
  end

  def handle_info(msg, state) do
    :erlang.error('~p got unexpected info: ~tp~n', [:mnesia_recover, msg])
    {:noreply, state}
  end

  def terminate(reason, state) do
    :mnesia_monitor.terminate_proc(:mnesia_recover, reason, state)
  end

  def code_change(
        _OldVsn,
        {:state, supervisor, unclear_pid, unclear_decision, unclear_waitfor, tm_queue_len,
         initiated, early_msgs},
        _Extra
      ) do
    {:ok,
     r_state(
       supervisor: supervisor,
       unclear_pid: unclear_pid,
       unclear_decision: unclear_decision,
       unclear_waitfor: unclear_waitfor,
       tm_queue_len: tm_queue_len,
       initiated: initiated,
       early_msgs: early_msgs
     )}
  end

  def code_change(_OldVsn, r_state() = state, _Extra) do
    {:ok, state}
  end

  defp handle_early_msgs(state, from) do
    res =
      do_handle_early_msgs(
        r_state(state, :early_msgs),
        r_state(state, early_msgs: [], initiated: true)
      )

    :gen_server.reply(from, :ok)
    res
  end

  defp do_handle_early_msgs([msg | msgs], state) do
    case do_handle_early_msgs(msgs, state) do
      {:stop, reason, state2} ->
        {:stop, reason, state2}

      {:noreply, state2} ->
        handle_early_msg(msg, state2)
    end
  end

  defp do_handle_early_msgs([], state) do
    {:noreply, state}
  end

  defp handle_early_msg({:call, msg, from}, state) do
    case handle_call(msg, from, state) do
      {:reply, r, s} ->
        :gen_server.reply(from, r)
        {:noreply, s}

      other ->
        other
    end
  end

  defp handle_early_msg({:cast, msg}, state) do
    handle_cast(msg, state)
  end

  defp handle_early_msg({:info, msg}, state) do
    handle_info(msg, state)
  end

  defp tabs() do
    curr = val(:latest_transient_decision)
    prev = val(:previous_transient_decisions)
    [curr, :mnesia_decision | prev]
  end

  defp decision(tid) do
    decision(tid, tabs())
  end

  defp decision(tid, [tab | tabs]) do
    try do
      :ets.lookup(tab, tid)
    catch
      :error, _ ->
        decision(tid, tabs)
    else
      [d] when elem(d, 0) === :decision ->
        d

      [c] when elem(c, 0) === :transient_decision ->
        r_decision(
          tid: r_transient_decision(c, :tid),
          outcome: r_transient_decision(c, :outcome),
          disc_nodes: [],
          ram_nodes: []
        )

      [] ->
        decision(tid, tabs)
    end
  end

  defp decision(_Tid, []) do
    :no_decision
  end

  def outcome(tid, default) do
    outcome(tid, default, tabs())
  end

  defp outcome(tid, default, [tab | tabs]) do
    try do
      :ets.lookup_element(tab, tid, 3)
    catch
      :error, _ ->
        outcome(tid, default, tabs)
    end
  end

  defp outcome(_Tid, default, []) do
    default
  end

  defp filter_outcome(val) do
    case val do
      :unclear ->
        :unclear

      :aborted ->
        :aborted

      :presume_abort ->
        :aborted

      :committed ->
        :committed

      :pre_commit ->
        :unclear
    end
  end

  defp filter_aborted(d) when r_decision(d, :outcome) == :presume_abort do
    r_decision(d, outcome: :aborted)
  end

  defp filter_aborted(d) do
    d
  end

  defp merge_decisions(node, d, newD0) do
    newD = filter_aborted(newD0)

    cond do
      d == :no_decision and node() != node ->
        r_decision(newD, disc_nodes: [])

      d == :no_decision ->
        newD

      elem(d, 0) === :decision ->
        discNs = r_decision(d, :disc_nodes) -- [node(), node]
        oldD = filter_aborted(r_decision(d, disc_nodes: discNs))

        cond do
          r_decision(oldD, :outcome) == :unclear and
              r_decision(newD, :outcome) == :unclear ->
            d

          r_decision(oldD, :outcome) == r_decision(newD, :outcome) ->
            oldD

          r_decision(oldD, :outcome) == :committed and
              r_decision(newD, :outcome) == :aborted ->
            msg = {:inconsistent_database, :bad_decision, node}
            :mnesia_lib.report_system_event(msg)
            r_decision(oldD, outcome: :aborted)

          r_decision(oldD, :outcome) == :aborted ->
            r_decision(oldD, outcome: :aborted)

          r_decision(newD, :outcome) == :aborted ->
            r_decision(oldD, outcome: :aborted)

          r_decision(oldD, :outcome) == :committed and
              r_decision(newD, :outcome) == :unclear ->
            r_decision(oldD, outcome: :committed)

          r_decision(oldD, :outcome) == :unclear and
              r_decision(newD, :outcome) == :committed ->
            r_decision(oldD, outcome: :committed)
        end
    end
  end

  defp add_remote_decisions(node, [d | tail], state)
       when elem(d, 0) === :decision do
    state2 = add_remote_decision(node, d, state)
    add_remote_decisions(node, tail, state2)
  end

  defp add_remote_decisions(node, [c | tail], state)
       when elem(c, 0) === :transient_decision do
    d =
      r_decision(
        tid: r_transient_decision(c, :tid),
        outcome: r_transient_decision(c, :outcome),
        disc_nodes: [],
        ram_nodes: []
      )

    state2 = add_remote_decision(node, d, state)
    add_remote_decisions(node, tail, state2)
  end

  defp add_remote_decisions(node, [{:mnesia_down, _, _, _} | tail], state) do
    add_remote_decisions(node, tail, state)
  end

  defp add_remote_decisions(node, [{:trans_tid, :serial, serial} | tail], state) do
    sync_trans_tid_serial(serial)

    case r_state(state, :unclear_decision) do
      :undefined ->
        :ignored

      d ->
        case :lists.member(node, r_decision(d, :ram_nodes)) do
          true ->
            :ignore

          false ->
            abcast([node], {:what_decision, node(), d})
        end
    end

    add_remote_decisions(node, tail, state)
  end

  defp add_remote_decisions(_Node, [], state) do
    state
  end

  defp add_remote_decision(node, newD, state) do
    tid = r_decision(newD, :tid)
    oldD = decision(tid)
    d = merge_decisions(node, oldD, newD)
    do_log_decision(d, false, :undefined)
    outcome = r_decision(d, :outcome)

    cond do
      oldD == :no_decision ->
        :ignore

      outcome == :unclear ->
        :ignore

      true ->
        case :erlang.or(
               :lists.member(
                 node(),
                 r_decision(newD, :disc_nodes)
               ),
               :lists.member(node(), r_decision(newD, :ram_nodes))
             ) do
          true ->
            tell_im_certain([node], d)

          false ->
            :ignore
        end
    end

    case r_state(state, :unclear_decision) do
      u when r_decision(u, :tid) == tid ->
        waitFor = r_state(state, :unclear_waitfor) -- [node]

        cond do
          outcome == :unclear and waitFor == [] ->
            newOutcome = :aborted
            certainD = r_decision(d, outcome: newOutcome, disc_nodes: [], ram_nodes: [])
            tell_im_certain(r_decision(d, :disc_nodes), certainD)
            tell_im_certain(r_decision(d, :ram_nodes), certainD)
            do_log_decision(certainD, false, :undefined)

            verbose('Decided to abort transaction ~p since everybody are uncertain ~p~n', [
              tid,
              certainD
            ])

            :gen_server.reply(
              r_state(state, :unclear_pid),
              {:ok, newOutcome}
            )

            r_state(state,
              unclear_pid: :undefined,
              unclear_decision: :undefined,
              unclear_waitfor: :undefined
            )

          outcome != :unclear ->
            verbose('~p told us that transaction ~p was ~p~n', [node, tid, outcome])

            :gen_server.reply(
              r_state(state, :unclear_pid),
              {:ok, outcome}
            )

            r_state(state,
              unclear_pid: :undefined,
              unclear_decision: :undefined,
              unclear_waitfor: :undefined
            )

          outcome == :unclear ->
            r_state(state, unclear_waitfor: waitFor)
        end

      _ ->
        state
    end
  end

  defp announce_all([]) do
    :ok
  end

  defp announce_all(toNodes) do
    tid = trans_tid_serial()
    announce(toNodes, [{:trans_tid, :serial, tid}], [], false)
  end

  defp announce(toNodes, [head | tail], acc, forceSend) do
    acc2 = arrange(toNodes, head, acc, forceSend)
    announce(toNodes, tail, acc2, forceSend)
  end

  defp announce(_ToNodes, [], acc, _ForceSend) do
    send_decisions(acc)
  end

  defp send_decisions([{node, decisions} | tail]) do
    abcast([node], {:decisions, node(), decisions})
    send_decisions(tail)
  end

  defp send_decisions([]) do
    :ok
  end

  defp arrange([to | toNodes], d, acc, forceSend)
       when elem(d, 0) === :decision do
    needsAdd =
      :erlang.or(
        :erlang.or(
          forceSend,
          :lists.member(to, r_decision(d, :disc_nodes))
        ),
        :lists.member(to, r_decision(d, :ram_nodes))
      )

    case needsAdd do
      true ->
        acc2 = add_decision(to, d, acc)
        arrange(toNodes, d, acc2, forceSend)

      false ->
        arrange(toNodes, d, acc, forceSend)
    end
  end

  defp arrange([to | toNodes], {:trans_tid, :serial, serial}, acc, forceSend) do
    acc2 = add_decision(to, {:trans_tid, :serial, serial}, acc)
    arrange(toNodes, {:trans_tid, :serial, serial}, acc2, forceSend)
  end

  defp arrange([], _Decision, acc, _ForceSend) do
    acc
  end

  defp add_decision(node, decision, [{node, decisions} | tail]) do
    [{node, [decision | decisions]} | tail]
  end

  defp add_decision(node, decision, [head | tail]) do
    [head | add_decision(node, decision, tail)]
  end

  defp add_decision(node, decision, []) do
    [{node, [decision]}]
  end
end
