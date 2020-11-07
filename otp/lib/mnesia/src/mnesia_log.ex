defmodule :m_mnesia_log do
  use Bitwise
  import :mnesia_lib, only: [dbg_out: 2, dir: 1, error: 2, exists: 1, fatal: 2, val: 1]
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

  def trans_log_header() do
    log_header(:trans_log, version())
  end

  def backup_log_header() do
    log_header(:backup_log, '1.2')
  end

  defp decision_log_header() do
    log_header(:decision_log, decision_log_version())
  end

  defp decision_tab_header() do
    log_header(:decision_tab, decision_tab_version())
  end

  def dcl_log_header() do
    log_header(:dcl_log, dcl_version())
  end

  def dcd_log_header() do
    log_header(:dcd_log, dcd_version())
  end

  defp log_header(kind, version) do
    r_log_header(
      log_version: version,
      log_kind: kind,
      mnesia_version: :mnesia.system_info(:version),
      node: node(),
      now: :erlang.timestamp()
    )
  end

  def version() do
    '4.3'
  end

  def decision_log_version() do
    '3.0'
  end

  def decision_tab_version() do
    '1.0'
  end

  def dcl_version() do
    '1.0'
  end

  def dcd_version() do
    '1.0'
  end

  def append(log, bin) when is_binary(bin) do
    :disk_log.balog(log, bin)
  end

  def append(log, term) do
    :disk_log.alog(log, term)
  end

  defp sappend(log, bin) when is_binary(bin) do
    :ok = :disk_log.blog(log, bin)
  end

  defp sappend(log, term) do
    :ok = :disk_log.log(log, term)
  end

  def log(c) do
    case need_log(c) and :mnesia_monitor.use_dir() do
      true ->
        cond do
          elem(c, 0) === :commit ->
            append(:latest_log, strip_snmp(c))

          true ->
            append(:latest_log, c)
        end

        :mnesia_dumper.incr_log_writes()

      false ->
        :ignore
    end
  end

  def slog(c) do
    case need_log(c) and :mnesia_monitor.use_dir() do
      true ->
        cond do
          elem(c, 0) === :commit ->
            sappend(:latest_log, strip_snmp(c))

          true ->
            sappend(:latest_log, c)
        end

        :mnesia_dumper.incr_log_writes()

      false ->
        :ignore
    end
  end

  defp need_log(r_commit(disc_copies: [], disc_only_copies: [], schema_ops: [], ext: ext)) do
    :lists.keymember(:ext_copies, 1, ext)
  end

  defp need_log(_) do
    true
  end

  defp strip_snmp(r_commit(ext: []) = cR) do
    cR
  end

  defp strip_snmp(r_commit(ext: ext) = cR) do
    r_commit(cR, ext: :lists.keydelete(:snmp, 1, ext))
  end

  def log_files() do
    [previous_log_file(), latest_log_file(), decision_tab_file()]
  end

  def latest_log_file() do
    dir(latest_log_name())
  end

  def previous_log_file() do
    dir('PREVIOUS.LOG')
  end

  def decision_log_file() do
    dir(decision_log_name())
  end

  def decision_tab_file() do
    dir(decision_tab_name())
  end

  def previous_decision_log_file() do
    dir('PDECISION.LOG')
  end

  defp latest_log_name() do
    'LATEST.LOG'
  end

  defp decision_log_name() do
    'DECISION.LOG'
  end

  defp decision_tab_name() do
    'DECISION_TAB.LOG'
  end

  def init() do
    case :mnesia_monitor.use_dir() do
      true ->
        prev = previous_log_file()
        verify_no_exists(prev)
        latest = latest_log_file()
        verify_no_exists(latest)
        header = trans_log_header()
        open_log(:latest_log, header, latest)

      false ->
        :ok
    end
  end

  defp verify_no_exists(fname) do
    case exists(fname) do
      false ->
        :ok

      true ->
        fatal('Log file exists: ~tp~n', [fname])
    end
  end

  defp open_log(name, header, fname) do
    exists = exists(fname)
    open_log(name, header, fname, exists)
  end

  def open_log(name, header, fname, exists) do
    repair = :mnesia_monitor.get_env(:auto_repair)
    open_log(name, header, fname, exists, repair)
  end

  defp open_log(name, header, fname, exists, repair) do
    case name == :previous_log do
      true ->
        open_log(name, header, fname, exists, repair, :read_only)

      false ->
        open_log(name, header, fname, exists, repair, :read_write)
    end
  end

  def open_log(name, header, fname, exists, repair, mode) do
    args = [{:file, fname}, {:name, name}, {:repair, repair}, {:mode, mode}]

    case :mnesia_monitor.open_log(args) do
      {:ok, log} when exists == true ->
        log

      {:ok, log} ->
        write_header(log, header)
        log

      {:repaired, log, _, {:badbytes, 0}} when exists == true ->
        log

      {:repaired, log, _, {:badbytes, 0}} ->
        write_header(log, header)
        log

      {:repaired, log, _Recover, badBytes} ->
        :mnesia_lib.important('Data may be missing, log ~tp repaired: Lost ~p bytes~n', [
          fname,
          badBytes
        ])

        log

      {:error, reason = {:file_error, _Fname, :emfile}} ->
        fatal('Cannot open log file ~tp: ~tp~n', [fname, reason])

      {:error, reason} when repair == true ->
        :file.delete(fname)

        :mnesia_lib.important('Data may be missing, Corrupt logfile deleted: ~tp, ~tp ~n', [
          fname,
          reason
        ])

        open_log(name, header, fname, false, false, :read_write)

      {:error, reason} ->
        fatal('Cannot open log file ~tp: ~tp~n', [fname, reason])
    end
  end

  defp write_header(log, header) do
    append(log, header)
  end

  def write_trans_log_header() do
    write_header(:latest_log, trans_log_header())
  end

  def stop() do
    case :mnesia_monitor.use_dir() do
      true ->
        close_log(:latest_log)

      false ->
        :ok
    end
  end

  def close_log(log) do
    case :disk_log.sync(log) do
      :ok ->
        :ok

      {:error, {:read_only_mode, ^log}} ->
        :ok

      {:error, reason} ->
        :mnesia_lib.important('Failed syncing ~tp to_disk reason ~tp ~n', [log, reason])
    end

    :mnesia_monitor.close_log(log)
  end

  def unsafe_close_log(log) do
    :mnesia_monitor.unsafe_close_log(log)
  end

  def purge_some_logs() do
    :mnesia_monitor.unsafe_close_log(:latest_log)
    _ = :file.delete(latest_log_file())
    _ = :file.delete(decision_tab_file())
    :ok
  end

  def purge_all_logs() do
    _ = :file.delete(previous_log_file())
    _ = :file.delete(latest_log_file())
    _ = :file.delete(decision_tab_file())
    :ok
  end

  def prepare_log_dump(initBy) do
    diff = :mnesia_dumper.get_log_writes() - :mnesia_lib.read_counter(:trans_log_writes_prev)

    cond do
      diff == 0 and initBy != :startup ->
        :already_dumped

      true ->
        case :mnesia_monitor.use_dir() do
          true ->
            prev = previous_log_file()
            prepare_prev(diff, initBy, prev, exists(prev))

          false ->
            :already_dumped
        end
    end
  end

  defp prepare_prev(diff, _, _, true) do
    {:needs_dump, diff}
  end

  defp prepare_prev(diff, :startup, prev, false) do
    latest = latest_log_file()

    case exists(latest) do
      true ->
        case :file.rename(latest, prev) do
          :ok ->
            {:needs_dump, diff}

          {:error, reason} ->
            {:error, reason}
        end

      false ->
        :already_dumped
    end
  end

  defp prepare_prev(diff, _InitBy, prev, false) do
    head = trans_log_header()

    case :mnesia_monitor.reopen_log(:latest_log, prev, head) do
      :ok ->
        {:needs_dump, diff}

      {:error, reason} ->
        latest = latest_log_file()
        {:error, {'Cannot rename log file', [latest, prev, reason]}}
    end
  end

  def init_log_dump() do
    fname = previous_log_file()
    open_log(:previous_log, trans_log_header(), fname)
    :start
  end

  def chunk_log(cont) do
    chunk_log(:previous_log, cont)
  end

  def chunk_log(_Log, :eof) do
    :eof
  end

  def chunk_log(log, cont) do
    case :disk_log.chunk(log, cont) do
      {:error, reason} ->
        fatal('Possibly truncated ~tp file: ~tp~n', [log, reason])

      {c2, chunk, _BadBytes} ->
        :mnesia_lib.important('~tp repaired, lost ~p bad bytes~n', [log, _BadBytes])
        {c2, chunk}

      other ->
        other
    end
  end

  def confirm_log_dump(updates) do
    case :mnesia_monitor.close_log(:previous_log) do
      :ok ->
        :file.delete(previous_log_file())

        :mnesia_lib.incr_counter(
          :trans_log_writes_prev,
          updates
        )

        :dumped

      {:error, reason} ->
        {:error, reason}
    end
  end

  def open_decision_log() do
    latest = decision_log_file()
    open_log(:decision_log, decision_log_header(), latest)
    :start
  end

  def prepare_decision_log_dump() do
    prev = previous_decision_log_file()
    prepare_decision_log_dump(exists(prev), prev)
  end

  defp prepare_decision_log_dump(false, prev) do
    head = decision_log_header()

    case :mnesia_monitor.reopen_log(:decision_log, prev, head) do
      :ok ->
        prepare_decision_log_dump(true, prev)

      {:error, reason} ->
        fatal('Cannot rename decision log file ~tp -> ~tp: ~tp~n', [
          decision_log_file(),
          prev,
          reason
        ])
    end
  end

  defp prepare_decision_log_dump(true, prev) do
    open_log(:previous_decision_log, decision_log_header(), prev)
    :start
  end

  def chunk_decision_log(cont) do
    chunk_log(:previous_decision_log, cont)
  end

  def confirm_decision_log_dump() do
    case :mnesia_monitor.close_log(:previous_decision_log) do
      :ok ->
        :file.delete(previous_decision_log_file())

      {:error, reason} ->
        fatal('Cannot confirm decision log dump: ~tp~n', [reason])
    end
  end

  def save_decision_tab(decisions) do
    log = :decision_tab
    tmp = :mnesia_lib.dir('DECISION_TAB.TMP')
    :file.delete(tmp)
    open_log(log, decision_tab_header(), tmp)
    append(log, decisions)
    close_log(log)
    tabFile = decision_tab_file()
    :ok = :file.rename(tmp, tabFile)
  end

  def open_decision_tab() do
    tabFile = decision_tab_file()
    open_log(:decision_tab, decision_tab_header(), tabFile)
    :start
  end

  def close_decision_tab() do
    close_log(:decision_tab)
  end

  def chunk_decision_tab(cont) do
    chunk_log(:decision_tab, cont)
  end

  def close_decision_log() do
    close_log(:decision_log)
  end

  def log_decision(decision) do
    append(:decision_log, decision)
  end

  def view() do
    :lists.foreach(
      fn f ->
        view(f)
      end,
      log_files()
    )
  end

  def view(file) do
    :mnesia_lib.show('*****  ~tp ***** ~n', [file])

    case exists(file) do
      false ->
        :nolog

      true ->
        n = :view_only
        args = [{:file, file}, {:name, n}, {:mode, :read_only}]

        case :disk_log.open(args) do
          {:ok, ^n} ->
            view_file(:start, n)

          {:repaired, _, _, _} ->
            view_file(:start, n)

          {:error, reason} ->
            :erlang.error('Cannot open log ~tp: ~tp~n', [file, reason])
        end
    end
  end

  defp view_file(c, log) do
    case :disk_log.chunk(log, c) do
      {:error, reason} ->
        :erlang.error('** Possibly truncated FILE ~tp~n', [reason])
        :error

      :eof ->
        :disk_log.close(log)
        :eof

      {c2, terms, _BadBytes} ->
        dbg_out('Lost ~p bytes in ~tp ~n', [_BadBytes, log])

        :lists.foreach(
          fn x ->
            :mnesia_lib.show('~tp~n', [x])
          end,
          terms
        )

        view_file(c2, log)

      {c2, terms} ->
        :lists.foreach(
          fn x ->
            :mnesia_lib.show('~tp~n', [x])
          end,
          terms
        )

        view_file(c2, log)
    end
  end

  Record.defrecord(:r_backup_args, :backup_args,
    name: :undefined,
    module: :undefined,
    opaque: :undefined,
    scope: :undefined,
    prev_name: :undefined,
    tables: :undefined,
    cookie: :undefined
  )

  def backup(opaque) do
    backup(opaque, [])
  end

  def backup(opaque, mod) when is_atom(mod) do
    backup(opaque, [{:module, mod}])
  end

  def backup(opaque, args) when is_list(args) do
    cpArgs = [{:ram_overrides_dump, false}, {:max, val({:schema, :tables})}]

    case :mnesia_checkpoint.activate(cpArgs) do
      {:ok, name, _Nodes} ->
        res = backup_checkpoint(name, opaque, args)
        :mnesia_checkpoint.deactivate(name)
        res

      {:error, reason} ->
        {:error, reason}
    end
  end

  def backup_checkpoint(name, opaque) do
    backup_checkpoint(name, opaque, [])
  end

  def backup_checkpoint(name, opaque, mod) when is_atom(mod) do
    backup_checkpoint(name, opaque, [{:module, mod}])
  end

  def backup_checkpoint(name, opaque, args) when is_list(args) do
    defaultMod = :mnesia_monitor.get_env(:backup_module)

    b =
      r_backup_args(
        name: name,
        module: defaultMod,
        opaque: opaque,
        scope: :global,
        tables: :all,
        prev_name: name
      )

    case check_backup_args(args, b) do
      {:ok, b2} ->
        self = self()
        pid = spawn_link(:mnesia_log, :backup_master, [self, b2])

        receive do
          {^pid, ^self, res} ->
            res
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp check_backup_args([arg | tail], b) do
    try do
      check_backup_arg_type(arg, b)
    catch
      :error, _ ->
        {:error, {:badarg, arg}}
    else
      b2 ->
        check_backup_args(tail, b2)
    end
  end

  defp check_backup_args([], b) do
    {:ok, b}
  end

  defp check_backup_arg_type(arg, b) do
    case arg do
      {:scope, :global} ->
        r_backup_args(b, scope: :global)

      {:scope, :local} ->
        r_backup_args(b, scope: :local)

      {:module, mod} ->
        mod2 =
          :mnesia_monitor.do_check_type(
            :backup_module,
            mod
          )

        r_backup_args(b, module: mod2)

      {:incremental, name} ->
        r_backup_args(b, prev_name: name)

      {:tables, tabs} when is_list(tabs) ->
        r_backup_args(b, tables: tabs)
    end
  end

  def backup_master(clientPid, b) do
    :erlang.process_flag(:trap_exit, true)

    try do
      do_backup_master(b)
    catch
      _, reason ->
        send(clientPid, {self(), clientPid, {:error, {:EXIT, reason}}})
    else
      res ->
        send(clientPid, {self(), clientPid, res})
    end

    :erlang.unlink(clientPid)
    exit(:normal)
  end

  defp do_backup_master(b) do
    name = r_backup_args(b, :name)
    b2 = safe_apply(b, :open_write, [r_backup_args(b, :opaque)])
    b3 = safe_write(b2, [backup_log_header()])

    case :mnesia_checkpoint.tables_and_cookie(name) do
      {:ok, allTabs, cookie} ->
        tabs = select_tables(allTabs, b3)
        b4 = r_backup_args(b3, cookie: cookie)
        b5 = backup_schema(b4, tabs)
        b6 = :lists.foldl(&backup_tab/2, b5, tabs -- [:schema])
        safe_apply(b6, :commit_write, [r_backup_args(b6, :opaque)])
        :ok

      {:error, reason} ->
        abort_write(b3, {:mnesia_log, :backup_master}, [b], {:error, reason})
    end
  end

  defp select_tables(allTabs, b) do
    tabs =
      case r_backup_args(b, :tables) do
        :all ->
          allTabs

        someTabs when is_list(someTabs) ->
          someTabs
      end

    case r_backup_args(b, :scope) do
      :global ->
        tabs

      :local ->
        name = r_backup_args(b, :name)

        for t <- tabs,
            :mnesia_checkpoint.most_local_node(name, t) == {:ok, node()} do
          t
        end
    end
  end

  defp safe_write(b, []) do
    b
  end

  defp safe_write(b, recs) do
    safe_apply(b, :write, [r_backup_args(b, :opaque), recs])
  end

  defp backup_schema(b, tabs) do
    case :lists.member(:schema, tabs) do
      true ->
        backup_tab(:schema, b)

      false ->
        defs =
          for t <- tabs do
            {:schema, t, :mnesia_schema.get_create_list(t)}
          end

        safe_write(b, defs)
    end
  end

  defp safe_apply(b, :write, [_, items]) when items == [] do
    b
  end

  defp safe_apply(b, what, args) do
    abort = abort_write_fun(b, what, args)

    receive do
      {:EXIT, pid, r} ->
        abort.({:EXIT, pid, r})
    after
      0 ->
        mod = r_backup_args(b, :module)

        try do
          apply(mod, what, args)
        catch
          _, r ->
            abort.(r)
        else
          {:ok, opaque} ->
            r_backup_args(b, opaque: opaque)

          {:error, r} ->
            abort.(r)
        end
    end
  end

  defp abort_write_fun(b, what, args) do
    fn r ->
      abort_write(b, what, args, r)
    end
  end

  defp abort_write(b, what, args, reason) do
    mod = r_backup_args(b, :module)
    opaque = r_backup_args(b, :opaque)
    dbg_out('Failed to perform backup. M=~p:F=~tp:A=~tp -> ~tp~n', [mod, what, args, reason])

    try do
      {:ok, _Res} = apply(mod, :abort_write, [opaque])
    catch
      _, other ->
        :erlang.error('Failed to abort backup. ~p:~tp~tp -> ~tp~n', [
          mod,
          :abort_write,
          [opaque],
          other
        ])

        throw({:error, reason})
    else
      _ ->
        throw({:error, reason})
    end
  end

  defp backup_tab(tab, b) do
    name = r_backup_args(b, :name)

    case :mnesia_checkpoint.most_local_node(name, tab) do
      {:ok, node} when node == node() ->
        tab_copier(self(), b, tab)

      {:ok, node} ->
        remoteB = b
        pid = :erlang.spawn_link(node, :mnesia_log, :tab_copier, [self(), remoteB, tab])
        recName = val({tab, :record_name})
        tab_receiver(pid, b, tab, recName, 0)

      {:error, reason} ->
        abort_write(b, {:mnesia_log, :backup_tab}, [tab, b], {:error, reason})
    end
  end

  def tab_copier(pid, b, tab) when elem(b, 0) === :backup_args do
    name = r_backup_args(b, :name)
    prevName = r_backup_args(b, :prev_name)
    {firstName, firstSource} = select_source(tab, name, prevName)
    :ok
    res = handle_more(pid, b, tab, firstName, firstSource, name)
    :ok
    handle_last(pid, res)
  end

  defp select_source(tab, name, prevName) do
    cond do
      tab == :schema ->
        {name, :table}

      name == prevName ->
        {name, :table}

      true ->
        case :mnesia_checkpoint.most_local_node(
               prevName,
               tab
             ) do
          {:ok, node} when node == node() ->
            {prevName, :retainer}

          _ ->
            dbg_out('Incremental backup escalated to full backup: ~tp~n', [tab])
            {name, :table}
        end
    end
  end

  defp handle_more(pid, b, tab, firstName, firstSource, name) do
    acc = {0, b}

    case {:mnesia_checkpoint.really_retain(name, tab),
          :mnesia_checkpoint.really_retain(firstName, tab)} do
      {true, true} ->
        acc2 = iterate(b, firstName, tab, pid, firstSource, :latest, :first, acc)
        iterate(b, name, tab, pid, :retainer, :checkpoint, :last, acc2)

      {false, false} ->
        iterate(b, name, tab, pid, :retainer, :checkpoint, :last, acc)

      bad ->
        reason =
          {'Checkpoints for incremental backup must have same setting of ram_overrides_dump', tab,
           name, firstName, bad}

        abort_write(b, {:mnesia_log, :backup_tab}, [tab, b], {:error, reason})
    end
  end

  defp handle_last(pid, {_Count, b}) when pid == self() do
    b
  end

  defp handle_last(pid, _Acc) do
    :erlang.unlink(pid)
    send(pid, {self(), {:last, {:ok, :dummy}}})
    exit(:normal)
  end

  defp iterate(b, name, tab, pid, source, age, pass, acc) do
    fun =
      cond do
        pid == self() ->
          recName = val({tab, :record_name})

          fn recs, a ->
            copy_records(recName, tab, recs, a)
          end

        true ->
          fn recs, a ->
            send_records(pid, tab, recs, pass, a)
          end
      end

    case :mnesia_checkpoint.iterate(name, tab, fun, acc, source, age) do
      {:ok, acc2} ->
        acc2

      {:error, reason} ->
        r = {:error, {'Tab copier iteration failed', reason}}
        abort_write(b, {:mnesia_log, :iterate}, [self(), b, tab], r)
    end
  end

  defp copy_records(_RecName, _Tab, [], acc) do
    acc
  end

  defp copy_records(recName, tab, recs, {count, b}) do
    recs2 = rec_filter(b, tab, recName, recs)
    b2 = safe_write(b, recs2)
    {count + 1, b2}
  end

  defp send_records(pid, tab, recs, pass, {count, b}) do
    receive do
      {^pid, :more, ^count} ->
        cond do
          pass == :last and recs == [] ->
            {count, b}

          true ->
            next = count + 1
            send(pid, {self(), {:more, next, recs}})
            {next, b}
        end

      msg ->
        exit({:send_records_unexpected_msg, tab, msg})
    end
  end

  defp tab_receiver(pid, b, tab, recName, slot) do
    send(pid, {self(), :more, slot})

    receive do
      {^pid, {:more, next, recs}} ->
        recs2 = rec_filter(b, tab, recName, recs)
        b2 = safe_write(b, recs2)
        tab_receiver(pid, b2, tab, recName, next)

      {^pid, {:last, {:ok, _}}} ->
        b

      {:EXIT, ^pid, {:error, r}} ->
        reason = {:error, {'Tab copier crashed', r}}
        abort_write(b, {:mnesia_log, :remote_tab_sender}, [self(), b, tab], reason)

      {:EXIT, ^pid, r} ->
        reason = {:error, {'Tab copier crashed', {:EXIT, r}}}
        abort_write(b, {:mnesia_log, :remote_tab_sender}, [self(), b, tab], reason)

      msg ->
        r = {:error, {'Tab receiver got unexpected msg', msg}}
        abort_write(b, {:mnesia_log, :remote_tab_sender}, [self(), b, tab], r)
    end
  end

  defp rec_filter(b, :schema, _RecName, recs) do
    try do
      :mnesia_bup.refresh_cookie(recs, r_backup_args(b, :cookie))
    catch
      {:error, _Reason} ->
        recs
    end
  end

  defp rec_filter(_B, tab, tab, recs) do
    recs
  end

  defp rec_filter(_B, tab, _RecName, recs) do
    for rec <- recs do
      :erlang.setelement(1, rec, tab)
    end
  end

  def ets2dcd(tab) do
    ets2dcd(tab, :dcd)
  end

  def ets2dcd(tab, ftype) do
    fname =
      case ftype do
        :dcd ->
          :mnesia_lib.tab2dcd(tab)

        :dmp ->
          :mnesia_lib.tab2dmp(tab)
      end

    tmpF = :mnesia_lib.tab2tmp(tab)
    :file.delete(tmpF)
    log = open_log({tab, :ets2dcd}, dcd_log_header(), tmpF, false)
    :mnesia_lib.db_fixtable(:ram_copies, tab, true)
    :ok = ets2dcd(:mnesia_lib.db_init_chunk(:ram_copies, tab, 1000), tab, log)
    :mnesia_lib.db_fixtable(:ram_copies, tab, false)
    close_log(log)
    :ok = :file.rename(tmpF, fname)
    :file.delete(:mnesia_lib.tab2dcl(tab))
    :ok
  end

  defp ets2dcd(:"$end_of_table", _Tab, _Log) do
    :ok
  end

  defp ets2dcd({recs, cont}, tab, log) do
    :ok = :disk_log.log_terms(log, recs)
    ets2dcd(:mnesia_lib.db_chunk(:ram_copies, cont), tab, log)
  end

  def dcd2ets(tab) do
    dcd2ets(tab, :mnesia_monitor.get_env(:auto_repair))
  end

  def dcd2ets(tab, rep) do
    dcd = :mnesia_lib.tab2dcd(tab)

    case :mnesia_lib.exists(dcd) do
      true ->
        log = open_log({tab, :dcd2ets}, dcd_log_header(), dcd, true, rep, :read_only)
        data = chunk_log(log, :start)
        :ok = insert_dcdchunk(data, log, tab)
        close_log(log)
        load_dcl(tab, rep)

      false ->
        fname = :mnesia_lib.tab2dat(tab)
        type = val({tab, :setorbag})

        case :mnesia_lib.dets_to_ets(tab, tab, fname, type, rep, :yes) do
          :loaded ->
            ets2dcd(tab)
            :file.delete(fname)
            0

          {:error, error} ->
            :erlang.error({'Failed to load table from disc', [tab, error]})
        end
    end
  end

  defp insert_dcdchunk({cont, [logH | rest]}, log, tab)
       when elem(logH, 0) === :log_header and
              r_log_header(logH, :log_kind) == :dcd_log and
              r_log_header(logH, :log_version) >= '1.0' do
    insert_dcdchunk({cont, rest}, log, tab)
  end

  defp insert_dcdchunk({cont, recs}, log, tab) do
    true = :ets.insert(tab, recs)
    insert_dcdchunk(chunk_log(log, cont), log, tab)
  end

  defp insert_dcdchunk(:eof, _Log, _Tab) do
    :ok
  end

  defp load_dcl(tab, rep) do
    fName = :mnesia_lib.tab2dcl(tab)

    case :mnesia_lib.exists(fName) do
      true ->
        name = {:load_dcl, tab}
        open_log(name, dcl_log_header(), fName, true, rep, :read_only)
        firstChunk = chunk_log(name, :start)
        n = insert_logchunk(firstChunk, name, 0)
        close_log(name)
        n

      false ->
        0
    end
  end

  defp insert_logchunk({c2, recs}, tab, c) do
    n = add_recs(recs, c)
    insert_logchunk(chunk_log(tab, c2), tab, c + n)
  end

  defp insert_logchunk(:eof, _Tab, c) do
    c
  end

  defp add_recs([{{tab, _Key}, val, :write} | rest], n) do
    true = :ets.insert(tab, val)
    add_recs(rest, n + 1)
  end

  defp add_recs([{{tab, key}, _Val, :delete} | rest], n) do
    true = :ets.delete(tab, key)
    add_recs(rest, n + 1)
  end

  defp add_recs(
         [{{tab, _Key}, val, :delete_object} | rest],
         n
       ) do
    true = :ets.match_delete(tab, val)
    add_recs(rest, n + 1)
  end

  defp add_recs(
         [{{tab, key}, val, :update_counter} | rest],
         n
       ) do
    {recName, incr} = val

    try do
      counterVal = :ets.update_counter(tab, key, incr)
      true = counterVal >= 0
    catch
      :error, _ when incr < 0 ->
        zero = {recName, key, 0}
        true = :ets.insert(tab, zero)

      :error, _ ->
        zero = {recName, key, incr}
        true = :ets.insert(tab, zero)
    end

    add_recs(rest, n + 1)
  end

  defp add_recs([logH | rest], n)
       when elem(logH, 0) === :log_header and
              r_log_header(logH, :log_kind) == :dcl_log and
              r_log_header(logH, :log_version) >= '1.0' do
    add_recs(rest, n)
  end

  defp add_recs(
         [{{tab, _Key}, _Val, :clear_table} | rest],
         n
       ) do
    size = :ets.info(tab, :size)
    true = :ets.delete_all_objects(tab)
    add_recs(rest, n + size)
  end

  defp add_recs([], n) do
    n
  end
end
