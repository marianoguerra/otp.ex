defmodule :m_mnesia_dumper do
  use Bitwise
  import :mnesia_lib, only: [dbg_out: 2, fatal: 2]
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

  Record.defrecord(:r_file_info, :file_info,
    size: :undefined,
    type: :undefined,
    access: :undefined,
    atime: :undefined,
    mtime: :undefined,
    ctime: :undefined,
    mode: :undefined,
    links: :undefined,
    major_device: :undefined,
    minor_device: :undefined,
    inode: :undefined,
    uid: :undefined,
    gid: :undefined
  )

  Record.defrecord(:r_file_descriptor, :file_descriptor,
    module: :undefined,
    data: :undefined
  )

  def get_log_writes() do
    max = :mnesia_monitor.get_env(:dump_log_write_threshold)
    prev = :mnesia_lib.read_counter(:trans_log_writes)
    left = :mnesia_lib.read_counter(:trans_log_writes_left)
    diff = max - left
    prev + diff
  end

  def incr_log_writes() do
    left =
      :mnesia_lib.incr_counter(
        :trans_log_writes_left,
        -1
      )

    cond do
      left === 0 ->
        adjust_log_writes(true)

      true ->
        :ignore
    end
  end

  defp adjust_log_writes(doCast) do
    token = {:mnesia_adjust_log_writes, self()}

    case :global.set_lock(token, [node()], 1) do
      false ->
        :ignore

      true ->
        case doCast do
          false ->
            :ignore

          true ->
            :mnesia_controller.async_dump_log(:write_threshold)
        end

        max = :mnesia_monitor.get_env(:dump_log_write_threshold)
        left = :mnesia_lib.read_counter(:trans_log_writes_left)
        :mnesia_lib.set_counter(:trans_log_writes_left, max)
        diff = max - left
        _ = :mnesia_lib.incr_counter(:trans_log_writes, diff)
        :global.del_lock(token, [node()])
    end
  end

  def opt_dump_log(initBy) do
    reg =
      case :erlang.whereis(:mnesia_dumper_load_regulator) do
        :undefined ->
          :nopid

        pid when is_pid(pid) ->
          pid
      end

    perform_dump(initBy, reg)
  end

  def snapshot_dcd(tables) do
    :lists.foreach(
      fn tab ->
        case :mnesia_lib.storage_type_at_node(node(), tab) do
          :disc_copies ->
            :mnesia_log.ets2dcd(tab)

          _ ->
            :skip
        end
      end,
      tables
    )

    :dumped
  end

  defp perform_dump(initBy, regulator)
       when initBy == :scan_decisions do
    :ok
    dbg_out('Transaction log dump initiated by ~w~n', [initBy])
    scan_decisions(:mnesia_log.previous_log_file(), initBy, regulator)
    scan_decisions(:mnesia_log.latest_log_file(), initBy, regulator)
  end

  defp perform_dump(initBy, regulator) do
    :ok
    logState = :mnesia_log.prepare_log_dump(initBy)
    dbg_out('Transaction log dump initiated by ~w: ~w~n', [initBy, logState])
    adjust_log_writes(false)

    case logState do
      :already_dumped ->
        :mnesia_recover.allow_garb()
        :dumped

      {:needs_dump, diff} ->
        u = :mnesia_monitor.get_env(:dump_log_update_in_place)
        cont = :mnesia_log.init_log_dump()
        :mnesia_recover.sync()

        try do
          do_perform_dump(cont, u, initBy, regulator, :undefined)
        catch
          :exit, reason when reason !== :fatal ->
            case :mnesia_monitor.get_env(:auto_repair) do
              true ->
                :mnesia_lib.important(:error, reason)
                :mnesia_log.confirm_log_dump(diff)

              false ->
                fatal(:error, reason)
            end
        else
          :ok ->
            :ok

            case :mnesia_monitor.use_dir() do
              true ->
                :mnesia_recover.dump_decision_tab()

              false ->
                :mnesia_log.purge_some_logs()
            end

            :mnesia_recover.allow_garb()
            :mnesia_log.confirm_log_dump(diff)
        end

      {:error, reason} ->
        {:error, {'Cannot prepare log dump', reason}}
    end
  end

  defp scan_decisions(fname, initBy, regulator) do
    exists = :mnesia_lib.exists(fname)

    case exists do
      false ->
        :ok

      true ->
        header = :mnesia_log.trans_log_header()
        name = :previous_log

        :mnesia_log.open_log(
          name,
          header,
          fname,
          exists,
          :mnesia_monitor.get_env(:auto_repair),
          :read_only
        )

        cont = :start

        try do
          do_perform_dump(cont, false, initBy, regulator, :undefined)
        catch
          :exit, reason when reason !== :fatal ->
            {:error, reason}
        after
          :mnesia_log.close_log(name)
        end
    end
  end

  defp do_perform_dump(cont, inPlace, initBy, regulator, oldVersion) do
    case :mnesia_log.chunk_log(cont) do
      {c2, recs} ->
        try do
          insert_recs(recs, inPlace, initBy, regulator, oldVersion)
        catch
          _, r when r !== :fatal ->
            reason = {'Transaction log dump error: ~tp~n', [{r, __STACKTRACE__}]}
            close_files(inPlace, {:error, reason}, initBy)
            exit(reason)
        else
          version ->
            do_perform_dump(c2, inPlace, initBy, regulator, version)
        end

      :eof ->
        close_files(inPlace, :ok, initBy)
        :erlang.erase(:mnesia_dumper_dets)
        :ok
    end
  end

  defp insert_recs([rec | recs], inPlace, initBy, regulator, logV) do
    regulate(regulator)

    case insert_rec(rec, inPlace, initBy, logV) do
      logH when elem(logH, 0) === :log_header ->
        insert_recs(recs, inPlace, initBy, regulator, r_log_header(logH, :log_version))

      _ ->
        insert_recs(recs, inPlace, initBy, regulator, logV)
    end
  end

  defp insert_recs([], _InPlace, _InitBy, _Regulator, version) do
    version
  end

  defp insert_rec(rec, _InPlace, :scan_decisions, _LogV) do
    cond do
      elem(rec, 0) === :commit ->
        :ignore

      elem(rec, 0) === :log_header ->
        :ignore

      true ->
        :mnesia_recover.note_log_decision(rec, :scan_decisions)
    end
  end

  defp insert_rec(rec, inPlace, initBy, logV)
       when elem(rec, 0) === :commit do
    d = r_commit(rec, :decision)

    case :mnesia_recover.wait_for_decision(d, initBy) do
      {tid, :committed} ->
        do_insert_rec(tid, :mnesia_tm.new_cr_format(rec), inPlace, initBy, logV)

      {tid, :aborted} ->
        case initBy do
          :startup ->
            :mnesia_schema.undo_prepare_commit(
              tid,
              :mnesia_tm.new_cr_format(rec)
            )

          _ ->
            :ok
        end
    end
  end

  defp insert_rec(h, _InPlace, _InitBy, _LogV)
       when elem(h, 0) === :log_header do
    currentVersion = :mnesia_log.version()

    cond do
      r_log_header(h, :log_kind) != :trans_log ->
        exit({'Bad kind of transaction log', h})

      r_log_header(h, :log_version) == currentVersion ->
        :ok

      r_log_header(h, :log_version) == '4.2' ->
        :ok

      r_log_header(h, :log_version) == '4.1' ->
        :ok

      r_log_header(h, :log_version) == '4.0' ->
        :ok

      true ->
        fatal('Bad version of transaction log: ~p~n', [h])
    end

    h
  end

  defp insert_rec(_Rec, _InPlace, _InitBy, _LogV) do
    :ok
  end

  defp do_insert_rec(tid, rec, inPlace, initBy, logV) do
    case r_commit(rec, :schema_ops) do
      [] ->
        :ignore

      schemaOps ->
        case val({:schema, :storage_type}) do
          :ram_copies ->
            insert_ops(tid, :schema_ops, schemaOps, inPlace, initBy, logV)

          storage ->
            true = open_files(:schema, storage, inPlace, initBy)
            insert_ops(tid, :schema_ops, schemaOps, inPlace, initBy, logV)
        end
    end

    d = r_commit(rec, :disc_copies)
    insert_ops(tid, :disc_copies, d, inPlace, initBy, logV)
    insert_ext_ops(tid, commit_ext(rec), inPlace, initBy)

    case initBy do
      :startup ->
        dO = r_commit(rec, :disc_only_copies)
        insert_ops(tid, :disc_only_copies, dO, inPlace, initBy, logV)

      _ ->
        :ignore
    end
  end

  defp commit_ext(r_commit(ext: [])) do
    []
  end

  defp commit_ext(r_commit(ext: ext)) do
    case :lists.keyfind(:ext_copies, 1, ext) do
      {_, c} ->
        c

      false ->
        []
    end
  end

  def update(_Tid, [], _DumperMode) do
    :dumped
  end

  def update(tid, schemaOps, dumperMode) do
    useDir = :mnesia_monitor.use_dir()
    res = perform_update(tid, schemaOps, dumperMode, useDir)
    :mnesia_controller.release_schema_commit_lock()
    res
  end

  defp perform_update(_Tid, _SchemaOps, :mandatory, true) do
    initBy = :schema_update
    :ok
    opt_dump_log(initBy)
  end

  defp perform_update(tid, schemaOps, _DumperMode, _UseDir) do
    initBy = :fast_schema_update
    inPlace = :mnesia_monitor.get_env(:dump_log_update_in_place)

    try do
      insert_ops(tid, :schema_ops, schemaOps, inPlace, initBy, :mnesia_log.version())
      :ok
      close_files(inPlace, :ok, initBy)
      :ok
    catch
      _, reason when reason !== :fatal ->
        error = {:error, {'Schema update error', {reason, __STACKTRACE__}}}
        close_files(inPlace, error, initBy)
        fatal('Schema update error ~tp ~tp', [{reason, __STACKTRACE__}, schemaOps])
    end
  end

  defp insert_ext_ops(tid, extOps, inPlace, initBy) do
    :lists.foreach(
      fn {ext, op} ->
        case storage_semantics(ext) do
          semantics
          when semantics == :disc_copies or
                 (semantics == :disc_only_copies and
                    initBy == :startup) ->
            insert_op(tid, ext, op, inPlace, initBy)

          _Other ->
            :ok
        end
      end,
      extOps
    )
  end

  defp insert_ops(_Tid, _Storage, [], _InPlace, _InitBy, _) do
    :ok
  end

  defp insert_ops(tid, storage, [op], inPlace, initBy, ver)
       when ver >= '4.3' do
    insert_op(tid, storage, op, inPlace, initBy)
    :ok
  end

  defp insert_ops(tid, storage, [op | ops], inPlace, initBy, ver)
       when ver >= '4.3' do
    insert_op(tid, storage, op, inPlace, initBy)
    insert_ops(tid, storage, ops, inPlace, initBy, ver)
  end

  defp insert_ops(tid, storage, [op | ops], inPlace, initBy, ver)
       when ver < '4.3' do
    insert_ops(tid, storage, ops, inPlace, initBy, ver)
    insert_op(tid, storage, op, inPlace, initBy)
  end

  defp disc_insert(_Tid, storage, tab, key, val, op, inPlace, initBy) do
    semantics = storage_semantics(storage)

    case open_files(tab, semantics, storage, inPlace, initBy) do
      true ->
        case semantics do
          :disc_copies when tab != :schema ->
            :mnesia_log.append(
              {:mnesia_dumper, tab},
              {{tab, key}, val, op}
            )

            :ok

          _ ->
            dets_insert(op, tab, key, val, storage)
        end

      false ->
        :ignore
    end
  end

  defp dets_insert(op, tab, key, val, storage0) do
    storage =
      cond do
        tab == :schema ->
          :disc_only_copies

        true ->
          storage0
      end

    case op do
      :write ->
        dets_updated(tab, key)
        :ok = :mnesia_lib.db_put(storage, tab, val)

      :delete ->
        dets_updated(tab, key)
        :ok = :mnesia_lib.db_erase(storage, tab, key)

      :update_counter ->
        case dets_incr_counter(tab, key) do
          true ->
            {recName, incr} = val

            try do
              _ = :mnesia_lib.db_update_counter(storage, tab, key, incr)
            catch
              :error, _ when incr < 0 ->
                zero = {recName, key, 0}
                :ok = :mnesia_lib.db_put(storage, tab, zero)

              :error, _ ->
                init = {recName, key, incr}
                :ok = :mnesia_lib.db_put(storage, tab, init)
            end

          false ->
            :ok
        end

      :delete_object ->
        dets_updated(tab, key)
        :mnesia_lib.db_match_erase(storage, tab, val)

      :clear_table ->
        dets_cleared(tab)
        :ok = :mnesia_lib.db_match_erase(storage, tab, :_)
    end
  end

  defp dets_updated(tab, key) do
    case :erlang.get(:mnesia_dumper_dets) do
      :undefined ->
        empty = :gb_trees.empty()
        tree = :gb_trees.insert(tab, :gb_sets.singleton(key), empty)
        :erlang.put(:mnesia_dumper_dets, tree)

      tree ->
        case :gb_trees.lookup(tab, tree) do
          {:value, :cleared} ->
            :ignore

          {:value, set} ->
            t = :gb_trees.update(tab, :gb_sets.add(key, set), tree)
            :erlang.put(:mnesia_dumper_dets, t)

          :none ->
            t = :gb_trees.insert(tab, :gb_sets.singleton(key), tree)
            :erlang.put(:mnesia_dumper_dets, t)
        end
    end
  end

  defp dets_incr_counter(tab, key) do
    case :erlang.get(:mnesia_dumper_dets) do
      :undefined ->
        false

      tree ->
        case :gb_trees.lookup(tab, tree) do
          {:value, :cleared} ->
            true

          {:value, set} ->
            :gb_sets.is_member(key, set)

          :none ->
            false
        end
    end
  end

  defp dets_cleared(tab) do
    case :erlang.get(:mnesia_dumper_dets) do
      :undefined ->
        empty = :gb_trees.empty()
        tree = :gb_trees.insert(tab, :cleared, empty)
        :erlang.put(:mnesia_dumper_dets, tree)

      tree ->
        case :gb_trees.lookup(tab, tree) do
          {:value, :cleared} ->
            :ignore

          _ ->
            t = :gb_trees.enter(tab, :cleared, tree)
            :erlang.put(:mnesia_dumper_dets, t)
        end
    end
  end

  defp insert(tid, storage, tab, key, [val | tail], op, inPlace, initBy) do
    insert(tid, storage, tab, key, val, op, inPlace, initBy)
    insert(tid, storage, tab, key, tail, op, inPlace, initBy)
  end

  defp insert(_Tid, _Storage, _Tab, _Key, [], _Op, _InPlace, _InitBy) do
    :ok
  end

  defp insert(tid, storage, tab, key, val, op, inPlace, initBy) do
    semantics = storage_semantics(storage)
    item = {{tab, key}, val, op}

    case initBy do
      :startup ->
        disc_insert(tid, storage, tab, key, val, op, inPlace, initBy)

      _ when semantics == :ram_copies ->
        :mnesia_tm.do_update_op(tid, storage, item)
        snmp = :mnesia_tm.prepare_snmp(tab, key, [item])
        :mnesia_tm.do_snmp(tid, snmp)

      _ when semantics == :disc_copies ->
        disc_insert(tid, storage, tab, key, val, op, inPlace, initBy)
        :mnesia_tm.do_update_op(tid, storage, item)
        snmp = :mnesia_tm.prepare_snmp(tab, key, [item])
        :mnesia_tm.do_snmp(tid, snmp)

      _ when semantics == :disc_only_copies ->
        :mnesia_tm.do_update_op(tid, storage, item)
        snmp = :mnesia_tm.prepare_snmp(tab, key, [item])
        :mnesia_tm.do_snmp(tid, snmp)

      _ when :erlang.element(1, storage) == :ext ->
        :mnesia_tm.do_update_op(tid, storage, item)
        snmp = :mnesia_tm.prepare_snmp(tab, key, [item])
        :mnesia_tm.do_snmp(tid, snmp)

      _ when storage == :unknown ->
        :ignore
    end
  end

  defp disc_delete_table(tab, {:ext, alias, mod}) do
    mod.close_table(alias, tab)
    mod.delete_table(alias, tab)
  end

  defp disc_delete_table(tab, storage) do
    case :mnesia_monitor.use_dir() do
      true ->
        cond do
          storage == :disc_only_copies or tab == :schema ->
            :mnesia_monitor.unsafe_close_dets(tab)
            dat = :mnesia_lib.tab2dat(tab)
            :file.delete(dat)
            :ok

          true ->
            dclFile = :mnesia_lib.tab2dcl(tab)

            case :erlang.get({:mnesia_dumper, tab}) do
              {:opened_dumper, :dcl} ->
                del_opened_tab(tab)
                :mnesia_log.unsafe_close_log(tab)

              _ ->
                :ok
            end

            :file.delete(dclFile)
            dcdFile = :mnesia_lib.tab2dcd(tab)
            :file.delete(dcdFile)
            :ok
        end

        :erlang.erase({:mnesia_dumper, tab})
        :ok

      false ->
        :ok
    end
  end

  defp disc_delete_indecies(tab, cs, storage) do
    case storage_semantics(storage) do
      :disc_only_copies ->
        indecies = r_cstruct(cs, :index)
        :mnesia_index.del_transient(tab, indecies, storage)

      _ ->
        :ok
    end
  end

  defp insert_op(tid, storage, {{tab, key}, val, op}, inPlace, initBy) do
    disc_insert(tid, storage, tab, key, val, op, inPlace, initBy)
  end

  defp insert_op(_Tid, :schema_ops, _OP, _InPlace, initby)
       when initby != :startup and
              initby != :fast_schema_update and
              initby != :schema_update do
    :ignore
  end

  defp insert_op(tid, _, {:op, :rec, storage, item}, inPlace, initBy) do
    {{tab, key}, valList, op} = item
    insert(tid, storage, tab, key, valList, op, inPlace, initBy)
  end

  defp insert_op(tid, _, {:op, :change_table_copy_type, n, fromS, toS, tabDef}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)
    val = :mnesia_schema.insert_cstruct(tid, cs, true)
    {:schema, tab, _} = val
    fromSem = storage_semantics(fromS)
    toSem = storage_semantics(toS)

    case :lists.member(n, val({:current, :db_nodes})) do
      true when initBy != :startup ->
        :mnesia_controller.add_active_replica(tab, n, cs)

      _ ->
        :ignore
    end

    cond do
      n == node() ->
        dmp = :mnesia_lib.tab2dmp(tab)
        dat = :mnesia_lib.tab2dat(tab)
        dcd = :mnesia_lib.tab2dcd(tab)
        dcl = :mnesia_lib.tab2dcl(tab)
        logtmp = :mnesia_lib.tab2logtmp(tab)

        case {fromS, toS} do
          {{:ext, _FromAlias, _FromMod}, {:ext, toAlias, toMod}} ->
            disc_delete_table(tab, fromS)
            :ok = toMod.delete_table(toAlias, tab)
            :ok = :mnesia_monitor.unsafe_create_external(tab, toAlias, toMod, tabDef)
            :ok = toMod.load_table(toAlias, tab, {:dumper, :change_table_copy_type}, tabDef)
            :ok = load_from_logfile(toS, tab, logtmp)
            :file.delete(logtmp)
            restore_indexes(tab, toS, cs)

          {_, {:ext, toAlias, toMod}} ->
            case fromSem do
              :ram_copies ->
                :mnesia_schema.ram_delete_table(tab, fromS)

              _ ->
                cond do
                  fromSem == :disc_copies ->
                    :mnesia_schema.ram_delete_table(tab, fromS)

                  true ->
                    :ok
                end

                disc_delete_table(tab, fromS)
            end

            :ok = toMod.delete_table(toAlias, tab)
            :ok = :mnesia_monitor.unsafe_create_external(tab, toAlias, toMod, tabDef)
            :ok = toMod.load_table(toAlias, tab, {:dumper, :change_table_copy_type}, tabDef)
            :ok = load_from_logfile(toS, tab, logtmp)
            :file.delete(logtmp)
            restore_indexes(tab, toS, cs)

          {{:ext, _FromAlias, _FromMod} = ^fromS, toS} ->
            disc_delete_table(tab, fromS)

            case toS do
              :ram_copies ->
                change_disc_to_ram(tab, cs, fromS, toS, logtmp, initBy)

              :disc_copies ->
                args = [{:keypos, 2}, :public, :named_table, r_cstruct(cs, :type)]
                :mnesia_monitor.mktab(tab, args)
                :ok = load_from_logfile(toS, tab, logtmp)
                :file.delete(logtmp)

              :disc_only_copies ->
                true = open_files(tab, toS, inPlace, initBy)
                :ok = load_from_logfile(toS, tab, logtmp)
                :file.delete(logtmp)
            end

            restore_indexes(tab, toS, cs)

          _NoneAreExt ->
            case {fromSem, toSem} do
              {:ram_copies, :disc_copies} when tab == :schema ->
                :ok = ensure_rename(dmp, dat)

              {:ram_copies, :disc_copies} ->
                :file.delete(dcl)
                :ok = ensure_rename(dmp, dcd)

              {:disc_copies, :ram_copies} when tab == :schema ->
                :mnesia_lib.set(:use_dir, false)
                :mnesia_monitor.unsafe_close_dets(tab)
                :ok = :file.delete(dat)

              {:disc_copies, :ram_copies} ->
                _ = :file.delete(dcl)
                _ = :file.delete(dcd)
                :ok

              {:ram_copies, :disc_only_copies} ->
                :ok = ensure_rename(dmp, dat)
                true = open_files(tab, toS, inPlace, initBy)
                :mnesia_schema.ram_delete_table(tab, fromS)
                restore_indexes(tab, toS, cs)

              {:disc_only_copies, :ram_copies}
              when fromS == :disc_only_copies ->
                :mnesia_monitor.unsafe_close_dets(tab)
                disc_delete_indecies(tab, cs, fromS)

                case initBy do
                  :startup ->
                    :ignore

                  _ ->
                    :mnesia_controller.get_disc_copy(tab)
                    :ok
                end

                disc_delete_table(tab, fromS)

              {:disc_only_copies, :ram_copies}
              when :erlang.element(
                     1,
                     fromS
                   ) == :ext ->
                change_disc_to_ram(tab, cs, fromS, toS, logtmp, initBy)

              {:disc_copies, :disc_only_copies} ->
                :ok = ensure_rename(dmp, dat)
                true = open_files(tab, toS, inPlace, initBy)
                :mnesia_schema.ram_delete_table(tab, fromS)
                restore_indexes(tab, toS, cs)
                _ = :file.delete(dcl)
                _ = :file.delete(dcd)
                :ok

              {:disc_only_copies, :disc_copies} ->
                :mnesia_monitor.unsafe_close_dets(tab)
                disc_delete_indecies(tab, cs, :disc_only_copies)

                case initBy do
                  :startup ->
                    :ignore

                  _ ->
                    :mnesia_log.ets2dcd(tab)
                    :mnesia_controller.get_disc_copy(tab)
                    disc_delete_table(tab, :disc_only_copies)
                end
            end
        end

      true ->
        :ignore
    end

    s = val({:schema, :storage_type})
    disc_insert(tid, s, :schema, tab, val, :write, inPlace, initBy)
  end

  defp insert_op(tid, _, {:op, :transform, _Fun, tabDef}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)

    case :mnesia_lib.cs_to_storage_type(node(), cs) do
      :disc_copies ->
        open_dcl(r_cstruct(cs, :name))

      _ ->
        :ignore
    end

    insert_cstruct(tid, cs, true, inPlace, initBy)
  end

  defp insert_op(tid, _, {:op, :restore_recreate, tabDef}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)
    tab = r_cstruct(cs, :name)
    type = r_cstruct(cs, :type)
    storage = :mnesia_lib.cs_to_storage_type(node(), cs)

    semantics =
      cond do
        storage == :unknown ->
          :unknown

        true ->
          storage_semantics(storage)
      end

    disc_delete_table(tab, storage)
    disc_delete_indecies(tab, cs, storage)

    case initBy do
      :startup ->
        :ignore

      _ ->
        case (try do
                :ets.lookup_element(:mnesia_gvar, {tab, :cstruct}, 2)
              catch
                :error, _ ->
                  {:EXIT, {:badarg, []}}
              end) do
          {:EXIT, _} ->
            :ignore

          _ ->
            :mnesia_schema.ram_delete_table(tab, storage)
            :mnesia_checkpoint.tm_del_copy(tab, node())
        end
    end

    storageProps = r_cstruct(cs, :storage_properties)

    cond do
      initBy == :startup or semantics == :unknown ->
        :ignore

      semantics == :ram_copies ->
        etsProps = :proplists.get_value(:ets, storageProps, [])

        args = [
          {:keypos, 2},
          :public,
          :named_table,
          type
          | etsProps
        ]

        :mnesia_monitor.mktab(tab, args)

      semantics == :disc_copies ->
        etsProps = :proplists.get_value(:ets, storageProps, [])

        args = [
          {:keypos, 2},
          :public,
          :named_table,
          type
          | etsProps
        ]

        :mnesia_monitor.mktab(tab, args)
        file = :mnesia_lib.tab2dcd(tab)

        fArg = [
          {:file, file},
          {:name, {:mnesia, :create}},
          {:repair, false},
          {:mode, :read_write}
        ]

        {:ok, log} = :mnesia_monitor.open_log(fArg)
        :mnesia_monitor.unsafe_close_log(log)

      storage == :disc_only_copies ->
        file = :mnesia_lib.tab2dat(tab)
        :file.delete(file)
        detsProps = :proplists.get_value(:dets, storageProps, [])

        args = [
          {:file, :mnesia_lib.tab2dat(tab)},
          {:type,
           :mnesia_lib.disk_type(
             tab,
             type
           )},
          {:keypos, 2},
          {:repair, :mnesia_monitor.get_env(:auto_repair)}
          | detsProps
        ]

        :mnesia_monitor.open_dets(tab, args)

      :erlang.element(1, storage) == :ext ->
        {:ext, alias, mod} = storage
        mod.create_table(alias, tab, [])
    end

    insert_op(tid, :ignore, {:op, :create_table, tabDef}, inPlace, initBy)
  end

  defp insert_op(tid, _, {:op, :create_table, tabDef}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)
    insert_cstruct(tid, cs, false, inPlace, initBy)
    tab = r_cstruct(cs, :name)
    storage = :mnesia_lib.cs_to_storage_type(node(), cs)
    storageProps = r_cstruct(cs, :storage_properties)
    semantics = storage_semantics(storage)

    case initBy do
      :startup ->
        case semantics do
          :unknown ->
            :ignore

          :ram_copies ->
            :ignore

          :disc_copies ->
            dcd = :mnesia_lib.tab2dcd(tab)

            case :mnesia_lib.exists(dcd) do
              true ->
                :ignore

              false ->
                :mnesia_log.open_log(
                  :temp,
                  :mnesia_log.dcd_log_header(),
                  dcd,
                  false,
                  false,
                  :read_write
                )

                :mnesia_log.unsafe_close_log(:temp)
            end

          :disc_only_copies ->
            detsProps = :proplists.get_value(:dets, storageProps, [])
            try_create_disc_only_copy(storage, tab, cs, detsProps)
        end

      _ ->
        copies = :mnesia_lib.copy_holders(cs)

        active =
          :mnesia_lib.intersect(
            copies,
            val({:current, :db_nodes})
          )

        for n <- active do
          :mnesia_controller.add_active_replica(tab, n, cs)
        end

        case storage do
          :unknown ->
            :mnesia_lib.unset({tab, :create_table})

            case r_cstruct(cs, :local_content) do
              true ->
                :ignore

              false ->
                :mnesia_lib.set_remote_where_to_read(tab)
            end

          _ ->
            case r_cstruct(cs, :local_content) do
              true ->
                :mnesia_lib.set_local_content_whereabouts(tab)

              false ->
                :mnesia_lib.set({tab, :where_to_read}, node())
            end

            case semantics do
              :ram_copies ->
                :ignore

              _ ->
                disc_delete_indecies(tab, cs, storage)
            end

            :mnesia_controller.create_table(tab)
            :mnesia_lib.unset({tab, :create_table})
        end
    end
  end

  defp insert_op(_Tid, _, {:op, :dump_table, size, tabDef}, _InPlace, _InitBy) do
    case size do
      :unknown ->
        :ignore

      _ ->
        cs = :mnesia_schema.list2cs(tabDef)
        tab = r_cstruct(cs, :name)
        dmp = :mnesia_lib.tab2dmp(tab)
        dat = :mnesia_lib.tab2dcd(tab)

        case size do
          0 ->
            :file.delete(dmp)
            :file.delete(dat)

          _ ->
            :ok = ensure_rename(dmp, dat)
        end
    end
  end

  defp insert_op(tid, _, {:op, :delete_table, tabDef}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)
    tab = r_cstruct(cs, :name)

    case :mnesia_lib.cs_to_storage_type(node(), cs) do
      :unknown ->
        :ignore

      storage ->
        disc_delete_table(tab, storage)
        disc_delete_indecies(tab, cs, storage)

        case initBy do
          :startup ->
            :ignore

          _ ->
            :mnesia_schema.ram_delete_table(tab, storage)
            :mnesia_checkpoint.tm_del_copy(tab, node())
        end
    end

    delete_cstruct(tid, cs, inPlace, initBy)
  end

  defp insert_op(tid, _, {:op, :clear_table, tabDef}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)
    tab = r_cstruct(cs, :name)

    case :mnesia_lib.cs_to_storage_type(node(), cs) do
      :unknown ->
        :ignore

      storage ->
        oid = :_

        cond do
          storage == :disc_copies ->
            open_dcl(r_cstruct(cs, :name))

          true ->
            :ignore
        end

        try do
          insert(tid, storage, tab, :_, oid, :clear_table, inPlace, initBy)
        catch
          _, _Reason ->
            {:EXIT, _Reason}
        end
    end
  end

  defp insert_op(tid, _, {:op, :merge_schema, tabDef}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)

    case r_cstruct(cs, :name) do
      :schema ->
        update = fn nS = {node, storage} ->
          case :mnesia_lib.cs_to_storage_type(node, cs) do
            ^storage ->
              nS

            :disc_copies when node == node() ->
              dir = :mnesia_lib.dir()
              :ok = :mnesia_schema.opt_create_dir(true, dir)
              :mnesia_schema.purge_dir(dir, [])
              :mnesia_log.purge_all_logs()
              :mnesia_lib.set(:use_dir, true)
              :mnesia_log.init()
              ns = val({:current, :db_nodes})

              f = fn u ->
                :mnesia_recover.log_mnesia_up(u)
              end

              :lists.foreach(f, ns)
              raw_named_dump_table(:schema, :dat)
              temp_set_master_nodes()
              {node, :disc_copies}

            cSstorage ->
              {node, cSstorage}
          end
        end

        w2C0 = val({:schema, :where_to_commit})

        w2C =
          case w2C0 do
            {:blocked, list} ->
              {:blocked, :lists.map(update, list)}

            list ->
              :lists.map(update, list)
          end

        cond do
          w2C == w2C0 ->
            :ignore

          true ->
            :mnesia_lib.set({:schema, :where_to_commit}, w2C)
        end

      _ ->
        :ignore
    end

    insert_cstruct(tid, cs, false, inPlace, initBy)
  end

  defp insert_op(tid, _, {:op, :del_table_copy, storage, node, tabDef}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)
    tab = r_cstruct(cs, :name)

    cond do
      tab == :schema and storage == :ram_copies ->
        insert_cstruct(tid, cs, true, inPlace, initBy)

      tab != :schema ->
        :mnesia_controller.del_active_replica(tab, node)
        :mnesia_lib.del({tab, storage_alias(storage)}, node)

        cond do
          node == node() ->
            case r_cstruct(cs, :local_content) do
              true ->
                :mnesia_lib.set({tab, :where_to_read}, :nowhere)

              false ->
                :mnesia_lib.set_remote_where_to_read(tab)
            end

            :mnesia_lib.del({:schema, :local_tables}, tab)
            :mnesia_lib.set({tab, :storage_type}, :unknown)
            insert_cstruct(tid, cs, true, inPlace, initBy)
            disc_delete_table(tab, storage)
            disc_delete_indecies(tab, cs, storage)
            :mnesia_schema.ram_delete_table(tab, storage)
            :mnesia_checkpoint.tm_del_copy(tab, node)

          true ->
            case val({tab, :where_to_read}) do
              ^node ->
                :mnesia_lib.set_remote_where_to_read(tab)

              _ ->
                :ignore
            end

            insert_cstruct(tid, cs, true, inPlace, initBy)
        end
    end
  end

  defp insert_op(tid, _, {:op, :add_table_copy, _Storage, _Node, tabDef}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)
    insert_cstruct(tid, cs, true, inPlace, initBy)
  end

  defp insert_op(tid, _, {:op, :add_snmp, _Us, tabDef}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)
    insert_cstruct(tid, cs, true, inPlace, initBy)
  end

  defp insert_op(tid, _, {:op, :del_snmp, tabDef}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)
    tab = r_cstruct(cs, :name)
    storage = :mnesia_lib.cs_to_storage_type(node(), cs)

    cond do
      initBy != :startup and storage != :unknown ->
        case (try do
                :ets.lookup_element(:mnesia_gvar, {tab, {:index, :snmp}}, 2)
              catch
                :error, _ ->
                  {:EXIT, {:badarg, []}}
              end) do
          {:EXIT, _} ->
            :ignore

          stab ->
            :mnesia_snmp_hook.delete_table(tab, stab)
            :mnesia_lib.unset({tab, {:index, :snmp}})
        end

      true ->
        :ignore
    end

    insert_cstruct(tid, cs, true, inPlace, initBy)
  end

  defp insert_op(tid, _, {:op, :add_index, pos, tabDef}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)
    tab = insert_cstruct(tid, cs, true, inPlace, initBy)
    storage = :mnesia_lib.cs_to_storage_type(node(), cs)
    semantics = storage_semantics(storage)

    case initBy do
      :startup when semantics == :disc_only_copies ->
        true = open_files(tab, semantics, storage, inPlace, initBy)
        :mnesia_index.init_indecies(tab, storage, [pos])

      :startup ->
        :ignore

      _ ->
        case val({tab, :where_to_read}) do
          :nowhere ->
            :ignore

          _ ->
            :mnesia_index.init_indecies(tab, storage, [pos])
        end
    end
  end

  defp insert_op(tid, _, {:op, :del_index, pos, tabDef}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)
    tab = r_cstruct(cs, :name)
    storage = :mnesia_lib.cs_to_storage_type(node(), cs)

    case initBy do
      :startup when storage == :disc_only_copies ->
        :mnesia_index.del_index_table(tab, storage, pos)

      :startup ->
        :ignore

      _ when :erlang.element(1, storage) == :ext ->
        :mnesia_index.del_index_table(tab, storage, pos)

      _ ->
        :mnesia_index.del_index_table(tab, storage, pos)
    end

    insert_cstruct(tid, cs, true, inPlace, initBy)
  end

  defp insert_op(
         tid,
         _,
         {:op, :change_table_access_mode, tabDef, _OldAccess, _Access},
         inPlace,
         initBy
       ) do
    cs = :mnesia_schema.list2cs(tabDef)

    case initBy do
      :startup ->
        :ignore

      _ ->
        :mnesia_controller.change_table_access_mode(cs)
    end

    insert_cstruct(tid, cs, true, inPlace, initBy)
  end

  defp insert_op(
         tid,
         _,
         {:op, :change_table_majority, tabDef, _OldAccess, _Access},
         inPlace,
         initBy
       ) do
    cs = :mnesia_schema.list2cs(tabDef)

    case initBy do
      :startup ->
        :ignore

      _ ->
        :mnesia_controller.change_table_majority(cs)
    end

    insert_cstruct(tid, cs, true, inPlace, initBy)
  end

  defp insert_op(
         tid,
         _,
         {:op, :change_table_load_order, tabDef, _OldLevel, _Level},
         inPlace,
         initBy
       ) do
    cs = :mnesia_schema.list2cs(tabDef)
    insert_cstruct(tid, cs, true, inPlace, initBy)
  end

  defp insert_op(tid, _, {:op, :delete_property, tabDef, propKey}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)
    tab = r_cstruct(cs, :name)
    :mnesia_lib.unset({tab, :user_property, propKey})
    insert_cstruct(tid, cs, true, inPlace, initBy)
  end

  defp insert_op(tid, _, {:op, :write_property, tabDef, _Prop}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)
    insert_cstruct(tid, cs, true, inPlace, initBy)
  end

  defp insert_op(tid, _, {:op, :change_table_frag, _Change, tabDef}, inPlace, initBy) do
    cs = :mnesia_schema.list2cs(tabDef)
    insert_cstruct(tid, cs, true, inPlace, initBy)
  end

  defp storage_semantics({:ext, alias, mod}) do
    mod.semantics(alias, :storage)
  end

  defp storage_semantics(storage) when is_atom(storage) do
    storage
  end

  defp storage_alias({:ext, alias, _}) do
    alias
  end

  defp storage_alias(storage) when is_atom(storage) do
    storage
  end

  defp change_disc_to_ram(tab, cs, fromS, toS, logtmp, initBy) do
    disc_delete_indecies(tab, cs, fromS)

    case initBy do
      :startup ->
        :ignore

      _ ->
        ^tab = :ets.info(tab, :name)
        load_from_logfile(toS, tab, logtmp)
        posList = r_cstruct(cs, :index)
        :mnesia_index.init_indecies(tab, toS, posList)
    end

    disc_delete_table(tab, fromS)
  end

  defp try_create_disc_only_copy({:ext, alias, mod}, tab, cs, _) do
    mod.create_table(alias, tab, :mnesia_schema.cs2list(cs))
  end

  defp try_create_disc_only_copy(:disc_only_copies, tab, cs, detsProps) do
    args = [
      {:file, :mnesia_lib.tab2dat(tab)},
      {:type,
       :mnesia_lib.disk_type(
         tab,
         r_cstruct(cs, :type)
       )},
      {:keypos, 2},
      {:repair, :mnesia_monitor.get_env(:auto_repair)}
      | detsProps
    ]

    case :mnesia_monitor.open_dets(tab, args) do
      {:ok, _} ->
        :mnesia_monitor.unsafe_close_dets(tab)

      {:error, error} ->
        exit({'Failed to create dets table', error})
    end
  end

  defp restore_indexes(tab, toS, cs) do
    posList = r_cstruct(cs, :index)
    :mnesia_index.init_indecies(tab, toS, posList)
  end

  defp open_files(tab, storage, updateInPlace, initBy) do
    open_files(tab, storage_semantics(storage), storage, updateInPlace, initBy)
  end

  defp open_files(tab, semantics, storage, updateInPlace, initBy)
       when storage != :unknown and
              semantics != :ram_copies do
    case :erlang.get({:mnesia_dumper, tab}) do
      :undefined ->
        case (try do
                :ets.lookup_element(:mnesia_gvar, {tab, :setorbag}, 2)
              catch
                :error, _ ->
                  {:EXIT, {:badarg, []}}
              end) do
          {:EXIT, _} ->
            false

          type ->
            cs = val({tab, :cstruct})

            cond do
              semantics == :disc_copies and tab != :schema ->
                bool = open_disc_copies(tab, initBy)
                bool

              storage == :disc_only_copies or tab == :schema ->
                props = val({tab, :storage_properties})
                detsProps = :proplists.get_value(:dets, props, [])
                fname = prepare_open(tab, updateInPlace)

                args = [
                  {:file, fname},
                  {:keypos, 2},
                  {:repair, :mnesia_monitor.get_env(:auto_repair)},
                  {:type,
                   :mnesia_lib.disk_type(
                     tab,
                     type
                   )}
                  | detsProps
                ]

                {:ok, _} = :mnesia_monitor.open_dets(tab, args)

                :erlang.put(
                  {:mnesia_dumper, tab},
                  {:opened_dumper, :dat}
                )

                true

              :erlang.element(1, storage) == :ext ->
                {:ext, alias, mod} = storage
                mod.load_table(alias, tab, initBy, :mnesia_schema.cs2list(cs))

                :erlang.put(
                  {:mnesia_dumper, tab},
                  {:opened_dumper, :ext}
                )

                true
            end
        end

      :already_dumped ->
        false

      {:opened_dumper, _} ->
        true
    end
  end

  defp open_files(_Tab, _Semantics, _Storage, _UpdateInPlace, _InitBy) do
    false
  end

  defp open_disc_copies(tab, initBy) do
    dumpEts = needs_dump_ets(tab)

    cond do
      dumpEts == false or initBy == :startup ->
        dclF = :mnesia_lib.tab2dcl(tab)

        :mnesia_log.open_log(
          {:mnesia_dumper, tab},
          :mnesia_log.dcl_log_header(),
          dclF,
          :mnesia_lib.exists(dclF),
          :mnesia_monitor.get_env(:auto_repair),
          :read_write
        )

        :erlang.put(
          {:mnesia_dumper, tab},
          {:opened_dumper, :dcl}
        )

        true

      true ->
        :mnesia_log.ets2dcd(tab)
        :erlang.put({:mnesia_dumper, tab}, :already_dumped)
        false
    end
  end

  def needs_dump_ets(tab) do
    dclF = :mnesia_lib.tab2dcl(tab)

    case :file.read_file_info(dclF) do
      {:error, :enoent} ->
        false

      {:ok, dclInfo} ->
        dcdF = :mnesia_lib.tab2dcd(tab)

        case :file.read_file_info(dcdF) do
          {:error, reason} ->
            :mnesia_lib.dbg_out('File ~tp info_error ~tp ~n', [dcdF, reason])
            true

          {:ok, dcdInfo} ->
            mul =
              case (try do
                      :ets.lookup_element(:mnesia_gvar, :dc_dump_limit, 2)
                    catch
                      :error, _ ->
                        {:EXIT, {:badarg, []}}
                    end) do
                {:EXIT, _} ->
                  4

                val ->
                  val
              end

            r_file_info(dcdInfo, :size) <= r_file_info(dclInfo, :size) * mul
        end
    end
  end

  defp open_dcl(tab) do
    case :erlang.get({:mnesia_dumper, tab}) do
      {:opened_dumper, _} ->
        true

      _ ->
        dclF = :mnesia_lib.tab2dcl(tab)

        :mnesia_log.open_log(
          {:mnesia_dumper, tab},
          :mnesia_log.dcl_log_header(),
          dclF,
          :mnesia_lib.exists(dclF),
          :mnesia_monitor.get_env(:auto_repair),
          :read_write
        )

        :erlang.put(
          {:mnesia_dumper, tab},
          {:opened_dumper, :dcl}
        )

        true
    end
  end

  defp prepare_open(tab, updateInPlace) do
    dat = :mnesia_lib.tab2dat(tab)

    case updateInPlace do
      true ->
        dat

      false ->
        tmp = :mnesia_lib.tab2tmp(tab)

        try do
          :ok = :mnesia_lib.copy_file(dat, tmp)
        catch
          :error, error ->
            fatal('Cannot copy dets file ~tp to ~tp: ~tp~n', [dat, tmp, error])
        end

        tmp
    end
  end

  defp del_opened_tab(tab) do
    :erlang.erase({:mnesia_dumper, tab})
  end

  defp close_files(updateInPlace, outcome, initBy) do
    close_files(updateInPlace, outcome, initBy, :erlang.get())
  end

  defp close_files(inPlace, outcome, initBy, [{{:mnesia_dumper, tab}, :already_dumped} | tail]) do
    :erlang.erase({:mnesia_dumper, tab})
    close_files(inPlace, outcome, initBy, tail)
  end

  defp close_files(inPlace, outcome, initBy, [
         {{:mnesia_dumper, tab}, {:opened_dumper, type}}
         | tail
       ]) do
    :erlang.erase({:mnesia_dumper, tab})
    storage = val({tab, :storage_type})

    case storage_semantics(storage) do
      :disc_only_copies when initBy != :startup ->
        :ignore

      :disc_copies
      when storage != :unknown and
             tab != :schema ->
        :mnesia_log.close_log({:mnesia_dumper, tab})

      _ ->
        do_close(inPlace, outcome, tab, type, storage)
    end

    close_files(inPlace, outcome, initBy, tail)
  end

  defp close_files(inPlace, outcome, initBy, [_ | tail]) do
    close_files(inPlace, outcome, initBy, tail)
  end

  defp close_files(_, _, _InitBy, []) do
    :ok
  end

  defp do_close(_, _, tab, :ext, {:ext, alias, mod}) do
    mod.close_table(alias, tab)
  end

  defp do_close(_, _, tab, :dcl, :unknown) do
    :mnesia_log.close_log({:mnesia_dumper, tab})
    :file.delete(:mnesia_lib.tab2dcl(tab))
  end

  defp do_close(_, _, tab, :dcl, _) do
    :mnesia_log.close_log({:mnesia_dumper, tab})
  end

  defp do_close(_, _, _Tab, :ext, :unknown) do
    :ok
  end

  defp do_close(inPlace, outcome, tab, :dat, storage) do
    :mnesia_monitor.close_dets(tab)

    cond do
      storage == :unknown and inPlace == true ->
        :file.delete(:mnesia_lib.tab2dat(tab))

      inPlace == true ->
        :ok

      outcome == :ok and storage != :unknown ->
        tabDat = :mnesia_lib.tab2dat(tab)
        :ok = :file.rename(:mnesia_lib.tab2tmp(tab), tabDat)

      true ->
        :file.delete(:mnesia_lib.tab2tmp(tab))
    end
  end

  defp ensure_rename(from, to) do
    case :mnesia_lib.exists(from) do
      true ->
        :file.rename(from, to)

      false ->
        case :mnesia_lib.exists(to) do
          true ->
            :ok

          false ->
            {:error, {:rename_failed, from, to}}
        end
    end
  end

  defp insert_cstruct(tid, cs, keepWhereabouts, inPlace, initBy) do
    val = :mnesia_schema.insert_cstruct(tid, cs, keepWhereabouts)
    {:schema, tab, _} = val
    s = val({:schema, :storage_type})
    disc_insert(tid, s, :schema, tab, val, :write, inPlace, initBy)
    tab
  end

  defp delete_cstruct(tid, cs, inPlace, initBy) do
    val = :mnesia_schema.delete_cstruct(tid, cs)
    {:schema, tab, _} = val
    s = val({:schema, :storage_type})
    disc_insert(tid, s, :schema, tab, val, :delete, inPlace, initBy)
    tab
  end

  defp temp_set_master_nodes() do
    tabs = val({:schema, :local_tables})

    masters =
      for tab <- tabs do
        {tab,
         (val({tab, :disc_copies}) ++
            val({tab, :ram_copies}) ++ val({tab, :disc_only_copies}) ++ external_copies(tab)) --
           [node()]}
      end

    :mnesia_recover.log_master_nodes(masters, false, :yes)
    :ok
  end

  defp external_copies(tab) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :external_copies}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        []

      ext ->
        :lists.concat(
          for {_, ns} <- ext do
            ns
          end
        )
    end
  end

  def raw_named_dump_table(tab, ftype) do
    case :mnesia_monitor.use_dir() do
      true ->
        :mnesia_lib.lock_table(tab)
        tmpFname = :mnesia_lib.tab2tmp(tab)

        fname =
          case ftype do
            :dat ->
              :mnesia_lib.tab2dat(tab)

            :dmp ->
              :mnesia_lib.tab2dmp(tab)
          end

        :file.delete(tmpFname)
        :file.delete(fname)
        tabSize = :ets.info(tab, :size)
        tabRef = tab
        diskType = :mnesia_lib.disk_type(tab)

        args = [
          {:file, tmpFname},
          {:keypos, 2},
          {:estimated_no_objects, tabSize + 256},
          {:repair, :mnesia_monitor.get_env(:auto_repair)},
          {:type, diskType}
        ]

        case :mnesia_lib.dets_sync_open(tabRef, args) do
          {:ok, ^tabRef} ->
            storage = :ram_copies
            :mnesia_lib.db_fixtable(storage, tab, true)

            try do
              :ok = raw_dump_table(tabRef, tab)
              :ok = :file.rename(tmpFname, fname)
            catch
              _, reason ->
                try do
                  :file.delete(tmpFname)
                catch
                  :error, _ ->
                    :ok
                end

                exit({'Dump of table to disc failed', reason})
            after
              :mnesia_lib.db_fixtable(storage, tab, false)
              :mnesia_lib.dets_sync_close(tab)
              :mnesia_lib.unlock_table(tab)
            end

          {:error, reason} ->
            :mnesia_lib.unlock_table(tab)
            exit({'Open of file before dump to disc failed', reason})
        end

      false ->
        exit({:has_no_disc, node()})
    end
  end

  def raw_dump_table(detsRef, etsRef) do
    :dets.from_ets(detsRef, etsRef)
  end

  def dump_to_logfile(storage, tab) do
    case :mnesia_monitor.use_dir() do
      true ->
        logtmp = :mnesia_lib.tab2logtmp(tab)
        :file.delete(logtmp)

        case :disk_log.open([
               {:name, make_ref()},
               {:file, logtmp},
               {:repair, false},
               {:linkto, self()}
             ]) do
          {:ok, fd} ->
            :mnesia_lib.db_fixtable(storage, tab, true)

            try do
              do_dump_to_logfile(storage, tab, fd)
            after
              :mnesia_lib.db_fixtable(storage, tab, false)
            end

          {:error, _} = error ->
            error
        end

      false ->
        {:error, {:has_no_disc, node()}}
    end
  end

  defp do_dump_to_logfile(storage, tab, fd) do
    pat = [{:_, [], [:"$_"]}]
    log_terms(:mnesia_lib.db_select_init(storage, tab, pat, 100), storage, tab, pat, fd)
  end

  defp log_terms({objs, cont}, storage, tab, pat, fd) do
    :ok = :disk_log.alog_terms(fd, objs)
    log_terms(:mnesia_lib.db_select_cont(storage, cont, :_), storage, tab, pat, fd)
  end

  defp log_terms(:"$end_of_table", _, _, _, fd) do
    :disk_log.close(fd)
  end

  def load_from_logfile(storage, tab, f) do
    case :disk_log.open([{:name, make_ref()}, {:file, f}, {:repair, true}, {:linkto, self()}]) do
      {:ok, fd} ->
        chunk_from_log(:disk_log.chunk(fd, :start), fd, storage, tab)

      {:repaired, fd, _, _} ->
        chunk_from_log(:disk_log.chunk(fd, :start), fd, storage, tab)

      {:error, _} = e ->
        e
    end
  end

  defp chunk_from_log({cont, terms}, fd, storage, tab) do
    _ =
      for t <- terms do
        :mnesia_lib.db_put(storage, tab, t)
      end

    chunk_from_log(:disk_log.chunk(fd, cont), fd, storage, tab)
  end

  defp chunk_from_log(:eof, _, _, _) do
    :ok
  end

  def start_regulator() do
    case :mnesia_monitor.get_env(:dump_log_load_regulation) do
      false ->
        :nopid

      true ->
        n = :mnesia_dumper_load_regulator

        case :mnesia_monitor.start_proc(n, :mnesia_dumper, :regulator_init, [self()]) do
          {:ok, pid} ->
            pid

          {:error, reason} ->
            fatal('Failed to start ~n: ~tp~n', [n, reason])
        end
    end
  end

  def regulator_init(parent) do
    :erlang.process_flag(:priority, :low)
    :erlang.register(:mnesia_dumper_load_regulator, self())
    :proc_lib.init_ack(parent, {:ok, self()})
    regulator_loop()
  end

  defp regulator_loop() do
    receive do
      {:regulate, from} ->
        send(from, {:regulated, self()})
        regulator_loop()

      {:stop, from} ->
        send(from, {:stopped, self()})
        exit(:normal)
    end
  end

  defp regulate(:nopid) do
    :ok
  end

  defp regulate(regulatorPid) do
    send(regulatorPid, {:regulate, self()})

    receive do
      {:regulated, ^regulatorPid} ->
        :ok
    end
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
