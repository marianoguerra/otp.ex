defmodule :m_mnesia_loader do
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

  def disc_load_table(tab, reason) do
    storage = val({tab, :storage_type})
    type = val({tab, :setorbag})
    dbg_out('Getting table ~tp (~p) from disc: ~tp~n', [tab, storage, reason])
    :ok
    do_get_disc_copy2(tab, reason, storage, type)
  end

  defp do_get_disc_copy2(tab, _Reason, storage, _Type)
       when storage == :unknown do
    verbose('Local table copy of ~tp has recently been deleted, ignored.~n', [tab])
    {:not_loaded, :storage_unknown}
  end

  defp do_get_disc_copy2(tab, reason, storage, type)
       when storage == :disc_copies do
    repair = :mnesia_monitor.get_env(:auto_repair)
    storageProps = val({tab, :storage_properties})
    etsOpts = :proplists.get_value(:ets, storageProps, [])

    args = [
      {:keypos, 2},
      :public,
      :named_table,
      type
      | etsOpts
    ]

    case reason do
      {:dumper, dR} when is_atom(dR) ->
        :ignore

      _ ->
        :mnesia_monitor.mktab(tab, args)
        _Count = :mnesia_log.dcd2ets(tab, repair)

        case :mnesia_monitor.get_env(:dump_disc_copies_at_startup) and
               :mnesia_dumper.needs_dump_ets(tab) do
          true ->
            :ok = :mnesia_log.ets2dcd(tab)

          _ ->
            :ignore
        end
    end

    :mnesia_index.init_index(tab, storage)
    snmpify(tab, storage)
    set({tab, :load_node}, node())
    set({tab, :load_reason}, reason)
    {:loaded, :ok}
  end

  defp do_get_disc_copy2(tab, reason, storage, type)
       when storage == :ram_copies do
    storageProps = val({tab, :storage_properties})
    etsOpts = :proplists.get_value(:ets, storageProps, [])

    args = [
      {:keypos, 2},
      :public,
      :named_table,
      type
      | etsOpts
    ]

    case reason do
      {:dumper, dR} when is_atom(dR) ->
        :ignore

      _ ->
        :mnesia_monitor.mktab(tab, args)
        fname = :mnesia_lib.tab2dcd(tab)
        datname = :mnesia_lib.tab2dat(tab)
        repair = :mnesia_monitor.get_env(:auto_repair)

        case :mnesia_monitor.use_dir() do
          true ->
            case :mnesia_lib.exists(fname) do
              true ->
                :mnesia_log.dcd2ets(tab, repair)

              false ->
                case :mnesia_lib.exists(datname) do
                  true ->
                    :mnesia_lib.dets_to_ets(tab, tab, datname, type, repair, :no)

                  false ->
                    false
                end
            end

          false ->
            false
        end
    end

    :mnesia_index.init_index(tab, storage)
    snmpify(tab, storage)
    set({tab, :load_node}, node())
    set({tab, :load_reason}, reason)
    {:loaded, :ok}
  end

  defp do_get_disc_copy2(tab, reason, storage, type)
       when storage == :disc_only_copies do
    storageProps = val({tab, :storage_properties})
    detsOpts = :proplists.get_value(:dets, storageProps, [])

    args = [
      {:file, :mnesia_lib.tab2dat(tab)},
      {:type,
       :mnesia_lib.disk_type(
         tab,
         type
       )},
      {:keypos, 2},
      {:repair, :mnesia_monitor.get_env(:auto_repair)}
      | detsOpts
    ]

    case reason do
      {:dumper, dR} when is_atom(dR) ->
        :mnesia_index.init_index(tab, storage)
        snmpify(tab, storage)
        set({tab, :load_node}, node())
        set({tab, :load_reason}, reason)
        {:loaded, :ok}

      _ ->
        case :mnesia_monitor.open_dets(tab, args) do
          {:ok, _} ->
            :mnesia_index.init_index(tab, storage)
            snmpify(tab, storage)
            set({tab, :load_node}, node())
            set({tab, :load_reason}, reason)
            {:loaded, :ok}

          {:error, error} ->
            {:not_loaded, {'Failed to create dets table', error}}
        end
    end
  end

  defp do_get_disc_copy2(tab, reason, storage = {:ext, alias, mod}, _Type) do
    cs = val({tab, :cstruct})

    case :mnesia_monitor.unsafe_create_external(tab, alias, mod, cs) do
      :ok ->
        :ok = ext_load_table(mod, alias, tab, reason)
        :mnesia_index.init_index(tab, storage)
        set({tab, :load_node}, node())
        set({tab, :load_reason}, reason)
        {:loaded, :ok}

      other ->
        {:not_loaded, other}
    end
  end

  def net_load_table(tab, {:dumper, {:add_table_copy, _}} = reason, ns, cs) do
    try_net_load_table(tab, reason, ns, cs)
  end

  def net_load_table(tab, reason, ns, _Cs) do
    try_net_load_table(tab, reason, ns, val({tab, :cstruct}))
  end

  defp try_net_load_table(tab, _Reason, [], _Cs) do
    verbose('Copy failed. No active replicas of ~tp are available.~n', [tab])
    {:not_loaded, :none_active}
  end

  defp try_net_load_table(tab, reason, ns, cs) do
    storage = :mnesia_lib.cs_to_storage_type(node(), cs)
    do_get_network_copy(tab, reason, ns, storage, cs)
  end

  defp do_get_network_copy(tab, _Reason, _Ns, :unknown, _Cs) do
    verbose('Local table copy of ~tp has recently been deleted, ignored.~n', [tab])
    {:not_loaded, :storage_unknown}
  end

  defp do_get_network_copy(tab, reason, ns, storage, cs) do
    [node | tail] = ns

    case :lists.member(
           node,
           val({:current, :db_nodes})
         ) do
      true ->
        dbg_out('Getting table ~tp (~p) from node ~p: ~tp~n', [tab, storage, node, reason])
        :ok

        case init_receiver(node, tab, storage, cs, reason) do
          :ok ->
            set({tab, :load_node}, node)
            set({tab, :load_reason}, reason)
            :mnesia_controller.i_have_tab(tab)
            dbg_out('Table ~tp copied from ~p to ~p~n', [tab, node, node()])
            {:loaded, :ok}

          err = {:error, _}
          when :erlang.element(
                 1,
                 reason
               ) == :dumper ->
            {:not_loaded, err}

          :restart ->
            try_net_load_table(tab, reason, tail ++ [node], cs)

          :down ->
            try_net_load_table(tab, reason, tail, cs)
        end

      false ->
        try_net_load_table(tab, reason, tail, cs)
    end
  end

  defp snmpify(tab, storage) do
    do_snmpify(tab, val({tab, :snmp}), storage)
  end

  defp do_snmpify(_Tab, [], _Storage) do
    :ignore
  end

  defp do_snmpify(tab, us, storage) do
    snmp = :mnesia_snmp_hook.create_table(us, tab, storage)
    set({tab, {:index, :snmp}}, snmp)
  end

  defp init_receiver(node, tab, storage, cs, reas = {:dumper, {:add_table_copy, tid}}) do
    :rpc.call(node, :mnesia_lib, :set, [{:mnesia_loader, :active_trans}, tid])

    case start_remote_sender(node, tab, storage) do
      {senderPid, tabSize, detsData} ->
        start_receiver(tab, storage, cs, senderPid, tabSize, detsData, reas)

      else__ ->
        else__
    end
  end

  defp init_receiver(node, tab, storage, cs, reason) do
    load = fn ->
      {_, tid, ts} = :erlang.get(:mnesia_activity_state)
      :mnesia_locker.rlock(tid, r_tidstore(ts, :store), {:schema, tab})
      active = val({tab, :active_replicas})

      case val({tab, :where_to_read}) == node() do
        true ->
          :ok

        _ ->
          true = :lists.member(node, active)
          {senderPid, tabSize, detsData} = start_remote_sender(node, tab, storage)
          init = table_init_fun(senderPid, storage)
          args = [self(), tab, storage, cs, senderPid, tabSize, detsData, init]
          pid = spawn_link(:mnesia_loader, :spawned_receiver, args)
          :erlang.put(:mnesia_real_loader, pid)
          wait_on_load_complete(pid)
      end
    end

    res =
      case :mnesia.transaction(load, 20) do
        {:atomic, {:error, result}}
        when :erlang.element(
               1,
               reason
             ) == :dumper ->
          {:error, result}

        {:atomic, {:error, result}} ->
          fatal('Cannot create table ~tp: ~tp~n', [[tab, storage], result])

        {:atomic, result} ->
          result

        {:aborted, :nomore} ->
          :restart

        {:aborted, _Reas} ->
          verbose('Receiver failed on ~tp from ~p:~nReason: ~tp~n', [tab, node, _Reas])
          :down
      end

    :erlang.unlink(:erlang.whereis(:mnesia_tm))
    res
  end

  defp start_remote_sender(node, tab, storage) do
    :mnesia_controller.start_remote_sender(node, tab, self(), storage)
    :erlang.put(:mnesia_table_sender_node, {tab, node})

    receive do
      {senderPid, {:first, _} = msg}
      when is_pid(senderPid) and
             :erlang.element(1, storage) == :ext ->
        {:ext, alias, mod} = storage
        {sz, data} = mod.receiver_first_message(senderPid, msg, alias, tab)
        {senderPid, sz, data}

      {senderPid, {:first, tabSize}} = _M1 ->
        {senderPid, tabSize, false}

      {senderPid, {:first, tabSize, detsData}} ->
        {senderPid, tabSize, detsData}

      {:copier_done, ^node} ->
        verbose('Sender of table ~tp crashed on node ~p ~n', [tab, node])
        down(tab, storage)
    end
  end

  defp table_init_fun(senderPid, storage) do
    fn
      :read ->
        receiver = self()
        send(senderPid, {receiver, :more})
        get_data(senderPid, receiver, storage)

      :close ->
        :ok
    end
  end

  defp start_receiver(
         tab,
         storage,
         cs,
         senderPid,
         tabSize,
         detsData,
         {:dumper, {:add_table_copy, _}}
       ) do
    init = table_init_fun(senderPid, storage)

    case do_init_table(tab, storage, cs, senderPid, tabSize, detsData, self(), init) do
      err = {:error, _} ->
        send(senderPid, {:copier_done, node()})
        err

      else__ ->
        else__
    end
  end

  def spawned_receiver(replyTo, tab, storage, cs, senderPid, tabSize, detsData, init) do
    :erlang.process_flag(:trap_exit, true)
    done = do_init_table(tab, storage, cs, senderPid, tabSize, detsData, replyTo, init)

    try do
      send(replyTo, {self(), done})
      :erlang.unlink(replyTo)
      :erlang.unlink(:erlang.whereis(:mnesia_controller))
    catch
      _, _ ->
        :ok
    end

    exit(:normal)
  end

  defp wait_on_load_complete(pid) do
    receive do
      {^pid, res} ->
        res

      {:EXIT, ^pid, reason} ->
        :erlang.error(reason)

      else__ ->
        send(pid, else__)
        wait_on_load_complete(pid)
    end
  end

  defp do_init_table(tab, storage, cs, senderPid, tabSize, detsInfo, origTabRec, init) do
    case create_table(tab, tabSize, storage, cs) do
      {^storage, ^tab} ->
        node = node(senderPid)

        :erlang.put(
          :mnesia_table_receiver,
          {tab, node, senderPid}
        )

        :mnesia_tm.block_tab(tab)

        case init_table(tab, storage, init, detsInfo, senderPid) do
          :ok ->
            tab_receiver(node, tab, storage, cs, origTabRec)

          reason ->
            msg = '[d]ets:init table failed'
            verbose('~ts: ~tp: ~tp~n', [msg, tab, reason])
            down(tab, storage)
        end

      error ->
        error
    end
  end

  defp create_table(tab, tabSize, storage, cs) do
    storageProps = val({tab, :storage_properties})

    cond do
      storage == :disc_only_copies ->
        :mnesia_lib.lock_table(tab)
        tmp = :mnesia_lib.tab2tmp(tab)
        size = :lists.max([tabSize, 256])

        detsOpts =
          :lists.keydelete(
            :estimated_no_objects,
            1,
            :proplists.get_value(:dets, storageProps, [])
          )

        args = [
          {:file, tmp},
          {:keypos, 2},
          {:estimated_no_objects, size},
          {:repair, :mnesia_monitor.get_env(:auto_repair)},
          {:type,
           :mnesia_lib.disk_type(
             tab,
             r_cstruct(cs, :type)
           )}
          | detsOpts
        ]

        :file.delete(tmp)

        case :mnesia_lib.dets_sync_open(tab, args) do
          {:ok, _} ->
            :mnesia_lib.unlock_table(tab)
            {storage, tab}

          else__ ->
            :mnesia_lib.unlock_table(tab)
            else__
        end

      storage == :ram_copies or storage == :disc_copies ->
        etsOpts = :proplists.get_value(:ets, storageProps, [])
        args = [{:keypos, 2}, :public, :named_table, r_cstruct(cs, :type) | etsOpts]

        case :mnesia_monitor.unsafe_mktab(tab, args) do
          ^tab ->
            {storage, tab}

          else__ ->
            else__
        end

      :erlang.element(1, storage) == :ext ->
        {_, alias, mod} = storage

        case :mnesia_monitor.unsafe_create_external(tab, alias, mod, cs) do
          :ok ->
            {storage, tab}

          else__ ->
            else__
        end
    end
  end

  defp tab_receiver(node, tab, storage, cs, origTabRec) do
    receive do
      {senderPid, {:no_more, datBin}} ->
        finish_copy(storage, tab, cs, senderPid, datBin, origTabRec)

      {:copier_done, ^node} ->
        verbose('Sender of table ~tp crashed on node ~p ~n', [tab, node])
        down(tab, storage)

      {:EXIT, pid, reason} ->
        handle_exit(pid, reason)
        tab_receiver(node, tab, storage, cs, origTabRec)
    end
  end

  defp make_table_fun(pid, tabRec, storage) do
    fn
      :close ->
        :ok

      {:read, msg} ->
        send(pid, {tabRec, msg})
        get_data(pid, tabRec, storage)

      :read ->
        get_data(pid, tabRec, storage)
    end
  end

  defp get_data(pid, tabRec, storage) do
    receive do
      {^pid, {:more_z, compressedRecs}}
      when is_binary(compressedRecs) ->
        maybe_reply(pid, {tabRec, :more}, storage)
        {zlib_uncompress(compressedRecs), make_table_fun(pid, tabRec, storage)}

      {^pid, {:more, recs}} ->
        maybe_reply(pid, {tabRec, :more}, storage)
        {recs, make_table_fun(pid, tabRec, storage)}

      {^pid, :no_more} ->
        :end_of_input

      {:copier_done, node} ->
        case node(pid) do
          ^node ->
            {:copier_done, node}

          _ ->
            get_data(pid, tabRec, storage)
        end

      {:EXIT, ^pid, reason} ->
        handle_exit(pid, reason)
        get_data(pid, tabRec, storage)
    end
  end

  defp maybe_reply(_, _, {:ext, _, _}) do
    :ignore
  end

  defp maybe_reply(pid, msg, _) do
    send(pid, msg)
  end

  defp ext_init_table(alias, mod, tab, fun, state, sender) do
    :ok = ext_load_table(mod, alias, tab, {:net_load, node(sender)})
    ext_init_table(:read, alias, mod, tab, fun, state, sender)
  end

  defp ext_load_table(mod, alias, tab, reason) do
    cS = val({tab, :cstruct})
    mod.load_table(alias, tab, reason, :mnesia_schema.cs2list(cS))
  end

  defp ext_init_table(action, alias, mod, tab, fun, state, sender) do
    case fun.(action) do
      {:copier_done, node} ->
        verbose('Receiver of table ~tp crashed on ~p (more)~n', [tab, node])
        down(tab, {:ext, alias, mod})

      {data, newFun} ->
        case mod.receive_data(data, alias, tab, sender, state) do
          {:more, newState} ->
            ext_init_table({:read, :more}, alias, mod, tab, newFun, newState, sender)

          {{:more, msg}, newState} ->
            ext_init_table({:read, msg}, alias, mod, tab, newFun, newState, sender)
        end

      :end_of_input ->
        mod.receive_done(alias, tab, sender, state)
        :ok = fun.(:close)
    end
  end

  defp init_table(tab, {:ext, alias, mod}, fun, state, sender) do
    ext_init_table(alias, mod, tab, fun, state, sender)
  end

  defp init_table(tab, :disc_only_copies, fun, detsInfo, sender) do
    ertsVer = :erlang.system_info(:version)

    case detsInfo do
      {^ertsVer, detsData} ->
        try do
          :dets.is_compatible_bchunk_format(tab, detsData)
        catch
          :error, {:undef, [{:dets, _, _, _} | _]} ->
            send(sender, {self(), {:old_protocol, tab}})
            :dets.init_table(tab, fun)

          :error, what ->
            what
        else
          false ->
            send(sender, {self(), {:old_protocol, tab}})
            :dets.init_table(tab, fun)

          true ->
            :dets.init_table(tab, fun, [{:format, :bchunk}])
        end

      old when old != false ->
        send(sender, {self(), {:old_protocol, tab}})
        :dets.init_table(tab, fun)

      _ ->
        :dets.init_table(tab, fun)
    end
  end

  defp init_table(tab, _, fun, _DetsInfo, _) do
    try do
      true = :ets.init_table(tab, fun)
      :ok
    catch
      _, else__ ->
        {else__, __STACKTRACE__}
    end
  end

  defp finish_copy(storage, tab, cs, senderPid, datBin, origTabRec) do
    tabRef = {storage, tab}
    subscr_receiver(tabRef, r_cstruct(cs, :record_name))

    case handle_last(tabRef, r_cstruct(cs, :type), datBin) do
      :ok ->
        :mnesia_index.init_index(tab, storage)
        snmpify(tab, storage)
        send(senderPid, {origTabRec, :no_more})
        :mnesia_tm.unblock_tab(tab)
        :ok

      {:error, reason} ->
        msg = 'Failed to handle last'
        verbose('~ts: ~tp: ~tp~n', [msg, tab, reason])
        down(tab, storage)
    end
  end

  defp subscr_receiver(tabRef = {_, tab}, recName) do
    receive do
      {:mnesia_table_event, {op, val, _Tid}}
      when :erlang.element(1, val) === tab ->
        cond do
          tab == recName ->
            handle_event(tabRef, op, val)

          true ->
            handle_event(tabRef, op, :erlang.setelement(1, val, recName))
        end

        subscr_receiver(tabRef, recName)

      {:mnesia_table_event, {op, val, _Tid}}
      when :erlang.element(1, val) === :schema ->
        case op do
          :delete ->
            handle_event(tabRef, :clear_table, {tab, :all})

          _ ->
            :ok
        end

        subscr_receiver(tabRef, recName)

      {:EXIT, pid, reason} ->
        handle_exit(pid, reason)
        subscr_receiver(tabRef, recName)
    after
      0 ->
        :ok
    end
  end

  defp handle_event(tabRef, :write, rec) do
    db_put(tabRef, rec)
  end

  defp handle_event(tabRef, :delete, {_Tab, key}) do
    db_erase(tabRef, key)
  end

  defp handle_event(tabRef, :delete_object, oldRec) do
    db_match_erase(tabRef, oldRec)
  end

  defp handle_event(tabRef, :clear_table, {_Tab, _Key}) do
    db_match_erase(tabRef, :_)
  end

  defp handle_last({:disc_copies, tab}, _Type, :nobin) do
    ret = :mnesia_log.ets2dcd(tab)
    fname = :mnesia_lib.tab2dat(tab)

    case :mnesia_lib.exists(fname) do
      true ->
        :file.delete(fname)

      false ->
        :ok
    end

    ret
  end

  defp handle_last({:disc_only_copies, tab}, type, :nobin) do
    :mnesia_lib.dets_sync_close(tab)
    tmp = :mnesia_lib.tab2tmp(tab)
    dat = :mnesia_lib.tab2dat(tab)

    case :file.rename(tmp, dat) do
      :ok ->
        storageProps = val({tab, :storage_properties})
        detsOpts = :proplists.get_value(:dets, storageProps, [])

        args = [
          {:file, :mnesia_lib.tab2dat(tab)},
          {:type,
           :mnesia_lib.disk_type(
             tab,
             type
           )},
          {:keypos, 2},
          {:repair, :mnesia_monitor.get_env(:auto_repair)}
          | detsOpts
        ]

        :mnesia_monitor.open_dets(tab, args)
        :ok

      {:error, reason} ->
        {:error, {'Cannot swap tmp files', tab, reason}}
    end
  end

  defp handle_last({:ram_copies, _Tab}, _Type, :nobin) do
    :ok
  end

  defp handle_last({:ram_copies, tab}, _Type, datBin) do
    case :mnesia_monitor.use_dir() do
      true ->
        :mnesia_lib.lock_table(tab)
        tmp = :mnesia_lib.tab2tmp(tab)
        :ok = :file.write_file(tmp, datBin)
        :ok = :file.rename(tmp, :mnesia_lib.tab2dcd(tab))
        :mnesia_lib.unlock_table(tab)
        :ok

      false ->
        :ok
    end
  end

  defp handle_last(_Storage, _Type, :nobin) do
    :ok
  end

  defp down(tab, storage) do
    case storage do
      :ram_copies ->
        try do
          :ets.delete(tab)
        catch
          :error, _ ->
            :ok
        end

      :disc_copies ->
        try do
          :ets.delete(tab)
        catch
          :error, _ ->
            :ok
        end

      :disc_only_copies ->
        tmpFile = :mnesia_lib.tab2tmp(tab)
        :mnesia_lib.dets_sync_close(tab)
        :file.delete(tmpFile)

      {:ext, alias, mod} ->
        (fn ->
           try do
             mod.close_table(alias, tab)
           catch
             _, _Reason ->
               {:EXIT, _Reason}
           end
         end).()

        (fn ->
           try do
             mod.delete_table(alias, tab)
           catch
             _, _Reason ->
               {:EXIT, _Reason}
           end
         end).()
    end

    :mnesia_checkpoint.tm_del_copy(tab, node())

    :mnesia_controller.sync_del_table_copy_whereabouts(
      tab,
      node()
    )

    :mnesia_tm.unblock_tab(tab)
    flush_subcrs()
    :down
  end

  defp flush_subcrs() do
    receive do
      {:mnesia_table_event, _} ->
        flush_subcrs()

      {:EXIT, pid, reason} ->
        handle_exit(pid, reason)
        flush_subcrs()
    after
      0 ->
        :done
    end
  end

  defp db_erase({:ram_copies, tab}, key) do
    true = :ets.delete(tab, key)
  end

  defp db_erase({:disc_copies, tab}, key) do
    true = :ets.delete(tab, key)
  end

  defp db_erase({:disc_only_copies, tab}, key) do
    :ok = :dets.delete(tab, key)
  end

  defp db_erase({{:ext, alias, mod}, tab}, key) do
    :ok = mod.delete(alias, tab, key)
  end

  defp db_match_erase({:ram_copies, tab}, pat) do
    true = :ets.match_delete(tab, pat)
  end

  defp db_match_erase({:disc_copies, tab}, pat) do
    true = :ets.match_delete(tab, pat)
  end

  defp db_match_erase({:disc_only_copies, tab}, pat) do
    :ok = :dets.match_delete(tab, pat)
  end

  defp db_match_erase({{:ext, alias, mod}, tab}, pat) do
    case mod.match_delete(alias, tab, pat) do
      n when is_integer(n) ->
        :ok

      true ->
        :ok

      :ok ->
        :ok
    end
  end

  defp db_put({:ram_copies, tab}, val) do
    true = :ets.insert(tab, val)
  end

  defp db_put({:disc_copies, tab}, val) do
    true = :ets.insert(tab, val)
  end

  defp db_put({:disc_only_copies, tab}, val) do
    :ok = :dets.insert(tab, val)
  end

  defp db_put({{:ext, alias, mod}, tab}, val) do
    :ok = mod.insert(alias, tab, val)
  end

  defp calc_nokeys(storage, tab) do
    key = :mnesia_lib.db_first(storage, tab)
    recs = :mnesia_lib.db_get(storage, tab, key)
    binSize = :erlang.size(:erlang.term_to_binary(recs))
    div(7500, binSize) + 1
  end

  def send_table(pid, tab, remoteS) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :storage_type}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        {:error, {:no_exists, tab}}

      :unknown ->
        {:error, {:no_exists, tab}}

      storage ->
        do_send_table(pid, tab, storage, remoteS)
    end
  end

  defp do_send_table(pid, tab, storage, remoteS) do
    {init, chunk} =
      case storage do
        {:ext, alias, mod} ->
          case mod.sender_init(alias, tab, remoteS, pid) do
            {:standard, i, c} ->
              send(pid, {self(), {:first, mod.info(alias, tab, :size)}})
              {i, c}

            {_, _} = res ->
              res
          end

        ^storage ->
          tabSize = :mnesia.table_info(tab, :size)
          keysPerTransfer = calc_nokeys(storage, tab)
          chunkData = :dets.info(tab, :bchunk_format)

          useDetsChunk =
            storage == remoteS and storage == :disc_only_copies and chunkData != :undefined

          cond do
            useDetsChunk == true ->
              detsInfo = :erlang.system_info(:version)
              send(pid, {self(), {:first, tabSize, {detsInfo, chunkData}}})

            true ->
              send(pid, {self(), {:first, tabSize}})
          end

          {_I, _C} = reader_funcs(useDetsChunk, tab, storage, keysPerTransfer)
      end

    :erlang.put(:mnesia_table_sender, {tab, node(pid), pid})

    sendIt = fn ->
      needLock = need_lock(tab)
      {:atomic, :ok} = prepare_copy(pid, tab, storage, needLock)
      send_more(pid, 1, chunk, init.(), tab, storage)
      finish_copy(pid, tab, storage, remoteS, needLock)
    end

    try do
      sendIt.()
    catch
      :receiver_died ->
        cleanup_tab_copier(pid, storage, tab)
        :ok

      :error, reason ->
        cleanup_tab_copier(pid, storage, tab)
        {:error, {:tab_copier, tab, {reason, __STACKTRACE__}}}
    else
      {_, :receiver_died} ->
        :ok

      {:atomic, :no_more} ->
        :ok
    after
      :erlang.unlink(:erlang.whereis(:mnesia_tm))
    end
  end

  defp prepare_copy(pid, tab, storage, needLock) do
    trans = fn ->
      needLock and :mnesia.lock_table(tab, :load)
      :mnesia_subscr.subscribe(pid, {:table, tab})
      update_where_to_write(tab, node(pid))
      :mnesia_lib.db_fixtable(storage, tab, true)
      :ok
    end

    :mnesia.transaction(trans)
  end

  defp need_lock(tab) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {:mnesia_loader, :active_trans}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      r_tid() = tid ->
        :mnesia_lib.unset({:mnesia_loader, :active_trans})

        case :mnesia_locker.get_held_locks(tab) do
          [{:write, ^tid} | _] ->
            false

          _Locks ->
            true
        end

      _ ->
        true
    end
  end

  defp update_where_to_write(tab, node) do
    case val({tab, :access_mode}) do
      :read_only ->
        :ignore

      :read_write ->
        current = val({:current, :db_nodes})

        ns =
          case :lists.member(node, current) do
            true ->
              current

            false ->
              [node | current]
          end

        update_where_to_write(ns, tab, node)
    end
  end

  defp update_where_to_write([], _, _) do
    :ok
  end

  defp update_where_to_write([h | t], tab, addNode) do
    :rpc.call(h, :mnesia_controller, :call, [
      {:update_where_to_write, [:add, tab, addNode], self()}
    ])

    update_where_to_write(t, tab, addNode)
  end

  defp send_more(pid, n, chunk, dataState, tab, storage) do
    receive do
      {newPid, :more} ->
        case send_packet(n - 1, newPid, chunk, dataState) do
          new when is_integer(new) ->
            new - 1

          newData ->
            send_more(newPid, 20, chunk, newData, tab, storage)
        end

      {newPid, {:more, msg}}
      when :erlang.element(
             1,
             storage
           ) == :ext ->
        {:ext, alias, mod} = storage
        {newChunk, newState} = mod.sender_handle_info(msg, alias, tab, newPid, dataState)

        case send_packet(n - 1, newPid, newChunk, newState) do
          new when is_integer(new) ->
            new - 1

          newData ->
            send_more(newPid, n, newChunk, newData, tab, storage)
        end

      {_NewPid, {:old_protocol, ^tab}} ->
        ^storage = val({tab, :storage_type})
        {init, newChunk} = reader_funcs(false, tab, storage, calc_nokeys(storage, tab))
        send_more(pid, 1, newChunk, init.(), tab, storage)

      {:copier_done, node} when node == node(pid) ->
        verbose('Receiver of table ~tp crashed on ~p (more)~n', [tab, node])
        throw(:receiver_died)
    end
  end

  defp reader_funcs(useDetsChunk, tab, storage, keysPerTransfer) do
    case useDetsChunk do
      false ->
        {fn ->
           :mnesia_lib.db_init_chunk(storage, tab, keysPerTransfer)
         end,
         fn cont ->
           :mnesia_lib.db_chunk(storage, cont)
         end}

      true ->
        {fn ->
           dets_bchunk(tab, :start)
         end,
         fn cont ->
           dets_bchunk(tab, cont)
         end}
    end
  end

  defp dets_bchunk(tab, chunk) do
    case :dets.bchunk(tab, chunk) do
      {cont, data} ->
        {data, cont}

      else__ ->
        else__
    end
  end

  defp zlib_compress(data, level) do
    binData = :erlang.term_to_binary(data)
    z = :zlib.open()
    :zlib.deflateInit(z, level)
    bs = :zlib.deflate(z, binData, :finish)
    :zlib.deflateEnd(z)
    :zlib.close(z)
    :erlang.list_to_binary(bs)
  end

  defp zlib_uncompress(data) when is_binary(data) do
    :erlang.binary_to_term(:zlib.uncompress(data))
  end

  defp compression_level() do
    noCompression = 0

    case (try do
            :ets.lookup_element(:mnesia_gvar, :send_compressed, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        :mnesia_lib.set(:send_compressed, noCompression)
        noCompression

      val ->
        val
    end
  end

  defp send_packet(n, pid, _Chunk, :"$end_of_table") do
    send(pid, {self(), :no_more})
    n
  end

  defp send_packet(n, pid, chunk, {[], cont}) do
    send_packet(n, pid, chunk, chunk.(cont))
  end

  defp send_packet(n, pid, chunk, {recs, cont}) when n < 20 do
    case compression_level() do
      0 ->
        send(pid, {self(), {:more, recs}})

      level ->
        send(pid, {self(), {:more_z, zlib_compress(recs, level)}})
    end

    send_packet(n + 1, pid, chunk, chunk.(cont))
  end

  defp send_packet(_N, _Pid, _Chunk, dataState) do
    dataState
  end

  defp finish_copy(pid, tab, storage, remoteS, needLock) do
    recNode = node(pid)
    datBin = dat2bin(tab, storage, remoteS)
    node = node(pid)

    trans = fn ->
      needLock and :mnesia.read_lock_table(tab)

      receive do
        {:copier_done, ^node} ->
          throw(:receiver_died)
      after
        0 ->
          :ok
      end

      a = val({tab, :access_mode})

      :mnesia_controller.sync_and_block_table_whereabouts(
        tab,
        recNode,
        remoteS,
        a
      )

      cleanup_tab_copier(pid, storage, tab)
      :mnesia_checkpoint.tm_add_copy(tab, recNode)
      send(pid, {self(), {:no_more, datBin}})

      receive do
        {^pid, :no_more} ->
          :no_more

        {:copier_done, ^node} ->
          verbose('Tab receiver ~tp crashed (more): ~p~n', [tab, node])
          :receiver_died
      end
    end

    :mnesia.transaction(trans)
  end

  defp cleanup_tab_copier(pid, storage, tab) do
    :mnesia_lib.db_fixtable(storage, tab, false)
    :mnesia_subscr.unsubscribe(pid, {:table, tab})
  end

  defp dat2bin(tab, :ram_copies, :ram_copies) do
    :mnesia_lib.lock_table(tab)
    res = :file.read_file(:mnesia_lib.tab2dcd(tab))
    :mnesia_lib.unlock_table(tab)

    case res do
      {:ok, datBin} ->
        datBin

      _ ->
        :nobin
    end
  end

  defp dat2bin(_Tab, _LocalS, _RemoteS) do
    :nobin
  end

  defp handle_exit(pid, reason) when node(pid) == node() do
    :erlang.error(reason)
  end

  defp handle_exit(_Pid, _Reason) do
    :ignore
  end
end
