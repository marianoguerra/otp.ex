defmodule :m_mnesia_schema do
  use Bitwise
  import :mnesia_lib, only: [dbg_out: 2, del: 2, set: 2, verbose: 2]
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

  def init(ignoreFallback) do
    res = read_schema(true, ignoreFallback)
    {:ok, source, _CreateList} = exit_on_error(res)
    verbose('Schema initiated from: ~p~n', [source])
    set({:schema, :tables}, [])
    set({:schema, :local_tables}, [])
    do_set_schema(:schema)
    tabs = set_schema(:ets.first(:schema))

    :lists.foreach(
      fn tab ->
        clear_whereabouts(tab)
      end,
      tabs
    )

    set({:schema, :where_to_read}, node())
    set({:schema, :load_node}, node())
    set({:schema, :load_reason}, :initial)
    :mnesia_controller.add_active_replica(:schema, node())
    init_backends()
  end

  def init_backends() do
    backends =
      :lists.foldl(
        fn {alias, mod}, acc ->
          :orddict.append(mod, alias, acc)
        end,
        :orddict.new(),
        get_ext_types()
      )

    for {mod, aliases} <- backends do
      init_backend(mod, aliases)
    end

    :ok
  end

  defp init_backend(mod, [_ | _] = aliases) do
    case mod.init_backend() do
      :ok ->
        mod.add_aliases(aliases)

      error ->
        :mnesia.abort({:backend_init_error, error})
    end
  end

  defp exit_on_error({:error, reason}) do
    exit(reason)
  end

  defp exit_on_error(goodRes) do
    goodRes
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

  defp set_schema(:"$end_of_table") do
    []
  end

  defp set_schema(tab) do
    do_set_schema(tab)
    [tab | set_schema(:ets.next(:schema, tab))]
  end

  def get_create_list(tab) do
    :ets.lookup_element(:schema, tab, 3)
  end

  defp do_set_schema(tab) do
    list = get_create_list(tab)
    cs = list2cs(list)
    do_set_schema(tab, cs)
  end

  defp do_set_schema(tab, cs) do
    type = r_cstruct(cs, :type)
    set({tab, :setorbag}, type)
    set({tab, :local_content}, r_cstruct(cs, :local_content))
    set({tab, :ram_copies}, r_cstruct(cs, :ram_copies))
    set({tab, :disc_copies}, r_cstruct(cs, :disc_copies))
    set({tab, :disc_only_copies}, r_cstruct(cs, :disc_only_copies))
    set({tab, :external_copies}, r_cstruct(cs, :external_copies))
    set({tab, :load_order}, r_cstruct(cs, :load_order))
    set({tab, :access_mode}, r_cstruct(cs, :access_mode))
    set({tab, :majority}, r_cstruct(cs, :majority))
    set({tab, :all_nodes}, :mnesia_lib.cs_to_nodes(cs))
    set({tab, :snmp}, r_cstruct(cs, :snmp))
    set({tab, :user_properties}, r_cstruct(cs, :user_properties))

    for p <- r_cstruct(cs, :user_properties) do
      set({tab, :user_property, :erlang.element(1, p)}, p)
    end

    set({tab, :frag_properties}, r_cstruct(cs, :frag_properties))
    :mnesia_frag.set_frag_hash(tab, r_cstruct(cs, :frag_properties))

    set(
      {tab, :storage_properties},
      r_cstruct(cs, :storage_properties)
    )

    set({tab, :attributes}, r_cstruct(cs, :attributes))
    arity = length(r_cstruct(cs, :attributes)) + 1
    set({tab, :arity}, arity)
    recName = r_cstruct(cs, :record_name)
    set({tab, :record_name}, recName)
    set({tab, :wild_pattern}, wild(recName, arity))

    set(
      {tab, :index},
      for {p, _} <- r_cstruct(cs, :index) do
        p
      end
    )

    case r_cstruct(cs, :index) do
      [] ->
        set(
          {tab, :index_info},
          :mnesia_index.index_info(type, [])
        )

      _ ->
        :ignore
    end

    set({tab, :cookie}, r_cstruct(cs, :cookie))
    set({tab, :version}, r_cstruct(cs, :version))
    set({tab, :cstruct}, cs)

    storage =
      :mnesia_lib.schema_cs_to_storage_type(
        node(),
        cs
      )

    set({tab, :storage_type}, storage)
    set_record_validation(tab, storage, recName, arity, type)
    :mnesia_lib.add({:schema, :tables}, tab)
    ns = :mnesia_lib.cs_to_nodes(cs)

    case :lists.member(node(), ns) do
      true ->
        :mnesia_lib.add({:schema, :local_tables}, tab)

      false when tab == :schema ->
        :mnesia_lib.add({:schema, :local_tables}, tab)

      false ->
        :ignore
    end

    set_ext_types(tab, get_ext_types(), r_cstruct(cs, :external_copies))
  end

  defp set_record_validation(tab, {:ext, alias, mod}, recName, arity, type) do
    set(
      {tab, :record_validation},
      {recName, arity, type, alias, mod}
    )
  end

  defp set_record_validation(tab, _, recName, arity, type) do
    set({tab, :record_validation}, {recName, arity, type})
  end

  defp set_ext_types(tab, extTypes, extCopies) do
    :lists.foreach(
      fn {type, _} = key ->
        nodes =
          case :lists.keyfind(key, 1, extCopies) do
            {_, ns} ->
              ns

            false ->
              []
          end

        set({tab, type}, nodes)
      end,
      extTypes
    )
  end

  defp wild(recName, arity) do
    wp0 = :erlang.list_to_tuple(:lists.duplicate(arity, :_))
    :erlang.setelement(1, wp0, recName)
  end

  def read_nodes() do
    case :mnesia_lib.ensure_loaded(:mnesia) do
      :ok ->
        case read_schema(false) do
          {:ok, _Source, createList} ->
            cs = list2cs(createList)
            {:ok, r_cstruct(cs, :disc_copies) ++ r_cstruct(cs, :ram_copies)}

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  def version() do
    case read_schema(false) do
      {:ok, source, createList} when source != :default ->
        cs = list2cs(createList)
        {version, _Details} = r_cstruct(cs, :version)
        version

      _ ->
        case dir_exists(:mnesia_lib.dir()) do
          true ->
            {1, 0}

          false ->
            {0, 0}
        end
    end
  end

  def incr_version(cs) do
    {{major, minor}, _} = r_cstruct(cs, :version)

    nodes =
      :mnesia_lib.intersect(
        val({:schema, :disc_copies}),
        :mnesia_lib.cs_to_nodes(cs)
      )

    v =
      case nodes -- val({r_cstruct(cs, :name), :active_replicas}) do
        [] ->
          {major + 1, 0}

        _ ->
          {major, minor + 1}
      end

    r_cstruct(cs, version: {v, {node(), :erlang.timestamp()}})
  end

  def insert_cstruct(tid, cs, keepWhereabouts) do
    tab = r_cstruct(cs, :name)
    tabDef = cs2list(cs)
    val = {:schema, tab, tabDef}
    :mnesia_checkpoint.tm_retain(tid, :schema, tab, :write)
    :mnesia_subscr.report_table_event(:schema, tid, val, :write)
    active = val({tab, :active_replicas})

    case keepWhereabouts do
      true ->
        :ignore

      false when active == [] ->
        clear_whereabouts(tab)

      false ->
        :ignore
    end

    set({tab, :cstruct}, cs)
    :ets.insert(:schema, val)
    do_set_schema(tab, cs)
    val
  end

  defp clear_whereabouts(tab) do
    set({tab, :checkpoints}, [])
    set({tab, :subscribers}, [])
    set({tab, :where_to_read}, :nowhere)
    set({tab, :active_replicas}, [])
    set({tab, :commit_work}, [])
    set({tab, :where_to_write}, [])
    set({tab, :where_to_commit}, [])
    set({tab, :load_by_force}, false)
    set({tab, :load_node}, :unknown)
    set({tab, :load_reason}, :unknown)
  end

  def delete_cstruct(tid, cs) do
    tab = r_cstruct(cs, :name)
    tabDef = cs2list(cs)
    val = {:schema, tab, tabDef}
    :mnesia_checkpoint.tm_retain(tid, :schema, tab, :delete)
    :mnesia_subscr.report_table_event(:schema, tid, val, :delete)

    :mnesia_controller.update(fn ->
      :ets.match_delete(
        :mnesia_gvar,
        {{tab, :_}, :_}
      )

      :ets.match_delete(
        :mnesia_gvar,
        {{tab, :_, :_}, :_}
      )

      del({:schema, :local_tables}, tab)
      del({:schema, :tables}, tab)
      :ets.delete(:schema, tab)
    end)

    val
  end

  def delete_schema(ns) when is_list(ns) and ns != [] do
    runningNs = :mnesia_lib.running_nodes(ns)
    reason = 'Cannot delete schema on all nodes'

    cond do
      runningNs == [] ->
        case :rpc.multicall(ns, :mnesia_schema, :delete_schema2, []) do
          {replies, []} ->
            case (for r <- replies, r != :ok do
                    r
                  end) do
              [] ->
                :ok

              badReplies ->
                verbose('~s: ~tp~n', [reason, badReplies])
                {:error, {'All nodes not running', badReplies}}
            end

          {_Replies, badNs} ->
            verbose('~s: ~p~n', [reason, badNs])
            {:error, {'All nodes not running', badNs}}
        end

      true ->
        verbose('~s: ~p~n', [reason, runningNs])
        {:error, {'Mnesia is not stopped everywhere', runningNs}}
    end
  end

  def delete_schema(ns) do
    {:error, {:badarg, ns}}
  end

  def delete_schema2() do
    case :mnesia_lib.ensure_loaded(:mnesia) do
      :ok ->
        case :mnesia_lib.is_running() do
          :no ->
            dir = :mnesia_lib.dir()
            purge_dir(dir, [])
            :ok

          _ ->
            {:error, {'Mnesia still running', node()}}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  def ensure_no_schema([h | t]) when is_atom(h) do
    case :rpc.call(h, :mnesia_schema, :remote_read_schema, []) do
      {:badrpc, reason} ->
        {h, {'All nodes not running', h, reason}}

      {:ok, source, _} when source != :default ->
        {h, {:already_exists, h}}

      _ ->
        ensure_no_schema(t)
    end
  end

  def ensure_no_schema([h | _]) do
    {:error, {:badarg, h}}
  end

  def ensure_no_schema([]) do
    :ok
  end

  def remote_read_schema() do
    case :mnesia_lib.ensure_loaded(:mnesia) do
      :ok ->
        case :mnesia_monitor.get_env(:schema_location) do
          :opt_disc ->
            read_schema(false)

          _ ->
            read_schema(false)
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp dir_exists(dir) do
    dir_exists(dir, :mnesia_monitor.use_dir())
  end

  defp dir_exists(dir, true) do
    case :file.read_file_info(dir) do
      {:ok, _} ->
        true

      _ ->
        false
    end
  end

  defp dir_exists(_Dir, false) do
    false
  end

  def opt_create_dir(useDir, dir) when useDir == true do
    case dir_exists(dir, useDir) do
      true ->
        check_can_write(dir)

      false ->
        case :file.make_dir(dir) do
          :ok ->
            verbose('Create Directory ~tp~n', [dir])
            :ok

          {:error, reason} ->
            verbose('Cannot create mnesia dir ~tp~n', [reason])
            {:error, {'Cannot create Mnesia dir', dir, reason}}
        end
    end
  end

  def opt_create_dir(false, _) do
    {:error, {:has_no_disc, node()}}
  end

  defp check_can_write(dir) do
    case :file.read_file_info(dir) do
      {:ok, fI}
      when r_file_info(fI, :type) == :directory and
             r_file_info(fI, :access) == :read_write ->
        :ok

      {:ok, _} ->
        {:error, 'Not allowed to write in Mnesia dir', dir}

      _ ->
        {:error, 'Non existent Mnesia dir', dir}
    end
  end

  def lock_schema() do
    :mnesia_lib.lock_table(:schema)
  end

  def unlock_schema() do
    :mnesia_lib.unlock_table(:schema)
  end

  defp read_schema(keep) do
    read_schema(keep, false)
  end

  defp read_schema(keep, ignoreFallback) do
    lock_schema()

    res =
      case :mnesia.system_info(:is_running) do
        :yes ->
          {:ok, :ram, get_create_list(:schema)}

        _IsRunning ->
          case :mnesia_monitor.use_dir() do
            true ->
              read_disc_schema(keep, ignoreFallback)

            false when keep == true ->
              args = [{:keypos, 2}, :public, :named_table, :set]
              :mnesia_monitor.mktab(:schema, args)
              createList = get_initial_schema(:ram_copies, [])
              :ets.insert(:schema, {:schema, :schema, createList})
              {:ok, :default, createList}

            false when keep == false ->
              createList = get_initial_schema(:ram_copies, [])
              {:ok, :default, createList}
          end
      end

    unlock_schema()
    res
  end

  defp read_disc_schema(keep, ignoreFallback) do
    running = :mnesia.system_info(:is_running)

    case :mnesia_bup.fallback_exists() do
      true when ignoreFallback == false and running != :yes ->
        :mnesia_bup.fallback_to_schema()

      _ ->
        dat = :mnesia_lib.tab2dat(:schema)

        case :mnesia_lib.exists(dat) do
          true ->
            do_read_disc_schema(dat, keep)

          false ->
            dmp = :mnesia_lib.tab2dmp(:schema)

            case :mnesia_lib.exists(dmp) do
              true ->
                do_read_disc_schema(dmp, keep)

              false ->
                {:error, 'No schema file exists'}
            end
        end
    end
  end

  defp do_read_disc_schema(fname, keep) do
    t =
      case keep do
        false ->
          args = [{:keypos, 2}, :public, :set]
          _ = :ets.new(:schema, args)

        true ->
          args = [{:keypos, 2}, :public, :named_table, :set]
          :mnesia_monitor.mktab(:schema, args)
      end

    repair = :mnesia_monitor.get_env(:auto_repair)

    res =
      case :mnesia_lib.dets_to_ets(:schema, t, fname, :set, repair, :no) do
        :loaded ->
          {:ok, :disc, :ets.lookup_element(t, :schema, 3)}

        other ->
          {:error, {'Cannot read schema', fname, other}}
      end

    case keep do
      true ->
        :ignore

      false ->
        :ets.delete(t)
    end

    res
  end

  defp get_initial_schema(schemaStorage, nodes) do
    get_initial_schema(schemaStorage, nodes, [])
  end

  def get_initial_schema(schemaStorage, nodes, properties) do
    userProps = initial_schema_properties(properties)

    cs =
      r_cstruct(
        name: :schema,
        record_name: :schema,
        attributes: [:table, :cstruct],
        user_properties: userProps
      )

    cs2 =
      case schemaStorage do
        :ram_copies ->
          r_cstruct(cs, ram_copies: nodes)

        :disc_copies ->
          r_cstruct(cs, disc_copies: nodes)
      end

    cs2list(cs2)
  end

  defp initial_schema_properties(props0) do
    defaultProps = remove_duplicates(:mnesia_monitor.get_env(:schema))

    props =
      :lists.foldl(
        fn {k, v}, acc ->
          :lists.keystore(k, 1, acc, {k, v})
        end,
        defaultProps,
        remove_duplicates(props0)
      )

    initial_schema_properties_(props)
  end

  defp initial_schema_properties_([{:backend_types, types} | props]) do
    :lists.foreach(
      fn {name, module} ->
        verify_backend_type(name, module)
      end,
      types
    )

    [
      {:mnesia_backend_types, types}
      | initial_schema_properties_(props)
    ]
  end

  defp initial_schema_properties_([{:index_plugins, plugins} | props]) do
    :lists.foreach(
      fn {name, module, function} ->
        verify_index_plugin(name, module, function)
      end,
      plugins
    )

    [
      {:mnesia_index_plugins, plugins}
      | initial_schema_properties_(props)
    ]
  end

  defp initial_schema_properties_([p | _Props]) do
    :mnesia.abort({:bad_schema_property, p})
  end

  defp initial_schema_properties_([]) do
    []
  end

  defp remove_duplicates([{k, _} = h | t]) do
    [
      h
      | remove_duplicates(
          for {k1, _} = x <- t,
              k1 !== k do
            x
          end
        )
    ]
  end

  defp remove_duplicates([]) do
    []
  end

  def read_cstructs_from_disc() do
    fname = :mnesia_lib.tab2dat(:schema)

    case :mnesia_lib.exists(fname) do
      true ->
        args = [
          {:file, fname},
          {:keypos, 2},
          {:repair, :mnesia_monitor.get_env(:auto_repair)},
          {:type, :set}
        ]

        case :dets.open_file(make_ref(), args) do
          {:ok, tab} ->
            extTypes = get_ext_types_disc()

            fun = fn {_, _, list} ->
              {:continue, list2cs(list, extTypes)}
            end

            cstructs = :dets.traverse(tab, fun)
            :dets.close(tab)
            {:ok, cstructs}

          {:error, reason} ->
            {:error, reason}
        end

      false ->
        {:error, 'No schema file exists'}
    end
  end

  def get_tid_ts_and_lock(tab, intent) do
    tidTs = :erlang.get(:mnesia_activity_state)

    case tidTs do
      {_Mod, tid, ts} when elem(ts, 0) === :tidstore ->
        store = r_tidstore(ts, :store)

        case intent do
          :read ->
            :mnesia_locker.rlock_table(tid, store, tab)

          :write ->
            :mnesia_locker.wlock_table(tid, store, tab)

          :none ->
            :ignore
        end

        tidTs

      _ ->
        :mnesia.abort(:no_transaction)
    end
  end

  def schema_transaction(fun) do
    case :erlang.get(:mnesia_activity_state) do
      :undefined ->
        args = [self(), fun, :erlang.whereis(:mnesia_controller)]
        pid = spawn_link(:mnesia_schema, :schema_coordinator, args)

        receive do
          {:transaction_done, res, ^pid} ->
            res

          {:EXIT, ^pid, r} ->
            {:aborted, {:transaction_crashed, r}}
        end

      _ ->
        {:aborted, :nested_transaction}
    end
  end

  def schema_coordinator(client, _Fun, :undefined) do
    res = {:aborted, {:node_not_running, node()}}
    send(client, {:transaction_done, res, self()})
    :erlang.unlink(client)
  end

  def schema_coordinator(client, fun, controller)
      when is_pid(controller) do
    :erlang.put(:transaction_client, client)
    :erlang.link(controller)
    :erlang.unlink(client)
    res = :mnesia.transaction(fun)
    send(client, {:transaction_done, res, self()})
    :erlang.unlink(controller)
    :erlang.unlink(:erlang.whereis(:mnesia_tm))
    exit(:normal)
  end

  def insert_schema_ops({_Mod, _Tid, ts}, schemaIOps) do
    do_insert_schema_ops(r_tidstore(ts, :store), schemaIOps)
  end

  defp do_insert_schema_ops(store, [head | tail]) do
    :ets.insert(store, head)
    do_insert_schema_ops(store, tail)
  end

  defp do_insert_schema_ops(_Store, []) do
    :ok
  end

  defp api_list2cs(list) when is_list(list) do
    name = pick(:unknown, :name, list, :must)
    keys = check_keys(name, list)
    check_duplicates(name, keys)
    list2cs(list)
  end

  defp api_list2cs(other) do
    :mnesia.abort({:badarg, other})
  end

  def vsn_cs2list(cs) do
    cs2list(cs)
  end

  defp cs2list(false, cs) do
    cs2list(cs)
  end

  def cs2list(cs) when elem(cs, 0) === :cstruct do
    tags = Keyword.keys(r_cstruct(r_cstruct()))
    rec2list(tags, tags, 2, cs)
  end

  def cs2list(createList) when is_list(createList) do
    createList
  end

  def cs2list(cs)
      when :erlang.element(1, cs) == :cstruct and
             tuple_size(cs) == 20 do
    tags = [
      :name,
      :type,
      :ram_copies,
      :disc_copies,
      :disc_only_copies,
      :external_copies,
      :load_order,
      :access_mode,
      :majority,
      :index,
      :snmp,
      :local_content,
      :record_name,
      :attributes,
      :user_properties,
      :frag_properties,
      :storage_properties,
      :cookie,
      :version
    ]

    rec2list(tags, tags, 2, cs)
  end

  def cs2list(cs)
      when :erlang.element(1, cs) == :cstruct and
             tuple_size(cs) == 19 do
    tags = [
      :name,
      :type,
      :ram_copies,
      :disc_copies,
      :disc_only_copies,
      :load_order,
      :access_mode,
      :majority,
      :index,
      :snmp,
      :local_content,
      :record_name,
      :attributes,
      :user_properties,
      :frag_properties,
      :storage_properties,
      :cookie,
      :version
    ]

    rec2list(tags, tags, 2, cs)
  end

  defp rec2list([:index | tags], [:index | orig], pos, rec) do
    val = :erlang.element(pos, rec)

    [
      {:index,
       :lists.map(
         fn
           {_, _Type} = p ->
             p

           p when is_integer(p) or is_atom(p) ->
             {p, :ordered}
         end,
         val
       )}
      | rec2list(tags, orig, pos + 1, rec)
    ]
  end

  defp rec2list([:external_copies | tags], orig0, pos, rec) do
    orig =
      case orig0 do
        [:external_copies | rest] ->
          rest

        _ ->
          orig0
      end

    val = :erlang.element(pos, rec)

    for {{alias, _}, ns} <- val do
      {alias, ns}
    end ++ rec2list(tags, orig, pos + 1, rec)
  end

  defp rec2list([tag | tags], [tag | orig], pos, rec) do
    val = :erlang.element(pos, rec)
    [{tag, val} | rec2list(tags, orig, pos + 1, rec)]
  end

  defp rec2list([], _, _Pos, _Rec) do
    []
  end

  defp rec2list(tags, [_ | orig], pos, rec) do
    rec2list(tags, orig, pos + 1, rec)
  end

  def normalize_cs(cstructs, _Node) do
    cstructs
  end

  def list2cs(list) do
    list2cs(list, get_ext_types())
  end

  def list2cs(list, extTypes) when is_list(list) do
    name = pick(:unknown, :name, list, :must)
    type = pick(name, :type, list, :set)
    rc0 = pick(name, :ram_copies, list, [])
    dc = pick(name, :disc_copies, list, [])
    doc = pick(name, :disc_only_copies, list, [])
    ext = pick_external_copies(list, extTypes)

    rc =
      case {rc0, dc, doc, ext} do
        {[], [], [], []} ->
          [node()]

        _ ->
          rc0
      end

    lC = pick(name, :local_content, list, false)
    recName = pick(name, :record_name, list, name)
    attrs = pick(name, :attributes, list, [:key, :val])
    snmp = pick(name, :snmp, list, [])
    loadOrder = pick(name, :load_order, list, 0)
    accessMode = pick(name, :access_mode, list, :read_write)
    majority = pick(name, :majority, list, false)
    userProps = pick(name, :user_properties, list, [])

    verify(
      {:alt, [nil, :list]},
      :mnesia_lib.etype(userProps),
      {:bad_type, name, {:user_properties, userProps}}
    )

    cookie =
      pick(
        name,
        :cookie,
        list,
        {{:erlang.monotonic_time() + :erlang.time_offset(), :erlang.unique_integer(), 1}, node()}
      )

    version = pick(name, :version, list, {{2, 0}, []})
    ix = pick(name, :index, list, [])
    verify({:alt, [nil, :list]}, :mnesia_lib.etype(ix), {:bad_type, name, {:index, [ix]}})
    frag = pick(name, :frag_properties, list, [])

    verify(
      {:alt, [nil, :list]},
      :mnesia_lib.etype(frag),
      {:badarg, name, {:frag_properties, frag}}
    )

    bEProps = pick(name, :storage_properties, list, [])

    verify(
      {:alt, [nil, :list]},
      :mnesia_lib.etype(ix),
      {:badarg, name, {:storage_properties, bEProps}}
    )

    checkProp = fn
      opt, opts when is_atom(opt) ->
        :lists.member(opt, opts) and :mnesia.abort({:badarg, name, opt})

      tuple, opts when is_tuple(tuple) ->
        :lists.member(
          :erlang.element(1, tuple),
          opts
        ) and :mnesia.abort({:badarg, name, tuple})

      what, _ ->
        :mnesia.abort({:badarg, name, what})
    end

    badEtsOpts = [
      :set,
      :ordered_set,
      :bag,
      :duplicate_bag,
      :public,
      :private,
      :protected,
      :keypos,
      :named_table
    ]

    etsOpts = :proplists.get_value(:ets, bEProps, [])
    is_list(etsOpts) or :mnesia.abort({:badarg, name, {:ets, etsOpts}})

    for prop <- etsOpts do
      checkProp.(prop, badEtsOpts)
    end

    badDetsOpts = [:type, :keypos, :repair, :access, :file]
    detsOpts = :proplists.get_value(:dets, bEProps, [])
    is_list(detsOpts) or :mnesia.abort({:badarg, name, {:dets, detsOpts}})

    for prop <- detsOpts do
      checkProp.(prop, badDetsOpts)
    end

    case :erlang.whereis(:mnesia_controller) do
      :undefined ->
        :ignore

      pid when is_pid(pid) ->
        keys = check_keys(name, list)
        check_duplicates(name, keys)
    end

    cs0 =
      r_cstruct(
        name: name,
        ram_copies: rc,
        disc_copies: dc,
        disc_only_copies: doc,
        external_copies: ext,
        type: type,
        index: ix,
        snmp: snmp,
        load_order: loadOrder,
        access_mode: accessMode,
        majority: majority,
        local_content: lC,
        record_name: recName,
        attributes: attrs,
        user_properties: :lists.sort(userProps),
        frag_properties: :lists.sort(frag),
        storage_properties: :lists.sort(bEProps),
        cookie: cookie,
        version: version
      )

    case ix do
      [] ->
        cs0

      [_ | _] ->
        ix2 = expand_index_attrs(cs0)
        r_cstruct(cs0, index: ix2)
    end
  end

  def list2cs(other, _ExtTypes) do
    :mnesia.abort({:badarg, other})
  end

  def pick(tab, key, list, default) do
    case :lists.keysearch(key, 1, list) do
      false when default == :must ->
        :mnesia.abort({:badarg, tab, 'Missing key', key, list})

      false ->
        default

      {:value, {^key, value}} ->
        value

      {:value, badArg} ->
        :mnesia.abort({:bad_type, tab, badArg})
    end
  end

  defp pick_external_copies(_List, []) do
    []
  end

  defp pick_external_copies(list, extTypes) do
    :lists.foldr(
      fn {k, val}, acc ->
        case :lists.keyfind(k, 1, extTypes) do
          false ->
            acc

          {_, mod} ->
            [{{k, mod}, val} | acc]
        end
      end,
      [],
      list
    )
  end

  defp expand_storage_type(s)
       when s == :ram_copies or s == :disc_copies or
              s == :disc_only_copies do
    s
  end

  defp expand_storage_type(s) do
    case :lists.keyfind(s, 1, get_ext_types()) do
      false ->
        :mnesia.abort({:bad_type, {:storage_type, s}})

      {alias, mod} ->
        {:ext, alias, mod}
    end
  end

  defp get_ext_types() do
    get_schema_user_property(:mnesia_backend_types)
  end

  defp get_index_plugins() do
    get_schema_user_property(:mnesia_index_plugins)
  end

  defp get_schema_user_property(key) do
    case dirty_read_table_property(:schema, key) do
      :undefined ->
        []

      {_, types} ->
        types
    end
  end

  defp get_ext_types_disc() do
    try do
      get_ext_types_disc_()
    catch
      :error, _ ->
        []
    end
  end

  defp get_ext_types_disc_() do
    case :mnesia_schema.remote_read_schema() do
      {:ok, _, prop} ->
        k1 = :user_properties

        case :lists.keyfind(k1, 1, prop) do
          {^k1, userProp} ->
            k2 = :mnesia_backend_types

            case :lists.keyfind(k2, 1, userProp) do
              {^k2, types} ->
                types

              _ ->
                []
            end

          _ ->
            []
        end

      _ ->
        []
    end
  end

  def attr_tab_to_pos(_Tab, pos) when is_integer(pos) do
    pos
  end

  def attr_tab_to_pos(tab, attr) do
    attr_to_pos(attr, val({tab, :attributes}))
  end

  def attr_to_pos({_} = p, _) do
    p
  end

  def attr_to_pos(pos, _Attrs) when is_integer(pos) do
    pos
  end

  def attr_to_pos(attr, attrs) when is_atom(attr) do
    attr_to_pos(attr, attrs, 2)
  end

  def attr_to_pos(attr, _) do
    :mnesia.abort({:bad_type, attr})
  end

  defp attr_to_pos(attr, [attr | _Attrs], pos) do
    pos
  end

  defp attr_to_pos(attr, [_ | attrs], pos) do
    attr_to_pos(attr, attrs, pos + 1)
  end

  defp attr_to_pos(attr, _, _) do
    :mnesia.abort({:bad_type, attr})
  end

  defp check_keys(tab, attrs) do
    types =
      for {t, _} <- get_ext_types() do
        t
      end

    check_keys(tab, attrs, types ++ Keyword.keys(r_cstruct(r_cstruct())))
  end

  def check_keys(tab, [{key, _Val} | tail], items) do
    key1 =
      cond do
        is_tuple(key) ->
          :erlang.element(1, key)

        true ->
          key
      end

    case :lists.member(key1, items) do
      true ->
        [key | check_keys(tab, tail, items)]

      false ->
        :mnesia.abort({:badarg, tab, key})
    end
  end

  def check_keys(_, [], _) do
    []
  end

  def check_keys(tab, arg, _) do
    :mnesia.abort({:badarg, tab, arg})
  end

  def check_duplicates(tab, keys) do
    case has_duplicates(keys) do
      false ->
        :ok

      true ->
        :mnesia.abort({:badarg, tab, 'Duplicate keys', keys})
    end
  end

  defp has_duplicates([h | t]) do
    case :lists.member(h, t) do
      true ->
        true

      false ->
        has_duplicates(t)
    end
  end

  defp has_duplicates([]) do
    false
  end

  defp verify_cstruct(r_cstruct() = cs) do
    assert_correct_cstruct(cs)
    cs1 = verify_external_copies(r_cstruct(cs, index: expand_index_attrs(cs)))
    assert_correct_cstruct(cs1)
    cs1
  end

  defp expand_index_attrs(r_cstruct(index: ix, attributes: attrs, name: tab) = cs) do
    prefered = prefered_index_types(cs)
    expand_index_attrs(ix, tab, attrs, prefered)
  end

  defp expand_index_attrs(ix, tab, attrs, prefered) do
    :lists.map(
      fn
        p when is_integer(p) or is_atom(p) ->
          {attr_to_pos(p, attrs), prefered}

        {a} = p when is_atom(a) ->
          {p, prefered}

        {p, type} ->
          {attr_to_pos(p, attrs), type}

        _Other ->
          :mnesia.abort({:bad_type, tab, {:index, ix}})
      end,
      ix
    )
  end

  defp prefered_index_types(r_cstruct(external_copies: ext)) do
    extTypes =
      for {s, ns} <- ext, ns !== [] do
        :mnesia_lib.semantics(s, :index_types)
      end

    case intersect_types(extTypes) do
      [] ->
        :ordered

      [pref | _] ->
        pref
    end
  end

  defp intersect_types([]) do
    []
  end

  defp intersect_types([s1, s2 | rest]) do
    intersect_types([s1 -- s1 -- s2 | rest])
  end

  defp intersect_types([s]) do
    s
  end

  defp verify_external_copies(r_cstruct(external_copies: []) = cs) do
    cs
  end

  defp verify_external_copies(r_cstruct(name: tab, external_copies: eC) = cs) do
    bad = {:bad_type, tab, {:external_copies, eC}}

    allECNodes =
      :lists.concat(
        for {_, ns} <- eC,
            is_list(ns) do
          ns
        end
      )

    verify(
      true,
      length(:lists.usort(allECNodes)) == length(allECNodes),
      bad
    )

    csL = cs2list(cs)

    csL1 =
      :lists.foldl(
        fn
          {{alias, mod}, ns} = _X, csLx ->
            badTab = fn why ->
              {why, tab, {{:ext, alias, mod}, ns}}
            end

            verify(:atom, :mnesia_lib.etype(mod), badTab)

            verify(
              true,
              fn ->
                :lists.all(&is_atom/1, ns)
              end,
              badTab
            )

            check_semantics(mod, alias, badTab, cs)

            try do
              mod.check_definition(alias, tab, ns, csLx)
            catch
              :error, e ->
                :mnesia.abort(badTab.(e))
            else
              :ok ->
                csLx

              {:ok, csLx1} ->
                csLx1

              {:error, reason} ->
                :mnesia.abort(badTab.(reason))
            end

          _, csLx ->
            csLx
        end,
        csL,
        eC
      )

    list2cs(csL1)
  end

  defp check_semantics(mod, alias, badTab, r_cstruct(type: type)) do
    ext = {:ext, alias, mod}

    case :lists.member(
           :mnesia_lib.semantics(
             ext,
             :storage
           ),
           [:ram_copies, :disc_copies, :disc_only_copies]
         ) do
      false ->
        :mnesia.abort(badTab.(:invalid_storage))

      true ->
        :ok
    end

    case :lists.member(
           type,
           :mnesia_lib.semantics(ext, :types)
         ) do
      false ->
        :mnesia.abort(badTab.(:bad_type))

      true ->
        :ok
    end
  end

  defp assert_correct_cstruct(cs) when elem(cs, 0) === :cstruct do
    verify_nodes(cs)
    tab = r_cstruct(cs, :name)
    verify(:atom, :mnesia_lib.etype(tab), {:bad_type, tab})
    type = r_cstruct(cs, :type)

    verify(
      true,
      :lists.member(type, [:set, :bag, :ordered_set]),
      {:bad_type, tab, {:type, type}}
    )

    cond do
      type == :ordered_set and
          r_cstruct(cs, :disc_only_copies) != [] ->
        :mnesia.abort({:bad_type, tab, {:not_supported, type, :disc_only_copies}})

      true ->
        :ok
    end

    recName = r_cstruct(cs, :record_name)
    verify(:atom, :mnesia_lib.etype(recName), {:bad_type, tab, {:record_name, recName}})
    attrs = r_cstruct(cs, :attributes)
    verify(:list, :mnesia_lib.etype(attrs), {:bad_type, tab, {:attributes, attrs}})
    arity = length(attrs) + 1
    verify(true, arity > 2, {:bad_type, tab, {:attributes, attrs}})

    :lists.foldl(
      fn
        attr, _Other when attr == :snmp ->
          :mnesia.abort({:bad_type, tab, {:attributes, [attr]}})

        attr, other ->
          verify(:atom, :mnesia_lib.etype(attr), {:bad_type, tab, {:attributes, [attr]}})

          verify(
            false,
            :lists.member(attr, other),
            {:combine_error, tab, {:attributes, [attr | other]}}
          )

          [attr | other]
      end,
      [],
      attrs
    )

    index = r_cstruct(cs, :index)
    verify({:alt, [nil, :list]}, :mnesia_lib.etype(index), {:bad_type, tab, {:index, index}})
    ixPlugins = get_index_plugins()
    allowIndexOnKey = check_if_allow_index_on_key()

    ixFun = fn pos ->
      verify(
        true,
        fn ->
          i = index_pos(pos)

          case pos do
            {_, t} ->
              (t == :bag or t == :ordered) and
                good_ix_pos(
                  i,
                  allowIndexOnKey,
                  arity,
                  ixPlugins
                )

            _ ->
              good_ix_pos(pos, allowIndexOnKey, arity, ixPlugins)
          end
        end,
        {:bad_type, tab, {:index, [pos]}}
      )
    end

    :lists.foreach(ixFun, index)
    lC = r_cstruct(cs, :local_content)
    verify({:alt, [true, false]}, lC, {:bad_type, tab, {:local_content, lC}})
    access = r_cstruct(cs, :access_mode)
    verify({:alt, [:read_write, :read_only]}, access, {:bad_type, tab, {:access_mode, access}})
    majority = r_cstruct(cs, :majority)
    verify({:alt, [true, false]}, majority, {:bad_type, tab, {:majority, majority}})

    case majority do
      true ->
        verify(false, lC, {:combine_error, tab, [{:local_content, true}, {:majority, true}]})

      false ->
        :ok
    end

    snmp = r_cstruct(cs, :snmp)
    verify(true, :mnesia_snmp_hook.check_ustruct(snmp), {:badarg, tab, {:snmp, snmp}})

    checkProp = fn
      prop
      when is_tuple(prop) and
             :erlang.size(prop) >= 1 ->
        :ok

      prop ->
        :mnesia.abort({:bad_type, tab, {:user_properties, [prop]}})
    end

    :lists.foreach(checkProp, r_cstruct(cs, :user_properties))

    case r_cstruct(cs, :cookie) do
      {{megaSecs, secs, microSecs}, _Node}
      when is_integer(megaSecs) and is_integer(secs) and
             is_integer(microSecs) and is_atom(:node) ->
        :ok

      cookie ->
        :mnesia.abort({:bad_type, tab, {:cookie, cookie}})
    end

    case r_cstruct(cs, :version) do
      {{major, minor}, _Detail}
      when is_integer(major) and
             is_integer(minor) ->
        :ok

      version ->
        :mnesia.abort({:bad_type, tab, {:version, version}})
    end
  end

  defp good_ix_pos({_} = p, _, _, plugins) do
    :lists.keymember(p, 1, plugins)
  end

  defp good_ix_pos(i, true, arity, _) when is_integer(i) do
    i >= 0 and i <= arity
  end

  defp good_ix_pos(i, false, arity, _) when is_integer(i) do
    i > 2 and i <= arity
  end

  defp good_ix_pos(_, _, _, _) do
    false
  end

  defp check_if_allow_index_on_key() do
    case :mnesia_monitor.get_env(:allow_index_on_key) do
      true ->
        true

      _ ->
        false
    end
  end

  defp verify_nodes(cs) do
    tab = r_cstruct(cs, :name)
    ram = r_cstruct(cs, :ram_copies)
    disc = r_cstruct(cs, :disc_copies)
    discOnly = r_cstruct(cs, :disc_only_copies)

    ext =
      :lists.append(
        for {_, ns} <- r_cstruct(cs, :external_copies) do
          ns
        end
      )

    loadOrder = r_cstruct(cs, :load_order)
    verify({:alt, [nil, :list]}, :mnesia_lib.etype(ram), {:bad_type, tab, {:ram_copies, ram}})
    verify({:alt, [nil, :list]}, :mnesia_lib.etype(disc), {:bad_type, tab, {:disc_copies, disc}})

    :lists.foreach(
      fn {bE, ns} ->
        verify({:alt, [nil, :list]}, :mnesia_lib.etype(ns), {:bad_type, tab, {bE, ns}})

        :lists.foreach(
          fn n ->
            verify(:atom, :mnesia_lib.etype(n), {:bad_type, tab, {bE, ns}})
          end,
          ns
        )
      end,
      r_cstruct(cs, :external_copies)
    )

    case tab do
      :schema ->
        verify([], discOnly, {:bad_type, tab, {:disc_only_copies, discOnly}})

      _ ->
        verify(
          {:alt, [nil, :list]},
          :mnesia_lib.etype(discOnly),
          {:bad_type, tab, {:disc_only_copies, discOnly}}
        )
    end

    verify(:integer, :mnesia_lib.etype(loadOrder), {:bad_type, tab, {:load_order, loadOrder}})
    nodes = ram ++ disc ++ discOnly ++ ext

    verify(
      :list,
      :mnesia_lib.etype(nodes),
      {:combine_error, tab,
       [{:ram_copies, []}, {:disc_copies, []}, {:disc_only_copies, []}, {:external_copies, []}]}
    )

    verify(false, has_duplicates(nodes), {:combine_error, tab, nodes})

    atomCheck = fn n ->
      verify(:atom, :mnesia_lib.etype(n), {:bad_type, tab, n})
    end

    :lists.foreach(atomCheck, nodes)
  end

  def verify(expected, fun, error) when is_function(fun) do
    do_verify(
      expected,
      try do
        fun.()
      catch
        _, _Reason ->
          {:EXIT, _Reason}
      end,
      error
    )
  end

  def verify(expected, actual, error) do
    do_verify(expected, actual, error)
  end

  defp do_verify({:alt, values}, value, error) do
    case :lists.member(value, values) do
      true ->
        :ok

      false ->
        :mnesia.abort(error)
    end
  end

  defp do_verify(value, value, _) do
    :ok
  end

  defp do_verify(_Value, _, error) do
    :mnesia.abort(error)
  end

  defp ensure_writable(tab) do
    case val({tab, :where_to_write}) do
      [] ->
        :mnesia.abort({:read_only, tab})

      _ ->
        :ok
    end
  end

  def ensure_active(cs) do
    ensure_active(cs, :active_replicas)
  end

  defp ensure_active(cs, what) do
    tab = r_cstruct(cs, :name)
    w = {tab, what}
    ensure_non_empty(w)

    nodes =
      :mnesia_lib.intersect(
        val({:schema, :disc_copies}),
        :mnesia_lib.cs_to_nodes(cs)
      )

    case nodes -- val(w) do
      [] ->
        :ok

      ns ->
        expl = 'All replicas on diskfull nodes are not active yet'

        case val({tab, :local_content}) do
          true ->
            case :rpc.multicall(ns, :mnesia_schema, :is_remote_member, [w]) do
              {replies, []} ->
                check_active(replies, expl, tab)

              {_Replies, badNs} ->
                :mnesia.abort({:not_active, expl, tab, badNs})
            end

          false ->
            :mnesia.abort({:not_active, expl, tab, ns})
        end
    end
  end

  defp ensure_non_empty({tab, vhat}) do
    case val({tab, vhat}) do
      [] ->
        :mnesia.abort({:no_exists, tab})

      _ ->
        :ok
    end
  end

  defp ensure_not_active(tab = :schema, node) do
    active = val({tab, :active_replicas})

    case :lists.member(node, active) do
      false when active !== [] ->
        :ok

      false ->
        :mnesia.abort({:no_exists, tab})

      true ->
        expl = 'Mnesia is running'
        :mnesia.abort({:active, expl, node})
    end
  end

  def is_remote_member(key) do
    isActive = :lists.member(node(), val(key))
    {isActive, node()}
  end

  defp check_active([{true, _Node} | replies], expl, tab) do
    check_active(replies, expl, tab)
  end

  defp check_active([{false, node} | _Replies], expl, tab) do
    :mnesia.abort({:not_active, expl, tab, [node]})
  end

  defp check_active([{:badrpc, reason} | _Replies], expl, tab) do
    :mnesia.abort({:not_active, expl, tab, reason})
  end

  defp check_active([], _Expl, _Tab) do
    :ok
  end

  def add_backend_type(name, module) do
    case schema_transaction(fn ->
           do_add_backend_type(name, module)
         end) do
      {:atomic, needsInit} ->
        case needsInit do
          true ->
            module.init_backend()

          false ->
            :ignore
        end

        module.add_aliases([name])
        {:atomic, :ok}

      other ->
        other
    end
  end

  def do_add_backend_type(name, module) do
    verify_backend_type(name, module)

    types =
      case do_read_table_property(
             :schema,
             :mnesia_backend_types
           ) do
        :undefined ->
          []

        {_, ts} ->
          case :lists.keymember(name, 1, ts) do
            true ->
              :mnesia.abort({:backend_type_already_exists, name})

            false ->
              ts
          end
      end

    moduleRegistered = :lists.keymember(module, 2, types)

    do_write_table_property(
      :schema,
      {:mnesia_backend_types, [{name, module} | types]}
    )

    moduleRegistered
  end

  def delete_backend_type(name) do
    schema_transaction(fn ->
      do_delete_backend_type(name)
    end)
  end

  def do_delete_backend_type(name) do
    case do_read_table_property(
           :schema,
           :mnesia_backend_types
         ) do
      :undefined ->
        []

      {_, ts} ->
        case :lists.keyfind(name, 1, ts) do
          {_, mod} ->
            case using_backend_type(name, mod) do
              [_ | _] = tabs ->
                :mnesia.abort({:backend_in_use, {name, tabs}})

              [] ->
                do_write_table_property(
                  :schema,
                  {:mnesia_backend_types, :lists.keydelete(name, 1, ts)}
                )
            end

          false ->
            :mnesia.abort({:no_such_backend, name})
        end
    end
  end

  defp using_backend_type(name, mod) do
    ext =
      :ets.select(
        :mnesia_gvar,
        [{{{:"$1", :external_copies}, :"$2"}, [], [{{:"$1", :"$2"}}]}]
      )

    entry = {name, mod}

    for {t, c} <- ext, :lists.keymember(entry, 1, c) do
      t
    end
  end

  defp verify_backend_type(name, module) do
    case legal_backend_name(name) do
      false ->
        :mnesia.abort({:bad_type, {:backend_type, name, module}})

      true ->
        :ok
    end

    expectedExports = :mnesia_backend_type.behaviour_info(:callbacks)

    exports =
      try do
        module.module_info(:exports)
      catch
        :error, _ ->
          :mnesia.abort({:undef_backend, module})
      end

    case expectedExports -- exports do
      [] ->
        :ok

      _Other ->
        :io.fwrite(:user, 'Missing backend_type exports: ~tp~n', [_Other])
        :mnesia.abort({:bad_type, {:backend_type, name, module}})
    end
  end

  defp legal_backend_name(name) do
    is_atom(name) and not :lists.member(name, Keyword.keys(r_cstruct(r_cstruct())))
  end

  def backend_types() do
    [
      :ram_copies,
      :disc_copies,
      :disc_only_copies
      | for {t, _} <- get_ext_types() do
          t
        end
    ]
  end

  def add_index_plugin(name, module, function) do
    schema_transaction(fn ->
      do_add_index_plugin(name, module, function)
    end)
  end

  def do_add_index_plugin(name, module, function) do
    verify_index_plugin(name, module, function)

    plugins =
      case do_read_table_property(
             :schema,
             :mnesia_index_plugins
           ) do
        :undefined ->
          []

        {_, ps} ->
          case :lists.keymember(name, 1, ps) do
            true ->
              :mnesia.abort({:index_plugin_already_exists, name})

            false ->
              ps
          end
      end

    do_write_table_property(
      :schema,
      {:mnesia_index_plugins, [{name, module, function} | plugins]}
    )
  end

  def delete_index_plugin(p) do
    schema_transaction(fn ->
      do_delete_index_plugin(p)
    end)
  end

  def do_delete_index_plugin({a} = p) when is_atom(a) do
    plugins = get_index_plugins()

    case :lists.keyfind(p, 1, plugins) do
      false ->
        :mnesia.abort({:no_exists, {:index_plugin, p}})

      _Found ->
        case :ets.select(
               :mnesia_gvar,
               [
                 {{{:"$1", {:index, {p, :_}}}, :_}, [], [:"$1"]},
                 {{{:"$1", {:index, p}}, :_}, [], [:"$1"]}
               ],
               1
             ) do
          {[_], _} ->
            :mnesia.abort({:plugin_in_use, p})

          :"$end_of_table" ->
            do_write_table_property(
              :schema,
              {:mnesia_index_plugins, :lists.keydelete(p, 1, plugins)}
            )
        end
    end
  end

  defp verify_index_plugin({a} = name, module, function)
       when is_atom(a) and is_atom(module) and
              is_atom(function) do
    case :code.ensure_loaded(module) do
      {:error, :nofile} ->
        :mnesia.abort({:bad_type, {:index_plugin, name, module, function}})

      {:module, _} ->
        case :erlang.function_exported(module, function, 3) do
          true ->
            :ok

          false ->
            :mnesia.abort({:bad_type, {:index_plugin, name, module, function}})
        end
    end
  end

  defp verify_index_plugin(name, module, function) do
    :mnesia.abort({:bad_type, {:index_plugin, name, module, function}})
  end

  def index_plugins() do
    get_index_plugins()
  end

  def create_table([_ | _] = tabDef) do
    schema_transaction(fn ->
      do_multi_create_table(tabDef)
    end)
  end

  def create_table(arg) do
    {:aborted, {:badarg, arg}}
  end

  defp do_multi_create_table(tabDef) do
    get_tid_ts_and_lock(:schema, :write)
    ensure_writable(:schema)
    do_create_table(tabDef)
    :ok
  end

  def do_create_table(tabDef) when is_list(tabDef) do
    cs = api_list2cs(tabDef)

    case r_cstruct(cs, :frag_properties) do
      [] ->
        do_create_table_1(cs)

      _Props ->
        csList = :mnesia_frag.expand_cstruct(cs)
        :lists.foreach(&do_create_table_1/1, csList)
    end
  end

  defp do_create_table_1(cs) do
    {_Mod, _Tid, ts} = get_tid_ts_and_lock(:schema, :none)
    store = r_tidstore(ts, :store)
    do_insert_schema_ops(store, make_create_table(cs))
  end

  def make_create_table(cs) do
    tab = r_cstruct(cs, :name)
    verify(false, check_if_exists(tab), {:already_exists, tab})
    unsafe_make_create_table(cs)
  end

  defp unsafe_make_create_table(cs0) do
    {_Mod, tid, ts} = get_tid_ts_and_lock(:schema, :none)
    cs = verify_cstruct(cs0)
    tab = r_cstruct(cs, :name)
    discNodes = r_cstruct(cs, :disc_copies) ++ r_cstruct(cs, :disc_only_copies)
    runningNodes = val({:current, :db_nodes})

    checkDisc = fn n ->
      verify(true, :lists.member(n, runningNodes), {:not_active, tab, n})
    end

    :lists.foreach(checkDisc, discNodes)

    nodes =
      :mnesia_lib.intersect(
        :mnesia_lib.cs_to_nodes(cs),
        runningNodes
      )

    store = r_tidstore(ts, :store)
    :mnesia_locker.wlock_no_exist(tid, store, tab, nodes)
    [{:op, :create_table, vsn_cs2list(cs)}]
  end

  defp check_if_exists(tab) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    {_, _, ts} = tidTs
    store = r_tidstore(ts, :store)

    :ets.foldl(
      fn
        {:op, :create_table, [{:name, t} | _]}, _Acc
        when t == tab ->
          true

        {:op, :delete_table, [{:name, t} | _]}, _Acc
        when t == tab ->
          false

        _Other, acc ->
          acc
      end,
      existed_before(tab),
      store
    )
  end

  defp existed_before(tab) do
    :EXIT !==
      :erlang.element(
        1,
        try do
          :ets.lookup_element(:mnesia_gvar, {tab, :cstruct}, 2)
        catch
          :error, _ ->
            {:EXIT, {:badarg, []}}
        end
      )
  end

  def delete_table(tab) do
    schema_transaction(fn ->
      do_delete_table(tab)
    end)
  end

  def do_delete_table(:schema) do
    :mnesia.abort({:bad_type, :schema})
  end

  def do_delete_table(tab) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    ensure_writable(:schema)

    insert_schema_ops(
      tidTs,
      make_delete_table(tab, :whole_table)
    )
  end

  def make_delete_table(tab, mode) do
    case existed_before(tab) do
      false ->
        tidTs = get_tid_ts_and_lock(:schema, :write)
        {_, _, ts} = tidTs
        store = r_tidstore(ts, :store)

        deleted =
          :ets.select_delete(
            store,
            [
              {{:op, :"$1", [{:name, tab} | :_]},
               [{:or, {:==, :"$1", :create_table}, {:==, :"$1", :delete_table}}], [true]}
            ]
          )

        :ets.select_delete(
          store,
          [
            {{:op, :"$1", [{:name, tab} | :_], :_},
             [{:or, {:==, :"$1", :write_table_property}, {:==, :"$1", :delete_table_property}}],
             [true]}
          ]
        )

        case deleted do
          0 ->
            :mnesia.abort({:no_exists, tab})

          _ ->
            []
        end

      true ->
        case mode do
          :whole_table ->
            case val({tab, :frag_properties}) do
              [] ->
                [make_delete_table2(tab)]

              _Props ->
                :mnesia_frag.lookup_frag_hash(tab)
                f = :mnesia_frag.lookup_foreigners(tab)
                verify([], f, {:combine_error, tab, 'Too many foreigners', f})

                for t <- :mnesia_frag.frag_names(tab) do
                  make_delete_table2(t)
                end
            end

          :single_frag ->
            [make_delete_table2(tab)]
        end
    end
  end

  defp make_delete_table2(tab) do
    get_tid_ts_and_lock(tab, :write)
    cs = val({tab, :cstruct})
    ensure_active(cs)
    ensure_writable(tab)
    {:op, :delete_table, vsn_cs2list(cs)}
  end

  def change_table_frag(tab, change) do
    schema_transaction(fn ->
      do_change_table_frag(tab, change)
    end)
  end

  defp do_change_table_frag(tab, change)
       when is_atom(tab) and
              tab != :schema do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    ops = :mnesia_frag.change_table_frag(tab, change)

    for op <- ops do
      insert_schema_ops(tidTs, op)
    end

    :ok
  end

  defp do_change_table_frag(tab, _Change) do
    :mnesia.abort({:bad_type, tab})
  end

  defp do_clear_table(:schema) do
    :mnesia.abort({:bad_type, :schema})
  end

  defp do_clear_table(tab) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    get_tid_ts_and_lock(tab, :write)
    insert_schema_ops(tidTs, make_clear_table(tab))
  end

  defp make_clear_table(tab) do
    cs = val({tab, :cstruct})
    ensure_writable(tab)
    [{:op, :clear_table, vsn_cs2list(cs)}]
  end

  def add_table_copy(tab, node, storage) do
    schema_transaction(fn ->
      do_add_table_copy(tab, node, storage)
    end)
  end

  defp do_add_table_copy(tab, node, storage)
       when is_atom(tab) and
              is_atom(node) do
    tidTs = get_tid_ts_and_lock(:schema, :write)

    insert_schema_ops(
      tidTs,
      make_add_table_copy(tab, node, storage)
    )
  end

  defp do_add_table_copy(tab, node, _) do
    :mnesia.abort({:badarg, tab, node})
  end

  defp make_add_table_copy(tab, node, storage) do
    ensure_writable(:schema)
    cs = incr_version(val({tab, :cstruct}))
    ns = :mnesia_lib.cs_to_nodes(cs)
    verify(false, :lists.member(node, ns), {:already_exists, tab, node})
    cs2 = verify_cstruct(new_cs(cs, node, storage, :add))

    isRunning =
      :lists.member(
        node,
        val({:current, :db_nodes})
      )

    cond do
      tab == :schema ->
        cond do
          storage != :ram_copies ->
            :mnesia.abort({:badarg, tab, storage})

          isRunning == true ->
            :mnesia.abort({:already_exists, tab, node})

          true ->
            :ignore
        end

      storage == :ram_copies ->
        :ignore

      isRunning == true ->
        :ignore

      isRunning == false ->
        :mnesia.abort({:not_active, :schema, node})
    end

    [{:op, :add_table_copy, storage, node, vsn_cs2list(cs2)}]
  end

  def del_table_copy(tab, node) do
    schema_transaction(fn ->
      do_del_table_copy(tab, node)
    end)
  end

  defp do_del_table_copy(tab, node) when is_atom(node) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    insert_schema_ops(tidTs, make_del_table_copy(tab, node))
  end

  defp do_del_table_copy(tab, node) do
    :mnesia.abort({:badarg, tab, node})
  end

  defp make_del_table_copy(tab, node) do
    ensure_writable(:schema)
    cs = incr_version(val({tab, :cstruct}))

    storage =
      :mnesia_lib.schema_cs_to_storage_type(
        node,
        cs
      )

    cs2 = new_cs(cs, node, storage, :del)

    case :mnesia_lib.cs_to_nodes(cs2) do
      [] when tab == :schema ->
        :mnesia.abort({:combine_error, tab, 'Last replica'})

      [] ->
        ensure_active(cs)
        dbg_out('Last replica deleted in table ~tp~n', [tab])
        make_delete_table(tab, :whole_table)

      _ when tab == :schema ->
        ensure_not_active(tab, node)
        cs3 = verify_cstruct(cs2)

        ops =
          remove_node_from_tabs(
            val({:schema, :tables}),
            node
          )

        [
          {:op, :del_table_copy, :ram_copies, node, vsn_cs2list(cs3)}
          | ops
        ]

      _ ->
        ensure_active(cs)
        cs3 = verify_cstruct(cs2)
        get_tid_ts_and_lock(tab, :write)
        [{:op, :del_table_copy, storage, node, vsn_cs2list(cs3)}]
    end
  end

  defp remove_node_from_tabs([], _Node) do
    []
  end

  defp remove_node_from_tabs([:schema | rest], node) do
    remove_node_from_tabs(rest, node)
  end

  defp remove_node_from_tabs([tab | rest], node) do
    {cs, isFragModified} =
      :mnesia_frag.remove_node(
        node,
        incr_version(val({tab, :cstruct}))
      )

    case :mnesia_lib.schema_cs_to_storage_type(
           node,
           cs
         ) do
      :unknown ->
        case isFragModified do
          true ->
            [
              {:op, :change_table_frag, {:del_node, node}, vsn_cs2list(cs)}
              | remove_node_from_tabs(rest, node)
            ]

          false ->
            remove_node_from_tabs(rest, node)
        end

      storage ->
        cs2 = new_cs(cs, node, storage, :del)

        case :mnesia_lib.cs_to_nodes(cs2) do
          [] ->
            [
              {:op, :delete_table, vsn_cs2list(cs)}
              | remove_node_from_tabs(rest, node)
            ]

          _Ns ->
            cs3 = verify_cstruct(cs2)
            get_tid_ts_and_lock(tab, :write)

            [
              {:op, :del_table_copy, :ram_copies, node, vsn_cs2list(cs3)}
              | remove_node_from_tabs(rest, node)
            ]
        end
    end
  end

  defp new_cs(cs, node, :ram_copies, :add) do
    r_cstruct(cs, ram_copies: opt_add(node, r_cstruct(cs, :ram_copies)))
  end

  defp new_cs(cs, node, :disc_copies, :add) do
    r_cstruct(cs, disc_copies: opt_add(node, r_cstruct(cs, :disc_copies)))
  end

  defp new_cs(cs, node, :disc_only_copies, :add) do
    r_cstruct(cs,
      disc_only_copies:
        opt_add(
          node,
          r_cstruct(cs, :disc_only_copies)
        )
    )
  end

  defp new_cs(cs, node, :ram_copies, :del) do
    r_cstruct(cs,
      ram_copies:
        :lists.delete(
          node,
          r_cstruct(cs, :ram_copies)
        )
    )
  end

  defp new_cs(cs, node, :disc_copies, :del) do
    r_cstruct(cs,
      disc_copies:
        :lists.delete(
          node,
          r_cstruct(cs, :disc_copies)
        )
    )
  end

  defp new_cs(cs, node, :disc_only_copies, :del) do
    r_cstruct(cs,
      disc_only_copies:
        :lists.delete(
          node,
          r_cstruct(cs, :disc_only_copies)
        )
    )
  end

  defp new_cs(r_cstruct(external_copies: extCps) = cs, node, storage0, op) do
    storage =
      case storage0 do
        {:ext, alias, _} ->
          alias

        alias ->
          alias
      end

    extTypes = get_ext_types()

    case :lists.keyfind(storage, 1, extTypes) do
      false ->
        :mnesia.abort({:badarg, r_cstruct(cs, :name), storage})

      {_, mod} ->
        key = {storage, mod}

        case {:lists.keymember(key, 1, extCps), op} do
          {false, :del} ->
            cs

          {false, :add} ->
            r_cstruct(cs, external_copies: [{key, [node]} | extCps])

          {true, _} ->
            f = fn
              {k, ns} when k == key ->
                case op do
                  :del ->
                    {k, :lists.delete(node, ns)}

                  :add ->
                    {k, opt_add(node, ns)}
                end

              x ->
                x
            end

            r_cstruct(cs, external_copies: :lists.map(f, extCps))
        end
    end
  end

  defp opt_add(n, l) do
    [n | :lists.delete(n, l)]
  end

  def move_table(tab, fromNode, toNode) do
    schema_transaction(fn ->
      do_move_table(tab, fromNode, toNode)
    end)
  end

  defp do_move_table(:schema, _FromNode, _ToNode) do
    :mnesia.abort({:bad_type, :schema})
  end

  defp do_move_table(tab, fromNode, toNode)
       when is_atom(fromNode) and is_atom(toNode) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    get_tid_ts_and_lock(tab, :write)

    insert_schema_ops(
      tidTs,
      make_move_table(tab, fromNode, toNode)
    )
  end

  defp do_move_table(tab, fromNode, toNode) do
    :mnesia.abort({:badarg, tab, fromNode, toNode})
  end

  defp make_move_table(tab, fromNode, toNode) do
    ensure_writable(:schema)
    cs = incr_version(val({tab, :cstruct}))
    ns = :mnesia_lib.cs_to_nodes(cs)
    verify(false, :lists.member(toNode, ns), {:already_exists, tab, toNode})

    verify(
      true,
      :lists.member(fromNode, val({tab, :where_to_write})),
      {:not_active, tab, fromNode}
    )

    verify(false, val({tab, :local_content}), {'Cannot move table with local content', tab})
    ensure_active(cs)
    running = val({:current, :db_nodes})

    storage =
      :mnesia_lib.schema_cs_to_storage_type(
        fromNode,
        cs
      )

    verify(true, :lists.member(toNode, running), {:not_active, :schema, toNode})
    cs2 = new_cs(cs, toNode, storage, :add)
    cs3 = verify_cstruct(new_cs(cs2, fromNode, storage, :del))

    [
      {:op, :add_table_copy, storage, toNode, vsn_cs2list(cs2)},
      {:op, :sync_trans},
      {:op, :del_table_copy, storage, fromNode, vsn_cs2list(cs3)}
    ]
  end

  def change_table_copy_type(tab, node, toS) do
    schema_transaction(fn ->
      do_change_table_copy_type(tab, node, toS)
    end)
  end

  def do_change_table_copy_type(tab, node, toS) when is_atom(node) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    get_tid_ts_and_lock(tab, :write)

    insert_schema_ops(
      tidTs,
      make_change_table_copy_type(tab, node, toS)
    )
  end

  def do_change_table_copy_type(tab, node, _ToS) do
    :mnesia.abort({:badarg, tab, node})
  end

  defp make_change_table_copy_type(tab, node, :unknown) do
    make_del_table_copy(tab, node)
  end

  defp make_change_table_copy_type(tab, node, toS) do
    ensure_writable(:schema)
    cs = incr_version(val({tab, :cstruct}))
    fromS = :mnesia_lib.storage_type_at_node(node, tab)
    toSExp = expand_storage_type(toS)

    case compare_storage_type(false, fromS, toSExp) do
      {:same, _} ->
        :mnesia.abort({:already_exists, tab, node, toSExp})

      {:diff, _} ->
        :ignore

      :incompatible ->
        ensure_active(cs)
    end

    cs2 = new_cs(cs, node, fromS, :del)
    cs3 = verify_cstruct(new_cs(cs2, node, toS, :add))
    [{:op, :change_table_copy_type, node, fromS, toSExp, vsn_cs2list(cs3)}]
  end

  def add_table_index(tab, pos) do
    schema_transaction(fn ->
      do_add_table_index(tab, pos)
    end)
  end

  defp do_add_table_index(:schema, _Attr) do
    :mnesia.abort({:bad_type, :schema})
  end

  defp do_add_table_index(tab, attr) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    get_tid_ts_and_lock(tab, :read)
    pos = attr_tab_to_pos(tab, attr)
    insert_schema_ops(tidTs, make_add_table_index(tab, pos))
  end

  defp make_add_table_index(tab, pos) do
    ensure_writable(:schema)
    cs = incr_version(val({tab, :cstruct}))
    ensure_active(cs)
    ix = r_cstruct(cs, :index)
    verify(false, :lists.keymember(index_pos(pos), 1, ix), {:already_exists, tab, pos})
    ix2 = :lists.sort([pos | ix])
    cs2 = verify_cstruct(r_cstruct(cs, index: ix2))
    newPosInfo = :lists.keyfind(pos, 1, r_cstruct(cs2, :index))
    [{:op, :add_index, newPosInfo, vsn_cs2list(cs2)}]
  end

  def del_table_index(tab, pos) do
    schema_transaction(fn ->
      do_del_table_index(tab, pos)
    end)
  end

  defp do_del_table_index(:schema, _Attr) do
    :mnesia.abort({:bad_type, :schema})
  end

  defp do_del_table_index(tab, attr) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    get_tid_ts_and_lock(tab, :read)
    pos = attr_tab_to_pos(tab, attr)
    insert_schema_ops(tidTs, make_del_table_index(tab, pos))
  end

  defp make_del_table_index(tab, pos) do
    ensure_writable(:schema)
    cs = incr_version(val({tab, :cstruct}))
    ensure_active(cs)
    ix = r_cstruct(cs, :index)
    verify(true, :lists.keymember(pos, 1, ix), {:no_exists, tab, pos})
    cs2 = verify_cstruct(r_cstruct(cs, index: :lists.keydelete(pos, 1, ix)))
    [{:op, :del_index, pos, vsn_cs2list(cs2)}]
  end

  def add_snmp(tab, ustruct) do
    schema_transaction(fn ->
      do_add_snmp(tab, ustruct)
    end)
  end

  defp do_add_snmp(:schema, _Ustruct) do
    :mnesia.abort({:bad_type, :schema})
  end

  defp do_add_snmp(tab, ustruct) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    get_tid_ts_and_lock(tab, :read)
    insert_schema_ops(tidTs, make_add_snmp(tab, ustruct))
  end

  defp make_add_snmp(tab, ustruct) do
    ensure_writable(:schema)
    cs = incr_version(val({tab, :cstruct}))
    ensure_active(cs)
    verify([], r_cstruct(cs, :snmp), {:already_exists, tab, :snmp})
    error = {:badarg, tab, :snmp, ustruct}
    verify(true, :mnesia_snmp_hook.check_ustruct(ustruct), error)
    cs2 = verify_cstruct(r_cstruct(cs, snmp: ustruct))
    [{:op, :add_snmp, ustruct, vsn_cs2list(cs2)}]
  end

  def del_snmp(tab) do
    schema_transaction(fn ->
      do_del_snmp(tab)
    end)
  end

  defp do_del_snmp(:schema) do
    :mnesia.abort({:bad_type, :schema})
  end

  defp do_del_snmp(tab) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    get_tid_ts_and_lock(tab, :read)
    insert_schema_ops(tidTs, make_del_snmp(tab))
  end

  defp make_del_snmp(tab) do
    ensure_writable(:schema)
    cs = incr_version(val({tab, :cstruct}))
    ensure_active(cs)
    cs2 = verify_cstruct(r_cstruct(cs, snmp: []))
    [{:op, :del_snmp, vsn_cs2list(cs2)}]
  end

  def transform_table(tab, fun, newAttrs, newRecName)
      when is_function(fun) and is_list(newAttrs) and
             is_atom(newRecName) do
    schema_transaction(fn ->
      do_transform_table(tab, fun, newAttrs, newRecName)
    end)
  end

  def transform_table(tab, :ignore, newAttrs, newRecName)
      when is_list(newAttrs) and is_atom(newRecName) do
    schema_transaction(fn ->
      do_transform_table(tab, :ignore, newAttrs, newRecName)
    end)
  end

  def transform_table(tab, fun, newAttrs, newRecName) do
    {:aborted, {:bad_type, tab, fun, newAttrs, newRecName}}
  end

  defp do_transform_table(:schema, _Fun, _NewAttrs, _NewRecName) do
    :mnesia.abort({:bad_type, :schema})
  end

  defp do_transform_table(tab, fun, newAttrs, newRecName) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    get_tid_ts_and_lock(tab, :write)

    insert_schema_ops(
      tidTs,
      make_transform(tab, fun, newAttrs, newRecName)
    )
  end

  defp make_transform(tab, fun, newAttrs, newRecName) do
    ensure_writable(:schema)
    cs = incr_version(val({tab, :cstruct}))
    ensure_active(cs)
    ensure_writable(tab)

    case r_cstruct(cs, :index) do
      [] ->
        cs2 =
          verify_cstruct(
            r_cstruct(cs,
              attributes: newAttrs,
              record_name: newRecName
            )
          )

        [{:op, :transform, fun, vsn_cs2list(cs2)}]

      posList ->
        delIdx = fn {pos, _}, ncs ->
          ix = r_cstruct(ncs, :index)
          ix2 = :lists.keydelete(pos, 1, ix)
          ncs1 = r_cstruct(ncs, index: ix2)
          op = {:op, :del_index, pos, vsn_cs2list(ncs1)}
          {op, ncs1}
        end

        addIdx = fn {_, _} = pos, ncs ->
          ix = r_cstruct(ncs, :index)
          ix2 = :lists.sort([pos | ix])
          ncs1 = r_cstruct(ncs, index: ix2)
          op = {:op, :add_index, pos, vsn_cs2list(ncs1)}
          {op, ncs1}
        end

        {delOps, cs1} = :lists.mapfoldl(delIdx, cs, posList)

        cs2 =
          r_cstruct(cs1,
            attributes: newAttrs,
            record_name: newRecName
          )

        {addOps, cs3} = :lists.mapfoldl(addIdx, cs2, posList)
        _ = verify_cstruct(cs3)
        :lists.flatten([delOps, {:op, :transform, fun, vsn_cs2list(cs2)}, addOps])
    end
  end

  defp index_pos({pos, _}) do
    pos
  end

  defp index_pos(pos) when is_integer(pos) do
    pos
  end

  defp index_pos({p} = pos) when is_atom(p) do
    pos
  end

  def change_table_access_mode(tab, mode) do
    schema_transaction(fn ->
      do_change_table_access_mode(tab, mode)
    end)
  end

  defp do_change_table_access_mode(tab, mode) do
    {_Mod, tid, ts} = get_tid_ts_and_lock(:schema, :write)
    store = r_tidstore(ts, :store)
    :mnesia_locker.wlock_no_exist(tid, store, :schema, val({:schema, :active_replicas}))
    :mnesia_locker.wlock_no_exist(tid, store, tab, val({tab, :active_replicas}))

    do_insert_schema_ops(
      store,
      make_change_table_access_mode(tab, mode)
    )
  end

  defp make_change_table_access_mode(tab, mode) do
    ensure_writable(:schema)
    cs = incr_version(val({tab, :cstruct}))
    ensure_active(cs)
    oldMode = r_cstruct(cs, :access_mode)
    verify(false, oldMode == mode, {:already_exists, tab, mode})
    cs2 = verify_cstruct(r_cstruct(cs, access_mode: mode))
    [{:op, :change_table_access_mode, vsn_cs2list(cs2), oldMode, mode}]
  end

  def change_table_load_order(tab, loadOrder) do
    schema_transaction(fn ->
      do_change_table_load_order(tab, loadOrder)
    end)
  end

  defp do_change_table_load_order(:schema, _LoadOrder) do
    :mnesia.abort({:bad_type, :schema})
  end

  defp do_change_table_load_order(tab, loadOrder) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    get_tid_ts_and_lock(tab, :none)

    insert_schema_ops(
      tidTs,
      make_change_table_load_order(tab, loadOrder)
    )
  end

  defp make_change_table_load_order(tab, loadOrder) do
    ensure_writable(:schema)
    cs = incr_version(val({tab, :cstruct}))
    ensure_active(cs)
    oldLoadOrder = r_cstruct(cs, :load_order)
    cs2 = verify_cstruct(r_cstruct(cs, load_order: loadOrder))
    [{:op, :change_table_load_order, vsn_cs2list(cs2), oldLoadOrder, loadOrder}]
  end

  def change_table_majority(tab, majority) when is_boolean(majority) do
    schema_transaction(fn ->
      do_change_table_majority(tab, majority)
    end)
  end

  defp do_change_table_majority(:schema, _Majority) do
    :mnesia.abort({:bad_type, :schema})
  end

  defp do_change_table_majority(tab, majority) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    get_tid_ts_and_lock(tab, :none)

    insert_schema_ops(
      tidTs,
      make_change_table_majority(tab, majority)
    )
  end

  defp make_change_table_majority(tab, majority) do
    ensure_writable(:schema)
    cs = incr_version(val({tab, :cstruct}))
    ensure_active(cs)
    oldMajority = r_cstruct(cs, :majority)
    cs2 = r_cstruct(cs, majority: majority)

    fragOps =
      case :lists.keyfind(:base_table, 1, r_cstruct(cs, :frag_properties)) do
        {_, ^tab} ->
          fragNames = :mnesia_frag.frag_names(tab) -- [tab]

          :lists.map(
            fn t ->
              get_tid_ts_and_lock(tab, :none)
              csT = incr_version(val({t, :cstruct}))
              ensure_active(csT)
              csT2 = r_cstruct(csT, majority: majority)
              verify_cstruct(csT2)
              {:op, :change_table_majority, vsn_cs2list(csT2), oldMajority, majority}
            end,
            fragNames
          )

        false ->
          []

        {_, _} ->
          :mnesia.abort({:bad_type, tab})
      end

    verify_cstruct(cs2)

    [
      {:op, :change_table_majority, vsn_cs2list(cs2), oldMajority, majority}
      | fragOps
    ]
  end

  def write_table_property(tab, prop)
      when is_tuple(prop) and
             :erlang.size(prop) >= 1 do
    schema_transaction(fn ->
      do_write_table_property(tab, prop)
    end)
  end

  def write_table_property(tab, prop) do
    {:aborted, {:bad_type, tab, prop}}
  end

  def do_write_table_property(tab, prop) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    {_, _, ts} = tidTs
    store = r_tidstore(ts, :store)

    case change_prop_in_existing_op(tab, prop, :write_property, store) do
      true ->
        dbg_out('change_prop_in_existing_op(~tp,~p,write_property,Store) -> true~n', [tab, prop])
        :ok

      false ->
        dbg_out('change_prop_in_existing_op(~tp,~p,write_property,Store) -> false~n', [tab, prop])
        get_tid_ts_and_lock(tab, :none)

        insert_schema_ops(
          tidTs,
          make_write_table_properties(tab, [prop])
        )
    end
  end

  defp make_write_table_properties(tab, props) do
    ensure_writable(:schema)
    cs = incr_version(val({tab, :cstruct}))
    ensure_active(cs)
    make_write_table_properties(tab, props, cs)
  end

  defp make_write_table_properties(tab, [prop | props], cs) do
    oldProps = r_cstruct(cs, :user_properties)
    propKey = :erlang.element(1, prop)
    delProps = :lists.keydelete(propKey, 1, oldProps)
    mergedProps = :lists.merge(delProps, [prop])
    cs2 = verify_cstruct(r_cstruct(cs, user_properties: mergedProps))

    [
      {:op, :write_property, vsn_cs2list(cs2), prop}
      | make_write_table_properties(tab, props, cs2)
    ]
  end

  defp make_write_table_properties(_Tab, [], _Cs) do
    []
  end

  defp change_prop_in_existing_op(tab, prop, how, store) do
    ops = :ets.match_object(store, :_)

    case update_existing_op(ops, tab, prop, how, []) do
      {true, ops1} ->
        :ets.match_delete(store, :_)

        for op <- ops1 do
          :ets.insert(store, op)
        end

        true

      false ->
        false
    end
  end

  defp update_existing_op(
         [
           {:op, op, l = [{:name, tab} | _], _OldProp}
           | ops
         ],
         tab,
         prop,
         how,
         acc
       )
       when op == :write_property or op == :delete_property do
    l1 = insert_prop(prop, l, how)
    newOp = {:op, how, l1, prop}
    {true, :lists.reverse(acc) ++ [newOp | ops]}
  end

  defp update_existing_op([op = {:op, :create_table, l} | ops], tab, prop, how, acc) do
    case :lists.keysearch(:name, 1, l) do
      {:value, {_, ^tab}} ->
        l1 = insert_prop(prop, l, how)
        {true, :lists.reverse(acc) ++ [{:op, :create_table, l1} | ops]}

      _ ->
        update_existing_op(ops, tab, prop, how, [op | acc])
    end
  end

  defp update_existing_op([op | ops], tab, prop, how, acc) do
    update_existing_op(ops, tab, prop, how, [op | acc])
  end

  defp update_existing_op([], _, _, _, _) do
    false
  end

  def do_read_table_property(tab, key) do
    tidTs = get_tid_ts_and_lock(:schema, :read)
    {_, _, ts} = tidTs
    store = r_tidstore(ts, :store)

    props =
      :ets.foldl(
        fn
          {:op, :announce_im_running, _, opts, _, _}, _Acc
          when tab == :schema ->
            find_props(opts)

          {:op, :create_table, [{:name, t} | opts]}, _Acc
          when t == tab ->
            find_props(opts)

          {:op, op, [{:name, t} | opts], _Prop}, _Acc
          when (t == tab and op == :write_property) or
                 (t == tab and op == :delete_property) ->
            find_props(opts)

          {:op, :delete_table, [{:name, t} | _]}, _Acc
          when t == tab ->
            []

          _Other, acc ->
            acc
        end,
        :undefined,
        store
      )

    case props do
      :undefined ->
        get_tid_ts_and_lock(tab, :read)
        dirty_read_table_property(tab, key)

      _ when is_list(props) ->
        case :lists.keyfind(key, 1, props) do
          false ->
            :undefined

          other ->
            other
        end
    end
  end

  defp dirty_read_table_property(tab, key) do
    try do
      :ets.lookup_element(:mnesia_gvar, {tab, :user_property, key}, 2)
    catch
      :error, _ ->
        :undefined
    end
  end

  defp insert_prop(prop, l, how) do
    prev = find_props(l)
    mergedProps = merge_with_previous(how, prop, prev)
    replace_props(l, mergedProps)
  end

  defp find_props([{:user_properties, p} | _]) do
    p
  end

  defp find_props([_H | t]) do
    find_props(t)
  end

  defp replace_props([{:user_properties, _} | t], p) do
    [{:user_properties, p} | t]
  end

  defp replace_props([h | t], p) do
    [h | replace_props(t, p)]
  end

  defp merge_with_previous(:write_property, prop, prev) do
    key = :erlang.element(1, prop)
    prev1 = :lists.keydelete(key, 1, prev)
    :lists.sort([prop | prev1])
  end

  defp merge_with_previous(:delete_property, propKey, prev) do
    :lists.keydelete(propKey, 1, prev)
  end

  def delete_table_property(tab, propKey) do
    schema_transaction(fn ->
      do_delete_table_property(tab, propKey)
    end)
  end

  def do_delete_table_property(tab, propKey) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    {_, _, ts} = tidTs
    store = r_tidstore(ts, :store)

    case change_prop_in_existing_op(tab, propKey, :delete_property, store) do
      true ->
        dbg_out('change_prop_in_existing_op(~tp,~p,delete_property,Store) -> true~n', [
          tab,
          propKey
        ])

        :ok

      false ->
        dbg_out('change_prop_in_existing_op(~tp,~p,delete_property,Store) -> false~n', [
          tab,
          propKey
        ])

        get_tid_ts_and_lock(tab, :none)

        insert_schema_ops(
          tidTs,
          make_delete_table_properties(tab, [propKey])
        )
    end
  end

  defp make_delete_table_properties(tab, propKeys) do
    ensure_writable(:schema)
    cs = incr_version(val({tab, :cstruct}))
    ensure_active(cs)
    make_delete_table_properties(tab, propKeys, cs)
  end

  defp make_delete_table_properties(tab, [propKey | propKeys], cs) do
    oldProps = r_cstruct(cs, :user_properties)
    props = :lists.keydelete(propKey, 1, oldProps)
    cs2 = verify_cstruct(r_cstruct(cs, user_properties: props))

    [
      {:op, :delete_property, vsn_cs2list(cs2), propKey}
      | make_delete_table_properties(tab, propKeys, cs2)
    ]
  end

  defp make_delete_table_properties(_Tab, [], _Cs) do
    []
  end

  def prepare_commit(tid, commit, waitFor) do
    case r_commit(commit, :schema_ops) do
      [] ->
        {false, commit, :optional}

      origOps ->
        {modified, ops, dumperMode} = prepare_ops(tid, origOps, waitFor, false, [], :optional)
        initBy = :schema_prepare
        goodRes = {modified, r_commit(commit, schema_ops: :lists.reverse(ops)), dumperMode}

        case dumperMode do
          :optional ->
            dbg_out('Transaction log dump skipped (~p): ~w~n', [dumperMode, initBy])

          :mandatory ->
            case :mnesia_controller.sync_dump_log(initBy) do
              :dumped ->
                goodRes

              {:error, reason} ->
                :mnesia.abort(reason)
            end
        end

        case ops do
          [] ->
            :ignore

          _ ->
            :mnesia_controller.wait_for_schema_commit_lock()
        end

        goodRes
    end
  end

  defp prepare_ops(tid, [op | ops], waitFor, changed, acc, dumperMode) do
    case prepare_op(tid, op, waitFor) do
      {true, :mandatory} ->
        prepare_ops(tid, ops, waitFor, changed, [op | acc], :mandatory)

      {true, :optional} ->
        prepare_ops(tid, ops, waitFor, changed, [op | acc], dumperMode)

      {true, ops2, :mandatory} ->
        prepare_ops(tid, ops, waitFor, true, ops2 ++ acc, :mandatory)

      {true, ops2, :optional} ->
        prepare_ops(tid, ops, waitFor, true, ops2 ++ acc, dumperMode)

      {false, :optional} ->
        prepare_ops(tid, ops, waitFor, true, acc, dumperMode)
    end
  end

  defp prepare_ops(_Tid, [], _WaitFor, changed, acc, dumperMode) do
    {changed, acc, dumperMode}
  end

  defp prepare_op(_Tid, {:op, :rec, :unknown, rec}, _WaitFor) do
    {{tab, key}, items, _Op} = rec

    case val({tab, :storage_type}) do
      :unknown ->
        {false, :optional}

      storage ->
        :mnesia_tm.prepare_snmp(tab, key, items)
        {true, [{:op, :rec, storage, rec}], :optional}
    end
  end

  defp prepare_op(
         _Tid,
         {:op, :announce_im_running, node, schemaDef, running, remoteRunning},
         _WaitFor
       ) do
    schemaCs = list2cs(schemaDef)

    cond do
      node == node() ->
        :ignore

      true ->
        current =
          :mnesia_lib.intersect(
            val({:current, :db_nodes}),
            [node() | val(:recover_nodes)]
          )

        newNodes = :mnesia_lib.uniq(running ++ remoteRunning) -- current

        :mnesia_lib.set(
          :prepare_op,
          {:announce_im_running, newNodes}
        )

        announce_im_running(newNodes, schemaCs)
    end

    {false, :optional}
  end

  defp prepare_op(_Tid, {:op, :sync_trans}, {:part, coordPid}) do
    send(coordPid, {:sync_trans, self()})

    receive do
      {:sync_trans, ^coordPid} ->
        {false, :optional}

      {:mnesia_down, _Node} = else__ ->
        :mnesia_lib.verbose('sync_op terminated due to ~tp~n', [else__])
        :mnesia.abort(else__)

      {:EXIT, _, _} = else__ ->
        :mnesia_lib.verbose('sync_op terminated due to ~tp~n', [else__])
        :mnesia.abort(else__)
    end
  end

  defp prepare_op(_Tid, {:op, :sync_trans}, {:coord, nodes}) do
    case receive_sync(nodes, []) do
      {:abort, reason} ->
        :mnesia_lib.verbose('sync_op terminated due to ~tp~n', [reason])
        :mnesia.abort(reason)

      pids ->
        for pid <- pids do
          send(pid, {:sync_trans, self()})
        end

        {false, :optional}
    end
  end

  defp prepare_op(tid, {:op, :create_table, tabDef}, _WaitFor) do
    cs = list2cs(tabDef)
    storage = :mnesia_lib.cs_to_storage_type(node(), cs)
    useDir = :mnesia_monitor.use_dir()
    tab = r_cstruct(cs, :name)

    case storage do
      :disc_copies when useDir == false ->
        useDirReason = {:bad_type, tab, storage, node()}
        :mnesia.abort(useDirReason)

      :disc_only_copies when useDir == false ->
        useDirReason = {:bad_type, tab, storage, node()}
        :mnesia.abort(useDirReason)

      :ram_copies ->
        :mnesia_lib.set({tab, :create_table}, true)
        create_ram_table(tab, cs)
        insert_cstruct(tid, cs, false)
        {true, :optional}

      :disc_copies ->
        :mnesia_lib.set({tab, :create_table}, true)
        create_ram_table(tab, cs)
        create_disc_table(tab)
        insert_cstruct(tid, cs, false)
        {true, :optional}

      :disc_only_copies ->
        :mnesia_lib.set({tab, :create_table}, true)
        create_disc_only_table(tab, cs)
        insert_cstruct(tid, cs, false)
        {true, :optional}

      {:ext, alias, mod} ->
        :mnesia_lib.set({tab, :create_table}, true)
        create_external_table(alias, tab, mod, cs)
        insert_cstruct(tid, cs, false)
        {true, :optional}

      :unknown ->
        :mnesia_lib.set({tab, :create_table}, true)
        insert_cstruct(tid, cs, false)
        {true, :optional}
    end
  end

  defp prepare_op(
         tid,
         {:op, :add_table_copy, storage, node, tabDef},
         _WaitFor
       ) do
    cs = list2cs(tabDef)
    tab = r_cstruct(cs, :name)

    cond do
      tab == :schema ->
        {true, :optional}

      node == node() ->
        case :mnesia_lib.val({:schema, :storage_type}) do
          :ram_copies when storage != :ram_copies ->
            error = {:combine_error, tab, 'has no disc', node}
            :mnesia.abort(error)

          _ ->
            :ok
        end

        insert_cstruct(tid, cs, true)

        case :mnesia_controller.get_network_copy(tid, tab, cs) do
          {:loaded, :ok} ->
            {true, :optional}

          {:not_loaded, errReason} ->
            reason = {:system_limit, tab, {node, errReason}}
            :mnesia.abort(reason)
        end

      node != node() ->
        cond do
          storage != :ram_copies ->
            case :mnesia_lib.schema_cs_to_storage_type(
                   node(),
                   cs
                 ) do
              :ram_copies ->
                dat = :mnesia_lib.tab2dcd(tab)

                case :mnesia_lib.exists(dat) do
                  true ->
                    :mnesia.abort({:combine_error, tab, storage, 'Table dumped to disc', node()})

                  false ->
                    :ok
                end

              _ ->
                :ok
            end

          true ->
            :ok
        end

        insert_cstruct(tid, cs, true)
        {true, :optional}
    end
  end

  defp prepare_op(
         _Tid,
         {:op, :del_table_copy, _Storage, node, tabDef},
         _WaitFor
       ) do
    cs = list2cs(tabDef)
    tab = r_cstruct(cs, :name)
    set_where_to_read(tab, node, cs)
    {true, :optional}
  end

  defp prepare_op(
         _Tid,
         {:op, :change_table_copy_type, n, fromS, toS, tabDef},
         _WaitFor
       )
       when n == node() do
    cs = list2cs(tabDef)
    tab = r_cstruct(cs, :name)
    notActive = :mnesia_lib.not_active_here(tab)

    cond do
      tab !== :schema ->
        check_if_disc_required(fromS, toS)

      true ->
        :ok
    end

    cond do
      notActive == true ->
        :mnesia.abort({:not_active, tab, node()})

      tab == :schema ->
        case {fromS, toS} do
          {:ram_copies, :disc_copies} ->
            case :mnesia.system_info(:schema_location) do
              :opt_disc ->
                :ignore

              _ ->
                :mnesia.abort({:combine_error, tab, node(), 'schema_location must be opt_disc'})
            end

            dir = :mnesia_lib.dir()

            case opt_create_dir(true, dir) do
              :ok ->
                purge_dir(dir, [])
                :mnesia_log.purge_all_logs()
                set(:use_dir, true)
                :mnesia_log.init()
                ns = val({:current, :db_nodes})

                f = fn u ->
                  :mnesia_recover.log_mnesia_up(u)
                end

                :lists.foreach(f, ns)
                :mnesia_dumper.raw_named_dump_table(tab, :dmp)
                :mnesia_checkpoint.tm_change_table_copy_type(tab, fromS, toS)

              {:error, reason} ->
                :mnesia.abort(reason)
            end

          {:disc_copies, :ram_copies} ->
            ltabs = val({:schema, :local_tables}) -- [:schema]

            dtabs =
              for l <- ltabs,
                  val({l, :storage_type}) != :ram_copies do
                l
              end

            verify([], dtabs, {'Disc resident tables', dtabs, n})

          _ ->
            :mnesia.abort({:combine_error, tab, toS})
        end

      :erlang.element(1, fromS) == :ext or
          :erlang.element(1, toS) == :ext ->
        cond do
          toS == :ram_copies ->
            create_ram_table(tab, cs)

          true ->
            :ok
        end

        :mnesia_dumper.dump_to_logfile(fromS, tab)
        :mnesia_checkpoint.tm_change_table_copy_type(tab, fromS, toS)

      fromS == :ram_copies ->
        case :mnesia_monitor.use_dir() do
          true ->
            dat = :mnesia_lib.tab2dcd(tab)

            case :mnesia_lib.exists(dat) do
              true ->
                :mnesia.abort({:combine_error, tab, node(), 'Table dump exists'})

              false ->
                case toS do
                  :disc_copies ->
                    :mnesia_log.ets2dcd(tab, :dmp)

                  :disc_only_copies ->
                    :mnesia_dumper.raw_named_dump_table(tab, :dmp)
                end

                :mnesia_checkpoint.tm_change_table_copy_type(tab, fromS, toS)
            end

          false ->
            :mnesia.abort({:has_no_disc, node()})
        end

      fromS == :disc_copies and toS == :disc_only_copies ->
        :mnesia_dumper.raw_named_dump_table(tab, :dmp)

      fromS == :disc_only_copies ->
        type = r_cstruct(cs, :type)
        create_ram_table(tab, cs)
        datname = :mnesia_lib.tab2dat(tab)
        repair = :mnesia_monitor.get_env(:auto_repair)

        case :mnesia_lib.dets_to_ets(tab, tab, datname, type, repair, :no) do
          :loaded ->
            :ok

          reason ->
            err = 'Failed to copy disc data to ram'
            :mnesia.abort({:system_limit, tab, {err, reason}})
        end

      true ->
        :ignore
    end

    {true, :mandatory}
  end

  defp prepare_op(
         _Tid,
         {:op, :change_table_copy_type, n, _FromS, _ToS, _TabDef},
         _WaitFor
       )
       when n != node() do
    {true, :mandatory}
  end

  defp prepare_op(_Tid, {:op, :delete_table, _TabDef}, _WaitFor) do
    {true, :mandatory}
  end

  defp prepare_op(_Tid, {:op, :dump_table, :unknown, tabDef}, _WaitFor) do
    cs = list2cs(tabDef)
    tab = r_cstruct(cs, :name)

    case :lists.member(node(), r_cstruct(cs, :ram_copies)) do
      true ->
        case :mnesia_monitor.use_dir() do
          true ->
            :mnesia_log.ets2dcd(tab, :dmp)
            size = :mnesia.table_info(tab, :size)
            {true, [{:op, :dump_table, size, tabDef}], :optional}

          false ->
            :mnesia.abort({:has_no_disc, node()})
        end

      false ->
        {false, :optional}
    end
  end

  defp prepare_op(_Tid, {:op, :add_snmp, ustruct, tabDef}, _WaitFor) do
    cs = list2cs(tabDef)

    case :mnesia_lib.cs_to_storage_type(node(), cs) do
      :unknown ->
        {true, :optional}

      storage ->
        tab = r_cstruct(cs, :name)
        stab = :mnesia_snmp_hook.create_table(ustruct, tab, storage)
        :mnesia_lib.set({tab, {:index, :snmp}}, stab)
        {true, :optional}
    end
  end

  defp prepare_op(_Tid, {:op, :transform, :ignore, _TabDef}, _WaitFor) do
    {true, :mandatory}
  end

  defp prepare_op(_Tid, {:op, :transform, fun, tabDef}, _WaitFor) do
    cs = list2cs(tabDef)

    case :mnesia_lib.cs_to_storage_type(node(), cs) do
      :unknown ->
        {true, :mandatory}

      storage ->
        tab = r_cstruct(cs, :name)
        recName = r_cstruct(cs, :record_name)
        type = r_cstruct(cs, :type)
        newArity = length(r_cstruct(cs, :attributes)) + 1
        :mnesia_lib.db_fixtable(storage, tab, true)
        key = :mnesia_lib.db_first(tab)
        op = {:op, :transform, fun, tabDef}

        try do
          transform_objs(fun, tab, recName, key, newArity, storage, type, [op])
        catch
          _, reason ->
            :mnesia_lib.db_fixtable(storage, tab, false)

            :mnesia_lib.important('Transform function failed: \'~tp\' in \'~tp\'', [
              reason,
              __STACKTRACE__
            ])

            exit({'Bad transform function', tab, fun, node(), reason})
        else
          objs ->
            :mnesia_lib.db_fixtable(storage, tab, false)
            {true, objs, :mandatory}
        end
    end
  end

  defp prepare_op(_Tid, {:op, :merge_schema, tabDef}, _WaitFor) do
    cs = list2cs(tabDef)

    case verify_merge(cs) do
      :ok ->
        {true, :optional}

      error ->
        verbose('Merge_Schema ~p failed on ~p: ~tp~n', [_Tid, node(), error])
        :mnesia.abort({:bad_commit, error})
    end
  end

  defp prepare_op(_Tid, _Op, _WaitFor) do
    {true, :optional}
  end

  defp check_if_disc_required(fromS, toS) do
    fromSem = :mnesia_lib.semantics(fromS, :storage)
    toSem = :mnesia_lib.semantics(toS, :storage)

    case {fromSem, toSem} do
      {:ram_copies, _}
      when toSem == :disc_copies or
             toSem == :disc_only_copies ->
        case :mnesia_monitor.use_dir() do
          true ->
            :ok

          false ->
            :mnesia.abort({:has_no_disc, node()})
        end

      _ ->
        :ok
    end
  end

  defp create_ram_table(
         tab,
         r_cstruct(type: type, storage_properties: props)
       ) do
    etsOpts = :proplists.get_value(:ets, props, [])

    args = [
      {:keypos, 2},
      :public,
      :named_table,
      type
      | etsOpts
    ]

    case :mnesia_monitor.unsafe_mktab(tab, args) do
      ^tab ->
        :ok

      {:error, reason} ->
        err = 'Failed to create ets table'
        :mnesia.abort({:system_limit, tab, {err, reason}})
    end
  end

  defp create_disc_table(tab) do
    file = :mnesia_lib.tab2dcd(tab)
    :file.delete(file)
    fArg = [{:file, file}, {:name, {:mnesia, :create}}, {:repair, false}, {:mode, :read_write}]

    case :mnesia_monitor.open_log(fArg) do
      {:ok, log} ->
        :mnesia_monitor.unsafe_close_log(log)
        :ok

      {:error, reason} ->
        err = 'Failed to create disc table'
        :mnesia.abort({:system_limit, tab, {err, reason}})
    end
  end

  defp create_disc_only_table(
         tab,
         r_cstruct(type: type, storage_properties: props)
       ) do
    file = :mnesia_lib.tab2dat(tab)
    :file.delete(file)
    detsOpts = :proplists.get_value(:dets, props, [])

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

    case :mnesia_monitor.unsafe_open_dets(tab, args) do
      {:ok, _} ->
        :ok

      {:error, reason} ->
        err = 'Failed to create disc table'
        :mnesia.abort({:system_limit, tab, {err, reason}})
    end
  end

  defp create_external_table(alias, tab, mod, cs) do
    case :mnesia_monitor.unsafe_create_external(tab, alias, mod, cs) do
      :ok ->
        :ok

      {:error, reason} ->
        err = 'Failed to create external table'
        :mnesia.abort({:system_limit, tab, {err, reason}})
    end
  end

  defp receive_sync([], pids) do
    pids
  end

  defp receive_sync(nodes, pids) do
    receive do
      {:sync_trans, pid} ->
        node = node(pid)
        receive_sync(:lists.delete(node, nodes), [pid | pids])

      else__ ->
        {:abort, else__}
    end
  end

  def set_where_to_read(tab, node, cs) do
    case :mnesia_lib.val({tab, :where_to_read}) do
      ^node ->
        case r_cstruct(cs, :local_content) do
          true ->
            :ok

          false ->
            :mnesia_lib.set_remote_where_to_read(tab, [node])
            :ok
        end

      _ ->
        :ok
    end
  end

  defp transform_objs(_Fun, _Tab, _RT, :"$end_of_table", _NewArity, _Storage, _Type, acc) do
    acc
  end

  defp transform_objs(fun, tab, recName, key, a, storage, type, acc) do
    objs = :mnesia_lib.db_get(tab, key)
    nextKey = :mnesia_lib.db_next_key(tab, key)
    oid = {tab, key}
    newObjs = {ws, ds} = transform_obj(tab, recName, key, fun, objs, a, type, [], [])

    cond do
      newObjs == {[], []} ->
        transform_objs(fun, tab, recName, nextKey, a, storage, type, acc)

      type == :bag ->
        transform_objs(fun, tab, recName, nextKey, a, storage, type, [
          {:op, :rec, storage, {oid, ws, :write}},
          {:op, :rec, storage, {oid, [oid], :delete}}
          | acc
        ])

      ds == [] ->
        transform_objs(fun, tab, recName, nextKey, a, storage, type, [
          {:op, :rec, storage, {oid, ws, :write}} | acc
        ])

      ws == [] ->
        transform_objs(fun, tab, recName, nextKey, a, storage, type, [
          {:op, :rec, storage, {oid, ds, :write}} | acc
        ])

      true ->
        transform_objs(fun, tab, recName, nextKey, a, storage, type, [
          {:op, :rec, storage, {oid, ws, :write}},
          {:op, :rec, storage, {oid, ds, :delete}}
          | acc
        ])
    end
  end

  defp transform_obj(tab, recName, key, fun, [obj | rest], newArity, type, ws, ds) do
    newObj = fun.(obj)

    cond do
      :erlang.size(newObj) != newArity ->
        exit({'Bad arity', obj, newObj})

      newObj == obj ->
        transform_obj(tab, recName, key, fun, rest, newArity, type, ws, ds)

      recName == :erlang.element(1, newObj) and
          key == :erlang.element(2, newObj) ->
        transform_obj(tab, recName, key, fun, rest, newArity, type, [newObj | ws], ds)

      newObj == :delete ->
        case type do
          :bag ->
            transform_obj(tab, recName, key, fun, rest, newArity, type, ws, ds)

          _ ->
            transform_obj(tab, recName, key, fun, rest, newArity, type, ws, [newObj | ds])
        end

      true ->
        exit({'Bad key or Record Name', obj, newObj})
    end
  end

  defp transform_obj(_Tab, _RecName, _Key, _Fun, [], _NewArity, _Type, ws, ds) do
    {:lists.reverse(ws), :lists.reverse(ds)}
  end

  def undo_prepare_commit(tid, commit) do
    case r_commit(commit, :schema_ops) do
      [] ->
        :ignore

      ops ->
        try do
          :mnesia_controller.release_schema_commit_lock()
        catch
          :error, _ ->
            :ok
        end

        undo_prepare_ops(tid, ops)
    end

    commit
  end

  defp undo_prepare_ops(tid, [op | ops]) do
    case :erlang.element(1, op) do
      theOp when theOp != :op and theOp != :restore_op ->
        undo_prepare_ops(tid, ops)

      _ ->
        undo_prepare_ops(tid, ops)
        undo_prepare_op(tid, op)
    end
  end

  defp undo_prepare_ops(_Tid, []) do
    []
  end

  defp undo_prepare_op(
         _Tid,
         {:op, :announce_im_running, _Node, _, _Running, _RemoteRunning}
       ) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, :prepare_op, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:announce_im_running, new} ->
        unannounce_im_running(new)

      _Else ->
        :ok
    end
  end

  defp undo_prepare_op(_Tid, {:op, :sync_trans}) do
    :ok
  end

  defp undo_prepare_op(tid, {:op, :create_table, tabDef}) do
    cs = list2cs(tabDef)
    tab = r_cstruct(cs, :name)
    :mnesia_lib.unset({tab, :create_table})
    delete_cstruct(tid, cs)

    case :mnesia_lib.cs_to_storage_type(node(), cs) do
      :unknown ->
        :ok

      :ram_copies ->
        ram_delete_table(tab, :ram_copies)

      :disc_copies ->
        ram_delete_table(tab, :disc_copies)
        dcdFile = :mnesia_lib.tab2dcd(tab)
        :file.delete(dcdFile)

      :disc_only_copies ->
        :mnesia_monitor.unsafe_close_dets(tab)
        dat = :mnesia_lib.tab2dat(tab)
        :file.delete(dat)

      {:ext, alias, mod} ->
        mod.close_table(alias, tab)
        mod.delete_table(alias, tab)
    end
  end

  defp undo_prepare_op(
         tid,
         {:op, :add_table_copy, storage, node, tabDef}
       ) do
    cs = list2cs(tabDef)
    tab = r_cstruct(cs, :name)

    cond do
      tab == :schema ->
        true

      node == node() ->
        :mnesia_checkpoint.tm_del_copy(tab, node)
        :mnesia_controller.unannounce_add_table_copy(tab, node)

        cond do
          storage == :disc_only_copies or tab == :schema ->
            :mnesia_monitor.close_dets(tab)
            :file.delete(:mnesia_lib.tab2dat(tab))

          true ->
            :file.delete(:mnesia_lib.tab2dcd(tab))
        end

        ram_delete_table(tab, storage)
        cs2 = new_cs(cs, node, storage, :del)
        insert_cstruct(tid, cs2, true)

      node != node() ->
        :mnesia_controller.unannounce_add_table_copy(tab, node)
        cs2 = new_cs(cs, node, storage, :del)
        insert_cstruct(tid, cs2, true)
    end
  end

  defp undo_prepare_op(
         _Tid,
         {:op, :del_table_copy, _, node, tabDef}
       ) do
    cs = list2cs(tabDef)
    tab = r_cstruct(cs, :name)

    cond do
      node() === node ->
        :mnesia_lib.set({tab, :where_to_read}, node)

      true ->
        case :mnesia_lib.val({tab, :where_to_read}) do
          :nowhere ->
            :mnesia_lib.set_remote_where_to_read(tab)

          _ ->
            :ignore
        end
    end
  end

  defp undo_prepare_op(
         _Tid,
         {:op, :change_table_copy_type, n, fromS, toS, tabDef}
       )
       when n == node() do
    cs = list2cs(tabDef)
    tab = r_cstruct(cs, :name)
    :mnesia_checkpoint.tm_change_table_copy_type(tab, toS, fromS)
    dmp = :mnesia_lib.tab2dmp(tab)

    case {fromS, toS} do
      {:ram_copies, :disc_copies} when tab == :schema ->
        :file.delete(dmp)
        :mnesia_log.purge_some_logs()
        set(:use_dir, false)

      {:ram_copies, :disc_copies} ->
        :file.delete(dmp)

      {:ram_copies, :disc_only_copies} ->
        :file.delete(dmp)

      {:disc_only_copies, _} ->
        ram_delete_table(tab, :ram_copies)

      _ ->
        :ignore
    end
  end

  defp undo_prepare_op(_Tid, {:op, :dump_table, _Size, tabDef}) do
    cs = list2cs(tabDef)

    case :lists.member(node(), r_cstruct(cs, :ram_copies)) do
      true ->
        tab = r_cstruct(cs, :name)
        dmp = :mnesia_lib.tab2dmp(tab)
        :file.delete(dmp)

      false ->
        :ignore
    end
  end

  defp undo_prepare_op(_Tid, {:op, :add_snmp, _Ustruct, tabDef}) do
    cs = list2cs(tabDef)

    case :mnesia_lib.cs_to_storage_type(node(), cs) do
      :unknown ->
        true

      _Storage ->
        tab = r_cstruct(cs, :name)

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
    end
  end

  defp undo_prepare_op(_Tid, _Op) do
    :ignore
  end

  def ram_delete_table(tab, storage) do
    case storage do
      :unknown ->
        :ignore

      {:ext, _, _} ->
        :ignore

      :disc_only_copies ->
        :ignore

      _Else ->
        try do
          :mnesia_index.del_transient(tab, storage)
        catch
          _, _Reason ->
            {:EXIT, _Reason}
        end

        case (try do
                :ets.lookup_element(:mnesia_gvar, {tab, {:index, :snmp}}, 2)
              catch
                :error, _ ->
                  {:EXIT, {:badarg, []}}
              end) do
          {:EXIT, _} ->
            :ignore

          etab ->
            try do
              :mnesia_snmp_hook.delete_table(tab, etab)
            catch
              :error, _ ->
                :ok
            end
        end

        try do
          :ets.delete(tab)
        catch
          :error, _ ->
            :ok
        end
    end
  end

  def purge_dir(dir, keepFiles) do
    suffixes = known_suffixes()
    purge_dir(dir, keepFiles, suffixes)
  end

  defp purge_dir(dir, keepFiles, suffixes) do
    case dir_exists(dir) do
      true ->
        {:ok, allFiles} = :file.list_dir(dir)
        purge_known_files(allFiles, keepFiles, dir, suffixes)

      false ->
        :ok
    end
  end

  def purge_tmp_files() do
    case :mnesia_monitor.use_dir() do
      true ->
        dir = :mnesia_lib.dir()
        keepFiles = []
        exists = :mnesia_lib.exists(:mnesia_lib.tab2dat(:schema))

        case exists do
          true ->
            suffixes = tmp_suffixes()
            purge_dir(dir, keepFiles, suffixes)

          false ->
            suffixes = known_suffixes()
            purge_dir(dir, keepFiles, suffixes)
            :mnesia_lib.set(:use_dir, false)
        end

      false ->
        :ok
    end
  end

  defp purge_known_files([file | tail], keepFiles, dir, suffixes) do
    case :lists.member(file, keepFiles) do
      true ->
        :ignore

      false ->
        case has_known_suffix(file, suffixes, false) do
          false ->
            :ignore

          true ->
            absFile = :filename.join([dir, file])
            delete_recursive(absFile)
        end
    end

    purge_known_files(tail, keepFiles, dir, suffixes)
  end

  defp purge_known_files([], _KeepFiles, _Dir, _Suffixes) do
    :ok
  end

  defp delete_recursive(path) do
    case :filelib.is_dir(path) do
      true ->
        {:ok, names} = :file.list_dir(path)

        :lists.foreach(
          fn name ->
            delete_recursive(:filename.join(path, name))
          end,
          names
        )

        :file.del_dir(path)

      false ->
        :file.delete(path)
    end
  end

  defp has_known_suffix(_File, _Suffixes, true) do
    true
  end

  defp has_known_suffix(file, [suffix | tail], false) do
    has_known_suffix(file, tail, :lists.suffix(suffix, file))
  end

  defp has_known_suffix(_File, [], bool) do
    bool
  end

  defp known_suffixes() do
    known_suffixes(get_ext_types_disc())
  end

  defp known_suffixes(ext) do
    real_suffixes(ext) ++ tmp_suffixes(ext)
  end

  defp real_suffixes(ext) do
    ['.DAT', '.LOG', '.BUP', '.DCL', '.DCD'] ++ ext_real_suffixes(ext)
  end

  defp tmp_suffixes() do
    tmp_suffixes(get_ext_types_disc())
  end

  defp tmp_suffixes(ext) do
    ['.TMP', '.BUPTMP', '.RET', '.DMP', '.'] ++ ext_tmp_suffixes(ext)
  end

  defp ext_real_suffixes(ext) do
    try do
      :lists.foldl(
        fn mod, acc ->
          acc ++ mod.real_suffixes()
        end,
        [],
        for {_, m} <- ext do
          m
        end
      )
    catch
      :error, e ->
        verbose('Cant find real ext suffixes (~tp)~n', [e])
        []
    end
  end

  defp ext_tmp_suffixes(ext) do
    try do
      :lists.foldl(
        fn mod, acc ->
          acc ++ mod.tmp_suffixes()
        end,
        [],
        for {_, m} <- ext do
          m
        end
      )
    catch
      :error, e ->
        verbose('Cant find tmp ext suffixes (~tp)~n', [e])
        []
    end
  end

  def info() do
    tabs = :lists.sort(val({:schema, :tables}))

    :lists.foreach(
      fn t ->
        info(t)
      end,
      tabs
    )

    :ok
  end

  def info(tab) do
    props = get_table_properties(tab)
    :io.format('-- Properties for ~tw table --- ~n', [tab])
    info2(tab, props)
  end

  defp info2(tab, [{:cstruct, _V} | tail]) do
    info2(tab, tail)
  end

  defp info2(tab, [{:frag_hash, _V} | tail]) do
    info2(tab, tail)
  end

  defp info2(tab, [{p, v} | tail]) do
    :io.format('~-20tw -> ~tp~n', [p, v])
    info2(tab, tail)
  end

  defp info2(_, []) do
    :io.format('~n', [])
  end

  def get_table_properties(tab) do
    try do
      :mnesia_lib.db_match_object(:ram_copies, :mnesia_gvar, {{tab, :_}, :_})
    catch
      :error, _ ->
        :mnesia.abort({:no_exists, tab, :all})
    else
      rawGvar ->
        case (for {{_Tab, item}, val} <- rawGvar do
                {item, val}
              end) do
          [] ->
            []

          gvar ->
            size = {:size, :mnesia.table_info(tab, :size)}
            memory = {:memory, :mnesia.table_info(tab, :memory)}
            master = {:master_nodes, :mnesia.table_info(tab, :master_nodes)}
            :lists.sort([size, memory, master | gvar])
        end
    end
  end

  Record.defrecord(:r_r, :r,
    iter: :schema,
    module: :undefined,
    table_options: [],
    default_op: :clear_tables,
    tables: [],
    opaque: :undefined,
    insert_op: :error_fun,
    recs: :error_recs
  )

  def restore(opaque) do
    restore(opaque, [], :mnesia_monitor.get_env(:backup_module))
  end

  def restore(opaque, args) when is_list(args) do
    restore(opaque, args, :mnesia_monitor.get_env(:backup_module))
  end

  def restore(_Opaque, badArg) do
    {:aborted, {:badarg, badArg}}
  end

  def restore(opaque, args, module)
      when is_list(args) and
             is_atom(module) do
    initR = r_r(opaque: opaque, module: module)

    try do
      :lists.foldl(&check_restore_arg/2, initR, args)
    catch
      :exit, reason ->
        {:aborted, reason}
    else
      r when elem(r, 0) === :r ->
        case :mnesia_bup.read_schema(r_r(r, :module), opaque) do
          {:error, reason} ->
            {:aborted, reason}

          bupSchema ->
            schema_transaction(fn ->
              do_restore(r, bupSchema)
            end)
        end
    end
  end

  def restore(_Opaque, args, module) do
    {:aborted, {:badarg, args, module}}
  end

  defp check_restore_arg({:module, mod}, r) when is_atom(mod) do
    r_r(r, module: mod)
  end

  defp check_restore_arg({:clear_tables, list}, r) when is_list(list) do
    case :lists.member(:schema, list) do
      false ->
        tableList =
          for tab <- list do
            {tab, :clear_tables}
          end

        r_r(r, table_options: r_r(r, :table_options) ++ tableList)

      true ->
        exit({:badarg, {:clear_tables, :schema}})
    end
  end

  defp check_restore_arg({:recreate_tables, list}, r)
       when is_list(list) do
    case :lists.member(:schema, list) do
      false ->
        tableList =
          for tab <- list do
            {tab, :recreate_tables}
          end

        r_r(r, table_options: r_r(r, :table_options) ++ tableList)

      true ->
        exit({:badarg, {:recreate_tables, :schema}})
    end
  end

  defp check_restore_arg({:keep_tables, list}, r) when is_list(list) do
    tableList =
      for tab <- list do
        {tab, :keep_tables}
      end

    r_r(r, table_options: r_r(r, :table_options) ++ tableList)
  end

  defp check_restore_arg({:skip_tables, list}, r) when is_list(list) do
    tableList =
      for tab <- list do
        {tab, :skip_tables}
      end

    r_r(r, table_options: r_r(r, :table_options) ++ tableList)
  end

  defp check_restore_arg({:default_op, op}, r) do
    case op do
      :clear_tables ->
        :ok

      :recreate_tables ->
        :ok

      :keep_tables ->
        :ok

      :skip_tables ->
        :ok

      else__ ->
        exit({:badarg, {:bad_default_op, else__}})
    end

    r_r(r, default_op: op)
  end

  defp check_restore_arg(badArg, _) do
    exit({:badarg, badArg})
  end

  defp do_restore(r, bupSchema) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    r2 = restore_schema(bupSchema, r)
    insert_schema_ops(tidTs, [{:restore_op, r2}])

    for tabStruct <- r_r(r2, :tables) do
      :erlang.element(1, tabStruct)
    end
  end

  def arrange_restore(r, fun, recs) do
    r2 = r_r(r, insert_op: fun, recs: recs)

    case :mnesia_bup.iterate(r_r(r, :module), &restore_items/5, r_r(r, :opaque), r2) do
      {:ok, r3} ->
        r_r(r3, :recs)

      {:error, reason} ->
        :mnesia.abort(reason)
    end
  end

  defp restore_items([rec | recs], header, schema, ext, r) do
    tab = :erlang.element(1, rec)

    case :lists.keysearch(tab, 1, r_r(r, :tables)) do
      {:value, {^tab, where0, snmp, recName}} ->
        where =
          case where0 do
            :undefined ->
              val({tab, :where_to_commit})

            _ ->
              where0
          end

        {rest, nRecs} =
          restore_tab_items(
            [rec | recs],
            tab,
            recName,
            where,
            snmp,
            r_r(r, :recs),
            r_r(r, :insert_op)
          )

        restore_items(rest, header, schema, ext, r_r(r, recs: nRecs))

      false ->
        rest = skip_tab_items(recs, tab)
        restore_items(rest, header, schema, ext, r)
    end
  end

  defp restore_items([], _Header, _Schema, _Ext, r) do
    r
  end

  defp restore_func(tab, r) do
    case :lists.keysearch(tab, 1, r_r(r, :table_options)) do
      {:value, {^tab, oP}} ->
        oP

      false ->
        r_r(r, :default_op)
    end
  end

  defp where_to_commit(tab, csList) do
    ram =
      for n <- pick(tab, :ram_copies, csList, []) do
        {n, :ram_copies}
      end

    disc =
      for n <- pick(tab, :disc_copies, csList, []) do
        {n, :disc_copies}
      end

    discO =
      for n <- pick(tab, :disc_only_copies, csList, []) do
        {n, :disc_only_copies}
      end

    extNodes =
      for {alias, mod} <- get_ext_types() do
        {alias, mod, pick(tab, alias, csList, [])}
      end

    ext =
      :lists.foldl(
        fn {alias, mod, ns}, acc ->
          for n <- ns do
            {n, {:ext, alias, mod}}
          end ++ acc
        end,
        [],
        extNodes
      )

    ram ++ disc ++ discO ++ ext
  end

  defp restore_schema([{:schema, :schema, _List} | schema], r) do
    restore_schema(schema, r)
  end

  defp restore_schema([{:schema, tab, list} | schema], r) do
    case restore_func(tab, r) do
      :clear_tables ->
        do_clear_table(tab)
        snmp = val({tab, :snmp})
        recName = val({tab, :record_name})

        r2 =
          r_r(r,
            tables: [
              {tab, :undefined, snmp, recName}
              | r_r(r, :tables)
            ]
          )

        restore_schema(schema, r2)

      :recreate_tables ->
        tidTs =
          case (try do
                  :ets.lookup_element(:mnesia_gvar, {tab, :cstruct}, 2)
                catch
                  :error, _ ->
                    {:EXIT, {:badarg, []}}
                end) do
            {:EXIT, _} ->
              tTs = {_Mod, tid, ts} = :erlang.get(:mnesia_activity_state)
              runningNodes = val({:current, :db_nodes})

              nodes =
                :mnesia_lib.intersect(
                  :mnesia_lib.cs_to_nodes(list2cs(list)),
                  runningNodes
                )

              :mnesia_locker.wlock_no_exist(tid, r_tidstore(ts, :store), tab, nodes)
              tTs

            _ ->
              get_tid_ts_and_lock(tab, :write)
          end

        nC =
          {:cookie,
           {{:erlang.monotonic_time() + :erlang.time_offset(), :erlang.unique_integer(), 1},
            node()}}

        list2 = :lists.keyreplace(:cookie, 1, list, nC)
        where = where_to_commit(tab, list2)
        snmp = pick(tab, :snmp, list2, [])
        recName = pick(tab, :record_name, list2, tab)

        insert_schema_ops(
          tidTs,
          [{:op, :restore_recreate, list2}]
        )

        r2 =
          r_r(r,
            tables: [
              {tab, where, snmp, recName}
              | r_r(r, :tables)
            ]
          )

        restore_schema(schema, r2)

      :keep_tables ->
        get_tid_ts_and_lock(tab, :write)
        snmp = val({tab, :snmp})
        recName = val({tab, :record_name})

        r2 =
          r_r(r,
            tables: [
              {tab, :undefined, snmp, recName}
              | r_r(r, :tables)
            ]
          )

        restore_schema(schema, r2)

      :skip_tables ->
        restore_schema(schema, r)
    end
  end

  defp restore_schema([{:schema, tab} | schema], r) do
    do_delete_table(tab)
    tabs = :lists.delete(tab, r_r(r, :tables))
    restore_schema(schema, r_r(r, tables: tabs))
  end

  defp restore_schema([], r) do
    r
  end

  defp restore_tab_items([rec | rest], tab, recName, where, snmp, recs, op)
       when :erlang.element(1, rec) == tab do
    newRecs = op.(rec, recs, recName, where, snmp)
    restore_tab_items(rest, tab, recName, where, snmp, newRecs, op)
  end

  defp restore_tab_items(rest, _Tab, _RecName, _Where, _Snmp, recs, _Op) do
    {rest, recs}
  end

  defp skip_tab_items([rec | rest], tab)
       when :erlang.element(
              1,
              rec
            ) == tab do
    skip_tab_items(rest, tab)
  end

  defp skip_tab_items(recs, _) do
    recs
  end

  def dump_tables(tabs) when is_list(tabs) do
    schema_transaction(fn ->
      do_dump_tables(tabs)
    end)
  end

  def dump_tables(tabs) do
    {:aborted, {:bad_type, tabs}}
  end

  defp do_dump_tables(tabs) do
    tidTs = get_tid_ts_and_lock(:schema, :write)
    insert_schema_ops(tidTs, make_dump_tables(tabs))
  end

  defp make_dump_tables([:schema | _Tabs]) do
    :mnesia.abort({:bad_type, :schema})
  end

  defp make_dump_tables([tab | tabs]) do
    get_tid_ts_and_lock(tab, :read)
    tabDef = get_create_list(tab)

    discResident =
      val({tab, :disc_copies}) ++
        val({tab, :disc_only_copies}) ++
        :lists.concat(
          for {{a, m}, ns} <- val({tab, :external_copies}),
              :lists.member(
                :mnesia_lib.semantics(
                  {:ext, a, m},
                  :storage
                ),
                [:disc_copies, :disc_only_copies]
              ) do
            ns
          end
        )

    verify([], discResident, {'Only allowed on ram_copies', tab, discResident})

    [
      {:op, :dump_table, :unknown, tabDef}
      | make_dump_tables(tabs)
    ]
  end

  defp make_dump_tables([]) do
    []
  end

  def merge_schema() do
    schema_transaction(fn ->
      do_merge_schema([])
    end)
  end

  def merge_schema(userFun) do
    schema_transaction(fn ->
      userFun.(fn arg ->
        do_merge_schema(arg)
      end)
    end)
  end

  defp do_merge_schema(lockTabs0) do
    {_Mod, tid, ts} = get_tid_ts_and_lock(:schema, :write)

    lockTabs =
      for t <- lockTabs0 do
        {t, tab_to_nodes(t)}
      end

    for {t, _} <- lockTabs do
      get_tid_ts_and_lock(t, :write)
    end

    connected = val(:recover_nodes)
    running = val({:current, :db_nodes})
    store = r_tidstore(ts, :store)

    case running -- :ets.lookup_element(store, :nodes, 2) do
      [] ->
        :ok

      miss ->
        :mnesia.abort({:bad_commit, {:missing_lock, miss}})
    end

    case connected -- running do
      [node | _] = otherNodes ->
        :mnesia_locker.wlock_no_exist(tid, store, :schema, [node])

        for {t, ns} <- lockTabs do
          :mnesia_locker.wlock_no_exist(tid, store, t, :mnesia_lib.intersect(ns, otherNodes))
        end

        case fetch_cstructs(node) do
          {:cstructs, cstructs, remoteRunning1} ->
            lockedAlready = running ++ [node]
            {new, old} = :mnesia_recover.connect_nodes(remoteRunning1)

            remoteRunning =
              :mnesia_lib.intersect(
                new ++ old,
                remoteRunning1
              )

            cond do
              remoteRunning != remoteRunning1 ->
                :mnesia_lib.error(
                  'Mnesia on ~p could not connect to node(s) ~p~n',
                  [node(), remoteRunning1 -- remoteRunning]
                )

                :mnesia.abort({:node_not_running, remoteRunning1 -- remoteRunning})

              true ->
                :ok
            end

            needsLock = remoteRunning -- lockedAlready
            :mnesia_locker.wlock_no_exist(tid, store, :schema, needsLock)

            for {t, ns} <- lockTabs do
              :mnesia_locker.wlock_no_exist(
                tid,
                store,
                t,
                :mnesia_lib.intersect(
                  ns,
                  needsLock
                )
              )
            end

            {:value, schemaCs} = :lists.keysearch(:schema, r_cstruct(:name), cstructs)
            schemaDef = cs2list(false, schemaCs)
            a = [{:op, :announce_im_running, node(), schemaDef, running, remoteRunning}]
            do_insert_schema_ops(store, a)

            do_insert_schema_ops(
              store,
              make_merge_schema(node, false, cstructs)
            )

            tabs = val({:schema, :tables})

            ops =
              for t <- tabs,
                  not :lists.keymember(t, r_cstruct(:name), cstructs) do
                {:op, :merge_schema, get_create_list(t)}
              end

            do_insert_schema_ops(store, ops)
            newNodes = remoteRunning -- running

            :mnesia_lib.set(
              :prepare_op,
              {:announce_im_running, newNodes}
            )

            announce_im_running(newNodes, schemaCs)
            {:merged, running, remoteRunning}

          {:error, reason} ->
            {'Cannot get cstructs', node, reason}

          {:badrpc, reason} ->
            {'Cannot get cstructs', node, {:badrpc, reason}}
        end

      [] ->
        :not_merged
    end
  end

  defp fetch_cstructs(node) do
    :rpc.call(node, :mnesia_controller, :get_remote_cstructs, [])
  end

  defp tab_to_nodes(tab) when is_atom(tab) do
    cs = val({tab, :cstruct})
    :mnesia_lib.cs_to_nodes(cs)
  end

  defp make_merge_schema(node, needsConv, [cs | cstructs]) do
    ops = do_make_merge_schema(node, needsConv, cs)
    ops ++ make_merge_schema(node, needsConv, cstructs)
  end

  defp make_merge_schema(_Node, _, []) do
    []
  end

  defp do_make_merge_schema(node, needsConv, remoteCs = r_cstruct(name: :schema)) do
    cs = val({:schema, :cstruct})
    masters = :mnesia_recover.get_master_nodes(:schema)
    hasRemoteMaster = :lists.member(node, masters)
    hasLocalMaster = :lists.member(node(), masters)
    force = :erlang.or(hasLocalMaster, hasRemoteMaster)
    stCsLocal = :mnesia_lib.cs_to_storage_type(node(), cs)

    stRcsLocal =
      :mnesia_lib.cs_to_storage_type(
        node(),
        remoteCs
      )

    stCsRemote = :mnesia_lib.cs_to_storage_type(node, cs)

    stRcsRemote =
      :mnesia_lib.cs_to_storage_type(
        node,
        remoteCs
      )

    cond do
      r_cstruct(cs, :cookie) == r_cstruct(remoteCs, :cookie) and
          r_cstruct(cs, :version) == r_cstruct(remoteCs, :version) ->
        []

      r_cstruct(cs, :cookie) != r_cstruct(remoteCs, :cookie) and
        r_cstruct(cs, :disc_copies) != [] and
          r_cstruct(remoteCs, :disc_copies) != [] ->
        cond do
          hasLocalMaster == true and hasRemoteMaster == false ->
            [{:op, :merge_schema, cs2list(needsConv, cs)}]

          hasRemoteMaster == true and hasLocalMaster == false ->
            [{:op, :merge_schema, cs2list(needsConv, remoteCs)}]

          true ->
            str =
              :io_lib.format(
                'Incompatible schema cookies. Please, restart from old backup.~w = ~w, ~w = ~w~n',
                [node, cs2list(remoteCs), node(), cs2list(cs)]
              )

            throw(str)
        end

      stCsLocal != stRcsLocal and stRcsLocal != :unknown and
          stCsLocal != :ram_copies ->
        str =
          :io_lib.format(
            'Incompatible schema storage types (local). on ~w storage ~w, on ~w storage ~w~n',
            [node(), stCsLocal, node, stRcsLocal]
          )

        throw(str)

      stCsRemote != stRcsRemote and
        stCsRemote != :unknown and
          stRcsRemote != :ram_copies ->
        str =
          :io_lib.format(
            'Incompatible schema storage types (remote). on ~w cs ~w, on ~w rcs ~w~n',
            [node(), cs2list(cs), node, cs2list(remoteCs)]
          )

        throw(str)

      r_cstruct(cs, :disc_copies) != [] ->
        mergedCs = merge_cstructs(cs, remoteCs, force)
        [{:op, :merge_schema, cs2list(needsConv, mergedCs)}]

      r_cstruct(remoteCs, :disc_copies) != [] ->
        mergedCs = merge_cstructs(remoteCs, cs, force)
        [{:op, :merge_schema, cs2list(needsConv, mergedCs)}]

      cs > remoteCs ->
        mergedCs = merge_cstructs(remoteCs, cs, force)
        [{:op, :merge_schema, cs2list(needsConv, mergedCs)}]

      true ->
        mergedCs = merge_cstructs(cs, remoteCs, force)
        [{:op, :merge_schema, cs2list(needsConv, mergedCs)}]
    end
  end

  defp do_make_merge_schema(node, needsConv, remoteCs = r_cstruct()) do
    tab = r_cstruct(remoteCs, :name)
    masters = :mnesia_recover.get_master_nodes(:schema)
    hasRemoteMaster = :lists.member(node, masters)
    hasLocalMaster = :lists.member(node(), masters)
    force = :erlang.or(hasLocalMaster, hasRemoteMaster)

    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :cstruct}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        [{:op, :merge_schema, cs2list(needsConv, remoteCs)}]

      cs when r_cstruct(cs, :cookie) == r_cstruct(remoteCs, :cookie) ->
        cond do
          r_cstruct(cs, :version) == r_cstruct(remoteCs, :version) ->
            []

          r_cstruct(cs, :version) > r_cstruct(remoteCs, :version) ->
            mergedCs = merge_cstructs(cs, remoteCs, force)
            [{:op, :merge_schema, cs2list(needsConv, mergedCs)}]

          r_cstruct(cs, :version) < r_cstruct(remoteCs, :version) ->
            mergedCs = merge_cstructs(remoteCs, cs, force)
            [{:op, :merge_schema, cs2list(needsConv, mergedCs)}]
        end

      cs ->
        cond do
          hasLocalMaster == true and hasRemoteMaster == false ->
            [{:op, :merge_schema, cs2list(needsConv, cs)}]

          hasRemoteMaster == true and hasLocalMaster == false ->
            [{:op, :merge_schema, cs2list(needsConv, remoteCs)}]

          true ->
            str =
              :io_lib.format(
                'Bad cookie in table definition ~w: ~w = ~w, ~w = ~w~n',
                [tab, node(), cs, node, remoteCs]
              )

            throw(str)
        end
    end
  end

  defp merge_cstructs(cs0, remoteCs, force) do
    cs = verify_cstruct(cs0)

    try do
      do_merge_cstructs(cs, remoteCs, force)
    catch
      :exit, {:aborted, _Reason} when force == true ->
        cs

      :exit, reason ->
        exit(reason)

      :error, reason ->
        exit(reason)
    else
      mergedCs when elem(mergedCs, 0) === :cstruct ->
        mergedCs
    end
  end

  defp do_merge_cstructs(cs, remoteCs0, force) do
    remoteCs = verify_cstruct(remoteCs0)
    ns = :mnesia_lib.uniq(:mnesia_lib.cs_to_nodes(cs) ++ :mnesia_lib.cs_to_nodes(remoteCs))
    {anythingNew, mergedCs} = merge_storage_type(ns, false, cs, remoteCs, force)
    verify_cstruct(merge_versions(anythingNew, mergedCs, remoteCs, force))
  end

  defp merge_storage_type([n | ns], anythingNew, cs, remoteCs, force) do
    local = :mnesia_lib.cs_to_storage_type(n, cs)
    remote = :mnesia_lib.cs_to_storage_type(n, remoteCs)

    case compare_storage_type(true, local, remote) do
      {:same, _Storage} ->
        merge_storage_type(ns, anythingNew, cs, remoteCs, force)

      {:diff, storage} ->
        cs2 = change_storage_type(n, storage, cs)
        merge_storage_type(ns, true, cs2, remoteCs, force)

      :incompatible when force == true ->
        merge_storage_type(ns, anythingNew, cs, remoteCs, force)

      other ->
        str =
          :io_lib.format(
            'Cannot merge storage type for node ~w in cstruct ~w with remote cstruct ~w (~w)~n',
            [n, cs, remoteCs, other]
          )

        throw(str)
    end
  end

  defp merge_storage_type([], anythingNew, mergedCs, _RemoteCs, _Force) do
    {anythingNew, mergedCs}
  end

  defp compare_storage_type(_Retry, any, any) do
    {:same, any}
  end

  defp compare_storage_type(_Retry, :unknown, any) do
    {:diff, any}
  end

  defp compare_storage_type(_Retry, :ram_copies, :disc_copies) do
    {:diff, :disc_copies}
  end

  defp compare_storage_type(_Retry, :disc_copies, :disc_only_copies) do
    {:diff, :disc_only_copies}
  end

  defp compare_storage_type(true, one, another) do
    compare_storage_type(false, another, one)
  end

  defp compare_storage_type(false, _One, _Another) do
    :incompatible
  end

  defp change_storage_type(n, :ram_copies, cs) do
    nodes = [n | r_cstruct(cs, :ram_copies)]
    r_cstruct(cs, ram_copies: :mnesia_lib.uniq(nodes))
  end

  defp change_storage_type(n, :disc_copies, cs) do
    nodes = [n | r_cstruct(cs, :disc_copies)]
    r_cstruct(cs, disc_copies: :mnesia_lib.uniq(nodes))
  end

  defp change_storage_type(n, :disc_only_copies, cs) do
    nodes = [n | r_cstruct(cs, :disc_only_copies)]
    r_cstruct(cs, disc_only_copies: :mnesia_lib.uniq(nodes))
  end

  defp merge_versions(anythingNew, cs, remoteCs, force) do
    cond do
      r_cstruct(cs, :name) == :schema ->
        :ok

      r_cstruct(cs, :name) != :schema and
          r_cstruct(cs, :cookie) == r_cstruct(remoteCs, :cookie) ->
        :ok

      force == true ->
        :ok

      true ->
        str =
          :io_lib.format(
            'Bad cookies. Cannot merge definitions of table ~tw. Local = ~w, Remote = ~w~n',
            [r_cstruct(cs, :name), cs, remoteCs]
          )

        throw(str)
    end

    cond do
      r_cstruct(cs, :name) == r_cstruct(remoteCs, :name) and
        r_cstruct(cs, :type) == r_cstruct(remoteCs, :type) and
        r_cstruct(cs, :local_content) == r_cstruct(remoteCs, :local_content) and
        r_cstruct(cs, :attributes) == r_cstruct(remoteCs, :attributes) and
        r_cstruct(cs, :index) == r_cstruct(remoteCs, :index) and
        r_cstruct(cs, :snmp) == r_cstruct(remoteCs, :snmp) and
        r_cstruct(cs, :access_mode) == r_cstruct(remoteCs, :access_mode) and
        r_cstruct(cs, :majority) == r_cstruct(remoteCs, :majority) and
        r_cstruct(cs, :load_order) == r_cstruct(remoteCs, :load_order) and
          r_cstruct(cs, :user_properties) == r_cstruct(remoteCs, :user_properties) ->
        do_merge_versions(anythingNew, cs, remoteCs)

      force == true ->
        do_merge_versions(anythingNew, cs, remoteCs)

      true ->
        str1 =
          :io_lib.format('Cannot merge definitions of table ~tw. Local = ~w, Remote = ~w~n', [
            r_cstruct(cs, :name),
            cs,
            remoteCs
          ])

        throw(str1)
    end
  end

  defp do_merge_versions(anythingNew, mergedCs, remoteCs) do
    {{major1, minor1}, _Detail1} = r_cstruct(mergedCs, :version)
    {{major2, minor2}, _Detail2} = r_cstruct(remoteCs, :version)

    cond do
      anythingNew == false ->
        mergedCs

      r_cstruct(mergedCs, :version) == r_cstruct(remoteCs, :version) ->
        v = {{major1, minor1}, :dummy}
        incr_version(r_cstruct(mergedCs, version: v))

      major1 == major2 ->
        minor = :lists.max([minor1, minor2])
        v = {{major1, minor}, :dummy}
        incr_version(r_cstruct(mergedCs, version: v))

      major1 != major2 ->
        major = :lists.max([major1, major2])
        v = {{major, 0}, :dummy}
        incr_version(r_cstruct(mergedCs, version: v))
    end
  end

  defp verify_merge(remoteCs) do
    tab = r_cstruct(remoteCs, :name)
    masters = :mnesia_recover.get_master_nodes(:schema)
    hasRemoteMaster = masters != []

    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :cstruct}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        :ok

      cs ->
        stCsLocal = :mnesia_lib.cs_to_storage_type(node(), cs)

        stRcsLocal =
          :mnesia_lib.cs_to_storage_type(
            node(),
            remoteCs
          )

        cond do
          stCsLocal == stRcsLocal ->
            :ok

          stCsLocal == :unknown ->
            :ok

          stRcsLocal == :unknown and hasRemoteMaster == false ->
            {:merge_error, cs, remoteCs}

          true ->
            :ok
        end
    end
  end

  defp announce_im_running([n | ns], schemaCs) do
    {l1, l2} = :mnesia_recover.connect_nodes([n])

    case :erlang.or(
           :lists.member(n, l1),
           :lists.member(n, l2)
         ) do
      true ->
        :mnesia_lib.add({:current, :db_nodes}, n)
        :mnesia_controller.add_active_replica(:schema, n, schemaCs)

      false ->
        :mnesia_lib.error('Mnesia on ~p could not connect to node ~p~n', [node(), n])
        :mnesia.abort({:node_not_running, n})
    end

    announce_im_running(ns, schemaCs)
  end

  defp announce_im_running([], _) do
    []
  end

  defp unannounce_im_running([n | ns]) do
    :mnesia_lib.del({:current, :db_nodes}, n)
    :mnesia_controller.del_active_replica(:schema, n)
    unannounce_im_running(ns)
  end

  defp unannounce_im_running([]) do
    :ok
  end
end
