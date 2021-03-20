defmodule :m_mnesia_bup do
  use Bitwise
  import :mnesia_lib, only: [dbg_out: 2, verbose: 2]
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

  Record.defrecord(:r_restore, :restore,
    mode: :undefined,
    bup_module: :undefined,
    bup_data: :undefined
  )

  Record.defrecord(:r_fallback_args, :fallback_args,
    opaque: :undefined,
    scope: :global,
    module: :mnesia_monitor.get_env(:backup_module),
    use_default_dir: true,
    mnesia_dir: :undefined,
    fallback_bup: :undefined,
    fallback_tmp: :undefined,
    skip_tables: [],
    keep_tables: [],
    default_op: :keep_tables
  )

  def iterate(mod, fun, opaque, acc) do
    r = r_restore(bup_module: mod, bup_data: opaque)

    try do
      read_schema_section(r)
    catch
      {:error, _} = err ->
        err
    else
      {r2, {header, schema, rest}} ->
        ext = get_ext_types(schema)

        try do
          iter(r2, header, schema, ext, fun, acc, rest)
        catch
          err ->
            close_read(r2)
            err

          _, reason ->
            close_read(r2)
            {:error, {reason, __STACKTRACE__}}
        else
          {:ok, r3, res} ->
            close_read(r3)
            {:ok, res}
        end
    end
  end

  defp get_ext_types(schema) do
    try do
      list = lookup_schema(:schema, schema)

      case :lists.keyfind(:user_properties, 1, list) do
        {_, props} ->
          :proplists.get_value(:mnesia_backend_types, props, [])

        false ->
          []
      end
    catch
      {:error, {'Cannot lookup', _}} ->
        []
    end
  end

  defp iter(r, header, schema, ext, fun, acc, []) do
    case safe_apply(r, :read, [r_restore(r, :bup_data)]) do
      {r2, []} ->
        res = fun.([], header, schema, ext, acc)
        {:ok, r2, res}

      {r2, bupItems} ->
        iter(r2, header, schema, ext, fun, acc, bupItems)
    end
  end

  defp iter(r, header, schema, ext, fun, acc, bupItems) do
    acc2 = fun.(bupItems, header, schema, ext, acc)
    iter(r, header, schema, ext, fun, acc2, [])
  end

  defp safe_apply(r, :write, [_, items]) when items === [] do
    r
  end

  defp safe_apply(r, what, args) do
    abort = abort_restore_fun(r, what, args)
    mod = r_restore(r, :bup_module)

    try do
      apply(mod, what, args)
    catch
      _, re ->
        abort.(re)
    else
      {:ok, opaque, items} when what === :read ->
        {r_restore(r, bup_data: opaque), items}

      {:ok, opaque} when what !== :read ->
        r_restore(r, bup_data: opaque)

      {:error, re} ->
        abort.(re)

      re ->
        abort.(re)
    end
  end

  defp abort_restore_fun(r, what, args) do
    fn re ->
      abort_restore(r, what, args, re)
    end
  end

  defp abort_restore(r = r_restore(bup_module: mod), what, args, reason) do
    dbg_out('Restore aborted. ~p:~p~p -> ~p~n', [mod, what, args, reason])
    close_read(r)
    throw({:error, reason})
  end

  defp close_read(r_restore(bup_module: mod, bup_data: opaque)) do
    try do
      mod.close_read(opaque)
    catch
      :error, _ ->
        :ok
    end
  end

  def fallback_to_schema() do
    fname = fallback_bup()
    fallback_to_schema(fname)
  end

  defp fallback_to_schema(fname) do
    mod = :mnesia_backup

    case read_schema(mod, fname) do
      {:error, reason} ->
        {:error, reason}

      schema ->
        try do
          lookup_schema(:schema, schema)
        catch
          _ ->
            {:error, 'No schema in fallback'}
        else
          list ->
            {:ok, :fallback, list}
        end
    end
  end

  def read_schema(mod, opaque) do
    r = r_restore(bup_module: mod, bup_data: opaque)

    try do
      read_schema_section(r)
    catch
      {:error, _} = error ->
        error
    else
      {r2, {_Header, schema, _}} ->
        close_read(r2)
        schema
    end
  end

  defp read_schema_section(r) do
    {r2, {h, schema, rest}} = do_read_schema_section(r)
    schema2 = convert_schema(r_log_header(h, :log_version), schema)
    {r2, {h, schema2, rest}}
  end

  defp do_read_schema_section(r) do
    r2 = safe_apply(r, :open_read, [r_restore(r, :bup_data)])

    try do
      {r3, rawSchema} = safe_apply(r2, :read, [r_restore(r2, :bup_data)])
      do_read_schema_section(r3, verify_header(rawSchema), [])
    catch
      t, e ->
        close_read(r2)
        :erlang.raise(t, e, __STACKTRACE__)
    end
  end

  defp do_read_schema_section(r, {:ok, b, c, []}, acc) do
    case safe_apply(r, :read, [r_restore(r, :bup_data)]) do
      {r2, []} ->
        {r2, {b, acc, []}}

      {r2, rawSchema} ->
        do_read_schema_section(r2, {:ok, b, c, rawSchema}, acc)
    end
  end

  defp do_read_schema_section(r, {:ok, b, c, [head | tail]}, acc)
       when :erlang.element(1, head) === :schema do
    do_read_schema_section(r, {:ok, b, c, tail}, acc ++ [head])
  end

  defp do_read_schema_section(r, {:ok, b, _C, rest}, acc) do
    {r, {b, acc, rest}}
  end

  defp do_read_schema_section(_R, {:error, reason}, _Acc) do
    throw({:error, reason})
  end

  defp verify_header([h | rawSchema])
       when elem(h, 0) === :log_header do
    current = :mnesia_log.backup_log_header()

    cond do
      r_log_header(h, :log_kind) === r_log_header(current, :log_kind) ->
        versions = ['0.1', '1.1', r_log_header(current, :log_version)]

        case :lists.member(r_log_header(h, :log_version), versions) do
          true ->
            {:ok, h, current, rawSchema}

          false ->
            {:error, {'Bad header version. Cannot be used as backup.', h}}
        end

      true ->
        {:error, {'Bad kind of header. Cannot be used as backup.', h}}
    end
  end

  defp verify_header(rawSchema) do
    {:error,
     {'Missing header. Cannot be used as backup.',
      try do
        hd(rawSchema)
      catch
        _, _Reason ->
          {:EXIT, _Reason}
      end}}
  end

  def refresh_cookie(schema, newCookie) do
    case :lists.keysearch(:schema, 2, schema) do
      {:value, {:schema, :schema, list}} ->
        cs = :mnesia_schema.list2cs(list)
        cs2 = r_cstruct(cs, cookie: newCookie)
        item = {:schema, :schema, :mnesia_schema.cs2list(cs2)}
        :lists.keyreplace(:schema, 2, schema, item)

      false ->
        reason = 'No schema found. Cannot be used as backup.'
        throw({:error, {reason, schema}})
    end
  end

  defp convert_schema('0.1', schema) do
    convert_0_1(schema)
  end

  defp convert_schema('1.1', schema) do
    current = :mnesia_log.backup_log_header()
    convert_schema(r_log_header(current, :log_version), schema)
  end

  defp convert_schema(latest, schema) do
    h = :mnesia_log.backup_log_header()

    cond do
      r_log_header(h, :log_version) === latest ->
        schema

      true ->
        reason = 'Bad backup header version. Cannot convert schema.'
        throw({:error, {reason, h}})
    end
  end

  defp convert_0_1(schema) do
    case :lists.keysearch(:schema, 2, schema) do
      {:value, {:schema, :schema, list}} ->
        schema2 = :lists.keydelete(:schema, 2, schema)
        cs = :mnesia_schema.list2cs(list)
        convert_0_1(schema2, [], cs)

      false ->
        list = :mnesia_schema.get_initial_schema(:disc_copies, [node()], [])
        cs = :mnesia_schema.list2cs(list)
        convert_0_1(schema, [], cs)
    end
  end

  defp convert_0_1([{:schema, :cookie, cookie} | schema], acc, cs) do
    convert_0_1(schema, acc, r_cstruct(cs, cookie: cookie))
  end

  defp convert_0_1([{:schema, :db_nodes, dbNodes} | schema], acc, cs) do
    convert_0_1(schema, acc, r_cstruct(cs, disc_copies: dbNodes))
  end

  defp convert_0_1([{:schema, :version, version} | schema], acc, cs) do
    convert_0_1(schema, acc, r_cstruct(cs, version: version))
  end

  defp convert_0_1([{:schema, tab, def__} | schema], acc, cs) do
    head =
      case :lists.keysearch(:index, 1, def__) do
        {:value, {:index, posList}} ->
          p = posList -- [:snmp]
          def2 = :lists.keyreplace(:index, 1, def__, {:index, p})
          {:schema, tab, def2}

        false ->
          {:schema, tab, def__}
      end

    convert_0_1(schema, [head | acc], cs)
  end

  defp convert_0_1([head | schema], acc, cs) do
    convert_0_1(schema, [head | acc], cs)
  end

  defp convert_0_1([], acc, cs) do
    [schema2bup({:schema, :schema, cs}) | acc]
  end

  def lookup_schema(key, schema) do
    case :lists.keysearch(key, 2, schema) do
      {:value, {:schema, ^key, val}} ->
        val

      false ->
        throw({:error, {'Cannot lookup', key}})
    end
  end

  def schema2bup({:schema, tab}) do
    {:schema, tab}
  end

  def schema2bup({:schema, tab, tableDef}) do
    {:schema, tab, :mnesia_schema.cs2list(tableDef)}
  end

  def create_schema(nodes) do
    create_schema(nodes, [])
  end

  def create_schema([], props) do
    create_schema([node()], props)
  end

  def create_schema(ns, props)
      when is_list(ns) and
             is_list(props) do
    case is_set(ns) do
      true ->
        create_schema(ns, :mnesia_schema.ensure_no_schema(ns), props)

      false ->
        {:error, {:combine_error, ns}}
    end
  end

  def create_schema(ns, _Props) do
    {:error, {:badarg, ns}}
  end

  defp is_set(list) when is_list(list) do
    :ordsets.is_set(:lists.sort(list))
  end

  defp is_set(_) do
    false
  end

  defp create_schema(ns, :ok, props) do
    case :mnesia_lib.ensure_loaded(:mnesia) do
      :ok ->
        case :mnesia_monitor.get_env(:schema_location) do
          :ram ->
            {:error, {:has_no_disc, node()}}

          _ ->
            case :mnesia_schema.opt_create_dir(
                   true,
                   :mnesia_lib.dir()
                 ) do
              {:error, what} ->
                {:error, what}

              :ok ->
                mod = :mnesia_backup
                str = mk_str()
                file = :mnesia_lib.dir(str)
                :file.delete(file)

                try do
                  make_initial_backup(ns, file, mod, props)
                catch
                  {:error, reason} ->
                    {:error, reason}
                else
                  {:ok, _Res} ->
                    case do_install_fallback(file, mod) do
                      :ok ->
                        :file.delete(file)
                        :ok

                      {:error, reason} ->
                        {:error, reason}
                    end
                end
            end
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp create_schema(_Ns, {:error, reason}, _) do
    {:error, reason}
  end

  defp create_schema(_Ns, reason, _) do
    {:error, reason}
  end

  defp mk_str() do
    now = :erlang.integer_to_list(:erlang.unique_integer([:positive]))
    :lists.concat([node()] ++ now ++ '.TMP')
  end

  def make_initial_backup(ns, opaque, mod) do
    make_initial_backup(ns, opaque, mod, [])
  end

  defp make_initial_backup(ns, opaque, mod, props) do
    orig = :mnesia_schema.get_initial_schema(:disc_copies, ns, props)

    modded =
      :proplists.delete(
        :storage_properties,
        :proplists.delete(:majority, orig)
      )

    schema = [{:schema, :schema, modded}]
    o2 = do_apply(mod, :open_write, [opaque], opaque)
    o3 = do_apply(mod, :write, [o2, [:mnesia_log.backup_log_header()]], o2)
    o4 = do_apply(mod, :write, [o3, schema], o3)
    o5 = do_apply(mod, :commit_write, [o4], o4)
    {:ok, o5}
  end

  defp do_apply(_, :write, [_, items], opaque)
       when items === [] do
    opaque
  end

  defp do_apply(mod, what, args, _Opaque) do
    try do
      apply(mod, what, args)
    catch
      _, reason ->
        throw({:error, {:EXIT, reason}})
    else
      {:ok, opaque2} ->
        opaque2

      {:error, reason} ->
        throw({:error, reason})
    end
  end

  def install_fallback(opaque) do
    install_fallback(opaque, [])
  end

  def install_fallback(opaque, args) do
    case :mnesia_lib.ensure_loaded(:mnesia) do
      :ok ->
        do_install_fallback(opaque, args)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_install_fallback(opaque, mod) when is_atom(mod) do
    do_install_fallback(opaque, [{:module, mod}])
  end

  defp do_install_fallback(opaque, args) when is_list(args) do
    case check_fallback_args(args, r_fallback_args(opaque: opaque)) do
      {:ok, fA} ->
        do_install_fallback(fA)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_install_fallback(_Opaque, args) do
    {:error, {:badarg, args}}
  end

  defp check_fallback_args([arg | tail], fA) do
    try do
      check_fallback_arg_type(arg, fA)
    catch
      :error, _ ->
        {:error, {:badarg, arg}}
    else
      fA2 ->
        check_fallback_args(tail, fA2)
    end
  end

  defp check_fallback_args([], fA) do
    {:ok, fA}
  end

  defp check_fallback_arg_type(arg, fA) do
    case arg do
      {:scope, :global} ->
        r_fallback_args(fA, scope: :global)

      {:scope, :local} ->
        r_fallback_args(fA, scope: :local)

      {:module, mod} ->
        mod2 =
          :mnesia_monitor.do_check_type(
            :backup_module,
            mod
          )

        r_fallback_args(fA, module: mod2)

      {:mnesia_dir, dir} ->
        r_fallback_args(fA, mnesia_dir: dir, use_default_dir: false)

      {:keep_tables, tabs} ->
        atom_list(tabs)
        r_fallback_args(fA, keep_tables: tabs)

      {:skip_tables, tabs} ->
        atom_list(tabs)
        r_fallback_args(fA, skip_tables: tabs)

      {:default_op, :keep_tables} ->
        r_fallback_args(fA, default_op: :keep_tables)

      {:default_op, :skip_tables} ->
        r_fallback_args(fA, default_op: :skip_tables)
    end
  end

  defp atom_list([h | t]) when is_atom(h) do
    atom_list(t)
  end

  defp atom_list([]) do
    :ok
  end

  defp do_install_fallback(fA) do
    pid = spawn_link(:mnesia_bup, :install_fallback_master, [self(), fA])

    res =
      receive do
        {:EXIT, ^pid, reason} ->
          {:error, {:EXIT, reason}}

        {^pid, res2} ->
          case res2 do
            {:ok, _} ->
              :ok

            {:error, reason} ->
              {:error, {'Cannot install fallback', reason}}
          end
      end

    res
  end

  def install_fallback_master(clientPid, fA) do
    :erlang.process_flag(:trap_exit, true)
    state = {:start, fA}
    opaque = r_fallback_args(fA, :opaque)
    mod = r_fallback_args(fA, :module)
    res = iterate(mod, &restore_recs/5, opaque, state)
    :erlang.unlink(clientPid)
    send(clientPid, {self(), res})
    exit(:shutdown)
  end

  defp restore_recs(_, _, _, _, :stop) do
    throw({:error, 'restore_recs already stopped'})
  end

  defp restore_recs(recs, header, schema, ext, {:start, fA}) do
    schema2 =
      convert_schema(
        r_log_header(header, :log_version),
        schema
      )

    createList = lookup_schema(:schema, schema2)

    try do
      :mnesia_schema.list2cs(createList)
    catch
      _, reason ->
        throw({:error, {'Bad schema in restore_recs', reason}})
    else
      cs ->
        ns = get_fallback_nodes(fA, r_cstruct(cs, :disc_copies))
        :global.set_lock({{:mnesia_table_lock, :schema}, self()}, ns, :infinity)
        args = [self(), fA]

        pids =
          for n <- ns do
            :erlang.spawn_link(n, :mnesia_bup, :fallback_receiver, args)
          end

        send_fallback(pids, {:start, header, schema2})
        res = restore_recs(recs, header, schema2, ext, pids)

        :global.del_lock(
          {{:mnesia_table_lock, :schema}, self()},
          ns
        )

        res
    end
  end

  defp restore_recs([], _Header, _Schema, _Ext, pids) do
    send_fallback(pids, :swap)
    send_fallback(pids, :stop)
    :stop
  end

  defp restore_recs(recs, _, _, _, pids) do
    send_fallback(pids, {:records, recs})
    pids
  end

  defp get_fallback_nodes(fA, ns) do
    this = node()

    case :lists.member(this, ns) do
      true ->
        case r_fallback_args(fA, :scope) do
          :global ->
            ns

          :local ->
            [this]
        end

      false ->
        throw({:error, {'No disc resident schema on local node', ns}})
    end
  end

  defp send_fallback(pids, msg)
       when is_list(pids) and
              pids !== [] do
    :lists.foreach(
      fn pid ->
        send(pid, {self(), msg})
      end,
      pids
    )

    rec_answers(pids, [])
  end

  defp rec_answers([], acc) do
    case {:lists.keysearch(:error, 1, acc), :mnesia_lib.uniq(acc)} do
      {{:value, {:error, val}}, _} ->
        throw({:error, val})

      {_, [sameAnswer]} ->
        sameAnswer

      {_, other} ->
        throw({:error, {'Different answers', other}})
    end
  end

  defp rec_answers(pids, acc) do
    receive do
      {:EXIT, pid, :stopped} ->
        pids2 = :lists.delete(pid, pids)
        rec_answers(pids2, [:stopped | acc])

      {:EXIT, pid, reason} ->
        pids2 = :lists.delete(pid, pids)

        rec_answers(
          pids2,
          [{:error, {:EXIT, pid, reason}} | acc]
        )

      {pid, reply} ->
        pids2 = :lists.delete(pid, pids)
        rec_answers(pids2, [reply | acc])
    end
  end

  def fallback_exists() do
    fname = fallback_bup()
    fallback_exists(fname)
  end

  defp fallback_exists(fname) do
    case :mnesia_monitor.use_dir() do
      true ->
        :mnesia_lib.exists(fname)

      false ->
        case (try do
                :ets.lookup_element(:mnesia_gvar, :active_fallback, 2)
              catch
                :error, _ ->
                  {:EXIT, {:badarg, []}}
              end) do
          {:EXIT, _} ->
            false

          bool ->
            bool
        end
    end
  end

  defp fallback_name() do
    'FALLBACK.BUP'
  end

  def fallback_bup() do
    :mnesia_lib.dir(fallback_name())
  end

  defp fallback_tmp_name() do
    'FALLBACK.TMP'
  end

  def fallback_receiver(master, fA) do
    :erlang.process_flag(:trap_exit, true)

    res =
      try do
        :erlang.register(:mnesia_fallback, self())
        fA2 = check_fallback_dir(fA)
        bup = r_fallback_args(fA2, :fallback_bup)
        false = :mnesia_lib.exists(bup)
        mod = :mnesia_backup
        tmp = r_fallback_args(fA2, :fallback_tmp)
        r = r_restore(mode: :replace, bup_module: mod, bup_data: tmp)
        :file.delete(tmp)
        fallback_receiver_loop(master, r, fA2, :schema)
      catch
        :error, _ ->
          reason = {:already_exists, node()}
          local_fallback_error(master, reason)

        {:error, reason} ->
          local_fallback_error(master, reason)
      end

    exit(res)
  end

  defp local_fallback_error(master, reason) do
    send(master, {self(), {:error, reason}})
    :erlang.unlink(master)
    exit(reason)
  end

  defp check_fallback_dir(master, fA) do
    try do
      check_fallback_dir(fA)
    catch
      {:error, reason} ->
        local_fallback_error(master, reason)
    end
  end

  defp check_fallback_dir(fA) do
    case :mnesia.system_info(:schema_location) do
      :ram ->
        reason = {:has_no_disc, node()}
        throw({:error, reason})

      _ ->
        dir = check_fallback_dir_arg(fA)
        bup = :filename.join([dir, fallback_name()])
        tmp = :filename.join([dir, fallback_tmp_name()])
        r_fallback_args(fA, fallback_bup: bup, fallback_tmp: tmp, mnesia_dir: dir)
    end
  end

  defp check_fallback_dir_arg(fA) do
    case r_fallback_args(fA, :use_default_dir) do
      true ->
        :mnesia_lib.dir()

      false when r_fallback_args(fA, :scope) === :local ->
        dir = r_fallback_args(fA, :mnesia_dir)

        try do
          :mnesia_monitor.do_check_type(:dir, dir)
        catch
          _, _ ->
            reason = {:badarg, {:dir, dir}, node()}
            throw({:error, reason})
        end

      false when r_fallback_args(fA, :scope) === :global ->
        reason = {:combine_error, :global, :dir, node()}
        throw({:error, reason})
    end
  end

  defp fallback_receiver_loop(master, r, fA, state) do
    receive do
      {^master, {:start, header, schema}}
      when state === :schema ->
        dir = r_fallback_args(fA, :mnesia_dir)

        throw_bad_res(
          :ok,
          :mnesia_schema.opt_create_dir(true, dir)
        )

        r2 = safe_apply(r, :open_write, [r_restore(r, :bup_data)])
        r3 = safe_apply(r2, :write, [r_restore(r2, :bup_data), [header]])

        bupSchema =
          for s <- schema do
            schema2bup(s)
          end

        r4 = safe_apply(r3, :write, [r_restore(r3, :bup_data), bupSchema])
        send(master, {self(), :ok})
        fallback_receiver_loop(master, r4, fA, :records)

      {^master, {:records, recs}} when state === :records ->
        r2 = safe_apply(r, :write, [r_restore(r, :bup_data), recs])
        send(master, {self(), :ok})
        fallback_receiver_loop(master, r2, fA, :records)

      {^master, :swap} when state !== :schema ->
        :ok
        safe_apply(r, :commit_write, [r_restore(r, :bup_data)])
        bup = r_fallback_args(fA, :fallback_bup)
        tmp = r_fallback_args(fA, :fallback_tmp)
        throw_bad_res(:ok, :file.rename(tmp, bup))

        try do
          :mnesia_lib.set(:active_fallback, true)
        catch
          :error, _ ->
            :ok
        end

        :ok
        send(master, {self(), :ok})
        fallback_receiver_loop(master, r, fA, :stop)

      {^master, :stop} when state === :stop ->
        :stopped

      msg ->
        safe_apply(r, :abort_write, [r_restore(r, :bup_data)])
        tmp = r_fallback_args(fA, :fallback_tmp)
        :file.delete(tmp)
        throw({:error, 'Unexpected msg fallback_receiver_loop', msg})
    end
  end

  defp throw_bad_res(expected, expected) do
    expected
  end

  defp throw_bad_res(_Expected, {:error, actual}) do
    throw({:error, actual})
  end

  defp throw_bad_res(_Expected, actual) do
    throw({:error, actual})
  end

  Record.defrecord(:r_local_tab, :local_tab,
    name: :undefined,
    storage_type: :undefined,
    open: :undefined,
    add: :undefined,
    close: :undefined,
    swap: :undefined,
    record_name: :undefined,
    opened: :undefined
  )

  def tm_fallback_start(ignoreFallback) do
    :mnesia_schema.lock_schema()

    res =
      do_fallback_start(
        fallback_exists(),
        ignoreFallback
      )

    :mnesia_schema.unlock_schema()

    case res do
      :ok ->
        :ok

      {:error, reason} ->
        exit(reason)
    end
  end

  defp do_fallback_start(false, _IgnoreFallback) do
    :ok
  end

  defp do_fallback_start(true, true) do
    verbose('Ignoring fallback at startup, but leaving it active...~n', [])
    :mnesia_lib.set(:active_fallback, true)
    :ok
  end

  defp do_fallback_start(true, false) do
    verbose('Starting from fallback...~n', [])
    bupFile = fallback_bup()
    mod = :mnesia_backup

    localTabs =
      _ =
      :ets.new(
        :mnesia_local_tables,
        [:set, :public, {:keypos, 2}]
      )

    case iterate(mod, &restore_tables/5, bupFile, {:start, localTabs}) do
      {:ok, _Res} ->
        try do
          :dets.close(:schema)
        catch
          :error, _ ->
            :ok
        end

        tmpSchema = :mnesia_lib.tab2tmp(:schema)
        datSchema = :mnesia_lib.tab2dat(:schema)
        allLT = :ets.match_object(localTabs, :_)
        :ets.delete(localTabs)

        case :file.rename(tmpSchema, datSchema) do
          :ok ->
            for lT <- allLT, r_local_tab(lT, :name) !== :schema do
              r_local_tab(lT, :swap).(r_local_tab(lT, :name), lT)
            end

            :file.delete(bupFile)
            :ok

          {:error, reason} ->
            :file.delete(tmpSchema)
            {:error, {'Cannot start from fallback. Rename error.', reason}}
        end

      {:error, reason} ->
        {:error, {'Cannot start from fallback', reason}}
    end
  end

  defp restore_tables(all = [rec | recs], header, schema, ext, state = {:local, localTabs, lT}) do
    tab = :erlang.element(1, rec)

    cond do
      tab === r_local_tab(lT, :name) ->
        key = :erlang.element(2, rec)
        r_local_tab(lT, :add).(tab, key, rec, lT)
        restore_tables(recs, header, schema, ext, state)

      true ->
        newState = {:new, localTabs}
        restore_tables(all, header, schema, ext, newState)
    end
  end

  defp restore_tables(all = [rec | recs], header, schema, ext, {:new, localTabs}) do
    tab = :erlang.element(1, rec)

    case :ets.lookup(localTabs, tab) do
      [] ->
        state = {:not_local, localTabs, tab}
        restore_tables(recs, header, schema, ext, state)

      [lT] when elem(lT, 0) === :local_tab ->
        state = {:local, localTabs, lT}

        case r_local_tab(lT, :opened) do
          true ->
            :ignore

          false ->
            r_local_tab(lT, :open).(tab, lT)
            :ets.insert(localTabs, r_local_tab(lT, opened: true))
        end

        restore_tables(all, header, schema, ext, state)
    end
  end

  defp restore_tables(
         all = [rec | recs],
         header,
         schema,
         ext,
         s = {:not_local, localTabs, prevTab}
       ) do
    tab = :erlang.element(1, rec)

    cond do
      tab === prevTab ->
        restore_tables(recs, header, schema, ext, s)

      true ->
        state = {:new, localTabs}
        restore_tables(all, header, schema, ext, state)
    end
  end

  defp restore_tables(recs, header, schema, ext, {:start, localTabs}) do
    dir = :mnesia_lib.dir()
    oldDir = :filename.join([dir, 'OLD_DIR'])
    :mnesia_schema.purge_dir(oldDir, [])
    :mnesia_schema.purge_dir(dir, [fallback_name()])
    init_dat_files(schema, ext, localTabs)
    state = {:new, localTabs}
    restore_tables(recs, header, schema, ext, state)
  end

  defp restore_tables([], _Header, _Schema, _Ext, state) do
    state
  end

  defp init_dat_files(schema, ext, localTabs) do
    tmpFile = :mnesia_lib.tab2tmp(:schema)
    args = [{:file, tmpFile}, {:keypos, 2}, {:type, :set}]

    case :dets.open_file(:schema, args) do
      {:ok, _} ->
        create_dat_files(schema, ext, localTabs)
        :ok = :dets.close(:schema)

        localTab =
          r_local_tab(
            name: :schema,
            storage_type: :disc_copies,
            open: :undefined,
            add: :undefined,
            close: :undefined,
            swap: :undefined,
            record_name: :schema,
            opened: false
          )

        :ets.insert(localTabs, localTab)

      {:error, reason} ->
        throw({:error, {'Cannot open file', :schema, args, reason}})
    end
  end

  defp create_dat_files([{:schema, :schema, tabDef} | tail], ext, localTabs) do
    :ok = :dets.insert(:schema, {:schema, :schema, tabDef})
    create_dat_files(tail, ext, localTabs)
  end

  defp create_dat_files([{:schema, tab, tabDef} | tail], ext, localTabs) do
    tmpFile = :mnesia_lib.tab2tmp(tab)
    datFile = :mnesia_lib.tab2dat(tab)
    dclFile = :mnesia_lib.tab2dcl(tab)
    dcdFile = :mnesia_lib.tab2dcd(tab)

    expunge = fn ->
      :file.delete(datFile)
      :file.delete(dclFile)
      :file.delete(dcdFile)
    end

    :mnesia_lib.dets_sync_close(tab)
    :file.delete(tmpFile)
    cs = :mnesia_schema.list2cs(tabDef, ext)
    :ok = :dets.insert(:schema, {:schema, tab, tabDef})
    recName = r_cstruct(cs, :record_name)
    storage = :mnesia_lib.cs_to_storage_type(node(), cs)
    delete_ext(storage, tab)
    semantics = :mnesia_lib.semantics(storage, :storage)

    cond do
      semantics === :undefined ->
        :ok = :dets.delete(:schema, {:schema, tab})
        create_dat_files(tail, ext, localTabs)

      semantics === :disc_only_copies ->
        open = disc_only_open_fun(storage, cs)
        add = disc_only_add_fun(storage, cs)
        close = disc_only_close_fun(storage)
        swap = disc_only_swap_fun(storage, expunge, open, close)

        localTab =
          r_local_tab(
            name: tab,
            storage_type: storage,
            open: open,
            add: add,
            close: close,
            swap: swap,
            record_name: recName,
            opened: false
          )

        :ets.insert(localTabs, localTab)
        create_dat_files(tail, ext, localTabs)

      semantics === :ram_copies or storage === :disc_copies ->
        open = fn t, lT when t === r_local_tab(lT, :name) ->
          :mnesia_log.open_log(
            {:mnesia_bup, t},
            :mnesia_log.dcl_log_header(),
            tmpFile,
            false,
            false,
            :read_write
          )
        end

        add = fn t, key, rec, lT when t === r_local_tab(lT, :name) ->
          log = {:mnesia_bup, t}

          case rec do
            {_T, ^key} ->
              :mnesia_log.append(log, {{t, key}, {t, key}, :delete})

            ^rec when t === recName ->
              :mnesia_log.append(log, {{t, key}, rec, :write})

            ^rec ->
              rec2 = :erlang.setelement(1, rec, recName)
              :mnesia_log.append(log, {{t, key}, rec2, :write})
          end
        end

        close = fn t, lT when t === r_local_tab(lT, :name) ->
          :mnesia_log.close_log({:mnesia_bup, t})
        end

        swap = fn t, lT when t === r_local_tab(lT, :name) ->
          expunge.()

          cond do
            storage === :ram_copies and
                r_local_tab(lT, :opened) === false ->
              :ok

            true ->
              log =
                :mnesia_log.open_log(:fallback_tab, :mnesia_log.dcd_log_header(), dcdFile, false)

              :mnesia_log.close_log(log)

              case r_local_tab(lT, :opened) do
                true ->
                  close.(t, lT)

                false ->
                  open.(t, lT)
                  close.(t, lT)
              end

              case :file.rename(tmpFile, dclFile) do
                :ok ->
                  :ok

                {:error, reason} ->
                  :mnesia_lib.fatal('Cannot rename file ~tp -> ~tp: ~tp~n', [
                    tmpFile,
                    dclFile,
                    reason
                  ])
              end
          end
        end

        localTab =
          r_local_tab(
            name: tab,
            storage_type: storage,
            open: open,
            add: add,
            close: close,
            swap: swap,
            record_name: recName,
            opened: false
          )

        :ets.insert(localTabs, localTab)
        create_dat_files(tail, ext, localTabs)

      true ->
        :erlang.error(
          {:unknown_semantics, [{:semantics, semantics}, {:tabdef, tabDef}, {:ext, ext}]}
        )
    end
  end

  defp create_dat_files([{:schema, tab} | tail], ext, localTabs) do
    :ets.delete(localTabs, tab)
    :ok = :dets.delete(:schema, {:schema, tab})
    tmpFile = :mnesia_lib.tab2tmp(tab)
    :mnesia_lib.dets_sync_close(tab)
    :file.delete(tmpFile)
    create_dat_files(tail, ext, localTabs)
  end

  defp create_dat_files([], _Ext, _LocalTabs) do
    :ok
  end

  defp delete_ext({:ext, alias, mod}, tab) do
    mod.close_table(alias, tab)
    mod.delete_table(alias, tab)
    :ok
  end

  defp delete_ext(_, _) do
    :ok
  end

  defp disc_only_open_fun(:disc_only_copies, r_cstruct(name: tab) = cs) do
    tmpFile = :mnesia_lib.tab2tmp(tab)

    args = [
      {:file, tmpFile},
      {:keypos, 2},
      {:type,
       :mnesia_lib.disk_type(
         tab,
         r_cstruct(cs, :type)
       )}
    ]

    fn t, lT when t === r_local_tab(lT, :name) ->
      case :mnesia_lib.dets_sync_open(t, args) do
        {:ok, _} ->
          :ok

        {:error, reason} ->
          throw({:error, {'Cannot open file', t, args, reason}})
      end
    end
  end

  defp disc_only_open_fun({:ext, alias, mod}, cs) do
    fn t, lT when t === r_local_tab(lT, :name) ->
      :ok = mod.load_table(alias, t, :restore, :mnesia_schema.cs2list(cs))
    end
  end

  defp disc_only_add_fun(storage, r_cstruct(name: tab, record_name: recName)) do
    fn t, key, rec, r_local_tab(name: t) when t === tab ->
      case rec do
        {_T, ^key} ->
          :ok = :mnesia_lib.db_erase(storage, t, key)

        ^rec when t === recName ->
          :ok = :mnesia_lib.db_put(storage, t, rec)

        ^rec ->
          :ok = :mnesia_lib.db_put(storage, t, :erlang.setelement(1, rec, recName))
      end
    end
  end

  defp disc_only_close_fun(:disc_only_copies) do
    fn t, lT when t === r_local_tab(lT, :name) ->
      :mnesia_lib.dets_sync_close(t)
    end
  end

  defp disc_only_close_fun({:ext, alias, mod}) do
    fn t, _LT ->
      mod.sync_close_table(alias, t)
    end
  end

  defp disc_only_swap_fun(:disc_only_copies, expunge, open, close) do
    fn t, lT when t === r_local_tab(lT, :name) ->
      tmpFile = :mnesia_lib.tab2tmp(t)
      datFile = :mnesia_lib.tab2dat(t)
      expunge.()

      case r_local_tab(lT, :opened) do
        true ->
          close.(t, lT)

        false ->
          open.(t, lT)
          close.(t, lT)
      end

      case :file.rename(tmpFile, datFile) do
        :ok ->
          :ok

        {:error, reason} ->
          :mnesia_lib.fatal('Cannot rename file ~tp -> ~tp: ~tp~n', [tmpFile, datFile, reason])
      end
    end
  end

  defp disc_only_swap_fun({:ext, _Alias, _Mod}, _Expunge, _Open, close) do
    fn t, r_local_tab(name: t) = lT ->
      close.(t, lT)
    end
  end

  def uninstall_fallback() do
    uninstall_fallback([{:scope, :global}])
  end

  def uninstall_fallback(args) do
    case check_fallback_args(args, r_fallback_args()) do
      {:ok, fA} ->
        do_uninstall_fallback(fA)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_uninstall_fallback(fA) do
    case :mnesia_lib.ensure_loaded(:mnesia) do
      :ok ->
        pid = spawn_link(:mnesia_bup, :uninstall_fallback_master, [self(), fA])

        receive do
          {:EXIT, ^pid, reason} ->
            {:error, {:EXIT, reason}}

          {^pid, res} ->
            res
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  def uninstall_fallback_master(clientPid, fA) do
    :erlang.process_flag(:trap_exit, true)
    fA2 = check_fallback_dir(clientPid, fA)
    bup = r_fallback_args(fA2, :fallback_bup)

    case fallback_to_schema(bup) do
      {:ok, :fallback, list} ->
        cs = :mnesia_schema.list2cs(list)

        try do
          get_fallback_nodes(fA, r_cstruct(cs, :disc_copies))
        catch
          {:error, reason} ->
            local_fallback_error(clientPid, reason)
        else
          ns when is_list(ns) ->
            do_uninstall(clientPid, ns, fA)
        end

      {:error, reason} ->
        local_fallback_error(clientPid, reason)
    end
  end

  defp do_uninstall(clientPid, ns, fA) do
    args = [self(), fA]
    :global.set_lock({{:mnesia_table_lock, :schema}, self()}, ns, :infinity)

    pids =
      for n <- ns do
        :erlang.spawn_link(n, :mnesia_bup, :local_uninstall_fallback, args)
      end

    res = do_uninstall(clientPid, pids, [], [], :ok)

    :global.del_lock(
      {{:mnesia_table_lock, :schema}, self()},
      ns
    )

    send(clientPid, {self(), res})
    :erlang.unlink(clientPid)
    exit(:shutdown)
  end

  defp do_uninstall(clientPid, [pid | pids], goodPids, badNodes, res) do
    receive do
      {:EXIT, ^pid, reason} ->
        badNode = node(pid)
        badRes = {:error, {'Uninstall fallback', badNode, reason}}
        do_uninstall(clientPid, pids, goodPids, [badNode | badNodes], badRes)

      {^pid, {:error, reason}} ->
        badNode = node(pid)
        badRes = {:error, {'Uninstall fallback', badNode, reason}}
        do_uninstall(clientPid, pids, goodPids, [badNode | badNodes], badRes)

      {^pid, :started} ->
        do_uninstall(clientPid, pids, [pid | goodPids], badNodes, res)
    end
  end

  defp do_uninstall(clientPid, [], goodPids, [], :ok) do
    :lists.foreach(
      fn pid ->
        send(pid, {self(), :do_uninstall})
      end,
      goodPids
    )

    rec_uninstall(clientPid, goodPids, :ok)
  end

  defp do_uninstall(_ClientPid, [], goodPids, badNodes, badRes) do
    :lists.foreach(
      fn pid ->
        :erlang.exit(pid, :shutdown)
      end,
      goodPids
    )

    {:error, {:node_not_running, badNodes, badRes}}
  end

  def local_uninstall_fallback(master, fA) do
    :erlang.register(:mnesia_fallback, self())
    fA2 = check_fallback_dir(master, fA)
    send(master, {self(), :started})

    receive do
      {^master, :do_uninstall} ->
        :ok

        try do
          :mnesia_lib.set(:active_fallback, false)
        catch
          :error, _ ->
            :ok
        end

        tmp = r_fallback_args(fA2, :fallback_tmp)
        bup = r_fallback_args(fA2, :fallback_bup)
        :file.delete(tmp)
        res = :file.delete(bup)
        :erlang.unregister(:mnesia_fallback)
        :ok
        send(master, {self(), res})
        :erlang.unlink(master)
        exit(:normal)
    end
  end

  defp rec_uninstall(clientPid, [pid | pids], accRes) do
    receive do
      {:EXIT, ^pid, r} ->
        reason = {:node_not_running, {node(pid), r}}
        rec_uninstall(clientPid, pids, {:error, reason})

      {^pid, :ok} ->
        rec_uninstall(clientPid, pids, accRes)

      {^pid, badRes} ->
        rec_uninstall(clientPid, pids, badRes)
    end
  end

  defp rec_uninstall(_, [], res) do
    res
  end

  def traverse_backup(source, target, fun, acc) do
    mod = :mnesia_monitor.get_env(:backup_module)
    traverse_backup(source, mod, target, mod, fun, acc)
  end

  def traverse_backup(source, sourceMod, target, targetMod, fun, acc) do
    args = [self(), source, sourceMod, target, targetMod, fun, acc]
    pid = spawn_link(:mnesia_bup, :do_traverse_backup, args)

    receive do
      {:EXIT, ^pid, reason} ->
        {:error, {'Backup traversal crashed', reason}}

      {:iter_done, ^pid, res} ->
        res
    end
  end

  def do_traverse_backup(clientPid, source, sourceMod, target, targetMod, fun, acc) do
    :erlang.process_flag(:trap_exit, true)

    iter =
      cond do
        targetMod !== :read_only ->
          try do
            do_apply(targetMod, :open_write, [target], target)
          catch
            {:error, error} ->
              :erlang.unlink(clientPid)
              send(clientPid, {:iter_done, self(), {:error, error}})
              exit(error)
          end

        true ->
          :ignore
      end

    a = {:start, fun, acc, targetMod, iter}

    res =
      case iterate(sourceMod, &trav_apply/5, source, a) do
        {:ok, {:iter, _, acc2, _, iter2}}
        when targetMod !== :read_only ->
          try do
            do_apply(targetMod, :commit_write, [iter2], iter2)
            {:ok, acc2}
          catch
            {:error, reason} ->
              {:error, reason}
          end

        {:ok, {:iter, _, acc2, _, _}} ->
          {:ok, acc2}

        {:error, reason} when targetMod !== :read_only ->
          try do
            do_apply(targetMod, :abort_write, [iter], iter)
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end

          {:error, {'Backup traversal failed', reason}}

        {:error, reason} ->
          {:error, {'Backup traversal failed', reason}}
      end

    :erlang.unlink(clientPid)
    send(clientPid, {:iter_done, self(), res})
  end

  def trav_apply(recs, _Header, _Schema, _Ext, {:iter, fun, acc, mod, iter}) do
    {newRecs, acc2} = filter_foldl(fun, acc, recs)

    cond do
      mod !== :read_only and newRecs !== [] ->
        iter2 = do_apply(mod, :write, [iter, newRecs], iter)
        {:iter, fun, acc2, mod, iter2}

      true ->
        {:iter, fun, acc2, mod, iter}
    end
  end

  def trav_apply(recs, header, schema, ext, {:start, fun, acc, mod, iter}) do
    iter2 =
      cond do
        mod !== :read_only ->
          do_apply(mod, :write, [iter, [header]], iter)

        true ->
          iter
      end

    travAcc = trav_apply(schema, header, schema, ext, {:iter, fun, acc, mod, iter2})
    trav_apply(recs, header, schema, ext, travAcc)
  end

  defp filter_foldl(fun, acc, [head | tail]) do
    case fun.(head, acc) do
      {headItems, headAcc} when is_list(headItems) ->
        {tailItems, tailAcc} = filter_foldl(fun, headAcc, tail)
        {headItems ++ tailItems, tailAcc}

      other ->
        throw({:error, {'Fun must return a list', other}})
    end
  end

  defp filter_foldl(_Fun, acc, []) do
    {[], acc}
  end
end
