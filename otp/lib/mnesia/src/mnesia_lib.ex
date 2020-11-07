defmodule :m_mnesia_lib do
  use Bitwise
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

  def search_delete(obj, list) do
    search_delete(obj, list, [], :none)
  end

  defp search_delete(obj, [obj | tail], ack, _Res) do
    search_delete(obj, tail, ack, obj)
  end

  defp search_delete(obj, [h | t], ack, res) do
    search_delete(obj, t, [h | ack], res)
  end

  defp search_delete(_, [], ack, res) do
    {res, ack}
  end

  def key_search_delete(key, pos, tupleList) do
    key_search_delete(key, pos, tupleList, :none, [])
  end

  defp key_search_delete(key, pos, [h | t], _Obj, ack)
       when :erlang.element(pos, h) == key do
    key_search_delete(key, pos, t, h, ack)
  end

  defp key_search_delete(key, pos, [h | t], obj, ack) do
    key_search_delete(key, pos, t, obj, [h | ack])
  end

  defp key_search_delete(_, _, [], obj, ack) do
    {obj, ack}
  end

  def key_search_all(key, pos, tupleList) do
    key_search_all(key, pos, tupleList, [])
  end

  defp key_search_all(key, n, [h | t], ack)
       when :erlang.element(
              n,
              h
            ) == key do
    key_search_all(key, n, t, [h | ack])
  end

  defp key_search_all(key, n, [_ | t], ack) do
    key_search_all(key, n, t, ack)
  end

  defp key_search_all(_, _, [], ack) do
    ack
  end

  def intersect(l1, l2) do
    l2 -- l2 -- l1
  end

  def elems(i, [h | t]) do
    [:erlang.element(i, h) | elems(i, t)]
  end

  def elems(_, []) do
    []
  end

  def sort_commit(list) do
    sort_commit2(list, [])
  end

  defp sort_commit2([{:checkpoints, chkpL} | rest], acc) do
    [{:checkpoints, chkpL} | rest] ++ acc
  end

  defp sort_commit2([h | r], acc) do
    sort_commit2(r, [h | acc])
  end

  defp sort_commit2([], acc) do
    acc
  end

  def is_string([h | t]) do
    cond do
      0 <= h and h < 256 and is_integer(h) ->
        is_string(t)

      true ->
        false
    end
  end

  def is_string([]) do
    true
  end

  def union([h | l1], l2) do
    case :lists.member(h, l2) do
      true ->
        union(l1, l2)

      false ->
        [h | union(l1, l2)]
    end
  end

  def union([], l2) do
    l2
  end

  def uniq([]) do
    []
  end

  def uniq(list) do
    [h | t] = :lists.sort(list)
    uniq1(h, t, [])
  end

  defp uniq1(h, [h | r], ack) do
    uniq1(h, r, ack)
  end

  defp uniq1(old, [h | r], ack) do
    uniq1(h, r, [old | ack])
  end

  defp uniq1(old, [], ack) do
    [old | ack]
  end

  def to_list(x) when is_list(x) do
    x
  end

  def to_list(x) do
    :erlang.atom_to_list(x)
  end

  def all_nodes() do
    ns = :mnesia.system_info(:db_nodes) ++ :mnesia.system_info(:extra_db_nodes)
    :mnesia_lib.uniq(ns)
  end

  def running_nodes() do
    running_nodes(all_nodes())
  end

  def running_nodes(ns) do
    {replies, _BadNs} = :rpc.multicall(ns, :mnesia_lib, :is_running_remote, [])

    for {goodState, n} <- replies, goodState == true do
      n
    end
  end

  def is_running_remote() do
    isRunning = is_running()
    {isRunning == :yes, node()}
  end

  def is_running(node) when is_atom(node) do
    case :rpc.call(node, :mnesia_lib, :is_running, []) do
      {:badrpc, _} ->
        :no

      x ->
        x
    end
  end

  def is_running() do
    case (try do
            :ets.lookup_element(:mnesia_gvar, :mnesia_status, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        :no

      :running ->
        :yes

      :starting ->
        :starting

      :stopping ->
        :stopping
    end
  end

  def show(x) do
    show(x, [])
  end

  def show(f, a) do
    :io.format(:user, f, a)
  end

  def pad_name([char | chars], len, tail) do
    [char | pad_name(chars, len - 1, tail)]
  end

  def pad_name([], len, tail) when len <= 0 do
    tail
  end

  def pad_name([], len, tail) do
    [?\s | pad_name([], len - 1, tail)]
  end

  defp active_here(tab) do
    case val({tab, :where_to_read}) do
      node when node == node() ->
        true

      _ ->
        false
    end
  end

  def not_active_here(tab) do
    not active_here(tab)
  end

  def exists(fname) do
    :filelib.is_regular(fname)
  end

  def dir() do
    :mnesia_monitor.get_env(:dir)
  end

  def dir(fname) do
    :filename.join([dir(), to_list(fname)])
  end

  def tab2dat(tab) do
    dir(:lists.concat([tab, '.DAT']))
  end

  def tab2tmp(tab) do
    dir(:lists.concat([tab, '.TMP']))
  end

  def tab2dmp(tab) do
    dir(:lists.concat([tab, '.DMP']))
  end

  def tab2dcd(tab) do
    dir(:lists.concat([tab, '.DCD']))
  end

  def tab2dcl(tab) do
    dir(:lists.concat([tab, '.DCL']))
  end

  def tab2logtmp(tab) do
    dir(:lists.concat([tab, '.LOGTMP']))
  end

  def storage_type_at_node(node, tab) do
    search_key(
      node,
      [
        [
          {:disc_copies, val({tab, :disc_copies})},
          {:ram_copies, val({tab, :ram_copies})},
          {:disc_only_copies, val({tab, :disc_only_copies})}
        ]
        | wrap_external(val({tab, :external_copies}))
      ]
    )
  end

  def cs_to_storage_type(node, cs) do
    search_key(
      node,
      [
        [
          {:disc_copies, r_cstruct(cs, :disc_copies)},
          {:ram_copies, r_cstruct(cs, :ram_copies)},
          {:disc_only_copies, r_cstruct(cs, :disc_only_copies)}
        ]
        | wrap_external(r_cstruct(cs, :external_copies))
      ]
    )
  end

  def semantics({:ext, alias, mod}, item) do
    mod.semantics(alias, item)
  end

  def semantics({alias, mod}, item) do
    mod.semantics(alias, item)
  end

  def semantics(type, :storage)
      when type == :ram_copies or
             type == :disc_copies or
             type == :disc_only_copies do
    type
  end

  def semantics(type, :types)
      when type == :ram_copies or
             type == :disc_copies or
             type == :disc_only_copies do
    [:set, :ordered_set, :bag]
  end

  def semantics(:disc_only_copies, :index_types) do
    [:bag]
  end

  def semantics(type, :index_types)
      when type == :ram_copies or
             type == :disc_copies or
             type == :disc_only_copies do
    [:bag, :ordered]
  end

  def semantics(_, _) do
    :undefined
  end

  defp wrap_external(l) do
    for {{alias, mod}, ns} <- l do
      {{:ext, alias, mod}, ns}
    end
  end

  def schema_cs_to_storage_type(node, cs) do
    case cs_to_storage_type(node, cs) do
      :unknown when r_cstruct(cs, :name) == :schema ->
        :ram_copies

      other ->
        other
    end
  end

  defp search_key(key, [{val, list} | tail]) do
    case :lists.member(key, list) do
      true ->
        val

      false ->
        search_key(key, tail)
    end
  end

  defp search_key(_Key, []) do
    :unknown
  end

  def validate_key(tab, key) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :record_validation}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {recName, arity, type} ->
        {recName, arity, type}

      {recName, arity, type, alias, mod} ->
        mod.validate_key(alias, tab, recName, arity, type, key)

      {:EXIT, _} ->
        :mnesia.abort({:no_exists, tab})
    end
  end

  def validate_record(tab, obj) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :record_validation}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {recName, arity, type}
      when tuple_size(obj) == arity and
             recName == :erlang.element(1, obj) ->
        {recName, arity, type}

      {recName, arity, type, alias, mod}
      when tuple_size(obj) == arity and
             recName == :erlang.element(1, obj) ->
        mod.validate_record(alias, tab, recName, arity, type, obj)

      {:EXIT, _} ->
        :mnesia.abort({:no_exists, tab})

      _ ->
        :mnesia.abort({:bad_type, obj})
    end
  end

  def val(var) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, var, 2)
          catch
            :error, _ ->
              {:EXIT, __STACKTRACE__}
          end) do
      {:EXIT, stacktrace} ->
        other_val(var, stacktrace)

      _VaLuE_ ->
        _VaLuE_
    end
  end

  def set(var, val) do
    :ets.insert(:mnesia_gvar, {var, val})
  end

  def unset(var) do
    :ets.delete(:mnesia_gvar, var)
  end

  def other_val(var, stacktrace) do
    case other_val_1(var) do
      :error ->
        pr_other(var, stacktrace)

      val ->
        val
    end
  end

  defp other_val_1(var) do
    case var do
      {_, :where_to_read} ->
        :nowhere

      {_, :where_to_write} ->
        []

      {_, :active_replicas} ->
        []

      _ ->
        :error
    end
  end

  defp pr_other(var, stacktrace) do
    why =
      case is_running() do
        :no ->
          {:node_not_running, node()}

        _ ->
          {:no_exists, var}
      end

    verbose(
      '~p (~tp) val(mnesia_gvar, ~tw) -> ~p ~tp ~n',
      [self(), :erlang.process_info(self(), :registered_name), var, why, stacktrace]
    )

    :mnesia.abort(why)
  end

  def add(var, val) do
    l = val(var)
    set(var, [val | :lists.delete(val, l)])
  end

  def add_list(var, list) do
    l = val(var)
    set(var, union(l, list))
  end

  def del(var, val) do
    l = val(var)
    set(var, :lists.delete(val, l))
  end

  def add_lsort(var, val) when node() == val do
    l = val(var)
    set(var, [val | :lists.delete(val, l)])
  end

  def add_lsort(var, val) do
    case val(var) do
      [head | rest] when head == node() ->
        set(var, [head | lsort_add(val, rest)])

      list ->
        set(var, lsort_add(val, list))
    end
  end

  defp lsort_add(val, list) do
    case :ordsets.is_element(val, list) do
      true ->
        list

      false ->
        :ordsets.add_element(val, list)
    end
  end

  def ensure_loaded(appl) do
    case :application_controller.get_loaded(appl) do
      {true, _} ->
        :ok

      false ->
        case :application.load(appl) do
          :ok ->
            :ok

          {:error, {:already_loaded, ^appl}} ->
            :ok

          {:error, reason} ->
            {:error, {:application_load_error, reason}}
        end
    end
  end

  def local_active_tables() do
    tabs = val({:schema, :local_tables})

    :lists.zf(
      fn tab ->
        active_here(tab)
      end,
      tabs
    )
  end

  def active_tables() do
    tabs = val({:schema, :tables})

    f = fn tab ->
      case val({tab, :where_to_read}) do
        :nowhere ->
          false

        _ ->
          {true, tab}
      end
    end

    :lists.zf(f, tabs)
  end

  def etype(x) when is_integer(x) do
    :integer
  end

  def etype([]) do
    nil
  end

  def etype(x) when is_list(x) do
    :list
  end

  def etype(x) when is_tuple(x) do
    :tuple
  end

  def etype(x) when is_atom(x) do
    :atom
  end

  def etype(_) do
    :othertype
  end

  def remote_copy_holders(cs) do
    copy_holders(cs) -- [node()]
  end

  def copy_holders(cs) when r_cstruct(cs, :local_content) == false do
    cs_to_nodes(cs)
  end

  def copy_holders(cs) when r_cstruct(cs, :local_content) == true do
    case :lists.member(node(), cs_to_nodes(cs)) do
      true ->
        [node()]

      false ->
        []
    end
  end

  def set_remote_where_to_read(tab) do
    set_remote_where_to_read(tab, [])
  end

  def set_remote_where_to_read(tab, ignore) do
    active = val({tab, :active_replicas})

    valid =
      case :mnesia_recover.get_master_nodes(tab) do
        [] ->
          active

        masters ->
          :mnesia_lib.intersect(masters, active)
      end

    available =
      :mnesia_lib.intersect(
        val({:current, :db_nodes}),
        valid -- ignore
      )

    discOnlyC = val({tab, :disc_only_copies})
    prefered = available -- discOnlyC

    cond do
      prefered != [] ->
        set({tab, :where_to_read}, hd(prefered))

      available != [] ->
        set({tab, :where_to_read}, hd(available))

      true ->
        set({tab, :where_to_read}, :nowhere)
    end
  end

  def set_local_content_whereabouts(tab) do
    add({:schema, :local_tables}, tab)
    add({tab, :active_replicas}, node())
    set({tab, :where_to_write}, [node()])
    set({tab, :where_to_read}, node())
  end

  def create_counter(name) do
    set_counter(name, 0)
  end

  def set_counter(name, val) do
    :ets.insert(:mnesia_stats, {name, val})
  end

  def incr_counter(name) do
    :ets.update_counter(:mnesia_stats, name, 1)
  end

  def incr_counter(name, i) do
    :ets.update_counter(:mnesia_stats, name, i)
  end

  def read_counter(name) do
    :ets.lookup_element(:mnesia_stats, name, 2)
  end

  def cs_to_nodes(cs) do
    ext_nodes(r_cstruct(cs, :external_copies)) ++
      r_cstruct(cs, :disc_only_copies) ++
      r_cstruct(cs, :disc_copies) ++ r_cstruct(cs, :ram_copies)
  end

  defp ext_nodes(ext) do
    :lists.flatmap(
      fn {_, ns} ->
        ns
      end,
      ext
    )
  end

  defp overload_types() do
    [:mnesia_tm, :mnesia_dump_log]
  end

  defp valid_overload_type(t) do
    case :lists.member(t, overload_types()) do
      false ->
        :erlang.error(:bad_type)

      true ->
        true
    end
  end

  def overload_set(type, bool) when is_boolean(bool) do
    valid_overload_type(type)
    set({:overload, type}, bool)
  end

  def overload_read() do
    for t <- overload_types() do
      {t, overload_read(t)}
    end
  end

  def overload_read(t) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {:overload, t}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        valid_overload_type(t)
        false

      flag when is_boolean(flag) ->
        flag
    end
  end

  def dist_coredump() do
    dist_coredump(all_nodes())
  end

  defp dist_coredump(ns) do
    {replies, _} = :rpc.multicall(ns, :mnesia_lib, :coredump, [])
    replies
  end

  def coredump() do
    coredump({:crashinfo, {'user initiated~n', []}})
  end

  def coredump(crashInfo) do
    core = mkcore(crashInfo)
    out = core_file()
    important('Writing Mnesia core to file: ~tp...~tp~n', [out, crashInfo])
    _ = :file.write_file(out, core)
    out
  end

  def core_file() do
    integers = :erlang.tuple_to_list(:erlang.date()) ++ :erlang.tuple_to_list(:erlang.time())

    fun = fn
      i when i < 10 ->
        ['_0', i]

      i ->
        ['_', i]
    end

    list =
      :lists.append(
        for i <- integers do
          fun.(i)
        end
      )

    case :mnesia_monitor.get_env(:core_dir) do
      dir when is_list(dir) ->
        :filename.absname(
          :lists.concat(['MnesiaCore.', node()] ++ list),
          dir
        )

      _ ->
        :filename.absname(:lists.concat(['MnesiaCore.', node()] ++ list))
    end
  end

  def mkcore(crashInfo) do
    nodes = [node() | :erlang.nodes()]

    heldLocks =
      (fn ->
         try do
           :mnesia.system_info(:held_locks)
         catch
           _, _Reason ->
             {:EXIT, _Reason}
         end
       end).()

    core = [
      crashInfo,
      {:time, {:erlang.date(), :erlang.time()}},
      {:self, proc_dbg_info(self())},
      {:nodes,
       (fn ->
          try do
            :rpc.multicall(nodes, :mnesia_lib, :get_node_number, [])
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:applications,
       (fn ->
          try do
            :lists.sort(:application.loaded_applications())
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:flags,
       (fn ->
          try do
            :init.get_arguments()
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:code_path,
       (fn ->
          try do
            :code.get_path()
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:code_loaded,
       (fn ->
          try do
            :lists.sort(:code.all_loaded())
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:etsinfo,
       (fn ->
          try do
            ets_info(:ets.all())
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:version,
       (fn ->
          try do
            :mnesia.system_info(:version)
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:schema,
       (fn ->
          try do
            :ets.tab2list(:schema)
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:gvar,
       (fn ->
          try do
            :ets.tab2list(:mnesia_gvar)
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:master_nodes,
       (fn ->
          try do
            :mnesia_recover.get_master_node_info()
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:processes,
       (fn ->
          try do
            procs()
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:relatives,
       (fn ->
          try do
            relatives()
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:workers,
       (fn ->
          try do
            workers(:mnesia_controller.get_workers(2000))
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:locking_procs,
       (fn ->
          try do
            locking_procs(heldLocks)
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:held_locks, heldLocks},
      {:lock_queue,
       (fn ->
          try do
            :mnesia.system_info(:lock_queue)
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:load_info,
       (fn ->
          try do
            :mnesia_controller.get_info(2000)
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:trans_info,
       (fn ->
          try do
            :mnesia_tm.get_info(2000)
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:schema_file,
       (fn ->
          try do
            :file.read_file(tab2dat(:schema))
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:dir_info,
       (fn ->
          try do
            dir_info()
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()},
      {:logfile,
       (fn ->
          try do
            {:ok, read_log_files()}
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end
        end).()}
    ]

    :erlang.term_to_binary(core)
  end

  defp procs() do
    fun = fn p ->
      {p,
       try do
         :lists.zf(&proc_info/1, :erlang.process_info(p))
       catch
         _, _Reason ->
           {:EXIT, _Reason}
       end}
    end

    :lists.map(fun, :erlang.processes())
  end

  defp proc_info({:registered_name, val}) do
    {true, val}
  end

  defp proc_info({:message_queue_len, val}) do
    {true, val}
  end

  defp proc_info({:status, val}) do
    {true, val}
  end

  defp proc_info({:current_function, val}) do
    {true, val}
  end

  defp proc_info(_) do
    false
  end

  def get_node_number() do
    {node(), self()}
  end

  def have_majority(tab, haveNodes) do
    have_majority(tab, val({tab, :all_nodes}), haveNodes)
  end

  def have_majority(_Tab, allNodes, haveNodes) do
    missing = allNodes -- haveNodes
    present = allNodes -- missing
    length(present) > length(missing)
  end

  defp read_log_files() do
    for f <- :mnesia_log.log_files() do
      {f,
       try do
         :file.read_file(f)
       catch
         _, _Reason ->
           {:EXIT, _Reason}
       end}
    end
  end

  def dir_info() do
    {:ok, cwd} = :file.get_cwd()
    dir = dir()

    [{:cwd, cwd, :file.read_file_info(cwd)}, {:mnesia_dir, dir, :file.read_file_info(dir)}] ++
      case :file.list_dir(dir) do
        {:ok, files} ->
          for f <- files do
            {:mnesia_file, f,
             try do
               :file.read_file_info(dir(f))
             catch
               _, _Reason ->
                 {:EXIT, _Reason}
             end}
          end

        other ->
          [other]
      end
  end

  defp ets_info([h | t]) do
    [{:table, h, mk_info_tuple(:ets.info(h))} | ets_info(t)]
  end

  defp ets_info([]) do
    []
  end

  defp mk_info_tuple(t) when is_list(t) do
    :erlang.list_to_tuple(t)
  end

  defp mk_info_tuple(t) do
    t
  end

  defp relatives() do
    info = fn name ->
      case :erlang.whereis(name) do
        :undefined ->
          false

        pid ->
          {true, {name, pid, proc_dbg_info(pid)}}
      end
    end

    :lists.zf(info, :mnesia.ms())
  end

  defp workers({:workers, loaders, senders, dumper}) do
    info = fn
      {pid, {:send_table, tab, _Receiver, _St}} ->
        case pid do
          :undefined ->
            false

          ^pid ->
            {true, {pid, tab, proc_dbg_info(pid)}}
        end

      {pid, what} when is_pid(pid) ->
        {true, {pid, what, proc_dbg_info(pid)}}

      {name, pid} ->
        case pid do
          :undefined ->
            false

          ^pid ->
            {true, {name, pid, proc_dbg_info(pid)}}
        end
    end

    sInfo = :lists.zf(info, senders)
    linfo = :lists.zf(info, loaders)

    [
      [{:senders, sInfo}, {:loader, linfo}]
      | :lists.zf(
          info,
          [{:dumper, dumper}]
        )
    ]
  end

  defp locking_procs(lockList) when is_list(lockList) do
    tids =
      for lock <- lockList do
        :erlang.element(3, lock)
      end

    uT = uniq(tids)

    info = fn tid ->
      pid = r_tid(tid, :pid)

      case node(pid) == node() do
        true ->
          {true, {pid, proc_dbg_info(pid)}}

        _ ->
          false
      end
    end

    :lists.zf(info, uT)
  end

  defp proc_dbg_info(pid) do
    try do
      [
        :erlang.process_info(pid, :current_stacktrace)
        | :erlang.process_info(pid)
      ]
    catch
      _, r ->
        [{:process_info, :crashed, r}]
    end
  end

  def view() do
    bin = mkcore({:crashinfo, {'view only~n', []}})
    vcore(bin)
  end

  def view(file) do
    case suffix(['.DAT', '.RET', '.DMP', '.TMP'], file) do
      true ->
        view(file, :dat)

      false ->
        case suffix(['.LOG', '.BUP', '.ETS', '.LOGTMP'], file) do
          true ->
            view(file, :log)

          false ->
            case :lists.prefix('MnesiaCore.', file) do
              true ->
                view(file, :core)

              false ->
                {:error, 'Unknown file name'}
            end
        end
    end
  end

  def view(file, :dat) do
    :dets.view(file)
  end

  def view(file, :log) do
    :mnesia_log.view(file)
  end

  def view(file, :core) do
    vcore(file)
  end

  defp suffix(suffixes, file) do
    fun = fn s ->
      :lists.suffix(s, file)
    end

    :lists.any(fun, suffixes)
  end

  def vcore() do
    prefix = :lists.concat(['MnesiaCore.', node()])

    filter = fn f ->
      :lists.prefix(prefix, f)
    end

    {:ok, cwd} = :file.get_cwd()

    case :file.list_dir(cwd) do
      {:ok, files} ->
        coreFiles = :lists.sort(:lists.zf(filter, files))
        show('Mnesia core files: ~tp~n', [coreFiles])
        vcore(:lists.last(coreFiles))

      error ->
        error
    end
  end

  def vcore(bin) when is_binary(bin) do
    core = :erlang.binary_to_term(bin)

    fun = fn {item, info} ->
      show('***** ~tp *****~n', [item])

      case (fn ->
              try do
                vcore_elem({item, info})
              catch
                _, _Reason ->
                  {:EXIT, _Reason}
              end
            end).() do
        {:EXIT, reason} ->
          show('{\'EXIT\', ~tp}~n', [reason])

        _ ->
          :ok
      end
    end

    :lists.foreach(fun, core)
  end

  def vcore(file) do
    show('~n***** Mnesia core: ~tp *****~n', [file])

    case :file.read_file(file) do
      {:ok, bin} ->
        vcore(bin)

      _ ->
        :nocore
    end
  end

  defp vcore_elem({:schema_file, {:ok, b}}) do
    fname = '/tmp/schema.DAT'
    _ = :file.write_file(fname, b)
    _ = :dets.view(fname)
    _ = :file.delete(fname)
  end

  defp vcore_elem({:logfile, {:ok, binList}}) do
    fun = fn {f, info} ->
      show('----- logfile: ~tp -----~n', [f])

      case info do
        {:ok, b} ->
          fname = '/tmp/mnesia_vcore_elem.TMP'
          :file.write_file(fname, b)
          :mnesia_log.view(fname)
          :file.delete(fname)

        _ ->
          show('~tp~n', [info])
      end
    end

    :lists.foreach(fun, binList)
  end

  defp vcore_elem({:crashinfo, {format, args}}) do
    show(format, args)
  end

  defp vcore_elem({:gvar, l}) do
    show('~tp~n', [:lists.sort(l)])
  end

  defp vcore_elem({:transactions, info}) do
    :mnesia_tm.display_info(:user, info)
  end

  defp vcore_elem({_Item, info}) do
    show('~tp~n', [info])
  end

  def fix_error(x) do
    set(:last_error, x)

    case x do
      {:aborted, reason} ->
        reason

      {:abort, reason} ->
        reason

      y when is_atom(y) ->
        y

      {:EXIT, {_Reason, {mod, _, _}}} when is_atom(mod) ->
        save(x)

        case :erlang.atom_to_list(mod) do
          [[?m, ?n, ?e] | _] ->
            :badarg

          _ ->
            x
        end

      _ ->
        x
    end
  end

  def last_error() do
    val(:last_error)
  end

  def error_desc(:nested_transaction) do
    'Nested transactions are not allowed'
  end

  def error_desc(:badarg) do
    'Bad or invalid argument, possibly bad type'
  end

  def error_desc(:no_transaction) do
    'Operation not allowed outside transactions'
  end

  def error_desc(:combine_error) do
    'Table options were ilegally combined'
  end

  def error_desc(:bad_index) do
    'Index already exists or was out of bounds'
  end

  def error_desc(:already_exists) do
    'Some schema option we try to set is already on'
  end

  def error_desc(:index_exists) do
    'Some ops cannot  be performed on tabs with index'
  end

  def error_desc(:no_exists) do
    'Tried to perform op on non-existing (non alive) item'
  end

  def error_desc(:system_limit) do
    'Some system_limit was exhausted'
  end

  def error_desc(:mnesia_down) do
    'A transaction involving objects at some remote node which died while transaction was executing*and* object(s) are no longer available elsewherein the network'
  end

  def error_desc(:not_a_db_node) do
    'A node which is non existant in the schema was mentioned'
  end

  def error_desc(:bad_type) do
    'Bad type on some provided arguments'
  end

  def error_desc(:node_not_running) do
    'Node not running'
  end

  def error_desc(:truncated_binary_file) do
    'Truncated binary in file'
  end

  def error_desc(:active) do
    'Some delete ops require that all active objects are removed'
  end

  def error_desc(:illegal) do
    'Operation not supported on object'
  end

  def error_desc({:EXIT, reason}) do
    error_desc(reason)
  end

  def error_desc({:error, reason}) do
    error_desc(reason)
  end

  def error_desc({:aborted, reason}) do
    error_desc(reason)
  end

  def error_desc(reason)
      when is_tuple(reason) and
             :erlang.size(reason) > 0 do
    :erlang.setelement(1, reason, error_desc(:erlang.element(1, reason)))
  end

  def error_desc(reason) do
    reason
  end

  def dirty_rpc_error_tag(reason) do
    case reason do
      {:EXIT, _} ->
        :badarg

      :no_variable ->
        :badarg

      _ ->
        :no_exists
    end
  end

  def fatal(format, args) do
    try do
      try do
        set(:mnesia_status, :stopping)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end
    catch
      :error, _ ->
        :ok
    end

    core = mkcore({:crashinfo, {format, args}})
    report_fatal(format, args, core)
    :timer.sleep(10000)
    :mnesia.lkill()
    exit(:fatal)
  end

  def report_fatal(format, args) do
    report_fatal(format, args, :nocore)
  end

  defp report_fatal(format, args, core) do
    report_system_event({:mnesia_fatal, format, args, core})

    try do
      :erlang.exit(:erlang.whereis(:mnesia_monitor), :fatal)
    catch
      :error, _ ->
        :ok
    end
  end

  def random_time(retries, _Counter0) do
    upperLimit = 500
    dup = retries * retries
    maxIntv = trunc(upperLimit * (1 - 50 / (dup + 50)))
    dup + :rand.uniform(maxIntv)
  end

  def report_system_event(event0) do
    event = {:mnesia_system_event, event0}
    report_system_event(catch_notify(event), event)

    case (try do
            :ets.lookup_element(:mnesia_gvar, :subscribers, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        :ignore

      pids ->
        :lists.foreach(
          fn pid ->
            send(pid, event)
          end,
          pids
        )
    end

    :ok
  end

  defp catch_notify(event) do
    case :erlang.whereis(:mnesia_event) do
      :undefined ->
        {:EXIT, {:badarg, {:mnesia_event, event}}}

      pid ->
        :gen_event.notify(pid, event)
    end
  end

  defp report_system_event({:EXIT, reason}, event) do
    mod = :mnesia_monitor.get_env(:event_module)

    case :mnesia_sup.start_event() do
      {:ok, pid} ->
        :erlang.link(pid)
        :gen_event.call(:mnesia_event, mod, event, :infinity)
        :erlang.unlink(pid)

        receive do
          {:EXIT, ^pid, _Reason} ->
            :ok
        after
          0 ->
            :gen_event.stop(:mnesia_event)
        end

      error ->
        msg = 'Mnesia(~tp): Cannot report event ~tp: ~tp (~tp)~n'

        :error_logger.format(
          msg,
          [node(), event, reason, error]
        )
    end

    :ok
  end

  defp report_system_event(_Res, _Event) do
    :ok
  end

  def important(format, args) do
    save({format, args})
    report_system_event({:mnesia_info, format, args})
  end

  def warning(format, args) do
    save({format, args})
    report_system_event({:mnesia_warning, format, args})
  end

  def error(format, args) do
    save({format, args})
    report_system_event({:mnesia_error, format, args})
  end

  def verbose(format, args) do
    case :mnesia_monitor.get_env(:debug) do
      :none ->
        save({format, args})

      :verbose ->
        important(format, args)

      :debug ->
        important(format, args)

      :trace ->
        important(format, args)
    end
  end

  def dbg_out(format, args) do
    case :mnesia_monitor.get_env(:debug) do
      :none ->
        :ignore

      :verbose ->
        save({format, args})

      _ ->
        report_system_event({:mnesia_info, format, args})
    end
  end

  defp save(dbgInfo) do
    try do
      save2(dbgInfo)
    catch
      :error, _ ->
        :ok
    end
  end

  defp save2(dbgInfo) do
    key = {:"$$$_report", :current_pos}

    p =
      case :ets.lookup_element(:mnesia_gvar, key, 2) do
        30 ->
          -1

        i ->
          i
      end

    set({:"$$$_report", :current_pos}, p + 1)

    set(
      {:"$$$_report", p + 1},
      {:erlang.date(), :erlang.time(), dbgInfo}
    )
  end

  def copy_file(from, to) do
    case :file.open(from, [:raw, :binary, :read]) do
      {:ok, f} ->
        case :file.open(to, [:raw, :binary, :write]) do
          {:ok, t} ->
            res = copy_file_loop(f, t, 8000)
            :ok = :file.close(f)
            :ok = :file.close(t)
            res

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp copy_file_loop(f, t, chunkSize) do
    case :file.read(f, chunkSize) do
      {:ok, bin} ->
        :ok = :file.write(t, bin)
        copy_file_loop(f, t, chunkSize)

      :eof ->
        :ok

      {:error, reason} ->
        {:error, reason}
    end
  end

  def db_get(tab, key) do
    db_get(val({tab, :storage_type}), tab, key)
  end

  def db_get(:ram_copies, tab, key) do
    :ets.lookup(tab, key)
  end

  def db_get(:disc_copies, tab, key) do
    :ets.lookup(tab, key)
  end

  def db_get(:disc_only_copies, tab, key) do
    :dets.lookup(tab, key)
  end

  def db_get({:ext, alias, mod}, tab, key) do
    mod.lookup(alias, tab, key)
  end

  def db_init_chunk(tab) do
    db_init_chunk(val({tab, :storage_type}), tab, 1000)
  end

  def db_init_chunk(tab, n) do
    db_init_chunk(val({tab, :storage_type}), tab, n)
  end

  def db_init_chunk({:ext, alias, mod}, tab, n) do
    mod.select(alias, tab, [{:_, [], [:"$_"]}], n)
  end

  def db_init_chunk(:disc_only_copies, tab, n) do
    :dets.select(tab, [{:_, [], [:"$_"]}], n)
  end

  def db_init_chunk(_, tab, n) do
    :ets.select(tab, [{:_, [], [:"$_"]}], n)
  end

  def db_chunk({:ext, _Alias, mod}, state) do
    mod.select(state)
  end

  def db_chunk(:disc_only_copies, state) do
    :dets.select(state)
  end

  def db_chunk(_, state) do
    :ets.select(state)
  end

  def db_put(tab, val) do
    db_put(val({tab, :storage_type}), tab, val)
  end

  def db_put(:ram_copies, tab, val) do
    :ets.insert(tab, val)
    :ok
  end

  def db_put(:disc_copies, tab, val) do
    :ets.insert(tab, val)
    :ok
  end

  def db_put(:disc_only_copies, tab, val) do
    :dets.insert(tab, val)
  end

  def db_put({:ext, alias, mod}, tab, val) do
    mod.insert(alias, tab, val)
  end

  def db_match_object(tab, pat) do
    db_match_object(val({tab, :storage_type}), tab, pat)
  end

  def db_match_object(storage, tab, pat) do
    db_fixtable(storage, tab, true)

    try do
      case storage do
        :disc_only_copies ->
          :dets.match_object(tab, pat)

        {:ext, alias, mod} ->
          mod.select(alias, tab, [{pat, [], [:"$_"]}])

        _ ->
          :ets.match_object(tab, pat)
      end
    after
      db_fixtable(storage, tab, false)
    end
  end

  def db_foldl(fun, acc, tab) do
    db_foldl(val({tab, :storage_type}), fun, acc, tab)
  end

  def db_foldl(storage, fun, acc, tab) do
    limit = :mnesia_monitor.get_env(:fold_chunk_size)
    db_foldl(storage, fun, acc, tab, [{:_, [], [:"$_"]}], limit)
  end

  def db_foldl(:ram_copies, fun, acc, tab, pat, limit) do
    :mnesia_lib.db_fixtable(:ram_copies, tab, true)

    try do
      select_foldl(db_select_init(:ram_copies, tab, pat, limit), fun, acc, :ram_copies)
    after
      :mnesia_lib.db_fixtable(:ram_copies, tab, false)
    end
  end

  def db_foldl(storage, fun, acc, tab, pat, limit) do
    select_foldl(:mnesia_lib.db_select_init(storage, tab, pat, limit), fun, acc, storage)
  end

  defp select_foldl({objs, cont}, fun, acc, storage) do
    select_foldl(
      :mnesia_lib.db_select_cont(storage, cont, []),
      fun,
      :lists.foldl(fun, acc, objs),
      storage
    )
  end

  defp select_foldl(:"$end_of_table", _, acc, _) do
    acc
  end

  def db_select(tab, pat) do
    db_select(val({tab, :storage_type}), tab, pat)
  end

  def db_select(storage, tab, pat) do
    db_fixtable(storage, tab, true)

    try do
      case storage do
        :disc_only_copies ->
          :dets.select(tab, pat)

        {:ext, alias, mod} ->
          mod.select(alias, tab, pat)

        _ ->
          :ets.select(tab, pat)
      end
    after
      db_fixtable(storage, tab, false)
    end
  end

  def db_select_init({:ext, alias, mod}, tab, pat, limit) do
    mod.select(alias, tab, pat, limit)
  end

  def db_select_init(:disc_only_copies, tab, pat, limit) do
    :dets.select(tab, pat, limit)
  end

  def db_select_init(_, tab, pat, limit) do
    :ets.select(tab, pat, limit)
  end

  def db_select_cont({:ext, _Alias, mod}, cont0, ms) do
    cont = mod.repair_continuation(cont0, ms)
    mod.select(cont)
  end

  def db_select_cont(:disc_only_copies, cont0, ms) do
    cont = :dets.repair_continuation(cont0, ms)
    :dets.select(cont)
  end

  def db_select_cont(_, cont0, ms) do
    cont = :ets.repair_continuation(cont0, ms)
    :ets.select(cont)
  end

  def db_fixtable(:ets, tab, bool) do
    :ets.safe_fixtable(tab, bool)
  end

  def db_fixtable(:ram_copies, tab, bool) do
    :ets.safe_fixtable(tab, bool)
  end

  def db_fixtable(:disc_copies, tab, bool) do
    :ets.safe_fixtable(tab, bool)
  end

  def db_fixtable(:dets, tab, bool) do
    :dets.safe_fixtable(tab, bool)
  end

  def db_fixtable(:disc_only_copies, tab, bool) do
    :dets.safe_fixtable(tab, bool)
  end

  def db_fixtable({:ext, alias, mod}, tab, bool) do
    mod.fixtable(alias, tab, bool)
  end

  def db_erase(tab, key) do
    db_erase(val({tab, :storage_type}), tab, key)
  end

  def db_erase(:ram_copies, tab, key) do
    :ets.delete(tab, key)
    :ok
  end

  def db_erase(:disc_copies, tab, key) do
    :ets.delete(tab, key)
    :ok
  end

  def db_erase(:disc_only_copies, tab, key) do
    :dets.delete(tab, key)
  end

  def db_erase({:ext, alias, mod}, tab, key) do
    mod.delete(alias, tab, key)
    :ok
  end

  def db_match_erase(tab, :_) do
    db_delete_all(val({tab, :storage_type}), tab)
  end

  def db_match_erase(tab, pat) do
    db_match_erase(val({tab, :storage_type}), tab, pat)
  end

  def db_match_erase(:ram_copies, tab, pat) do
    :ets.match_delete(tab, pat)
    :ok
  end

  def db_match_erase(:disc_copies, tab, pat) do
    :ets.match_delete(tab, pat)
    :ok
  end

  def db_match_erase(:disc_only_copies, tab, pat) do
    :dets.match_delete(tab, pat)
  end

  def db_match_erase({:ext, alias, mod}, tab, pat) do
    mod.match_delete(alias, tab, pat)
    :ok
  end

  defp db_delete_all(:ram_copies, tab) do
    :ets.delete_all_objects(tab)
  end

  defp db_delete_all(:disc_copies, tab) do
    :ets.delete_all_objects(tab)
  end

  defp db_delete_all(:disc_only_copies, tab) do
    :dets.delete_all_objects(tab)
  end

  def db_first(tab) do
    db_first(val({tab, :storage_type}), tab)
  end

  def db_first(:ram_copies, tab) do
    :ets.first(tab)
  end

  def db_first(:disc_copies, tab) do
    :ets.first(tab)
  end

  def db_first(:disc_only_copies, tab) do
    :dets.first(tab)
  end

  def db_first({:ext, alias, mod}, tab) do
    mod.first(alias, tab)
  end

  def db_next_key(tab, key) do
    db_next_key(val({tab, :storage_type}), tab, key)
  end

  def db_next_key(:ram_copies, tab, key) do
    :ets.next(tab, key)
  end

  def db_next_key(:disc_copies, tab, key) do
    :ets.next(tab, key)
  end

  def db_next_key(:disc_only_copies, tab, key) do
    :dets.next(tab, key)
  end

  def db_next_key({:ext, alias, mod}, tab, key) do
    mod.next(alias, tab, key)
  end

  def db_last(tab) do
    db_last(val({tab, :storage_type}), tab)
  end

  def db_last(:ram_copies, tab) do
    :ets.last(tab)
  end

  def db_last(:disc_copies, tab) do
    :ets.last(tab)
  end

  def db_last(:disc_only_copies, tab) do
    :dets.first(tab)
  end

  def db_last({:ext, alias, mod}, tab) do
    mod.last(alias, tab)
  end

  def db_prev_key(tab, key) do
    db_prev_key(val({tab, :storage_type}), tab, key)
  end

  def db_prev_key(:ram_copies, tab, key) do
    :ets.prev(tab, key)
  end

  def db_prev_key(:disc_copies, tab, key) do
    :ets.prev(tab, key)
  end

  def db_prev_key(:disc_only_copies, tab, key) do
    :dets.next(tab, key)
  end

  def db_prev_key({:ext, alias, mod}, tab, key) do
    mod.prev(alias, tab, key)
  end

  def db_slot(tab, pos) do
    db_slot(val({tab, :storage_type}), tab, pos)
  end

  def db_slot(:ram_copies, tab, pos) do
    :ets.slot(tab, pos)
  end

  def db_slot(:disc_copies, tab, pos) do
    :ets.slot(tab, pos)
  end

  def db_slot(:disc_only_copies, tab, pos) do
    :dets.slot(tab, pos)
  end

  def db_slot({:ext, alias, mod}, tab, pos) do
    mod.slot(alias, tab, pos)
  end

  def db_update_counter(tab, c, val) do
    db_update_counter(val({tab, :storage_type}), tab, c, val)
  end

  def db_update_counter(:ram_copies, tab, c, val) do
    :ets.update_counter(tab, c, val)
  end

  def db_update_counter(:disc_copies, tab, c, val) do
    :ets.update_counter(tab, c, val)
  end

  def db_update_counter(:disc_only_copies, tab, c, val) do
    :dets.update_counter(tab, c, val)
  end

  def db_update_counter({:ext, alias, mod}, tab, c, val) do
    mod.update_counter(alias, tab, c, val)
  end

  def db_erase_tab(tab) do
    db_erase_tab(val({tab, :storage_type}), tab)
  end

  def db_erase_tab(:ram_copies, tab) do
    :ets.delete(tab)
  end

  def db_erase_tab(:disc_copies, tab) do
    :ets.delete(tab)
  end

  def db_erase_tab(:disc_only_copies, _Tab) do
    :ignore
  end

  def db_erase_tab({:ext, _Alias, _Mod}, _Tab) do
    :ignore
  end

  def dets_to_ets(tabname, tab, file, type, rep, lock) do
    {open, close} = mkfuns(lock)

    case open.(
           tabname,
           [{:file, file}, {:type, disk_type(tab, type)}, {:keypos, 2}, {:repair, rep}]
         ) do
      {:ok, ^tabname} ->
        res = :dets.to_ets(tabname, tab)
        :ok = close.(tabname)
        trav_ret(res, tab)

      other ->
        other
    end
  end

  defp trav_ret(tabname, tabname) do
    :loaded
  end

  defp trav_ret(other, _Tabname) do
    other
  end

  defp mkfuns(:yes) do
    {fn tab, args ->
       dets_sync_open(tab, args)
     end,
     fn tab ->
       dets_sync_close(tab)
     end}
  end

  defp mkfuns(:no) do
    {fn tab, args ->
       :dets.open_file(tab, args)
     end,
     fn tab ->
       :dets.close(tab)
     end}
  end

  def disk_type(tab) do
    disk_type(tab, val({tab, :setorbag}))
  end

  def disk_type(_Tab, :ordered_set) do
    :set
  end

  def disk_type(_, type) do
    type
  end

  def dets_sync_open(tab, ref, file) do
    args = [
      {:file, file},
      {:keypos, 2},
      {:repair, :mnesia_monitor.get_env(:auto_repair)},
      {:type, disk_type(tab)}
    ]

    dets_sync_open(ref, args)
  end

  def lock_table(tab) do
    :global.set_lock({{:mnesia_table_lock, tab}, self()}, [node()], :infinity)
  end

  def unlock_table(tab) do
    :global.del_lock(
      {{:mnesia_table_lock, tab}, self()},
      [node()]
    )
  end

  def dets_sync_open(tab, args) do
    lock_table(tab)

    case :dets.open_file(tab, args) do
      {:ok, ^tab} ->
        {:ok, tab}

      other ->
        dets_sync_close(tab)
        other
    end
  end

  def dets_sync_close(tab) do
    try do
      :dets.close(tab)
    catch
      :error, _ ->
        :ok
    end

    unlock_table(tab)
    :ok
  end

  def readable_indecies(tab) do
    val({tab, :index})
  end

  Record.defrecord(:r_debug_info, :debug_info,
    id: :undefined,
    function: :undefined,
    context: :undefined,
    file: :undefined,
    line: :undefined
  )

  def scratch_debug_fun() do
    dbg_out('scratch_debug_fun(): ~p~n', [:mnesia_debug])

    try do
      :ets.delete(:mnesia_debug)
    catch
      :error, _ ->
        :ok
    end

    _ =
      :ets.new(
        :mnesia_debug,
        [:set, :public, :named_table, {:keypos, 2}]
      )
  end

  def activate_debug_fun(funId, fun, initialContext, file, line) do
    info = r_debug_info(id: funId, function: fun, context: initialContext, file: file, line: line)
    update_debug_info(info)
  end

  defp update_debug_info(info) do
    try do
      :ets.insert(:mnesia_debug, info)
      :ok
    catch
      :error, _ ->
        scratch_debug_fun()
        :ets.insert(:mnesia_debug, info)
    end

    dbg_out('update_debug_info(~p)~n', [info])
    :ok
  end

  def deactivate_debug_fun(funId, _File, _Line) do
    try do
      :ets.delete(:mnesia_debug, funId)
    catch
      :error, _ ->
        :ok
    end

    :ok
  end

  def eval_debug_fun(funId, evalContext, evalFile, evalLine) do
    try do
      case :ets.lookup(:mnesia_debug, funId) do
        [] ->
          :ok

        [info] ->
          oldContext = r_debug_info(info, :context)

          dbg_out(
            '~s(~p): ~w activated in ~s(~p)~n  eval_debug_fun(~w, ~w)~n',
            [
              :filename.basename(evalFile),
              evalLine,
              r_debug_info(info, :id),
              :filename.basename(r_debug_info(info, :file)),
              r_debug_info(info, :line),
              oldContext,
              evalContext
            ]
          )

          fun = r_debug_info(info, :function)
          newContext = fun.(oldContext, evalContext)

          case :ets.lookup(:mnesia_debug, funId) do
            [^info] when newContext != oldContext ->
              newInfo = r_debug_info(info, context: newContext)
              update_debug_info(newInfo)

            _ ->
              :ok
          end
      end
    catch
      _, _ ->
        :ok
    end
  end

  def is_debug_compiled() do
    false
  end
end
