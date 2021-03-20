defmodule :m_mnesia_frag do
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

  Record.defrecord(:r_frag_state, :frag_state,
    foreign_key: :undefined,
    n_fragments: :undefined,
    hash_module: :undefined,
    hash_state: :undefined
  )

  def lock(activityId, opaque, {:table, tab}, lockKind) do
    case frag_names(tab) do
      [^tab] ->
        :mnesia.lock(activityId, opaque, {:table, tab}, lockKind)

      frags ->
        deepNs =
          for f <- frags do
            :mnesia.lock(activityId, opaque, {:table, f}, lockKind)
          end

        :mnesia_lib.uniq(:lists.append(deepNs))
    end
  end

  def lock(activityId, opaque, lockItem, lockKind) do
    :mnesia.lock(activityId, opaque, lockItem, lockKind)
  end

  def write(activityId, opaque, tab, rec, lockKind) do
    frag = record_to_frag_name(tab, rec)
    :mnesia.write(activityId, opaque, frag, rec, lockKind)
  end

  def delete(activityId, opaque, tab, key, lockKind) do
    frag = key_to_frag_name(tab, key)
    :mnesia.delete(activityId, opaque, frag, key, lockKind)
  end

  def delete_object(activityId, opaque, tab, rec, lockKind) do
    frag = record_to_frag_name(tab, rec)
    :mnesia.delete_object(activityId, opaque, frag, rec, lockKind)
  end

  def read(activityId, opaque, tab, key, lockKind) do
    frag = key_to_frag_name(tab, key)
    :mnesia.read(activityId, opaque, frag, key, lockKind)
  end

  def match_object(activityId, opaque, tab, headPat, lockKind) do
    matchSpec = [{headPat, [], [:"$_"]}]
    select(activityId, opaque, tab, matchSpec, lockKind)
  end

  def select(activityId, opaque, tab, matchSpec, lockKind) do
    do_select(activityId, opaque, tab, matchSpec, lockKind)
  end

  def select(activityId, opaque, tab, matchSpec, limit, lockKind) do
    init_select(activityId, opaque, tab, matchSpec, limit, lockKind)
  end

  def all_keys(activityId, opaque, tab, lockKind) do
    match =
      for frag <- frag_names(tab) do
        :mnesia.all_keys(activityId, opaque, frag, lockKind)
      end

    :lists.append(match)
  end

  def clear_table(activityId, opaque, tab, obj) do
    for frag <- frag_names(tab) do
      :mnesia.clear_table(activityId, opaque, frag, obj)
    end

    :ok
  end

  def index_match_object(activityId, opaque, tab, pat, attr, lockKind) do
    match =
      for frag <- frag_names(tab) do
        :mnesia.index_match_object(activityId, opaque, frag, pat, attr, lockKind)
      end

    :lists.append(match)
  end

  def index_read(activityId, opaque, tab, key, attr, lockKind) do
    match =
      for frag <- frag_names(tab) do
        :mnesia.index_read(activityId, opaque, frag, key, attr, lockKind)
      end

    :lists.append(match)
  end

  def foldl(activityId, opaque, fun, acc, tab, lockKind) do
    fun2 = fn frag, a ->
      :mnesia.foldl(activityId, opaque, fun, a, frag, lockKind)
    end

    :lists.foldl(fun2, acc, frag_names(tab))
  end

  def foldr(activityId, opaque, fun, acc, tab, lockKind) do
    fun2 = fn frag, a ->
      :mnesia.foldr(activityId, opaque, fun, a, frag, lockKind)
    end

    :lists.foldr(fun2, acc, frag_names(tab))
  end

  def table_info(activityId, opaque, {tab, key}, item) do
    frag = key_to_frag_name(tab, key)
    table_info2(activityId, opaque, tab, frag, item)
  end

  def table_info(activityId, opaque, tab, item) do
    table_info2(activityId, opaque, tab, tab, item)
  end

  defp table_info2(activityId, opaque, tab, frag, item) do
    case item do
      :size ->
        sumFun = fn {_, size}, acc ->
          acc + size
        end

        :lists.foldl(sumFun, 0, frag_size(activityId, opaque, tab))

      :memory ->
        sumFun = fn {_, size}, acc ->
          acc + size
        end

        :lists.foldl(sumFun, 0, frag_memory(activityId, opaque, tab))

      :base_table ->
        lookup_prop(tab, :base_table)

      :node_pool ->
        lookup_prop(tab, :node_pool)

      :n_fragments ->
        fH = lookup_frag_hash(tab)
        r_frag_state(fH, :n_fragments)

      :foreign_key ->
        fH = lookup_frag_hash(tab)
        r_frag_state(fH, :foreign_key)

      :foreigners ->
        lookup_foreigners(tab)

      :n_ram_copies ->
        length(val({tab, :ram_copies}))

      :n_disc_copies ->
        length(val({tab, :disc_copies}))

      :n_disc_only_copies ->
        length(val({tab, :disc_only_copies}))

      :n_external_copies ->
        length(val({tab, :external_copies}))

      :frag_names ->
        frag_names(tab)

      :frag_dist ->
        frag_dist(tab)

      :frag_size ->
        frag_size(activityId, opaque, tab)

      :frag_memory ->
        frag_memory(activityId, opaque, tab)

      _ ->
        :mnesia.table_info(activityId, opaque, frag, item)
    end
  end

  def first(activityId, opaque, tab) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :frag_hash}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        :mnesia.first(activityId, opaque, tab)

      fH ->
        firstFrag = tab

        case :mnesia.first(activityId, opaque, firstFrag) do
          :"$end_of_table" ->
            search_first(activityId, opaque, tab, 1, fH)

          next ->
            next
        end
    end
  end

  defp search_first(activityId, opaque, tab, n, fH)
       when n < r_frag_state(fH, :n_fragments) do
    nextN = n + 1
    nextFrag = n_to_frag_name(tab, nextN)

    case :mnesia.first(activityId, opaque, nextFrag) do
      :"$end_of_table" ->
        search_first(activityId, opaque, tab, nextN, fH)

      next ->
        next
    end
  end

  defp search_first(_ActivityId, _Opaque, _Tab, _N, _FH) do
    :"$end_of_table"
  end

  def last(activityId, opaque, tab) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :frag_hash}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        :mnesia.last(activityId, opaque, tab)

      fH ->
        lastN = r_frag_state(fH, :n_fragments)
        search_last(activityId, opaque, tab, lastN, fH)
    end
  end

  defp search_last(activityId, opaque, tab, n, fH) when n >= 1 do
    frag = n_to_frag_name(tab, n)

    case :mnesia.last(activityId, opaque, frag) do
      :"$end_of_table" ->
        prevN = n - 1
        search_last(activityId, opaque, tab, prevN, fH)

      prev ->
        prev
    end
  end

  defp search_last(_ActivityId, _Opaque, _Tab, _N, _FH) do
    :"$end_of_table"
  end

  def prev(activityId, opaque, tab, key) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :frag_hash}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        :mnesia.prev(activityId, opaque, tab, key)

      fH ->
        n = key_to_n(fH, key)
        frag = n_to_frag_name(tab, n)

        case :mnesia.prev(activityId, opaque, frag, key) do
          :"$end_of_table" ->
            search_prev(activityId, opaque, tab, n)

          prev ->
            prev
        end
    end
  end

  defp search_prev(activityId, opaque, tab, n) when n > 1 do
    prevN = n - 1
    prevFrag = n_to_frag_name(tab, prevN)

    case :mnesia.last(activityId, opaque, prevFrag) do
      :"$end_of_table" ->
        search_prev(activityId, opaque, tab, prevN)

      prev ->
        prev
    end
  end

  defp search_prev(_ActivityId, _Opaque, _Tab, _N) do
    :"$end_of_table"
  end

  def next(activityId, opaque, tab, key) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :frag_hash}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        :mnesia.next(activityId, opaque, tab, key)

      fH ->
        n = key_to_n(fH, key)
        frag = n_to_frag_name(tab, n)

        case :mnesia.next(activityId, opaque, frag, key) do
          :"$end_of_table" ->
            search_next(activityId, opaque, tab, n, fH)

          prev ->
            prev
        end
    end
  end

  defp search_next(activityId, opaque, tab, n, fH)
       when n < r_frag_state(fH, :n_fragments) do
    nextN = n + 1
    nextFrag = n_to_frag_name(tab, nextN)

    case :mnesia.first(activityId, opaque, nextFrag) do
      :"$end_of_table" ->
        search_next(activityId, opaque, tab, nextN, fH)

      next ->
        next
    end
  end

  defp search_next(_ActivityId, _Opaque, _Tab, _N, _FH) do
    :"$end_of_table"
  end

  defp frag_size(activityId, opaque, tab) do
    for f <- frag_names(tab) do
      {f, remote_table_info(activityId, opaque, f, :size)}
    end
  end

  defp frag_memory(activityId, opaque, tab) do
    for f <- frag_names(tab) do
      {f, remote_table_info(activityId, opaque, f, :memory)}
    end
  end

  defp remote_table_info(activityId, opaque, tab, item) do
    n = val({tab, :where_to_read})

    case :rpc.call(n, :mnesia, :table_info, [activityId, opaque, tab, item]) do
      {:badrpc, _} ->
        :mnesia.abort({:no_exists, tab, item})

      info ->
        info
    end
  end

  defp init_select(tid, opaque, tab, pat, limit, lockKind) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :frag_hash}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        :mnesia.select(tid, opaque, tab, pat, limit, lockKind)

      fH ->
        fragNumbers = verify_numbers(fH, pat)

        fun = fn num ->
          name = n_to_frag_name(tab, num)
          node = val({name, :where_to_read})
          storage = :mnesia_lib.storage_type_at_node(node, name)
          :mnesia.lock(tid, opaque, {:table, name}, lockKind)
          {name, node, storage}
        end

        [{fTab, node, type} | nameNodes] =
          :lists.map(
            fun,
            fragNumbers
          )

        initFun = fn fixedSpec ->
          :mnesia.dirty_sel_init(node, fTab, fixedSpec, limit, type)
        end

        res =
          :mnesia.fun_select(tid, opaque, fTab, pat, lockKind, fTab, initFun, limit, node, type)

        frag_sel_cont(res, nameNodes, {pat, lockKind, limit})
    end
  end

  def select_cont(_Tid, _, {:frag_cont, :"$end_of_table", [], _}) do
    :"$end_of_table"
  end

  def select_cont(tid, ts, {:frag_cont, :"$end_of_table", [{tab, node, type} | rest], args}) do
    {spec, lockKind, limit} = args

    initFun = fn fixedSpec ->
      :mnesia.dirty_sel_init(node, tab, fixedSpec, limit, type)
    end

    res = :mnesia.fun_select(tid, ts, tab, spec, lockKind, tab, initFun, limit, node, type)
    frag_sel_cont(res, rest, args)
  end

  def select_cont(tid, ts, {:frag_cont, cont, tabL, args}) do
    frag_sel_cont(:mnesia.select_cont(tid, ts, cont), tabL, args)
  end

  def select_cont(tid, ts, else__) do
    :mnesia.select_cont(tid, ts, else__)
  end

  defp frag_sel_cont(:"$end_of_table", [], _) do
    :"$end_of_table"
  end

  defp frag_sel_cont(:"$end_of_table", tabL, args) do
    {[], {:frag_cont, :"$end_of_table", tabL, args}}
  end

  defp frag_sel_cont({recs, cont}, tabL, args) do
    {recs, {:frag_cont, cont, tabL, args}}
  end

  defp do_select(activityId, opaque, tab, matchSpec, lockKind) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :frag_hash}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        :mnesia.select(activityId, opaque, tab, matchSpec, lockKind)

      fH ->
        fragNumbers = verify_numbers(fH, matchSpec)

        fun = fn num ->
          name = n_to_frag_name(tab, num)
          node = val({name, :where_to_read})
          :mnesia.lock(activityId, opaque, {:table, name}, lockKind)
          {name, node}
        end

        nameNodes = :lists.map(fun, fragNumbers)

        selectAllFun = fn patchedMatchSpec ->
          match =
            for {name, _Node} <- nameNodes do
              :mnesia.dirty_select(
                name,
                patchedMatchSpec
              )
            end

          :lists.append(match)
        end

        case (for {name, node} <- nameNodes, node != node() do
                {name, node}
              end) do
          [] ->
            :mnesia.fun_select(activityId, opaque, tab, matchSpec, :none, :_, selectAllFun)

          remoteNameNodes ->
            type = val({tab, :setorbag})

            selectFun = fn patchedMatchSpec ->
              ref = make_ref()
              args = [self(), ref, remoteNameNodes, patchedMatchSpec]
              pid = spawn_link(:mnesia_frag, :local_select, args)

              localMatch0 =
                for {name, node} <- nameNodes,
                    node == node() do
                  :mnesia.dirty_select(
                    name,
                    patchedMatchSpec
                  )
                end

              localMatch =
                case type do
                  :ordered_set ->
                    :lists.merge(localMatch0)

                  _ ->
                    :lists.append(localMatch0)
                end

              oldSelectFun = fn ->
                selectAllFun.(patchedMatchSpec)
              end

              local_collect(ref, pid, type, localMatch, oldSelectFun)
            end

            :mnesia.fun_select(activityId, opaque, tab, matchSpec, :none, :_, selectFun)
        end
    end
  end

  defp verify_numbers(fH, matchSpec) do
    hashState = r_frag_state(fH, :hash_state)

    fragNumbers =
      case r_frag_state(fH, :hash_module) do
        hashMod when hashMod == :mnesia_frag_hash ->
          :mnesia_frag_hash.match_spec_to_frag_numbers(
            hashState,
            matchSpec
          )

        hashMod ->
          hashMod.match_spec_to_frag_numbers(hashState, matchSpec)
      end

    n = r_frag_state(fH, :n_fragments)

    verifyFun = fn
      f
      when is_integer(f) and f >= 1 and
             f <= n ->
        false

      _F ->
        true
    end

    try do
      frags = :lists.filter(verifyFun, fragNumbers)
      frags == [] or :erlang.error(frags)
      fragNumbers
    catch
      :error, badFrags ->
        :mnesia.abort(
          {'match_spec_to_frag_numbers: Fragment numbers out of range', badFrags, {:range, 1, n}}
        )
    end
  end

  def local_select(replyTo, ref, remoteNameNodes, matchSpec) do
    remoteNodes =
      :mnesia_lib.uniq(
        for {_Name, node} <- remoteNameNodes do
          node
        end
      )

    args = [replyTo, ref, remoteNameNodes, matchSpec]
    {replies, badNodes} = :rpc.multicall(remoteNodes, :mnesia_frag, :remote_select, args)

    case :mnesia_lib.uniq(replies) -- [:ok] do
      [] when badNodes == [] ->
        send(replyTo, {:local_select, ref, :ok})

      _ when badNodes != [] ->
        send(replyTo, {:local_select, ref, {:error, {:node_not_running, hd(badNodes)}}})

      [{:badrpc, {:EXIT, reason}} | _] ->
        send(replyTo, {:local_select, ref, {:error, reason}})

      [reason | _] ->
        send(replyTo, {:local_select, ref, {:error, reason}})
    end

    :erlang.unlink(replyTo)
    exit(:normal)
  end

  def remote_select(replyTo, ref, nameNodes, matchSpec) do
    do_remote_select(replyTo, ref, nameNodes, matchSpec)
  end

  defp do_remote_select(replyTo, ref, [{name, node} | nameNodes], matchSpec) do
    cond do
      node == node() ->
        res =
          try do
            {:ok, :mnesia.dirty_select(name, matchSpec)}
          catch
            _, _Reason ->
              {:EXIT, _Reason}
          end

        send(replyTo, {:remote_select, ref, node, res})
        do_remote_select(replyTo, ref, nameNodes, matchSpec)

      true ->
        do_remote_select(replyTo, ref, nameNodes, matchSpec)
    end
  end

  defp do_remote_select(_ReplyTo, _Ref, [], _MatchSpec) do
    :ok
  end

  defp local_collect(ref, pid, type, localMatch, oldSelectFun) do
    receive do
      {:local_select, ^ref, :ok} ->
        remote_collect_ok(ref, type, localMatch, oldSelectFun)

      {:local_select, ^ref, {:error, reason}} ->
        remote_collect_error(ref, type, reason, oldSelectFun)

      {:EXIT, ^pid, reason} ->
        remote_collect_error(ref, type, reason, oldSelectFun)
    end
  end

  defp remote_collect_ok(ref, type, acc, oldSelectFun) do
    receive do
      {:remote_select, ^ref, node, remoteRes} ->
        case remoteRes do
          {:ok, remoteMatch} ->
            matches =
              case type do
                :ordered_set ->
                  :lists.merge(remoteMatch, acc)

                _ ->
                  remoteMatch ++ acc
              end

            remote_collect_ok(ref, type, matches, oldSelectFun)

          _ ->
            reason = {:node_not_running, node}
            remote_collect_error(ref, type, reason, oldSelectFun)
        end
    after
      0 ->
        acc
    end
  end

  defp remote_collect_error(ref, type, reason, oldSelectFun) do
    receive do
      {:remote_select, ^ref, _Node, _RemoteRes} ->
        remote_collect_error(ref, type, reason, oldSelectFun)
    after
      0 ->
        :mnesia.abort({:error, reason})
    end
  end

  def expand_cstruct(cs) do
    expand_cstruct(cs, :create)
  end

  defp expand_cstruct(cs, mode) do
    tab = r_cstruct(cs, :name)
    props = r_cstruct(cs, :frag_properties)
    :mnesia_schema.verify({:alt, [nil, :list]}, :mnesia_lib.etype(props), {:badarg, tab, props})

    validKeys = [
      :foreign_key,
      :n_fragments,
      :node_pool,
      :n_ram_copies,
      :n_disc_copies,
      :n_disc_only_copies,
      :n_external_copies,
      :hash_module,
      :hash_state
    ]

    keys = :mnesia_schema.check_keys(tab, props, validKeys)
    :mnesia_schema.check_duplicates(tab, keys)
    foreignKey = :mnesia_schema.pick(tab, :foreign_key, props, :undefined)

    {foreignKey2, n, pool, defaultNR, defaultND, defaultNDO, defaultNExt} =
      pick_props(tab, cs, foreignKey)

    badPool = {:bad_type, tab, {:node_pool, pool}}
    :mnesia_schema.verify(:list, :mnesia_lib.etype(pool), badPool)

    notAtom = fn
      a when is_atom(a) ->
        false

      _A ->
        true
    end

    :mnesia_schema.verify(
      [],
      for p <- pool, notAtom.(p) do
        p
      end,
      badPool
    )

    nR = :mnesia_schema.pick(tab, :n_ram_copies, props, 0)
    nD = :mnesia_schema.pick(tab, :n_disc_copies, props, 0)
    nDO = :mnesia_schema.pick(tab, :n_disc_only_copies, props, 0)
    nExt = :mnesia_schema.pick(tab, :n_external_copies, props, 0)

    posInt = fn
      i when is_integer(i) and i >= 0 ->
        true

      _I ->
        false
    end

    :mnesia_schema.verify(true, posInt.(nR), {:bad_type, tab, {:n_ram_copies, nR}})
    :mnesia_schema.verify(true, posInt.(nD), {:bad_type, tab, {:n_disc_copies, nD}})
    :mnesia_schema.verify(true, posInt.(nDO), {:bad_type, tab, {:n_disc_only_copies, nDO}})
    :mnesia_schema.verify(true, posInt.(nExt), {:bad_type, tab, {:n_external_copies, nDO}})
    cs2 = verify_n_fragments(n, cs, mode)
    hashMod = :mnesia_schema.pick(tab, :hash_module, props, :mnesia_frag_hash)
    hashState = :mnesia_schema.pick(tab, :hash_state, props, :undefined)
    hashState2 = hashMod.init_state(tab, hashState)

    fH =
      r_frag_state(
        foreign_key: foreignKey2,
        n_fragments: 1,
        hash_module: hashMod,
        hash_state: hashState2
      )

    cond do
      nR == 0 and nD == 0 and nDO == 0 and nExt == 0 ->
        do_expand_cstruct(cs2, fH, n, pool, defaultNR, defaultND, defaultNDO, defaultNExt, mode)

      true ->
        do_expand_cstruct(cs2, fH, n, pool, nR, nD, nDO, nExt, mode)
    end
  end

  defp do_expand_cstruct(cs, fH, n, pool, nR, nD, nDO, nExt, mode) do
    tab = r_cstruct(cs, :name)
    lC = r_cstruct(cs, :local_content)
    :mnesia_schema.verify(false, lC, {:combine_error, tab, {:local_content, lC}})
    snmp = r_cstruct(cs, :snmp)
    :mnesia_schema.verify([], snmp, {:combine_error, tab, {:snmp, snmp}})
    commonProps = [{:base_table, tab}]
    cs2 = r_cstruct(cs, frag_properties: :lists.sort(commonProps))
    expand_frag_cstructs(n, nR, nD, nDO, nExt, cs2, pool, pool, fH, mode)
  end

  defp verify_n_fragments(n, cs, mode) when is_integer(n) and n >= 1 do
    case mode do
      :create ->
        r_cstruct(cs, ram_copies: [], disc_copies: [], disc_only_copies: [], external_copies: [])

      :activate ->
        reason = {:combine_error, r_cstruct(cs, :name), {:n_fragments, n}}
        :mnesia_schema.verify(1, n, reason)
        cs
    end
  end

  defp verify_n_fragments(n, cs, _Mode) do
    :mnesia.abort({:bad_type, r_cstruct(cs, :name), {:n_fragments, n}})
  end

  defp pick_props(tab, cs, {foreignTab, attr}) do
    :mnesia_schema.verify(true, foreignTab != tab, {:combine_error, tab, {foreignTab, attr}})
    props = r_cstruct(cs, :frag_properties)
    attrs = r_cstruct(cs, :attributes)
    foreignKey = lookup_prop(foreignTab, :foreign_key)
    foreignN = lookup_prop(foreignTab, :n_fragments)
    foreignPool = lookup_prop(foreignTab, :node_pool)
    n = :mnesia_schema.pick(tab, :n_fragments, props, foreignN)
    pool = :mnesia_schema.pick(tab, :node_pool, props, foreignPool)

    :mnesia_schema.verify(
      foreignN,
      n,
      {:combine_error, tab, {:n_fragments, n}, foreignTab, {:n_fragments, foreignN}}
    )

    :mnesia_schema.verify(
      foreignPool,
      pool,
      {:combine_error, tab, {:node_pool, pool}, foreignTab, {:node_pool, foreignPool}}
    )

    :mnesia_schema.verify(
      :undefined,
      foreignKey,
      {:combine_error, tab, 'Multiple levels of foreign_key dependencies', {foreignTab, attr},
       foreignKey}
    )

    key = {foreignTab, :mnesia_schema.attr_to_pos(attr, attrs)}
    defaultNR = length(val({foreignTab, :ram_copies}))
    defaultND = length(val({foreignTab, :disc_copies}))
    defaultNDO = length(val({foreignTab, :disc_only_copies}))
    defaultNExt = length(val({foreignTab, :external_copies}))
    {key, n, pool, defaultNR, defaultND, defaultNDO, defaultNExt}
  end

  defp pick_props(tab, cs, :undefined) do
    props = r_cstruct(cs, :frag_properties)
    defaultN = 1
    defaultPool = :mnesia.system_info(:db_nodes)
    n = :mnesia_schema.pick(tab, :n_fragments, props, defaultN)
    pool = :mnesia_schema.pick(tab, :node_pool, props, defaultPool)
    defaultNR = 1
    defaultND = 0
    defaultNDO = 0
    defaultNExt = 0
    {:undefined, n, pool, defaultNR, defaultND, defaultNDO, defaultNExt}
  end

  defp pick_props(tab, _Cs, badKey) do
    :mnesia.abort({:bad_type, tab, {:foreign_key, badKey}})
  end

  defp expand_frag_cstructs(n, nR, nD, nDO, nExt, commonCs, dist, pool, fH, mode)
       when n > 1 and mode == :create do
    frag = n_to_frag_name(r_cstruct(commonCs, :name), n)
    cs = r_cstruct(commonCs, name: frag)
    {cs2, revModDist, restDist} = set_frag_nodes(nR, nD, nDO, nExt, cs, dist, [])
    modDist = :lists.reverse(revModDist)
    dist2 = rearrange_dist(cs, modDist, restDist, pool)
    {fH2, _FromFrags, _AdditionalWriteFrags} = adjust_before_split(fH)
    csList = expand_frag_cstructs(n - 1, nR, nD, nDO, nExt, commonCs, dist2, pool, fH2, mode)
    [cs2 | csList]
  end

  defp expand_frag_cstructs(1, nR, nD, nDO, nExt, commonCs, dist, pool, fH, mode) do
    baseProps =
      r_cstruct(commonCs, :frag_properties) ++
        [
          {:foreign_key, r_frag_state(fH, :foreign_key)},
          {:hash_module, r_frag_state(fH, :hash_module)},
          {:hash_state, r_frag_state(fH, :hash_state)},
          {:n_fragments, r_frag_state(fH, :n_fragments)},
          {:node_pool, pool}
        ]

    baseCs = r_cstruct(commonCs, frag_properties: :lists.sort(baseProps))

    case mode do
      :activate ->
        [baseCs]

      :create ->
        {baseCs2, _, _} = set_frag_nodes(nR, nD, nDO, nExt, baseCs, dist, [])
        [baseCs2]
    end
  end

  defp set_frag_nodes(nR, nD, nDO, nExt, cs, [head | tail], acc)
       when nR > 0 do
    pos = r_cstruct(:ram_copies)
    {cs2, head2} = set_frag_node(cs, pos, head)
    set_frag_nodes(nR - 1, nD, nDO, nExt, cs2, tail, [head2 | acc])
  end

  defp set_frag_nodes(nR, nD, nDO, nExt, cs, [head | tail], acc)
       when nD > 0 do
    pos = r_cstruct(:disc_copies)
    {cs2, head2} = set_frag_node(cs, pos, head)
    set_frag_nodes(nR, nD - 1, nDO, nExt, cs2, tail, [head2 | acc])
  end

  defp set_frag_nodes(nR, nD, nDO, nExt, cs, [head | tail], acc)
       when nDO > 0 do
    pos = r_cstruct(:disc_only_copies)
    {cs2, head2} = set_frag_node(cs, pos, head)
    set_frag_nodes(nR, nD, nDO - 1, nExt, cs2, tail, [head2 | acc])
  end

  defp set_frag_nodes(nR, nD, nDO, nExt, cs, [head | tail], acc)
       when nExt > 0 do
    pos = r_cstruct(:external_copies)
    {cs2, head2} = set_frag_node(cs, pos, head)
    set_frag_nodes(nR, nD, nDO, nExt - 1, cs2, tail, [head2 | acc])
  end

  defp set_frag_nodes(0, 0, 0, 0, cs, restDist, modDist) do
    {cs, modDist, restDist}
  end

  defp set_frag_nodes(_, _, _, _, cs, [], _) do
    :mnesia.abort({:combine_error, r_cstruct(cs, :name), 'Too few nodes in node_pool'})
  end

  defp set_frag_node(cs, pos, head) do
    ns = :erlang.element(pos, cs)

    {node, count2} =
      case head do
        {n, count}
        when is_atom(n) and is_integer(count) and
               count >= 0 ->
          {n, count + 1}

        n when is_atom(n) ->
          {n, 1}

        badNode ->
          :mnesia.abort({:bad_type, r_cstruct(cs, :name), badNode})
      end

    :mnesia_schema.verify(
      true,
      :lists.member(node, val({:current, :db_nodes})),
      {:not_active, r_cstruct(cs, :name), node}
    )

    cs2 = :erlang.setelement(pos, cs, [node | ns])
    {cs2, {node, count2}}
  end

  defp rearrange_dist(cs, [{node, count} | modDist], dist, pool) do
    dist2 = insert_dist(cs, node, count, dist, pool)
    rearrange_dist(cs, modDist, dist2, pool)
  end

  defp rearrange_dist(_Cs, [], dist, _) do
    dist
  end

  defp insert_dist(cs, node, count, [head | tail], pool) do
    case head do
      {node2, count2}
      when is_atom(node2) and
             is_integer(count2) and count2 >= 0 ->
        case node_diff(node, count, node2, count2, pool) do
          :less ->
            [{node, count}, head | tail]

          :greater ->
            [head | insert_dist(cs, node, count, tail, pool)]
        end

      node2 when is_atom(node2) ->
        insert_dist(cs, node, count, [{node2, 0} | tail], pool)

      badNode ->
        :mnesia.abort({:bad_type, r_cstruct(cs, :name), badNode})
    end
  end

  defp insert_dist(_Cs, node, count, [], _Pool) do
    [{node, count}]
  end

  defp insert_dist(_Cs, _Node, _Count, dist, _Pool) do
    :mnesia.abort({:bad_type, dist})
  end

  defp node_diff(_Node, count, _Node2, count2, _Pool)
       when count < count2 do
    :less
  end

  defp node_diff(node, count, node2, count2, pool)
       when count == count2 do
    pos = list_pos(node, pool, 1)
    pos2 = list_pos(node2, pool, 1)

    cond do
      pos < pos2 ->
        :less

      pos > pos2 ->
        :greater
    end
  end

  defp node_diff(_Node, count, _Node2, count2, _Pool)
       when count > count2 do
    :greater
  end

  defp list_pos(h, [h | _T], pos) do
    pos
  end

  defp list_pos(e, [_H | t], pos) do
    list_pos(e, t, pos + 1)
  end

  def change_table_frag(tab, {:activate, fragProps}) do
    make_activate(tab, fragProps)
  end

  def change_table_frag(tab, :deactivate) do
    make_deactivate(tab)
  end

  def change_table_frag(tab, {:add_frag, sortedNodes}) do
    make_multi_add_frag(tab, sortedNodes)
  end

  def change_table_frag(tab, :del_frag) do
    make_multi_del_frag(tab)
  end

  def change_table_frag(tab, {:add_node, node}) do
    make_multi_add_node(tab, node)
  end

  def change_table_frag(tab, {:del_node, node}) do
    make_multi_del_node(tab, node)
  end

  def change_table_frag(tab, change) do
    :mnesia.abort({:bad_type, tab, change})
  end

  defp make_activate(tab, props) do
    cs = :mnesia_schema.incr_version(val({tab, :cstruct}))
    :mnesia_schema.ensure_active(cs)

    case r_cstruct(cs, :frag_properties) do
      [] ->
        cs2 = r_cstruct(cs, frag_properties: props)
        [cs3] = expand_cstruct(cs2, :activate)
        tabDef = :mnesia_schema.vsn_cs2list(cs3)
        op = {:op, :change_table_frag, :activate, tabDef}
        [[op]]

      badProps ->
        :mnesia.abort({:already_exists, tab, {:frag_properties, badProps}})
    end
  end

  defp make_deactivate(tab) do
    cs = :mnesia_schema.incr_version(val({tab, :cstruct}))
    :mnesia_schema.ensure_active(cs)
    foreigners = lookup_foreigners(tab)
    baseTab = lookup_prop(tab, :base_table)
    fH = lookup_frag_hash(tab)

    cond do
      baseTab != tab ->
        :mnesia.abort({:combine_error, tab, 'Not a base table'})

      foreigners != [] ->
        :mnesia.abort({:combine_error, tab, 'Too many foreigners', foreigners})

      r_frag_state(fH, :n_fragments) > 1 ->
        :mnesia.abort({:combine_error, tab, 'Too many fragments'})

      true ->
        cs2 = r_cstruct(cs, frag_properties: [])
        tabDef = :mnesia_schema.vsn_cs2list(cs2)
        op = {:op, :change_table_frag, :deactivate, tabDef}
        [[op]]
    end
  end

  defp make_multi_add_frag(tab, sortedNs) when is_list(sortedNs) do
    verify_multi(tab)
    ops = make_add_frag(tab, sortedNs)

    moreOps =
      for t <- lookup_foreigners(tab) do
        make_add_frag(t, sortedNs)
      end

    [ops | moreOps]
  end

  defp make_multi_add_frag(tab, sortedNs) do
    :mnesia.abort({:bad_type, tab, sortedNs})
  end

  defp verify_multi(tab) do
    fH = lookup_frag_hash(tab)
    foreignKey = r_frag_state(fH, :foreign_key)

    :mnesia_schema.verify(
      :undefined,
      foreignKey,
      {:combine_error, tab, 'Op only allowed via foreign table', {:foreign_key, foreignKey}}
    )
  end

  defp make_frag_names_and_acquire_locks(tab, n, fragIndecies, doNotLockN) do
    :mnesia_schema.get_tid_ts_and_lock(tab, :write)

    fun = fn index, fN ->
      cond do
        doNotLockN == true and index == n ->
          name = n_to_frag_name(tab, index)
          :erlang.setelement(index, fN, name)

        true ->
          name = n_to_frag_name(tab, index)
          :mnesia_schema.get_tid_ts_and_lock(name, :write)
          :erlang.setelement(index, fN, name)
      end
    end

    fragNames = :erlang.make_tuple(n, :undefined)
    :lists.foldl(fun, fragNames, fragIndecies)
  end

  defp make_add_frag(tab, sortedNs) do
    cs = :mnesia_schema.incr_version(val({tab, :cstruct}))
    :mnesia_schema.ensure_active(cs)
    fH = lookup_frag_hash(tab)
    {fH2, fromIndecies, writeIndecies} = adjust_before_split(fH)
    n = r_frag_state(fH2, :n_fragments)
    fragNames = make_frag_names_and_acquire_locks(tab, n, writeIndecies, true)
    newFrag = :erlang.element(n, fragNames)
    nR = length(r_cstruct(cs, :ram_copies))
    nD = length(r_cstruct(cs, :disc_copies))
    nDO = length(r_cstruct(cs, :disc_only_copies))
    nExt = length(r_cstruct(cs, :external_copies))

    newCs =
      r_cstruct(cs,
        name: newFrag,
        frag_properties: [{:base_table, tab}],
        ram_copies: [],
        disc_copies: [],
        disc_only_copies: [],
        external_copies: []
      )

    {newCs2, _, _} = set_frag_nodes(nR, nD, nDO, nExt, newCs, sortedNs, [])
    [newOp] = :mnesia_schema.make_create_table(newCs2)
    splitOps = split(tab, fH2, fromIndecies, fragNames, [])
    cs2 = replace_frag_hash(cs, fH2)
    tabDef = :mnesia_schema.vsn_cs2list(cs2)
    baseOp = {:op, :change_table_frag, {:add_frag, sortedNs}, tabDef}
    [baseOp, newOp | splitOps]
  end

  defp replace_frag_hash(cs, fH) when elem(fH, 0) === :frag_state do
    fun = fn prop ->
      case prop do
        {:n_fragments, _} ->
          {true, {:n_fragments, r_frag_state(fH, :n_fragments)}}

        {:hash_module, _} ->
          {true, {:hash_module, r_frag_state(fH, :hash_module)}}

        {:hash_state, _} ->
          {true, {:hash_state, r_frag_state(fH, :hash_state)}}

        {:next_n_to_split, _} ->
          false

        {:n_doubles, _} ->
          false

        _ ->
          true
      end
    end

    props = :lists.zf(fun, r_cstruct(cs, :frag_properties))
    r_cstruct(cs, frag_properties: props)
  end

  defp adjust_before_split(fH) do
    hashState = r_frag_state(fH, :hash_state)

    {hashState2, fromFrags, additionalWriteFrags} =
      case r_frag_state(fH, :hash_module) do
        hashMod when hashMod == :mnesia_frag_hash ->
          :mnesia_frag_hash.add_frag(hashState)

        hashMod ->
          hashMod.add_frag(hashState)
      end

    n = r_frag_state(fH, :n_fragments) + 1

    verifyFun = fn
      f
      when is_integer(f) and f >= 1 and
             f <= n ->
        false

      _F ->
        true
    end

    try do
      fromFrags2 = :lists.sort(fromFrags)

      unionFrags =
        :lists.merge(
          fromFrags2,
          :lists.sort(additionalWriteFrags)
        )

      frags = :lists.filter(verifyFun, unionFrags)
      frags == [] or :erlang.error(frags)
      fH2 = r_frag_state(fH, n_fragments: n, hash_state: hashState2)
      {fH2, fromFrags2, unionFrags}
    catch
      :error, badFrags ->
        :mnesia.abort({'add_frag: Fragment numbers out of range', badFrags, {:range, 1, n}})
    end
  end

  defp split(tab, fH, [splitN | splitNs], fragNames, ops) do
    splitFrag = :erlang.element(splitN, fragNames)
    pat = :mnesia.table_info(splitFrag, :wild_pattern)
    {_Mod, tid, ts} = :mnesia_schema.get_tid_ts_and_lock(tab, :none)
    recs = :mnesia.match_object(tid, ts, splitFrag, pat, :read)
    ops2 = do_split(fH, splitN, fragNames, recs, ops)
    split(tab, fH, splitNs, fragNames, ops2)
  end

  defp split(_Tab, _FH, [], _FragNames, ops) do
    ops
  end

  defp do_split(fH, oldN, fragNames, [rec | recs], ops) do
    pos = key_pos(fH)
    hashKey = :erlang.element(pos, rec)

    case key_to_n(fH, hashKey) do
      newN when newN == oldN ->
        do_split(fH, oldN, fragNames, recs, ops)

      newN ->
        case :erlang.element(newN, fragNames) do
          newFrag when newFrag != :undefined ->
            oldFrag = :erlang.element(oldN, fragNames)
            key = :erlang.element(2, rec)
            newOid = {newFrag, key}
            oldOid = {oldFrag, key}

            ops2 = [
              {:op, :rec, :unknown, {newOid, [rec], :write}},
              {:op, :rec, :unknown, {oldOid, [oldOid], :delete}}
              | ops
            ]

            do_split(fH, oldN, fragNames, recs, ops2)

          _NewFrag ->
            :mnesia.abort({'add_frag: Fragment not locked', newN})
        end
    end
  end

  defp do_split(_FH, _OldN, _FragNames, [], ops) do
    ops
  end

  defp make_multi_del_frag(tab) do
    verify_multi(tab)
    ops = make_del_frag(tab)

    moreOps =
      for t <- lookup_foreigners(tab) do
        make_del_frag(t)
      end

    [ops | moreOps]
  end

  defp make_del_frag(tab) do
    fH = lookup_frag_hash(tab)

    case r_frag_state(fH, :n_fragments) do
      n when n > 1 ->
        cs = :mnesia_schema.incr_version(val({tab, :cstruct}))
        :mnesia_schema.ensure_active(cs)
        {fH2, fromIndecies, writeIndecies} = adjust_before_merge(fH)
        fragNames = make_frag_names_and_acquire_locks(tab, n, writeIndecies, false)
        mergeOps = merge(tab, fH2, fromIndecies, fragNames, [])
        lastFrag = :erlang.element(n, fragNames)

        [lastOp] =
          :mnesia_schema.make_delete_table(
            lastFrag,
            :single_frag
          )

        cs2 = replace_frag_hash(cs, fH2)
        tabDef = :mnesia_schema.vsn_cs2list(cs2)
        baseOp = {:op, :change_table_frag, :del_frag, tabDef}
        [baseOp, lastOp | mergeOps]

      _ ->
        :mnesia.abort({:no_exists, tab})
    end
  end

  defp adjust_before_merge(fH) do
    hashState = r_frag_state(fH, :hash_state)

    {hashState2, fromFrags, additionalWriteFrags} =
      case r_frag_state(fH, :hash_module) do
        hashMod when hashMod == :mnesia_frag_hash ->
          :mnesia_frag_hash.del_frag(hashState)

        hashMod ->
          hashMod.del_frag(hashState)
      end

    n = r_frag_state(fH, :n_fragments)

    verifyFun = fn
      f
      when is_integer(f) and f >= 1 and
             f <= n ->
        false

      _F ->
        true
    end

    try do
      fromFrags2 = :lists.sort(fromFrags)

      unionFrags =
        :lists.merge(
          fromFrags2,
          :lists.sort(additionalWriteFrags)
        )

      frags = :lists.filter(verifyFun, unionFrags)
      [] == frags or :erlang.error(frags)

      case :lists.member(n, fromFrags2) do
        true ->
          fH2 = r_frag_state(fH, n_fragments: n - 1, hash_state: hashState2)
          {fH2, fromFrags2, unionFrags}

        false ->
          :mnesia.abort({'del_frag: Last fragment number not included', n})
      end
    catch
      :error, badFrags ->
        :mnesia.abort({'del_frag: Fragment numbers out of range', badFrags, {:range, 1, n}})
    end
  end

  defp merge(tab, fH, [fromN | fromNs], fragNames, ops) do
    fromFrag = :erlang.element(fromN, fragNames)
    pat = :mnesia.table_info(fromFrag, :wild_pattern)
    {_Mod, tid, ts} = :mnesia_schema.get_tid_ts_and_lock(tab, :none)
    recs = :mnesia.match_object(tid, ts, fromFrag, pat, :read)
    ops2 = do_merge(fH, fromN, fragNames, recs, ops)
    merge(tab, fH, fromNs, fragNames, ops2)
  end

  defp merge(_Tab, _FH, [], _FragNames, ops) do
    ops
  end

  defp do_merge(fH, oldN, fragNames, [rec | recs], ops) do
    pos = key_pos(fH)
    lastN = r_frag_state(fH, :n_fragments) + 1
    hashKey = :erlang.element(pos, rec)

    case key_to_n(fH, hashKey) do
      newN when newN == lastN ->
        :mnesia.abort({'del_frag: Fragment number out of range', newN, {:range, 1, lastN}})

      newN when newN == oldN ->
        do_merge(fH, oldN, fragNames, recs, ops)

      newN when oldN == lastN ->
        case :erlang.element(newN, fragNames) do
          newFrag when newFrag != :undefined ->
            key = :erlang.element(2, rec)
            newOid = {newFrag, key}

            ops2 = [
              {:op, :rec, :unknown, {newOid, [rec], :write}}
              | ops
            ]

            do_merge(fH, oldN, fragNames, recs, ops2)

          _NewFrag ->
            :mnesia.abort({'del_frag: Fragment not locked', newN})
        end

      newN ->
        case :erlang.element(newN, fragNames) do
          newFrag when newFrag != :undefined ->
            oldFrag = :erlang.element(oldN, fragNames)
            key = :erlang.element(2, rec)
            newOid = {newFrag, key}
            oldOid = {oldFrag, key}

            ops2 = [
              {:op, :rec, :unknown, {newOid, [rec], :write}},
              {:op, :rec, :unknown, {oldOid, [oldOid], :delete}}
              | ops
            ]

            do_merge(fH, oldN, fragNames, recs, ops2)

          _NewFrag ->
            :mnesia.abort({'del_frag: Fragment not locked', newN})
        end
    end
  end

  defp do_merge(_FH, _OldN, _FragNames, [], ops) do
    ops
  end

  defp make_multi_add_node(tab, node) do
    verify_multi(tab)
    ops = make_add_node(tab, node)

    moreOps =
      for t <- lookup_foreigners(tab) do
        make_add_node(t, node)
      end

    [ops | moreOps]
  end

  defp make_add_node(tab, node) when is_atom(node) do
    pool = lookup_prop(tab, :node_pool)

    case :lists.member(node, pool) do
      false ->
        cs = :mnesia_schema.incr_version(val({tab, :cstruct}))
        pool2 = pool ++ [node]
        props = r_cstruct(cs, :frag_properties)
        props2 = :lists.keyreplace(:node_pool, 1, props, {:node_pool, pool2})
        cs2 = r_cstruct(cs, frag_properties: props2)
        tabDef = :mnesia_schema.vsn_cs2list(cs2)
        op = {:op, :change_table_frag, {:add_node, node}, tabDef}
        [op]

      true ->
        :mnesia.abort({:already_exists, tab, node})
    end
  end

  defp make_add_node(tab, node) do
    :mnesia.abort({:bad_type, tab, node})
  end

  defp make_multi_del_node(tab, node) do
    verify_multi(tab)
    ops = make_del_node(tab, node)

    moreOps =
      for t <- lookup_foreigners(tab) do
        make_del_node(t, node)
      end

    [ops | moreOps]
  end

  defp make_del_node(tab, node) when is_atom(node) do
    cs = :mnesia_schema.incr_version(val({tab, :cstruct}))
    :mnesia_schema.ensure_active(cs)
    pool = lookup_prop(tab, :node_pool)

    case :lists.member(node, pool) do
      true ->
        pool2 = pool -- [node]

        props =
          :lists.keyreplace(:node_pool, 1, r_cstruct(cs, :frag_properties), {:node_pool, pool2})

        cs2 = r_cstruct(cs, frag_properties: props)
        tabDef = :mnesia_schema.vsn_cs2list(cs2)
        op = {:op, :change_table_frag, {:del_node, node}, tabDef}
        [op]

      false ->
        :mnesia.abort({:no_exists, tab, node})
    end
  end

  defp make_del_node(tab, node) do
    :mnesia.abort({:bad_type, tab, node})
  end

  def remove_node(node, cs) do
    tab = r_cstruct(cs, :name)

    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :frag_hash}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        {cs, false}

      _ ->
        pool = lookup_prop(tab, :node_pool)

        case :lists.member(node, pool) do
          true ->
            pool2 = pool -- [node]

            props =
              :lists.keyreplace(
                :node_pool,
                1,
                r_cstruct(cs, :frag_properties),
                {:node_pool, pool2}
              )

            {r_cstruct(cs, frag_properties: props), true}

          false ->
            {cs, false}
        end
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

  def set_frag_hash(tab, props) do
    case props_to_frag_hash(tab, props) do
      fH when elem(fH, 0) === :frag_state ->
        :mnesia_lib.set({tab, :frag_hash}, fH)

      :no_hash ->
        :mnesia_lib.unset({tab, :frag_hash})
    end
  end

  defp props_to_frag_hash(_Tab, []) do
    :no_hash
  end

  defp props_to_frag_hash(tab, props) do
    case :mnesia_schema.pick(tab, :base_table, props, :undefined) do
      t when t == tab ->
        foreign = :mnesia_schema.pick(tab, :foreign_key, props, :must)
        n = :mnesia_schema.pick(tab, :n_fragments, props, :must)

        case :mnesia_schema.pick(tab, :hash_module, props, :undefined) do
          :undefined ->
            :no_hash

          hashMod ->
            hashState = :mnesia_schema.pick(tab, :hash_state, props, :must)

            r_frag_state(
              foreign_key: foreign,
              n_fragments: n,
              hash_module: hashMod,
              hash_state: hashState
            )
        end

      _ ->
        :no_hash
    end
  end

  defp lookup_prop(tab, prop) do
    props = val({tab, :frag_properties})

    case :lists.keysearch(prop, 1, props) do
      {:value, {^prop, val}} ->
        val

      false ->
        :mnesia.abort({:no_exists, tab, prop, {:frag_properties, props}})
    end
  end

  def lookup_frag_hash(tab) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :frag_hash}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      fH when elem(fH, 0) === :frag_state ->
        fH

      {:frag_hash, _K, _N, _S, _D} ->
        :mnesia.abort({:no_hash, tab, :frag_properties, :frag_hash})

      {:EXIT, _} ->
        :mnesia.abort({:no_exists, tab, :frag_properties, :frag_hash})
    end
  end

  def lookup_foreigners(tab) do
    hashPat = {:_, {tab, :_}, :_, :_, :_}

    for [t] <-
          :ets.match(
            :mnesia_gvar,
            {{:"$1", :frag_hash}, hashPat}
          ) do
      t
    end
  end

  defp record_to_frag_name(tab, rec) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :frag_hash}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        tab

      fH ->
        pos = key_pos(fH)
        key = :erlang.element(pos, rec)
        n = key_to_n(fH, key)
        n_to_frag_name(tab, n)
    end
  end

  defp key_pos(fH) do
    case r_frag_state(fH, :foreign_key) do
      :undefined ->
        2

      {_ForeignTab, pos} ->
        pos
    end
  end

  defp key_to_frag_name({baseTab, _} = tab, key) do
    n = key_to_frag_number(tab, key)
    n_to_frag_name(baseTab, n)
  end

  defp key_to_frag_name(tab, key) do
    n = key_to_frag_number(tab, key)
    n_to_frag_name(tab, n)
  end

  defp n_to_frag_name(tab, 1) do
    tab
  end

  defp n_to_frag_name(tab, n) when is_atom(tab) and is_integer(n) do
    :erlang.list_to_atom(:erlang.atom_to_list(tab) ++ '_frag' ++ :erlang.integer_to_list(n))
  end

  defp n_to_frag_name(tab, n) do
    :mnesia.abort({:bad_type, tab, n})
  end

  defp key_to_frag_number({tab, foreignKey}, _Key) do
    fH = val({tab, :frag_hash})

    case r_frag_state(fH, :foreign_key) do
      {_ForeignTab, _Pos} ->
        key_to_n(fH, foreignKey)

      :undefined ->
        :mnesia.abort({:combine_error, tab, :frag_properties, {:foreign_key, :undefined}})
    end
  end

  defp key_to_frag_number(tab, key) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :frag_hash}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        1

      fH ->
        key_to_n(fH, key)
    end
  end

  defp key_to_n(fH, key) do
    hashState = r_frag_state(fH, :hash_state)

    n =
      case r_frag_state(fH, :hash_module) do
        hashMod when hashMod == :mnesia_frag_hash ->
          :mnesia_frag_hash.key_to_frag_number(hashState, key)

        hashMod ->
          hashMod.key_to_frag_number(hashState, key)
      end

    cond do
      is_integer(n) and n >= 1 and
          n <= r_frag_state(fH, :n_fragments) ->
        n

      true ->
        :mnesia.abort(
          {'key_to_frag_number: Fragment number out of range', n,
           {:range, 1, r_frag_state(fH, :n_fragments)}}
        )
    end
  end

  def frag_names(tab) do
    case (try do
            :ets.lookup_element(:mnesia_gvar, {tab, :frag_hash}, 2)
          catch
            :error, _ ->
              {:EXIT, {:badarg, []}}
          end) do
      {:EXIT, _} ->
        [tab]

      fH ->
        n = r_frag_state(fH, :n_fragments)
        frag_names(tab, n, [])
    end
  end

  defp frag_names(tab, 1, acc) do
    [tab | acc]
  end

  defp frag_names(tab, n, acc) do
    frag = n_to_frag_name(tab, n)
    frag_names(tab, n - 1, [frag | acc])
  end

  defp frag_dist(tab) do
    pool = lookup_prop(tab, :node_pool)

    dist =
      for node <- pool do
        {:good, node, 0}
      end

    dist2 = count_frag(frag_names(tab), dist)
    sort_dist(dist2)
  end

  defp count_frag([frag | frags], dist) do
    dist2 = incr_nodes(val({frag, :ram_copies}), dist)
    dist3 = incr_nodes(val({frag, :disc_copies}), dist2)

    dist4 =
      incr_nodes(
        val({frag, :disc_only_copies}),
        dist3
      )

    dist5 = incr_nodes(val({frag, :external_copies}), dist4)
    count_frag(frags, dist5)
  end

  defp count_frag([], dist) do
    dist
  end

  defp incr_nodes([node | nodes], dist) do
    dist2 = incr_node(node, dist)
    incr_nodes(nodes, dist2)
  end

  defp incr_nodes([], dist) do
    dist
  end

  defp incr_node(node, [{kind, node, count} | tail]) do
    [{kind, node, count + 1} | tail]
  end

  defp incr_node(node, [head | tail]) do
    [head | incr_node(node, tail)]
  end

  defp incr_node(node, []) do
    [{:bad, node, 1}]
  end

  defp sort_dist(dist) do
    dist2 = deep_dist(dist, [])
    dist3 = :lists.keysort(1, dist2)
    shallow_dist(dist3)
  end

  defp deep_dist([head | tail], deep) do
    {kind, _Node, count} = head
    {tag, same, other} = pick_count(kind, count, [head | tail])
    deep_dist(other, [{tag, same} | deep])
  end

  defp deep_dist([], deep) do
    deep
  end

  defp pick_count(kind, count, [{kind2, node2, count2} | tail]) do
    head = {node2, count2}
    {_, same, other} = pick_count(kind, count, tail)

    cond do
      kind == :bad ->
        {:bad, [head | same], other}

      kind2 == :bad ->
        {count, same, [{kind2, node2, count2} | other]}

      count == count2 ->
        {count, [head | same], other}

      true ->
        {count, same, [{kind2, node2, count2} | other]}
    end
  end

  defp pick_count(_Kind, count, []) do
    {count, [], []}
  end

  defp shallow_dist([{_Tag, shallow} | deep]) do
    shallow ++ shallow_dist(deep)
  end

  defp shallow_dist([]) do
    []
  end
end
