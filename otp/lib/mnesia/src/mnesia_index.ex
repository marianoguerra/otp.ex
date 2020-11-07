defmodule :m_mnesia_index do
  use Bitwise
  import :mnesia_lib, only: [val: 1, verbose: 2]
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

  Record.defrecord(:r_index, :index,
    setorbag: :undefined,
    pos_list: :undefined
  )

  def read(tid, store, tab, ixKey, pos) do
    resList = :mnesia_locker.ixrlock(tid, store, tab, ixKey, pos)

    case val({tab, :setorbag}) do
      :bag when is_integer(pos) ->
        :mnesia_lib.key_search_all(ixKey, pos, resList)

      :bag when is_tuple(pos) ->
        tabStorage = val({tab, :storage_type})
        valsF = index_vals_f(tabStorage, tab, pos)

        for obj <- resList, :lists.member(ixKey, valsF.(obj)) do
          obj
        end

      _ ->
        resList
    end
  end

  def ext_index_instances(tab) do
    r_index(pos_list: posL) = val({tab, :index_info})

    :lists.foldr(
      fn
        {_, {{:ext, alias, mod}, tag}}, acc ->
          [{alias, mod, tag} | acc]

        _, acc ->
          acc
      end,
      [],
      posL
    )
  end

  def add_index(r_index(pos_list: posL, setorbag: sorB), storage, tab, key, obj, old) do
    add_index2(posL, sorB, storage, tab, key, obj, old)
  end

  defp add_index2([{{pos, type}, ixt} | tail], :bag, storage, tab, k, obj, oldRecs) do
    valsF = index_vals_f(storage, tab, pos)
    vals = valsF.(obj)
    put_index_vals(type, ixt, vals, k)
    add_index2(tail, :bag, storage, tab, k, obj, oldRecs)
  end

  defp add_index2([{{pos, type}, ixt} | tail], sorB, storage, tab, k, obj, oldRecs0) do
    valsF = index_vals_f(storage, tab, pos)
    newVals = valsF.(obj)

    oldRecs1 =
      case oldRecs0 do
        :undefined ->
          :mnesia_lib.db_get(storage, tab, k)

        _ ->
          oldRecs0
      end

    idxVal = valsF.(obj)

    case (for old <- oldRecs1, valsF.(old) !== idxVal do
            old
          end) do
      [] when oldRecs1 === [] ->
        put_index_vals(type, ixt, newVals, k)
        add_index2(tail, sorB, storage, tab, k, obj, oldRecs1)

      [] ->
        add_index2(tail, sorB, storage, tab, k, obj, oldRecs1)

      oldRecs ->
        for oldObj <- oldRecs do
          del_ixes(type, ixt, valsF, oldObj, k)
        end

        put_index_vals(type, ixt, newVals, k)
        add_index2(tail, sorB, storage, tab, k, obj, oldRecs1)
    end
  end

  defp add_index2([], _, _, _Tab, _K, _Obj, _) do
    :ok
  end

  def delete_index(index, storage, tab, k) do
    delete_index2(r_index(index, :pos_list), storage, tab, k)
  end

  defp delete_index2([{{pos, type}, ixt} | tail], storage, tab, k) do
    delObjs = :mnesia_lib.db_get(storage, tab, k)
    valsF = index_vals_f(storage, tab, pos)

    for obj <- delObjs do
      del_ixes(type, ixt, valsF, obj, k)
    end

    delete_index2(tail, storage, tab, k)
  end

  defp delete_index2([], _Storage, _Tab, _K) do
    :ok
  end

  defp put_index_vals(:ordered, ixt, vals, k) do
    for v <- vals do
      db_put(ixt, {{v, k}})
    end
  end

  defp put_index_vals(:bag, ixt, vals, k) do
    for v <- vals do
      db_put(ixt, {v, k})
    end
  end

  defp del_ixes(:bag, ixt, valsF, obj, key) do
    vals = valsF.(obj)

    for v <- vals do
      db_match_erase(ixt, {v, key})
    end
  end

  defp del_ixes(:ordered, ixt, valsF, obj, key) do
    vals = valsF.(obj)

    for v <- vals do
      db_erase(ixt, {v, key})
    end
  end

  def del_object_index(r_index(pos_list: posL, setorbag: sorB), storage, tab, k, obj) do
    del_object_index2(posL, sorB, storage, tab, k, obj)
  end

  defp del_object_index2([], _, _Storage, _Tab, _K, _Obj) do
    :ok
  end

  defp del_object_index2([{{pos, type}, ixt} | tail], soB, storage, tab, k, obj) do
    valsF = index_vals_f(storage, tab, pos)

    case soB do
      :bag ->
        del_object_bag(type, valsF, tab, k, obj, ixt)

      _ ->
        del_ixes(type, ixt, valsF, obj, k)
    end

    del_object_index2(tail, soB, storage, tab, k, obj)
  end

  defp del_object_bag(type, valsF, tab, key, obj, ixt) do
    ixKeys = valsF.(obj)

    found =
      for x <- :mnesia_lib.db_get(tab, key) do
        {x, valsF.(x)}
      end

    del_object_bag_(ixKeys, found, type, tab, key, obj, ixt)
  end

  defp del_object_bag_([ixK | ixKs], found, type, tab, key, obj, ixt) do
    case (for {x, ixes} <- found,
              :lists.member(ixK, ixes) do
            x
          end) do
      [old] when old === obj ->
        case type do
          :bag ->
            db_match_erase(ixt, {ixK, key})

          :ordered ->
            db_erase(ixt, {ixK, key})
        end

      _ ->
        :ok
    end

    del_object_bag_(ixKs, found, type, tab, key, obj, ixt)
  end

  defp del_object_bag_([], _, _, _, _, _, _) do
    :ok
  end

  def clear_index(index, tab, k, obj) do
    clear_index2(r_index(index, :pos_list), tab, k, obj)
  end

  defp clear_index2([], _Tab, _K, _Obj) do
    :ok
  end

  defp clear_index2([{_Pos, ixt} | tail], tab, k, obj) do
    db_match_erase(ixt, obj)
    clear_index2(tail, tab, k, obj)
  end

  def dirty_match_object(tab, pat, pos) when is_integer(pos) do
    case :erlang.element(2, pat) do
      :_ ->
        ixKey = :erlang.element(pos, pat)
        realKeys = realkeys(tab, pos, ixKey)
        merge(realKeys, tab, pat, [])

      _Else ->
        :mnesia_lib.db_match_object(tab, pat)
    end
  end

  defp merge([{_IxKey, realKey} | tail], tab, pat, ack) do
    pat2 = :erlang.setelement(2, pat, realKey)
    recs = :mnesia_lib.db_match_object(tab, pat2)
    merge(tail, tab, pat, recs ++ ack)
  end

  defp merge([], _, _, ack) do
    ack
  end

  defp realkeys(tab, pos, ixKey) do
    index = get_index_table(tab, pos)
    db_get(index, ixKey)
  end

  def dirty_select(tab, spec, pos) when is_integer(pos) do
    ixKey = :erlang.element(pos, spec)
    realKeys = realkeys(tab, pos, ixKey)
    storageType = val({tab, :storage_type})

    :lists.append(
      for {_, key} <- realKeys do
        :mnesia_lib.db_get(storageType, tab, key)
      end
    )
  end

  def dirty_read(tab, ixKey, pos) do
    :mnesia.dirty_rpc(tab, :mnesia_index, :dirty_read2, [tab, ixKey, pos])
  end

  def dirty_read2(tab, ixKey, pos) do
    r_index(pos_list: posL) = val({tab, :index_info})
    storage = val({tab, :storage_type})
    {type, ixt} = pick_index(posL, tab, pos)

    pat =
      case type do
        :ordered ->
          [{{{ixKey, :"$1"}}, [], [:"$1"]}]

        :bag ->
          [{{ixKey, :"$1"}, [], [:"$1"]}]
      end

    keys = db_select(ixt, pat)
    valsF = index_vals_f(storage, tab, pos)

    :lists.reverse(
      :lists.foldl(
        fn k, acc ->
          :lists.foldl(
            fn obj, acc1 ->
              case :lists.member(
                     ixKey,
                     valsF.(obj)
                   ) do
                true ->
                  [obj | acc1]

                false ->
                  acc1
              end
            end,
            acc,
            :mnesia_lib.db_get(
              storage,
              tab,
              k
            )
          )
        end,
        [],
        keys
      )
    )
  end

  defp pick_index([{{{pfx, _, _}, ixType}, ixt} | _], _Tab, {_} = pfx) do
    {ixType, ixt}
  end

  defp pick_index([{{pos, ixType}, ixt} | _], _Tab, pos) do
    {ixType, ixt}
  end

  defp pick_index([_ | t], tab, pos) do
    pick_index(t, tab, pos)
  end

  defp pick_index([], tab, pos) do
    :mnesia.abort({:no_exist, tab, {:index, pos}})
  end

  def tab2filename(tab, {a}) when is_atom(a) do
    :mnesia_lib.dir(tab) ++ '_-' ++ :erlang.atom_to_list(a) ++ '-.DAT'
  end

  def tab2filename(tab, t) when is_tuple(t) do
    tab2filename(tab, :erlang.element(1, t))
  end

  def tab2filename(tab, pos) when is_integer(pos) do
    :mnesia_lib.dir(tab) ++ '_' ++ :erlang.integer_to_list(pos) ++ '.DAT'
  end

  def init_index(tab, storage) do
    cs = val({tab, :cstruct})
    posList = r_cstruct(cs, :index)
    init_indecies(tab, storage, posList)
  end

  def init_indecies(tab, storage, posList) do
    case storage do
      :unknown ->
        :ignore

      {:ext, alias, mod} ->
        init_ext_index(tab, storage, alias, mod, posList)

      :disc_only_copies ->
        init_disc_index(tab, storage, posList)

      :ram_copies ->
        make_ram_index(tab, storage, posList)

      :disc_copies ->
        make_ram_index(tab, storage, posList)
    end
  end

  def del_index_table(_, :unknown, _) do
    :ignore
  end

  def del_index_table(tab, storage, {_} = pos) do
    delete_transient_index(tab, pos, storage)
    :mnesia_lib.del({tab, :index}, pos)
  end

  def del_index_table(tab, storage, pos) when is_integer(pos) do
    delete_transient_index(tab, pos, storage)
    :mnesia_lib.del({tab, :index}, pos)
  end

  def del_transient(tab, storage) do
    posList = val({tab, :index})
    del_transient(tab, posList, storage)
  end

  def del_transient(_, [], _) do
    :ok
  end

  def del_transient(tab, [pos | tail], storage) do
    delete_transient_index(tab, pos, storage)
    del_transient(tab, tail, storage)
  end

  defp delete_transient_index(tab, pos, {:ext, alias, mod}) do
    posInfo =
      case pos do
        _ when is_integer(pos) ->
          cs = val({tab, :cstruct})
          :lists.keyfind(pos, 1, r_cstruct(cs, :index))

        {p, t} ->
          {p, t}
      end

    tag = {tab, :index, posInfo}
    mod.close_table(alias, tag)
    mod.delete_table(alias, tag)
    del_index_info(tab, pos)
    :mnesia_lib.unset({tab, {:index, pos}})
  end

  defp delete_transient_index(tab, pos, :disc_only_copies) do
    tag = {tab, :index, pos}
    :mnesia_monitor.unsafe_close_dets(tag)
    _ = :file.delete(tab2filename(tab, pos))
    del_index_info(tab, pos)
    :mnesia_lib.unset({tab, {:index, pos}})
  end

  defp delete_transient_index(tab, pos, _Storage) do
    ixt = val({tab, {:index, pos}})
    :ets.delete(ixt)
    del_index_info(tab, pos)
    :mnesia_lib.unset({tab, {:index, pos}})
  end

  defp init_disc_index(_Tab, _Storage, []) do
    :done
  end

  defp init_disc_index(tab, :disc_only_copies, [{pos, _Pref} | tail]) do
    posInfo = {pos, :bag}
    fn__ = tab2filename(tab, pos)
    ixTag = {tab, :index, posInfo}
    _ = :file.delete(fn__)
    args = [{:file, fn__}, {:keypos, 1}, {:type, :bag}]
    :mnesia_monitor.open_dets(ixTag, args)
    storage = :disc_only_copies
    key = :mnesia_lib.db_first(storage, tab)
    recs = :mnesia_lib.db_get(storage, tab, key)
    binSize = :erlang.size(:erlang.term_to_binary(recs))
    keysPerChunk = div(4000, binSize) + 1
    init = {:start, keysPerChunk}
    :mnesia_lib.db_fixtable(storage, tab, true)

    :ok =
      :dets.init_table(
        ixTag,
        create_fun(init, tab, pos)
      )

    :mnesia_lib.db_fixtable(storage, tab, false)
    :mnesia_lib.set({tab, {:index, posInfo}}, ixTag)
    add_index_info(tab, val({tab, :setorbag}), {posInfo, {:dets, ixTag}})
    init_disc_index(tab, storage, tail)
  end

  defp init_ext_index(_, _, _, _, []) do
    :done
  end

  defp init_ext_index(tab, storage, alias, mod, [{pos, type} | tail]) do
    posInfo = {pos, type}
    ixTag = {tab, :index, posInfo}
    cS = val({tab, :cstruct})
    csList = :mnesia_schema.cs2list(cS)
    _Res = :mnesia_monitor.unsafe_create_external(ixTag, alias, mod, csList)
    mod.load_table(alias, ixTag, :init_index, csList)

    case mod.is_index_consistent(alias, ixTag) do
      false ->
        mod.index_is_consistent(alias, ixTag, false)
        mod.match_delete(alias, ixTag, :_)
        ixValsF = index_vals_f(storage, tab, pos)

        ixObjF =
          case type do
            :bag ->
              fn ixVal, key ->
                {ixVal, key}
              end

            :ordered ->
              fn ixVal, key ->
                {{ixVal, key}}
              end
          end

        :mnesia_lib.db_fixtable(storage, tab, true)

        :mnesia_lib.db_foldl(
          storage,
          fn rec, acc ->
            key = :erlang.element(2, rec)

            :lists.foreach(
              fn v ->
                ixObj = ixObjF.(v, key)
                mod.insert(alias, ixTag, ixObj)
              end,
              ixValsF.(rec)
            )

            acc
          end,
          :ok,
          tab,
          [{:_, [], [:"$_"]}],
          100
        )

        mod.index_is_consistent(alias, ixTag, true)

      true ->
        :ignore
    end

    :mnesia_lib.set({tab, {:index, posInfo}}, ixTag)
    add_index_info(tab, val({tab, :setorbag}), {posInfo, {storage, ixTag}})
    init_ext_index(tab, storage, alias, mod, tail)
  end

  defp create_fun(cont, tab, pos) do
    ixF = index_vals_f(:disc_only_copies, tab, pos)

    fn
      :read ->
        data =
          case cont do
            {:start, keysPerChunk} ->
              :mnesia_lib.db_init_chunk(:disc_only_copies, tab, keysPerChunk)

            :"$end_of_table" ->
              :"$end_of_table"

            _Else ->
              :mnesia_lib.db_chunk(:disc_only_copies, cont)
          end

        case data do
          :"$end_of_table" ->
            :end_of_input

          {recs, next} ->
            idxElems =
              :lists.flatmap(
                fn obj ->
                  primK = :erlang.element(2, obj)

                  for v <- ixF.(obj) do
                    {v, primK}
                  end
                end,
                recs
              )

            {idxElems, create_fun(next, tab, pos)}
        end

      :close ->
        :ok
    end
  end

  defp make_ram_index(_, _, []) do
    :done
  end

  defp make_ram_index(tab, storage, [pos | tail]) do
    add_ram_index(tab, storage, pos)
    make_ram_index(tab, storage, tail)
  end

  defp add_ram_index(tab, storage, {pos, _Pref}) do
    type = :ordered
    verbose('Creating index for ~tw ~p ~p~n', [tab, pos, type])
    setOrBag = val({tab, :setorbag})
    ixValsF = index_vals_f(storage, tab, pos)

    ixFun = fn val, key ->
      {{val, key}}
    end

    index =
      :mnesia_monitor.mktab(
        :mnesia_index,
        [:ordered_set, :public]
      )

    insert = fn rec, _Acc ->
      primK = :erlang.element(2, rec)

      true =
        :ets.insert(
          index,
          for v <- ixValsF.(rec) do
            ixFun.(v, primK)
          end
        )
    end

    :mnesia_lib.db_fixtable(:ram_copies, tab, true)
    true = :mnesia_lib.db_foldl(storage, insert, true, tab)
    :mnesia_lib.db_fixtable(:ram_copies, tab, false)
    :mnesia_lib.set({tab, {:index, pos}}, index)
    add_index_info(tab, setOrBag, {{pos, type}, {:ram, index}})
  end

  defp add_ram_index(_Tab, _, :snmp) do
    :ok
  end

  def index_info(setOrBag, posList) do
    ixPlugins = :mnesia_schema.index_plugins()

    expPosList =
      :lists.map(
        fn {{p, type}, ixt} = pI ->
          case p do
            {_} = ixN ->
              {_, m, f} = :lists.keyfind(ixN, 1, ixPlugins)
              {{{ixN, m, f}, type}, ixt}

            _ ->
              pI
          end
        end,
        posList
      )

    r_index(setorbag: setOrBag, pos_list: expPosList)
  end

  defp add_index_info(tab, setOrBag, ixElem) do
    commit = val({tab, :commit_work})

    case :lists.keysearch(:index, 1, commit) do
      false ->
        indexInfo = index_info(setOrBag, [ixElem])
        :mnesia_lib.set({tab, :index_info}, indexInfo)

        :mnesia_lib.set(
          {tab, :index},
          index_positions(indexInfo)
        )

        :mnesia_lib.set(
          {tab, :commit_work},
          :mnesia_lib.sort_commit([indexInfo | commit])
        )

      {:value, old} ->
        index = r_index(old, pos_list: [ixElem | r_index(old, :pos_list)])
        :mnesia_lib.set({tab, :index_info}, index)
        :mnesia_lib.set({tab, :index}, index_positions(index))
        newC = :lists.keyreplace(:index, 1, commit, index)

        :mnesia_lib.set(
          {tab, :commit_work},
          :mnesia_lib.sort_commit(newC)
        )
    end
  end

  defp index_positions(r_index(pos_list: pL)) do
    for {{p, _}, _} <- pL do
      p
    end
  end

  defp del_index_info(tab, pos) do
    commit = val({tab, :commit_work})

    case :lists.keysearch(:index, 1, commit) do
      false ->
        :skip

      {:value, old} ->
        case :lists.filter(
               fn {p, _} ->
                 :erlang.element(1, p) !== pos
               end,
               r_index(old, :pos_list)
             ) do
          [] ->
            indexInfo = index_info(r_index(old, :setorbag), [])
            :mnesia_lib.set({tab, :index_info}, indexInfo)

            :mnesia_lib.set(
              {tab, :index},
              index_positions(indexInfo)
            )

            newC = :lists.keydelete(:index, 1, commit)

            :mnesia_lib.set(
              {tab, :commit_work},
              :mnesia_lib.sort_commit(newC)
            )

          new ->
            index = r_index(old, pos_list: new)
            :mnesia_lib.set({tab, :index_info}, index)
            :mnesia_lib.set({tab, :index}, index_positions(index))
            newC = :lists.keyreplace(:index, 1, commit, index)

            :mnesia_lib.set(
              {tab, :commit_work},
              :mnesia_lib.sort_commit(newC)
            )
        end
    end
  end

  def db_put({:ram, ixt}, v) do
    true = :ets.insert(ixt, v)
  end

  def db_put({{:ext, _, _} = ext, ixt}, v) do
    :mnesia_lib.db_put(ext, ixt, v)
  end

  def db_put({:dets, ixt}, v) do
    :ok = :dets.insert(ixt, v)
  end

  def db_get({:ram, _} = ixt, ixKey) do
    pat = [{{{ixKey, :"$1"}}, [], [{:element, 1, :"$_"}]}]
    db_select(ixt, pat)
  end

  def db_get(
        {{:ext, _, _} = _Storage, {_, _, {_, type}}} = ixt,
        ixKey
      ) do
    pat =
      case type do
        :ordered ->
          [{{{ixKey, :"$1"}}, [], [{:element, 1, :"$_"}]}]

        :bag ->
          [{{ixKey, :_}, [], [:"$_"]}]
      end

    db_select(ixt, pat)
  end

  def db_get({:dets, ixt}, k) do
    :dets.lookup(ixt, k)
  end

  defp db_erase({:ram, ixt}, k) do
    :ets.delete(ixt, k)
  end

  defp db_erase({{:ext, _, _} = ext, ixt}, k) do
    :mnesia_lib.db_erase(ext, ixt, k)
  end

  defp db_erase({:dets, ixt}, k) do
    :dets.delete(ixt, k)
  end

  def db_match_erase({:ram, ixt}, pat) do
    true = :ets.match_delete(ixt, pat)
  end

  def db_match_erase({{:ext, _, _} = ext, ixt}, pat) do
    :mnesia_lib.db_match_erase(ext, ixt, pat)
  end

  def db_match_erase({:dets, ixt}, pat) do
    :ok = :dets.match_delete(ixt, pat)
  end

  defp db_select({:ram, ixt}, pat) do
    :ets.select(ixt, pat)
  end

  defp db_select({{:ext, _, _} = ext, ixt}, pat) do
    :mnesia_lib.db_select(ext, ixt, pat)
  end

  defp db_select({:dets, ixt}, pat) do
    :dets.select(ixt, pat)
  end

  def get_index_table(tab, pos) do
    get_index_table(tab, val({tab, :storage_type}), pos)
  end

  def get_index_table(tab, _Storage, pos) do
    r_index(pos_list: posL) = val({tab, :index_info})
    {_IxType, ixt} = pick_index(posL, tab, pos)
    ixt
  end

  def index_vals_f(storage, tab, {_} = pos) do
    index_vals_f(storage, tab, :lists.keyfind(pos, 1, :mnesia_schema.index_plugins()))
  end

  def index_vals_f(_Storage, tab, {pos, m, f}) do
    fn obj ->
      apply(m, f, [tab, pos, obj])
    end
  end

  def index_vals_f(storage, tab, pos) when is_integer(pos) do
    case :mnesia_lib.semantics(storage, :index_fun) do
      :undefined ->
        fn obj ->
          [:erlang.element(pos, obj)]
        end

      f when is_function(f, 4) ->
        {:ext, alias, _Mod} = storage

        fn obj ->
          f.(alias, tab, pos, obj)
        end
    end
  end
end
