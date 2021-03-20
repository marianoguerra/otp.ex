defmodule :m_beam_ssa_type do
  use Bitwise

  import :lists,
    only: [
      all: 2,
      any: 2,
      duplicate: 2,
      foldl: 3,
      keyfind: 3,
      member: 2,
      reverse: 1,
      split: 2,
      zip: 2
    ]

  require Record

  Record.defrecord(:r_b_module, :b_module,
    anno: %{},
    name: :undefined,
    exports: :undefined,
    attributes: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_b_function, :b_function,
    anno: %{},
    args: :undefined,
    bs: :undefined,
    cnt: :undefined
  )

  Record.defrecord(:r_b_blk, :b_blk, anno: %{}, is: :undefined, last: :undefined)
  Record.defrecord(:r_b_set, :b_set, anno: %{}, dst: :none, op: :undefined, args: [])
  Record.defrecord(:r_b_ret, :b_ret, anno: %{}, arg: :undefined)

  Record.defrecord(:r_b_br, :b_br, anno: %{}, bool: :undefined, succ: :undefined, fail: :undefined)

  Record.defrecord(:r_b_switch, :b_switch,
    anno: %{},
    arg: :undefined,
    fail: :undefined,
    list: :undefined
  )

  Record.defrecord(:r_b_var, :b_var, name: :undefined)
  Record.defrecord(:r_b_literal, :b_literal, val: :undefined)
  Record.defrecord(:r_b_remote, :b_remote, mod: :undefined, name: :undefined, arity: :undefined)

  Record.defrecord(:r_b_local, :b_local,
    name: :undefined,
    arity: :undefined
  )

  Record.defrecord(:r_func_info, :func_info,
    in: :ordsets.new(),
    out: :ordsets.new(),
    exported: true,
    arg_types: [],
    succ_types: []
  )

  Record.defrecord(:r_opt_st, :opt_st,
    ssa: :undefined,
    args: :undefined,
    cnt: :undefined,
    anno: :undefined
  )

  Record.defrecord(:r_t_atom, :t_atom, elements: :any)
  Record.defrecord(:r_t_bitstring, :t_bitstring, size_unit: 1)
  Record.defrecord(:r_t_bs_context, :t_bs_context, tail_unit: 1, slots: 0, valid: 0)
  Record.defrecord(:r_t_bs_matchable, :t_bs_matchable, tail_unit: 1)
  Record.defrecord(:r_t_float, :t_float, elements: :any)
  Record.defrecord(:r_t_fun, :t_fun, arity: :any, type: :any)
  Record.defrecord(:r_t_integer, :t_integer, elements: :any)

  Record.defrecord(:r_t_map, :t_map,
    super_key: :any,
    super_value: :any
  )

  Record.defrecord(:r_t_cons, :t_cons,
    type: :any,
    terminator: :any
  )

  Record.defrecord(:r_t_list, :t_list,
    type: :any,
    terminator: :any
  )

  Record.defrecord(:r_t_tuple, :t_tuple, size: 0, exact: false, elements: %{})

  Record.defrecord(:r_t_union, :t_union,
    atom: :none,
    list: :none,
    number: :none,
    tuple_set: :none,
    other: :none
  )

  Record.defrecord(:r_metadata, :metadata,
    func_id: :undefined,
    limit_return: :undefined,
    params: :undefined,
    used_once: :undefined
  )

  def opt_start(stMap, funcDb0) when funcDb0 !== %{} do
    {argDb, funcDb} = signatures(stMap, funcDb0)
    opt_start_1(:maps.keys(stMap), argDb, stMap, funcDb)
  end

  def opt_start(stMap, funcDb) do
    {stMap, funcDb}
  end

  defp opt_start_1([id | ids], argDb, stMap0, funcDb0) do
    case argDb do
      %{^id => argTypes} ->
        r_opt_st(ssa: linear0, args: args) =
          st0 =
          :erlang.map_get(
            id,
            stMap0
          )

        ts = :maps.from_list(zip(args, argTypes))
        {linear, funcDb} = opt_function(linear0, args, id, ts, funcDb0)
        st = r_opt_st(st0, ssa: linear)
        stMap = %{stMap0 | id => st}
        opt_start_1(ids, argDb, stMap, funcDb)

      %{} ->
        stMap = :maps.remove(id, stMap0)
        funcDb = :maps.remove(id, funcDb0)
        opt_start_1(ids, argDb, stMap, funcDb)
    end
  end

  defp opt_start_1([], _CommittedArgs, stMap, funcDb) do
    {stMap, funcDb}
  end

  Record.defrecord(:r_sig_st, :sig_st, wl: wl_new(), committed: %{}, updates: %{})

  defp signatures(stMap, funcDb0) do
    state0 = init_sig_st(stMap, funcDb0)
    {state, funcDb} = signatures_1(stMap, funcDb0, state0)
    {r_sig_st(state, :committed), funcDb}
  end

  defp signatures_1(stMap, funcDb0, state0) do
    case wl_next(r_sig_st(state0, :wl)) do
      {:ok, funcId} ->
        {state, funcDb} = sig_function(funcId, stMap, state0, funcDb0)
        signatures_1(stMap, funcDb, state)

      :empty ->
        r_sig_st(updates: same, committed: same) = state0
        {state0, funcDb0}
    end
  end

  defp sig_function(id, stMap, state0, funcDb0) do
    case sig_function_1(id, stMap, state0, funcDb0) do
      {false, false, state, funcDb} ->
        wl = wl_pop(id, r_sig_st(state, :wl))
        {r_sig_st(state, wl: wl), funcDb}

      {false, true, state, funcDb} ->
        {state, funcDb}

      {true, wlChanged, state, funcDb} ->
        wl0 =
          case wlChanged do
            true ->
              r_sig_st(state, :wl)

            false ->
              wl_pop(id, r_sig_st(state, :wl))
          end

        r_func_info(in: cs0) = :erlang.map_get(id, funcDb0)

        callers =
          for c <- cs0,
              :erlang.is_map_key(c, r_sig_st(state, :updates)) do
            c
          end

        wl = wl_defer_list(callers, wl0)
        {r_sig_st(state, wl: wl), funcDb}
    end
  end

  defp sig_function_1(id, stMap, state0, funcDb) do
    r_opt_st(ssa: linear, args: args) = :erlang.map_get(id, stMap)
    {argTypes, state1} = sig_commit_args(id, state0)
    ts = :maps.from_list(zip(args, argTypes))

    fakeCall =
      r_b_set(
        op: :call,
        args: [
          r_b_remote(mod: r_b_literal(val: :unknown), name: r_b_literal(val: :unknown), arity: 0)
        ]
      )

    ds =
      :maps.from_list(
        for r_b_var() = var <- args do
          {var, r_b_set(fakeCall, dst: var)}
        end
      )

    ls = %{1 => {:incoming, ts}, 0 => {:incoming, ts}}
    meta = init_metadata(id, linear, args)
    wl0 = r_sig_st(state1, :wl)
    {state, succTypes} = sig_bs(linear, ds, ls, funcDb, %{}, [], meta, state1)
    wlChanged = wl_changed(wl0, r_sig_st(state, :wl))
    %{^id => r_func_info(succ_types: succTypes0) = entry0} = funcDb

    cond do
      succTypes0 === succTypes ->
        {false, wlChanged, state, funcDb}

      succTypes0 !== succTypes ->
        entry = r_func_info(entry0, succ_types: succTypes)
        {true, wlChanged, state, %{funcDb | id => entry}}
    end
  end

  defp sig_bs(
         [{l, r_b_blk(is: is, last: last0)} | bs],
         ds0,
         ls0,
         fdb,
         sub0,
         succTypes0,
         meta,
         state0
       ) do
    case ls0 do
      %{^l => incoming} ->
        {:incoming, ts0} = incoming
        {ts, ds, sub, state} = sig_is(is, ts0, ds0, ls0, fdb, sub0, state0)
        last = simplify_terminator(last0, ts, ds, sub)
        succTypes = update_success_types(last, ts, ds, meta, succTypes0)
        usedOnce = r_metadata(meta, :used_once)
        {_, ls1} = update_successors(last, ts, ds, ls0, usedOnce)
        ls = %{ls1 | l => {:outgoing, ts}}
        sig_bs(bs, ds, ls, fdb, sub, succTypes, meta, state)

      %{} ->
        sig_bs(bs, ds0, ls0, fdb, sub0, succTypes0, meta, state0)
    end
  end

  defp sig_bs([], _Ds, _Ls, _Fdb, _Sub, succTypes, _Meta, state) do
    {state, succTypes}
  end

  defp sig_is(
         [
           r_b_set(op: :call, args: [r_b_local() = callee | _] = args0, dst: dst) = i0
           | is
         ],
         ts0,
         ds0,
         ls,
         fdb,
         sub,
         state0
       ) do
    args = simplify_args(args0, ts0, sub)
    i1 = :beam_ssa.normalize(r_b_set(i0, args: args))
    [_ | callArgs] = args
    {i, state} = sig_local_call(i1, callee, callArgs, ts0, fdb, state0)
    ts = update_types(i, ts0, ds0)
    ds = Map.put(ds0, dst, i)
    sig_is(is, ts, ds, ls, fdb, sub, state)
  end

  defp sig_is(
         [
           r_b_set(op: :call, args: [r_b_var() | _] = args0, dst: dst) = i0
           | is
         ],
         ts0,
         ds0,
         ls,
         fdb,
         sub,
         state
       ) do
    args = simplify_args(args0, ts0, sub)
    i1 = :beam_ssa.normalize(r_b_set(i0, args: args))
    [fun | _] = args

    i =
      case normalized_type(fun, ts0) do
        r_t_fun(type: type) ->
          :beam_ssa.add_anno(:result_type, type, i1)

        _ ->
          i1
      end

    ts = update_types(i, ts0, ds0)
    ds = Map.put(ds0, dst, i)
    sig_is(is, ts, ds, ls, fdb, sub, state)
  end

  defp sig_is(
         [
           r_b_set(op: makeFun, args: args0, dst: dst) = i0
           | is
         ],
         ts0,
         ds0,
         ls,
         fdb,
         sub0,
         state0
       )
       when makeFun === :make_fun or
              makeFun === :old_make_fun do
    args = simplify_args(args0, ts0, sub0)
    i1 = :beam_ssa.normalize(r_b_set(i0, args: args))
    {i, state} = sig_make_fun(i1, ts0, fdb, state0)
    ts = update_types(i, ts0, ds0)
    ds = Map.put(ds0, dst, i)
    sig_is(is, ts, ds, ls, fdb, sub0, state)
  end

  defp sig_is([i0 | is], ts0, ds0, ls, fdb, sub0, state) do
    case simplify(i0, ts0, ds0, ls, sub0) do
      {r_b_set(), ts, ds} ->
        sig_is(is, ts, ds, ls, fdb, sub0, state)

      sub when is_map(sub) ->
        sig_is(is, ts0, ds0, ls, fdb, sub, state)
    end
  end

  defp sig_is([], ts, ds, _Ls, _Fdb, sub, state) do
    {ts, ds, sub, state}
  end

  defp sig_local_call(i0, callee, args, ts, fdb, state) do
    argTypes = argument_types(args, ts)
    i = sig_local_return(i0, callee, argTypes, fdb)
    {i, sig_update_args(callee, argTypes, state)}
  end

  defp sig_make_fun(
         r_b_set(
           op: makeFun,
           args: [r_b_local() = callee | freeVars]
         ) = i0,
         ts,
         fdb,
         state
       )
       when makeFun === :make_fun or
              makeFun === :old_make_fun do
    argCount = r_b_local(callee, :arity) - length(freeVars)

    fVTypes =
      for freeVar <- freeVars do
        raw_type(freeVar, ts)
      end

    argTypes = duplicate(argCount, :any) ++ fVTypes
    i = sig_local_return(i0, callee, argTypes, fdb)
    {i, sig_update_args(callee, argTypes, state)}
  end

  defp sig_local_return(i, callee, argTypes, fdb) do
    r_func_info(succ_types: succTypes) = :erlang.map_get(callee, fdb)

    case return_type(succTypes, argTypes) do
      :any ->
        i

      type ->
        :beam_ssa.add_anno(:result_type, type, i)
    end
  end

  defp init_sig_st(stMap, funcDb) do
    roots = init_sig_roots(funcDb)

    r_sig_st(
      committed: %{},
      updates: init_sig_args(roots, stMap, %{}),
      wl: wl_defer_list(roots, wl_new())
    )
  end

  defp init_sig_roots(funcDb) do
    :maps.fold(
      fn
        id, r_func_info(exported: true), acc ->
          [id | acc]

        _, _, acc ->
          acc
      end,
      [],
      funcDb
    )
  end

  defp init_sig_args([root | roots], stMap, acc) do
    r_opt_st(args: args0) = :erlang.map_get(root, stMap)
    argTypes = :lists.duplicate(length(args0), :any)
    init_sig_args(roots, stMap, Map.put(acc, root, argTypes))
  end

  defp init_sig_args([], _StMap, acc) do
    acc
  end

  defp sig_commit_args(
         id,
         r_sig_st(updates: us, committed: committed0) = state0
       ) do
    types = :erlang.map_get(id, us)
    committed = Map.put(committed0, id, types)
    state = r_sig_st(state0, committed: committed)
    {types, state}
  end

  defp sig_update_args(callee, types, r_sig_st(committed: committed) = state) do
    case committed do
      %{^callee => current} ->
        case parallel_join(current, types) do
          ^current ->
            state

          widened ->
            sig_update_args_1(callee, widened, state)
        end

      %{} ->
        sig_update_args_1(callee, types, state)
    end
  end

  defp sig_update_args_1(callee, types, r_sig_st(updates: us0, wl: wl0) = state) do
    us =
      case us0 do
        %{^callee => current} ->
          Map.put(us0, callee, parallel_join(current, types))

        %{} ->
          Map.put(us0, callee, types)
      end

    r_sig_st(state, updates: us, wl: wl_add(callee, wl0))
  end

  def opt_continue(linear0, args, anno, funcDb)
      when funcDb !== %{} do
    id = get_func_id(anno)

    case funcDb do
      %{^id => r_func_info(exported: false, arg_types: argTypes)} ->
        ts = join_arg_types(args, argTypes, %{})
        opt_function(linear0, args, id, ts, funcDb)

      %{^id => r_func_info(exported: true)} ->
        ts =
          :maps.from_list(
            for r_b_var() = v <- args do
              {v, :any}
            end
          )

        opt_function(linear0, args, id, ts, funcDb)
    end
  end

  def opt_continue(linear0, args, anno, _FuncDb) do
    id = get_func_id(anno)

    ts =
      :maps.from_list(
        for r_b_var() = v <- args do
          {v, :any}
        end
      )

    {linear, _} = opt_function(linear0, args, id, ts, %{})
    {linear, %{}}
  end

  defp join_arg_types([arg | args], [typeMap | tMs], ts) do
    type = :beam_types.join(:maps.values(typeMap))
    join_arg_types(args, tMs, Map.put(ts, arg, type))
  end

  defp join_arg_types([], [], ts) do
    ts
  end

  defp opt_function(linear0, args, id, ts, funcDb0) do
    fakeCall =
      r_b_set(
        op: :call,
        args: [
          r_b_remote(mod: r_b_literal(val: :unknown), name: r_b_literal(val: :unknown), arity: 0)
        ]
      )

    ds =
      :maps.from_list(
        for r_b_var() = var <- args do
          {var, r_b_set(fakeCall, dst: var)}
        end
      )

    ls = %{1 => {:incoming, ts}, 0 => {:incoming, ts}}
    meta = init_metadata(id, linear0, args)
    {linear, funcDb, succTypes} = opt_bs(linear0, ds, ls, funcDb0, %{}, [], meta, [])

    case funcDb do
      %{^id => entry0} ->
        entry = r_func_info(entry0, succ_types: succTypes)
        {linear, %{funcDb | id => entry}}

      %{} ->
        {linear, funcDb}
    end
  end

  defp get_func_id(anno) do
    %{func_info: {_Mod, name, arity}} = anno
    r_b_local(name: r_b_literal(val: name), arity: arity)
  end

  defp opt_bs(
         [{l, r_b_blk(is: is0, last: last0) = blk0} | bs],
         ds0,
         ls0,
         fdb0,
         sub0,
         succTypes0,
         meta,
         acc
       ) do
    case ls0 do
      %{^l => incoming} ->
        {:incoming, ts0} = incoming
        {is, ts, ds, fdb, sub} = opt_is(is0, ts0, ds0, ls0, fdb0, sub0, meta, [])
        last1 = simplify_terminator(last0, ts, ds, sub)
        succTypes = update_success_types(last1, ts, ds, meta, succTypes0)
        usedOnce = r_metadata(meta, :used_once)
        {last, ls1} = update_successors(last1, ts, ds, ls0, usedOnce)
        ls = %{ls1 | l => {:outgoing, ts}}
        blk = r_b_blk(blk0, is: is, last: last)
        opt_bs(bs, ds, ls, fdb, sub, succTypes, meta, [{l, blk} | acc])

      %{} ->
        opt_bs(bs, ds0, ls0, fdb0, sub0, succTypes0, meta, acc)
    end
  end

  defp opt_bs([], _Ds, _Ls, fdb, _Sub, succTypes, _Meta, acc) do
    {reverse(acc), fdb, succTypes}
  end

  defp opt_is(
         [
           r_b_set(op: :call, args: [r_b_local() = callee | _] = args0, dst: dst) = i0
           | is
         ],
         ts0,
         ds0,
         ls,
         fdb0,
         sub,
         meta,
         acc
       ) do
    args = simplify_args(args0, ts0, sub)
    i1 = :beam_ssa.normalize(r_b_set(i0, args: args))
    [_ | callArgs] = args
    {i, fdb} = opt_local_call(i1, callee, callArgs, dst, ts0, fdb0, meta)
    ts = update_types(i, ts0, ds0)
    ds = Map.put(ds0, dst, i)
    opt_is(is, ts, ds, ls, fdb, sub, meta, [i | acc])
  end

  defp opt_is(
         [
           r_b_set(op: :call, args: [r_b_var() | _] = args0, dst: dst) = i0
           | is
         ],
         ts0,
         ds0,
         ls,
         fdb,
         sub,
         meta,
         acc
       ) do
    args = simplify_args(args0, ts0, sub)
    i1 = :beam_ssa.normalize(r_b_set(i0, args: args))
    [fun | _] = args

    i =
      case normalized_type(fun, ts0) do
        r_t_fun(type: type) when type !== :any ->
          :beam_ssa.add_anno(:result_type, type, i1)

        _ ->
          i1
      end

    ts = update_types(i, ts0, ds0)
    ds = Map.put(ds0, dst, i)
    opt_is(is, ts, ds, ls, fdb, sub, meta, [i | acc])
  end

  defp opt_is(
         [
           r_b_set(op: makeFun, args: args0, dst: dst) = i0
           | is
         ],
         ts0,
         ds0,
         ls,
         fdb0,
         sub0,
         meta,
         acc
       )
       when makeFun === :make_fun or
              makeFun === :old_make_fun do
    args = simplify_args(args0, ts0, sub0)
    i1 = :beam_ssa.normalize(r_b_set(i0, args: args))
    {i, fdb} = opt_make_fun(i1, ts0, fdb0, meta)
    ts = update_types(i, ts0, ds0)
    ds = Map.put(ds0, dst, i)
    opt_is(is, ts, ds, ls, fdb, sub0, meta, [i | acc])
  end

  defp opt_is([i0 | is], ts0, ds0, ls, fdb, sub0, meta, acc) do
    case simplify(i0, ts0, ds0, ls, sub0) do
      {r_b_set() = i, ts, ds} ->
        opt_is(is, ts, ds, ls, fdb, sub0, meta, [i | acc])

      sub when is_map(sub) ->
        opt_is(is, ts0, ds0, ls, fdb, sub, meta, acc)
    end
  end

  defp opt_is([], ts, ds, _Ls, fdb, sub, _Meta, acc) do
    {reverse(acc), ts, ds, fdb, sub}
  end

  defp opt_local_call(i0, callee, args, dst, ts, fdb, meta) do
    argTypes = argument_types(args, ts)
    i = opt_local_return(i0, callee, argTypes, fdb)

    case fdb do
      %{^callee => r_func_info(exported: false, arg_types: aT0) = info0} ->
        callId = {r_metadata(meta, :func_id), dst}
        aT = update_arg_types(argTypes, aT0, callId)
        info = r_func_info(info0, arg_types: aT)
        {i, %{fdb | callee => info}}

      %{} ->
        {i, fdb}
    end
  end

  defp opt_make_fun(
         r_b_set(op: makeFun, dst: dst, args: [r_b_local() = callee | freeVars]) = i0,
         ts,
         fdb,
         meta
       )
       when makeFun === :make_fun or
              makeFun === :old_make_fun do
    argCount = r_b_local(callee, :arity) - length(freeVars)

    fVTypes =
      for freeVar <- freeVars do
        raw_type(freeVar, ts)
      end

    argTypes = duplicate(argCount, :any) ++ fVTypes
    i = opt_local_return(i0, callee, argTypes, fdb)

    case fdb do
      %{^callee => r_func_info(exported: false, arg_types: aT0) = info0} ->
        callId = {r_metadata(meta, :func_id), dst}
        aT = update_arg_types(argTypes, aT0, callId)
        info = r_func_info(info0, arg_types: aT)
        {i, %{fdb | callee => info}}

      %{} ->
        {i, fdb}
    end
  end

  defp opt_local_return(i, callee, argTypes, fdb) when fdb !== %{} do
    r_func_info(succ_types: succTypes) = :erlang.map_get(callee, fdb)

    case return_type(succTypes, argTypes) do
      :any ->
        i

      type ->
        :beam_ssa.add_anno(:result_type, type, i)
    end
  end

  defp opt_local_return(i, _Callee, _ArgTyps, _Fdb) do
    i
  end

  defp update_arg_types([argType | argTypes], [typeMap0 | typeMaps], callId) do
    typeMap = Map.put(typeMap0, callId, argType)
    [typeMap | update_arg_types(argTypes, typeMaps, callId)]
  end

  defp update_arg_types([], [], _CallId) do
    []
  end

  def opt_finish(args, anno, funcDb) do
    id = get_func_id(anno)

    case funcDb do
      %{^id => r_func_info(exported: false, arg_types: argTypes)} ->
        paramInfo0 = :maps.get(:parameter_info, anno, %{})
        paramInfo = opt_finish_1(args, argTypes, paramInfo0)
        {Map.put(anno, :parameter_info, paramInfo), funcDb}

      %{} ->
        {anno, funcDb}
    end
  end

  defp opt_finish_1([arg | args], [typeMap | typeMaps], acc0) do
    case :beam_types.join(:maps.values(typeMap)) do
      :any ->
        opt_finish_1(args, typeMaps, acc0)

      joinedType ->
        info = :maps.get(arg, acc0, [])
        acc = Map.put(acc0, arg, [{:type, joinedType} | info])
        opt_finish_1(args, typeMaps, acc)
    end
  end

  defp opt_finish_1([], [], acc) do
    acc
  end

  defp simplify_terminator(r_b_br(bool: bool) = br0, ts, ds, sub) do
    br = :beam_ssa.normalize(r_b_br(br0, bool: simplify_arg(bool, ts, sub)))
    simplify_not(br, ts, ds, sub)
  end

  defp simplify_terminator(r_b_switch(arg: arg0, fail: fail, list: list0) = sw0, ts, ds, sub) do
    arg = simplify_arg(arg0, ts, sub)

    list =
      for {val, lbl} <- list0, lbl !== fail do
        {val, lbl}
      end

    case :beam_ssa.normalize(
           r_b_switch(sw0,
             arg: arg,
             list: list
           )
         ) do
      r_b_switch() = sw ->
        case :beam_types.is_boolean_type(raw_type(arg, ts)) do
          true ->
            simplify_switch_bool(sw, ts, ds, sub)

          false ->
            sw
        end

      r_b_br() = br ->
        simplify_terminator(br, ts, ds, sub)
    end
  end

  defp simplify_terminator(r_b_ret(arg: arg) = ret, ts, ds, sub) do
    case ds do
      %{^arg => r_b_set(op: :call)} ->
        ret

      %{} ->
        r_b_ret(ret, arg: simplify_arg(arg, ts, sub))
    end
  end

  defp simplify(r_b_set(op: :phi, dst: dst, args: args0) = i0, ts0, ds0, ls, sub) do
    {type, args} = simplify_phi_args(args0, ls, sub, :none, [])

    case phi_all_same(args) do
      true ->
        [{val, _} | _] = args
        Map.put(sub, dst, val)

      false ->
        i = r_b_set(i0, args: args)
        ts = Map.put(ts0, dst, type)
        ds = Map.put(ds0, dst, i)
        {i, ts, ds}
    end
  end

  defp simplify(r_b_set(op: {:succeeded, kind}, args: [arg], dst: dst) = i0, ts0, ds0, _Ls, sub) do
    type =
      case will_succeed(i0, ts0, ds0, sub) do
        :yes ->
          :beam_types.make_atom(true)

        :no ->
          :beam_types.make_atom(false)

        :maybe ->
          :beam_types.make_boolean()
      end

    case type do
      r_t_atom(elements: [true]) ->
        lit = r_b_literal(val: true)
        Map.put(sub, dst, lit)

      r_t_atom(elements: [false]) when kind === :guard ->
        lit = r_b_literal(val: false)
        Map.put(sub, dst, lit)

      _ ->
        true = :erlang.is_map_key(arg, ds0)
        i = :beam_ssa.normalize(i0)
        ts = Map.put(ts0, dst, type)
        ds = Map.put(ds0, dst, i)
        {i, ts, ds}
    end
  end

  defp simplify(r_b_set(op: :bs_match, dst: dst, args: args0) = i0, ts0, ds0, _Ls, sub) do
    args = simplify_args(args0, ts0, sub)
    i1 = :beam_ssa.normalize(r_b_set(i0, args: args))

    i2 =
      case {args0, args} do
        {[_, _, _, r_b_var(), _], [type, val, flags, r_b_literal(val: :all), unit]} ->
          r_b_set(i1, args: [type, val, flags, r_b_literal(val: :bad_size), unit])

        {_, _} ->
          i1
      end

    r_b_set() = i3 = simplify(i2, ts0)
    i = :beam_ssa.normalize(i3)
    ts = update_types(i, ts0, ds0)
    ds = Map.put(ds0, dst, i)
    {i, ts, ds}
  end

  defp simplify(r_b_set(dst: dst, args: args0) = i0, ts0, ds0, _Ls, sub) do
    args = simplify_args(args0, ts0, sub)
    i1 = :beam_ssa.normalize(r_b_set(i0, args: args))

    case simplify(i1, ts0) do
      r_b_set() = i2 ->
        i = :beam_ssa.normalize(i2)
        ts = update_types(i, ts0, ds0)
        ds = Map.put(ds0, dst, i)
        {i, ts, ds}

      r_b_literal() = lit ->
        Map.put(sub, dst, lit)

      r_b_var() = var ->
        Map.put(sub, dst, var)
    end
  end

  defp simplify(r_b_set(op: {:bif, :and}, args: args) = i, ts) do
    case is_safe_bool_op(args, ts) do
      true ->
        case args do
          [_, r_b_literal(val: false) = res] ->
            res

          [res, r_b_literal(val: true)] ->
            res

          _ ->
            eval_bif(i, ts)
        end

      false ->
        i
    end
  end

  defp simplify(r_b_set(op: {:bif, :or}, args: args) = i, ts) do
    case is_safe_bool_op(args, ts) do
      true ->
        case args do
          [res, r_b_literal(val: false)] ->
            res

          [_, r_b_literal(val: true) = res] ->
            res

          _ ->
            eval_bif(i, ts)
        end

      false ->
        i
    end
  end

  defp simplify(
         r_b_set(
           op: {:bif, :element},
           args: [r_b_literal(val: index), tuple]
         ) = i0,
         ts
       ) do
    case normalized_type(tuple, ts) do
      r_t_tuple(size: size)
      when is_integer(index) and 1 <= index and
             index <= size ->
        i =
          r_b_set(i0,
            op: :get_tuple_element,
            args: [tuple, r_b_literal(val: index - 1)]
          )

        simplify(i, ts)

      _ ->
        eval_bif(i0, ts)
    end
  end

  defp simplify(r_b_set(op: {:bif, :hd}, args: [list]) = i, ts) do
    case normalized_type(list, ts) do
      r_t_cons() ->
        r_b_set(i, op: :get_hd)

      _ ->
        eval_bif(i, ts)
    end
  end

  defp simplify(r_b_set(op: {:bif, :tl}, args: [list]) = i, ts) do
    case normalized_type(list, ts) do
      r_t_cons() ->
        r_b_set(i, op: :get_tl)

      _ ->
        eval_bif(i, ts)
    end
  end

  defp simplify(r_b_set(op: {:bif, :size}, args: [term]) = i, ts) do
    case normalized_type(term, ts) do
      r_t_tuple() ->
        simplify(r_b_set(i, op: {:bif, :tuple_size}), ts)

      r_t_bitstring(size_unit: u) when rem(u, 8) === 0 ->
        simplify(r_b_set(i, op: {:bif, :byte_size}), ts)

      _ ->
        eval_bif(i, ts)
    end
  end

  defp simplify(
         r_b_set(op: {:bif, :tuple_size}, args: [term]) = i,
         ts
       ) do
    case normalized_type(term, ts) do
      r_t_tuple(size: size, exact: true) ->
        r_b_literal(val: size)

      _ ->
        i
    end
  end

  defp simplify(
         r_b_set(
           op: {:bif, :is_function},
           args: [fun, r_b_literal(val: arity)]
         ) = i,
         ts
       )
       when is_integer(arity) and arity >= 0 do
    case normalized_type(fun, ts) do
      r_t_fun(arity: :any) ->
        i

      r_t_fun(arity: ^arity) ->
        r_b_literal(val: true)

      :any ->
        i

      _ ->
        r_b_literal(val: false)
    end
  end

  defp simplify(
         r_b_set(
           op: {:bif, :is_map_key},
           args: [key, map]
         ) = i,
         ts
       ) do
    case normalized_type(map, ts) do
      r_t_map() ->
        r_b_set(i, op: :has_map_field, args: [map, key])

      _ ->
        i
    end
  end

  defp simplify(r_b_set(op: {:bif, op0}, args: args) = i, ts)
       when op0 === :== or op0 === :"/=" do
    types = normalized_types(args, ts)

    eqEq0 =
      case {:beam_types.meet(types), :beam_types.join(types)} do
        {:none, :any} ->
          true

        {r_t_integer(), r_t_integer()} ->
          true

        {r_t_float(), r_t_float()} ->
          true

        {r_t_bitstring(), _} ->
          true

        {r_t_atom(), _} ->
          true

        {_, _} ->
          false
      end

    eqEq = eqEq0 or any_non_numeric_argument(args, ts)

    case eqEq do
      true ->
        op =
          case op0 do
            :== ->
              :"=:="

            :"/=" ->
              :"=/="
          end

        simplify(r_b_set(i, op: {:bif, op}), ts)

      false ->
        eval_bif(i, ts)
    end
  end

  defp simplify(r_b_set(op: {:bif, :"=:="}, args: [same, same]), _Ts) do
    r_b_literal(val: true)
  end

  defp simplify(r_b_set(op: {:bif, :"=:="}, args: [lHS, rHS]) = i, ts) do
    lType = raw_type(lHS, ts)
    rType = raw_type(rHS, ts)

    case :beam_types.meet(lType, rType) do
      :none ->
        r_b_literal(val: false)

      _ ->
        case {:beam_types.is_boolean_type(lType), :beam_types.normalize(rType)} do
          {true, r_t_atom(elements: [true])} ->
            lHS

          {true, r_t_atom(elements: [false])} ->
            simplify(r_b_set(i, op: {:bif, :not}, args: [lHS]), ts)

          {_, _} ->
            eval_bif(i, ts)
        end
    end
  end

  defp simplify(r_b_set(op: {:bif, op}, args: args) = i, ts) do
    types = normalized_types(args, ts)

    case is_float_op(op, types) do
      false ->
        eval_bif(i, ts)

      true ->
        annoArgs =
          for a <- types do
            anno_float_arg(a)
          end

        eval_bif(:beam_ssa.add_anno(:float_op, annoArgs, i), ts)
    end
  end

  defp simplify(r_b_set(op: :bs_extract, args: [ctx]) = i, ts) do
    case raw_type(ctx, ts) do
      r_t_bitstring() ->
        ctx

      r_t_bs_context() ->
        i
    end
  end

  defp simplify(
         r_b_set(
           op: :bs_match,
           args: [
             r_b_literal(val: :binary),
             ctx,
             _Flags,
             r_b_literal(val: :all),
             r_b_literal(val: opUnit)
           ]
         ) = i,
         ts
       ) do
    r_t_bs_context(tail_unit: ctxUnit) = raw_type(ctx, ts)

    cond do
      rem(ctxUnit, opUnit) === 0 ->
        r_b_set(i, op: :bs_get_tail, args: [ctx])

      rem(ctxUnit, opUnit) !== 0 ->
        i
    end
  end

  defp simplify(
         r_b_set(
           op: :bs_start_match,
           args: [r_b_literal(val: :new), src]
         ) = i,
         ts
       ) do
    case raw_type(src, ts) do
      r_t_bs_context() ->
        r_b_set(i, op: :bs_start_match, args: [r_b_literal(val: :resume), src])

      _ ->
        i
    end
  end

  defp simplify(
         r_b_set(
           op: :get_tuple_element,
           args: [tuple, r_b_literal(val: n)]
         ) = i,
         ts
       ) do
    r_t_tuple(size: size, elements: es) = normalized_type(tuple, ts)
    true = size > n
    elemType = :beam_types.get_tuple_element(n + 1, es)

    case :beam_types.get_singleton_value(elemType) do
      {:ok, val} ->
        r_b_literal(val: val)

      :error ->
        i
    end
  end

  defp simplify(
         r_b_set(op: :is_nonempty_list, args: [src]) = i,
         ts
       ) do
    case normalized_type(src, ts) do
      :any ->
        i

      r_t_list() ->
        i

      r_t_cons() ->
        r_b_literal(val: true)

      _ ->
        r_b_literal(val: false)
    end
  end

  defp simplify(
         r_b_set(
           op: :is_tagged_tuple,
           args: [src, r_b_literal(val: size), r_b_literal() = tag]
         ) = i,
         ts
       ) do
    simplify_is_record(i, normalized_type(src, ts), size, tag, ts)
  end

  defp simplify(
         r_b_set(op: :put_list, args: [r_b_literal(val: h), r_b_literal(val: t)]),
         _Ts
       ) do
    r_b_literal(val: [h | t])
  end

  defp simplify(r_b_set(op: :put_tuple, args: args) = i, _Ts) do
    case make_literal_list(args) do
      :none ->
        i

      list ->
        r_b_literal(val: :erlang.list_to_tuple(list))
    end
  end

  defp simplify(r_b_set(op: :wait_timeout, args: [r_b_literal(val: 0)]), _Ts) do
    r_b_literal(val: true)
  end

  defp simplify(
         r_b_set(op: :call, args: [r_b_remote() = rem | args]) = i,
         ts
       ) do
    case rem do
      r_b_remote(mod: r_b_literal(val: mod), name: r_b_literal(val: name)) ->
        simplify_remote_call(mod, name, args, ts, i)

      r_b_remote() ->
        i
    end
  end

  defp simplify(
         r_b_set(op: :call, args: [r_b_literal(val: fun) | args]) = i,
         _Ts
       )
       when is_function(fun, length(args)) do
    fI = :erlang.fun_info(fun)
    {:module, m} = keyfind(:module, 1, fI)
    {:name, f} = keyfind(:name, 1, fI)
    {:arity, a} = keyfind(:arity, 1, fI)
    rem = r_b_remote(mod: r_b_literal(val: m), name: r_b_literal(val: f), arity: a)
    r_b_set(i, args: [rem | args])
  end

  defp simplify(i, _Ts) do
    i
  end

  defp will_succeed(r_b_set(args: [src]), ts, ds, sub) do
    case {ds, ts} do
      {%{}, %{^src => :none}} ->
        :no

      {%{^src => i}, %{}} ->
        will_succeed_1(i, src, ts, sub)

      {%{}, %{}} ->
        true = :erlang.is_map_key(src, sub)
        :yes
    end
  end

  defp will_succeed_1(r_b_set(op: :bs_get_tail), _Src, _Ts, _Sub) do
    :yes
  end

  defp will_succeed_1(r_b_set(op: :bs_start_match, args: [_, arg]), _Src, ts, _Sub) do
    argType = raw_type(arg, ts)

    case :beam_types.is_bs_matchable_type(argType) do
      true ->
        :yes

      false ->
        case :beam_types.meet(argType, r_t_bs_matchable()) do
          :none ->
            :no

          _ ->
            :maybe
        end
    end
  end

  defp will_succeed_1(r_b_set(op: {:bif, bif}, args: bifArgs), _Src, ts, _Sub) do
    argTypes = normalized_types(bifArgs, ts)
    :beam_call_types.will_succeed(:erlang, bif, argTypes)
  end

  defp will_succeed_1(
         r_b_set(
           op: :call,
           args: [
             r_b_remote(mod: r_b_literal(val: mod), name: r_b_literal(val: func))
             | callArgs
           ]
         ),
         _Src,
         ts,
         _Sub
       ) do
    argTypes = normalized_types(callArgs, ts)
    :beam_call_types.will_succeed(mod, func, argTypes)
  end

  defp will_succeed_1(r_b_set(op: :get_hd), _Src, _Ts, _Sub) do
    :yes
  end

  defp will_succeed_1(r_b_set(op: :get_tl), _Src, _Ts, _Sub) do
    :yes
  end

  defp will_succeed_1(r_b_set(op: :has_map_field), _Src, _Ts, _Sub) do
    :yes
  end

  defp will_succeed_1(r_b_set(op: :get_tuple_element), _Src, _Ts, _Sub) do
    :yes
  end

  defp will_succeed_1(r_b_set(op: :put_tuple), _Src, _Ts, _Sub) do
    :yes
  end

  defp will_succeed_1(r_b_set(op: :bs_add, args: [arg1, arg2, _]), _Src, _Ts, _Sub) do
    case all(
           fn
             r_b_literal(val: size) ->
               is_integer(size) and size >= 0

             r_b_var() ->
               true
           end,
           [arg1, arg2]
         ) do
      true ->
        :maybe

      false ->
        :no
    end
  end

  defp will_succeed_1(
         r_b_set(
           op: :bs_init,
           args: [r_b_literal(val: :new), r_b_literal(val: size), _Unit]
         ),
         _Src,
         _Ts,
         _Sub
       ) do
    cond do
      is_integer(size) and size >= 0 ->
        :maybe

      true ->
        :no
    end
  end

  defp will_succeed_1(
         r_b_set(
           op: :bs_init,
           args: [r_b_literal(), _, r_b_literal(val: size), _Unit]
         ),
         _Src,
         _Ts,
         _Sub
       ) do
    cond do
      is_integer(size) and size >= 0 ->
        :maybe

      true ->
        :no
    end
  end

  defp will_succeed_1(
         r_b_set(
           op: :bs_match,
           args: [r_b_literal(val: type), _, _, r_b_literal(val: size), _]
         ),
         _Src,
         _Ts,
         _Sub
       ) do
    cond do
      is_integer(size) and size >= 0 ->
        :maybe

      type === :binary and size === :all ->
        :maybe

      true ->
        :no
    end
  end

  defp will_succeed_1(r_b_set(op: :call), _Src, _Ts, _Sub) do
    :maybe
  end

  defp will_succeed_1(r_b_set(op: :get_map_element), _Src, _Ts, _Sub) do
    :maybe
  end

  defp will_succeed_1(r_b_set(op: :wait), _Src, _Ts, _Sub) do
    :no
  end

  defp will_succeed_1(r_b_set(), src, ts, sub) do
    case simplify_arg(src, ts, sub) do
      r_b_var() = ^src ->
        :maybe

      _ ->
        :yes
    end
  end

  defp simplify_is_record(
         i,
         r_t_tuple(exact: exact, size: size, elements: es),
         recSize,
         r_b_literal(val: tagVal) = recTag,
         ts
       ) do
    tagType = :maps.get(1, es, :any)

    tagMatch =
      case :beam_types.get_singleton_value(tagType) do
        {:ok, ^tagVal} ->
          :yes

        {:ok, _} ->
          :no

        :error ->
          case :beam_types.meet(
                 raw_type(recTag, ts),
                 tagType
               ) do
            :none ->
              :no

            _ ->
              :maybe
          end
      end

    cond do
      (size !== recSize and exact) or size > recSize or
          tagMatch === :no ->
        r_b_literal(val: false)

      size === recSize and exact and tagMatch === :yes ->
        r_b_literal(val: true)

      true ->
        i
    end
  end

  defp simplify_is_record(i, :any, _Size, _Tag, _Ts) do
    i
  end

  defp simplify_is_record(_I, _Type, _Size, _Tag, _Ts) do
    r_b_literal(val: false)
  end

  defp simplify_switch_bool(r_b_switch(arg: b, fail: fail, list: list0), ts, ds, sub) do
    falseVal = r_b_literal(val: false)
    trueVal = r_b_literal(val: true)
    list1 = list0 ++ [{falseVal, fail}, {trueVal, fail}]
    {_, falseLbl} = keyfind(falseVal, 1, list1)
    {_, trueLbl} = keyfind(trueVal, 1, list1)
    br = r_b_br(bool: b, succ: trueLbl, fail: falseLbl)
    simplify_terminator(br, ts, ds, sub)
  end

  defp simplify_not(r_b_br(bool: r_b_var() = v, succ: succ, fail: fail) = br0, ts, ds, sub) do
    case ds do
      %{^v => r_b_set(op: {:bif, :not}, args: [bool])} ->
        case :beam_types.is_boolean_type(
               raw_type(
                 bool,
                 ts
               )
             ) do
          true ->
            br = r_b_br(br0, bool: bool, succ: fail, fail: succ)
            simplify_terminator(br, ts, ds, sub)

          false ->
            br0
        end

      %{} ->
        br0
    end
  end

  defp simplify_not(r_b_br(bool: r_b_literal()) = br, _Sub, _Ts, _Ds) do
    br
  end

  defp simplify_phi_args([{arg0, from} | rest], ls, sub, type0, args) do
    case ls do
      %{^from => outgoing} ->
        {:outgoing, ts} = outgoing
        arg = simplify_arg(arg0, ts, sub)
        type = :beam_types.join(raw_type(arg, ts), type0)
        phi = {arg, from}
        simplify_phi_args(rest, ls, sub, type, [phi | args])

      %{} ->
        simplify_phi_args(rest, ls, sub, type0, args)
    end
  end

  defp simplify_phi_args([], _Ls, _Sub, type, args) do
    {type, reverse(args)}
  end

  defp phi_all_same([{arg, _From} | phis]) do
    phi_all_same_1(phis, arg)
  end

  defp phi_all_same_1([{arg, _From} | phis], arg) do
    phi_all_same_1(phis, arg)
  end

  defp phi_all_same_1([], _Arg) do
    true
  end

  defp phi_all_same_1(_Phis, _Arg) do
    false
  end

  defp simplify_remote_call(:erlang, :throw, [term], ts, i) do
    type = normalized_type(term, ts)
    :beam_ssa.add_anno(:thrown_type, type, i)
  end

  defp simplify_remote_call(:erlang, :++, [r_b_literal(val: []), tl], _Ts, _I) do
    tl
  end

  defp simplify_remote_call(
         :erlang,
         :setelement,
         [r_b_literal(val: pos), r_b_literal(val: tuple), r_b_var() = value],
         _Ts,
         i
       )
       when is_integer(pos) and 1 <= pos and
              pos <= tuple_size(tuple) do
    els0 =
      for el <- :erlang.tuple_to_list(tuple) do
        r_b_literal(val: el)
      end

    {bef, [_ | aft]} = split(pos - 1, els0)
    els = bef ++ [value | aft]
    r_b_set(i, op: :put_tuple, args: els)
  end

  defp simplify_remote_call(mod, name, args, _Ts, i) do
    case :erl_bifs.is_pure(mod, name, length(args)) do
      true ->
        simplify_pure_call(mod, name, args, i)

      false ->
        i
    end
  end

  defp simplify_pure_call(mod, name, args0, i) do
    case make_literal_list(args0) do
      :none ->
        i

      args ->
        try do
          apply(mod, name, args)
        catch
          _, _ ->
            i
        else
          val ->
            case :cerl.is_literal_term(val) do
              true ->
                r_b_literal(val: val)

              false ->
                i
            end
        end
    end
  end

  defp any_non_numeric_argument([r_b_literal(val: lit) | _], _Ts) do
    is_non_numeric(lit)
  end

  defp any_non_numeric_argument([r_b_var() = v | t], ts) do
    is_non_numeric_type(
      raw_type(
        v,
        ts
      )
    ) or any_non_numeric_argument(t, ts)
  end

  defp any_non_numeric_argument([], _Ts) do
    false
  end

  defp is_non_numeric([h | t]) do
    is_non_numeric(h) and is_non_numeric(t)
  end

  defp is_non_numeric(tuple) when is_tuple(tuple) do
    is_non_numeric_tuple(tuple, tuple_size(tuple))
  end

  defp is_non_numeric(map) when is_map(map) do
    is_non_numeric(:maps.values(map))
  end

  defp is_non_numeric(num) when is_number(num) do
    false
  end

  defp is_non_numeric(_) do
    true
  end

  defp is_non_numeric_tuple(tuple, el) when el >= 1 do
    is_non_numeric(
      :erlang.element(
        el,
        tuple
      )
    ) and
      is_non_numeric_tuple(
        tuple,
        el - 1
      )
  end

  defp is_non_numeric_tuple(_Tuple, 0) do
    true
  end

  defp is_non_numeric_type(r_t_atom()) do
    true
  end

  defp is_non_numeric_type(r_t_bitstring()) do
    true
  end

  defp is_non_numeric_type(r_t_cons(type: type, terminator: terminator)) do
    is_non_numeric_type(type) and is_non_numeric_type(terminator)
  end

  defp is_non_numeric_type(r_t_list(type: type, terminator: terminator)) do
    is_non_numeric_type(type) and is_non_numeric_type(terminator)
  end

  defp is_non_numeric_type(r_t_map(super_value: value)) do
    is_non_numeric_type(value)
  end

  defp is_non_numeric_type(nil) do
    true
  end

  defp is_non_numeric_type(r_t_tuple(size: size, exact: true, elements: types))
       when map_size(types) === size do
    is_non_numeric_tuple_type(size, types)
  end

  defp is_non_numeric_type(_) do
    false
  end

  defp is_non_numeric_tuple_type(0, _Types) do
    true
  end

  defp is_non_numeric_tuple_type(pos, types) do
    is_non_numeric_type(
      :erlang.map_get(
        pos,
        types
      )
    ) and
      is_non_numeric_tuple_type(
        pos - 1,
        types
      )
  end

  defp make_literal_list(args) do
    make_literal_list(args, [])
  end

  defp make_literal_list([r_b_literal(val: h) | t], acc) do
    make_literal_list(t, [h | acc])
  end

  defp make_literal_list([_ | _], _) do
    :none
  end

  defp make_literal_list([], acc) do
    reverse(acc)
  end

  defp is_safe_bool_op([lHS, rHS], ts) do
    lType = raw_type(lHS, ts)
    rType = raw_type(rHS, ts)
    :beam_types.is_boolean_type(lType) and :beam_types.is_boolean_type(rType)
  end

  defp eval_bif(r_b_set(op: {:bif, bif}, args: args) = i, ts) do
    arity = length(args)

    case :erl_bifs.is_pure(:erlang, bif, arity) do
      false ->
        i

      true ->
        case make_literal_list(args) do
          :none ->
            eval_type_test_bif(i, bif, raw_types(args, ts))

          litArgs ->
            try do
              apply(:erlang, bif, litArgs)
            catch
              :error, _ ->
                i
            else
              val ->
                r_b_literal(val: val)
            end
        end
    end
  end

  defp eval_type_test_bif(i, :is_atom, [type]) do
    eval_type_test_bif_1(i, type, r_t_atom())
  end

  defp eval_type_test_bif(i, :is_binary, [type]) do
    eval_type_test_bif_1(i, type, r_t_bs_matchable(tail_unit: 8))
  end

  defp eval_type_test_bif(i, :is_bitstring, [type]) do
    eval_type_test_bif_1(i, type, r_t_bs_matchable())
  end

  defp eval_type_test_bif(i, :is_boolean, [type]) do
    case :beam_types.is_boolean_type(type) do
      true ->
        r_b_literal(val: true)

      false ->
        case :beam_types.meet(type, r_t_atom()) do
          r_t_atom(elements: [_ | _] = es) ->
            case any(&is_boolean/1, es) do
              true ->
                i

              false ->
                r_b_literal(val: false)
            end

          r_t_atom() ->
            i

          :none ->
            r_b_literal(val: false)
        end
    end
  end

  defp eval_type_test_bif(i, :is_float, [type]) do
    eval_type_test_bif_1(i, type, r_t_float())
  end

  defp eval_type_test_bif(i, :is_function, [type]) do
    eval_type_test_bif_1(i, type, r_t_fun())
  end

  defp eval_type_test_bif(i, :is_integer, [type]) do
    eval_type_test_bif_1(i, type, r_t_integer())
  end

  defp eval_type_test_bif(i, :is_list, [type]) do
    eval_type_test_bif_1(i, type, r_t_list())
  end

  defp eval_type_test_bif(i, :is_map, [type]) do
    eval_type_test_bif_1(i, type, r_t_map())
  end

  defp eval_type_test_bif(i, :is_number, [type]) do
    eval_type_test_bif_1(i, type, :number)
  end

  defp eval_type_test_bif(i, :is_tuple, [type]) do
    eval_type_test_bif_1(i, type, r_t_tuple())
  end

  defp eval_type_test_bif(i, op, types) do
    case types do
      [r_t_integer(), r_t_integer(elements: {0, 0})]
      when op === :+ or
             op === :- or op === :bor or op === :bxor ->
        r_b_set(args: [result, _]) = i
        result

      [r_t_integer(), r_t_integer(elements: {0, 0})]
      when op === :* or
             op === :band ->
        r_b_literal(val: 0)

      [r_t_integer(), r_t_integer(elements: {1, 1})] when op === :* or op === :div ->
        r_b_set(args: [result, _]) = i
        result

      [r_t_integer(elements: {lMin, lMax}), r_t_integer(elements: {rMin, rMax})] ->
        case is_inequality_op(op) do
          true ->
            case {apply(:erlang, op, [lMin, rMin]), apply(:erlang, op, [lMax, rMin]),
                  apply(:erlang, op, [lMin, rMax]), apply(:erlang, op, [lMax, rMax])} do
              {bool, bool, bool, bool} ->
                r_b_literal(val: bool)

              _ ->
                i
            end

          false ->
            i
        end

      _ ->
        i
    end
  end

  defp is_inequality_op(:<) do
    true
  end

  defp is_inequality_op(:"=<") do
    true
  end

  defp is_inequality_op(:>) do
    true
  end

  defp is_inequality_op(:>=) do
    true
  end

  defp is_inequality_op(_) do
    false
  end

  defp eval_type_test_bif_1(i, argType, required) do
    case :beam_types.meet(argType, required) do
      ^argType ->
        r_b_literal(val: true)

      :none ->
        r_b_literal(val: false)

      _ ->
        i
    end
  end

  defp simplify_args(args, ts, sub) do
    for arg <- args do
      simplify_arg(arg, ts, sub)
    end
  end

  defp simplify_arg(r_b_var() = arg0, ts, sub) do
    case sub_arg(arg0, sub) do
      r_b_literal() = litArg ->
        litArg

      r_b_var() = arg ->
        case :beam_types.get_singleton_value(
               raw_type(
                 arg,
                 ts
               )
             ) do
          {:ok, val} ->
            r_b_literal(val: val)

          :error ->
            arg
        end
    end
  end

  defp simplify_arg(r_b_remote(mod: mod, name: name) = rem, ts, sub) do
    r_b_remote(rem,
      mod: simplify_arg(mod, ts, sub),
      name: simplify_arg(name, ts, sub)
    )
  end

  defp simplify_arg(arg, _Ts, _Sub) do
    arg
  end

  defp sub_arg(r_b_var() = old, sub) do
    case sub do
      %{^old => new} ->
        new

      %{} ->
        old
    end
  end

  defp is_float_op(:-, [r_t_float()]) do
    true
  end

  defp is_float_op(:/, [_, _]) do
    true
  end

  defp is_float_op(op, [r_t_float(), _Other]) do
    is_float_op_1(op)
  end

  defp is_float_op(op, [_Other, r_t_float()]) do
    is_float_op_1(op)
  end

  defp is_float_op(_, _) do
    false
  end

  defp is_float_op_1(:+) do
    true
  end

  defp is_float_op_1(:-) do
    true
  end

  defp is_float_op_1(:*) do
    true
  end

  defp is_float_op_1(_) do
    false
  end

  defp anno_float_arg(r_t_float()) do
    :float
  end

  defp anno_float_arg(_) do
    :convert
  end

  defp return_type(succTypes0, callArgs0) do
    succTypes = st_filter_reachable(succTypes0, callArgs0, [], [])
    st_join_return_types(succTypes, :none)
  end

  defp st_filter_reachable(
         [
           {succArgs, {:call_self, selfArgs}} = succType
           | rest
         ],
         callArgs0,
         deferred,
         acc
       ) do
    case st_is_reachable(succArgs, callArgs0) do
      true ->
        callArgs = parallel_join(selfArgs, callArgs0)
        st_filter_reachable(rest, callArgs, deferred, acc)

      false ->
        st_filter_reachable(rest, callArgs0, [succType | deferred], acc)
    end
  end

  defp st_filter_reachable([succType | rest], callArgs, deferred, acc) do
    st_filter_reachable(rest, callArgs, deferred, [succType | acc])
  end

  defp st_filter_reachable([], callArgs, deferred, acc) do
    case st_any_reachable(deferred, callArgs) do
      true ->
        st_filter_reachable(deferred, callArgs, [], acc)

      false ->
        for {succArgs, _} = sT <- acc,
            st_is_reachable(succArgs, callArgs) do
          sT
        end
    end
  end

  defp st_join_return_types([{_SuccArgs, succRet} | rest], acc0) do
    st_join_return_types(
      rest,
      :beam_types.join(succRet, acc0)
    )
  end

  defp st_join_return_types([], acc) do
    acc
  end

  defp st_any_reachable([{succArgs, _} | succType], callArgs) do
    case st_is_reachable(succArgs, callArgs) do
      true ->
        true

      false ->
        st_any_reachable(succType, callArgs)
    end
  end

  defp st_any_reachable([], _CallArgs) do
    false
  end

  defp st_is_reachable([a | succArgs], [b | callArgs]) do
    case :beam_types.meet(a, b) do
      :none ->
        false

      _Other ->
        st_is_reachable(succArgs, callArgs)
    end
  end

  defp st_is_reachable([], []) do
    true
  end

  defp update_success_types(r_b_ret(arg: arg), ts, ds, meta, succTypes) do
    r_metadata(func_id: funcId, limit_return: limited, params: params) = meta

    retType =
      case ds do
        %{^arg => r_b_set(op: :call, args: [^funcId | args])} ->
          {:call_self, argument_types(args, ts)}

        %{} ->
          argument_type(arg, ts)
      end

    argTypes = argument_types(params, ts)

    case limited do
      true ->
        ust_limited(succTypes, argTypes, retType)

      false ->
        ust_unlimited(succTypes, argTypes, retType)
    end
  end

  defp update_success_types(_Last, _Ts, _Ds, _Meta, succTypes) do
    succTypes
  end

  defp ust_limited(succTypes, callArgs, {:call_self, selfArgs}) do
    newArgs = parallel_join(callArgs, selfArgs)
    ust_limited_1(succTypes, newArgs, :none)
  end

  defp ust_limited(succTypes, callArgs, callRet) do
    ust_limited_1(succTypes, callArgs, callRet)
  end

  defp ust_limited_1([], argTypes, retType) do
    [{argTypes, retType}]
  end

  defp ust_limited_1([{succArgs, succRet}], callArgs, callRet) do
    newTypes = parallel_join(succArgs, callArgs)
    newType = :beam_types.join(succRet, callRet)
    [{newTypes, newType}]
  end

  defp ust_unlimited(succTypes, _CallArgs, :none) do
    succTypes
  end

  defp ust_unlimited([{sameArgs, sameType} | _] = succTypes, sameArgs, sameType) do
    succTypes
  end

  defp ust_unlimited([succType | succTypes], callArgs, callRet) do
    [succType | ust_unlimited(succTypes, callArgs, callRet)]
  end

  defp ust_unlimited([], callArgs, callRet) do
    [{callArgs, callRet}]
  end

  defp update_successors(
         r_b_br(bool: r_b_literal(val: true), succ: succ) = last,
         ts,
         _Ds,
         ls,
         _UsedOnce
       ) do
    {last, update_successor(succ, ts, ls)}
  end

  defp update_successors(
         r_b_br(bool: r_b_var() = bool, succ: succ, fail: fail) = last0,
         ts,
         ds,
         ls0,
         usedOnce
       ) do
    isTempVar = :cerl_sets.is_element(bool, usedOnce)

    case infer_types_br(bool, ts, isTempVar, ds) do
      {%{} = succTs, %{} = failTs} ->
        ls1 = update_successor(succ, succTs, ls0)
        ls = update_successor(fail, failTs, ls1)
        {last0, ls}

      {%{} = succTs, :none} ->
        last = r_b_br(last0, bool: r_b_literal(val: true), fail: succ)
        {last, update_successor(succ, succTs, ls0)}

      {:none, %{} = failTs} ->
        last = r_b_br(last0, bool: r_b_literal(val: true), succ: fail)
        {last, update_successor(fail, failTs, ls0)}
    end
  end

  defp update_successors(
         r_b_switch(arg: r_b_var() = v, fail: fail0, list: list0) = last0,
         ts,
         ds,
         ls0,
         usedOnce
       ) do
    isTempVar = :cerl_sets.is_element(v, usedOnce)
    {list1, failTs, ls1} = update_switch(list0, v, raw_type(v, ts), ts, ds, ls0, isTempVar, [])

    case failTs do
      :none ->
        case list1 do
          [{r_b_literal(val: 0), _} | _] ->
            {list, [{_, fail}]} = split(length(list1) - 1, list1)
            last = r_b_switch(last0, fail: fail, list: list)
            {last, ls1}

          [{_, fail} | list] ->
            last = r_b_switch(last0, fail: fail, list: list)
            {last, ls1}
        end

      %{} ->
        ls = update_successor(fail0, failTs, ls1)
        last = r_b_switch(last0, list: list1)
        {last, ls}
    end
  end

  defp update_successors(r_b_ret() = last, _Ts, _Ds, ls, _UsedOnce) do
    {last, ls}
  end

  defp update_switch([{val, lbl} = sw | list], v, failType0, ts, ds, ls0, isTempVar, acc) do
    failType =
      :beam_types.subtract(
        failType0,
        raw_type(val, ts)
      )

    case infer_types_switch(v, val, ts, isTempVar, ds) do
      :none ->
        update_switch(list, v, failType, ts, ds, ls0, isTempVar, acc)

      swTs ->
        ls = update_successor(lbl, swTs, ls0)
        update_switch(list, v, failType, ts, ds, ls, isTempVar, [sw | acc])
    end
  end

  defp update_switch([], _V, :none, _Ts, _Ds, ls, _IsTempVar, acc) do
    {reverse(acc), :none, ls}
  end

  defp update_switch([], v, failType, ts, ds, ls, isTempVar, acc) do
    failTs =
      case :beam_types.get_singleton_value(failType) do
        {:ok, value} ->
          lit = r_b_literal(val: value)
          infer_types_switch(v, lit, ts, isTempVar, ds)

        :error when isTempVar ->
          ts_remove_var(v, ts)

        :error ->
          %{ts | v => failType}
      end

    {reverse(acc), failTs, ls}
  end

  defp update_successor(1, _Ts, ls) do
    ls
  end

  defp update_successor(s, ts0, ls) do
    case ls do
      %{^s => {:outgoing, _}} ->
        ls

      %{^s => {:incoming, inTs}} ->
        ts = join_types(ts0, inTs)
        %{ls | s => {:incoming, ts}}

      %{} ->
        Map.put(ls, s, {:incoming, ts0})
    end
  end

  defp update_types(r_b_set(op: op, dst: dst, anno: anno, args: args), ts, ds) do
    t = type(op, args, anno, ts, ds)
    Map.put(ts, dst, t)
  end

  defp type({:bif, bif}, args, _Anno, ts, _Ds) do
    argTypes = normalized_types(args, ts)
    {retType, _, _} = :beam_call_types.types(:erlang, bif, argTypes)
    retType
  end

  defp type(:bs_init, _Args, _Anno, _Ts, _Ds) do
    r_t_bitstring()
  end

  defp type(:bs_extract, [ctx], _Anno, _Ts, ds) do
    r_b_set(op: :bs_match, args: args) = :erlang.map_get(ctx, ds)
    bs_match_type(args)
  end

  defp type(:bs_start_match, [_, src], _Anno, ts, _Ds) do
    case :beam_types.meet(r_t_bs_matchable(), raw_type(src, ts)) do
      :none ->
        :none

      t ->
        unit = :beam_types.get_bs_matchable_unit(t)
        r_t_bs_context(tail_unit: unit)
    end
  end

  defp type(
         :bs_match,
         [
           r_b_literal(val: :binary),
           ctx,
           _Flags,
           r_b_literal(val: :all),
           r_b_literal(val: opUnit)
         ],
         _Anno,
         ts,
         _Ds
       ) do
    ctxType = raw_type(ctx, ts)
    opType = r_t_bs_context(tail_unit: opUnit)
    :beam_types.meet(ctxType, opType)
  end

  defp type(:bs_match, args, _Anno, ts, _Ds) do
    [_, ctx | _] = args
    r_t_bs_context(tail_unit: ctxUnit) = raw_type(ctx, ts)
    opUnit = bs_match_stride(args, ts)
    r_t_bs_context(tail_unit: gcd(opUnit, ctxUnit))
  end

  defp type(:bs_get_tail, [ctx], _Anno, ts, _Ds) do
    r_t_bs_context(tail_unit: unit) = raw_type(ctx, ts)
    r_t_bitstring(size_unit: unit)
  end

  defp type(
         :call,
         [r_b_remote(mod: r_b_literal(val: mod), name: r_b_literal(val: name)) | args],
         _Anno,
         ts,
         _Ds
       ) do
    argTypes = normalized_types(args, ts)
    {retType, _, _} = :beam_call_types.types(mod, name, argTypes)
    retType
  end

  defp type(:call, [r_b_remote() | _Args], _Anno, _Ts, _Ds) do
    :any
  end

  defp type(:call, [r_b_local() | _Args], anno, _Ts, _Ds) do
    case anno do
      %{result_type: type} ->
        type

      %{} ->
        :any
    end
  end

  defp type(:call, [r_b_var() | _Args], anno, _Ts, _Ds) do
    case anno do
      %{result_type: type} ->
        type

      %{} ->
        :any
    end
  end

  defp type(:call, [r_b_literal(val: fun) | args], _Anno, _Ts, _Ds) do
    case is_function(fun, length(args)) do
      true ->
        :any

      false ->
        :none
    end
  end

  defp type(:extract, [v, r_b_literal(val: idx)], _Anno, _Ts, ds) do
    case :erlang.map_get(v, ds) do
      r_b_set(op: :landingpad) when idx === 0 ->
        r_t_atom(elements: [:error, :exit, :throw])

      r_b_set(op: :landingpad) when idx === 1 ->
        :any

      r_b_set(op: :landingpad) when idx === 2 ->
        :any
    end
  end

  defp type(:get_hd, [src], _Anno, ts, _Ds) do
    srcType = r_t_cons() = normalized_type(src, ts)
    {retType, _, _} = :beam_call_types.types(:erlang, :hd, [srcType])
    retType
  end

  defp type(:get_tl, [src], _Anno, ts, _Ds) do
    srcType = r_t_cons() = normalized_type(src, ts)
    {retType, _, _} = :beam_call_types.types(:erlang, :tl, [srcType])
    retType
  end

  defp type(:get_map_element, [_, _] = args0, _Anno, ts, _Ds) do
    [r_t_map() = map, key] = normalized_types(args0, ts)
    {retType, _, _} = :beam_call_types.types(:erlang, :map_get, [key, map])
    retType
  end

  defp type(:get_tuple_element, [tuple, offset], _Anno, ts, _Ds) do
    r_t_tuple(size: size, elements: es) = normalized_type(tuple, ts)
    r_b_literal(val: n) = offset
    true = size > n
    :beam_types.get_tuple_element(n + 1, es)
  end

  defp type(:has_map_field, [_, _] = args0, _Anno, ts, _Ds) do
    [r_t_map() = map, key] = normalized_types(args0, ts)
    {retType, _, _} = :beam_call_types.types(:erlang, :is_map_key, [key, map])
    retType
  end

  defp type(:is_nonempty_list, [_], _Anno, _Ts, _Ds) do
    :beam_types.make_boolean()
  end

  defp type(:is_tagged_tuple, [_, r_b_literal(), r_b_literal()], _Anno, _Ts, _Ds) do
    :beam_types.make_boolean()
  end

  defp type(makeFun, [r_b_local(arity: totalArity) | env], anno, _Ts, _Ds)
       when makeFun === :make_fun or
              makeFun === :old_make_fun do
    retType =
      case anno do
        %{result_type: type} ->
          type

        %{} ->
          :any
      end

    r_t_fun(arity: totalArity - length(env), type: retType)
  end

  defp type(:put_map, [_Kind, map | ss], _Anno, ts, _Ds) do
    put_map_type(map, ss, ts)
  end

  defp type(:put_list, [head, tail], _Anno, ts, _Ds) do
    headType = raw_type(head, ts)
    tailType = raw_type(tail, ts)
    :beam_types.make_cons(headType, tailType)
  end

  defp type(:put_tuple, args, _Anno, ts, _Ds) do
    {es, _} =
      foldl(
        fn arg, {es0, index} ->
          type = raw_type(arg, ts)
          es = :beam_types.set_tuple_element(index, type, es0)
          {es, index + 1}
        end,
        {%{}, 1},
        args
      )

    r_t_tuple(exact: true, size: length(args), elements: es)
  end

  defp type(:resume, [_, _], _Anno, _Ts, _Ds) do
    :none
  end

  defp type(_, _, _, _, _) do
    :any
  end

  defp put_map_type(map, ss, ts) do
    pmt_1(ss, ts, normalized_type(map, ts))
  end

  defp pmt_1([key0, value0 | ss], ts, acc0) do
    key = normalized_type(key0, ts)
    value = normalized_type(value0, ts)
    {acc, _, _} = :beam_call_types.types(:maps, :put, [key, value, acc0])
    pmt_1(ss, ts, acc)
  end

  defp pmt_1([], _Ts, acc) do
    acc
  end

  defp bs_match_stride([r_b_literal(val: type) | args], ts) do
    bs_match_stride(type, args, ts)
  end

  defp bs_match_stride(_, [_, _, size, r_b_literal(val: unit)], ts) do
    case raw_type(size, ts) do
      r_t_integer(elements: {sz, sz}) when is_integer(sz) ->
        sz * unit

      _ ->
        unit
    end
  end

  defp bs_match_stride(:string, [_, r_b_literal(val: string)], _) do
    bit_size(string)
  end

  defp bs_match_stride(:utf8, _, _) do
    8
  end

  defp bs_match_stride(:utf16, _, _) do
    16
  end

  defp bs_match_stride(:utf32, _, _) do
    32
  end

  defp bs_match_stride(_, _, _) do
    1
  end

  defp bs_match_type([r_b_literal(val: type) | args]) do
    bs_match_type(type, args)
  end

  defp bs_match_type(:binary, args) do
    [_, _, _, r_b_literal(val: u)] = args
    r_t_bitstring(size_unit: u)
  end

  defp bs_match_type(:float, _) do
    r_t_float()
  end

  defp bs_match_type(:integer, args) do
    case args do
      [_, r_b_literal(val: flags), r_b_literal(val: size), r_b_literal(val: unit)]
      when size * unit < 64 ->
        numBits = size * unit

        case member(:unsigned, flags) do
          true ->
            :beam_types.make_integer(0, 1 <<< (numBits - 1))

          false ->
            r_t_integer()
        end

      [_ | _] ->
        r_t_integer()
    end
  end

  defp bs_match_type(:skip, _) do
    :any
  end

  defp bs_match_type(:string, _) do
    :any
  end

  defp bs_match_type(:utf8, _) do
    :beam_types.make_integer(0, 1_114_111)
  end

  defp bs_match_type(:utf16, _) do
    :beam_types.make_integer(0, 1_114_111)
  end

  defp bs_match_type(:utf32, _) do
    :beam_types.make_integer(0, 1_114_111)
  end

  defp normalized_types(values, ts) do
    for val <- values do
      normalized_type(val, ts)
    end
  end

  defp normalized_type(v, ts) do
    :beam_types.normalize(raw_type(v, ts))
  end

  defp argument_types(values, ts) do
    for val <- values do
      argument_type(val, ts)
    end
  end

  defp argument_type(v, ts) do
    :beam_types.limit_depth(raw_type(v, ts))
  end

  defp raw_types(values, ts) do
    for val <- values do
      raw_type(val, ts)
    end
  end

  defp raw_type(r_b_literal(val: value), _Ts) do
    :beam_types.make_type_from_value(value)
  end

  defp raw_type(v, ts) do
    :erlang.map_get(v, ts)
  end

  defp infer_types_br(r_b_var() = v, ts, isTempVar, ds) do
    %{^v => r_b_set(op: op, args: args)} = ds
    {posTypes, negTypes} = infer_type(op, args, ts, ds)
    succTs0 = meet_types(posTypes, ts)
    failTs0 = subtract_types(negTypes, ts)

    case isTempVar do
      true ->
        succTs = ts_remove_var(v, succTs0)
        failTs = ts_remove_var(v, failTs0)
        {succTs, failTs}

      false ->
        succTs = infer_br_value(v, true, succTs0)
        failTs = infer_br_value(v, false, failTs0)
        {succTs, failTs}
    end
  end

  defp infer_br_value(_V, _Bool, :none) do
    :none
  end

  defp infer_br_value(v, bool, newTs) do
    %{^v => t} = newTs

    case :beam_types.is_boolean_type(t) do
      true ->
        %{newTs | v => :beam_types.make_atom(bool)}

      false ->
        newTs
    end
  end

  defp infer_types_switch(v, lit, ts0, isTempVar, ds) do
    {posTypes, _} = infer_type({:bif, :"=:="}, [v, lit], ts0, ds)
    ts = meet_types(posTypes, ts0)

    case isTempVar do
      true ->
        ts_remove_var(v, ts)

      false ->
        ts
    end
  end

  defp ts_remove_var(_V, :none) do
    :none
  end

  defp ts_remove_var(v, ts) do
    :maps.remove(v, ts)
  end

  defp infer_type({:succeeded, _}, [r_b_var() = src], ts, ds) do
    r_b_set(op: op, args: args) = :maps.get(src, ds)
    infer_success_type(op, args, ts, ds)
  end

  defp infer_type(
         :is_tagged_tuple,
         [r_b_var() = src, r_b_literal(val: size), r_b_literal() = tag],
         _Ts,
         _Ds
       ) do
    es = :beam_types.set_tuple_element(1, raw_type(tag, %{}), %{})
    t = {src, r_t_tuple(exact: true, size: size, elements: es)}
    {[t], [t]}
  end

  defp infer_type(:is_nonempty_list, [r_b_var() = src], _Ts, _Ds) do
    t = {src, r_t_cons()}
    {[t], [t]}
  end

  defp infer_type({:bif, :is_atom}, [arg], _Ts, _Ds) do
    t = {arg, r_t_atom()}
    {[t], [t]}
  end

  defp infer_type({:bif, :is_binary}, [arg], _Ts, _Ds) do
    t = {arg, r_t_bitstring(size_unit: 8)}
    {[t], [t]}
  end

  defp infer_type({:bif, :is_bitstring}, [arg], _Ts, _Ds) do
    t = {arg, r_t_bitstring()}
    {[t], [t]}
  end

  defp infer_type({:bif, :is_boolean}, [arg], _Ts, _Ds) do
    t = {arg, :beam_types.make_boolean()}
    {[t], [t]}
  end

  defp infer_type({:bif, :is_float}, [arg], _Ts, _Ds) do
    t = {arg, r_t_float()}
    {[t], [t]}
  end

  defp infer_type({:bif, :is_integer}, [arg], _Ts, _Ds) do
    t = {arg, r_t_integer()}
    {[t], [t]}
  end

  defp infer_type({:bif, :is_list}, [arg], _Ts, _Ds) do
    t = {arg, r_t_list()}
    {[t], [t]}
  end

  defp infer_type({:bif, :is_map}, [arg], _Ts, _Ds) do
    t = {arg, r_t_map()}
    {[t], [t]}
  end

  defp infer_type({:bif, :is_number}, [arg], _Ts, _Ds) do
    t = {arg, :number}
    {[t], [t]}
  end

  defp infer_type({:bif, :is_tuple}, [arg], _Ts, _Ds) do
    t = {arg, r_t_tuple()}
    {[t], [t]}
  end

  defp infer_type({:bif, :"=:="}, [r_b_var() = lHS, r_b_var() = rHS], ts, _Ds) do
    lType = raw_type(lHS, ts)
    rType = raw_type(rHS, ts)
    type = :beam_types.meet(lType, rType)

    posTypes =
      for {v, origType} <- [{lHS, lType}, {rHS, rType}],
          origType !== type do
        {v, type}
      end

    negTypes =
      case :beam_types.is_singleton_type(type) do
        true ->
          posTypes

        false ->
          []
      end

    {posTypes, negTypes}
  end

  defp infer_type({:bif, :"=:="}, [r_b_var() = src, r_b_literal() = lit], ts, ds) do
    def__ = :maps.get(src, ds)
    litType = raw_type(lit, ts)

    posTypes = [
      {src, litType}
      | infer_eq_lit(
          def__,
          litType
        )
    ]

    negTypes =
      case :beam_types.is_singleton_type(litType) do
        true ->
          posTypes

        false ->
          []
      end

    {posTypes, negTypes}
  end

  defp infer_type(_Op, _Args, _Ts, _Ds) do
    {[], []}
  end

  defp infer_success_type({:bif, op}, args, ts, _Ds) do
    argTypes = normalized_types(args, ts)
    {_, posTypes0, canSubtract} = :beam_call_types.types(:erlang, op, argTypes)

    posTypes =
      for {r_b_var(), _} = t <-
            zip(
              args,
              posTypes0
            ) do
        t
      end

    case canSubtract do
      true ->
        {posTypes, posTypes}

      false ->
        {posTypes, []}
    end
  end

  defp infer_success_type(:call, [r_b_var() = fun | args], _Ts, _Ds) do
    t = {fun, r_t_fun(arity: length(args))}
    {[t], []}
  end

  defp infer_success_type(:bs_start_match, [_, r_b_var() = src], _Ts, _Ds) do
    t = {src, r_t_bs_matchable()}
    {[t], [t]}
  end

  defp infer_success_type(
         :bs_match,
         [
           r_b_literal(val: :binary),
           ctx,
           _Flags,
           r_b_literal(val: :all),
           r_b_literal(val: opUnit)
         ],
         _Ts,
         _Ds
       ) do
    t = {ctx, r_t_bs_context(tail_unit: opUnit)}
    {[t], [t]}
  end

  defp infer_success_type(_Op, _Args, _Ts, _Ds) do
    {[], []}
  end

  defp infer_eq_lit(
         r_b_set(op: {:bif, :tuple_size}, args: [r_b_var() = tuple]),
         r_t_integer(elements: {size, size})
       ) do
    [{tuple, r_t_tuple(exact: true, size: size)}]
  end

  defp infer_eq_lit(
         r_b_set(
           op: :get_tuple_element,
           args: [r_b_var() = tuple, r_b_literal(val: n)]
         ),
         litType
       ) do
    index = n + 1

    case :beam_types.set_tuple_element(index, litType, %{}) do
      %{^index => _} = es ->
        [{tuple, r_t_tuple(size: index, elements: es)}]

      %{} ->
        []
    end
  end

  defp infer_eq_lit(_, _) do
    []
  end

  defp join_types(ts, ts) do
    ts
  end

  defp join_types(lHS, rHS) do
    cond do
      map_size(lHS) < map_size(rHS) ->
        join_types_1(:maps.keys(lHS), rHS, lHS)

      true ->
        join_types_1(:maps.keys(rHS), lHS, rHS)
    end
  end

  defp join_types_1([v | vs], bigger, smaller) do
    case {bigger, smaller} do
      {%{^v => same}, %{^v => same}} ->
        join_types_1(vs, bigger, smaller)

      {%{^v => lHS}, %{^v => rHS}} ->
        t = :beam_types.join(lHS, rHS)
        join_types_1(vs, bigger, %{smaller | v => t})

      {%{}, %{^v => _}} ->
        join_types_1(vs, bigger, :maps.remove(v, smaller))
    end
  end

  defp join_types_1([], _Bigger, smaller) do
    smaller
  end

  defp meet_types([{v, t0} | vs], ts) do
    %{^v => t1} = ts

    case :beam_types.meet(t0, t1) do
      :none ->
        :none

      ^t1 ->
        meet_types(vs, ts)

      t ->
        meet_types(vs, %{ts | v => t})
    end
  end

  defp meet_types([], ts) do
    ts
  end

  defp subtract_types([{v, t0} | vs], ts) do
    %{^v => t1} = ts

    case :beam_types.subtract(t1, t0) do
      :none ->
        :none

      ^t1 ->
        subtract_types(vs, ts)

      t ->
        subtract_types(vs, %{ts | v => t})
    end
  end

  defp subtract_types([], ts) do
    ts
  end

  defp parallel_join([a | as], [b | bs]) do
    [:beam_types.join(a, b) | parallel_join(as, bs)]
  end

  defp parallel_join([], []) do
    []
  end

  defp gcd(a, b) do
    case rem(a, b) do
      0 ->
        b

      x ->
        gcd(b, x)
    end
  end

  defp init_metadata(funcId, linear, params) do
    {retCounter, map0} = init_metadata_1(reverse(linear), 0, %{})
    map = :maps.without(params, map0)
    usedOnce = :cerl_sets.from_list(:maps.keys(map))

    r_metadata(
      func_id: funcId,
      limit_return: retCounter >= 30,
      params: params,
      used_once: usedOnce
    )
  end

  defp init_metadata_1([{l, r_b_blk(is: is, last: last)} | bs], retCounter0, uses0) do
    retCounter =
      case last do
        r_b_ret() ->
          retCounter0 + 1

        _ ->
          retCounter0
      end

    uses1 = used_once_last_uses(:beam_ssa.used(last), l, uses0)
    uses = used_once_2(reverse(is), l, uses1)
    init_metadata_1(bs, retCounter, uses)
  end

  defp init_metadata_1([], retCounter, uses) do
    {retCounter, uses}
  end

  defp used_once_2([r_b_set(dst: dst) = i | is], l, uses0) do
    uses = used_once_uses(:beam_ssa.used(i), l, uses0)

    case uses do
      %{^dst => [^l]} ->
        used_once_2(is, l, uses)

      %{} ->
        used_once_2(is, l, :maps.remove(dst, uses))
    end
  end

  defp used_once_2([], _, uses) do
    uses
  end

  defp used_once_uses([v | vs], l, uses) do
    case uses do
      %{^v => :more_than_once} ->
        used_once_uses(vs, l, uses)

      %{} ->
        used_once_uses(vs, l, Map.put(uses, v, :more_than_once))
    end
  end

  defp used_once_uses([], _, uses) do
    uses
  end

  defp used_once_last_uses([v | vs], l, uses) do
    case uses do
      %{^v => [_]} ->
        used_once_last_uses(vs, l, %{uses | v => :more_than_once})

      %{^v => :more_than_once} ->
        used_once_last_uses(vs, l, uses)

      %{} ->
        used_once_last_uses(vs, l, Map.put(uses, v, [l]))
    end
  end

  defp used_once_last_uses([], _, uses) do
    uses
  end

  Record.defrecord(:r_worklist, :worklist, counter: 0, elements: :gb_trees.empty(), indexes: %{})

  defp wl_new() do
    r_worklist()
  end

  defp wl_add(
         element,
         r_worklist(counter: counter, elements: es, indexes: is)
       ) do
    case is do
      %{^element => index} ->
        wl_add_1(element, counter, :gb_trees.delete(index, es), is)

      %{} ->
        wl_add_1(element, counter, es, is)
    end
  end

  defp wl_add_1(element, counter0, es0, is0) do
    counter = counter0 + 1
    es = :gb_trees.insert(counter, element, es0)
    is = Map.put(is0, element, counter)
    r_worklist(counter: counter, elements: es, indexes: is)
  end

  defp wl_changed(r_worklist(counter: same), r_worklist(counter: same)) do
    false
  end

  defp wl_changed(r_worklist(), r_worklist()) do
    true
  end

  defp wl_defer_list(
         elements,
         r_worklist(counter: counter, elements: es, indexes: is)
       ) do
    wl_defer_list_1(elements, counter, es, is)
  end

  defp wl_defer_list_1([element | elements], counter0, es0, is0) do
    case is0 do
      %{^element => _} ->
        wl_defer_list_1(elements, counter0, es0, is0)

      %{} ->
        counter = counter0 + 1
        es = :gb_trees.insert(-counter, element, es0)
        is = Map.put(is0, element, -counter)
        wl_defer_list_1(elements, counter, es, is)
    end
  end

  defp wl_defer_list_1([], counter, es, is) do
    r_worklist(counter: counter, elements: es, indexes: is)
  end

  defp wl_next(r_worklist(indexes: is)) when is === %{} do
    :empty
  end

  defp wl_next(r_worklist(elements: es, indexes: is)) when is !== %{} do
    {_Key, element} = :gb_trees.largest(es)
    {:ok, element}
  end

  defp wl_pop(
         element,
         r_worklist(counter: counter0, elements: es0, indexes: is0) = wl
       ) do
    counter = counter0 + 1
    {_Key, ^element, es} = :gb_trees.take_largest(es0)
    is = :maps.remove(element, is0)
    r_worklist(wl, counter: counter, elements: es, indexes: is)
  end
end
