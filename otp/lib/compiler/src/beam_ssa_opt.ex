defmodule :m_beam_ssa_opt do
  use Bitwise

  import :lists,
    only: [
      all: 2,
      append: 1,
      duplicate: 2,
      flatten: 1,
      foldl: 3,
      keyfind: 3,
      last: 1,
      mapfoldl: 3,
      member: 2,
      partition: 2,
      reverse: 1,
      reverse: 2,
      sort: 1,
      splitwith: 2,
      takewhile: 2,
      unzip: 1
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

  def module(module, opts) do
    funcDb =
      case :proplists.get_value(:no_module_opt, opts, false) do
        false ->
          build_func_db(module)

        true ->
          %{}
      end

    stMap0 = build_st_map(module)
    order = get_call_order_po(stMap0, funcDb)

    phases = [
      {:once, order, prologue_passes(opts)},
      {:module, module_passes(opts)},
      {:fixpoint, order, repeated_passes(opts)},
      {:once, order, epilogue_passes(opts)}
    ]

    stMap = run_phases(phases, stMap0, funcDb)
    {:ok, finish(module, stMap)}
  end

  defp run_phases([{:module, passes} | phases], stMap0, funcDb0) do
    {stMap, funcDb} =
      :compile.run_sub_passes(
        passes,
        {stMap0, funcDb0}
      )

    run_phases(phases, stMap, funcDb)
  end

  defp run_phases([{:once, funcIds0, passes} | phases], stMap0, funcDb0) do
    funcIds = skip_removed(funcIds0, stMap0)
    {stMap, funcDb} = phase(funcIds, passes, stMap0, funcDb0)
    run_phases(phases, stMap, funcDb)
  end

  defp run_phases([{:fixpoint, funcIds0, passes} | phases], stMap0, funcDb0) do
    funcIds = skip_removed(funcIds0, stMap0)
    revFuncIds = reverse(funcIds)
    order = {funcIds, revFuncIds}
    {stMap, funcDb} = fixpoint(revFuncIds, order, passes, stMap0, funcDb0, 16)
    run_phases(phases, stMap, funcDb)
  end

  defp run_phases([], stMap, _FuncDb) do
    stMap
  end

  defp skip_removed(funcIds, stMap) do
    for f <- funcIds, :erlang.is_map_key(f, stMap) do
      f
    end
  end

  defp fixpoint(_FuncIds, _Order, _Passes, stMap, funcDb, 0) do
    {stMap, funcDb}
  end

  defp fixpoint(funcIds0, order0, passes, stMap0, funcDb0, n) do
    {stMap, funcDb} = phase(funcIds0, passes, stMap0, funcDb0)
    repeat = changed(funcIds0, funcDb0, funcDb, stMap0, stMap)

    case :cerl_sets.size(repeat) do
      0 ->
        {stMap, funcDb}

      _ ->
        {orderA, orderB} = order0
        order = {orderB, orderA}

        funcIds =
          for id <- orderA,
              :cerl_sets.is_element(id, repeat) do
            id
          end

        fixpoint(funcIds, order, passes, stMap, funcDb, n - 1)
    end
  end

  defp phase([funcId | ids], ps, stMap, funcDb0) do
    try do
      :compile.run_sub_passes(
        ps,
        {:erlang.map_get(funcId, stMap), funcDb0}
      )
    catch
      class, error ->
        r_b_local(name: r_b_literal(val: name), arity: arity) = funcId
        :io.fwrite('Function: ~w/~w\n', [name, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    else
      {st, funcDb} ->
        phase(ids, ps, %{stMap | funcId => st}, funcDb)
    end
  end

  defp phase([], _Ps, stMap, funcDb) do
    {stMap, funcDb}
  end

  defp changed(prevIds, funcDb0, funcDb, stMap0, stMap) do
    f = fn id, a ->
      case :cerl_sets.is_element(id, a) do
        true ->
          a

        false ->
          {r_func_info(arg_types: aTs0, succ_types: sT0),
           r_func_info(
             arg_types: aTs1,
             succ_types: sT1
           )} = {:erlang.map_get(id, funcDb0), :erlang.map_get(id, funcDb)}

          opts =
            case aTs0 === aTs1 do
              true ->
                []

              false ->
                [:called]
            end ++
              case sT0 === sT1 do
                true ->
                  []

                false ->
                  [:callers]
              end

          case opts do
            [] ->
              a

            [_ | _] ->
              add_changed([id], opts, funcDb, a)
          end
      end
    end

    ids = foldl(f, :cerl_sets.new(), :maps.keys(funcDb))

    foldl(
      fn id, a ->
        case :cerl_sets.is_element(id, a) do
          true ->
            a

          false ->
            case {:erlang.map_get(id, stMap0), :erlang.map_get(id, stMap)} do
              {same, same} ->
                a

              {_, _} ->
                :cerl_sets.add_element(id, a)
            end
        end
      end,
      ids,
      prevIds
    )
  end

  defp add_changed([id | ids], opts, funcDb, s0)
       when :erlang.is_map_key(id, funcDb) do
    case :cerl_sets.is_element(id, s0) do
      true ->
        add_changed(ids, opts, funcDb, s0)

      false ->
        s1 = :cerl_sets.add_element(id, s0)
        r_func_info(in: in__, out: out) = :erlang.map_get(id, funcDb)

        s2 =
          case member(:callers, opts) do
            true ->
              add_changed(in__, opts, funcDb, s1)

            false ->
              s1
          end

        s =
          case member(:called, opts) do
            true ->
              add_changed(out, opts, funcDb, s2)

            false ->
              s2
          end

        add_changed(ids, opts, funcDb, s)
    end
  end

  defp add_changed([_ | ids], opts, funcDb, s) do
    add_changed(ids, opts, funcDb, s)
  end

  defp add_changed([], _, _, s) do
    s
  end

  defp get_func_id(f) do
    {_Mod, name, arity} = :beam_ssa.get_anno(:func_info, f)
    r_b_local(name: r_b_literal(val: name), arity: arity)
  end

  defp build_st_map(r_b_module(body: fs)) do
    build_st_map_1(fs, %{})
  end

  defp build_st_map_1([f | fs], map) do
    r_b_function(anno: anno, args: args, cnt: counter, bs: bs) = f
    st = r_opt_st(anno: anno, args: args, cnt: counter, ssa: bs)
    build_st_map_1(fs, %{map | get_func_id(f) => st})
  end

  defp build_st_map_1([], map) do
    map
  end

  defp finish(r_b_module(body: fs0) = module, stMap) do
    r_b_module(module, body: finish_1(fs0, stMap))
  end

  defp finish_1([f0 | fs], stMap) do
    funcId = get_func_id(f0)

    case stMap do
      %{^funcId => r_opt_st(anno: anno, cnt: counter, ssa: blocks)} ->
        f = r_b_function(f0, anno: anno, bs: blocks, cnt: counter)
        [f | finish_1(fs, stMap)]

      %{} ->
        finish_1(fs, stMap)
    end
  end

  defp finish_1([], _StMap) do
    []
  end

  defp prologue_passes(opts) do
    ps = [
      {:ssa_opt_split_blocks, &ssa_opt_split_blocks/1},
      {:ssa_opt_coalesce_phis, &ssa_opt_coalesce_phis/1},
      {:ssa_opt_tail_phis, &ssa_opt_tail_phis/1},
      {:ssa_opt_element, &ssa_opt_element/1},
      {:ssa_opt_linearize, &ssa_opt_linearize/1},
      {:ssa_opt_tuple_size, &ssa_opt_tuple_size/1},
      {:ssa_opt_record, &ssa_opt_record/1},
      {:ssa_opt_cse, &ssa_opt_cse/1},
      {:ssa_opt_live, &ssa_opt_live/1},
      {:ssa_opt_receive_after, &ssa_opt_receive_after/1}
    ]

    passes_1(ps, opts)
  end

  defp module_passes(opts) do
    ps0 = [
      {:ssa_opt_type_start,
       fn {stMap, funcDb} ->
         :beam_ssa_type.opt_start(stMap, funcDb)
       end}
    ]

    passes_1(ps0, opts)
  end

  defp repeated_passes(opts) do
    ps = [
      {:ssa_opt_live, &ssa_opt_live/1},
      {:ssa_opt_ne, &ssa_opt_ne/1},
      {:ssa_opt_bs_puts, &ssa_opt_bs_puts/1},
      {:ssa_opt_dead, &ssa_opt_dead/1},
      {:ssa_opt_cse, &ssa_opt_cse/1},
      {:ssa_opt_tail_phis, &ssa_opt_tail_phis/1},
      {:ssa_opt_sink, &ssa_opt_sink/1},
      {:ssa_opt_tuple_size, &ssa_opt_tuple_size/1},
      {:ssa_opt_record, &ssa_opt_record/1},
      {:ssa_opt_type_continue, &ssa_opt_type_continue/1}
    ]

    passes_1(ps, opts)
  end

  defp epilogue_passes(opts) do
    ps = [
      {:ssa_opt_type_finish, &ssa_opt_type_finish/1},
      {:ssa_opt_float, &ssa_opt_float/1},
      {:ssa_opt_sw, &ssa_opt_sw/1},
      {:ssa_opt_try, &ssa_opt_try/1},
      {:ssa_opt_live, &ssa_opt_live/1},
      {:ssa_opt_bsm, &ssa_opt_bsm/1},
      {:ssa_opt_bsm_shortcut, &ssa_opt_bsm_shortcut/1},
      {:ssa_opt_sink, &ssa_opt_sink/1},
      {:ssa_opt_blockify, &ssa_opt_blockify/1},
      {:ssa_opt_merge_blocks, &ssa_opt_merge_blocks/1},
      {:ssa_opt_get_tuple_element, &ssa_opt_get_tuple_element/1},
      {:ssa_opt_tail_calls, &ssa_opt_tail_calls/1},
      {:ssa_opt_trim_unreachable, &ssa_opt_trim_unreachable/1},
      {:ssa_opt_unfold_literals, &ssa_opt_unfold_literals/1}
    ]

    passes_1(ps, opts)
  end

  defp passes_1(ps, opts0) do
    negations =
      for {n, _} <- ps do
        {:erlang.list_to_atom('no_' ++ :erlang.atom_to_list(n)), n}
      end

    opts = :proplists.substitute_negations(negations, opts0)

    for {name, _} = p <- ps do
      case :proplists.get_value(name, opts, true) do
        true ->
          p

        false ->
          {noName, ^name} = keyfind(name, 2, negations)

          {noName,
           fn s ->
             s
           end}
      end
    end
  end

  defp build_func_db(r_b_module(body: fs, attributes: attr, exports: exports0)) do
    exports = fdb_exports(attr, exports0)

    try do
      fdb_fs(fs, exports, %{})
    catch
      :load_nif ->
        %{}
    end
  end

  defp fdb_exports([{:on_load, l} | attrs], exports) do
    fdb_exports(attrs, flatten(l) ++ exports)
  end

  defp fdb_exports([_Attr | attrs], exports) do
    fdb_exports(attrs, exports)
  end

  defp fdb_exports([], exports) do
    :gb_sets.from_list(exports)
  end

  defp fdb_fs([r_b_function(args: args, bs: bs) = f | fs], exports, funcDb0) do
    id = get_func_id(f)
    r_b_local(name: r_b_literal(val: name), arity: arity) = id
    exported = :gb_sets.is_element({name, arity}, exports)
    argTypes = duplicate(length(args), %{})

    funcDb1 =
      case funcDb0 do
        %{^id => info} ->
          %{funcDb0 | id => r_func_info(info, exported: exported, arg_types: argTypes)}

        %{} ->
          %{funcDb0 | id => r_func_info(exported: exported, arg_types: argTypes)}
      end

    funcDb =
      :beam_ssa.fold_rpo(
        fn _L, r_b_blk(is: is), funcDb ->
          fdb_is(is, id, funcDb)
        end,
        funcDb1,
        bs
      )

    fdb_fs(fs, exports, funcDb)
  end

  defp fdb_fs([], _Exports, funcDb) do
    funcDb
  end

  defp fdb_is([r_b_set(op: :call, args: [r_b_local() = callee | _]) | is], caller, funcDb) do
    fdb_is(is, caller, fdb_update(caller, callee, funcDb))
  end

  defp fdb_is(
         [
           r_b_set(
             op: :call,
             args: [
               r_b_remote(mod: r_b_literal(val: :erlang), name: r_b_literal(val: :load_nif)),
               _Path,
               _LoadInfo
             ]
           )
           | _Is
         ],
         _Caller,
         _FuncDb
       ) do
    throw(:load_nif)
  end

  defp fdb_is([r_b_set(op: makeFun, args: [r_b_local() = callee | _]) | is], caller, funcDb)
       when makeFun === :make_fun or
              makeFun === :old_make_fun do
    fdb_is(is, caller, fdb_update(caller, callee, funcDb))
  end

  defp fdb_is([_ | is], caller, funcDb) do
    fdb_is(is, caller, funcDb)
  end

  defp fdb_is([], _Caller, funcDb) do
    funcDb
  end

  defp fdb_update(caller, callee, funcDb) do
    callerVertex = :maps.get(caller, funcDb, r_func_info())
    calleeVertex = :maps.get(callee, funcDb, r_func_info())

    calls =
      :ordsets.add_element(
        callee,
        r_func_info(callerVertex, :out)
      )

    calledBy =
      :ordsets.add_element(
        caller,
        r_func_info(calleeVertex, :in)
      )

    %{
      funcDb
      | caller => r_func_info(callerVertex, out: calls),
        callee => r_func_info(calleeVertex, in: calledBy)
    }
  end

  defp get_call_order_po(stMap, funcDb) do
    order = gco_po(funcDb)

    order ++
      :maps.fold(
        fn k, _V, acc ->
          case :erlang.is_map_key(k, funcDb) do
            false ->
              [k | acc]

            true ->
              acc
          end
        end,
        [],
        stMap
      )
  end

  defp gco_po(funcDb) do
    all = sort(:maps.keys(funcDb))
    {rPO, _} = gco_rpo(all, funcDb, :cerl_sets.new(), [])
    reverse(rPO)
  end

  defp gco_rpo([id | ids], funcDb, seen0, acc0) do
    case :cerl_sets.is_element(id, seen0) do
      true ->
        gco_rpo(ids, funcDb, seen0, acc0)

      false ->
        r_func_info(out: successors) = :erlang.map_get(id, funcDb)
        seen1 = :cerl_sets.add_element(id, seen0)
        {acc, seen} = gco_rpo(successors, funcDb, seen1, acc0)
        gco_rpo(ids, funcDb, seen, [id | acc])
    end
  end

  defp gco_rpo([], _, seen, acc) do
    {acc, seen}
  end

  defp ssa_opt_dead({r_opt_st(ssa: linear) = st, funcDb}) do
    {r_opt_st(st, ssa: :beam_ssa_dead.opt(linear)), funcDb}
  end

  defp ssa_opt_linearize({r_opt_st(ssa: blocks) = st, funcDb}) do
    {r_opt_st(st, ssa: :beam_ssa.linearize(blocks)), funcDb}
  end

  defp ssa_opt_type_continue({r_opt_st(ssa: linear0, args: args, anno: anno) = st0, funcDb0}) do
    {linear, funcDb} = :beam_ssa_type.opt_continue(linear0, args, anno, funcDb0)
    {r_opt_st(st0, ssa: linear), funcDb}
  end

  defp ssa_opt_type_finish({r_opt_st(args: args, anno: anno0) = st0, funcDb0}) do
    {anno, funcDb} = :beam_ssa_type.opt_finish(args, anno0, funcDb0)
    {r_opt_st(st0, anno: anno), funcDb}
  end

  defp ssa_opt_blockify({r_opt_st(ssa: linear) = st, funcDb}) do
    {r_opt_st(st, ssa: :maps.from_list(linear)), funcDb}
  end

  defp ssa_opt_trim_unreachable({r_opt_st(ssa: blocks) = st, funcDb}) do
    {r_opt_st(st, ssa: :beam_ssa.trim_unreachable(blocks)), funcDb}
  end

  defp ssa_opt_merge_blocks({r_opt_st(ssa: blocks0) = st, funcDb}) do
    blocks = :beam_ssa.merge_blocks(blocks0)
    {r_opt_st(st, ssa: blocks), funcDb}
  end

  defp ssa_opt_split_blocks({r_opt_st(ssa: blocks0, cnt: count0) = st, funcDb}) do
    p = fn
      r_b_set(op: {:bif, :element}) ->
        true

      r_b_set(op: :call) ->
        true

      r_b_set(op: :make_fun) ->
        true

      r_b_set(op: :old_make_fun) ->
        true

      _ ->
        false
    end

    {blocks, count} = :beam_ssa.split_blocks(p, blocks0, count0)
    {r_opt_st(st, ssa: blocks, cnt: count), funcDb}
  end

  defp ssa_opt_coalesce_phis({r_opt_st(ssa: blocks0) = st, funcDb}) do
    ls = :beam_ssa.rpo(blocks0)
    blocks = c_phis_1(ls, blocks0)
    {r_opt_st(st, ssa: blocks), funcDb}
  end

  defp c_phis_1([l | ls], blocks0) do
    case :erlang.map_get(l, blocks0) do
      r_b_blk(is: [r_b_set(op: :phi) | _]) = blk ->
        blocks = c_phis_2(l, blk, blocks0)
        c_phis_1(ls, blocks)

      r_b_blk() ->
        c_phis_1(ls, blocks0)
    end
  end

  defp c_phis_1([], blocks) do
    blocks
  end

  defp c_phis_2(l, r_b_blk(is: is0) = blk0, blocks0) do
    case c_phis_args(is0, blocks0) do
      :none ->
        blocks0

      {_, _, preds} = info ->
        is = c_rewrite_phis(is0, info)
        blk = r_b_blk(blk0, is: is)
        blocks = %{blocks0 | l => blk}
        c_fix_branches(preds, l, blocks)
    end
  end

  defp c_phis_args([r_b_set(op: :phi, args: args0) | is], blocks) do
    case c_phis_args_1(args0, blocks) do
      :none ->
        c_phis_args(is, blocks)

      res ->
        res
    end
  end

  defp c_phis_args(_, _Blocks) do
    :none
  end

  defp c_phis_args_1([{var, pred} | as], blocks) do
    case c_get_pred_vars(var, pred, blocks) do
      :none ->
        c_phis_args_1(as, blocks)

      result ->
        result
    end
  end

  defp c_phis_args_1([], _Blocks) do
    :none
  end

  defp c_get_pred_vars(var, pred, blocks) do
    case :erlang.map_get(pred, blocks) do
      r_b_blk(is: [r_b_set(op: :phi, dst: ^var, args: args)]) ->
        {var, pred, args}

      r_b_blk() ->
        :none
    end
  end

  defp c_rewrite_phis([r_b_set(op: :phi, args: args0) = i | is], info) do
    args = c_rewrite_phi(args0, info)
    [r_b_set(i, args: args) | c_rewrite_phis(is, info)]
  end

  defp c_rewrite_phis(is, _Info) do
    is
  end

  defp c_rewrite_phi([{var, pred} | as], {var, pred, values}) do
    values ++ as
  end

  defp c_rewrite_phi([{value, pred} | as], {_, pred, values}) do
    for {_, p} <- values do
      {value, p}
    end ++ as
  end

  defp c_rewrite_phi([a | as], info) do
    [a | c_rewrite_phi(as, info)]
  end

  defp c_rewrite_phi([], _Info) do
    []
  end

  defp c_fix_branches([{_, pred} | as], l, blocks0) do
    r_b_blk(last: last0) = blk0 = :erlang.map_get(pred, blocks0)
    r_b_br(bool: r_b_literal(val: true)) = last0
    last = r_b_br(last0, bool: r_b_literal(val: true), succ: l, fail: l)
    blk = r_b_blk(blk0, last: last)
    blocks = %{blocks0 | pred => blk}
    c_fix_branches(as, l, blocks)
  end

  defp c_fix_branches([], _, blocks) do
    blocks
  end

  defp ssa_opt_tail_phis({r_opt_st(ssa: sSA0, cnt: count0) = st, funcDb}) do
    {sSA, count} = opt_tail_phis(sSA0, count0)
    {r_opt_st(st, ssa: sSA, cnt: count), funcDb}
  end

  defp opt_tail_phis(blocks, count) when is_map(blocks) do
    opt_tail_phis(:maps.values(blocks), blocks, count)
  end

  defp opt_tail_phis(linear0, count0) when is_list(linear0) do
    blocks0 = :maps.from_list(linear0)
    {blocks, count} = opt_tail_phis(blocks0, count0)
    {:beam_ssa.linearize(blocks), count}
  end

  defp opt_tail_phis([r_b_blk(is: is0, last: last) | bs], blocks0, count0) do
    case {is0, last} do
      {[r_b_set(op: :phi, args: [[_, _] | _]) | _], r_b_ret(arg: r_b_var()) = ret} ->
        {phis, is} =
          splitwith(
            fn r_b_set(op: op) ->
              op === :phi
            end,
            is0
          )

        case suitable_tail_ops(is) do
          true ->
            {blocks, count} = opt_tail_phi(phis, is, ret, blocks0, count0)
            opt_tail_phis(bs, blocks, count)

          false ->
            opt_tail_phis(bs, blocks0, count0)
        end

      {_, _} ->
        opt_tail_phis(bs, blocks0, count0)
    end
  end

  defp opt_tail_phis([], blocks, count) do
    {blocks, count}
  end

  defp opt_tail_phi(phis0, is, ret, blocks0, count0) do
    phis = rel2fam(reduce_phis(phis0))

    {blocks, count, cost} =
      foldl(
        fn phiArg, acc ->
          opt_tail_phi_arg(phiArg, is, ret, acc)
        end,
        {blocks0, count0, 0},
        phis
      )

    maxCost = length(phis) * 3 + 2

    cond do
      cost <= maxCost ->
        {blocks, count}

      true ->
        {blocks0, count0}
    end
  end

  defp reduce_phis([r_b_set(dst: phiDst, args: phiArgs) | is]) do
    for {val, l} <- phiArgs do
      {l, {phiDst, val}}
    end ++ reduce_phis(is)
  end

  defp reduce_phis([]) do
    []
  end

  defp opt_tail_phi_arg({predL, sub0}, is0, ret0, {blocks0, count0, cost0}) do
    blk0 = :erlang.map_get(predL, blocks0)
    r_b_blk(is: isPrefix, last: r_b_br(succ: next, fail: next)) = blk0
    sub1 = :maps.from_list(sub0)
    {is1, count, sub} = new_names(is0, sub1, count0, [])

    is2 =
      for i <- is1 do
        sub(i, sub)
      end

    cost = build_cost(is2, cost0)
    is = isPrefix ++ is2
    ret = sub(ret0, sub)
    blk = r_b_blk(blk0, is: is, last: ret)
    blocks = %{blocks0 | predL => blk}
    {blocks, count, cost}
  end

  defp new_names([r_b_set(dst: dst) = i | is], sub0, count0, acc) do
    {newDst, count} = new_var(dst, count0)
    sub = %{sub0 | dst => newDst}
    new_names(is, sub, count, [r_b_set(i, dst: newDst) | acc])
  end

  defp new_names([], sub, count, acc) do
    {reverse(acc), count, sub}
  end

  defp suitable_tail_ops(is) do
    all(
      fn r_b_set(op: op) ->
        is_suitable_tail_op(op)
      end,
      is
    )
  end

  defp is_suitable_tail_op({:bif, _}) do
    true
  end

  defp is_suitable_tail_op(:put_list) do
    true
  end

  defp is_suitable_tail_op(:put_tuple) do
    true
  end

  defp is_suitable_tail_op(_) do
    false
  end

  defp build_cost([r_b_set(op: :put_list, args: args) | is], cost) do
    case are_all_literals(args) do
      true ->
        build_cost(is, cost)

      false ->
        build_cost(is, cost + 1)
    end
  end

  defp build_cost([r_b_set(op: :put_tuple, args: args) | is], cost) do
    case are_all_literals(args) do
      true ->
        build_cost(is, cost)

      false ->
        build_cost(is, cost + length(args) + 1)
    end
  end

  defp build_cost([r_b_set(op: {:bif, _}, args: args) | is], cost) do
    case are_all_literals(args) do
      true ->
        build_cost(is, cost)

      false ->
        build_cost(is, cost + 1)
    end
  end

  defp build_cost([], cost) do
    cost
  end

  defp are_all_literals(args) do
    all(
      fn
        r_b_literal() ->
          true

        _ ->
          false
      end,
      args
    )
  end

  defp ssa_opt_element({r_opt_st(ssa: blocks) = st, funcDb}) do
    getEls = collect_element_calls(:beam_ssa.linearize(blocks))
    chains = collect_chains(getEls, [])
    {r_opt_st(st, ssa: swap_element_calls(chains, blocks)), funcDb}
  end

  defp collect_element_calls([{l, r_b_blk(is: is0, last: last)} | bs]) do
    case {is0, last} do
      {[
         r_b_set(
           op: {:bif, :element},
           dst: element,
           args: [r_b_literal(val: n), r_b_var() = tuple]
         ),
         r_b_set(op: {:succeeded, :guard}, dst: bool, args: [element])
       ], r_b_br(bool: bool, succ: succ, fail: fail)} ->
        info = {l, succ, {tuple, fail}, n}
        [info | collect_element_calls(bs)]

      {_, _} ->
        collect_element_calls(bs)
    end
  end

  defp collect_element_calls([]) do
    []
  end

  defp collect_chains(
         [{this, _, v, _} = el | els],
         [{_, this, v, _} | _] = chain
       ) do
    collect_chains(els, [el | chain])
  end

  defp collect_chains([el | els], [[_, _] | _] = chain) do
    [chain | collect_chains(els, [el])]
  end

  defp collect_chains([el | els], _Chain) do
    collect_chains(els, [el])
  end

  defp collect_chains([], [[_, _] | _] = chain) do
    [chain]
  end

  defp collect_chains([], _) do
    []
  end

  defp swap_element_calls(
         [[{l, _, _, n} | _] = chain | chains],
         blocks0
       ) do
    blocks = swap_element_calls_1(chain, {n, l}, blocks0)
    swap_element_calls(chains, blocks)
  end

  defp swap_element_calls([], blocks) do
    blocks
  end

  defp swap_element_calls_1([{l1, _, _, n1}], {n2, l2}, blocks)
       when n2 > n1 do
    %{^l1 => blk1, ^l2 => blk2} = blocks
    [r_b_set(dst: dst1) = getEl1, succ1] = r_b_blk(blk1, :is)
    [r_b_set(dst: dst2) = getEl2, succ2] = r_b_blk(blk2, :is)
    is1 = [getEl2, r_b_set(succ1, args: [dst2])]
    is2 = [getEl1, r_b_set(succ2, args: [dst1])]
    %{blocks | l1 => r_b_blk(blk1, is: is1), l2 => r_b_blk(blk2, is: is2)}
  end

  defp swap_element_calls_1([{l, _, _, n1} | els], {n2, _}, blocks)
       when n1 > n2 do
    swap_element_calls_1(els, {n2, l}, blocks)
  end

  defp swap_element_calls_1([_ | els], highest, blocks) do
    swap_element_calls_1(els, highest, blocks)
  end

  defp swap_element_calls_1([], _, blocks) do
    blocks
  end

  defp ssa_opt_record({r_opt_st(ssa: linear) = st, funcDb}) do
    blocks = :maps.from_list(linear)
    {r_opt_st(st, ssa: record_opt(linear, blocks)), funcDb}
  end

  defp record_opt(
         [{l, r_b_blk(is: is0, last: last) = blk0} | bs],
         blocks
       ) do
    is = record_opt_is(is0, last, blocks)
    blk = r_b_blk(blk0, is: is)
    [{l, blk} | record_opt(bs, blocks)]
  end

  defp record_opt([], _Blocks) do
    []
  end

  defp record_opt_is(
         [r_b_set(op: {:bif, :is_tuple}, dst: bool, args: [tuple]) = set],
         last,
         blocks
       ) do
    case is_tagged_tuple(tuple, bool, last, blocks) do
      {:yes, size, tag} ->
        args = [tuple, size, tag]
        [r_b_set(set, op: :is_tagged_tuple, args: args)]

      :no ->
        [set]
    end
  end

  defp record_opt_is([i | is] = is0, r_b_br(bool: bool) = last, blocks) do
    case is_tagged_tuple_1(is0, last, blocks) do
      {:yes, _Fail, tuple, arity, tag} ->
        args = [tuple, arity, tag]
        [r_b_set(i, op: :is_tagged_tuple, dst: bool, args: args)]

      :no ->
        [i | record_opt_is(is, last, blocks)]
    end
  end

  defp record_opt_is([i | is], last, blocks) do
    [i | record_opt_is(is, last, blocks)]
  end

  defp record_opt_is([], _Last, _Blocks) do
    []
  end

  defp is_tagged_tuple(
         r_b_var() = tuple,
         bool,
         r_b_br(bool: bool, succ: succ, fail: fail),
         blocks
       ) do
    r_b_blk(is: is, last: last) = :erlang.map_get(succ, blocks)

    case is_tagged_tuple_1(is, last, blocks) do
      {:yes, ^fail, ^tuple, arity, tag} ->
        {:yes, arity, tag}

      _ ->
        :no
    end
  end

  defp is_tagged_tuple(_, _, _, _) do
    :no
  end

  defp is_tagged_tuple_1(is, last, blocks) do
    case {is, last} do
      {[
         r_b_set(op: {:bif, :tuple_size}, dst: arityVar, args: [r_b_var() = tuple]),
         r_b_set(
           op: {:bif, :"=:="},
           dst: bool,
           args: [arityVar, r_b_literal(val: arityVal) = arity]
         )
       ], r_b_br(bool: bool, succ: succ, fail: fail)}
      when is_integer(arityVal) ->
        succBlk = :erlang.map_get(succ, blocks)

        case is_tagged_tuple_2(succBlk, tuple, fail) do
          :no ->
            :no

          {:yes, tag} ->
            {:yes, fail, tuple, arity, tag}
        end

      _ ->
        :no
    end
  end

  defp is_tagged_tuple_2(
         r_b_blk(
           is: is,
           last: r_b_br(bool: r_b_var() = bool, fail: fail)
         ),
         tuple,
         fail
       ) do
    is_tagged_tuple_3(is, bool, tuple)
  end

  defp is_tagged_tuple_2(r_b_blk(), _, _) do
    :no
  end

  defp is_tagged_tuple_3(
         [
           r_b_set(
             op: :get_tuple_element,
             dst: tagVar,
             args: [r_b_var() = tuple, r_b_literal(val: 0)]
           )
           | is
         ],
         bool,
         tuple
       ) do
    is_tagged_tuple_4(is, bool, tagVar)
  end

  defp is_tagged_tuple_3([_ | is], bool, tuple) do
    is_tagged_tuple_3(is, bool, tuple)
  end

  defp is_tagged_tuple_3([], _, _) do
    :no
  end

  defp is_tagged_tuple_4(
         [
           r_b_set(
             op: {:bif, :"=:="},
             dst: bool,
             args: [r_b_var() = tagVar, r_b_literal(val: tagVal) = tag]
           )
         ],
         bool,
         tagVar
       )
       when is_atom(tagVal) do
    {:yes, tag}
  end

  defp is_tagged_tuple_4([_ | is], bool, tagVar) do
    is_tagged_tuple_4(is, bool, tagVar)
  end

  defp is_tagged_tuple_4([], _, _) do
    :no
  end

  defp ssa_opt_cse({r_opt_st(ssa: linear) = st, funcDb}) do
    m = %{0 => %{}}
    {r_opt_st(st, ssa: cse(linear, %{}, m)), funcDb}
  end

  defp cse([{l, r_b_blk(is: is0, last: last0) = blk} | bs], sub0, m0) do
    es0 = :erlang.map_get(l, m0)
    {is1, es, sub} = cse_is(is0, es0, sub0, [])
    last = sub(last0, sub)
    m = cse_successors(is1, blk, es, m0)
    is = reverse(is1)
    [{l, r_b_blk(blk, is: is, last: last)} | cse(bs, sub, m)]
  end

  defp cse([], _, _) do
    []
  end

  defp cse_successors(
         [
           [r_b_set(op: {:succeeded, _}, args: [src]), bif]
           | _
         ],
         blk,
         esSucc,
         m0
       ) do
    case cse_suitable(bif) do
      true ->
        r_b_blk(last: r_b_br(succ: succ, fail: fail)) = blk
        m = cse_successors_1([succ], esSucc, m0)

        esFail =
          :maps.filter(
            fn _, val ->
              val !== src
            end,
            esSucc
          )

        cse_successors_1([fail], esFail, m)

      false ->
        cse_successors_1(:beam_ssa.successors(blk), esSucc, m0)
    end
  end

  defp cse_successors(_Is, blk, es, m) do
    cse_successors_1(:beam_ssa.successors(blk), es, m)
  end

  defp cse_successors_1([l | ls], es0, m) do
    case m do
      %{^l => es1} when map_size(es1) === 0 ->
        cse_successors_1(ls, es0, m)

      %{^l => es1} ->
        es =
          :maps.filter(
            fn key, value ->
              case es1 do
                %{^key => ^value} ->
                  true

                %{} ->
                  false
              end
            end,
            es0
          )

        cse_successors_1(ls, es0, %{m | l => es})

      %{} ->
        cse_successors_1(ls, es0, %{m | l => es0})
    end
  end

  defp cse_successors_1([], _, m) do
    m
  end

  defp cse_is(
         [
           r_b_set(op: {:succeeded, _}, dst: bool, args: [src]) = i0
           | is
         ],
         es,
         sub0,
         acc
       ) do
    i = sub(i0, sub0)

    case i do
      r_b_set(args: [^src]) ->
        cse_is(is, es, sub0, [i | acc])

      r_b_set() ->
        sub = %{sub0 | bool => r_b_literal(val: true)}
        cse_is(is, es, sub, acc)
    end
  end

  defp cse_is([r_b_set(dst: dst) = i0 | is], es0, sub0, acc) do
    i = sub(i0, sub0)

    case :beam_ssa.clobbers_xregs(i) do
      true ->
        cse_is(is, %{}, sub0, [i | acc])

      false ->
        case cse_expr(i) do
          :none ->
            cse_is(is, es0, sub0, [i | acc])

          {:ok, exprKey} ->
            case es0 do
              %{^exprKey => src} ->
                sub = %{sub0 | dst => src}
                cse_is(is, es0, sub, acc)

              %{} ->
                es = %{es0 | exprKey => dst}
                cse_is(is, es, sub0, [i | acc])
            end
        end
    end
  end

  defp cse_is([], es, sub, acc) do
    {acc, es, sub}
  end

  defp cse_expr(r_b_set(op: op, args: args) = i) do
    case cse_suitable(i) do
      true ->
        {:ok, {op, args}}

      false ->
        :none
    end
  end

  defp cse_suitable(r_b_set(op: :get_hd)) do
    true
  end

  defp cse_suitable(r_b_set(op: :get_tl)) do
    true
  end

  defp cse_suitable(r_b_set(op: :put_list)) do
    true
  end

  defp cse_suitable(r_b_set(op: :get_tuple_element)) do
    true
  end

  defp cse_suitable(r_b_set(op: :put_tuple)) do
    true
  end

  defp cse_suitable(r_b_set(op: {:bif, :tuple_size})) do
    false
  end

  defp cse_suitable(r_b_set(anno: anno, op: {:bif, name}, args: args)) do
    arity = length(args)

    not (:erlang.is_map_key(
           :float_op,
           anno
         ) or
           :erl_internal.new_type_test(
             name,
             arity
           ) or
           :erl_internal.comp_op(
             name,
             arity
           ) or
           :erl_internal.bool_op(
             name,
             arity
           ))
  end

  defp cse_suitable(r_b_set()) do
    false
  end

  Record.defrecord(:r_fs, :fs,
    s: :undefined,
    regs: %{},
    vars: :cerl_sets.new(),
    fail: :none,
    non_guards: :undefined,
    bs: :undefined
  )

  defp ssa_opt_float({r_opt_st(ssa: linear0, cnt: count0) = st, funcDb}) do
    nonGuards = non_guards(linear0)
    blocks = :maps.from_list(linear0)
    fs = r_fs(non_guards: nonGuards, bs: blocks)
    {linear, count} = float_opt(linear0, count0, fs)
    {r_opt_st(st, ssa: linear, cnt: count), funcDb}
  end

  defp float_can_optimize_blk(
         r_b_blk(last: r_b_br(bool: r_b_var(), fail: f)),
         r_fs(non_guards: nonGuards)
       ) do
    :gb_sets.is_member(f, nonGuards)
  end

  defp float_can_optimize_blk(r_b_blk(), r_fs()) do
    false
  end

  defp float_opt([{l, blk} | bs0], count0, fs) do
    case float_can_optimize_blk(blk, fs) do
      true ->
        float_opt_1(l, blk, bs0, count0, fs)

      false ->
        {bs, count} = float_opt(bs0, count0, fs)
        {[{l, blk} | bs], count}
    end
  end

  defp float_opt([], count, _Fs) do
    {[], count}
  end

  defp float_opt_1(l, r_b_blk(is: is0) = blk0, bs0, count0, fs0) do
    case float_opt_is(is0, fs0, count0, []) do
      {is1, fs1, count1} ->
        fs2 = float_fail_label(blk0, fs1)
        fail = r_fs(fs2, :fail)
        {flush, blk, fs, count2} = float_maybe_flush(blk0, fs2, count1)
        split = float_split_conv(is1, blk)
        {blks0, count3} = float_number(split, l, count2)
        {blks, count4} = float_conv(blks0, fail, count3)
        {bs, count} = float_opt(bs0, count4, fs)
        {blks ++ flush ++ bs, count}

      :none ->
        {bs, count} = float_opt(bs0, count0, fs0)
        {[{l, blk0} | bs], count}
    end
  end

  defp float_split_conv(is0, blk) do
    br = r_b_br(bool: r_b_literal(val: true), succ: 0, fail: 0)

    case splitwith(
           fn r_b_set(op: op) ->
             op !== {:float, :convert}
           end,
           is0
         ) do
      {is, []} ->
        [r_b_blk(blk, is: is)]

      {[_ | _] = is1, [r_b_set(op: {:float, :convert}) = conv | is2]} ->
        [
          [r_b_blk(is: is1, last: br), r_b_blk(is: [conv], last: br)]
          | float_split_conv(is2, blk)
        ]

      {[], [r_b_set(op: {:float, :convert}) = conv | is1]} ->
        [r_b_blk(is: [conv], last: br) | float_split_conv(is1, blk)]
    end
  end

  defp float_number([b | bs0], firstL, count0) do
    {bs, count} = float_number(bs0, count0)
    {[{firstL, b} | bs], count}
  end

  defp float_number([b | bs0], count0) do
    {bs, count} = float_number(bs0, count0 + 1)
    {[{count0, b} | bs], count}
  end

  defp float_number([], count) do
    {[], count}
  end

  defp float_conv([{l, r_b_blk(is: is0) = blk0} | bs0], fail, count0) do
    case is0 do
      [r_b_set(op: {:float, :convert}) = conv] ->
        {bool0, count1} = new_reg(:"@ssa_bool", count0)
        bool = r_b_var(name: bool0)
        succeeded = r_b_set(op: {:succeeded, :body}, dst: bool, args: [r_b_set(conv, :dst)])
        is = [conv, succeeded]
        [{nextL, _} | _] = bs0
        br = r_b_br(bool: bool, succ: nextL, fail: fail)
        blk = r_b_blk(blk0, is: is, last: br)
        {bs, count} = float_conv(bs0, fail, count1)
        {[{l, blk} | bs], count}

      [_ | _] ->
        case bs0 do
          [{nextL, _} | _] ->
            br = r_b_br(bool: r_b_literal(val: true), succ: nextL, fail: nextL)
            blk = r_b_blk(blk0, last: br)
            {bs, count} = float_conv(bs0, fail, count0)
            {[{l, blk} | bs], count}

          [] ->
            {[{l, blk0}], count0}
        end
    end
  end

  defp float_maybe_flush(blk0, r_fs(s: :cleared, fail: fail, bs: blocks) = fs0, count0) do
    r_b_blk(last: r_b_br(bool: r_b_var(), succ: succ) = br) = blk0
    r_b_blk(is: is) = succBlk = :erlang.map_get(succ, blocks)
    canOptimizeSucc = float_can_optimize_blk(succBlk, fs0)

    case is do
      [r_b_set(anno: %{:float_op => _}) | _] when canOptimizeSucc ->
        {[], blk0, fs0, count0}

      _ ->
        {bool0, count1} = new_reg(:"@ssa_bool", count0)
        bool = r_b_var(name: bool0)
        checkL = count1
        flushL = count1 + 1
        count = count1 + 2
        blk = r_b_blk(blk0, last: r_b_br(br, succ: checkL))
        checkIs = [r_b_set(op: {:float, :checkerror}, dst: bool)]
        checkBr = r_b_br(bool: bool, succ: flushL, fail: fail)
        checkBlk = r_b_blk(is: checkIs, last: checkBr)
        flushIs = float_flush_regs(fs0)
        flushBr = r_b_br(bool: r_b_literal(val: true), succ: succ, fail: succ)
        flushBlk = r_b_blk(is: flushIs, last: flushBr)
        fs = r_fs(fs0, s: :undefined, regs: %{}, fail: :none)
        flushBs = [{checkL, checkBlk}, {flushL, flushBlk}]
        {flushBs, blk, fs, count}
    end
  end

  defp float_maybe_flush(blk, fs, count) do
    {[], blk, fs, count}
  end

  defp float_opt_is(
         [r_b_set(op: {:succeeded, _}, args: [src]) = i0],
         r_fs(regs: rs) = fs,
         count,
         acc
       ) do
    case rs do
      %{^src => fr} ->
        i = r_b_set(i0, args: [fr])
        {reverse(acc, [i]), fs, count}

      %{} ->
        {reverse(acc, [i0]), fs, count}
    end
  end

  defp float_opt_is([r_b_set(anno: anno0) = i0 | is0], fs0, count0, acc) do
    case anno0 do
      %{:float_op => fTypes} ->
        anno = :maps.remove(:float_op, anno0)
        i1 = r_b_set(i0, anno: anno)
        {is, fs, count} = float_make_op(i1, fTypes, fs0, count0)
        float_opt_is(is0, fs, count, reverse(is, acc))

      %{} ->
        float_opt_is(is0, r_fs(fs0, regs: %{}), count0, [i0 | acc])
    end
  end

  defp float_opt_is([], fs, _Count, _Acc) do
    r_fs(s: :undefined) = fs
    :none
  end

  defp float_make_op(
         r_b_set(op: {:bif, op}, dst: dst, args: as0, anno: anno) = i0,
         ts,
         r_fs(s: s, regs: rs0, vars: vs0) = fs,
         count0
       ) do
    {as1, rs1, count1} = float_load(as0, ts, anno, rs0, count0, [])
    {as, is0} = unzip(as1)
    {fr, count2} = new_reg(:"@fr", count1)
    frDst = r_b_var(name: fr)
    i = r_b_set(i0, op: {:float, op}, dst: frDst, args: as)
    vs = :cerl_sets.add_element(dst, vs0)
    rs = %{rs1 | dst => frDst}
    is = append(is0) ++ [i]

    case s do
      :undefined ->
        {ignore, count} = new_reg(:"@ssa_ignore", count2)
        c = r_b_set(op: {:float, :clearerror}, dst: r_b_var(name: ignore))
        {[c | is], r_fs(fs, s: :cleared, regs: rs, vars: vs), count}

      :cleared ->
        {is, r_fs(fs, regs: rs, vars: vs), count2}
    end
  end

  defp float_load([a | as], [t | ts], anno, rs0, count0, acc) do
    {load, rs, count} = float_reg_arg(a, t, anno, rs0, count0)
    float_load(as, ts, anno, rs, count, [load | acc])
  end

  defp float_load([], [], _Anno, rs, count, acc) do
    {reverse(acc), rs, count}
  end

  defp float_reg_arg(a, t, anno, rs, count0) do
    case rs do
      %{^a => fr} ->
        {{fr, []}, rs, count0}

      %{} ->
        {fr, count} = new_float_copy_reg(count0)
        dst = r_b_var(name: fr)
        i0 = float_load_reg(t, a, dst)
        i = r_b_set(i0, anno: anno)
        {{dst, [i]}, %{rs | a => dst}, count}
    end
  end

  defp float_load_reg(:convert, r_b_var() = src, dst) do
    r_b_set(op: {:float, :convert}, dst: dst, args: [src])
  end

  defp float_load_reg(:convert, r_b_literal(val: val) = src, dst) do
    try do
      :erlang.float(val)
    catch
      :error, _ ->
        r_b_set(op: {:float, :convert}, dst: dst, args: [src])
    else
      f ->
        r_b_set(op: {:float, :put}, dst: dst, args: [r_b_literal(val: f)])
    end
  end

  defp float_load_reg(:float, src, dst) do
    r_b_set(op: {:float, :put}, dst: dst, args: [src])
  end

  defp new_float_copy_reg(count) do
    new_reg(:"@fr_copy", count)
  end

  defp new_reg(base, count) do
    fr = {base, count}
    {fr, count + 1}
  end

  defp float_fail_label(r_b_blk(last: last), fs) do
    case last do
      r_b_br(bool: r_b_var(), fail: fail) ->
        r_fs(fs, fail: fail)

      _ ->
        fs
    end
  end

  defp float_flush_regs(r_fs(regs: rs)) do
    :maps.fold(
      fn
        _, r_b_var(name: {:"@fr_copy", _}), acc ->
          acc

        dst, fr, acc ->
          [r_b_set(op: {:float, :get}, dst: dst, args: [fr]) | acc]
      end,
      [],
      rs
    )
  end

  defp ssa_opt_live({r_opt_st(ssa: linear0) = st, funcDb}) do
    revLinear = reverse(linear0)
    blocks0 = :maps.from_list(revLinear)
    blocks = live_opt(revLinear, %{}, blocks0)
    linear = :beam_ssa.linearize(blocks)
    {r_opt_st(st, ssa: linear), funcDb}
  end

  defp live_opt([{l, blk0} | bs], liveMap0, blocks) do
    blk1 = :beam_ssa_share.block(blk0, blocks)
    successors = :beam_ssa.successors(blk1)
    live0 = live_opt_succ(successors, l, liveMap0, :gb_sets.empty())
    {blk, live} = live_opt_blk(blk1, live0)
    liveMap = live_opt_phis(r_b_blk(blk, :is), l, live, liveMap0)
    live_opt(bs, liveMap, %{blocks | l => blk})
  end

  defp live_opt([], _, acc) do
    acc
  end

  defp live_opt_succ([s | ss], l, liveMap, live0) do
    key = {s, l}

    case liveMap do
      %{^key => live} ->
        live_opt_succ(ss, l, liveMap, :gb_sets.union(live, live0))

      %{^s => live} ->
        live_opt_succ(ss, l, liveMap, :gb_sets.union(live, live0))

      %{} ->
        live_opt_succ(ss, l, liveMap, live0)
    end
  end

  defp live_opt_succ([], _, _, acc) do
    acc
  end

  defp live_opt_phis(is, l, live0, liveMap0) do
    liveMap = %{liveMap0 | l => live0}

    phis =
      takewhile(
        fn r_b_set(op: op) ->
          op === :phi
        end,
        is
      )

    case phis do
      [] ->
        liveMap

      [_ | _] ->
        phiArgs =
          append(
            for r_b_set(args: args) <- phis do
              args
            end
          )

        case (for {r_b_var() = v, p} <- phiArgs do
                {p, v}
              end) do
          [_ | _] = phiVars ->
            phiLive0 = rel2fam(phiVars)

            phiLive =
              for {p, vs} <- phiLive0 do
                {{l, p}, :gb_sets.union(:gb_sets.from_list(vs), live0)}
              end

            :maps.merge(liveMap, :maps.from_list(phiLive))

          [] ->
            liveMap
        end
    end
  end

  defp live_opt_blk(r_b_blk(is: is0, last: last) = blk, live0) do
    live1 =
      :gb_sets.union(
        live0,
        :gb_sets.from_ordset(:beam_ssa.used(last))
      )

    {is, live} = live_opt_is(reverse(is0), live1, [])
    {r_b_blk(blk, is: is), live}
  end

  defp live_opt_is([r_b_set(op: :phi, dst: dst) = i | is], live, acc) do
    case :gb_sets.is_member(dst, live) do
      true ->
        live_opt_is(is, live, [i | acc])

      false ->
        live_opt_is(is, live, acc)
    end
  end

  defp live_opt_is(
         [
           [
             r_b_set(op: {:succeeded, :guard}, dst: succDst, args: [dst]) = succI,
             r_b_set(op: op, dst: dst) = i0
           ]
           | is
         ],
         live0,
         acc
       ) do
    case {:gb_sets.is_member(succDst, live0), :gb_sets.is_member(dst, live0)} do
      {true, true} ->
        live = :gb_sets.delete(succDst, live0)
        live_opt_is([i0 | is], live, [succI | acc])

      {true, false} ->
        case op do
          {:bif, :not} ->
            i = r_b_set(i0, op: {:bif, :is_boolean}, dst: succDst)
            live_opt_is([i | is], live0, acc)

          {:bif, :tuple_size} ->
            i = r_b_set(i0, op: {:bif, :is_tuple}, dst: succDst)
            live_opt_is([i | is], live0, acc)

          :bs_start_match ->
            [r_b_literal(val: :new), bin] = r_b_set(i0, :args)
            i = r_b_set(i0, op: {:bif, :is_bitstring}, args: [bin], dst: succDst)
            live_opt_is([i | is], live0, acc)

          :get_map_element ->
            i = r_b_set(i0, op: :has_map_field, dst: succDst)
            live_opt_is([i | is], live0, acc)

          _ ->
            live1 = :gb_sets.delete(succDst, live0)
            live = :gb_sets.add(dst, live1)
            live_opt_is([i0 | is], live, [succI | acc])
        end

      {false, true} ->
        live_opt_is([i0 | is], live0, acc)

      {false, false} ->
        live_opt_is(is, live0, acc)
    end
  end

  defp live_opt_is([r_b_set(dst: dst) = i | is], live0, acc) do
    case :gb_sets.is_member(dst, live0) do
      true ->
        liveUsed = :gb_sets.from_ordset(:beam_ssa.used(i))
        live1 = :gb_sets.union(live0, liveUsed)
        live = :gb_sets.delete(dst, live1)
        live_opt_is(is, live, [i | acc])

      false ->
        case :beam_ssa.no_side_effect(i) do
          true ->
            live_opt_is(is, live0, acc)

          false ->
            liveUsed = :gb_sets.from_ordset(:beam_ssa.used(i))
            live = :gb_sets.union(live0, liveUsed)
            live_opt_is(is, live, [i | acc])
        end
    end
  end

  defp live_opt_is([], live, acc) do
    {acc, live}
  end

  defp ssa_opt_try({r_opt_st(ssa: linear0) = st, funcDb}) do
    linear1 = opt_try(linear0)
    linear = :beam_ssa.trim_unreachable(linear1)
    {r_opt_st(st, ssa: linear), funcDb}
  end

  defp opt_try([
         {l, r_b_blk(is: [r_b_set(op: :new_try_tag)], last: last) = blk0}
         | bs0
       ]) do
    r_b_br(succ: succ, fail: fail) = last
    ws = :cerl_sets.from_list([succ, fail])

    try do
      do_opt_try(bs0, ws)
    catch
      :not_possible ->
        [{l, blk0} | opt_try(bs0)]
    else
      bs ->
        blk =
          r_b_blk(blk0,
            is: [],
            last: r_b_br(bool: r_b_literal(val: true), succ: succ, fail: succ)
          )

        [{l, blk} | opt_try(bs)]
    end
  end

  defp opt_try([{l, blk} | bs]) do
    [{l, blk} | opt_try(bs)]
  end

  defp opt_try([]) do
    []
  end

  defp do_opt_try([{l, blk} | bs] = bs0, ws0) do
    case :cerl_sets.is_element(l, ws0) do
      false ->
        case :cerl_sets.size(ws0) do
          0 ->
            bs0

          _ ->
            [{l, blk} | do_opt_try(bs, ws0)]
        end

      true ->
        ws1 = :cerl_sets.del_element(l, ws0)
        r_b_blk(is: is0) = blk

        case is_safe_without_try(is0, []) do
          {:safe, is} ->
            successors = :beam_ssa.successors(blk)

            ws =
              :cerl_sets.union(
                :cerl_sets.from_list(successors),
                ws1
              )

            [{l, r_b_blk(blk, is: is)} | do_opt_try(bs, ws)]

          :unsafe ->
            throw(:not_possible)

          {:done, is} ->
            [{l, r_b_blk(blk, is: is)} | do_opt_try(bs, ws1)]
        end
    end
  end

  defp do_opt_try([], ws) do
    0 = :cerl_sets.size(ws)
    []
  end

  defp is_safe_without_try([r_b_set(op: :kill_try_tag) | is], acc) do
    {:done, reverse(acc, is)}
  end

  defp is_safe_without_try([r_b_set(op: :extract) | _], _Acc) do
    :unsafe
  end

  defp is_safe_without_try([r_b_set(op: :landingpad) | is], acc) do
    is_safe_without_try(is, acc)
  end

  defp is_safe_without_try([r_b_set(op: {:succeeded, :body}) = i0 | is], acc) do
    i = r_b_set(i0, op: {:succeeded, :guard})
    is_safe_without_try(is, [i | acc])
  end

  defp is_safe_without_try([r_b_set(op: op) = i | is], acc) do
    isSafe =
      case op do
        :phi ->
          true

        _ ->
          :beam_ssa.no_side_effect(i)
      end

    case isSafe do
      true ->
        is_safe_without_try(is, [i | acc])

      false ->
        :unsafe
    end
  end

  defp is_safe_without_try([], acc) do
    {:safe, reverse(acc)}
  end

  defp ssa_opt_bsm({r_opt_st(ssa: linear) = st, funcDb}) do
    extracted0 = bsm_extracted(linear)
    extracted = :cerl_sets.from_list(extracted0)
    {r_opt_st(st, ssa: bsm_skip(linear, extracted)), funcDb}
  end

  defp bsm_skip([{l, r_b_blk(is: is0) = blk} | bs0], extracted) do
    bs = bsm_skip(bs0, extracted)
    is = bsm_skip_is(is0, extracted)
    coalesce_skips({l, r_b_blk(blk, is: is)}, bs)
  end

  defp bsm_skip([], _) do
    []
  end

  defp bsm_skip_is([i0 | is], extracted) do
    case i0 do
      r_b_set(op: :bs_match, dst: ctx, args: [[r_b_literal(val: t) = type, prevCtx] | args0])
      when t !== :float and t !== :string and t !== :skip ->
        i =
          case :cerl_sets.is_element(ctx, extracted) do
            true ->
              i0

            false ->
              args = [[r_b_literal(val: :skip), prevCtx, type] | args0]
              r_b_set(i0, args: args)
          end

        [i | is]

      r_b_set() ->
        [i0 | bsm_skip_is(is, extracted)]
    end
  end

  defp bsm_skip_is([], _) do
    []
  end

  defp bsm_extracted([{_, r_b_blk(is: is)} | bs]) do
    case is do
      [r_b_set(op: :bs_extract, args: [ctx]) | _] ->
        [ctx | bsm_extracted(bs)]

      _ ->
        bsm_extracted(bs)
    end
  end

  defp bsm_extracted([]) do
    []
  end

  defp coalesce_skips(
         {l,
          r_b_blk(
            is: [r_b_set(op: :bs_extract) = extract | is0],
            last: last0
          ) = blk0},
         bs0
       ) do
    case coalesce_skips_is(is0, last0, bs0) do
      :not_possible ->
        [{l, blk0} | bs0]

      {is, last, bs} ->
        blk = r_b_blk(blk0, is: [extract | is], last: last)
        [{l, blk} | bs]
    end
  end

  defp coalesce_skips({l, r_b_blk(is: is0, last: last0) = blk0}, bs0) do
    case coalesce_skips_is(is0, last0, bs0) do
      :not_possible ->
        [{l, blk0} | bs0]

      {is, last, bs} ->
        blk = r_b_blk(blk0, is: is, last: last)
        [{l, blk} | bs]
    end
  end

  defp coalesce_skips_is(
         [
           r_b_set(
             op: :bs_match,
             args: [
               r_b_literal(val: :skip),
               ctx0,
               type,
               flags,
               r_b_literal(val: size0),
               r_b_literal(val: unit0)
             ]
           ) = skip0,
           r_b_set(op: {:succeeded, :guard})
         ],
         r_b_br(succ: l2, fail: fail) = br0,
         bs0
       )
       when is_integer(size0) do
    case bs0 do
      [
        {^l2,
         r_b_blk(
           is: [
             r_b_set(
               op: :bs_match,
               dst: skipDst,
               args: [
                 r_b_literal(val: :skip),
                 _,
                 _,
                 _,
                 r_b_literal(val: size1),
                 r_b_literal(val: unit1)
               ]
             ),
             r_b_set(op: {:succeeded, :guard}) = succeeded
           ],
           last: r_b_br(fail: ^fail) = br
         )}
        | bs
      ]
      when is_integer(size1) ->
        skipBits = size0 * unit0 + size1 * unit1

        skip =
          r_b_set(skip0,
            dst: skipDst,
            args: [
              r_b_literal(val: :skip),
              ctx0,
              type,
              flags,
              r_b_literal(val: skipBits),
              r_b_literal(val: 1)
            ]
          )

        is = [skip, succeeded]
        {is, br, bs}

      [
        {^l2,
         r_b_blk(
           is: [
             r_b_set(
               op: :bs_test_tail,
               args: [_Ctx, r_b_literal(val: tailSkip)]
             )
           ],
           last: r_b_br(succ: nextSucc, fail: ^fail)
         )}
        | bs
      ] ->
        skipBits = size0 * unit0

        testTail =
          r_b_set(skip0,
            op: :bs_test_tail,
            args: [ctx0, r_b_literal(val: skipBits + tailSkip)]
          )

        br = r_b_br(br0, bool: r_b_set(testTail, :dst), succ: nextSucc)
        is = [testTail]
        {is, br, bs}

      _ ->
        :not_possible
    end
  end

  defp coalesce_skips_is(_, _, _) do
    :not_possible
  end

  defp ssa_opt_bsm_shortcut({r_opt_st(ssa: linear) = st, funcDb}) do
    positions = bsm_positions(linear, %{})

    case map_size(positions) do
      0 ->
        {st, funcDb}

      _ ->
        {r_opt_st(st, ssa: bsm_shortcut(linear, positions)), funcDb}
    end
  end

  defp bsm_positions([{l, r_b_blk(is: is, last: last)} | bs], posMap0) do
    posMap = bsm_positions_is(is, posMap0)

    case {is, last} do
      {[r_b_set(op: :bs_test_tail, dst: bool, args: [ctx, r_b_literal(val: bits0)])],
       r_b_br(bool: bool, fail: fail)} ->
        bits = bits0 + :erlang.map_get(ctx, posMap0)
        bsm_positions(bs, %{posMap | l => {bits, fail}})

      {_, _} ->
        bsm_positions(bs, posMap)
    end
  end

  defp bsm_positions([], posMap) do
    posMap
  end

  defp bsm_positions_is(
         [r_b_set(op: :bs_start_match, dst: new) | is],
         posMap0
       ) do
    posMap = %{posMap0 | new => 0}
    bsm_positions_is(is, posMap)
  end

  defp bsm_positions_is(
         [r_b_set(op: :bs_match, dst: new, args: args) | is],
         posMap0
       ) do
    [[_, old] | _] = args
    %{^old => bits0} = posMap0
    bits = bsm_update_bits(args, bits0)
    posMap = %{posMap0 | new => bits}
    bsm_positions_is(is, posMap)
  end

  defp bsm_positions_is([_ | is], posMap) do
    bsm_positions_is(is, posMap)
  end

  defp bsm_positions_is([], posMap) do
    posMap
  end

  defp bsm_update_bits([r_b_literal(val: :string), _, r_b_literal(val: string)], bits) do
    bits + bit_size(string)
  end

  defp bsm_update_bits([r_b_literal(val: :utf8) | _], bits) do
    bits + 8
  end

  defp bsm_update_bits([r_b_literal(val: :utf16) | _], bits) do
    bits + 16
  end

  defp bsm_update_bits([r_b_literal(val: :utf32) | _], bits) do
    bits + 32
  end

  defp bsm_update_bits([_, _, _, r_b_literal(val: sz), r_b_literal(val: u)], bits)
       when is_integer(sz) do
    bits + sz * u
  end

  defp bsm_update_bits(_, bits) do
    bits
  end

  defp bsm_shortcut(
         [{l, r_b_blk(is: is, last: last0) = blk} | bs],
         posMap
       ) do
    case {is, last0} do
      {[
         r_b_set(op: :bs_match, dst: new, args: [[_, old] | _]),
         r_b_set(op: {:succeeded, :guard}, dst: bool, args: [new])
       ], r_b_br(bool: bool, fail: fail)} ->
        case posMap do
          %{^old => bits, ^fail => {tailBits, nextFail}}
          when bits > tailBits ->
            last = r_b_br(last0, fail: nextFail)
            [{l, r_b_blk(blk, last: last)} | bsm_shortcut(bs, posMap)]

          %{} ->
            [{l, blk} | bsm_shortcut(bs, posMap)]
        end

      {_, _} ->
        [{l, blk} | bsm_shortcut(bs, posMap)]
    end
  end

  defp bsm_shortcut([], _PosMap) do
    []
  end

  defp ssa_opt_bs_puts({r_opt_st(ssa: linear0, cnt: count0) = st, funcDb}) do
    {linear, count} = opt_bs_puts(linear0, count0, [])
    {r_opt_st(st, ssa: linear, cnt: count), funcDb}
  end

  defp opt_bs_puts([{l, r_b_blk(is: is) = blk0} | bs], count0, acc0) do
    case is do
      [r_b_set(op: :bs_put) = i0] ->
        case opt_bs_put(l, i0, blk0, count0, acc0) do
          :not_possible ->
            opt_bs_puts(bs, count0, [{l, blk0} | acc0])

          {count, acc1} ->
            acc = opt_bs_puts_merge(acc1)
            opt_bs_puts(bs, count, acc)
        end

      _ ->
        opt_bs_puts(bs, count0, [{l, blk0} | acc0])
    end
  end

  defp opt_bs_puts([], count, acc) do
    {reverse(acc), count}
  end

  defp opt_bs_puts_merge([
         [{l1, r_b_blk(is: is) = blk0}, {l2, r_b_blk(is: accIs)} = bAcc]
         | acc
       ]) do
    case {accIs, is} do
      {[
         r_b_set(
           op: :bs_put,
           args: [
             r_b_literal(val: :binary),
             r_b_literal(),
             r_b_literal(val: bin0),
             r_b_literal(val: :all),
             r_b_literal(val: 1)
           ]
         )
       ],
       [
         r_b_set(
           op: :bs_put,
           args: [
             r_b_literal(val: :binary),
             r_b_literal(),
             r_b_literal(val: bin1),
             r_b_literal(val: :all),
             r_b_literal(val: 1)
           ]
         ) = i0
       ]} ->
        bin = <<bin0::bitstring, bin1::bitstring>>
        i = r_b_set(i0, args: bs_put_args(:binary, bin, :all))
        blk = r_b_blk(blk0, is: [i])
        [{l2, blk} | acc]

      {_, _} ->
        [[{l1, blk0}, bAcc] | acc]
    end
  end

  defp opt_bs_put(l, i0, r_b_blk(last: br0) = blk0, count0, acc) do
    case opt_bs_put(i0) do
      [bin] when is_bitstring(bin) ->
        args = bs_put_args(:binary, bin, :all)
        i = r_b_set(i0, args: args)
        blk = r_b_blk(blk0, is: [i])
        {count0, [{l, blk} | acc]}

      [{:int, int, size}, bin] when is_bitstring(bin) ->
        intArgs = bs_put_args(:integer, int, size)
        binArgs = bs_put_args(:binary, bin, :all)
        {binL, binVarNum} = {count0, count0 + 1}
        count = count0 + 2
        binVar = r_b_var(name: {:"@ssa_bool", binVarNum})
        binI = r_b_set(i0, dst: binVar, args: binArgs)

        binBlk =
          r_b_blk(blk0,
            is: [binI],
            last: r_b_br(br0, bool: binVar)
          )

        intI = r_b_set(i0, args: intArgs)
        intBlk = r_b_blk(blk0, is: [intI], last: r_b_br(br0, succ: binL))
        {count, [[{binL, binBlk}, {l, intBlk}] | acc]}

      :not_possible ->
        :not_possible
    end
  end

  defp opt_bs_put(
         r_b_set(
           args: [
             r_b_literal(val: :binary),
             _,
             r_b_literal(val: val),
             r_b_literal(val: :all),
             r_b_literal(val: unit)
           ]
         )
       )
       when is_bitstring(val) do
    cond do
      rem(bit_size(val), unit) === 0 ->
        [val]

      true ->
        :not_possible
    end
  end

  defp opt_bs_put(
         r_b_set(
           args: [
             r_b_literal(val: type),
             r_b_literal(val: flags),
             r_b_literal(val: val),
             r_b_literal(val: size),
             r_b_literal(val: unit)
           ]
         ) = i0
       )
       when is_integer(size) do
    effectiveSize = size * unit

    cond do
      effectiveSize > 0 ->
        case {type, opt_bs_put_endian(flags)} do
          {:integer, :big} when is_integer(val) ->
            cond do
              effectiveSize < 64 ->
                [<<val::size(effectiveSize)>>]

              true ->
                opt_bs_put_split_int(val, effectiveSize)
            end

          {:integer, :little}
          when is_integer(val) and
                 effectiveSize < 128 ->
            <<int::size(effectiveSize)>> = <<val::size(effectiveSize)-little>>
            args = bs_put_args(type, int, effectiveSize)
            i = r_b_set(i0, args: args)
            opt_bs_put(i)

          {:binary, _} when is_bitstring(val) ->
            case val do
              <<bitstring::size(effectiveSize)-bits, _::bits>> ->
                [bitstring]

              _ ->
                :not_possible
            end

          {:float, endian} ->
            try do
              [opt_bs_put_float(val, effectiveSize, endian)]
            catch
              :error, _ ->
                :not_possible
            end

          {_, _} ->
            :not_possible
        end

      true ->
        :not_possible
    end
  end

  defp opt_bs_put(r_b_set()) do
    :not_possible
  end

  defp opt_bs_put_float(n, sz, endian) do
    case endian do
      :big ->
        <<n::size(sz)-big-float-unit(1)>>

      :little ->
        <<n::size(sz)-little-float-unit(1)>>
    end
  end

  defp bs_put_args(type, val, size) do
    [
      r_b_literal(val: type),
      r_b_literal(val: [:unsigned, :big]),
      r_b_literal(val: val),
      r_b_literal(val: size),
      r_b_literal(val: 1)
    ]
  end

  defp opt_bs_put_endian([:big = e | _]) do
    e
  end

  defp opt_bs_put_endian([:little = e | _]) do
    e
  end

  defp opt_bs_put_endian([:native = e | _]) do
    e
  end

  defp opt_bs_put_endian([_ | fs]) do
    opt_bs_put_endian(fs)
  end

  defp opt_bs_put_split_int(int, size) do
    pos = opt_bs_put_split_int_1(int, 0, size - 1)
    upperSize = size - pos

    cond do
      pos === 0 ->
        :not_possible

      upperSize < 64 ->
        [<<int::size(size)>>]

      true ->
        [{:int, int >>> pos, upperSize}, <<int::size(pos)>>]
    end
  end

  defp opt_bs_put_split_int_1(_Int, l, r) when l > r do
    8 * div(l + 7, 8)
  end

  defp opt_bs_put_split_int_1(int, l, r) do
    mid = div(l + r, 2)

    case int >>> mid do
      upper when upper === 0 or upper === -1 ->
        opt_bs_put_split_int_1(int, l, mid - 1)

      _ ->
        opt_bs_put_split_int_1(int, mid + 1, r)
    end
  end

  defp ssa_opt_tuple_size({r_opt_st(ssa: linear0, cnt: count0) = st, funcDb}) do
    nonGuards = non_guards(linear0)
    {linear, count} = opt_tup_size(linear0, nonGuards, count0, [])
    {r_opt_st(st, ssa: linear, cnt: count), funcDb}
  end

  defp opt_tup_size([{l, r_b_blk(is: is, last: last) = blk} | bs], nonGuards, count0, acc0) do
    case {is, last} do
      {[r_b_set(op: {:bif, :"=:="}, dst: bool, args: [r_b_var() = tup, r_b_literal(val: arity)])],
       r_b_br(bool: bool)}
      when is_integer(arity) and arity >= 0 ->
        {acc, count} = opt_tup_size_1(tup, l, nonGuards, count0, acc0)
        opt_tup_size(bs, nonGuards, count, [{l, blk} | acc])

      {_, _} ->
        opt_tup_size(bs, nonGuards, count0, [{l, blk} | acc0])
    end
  end

  defp opt_tup_size([], _NonGuards, count, acc) do
    {reverse(acc), count}
  end

  defp opt_tup_size_1(size, eqL, nonGuards, count0, [{l, blk0} | acc]) do
    r_b_blk(is: is0, last: last) = blk0

    case last do
      r_b_br(bool: bool, succ: ^eqL, fail: fail) ->
        case :gb_sets.is_member(fail, nonGuards) do
          true ->
            {[{l, blk0} | acc], count0}

          false ->
            case opt_tup_size_is(is0, bool, size, []) do
              :none ->
                {[{l, blk0} | acc], count0}

              {preIs, tupleSizeIs, tuple} ->
                opt_tup_size_2(preIs, tupleSizeIs, l, eqL, tuple, fail, count0, acc)
            end
        end

      _ ->
        {[{l, blk0} | acc], count0}
    end
  end

  defp opt_tup_size_1(_, _, _, count, acc) do
    {acc, count}
  end

  defp opt_tup_size_2(preIs, tupleSizeIs, preL, eqL, tuple, fail, count0, acc) do
    isTupleL = count0
    tupleSizeL = count0 + 1
    bool = r_b_var(name: {:"@ssa_bool", count0 + 2})
    count = count0 + 3
    true__ = r_b_literal(val: true)
    preBr = r_b_br(bool: true__, succ: isTupleL, fail: isTupleL)
    preBlk = r_b_blk(is: preIs, last: preBr)
    isTupleIs = [r_b_set(op: {:bif, :is_tuple}, dst: bool, args: [tuple])]
    isTupleBr = r_b_br(bool: bool, succ: tupleSizeL, fail: fail)
    isTupleBlk = r_b_blk(is: isTupleIs, last: isTupleBr)
    tupleSizeBr = r_b_br(bool: true__, succ: eqL, fail: eqL)
    tupleSizeBlk = r_b_blk(is: tupleSizeIs, last: tupleSizeBr)

    {[
       [{tupleSizeL, tupleSizeBlk}, {isTupleL, isTupleBlk}, {preL, preBlk}]
       | acc
     ], count}
  end

  defp opt_tup_size_is(
         [
           r_b_set(op: {:bif, :tuple_size}, dst: size, args: [tuple]) = i,
           r_b_set(op: {:succeeded, _}, dst: bool, args: [size])
         ],
         bool,
         size,
         acc
       ) do
    {reverse(acc), [i], tuple}
  end

  defp opt_tup_size_is([i | is], bool, size, acc) do
    opt_tup_size_is(is, bool, size, [i | acc])
  end

  defp opt_tup_size_is([], _, _, _Acc) do
    :none
  end

  defp ssa_opt_sw({r_opt_st(ssa: linear0, cnt: count0) = st, funcDb}) do
    {linear, count} = opt_sw(linear0, count0, [])
    {r_opt_st(st, ssa: linear, cnt: count), funcDb}
  end

  defp opt_sw([{l, r_b_blk(is: is, last: r_b_switch() = sw0) = blk0} | bs], count0, acc) do
    case sw0 do
      r_b_switch(arg: arg, fail: fail, list: [{lit, lbl}]) ->
        {bool, count} = new_var(:"@ssa_bool", count0)
        isEq = r_b_set(op: {:bif, :"=:="}, dst: bool, args: [arg, lit])
        br = r_b_br(bool: bool, succ: lbl, fail: fail)
        blk = r_b_blk(blk0, is: is ++ [isEq], last: br)
        opt_sw(bs, count, [{l, blk} | acc])

      r_b_switch(
        arg: arg,
        fail: fail,
        list: [{r_b_literal(val: b1), lbl}, {r_b_literal(val: b2), lbl}]
      )
      when b1 === not b2 ->
        {bool, count} = new_var(:"@ssa_bool", count0)
        isBool = r_b_set(op: {:bif, :is_boolean}, dst: bool, args: [arg])
        br = r_b_br(bool: bool, succ: lbl, fail: fail)
        blk = r_b_blk(blk0, is: is ++ [isBool], last: br)
        opt_sw(bs, count, [{l, blk} | acc])

      _ ->
        opt_sw(bs, count0, [{l, blk0} | acc])
    end
  end

  defp opt_sw([{l, r_b_blk() = blk} | bs], count, acc) do
    opt_sw(bs, count, [{l, blk} | acc])
  end

  defp opt_sw([], count, acc) do
    {reverse(acc), count}
  end

  defp ssa_opt_receive_after({r_opt_st(ssa: linear) = st, funcDb}) do
    {r_opt_st(st, ssa: recv_after_opt(linear)), funcDb}
  end

  defp recv_after_opt([
         [
           {l1,
            r_b_blk(
              is: is0,
              last: r_b_br(bool: r_b_var(), succ: l2, fail: 1)
            ) = blk1},
           {l2,
            r_b_blk(
              is: [],
              last:
                r_b_br(
                  bool: r_b_var() = waitBool,
                  fail: fail
                ) = br0
            ) = blk2}
         ]
         | bs
       ]) do
    case recv_after_opt_is(is0, waitBool, []) do
      {:yes, is} ->
        br = r_b_br(br0, bool: r_b_literal(val: true), succ: fail, fail: fail)
        [{l1, r_b_blk(blk1, is: is, last: br)} | recv_after_opt(bs)]

      :no ->
        [[{l1, blk1}, {l2, blk2}] | recv_after_opt(bs)]
    end
  end

  defp recv_after_opt([b | bs]) do
    [b | recv_after_opt(bs)]
  end

  defp recv_after_opt([]) do
    []
  end

  defp recv_after_opt_is(
         [
           r_b_set(op: :wait_timeout, args: [r_b_literal(val: :infinity)], dst: waitBool) = i0,
           r_b_set(op: {:succeeded, :body}, args: [waitBool])
         ],
         waitBool,
         acc
       ) do
    i = r_b_set(i0, op: :wait, args: [])
    {:yes, reverse(acc, [i])}
  end

  defp recv_after_opt_is([i | is], waitBool, acc) do
    recv_after_opt_is(is, waitBool, [i | acc])
  end

  defp recv_after_opt_is([], _WaitBool, _Acc) do
    :no
  end

  defp ssa_opt_ne({r_opt_st(ssa: linear0) = st, funcDb}) do
    linear = opt_ne(linear0, {:uses, linear0})
    {r_opt_st(st, ssa: linear), funcDb}
  end

  defp opt_ne(
         [
           {l,
            r_b_blk(
              is: [_ | _] = is0,
              last: r_b_br(bool: r_b_var() = bool)
            ) = blk0}
           | bs
         ],
         uses0
       ) do
    case last(is0) do
      r_b_set(op: {:bif, :"=/="}, dst: ^bool) = i0 ->
        i = r_b_set(i0, op: {:bif, :"=:="})
        {blk, uses} = opt_ne_replace(i, blk0, uses0)
        [{l, blk} | opt_ne(bs, uses)]

      r_b_set(op: {:bif, :"/="}, dst: ^bool) = i0 ->
        i = r_b_set(i0, op: {:bif, :==})
        {blk, uses} = opt_ne_replace(i, blk0, uses0)
        [{l, blk} | opt_ne(bs, uses)]

      _ ->
        [{l, blk0} | opt_ne(bs, uses0)]
    end
  end

  defp opt_ne([{l, blk} | bs], uses) do
    [{l, blk} | opt_ne(bs, uses)]
  end

  defp opt_ne([], _Uses) do
    []
  end

  defp opt_ne_replace(
         r_b_set(dst: bool) = i,
         r_b_blk(is: is0, last: r_b_br(succ: succ, fail: fail) = br0) = blk,
         uses0
       ) do
    case opt_ne_single_use(bool, uses0) do
      {true, uses} ->
        is = replace_last(is0, i)
        br = r_b_br(br0, succ: fail, fail: succ)
        {r_b_blk(blk, is: is, last: br), uses}

      {false, uses} ->
        {blk, uses}
    end
  end

  defp replace_last([_], repl) do
    [repl]
  end

  defp replace_last([i | is], repl) do
    [i | replace_last(is, repl)]
  end

  defp opt_ne_single_use(var, {:uses, linear}) do
    uses = :beam_ssa.uses(:maps.from_list(linear))
    opt_ne_single_use(var, uses)
  end

  defp opt_ne_single_use(var, uses) when is_map(uses) do
    {case uses do
       %{^var => [_]} ->
         true

       %{^var => [_ | _]} ->
         false
     end, uses}
  end

  defp ssa_opt_sink({r_opt_st(ssa: linear) = st, funcDb}) do
    case def_blocks(linear) do
      [] ->
        {st, funcDb}

      [_ | _] = defs0 ->
        defs = :maps.from_list(defs0)
        {do_ssa_opt_sink(defs, st), funcDb}
    end
  end

  defp do_ssa_opt_sink(defs, r_opt_st(ssa: linear) = st) do
    used = used_blocks(linear, defs, [])
    blocks0 = :maps.from_list(linear)
    preds = :beam_ssa.predecessors(blocks0)
    {dom, numbering} = :beam_ssa.dominators(blocks0, preds)
    unsuitable = unsuitable(linear, blocks0)
    defLocs0 = new_def_locations(used, defs, dom, numbering, unsuitable)
    ps = partition_deflocs(defLocs0, defs, blocks0)
    defLocs1 = filter_deflocs(ps, preds, blocks0)
    defLocs = sort(defLocs1)

    blocks =
      foldl(
        fn {v, {from, to}}, a ->
          move_defs(v, from, to, a)
        end,
        blocks0,
        defLocs
      )

    r_opt_st(st, ssa: :beam_ssa.linearize(blocks))
  end

  defp def_blocks([{l, r_b_blk(is: is)} | bs]) do
    def_blocks_is(is, l, def_blocks(bs))
  end

  defp def_blocks([]) do
    []
  end

  defp def_blocks_is(
         [
           r_b_set(op: :get_tuple_element, args: [tuple, _], dst: dst)
           | is
         ],
         l,
         acc
       ) do
    def_blocks_is(is, l, [{dst, {l, tuple}} | acc])
  end

  defp def_blocks_is([_ | is], l, acc) do
    def_blocks_is(is, l, acc)
  end

  defp def_blocks_is([], _, acc) do
    acc
  end

  defp used_blocks([{l, blk} | bs], def__, acc0) do
    used = :beam_ssa.used(blk)

    acc =
      for v <- used, :maps.is_key(v, def__) do
        {v, l}
      end ++ acc0

    used_blocks(bs, def__, acc)
  end

  defp used_blocks([], _Def, acc) do
    rel2fam(acc)
  end

  defp partition_deflocs(defLoc, _Defs, blocks) do
    {blkNums0, _} =
      mapfoldl(
        fn l, n ->
          {{l, n}, n + 1}
        end,
        0,
        :beam_ssa.rpo(blocks)
      )

    blkNums = :maps.from_list(blkNums0)

    s =
      for {v, tuple, {from, to}} <- defLoc do
        {tuple, {:erlang.map_get(to, blkNums), {v, {from, to}}}}
      end

    f = rel2fam(s)
    partition_deflocs_1(f, blocks)
  end

  defp partition_deflocs_1([{tuple, defLocs0} | t], blocks) do
    defLocs1 =
      for {_, dL} <- defLocs0 do
        dL
      end

    defLocs = partition_dl(defLocs1, blocks)

    for dL <- defLocs do
      {tuple, dL}
    end ++ partition_deflocs_1(t, blocks)
  end

  defp partition_deflocs_1([], _) do
    []
  end

  defp partition_dl([_] = defLoc, _Blocks) do
    [defLoc]
  end

  defp partition_dl([{_, {_, first}} | _] = defLoc0, blocks) do
    rPO = :beam_ssa.rpo([first], blocks)
    {p, defLoc} = partition_dl_1(defLoc0, rPO, [])
    [p | partition_dl(defLoc, blocks)]
  end

  defp partition_dl([], _Blocks) do
    []
  end

  defp partition_dl_1([{_, {_, l}} = dL | dLs], [l | _] = ls, acc) do
    partition_dl_1(dLs, ls, [dL | acc])
  end

  defp partition_dl_1([_ | _] = dLs, [_ | ls], acc) do
    partition_dl_1(dLs, ls, acc)
  end

  defp partition_dl_1([], _, acc) do
    {reverse(acc), []}
  end

  defp partition_dl_1([_ | _] = dLs, [], acc) do
    {reverse(acc), dLs}
  end

  defp filter_deflocs([{tuple, defLoc0} | dLs], preds, blocks) do
    [{_, {_, first}} | _] = defLoc0
    paths = find_paths_to_check(defLoc0, first)

    willGC0 =
      :ordsets.from_list(
        for {{_, _} = fromTo, _} <- paths do
          fromTo
        end
      )

    willGC1 =
      for {from, to} <- willGC0 do
        {{from, to}, will_gc(from, to, preds, blocks, true)}
      end

    willGC = :maps.from_list(willGC1)

    {defLocGC0, defLocNoGC} =
      partition(
        fn {{_, _} = fromTo, _} ->
          :erlang.map_get(fromTo, willGC)
        end,
        paths
      )

    defLocGC = filter_gc_deflocs(defLocGC0, tuple, first, preds, blocks)
    defLoc1 = defLocGC ++ defLocNoGC

    for {_, {_, {from, to}} = dL} <- defLoc1,
        from !== to do
      dL
    end ++ filter_deflocs(dLs, preds, blocks)
  end

  defp filter_deflocs([], _, _) do
    []
  end

  defp filter_gc_deflocs(defLocGC, tuple, first, preds, blocks) do
    case defLocGC do
      [] ->
        []

      [{_, {_, {from, to}}}] ->
        case is_on_stack(first, tuple, blocks) do
          true ->
            defLocGC

          false ->
            case will_gc(from, to, preds, blocks, false) do
              false ->
                defLocGC

              true ->
                []
            end
        end

      [[_, _] | _] ->
        defLocGC
    end
  end

  defp find_paths_to_check([{_, {_, to}} = move | t], first) do
    [{{first, to}, move} | find_paths_to_check(t, first)]
  end

  defp find_paths_to_check([], _First) do
    []
  end

  defp will_gc(from, to, preds, blocks, all) do
    between = :beam_ssa.between(from, to, preds, blocks)
    will_gc_1(between, to, blocks, all, %{from => false})
  end

  defp will_gc_1([to | _], to, _Blocks, _All, willGC) do
    :erlang.map_get(to, willGC)
  end

  defp will_gc_1([l | ls], to, blocks, all, willGC0) do
    r_b_blk(is: is) = blk = :erlang.map_get(l, blocks)
    gC = :erlang.map_get(l, willGC0) or will_gc_is(is, all)
    willGC = gc_update_successors(blk, gC, willGC0)
    will_gc_1(ls, to, blocks, all, willGC)
  end

  defp will_gc_is([r_b_set(op: :call, args: args) | is], false) do
    case args do
      [r_b_remote(mod: r_b_literal(val: :erlang)) | _] ->
        will_gc_is(is, false)

      [_ | _] ->
        true
    end
  end

  defp will_gc_is([_ | is], false) do
    will_gc_is(is, false)
  end

  defp will_gc_is([i | is], all) do
    :beam_ssa.clobbers_xregs(i) or will_gc_is(is, all)
  end

  defp will_gc_is([], _All) do
    false
  end

  defp is_on_stack(from, var, blocks) do
    is_on_stack(:beam_ssa.rpo([from], blocks), var, blocks, %{from => false})
  end

  defp is_on_stack([l | ls], var, blocks, willGC0) do
    r_b_blk(is: is) = blk = :erlang.map_get(l, blocks)
    gC0 = :erlang.map_get(l, willGC0)

    try do
      is_on_stack_is(is, var, gC0)
    catch
      {:done, gC} ->
        gC
    else
      gC ->
        willGC = gc_update_successors(blk, gC, willGC0)
        is_on_stack(ls, var, blocks, willGC)
    end
  end

  defp is_on_stack([], _Var, _, _) do
    false
  end

  defp is_on_stack_is([r_b_set(op: :get_tuple_element) | is], var, gC) do
    is_on_stack_is(is, var, gC)
  end

  defp is_on_stack_is([i | is], var, gC0) do
    case gC0 and member(var, :beam_ssa.used(i)) do
      true ->
        throw({:done, gC0})

      false ->
        gC = gC0 or :beam_ssa.clobbers_xregs(i)
        is_on_stack_is(is, var, gC)
    end
  end

  defp is_on_stack_is([], _, gC) do
    gC
  end

  defp gc_update_successors(blk, gC, willGC) do
    foldl(
      fn l, acc ->
        case acc do
          %{^l => true} ->
            acc

          %{^l => false} when gC === false ->
            acc

          %{} ->
            %{acc | l => gC}
        end
      end,
      willGC,
      :beam_ssa.successors(blk)
    )
  end

  defp unsuitable(linear, blocks) do
    predecessors = :beam_ssa.predecessors(blocks)
    unsuitable0 = unsuitable_1(linear)
    unsuitable1 = unsuitable_recv(linear, blocks, predecessors)
    :gb_sets.from_list(unsuitable0 ++ unsuitable1)
  end

  defp unsuitable_1([{l, r_b_blk(is: [r_b_set(op: op) = i | _])} | bs]) do
    unsuitable =
      case op do
        :bs_extract ->
          true

        :bs_put ->
          true

        {:float, _} ->
          true

        :landingpad ->
          true

        _ ->
          :beam_ssa.is_loop_header(i)
      end

    case unsuitable do
      true ->
        [l | unsuitable_1(bs)]

      false ->
        unsuitable_1(bs)
    end
  end

  defp unsuitable_1([{_, r_b_blk()} | bs]) do
    unsuitable_1(bs)
  end

  defp unsuitable_1([]) do
    []
  end

  defp unsuitable_recv([{l, r_b_blk(is: [r_b_set(op: op) | _])} | bs], blocks, predecessors) do
    ls =
      case op do
        :remove_message ->
          unsuitable_loop(l, blocks, predecessors)

        :recv_next ->
          unsuitable_loop(l, blocks, predecessors)

        _ ->
          []
      end

    ls ++ unsuitable_recv(bs, blocks, predecessors)
  end

  defp unsuitable_recv([_ | bs], blocks, predecessors) do
    unsuitable_recv(bs, blocks, predecessors)
  end

  defp unsuitable_recv([], _, _) do
    []
  end

  defp unsuitable_loop(l, blocks, predecessors) do
    unsuitable_loop(l, blocks, predecessors, [])
  end

  defp unsuitable_loop(l, blocks, predecessors, acc) do
    ps = :erlang.map_get(l, predecessors)
    unsuitable_loop_1(ps, blocks, predecessors, acc)
  end

  defp unsuitable_loop_1([p | ps], blocks, predecessors, acc0) do
    case is_loop_header(p, blocks) do
      true ->
        unsuitable_loop_1(ps, blocks, predecessors, acc0)

      false ->
        case :ordsets.is_element(p, acc0) do
          false ->
            acc1 = :ordsets.add_element(p, acc0)
            acc = unsuitable_loop(p, blocks, predecessors, acc1)
            unsuitable_loop_1(ps, blocks, predecessors, acc)

          true ->
            unsuitable_loop_1(ps, blocks, predecessors, acc0)
        end
    end
  end

  defp unsuitable_loop_1([], _, _, acc) do
    acc
  end

  defp is_loop_header(l, blocks) do
    case :erlang.map_get(l, blocks) do
      r_b_blk(is: [i | _]) ->
        :beam_ssa.is_loop_header(i)

      r_b_blk() ->
        false
    end
  end

  defp new_def_locations([{v, usedIn} | vs], defs, dom, numbering, unsuitable) do
    {defIn, tuple} = :erlang.map_get(v, defs)
    common = common_dominator(usedIn, dom, numbering, unsuitable)

    sink =
      case member(
             common,
             :erlang.map_get(defIn, dom)
           ) do
        true ->
          {v, tuple, {defIn, defIn}}

        false ->
          {v, tuple, {defIn, common}}
      end

    [sink | new_def_locations(vs, defs, dom, numbering, unsuitable)]
  end

  defp new_def_locations([], _, _, _, _) do
    []
  end

  defp common_dominator(ls0, dom, numbering, unsuitable) do
    [common | _] = :beam_ssa.common_dominators(ls0, dom, numbering)

    case :gb_sets.is_member(common, unsuitable) do
      true ->
        [[^common, oneUp] | _] = :erlang.map_get(common, dom)
        common_dominator([oneUp], dom, numbering, unsuitable)

      false ->
        common
    end
  end

  defp move_defs(v, from, to, blocks) do
    %{^from => fromBlk0, ^to => toBlk0} = blocks
    {def__, fromBlk} = remove_def(v, fromBlk0)

    try do
      insert_def(v, def__, toBlk0)
    catch
      :not_possible ->
        blocks
    else
      toBlk ->
        %{blocks | from => fromBlk, to => toBlk}
    end
  end

  defp remove_def(v, r_b_blk(is: is0) = blk) do
    {def__, is} = remove_def_is(is0, v, [])
    {def__, r_b_blk(blk, is: is)}
  end

  defp remove_def_is([r_b_set(dst: dst) = def__ | is], dst, acc) do
    {def__, reverse(acc, is)}
  end

  defp remove_def_is([i | is], dst, acc) do
    remove_def_is(is, dst, [i | acc])
  end

  defp insert_def(v, def__, r_b_blk(is: is0) = blk) do
    is = insert_def_is(is0, v, def__)
    r_b_blk(blk, is: is)
  end

  defp insert_def_is([r_b_set(op: :phi) = i | is], v, def__) do
    case member(v, :beam_ssa.used(i)) do
      true ->
        throw(:not_possible)

      false ->
        [i | insert_def_is(is, v, def__)]
    end
  end

  defp insert_def_is([r_b_set(op: op) = i | is] = is0, v, def__) do
    action0 =
      case op do
        :call ->
          :beyond

        :catch_end ->
          :beyond

        :timeout ->
          :beyond

        _ ->
          :here
      end

    action =
      case is do
        [r_b_set(op: {:succeeded, _}) | _] ->
          :here

        _ ->
          action0
      end

    case action do
      :beyond ->
        case member(v, :beam_ssa.used(i)) do
          true ->
            [def__ | is0]

          false ->
            [i | insert_def_is(is, v, def__)]
        end

      :here ->
        [def__ | is0]
    end
  end

  defp insert_def_is([], _V, def__) do
    [def__]
  end

  defp ssa_opt_get_tuple_element({r_opt_st(ssa: blocks0) = st, funcDb}) do
    blocks =
      opt_get_tuple_element(
        :maps.to_list(blocks0),
        blocks0
      )

    {r_opt_st(st, ssa: blocks), funcDb}
  end

  defp opt_get_tuple_element([{l, r_b_blk(is: is0) = blk0} | bs], blocks) do
    case opt_get_tuple_element_is(is0, false, []) do
      {:yes, is} ->
        blk = r_b_blk(blk0, is: is)
        opt_get_tuple_element(bs, %{blocks | l => blk})

      :no ->
        opt_get_tuple_element(bs, blocks)
    end
  end

  defp opt_get_tuple_element([], blocks) do
    blocks
  end

  defp opt_get_tuple_element_is(
         [
           r_b_set(
             op: :get_tuple_element,
             args: [r_b_var() = src, _]
           ) = i0
           | is0
         ],
         _AnyChange,
         acc
       ) do
    {getIs0, is} = collect_get_tuple_element(is0, src, [i0])

    getIs1 =
      sort(
        for r_b_set(args: [_, pos]) = i <- getIs0 do
          {pos, i}
        end
      )

    getIs =
      for {_, i} <- getIs1 do
        i
      end

    opt_get_tuple_element_is(is, true, reverse(getIs, acc))
  end

  defp opt_get_tuple_element_is([i | is], anyChange, acc) do
    opt_get_tuple_element_is(is, anyChange, [i | acc])
  end

  defp opt_get_tuple_element_is([], anyChange, acc) do
    case anyChange do
      true ->
        {:yes, reverse(acc)}

      false ->
        :no
    end
  end

  defp collect_get_tuple_element(
         [
           r_b_set(op: :get_tuple_element, args: [src, _]) = i
           | is
         ],
         src,
         acc
       ) do
    collect_get_tuple_element(is, src, [i | acc])
  end

  defp collect_get_tuple_element(is, _Src, acc) do
    {acc, is}
  end

  defp ssa_opt_unfold_literals({st, funcDb}) do
    r_opt_st(ssa: blocks0, args: args, anno: anno, cnt: count0) = st
    paramInfo = :maps.get(:parameter_info, anno, %{})
    litMap = collect_arg_literals(args, paramInfo, 0, %{})

    case map_size(litMap) do
      0 ->
        {st, funcDb}

      _ ->
        safeMap = %{0 => true}

        {blocks, count} =
          unfold_literals(:beam_ssa.rpo(blocks0), litMap, safeMap, count0, blocks0)

        {r_opt_st(st, ssa: blocks, cnt: count), funcDb}
    end
  end

  defp collect_arg_literals([v | vs], info, x, acc0) do
    case info do
      %{^v => varInfo} ->
        type = :proplists.get_value(:type, varInfo, :any)

        case :beam_types.get_singleton_value(type) do
          {:ok, val} ->
            f = fn vars ->
              [{x, v} | vars]
            end

            acc = :maps.update_with(val, f, [{x, v}], acc0)
            collect_arg_literals(vs, info, x + 1, acc)

          :error ->
            collect_arg_literals(vs, info, x + 1, acc0)
        end

      %{} ->
        collect_arg_literals(vs, info, x + 1, acc0)
    end
  end

  defp collect_arg_literals([], _Info, _X, acc) do
    acc
  end

  defp unfold_literals([l | ls], litMap, safeMap0, count0, blocks0) do
    {blocks, safe, count} =
      case :erlang.map_get(
             l,
             safeMap0
           ) do
        false ->
          {blocks0, false, count0}

        true ->
          r_b_blk(is: is0) =
            blk =
            :erlang.map_get(
              l,
              blocks0
            )

          {is, safe0, count1} =
            unfold_lit_is(
              is0,
              litMap,
              count0,
              []
            )

          {%{blocks0 | l => r_b_blk(blk, is: is)}, safe0, count1}
      end

    successors = :beam_ssa.successors(l, blocks)
    safeMap = unfold_update_succ(successors, safe, safeMap0)
    unfold_literals(ls, litMap, safeMap, count, blocks)
  end

  defp unfold_literals([], _, _, count, blocks) do
    {blocks, count}
  end

  defp unfold_update_succ([s | ss], safe, safeMap0) do
    f = fn prev ->
      :erlang.and(prev, safe)
    end

    safeMap = :maps.update_with(s, f, safe, safeMap0)
    unfold_update_succ(ss, safe, safeMap)
  end

  defp unfold_update_succ([], _, safeMap) do
    safeMap
  end

  defp unfold_lit_is(
         [
           r_b_set(
             op: :call,
             args: [
               r_b_remote(
                 mod: r_b_literal(val: :erlang),
                 name: r_b_literal(val: :error),
                 arity: 2
               ),
               r_b_literal(val: :function_clause),
               argumentList
             ]
           ) = i0
           | is
         ],
         litMap,
         count0,
         acc0
       ) do
    case unfold_arg_list(acc0, argumentList, litMap, count0, 0, []) do
      {[finalPutList | _] = acc, count} ->
        r_b_set(op: :put_list, dst: listVar) = finalPutList
        r_b_set(args: [erlangError, fc, _]) = i0
        i = r_b_set(i0, args: [erlangError, fc, listVar])
        {reverse(acc, [i | is]), false, count}

      {[], _} ->
        {reverse(acc0, [i0 | is]), false, count0}
    end
  end

  defp unfold_lit_is([r_b_set(op: op, args: args0) = i0 | is], litMap, count, acc) do
    unfold =
      case op do
        :call ->
          true

        :old_make_fun ->
          true

        _ ->
          false
      end

    i =
      case unfold do
        true ->
          args = unfold_call_args(args0, litMap, -1)
          r_b_set(i0, args: args)

        false ->
          i0
      end

    case :beam_ssa.clobbers_xregs(i) do
      true ->
        {reverse(acc, [i | is]), false, count}

      false ->
        unfold_lit_is(is, litMap, count, [i | acc])
    end
  end

  defp unfold_lit_is([], _LitMap, count, acc) do
    {reverse(acc), true, count}
  end

  defp unfold_arg_list(is, r_b_literal(val: [hd | tl]), litMap, count0, x, acc) do
    {putListDst, count} = new_var(:"@put_list", count0)

    putList =
      r_b_set(op: :put_list, dst: putListDst, args: [r_b_literal(val: hd), r_b_literal(val: tl)])

    unfold_arg_list([putList | is], putListDst, litMap, count, x, acc)
  end

  defp unfold_arg_list(
         [
           r_b_set(op: :put_list, dst: list, args: [hd0, r_b_literal(val: [hd | tl])]) = i0
           | is0
         ],
         list,
         litMap,
         count0,
         x,
         acc
       ) do
    {putListDst, count} = new_var(:"@put_list", count0)

    putList =
      r_b_set(op: :put_list, dst: putListDst, args: [r_b_literal(val: hd), r_b_literal(val: tl)])

    i = r_b_set(i0, args: [hd0, putListDst])
    unfold_arg_list([[i, putList] | is0], list, litMap, count, x, acc)
  end

  defp unfold_arg_list(
         [
           r_b_set(op: :put_list, dst: list, args: [hd0, tl]) = i0
           | is
         ],
         list,
         litMap,
         count,
         x,
         acc
       ) do
    hd = unfold_arg(hd0, litMap, x)
    i = r_b_set(i0, args: [hd, tl])
    unfold_arg_list(is, tl, litMap, count, x + 1, [i | acc])
  end

  defp unfold_arg_list([i | is], list, litMap, count, x, acc) do
    unfold_arg_list(is, list, litMap, count, x, [i | acc])
  end

  defp unfold_arg_list([], _, _, count, _, acc) do
    {reverse(acc), count}
  end

  defp unfold_call_args([a0 | as], litMap, x) do
    a = unfold_arg(a0, litMap, x)
    [a | unfold_call_args(as, litMap, x + 1)]
  end

  defp unfold_call_args([], _, _) do
    []
  end

  defp unfold_arg(r_b_literal(val: val) = lit, litMap, x) do
    case litMap do
      %{^val => vars} ->
        case keyfind(x, 1, vars) do
          false ->
            lit

          {^x, var} ->
            var
        end

      %{} ->
        lit
    end
  end

  defp unfold_arg(expr, _LitMap, _X) do
    expr
  end

  defp ssa_opt_tail_calls({st, funcDb}) do
    r_opt_st(ssa: blocks0) = st
    blocks = opt_tail_calls(:beam_ssa.rpo(blocks0), blocks0)
    {r_opt_st(st, ssa: blocks), funcDb}
  end

  defp opt_tail_calls([l | ls], blocks0) do
    r_b_blk(is: is0, last: last) =
      blk0 =
      :erlang.map_get(
        l,
        blocks0
      )

    case is_potential_tail_call(last, blocks0) do
      {:yes, bool, ret} ->
        case is_tail_call_is(is0, bool, ret, []) do
          {:yes, is, var} ->
            blk = r_b_blk(blk0, is: is, last: r_b_ret(arg: var))
            blocks = %{blocks0 | l => blk}
            opt_tail_calls(ls, blocks)

          :no ->
            opt_tail_calls(ls, blocks0)
        end

      :no ->
        opt_tail_calls(ls, blocks0)
    end
  end

  defp opt_tail_calls([], blocks) do
    blocks
  end

  defp is_potential_tail_call(r_b_br(bool: r_b_var() = bool, succ: succ), blocks) do
    case blocks do
      %{^succ => r_b_blk(is: [], last: r_b_ret(arg: arg))} ->
        {:yes, bool, arg}

      %{} ->
        :no
    end
  end

  defp is_potential_tail_call(_, _) do
    :no
  end

  defp is_tail_call_is(
         [r_b_set(op: :call, dst: dst) = call, r_b_set(op: {:succeeded, :body}, dst: bool)],
         bool,
         ret,
         acc
       ) do
    isTailCall =
      case ret do
        r_b_literal(val: val) ->
          type = :beam_ssa.get_anno(:result_type, call, :any)

          case :beam_types.get_singleton_value(type) do
            {:ok, ^val} ->
              true

            {:ok, _} ->
              false

            :error ->
              false
          end

        r_b_var() ->
          ret === dst
      end

    case isTailCall do
      true ->
        is = reverse(acc, [call])
        {:yes, is, dst}

      false ->
        :no
    end
  end

  defp is_tail_call_is([i | is], bool, ret, acc) do
    is_tail_call_is(is, bool, ret, [i | acc])
  end

  defp is_tail_call_is([], _Bool, _Ret, _Acc) do
    :no
  end

  defp non_guards(linear) do
    :gb_sets.from_list(non_guards_1(linear))
  end

  defp non_guards_1([{l, r_b_blk(is: is)} | bs]) do
    case is do
      [r_b_set(op: :landingpad) | _] ->
        [l | non_guards_1(bs)]

      _ ->
        non_guards_1(bs)
    end
  end

  defp non_guards_1([]) do
    [1]
  end

  defp rel2fam(s0) do
    s1 = :sofs.relation(s0)
    s = :sofs.rel2fam(s1)
    :sofs.to_external(s)
  end

  defp sub(i, sub) do
    :beam_ssa.normalize(sub_1(i, sub))
  end

  defp sub_1(r_b_set(op: :phi, args: args) = i, sub) do
    r_b_set(i,
      args:
        for {a, p} <- args do
          {sub_arg(a, sub), p}
        end
    )
  end

  defp sub_1(r_b_set(args: args) = i, sub) do
    r_b_set(i,
      args:
        for a <- args do
          sub_arg(a, sub)
        end
    )
  end

  defp sub_1(r_b_br(bool: r_b_var() = old) = br, sub) do
    new = sub_arg(old, sub)
    r_b_br(br, bool: new)
  end

  defp sub_1(r_b_switch(arg: r_b_var() = old) = sw, sub) do
    new = sub_arg(old, sub)
    r_b_switch(sw, arg: new)
  end

  defp sub_1(r_b_ret(arg: r_b_var() = old) = ret, sub) do
    new = sub_arg(old, sub)
    r_b_ret(ret, arg: new)
  end

  defp sub_1(last, _) do
    last
  end

  defp sub_arg(r_b_remote(mod: mod, name: name) = rem, sub) do
    r_b_remote(rem,
      mod: sub_arg(mod, sub),
      name: sub_arg(name, sub)
    )
  end

  defp sub_arg(old, sub) do
    case sub do
      %{^old => new} ->
        new

      %{} ->
        old
    end
  end

  defp new_var(r_b_var(name: {base, n}), count) do
    true = is_integer(n)
    {r_b_var(name: {base, count}), count + 1}
  end

  defp new_var(r_b_var(name: base), count) do
    {r_b_var(name: {base, count}), count + 1}
  end

  defp new_var(base, count) when is_atom(base) do
    {r_b_var(name: {base, count}), count + 1}
  end
end
