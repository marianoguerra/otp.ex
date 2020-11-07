defmodule :m_beam_ssa_bool do
  use Bitwise

  import :lists,
    only: [all: 2, foldl: 3, keyfind: 3, last: 1, partition: 2, reverse: 1, reverse: 2, sort: 1]

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

  Record.defrecord(:r_st, :st,
    defs: %{},
    ldefs: %{},
    count: :undefined,
    dom: :undefined,
    uses: :undefined
  )

  def module(r_b_module(body: fs0) = module, _Opts) do
    fs =
      for f <- fs0 do
        function(f)
      end

    {:ok, r_b_module(module, body: fs)}
  end

  defp function(r_b_function(anno: anno) = f) do
    try do
      opt_function(f)
    catch
      class, error ->
        %{:func_info => {_, name, arity}} = anno
        :io.fwrite('Function: ~w/~w\n', [name, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  defp opt_function(r_b_function(bs: blocks0, cnt: count0) = f) do
    {blocks1, count1} = pre_opt(blocks0, count0)
    defVars = interesting_defs(blocks1)

    cond do
      map_size(defVars) > 1 ->
        dom = :beam_ssa.dominators(blocks1)
        uses = :beam_ssa.uses(blocks1)
        st0 = r_st(defs: defVars, count: count1, dom: dom, uses: uses)
        {blocks2, st} = bool_opt(blocks1, st0)
        count = r_st(st, :count)
        blocks3 = :beam_ssa.trim_unreachable(blocks2)
        blocks = :beam_ssa.merge_blocks(blocks3)
        r_b_function(f, bs: blocks, cnt: count)

      true ->
        r_b_function(f, bs: blocks1, cnt: count1)
    end
  end

  defp pre_opt(blocks, count) do
    top = :beam_ssa.rpo(blocks)
    sub0 = %{:uses => {:uses, blocks}}
    sub1 = get_phi_info(top, blocks, sub0)
    sub = :maps.remove(:uses, sub1)
    reached = :cerl_sets.from_list([hd(top)])
    pre_opt(top, sub, reached, count, blocks)
  end

  defp get_phi_info([l | ls], blocks, sub0) do
    sub = get_phi_info(ls, blocks, sub0)
    r_b_blk(is: is) = :erlang.map_get(l, blocks)
    get_phi_info_is(is, l, sub)
  end

  defp get_phi_info([], _, sub) do
    sub
  end

  defp get_phi_info_is([i | is], from, sub0) do
    sub = get_phi_info_is(is, from, sub0)
    get_phi_info_instr(i, from, sub)
  end

  defp get_phi_info_is([], _, sub) do
    sub
  end

  defp get_phi_info_instr(
         r_b_set(
           op: {:bif, :"=:="},
           args: [r_b_var() = bool, r_b_literal(val: true)]
         ),
         _From,
         sub
       ) do
    %{sub | bool => :"=:="}
  end

  defp get_phi_info_instr(r_b_set(op: :phi, dst: dst, args: args), from, sub0) do
    {safe, sub} =
      case sub0 do
        %{^dst => :"=:="} ->
          get_phi_info_single_use(dst, sub0)

        %{^dst => {:true_or_any, _}} ->
          get_phi_info_single_use(dst, sub0)

        %{} ->
          {false, sub0}
      end

    case safe do
      true ->
        foldl(
          fn
            {r_b_var() = v, _}, a ->
              %{a | v => {:true_or_any, from}}

            _, a ->
              a
          end,
          sub,
          args
        )

      false ->
        sub
    end
  end

  defp get_phi_info_instr(_, _, sub) do
    sub
  end

  defp get_phi_info_single_use(var, sub) do
    case :erlang.map_get(:uses, sub) do
      uses when is_map(uses) ->
        {case uses do
           %{^var => [_]} ->
             true

           %{^var => [_ | _]} ->
             false
         end, sub}

      {:uses, blocks} ->
        uses = :beam_ssa.uses(blocks)
        get_phi_info_single_use(var, %{sub | :uses => uses})
    end
  end

  defp pre_opt([l | ls], sub0, reached0, count0, blocks) do
    case :cerl_sets.is_element(l, reached0) do
      false ->
        pre_opt(ls, sub0, reached0, count0, :maps.remove(l, blocks))

      true ->
        r_b_blk(is: is0, last: last0) =
          blk0 =
          :erlang.map_get(
            l,
            blocks
          )

        {is, sub} = pre_opt_is(is0, reached0, sub0, [])

        case pre_opt_terminator(last0, sub, blocks) do
          {r_b_set() = test0, r_b_br() = br0} ->
            bool = r_b_var(name: {:"@ssa_bool", count0})
            count = count0 + 1
            test = r_b_set(test0, dst: bool)
            br = :beam_ssa.normalize(r_b_br(br0, bool: bool))
            blk = r_b_blk(blk0, is: is ++ [test], last: br)
            successors = :beam_ssa.successors(blk)

            reached =
              :cerl_sets.union(
                reached0,
                :cerl_sets.from_list(successors)
              )

            pre_opt(ls, sub, reached, count, %{blocks | l => blk})

          last ->
            blk = r_b_blk(blk0, is: is, last: last)
            successors = :beam_ssa.successors(blk)

            reached =
              :cerl_sets.union(
                reached0,
                :cerl_sets.from_list(successors)
              )

            pre_opt(ls, sub, reached, count0, %{blocks | l => blk})
        end
    end
  end

  defp pre_opt([], _, _, count, blocks) do
    {blocks, count}
  end

  defp pre_opt_is([r_b_set(op: :phi, dst: dst, args: args0) = i0 | is], reached, sub0, acc) do
    args1 =
      for {val, from} <- args0,
          :cerl_sets.is_element(from, reached) do
        {val, from}
      end

    args = sub_args(args1, sub0)

    case all_same(args) do
      true ->
        {arg, _} = hd(args)
        sub = %{sub0 | dst => arg}
        pre_opt_is(is, reached, sub, acc)

      false ->
        case pre_is_phi_bool(args, sub0) do
          true ->
            anno = r_b_set(i0, :anno)

            i =
              r_b_set(i0,
                args: args,
                anno: %{anno | :boolean_phi => true}
              )

            sub = %{sub0 | dst => i}
            pre_opt_is(is, reached, sub, [i | acc])

          false ->
            i = r_b_set(i0, args: args)
            pre_opt_is(is, reached, sub0, [i | acc])
        end
    end
  end

  defp pre_opt_is(
         [
           r_b_set(op: {:succeeded, _}, dst: dst, args: args0) = i0
           | is
         ],
         reached,
         sub0,
         acc
       ) do
    [arg] = args = sub_args(args0, sub0)
    i = r_b_set(i0, args: args)

    case pre_is_safe_bool(arg, sub0) do
      true ->
        sub = %{sub0 | dst => r_b_literal(val: true)}
        pre_opt_is(is, reached, sub, acc)

      false ->
        pre_opt_is(is, reached, sub0, [i | acc])
    end
  end

  defp pre_opt_is([r_b_set(dst: dst, args: args0) = i0 | is], reached, sub0, acc) do
    args = sub_args(args0, sub0)
    i = r_b_set(i0, args: args)

    case is_bool_expr(i) do
      true ->
        case pre_eval_op(i, sub0) do
          :none ->
            sub = %{sub0 | dst => i}
            pre_opt_is(is, reached, sub, [i | acc])

          r_b_var() = var ->
            [r_b_set(op: {:succeeded, _}, dst: succDst, args: [^dst])] = is
            sub = %{sub0 | dst => var, succDst => r_b_literal(val: true)}
            pre_opt_is([], reached, sub, acc)

          r_b_literal() = lit ->
            sub = %{sub0 | dst => lit}
            pre_opt_is(is, reached, sub, acc)
        end

      false ->
        pre_opt_is(is, reached, sub0, [i | acc])
    end
  end

  defp pre_opt_is([], _Reached, sub, acc) do
    {reverse(acc), sub}
  end

  defp pre_opt_terminator(r_b_br(bool: r_b_literal()) = br, _Sub, _Blocks) do
    br
  end

  defp pre_opt_terminator(r_b_br(bool: bool) = br0, sub, blocks) do
    case :beam_ssa.normalize(
           r_b_br(br0,
             bool:
               sub_arg(
                 bool,
                 sub
               )
           )
         ) do
      ^br0 ->
        br0

      r_b_br(bool: r_b_literal(val: true), succ: next) = br ->
        r_b_blk(is: is, last: last) = :erlang.map_get(next, blocks)

        case {is, last} do
          {[], r_b_switch()} ->
            pre_opt_terminator(last, sub, blocks)

          {_, _} ->
            br
        end
    end
  end

  defp pre_opt_terminator(r_b_ret(arg: arg) = ret, sub, _Blocks) do
    :beam_ssa.normalize(r_b_ret(ret, arg: sub_arg(arg, sub)))
  end

  defp pre_opt_terminator(r_b_switch(arg: arg0) = sw0, sub, blocks) do
    case :beam_ssa.normalize(
           r_b_switch(sw0,
             arg:
               sub_arg(
                 arg0,
                 sub
               )
           )
         ) do
      r_b_switch(arg: arg, list: list) = sw ->
        case sort(list) do
          [{r_b_literal(val: false), fail}, {r_b_literal(val: true), succ}] ->
            case pre_is_arg_bool(arg, sub) do
              false ->
                pre_opt_sw(sw, fail, succ, sub, blocks)

              true ->
                :beam_ssa.normalize(r_b_br(bool: arg, succ: succ, fail: fail))
            end

          _ ->
            sw
        end

      other ->
        pre_opt_terminator(other, sub, blocks)
    end
  end

  defp pre_opt_sw(r_b_switch(arg: arg, fail: fail) = sw, false__, true__, sub, blocks) do
    case sub do
      %{^arg => {:true_or_any, phiL}} ->
        %{^fail => failBlk, ^false__ => falseBlk, ^phiL => phiBlk} = blocks

        case {failBlk, falseBlk, phiBlk} do
          {r_b_blk(is: [], last: r_b_br(succ: ^phiL, fail: ^phiL)),
           r_b_blk(is: [], last: r_b_br(succ: ^phiL, fail: ^phiL)),
           r_b_blk(is: [r_b_set(op: :phi, args: phiArgs) | _])} ->
            case keyfind(false__, 2, phiArgs) do
              {r_b_literal(val: bool), ^false__} when bool !== true ->
                dummyDst = r_b_var(name: 0)
                br0 = r_b_br(bool: dummyDst, succ: true__, fail: false__)
                br = :beam_ssa.normalize(br0)

                {r_b_set(op: {:bif, :"=:="}, dst: dummyDst, args: [arg, r_b_literal(val: true)]),
                 br}

              {_, _} ->
                sw
            end

          {_, _, _} ->
            sw
        end

      %{} ->
        sw
    end
  end

  defp pre_eval_op(r_b_set(op: {:bif, op}, args: args), sub) do
    case pre_are_args_bool(args, sub) do
      true ->
        case {op, args} do
          {:and, [r_b_literal(val: true), r_b_var() = res]} ->
            res

          {:and, [r_b_literal(val: false) = res, r_b_var()]} ->
            res

          {:and, [r_b_var() = res, r_b_literal(val: true)]} ->
            res

          {:and, [r_b_var(), r_b_literal(val: false) = res]} ->
            res

          {:or, [r_b_literal(val: true) = res, r_b_var()]} ->
            res

          {:or, [r_b_literal(val: false), r_b_var() = res]} ->
            res

          {:or, [r_b_var(), r_b_literal(val: true) = res]} ->
            res

          {:or, [r_b_var() = res, r_b_literal(val: false)]} ->
            res

          _ ->
            :none
        end

      false ->
        :none
    end
  end

  defp all_same([{h, _} | t]) do
    all(
      fn {e, _} ->
        e === h
      end,
      t
    )
  end

  defp pre_is_phi_bool([{r_b_literal(val: lit), _} | as], sub) do
    is_boolean(lit) and pre_is_phi_bool(as, sub)
  end

  defp pre_is_phi_bool([{r_b_var() = a, _} | as], sub) do
    case sub do
      %{^a => r_b_set()} ->
        pre_is_phi_bool(as, sub)

      %{} ->
        false
    end
  end

  defp pre_is_phi_bool([], _Sub) do
    true
  end

  defp pre_is_safe_bool(r_b_literal(), _Sub) do
    true
  end

  defp pre_is_safe_bool(var, sub) do
    case sub do
      %{^var => r_b_set(op: {:bif, :is_function}, args: [_, arity])} ->
        case arity do
          r_b_literal(val: lit) ->
            is_integer(lit) and lit >= 0

          r_b_var() ->
            false
        end

      %{^var => r_b_set(op: {:bif, op}, args: args)} ->
        arity = length(args)

        :erl_internal.bool_op(
          op,
          arity
        ) and pre_are_args_bool(args, sub)

      %{} ->
        false
    end
  end

  defp pre_are_args_bool([a | as], sub) do
    pre_is_arg_bool(a, sub) and pre_are_args_bool(as, sub)
  end

  defp pre_are_args_bool([], _Sub) do
    true
  end

  defp pre_is_arg_bool(r_b_literal(val: lit), _Sub) do
    is_boolean(lit)
  end

  defp pre_is_arg_bool(r_b_var() = a, sub) do
    case sub do
      %{^a => r_b_set()} ->
        true

      %{} ->
        false
    end
  end

  defp interesting_defs(blocks) do
    interesting_defs(:maps.to_list(blocks), [])
  end

  defp interesting_defs([{l, r_b_blk(is: is)} | bs], acc) do
    interesting_defs(bs, interesting_defs_is(is, l, acc))
  end

  defp interesting_defs([], acc) do
    :maps.from_list(acc)
  end

  defp interesting_defs_is([r_b_set(op: {:bif, _}, dst: v) = i | is], l, acc) do
    case is_bool_expr(i) do
      true ->
        interesting_defs_is(is, l, [{v, {l, i}} | acc])

      false ->
        interesting_defs_is(is, l, acc)
    end
  end

  defp interesting_defs_is([r_b_set(op: :phi, dst: v) = set | is], l, acc) do
    interesting_defs_is(is, l, [{v, {l, set}} | acc])
  end

  defp interesting_defs_is([r_b_set() | is], l, acc) do
    interesting_defs_is(is, l, acc)
  end

  defp interesting_defs_is([], _L, acc) do
    acc
  end

  defp bool_opt(blocks, st) do
    bool_opt(:beam_ssa.rpo(blocks), blocks, st)
  end

  defp bool_opt([l | ls], blocks0, st0) do
    {blocks, st1} = bool_opt(ls, blocks0, st0)

    case blocks do
      %{^l => r_b_blk(is: [_ | _] = is, last: r_b_br(bool: r_b_var() = bool) = br)} ->
        case last(is) do
          r_b_set(op: {:bif, :"=:="}, dst: ^bool, args: [r_b_var(), r_b_literal(val: true)]) ->
            try do
              bool_opt_rewrite(bool, l, br, blocks, st1)
            catch
              :not_possible ->
                {blocks, st1}
            end

          r_b_set() ->
            {blocks, st1}
        end

      %{} ->
        {blocks, st1}
    end
  end

  defp bool_opt([], blocks, st) do
    {blocks, st}
  end

  defp bool_opt_rewrite(bool, from, br, blocks0, st0) do
    treeVars = collect_bool_vars(bool, st0)

    case treeVars do
      [^bool] ->
        not_possible()

      [_ | _] ->
        :ok
    end

    dom = bool_opt_dom(treeVars, st0)
    {domPreIs, blocks1} = split_dom_block(dom, blocks0)
    bs = collect_digraph_blocks(dom, from, br, blocks1)
    {root, g0, st1} = build_digraph(bs, br, st0)
    lDefs = digraph_bool_def(g0)
    st = r_st(st1, ldefs: lDefs)
    g1 = opt_digraph_top(bool, g0, st)
    g = shortcut_branches(root, g1, st)
    ensure_init(root, g, g0)
    domBlk0 = :erlang.map_get(dom, blocks1)

    blocks2 =
      :maps.without(
        for {l, r_b_blk()} <- bs do
          l
        end,
        blocks1
      )

    blocks3 = digraph_to_ssa([root], g, blocks2)

    domBlk =
      r_b_blk(domBlk0,
        is: domPreIs,
        last: oneway_br(root)
      )

    blocks = %{blocks3 | dom => domBlk}
    {blocks, r_st(st, ldefs: %{})}
  end

  defp collect_bool_vars(rootBool, st) do
    r_b_set(args: [r_b_var() = var, r_b_literal()]) = get_def(rootBool, st)
    collect_bool_vars([var], st, [rootBool])
  end

  defp collect_bool_vars([v | vs], st, acc) do
    case get_def(v, st) do
      r_b_set(op: :phi, anno: anno, args: args) ->
        {vars, ls} = collect_phi_args(args, anno)
        collect_bool_vars(vars ++ vs, st, ls ++ vars ++ acc)

      r_b_set(args: args) = i ->
        vars =
          for r_b_var() = arg <- args do
            arg
          end

        case is_rewritable_bool_op(i) do
          true ->
            collect_bool_vars(vars ++ vs, st, [v | acc])

          false ->
            collect_bool_vars(vs, st, [v | acc])
        end

      :none ->
        collect_bool_vars(vs, st, acc)
    end
  end

  defp collect_bool_vars([], _St, acc) do
    :ordsets.from_list(acc)
  end

  defp is_rewritable_bool_op(r_b_set(op: {:bif, bif})) do
    case bif do
      :and ->
        true

      :or ->
        true

      :not ->
        true

      _ ->
        false
    end
  end

  defp collect_phi_args(args, anno) do
    case :erlang.is_map_key(:boolean_phi, anno) do
      true ->
        vars =
          for {r_b_var() = v, _} <- args do
            v
          end

        case vars do
          [_ | _] ->
            {vars, []}

          [] ->
            ls =
              for {_, l} <- args do
                {:block, l}
              end

            {[], ls}
        end

      false ->
        {[], []}
    end
  end

  defp bool_opt_dom(treeVars, r_st(defs: defs, dom: {domBy, num})) do
    ls0 =
      foldl(
        fn
          {:block, l}, a ->
            [l | a]

          v, a ->
            {l, _} = :erlang.map_get(v, defs)
            [l | a]
        end,
        [],
        treeVars
      )

    ls = :ordsets.from_list(ls0)
    [common | _] = :beam_ssa.common_dominators(ls, domBy, num)
    common
  end

  defp split_dom_block(l, blocks0) do
    r_b_blk(is: is) = blk0 = :erlang.map_get(l, blocks0)
    {preIs, tailIs} = split_dom_block_is(is, [])
    blk = r_b_blk(blk0, is: tailIs)
    blocks = %{blocks0 | l => blk}
    {preIs, blocks}
  end

  defp split_dom_block_is([r_b_set(), r_b_set(op: {:succeeded, _})] = is, preAcc) do
    {reverse(preAcc), is}
  end

  defp split_dom_block_is([r_b_set() = i | is] = is0, preAcc) do
    case is_bool_expr(i) do
      true ->
        {reverse(preAcc), is0}

      false ->
        split_dom_block_is(is, [i | preAcc])
    end
  end

  defp split_dom_block_is([], preAcc) do
    {reverse(preAcc), []}
  end

  defp collect_digraph_blocks(firstL, lastL, r_b_br(succ: succ, fail: fail), blocks) do
    ws = :gb_sets.singleton(firstL)
    seen = :cerl_sets.from_list([succ, fail])
    collect_digraph_blocks(ws, lastL, blocks, seen, [])
  end

  defp collect_digraph_blocks(ws0, lastL, blocks, seen0, acc0) do
    case :gb_sets.is_empty(ws0) do
      true ->
        acc0

      false ->
        {l, ws1} = :gb_sets.take_smallest(ws0)
        seen = :cerl_sets.add_element(l, seen0)
        blk = :erlang.map_get(l, blocks)
        acc = [{l, blk} | acc0]
        ws = cdb_update_workset(l, blk, lastL, seen, ws1)
        collect_digraph_blocks(ws, lastL, blocks, seen, acc)
    end
  end

  defp cdb_update_workset(lastL, _Blk, lastL, _Seen, ws) do
    ws
  end

  defp cdb_update_workset(_L, blk, _LastL, seen, ws) do
    successors = :beam_ssa.successors(blk)
    cdb_update_workset(successors, seen, ws)
  end

  defp cdb_update_workset([l | ls], seen, ws) do
    case :cerl_sets.is_element(l, seen) do
      true ->
        cdb_update_workset(ls, seen, ws)

      false ->
        cdb_update_workset(ls, seen, :gb_sets.add_element(l, ws))
    end
  end

  defp cdb_update_workset([], _Seen, ws) do
    ws
  end

  defp build_digraph(bs, r_b_br(succ: succ, fail: fail), st0) do
    ignore = :ordsets.from_list([succ, fail])
    g0 = :beam_digraph.new()
    {map0, g1, st1} = build_mapping(bs, %{}, g0, st0)
    {map, g2} = add_external_vertices(ignore, map0, g1)
    {g, st} = build_digraph_1(bs, g2, map, st1)
    [root] = digraph_roots(g)
    {root, g, st}
  end

  defp build_mapping([{l, blk} | bs], map0, g0, st0) do
    {vtx, st} = new_label(st0)
    map = %{map0 | l => vtx}

    label =
      case blk do
        r_b_blk(is: []) ->
          :br

        r_b_blk() ->
          :initial
      end

    g = :beam_digraph.add_vertex(g0, vtx, label)
    build_mapping(bs, map, g, st)
  end

  defp build_mapping([], map, g, st) do
    {map, g, st}
  end

  defp add_external_vertices([v | vs], map0, g0) do
    g = :beam_digraph.add_vertex(g0, v, {:external, %{}})
    map = %{map0 | v => v}
    add_external_vertices(vs, map, g)
  end

  defp add_external_vertices([], map, g) do
    {map, g}
  end

  defp build_digraph_1([{l, blk} | bs], g0, map, st0) do
    r_b_blk(is: is, last: last) = blk
    vtx = :erlang.map_get(l, map)
    {g, st} = build_digraph_is(is, last, vtx, map, g0, st0)
    build_digraph_1(bs, g, map, st)
  end

  defp build_digraph_1([], g, _Map, st) do
    {g, st}
  end

  defp build_digraph_is([r_b_set(op: :phi, args: args0) = i0 | is], last, vtx, map, g, st) do
    case is do
      [r_b_set(op: :phi) | _] ->
        not_possible()

      _ ->
        :ok
    end

    args =
      for {v, l} <- args0 do
        {v,
         case map do
           %{^l => other} ->
             other

           %{} ->
             not_possible()
         end}
      end

    i = r_b_set(i0, args: args)
    build_digraph_is_1(i, is, last, vtx, map, g, st)
  end

  defp build_digraph_is([r_b_set() = i | is], last, vtx, map, g, st) do
    case :beam_ssa.no_side_effect(i) do
      true ->
        build_digraph_is_1(i, is, last, vtx, map, g, st)

      false ->
        not_possible()
    end
  end

  defp build_digraph_is([], last, from, map, g0, st) do
    case last do
      r_b_br(bool: r_b_literal(val: true), succ: to0, fail: to0) ->
        to = :erlang.map_get(to0, map)
        g = :beam_digraph.add_edge(g0, from, to, :next)
        {g, st}

      r_b_br(bool: r_b_var() = bool, succ: succ0, fail: fail0) ->
        %{^succ0 => succ, ^fail0 => fail} = map

        case :beam_digraph.vertex(g0, from) do
          r_b_set(dst: ^bool) ->
            g = add_succ_fail_edges(from, succ, fail, g0)
            {g, st}

          r_b_set() ->
            not_possible()

          :br ->
            g1 = add_succ_fail_edges(from, succ, fail, g0)
            g = :beam_digraph.add_vertex(g1, from, {:br, bool})
            {g, st}
        end

      _ ->
        not_possible()
    end
  end

  defp build_digraph_is_1(i, is, last, vtx, map, g0, st0) do
    g1 = :beam_digraph.add_vertex(g0, vtx, i)

    case is do
      [] ->
        build_digraph_is(is, last, vtx, map, g1, st0)

      [_ | _] ->
        {nextVtx, st} = new_label(st0)
        g2 = :beam_digraph.add_vertex(g1, nextVtx, :initial)
        g = :beam_digraph.add_edge(g2, vtx, nextVtx, :next)
        build_digraph_is(is, last, nextVtx, map, g, st)
    end
  end

  defp opt_digraph_top(arg, g0, st) do
    i = get_def(arg, g0, st)
    r_b_set(op: {:bif, :"=:="}, dst: dst, args: [r_b_var() = bool, r_b_literal(val: true)]) = i
    {:br, succ, fail} = get_targets(dst, g0, st)
    g1 = ensure_single_use(dst, g0, st)
    g = convert_to_br_node(i, succ, g1, st)
    redirect_test(bool, {:fail, fail}, g, st)
  end

  defp do_opt_digraph([a | as], g0, st) do
    i = get_def(a, g0, st)

    try do
      opt_digraph_instr(i, g0, st)
    catch
      :not_possible ->
        do_opt_digraph(as, g0, st)
    else
      g ->
        do_opt_digraph(as, g, st)
    end
  end

  defp do_opt_digraph([], g, _St) do
    g
  end

  defp opt_digraph_instr(r_b_set(dst: dst) = i, g0, st) do
    {:br, succ, fail} = get_targets(dst, g0, st)
    g1 = ensure_single_use(dst, g0, st)

    case i do
      r_b_set(op: {:bif, :and}, args: args) ->
        g2 = convert_to_br_node(i, succ, g1, st)
        {first, second} = order_args(args, g2, st)
        g = redirect_test(first, {:fail, fail}, g2, st)
        redirect_test(second, {:fail, fail}, g, st)

      r_b_set(op: {:bif, :or}, args: args) ->
        {first, second} = order_args(args, g1, st)
        ensure_no_failing_instructions(first, second, g1, st)
        g2 = convert_to_br_node(i, succ, g1, st)
        g = redirect_test(first, {:succ, succ}, g2, st)
        redirect_test(second, {:fail, fail}, g, st)

      r_b_set(op: {:bif, :xor}) ->
        not_possible()

      r_b_set(op: {:bif, :not}) ->
        not_possible()

      r_b_set(op: :phi, dst: bool) ->
        vtx = get_vertex(bool, st)
        g2 = del_out_edges(vtx, g1)
        g = :beam_digraph.add_edge(g2, vtx, succ, :next)
        redirect_test(bool, {:fail, fail}, g, st)

      r_b_set() ->
        g1
    end
  end

  defp ensure_single_use(bool, g, r_st(uses: u) = st) do
    case :erlang.map_get(bool, u) do
      [_] ->
        g

      uses ->
        vtx = get_vertex(bool, st)
        ensure_single_use_1(bool, vtx, uses, g)
    end
  end

  defp ensure_single_use_1(bool, vtx, uses, g) do
    fail =
      case get_targets(vtx, g) do
        {:br, _, fail0} ->
          fail0

        _ ->
          not_possible()
      end

    case partition(
           fn
             {l, r_b_set()} when l === fail ->
               true

             _ ->
               false
           end,
           uses
         ) do
      {[_], [_]} ->
        case {:beam_digraph.vertex(g, fail), :beam_digraph.in_edges(g, fail)} do
          {{:external, bs0}, [_]} ->
            bs = %{bs0 | bool => r_b_literal(val: false)}
            :beam_digraph.add_vertex(g, fail, {:external, bs})

          _ ->
            not_possible()
        end

      {_, _} ->
        not_possible()
    end
  end

  defp convert_to_br_node(i, target, g0, st) do
    vtx = get_vertex(i, st)
    g1 = del_out_edges(vtx, g0)
    g = :beam_digraph.add_vertex(g1, vtx, :br)
    :beam_digraph.add_edge(g, vtx, target, :next)
  end

  defp ensure_no_failing_instructions(first, second, g, st) do
    vs0 = covered(get_vertex(first, st), get_vertex(second, st), g)

    vs =
      for v <- vs0 do
        {v, :beam_digraph.vertex(g, v)}
      end

    failing =
      for {v, r_b_set(op: {:succeeded, _})} = p <- vs,
          not eaten_by_phi(v, g) do
        p
      end

    case failing do
      [] ->
        :ok

      [_ | _] ->
        not_possible()
    end
  end

  defp eaten_by_phi(v, g) do
    {:br, _, fail} = get_targets(v, g)

    case :beam_digraph.vertex(g, fail) do
      :br ->
        [to] = :beam_digraph.out_neighbours(g, fail)

        case :beam_digraph.vertex(g, to) do
          r_b_set(op: :phi) ->
            true

          _ ->
            false
        end

      _ ->
        false
    end
  end

  defp order_args([r_b_var() = varA, r_b_var() = varB], g, st) do
    {vA, vB} = {get_vertex(varA, st), get_vertex(varB, st)}

    case :beam_digraph.is_path(g, vA, vB) do
      true ->
        {varA, varB}

      false ->
        true = :beam_digraph.is_path(g, vB, vA)
        {varB, varB}
    end
  end

  defp order_args(_Args, _G, _St) do
    not_possible()
  end

  defp redirect_test(bool, succFail, g0, st) do
    v = get_vertex(bool, st)
    i = get_def(bool, g0, st)

    case i do
      r_b_set(op: :phi, args: args) ->
        g = ensure_single_use(bool, g0, st)
        redirect_phi(bool, args, succFail, g, st)

      r_b_set() ->
        g1 = redirect_test_1(v, succFail, g0)
        g = ensure_single_use(bool, g1, st)
        do_opt_digraph([bool], g, st)
    end
  end

  defp redirect_test_1(v, succFail, g) do
    case get_targets(v, g) do
      {:br, _Succ, fail} ->
        case succFail do
          {:fail, ^fail} ->
            g

          {:succ, _} ->
            not_possible()
        end

      {:br, next} ->
        case succFail do
          {:succ, succ} ->
            add_succ_fail_edges(v, succ, next, g)

          {:fail, fail} ->
            add_succ_fail_edges(v, next, fail, g)
        end
    end
  end

  defp redirect_phi(phi, args, succFail, g0, st) do
    phiVtx = get_vertex(phi, st)
    g = :beam_digraph.add_vertex(g0, phiVtx, :br)
    redirect_phi_1(phiVtx, sort(args), succFail, g, st)
  end

  defp redirect_phi_1(
         phiVtx,
         [{r_b_literal(val: false), falseExit}, {r_b_var() = succBool, _BoolExit}],
         succFail,
         g0,
         st
       ) do
    boolVtx = get_vertex(succBool, st)
    ensure_disjoint_paths(g0, boolVtx, falseExit)
    [falseOut] = :beam_digraph.out_edges(g0, falseExit)
    g1 = :beam_digraph.del_edge(g0, falseOut)

    case succFail do
      {:fail, fail} ->
        g2 = :beam_digraph.add_edge(g1, falseExit, fail, :next)
        g = add_succ_fail_edges(boolVtx, phiVtx, falseExit, g2)
        do_opt_digraph([succBool], g, st)

      {:succ, succ} ->
        g2 = :beam_digraph.add_edge(g1, falseExit, phiVtx, :next)
        g = add_succ_fail_edges(boolVtx, succ, phiVtx, g2)
        do_opt_digraph([succBool], g, st)
    end
  end

  defp redirect_phi_1(
         phiVtx,
         [{r_b_literal(val: true), trueExit}, {r_b_var() = succBool, _BoolExit}],
         {:fail, fail},
         g0,
         st
       ) do
    boolVtx = get_vertex(succBool, st)
    ensure_disjoint_paths(g0, boolVtx, trueExit)
    [trueOut] = :beam_digraph.out_edges(g0, trueExit)
    g1 = :beam_digraph.del_edge(g0, trueOut)
    g2 = :beam_digraph.add_edge(g1, trueExit, phiVtx, :next)
    g = add_succ_fail_edges(boolVtx, phiVtx, fail, g2)
    do_opt_digraph([succBool], g, st)
  end

  defp redirect_phi_1(
         _PhiVtx,
         [{r_b_literal(val: false), falseExit}, {r_b_literal(val: true), trueExit}],
         succFail,
         g0,
         _St
       ) do
    case succFail do
      {:fail, fail} ->
        [falseOut] = :beam_digraph.out_edges(g0, falseExit)
        g = :beam_digraph.del_edge(g0, falseOut)
        :beam_digraph.add_edge(g, falseExit, fail, :next)

      {:succ, succ} ->
        [trueOut] = :beam_digraph.out_edges(g0, trueExit)
        g = :beam_digraph.del_edge(g0, trueOut)
        :beam_digraph.add_edge(g, trueExit, succ, :next)
    end
  end

  defp redirect_phi_1(_PhiVtx, _Args, _SuccFail, _G, _St) do
    not_possible()
  end

  defp digraph_bool_def(g) do
    vs = :beam_digraph.vertices(g)

    ds =
      for {vtx, r_b_set(dst: dst)} <- vs do
        {dst, vtx}
      end

    :maps.from_list(ds)
  end

  defp ensure_disjoint_paths(g, v1, v2) do
    case :beam_digraph.is_path(g, v1, v2) or :beam_digraph.is_path(g, v2, v1) do
      true ->
        not_possible()

      false ->
        :ok
    end
  end

  defp shortcut_branches(vtx, g, st) do
    vs = reverse(:beam_digraph.reverse_postorder(g, [vtx]))
    do_shortcut_branches(vs, g, st)
  end

  defp do_shortcut_branches([v | vs], g0, st) do
    case get_targets(v, g0) do
      {:br, succ0, fail0} ->
        {succBs, failBs} = eval_bs(v, g0, st)
        succ = eval_instr(succ0, g0, succBs)
        g1 = redirect_edge(v, succ0, {:succ, succ}, g0)
        fail = eval_instr(fail0, g1, failBs)
        g = redirect_edge(v, fail0, {:fail, fail}, g1)
        do_shortcut_branches(vs, g, st)

      {:br, next0} ->
        next = eval_instr(next0, g0, %{})
        g = redirect_edge(v, next0, {:next, next}, g0)
        do_shortcut_branches(vs, g, st)

      :none ->
        do_shortcut_branches(vs, g0, st)
    end
  end

  defp do_shortcut_branches([], g, _St) do
    g
  end

  defp redirect_edge(_From, to, {_Label, to}, g) do
    g
  end

  defp redirect_edge(from, to0, {label, to}, g0) do
    g = :beam_digraph.del_edge(g0, {from, to0, label})
    :beam_digraph.add_edge(g, from, to, label)
  end

  defp eval_bs(vtx, g, st) do
    case :beam_digraph.vertex(g, vtx) do
      r_b_set(op: {:bif, :"=:="}, args: [r_b_var() = bool, r_b_literal(val: true)]) ->
        case get_def(bool, g, st) do
          r_b_set(op: :phi) = phi ->
            phi_bs(phi)

          _ ->
            {%{}, %{}}
        end

      _ ->
        {%{}, %{}}
    end
  end

  defp phi_bs(r_b_set(op: :phi, dst: phiDst, args: phiArgs)) do
    literals0 =
      for {r_b_literal() = lit, _} <- phiArgs do
        lit
      end

    case length(literals0) === length(phiArgs) do
      true ->
        literals = :ordsets.from_list(literals0)

        case partition(
               fn r_b_literal(val: val) ->
                 val === true
               end,
               literals
             ) do
          {[true__], [failVal]} ->
            succBs = %{phiDst => true__}
            failBs = %{phiDst => failVal}
            {succBs, failBs}

          {_, _} ->
            {%{}, %{}}
        end

      false ->
        {%{}, %{}}
    end
  end

  defp eval_instr(vtx, g, bs) do
    case :beam_digraph.vertex(g, vtx) do
      r_b_set() when map_size(bs) === 0 ->
        vtx

      r_b_set() = i ->
        case is_safe_bool_expr(i) do
          true ->
            eval_safe_bool_expr(i, vtx, g, bs)

          false ->
            vtx
        end

      :br ->
        [next] = :beam_digraph.out_neighbours(g, vtx)

        case :beam_digraph.vertex(g, next) do
          r_b_set(op: :phi) ->
            vtx

          _ ->
            eval_instr(next, g, bs)
        end

      {:br, r_b_var()} ->
        vtx

      {:external, _} ->
        vtx
    end
  end

  defp eval_safe_bool_expr(r_b_set(op: {:bif, bif}, dst: dst, args: args0), vtx, g, bs) do
    case get_targets(vtx, g) do
      {:br, succ, fail} ->
        true__ = r_b_literal(val: true)
        false__ = r_b_literal(val: false)
        args = sub_args(args0, bs)

        case eval_bif(bif, args) do
          :none ->
            case {eval_instr(succ, g, %{bs | dst => true__}),
                  eval_instr(fail, g, %{bs | dst => false__})} do
              {same, same} ->
                same

              {_, _} ->
                vtx
            end

          true ->
            eval_instr(succ, g, %{bs | dst => true__})

          false ->
            eval_instr(fail, g, %{bs | dst => false__})
        end

      {:br, _} ->
        vtx
    end
  end

  defp eval_bif(bif, args0) do
    case eval_literal_args(args0, []) do
      :none ->
        :none

      args ->
        apply(:erlang, bif, args)
    end
  end

  defp eval_literal_args([r_b_literal(val: val) | as], acc) do
    eval_literal_args(as, [val | acc])
  end

  defp eval_literal_args([_ | _], _) do
    :none
  end

  defp eval_literal_args([], acc) do
    reverse(acc)
  end

  defp ensure_init(root, g, g0) do
    vs = :beam_digraph.vertices(g)
    used = ensure_init_used(g0)

    vars =
      :maps.from_list(
        for {_, r_b_set(dst: dst)} <- vs do
          {dst, :unset}
        end
      )

    rPO = :beam_digraph.reverse_postorder(g, [root])
    ensure_init_1(rPO, used, g, %{root => vars})
  end

  defp ensure_init_1([v | vs], used, g, initMaps0) do
    initMaps = ensure_init_instr(v, used, g, initMaps0)
    ensure_init_1(vs, used, g, initMaps)
  end

  defp ensure_init_1([], _, _, _) do
    :ok
  end

  defp ensure_init_instr(vtx, used, g, initMaps0) do
    varMap0 = :erlang.map_get(vtx, initMaps0)

    case :beam_digraph.vertex(g, vtx) do
      r_b_set(dst: dst) = i ->
        do_ensure_init_instr(i, varMap0, initMaps0)
        outVs = :beam_digraph.out_neighbours(g, vtx)
        varMap = %{varMap0 | dst => :set}
        initMaps = %{initMaps0 | vtx => varMap}
        ensure_init_successors(outVs, g, varMap, initMaps)

      {:external, _} ->
        case (for {v, :unset} <- :maps.to_list(varMap0) do
                v
              end) do
          [] ->
            initMaps0

          [_ | _] = unset0 ->
            unset = :ordsets.from_list(unset0)

            case :ordsets.is_subset(unset, used) do
              true ->
                initMaps0

              false ->
                not_possible()
            end
        end

      _ ->
        outVs = :beam_digraph.out_neighbours(g, vtx)
        ensure_init_successors(outVs, g, varMap0, initMaps0)
    end
  end

  defp ensure_init_used(g) do
    vs = :beam_digraph.vertices(g)
    ensure_init_used_1(vs, g, [])
  end

  defp ensure_init_used_1([{vtx, r_b_set(dst: dst) = i} | vs], g, acc0) do
    acc1 = [:beam_ssa.used(i) | acc0]

    case :beam_digraph.out_degree(g, vtx) do
      2 ->
        acc = [[dst] | acc1]
        ensure_init_used_1(vs, g, acc)

      _ ->
        ensure_init_used_1(vs, g, acc1)
    end
  end

  defp ensure_init_used_1([{_Vtx, {:br, bool}} | vs], g, acc) do
    ensure_init_used_1(vs, g, [[bool] | acc])
  end

  defp ensure_init_used_1([_ | vs], g, acc) do
    ensure_init_used_1(vs, g, acc)
  end

  defp ensure_init_used_1([], _G, acc) do
    :ordsets.union(acc)
  end

  defp do_ensure_init_instr(r_b_set(op: :phi, args: args), _VarMap, initMaps) do
    _ =
      for {r_b_var() = var, from} <- args do
        ensure_init_used(var, :erlang.map_get(from, initMaps))
      end

    :ok
  end

  defp do_ensure_init_instr(r_b_set() = i, varMap, _InitMaps) do
    used = :beam_ssa.used(i)

    _ =
      for var <- used do
        ensure_init_used(var, varMap)
      end

    :ok
  end

  defp ensure_init_used(var, varMap) do
    case varMap do
      %{^var => :unset} ->
        not_possible()

      %{^var => :set} ->
        :ok

      %{} ->
        :ok
    end
  end

  defp ensure_init_successors([to | vs], g, vars0, initMaps0) do
    case initMaps0 do
      %{^to => vars1} ->
        vars = join_inits(vars0, vars1)
        initMaps = %{initMaps0 | to => vars}
        ensure_init_successors(vs, g, vars0, initMaps)

      %{} ->
        initMaps = %{initMaps0 | to => vars0}
        ensure_init_successors(vs, g, vars0, initMaps)
    end
  end

  defp ensure_init_successors([], _, _, initMaps) do
    initMaps
  end

  defp join_inits(varMap0, varMap1) do
    join_inits_1(:maps.to_list(varMap0), varMap1)
  end

  defp join_inits_1([{v, state0} | vs], varMap) do
    state1 = :erlang.map_get(v, varMap)

    state =
      case {state0, state1} do
        {:set, :set} ->
          :set

        {_, _} ->
          :unset
      end

    case state === state1 do
      true ->
        join_inits_1(vs, varMap)

      false ->
        join_inits_1(vs, %{varMap | v => state})
    end
  end

  defp join_inits_1([], varMap) do
    varMap
  end

  defp digraph_to_ssa(ls, g, blocks0) do
    seen = :cerl_sets.new()
    {blocks, _} = digraph_to_ssa(ls, g, blocks0, seen)
    blocks
  end

  defp digraph_to_ssa([l | ls], g, blocks0, seen0) do
    seen1 = :cerl_sets.add_element(l, seen0)
    {blk, successors0} = digraph_to_ssa_blk(l, g, blocks0, [])
    blocks1 = %{blocks0 | l => blk}

    successors =
      for s <- successors0,
          not :cerl_sets.is_element(s, seen1) do
        s
      end

    {blocks, seen} = digraph_to_ssa(successors, g, blocks1, seen1)
    digraph_to_ssa(ls, g, blocks, seen)
  end

  defp digraph_to_ssa([], _G, blocks, seen) do
    {blocks, seen}
  end

  defp digraph_to_ssa_blk(from, g, blocks, acc0) do
    case :beam_digraph.vertex(g, from) do
      r_b_set(dst: dst) = i ->
        case get_targets(from, g) do
          {:br, succ, fail} ->
            br = :beam_ssa.normalize(r_b_br(bool: dst, succ: succ, fail: fail))
            is = reverse(acc0, [i])
            blk = r_b_blk(is: is, last: br)
            {blk, :beam_ssa.successors(blk)}

          {:br, next} ->
            br = oneway_br(next)
            is = reverse(acc0, [i])
            blk = r_b_blk(is: is, last: br)
            {blk, :beam_ssa.successors(blk)}
        end

      :br ->
        {:br, next} = get_targets(from, g)
        blk = r_b_blk(is: [], last: oneway_br(next))
        {blk, :beam_ssa.successors(blk)}

      {:br, bool} ->
        [] = acc0
        {:br, succ, fail} = get_targets(from, g)
        br = :beam_ssa.normalize(r_b_br(bool: bool, succ: succ, fail: fail))
        blk = r_b_blk(is: [], last: br)
        {blk, :beam_ssa.successors(blk)}

      {:external, sub} ->
        r_b_blk(is: is0) = blk = :erlang.map_get(from, blocks)

        is =
          for r_b_set(args: args0) = i <- is0 do
            r_b_set(i, args: sub_args(args0, sub))
          end

        {r_b_blk(blk, is: is), []}
    end
  end

  defp get_def(r_b_var() = bool, r_st(defs: defs)) do
    case defs do
      %{^bool => {_, def__}} ->
        def__

      %{} ->
        :none
    end
  end

  defp get_def(var, g, r_st(ldefs: lDefs, defs: defs)) do
    case lDefs do
      %{^var => vtx} ->
        :beam_digraph.vertex(g, vtx)

      %{} ->
        case defs do
          %{^var => {_, def__}} ->
            def__

          %{} ->
            :none
        end
    end
  end

  defp add_succ_fail_edges(from, succ, fail, g0) do
    g1 = :beam_digraph.add_edge(g0, from, succ, :succ)
    g = :beam_digraph.add_edge(g1, from, fail, :fail)

    case :beam_digraph.out_edges(g0, from) do
      [{^from, _, :next} = e] ->
        :beam_digraph.del_edge(g, e)

      [] ->
        g
    end
  end

  defp get_vertex(r_b_set(dst: dst), st) do
    get_vertex(dst, st)
  end

  defp get_vertex(r_b_var() = var, r_st(ldefs: lDefs)) do
    :erlang.map_get(var, lDefs)
  end

  defp get_targets(vtx, g) when is_integer(vtx) do
    case :beam_digraph.out_edges(g, vtx) do
      [{_, to, :next}] ->
        {:br, to}

      [{_, succ, :succ}, {_, fail, :fail}] ->
        {:br, succ, fail}

      [{_, fail, :fail}, {_, succ, :succ}] ->
        {:br, succ, fail}

      [] ->
        :none
    end
  end

  defp get_targets(r_b_var() = var, g, r_st(ldefs: lDefs)) do
    get_targets(:erlang.map_get(var, lDefs), g)
  end

  defp del_out_edges(v, g) do
    :beam_digraph.del_edges(
      g,
      :beam_digraph.out_edges(g, v)
    )
  end

  defp covered(from, to, g) do
    seen0 = :cerl_sets.new()
    {:yes, seen} = covered_1(from, to, g, seen0)
    :cerl_sets.to_list(seen)
  end

  defp covered_1(to, to, _G, seen) do
    {:yes, seen}
  end

  defp covered_1(from, to, g, seen0) do
    vs0 = :beam_digraph.out_neighbours(g, from)

    vs =
      for v <- vs0,
          not :cerl_sets.is_element(v, seen0) do
        v
      end

    seen = :cerl_sets.union(:cerl_sets.from_list(vs), seen0)

    case vs do
      [] ->
        :no

      [_ | _] ->
        covered_list(vs, to, g, seen, false)
    end
  end

  defp covered_list([v | vs], to, g, seen0, anyFound) do
    case covered_1(v, to, g, seen0) do
      {:yes, seen} ->
        covered_list(vs, to, g, seen, true)

      :no ->
        covered_list(vs, to, g, seen0, anyFound)
    end
  end

  defp covered_list([], _, _, seen, anyFound) do
    case anyFound do
      true ->
        {:yes, seen}

      false ->
        :no
    end
  end

  defp digraph_roots(g) do
    digraph_roots_1(:beam_digraph.vertices(g), g)
  end

  defp digraph_roots_1([{v, _} | vs], g) do
    case :beam_digraph.in_degree(g, v) do
      0 ->
        [v | digraph_roots_1(vs, g)]

      _ ->
        digraph_roots_1(vs, g)
    end
  end

  defp digraph_roots_1([], _G) do
    []
  end

  defp not_possible() do
    throw(:not_possible)
  end

  defp new_label(r_st(count: count) = st) do
    {count, r_st(st, count: count + 1)}
  end

  defp sub_args(args, sub) do
    for arg <- args do
      sub_arg(arg, sub)
    end
  end

  defp sub_arg({r_b_var() = arg, from}, sub) when is_integer(from) do
    {do_sub_arg(arg, sub), from}
  end

  defp sub_arg(r_b_var() = arg, sub) do
    do_sub_arg(arg, sub)
  end

  defp sub_arg(r_b_remote(mod: mod, name: name) = rem, sub) do
    r_b_remote(rem,
      mod: do_sub_arg(mod, sub),
      name: do_sub_arg(name, sub)
    )
  end

  defp sub_arg(arg, _Sub) do
    arg
  end

  defp do_sub_arg(r_b_var() = old, sub) do
    case sub do
      %{^old => r_b_literal() = new} ->
        new

      %{^old => r_b_var() = new} ->
        new

      %{} ->
        old
    end
  end

  defp do_sub_arg(r_b_literal() = old, _Sub) do
    old
  end

  defp is_bool_expr(r_b_set(op: {:bif, op}, args: args)) do
    arity = length(args)

    :erl_internal.comp_op(
      op,
      arity
    ) or
      :erl_internal.new_type_test(
        op,
        arity
      ) or
      :erl_internal.bool_op(
        op,
        arity
      )
  end

  defp is_bool_expr(_) do
    false
  end

  defp is_safe_bool_expr(r_b_set(op: {:bif, op}, args: args)) do
    arity = length(args)

    :erl_internal.comp_op(
      op,
      arity
    ) or :erl_internal.new_type_test(op, arity)
  end

  defp is_safe_bool_expr(r_b_set()) do
    false
  end

  defp oneway_br(to) do
    r_b_br(bool: r_b_literal(val: true), succ: to, fail: to)
  end
end
