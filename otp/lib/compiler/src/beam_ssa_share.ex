defmodule :m_beam_ssa_share do
  use Bitwise
  import :lists, only: [keyfind: 3, reverse: 1, sort: 1]
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

  def module(r_b_module(body: fs0) = module, _Opts) do
    fs =
      for f <- fs0 do
        function(f)
      end

    {:ok, r_b_module(module, body: fs)}
  end

  def block(r_b_blk(is: is0, last: last0) = blk, blocks) do
    case share_terminator(last0, blocks) do
      :none ->
        blk

      r_b_br(succ: same, fail: same) = last ->
        case reverse(is0) do
          [
            [r_b_set(op: {:succeeded, kind}, args: [dst]), r_b_set(dst: dst)]
            | is
          ] ->
            :guard = kind

            r_b_blk(blk,
              is: reverse(is),
              last: :beam_ssa.normalize(last)
            )

          _ ->
            r_b_blk(blk, last: :beam_ssa.normalize(last))
        end

      last ->
        r_b_blk(blk, last: :beam_ssa.normalize(last))
    end
  end

  defp function(r_b_function(anno: anno, bs: blocks0) = f) do
    try do
      pO = reverse(:beam_ssa.rpo(blocks0))
      {blocks1, changed} = blocks(pO, blocks0, false)

      blocks =
        case changed do
          true ->
            :beam_ssa.trim_unreachable(blocks1)

          false ->
            blocks0
        end

      r_b_function(f, bs: blocks)
    catch
      class, error ->
        %{:func_info => {_, name, arity}} = anno
        :io.fwrite('Function: ~w/~w\n', [name, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  defp blocks([l | ls], blocks, changed) do
    r_b_blk(last: last0) = blk0 = :erlang.map_get(l, blocks)

    case block(blk0, blocks) do
      r_b_blk(last: ^last0) ->
        blocks(ls, blocks, changed)

      r_b_blk() = blk ->
        blocks(ls, %{blocks | l => blk}, true)
    end
  end

  defp blocks([], blocks, changed) do
    {blocks, changed}
  end

  defp share_terminator(
         r_b_br(bool: r_b_var(), succ: succ0, fail: fail0) = br,
         blocks
       ) do
    {succ, succBlk} = shortcut_nonempty_block(succ0, blocks)
    {fail, failBlk} = shortcut_nonempty_block(fail0, blocks)

    case are_equivalent(succ, succBlk, fail, failBlk, blocks) do
      true ->
        r_b_br(br, succ: succ, fail: succ)

      false ->
        cond do
          succ === succ0 and fail === fail0 ->
            :none

          true ->
            r_b_br(br, succ: succ, fail: fail)
        end
    end
  end

  defp share_terminator(r_b_switch() = sw, blocks) do
    share_switch(sw, blocks)
  end

  defp share_terminator(_Last, _Blocks) do
    :none
  end

  defp are_equivalent(_Succ, _, 1, _, _Blocks) do
    false
  end

  defp are_equivalent(
         _Succ,
         r_b_blk(is: is1, last: r_b_ret(arg: retVal1) = ret1),
         _Fail,
         r_b_blk(is: is2, last: r_b_ret(arg: retVal2) = ret2),
         _Blocks
       ) do
    case {retVal1, retVal2} do
      {r_b_literal(), r_b_literal()} ->
        case retVal1 === retVal2 do
          true ->
            can1 = canonical_is(is1)
            can2 = canonical_is(is2)
            can1 === can2

          false ->
            false
        end

      {r_b_var(), r_b_var()} ->
        can1 = canonical_is(is1 ++ [ret1])
        can2 = canonical_is(is2 ++ [ret2])
        can1 === can2

      {_, _} ->
        false
    end
  end

  defp are_equivalent(
         succ,
         r_b_blk(is: is1, last: r_b_br(bool: r_b_literal(val: true), succ: target)),
         fail,
         r_b_blk(is: is2, last: r_b_br(bool: r_b_literal(val: true), succ: target)),
         blocks
       ) do
    r_b_blk(is: is) = :erlang.map_get(target, blocks)
    phis1 = canonical_terminator_phis(is, succ)
    phis2 = canonical_terminator_phis(is, fail)

    case {phis1, phis2} do
      {[r_b_set(args: [r_b_literal()]) | _], _} when phis1 !== phis2 ->
        false

      {_, [r_b_set(args: [r_b_literal()]) | _]} when phis1 !== phis2 ->
        false

      {_, _} ->
        can1 = canonical_is(is1 ++ phis1)
        can2 = canonical_is(is2 ++ phis2)
        can1 === can2
    end
  end

  defp are_equivalent(
         succ0,
         r_b_blk(is: is1, last: r_b_br(bool: r_b_var(), fail: same)),
         fail0,
         r_b_blk(is: is2, last: r_b_br(bool: r_b_var(), fail: same)),
         blocks
       ) do
    case canonical_is(is1) === canonical_is(is2) do
      false ->
        false

      true ->
        sw =
          r_b_switch(arg: r_b_var(name: :not_used), fail: fail0, list: [{r_b_literal(), succ0}])

        r_b_switch(fail: fail, list: [{_, succ}]) =
          share_switch(
            sw,
            blocks
          )

        fail === succ
    end
  end

  defp are_equivalent(_, _, _, _, _) do
    false
  end

  defp share_switch(r_b_switch(fail: fail0, list: list0) = sw, blocks) do
    prep = share_prepare_sw([{:value, fail0} | list0], blocks, 0, [])
    res = do_share_switch(prep, blocks, [])

    [{_, fail} | list] =
      for {_, vL} <- sort(res) do
        vL
      end

    r_b_switch(sw, fail: fail, list: list)
  end

  defp share_prepare_sw([{v, l0} | t], blocks, n, acc) do
    {l, _Blk} = shortcut_nonempty_block(l0, blocks)
    share_prepare_sw(t, blocks, n + 1, [{{l, %{}}, {n, {v, l}}} | acc])
  end

  defp share_prepare_sw([], _, _, acc) do
    acc
  end

  defp do_share_switch(prep, blocks, acc) do
    map = share_switch_1(prep, blocks, %{})
    share_switch_2(:maps.values(map), blocks, acc)
  end

  defp share_switch_1([{next0, res} | t], blocks, map) do
    {can, next} = canonical_block(next0, blocks)

    case map do
      %{^can => ls} ->
        share_switch_1(t, blocks, %{map | can => [{next, res} | ls]})

      %{} ->
        share_switch_1(t, blocks, %{map | can => [{next, res}]})
    end
  end

  defp share_switch_1([], _Blocks, map) do
    map
  end

  defp share_switch_2([[{_, {n, res}}] | t], blocks, acc) do
    share_switch_2(t, blocks, [{n, res} | acc])
  end

  defp share_switch_2([[{:done, {_, {_, common}}} | _] = eqs | t], blocks, acc0) do
    acc =
      for {:done, {n, {v, _}}} <- eqs do
        {n, {v, common}}
      end ++ acc0

    share_switch_2(t, blocks, acc)
  end

  defp share_switch_2([[{_, _} | _] = prep | t], blocks, acc0) do
    acc = do_share_switch(prep, blocks, acc0)
    share_switch_2(t, blocks, acc)
  end

  defp share_switch_2([], _, acc) do
    acc
  end

  defp canonical_block({1, _VarMap}, _Blocks) do
    {{:none, 1}, :done}
  end

  defp canonical_block({l, varMap0}, blocks) do
    r_b_blk(is: is, last: last0) = :erlang.map_get(l, blocks)

    case canonical_terminator(l, last0, blocks) do
      :none ->
        {{:none, l}, :done}

      {last, :done} ->
        {can, _VarMap} = canonical_is(is ++ last, varMap0, [])
        {can, :done}

      {last, next} ->
        {can, varMap} = canonical_is(is ++ last, varMap0, [])
        {can, {next, varMap}}
    end
  end

  defp canonical_is(is) do
    {can, _} = canonical_is(is, %{}, [])
    can
  end

  defp canonical_is([r_b_set(op: op, dst: dst, args: args0) = i | is], varMap0, acc) do
    args =
      for arg <- args0 do
        canonical_arg(arg, varMap0)
      end

    var = {:var, map_size(varMap0)}
    varMap = %{varMap0 | dst => var}

    lineAnno =
      case op do
        :bs_match ->
          []

        _ ->
          :beam_ssa.get_anno(:location, i, :none)
      end

    canonical_is(is, varMap, {op, lineAnno, var, args, acc})
  end

  defp canonical_is([r_b_ret(arg: arg)], varMap, acc0) do
    acc1 =
      case acc0 do
        {:call, _Anno, var, [r_b_local() | _] = args, prevAcc} ->
          {:call, [], var, args, prevAcc}

        _ ->
          acc0
      end

    {{:ret, canonical_arg(arg, varMap), acc1}, varMap}
  end

  defp canonical_is([r_b_br(bool: r_b_var() = arg, fail: fail)], varMap, acc) do
    {{:br, canonical_arg(arg, varMap), :succ, fail, acc}, varMap}
  end

  defp canonical_is([r_b_br(succ: succ)], varMap, acc) do
    {{:br, succ, acc}, varMap}
  end

  defp canonical_is([], varMap, acc) do
    {acc, varMap}
  end

  defp canonical_terminator(_L, r_b_ret() = ret, _Blocks) do
    {[ret], :done}
  end

  defp canonical_terminator(l, r_b_br(bool: r_b_literal(val: true), succ: succ) = br, blocks) do
    r_b_blk(is: is) = :erlang.map_get(succ, blocks)

    case canonical_terminator_phis(is, l) do
      [] ->
        {[], succ}

      [_ | _] = phis ->
        {phis ++ [br], :done}
    end
  end

  defp canonical_terminator(_L, r_b_br(bool: r_b_var(), succ: succ) = br, _Blocks) do
    {[br], succ}
  end

  defp canonical_terminator(_, _, _) do
    :none
  end

  defp canonical_terminator_phis([r_b_set(op: :phi, args: phiArgs) = phi | is], l) do
    {value, ^l} = keyfind(l, 2, phiArgs)

    [
      r_b_set(phi, op: :copy, args: [value])
      | canonical_terminator_phis(is, l)
    ]
  end

  defp canonical_terminator_phis([r_b_set() = i | _], l) do
    case :beam_ssa.is_loop_header(i) do
      true ->
        [r_b_set(i, op: :copy, args: [r_b_literal(val: l)])]

      false ->
        []
    end
  end

  defp canonical_terminator_phis(_, _) do
    []
  end

  defp canonical_arg(r_b_var() = var, varMap) do
    case varMap do
      %{^var => canonicalVar} ->
        canonicalVar

      %{} ->
        var
    end
  end

  defp canonical_arg(r_b_remote(mod: mod, name: name), varMap) do
    {:remote, canonical_arg(mod, varMap), canonical_arg(name, varMap)}
  end

  defp canonical_arg(other, _VarMap) do
    other
  end

  defp shortcut_nonempty_block(l, blocks) do
    case :erlang.map_get(l, blocks) do
      r_b_blk(
        is: [],
        last: r_b_br(bool: r_b_literal(val: true), succ: succ)
      ) = blk ->
        case is_forbidden(succ, blocks) do
          false ->
            shortcut_nonempty_block(succ, blocks)

          true ->
            {l, blk}
        end

      r_b_blk() = blk ->
        {l, blk}
    end
  end

  defp is_forbidden(l, blocks) do
    case :erlang.map_get(l, blocks) do
      r_b_blk(is: [r_b_set(op: :phi) | _]) ->
        true

      r_b_blk(is: [r_b_set() = i | _]) ->
        :beam_ssa.is_loop_header(i)

      r_b_blk() ->
        false
    end
  end
end
