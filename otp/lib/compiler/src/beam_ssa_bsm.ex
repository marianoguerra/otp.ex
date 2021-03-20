defmodule :m_beam_ssa_bsm do
  use Bitwise

  import :lists,
    only: [
      foldl: 3,
      mapfoldl: 3,
      max: 1,
      member: 2,
      nth: 2,
      reverse: 1,
      reverse: 2,
      splitwith: 2,
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

  def format_error(optInfo) do
    format_opt_info(optInfo)
  end

  def module(r_b_module(body: fs0) = module, opts) do
    modInfo = analyze_module(module)

    {fs, _} =
      :compile.run_sub_passes(
        [
          {:combine_matches, &combine_matches/1},
          {:accept_context_args, &accept_context_args/1},
          {:combine_matches, &combine_matches/1},
          {:allow_context_passthrough, &allow_context_passthrough/1},
          {:skip_outgoing_tail_extraction, &skip_outgoing_tail_extraction/1},
          {:annotate_context_parameters, &annotate_context_parameters/1}
        ],
        {fs0, modInfo}
      )

    ws =
      case :proplists.get_bool(:bin_opt_info, opts) do
        true ->
          collect_opt_info(fs)

        false ->
          []
      end

    {:ok, r_b_module(module, body: fs), ws}
  end

  defp analyze_module(r_b_module(body: fs)) do
    foldl(
      fn r_b_function(args: parameters) = f, i ->
        funcInfo = %{has_bsm_ops: has_bsm_ops(f), parameters: parameters, parameter_info: %{}}
        funcId = get_fa(f)
        Map.put(i, funcId, funcInfo)
      end,
      %{},
      fs
    )
  end

  defp has_bsm_ops(r_b_function(bs: blocks)) do
    hbo_blocks(:maps.to_list(blocks))
  end

  defp hbo_blocks([{_, r_b_blk(is: is)} | blocks]) do
    case hbo_is(is) do
      false ->
        hbo_blocks(blocks)

      true ->
        true
    end
  end

  defp hbo_blocks([]) do
    false
  end

  defp hbo_is([r_b_set(op: :bs_start_match) | _]) do
    true
  end

  defp hbo_is([_I | is]) do
    hbo_is(is)
  end

  defp hbo_is([]) do
    false
  end

  defp check_context_call(r_b_set(args: args), arg, ctxChain, modInfo) do
    aliases = [arg | ctxChain]
    ccc_1(args, arg, aliases, modInfo)
  end

  defp ccc_1([r_b_local() = call | args], ctx, aliases, modInfo) do
    useCount =
      foldl(
        fn arg, c ->
          case member(arg, aliases) do
            true ->
              c + 1

            false ->
              c
          end
        end,
        0,
        args
      )

    cond do
      useCount === 1 ->
        r_b_local(name: r_b_literal(val: name), arity: arity) = call
        callee = {name, arity}
        paramInfo = funcinfo_get(callee, :parameter_info, modInfo)
        parameters = funcinfo_get(callee, :parameters, modInfo)
        parameter = nth(1 + arg_index(ctx, args), parameters)

        case paramInfo do
          %{^parameter => :suitable_for_reuse} ->
            :suitable_for_reuse

          %{^parameter => {:unsuitable_call, {^call, _}} = info} ->
            info

          %{^parameter => info} ->
            {:unsuitable_call, {call, info}}

          %{} ->
            {:no_match_on_entry, call}
        end

      useCount > 1 ->
        {:multiple_uses_in_call, call}
    end
  end

  defp ccc_1([r_b_remote() = call | _Args], _Ctx, _CtxChain, _ModInfo) do
    {:remote_call, call}
  end

  defp ccc_1([fun | _Args], _Ctx, _CtxChain, _ModInfo) do
    {:fun_call, fun}
  end

  defp arg_index(var, args) do
    arg_index_1(var, args, 0)
  end

  defp arg_index_1(var, [var | _Args], index) do
    index
  end

  defp arg_index_1(var, [_Arg | args], index) do
    arg_index_1(var, args, index + 1)
  end

  defp is_tail_binary(
         r_b_set(
           op: :bs_match,
           args: [r_b_literal(val: :binary) | rest]
         )
       ) do
    member(r_b_literal(val: :all), rest)
  end

  defp is_tail_binary(r_b_set(op: :bs_get_tail)) do
    true
  end

  defp is_tail_binary(_) do
    false
  end

  defp is_tail_binary(r_b_var() = var, defs) do
    case find_match_definition(var, defs) do
      {:ok, def__} ->
        is_tail_binary(def__)

      _ ->
        false
    end
  end

  defp is_tail_binary(_Literal, _Defs) do
    false
  end

  defp assert_match_context(r_b_var() = var, defs) do
    case :maps.find(var, defs) do
      {:ok, r_b_set(op: :bs_match, args: [_, r_b_var() = ctx | _])} ->
        assert_match_context(ctx, defs)

      {:ok, r_b_set(op: :bs_start_match)} ->
        :ok
    end
  end

  defp find_match_definition(r_b_var() = var, defs) do
    case :maps.find(var, defs) do
      {:ok, r_b_set(op: :bs_extract, args: [ctx])} ->
        :maps.find(ctx, defs)

      {:ok, r_b_set(op: :bs_get_tail) = def__} ->
        {:ok, def__}

      _ ->
        :error
    end
  end

  defp context_chain_of(r_b_var() = var, defs) do
    case :maps.find(var, defs) do
      {:ok, r_b_set(op: :bs_match, args: [_, r_b_var() = ctx | _])} ->
        [ctx | context_chain_of(ctx, defs)]

      {:ok, r_b_set(op: :bs_get_tail, args: [ctx])} ->
        [ctx | context_chain_of(ctx, defs)]

      {:ok, r_b_set(op: :bs_extract, args: [ctx])} ->
        [ctx | context_chain_of(ctx, defs)]

      _ ->
        []
    end
  end

  defp match_context_of(r_b_var() = var, defs) do
    ctx = match_context_of_1(var, defs)
    assert_match_context(ctx, defs)
    ctx
  end

  defp match_context_of_1(var, defs) do
    case :maps.get(var, defs) do
      r_b_set(op: :bs_extract, args: [r_b_var() = ctx0]) ->
        r_b_set(
          op: :bs_match,
          args: [_, r_b_var() = ctx | _]
        ) = :maps.get(ctx0, defs)

        ctx

      r_b_set(op: :bs_get_tail, args: [r_b_var() = ctx]) ->
        ctx
    end
  end

  defp funcinfo_get(r_b_function() = f, attribute, modInfo) do
    funcinfo_get(get_fa(f), attribute, modInfo)
  end

  defp funcinfo_get({_, _} = key, attribute, modInfo) do
    funcInfo = :maps.get(key, modInfo)
    :maps.get(attribute, funcInfo)
  end

  defp funcinfo_set(r_b_function() = f, attribute, value, modInfo) do
    funcinfo_set(get_fa(f), attribute, value, modInfo)
  end

  defp funcinfo_set(key, attribute, value, modInfo) do
    funcInfo = :maps.put(attribute, value, :maps.get(key, modInfo, %{}))
    :maps.put(key, funcInfo, modInfo)
  end

  defp get_fa(r_b_function(anno: anno)) do
    {_, name, arity} = :maps.get(:func_info, anno)
    {name, arity}
  end

  Record.defrecord(:r_amb, :amb,
    dominators: :undefined,
    match_aliases: :undefined,
    cnt: :undefined,
    promotions: %{}
  )

  defp alias_matched_binaries(blocks0, counter, aliasMap)
       when aliasMap !== %{} do
    {dominators, _} = :beam_ssa.dominators(blocks0)
    state0 = r_amb(dominators: dominators, match_aliases: aliasMap, cnt: counter)
    {blocks, state} = :beam_ssa.mapfold_blocks_rpo(&amb_1/3, [0], state0, blocks0)
    {amb_insert_promotions(blocks, state), r_amb(state, :cnt)}
  end

  defp alias_matched_binaries(blocks, counter, _AliasMap) do
    {blocks, counter}
  end

  defp amb_1(lbl, r_b_blk(is: is0, last: last0) = block, state0) do
    {is, state1} =
      mapfoldl(
        fn i, state ->
          amb_assign_set(i, lbl, state)
        end,
        state0,
        is0
      )

    {last1, state} = amb_assign_last(last0, lbl, state1)
    last = :beam_ssa.normalize(last1)
    {r_b_blk(block, is: is, last: last), state}
  end

  defp amb_assign_set(r_b_set(op: :phi, args: args0) = i, _Lbl, state0) do
    {args, state} =
      mapfoldl(
        fn {arg0, lbl}, acc ->
          {arg, state} = amb_get_alias(arg0, lbl, acc)
          {{arg, lbl}, state}
        end,
        state0,
        args0
      )

    {r_b_set(i, args: args), state}
  end

  defp amb_assign_set(r_b_set(args: args0) = i, lbl, state0) do
    {args, state} =
      mapfoldl(
        fn arg0, acc ->
          amb_get_alias(arg0, lbl, acc)
        end,
        state0,
        args0
      )

    {r_b_set(i, args: args), state}
  end

  defp amb_assign_last(r_b_ret(arg: arg0) = t, lbl, state0) do
    {arg, state} = amb_get_alias(arg0, lbl, state0)
    {r_b_ret(t, arg: arg), state}
  end

  defp amb_assign_last(r_b_switch(arg: arg0) = t, lbl, state0) do
    {arg, state} = amb_get_alias(arg0, lbl, state0)
    {r_b_switch(t, arg: arg), state}
  end

  defp amb_assign_last(r_b_br(bool: arg0) = t, lbl, state0) do
    {arg, state} = amb_get_alias(arg0, lbl, state0)
    {r_b_br(t, bool: arg), state}
  end

  defp amb_get_alias(r_b_var() = arg, lbl, state) do
    case :maps.find(arg, r_amb(state, :match_aliases)) do
      {:ok, {aliasAfter, context}} ->
        dominators = :maps.get(lbl, r_amb(state, :dominators))

        case member(aliasAfter, dominators) do
          true ->
            amb_create_alias(arg, context, lbl, state)

          false ->
            {arg, state}
        end

      :error ->
        {arg, state}
    end
  end

  defp amb_get_alias(r_b_remote(mod: mod0, name: name0) = arg0, lbl, state0) do
    {mod, state1} = amb_get_alias(mod0, lbl, state0)
    {name, state} = amb_get_alias(name0, lbl, state1)
    arg = r_b_remote(arg0, mod: mod, name: name)
    {arg, state}
  end

  defp amb_get_alias(arg, _Lbl, state) do
    {arg, state}
  end

  defp amb_create_alias(r_b_var() = arg0, context, lbl, state0) do
    dominators = :maps.get(lbl, r_amb(state0, :dominators))
    promotions0 = r_amb(state0, :promotions)

    prevPromotions =
      for dom <- dominators,
          :erlang.is_map_key({dom, arg0}, promotions0) do
        :maps.get({dom, arg0}, promotions0)
      end

    case prevPromotions do
      [_ | _] ->
        r_b_set(dst: alias) = max(prevPromotions)
        {alias, state0}

      [] ->
        counter = r_amb(state0, :cnt)
        alias = r_b_var(name: {:"@ssa_bsm_alias", counter})
        promotion = r_b_set(op: :bs_get_tail, dst: alias, args: [context])
        promotions = :maps.put({lbl, arg0}, promotion, promotions0)

        state =
          r_amb(state0,
            promotions: promotions,
            cnt: counter + 1
          )

        {alias, state}
    end
  end

  defp amb_insert_promotions(blocks0, state) do
    f = fn {lbl, r_b_var()}, promotion, blocks ->
      block = :maps.get(lbl, blocks)
      alias = r_b_set(promotion, :dst)

      {before, after__} =
        splitwith(
          fn r_b_set(args: args) ->
            not is_var_in_args(alias, args)
          end,
          r_b_blk(block, :is)
        )

      is = before ++ [promotion | after__]
      :maps.put(lbl, r_b_blk(block, is: is), blocks)
    end

    :maps.fold(f, blocks0, r_amb(state, :promotions))
  end

  defp is_var_in_args(var, [var | _]) do
    true
  end

  defp is_var_in_args(var, [r_b_remote(name: var) | _]) do
    true
  end

  defp is_var_in_args(var, [r_b_remote(mod: var) | _]) do
    true
  end

  defp is_var_in_args(var, [_ | args]) do
    is_var_in_args(var, args)
  end

  defp is_var_in_args(_Var, []) do
    false
  end

  Record.defrecord(:r_cm, :cm,
    definitions: :undefined,
    dominators: :undefined,
    blocks: :undefined,
    match_aliases: %{},
    prior_matches: %{},
    renames: %{}
  )

  defp combine_matches({fs0, modInfo}) do
    fs =
      for f <- fs0 do
        combine_matches(f, modInfo)
      end

    {fs, modInfo}
  end

  defp combine_matches(r_b_function(bs: blocks0, cnt: counter0) = f, modInfo) do
    case funcinfo_get(f, :has_bsm_ops, modInfo) do
      true ->
        {dominators, _} = :beam_ssa.dominators(blocks0)

        {blocks1, state} =
          :beam_ssa.mapfold_blocks_rpo(
            fn lbl, r_b_blk(is: is0) = block0, state0 ->
              {is, state} =
                cm_1(
                  is0,
                  [],
                  lbl,
                  state0
                )

              {r_b_blk(block0, is: is), state}
            end,
            [0],
            r_cm(
              definitions: :beam_ssa.definitions(blocks0),
              dominators: dominators,
              blocks: blocks0
            ),
            blocks0
          )

        blocks2 = :beam_ssa.rename_vars(r_cm(state, :renames), [0], blocks1)

        {blocks, counter} =
          alias_matched_binaries(
            blocks2,
            counter0,
            r_cm(state, :match_aliases)
          )

        r_b_function(f,
          bs: :beam_ssa.trim_unreachable(blocks),
          cnt: counter
        )

      false ->
        f
    end
  end

  defp cm_1(
         [
           r_b_set(op: :bs_start_match, dst: ctx, args: [_, src]),
           r_b_set(op: {:succeeded, :guard}, dst: bool, args: [ctx])
         ] = matchSeq,
         acc0,
         lbl,
         state0
       ) do
    acc = reverse(acc0)

    case is_tail_binary(src, r_cm(state0, :definitions)) do
      true ->
        cm_combine_tail(src, ctx, bool, acc, state0)

      false ->
        cm_handle_priors(src, ctx, bool, acc, matchSeq, lbl, state0)
    end
  end

  defp cm_1([i | is], acc, lbl, state) do
    cm_1(is, [i | acc], lbl, state)
  end

  defp cm_1([], acc, _Lbl, state) do
    {reverse(acc), state}
  end

  defp cm_handle_priors(src, dstCtx, bool, acc, matchSeq, lbl, state0) do
    priorCtxs =
      case :maps.find(
             src,
             r_cm(state0, :prior_matches)
           ) do
        {:ok, priors} ->
          dominators = :maps.get(lbl, r_cm(state0, :dominators), [])

          for {validAfter, ctx} <- priors,
              member(validAfter, dominators) do
            ctx
          end

        :error ->
          []
      end

    case priorCtxs do
      [ctx | _] ->
        renames0 = r_cm(state0, :renames)
        renames = Map.merge(renames0, %{bool => r_b_literal(val: true), dstCtx => ctx})
        {acc, r_cm(state0, renames: renames)}

      [] ->
        state = cm_register_prior(src, dstCtx, lbl, state0)
        {acc ++ matchSeq, state}
    end
  end

  defp cm_register_prior(src, dstCtx, lbl, state) do
    block = :maps.get(lbl, r_cm(state, :blocks))
    r_b_br(succ: validAfter) = r_b_blk(block, :last)
    priors0 = :maps.get(src, r_cm(state, :prior_matches), [])
    priors = [{validAfter, dstCtx} | priors0]
    priorMatches = :maps.put(src, priors, r_cm(state, :prior_matches))
    r_cm(state, prior_matches: priorMatches)
  end

  defp cm_combine_tail(src, dstCtx, bool, acc, state0) do
    srcCtx0 = match_context_of(src, r_cm(state0, :definitions))
    {srcCtx, renames} = cm_combine_tail_1(bool, dstCtx, srcCtx0, r_cm(state0, :renames))
    aliases = :maps.put(src, {0, srcCtx}, r_cm(state0, :match_aliases))

    state =
      r_cm(state0,
        match_aliases: aliases,
        renames: renames
      )

    {acc, state}
  end

  defp cm_combine_tail_1(bool, dstCtx, srcCtx, renames0) do
    case renames0 do
      %{^srcCtx => new} ->
        cm_combine_tail_1(bool, dstCtx, new, renames0)

      %{} ->
        renames = Map.merge(renames0, %{bool => r_b_literal(val: true), dstCtx => srcCtx})
        {srcCtx, renames}
    end
  end

  Record.defrecord(:r_aca, :aca,
    unused_parameters: :undefined,
    counter: :undefined,
    parameter_info: %{},
    match_aliases: %{}
  )

  defp accept_context_args({fs, modInfo}) do
    mapfoldl(&accept_context_args/2, modInfo, fs)
  end

  defp accept_context_args(r_b_function(bs: blocks0) = f, modInfo0) do
    case funcinfo_get(f, :has_bsm_ops, modInfo0) do
      true ->
        parameters = :ordsets.from_list(funcinfo_get(f, :parameters, modInfo0))

        state0 =
          r_aca(
            unused_parameters: parameters,
            counter: r_b_function(f, :cnt)
          )

        {blocks1, state} = aca_1(blocks0, state0)

        {blocks, counter} =
          alias_matched_binaries(
            blocks1,
            r_aca(state, :counter),
            r_aca(state, :match_aliases)
          )

        modInfo = funcinfo_set(f, :parameter_info, r_aca(state, :parameter_info), modInfo0)
        {r_b_function(f, bs: blocks, cnt: counter), modInfo}

      false ->
        {f, modInfo0}
    end
  end

  defp aca_1(blocks, state) do
    entryBlock = :maps.get(0, blocks)
    aca_enable_reuse(r_b_blk(entryBlock, :is), entryBlock, blocks, [], state)
  end

  defp aca_enable_reuse(
         [
           r_b_set(op: :bs_start_match, args: [_, src]) = i0
           | rest
         ],
         entryBlock,
         blocks0,
         acc,
         state0
       ) do
    case aca_is_reuse_safe(src, state0) do
      true ->
        {i, last0, blocks1, state} = aca_reuse_context(i0, entryBlock, blocks0, state0)
        is = reverse([i | acc], rest)
        last = :beam_ssa.normalize(last0)
        blocks = :maps.put(0, r_b_blk(entryBlock, is: is, last: last), blocks1)
        {:beam_ssa.trim_unreachable(blocks), state}

      false ->
        {blocks0, state0}
    end
  end

  defp aca_enable_reuse([i | is], entryBlock, blocks, acc, state0) do
    unusedParams0 = r_aca(state0, :unused_parameters)

    case :ordsets.intersection(
           unusedParams0,
           :beam_ssa.used(i)
         ) do
      [] ->
        aca_enable_reuse(is, entryBlock, blocks, [i | acc], state0)

      prematureUses ->
        unusedParams =
          :ordsets.subtract(
            unusedParams0,
            prematureUses
          )

        paramInfo =
          foldl(
            fn a, ps ->
              :maps.put(a, {:used_before_match, i}, ps)
            end,
            r_aca(state0, :parameter_info),
            prematureUses
          )

        state =
          r_aca(state0,
            unused_parameters: unusedParams,
            parameter_info: paramInfo
          )

        aca_enable_reuse(is, entryBlock, blocks, [i | acc], state)
    end
  end

  defp aca_enable_reuse([], _EntryBlock, blocks, _Acc, state) do
    {blocks, state}
  end

  defp aca_is_reuse_safe(src, state) do
    :ordsets.is_element(src, r_aca(state, :unused_parameters))
  end

  defp aca_reuse_context(
         r_b_set(op: :bs_start_match, dst: dst, args: [_, src]) = i0,
         block,
         blocks0,
         state0
       ) do
    {state1, last, blocks} = aca_handle_convergence(src, state0, r_b_blk(block, :last), blocks0)
    aliases = :maps.put(src, {r_b_br(last, :succ), dst}, r_aca(state1, :match_aliases))
    paramInfo = :maps.put(src, :suitable_for_reuse, r_aca(state1, :parameter_info))

    state =
      r_aca(state1,
        match_aliases: aliases,
        parameter_info: paramInfo
      )

    i = :beam_ssa.add_anno(:accepts_match_contexts, true, i0)
    {i, last, blocks, state}
  end

  defp aca_handle_convergence(src, state0, last0, blocks0) do
    r_b_br(fail: fail0, succ: succ0) = last0
    succPath = :beam_ssa.rpo([succ0], blocks0)
    failPath = :beam_ssa.rpo([fail0], blocks0)

    convergedPaths =
      :ordsets.intersection(
        :ordsets.from_list(succPath),
        :ordsets.from_list(failPath)
      )

    case :maps.is_key(
           src,
           :beam_ssa.uses(convergedPaths, blocks0)
         ) do
      true ->
        case shortest(succPath, failPath) do
          :left ->
            {succ, blocks, counter} =
              aca_copy_successors(
                succ0,
                blocks0,
                r_aca(state0, :counter)
              )

            state = r_aca(state0, counter: counter)
            last = :beam_ssa.normalize(r_b_br(last0, succ: succ))
            {state, last, blocks}

          :right ->
            {fail, blocks, counter} =
              aca_copy_successors(
                fail0,
                blocks0,
                r_aca(state0, :counter)
              )

            state = r_aca(state0, counter: counter)
            last = :beam_ssa.normalize(r_b_br(last0, fail: fail))
            {state, last, blocks}
        end

      false ->
        {state0, last0, blocks0}
    end
  end

  defp shortest([_ | as], [_ | bs]) do
    shortest(as, bs)
  end

  defp shortest([], _) do
    :left
  end

  defp shortest(_, []) do
    :right
  end

  defp aca_copy_successors(lbl0, blocks0, counter0) do
    path = :beam_ssa.rpo([lbl0], blocks0)
    {bRs, counter1} = aca_cs_build_brs(path, counter0, %{})
    {blocks, counter} = aca_cs_1(path, blocks0, counter1, %{}, bRs, %{})
    lbl = :maps.get(lbl0, bRs)
    {lbl, blocks, counter}
  end

  defp aca_cs_build_brs([1 = lbl | path], counter, acc) do
    aca_cs_build_brs(path, counter, Map.put(acc, lbl, lbl))
  end

  defp aca_cs_build_brs([lbl | path], counter0, acc) do
    aca_cs_build_brs(path, counter0 + 1, Map.put(acc, lbl, counter0))
  end

  defp aca_cs_build_brs([], counter, acc) do
    {acc, counter}
  end

  defp aca_cs_1([lbl0 | path], blocks, counter0, vRs0, bRs, acc0) do
    block0 = :maps.get(lbl0, blocks)
    lbl = :maps.get(lbl0, bRs)
    {vRs, block, counter} = aca_cs_block(block0, counter0, vRs0, bRs)
    acc = :maps.put(lbl, block, acc0)
    aca_cs_1(path, blocks, counter, vRs, bRs, acc)
  end

  defp aca_cs_1([], blocks, counter, _VRs, _BRs, acc) do
    {:maps.merge(blocks, acc), counter}
  end

  defp aca_cs_block(r_b_blk(is: is0, last: last0) = block0, counter0, vRs0, bRs) do
    {vRs, is, counter} = aca_cs_is(is0, counter0, vRs0, bRs, [])
    last1 = aca_cs_last(last0, vRs, bRs)
    last = :beam_ssa.normalize(last1)
    block = r_b_blk(block0, is: is, last: last)
    {vRs, block, counter}
  end

  defp aca_cs_is([r_b_set(op: op, dst: dst0, args: args0) = i0 | is], counter0, vRs0, bRs, acc) do
    args =
      case op do
        :phi ->
          aca_cs_args_phi(args0, vRs0, bRs)

        _ ->
          aca_cs_args(args0, vRs0)
      end

    counter = counter0 + 1
    dst = r_b_var(name: {:"@ssa_bsm_aca", counter})
    i = r_b_set(i0, dst: dst, args: args)
    vRs = :maps.put(dst0, dst, vRs0)
    aca_cs_is(is, counter, vRs, bRs, [i | acc])
  end

  defp aca_cs_is([], counter, vRs, _BRs, acc) do
    {vRs, reverse(acc), counter}
  end

  defp aca_cs_last(r_b_switch(arg: arg0, list: switch0, fail: fail0) = sw, vRs, bRs) do
    switch =
      for {literal, lbl} <- switch0 do
        {literal, :maps.get(lbl, bRs)}
      end

    r_b_switch(sw, arg: aca_cs_arg(arg0, vRs), fail: :maps.get(fail0, bRs), list: switch)
  end

  defp aca_cs_last(r_b_br(bool: arg0, succ: succ0, fail: fail0) = br, vRs, bRs) do
    r_b_br(br,
      bool: aca_cs_arg(arg0, vRs),
      succ: :maps.get(succ0, bRs),
      fail: :maps.get(fail0, bRs)
    )
  end

  defp aca_cs_last(r_b_ret(arg: arg0) = ret, vRs, _BRs) do
    r_b_ret(ret, arg: aca_cs_arg(arg0, vRs))
  end

  defp aca_cs_args_phi([{arg, lbl} | args], vRs, bRs) do
    case bRs do
      %{^lbl => new} ->
        [{aca_cs_arg(arg, vRs), new} | aca_cs_args_phi(args, vRs, bRs)]

      %{} ->
        aca_cs_args_phi(args, vRs, bRs)
    end
  end

  defp aca_cs_args_phi([], _VRs, _BRs) do
    []
  end

  defp aca_cs_args([arg | args], vRs) do
    [aca_cs_arg(arg, vRs) | aca_cs_args(args, vRs)]
  end

  defp aca_cs_args([], _VRs) do
    []
  end

  defp aca_cs_arg(r_b_remote(mod: mod0, name: name0) = rem, vRs) do
    mod = aca_cs_arg(mod0, vRs)
    name = aca_cs_arg(name0, vRs)
    r_b_remote(rem, mod: mod, name: name)
  end

  defp aca_cs_arg(arg, vRs) do
    case vRs do
      %{^arg => new} ->
        new

      %{} ->
        arg
    end
  end

  defp allow_context_passthrough({fs, modInfo0}) do
    fsUses =
      for f <- fs do
        {f, :beam_ssa.uses(r_b_function(f, :bs))}
      end

    modInfo = acp_forward_params(fsUses, modInfo0)
    {fs, modInfo}
  end

  defp acp_forward_params(fsUses, modInfo0) do
    f = fn {r_b_function(args: parameters) = func, useMap}, modInfo ->
      paramInfo =
        foldl(
          fn param, paramInfo ->
            uses = :maps.get(param, useMap, [])
            acp_1(param, uses, modInfo, paramInfo)
          end,
          funcinfo_get(func, :parameter_info, modInfo),
          parameters
        )

      funcinfo_set(func, :parameter_info, paramInfo, modInfo)
    end

    case foldl(f, modInfo0, fsUses) do
      ^modInfo0 ->
        modInfo0

      changed ->
        acp_forward_params(fsUses, changed)
    end
  end

  defp acp_1(param, [{0, r_b_set(op: :call) = i}], modInfo, paramInfo) do
    case check_context_call(i, param, [], modInfo) do
      {:no_match_on_entry, _} ->
        paramInfo

      other ->
        :maps.put(param, other, paramInfo)
    end
  end

  defp acp_1(_Param, _Uses, _ModInfo, paramInfo) do
    paramInfo
  end

  Record.defrecord(:r_sote, :sote,
    definitions: :undefined,
    mod_info: :undefined,
    match_aliases: %{}
  )

  defp skip_outgoing_tail_extraction({fs0, modInfo}) do
    fs =
      for f <- fs0 do
        skip_outgoing_tail_extraction(f, modInfo)
      end

    {fs, modInfo}
  end

  defp skip_outgoing_tail_extraction(r_b_function(bs: blocks0) = f, modInfo) do
    case funcinfo_get(f, :has_bsm_ops, modInfo) do
      true ->
        state0 =
          r_sote(
            definitions: :beam_ssa.definitions(blocks0),
            mod_info: modInfo
          )

        {blocks1, state} =
          :beam_ssa.mapfold_instrs_rpo(&sote_rewrite_calls/2, [0], state0, blocks0)

        {blocks, counter} =
          alias_matched_binaries(
            blocks1,
            r_b_function(f, :cnt),
            r_sote(state, :match_aliases)
          )

        r_b_function(f, bs: blocks, cnt: counter)

      false ->
        f
    end
  end

  defp sote_rewrite_calls(r_b_set(op: :call, args: args) = call, state) do
    sote_rewrite_call(call, args, [], state)
  end

  defp sote_rewrite_calls(i, state) do
    {i, state}
  end

  defp sote_rewrite_call(call, [], argsOut, state) do
    {r_b_set(call, args: reverse(argsOut)), state}
  end

  defp sote_rewrite_call(call0, [arg | argsIn], argsOut, state0) do
    case is_tail_binary(arg, r_sote(state0, :definitions)) do
      true ->
        ctxChain =
          context_chain_of(
            arg,
            r_sote(state0, :definitions)
          )

        case check_context_call(call0, arg, ctxChain, r_sote(state0, :mod_info)) do
          :suitable_for_reuse ->
            ctx = match_context_of(arg, r_sote(state0, :definitions))
            matchAliases0 = r_sote(state0, :match_aliases)
            matchAliases = :maps.put(arg, {0, ctx}, matchAliases0)
            state = r_sote(state0, match_aliases: matchAliases)
            call = :beam_ssa.add_anno(:bsm_info, :context_reused, call0)
            sote_rewrite_call(call, argsIn, [ctx | argsOut], state)

          other ->
            call = :beam_ssa.add_anno(:bsm_info, other, call0)
            sote_rewrite_call(call, argsIn, [arg | argsOut], state0)
        end

      false ->
        sote_rewrite_call(call0, argsIn, [arg | argsOut], state0)
    end
  end

  defp annotate_context_parameters({fs, modInfo}) do
    mapfoldl(&annotate_context_parameters/2, modInfo, fs)
  end

  defp annotate_context_parameters(f, modInfo) do
    paramInfo = funcinfo_get(f, :parameter_info, modInfo)
    paramAnno0 = :beam_ssa.get_anno(:parameter_info, f, %{})

    paramAnno =
      :maps.fold(
        fn
          k, _V, acc
          when :erlang.is_map_key(k, acc) ->
            :erlang.error(:conflicting_parameter_types)

          k, :suitable_for_reuse, acc ->
            info = :maps.get(k, acc, [])
            Map.put(acc, k, [:accepts_match_context | info])

          _K, _V, acc ->
            acc
        end,
        paramAnno0,
        paramInfo
      )

    {:beam_ssa.add_anno(:parameter_info, paramAnno, f), modInfo}
  end

  defp collect_opt_info(fs) do
    foldl(
      fn r_b_function(bs: blocks) = f, acc0 ->
        useMap = :beam_ssa.uses(blocks)
        where = :beam_ssa.get_anno(:location, f, [])

        :beam_ssa.fold_instrs_rpo(
          fn i, acc ->
            collect_opt_info_1(i, where, useMap, acc)
          end,
          [0],
          acc0,
          blocks
        )
      end,
      [],
      fs
    )
  end

  defp collect_opt_info_1(r_b_set(op: op, anno: anno, dst: dst) = i, where, useMap, acc0) do
    case is_tail_binary(i) do
      true when op === :bs_match ->
        uses0 = :maps.get(dst, useMap, [])

        case (for {_, r_b_set(op: :bs_extract) = e} <- uses0 do
                e
              end) do
          [use] ->
            add_unopt_binary_info(use, false, where, useMap, acc0)

          [] ->
            acc0
        end

      true ->
        uses = :maps.get(dst, useMap, [])

        foldl(
          fn {_Lbl, use}, acc ->
            add_unopt_binary_info(use, false, where, useMap, acc)
          end,
          acc0,
          uses
        )

      false ->
        add_opt_info(anno, where, acc0)
    end
  end

  defp collect_opt_info_1(r_b_ret(anno: anno), where, _UseMap, acc) do
    add_opt_info(anno, where, acc)
  end

  defp collect_opt_info_1(_I, _Where, _Uses, acc) do
    acc
  end

  defp add_opt_info(anno, where, acc) do
    case :maps.find(:bsm_info, anno) do
      {:ok, term} ->
        [make_warning(term, anno, where) | acc]

      :error ->
        acc
    end
  end

  defp add_unopt_binary_info(r_b_set(op: follow, dst: dst), _Nested, where, useMap, acc0)
       when follow === :put_tuple or follow === :put_list or
              follow === :put_map do
    {_, uses} = unzip(:maps.get(dst, useMap, []))

    foldl(
      fn use, acc ->
        add_unopt_binary_info(use, true, where, useMap, acc)
      end,
      acc0,
      uses
    )
  end

  defp add_unopt_binary_info(r_b_set(op: follow, dst: dst), nested, where, useMap, acc0)
       when follow === :bs_extract or follow === :phi do
    {_, uses} = unzip(:maps.get(dst, useMap, []))

    foldl(
      fn use, acc ->
        add_unopt_binary_info(use, nested, where, useMap, acc)
      end,
      acc0,
      uses
    )
  end

  defp add_unopt_binary_info(
         r_b_set(
           op: :call,
           args: [
             r_b_remote(mod: r_b_literal(val: :erlang), name: r_b_literal(val: :error))
             | _Ignored
           ]
         ),
         _Nested,
         _Where,
         _UseMap,
         acc
       ) do
    acc
  end

  defp add_unopt_binary_info(r_b_switch(anno: anno) = i, nested, where, _UseMap, acc) do
    [make_promotion_warning(i, nested, anno, where) | acc]
  end

  defp add_unopt_binary_info(r_b_set(anno: anno) = i, nested, where, _UseMap, acc) do
    [make_promotion_warning(i, nested, anno, where) | acc]
  end

  defp add_unopt_binary_info(r_b_ret(anno: anno) = i, nested, where, _UseMap, acc) do
    [make_promotion_warning(i, nested, anno, where) | acc]
  end

  defp add_unopt_binary_info(r_b_br(anno: anno) = i, nested, where, _UseMap, acc) do
    [make_promotion_warning(i, nested, anno, where) | acc]
  end

  defp make_promotion_warning(i, nested, anno, where) do
    make_warning({:binary_created, i, nested}, anno, where)
  end

  defp make_warning(term, anno, where) do
    {file, line} = :maps.get(:location, anno, where)
    {file, [{line, :beam_ssa_bsm, term}]}
  end

  defp format_opt_info(:context_reused) do
    'OPTIMIZED: match context reused'
  end

  defp format_opt_info({:binary_created, _, _} = promotion) do
    :io_lib.format('BINARY CREATED: ~s', [format_opt_info_1(promotion)])
  end

  defp format_opt_info(other) do
    :io_lib.format('NOT OPTIMIZED: ~s', [format_opt_info_1(other)])
  end

  defp format_opt_info_1({:binary_created, r_b_set(op: :call, args: [call | _]), false}) do
    :io_lib.format('binary is used in call to ~s which doesn\'t support context reuse', [
      format_call(call)
    ])
  end

  defp format_opt_info_1({:binary_created, r_b_set(op: :call, args: [call | _]), true}) do
    :io_lib.format('binary is used in term passed to ~s', [format_call(call)])
  end

  defp format_opt_info_1({:binary_created, r_b_set(op: {:bif, bIF}, args: args), false}) do
    :io_lib.format('binary is used in ~p/~p which doesn\'t support context reuse', [
      bIF,
      length(args)
    ])
  end

  defp format_opt_info_1({:binary_created, r_b_set(op: {:bif, bIF}, args: args), true}) do
    :io_lib.format('binary is used in term passed to ~p/~p', [bIF, length(args)])
  end

  defp format_opt_info_1({:binary_created, r_b_set(op: op), false}) do
    :io_lib.format('binary is used in \'~p\' which doesn\'t support context reuse', [op])
  end

  defp format_opt_info_1({:binary_created, r_b_set(op: op), true}) do
    :io_lib.format('binary is used in term passed to \'~p\'', [op])
  end

  defp format_opt_info_1({:binary_created, r_b_ret(), false}) do
    :io_lib.format('binary is returned from the function', [])
  end

  defp format_opt_info_1({:binary_created, r_b_ret(), true}) do
    :io_lib.format('binary is used in a term that is returned from the function', [])
  end

  defp format_opt_info_1({:unsuitable_call, {call, inner}}) do
    :io_lib.format(
      'binary used in call to ~s, where ~s',
      [format_call(call), format_opt_info_1(inner)]
    )
  end

  defp format_opt_info_1({:remote_call, call}) do
    :io_lib.format('binary is used in remote call to ~s', [format_call(call)])
  end

  defp format_opt_info_1({:fun_call, call}) do
    :io_lib.format('binary is used in fun call (~s)', [format_call(call)])
  end

  defp format_opt_info_1({:multiple_uses_in_call, call}) do
    :io_lib.format('binary is passed as multiple arguments to ~s', [format_call(call)])
  end

  defp format_opt_info_1({:no_match_on_entry, call}) do
    :io_lib.format(
      'binary is used in call to ~s which does not begin with a suitable binary match',
      [format_call(call)]
    )
  end

  defp format_opt_info_1({:used_before_match, r_b_set(op: :call, args: [call | _])}) do
    :io_lib.format('binary is used in call to ~s before being matched', [format_call(call)])
  end

  defp format_opt_info_1({:used_before_match, r_b_set(op: {:bif, bIF}, args: args)}) do
    :io_lib.format('binary is used in ~p/~p before being matched', [bIF, length(args)])
  end

  defp format_opt_info_1({:used_before_match, r_b_set(op: :phi)}) do
    :io_lib.format('binary is returned from an expression before being matched', [])
  end

  defp format_opt_info_1({:used_before_match, r_b_set(op: op)}) do
    :io_lib.format('binary is used in \'~p\' before being matched', [op])
  end

  defp format_opt_info_1(term) do
    :io_lib.format('~w', [term])
  end

  defp format_call(r_b_local(name: r_b_literal(val: f), arity: a)) do
    :io_lib.format('~p/~p', [f, a])
  end

  defp format_call(r_b_remote(mod: r_b_literal(val: m), name: r_b_literal(val: f), arity: a)) do
    :io_lib.format('~p:~p/~p', [m, f, a])
  end

  defp format_call(fun) do
    :io_lib.format('~p', [fun])
  end
end
