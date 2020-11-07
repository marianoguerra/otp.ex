defmodule :m_beam_kernel_to_ssa do
  use Bitwise

  import :lists,
    only: [
      all: 2,
      append: 1,
      flatmap: 2,
      foldl: 3,
      keysort: 2,
      map: 2,
      mapfoldl: 3,
      member: 2,
      reverse: 1,
      reverse: 2,
      sort: 1
    ]

  require Record

  Record.defrecord(:r_k_literal, :k_literal,
    anno: [],
    val: :undefined
  )

  Record.defrecord(:r_k_tuple, :k_tuple, anno: [], es: :undefined)

  Record.defrecord(:r_k_map, :k_map,
    anno: [],
    var: :EFE_TODO_NESTED_RECORD,
    op: :undefined,
    es: :undefined
  )

  Record.defrecord(:r_k_map_pair, :k_map_pair, anno: [], key: :undefined, val: :undefined)
  Record.defrecord(:r_k_cons, :k_cons, anno: [], hd: :undefined, tl: :undefined)

  Record.defrecord(:r_k_binary, :k_binary,
    anno: [],
    segs: :undefined
  )

  Record.defrecord(:r_k_bin_seg, :k_bin_seg,
    anno: [],
    size: :undefined,
    unit: :undefined,
    type: :undefined,
    flags: :undefined,
    seg: :undefined,
    next: :undefined
  )

  Record.defrecord(:r_k_bin_int, :k_bin_int,
    anno: [],
    size: :undefined,
    unit: :undefined,
    flags: :undefined,
    val: :undefined,
    next: :undefined
  )

  Record.defrecord(:r_k_bin_end, :k_bin_end, anno: [])
  Record.defrecord(:r_k_var, :k_var, anno: [], name: :undefined)
  Record.defrecord(:r_k_local, :k_local, anno: [], name: :undefined, arity: :undefined)

  Record.defrecord(:r_k_remote, :k_remote,
    anno: [],
    mod: :undefined,
    name: :undefined,
    arity: :undefined
  )

  Record.defrecord(:r_k_internal, :k_internal, anno: [], name: :undefined, arity: :undefined)

  Record.defrecord(:r_k_mdef, :k_mdef,
    anno: [],
    name: :undefined,
    exports: :undefined,
    attributes: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_k_fdef, :k_fdef,
    anno: [],
    func: :undefined,
    arity: :undefined,
    vars: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_k_seq, :k_seq, anno: [], arg: :undefined, body: :undefined)
  Record.defrecord(:r_k_put, :k_put, anno: [], arg: :undefined, ret: [])
  Record.defrecord(:r_k_bif, :k_bif, anno: [], op: :undefined, args: :undefined, ret: [])
  Record.defrecord(:r_k_test, :k_test, anno: [], op: :undefined, args: :undefined)
  Record.defrecord(:r_k_call, :k_call, anno: [], op: :undefined, args: :undefined, ret: [])
  Record.defrecord(:r_k_enter, :k_enter, anno: [], op: :undefined, args: :undefined)

  Record.defrecord(:r_k_try, :k_try,
    anno: [],
    arg: :undefined,
    vars: :undefined,
    body: :undefined,
    evars: :undefined,
    handler: :undefined,
    ret: []
  )

  Record.defrecord(:r_k_try_enter, :k_try_enter,
    anno: [],
    arg: :undefined,
    vars: :undefined,
    body: :undefined,
    evars: :undefined,
    handler: :undefined
  )

  Record.defrecord(:r_k_catch, :k_catch, anno: [], body: :undefined, ret: [])

  Record.defrecord(:r_k_letrec_goto, :k_letrec_goto,
    anno: [],
    label: :undefined,
    first: :undefined,
    then: :undefined,
    ret: []
  )

  Record.defrecord(:r_k_goto, :k_goto,
    anno: [],
    label: :undefined
  )

  Record.defrecord(:r_k_match, :k_match, anno: [], body: :undefined, ret: [])
  Record.defrecord(:r_k_alt, :k_alt, anno: [], first: :undefined, then: :undefined)
  Record.defrecord(:r_k_select, :k_select, anno: [], var: :undefined, types: :undefined)

  Record.defrecord(:r_k_type_clause, :k_type_clause,
    anno: [],
    type: :undefined,
    values: :undefined
  )

  Record.defrecord(:r_k_val_clause, :k_val_clause, anno: [], val: :undefined, body: :undefined)

  Record.defrecord(:r_k_guard, :k_guard,
    anno: [],
    clauses: :undefined
  )

  Record.defrecord(:r_k_guard_clause, :k_guard_clause,
    anno: [],
    guard: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_k_break, :k_break, anno: [], args: [])
  Record.defrecord(:r_k_return, :k_return, anno: [], args: [])

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

  Record.defrecord(:r_cg, :cg,
    lcount: 1,
    bfail: 1,
    catch_label: :none,
    vars: %{},
    break: 0,
    recv: 0,
    ultimate_failure: 0,
    labels: %{},
    no_make_fun3: false
  )

  Record.defrecord(:r_cg_break, :cg_break,
    args: :undefined,
    phi: :undefined
  )

  Record.defrecord(:r_cg_phi, :cg_phi, vars: :undefined)
  Record.defrecord(:r_cg_unreachable, :cg_unreachable, [])

  def module(
        r_k_mdef(name: mod, exports: es, attributes: attr, body: forms),
        opts
      ) do
    noMakeFun3 = :proplists.get_bool(:no_make_fun3, opts)
    body = functions(forms, mod, noMakeFun3)
    module = r_b_module(name: mod, exports: es, attributes: attr, body: body)
    {:ok, module}
  end

  defp functions(forms, mod, noMakeFun3) do
    for f <- forms do
      function(f, mod, noMakeFun3)
    end
  end

  defp function(
         r_k_fdef(anno: anno0, func: name, arity: arity, vars: as0, body: kb),
         mod,
         noMakeFun3
       ) do
    try do
      r_k_match() = kb
      st0 = r_cg(no_make_fun3: noMakeFun3)
      {as, st1} = new_ssa_vars(as0, st0)
      {asm, st} = cg_fun(kb, st1)
      anno1 = line_anno(anno0)
      anno = %{anno1 | :func_info => {mod, name, arity}}
      r_b_function(anno: anno, args: as, bs: asm, cnt: r_cg(st, :lcount))
    catch
      class, error ->
        :io.fwrite('Function: ~w/~w\n', [name, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  defp cg_fun(ke, st0) do
    {ultimateFail, failIs, st1} = make_failure(:badarg, st0)
    1 = ultimateFail

    st2 =
      r_cg(st1,
        bfail: ultimateFail,
        ultimate_failure: ultimateFail
      )

    {b, st} = cg(ke, st2)
    asm = [{:label, 0} | b ++ failIs]
    finalize(asm, st)
  end

  defp make_failure(reason, st0) do
    {lbl, st1} = new_label(st0)
    {dst, st} = new_ssa_var(:"@ssa_ret", st1)

    is = [
      {:label, lbl},
      r_b_set(
        op: :call,
        dst: dst,
        args: [
          r_b_remote(mod: r_b_literal(val: :erlang), name: r_b_literal(val: :error), arity: 1),
          r_b_literal(val: reason)
        ]
      ),
      r_b_ret(arg: dst)
    ]

    {lbl, is, st}
  end

  defp cg(r_k_match(body: m, ret: rs), st) do
    do_match_cg(m, rs, st)
  end

  defp cg(r_k_seq(arg: arg, body: body), st0) do
    {argIs, st1} = cg(arg, st0)
    {bodyIs, st} = cg(body, st1)
    {argIs ++ bodyIs, st}
  end

  defp cg(r_k_call(anno: le, op: func, args: as, ret: rs), st) do
    call_cg(func, as, rs, le, st)
  end

  defp cg(r_k_enter(anno: le, op: func, args: as), st) do
    enter_cg(func, as, le, st)
  end

  defp cg(r_k_bif(anno: le) = bif, st) do
    bif_cg(bif, le, st)
  end

  defp cg(
         r_k_try(arg: ta, vars: vs, body: tb, evars: evs, handler: th, ret: rs),
         st
       ) do
    try_cg(ta, vs, tb, evs, th, rs, st)
  end

  defp cg(
         r_k_try_enter(arg: ta, vars: vs, body: tb, evars: evs, handler: th),
         st
       ) do
    try_enter_cg(ta, vs, tb, evs, th, st)
  end

  defp cg(r_k_catch(body: cb, ret: [r]), st) do
    do_catch_cg(cb, r, st)
  end

  defp cg(r_k_put(anno: le, arg: con, ret: var), st) do
    put_cg(var, con, le, st)
  end

  defp cg(r_k_return(args: [ret0]), st) do
    ret = ssa_arg(ret0, st)
    {[r_b_ret(arg: ret)], st}
  end

  defp cg(r_k_break(args: bs), r_cg(break: br) = st) do
    args = ssa_args(bs, st)
    {[r_cg_break(args: args, phi: br)], st}
  end

  defp cg(
         r_k_letrec_goto(label: label, first: first, then: then, ret: rs),
         r_cg(break: oldBreak, labels: labels0) = st0
       ) do
    {tf, st1} = new_label(st0)
    {b, st2} = new_label(st1)
    labels = %{labels0 | label => tf}

    {fis, st3} =
      cg(
        first,
        r_cg(st2, labels: labels, break: b)
      )

    {sis, st4} = cg(then, st3)
    st5 = r_cg(st4, labels: labels0)
    {breakVars, st} = new_ssa_vars(rs, st5)
    phi = r_cg_phi(vars: breakVars)
    {fis ++ [{:label, tf}] ++ sis ++ [{:label, b}, phi], r_cg(st, break: oldBreak)}
  end

  defp cg(r_k_goto(label: label), r_cg(labels: labels) = st) do
    branch = :erlang.map_get(label, labels)
    {[make_uncond_branch(branch)], st}
  end

  defp do_match_cg(m, rs, r_cg(bfail: bfail, break: oldBreak) = st0) do
    {b, st1} = new_label(st0)
    {mis, st2} = match_cg(m, bfail, r_cg(st1, break: b))
    st3 = r_cg(st2, break: oldBreak)
    {breakVars, st} = new_ssa_vars(rs, st3)
    {mis ++ [{:label, b}, r_cg_phi(vars: breakVars)], st}
  end

  defp match_cg(r_k_alt(first: f, then: s), fail, st0) do
    {tf, st1} = new_label(st0)
    {fis, st2} = match_cg(f, tf, st1)
    {sis, st3} = match_cg(s, fail, st2)
    {fis ++ [{:label, tf}] ++ sis, st3}
  end

  defp match_cg(r_k_select(var: r_k_var() = v, types: scs), fail, st) do
    match_fmf(
      fn s, f, sta ->
        select_cg(s, v, f, fail, sta)
      end,
      fail,
      st,
      scs
    )
  end

  defp match_cg(r_k_guard(clauses: gcs), fail, st) do
    match_fmf(
      fn g, f, sta ->
        guard_clause_cg(g, f, sta)
      end,
      fail,
      st,
      gcs
    )
  end

  defp match_cg(ke, _Fail, st0) do
    cg(ke, st0)
  end

  defp select_cg(r_k_type_clause(type: :k_binary, values: [s]), var, tf, vf, st) do
    select_binary(s, var, tf, vf, st)
  end

  defp select_cg(r_k_type_clause(type: :k_bin_seg, values: vs), var, tf, _Vf, st) do
    select_bin_segs(vs, var, tf, st)
  end

  defp select_cg(r_k_type_clause(type: :k_bin_int, values: vs), var, tf, _Vf, st) do
    select_bin_segs(vs, var, tf, st)
  end

  defp select_cg(r_k_type_clause(type: :k_bin_end, values: [s]), var, tf, _Vf, st) do
    select_bin_end(s, var, tf, st)
  end

  defp select_cg(r_k_type_clause(type: :k_map, values: vs), var, tf, vf, st) do
    select_map(vs, var, tf, vf, st)
  end

  defp select_cg(r_k_type_clause(type: :k_cons, values: [s]), var, tf, vf, st) do
    select_cons(s, var, tf, vf, st)
  end

  defp select_cg(r_k_type_clause(type: :k_nil, values: [s]), var, tf, vf, st) do
    select_nil(s, var, tf, vf, st)
  end

  defp select_cg(r_k_type_clause(type: :k_literal, values: vs), var, tf, vf, st) do
    select_literal(vs, var, tf, vf, st)
  end

  defp select_cg(r_k_type_clause(type: type, values: scs), var, tf, vf, st0) do
    {vis, st1} =
      mapfoldl(
        fn s, sta ->
          {val, is, stb} = select_val(s, var, vf, sta)
          {{is, [val]}, stb}
        end,
        st0,
        scs
      )

    optVls = combine(:lists.sort(combine(vis)))
    {vls, sis, st2} = select_labels(optVls, st1, [], [])
    arg = ssa_arg(var, st2)
    {is, st} = select_val_cg(type, arg, vls, tf, vf, sis, st2)
    {is, st}
  end

  defp select_val_cg(:k_atom, {:bool, dst}, vls, _Tf, _Vf, sis, st) do
    [{r_b_literal(val: false), fail}, {r_b_literal(val: true), succ}] = sort(vls)

    case dst do
      r_b_var() ->
        br = r_b_br(bool: dst, succ: succ, fail: fail)
        {[br | sis], st}

      r_b_literal(val: true) = bool ->
        br = r_b_br(bool: bool, succ: succ, fail: succ)
        {[br | sis], st}
    end
  end

  defp select_val_cg(:k_atom, {:succeeded, dst}, vls, _Tf, _Vf, sis, st0) do
    [{r_b_literal(val: false), fail}, {r_b_literal(val: true), succ}] = sort(vls)
    r_b_var() = dst
    {bool, st} = new_ssa_var(:"@ssa_bool", st0)
    succeeded = r_b_set(op: {:succeeded, :guard}, dst: bool, args: [dst])
    br = r_b_br(bool: bool, succ: succ, fail: fail)
    {[[succeeded, br] | sis], st}
  end

  defp select_val_cg(:k_tuple, tuple, vls, tf, vf, sis, st0) do
    {is0, st1} = make_cond_branch({:bif, :is_tuple}, [tuple], tf, st0)
    {arity, st2} = new_ssa_var(:"@ssa_arity", st1)
    getArity = r_b_set(op: {:bif, :tuple_size}, dst: arity, args: [tuple])
    {is, st} = select_val_cg(:k_int, arity, vls, vf, vf, sis, st2)
    {is0 ++ [getArity] ++ is, st}
  end

  defp select_val_cg(type, r, vls, tf, vf, sis, st0) do
    {typeIs, st1} =
      cond do
        tf === vf ->
          {[], st0}

        true ->
          test = select_type_test(type)
          make_cond_branch(test, [r], tf, st0)
      end

    case vls do
      [{val, succ}] ->
        {is, st} = make_cond({:bif, :"=:="}, [r, val], vf, succ, st1)
        {typeIs ++ is ++ sis, st}

      [_ | _] ->
        {typeIs ++ [r_b_switch(arg: r, fail: vf, list: vls) | sis], st1}
    end
  end

  defp select_type_test(:k_int) do
    {:bif, :is_integer}
  end

  defp select_type_test(:k_atom) do
    {:bif, :is_atom}
  end

  defp select_type_test(:k_float) do
    {:bif, :is_float}
  end

  defp combine([[{is, vs1}, {is, vs2}] | vis]) do
    combine([{is, vs1 ++ vs2} | vis])
  end

  defp combine([v | vis]) do
    [v | combine(vis)]
  end

  defp combine([]) do
    []
  end

  defp select_labels([{is, vs} | vis], st0, vls, sis) do
    {lbl, st1} = new_label(st0)
    select_labels(vis, st1, add_vls(vs, lbl, vls), [[{:label, lbl} | is] | sis])
  end

  defp select_labels([], st, vls, sis) do
    {vls, append(sis), st}
  end

  defp add_vls([v | vs], lbl, acc) do
    add_vls(vs, lbl, [{r_b_literal(val: v), lbl} | acc])
  end

  defp add_vls([], _, acc) do
    acc
  end

  defp select_literal(s, v, tf, vf, st) do
    src = ssa_arg(v, st)

    f = fn valClause, fail, st0 ->
      {val, valIs, st1} = select_val(valClause, v, vf, st0)
      args = [src, r_b_literal(val: val)]
      {is, st2} = make_cond_branch({:bif, :"=:="}, args, fail, st1)
      {is ++ valIs, st2}
    end

    match_fmf(f, tf, st, s)
  end

  defp select_cons(r_k_val_clause(val: r_k_cons(hd: hd, tl: tl), body: b), v, tf, vf, st0) do
    es = [hd, tl]
    {eis, st1} = select_extract_cons(v, es, st0)
    {bis, st2} = match_cg(b, vf, st1)
    src = ssa_arg(v, st2)
    {is, st} = make_cond_branch(:is_nonempty_list, [src], tf, st2)
    {is ++ eis ++ bis, st}
  end

  defp select_nil(r_k_val_clause(val: r_k_literal(val: []), body: b), v, tf, vf, st0) do
    {bis, st1} = match_cg(b, vf, st0)
    src = ssa_arg(v, st1)
    {is, st} = make_cond_branch({:bif, :"=:="}, [src, r_b_literal(val: [])], tf, st1)
    {is ++ bis, st}
  end

  defp select_binary(
         r_k_val_clause(val: r_k_binary(segs: r_k_var(name: ctx0)), body: b),
         r_k_var() = src,
         tf,
         vf,
         st0
       ) do
    {ctx, st1} = new_ssa_var(ctx0, st0)
    {bis0, st2} = match_cg(b, vf, st1)
    {testIs, st} = make_succeeded(ctx, {:guard, tf}, st2)

    bis1 =
      [r_b_set(op: :bs_start_match, dst: ctx, args: [r_b_literal(val: :new), ssa_arg(src, st)])] ++
        testIs ++ bis0

    bis = finish_bs_matching(bis1)
    {bis, st}
  end

  defp finish_bs_matching([
         r_b_set(
           op: :bs_match,
           args: [r_b_literal(val: :string), ctx, r_b_literal(val: binList)]
         ) = set
         | is
       ])
       when is_list(binList) do
    i =
      r_b_set(set,
        args: [
          r_b_literal(val: :string),
          ctx,
          r_b_literal(val: :erlang.list_to_bitstring(binList))
        ]
      )

    finish_bs_matching([i | is])
  end

  defp finish_bs_matching([i | is]) do
    [i | finish_bs_matching(is)]
  end

  defp finish_bs_matching([]) do
    []
  end

  defp make_cond(cond__, args, fail, succ, st0) do
    {bool, st} = new_ssa_var(:"@ssa_bool", st0)
    bif = r_b_set(op: cond__, dst: bool, args: args)
    br = r_b_br(bool: bool, succ: succ, fail: fail)
    {[bif, br], st}
  end

  defp make_cond_branch(cond__, args, fail, st0) do
    {bool, st1} = new_ssa_var(:"@ssa_bool", st0)
    {succ, st} = new_label(st1)
    bif = r_b_set(op: cond__, dst: bool, args: args)
    br = r_b_br(bool: bool, succ: succ, fail: fail)
    {[bif, br, {:label, succ}], st}
  end

  defp make_uncond_branch(fail) do
    r_b_br(bool: r_b_literal(val: true), succ: fail, fail: fail)
  end

  defp make_succeeded(var, {:guard, fail}, st) do
    make_succeeded_1(var, :guard, fail, st)
  end

  defp make_succeeded(var, {:in_catch, catchLbl}, st) do
    make_succeeded_1(var, :body, catchLbl, st)
  end

  defp make_succeeded(var, {:no_catch, fail}, st) do
    r_cg(ultimate_failure: ^fail) = st
    make_succeeded_1(var, :body, fail, st)
  end

  defp make_succeeded_1(var, kind, fail, st0) do
    {bool, st1} = new_ssa_var(:"@ssa_bool", st0)
    {succ, st} = new_label(st1)

    check = [
      r_b_set(op: {:succeeded, kind}, dst: bool, args: [var]),
      r_b_br(bool: bool, succ: succ, fail: fail)
    ]

    {check ++ [{:label, succ}], st}
  end

  defp select_bin_segs(scs, ivar, tf, st) do
    match_fmf(
      fn s, fail, sta ->
        select_bin_seg(s, ivar, fail, sta)
      end,
      tf,
      st,
      scs
    )
  end

  defp select_bin_seg(
         r_k_val_clause(
           val: r_k_bin_seg(size: size, unit: u, type: t, seg: seg, flags: fs, next: next),
           body: b,
           anno: anno
         ),
         r_k_var() = src,
         fail,
         st0
       ) do
    lineAnno = line_anno(anno)
    ctx = get_context(src, st0)
    {mis, st1} = select_extract_bin(next, size, u, t, fs, fail, ctx, lineAnno, st0)
    {extracted, st2} = new_ssa_var(r_k_var(seg, :name), st1)
    {bis, st} = match_cg(b, fail, st2)
    bsGet = r_b_set(op: :bs_extract, dst: extracted, args: [ssa_arg(next, st)])
    is = mis ++ [bsGet] ++ bis
    {is, st}
  end

  defp select_bin_seg(
         r_k_val_clause(
           val: r_k_bin_int(size: sz, unit: u, flags: fs, val: val, next: next),
           body: b
         ),
         r_k_var() = src,
         fail,
         st0
       ) do
    ctx = get_context(src, st0)
    {mis, st1} = select_extract_int(next, val, sz, u, fs, fail, ctx, st0)
    {bis, st} = match_cg(b, fail, st1)

    is =
      case mis ++ bis do
        [
          [
            r_b_set(
              op: :bs_match,
              args: [r_b_literal(val: :string), otherCtx1, bin1]
            ),
            r_b_set(op: {:succeeded, :guard}, dst: bool1),
            r_b_br(bool: bool1, succ: succ, fail: ^fail),
            {:label, succ},
            r_b_set(op: :bs_match, dst: dst, args: [r_b_literal(val: :string), _OtherCtx2, bin2])
          ]
          | [
              [r_b_set(op: {:succeeded, :guard}, dst: bool2), r_b_br(bool: bool2, fail: ^fail)]
              | _
            ] = is0
        ] ->
          {r_b_literal(val: b1), r_b_literal(val: b2)} = {bin1, bin2}
          bin = r_b_literal(val: [b1, b2])

          set =
            r_b_set(op: :bs_match, dst: dst, args: [r_b_literal(val: :string), otherCtx1, bin])

          [set | is0]

        is0 ->
          is0
      end

    {is, st}
  end

  defp get_context(r_k_var() = var, st) do
    ssa_arg(var, st)
  end

  defp select_bin_end(r_k_val_clause(val: r_k_bin_end(), body: b), src, tf, st0) do
    ctx = get_context(src, st0)
    {bis, st1} = match_cg(b, tf, st0)
    {testIs, st} = make_cond_branch(:bs_test_tail, [ctx, r_b_literal(val: 0)], tf, st1)
    is = testIs ++ bis
    {is, st}
  end

  defp select_extract_bin(r_k_var(name: hd), size0, unit, type, flags, vf, ctx, anno, st0) do
    {dst, st1} = new_ssa_var(hd, st0)

    size =
      case {size0, ssa_arg(size0, st0)} do
        {r_k_var(), r_b_literal(val: :all)} ->
          r_b_literal(val: :bad_size)

        {_, size1} ->
          size1
      end

    build_bs_instr(anno, type, vf, ctx, size, unit, flags, dst, st1)
  end

  defp select_extract_int(r_k_var(name: tl), 0, r_k_literal(val: 0), _U, _Fs, _Vf, ctx, st0) do
    st = set_ssa_var(tl, ctx, st0)
    {[], st}
  end

  defp select_extract_int(r_k_var(name: tl), val, r_k_literal(val: sz), u, fs, vf, ctx, st0)
       when is_integer(sz) do
    {dst, st1} = new_ssa_var(tl, st0)
    bits = u * sz

    bin =
      case member(:big, fs) do
        true ->
          <<val::size(bits)>>

        false ->
          true = member(:little, fs)
          <<val::size(bits)-little>>
      end

    ^bits = bit_size(bin)
    {testIs, st} = make_succeeded(dst, {:guard, vf}, st1)

    set =
      r_b_set(
        op: :bs_match,
        dst: dst,
        args: [r_b_literal(val: :string), ctx, r_b_literal(val: bin)]
      )

    {[set | testIs], st}
  end

  defp build_bs_instr(anno, type, fail, ctx, size, unit0, flags0, dst, st0) do
    unit = r_b_literal(val: unit0)
    flags = r_b_literal(val: flags0)
    needSize = bs_need_size(type)
    typeArg = r_b_literal(val: type)

    get =
      case needSize do
        true ->
          r_b_set(anno: anno, op: :bs_match, dst: dst, args: [typeArg, ctx, flags, size, unit])

        false ->
          r_b_set(anno: anno, op: :bs_match, dst: dst, args: [typeArg, ctx, flags])
      end

    {is, st} = make_succeeded(dst, {:guard, fail}, st0)
    {[get | is], st}
  end

  defp select_val(r_k_val_clause(val: r_k_tuple(es: es), body: b), v, vf, st0) do
    {eis, st1} = select_extract_tuple(v, es, st0)
    {bis, st2} = match_cg(b, vf, st1)
    {length(es), eis ++ bis, st2}
  end

  defp select_val(r_k_val_clause(val: r_k_literal(val: val), body: b), _V, vf, st0) do
    {bis, st1} = match_cg(b, vf, st0)
    {val, bis, st1}
  end

  defp select_extract_tuple(src, vs, st0) do
    tuple = ssa_arg(src, st0)

    f = fn r_k_var(anno: anno, name: v), {elem, s0} ->
      case member(:unused, anno) do
        true ->
          {[], {elem + 1, s0}}

        false ->
          args = [tuple, r_b_literal(val: elem)]
          {dst, s} = new_ssa_var(v, s0)
          get = r_b_set(op: :get_tuple_element, dst: dst, args: args)
          {[get], {elem + 1, s}}
      end
    end

    {es, {_, st}} = flatmapfoldl(f, {0, st0}, vs)
    {es, st}
  end

  defp select_map(scs, v, tf, vf, st0) do
    mapSrc = ssa_arg(v, st0)

    {is, st1} =
      match_fmf(
        fn r_k_val_clause(
             val: r_k_map(op: :exact, es: es),
             body: b
           ),
           fail,
           st1 ->
          select_map_val(v, es, b, fail, st1)
        end,
        vf,
        st0,
        scs
      )

    {testIs, st} = make_cond_branch({:bif, :is_map}, [mapSrc], tf, st1)
    {testIs ++ is, st}
  end

  defp select_map_val(v, es, b, fail, st0) do
    {eis, st1} = select_extract_map(es, v, fail, st0)
    {bis, st2} = match_cg(b, fail, st1)
    {eis ++ bis, st2}
  end

  defp select_extract_map([p | ps], src, fail, st0) do
    mapSrc = ssa_arg(src, st0)
    r_k_map_pair(key: key0, val: r_k_var(name: dst0)) = p
    key = ssa_arg(key0, st0)
    {dst, st1} = new_ssa_var(dst0, st0)
    set = r_b_set(op: :get_map_element, dst: dst, args: [mapSrc, key])
    {testIs, st2} = make_succeeded(dst, {:guard, fail}, st1)
    {is, st} = select_extract_map(ps, src, fail, st2)
    {[set | testIs] ++ is, st}
  end

  defp select_extract_map([], _, _, st) do
    {[], st}
  end

  defp select_extract_cons(src0, [r_k_var(name: hd), r_k_var(name: tl)], st0) do
    src = ssa_arg(src0, st0)
    {hdDst, st1} = new_ssa_var(hd, st0)
    {tlDst, st2} = new_ssa_var(tl, st1)
    getHd = r_b_set(op: :get_hd, dst: hdDst, args: [src])
    getTl = r_b_set(op: :get_tl, dst: tlDst, args: [src])
    {[getHd, getTl], st2}
  end

  defp guard_clause_cg(r_k_guard_clause(guard: g, body: b), fail, st0) do
    {gis, st1} = guard_cg(g, fail, st0)
    {bis, st} = match_cg(b, fail, st1)
    {gis ++ bis, st}
  end

  defp guard_cg(
         r_k_try(
           arg: ts,
           vars: [],
           body: r_k_break(args: []),
           evars: [],
           handler: r_k_break(args: [])
         ),
         fail,
         r_cg(bfail: oldBfail, break: oldBreak) = st0
       ) do
    {next, st1} = new_label(st0)
    {tis, st2} = guard_cg(ts, fail, r_cg(st1, bfail: fail, break: next))
    is = tis ++ [{:label, next}, r_cg_phi(vars: [])]
    {is, r_cg(st2, bfail: oldBfail, break: oldBreak)}
  end

  defp guard_cg(r_k_test(op: test0, args: as), fail, st0) do
    r_k_remote(mod: r_k_literal(val: :erlang), name: r_k_literal(val: test)) = test0
    test_cg(test, false, as, fail, st0)
  end

  defp guard_cg(r_k_seq(arg: arg, body: body), fail, st0) do
    {argIs, st1} = guard_cg(arg, fail, st0)
    {bodyIs, st} = guard_cg(body, fail, st1)
    {argIs ++ bodyIs, st}
  end

  defp guard_cg(g, _Fail, st) do
    cg(g, st)
  end

  defp test_cg(:"=/=", inverted, as, fail, st) do
    test_cg(:"=:=", not inverted, as, fail, st)
  end

  defp test_cg(:"/=", inverted, as, fail, st) do
    test_cg(:==, not inverted, as, fail, st)
  end

  defp test_cg(test, inverted, as0, fail, st0) do
    as = ssa_args(as0, st0)

    case {test, ssa_args(as0, st0)} do
      {:is_record, [tuple, r_b_literal(val: atom) = tag, r_b_literal(val: int) = arity]}
      when is_atom(atom) and is_integer(int) ->
        false = inverted
        test_is_record_cg(fail, tuple, tag, arity, st0)

      {_, ^as} ->
        {bool, st1} = new_ssa_var(:"@ssa_bool", st0)
        {succ, st} = new_label(st1)
        bif = r_b_set(op: {:bif, test}, dst: bool, args: as)

        br =
          case inverted do
            false ->
              r_b_br(bool: bool, succ: succ, fail: fail)

            true ->
              r_b_br(bool: bool, succ: fail, fail: succ)
          end

        {[bif, br, {:label, succ}], st}
    end
  end

  defp test_is_record_cg(fail, tuple, tagVal, arityVal, st0) do
    {arity, st1} = new_ssa_var(:"@ssa_arity", st0)
    {tag, st2} = new_ssa_var(:"@ssa_tag", st1)
    {is0, st3} = make_cond_branch({:bif, :is_tuple}, [tuple], fail, st2)
    getArity = r_b_set(op: {:bif, :tuple_size}, dst: arity, args: [tuple])
    {is1, st4} = make_cond_branch({:bif, :"=:="}, [arity, arityVal], fail, st3)
    getTag = r_b_set(op: :get_tuple_element, dst: tag, args: [tuple, r_b_literal(val: 0)])
    {is2, st} = make_cond_branch({:bif, :"=:="}, [tag, tagVal], fail, st4)
    is = is0 ++ [getArity] ++ is1 ++ [getTag] ++ is2
    {is, st}
  end

  defp match_fmf(f, lastFail, st, [h]) do
    f.(h, lastFail, st)
  end

  defp match_fmf(f, lastFail, st0, [h | t]) do
    {fail, st1} = new_label(st0)
    {r, st2} = f.(h, fail, st1)
    {rs, st3} = match_fmf(f, lastFail, st2, t)
    {r ++ [{:label, fail}] ++ rs, st3}
  end

  defp fail_context(r_cg(catch_label: catch__, bfail: fail, ultimate_failure: ult)) do
    cond do
      fail !== ult ->
        {:guard, fail}

      catch__ === :none ->
        {:no_catch, fail}

      is_integer(catch__) ->
        {:in_catch, catch__}
    end
  end

  defp call_cg(func, as, [], le, st) do
    call_cg(func, as, [r_k_var(name: :"@ssa_ignored")], le, st)
  end

  defp call_cg(func, as, [r_k_var(name: r) | moreRs] = rs, le, st0) do
    case fail_context(st0) do
      {:guard, fail} ->
        r_k_remote(mod: r_k_literal(val: :erlang), name: r_k_literal(val: :error)) = func
        st = set_unused_ssa_vars(rs, st0)
        {[make_uncond_branch(fail), r_cg_unreachable()], st}

      failCtx ->
        args = ssa_args([func | as], st0)
        {ret, st1} = new_ssa_var(r, st0)
        call = r_b_set(anno: line_anno(le), op: :call, dst: ret, args: args)
        st2 = set_unused_ssa_vars(moreRs, st1)
        {testIs, st} = make_succeeded(ret, failCtx, st2)
        {[call | testIs], st}
    end
  end

  defp enter_cg(func, as0, le, st0) do
    as = ssa_args([func | as0], st0)
    {ret, st} = new_ssa_var(:"@ssa_ret", st0)
    call = r_b_set(anno: line_anno(le), op: :call, dst: ret, args: as)
    {[call, r_b_ret(arg: ret)], st}
  end

  defp bif_cg(r_k_bif(op: r_k_internal(name: name), args: as, ret: rs), _Le, st) do
    internal_cg(name, as, rs, st)
  end

  defp bif_cg(
         r_k_bif(
           op:
             r_k_remote(
               mod: r_k_literal(val: :erlang),
               name: r_k_literal(val: name)
             ),
           args: as,
           ret: rs
         ),
         le,
         st
       ) do
    bif_cg(name, as, rs, le, st)
  end

  defp internal_cg(:raise, as, [r_k_var(name: dst0)], st0) do
    args = ssa_args(as, st0)
    {dst, st} = new_ssa_var(dst0, st0)
    resume = r_b_set(op: :resume, dst: dst, args: args)

    case fail_context(st) do
      {:no_catch, _Fail} ->
        is = [resume, r_b_ret(arg: dst), r_cg_unreachable()]
        {is, st}

      {:in_catch, fail} ->
        is = [resume, make_uncond_branch(fail), r_cg_unreachable()]
        {is, st}
    end
  end

  defp internal_cg(:recv_peek_message, [], [r_k_var(name: succeeded0), r_k_var(name: dst0)], st0) do
    {dst, st1} = new_ssa_var(dst0, st0)
    st = new_succeeded_value(succeeded0, dst, st1)
    set = r_b_set(op: :peek_message, dst: dst, args: [])
    {[set], st}
  end

  defp internal_cg(:recv_wait_timeout, as, [r_k_var(name: succeeded0)], st0) do
    case ssa_args(as, st0) do
      [r_b_literal(val: 0)] ->
        st = new_bool_value(succeeded0, r_b_literal(val: true), st0)
        {[], st}

      args ->
        {wait, st1} = new_ssa_var(:"@ssa_wait", st0)
        {succ, st2} = make_succeeded(wait, fail_context(st1), st1)
        st = new_bool_value(succeeded0, wait, st2)
        set = r_b_set(op: :wait_timeout, dst: wait, args: args)
        {[set | succ], st}
    end
  end

  defp internal_cg(op0, as, [r_k_var(name: dst0)], st0)
       when is_atom(op0) do
    {dst, st} = new_ssa_var(dst0, st0)
    args = ssa_args(as, st)
    op = fix_op(op0, st)
    set = r_b_set(op: op, dst: dst, args: args)
    {[set], st}
  end

  defp internal_cg(op0, as, [], st0) when is_atom(op0) do
    {dst, st} = new_ssa_var(:"@ssa_ignored", st0)
    args = ssa_args(as, st)
    op = fix_op(op0, st)
    set = r_b_set(op: op, dst: dst, args: args)
    {[set], st}
  end

  defp fix_op(:make_fun, r_cg(no_make_fun3: true)) do
    :old_make_fun
  end

  defp fix_op(op, _) do
    op
  end

  defp bif_cg(bif, as0, [r_k_var(name: dst0)], le, st0) do
    {dst, st1} = new_ssa_var(dst0, st0)

    case {bif, ssa_args(as0, st0)} do
      {:is_record, [tuple, r_b_literal(val: atom) = tag, r_b_literal(val: int) = arity]}
      when is_atom(atom) and is_integer(int) ->
        bif_is_record_cg(dst, tuple, tag, arity, st1)

      {_, as} ->
        i = r_b_set(anno: line_anno(le), op: {:bif, bif}, dst: dst, args: as)

        case :erl_bifs.is_safe(:erlang, bif, length(as)) do
          false ->
            failCtx = fail_context(st1)
            {is, st} = make_succeeded(dst, failCtx, st1)
            {[i | is], st}

          true ->
            {[i], st1}
        end
    end
  end

  defp bif_is_record_cg(dst, tuple, tagVal, arityVal, st0) do
    {arity, st1} = new_ssa_var(:"@ssa_arity", st0)
    {tag, st2} = new_ssa_var(:"@ssa_tag", st1)
    {phi, st3} = new_label(st2)
    {false__, st4} = new_label(st3)
    {is0, st5} = make_cond_branch({:bif, :is_tuple}, [tuple], false__, st4)
    getArity = r_b_set(op: {:bif, :tuple_size}, dst: arity, args: [tuple])
    {is1, st6} = make_cond_branch({:bif, :"=:="}, [arity, arityVal], false__, st5)
    getTag = r_b_set(op: :get_tuple_element, dst: tag, args: [tuple, r_b_literal(val: 0)])
    {is2, st} = make_cond_branch({:bif, :"=:="}, [tag, tagVal], false__, st6)

    is3 = [
      r_cg_break(args: [r_b_literal(val: true)], phi: phi),
      {:label, false__},
      r_cg_break(args: [r_b_literal(val: false)], phi: phi),
      {:label, phi},
      r_cg_phi(vars: [dst])
    ]

    is = is0 ++ [getArity] ++ is1 ++ [getTag] ++ is2 ++ is3
    {is, st}
  end

  defp try_cg(ta, vs, tb, evs, th, rs, st0) do
    {b, st1} = new_label(st0)
    {h, st2} = new_label(st1)
    {e, st3} = new_label(st2)
    {next, st4} = new_label(st3)
    {tryTag, st5} = new_ssa_var(:"@ssa_catch_tag", st4)
    {ssaVs, st6} = new_ssa_vars(vs, st5)
    {ssaEvs, st7} = new_ssa_vars(evs, st6)
    {ais, st8} = cg(ta, r_cg(st7, break: b, catch_label: h))

    case {vs, tb, th, is_guard_cg_safe_list(ais)} do
      {[r_k_var(name: x)], r_k_break(args: [r_k_var(name: x)]), r_k_break(args: [r_k_literal()]),
       true} ->
        {protIs, st9} = guard_cg(ta, h, r_cg(st7, break: b, bfail: h))
        {his, st10} = cg(th, st9)
        {retVars, st} = new_ssa_vars(rs, st10)
        is = protIs ++ [{:label, h}] ++ his ++ [{:label, b}, r_cg_phi(vars: retVars)]
        {is, r_cg(st, break: r_cg(st0, :break), bfail: r_cg(st7, :bfail))}

      {[r_k_var(name: x)], r_k_break(args: [r_k_literal() = succLit0, r_k_var(name: x)]),
       r_k_break(args: [r_k_literal(val: false), r_k_literal()]), true} ->
        {finalLabel, st9} = new_label(st7)
        {protIs, st10} = guard_cg(ta, h, r_cg(st9, break: b, bfail: h))
        {his, st11} = cg(th, r_cg(st10, break: finalLabel))
        {retVars, st12} = new_ssa_vars(rs, st11)
        {result, st} = new_ssa_var(:"@ssa_result", st12)
        succLit = ssa_arg(succLit0, st)

        is =
          protIs ++
            [{:label, h}] ++
            his ++
            [
              {:label, b},
              r_cg_phi(vars: [result]),
              r_cg_break(
                args: [succLit, result],
                phi: finalLabel
              ),
              {:label, finalLabel},
              r_cg_phi(vars: retVars)
            ]

        {is, r_cg(st, break: r_cg(st0, :break), bfail: r_cg(st7, :bfail))}

      {_, r_k_break(args: []), r_k_break(args: []), true} ->
        {protIs, st9} = guard_cg(ta, h, r_cg(st7, break: b, bfail: h))
        {his, st10} = cg(th, st9)
        {retVars, st} = new_ssa_vars(rs, st10)
        is = protIs ++ [{:label, h}] ++ his ++ [{:label, b}, r_cg_phi(vars: retVars)]
        {is, r_cg(st, break: r_cg(st0, :break), bfail: r_cg(st7, :bfail))}

      {_, _, _, _} ->
        st9 =
          r_cg(st8,
            break: e,
            catch_label: r_cg(st7, :catch_label)
          )

        {bis, st10} = cg(tb, st9)
        {his, st11} = cg(th, st10)
        {breakVars, st12} = new_ssa_vars(rs, st11)
        {catchedAgg, st13} = new_ssa_var(:"@ssa_agg", st12)
        extractVs = extract_vars(ssaEvs, catchedAgg, 0)
        killTryTag = r_b_set(op: :kill_try_tag, args: [tryTag])
        args = [r_b_literal(val: :try), tryTag]

        handler =
          [{:label, h}, r_b_set(op: :landingpad, dst: catchedAgg, args: args)] ++
            extractVs ++ [killTryTag]

        {[
           r_b_set(op: :new_try_tag, dst: tryTag, args: [r_b_literal(val: :try)]),
           r_b_br(bool: tryTag, succ: next, fail: h),
           {:label, next}
         ] ++
           ais ++
           [{:label, b}, r_cg_phi(vars: ssaVs), killTryTag] ++
           bis ++ handler ++ his ++ [{:label, e}, r_cg_phi(vars: breakVars)],
         r_cg(st13, break: r_cg(st0, :break))}
    end
  end

  defp is_guard_cg_safe_list(is) do
    all(&is_guard_cg_safe/1, is)
  end

  defp is_guard_cg_safe(r_b_set(op: :call, args: args)) do
    case args do
      [
        r_b_remote(mod: r_b_literal(val: :erlang), name: r_b_literal(val: :error), arity: 1)
        | _
      ] ->
        true

      _ ->
        false
    end
  end

  defp is_guard_cg_safe(r_b_set() = i) do
    not :beam_ssa.clobbers_xregs(i)
  end

  defp is_guard_cg_safe(r_b_br()) do
    true
  end

  defp is_guard_cg_safe(r_b_switch()) do
    true
  end

  defp is_guard_cg_safe(r_cg_break()) do
    true
  end

  defp is_guard_cg_safe(r_cg_phi()) do
    true
  end

  defp is_guard_cg_safe({:label, _}) do
    true
  end

  defp is_guard_cg_safe(r_cg_unreachable()) do
    false
  end

  defp try_enter_cg(ta, vs, tb, evs, th, st0) do
    {b, st1} = new_label(st0)
    {h, st2} = new_label(st1)
    {next, st3} = new_label(st2)
    {tryTag, st4} = new_ssa_var(:"@ssa_catch_tag", st3)
    {ssaVs, st5} = new_ssa_vars(vs, st4)
    {ssaEvs, st6} = new_ssa_vars(evs, st5)
    {ais, st7} = cg(ta, r_cg(st6, break: b, catch_label: h))
    st8 = r_cg(st7, catch_label: r_cg(st6, :catch_label))
    {bis, st9} = cg(tb, st8)
    {his, st10} = cg(th, st9)
    {catchedAgg, st} = new_ssa_var(:"@ssa_agg", st10)
    extractVs = extract_vars(ssaEvs, catchedAgg, 0)
    killTryTag = r_b_set(op: :kill_try_tag, args: [tryTag])
    args = [r_b_literal(val: :try), tryTag]

    handler =
      [{:label, h}, r_b_set(op: :landingpad, dst: catchedAgg, args: args)] ++
        extractVs ++ [killTryTag]

    {[
       r_b_set(op: :new_try_tag, dst: tryTag, args: [r_b_literal(val: :try)]),
       r_b_br(bool: tryTag, succ: next, fail: h),
       {:label, next}
     ] ++ ais ++ [{:label, b}, r_cg_phi(vars: ssaVs), killTryTag] ++ bis ++ handler ++ his,
     r_cg(st, break: r_cg(st0, :break))}
  end

  defp extract_vars([v | vs], agg, n) do
    i = r_b_set(op: :extract, dst: v, args: [agg, r_b_literal(val: n)])
    [i | extract_vars(vs, agg, n + 1)]
  end

  defp extract_vars([], _, _) do
    []
  end

  defp do_catch_cg(block, r_k_var(name: r), st0) do
    {b, st1} = new_label(st0)
    {next, st2} = new_label(st1)
    {h, st3} = new_label(st2)
    {catchReg, st4} = new_ssa_var(:"@ssa_catch_tag", st3)
    {dst, st5} = new_ssa_var(r, st4)
    {succ, st6} = new_label(st5)

    {cis, st7} =
      cg(
        block,
        r_cg(st6, break: succ, catch_label: h)
      )

    {catchedVal, st8} = new_ssa_var(:"@catched_val", st7)
    {succVal, st9} = new_ssa_var(:"@success_val", st8)
    {catchedAgg, st10} = new_ssa_var(:"@ssa_agg", st9)
    {catchEndVal, st} = new_ssa_var(:"@catch_end_val", st10)
    args = [r_b_literal(val: :catch), catchReg]

    {[
       r_b_set(op: :new_try_tag, dst: catchReg, args: [r_b_literal(val: :catch)]),
       r_b_br(bool: catchReg, succ: next, fail: h),
       {:label, next}
     ] ++
       cis ++
       [
         {:label, h},
         r_b_set(op: :landingpad, dst: catchedAgg, args: args),
         r_b_set(op: :extract, dst: catchedVal, args: [catchedAgg, r_b_literal(val: 0)]),
         r_cg_break(args: [catchedVal], phi: b),
         {:label, succ},
         r_cg_phi(vars: [succVal]),
         r_cg_break(args: [succVal], phi: b),
         {:label, b},
         r_cg_phi(vars: [catchEndVal]),
         r_b_set(op: :catch_end, dst: dst, args: [catchReg, catchEndVal])
       ],
     r_cg(st,
       break: r_cg(st1, :break),
       catch_label: r_cg(st1, :catch_label)
     )}
  end

  defp put_cg([r_k_var(name: r)], r_k_cons(hd: hd, tl: tl), _Le, st0) do
    args = ssa_args([hd, tl], st0)
    {dst, st} = new_ssa_var(r, st0)
    putList = r_b_set(op: :put_list, dst: dst, args: args)
    {[putList], st}
  end

  defp put_cg([r_k_var(name: r)], r_k_tuple(es: es), _Le, st0) do
    {ret, st} = new_ssa_var(r, st0)
    args = ssa_args(es, st)
    putTuple = r_b_set(op: :put_tuple, dst: ret, args: args)
    {[putTuple], st}
  end

  defp put_cg([r_k_var(name: r)], r_k_binary(segs: segs), le, st0) do
    failCtx = fail_context(st0)
    {dst, st1} = new_ssa_var(r, st0)
    cg_binary(dst, segs, failCtx, le, st1)
  end

  defp put_cg(
         [r_k_var(name: r)],
         r_k_map(op: op, var: map, es: [r_k_map_pair(key: r_k_var() = k, val: v)]),
         le,
         st0
       ) do
    srcMap = ssa_arg(map, st0)
    lineAnno = line_anno(le)
    list = [ssa_arg(k, st0), ssa_arg(v, st0)]
    {dst, st1} = new_ssa_var(r, st0)
    {is, st} = put_cg_map(lineAnno, op, srcMap, dst, list, st1)
    {is, st}
  end

  defp put_cg([r_k_var(name: r)], r_k_map(op: op, var: map, es: es), le, st0) do
    [] =
      for r_k_map_pair(key: r_k_var() = var) <- es do
        var
      end

    srcMap = ssa_arg(map, st0)
    lineAnno = line_anno(le)

    list =
      flatmap(
        fn r_k_map_pair(key: k, val: v) ->
          [ssa_arg(k, st0), ssa_arg(v, st0)]
        end,
        es
      )

    {dst, st1} = new_ssa_var(r, st0)
    {is, st} = put_cg_map(lineAnno, op, srcMap, dst, list, st1)
    {is, st}
  end

  defp put_cg([r_k_var(name: r)], con0, _Le, st0) do
    con = ssa_arg(con0, st0)
    st = set_ssa_var(r, con, st0)
    {[], st}
  end

  defp put_cg_map(lineAnno, op, srcMap, dst, list, st0) do
    args = [[r_b_literal(val: op), srcMap] | list]
    putMap = r_b_set(anno: lineAnno, op: :put_map, dst: dst, args: args)

    cond do
      op === :assoc ->
        {[putMap], st0}

      true ->
        failCtx = fail_context(st0)
        {is, st} = make_succeeded(dst, failCtx, st0)
        {[putMap | is], st}
    end
  end

  defp cg_binary(dst, segs0, failCtx, le, st0) do
    {putCode0, szCalc0, st1} = cg_bin_put(segs0, failCtx, st0)
    lineAnno = line_anno(le)
    anno = le

    case putCode0 do
      [
        [
          r_b_set(op: :bs_put, dst: bool, args: [[_, _, src, r_b_literal(val: :all)] | _]),
          r_b_br(bool: bool),
          {:label, _}
        ]
        | _
      ] ->
        r_k_bin_seg(unit: unit0, next: segs) = segs0
        unit = r_b_literal(val: unit0)
        {putCode, szCalc1, st2} = cg_bin_put(segs, failCtx, st1)
        {_, szVar, szCode0, st3} = cg_size_calc(1, szCalc1, failCtx, st2)
        szCode = cg_bin_anno(szCode0, lineAnno)

        args =
          case member(:single_use, anno) do
            true ->
              [r_b_literal(val: :private_append), src, szVar, unit]

            false ->
              [r_b_literal(val: :append), src, szVar, unit]
          end

        bsInit = r_b_set(anno: lineAnno, op: :bs_init, dst: dst, args: args)
        {testIs, st} = make_succeeded(dst, failCtx, st3)
        {szCode ++ [bsInit] ++ testIs ++ putCode, st}

      [r_b_set(op: :bs_put) | _] ->
        {unit, szVar, szCode0, st2} = cg_size_calc(8, szCalc0, failCtx, st1)
        szCode = cg_bin_anno(szCode0, lineAnno)
        args = [r_b_literal(val: :new), szVar, unit]
        bsInit = r_b_set(anno: lineAnno, op: :bs_init, dst: dst, args: args)
        {testIs, st} = make_succeeded(dst, failCtx, st2)
        {szCode ++ [bsInit] ++ testIs ++ putCode0, st}
    end
  end

  defp cg_bin_anno([set | sets], anno) do
    [r_b_set(set, anno: anno) | sets]
  end

  defp cg_bin_anno([], _) do
    []
  end

  defp cg_size_calc(unit, :error, _FailCtx, st) do
    {r_b_literal(val: unit), r_b_literal(val: :badarg), [], st}
  end

  defp cg_size_calc(8, [{1, _} | _] = szCalc, failCtx, st) do
    cg_size_calc(1, szCalc, failCtx, st)
  end

  defp cg_size_calc(8, szCalc, failCtx, st0) do
    {var, pre, st} = cg_size_calc_1(szCalc, failCtx, st0)
    {r_b_literal(val: 8), var, pre, st}
  end

  defp cg_size_calc(1, szCalc0, failCtx, st0) do
    szCalc =
      map(
        fn
          {8, r_b_literal(val: size)} ->
            {1, r_b_literal(val: 8 * size)}

          {8, {{:bif, :byte_size}, src}} ->
            {1, {{:bif, :bit_size}, src}}

          {8, {_, _} = utfCalc} ->
            {1, {:*, r_b_literal(val: 8), utfCalc}}

          {_, _} = pair ->
            pair
        end,
        szCalc0
      )

    {var, pre, st} = cg_size_calc_1(szCalc, failCtx, st0)
    {r_b_literal(val: 1), var, pre, st}
  end

  defp cg_size_calc_1(szCalc, failCtx, st0) do
    cg_size_calc_2(szCalc, r_b_literal(val: 0), failCtx, st0)
  end

  defp cg_size_calc_2([{_, {:*, unit, {_, _} = bif}} | t], sum0, failCtx, st0) do
    {sum1, pre0, st1} = cg_size_calc_2(t, sum0, failCtx, st0)
    {bifDst, pre1, st2} = cg_size_bif(bif, failCtx, st1)
    {sum, pre2, st} = cg_size_add(sum1, bifDst, unit, failCtx, st2)
    {sum, pre0 ++ pre1 ++ pre2, st}
  end

  defp cg_size_calc_2([{_, r_b_literal() = sz} | t], sum0, failCtx, st0) do
    {sum1, pre0, st1} = cg_size_calc_2(t, sum0, failCtx, st0)
    {sum, pre, st} = cg_size_add(sum1, sz, r_b_literal(val: 1), failCtx, st1)
    {sum, pre0 ++ pre, st}
  end

  defp cg_size_calc_2([{_, r_b_var() = sz} | t], sum0, failCtx, st0) do
    {sum1, pre0, st1} = cg_size_calc_2(t, sum0, failCtx, st0)
    {sum, pre, st} = cg_size_add(sum1, sz, r_b_literal(val: 1), failCtx, st1)
    {sum, pre0 ++ pre, st}
  end

  defp cg_size_calc_2([{_, {_, _} = bif} | t], sum0, failCtx, st0) do
    {sum1, pre0, st1} = cg_size_calc_2(t, sum0, failCtx, st0)
    {bifDst, pre1, st2} = cg_size_bif(bif, failCtx, st1)
    {sum, pre2, st} = cg_size_add(sum1, bifDst, r_b_literal(val: 1), failCtx, st2)
    {sum, pre0 ++ pre1 ++ pre2, st}
  end

  defp cg_size_calc_2([], sum, _FailCtx, st) do
    {sum, [], st}
  end

  defp cg_size_bif(r_b_var() = var, _FailCtx, st) do
    {var, [], st}
  end

  defp cg_size_bif({name, src}, failCtx, st0) do
    {dst, st1} = new_ssa_var(:"@ssa_bif", st0)
    bif = r_b_set(op: name, dst: dst, args: [src])
    {testIs, st} = make_succeeded(dst, failCtx, st1)
    {dst, [bif | testIs], st}
  end

  defp cg_size_add(r_b_literal(val: 0), val, r_b_literal(val: 1), _FailCtx, st) do
    {val, [], st}
  end

  defp cg_size_add(a, b, unit, failCtx, st0) do
    {dst, st1} = new_ssa_var(:"@ssa_sum", st0)
    {testIs, st} = make_succeeded(dst, failCtx, st1)
    bsAdd = r_b_set(op: :bs_add, dst: dst, args: [a, b, unit])
    {dst, [bsAdd | testIs], st}
  end

  defp cg_bin_put(seg, failCtx, st) do
    cg_bin_put_1(seg, failCtx, [], [], st)
  end

  defp cg_bin_put_1(
         r_k_bin_seg(size: size0, unit: u, type: t, flags: fs, seg: src0, next: next),
         failCtx,
         acc,
         szCalcAcc,
         st0
       ) do
    [src, size] = ssa_args([src0, size0], st0)
    needSize = bs_need_size(t)
    typeArg = r_b_literal(val: t)
    flags = r_b_literal(val: fs)
    unit = r_b_literal(val: u)

    args =
      case needSize do
        true ->
          [typeArg, flags, src, size, unit]

        false ->
          [typeArg, flags, src]
      end

    {_, failLbl} = failCtx
    {is, st} = make_cond_branch(:bs_put, args, failLbl, st0)
    szCalc = bin_size_calc(t, src, size, u)
    cg_bin_put_1(next, failCtx, reverse(is, acc), [szCalc | szCalcAcc], st)
  end

  defp cg_bin_put_1(r_k_bin_end(), _, acc, szCalcAcc, st) do
    szCalc = fold_size_calc(szCalcAcc, 0, [])
    {reverse(acc), szCalc, st}
  end

  defp bs_need_size(:utf8) do
    false
  end

  defp bs_need_size(:utf16) do
    false
  end

  defp bs_need_size(:utf32) do
    false
  end

  defp bs_need_size(_) do
    true
  end

  defp bin_size_calc(:utf8, src, _Size, _Unit) do
    {8, {:bs_utf8_size, src}}
  end

  defp bin_size_calc(:utf16, src, _Size, _Unit) do
    {8, {:bs_utf16_size, src}}
  end

  defp bin_size_calc(:utf32, _Src, _Size, _Unit) do
    {8, r_b_literal(val: 4)}
  end

  defp bin_size_calc(:binary, src, r_b_literal(val: :all), unit) do
    case rem(unit, 8) do
      0 ->
        {8, {{:bif, :byte_size}, src}}

      _ ->
        {1, {{:bif, :bit_size}, src}}
    end
  end

  defp bin_size_calc(_Type, _Src, size, unit) do
    {unit, size}
  end

  defp fold_size_calc([{unit, r_b_literal(val: size)} | t], bits, acc) do
    cond do
      is_integer(size) ->
        fold_size_calc(t, bits + unit * size, acc)

      true ->
        :error
    end
  end

  defp fold_size_calc([{u, r_b_var()} = h | t], bits, acc)
       when u === 1 or
              u === 8 do
    fold_size_calc(t, bits, [h | acc])
  end

  defp fold_size_calc([{u, r_b_var() = var} | t], bits, acc) do
    fold_size_calc(t, bits, [{1, {:*, r_b_literal(val: u), var}} | acc])
  end

  defp fold_size_calc([{_, _} = h | t], bits, acc) do
    fold_size_calc(t, bits, [h | acc])
  end

  defp fold_size_calc([], bits, acc) do
    bytes = div(bits, 8)
    remBits = rem(bits, 8)

    sizes =
      sort([
        [{1, r_b_literal(val: remBits)}, {8, r_b_literal(val: bytes)}]
        | acc
      ])

    for {_, sz} = pair <- sizes, sz !== r_b_literal(val: 0) do
      pair
    end
  end

  defp ssa_args(as, st) do
    for a <- as do
      ssa_arg(a, st)
    end
  end

  defp ssa_arg(r_k_var(name: v), r_cg(vars: vars)) do
    :erlang.map_get(v, vars)
  end

  defp ssa_arg(r_k_literal(val: v), _) do
    r_b_literal(val: v)
  end

  defp ssa_arg(r_k_remote(mod: mod0, name: name0, arity: arity), st) do
    mod = ssa_arg(mod0, st)
    name = ssa_arg(name0, st)
    r_b_remote(mod: mod, name: name, arity: arity)
  end

  defp ssa_arg(r_k_local(name: name, arity: arity), _)
       when is_atom(name) do
    r_b_local(name: r_b_literal(val: name), arity: arity)
  end

  defp new_succeeded_value(varBase, var, r_cg(vars: vars0) = st) do
    vars = %{vars0 | varBase => {:succeeded, var}}
    r_cg(st, vars: vars)
  end

  defp new_bool_value(varBase, var, r_cg(vars: vars0) = st) do
    vars = %{vars0 | varBase => {:bool, var}}
    r_cg(st, vars: vars)
  end

  defp new_ssa_vars(vs, st) do
    mapfoldl(
      fn r_k_var(name: v), s ->
        new_ssa_var(v, s)
      end,
      st,
      vs
    )
  end

  defp new_ssa_var(varBase, r_cg(lcount: uniq, vars: vars) = st0)
       when is_atom(varBase) or is_integer(varBase) do
    case vars do
      %{^varBase => _} ->
        var = r_b_var(name: {varBase, uniq})

        st =
          r_cg(st0,
            lcount: uniq + 1,
            vars: %{vars | varBase => var}
          )

        {var, st}

      %{} ->
        var = r_b_var(name: varBase)
        st = r_cg(st0, vars: %{vars | varBase => var})
        {var, st}
    end
  end

  defp set_unused_ssa_vars(vars, st) do
    foldl(
      fn r_k_var(name: v), s ->
        set_ssa_var(v, r_b_literal(val: :unused), s)
      end,
      st,
      vars
    )
  end

  defp set_ssa_var(varBase, val, r_cg(vars: vars) = st)
       when is_atom(varBase) or is_integer(varBase) do
    r_cg(st, vars: %{vars | varBase => val})
  end

  defp new_label(r_cg(lcount: next) = st) do
    {next, r_cg(st, lcount: next + 1)}
  end

  defp line_anno([line, {:file, name}]) when is_integer(line) do
    line_anno_1(name, line)
  end

  defp line_anno([_ | _] = a) do
    {name, line} = find_loc(a, :no_file, 0)
    line_anno_1(name, line)
  end

  defp line_anno([]) do
    %{}
  end

  defp line_anno_1(:no_file, _) do
    %{}
  end

  defp line_anno_1(_, 0) do
    %{}
  end

  defp line_anno_1(name, line) do
    %{:location => {name, line}}
  end

  defp find_loc([line | t], file, _) when is_integer(line) do
    find_loc(t, file, line)
  end

  defp find_loc([{:file, file} | t], _, line) do
    find_loc(t, file, line)
  end

  defp find_loc([_ | t], file, line) do
    find_loc(t, file, line)
  end

  defp find_loc([], file, line) do
    {file, line}
  end

  defp flatmapfoldl(f, accu0, [hd | tail]) do
    {r, accu1} = f.(hd, accu0)
    {rs, accu2} = flatmapfoldl(f, accu1, tail)
    {r ++ rs, accu2}
  end

  defp flatmapfoldl(_, accu, []) do
    {[], accu}
  end

  defp finalize(asm0, st0) do
    asm1 = fix_phis(asm0)
    {asm, st} = fix_sets(asm1, [], st0)
    {build_map(asm), st}
  end

  defp fix_phis(is) do
    fix_phis_1(is, :none, %{})
  end

  defp fix_phis_1([[{:label, lbl}, r_cg_phi(vars: vars)] | is0], _Lbl, map0) do
    case map0 do
      %{^lbl => pairs} ->
        phis = gen_phis(vars, pairs)
        map = :maps.remove(lbl, map0)
        [{:label, lbl}] ++ phis ++ fix_phis_1(is0, lbl, map)

      %{} ->
        is = drop_upto_label(is0)
        fix_phis_1(is, :none, map0)
    end
  end

  defp fix_phis_1([{:label, l} = i | is], _Lbl, map) do
    [i | fix_phis_1(is, l, map)]
  end

  defp fix_phis_1([r_cg_unreachable() | is0], _Lbl, map) do
    is = drop_upto_label(is0)
    fix_phis_1(is, :none, map)
  end

  defp fix_phis_1([r_cg_break(args: args, phi: target) | is], lbl, map)
       when is_integer(lbl) do
    pairs1 =
      case map do
        %{^target => pairs0} ->
          pairs0

        %{} ->
          []
      end

    pairs = [
      for arg <- args do
        {arg, lbl}
      end
      | pairs1
    ]

    i = make_uncond_branch(target)
    [i | fix_phis_1(is, :none, %{map | target => pairs})]
  end

  defp fix_phis_1([i | is], lbl, map) do
    [i | fix_phis_1(is, lbl, map)]
  end

  defp fix_phis_1([], _, map) do
    [] = :maps.to_list(map)
    []
  end

  defp gen_phis([v | vs], preds0) do
    {pairs, preds} = collect_preds(preds0, [], [])
    [_ | _] = pairs
    [r_b_set(op: :phi, dst: v, args: pairs) | gen_phis(vs, preds)]
  end

  defp gen_phis([], _) do
    []
  end

  defp collect_preds([[first | rest] | t], colAcc, restAcc) do
    collect_preds(t, [first | colAcc], [rest | restAcc])
  end

  defp collect_preds([], colAcc, restAcc) do
    {keysort(2, colAcc), restAcc}
  end

  defp drop_upto_label([{:label, _} | _] = is) do
    is
  end

  defp drop_upto_label([_ | is]) do
    drop_upto_label(is)
  end

  defp fix_sets(
         [
           [r_b_set(op: op, dst: dst) = set, r_b_ret(arg: dst) = ret]
           | is
         ],
         acc,
         st
       ) do
    noValue =
      case op do
        :remove_message ->
          true

        :timeout ->
          true

        _ ->
          false
      end

    case noValue do
      true ->
        fix_sets(is, [[r_b_ret(ret, arg: r_b_literal(val: :ok)), set] | acc], st)

      false ->
        fix_sets(is, [[ret, set] | acc], st)
    end
  end

  defp fix_sets([r_b_set(dst: :none) = set | is], acc, st0) do
    {dst, st} = new_ssa_var(:"@ssa_ignored", st0)
    i = r_b_set(set, dst: dst)
    fix_sets(is, [i | acc], st)
  end

  defp fix_sets([i | is], acc, st) do
    fix_sets(is, [i | acc], st)
  end

  defp fix_sets([], acc, st) do
    {reverse(acc), st}
  end

  defp build_map(is) do
    linear0 = build_graph_1(is, [], [])
    linear = :beam_ssa.trim_unreachable(linear0)
    :maps.from_list(linear)
  end

  defp build_graph_1([{:label, l} | is], lbls, []) do
    build_graph_1(is, [l | lbls], [])
  end

  defp build_graph_1([{:label, l} | is], lbls, [_ | _] = blockAcc) do
    make_blocks(lbls, blockAcc) ++ build_graph_1(is, [l], [])
  end

  defp build_graph_1([i | is], lbls, blockAcc) do
    build_graph_1(is, lbls, [i | blockAcc])
  end

  defp build_graph_1([], lbls, blockAcc) do
    make_blocks(lbls, blockAcc)
  end

  defp make_blocks(lbls, [last0 | is0]) do
    is = reverse(is0)
    last = :beam_ssa.normalize(last0)
    block = r_b_blk(is: is, last: last)

    for l <- lbls do
      {l, block}
    end
  end
end
