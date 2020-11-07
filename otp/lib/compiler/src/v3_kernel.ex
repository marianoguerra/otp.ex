defmodule :m_v3_kernel do
  use Bitwise

  import :lists,
    only: [
      all: 2,
      droplast: 1,
      flatten: 1,
      foldl: 3,
      foldr: 3,
      keyfind: 3,
      keyreplace: 4,
      last: 1,
      map: 2,
      mapfoldl: 3,
      member: 2,
      partition: 2,
      reverse: 1,
      sort: 1,
      sort: 2,
      splitwith: 2
    ]

  import :ordsets, only: [add_element: 2, intersection: 2, subtract: 2, union: 1, union: 2]
  require Record
  Record.defrecord(:r_c_alias, :c_alias, anno: [], var: :undefined, pat: :undefined)
  Record.defrecord(:r_c_apply, :c_apply, anno: [], op: :undefined, args: :undefined)

  Record.defrecord(:r_c_binary, :c_binary,
    anno: [],
    segments: :undefined
  )

  Record.defrecord(:r_c_bitstr, :c_bitstr,
    anno: [],
    val: :undefined,
    size: :undefined,
    unit: :undefined,
    type: :undefined,
    flags: :undefined
  )

  Record.defrecord(:r_c_call, :c_call,
    anno: [],
    module: :undefined,
    name: :undefined,
    args: :undefined
  )

  Record.defrecord(:r_c_case, :c_case, anno: [], arg: :undefined, clauses: :undefined)

  Record.defrecord(:r_c_catch, :c_catch,
    anno: [],
    body: :undefined
  )

  Record.defrecord(:r_c_clause, :c_clause,
    anno: [],
    pats: :undefined,
    guard: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_c_cons, :c_cons, anno: [], hd: :undefined, tl: :undefined)
  Record.defrecord(:r_c_fun, :c_fun, anno: [], vars: :undefined, body: :undefined)

  Record.defrecord(:r_c_let, :c_let, anno: [], vars: :undefined, arg: :undefined, body: :undefined)

  Record.defrecord(:r_c_letrec, :c_letrec, anno: [], defs: :undefined, body: :undefined)

  Record.defrecord(:r_c_literal, :c_literal,
    anno: [],
    val: :undefined
  )

  Record.defrecord(:r_c_map, :c_map,
    anno: [],
    arg: :EFE_TODO_NESTED_RECORD,
    es: :undefined,
    is_pat: false
  )

  Record.defrecord(:r_c_map_pair, :c_map_pair,
    anno: [],
    op: :undefined,
    key: :undefined,
    val: :undefined
  )

  Record.defrecord(:r_c_module, :c_module,
    anno: [],
    name: :undefined,
    exports: :undefined,
    attrs: :undefined,
    defs: :undefined
  )

  Record.defrecord(:r_c_primop, :c_primop, anno: [], name: :undefined, args: :undefined)

  Record.defrecord(:r_c_receive, :c_receive,
    anno: [],
    clauses: :undefined,
    timeout: :undefined,
    action: :undefined
  )

  Record.defrecord(:r_c_seq, :c_seq, anno: [], arg: :undefined, body: :undefined)

  Record.defrecord(:r_c_try, :c_try,
    anno: [],
    arg: :undefined,
    vars: :undefined,
    body: :undefined,
    evars: :undefined,
    handler: :undefined
  )

  Record.defrecord(:r_c_tuple, :c_tuple, anno: [], es: :undefined)

  Record.defrecord(:r_c_values, :c_values,
    anno: [],
    es: :undefined
  )

  Record.defrecord(:r_c_var, :c_var, anno: [], name: :undefined)

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

  defp get_kanno(kthing) do
    :erlang.element(2, kthing)
  end

  defp set_kanno(kthing, anno) do
    :erlang.setelement(2, kthing, anno)
  end

  defp copy_anno(kdst, ksrc) do
    anno = get_kanno(ksrc)
    set_kanno(kdst, anno)
  end

  Record.defrecord(:r_ivalues, :ivalues,
    anno: [],
    args: :undefined
  )

  Record.defrecord(:r_ifun, :ifun, anno: [], vars: :undefined, body: :undefined)
  Record.defrecord(:r_iset, :iset, anno: [], vars: :undefined, arg: :undefined, body: :undefined)

  Record.defrecord(:r_iletrec, :iletrec,
    anno: [],
    defs: :undefined
  )

  Record.defrecord(:r_ialias, :ialias, anno: [], vars: :undefined, pat: :undefined)

  Record.defrecord(:r_iclause, :iclause,
    anno: [],
    isub: :undefined,
    osub: :undefined,
    pats: :undefined,
    guard: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_kern, :kern,
    func: :undefined,
    fargs: [],
    vcount: 0,
    fcount: 0,
    ds: :cerl_sets.new(),
    funs: [],
    free: %{},
    ws: [],
    no_shared_fun_wrappers: false,
    labels: :cerl_sets.new()
  )

  def module(
        r_c_module(anno: a, name: m, exports: es, attrs: as, defs: fs),
        options
      ) do
    kas = attributes(as)

    kes =
      map(
        fn r_c_var(name: {_, _} = fname) ->
          fname
        end,
        es
      )

    noSharedFunWrappers =
      :proplists.get_bool(
        :no_shared_fun_wrappers,
        options
      )

    st0 = r_kern(no_shared_fun_wrappers: noSharedFunWrappers)
    {kfs, st} = mapfoldl(&function/2, st0, fs)

    {:ok,
     r_k_mdef(
       anno: a,
       name: r_c_literal(m, :val),
       exports: kes,
       attributes: kas,
       body: kfs ++ r_kern(st, :funs)
     ), sort(r_kern(st, :ws))}
  end

  defp attributes([{r_c_literal(val: name), r_c_literal(val: val)} | as]) do
    case include_attribute(name) do
      false ->
        attributes(as)

      true ->
        [{name, val} | attributes(as)]
    end
  end

  defp attributes([]) do
    []
  end

  defp include_attribute(:type) do
    false
  end

  defp include_attribute(:spec) do
    false
  end

  defp include_attribute(:callback) do
    false
  end

  defp include_attribute(:opaque) do
    false
  end

  defp include_attribute(:export_type) do
    false
  end

  defp include_attribute(:record) do
    false
  end

  defp include_attribute(:optional_callbacks) do
    false
  end

  defp include_attribute(:file) do
    false
  end

  defp include_attribute(:compile) do
    false
  end

  defp include_attribute(_) do
    true
  end

  defp function({r_c_var(name: {f, arity} = fA), body}, st0) do
    try do
      count = :cerl_trees.next_free_variable_name(body)
      st1 = r_kern(st0, func: fA, vcount: count, fcount: 0, ds: :cerl_sets.new())
      {r_ifun(anno: ab, vars: kvs, body: b0), [], st2} = expr(body, new_sub(), st1)
      {b1, _, st3} = ubody(b0, :return, st2)
      {make_fdef(ab, f, arity, kvs, b1), st3}
    catch
      class, error ->
        :io.fwrite('Function: ~w/~w\n', [f, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  defp body(r_c_values(anno: a, es: ces), sub, st0) do
    {kes, pe, st1} = atomic_list(ces, sub, st0)
    {r_ivalues(anno: a, args: kes), pe, st1}
  end

  defp body(ce, sub, st0) do
    expr(ce, sub, st0)
  end

  defp guard(g0, sub, st0) do
    {ge0, pre, st1} = expr(g0, sub, st0)
    {ge, st} = gexpr_test(ge0, st1)
    {pre_seq(pre, ge), st}
  end

  defp gexpr_test(
         r_k_bif(
           anno: a,
           op:
             r_k_remote(mod: r_k_literal(val: :erlang), name: r_k_literal(val: f), arity: ar) = op,
           args: kargs
         ) = ke,
         st
       ) do
    case :erl_internal.new_type_test(
           f,
           ar
         ) or :erl_internal.comp_op(f, ar) do
      true ->
        {r_k_test(anno: a, op: op, args: kargs), st}

      false ->
        gexpr_test_add(ke, st)
    end
  end

  defp gexpr_test(
         r_k_try(
           arg: b0,
           vars: [r_k_var(name: x)],
           body: r_k_var(name: x),
           handler: r_k_literal(val: false)
         ) = try,
         st0
       ) do
    {b, st} = gexpr_test(b0, st0)
    {r_k_try(try, arg: b), st}
  end

  defp gexpr_test(r_iset(body: b0) = iset, st0) do
    {b1, st1} = gexpr_test(b0, st0)
    {r_iset(iset, body: b1), st1}
  end

  defp gexpr_test(ke, st) do
    gexpr_test_add(ke, st)
  end

  defp gexpr_test_add(ke, st0) do
    test = r_k_remote(mod: r_k_literal(val: :erlang), name: r_k_literal(val: :"=:="), arity: 2)
    {ae, ap, st1} = force_atomic(ke, st0)

    {pre_seq(
       ap,
       r_k_test(anno: get_kanno(ke), op: test, args: [ae, r_k_literal(val: true)])
     ), st1}
  end

  defp expr(r_c_var(anno: a0, name: {name, arity}) = fname, sub, st) do
    vs =
      for v <- integers(1, arity) do
        r_c_var(name: :erlang.list_to_atom('V' ++ :erlang.integer_to_list(v)))
      end

    case r_kern(st, :no_shared_fun_wrappers) do
      false ->
        wrapper0 = ['-fun.', :erlang.atom_to_list(name), '/', :erlang.integer_to_list(arity), '-']
        wrapper = :erlang.list_to_atom(flatten(wrapper0))
        id = {:id, {0, 0, wrapper}}
        a = keyreplace(:id, 1, a0, id)
        fun = r_c_fun(anno: a, vars: vs, body: r_c_apply(anno: a, op: fname, args: vs))
        expr(fun, sub, st)

      true ->
        fun = r_c_fun(anno: a0, vars: vs, body: r_c_apply(anno: a0, op: fname, args: vs))
        expr(fun, sub, st)
    end
  end

  defp expr(r_c_var(anno: a, name: v), sub, st) do
    {r_k_var(anno: a, name: get_vsub(v, sub)), [], st}
  end

  defp expr(r_c_literal(anno: a, val: v), _Sub, st) do
    {r_k_literal(anno: a, val: v), [], st}
  end

  defp expr(r_c_cons(anno: a, hd: ch, tl: ct), sub, st0) do
    {kh0, hp0, st1} = expr(ch, sub, st0)
    {kt0, tp0, st2} = expr(ct, sub, st1)
    {kt1, tp1, st3} = force_atomic(kt0, st2)
    {kh1, hp1, st4} = force_atomic(kh0, st3)
    {r_k_cons(anno: a, hd: kh1, tl: kt1), hp0 ++ tp0 ++ tp1 ++ hp1, st4}
  end

  defp expr(r_c_tuple(anno: a, es: ces), sub, st0) do
    {kes, ep, st1} = atomic_list(ces, sub, st0)
    {r_k_tuple(anno: a, es: kes), ep, st1}
  end

  defp expr(r_c_map(anno: a, arg: var, es: ces), sub, st0) do
    expr_map(a, var, ces, sub, st0)
  end

  defp expr(r_c_binary(anno: a, segments: cv), sub, st0) do
    try do
      atomic_bin(cv, sub, st0)
    catch
      :bad_element_size ->
        st1 = add_warning(get_line(a), :bad_segment_size, a, st0)
        erl = r_c_literal(val: :erlang)
        name = r_c_literal(val: :error)
        args = [r_c_literal(val: :badarg)]
        error = r_c_call(anno: a, module: erl, name: name, args: args)
        expr(error, sub, st1)
    else
      {kv, ep, st1} ->
        {r_k_binary(anno: a, segs: kv), ep, st1}
    end
  end

  defp expr(r_c_fun(anno: a, vars: cvs, body: cb), sub0, r_kern(fargs: oldFargs) = st0) do
    {kvs, sub1, st1} = pattern_list(cvs, sub0, st0)
    {kb, pb, st2} = body(cb, sub1, r_kern(st1, fargs: kvs))
    {r_ifun(anno: a, vars: kvs, body: pre_seq(pb, kb)), [], r_kern(st2, fargs: oldFargs)}
  end

  defp expr(r_c_seq(arg: ca, body: cb), sub, st0) do
    {ka, pa, st1} = body(ca, sub, st0)
    {kb, pb, st2} = body(cb, sub, st1)
    {kb, pa ++ [ka] ++ pb, st2}
  end

  defp expr(r_c_let(anno: a, vars: cvs, arg: ca, body: cb), sub0, st0) do
    {ka, pa, st1} = body(ca, sub0, st0)
    {kps, sub1, st2} = pattern_list(cvs, sub0, st1)

    sets =
      case ka do
        r_ivalues(args: kas) ->
          foldr2(
            fn v, val, sb ->
              [r_iset(vars: [v], arg: val) | sb]
            end,
            [],
            kps,
            kas
          )

        _Other ->
          [r_iset(anno: a, vars: kps, arg: ka)]
      end

    {kb, pb, st3} = body(cb, sub1, st2)
    {kb, pa ++ sets ++ pb, st3}
  end

  defp expr(r_c_letrec(anno: a, defs: cfs, body: cb), sub, st) do
    case member(:letrec_goto, a) do
      true ->
        letrec_goto(cfs, cb, sub, st)

      false ->
        letrec_local_function(a, cfs, cb, sub, st)
    end
  end

  defp expr(r_c_case(arg: ca, clauses: ccs), sub, st0) do
    {ka, pa, st1} = body(ca, sub, st0)
    {kvs, pv, st2} = match_vars(ka, st1)
    {km, st3} = kmatch(kvs, ccs, sub, st2)
    match = flatten_seq(build_match(km))
    {last(match), pa ++ pv ++ droplast(match), st3}
  end

  defp expr(r_c_apply(anno: a, op: cop, args: cargs), sub, st) do
    c_apply(a, cop, cargs, sub, st)
  end

  defp expr(r_c_call(anno: a, module: m0, name: f0, args: cargs), sub, st0) do
    ar = length(cargs)

    {[[m, f] | kargs], ap, st1} =
      atomic_list(
        [
          [m0, f0]
          | cargs
        ],
        sub,
        st0
      )

    remote = r_k_remote(mod: m, name: f, arity: ar)

    case call_type(m0, f0, cargs) do
      :bif ->
        {r_k_bif(anno: a, op: remote, args: kargs), ap, st1}

      :call ->
        {r_k_call(anno: a, op: remote, args: kargs), ap, st1}

      :error ->
        st = add_warning(get_line(a), :bad_call, a, st0)

        call =
          r_c_call(
            anno: a,
            module: r_c_literal(val: :erlang),
            name: r_c_literal(val: :apply),
            args: [m0, f0, :cerl.make_list(cargs)]
          )

        expr(call, sub, st)
    end
  end

  defp expr(r_c_primop(anno: a, name: r_c_literal(val: :match_fail), args: [arg]), sub, st) do
    translate_match_fail(arg, sub, a, st)
  end

  defp expr(r_c_primop(anno: a, name: r_c_literal(val: n), args: cargs), sub, st0) do
    {kargs, ap, st1} = atomic_list(cargs, sub, st0)
    ar = length(cargs)
    {r_k_bif(anno: a, op: r_k_internal(name: n, arity: ar), args: kargs), ap, st1}
  end

  defp expr(r_c_try(anno: a, arg: ca, vars: cvs, body: cb, evars: evs, handler: ch), sub0, st0) do
    {ka, pa, st1} = body(ca, sub0, st0)
    {kcvs, sub1, st2} = pattern_list(cvs, sub0, st1)
    {kb, pb, st3} = body(cb, sub1, st2)
    {kevs, sub2, st4} = pattern_list(evs, sub0, st3)
    {kh, ph, st5} = body(ch, sub2, st4)

    {r_k_try(
       anno: a,
       arg: pre_seq(pa, ka),
       vars: kcvs,
       body: pre_seq(pb, kb),
       evars: kevs,
       handler: pre_seq(ph, kh)
     ), [], st5}
  end

  defp expr(r_c_catch(anno: a, body: cb), sub, st0) do
    {kb, pb, st1} = body(cb, sub, st0)
    {r_k_catch(anno: a, body: pre_seq(pb, kb)), [], st1}
  end

  defp letrec_local_function(a, cfs, cb, sub0, st0) do
    {fs0, {sub1, st1}} =
      mapfoldl(
        fn {r_c_var(name: {f, ar}), b0}, {sub, s0} ->
          {n, st1} =
            new_fun_name(
              :erlang.atom_to_list(f) ++ '/' ++ :erlang.integer_to_list(ar),
              s0
            )

          b = set_kanno(b0, [{:letrec_name, n}])
          {{n, b}, {set_fsub(f, ar, n, sub), st1}}
        end,
        {sub0, st0},
        cfs
      )

    {fs1, st2} =
      mapfoldl(
        fn {n, fd0}, s1 ->
          {fd1, [], st2} = expr(fd0, sub1, s1)
          fd = set_kanno(fd1, a)
          {{n, fd}, st2}
        end,
        st1,
        fs0
      )

    {kb, pb, st3} = body(cb, sub1, st2)
    {kb, [r_iletrec(anno: a, defs: fs1) | pb], st3}
  end

  defp letrec_goto([{r_c_var(name: {label, 0}), cfail}], cb, sub0, r_kern(labels: labels0) = st0) do
    labels = :cerl_sets.add_element(label, labels0)
    {kb, pb, st1} = body(cb, sub0, r_kern(st0, labels: labels))
    r_c_fun(body: failBody) = cfail
    {kfail, fb, st2} = body(failBody, sub0, st1)

    case {kb, kfail, fb} do
      {r_k_goto(label: ^label), r_k_goto() = innerGoto, []} ->
        {innerGoto, pb, st2}

      {_, _, _} ->
        st3 = r_kern(st2, labels: labels0)
        alt = r_k_letrec_goto(label: label, first: kb, then: pre_seq(fb, kfail))
        {alt, pb, st3}
    end
  end

  defp translate_match_fail(arg, sub, anno, st0) do
    cargs =
      case {:cerl.data_type(arg), :cerl.data_es(arg)} do
        {:tuple, [r_c_literal(val: :function_clause) | as]} ->
          translate_fc_args(as, sub, st0)

        {_, _} ->
          [arg]
      end

    {kargs, ap, st} = atomic_list(cargs, sub, st0)
    ar = length(cargs)

    call =
      r_k_call(
        anno: anno,
        op: r_k_remote(mod: r_k_literal(val: :erlang), name: r_k_literal(val: :error), arity: ar),
        args: kargs
      )

    {call, ap, st}
  end

  defp translate_fc_args(as, sub, r_kern(fargs: fargs)) do
    case same_args(as, fargs, sub) do
      true ->
        [r_c_literal(val: :function_clause), :cerl.make_list(as)]

      false ->
        [:cerl.c_tuple([r_c_literal(val: :case_clause), :cerl.c_tuple(as)])]
    end
  end

  defp same_args([r_c_var(name: cv) | vs], [r_k_var(name: kv) | as], sub) do
    get_vsub(cv, sub) === kv and same_args(vs, as, sub)
  end

  defp same_args([], [], _Sub) do
    true
  end

  defp same_args(_, _, _) do
    false
  end

  defp expr_map(a, var0, ces, sub, st0) do
    {var, mps, st1} = expr(var0, sub, st0)
    {km, eps, st2} = map_split_pairs(a, var, ces, sub, st1)
    {km, eps ++ mps, st2}
  end

  defp map_split_pairs(a, var, ces, sub, st0) do
    pairs0 =
      for r_c_map_pair(op: r_c_literal(val: op), key: k, val: v) <- ces do
        {op, k, v}
      end

    {pairs, esp, st1} =
      foldr(
        fn {op, k0, v0}, {ops, espi, sti0}
           when op === :assoc or op === :exact ->
          {k, eps1, sti1} = atomic(k0, sub, sti0)
          {v, eps2, sti2} = atomic(v0, sub, sti1)
          {[{op, k, v} | ops], eps1 ++ eps2 ++ espi, sti2}
        end,
        {[], [], st0},
        pairs0
      )

    map_split_pairs_1(a, var, pairs, esp, st1)
  end

  defp map_split_pairs_1(a, map0, [{op, key, val} | pairs1] = pairs0, esp0, st0) do
    {map1, em, st1} = force_atomic(map0, st0)

    case key do
      r_k_var() ->
        kes = [r_k_map_pair(key: key, val: val)]
        map = r_k_map(anno: a, op: op, var: map1, es: kes)
        map_split_pairs_1(a, map, pairs1, esp0 ++ em, st1)

      _ ->
        {l, pairs} =
          splitwith(
            fn
              {_, r_k_var(), _} ->
                false

              {_, _, _} ->
                true
            end,
            pairs0
          )

        {map, esp, st2} = map_group_pairs(a, map1, l, esp0 ++ em, st1)
        map_split_pairs_1(a, map, pairs, esp, st2)
    end
  end

  defp map_split_pairs_1(_, map, [], esp, st0) do
    {map, esp, st0}
  end

  defp map_group_pairs(a, var, pairs0, esp, st0) do
    pairs = map_remove_dup_keys(pairs0)

    assoc =
      for {_, {:assoc, k, v}} <- pairs do
        r_k_map_pair(key: k, val: v)
      end

    exact =
      for {_, {:exact, k, v}} <- pairs do
        r_k_map_pair(key: k, val: v)
      end

    case {assoc, exact} do
      {[_ | _], []} ->
        {r_k_map(anno: a, op: :assoc, var: var, es: assoc), esp, st0}

      {[], [_ | _]} ->
        {r_k_map(anno: a, op: :exact, var: var, es: exact), esp, st0}

      {[_ | _], [_ | _]} ->
        map = r_k_map(anno: a, op: :assoc, var: var, es: assoc)
        {mvar, em, st1} = force_atomic(map, st0)
        {r_k_map(anno: a, op: :exact, var: mvar, es: exact), esp ++ em, st1}
    end
  end

  defp map_remove_dup_keys(es) do
    map_remove_dup_keys(es, %{})
  end

  defp map_remove_dup_keys([{:assoc, k0, v} | es0], used0) do
    k = map_key_clean(k0)

    op =
      case used0 do
        %{^k => {:exact, _, _}} ->
          :exact

        %{} ->
          :assoc
      end

    used1 = %{used0 | k => {op, k0, v}}
    map_remove_dup_keys(es0, used1)
  end

  defp map_remove_dup_keys([{:exact, k0, v} | es0], used0) do
    k = map_key_clean(k0)

    op =
      case used0 do
        %{^k => {:assoc, _, _}} ->
          :assoc

        %{} ->
          :exact
      end

    used1 = %{used0 | k => {op, k0, v}}
    map_remove_dup_keys(es0, used1)
  end

  defp map_remove_dup_keys([], used) do
    sort(:maps.to_list(used))
  end

  defp map_key_clean(r_k_var(name: v)) do
    {:var, v}
  end

  defp map_key_clean(r_k_literal(val: v)) do
    {:lit, v}
  end

  defp call_type(r_c_literal(val: m), r_c_literal(val: f), as)
       when is_atom(m) and
              is_atom(f) do
    case is_remote_bif(m, f, as) do
      false ->
        :call

      true ->
        :bif
    end
  end

  defp call_type(r_c_var(), r_c_literal(val: a), _) when is_atom(a) do
    :call
  end

  defp call_type(r_c_literal(val: a), r_c_var(), _) when is_atom(a) do
    :call
  end

  defp call_type(r_c_var(), r_c_var(), _) do
    :call
  end

  defp call_type(_, _, _) do
    :error
  end

  defp match_vars(r_ivalues(args: as), st) do
    foldr(
      fn ka, {vs, vsp, st0} ->
        {v, vp, st1} = force_variable(ka, st0)
        {[v | vs], vp ++ vsp, st1}
      end,
      {[], [], st},
      as
    )
  end

  defp match_vars(ka, st0) do
    {v, vp, st1} = force_variable(ka, st0)
    {[v], vp, st1}
  end

  defp c_apply(a, r_c_var(anno: ra, name: {f0, ar}), cargs, sub, r_kern(labels: labels) = st0) do
    case ar === 0 and :cerl_sets.is_element(f0, labels) do
      true ->
        {r_k_goto(label: f0), [], st0}

      false ->
        {kargs, ap, st1} = atomic_list(cargs, sub, st0)
        f1 = get_fsub(f0, ar, sub)
        {r_k_call(anno: a, op: r_k_local(anno: ra, name: f1, arity: ar), args: kargs), ap, st1}
    end
  end

  defp c_apply(a, cop, cargs, sub, st0) do
    {kop, op, st1} = variable(cop, sub, st0)
    {kargs, ap, st2} = atomic_list(cargs, sub, st1)
    {r_k_call(anno: a, op: kop, args: kargs), op ++ ap, st2}
  end

  defp flatten_seq(r_iset(anno: a, vars: vs, arg: arg, body: b)) do
    [r_iset(anno: a, vars: vs, arg: arg) | flatten_seq(b)]
  end

  defp flatten_seq(ke) do
    [ke]
  end

  defp pre_seq(
         [r_iset(anno: a, vars: vs, arg: arg, body: b) | ps],
         k
       ) do
    ^b = :undefined
    r_iset(anno: a, vars: vs, arg: arg, body: pre_seq(ps, k))
  end

  defp pre_seq([p | ps], k) do
    r_iset(vars: [], arg: p, body: pre_seq(ps, k))
  end

  defp pre_seq([], k) do
    k
  end

  defp atomic(ce, sub, st0) do
    {ke, kp, st1} = expr(ce, sub, st0)
    {ka, ap, st2} = force_atomic(ke, st1)
    {ka, kp ++ ap, st2}
  end

  defp force_atomic(ke, st0) do
    case is_atomic(ke) do
      true ->
        {ke, [], st0}

      false ->
        {v, st1} = new_var(st0)
        {v, [r_iset(vars: [v], arg: ke)], st1}
    end
  end

  defp atomic_bin(
         [
           r_c_bitstr(anno: a, val: e0, size: s0, unit: u0, type: t, flags: fs0)
           | es0
         ],
         sub,
         st0
       ) do
    {e, ap1, st1} = atomic(e0, sub, st0)
    {s1, ap2, st2} = atomic(s0, sub, st1)
    validate_bin_element_size(s1)
    u1 = :cerl.concrete(u0)
    fs1 = :cerl.concrete(fs0)
    {es, ap3, st3} = atomic_bin(es0, sub, st2)

    {r_k_bin_seg(
       anno: a,
       size: s1,
       unit: u1,
       type: :cerl.concrete(t),
       flags: fs1,
       seg: e,
       next: es
     ), ap1 ++ ap2 ++ ap3, st3}
  end

  defp atomic_bin([], _Sub, st) do
    {r_k_bin_end(), [], st}
  end

  defp validate_bin_element_size(r_k_var()) do
    :ok
  end

  defp validate_bin_element_size(r_k_literal(val: val)) do
    case val do
      :all ->
        :ok

      :undefined ->
        :ok

      _ when is_integer(val) and val >= 0 ->
        :ok

      _ ->
        throw(:bad_element_size)
    end
  end

  defp atomic_list(ces, sub, st) do
    foldr(
      fn ce, {kes, esp, st0} ->
        {ke, ep, st1} = atomic(ce, sub, st0)
        {[ke | kes], ep ++ esp, st1}
      end,
      {[], [], st},
      ces
    )
  end

  defp is_atomic(r_k_literal()) do
    true
  end

  defp is_atomic(r_k_var()) do
    true
  end

  defp is_atomic(_) do
    false
  end

  defp variable(ce, sub, st0) do
    {ke, kp, st1} = expr(ce, sub, st0)
    {kv, vp, st2} = force_variable(ke, st1)
    {kv, kp ++ vp, st2}
  end

  defp force_variable(r_k_var() = ke, st) do
    {ke, [], st}
  end

  defp force_variable(ke, st0) do
    {v, st1} = new_var(st0)
    {v, [r_iset(vars: [v], arg: ke)], st1}
  end

  defp pattern(r_c_var(anno: a, name: v), _Isub, osub, st0) do
    case :cerl_sets.is_element(v, r_kern(st0, :ds)) do
      true ->
        {new, st1} = new_var_name(st0)

        {r_k_var(anno: a, name: new), set_vsub(v, new, osub),
         r_kern(st1, ds: :cerl_sets.add_element(new, r_kern(st1, :ds)))}

      false ->
        {r_k_var(anno: a, name: v), osub,
         r_kern(st0, ds: :cerl_sets.add_element(v, r_kern(st0, :ds)))}
    end
  end

  defp pattern(r_c_literal(anno: a, val: val), _Isub, osub, st) do
    {r_k_literal(anno: a, val: val), osub, st}
  end

  defp pattern(r_c_cons(anno: a, hd: ch, tl: ct), isub, osub0, st0) do
    {kh, osub1, st1} = pattern(ch, isub, osub0, st0)
    {kt, osub2, st2} = pattern(ct, isub, osub1, st1)
    {r_k_cons(anno: a, hd: kh, tl: kt), osub2, st2}
  end

  defp pattern(r_c_tuple(anno: a, es: ces), isub, osub0, st0) do
    {kes, osub1, st1} = pattern_list(ces, isub, osub0, st0)
    {r_k_tuple(anno: a, es: kes), osub1, st1}
  end

  defp pattern(r_c_map(anno: a, es: ces), isub, osub0, st0) do
    {kes, osub1, st1} = pattern_map_pairs(ces, isub, osub0, st0)
    {r_k_map(anno: a, op: :exact, es: kes), osub1, st1}
  end

  defp pattern(r_c_binary(anno: a, segments: cv), isub, osub0, st0) do
    {kv, osub1, st1} = pattern_bin(cv, isub, osub0, st0)
    {r_k_binary(anno: a, segs: kv), osub1, st1}
  end

  defp pattern(r_c_alias(anno: a, var: cv, pat: cp), isub, osub0, st0) do
    {cvs, cpat} = flatten_alias(cp)
    {kvs, osub1, st1} = pattern_list([cv | cvs], isub, osub0, st0)
    {kpat, osub2, st2} = pattern(cpat, isub, osub1, st1)
    {r_ialias(anno: a, vars: kvs, pat: kpat), osub2, st2}
  end

  defp flatten_alias(r_c_alias(var: v, pat: p)) do
    {vs, pat} = flatten_alias(p)
    {[v | vs], pat}
  end

  defp flatten_alias(pat) do
    {[], pat}
  end

  defp pattern_map_pairs(ces0, isub, osub0, st0) do
    {kes, {osub1, st1}} =
      mapfoldl(
        fn r_c_map_pair(anno: a, key: ck, val: cv), {osubi0, sti0} ->
          {kk, [], sti1} = expr(ck, isub, sti0)
          {kv, osubi2, sti2} = pattern(cv, isub, osubi0, sti1)
          {r_k_map_pair(anno: a, key: kk, val: kv), {osubi2, sti2}}
        end,
        {osub0, st0},
        ces0
      )

    kes1 =
      sort(
        fn r_k_map_pair(key: kkA), r_k_map_pair(key: kkB) ->
          a = map_key_clean(kkA)
          b = map_key_clean(kkB)
          :erts_internal.cmp_term(a, b) < 0
        end,
        kes
      )

    {kes1, osub1, st1}
  end

  defp pattern_bin(es, isub, osub0, st) do
    pattern_bin_1(es, isub, osub0, st)
  end

  defp pattern_bin_1(
         [
           r_c_bitstr(anno: a, val: e0, size: s0, unit: u0, type: t, flags: fs0)
           | es0
         ],
         isub,
         osub0,
         st0
       ) do
    {s1, [], st1} = expr(s0, isub, st0)

    s =
      case s1 do
        r_k_var() ->
          s1

        r_k_literal(val: val) when is_integer(val) or is_atom(val) ->
          s1

        _ ->
          r_k_literal(val: :bad_size)
      end

    u = :cerl.concrete(u0)
    fs = :cerl.concrete(fs0)
    {e, osub1, st2} = pattern(e0, isub, osub0, st1)
    {es, osub, st3} = pattern_bin_1(es0, isub, osub1, st2)
    {build_bin_seg(a, s, u, :cerl.concrete(t), fs, e, es), osub, st3}
  end

  defp pattern_bin_1([], _Isub, osub, st) do
    {r_k_bin_end(), osub, st}
  end

  defp build_bin_seg(
         a,
         r_k_literal(val: bits) = sz,
         u,
         :integer = type,
         [:unsigned, :big] = flags,
         r_k_literal(val: int) = seg,
         next
       )
       when is_integer(bits) do
    size = bits * u

    case integer_fits_and_is_expandable(int, size) do
      true ->
        build_bin_seg_integer_recur(a, size, int, next)

      false ->
        r_k_bin_seg(anno: a, size: sz, unit: u, type: type, flags: flags, seg: seg, next: next)
    end
  end

  defp build_bin_seg(
         a,
         sz,
         u,
         :utf8 = type,
         [:unsigned, :big] = flags,
         r_k_literal(val: utf8) = seg,
         next
       ) do
    case utf8_fits(utf8) do
      {int, bits} ->
        build_bin_seg_integer_recur(a, bits, int, next)

      :error ->
        r_k_bin_seg(anno: a, size: sz, unit: u, type: type, flags: flags, seg: seg, next: next)
    end
  end

  defp build_bin_seg(a, sz, u, type, flags, seg, next) do
    r_k_bin_seg(anno: a, size: sz, unit: u, type: type, flags: flags, seg: seg, next: next)
  end

  defp build_bin_seg_integer_recur(a, bits, val, next) when bits > 8 do
    nextBits = bits - 8
    nextVal = val &&& 1 <<< (nextBits - 1)
    last = build_bin_seg_integer_recur(a, nextBits, nextVal, next)
    build_bin_seg_integer(a, 8, val >>> nextBits, last)
  end

  defp build_bin_seg_integer_recur(a, bits, val, next) do
    build_bin_seg_integer(a, bits, val, next)
  end

  defp build_bin_seg_integer(a, bits, val, next) do
    sz = r_k_literal(anno: a, val: bits)
    seg = r_k_literal(anno: a, val: val)

    r_k_bin_seg(
      anno: a,
      size: sz,
      unit: 1,
      type: :integer,
      flags: [:unsigned, :big],
      seg: seg,
      next: next
    )
  end

  defp integer_fits_and_is_expandable(int, size)
       when is_integer(int) and
              is_integer(size) and 0 < size and size <= 1024 do
    case <<int::size(size)>> do
      <<^int::size(size)>> ->
        true

      _ ->
        false
    end
  end

  defp integer_fits_and_is_expandable(_Int, _Size) do
    false
  end

  defp utf8_fits(utf8) do
    try do
      bin = <<utf8::utf8>>
      bits = bit_size(bin)
      <<int::size(bits)>> = bin
      {int, bits}
    catch
      _, _ ->
        :error
    end
  end

  defp pattern_list(ces, sub, st) do
    pattern_list(ces, sub, sub, st)
  end

  defp pattern_list(ces, isub, osub, st) do
    foldr(
      fn ce, {kes, osub0, st0} ->
        {ke, osub1, st1} = pattern(ce, isub, osub0, st0)
        {[ke | kes], osub1, st1}
      end,
      {[], osub, st},
      ces
    )
  end

  defp new_sub() do
    {%{}, %{}}
  end

  defp get_vsub(key, subs) do
    bimap_get(key, subs, key)
  end

  defp get_fsub(name, arity, subs) do
    bimap_get({name, arity}, subs, name)
  end

  defp set_vsub(same, same, subs) do
    subs
  end

  defp set_vsub(key, val, subs) do
    bimap_set(key, val, subs)
  end

  defp set_fsub(name, arity, val, subs) do
    set_vsub({name, arity}, val, subs)
  end

  defp subst_vsub(key, val, subs) do
    bimap_rename(key, val, subs)
  end

  defp bimap_get(key, {map, _InvMap}, default) do
    case map do
      %{^key => val} ->
        val

      _ ->
        default
    end
  end

  defp bimap_set(key, val, {map0, invMap0}) do
    invMap = bm_update_inv_lookup(key, val, map0, invMap0)
    map = %{map0 | key => val}
    {map, invMap}
  end

  defp bm_update_inv_lookup(key, val, map, invMap0) do
    invMap = bm_cleanup_inv_lookup(key, map, invMap0)

    case invMap do
      %{^val => keys} ->
        %{invMap | val => :ordsets.add_element(key, keys)}

      %{} ->
        %{invMap | val => [key]}
    end
  end

  defp bm_cleanup_inv_lookup(key, map, invMap)
       when :erlang.is_map_key(
              key,
              map
            ) do
    %{^key => old} = map

    case invMap do
      %{^old => [^key]} ->
        :maps.remove(old, invMap)

      %{^old => [_ | _] = keys} ->
        %{invMap | old => :ordsets.del_element(key, keys)}
    end
  end

  defp bm_cleanup_inv_lookup(_Key, _Map, invMap) do
    invMap
  end

  defp bimap_rename(key, val, {map0, invMap0})
       when :erlang.is_map_key(key, invMap0) do
    keys = :erlang.map_get(key, invMap0)
    map1 = %{map0 | key => val}
    map = bimap_update_lookup(keys, val, map1)
    invMap1 = :maps.remove(key, invMap0)
    invMap = %{invMap1 | val => :ordsets.add_element(key, keys)}
    {map, invMap}
  end

  defp bimap_rename(key, val, subs) do
    bimap_set(key, val, subs)
  end

  defp bimap_update_lookup([key | keys], val, map) do
    bimap_update_lookup(keys, val, %{map | key => val})
  end

  defp bimap_update_lookup([], _Val, map) do
    map
  end

  defp new_fun_name(st) do
    new_fun_name('anonymous', st)
  end

  defp new_fun_name(type, r_kern(func: {f, arity}, fcount: c) = st) do
    name =
      '-' ++
        :erlang.atom_to_list(f) ++
        '/' ++
        :erlang.integer_to_list(arity) ++ '-' ++ type ++ '-' ++ :erlang.integer_to_list(c) ++ '-'

    {:erlang.list_to_atom(name), r_kern(st, fcount: c + 1)}
  end

  defp new_var_name(r_kern(vcount: c) = st) do
    {c, r_kern(st, vcount: c + 1)}
  end

  defp new_var(st0) do
    {new, st1} = new_var_name(st0)
    {r_k_var(name: new), st1}
  end

  defp new_vars(n, st) do
    new_vars(n, st, [])
  end

  defp new_vars(n, st0, vs) when n > 0 do
    {v, st1} = new_var(st0)
    new_vars(n - 1, st1, [v | vs])
  end

  defp new_vars(0, st, vs) do
    {vs, st}
  end

  defp make_vars(vs) do
    for v <- vs do
      r_k_var(name: v)
    end
  end

  defp is_remote_bif(:erlang, :get, [_]) do
    true
  end

  defp is_remote_bif(:erlang, :is_record, [_, tag, sz]) do
    case {tag, sz} do
      {r_c_literal(val: atom), r_c_literal(val: int)}
      when is_atom(atom) and
             is_integer(int) ->
        true

      {_, _} ->
        false
    end
  end

  defp is_remote_bif(:erlang, n, as) do
    arity = length(as)

    case :erl_internal.guard_bif(n, arity) do
      true ->
        true

      false ->
        try do
          :erl_internal.op_type(n, arity)
        catch
          _, _ ->
            false
        else
          :arith ->
            true

          :bool ->
            true

          :comp ->
            true

          :list ->
            false

          :send ->
            false
        end
    end
  end

  defp is_remote_bif(_, _, _) do
    false
  end

  defp bif_vals(:recv_peek_message, 0) do
    2
  end

  defp bif_vals(_, _) do
    1
  end

  defp bif_vals(_, _, _) do
    1
  end

  defp foldr2(fun, acc0, [e1 | l1], [e2 | l2]) do
    acc1 = fun.(e1, e2, acc0)
    foldr2(fun, acc1, l1, l2)
  end

  defp foldr2(_, acc, [], []) do
    acc
  end

  defp kmatch(us, ccs, sub, st0) do
    {cs, st1} = match_pre(ccs, sub, st0)
    def__ = :fail
    match(us, cs, def__, st1)
  end

  defp match_pre(cs, sub0, st) do
    foldr(
      fn r_c_clause(anno: a, pats: ps, guard: g, body: b), {cs0, st0} ->
        {kps, osub1, st1} = pattern_list(ps, sub0, st0)

        {[
           r_iclause(anno: a, isub: sub0, osub: osub1, pats: kps, guard: g, body: b)
           | cs0
         ], st1}
      end,
      {[], st},
      cs
    )
  end

  defp match([_U | _Us] = l, cs, def__, st0) do
    pcss = partition(cs)

    foldr(
      fn pcs, {d, st} ->
        match_varcon(l, pcs, d, st)
      end,
      {def__, st0},
      pcss
    )
  end

  defp match([], cs, def__, st) do
    match_guard(cs, def__, st)
  end

  defp match_guard(cs0, def0, st0) do
    {cs1, def1, st1} = match_guard_1(cs0, def0, st0)
    {build_alt(build_guard(cs1), def1), st1}
  end

  defp match_guard_1(
         [
           r_iclause(anno: a, osub: osub, guard: g, body: b)
           | cs0
         ],
         def0,
         st0
       ) do
    case is_true_guard(g) do
      true ->
        {kb, pb, st1} = body(b, osub, st0)
        st2 = maybe_add_warning(cs0, a, st1)
        st = maybe_add_warning(def0, a, st2)
        {[], pre_seq(pb, kb), st}

      false ->
        {kg, st1} = guard(g, osub, st0)
        {kb, pb, st2} = body(b, osub, st1)
        {cs1, def1, st3} = match_guard_1(cs0, def0, st2)
        {[r_k_guard_clause(guard: kg, body: pre_seq(pb, kb)) | cs1], def1, st3}
    end
  end

  defp match_guard_1([], def__, st) do
    {[], def__, st}
  end

  defp maybe_add_warning([c | _], matchAnno, st) do
    maybe_add_warning(c, matchAnno, st)
  end

  defp maybe_add_warning([], _MatchAnno, st) do
    st
  end

  defp maybe_add_warning(:fail, _MatchAnno, st) do
    st
  end

  defp maybe_add_warning(ke, matchAnno, st) do
    case is_compiler_generated(ke) do
      true ->
        st

      false ->
        anno = get_kanno(ke)
        line = get_line(anno)
        matchLine = get_line(matchAnno)

        warn =
          case matchLine do
            :none ->
              :nomatch_shadow

            _ ->
              {:nomatch_shadow, matchLine}
          end

        add_warning(line, warn, anno, st)
    end
  end

  defp get_line([line | _]) when is_integer(line) do
    line
  end

  defp get_line([_ | t]) do
    get_line(t)
  end

  defp get_line([]) do
    :none
  end

  defp get_file([{:file, file} | _]) do
    file
  end

  defp get_file([_ | t]) do
    get_file(t)
  end

  defp get_file([]) do
    'no_file'
  end

  defp is_true_guard(r_c_literal(val: true)) do
    true
  end

  defp is_true_guard(_) do
    false
  end

  defp partition([c1 | cs]) do
    v1 = is_var_clause(c1)

    {more, rest} =
      splitwith(
        fn c ->
          is_var_clause(c) === v1
        end,
        cs
      )

    [[c1 | more] | partition(rest)]
  end

  defp partition([]) do
    []
  end

  defp match_varcon(us, [c | _] = cs, def__, st) do
    case is_var_clause(c) do
      true ->
        match_var(us, cs, def__, st)

      false ->
        match_con(us, cs, def__, st)
    end
  end

  defp match_var([u | us], cs0, def__, st) do
    cs1 =
      map(
        fn r_iclause(isub: isub0, osub: osub0, pats: [arg | as]) = c ->
          vs = [arg_arg(arg) | arg_alias(arg)]

          osub1 =
            foldl(
              fn r_k_var(name: v), acc ->
                subst_vsub(v, r_k_var(u, :name), acc)
              end,
              osub0,
              vs
            )

          isub1 =
            foldl(
              fn r_k_var(name: v), acc ->
                subst_vsub(v, r_k_var(u, :name), acc)
              end,
              isub0,
              vs
            )

          r_iclause(c, isub: isub1, osub: osub1, pats: as)
        end,
        cs0
      )

    match(us, cs1, def__, st)
  end

  defp match_con([u | _Us] = l, cs, def__, st0) do
    ttcs0 = select_types(cs, [], [], [], [], [], [], [], [], [])

    ttcs1 =
      for {t, [_ | _] = types} <- ttcs0 do
        {t, types}
      end

    ttcs = opt_single_valued(ttcs1)

    {scs, st1} =
      mapfoldl(
        fn {t, tcs}, st ->
          {[s | _] = sc, s1} = match_value(l, t, tcs, :fail, st)
          anno = get_kanno(s)
          {r_k_type_clause(anno: anno, type: t, values: sc), s1}
        end,
        st0,
        ttcs
      )

    {build_alt_1st_no_fail(build_select(u, scs), def__), st1}
  end

  defp select_types([noExpC | cs], bin, binCon, cons, tuple, map, atom, float, int, nil__) do
    c = expand_pat_lit_clause(noExpC)

    case clause_con(c) do
      :k_binary ->
        select_types(cs, [c | bin], binCon, cons, tuple, map, atom, float, int, nil__)

      :k_bin_seg ->
        select_types(cs, bin, [c | binCon], cons, tuple, map, atom, float, int, nil__)

      :k_bin_end ->
        select_types(cs, bin, [c | binCon], cons, tuple, map, atom, float, int, nil__)

      :k_cons ->
        select_types(cs, bin, binCon, [c | cons], tuple, map, atom, float, int, nil__)

      :k_tuple ->
        select_types(cs, bin, binCon, cons, [c | tuple], map, atom, float, int, nil__)

      :k_map ->
        select_types(cs, bin, binCon, cons, tuple, [c | map], atom, float, int, nil__)

      :k_atom ->
        select_types(cs, bin, binCon, cons, tuple, map, [c | atom], float, int, nil__)

      :k_float ->
        select_types(cs, bin, binCon, cons, tuple, map, atom, [c | float], int, nil__)

      :k_int ->
        select_types(cs, bin, binCon, cons, tuple, map, atom, float, [c | int], nil__)

      :k_nil ->
        select_types(cs, bin, binCon, cons, tuple, map, atom, float, int, [c | nil__])
    end
  end

  defp select_types([], bin, binCon, cons, tuple, map, atom, float, int, nil__) do
    [{:k_binary, reverse(bin)}] ++
      handle_bin_con(reverse(binCon)) ++
      [
        {:k_cons, reverse(cons)},
        {:k_tuple, reverse(tuple)},
        {:k_map, reverse(map)},
        {:k_atom, reverse(atom)},
        {:k_float, reverse(float)},
        {:k_int, reverse(int)},
        {:k_nil, reverse(nil__)}
      ]
  end

  defp expand_pat_lit_clause(
         r_iclause(
           pats: [
             r_ialias(pat: r_k_literal(anno: a, val: val)) = alias
             | ps
           ]
         ) = c
       ) do
    p = expand_pat_lit(val, a)
    r_iclause(c, pats: [r_ialias(alias, pat: p) | ps])
  end

  defp expand_pat_lit_clause(r_iclause(pats: [r_k_literal(anno: a, val: val) | ps]) = c) do
    p = expand_pat_lit(val, a)
    r_iclause(c, pats: [p | ps])
  end

  defp expand_pat_lit_clause(c) do
    c
  end

  defp expand_pat_lit([h | t], a) do
    r_k_cons(anno: a, hd: r_k_literal(anno: a, val: h), tl: r_k_literal(anno: a, val: t))
  end

  defp expand_pat_lit(tuple, a) when is_tuple(tuple) do
    r_k_tuple(
      anno: a,
      es:
        for e <- :erlang.tuple_to_list(tuple) do
          r_k_literal(anno: a, val: e)
        end
    )
  end

  defp expand_pat_lit(lit, a) do
    r_k_literal(anno: a, val: lit)
  end

  defp opt_single_valued(ttcs) do
    opt_single_valued(ttcs, [], [])
  end

  defp opt_single_valued(
         [{_, [r_iclause(pats: [r_k_literal() | _])]} = ttc | ttcs],
         ttcAcc,
         litAcc
       ) do
    opt_single_valued(ttcs, [ttc | ttcAcc], litAcc)
  end

  defp opt_single_valued([{_, [r_iclause(pats: [p0 | ps]) = tc]} = ttc | ttcs], ttcAcc, litAcc) do
    try do
      combine_lit_pat(p0)
    catch
      :not_possible ->
        opt_single_valued(ttcs, [ttc | ttcAcc], litAcc)
    else
      p ->
        litTtc = r_iclause(tc, pats: [p | ps])
        opt_single_valued(ttcs, ttcAcc, [litTtc | litAcc])
    end
  end

  defp opt_single_valued([ttc | ttcs], ttcAcc, litAcc) do
    opt_single_valued(ttcs, [ttc | ttcAcc], litAcc)
  end

  defp opt_single_valued([], ttcAcc, []) do
    reverse(ttcAcc)
  end

  defp opt_single_valued([], ttcAcc, litAcc) do
    literals = {:k_literal, reverse(litAcc)}

    case reverse(ttcAcc) do
      [{:k_binary, _} = bin | ttcs] ->
        [[bin, literals] | ttcs]

      ttcs ->
        [literals | ttcs]
    end
  end

  defp combine_lit_pat(r_ialias(pat: pat0) = alias) do
    pat = combine_lit_pat(pat0)
    r_ialias(alias, pat: pat)
  end

  defp combine_lit_pat(r_k_literal()) do
    throw(:not_possible)
  end

  defp combine_lit_pat(pat) do
    do_combine_lit_pat(pat)
  end

  defp do_combine_lit_pat(r_k_binary(anno: a, segs: segs)) do
    bin = combine_bin_segs(segs)
    r_k_literal(anno: a, val: bin)
  end

  defp do_combine_lit_pat(r_k_cons(anno: a, hd: hd0, tl: tl0)) do
    r_k_literal(val: hd) = do_combine_lit_pat(hd0)
    r_k_literal(val: tl) = do_combine_lit_pat(tl0)
    r_k_literal(anno: a, val: [hd | tl])
  end

  defp do_combine_lit_pat(r_k_literal() = lit) do
    lit
  end

  defp do_combine_lit_pat(r_k_tuple(anno: a, es: es0)) do
    es =
      for el <- es0 do
        r_k_literal(val: lit) = do_combine_lit_pat(el)
        lit
      end

    r_k_literal(anno: a, val: :erlang.list_to_tuple(es))
  end

  defp do_combine_lit_pat(_) do
    throw(:not_possible)
  end

  defp combine_bin_segs(
         r_k_bin_seg(
           size: r_k_literal(val: 8),
           unit: 1,
           type: :integer,
           flags: [:unsigned, :big],
           seg: r_k_literal(val: int),
           next: next
         )
       )
       when is_integer(int) and 0 <= int and int <= 255 do
    <<int, combine_bin_segs(next)::bits>>
  end

  defp combine_bin_segs(r_k_bin_end()) do
    <<>>
  end

  defp combine_bin_segs(_) do
    throw(:not_possible)
  end

  defp handle_bin_con(cs) do
    try do
      {binSegs0, binEnd} =
        partition(
          fn c ->
            clause_con(c) === :k_bin_seg
          end,
          cs
        )

      binSegs = select_bin_int(binSegs0)

      case binEnd do
        [] ->
          binSegs

        [_ | _] ->
          binSegs ++ [{:k_bin_end, binEnd}]
      end
    catch
      :not_possible ->
        handle_bin_con_not_possible(cs)
    end
  end

  defp handle_bin_con_not_possible([c1 | cs]) do
    con = clause_con(c1)

    {more, rest} =
      splitwith(
        fn c ->
          clause_con(c) === con
        end,
        cs
      )

    [{con, [c1 | more]} | handle_bin_con_not_possible(rest)]
  end

  defp handle_bin_con_not_possible([]) do
    []
  end

  defp select_bin_int([
         r_iclause(
           pats: [
             r_k_bin_seg(
               anno: a,
               type: :integer,
               size: r_k_literal(val: bits0) = sz,
               unit: u,
               flags: fl,
               seg: r_k_literal(val: val),
               next: n
             )
             | ps
           ]
         ) = c
         | cs0
       ])
       when is_integer(bits0) do
    bits = u * bits0

    cond do
      bits > 1024 ->
        throw(:not_possible)

      true ->
        :ok
    end

    select_assert_match_possible(bits, val, fl)
    p = r_k_bin_int(anno: a, size: sz, unit: u, flags: fl, val: val, next: n)

    case member(:native, fl) do
      true ->
        throw(:not_possible)

      false ->
        :ok
    end

    cs = select_bin_int_1(cs0, bits, fl, val)
    [{:k_bin_int, [r_iclause(c, pats: [p | ps]) | cs]}]
  end

  defp select_bin_int(_) do
    throw(:not_possible)
  end

  defp select_bin_int_1(
         [
           r_iclause(
             pats: [
               r_k_bin_seg(
                 anno: a,
                 type: :integer,
                 size: r_k_literal(val: bits0) = sz,
                 unit: u,
                 flags: fl,
                 seg: r_k_literal(val: val),
                 next: n
               )
               | ps
             ]
           ) = c
           | cs
         ],
         bits,
         fl,
         val
       )
       when is_integer(val) do
    cond do
      bits0 * u === bits ->
        :ok

      true ->
        throw(:not_possible)
    end

    p = r_k_bin_int(anno: a, size: sz, unit: u, flags: fl, val: val, next: n)
    [r_iclause(c, pats: [p | ps]) | select_bin_int_1(cs, bits, fl, val)]
  end

  defp select_bin_int_1([], _, _, _) do
    []
  end

  defp select_bin_int_1(_, _, _, _) do
    throw(:not_possible)
  end

  defp select_assert_match_possible(sz, val, fs)
       when is_integer(sz) and
              sz >= 0 and is_integer(val) do
    emptyBindings = :erl_eval.new_bindings()
    matchFun = match_fun(val)

    evalFun = fn {:integer, _, s}, b ->
      {:value, s, b}
    end

    expr = [{:bin_element, 0, {:integer, 0, val}, {:integer, 0, sz}, [{:unit, 1} | fs]}]
    {:value, bin, ^emptyBindings} = :eval_bits.expr_grp(expr, emptyBindings, evalFun)

    try do
      {:match, _} =
        :eval_bits.match_bits(expr, bin, emptyBindings, emptyBindings, matchFun, evalFun)

      :ok
    catch
      :nomatch ->
        throw(:not_possible)
    end
  end

  defp select_assert_match_possible(_, _, _) do
    throw(:not_possible)
  end

  defp match_fun(val) do
    fn :match, {{:integer, _, _}, newV, bs}
       when newV === val ->
      {:match, bs}
    end
  end

  defp match_value(us0, t, cs0, def__, st0) do
    {us1, cs1, st1} = partition_intersection(t, us0, cs0, st0)
    uCss = group_value(t, us1, cs1)

    mapfoldl(
      fn {us, cs}, st ->
        match_clause(us, cs, def__, st)
      end,
      st1,
      uCss
    )
  end

  defp partition_intersection(:k_map, [u | _] = us, [[_, _] | _] = cs0, st0) do
    ps =
      for c <- cs0 do
        clause_val(c)
      end

    case find_key_intersection(ps) do
      :none ->
        {us, cs0, st0}

      ks ->
        cs1 =
          map(
            fn r_iclause(pats: [arg | args]) = c ->
              {arg1, arg2} = partition_keys(arg, ks)
              r_iclause(c, pats: [[arg1, arg2] | args])
            end,
            cs0
          )

        {[u | us], cs1, st0}
    end
  end

  defp partition_intersection(_, us, cs, st) do
    {us, cs, st}
  end

  defp partition_keys(r_k_map(es: pairs) = map, ks) do
    f = fn r_k_map_pair(key: key) ->
      :cerl_sets.is_element(map_key_clean(key), ks)
    end

    {ps1, ps2} = partition(f, pairs)
    {r_k_map(map, es: ps1), r_k_map(map, es: ps2)}
  end

  defp partition_keys(r_ialias(pat: map) = alias, ks) do
    {map1, map2} = partition_keys(map, ks)
    {map1, r_ialias(alias, pat: map2)}
  end

  defp find_key_intersection(ps) do
    sets =
      for ks <- ps do
        :cerl_sets.from_list(ks)
      end

    intersection = :cerl_sets.intersection(sets)

    case :cerl_sets.size(intersection) do
      0 ->
        :none

      _ ->
        all =
          all(
            fn kset ->
              kset === intersection
            end,
            sets
          )

        case all do
          true ->
            :none

          false ->
            intersection
        end
    end
  end

  defp group_value(:k_cons, us, cs) do
    [{us, cs}]
  end

  defp group_value(:k_nil, us, cs) do
    [{us, cs}]
  end

  defp group_value(:k_binary, us, cs) do
    [{us, cs}]
  end

  defp group_value(:k_bin_end, us, cs) do
    [{us, cs}]
  end

  defp group_value(:k_bin_seg, us, cs) do
    group_keeping_order(us, cs)
  end

  defp group_value(:k_bin_int, us, cs) do
    [{us, cs}]
  end

  defp group_value(:k_map, us, cs) do
    group_keeping_order(us, cs)
  end

  defp group_value(_, us, cs) do
    map = group_values(cs, %{})

    sort(
      :maps.fold(
        fn _, vcs, css ->
          [{us, reverse(vcs)} | css]
        end,
        [],
        map
      )
    )
  end

  defp group_values([c | cs], acc) do
    val = clause_val(c)

    case acc do
      %{^val => gcs} ->
        group_values(cs, %{acc | val => [c | gcs]})

      %{} ->
        group_values(cs, %{acc | val => [c]})
    end
  end

  defp group_values([], acc) do
    acc
  end

  defp group_keeping_order(us, [c1 | cs]) do
    v1 = clause_val(c1)

    {more, rest} =
      splitwith(
        fn c ->
          clause_val(c) === v1
        end,
        cs
      )

    [{us, [c1 | more]} | group_keeping_order(us, rest)]
  end

  defp group_keeping_order(_, []) do
    []
  end

  defp match_clause([u | us], [c | _] = cs0, def__, st0) do
    anno = get_kanno(c)
    {match0, vs, st1} = get_match(get_con(cs0), st0)
    match = sub_size_var(match0, cs0)
    {cs1, st2} = new_clauses(cs0, u, st1)
    cs2 = squeeze_clauses_by_bin_integer_count(cs1, [])
    {b, st3} = match(vs ++ us, cs2, def__, st2)
    {r_k_val_clause(anno: anno, val: match, body: b), st3}
  end

  defp sub_size_var(
         r_k_bin_seg(size: r_k_var(name: name) = kvar) = binSeg,
         [r_iclause(isub: sub) | _]
       ) do
    r_k_bin_seg(binSeg, size: r_k_var(kvar, name: get_vsub(name, sub)))
  end

  defp sub_size_var(k, _) do
    k
  end

  defp get_con([c | _]) do
    arg_arg(clause_arg(c))
  end

  defp get_match(r_k_cons(), st0) do
    {[h, t] = l, st1} = new_vars(2, st0)
    {r_k_cons(hd: h, tl: t), l, st1}
  end

  defp get_match(r_k_binary(), st0) do
    {[v] = mes, st1} = new_vars(1, st0)
    {r_k_binary(segs: v), mes, st1}
  end

  defp get_match(
         r_k_bin_seg(
           size: r_k_literal(val: :all),
           next: {:k_bin_end, []}
         ) = seg,
         st0
       ) do
    {[s, n], st1} = new_vars(2, st0)
    {r_k_bin_seg(seg, seg: s, next: n), [s], st1}
  end

  defp get_match(r_k_bin_seg() = seg, st0) do
    {[s, n], st1} = new_vars(2, st0)
    {r_k_bin_seg(seg, seg: s, next: n), [s, n], st1}
  end

  defp get_match(r_k_bin_int() = binInt, st0) do
    {n, st1} = new_var(st0)
    {r_k_bin_int(binInt, next: n), [n], st1}
  end

  defp get_match(r_k_tuple(es: es), st0) do
    {mes, st1} = new_vars(length(es), st0)
    {r_k_tuple(es: mes), mes, st1}
  end

  defp get_match(r_k_map(op: :exact, es: es0), st0) do
    {mes, st1} = new_vars(length(es0), st0)

    {es, _} =
      mapfoldl(
        fn r_k_map_pair() = pair, [v | vs] ->
          {r_k_map_pair(pair, val: v), vs}
        end,
        mes,
        es0
      )

    {r_k_map(op: :exact, es: es), mes, st1}
  end

  defp get_match(m, st) do
    {m, [], st}
  end

  defp new_clauses(cs0, u, st) do
    cs1 =
      map(
        fn r_iclause(isub: isub0, osub: osub0, pats: [arg | as]) = c ->
          head =
            case arg_arg(arg) do
              r_k_cons(hd: h, tl: t) ->
                [[h, t] | as]

              r_k_tuple(es: es) ->
                es ++ as

              r_k_binary(segs: e) ->
                [e | as]

              r_k_bin_seg(size: r_k_literal(val: :all), seg: s, next: {:k_bin_end, []}) ->
                [s | as]

              r_k_bin_seg(seg: s, next: n) ->
                [[s, n] | as]

              r_k_bin_int(next: n) ->
                [n | as]

              r_k_map(op: :exact, es: es) ->
                vals =
                  for r_k_map_pair(val: v) <- es do
                    v
                  end

                vals ++ as

              _Other ->
                as
            end

          vs = arg_alias(arg)

          osub1 =
            foldl(
              fn r_k_var(name: v), acc ->
                subst_vsub(v, r_k_var(u, :name), acc)
              end,
              osub0,
              vs
            )

          isub1 =
            foldl(
              fn r_k_var(name: v), acc ->
                subst_vsub(v, r_k_var(u, :name), acc)
              end,
              isub0,
              vs
            )

          r_iclause(c, isub: isub1, osub: osub1, pats: head)
        end,
        cs0
      )

    {cs1, st}
  end

  defp squeeze_clauses_by_bin_integer_count([clause | clauses], acc) do
    case clause_count_bin_integer_segments(clause) do
      {:literal, n} ->
        squeeze_clauses_by_bin_integer_count(clauses, n, 1, [clause], acc)

      _ ->
        squeeze_clauses_by_bin_integer_count(
          clauses,
          [[clause] | acc]
        )
    end
  end

  defp squeeze_clauses_by_bin_integer_count(_, acc) do
    flat_reverse(acc, [])
  end

  defp squeeze_clauses_by_bin_integer_count([], n, count, groupAcc, acc) do
    squeezed = squeeze_clauses(groupAcc, fix_count_without_variadic_segment(n), count)
    flat_reverse([squeezed | acc], [])
  end

  defp squeeze_clauses_by_bin_integer_count(
         [r_iclause(pats: [r_k_bin_end() | _]) = clause],
         n,
         count,
         groupAcc,
         acc
       ) do
    squeezed = squeeze_clauses(groupAcc, fix_count_without_variadic_segment(n), count)
    flat_reverse([[clause | squeezed] | acc], [])
  end

  defp squeeze_clauses_by_bin_integer_count([clause | clauses], n, count, groupAcc, acc) do
    case clause_count_bin_integer_segments(clause) do
      {:literal, newN} ->
        squeeze_clauses_by_bin_integer_count(
          clauses,
          min(n, newN),
          count + 1,
          [clause | groupAcc],
          acc
        )

      {:variadic, newN} when newN <= n ->
        squeezed = squeeze_clauses(groupAcc, newN, count)

        squeeze_clauses_by_bin_integer_count(
          clauses,
          [[clause | squeezed] | acc]
        )

      _ ->
        squeeze_clauses_by_bin_integer_count(
          clauses,
          [[clause | groupAcc] | acc]
        )
    end
  end

  defp clause_count_bin_integer_segments(
         r_iclause(pats: [r_k_bin_seg(seg: r_k_literal()) = binSeg | _])
       ) do
    count_bin_integer_segments(binSeg, 0)
  end

  defp clause_count_bin_integer_segments(
         r_iclause(
           pats: [
             r_k_bin_seg(
               size: r_k_literal(val: size),
               unit: unit,
               type: :integer,
               flags: [:unsigned, :big],
               seg: r_k_var()
             )
             | _
           ]
         )
       )
       when rem(size * unit, 8) === 0 do
    {:variadic, div(size * unit, 8)}
  end

  defp clause_count_bin_integer_segments(_) do
    :error
  end

  defp count_bin_integer_segments(
         r_k_bin_seg(
           size: r_k_literal(val: 8),
           unit: 1,
           type: :integer,
           flags: [:unsigned, :big],
           seg: r_k_literal(val: int),
           next: next
         ),
         count
       )
       when is_integer(int) and 0 <= int and int <= 255 do
    count_bin_integer_segments(next, count + 1)
  end

  defp count_bin_integer_segments(_, count) when count > 0 do
    {:literal, count}
  end

  defp count_bin_integer_segments(_, _Count) do
    :error
  end

  defp fix_count_without_variadic_segment(n) when n > 3 do
    2
  end

  defp fix_count_without_variadic_segment(n) do
    n
  end

  defp squeeze_clauses(clauses, size, count)
       when count >= 16 or
              size <= 1 do
    clauses
  end

  defp squeeze_clauses(clauses, size, _Count) do
    squeeze_clauses(clauses, size)
  end

  defp squeeze_clauses(
         [
           r_iclause(
             pats: [
               r_k_bin_seg(seg: r_k_literal()) = binSeg
               | pats
             ]
           ) = clause
           | clauses
         ],
         size
       ) do
    [
      r_iclause(clause,
        pats: [
          squeeze_segments(binSeg, 0, 0, size)
          | pats
        ]
      )
      | squeeze_clauses(clauses, size)
    ]
  end

  defp squeeze_clauses([], _Size) do
    []
  end

  defp squeeze_segments(
         r_k_bin_seg(size: sz, seg: r_k_literal(val: val) = lit) = binSeg,
         acc,
         size,
         1
       ) do
    r_k_bin_seg(binSeg,
      size: r_k_literal(sz, val: size + 8),
      seg: r_k_literal(lit, val: acc <<< 8 ||| val)
    )
  end

  defp squeeze_segments(r_k_bin_seg(seg: r_k_literal(val: val), next: next), acc, size, count) do
    squeeze_segments(next, acc <<< 8 ||| val, size + 8, count - 1)
  end

  defp squeeze_segments(r_k_bin_end(), acc, size, count) do
    :erlang.error({acc, size, count})
  end

  defp flat_reverse([head | tail], acc) do
    flat_reverse(tail, flat_reverse_1(head, acc))
  end

  defp flat_reverse([], acc) do
    acc
  end

  defp flat_reverse_1([head | tail], acc) do
    flat_reverse_1(tail, [head | acc])
  end

  defp flat_reverse_1([], acc) do
    acc
  end

  defp build_guard([]) do
    :fail
  end

  defp build_guard(cs) do
    r_k_guard(clauses: cs)
  end

  defp build_select(v, [tc | _] = tcs) do
    copy_anno(r_k_select(var: v, types: tcs), tc)
  end

  defp build_alt(:fail, then) do
    then
  end

  defp build_alt(first, then) do
    build_alt_1st_no_fail(first, then)
  end

  defp build_alt_1st_no_fail(first, :fail) do
    first
  end

  defp build_alt_1st_no_fail(first, then) do
    copy_anno(r_k_alt(first: first, then: then), first)
  end

  defp build_match(r_k_alt() = km) do
    copy_anno(r_k_match(body: km), km)
  end

  defp build_match(r_k_select() = km) do
    copy_anno(r_k_match(body: km), km)
  end

  defp build_match(r_k_guard() = km) do
    copy_anno(r_k_match(body: km), km)
  end

  defp build_match(km) do
    km
  end

  defp clause_arg(r_iclause(pats: [arg | _])) do
    arg
  end

  defp clause_con(c) do
    arg_con(clause_arg(c))
  end

  defp clause_val(c) do
    arg_val(clause_arg(c), c)
  end

  defp is_var_clause(c) do
    clause_con(c) === :k_var
  end

  defp arg_arg(r_ialias(pat: con)) do
    con
  end

  defp arg_arg(con) do
    con
  end

  defp arg_alias(r_ialias(vars: as)) do
    as
  end

  defp arg_alias(_Con) do
    []
  end

  defp arg_con(arg) do
    case arg_arg(arg) do
      r_k_cons() ->
        :k_cons

      r_k_tuple() ->
        :k_tuple

      r_k_map() ->
        :k_map

      r_k_binary() ->
        :k_binary

      r_k_bin_end() ->
        :k_bin_end

      r_k_bin_seg() ->
        :k_bin_seg

      r_k_var() ->
        :k_var

      r_k_literal(val: []) ->
        :k_nil

      r_k_literal(val: val) ->
        cond do
          is_atom(val) ->
            :k_atom

          is_integer(val) ->
            :k_int

          is_float(val) ->
            :k_float

          true ->
            :k_literal
        end
    end
  end

  defp arg_val(arg, c) do
    case arg_arg(arg) do
      r_k_literal(val: lit) ->
        lit

      r_k_tuple(es: es) ->
        length(es)

      r_k_bin_seg(size: s, unit: u, type: t, flags: fs) ->
        case s do
          r_k_var(name: v) ->
            r_iclause(isub: isub) = c
            {r_k_var(name: get_vsub(v, isub)), u, t, fs}

          _ ->
            {set_kanno(s, []), u, t, fs}
        end

      r_k_map(op: :exact, es: es) ->
        sort(
          fn a, b ->
            :erts_internal.cmp_term(a, b) < 0
          end,
          for r_k_map_pair(key: key) <- es do
            map_key_clean(key)
          end
        )
    end
  end

  defp ubody_used_vars(expr, st) do
    {_, used, _} = ubody(expr, :return, r_kern(st, funs: :ignore))
    used
  end

  defp ubody(r_iset(vars: [], arg: r_iletrec() = let, body: b0), br, st0) do
    st = iletrec_funs(let, st0)
    ubody(b0, br, st)
  end

  defp ubody(r_iset(vars: [], arg: r_k_literal(), body: b0), br, st0) do
    ubody(b0, br, st0)
  end

  defp ubody(r_iset(anno: a, vars: vs, arg: e0, body: b0), br, st0) do
    {e1, eu, st1} = uexpr(e0, {:break, vs}, st0)
    {b1, bu, st2} = ubody(b0, br, st1)
    ns = lit_list_vars(vs)
    used = union(eu, subtract(bu, ns))
    {r_k_seq(anno: a, arg: e1, body: b1), used, st2}
  end

  defp ubody(r_ivalues(anno: a, args: as), :return, st) do
    au = lit_list_vars(as)
    {r_k_return(anno: a, args: as), au, st}
  end

  defp ubody(r_ivalues(anno: a, args: as), {:break, _Vbs}, st) do
    au = lit_list_vars(as)
    {r_k_break(anno: a, args: as), au, st}
  end

  defp ubody(r_k_goto() = goto, _Br, st) do
    {goto, [], st}
  end

  defp ubody(e, :return, st0) do
    case is_enter_expr(e) do
      true ->
        uexpr(e, :return, st0)

      false ->
        {ea, pa, st1} = force_atomic(e, st0)
        ubody(pre_seq(pa, r_ivalues(args: [ea])), :return, st1)
    end
  end

  defp ubody(e, {:break, [_]} = break, st0) do
    {ea, pa, st1} = force_atomic(e, st0)
    ubody(pre_seq(pa, r_ivalues(args: [ea])), break, st1)
  end

  defp ubody(e, {:break, rs} = break, st0) do
    {vs, st1} = new_vars(length(rs), st0)
    iset = r_iset(vars: vs, arg: e)
    preSeq = pre_seq([iset], r_ivalues(args: vs))
    ubody(preSeq, break, st1)
  end

  defp iletrec_funs(r_iletrec(defs: fs), st0) do
    free =
      foldl(
        fn {_, r_ifun(vars: vs, body: fb0)}, free0 ->
          fbu = ubody_used_vars(fb0, st0)
          ns = lit_list_vars(vs)
          free1 = subtract(fbu, ns)
          union(free1, free0)
        end,
        [],
        fs
      )

    freeVs = make_vars(free)

    st1 =
      foldl(
        fn {n, r_ifun(vars: vs)}, lst ->
          store_free(n, length(vs), freeVs, lst)
        end,
        st0,
        fs
      )

    iletrec_funs_gen(fs, freeVs, st1)
  end

  defp iletrec_funs_gen(_, _, r_kern(funs: :ignore) = st) do
    st
  end

  defp iletrec_funs_gen(fs, freeVs, st) do
    foldl(
      fn {n, r_ifun(anno: fa, vars: vs, body: fb0)}, lst0 ->
        arity0 = length(vs)
        {fb1, _, lst1} = ubody(fb0, :return, lst0)
        arity = arity0 + length(freeVs)
        fun = make_fdef(fa, n, arity, vs ++ freeVs, fb1)
        r_kern(lst1, funs: [fun | r_kern(lst1, :funs)])
      end,
      st,
      fs
    )
  end

  defp is_enter_expr(r_k_try()) do
    true
  end

  defp is_enter_expr(r_k_call()) do
    true
  end

  defp is_enter_expr(r_k_match()) do
    true
  end

  defp is_enter_expr(r_k_letrec_goto()) do
    true
  end

  defp is_enter_expr(_) do
    false
  end

  defp uexpr(r_k_test(anno: a, op: op, args: as) = test, {:break, rs}, st) do
    [] = rs
    used = union(op_vars(op), lit_list_vars(as))
    {r_k_test(test, anno: a), used, st}
  end

  defp uexpr(r_iset(anno: a, vars: vs, arg: e0, body: b0), {:break, _} = br, st0) do
    ns = lit_list_vars(vs)
    {e1, eu, st1} = uexpr(e0, {:break, vs}, st0)
    {b1, bu, st2} = uexpr(b0, br, st1)
    used = union(eu, subtract(bu, ns))
    {r_k_seq(anno: a, arg: e1, body: b1), used, st2}
  end

  defp uexpr(r_k_call(anno: a, op: r_k_local(name: f, arity: ar) = op, args: as0) = call, br, st) do
    free = get_free(f, ar, st)
    as1 = as0 ++ free
    used = lit_list_vars(as1)

    {case br do
       {:break, rs} ->
         r_k_call(call, anno: a, op: r_k_local(op, arity: ar + length(free)), args: as1, ret: rs)

       :return ->
         r_k_enter(anno: a, op: r_k_local(op, arity: ar + length(free)), args: as1)
     end, used, st}
  end

  defp uexpr(r_k_call(anno: a, op: op, args: as) = call, {:break, rs}, st) do
    used = union(op_vars(op), lit_list_vars(as))
    {r_k_call(call, anno: a, ret: rs), used, st}
  end

  defp uexpr(r_k_call(anno: a, op: op, args: as), :return, st) do
    used = union(op_vars(op), lit_list_vars(as))
    {r_k_enter(anno: a, op: op, args: as), used, st}
  end

  defp uexpr(r_k_bif(anno: a, op: op, args: as) = bif, {:break, rs}, st0) do
    used = union(op_vars(op), lit_list_vars(as))
    {brs, st1} = bif_returns(op, rs, st0)
    {r_k_bif(bif, anno: a, ret: brs), used, st1}
  end

  defp uexpr(r_k_match(anno: a, body: b0), br, st0) do
    rs = break_rets(br)
    {b1, bu, st1} = umatch(b0, br, st0)
    {r_k_match(anno: a, body: b1, ret: rs), bu, st1}
  end

  defp uexpr(
         r_k_try(anno: a, arg: a0, vars: vs, body: b0, evars: evs, handler: h0),
         {:break, rs0} = br,
         st0
       ) do
    case {vs, b0, h0, rs0} do
      {[r_k_var(name: x)], r_k_var(name: x), r_k_literal(), []} ->
        {a1, bu, st} = ubody(a0, {:break, []}, st0)

        {r_k_try(
           anno: a,
           arg: a1,
           vars: [],
           body: r_k_break(),
           evars: [],
           handler: r_k_break(),
           ret: rs0
         ), bu, st}

      {_, _, _, _} ->
        {avs, st1} = new_vars(length(vs), st0)
        {a1, au, st2} = ubody(a0, {:break, avs}, st1)
        {b1, bu, st3} = ubody(b0, br, st2)
        {h1, hu, st4} = ubody(h0, br, st3)
        used = union([au, subtract(bu, lit_list_vars(vs)), subtract(hu, lit_list_vars(evs))])

        {r_k_try(anno: a, arg: a1, vars: vs, body: b1, evars: evs, handler: h1, ret: rs0), used,
         st4}
    end
  end

  defp uexpr(r_k_try(anno: a, arg: a0, vars: vs, body: b0, evars: evs, handler: h0), :return, st0) do
    {avs, st1} = new_vars(length(vs), st0)
    {a1, au, st2} = ubody(a0, {:break, avs}, st1)
    {b1, bu, st3} = ubody(b0, :return, st2)
    {h1, hu, st4} = ubody(h0, :return, st3)
    used = union([au, subtract(bu, lit_list_vars(vs)), subtract(hu, lit_list_vars(evs))])
    {r_k_try_enter(anno: a, arg: a1, vars: vs, body: b1, evars: evs, handler: h1), used, st4}
  end

  defp uexpr(r_k_catch(anno: a, body: b0), {:break, rs0}, st0) do
    {rb, st1} = new_var(st0)
    {b1, bu, st2} = ubody(b0, {:break, [rb]}, st1)
    {ns, st3} = new_vars(1 - length(rs0), st2)
    rs1 = rs0 ++ ns
    {r_k_catch(anno: a, body: b1, ret: rs1), bu, st3}
  end

  defp uexpr(r_ifun(anno: a, vars: vs, body: b0), {:break, rs}, st0) do
    {b1, bu, st1} = ubody(b0, :return, st0)
    ns = lit_list_vars(vs)
    free = subtract(bu, ns)
    fvs = make_vars(free)
    arity = length(vs) + length(free)

    {fname, st} =
      case keyfind(:id, 1, a) do
        {:id, {_, _, fname0}} ->
          {fname0, st1}

        false ->
          new_fun_name(st1)
      end

    fun = make_fdef(a, fname, arity, vs ++ fvs, b1)
    local = r_k_local(name: fname, arity: arity)

    {r_k_bif(
       anno: a,
       op: r_k_internal(name: :make_fun, arity: length(free) + 2),
       args: [local | fvs],
       ret: rs
     ), free, add_local_function(fun, st)}
  end

  defp uexpr(r_k_letrec_goto(anno: a, first: f0, then: t0) = matchAlt, br, st0) do
    rs = break_rets(br)
    {f1, fu, st1} = ubody(f0, br, st0)
    {t1, tu, st2} = ubody(t0, br, st1)
    used = union(fu, tu)
    {r_k_letrec_goto(matchAlt, anno: a, first: f1, then: t1, ret: rs), used, st2}
  end

  defp uexpr(lit, {:break, rs0}, st0) do
    used = lit_vars(lit)
    {rs, st1} = ensure_return_vars(rs0, st0)
    {r_k_put(anno: get_kanno(lit), arg: lit, ret: rs), used, st1}
  end

  defp add_local_function(_, r_kern(funs: :ignore) = st) do
    st
  end

  defp add_local_function(
         r_k_fdef(func: name, arity: arity) = f,
         r_kern(funs: funs) = st
       ) do
    case is_defined(name, arity, funs) do
      false ->
        r_kern(st, funs: [f | funs])

      true ->
        st
    end
  end

  defp is_defined(name, arity, [r_k_fdef(func: name, arity: arity) | _]) do
    true
  end

  defp is_defined(name, arity, [r_k_fdef() | t]) do
    is_defined(name, arity, t)
  end

  defp is_defined(_, _, []) do
    false
  end

  defp make_fdef(anno, name, arity, vs, r_k_match() = body) do
    r_k_fdef(anno: anno, func: name, arity: arity, vars: vs, body: body)
  end

  defp make_fdef(anno, name, arity, vs, body) do
    ka = get_kanno(body)
    match = r_k_match(anno: ka, body: body, ret: [])
    r_k_fdef(anno: anno, func: name, arity: arity, vars: vs, body: match)
  end

  defp get_free(f, a, r_kern(free: freeMap)) do
    key = {f, a}

    case freeMap do
      %{^key => val} ->
        val

      _ ->
        []
    end
  end

  defp store_free(f, a, free, r_kern(free: freeMap0) = st) do
    key = {f, a}
    freeMap = %{freeMap0 | key => free}
    r_kern(st, free: freeMap)
  end

  defp break_rets({:break, rs}) do
    rs
  end

  defp break_rets(:return) do
    []
  end

  defp bif_returns(r_k_remote(mod: m, name: n, arity: ar), rs, st0) do
    {ns, st1} =
      new_vars(
        bif_vals(m, n, ar) - length(rs),
        st0
      )

    {rs ++ ns, st1}
  end

  defp bif_returns(r_k_internal(name: n, arity: ar), rs, st0) do
    {ns, st1} = new_vars(bif_vals(n, ar) - length(rs), st0)
    {rs ++ ns, st1}
  end

  defp ensure_return_vars([], st) do
    new_vars(1, st)
  end

  defp ensure_return_vars([_] = rs, st) do
    {rs, st}
  end

  defp umatch(r_k_alt(anno: a, first: f0, then: t0), br, st0) do
    {f1, fu, st1} = umatch(f0, br, st0)
    {t1, tu, st2} = umatch(t0, br, st1)
    used = union(fu, tu)
    {r_k_alt(anno: a, first: f1, then: t1), used, st2}
  end

  defp umatch(r_k_select(anno: a, var: v, types: ts0), br, st0) do
    {ts1, tus, st1} = umatch_list(ts0, br, st0)
    used = add_element(r_k_var(v, :name), tus)
    {r_k_select(anno: a, var: v, types: ts1), used, st1}
  end

  defp umatch(r_k_type_clause(anno: a, type: t, values: vs0), br, st0) do
    {vs1, vus, st1} = umatch_list(vs0, br, st0)
    {r_k_type_clause(anno: a, type: t, values: vs1), vus, st1}
  end

  defp umatch(r_k_val_clause(anno: a, val: p0, body: b0), br, st0) do
    {u0, ps} = pat_vars(p0)
    {b1, bu, st1} = umatch(b0, br, st0)
    p = pat_anno_unused(p0, bu, ps)
    used = union(u0, subtract(bu, ps))
    {r_k_val_clause(anno: a, val: p, body: b1), used, st1}
  end

  defp umatch(r_k_guard(anno: a, clauses: gs0), br, st0) do
    {gs1, gus, st1} = umatch_list(gs0, br, st0)
    {r_k_guard(anno: a, clauses: gs1), gus, st1}
  end

  defp umatch(r_k_guard_clause(anno: a, guard: g0, body: b0), br, st0) do
    {g1, gu, st1} = uexpr(g0, {:break, []}, st0)
    {b1, bu, st2} = umatch(b0, br, st1)
    used = union(gu, bu)
    {r_k_guard_clause(anno: a, guard: g1, body: b1), used, st2}
  end

  defp umatch(b0, br, st0) do
    ubody(b0, br, st0)
  end

  defp umatch_list(ms0, br, st) do
    foldr(
      fn m0, {ms1, us, sta} ->
        {m1, mu, stb} = umatch(m0, br, sta)
        {[m1 | ms1], union(mu, us), stb}
      end,
      {[], [], st},
      ms0
    )
  end

  defp pat_anno_unused(r_k_tuple(es: es0) = p, used0, ps) do
    used = intersection(used0, ps)

    es =
      for r_k_var(name: v) = var <- es0 do
        case member(v, used) do
          true ->
            var

          false ->
            set_kanno(var, [:unused | get_kanno(var)])
        end
      end

    r_k_tuple(p, es: es)
  end

  defp pat_anno_unused(p, _Used, _Ps) do
    p
  end

  defp op_vars(r_k_remote(mod: mod, name: name)) do
    :ordsets.from_list(
      for r_k_var(name: v) <- [mod, name] do
        v
      end
    )
  end

  defp op_vars(r_k_internal()) do
    []
  end

  defp op_vars(atomic) do
    lit_vars(atomic)
  end

  defp lit_vars(r_k_var(name: n)) do
    [n]
  end

  defp lit_vars(r_k_cons(hd: h, tl: t)) do
    union(lit_vars(h), lit_vars(t))
  end

  defp lit_vars(r_k_map(var: var, es: es)) do
    lit_list_vars([var | es])
  end

  defp lit_vars(r_k_map_pair(key: k, val: v)) do
    union(lit_vars(k), lit_vars(v))
  end

  defp lit_vars(r_k_binary(segs: v)) do
    lit_vars(v)
  end

  defp lit_vars(r_k_bin_end()) do
    []
  end

  defp lit_vars(r_k_bin_seg(size: size, seg: s, next: n)) do
    union(lit_vars(size), union(lit_vars(s), lit_vars(n)))
  end

  defp lit_vars(r_k_tuple(es: es)) do
    lit_list_vars(es)
  end

  defp lit_vars(r_k_literal()) do
    []
  end

  defp lit_list_vars(ps) do
    foldl(
      fn p, vs ->
        union(lit_vars(p), vs)
      end,
      [],
      ps
    )
  end

  defp pat_vars(r_k_var(name: n)) do
    {[], [n]}
  end

  defp pat_vars(r_k_literal()) do
    {[], []}
  end

  defp pat_vars(r_k_cons(hd: h, tl: t)) do
    pat_list_vars([h, t])
  end

  defp pat_vars(r_k_binary(segs: v)) do
    pat_vars(v)
  end

  defp pat_vars(r_k_bin_seg(size: size, seg: s, next: n)) do
    {u1, new} = pat_list_vars([s, n])
    {[], u2} = pat_vars(size)
    {union(u1, u2), new}
  end

  defp pat_vars(r_k_bin_int(size: size, next: n)) do
    {[], new} = pat_vars(n)
    {[], u} = pat_vars(size)
    {u, new}
  end

  defp pat_vars(r_k_bin_end()) do
    {[], []}
  end

  defp pat_vars(r_k_tuple(es: es)) do
    pat_list_vars(es)
  end

  defp pat_vars(r_k_map(es: es)) do
    pat_list_vars(es)
  end

  defp pat_vars(r_k_map_pair(key: k, val: v)) do
    {u1, new} = pat_vars(v)
    {[], u2} = pat_vars(k)
    {union(u1, u2), new}
  end

  defp pat_list_vars(ps) do
    foldl(
      fn p, {used0, new0} ->
        {used, new} = pat_vars(p)
        {union(used0, used), union(new0, new)}
      end,
      {[], []},
      ps
    )
  end

  defp integers(n, m) when n <= m do
    [n | integers(n + 1, m)]
  end

  defp integers(_, _) do
    []
  end

  def format_error({:nomatch_shadow, line}) do
    m =
      :io_lib.format(
        'this clause cannot match because a previous clause at line ~p always matches',
        [line]
      )

    flatten(m)
  end

  def format_error(:nomatch_shadow) do
    'this clause cannot match because a previous clause always matches'
  end

  def format_error(:bad_call) do
    'invalid module and/or function name; this call will always fail'
  end

  def format_error(:bad_segment_size) do
    'binary construction will fail because of a type mismatch'
  end

  defp add_warning(:none, term, anno, r_kern(ws: ws) = st) do
    file = get_file(anno)
    r_kern(st, ws: [{file, [{:none, :v3_kernel, term}]} | ws])
  end

  defp add_warning(line, term, anno, r_kern(ws: ws) = st) do
    file = get_file(anno)
    r_kern(st, ws: [{file, [{line, :v3_kernel, term}]} | ws])
  end

  defp is_compiler_generated(ke) do
    anno = get_kanno(ke)
    member(:compiler_generated, anno)
  end
end
