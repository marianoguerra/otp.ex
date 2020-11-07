defmodule :m_core_lint do
  use Bitwise
  import :lists, only: [all: 2, foldl: 3, reverse: 1]
  import :ordsets, only: [add_element: 2, is_element: 2, union: 2]
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
  Record.defrecord(:r_lint, :lint, module: :undefined, func: :undefined, errors: [], warnings: [])

  def format_error(:invalid_attributes) do
    'invalid attributes'
  end

  def format_error(:invalid_exports) do
    'invalid exports'
  end

  def format_error({:arg_mismatch, {f, a}}) do
    :io_lib.format('argument count mismatch in ~w/~w', [f, a])
  end

  def format_error({:illegal_expr, {f, a}}) do
    :io_lib.format('illegal expression in ~w/~w', [f, a])
  end

  def format_error({:illegal_guard, {f, a}}) do
    :io_lib.format('illegal guard expression in ~w/~w', [f, a])
  end

  def format_error({:illegal_try, {f, a}}) do
    :io_lib.format('illegal try expression in ~w/~w', [f, a])
  end

  def format_error({:not_bs_pattern, {f, a}}) do
    :io_lib.format('expecting bit syntax pattern in ~w/~w', [f, a])
  end

  def format_error({:not_pattern, {f, a}}) do
    :io_lib.format('expecting pattern in ~w/~w', [f, a])
  end

  def format_error({:not_var, {f, a}}) do
    :io_lib.format('expecting variable in ~w/~w', [f, a])
  end

  def format_error({:pattern_mismatch, {f, a}}) do
    :io_lib.format('pattern count mismatch in ~w/~w', [f, a])
  end

  def format_error({:return_mismatch, {f, a}}) do
    :io_lib.format('return count mismatch in ~w/~w', [f, a])
  end

  def format_error({:undefined_function, {f, a}}) do
    :io_lib.format('function ~w/~w undefined', [f, a])
  end

  def format_error({:duplicate_var, n, {f, a}}) do
    :io_lib.format('duplicate variable ~p in ~w/~w', [n, f, a])
  end

  def format_error({:unbound_var, n, {f, a}}) do
    :io_lib.format('unbound variable ~p in ~w/~w', [n, f, a])
  end

  def format_error({:undefined_function, {f1, a1}, {f2, a2}}) do
    :io_lib.format('undefined function ~w/~w in ~w/~w', [f1, a1, f2, a2])
  end

  def format_error({:tail_segment_not_at_end, {f, a}}) do
    :io_lib.format('binary tail segment not at end in ~w/~w', [f, a])
  end

  def module(m) do
    module(m, [])
  end

  def module(
        r_c_module(name: m, exports: es, attrs: as, defs: ds),
        _Opts
      ) do
    defined = defined_funcs(ds)
    st0 = r_lint(module: r_c_literal(m, :val))
    st1 = check_exports(es, st0)
    st2 = check_attrs(as, st1)
    st3 = module_defs(ds, defined, st2)
    st4 = check_state(es, defined, st3)
    return_status(st4)
  end

  defp defined_funcs(fs) do
    foldl(
      fn {r_c_var(name: {_I, _A} = iA), _}, def__ ->
        add_element(iA, def__)
      end,
      [],
      fs
    )
  end

  defp return_status(st) do
    ws = reverse(r_lint(st, :warnings))

    case reverse(r_lint(st, :errors)) do
      [] ->
        {:ok, [{r_lint(st, :module), ws}]}

      es ->
        {:error, [{r_lint(st, :module), es}], [{r_lint(st, :module), ws}]}
    end
  end

  defp add_error(e, st) do
    r_lint(st, errors: [{:none, :core_lint, e} | r_lint(st, :errors)])
  end

  defp check_exports(es, st) do
    case all(
           fn
             r_c_var(name: {name, arity})
             when is_atom(name) and is_integer(arity) ->
               true

             _ ->
               false
           end,
           es
         ) do
      true ->
        st

      false ->
        add_error(:invalid_exports, st)
    end
  end

  defp check_attrs(as, st) do
    case all(
           fn
             {r_c_literal(), r_c_literal()} ->
               true

             _ ->
               false
           end,
           as
         ) do
      true ->
        st

      false ->
        add_error(:invalid_attributes, st)
    end
  end

  defp check_state(es, defined, st) do
    foldl(
      fn r_c_var(name: {_N, _A} = f), st1 ->
        case is_element(f, defined) do
          true ->
            st1

          false ->
            add_error({:undefined_function, f}, st)
        end
      end,
      st,
      es
    )
  end

  defp module_defs(b, def__, st) do
    foldl(
      fn func, st0 ->
        {r_c_var(name: {_F, _A} = fA), _} = func
        st1 = r_lint(st0, func: fA)
        function(func, def__, st1)
      end,
      st,
      b
    )
  end

  defp functions(fs, def__, rt, st0) do
    foldl(
      fn
        {_Name, r_c_fun(vars: vs, body: b)}, sti0 ->
          {vvs, st} = variable_list(vs, sti0)
          body(b, union(vvs, def__), rt, st)

        _, st ->
          add_error({:illegal_expr, r_lint(st, :func)}, st)
      end,
      st0,
      fs
    )
  end

  defp function({r_c_var(name: {_, _}), b}, def__, st) do
    case b do
      r_c_fun() ->
        expr(b, def__, 1, st)

      _ ->
        add_error({:illegal_expr, r_lint(st, :func)}, st)
    end
  end

  defp body(r_c_values(es: es), def__, rt, st) do
    return_match(rt, length(es), expr_list(es, def__, st))
  end

  defp body(e, def__, rt, st0) do
    st1 = expr(e, def__, rt, st0)

    case is_simple_top(e) do
      true ->
        return_match(rt, 1, st1)

      false ->
        st1
    end
  end

  defp guard(expr, def__, st) do
    gexpr(expr, def__, 1, st)
  end

  defp gbody(r_c_values(es: es), def__, rt, st) do
    return_match(rt, length(es), gexpr_list(es, def__, st))
  end

  defp gbody(e, def__, rt, st0) do
    st1 = gexpr(e, def__, rt, st0)

    case is_simple_top(e) do
      true ->
        return_match(rt, 1, st1)

      false ->
        st1
    end
  end

  defp gexpr(r_c_var(name: n), def__, rt, st)
       when is_atom(n) or
              is_integer(n) do
    return_match(rt, 1, expr_var(n, def__, st))
  end

  defp gexpr(r_c_literal(), _Def, rt, st) do
    return_match(rt, 1, st)
  end

  defp gexpr(r_c_cons(hd: h, tl: t), def__, rt, st) do
    return_match(rt, 1, gexpr_list([h, t], def__, st))
  end

  defp gexpr(r_c_tuple(es: es), def__, rt, st) do
    return_match(rt, 1, gexpr_list(es, def__, st))
  end

  defp gexpr(r_c_map(es: es), def__, rt, st) do
    return_match(rt, 1, gexpr_list(es, def__, st))
  end

  defp gexpr(r_c_map_pair(key: k, val: v), def__, rt, st) do
    return_match(rt, 1, gexpr_list([k, v], def__, st))
  end

  defp gexpr(r_c_binary(segments: ss), def__, rt, st) do
    return_match(rt, 1, gbitstr_list(ss, def__, st))
  end

  defp gexpr(r_c_seq(arg: arg, body: b), def__, rt, st0) do
    st1 = gexpr(arg, def__, 1, st0)
    return_match(rt, 1, gbody(b, def__, rt, st1))
  end

  defp gexpr(r_c_let(vars: vs, arg: arg, body: b), def__, rt, st0) do
    st1 = gbody(arg, def__, let_varcount(vs), st0)
    {lvs, st2} = variable_list(vs, st1)
    gbody(b, union(lvs, def__), rt, st2)
  end

  defp gexpr(
         r_c_call(
           module: r_c_literal(val: :erlang),
           name: r_c_literal(val: :is_record),
           args: [arg, r_c_literal(val: tag), r_c_literal(val: size)]
         ),
         def__,
         rt,
         st
       )
       when is_atom(tag) and is_integer(size) do
    return_match(rt, 1, gexpr(arg, def__, 1, st))
  end

  defp gexpr(
         r_c_call(
           module: r_c_literal(val: :erlang),
           name: r_c_literal(val: :is_record)
         ),
         _Def,
         rt,
         st
       ) do
    return_match(rt, 1, add_error({:illegal_guard, r_lint(st, :func)}, st))
  end

  defp gexpr(
         r_c_call(module: r_c_literal(val: :erlang), name: r_c_literal(val: name), args: as),
         def__,
         rt,
         st0
       )
       when is_atom(name) do
    st1 = return_match(rt, 1, st0)
    arity = length(as)

    case is_guard_bif(name, arity) do
      true ->
        gexpr_list(as, def__, st1)

      false ->
        case {name, arity} do
          {:error, 1} ->
            gexpr_list(as, def__, st1)

          _ ->
            add_error({:illegal_guard, r_lint(st1, :func)}, st1)
        end
    end
  end

  defp gexpr(r_c_primop(name: r_c_literal(val: a), args: as), def__, _Rt, st0)
       when is_atom(a) do
    gexpr_list(as, def__, st0)
  end

  defp gexpr(
         r_c_try(
           arg: e,
           vars: [r_c_var(name: x)],
           body: r_c_var(name: x),
           evars: [r_c_var(), r_c_var()],
           handler: r_c_literal(val: false)
         ),
         def__,
         rt,
         st
       ) do
    gbody(e, def__, rt, st)
  end

  defp gexpr(r_c_case(arg: arg, clauses: cs), def__, rt, st0) do
    patCount = case_patcount(cs)
    st1 = gbody(arg, def__, patCount, st0)
    clauses(cs, def__, patCount, rt, st1)
  end

  defp gexpr(_Core, _, _, st) do
    add_error({:illegal_guard, r_lint(st, :func)}, st)
  end

  defp gexpr_list(es, def__, st0) do
    foldl(
      fn e, st ->
        gexpr(e, def__, 1, st)
      end,
      st0,
      es
    )
  end

  defp gbitstr_list(es, def__, st0) do
    foldl(
      fn e, st ->
        gbitstr(e, def__, st)
      end,
      st0,
      es
    )
  end

  defp gbitstr(r_c_bitstr(val: v, size: s), def__, st) do
    gexpr_list([v, s], def__, st)
  end

  defp is_guard_bif(name, arity) do
    :erl_internal.guard_bif(
      name,
      arity
    ) or
      :erl_internal.arith_op(
        name,
        arity
      ) or
      :erl_internal.bool_op(
        name,
        arity
      ) or
      :erl_internal.comp_op(
        name,
        arity
      )
  end

  defp expr(r_c_var(name: {_, _} = fA), def__, rt, st) do
    return_match(rt, 1, expr_fname(fA, def__, st))
  end

  defp expr(r_c_var(name: n), def__, rt, st) do
    return_match(rt, 1, expr_var(n, def__, st))
  end

  defp expr(r_c_literal(), _Def, rt, st) do
    return_match(rt, 1, st)
  end

  defp expr(r_c_cons(hd: h, tl: t), def__, rt, st) do
    return_match(rt, 1, expr_list([h, t], def__, st))
  end

  defp expr(r_c_tuple(es: es), def__, rt, st) do
    return_match(rt, 1, expr_list(es, def__, st))
  end

  defp expr(r_c_map(es: es), def__, rt, st) do
    return_match(rt, 1, expr_list(es, def__, st))
  end

  defp expr(r_c_map_pair(key: k, val: v), def__, rt, st) do
    return_match(rt, 1, expr_list([k, v], def__, st))
  end

  defp expr(r_c_binary(segments: ss), def__, rt, st) do
    return_match(rt, 1, bitstr_list(ss, def__, st))
  end

  defp expr(r_c_fun(vars: vs, body: b), def__, rt, st0) do
    {vvs, st1} = variable_list(vs, st0)
    return_match(rt, 1, body(b, union(vvs, def__), 1, st1))
  end

  defp expr(r_c_seq(arg: arg, body: b), def__, rt, st0) do
    st1 = expr(arg, def__, 1, st0)
    body(b, def__, rt, st1)
  end

  defp expr(r_c_let(vars: vs, arg: arg, body: b), def__, rt, st0) do
    st1 = body(arg, def__, let_varcount(vs), st0)
    {lvs, st2} = variable_list(vs, st1)
    body(b, union(lvs, def__), rt, st2)
  end

  defp expr(r_c_letrec(defs: fs, body: b), def0, rt, st0) do
    def1 = union(defined_funcs(fs), def0)
    st1 = functions(fs, def1, rt, st0)
    body(b, def1, rt, r_lint(st1, func: r_lint(st0, :func)))
  end

  defp expr(r_c_case(arg: arg, clauses: cs), def__, rt, st0) do
    pc = case_patcount(cs)
    st1 = body(arg, def__, pc, st0)
    clauses(cs, def__, pc, rt, st1)
  end

  defp expr(r_c_receive(clauses: cs, timeout: t, action: a), def__, rt, st0) do
    st1 = expr(t, def__, 1, st0)
    st2 = body(a, def__, rt, st1)
    clauses(cs, def__, 1, rt, st2)
  end

  defp expr(r_c_apply(op: op, args: as), def__, _Rt, st0) do
    st1 = apply_op(op, def__, length(as), st0)
    return_match(:any, 1, expr_list(as, def__, st1))
  end

  defp expr(
         r_c_call(module: r_c_literal(val: :erlang), name: r_c_literal(val: name), args: as),
         def__,
         rt,
         st0
       )
       when is_atom(name) do
    st1 = expr_list(as, def__, st0)

    case :erl_bifs.is_exit_bif(:erlang, name, length(as)) do
      true ->
        st1

      false ->
        return_match(rt, 1, st1)
    end
  end

  defp expr(r_c_call(module: m, name: n, args: as), def__, _Rt, st0) do
    st1 = expr(m, def__, 1, st0)
    st2 = expr(n, def__, 1, st1)
    expr_list(as, def__, st2)
  end

  defp expr(r_c_primop(name: r_c_literal(val: a), args: as), def__, rt, st0)
       when is_atom(a) do
    st1 = expr_list(as, def__, st0)

    case a do
      :match_fail ->
        st1

      :recv_peek_message ->
        return_match(rt, 2, st1)

      _ ->
        return_match(rt, 1, st1)
    end
  end

  defp expr(r_c_catch(body: b), def__, rt, st) do
    return_match(rt, 1, body(b, def__, 1, st))
  end

  defp expr(r_c_try(arg: a, vars: vs, body: b, evars: evs, handler: h), def__, rt, st0) do
    st1 =
      case evs do
        [_, _, _] ->
          st0

        _ ->
          add_error({:illegal_try, r_lint(st0, :func)}, st0)
      end

    st2 = body(a, def__, let_varcount(vs), st1)
    {ns, st3} = variable_list(vs, st2)
    st4 = body(b, union(ns, def__), rt, st3)
    {ens, st5} = variable_list(evs, st4)
    body(h, union(ens, def__), rt, st5)
  end

  defp expr(_Other, _, _, st) do
    add_error({:illegal_expr, r_lint(st, :func)}, st)
  end

  defp expr_list(es, def__, st0) do
    foldl(
      fn e, st ->
        expr(e, def__, 1, st)
      end,
      st0,
      es
    )
  end

  defp bitstr_list(es, def__, st0) do
    foldl(
      fn e, st ->
        bitstr(e, def__, st)
      end,
      st0,
      es
    )
  end

  defp bitstr(r_c_bitstr(val: v, size: s), def__, st) do
    expr_list([v, s], def__, st)
  end

  defp apply_op(r_c_var(name: {_I, a} = iA), def__, ac, st0) do
    st1 = expr_fname(iA, def__, st0)
    arg_match(ac, a, st1)
  end

  defp apply_op(e, def__, _, st) do
    expr(e, def__, 1, st)
  end

  defp expr_var(n, def__, st) do
    case is_element(n, def__) do
      true ->
        st

      false ->
        add_error({:unbound_var, n, r_lint(st, :func)}, st)
    end
  end

  defp expr_fname(fname, def__, st) do
    case is_element(fname, def__) do
      true ->
        st

      false ->
        add_error(
          {:undefined_function, fname, r_lint(st, :func)},
          st
        )
    end
  end

  defp let_varcount([]) do
    :any
  end

  defp let_varcount(es) do
    length(es)
  end

  defp case_patcount([r_c_clause(pats: ps) | _]) do
    length(ps)
  end

  defp clauses(cs, def__, pc, rt, st0) do
    foldl(
      fn c, st ->
        clause(c, def__, pc, rt, st)
      end,
      st0,
      cs
    )
  end

  defp clause(r_c_clause(pats: ps, guard: g, body: b), def0, pc, rt, st0) do
    st1 = pattern_match(pc, length(ps), st0)
    {pvs, st2} = pattern_list(ps, def0, st1)
    def1 = union(pvs, def0)
    st3 = guard(g, def1, st2)
    body(b, def1, rt, st3)
  end

  defp variable(r_c_var(name: n), ps, st) do
    case is_element(n, ps) do
      true ->
        {[], add_error({:duplicate_var, n, r_lint(st, :func)}, st)}

      false ->
        {[n], st}
    end
  end

  defp variable(_, def__, st) do
    {def__, add_error({:not_var, r_lint(st, :func)}, st)}
  end

  defp variable_list(vs, st) do
    variable_list(vs, [], st)
  end

  defp variable_list(vs, ps, st) do
    foldl(
      fn v, {ps0, st0} ->
        {vvs, st1} = variable(v, ps0, st0)
        {union(vvs, ps0), st1}
      end,
      {ps, st},
      vs
    )
  end

  defp pattern(r_c_var(name: n), def__, ps, st) do
    pat_var(n, def__, ps, st)
  end

  defp pattern(r_c_literal(), _Def, ps, st) do
    {ps, st}
  end

  defp pattern(r_c_cons(hd: h, tl: t), def__, ps, st) do
    pattern_list([h, t], def__, ps, st)
  end

  defp pattern(r_c_tuple(es: es), def__, ps, st) do
    pattern_list(es, def__, ps, st)
  end

  defp pattern(r_c_map(es: es), def__, ps, st) do
    pattern_list(es, def__, ps, st)
  end

  defp pattern(r_c_map_pair(op: r_c_literal(val: :exact), key: k, val: v), def__, ps, st) do
    pat_map_expr(k, def__, st)
    pattern_list([v], def__, ps, st)
  end

  defp pattern(r_c_binary(segments: ss), def__, ps, st0) do
    st = pat_bin_tail_check(ss, st0)
    pat_bin(ss, def__, ps, st)
  end

  defp pattern(r_c_alias(var: v, pat: p), def__, ps, st0) do
    {vvs, st1} = variable(v, ps, st0)
    pattern(p, def__, union(vvs, ps), st1)
  end

  defp pattern(_Other, _, ps, st) do
    {ps, add_error({:not_pattern, r_lint(st, :func)}, st)}
  end

  defp pat_var(n, _Def, ps, st) do
    case is_element(n, ps) do
      true ->
        {ps, add_error({:duplicate_var, n, r_lint(st, :func)}, st)}

      false ->
        {add_element(n, ps), st}
    end
  end

  defp pat_bin(es, def__, ps0, st0) do
    foldl(
      fn e, {ps, st} ->
        pat_segment(e, def__, ps, st)
      end,
      {ps0, st0},
      es
    )
  end

  defp pat_segment(r_c_bitstr(val: v, size: s, type: t), def__, ps0, st0) do
    st1 = pat_bit_expr(s, t, def__, st0)
    pattern(v, def__, ps0, st1)
  end

  defp pat_segment(_, _, ps, st) do
    {ps, add_error({:not_bs_pattern, r_lint(st, :func)}, st)}
  end

  defp pat_bin_tail_check([r_c_bitstr(size: r_c_literal(val: :all))], st) do
    st
  end

  defp pat_bin_tail_check([r_c_bitstr(size: r_c_literal(val: :all)) | _], st) do
    add_error({:tail_segment_not_at_end, r_lint(st, :func)}, st)
  end

  defp pat_bin_tail_check([_ | ss], st) do
    pat_bin_tail_check(ss, st)
  end

  defp pat_bin_tail_check([], st) do
    st
  end

  defp pat_bit_expr(r_c_var(name: n), _, def__, st) do
    expr_var(n, def__, st)
  end

  defp pat_bit_expr(r_c_literal(), _, _, st) do
    st
  end

  defp pat_bit_expr(r_c_binary(), _, _Def, st) do
    st
  end

  defp pat_bit_expr(_, _, _, st) do
    add_error({:illegal_expr, r_lint(st, :func)}, st)
  end

  defp pat_map_expr(r_c_var(name: n), def__, st) do
    expr_var(n, def__, st)
  end

  defp pat_map_expr(r_c_literal(), _Def, st) do
    st
  end

  defp pat_map_expr(_, _, st) do
    add_error({:illegal_expr, r_lint(st, :func)}, st)
  end

  defp pattern_list(pats, def__, st) do
    pattern_list(pats, def__, [], st)
  end

  defp pattern_list(pats, def__, ps0, st0) do
    foldl(
      fn p, {ps, st} ->
        pattern(p, def__, ps, st)
      end,
      {ps0, st0},
      pats
    )
  end

  defp pattern_match(n, n, st) do
    st
  end

  defp pattern_match(_Req, _Sup, st) do
    add_error({:pattern_mismatch, r_lint(st, :func)}, st)
  end

  defp return_match(:any, _Sup, st) do
    st
  end

  defp return_match(n, n, st) do
    st
  end

  defp return_match(_Req, _Sup, st) do
    add_error({:return_mismatch, r_lint(st, :func)}, st)
  end

  defp arg_match(n, n, st) do
    st
  end

  defp arg_match(_Req, _Sup, st) do
    add_error({:arg_mismatch, r_lint(st, :func)}, st)
  end

  defp is_simple_top(r_c_var()) do
    true
  end

  defp is_simple_top(r_c_cons()) do
    true
  end

  defp is_simple_top(r_c_tuple()) do
    true
  end

  defp is_simple_top(r_c_binary()) do
    true
  end

  defp is_simple_top(r_c_literal()) do
    true
  end

  defp is_simple_top(_) do
    false
  end
end
