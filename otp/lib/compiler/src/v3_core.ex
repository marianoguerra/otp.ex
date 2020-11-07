defmodule :m_v3_core do
  use Bitwise
  import :cerl, only: [ann_c_cons: 3, ann_c_map: 3, ann_c_tuple: 2, c_tuple: 1]

  import :lists,
    only: [
      droplast: 1,
      duplicate: 2,
      foldl: 3,
      foldr: 3,
      foreach: 2,
      keyfind: 3,
      last: 1,
      map: 2,
      mapfoldl: 3,
      member: 2,
      reverse: 1,
      reverse: 2,
      sort: 1,
      splitwith: 2
    ]

  import :ordsets,
    only: [
      add_element: 2,
      del_element: 2,
      intersection: 2,
      is_element: 2,
      subtract: 2,
      union: 1,
      union: 2
    ]

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
  Record.defrecord(:r_a, :a, us: [], ns: [], anno: [])

  Record.defrecord(:r_iapply, :iapply,
    anno: :EFE_TODO_NESTED_RECORD,
    op: :undefined,
    args: :undefined
  )

  Record.defrecord(:r_ibinary, :ibinary,
    anno: :EFE_TODO_NESTED_RECORD,
    segments: :undefined
  )

  Record.defrecord(:r_ibitstr, :ibitstr,
    anno: :EFE_TODO_NESTED_RECORD,
    val: :undefined,
    size: :undefined,
    unit: :undefined,
    type: :undefined,
    flags: :undefined
  )

  Record.defrecord(:r_icall, :icall,
    anno: :EFE_TODO_NESTED_RECORD,
    module: :undefined,
    name: :undefined,
    args: :undefined
  )

  Record.defrecord(:r_icase, :icase,
    anno: :EFE_TODO_NESTED_RECORD,
    args: :undefined,
    clauses: :undefined,
    fc: :undefined
  )

  Record.defrecord(:r_icatch, :icatch,
    anno: :EFE_TODO_NESTED_RECORD,
    body: :undefined
  )

  Record.defrecord(:r_iclause, :iclause,
    anno: :EFE_TODO_NESTED_RECORD,
    pats: :undefined,
    guard: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_ifun, :ifun,
    anno: :EFE_TODO_NESTED_RECORD,
    id: :undefined,
    vars: :undefined,
    clauses: :undefined,
    fc: :undefined,
    name: :unnamed
  )

  Record.defrecord(:r_iletrec, :iletrec,
    anno: :EFE_TODO_NESTED_RECORD,
    defs: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_imatch, :imatch,
    anno: :EFE_TODO_NESTED_RECORD,
    pat: :undefined,
    guard: [],
    arg: :undefined,
    fc: :undefined
  )

  Record.defrecord(:r_imap, :imap,
    anno: :EFE_TODO_NESTED_RECORD,
    arg: :EFE_TODO_NESTED_RECORD,
    es: :undefined,
    is_pat: false
  )

  Record.defrecord(:r_imappair, :imappair,
    anno: :EFE_TODO_NESTED_RECORD,
    op: :undefined,
    key: :undefined,
    val: :undefined
  )

  Record.defrecord(:r_iprimop, :iprimop,
    anno: :EFE_TODO_NESTED_RECORD,
    name: :undefined,
    args: :undefined
  )

  Record.defrecord(:r_iprotect, :iprotect,
    anno: :EFE_TODO_NESTED_RECORD,
    body: :undefined
  )

  Record.defrecord(:r_ireceive1, :ireceive1,
    anno: :EFE_TODO_NESTED_RECORD,
    clauses: :undefined
  )

  Record.defrecord(:r_ireceive2, :ireceive2,
    anno: :EFE_TODO_NESTED_RECORD,
    clauses: :undefined,
    timeout: :undefined,
    action: :undefined
  )

  Record.defrecord(:r_iset, :iset, anno: :EFE_TODO_NESTED_RECORD, var: :undefined, arg: :undefined)

  Record.defrecord(:r_itry, :itry,
    anno: :EFE_TODO_NESTED_RECORD,
    args: :undefined,
    vars: :undefined,
    body: :undefined,
    evars: :undefined,
    handler: :undefined
  )

  Record.defrecord(:r_ifilter, :ifilter,
    anno: :EFE_TODO_NESTED_RECORD,
    arg: :undefined
  )

  Record.defrecord(:r_igen, :igen,
    anno: :EFE_TODO_NESTED_RECORD,
    acc_pat: :undefined,
    acc_guard: :undefined,
    skip_pat: :undefined,
    tail: :undefined,
    tail_pat: :undefined,
    arg: :undefined
  )

  Record.defrecord(:r_isimple, :isimple,
    anno: :EFE_TODO_NESTED_RECORD,
    term: :undefined
  )

  Record.defrecord(:r_core, :core,
    vcount: 0,
    fcount: 0,
    gcount: 0,
    function: {:none, 0},
    in_guard: false,
    wanted: true,
    opts: [],
    dialyzer: false,
    ws: [],
    file: [{:file, ''}]
  )

  Record.defrecord(:r_imodule, :imodule,
    name: [],
    exports: :ordsets.new(),
    attrs: [],
    defs: [],
    file: [],
    opts: [],
    ws: []
  )

  def module(forms0, opts) do
    forms = :erl_internal.add_predefined_functions(forms0)

    module =
      foldl(
        fn f, acc ->
          form(f, acc, opts)
        end,
        r_imodule(),
        forms
      )

    r_imodule(name: mod, exports: exp0, attrs: as0, defs: kfs0, ws: ws) = module

    exp =
      case member(:export_all, opts) do
        true ->
          defined_functions(forms)

        false ->
          exp0
      end

    cexp =
      for {_, _} = fA <- exp do
        r_c_var(name: fA)
      end

    as = reverse(as0)
    kfs = reverse(kfs0)
    {:ok, r_c_module(name: r_c_literal(val: mod), exports: cexp, attrs: as, defs: kfs), ws}
  end

  defp form({:function, _, _, _, _} = f0, module, opts) do
    r_imodule(file: file, defs: defs, ws: ws0) = module
    {f, ws} = function(f0, ws0, file, opts)
    r_imodule(module, defs: [f | defs], ws: ws)
  end

  defp form({:attribute, _, :module, mod}, module, _Opts) do
    true = is_atom(mod)
    r_imodule(module, name: mod)
  end

  defp form({:attribute, _, :file, {file, _Line}} = f, r_imodule(attrs: as) = module, _Opts) do
    r_imodule(module, file: file, attrs: [attribute(f) | as])
  end

  defp form({:attribute, _, :import, _}, module, _Opts) do
    module
  end

  defp form({:attribute, _, :export, es}, r_imodule(exports: exp0) = module, _Opts) do
    exp = :ordsets.union(:ordsets.from_list(es), exp0)
    r_imodule(module, exports: exp)
  end

  defp form({:attribute, _, _, _} = f, r_imodule(attrs: as) = module, _Opts) do
    r_imodule(module, attrs: [attribute(f) | as])
  end

  defp form(_, module, _Opts) do
    module
  end

  defp attribute({:attribute, a, name, val0}) do
    line = [:erl_anno.location(a)]

    val =
      cond do
        is_list(val0) ->
          val0

        true ->
          [val0]
      end

    {r_c_literal(val: name, anno: line), r_c_literal(val: val, anno: line)}
  end

  defp defined_functions(forms) do
    fs =
      for {:function, _, name, arity, _} <- forms do
        {name, arity}
      end

    :ordsets.from_list(fs)
  end

  defp function({:function, _, name, arity, cs0}, ws0, file, opts) do
    try do
      st0 =
        r_core(
          vcount: 0,
          function: {name, arity},
          opts: opts,
          dialyzer: member(:dialyzer, opts),
          ws: ws0,
          file: [{:file, file}]
        )

      {b0, st1} = body(cs0, name, arity, st0)
      {b1, st2} = ubody(b0, st1)
      {b2, st3} = cbody(b1, st2)
      {b3, r_core(ws: ws)} = lbody(b2, st3)
      {{r_c_var(name: {name, arity}), b3}, ws}
    catch
      class, error ->
        :io.fwrite('Function: ~w/~w\n', [name, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  defp body(cs0, name, arity, st0) do
    anno = lineno_anno(:erlang.element(2, hd(cs0)), st0)
    funAnno = [{:function, {name, arity}} | anno]
    {args0, st1} = new_vars(anno, arity, st0)
    args = reverse(args0)
    {cs1, st2} = clauses(cs0, st1)
    {ps, st3} = new_vars(arity, st2)
    fc = function_clause(ps, anno)
    {r_ifun(anno: r_a(anno: funAnno), id: [], vars: args, clauses: cs1, fc: fc), st3}
  end

  defp clauses([c0 | cs0], st0) do
    {c, st1} = clause(c0, st0)
    {cs, st2} = clauses(cs0, st1)
    {[c | cs], st2}
  end

  defp clauses([], st) do
    {[], st}
  end

  defp clause({:clause, lc, h0, g0, b0}, st0) do
    try do
      head(h0, st0)
    catch
      :nomatch ->
        st1 = add_warning(lc, :nomatch, st0)

        h1 =
          for p <- h0 do
            sanitize(p)
          end

        false = h0 === h1
        g1 = [[{:atom, lc, false}]]
        lcNoWarn = no_compiler_warning(lc)
        clause({:clause, lcNoWarn, h1, g1, b0}, st1)
    else
      {h1, st1} ->
        {g1, st2} = guard(g0, st1)
        {b1, st3} = exprs(b0, st2)
        anno = lineno_anno(lc, st3)
        {r_iclause(anno: r_a(anno: anno), pats: h1, guard: g1, body: b1), st3}
    end
  end

  defp clause_arity({:clause, _, h0, _, _}) do
    length(h0)
  end

  defp head(ps, st) do
    pattern_list(ps, st)
  end

  defp guard([], st) do
    {[], st}
  end

  defp guard(gs0, st0) do
    gs1 =
      foldr(
        fn gt0, rhs ->
          gt1 = guard_tests(gt0)
          l = :erlang.element(2, gt1)
          {:op, l, :or, gt1, rhs}
        end,
        guard_tests(last(gs0)),
        droplast(gs0)
      )

    {gs, st} = gexpr_top(gs1, r_core(st0, in_guard: true))
    {gs, r_core(st, in_guard: false)}
  end

  defp guard_tests(gs) do
    l = :erlang.element(2, hd(gs))

    {:protect, l,
     foldr(
       fn g, rhs ->
         {:op, l, :and, g, rhs}
       end,
       last(gs),
       droplast(gs)
     )}
  end

  defp gexpr_top(e0, st0) do
    {e1, eps0, bools, st1} = gexpr(e0, [], st0)
    {e, eps, st} = force_booleans(bools, e1, eps0, st1)
    {eps ++ [e], st}
  end

  defp gexpr({:protect, line, arg}, bools0, st0) do
    case gexpr(arg, [], st0) do
      {e0, [], bools, st1} ->
        {e, eps, st} = force_booleans(bools, e0, [], st1)
        {e, eps, bools0, st}

      {e0, eps0, bools, st1} ->
        {e, eps, st} = force_booleans(bools, e0, eps0, st1)
        anno = lineno_anno(line, st)
        {r_iprotect(anno: r_a(anno: anno), body: eps ++ [e]), [], bools0, st}
    end
  end

  defp gexpr({:op, _, :andalso, _, _} = e0, bools, st0) do
    {:op, l, :andalso, e1, e2} = right_assoc(e0, :andalso)
    anno = lineno_anno(l, st0)
    {r_c_var(name: v0), st} = new_var(anno, st0)
    v = {:var, l, v0}
    false__ = {:atom, l, false}
    e = make_bool_switch(l, e1, v, e2, false__)
    gexpr(e, bools, st)
  end

  defp gexpr({:op, _, :orelse, _, _} = e0, bools, st0) do
    {:op, l, :orelse, e1, e2} = right_assoc(e0, :orelse)
    anno = lineno_anno(l, st0)
    {r_c_var(name: v0), st} = new_var(anno, st0)
    v = {:var, l, v0}
    true__ = {:atom, l, true}
    e = make_bool_switch(l, e1, v, true__, e2)
    gexpr(e, bools, st)
  end

  defp gexpr({:op, line, op, l, r} = e, bools, st) do
    case :erl_internal.bool_op(op, 2) do
      true ->
        gexpr_bool(op, l, r, bools, st, line)

      false ->
        gexpr_test(e, bools, st)
    end
  end

  defp gexpr(
         {:call, line, {:remote, _, {:atom, _, :erlang}, {:atom, _, op}}, [l, r]} = e,
         bools,
         st
       ) do
    case :erl_internal.bool_op(op, 2) do
      true ->
        gexpr_bool(op, l, r, bools, st, line)

      false ->
        gexpr_test(e, bools, st)
    end
  end

  defp gexpr({:op, line, :not, a}, bools, st) do
    gexpr_not(a, bools, st, line)
  end

  defp gexpr({:call, line, {:remote, _, {:atom, _, :erlang}, {:atom, _, :not}}, [a]}, bools, st) do
    gexpr_not(a, bools, st, line)
  end

  defp gexpr(e0, bools, st0) do
    gexpr_test(e0, bools, st0)
  end

  defp gexpr_bool(op, l, r, bools0, st0, line) do
    {le, lps, bools1, st1} = gexpr(l, bools0, st0)
    {ll, llps, st2} = force_safe(le, st1)
    {re, rps, bools, st3} = gexpr(r, bools1, st2)
    {rl, rlps, st4} = force_safe(re, st3)
    anno = lineno_anno(line, st4)

    {r_icall(
       anno: r_a(anno: anno),
       module: r_c_literal(anno: anno, val: :erlang),
       name: r_c_literal(anno: anno, val: op),
       args: [ll, rl]
     ), lps ++ llps ++ rps ++ rlps, bools, st4}
  end

  defp gexpr_not(a, bools0, st0, line) do
    {ae0, aps, bools, st1} = gexpr(a, bools0, st0)

    case ae0 do
      r_icall(
        module: r_c_literal(val: :erlang),
        name: r_c_literal(val: :"=:="),
        args: [e, r_c_literal(val: true)]
      ) = eqCall ->
        ae = r_icall(eqCall, args: [e, r_c_literal(val: false)])
        {al, alps, st2} = force_safe(ae, st1)
        {al, aps ++ alps, bools, st2}

      ae ->
        {al, alps, st2} = force_safe(ae, st1)
        anno = lineno_anno(line, st2)

        {r_icall(
           anno: r_a(anno: anno),
           module: r_c_literal(anno: anno, val: :erlang),
           name: r_c_literal(anno: anno, val: :not),
           args: [al]
         ), aps ++ alps, bools, st2}
    end
  end

  defp gexpr_test({:atom, l, true}, bools, st0) do
    {r_c_literal(anno: lineno_anno(l, st0), val: true), [], bools, st0}
  end

  defp gexpr_test({:atom, l, false}, bools, st0) do
    {r_c_literal(anno: lineno_anno(l, st0), val: false), [], bools, st0}
  end

  defp gexpr_test(e0, bools0, st0) do
    {e1, eps0, st1} = expr(e0, st0)

    case e1 do
      r_icall(
        anno: anno,
        module: r_c_literal(val: :erlang),
        name: r_c_literal(val: :is_function),
        args: [_, _]
      ) ->
        lanno = r_a(anno, :anno)
        {new, st2} = new_var(lanno, st1)
        {icall_eq_true(new), eps0 ++ [r_iset(anno: anno, var: new, arg: e1)], bools0, st2}

      r_icall(anno: anno, module: r_c_literal(val: :erlang), name: r_c_literal(val: n), args: as) ->
        ar = length(as)

        case :erl_internal.new_type_test(
               n,
               ar
             ) or
               :erl_internal.comp_op(
                 n,
                 ar
               ) or
               :erl_internal.bool_op(
                 n,
                 ar
               ) do
          true ->
            {e1, eps0, bools0, st1}

          false ->
            lanno = r_a(anno, :anno)
            {new, st2} = new_var(lanno, st1)
            bools = [new | bools0]
            {icall_eq_true(new), eps0 ++ [r_iset(anno: anno, var: new, arg: e1)], bools, st2}
        end

      _ ->
        lanno = get_lineno_anno(e1)
        aCompGen = r_a(anno: [:compiler_generated])

        case is_simple(e1) do
          true ->
            bools = [e1 | bools0]
            {icall_eq_true(e1), eps0, bools, st1}

          false ->
            {new, st2} = new_var(lanno, st1)
            bools = [new | bools0]
            {icall_eq_true(new), eps0 ++ [r_iset(anno: aCompGen, var: new, arg: e1)], bools, st2}
        end
    end
  end

  defp icall_eq_true(arg) do
    r_icall(
      anno: r_a(anno: [:compiler_generated]),
      module: r_c_literal(val: :erlang),
      name: r_c_literal(val: :"=:="),
      args: [arg, r_c_literal(val: true)]
    )
  end

  defp force_booleans(vs0, e, eps, st) do
    vs1 =
      for v <- vs0 do
        set_anno(v, [])
      end

    vs = unforce(e, eps, vs1)
    force_booleans_1(vs, e, eps, st)
  end

  defp force_booleans_1([], e, eps, st) do
    {e, eps, st}
  end

  defp force_booleans_1([v | vs], e0, eps0, st0) do
    {e1, eps1, st1} = force_safe(e0, st0)
    aCompGen = r_a(anno: [:compiler_generated])

    call =
      r_icall(
        anno: aCompGen,
        module: r_c_literal(val: :erlang),
        name: r_c_literal(val: :is_boolean),
        args: [v]
      )

    {new, st} = new_var([], st1)
    iset = r_iset(var: new, arg: call)
    eps = eps0 ++ eps1 ++ [iset]

    e =
      r_icall(
        anno: aCompGen,
        module: r_c_literal(val: :erlang),
        name: r_c_literal(val: :and),
        args: [e1, new]
      )

    force_booleans_1(vs, e, eps, st)
  end

  defp unforce(_, _, []) do
    []
  end

  defp unforce(e, eps, vs) do
    tree = unforce_tree(eps ++ [e], :gb_trees.empty())
    unforce(tree, vs)
  end

  defp unforce_tree([r_iset(var: r_c_var(name: v), arg: arg0) | es], d0) do
    arg = unforce_tree_subst(arg0, d0)
    d = :gb_trees.insert(v, arg, d0)
    unforce_tree(es, d)
  end

  defp unforce_tree([r_icall() = call], d) do
    unforce_tree_subst(call, d)
  end

  defp unforce_tree([r_c_var(name: v)], d) do
    :gb_trees.get(v, d)
  end

  defp unforce_tree_subst(
         r_icall(
           module: r_c_literal(val: :erlang),
           name: r_c_literal(val: :"=:="),
           args: [_Expr, r_c_literal(val: bool)]
         ) = call,
         _
       )
       when is_boolean(bool) do
    call
  end

  defp unforce_tree_subst(r_icall(args: args0) = call, d) do
    args =
      map(
        fn
          r_c_var(name: v) = var ->
            case :gb_trees.lookup(v, d) do
              {:value, val} ->
                val

              :none ->
                var
            end

          expr ->
            expr
        end,
        args0
      )

    r_icall(call, args: args)
  end

  defp unforce_tree_subst(expr, _) do
    expr
  end

  defp unforce(
         r_icall(module: r_c_literal(val: :erlang), name: r_c_literal(val: name), args: args),
         vs0
       ) do
    case {name, args} do
      {:and, [arg1, arg2]} ->
        vs = unforce(arg1, vs0)
        unforce(arg2, vs)

      {:"=:=", [e, r_c_literal(val: bool)]} when is_boolean(bool) ->
        vs0 -- [set_anno(e, [])]

      {_, _} ->
        vs0
    end
  end

  defp unforce(_, vs) do
    vs
  end

  defp exprs([e0 | es0], st0) do
    {e1, eps, st1} = expr(e0, st0)
    {es1, st2} = exprs(es0, st1)
    {eps ++ [e1] ++ es1, st2}
  end

  defp exprs([], st) do
    {[], st}
  end

  defp expr({:var, l, v}, st) do
    {r_c_var(anno: lineno_anno(l, st), name: v), [], st}
  end

  defp expr({:char, l, c}, st) do
    {r_c_literal(anno: full_anno(l, st), val: c), [], st}
  end

  defp expr({:integer, l, i}, st) do
    {r_c_literal(anno: full_anno(l, st), val: i), [], st}
  end

  defp expr({:float, l, f}, st) do
    {r_c_literal(anno: full_anno(l, st), val: f), [], st}
  end

  defp expr({:atom, l, a}, st) do
    {r_c_literal(anno: full_anno(l, st), val: a), [], st}
  end

  defp expr({nil, l}, st) do
    {r_c_literal(anno: full_anno(l, st), val: []), [], st}
  end

  defp expr({:string, l, s}, st) do
    {r_c_literal(anno: full_anno(l, st), val: s), [], st}
  end

  defp expr({:cons, l, h0, t0}, st0) do
    {h1, hps, st1} = safe(h0, st0)
    {t1, tps, st2} = safe(t0, st1)
    a = full_anno(l, st2)
    {annotate_cons(a, h1, t1, st2), hps ++ tps, st2}
  end

  defp expr({:lc, l, e, qs0}, st0) do
    {qs1, st1} = preprocess_quals(l, qs0, st0)
    lc_tq(l, e, qs1, r_c_literal(anno: lineno_anno(l, st1), val: []), st1)
  end

  defp expr({:bc, l, e, qs}, st) do
    bc_tq(l, e, qs, st)
  end

  defp expr({:tuple, l, es0}, st0) do
    {es1, eps, st1} = safe_list(es0, st0)
    a = record_anno(l, st1)
    {annotate_tuple(a, es1, st1), eps, st1}
  end

  defp expr({:map, l, es0}, st0) do
    map_build_pairs(r_c_literal(val: %{}), es0, full_anno(l, st0), st0)
  end

  defp expr({:map, l, m, es}, st) do
    expr_map(m, es, l, st)
  end

  defp expr({:bin, l, es0}, st0) do
    try do
      expr_bin(es0, full_anno(l, st0), st0)
    catch
      {:bad_binary, eps, st1} ->
        st = add_warning(l, :bad_binary, st1)
        lineAnno = lineno_anno(l, st)
        as = [r_c_literal(anno: lineAnno, val: :badarg)]

        {r_icall(
           anno: r_a(anno: lineAnno),
           module: r_c_literal(anno: lineAnno, val: :erlang),
           name: r_c_literal(anno: lineAnno, val: :error),
           args: as
         ), eps, st}
    else
      {_, _, _} = res ->
        res
    end
  end

  defp expr({:block, _, es0}, st0) do
    {es1, st1} = exprs(droplast(es0), st0)
    {e1, eps, st2} = expr(last(es0), st1)
    {e1, es1 ++ eps, st2}
  end

  defp expr({:if, l, cs0}, st0) do
    {cs1, st1} = clauses(cs0, st0)
    lanno = lineno_anno(l, st1)
    fc = fail_clause([], lanno, r_c_literal(val: :if_clause))
    {r_icase(anno: r_a(anno: lanno), args: [], clauses: cs1, fc: fc), [], st1}
  end

  defp expr({:case, l, e0, cs0}, st0) do
    {e1, eps, st1} = novars(e0, st0)
    {cs1, st2} = clauses(cs0, st1)
    {fpat, st3} = new_var(st2)
    lanno = lineno_anno(l, st2)
    fc = fail_clause([fpat], lanno, c_tuple([r_c_literal(val: :case_clause), fpat]))
    {r_icase(anno: r_a(anno: lanno), args: [e1], clauses: cs1, fc: fc), eps, st3}
  end

  defp expr({:receive, l, cs0}, st0) do
    {cs1, st1} = clauses(cs0, st0)
    {r_ireceive1(anno: r_a(anno: lineno_anno(l, st1)), clauses: cs1), [], st1}
  end

  defp expr({:receive, l, cs0, te0, tes0}, st0) do
    {te1, teps, st1} = novars(te0, st0)
    {tes1, st2} = exprs(tes0, st1)
    {cs1, st3} = clauses(cs0, st2)

    {r_ireceive2(anno: r_a(anno: lineno_anno(l, st3)), clauses: cs1, timeout: te1, action: tes1),
     teps, st3}
  end

  defp expr({:try, l, es0, [], ecs, []}, st0) do
    {es1, st1} = exprs(es0, st0)
    {v, st2} = new_var(st1)
    {evs, hs, st3} = try_exception(ecs, st2)
    lanno = lineno_anno(l, st3)

    {r_itry(anno: r_a(anno: lanno), args: es1, vars: [v], body: [v], evars: evs, handler: hs), [],
     st3}
  end

  defp expr({:try, l, es0, cs0, ecs, []}, st0) do
    {es1, st1} = exprs(es0, st0)
    {v, st2} = new_var(st1)
    {cs1, st3} = clauses(cs0, st2)
    {fpat, st4} = new_var(st3)
    lanno = lineno_anno(l, st4)
    fc = fail_clause([fpat], lanno, c_tuple([r_c_literal(val: :try_clause), fpat]))
    {evs, hs, st5} = try_exception(ecs, st4)

    {r_itry(
       anno: r_a(anno: lineno_anno(l, st5)),
       args: es1,
       vars: [v],
       body: [r_icase(anno: r_a(anno: lanno), args: [v], clauses: cs1, fc: fc)],
       evars: evs,
       handler: hs
     ), [], st5}
  end

  defp expr({:try, l, es0, [], [], as0}, st0) do
    try_after(l, es0, as0, st0)
  end

  defp expr({:try, l, es, cs, ecs, as}, st0) do
    expr(
      {:try, l, [{:try, l, es, cs, ecs, []}], [], [], as},
      st0
    )
  end

  defp expr({:catch, l, e0}, st0) do
    {e1, eps, st1} = expr(e0, st0)
    lanno = lineno_anno(l, st1)
    {r_icatch(anno: r_a(anno: lanno), body: eps ++ [e1]), [], st1}
  end

  defp expr({:fun, l, {:function, f, a}}, st0) do
    {fname, st1} = new_fun_name(st0)
    lanno = full_anno(l, st1)
    id = {0, 0, fname}
    {r_c_var(anno: lanno ++ [{:id, id}], name: {f, a}), [], st1}
  end

  defp expr({:fun, l, {:function, m, f, a}}, st0) do
    {as, aps, st1} = safe_list([m, f, a], st0)
    lanno = full_anno(l, st1)

    {r_icall(
       anno: r_a(anno: lanno),
       module: r_c_literal(val: :erlang),
       name: r_c_literal(val: :make_fun),
       args: as
     ), aps, st1}
  end

  defp expr({:fun, l, {:clauses, cs}}, st) do
    fun_tq(cs, l, st, :unnamed)
  end

  defp expr({:named_fun, l, :_, cs}, st) do
    fun_tq(cs, l, st, :unnamed)
  end

  defp expr({:named_fun, l, name, cs}, st) do
    fun_tq(cs, l, st, {:named, name})
  end

  defp expr({:call, l, {:remote, _, m, f}, as0}, st0) do
    {[[m1, f1] | as1], aps, st1} =
      safe_list(
        [[m, f] | as0],
        st0
      )

    anno = full_anno(l, st1)
    {r_icall(anno: r_a(anno: anno), module: m1, name: f1, args: as1), aps, st1}
  end

  defp expr({:call, lc, {:atom, lf, f}, as0}, st0) do
    {as1, aps, st1} = safe_list(as0, st0)

    op =
      r_c_var(
        anno: lineno_anno(lf, st1),
        name: {f, length(as1)}
      )

    {r_iapply(anno: r_a(anno: lineno_anno(lc, st1)), op: op, args: as1), aps, st1}
  end

  defp expr({:call, l, funExp, as0}, st0) do
    {fun, fps, st1} = safe(funExp, st0)
    {as1, aps, st2} = safe_list(as0, st1)
    lanno = lineno_anno(l, st2)
    {r_iapply(anno: r_a(anno: lanno), op: fun, args: as1), fps ++ aps, st2}
  end

  defp expr({:match, l, p0, e0}, st0) do
    {p1, e1} = fold_match(e0, p0)

    st1 =
      case p1 do
        {:var, _, :_} ->
          r_core(st0, wanted: false)

        _ ->
          st0
      end

    {e2, eps1, st2} = novars(e1, st1)
    st3 = r_core(st2, wanted: r_core(st0, :wanted))

    {p2, st4} =
      try do
        pattern(p1, st3)
      catch
        thrown ->
          {thrown, st3}
      end

    {fpat, st5} = new_var(st4)
    lanno = lineno_anno(l, st5)
    fc = fail_clause([fpat], lanno, c_tuple([r_c_literal(val: :badmatch), fpat]))

    case p2 do
      :nomatch ->
        st6 = add_warning(l, :nomatch, st5)
        {expr, eps3, st7} = safe(e1, st6)
        sanPat0 = sanitize(p1)
        {sanPat, st} = pattern(sanPat0, st7)
        badmatch = c_tuple([r_c_literal(val: :badmatch), expr])

        fail =
          r_iprimop(anno: r_a(anno: lanno), name: r_c_literal(val: :match_fail), args: [badmatch])

        eps = eps3 ++ [fail]
        {r_imatch(anno: r_a(anno: lanno), pat: sanPat, arg: expr, fc: fc), eps, st}

      other when not is_atom(other) ->
        {p3, e3, eps2} = letify_aliases(p2, e2)
        eps = eps1 ++ eps2
        {r_imatch(anno: r_a(anno: lanno), pat: p3, arg: e3, fc: fc), eps, st5}
    end
  end

  defp expr({:op, _, :++, {:lc, llc, e, qs0}, more}, st0) do
    {mc, mps, st1} = safe(more, st0)
    {qs, st2} = preprocess_quals(llc, qs0, st1)
    {y, yps, st} = lc_tq(llc, e, qs, mc, st2)
    {y, mps ++ yps, st}
  end

  defp expr({:op, _, :andalso, _, _} = e0, st0) do
    {:op, l, :andalso, e1, e2} = right_assoc(e0, :andalso)
    anno = lineno_anno(l, st0)
    {r_c_var(name: v0), st} = new_var(anno, st0)
    v = {:var, l, v0}
    false__ = {:atom, l, false}
    e = make_bool_switch(l, e1, v, e2, false__)
    expr(e, st)
  end

  defp expr({:op, _, :orelse, _, _} = e0, st0) do
    {:op, l, :orelse, e1, e2} = right_assoc(e0, :orelse)
    anno = lineno_anno(l, st0)
    {r_c_var(name: v0), st} = new_var(anno, st0)
    v = {:var, l, v0}
    true__ = {:atom, l, true}
    e = make_bool_switch(l, e1, v, true__, e2)
    expr(e, st)
  end

  defp expr({:op, l, op, a0}, st0) do
    {a1, aps, st1} = safe(a0, st0)
    lineAnno = full_anno(l, st1)

    {r_icall(
       anno: r_a(anno: lineAnno),
       module: r_c_literal(anno: lineAnno, val: :erlang),
       name: r_c_literal(anno: lineAnno, val: op),
       args: [a1]
     ), aps, st1}
  end

  defp expr({:op, l, op, l0, r0}, st0) do
    {as, aps, st1} = safe_list([l0, r0], st0)
    lineAnno = full_anno(l, st1)

    {r_icall(
       anno: r_a(anno: lineAnno),
       module: r_c_literal(anno: lineAnno, val: :erlang),
       name: r_c_literal(anno: lineAnno, val: op),
       args: as
     ), aps, st1}
  end

  defp letify_aliases(r_c_alias(var: v, pat: p0), e0) do
    {p1, e1, eps0} = letify_aliases(p0, v)
    {p1, e1, [r_iset(var: v, arg: e0) | eps0]}
  end

  defp letify_aliases(p, e) do
    {p, e, []}
  end

  defp sanitize({:match, l, p1, p2}) do
    {:tuple, l, [sanitize(p1), sanitize(p2)]}
  end

  defp sanitize({:cons, l, h, t}) do
    {:cons, l, sanitize(h), sanitize(t)}
  end

  defp sanitize({:tuple, l, ps0}) do
    ps =
      for p <- ps0 do
        sanitize(p)
      end

    {:tuple, l, ps}
  end

  defp sanitize({:bin, l, segs0}) do
    segs =
      for {:bin_element, _, {:var, _, _} = var, _, _} <- segs0 do
        var
      end

    {:tuple, l, segs}
  end

  defp sanitize({:map, l, ps0}) do
    ps =
      for {:map_field_exact, _, _, v} <- ps0 do
        sanitize(v)
      end

    {:tuple, l, ps}
  end

  defp sanitize({:op, l, _Name, p1, p2}) do
    {:tuple, l, [sanitize(p1), sanitize(p2)]}
  end

  defp sanitize(p) do
    p
  end

  defp make_bool_switch(l, e, v, t, f) do
    negL = no_compiler_warning(l)
    error = {:tuple, negL, [{:atom, negL, :badarg}, v]}

    {:case, negL, e,
     [
       {:clause, negL, [{:atom, negL, true}], [], [t]},
       {:clause, negL, [{:atom, negL, false}], [], [f]},
       {:clause, negL, [v], [],
        [{:call, negL, {:remote, negL, {:atom, negL, :erlang}, {:atom, negL, :error}}, [error]}]}
     ]}
  end

  defp expr_map(m0, es0, l, st0) do
    {m1, eps0, st1} = safe_map(m0, st0)
    badmap = badmap_term(m1, st1)
    a = lineno_anno(l, st1)
    fc = fail_clause([], [{:eval_failure, :badmap} | a], badmap)
    {m2, eps1, st2} = map_build_pairs(m1, es0, full_anno(l, st1), st1)

    m3 =
      case es0 do
        [] ->
          m1

        [_ | _] ->
          m2
      end

    cs = [
      r_iclause(
        anno: r_a(anno: [:compiler_generated | a]),
        pats: [],
        guard: [
          r_icall(
            anno: r_a(anno: a),
            module: r_c_literal(anno: a, val: :erlang),
            name: r_c_literal(anno: a, val: :is_map),
            args: [m1]
          )
        ],
        body: [m3]
      )
    ]

    eps = eps0 ++ eps1
    {r_icase(anno: r_a(anno: a), args: [], clauses: cs, fc: fc), eps, st2}
  end

  defp safe_map(m0, st0) do
    case safe(m0, st0) do
      {r_c_var(), _, _} = res ->
        res

      {r_c_literal(val: map), _, _} = res when is_map(map) ->
        res

      {notMap, eps0, st1} ->
        {v, st2} = new_var(st1)
        anno = :cerl.get_ann(notMap)
        eps1 = [r_iset(anno: r_a(anno: anno), var: v, arg: notMap)]
        {v, eps0 ++ eps1, st2}
    end
  end

  defp badmap_term(_Map, r_core(in_guard: true)) do
    r_c_literal(val: :badmap)
  end

  defp badmap_term(map, r_core(in_guard: false)) do
    c_tuple([r_c_literal(val: :badmap), map])
  end

  defp map_build_pairs(map, es0, ann, st0) do
    {es, pre, _, st1} = map_build_pairs_1(es0, :cerl_sets.new(), st0)
    {ann_c_map(ann, map, es), pre, st1}
  end

  defp map_build_pairs_1([{op0, l, k0, v0} | es], used0, st0) do
    {k, pre0, st1} = safe(k0, st0)
    {v, pre1, st2} = safe(v0, st1)
    {pairs, pre2, used1, st3} = map_build_pairs_1(es, used0, st2)
    as = lineno_anno(l, st3)
    op = map_op(op0)
    {used2, st4} = maybe_warn_repeated_keys(k, l, used1, st3)
    pair = :cerl.ann_c_map_pair(as, op, k, v)
    {[pair | pairs], pre0 ++ pre1 ++ pre2, used2, st4}
  end

  defp map_build_pairs_1([], used, st) do
    {[], [], used, st}
  end

  defp maybe_warn_repeated_keys(ck, line, used, st) do
    case :cerl.is_literal(ck) do
      false ->
        {used, st}

      true ->
        k = :cerl.concrete(ck)

        case :cerl_sets.is_element(k, used) do
          true ->
            {used, add_warning(line, {:map_key_repeated, k}, st)}

          false ->
            {:cerl_sets.add_element(k, used), st}
        end
    end
  end

  defp map_op(:map_field_assoc) do
    r_c_literal(val: :assoc)
  end

  defp map_op(:map_field_exact) do
    r_c_literal(val: :exact)
  end

  defp try_exception(ecs0, st0) do
    {evs, st1} = new_vars(3, st0)
    {ecs1, st2} = clauses(ecs0, st1)
    ecs2 = try_build_stacktrace(ecs1, hd(evs))
    [_, value, info] = evs

    lA =
      case ecs2 do
        [] ->
          []

        [c | _] ->
          get_lineno_anno(c)
      end

    ec =
      r_iclause(
        anno: r_a(anno: [:compiler_generated | lA]),
        pats: [c_tuple(evs)],
        guard: [r_c_literal(val: true)],
        body: [r_iprimop(anno: r_a(), name: r_c_literal(val: :raise), args: [info, value])]
      )

    hs = [r_icase(anno: r_a(anno: lA), args: [c_tuple(evs)], clauses: ecs2, fc: ec)]
    {evs, hs, st2}
  end

  defp try_after(line, es0, as0, st0) do
    as1 = ta_sanitize_as(as0, line)
    {es, st1} = exprs(es0, st0)
    {as, st2} = exprs(as1, st1)
    {v, st3} = new_var(st2)
    lineAnno = lineno_anno(line, st3)

    case is_iexprs_small(as, 20) do
      true ->
        try_after_small(lineAnno, es, as, v, st3)

      false ->
        try_after_large(lineAnno, es, as, v, st3)
    end
  end

  defp ta_sanitize_as([expr], line) do
    [{:match, line, {:var, [], :_}, expr}]
  end

  defp ta_sanitize_as([expr | exprs], line) do
    [expr | ta_sanitize_as(exprs, line)]
  end

  defp try_after_large(lA, es, as, v, st0) do
    lanno = r_a(anno: lA)
    {name, st1} = new_fun_name('after', st0)
    fc = function_clause([], lA)

    fun =
      r_ifun(
        anno: lanno,
        id: [],
        vars: [],
        clauses: [r_iclause(anno: lanno, pats: [], guard: [r_c_literal(val: true)], body: as)],
        fc: fc
      )

    app =
      r_iapply(
        anno: r_a(anno: [:compiler_generated | lA]),
        op: r_c_var(anno: lA, name: {name, 0}),
        args: []
      )

    {evs, hs, st} = after_block([app], st1)
    try = r_itry(anno: lanno, args: es, vars: [v], body: [app, v], evars: evs, handler: hs)
    letrec = r_iletrec(anno: lanno, defs: [{{name, 0}, fun}], body: [try])
    {letrec, [], st}
  end

  defp try_after_small(lA, es, as, v, st0) do
    lanno = r_a(anno: lA)
    {evs, hs, st1} = after_block(as, st0)
    try = r_itry(anno: lanno, args: es, vars: [v], body: as ++ [v], evars: evs, handler: hs)
    {try, [], st1}
  end

  defp after_block(as, st0) do
    {evs, st1} = new_vars(3, st0)
    [_, value, info] = evs
    b = as ++ [r_iprimop(anno: r_a(), name: r_c_literal(val: :raise), args: [info, value])]

    ec =
      r_iclause(
        anno: r_a(anno: [:compiler_generated]),
        pats: [c_tuple(evs)],
        guard: [r_c_literal(val: true)],
        body: b
      )

    hs = [r_icase(anno: r_a(), args: [c_tuple(evs)], clauses: [], fc: ec)]
    {evs, hs, st1}
  end

  defp try_build_stacktrace([r_iclause(pats: ps0, body: b0) = c0 | cs], rawStk) do
    [r_c_tuple(es: [class, exc, stk]) = tup] = ps0

    case stk do
      r_c_var(name: :_) ->
        [c0 | try_build_stacktrace(cs, rawStk)]

      _ ->
        ps = [r_c_tuple(tup, es: [class, exc, rawStk])]
        call = r_iprimop(anno: r_a(), name: r_c_literal(val: :build_stacktrace), args: [rawStk])
        iset = r_iset(var: stk, arg: call)
        b = [iset | b0]
        c = r_iclause(c0, pats: ps, body: b)
        [c | try_build_stacktrace(cs, rawStk)]
    end
  end

  defp try_build_stacktrace([], _) do
    []
  end

  defp is_iexprs_small(exprs, threshold) do
    0 < is_iexprs_small_1(exprs, threshold)
  end

  defp is_iexprs_small_1(_, 0) do
    0
  end

  defp is_iexprs_small_1([], threshold) do
    threshold
  end

  defp is_iexprs_small_1([expr | exprs], threshold0) do
    threshold = is_iexprs_small_2(expr, threshold0 - 1)
    is_iexprs_small_1(exprs, threshold)
  end

  defp is_iexprs_small_2(r_iclause(guard: guards, body: body), threshold0) do
    threshold = is_iexprs_small_1(guards, threshold0)
    is_iexprs_small_1(body, threshold)
  end

  defp is_iexprs_small_2(r_itry(body: body, handler: handler), threshold0) do
    threshold = is_iexprs_small_1(body, threshold0)
    is_iexprs_small_1(handler, threshold)
  end

  defp is_iexprs_small_2(r_imatch(guard: guards), threshold) do
    is_iexprs_small_1(guards, threshold)
  end

  defp is_iexprs_small_2(r_icase(clauses: clauses), threshold) do
    is_iexprs_small_1(clauses, threshold)
  end

  defp is_iexprs_small_2(r_ifun(clauses: clauses), threshold) do
    is_iexprs_small_1(clauses, threshold)
  end

  defp is_iexprs_small_2(r_ireceive1(clauses: clauses), threshold) do
    is_iexprs_small_1(clauses, threshold)
  end

  defp is_iexprs_small_2(r_ireceive2(clauses: clauses), threshold) do
    is_iexprs_small_1(clauses, threshold)
  end

  defp is_iexprs_small_2(r_icatch(body: body), threshold) do
    is_iexprs_small_1(body, threshold)
  end

  defp is_iexprs_small_2(r_iletrec(body: body), threshold) do
    is_iexprs_small_1(body, threshold)
  end

  defp is_iexprs_small_2(r_iprotect(body: body), threshold) do
    is_iexprs_small_1(body, threshold)
  end

  defp is_iexprs_small_2(r_iset(arg: arg), threshold) do
    is_iexprs_small_2(arg, threshold)
  end

  defp is_iexprs_small_2(_, threshold) do
    threshold
  end

  defp expr_bin(es0, anno, st0) do
    es1 =
      for e <- es0 do
        bin_element(e)
      end

    case constant_bin(es1) do
      :error ->
        case expr_bin_1(es1, st0) do
          {[], eps, st} ->
            emptyBin = <<>>
            {r_c_literal(anno: anno, val: emptyBin), eps, st}

          {es, eps, st} ->
            {r_ibinary(anno: r_a(anno: anno), segments: es), eps, st}
        end

      bin ->
        {r_c_literal(anno: anno, val: bin), [], st0}
    end
  end

  defp expr_bin_1(es, st0) do
    res =
      foldr(
        fn e, {ces, eps0, s0} ->
          try do
            bitstr(e, s0)
          catch
            {:bad_binary, eps, s1} ->
              {:bad_binary, eps ++ eps0, s1}
          else
            {ce, eps, s1} when is_list(ces) ->
              {ce ++ ces, eps ++ eps0, s1}

            {_Ce, eps, s1} ->
              {ces, eps ++ eps0, s1}
          end
        end,
        {[], [], st0},
        es
      )

    case res do
      {:bad_binary, eps, st} ->
        throw({:bad_binary, eps, st})

      {_, _, _} = ^res ->
        res
    end
  end

  defp bitstrs([e0 | es0], st0) do
    {e, eps0, st1} = bitstr(e0, st0)
    {es, eps1, st2} = bitstrs(es0, st1)
    {e ++ es, eps0 ++ eps1, st2}
  end

  defp bitstrs([], st) do
    {[], [], st}
  end

  defp bitstr(
         {:bin_element, line, {:string, _, s}, {:integer, _, 8}, _},
         st
       ) do
    bitstrs(bin_expand_string(s, line, 0, 0), st)
  end

  defp bitstr(
         {:bin_element, line, {:string, _, []}, sz0, ts},
         st0
       ) do
    {[r_c_bitstr(size: sz)], eps0, st1} =
      bitstr(
        {:bin_element, line, {:char, line, 0}, sz0, ts},
        st0
      )

    case sz do
      r_c_literal(val: :undefined) ->
        {[], [], st1}

      r_c_literal(val: int) when is_integer(int) and int >= 0 ->
        {[], [], st1}

      r_c_var() ->
        erlang = {:atom, line, :erlang}
        test0 = {:call, line, {:remote, line, erlang, {:atom, line, :is_integer}}, [sz0]}

        test1 =
          {:call, line, {:remote, line, erlang, {:atom, line, :>=}}, [sz0, {:integer, line, 0}]}

        test2 = {:op, line, :andalso, test0, test1}

        fail =
          {:call, line, {:remote, line, erlang, {:atom, line, :error}}, [{:atom, line, :badarg}]}

        test = {:op, line, :orelse, test2, fail}
        match = {:match, line, {:var, line, :_}, test}
        {_, eps1, st2} = expr(match, st1)
        eps = eps0 ++ eps1
        {[], eps, st2}
    end
  end

  defp bitstr(
         {:bin_element, line, {:string, _, s}, sz0, ts},
         st0
       ) do
    {[bitstr], eps, st1} =
      bitstr(
        {:bin_element, line, {:char, line, 0}, sz0, ts},
        st0
      )

    es =
      for c <- s do
        r_c_bitstr(bitstr, val: r_c_literal(anno: full_anno(line, st1), val: c))
      end

    {es, eps, st1}
  end

  defp bitstr(
         {:bin_element, _, e0, size0, [[type, {:unit, unit}] | flags]},
         st0
       ) do
    {e1, eps0, st1} = safe(e0, st0)
    {size1, eps1, st2} = safe(size0, st1)
    eps = eps0 ++ eps1

    case {type, e1} do
      {_, r_c_var()} ->
        :ok

      {:integer, r_c_literal(val: i)} when is_integer(i) ->
        :ok

      {:utf8, r_c_literal(val: i)} when is_integer(i) ->
        :ok

      {:utf16, r_c_literal(val: i)} when is_integer(i) ->
        :ok

      {:utf32, r_c_literal(val: i)} when is_integer(i) ->
        :ok

      {:float, r_c_literal(val: v)} when is_number(v) ->
        :ok

      {:binary, r_c_literal(val: v)} when is_bitstring(v) ->
        :ok

      {_, _} ->
        throw({:bad_binary, eps, st2})
    end

    case size1 do
      r_c_var() ->
        :ok

      r_c_literal(val: sz) when is_integer(sz) and sz >= 0 ->
        :ok

      r_c_literal(val: :undefined) ->
        :ok

      r_c_literal(val: :all) ->
        :ok

      _ ->
        throw({:bad_binary, eps, st2})
    end

    {[
       r_c_bitstr(
         val: e1,
         size: size1,
         unit: r_c_literal(val: unit),
         type: r_c_literal(val: type),
         flags: r_c_literal(val: flags)
       )
     ], eps, st2}
  end

  defp bin_element({:bin_element, line, expr, size0, type0}) do
    {size, type} = make_bit_type(line, size0, type0)
    {:bin_element, line, expr, size, type}
  end

  defp make_bit_type(line, :default, type0) do
    case :erl_bits.set_bit_type(:default, type0) do
      {:ok, :all, bt} ->
        {make_all_size(line), :erl_bits.as_list(bt)}

      {:ok, :undefined, bt} ->
        {{:atom, line, :undefined}, :erl_bits.as_list(bt)}

      {:ok, size, bt} ->
        {{:integer, line, size}, :erl_bits.as_list(bt)}
    end
  end

  defp make_bit_type(_Line, {:atom, anno, :all} = size, type0) do
    case :erl_anno.generated(anno) do
      true ->
        {:ok, ^size, bt} = :erl_bits.set_bit_type(size, type0)
        {size, :erl_bits.as_list(bt)}

      false ->
        throw(:nomatch)
    end
  end

  defp make_bit_type(_Line, size0, type0) do
    {:ok, size1, bt} = :erl_bits.set_bit_type(size0, type0)

    size =
      case size1 do
        {:char, anno, charVal} ->
          {:integer, anno, charVal}

        _ ->
          size1
      end

    {size, :erl_bits.as_list(bt)}
  end

  defp make_all_size(line) do
    anno = :erl_anno.set_generated(true, line)
    {:atom, anno, :all}
  end

  defp constant_bin(es) do
    try do
      constant_bin_1(es)
    catch
      :error ->
        :error
    end
  end

  defp constant_bin_1(es) do
    verify_suitable_fields(es)
    emptyBindings = :erl_eval.new_bindings()

    evalFun = fn
      {:string, _, s}, b ->
        {:value, s, b}

      {:integer, _, i}, b ->
        {:value, i, b}

      {:char, _, c}, b ->
        {:value, c, b}

      {:float, _, f}, b ->
        {:value, f, b}

      {:atom, _, :undefined}, b ->
        {:value, :undefined, b}
    end

    try do
      :eval_bits.expr_grp(es, emptyBindings, evalFun)
    catch
      :error, _ ->
        :error
    else
      {:value, bin, ^emptyBindings} ->
        bin
    end
  end

  defp verify_suitable_fields([{:bin_element, _, val, szTerm, opts} | es]) do
    case member(:big, opts) or member(:little, opts) do
      true ->
        :ok

      false ->
        throw(:error)
    end

    {:unit, unit} = keyfind(:unit, 1, opts)

    case {szTerm, val} do
      {{:atom, _, :undefined}, {:string, _, _}} ->
        :ok

      {{:atom, _, :undefined}, {:char, _, _}} ->
        :ok

      {{:atom, _, :undefined}, {:integer, _, _}} ->
        :ok

      {{:integer, _, sz}, _} when sz * unit <= 256 ->
        :ok

      {{:integer, _, sz0}, {:integer, _, int}} ->
        sz = sz0 * unit

        case count_bits(int) do
          bitsNeeded when 2 * bitsNeeded >= sz ->
            :ok

          _ ->
            throw(:error)
        end

      {_, _} ->
        throw(:error)
    end

    verify_suitable_fields(es)
  end

  defp verify_suitable_fields([]) do
    :ok
  end

  defp count_bits(int) do
    count_bits_1(abs(int), 64)
  end

  defp count_bits_1(0, bits) do
    bits
  end

  defp count_bits_1(int, bits) do
    count_bits_1(int >>> 64, bits + 64)
  end

  defp bin_expand_string(s, line, val, size) when size >= 2048 do
    combined = make_combined(line, val, size)
    [combined | bin_expand_string(s, line, 0, 0)]
  end

  defp bin_expand_string([h | t], line, val, size) do
    bin_expand_string(t, line, val <<< 8 ||| h, size + 8)
  end

  defp bin_expand_string([], line, val, size) do
    [make_combined(line, val, size)]
  end

  defp make_combined(line, val, size) do
    {:bin_element, line, {:integer, line, val}, {:integer, line, size},
     [:integer, {:unit, 1}, :unsigned, :big]}
  end

  defp fun_tq(cs0, l, st0, nameInfo) do
    arity = clause_arity(hd(cs0))
    {cs1, st1} = clauses(cs0, st0)
    {args, st2} = new_vars(arity, st1)
    {ps, st3} = new_vars(arity, st2)
    anno = full_anno(l, st3)
    {name, st4} = new_fun_name(st3)
    fc = function_clause(ps, anno)
    id = {0, 0, name}

    fun =
      r_ifun(
        anno: r_a(anno: anno),
        id: [{:id, id}],
        vars: args,
        clauses: cs1,
        fc: fc,
        name: nameInfo
      )

    {fun, [], st4}
  end

  defp lc_tq(
         line,
         e,
         [
           r_igen(
             anno: r_a(anno: gA) = gAnno,
             acc_pat: accPat,
             acc_guard: accGuard,
             skip_pat: skipPat,
             tail: tail,
             tail_pat: tailPat,
             arg: {pre, arg}
           )
           | qs
         ],
         mc,
         st0
       ) do
    {name, st1} = new_fun_name('lc', st0)
    lA = lineno_anno(line, st1)
    lAnno = r_a(anno: lA)
    f = r_c_var(anno: lA, name: {name, 1})
    nc = r_iapply(anno: gAnno, op: f, args: [tail])
    {[fcVar, var], st2} = new_vars(2, st1)
    fc = function_clause([fcVar], gA)
    tailClause = r_iclause(anno: lAnno, pats: [tailPat], guard: [], body: [mc])

    cs0 =
      case {accPat, accGuard} do
        {^skipPat, []} ->
          [tailClause]

        _ ->
          [
            r_iclause(
              anno: r_a(anno: [:compiler_generated | lA]),
              pats: [skipPat],
              guard: [],
              body: [nc]
            ),
            tailClause
          ]
      end

    {cs, st4} =
      case accPat do
        :nomatch ->
          {cs0, st2}

        _ ->
          {lc, lps, st3} = lc_tq(line, e, qs, nc, st2)

          {[
             r_iclause(anno: lAnno, pats: [accPat], guard: accGuard, body: lps ++ [lc])
             | cs0
           ], st3}
      end

    fun = r_ifun(anno: gAnno, id: [], vars: [var], clauses: cs, fc: fc)

    {r_iletrec(
       anno: r_a(gAnno, anno: [:list_comprehension | gA]),
       defs: [{{name, 1}, fun}],
       body: pre ++ [r_iapply(anno: gAnno, op: f, args: [arg])]
     ), [], st4}
  end

  defp lc_tq(line, e, [r_ifilter() = filter | qs], mc, st) do
    filter_tq(line, e, filter, mc, st, qs, &lc_tq/5)
  end

  defp lc_tq(line, e0, [], mc0, st0) do
    {h1, hps, st1} = safe(e0, st0)
    {t1, tps, st} = force_safe(mc0, st1)
    anno = lineno_anno(line, st)
    e = ann_c_cons(anno, h1, t1)
    {set_anno(e, [:compiler_generated | anno]), hps ++ tps, st}
  end

  defp bc_tq(line, exp, qs0, st0) do
    {binVar, st1} = new_var(st0)
    {sz, szPre, st2} = bc_initial_size(exp, qs0, st1)
    {qs, st3} = preprocess_quals(line, qs0, st2)
    {e, bcPre, st} = bc_tq1(line, exp, qs, binVar, st3)

    pre =
      szPre ++
        [
          r_iset(
            var: binVar,
            arg:
              r_iprimop(
                name: r_c_literal(val: :bs_init_writable),
                args: [sz]
              )
          )
        ] ++ bcPre

    {e, pre, st}
  end

  defp bc_tq1(
         line,
         e,
         [
           r_igen(
             anno: gAnno,
             acc_pat: accPat,
             acc_guard: accGuard,
             skip_pat: skipPat,
             tail: tail,
             tail_pat: tailPat,
             arg: {pre, arg}
           )
           | qs
         ],
         mc,
         st0
       ) do
    {name, st1} = new_fun_name('lbc', st0)
    lA = lineno_anno(line, st1)
    lAnno = r_a(anno: lA)
    {[_, accVar] = vars, st2} = new_vars(lA, 2, st1)
    {[_, _] = fcVars, st3} = new_vars(lA, 2, st2)
    f = r_c_var(anno: lA, name: {name, 2})
    nc = r_iapply(anno: gAnno, op: f, args: [tail, accVar])
    fc = function_clause(fcVars, lA)
    tailClause = r_iclause(anno: lAnno, pats: [tailPat, accVar], guard: [], body: [accVar])

    cs0 =
      case {accPat, accGuard} do
        {^skipPat, []} ->
          [tailClause]

        _ ->
          [
            r_iclause(
              anno: r_a(anno: [:compiler_generated | lA]),
              pats: [skipPat, accVar],
              guard: [],
              body: [nc]
            ),
            tailClause
          ]
      end

    {cs, st} =
      case accPat do
        :nomatch ->
          {cs0, st3}

        _ ->
          {bc, bps, st4} = bc_tq1(line, e, qs, accVar, st3)
          body = bps ++ [r_iset(var: accVar, arg: bc), nc]

          {[
             r_iclause(anno: lAnno, pats: [accPat, accVar], guard: accGuard, body: body)
             | cs0
           ], st4}
      end

    fun = r_ifun(anno: lAnno, id: [], vars: vars, clauses: cs, fc: fc)

    {r_iletrec(
       anno: r_a(lAnno, anno: [:list_comprehension | lA]),
       defs: [{{name, 2}, fun}],
       body: pre ++ [r_iapply(anno: lAnno, op: f, args: [arg, mc])]
     ), [], st}
  end

  defp bc_tq1(line, e, [r_ifilter() = filter | qs], mc, st) do
    filter_tq(line, e, filter, mc, st, qs, &bc_tq1/5)
  end

  defp bc_tq1(_, {:bin, bl, elements}, [], accVar, st0) do
    bc_tq_build(bl, [], accVar, elements, st0)
  end

  defp bc_tq1(line, e0, [], accVar, st0) do
    bsFlags = [:binary, {:unit, 1}]
    bsSize = make_all_size(line)
    {e1, pre0, st1} = safe(e0, st0)

    case e1 do
      r_c_var(name: varName) ->
        var = {:var, line, varName}
        els = [{:bin_element, line, var, bsSize, bsFlags}]
        bc_tq_build(line, pre0, accVar, els, st1)

      r_c_literal(val: val) when is_bitstring(val) ->
        bits = bit_size(val)
        <<int0::size(bits)>> = val
        int = {:integer, line, int0}
        sz = {:integer, line, bits}
        els = [{:bin_element, line, int, sz, [:integer, {:unit, 1}, :big]}]
        bc_tq_build(line, pre0, accVar, els, st1)

      _ ->
        els = [{:bin_element, line, {:atom, line, :bad_value}, bsSize, bsFlags}]
        bc_tq_build(line, pre0, accVar, els, st1)
    end
  end

  defp bc_tq_build(line, pre0, r_c_var(name: accVar), elements0, st0) do
    elements = [
      {:bin_element, line, {:var, line, accVar}, make_all_size(line), [:binary, {:unit, 1}]}
      | elements0
    ]

    {e, pre, st} = expr({:bin, line, elements}, st0)
    r_a(anno: a) = anno0 = get_anno(e)

    anno =
      r_a(anno0,
        anno: [
          [:compiler_generated, :single_use]
          | a
        ]
      )

    {set_anno(e, anno), pre0 ++ pre, st}
  end

  defp filter_tq(
         line,
         e,
         r_ifilter(anno: r_a(anno: lA) = lAnno, arg: {pre, arg}),
         mc,
         st0,
         qs,
         tqFun
       ) do
    {lc, lps, st1} = tqFun.(line, e, qs, mc, st0)
    {failPat, st2} = new_var(st1)
    fc = fail_clause([failPat], lA, c_tuple([r_c_literal(val: :case_clause), failPat]))

    {r_icase(
       anno: r_a(lAnno, anno: [:list_comprehension | lA]),
       args: [arg],
       clauses: [
         r_iclause(anno: lAnno, pats: [r_c_literal(val: true)], guard: [], body: lps ++ [lc]),
         r_iclause(
           anno: r_a(lAnno, anno: [:compiler_generated | lA]),
           pats: [r_c_literal(val: false)],
           guard: [],
           body: [mc]
         )
       ],
       fc: fc
     ), pre, st2}
  end

  defp filter_tq(line, e, r_ifilter(anno: r_a(anno: lA) = lAnno, arg: guard), mc, st0, qs, tqFun)
       when is_list(guard) do
    {lc, lps, st1} = tqFun.(line, e, qs, mc, st0)

    {r_icase(
       anno: r_a(lAnno, anno: [:list_comprehension | lA]),
       args: [],
       clauses: [r_iclause(anno: lAnno, pats: [], guard: guard, body: lps ++ [lc])],
       fc:
         r_iclause(
           anno: r_a(lAnno, anno: [:compiler_generated | lA]),
           pats: [],
           guard: [],
           body: [mc]
         )
     ), [], st1}
  end

  defp preprocess_quals(line, qs, st) do
    preprocess_quals(line, qs, st, [])
  end

  defp preprocess_quals(line, [q | qs0], st0, acc) do
    case is_generator(q) do
      true ->
        {gs, qs} = splitwith(&is_guard_test/1, qs0)
        {gen, st} = generator(line, q, gs, st0)
        preprocess_quals(line, qs, st, [gen | acc])

      false ->
        lAnno = r_a(anno: lineno_anno(get_qual_anno(q), st0))

        case is_guard_test(q) do
          true ->
            {gs, qs} = splitwith(&is_guard_test/1, qs0)
            {cg, st} = lc_guard_tests([q | gs], st0)
            filter = r_ifilter(anno: lAnno, arg: cg)
            preprocess_quals(line, qs, st, [filter | acc])

          false ->
            {ce, pre, st} = novars(q, st0)
            filter = r_ifilter(anno: lAnno, arg: {pre, ce})
            preprocess_quals(line, qs0, st, [filter | acc])
        end
    end
  end

  defp preprocess_quals(_, [], st, acc) do
    {reverse(acc), st}
  end

  defp is_generator({:generate, _, _, _}) do
    true
  end

  defp is_generator({:b_generate, _, _, _}) do
    true
  end

  defp is_generator(_) do
    false
  end

  defp get_qual_anno(abstract) do
    :erlang.element(2, abstract)
  end

  defp generator(line, {:generate, lg, p0, e}, gs, st0) do
    lA = lineno_anno(line, st0)
    gA = lineno_anno(lg, st0)
    {head, st1} = list_gen_pattern(p0, line, st0)
    {[tail, skip], st2} = new_vars(2, st1)
    {cg, st3} = lc_guard_tests(gs, st2)

    {accPat, skipPat} =
      case head do
        r_c_var() ->
          cons = ann_c_cons(lA, head, tail)
          {cons, cons}

        :nomatch ->
          {:nomatch, ann_c_cons(lA, skip, tail)}

        _ ->
          {ann_c_cons(lA, head, tail), ann_c_cons(lA, skip, tail)}
      end

    {ce, pre, st4} = safe(e, st3)

    gen =
      r_igen(
        anno: r_a(anno: gA),
        acc_pat: accPat,
        acc_guard: cg,
        skip_pat: skipPat,
        tail: tail,
        tail_pat: r_c_literal(anno: lA, val: []),
        arg: {pre, ce}
      )

    {gen, st4}
  end

  defp generator(line, {:b_generate, lg, p, e}, gs, st0) do
    lA = lineno_anno(line, st0)
    gA = lineno_anno(lg, st0)

    try do
      pattern(p, st0)
    catch
      :nomatch ->
        {ce, pre, st1} = safe(e, st0)

        gen =
          r_igen(
            anno: r_a(anno: gA),
            acc_pat: :nomatch,
            acc_guard: [],
            skip_pat: :nomatch,
            tail_pat: r_c_var(name: :_),
            arg: {pre, ce}
          )

        {gen, st1}
    else
      {r_ibinary(segments: segs) = cp, st1} ->
        {accSegs, tail, tailSeg, st2} = append_tail_segment(segs, st1)
        accPat = r_ibinary(cp, segments: accSegs)
        {cg, st3} = lc_guard_tests(gs, st2)
        {skipSegs, st4} = emasculate_segments(accSegs, st3)
        skipPat = r_ibinary(cp, segments: skipSegs)
        {ce, pre, st5} = safe(e, st4)

        gen =
          r_igen(
            anno: r_a(anno: gA),
            acc_pat: accPat,
            acc_guard: cg,
            skip_pat: skipPat,
            tail: tail,
            tail_pat: r_ibinary(anno: r_a(anno: lA), segments: [tailSeg]),
            arg: {pre, ce}
          )

        {gen, st5}
    end
  end

  defp append_tail_segment(segs, st0) do
    {var, st} = new_var(st0)

    tail =
      r_ibitstr(
        val: var,
        size: [r_c_literal(val: :all)],
        unit: r_c_literal(val: 1),
        type: r_c_literal(val: :binary),
        flags: r_c_literal(val: [:unsigned, :big])
      )

    {segs ++ [tail], var, tail, st}
  end

  defp emasculate_segments(segs, st) do
    emasculate_segments(segs, st, [])
  end

  defp emasculate_segments([r_ibitstr(val: r_c_var()) = b | rest], st, acc) do
    emasculate_segments(rest, st, [b | acc])
  end

  defp emasculate_segments([b | rest], st0, acc) do
    {var, st1} = new_var(st0)
    emasculate_segments(rest, st1, [r_ibitstr(b, val: var) | acc])
  end

  defp emasculate_segments([], st, acc) do
    {reverse(acc), st}
  end

  defp lc_guard_tests([], st) do
    {[], st}
  end

  defp lc_guard_tests(gs0, st0) do
    gs1 = guard_tests(gs0)
    {gs, st} = gexpr_top(gs1, r_core(st0, in_guard: true))
    {gs, r_core(st, in_guard: false)}
  end

  defp list_gen_pattern(p0, line, st) do
    try do
      pattern(p0, st)
    catch
      :nomatch ->
        {:nomatch, add_warning(line, :nomatch, st)}
    end
  end

  defp bc_initial_size(e0, q, st0) do
    try do
      e = bin_bin_element(e0)
      {elemSzExpr, elemSzPre, eVs, st1} = bc_elem_size(e, st0)
      {v, st2} = new_var(st1)
      {genSzExpr, genSzPre, st3} = bc_gen_size(q, eVs, st2)

      case elemSzExpr do
        r_c_literal(val: elemSz) when rem(elemSz, 8) === 0 ->
          numBytesExpr = r_c_literal(val: div(elemSz, 8))

          bytesExpr = [
            r_iset(
              var: v,
              arg: bc_mul(genSzExpr, numBytesExpr)
            )
          ]

          {v, elemSzPre ++ genSzPre ++ bytesExpr, st3}

        _ ->
          {[bitsV, plusSevenV], st} = new_vars(2, st3)

          bitsExpr =
            r_iset(
              var: bitsV,
              arg: bc_mul(genSzExpr, elemSzExpr)
            )

          plusSevenExpr =
            r_iset(
              var: plusSevenV,
              arg: bc_add(bitsV, r_c_literal(val: 7))
            )

          expr = r_iset(var: v, arg: bc_bsr(plusSevenV, r_c_literal(val: 3)))
          {v, elemSzPre ++ genSzPre ++ [bitsExpr, plusSevenExpr, expr], st}
      end
    catch
      :impossible ->
        {r_c_literal(val: 256), [], st0}

      :nomatch ->
        {r_c_literal(val: 1), [], st0}
    end
  end

  defp bc_elem_size({:bin, _, el}, st0) do
    case bc_elem_size_1(el, :ordsets.new(), 0, []) do
      {bits, []} ->
        {r_c_literal(val: bits), [], [], st0}

      {bits, vars0} ->
        [{u, v0} | pairs] = sort(vars0)
        f = bc_elem_size_combine(pairs, u, [v0], [])

        vs =
          for {_, r_c_var(name: v)} <- vars0 do
            v
          end

        {e, pre, st} = bc_mul_pairs(f, r_c_literal(val: bits), [], st0)
        {e, pre, vs, st}
    end
  end

  defp bc_elem_size(_, _) do
    throw(:impossible)
  end

  defp bc_elem_size_1(
         [
           {:bin_element, _, {:string, _, string}, {:integer, _, n}, _} = el
           | es
         ],
         defVars,
         bits,
         sizeVars
       ) do
    u = get_unit(el)
    bc_elem_size_1(es, defVars, bits + u * n * length(string), sizeVars)
  end

  defp bc_elem_size_1(
         [
           {:bin_element, _, expr, {:integer, _, n}, _} = el
           | es
         ],
         defVars0,
         bits,
         sizeVars
       ) do
    u = get_unit(el)
    defVars = bc_elem_size_def_var(expr, defVars0)
    bc_elem_size_1(es, defVars, bits + u * n, sizeVars)
  end

  defp bc_elem_size_1(
         [
           {:bin_element, _, expr, {:var, _, src}, _} = el
           | es
         ],
         defVars0,
         bits,
         sizeVars
       ) do
    case :ordsets.is_element(src, defVars0) do
      false ->
        u = get_unit(el)
        defVars = bc_elem_size_def_var(expr, defVars0)
        bc_elem_size_1(es, defVars, bits, [{u, r_c_var(name: src)} | sizeVars])

      true ->
        throw(:impossible)
    end
  end

  defp bc_elem_size_1([_ | _], _, _, _) do
    throw(:impossible)
  end

  defp bc_elem_size_1([], _DefVars, bits, sizeVars) do
    {bits, sizeVars}
  end

  defp bc_elem_size_def_var({:var, _, var}, defVars) do
    :ordsets.add_element(var, defVars)
  end

  defp bc_elem_size_def_var(_Expr, defVars) do
    defVars
  end

  defp bc_elem_size_combine([{u, v} | t], u, uVars, acc) do
    bc_elem_size_combine(t, u, [v | uVars], acc)
  end

  defp bc_elem_size_combine([{u, v} | t], oldU, uVars, acc) do
    bc_elem_size_combine(t, u, [v], [{oldU, uVars} | acc])
  end

  defp bc_elem_size_combine([], u, uvars, acc) do
    [{u, uvars} | acc]
  end

  defp bc_mul_pairs([{u, l0} | t], e0, pre, st0) do
    {addExpr, addPre, st1} = bc_add_list(l0, st0)
    {[v1, v2], st} = new_vars(2, st1)
    set1 = r_iset(var: v1, arg: bc_mul(addExpr, r_c_literal(val: u)))
    set2 = r_iset(var: v2, arg: bc_add(v1, e0))
    bc_mul_pairs(t, v2, [[set2, set1] | reverse(addPre, pre)], st)
  end

  defp bc_mul_pairs([], e, pre, st) do
    {e, reverse(pre), st}
  end

  defp bc_add_list([v], st) do
    {v, [], st}
  end

  defp bc_add_list([h | t], st) do
    bc_add_list_1(t, [], h, st)
  end

  defp bc_add_list_1([h | t], pre, e, st0) do
    {var, st} = new_var(st0)
    set = r_iset(var: var, arg: bc_add(h, e))
    bc_add_list_1(t, [set | pre], var, st)
  end

  defp bc_add_list_1([], pre, e, st) do
    {e, reverse(pre), st}
  end

  defp bc_gen_size(q, eVs, st) do
    bc_gen_size_1(q, eVs, r_c_literal(val: 1), [], st)
  end

  defp bc_gen_size_1([{:generate, l, el, gen} | qs], eVs, e0, pre0, st0) do
    bc_verify_non_filtering(el, eVs)

    case gen do
      {:var, _, listVar} ->
        lanno = lineno_anno(l, st0)
        {lenVar, st1} = new_var(st0)

        set =
          r_iset(
            var: lenVar,
            arg:
              r_icall(
                anno: r_a(anno: lanno),
                module: r_c_literal(val: :erlang),
                name: r_c_literal(val: :length),
                args: [r_c_var(name: listVar)]
              )
          )

        {e, pre, st} = bc_gen_size_mul(e0, lenVar, [set | pre0], st1)
        bc_gen_size_1(qs, eVs, e, pre, st)

      _ ->
        len = bc_list_length(gen, 0)
        {e, pre, st} = bc_gen_size_mul(e0, r_c_literal(val: len), pre0, st0)
        bc_gen_size_1(qs, eVs, e, pre, st)
    end
  end

  defp bc_gen_size_1([{:b_generate, _, el0, gen0} | qs], eVs, e0, pre0, st0) do
    el = bin_bin_element(el0)
    gen = bin_bin_element(gen0)
    bc_verify_non_filtering(el, eVs)
    {matchSzExpr, pre1, _, st1} = bc_elem_size(el, st0)
    pre2 = reverse(pre1, pre0)
    {resVar, st2} = new_var(st1)
    {bitSizeExpr, pre3, st3} = bc_gen_bit_size(gen, pre2, st2)

    div =
      r_iset(
        var: resVar,
        arg: bc_div(bitSizeExpr, matchSzExpr)
      )

    pre4 = [div | pre3]
    {e, pre, st} = bc_gen_size_mul(e0, resVar, pre4, st3)
    bc_gen_size_1(qs, eVs, e, pre, st)
  end

  defp bc_gen_size_1([], _, e, pre, st) do
    {e, reverse(pre), st}
  end

  defp bc_gen_size_1(_, _, _, _, _) do
    throw(:impossible)
  end

  defp bin_bin_element({:bin, l, el}) do
    {:bin, l,
     for e <- el do
       bin_element(e)
     end}
  end

  defp bin_bin_element(other) do
    other
  end

  defp bc_gen_bit_size({:var, l, v}, pre0, st0) do
    lanno = lineno_anno(l, st0)
    {szVar, st} = new_var(st0)

    pre = [
      r_iset(
        var: szVar,
        arg:
          r_icall(
            anno: r_a(anno: lanno),
            module: r_c_literal(val: :erlang),
            name: r_c_literal(val: :bit_size),
            args: [r_c_var(name: v)]
          )
      )
      | pre0
    ]

    {szVar, pre, st}
  end

  defp bc_gen_bit_size({:bin, _, _} = bin, pre, st) do
    {r_c_literal(val: bc_bin_size(bin)), pre, st}
  end

  defp bc_gen_bit_size(_, _, _) do
    throw(:impossible)
  end

  defp bc_verify_non_filtering({:bin, _, els}, eVs) do
    foreach(
      fn
        {:bin_element, _, {:var, _, v}, _, _} ->
          case member(v, eVs) do
            true ->
              throw(:impossible)

            false ->
              :ok
          end

        _ ->
          throw(:impossible)
      end,
      els
    )
  end

  defp bc_verify_non_filtering({:var, _, v}, eVs) do
    case member(v, eVs) do
      true ->
        throw(:impossible)

      false ->
        :ok
    end
  end

  defp bc_verify_non_filtering(_, _) do
    throw(:impossible)
  end

  defp bc_list_length({:string, _, str}, len) do
    len + length(str)
  end

  defp bc_list_length({:cons, _, _, t}, len) do
    bc_list_length(t, len + 1)
  end

  defp bc_list_length({nil, _}, len) do
    len
  end

  defp bc_list_length(_, _) do
    throw(:impossible)
  end

  defp bc_bin_size({:bin, _, els}) do
    bc_bin_size_1(els, 0)
  end

  defp bc_bin_size_1(
         [
           {:bin_element, _, {:string, _, string}, {:integer, _, sz}, _} = el
           | els
         ],
         n
       ) do
    u = get_unit(el)
    bc_bin_size_1(els, n + u * sz * length(string))
  end

  defp bc_bin_size_1(
         [
           {:bin_element, _, _, {:integer, _, sz}, _} = el
           | els
         ],
         n
       ) do
    u = get_unit(el)
    bc_bin_size_1(els, n + u * sz)
  end

  defp bc_bin_size_1([], n) do
    n
  end

  defp bc_bin_size_1(_, _) do
    throw(:impossible)
  end

  defp bc_gen_size_mul(r_c_literal(val: 1), e, pre, st) do
    {e, pre, st}
  end

  defp bc_gen_size_mul(e1, e2, pre, st0) do
    {v, st} = new_var(st0)
    {v, [r_iset(var: v, arg: bc_mul(e1, e2)) | pre], st}
  end

  defp bc_mul(e1, r_c_literal(val: 1)) do
    e1
  end

  defp bc_mul(e1, e2) do
    r_icall(module: r_c_literal(val: :erlang), name: r_c_literal(val: :*), args: [e1, e2])
  end

  defp bc_div(e1, e2) do
    r_icall(module: r_c_literal(val: :erlang), name: r_c_literal(val: :div), args: [e1, e2])
  end

  defp bc_add(e1, r_c_literal(val: 0)) do
    e1
  end

  defp bc_add(e1, e2) do
    r_icall(module: r_c_literal(val: :erlang), name: r_c_literal(val: :+), args: [e1, e2])
  end

  defp bc_bsr(e1, e2) do
    r_icall(module: r_c_literal(val: :erlang), name: r_c_literal(val: :bsr), args: [e1, e2])
  end

  defp get_unit({:bin_element, _, _, _, flags}) do
    {:unit, u} = keyfind(:unit, 1, flags)
    u
  end

  defp is_guard_test(e) do
    isOverridden = fn {_, _} ->
      true
    end

    :erl_lint.is_guard_test(e, [], isOverridden)
  end

  defp novars(e0, st0) do
    {e1, eps, st1} = expr(e0, st0)
    {se, sps, st2} = force_novars(e1, st1)
    {se, eps ++ sps, st2}
  end

  defp force_novars(r_iapply() = app, st) do
    {app, [], st}
  end

  defp force_novars(r_icall() = call, st) do
    {call, [], st}
  end

  defp force_novars(r_ifun() = fun, st) do
    {fun, [], st}
  end

  defp force_novars(r_ibinary() = bin, st) do
    {bin, [], st}
  end

  defp force_novars(r_c_map() = bin, st) do
    {bin, [], st}
  end

  defp force_novars(ce, st) do
    force_safe(ce, st)
  end

  defp safe(e0, st0) do
    {e1, eps, st1} = expr(e0, st0)
    {se, sps, st2} = force_safe(e1, st1)
    {se, eps ++ sps, st2}
  end

  defp safe_list(es, st) do
    foldr(
      fn e, {ces, esp, st0} ->
        {ce, ep, st1} = safe(e, st0)
        {[ce | ces], ep ++ esp, st1}
      end,
      {[], [], st},
      es
    )
  end

  defp force_safe(r_imatch(pat: p, arg: e) = imatch, st0) do
    {le, lps0, st1} = force_safe(e, st0)
    lps = lps0 ++ [r_imatch(imatch, arg: le)]

    case le do
      r_c_var() ->
        {le, lps, st1}

      _ ->
        {v, st2} = new_var(st1)
        {v, lps0 ++ [r_imatch(imatch, pat: r_c_alias(var: v, pat: p), arg: le)], st2}
    end
  end

  defp force_safe(ce, st0) do
    case is_safe(ce) do
      true ->
        {ce, [], st0}

      false ->
        {v, st1} = new_var(st0)
        {v, [r_iset(var: v, arg: ce)], st1}
    end
  end

  defp is_safe(r_c_cons()) do
    true
  end

  defp is_safe(r_c_tuple()) do
    true
  end

  defp is_safe(r_c_var(name: {_, _})) do
    false
  end

  defp is_safe(r_c_var(name: _)) do
    true
  end

  defp is_safe(r_c_literal()) do
    true
  end

  defp is_safe(_) do
    false
  end

  defp fold_match({:match, l, p0, e0}, p) do
    {p1, e1} = fold_match(e0, p)
    {{:match, l, p0, p1}, e1}
  end

  defp fold_match(e, p) do
    {p, e}
  end

  defp pattern({:var, l, v}, st) do
    {r_c_var(anno: lineno_anno(l, st), name: v), st}
  end

  defp pattern({:char, l, c}, st) do
    {r_c_literal(anno: lineno_anno(l, st), val: c), st}
  end

  defp pattern({:integer, l, i}, st) do
    {r_c_literal(anno: lineno_anno(l, st), val: i), st}
  end

  defp pattern({:float, l, f}, st) do
    {r_c_literal(anno: lineno_anno(l, st), val: f), st}
  end

  defp pattern({:atom, l, a}, st) do
    {r_c_literal(anno: lineno_anno(l, st), val: a), st}
  end

  defp pattern({:string, l, s}, st) do
    {r_c_literal(anno: lineno_anno(l, st), val: s), st}
  end

  defp pattern({nil, l}, st) do
    {r_c_literal(anno: lineno_anno(l, st), val: []), st}
  end

  defp pattern({:cons, l, h, t}, st) do
    {ph, st1} = pattern(h, st)
    {pt, st2} = pattern(t, st1)
    {annotate_cons(lineno_anno(l, st), ph, pt, st2), st2}
  end

  defp pattern({:tuple, l, ps}, st) do
    {ps1, st1} = pattern_list(ps, st)
    {annotate_tuple(record_anno(l, st), ps1, st), st1}
  end

  defp pattern({:map, l, pairs}, st0) do
    {ps, st1} = pattern_map_pairs(pairs, st0)
    {r_imap(anno: r_a(anno: lineno_anno(l, st1)), es: ps), st1}
  end

  defp pattern({:bin, l, ps}, st0) do
    {segments, st} = pat_bin(ps, st0)

    {r_ibinary(
       anno: r_a(anno: lineno_anno(l, st)),
       segments: segments
     ), st}
  end

  defp pattern({:match, _, p1, p2}, st) do
    {cp1, st1} = pattern(p1, st)
    {cp2, st2} = pattern(p2, st1)
    {pat_alias(cp1, cp2), st2}
  end

  defp pattern({:op, _, :++, {nil, _}, r}, st) do
    pattern(r, st)
  end

  defp pattern({:op, _, :++, {:cons, li, h, t}, r}, st) do
    pattern({:cons, li, h, {:op, li, :++, t, r}}, st)
  end

  defp pattern({:op, _, :++, {:string, li, l}, r}, st) do
    pattern(string_to_conses(li, l, r), st)
  end

  defp pattern({:op, _Line, _Op, _A} = op, st) do
    pattern(:erl_eval.partial_eval(op), st)
  end

  defp pattern({:op, _Line, _Op, _L, _R} = op, st) do
    pattern(:erl_eval.partial_eval(op), st)
  end

  defp pattern_map_pairs(ps, st0) do
    {cMapPairs, st1} = mapfoldl(&pattern_map_pair/2, st0, ps)
    {pat_alias_map_pairs(cMapPairs), st1}
  end

  defp pattern_map_pair({:map_field_exact, l, k, v}, st0) do
    ck0 = :erl_eval.partial_eval(k)
    {ck, st1} = exprs([ck0], st0)
    {cv, st2} = pattern(v, st1)

    {r_imappair(
       anno: r_a(anno: lineno_anno(l, st2)),
       op: r_c_literal(val: :exact),
       key: ck,
       val: cv
     ), st2}
  end

  defp pat_alias_map_pairs(ps) do
    d0 =
      foldl(
        fn r_imappair(key: k0) = pair, a ->
          k = map_sort_key(k0, a)

          case a do
            %{^k => aliases} ->
              %{a | k => [pair | aliases]}

            %{} ->
              %{a | k => [pair]}
          end
        end,
        %{},
        ps
      )

    d = sort(:maps.to_list(d0))
    pat_alias_map_pairs_1(d)
  end

  defp pat_alias_map_pairs_1([{_, [r_imappair(val: v0) = pair | vs]} | t]) do
    v =
      foldl(
        fn r_imappair(val: v), pat ->
          pat_alias(v, pat)
        end,
        v0,
        vs
      )

    [r_imappair(pair, val: v) | pat_alias_map_pairs_1(t)]
  end

  defp pat_alias_map_pairs_1([]) do
    []
  end

  defp map_sort_key(key, keyMap) do
    case key do
      [r_c_literal() = lit] ->
        {:atomic, :cerl.set_ann(lit, [])}

      [r_c_var() = var] ->
        {:atomic, :cerl.set_ann(var, [])}

      _ ->
        {:expr, map_size(keyMap)}
    end
  end

  defp pat_bin(ps0, st) do
    ps = pat_bin_expand_strings(ps0)
    pat_segments(ps, st)
  end

  defp pat_bin_expand_strings(es0) do
    foldr(
      fn
        {:bin_element, line, {:string, _, s}, sz, ts}, es1 ->
          foldr(
            fn c, es ->
              [{:bin_element, line, {:char, line, c}, sz, ts} | es]
            end,
            es1,
            s
          )

        e, es ->
          [e | es]
      end,
      [],
      es0
    )
  end

  defp pat_segments([p0 | ps0], st0) do
    {p, st1} = pat_segment(p0, st0)
    {ps, st2} = pat_segments(ps0, st1)
    {[p | ps], st2}
  end

  defp pat_segments([], st) do
    {[], st}
  end

  defp pat_segment({:bin_element, l, val, size0, type0}, st) do
    {size1, type1} = make_bit_type(l, size0, type0)
    [[type, {:unit, unit}] | flags] = type1
    anno = lineno_anno(l, st)
    {pval0, st1} = pattern(val, st)
    pval = coerce_to_float(pval0, type0)
    size = :erl_eval.partial_eval(size1)
    {psize, st2} = exprs([size], st1)

    {r_ibitstr(
       anno: r_a(anno: anno),
       val: pval,
       size: psize,
       unit: r_c_literal(val: unit),
       type: r_c_literal(val: type),
       flags: r_c_literal(val: flags)
     ), st2}
  end

  defp coerce_to_float(r_c_literal(val: int) = e, [:float | _])
       when is_integer(int) do
    try do
      r_c_literal(e, val: :erlang.float(int))
    catch
      :error, :badarg ->
        e
    end
  end

  defp coerce_to_float(e, _) do
    e
  end

  defp pat_alias(r_c_var(name: v1) = p, r_c_var(name: v1)) do
    p
  end

  defp pat_alias(
         r_c_var(name: v1) = var,
         r_c_alias(var: r_c_var(name: v2), pat: pat) = alias
       ) do
    cond do
      v1 === v2 ->
        alias

      true ->
        r_c_alias(alias, pat: pat_alias(var, pat))
    end
  end

  defp pat_alias(r_c_var() = p1, p2) do
    r_c_alias(var: p1, pat: p2)
  end

  defp pat_alias(r_c_alias(var: r_c_var(name: v1)) = alias, r_c_var(name: v1)) do
    alias
  end

  defp pat_alias(
         r_c_alias(var: r_c_var(name: v1) = var1, pat: p1),
         r_c_alias(var: r_c_var(name: v2) = var2, pat: p2)
       ) do
    pat = pat_alias(p1, p2)

    cond do
      v1 === v2 ->
        r_c_alias(var: var1, pat: pat)

      true ->
        pat_alias(var1, pat_alias(var2, pat))
    end
  end

  defp pat_alias(r_c_alias(var: r_c_var() = var, pat: p1), p2) do
    r_c_alias(var: var, pat: pat_alias(p1, p2))
  end

  defp pat_alias(r_imap(es: es1) = m, r_imap(es: es2)) do
    r_imap(m, es: pat_alias_map_pairs(es1 ++ es2))
  end

  defp pat_alias(p1, r_c_var() = var) do
    r_c_alias(var: var, pat: p1)
  end

  defp pat_alias(p1, r_c_alias(pat: p2) = alias) do
    r_c_alias(alias, pat: pat_alias(p1, p2))
  end

  defp pat_alias(p1, p2) do
    case :cerl.is_data(p1) and :cerl.is_data(p2) do
      false ->
        throw(:nomatch)

      true ->
        :ok
    end

    type = :cerl.data_type(p1)

    case :cerl.data_type(p2) do
      ^type ->
        :ok

      _ ->
        throw(:nomatch)
    end

    es1 = :cerl.data_es(p1)
    es2 = :cerl.data_es(p2)
    es = pat_alias_list(es1, es2)
    :cerl.make_data(type, es)
  end

  defp pat_alias_list([a1 | a1s], [a2 | a2s]) do
    [pat_alias(a1, a2) | pat_alias_list(a1s, a2s)]
  end

  defp pat_alias_list([], []) do
    []
  end

  defp pat_alias_list(_, _) do
    throw(:nomatch)
  end

  defp pattern_list([p0 | ps0], st0) do
    {p1, st1} = pattern(p0, st0)
    {ps1, st2} = pattern_list(ps0, st1)
    {[p1 | ps1], st2}
  end

  defp pattern_list([], st) do
    {[], st}
  end

  defp string_to_conses(line, cs, tail) do
    foldr(
      fn c, t ->
        {:cons, line, {:char, line, c}, t}
      end,
      tail,
      cs
    )
  end

  defp make_vars(vs) do
    for v <- vs do
      r_c_var(name: v)
    end
  end

  defp new_fun_name(r_core(function: {f, a}, fcount: i) = st) do
    name =
      '-' ++
        :erlang.atom_to_list(f) ++
        '/' ++ :erlang.integer_to_list(a) ++ '-fun-' ++ :erlang.integer_to_list(i) ++ '-'

    {:erlang.list_to_atom(name), r_core(st, fcount: i + 1)}
  end

  defp new_fun_name(type, r_core(fcount: c) = st) do
    {:erlang.list_to_atom(type ++ '$^' ++ :erlang.integer_to_list(c)), r_core(st, fcount: c + 1)}
  end

  defp new_var_name(r_core(vcount: c) = st) do
    {c, r_core(st, vcount: c + 1)}
  end

  defp new_var(st) do
    new_var([], st)
  end

  defp new_var(anno, st0) when is_list(anno) do
    {new, st} = new_var_name(st0)
    {r_c_var(anno: anno, name: new), st}
  end

  defp new_vars(n, st) do
    new_vars_1(n, [], st, [])
  end

  defp new_vars(anno, n, st) do
    new_vars_1(n, anno, st, [])
  end

  defp new_vars_1(n, anno, st0, vs) when n > 0 do
    {v, st1} = new_var(anno, st0)
    new_vars_1(n - 1, anno, st1, [v | vs])
  end

  defp new_vars_1(0, _, st, vs) do
    {vs, st}
  end

  defp function_clause(ps, lineAnno) do
    fail_clause(ps, lineAnno, ann_c_tuple(lineAnno, [r_c_literal(val: :function_clause) | ps]))
  end

  defp fail_clause(pats, anno, arg) do
    r_iclause(
      anno: r_a(anno: [:compiler_generated]),
      pats: pats,
      guard: [],
      body: [r_iprimop(anno: r_a(anno: anno), name: r_c_literal(val: :match_fail), args: [arg])]
    )
  end

  defp right_assoc({:op, l1, op, {:op, l2, op, e1, e2}, e3}, op) do
    right_assoc(
      {:op, l2, op, e1, {:op, l1, op, e2, e3}},
      op
    )
  end

  defp right_assoc(e, _Op) do
    e
  end

  defp annotate_tuple(a, es, r_core(dialyzer: dialyzer)) do
    case dialyzer do
      true ->
        node = :cerl.ann_c_tuple(a, [:cerl.c_var(:any)])
        :cerl.update_c_tuple_skel(node, es)

      false ->
        ann_c_tuple(a, es)
    end
  end

  defp annotate_cons(a, h, t, r_core(dialyzer: dialyzer)) do
    case dialyzer do
      true ->
        node = :cerl.ann_c_cons(a, :cerl.c_var(:any), :cerl.c_var(:any))
        :cerl.update_c_cons_skel(node, h, t)

      false ->
        ann_c_cons(a, h, t)
    end
  end

  defp ubody(b, st) do
    uexpr(b, [], st)
  end

  defp ufun_clauses(lcs, ks, st0) do
    mapfoldl(
      fn lc, st ->
        ufun_clause(lc, ks, st)
      end,
      st0,
      lcs
    )
  end

  defp ufun_clause(cl0, ks, st0) do
    {cl1, pvs, used, _, st1} = do_uclause(cl0, ks, st0)
    a0 = get_anno(cl1)
    a = r_a(a0, us: subtract(used, pvs), ns: [])
    {r_iclause(cl1, anno: a), st1}
  end

  defp uclauses(lcs, ks, st0) do
    mapfoldl(
      fn lc, st ->
        uclause(lc, ks, st)
      end,
      st0,
      lcs
    )
  end

  defp uclause(cl0, ks, st0) do
    {cl1, _Pvs, used, new, st1} = do_uclause(cl0, ks, st0)
    a0 = get_anno(cl1)
    a = r_a(a0, us: used, ns: new)
    {r_iclause(cl1, anno: a), st1}
  end

  defp do_uclause(r_iclause(anno: anno, pats: ps0, guard: g0, body: b0), ks0, st0) do
    {ps1, pg, pvs, pus, st1} = upattern_list(ps0, ks0, st0)
    pu = union(pus, intersection(pvs, ks0))
    pn = subtract(pvs, pu)
    ks1 = union(pn, ks0)
    {g1, st2} = uguard(pg, g0, ks1, st1)
    gu = used_in_any(g1)
    gn = new_in_any(g1)
    ks2 = union(gn, ks1)
    {b1, st3} = uexprs(b0, ks2, st2)

    used =
      intersection(
        union([pu, gu, used_in_any(b1)]),
        ks0
      )

    new = union([pn, gn, new_in_any(b1)])
    {r_iclause(anno: anno, pats: ps1, guard: g1, body: b1), pvs, used, new, st3}
  end

  defp uguard([], [], _, st) do
    {[], st}
  end

  defp uguard(pg, [], ks, st) do
    uguard(droplast(pg), [last(pg)], ks, st)
  end

  defp uguard(pg, gs0, ks, st0) do
    {gs3, st5} =
      foldr(
        fn t, {gs1, st1} ->
          {l, st2} = new_var(st1)
          {r, st3} = new_var(st2)

          {[r_iset(var: l, arg: t)] ++
             droplast(gs1) ++
             [
               r_iset(
                 var: r,
                 arg: last(gs1)
               ),
               r_icall(
                 anno: r_a(),
                 module: r_c_literal(val: :erlang),
                 name: r_c_literal(val: :and),
                 args: [l, r]
               )
             ], st3}
        end,
        {gs0, st0},
        pg
      )

    uexprs(gs3, ks, st5)
  end

  defp uexprs([r_imatch(anno: a, pat: p0, arg: arg, fc: fc) | les], ks, st0) do
    case upat_is_new_var(p0, ks) do
      true ->
        uexprs([r_iset(var: p0, arg: arg) | les], ks, st0)

      false when les === [] ->
        {la0, lps, st1} = force_safe(arg, st0)
        la = mark_compiler_generated(la0)
        mc = r_iclause(anno: a, pats: [p0], guard: [], body: [la])
        uexprs(lps ++ [r_icase(anno: a, args: [la0], clauses: [mc], fc: fc)], ks, st1)

      false ->
        mc = r_iclause(anno: a, pats: [p0], guard: [], body: les)
        uexprs([r_icase(anno: a, args: [arg], clauses: [mc], fc: fc)], ks, st0)
    end
  end

  defp uexprs([le0 | les0], ks, st0) do
    {le1, st1} = uexpr(le0, ks, st0)
    {les1, st2} = uexprs(les0, union(r_a(get_anno(le1), :ns), ks), st1)
    {[le1 | les1], st2}
  end

  defp uexprs([], _, st) do
    {[], st}
  end

  defp upat_is_new_var(r_c_var(name: v), ks) do
    not is_element(v, ks)
  end

  defp upat_is_new_var(_, _) do
    false
  end

  defp mark_compiler_generated(r_c_cons(anno: a, hd: h, tl: t)) do
    ann_c_cons([:compiler_generated | a], mark_compiler_generated(h), mark_compiler_generated(t))
  end

  defp mark_compiler_generated(r_c_tuple(anno: a, es: es0)) do
    es =
      for e <- es0 do
        mark_compiler_generated(e)
      end

    ann_c_tuple([:compiler_generated | a], es)
  end

  defp mark_compiler_generated(r_c_var(anno: a) = var) do
    r_c_var(var, anno: [:compiler_generated | a])
  end

  defp mark_compiler_generated(r_c_literal(anno: a) = lit) do
    r_c_literal(lit, anno: [:compiler_generated | a])
  end

  defp uexpr(r_iset(anno: a, var: v, arg: a0), ks, st0) do
    {a1, st1} = uexpr(a0, ks, st0)

    {r_iset(
       anno:
         r_a(a,
           us:
             del_element(
               r_c_var(v, :name),
               r_a(get_anno(a1), :us)
             ),
           ns: add_element(r_c_var(v, :name), r_a(get_anno(a1), :ns))
         ),
       var: v,
       arg: a1
     ), st1}
  end

  defp uexpr(r_iletrec(anno: a, defs: fs0, body: b0), ks, st0) do
    {fs1, st1} =
      mapfoldl(
        fn {name, f0}, s0 ->
          {f1, s1} = uexpr(f0, ks, s0)
          {{name, f1}, s1}
        end,
        st0,
        fs0
      )

    {b1, st2} = uexprs(b0, ks, st1)

    used =
      used_in_any(
        map(
          fn {_, f} ->
            f
          end,
          fs1
        ) ++ b1
      )

    {r_iletrec(anno: r_a(a, us: used, ns: []), defs: fs1, body: b1), st2}
  end

  defp uexpr(r_icase(anno: r_a(anno: anno) = a, args: as0, clauses: cs0, fc: fc0), ks, st0) do
    {as1, st1} = uexpr_list(as0, ks, st0)
    {cs1, st2} = uclauses(cs0, ks, st1)
    {fc1, st3} = uclause(fc0, ks, st2)
    used = union(used_in_any(as1), used_in_any(cs1))

    new =
      case member(:list_comprehension, anno) do
        true ->
          []

        false ->
          new_in_all(cs1)
      end

    {r_icase(anno: r_a(a, us: used, ns: new), args: as1, clauses: cs1, fc: fc1), st3}
  end

  defp uexpr(
         r_ifun(anno: a0, id: id, vars: as, clauses: cs0, fc: fc0, name: name) = fun0,
         ks0,
         st0
       ) do
    {fun1, st2} =
      case ks0 do
        [] ->
          {fun0, st0}

        [_ | _] ->
          {cs1, st1} = rename_shadowing_clauses(cs0, ks0, st0)
          {r_ifun(fun0, clauses: cs1), st1}
      end

    r_ifun(clauses: cs2) = fun1
    avs = lit_list_vars(as)

    ks1 =
      case name do
        :unnamed ->
          ks0

        {:named, fName} ->
          union(subtract([fName], avs), ks0)
      end

    ks2 = union(avs, ks1)
    {cs3, st3} = ufun_clauses(cs2, ks2, st2)
    {fc1, st4} = ufun_clause(fc0, ks2, st3)

    used =
      subtract(
        intersection(used_in_any(cs3), ks1),
        avs
      )

    a1 = r_a(a0, us: used, ns: [])
    {r_ifun(anno: a1, id: id, vars: as, clauses: cs3, fc: fc1, name: name), st4}
  end

  defp uexpr(r_iapply(anno: a, op: op, args: as), _, st) do
    used = union(lit_vars(op), lit_list_vars(as))
    {r_iapply(anno: r_a(a, us: used), op: op, args: as), st}
  end

  defp uexpr(r_iprimop(anno: a, name: name, args: as), _, st) do
    used = lit_list_vars(as)
    {r_iprimop(anno: r_a(a, us: used), name: name, args: as), st}
  end

  defp uexpr(r_icall(anno: a, module: mod, name: name, args: as), _, st) do
    used = union([lit_vars(mod), lit_vars(name), lit_list_vars(as)])
    {r_icall(anno: r_a(a, us: used), module: mod, name: name, args: as), st}
  end

  defp uexpr(r_itry(anno: a, args: as0, vars: vs, body: bs0, evars: evs, handler: hs0), ks, st0) do
    {as1, st1} = uexprs(as0, ks, st0)
    argKs = union(ks, new_in_any(as1))
    {bs1, st2} = uexprs(bs0, argKs, st1)
    {hs1, st3} = uexprs(hs0, ks, st2)
    used = intersection(used_in_any(bs1 ++ hs1 ++ as1), ks)

    {r_itry(
       anno: r_a(a, us: used, ns: []),
       args: as1,
       vars: vs,
       body: bs1,
       evars: evs,
       handler: hs1
     ), st3}
  end

  defp uexpr(r_icatch(anno: a, body: es0), ks, st0) do
    {es1, st1} = uexprs(es0, ks, st0)
    {r_icatch(anno: r_a(a, us: used_in_any(es1)), body: es1), st1}
  end

  defp uexpr(r_ireceive1(anno: a, clauses: cs0), ks, st0) do
    {cs1, st1} = uclauses(cs0, ks, st0)

    {r_ireceive1(
       anno:
         r_a(a,
           us: used_in_any(cs1),
           ns: new_in_all(cs1)
         ),
       clauses: cs1
     ), st1}
  end

  defp uexpr(r_ireceive2(anno: a, clauses: cs0, timeout: te0, action: tes0), ks, st0) do
    {te1, st1} = uexpr(te0, ks, st0)
    {cs1, st2} = uclauses(cs0, ks, st1)
    {tes1, st3} = uexprs(tes0, ks, st2)
    used = union([used_in_any(cs1), used_in_any(tes1), r_a(get_anno(te1), :us)])

    new =
      case cs1 do
        [] ->
          new_in_any(tes1)

        _ ->
          intersection(new_in_all(cs1), new_in_any(tes1))
      end

    {r_ireceive2(anno: r_a(a, us: used, ns: new), clauses: cs1, timeout: te1, action: tes1), st3}
  end

  defp uexpr(r_iprotect(anno: a, body: es0), ks, st0) do
    {es1, st1} = uexprs(es0, ks, st0)
    used = used_in_any(es1)
    {r_iprotect(anno: r_a(a, us: used), body: es1), st1}
  end

  defp uexpr(r_ibinary(anno: a, segments: ss), _, st) do
    used = bitstr_vars(ss)
    {r_ibinary(anno: r_a(a, us: used), segments: ss), st}
  end

  defp uexpr(r_c_literal() = lit, _, st) do
    anno = get_anno(lit)
    {set_anno(lit, r_a(us: [], anno: anno)), st}
  end

  defp uexpr(simple, _, st) do
    true = is_simple(simple)
    vs = lit_vars(simple)
    anno = get_anno(simple)
    {r_isimple(anno: r_a(us: vs, anno: anno), term: simple), st}
  end

  defp uexpr_list(les0, ks, st0) do
    mapfoldl(
      fn le, st ->
        uexpr(le, ks, st)
      end,
      st0,
      les0
    )
  end

  defp upattern(r_c_var(name: :_), _, st0) do
    {new, st1} = new_var_name(st0)
    {r_c_var(name: new), [], [new], [], st1}
  end

  defp upattern(r_c_var(name: v) = var, ks, st0) do
    case is_element(v, ks) do
      true ->
        {n, st1} = new_var_name(st0)
        new = r_c_var(name: n)
        lA = get_lineno_anno(var)

        test =
          r_icall(
            anno: r_a(anno: lA, us: add_element(n, [v])),
            module: r_c_literal(val: :erlang),
            name: r_c_literal(val: :"=:="),
            args: [new, var]
          )

        {new, [test], [n], [], st1}

      false ->
        {var, [], [v], [], st0}
    end
  end

  defp upattern(r_c_cons(hd: h0, tl: t0) = cons, ks, st0) do
    {h1, hg, hv, hu, st1} = upattern(h0, ks, st0)
    {t1, tg, tv, tu, st2} = upattern(t0, union(hv, ks), st1)
    {r_c_cons(cons, hd: h1, tl: t1), hg ++ tg, union(hv, tv), union(hu, tu), st2}
  end

  defp upattern(r_c_tuple(es: es0) = tuple, ks, st0) do
    {es1, esg, esv, eus, st1} = upattern_list(es0, ks, st0)
    {r_c_tuple(tuple, es: es1), esg, esv, eus, st1}
  end

  defp upattern(r_imap(es: es0) = map, ks, st0) do
    {es1, esg, esv, eus, st1} = upattern_list(es0, ks, st0)
    {r_imap(map, es: es1), esg, esv, eus, st1}
  end

  defp upattern(r_imappair(op: r_c_literal(val: :exact), key: k0, val: v0) = pair, ks, st0) do
    {v, vg, vn, vu, st1} = upattern(v0, ks, st0)
    {k, st2} = uexprs(k0, ks, st1)
    ku = used_in_expr(k)
    {r_imappair(pair, key: k, val: v), vg, vn, union(ku, vu), st2}
  end

  defp upattern(r_ibinary(segments: es0) = bin, ks, st0) do
    {es1, esg, esv, eus, st1} = upat_bin(es0, ks, st0)
    {r_ibinary(bin, segments: es1), esg, esv, eus, st1}
  end

  defp upattern(r_c_alias(var: v0, pat: p0) = alias, ks, st0) do
    {v1, vg, vv, vu, st1} = upattern(v0, ks, st0)
    {p1, pg, pv, pu, st2} = upattern(p0, union(vv, ks), st1)
    {r_c_alias(alias, var: v1, pat: p1), vg ++ pg, union(vv, pv), union(vu, pu), st2}
  end

  defp upattern(other, _, st) do
    {other, [], [], [], st}
  end

  defp upattern_list([p0 | ps0], ks, st0) do
    {p1, pg, pv, pu, st1} = upattern(p0, ks, st0)
    {ps1, psg, psv, psu, st2} = upattern_list(ps0, union(pv, ks), st1)
    {[p1 | ps1], pg ++ psg, union(pv, psv), union(pu, psu), st2}
  end

  defp upattern_list([], _, st) do
    {[], [], [], [], st}
  end

  defp upat_bin(es0, ks, st0) do
    {es1, pg, pv, pu0, st1} = upat_bin(es0, ks, [], st0)
    pu1 = subtract(pu0, intersection(pv, pu0))
    {es1, pg, pv, pu1, st1}
  end

  defp upat_bin([p0 | ps0], ks, bs, st0) do
    {p1, pg, pv, pu, bs1, st1} = upat_element(p0, ks, bs, st0)
    {ps1, psg, psv, psu, st2} = upat_bin(ps0, union(pv, ks), bs1, st1)
    {[p1 | ps1], pg ++ psg, union(pv, psv), union(pu, psu), st2}
  end

  defp upat_bin([], _, _, st) do
    {[], [], [], [], st}
  end

  defp upat_element(r_ibitstr(val: h0, size: sz0) = seg, ks, bs0, st0) do
    {h1, hg, hv, [], st1} = upattern(h0, ks, st0)

    bs1 =
      case h0 do
        r_c_var(name: hname) ->
          case h1 do
            r_c_var(name: ^hname) ->
              bs0

            r_c_var(name: other) ->
              [{hname, other} | bs0]
          end

        _ ->
          bs0
      end

    case sz0 do
      [r_c_var(name: vname)] ->
        {sz1, us} = rename_bitstr_size(vname, bs0)
        {sz2, st2} = uexprs([sz1], ks, st1)
        {r_ibitstr(seg, val: h1, size: sz2), hg, hv, us, bs1, st2}

      [r_c_literal()] ->
        {sz1, st2} = uexprs(sz0, ks, st1)
        us = []
        {r_ibitstr(seg, val: h1, size: sz1), hg, hv, us, bs1, st2}

      expr when is_list(expr) ->
        sz1 =
          for {old, new} <- bs0 do
            r_iset(var: r_c_var(name: old), arg: r_c_var(name: new))
          end ++ expr

        {sz2, st2} = uexprs(sz1, ks, st1)
        us = used_in_expr(sz2)
        {r_ibitstr(seg, val: h1, size: sz2), hg, hv, us, bs1, st2}
    end
  end

  defp rename_bitstr_size(v, [{v, n} | _]) do
    new = r_c_var(name: n)
    {new, [n]}
  end

  defp rename_bitstr_size(v, [_ | rest]) do
    rename_bitstr_size(v, rest)
  end

  defp rename_bitstr_size(v, []) do
    old = r_c_var(name: v)
    {old, [v]}
  end

  defp used_in_expr([le | les]) do
    r_a(us: us, ns: ns) = get_anno(le)
    used = used_in_expr(les)
    union(us, subtract(used, ns))
  end

  defp used_in_expr([]) do
    []
  end

  defp used_in_any(les) do
    foldl(
      fn le, ns ->
        union(r_a(get_anno(le), :us), ns)
      end,
      [],
      les
    )
  end

  defp new_in_any(les) do
    foldl(
      fn le, ns ->
        union(r_a(get_anno(le), :ns), ns)
      end,
      [],
      les
    )
  end

  defp new_in_all([le | les]) do
    foldl(
      fn l, ns ->
        intersection(r_a(get_anno(l), :ns), ns)
      end,
      r_a(get_anno(le), :ns),
      les
    )
  end

  defp new_in_all([]) do
    []
  end

  defp rename_shadowing_clauses([c0 | cs0], ks, st0) do
    {c, st1} = rename_shadowing_clause(c0, ks, st0)
    {cs, st} = rename_shadowing_clauses(cs0, ks, st1)
    {[c | cs], st}
  end

  defp rename_shadowing_clauses([], _Ks, st) do
    {[], st}
  end

  defp rename_shadowing_clause(r_iclause(pats: ps0, guard: g0, body: b0) = c, ks, st0) do
    subs = {[], []}
    {ps, {_Isub, osub}, st} = ren_pats(ps0, ks, subs, st0)

    g =
      case g0 do
        [] ->
          g0

        [_ | _] ->
          osub ++ g0
      end

    b = osub ++ b0
    {r_iclause(c, pats: ps, guard: g, body: b), st}
  end

  defp ren_pats([p0 | ps0], ks, {_, _} = subs0, st0) do
    {p, subs1, st1} = ren_pat(p0, ks, subs0, st0)
    {ps, subs, st} = ren_pats(ps0, ks, subs1, st1)
    {[p | ps], subs, st}
  end

  defp ren_pats([], _Ks, {_, _} = subs, st) do
    {[], subs, st}
  end

  defp ren_pat(r_c_var(name: :_) = p, _Ks, subs, st) do
    {p, subs, st}
  end

  defp ren_pat(r_c_var(name: v) = old, ks, {isub0, osub0} = subs, st0) do
    case member(v, ks) do
      true ->
        case ren_is_subst(v, osub0) do
          {:yes, new} ->
            {new, subs, st0}

          :no ->
            {new, st} = new_var(st0)
            osub = [r_iset(var: old, arg: new) | osub0]
            {new, {isub0, osub}, st}
        end

      false ->
        {old, subs, st0}
    end
  end

  defp ren_pat(r_c_literal() = p, _Ks, {_, _} = subs, st) do
    {p, subs, st}
  end

  defp ren_pat(r_c_alias(var: var0, pat: pat0) = alias, ks, {_, _} = subs0, st0) do
    {var, subs1, st1} = ren_pat(var0, ks, subs0, st0)
    {pat, subs, st} = ren_pat(pat0, ks, subs1, st1)
    {r_c_alias(alias, var: var, pat: pat), subs, st}
  end

  defp ren_pat(r_imap(es: es0) = map, ks, {_, _} = subs0, st0) do
    {es, subs, st} = ren_pat_map(es0, ks, subs0, st0)
    {r_imap(map, es: es), subs, st}
  end

  defp ren_pat(r_ibinary(segments: es0) = p, ks, {isub, osub0}, st0) do
    {es, _Isub, osub, st} = ren_pat_bin(es0, ks, isub, osub0, st0)
    {r_ibinary(p, segments: es), {isub, osub}, st}
  end

  defp ren_pat(p, ks0, {_, _} = subs0, st0) do
    es0 = :cerl.data_es(p)
    {es, subs, st} = ren_pats(es0, ks0, subs0, st0)
    {:cerl.make_data(:cerl.data_type(p), es), subs, st}
  end

  defp ren_pat_bin([r_ibitstr(val: val0, size: sz0) = e | es0], ks, isub0, osub0, st0) do
    sz = ren_get_subst(sz0, isub0)
    {val, {_, osub1}, st1} = ren_pat(val0, ks, {isub0, osub0}, st0)

    isub1 =
      case val0 do
        r_c_var() ->
          [r_iset(var: val0, arg: val) | isub0]

        _ ->
          isub0
      end

    {es, isub, osub, st} = ren_pat_bin(es0, ks, isub1, osub1, st1)
    {[r_ibitstr(e, val: val, size: sz) | es], isub, osub, st}
  end

  defp ren_pat_bin([], _Ks, isub, osub, st) do
    {[], isub, osub, st}
  end

  defp ren_pat_map([r_imappair(val: val0) = mapPair | es0], ks, subs0, st0) do
    {val, subs1, st1} = ren_pat(val0, ks, subs0, st0)
    {es, subs, st} = ren_pat_map(es0, ks, subs1, st1)
    {[r_imappair(mapPair, val: val) | es], subs, st}
  end

  defp ren_pat_map([], _Ks, subs, st) do
    {[], subs, st}
  end

  defp ren_get_subst([r_c_var(name: v)] = old, sub) do
    case ren_is_subst(v, sub) do
      :no ->
        old

      {:yes, new} ->
        [new]
    end
  end

  defp ren_get_subst([r_c_literal()] = old, _Sub) do
    old
  end

  defp ren_get_subst(expr, sub) when is_list(expr) do
    sub ++ expr
  end

  defp ren_is_subst(v, [r_iset(var: r_c_var(name: v), arg: arg) | _]) do
    {:yes, arg}
  end

  defp ren_is_subst(v, [_ | sub]) do
    ren_is_subst(v, sub)
  end

  defp ren_is_subst(_V, []) do
    :no
  end

  defp cbody(b0, st0) do
    {b1, _, _, st1} = cexpr(b0, [], st0)
    {b1, st1}
  end

  defp cclause(r_iclause(anno: r_a(anno: anno), pats: ps0, guard: g0, body: b0), exp, st0) do
    ps = cpattern_list(ps0)
    {b1, _Us1, st1} = cexprs(b0, exp, st0)
    {g1, st2} = cguard(g0, st1)
    {r_c_clause(anno: anno, pats: ps, guard: g1, body: b1), st2}
  end

  defp cclauses(lcs, es, st0) do
    mapfoldl(
      fn lc, st ->
        cclause(lc, es, st)
      end,
      st0,
      lcs
    )
  end

  defp cguard([], st) do
    {r_c_literal(val: true), st}
  end

  defp cguard(gs, st0) do
    {g, _, st1} = cexprs(gs, [], st0)
    {g, st1}
  end

  defp cpattern_list([p | ps]) do
    [cpattern(p) | cpattern_list(ps)]
  end

  defp cpattern_list([]) do
    []
  end

  defp cpattern(r_c_alias(pat: pat) = alias) do
    r_c_alias(alias, pat: cpattern(pat))
  end

  defp cpattern(r_c_cons(hd: hd, tl: tl) = cons) do
    r_c_cons(cons, hd: cpattern(hd), tl: cpattern(tl))
  end

  defp cpattern(r_c_tuple(es: es) = tup) do
    r_c_tuple(tup, es: cpattern_list(es))
  end

  defp cpattern(r_imap(anno: r_a(anno: anno), es: es)) do
    r_c_map(anno: anno, es: cpat_map_pairs(es), is_pat: true)
  end

  defp cpattern(r_ibinary(anno: r_a(anno: anno), segments: segs0)) do
    segs =
      for s <- segs0 do
        cpat_bin_seg(s)
      end

    r_c_binary(anno: anno, segments: segs)
  end

  defp cpattern(other) do
    other
  end

  defp cpat_map_pairs([
         r_imappair(anno: r_a(anno: anno), op: op, key: key0, val: val0)
         | t
       ]) do
    {key, _, _} = cexprs(key0, [], r_core())
    val = cpattern(val0)
    pair = r_c_map_pair(anno: anno, op: op, key: key, val: val)
    [pair | cpat_map_pairs(t)]
  end

  defp cpat_map_pairs([]) do
    []
  end

  defp cpat_bin_seg(
         r_ibitstr(anno: r_a(anno: anno), val: e, size: sz0, unit: unit, type: type, flags: flags)
       ) do
    {sz, _, _} = cexprs(sz0, [], r_core())
    r_c_bitstr(anno: anno, val: e, size: sz, unit: unit, type: type, flags: flags)
  end

  defp cexprs([r_iset(var: r_c_var(name: name) = var) = iset], as, st) do
    isimple = r_isimple(anno: r_a(us: [name]), term: var)
    cexprs([iset, isimple], as, st)
  end

  defp cexprs([le], as, st0) do
    {ce, es, us, st1} = cexpr(le, as, st0)
    exp = make_vars(as)

    cond do
      es === [] ->
        {:core_lib.make_values([ce | exp]), union(us, as), st1}

      true ->
        {r, st2} = new_var(st1)

        {r_c_let(
           anno: get_lineno_anno(ce),
           vars: [r | make_vars(es)],
           arg: ce,
           body: :core_lib.make_values([r | exp])
         ), union(us, as), st2}
    end
  end

  defp cexprs([r_iset(anno: r_a(anno: a), var: v, arg: a0) | les], as0, st0) do
    {ces, as1, st1} = cexprs(les, as0, st0)
    {a1, es, us, st2} = cexpr(a0, as1, st1)
    {r_c_let(anno: a, vars: [v | make_vars(es)], arg: a1, body: ces), union(us, as1), st2}
  end

  defp cexprs([le | les], as0, st0) do
    {ces, as1, st1} = cexprs(les, as0, st0)
    {ce, es, us, st2} = cexpr(le, as1, st1)

    cond do
      es === [] ->
        {r_c_seq(arg: ce, body: ces), union(us, as1), st2}

      true ->
        {r, st3} = new_var(st2)
        {r_c_let(vars: [r | make_vars(es)], arg: ce, body: ces), union(us, as1), st3}
    end
  end

  defp cexpr(r_iletrec(anno: a, defs: fs0, body: b0), as, st0) do
    {fs1, {_, st1}} =
      mapfoldl(
        fn {{_Name, _Arity} = nA, f0}, {used, s0} ->
          {f1, [], us, s1} = cexpr(f0, [], s0)
          {{r_c_var(name: nA), f1}, {union(us, used), s1}}
        end,
        {[], st0},
        fs0
      )

    exp = intersection(r_a(a, :ns), as)
    {b1, _Us, st2} = cexprs(b0, exp, st1)
    {r_c_letrec(anno: r_a(a, :anno), defs: fs1, body: b1), exp, r_a(a, :us), st2}
  end

  defp cexpr(r_icase(anno: a, args: largs, clauses: lcs, fc: lfc), as, st0) do
    exp = intersection(r_a(a, :ns), as)

    {cargs, st1} =
      foldr(
        fn la, {cas, sta} ->
          {ca, [], _Us1, stb} = cexpr(la, as, sta)
          {[ca | cas], stb}
        end,
        {[], st0},
        largs
      )

    {ccs, st2} = cclauses(lcs, exp, st1)
    {cfc0, st3} = cclause(lfc, [], st2)
    {cfc, st4} = c_add_dummy_export(cfc0, exp, st3)

    {r_c_case(anno: r_a(a, :anno), arg: :core_lib.make_values(cargs), clauses: ccs ++ [cfc]), exp,
     r_a(a, :us), st4}
  end

  defp cexpr(r_ireceive1(anno: a, clauses: lcs), as, st0) do
    exp = intersection(r_a(a, :ns), as)
    {ccs, st1} = cclauses(lcs, exp, st0)
    true__ = r_c_literal(val: true)

    action =
      :core_lib.make_values(
        :lists.duplicate(
          1 + length(exp),
          true__
        )
      )

    {r_c_receive(
       anno: r_a(a, :anno),
       clauses: ccs,
       timeout: r_c_literal(val: :infinity),
       action: action
     ), exp, r_a(a, :us), st1}
  end

  defp cexpr(r_ireceive2(anno: a, clauses: lcs, timeout: lto, action: les), as, st0) do
    exp = intersection(r_a(a, :ns), as)
    {cto, [], _Us1, st1} = cexpr(lto, as, st0)
    {ccs, st2} = cclauses(lcs, exp, st1)
    {ces, _Us2, st3} = cexprs(les, exp, st2)

    {r_c_receive(anno: r_a(a, :anno), clauses: ccs, timeout: cto, action: ces), exp, r_a(a, :us),
     st3}
  end

  defp cexpr(r_itry(anno: a, args: la, vars: vs0, body: lb, evars: evs, handler: lh), _As, st0) do
    asExp = intersection(new_in_any(la), used_in_any(lb))
    {ca, _Us1, st1} = cexprs(la, asExp, st0)
    {cb, _Us2, st2} = cexprs(lb, [], st1)
    {ch, _Us3, st3} = cexprs(lh, [], st2)

    vs =
      vs0 ++
        for v <- asExp do
          r_c_var(name: v)
        end

    {r_c_try(anno: r_a(a, :anno), arg: ca, vars: vs, body: cb, evars: evs, handler: ch), [],
     r_a(a, :us), st3}
  end

  defp cexpr(r_icatch(anno: a, body: les), _As, st0) do
    {ces, _Us1, st1} = cexprs(les, [], st0)
    {r_c_catch(body: ces), [], r_a(a, :us), st1}
  end

  defp cexpr(r_ifun(name: :unnamed) = fun, as, st0) do
    cfun(fun, as, st0)
  end

  defp cexpr(
         r_ifun(anno: r_a(us: us0) = a0, name: {:named, name}, fc: r_iclause(pats: ps)) = fun0,
         as,
         st0
       ) do
    case is_element(name, us0) do
      false ->
        cfun(fun0, as, st0)

      true ->
        a1 = r_a(a0, us: del_element(name, us0))
        fun1 = r_ifun(fun0, anno: a1)
        {r_c_fun(body: body) = cFun0, [], us1, st1} = cfun(fun1, as, st0)
        recVar = r_c_var(name: {name, length(ps)})
        let = r_c_let(vars: [r_c_var(name: name)], arg: recVar, body: body)
        cFun1 = r_c_fun(cFun0, body: let)
        letrec = r_c_letrec(anno: r_a(a0, :anno), defs: [{recVar, cFun1}], body: recVar)
        {letrec, [], us1, st1}
    end
  end

  defp cexpr(r_iapply(anno: a, op: op, args: args), _As, st) do
    {r_c_apply(anno: r_a(a, :anno), op: op, args: args), [], r_a(a, :us), st}
  end

  defp cexpr(r_icall(anno: a, module: mod, name: name, args: args), _As, st0) do
    anno = r_a(a, :anno)

    case not :cerl.is_c_atom(mod) and
           member(
             :tuple_calls,
             r_core(st0, :opts)
           ) do
      true ->
        genAnno = [:compiler_generated | anno]
        {tupleVar, st1} = new_var(genAnno, st0)
        {tupleSizeVar, st2} = new_var(genAnno, st1)
        {tupleModVar, st3} = new_var(genAnno, st2)
        {tupleArgsVar, st4} = new_var(genAnno, st3)
        tryVar = :cerl.c_var(:Try)

        tupleGuardExpr =
          :cerl.c_let(
            [tupleSizeVar],
            c_call_erl(:tuple_size, [tupleVar]),
            c_call_erl(
              :>,
              [tupleSizeVar, :cerl.c_int(0)]
            )
          )

        tupleGuard =
          :cerl.c_try(
            tupleGuardExpr,
            [tryVar],
            tryVar,
            [:cerl.c_var(:T), :cerl.c_var(:R)],
            :cerl.c_atom(false)
          )

        tupleApply =
          :cerl.c_let(
            [tupleModVar],
            c_call_erl(
              :element,
              [:cerl.c_int(1), tupleVar]
            ),
            :cerl.c_let(
              [tupleArgsVar],
              :cerl.make_list(args ++ [tupleVar]),
              c_call_erl(
                :apply,
                [tupleModVar, name, tupleArgsVar]
              )
            )
          )

        tupleClause = :cerl.ann_c_clause(genAnno, [tupleVar], tupleGuard, tupleApply)
        {otherVar, st5} = new_var(genAnno, st4)
        otherApply = :cerl.ann_c_call(genAnno, otherVar, name, args)
        otherClause = :cerl.ann_c_clause(genAnno, [otherVar], otherApply)
        {:cerl.ann_c_case(genAnno, mod, [tupleClause, otherClause]), [], r_a(a, :us), st5}

      false ->
        {r_c_call(anno: anno, module: mod, name: name, args: args), [], r_a(a, :us), st0}
    end
  end

  defp cexpr(r_iprimop(anno: a, name: name, args: args), _As, st) do
    {r_c_primop(anno: r_a(a, :anno), name: name, args: args), [], r_a(a, :us), st}
  end

  defp cexpr(r_iprotect(anno: a, body: es), _As, st0) do
    {ce, _, st1} = cexprs(es, [], st0)
    v = r_c_var(name: :Try)
    vs = [r_c_var(name: :T), r_c_var(name: :R)]

    {r_c_try(
       anno: r_a(a, :anno),
       arg: ce,
       vars: [v],
       body: v,
       evars: vs,
       handler: r_c_literal(val: false)
     ), [], r_a(a, :us), st1}
  end

  defp cexpr(r_ibinary(anno: r_a(anno: anno, us: us), segments: segs), _As, st) do
    {r_c_binary(anno: anno, segments: segs), [], us, st}
  end

  defp cexpr(r_c_literal() = lit, _As, st) do
    anno = get_anno(lit)
    vs = r_a(anno, :us)
    {set_anno(lit, r_a(anno, :anno)), [], vs, st}
  end

  defp cexpr(r_isimple(anno: r_a(us: vs), term: simple), _As, st) do
    true = is_simple(simple)
    {simple, [], vs, st}
  end

  defp cfun(r_ifun(anno: a, id: id, vars: args, clauses: lcs, fc: lfc), _As, st0) do
    {ccs, st1} = cclauses(lcs, [], st0)
    {cfc, st2} = cclause(lfc, [], st1)
    anno = r_a(a, :anno)

    {r_c_fun(
       anno: id ++ anno,
       vars: args,
       body:
         r_c_case(
           anno: anno,
           arg: set_anno(:core_lib.make_values(args), anno),
           clauses: ccs ++ [cfc]
         )
     ), [], r_a(a, :us), st2}
  end

  defp c_call_erl(fun, args) do
    as = [:compiler_generated]
    :cerl.ann_c_call(as, :cerl.c_atom(:erlang), :cerl.c_atom(fun), args)
  end

  defp c_add_dummy_export(r_c_clause(body: b0) = c, [_ | _] = exp, st0) do
    {v, st1} = new_var(st0)

    b =
      r_c_let(
        vars: [v],
        arg: b0,
        body: r_c_values(es: [v | duplicate(length(exp), r_c_literal(val: []))])
      )

    {r_c_clause(c, body: b), st1}
  end

  defp c_add_dummy_export(c, [], st) do
    {c, st}
  end

  defp lbody(b, st) do
    :cerl_trees.mapfold(&lexpr/2, st, b)
  end

  defp lexpr(r_c_case() = case__, st) do
    split_case(case__, st)
  end

  defp lexpr(
         r_c_receive(clauses: [], timeout: timeout0, action: action),
         st0
       ) do
    false__ = r_c_literal(val: false)
    true__ = r_c_literal(val: true)

    {timeout, outer0, st1} =
      case is_safe(timeout0) do
        true ->
          {timeout0, false__, st0}

        false ->
          {timeoutVar, sti0} = new_var(st0)
          outerLet = r_c_let(vars: [timeoutVar], arg: timeout0, body: false__)
          {timeoutVar, outerLet, sti0}
      end

    maybeIgnore =
      case timeout do
        r_c_literal(val: :infinity) ->
          [:dialyzer_ignore]

        _ ->
          []
      end

    {loopName, st2} = new_fun_name('recv', st1)
    loopFun = r_c_var(name: {loopName, 0})
    applyLoop = r_c_apply(anno: [:dialyzer_ignore], op: loopFun, args: [])

    timeoutCs = [
      r_c_clause(
        anno: maybeIgnore,
        pats: [true__],
        guard: true__,
        body: r_c_seq(arg: primop(:timeout), body: action)
      ),
      r_c_clause(
        anno: [:compiler_generated, :dialyzer_ignore],
        pats: [false__],
        guard: true__,
        body: applyLoop
      )
    ]

    {timeoutBool, st3} = new_var(st2)
    timeoutCase = r_c_case(anno: [:receive_timeout], arg: timeoutBool, clauses: timeoutCs)

    timeoutLet =
      r_c_let(
        vars: [timeoutBool],
        arg: primop(:recv_wait_timeout, [timeout]),
        body: timeoutCase
      )

    fun = r_c_fun(vars: [], body: timeoutLet)
    letrec = r_c_letrec(anno: [:letrec_goto], defs: [{loopFun, fun}], body: applyLoop)

    outer =
      case outer0 do
        r_c_let() ->
          r_c_let(outer0, body: letrec)

        _ ->
          letrec
      end

    {outer, st3}
  end

  defp lexpr(
         r_c_receive(anno: recvAnno, clauses: cs0, timeout: timeout0, action: action),
         st0
       ) do
    false__ = r_c_literal(val: false)
    true__ = r_c_literal(val: true)

    {timeout, outer0, st1} =
      case is_safe(timeout0) do
        true ->
          {timeout0, false__, st0}

        false ->
          {timeoutVar, sti0} = new_var(st0)
          outerLet = r_c_let(vars: [timeoutVar], arg: timeout0, body: false__)
          {timeoutVar, outerLet, sti0}
      end

    maybeIgnore =
      case timeout do
        r_c_literal(val: :infinity) ->
          [:dialyzer_ignore]

        _ ->
          []
      end

    {loopName, st2} = new_fun_name('recv', st1)
    loopFun = r_c_var(name: {loopName, 0})
    applyLoop = r_c_apply(anno: [:dialyzer_ignore], op: loopFun, args: [])
    cs1 = rewrite_cs(cs0)
    recvNext = r_c_seq(arg: primop(:recv_next), body: applyLoop)

    recvNextC =
      r_c_clause(
        anno: [:compiler_generated, :dialyzer_ignore],
        pats: [r_c_var(name: :Other)],
        guard: true__,
        body: recvNext
      )

    cs = cs1 ++ [recvNextC]
    {msg, st3} = new_var(st2)

    {msgCase, st4} =
      split_case(
        r_c_case(anno: recvAnno, arg: msg, clauses: cs),
        st3
      )

    timeoutCs = [
      r_c_clause(
        pats: [true__],
        guard: true__,
        body: r_c_seq(arg: primop(:timeout), body: action)
      ),
      r_c_clause(anno: [:dialyzer_ignore], pats: [false__], guard: true__, body: applyLoop)
    ]

    {timeoutBool, st5} = new_var(st4)
    timeoutCase = r_c_case(arg: timeoutBool, clauses: timeoutCs)

    timeoutLet =
      r_c_let(
        vars: [timeoutBool],
        arg: primop(:recv_wait_timeout, [timeout]),
        body: timeoutCase
      )

    {peekSucceeded, st6} = new_var(st5)

    peekCs = [
      r_c_clause(pats: [true__], guard: true__, body: msgCase),
      r_c_clause(anno: maybeIgnore, pats: [false__], guard: true__, body: timeoutLet)
    ]

    peekCase = r_c_case(arg: peekSucceeded, clauses: peekCs)
    peekLet = r_c_let(vars: [peekSucceeded, msg], arg: primop(:recv_peek_message), body: peekCase)
    fun = r_c_fun(vars: [], body: peekLet)
    letrec = r_c_letrec(anno: [:letrec_goto], defs: [{loopFun, fun}], body: applyLoop)

    outer =
      case outer0 do
        r_c_let() ->
          r_c_let(outer0, body: letrec)

        _ ->
          letrec
      end

    {outer, st6}
  end

  defp lexpr(tree, st) do
    {tree, st}
  end

  defp rewrite_cs([r_c_clause(body: b0) = c | cs]) do
    b = r_c_seq(arg: primop(:remove_message), body: b0)
    [r_c_clause(c, body: b) | rewrite_cs(cs)]
  end

  defp rewrite_cs([]) do
    []
  end

  defp primop(name) do
    primop(name, [])
  end

  defp primop(name, args) do
    r_c_primop(name: r_c_literal(val: name), args: args)
  end

  defp split_case(
         r_c_case(anno: caseAnno, arg: arg, clauses: cs0) = case0,
         st0
       ) do
    args =
      case arg do
        r_c_values(es: es) ->
          es

        _ ->
          [arg]
      end

    {varArgs, st1} = split_var_args(args, st0)

    case split_clauses(cs0, varArgs, caseAnno, st1) do
      :none ->
        {case0, st0}

      {preCase, aftCs, st2} ->
        aftCase =
          r_c_case(case0,
            arg: :core_lib.make_values(varArgs),
            clauses: aftCs
          )

        aftFun = r_c_fun(vars: [], body: aftCase)
        {letrec, st3} = split_case_letrec(aftFun, preCase, st2)
        body = split_letify(varArgs, args, letrec, [], [])
        {body, st3}
    end
  end

  defp split_var_args(args, st) do
    mapfoldl(
      fn
        r_c_var() = var, s0 ->
          {var, s0}

        r_c_literal() = lit, s0 ->
          {lit, s0}

        _, s0 ->
          new_var(s0)
      end,
      st,
      args
    )
  end

  defp split_letify([same | vs], [same | args], body, vsAcc, argAcc) do
    split_letify(vs, args, body, vsAcc, argAcc)
  end

  defp split_letify([v | vs], [arg | args], body, vsAcc, argAcc) do
    split_letify(vs, args, body, [v | vsAcc], [arg | argAcc])
  end

  defp split_letify([], [], body, [], []) do
    body
  end

  defp split_letify([], [], body, [_ | _] = vsAcc, [_ | _] = argAcc) do
    r_c_let(vars: reverse(vsAcc), arg: :core_lib.make_values(reverse(argAcc)), body: body)
  end

  defp split_case_letrec(r_c_fun(anno: funAnno0) = fun0, body, r_core(gcount: c) = st0) do
    funAnno = [:compiler_generated | funAnno0]
    fun = r_c_fun(fun0, anno: funAnno)
    anno = [:letrec_goto]
    defFunName = goto_func(c)
    letrec = r_c_letrec(anno: anno, defs: [{r_c_var(name: defFunName), fun}], body: body)
    st = r_core(st0, gcount: c + 1)
    lbody(letrec, st)
  end

  defp split_clauses([c0 | cs0], args, caseAnno, st0) do
    case split_clauses(cs0, args, caseAnno, st0) do
      :none ->
        case split_clause(c0, st0) do
          :none ->
            :none

          {ps, nested, st1} ->
            {case__, st2} = split_reconstruct(args, ps, nested, c0, caseAnno, st1)
            {case__, cs0, st2}
        end

      {case0, cs, st} ->
        r_c_case(clauses: newClauses) = case0
        case__ = r_c_case(case0, clauses: [c0 | newClauses])
        {case__, cs, st}
    end
  end

  defp split_clauses([], _, _, _) do
    :none
  end

  defp goto_func(count) do
    {:erlang.list_to_atom('label^' ++ :erlang.integer_to_list(count)), 0}
  end

  defp split_reconstruct(args, ps, nil, r_c_clause(anno: anno) = c0, caseAnno, st0) do
    c = r_c_clause(c0, pats: ps)
    {fc, st1} = split_fc_clause(ps, anno, st0)
    {r_c_case(anno: caseAnno, arg: :core_lib.make_values(args), clauses: [c, fc]), st1}
  end

  defp split_reconstruct(args, ps, {:split, splitArgs, pat, nested}, c, caseAnno, st) do
    split =
      {:split, splitArgs,
       fn body ->
         body
       end, pat, nested}

    split_reconstruct(args, ps, split, c, caseAnno, st)
  end

  defp split_reconstruct(
         args,
         ps,
         {:split, splitArgs, wrap, pat, nested},
         r_c_clause(anno: anno) = c0,
         caseAnno,
         st0
       ) do
    {innerCase, st1} = split_reconstruct(splitArgs, [pat], nested, c0, caseAnno, st0)
    {fc, st2} = split_fc_clause(args, anno, st1)
    wrapped = wrap.(innerCase)
    c = r_c_clause(c0, pats: ps, guard: r_c_literal(val: true), body: wrapped)
    {r_c_case(anno: caseAnno, arg: :core_lib.make_values(args), clauses: [c, fc]), st2}
  end

  defp split_fc_clause(args, anno0, r_core(gcount: count) = st0) do
    anno = [:compiler_generated | anno0]
    arity = length(args)
    {vars, st1} = new_vars(arity, st0)
    op = r_c_var(name: goto_func(count))
    apply = r_c_apply(anno: anno, op: op, args: [])

    {r_c_clause(
       anno: [:dialyzer_ignore | anno],
       pats: vars,
       guard: r_c_literal(val: true),
       body: apply
     ), st1}
  end

  defp split_clause(r_c_clause(pats: ps0), st0) do
    case split_pats(ps0, st0) do
      :none ->
        :none

      {ps, case__, st} ->
        {ps, case__, st}
    end
  end

  defp split_pats([p0 | ps0], st0) do
    case split_pats(ps0, st0) do
      :none ->
        case split_pat(p0, st0) do
          :none ->
            :none

          {p, case__, st} ->
            {[p | ps0], case__, st}
        end

      {ps, case__, st} ->
        {[p0 | ps], case__, st}
    end
  end

  defp split_pats([], _) do
    :none
  end

  defp split_pat(r_c_binary(segments: segs0) = bin, st0) do
    vars = :gb_sets.empty()

    case split_bin_segments(segs0, vars, st0, []) do
      :none ->
        :none

      {tailVar, wrap, bef, aft, st} ->
        befBin = r_c_binary(bin, segments: bef)
        {befBin, {:split, [tailVar], wrap, r_c_binary(bin, segments: aft), nil}, st}
    end
  end

  defp split_pat(r_c_map(es: es) = map, st) do
    split_map_pat(es, map, st, [])
  end

  defp split_pat(r_c_var(), _) do
    :none
  end

  defp split_pat(r_c_alias(pat: pat) = alias0, st0) do
    case split_pat(pat, st0) do
      :none ->
        :none

      {ps, split, st1} ->
        {var, st} = new_var(st1)
        alias = r_c_alias(alias0, pat: var)
        {alias, {:split, [var], ps, split}, st}
    end
  end

  defp split_pat(data, st0) do
    type = :cerl.data_type(data)
    es = :cerl.data_es(data)
    split_data(es, type, st0, [])
  end

  defp split_map_pat([r_c_map_pair(key: key, val: val) = e0 | es], map0, st0, acc) do
    case eval_map_key(key, e0, es, map0, st0) do
      :none ->
        case split_pat(val, st0) do
          :none ->
            split_map_pat(es, map0, st0, [e0 | acc])

          {ps, split, st1} ->
            {var, st} = new_var(st1)
            e = r_c_map_pair(e0, val: var)
            map = r_c_map(map0, es: reverse(acc, [e | es]))
            {map, {:split, [var], ps, split}, st}
        end

      {mapVar, split, st1} ->
        befMap0 = r_c_map(map0, es: reverse(acc))
        befMap = r_c_alias(var: mapVar, pat: befMap0)
        {befMap, split, st1}
    end
  end

  defp split_map_pat([], _, _, _) do
    :none
  end

  defp eval_map_key(r_c_var(), _E, _Es, _Map, _St) do
    :none
  end

  defp eval_map_key(r_c_literal(), _E, _Es, _Map, _St) do
    :none
  end

  defp eval_map_key(key, e0, es, map, st0) do
    {[keyVar, mapVar], st1} = new_vars(2, st0)
    e = r_c_map_pair(e0, key: keyVar)
    aftMap0 = r_c_map(map, es: [e | es])
    {wrap, caseArg, aftMap, st2} = wrap_map_key_fun(key, keyVar, mapVar, aftMap0, st1)
    {mapVar, {:split, [caseArg], wrap, aftMap, nil}, st2}
  end

  defp wrap_map_key_fun(key, keyVar, mapVar, aftMap, st0) do
    case is_safe(key) do
      true ->
        {fn body ->
           r_c_let(vars: [keyVar], arg: key, body: body)
         end, mapVar, aftMap, st0}

      false ->
        {[succVar | evars], st} = new_vars(4, st0)

        {fn body ->
           try =
             r_c_try(
               arg: key,
               vars: [keyVar],
               body: r_c_values(es: [r_c_literal(val: true), keyVar]),
               evars: evars,
               handler: r_c_values(es: [r_c_literal(val: false), r_c_literal(val: false)])
             )

           r_c_let(vars: [succVar, keyVar], arg: try, body: body)
         end, r_c_tuple(es: [succVar, mapVar]), r_c_tuple(es: [r_c_literal(val: true), aftMap]),
         st}
    end
  end

  defp split_data([e | es0], type, st0, acc) do
    case split_pat(e, st0) do
      :none ->
        split_data(es0, type, st0, [e | acc])

      {ps, split, st1} ->
        {var, st} = new_var(st1)
        data = :cerl.make_data(type, reverse(acc, [var | es0]))
        {data, {:split, [var], ps, split}, st}
    end
  end

  defp split_data([], _, _, _) do
    :none
  end

  defp split_bin_segments([r_c_bitstr(val: val, size: size) = s0 | segs], vars0, st0, acc) do
    vars =
      case val do
        r_c_var(name: v) ->
          :gb_sets.add(v, vars0)

        _ ->
          vars0
      end

    case size do
      r_c_literal() ->
        split_bin_segments(segs, vars, st0, [s0 | acc])

      r_c_var(name: sizeVar) ->
        case :gb_sets.is_member(sizeVar, vars0) do
          true ->
            {tailVar, tail, st} = split_tail_seg(s0, segs, st0)

            wrap = fn body ->
              body
            end

            {tailVar, wrap, reverse(acc, [tail]), [s0 | segs], st}

          false ->
            split_bin_segments(segs, vars, st0, [s0 | acc])
        end

      _ ->
        {tailVar, tail, st1} = split_tail_seg(s0, segs, st0)
        {sizeVar, st2} = new_var(st1)
        s = r_c_bitstr(s0, size: sizeVar)
        {wrap, st3} = split_wrap(sizeVar, size, st2)
        {tailVar, wrap, reverse(acc, [tail]), [s | segs], st3}
    end
  end

  defp split_bin_segments(_, _, _, _) do
    :none
  end

  defp split_tail_seg(r_c_bitstr(anno: a) = s, segs, st0) do
    {tailVar, st} = new_var(st0)
    unit = split_bin_unit([s | segs], st0)

    {tailVar,
     r_c_bitstr(
       anno: a,
       val: tailVar,
       size: r_c_literal(val: :all),
       unit: r_c_literal(val: unit),
       type: r_c_literal(val: :binary),
       flags: r_c_literal(val: [:unsigned, :big])
     ), st}
  end

  defp split_wrap(sizeVar, sizeExpr, st0) do
    {evars, st1} = new_vars(3, st0)

    {fn body ->
       try =
         r_c_try(
           arg: sizeExpr,
           vars: [sizeVar],
           body: sizeVar,
           evars: evars,
           handler: r_c_literal(val: :bad_size)
         )

       r_c_let(vars: [sizeVar], arg: try, body: body)
     end, st1}
  end

  defp split_bin_unit(ss, r_core(dialyzer: dialyzer)) do
    case dialyzer do
      true ->
        split_bin_unit_1(ss, 0)

      false ->
        1
    end
  end

  defp split_bin_unit_1(
         [
           r_c_bitstr(type: r_c_literal(val: type), size: size, unit: r_c_literal(val: u))
           | ss
         ],
         gCU
       ) do
    bits =
      case {type, size} do
        {:utf8, _} ->
          8

        {:utf16, _} ->
          16

        {:utf32, _} ->
          32

        {_, r_c_literal(val: 0)} ->
          1

        {_, r_c_literal(val: sz)} when is_integer(sz) ->
          sz * u

        {_, _} ->
          u
      end

    split_bin_unit_1(ss, gcd(gCU, bits))
  end

  defp split_bin_unit_1([], gCU) do
    gCU
  end

  defp gcd(a, b) do
    case rem(a, b) do
      0 ->
        b

      x ->
        gcd(b, x)
    end
  end

  defp lit_vars(lit) do
    lit_vars(lit, [])
  end

  defp lit_vars(r_c_cons(hd: h, tl: t), vs) do
    lit_vars(h, lit_vars(t, vs))
  end

  defp lit_vars(r_c_tuple(es: es), vs) do
    lit_list_vars(es, vs)
  end

  defp lit_vars(r_c_map(arg: v, es: es), vs) do
    lit_vars(v, lit_list_vars(es, vs))
  end

  defp lit_vars(r_c_map_pair(key: k, val: v), vs) do
    lit_vars(k, lit_vars(v, vs))
  end

  defp lit_vars(r_c_var(name: v), vs) do
    add_element(v, vs)
  end

  defp lit_vars(_, vs) do
    vs
  end

  defp lit_list_vars(ls) do
    lit_list_vars(ls, [])
  end

  defp lit_list_vars(ls, vs) do
    foldl(
      fn l, vs0 ->
        lit_vars(l, vs0)
      end,
      vs,
      ls
    )
  end

  defp bitstr_vars(segs) do
    bitstr_vars(segs, [])
  end

  defp bitstr_vars(segs, vs) do
    foldl(
      fn r_c_bitstr(val: v, size: s), vs0 ->
        lit_vars(v, lit_vars(s, vs0))
      end,
      vs,
      segs
    )
  end

  defp record_anno(l, r_core(dialyzer: dialyzer) = st) do
    case :erl_anno.record(l) and dialyzer do
      true ->
        [:record | lineno_anno(l, st)]

      false ->
        full_anno(l, st)
    end
  end

  defp full_anno(l, r_core(wanted: false) = st) do
    [:result_not_wanted | lineno_anno(l, st)]
  end

  defp full_anno(l, r_core(wanted: true) = st) do
    lineno_anno(l, st)
  end

  defp lineno_anno(l, st) do
    line = :erl_anno.line(l)
    generated = :erl_anno.generated(l)

    compilerGenerated =
      for _ <- [:EFE_DUMMY_GEN],
          generated do
        :compiler_generated
      end

    [line] ++ r_core(st, :file) ++ compilerGenerated
  end

  defp get_lineno_anno(ce) do
    case get_anno(ce) do
      r_a(anno: a) ->
        a

      a when is_list(a) ->
        a
    end
  end

  defp no_compiler_warning(anno) do
    :erl_anno.set_generated(true, anno)
  end

  defp get_anno(c) do
    :erlang.element(2, c)
  end

  defp set_anno(c, a) do
    :erlang.setelement(2, c, a)
  end

  defp is_simple(r_c_var()) do
    true
  end

  defp is_simple(r_c_literal()) do
    true
  end

  defp is_simple(r_c_cons(hd: h, tl: t)) do
    is_simple(h) and is_simple(t)
  end

  defp is_simple(r_c_tuple(es: es)) do
    is_simple_list(es)
  end

  defp is_simple(r_c_map(es: es)) do
    is_simple_list(es)
  end

  defp is_simple(r_c_map_pair(key: k, val: v)) do
    is_simple(k) and is_simple(v)
  end

  defp is_simple(_) do
    false
  end

  defp is_simple_list(es) do
    :lists.all(&is_simple/1, es)
  end

  def format_error(:nomatch) do
    'pattern cannot possibly match'
  end

  def format_error(:bad_binary) do
    'binary construction will fail because of a type mismatch'
  end

  def format_error({:map_key_repeated, key}) when is_atom(key) do
    :io_lib.format('key \'~w\' will be overridden in expression', [key])
  end

  def format_error({:map_key_repeated, key}) do
    :io_lib.format('key ~p will be overridden in expression', [key])
  end

  defp add_warning(anno, term, r_core(ws: ws, file: [{:file, file}]) = st) do
    case :erl_anno.generated(anno) do
      false ->
        r_core(st,
          ws: [
            {file, [{:erl_anno.location(anno), :v3_core, term}]}
            | ws
          ]
        )

      true ->
        st
    end
  end
end
