defmodule :m_cerl_pmatch do
  use Bitwise
  import :lists, only: [all: 2, foldl: 3, foldr: 3, keysort: 2, mapfoldl: 3, splitwith: 2]

  def core_transform(m, opts) do
    :cerl.to_records(transform(:cerl.from_records(m), opts))
  end

  def transform(m, _Opts) do
    expr(m, env__empty())
  end

  def clauses(cs, env) do
    clauses(cs, :none, env)
  end

  defp clauses([c | _] = cs, else__, env) do
    vs = new_vars(:cerl.clause_arity(c), env)
    e = match(vs, cs, else__, add_vars(vs, env))
    {e, vs}
  end

  defp match([], cs, else__, _Env) do
    cs1 =
      cond do
        else__ === :none ->
          cs

        true ->
          cs ++ [:cerl.c_clause([], else__)]
      end

    case :cerl_clauses.reduce(cs1) do
      {true, {c, []}} ->
        :cerl.clause_body(c)

      {false, cs2} ->
        :cerl.c_case(:cerl.c_values([]), cs2)
    end
  end

  defp match([v | _] = vs, cs, else__, env) do
    foldr(
      fn csF, elseF ->
        match_var_con(vs, csF, elseF, env)
      end,
      else__,
      group(
        for c <- cs do
          unalias(c, v)
        end,
        &is_var_clause/1
      )
    )
  end

  defp group([], _F) do
    []
  end

  defp group([x | _] = xs, f) do
    group(xs, f, f.(x))
  end

  defp group(xs, f, p) do
    {first, rest} =
      splitwith(
        fn x ->
          f.(x) === p
        end,
        xs
      )

    [first | group(rest, f)]
  end

  defp is_var_clause(c) do
    :cerl.is_c_var(hd(:cerl.clause_pats(c)))
  end

  defp match_var_con(vs, cs, :none = else__, env) do
    match_var_con_1(vs, cs, else__, env)
  end

  defp match_var_con(vs, cs, else__, env) do
    case is_lightweight(else__) do
      true ->
        match_var_con_1(vs, cs, else__, env)

      false ->
        f = new_fvar('match_', 0, env)
        else1 = :cerl.c_apply(f, [])
        env1 = add_vars([f], env)

        :cerl.c_letrec(
          [{f, :cerl.c_fun([], else__)}],
          match_var_con_1(vs, cs, else1, env1)
        )
    end
  end

  defp match_var_con_1(vs, cs, else__, env) do
    case is_var_clause(hd(cs)) do
      true ->
        match_var(vs, cs, else__, env)

      false ->
        match_con(vs, cs, else__, env)
    end
  end

  defp match_var([v | vs], cs, else__, env) do
    cs1 =
      for c <- cs do
        [p | ps] = :cerl.clause_pats(c)
        g = make_let([p], v, :cerl.clause_guard(c))
        b = make_let([p], v, :cerl.clause_body(c))
        :cerl.update_c_clause(c, ps, g, b)
      end

    match(vs, cs1, else__, env)
  end

  defp match_con([v | vs], cs, else__, env) do
    case group_con(cs) do
      [{_, _, gs}] ->
        make_switch(
          v,
          for {dG, _, csG} <- gs do
            match_congroup(dG, vs, csG, else__, env)
          end,
          else__,
          env
        )

      ts ->
        cs1 =
          for {t, _, gs} <- ts do
            match_typegroup(t, v, vs, gs, else__, env)
          end

        make_switch(v, cs1, else__, env)
    end
  end

  defp match_typegroup(_T, _V, vs, [{d, _, cs}], else__, env)
       when :erlang.element(1, d) != {:binary} do
    match_congroup(d, vs, cs, else__, env)
  end

  defp match_typegroup(t, v, vs, gs, else__, env) do
    body =
      make_switch(
        v,
        for {d, _, cs} <- gs do
          match_congroup(d, vs, cs, else__, env)
        end,
        else__,
        env
      )

    typetest_clause(t, v, body, env)
  end

  defp match_congroup({{:binary}, segs}, vs, cs, else__, env) do
    body = match(vs, cs, else__, env)
    :cerl.c_clause([make_pat({:binary}, segs)], body)
  end

  defp match_congroup({d, a}, vs, cs, else__, env) do
    vs1 = new_vars(a, env)
    body = match(vs1 ++ vs, cs, else__, add_vars(vs1, env))
    :cerl.c_clause([make_pat(d, vs1)], body)
  end

  defp make_switch(v, cs, else__, env) do
    :cerl.c_case(
      v,
      cond do
        else__ === :none ->
          cs

        true ->
          cs ++ [:cerl.c_clause([new_var(env)], else__)]
      end
    )
  end

  defp group_con(cs) do
    {cs1, _} =
      mapfoldl(
        fn c, n ->
          [p | ps] = :cerl.clause_pats(c)
          ps1 = sub_pats(p) ++ ps
          g = :cerl.clause_guard(c)
          b = :cerl.clause_body(c)
          c1 = :cerl.update_c_clause(c, ps1, g, b)
          d = con_desc(p)
          {{d, n, c1}, n + 1}
        end,
        0,
        cs
      )

    css =
      group(
        keysort(1, cs1),
        fn {d, _, _} ->
          d
        end
      )

    gs =
      for c <- css do
        finalize_congroup(c)
      end

    gss =
      group(
        gs,
        fn {d, _, _} ->
          con_desc_type(d)
        end
      )

    ts =
      for g <- gss do
        finalize_typegroup(g)
      end

    keysort(2, ts)
  end

  defp finalize_congroup(cs) do
    [{d, n, _} | _] = cs1 = keysort(2, cs)

    {d, n,
     for {_, _, c} <- cs1 do
       c
     end}
  end

  defp finalize_typegroup(gs) do
    [{d, n, _} | _] = gs1 = keysort(2, gs)
    {con_desc_type(d), n, gs1}
  end

  defp unalias(c, v) do
    [p | ps] = :cerl.clause_pats(c)
    b = :cerl.clause_body(c)
    g = :cerl.clause_guard(c)
    unalias(p, v, ps, b, g, c)
  end

  defp unalias(p, v, ps, b, g, c) do
    case :cerl.type(p) do
      :alias ->
        v1 = :cerl.alias_var(p)
        b1 = make_let([v1], v, b)
        g1 = make_let([v1], v, g)
        unalias(:cerl.alias_pat(p), v, ps, b1, g1, c)

      _ ->
        :cerl.update_c_clause(c, [p | ps], g, b)
    end
  end

  defp typetest_clause([], _V, e, _Env) do
    :cerl.c_clause([:cerl.c_nil()], e)
  end

  defp typetest_clause(:atom, v, e, _Env) do
    typetest_clause_1(:is_atom, v, e)
  end

  defp typetest_clause(:integer, v, e, _Env) do
    typetest_clause_1(:is_integer, v, e)
  end

  defp typetest_clause(:float, v, e, _Env) do
    typetest_clause_1(:is_float, v, e)
  end

  defp typetest_clause(:cons, _V, e, env) do
    [v1, v2] = new_vars(2, env)
    :cerl.c_clause([:cerl.c_cons(v1, v2)], e)
  end

  defp typetest_clause(:tuple, v, e, _Env) do
    typetest_clause_1(:is_tuple, v, e)
  end

  defp typetest_clause(:binary, v, e, _Env) do
    typetest_clause_1(:is_binary, v, e)
  end

  defp typetest_clause_1(t, v, e) do
    :cerl.c_clause(
      [v],
      :cerl.c_call(:cerl.c_atom(:erlang), :cerl.c_atom(t), [v]),
      e
    )
  end

  defp con_desc(e) do
    case :cerl.type(e) do
      :cons ->
        {{:cons}, 2}

      :tuple ->
        {{:tuple}, :cerl.tuple_arity(e)}

      :binary ->
        {{:binary}, :cerl.binary_segments(e)}

      :literal ->
        case :cerl.concrete(e) do
          [_ | _] ->
            {{:cons}, 2}

          t when is_tuple(t) ->
            {{:tuple}, tuple_size(t)}

          v ->
            {v, 0}
        end

      _ ->
        throw({:bad_constructor, e})
    end
  end

  defp con_desc_type({[], _}) do
    []
  end

  defp con_desc_type({v, _}) when is_atom(v) do
    :atom
  end

  defp con_desc_type({v, _}) when is_integer(v) do
    :integer
  end

  defp con_desc_type({v, _}) when is_float(v) do
    :float
  end

  defp con_desc_type({{:cons}, 2}) do
    :cons
  end

  defp con_desc_type({{:tuple}, _}) do
    :tuple
  end

  defp con_desc_type({{:binary}, _}) do
    :binary
  end

  defp make_pat({:cons}, [v1, v2]) do
    :cerl.c_cons(v1, v2)
  end

  defp make_pat({:tuple}, vs) do
    :cerl.c_tuple(vs)
  end

  defp make_pat({:binary}, segs) do
    :cerl.c_binary(segs)
  end

  defp make_pat(val, []) do
    :cerl.abstract(val)
  end

  defp sub_pats(e) do
    case :cerl.type(e) do
      :cons ->
        [:cerl.cons_hd(e), :cerl.cons_tl(e)]

      :tuple ->
        :cerl.tuple_es(e)

      :binary ->
        []

      :literal ->
        case :cerl.concrete(e) do
          [h | t] ->
            [:cerl.abstract(h), :cerl.abstract(t)]

          t when is_tuple(t) ->
            for x <- :erlang.tuple_to_list(t) do
              :cerl.abstract(x)
            end

          _ ->
            []
        end

      _ ->
        throw({:bad_constructor_pattern, e})
    end
  end

  defp make_let(vs, a, b) do
    :cerl_lib.reduce_expr(:cerl.c_let(vs, a, b))
  end

  def expr(e, env) do
    case :cerl.type(e) do
      :binary ->
        es = expr_list(:cerl.binary_segments(e), env)
        :cerl.update_c_binary(e, es)

      :bitstr ->
        v = expr(:cerl.bitstr_val(e), env)
        sz = expr(:cerl.bitstr_size(e), env)
        unit = expr(:cerl.bitstr_unit(e), env)
        type = expr(:cerl.bitstr_type(e), env)
        :cerl.update_c_bitstr(e, v, sz, unit, type, :cerl.bitstr_flags(e))

      :literal ->
        e

      :var ->
        e

      :values ->
        es = expr_list(:cerl.values_es(e), env)
        :cerl.update_c_values(e, es)

      :cons ->
        h = expr(:cerl.cons_hd(e), env)
        t = expr(:cerl.cons_tl(e), env)
        :cerl.update_c_cons(e, h, t)

      :tuple ->
        es = expr_list(:cerl.tuple_es(e), env)
        :cerl.update_c_tuple(e, es)

      :let ->
        a = expr(:cerl.let_arg(e), env)
        vs = :cerl.let_vars(e)
        env1 = add_vars(vs, env)
        b = expr(:cerl.let_body(e), env1)
        :cerl.update_c_let(e, vs, a, b)

      :seq ->
        a = expr(:cerl.seq_arg(e), env)
        b = expr(:cerl.seq_body(e), env)
        :cerl.update_c_seq(e, a, b)

      :apply ->
        op = expr(:cerl.apply_op(e), env)
        as = expr_list(:cerl.apply_args(e), env)
        :cerl.update_c_apply(e, op, as)

      :call ->
        m = expr(:cerl.call_module(e), env)
        n = expr(:cerl.call_name(e), env)
        as = expr_list(:cerl.call_args(e), env)
        :cerl.update_c_call(e, m, n, as)

      :primop ->
        as = expr_list(:cerl.primop_args(e), env)
        :cerl.update_c_primop(e, :cerl.primop_name(e), as)

      :case ->
        a = expr(:cerl.case_arg(e), env)
        cs = expr_list(:cerl.case_clauses(e), env)
        {e1, vs} = clauses(cs, env)
        make_let(vs, a, e1)

      :clause ->
        vs = :cerl.clause_vars(e)
        env1 = add_vars(vs, env)
        g = expr(:cerl.clause_guard(e), env1)
        b = expr(:cerl.clause_body(e), env1)
        :cerl.update_c_clause(e, :cerl.clause_pats(e), g, b)

      :fun ->
        vs = :cerl.fun_vars(e)
        env1 = add_vars(vs, env)
        b = expr(:cerl.fun_body(e), env1)
        :cerl.update_c_fun(e, vs, b)

      :receive ->
        cs = expr_list(:cerl.receive_clauses(e), env)
        t = expr(:cerl.receive_timeout(e), env)
        a = expr(:cerl.receive_action(e), env)
        :cerl.update_c_receive(e, cs, t, a)

      :try ->
        a = expr(:cerl.try_arg(e), env)
        vs = :cerl.try_vars(e)
        b = expr(:cerl.try_body(e), add_vars(vs, env))
        evs = :cerl.try_evars(e)
        h = expr(:cerl.try_handler(e), add_vars(evs, env))
        :cerl.update_c_try(e, a, vs, b, evs, h)

      :catch ->
        b = expr(:cerl.catch_body(e), env)
        :cerl.update_c_catch(e, b)

      :letrec ->
        ds = :cerl.letrec_defs(e)
        env1 = add_defs(ds, env)
        ds1 = defs(ds, env1)
        b = expr(:cerl.letrec_body(e), env1)
        :cerl.update_c_letrec(e, ds1, b)

      :module ->
        ds = :cerl.module_defs(e)
        env1 = add_defs(ds, env)
        ds1 = defs(ds, env1)

        :cerl.update_c_module(
          e,
          :cerl.module_name(e),
          :cerl.module_exports(e),
          :cerl.module_attrs(e),
          ds1
        )
    end
  end

  defp expr_list(es, env) do
    for e <- es do
      expr(e, env)
    end
  end

  defp defs(ds, env) do
    for {v, f} <- ds do
      {v, expr(f, env)}
    end
  end

  defp new_var(env) do
    name = env__new_vname(env)
    :cerl.c_var(name)
  end

  defp new_vars(n, env) do
    for v <- env__new_vnames(n, env) do
      :cerl.c_var(v)
    end
  end

  defp new_fvar(a, n, env) do
    name = env__new_fname(a, n, env)
    :cerl.c_var(name)
  end

  defp add_vars(vs, env) do
    foldl(
      fn v, e ->
        env__bind(:cerl.var_name(v), [], e)
      end,
      env,
      vs
    )
  end

  defp add_defs(ds, env) do
    foldl(
      fn {v, _F}, e ->
        env__bind(:cerl.var_name(v), [], e)
      end,
      env,
      ds
    )
  end

  defp is_lightweight(e) do
    case :erlang.get(:cerl_pmatch_duplicate_code) do
      :never ->
        :cerl.type(e) === :var

      :always ->
        true

      _ ->
        is_lightweight_1(e)
    end
  end

  defp is_lightweight_1(e) do
    case :cerl.type(e) do
      :var ->
        true

      :literal ->
        true

      :fun ->
        true

      :values ->
        all(&is_simple/1, :cerl.values_es(e))

      :cons ->
        is_simple(:cerl.cons_hd(e)) and is_simple(:cerl.cons_tl(e))

      :tuple ->
        all(&is_simple/1, :cerl.tuple_es(e))

      :let ->
        is_simple(:cerl.let_arg(e)) and is_lightweight_1(:cerl.let_body(e))

      :seq ->
        is_simple(:cerl.seq_arg(e)) and is_lightweight_1(:cerl.seq_body(e))

      :primop ->
        all(&is_simple/1, :cerl.primop_args(e))

      :apply ->
        is_simple(:cerl.apply_op(e)) and
          all(
            &is_simple/1,
            :cerl.apply_args(e)
          )

      :call ->
        is_simple(:cerl.call_module(e)) and is_simple(:cerl.call_name(e)) and
          all(
            &is_simple/1,
            :cerl.call_args(e)
          )

      _ ->
        false
    end
  end

  defp is_simple(e) do
    case :cerl.type(e) do
      :var ->
        true

      :literal ->
        true

      :values ->
        all(&is_simple/1, :cerl.values_es(e))

      _ ->
        false
    end
  end

  defp env__bind(key, val, env) do
    :rec_env.bind(key, val, env)
  end

  defp env__empty() do
    :rec_env.empty()
  end

  defp env__new_vname(env) do
    :rec_env.new_key(env)
  end

  defp env__new_vnames(n, env) do
    :rec_env.new_keys(n, env)
  end

  defp env__new_fname(f, a, env) do
    :rec_env.new_key(
      fn x ->
        s = :erlang.integer_to_list(x)
        {:erlang.list_to_atom(f ++ s), a}
      end,
      env
    )
  end
end
