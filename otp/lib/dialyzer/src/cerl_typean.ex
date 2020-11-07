defmodule :m_cerl_typean do
  use Bitwise

  import :cerl,
    only: [
      alias_pat: 1,
      alias_var: 1,
      ann_c_fun: 3,
      ann_c_var: 2,
      apply_args: 1,
      apply_op: 1,
      atom_val: 1,
      binary_segments: 1,
      bitstr_flags: 1,
      bitstr_size: 1,
      bitstr_type: 1,
      bitstr_val: 1,
      c_letrec: 2,
      c_nil: 0,
      c_values: 1,
      call_args: 1,
      call_module: 1,
      call_name: 1,
      case_arg: 1,
      case_clauses: 1,
      catch_body: 1,
      clause_body: 1,
      clause_guard: 1,
      clause_pats: 1,
      concrete: 1,
      cons_hd: 1,
      cons_tl: 1,
      fun_body: 1,
      fun_vars: 1,
      get_ann: 1,
      int_val: 1,
      is_c_atom: 1,
      is_c_int: 1,
      let_arg: 1,
      let_body: 1,
      let_vars: 1,
      letrec_body: 1,
      letrec_defs: 1,
      module_defs: 1,
      module_exports: 1,
      pat_vars: 1,
      primop_args: 1,
      primop_name: 1,
      receive_action: 1,
      receive_clauses: 1,
      receive_timeout: 1,
      seq_arg: 1,
      seq_body: 1,
      set_ann: 2,
      try_arg: 1,
      try_body: 1,
      try_evars: 1,
      try_handler: 1,
      try_vars: 1,
      tuple_arity: 1,
      tuple_es: 1,
      type: 1,
      values_es: 1,
      var_name: 1
    ]

  import :cerl_trees, only: [get_label: 1]

  import :erl_types,
    only: [
      t_any: 0,
      t_atom: 0,
      t_atom_vals: 1,
      t_binary: 0,
      t_cons: 2,
      t_cons_hd: 1,
      t_cons_tl: 1,
      t_float: 0,
      t_from_range: 2,
      t_from_term: 1,
      t_fun: 0,
      t_fun: 2,
      t_inf: 2,
      t_integer: 0,
      t_is_any: 1,
      t_is_atom: 1,
      t_is_cons: 1,
      t_is_list: 1,
      t_is_maybe_improper_list: 1,
      t_is_none: 1,
      t_is_tuple: 1,
      t_limit: 2,
      t_list_elements: 1,
      t_maybe_improper_list: 0,
      t_none: 0,
      t_number: 0,
      t_pid: 0,
      t_port: 0,
      t_product: 1,
      t_reference: 0,
      t_sup: 2,
      t_to_tlist: 1,
      t_tuple: 0,
      t_tuple: 1,
      t_tuple_args: 1,
      t_tuple_size: 1,
      t_tuple_subtypes: 1
    ]

  def core_transform(code, _Opts) do
    {code1, _} = :cerl_trees.label(:cerl.from_records(code))
    {code2, _, _} = annotate(code1)
    :cerl.to_records(code2)
  end

  defp annotate(tree) do
    annotate(tree, 3)
  end

  defp annotate(tree, limit) do
    {_, _, esc, dep, par} = :cerl_closurean.analyze(tree)
    annotate(tree, limit, esc, dep, par)
  end

  defp annotate(tree, limit, esc, dep, par) do
    {type, out, vars} = analyze(tree, limit, esc, dep, par)

    delAnn = fn t ->
      set_ann(t, delete_ann(:type, get_ann(t)))
    end

    setType = fn t, dict ->
      case :dict.find(get_label(t), dict) do
        {:ok, x} ->
          case t_is_any(x) do
            true ->
              delAnn.(t)

            false ->
              set_ann(t, append_ann(:type, x, get_ann(t)))
          end

        :error ->
          delAnn.(t)
      end
    end

    f = fn t ->
      case type(t) do
        :var ->
          setType.(t, vars)

        :apply ->
          setType.(t, out)

        :call ->
          setType.(t, out)

        :primop ->
          setType.(t, out)

        :fun ->
          setType.(t, out)

        _ ->
          delAnn.(t)
      end
    end

    {:cerl_trees.map(f, tree), type, vars}
  end

  defp append_ann(tag, val, [x | xs]) do
    cond do
      tuple_size(x) >= 1 and
          :erlang.element(1, x) === tag ->
        append_ann(tag, val, xs)

      true ->
        [x | append_ann(tag, val, xs)]
    end
  end

  defp append_ann(tag, val, []) do
    [{tag, val}]
  end

  defp delete_ann(tag, [x | xs]) do
    cond do
      tuple_size(x) >= 1 and
          :erlang.element(1, x) === tag ->
        delete_ann(tag, xs)

      true ->
        [x | delete_ann(tag, xs)]
    end
  end

  defp delete_ann(_, []) do
    []
  end

  require Record

  Record.defrecord(:r_state, :state,
    k: :undefined,
    vars: :undefined,
    out: :undefined,
    dep: :undefined,
    work: :undefined,
    funs: :undefined,
    envs: :undefined
  )

  def analyze(tree) do
    analyze(tree, 3)
  end

  defp analyze(tree, limit) do
    {_, _, esc, dep, par} = :cerl_closurean.analyze(tree)
    analyze(tree, limit, esc, dep, par)
  end

  defp analyze(tree, limit, esc0, dep0, par) do
    labelExtL = [{:label, :external}]
    external = ann_c_var(labelExtL, {:external, 1})
    extFun = ann_c_fun(labelExtL, [], ann_c_var([{:label, :any}], :Any))
    labelTopL = [{:label, :top}]
    top = ann_c_var(labelTopL, {:top, 0})
    topFun = ann_c_fun(labelTopL, [], tree)

    startFun =
      ann_c_fun(
        [{:label, :start}],
        [],
        c_letrec(
          [{external, extFun}, {top, topFun}],
          c_nil()
        )
      )

    esc = :sets.from_list(esc0)
    any = t_any()
    none = t_none()
    funs0 = :dict.new()
    vars0 = :dict.store(:any, any, :dict.new())
    out0 = :dict.store(:top, none, :dict.store(:external, none, :dict.new()))
    envs0 = :dict.store(:top, :dict.new(), :dict.store(:external, :dict.new(), :dict.new()))

    f = fn t, s = {fs, vs, os, es} ->
      case type(t) do
        :fun ->
          l = get_label(t)
          as = fun_vars(t)

          x =
            case :sets.is_element(l, esc) do
              true ->
                any

              false ->
                none
            end

          {:dict.store(l, t, fs), bind_vars_single(as, x, vs), :dict.store(l, none, os),
           :dict.store(l, :dict.new(), es)}

        _ ->
          s
      end
    end

    {funs, vars, out, envs} =
      :cerl_trees.fold(
        f,
        {funs0, vars0, out0, envs0},
        startFun
      )

    dep =
      :lists.foldl(
        fn {l, l1}, d ->
          add_dep(l, l1, d)
        end,
        dep0,
        :dict.to_list(par)
      )

    st =
      loop(
        topFun,
        :top,
        r_state(
          vars: vars,
          out: out,
          dep: dep,
          work: init_work(),
          funs: funs,
          envs: envs,
          k: limit
        )
      )

    {:dict.fetch(:top, r_state(st, :out)), tidy_dict([:top, :external], r_state(st, :out)),
     tidy_dict([:any], r_state(st, :vars))}
  end

  defp tidy_dict([x | xs], d) do
    tidy_dict(xs, :dict.erase(x, d))
  end

  defp tidy_dict([], d) do
    d
  end

  defp loop(t, l, st0) do
    env = :dict.fetch(l, r_state(st0, :envs))
    x0 = :dict.fetch(l, r_state(st0, :out))
    {x1, st1} = visit(fun_body(t), env, st0)
    x = limit(x1, r_state(st1, :k))

    {w, m} =
      case equal(x0, x) do
        true ->
          {r_state(st1, :work), r_state(st1, :out)}

        false ->
          m1 = :dict.store(l, x, r_state(st1, :out))

          case :dict.find(l, r_state(st1, :dep)) do
            {:ok, s} ->
              {add_work(s, r_state(st1, :work)), m1}

            :error ->
              {r_state(st1, :work), m1}
          end
      end

    st2 = r_state(st1, out: m)

    case take_work(w) do
      {:ok, l1, w1} ->
        t1 = :dict.fetch(l1, r_state(st2, :funs))
        loop(t1, l1, r_state(st2, work: w1))

      :none ->
        st2
    end
  end

  defp visit(t, env, st) do
    case type(t) do
      :literal ->
        {t_from_term(concrete(t)), st}

      :var ->
        l = get_label(t)
        vars = r_state(st, :vars)

        case :dict.find(l, vars) do
          {:ok, x} ->
            case :dict.find(var_name(t), env) do
              {:ok, x1} ->
                {meet(x, x1), st}

              :error ->
                {x, st}
            end

          :error ->
            x = t_none()
            vars1 = :dict.store(l, x, vars)
            st1 = r_state(st, vars: vars1)
            {x, st1}
        end

      :fun ->
        l = get_label(t)

        xs =
          for v <- fun_vars(t) do
            :dict.fetch(get_label(v), r_state(st, :vars))
          end

        x = :dict.fetch(l, r_state(st, :out))

        st1 =
          r_state(st,
            work: add_work([l], r_state(st, :work)),
            envs: :dict.store(l, env, r_state(st, :envs))
          )

        {t_fun(xs, x), st1}

      :values ->
        {xs, st1} = visit_list(values_es(t), env, st)
        {t_product(xs), st1}

      :cons ->
        {[x1, x2], st1} = visit_list([cons_hd(t), cons_tl(t)], env, st)
        {t_cons(x1, x2), st1}

      :tuple ->
        {xs, st1} = visit_list(tuple_es(t), env, st)
        {t_tuple(xs), st1}

      :let ->
        {x, st1} = visit(let_arg(t), env, st)
        letVars = let_vars(t)
        st1Vars = r_state(st1, :vars)

        vars =
          case t_is_any(x) or t_is_none(x) do
            true ->
              bind_vars_single(letVars, x, st1Vars)

            false ->
              bind_vars(letVars, t_to_tlist(x), st1Vars)
          end

        visit(let_body(t), env, r_state(st1, vars: vars))

      :seq ->
        {_, st1} = visit(seq_arg(t), env, st)
        visit(seq_body(t), env, st1)

      :apply ->
        {_F, st1} = visit(apply_op(t), env, st)
        {as, st2} = visit_list(apply_args(t), env, st1)
        l = get_label(t)
        ls = get_deps(l, r_state(st, :dep))
        out = r_state(st2, :out)

        x =
          join_list(
            for l1 <- ls do
              :dict.fetch(l1, out)
            end
          )

        out1 = :dict.store(l, x, out)
        {x, call_site(ls, as, r_state(st2, out: out1))}

      :call ->
        m = call_module(t)
        f = call_name(t)
        as = call_args(t)
        {[x1, x2], st1} = visit_list([m, f], env, st)
        {xs, st2} = visit_list(as, env, st1)

        x =
          case {t_atom_vals(x1), t_atom_vals(x2)} do
            {[m1], [f1]} ->
              a = length(as)
              call_type(m1, f1, a, xs)

            _ ->
              t_any()
          end

        l = get_label(t)
        {x, r_state(st2, out: :dict.store(l, x, r_state(st2, :out)))}

      :primop ->
        as = primop_args(t)
        {xs, st1} = visit_list(as, env, st)
        f = atom_val(primop_name(t))
        a = length(as)
        l = get_label(t)
        x = primop_type(f, a, xs)
        {x, r_state(st1, out: :dict.store(l, x, r_state(st1, :out)))}

      :case ->
        {x, st1} = visit(case_arg(t), env, st)

        xs =
          case t_is_any(x) or t_is_none(x) do
            true ->
              for _ <- :cerl.case_clauses(t) do
                x
              end

            false ->
              t_to_tlist(x)
          end

        join_visit_clauses(xs, case_clauses(t), env, st1)

      :receive ->
        any = t_any()
        {x1, st1} = join_visit_clauses([any], receive_clauses(t), env, st)
        {x2, st2} = visit(receive_timeout(t), env, st1)

        case t_is_atom(x2) and t_atom_vals(x2) === [:infinity] do
          true ->
            {x1, st2}

          false ->
            {x3, st3} = visit(receive_action(t), env, st2)
            {join(x1, x3), st3}
        end

      :try ->
        {x, st1} = visit(try_arg(t), env, st)
        any = t_any()
        atom = t_atom()
        tryVars = try_vars(t)
        st1Vars = r_state(st1, :vars)

        vars =
          case t_is_any(x) or t_is_none(x) do
            true ->
              bind_vars_single(tryVars, x, st1Vars)

            false ->
              bind_vars(tryVars, t_to_tlist(x), st1Vars)
          end

        {x1, st2} = visit(try_body(t), env, r_state(st1, vars: vars))
        eVars = bind_vars(try_evars(t), [atom, any, any], r_state(st2, :vars))
        {x2, st3} = visit(try_handler(t), env, r_state(st2, vars: eVars))
        {join(x1, x2), st3}

      :catch ->
        {_, st1} = visit(catch_body(t), env, st)
        {t_any(), st1}

      :binary ->
        {_, st1} = visit_list(binary_segments(t), env, st)
        {t_binary(), st1}

      :bitstr ->
        {_, st1} = visit(bitstr_val(t), env, st)
        {_, st2} = visit(bitstr_size(t), env, st1)
        {t_none(), st2}

      :letrec ->
        vars = bind_defs(letrec_defs(t), r_state(st, :vars), r_state(st, :out))

        ls =
          for {_, f} <- letrec_defs(t) do
            get_label(f)
          end

        st1 =
          r_state(st,
            work: add_work(ls, r_state(st, :work)),
            vars: vars
          )

        visit(letrec_body(t), env, st1)

      :module ->
        {_, st1} =
          visit(
            c_letrec(
              module_defs(t),
              c_values(module_exports(t))
            ),
            env,
            st
          )

        {t_none(), st1}
    end
  end

  defp visit_clause(t, xs, env, st) do
    env1 = env
    vars = bind_pats(clause_pats(t), xs, r_state(st, :vars))
    g = clause_guard(t)
    {_, st1} = visit(g, env1, r_state(st, vars: vars))
    env2 = guard_filters(g, env1)
    visit(clause_body(t), env2, st1)
  end

  defp visit_list([t | ts], env, st) do
    {x, st1} = visit(t, env, st)
    {xs, st2} = visit_list(ts, env, st1)
    {[x | xs], st2}
  end

  defp visit_list([], _Env, st) do
    {[], st}
  end

  defp join_visit_clauses(xs, [t | ts], env, st) do
    {x1, st1} = visit_clause(t, xs, env, st)
    {x2, st2} = join_visit_clauses(xs, ts, env, st1)
    {join(x1, x2), st2}
  end

  defp join_visit_clauses(_, [], _Env, st) do
    {t_none(), st}
  end

  defp bind_defs([{v, f} | ds], vars, out) do
    xs =
      for v1 <- fun_vars(f) do
        :dict.fetch(get_label(v1), vars)
      end

    x = :dict.fetch(get_label(f), out)
    bind_defs(ds, :dict.store(get_label(v), t_fun(xs, x), vars), out)
  end

  defp bind_defs([], vars, _Out) do
    vars
  end

  defp bind_pats(ps, xs, vars) do
    cond do
      length(xs) === length(ps) ->
        bind_pats_list(ps, xs, vars)

      true ->
        bind_pats_single(ps, t_none(), vars)
    end
  end

  defp bind_pats_list([p | ps], [x | xs], vars) do
    vars1 = bind_pat_vars(p, x, vars)
    bind_pats_list(ps, xs, vars1)
  end

  defp bind_pats_list([], [], vars) do
    vars
  end

  defp bind_pats_single([p | ps], x, vars) do
    bind_pats_single(ps, x, bind_pat_vars(p, x, vars))
  end

  defp bind_pats_single([], _X, vars) do
    vars
  end

  defp bind_pat_vars(p, x, vars) do
    case type(p) do
      :var ->
        :dict.store(get_label(p), x, vars)

      :literal ->
        vars

      :cons ->
        case t_is_cons(x) do
          true ->
            vars1 = bind_pat_vars(cons_hd(p), t_cons_hd(x), vars)
            bind_pat_vars(cons_tl(p), t_cons_tl(x), vars1)

          false ->
            case t_is_list(x) do
              true ->
                vars1 = bind_pat_vars(cons_hd(p), t_list_elements(x), vars)
                bind_pat_vars(cons_tl(p), x, vars1)

              false ->
                case t_is_maybe_improper_list(x) do
                  true ->
                    x1 = t_list_elements(x)
                    vars1 = bind_pat_vars(cons_hd(p), x1, vars)
                    bind_pat_vars(cons_tl(p), x1, vars1)

                  false ->
                    bind_vars_single(pat_vars(p), top_or_bottom(x), vars)
                end
            end
        end

      :tuple ->
        case t_is_tuple(x) do
          true ->
            case t_tuple_subtypes(x) do
              :unknown ->
                bind_vars_single(pat_vars(p), top_or_bottom(x), vars)

              [tuple] ->
                case t_tuple_size(tuple) === tuple_arity(p) do
                  true ->
                    bind_pats_list(tuple_es(p), t_tuple_args(tuple), vars)

                  false ->
                    bind_vars_single(pat_vars(p), top_or_bottom(x), vars)
                end

              list when is_list(list) ->
                bind_vars_single(pat_vars(p), top_or_bottom(x), vars)
            end

          false ->
            bind_vars_single(pat_vars(p), top_or_bottom(x), vars)
        end

      :binary ->
        bind_pats_single(binary_segments(p), t_none(), vars)

      :bitstr ->
        size = bitstr_size(p)

        valType =
          case concrete(bitstr_type(p)) do
            :float ->
              t_float()

            :binary ->
              t_binary()

            :integer ->
              case is_c_int(size) do
                false ->
                  t_integer()

                true ->
                  sizeVal = int_val(size)
                  flags = concrete(bitstr_flags(p))

                  case :lists.member(:signed, flags) do
                    true ->
                      t_from_range(
                        -(1 <<< (sizeVal - 1)),
                        1 <<< (sizeVal - 1 - 1)
                      )

                    false ->
                      t_from_range(0, 1 <<< (sizeVal - 1))
                  end
              end
          end

        bind_pat_vars(bitstr_val(p), valType, vars)

      :alias ->
        p1 = alias_pat(p)
        vars1 = bind_pat_vars(p1, x, vars)
        :dict.store(get_label(alias_var(p)), pat_type(p1, vars1), vars1)
    end
  end

  defp pat_type(p, vars) do
    case type(p) do
      :var ->
        :dict.fetch(get_label(p), vars)

      :literal ->
        t_from_term(concrete(p))

      :cons ->
        t_cons(
          pat_type(cons_hd(p), vars),
          pat_type(cons_tl(p), vars)
        )

      :tuple ->
        t_tuple(
          for e <- tuple_es(p) do
            pat_type(e, vars)
          end
        )

      :binary ->
        t_binary()

      :alias ->
        pat_type(alias_pat(p), vars)
    end
  end

  defp bind_vars(vs, xs, vars) do
    cond do
      length(vs) === length(xs) ->
        bind_vars_list(vs, xs, vars)

      true ->
        bind_vars_single(vs, t_none(), vars)
    end
  end

  defp bind_vars_list([v | vs], [x | xs], vars) do
    bind_vars_list(vs, xs, :dict.store(get_label(v), x, vars))
  end

  defp bind_vars_list([], [], vars) do
    vars
  end

  defp bind_vars_single([v | vs], x, vars) do
    bind_vars_single(vs, x, :dict.store(get_label(v), x, vars))
  end

  defp bind_vars_single([], _X, vars) do
    vars
  end

  defp add_dep(source, target, deps) do
    case :dict.find(source, deps) do
      {:ok, x} ->
        case set__is_member(target, x) do
          true ->
            deps

          false ->
            :dict.store(source, set__add(target, x), deps)
        end

      :error ->
        :dict.store(source, set__singleton(target), deps)
    end
  end

  defp call_site(ls, xs, st) do
    {w, v} =
      call_site(
        ls,
        xs,
        r_state(st, :work),
        r_state(st, :vars),
        r_state(st, :funs),
        r_state(st, :k)
      )

    r_state(st, work: w, vars: v)
  end

  defp call_site([l | ls], xs, w, v, fs, limit) do
    vs = fun_vars(:dict.fetch(l, fs))

    case bind_args(vs, xs, v, limit) do
      {v1, true} ->
        call_site(ls, xs, add_work([l], w), v1, fs, limit)

      {v1, false} ->
        call_site(ls, xs, w, v1, fs, limit)
    end
  end

  defp call_site([], _, w, v, _, _) do
    {w, v}
  end

  defp bind_args(vs, xs, vars, limit) do
    cond do
      length(vs) === length(xs) ->
        bind_args(vs, xs, vars, limit, false)

      true ->
        {vars, false}
    end
  end

  defp bind_args([v | vs], [x | xs], vars, limit, ch) do
    l = get_label(v)
    {vars1, ch1} = bind_arg(l, x, vars, limit, ch)
    bind_args(vs, xs, vars1, limit, ch1)
  end

  defp bind_args([], [], vars, _Limit, ch) do
    {vars, ch}
  end

  defp bind_arg(l, x, vars, limit, ch) do
    x0 = :dict.fetch(l, vars)
    x1 = limit(join(x, x0), limit)

    case equal(x0, x1) do
      true ->
        {vars, ch}

      false ->
        {:dict.store(l, x1, vars), true}
    end
  end

  defp meet(x, y) do
    t_inf(x, y)
  end

  defp join(x, y) do
    t_sup(x, y)
  end

  defp join_list([xs | xss]) do
    join(xs, join_list(xss))
  end

  defp join_list([]) do
    t_none()
  end

  defp equal(x, y) do
    x === y
  end

  defp limit(x, k) do
    t_limit(x, k)
  end

  defp top_or_bottom(t) do
    case t_is_none(t) do
      true ->
        t

      false ->
        t_any()
    end
  end

  defp strict(xs, t) do
    case :erl_types.any_none(xs) do
      true ->
        t_none()

      false ->
        t
    end
  end

  defp set__singleton(x) do
    [x]
  end

  defp set__add(x, s) do
    :ordsets.add_element(x, s)
  end

  defp set__is_member(x, s) do
    :ordsets.is_element(x, s)
  end

  defp queue__new() do
    {[], []}
  end

  defp queue__put(x, {in__, out}) do
    {[x | in__], out}
  end

  defp queue__get({in__, [x | out]}) do
    {:ok, x, {in__, out}}
  end

  defp queue__get({[], _}) do
    :empty
  end

  defp queue__get({in__, _}) do
    [x | in1] = :lists.reverse(in__)
    {:ok, x, {[], in1}}
  end

  defp init_work() do
    {queue__put(:external, queue__new()), :sets.new()}
  end

  defp add_work(ls, {q, set}) do
    add_work(ls, q, set)
  end

  defp add_work([l | ls], q, set) do
    case :sets.is_element(l, set) do
      true ->
        add_work(ls, q, set)

      false ->
        add_work(ls, queue__put(l, q), :sets.add_element(l, set))
    end
  end

  defp add_work([], q, set) do
    {q, set}
  end

  defp take_work({queue0, set0}) do
    case queue__get(queue0) do
      {:ok, l, queue1} ->
        set1 = :sets.del_element(l, set0)
        {:ok, l, {queue1, set1}}

      :empty ->
        :none
    end
  end

  defp get_deps(l, dep) do
    case :dict.find(l, dep) do
      {:ok, ls} ->
        ls

      :error ->
        []
    end
  end

  defp primop_type(:match_fail, 1, _) do
    t_none()
  end

  defp primop_type(_, _, xs) do
    strict(xs, t_any())
  end

  defp call_type(m, f, a, xs) do
    :erl_bif_types.type(m, f, a, xs)
  end

  defp guard_filters(t, env) do
    guard_filters(t, env, :dict.new())
  end

  defp guard_filters(t, env, vars) do
    case type(t) do
      :call ->
        m = call_module(t)
        f = call_name(t)

        case is_c_atom(m) and is_c_atom(f) do
          true ->
            as = call_args(t)

            case {atom_val(m), atom_val(f), length(as)} do
              {:erlang, :and, 2} ->
                [a1, a2] = as
                guard_filters(a1, guard_filters(a2, env))

              {:erlang, :is_atom, 1} ->
                filter(as, t_atom(), env)

              {:erlang, :is_binary, 1} ->
                filter(as, t_binary(), env)

              {:erlang, :is_float, 1} ->
                filter(as, t_float(), env)

              {:erlang, :is_function, 1} ->
                filter(as, t_fun(), env)

              {:erlang, :is_integer, 1} ->
                filter(as, t_integer(), env)

              {:erlang, :is_list, 1} ->
                filter(as, t_maybe_improper_list(), env)

              {:erlang, :is_number, 1} ->
                filter(as, t_number(), env)

              {:erlang, :is_pid, 1} ->
                filter(as, t_pid(), env)

              {:erlang, :is_port, 1} ->
                filter(as, t_port(), env)

              {:erlang, :is_reference, 1} ->
                filter(as, t_reference(), env)

              {:erlang, :is_tuple, 1} ->
                filter(as, t_tuple(), env)

              _ ->
                env
            end

          false ->
            env
        end

      :var ->
        case :dict.find(var_name(t), vars) do
          {:ok, t1} ->
            guard_filters(t1, env, vars)

          :error ->
            env
        end

      :let ->
        case let_vars(t) do
          [v] ->
            guard_filters(let_body(t), env, :dict.store(var_name(v), let_arg(t), vars))

          _ ->
            env
        end

      :values ->
        case values_es(t) do
          [t1] ->
            guard_filters(t1, env, vars)

          _ ->
            env
        end

      _ ->
        env
    end
  end

  defp filter(as, x, env) do
    [a] = as

    case type(a) do
      :var ->
        v = var_name(a)

        case :dict.find(v, env) do
          {:ok, x1} ->
            :dict.store(v, meet(x, x1), env)

          :error ->
            :dict.store(v, x, env)
        end

      _ ->
        env
    end
  end

  def pp_hook() do
    &pp_hook/3
  end

  defp pp_hook(node, ctxt, cont) do
    as = :cerl.get_ann(node)

    as1 =
      :proplists.delete(
        :type,
        :proplists.delete(:label, as)
      )

    as2 =
      :proplists.delete(
        :typesig,
        :proplists.delete(:file, as1)
      )

    d = cont.(:cerl.set_ann(node, []), ctxt)

    t =
      case :proplists.lookup(:type, as) do
        {:type, t0} ->
          t0

        :none ->
          case :proplists.lookup(:typesig, as) do
            {:typesig, t0} ->
              t0

            :none ->
              t_any()
          end
      end

    d1 =
      case :erl_types.t_is_any(t) do
        true ->
          d

        false ->
          case :cerl.is_literal(node) do
            true ->
              d

            false ->
              s = :erl_types.t_to_string(t)

              q =
                :prettypr.beside(
                  :prettypr.text('::'),
                  :prettypr.text(s)
                )

              :prettypr.beside(d, q)
          end
      end

    :cerl_prettypr.annotate(d1, as2, ctxt)
  end
end
