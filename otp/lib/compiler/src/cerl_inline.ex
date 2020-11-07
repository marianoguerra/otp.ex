defmodule :m_cerl_inline do
  use Bitwise

  import :cerl,
    only: [
      abstract: 1,
      alias_pat: 1,
      alias_var: 1,
      ann_c_let: 4,
      apply_args: 1,
      apply_op: 1,
      atom_name: 1,
      atom_val: 1,
      binary_segments: 1,
      bitstr_flags: 1,
      bitstr_size: 1,
      bitstr_type: 1,
      bitstr_unit: 1,
      bitstr_val: 1,
      c_fun: 2,
      c_int: 1,
      c_let: 3,
      c_seq: 2,
      c_tuple: 1,
      c_values: 1,
      c_var: 1,
      call_args: 1,
      call_module: 1,
      call_name: 1,
      case_arg: 1,
      case_arity: 1,
      case_clauses: 1,
      catch_body: 1,
      clause_body: 1,
      clause_guard: 1,
      clause_pats: 1,
      clause_vars: 1,
      concrete: 1,
      cons_hd: 1,
      cons_tl: 1,
      data_arity: 1,
      data_es: 1,
      data_type: 1,
      fname_arity: 1,
      fun_body: 1,
      fun_vars: 1,
      get_ann: 1,
      int_val: 1,
      is_c_atom: 1,
      is_c_cons: 1,
      is_c_fname: 1,
      is_c_int: 1,
      is_c_list: 1,
      is_c_seq: 1,
      is_c_tuple: 1,
      is_c_var: 1,
      is_data: 1,
      is_literal: 1,
      is_literal_term: 1,
      let_arg: 1,
      let_body: 1,
      let_vars: 1,
      letrec_body: 1,
      letrec_defs: 1,
      list_elements: 1,
      list_length: 1,
      make_data_skel: 2,
      make_list: 1,
      map_arg: 1,
      map_es: 1,
      map_pair_key: 1,
      map_pair_op: 1,
      map_pair_val: 1,
      module_attrs: 1,
      module_defs: 1,
      module_exports: 1,
      module_name: 1,
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
      update_c_alias: 3,
      update_c_apply: 3,
      update_c_binary: 2,
      update_c_bitstr: 6,
      update_c_call: 4,
      update_c_case: 3,
      update_c_catch: 2,
      update_c_clause: 4,
      update_c_let: 4,
      update_c_letrec: 3,
      update_c_map: 3,
      update_c_map_pair: 4,
      update_c_module: 5,
      update_c_primop: 3,
      update_c_receive: 4,
      update_c_seq: 3,
      update_c_try: 6,
      update_c_values: 2,
      update_data: 3,
      values_es: 1,
      var_name: 1
    ]

  import :lists, only: [foldl: 3, foldr: 3, mapfoldl: 3, member: 2, reverse: 1]

  defp debug_runtime() do
    false
  end

  defp debug_counters() do
    false
  end

  defp default_effort() do
    150
  end

  defp default_size() do
    24
  end

  defp default_unroll() do
    1
  end

  defp weight(:var) do
    0
  end

  defp weight(:values) do
    0
  end

  defp weight(:literal) do
    1
  end

  defp weight(:data) do
    1
  end

  defp weight(:element) do
    1
  end

  defp weight(:argument) do
    1
  end

  defp weight(:fun) do
    6
  end

  defp weight(:let) do
    0
  end

  defp weight(:letrec) do
    0
  end

  defp weight(:case) do
    0
  end

  defp weight(:clause) do
    1
  end

  defp weight(:receive) do
    9
  end

  defp weight(:try) do
    1
  end

  defp weight(:catch) do
    1
  end

  defp weight(:apply) do
    3
  end

  defp weight(:call) do
    3
  end

  defp weight(:primop) do
    2
  end

  defp weight(:binary) do
    4
  end

  defp weight(:bitstr) do
    3
  end

  defp weight(:map) do
    4
  end

  defp weight(:map_pair) do
    3
  end

  defp weight(:module) do
    1
  end

  require Record
  Record.defrecord(:r_ref, :ref, name: :undefined, opnd: :undefined, loc: :undefined)

  Record.defrecord(:r_opnd, :opnd,
    expr: :undefined,
    ren: :undefined,
    env: :undefined,
    loc: :undefined,
    effort: :undefined,
    no_inline: :undefined
  )

  Record.defrecord(:r_cache, :cache,
    expr: :undefined,
    size: :undefined
  )

  Record.defrecord(:r_app, :app, opnds: :undefined, ctxt: :undefined, loc: :undefined)

  def core_transform(code, opts) do
    :cerl.to_records(
      transform(
        :cerl.from_records(code),
        opts
      )
    )
  end

  def transform(tree) do
    transform(tree, [])
  end

  def transform(tree, opts) do
    main(tree, :value, opts)
  end

  defp main(tree, ctxt, opts) do
    opts1 =
      opts ++
        [
          {:inline_size, default_size()},
          {:inline_effort, default_effort()},
          {:inline_unroll, default_unroll()}
        ]

    reply = self()

    pid =
      spawn_link(fn ->
        start(reply, tree, ctxt, opts1)
      end)

    receive do
      {^pid, tree1} ->
        tree1
    end
  end

  defp start(reply, tree, ctxt, opts) do
    init_debug()

    case debug_runtime() do
      false ->
        :ok
    end

    size =
      :erlang.max(
        1,
        :proplists.get_value(:inline_size, opts)
      )

    effort =
      :erlang.max(
        1,
        :proplists.get_value(:inline_effort, opts)
      )

    unroll =
      :erlang.max(
        1,
        :proplists.get_value(:inline_unroll, opts)
      )

    case :proplists.get_bool(:verbose, opts) do
      true ->
        :io.fwrite('Inlining: inline_size=~w inline_effort=~w\n', [size, effort])

      false ->
        :ok
    end

    s = st__new(effort, size, unroll)
    {tree2, _S2} = i(tree, ctxt, s)
    report_debug()
    send(reply, {self(), tree2})
  end

  defp init_debug() do
    case debug_counters() do
      false ->
        :ok
    end
  end

  defp report_debug() do
    case debug_runtime() do
      false ->
        :ok
    end

    case debug_counters() do
      false ->
        :ok
    end
  end

  defp i(e, ctxt, s) do
    i(e, ctxt, ren__identity(), env__empty(), s)
  end

  defp i(e, ctxt, ren, env, s0) do
    s = count_effort(1, s0)

    case is_data(e) do
      true ->
        i_data(e, ctxt, ren, env, s)

      false ->
        case type(e) do
          :var ->
            i_var(e, ctxt, ren, env, s)

          :values ->
            i_values(e, ctxt, ren, env, s)

          :fun ->
            i_fun(e, ctxt, ren, env, s)

          :seq ->
            i_seq(e, ctxt, ren, env, s)

          :let ->
            i_let(e, ctxt, ren, env, s)

          :letrec ->
            i_letrec(e, ctxt, ren, env, s)

          :case ->
            i_case(e, ctxt, ren, env, s)

          :receive ->
            i_receive(e, ctxt, ren, env, s)

          :apply ->
            i_apply(e, ctxt, ren, env, s)

          :call ->
            i_call(e, ctxt, ren, env, s)

          :primop ->
            i_primop(e, ren, env, s)

          :try ->
            i_try(e, ctxt, ren, env, s)

          :catch ->
            i_catch(e, ctxt, ren, env, s)

          :binary ->
            i_binary(e, ren, env, s)

          :map ->
            i_map(e, ctxt, ren, env, s)

          :module ->
            i_module(e, ctxt, ren, env, s)
        end
    end
  end

  defp i_data(e, ctxt, ren, env, s) do
    case is_literal(e) do
      true ->
        case ctxt do
          :effect ->
            {void(), count_size(weight(:literal), s)}

          _ ->
            {e, count_size(weight(:literal), s)}
        end

      false ->
        case ctxt do
          :effect ->
            {es1, s1} =
              mapfoldl(
                fn e, s ->
                  i(e, :effect, ren, env, s)
                end,
                s,
                data_es(e)
              )

            e1 =
              foldl(
                fn e1, e2 ->
                  make_seq(e1, e2)
                end,
                void(),
                es1
              )

            {e1, s1}

          _ ->
            {es1, s1} =
              mapfoldl(
                fn e, s ->
                  i(e, :value, ren, env, s)
                end,
                s,
                data_es(e)
              )

            n = weight(:data) + length(es1) * weight(:element)
            s2 = count_size(n, s1)
            {update_data(e, data_type(e), es1), s2}
        end
    end
  end

  defp i_var(e, ctxt, ren, env, s) do
    case ctxt do
      :effect ->
        {void(), count_size(weight(:literal), s)}

      _ ->
        name = var_name(e)

        case env__lookup(ren__map(name, ren), env) do
          {:ok, r} ->
            case r_ref(r, :opnd) do
              :undefined ->
                residualize_var(r, s)

              opnd ->
                i_var_1(r, opnd, ctxt, env, s)
            end

          :error ->
            {e, count_size(weight(:var), s)}
        end
    end
  end

  defp i_var_1(r, opnd, ctxt, env, s) do
    l = r_opnd(opnd, :loc)

    case st__test_inner_pending(l, s) do
      true ->
        residualize_var(r, s)

      false ->
        s1 = st__mark_inner_pending(l, s)

        try do
          visit(opnd, s1)
        catch
          x ->
            _S2 = st__clear_inner_pending(r_opnd(opnd, :loc), s1)
            throw(x)
        else
          {e, s2} ->
            s3 = st__clear_inner_pending(l, s2)
            copy(r, opnd, e, ctxt, env, s3)
        end
    end
  end

  defp i_values(e, ctxt, ren, env, s) do
    case values_es(e) do
      [e1] ->
        i(e1, ctxt, ren, env, s)

      es ->
        case ctxt do
          :effect ->
            {es1, s1} =
              mapfoldl(
                fn e, s ->
                  i(e, :effect, ren, env, s)
                end,
                s,
                es
              )

            e1 =
              foldl(
                fn e1, e2 ->
                  make_seq(e1, e2)
                end,
                void(),
                es1
              )

            {e1, s1}

          _ ->
            {es1, s1} =
              mapfoldl(
                fn e, s ->
                  i(e, :value, ren, env, s)
                end,
                s,
                es
              )

            s2 = count_size(weight(:values), s1)
            {update_c_values(e, es1), s2}
        end
    end
  end

  defp i_let(e, ctxt, ren, env, s) do
    case let_vars(e) do
      [v] ->
        i_let_1(v, e, ctxt, ren, env, s)

      vs ->
        {a, s1} = i(let_arg(e), :value, ren, env, s)

        case get_components(length(vs), result(a)) do
          {true, as} ->
            {e1, s2} = i_let_2(vs, as, e, ctxt, ren, env, s1)
            {hoist_effects(a, e1), s2}

          false ->
            {_, ren1, env1, s2} = bind_locals(vs, ren, env, s1)
            vs1 = i_params(vs, ren1, env1)
            {b, s3} = i(let_body(e), :value, ren1, env1, s2)
            s4 = count_size(weight(:let), s3)
            {update_c_let(e, vs1, a, b), s4}
        end
    end
  end

  defp i_let_1(v, e, ctxt, ren, env, s) do
    {opnd, s1} = make_opnd(let_arg(e), ren, env, s)
    {[r], ren1, env1, s2} = bind_locals([v], [opnd], ren, env, s1)
    {e1, s3} = i(let_body(e), ctxt, ren1, env1, s2)
    i_let_3([r], [opnd], e1, s3)
  end

  defp i_let_2(vs, as, e, ctxt, ren, env, s) do
    {opnds, s1} =
      mapfoldl(
        fn e, s ->
          make_opnd(e, ren__identity(), env, s)
        end,
        s,
        as
      )

    {rs, ren1, env1, s2} = bind_locals(vs, opnds, ren, env, s1)
    {e1, s3} = i(let_body(e), ctxt, ren1, env1, s2)
    i_let_3(rs, opnds, e1, s3)
  end

  defp i_let_3(rs, opnds, e, s) do
    {e1, s1} = make_let_bindings(rs, e, s)
    residualize_operands(opnds, e1, s1)
  end

  defp i_seq(e, ctxt, ren, env, s) do
    {e1, s1} = i(seq_arg(e), :effect, ren, env, s)
    {e2, s2} = i(seq_body(e), ctxt, ren, env, s1)
    {make_seq(e1, e2), s2}
  end

  defp i_case(e, ctxt, ren, env, s) do
    {a, s1} = i(case_arg(e), :value, ren, env, s)
    a1 = result(a)
    ctxt1 = safe_context(ctxt)

    {e1, s2} =
      case get_components(case_arity(e), a1) do
        {true, as} ->
          i_case_1(as, e, ctxt1, ren, env, s1)

        false ->
          i_case_1([], e, ctxt1, ren, env, s1)
      end

    {hoist_effects(a, e1), s2}
  end

  defp i_case_1(as, e, ctxt, ren, env, s) do
    case i_clauses(as, case_clauses(e), ctxt, ren, env, s) do
      {false, {as1, vs, env1, cs}, s1} ->
        cond do
          cs === [] ->
            report_warning('empty list of clauses in residual program!.\n')

          true ->
            :ok
        end

        {a, s2} = i(c_values(as1), :value, ren__identity(), env1, s1)
        {e1, s3} = i_case_2(cs, a, e, s2)
        i_case_3(vs, env1, e1, s3)

      {true, {_, vs, env1, [c]}, s1} ->
        i_case_3(vs, env1, clause_body(c), s1)
    end
  end

  defp i_case_2(cs, a, e, s) do
    case equivalent_clauses(cs) do
      false ->
        n = weight(:case) + weight(:clause) * length(cs)
        s1 = count_size(n, s)
        {update_c_case(e, a, cs), s1}

      true ->
        case :cerl_clauses.any_catchall(cs) do
          true ->
            e1 = make_seq(a, clause_body(hd(cs)))
            {e1, s}

          false ->
            e1 = update_c_case(e, a, set_clause_bodies(cs, void()))
            {make_seq(e1, clause_body(hd(cs))), s}
        end
    end
  end

  defp i_case_3(vs, env, e, s) do
    rs =
      for v <- vs do
        env__get(var_name(v), env)
      end

    {e1, s1} = make_let_bindings(rs, e, s)

    opnds =
      for r <- rs do
        r_ref(r, :opnd)
      end

    residualize_operands(opnds, e1, s1)
  end

  defp i_clauses(cs, ctxt, ren, env, s) do
    i_clauses([], cs, ctxt, ren, env, s)
  end

  defp i_clauses(es, cs, ctxt, ren, env, s) do
    {ts, {vs, env0}} =
      mapfoldl(
        fn e, {vs, env} ->
          {t, vs1, env1} = make_template(e, env)
          {t, {vs1 ++ vs, env1}}
        end,
        {[], env},
        es
      )

    vs1 = :lists.reverse(vs)

    {ren1, env1, s1} =
      foldl(
        fn v, {ren, env, s} ->
          e = env__get(var_name(v), env0)
          {opnd, s_1} = make_opnd(e, ren__identity(), env, s)
          {_, ren1, env1, s_2} = bind_locals([v], [opnd], ren, env, s_1)
          {ren1, env1, s_2}
        end,
        {ren, env, s},
        vs1
      )

    {cs1, s2} =
      mapfoldl(
        fn c, s ->
          i_clause_head(c, ts, ren1, env1, s)
        end,
        s1,
        cs
      )

    as =
      for t <- ts do
        hd(get_ann(t))
      end

    case :cerl_clauses.reduce(cs1, ts) do
      {false, cs2} ->
        {cs3, s3} =
          mapfoldl(
            fn c, s ->
              i_clause_body(c, ctxt, s)
            end,
            s2,
            cs2
          )

        {false, {as, vs1, env1, cs3}, s3}

      {true, {c, _}} ->
        {c1, ren2, env2, _} = get_clause_extras(c)
        {b, s3} = i(clause_body(c), ctxt, ren2, env2, s2)
        c2 = update_c_clause(c1, clause_pats(c1), clause_guard(c1), b)
        {true, {as, vs1, env1, [c2]}, s3}
    end
  end

  defp i_clause_head(c, ts, ren, env, s) do
    ps = clause_pats(c)

    bs =
      case :cerl_clauses.match_list(ps, ts) do
        {_, bs1} ->
          bs1

        :none ->
          []
      end

    {_, ren1, env1, s1} = bind_locals(clause_vars(c), ren, env, s)
    s2 = new_passive_size(get_size_limit(s1), s1)

    {ps1, s3} =
      mapfoldl(
        fn p, s ->
          i_pattern(p, ren1, env1, ren, env, s)
        end,
        s2,
        ps
      )

    g = add_match_bindings(bs, clause_guard(c))
    b = add_match_bindings(bs, clause_body(c))
    {g1, s4} = i(g, :value, ren1, env1, s3)

    s5 =
      case is_literal(g1) do
        true ->
          revert_size(s3, s4)

        false ->
          s4
      end

    size = get_size_value(s5)
    c1 = update_c_clause(c, ps1, g1, b)
    {set_clause_extras(c1, ren1, env1, size), revert_size(s, s5)}
  end

  defp add_match_bindings(bs, e) do
    case is_literal(e) do
      true ->
        e

      false ->
        vs =
          for {v, ^e} <- bs, e !== :any do
            v
          end

        es =
          for {_V, ^e} <- bs, e !== :any do
            hd(get_ann(e))
          end

        c_let(vs, c_values(es), e)
    end
  end

  defp i_clause_body(c0, ctxt, s) do
    {c, ren, env, size} = get_clause_extras(c0)
    s1 = count_size(size, s)
    {b, s2} = i(clause_body(c), ctxt, ren, env, s1)
    c1 = update_c_clause(c, clause_pats(c), clause_guard(c), b)
    {c1, s2}
  end

  defp get_clause_extras(c) do
    [{ren, env, size} | as] = get_ann(c)
    {set_ann(c, as), ren, env, size}
  end

  defp set_clause_extras(c, ren, env, size) do
    as = [{ren, env, size} | get_ann(c)]
    set_ann(c, as)
  end

  defp i_fun(e, ctxt, ren, env, s) do
    case ctxt do
      :effect ->
        {void(), count_size(weight(:literal), s)}

      :value ->
        vs = fun_vars(e)
        {_, ren1, env1, s1} = bind_locals(vs, ren, env, s)
        vs1 = i_params(vs, ren1, env1)
        {b, s2} = i(fun_body(e), :value, ren1, env1, s1)
        s3 = count_size(weight(:fun), s2)
        {set_ann(c_fun(vs1, b), kill_id_anns(get_ann(e))), s3}

      r_app() ->
        inline(e, ctxt, ren, env, s)
    end
  end

  defp i_letrec(e, ctxt, ren, env, s) do
    noInline = member(:letrec_goto, get_ann(e))
    {es, b, _, s1} = i_letrec(letrec_defs(e), letrec_body(e), [], ctxt, ren, env, noInline, s)

    case es do
      [] ->
        {b, s1}

      _ ->
        s2 = count_size(weight(:letrec), s1)
        {update_c_letrec(e, es, b), s2}
    end
  end

  defp i_letrec(es, b, xs, ctxt, ren, env, noInline, s) do
    {opnds, s1} =
      mapfoldl(
        fn {_, e}, s ->
          make_opnd(e, :undefined, :undefined, noInline, s)
        end,
        s,
        es
      )

    {rs, ren1, env1, s2} =
      bind_recursive(
        for {f, _} <- es do
          f
        end,
        opnds,
        ren,
        env,
        s1
      )

    {xs1, s3} =
      mapfoldl(
        fn x, s ->
          name = ren__map(var_name(x), ren1)

          case env__lookup(name, env1) do
            {:ok, r} ->
              s_1 = i_letrec_export(r, s)
              {ref_to_var(r), s_1}

            :error ->
              {n, a} = var_name(x)
              report_warning('export `~w\'/~w not defined.\n', [n, a])
              {x, s}
          end
        end,
        s2,
        xs
      )

    {b1, s4} = i(b, ctxt, ren1, env1, s3)
    rs1 = keep_referenced(rs, s4)

    {es1, s5} =
      mapfoldl(
        fn r, s ->
          {e_1, s_1} = visit(r_ref(r, :opnd), s)
          {{ref_to_var(r), e_1}, s_1}
        end,
        s4,
        rs1
      )

    {es1, b1, xs1, s5}
  end

  defp i_letrec_export(r, s) do
    opnd = r_ref(r, :opnd)
    s1 = st__mark_inner_pending(r_opnd(opnd, :loc), s)
    {_, s2} = visit(opnd, s1)

    {_, s3} =
      residualize_var(
        r,
        st__clear_inner_pending(r_opnd(opnd, :loc), s2)
      )

    s3
  end

  defp i_apply(e, ctxt, ren, env, s) do
    {opnds, s1} =
      mapfoldl(
        fn e, s ->
          make_opnd(e, ren, env, s)
        end,
        s,
        apply_args(e)
      )

    {l, s2} = st__new_app_loc(s1)
    ctxt1 = r_app(opnds: opnds, ctxt: ctxt, loc: l)
    {e1, s3} = i(apply_op(e), ctxt1, ren, env, s2)

    case st__get_app_inlined(l, s3) do
      true ->
        residualize_operands(opnds, e1, s3)

      false ->
        {es, s4} =
          mapfoldl(
            fn opnd, s ->
              visit_and_count_size(opnd, s)
            end,
            s3,
            opnds
          )

        arity = length(es)

        e2 =
          case is_c_fname(e1) and length(es) !== fname_arity(e1) do
            true ->
              v = new_var(env)
              ann_c_let(get_ann(e), [v], e1, update_c_apply(e, v, es))

            false ->
              update_c_apply(e, e1, es)
          end

        n = apply_size(arity)
        {e2, count_size(n, s4)}
    end
  end

  defp apply_size(a) do
    weight(:apply) + weight(:argument) * a
  end

  defp i_call(e, ctxt, ren, env, s) do
    {m, s1} = i(call_module(e), :value, ren, env, s)
    {f, s2} = i(call_name(e), :value, ren, env, s1)
    as = call_args(e)
    arity = length(as)
    static = :erlang.and(is_c_atom(m), is_c_atom(f))

    s3 =
      case static do
        true ->
          revert_size(s, s2)

        false ->
          s2
      end

    case ctxt do
      :effect when static === true ->
        case is_safe_call(atom_val(m), atom_val(f), arity) do
          true ->
            i(c_values(as), :effect, ren, env, s3)

          false ->
            i_call_1(static, m, f, arity, as, e, ctxt, ren, env, s3)
        end

      _ ->
        i_call_1(static, m, f, arity, as, e, ctxt, ren, env, s3)
    end
  end

  defp i_call_1(static, m, f, arity, as, e, ctxt, ren, env, s) do
    {as1, s1} =
      mapfoldl(
        fn x, a ->
          i(x, :value, ren, env, a)
        end,
        s,
        as
      )

    case static do
      true ->
        case :erl_bifs.is_pure(atom_val(m), atom_val(f), arity) do
          true ->
            case all_static(as1) do
              true ->
                i_call_3(m, f, as1, e, ctxt, env, s1)

              false ->
                i_call_4(m, f, as1, e, ctxt, env, s1)
            end

          false ->
            i_call_2(m, f, as1, e, s1)
        end

      false ->
        i_call_2(m, f, as1, e, s1)
    end
  end

  defp i_call_2(m, f, as, e, s) do
    n = weight(:call) + weight(:argument) * length(as)
    {update_c_call(e, m, f, as), count_size(n, s)}
  end

  defp i_call_3(m, f, as, e, ctxt, env, s) do
    vs =
      for a <- as do
        concrete(result(a))
      end

    try do
      apply(atom_val(m), atom_val(f), vs)
    catch
      :error, _ ->
        i_call_4(m, f, as, e, ctxt, env, s)
    else
      v ->
        case is_literal_term(v) do
          true ->
            s1 = count_size(weight(:values), s)
            s2 = count_size(weight(:literal), s1)
            {make_seq(c_values(as), abstract(v)), s2}

          false ->
            i_call_4(m, f, as, e, ctxt, env, s)
        end
    end
  end

  defp i_call_4(m, f, as, e, ctxt, env, s) do
    case reduce_bif_call(atom_val(m), atom_val(f), as, env) do
      false ->
        i_call_2(m, f, as, e, s)

      {true, e1} ->
        i(e1, ctxt, ren__identity(), env, s)
    end
  end

  defp i_primop(e, ren, env, s) do
    {as, s1} =
      mapfoldl(
        fn e, s ->
          i(e, :value, ren, env, s)
        end,
        s,
        primop_args(e)
      )

    n = weight(:primop) + weight(:argument) * length(as)
    {update_c_primop(e, primop_name(e), as), count_size(n, s1)}
  end

  defp i_try(e, ctxt, ren, env, s) do
    {a, s1} = i(try_arg(e), :value, ren, env, s)
    vs = try_vars(e)
    {_, ren1, env1, s2} = bind_locals(vs, ren, env, s1)
    vs1 = i_params(vs, ren1, env1)
    {b, s3} = i(try_body(e), ctxt, ren1, env1, s2)

    case is_safe(a) do
      true ->
        i(c_let(vs1, a, b), ctxt, ren__identity(), env, s3)

      false ->
        evs = try_evars(e)
        {_, ren2, env2, s4} = bind_locals(evs, ren, env, s3)
        evs1 = i_params(evs, ren2, env2)
        {h, s5} = i(try_handler(e), ctxt, ren2, env2, s4)
        s6 = count_size(weight(:try), s5)
        {update_c_try(e, a, vs1, b, evs1, h), s6}
    end
  end

  defp i_catch(e, ctxt, ren, env, s) do
    {e1, s1} = eS1 = i(catch_body(e), safe_context(ctxt), ren, env, s)

    case is_safe(e1) do
      true ->
        eS1

      false ->
        s2 = count_size(weight(:catch), s1)
        {update_c_catch(e, e1), s2}
    end
  end

  defp i_receive(e, ctxt, ren, env, s) do
    {t, s1} = i(receive_timeout(e), :value, ren, env, s)
    {b, s2} = i(receive_action(e), ctxt, ren, env, s1)
    ctxt1 = safe_context(ctxt)

    case i_clauses(receive_clauses(e), ctxt1, ren, env, s2) do
      {false, {[], _, _, cs}, s3} ->
        cond do
          cs === [] ->
            case is_c_int(t) and int_val(t) === 0 do
              true ->
                {b, s3}

              false ->
                i_receive_1(e, cs, t, b, s3)
            end

          true ->
            i_receive_1(e, cs, t, b, s3)
        end

      {true, {_, _, _, cs}, s3} ->
        i_receive_1(e, cs, t, b, s3)
    end
  end

  defp i_receive_1(e, cs, t, b, s) do
    n = weight(:receive) + weight(:clause) * length(cs)
    {update_c_receive(e, cs, t, b), count_size(n, s)}
  end

  defp i_module(e, ctxt, ren, env, s) do
    exps = i_module_exports(e)
    {es, _, xs1, s1} = i_letrec(module_defs(e), void(), exps, ctxt, ren, env, false, s)

    case es do
      [] ->
        report_warning('no function definitions remaining in module `~s\'.\n', [
          atom_name(module_name(e))
        ])

      _ ->
        :ok
    end

    e1 = update_c_module(e, module_name(e), xs1, module_attrs(e), es)
    {e1, count_size(weight(:module), s1)}
  end

  defp i_module_exports(e) do
    exps = module_exports(e)
    attrs = module_attrs(e)

    case i_module_on_load(attrs) do
      :none ->
        exps

      [{_, _} = fA] ->
        :ordsets.add_element(c_var(fA), exps)
    end
  end

  defp i_module_on_load([{key, val} | t]) do
    case concrete(key) do
      :on_load ->
        concrete(val)

      _ ->
        i_module_on_load(t)
    end
  end

  defp i_module_on_load([]) do
    :none
  end

  defp i_binary(e, ren, env, s) do
    {es, s1} =
      mapfoldl(
        fn e, s ->
          i_bitstr(e, ren, env, s)
        end,
        s,
        binary_segments(e)
      )

    s2 = count_size(weight(:binary), s1)
    {update_c_binary(e, es), s2}
  end

  defp i_bitstr(e, ren, env, s) do
    {val, s1} = i(bitstr_val(e), :value, ren, env, s)
    {size, s2} = i(bitstr_size(e), :value, ren, env, s1)
    unit = bitstr_unit(e)
    type = bitstr_type(e)
    flags = bitstr_flags(e)
    s3 = count_size(weight(:bitstr), s2)
    {update_c_bitstr(e, val, size, unit, type, flags), s3}
  end

  defp i_map(e, ctx, ren, env, s0) do
    {m1, s1} = i(map_arg(e), :value, ren, env, s0)

    {es, s2} =
      mapfoldl(
        fn e, s ->
          i_map_pair(e, ctx, ren, env, s)
        end,
        s1,
        map_es(e)
      )

    s3 = count_size(weight(:map), s2)
    {update_c_map(e, m1, es), s3}
  end

  defp i_map_pair(e, ctx, ren, env, s0) do
    {key, s1} = i(map_pair_key(e), :value, ren, env, s0)
    {val, s2} = i(map_pair_val(e), ctx, ren, env, s1)
    op = map_pair_op(e)
    s3 = count_size(weight(:map_pair), s2)
    {update_c_map_pair(e, op, key, val), s3}
  end

  defp i_params([v | vs], ren, env) do
    name = ren__map(var_name(v), ren)

    case env__lookup(name, env) do
      {:ok, r} ->
        [ref_to_var(r) | i_params(vs, ren, env)]

      :error ->
        report_internal_error('variable `~w\' not bound in pattern.\n', [name])
        exit(:error)
    end
  end

  defp i_params([], _, _) do
    []
  end

  defp i_pattern(e, ren, env, ren0, env0, s) do
    case type(e) do
      :var ->
        name = ren__map(var_name(e), ren)

        case env__lookup(name, env) do
          {:ok, r} ->
            {ref_to_var(r), s}

          :error ->
            report_internal_error('variable `~w\' not bound in pattern.\n', [name])
            exit(:error)
        end

      :alias ->
        v = alias_var(e)
        name = ren__map(var_name(v), ren)

        case env__lookup(name, env) do
          {:ok, r} ->
            v1 = ref_to_var(r)
            {p, s1} = i_pattern(alias_pat(e), ren, env, ren0, env0, s)
            {update_c_alias(e, v1, p), s1}

          :error ->
            report_internal_error('variable `~w\' not bound in pattern.\n', [name])
            exit(:error)
        end

      :binary ->
        {es, s1} =
          mapfoldl(
            fn e, s ->
              i_bitstr_pattern(e, ren, env, ren0, env0, s)
            end,
            s,
            binary_segments(e)
          )

        s2 = count_size(weight(:binary), s1)
        {update_c_binary(e, es), s2}

      :map ->
        {es, s1} =
          mapfoldl(
            fn e, s ->
              i_map_pair_pattern(e, ren, env, ren0, env0, s)
            end,
            s,
            map_es(e)
          )

        s2 = count_size(weight(:map), s1)
        {update_c_map(e, map_arg(e), es), s2}

      _ ->
        case is_literal(e) do
          true ->
            {e, count_size(weight(:literal), s)}

          false ->
            {es1, s1} =
              mapfoldl(
                fn e, s ->
                  i_pattern(e, ren, env, ren0, env0, s)
                end,
                s,
                data_es(e)
              )

            n = weight(:data) + length(es1) * weight(:element)
            s2 = count_size(n, s1)
            {update_data(e, data_type(e), es1), s2}
        end
    end
  end

  defp i_bitstr_pattern(e, ren, env, ren0, env0, s) do
    {val, s1} = i_pattern(bitstr_val(e), ren, env, ren0, env0, s)
    {size, s2} = i(bitstr_size(e), :value, ren0, env0, s1)
    unit = bitstr_unit(e)
    type = bitstr_type(e)
    flags = bitstr_flags(e)
    s3 = count_size(weight(:bitstr), s2)
    {update_c_bitstr(e, val, size, unit, type, flags), s3}
  end

  defp i_map_pair_pattern(e, ren, env, ren0, env0, s) do
    {key, s1} = i(map_pair_key(e), :value, ren0, env0, s)
    {val, s2} = i_pattern(map_pair_val(e), ren, env, ren0, env0, s1)
    op = map_pair_op(e)
    s3 = count_size(weight(:map_pair), s2)
    {update_c_map_pair(e, op, key, val), s3}
  end

  defp inline(e, r_app(opnds: opnds, ctxt: ctxt, loc: l), ren, env, s) do
    vs = fun_vars(e)

    cond do
      length(opnds) !== length(vs) ->
        {e, s}

      true ->
        {rs, ren1, env1, s1} = bind_locals(vs, opnds, ren, env, s)
        {e1, s2} = i(fun_body(e), ctxt, ren1, env1, s1)
        {e2, s3} = make_let_bindings(rs, e1, s2)
        {e2, st__set_app_inlined(l, s3)}
    end
  end

  defp make_let_bindings([r | rs], e, s) do
    {e1, s1} = make_let_bindings(rs, e, s)
    make_let_binding(r, e1, s1)
  end

  defp make_let_bindings([], e, s) do
    {e, s}
  end

  defp make_let_binding(r, e, s) do
    case is_literal(e) do
      true ->
        make_let_binding_1(r, e, s)

      false ->
        case is_c_var(e) do
          true ->
            case var_name(e) === r_ref(r, :name) do
              true ->
                visit_and_count_size(r_ref(r, :opnd), s)

              false ->
                make_let_binding_1(r, e, s)
            end

          false ->
            case st__get_var_referenced(r_ref(r, :loc), s) do
              true ->
                {e1, s1} = visit_and_count_size(r_ref(r, :opnd), s)
                s2 = count_size(weight(:let), s1)
                {c_let([ref_to_var(r)], e1, e), s2}

              false ->
                make_let_binding_1(r, e, s)
            end
        end
    end
  end

  defp make_let_binding_1(r, e, s) do
    opnd = r_ref(r, :opnd)
    {e, st__set_opnd_effect(r_opnd(opnd, :loc), s)}
  end

  defp copy(r, opnd, e, ctxt, env, s) do
    case is_c_var(e) and not is_c_fname(e) do
      true ->
        copy_var(env__get(var_name(e), r_opnd(opnd, :env)), ctxt, env, s)

      false ->
        case is_literal(e) do
          true ->
            {e, count_size(weight(:literal), s)}

          false ->
            copy_1(r, opnd, e, ctxt, env, s)
        end
    end
  end

  defp copy_var(r, ctxt, env, s) do
    case r_ref(r, :opnd) do
      :undefined ->
        residualize_var(r, s)

      opnd ->
        case st__lookup_opnd_cache(r_opnd(opnd, :loc), s) do
          :error ->
            residualize_var(r, s)

          {:ok, r_cache(expr: e1)} ->
            copy_1(r, opnd, e1, ctxt, env, s)
        end
    end
  end

  defp copy_1(r, r_opnd(no_inline: true), _E, _Ctxt, _Env, s) do
    residualize_var(r, s)
  end

  defp copy_1(r, opnd, e, ctxt, env, s) do
    case type(e) do
      :fun ->
        case ctxt do
          r_app() ->
            case st__test_outer_pending(r_opnd(opnd, :loc), s) do
              false ->
                copy_inline(r, opnd, e, ctxt, env, s)

              true ->
                residualize_var(r, s)
            end

          _ ->
            residualize_var(r, s)
        end

      :var ->
        case ctxt do
          r_app() ->
            case st__test_outer_pending(r_opnd(opnd, :loc), s) do
              false ->
                r1 = env__get(var_name(e), r_opnd(opnd, :env))
                copy_var(r1, ctxt, env, s)

              true ->
                residualize_var(r, s)
            end

          _ ->
            residualize_var(r, s)
        end

      _ ->
        residualize_var(r, s)
    end
  end

  defp copy_inline(r, opnd, e, ctxt, env, s) do
    s1 = st__mark_outer_pending(r_opnd(opnd, :loc), s)

    try do
      copy_inline_1(r, e, ctxt, env, s1)
    catch
      x ->
        _S2 = st__clear_outer_pending(r_opnd(opnd, :loc), s1)
        throw(x)
    else
      {e1, s2} ->
        {e1, st__clear_outer_pending(r_opnd(opnd, :loc), s2)}
    end
  end

  defp copy_inline_1(r, e, ctxt, env, s) do
    case effort_is_active(s) do
      true ->
        copy_inline_2(r, e, ctxt, env, s)

      false ->
        s1 = new_active_effort(get_effort_limit(s), s)

        try do
          copy_inline_2(r, e, ctxt, env, s1)
        catch
          {:counter_exceeded, :effort, _} ->
            residualize_var(r, s)
        else
          {e1, s2} ->
            {e1, revert_effort(s, s2)}
        end
    end
  end

  defp copy_inline_2(r, e, ctxt, env, s) do
    limit =
      case size_is_active(s) do
        true ->
          get_size_limit(s) - get_size_value(s)

        false ->
          get_size_limit(s)
      end

    s1 =
      new_active_size(
        limit + apply_size(length(r_app(ctxt, :opnds))),
        s
      )

    try do
      inline(e, ctxt, ren__identity(), env, s1)
    catch
      {:counter_exceeded, :size, s2} ->
        s3 = revert_size(s, s2)
        s4 = reset_nested_apps(ctxt, s3)
        residualize_var(r, s4)
    else
      {e1, s2} ->
        {e1, revert_size(s, s2)}
    end
  end

  defp reset_nested_apps(r_app(ctxt: ctxt, loc: l), s) do
    reset_nested_apps(ctxt, st__clear_app_inlined(l, s))
  end

  defp reset_nested_apps(_, s) do
    s
  end

  defp new_var(env) do
    name = env__new_vname(env)
    c_var(name)
  end

  defp new_template_var(env) do
    name = env__new_tname(env)
    c_var(name)
  end

  defp residualize_var(r, s) do
    s1 = count_size(weight(:var), s)
    {ref_to_var(r), st__set_var_referenced(r_ref(r, :loc), s1)}
  end

  defp result(e) do
    case is_c_seq(e) do
      true ->
        seq_body(e)

      false ->
        e
    end
  end

  defp hoist_effects(a, e) do
    case type(a) do
      :seq ->
        make_seq(seq_arg(a), e)

      _ ->
        e
    end
  end

  defp make_seq(e1, e2) do
    case is_safe(e1) do
      true ->
        e2

      false ->
        e3 =
          case is_c_seq(e1) do
            true ->
              case is_safe(seq_body(e1)) do
                true ->
                  seq_arg(e1)

                false ->
                  e1
              end

            false ->
              e1
          end

        case is_c_seq(e2) do
          true ->
            c_seq(c_seq(e3, seq_arg(e2)), seq_body(e2))

          false ->
            c_seq(e3, e2)
        end
    end
  end

  defp is_safe(e) do
    case is_data(e) do
      true ->
        is_safe_list(data_es(e))

      false ->
        case type(e) do
          :var ->
            true

          :fun ->
            true

          :values ->
            is_safe_list(values_es(e))

          :seq ->
            is_safe(seq_arg(e)) and is_safe(seq_body(e))

          :let ->
            is_safe(let_arg(e)) and is_safe(let_body(e))

          :letrec ->
            is_safe(letrec_body(e))

          :try ->
            is_safe(try_arg(e)) and is_safe(try_body(e))

          :catch ->
            is_safe(catch_body(e))

          :call ->
            m = call_module(e)
            f = call_name(e)

            case is_c_atom(m) and is_c_atom(f) do
              true ->
                as = call_args(e)
                is_safe_list(as) and is_safe_call(atom_val(m), atom_val(f), length(as))

              false ->
                false
            end

          _ ->
            false
        end
    end
  end

  defp is_safe_list([e | es]) do
    case is_safe(e) do
      true ->
        is_safe_list(es)

      false ->
        false
    end
  end

  defp is_safe_list([]) do
    true
  end

  defp is_safe_call(m, f, a) do
    :erl_bifs.is_safe(m, f, a)
  end

  defp make_locals(vs, ren, env) do
    make_locals(vs, [], ren, env)
  end

  defp make_locals([v | vs], as, ren, env) do
    name = var_name(v)

    case env__is_defined(name, env) do
      false ->
        name1 = name
        ren1 = ren__add_identity(name, ren)

      true ->
        name1 =
          case name do
            {a, n} ->
              env__new_fname(a, n, env)

            _ ->
              env__new_vname(env)
          end

        ren1 = ren__add(name, name1, ren)
    end

    env1 = env__bind(name1, :dummy, env)
    make_locals(vs, [name1 | as], ren1, env1)
  end

  defp make_locals([], as, ren, env) do
    {reverse(as), ren, env}
  end

  defp bind_locals(vs, ren, env, s) do
    opnds =
      for _ <- vs do
        :undefined
      end

    bind_locals(vs, opnds, ren, env, s)
  end

  defp bind_locals(vs, opnds, ren, env, s) do
    {ns, ren1, env1} = make_locals(vs, ren, env)
    {rs, env2, s1} = bind_locals_1(ns, opnds, [], env1, s)
    {rs, ren1, env2, s1}
  end

  defp bind_locals_1([n | ns], [opnd | opnds], rs, env, s) do
    {r, s1} = new_ref(n, opnd, s)
    env1 = env__bind(n, r, env)
    bind_locals_1(ns, opnds, [r | rs], env1, s1)
  end

  defp bind_locals_1([], [], rs, env, s) do
    {:lists.reverse(rs), env, s}
  end

  defp new_refs(ns, opnds, s) do
    new_refs(ns, opnds, [], s)
  end

  defp new_refs([n | ns], [opnd | opnds], rs, s) do
    {r, s1} = new_ref(n, opnd, s)
    new_refs(ns, opnds, [r | rs], s1)
  end

  defp new_refs([], [], rs, s) do
    {:lists.reverse(rs), s}
  end

  defp new_ref(n, opnd, s) do
    {l, s1} = st__new_ref_loc(s)
    {r_ref(name: n, opnd: opnd, loc: l), s1}
  end

  defp bind_recursive(vs, opnds, ren, env, s) do
    {ns, ren1, env1} = make_locals(vs, ren, env)
    {rs, s1} = new_refs(ns, opnds, s)

    fun = fn r, env ->
      opnd = r_ref(r, :opnd)
      r_ref(r, opnd: r_opnd(opnd, ren: ren1, env: env))
    end

    {rs, ren1, env__bind_recursive(ns, rs, fun, env1), s1}
  end

  defp safe_context(ctxt) do
    case ctxt do
      r_app() ->
        :value

      _ ->
        ctxt
    end
  end

  defp ref_to_var(r_ref(name: name)) do
    c_var(name)
  end

  defp make_opnd(e, ren, env, s) do
    make_opnd(e, ren, env, false, s)
  end

  defp make_opnd(e, ren, env, noInline, s) do
    {l, s1} = st__new_opnd_loc(s)
    c = st__get_effort(s1)
    opnd = r_opnd(expr: e, ren: ren, env: env, loc: l, effort: c, no_inline: noInline)
    {opnd, s1}
  end

  defp keep_referenced(rs, s) do
    for r <- rs, st__get_var_referenced(r_ref(r, :loc), s) do
      r
    end
  end

  defp residualize_operands(opnds, e, s) do
    foldr(
      fn opnd, {e, s} ->
        residualize_operand(opnd, e, s)
      end,
      {e, s},
      opnds
    )
  end

  defp residualize_operand(opnd, e, s) do
    case st__get_opnd_effect(r_opnd(opnd, :loc), s) do
      true ->
        {e1, s1} = i(r_opnd(opnd, :expr), :effect, r_opnd(opnd, :ren), r_opnd(opnd, :env), s)
        {make_seq(e1, e), s1}

      false ->
        {e, s}
    end
  end

  defp visit(opnd, s) do
    {c, s1} = visit_1(opnd, s)
    {r_cache(c, :expr), s1}
  end

  defp visit_and_count_size(opnd, s) do
    {c, s1} = visit_1(opnd, s)
    {r_cache(c, :expr), count_size(r_cache(c, :size), s1)}
  end

  defp visit_1(opnd, s) do
    case st__lookup_opnd_cache(r_opnd(opnd, :loc), s) do
      :error ->
        effort = r_opnd(opnd, :effort)
        active = counter__is_active(effort)

        s1 =
          case active do
            true ->
              s

            false ->
              st__set_effort(effort, s)
          end

        s2 = new_passive_size(get_size_limit(s1), s1)
        {e, s3} = i(r_opnd(opnd, :expr), :value, r_opnd(opnd, :ren), r_opnd(opnd, :env), s2)
        size = get_size_value(s3)
        c = r_cache(expr: e, size: size)

        s4 =
          revert_size(
            s,
            st__set_opnd_cache(r_opnd(opnd, :loc), c, s3)
          )

        case active do
          true ->
            {c, s4}

          false ->
            {c, revert_effort(s, s4)}
        end

      {:ok, c} ->
        {c, s}
    end
  end

  defp make_template(e, env) do
    make_template(e, [], env)
  end

  defp make_template(e, vs0, env0) do
    case is_data(e) do
      true ->
        {ts, {vs1, env1}} =
          mapfoldl(
            fn e, {vs0, env0} ->
              {t, vs1, env1} = make_template(e, vs0, env0)
              {t, {vs1, env1}}
            end,
            {vs0, env0},
            data_es(e)
          )

        t = make_data_skel(data_type(e), ts)

        e1 =
          update_data(
            e,
            data_type(e),
            for ^t <- ts do
              hd(get_ann(t))
            end
          )

        v = new_template_var(env1)
        env2 = env__bind(var_name(v), e1, env1)
        {set_ann(t, [v]), [v | vs1], env2}

      false ->
        case type(e) do
          :seq ->
            {t, vs1, env1} = make_template(seq_body(e), vs0, env0)
            v = var_name(hd(get_ann(t)))
            e1 = update_c_seq(e, seq_arg(e), env__get(v, env1))
            env2 = env__bind(v, e1, env1)
            {t, vs1, env2}

          _ ->
            v = new_template_var(env0)
            env1 = env__bind(var_name(v), e, env0)
            {set_ann(v, [v]), [v | vs0], env1}
        end
    end
  end

  defp equivalent_clauses([]) do
    true
  end

  defp equivalent_clauses([c | cs]) do
    env = :cerl_trees.variables(c_values(clause_pats(c)))
    equivalent_clauses_1(clause_body(c), cs, env)
  end

  defp equivalent_clauses_1(e, [c | cs], env) do
    env1 = :cerl_trees.variables(c_values(clause_pats(c)))

    case equivalent(e, clause_body(c), :ordsets.union(env, env1)) do
      true ->
        equivalent_clauses_1(e, cs, env)

      false ->
        false
    end
  end

  defp equivalent_clauses_1(_, [], _Env) do
    true
  end

  defp equivalent(e1, e2, env) do
    case is_data(e1) do
      true ->
        case is_data(e2) do
          true ->
            t1 = {data_type(e1), data_arity(e1)}
            t2 = {data_type(e2), data_arity(e2)}
            t1 === t2 and equivalent_lists(data_es(e1), data_es(e2), env)

          false ->
            false
        end

      false ->
        case type(e1) do
          :var ->
            case is_c_var(e2) do
              true ->
                n1 = var_name(e1)
                n2 = var_name(e2)
                n1 === n2 and not :ordsets.is_element(n1, env)

              false ->
                false
            end

          _ ->
            false
        end
    end
  end

  defp equivalent_lists([e1 | es1], [e2 | es2], env) do
    :erlang.and(
      equivalent(e1, e2, env),
      equivalent_lists(es1, es2, env)
    )
  end

  defp equivalent_lists([], [], _) do
    true
  end

  defp equivalent_lists(_, _, _) do
    false
  end

  defp reduce_bif_call(m, f, as, env) do
    reduce_bif_call_1(m, f, length(as), as, env)
  end

  defp reduce_bif_call_1(:erlang, :element, 2, [x, y], _Env) do
    case :erlang.and(is_c_int(x), is_c_tuple(y)) do
      true ->
        t = :erlang.list_to_tuple(tuple_es(y))
        n = int_val(x)

        cond do
          is_integer(n) and n > 0 and n <= tuple_size(t) ->
            e = :erlang.element(n, t)
            es = :erlang.tuple_to_list(:erlang.setelement(n, t, void()))
            {true, make_seq(c_tuple(es), e)}

          true ->
            false
        end

      false ->
        false
    end
  end

  defp reduce_bif_call_1(:erlang, :hd, 1, [x], _Env) do
    case is_c_cons(x) do
      true ->
        {true, make_seq(cons_tl(x), cons_hd(x))}

      false ->
        false
    end
  end

  defp reduce_bif_call_1(:erlang, :length, 1, [x], _Env) do
    case is_c_list(x) do
      true ->
        {true, make_seq(x, c_int(list_length(x)))}

      false ->
        false
    end
  end

  defp reduce_bif_call_1(:erlang, :list_to_tuple, 1, [x], _Env) do
    case is_c_list(x) do
      true ->
        {true, c_tuple(list_elements(x))}

      false ->
        false
    end
  end

  defp reduce_bif_call_1(:erlang, :setelement, 3, [x, y, z], env) do
    case :erlang.and(is_c_int(x), is_c_tuple(y)) do
      true ->
        t = :erlang.list_to_tuple(tuple_es(y))
        n = int_val(x)

        cond do
          is_integer(n) and n > 0 and n <= tuple_size(t) ->
            e = :erlang.element(n, t)

            case is_simple(z) do
              true ->
                es = :erlang.tuple_to_list(:erlang.setelement(n, t, z))
                {true, make_seq(e, c_tuple(es))}

              false ->
                v = new_var(env)
                es = :erlang.tuple_to_list(:erlang.setelement(n, t, v))
                e1 = make_seq(e, c_tuple(es))
                {true, c_let([v], z, e1)}
            end

          true ->
            false
        end

      false ->
        false
    end
  end

  defp reduce_bif_call_1(:erlang, :size, 1, [x], env) do
    case is_c_tuple(x) do
      true ->
        reduce_bif_call_1(:erlang, :tuple_size, 1, [x], env)

      false ->
        false
    end
  end

  defp reduce_bif_call_1(:erlang, :tl, 1, [x], _Env) do
    case is_c_cons(x) do
      true ->
        {true, make_seq(cons_hd(x), cons_tl(x))}

      false ->
        false
    end
  end

  defp reduce_bif_call_1(:erlang, :tuple_size, 1, [x], _Env) do
    case is_c_tuple(x) do
      true ->
        {true, make_seq(x, c_int(tuple_arity(x)))}

      false ->
        false
    end
  end

  defp reduce_bif_call_1(:erlang, :tuple_to_list, 1, [x], _Env) do
    case is_c_tuple(x) do
      true ->
        {true, make_list(tuple_es(x))}

      false ->
        false
    end
  end

  defp reduce_bif_call_1(_M, _F, _A, _As, _Env) do
    false
  end

  defp effort_is_active(s) do
    counter__is_active(st__get_effort(s))
  end

  defp size_is_active(s) do
    counter__is_active(st__get_size(s))
  end

  defp get_effort_limit(s) do
    counter__limit(st__get_effort(s))
  end

  defp new_active_effort(limit, s) do
    st__set_effort(counter__new_active(limit), s)
  end

  defp revert_effort(s1, s2) do
    st__set_effort(st__get_effort(s1), s2)
  end

  defp new_active_size(limit, s) do
    st__set_size(counter__new_active(limit), s)
  end

  defp new_passive_size(limit, s) do
    st__set_size(counter__new_passive(limit), s)
  end

  defp revert_size(s1, s2) do
    st__set_size(st__get_size(s1), s2)
  end

  defp count_effort(n, s) do
    c = st__get_effort(s)
    c1 = counter__add(n, c, :effort, s)

    case debug_counters() do
      false ->
        :ok
    end

    st__set_effort(c1, s)
  end

  defp count_size(n, s) do
    c = st__get_size(s)
    c1 = counter__add(n, c, :size, s)

    case debug_counters() do
      false ->
        :ok
    end

    st__set_size(c1, s)
  end

  defp get_size_value(s) do
    counter__value(st__get_size(s))
  end

  defp get_size_limit(s) do
    counter__limit(st__get_size(s))
  end

  defp kill_id_anns([{:id, _} | as]) do
    kill_id_anns(as)
  end

  defp kill_id_anns([a | as]) do
    [a | kill_id_anns(as)]
  end

  defp kill_id_anns([]) do
    []
  end

  defp void() do
    abstract(:ok)
  end

  defp is_simple(e) do
    case type(e) do
      :literal ->
        true

      :var ->
        true

      :fun ->
        true

      _ ->
        false
    end
  end

  defp get_components(n, e) do
    case type(e) do
      :values ->
        es = values_es(e)

        cond do
          length(es) === n ->
            {true, es}

          true ->
            false
        end

      _ when n === 1 ->
        {true, [e]}

      _ ->
        false
    end
  end

  defp all_static(es) do
    :lists.all(
      fn e ->
        is_literal(result(e))
      end,
      es
    )
  end

  defp set_clause_bodies([c | cs], b) do
    [
      update_c_clause(c, clause_pats(c), clause_guard(c), b)
      | set_clause_bodies(cs, b)
    ]
  end

  defp set_clause_bodies([], _) do
    []
  end

  defp ren__identity() do
    %{}
  end

  defp ren__add(x, y, ren) do
    %{ren | x => y}
  end

  defp ren__map(x, ren) do
    case ren do
      %{^x => y} ->
        y

      %{} ->
        x
    end
  end

  defp ren__add_identity(x, ren) do
    :maps.remove(x, ren)
  end

  defp env__empty() do
    :rec_env.empty()
  end

  defp env__bind(key, val, env) do
    :rec_env.bind(key, val, env)
  end

  defp env__bind_recursive(ks, vs, f, env) do
    :rec_env.bind_recursive(ks, vs, f, env)
  end

  defp env__lookup(key, env) do
    :rec_env.lookup(key, env)
  end

  defp env__get(key, env) do
    :rec_env.get(key, env)
  end

  defp env__is_defined(key, env) do
    :rec_env.is_defined(key, env)
  end

  defp env__new_vname(env) do
    :rec_env.new_key(env)
  end

  defp env__new_tname(env) do
    :rec_env.new_key(
      fn i ->
        :erlang.list_to_atom('@i' ++ :erlang.integer_to_list(i))
      end,
      env
    )
  end

  defp env__new_fname(a, n, env) do
    :rec_env.new_key(
      fn x ->
        s = :erlang.integer_to_list(x)
        {:erlang.list_to_atom(:erlang.atom_to_list(a) ++ '_' ++ s), n}
      end,
      env
    )
  end

  Record.defrecord(:r_state, :state,
    free: :undefined,
    size: :undefined,
    effort: :undefined,
    unroll: :undefined,
    cache: :undefined,
    var_flags: :undefined,
    opnd_flags: :undefined,
    app_flags: :undefined
  )

  Record.defrecord(:r_var_flags, :var_flags,
    lab: :undefined,
    referenced: false
  )

  Record.defrecord(:r_opnd_flags, :opnd_flags,
    lab: :undefined,
    inner_pending: 1,
    outer_pending: 1,
    effect: false
  )

  Record.defrecord(:r_app_flags, :app_flags,
    lab: :undefined,
    inlined: false
  )

  defp st__new(effort, size, unroll) do
    etsOpts = [:set, :private, {:keypos, 2}]

    r_state(
      free: 0,
      size: counter__new_passive(size),
      effort: counter__new_passive(effort),
      unroll: unroll,
      cache: :maps.new(),
      var_flags: :ets.new(:var, etsOpts),
      opnd_flags: :ets.new(:opnd, etsOpts),
      app_flags: :ets.new(:app, etsOpts)
    )
  end

  defp st__new_loc(s) do
    n = r_state(s, :free)
    {n, r_state(s, free: n + 1)}
  end

  defp st__get_effort(s) do
    r_state(s, :effort)
  end

  defp st__set_effort(c, s) do
    r_state(s, effort: c)
  end

  defp st__get_size(s) do
    r_state(s, :size)
  end

  defp st__set_size(c, s) do
    r_state(s, size: c)
  end

  defp st__set_var_referenced(l, s) do
    t = r_state(s, :var_flags)
    [f] = :ets.lookup(t, l)
    :ets.insert(t, r_var_flags(f, referenced: true))
    s
  end

  defp st__get_var_referenced(l, s) do
    :ets.lookup_element(r_state(s, :var_flags), l, r_var_flags(:referenced))
  end

  defp st__lookup_opnd_cache(l, s) do
    :maps.find(l, r_state(s, :cache))
  end

  defp st__set_opnd_cache(l, c, s) do
    r_state(s, cache: :maps.put(l, c, r_state(s, :cache)))
  end

  defp st__set_opnd_effect(l, s) do
    t = r_state(s, :opnd_flags)
    [f] = :ets.lookup(t, l)
    :ets.insert(t, r_opnd_flags(f, effect: true))
    s
  end

  defp st__get_opnd_effect(l, s) do
    :ets.lookup_element(r_state(s, :opnd_flags), l, r_opnd_flags(:effect))
  end

  defp st__set_app_inlined(l, s) do
    t = r_state(s, :app_flags)
    [f] = :ets.lookup(t, l)
    :ets.insert(t, r_app_flags(f, inlined: true))
    s
  end

  defp st__clear_app_inlined(l, s) do
    t = r_state(s, :app_flags)
    [f] = :ets.lookup(t, l)
    :ets.insert(t, r_app_flags(f, inlined: false))
    s
  end

  defp st__get_app_inlined(l, s) do
    :ets.lookup_element(r_state(s, :app_flags), l, r_app_flags(:inlined))
  end

  defp st__test_inner_pending(l, s) do
    t = r_state(s, :opnd_flags)
    p = :ets.lookup_element(t, l, r_opnd_flags(:inner_pending))
    p <= 0
  end

  defp st__mark_inner_pending(l, s) do
    :ets.update_counter(r_state(s, :opnd_flags), l, {r_opnd_flags(:inner_pending), -1})
    s
  end

  defp st__clear_inner_pending(l, s) do
    :ets.update_counter(r_state(s, :opnd_flags), l, {r_opnd_flags(:inner_pending), 1})
    s
  end

  defp st__test_outer_pending(l, s) do
    t = r_state(s, :opnd_flags)
    p = :ets.lookup_element(t, l, r_opnd_flags(:outer_pending))
    p <= 0
  end

  defp st__mark_outer_pending(l, s) do
    :ets.update_counter(r_state(s, :opnd_flags), l, {r_opnd_flags(:outer_pending), -1})
    s
  end

  defp st__clear_outer_pending(l, s) do
    :ets.update_counter(r_state(s, :opnd_flags), l, {r_opnd_flags(:outer_pending), 1})
    s
  end

  defp st__new_app_loc(s) do
    v = {l, _S1} = st__new_loc(s)
    :ets.insert(r_state(s, :app_flags), r_app_flags(lab: l))
    v
  end

  defp st__new_ref_loc(s) do
    v = {l, _S1} = st__new_loc(s)
    :ets.insert(r_state(s, :var_flags), r_var_flags(lab: l))
    v
  end

  defp st__new_opnd_loc(s) do
    v = {l, _S1} = st__new_loc(s)
    n = r_state(s, :unroll)

    :ets.insert(
      r_state(s, :opnd_flags),
      r_opnd_flags(lab: l, inner_pending: n, outer_pending: n)
    )

    v
  end

  defp counter__new_passive(limit) when limit > 0 do
    {0, limit}
  end

  defp counter__new_active(limit) when limit > 0 do
    {limit, limit}
  end

  defp counter__is_active({c, _}) do
    c > 0
  end

  defp counter__limit({_, l}) do
    l
  end

  defp counter__value({n, l}) do
    cond do
      n > 0 ->
        l - n

      true ->
        -n
    end
  end

  defp counter__add(n, {v, l}, type, data) do
    n1 = v - n

    cond do
      v > 0 and n1 <= 0 ->
        case debug_counters() do
          false ->
            :ok
        end

        throw({:counter_exceeded, type, data})

      true ->
        {n1, l}
    end
  end

  defp report_internal_error(s, vs) do
    report_error('internal error: ' ++ s, vs)
  end

  defp report_error(d, vs) do
    report({:error, d}, vs)
  end

  defp report_warning(d) do
    report_warning(d, [])
  end

  defp report_warning(d, vs) do
    report({:warning, d}, vs)
  end

  defp report(d, vs) do
    :io.put_chars(format(d, vs))
  end

  defp format({:error, d}, vs) do
    ['error: ', format(d, vs)]
  end

  defp format({:warning, d}, vs) do
    ['warning: ', format(d, vs)]
  end

  defp format(s, vs) when is_list(s) do
    [:io_lib.fwrite(s, vs), ?\n]
  end
end
