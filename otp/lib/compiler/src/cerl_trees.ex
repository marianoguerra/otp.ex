defmodule :m_cerl_trees do
  use Bitwise

  import :cerl,
    only: [
      alias_pat: 1,
      alias_var: 1,
      ann_c_alias: 3,
      ann_c_apply: 3,
      ann_c_binary: 2,
      ann_c_bitstr: 6,
      ann_c_call: 4,
      ann_c_case: 3,
      ann_c_catch: 2,
      ann_c_clause: 4,
      ann_c_cons_skel: 3,
      ann_c_fun: 3,
      ann_c_let: 4,
      ann_c_letrec: 3,
      ann_c_map: 3,
      ann_c_map_pair: 4,
      ann_c_map_pattern: 2,
      ann_c_module: 5,
      ann_c_primop: 3,
      ann_c_receive: 4,
      ann_c_seq: 3,
      ann_c_try: 6,
      ann_c_tuple_skel: 2,
      ann_c_values: 2,
      apply_args: 1,
      apply_op: 1,
      binary_segments: 1,
      bitstr_flags: 1,
      bitstr_size: 1,
      bitstr_type: 1,
      bitstr_unit: 1,
      bitstr_val: 1,
      call_args: 1,
      call_module: 1,
      call_name: 1,
      case_arg: 1,
      case_clauses: 1,
      catch_body: 1,
      clause_body: 1,
      clause_guard: 1,
      clause_pats: 1,
      clause_vars: 1,
      concrete: 1,
      cons_hd: 1,
      cons_tl: 1,
      fun_body: 1,
      fun_vars: 1,
      get_ann: 1,
      is_c_map_pattern: 1,
      let_arg: 1,
      let_body: 1,
      let_vars: 1,
      letrec_body: 1,
      letrec_defs: 1,
      letrec_vars: 1,
      map_arg: 1,
      map_es: 1,
      map_pair_key: 1,
      map_pair_op: 1,
      map_pair_val: 1,
      module_attrs: 1,
      module_defs: 1,
      module_exports: 1,
      module_name: 1,
      module_vars: 1,
      primop_args: 1,
      primop_name: 1,
      receive_action: 1,
      receive_clauses: 1,
      receive_timeout: 1,
      seq_arg: 1,
      seq_body: 1,
      set_ann: 2,
      subtrees: 1,
      try_arg: 1,
      try_body: 1,
      try_evars: 1,
      try_handler: 1,
      try_vars: 1,
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
      update_c_cons: 3,
      update_c_cons_skel: 3,
      update_c_fun: 3,
      update_c_let: 4,
      update_c_letrec: 3,
      update_c_map: 3,
      update_c_map_pair: 4,
      update_c_module: 5,
      update_c_primop: 3,
      update_c_receive: 4,
      update_c_seq: 3,
      update_c_try: 6,
      update_c_tuple: 2,
      update_c_tuple_skel: 2,
      update_c_values: 2,
      values_es: 1,
      var_name: 1
    ]

  def depth(t) do
    case subtrees(t) do
      [] ->
        0

      gs ->
        1 +
          :lists.foldl(
            fn g, a ->
              :erlang.max(depth_1(g), a)
            end,
            0,
            gs
          )
    end
  end

  defp depth_1(ts) do
    :lists.foldl(
      fn t, a ->
        :erlang.max(depth(t), a)
      end,
      0,
      ts
    )
  end

  def size(t) do
    fold(
      fn _, s ->
        s + 1
      end,
      0,
      t
    )
  end

  def map(f, t) do
    f.(map_1(f, t))
  end

  defp map_1(f, t) do
    case type(t) do
      :literal ->
        case concrete(t) do
          [_ | _] ->
            update_c_cons(t, map(f, cons_hd(t)), map(f, cons_tl(t)))

          v when tuple_size(v) > 0 ->
            update_c_tuple(t, map_list(f, tuple_es(t)))

          _ ->
            t
        end

      :var ->
        t

      :values ->
        update_c_values(t, map_list(f, values_es(t)))

      :cons ->
        update_c_cons_skel(t, map(f, cons_hd(t)), map(f, cons_tl(t)))

      :tuple ->
        update_c_tuple_skel(t, map_list(f, tuple_es(t)))

      :map ->
        update_c_map(t, map(f, map_arg(t)), map_list(f, map_es(t)))

      :map_pair ->
        update_c_map_pair(
          t,
          map(f, map_pair_op(t)),
          map(f, map_pair_key(t)),
          map(f, map_pair_val(t))
        )

      :let ->
        update_c_let(t, map_list(f, let_vars(t)), map(f, let_arg(t)), map(f, let_body(t)))

      :seq ->
        update_c_seq(t, map(f, seq_arg(t)), map(f, seq_body(t)))

      :apply ->
        update_c_apply(t, map(f, apply_op(t)), map_list(f, apply_args(t)))

      :call ->
        update_c_call(t, map(f, call_module(t)), map(f, call_name(t)), map_list(f, call_args(t)))

      :primop ->
        update_c_primop(t, map(f, primop_name(t)), map_list(f, primop_args(t)))

      :case ->
        update_c_case(t, map(f, case_arg(t)), map_list(f, case_clauses(t)))

      :clause ->
        update_c_clause(
          t,
          map_list(f, clause_pats(t)),
          map(f, clause_guard(t)),
          map(f, clause_body(t))
        )

      :alias ->
        update_c_alias(t, map(f, alias_var(t)), map(f, alias_pat(t)))

      :fun ->
        update_c_fun(t, map_list(f, fun_vars(t)), map(f, fun_body(t)))

      :receive ->
        update_c_receive(
          t,
          map_list(f, receive_clauses(t)),
          map(f, receive_timeout(t)),
          map(f, receive_action(t))
        )

      :try ->
        update_c_try(
          t,
          map(f, try_arg(t)),
          map_list(f, try_vars(t)),
          map(f, try_body(t)),
          map_list(f, try_evars(t)),
          map(f, try_handler(t))
        )

      :catch ->
        update_c_catch(t, map(f, catch_body(t)))

      :binary ->
        update_c_binary(t, map_list(f, binary_segments(t)))

      :bitstr ->
        update_c_bitstr(
          t,
          map(f, bitstr_val(t)),
          map(f, bitstr_size(t)),
          map(f, bitstr_unit(t)),
          map(f, bitstr_type(t)),
          map(f, bitstr_flags(t))
        )

      :letrec ->
        update_c_letrec(t, map_pairs(f, letrec_defs(t)), map(f, letrec_body(t)))

      :module ->
        update_c_module(
          t,
          map(f, module_name(t)),
          map_list(f, module_exports(t)),
          map_pairs(f, module_attrs(t)),
          map_pairs(f, module_defs(t))
        )
    end
  end

  defp map_list(f, [t | ts]) do
    [map(f, t) | map_list(f, ts)]
  end

  defp map_list(_, []) do
    []
  end

  defp map_pairs(f, [{t1, t2} | ps]) do
    [{map(f, t1), map(f, t2)} | map_pairs(f, ps)]
  end

  defp map_pairs(_, []) do
    []
  end

  def fold(f, s, t) do
    f.(t, fold_1(f, s, t))
  end

  defp fold_1(f, s, t) do
    case type(t) do
      :literal ->
        case concrete(t) do
          [_ | _] ->
            fold(f, fold(f, s, cons_hd(t)), cons_tl(t))

          v when tuple_size(v) > 0 ->
            fold_list(f, s, tuple_es(t))

          _ ->
            s
        end

      :var ->
        s

      :values ->
        fold_list(f, s, values_es(t))

      :cons ->
        fold(f, fold(f, s, cons_hd(t)), cons_tl(t))

      :tuple ->
        fold_list(f, s, tuple_es(t))

      :map ->
        fold_list(f, s, map_es(t))

      :map_pair ->
        fold(
          f,
          fold(f, fold(f, s, map_pair_op(t)), map_pair_key(t)),
          map_pair_val(t)
        )

      :let ->
        fold(
          f,
          fold(f, fold_list(f, s, let_vars(t)), let_arg(t)),
          let_body(t)
        )

      :seq ->
        fold(f, fold(f, s, seq_arg(t)), seq_body(t))

      :apply ->
        fold_list(f, fold(f, s, apply_op(t)), apply_args(t))

      :call ->
        fold_list(
          f,
          fold(f, fold(f, s, call_module(t)), call_name(t)),
          call_args(t)
        )

      :primop ->
        fold_list(f, fold(f, s, primop_name(t)), primop_args(t))

      :case ->
        fold_list(f, fold(f, s, case_arg(t)), case_clauses(t))

      :clause ->
        fold(
          f,
          fold(f, fold_list(f, s, clause_pats(t)), clause_guard(t)),
          clause_body(t)
        )

      :alias ->
        fold(f, fold(f, s, alias_var(t)), alias_pat(t))

      :fun ->
        fold(f, fold_list(f, s, fun_vars(t)), fun_body(t))

      :receive ->
        fold(
          f,
          fold(f, fold_list(f, s, receive_clauses(t)), receive_timeout(t)),
          receive_action(t)
        )

      :try ->
        fold(
          f,
          fold_list(
            f,
            fold(
              f,
              fold_list(f, fold(f, s, try_arg(t)), try_vars(t)),
              try_body(t)
            ),
            try_evars(t)
          ),
          try_handler(t)
        )

      :catch ->
        fold(f, s, catch_body(t))

      :binary ->
        fold_list(f, s, binary_segments(t))

      :bitstr ->
        fold(
          f,
          fold(
            f,
            fold(
              f,
              fold(f, fold(f, s, bitstr_val(t)), bitstr_size(t)),
              bitstr_unit(t)
            ),
            bitstr_type(t)
          ),
          bitstr_flags(t)
        )

      :letrec ->
        fold(f, fold_pairs(f, s, letrec_defs(t)), letrec_body(t))

      :module ->
        fold_pairs(
          f,
          fold_pairs(
            f,
            fold_list(f, fold(f, s, module_name(t)), module_exports(t)),
            module_attrs(t)
          ),
          module_defs(t)
        )
    end
  end

  defp fold_list(f, s, [t | ts]) do
    fold_list(f, fold(f, s, t), ts)
  end

  defp fold_list(_, s, []) do
    s
  end

  defp fold_pairs(f, s, [{t1, t2} | ps]) do
    fold_pairs(f, fold(f, fold(f, s, t1), t2), ps)
  end

  defp fold_pairs(_, s, []) do
    s
  end

  def mapfold(f, s0, t) do
    mapfold(
      fn t0, a ->
        {t0, a}
      end,
      f,
      s0,
      t
    )
  end

  def mapfold(pre, post, s00, t0) do
    {t, s0} = pre.(t0, s00)

    case type(t) do
      :literal ->
        case concrete(t) do
          [_ | _] ->
            {t1, s1} = mapfold(pre, post, s0, cons_hd(t))
            {t2, s2} = mapfold(pre, post, s1, cons_tl(t))
            post.(update_c_cons(t, t1, t2), s2)

          v when tuple_size(v) > 0 ->
            {ts, s1} = mapfold_list(pre, post, s0, tuple_es(t))
            post.(update_c_tuple(t, ts), s1)

          _ ->
            post.(t, s0)
        end

      :var ->
        post.(t, s0)

      :values ->
        {ts, s1} = mapfold_list(pre, post, s0, values_es(t))
        post.(update_c_values(t, ts), s1)

      :cons ->
        {t1, s1} = mapfold(pre, post, s0, cons_hd(t))
        {t2, s2} = mapfold(pre, post, s1, cons_tl(t))
        post.(update_c_cons_skel(t, t1, t2), s2)

      :tuple ->
        {ts, s1} = mapfold_list(pre, post, s0, tuple_es(t))
        post.(update_c_tuple_skel(t, ts), s1)

      :map ->
        {m, s1} = mapfold(pre, post, s0, map_arg(t))
        {ts, s2} = mapfold_list(pre, post, s1, map_es(t))
        post.(update_c_map(t, m, ts), s2)

      :map_pair ->
        {op, s1} = mapfold(pre, post, s0, map_pair_op(t))
        {key, s2} = mapfold(pre, post, s1, map_pair_key(t))
        {val, s3} = mapfold(pre, post, s2, map_pair_val(t))
        post.(update_c_map_pair(t, op, key, val), s3)

      :let ->
        {vs, s1} = mapfold_list(pre, post, s0, let_vars(t))
        {a, s2} = mapfold(pre, post, s1, let_arg(t))
        {b, s3} = mapfold(pre, post, s2, let_body(t))
        post.(update_c_let(t, vs, a, b), s3)

      :seq ->
        {a, s1} = mapfold(pre, post, s0, seq_arg(t))
        {b, s2} = mapfold(pre, post, s1, seq_body(t))
        post.(update_c_seq(t, a, b), s2)

      :apply ->
        {e, s1} = mapfold(pre, post, s0, apply_op(t))
        {as, s2} = mapfold_list(pre, post, s1, apply_args(t))
        post.(update_c_apply(t, e, as), s2)

      :call ->
        {m, s1} = mapfold(pre, post, s0, call_module(t))
        {n, s2} = mapfold(pre, post, s1, call_name(t))
        {as, s3} = mapfold_list(pre, post, s2, call_args(t))
        post.(update_c_call(t, m, n, as), s3)

      :primop ->
        {n, s1} = mapfold(pre, post, s0, primop_name(t))
        {as, s2} = mapfold_list(pre, post, s1, primop_args(t))
        post.(update_c_primop(t, n, as), s2)

      :case ->
        {a, s1} = mapfold(pre, post, s0, case_arg(t))
        {cs, s2} = mapfold_list(pre, post, s1, case_clauses(t))
        post.(update_c_case(t, a, cs), s2)

      :clause ->
        {ps, s1} = mapfold_list(pre, post, s0, clause_pats(t))
        {g, s2} = mapfold(pre, post, s1, clause_guard(t))
        {b, s3} = mapfold(pre, post, s2, clause_body(t))
        post.(update_c_clause(t, ps, g, b), s3)

      :alias ->
        {v, s1} = mapfold(pre, post, s0, alias_var(t))
        {p, s2} = mapfold(pre, post, s1, alias_pat(t))
        post.(update_c_alias(t, v, p), s2)

      :fun ->
        {vs, s1} = mapfold_list(pre, post, s0, fun_vars(t))
        {b, s2} = mapfold(pre, post, s1, fun_body(t))
        post.(update_c_fun(t, vs, b), s2)

      :receive ->
        {cs, s1} = mapfold_list(pre, post, s0, receive_clauses(t))
        {e, s2} = mapfold(pre, post, s1, receive_timeout(t))
        {a, s3} = mapfold(pre, post, s2, receive_action(t))
        post.(update_c_receive(t, cs, e, a), s3)

      :try ->
        {e, s1} = mapfold(pre, post, s0, try_arg(t))
        {vs, s2} = mapfold_list(pre, post, s1, try_vars(t))
        {b, s3} = mapfold(pre, post, s2, try_body(t))
        {evs, s4} = mapfold_list(pre, post, s3, try_evars(t))
        {h, s5} = mapfold(pre, post, s4, try_handler(t))
        post.(update_c_try(t, e, vs, b, evs, h), s5)

      :catch ->
        {b, s1} = mapfold(pre, post, s0, catch_body(t))
        post.(update_c_catch(t, b), s1)

      :binary ->
        {ds, s1} = mapfold_list(pre, post, s0, binary_segments(t))
        post.(update_c_binary(t, ds), s1)

      :bitstr ->
        {val, s1} = mapfold(pre, post, s0, bitstr_val(t))
        {size, s2} = mapfold(pre, post, s1, bitstr_size(t))
        {unit, s3} = mapfold(pre, post, s2, bitstr_unit(t))
        {type, s4} = mapfold(pre, post, s3, bitstr_type(t))
        {flags, s5} = mapfold(pre, post, s4, bitstr_flags(t))

        post.(
          update_c_bitstr(t, val, size, unit, type, flags),
          s5
        )

      :letrec ->
        {ds, s1} = mapfold_pairs(pre, post, s0, letrec_defs(t))
        {b, s2} = mapfold(pre, post, s1, letrec_body(t))
        post.(update_c_letrec(t, ds, b), s2)

      :module ->
        {n, s1} = mapfold(pre, post, s0, module_name(t))
        {es, s2} = mapfold_list(pre, post, s1, module_exports(t))
        {as, s3} = mapfold_pairs(pre, post, s2, module_attrs(t))
        {ds, s4} = mapfold_pairs(pre, post, s3, module_defs(t))
        post.(update_c_module(t, n, es, as, ds), s4)
    end
  end

  defp mapfold_list(pre, post, s0, [t | ts]) do
    {t1, s1} = mapfold(pre, post, s0, t)
    {ts1, s2} = mapfold_list(pre, post, s1, ts)
    {[t1 | ts1], s2}
  end

  defp mapfold_list(_, _, s, []) do
    {[], s}
  end

  defp mapfold_pairs(pre, post, s0, [{t1, t2} | ps]) do
    {t3, s1} = mapfold(pre, post, s0, t1)
    {t4, s2} = mapfold(pre, post, s1, t2)
    {ps1, s3} = mapfold_pairs(pre, post, s2, ps)
    {[{t3, t4} | ps1], s3}
  end

  defp mapfold_pairs(_, _, s, []) do
    {[], s}
  end

  def variables(t) do
    variables(t, false)
  end

  def free_variables(t) do
    variables(t, true)
  end

  defp variables(t, s) do
    case type(t) do
      :literal ->
        []

      :var ->
        [var_name(t)]

      :values ->
        vars_in_list(values_es(t), s)

      :cons ->
        :ordsets.union(
          variables(cons_hd(t), s),
          variables(cons_tl(t), s)
        )

      :tuple ->
        vars_in_list(tuple_es(t), s)

      :map ->
        vars_in_list([map_arg(t) | map_es(t)], s)

      :map_pair ->
        vars_in_list(
          [map_pair_op(t), map_pair_key(t), map_pair_val(t)],
          s
        )

      :let ->
        vs = variables(let_body(t), s)
        vs1 = var_list_names(let_vars(t))

        vs2 =
          case s do
            true ->
              :ordsets.subtract(vs, vs1)

            false ->
              :ordsets.union(vs, vs1)
          end

        :ordsets.union(variables(let_arg(t), s), vs2)

      :seq ->
        :ordsets.union(
          variables(seq_arg(t), s),
          variables(seq_body(t), s)
        )

      :apply ->
        :ordsets.union(
          variables(apply_op(t), s),
          vars_in_list(apply_args(t), s)
        )

      :call ->
        :ordsets.union(
          variables(call_module(t), s),
          :ordsets.union(
            variables(call_name(t), s),
            vars_in_list(call_args(t), s)
          )
        )

      :primop ->
        vars_in_list(primop_args(t), s)

      :case ->
        :ordsets.union(
          variables(case_arg(t), s),
          vars_in_list(case_clauses(t), s)
        )

      :clause ->
        vs =
          :ordsets.union(
            variables(clause_guard(t), s),
            variables(clause_body(t), s)
          )

        vs1 = vars_in_list(clause_pats(t), s)

        case s do
          true ->
            :ordsets.subtract(vs, vs1)

          false ->
            :ordsets.union(vs, vs1)
        end

      :alias ->
        :ordsets.add_element(
          var_name(alias_var(t)),
          variables(alias_pat(t))
        )

      :fun ->
        vs = variables(fun_body(t), s)
        vs1 = var_list_names(fun_vars(t))

        case s do
          true ->
            :ordsets.subtract(vs, vs1)

          false ->
            :ordsets.union(vs, vs1)
        end

      :receive ->
        :ordsets.union(
          vars_in_list(receive_clauses(t), s),
          :ordsets.union(
            variables(receive_timeout(t), s),
            variables(receive_action(t), s)
          )
        )

      :try ->
        vs = variables(try_body(t), s)
        vs1 = var_list_names(try_vars(t))

        vs2 =
          case s do
            true ->
              :ordsets.subtract(vs, vs1)

            false ->
              :ordsets.union(vs, vs1)
          end

        vs3 = variables(try_handler(t), s)
        vs4 = var_list_names(try_evars(t))

        vs5 =
          case s do
            true ->
              :ordsets.subtract(vs3, vs4)

            false ->
              :ordsets.union(vs3, vs4)
          end

        :ordsets.union(
          variables(try_arg(t), s),
          :ordsets.union(vs2, vs5)
        )

      :catch ->
        variables(catch_body(t), s)

      :binary ->
        vars_in_list(binary_segments(t), s)

      :bitstr ->
        :ordsets.union(
          variables(bitstr_val(t), s),
          variables(bitstr_size(t), s)
        )

      :letrec ->
        vs = vars_in_defs(letrec_defs(t), s)
        vs1 = :ordsets.union(variables(letrec_body(t), s), vs)
        vs2 = var_list_names(letrec_vars(t))

        case s do
          true ->
            :ordsets.subtract(vs1, vs2)

          false ->
            :ordsets.union(vs1, vs2)
        end

      :module ->
        vs = vars_in_defs(module_defs(t), s)

        vs1 =
          :ordsets.union(
            vars_in_list(module_exports(t), s),
            vs
          )

        vs2 = var_list_names(module_vars(t))

        case s do
          true ->
            :ordsets.subtract(vs1, vs2)

          false ->
            :ordsets.union(vs1, vs2)
        end
    end
  end

  defp vars_in_list(ts, s) do
    vars_in_list(ts, s, [])
  end

  defp vars_in_list([t | ts], s, a) do
    vars_in_list(ts, s, :ordsets.union(variables(t, s), a))
  end

  defp vars_in_list([], _, a) do
    a
  end

  defp vars_in_defs(ds, s) do
    vars_in_defs(ds, s, [])
  end

  defp vars_in_defs([{_, post} | ds], s, a) do
    vars_in_defs(ds, s, :ordsets.union(variables(post, s), a))
  end

  defp vars_in_defs([], _, a) do
    a
  end

  defp var_list_names(vs) do
    var_list_names(vs, [])
  end

  defp var_list_names([v | vs], a) do
    var_list_names(vs, :ordsets.add_element(var_name(v), a))
  end

  defp var_list_names([], a) do
    a
  end

  def next_free_variable_name(t) do
    1 + next_free(t, -1)
  end

  defp next_free(t, max) do
    case type(t) do
      :literal ->
        max

      :var ->
        case var_name(t) do
          int when is_integer(int) ->
            max(int, max)

          _ ->
            max
        end

      :values ->
        next_free_in_list(values_es(t), max)

      :cons ->
        next_free(cons_hd(t), next_free(cons_tl(t), max))

      :tuple ->
        next_free_in_list(tuple_es(t), max)

      :map ->
        next_free_in_list([map_arg(t) | map_es(t)], max)

      :map_pair ->
        next_free_in_list(
          [map_pair_op(t), map_pair_key(t), map_pair_val(t)],
          max
        )

      :let ->
        max1 = next_free(let_body(t), max)
        max2 = next_free_in_list(let_vars(t), max1)
        next_free(let_arg(t), max2)

      :seq ->
        next_free(seq_arg(t), next_free(seq_body(t), max))

      :apply ->
        next_free(
          apply_op(t),
          next_free_in_list(apply_args(t), max)
        )

      :call ->
        next_free(
          call_module(t),
          next_free(
            call_name(t),
            next_free_in_list(call_args(t), max)
          )
        )

      :primop ->
        next_free_in_list(primop_args(t), max)

      :case ->
        next_free(
          case_arg(t),
          next_free_in_list(case_clauses(t), max)
        )

      :clause ->
        max1 =
          next_free(
            clause_guard(t),
            next_free(clause_body(t), max)
          )

        next_free_in_list(clause_pats(t), max1)

      :alias ->
        next_free(alias_var(t), next_free(alias_pat(t), max))

      :fun ->
        next_free(
          fun_body(t),
          next_free_in_list(fun_vars(t), max)
        )

      :receive ->
        max1 =
          next_free_in_list(
            receive_clauses(t),
            next_free(receive_timeout(t), max)
          )

        next_free(receive_action(t), max1)

      :try ->
        max1 = next_free(try_body(t), max)
        max2 = next_free_in_list(try_vars(t), max1)
        max3 = next_free(try_handler(t), max2)
        max4 = next_free_in_list(try_evars(t), max3)
        next_free(try_arg(t), max4)

      :catch ->
        next_free(catch_body(t), max)

      :binary ->
        next_free_in_list(binary_segments(t), max)

      :bitstr ->
        next_free(bitstr_val(t), next_free(bitstr_size(t), max))

      :letrec ->
        max1 = next_free_in_defs(letrec_defs(t), max)
        max2 = next_free(letrec_body(t), max1)
        next_free_in_list(letrec_vars(t), max2)

      :module ->
        next_free_in_defs(module_defs(t), max)
    end
  end

  defp next_free_in_list([h | t], max) do
    next_free_in_list(t, next_free(h, max))
  end

  defp next_free_in_list([], max) do
    max
  end

  defp next_free_in_defs([{_, post} | ds], max) do
    next_free_in_defs(ds, next_free(post, max))
  end

  defp next_free_in_defs([], max) do
    max
  end

  def label(t) do
    label(t, 0)
  end

  def label(t, n) do
    label(t, n, %{})
  end

  defp label(t, n, env) do
    case type(t) do
      :literal ->
        {t, n}

      :var ->
        varName = var_name(t)

        {as, n1} =
          case env do
            %{^varName => l} ->
              {a, _} = label_ann(t, l)
              {a, n}

            %{} ->
              label_ann(t, n)
          end

        {set_ann(t, as), n1}

      :values ->
        {ts, n1} = label_list(values_es(t), n, env)
        {as, n2} = label_ann(t, n1)
        {ann_c_values(as, ts), n2}

      :cons ->
        {t1, n1} = label(cons_hd(t), n, env)
        {t2, n2} = label(cons_tl(t), n1, env)
        {as, n3} = label_ann(t, n2)
        {ann_c_cons_skel(as, t1, t2), n3}

      :tuple ->
        {ts, n1} = label_list(tuple_es(t), n, env)
        {as, n2} = label_ann(t, n1)
        {ann_c_tuple_skel(as, ts), n2}

      :map ->
        case is_c_map_pattern(t) do
          false ->
            {m, n1} = label(map_arg(t), n, env)
            {ts, n2} = label_list(map_es(t), n1, env)
            {as, n3} = label_ann(t, n2)
            {ann_c_map(as, m, ts), n3}

          true ->
            {ts, n1} = label_list(map_es(t), n, env)
            {as, n2} = label_ann(t, n1)
            {ann_c_map_pattern(as, ts), n2}
        end

      :map_pair ->
        {op, n1} = label(map_pair_op(t), n, env)
        {key, n2} = label(map_pair_key(t), n1, env)
        {val, n3} = label(map_pair_val(t), n2, env)
        {as, n4} = label_ann(t, n3)
        {ann_c_map_pair(as, op, key, val), n4}

      :let ->
        {a, n1} = label(let_arg(t), n, env)
        {vs, n2, env1} = label_vars(let_vars(t), n1, env)
        {b, n3} = label(let_body(t), n2, env1)
        {as, n4} = label_ann(t, n3)
        {ann_c_let(as, vs, a, b), n4}

      :seq ->
        {a, n1} = label(seq_arg(t), n, env)
        {b, n2} = label(seq_body(t), n1, env)
        {as, n3} = label_ann(t, n2)
        {ann_c_seq(as, a, b), n3}

      :apply ->
        {e, n1} = label(apply_op(t), n, env)
        {es, n2} = label_list(apply_args(t), n1, env)
        {as, n3} = label_ann(t, n2)
        {ann_c_apply(as, e, es), n3}

      :call ->
        {m, n1} = label(call_module(t), n, env)
        {f, n2} = label(call_name(t), n1, env)
        {es, n3} = label_list(call_args(t), n2, env)
        {as, n4} = label_ann(t, n3)
        {ann_c_call(as, m, f, es), n4}

      :primop ->
        {f, n1} = label(primop_name(t), n, env)
        {es, n2} = label_list(primop_args(t), n1, env)
        {as, n3} = label_ann(t, n2)
        {ann_c_primop(as, f, es), n3}

      :case ->
        {a, n1} = label(case_arg(t), n, env)
        {cs, n2} = label_list(case_clauses(t), n1, env)
        {as, n3} = label_ann(t, n2)
        {ann_c_case(as, a, cs), n3}

      :clause ->
        {_, n1, env1} = label_vars(clause_vars(t), n, env)
        {ps, n2} = label_list(clause_pats(t), n1, env1)
        {g, n3} = label(clause_guard(t), n2, env1)
        {b, n4} = label(clause_body(t), n3, env1)
        {as, n5} = label_ann(t, n4)
        {ann_c_clause(as, ps, g, b), n5}

      :alias ->
        {v, n1} = label(alias_var(t), n, env)
        {p, n2} = label(alias_pat(t), n1, env)
        {as, n3} = label_ann(t, n2)
        {ann_c_alias(as, v, p), n3}

      :fun ->
        {vs, n1, env1} = label_vars(fun_vars(t), n, env)
        {b, n2} = label(fun_body(t), n1, env1)
        {as, n3} = label_ann(t, n2)
        {ann_c_fun(as, vs, b), n3}

      :receive ->
        {cs, n1} = label_list(receive_clauses(t), n, env)
        {e, n2} = label(receive_timeout(t), n1, env)
        {a, n3} = label(receive_action(t), n2, env)
        {as, n4} = label_ann(t, n3)
        {ann_c_receive(as, cs, e, a), n4}

      :try ->
        {e, n1} = label(try_arg(t), n, env)
        {vs, n2, env1} = label_vars(try_vars(t), n1, env)
        {b, n3} = label(try_body(t), n2, env1)
        {evs, n4, env2} = label_vars(try_evars(t), n3, env)
        {h, n5} = label(try_handler(t), n4, env2)
        {as, n6} = label_ann(t, n5)
        {ann_c_try(as, e, vs, b, evs, h), n6}

      :catch ->
        {b, n1} = label(catch_body(t), n, env)
        {as, n2} = label_ann(t, n1)
        {ann_c_catch(as, b), n2}

      :binary ->
        {ds, n1} = label_list(binary_segments(t), n, env)
        {as, n2} = label_ann(t, n1)
        {ann_c_binary(as, ds), n2}

      :bitstr ->
        {val, n1} = label(bitstr_val(t), n, env)
        {size, n2} = label(bitstr_size(t), n1, env)
        {unit, n3} = label(bitstr_unit(t), n2, env)
        {type, n4} = label(bitstr_type(t), n3, env)
        {flags, n5} = label(bitstr_flags(t), n4, env)
        {as, n6} = label_ann(t, n5)
        {ann_c_bitstr(as, val, size, unit, type, flags), n6}

      :letrec ->
        {_, n1, env1} = label_vars(letrec_vars(t), n, env)
        {ds, n2} = label_defs(letrec_defs(t), n1, env1)
        {b, n3} = label(letrec_body(t), n2, env1)
        {as, n4} = label_ann(t, n3)
        {ann_c_letrec(as, ds, b), n4}

      :module ->
        {_, n1, env1} = label_vars(module_vars(t), n, env)
        {ts, n2} = label_defs(module_attrs(t), n1, env1)
        {ds, n3} = label_defs(module_defs(t), n2, env1)
        {es, n4} = label_list(module_exports(t), n3, env1)
        {as, n5} = label_ann(t, n4)
        {ann_c_module(as, module_name(t), es, ts, ds), n5}
    end
  end

  defp label_list([t | ts], n, env) do
    {t1, n1} = label(t, n, env)
    {ts1, n2} = label_list(ts, n1, env)
    {[t1 | ts1], n2}
  end

  defp label_list([], n, _Env) do
    {[], n}
  end

  defp label_vars([t | ts], n, env) do
    env1 = Map.put(env, var_name(t), n)
    {as, n1} = label_ann(t, n)
    t1 = set_ann(t, as)
    {ts1, n2, env2} = label_vars(ts, n1, env1)
    {[t1 | ts1], n2, env2}
  end

  defp label_vars([], n, env) do
    {[], n, env}
  end

  defp label_defs([{f, t} | ds], n, env) do
    {f1, n1} = label(f, n, env)
    {t1, n2} = label(t, n1, env)
    {ds1, n3} = label_defs(ds, n2, env)
    {[{f1, t1} | ds1], n3}
  end

  defp label_defs([], n, _Env) do
    {[], n}
  end

  defp label_ann(t, n) do
    {[{:label, n} | filter_labels(get_ann(t))], n + 1}
  end

  defp filter_labels([{:label, _} | as]) do
    filter_labels(as)
  end

  defp filter_labels([a | as]) do
    [a | filter_labels(as)]
  end

  defp filter_labels([]) do
    []
  end

  def get_label(t) do
    case get_ann(t) do
      [{:label, l} | _] ->
        l

      _ ->
        throw({:missing_label, t})
    end
  end
end
