defmodule :m_dialyzer_clean_core do
  use Bitwise

  def clean(tree) do
    case :cerl.type(tree) do
      :apply ->
        op = clean(:cerl.apply_op(tree))
        args = clean_list(:cerl.apply_args(tree))
        :cerl.update_c_apply(tree, op, args)

      :binary ->
        segments = clean_list(:cerl.binary_segments(tree))
        :cerl.update_c_binary(tree, segments)

      :bitstr ->
        val = clean(:cerl.bitstr_val(tree))
        size = clean(:cerl.bitstr_size(tree))
        unit = :cerl.bitstr_unit(tree)
        type = :cerl.bitstr_type(tree)
        flags = :cerl.bitstr_flags(tree)
        :cerl.update_c_bitstr(tree, val, size, unit, type, flags)

      :case ->
        arg = clean(:cerl.case_arg(tree))
        clauses = clean_clauses(:cerl.case_clauses(tree))
        :cerl.update_c_case(tree, arg, clauses)

      :call ->
        args = clean_list(:cerl.call_args(tree))
        module = clean(:cerl.call_module(tree))
        name = clean(:cerl.call_name(tree))
        :cerl.update_c_call(tree, module, name, args)

      :catch ->
        body = clean(:cerl.catch_body(tree))
        :cerl.update_c_catch(tree, body)

      :cons ->
        hd = clean(:cerl.cons_hd(tree))
        tl = clean(:cerl.cons_tl(tree))
        :cerl.update_c_cons_skel(tree, hd, tl)

      :fun ->
        body = clean(:cerl.fun_body(tree))
        vars = :cerl.fun_vars(tree)
        :cerl.update_c_fun(tree, vars, body)

      :let ->
        arg = clean(:cerl.let_arg(tree))
        body = clean(:cerl.let_body(tree))
        vars = :cerl.let_vars(tree)
        :cerl.update_c_let(tree, vars, arg, body)

      :letrec ->
        clean_letrec(tree)

      :literal ->
        tree

      :module ->
        defs = clean_defs(:cerl.module_defs(tree))
        name = :cerl.module_name(tree)
        exports = :cerl.module_exports(tree)
        attrs = :cerl.module_attrs(tree)
        :cerl.update_c_module(tree, name, exports, attrs, defs)

      :primop ->
        args = clean_list(:cerl.primop_args(tree))
        name = :cerl.primop_name(tree)
        :cerl.update_c_primop(tree, name, args)

      :receive ->
        clauses = clean_clauses(:cerl.receive_clauses(tree))
        timeout = clean(:cerl.receive_timeout(tree))
        action = clean(:cerl.receive_action(tree))
        :cerl.update_c_receive(tree, clauses, timeout, action)

      :seq ->
        arg = clean(:cerl.seq_arg(tree))
        body = clean(:cerl.seq_body(tree))
        :cerl.update_c_seq(tree, arg, body)

      :try ->
        arg = clean(:cerl.try_arg(tree))
        body = clean(:cerl.try_body(tree))
        handler = clean(:cerl.try_handler(tree))
        vs = :cerl.try_vars(tree)
        evs = :cerl.try_evars(tree)
        try = :cerl.update_c_try(tree, arg, vs, body, evs, handler)
        try

      :tuple ->
        elements = clean_list(:cerl.tuple_es(tree))
        :cerl.update_c_tuple_skel(tree, elements)

      :map ->
        arg = clean(:cerl.map_arg(tree))
        entries = clean_map_pairs(:cerl.map_es(tree))
        :cerl.update_c_map(tree, arg, entries)

      :values ->
        elements = clean_list(:cerl.values_es(tree))
        :cerl.update_c_values(tree, elements)

      :var ->
        tree
    end
  end

  defp clean_letrec(tree) do
    case :lists.member(
           :letrec_goto,
           :cerl.get_ann(tree)
         ) do
      true ->
        [{_Name, fun}] = :cerl.letrec_defs(tree)
        funBody = :cerl.fun_body(fun)
        funBody1 = clean(funBody)
        body = clean(:cerl.letrec_body(tree))

        case dialyzer_ignore(body) do
          true ->
            funBody1

          false ->
            primopUnknown =
              :cerl.c_primop(
                :cerl.abstract(:dialyzer_unknown),
                []
              )

            clauses = [
              :cerl.c_clause([:cerl.abstract(:a)], body),
              :cerl.c_clause([:cerl.abstract(:b)], funBody1)
            ]

            :cerl.c_case(primopUnknown, clauses)
        end

      false ->
        defs = clean_defs(:cerl.letrec_defs(tree))
        body = clean(:cerl.letrec_body(tree))
        :cerl.update_c_letrec(tree, defs, body)
    end
  end

  defp clean_defs(defs) do
    for {name, fun} <- defs do
      {name, clean(fun)}
    end
  end

  defp clean_clauses([clause | tail]) do
    case clean_clause(clause) do
      :ignore ->
        clean_clauses(tail)

      clause1 ->
        tail1 = clean_clauses(tail)
        [clause1 | tail1]
    end
  end

  defp clean_clauses([]) do
    []
  end

  defp clean_clause(clause) do
    body = :cerl.clause_body(clause)

    case dialyzer_ignore(clause) or is_raising_body(body) do
      true ->
        :ignore

      false ->
        g = clean(:cerl.clause_guard(clause))
        body1 = clean(body)
        pats = :cerl.clause_pats(clause)
        :cerl.update_c_clause(clause, pats, g, body1)
    end
  end

  defp is_raising_body(body) do
    case :cerl.type(body) do
      :primop ->
        case :cerl.atom_val(:cerl.primop_name(body)) do
          :match_fail ->
            true

          :raise ->
            true

          _ ->
            false
        end

      _ ->
        false
    end
  end

  defp clean_list(trees) do
    for tree <- trees do
      clean(tree)
    end
  end

  defp clean_map_pairs([pair | pairs]) do
    key = clean(:cerl.map_pair_key(pair))
    val = clean(:cerl.map_pair_val(pair))
    pairs1 = clean_map_pairs(pairs)
    op = :cerl.map_pair_op(pair)
    pair1 = :cerl.update_c_map_pair(pair, op, key, val)
    [pair1 | pairs1]
  end

  defp clean_map_pairs([]) do
    []
  end

  defp dialyzer_ignore(tree) do
    :lists.member(:dialyzer_ignore, :cerl.get_ann(tree))
  end
end
