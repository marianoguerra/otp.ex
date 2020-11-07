defmodule :m_dialyzer_dep do
  use Bitwise
  require Record

  Record.defrecord(:r_analysis, :analysis,
    analysis_pid: :undefined,
    type: :succ_typings,
    defines: [],
    doc_plt: :undefined,
    files: [],
    include_dirs: [],
    start_from: :byte_code,
    plt: :undefined,
    use_contracts: true,
    race_detection: false,
    behaviours_chk: false,
    timing: false,
    timing_server: :none,
    callgraph_file: '',
    solvers: :undefined
  )

  Record.defrecord(:r_options, :options,
    files: [],
    files_rec: [],
    analysis_type: :succ_typings,
    timing: false,
    defines: [],
    from: :byte_code,
    get_warnings: :maybe,
    init_plts: [],
    include_dirs: [],
    output_plt: :none,
    legal_warnings: :ordsets.new(),
    report_mode: :normal,
    erlang_mode: false,
    use_contracts: true,
    output_file: :none,
    output_format: :formatted,
    filename_opt: :basename,
    indent_opt: true,
    callgraph_file: '',
    check_plt: true,
    solvers: [],
    native: :maybe,
    native_cache: true
  )

  Record.defrecord(:r_contract, :contract, contracts: [], args: [], forms: [])

  def analyze(tree) do
    {_, state} = traverse(tree, map__new(), state__new(tree), :top)
    esc = state__esc(state)
    state1 = state__add_deps(:external, output(esc), state)
    deps = state__deps(state1)
    calls = state__calls(state1)
    letrecs = state__letrecs(state1)
    {map__finalize(deps), set__to_ordsets(esc), map__finalize(calls), letrecs}
  end

  defp traverse(tree, out, state, currentFun) do
    case :cerl.type(tree) do
      :apply ->
        op = :cerl.apply_op(tree)
        args = :cerl.apply_args(tree)

        case :var === :cerl.type(op) do
          false ->
            {output(:none), state}

          true ->
            opLabel = :cerl_trees.get_label(op)

            opFuns =
              case map__lookup(opLabel, out) do
                :none ->
                  output(:none)

                {:value, oF} ->
                  oF
              end

            {argFuns, state2} = traverse_list(args, out, state, currentFun)
            state3 = state__add_esc(merge_outs(argFuns), state2)
            state4 = state__add_deps(currentFun, opFuns, state3)

            state5 =
              state__store_callsite(:cerl_trees.get_label(tree), opFuns, length(args), state4)

            case state__get_rvals(opLabel, state5) do
              1 ->
                {output(set__singleton(:external)), state5}

              numRvals ->
                list =
                  :lists.duplicate(
                    numRvals,
                    output(set__singleton(:external))
                  )

                {output(list), state5}
            end
        end

      :binary ->
        {output(:none), state}

      :case ->
        arg = :cerl.case_arg(tree)
        {funs, newState} = traverse(arg, out, state, currentFun)
        clauses = :cerl.case_clauses(tree)
        traverse_clauses(clauses, funs, out, newState, currentFun)

      :call ->
        args = :cerl.call_args(tree)
        {argFuns, state1} = traverse_list(args, out, state, currentFun)
        remote_call(tree, merge_outs(argFuns), state1)

      :catch ->
        traverse(:cerl.catch_body(tree), out, state, currentFun)

      :cons ->
        {hdFuns, state1} = traverse(:cerl.cons_hd(tree), out, state, currentFun)
        {tlFuns, state2} = traverse(:cerl.cons_tl(tree), out, state1, currentFun)
        {merge_outs([hdFuns, tlFuns]), state2}

      :fun ->
        body = :cerl.fun_body(tree)
        label = :cerl_trees.get_label(tree)

        state1 =
          cond do
            currentFun === :top ->
              state__add_deps(:top, output(set__singleton(label)), state)

            true ->
              o1 = output(set__singleton(currentFun))
              o2 = output(set__singleton(label))
              tmpState = state__add_deps(label, o1, state)
              state__add_deps(currentFun, o2, tmpState)
          end

        vars = :cerl.fun_vars(tree)
        out1 = bind_single(vars, output(set__singleton(:external)), out)
        {bodyFuns, state2} = traverse(body, out1, state1, :cerl_trees.get_label(tree))
        {output(set__singleton(label)), state__add_esc(bodyFuns, state2)}

      :let ->
        vars = :cerl.let_vars(tree)
        arg = :cerl.let_arg(tree)
        body = :cerl.let_body(tree)
        oldNumRvals = state__num_rvals(state)
        state1 = state__store_num_rvals(length(vars), state)
        {argFuns, state2} = traverse(arg, out, state1, currentFun)
        out1 = bind_list(vars, argFuns, out)
        state3 = state__store_num_rvals(oldNumRvals, state2)
        traverse(body, out1, state3, currentFun)

      :letrec ->
        defs = :cerl.letrec_defs(tree)
        body = :cerl.letrec_body(tree)

        state1 =
          :lists.foldl(
            fn {var, fun}, acc ->
              state__add_letrecs(
                :cerl_trees.get_label(var),
                :cerl_trees.get_label(fun),
                acc
              )
            end,
            state,
            defs
          )

        out1 = bind_defs(defs, out)
        numRvals = state__num_rvals(state1)
        state2 = traverse_defs(defs, out1, state1, currentFun, numRvals)
        traverse(body, out1, state2, currentFun)

      :literal ->
        {output(:none), state}

      :module ->
        defs = :cerl.module_defs(tree)
        out1 = bind_defs(defs, out)
        state1 = traverse_defs(defs, out1, state, currentFun, 1)
        {output(:none), state1}

      :primop ->
        args = :cerl.primop_args(tree)
        {argFuns, state1} = traverse_list(args, out, state, currentFun)
        primop(tree, merge_outs(argFuns), state1)

      :receive ->
        clauses = :cerl.receive_clauses(tree)
        timeOut = :cerl.receive_timeout(tree)
        action = :cerl.receive_action(tree)
        {clauseFuns, state1} = traverse_clauses(clauses, output(:none), out, state, currentFun)
        {_, state2} = traverse(timeOut, out, state1, currentFun)
        {actionFuns, state3} = traverse(action, out, state2, currentFun)
        {merge_outs([clauseFuns, actionFuns]), state3}

      :seq ->
        {_, state1} = traverse(:cerl.seq_arg(tree), out, state, currentFun)
        traverse(:cerl.seq_body(tree), out, state1, currentFun)

      :try ->
        arg = :cerl.try_arg(tree)
        body = :cerl.try_body(tree)
        vars = :cerl.try_vars(tree)
        eVars = :cerl.try_evars(tree)
        handler = :cerl.try_handler(tree)
        {argFuns, state1} = traverse(arg, out, state, currentFun)
        out1 = bind_list(vars, argFuns, out)
        {bodyFuns, state2} = traverse(body, out1, state1, currentFun)
        out2 = bind_single(eVars, output(set__singleton(:external)), out)
        {handlerFuns, state3} = traverse(handler, out2, state2, currentFun)
        {merge_outs([bodyFuns, handlerFuns]), state3}

      :tuple ->
        args = :cerl.tuple_es(tree)
        {list, state1} = traverse_list(args, out, state, currentFun)
        {merge_outs(list), state1}

      :map ->
        args = :cerl.map_es(tree)
        {list, state1} = traverse_list(args, out, state, currentFun)
        {merge_outs(list), state1}

      :map_pair ->
        key = :cerl.map_pair_key(tree)
        val = :cerl.map_pair_val(tree)
        {list, state1} = traverse_list([key, val], out, state, currentFun)
        {merge_outs(list), state1}

      :values ->
        oldNumRvals = state__num_rvals(state)
        state1 = state__store_num_rvals(1, state)
        {list, state2} = traverse_list(:cerl.values_es(tree), out, state1, currentFun)
        state3 = state__store_num_rvals(oldNumRvals, state2)
        {list, state3}

      :var ->
        case map__lookup(:cerl_trees.get_label(tree), out) do
          :none ->
            {output(:none), state}

          {:value, val} ->
            case is_only_external(val) do
              true ->
                {val, state}

              false ->
                {val, state__add_deps(currentFun, val, state)}
            end
        end
    end
  end

  defp traverse_list(trees, out, state, currentFun) do
    traverse_list(trees, out, state, currentFun, [])
  end

  defp traverse_list([tree | left], out, state, currentFun, acc) do
    {x, state1} = traverse(tree, out, state, currentFun)
    traverse_list(left, out, state1, currentFun, [x | acc])
  end

  defp traverse_list([], _Out, state, _CurrentFun, acc) do
    {output(:lists.reverse(acc)), state}
  end

  defp traverse_defs([{_, fun} | left], out, state, currentFun, numRvals) do
    state1 = state__store_num_rvals(numRvals, state)
    {_, state2} = traverse(fun, out, state1, currentFun)
    traverse_defs(left, out, state2, currentFun, numRvals)
  end

  defp traverse_defs([], _Out, state, _CurrentFun, _NumRvals) do
    state
  end

  defp traverse_clauses(clauses, argFuns, out, state, currentFun) do
    case clauses do
      [] ->
        {output(:none), state}

      clauses1 ->
        traverse_clauses(clauses1, argFuns, out, state, currentFun, [])
    end
  end

  defp traverse_clauses([clause | left], argFuns, out, state, currentFun, acc) do
    pats = :cerl.clause_pats(clause)
    guard = :cerl.clause_guard(clause)
    body = :cerl.clause_body(clause)
    out1 = bind_pats_list(pats, argFuns, out)
    {_, state2} = traverse(guard, out1, state, currentFun)
    {bodyFuns, state3} = traverse(body, out1, state2, currentFun)
    traverse_clauses(left, argFuns, out, state3, currentFun, [bodyFuns | acc])
  end

  defp traverse_clauses([], _ArgFuns, _Out, state, _CurrentFun, acc) do
    {merge_outs(acc), state}
  end

  defp remote_call(tree, argFuns, state) do
    m = :cerl.call_module(tree)
    f = :cerl.call_name(tree)
    a = length(:cerl.call_args(tree))

    case :cerl.is_c_atom(m) and :cerl.is_c_atom(f) do
      false ->
        {output(set__singleton(:external)), state__add_esc(argFuns, state)}

      true ->
        m1 = :cerl.atom_val(m)
        f1 = :cerl.atom_val(f)
        literal = :cerl_closurean.is_literal_op(m1, f1, a)

        case :erl_bifs.is_pure(m1, f1, a) do
          true ->
            case literal do
              true ->
                {output(:none), state}

              false ->
                {output(set__singleton(:external)), state__add_esc(argFuns, state)}
            end

          false ->
            state1 =
              case :cerl_closurean.is_escape_op(m1, f1, a) do
                true ->
                  state__add_esc(argFuns, state)

                false ->
                  state
              end

            case literal do
              true ->
                {output(:none), state1}

              false ->
                {add_external(argFuns), state1}
            end
        end
    end
  end

  defp primop(tree, argFuns, state) do
    f = :cerl.atom_val(:cerl.primop_name(tree))
    a = length(:cerl.primop_args(tree))

    state1 =
      case :cerl_closurean.is_escape_op(f, a) do
        true ->
          state__add_esc(argFuns, state)

        false ->
          state
      end

    case :cerl_closurean.is_literal_op(f, a) do
      true ->
        {output(:none), state1}

      false ->
        {argFuns, state1}
    end
  end

  Record.defrecord(:r_set, :set, set: :undefined)

  defp set__singleton(val) do
    r_set(set: :sets.add_element(val, :sets.new()))
  end

  defp set__from_list(list) do
    r_set(set: :sets.from_list(list))
  end

  defp set__is_element(_El, :none) do
    false
  end

  defp set__is_element(el, r_set(set: set)) do
    :sets.is_element(el, set)
  end

  defp set__union(:none, set) do
    set
  end

  defp set__union(set, :none) do
    set
  end

  defp set__union(r_set(set: s1), r_set(set: s2)) do
    r_set(set: :sets.union(s1, s2))
  end

  defp set__to_ordsets(:none) do
    []
  end

  defp set__to_ordsets(r_set(set: set)) do
    :ordsets.from_list(:sets.to_list(set))
  end

  defp set__size(:none) do
    0
  end

  defp set__size(r_set(set: set)) do
    :sets.size(set)
  end

  defp set__filter(r_set(set: set), fun) do
    newSet = :sets.filter(fun, set)

    case :sets.size(newSet) === 0 do
      true ->
        :none

      false ->
        r_set(set: newSet)
    end
  end

  Record.defrecord(:r_output, :output,
    type: :undefined,
    content: :undefined
  )

  defp output(:none) do
    r_output(type: :single, content: :none)
  end

  defp output(s = r_set()) do
    r_output(type: :single, content: s)
  end

  defp output(list) when is_list(list) do
    r_output(type: :list, content: list)
  end

  defp merge_outs([h | t]) do
    merge_outs(t, h)
  end

  defp merge_outs(r_output(type: :list, content: [h | t])) do
    merge_outs(t, h)
  end

  defp merge_outs(r_output(type: :list, content: [])) do
    output(:none)
  end

  defp merge_outs([r_output(content: :none) | left], o) do
    merge_outs(left, o)
  end

  defp merge_outs([o | left], r_output(content: :none)) do
    merge_outs(left, o)
  end

  defp merge_outs(
         [r_output(type: :single, content: s1) | left],
         r_output(type: :single, content: s2)
       ) do
    merge_outs(left, output(set__union(s1, s2)))
  end

  defp merge_outs(
         [r_output(type: :list, content: l1) | left],
         r_output(type: :list, content: l2)
       ) do
    newList =
      for {x, y} <- :lists.zip(l1, l2) do
        merge_outs([x, y])
      end

    merge_outs(left, output(newList))
  end

  defp merge_outs([], res) do
    res
  end

  defp filter_outs(r_output(type: :single, content: s), fun) do
    output(set__filter(s, fun))
  end

  defp add_external(r_output(type: :single, content: set)) do
    output(set__union(set, set__singleton(:external)))
  end

  defp add_external(r_output(type: :list, content: list)) do
    output(
      for o <- list do
        add_external(o)
      end
    )
  end

  defp is_only_external(r_output(type: :single, content: set)) do
    set__is_element(:external, set) and set__size(set) === 1
  end

  defp map__new() do
    :dict.new()
  end

  defp map__add(_Label, :none, map) do
    map
  end

  defp map__add(label, set, map) do
    case map__lookup(label, map) do
      {:value, oldSet} ->
        newSet = set__union(oldSet, set)
        map__store(label, newSet, map)

      :none ->
        map__store(label, set, map)
    end
  end

  defp map__store(label, val, map) do
    :dict.store(label, val, map)
  end

  defp map__lookup(label, map) do
    case :dict.find(label, map) do
      {:ok, val} ->
        {:value, val}

      :error ->
        :none
    end
  end

  defp map__finalize(map) do
    :dict.map(
      fn
        _Key, r_set() = set ->
          set__to_ordsets(set)

        _Key, r_output(type: :single, content: set) ->
          set__to_ordsets(set)
      end,
      map
    )
  end

  defp bind_pats_list(_Pats, r_output(content: :none), map) do
    map
  end

  defp bind_pats_list([pat], r_output(type: :single) = o, map) do
    bind_single(all_vars(pat), o, map)
  end

  defp bind_pats_list(pats, r_output(type: :list, content: list), map) do
    bind_pats_list(pats, list, map)
  end

  defp bind_pats_list([pat | patLeft], [r_output(type: :single) = o | setLeft], map) do
    map1 = bind_single(all_vars(pat), o, map)
    bind_pats_list(patLeft, setLeft, map1)
  end

  defp bind_pats_list([pat | patLeft], [r_output(type: :list, content: list) | setLeft], map) do
    map1 =
      case :cerl.is_c_values(pat) do
        true ->
          bind_pats_list(:cerl.values_es(pat), list, map)

        false ->
          bind_single(all_vars(pat), merge_outs(list), map)
      end

    bind_pats_list(patLeft, setLeft, map1)
  end

  defp bind_pats_list([], [], map) do
    map
  end

  defp bind_single([var | left], o, map) do
    bind_single(left, o, map__store(:cerl_trees.get_label(var), o, map))
  end

  defp bind_single([], _O, map) do
    map
  end

  defp bind_list(list, r_output(type: :single) = o, map) do
    bind_single(list, o, map)
  end

  defp bind_list(list1, r_output(type: :list, content: list2), map) do
    bind_list1(list1, list2, map)
  end

  defp bind_list1([var | varLeft], [o | oLeft], map) do
    bind_list1(varLeft, oLeft, map__store(:cerl_trees.get_label(var), o, map))
  end

  defp bind_list1([], [], map) do
    map
  end

  defp bind_defs([{var, fun} | left], map) do
    o = output(set__singleton(:cerl_trees.get_label(fun)))
    map1 = map__store(:cerl_trees.get_label(var), o, map)
    bind_defs(left, map1)
  end

  defp bind_defs([], map) do
    map
  end

  defp all_vars(tree) do
    all_vars(tree, [])
  end

  defp all_vars(tree, accIn) do
    :cerl_trees.fold(
      fn subTree, acc ->
        case :cerl.is_c_var(subTree) do
          true ->
            [subTree | acc]

          false ->
            acc
        end
      end,
      accIn,
      tree
    )
  end

  Record.defrecord(:r_state, :state,
    deps: :undefined,
    esc: :undefined,
    calls: :undefined,
    arities: :undefined,
    letrecs: :undefined,
    num_rvals: 1,
    rvals: %{}
  )

  defp state__new(tree) do
    exports =
      set__from_list(
        for x <- :cerl.module_exports(tree) do
          x
        end
      )

    expLs =
      for {var, fun} <- :cerl.module_defs(tree),
          set__is_element(var, exports) do
        :cerl_trees.get_label(fun)
      end

    onLoadFAs =
      :lists.flatten(
        for {attr, args} <- :cerl.module_attrs(tree),
            :cerl.atom_val(attr) === :on_load do
          :cerl.atom_val(args)
        end
      )

    onLoadLs =
      for {var, fun} <- :cerl.module_defs(tree),
          :lists.member(:cerl.var_name(var), onLoadFAs) do
        :cerl_trees.get_label(fun)
      end

    initEsc = set__from_list(onLoadLs ++ expLs)
    arities = :cerl_trees.fold(&find_arities/2, :dict.new(), tree)

    r_state(
      deps: map__new(),
      esc: initEsc,
      calls: map__new(),
      arities: arities,
      letrecs: map__new()
    )
  end

  defp find_arities(tree, accMap) do
    case :cerl.is_c_fun(tree) do
      true ->
        label = :cerl_trees.get_label(tree)
        arity = :cerl.fun_arity(tree)
        :dict.store(label, arity, accMap)

      false ->
        accMap
    end
  end

  defp state__add_deps(_From, r_output(content: :none), state) do
    state
  end

  defp state__add_deps(from, r_output(type: :single, content: to), r_state(deps: map) = state) do
    r_state(state, deps: map__add(from, to, map))
  end

  defp state__add_letrecs(
         var,
         fun,
         r_state(letrecs: map, num_rvals: numRvals, rvals: rvals) = state
       ) do
    r_state(state,
      letrecs: map__store(var, fun, map),
      rvals: %{rvals | var => numRvals}
    )
  end

  defp state__deps(r_state(deps: deps)) do
    deps
  end

  defp state__letrecs(r_state(letrecs: letrecs)) do
    letrecs
  end

  defp state__add_esc(r_output(content: :none), state) do
    state
  end

  defp state__add_esc(
         r_output(type: :single, content: set),
         r_state(esc: esc) = state
       ) do
    r_state(state, esc: set__union(set, esc))
  end

  defp state__add_esc(
         r_output(type: :list, content: [h | t]),
         r_state(esc: esc) = state
       ) do
    r_output(type: :single, content: set) = merge_outs(t, h)
    r_state(state, esc: set__union(set, esc))
  end

  defp state__esc(r_state(esc: esc)) do
    esc
  end

  defp state__store_callsite(_From, r_output(content: :none), _CallArity, state) do
    state
  end

  defp state__store_callsite(from, to, callArity, r_state(calls: calls, arities: arities) = state) do
    filter = fn
      :external ->
        true

      fun ->
        callArity === :dict.fetch(fun, arities)
    end

    case filter_outs(to, filter) do
      r_output(content: :none) ->
        state

      to1 ->
        r_state(state, calls: map__store(from, to1, calls))
    end
  end

  defp state__calls(r_state(calls: calls)) do
    calls
  end

  defp state__store_num_rvals(numRval, state) do
    r_state(state, num_rvals: numRval)
  end

  defp state__num_rvals(r_state(num_rvals: numRvals)) do
    numRvals
  end

  defp state__get_rvals(funLabel, r_state(rvals: rvals)) do
    case rvals do
      %{^funLabel => numRvals} ->
        numRvals

      %{} ->
        1
    end
  end
end
