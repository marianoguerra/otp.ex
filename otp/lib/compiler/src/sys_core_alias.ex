defmodule :m_sys_core_alias do
  use Bitwise
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
  Record.defrecord(:r_sub, :sub, p: %{}, v: :cerl_sets.new(), t: :none)

  def module(r_c_module(defs: ds0) = mod, _Opts) do
    ds1 =
      for d <- ds0 do
        __MODULE__.def(d)
      end

    {:ok, r_c_module(mod, defs: ds1), []}
  end

  defp def({r_c_var(name: {f, arity}) = name, b0}) do
    try do
      :erlang.put(:new_var_num, 0)
      {b1, _} = :cerl_trees.mapfold(&pre/2, &post/2, sub_new(:none), b0)
      :erlang.erase(:new_var_num)
      {name, b1}
    catch
      class, error ->
        :io.fwrite('Function: ~w/~w\n', [f, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  defp pre(r_c_clause(pats: pats) = node, sub0) do
    case get_pattern_keys(pats) do
      [] when r_sub(sub0, :t) !== :none ->
        varNames = get_variables(pats)
        {node, sub_fold(varNames, sub0)}

      [] ->
        {node, sub0}

      keys ->
        varNames = get_variables(pats)
        sub1 = sub_fold(varNames, sub0)
        sub2 = sub_add_keys(keys, sub1)
        r_sub(v: subNames, t: temp) = sub2

        sub3 =
          r_sub(sub2,
            v: merge_variables(varNames, subNames),
            t: {:clause, pats, keys, subNames, temp}
          )

        {r_c_clause(node, pats: []), sub3}
    end
  end

  defp pre(r_c_let(vars: vars) = node, sub)
       when r_sub(sub, :t) !== :none do
    {node, sub_fold(get_variables(vars), sub)}
  end

  defp pre(r_c_fun(vars: vars) = node, sub)
       when r_sub(sub, :t) !== :none do
    {node, sub_fold(get_variables(vars), sub)}
  end

  defp pre(node, sub0) when r_sub(sub0, :t) !== :none do
    case :cerl.is_data(node) and not :cerl.is_literal(node) do
      false ->
        {node, sub0}

      true ->
        kind = :cerl.data_type(node)
        es = :cerl.data_es(node)

        case sub_cache_nodes(kind, es, sub0) do
          {name, sub1} ->
            {:cerl.ann_c_var(:cerl.get_ann(node), name), sub1}

          :error ->
            {node, sub0}
        end
    end
  end

  defp pre(node, sub) do
    {node, sub}
  end

  defp post(
         r_c_clause() = node,
         r_sub(t: {:clause, pats0, keys, v, t}) = sub0
       ) do
    {sub1, postKeys} = sub_take_keys(keys, sub0)
    pats1 = put_pattern_keys(pats0, postKeys)
    sub2 = sub_unfold(r_sub(sub1, v: v, t: t))
    {r_c_clause(node, pats: pats1), sub2}
  end

  defp post(r_c_clause() = node, sub) do
    {node, sub_unfold(sub)}
  end

  defp post(r_c_let() = node, sub) do
    {node, sub_unfold(sub)}
  end

  defp post(r_c_fun() = node, sub) do
    {node, sub_unfold(sub)}
  end

  defp post(node, sub) do
    {node, sub}
  end

  defp sub_new(temp) do
    r_sub(t: temp)
  end

  defp sub_fold(varNames, r_sub(v: subNames) = sub) do
    case is_disjoint_variables(varNames, subNames) do
      true ->
        r_sub(sub, t: {:temp, r_sub(sub, :t)})

      false ->
        sub_new({:sub, sub})
    end
  end

  defp sub_unfold(r_sub(t: :none) = sub) do
    sub
  end

  defp sub_unfold(r_sub(t: {:temp, temp}) = sub) do
    r_sub(sub, t: temp)
  end

  defp sub_unfold(r_sub(t: {:sub, sub})) do
    sub
  end

  defp sub_add_keys(keys, r_sub(p: pat0) = sub) do
    pat1 =
      :lists.foldl(
        fn key, acc ->
          false = :maps.is_key(key, acc)
          :maps.put(key, 0, acc)
        end,
        pat0,
        keys
      )

    r_sub(sub, p: pat1)
  end

  defp sub_take_keys(keys, r_sub(p: pat0) = sub) do
    {pat1, acc} = sub_take_keys(keys, pat0, [])
    {r_sub(sub, p: pat1), acc}
  end

  defp sub_take_keys([k | t], sub0, acc) do
    case :maps.take(k, sub0) do
      {0, sub1} ->
        sub_take_keys(t, sub1, acc)

      {name, sub1} ->
        sub_take_keys(t, sub1, [{k, name} | acc])
    end
  end

  defp sub_take_keys([], sub, acc) do
    {sub, acc}
  end

  defp sub_cache_nodes(kind, nodes, r_sub(p: pat) = sub) do
    case nodes_to_key(kind, nodes) do
      {:ok, key} ->
        case pat do
          %{^key => 0} ->
            new_var_name(key, sub)

          %{^key => name} ->
            {name, sub}

          %{} ->
            :error
        end

      :error ->
        :error
    end
  end

  defp new_var_name(key, r_sub(p: pat) = sub) do
    counter = :erlang.get(:new_var_num)
    name = :erlang.list_to_atom('@r' ++ :erlang.integer_to_list(counter))
    :erlang.put(:new_var_num, counter + 1)
    {name, r_sub(sub, p: :maps.put(key, name, pat))}
  end

  defp get_variables(nodesList) do
    :cerl_sets.from_list(
      for node <- nodesList,
          var <- :cerl_trees.variables(node) do
        var
      end
    )
  end

  defp is_disjoint_variables(vars1, vars2) do
    :cerl_sets.is_disjoint(vars1, vars2)
  end

  defp merge_variables(vars1, vars2) do
    :cerl_sets.union(vars1, vars2)
  end

  defp get_pattern_keys(patterns) do
    :lists.foldl(&get_pattern_keys/2, [], patterns)
  end

  defp get_pattern_keys(r_c_tuple(es: es), acc0) do
    acc1 = accumulate_pattern_keys(:tuple, es, acc0)
    :lists.foldl(&get_pattern_keys/2, acc1, es)
  end

  defp get_pattern_keys(r_c_cons(hd: hd, tl: tl), acc0) do
    acc1 = accumulate_pattern_keys(:cons, [hd, tl], acc0)
    get_pattern_keys(tl, get_pattern_keys(hd, acc1))
  end

  defp get_pattern_keys(r_c_alias(pat: pat), acc0) do
    get_pattern_keys(pat, acc0)
  end

  defp get_pattern_keys(r_c_map(es: es), acc0) do
    :lists.foldl(&get_pattern_keys/2, acc0, es)
  end

  defp get_pattern_keys(r_c_map_pair(val: val), acc0) do
    get_pattern_keys(val, acc0)
  end

  defp get_pattern_keys(_, acc) do
    acc
  end

  defp accumulate_pattern_keys(kind, nodes, acc) do
    case nodes_to_key(kind, nodes) do
      {:ok, key} ->
        [key | acc]

      :error ->
        acc
    end
  end

  defp put_pattern_keys(patterns, []) do
    patterns
  end

  defp put_pattern_keys(patterns, keys) do
    {newPatterns, map} = :lists.mapfoldl(&alias_pattern_keys/2, :maps.from_list(keys), patterns)
    0 = map_size(map)
    newPatterns
  end

  defp alias_pattern_keys(r_c_tuple(anno: anno, es: es0) = node, acc0) do
    {es1, acc1} = :lists.mapfoldl(&alias_pattern_keys/2, acc0, es0)
    nodes_to_alias(:tuple, es0, anno, r_c_tuple(node, es: es1), acc1)
  end

  defp alias_pattern_keys(r_c_cons(anno: anno, hd: hd0, tl: tl0) = node, acc0) do
    {hd1, acc1} = alias_pattern_keys(hd0, acc0)
    {tl1, acc2} = alias_pattern_keys(tl0, acc1)
    nodes_to_alias(:cons, [hd0, tl0], anno, r_c_cons(node, hd: hd1, tl: tl1), acc2)
  end

  defp alias_pattern_keys(r_c_alias(pat: pat0) = node, acc0) do
    {pat1, acc1} = alias_pattern_keys(pat0, acc0)
    {r_c_alias(node, pat: pat1), acc1}
  end

  defp alias_pattern_keys(r_c_map(es: es0) = node, acc0) do
    {es1, acc1} = :lists.mapfoldl(&alias_pattern_keys/2, acc0, es0)
    {r_c_map(node, es: es1), acc1}
  end

  defp alias_pattern_keys(r_c_map_pair(val: val0) = node, acc0) do
    {val1, acc1} = alias_pattern_keys(val0, acc0)
    {r_c_map_pair(node, val: val1), acc1}
  end

  defp alias_pattern_keys(pattern, acc) do
    {pattern, acc}
  end

  defp nodes_to_alias(kind, inner, anno, node, keys0) do
    case nodes_to_key(kind, inner) do
      {:ok, key} ->
        case :maps.take(key, keys0) do
          {name, keys1} ->
            var = :cerl.ann_c_var(anno, name)
            {:cerl.ann_c_alias(anno, var, node), keys1}

          :error ->
            {node, keys0}
        end

      :error ->
        {node, keys0}
    end
  end

  defp nodes_to_key(kind, nodes) do
    ntk_1(nodes, [], kind, 100)
  end

  defp ntk_1(_, _Acc, _Kind, 0) do
    :error
  end

  defp ntk_1([r_c_alias(var: var) | t], acc, kind, n) do
    ntk_1([var | t], acc, kind, n - 1)
  end

  defp ntk_1([r_c_var(name: name) | t], acc, kind, n) do
    ntk_1(t, [[:var, name] | acc], kind, n - 1)
  end

  defp ntk_1([node | t], acc0, kind, n) do
    case :cerl.is_data(node) do
      true ->
        case ntk_1(:cerl.data_es(node), [], :cerl.data_type(node), n - 1) do
          {:ok, key} ->
            ntk_1(t, [key | acc0], kind, n - 1)

          :error ->
            :error
        end

      false ->
        :error
    end
  end

  defp ntk_1([], acc, kind, _N) do
    {:ok, [kind | acc]}
  end
end
