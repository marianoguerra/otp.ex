defmodule :m_erl_syntax_lib do
  use Bitwise

  def map(f, tree) do
    case :erl_syntax.subtrees(tree) do
      [] ->
        f.(tree)

      gs ->
        tree1 =
          :erl_syntax.make_tree(
            :erl_syntax.type(tree),
            for g <- gs do
              for t <- g do
                map(f, t)
              end
            end
          )

        f.(:erl_syntax.copy_attrs(tree, tree1))
    end
  end

  def map_subtrees(f, tree) do
    case :erl_syntax.subtrees(tree) do
      [] ->
        tree

      gs ->
        tree1 =
          :erl_syntax.make_tree(
            :erl_syntax.type(tree),
            for g <- gs do
              for t <- g do
                f.(t)
              end
            end
          )

        :erl_syntax.copy_attrs(tree, tree1)
    end
  end

  def fold(f, s, tree) do
    case :erl_syntax.subtrees(tree) do
      [] ->
        f.(tree, s)

      gs ->
        f.(tree, fold_1(f, s, gs))
    end
  end

  defp fold_1(f, s, [l | ls]) do
    fold_1(f, fold_2(f, s, l), ls)
  end

  defp fold_1(_, s, []) do
    s
  end

  defp fold_2(f, s, [t | ts]) do
    fold_2(f, fold(f, s, t), ts)
  end

  defp fold_2(_, s, []) do
    s
  end

  def fold_subtrees(f, s, tree) do
    foldl_listlist(f, s, :erl_syntax.subtrees(tree))
  end

  def foldl_listlist(f, s, [l | ls]) do
    foldl_listlist(f, foldl(f, s, l), ls)
  end

  def foldl_listlist(_, s, []) do
    s
  end

  defp foldl(f, s, [t | ts]) do
    foldl(f, f.(t, s), ts)
  end

  defp foldl(_, s, []) do
    s
  end

  def mapfold(f, s, tree) do
    case :erl_syntax.subtrees(tree) do
      [] ->
        f.(tree, s)

      gs ->
        {gs1, s1} = mapfold_1(f, s, gs)

        tree1 =
          :erl_syntax.make_tree(
            :erl_syntax.type(tree),
            gs1
          )

        f.(:erl_syntax.copy_attrs(tree, tree1), s1)
    end
  end

  defp mapfold_1(f, s, [l | ls]) do
    {l1, s1} = mapfold_2(f, s, l)
    {ls1, s2} = mapfold_1(f, s1, ls)
    {[l1 | ls1], s2}
  end

  defp mapfold_1(_, s, []) do
    {[], s}
  end

  defp mapfold_2(f, s, [t | ts]) do
    {t1, s1} = mapfold(f, s, t)
    {ts1, s2} = mapfold_2(f, s1, ts)
    {[t1 | ts1], s2}
  end

  defp mapfold_2(_, s, []) do
    {[], s}
  end

  def mapfold_subtrees(f, s, tree) do
    case :erl_syntax.subtrees(tree) do
      [] ->
        {tree, s}

      gs ->
        {gs1, s1} = mapfoldl_listlist(f, s, gs)

        tree1 =
          :erl_syntax.make_tree(
            :erl_syntax.type(tree),
            gs1
          )

        {:erl_syntax.copy_attrs(tree, tree1), s1}
    end
  end

  def mapfoldl_listlist(f, s, [l | ls]) do
    {l1, s1} = mapfoldl(f, s, l)
    {ls1, s2} = mapfoldl_listlist(f, s1, ls)
    {[l1 | ls1], s2}
  end

  def mapfoldl_listlist(_, s, []) do
    {[], s}
  end

  defp mapfoldl(f, s, [l | ls]) do
    {l1, s1} = f.(l, s)
    {ls1, s2} = mapfoldl(f, s1, ls)
    {[l1 | ls1], s2}
  end

  defp mapfoldl(_, s, []) do
    {[], s}
  end

  def variables(tree) do
    variables(tree, :sets.new())
  end

  defp variables(t, s) do
    case :erl_syntax.type(t) do
      :variable ->
        :sets.add_element(:erl_syntax.variable_name(t), s)

      :macro ->
        case :erl_syntax.macro_arguments(t) do
          :none ->
            s

          as ->
            variables_2(as, s)
        end

      _ ->
        case :erl_syntax.subtrees(t) do
          [] ->
            s

          gs ->
            variables_1(gs, s)
        end
    end
  end

  defp variables_1([l | ls], s) do
    variables_1(ls, variables_2(l, s))
  end

  defp variables_1([], s) do
    s
  end

  defp variables_2([t | ts], s) do
    variables_2(ts, variables(t, s))
  end

  defp variables_2([], s) do
    s
  end

  defp default_variable_name(n) do
    :erlang.list_to_atom('V' ++ :erlang.integer_to_list(n))
  end

  def new_variable_name(s) do
    new_variable_name(&default_variable_name/1, s)
  end

  def new_variable_name(f, s) do
    r = start_range(s)
    new_variable_name(r, f, s)
  end

  defp new_variable_name(r, f, s) do
    new_variable_name(generate(r, r), r, 0, f, s)
  end

  defp new_variable_name(n, r, t, f, s) when t < 3 do
    a = f.(n)

    case :sets.is_element(a, s) do
      true ->
        new_variable_name(generate(n, r), r, t + 1, f, s)

      false ->
        a
    end
  end

  defp new_variable_name(n, r, _T, f, s) do
    r1 = div(r * 8, 1)
    new_variable_name(generate(n, r1), r1, 0, f, s)
  end

  defp start_range(s) do
    :erlang.max(:sets.size(s) * 100, 100)
  end

  defp generate(_Key, range) do
    _ =
      case :rand.export_seed() do
        :undefined ->
          :rand.seed(:exsplus, {753, 8, 73})

        _ ->
          :ok
      end

    :rand.uniform(range)
  end

  def new_variable_names(n, s) do
    new_variable_names(n, &default_variable_name/1, s)
  end

  def new_variable_names(n, f, s) when is_integer(n) do
    r = start_range(s)
    new_variable_names(n, [], r, f, s)
  end

  defp new_variable_names(n, names, r, f, s) when n > 0 do
    name = new_variable_name(r, f, s)
    s1 = :sets.add_element(name, s)
    new_variable_names(n - 1, [name | names], r, f, s1)
  end

  defp new_variable_names(0, names, _, _, _) do
    names
  end

  def annotate_bindings(tree, env) do
    {tree1, _, _} = vann(tree, env)
    tree1
  end

  def annotate_bindings(tree) do
    as = :erl_syntax.get_ann(tree)

    case :lists.keyfind(:env, 1, as) do
      {:env, inVars} ->
        annotate_bindings(tree, inVars)

      _ ->
        :erlang.error(:badarg)
    end
  end

  defp vann(tree, env) do
    case :erl_syntax.type(tree) do
      :variable ->
        bound = []
        free = [:erl_syntax.variable_name(tree)]
        {ann_bindings(tree, env, bound, free), bound, free}

      :match_expr ->
        vann_match_expr(tree, env)

      :case_expr ->
        vann_case_expr(tree, env)

      :if_expr ->
        vann_if_expr(tree, env)

      :receive_expr ->
        vann_receive_expr(tree, env)

      :catch_expr ->
        vann_catch_expr(tree, env)

      :try_expr ->
        vann_try_expr(tree, env)

      :function ->
        vann_function(tree, env)

      :fun_expr ->
        vann_fun_expr(tree, env)

      :list_comp ->
        vann_list_comp(tree, env)

      :binary_comp ->
        vann_binary_comp(tree, env)

      :generator ->
        vann_generator(tree, env)

      :binary_generator ->
        vann_binary_generator(tree, env)

      :block_expr ->
        vann_block_expr(tree, env)

      :macro ->
        vann_macro(tree, env)

      _Type ->
        f = vann_list_join(env)
        {tree1, {bound, free}} = mapfold_subtrees(f, {[], []}, tree)
        {ann_bindings(tree1, env, bound, free), bound, free}
    end
  end

  defp vann_list_join(env) do
    fn t, {bound, free} ->
      {t1, bound1, free1} = vann(t, env)
      {t1, {:ordsets.union(bound, bound1), :ordsets.union(free, free1)}}
    end
  end

  defp vann_list(ts, env) do
    :lists.mapfoldl(vann_list_join(env), {[], []}, ts)
  end

  defp vann_function(tree, env) do
    cs = :erl_syntax.function_clauses(tree)
    {cs1, {_, free}} = vann_clauses(cs, env)
    n = :erl_syntax.function_name(tree)
    {n1, _, _} = vann(n, env)
    tree1 = rewrite(tree, :erl_syntax.function(n1, cs1))
    bound = []
    {ann_bindings(tree1, env, bound, free), bound, free}
  end

  defp vann_fun_expr(tree, env) do
    cs = :erl_syntax.fun_expr_clauses(tree)
    {cs1, {_, free}} = vann_clauses(cs, env)
    tree1 = rewrite(tree, :erl_syntax.fun_expr(cs1))
    bound = []
    {ann_bindings(tree1, env, bound, free), bound, free}
  end

  defp vann_match_expr(tree, env) do
    e = :erl_syntax.match_expr_body(tree)
    {e1, bound1, free1} = vann(e, env)
    env1 = :ordsets.union(env, bound1)
    p = :erl_syntax.match_expr_pattern(tree)
    {p1, bound2, free2} = vann_pattern(p, env1)
    bound = :ordsets.union(bound1, bound2)
    free = :ordsets.union(free1, free2)
    tree1 = rewrite(tree, :erl_syntax.match_expr(p1, e1))
    {ann_bindings(tree1, env, bound, free), bound, free}
  end

  defp vann_case_expr(tree, env) do
    e = :erl_syntax.case_expr_argument(tree)
    {e1, bound1, free1} = vann(e, env)
    env1 = :ordsets.union(env, bound1)
    cs = :erl_syntax.case_expr_clauses(tree)
    {cs1, {bound2, free2}} = vann_clauses(cs, env1)
    bound = :ordsets.union(bound1, bound2)
    free = :ordsets.union(free1, free2)
    tree1 = rewrite(tree, :erl_syntax.case_expr(e1, cs1))
    {ann_bindings(tree1, env, bound, free), bound, free}
  end

  defp vann_if_expr(tree, env) do
    cs = :erl_syntax.if_expr_clauses(tree)
    {cs1, {bound, free}} = vann_clauses(cs, env)
    tree1 = rewrite(tree, :erl_syntax.if_expr(cs1))
    {ann_bindings(tree1, env, bound, free), bound, free}
  end

  defp vann_catch_expr(tree, env) do
    e = :erl_syntax.catch_expr_body(tree)
    {e1, _, free} = vann(e, env)
    tree1 = rewrite(tree, :erl_syntax.catch_expr(e1))
    bound = []
    {ann_bindings(tree1, env, bound, free), bound, free}
  end

  defp vann_try_expr(tree, env) do
    es = :erl_syntax.try_expr_body(tree)
    {es1, {bound1, free1}} = vann_body(es, env)
    cs = :erl_syntax.try_expr_clauses(tree)

    {cs1, {_, free2}} =
      vann_clauses(
        cs,
        :ordsets.union(env, bound1)
      )

    hs = :erl_syntax.try_expr_handlers(tree)
    {hs1, {_, free3}} = vann_clauses(hs, env)
    as = :erl_syntax.try_expr_after(tree)
    {as1, {_, free4}} = vann_body(as, env)

    tree1 =
      rewrite(
        tree,
        :erl_syntax.try_expr(es1, cs1, hs1, as1)
      )

    bound = []

    free =
      :ordsets.union(
        free1,
        :ordsets.union(free2, :ordsets.union(free3, free4))
      )

    {ann_bindings(tree1, env, bound, free), bound, free}
  end

  defp vann_receive_expr(tree, env) do
    cs = :erl_syntax.receive_expr_clauses(tree)
    es = :erl_syntax.receive_expr_action(tree)
    c = :erl_syntax.clause([], es)

    {[c1 | cs1], {bound, free1}} =
      vann_clauses(
        [c | cs],
        env
      )

    es1 = :erl_syntax.clause_body(c1)

    {t1, _, free2} =
      case :erl_syntax.receive_expr_timeout(tree) do
        :none ->
          {:none, [], []}

        t ->
          vann(t, env)
      end

    free = :ordsets.union(free1, free2)

    tree1 =
      rewrite(
        tree,
        :erl_syntax.receive_expr(cs1, t1, es1)
      )

    {ann_bindings(tree1, env, bound, free), bound, free}
  end

  defp vann_list_comp(tree, env) do
    es = :erl_syntax.list_comp_body(tree)
    {es1, {bound1, free1}} = vann_list_comp_body(es, env)
    env1 = :ordsets.union(env, bound1)
    t = :erl_syntax.list_comp_template(tree)
    {t1, _, free2} = vann(t, env1)

    free =
      :ordsets.union(
        free1,
        :ordsets.subtract(free2, bound1)
      )

    bound = []
    tree1 = rewrite(tree, :erl_syntax.list_comp(t1, es1))
    {ann_bindings(tree1, env, bound, free), bound, free}
  end

  defp vann_list_comp_body_join() do
    fn t, {env, bound, free} ->
      {t1, bound1, free1} =
        case :erl_syntax.type(t) do
          :binary_generator ->
            vann_binary_generator(t, env)

          :generator ->
            vann_generator(t, env)

          _ ->
            {t2, _, free2} = vann(t, env)
            {t2, [], free2}
        end

      env1 = :ordsets.union(env, bound1)

      {t1,
       {env1, :ordsets.union(bound, bound1),
        :ordsets.union(free, :ordsets.subtract(free1, bound))}}
    end
  end

  defp vann_list_comp_body(ts, env) do
    f = vann_list_comp_body_join()
    {ts1, {_, bound, free}} = :lists.mapfoldl(f, {env, [], []}, ts)
    {ts1, {bound, free}}
  end

  defp vann_binary_comp(tree, env) do
    es = :erl_syntax.binary_comp_body(tree)
    {es1, {bound1, free1}} = vann_binary_comp_body(es, env)
    env1 = :ordsets.union(env, bound1)
    t = :erl_syntax.binary_comp_template(tree)
    {t1, _, free2} = vann(t, env1)

    free =
      :ordsets.union(
        free1,
        :ordsets.subtract(free2, bound1)
      )

    bound = []
    tree1 = rewrite(tree, :erl_syntax.binary_comp(t1, es1))
    {ann_bindings(tree1, env, bound, free), bound, free}
  end

  defp vann_binary_comp_body_join() do
    fn t, {env, bound, free} ->
      {t1, bound1, free1} =
        case :erl_syntax.type(t) do
          :binary_generator ->
            vann_binary_generator(t, env)

          :generator ->
            vann_generator(t, env)

          _ ->
            {t2, _, free2} = vann(t, env)
            {t2, [], free2}
        end

      env1 = :ordsets.union(env, bound1)

      {t1,
       {env1, :ordsets.union(bound, bound1),
        :ordsets.union(free, :ordsets.subtract(free1, bound))}}
    end
  end

  defp vann_binary_comp_body(ts, env) do
    f = vann_binary_comp_body_join()
    {ts1, {_, bound, free}} = :lists.mapfoldl(f, {env, [], []}, ts)
    {ts1, {bound, free}}
  end

  defp vann_generator(tree, env) do
    p = :erl_syntax.generator_pattern(tree)
    {p1, bound, _} = vann_pattern(p, [])
    e = :erl_syntax.generator_body(tree)
    {e1, _, free} = vann(e, env)
    tree1 = rewrite(tree, :erl_syntax.generator(p1, e1))
    {ann_bindings(tree1, env, bound, free), bound, free}
  end

  defp vann_binary_generator(tree, env) do
    p = :erl_syntax.binary_generator_pattern(tree)
    {p1, bound, _} = vann_pattern(p, env)
    e = :erl_syntax.binary_generator_body(tree)
    {e1, _, free} = vann(e, env)

    tree1 =
      rewrite(
        tree,
        :erl_syntax.binary_generator(p1, e1)
      )

    {ann_bindings(tree1, env, bound, free), bound, free}
  end

  defp vann_block_expr(tree, env) do
    es = :erl_syntax.block_expr_body(tree)
    {es1, {bound, free}} = vann_body(es, env)
    tree1 = rewrite(tree, :erl_syntax.block_expr(es1))
    {ann_bindings(tree1, env, bound, free), bound, free}
  end

  defp vann_body_join() do
    fn t, {env, bound, free} ->
      {t1, bound1, free1} = vann(t, env)
      env1 = :ordsets.union(env, bound1)

      {t1,
       {env1, :ordsets.union(bound, bound1),
        :ordsets.union(free, :ordsets.subtract(free1, bound))}}
    end
  end

  defp vann_body(ts, env) do
    {ts1, {_, bound, free}} = :lists.mapfoldl(vann_body_join(), {env, [], []}, ts)
    {ts1, {bound, free}}
  end

  defp vann_macro(tree, env) do
    {as, {bound, free}} =
      case :erl_syntax.macro_arguments(tree) do
        :none ->
          {:none, {[], []}}

        as1 ->
          vann_list(as1, env)
      end

    n = :erl_syntax.macro_name(tree)
    tree1 = rewrite(tree, :erl_syntax.macro(n, as))
    {ann_bindings(tree1, env, bound, free), bound, free}
  end

  defp vann_pattern(tree, env) do
    case :erl_syntax.type(tree) do
      :variable ->
        v = :erl_syntax.variable_name(tree)

        case :ordsets.is_element(v, env) do
          true ->
            bound = []
            free = [v]
            {ann_bindings(tree, env, bound, free), bound, free}

          false ->
            bound = [v]
            free = []
            {ann_bindings(tree, env, bound, free), bound, free}
        end

      :match_expr ->
        p = :erl_syntax.match_expr_pattern(tree)
        {p1, bound1, free1} = vann_pattern(p, env)
        e = :erl_syntax.match_expr_body(tree)
        {e1, bound2, free2} = vann_pattern(e, env)
        bound = :ordsets.union(bound1, bound2)
        free = :ordsets.union(free1, free2)
        tree1 = rewrite(tree, :erl_syntax.match_expr(p1, e1))
        {ann_bindings(tree1, env, bound, free), bound, free}

      :macro ->
        {as, {bound, free}} =
          case :erl_syntax.macro_arguments(tree) do
            :none ->
              {:none, {[], []}}

            as1 ->
              vann_patterns(as1, env)
          end

        n = :erl_syntax.macro_name(tree)
        tree1 = rewrite(tree, :erl_syntax.macro(n, as))
        {ann_bindings(tree1, env, bound, free), bound, free}

      _Type ->
        f = vann_patterns_join(env)
        {tree1, {bound, free}} = mapfold_subtrees(f, {[], []}, tree)
        {ann_bindings(tree1, env, bound, free), bound, free}
    end
  end

  defp vann_patterns_join(env) do
    fn t, {bound, free} ->
      {t1, bound1, free1} = vann_pattern(t, env)
      {t1, {:ordsets.union(bound, bound1), :ordsets.union(free, free1)}}
    end
  end

  defp vann_patterns(ps, env) do
    :lists.mapfoldl(vann_patterns_join(env), {[], []}, ps)
  end

  defp vann_clause(c, env) do
    {ps, {bound1, free1}} =
      vann_patterns(
        :erl_syntax.clause_patterns(c),
        env
      )

    env1 = :ordsets.union(env, bound1)

    {g1, _, free2} =
      case :erl_syntax.clause_guard(c) do
        :none ->
          {:none, [], []}

        g ->
          vann(g, env1)
      end

    {es, {bound2, free3}} =
      vann_body(
        :erl_syntax.clause_body(c),
        env1
      )

    bound = :ordsets.union(bound1, bound2)

    free =
      :ordsets.union(
        free1,
        :ordsets.subtract(
          :ordsets.union(free2, free3),
          bound1
        )
      )

    tree1 = rewrite(c, :erl_syntax.clause(ps, g1, es))
    {ann_bindings(tree1, env, bound, free), bound, free}
  end

  defp vann_clauses_join(env) do
    fn c, {bound, free} ->
      {c1, bound1, free1} = vann_clause(c, env)
      {c1, {:ordsets.intersection(bound, bound1), :ordsets.union(free, free1)}}
    end
  end

  defp vann_clauses([c | cs], env) do
    {c1, bound, free} = vann_clause(c, env)
    {cs1, bF} = :lists.mapfoldl(vann_clauses_join(env), {bound, free}, cs)
    {[c1 | cs1], bF}
  end

  defp vann_clauses([], _Env) do
    {[], {[], []}}
  end

  defp ann_bindings(tree, env, bound, free) do
    as0 = :erl_syntax.get_ann(tree)

    as1 = [
      {:env, env},
      {:bound, bound},
      {:free, free}
      | delete_binding_anns(as0)
    ]

    :erl_syntax.set_ann(tree, as1)
  end

  defp delete_binding_anns([{:env, _} | as]) do
    delete_binding_anns(as)
  end

  defp delete_binding_anns([{:bound, _} | as]) do
    delete_binding_anns(as)
  end

  defp delete_binding_anns([{:free, _} | as]) do
    delete_binding_anns(as)
  end

  defp delete_binding_anns([a | as]) do
    [a | delete_binding_anns(as)]
  end

  defp delete_binding_anns([]) do
    []
  end

  def is_fail_expr(e) do
    case :erl_syntax.type(e) do
      :application ->
        n = length(:erl_syntax.application_arguments(e))
        f = :erl_syntax.application_operator(e)

        case (try do
                {:ok, analyze_function_name(f)}
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          :syntax_error ->
            false

          {:ok, :exit} when n === 1 ->
            true

          {:ok, :throw} when n === 1 ->
            true

          {:ok, {:erlang, :exit}} when n === 1 ->
            true

          {:ok, {:erlang, :throw}} when n === 1 ->
            true

          {:ok, {:erlang, :error}} when n === 1 ->
            true

          {:ok, {:erlang, :error}} when n === 2 ->
            true

          {:ok, {:erlang, :fault}} when n === 1 ->
            true

          {:ok, {:erlang, :fault}} when n === 2 ->
            true

          _ ->
            false
        end

      _ ->
        false
    end
  end

  def analyze_forms(forms) when is_list(forms) do
    finfo_to_list(:lists.foldl(&collect_form/2, new_finfo(), forms))
  end

  def analyze_forms(forms) do
    analyze_forms(:erl_syntax.form_list_elements(:erl_syntax.flatten_form_list(forms)))
  end

  defp collect_form(node, info) do
    case analyze_form(node) do
      {:attribute, {name, data}} ->
        collect_attribute(name, data, info)

      {:attribute, :preprocessor} ->
        info

      {:function, name} ->
        finfo_add_function(name, info)

      {:error_marker, data} ->
        finfo_add_error(data, info)

      {:warning_marker, data} ->
        finfo_add_warning(data, info)

      _ ->
        info
    end
  end

  defp collect_attribute(:module, m, info) do
    finfo_set_module(m, info)
  end

  defp collect_attribute(:export, l, info) do
    finfo_add_exports(l, info)
  end

  defp collect_attribute(:import, {m, l}, info) do
    finfo_add_imports(m, l, info)
  end

  defp collect_attribute(:import, m, info) do
    finfo_add_module_import(m, info)
  end

  defp collect_attribute(:file, _, info) do
    info
  end

  defp collect_attribute(:record, {r, l}, info) do
    finfo_add_record(r, l, info)
  end

  defp collect_attribute(_, {n, v}, info) do
    finfo_add_attribute(n, v, info)
  end

  require Record

  Record.defrecord(:r_forms, :forms,
    module: :none,
    exports: [],
    module_imports: [],
    imports: [],
    attributes: [],
    records: [],
    errors: [],
    warnings: [],
    functions: []
  )

  defp new_finfo() do
    r_forms()
  end

  defp finfo_set_module(name, info) do
    case r_forms(info, :module) do
      :none ->
        r_forms(info, module: {:value, name})

      {:value, _} ->
        info
    end
  end

  defp finfo_add_exports(l, info) do
    r_forms(info, exports: l ++ r_forms(info, :exports))
  end

  defp finfo_add_module_import(m, info) do
    r_forms(info, module_imports: [m | r_forms(info, :module_imports)])
  end

  defp finfo_add_imports(m, l, info) do
    es = r_forms(info, :imports)

    case :lists.keyfind(m, 1, es) do
      {_, l1} ->
        es1 = :lists.keyreplace(m, 1, es, {m, l ++ l1})
        r_forms(info, imports: es1)

      false ->
        r_forms(info, imports: [{m, l} | es])
    end
  end

  defp finfo_add_attribute(name, val, info) do
    r_forms(info,
      attributes: [
        {name, val}
        | r_forms(info, :attributes)
      ]
    )
  end

  defp finfo_add_record(r, l, info) do
    r_forms(info, records: [{r, l} | r_forms(info, :records)])
  end

  defp finfo_add_error(r, info) do
    r_forms(info, errors: [r | r_forms(info, :errors)])
  end

  defp finfo_add_warning(r, info) do
    r_forms(info, warnings: [r | r_forms(info, :warnings)])
  end

  defp finfo_add_function(f, info) do
    r_forms(info, functions: [f | r_forms(info, :functions)])
  end

  defp finfo_to_list(info) do
    for {key, {:value, value}} <- [
          {:module, r_forms(info, :module)},
          {:exports, list_value(r_forms(info, :exports))},
          {:imports, list_value(r_forms(info, :imports))},
          {:module_imports, list_value(r_forms(info, :module_imports))},
          {:attributes, list_value(r_forms(info, :attributes))},
          {:records, list_value(r_forms(info, :records))},
          {:errors, list_value(r_forms(info, :errors))},
          {:warnings, list_value(r_forms(info, :warnings))},
          {:functions, list_value(r_forms(info, :functions))}
        ] do
      {key, value}
    end
  end

  defp list_value([]) do
    :none
  end

  defp list_value(list) do
    {:value, list}
  end

  def analyze_form(node) do
    case :erl_syntax.type(node) do
      :attribute ->
        {:attribute, analyze_attribute(node)}

      :function ->
        {:function, analyze_function(node)}

      :error_marker ->
        {:error_marker, :erl_syntax.error_marker_info(node)}

      :warning_marker ->
        {:warning_marker, :erl_syntax.warning_marker_info(node)}

      _ ->
        case :erl_syntax.is_form(node) do
          true ->
            :erl_syntax.type(node)

          false ->
            throw(:syntax_error)
        end
    end
  end

  def analyze_attribute(node) do
    name = :erl_syntax.attribute_name(node)

    case :erl_syntax.type(name) do
      :atom ->
        case :erl_syntax.atom_value(name) do
          :define ->
            :preprocessor

          :undef ->
            :preprocessor

          :include ->
            :preprocessor

          :include_lib ->
            :preprocessor

          :ifdef ->
            :preprocessor

          :ifndef ->
            :preprocessor

          :if ->
            :preprocessor

          :elif ->
            :preprocessor

          :else ->
            :preprocessor

          :endif ->
            :preprocessor

          a ->
            {a, analyze_attribute(a, node)}
        end

      _ ->
        throw(:syntax_error)
    end
  end

  defp analyze_attribute(:module, node) do
    analyze_module_attribute(node)
  end

  defp analyze_attribute(:export, node) do
    analyze_export_attribute(node)
  end

  defp analyze_attribute(:import, node) do
    analyze_import_attribute(node)
  end

  defp analyze_attribute(:file, node) do
    analyze_file_attribute(node)
  end

  defp analyze_attribute(:record, node) do
    analyze_record_attribute(node)
  end

  defp analyze_attribute(_, node) do
    analyze_wild_attribute(node)
  end

  def analyze_module_attribute(node) do
    case :erl_syntax.type(node) do
      :attribute ->
        case :erl_syntax.attribute_arguments(node) do
          [m] ->
            module_name_to_atom(m)

          [m, l] ->
            m1 = module_name_to_atom(m)
            l1 = analyze_variable_list(l)
            {m1, l1}

          _ ->
            throw(:syntax_error)
        end

      _ ->
        throw(:syntax_error)
    end
  end

  defp analyze_variable_list(node) do
    case :erl_syntax.is_proper_list(node) do
      true ->
        for v <- :erl_syntax.list_elements(node) do
          :erl_syntax.variable_name(v)
        end

      false ->
        throw(:syntax_error)
    end
  end

  def analyze_export_attribute(node) do
    case :erl_syntax.type(node) do
      :attribute ->
        case :erl_syntax.attribute_arguments(node) do
          [l] ->
            analyze_function_name_list(l)

          _ ->
            throw(:syntax_error)
        end

      _ ->
        throw(:syntax_error)
    end
  end

  defp analyze_function_name_list(node) do
    case :erl_syntax.is_proper_list(node) do
      true ->
        for f <- :erl_syntax.list_elements(node) do
          analyze_function_name(f)
        end

      false ->
        throw(:syntax_error)
    end
  end

  def analyze_function_name(node) do
    case :erl_syntax.type(node) do
      :atom ->
        :erl_syntax.atom_value(node)

      :arity_qualifier ->
        a = :erl_syntax.arity_qualifier_argument(node)

        case :erl_syntax.type(a) do
          :integer ->
            f = :erl_syntax.arity_qualifier_body(node)
            f1 = analyze_function_name(f)
            append_arity(:erl_syntax.integer_value(a), f1)

          _ ->
            throw(:syntax_error)
        end

      :module_qualifier ->
        m = :erl_syntax.module_qualifier_argument(node)

        case :erl_syntax.type(m) do
          :atom ->
            f = :erl_syntax.module_qualifier_body(node)
            f1 = analyze_function_name(f)
            {:erl_syntax.atom_value(m), f1}

          _ ->
            throw(:syntax_error)
        end

      _ ->
        throw(:syntax_error)
    end
  end

  defp append_arity(a, {module, name}) do
    {module, append_arity(a, name)}
  end

  defp append_arity(a, name) when is_atom(name) do
    {name, a}
  end

  defp append_arity(a, a) do
    a
  end

  defp append_arity(_A, name) do
    name
  end

  def analyze_import_attribute(node) do
    case :erl_syntax.type(node) do
      :attribute ->
        case :erl_syntax.attribute_arguments(node) do
          [m] ->
            module_name_to_atom(m)

          [m, l] ->
            m1 = module_name_to_atom(m)
            l1 = analyze_function_name_list(l)
            {m1, l1}

          _ ->
            throw(:syntax_error)
        end

      _ ->
        throw(:syntax_error)
    end
  end

  def analyze_type_name(node) do
    case :erl_syntax.type(node) do
      :atom ->
        :erl_syntax.atom_value(node)

      :arity_qualifier ->
        a = :erl_syntax.arity_qualifier_argument(node)
        n = :erl_syntax.arity_qualifier_body(node)

        case :erlang.and(
               :erl_syntax.type(a) === :integer,
               :erl_syntax.type(n) === :atom
             ) do
          true ->
            append_arity(
              :erl_syntax.integer_value(a),
              :erl_syntax.atom_value(n)
            )

          _ ->
            throw(:syntax_error)
        end

      :module_qualifier ->
        m = :erl_syntax.module_qualifier_argument(node)

        case :erl_syntax.type(m) do
          :atom ->
            n = :erl_syntax.module_qualifier_body(node)
            n1 = analyze_type_name(n)
            {:erl_syntax.atom_value(m), n1}

          _ ->
            throw(:syntax_error)
        end

      _ ->
        throw(:syntax_error)
    end
  end

  def analyze_wild_attribute(node) do
    case :erl_syntax.type(node) do
      :attribute ->
        n = :erl_syntax.attribute_name(node)

        case :erl_syntax.type(n) do
          :atom ->
            case :erl_syntax.attribute_arguments(node) do
              [v] ->
                case (try do
                        {:ok, :erl_syntax.concrete(v)}
                      catch
                        :error, e -> {:EXIT, {e, __STACKTRACE__}}
                        :exit, e -> {:EXIT, e}
                        e -> e
                      end) do
                  {:ok, val} ->
                    {:erl_syntax.atom_value(n), val}

                  _ ->
                    throw(:syntax_error)
                end

              _ ->
                throw(:syntax_error)
            end

          _ ->
            throw(:syntax_error)
        end

      _ ->
        throw(:syntax_error)
    end
  end

  def analyze_record_attribute(node) do
    case :erl_syntax.type(node) do
      :attribute ->
        case :erl_syntax.attribute_arguments(node) do
          [r, t] ->
            case :erl_syntax.type(r) do
              :atom ->
                es = analyze_record_attribute_tuple(t)
                {:erl_syntax.atom_value(r), es}

              _ ->
                throw(:syntax_error)
            end

          _ ->
            throw(:syntax_error)
        end

      _ ->
        throw(:syntax_error)
    end
  end

  defp analyze_record_attribute_tuple(node) do
    case :erl_syntax.type(node) do
      :tuple ->
        for f <- :erl_syntax.tuple_elements(node) do
          analyze_record_field(f)
        end

      _ ->
        throw(:syntax_error)
    end
  end

  def analyze_record_expr(node) do
    case :erl_syntax.type(node) do
      :record_expr ->
        a = :erl_syntax.record_expr_type(node)

        case :erl_syntax.type(a) do
          :atom ->
            fs0 =
              for f <- :erl_syntax.record_expr_fields(node) do
                analyze_record_field(f)
              end

            fs =
              for {n, {d, _T}} <- fs0 do
                {n, d}
              end

            {:record_expr, {:erl_syntax.atom_value(a), fs}}

          _ ->
            throw(:syntax_error)
        end

      :record_access ->
        f = :erl_syntax.record_access_field(node)

        case :erl_syntax.type(f) do
          :atom ->
            a = :erl_syntax.record_access_type(node)

            case :erl_syntax.type(a) do
              :atom ->
                {:record_access, {:erl_syntax.atom_value(a), :erl_syntax.atom_value(f)}}

              _ ->
                throw(:syntax_error)
            end

          _ ->
            throw(:syntax_error)
        end

      :record_index_expr ->
        f = :erl_syntax.record_index_expr_field(node)

        case :erl_syntax.type(f) do
          :atom ->
            a = :erl_syntax.record_index_expr_type(node)

            case :erl_syntax.type(a) do
              :atom ->
                {:record_index_expr, {:erl_syntax.atom_value(a), :erl_syntax.atom_value(f)}}

              _ ->
                throw(:syntax_error)
            end

          _ ->
            throw(:syntax_error)
        end

      type ->
        type
    end
  end

  def analyze_record_field(node) do
    case :erl_syntax.type(node) do
      :record_field ->
        a = :erl_syntax.record_field_name(node)

        case :erl_syntax.type(a) do
          :atom ->
            t = :erl_syntax.record_field_value(node)
            {:erl_syntax.atom_value(a), {t, :none}}

          _ ->
            throw(:syntax_error)
        end

      :typed_record_field ->
        f = :erl_syntax.typed_record_field_body(node)
        {n, {v, _none}} = analyze_record_field(f)
        t = :erl_syntax.typed_record_field_type(node)
        {n, {v, t}}

      _ ->
        throw(:syntax_error)
    end
  end

  def analyze_file_attribute(node) do
    case :erl_syntax.type(node) do
      :attribute ->
        case :erl_syntax.attribute_arguments(node) do
          [f, n] ->
            case :erlang.and(
                   :erl_syntax.type(f) === :string,
                   :erl_syntax.type(n) === :integer
                 ) do
              true ->
                {:erl_syntax.string_value(f), :erl_syntax.integer_value(n)}

              false ->
                throw(:syntax_error)
            end

          _ ->
            throw(:syntax_error)
        end

      _ ->
        throw(:syntax_error)
    end
  end

  def analyze_function(node) do
    case :erl_syntax.type(node) do
      :function ->
        n = :erl_syntax.function_name(node)

        case :erl_syntax.type(n) do
          :atom ->
            {:erl_syntax.atom_value(n), :erl_syntax.function_arity(node)}

          _ ->
            throw(:syntax_error)
        end

      _ ->
        throw(:syntax_error)
    end
  end

  def analyze_implicit_fun(node) do
    case :erl_syntax.type(node) do
      :implicit_fun ->
        analyze_function_name(:erl_syntax.implicit_fun_name(node))

      _ ->
        throw(:syntax_error)
    end
  end

  def analyze_application(node) do
    case :erl_syntax.type(node) do
      :application ->
        a = length(:erl_syntax.application_arguments(node))
        f = :erl_syntax.application_operator(node)

        case (try do
                {:ok, analyze_function_name(f)}
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          :syntax_error ->
            a

          {:ok, n} ->
            append_arity(a, n)

          _ ->
            throw(:syntax_error)
        end

      _ ->
        throw(:syntax_error)
    end
  end

  def analyze_type_application(node) do
    case :erl_syntax.type(node) do
      :type_application ->
        a = length(:erl_syntax.type_application_arguments(node))
        n = :erl_syntax.type_application_name(node)

        case (try do
                {:ok, analyze_type_name(n)}
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:ok, typeName} ->
            append_arity(a, typeName)

          _ ->
            throw(:syntax_error)
        end

      :user_type_application ->
        a = length(:erl_syntax.user_type_application_arguments(node))
        n = :erl_syntax.user_type_application_name(node)

        case (try do
                {:ok, analyze_type_name(n)}
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:ok, typeName} ->
            append_arity(a, typeName)

          _ ->
            throw(:syntax_error)
        end

      _ ->
        throw(:syntax_error)
    end
  end

  def function_name_expansions(fs) do
    function_name_expansions(fs, [])
  end

  defp function_name_expansions([f | fs], ack) do
    function_name_expansions(
      fs,
      function_name_expansions(f, f, ack)
    )
  end

  defp function_name_expansions([], ack) do
    ack
  end

  defp function_name_expansions({a, n}, name, ack) when is_integer(n) do
    [{{a, n}, name} | ack]
  end

  defp function_name_expansions({_, n}, name, ack) do
    function_name_expansions(n, name, ack)
  end

  defp function_name_expansions(a, name, ack) do
    [{a, name} | ack]
  end

  def strip_comments(tree) do
    map(&strip_comments_1/1, tree)
  end

  defp strip_comments_1(t) do
    case :erl_syntax.type(t) do
      :form_list ->
        es = :erl_syntax.form_list_elements(t)

        es1 =
          for e <- es, :erl_syntax.type(e) != :comment do
            e
          end

        t1 =
          :erl_syntax.copy_attrs(
            t,
            :erl_syntax.form_list(es1)
          )

        :erl_syntax.remove_comments(t1)

      :comment ->
        :erl_syntax.comment([])

      _ ->
        :erl_syntax.remove_comments(t)
    end
  end

  def to_comment(tree) do
    to_comment(tree, '% ')
  end

  def to_comment(tree, prefix) do
    f = fn t ->
      :erl_prettypr.format(t)
    end

    to_comment(tree, prefix, f)
  end

  def to_comment(tree, prefix, f) do
    :erl_syntax.comment(split_lines(f.(tree), prefix))
  end

  def limit(tree, depth) do
    limit(tree, depth, :erl_syntax.text('...'))
  end

  def limit(_Tree, depth, node) when depth < 0 do
    node
  end

  def limit(tree, depth, node) do
    limit_1(tree, depth, node)
  end

  defp limit_1(tree, depth, node) do
    case :erl_syntax.subtrees(tree) do
      [] ->
        cond do
          depth > 0 ->
            tree

          true ->
            case is_simple_leaf(tree) do
              true ->
                tree

              false ->
                node
            end
        end

      gs ->
        cond do
          depth > 1 ->
            gs1 =
              for g <- gs do
                for t <- limit_list(g, depth, node) do
                  limit_1(t, depth - 1, node)
                end
              end

            rewrite(
              tree,
              :erl_syntax.make_tree(:erl_syntax.type(tree), gs1)
            )

          depth === 0 ->
            node

          true ->
            gs1 =
              for g <- gs do
                cut_group(g, node)
              end

            rewrite(
              tree,
              :erl_syntax.make_tree(:erl_syntax.type(tree), gs1)
            )
        end
    end
  end

  defp cut_group([], _Node) do
    []
  end

  defp cut_group([t], node) do
    [limit_1(t, 0, node)]
  end

  defp cut_group(_Ts, node) do
    [node]
  end

  defp is_simple_leaf(tree) do
    case :erl_syntax.type(tree) do
      :atom ->
        true

      :char ->
        true

      :float ->
        true

      :integer ->
        true

      nil ->
        true

      :operator ->
        true

      :tuple ->
        true

      :underscore ->
        true

      :variable ->
        true

      _ ->
        false
    end
  end

  defp limit_list(ts, n, node) do
    cond do
      length(ts) > n ->
        limit_list_1(ts, n - 1, node)

      true ->
        ts
    end
  end

  defp limit_list_1([t | ts], n, node) do
    cond do
      n > 0 ->
        [t | limit_list_1(ts, n - 1, node)]

      true ->
        [node]
    end
  end

  defp limit_list_1([], _N, _Node) do
    []
  end

  defp rewrite(tree, tree1) do
    :erl_syntax.copy_attrs(tree, tree1)
  end

  defp module_name_to_atom(m) do
    case :erl_syntax.type(m) do
      :atom ->
        :erl_syntax.atom_value(m)

      _ ->
        throw(:syntax_error)
    end
  end

  defp split_lines(cs, prefix) do
    split_lines(cs, prefix, 0)
  end

  defp split_lines(cs, prefix, n) do
    :lists.reverse(split_lines(cs, n, [], [], prefix))
  end

  defp split_lines([?\r, ?\n | cs], _N, cs1, ls, prefix) do
    split_lines_1(cs, cs1, ls, prefix)
  end

  defp split_lines([?\r | cs], _N, cs1, ls, prefix) do
    split_lines_1(cs, cs1, ls, prefix)
  end

  defp split_lines([?\n | cs], _N, cs1, ls, prefix) do
    split_lines_1(cs, cs1, ls, prefix)
  end

  defp split_lines([?\t | cs], n, cs1, ls, prefix) do
    split_lines(cs, 0, push(8 - rem(n, 8), ?\s, cs1), ls, prefix)
  end

  defp split_lines([c | cs], n, cs1, ls, prefix) do
    split_lines(cs, n + 1, [c | cs1], ls, prefix)
  end

  defp split_lines([], _, [], ls, _) do
    ls
  end

  defp split_lines([], _N, cs, ls, prefix) do
    [prefix ++ :lists.reverse(cs) | ls]
  end

  defp split_lines_1(cs, cs1, ls, prefix) do
    split_lines(cs, 0, [], [prefix ++ :lists.reverse(cs1) | ls], prefix)
  end

  defp push(n, c, cs) when n > 0 do
    push(n - 1, c, [c | cs])
  end

  defp push(0, _, cs) do
    cs
  end
end
