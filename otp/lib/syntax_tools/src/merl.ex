defmodule :m_merl do
  use Bitwise

  def compile(code) do
    compile(code, [])
  end

  def compile(code, options) when not is_list(code) do
    case type(code) do
      :form_list ->
        compile(:erl_syntax.form_list_elements(code))

      _ ->
        compile([code], options)
    end
  end

  def compile(code, options0) when is_list(options0) do
    forms =
      for f <- code do
        :erl_syntax.revert(f)
      end

    options = [
      [:verbose, :report_errors, :report_warnings, :binary]
      | options0
    ]

    :compile.noenv_forms(forms, options)
  end

  def compile_and_load(code) do
    compile_and_load(code, [])
  end

  def compile_and_load(code, options) do
    case compile(code, options) do
      {:ok, moduleName, binary} ->
        _ = :code.load_binary(moduleName, '', binary)
        {:ok, binary}

      other ->
        other
    end
  end

  def var(name) do
    :erl_syntax.variable(name)
  end

  def term(term) do
    :erl_syntax.abstract(term)
  end

  def print(ts) when is_list(ts) do
    :lists.foreach(&print/1, ts)
  end

  def print(t) do
    :io.put_chars(:erl_prettypr.format(tree(t)))
    :io.nl()
  end

  def show(ts) when is_list(ts) do
    :lists.foreach(&show/1, ts)
  end

  def show(t) do
    :io.put_chars(pp(tree(t), 0))
    :io.nl()
  end

  defp pp(t, i) do
    [
      :lists.duplicate(i, ?\s),
      limit(
        :lists.flatten([
          :erlang.atom_to_list(type(t)),
          ': ',
          :erl_prettypr.format(
            :erl_syntax_lib.limit(
              t,
              3
            )
          )
        ]),
        79 - i
      ),
      ?\n,
      pp_1(
        :lists.filter(
          fn x ->
            x !== []
          end,
          subtrees(t)
        ),
        i + 2
      )
    ]
  end

  defp pp_1([g], i) do
    pp_2(g, i)
  end

  defp pp_1([g | gs], i) do
    [
      [pp_2(g, i), :lists.duplicate(i, ?\s), '+\n']
      | pp_1(
          gs,
          i
        )
    ]
  end

  defp pp_1([], _I) do
    []
  end

  defp pp_2(g, i) do
    for e <- g do
      pp(e, i)
    end
  end

  defp limit([?\n | cs], n) do
    limit([?\s | cs], n)
  end

  defp limit([?\r | cs], n) do
    limit([?\s | cs], n)
  end

  defp limit([?\v | cs], n) do
    limit([?\s | cs], n)
  end

  defp limit([?\t | cs], n) do
    limit([?\s | cs], n)
  end

  defp limit([[?\s, ?\s] | cs], n) do
    limit([?\s | cs], n)
  end

  defp limit([c | cs], n) when c < 32 do
    limit(cs, n)
  end

  defp limit([c | cs], n) when n > 3 do
    [c | limit(cs, n - 1)]
  end

  defp limit([[_C1, _C2, _C3, _C4] | _Cs], 3) do
    '...'
  end

  defp limit(cs, 3) do
    cs
  end

  defp limit([[_C1, _C2, _C3] | _], 2) do
    '..'
  end

  defp limit(cs, 2) do
    cs
  end

  defp limit([[_C1, _C2] | _], 1) do
    '.'
  end

  defp limit(cs, 1) do
    cs
  end

  defp limit(_, _) do
    []
  end

  def qquote(text, env) do
    qquote(1, text, env)
  end

  def qquote(startPos, text, env) do
    subst(quote(startPos, text), env)
  end

  def quote(text) do
    quote(1, text)
  end

  def quote({line, col}, text)
      when is_integer(line) and
             is_integer(col) do
    quote_1(line, col, text)
  end

  def quote(startPos, text) when is_integer(startPos) do
    quote_1(startPos, :undefined, text)
  end

  defp quote_1(startLine, startCol, text) do
    startPos =
      case :erlang.system_info(:version) do
        '5.6' ++ _ ->
          startLine

        '5.7' ++ _ ->
          startLine

        '5.8' ++ _ ->
          startLine

        _ when startCol === :undefined ->
          startLine

        _ ->
          {startLine, startCol}
      end

    flatText = flatten_text(text)
    {:ok, ts, _} = :erl_scan.string(flatText, startPos)
    merge_comments(startLine, :erl_comment_scan.string(flatText), parse_1(ts))
  end

  defp parse_1(ts) do
    case split_forms(ts) do
      {:ok, fs} ->
        parse_forms(fs)

      :error ->
        parse_2(ts)
    end
  end

  defp split_forms(ts) do
    split_forms(ts, [], [])
  end

  defp split_forms([{:dot, _} = t | ts], fs, as) do
    split_forms(ts, [:lists.reverse(as, [t]) | fs], [])
  end

  defp split_forms([t | ts], fs, as) do
    split_forms(ts, fs, [t | as])
  end

  defp split_forms([], fs, []) do
    {:ok, :lists.reverse(fs)}
  end

  defp split_forms([], [], _) do
    :error
  end

  defp split_forms([], _, [t | _]) do
    fail('incomplete form after ~p', [t])
  end

  defp parse_forms([ts | tss]) do
    case :erl_parse.parse_form(ts) do
      {:ok, form} ->
        [form | parse_forms(tss)]

      {:error, r} ->
        parse_error(r)
    end
  end

  defp parse_forms([]) do
    []
  end

  defp parse_2(ts) do
    a = a0()

    case :erl_parse.parse_exprs(ts ++ [{:dot, a}]) do
      {:ok, exprs} ->
        exprs

      {:error, e} ->
        parse_3(ts ++ [{:end, a}, {:dot, a}], [e])
    end
  end

  defp parse_3(ts, es) do
    a = a0()

    case :erl_parse.parse_exprs([
           [{:try, a}, {:atom, a, true}, {:catch, a}]
           | ts
         ]) do
      {:ok, [{:try, _, _, _, _, _} = x]} ->
        :erl_syntax.try_expr_handlers(x)

      {:error, e} ->
        parse_4(ts, [e | es])
    end
  end

  defp parse_4(ts, es) do
    a = a0()

    case :erl_parse.parse_exprs([{:fun, a} | ts]) do
      {:ok, [{:fun, _, {:clauses, cs}}]} ->
        cs

      {:error, e} ->
        parse_5(ts, [e | es])
    end
  end

  defp parse_5(ts, es) do
    a = a0()

    case :erl_parse.parse_exprs([
           [{:case, a}, {:atom, a, true}, {:of, a}]
           | ts
         ]) do
      {:ok, [{:case, _, _, cs}]} ->
        cs

      {:error, e} ->
        parse_error(:lists.last(:lists.sort([e | es])))
    end
  end

  defp parse_error({l, m, r})
       when is_atom(m) and
              is_integer(l) do
    fail('~w: ~ts', [l, m.format_error(r)])
  end

  defp parse_error({{l, c}, m, r})
       when is_atom(m) and
              is_integer(l) and is_integer(c) do
    fail('~w:~w: ~ts', [l, c, m.format_error(r)])
  end

  defp parse_error({_, m, r}) when is_atom(m) do
    fail(m.format_error(r))
  end

  defp parse_error(r) do
    fail('unknown parse error: ~tp', [r])
  end

  def template(trees) when is_list(trees) do
    for t <- trees do
      template_0(t)
    end
  end

  def template(tree) do
    template_0(tree)
  end

  defp template_0({:template, _, _, _} = template) do
    template
  end

  defp template_0({:*, _} = template) do
    template
  end

  defp template_0({_} = template) do
    template
  end

  defp template_0(tree) do
    case template_1(tree) do
      false ->
        tree

      {name} when is_list(name) ->
        fail('bad metavariable: \'~s\'', [tl(name)])

      template ->
        template
    end
  end

  defp template_1(tree) do
    case subtrees(tree) do
      [] ->
        case metavar(tree) do
          {'v_' ++ cs} = v when cs !== [] ->
            v

          {'n0' ++ cs} = v when cs !== [] ->
            v

          {'v@' ++ cs} when cs !== [] ->
            {:*, :erlang.list_to_atom(cs)}

          {'n9' ++ cs} when cs !== [] ->
            {:*, :erlang.list_to_integer(cs)}

          {'v' ++ cs} ->
            {:erlang.list_to_atom(cs)}

          {'n' ++ cs} ->
            {:erlang.list_to_integer(cs)}

          false ->
            false
        end

      gs ->
        case template_2(gs, [], false) do
          gs1 when is_list(gs1) ->
            {:template, type(tree), :erl_syntax.get_attrs(tree), gs1}

          other ->
            other
        end
    end
  end

  defp template_2([g | gs], as, bool) do
    case template_3(g, [], false) do
      {'v_' ++ cs} = v when cs !== [] ->
        v

      {'n0' ++ cs} = v when cs !== [] ->
        v

      {'v@' ++ cs} when cs !== [] ->
        {:*, :erlang.list_to_atom(cs)}

      {'n9' ++ cs} when cs !== [] ->
        {:*, :erlang.list_to_integer(cs)}

      {'v' ++ cs} when is_list(cs) ->
        {:erlang.list_to_atom(cs)}

      {'n' ++ cs} when is_list(cs) ->
        {:erlang.list_to_integer(cs)}

      false ->
        template_2(gs, [g | as], bool)

      g1 ->
        template_2(gs, [g1 | as], true)
    end
  end

  defp template_2([], _As, false) do
    false
  end

  defp template_2([], as, true) do
    :lists.reverse(as)
  end

  defp template_3([t | ts], as, bool) do
    case template_1(t) do
      {'v_' ++ cs} when cs !== [] ->
        {'v' ++ cs}

      {'n0' ++ cs} when cs !== [] ->
        {'n' ++ cs}

      false ->
        template_3(ts, [t | as], bool)

      t1 ->
        template_3(ts, [t1 | as], true)
    end
  end

  defp template_3([], _As, false) do
    false
  end

  defp template_3([], as, true) do
    :lists.reverse(as)
  end

  def meta_template(templates) when is_list(templates) do
    for t <- templates do
      meta_template_1(t)
    end
  end

  def meta_template(template) do
    meta_template_1(template)
  end

  defp meta_template_1({:template, type, attrs, groups}) do
    :erl_syntax.tuple([
      :erl_syntax.atom(:template),
      :erl_syntax.atom(type),
      :erl_syntax.abstract(attrs),
      :erl_syntax.list(
        for g <- groups do
          :erl_syntax.list(
            for t <- g do
              meta_template_1(t)
            end
          )
        end
      )
    ])
  end

  defp meta_template_1({var} = v) do
    meta_template_2(var, v)
  end

  defp meta_template_1({:*, var} = v) do
    meta_template_2(var, v)
  end

  defp meta_template_1(leaf) do
    :erl_syntax.abstract(leaf)
  end

  defp meta_template_2(var, v) when is_atom(var) do
    case :erlang.atom_to_list(var) do
      [c | _] = name
      when (c >= ?A and c <= ?Z) or
             (c >= 195 and c <= 195 and c != 195) ->
        case :lists.reverse(name) do
          '@' ++ ([_ | _] = revRealName) ->
            realName = :lists.reverse(revRealName)

            :erl_syntax.application(
              :erl_syntax.atom(:merl),
              :erl_syntax.atom(:term),
              [:erl_syntax.variable(realName)]
            )

          _ ->
            :erl_syntax.variable(name)
        end

      _ ->
        :erl_syntax.abstract(v)
    end
  end

  defp meta_template_2(var, v) when is_integer(var) do
    cond do
      var > 9 and rem(var, 10) === 9 ->
        cond do
          var > 99 and rem(var, 100) === 99 ->
            name = 'Q' ++ :erlang.integer_to_list(div(var, 100))

            :erl_syntax.application(
              :erl_syntax.atom(:merl),
              :erl_syntax.atom(:term),
              [:erl_syntax.variable(name)]
            )

          true ->
            name = :erlang.integer_to_list(div(var, 10))
            :erl_syntax.variable('Q' ++ name)
        end

      true ->
        :erl_syntax.abstract(v)
    end
  end

  def template_vars(template) do
    template_vars(template, [])
  end

  defp template_vars(templates, vars) when is_list(templates) do
    :lists.foldl(&template_vars_1/2, vars, templates)
  end

  defp template_vars(template, vars) do
    template_vars_1(template, vars)
  end

  defp template_vars_1({:template, _, _, groups}, vars) do
    :lists.foldl(
      fn g, v ->
        :lists.foldl(&template_vars_1/2, v, g)
      end,
      vars,
      groups
    )
  end

  defp template_vars_1({var}, vars) do
    :ordsets.add_element(var, vars)
  end

  defp template_vars_1({:*, var}, vars) do
    :ordsets.add_element(var, vars)
  end

  defp template_vars_1(_, vars) do
    vars
  end

  def tree(templates) when is_list(templates) do
    for t <- templates do
      tree_1(t)
    end
  end

  def tree(template) do
    tree_1(template)
  end

  defp tree_1({:template, type, attrs, groups}) do
    gs =
      for g <- groups do
        :lists.flatten(
          for t <- g do
            tree_1(t)
          end
        )
      end

    :erl_syntax.set_attrs(make_tree(type, gs), attrs)
  end

  defp tree_1({var}) when is_atom(var) do
    :erl_syntax.atom(:erlang.list_to_atom('@' ++ :erlang.atom_to_list(var)))
  end

  defp tree_1({var}) when is_integer(var) do
    :erl_syntax.integer(:erlang.list_to_integer('909' ++ :erlang.integer_to_list(var)))
  end

  defp tree_1({:*, var}) when is_atom(var) do
    :erl_syntax.atom(:erlang.list_to_atom('@@' ++ :erlang.atom_to_list(var)))
  end

  defp tree_1({:*, var}) when is_integer(var) do
    :erl_syntax.integer(:erlang.list_to_integer('9099' ++ :erlang.integer_to_list(var)))
  end

  defp tree_1(leaf) do
    leaf
  end

  def subst(trees, env) when is_list(trees) do
    for t <- trees do
      subst_0(t, env)
    end
  end

  def subst(tree, env) do
    subst_0(tree, env)
  end

  defp subst_0(tree, env) do
    tree_1(subst_1(template(tree), env))
  end

  def tsubst(trees, env) when is_list(trees) do
    for t <- trees do
      subst_1(template(t), env)
    end
  end

  def tsubst(tree, env) do
    subst_1(template(tree), env)
  end

  defp subst_1({:template, type, attrs, groups}, env) do
    gs1 =
      for g <- groups do
        :lists.flatten(
          for t <- g do
            subst_1(t, env)
          end
        )
      end

    {:template, type, attrs, gs1}
  end

  defp subst_1({var} = v, env) do
    case :lists.keyfind(var, 1, env) do
      {^var, treeOrTrees} ->
        treeOrTrees

      false ->
        v
    end
  end

  defp subst_1({:*, var} = v, env) do
    case :lists.keyfind(var, 1, env) do
      {^var, treeOrTrees} ->
        treeOrTrees

      false ->
        v
    end
  end

  defp subst_1(leaf, _Env) do
    leaf
  end

  def alpha(trees, env) when is_list(trees) do
    for t <- trees do
      alpha_1(template(t), env)
    end
  end

  def alpha(tree, env) do
    alpha_1(template(tree), env)
  end

  defp alpha_1({:template, type, attrs, groups}, env) do
    gs1 =
      for g <- groups do
        :lists.flatten(
          for t <- g do
            alpha_1(t, env)
          end
        )
      end

    {:template, type, attrs, gs1}
  end

  defp alpha_1({var} = v, env) do
    case :lists.keyfind(var, 1, env) do
      {^var, newVar} ->
        {newVar}

      false ->
        v
    end
  end

  defp alpha_1({:*, var} = v, env) do
    case :lists.keyfind(var, 1, env) do
      {^var, newVar} ->
        {:*, newVar}

      false ->
        v
    end
  end

  defp alpha_1(leaf, _Env) do
    leaf
  end

  def match(patterns, trees)
      when is_list(patterns) and
             is_list(trees) do
    try do
      {:ok, match_1(patterns, trees, [])}
    catch
      :error ->
        :error
    end
  end

  def match(patterns, tree) when is_list(patterns) do
    match(patterns, [tree])
  end

  def match(pattern, trees) when is_list(trees) do
    match([pattern], trees)
  end

  def match(pattern, tree) do
    try do
      {:ok, match_template(template(pattern), tree, [])}
    catch
      :error ->
        :error
    end
  end

  defp match_1([p | ps], [t | ts], dict) do
    match_1(ps, ts, match_template(template(p), t, dict))
  end

  defp match_1([], [], dict) do
    dict
  end

  defp match_1(_, _, _Dict) do
    :erlang.error(:merl_match_arity)
  end

  defp match_template({:template, type, _, gs}, tree, dict) do
    case type(tree) do
      ^type ->
        match_template_1(gs, subtrees(tree), dict)

      _ ->
        throw(:error)
    end
  end

  defp match_template({var}, _Tree, dict)
       when var === :_ or
              var === 0 do
    dict
  end

  defp match_template({var}, tree, dict) do
    :orddict.store(var, tree, dict)
  end

  defp match_template(tree1, tree2, dict) do
    case compare_trees(tree1, tree2) do
      true ->
        dict

      false ->
        throw(:error)
    end
  end

  defp match_template_1([g1 | gs1], [g2 | gs2], dict) do
    match_template_2(g1, g2, match_template_1(gs1, gs2, dict))
  end

  defp match_template_1([], [], dict) do
    dict
  end

  defp match_template_1(_, _, _Dict) do
    throw(:error)
  end

  defp match_template_2([{var} | ts1], [_ | ts2], dict)
       when var === :_ or var === 0 do
    match_template_2(ts1, ts2, dict)
  end

  defp match_template_2([{var} | ts1], [tree | ts2], dict) do
    match_template_2(ts1, ts2, :orddict.store(var, tree, dict))
  end

  defp match_template_2([{:*, var} | ts1], ts2, dict) do
    match_glob(:lists.reverse(ts1), :lists.reverse(ts2), var, dict)
  end

  defp match_template_2([t1 | ts1], [t2 | ts2], dict) do
    match_template_2(ts1, ts2, match_template(t1, t2, dict))
  end

  defp match_template_2([], [], dict) do
    dict
  end

  defp match_template_2(_, _, _Dict) do
    throw(:error)
  end

  defp match_glob([{:*, var} | _], _, _, _) do
    fail('multiple glob variables in same match group: ~w', [var])
  end

  defp match_glob([t1 | ts1], [t2 | ts2], var, dict) do
    match_glob(ts1, ts2, var, match_template(t1, t2, dict))
  end

  defp match_glob([], _Group, var, dict)
       when var === :_ or
              var === 0 do
    dict
  end

  defp match_glob([], group, var, dict) do
    :orddict.store(var, :lists.reverse(group), dict)
  end

  defp match_glob(_, _, _, _Dict) do
    throw(:error)
  end

  defp compare_trees(t1, t2) do
    type1 = type(t1)

    case type(t2) do
      ^type1 ->
        case subtrees(t1) do
          [] ->
            case subtrees(t2) do
              [] ->
                compare_leaves(type1, t1, t2)

              _Gs2 ->
                false
            end

          gs1 ->
            case subtrees(t2) do
              [] ->
                false

              gs2 ->
                compare_trees_1(gs1, gs2)
            end
        end

      _Type2 ->
        false
    end
  end

  defp compare_trees_1([g1 | gs1], [g2 | gs2]) do
    compare_trees_2(g1, g2) and compare_trees_1(gs1, gs2)
  end

  defp compare_trees_1([], []) do
    true
  end

  defp compare_trees_1(_, _) do
    false
  end

  defp compare_trees_2([t1 | ts1], [t2 | ts2]) do
    compare_trees(t1, t2) and compare_trees_2(ts1, ts2)
  end

  defp compare_trees_2([], []) do
    true
  end

  defp compare_trees_2(_, _) do
    false
  end

  defp compare_leaves(type, t1, t2) do
    case type do
      :atom ->
        :erl_syntax.atom_value(t1) === :erl_syntax.atom_value(t2)

      :char ->
        :erl_syntax.char_value(t1) === :erl_syntax.char_value(t2)

      :float ->
        :erl_syntax.float_value(t1) === :erl_syntax.float_value(t2)

      :integer ->
        :erl_syntax.integer_value(t1) === :erl_syntax.integer_value(t2)

      :string ->
        :erl_syntax.string_value(t1) === :erl_syntax.string_value(t2)

      :operator ->
        :erl_syntax.operator_name(t1) === :erl_syntax.operator_name(t2)

      :text ->
        :erl_syntax.text_string(t1) === :erl_syntax.text_string(t2)

      :variable ->
        :erl_syntax.variable_name(t1) === :erl_syntax.variable_name(t2)

      _ ->
        true
    end
  end

  def switch(trees, [{patterns, guardedActions} | cs])
      when is_list(guardedActions) do
    switch_1(trees, patterns, guardedActions, cs)
  end

  def switch(trees, [{patterns, guardedAction} | cs]) do
    switch_1(trees, patterns, [guardedAction], cs)
  end

  def switch(trees, [{patterns, guard, action} | cs]) do
    switch_1(trees, patterns, [{guard, action}], cs)
  end

  def switch(_Trees, [default | _Cs])
      when is_function(default, 0) do
    default.()
  end

  def switch(_Trees, []) do
    :erlang.error(:merl_switch_clause)
  end

  def switch(_Tree, _) do
    :erlang.error(:merl_switch_badarg)
  end

  defp switch_1(trees, patterns, guardedActions, cs) do
    case match(patterns, trees) do
      {:ok, env} ->
        switch_2(env, guardedActions, trees, cs)

      :error ->
        switch(trees, cs)
    end
  end

  defp switch_2(env, [{guard, action} | bs], trees, cs)
       when is_function(guard, 1) and
              is_function(action, 1) do
    case guard.(env) do
      true ->
        action.(env)

      false ->
        switch_2(env, bs, trees, cs)
    end
  end

  defp switch_2(env, [action | _Bs], _Trees, _Cs)
       when is_function(action, 1) do
    action.(env)
  end

  defp switch_2(_Env, [], trees, cs) do
    switch(trees, cs)
  end

  defp switch_2(_Env, _, _Trees, _Cs) do
    :erlang.error(:merl_switch_badarg)
  end

  defp fail(text) do
    fail(text, [])
  end

  defp fail(fs, as) do
    throw({:error, :lists.flatten(:io_lib.format(fs, as))})
  end

  defp flatten_text([l | _] = lines) when is_list(l) do
    :lists.foldr(
      fn s, t ->
        s ++ [?\n | t]
      end,
      '',
      lines
    )
  end

  defp flatten_text([b | _] = lines) when is_binary(b) do
    :lists.foldr(
      fn s, t ->
        :erlang.binary_to_list(s) ++ [?\n | t]
      end,
      '',
      lines
    )
  end

  defp flatten_text(text) when is_binary(text) do
    :erlang.binary_to_list(text)
  end

  defp flatten_text(text) do
    text
  end

  defp metavar(tree) do
    case type(tree) do
      :atom ->
        case :erl_syntax.atom_name(tree) do
          '@' ++ cs when cs !== [] ->
            {'v' ++ cs}

          _ ->
            false
        end

      :variable ->
        case :erl_syntax.variable_literal(tree) do
          '_@' ++ cs when cs !== [] ->
            {'v' ++ cs}

          _ ->
            false
        end

      :integer ->
        case :erl_syntax.integer_value(tree) do
          n when n >= 9090 ->
            case :erlang.integer_to_list(n) do
              '909' ++ cs ->
                {'n' ++ cs}

              _ ->
                false
            end

          _ ->
            false
        end

      :string ->
        case :erl_syntax.string_value(tree) do
          '\'@' ++ cs ->
            {'v' ++ cs}

          _ ->
            false
        end

      _ ->
        false
    end
  end

  defp type(t) do
    case :erl_syntax.type(t) do
      nil ->
        :list

      type ->
        type
    end
  end

  defp subtrees(t) do
    case :erl_syntax.type(t) do
      :tuple ->
        [:erl_syntax.tuple_elements(t)]

      nil ->
        [[], []]

      :list ->
        case :erl_syntax.list_suffix(t) do
          :none ->
            [:erl_syntax.list_prefix(t), []]

          s ->
            [:erl_syntax.list_prefix(t), [s]]
        end

      :binary_field ->
        [[:erl_syntax.binary_field_body(t)], :erl_syntax.binary_field_types(t)]

      :clause ->
        case :erl_syntax.clause_guard(t) do
          :none ->
            [:erl_syntax.clause_patterns(t), [], :erl_syntax.clause_body(t)]

          g ->
            [:erl_syntax.clause_patterns(t), [g], :erl_syntax.clause_body(t)]
        end

      :receive_expr ->
        case :erl_syntax.receive_expr_timeout(t) do
          :none ->
            [:erl_syntax.receive_expr_clauses(t), [], []]

          e ->
            [:erl_syntax.receive_expr_clauses(t), [e], :erl_syntax.receive_expr_action(t)]
        end

      :record_expr ->
        case :erl_syntax.record_expr_argument(t) do
          :none ->
            [[], [:erl_syntax.record_expr_type(t)], :erl_syntax.record_expr_fields(t)]

          v ->
            [[v], [:erl_syntax.record_expr_type(t)], :erl_syntax.record_expr_fields(t)]
        end

      :record_field ->
        case :erl_syntax.record_field_value(t) do
          :none ->
            [[:erl_syntax.record_field_name(t)], []]

          v ->
            [[:erl_syntax.record_field_name(t)], [v]]
        end

      _ ->
        :erl_syntax.subtrees(t)
    end
  end

  defp make_tree(:list, [p, []]) do
    :erl_syntax.list(p)
  end

  defp make_tree(:list, [p, [s]]) do
    :erl_syntax.list(p, s)
  end

  defp make_tree(:tuple, [e]) do
    :erl_syntax.tuple(e)
  end

  defp make_tree(:binary_field, [[b], ts]) do
    :erl_syntax.binary_field(b, ts)
  end

  defp make_tree(:clause, [p, [], b]) do
    :erl_syntax.clause(p, :none, b)
  end

  defp make_tree(:clause, [p, [g], b]) do
    :erl_syntax.clause(p, g, b)
  end

  defp make_tree(:receive_expr, [c, [], _A]) do
    :erl_syntax.receive_expr(c)
  end

  defp make_tree(:receive_expr, [c, [e], a]) do
    :erl_syntax.receive_expr(c, e, a)
  end

  defp make_tree(:record_expr, [[], [t], f]) do
    :erl_syntax.record_expr(t, f)
  end

  defp make_tree(:record_expr, [[e], [t], f]) do
    :erl_syntax.record_expr(e, t, f)
  end

  defp make_tree(:record_field, [[n], []]) do
    :erl_syntax.record_field(n)
  end

  defp make_tree(:record_field, [[n], [e]]) do
    :erl_syntax.record_field(n, e)
  end

  defp make_tree(type, groups) do
    :erl_syntax.make_tree(type, groups)
  end

  defp merge_comments(_StartLine, [], [t]) do
    t
  end

  defp merge_comments(_StartLine, [], ts) do
    ts
  end

  defp merge_comments(startLine, comments, ts) do
    merge_comments(startLine, comments, ts, [])
  end

  defp merge_comments(_StartLine, [], [], [t]) do
    t
  end

  defp merge_comments(_StartLine, [], [t], []) do
    t
  end

  defp merge_comments(_StartLine, [], ts, acc) do
    :lists.reverse(acc, ts)
  end

  defp merge_comments(startLine, cs, [], acc) do
    merge_comments(
      startLine,
      [],
      [],
      for {line, _, indent, text} <- cs do
        :erl_syntax.set_pos(
          :erl_syntax.comment(indent, text),
          anno(startLine + line - 1)
        )
      end ++ acc
    )
  end

  defp merge_comments(startLine, [c | cs], [t | ts], acc) do
    {line, _Col, indent, text} = c
    commentLine = startLine + line - 1

    case :erl_syntax.get_pos(t) do
      pos when pos < commentLine ->
        merge_comments(startLine, [c | cs], ts, [t | acc])

      ^commentLine ->
        tc =
          :erl_syntax.add_postcomments(
            [
              :erl_syntax.comment(
                indent,
                text
              )
            ],
            t
          )

        merge_comments(startLine, cs, [tc | ts], acc)

      _ ->
        tc =
          :erl_syntax.add_precomments(
            [
              :erl_syntax.comment(
                indent,
                text
              )
            ],
            t
          )

        merge_comments(startLine, cs, [tc | ts], acc)
    end
  end

  defp a0() do
    anno(0)
  end

  defp anno(location) do
    :erl_anno.new(location)
  end
end
