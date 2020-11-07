defmodule :m_edoc_extract do
  use Bitwise
  import :edoc_report, only: [report: 3, warning: 3]
  require Record
  Record.defrecord(:r_context, :context, dir: '', env: :undefined, opts: [])
  Record.defrecord(:r_doclet_gen, :doclet_gen, sources: [], app: [], modules: [])

  Record.defrecord(:r_doclet_toc, :doclet_toc,
    paths: :undefined,
    indir: :undefined
  )

  Record.defrecord(:r_module, :module,
    name: [],
    parameters: :none,
    functions: [],
    exports: [],
    attributes: [],
    records: [],
    encoding: :latin1
  )

  Record.defrecord(:r_env, :env,
    module: [],
    root: '',
    file_suffix: :undefined,
    apps: :undefined,
    modules: :undefined,
    app_default: :undefined,
    macros: [],
    includes: []
  )

  Record.defrecord(:r_comment, :comment,
    line: 0,
    text: :undefined
  )

  Record.defrecord(:r_entry, :entry,
    name: :undefined,
    args: [],
    line: 0,
    export: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_tag, :tag, name: :undefined, line: 0, origin: :comment, data: :undefined)

  def source(file, env, opts) do
    forms = :edoc.read_source(file, opts)
    comments = :edoc.read_comments(file, opts)
    source(forms, comments, file, env, opts)
  end

  def source(forms, comments, file, env, opts)
      when is_list(forms) do
    forms1 = :erl_syntax.form_list(forms)
    source(forms1, comments, file, env, opts)
  end

  def source(forms, comments, file, env, opts) do
    tree =
      :erl_recomment.quick_recomment_forms(
        forms,
        comments
      )

    typeDocs = find_type_docs(forms, comments, env, file)
    source1(tree, file, env, opts, typeDocs)
  end

  def source(forms, file, env, opts) when is_list(forms) do
    source(:erl_syntax.form_list(forms), file, env, opts)
  end

  def source(tree, file0, env, opts) do
    typeDocs = find_type_docs(tree, [], env, file0)
    source1(tree, file0, env, opts, typeDocs)
  end

  defp source1(tree, file0, env, opts, typeDocs) do
    forms = preprocess_forms(tree)
    file = :edoc_lib.filename(file0)
    module = get_module_info(tree, file)
    {header, footer, entries} = collect(forms, module)
    name = r_module(module, :name)
    env1 = r_env(env, module: name, root: '')
    env2 = add_macro_defs(module_macros(env1), opts, env1)
    entries1 = get_tags([[header, footer] | entries], env2, file, typeDocs)
    entries2 = :edoc_specs.add_data(entries1, opts, file, module)
    :edoc_tags.check_types(entries2, opts, file)
    data = :edoc_data.module(module, entries2, env2, opts)
    {name, data}
  end

  def header(file, env, opts) do
    forms = :edoc.read_source(file)
    comments = :edoc.read_comments(file)
    header(forms, comments, file, env, opts)
  end

  def header(forms, comments, file, env, opts)
      when is_list(forms) do
    forms1 = :erl_syntax.form_list(forms)
    header(forms1, comments, file, env, opts)
  end

  def header(forms, comments, file, env, opts) do
    tree =
      :erl_recomment.quick_recomment_forms(
        forms,
        comments
      )

    header(tree, file, env, opts)
  end

  def header(forms, file, env, opts) when is_list(forms) do
    header(:erl_syntax.form_list(forms), file, env, opts)
  end

  def header(tree, file0, env, _Opts) do
    forms = preprocess_forms(tree)
    file = :edoc_lib.filename(file0)
    module = r_module(name: r_env(env, :module))
    {header, footer, entries} = collect(forms, module)

    cond do
      r_entry(header, :data) != {[], [], []} ->
        warning(file, 'documentation before module declaration is ignored by @headerfile', [])

      true ->
        :ok
    end

    cond do
      entries != [] ->
        warning(file, 'documentation before function definitions is ignored by @headerfile', [])

      true ->
        :ok
    end

    [entry] = get_tags([r_entry(footer, name: :header)], env, file)
    r_entry(entry, :data)
  end

  defp add_macro_defs(defs0, opts, env) do
    defs = :proplists.append_values(:def, opts)
    :edoc_macros.check_defs(defs)
    r_env(env, macros: defs ++ defs0 ++ r_env(env, :macros))
  end

  def file(file, context, env, opts) do
    case :file.read_file(file) do
      {:ok, bin} ->
        enc =
          :edoc_lib.read_encoding(
            file,
            [{:in_comment_only, false}]
          )

        case (try do
                :unicode.characters_to_list(bin, enc)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          string when is_list(string) ->
            {:ok, text(string, context, env, opts, file)}

          _ ->
            {:error, :invalid_unicode}
        end

      {:error, _} = error ->
        error
    end
  end

  def text(text, context, env, opts) do
    text(text, context, env, opts, '')
  end

  defp text(text, context, env, opts, where) do
    env1 = add_macro_defs(file_macros(context, env), opts, env)
    cs = :edoc_lib.lines(text)
    ts0 = :edoc_tags.scan_lines(cs, 1)
    tags = :sets.from_list(:edoc_tags.tag_names())
    ts1 = :edoc_tags.filter_tags(ts0, tags, where)
    single = :sets.from_list(:edoc_tags.tags(:single))
    allow = :sets.from_list(:edoc_tags.tags(context))

    case :edoc_tags.check_tags(ts1, allow, single, where) do
      true ->
        exit(:error)

      false ->
        ts2 = :edoc_macros.expand_tags(ts1, env1, where)
        how = :dict.from_list(:edoc_tags.tag_parsers())
        :edoc_tags.parse_tags(ts2, how, env1, where)
    end
  end

  defp get_module_info(forms, file) do
    l =
      case (try do
              {:ok, :erl_syntax_lib.analyze_forms(forms)}
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:ok, l1} ->
          l1

        :syntax_error ->
          report(file, 'syntax error in input.', [])
          exit(:error)

        {:EXIT, r} ->
          exit(r)

        r ->
          throw(r)
      end

    {name, vars} =
      case :lists.keyfind(:module, 1, l) do
        {:module, n} when is_atom(n) ->
          {n, :none}

        {:module, {n, _} = mod} when is_atom(n) ->
          mod

        _ ->
          report(file, 'module name missing.', [])
          exit(:error)
      end

    functions =
      :ordsets.from_list(
        get_list_keyval(
          :functions,
          l
        )
      )

    exports =
      :ordsets.from_list(
        get_list_keyval(
          :exports,
          l
        )
      )

    attributes =
      :ordsets.from_list(
        get_list_keyval(
          :attributes,
          l
        )
      )

    records = get_list_keyval(:records, l)
    encoding = :edoc_lib.read_encoding(file, [])

    r_module(
      name: name,
      parameters: vars,
      functions: functions,
      exports: :ordsets.intersection(exports, functions),
      attributes: attributes,
      records: records,
      encoding: encoding
    )
  end

  defp get_list_keyval(key, l) do
    case :lists.keyfind(key, 1, l) do
      {^key, as} ->
        :ordsets.from_list(as)

      _ ->
        []
    end
  end

  defp preprocess_forms(tree) do
    preprocess_forms_1(:erl_syntax.form_list_elements(:erl_syntax.flatten_form_list(tree)))
  end

  defp preprocess_forms_1([f | fs]) do
    case :erl_syntax.get_precomments(f) do
      [] ->
        preprocess_forms_2(f, fs)

      cs ->
        cs ++ preprocess_forms_2(f, fs)
    end
  end

  defp preprocess_forms_1([]) do
    []
  end

  defp preprocess_forms_2(f, fs) do
    case :erl_syntax_lib.analyze_form(f) do
      :comment ->
        [f | preprocess_forms_1(fs)]

      {:function, _} ->
        [f | preprocess_forms_1(fs)]

      {:attribute, {:module, _}} ->
        [f | preprocess_forms_1(fs)]

      :text ->
        [f | preprocess_forms_1(fs)]

      {:attribute, {:record, _}} ->
        [f | preprocess_forms_1(fs)]

      {:attribute, {n, _}} ->
        case :edoc_specs.is_tag(n) do
          true ->
            [f | preprocess_forms_1(fs)]

          false ->
            preprocess_forms_1(fs)
        end

      _ ->
        preprocess_forms_1(fs)
    end
  end

  defp collect(fs, mod) do
    collect(fs, [], [], [], [], [], :undefined, mod)
  end

  defp collect([f | fs], cs, ss, ts, rs, as, header, mod) do
    case :erl_syntax_lib.analyze_form(f) do
      :comment ->
        collect(fs, [f | cs], ss, ts, rs, as, header, mod)

      {:function, name} ->
        l = :erl_syntax.get_pos(f)
        export = :ordsets.is_element(name, r_module(mod, :exports))
        args = parameters(:erl_syntax.function_clauses(f))

        collect(
          fs,
          [],
          [],
          [],
          [],
          [
            r_entry(
              name: name,
              args: args,
              line: l,
              export: export,
              data: {comment_text(cs), ss, ts, rs}
            )
            | as
          ],
          header,
          mod
        )

      {:attribute, {:module, _}} when header === :undefined ->
        l = :erl_syntax.get_pos(f)

        collect(
          fs,
          [],
          [],
          [],
          [],
          as,
          r_entry(name: :module, line: l, data: {comment_text(cs), ss, ts, rs}),
          mod
        )

      {:attribute, {:record, {_Name, fields}}} ->
        case is_typed_record(fields) do
          true ->
            collect(fs, cs, ss, ts, [f | rs], as, header, mod)

          false ->
            collect(fs, cs, ss, ts, rs, as, header, mod)
        end

      {:attribute, {n, _}} ->
        case :edoc_specs.tag(n) do
          :spec ->
            collect(fs, cs, [f | ss], ts, rs, as, header, mod)

          :type ->
            collect(fs, cs, ss, [f | ts], rs, as, header, mod)

          :unknown ->
            collect(fs, [], [], [], rs, as, header, mod)
        end

      _ ->
        collect(fs, [], [], [], [], as, header, mod)
    end
  end

  defp collect([], cs, ss, ts, rs, as, header, _Mod) do
    footer =
      r_entry(
        name: :footer,
        data: {comment_text(cs), ss, ts, rs}
      )

    as1 = :lists.reverse(as)

    cond do
      header === :undefined ->
        {r_entry(name: :module, data: {[], [], [], []}), footer, as1}

      true ->
        {header, footer, as1}
    end
  end

  defp is_typed_record([]) do
    false
  end

  defp is_typed_record([{_, {_, type}} | fs]) do
    type !== :none or is_typed_record(fs)
  end

  defp comment_text(cs) do
    comment_text(cs, [])
  end

  defp comment_text([c | cs], ss) do
    l = :erl_syntax.get_pos(c)

    comment_text(
      cs,
      [
        r_comment(
          line: l,
          text:
            for s <- :erl_syntax.comment_text(c) do
              remove_percent_chars(s)
            end
        )
        | ss
      ]
    )
  end

  defp comment_text([], ss) do
    ss
  end

  defp remove_percent_chars([?% | cs]) do
    [?\s | remove_percent_chars(cs)]
  end

  defp remove_percent_chars(cs) do
    cs
  end

  defp parameters(clauses) do
    select_names(
      for ps <- patterns(clauses) do
        find_names(ps)
      end
    )
  end

  defp patterns(cs) do
    :edoc_lib.transpose(
      for c <- cs do
        :erl_syntax.clause_patterns(c)
      end
    )
  end

  defp find_names(ps) do
    find_names(ps, [])
  end

  defp find_names([p | ps], ns) do
    case :erl_syntax.type(p) do
      :variable ->
        find_names(
          ps,
          [tidy_name(:erl_syntax.variable_name(p)) | ns]
        )

      :match_expr ->
        p1 = :erl_syntax.match_expr_pattern(p)
        p2 = :erl_syntax.match_expr_body(p)
        find_names([[p1, p2] | ps], ns)

      :list ->
        p1 = :erl_syntax.list_tail(p)
        find_names([p1 | ps], ns)

      :record_expr ->
        a = :erl_syntax.record_expr_type(p)
        atomName = :erl_syntax.atom_name(a)
        atom = :erlang.list_to_atom(atomName)

        case atomName === :lists.flatten(:io_lib.write_atom(atom)) do
          true ->
            n = :erlang.list_to_atom(capitalize(atomName))
            find_names(ps, [n | ns])

          false ->
            find_names(ps, ns)
        end

      :infix_expr ->
        p1 = :erl_syntax.infix_expr_right(p)
        find_names([p1 | ps], ns)

      _ ->
        find_names(ps, ns)
    end
  end

  defp find_names([], ns) do
    :lists.reverse(ns)
  end

  defp select_names(ls) do
    select_names(ls, [], :sets.new())
  end

  defp select_names([ns | ls], as, s) do
    a = select_name(ns, s)
    select_names(ls, [a | as], :sets.add_element(a, s))
  end

  defp select_names([], as, _) do
    :lists.reverse(as)
  end

  defp select_name([a | ns], s) do
    case :sets.is_element(a, s) do
      true ->
        select_name(ns, s)

      false ->
        a
    end
  end

  defp select_name([], _S) do
    :_
  end

  defp tidy_name(a) do
    case :erlang.atom_to_list(a) do
      [?_ | cs] ->
        :erlang.list_to_atom(tidy_name_1(cs))

      _ ->
        a
    end
  end

  defp tidy_name_1([?_ | cs]) do
    tidy_name_1(cs)
  end

  defp tidy_name_1([c | _] = cs) when c >= ?A and c <= ?Z do
    cs
  end

  defp tidy_name_1([c | _] = cs)
       when c >= 192 and c <= 222 and
              c !== 215 do
    cs
  end

  defp tidy_name_1(cs) do
    [?_ | cs]
  end

  defp capitalize([c | cs]) when c >= ?a and c <= ?z do
    [c - 32 | cs]
  end

  defp capitalize([c | cs])
       when c >= 224 and c <= 254 and
              c != 247 do
    [c - 32 | cs]
  end

  defp capitalize(cs) do
    cs
  end

  Record.defrecord(:r_tags, :tags,
    names: :undefined,
    single: :undefined,
    module: :undefined,
    function: :undefined,
    footer: :undefined
  )

  defp get_tags(es, env, file) do
    get_tags(es, env, file, :dict.new())
  end

  defp get_tags(es, env, file, typeDocs) do
    tags =
      r_tags(
        names: :sets.from_list(:edoc_tags.tag_names()),
        single: :sets.from_list(:edoc_tags.tags(:single)),
        module: :sets.from_list(:edoc_tags.tags(:module)),
        footer: :sets.from_list(:edoc_tags.tags(:footer)),
        function: :sets.from_list(:edoc_tags.tags(:function))
      )

    how = :dict.from_list(:edoc_tags.tag_parsers())
    get_tags(es, tags, env, how, file, typeDocs)
  end

  defp get_tags(
         [
           r_entry(
             name: name,
             data: {cs, specs, types, records}
           ) = e
           | es
         ],
         tags,
         env,
         how,
         file,
         typeDocs
       ) do
    where = {file, name}
    ts0 = scan_tags(cs)
    {ts1, specs1} = select_spec(ts0, where, specs)
    ts2 = check_tags(ts1, tags, where)
    ts3 = :edoc_macros.expand_tags(ts2, env, where)
    ts4 = :edoc_tags.parse_tags(ts3, how, env, where)
    ts = selected_specs(specs1, ts4)

    eTypes =
      for type <- types ++ records do
        :edoc_specs.type(type, typeDocs)
      end

    [r_entry(e, data: ts ++ eTypes) | get_tags(es, tags, env, how, file, typeDocs)]
  end

  defp get_tags([], _, _, _, _, _) do
    []
  end

  defp scan_tags([r_comment(line: l, text: ss) | es]) do
    :edoc_tags.scan_lines(ss, l) ++ scan_tags(es)
  end

  defp scan_tags([]) do
    []
  end

  defp check_tags(ts0, tags, where) do
    ts = :edoc_tags.filter_tags(ts0, r_tags(tags, :names), where)

    case check_tags_1(ts, tags, where) do
      false ->
        ts

      true ->
        exit(:error)
    end
  end

  defp check_tags_1(ts, tags, {_, :module} = where) do
    allow = r_tags(tags, :module)
    single = r_tags(tags, :single)
    :edoc_tags.check_tags(ts, allow, single, where)
  end

  defp check_tags_1(ts, tags, {_, :footer} = where) do
    allow = r_tags(tags, :footer)
    single = r_tags(tags, :single)
    :edoc_tags.check_tags(ts, allow, single, where)
  end

  defp check_tags_1(ts, tags, where) do
    allow = r_tags(tags, :function)
    single = r_tags(tags, :single)
    :edoc_tags.check_tags(ts, allow, single, where)
  end

  defp select_spec(ts, {_, {_F, _A}}, specs) do
    case :edoc_tags.filter_tags(
           ts,
           :sets.from_list([:spec])
         ) do
      [] ->
        {for s <- specs do
           :edoc_specs.dummy_spec(s)
         end ++ ts, specs}

      _ ->
        {ts, []}
    end
  end

  defp select_spec(ts, _Where, _Specs) do
    {ts, []}
  end

  defp selected_specs([], ts) do
    ts
  end

  defp selected_specs([f], [_ | ts]) do
    [:edoc_specs.spec(f) | ts]
  end

  defp module_macros(env) do
    [{:module, :erlang.atom_to_list(r_env(env, :module))}] ++ :edoc_macros.std_macros(env)
  end

  defp file_macros(_Context, env) do
    :edoc_macros.std_macros(env)
  end

  defp find_type_docs(forms0, comments, env, file) do
    tree = :erl_recomment.recomment_forms(forms0, comments)
    forms = preprocess_forms(tree)
    env1 = add_macro_defs(:edoc_macros.std_macros(env), [], env)

    f = fn c, line ->
      find_fun(c, line, env1, file)
    end

    :edoc_specs.docs(forms, f)
  end

  defp find_fun(c0, line, env, file) do
    c1 = comment_text(c0)

    text =
      :lists.append(
        for c <- c1 do
          r_comment(c, :text)
        end
      )

    comm = r_comment(line: line, text: text)
    [tag | _] = scan_tags([comm])
    [tag1] = :edoc_macros.expand_tags([tag], env, file)
    tag1
  end
end
