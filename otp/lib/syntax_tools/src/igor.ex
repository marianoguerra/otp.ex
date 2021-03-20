defmodule :m_igor do
  use Bitwise
  require Record

  Record.defrecord(:r_file_info, :file_info,
    size: :undefined,
    type: :undefined,
    access: :undefined,
    atime: :undefined,
    mtime: :undefined,
    ctime: :undefined,
    mode: :undefined,
    links: :undefined,
    major_device: :undefined,
    minor_device: :undefined,
    inode: :undefined,
    uid: :undefined,
    gid: :undefined
  )

  Record.defrecord(:r_file_descriptor, :file_descriptor,
    module: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_module, :module,
    name: :undefined,
    vars: :none,
    functions: :undefined,
    exports: :undefined,
    aliases: :undefined,
    attributes: :undefined,
    records: :undefined
  )

  defp default_printer(tree, options) do
    :erl_prettypr.format(tree, options)
  end

  def parse_transform(forms, options) do
    m = get_module_info(forms)
    name = r_module(m, :name)
    opts = :proplists.append_values(:igor, options)
    files = :proplists.append_values(:files, opts)

    opts1 = [
      {:comments, false},
      {:notes, :no},
      {:no_imports, true},
      {:file_attributes, :yes},
      {:preprocess, true},
      {:export, [name]}
      | opts
    ]

    {t, _} = merge_files(name, [forms], files, opts1)
    verbose('done.', opts1)
    :erl_syntax.revert_forms(t)
  end

  def merge(name, files) do
    merge(name, files, [])
  end

  def merge(name, files, opts) do
    opts1 =
      opts ++
        [
          {:backup_suffix, '.bak'},
          :backups,
          {:dir, ''},
          {:printer, &default_printer/2},
          {:stub_dir, 'stubs'},
          :stubs,
          {:suffix, '.erl'},
          {:verbose, false}
        ]

    {sources, enc} = merge_files1(files, opts1)
    {tree, stubs} = merge_sources(name, sources, opts1)
    dir = :proplists.get_value(:dir, opts1, '')
    filename = :proplists.get_value(:outfile, opts1, name)

    encoding =
      for _ <- [:EFE_DUMMY_GEN], enc !== :none do
        {:encoding, enc}
      end

    file = write_module(tree, filename, dir, encoding ++ opts1)
    [file | maybe_create_stubs(stubs, opts1)]
  end

  def merge_files(name, files, options) do
    merge_files(name, [], files, options)
  end

  def merge_files(name, trees, files, opts) do
    {sources, _Encoding} = merge_files1(files, opts)
    merge_sources(name, trees ++ sources, opts)
  end

  defp merge_files1([], _) do
    report_error('no files to merge.')
    exit(:badarg)
  end

  defp merge_files1(files, opts) do
    opts1 = opts ++ [{:includes, ['.']}, {:macros, []}, {:preprocess, false}, :comments]

    sourceEncodings =
      for f <- files do
        read_module(f, opts1)
      end

    {sources, [encoding | _]} = :lists.unzip(sourceEncodings)
    {sources, encoding}
  end

  Record.defrecord(:r_merge, :merge,
    target: :undefined,
    sources: :undefined,
    export: :undefined,
    static: :undefined,
    safe: :undefined,
    preserved: :undefined,
    no_headers: :undefined,
    notes: :undefined,
    redirect: :undefined,
    no_imports: :undefined,
    options: :undefined
  )

  def merge_sources(name, sources, opts) do
    opts1 =
      opts ++
        [
          {:export_all, false},
          {:file_attributes, :no},
          {:no_imports, false},
          {:notes, :yes},
          :tidy,
          {:verbose, false}
        ]

    trees =
      case sources do
        [] ->
          report_error('no sources to merge.')
          exit(:badarg)

        _ ->
          for m <- sources do
            cond do
              is_list(m) ->
                :erl_syntax.form_list(m)

              true ->
                m
            end
          end
      end

    modules =
      for t <- trees do
        get_module_info(t)
      end

    merge_sources_1(name, modules, trees, opts1)
  end

  Record.defrecord(:r_state, :state, export: :undefined)

  defp state__add_export(name, arity, s) do
    r_state(s,
      export:
        :sets.add_element(
          {name, arity},
          r_state(s, :export)
        )
    )
  end

  defp merge_sources_1(name, modules, trees, opts) do
    ns =
      for m <- modules do
        r_module(m, :name)
      end

    case duplicates(ns) do
      [] ->
        :ok

      ns1 ->
        report_error('same module names repeated in input: ~p.', [ns1])
        exit(:error)
    end

    sources = :ordsets.from_list(ns)
    all = :ordsets.add_element(name, sources)

    es =
      case :proplists.append_values(:export, opts) do
        [] ->
          case :ordsets.is_element(name, sources) do
            true ->
              [name]

            false ->
              [hd(ns)]
          end

        es1 when is_list(es1) ->
          :ordsets.from_list(es1)
      end

    export =
      case :proplists.get_bool(
             :export_all,
             opts
           ) do
        false ->
          es

        true ->
          :ordsets.union(sources, es)
      end

    check_module_names(export, sources, 'declared as exported')
    verbose('modules exported from `~w\': ~p.', [name, export], opts)

    static0 =
      :ordsets.from_list(
        :proplists.append_values(
          :static,
          opts
        )
      )

    static =
      case :proplists.is_defined(:static, opts) do
        false ->
          all

        true ->
          :ordsets.add_element(name, static0)
      end

    check_module_names(static, all, 'declared \'static\'')
    verbose('static modules: ~p.', [static], opts)

    safe =
      case :proplists.is_defined(:safe, opts) do
        false ->
          :ordsets.subtract(
            sources,
            :ordsets.add_element(name, static0)
          )

        true ->
          :ordsets.from_list(
            :proplists.append_values(
              :safe,
              opts
            )
          )
      end

    check_module_names(safe, all, 'declared \'safe\'')
    verbose('safe modules: ~p.', [safe], opts)

    preserved =
      (:ordsets.is_element(
         name,
         sources
       ) and
         :ordsets.is_element(
           name,
           export
         )) or
        :proplists.get_bool(
          :no_banner,
          opts
        )

    noHeaders = :proplists.get_bool(:no_headers, opts)
    notes = :proplists.get_value(:notes, opts, :always)
    rs = :proplists.append_values(:redirect, opts)

    redirect =
      case is_atom_map(rs) do
        true ->
          ms =
            :ordsets.from_list(
              for {m, _} <- rs do
                m
              end
            )

          case :ordsets.intersection(sources, ms) do
            [] ->
              :ok

            ms1 ->
              report_error('cannot redirect calls to modules in input set: ~p.', [ms1])
              exit(:error)
          end

          :dict.from_list(rs)

        false ->
          report_error('bad value for `redirect\' option: ~tP.', [rs, 10])
          exit(:error)
      end

    noImports =
      case :proplists.get_bool(
             :no_imports,
             opts
           ) do
        true ->
          :ordsets.from_list(sources ++ :dict.fetch_keys(redirect))

        false ->
          :ordsets.from_list(:dict.fetch_keys(redirect))
      end

    env =
      r_merge(
        target: name,
        sources: sources,
        export: export,
        safe: safe,
        static: static,
        preserved: preserved,
        no_headers: noHeaders,
        notes: notes,
        redirect: redirect,
        no_imports: noImports,
        options: opts
      )

    merge_sources_2(env, modules, trees, opts)
  end

  defp is_atom_map([{a1, a2} | as])
       when is_atom(a1) and
              is_atom(a2) do
    is_atom_map(as)
  end

  defp is_atom_map([]) do
    true
  end

  defp is_atom_map(_) do
    false
  end

  defp check_module_names(names, sources, txt) do
    case names -- sources do
      [] ->
        :ok

      xs ->
        report_error('unknown modules ~s: ~p.', [txt, xs])
        exit(:error)
    end
  end

  defp merge_sources_2(env, modules, trees, opts) do
    {names, renaming} = merge_namespaces(modules, env)
    {module, expansions} = merge_info(modules, names, renaming, env)
    st = r_state(export: :sets.new())
    {tree, header, st1} = merge_code(trees, modules, expansions, renaming, env, st)

    tree1 =
      :erl_syntax.form_list([make_preamble(module, header, env, st1), filter_forms(tree, env)])

    {tidy(tree1, opts), make_stubs(modules, renaming, env)}
  end

  defp make_preamble(module, header, env, st) do
    name = r_module(module, :name)
    vars = r_module(module, :vars)
    extras = :ordsets.from_list(:sets.to_list(r_state(st, :export)))
    exports = make_exports(r_module(module, :exports), extras)
    imports = make_imports(r_module(module, :aliases))
    attributes = make_attributes(r_module(module, :attributes))

    :erl_syntax.form_list(
      module_header(header, name, vars, env) ++ exports ++ imports ++ attributes
    )
  end

  defp module_header(forms, name, vars, env) do
    case r_merge(env, :preserved) do
      true ->
        update_header(forms, name, vars)

      false ->
        [
          comment(
            [
              '=====================================================================',
              'This module was formed by merging the following modules:',
              ''
            ] ++
              for m <- r_merge(env, :sources) do
                :lists.flatten(:io_lib.fwrite('\t\t`~w\'', [m]))
              end ++ ['', timestamp(), '']
          ),
          :erl_syntax.attribute(
            :erl_syntax.atom(:module),
            [:erl_syntax.atom(name)]
          )
        ]
    end
  end

  defp update_header(fs, name, vars) do
    [m | fs1] = :lists.reverse(fs)

    ps =
      cond do
        vars === :none ->
          []

        true ->
          [
            :erl_syntax.list(
              for v <- vars do
                :erl_syntax.variable(v)
              end
            )
          ]
      end

    m1 =
      rewrite(
        m,
        :erl_syntax.attribute(
          :erl_syntax.atom(:module),
          [:erl_syntax.atom(name) | ps]
        )
      )

    :lists.reverse([m1 | fs1])
  end

  defp make_exports(exports, extras) do
    case :ordsets.subtract(extras, exports) do
      [] ->
        [make_export(exports)]

      es ->
        [
          make_export(exports),
          comment(['** The following exports are not official: **']),
          make_export(es)
        ]
    end
  end

  defp make_export(names) do
    es =
      for {f, a} <- names do
        :erl_syntax.arity_qualifier(
          :erl_syntax.atom(f),
          :erl_syntax.integer(a)
        )
      end

    cond do
      es === [] ->
        comment(['** Nothing is officially exported from this module! **'])

      true ->
        :erl_syntax.attribute(
          :erl_syntax.atom(:export),
          [:erl_syntax.list(es)]
        )
    end
  end

  defp make_imports(as) do
    as1 =
      for {f, {_M, f}} = a <- as,
          not is_auto_import(f) do
        a
      end

    for {m, fs} <- group_imports(as1) do
      make_import(m, fs)
    end
  end

  defp make_import(module, names) do
    is =
      for {f, a} <- names do
        :erl_syntax.arity_qualifier(
          :erl_syntax.atom(f),
          :erl_syntax.integer(a)
        )
      end

    :erl_syntax.attribute(
      :erl_syntax.atom(:import),
      [:erl_syntax.atom(module), :erl_syntax.list(is)]
    )
  end

  defp group_imports(imports) do
    :dict.to_list(
      :lists.foldl(
        fn {f, {m, f}}, d ->
          case :dict.find(m, d) do
            {:ok, v} ->
              v1 = :ordsets.add_element(f, v)
              :dict.store(m, v1, d)

            :error ->
              :dict.store(m, [f], d)
          end
        end,
        :dict.new(),
        imports
      )
    )
  end

  defp make_stubs(modules, renaming, env) do
    make_stubs_1(modules, renaming, env)
  end

  defp make_stubs_1([m | ms], renaming, env) do
    name = r_module(m, :name)

    cond do
      name !== r_merge(env, :target) ->
        case :ordsets.is_element(name, r_merge(env, :export)) do
          true ->
            [make_stub(m, renaming.(name), env) | make_stubs_1(ms, renaming, env)]

          false ->
            make_stubs_1(ms, renaming, env)
        end

      true ->
        make_stubs_1(ms, renaming, env)
    end
  end

  defp make_stubs_1([], _, _) do
    []
  end

  defp make_stub(m, map, env) do
    target = r_merge(env, :target)

    es =
      for f <- r_module(m, :exports) do
        {f, {target, map.(f)}}
      end

    {r_module(m, :name), es, r_module(m, :attributes)}
  end

  Record.defrecord(:r_filter, :filter,
    records: :undefined,
    file_attributes: :undefined,
    attributes: :undefined
  )

  defp filter_forms(tree, env) do
    forms = :erl_syntax.form_list_elements(:erl_syntax.flatten_form_list(tree))
    :erl_syntax.form_list(filter_forms_1(forms, env))
  end

  defp filter_forms_1(forms, env) do
    {fs, _} = filter_forms_2(forms, env)
    :lists.reverse(fs)
  end

  defp filter_forms_2(forms, env) do
    fileAttrsOpt = :proplists.get_value(:file_attributes, r_merge(env, :options), :comment)

    fileAttrs =
      case fileAttrsOpt do
        :yes ->
          :keep

        :no ->
          :delete

        :comment ->
          :kill

        _ ->
          report_error('invalid value for option `file_attributes\': ~tw.', [fileAttrsOpt])
          exit(:error)
      end

    attrs =
      cond do
        length(r_merge(env, :sources)) === 1 ->
          :delete

        true ->
          :kill
      end

    s = r_filter(records: :sets.new(), file_attributes: fileAttrs, attributes: attrs)

    :lists.foldl(
      fn f, {fs, s0} ->
        case filter_form(f, s0) do
          {:keep, s1} ->
            {[f | fs], s1}

          {:kill, s1} ->
            {[kill_form(f) | fs], s1}

          {:delete, s1} ->
            case :erl_syntax.has_comments(f) do
              false ->
                {fs, s1}

              true ->
                {[kill_form(f) | fs], s1}
            end
        end
      end,
      {[], s},
      forms
    )
  end

  defp filter_form(f, s) do
    case :erl_syntax_lib.analyze_form(f) do
      {:attribute, {:file, _}} ->
        {r_filter(s, :file_attributes), s}

      {:attribute, {:module, _}} ->
        {:delete, s}

      {:attribute, {:export, _}} ->
        {:delete, s}

      {:attribute, {:import, _}} ->
        {:delete, s}

      {:attribute, {:record, {r, _}}} ->
        records = r_filter(s, :records)

        case :sets.is_element(r, records) do
          true ->
            {:kill, s}

          false ->
            s1 = r_filter(s, records: :sets.add_element(r, records))
            {:keep, s1}
        end

      {:attribute, :preprocessor} ->
        {:keep, s}

      {:attribute, _} ->
        {r_filter(s, :attributes), s}

      {:error_marker, _} ->
        {:delete, s}

      {:warning_marker, _} ->
        {:delete, s}

      :eof_marker ->
        {:delete, s}

      _ ->
        {:keep, s}
    end
  end

  defp kill_form(f) do
    f1 = :erl_syntax.set_precomments(f, [])
    f2 = :erl_syntax_lib.to_comment(f1, '%<<< ')

    :erl_syntax.set_precomments(
      f2,
      :erl_syntax.get_precomments(f)
    )
  end

  defp merge_namespaces(modules, env) do
    export = r_merge(env, :export)

    split = fn m ->
      :ordsets.is_element(r_module(m, :name), export)
    end

    {m1, m2} = split_list(split, modules)
    r = :dict.new()
    acc = {:sets.new(), r}
    {m3, acc1} = merge_namespaces_1(m1, acc)
    {_, maps0} = acc1

    case (for {m, map} <- :dict.to_list(maps0),
              :dict.size(map) !== 0 do
            {m, :dict.to_list(map)}
          end) do
      [] ->
        :ok

      fs ->
        report_warning('interface functions renamed:\n\t~tp.', [fs])
    end

    {m4, acc2} = merge_namespaces_1(m2, acc1)
    ms = m3 ++ m4
    acc3 = merge_namespaces_2(ms, acc2)
    {{names, maps}, _} = merge_namespaces_3(ms, acc3)
    {names, make_renaming_function(maps)}
  end

  defp merge_namespaces_1(modules, acc) do
    :lists.mapfoldl(
      fn module, {names, maps} ->
        exports = :sets.from_list(r_module(module, :exports))
        m = r_module(module, :name)
        {names1, map} = add_function_renamings(m, exports, names, :dict.new())
        maps1 = :dict.store(m, map, maps)
        {{module, exports}, {names1, maps1}}
      end,
      acc,
      modules
    )
  end

  defp merge_namespaces_2(modules, acc) do
    :lists.foldl(
      fn {module, exports}, {names, maps} ->
        other =
          :sets.subtract(
            :sets.from_list(r_module(module, :functions)),
            exports
          )

        m = r_module(module, :name)
        map = :dict.fetch(m, maps)
        {names1, map1} = add_function_renamings(m, other, names, map)
        maps1 = :dict.store(m, map1, maps)
        {names1, maps1}
      end,
      acc,
      modules
    )
  end

  defp merge_namespaces_3(modules, acc) do
    :lists.foldl(
      fn {module, _Exports}, {{names, maps}, rMap} ->
        records = r_module(module, :records)
        m = r_module(module, :name)
        map = :dict.fetch(m, maps)
        {names1, map1, rMap1} = add_record_renamings(m, records, names, map, rMap)
        maps1 = :dict.store(m, map1, maps)
        {{names1, maps1}, rMap1}
      end,
      {acc, :dict.new()},
      modules
    )
  end

  defp add_function_renamings(module, new, names, map) do
    clashes = :sets.to_list(:sets.intersection(new, names))

    :lists.foldl(
      fn f = {_, a}, {names, map}
         when is_integer(a) ->
        f1 = new_function_name(module, f, names)
        {:sets.add_element(f1, names), :dict.store(f, f1, map)}
      end,
      {:sets.union(new, names), map},
      clashes
    )
  end

  defp add_record_renamings(module, records, names, map, rMap) do
    :lists.foldl(
      fn n = {r, fs}, {names, map, rMap} ->
        case :sets.is_element({:record, r}, names) do
          true ->
            case :sets.is_element({:record, n}, names) do
              true ->
                {r1, _} = remap_record_name(n, rMap)
                map1 = :dict.store({:record, r}, {:record, r1}, map)
                {names, map1, rMap}

              false ->
                n1 = {r1, _} = new_record_name(module, r, fs, names)
                map1 = :dict.store({:record, r}, {:record, r1}, map)
                rMap1 = :dict.store(n, n1, rMap)
                names1 = :sets.add_element({:record, n1}, names)
                {names1, map1, rMap1}
            end

          false ->
            names1 = :sets.add_element({:record, r}, names)
            names2 = :sets.add_element({:record, n}, names1)
            {names2, map, rMap}
        end
      end,
      {names, map, rMap},
      records
    )
  end

  defp remap_record_name(n, map) do
    case :dict.find(n, map) do
      {:ok, n1} ->
        n1

      :error ->
        n
    end
  end

  defp map_record_name(r, map) do
    {:record, r1} = map.({:record, r})
    r1
  end

  defp new_function_name(m, {f, a}, names) do
    base = :erlang.atom_to_list(m) ++ '__' ++ :erlang.atom_to_list(f)
    name = {:erlang.list_to_atom(base), a}

    case :sets.is_element(name, names) do
      false ->
        name

      true ->
        new_function_name(1, a, base, names)
    end
  end

  defp new_function_name(n, arity, base, names) do
    name = {:erlang.list_to_atom(base ++ '_' ++ :erlang.integer_to_list(n)), arity}

    case :sets.is_element(name, names) do
      false ->
        name

      true ->
        new_function_name(n + 1, arity, base, names)
    end
  end

  defp new_record_name(m, r, fs, names) do
    base = :erlang.atom_to_list(m) ++ '__' ++ :erlang.atom_to_list(r)
    name = {:erlang.list_to_atom(base), fs}

    case :sets.is_element({:record, name}, names) do
      false ->
        name

      true ->
        new_record_name_1(1, base, fs, names)
    end
  end

  defp new_record_name_1(n, base, fs, names) do
    name = {:erlang.list_to_atom(base ++ '_' ++ :erlang.integer_to_list(n)), fs}

    case :sets.is_element({:record, name}, names) do
      false ->
        name

      true ->
        new_record_name_1(n + 1, base, fs, names)
    end
  end

  defp make_renaming_function(maps) do
    fn module ->
      case :dict.find(module, maps) do
        {:ok, map} ->
          fn name ->
            case :dict.find(name, map) do
              {:ok, name1} ->
                name1

              :error ->
                name
            end
          end

        :error ->
          fn name ->
            name
          end
      end
    end
  end

  defp merge_info(modules, names, renaming, env) do
    forbid = :sets.from_list(r_merge(env, :no_imports))
    expansions = alias_expansions(modules, names, forbid)
    module = merge_info_1(modules, renaming, expansions, env)
    {module, expansions}
  end

  defp merge_info_1(modules, renaming, expansions, env) do
    :lists.foldl(
      fn m, a ->
        name = r_module(m, :name)
        map = renaming.(name)
        functions = join_functions(map, r_module(m, :functions), r_module(a, :functions))
        exports = join_exports(env, name, map, r_module(m, :exports), r_module(a, :exports))
        aliases = join_aliases(name, expansions, r_module(m, :aliases), r_module(a, :aliases))

        attributes =
          join_attributes(env, name, r_module(m, :attributes), r_module(a, :attributes))

        records = join_records(map, r_module(m, :records), r_module(a, :records))

        r_module(a,
          functions: functions,
          exports: exports,
          aliases: aliases,
          attributes: attributes,
          records: records
        )
      end,
      r_module(
        name: r_merge(env, :target),
        functions: :ordsets.new(),
        exports: :ordsets.new(),
        aliases: :ordsets.new(),
        attributes: :ordsets.new(),
        records: :ordsets.new()
      ),
      modules
    )
  end

  defp join_functions(map, source, target) do
    :ordsets.union(
      :ordsets.from_list(
        for a <- source do
          map.(a)
        end
      ),
      target
    )
  end

  defp join_exports(env, name, map, source, target) do
    case :ordsets.is_element(name, r_merge(env, :export)) do
      true ->
        :ordsets.union(
          :ordsets.from_list(
            for f <- source do
              map.(f)
            end
          ),
          target
        )

      false ->
        target
    end
  end

  defp join_aliases(name, expansions, source, target) do
    as =
      case :dict.find(name, expansions) do
        {:ok, as1} ->
          :ordsets.from_list(:dict.to_list(as1))

        :error ->
          []
      end

    :ordsets.union(:ordsets.subtract(source, as), target)
  end

  defp join_attributes(env, name, source, target) do
    cond do
      r_merge(env, :target) === name ->
        :ordsets.union(source, target)

      true ->
        cond do
          length(r_merge(env, :sources)) === 1 ->
            :ordsets.union(source, target)

          true ->
            target
        end
    end
  end

  defp join_records(map, source, target) do
    renamed =
      for {r, fs} <- source do
        {map_record_name(r, map), fs}
      end

    :ordsets.union(:ordsets.from_list(renamed), target)
  end

  defp alias_expansions(modules, names, forbid) do
    table = alias_expansions_1(modules, forbid, names)
    alias_expansions_2(modules, table)
  end

  defp alias_expansions_1(modules, forbid, names) do
    :lists.foldl(
      fn m, t ->
        map =
          :lists.foldl(
            fn {a, f}, t1 ->
              case keep_alias(a, f, forbid, names) do
                true ->
                  t1

                false ->
                  :dict.store(a, f, t1)
              end
            end,
            :dict.new(),
            r_module(m, :aliases)
          )

        :dict.store(r_module(m, :name), map, t)
      end,
      :dict.new(),
      modules
    )
  end

  defp keep_alias(a, {m, _}, forbid, names) do
    case :sets.is_element(m, forbid) do
      true ->
        false

      false ->
        not :sets.is_element(a, names)
    end
  end

  defp alias_expansions_2(modules, table) do
    aliases =
      :lists.foldl(
        fn m, a ->
          :ordsets.union(a, r_module(m, :aliases))
        end,
        :ordsets.new(),
        modules
      )

    names =
      duplicates(
        for {f, _} <- aliases do
          f
        end
      )

    :lists.foldl(
      fn m, t ->
        n = r_module(m, :name)

        :lists.foldl(
          fn {a, f}, t1 ->
            case :ordsets.is_element(a, names) do
              true ->
                t2 = :dict.fetch(n, t1)

                :dict.store(
                  n,
                  :dict.store(a, f, t2),
                  t1
                )

              false ->
                t1
            end
          end,
          t,
          r_module(m, :aliases)
        )
      end,
      table,
      modules
    )
  end

  Record.defrecord(:r_code, :code,
    module: :undefined,
    target: :undefined,
    sources: :undefined,
    static: :undefined,
    safe: :undefined,
    preserved: :undefined,
    no_headers: :undefined,
    notes: :undefined,
    map: :undefined,
    renaming: :undefined,
    expand: :undefined,
    redirect: :undefined
  )

  defp merge_code(trees, modules, expansions, renaming, env, st) do
    env1 =
      r_code(
        target: r_merge(env, :target),
        sources: :sets.from_list(r_merge(env, :sources)),
        static: :sets.from_list(r_merge(env, :static)),
        safe: :sets.from_list(r_merge(env, :safe)),
        preserved: r_merge(env, :preserved),
        no_headers: r_merge(env, :no_headers),
        notes: r_merge(env, :notes),
        redirect: r_merge(env, :redirect),
        renaming: renaming
      )

    code = order_code(modules, trees, env1)

    {code1, header} =
      case r_code(env1, :preserved) do
        true ->
          take_header(code)

        false ->
          {code, :erl_syntax.form_list([])}
      end

    {forms, st1} = merge_code_1(code1, expansions, env1, st)
    tree = :erl_syntax.form_list(forms)
    {tree, header, st1}
  end

  defp merge_code_1(code, expansions, env, st) do
    :lists.foldr(
      fn {module, t}, {acc, st0} ->
        m = r_module(module, :name)

        expand =
          case :dict.find(m, expansions) do
            {:ok, dict} ->
              dict

            :error ->
              :dict.new()
          end

        env1 = r_code(env, module: m, map: r_code(env, :renaming).(m), expand: expand)
        {t1, st1} = transform(t, env1, st0)
        {[section_header(m, t1, env1) | acc], st1}
      end,
      {[], st},
      code
    )
  end

  defp order_code(modules, trees, env) do
    order_code(modules, trees, {}, [], env)
  end

  defp order_code([m | ms], [t | ts], first, rest, env) do
    t1 = :erl_syntax.flatten_form_list(t)

    case :erlang.and(
           r_module(m, :name) === r_code(env, :target),
           r_code(env, :preserved)
         ) do
      true ->
        order_code(ms, ts, {m, t1}, rest, env)

      false ->
        order_code(ms, ts, first, [{m, t1} | rest], env)
    end
  end

  defp order_code([], [], first, rest, _Env) do
    rest1 = :lists.reverse(rest)

    case first do
      {} ->
        rest1

      m ->
        [m | rest1]
    end
  end

  defp take_header([{m, t} | ms]) do
    fs = :erl_syntax.form_list_elements(t)
    {header, fs1} = take_header_1(fs, [])
    t1 = :erl_syntax.form_list(fs1)
    {[{m, t1} | ms], header}
  end

  defp take_header_1([f | fs], as) do
    case :erl_syntax_lib.analyze_form(f) do
      {:attribute, {:module, _}} ->
        {:lists.reverse([f | as]), fs}

      {:attribute, {:file, _}} ->
        take_header_1(fs, as)

      _ ->
        take_header_1(fs, [f | as])
    end
  end

  defp section_header(name, tree, env) do
    n = :sets.size(r_code(env, :sources))

    cond do
      n > 1 and name !== r_code(env, :target) and
        r_code(env, :notes) !== :no and
          r_code(env, :no_headers) !== true ->
        text = :io_lib.fwrite('The following code stems from module `~w\'.', [name])

        header =
          comment([
            '=====================================================================',
            '',
            :lists.flatten(text),
            ''
          ])

        :erl_syntax.form_list([header, tree])

      true ->
        tree
    end
  end

  defp transform(tree, env, st) do
    case :erl_syntax.type(tree) do
      :application ->
        transform_application(tree, env, st)

      :attribute ->
        transform_attribute(tree, env, st)

      :function ->
        transform_function(tree, env, st)

      :implicit_fun ->
        transform_implicit_fun(tree, env, st)

      :record_expr ->
        transform_record(tree, env, st)

      :record_index_expr ->
        transform_record(tree, env, st)

      :record_access ->
        transform_record(tree, env, st)

      _ ->
        default_transform(tree, env, st)
    end
  end

  defp default_transform(tree, env, st) do
    case :erl_syntax.subtrees(tree) do
      [] ->
        {tree, st}

      gs ->
        {gs1, st1} = transform_1(gs, env, st)

        tree1 =
          rewrite(
            tree,
            :erl_syntax.make_tree(:erl_syntax.type(tree), gs1)
          )

        {tree1, st1}
    end
  end

  defp transform_1([g | gs], env, st) do
    {g1, st1} = transform_list(g, env, st)
    {gs1, st2} = transform_1(gs, env, st1)
    {[g1 | gs1], st2}
  end

  defp transform_1([], _Env, st) do
    {[], st}
  end

  defp transform_list([t | ts], env, st) do
    {t1, st1} = transform(t, env, st)
    {ts1, st2} = transform_list(ts, env, st1)
    {[t1 | ts1], st2}
  end

  defp transform_list([], _Env, st) do
    {[], st}
  end

  defp transform_function(t, env, st) do
    {t1, st1} = default_transform(t, env, st)
    f = :erl_syntax_lib.analyze_function(t1)

    {v, text} =
      case r_code(env, :map).(f) do
        ^f ->
          {:none, []}

        {atom, _Arity} ->
          cs = :erl_syntax.function_clauses(t1)
          n = rename_atom(:erl_syntax.function_name(t1), atom)
          t2 = :erl_syntax.function(n, cs)
          {{:value, t2}, renaming_note(atom)}
      end

    {maybe_modified(v, t1, 2, text, env), st1}
  end

  defp renaming_note(name) do
    [:lists.flatten(:io_lib.fwrite('renamed function to `~tw\'', [name]))]
  end

  defp rename_atom(node, atom) do
    rewrite(node, :erl_syntax.atom(atom))
  end

  defp transform_implicit_fun(t, env, st) do
    {t1, st1} = default_transform(t, env, st)

    {v, text} =
      case :erl_syntax.type(:erl_syntax.implicit_fun_name(t1)) do
        :arity_qualifier ->
          f = :erl_syntax_lib.analyze_implicit_fun(t1)

          case r_code(env, :map).(f) do
            ^f ->
              {:none, []}

            {atom, arity} ->
              n =
                rewrite(
                  :erl_syntax.implicit_fun_name(t1),
                  :erl_syntax.arity_qualifier(
                    :erl_syntax.atom(atom),
                    :erl_syntax.integer(arity)
                  )
                )

              t2 = :erl_syntax.implicit_fun(n)
              {{:value, t2}, ['function was renamed']}
          end

        :module_qualifier ->
          {:none, []}
      end

    {maybe_modified_quiet(v, t1, 2, text, env), st1}
  end

  defp transform_application(t, env, st) do
    {as, st1} = transform_list(:erl_syntax.application_arguments(t), env, st)
    f = :erl_syntax.application_operator(t)

    case (try do
            {:ok, :erl_syntax_lib.analyze_function_name(f)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, name} ->
        transform_application_1(name, f, as, t, env, st1)

      :syntax_error ->
        {f1, st2} = transform(f, env, st1)
        {rewrite(t, :erl_syntax.application(f1, as)), st2}

      {:EXIT, r} ->
        exit(r)

      r ->
        throw(r)
    end
  end

  defp transform_application_1(name, f, as, t, env, st) do
    arity = length(as)
    {name1, f1} = expand_operator(name, arity, f, env)
    f2 = maybe_modified_quiet(f1, f, 7, ['unfolded alias'], env)
    {v, st1} = transform_application_2(name1, arity, f2, as, env, st)
    t1 = rewrite(t, :erl_syntax.application(f2, as))

    t3 =
      case v do
        :none ->
          t1

        {:value, {t2, depth, message}} ->
          maybe_modified_quiet({:value, t2}, t1, depth, message, env)
      end

    {t3, st1}
  end

  defp transform_application_2(name, arity, f, as, env, st)
       when is_atom(name) do
    transform_atom_application(name, arity, f, as, env, st)
  end

  defp transform_application_2({m, n}, arity, f, as, env, st)
       when is_atom(m) and is_atom(n) do
    transform_qualified_application(m, n, arity, f, as, env, st)
  end

  defp transform_application_2(_Name, _Arity, _F, _As, _Env, st) do
    {:none, st}
  end

  defp expand_operator(name, arity, _F, env) when is_atom(name) do
    case is_auto_import({name, arity}) do
      true ->
        {name, :none}

      false ->
        case :dict.find({name, arity}, r_code(env, :expand)) do
          {:ok, {m, {n, a}}} when a === arity ->
            f1 =
              :erl_syntax.module_qualifier(
                :erl_syntax.atom(m),
                :erl_syntax.atom(n)
              )

            {{m, n}, {:value, f1}}

          :error ->
            {name, :none}
        end
    end
  end

  defp expand_operator(name, _Arity, _F, _Env) do
    {name, :none}
  end

  defp transform_atom_application(name, arity, f, as, env, st) do
    case {name, arity} do
      {:apply, 2} ->
        warning_apply_2(r_code(env, :module), r_code(env, :target))
        {:none, st}

      {:apply, 3} ->
        transform_apply_call(f, as, env, st)

      {:spawn, 3} ->
        transform_spawn_call(f, as, env, st)

      {:spawn, 4} ->
        transform_spawn_call(f, as, env, st)

      {:spawn_link, 3} ->
        transform_spawn_call(f, as, env, st)

      {:spawn_link, 4} ->
        transform_spawn_call(f, as, env, st)

      _ ->
        case is_auto_import({name, arity}) do
          true ->
            {:none, st}

          false ->
            case r_code(env, :map).({name, arity}) do
              {n, a} when n === name and a === arity ->
                {:none, st}

              {n, a} when a === arity ->
                f1 = rewrite(f, :erl_syntax.atom(n))
                t = :erl_syntax.application(f1, as)
                v = {t, 2, ['callee was renamed']}
                {{:value, v}, st}
            end
        end
    end
  end

  defp transform_qualified_application(module, name, arity, f, as, env, st) do
    case {module, name, arity} do
      {:erlang, :apply, 2} ->
        warning_apply_2(r_code(env, :module), r_code(env, :target))
        {:none, st}

      {:erlang, :apply, 3} ->
        transform_apply_call(f, as, env, st)

      {:erlang, :spawn, 3} ->
        transform_spawn_call(f, as, env, st)

      {:erlang, :spawn, 4} ->
        transform_spawn_call(f, as, env, st)

      {:erlang, :spawn_link, 3} ->
        transform_spawn_call(f, as, env, st)

      {:erlang, :spawn_link, 4} ->
        transform_spawn_call(f, as, env, st)

      _ ->
        case :erlang.is_builtin(module, name, arity) do
          false ->
            transform_qualified_application_1(module, name, arity, f, as, env, st)

          true ->
            {:none, st}
        end
    end
  end

  defp transform_qualified_application_1(module, name, arity, f, as, env, st) do
    makeLocal = fn n ->
      f1 = rewrite(f, :erl_syntax.atom(n))
      :erl_syntax.application(f1, as)
    end

    makeRemote = fn ->
      :erl_syntax.application(f, as)
    end

    makeDynamic = fn m, n ->
      f1 =
        :erl_syntax.module_qualifier(
          :erl_syntax.atom(m),
          :erl_syntax.atom(n)
        )

      f2 = rewrite(f, f1)
      :erl_syntax.application(f2, as)
    end

    localise(module, name, arity, makeLocal, makeRemote, makeDynamic, 3, env, st)
  end

  defp transform_apply_call(f, as, env, st) do
    [module, name, list] = as

    case :erlang.and(
           :erlang.and(
             :erl_syntax.type(module) === :atom,
             :erl_syntax.type(name) === :atom
           ),
           :erl_syntax.is_proper_list(list)
         ) do
      true ->
        transform_apply_call_1(module, name, list, f, as, env, st)

      false ->
        warning_unsafe_call(:apply, r_code(env, :module), r_code(env, :target))
        {:none, st}
    end
  end

  defp transform_apply_call_1(module, name, list, f, _As, env, st) do
    f1 =
      rewrite(
        f,
        :erl_syntax.module_qualifier(module, name)
      )

    as1 = :erl_syntax.list_elements(list)
    m = :erl_syntax.atom_value(module)
    n = :erl_syntax.atom_value(name)
    a = length(as1)
    transform_qualified_application_1(m, n, a, f1, as1, env, st)
  end

  defp transform_spawn_call(f, as, env, st) do
    case as do
      [module, name, list] ->
        makeSpawn = fn as1 ->
          :erl_syntax.application(f, as1)
        end

        transform_spawn_call_1(module, name, list, makeSpawn, env, st)

      [node, module, name, list] ->
        makeSpawn = fn as1 ->
          :erl_syntax.application(f, [node | as1])
        end

        transform_spawn_call_1(module, name, list, makeSpawn, env, st)
    end
  end

  defp transform_spawn_call_1(module, name, list, makeSpawn, env, st) do
    case :erlang.and(
           :erlang.and(
             :erl_syntax.type(module) === :atom,
             :erl_syntax.type(name) === :atom
           ),
           :erl_syntax.is_proper_list(list)
         ) do
      true ->
        transform_spawn_call_2(module, name, list, makeSpawn, env, st)

      _ ->
        warning_unsafe_call(:spawn, r_code(env, :module), r_code(env, :target))
        {:none, st}
    end
  end

  defp transform_spawn_call_2(module, name, list, makeSpawn, env, st) do
    as = :erl_syntax.list_elements(list)
    arity = length(as)

    makeLocal = fn n ->
      a = rewrite(name, :erl_syntax.atom(n))
      b = :erl_syntax.application(a, as)
      c = :erl_syntax.clause([], [b])
      f = :erl_syntax.fun_expr([c])
      makeSpawn.([f])
    end

    makeRemote = fn ->
      makeSpawn.([module, name, list])
    end

    makeDynamic = fn m, n ->
      f = rewrite(name, :erl_syntax.atom(n))
      makeSpawn.([:erl_syntax.atom(m), f, list])
    end

    localise(
      :erl_syntax.atom_value(module),
      :erl_syntax.atom_value(name),
      arity,
      makeLocal,
      makeRemote,
      makeDynamic,
      4,
      env,
      st
    )
  end

  defp localise(module, name, arity, makeLocal, makeRemote, makeDynamic, depth, env, st) do
    case :sets.is_element(module, r_code(env, :sources)) do
      false ->
        case :dict.find(module, r_code(env, :redirect)) do
          {:ok, module1} ->
            t = makeDynamic.(module1, name)
            v = {t, depth, ['redirected call']}
            {{:value, v}, st}

          :error ->
            {:none, st}
        end

      true ->
        map = r_code(env, :renaming).(module)

        name1 =
          case map.({name, arity}) do
            {n, a} when a === arity ->
              n
          end

        safe = :sets.is_element(module, r_code(env, :safe))

        static =
          :erlang.or(
            :sets.is_element(
              module,
              r_code(env, :static)
            ),
            safe
          )

        case static do
          false ->
            l = makeLocal.(name1)
            l1 = :erl_syntax_lib.strip_comments(l)
            r = makeRemote.()
            {t, text} = protect_call(module, l1, r)
            v = {t, depth, text}
            {{:value, v}, st}

          true ->
            case safe do
              false ->
                target = r_code(env, :target)

                case module === target do
                  true ->
                    {:none, st}

                  false ->
                    st1 = state__add_export(name1, arity, st)
                    t = makeDynamic.(target, name1)
                    text = ['localised call']
                    v = {t, depth, text}
                    {{:value, v}, st1}
                end

              true ->
                t = makeLocal.(name1)
                text = ['localised safe call']
                v = {t, depth, text}
                {{:value, v}, st}
            end
        end
    end
  end

  defp protect_call(_Module, _Local, remote) do
    {remote, ['dynamic call']}
  end

  defp transform_attribute(t, env, st) do
    {t1, st1} = tSt1 = default_transform(t, env, st)

    case :erl_syntax_lib.analyze_attribute(t1) do
      {:record, {r, _}} ->
        f = fn r ->
          [_ | as] = :erl_syntax.attribute_arguments(t1)

          :erl_syntax.attribute(
            :erl_syntax.attribute_name(t1),
            [:erl_syntax.atom(r) | as]
          )
        end

        {v, text} = rename_record(r, f, env)
        {maybe_modified(v, t1, 2, text, env), st1}

      _ ->
        tSt1
    end
  end

  defp transform_record(t, env, st) do
    {t1, st1} = tSt1 = default_transform(t, env, st)

    x =
      case (try do
              :erl_syntax_lib.analyze_record_expr(t1)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:record_expr, {r, _}} ->
          f = fn r ->
            :erl_syntax.record_expr(
              :erl_syntax.record_expr_argument(t1),
              :erl_syntax.atom(r),
              :erl_syntax.record_expr_fields(t1)
            )
          end

          {r, f}

        {:record_index_expr, {r, _}} ->
          f = fn r ->
            :erl_syntax.record_index_expr(
              :erl_syntax.atom(r),
              :erl_syntax.record_index_expr_field(t1)
            )
          end

          {r, f}

        {:record_access, {r, _}} ->
          f = fn r ->
            :erl_syntax.record_access(
              :erl_syntax.record_access_argument(t1),
              :erl_syntax.atom(r),
              :erl_syntax.record_access_field(t1)
            )
          end

          {r, f}

        _Type ->
          false
      end

    case x do
      {r1, f1} ->
        {v, text} = rename_record(r1, f1, env)
        {maybe_modified(v, t1, 1, text, env), st1}

      false ->
        tSt1
    end
  end

  defp rename_record(r, f, env) do
    case map_record_name(r, r_code(env, :map)) do
      ^r ->
        {:none, []}

      r1 ->
        {{:value, f.(r1)}, ['record was renamed']}
    end
  end

  defp maybe_modified_quiet(v, node, depth, message, env) do
    case r_code(env, :notes) do
      :always ->
        maybe_modified_1(v, node, depth, message, :yes)

      _ ->
        maybe_modified_1(v, node, depth, message, :no)
    end
  end

  defp maybe_modified(v, node, depth, message, env) do
    maybe_modified_1(v, node, depth, message, r_code(env, :notes))
  end

  defp maybe_modified_1(:none, node, _Depth, _Message, _Notes) do
    node
  end

  defp maybe_modified_1({:value, node1}, node, depth, message, notes) do
    case notes do
      :no ->
        rewrite(node, node1)

      _ ->
        code =
          :erl_syntax.comment_text(
            :erl_syntax_lib.to_comment(
              :erl_syntax_lib.strip_comments(
                :erl_syntax_lib.limit(
                  node,
                  depth
                )
              ),
              '  '
            )
          )

        :erl_syntax.add_precomments(
          [
            comment_note(
              message ++
                [
                  'Original code:'
                  | code
                ]
            )
          ],
          rewrite(node, node1)
        )
    end
  end

  def create_stubs(stubs, opts) do
    opts1 =
      opts ++
        [
          {:backup_suffix, '.bak'},
          :backups,
          {:dir, ''},
          {:printer, &default_printer/2},
          {:stub_dir, 'stubs'},
          :stubs,
          {:suffix, '.erl'},
          {:verbose, false}
        ]

    :lists.foldl(
      fn s, fs ->
        f = create_stub(s, opts1)
        [f | fs]
      end,
      [],
      stubs
    )
  end

  defp maybe_create_stubs(stubs, opts) do
    case :proplists.get_bool(:stubs, opts) do
      true ->
        create_stubs(stubs, opts)

      false ->
        []
    end
  end

  defp create_stub({name, fs, attrs}, opts) do
    defs =
      for f <- fs do
        stub_function(f)
      end

    exports =
      for {f, _} <- fs do
        f
      end

    forms = stub_header(name, exports, attrs) ++ defs
    dir = :proplists.get_value(:stub_dir, opts, '')
    verbose('creating stub file for module `~w\'.', [name], opts)
    write_module(:erl_syntax.form_list(forms), name, dir, opts)
  end

  defp stub_function({{f, a}, {m, {f1, a1}}}) do
    vs = var_list(a)
    vs1 = var_list(a1)

    r =
      :erl_syntax.module_qualifier(
        :erl_syntax.atom(m),
        :erl_syntax.atom(f1)
      )

    call = :erl_syntax.application(r, vs1)

    :erl_syntax.function(
      :erl_syntax.atom(f),
      [:erl_syntax.clause(vs, [], [call])]
    )
  end

  defp var_list(n) do
    var_list(n, 1)
  end

  defp var_list(n, i) when n > 0 do
    [
      :erl_syntax.variable('X' ++ :erlang.integer_to_list(i))
      | var_list(n - 1, i + 1)
    ]
  end

  defp var_list(0, _) do
    []
  end

  defp stub_header(name, exports, attrs) do
    [
      comment([
        '=====================================================================',
        :io_lib.fwrite(
          'This is an automatically generated stub interface\nfor the module `~w\'.',
          [name]
        ),
        '',
        timestamp(),
        ''
      ]),
      :erl_syntax.attribute(
        :erl_syntax.atom(:module),
        [:erl_syntax.atom(name)]
      ),
      make_export(exports)
    ] ++ make_attributes(attrs)
  end

  def rename(files, renamings) do
    rename(files, renamings, [])
  end

  def rename(files, renamings, opts) do
    dict =
      case is_atom_map(renamings) do
        true ->
          :dict.from_list(renamings)

        false ->
          report_error('bad module renaming: ~tP.', [renamings, 10])
          exit(:error)
      end

    opts1 =
      [{:find_src_rules, []}] ++
        opts ++
        [
          {:backup_suffix, '.bak'},
          :backups,
          {:printer, &default_printer/2},
          :stubs,
          {:suffix, '.erl'},
          :comments,
          {:preprocess, false},
          {:tidy, false},
          :no_banner,
          {:notes, :no},
          {:verbose, false}
        ]

    :lists.flatmap(
      fn f ->
        rename_file(f, dict, opts1)
      end,
      files
    )
  end

  defp rename_file(file, dict, opts) do
    {s, enc} = read_module(file, opts)

    encoding =
      for _ <- [:EFE_DUMMY_GEN], enc !== :none, not :proplists.get_bool(:comments, opts) do
        {:encoding, enc}
      end

    m = get_module_info(s)
    name = r_module(m, :name)

    name1 =
      case :dict.find(name, dict) do
        {:ok, n} ->
          n

        :error ->
          name
      end

    dict1 = :dict.erase(name, dict)

    opts1 =
      [:no_headers, {:export, [name]}, {:static, [name]}, {:redirect, :dict.to_list(dict1)}] ++
        encoding ++ opts

    {tree, stubs} = merge_sources(name1, [s], opts1)
    dir = :filename.dirname(filename(file))
    file1 = write_module(tree, name1, dir, opts ++ encoding)

    [
      file1
      | maybe_create_stubs(
          stubs,
          [{:stub_dir, dir} | opts1]
        )
    ]
  end

  defp get_module_info(forms) do
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
          report_error('syntax error in input.')
          :erlang.error(:badarg)

        {:EXIT, r} ->
          exit(r)

        r ->
          throw(r)
      end

    {name, vars} =
      case :lists.keyfind(:module, 1, l) do
        {:module, {_N, _Vs} = nVs} ->
          nVs

        {:module, n} ->
          {n, :none}

        false ->
          report_error('in source code: module name missing.')
          exit(:error)
      end

    case :lists.keyfind(:errors, 1, l) do
      {:errors, ds} when ds !== [] ->
        report_errors(ds, name)
        exit(:error)

      _ ->
        :ok
    end

    case :lists.keyfind(:warnings, 1, l) do
      {:warnings, ds1} when ds1 !== [] ->
        report_warnings(ds1, name)

      _ ->
        :ok
    end

    functions =
      case :lists.keyfind(:functions, 1, l) do
        {:functions, fs} ->
          :ordsets.from_list(fs)

        _ ->
          []
      end

    exports =
      case :lists.keyfind(:exports, 1, l) do
        {:exports, es} ->
          :ordsets.from_list(es)

        _ ->
          []
      end

    imports =
      case :lists.keyfind(:imports, 1, l) do
        {:imports, is} ->
          expand_imports(is, name)

        _ ->
          []
      end

    attributes =
      case :lists.keyfind(:attributes, 1, l) do
        {:attributes, as} ->
          :ordsets.from_list(as)

        _ ->
          []
      end

    records =
      case :lists.keyfind(:records, 1, l) do
        {:records, rs} ->
          fold_record_fields(rs)

        _ ->
          []
      end

    check_records(records, name)

    r_module(
      name: name,
      vars: vars,
      functions: functions,
      exports: :ordsets.intersection(exports, functions),
      aliases: imports,
      attributes: attributes,
      records: records
    )
  end

  defp fold_record_fields(rs) do
    for {n, fs} <- rs do
      {n,
       for f <- fs do
         fold_record_field(f)
       end}
    end
  end

  defp fold_record_field({_Name, {:none, _Type}} = none) do
    none
  end

  defp fold_record_field({name, {f, type}}) do
    case :erl_syntax.is_literal(f) do
      true ->
        {name, {:value, :erl_syntax.concrete(f)}, type}

      false ->
        {name, {:hash, :erlang.phash(f, 16_777_215)}, type}
    end
  end

  defp fold_record_field({_Name, :none} = none) do
    none
  end

  defp fold_record_field({name, f}) do
    case :erl_syntax.is_literal(f) do
      true ->
        {name, {:value, :erl_syntax.concrete(f)}}

      false ->
        {name, {:hash, :erlang.phash(f, 16_777_215)}}
    end
  end

  defp report_errors([d | ds], name) do
    report_error('error: ' ++ error_text(d, name))
    report_errors(ds, name)
  end

  defp report_errors([], _) do
    :ok
  end

  defp report_warnings([d | ds], name) do
    report_warning(error_text(d, name))
    report_errors(ds, name)
  end

  defp report_warnings([], _) do
    :ok
  end

  defp error_text(d, name) do
    case d do
      {l, m, e} when is_integer(l) and is_atom(m) ->
        case (try do
                m.format_error(e)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          s when is_list(s) ->
            :io_lib.fwrite('`~w\', line ~w: ~ts.', [name, l, s])

          _ ->
            error_text_1(d, name)
        end

      _E ->
        error_text_1(d, name)
    end
  end

  defp error_text_1(d, name) do
    :io_lib.fwrite('error: `~w\', ~tP.', [name, d, 15])
  end

  defp check_records(rs, name) do
    case duplicates(
           for {n, _} <- rs do
             n
           end
         ) do
      [] ->
        :ok

      ns ->
        report_error('in module `~w\': multiply defined records: ~tp.', [name, ns])
        exit(:error)
    end
  end

  defp expand_imports(is, name) do
    fs =
      :ordsets.from_list(
        :lists.append(
          for {m, fs} <- is do
            for f <- fs do
              {m, f}
            end
          end
        )
      )

    as = :erl_syntax_lib.function_name_expansions(fs)

    case duplicates(
           for {n, _} <- as do
             n
           end
         ) do
      [] ->
        :ordsets.from_list(as)

      ns ->
        report_error('in module `~w\': multiply imported functions: ~tp.', [name, ns])
        exit(:error)
    end
  end

  defp open_output_file(fName) do
    case (try do
            :file.open(fName, [:write])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, fD} ->
        fD

      {:error, _} = error ->
        error_open_output(fName)
        exit(error)

      {:EXIT, r} ->
        error_open_output(fName)
        exit(r)

      r ->
        error_open_output(fName)
        exit(r)
    end
  end

  defp output_encoding(fD, opts) do
    case :proplists.get_value(:encoding, opts) do
      :undefined ->
        :ok =
          :io.setopts(
            fD,
            [{:encoding, :epp.default_encoding()}]
          )

      encoding ->
        :ok = :io.setopts(fD, [{:encoding, encoding}])
        encS = :epp.encoding_to_string(encoding)
        :ok = :io.fwrite(fD, "%% ~s\n", [encS])
    end
  end

  defp read_module(name, options) do
    case file_type(name) do
      {:value, _} ->
        read_module_1(name, options)

      :none ->
        rules = :proplists.get_value(:find_src_rules, options)

        case find_src(name, rules) do
          {:error, _} ->
            read_module_1(name, options)

          {:ok, name1} ->
            read_module_1(name1, options)
        end
    end
  end

  defp read_module_1(name, options) do
    verbose('reading module `~ts\'.', [filename(name)], options)
    {forms, enc} = read_module_2(name, options)

    case :proplists.get_bool(:comments, options) do
      false ->
        {forms, enc}

      true ->
        comments = :erl_comment_scan.file(name)
        {:erl_recomment.recomment_forms(forms, comments), enc}
    end
  end

  defp read_module_2(name, options) do
    case read_module_3(name, options) do
      {:ok, forms} ->
        check_forms(forms, name)
        enc = :epp.read_encoding(name)
        {forms, enc}

      {:error, _} = error ->
        error_read_file(name)
        exit(error)
    end
  end

  defp read_module_3(name, options) do
    case :proplists.get_bool(:preprocess, options) do
      false ->
        :epp_dodger.parse_file(name)

      true ->
        read_module_4(name, options)
    end
  end

  defp read_module_4(name, options) do
    includes =
      :proplists.append_values(
        :includes,
        options
      ) ++ [:filename.dirname(name), '.']

    macros =
      :proplists.append_values(
        :macros,
        options
      ) ++ []

    :epp.parse_file(name, includes, macros)
  end

  defp check_forms([f | fs], file) do
    case :erl_syntax.type(f) do
      :error_marker ->
        s =
          case :erl_syntax.error_marker_info(f) do
            {_, m, d} ->
              m.format_error(d)

            _ ->
              'unknown error'
          end

        report_error(
          'in file `~ts\' at line ~w:\n  ~ts',
          [filename(file), :erl_syntax.get_pos(f), s]
        )

        exit(:error)

      _ ->
        check_forms(fs, file)
    end
  end

  defp check_forms([], _) do
    :ok
  end

  defp find_src(name, :undefined) do
    :filelib.find_source(filename(name))
  end

  defp find_src(name, rules) do
    :filelib.find_source(filename(name), rules)
  end

  defp file_type(name) do
    case (try do
            :file.read_file_info(name)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, env} ->
        {:value, r_file_info(env, :type)}

      {:error, :enoent} ->
        :none

      {:error, _} = error ->
        error_read_file_info(name)
        exit(error)

      {:EXIT, r} ->
        error_read_file_info(name)
        exit(r)

      r ->
        error_read_file_info(name)
        throw(r)
    end
  end

  defp write_module(tree, name, dir, opts) do
    name1 = filename(name)
    dir1 = filename(dir)

    base =
      cond do
        dir1 === '' ->
          name1

        true ->
          case file_type(dir1) do
            {:value, :directory} ->
              :ok

            {:value, _} ->
              report_error('`~ts\' is not a directory.', [dir1])
              exit(:error)

            :none ->
              case :file.make_dir(dir1) do
                :ok ->
                  verbose('created directory `~ts\'.', [dir1], opts)
                  :ok

                e ->
                  report_error('failed to create directory `~ts\'.', [dir1])
                  exit({:make_dir, e})
              end
          end

          :filename.join(dir1, name1)
      end

    suffix = :proplists.get_value(:suffix, opts, '')
    file = base ++ suffix

    case :proplists.get_bool(:backups, opts) do
      true ->
        backup_file(file, opts)

      false ->
        :ok
    end

    printer = :proplists.get_value(:printer, opts)
    fD = open_output_file(file)
    :ok = output_encoding(fD, opts)
    verbose('writing to file `~ts\'.', [file], opts)

    v =
      try do
        {:ok, output(fD, printer, tree, opts)}
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    :ok = :file.close(fD)

    case v do
      {:ok, _} ->
        file

      {:EXIT, r} ->
        error_write_file(file)
        exit(r)

      r ->
        error_write_file(file)
        throw(r)
    end
  end

  defp output(fD, printer, tree, opts) do
    :io.put_chars(fD, printer.(tree, opts))
    :io.nl(fD)
  end

  defp backup_file(name, opts) do
    case file_type(name) do
      {:value, :regular} ->
        backup_file_1(name, opts)

      {:value, _} ->
        error_backup_file(name)
        exit(:error)

      :none ->
        :ok
    end
  end

  defp backup_file_1(name, opts) do
    name1 = filename(name)
    suffix = :proplists.get_value(:backup_suffix, opts, '')

    dest =
      :filename.join(
        :filename.dirname(name1),
        :filename.basename(name1) ++ suffix
      )

    case (try do
            :file.rename(name1, dest)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :ok ->
        verbose('made backup of file `~ts\'.', [name1], opts)

      {:error, r} ->
        error_backup_file(name1)
        exit({:error, r})

      {:EXIT, r} ->
        error_backup_file(name1)
        exit(r)

      r ->
        error_backup_file(name1)
        throw(r)
    end
  end

  defp tidy(tree, opts) do
    case :proplists.get_bool(:tidy, opts) do
      true ->
        verbose('tidying final module.', opts)
        :erl_tidy.module(tree, [:quiet])

      false ->
        tree
    end
  end

  defp make_attributes(as) do
    for a <- as do
      make_attribute(a)
    end
  end

  defp make_attribute({name, term}) do
    :erl_syntax.attribute(
      :erl_syntax.atom(name),
      [:erl_syntax.abstract(term)]
    )
  end

  defp is_auto_import({f, a}) do
    :erl_internal.bif(f, a)
  end

  defp timestamp() do
    {{yr, mth, dy}, {hr, mt, sc}} = :erlang.localtime()

    :lists.flatten(
      :io_lib.fwrite(
        'Created by Igor ~w-~2.2.0w-~2.2.0w, ~2.2.0w:~2.2.0w:~2.2.0w.',
        [yr, mth, dy, hr, mt, sc]
      )
    )
  end

  defp filename([c | t]) when is_integer(c) and c > 0 do
    [c | filename(t)]
  end

  defp filename([h | t]) do
    filename(h) ++ filename(t)
  end

  defp filename([]) do
    []
  end

  defp filename(n) when is_atom(n) do
    :erlang.atom_to_list(n)
  end

  defp filename(n) do
    report_error('bad filename: `~tP\'.', [n, 25])
    exit(:error)
  end

  defp duplicates(xs) do
    :ordsets.from_list(xs -- :ordsets.from_list(xs))
  end

  defp split_list(f, l) do
    split_list(l, f, [], [])
  end

  defp split_list([h | t], f, a1, a2) do
    case f.(h) do
      true ->
        split_list(t, f, [h | a1], a2)

      false ->
        split_list(t, f, a1, [h | a2])
    end
  end

  defp split_list([], _, a1, a2) do
    {:lists.reverse(a1), :lists.reverse(a2)}
  end

  defp rewrite(source, target) do
    :erl_syntax.copy_attrs(source, target)
  end

  defp comment_note([l | ls]) do
    comment(['Note from Igor: ' ++ l | ls], '%! ')
  end

  defp comment(txt) do
    comment(txt, '% ')
  end

  defp comment(txt, prefix) do
    :erl_syntax.comment(
      prefix_lines(
        split_lines(txt),
        prefix
      )
    )
  end

  defp prefix_lines([l | ls], prefix) do
    [prefix ++ l | prefix_lines(ls, prefix)]
  end

  defp prefix_lines([], _) do
    []
  end

  defp split_lines(ls) do
    split_lines(ls, [])
  end

  defp split_lines([l | ls], ls1) do
    split_lines(ls, split_lines(l, [], ls1))
  end

  defp split_lines([], ls1) do
    :lists.reverse(ls1)
  end

  defp split_lines([?\r, ?\n | cs], cs1, ls) do
    split_lines_1(cs, cs1, ls)
  end

  defp split_lines([?\r | cs], cs1, ls) do
    split_lines_1(cs, cs1, ls)
  end

  defp split_lines([?\n | cs], cs1, ls) do
    split_lines_1(cs, cs1, ls)
  end

  defp split_lines([c | cs], cs1, ls) do
    split_lines(cs, [c | cs1], ls)
  end

  defp split_lines([], cs, ls) do
    [:lists.reverse(cs) | ls]
  end

  defp split_lines_1(cs, cs1, ls) do
    split_lines(cs, [], [:lists.reverse(cs1) | ls])
  end

  defp warning_unsafe_call(name, module, target) do
    report_warning('call to `~tw\' in module `~w\' possibly unsafe in `~s\'.', [
      name,
      module,
      target
    ])
  end

  defp warning_apply_2(module, target) do
    report_warning('call to `apply/2\' in module `~w\' possibly unsafe in `~s\'.', [
      module,
      target
    ])
  end

  defp error_open_output(name) do
    report_error('cannot open file `~ts\' for output.', [filename(name)])
  end

  defp error_read_file(name) do
    report_error('error reading file `~ts\'.', [filename(name)])
  end

  defp error_read_file_info(name) do
    report_error('error getting file info: `~ts\'.', [filename(name)])
  end

  defp error_write_file(name) do
    report_error('error writing to file `~ts\'.', [filename(name)])
  end

  defp error_backup_file(name) do
    report_error('could not create backup of file `~ts\'.', [filename(name)])
  end

  defp verbose(s, opts) do
    verbose(s, [], opts)
  end

  defp verbose(s, vs, opts) do
    case :proplists.get_bool(:verbose, opts) do
      true ->
        report(s, vs)

      false ->
        :ok
    end
  end

  defp report_error(s) do
    report_error(s, [])
  end

  defp report_error(s, vs) do
    report(s, vs)
  end

  defp report_warning(s) do
    report_warning(s, [])
  end

  defp report_warning(s, vs) do
    report('warning: ' ++ s, vs)
  end

  defp report(s, vs) do
    :io.fwrite(:lists.concat([:igor, ': ', s, '\n']), vs)
  end
end
