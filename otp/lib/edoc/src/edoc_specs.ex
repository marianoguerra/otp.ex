defmodule :m_edoc_specs do
  use Bitwise
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
  Record.defrecord(:r_t_spec, :t_spec, name: :undefined, type: :undefined, defs: [])

  Record.defrecord(:r_t_typedef, :t_typedef,
    name: :undefined,
    args: :undefined,
    type: :undefined,
    defs: []
  )

  Record.defrecord(:r_t_throws, :t_throws,
    type: :undefined,
    defs: []
  )

  Record.defrecord(:r_t_def, :t_def,
    name: :undefined,
    type: :undefined
  )

  Record.defrecord(:r_t_name, :t_name, app: [], module: [], name: [])
  Record.defrecord(:r_t_var, :t_var, a: [], name: [])
  Record.defrecord(:r_t_type, :t_type, a: [], name: :undefined, args: [])
  Record.defrecord(:r_t_union, :t_union, a: [], types: [])
  Record.defrecord(:r_t_fun, :t_fun, a: [], args: :undefined, range: :undefined)
  Record.defrecord(:r_t_tuple, :t_tuple, a: [], types: [])
  Record.defrecord(:r_t_list, :t_list, a: [], type: :undefined)
  Record.defrecord(:r_t_nil, :t_nil, a: [])

  Record.defrecord(:r_t_nonempty_list, :t_nonempty_list,
    a: [],
    type: :undefined
  )

  Record.defrecord(:r_t_atom, :t_atom, a: [], val: :undefined)
  Record.defrecord(:r_t_integer, :t_integer, a: [], val: :undefined)
  Record.defrecord(:r_t_integer_range, :t_integer_range, a: [], from: :undefined, to: :undefined)
  Record.defrecord(:r_t_binary, :t_binary, a: [], base_size: 0, unit_size: 0)
  Record.defrecord(:r_t_float, :t_float, a: [], val: :undefined)
  Record.defrecord(:r_t_record, :t_record, a: [], name: :undefined, fields: [])
  Record.defrecord(:r_t_field, :t_field, a: [], name: :undefined, type: :undefined)
  Record.defrecord(:r_t_paren, :t_paren, a: [], type: :undefined)
  Record.defrecord(:r_t_map, :t_map, a: [], types: [])

  Record.defrecord(:r_t_map_field, :t_map_field,
    a: [],
    assoc_type: :undefined,
    k_type: :undefined,
    v_type: :undefined
  )

  def type(form, typeDocs) do
    {name, data0} = analyze_type_attribute(form)

    {typeName, type, args, doc} =
      case data0 do
        {r, fs} ->
          :record = name
          l = :erl_syntax.get_pos(form)
          {{:record, r}, {:type, l, :record, [{:atom, l, r} | fs]}, [], ''}

        {n, t, as} ->
          :type = tag(name)

          doc0 =
            case :dict.find(
                   {n, length(as)},
                   typeDocs
                 ) do
              {:ok, doc1} ->
                doc1

              :error ->
                ''
            end

          {r_t_name(name: n), t, as, doc0}
      end

    r_tag(
      name: :type,
      line: get_line(:erlang.element(2, type)),
      origin: :code,
      data:
        {r_t_typedef(name: typeName, args: d2e(args), type: d2e(opaque2abstr(name, type))), doc}
    )
  end

  def spec(form) do
    {name, _Arity, typeSpecs} = get_spec(form)

    r_tag(
      name: :spec,
      line:
        get_line(
          :erlang.element(
            2,
            :lists.nth(1, typeSpecs)
          )
        ),
      origin: :code,
      data:
        for typeSpec <- typeSpecs do
          aspec(d2e(typeSpec), name)
        end
    )
  end

  def dummy_spec(form) do
    {r_t_name(name: name), arity, typeSpecs} = get_spec(form)
    as = :lists.join(',', :lists.duplicate(arity, '_X'))
    s = :lists.flatten(:io_lib.format('~p(~s) -> true\n', [name, as]))
    r_tag(name: :spec, line: get_line(:erlang.element(2, hd(typeSpecs))), origin: :code, data: s)
  end

  def docs(forms, commentFun) do
    find_type_docs(forms, [], commentFun)
  end

  def add_data(entries, opts, file, module) do
    typeDefs0 = espec_types(entries)
    typeTable = :ets.new(:etypes, [:ordered_set])
    es1 = expand_records(entries, typeDefs0, typeTable, opts, file, module)

    es =
      for e <- es1 do
        use_tags(e, typeTable)
      end

    true = :ets.delete(typeTable)
    es
  end

  defp aspec(r_t_spec() = spec, name) do
    r_t_spec(spec, name: name)
  end

  defp aspec(type, name) do
    r_t_spec(name: name, type: type)
  end

  defp get_spec(form) do
    {:spec, data0} = :erl_syntax_lib.analyze_wild_attribute(form)

    case data0 do
      {{f, a}, d} ->
        {r_t_name(name: f), a, d}

      {{m, f, a}, d} ->
        {r_t_name(module: m, name: f), a, d}
    end
  end

  defp find_type_docs([], cs, _Fun) do
    :dict.from_list(cs)
  end

  defp find_type_docs([f | fs], cs, fun) do
    try do
      get_name_and_last_line(f)
    catch
      _, _ ->
        find_type_docs(fs, cs, fun)
    else
      {name, lastTypeLine} ->
        c0 = :erl_syntax.comment(['% @type f(). '])
        c1 = :erl_syntax.set_pos(c0, lastTypeLine)

        c2 = [
          c1
          | for c <- :erl_syntax.get_postcomments(f),
                :erl_syntax.get_pos(c) >= lastTypeLine do
              c
            end
        ]

        c3 = collect_comments(fs, lastTypeLine)

        r_tag(data: doc0) =
          fun.(
            :lists.reverse(c2 ++ c3),
            lastTypeLine
          )

        case strip(doc0) do
          '' ->
            find_type_docs(fs, cs, fun)

          doc ->
            w = :edoc_wiki.parse_xml(doc, lastTypeLine)
            find_type_docs(fs, [{name, w} | cs], fun)
        end
    end
  end

  defp collect_comments([], _Line) do
    []
  end

  defp collect_comments([f | fs], line) do
    l1 = :erl_syntax.get_pos(f)

    cond do
      l1 === line + 1 or l1 === line ->
        case is_comment(f) do
          true ->
            [f | collect_comments(fs, l1)]

          false ->
            []
        end

      true ->
        []
    end
  end

  defp is_comment(f) do
    :erl_syntax_lib.analyze_form(f) === :comment
  end

  defp strip('') do
    ''
  end

  defp strip([?\n | s]) do
    s
  end

  defp strip([_ | s]) do
    strip(s)
  end

  defp get_name_and_last_line(f) do
    {name, data} = analyze_type_attribute(f)
    :type = :edoc_specs.tag(name)
    attr = {:attribute, :erl_syntax.get_pos(f), name, data}

    fun = fn a ->
      line = get_line(a)

      case :erlang.get(:"$max_line") do
        max when max < line ->
          _ = :erlang.put(:"$max_line", line)

        _ ->
          :ok
      end
    end

    :undefined = :erlang.put(:"$max_line", 0)
    _ = :erl_parse.map_anno(fun, attr)
    line = :erlang.erase(:"$max_line")

    typeName =
      case data do
        {n, _T, as} when is_atom(n) ->
          {n, length(as)}
      end

    {typeName, line}
  end

  defp get_line(anno) do
    :erl_anno.line(anno)
  end

  defp espec_types(entries) do
    tags = get_all_tags(entries)

    commTs =
      for r_tag(
            name: :type,
            origin: :comment
          ) = t <- tags do
        type_name(t)
      end

    cT = :sets.from_list(commTs)

    for r_tag(name: name, origin: :code) = t <- tags,
        tag(name) === :type,
        not :sets.is_element(type_name(t), cT) do
      t
    end
  end

  defp get_all_tags(es) do
    :lists.flatmap(
      fn r_entry(data: ts) ->
        ts
      end,
      es
    )
  end

  defp opaque2abstr(:opaque, _T) do
    :undefined
  end

  defp opaque2abstr(:record, t) do
    t
  end

  defp opaque2abstr(:type, t) do
    t
  end

  defp use_tags(r_entry(data: ts) = e, typeTable) do
    use_tags(ts, e, typeTable, [])
  end

  defp use_tags([], e, _TypeTable, nTs) do
    r_entry(e, data: :lists.reverse(nTs))
  end

  defp use_tags([r_tag(origin: :code) = t | ts], e, typeTable, nTs) do
    case tag(r_tag(t, :name)) do
      :spec ->
        args = params(t, r_entry(e, :args))
        use_tags(ts, r_entry(e, args: args), typeTable, [t | nTs])

      :type ->
        typeName = type_name(t)

        case :ets.lookup(typeTable, typeName) do
          [{{{:record, _}, _}, _, _}] ->
            use_tags(ts, e, typeTable, nTs)

          [{_, _, :not_seen}] ->
            use_tags(ts, e, typeTable, nTs)

          [] ->
            use_tags(ts, e, typeTable, nTs)

          [{^typeName, tag, :seen}] ->
            use_tags(ts, e, typeTable, [tag | nTs])
        end
    end
  end

  defp use_tags([t | ts], e, typeTable, nTs) do
    use_tags(ts, e, typeTable, [t | nTs])
  end

  defp params(r_tag(name: :spec, data: data), default)
       when is_list(data) do
    for r_t_spec(type: r_t_fun(args: as)) <- data do
      parms(as, default)
    end
  end

  defp parms([], []) do
    []
  end

  defp parms([a | as], [d | ds]) do
    [param(a, d) | parms(as, ds)]
  end

  defp param(r_t_paren(type: type), default) do
    param(type, default)
  end

  defp param(r_t_record(name: r_t_atom(val: name)) = t, default) do
    atomList = :erlang.atom_to_list(name)

    case atomList === :lists.flatten(:io_lib.write_atom(name)) do
      true ->
        :erlang.list_to_atom(capitalize(atomList))

      false ->
        arg_name(:erlang.element(2, t), default)
    end
  end

  defp param(t, default) do
    arg_name(:erlang.element(2, t), default)
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

  defp arg_name([], default) do
    default
  end

  defp arg_name([a | as], default) do
    case is_name(a) do
      true ->
        a

      false ->
        arg_name(as, default)
    end
  end

  defp is_name(a) do
    is_atom(a)
  end

  defp d2e(t) do
    d2e(t, 0)
  end

  defp d2e({:ann_type, _, [v, t0]}, prec) do
    {_L, p, r} = :erl_parse.type_inop_prec(:"::")
    t1 = d2e(t0, r)
    t = :erlang.setelement(2, t1, [:erlang.element(3, v) | :erlang.element(2, t1)])
    maybe_paren(p, prec, t)
  end

  defp d2e(
         {:remote_type, _, [{:atom, _, m}, {:atom, _, f}, ts0]},
         _Prec
       ) do
    ts = d2e(ts0)

    typevar_anno(
      r_t_type(name: r_t_name(module: m, name: f), args: ts),
      ts
    )
  end

  defp d2e(
         {:type, _, :fun, [{:type, _, :product, as0}, ran0]},
         _Prec
       ) do
    ts = [ran | as] = d2e([ran0 | as0])
    typevar_anno(r_t_fun(args: as, range: ran), ts)
  end

  defp d2e(
         {:type, _, :fun, [a0 = {:type, _, :any}, ran0]},
         _Prec
       ) do
    ts = [a, ran] = d2e([a0, ran0])
    typevar_anno(r_t_fun(args: [a], range: ran), ts)
  end

  defp d2e({:type, _, :fun, []}, _Prec) do
    r_t_type(name: r_t_name(name: :function), args: [])
  end

  defp d2e({:type, _, :any}, _Prec) do
    r_t_var(name: :...)
  end

  defp d2e({:type, _, nil, []}, _Prec) do
    r_t_nil()
  end

  defp d2e({:paren_type, _, [t]}, prec) do
    d2e(t, prec)
  end

  defp d2e({:type, _, :list, [t0]}, _Prec) do
    t = d2e(t0)
    typevar_anno(r_t_list(type: t), [t])
  end

  defp d2e({:type, _, :nonempty_list, [t0]}, _Prec) do
    t = d2e(t0)
    typevar_anno(r_t_nonempty_list(type: t), [t])
  end

  defp d2e({:type, _, :bounded_fun, [t, gs]}, _Prec) do
    [f0 | defs] = d2e([t | gs])
    f = :erlang.setelement(2, f0, :lists.keydelete(:type_variables, 1, :erlang.element(2, f0)))
    r_t_spec(type: typevar_anno(f, [f0]), defs: defs)
  end

  defp d2e({:type, _, :range, [v1, v2]}, prec) do
    {_L, p, _R} = :erl_parse.type_inop_prec(:..)
    {:integer, _, i1} = :erl_eval.partial_eval(v1)
    {:integer, _, i2} = :erl_eval.partial_eval(v2)
    t0 = r_t_integer_range(from: i1, to: i2)
    maybe_paren(p, prec, t0)
  end

  defp d2e({:type, _, :constraint, [sub, ts0]}, _Prec) do
    case {sub, ts0} do
      {{:atom, _, :is_subtype}, [{:var, _, n}, t0]} ->
        ts = [t] = d2e([t0])
        r_t_def(name: r_t_var(name: n), type: typevar_anno(t, ts))

      {{:atom, _, :is_subtype}, [sT0, t0]} ->
        ts = [sT, t] = d2e([sT0, t0])
        r_t_def(name: sT, type: typevar_anno(t, ts))

      _ ->
        throw_error(get_line(:erlang.element(2, sub)), 'cannot handle guard', [])
    end
  end

  defp d2e({:type, _, :union, ts0}, prec) do
    {_L, p, r} = :erl_parse.type_inop_prec(:|)
    ts = d2e(ts0, r)
    t = maybe_paren(p, prec, r_t_union(types: ts))
    typevar_anno(t, ts)
  end

  defp d2e({:type, _, :tuple, :any}, _Prec) do
    r_t_type(name: r_t_name(name: :tuple), args: [])
  end

  defp d2e({:type, _, :binary, [base, unit]}, _Prec) do
    {:integer, _, b} = :erl_eval.partial_eval(base)
    {:integer, _, u} = :erl_eval.partial_eval(unit)
    r_t_binary(base_size: b, unit_size: u)
  end

  defp d2e({:type, _, :map, :any}, _Prec) do
    r_t_type(name: r_t_name(name: :map), args: [])
  end

  defp d2e({:type, _, :map, es}, _Prec) do
    r_t_map(types: d2e(es))
  end

  defp d2e({:type, _, :map_field_assoc, [k, v]}, prec) do
    t = r_t_map_field(assoc_type: :assoc, k_type: d2e(k), v_type: d2e(v))
    {p, _R} = :erl_parse.type_preop_prec(:"#")
    maybe_paren(p, prec, t)
  end

  defp d2e({:type, _, :map_field_exact, [k, v]}, prec) do
    t = r_t_map_field(assoc_type: :exact, k_type: d2e(k), v_type: d2e(v))
    {p, _R} = :erl_parse.type_preop_prec(:"#")
    maybe_paren(p, prec, t)
  end

  defp d2e({:type, _, :tuple, ts0}, _Prec) do
    ts = d2e(ts0)
    typevar_anno(r_t_tuple(types: ts), ts)
  end

  defp d2e({:type, _, :record, [name | fs0]}, prec) do
    atom = r_t_atom(val: :erlang.element(3, name))
    fs = d2e(fs0)
    {p, _R} = :erl_parse.type_preop_prec(:"#")
    t = maybe_paren(p, prec, r_t_record(name: atom, fields: fs))
    typevar_anno(t, fs)
  end

  defp d2e({:type, _, :field_type, [name, type0]}, prec) do
    {_L, p, r} = :erl_parse.type_inop_prec(:"::")
    type = maybe_paren(p, prec, d2e(type0, r))

    t =
      r_t_field(
        name: r_t_atom(val: :erlang.element(3, name)),
        type: type
      )

    typevar_anno(t, [type])
  end

  defp d2e(
         {:typed_record_field, {:record_field, l, name}, type},
         prec
       ) do
    d2e({:type, l, :field_type, [name, type]}, prec)
  end

  defp d2e(
         {:typed_record_field, {:record_field, l, name, _E}, type},
         prec
       ) do
    d2e({:type, l, :field_type, [name, type]}, prec)
  end

  defp d2e({:record_field, l, _Name, _E} = f, prec) do
    d2e(
      {:typed_record_field, f, {:type, l, :any, []}},
      prec
    )
  end

  defp d2e({:record_field, l, _Name} = f, prec) do
    d2e(
      {:typed_record_field, f, {:type, l, :any, []}},
      prec
    )
  end

  defp d2e({:type, _, name, types0}, _Prec) do
    types = d2e(types0)
    typevar_anno(r_t_type(name: r_t_name(name: name), args: types), types)
  end

  defp d2e({:user_type, _, name, types0}, _Prec) do
    types = d2e(types0)
    typevar_anno(r_t_type(name: r_t_name(name: name), args: types), types)
  end

  defp d2e({:var, _, :_}, _Prec) do
    r_t_type(name: r_t_name(name: :term))
  end

  defp d2e({:var, _, typeName}, _Prec) do
    typeVar = :ordsets.from_list([typeName])
    t = r_t_var(name: typeName)

    t1 =
      :erlang.setelement(2, t, [
        {:type_variables, typeVar}
        | :erlang.element(
            2,
            t
          )
      ])

    :erlang.setelement(2, t1, [typeName | :erlang.element(2, t1)])
  end

  defp d2e(l, prec) when is_list(l) do
    for t <- l do
      d2e(t, prec)
    end
  end

  defp d2e({:atom, _, a}, _Prec) do
    r_t_atom(val: a)
  end

  defp d2e(:undefined = u, _Prec) do
    u
  end

  defp d2e(expr, _Prec) do
    {:integer, _, i} = :erl_eval.partial_eval(expr)
    r_t_integer(val: i)
  end

  defp typevar_anno(type, ts) do
    vs = typevars(ts)

    case :ordsets.to_list(vs) do
      [] ->
        type

      _ ->
        :erlang.setelement(2, type, [{:type_variables, vs} | :erlang.element(2, type)])
    end
  end

  defp typevars(ts) do
    :ordsets.union(get_typevars(ts))
  end

  defp get_typevars(ts) do
    for t <- ts, t !== :undefined, {:type_variables, vs} <- :erlang.element(2, t) do
      vs
    end
  end

  defp maybe_paren(p, prec, t) when p < prec do
    r_t_paren(type: t)
  end

  defp maybe_paren(_P, _Prec, t) do
    t
  end

  Record.defrecord(:r_parms, :parms,
    tab: :undefined,
    warn: :undefined,
    file: :undefined,
    line: :undefined
  )

  defp expand_records(entries, typeDefs, dT, opts, file, module) do
    typeList =
      for t <- typeDefs do
        {type_name(t), t, :not_seen}
      end

    true = :ets.insert(dT, typeList)
    warn = :proplists.get_value(:report_missing_types, opts, false) === true
    p = r_parms(tab: dT, warn: warn, file: file, line: 0)

    exportedTypes =
      for {:export_type, ts} <- r_module(module, :attributes),
          is_list(ts),
          {n, i} <- ts,
          :ets.member(dT, name = {r_t_name(name: n), i}) do
        name
      end

    _ =
      :lists.foreach(
        fn {n, a} ->
          true = seen_type(n, a, p)
        end,
        exportedTypes
      )

    entries(entries, p, opts)
  end

  defp entries([e0 | es], p, opts) do
    e =
      case :edoc_data.hidden_filter([e0], opts) do
        [] ->
          e0

        [_] ->
          r_entry(e0, data: specs(r_entry(e0, :data), p))
      end

    [e | entries(es, p, opts)]
  end

  defp entries([], _P, _Opts) do
    []
  end

  defp specs(
         [
           r_tag(line: l, name: :spec, origin: :code, data: specs) = tag0
           | tags
         ],
         p0
       ) do
    p = r_parms(p0, line: l)

    data =
      for spec <- specs do
        r_t_spec(type: type0, defs: defs0) = spec
        type = xrecs(type0, p)
        defs = xrecs(defs0, p)
        r_t_spec(spec, type: type, defs: defs)
      end

    tag = r_tag(tag0, data: data)
    [tag | specs(tags, p)]
  end

  defp specs([tag | tags], p) do
    [tag | specs(tags, p)]
  end

  defp specs([], _P) do
    []
  end

  defp xrecs(r_t_def(type: type0) = t, p) do
    type = xrecs(type0, p)
    r_t_def(t, type: type)
  end

  defp xrecs(r_t_type(name: name, args: args0) = t, p) do
    args = xrecs(args0, p)
    nArgs = length(args)
    true = seen_type(name, nArgs, p)
    r_t_type(t, args: args)
  end

  defp xrecs(r_t_var() = t, _P) do
    t
  end

  defp xrecs(r_t_fun(args: args0, range: range0) = t, p) do
    args = xrecs(args0, p)
    range = xrecs(range0, p)
    r_t_fun(t, args: args, range: range)
  end

  defp xrecs(r_t_map(types: ts0) = t, p) do
    ts = xrecs(ts0, p)
    r_t_map(t, types: ts)
  end

  defp xrecs(r_t_map_field(k_type: kt, v_type: vt) = t, p) do
    r_t_map_field(t, k_type: xrecs(kt, p), v_type: xrecs(vt, p))
  end

  defp xrecs(r_t_tuple(types: types0) = t, p) do
    types = xrecs(types0, p)
    r_t_tuple(t, types: types)
  end

  defp xrecs(r_t_list(type: type0) = t, p) do
    type = xrecs(type0, p)
    r_t_list(t, type: type)
  end

  defp xrecs(r_t_nil() = t, _P) do
    t
  end

  defp xrecs(r_t_paren(type: type0) = t, p) do
    type = xrecs(type0, p)
    r_t_paren(t, type: type)
  end

  defp xrecs(r_t_nonempty_list(type: type0) = t, p) do
    type = xrecs(type0, p)
    r_t_nonempty_list(t, type: type)
  end

  defp xrecs(r_t_atom() = t, _P) do
    t
  end

  defp xrecs(r_t_integer() = t, _P) do
    t
  end

  defp xrecs(r_t_integer_range() = t, _P) do
    t
  end

  defp xrecs(r_t_binary() = t, _P) do
    t
  end

  defp xrecs(r_t_float() = t, _P) do
    t
  end

  defp xrecs(r_t_union(types: types0) = t, p) do
    types = xrecs(types0, p)
    r_t_union(t, types: types)
  end

  defp xrecs(r_t_record(fields: fields0) = t, p) do
    fields1 = xrecs(fields0, p)
    r_t_record(name: r_t_atom(val: name)) = t
    rName = {:record, name}
    true = seen_type(rName, 0, p)
    fields = select_fields(fields1, rName, r_parms(p, :tab))
    r_t_record(t, fields: fields)
  end

  defp xrecs(r_t_field(type: type0) = t, p) do
    type = xrecs(type0, p)
    r_t_field(t, type: type)
  end

  defp xrecs(:undefined = t, _P) do
    t
  end

  defp xrecs([] = t, _P) do
    t
  end

  defp xrecs([e0 | es0], p) do
    [xrecs(e0, p) | xrecs(es0, p)]
  end

  defp seen_type(n, nArgs, p) do
    typeName = {n, nArgs}
    r_parms(tab: dT) = p

    case {:ets.lookup(dT, typeName), n} do
      {[{^typeName, _, :seen}], _} ->
        true

      {[{^typeName, tagType, :not_seen}], _}
      when r_t_name(n, :module) === [] ->
        expand_datatype(tagType, :proper_type, dT, p)

      {[{^typeName, tagType, :not_seen}], {:record, _}} ->
        expand_datatype(tagType, :record_type, dT, p)

      {[], {:record, r}} ->
        r_parms(warn: w, line: l, file: file) = p

        for _ <- [:EFE_DUMMY_GEN], w do
          :edoc_report.warning(l, file, 'reference to untyped record ~w', [r])
        end

        :ets.insert(dT, {typeName, :fake, :seen})

      {[], _} ->
        true
    end
  end

  defp expand_datatype(tag0, kind, dT, p0) do
    r_tag(line: l, data: {t0, doc}) = tag0
    r_t_typedef(type: type0, defs: []) = t0
    typeName = type_name(tag0)
    true = :ets.update_element(dT, typeName, {3, :seen})
    p = r_parms(p0, line: l)

    type =
      case kind do
        :record_type ->
          r_t_record(fields: fields0) = type0
          fields = xrecs(fields0, p)
          r_t_record(type0, fields: fields)

        :proper_type ->
          xrecs(type0, p)
      end

    tag = r_tag(tag0, data: {r_t_typedef(t0, type: type), doc})
    :ets.insert(dT, {typeName, tag, :seen})
  end

  defp select_fields(fields, name, dT) do
    recordName = {name, 0}

    case :ets.lookup(dT, recordName) do
      [{^recordName, :fake, :seen}] ->
        fields

      [{^recordName, r_tag(data: {t, _Doc}), :seen}] ->
        r_t_typedef(args: [], type: r_t_record(fields: fs), defs: []) = t

        for f <- fs do
          find_field(f, fields)
        end
    end
  end

  defp find_field(f, fs) do
    case :lists.keyfind(r_t_field(f, :name), r_t_field(:name), fs) do
      false ->
        f

      nF ->
        nF
    end
  end

  defp type_name(
         r_tag(
           name: :type,
           data: {r_t_typedef(name: name, args: as), _}
         )
       ) do
    {name, length(as)}
  end

  defp analyze_type_attribute(form) do
    name = :erl_syntax.atom_value(:erl_syntax.attribute_name(form))

    case tag(name) do
      :type ->
        :erl_syntax_lib.analyze_wild_attribute(form)

      _ when name === :record ->
        {:attribute, _, :record, {n, fields}} = :erl_syntax.revert(form)
        {:record, {n, fields}}
    end
  end

  def is_tag(:opaque) do
    true
  end

  def is_tag(:spec) do
    true
  end

  def is_tag(:type) do
    true
  end

  def is_tag(_) do
    false
  end

  def tag(:opaque) do
    :type
  end

  def tag(:spec) do
    :spec
  end

  def tag(:type) do
    :type
  end

  def tag(_) do
    :unknown
  end

  defp throw_error(line, s, a) do
    :edoc_report.error(line, '', :io_lib.format(s, a))
    throw(:error)
  end
end
