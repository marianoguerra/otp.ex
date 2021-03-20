defmodule :m_edoc_tags do
  use Bitwise
  import :edoc_report, only: [error: 3, report: 4, warning: 4]
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

  def tags() do
    all = [:module, :footer, :function, :overview]

    [
      {:author, &parse_contact/4, [:module, :overview]},
      {:copyright, :text, [:module, :overview, :single]},
      {:deprecated, :xml, [:module, :function, :single]},
      {:doc, :xml, [:module, :function, :overview, :single]},
      {:docfile, &parse_file/4, all},
      {:end, :text, all},
      {:equiv, &parse_expr/4, [:function, :single]},
      {:headerfile, &parse_header/4, all},
      {:hidden, :text, [:module, :function, :single]},
      {:param, &parse_param/4, [:function]},
      {:private, :text, [:module, :function, :single]},
      {:reference, :xml, [:module, :footer, :overview]},
      {:returns, :xml, [:function, :single]},
      {:see, &parse_see/4, [:module, :function, :overview]},
      {:since, :text, [:module, :function, :overview, :single]},
      {:spec, &parse_spec/4, [:function, :single]},
      {:throws, &parse_throws/4, [:function, :single]},
      {:title, :text, [:overview, :single]},
      {:TODO, :xml, all},
      {:todo, :xml, all},
      {:type, &parse_typedef/4, [:module, :footer, :function]},
      {:version, :text, [:module, :overview, :single]}
    ]
  end

  defp aliases(:TODO) do
    :todo
  end

  defp aliases(:return) do
    :returns
  end

  defp aliases(t) do
    t
  end

  def tags(flag) do
    for {t, _, fs} <- tags(), :lists.member(flag, fs) do
      t
    end
  end

  def tag_names() do
    for {t, _, _} <- tags() do
      t
    end
  end

  def tag_parsers() do
    for {t, f, _} <- tags() do
      {t, f}
    end
  end

  def scan_lines(ss, l) do
    :lists.reverse(scan_lines(ss, l, []))
  end

  defp scan_lines([s | ss], l, as) do
    scan_lines(s, ss, l, as)
  end

  defp scan_lines([], _L, as) do
    as
  end

  defp scan_lines([?\s | cs], ss, l, as) do
    scan_lines(cs, ss, l, as)
  end

  defp scan_lines([?\t | cs], ss, l, as) do
    scan_lines(cs, ss, l, as)
  end

  defp scan_lines([?@ | cs], ss, l, as) do
    scan_tag(cs, ss, l, as, [])
  end

  defp scan_lines('TODO:' ++ _ = cs, ss, l, as) do
    scan_tag(cs, ss, l, as, [])
  end

  defp scan_lines(_, ss, l, as) do
    scan_lines(ss, l + 1, as)
  end

  defp scan_tag([c | cs], ss, l, as, ts)
       when c >= ?a and
              c <= ?z do
    scan_tag_1(cs, ss, l, as, [c | ts])
  end

  defp scan_tag([c | cs], ss, l, as, ts)
       when c >= ?A and
              c <= ?Z do
    scan_tag_1(cs, ss, l, as, [c | ts])
  end

  defp scan_tag([c | cs], ss, l, as, ts)
       when c >= 192 and
              c <= 255 and c !== 215 and
              c !== 247 do
    scan_tag_1(cs, ss, l, as, [c | ts])
  end

  defp scan_tag([?_ | cs], ss, l, as, ts) do
    scan_tag_1(cs, ss, l, as, [?_ | ts])
  end

  defp scan_tag(_Cs, ss, l, as, _Ts) do
    scan_lines(ss, l + 1, as)
  end

  defp scan_tag_1([c | cs], ss, l, as, ts)
       when c >= ?a and
              c <= ?z do
    scan_tag_1(cs, ss, l, as, [c | ts])
  end

  defp scan_tag_1([c | cs], ss, l, as, ts)
       when c >= ?A and
              c <= ?Z do
    scan_tag_1(cs, ss, l, as, [c | ts])
  end

  defp scan_tag_1([c | cs], ss, l, as, ts)
       when c >= ?0 and
              c <= ?9 do
    scan_tag_1(cs, ss, l, as, [c | ts])
  end

  defp scan_tag_1([c | cs], ss, l, as, ts)
       when c >= 192 and
              c <= 255 and c !== 215 and
              c !== 247 do
    scan_tag_1(cs, ss, l, as, [c | ts])
  end

  defp scan_tag_1([?_ | cs], ss, l, as, ts) do
    scan_tag_1(cs, ss, l, as, [?_ | ts])
  end

  defp scan_tag_1(cs, ss, l, as, ts) do
    scan_tag_2(cs, ss, l, as, {ts, l})
  end

  defp scan_tag_2([?\s | cs], ss, l, as, t) do
    scan_tag_lines(ss, t, [cs], l + 1, as)
  end

  defp scan_tag_2([?\t | cs], ss, l, as, t) do
    scan_tag_lines(ss, t, [cs], l + 1, as)
  end

  defp scan_tag_2([?: | cs], ss, l, as, t) do
    scan_tag_lines(ss, t, [cs], l + 1, as)
  end

  defp scan_tag_2([], ss, l, as, t) do
    scan_tag_lines(ss, t, [[]], l + 1, as)
  end

  defp scan_tag_2(_, ss, l, as, _T) do
    scan_lines(ss, l + 1, as)
  end

  defp scan_tag_lines([s | ss], t, ss1, l, as) do
    scan_tag_lines(s, s, ss, t, ss1, l, as)
  end

  defp scan_tag_lines([], {ts, l1}, ss1, _L, as) do
    [make_tag(ts, l1, ss1) | as]
  end

  defp scan_tag_lines([?\s | cs], s, ss, t, ss1, l, as) do
    scan_tag_lines(cs, s, ss, t, ss1, l, as)
  end

  defp scan_tag_lines([?\t | cs], s, ss, t, ss1, l, as) do
    scan_tag_lines(cs, s, ss, t, ss1, l, as)
  end

  defp scan_tag_lines([?@, c | _Cs], s, ss, {ts, l1}, ss1, l, as)
       when c >= ?a and c <= ?z do
    scan_lines(s, ss, l, [make_tag(ts, l1, ss1) | as])
  end

  defp scan_tag_lines([?@, c | _Cs], s, ss, {ts, l1}, ss1, l, as)
       when c >= ?A and c <= ?Z do
    scan_lines(s, ss, l, [make_tag(ts, l1, ss1) | as])
  end

  defp scan_tag_lines([?@, c | _Cs], s, ss, {ts, l1}, ss1, l, as)
       when c >= 192 and c <= 255 and c !== 215 and
              c !== 247 do
    scan_lines(s, ss, l, [make_tag(ts, l1, ss1) | as])
  end

  defp scan_tag_lines('TODO:' ++ _, s, ss, {ts, l1}, ss1, l, as) do
    scan_lines(s, ss, l, [make_tag(ts, l1, ss1) | as])
  end

  defp scan_tag_lines(_Cs, s, ss, t, ss1, l, as) do
    scan_tag_lines(ss, t, [s | ss1], l + 1, as)
  end

  defp make_tag(cs, l, ss) do
    r_tag(
      name: aliases(:erlang.list_to_atom(:lists.reverse(cs))),
      line: l,
      data: append_lines(:lists.reverse(ss))
    )
  end

  defp append_lines([l]) do
    l
  end

  defp append_lines([l | ls]) do
    l ++ [?\n | append_lines(ls)]
  end

  defp append_lines([]) do
    []
  end

  def filter_tags(ts, tags) do
    filter_tags(ts, tags, :no)
  end

  def filter_tags(ts, tags, where) do
    filter_tags(ts, tags, where, [])
  end

  defp filter_tags([r_tag(name: :clear) | ts], tags, where, _Ts1) do
    filter_tags(ts, tags, where)
  end

  defp filter_tags([r_tag(name: n, line: l) = t | ts], tags, where, ts1) do
    case :sets.is_element(n, tags) do
      true ->
        filter_tags(ts, tags, where, [t | ts1])

      false ->
        case where do
          :no ->
            :ok

          _ ->
            warning(l, where, 'tag @~s not recognized.', [n])
        end

        filter_tags(ts, tags, where, ts1)
    end
  end

  defp filter_tags([], _, _, ts) do
    :lists.reverse(ts)
  end

  def check_tags(ts, allow, single, where) do
    check_tags(ts, allow, single, where, false, :sets.new())
  end

  defp check_tags([r_tag(name: t, line: l) | ts], allow, single, where, error, seen) do
    case :sets.is_element(t, seen) do
      true ->
        case :sets.is_element(t, single) do
          false ->
            check_tags(ts, allow, single, where, error, seen)

          true ->
            report(l, where, 'multiple @~s tag.', [t])
            check_tags(ts, allow, single, where, true, seen)
        end

      false ->
        seen1 = :sets.add_element(t, seen)

        case :sets.is_element(t, allow) do
          true ->
            check_tags(ts, allow, single, where, error, seen1)

          false ->
            report(l, where, 'tag @~s not allowed here.', [t])
            check_tags(ts, allow, single, where, true, seen1)
        end
    end
  end

  defp check_tags([], _, _, _, error, _) do
    error
  end

  def parse_tags(ts, how, env, where) do
    parse_tags(ts, how, env, where, [])
  end

  defp parse_tags([r_tag(name: name) = t | ts], how, env, where, ts1) do
    case :dict.fetch(name, how) do
      :text ->
        parse_tags(ts, how, env, where, [t | ts1])

      :xml ->
        [t1] = parse_tag(t, &parse_xml/4, env, where)
        parse_tags(ts, how, env, where, [t1 | ts1])

      f when is_function(f) ->
        ts2 = parse_tag(t, f, env, where)
        parse_tags(ts, how, env, where, :lists.reverse(ts2, ts1))
    end
  end

  defp parse_tags([], _How, _Env, _Where, ts) do
    :lists.reverse(ts)
  end

  defp parse_tag(t, f, env, where) do
    case (try do
            {:ok, f.(r_tag(t, :data), r_tag(t, :line), env, where)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, data} ->
        [r_tag(t, data: data)]

      {:expand, ts} ->
        ts

      {:error, l, error} ->
        error(l, where, error)
        exit(:error)

      {:EXIT, r} ->
        exit(r)

      other ->
        throw(other)
    end
  end

  defp parse_xml(data, line, _Env, _Where) do
    :edoc_wiki.parse_xml(data, line)
  end

  defp parse_see(data, line, _Env, _Where) do
    :edoc_parser.parse_see(data, line)
  end

  defp parse_expr(data, line, _Env, _Where) do
    :edoc_lib.parse_expr(data, line)
  end

  defp parse_spec(data, line, _Env, {_, {f, a}} = _Where) do
    spec = :edoc_parser.parse_spec(data, line)
    r_t_spec(name: n, type: r_t_fun(args: as)) = spec

    cond do
      length(as) != a ->
        throw_error(line, '@spec arity does not match.')

      true ->
        case n do
          :undefined ->
            r_t_spec(spec, name: r_t_name(module: [], name: f))

          r_t_name(module: [], name: ^f) ->
            spec

          _ ->
            throw_error(line, '@spec name does not match.')
        end
    end
  end

  defp parse_param(data, line, _Env, {_, {_F, _A}} = _Where) do
    :edoc_parser.parse_param(data, line)
  end

  defp parse_throws(data, line, _Env, {_, {_F, _A}} = _Where) do
    :edoc_parser.parse_throws(data, line)
  end

  defp parse_contact(data, line, _Env, _Where) do
    case :edoc_lib.parse_contact(data, line) do
      {'', '', _URI} ->
        throw_error(line, 'must specify name or e-mail.')

      info ->
        info
    end
  end

  defp parse_typedef(data, line, _Env, where) do
    def__ = :edoc_parser.parse_typedef(data, line)
    {r_t_typedef(name: r_t_name(name: t), args: as), _} = def__
    nAs = length(as)

    case :edoc_types.is_predefined(t, nAs) do
      true ->
        case :edoc_types.is_new_predefined(t, nAs) do
          false ->
            throw_error(line, {'redefining built-in type \'~w\'.', [t]})

          true ->
            warning(line, where, 'redefining built-in type \'~w\'.', [t])
            def__
        end

      false ->
        def__
    end
  end

  defp parse_file(data, line, env, _Where) do
    case :edoc_lib.parse_expr(data, line) do
      {:string, _, file0} ->
        file = :edoc_lib.strip_space(file0)

        case :edoc_extract.file(file, :module, env, []) do
          {:ok, ts} ->
            throw({:expand, ts})

          {:error, r} ->
            throw_error(line, {:read_file, file, r})
        end

      _ ->
        throw_error(line, :file_not_string)
    end
  end

  defp parse_header(data, line, env, {where, _}) do
    parse_header(data, line, env, where)
  end

  defp parse_header(data, line, env, where) when is_list(where) do
    case :edoc_lib.parse_expr(data, line) do
      {:string, _, file} ->
        dir = :filename.dirname(where)
        path = r_env(env, :includes) ++ [dir]

        case :edoc_lib.find_file(path, file) do
          '' ->
            throw_error(line, {:file_not_found, file})

          file1 ->
            ts = :edoc_extract.header(file1, env, [])
            throw({:expand, ts})
        end

      _ ->
        throw_error(line, :file_not_string)
    end
  end

  defp throw_error(l, {:read_file, file, r}) do
    throw_error(l, {'error reading file \'~ts\': ~w', [:edoc_lib.filename(file), r]})
  end

  defp throw_error(l, {:file_not_found, f}) do
    throw_error(l, {'file not found: ~ts', [f]})
  end

  defp throw_error(l, :file_not_string) do
    throw_error(l, 'expected file name as a string')
  end

  defp throw_error(l, d) do
    throw({:error, l, d})
  end

  Record.defrecord(:r_parms, :parms,
    tab: :undefined,
    warn: :undefined,
    file: :undefined,
    line: :undefined
  )

  def check_types(entries, opts, file) do
    tags = :edoc_data.get_all_tags(entries)

    typeTags =
      for r_tag(data: {r_t_typedef(), _}) = tag <- tags do
        tag
      end

    entries2 = :edoc_data.hidden_filter(entries, opts)
    tags2 = :edoc_data.get_all_tags(entries2)

    specTags =
      for r_tag(data: r_t_spec()) = tag <- tags2 do
        tag
      end

    dT = :ets.new(:types, [:bag])

    _ =
      for r_tag(
            line: line,
            data: {r_t_typedef(name: name, args: as), _}
          ) <- typeTags do
        add_type(dT, name, as, file, line)
      end

    warn = :proplists.get_value(:report_missing_types, opts, false) === true
    p = r_parms(tab: dT, warn: warn, file: file, line: 0)

    try do
      check_types3(typeTags ++ specTags, p, [])
    after
      true = :ets.delete(dT)
    end
  end

  defp add_type(dT, name, args, file, line) do
    nArgs = length(args)
    typeName = {name, nArgs}

    case :lists.member(typeName, :ets.lookup(dT, name)) do
      true ->
        r_t_name(name: n) = name
        type_warning(line, file, 'duplicated type', n, nArgs)

      false ->
        :ets.insert(dT, {name, nArgs})
    end
  end

  defp check_types3([], _P, _Ls) do
    :ok
  end

  defp check_types3([tag | tags], p, ls) do
    check_type(tag, p, ls, tags)
  end

  defp check_type(r_tag(line: l, data: data), p0, ls, ts) do
    p = r_parms(p0, line: l)

    case data do
      {r_t_typedef(type: type, defs: defs), _} ->
        check_type(type, p, ls, defs ++ ts)

      r_t_spec(type: type, defs: defs) ->
        localTypes =
          for r_t_def(
                name:
                  r_t_type(
                    name: n,
                    args: args
                  )
              ) <- defs do
            {n, length(args)}
          end

        check_type(type, p, localTypes, defs)
        check_types3(ts, p, ls)

      _ ->
        check_types3(ts, p0, ls)
    end
  end

  defp check_type(r_t_def(type: type), p, ls, ts) do
    check_type(type, p, ls, ts)
  end

  defp check_type(r_t_type(name: name, args: args), p, ls, ts) do
    check_used_type(name, args, p, ls)
    check_types3(args ++ ts, p, ls)
  end

  defp check_type(r_t_var(), p, ls, ts) do
    check_types3(ts, p, ls)
  end

  defp check_type(r_t_fun(args: args, range: range), p, ls, ts) do
    check_type(range, p, ls, args ++ ts)
  end

  defp check_type(r_t_map(), p, ls, ts) do
    check_types3(ts, p, ls)
  end

  defp check_type(r_t_tuple(types: types), p, ls, ts) do
    check_types3(types ++ ts, p, ls)
  end

  defp check_type(r_t_list(type: type), p, ls, ts) do
    check_type(type, p, ls, ts)
  end

  defp check_type(r_t_nil(), p, ls, ts) do
    check_types3(ts, p, ls)
  end

  defp check_type(r_t_paren(type: type), p, ls, ts) do
    check_type(type, p, ls, ts)
  end

  defp check_type(r_t_nonempty_list(type: type), p, ls, ts) do
    check_type(type, p, ls, ts)
  end

  defp check_type(r_t_atom(), p, ls, ts) do
    check_types3(ts, p, ls)
  end

  defp check_type(r_t_integer(), p, ls, ts) do
    check_types3(ts, p, ls)
  end

  defp check_type(r_t_integer_range(), p, ls, ts) do
    check_types3(ts, p, ls)
  end

  defp check_type(r_t_binary(), p, ls, ts) do
    check_types3(ts, p, ls)
  end

  defp check_type(r_t_float(), p, ls, ts) do
    check_types3(ts, p, ls)
  end

  defp check_type(r_t_union(types: types), p, ls, ts) do
    check_types3(types ++ ts, p, ls)
  end

  defp check_type(r_t_record(fields: fields), p, ls, ts) do
    check_types3(fields ++ ts, p, ls)
  end

  defp check_type(r_t_field(type: type), p, ls, ts) do
    check_type(type, p, ls, ts)
  end

  defp check_type(:undefined, p, ls, ts) do
    check_types3(ts, p, ls)
  end

  defp check_used_type(r_t_name(name: n, module: mod) = name, args, p, localTypes) do
    nArgs = length(args)
    typeName = {name, nArgs}
    dT = r_parms(p, :tab)

    case mod !== [] or
           :lists.member(
             typeName,
             :ets.lookup(
               dT,
               name
             )
           ) or
           :edoc_types.is_predefined(
             n,
             nArgs
           ) or
           :lists.member(
             typeName,
             localTypes
           ) do
      true ->
        :ok

      false ->
        r_parms(warn: w, line: l, file: file) = p

        _ =
          for _ <- [:EFE_DUMMY_GEN], w do
            type_warning(l, file, 'missing type', n, nArgs)
          end

        :ok
    end
  end

  defp type_warning(line, file, s, n, nArgs) do
    aS =
      for _ <- [:EFE_DUMMY_GEN], nArgs > 0 do
        '/' ++ :erlang.integer_to_list(nArgs)
      end

    warning(line, file, s ++ ' ~w~s', [n, aS])
  end
end
