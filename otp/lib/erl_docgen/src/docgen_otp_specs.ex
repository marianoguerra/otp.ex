defmodule :m_docgen_otp_specs do
  use Bitwise
  require Record

  Record.defrecord(:r_xmlDecl, :xmlDecl,
    vsn: :undefined,
    encoding: :undefined,
    standalone: :undefined,
    attributes: :undefined
  )

  Record.defrecord(:r_xmlAttribute, :xmlAttribute,
    name: :undefined,
    expanded_name: [],
    nsinfo: [],
    namespace: [],
    parents: [],
    pos: :undefined,
    language: [],
    value: :undefined,
    normalized: :undefined
  )

  Record.defrecord(:r_xmlNamespace, :xmlNamespace,
    default: [],
    nodes: []
  )

  Record.defrecord(:r_xmlNsNode, :xmlNsNode,
    parents: [],
    pos: :undefined,
    prefix: :undefined,
    uri: []
  )

  Record.defrecord(:r_xmlElement, :xmlElement,
    name: :undefined,
    expanded_name: [],
    nsinfo: [],
    namespace: :EFE_TODO_NESTED_RECORD,
    parents: [],
    pos: :undefined,
    attributes: [],
    content: [],
    language: '',
    xmlbase: '',
    elementdef: :undeclared
  )

  Record.defrecord(:r_xmlText, :xmlText,
    parents: [],
    pos: :undefined,
    language: [],
    value: :undefined,
    type: :text
  )

  Record.defrecord(:r_xmlComment, :xmlComment,
    parents: [],
    pos: :undefined,
    language: [],
    value: :undefined
  )

  Record.defrecord(:r_xmlPI, :xmlPI,
    name: :undefined,
    parents: [],
    pos: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_xmlDocument, :xmlDocument, content: :undefined)

  Record.defrecord(:r_xmlContext, :xmlContext,
    axis_type: :forward,
    context_node: :undefined,
    context_position: 1,
    nodeset: [],
    bindings: [],
    functions: [],
    namespace: [],
    whole_document: :undefined
  )

  Record.defrecord(:r_xmlNode, :xmlNode, type: :element, node: :undefined, parents: [], pos: 1)

  Record.defrecord(:r_xmlObj, :xmlObj,
    type: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_xmerl_fun_states, :xmerl_fun_states,
    event: :undefined,
    hook: :undefined,
    rules: :undefined,
    fetch: :undefined,
    cont: :undefined
  )

  Record.defrecord(:r_xmerl_scanner, :xmerl_scanner,
    encoding: :undefined,
    standalone: :no,
    environment: :prolog,
    declarations: [],
    doctype_name: :undefined,
    doctype_DTD: :internal,
    comments: true,
    document: false,
    default_attrs: false,
    rules: :undefined,
    keep_rules: false,
    namespace_conformant: false,
    xmlbase: :undefined,
    xmlbase_cache: :undefined,
    fetch_path: [],
    filename: :file_name_unknown,
    validation: :off,
    schemaLocation: [],
    space: :preserve,
    event_fun: :undefined,
    hook_fun: :undefined,
    acc_fun: :undefined,
    fetch_fun: :undefined,
    close_fun: :undefined,
    continuation_fun: :undefined,
    rules_read_fun: :undefined,
    rules_write_fun: :undefined,
    rules_delete_fun: :undefined,
    user_state: :undefined,
    fun_states: :EFE_TODO_NESTED_RECORD,
    entity_references: [],
    text_decl: false,
    quiet: false,
    col: 1,
    line: 1,
    common_data: []
  )

  Record.defrecord(:r_xmerl_event, :xmerl_event,
    event: :undefined,
    line: :undefined,
    col: :undefined,
    pos: :undefined,
    data: :undefined
  )

  def module(element, options) do
    xML = layout_module(element, init_opts(options))
    export = :proplists.get_value(:xml_export, options, :xmerl_xml)
    :xmerl.export_simple(xML, export, [r_xmlAttribute(name: :prolog, value: '')])
  end

  Record.defrecord(:r_opts, :opts,
    pretty_print: :undefined,
    file_suffix: :undefined
  )

  defp init_opts(options) do
    r_opts(
      pretty_print: :proplists.get_value(:pretty_print, options, :erl_pp),
      file_suffix: :proplists.get_value(:file_suffix, options, '.html')
    )
  end

  defp layout_module(r_xmlElement(name: :module, content: es) = e, opts) do
    name = get_attrval(:name, e)

    functions =
      for elem <- get_content(:functions, es) do
        {function_name(elem), elem}
      end

    types =
      for elem <- get_content(:typedecls, es) do
        {type_name(elem), elem}
      end

    body = [
      {:module, [{:name, [name]}],
       ['\n'] ++
         types(
           :lists.sort(types),
           opts
         ) ++
         functions(
           :lists.sort(functions),
           opts
         ) ++ timestamp()}
    ]

    body
  end

  defp timestamp() do
    [
      {:timestamp,
       [
         :io_lib.fwrite(
           'Generated by EDoc, ~s, ~s.',
           [:edoc_lib.datestr(:erlang.date()), :edoc_lib.timestr(:erlang.time())]
         )
       ]},
      '\n'
    ]
  end

  defp functions(fs, opts) do
    :lists.flatmap(
      fn {name, e} ->
        function(name, e, opts)
      end,
      fs
    )
  end

  defp function(name, r_xmlElement(content: es), opts) do
    tS = get_content(:typespec, es)
    spec = typespec(tS, opts)

    [
      {:spec,
       name ++
         [r_xmlText(value: '\n' ++ :lists.duplicate(2, ?\s)), {:contract, spec}] ++
         typespec_annos(tS)},
      '\n'
    ]
  end

  defp function_name(e) do
    [] = get_attrval(:module, e)

    [
      r_xmlText(value: '\n' ++ :lists.duplicate(2, ?\s)),
      {:name, [atom(get_attrval(:name, e))]},
      r_xmlText(
        value:
          '\n' ++
            :lists.duplicate(
              2,
              ?\s
            )
      ),
      {:arity,
       [
         get_attrval(
           :arity,
           e
         )
       ]}
    ]
  end

  defp label_anchor(content, e) do
    case get_attrval(:label, e) do
      '' ->
        content

      ref ->
        [{:marker, [{:id, ref}], content}]
    end
  end

  defp typespec([], _Opts) do
    []
  end

  defp typespec(es, opts) do
    {head, lDefs} = collect_clause(es, opts)

    clause(
      head,
      lDefs
    ) ++ [r_xmlText(value: '\n' ++ :lists.duplicate(2, ?\s))]
  end

  defp collect_clause(es, opts) do
    name = t_name(get_elem(:erlangName, es))
    defs = get_elem(:localdef, es)
    [type] = get_elem(:type, es)
    {format_spec(name, type, opts), collect_local_defs(defs, opts)}
  end

  defp clause(head, lDefs) do
    fC =
      [r_xmlText(value: '\n' ++ :lists.duplicate(6, ?\s)), {:head, head}] ++
        local_clause_defs(lDefs)

    [r_xmlText(value: '\n' ++ :lists.duplicate(4, ?\s)), {:clause, fC}]
  end

  defp local_clause_defs([]) do
    []
  end

  defp local_clause_defs(lDefs) do
    localDefs =
      for t <- coalesce_local_defs(lDefs, []) do
        {:subtype, t}
      end

    [r_xmlText(value: '\n' ++ :lists.duplicate(6, ?\s)), {:guard, margin(8, localDefs)}]
  end

  defp types(ts, opts) do
    :lists.flatmap(
      fn {name, e} ->
        typedecl(name, e, opts)
      end,
      ts
    )
  end

  defp typedecl(name, e = r_xmlElement(content: es), opts) do
    tD = get_content(:typedef, es)
    typeDef = typedef(e, tD, opts)

    [
      {:type,
       name ++
         [r_xmlText(value: '\n' ++ :lists.duplicate(2, ?\s)), {:typedecl, typeDef}] ++
         typedef_annos(tD)},
      '\n'
    ]
  end

  defp type_name(r_xmlElement(content: es)) do
    typedef = get_content(:typedef, es)
    [e] = get_elem(:erlangName, typedef)
    args = get_content(:argtypes, typedef)
    [] = get_attrval(:module, e)

    [
      r_xmlText(value: '\n' ++ :lists.duplicate(2, ?\s)),
      {:name, [atom(get_attrval(:name, e))]},
      r_xmlText(
        value:
          '\n' ++
            :lists.duplicate(
              2,
              ?\s
            )
      ),
      {:n_vars, [:erlang.integer_to_list(length(args))]}
    ]
  end

  defp typedef(e, es, opts) do
    ns = get_elem(:erlangName, es)
    name = [t_name(ns), '('] ++ seq(&t_utype_elem/1, get_content(:argtypes, es), [')'])

    lDefs =
      collect_local_defs(
        get_elem(:localdef, es),
        opts
      )

    typeHead =
      case get_elem(:type, es) do
        [] ->
          label_anchor(name, e)

        type ->
          label_anchor(name, e) ++ format_type(name, type, opts)
      end

    [r_xmlText(value: '\n' ++ :lists.duplicate(6, ?\s)), {:typehead, typeHead}] ++
      local_type_defs(
        lDefs,
        []
      )
  end

  defp local_type_defs([], _) do
    []
  end

  defp local_type_defs(lDefs, last) do
    localDefs =
      for t <-
            coalesce_local_defs(
              lDefs,
              last
            ) do
        {:local_def, t}
      end

    [r_xmlText(value: '\n' ++ :lists.duplicate(6, ?\s)), {:local_defs, margin(8, localDefs)}]
  end

  defp collect_local_defs(es, opts) do
    for e <- es do
      collect_localdef(e, opts)
    end
  end

  defp collect_localdef(e = r_xmlElement(content: es), opts) do
    name =
      case get_elem(:typevar, es) do
        [] ->
          label_anchor(
            n0 = t_abstype(get_content(:abstype, es)),
            e
          )

        [v] ->
          n0 = t_var(v)
      end

    {name, n0, format_type(n0, get_elem(:type, es), opts)}
  end

  defp coalesce_local_defs([], _Last) do
    []
  end

  defp coalesce_local_defs([{name, n0, typeS} | l], last)
       when name === n0 do
    cld(l, [{name, n0}], typeS, last)
  end

  defp coalesce_local_defs([{name, n0, typeS} | l], last) do
    [
      local_def(n0, name, typeS, last, l)
      | coalesce_local_defs(l, last)
    ]
  end

  defp cld([{name, n0, typeS} | l], names, typeS, last)
       when name === n0 do
    cld(l, [{name, n0} | names], typeS, last)
  end

  defp cld(l, names0, typeS, last) do
    names = [{_, name0} | names1] = :lists.reverse(names0)

    nS =
      join(
        for {n, _} <- names do
          n
        end,
        [' = ']
      )

    [
      local_def(name0, nS, typeS, last, l)
      | for {_, n0} <- names1 do
          local_def(n0, '', '', [], l)
        end
    ] ++ coalesce_local_defs(l, last)
  end

  defp local_def(name, nS, typeS, last, l) do
    [
      {:typename, name},
      {:string,
       nS ++
         typeS ++
         for _ <- [:EFE_DUMMY_GEN],
             l === [] do
           last
         end}
    ]
  end

  defp join([h | t], sep) do
    h ++
      :lists.append(
        for x <- t do
          sep ++ x
        end
      )
  end

  defp format_spec(name, type, r_opts(pretty_print: :erl_pp) = opts) do
    try do
      l = t_clause(name, type)
      o = pp_clause(name, type)
      {r, '.\n'} = diaf(l, o, opts)
      r
    catch
      _, _ ->
        format_spec(name, type, r_opts(opts, pretty_print: :default))
    end
  end

  defp format_spec(sep, type, _Opts) do
    t_clause(sep, type)
  end

  defp t_clause(name, type) do
    r_xmlElement(content: [r_xmlElement(name: :fun, content: c)]) = type
    [name] ++ t_fun(c)
  end

  defp pp_clause(pre, type) do
    types = ot_utype([type])
    atom = :lists.duplicate(:erlang.iolist_size(pre), ?a)
    attr = {:attribute, 0, :spec, {{:erlang.list_to_atom(atom), 0}, [types]}}
    l1 = :erl_pp.attribute(:erl_parse.new_anno(attr))
    '-spec ' ++ l2 = :lists.flatten(l1)
    l3 = pre ++ :lists.nthtail(length(atom), l2)
    :re.replace(l3, '\n      ', '\n', [{:return, :list}, :global])
  end

  defp format_type(name, type, r_opts(pretty_print: :erl_pp) = opts) do
    try do
      l = t_utype(type)
      o = pp_type(name, type)
      {r, '.\n'} = diaf(l, o, opts)
      [' = '] ++ r
    catch
      _, _ ->
        format_type(name, type, r_opts(opts, pretty_print: :default))
    end
  end

  defp format_type(_Name, type, _Opts) do
    [' = '] ++ t_utype(type)
  end

  defp pp_type(prefix, type) do
    atom =
      :erlang.list_to_atom(
        :lists.duplicate(
          :erlang.iolist_size(prefix),
          ?a
        )
      )

    attr = {:attribute, 0, :type, {atom, ot_utype(type), []}}
    l1 = :erl_pp.attribute(:erl_parse.new_anno(attr))

    {l2, n} =
      case :lists.dropwhile(
             fn c ->
               c !== ?:
             end,
             :lists.flatten(l1)
           ) do
        ':: ' ++ l3 ->
          {l3, 9}

        '::\n' ++ l3 ->
          {'\n' ++ l3, 6}
      end

    ss = :lists.duplicate(n, ?\s)
    :re.replace(l2, '\n' ++ ss, '\n', [{:return, :list}, :global])
  end

  defp diaf(l, o0, opts) do
    {r0, o} = diaf(l, [], o0, [], opts)
    r1 = rewrite_some_predefs(:lists.reverse(r0))
    r = indentation(:lists.flatten(r1))
    {r, o}
  end

  defp diaf([c | l], st, [c | o], r, opts) do
    diaf(l, st, o, [[c] | r], opts)
  end

  defp diaf(' ' ++ l, st, o, r, opts) do
    diaf(l, st, o, r, opts)
  end

  defp diaf('', [cs | st], o, r, opts) do
    diaf(cs, st, o, r, opts)
  end

  defp diaf('', [], o, r, _Opts) do
    {r, o}
  end

  defp diaf(l, st, ' ' ++ o, r, opts) do
    diaf(l, st, o, [' ' | r], opts)
  end

  defp diaf(l, st, '\n' ++ o, r, opts) do
    ss =
      :lists.takewhile(
        fn c ->
          c === ?\s
        end,
        o
      )

    diaf(l, st, :lists.nthtail(length(ss), o), ['\n' ++ ss | r], opts)
  end

  defp diaf([{:seetype, hRef0, s0} | l], st, o0, r, opts) do
    {s, o} = diaf(s0, app_fix(o0), opts)
    hRef = fix_mod_ref(hRef0, opts)
    diaf(l, st, o, [{:seetype, hRef, s} | r], opts)
  end

  defp diaf('=' ++ l, st, '::' ++ o, r, opts) do
    diaf(l, st, o, ['=' | r], opts)
  end

  defp diaf([cs | l], st, o, r, opts) do
    diaf(cs, [l | st], o, r, opts)
  end

  defp rewrite_some_predefs(s) do
    xpredef(:lists.flatten(s))
  end

  defp xpredef([]) do
    []
  end

  defp xpredef('neg_integer()' ++ l) do
    ['integer() =< -1'] ++ xpredef(l)
  end

  defp xpredef('non_neg_integer()' ++ l) do
    ['integer() >= 0'] ++ xpredef(l)
  end

  defp xpredef('pos_integer()' ++ l) do
    ['integer() >= 1'] ++ xpredef(l)
  end

  defp xpredef([t | es]) when is_tuple(t) do
    [t | xpredef(es)]
  end

  defp xpredef([e | es]) do
    [[e] | xpredef(es)]
  end

  defp indentation([]) do
    []
  end

  defp indentation([?\n | l]) do
    [{:br, []} | indent(l)]
  end

  defp indentation([t | es]) when is_tuple(t) do
    [t | indentation(es)]
  end

  defp indentation([e | l]) do
    [[e] | indentation(l)]
  end

  defp indent([?\s | l]) do
    [{:nbsp, []} | indent(l)]
  end

  defp indent(l) do
    indentation(l)
  end

  defp app_fix(l) do
    try do
      {'//' ++ r1, l2} = app_fix(l, 1)
      [app, mod] = :string.lexemes(r1, '/')
      '//' ++ atom(app) ++ '/' ++ atom(mod) ++ l2
    catch
      _, _ ->
        l
    end
  end

  defp app_fix(l, i) do
    {l1, l2} = :lists.split(i, l)

    case :erl_scan.tokens([], l1 ++ '. ', 1) do
      {:done, {:ok, [{:atom, _, atom} | _], _}, _} ->
        {:erlang.atom_to_list(atom), l2}

      _ ->
        app_fix(l, i + 1)
    end
  end

  defp fix_mod_ref(hRef, r_opts(file_suffix: '')) do
    hRef
  end

  defp fix_mod_ref([{:marker, s}] = hRef0, r_opts(file_suffix: fS)) do
    {a, b} =
      :lists.splitwith(
        fn c ->
          c !== ?#
        end,
        s
      )

    case :lists.member(?:, a) do
      true ->
        hRef0

      false ->
        case {:lists.suffix(fS, a), b} do
          {true, '#' ++ _} ->
            [{:marker, :lists.sublist(a, length(a) - length(fS)) ++ b}]

          _ ->
            hRef0
        end
    end
  end

  defp see(e, es) do
    case href(e) do
      [] ->
        es

      [{:marker, ref}] ->
        [{:seetype, [{:marker, :lists.flatten(:string.replace(ref, '#type-', '#'))}], es}]
    end
  end

  defp href(e) do
    case get_attrval(:href, e) do
      '' ->
        []

      uRI ->
        [{:marker, uRI}]
    end
  end

  defp atom(string) do
    :io_lib.write_atom(:erlang.list_to_atom(string))
  end

  defp t_name([e]) do
    n = get_attrval(:name, e)

    case get_attrval(:module, e) do
      '' ->
        atom(n)

      m ->
        s = atom(m) ++ ':' ++ atom(n)

        case get_attrval(:app, e) do
          '' ->
            s

          a ->
            '//' ++ atom(a) ++ '/' ++ s
        end
    end
  end

  defp t_utype([e]) do
    t_utype_elem(e)
  end

  defp t_utype_elem(e = r_xmlElement(content: es)) do
    case get_attrval(:name, e) do
      '' ->
        t_type(es)

      name ->
        t = t_type(es)

        case t do
          [^name] ->
            t

          ^t ->
            [name] ++ ['::'] ++ t
        end
    end
  end

  defp t_type([e = r_xmlElement(name: :typevar)]) do
    t_var(e)
  end

  defp t_type([e = r_xmlElement(name: :atom)]) do
    t_atom(e)
  end

  defp t_type([e = r_xmlElement(name: :integer)]) do
    t_integer(e)
  end

  defp t_type([e = r_xmlElement(name: :range)]) do
    t_range(e)
  end

  defp t_type([e = r_xmlElement(name: :binary)]) do
    t_binary(e)
  end

  defp t_type([e = r_xmlElement(name: :float)]) do
    t_float(e)
  end

  defp t_type([r_xmlElement(name: nil)]) do
    t_nil()
  end

  defp t_type([r_xmlElement(name: :paren, content: es)]) do
    t_paren(es)
  end

  defp t_type([r_xmlElement(name: :list, content: es)]) do
    t_list(es)
  end

  defp t_type([r_xmlElement(name: :nonempty_list, content: es)]) do
    t_nonempty_list(es)
  end

  defp t_type([r_xmlElement(name: :map, content: es)]) do
    t_map(es)
  end

  defp t_type([r_xmlElement(name: :tuple, content: es)]) do
    t_tuple(es)
  end

  defp t_type([r_xmlElement(name: :fun, content: es)]) do
    ['fun('] ++ t_fun(es) ++ [')']
  end

  defp t_type([e = r_xmlElement(name: :record, content: es)]) do
    t_record(e, es)
  end

  defp t_type([e = r_xmlElement(name: :abstype, content: es)]) do
    t_abstype(e, es)
  end

  defp t_type([r_xmlElement(name: :union, content: es)]) do
    t_union(es)
  end

  defp t_var(e) do
    [get_attrval(:name, e)]
  end

  defp t_atom(e) do
    [
      :io_lib.write(
        :erlang.list_to_atom(
          get_attrval(
            :value,
            e
          )
        )
      )
    ]
  end

  defp t_integer(e) do
    [get_attrval(:value, e)]
  end

  defp t_range(e) do
    [get_attrval(:value, e)]
  end

  defp t_binary(e) do
    [get_attrval(:value, e)]
  end

  defp t_float(e) do
    [get_attrval(:value, e)]
  end

  defp t_nil() do
    ['[]']
  end

  defp t_paren(es) do
    ['('] ++ t_utype(get_elem(:type, es)) ++ [')']
  end

  defp t_list(es) do
    ['['] ++ t_utype(get_elem(:type, es)) ++ [']']
  end

  defp t_nonempty_list(es) do
    ['['] ++ t_utype(get_elem(:type, es)) ++ [', ...]']
  end

  defp t_tuple(es) do
    ['{'] ++ seq(&t_utype_elem/1, es, ['}'])
  end

  defp t_fun(es) do
    ['('] ++
      seq(&t_utype_elem/1, get_content(:argtypes, es), [') -> '] ++ t_utype(get_elem(:type, es)))
  end

  defp t_map(es) do
    fs = get_elem(:map_field, es)
    ['\#{'] ++ seq(&t_map_field/1, fs, ['}'])
  end

  defp t_map_field(r_xmlElement(content: [k, v]) = e) do
    kElem = t_utype_elem(k)
    vElem = t_utype_elem(v)

    aS =
      case get_attrval(:assoc_type, e) do
        'assoc' ->
          ' => '

        'exact' ->
          ' := '
      end

    kElem ++ [aS] ++ vElem
  end

  defp t_record(e, es) do
    name = ['#'] ++ t_type(get_elem(:atom, es))

    case get_elem(:field, es) do
      [] ->
        see(e, [name, '{}'])

      fs ->
        see(e, name) ++ ['{'] ++ seq(&t_field/1, fs, ['}'])
    end
  end

  defp t_field(r_xmlElement(content: es)) do
    t_type(
      get_elem(
        :atom,
        es
      )
    ) ++ [' = '] ++ t_utype(get_elem(:type, es))
  end

  defp t_abstype(e, es) do
    name = t_name(get_elem(:erlangName, es))

    case get_elem(:type, es) do
      [] ->
        see(e, [name, '()'])

      ts ->
        see(e, [name]) ++ ['('] ++ seq(&t_utype_elem/1, ts, [')'])
    end
  end

  defp t_abstype(es) do
    [t_name(get_elem(:erlangName, es)), '('] ++ seq(&t_utype_elem/1, get_elem(:type, es), [')'])
  end

  defp t_union(es) do
    seq(&t_utype_elem/1, es, ' | ', [])
  end

  defp seq(f, es, tail) do
    seq(f, es, ', ', tail)
  end

  defp seq(f, [e], _Sep, tail) do
    f.(e) ++ tail
  end

  defp seq(f, [e | es], sep, tail) do
    f.(e) ++ [sep] ++ seq(f, es, sep, tail)
  end

  defp seq(_F, [], _Sep, tail) do
    tail
  end

  defp get_elem(name, [r_xmlElement(name: name) = e | es]) do
    [e | get_elem(name, es)]
  end

  defp get_elem(name, [_ | es]) do
    get_elem(name, es)
  end

  defp get_elem(_, []) do
    []
  end

  defp get_attr(name, [r_xmlAttribute(name: name) = a | as]) do
    [a | get_attr(name, as)]
  end

  defp get_attr(name, [_ | as]) do
    get_attr(name, as)
  end

  defp get_attr(_, []) do
    []
  end

  defp get_attrval(name, r_xmlElement(attributes: as)) do
    case get_attr(name, as) do
      [r_xmlAttribute(value: v)] ->
        v

      [] ->
        ''
    end
  end

  defp get_content(name, es) do
    case get_elem(name, es) do
      [r_xmlElement(content: es1)] ->
        es1

      [] ->
        []
    end
  end

  def overview(_, _Options) do
    []
  end

  def package(_, _Options) do
    []
  end

  def type(_) do
    []
  end

  defp ot_utype([e]) do
    ot_utype_elem(e)
  end

  defp ot_utype_elem(e = r_xmlElement(content: es)) do
    case get_attrval(:name, e) do
      '' ->
        ot_type(es)

      n ->
        name = {:var, 0, :erlang.list_to_atom(n)}
        t = ot_type(es)

        case t do
          ^name ->
            t

          ^t ->
            {:ann_type, 0, [name, t]}
        end
    end
  end

  defp ot_type([e = r_xmlElement(name: :typevar)]) do
    ot_var(e)
  end

  defp ot_type([e = r_xmlElement(name: :atom)]) do
    ot_atom(e)
  end

  defp ot_type([e = r_xmlElement(name: :integer)]) do
    ot_integer(e)
  end

  defp ot_type([e = r_xmlElement(name: :range)]) do
    ot_range(e)
  end

  defp ot_type([e = r_xmlElement(name: :binary)]) do
    ot_binary(e)
  end

  defp ot_type([e = r_xmlElement(name: :float)]) do
    ot_float(e)
  end

  defp ot_type([r_xmlElement(name: nil)]) do
    ot_nil()
  end

  defp ot_type([r_xmlElement(name: :paren, content: es)]) do
    ot_paren(es)
  end

  defp ot_type([r_xmlElement(name: :list, content: es)]) do
    ot_list(es)
  end

  defp ot_type([r_xmlElement(name: :nonempty_list, content: es)]) do
    ot_nonempty_list(es)
  end

  defp ot_type([r_xmlElement(name: :tuple, content: es)]) do
    ot_tuple(es)
  end

  defp ot_type([r_xmlElement(name: :map, content: es)]) do
    ot_map(es)
  end

  defp ot_type([r_xmlElement(name: :fun, content: es)]) do
    ot_fun(es)
  end

  defp ot_type([r_xmlElement(name: :record, content: es)]) do
    ot_record(es)
  end

  defp ot_type([r_xmlElement(name: :abstype, content: es)]) do
    ot_abstype(es)
  end

  defp ot_type([r_xmlElement(name: :union, content: es)]) do
    ot_union(es)
  end

  defp ot_var(e) do
    {:var, 0, :erlang.list_to_atom(get_attrval(:name, e))}
  end

  defp ot_atom(e) do
    {:ok, [{:atom, a, name}], _} = :erl_scan.string(:lists.flatten(t_atom(e)), 0)
    {:atom, :erl_anno.line(a), name}
  end

  defp ot_integer(e) do
    {:integer, 0, :erlang.list_to_integer(get_attrval(:value, e))}
  end

  defp ot_range(e) do
    [i1, i2] = :string.lexemes(get_attrval(:value, e), '.')

    {:type, 0, :range,
     [{:integer, 0, :erlang.list_to_integer(i1)}, {:integer, 0, :erlang.list_to_integer(i2)}]}
  end

  defp ot_binary(e) do
    {base, unit} =
      case :string.lexemes(
             get_attrval(:value, e),
             ',:*><'
           ) do
        [] ->
          {0, 0}

        ['_', b] ->
          {:erlang.list_to_integer(b), 0}

        ['_', '_', u] ->
          {0, :erlang.list_to_integer(u)}

        ['_', b, _, '_', u] ->
          {:erlang.list_to_integer(b), :erlang.list_to_integer(u)}
      end

    {:type, 0, :binary, [{:integer, 0, base}, {:integer, 0, unit}]}
  end

  defp ot_float(e) do
    {:float, 0, :erlang.list_to_float(get_attrval(:value, e))}
  end

  defp ot_nil() do
    {nil, 0}
  end

  defp ot_paren(es) do
    {:paren_type, 0, [ot_utype(get_elem(:type, es))]}
  end

  defp ot_list(es) do
    {:type, 0, :list, [ot_utype(get_elem(:type, es))]}
  end

  defp ot_nonempty_list(es) do
    {:type, 0, :nonempty_list, [ot_utype(get_elem(:type, es))]}
  end

  defp ot_tuple(es) do
    {:type, 0, :tuple,
     for e <- es do
       ot_utype_elem(e)
     end}
  end

  defp ot_map(es) do
    {:type, 0, :map,
     for e <- get_elem(:map_field, es) do
       ot_map_field(e)
     end}
  end

  defp ot_map_field(r_xmlElement(content: [k, v]) = e) do
    a =
      case get_attrval(:assoc_type, e) do
        'assoc' ->
          :map_field_assoc

        'exact' ->
          :map_field_exact
      end

    {:type, 0, a, [ot_utype_elem(k), ot_utype_elem(v)]}
  end

  defp ot_fun(es) do
    range = ot_utype(get_elem(:type, es))

    args =
      for a <- get_content(:argtypes, es) do
        ot_utype_elem(a)
      end

    {:type, 0, :fun, [{:type, 0, :product, args}, range]}
  end

  defp ot_record(es) do
    {:type, 0, :record,
     [
       ot_type(get_elem(:atom, es))
       | for f <- get_elem(:field, es) do
           ot_field(f)
         end
     ]}
  end

  defp ot_field(r_xmlElement(content: es)) do
    {:type, 0, :field_type,
     [
       ot_type(get_elem(:atom, es)),
       ot_utype(
         get_elem(
           :type,
           es
         )
       )
     ]}
  end

  defp ot_abstype(es) do
    ot_name(
      get_elem(:erlangName, es),
      for elem <- get_elem(:type, es) do
        ot_utype_elem(elem)
      end
    )
  end

  defp ot_union(es) do
    {:type, 0, :union,
     for e <- es do
       ot_utype_elem(e)
     end}
  end

  defp ot_name(es, t) do
    case ot_name(es) do
      [mod, ':', atom] ->
        {:remote_type, 0,
         [{:atom, 0, :erlang.list_to_atom(mod)}, {:atom, 0, :erlang.list_to_atom(atom)}, t]}

      'tuple' when t === [] ->
        {:type, 0, :tuple, :any}

      'map' when t === [] ->
        {:type, 0, :map, :any}

      atom ->
        {:type, 0, :erlang.list_to_atom(atom), t}
    end
  end

  defp ot_name([e]) do
    atom = get_attrval(:name, e)

    case get_attrval(:module, e) do
      '' ->
        atom

      m ->
        case get_attrval(:app, e) do
          '' ->
            [m, ':', atom]

          a ->
            ['//' ++ a ++ '/' ++ m, ':', atom]
        end
    end
  end

  defp typespec_annos([]) do
    ['\n']
  end

  defp typespec_annos([_ | es]) do
    annotations(clause_annos(es))
  end

  defp clause_annos(es) do
    [annos(get_elem(:type, es)), local_defs_annos(get_elem(:localdef, es))]
  end

  defp typedef_annos(es) do
    annotations([
      case get_elem(:type, es) do
        [] ->
          []

        t ->
          annos(t)
      end ++
        :lists.flatmap(
          &annos_elem/1,
          get_content(:argtypes, es)
        ),
      local_defs_annos(get_elem(:localdef, es))
    ])
  end

  defp local_defs_annos(es) do
    :lists.flatmap(&localdef_annos/1, es)
  end

  defp localdef_annos(r_xmlElement(content: es)) do
    annos(get_elem(:type, es))
  end

  defp annotations(annoL) do
    annos = :lists.usort(:lists.flatten(annoL))
    margin(2, annos)
  end

  defp margin(n, l) do
    :lists.append(
      for e <- l do
        [r_xmlText(value: '\n' ++ :lists.duplicate(n, ?\s)), e]
      end
    ) ++ [r_xmlText(value: '\n' ++ :lists.duplicate(n - 2, ?\s))]
  end

  defp annos([e]) do
    annos_elem(e)
  end

  defp annos_elem(e = r_xmlElement(content: es)) do
    case get_attrval(:name, e) do
      '' ->
        annos_type(es)

      '...' ->
        annos_type(es)

      n ->
        [{:anno, [n]} | annos_type(es)]
    end
  end

  defp annos_type([r_xmlElement(name: :list, content: es)]) do
    annos(get_elem(:type, es))
  end

  defp annos_type([r_xmlElement(name: :nonempty_list, content: es)]) do
    annos(get_elem(:type, es))
  end

  defp annos_type([r_xmlElement(name: :tuple, content: es)]) do
    :lists.flatmap(&annos_elem/1, es)
  end

  defp annos_type([r_xmlElement(name: :fun, content: es)]) do
    annos(
      get_elem(
        :type,
        es
      )
    ) ++
      :lists.flatmap(
        &annos_elem/1,
        get_content(:argtypes, es)
      )
  end

  defp annos_type([r_xmlElement(name: :record, content: es)]) do
    :lists.append(
      for r_xmlElement(content: es1) <-
            get_elem(
              :field,
              es
            ) do
        annos(get_elem(:type, es1))
      end
    )
  end

  defp annos_type([r_xmlElement(name: :abstype, content: es)]) do
    :lists.flatmap(&annos_elem/1, get_elem(:type, es))
  end

  defp annos_type([r_xmlElement(name: :union, content: es)]) do
    :lists.flatmap(&annos_elem/1, es)
  end

  defp annos_type([e = r_xmlElement(name: :typevar)]) do
    annos_elem(e)
  end

  defp annos_type([r_xmlElement(name: :paren, content: es)]) do
    annos(get_elem(:type, es))
  end

  defp annos_type([r_xmlElement(name: :map, content: es)]) do
    :lists.flatmap(
      fn e ->
        annos_type([e])
      end,
      es
    )
  end

  defp annos_type([r_xmlElement(name: :map_field, content: es)]) do
    :lists.flatmap(&annos_elem/1, get_elem(:type, es))
  end

  defp annos_type(_) do
    []
  end
end
