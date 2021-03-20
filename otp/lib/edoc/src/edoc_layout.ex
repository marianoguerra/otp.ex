defmodule :m_edoc_layout do
  use Bitwise
  import :edoc_report, only: [report: 2]
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
    xML =
      layout_module(
        element,
        init_opts(element, options)
      )

    export = :proplists.get_value(:xml_export, options, :xmerl_html)
    :xmerl.export_simple(xML, export, [])
  end

  Record.defrecord(:r_opts, :opts,
    root: :undefined,
    stylesheet: :undefined,
    index_columns: :undefined,
    sort_functions: :undefined,
    encoding: :undefined,
    pretty_printer: :undefined
  )

  defp init_opts(element, options) do
    encoding =
      case get_attrval(:encoding, element) do
        'latin1' ->
          :latin1

        _ ->
          :utf8
      end

    r =
      r_opts(
        root: get_attrval(:root, element),
        index_columns: :proplists.get_value(:index_columns, options, 1),
        sort_functions: :proplists.get_value(:sort_functions, options, true),
        encoding: encoding,
        pretty_printer: :proplists.get_value(:pretty_printer, options, :"")
      )

    case :proplists.get_value(:stylesheet, options) do
      :undefined ->
        s = :edoc_lib.join_uri(r_opts(r, :root), 'stylesheet.css')
        r_opts(r, stylesheet: s)

      '' ->
        r

      s when is_list(s) ->
        r_opts(r, stylesheet: s)

      _ ->
        report('bad value for option `stylesheet\'.', [])
        exit(:error)
    end
  end

  defp layout_module(r_xmlElement(name: :module, content: es) = e, opts) do
    args = module_params(get_content(:args, es))
    name = get_attrval(:name, e)

    title =
      case get_elem(:args, es) do
        [] ->
          ['Module ', name]

        _ ->
          ['Abstract module ', name, ' [', {args}, ']']
      end

    desc = get_content(:description, es)
    shortDesc = get_content(:briefDescription, desc)
    fullDesc = get_content(:fullDescription, desc)

    functions =
      for ^e <- get_content(:functions, es) do
        {function_name(e, opts), e}
      end

    types =
      for ^e <- get_content(:typedecls, es) do
        {type_name(e, opts), e}
      end

    sortedFs =
      cond do
        r_opts(opts, :sort_functions) ->
          :lists.sort(functions)

        true ->
          functions
      end

    body =
      navigation('top') ++
        ['\n', :hr, '\n', '\n', {:h1, title}, '\n'] ++
        doc_index(
          fullDesc,
          functions,
          types
        ) ++
        shortDesc ++
        ['\n'] ++
        copyright(es) ++
        deprecated(
          es,
          'module'
        ) ++
        ['\n'] ++
        version(es) ++
        since(es) ++
        behaviours(
          es,
          name,
          opts
        ) ++
        authors(es) ++
        references(es) ++
        sees(es) ++
        todos(es) ++
        cond do
          fullDesc == [] ->
            []

          true ->
            [
              '\n',
              {:h2, [{:a, [{:name, 'description'}], ['Description']}]}
              | fullDesc
            ]
        end ++
        types(
          :lists.sort(types),
          opts
        ) ++
        function_index(
          sortedFs,
          r_opts(opts, :index_columns)
        ) ++
        functions(
          sortedFs,
          opts
        ) ++ [:hr, '\n'] ++ navigation('bottom') ++ footer()

    encoding = r_opts(opts, :encoding)
    xhtml(title, stylesheet(opts), body, encoding)
  end

  defp module_params(es) do
    as =
      for r_xmlElement(content: es1) <- es do
        {get_text(:argName, es1),
         get_content(
           :fullDescription,
           get_content(:description, es1)
         )}
      end

    case as do
      [] ->
        []

      [first | rest] ->
        [
          :erlang.element(1, first)
          | for {a, _D} <- rest do
              {[', ', a]}
            end
        ]
    end
  end

  defp footer() do
    ['\n', {:p, [{:i, ['Generated by EDoc']}]}, '\n']
  end

  defp stylesheet(opts) do
    case r_opts(opts, :stylesheet) do
      :undefined ->
        []

      cSS ->
        [
          {:link, [{:rel, 'stylesheet'}, {:type, 'text/css'}, {:href, cSS}, {:title, 'EDoc'}],
           []},
          '\n'
        ]
    end
  end

  defp navigation(where) do
    [
      '\n',
      {:div, [{:class, 'navbar'}],
       [
         {:a, [{:name, '#navbar_' ++ where}], []},
         {:table,
          [
            {:width, '100%'},
            {:border, 0},
            {:cellspacing, 0},
            {:cellpadding, 2},
            {:summary, 'navigation bar'}
          ],
          [
            {:tr,
             [
               {:td,
                [
                  {:a, [{:href, 'overview-summary.html'}, {:target, 'overviewFrame'}],
                   ['Overview']}
                ]},
               {:td,
                [
                  {:a, [{:href, 'http://www.erlang.org/'}],
                   [
                     {:img,
                      [
                        {:src, 'erlang.png'},
                        {:align, 'right'},
                        {:border, 0},
                        {:alt, 'erlang logo'}
                      ], []}
                   ]}
                ]}
             ]}
          ]}
       ]}
    ]
  end

  defp doc_index(fullDesc, functions, types) do
    case doc_index_rows(fullDesc, functions, types) do
      [] ->
        []

      rs ->
        [
          {:ul, [{:class, 'index'}],
           for {t, r} <- rs do
             {:li, [{:a, [{:href, local_label(r)}], [t]}]}
           end}
        ]
    end
  end

  defp doc_index_rows(fullDesc, functions, types) do
    cond do
      fullDesc == [] ->
        []

      true ->
        [{'Description', 'description'}]
    end ++
      cond do
        types == [] ->
          []

        true ->
          [{'Data Types', 'types'}]
      end ++
      cond do
        functions == [] ->
          []

        true ->
          [{'Function Index', 'index'}, {'Function Details', 'functions'}]
      end
  end

  defp function_index(fs, cols) do
    case function_index_rows(fs, cols, []) do
      [] ->
        []

      rows ->
        [
          '\n',
          {:h2, [{:a, [{:name, 'index'}], ['Function Index']}]},
          '\n',
          {:table,
           [
             {:width, '100%'},
             {:border, 1},
             {:cellspacing, 0},
             {:cellpadding, 2},
             {:summary, 'function index'}
           ], rows},
          '\n'
        ]
    end
  end

  defp function_index_rows(fs, cols, title) do
    rows = div(length(fs) + (cols - 1), cols)

    cond do
      title == [] ->
        []

      true ->
        [{:tr, [{:th, [{:colspan, cols * 2}, {:align, :left}], [title]}]}, '\n']
    end ++
      :lists.flatmap(
        &index_row/1,
        :edoc_lib.transpose(:edoc_lib.segment(fs, rows))
      )
  end

  defp index_row(fs) do
    [{:tr, :lists.flatmap(&index_col/1, fs)}, '\n']
  end

  defp index_col({name, f = r_xmlElement(content: es)}) do
    [
      {:td, [{:valign, 'top'}], label_href(function_header(name, f, '*'), f)},
      {:td, index_desc(es)}
    ]
  end

  defp index_desc(es) do
    desc = get_content(:description, es)

    case get_content(:deprecated, es) do
      [] ->
        []

      _ ->
        ['(', {:em, ['Deprecated']}, '.) ']
    end ++
      case get_content(:briefDescription, desc) do
        [] ->
          equiv(es)

        shortDesc ->
          shortDesc
      end
  end

  defp label_href(content, f) do
    case get_attrval(:label, f) do
      '' ->
        content

      ref ->
        [{:a, [{:href, local_label(ref)}], content}]
    end
  end

  defp functions(fs, opts) do
    es =
      :lists.flatmap(
        fn {name, e} ->
          function(name, e, opts)
        end,
        fs
      )

    cond do
      es == [] ->
        []

      true ->
        ['\n', {:h2, [{:a, [{:name, 'functions'}], ['Function Details']}]}, '\n' | es]
    end
  end

  defp function(name, e = r_xmlElement(content: es), opts) do
    ['\n', {:h3, [{:class, 'function'}], label_anchor(function_header(name, e, ' *'), e)}, '\n'] ++
      [
        {:div, [{:class, 'spec'}],
         case (for t <- get_contents(:typespec, es) do
                 typespec(t, opts)
               end) do
           [] ->
             [
               '\n',
               {:p,
                signature(
                  get_content(:args, es),
                  atom(
                    get_attrval(:name, e),
                    opts
                  )
                )},
               '\n'
             ]

           specs ->
             ['\n'] ++
               for spec <- specs do
                 {:p, spec}
               end ++ ['\n']
         end ++
           case (for a <- get_contents(:args, es) do
                   params(a)
                 end) do
             [] ->
               []

             as ->
               :lists.append(
                 for ps <- as do
                   [{:p, ps}, '\n']
                 end
               )
           end ++
           case (for ret <-
                       get_contents(
                         :returns,
                         es
                       ) do
                   returns(ret)
                 end) do
             [] ->
               []

             rets ->
               :lists.append(
                 for rs <- rets do
                   [{:p, rs}, '\n']
                 end
               )
           end}
      ] ++
      throws(
        es,
        opts
      ) ++
      equiv_p(es) ++
      deprecated(
        es,
        'function'
      ) ++ fulldesc(es) ++ since(es) ++ sees(es) ++ todos(es)
  end

  defp function_name(e, opts) do
    atom(
      get_attrval(:name, e),
      opts
    ) ++ '/' ++ get_attrval(:arity, e)
  end

  defp function_header(name, e, private) do
    case is_exported(e) do
      true ->
        [name]

      false ->
        [name, private]
    end
  end

  defp is_exported(e) do
    case get_attrval(:exported, e) do
      'yes' ->
        true

      _ ->
        false
    end
  end

  defp label_anchor(content, e) do
    case get_attrval(:label, e) do
      '' ->
        content

      ref ->
        [{:a, [{:name, ref}], content}]
    end
  end

  defp signature(es, name) do
    [{:tt, [name, '('] ++ seq(&arg/1, es) ++ [') -> any()']}]
  end

  defp arg(r_xmlElement(content: es)) do
    [get_text(:argName, es)]
  end

  defp params(es) do
    as =
      for r_xmlElement(content: es1) <- es do
        {get_text(:argName, es1),
         get_content(
           :fullDescription,
           get_content(:description, es1)
         )}
      end

    as1 =
      for a <- as, :erlang.element(2, a) != [] do
        a
      end

    cond do
      as1 == [] ->
        []

      true ->
        for {a, d} <- as1 do
          {[{:tt, [a]}, ': '] ++ d ++ [:br, '\n']}
        end
    end
  end

  defp returns(es) do
    case get_content(
           :fullDescription,
           get_content(:description, es)
         ) do
      [] ->
        []

      d ->
        ['returns: '] ++ d
    end
  end

  defp throws(es, opts) do
    case get_content(:throws, es) do
      [] ->
        []

      es1 ->
        [
          {:p,
           [
             'throws ',
             {:tt,
              t_utype(
                get_elem(:type, es1),
                opts
              )}
           ] ++
             local_defs(
               get_elem(:localdef, es1),
               opts
             )},
          '\n'
        ]
    end
  end

  defp typespec([], _Opts) do
    []
  end

  defp typespec(es, opts) do
    name = t_name(get_elem(:erlangName, es), opts)
    defs = get_elem(:localdef, es)
    [type] = get_elem(:type, es)

    format_spec(name, type, defs, opts) ++
      local_defs(
        defs,
        opts
      )
  end

  defp types([], _Opts) do
    []
  end

  defp types(ts, opts) do
    es =
      :lists.flatmap(
        fn {name, e} ->
          typedecl(name, e, opts)
        end,
        ts
      )

    ['\n', {:h2, [{:a, [{:name, 'types'}], ['Data Types']}]}, '\n' | es]
  end

  defp typedecl(name, e = r_xmlElement(content: es), opts) do
    ['\n', {:h3, [{:class, 'typedecl'}], label_anchor([name, '()'], e)}, '\n'] ++
      [{:p, typedef(get_content(:typedef, es), opts)}, '\n'] ++ fulldesc(es)
  end

  defp type_name(r_xmlElement(content: es), opts) do
    t_name(
      get_elem(:erlangName, get_content(:typedef, es)),
      opts
    )
  end

  defp typedef(es, opts) do
    name =
      [t_name(get_elem(:erlangName, es), opts), '('] ++
        seq(t_utype_elem_fun(opts), get_content(:argtypes, es), [')'])

    case get_elem(:type, es) do
      [] ->
        [{:b, ['abstract datatype']}, ': ', {:tt, name}]

      type ->
        format_type(name, name, type, [], opts)
    end ++ local_defs(get_elem(:localdef, es), opts)
  end

  defp local_defs(es, opts) do
    local_defs(es, [], opts)
  end

  defp local_defs([], _, _Opts) do
    []
  end

  defp local_defs(es0, last, opts) do
    [e | es] = :lists.reverse(es0)

    [
      '\n',
      {:ul, [{:class, 'definitions'}],
       :lists.reverse(
         :lists.append(
           for e1 <- es do
             localdef(e1, [], opts)
           end
         ),
         localdef(e, last, opts)
       )}
    ]
  end

  defp localdef(e = r_xmlElement(content: es), last, opts) do
    name =
      case get_elem(:typevar, es) do
        [] ->
          label_anchor(
            n0 =
              t_abstype(
                get_content(:abstype, es),
                opts
              ),
            e
          )

        [v] ->
          n0 = t_var(v)
      end

    [{:li, format_type(name, n0, get_elem(:type, es), last, opts)}]
  end

  defp format_spec(name, type, defs, r_opts(pretty_printer: :erl_pp) = opts) do
    try do
      l = t_clause(name, type, opts)
      o = pp_clause(name, type, opts)
      {r, '.\n'} = etypef(l, o, opts)
      [{:pre, r}]
    catch
      _, _ ->
        format_spec(name, type, defs, r_opts(opts, pretty_printer: :""))
    end
  end

  defp format_spec(sep, type, defs, opts) do
    br =
      cond do
        defs === [] ->
          :br

        true ->
          []
      end

    [{:tt, t_clause(sep, type, opts)}, br]
  end

  defp t_clause(name, type, opts) do
    r_xmlElement(content: [r_xmlElement(name: :fun, content: c)]) = type
    [name] ++ t_fun(c, opts)
  end

  defp pp_clause(pre, type, opts) do
    types = ot_utype([type])
    atom = :lists.duplicate(:string.length(pre), ?a)
    attr = {:attribute, 0, :spec, {{:erlang.list_to_atom(atom), 0}, [types]}}

    l1 =
      :erl_pp.attribute(
        :erl_parse.new_anno(attr),
        [{:encoding, r_opts(opts, :encoding)}]
      )

    '-spec ' ++ l2 = :lists.flatten(l1)
    l3 = pre ++ :lists.nthtail(length(atom), l2)
    :re.replace(l3, '\n      ', '\n', [{:return, :list}, :global, :unicode])
  end

  defp format_type(prefix, name, type, last, r_opts(pretty_printer: :erl_pp) = opts) do
    try do
      l = t_utype(type, opts)
      o = pp_type(name, type, opts)
      {r, '.\n'} = etypef(l, o, opts)
      [{:pre, prefix ++ [' = '] ++ r ++ last}]
    catch
      _, _ ->
        format_type(prefix, name, type, last, r_opts(opts, pretty_printer: :""))
    end
  end

  defp format_type(prefix, _Name, type, last, opts) do
    [{:tt, prefix ++ [' = '] ++ t_utype(type, opts) ++ last}]
  end

  defp pp_type(prefix, type, opts) do
    atom =
      :erlang.list_to_atom(
        :lists.duplicate(
          :string.length(prefix),
          ?a
        )
      )

    attr = {:attribute, 0, :type, {atom, ot_utype(type), []}}

    l1 =
      :erl_pp.attribute(
        :erl_parse.new_anno(attr),
        [{:encoding, r_opts(opts, :encoding)}]
      )

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
    :re.replace(l2, '\n' ++ ss, '\n', [{:return, :list}, :global, :unicode])
  end

  defp etypef(l, o0, opts) do
    {r, o} = etypef(l, [], o0, [], opts)
    {:lists.reverse(r), o}
  end

  defp etypef([c | l], st, [c | o], r, opts) do
    etypef(l, st, o, [[c] | r], opts)
  end

  defp etypef(' ' ++ l, st, o, r, opts) do
    etypef(l, st, o, r, opts)
  end

  defp etypef('', [cs | st], o, r, opts) do
    etypef(cs, st, o, r, opts)
  end

  defp etypef('', [], o, r, _Opts) do
    {r, o}
  end

  defp etypef(l, st, ' ' ++ o, r, opts) do
    etypef(l, st, o, [' ' | r], opts)
  end

  defp etypef(l, st, '\n' ++ o, r, opts) do
    ss =
      :lists.takewhile(
        fn c ->
          c === ?\s
        end,
        o
      )

    etypef(l, st, :lists.nthtail(length(ss), o), ['\n' ++ ss | r], opts)
  end

  defp etypef([{:a, hRef, s0} | l], st, o0, r, opts) do
    {s, o} = etypef(s0, app_fix(o0, opts), opts)
    etypef(l, st, o, [{:a, hRef, s} | r], opts)
  end

  defp etypef('=' ++ l, st, '::' ++ o, r, opts) do
    etypef(l, st, o, ['=' | r], opts)
  end

  defp etypef([cs | l], st, o, r, opts) do
    etypef(cs, [l | st], o, r, opts)
  end

  defp app_fix(l, opts) do
    try do
      {'//' ++ r1, l2} = app_fix1(l, 1)
      [app, mod] = :string.lexemes(r1, '/')
      '//' ++ atom(app, opts) ++ '/' ++ atom(mod, opts) ++ l2
    catch
      _, _ ->
        l
    end
  end

  defp app_fix1(l, i) do
    {l1, l2} = :lists.split(i, l)

    case :erl_scan.tokens([], l1 ++ '. ', 1) do
      {:done, {:ok, [{:atom, _, atom} | _], _}, _} ->
        {:erlang.atom_to_list(atom), l2}

      _ ->
        app_fix1(l, i + 1)
    end
  end

  defp fulldesc(es) do
    case get_content(
           :fullDescription,
           get_content(:description, es)
         ) do
      [] ->
        ['\n']

      desc ->
        [{:p, desc}, '\n']
    end
  end

  defp sees(es) do
    case get_elem(:see, es) do
      [] ->
        []

      es1 ->
        [{:p, [{:b, ['See also:']}, ' '] ++ seq(&see/1, es1, ['.'])}, '\n']
    end
  end

  defp see(e = r_xmlElement(content: es)) do
    see(e, es)
  end

  defp see(e, es) do
    case href(e) do
      [] ->
        es

      ref ->
        [{:a, ref, es}]
    end
  end

  defp href(e) do
    case get_attrval(:href, e) do
      '' ->
        []

      uRI ->
        t =
          case get_attrval(:target, e) do
            '' ->
              []

            s ->
              [{:target, s}]
          end

        [{:href, uRI} | t]
    end
  end

  defp equiv_p(es) do
    equiv(es, true)
  end

  defp equiv(es) do
    equiv(es, false)
  end

  defp equiv(es, p) do
    case get_content(:equiv, es) do
      [] ->
        []

      es1 ->
        case get_content(:expr, es1) do
          [] ->
            []

          [expr] ->
            expr1 = [{:tt, [expr]}]

            expr2 =
              case get_elem(:see, es1) do
                [] ->
                  expr1

                [e = r_xmlElement()] ->
                  see(e, expr1)
              end

            txt = ['Equivalent to '] ++ expr2 ++ ['.']

            case p do
              true ->
                [{:p, txt}]

              false ->
                txt
            end ++ ['\n']
        end
    end
  end

  defp copyright(es) do
    case get_content(:copyright, es) do
      [] ->
        []

      es1 ->
        [{:p, ['Copyright © ' | es1]}, '\n']
    end
  end

  defp version(es) do
    case get_content(:version, es) do
      [] ->
        []

      es1 ->
        [{:p, [{:b, ['Version:']}, ' ' | es1]}, '\n']
    end
  end

  defp since(es) do
    case get_content(:since, es) do
      [] ->
        []

      es1 ->
        [{:p, [{:b, ['Introduced in:']}, ' ' | es1]}, '\n']
    end
  end

  defp deprecated(es, s) do
    es1 =
      get_content(
        :description,
        get_content(:deprecated, es)
      )

    case get_content(:fullDescription, es1) do
      [] ->
        []

      es2 ->
        [{:p, [{:b, ['This ' ++ s ++ ' is deprecated:']}, ' ' | es2]}, '\n']
    end
  end

  defp behaviours(es, name, opts) do
    cBs = get_content(:callbacks, es)
    oCBs = get_content(:optional_callbacks, es)

    case get_elem(:behaviour, es) do
      [] ->
        []

      es1 ->
        [{:p, [{:b, ['Behaviours:']}, ' '] ++ seq(&behaviour/1, es1, ['.'])}, '\n']
    end ++
      cond do
        cBs === [] and oCBs === [] ->
          []

        true ->
          cBFun = fn e ->
            callback(e, opts)
          end

          req =
            cond do
              cBs === [] ->
                []

              true ->
                [:br, ' Required callback functions: '] ++ seq(cBFun, cBs, ['.'])
            end

          opt =
            cond do
              oCBs === [] ->
                []

              true ->
                [:br, ' Optional callback functions: '] ++ seq(cBFun, oCBs, ['.'])
            end

          [
            {:p,
             [{:b, ['This module defines the ', {:tt, [name]}, ' behaviour.']}] ++ req ++ opt},
            '\n'
          ]
      end
  end

  defp behaviour(e = r_xmlElement(content: es)) do
    see(e, [{:tt, es}])
  end

  defp callback(e = r_xmlElement(), opts) do
    name = get_attrval(:name, e)
    arity = get_attrval(:arity, e)
    [{:tt, [atom(name, opts), '/', arity]}]
  end

  defp authors(es) do
    case get_elem(:author, es) do
      [] ->
        []

      es1 ->
        [{:p, [{:b, ['Authors:']}, ' '] ++ seq(&author/1, es1, ['.'])}, '\n']
    end
  end

  defp atom(string, r_opts(encoding: :latin1)) do
    :io_lib.write_atom_as_latin1(:erlang.list_to_atom(string))
  end

  defp atom(string, r_opts(encoding: :utf8)) do
    :io_lib.write_atom(:erlang.list_to_atom(string))
  end

  defp author(e = r_xmlElement()) do
    name = get_attrval(:name, e)
    mail = get_attrval(:email, e)
    uRI = get_attrval(:website, e)

    cond do
      name == mail ->
        [{:a, [{:href, 'mailto:' ++ mail}], [{:tt, [mail]}]}]

      true ->
        cond do
          mail == '' ->
            [name]

          true ->
            [name, ' (', {:a, [{:href, 'mailto:' ++ mail}], [{:tt, [mail]}]}, ')']
        end
    end ++
      cond do
        uRI == '' ->
          []

        true ->
          [
            ' [',
            {:em, ['web site:']},
            ' ',
            {:tt, [{:a, [{:href, uRI}, {:target, '_top'}], [uRI]}]},
            ']'
          ]
      end
  end

  defp references(es) do
    case get_elem(:reference, es) do
      [] ->
        []

      es1 ->
        [
          {:p,
           [
             {:b, ['References']},
             {:ul,
              for r_xmlElement(content: c) <- es1 do
                {:li, c}
              end}
           ]},
          '\n'
        ]
    end
  end

  defp todos(es) do
    case get_elem(:todo, es) do
      [] ->
        []

      es1 ->
        todos =
          for r_xmlElement(content: c) <- es1 do
            {:li, [{:font, [{:color, :red}], c}]}
          end

        [{:p, [{:b, [{:font, [{:color, :red}], ['To do']}]}, {:ul, todos}]}, '\n']
    end
  end

  defp t_name([e], opts) do
    n = get_attrval(:name, e)

    case get_attrval(:module, e) do
      '' ->
        atom(n, opts)

      m ->
        s = atom(m, opts) ++ ':' ++ atom(n, opts)

        case get_attrval(:app, e) do
          '' ->
            s

          a ->
            '//' ++ atom(a, opts) ++ '/' ++ s
        end
    end
  end

  defp t_utype([e], opts) do
    t_utype_elem(e, opts)
  end

  defp t_utype_elem_fun(opts) do
    fn e ->
      t_utype_elem(e, opts)
    end
  end

  defp t_utype_elem(e = r_xmlElement(content: es), opts) do
    case get_attrval(:name, e) do
      '' ->
        t_type(es, opts)

      name ->
        t = t_type(es, opts)

        case t do
          [^name] ->
            t

          ^t ->
            [name] ++ ['::'] ++ t
        end
    end
  end

  defp t_type([e = r_xmlElement(name: :typevar)], _Opts) do
    t_var(e)
  end

  defp t_type([e = r_xmlElement(name: :atom)], opts) do
    t_atom(e, opts)
  end

  defp t_type([e = r_xmlElement(name: :integer)], _Opts) do
    t_integer(e)
  end

  defp t_type([e = r_xmlElement(name: :range)], _Opts) do
    t_range(e)
  end

  defp t_type([e = r_xmlElement(name: :binary)], _Opts) do
    t_binary(e)
  end

  defp t_type([e = r_xmlElement(name: :float)], _Opts) do
    t_float(e)
  end

  defp t_type([r_xmlElement(name: nil)], _Opts) do
    t_nil()
  end

  defp t_type([r_xmlElement(name: :paren, content: es)], opts) do
    t_paren(es, opts)
  end

  defp t_type([r_xmlElement(name: :list, content: es)], opts) do
    t_list(es, opts)
  end

  defp t_type([r_xmlElement(name: :nonempty_list, content: es)], opts) do
    t_nonempty_list(es, opts)
  end

  defp t_type([r_xmlElement(name: :map, content: es)], opts) do
    t_map(es, opts)
  end

  defp t_type([r_xmlElement(name: :tuple, content: es)], opts) do
    t_tuple(es, opts)
  end

  defp t_type([r_xmlElement(name: :fun, content: es)], opts) do
    ['fun('] ++ t_fun(es, opts) ++ [')']
  end

  defp t_type([e = r_xmlElement(name: :record, content: es)], opts) do
    t_record(e, es, opts)
  end

  defp t_type([e = r_xmlElement(name: :abstype, content: es)], opts) do
    t_abstype(e, es, opts)
  end

  defp t_type([r_xmlElement(name: :union, content: es)], opts) do
    t_union(es, opts)
  end

  defp t_var(e) do
    [get_attrval(:name, e)]
  end

  defp t_atom(e, opts) do
    [atom(get_attrval(:value, e), opts)]
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

  defp t_paren(es, opts) do
    ['('] ++ t_utype(get_elem(:type, es), opts) ++ [')']
  end

  defp t_list(es, opts) do
    ['['] ++ t_utype(get_elem(:type, es), opts) ++ [']']
  end

  defp t_nonempty_list(es, opts) do
    ['['] ++ t_utype(get_elem(:type, es), opts) ++ [', ...]']
  end

  defp t_tuple(es, opts) do
    ['{'] ++ seq(t_utype_elem_fun(opts), es, ['}'])
  end

  defp t_fun(es, opts) do
    ['('] ++
      seq(
        t_utype_elem_fun(opts),
        get_content(:argtypes, es),
        [') -> '] ++ t_utype(get_elem(:type, es), opts)
      )
  end

  defp t_map(es, opts) do
    fs = get_elem(:map_field, es)

    ['\#{'] ++
      seq(
        fn e ->
          t_map_field(e, opts)
        end,
        fs,
        ['}']
      )
  end

  defp t_map_field(r_xmlElement(content: [k, v]) = e, opts) do
    kElem = t_utype_elem(k, opts)
    vElem = t_utype_elem(v, opts)

    aS =
      case get_attrval(:assoc_type, e) do
        'assoc' ->
          ' => '

        'exact' ->
          ' := '
      end

    kElem ++ [aS] ++ vElem
  end

  defp t_record(e, es, opts) do
    name = ['#'] ++ t_type(get_elem(:atom, es), opts)

    case get_elem(:field, es) do
      [] ->
        see(e, [name, '{}'])

      fs ->
        see(e, name) ++
          ['{'] ++
          seq(
            fn f ->
              t_field(f, opts)
            end,
            fs,
            ['}']
          )
    end
  end

  defp t_field(r_xmlElement(content: es), opts) do
    t_type(
      get_elem(:atom, es),
      opts
    ) ++ [' = '] ++ t_utype(get_elem(:type, es), opts)
  end

  defp t_abstype(e, es, opts) do
    name = t_name(get_elem(:erlangName, es), opts)

    case get_elem(:type, es) do
      [] ->
        see(e, [name, '()'])

      ts ->
        see(e, [name]) ++ ['('] ++ seq(t_utype_elem_fun(opts), ts, [')'])
    end
  end

  defp t_abstype(es, opts) do
    [t_name(get_elem(:erlangName, es), opts), '('] ++
      seq(t_utype_elem_fun(opts), get_elem(:type, es), [')'])
  end

  defp t_union(es, opts) do
    seq(t_utype_elem_fun(opts), es, ' | ', [])
  end

  defp seq(f, es) do
    seq(f, es, [])
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

  defp get_elem(name, es) do
    for r_xmlElement(name: n) = e <- es, n === name do
      e
    end
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

  defp get_contents(name, es) do
    case get_elem(name, es) do
      [] ->
        []

      elems ->
        for r_xmlElement(content: es1) <- elems do
          es1
        end
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

  defp get_text(name, es) do
    case get_content(name, es) do
      [r_xmlText(value: text)] ->
        text

      [] ->
        ''
    end
  end

  defp local_label(r) do
    '#' ++ r
  end

  defp xhtml(title, cSS, body, encoding) do
    encString =
      case encoding do
        :latin1 ->
          'ISO-8859-1'

        :utf8 ->
          'UTF-8'
      end

    [
      {:html,
       [
         '\n',
         {:head,
          [
            '\n',
            {:meta,
             [{:"http-equiv", 'Content-Type'}, {:content, 'text/html; charset=' ++ encString}],
             []},
            '\n',
            {:title, title},
            '\n'
          ] ++ cSS},
         '\n',
         {:body, [{:bgcolor, 'white'}], body},
         '\n'
       ]},
      '\n'
    ]
  end

  def type(e) do
    opts = init_opts(e, [])
    type(e, [], opts)
  end

  defp type(e, ds, opts) do
    :xmerl.export_simple_content(
      t_utype_elem(
        e,
        opts
      ) ++ local_defs(ds, opts),
      :xmerl_html
    )
  end

  def overview(e = r_xmlElement(name: :overview, content: es), options) do
    opts = init_opts(e, options)
    title = [get_text(:title, es)]
    desc = get_content(:description, es)
    fullDesc = get_content(:fullDescription, desc)

    body =
      navigation('top') ++
        ['\n', {:h1, [title]}, '\n'] ++
        copyright(es) ++
        version(es) ++
        since(es) ++
        authors(es) ++
        references(es) ++
        sees(es) ++ todos(es) ++ fullDesc ++ ['\n', :hr] ++ navigation('bottom') ++ footer()

    encoding = r_opts(opts, :encoding)
    xML = xhtml(title, stylesheet(opts), body, encoding)
    :xmerl.export_simple(xML, :xmerl_html, [])
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
    name = :erlang.list_to_atom(get_attrval(:value, e))
    {:atom, :erl_anno.new(0), name}
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
end
