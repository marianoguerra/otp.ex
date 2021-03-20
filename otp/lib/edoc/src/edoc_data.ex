defmodule :m_edoc_data do
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

  def module(module, entries, env, opts) do
    name = :erlang.atom_to_list(r_module(module, :name))
    headerEntry = get_entry(:module, entries)
    headerTags = r_entry(headerEntry, :data)
    allTags = get_all_tags(entries)
    functions = function_filter(entries, opts)

    out =
      {:module,
       [{:name, name}, {:root, r_env(env, :root)}, {:encoding, r_module(module, :encoding)}] ++
         case is_private(headerTags) do
           true ->
             [{:private, 'yes'}]

           false ->
             []
         end ++
         case is_hidden(headerTags) do
           true ->
             [{:hidden, 'yes'}]

           false ->
             []
         end,
       module_args(r_module(module, :parameters)) ++
         behaviours(
           r_module(module, :attributes),
           env
         ) ++
         get_doc(headerTags) ++
         authors(headerTags) ++
         get_version(headerTags) ++
         get_since(headerTags) ++
         get_copyright(headerTags) ++
         get_deprecated(headerTags) ++
         sees(
           headerTags,
           env
         ) ++
         references(headerTags) ++
         todos(
           headerTags,
           opts
         ) ++
         [
           {:typedecls,
            types(
              allTags,
              env
            )},
           {:functions,
            functions(
              functions,
              env,
              opts
            )}
           | callbacks(
               functions,
               module,
               env,
               opts
             )
         ]}

    :xmerl_lib.expand_element(out)
  end

  def get_all_tags(es) do
    :lists.flatmap(
      fn r_entry(data: ts) ->
        ts
      end,
      es
    )
  end

  defp is_private(ts) do
    get_tags(:private, ts) !== []
  end

  defp description([]) do
    []
  end

  defp description(desc) do
    shortDesc = :edoc_lib.get_first_sentence(desc)
    [{:description, [{:briefDescription, shortDesc}, {:fullDescription, desc}]}]
  end

  defp module_args(:none) do
    []
  end

  defp module_args(vs) do
    [
      {:args,
       for v <- vs do
         {:arg, [{:argName, [:erlang.atom_to_list(v)]}]}
       end}
    ]
  end

  defp types(tags, env) do
    for r_tag(name: :type, data: {def__, doc}) <- tags do
      {:typedecl, [{:label, :edoc_types.to_label(def__)}],
       [:edoc_types.to_xml(def__, env)] ++ description(doc)}
    end
  end

  defp functions(es, env, opts) do
    for r_entry(name: {_, _} = n, args: as, export: export, data: ts) <- es do
      function(n, as, export, ts, env, opts)
    end
  end

  def hidden_filter(es, opts) do
    private = :proplists.get_bool(:private, opts)
    hidden = :proplists.get_bool(:hidden, opts)

    for e <- es,
        (case r_entry(e, :name) do
           {_, _} ->
             function_filter(e, private, hidden)

           _ ->
             true
         end) do
      e
    end
  end

  defp function_filter(es, opts) do
    private = :proplists.get_bool(:private, opts)
    hidden = :proplists.get_bool(:hidden, opts)

    for e <- es, function_filter(e, private, hidden) do
      e
    end
  end

  defp function_filter(r_entry(name: {_, _}, export: export, data: ts), private, hidden) do
    ((export and not is_private(ts)) or private) and (not is_hidden(ts) or hidden)
  end

  defp function_filter(_, _, _) do
    false
  end

  defp is_hidden(ts) do
    get_tags(:hidden, ts) !== []
  end

  defp callbacks(es, module, env, opts) do
    case :lists.any(
           fn
             r_entry(name: {:behaviour_info, 1}) ->
               true

             _ ->
               false
           end,
           es
         ) or :lists.keymember(:callback, 1, r_module(module, :attributes)) do
      true ->
        m = r_module(module, :name)
        fs = get_callback_functions(m, :callbacks)
        os1 = get_callback_functions(m, :optional_callbacks)

        fs1 =
          for fA <- fs, not :lists.member(fA, os1) do
            fA
          end

        req =
          cond do
            fs1 === [] ->
              []

            true ->
              [
                {:callbacks,
                 for fA <- fs1 do
                   callback(fA, env, opts)
                 end}
              ]
          end

        opt =
          cond do
            os1 === [] ->
              []

            true ->
              [
                {:optional_callbacks,
                 for fA <- os1 do
                   callback(fA, env, opts)
                 end}
              ]
          end

        req ++ opt

      false ->
        []
    end
  end

  defp get_callback_functions(m, callbacks) do
    try do
      for {f, a} = fA <- m.behaviour_info(callbacks), is_atom(f), is_integer(a), a >= 0 do
        fA
      end
    catch
      _, _ ->
        []
    end
  end

  defp callback({n, a}, _Env, _Opts) do
    {:callback, [{:name, :erlang.atom_to_list(n)}, {:arity, :erlang.integer_to_list(a)}], []}
  end

  defp function({n, a}, [] = as, export, ts, env, opts) do
    function({n, a}, [as], export, ts, env, opts)
  end

  defp function({n, a}, [hAs | _] = as, export, ts, env, opts)
       when not is_list(hAs) do
    function({n, a}, [as], export, ts, env, opts)
  end

  defp function({n, a}, as0, export, ts, env, opts) do
    {:function,
     [
       {:name, :erlang.atom_to_list(n)},
       {:arity, :erlang.integer_to_list(a)},
       {:exported,
        case export do
          true ->
            'yes'

          false ->
            'no'
        end},
       {:label,
        :edoc_refs.to_label(
          :edoc_refs.function(
            n,
            a
          )
        )}
     ],
     :lists.append(
       for clause <-
             :lists.seq(
               1,
               length(as0)
             ) do
         get_args(:lists.nth(clause, as0), ts, clause, env)
       end
     ) ++
       get_throws(ts, env) ++
       get_equiv(
         ts,
         env
       ) ++
       get_doc(ts) ++
       get_since(ts) ++
       get_deprecated(
         ts,
         n,
         a,
         env
       ) ++
       sees(
         ts,
         env
       ) ++
       todos(
         ts,
         opts
       )}
  end

  defp get_args(as, ts, clause, env) do
    {args, ret, spec} = signature(ts, as, clause, env)

    [
      {:args,
       for {a, d} <- args do
         {:arg, [{:argName, [:erlang.atom_to_list(a)]}] ++ description(d)}
       end}
    ] ++
      spec ++
      case ret do
        [] ->
          []

        _ ->
          [{:returns, description(ret)}]
      end
  end

  defp get_throws(ts, env) do
    case get_tags(:throws, ts) do
      [throws] ->
        type = r_tag(throws, :data)
        [:edoc_types.to_xml(type, env)]

      [] ->
        []
    end
  end

  defp get_equiv(ts, env) do
    case get_tags(:equiv, ts) do
      [equiv] ->
        expr = r_tag(equiv, :data)

        see =
          case get_expr_ref(r_tag(equiv, :data)) do
            :none ->
              []

            ref ->
              [see(ref, [:edoc_refs.to_string(ref)], env)]
          end

        [{:equiv, [{:expr, [:erl_prettypr.format(expr)]} | see]}]

      [] ->
        []
    end
  end

  defp get_doc(ts) do
    case get_tags(:doc, ts) do
      [t] ->
        description(r_tag(t, :data))

      [] ->
        []
    end
  end

  defp get_copyright(ts) do
    get_pcdata_tag(:copyright, ts)
  end

  defp get_version(ts) do
    get_pcdata_tag(:version, ts)
  end

  defp get_since(ts) do
    get_pcdata_tag(:since, ts)
  end

  defp get_pcdata_tag(tag, ts) do
    case get_tags(tag, ts) do
      [t] ->
        [{tag, [r_tag(t, :data)]}]

      [] ->
        []
    end
  end

  defp get_deprecated(ts) do
    case get_tags(:deprecated, ts) do
      [t] ->
        [{:deprecated, description(r_tag(t, :data))}]

      [] ->
        []
    end
  end

  defp get_deprecated(ts, f, a, env) do
    case get_deprecated(ts) do
      [] ->
        m = r_env(env, :module)

        case :otp_internal.obsolete(m, f, a) do
          {tag, text} when tag === :deprecated or tag === :removed ->
            deprecated([text])

          _ ->
            []
        end

      es ->
        es
    end
  end

  defp deprecated(desc) do
    [{:deprecated, description(desc)}]
  end

  defp get_expr_ref(expr) do
    case (try do
            {:ok, :erl_syntax_lib.analyze_application(expr)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, {f, a}} when is_atom(f) and is_integer(a) ->
        :edoc_refs.function(f, a)

      {:ok, {m, {f, a}}}
      when is_atom(m) and is_atom(f) and
             is_integer(a) ->
        :edoc_refs.function(m, f, a)

      _ ->
        :none
    end
  end

  defp authors(ts) do
    for r_tag(data: info) <- get_tags(:author, ts) do
      author(info)
    end
  end

  defp author({name, mail, uRI}) do
    {:author,
     [
       {:name,
        cond do
          name === '' ->
            mail

          true ->
            name
        end}
     ] ++
       cond do
         mail === '' ->
           case :lists.member(?@, name) do
             true ->
               [{:email, name}]

             false ->
               []
           end

         true ->
           [{:email, mail}]
       end ++
       cond do
         uRI === '' ->
           []

         true ->
           [{:website, uRI}]
       end, []}
  end

  defp behaviours(as, env) do
    for {:behaviour, b} <- as, is_atom(b) do
      {:behaviour, href(:edoc_refs.module(b), env), [:erlang.atom_to_list(b)]}
    end
  end

  defp sees(tags, env) do
    ts = get_tags(:see, tags)

    rs =
      :lists.keysort(
        1,
        for r_tag(data: data) <- ts do
          data
        end
      )

    for {ref, xML} <- rs do
      see(ref, xML, env)
    end
  end

  defp see(ref, [], env) do
    see(ref, [:edoc_refs.to_string(ref)], env)
  end

  defp see(ref, xML, env) do
    {:see, [{:name, :edoc_refs.to_string(ref)}] ++ href(ref, env), xML}
  end

  defp href(ref, env) do
    [
      {:href,
       :edoc_refs.get_uri(
         ref,
         env
       )}
    ] ++
      case :edoc_refs.is_top(ref, env) do
        true ->
          [{:target, '_top'}]

        false ->
          []
      end
  end

  defp references(tags) do
    for r_tag(data: xML) <- get_tags(:reference, tags) do
      {:reference, xML}
    end
  end

  defp todos(tags, opts) do
    case :proplists.get_bool(:todo, opts) do
      true ->
        for r_tag(data: xML) <- get_tags(:todo, tags) do
          {:todo, xML}
        end

      false ->
        []
    end
  end

  defp signature(ts, as, clause, env) do
    case get_tags(:spec, ts) do
      [t] ->
        spec = maybe_nth(clause, r_tag(t, :data))
        r = merge_returns(spec, ts)
        as0 = :edoc_types.arg_names(spec)
        ds0 = :edoc_types.arg_descs(spec)
        p = :dict.from_list(params(ts))
        as1 = merge_args(as0, as, ds0, p)

        spec1 =
          :edoc_types.set_arg_names(
            spec,
            for {a, _} <- as1 do
              a
            end
          )

        {as1, r, [:edoc_types.to_xml(spec1, env)]}

      [] ->
        s = :sets.new()

        {for a <- fix_argnames(as, s, 1) do
           {a, ''}
         end, [], []}
    end
  end

  defp maybe_nth(n, list) when is_list(list) do
    :lists.nth(n, list)
  end

  defp maybe_nth(1, other) do
    other
  end

  defp params(ts) do
    for t <- get_tags(:param, ts) do
      r_tag(t, :data)
    end
  end

  defp merge_returns(spec, ts) do
    case get_tags(:returns, ts) do
      [] ->
        case :edoc_types.range_desc(spec) do
          '' ->
            []

          txt ->
            [txt]
        end

      [t] ->
        r_tag(t, :data)
    end
  end

  defp merge_args(as, as1, ds, p) do
    merge_args(as, as1, ds, [], p, :sets.new(), 1)
  end

  defp merge_args([:_ | as], [:_ | as1], [d | ds], rs, p, s, n) do
    merge_args(as, as1, ds, rs, p, s, n, make_name(n, s), d)
  end

  defp merge_args([:_ | as], [a | as1], [d | ds], rs, p, s, n) do
    merge_args(as, as1, ds, rs, p, s, n, a, d)
  end

  defp merge_args([a | as], [_ | as1], [d | ds], rs, p, s, n) do
    merge_args(as, as1, ds, rs, p, s, n, a, d)
  end

  defp merge_args([], [], [], rs, _P, _S, _N) do
    :lists.reverse(rs)
  end

  defp merge_args(as, as1, ds, rs, p, s, n, a, d0) do
    d =
      case :dict.find(a, p) do
        {:ok, d1} ->
          d1

        :error when d0 === [] ->
          []

        :error ->
          [d0]
      end

    merge_args(as, as1, ds, [{a, d} | rs], p, :sets.add_element(a, s), n + 1)
  end

  defp fix_argnames([:_ | as], s, n) do
    a = make_name(n, s)
    [a | fix_argnames(as, :sets.add_element(a, s), n + 1)]
  end

  defp fix_argnames([a | as], s, n) do
    [a | fix_argnames(as, :sets.add_element(a, s), n + 1)]
  end

  defp fix_argnames([], _S, _N) do
    []
  end

  defp make_name(n, s) do
    make_name(n, s, 'X')
  end

  defp make_name(n, s, base) do
    a = :erlang.list_to_atom(base ++ :erlang.integer_to_list(n))

    case :sets.is_element(a, s) do
      true ->
        make_name(n, s, base ++ 'x')

      false ->
        a
    end
  end

  defp get_entry(name, [r_entry(name: name) = e | _Es]) do
    e
  end

  defp get_entry(name, [_ | es]) do
    get_entry(name, es)
  end

  defp get_tags(tag, [r_tag(name: tag) = t | ts]) do
    [t | get_tags(tag, ts)]
  end

  defp get_tags(tag, [_ | ts]) do
    get_tags(tag, ts)
  end

  defp get_tags(_, []) do
    []
  end

  def type(t, env) do
    :xmerl_lib.expand_element({:type, [:edoc_types.to_xml(t, env)]})
  end

  def overview(title, tags, env, opts) do
    env1 = r_env(env, root: '')
    :xmerl_lib.expand_element(overview_1(title, tags, env1, opts))
  end

  defp overview_1(title, tags, env, opts) do
    {:overview, [{:root, r_env(env, :root)}],
     [
       {:title,
        [
          get_title(
            tags,
            title
          )
        ]}
     ] ++
       get_doc(tags) ++
       authors(tags) ++
       get_copyright(tags) ++
       get_version(tags) ++
       get_since(tags) ++
       sees(
         tags,
         env
       ) ++
       references(tags) ++
       todos(
         tags,
         opts
       )}
  end

  defp get_title(ts, default) do
    case get_tags(:title, ts) do
      [t] ->
        r_tag(t, :data)

      [] ->
        default
    end
  end
end
