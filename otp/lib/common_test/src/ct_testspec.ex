defmodule :m_ct_testspec do
  use Bitwise
  require Record

  Record.defrecord(:r_conn, :conn,
    handle: :undefined,
    targetref: :undefined,
    address: :undefined,
    callback: :undefined
  )

  Record.defrecord(:r_testspec, :testspec,
    spec_dir: :undefined,
    nodes: [],
    init: [],
    label: [],
    profile: [],
    logdir: ['.'],
    logopts: [],
    basic_html: [],
    esc_chars: [],
    verbosity: [],
    silent_connections: [],
    cover: [],
    cover_stop: [],
    config: [],
    userconfig: [],
    event_handler: [],
    ct_hooks: [],
    enable_builtin_hooks: true,
    release_shell: false,
    include: [],
    auto_compile: [],
    abort_if_missing_suites: [],
    stylesheet: [],
    multiply_timetraps: [],
    scale_timetraps: [],
    create_priv_dir: [],
    alias: [],
    tests: [],
    unknown: [],
    merge_tests: true
  )

  Record.defrecord(:r_cover, :cover,
    app: :none,
    local_only: false,
    level: :details,
    excl_mods: [],
    incl_mods: [],
    cross: [],
    src: []
  )

  Record.defrecord(:r_conn_log, :conn_log,
    header: true,
    client: :undefined,
    name: :undefined,
    address: :undefined,
    conn_pid: :undefined,
    action: :undefined,
    module: :undefined
  )

  def prepare_tests(testSpec, node)
      when elem(testSpec, 0) === :testspec and
             is_atom(node) do
    case :lists.keysearch(node, 1, prepare_tests(testSpec)) do
      {:value, {^node, run, skip}} ->
        {run, skip}

      false ->
        {[], []}
    end
  end

  def prepare_tests(testSpec) when elem(testSpec, 0) === :testspec do
    tests = r_testspec(testSpec, :tests)
    {run, skip} = get_run_and_skip(tests, [], [])

    nodeList =
      :lists.map(
        fn n ->
          {n, {[], []}}
        end,
        list_nodes(testSpec)
      )

    nodeList1 = run_per_node(run, nodeList, r_testspec(testSpec, :merge_tests))
    nodeList2 = skip_per_node(skip, nodeList1)

    result =
      :lists.map(
        fn {node, {run1, skip1}} ->
          run2 =
            :lists.map(
              fn {d, {ss, cs}} ->
                {d, ss, cs}
              end,
              run1
            )

          skip2 =
            :lists.map(
              fn
                {d, {ss, cmt}} ->
                  {d, ss, cmt}

                {d, {s, cs, cmt}} ->
                  {d, s, cs, cmt}
              end,
              skip1
            )

          {node, run2, skip2}
        end,
        nodeList2
      )

    result
  end

  defp run_per_node([{{node, dir}, test} | ts], result, mergeTests) do
    {:value, {^node, {run, skip}}} = :lists.keysearch(node, 1, result)

    run1 =
      case mergeTests do
        false ->
          append({dir, test}, run)

        true ->
          merge_tests(dir, test, run)
      end

    run_per_node(
      ts,
      insert_in_order({node, {run1, skip}}, result, :replace),
      mergeTests
    )
  end

  defp run_per_node([], result, _) do
    result
  end

  defp merge_tests(dir, test = {:all, _}, testDirs) do
    testDirs1 =
      :lists.filter(
        fn
          {d, _} when d == dir ->
            false

          _ ->
            true
        end,
        testDirs
      )

    insert_in_order({dir, test}, testDirs1)
  end

  defp merge_tests(dir, test = {suite, :all}, testDirs) do
    testDirs1 =
      :lists.filter(
        fn
          {d, {s, _}}
          when d == dir and s == suite ->
            false

          _ ->
            true
        end,
        testDirs
      )

    testDirs1 ++ [{dir, test}]
  end

  defp merge_tests(dir, test, testDirs) do
    merge_suites(dir, test, testDirs)
  end

  defp merge_suites(dir, {suite, cases}, [{dir, {suite, cases0}} | dirs]) do
    cases1 = insert_in_order(cases, cases0)
    [{dir, {suite, cases1}} | dirs]
  end

  defp merge_suites(dir, test, [other | dirs]) do
    [other | merge_suites(dir, test, dirs)]
  end

  defp merge_suites(dir, test, []) do
    [{dir, test}]
  end

  defp skip_per_node([{{node, dir}, test} | ts], result) do
    {:value, {^node, {run, skip}}} = :lists.keysearch(node, 1, result)
    skip1 = [{dir, test} | skip]

    skip_per_node(
      ts,
      insert_in_order({node, {run, skip1}}, result, :replace)
    )
  end

  defp skip_per_node([], result) do
    result
  end

  defp get_run_and_skip([{{node, dir}, suites} | tests], run, skip) do
    testDir =
      :ct_util.get_testdir(
        dir,
        try do
          :erlang.element(1, hd(suites))
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end
      )

    case :lists.keysearch(:all, 1, suites) do
      {:value, _} ->
        skipped = get_skipped_suites(node, testDir, suites)

        get_run_and_skip(
          tests,
          [[{{node, testDir}, {:all, :all}}] | run],
          [skipped | skip]
        )

      false ->
        {r, s} = prepare_suites(node, testDir, suites, [], [])
        get_run_and_skip(tests, [r | run], [s | skip])
    end
  end

  defp get_run_and_skip([], run, skip) do
    {:lists.flatten(:lists.reverse(run)), :lists.flatten(:lists.reverse(skip))}
  end

  defp prepare_suites(node, dir, [{suite, cases} | suites], run, skip) do
    case :lists.member(:all, cases) do
      true ->
        skipped = get_skipped_cases(node, dir, suite, cases)

        prepare_suites(node, dir, suites, [[{{node, dir}, {suite, :all}}] | run], [skipped | skip])

      false ->
        {run1, skip1} = prepare_cases(node, dir, suite, cases, run, skip)
        prepare_suites(node, dir, suites, run1, skip1)
    end
  end

  defp prepare_suites(_Node, _Dir, [], run, skip) do
    {:lists.flatten(:lists.reverse(run)), :lists.flatten(:lists.reverse(skip))}
  end

  defp prepare_cases(node, dir, suite, cases, run, skip) do
    case get_skipped_cases(node, dir, suite, cases) do
      [skipAll = {{^node, ^dir}, {^suite, _Cmt}}] ->
        case :lists.any(
               fn
                 {{n, d}, {s, :all}}
                 when n == node and d == dir and s == suite ->
                   true

                 {{n, d}, {s, cs}}
                 when n == node and d == dir and
                        s == suite ->
                   :lists.member(:all, cs)

                 _ ->
                   false
               end,
               :lists.flatten(run)
             ) do
          true ->
            {run, [skipAll | skip]}

          false ->
            {[{{node, dir}, {suite, :all}} | run], [skipAll | skip]}
        end

      skipped ->
        prepC =
          :lists.foldr(
            fn
              {{g, cs}, {:skip, _Cmt}}, acc
              when is_atom(g) ->
                case :lists.keymember(g, 1, cases) do
                  true ->
                    acc

                  false ->
                    [{:skipped, g, cs} | acc]
                end

              {c, {:skip, _Cmt}}, acc ->
                case :lists.member(c, cases) do
                  true ->
                    acc

                  false ->
                    [{:skipped, c} | acc]
                end

              c, acc ->
                [c | acc]
            end,
            [],
            cases
          )

        {[{{node, dir}, {suite, prepC}} | run], [skipped | skip]}
    end
  end

  defp get_skipped_suites(node, dir, suites) do
    :lists.flatten(get_skipped_suites1(node, dir, suites))
  end

  defp get_skipped_suites1(node, dir, [{suite, cases} | suites]) do
    skippedCases = get_skipped_cases(node, dir, suite, cases)
    [skippedCases | get_skipped_suites1(node, dir, suites)]
  end

  defp get_skipped_suites1(_, _, []) do
    []
  end

  defp get_skipped_cases(node, dir, suite, cases) do
    case :lists.keysearch(:all, 1, cases) do
      {:value, {:all, {:skip, cmt}}} ->
        [{{node, dir}, {suite, cmt}}]

      _ ->
        get_skipped_cases1(node, dir, suite, cases)
    end
  end

  defp get_skipped_cases1(node, dir, suite, [{case__, {:skip, cmt}} | cs]) do
    [
      {{node, dir}, {suite, case__, cmt}}
      | get_skipped_cases1(node, dir, suite, cs)
    ]
  end

  defp get_skipped_cases1(node, dir, suite, [_Case | cs]) do
    get_skipped_cases1(node, dir, suite, cs)
  end

  defp get_skipped_cases1(_, _, _, []) do
    []
  end

  def collect_tests_from_file(specs, relaxed) do
    collect_tests_from_file(specs, [node()], relaxed)
  end

  def collect_tests_from_file(specs, nodes, relaxed) when is_list(nodes) do
    nodeRefs =
      :lists.map(
        fn n ->
          {:undefined, n}
        end,
        nodes
      )

    {join, specs1} =
      cond do
        is_list(hd(hd(specs))) ->
          {true, hd(specs)}

        true ->
          {false, specs}
      end

    specs2 =
      for s <- specs1 do
        :filename.absname(s)
      end

    tS0 = r_testspec(nodes: nodeRefs)

    try do
      create_testspecs(specs2, tS0, relaxed, join)
    catch
      _, error = {:error, _} ->
        error

      _, error ->
        {:error, error}
    else
      {{[], _}, separateTestSpecs} ->
        filter_and_convert(separateTestSpecs)

      {{_, r_testspec(tests: [])}, separateTestSpecs} ->
        filter_and_convert(separateTestSpecs)

      {joined, separateTestSpecs} ->
        [
          filter_and_convert(joined)
          | filter_and_convert(separateTestSpecs)
        ]
    end
  end

  defp filter_and_convert(joined) when is_tuple(joined) do
    hd(filter_and_convert([joined]))
  end

  defp filter_and_convert([{_, r_testspec(tests: [])} | tSs]) do
    filter_and_convert(tSs)
  end

  defp filter_and_convert([
         {[{specFile, mergeTests} | sMs], testSpec}
         | tSs
       ]) do
    r_testspec(config: cfgFiles) = testSpec

    testSpec1 =
      r_testspec(testSpec,
        config: delete_dups(cfgFiles),
        merge_tests: mergeTests
      )

    [
      {[
         specFile
         | for {sF, _} <- sMs do
             sF
           end
       ], testSpec1}
      | filter_and_convert(tSs)
    ]
  end

  defp filter_and_convert([]) do
    []
  end

  defp delete_dups(elems) do
    delete_dups1(:lists.reverse(elems), [])
  end

  defp delete_dups1([e | es], keep) do
    case :lists.member(e, es) do
      true ->
        delete_dups1(es, keep)

      false ->
        delete_dups1(es, [e | keep])
    end
  end

  defp delete_dups1([], keep) do
    keep
  end

  defp create_testspecs(specs, testSpec, relaxed, join) do
    specsTree = create_spec_tree(specs, testSpec, join, [])
    create_specs(specsTree, testSpec, testSpec, relaxed)
  end

  defp create_spec_tree([spec | specs], tS, joinWithNext, known) do
    specDir = :filename.dirname(:filename.absname(spec))
    tS1 = r_testspec(tS, spec_dir: specDir)
    specAbsName = get_absfile(spec, tS1)

    case :lists.member(specAbsName, known) do
      true ->
        throw({:error, {:cyclic_reference, specAbsName}})

      false ->
        case :file.consult(specAbsName) do
          {:ok, terms} ->
            terms1 = replace_names(terms)
            {inclJoin, inclSep} = get_included_specs(terms1, tS1)

            {specAbsName, terms1, create_spec_tree(inclJoin, tS, true, [specAbsName | known]),
             create_spec_tree(inclSep, tS, false, [specAbsName | known]), joinWithNext,
             create_spec_tree(specs, tS, joinWithNext, known)}

          {:error, reason} ->
            reasonStr =
              :lists.flatten(
                :io_lib.format(
                  '~ts',
                  [:file.format_error(reason)]
                )
              )

            throw({:error, {specAbsName, reasonStr}})
        end
    end
  end

  defp create_spec_tree([], _TS, _JoinWithNext, _Known) do
    []
  end

  defp create_specs(
         {spec, terms, inclJoin, inclSep, joinWithNext, nextSpec},
         testSpec,
         testSpec0,
         relaxed
       ) do
    specDir = :filename.dirname(:filename.absname(spec))
    testSpec1 = create_spec(terms, r_testspec(testSpec, spec_dir: specDir), joinWithNext, relaxed)
    {{joinSpecs1, joinTS1}, separate1} = create_specs(inclJoin, testSpec1, testSpec0, relaxed)

    {{joinSpecs2, joinTS2}, separate2} =
      case joinWithNext do
        true ->
          create_specs(nextSpec, joinTS1, testSpec0, relaxed)

        false ->
          {{[], joinTS1}, []}
      end

    {sepJoinSpecs, separate3} = create_specs(inclSep, testSpec0, testSpec0, relaxed)

    {sepJoinSpecs1, separate4} =
      case joinWithNext do
        true ->
          {{[], testSpec}, []}

        false ->
          create_specs(nextSpec, testSpec0, testSpec0, relaxed)
      end

    specInfo = {spec, r_testspec(testSpec1, :merge_tests)}

    allSeparate =
      for tSData = {ss, _TS} <-
            separate3 ++ separate1 ++ [sepJoinSpecs] ++ separate2 ++ [sepJoinSpecs1] ++ separate4,
          ss != [] do
        tSData
      end

    case {joinWithNext, joinSpecs1} do
      {true, _} ->
        {{[specInfo | joinSpecs1 ++ joinSpecs2], joinTS2}, allSeparate}

      {false, []} ->
        {{[], testSpec}, [{[specInfo], testSpec1} | allSeparate]}

      {false, _} ->
        {{[specInfo | joinSpecs1 ++ joinSpecs2], joinTS2}, allSeparate}
    end
  end

  defp create_specs([], testSpec, _, _Relaxed) do
    {{[], testSpec}, []}
  end

  defp create_spec(terms, testSpec, joinedByPrev, relaxed) do
    terms1 =
      cond do
        not joinedByPrev ->
          [{:set_merge_tests, true} | terms]

        true ->
          [{:set_merge_tests, false} | terms]
      end

    tS =
      r_testspec(
        tests: tests,
        logdir: logDirs
      ) = collect_tests({false, terms1}, testSpec, relaxed)

    logDirs1 = :lists.delete('.', logDirs) ++ ['.']
    r_testspec(tS, tests: :lists.flatten(tests), logdir: logDirs1)
  end

  def collect_tests_from_list(terms, relaxed) do
    collect_tests_from_list(terms, [node()], relaxed)
  end

  def collect_tests_from_list(terms, nodes, relaxed) when is_list(nodes) do
    {:ok, cwd} = :file.get_cwd()

    nodeRefs =
      :lists.map(
        fn n ->
          {:undefined, n}
        end,
        nodes
      )

    case (try do
            collect_tests({true, terms}, r_testspec(nodes: nodeRefs, spec_dir: cwd), relaxed)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      e = {:error, _} ->
        e

      tS ->
        r_testspec(tests: tests, logdir: logDirs) = tS
        logDirs1 = :lists.delete('.', logDirs) ++ ['.']
        r_testspec(tS, tests: :lists.flatten(tests), logdir: logDirs1)
    end
  end

  defp collect_tests({replace, terms}, testSpec = r_testspec(alias: as, nodes: ns), relaxed) do
    :erlang.put(:relaxed, relaxed)

    terms1 =
      cond do
        replace ->
          replace_names(terms)

        true ->
          terms
      end

    {mergeTestsDef, terms2} =
      case :proplists.get_value(:set_merge_tests, terms1, true) do
        false ->
          {r_testspec(testSpec, :merge_tests), :proplists.delete(:merge_tests, terms1)}

        true ->
          {true, terms1}
      end

    testSpec1 =
      get_global(
        terms2,
        r_testspec(testSpec,
          alias: :lists.reverse(as),
          nodes: :lists.reverse(ns),
          merge_tests: mergeTestsDef
        )
      )

    testSpec2 = get_all_nodes(terms2, testSpec1)
    {terms3, testSpec3} = filter_init_terms(terms2, [], testSpec2)
    add_tests(terms3, testSpec3)
  end

  defp replace_names(terms) do
    defs =
      :lists.flatmap(
        fn
          def__ = {:define, name, _Replacement} ->
            cond do
              not is_atom(name) ->
                throw({:illegal_name_in_testspec, name})

              true ->
                [first | _] = :erlang.atom_to_list(name)

                cond do
                  first == ?? or first == ?$ or first == ?_ or (first >= ?A and first <= ?Z) ->
                    [def__]

                  true ->
                    throw({:illegal_name_in_testspec, name})
                end
            end

          _ ->
            []
        end,
        terms
      )

    defProps = replace_names_in_defs(defs, [])
    replace_names(terms, [], defProps)
  end

  defp replace_names_in_defs([def__ | left], modDefs) do
    [{:define, name, replacement}] = replace_names([def__], [], modDefs)

    replace_names_in_defs(
      left,
      [{name, replacement} | modDefs]
    )
  end

  defp replace_names_in_defs([], modDefs) do
    modDefs
  end

  defp replace_names([term | ts], modified, defs)
       when is_tuple(term) do
    [typeTag | data] = :erlang.tuple_to_list(term)

    term1 =
      :erlang.list_to_tuple([
        typeTag
        | replace_names_in_elems(data, [], defs)
      ])

    replace_names(ts, [term1 | modified], defs)
  end

  defp replace_names([term | ts], modified, defs)
       when is_atom(term) do
    case :proplists.get_value(term, defs) do
      :undefined ->
        replace_names(ts, [term | modified], defs)

      replacement ->
        replace_names(ts, [replacement | modified], defs)
    end
  end

  defp replace_names([term = [ch | _] | ts], modified, defs)
       when is_integer(ch) do
    term1 = replace_names_in_string(term, defs)
    replace_names(ts, [term1 | modified], defs)
  end

  defp replace_names([term | ts], modified, defs) do
    replace_names(ts, [term | modified], defs)
  end

  defp replace_names([], modified, _Defs) do
    :lists.reverse(modified)
  end

  defp replace_names_in_elems([elem | es], modified, defs)
       when is_tuple(elem) do
    elem1 = :erlang.list_to_tuple(replace_names_in_elems(:erlang.tuple_to_list(elem), [], defs))
    replace_names_in_elems(es, [elem1 | modified], defs)
  end

  defp replace_names_in_elems([elem | es], modified, defs)
       when is_atom(elem) do
    case :proplists.get_value(elem, defs) do
      :undefined ->
        elem1 = replace_names_in_node(elem, defs)
        replace_names_in_elems(es, [elem1 | modified], defs)

      replacement ->
        replace_names_in_elems(es, [replacement | modified], defs)
    end
  end

  defp replace_names_in_elems([elem = [ch | _] | es], modified, defs)
       when is_integer(ch) do
    case replace_names_in_string(elem, defs) do
      ^elem ->
        list = replace_names_in_elems(elem, [], defs)
        replace_names_in_elems(es, [list | modified], defs)

      elem1 ->
        replace_names_in_elems(es, [elem1 | modified], defs)
    end
  end

  defp replace_names_in_elems([elem | es], modified, defs)
       when is_list(elem) do
    list = replace_names_in_elems(elem, [], defs)
    replace_names_in_elems(es, [list | modified], defs)
  end

  defp replace_names_in_elems([elem | es], modified, defs) do
    replace_names_in_elems(es, [elem | modified], defs)
  end

  defp replace_names_in_elems([], modified, _Defs) do
    :lists.reverse(modified)
  end

  defp replace_names_in_string(
         term,
         defs = [{name, replacement = [ch | _]} | ds]
       )
       when is_integer(ch) do
    try do
      :re.replace(term, [?' | :erlang.atom_to_list(name)] ++ '\'', replacement, [
        {:return, :list},
        :unicode
      ])
    catch
      _, _ ->
        term
    else
      ^term ->
        replace_names_in_string(term, ds)

      term1 ->
        replace_names_in_string(term1, defs)
    end
  end

  defp replace_names_in_string(term, [_ | ds]) do
    replace_names_in_string(term, ds)
  end

  defp replace_names_in_string(term, []) do
    term
  end

  defp replace_names_in_node(node, defs) do
    string = :erlang.atom_to_list(node)

    case :lists.member(?@, string) do
      true ->
        :erlang.list_to_atom(
          replace_names_in_node1(
            string,
            defs
          )
        )

      false ->
        node
    end
  end

  defp replace_names_in_node1(nodeStr, defs = [{name, replacement} | ds]) do
    replStr =
      case replacement do
        [ch | _] when is_integer(ch) ->
          replacement

        _ when is_atom(replacement) ->
          :erlang.atom_to_list(replacement)

        _ ->
          false
      end

    cond do
      replStr == false ->
        replace_names_in_node1(nodeStr, ds)

      true ->
        case :re.replace(nodeStr, :erlang.atom_to_list(name), replStr, [
               {:return, :list},
               :unicode
             ]) do
          ^nodeStr ->
            replace_names_in_node1(nodeStr, ds)

          nodeStr1 ->
            replace_names_in_node1(nodeStr1, defs)
        end
    end
  end

  defp replace_names_in_node1(nodeStr, []) do
    nodeStr
  end

  defp get_included_specs(terms, testSpec) do
    get_included_specs(terms, testSpec, [], [])
  end

  defp get_included_specs([{:specs, how, specOrSpecs} | ts], testSpec, join, sep) do
    specs =
      case specOrSpecs do
        [file | _] when is_list(file) ->
          for spec <- specOrSpecs do
            get_absfile(spec, testSpec)
          end

        [ch | _] when is_integer(ch) ->
          [get_absfile(specOrSpecs, testSpec)]
      end

    cond do
      how == :join ->
        get_included_specs(ts, testSpec, join ++ specs, sep)

      true ->
        get_included_specs(ts, testSpec, join, sep ++ specs)
    end
  end

  defp get_included_specs([_ | ts], testSpec, join, sep) do
    get_included_specs(ts, testSpec, join, sep)
  end

  defp get_included_specs([], _, join, sep) do
    {join, sep}
  end

  defp get_global([{:merge_tests, bool} | ts], spec) do
    get_global(ts, r_testspec(spec, merge_tests: bool))
  end

  defp get_global(
         [{:alias, ref, dir} | ts],
         spec = r_testspec(alias: refs)
       ) do
    get_global(
      ts,
      r_testspec(spec, alias: [{ref, get_absdir(dir, spec)} | refs])
    )
  end

  defp get_global(
         [{:node, ref, node} | ts],
         spec = r_testspec(nodes: refs)
       ) do
    get_global(
      ts,
      r_testspec(spec, nodes: [{ref, node} | :lists.keydelete(node, 2, refs)])
    )
  end

  defp get_global([_ | ts], spec) do
    get_global(ts, spec)
  end

  defp get_global([], spec = r_testspec(nodes: ns, alias: as)) do
    r_testspec(spec,
      nodes: :lists.reverse(ns),
      alias: :lists.reverse(as)
    )
  end

  defp get_absfile(callback, fullName, r_testspec(spec_dir: specDir)) do
    {:ok, oldWd} = :file.get_cwd()
    :ok = :file.set_cwd(specDir)
    r = callback.check_parameter(fullName)
    :ok = :file.set_cwd(oldWd)

    case r do
      {:ok, {:file, ^fullName}} ->
        file = :filename.basename(fullName)
        dir = get_absname(:filename.dirname(fullName), specDir)
        :filename.join(dir, file)

      {:ok, {:config, ^fullName}} ->
        fullName

      {:error, {:nofile, ^fullName}} ->
        fullName

      {:error, {:wrong_config, ^fullName}} ->
        fullName
    end
  end

  defp get_absfile(fullName, r_testspec(spec_dir: specDir)) do
    file = :filename.basename(fullName)
    dir = get_absname(:filename.dirname(fullName), specDir)
    :filename.join(dir, file)
  end

  defp get_absdir(dir, r_testspec(spec_dir: specDir)) do
    get_absname(dir, specDir)
  end

  defp get_absname(dir, specDir) do
    absName = :filename.absname(dir, specDir)
    shorten_path(absName, specDir)
  end

  defp shorten_path(path, specDir) do
    case shorten_split_path(:filename.split(path), []) do
      [] ->
        [root | _] = :filename.split(specDir)
        root

      short ->
        :filename.join(short)
    end
  end

  defp shorten_split_path(['..' | path], soFar) do
    shorten_split_path(path, tl(soFar))
  end

  defp shorten_split_path(['.' | path], soFar) do
    shorten_split_path(path, soFar)
  end

  defp shorten_split_path([dir | path], soFar) do
    shorten_split_path(path, [dir | soFar])
  end

  defp shorten_split_path([], soFar) do
    :lists.reverse(soFar)
  end

  defp get_all_nodes([{:suites, nodes, _, _} | ts], spec)
       when is_list(nodes) do
    get_all_nodes(ts, save_nodes(nodes, spec))
  end

  defp get_all_nodes([{:suites, node, _, _} | ts], spec) do
    get_all_nodes(ts, save_nodes([node], spec))
  end

  defp get_all_nodes([{:groups, [char | _], _, _, _} | ts], spec)
       when is_integer(char) do
    get_all_nodes(ts, spec)
  end

  defp get_all_nodes([{:groups, nodes, _, _, _} | ts], spec)
       when is_list(nodes) do
    get_all_nodes(ts, save_nodes(nodes, spec))
  end

  defp get_all_nodes([{:groups, nodes, _, _, _, _} | ts], spec)
       when is_list(nodes) do
    get_all_nodes(ts, save_nodes(nodes, spec))
  end

  defp get_all_nodes([{:groups, _, _, _, {:cases, _}} | ts], spec) do
    get_all_nodes(ts, spec)
  end

  defp get_all_nodes([{:groups, node, _, _, _} | ts], spec) do
    get_all_nodes(ts, save_nodes([node], spec))
  end

  defp get_all_nodes([{:groups, node, _, _, _, _} | ts], spec) do
    get_all_nodes(ts, save_nodes([node], spec))
  end

  defp get_all_nodes([{:cases, nodes, _, _, _} | ts], spec)
       when is_list(nodes) do
    get_all_nodes(ts, save_nodes(nodes, spec))
  end

  defp get_all_nodes([{:cases, node, _, _, _} | ts], spec) do
    get_all_nodes(ts, save_nodes([node], spec))
  end

  defp get_all_nodes([{:skip_suites, nodes, _, _, _} | ts], spec)
       when is_list(nodes) do
    get_all_nodes(ts, save_nodes(nodes, spec))
  end

  defp get_all_nodes([{:skip_suites, node, _, _, _} | ts], spec) do
    get_all_nodes(ts, save_nodes([node], spec))
  end

  defp get_all_nodes(
         [{:skip_groups, [char | _], _, _, _, _} | ts],
         spec
       )
       when is_integer(char) do
    get_all_nodes(ts, spec)
  end

  defp get_all_nodes([{:skip_groups, nodes, _, _, _, _} | ts], spec)
       when is_list(nodes) do
    get_all_nodes(ts, save_nodes(nodes, spec))
  end

  defp get_all_nodes(
         [{:skip_groups, node, _, _, _, _} | ts],
         spec
       ) do
    get_all_nodes(ts, save_nodes([node], spec))
  end

  defp get_all_nodes(
         [{:skip_groups, nodes, _, _, _, _, _} | ts],
         spec
       )
       when is_list(nodes) do
    get_all_nodes(ts, save_nodes(nodes, spec))
  end

  defp get_all_nodes(
         [{:skip_groups, node, _, _, _, _, _} | ts],
         spec
       ) do
    get_all_nodes(ts, save_nodes([node], spec))
  end

  defp get_all_nodes([{:skip_cases, nodes, _, _, _, _} | ts], spec)
       when is_list(nodes) do
    get_all_nodes(ts, save_nodes(nodes, spec))
  end

  defp get_all_nodes([{:skip_cases, node, _, _, _, _} | ts], spec) do
    get_all_nodes(ts, save_nodes([node], spec))
  end

  defp get_all_nodes([_Other | ts], spec) do
    get_all_nodes(ts, spec)
  end

  defp get_all_nodes([], spec) do
    spec
  end

  defp filter_init_terms([{:init, initOptions} | ts], newTerms, spec) do
    filter_init_terms(
      [
        {:init, list_nodes(spec), initOptions}
        | ts
      ],
      newTerms,
      spec
    )
  end

  defp filter_init_terms([{:init, :all_nodes, initOptions} | ts], newTerms, spec) do
    filter_init_terms(
      [
        {:init, list_nodes(spec), initOptions}
        | ts
      ],
      newTerms,
      spec
    )
  end

  defp filter_init_terms([{:init, nodeRef, initOptions} | ts], newTerms, spec)
       when is_atom(nodeRef) do
    filter_init_terms(
      [
        {:init, [nodeRef], initOptions}
        | ts
      ],
      newTerms,
      spec
    )
  end

  defp filter_init_terms([{:init, nodeRefs, initOption} | ts], newTerms, spec)
       when is_tuple(initOption) do
    filter_init_terms(
      [
        {:init, nodeRefs, [initOption]}
        | ts
      ],
      newTerms,
      spec
    )
  end

  defp filter_init_terms(
         [
           {:init, [nodeRef | nodeRefs], initOptions}
           | ts
         ],
         newTerms,
         spec = r_testspec(init: initData)
       ) do
    nodeStartOptions =
      case :lists.keyfind(:node_start, 1, initOptions) do
        {:node_start, nSOptions} ->
          case :lists.keyfind(:callback_module, 1, nSOptions) do
            {:callback_module, _Callback} ->
              nSOptions

            false ->
              [{:callback_module, :ct_slave} | nSOptions]
          end

        false ->
          []
      end

    evalTerms =
      case :lists.keyfind(:eval, 1, initOptions) do
        {:eval, mFA} when is_tuple(mFA) ->
          [mFA]

        {:eval, mFAs} when is_list(mFAs) ->
          mFAs

        false ->
          []
      end

    node = ref2node(nodeRef, r_testspec(spec, :nodes))
    initData2 = add_option({:node_start, nodeStartOptions}, node, initData, true)
    initData3 = add_option({:eval, evalTerms}, node, initData2, false)

    filter_init_terms(
      [{:init, nodeRefs, initOptions} | ts],
      newTerms,
      r_testspec(spec, init: initData3)
    )
  end

  defp filter_init_terms([{:init, [], _} | ts], newTerms, spec) do
    filter_init_terms(ts, newTerms, spec)
  end

  defp filter_init_terms([term | ts], newTerms, spec) do
    filter_init_terms(ts, [term | newTerms], spec)
  end

  defp filter_init_terms([], newTerms, spec) do
    {:lists.reverse(newTerms), spec}
  end

  defp add_option({key, value}, node, list, warnIfExists)
       when is_list(value) do
    oldOptions =
      case :lists.keyfind(node, 1, list) do
        {^node, options} ->
          options

        false ->
          []
      end

    newOption =
      case :lists.keyfind(key, 1, oldOptions) do
        {^key, oldOption}
        when warnIfExists and
               oldOption != [] ->
          :io.format('There is an option ~w=~w already defined for node ~w, skipping new ~w~n', [
            key,
            oldOption,
            node,
            value
          ])

          oldOption

        {^key, oldOption} ->
          oldOption ++ value

        false ->
          value
      end

    :lists.keystore(node, 1, list, {node, :lists.keystore(key, 1, oldOptions, {key, newOption})})
  end

  defp add_option({key, value}, node, list, warnIfExists) do
    add_option({key, [value]}, node, list, warnIfExists)
  end

  defp save_nodes(nodes, spec = r_testspec(nodes: nodeRefs)) do
    nodeRefs1 =
      :lists.foldr(
        fn
          :all_nodes, nR ->
            nR

          node, nR ->
            case :lists.keymember(node, 1, nR) do
              true ->
                nR

              false ->
                case :lists.keymember(node, 2, nR) do
                  true ->
                    nR

                  false ->
                    [{:undefined, node} | nR]
                end
            end
        end,
        nodeRefs,
        nodes
      )

    r_testspec(spec, nodes: nodeRefs1)
  end

  defp list_nodes(r_testspec(nodes: nodeRefs)) do
    :lists.map(
      fn {_Ref, node} ->
        node
      end,
      nodeRefs
    )
  end

  def get_tests(specs) do
    case collect_tests_from_file(specs, true) do
      tests when is_list(tests) ->
        {:ok,
         for {s, r} <- tests do
           {s, prepare_tests(r)}
         end}

      error ->
        error
    end
  end

  defp add_tests([{:suites, :all_nodes, dir, ss} | ts], spec) do
    add_tests(
      [{:suites, list_nodes(spec), dir, ss} | ts],
      spec
    )
  end

  defp add_tests([{:suites, dir, ss} | ts], spec) do
    add_tests([{:suites, :all_nodes, dir, ss} | ts], spec)
  end

  defp add_tests([{:suites, nodes, dir, ss} | ts], spec)
       when is_list(nodes) do
    ts1 = per_node(nodes, :suites, [dir, ss], ts, r_testspec(spec, :nodes))
    add_tests(ts1, spec)
  end

  defp add_tests([{:suites, node, dir, ss} | ts], spec) do
    tests = r_testspec(spec, :tests)

    tests1 =
      insert_suites(
        ref2node(node, r_testspec(spec, :nodes)),
        ref2dir(dir, spec),
        ss,
        tests,
        r_testspec(spec, :merge_tests)
      )

    add_tests(ts, r_testspec(spec, tests: tests1))
  end

  defp add_tests(
         [{:groups, :all_nodes, dir, suite, gs} | ts],
         spec
       ) do
    add_tests(
      [
        {:groups, list_nodes(spec), dir, suite, gs}
        | ts
      ],
      spec
    )
  end

  defp add_tests(
         [
           {:groups, :all_nodes, dir, suite, gs, {:cases, tCs}}
           | ts
         ],
         spec
       ) do
    add_tests(
      [
        {:groups, list_nodes(spec), dir, suite, gs, {:cases, tCs}}
        | ts
      ],
      spec
    )
  end

  defp add_tests([{:groups, dir, suite, gs} | ts], spec) do
    add_tests(
      [{:groups, :all_nodes, dir, suite, gs} | ts],
      spec
    )
  end

  defp add_tests(
         [{:groups, dir, suite, gs, {:cases, tCs}} | ts],
         spec
       ) do
    add_tests(
      [
        {:groups, :all_nodes, dir, suite, gs, {:cases, tCs}}
        | ts
      ],
      spec
    )
  end

  defp add_tests([{:groups, nodes, dir, suite, gs} | ts], spec)
       when is_list(nodes) do
    ts1 = per_node(nodes, :groups, [dir, suite, gs], ts, r_testspec(spec, :nodes))
    add_tests(ts1, spec)
  end

  defp add_tests(
         [
           {:groups, nodes, dir, suite, gs, {:cases, tCs}}
           | ts
         ],
         spec
       )
       when is_list(nodes) do
    ts1 = per_node(nodes, :groups, [dir, suite, gs, {:cases, tCs}], ts, r_testspec(spec, :nodes))
    add_tests(ts1, spec)
  end

  defp add_tests([{:groups, node, dir, suite, gs} | ts], spec) do
    tests = r_testspec(spec, :tests)

    tests1 =
      insert_groups(
        ref2node(node, r_testspec(spec, :nodes)),
        ref2dir(dir, spec),
        suite,
        gs,
        :all,
        tests,
        r_testspec(spec, :merge_tests)
      )

    add_tests(ts, r_testspec(spec, tests: tests1))
  end

  defp add_tests(
         [
           {:groups, node, dir, suite, gs, {:cases, tCs}}
           | ts
         ],
         spec
       ) do
    tests = r_testspec(spec, :tests)

    tests1 =
      insert_groups(
        ref2node(node, r_testspec(spec, :nodes)),
        ref2dir(dir, spec),
        suite,
        gs,
        tCs,
        tests,
        r_testspec(spec, :merge_tests)
      )

    add_tests(ts, r_testspec(spec, tests: tests1))
  end

  defp add_tests(
         [{:cases, :all_nodes, dir, suite, cs} | ts],
         spec
       ) do
    add_tests(
      [
        {:cases, list_nodes(spec), dir, suite, cs}
        | ts
      ],
      spec
    )
  end

  defp add_tests([{:cases, dir, suite, cs} | ts], spec) do
    add_tests(
      [{:cases, :all_nodes, dir, suite, cs} | ts],
      spec
    )
  end

  defp add_tests([{:cases, nodes, dir, suite, cs} | ts], spec)
       when is_list(nodes) do
    ts1 = per_node(nodes, :cases, [dir, suite, cs], ts, r_testspec(spec, :nodes))
    add_tests(ts1, spec)
  end

  defp add_tests([{:cases, node, dir, suite, cs} | ts], spec) do
    tests = r_testspec(spec, :tests)

    tests1 =
      insert_cases(
        ref2node(node, r_testspec(spec, :nodes)),
        ref2dir(dir, spec),
        suite,
        cs,
        tests,
        r_testspec(spec, :merge_tests)
      )

    add_tests(ts, r_testspec(spec, tests: tests1))
  end

  defp add_tests(
         [{:skip_suites, :all_nodes, dir, ss, cmt} | ts],
         spec
       ) do
    add_tests(
      [
        {:skip_suites, list_nodes(spec), dir, ss, cmt}
        | ts
      ],
      spec
    )
  end

  defp add_tests([{:skip_suites, dir, ss, cmt} | ts], spec) do
    add_tests(
      [
        {:skip_suites, :all_nodes, dir, ss, cmt}
        | ts
      ],
      spec
    )
  end

  defp add_tests(
         [{:skip_suites, nodes, dir, ss, cmt} | ts],
         spec
       )
       when is_list(nodes) do
    ts1 = per_node(nodes, :skip_suites, [dir, ss, cmt], ts, r_testspec(spec, :nodes))
    add_tests(ts1, spec)
  end

  defp add_tests(
         [{:skip_suites, node, dir, ss, cmt} | ts],
         spec
       ) do
    tests = r_testspec(spec, :tests)

    tests1 =
      skip_suites(
        ref2node(node, r_testspec(spec, :nodes)),
        ref2dir(dir, spec),
        ss,
        cmt,
        tests,
        r_testspec(spec, :merge_tests)
      )

    add_tests(ts, r_testspec(spec, tests: tests1))
  end

  defp add_tests(
         [
           {:skip_groups, :all_nodes, dir, suite, gs, cmt}
           | ts
         ],
         spec
       ) do
    add_tests(
      [
        {:skip_groups, list_nodes(spec), dir, suite, gs, cmt}
        | ts
      ],
      spec
    )
  end

  defp add_tests(
         [
           {:skip_groups, :all_nodes, dir, suite, gs, {:cases, tCs}, cmt}
           | ts
         ],
         spec
       ) do
    add_tests(
      [
        {:skip_groups, list_nodes(spec), dir, suite, gs, {:cases, tCs}, cmt}
        | ts
      ],
      spec
    )
  end

  defp add_tests(
         [{:skip_groups, dir, suite, gs, cmt} | ts],
         spec
       ) do
    add_tests(
      [
        {:skip_groups, :all_nodes, dir, suite, gs, cmt}
        | ts
      ],
      spec
    )
  end

  defp add_tests(
         [
           {:skip_groups, dir, suite, gs, {:cases, tCs}, cmt}
           | ts
         ],
         spec
       ) do
    add_tests(
      [
        {:skip_groups, :all_nodes, dir, suite, gs, {:cases, tCs}, cmt}
        | ts
      ],
      spec
    )
  end

  defp add_tests(
         [
           {:skip_groups, nodes, dir, suite, gs, cmt}
           | ts
         ],
         spec
       )
       when is_list(nodes) do
    ts1 = per_node(nodes, :skip_groups, [dir, suite, gs, cmt], ts, r_testspec(spec, :nodes))
    add_tests(ts1, spec)
  end

  defp add_tests(
         [
           {:skip_groups, nodes, dir, suite, gs, {:cases, tCs}, cmt}
           | ts
         ],
         spec
       )
       when is_list(nodes) do
    ts1 =
      per_node(
        nodes,
        :skip_groups,
        [dir, suite, gs, {:cases, tCs}, cmt],
        ts,
        r_testspec(spec, :nodes)
      )

    add_tests(ts1, spec)
  end

  defp add_tests(
         [
           {:skip_groups, node, dir, suite, gs, cmt}
           | ts
         ],
         spec
       ) do
    tests = r_testspec(spec, :tests)

    tests1 =
      skip_groups(
        ref2node(node, r_testspec(spec, :nodes)),
        ref2dir(dir, spec),
        suite,
        gs,
        :all,
        cmt,
        tests,
        r_testspec(spec, :merge_tests)
      )

    add_tests(ts, r_testspec(spec, tests: tests1))
  end

  defp add_tests(
         [
           {:skip_groups, node, dir, suite, gs, {:cases, tCs}, cmt}
           | ts
         ],
         spec
       ) do
    tests = r_testspec(spec, :tests)

    tests1 =
      skip_groups(
        ref2node(node, r_testspec(spec, :nodes)),
        ref2dir(dir, spec),
        suite,
        gs,
        tCs,
        cmt,
        tests,
        r_testspec(spec, :merge_tests)
      )

    add_tests(ts, r_testspec(spec, tests: tests1))
  end

  defp add_tests(
         [
           {:skip_cases, :all_nodes, dir, suite, cs, cmt}
           | ts
         ],
         spec
       ) do
    add_tests(
      [
        {:skip_cases, list_nodes(spec), dir, suite, cs, cmt}
        | ts
      ],
      spec
    )
  end

  defp add_tests(
         [{:skip_cases, dir, suite, cs, cmt} | ts],
         spec
       ) do
    add_tests(
      [
        {:skip_cases, :all_nodes, dir, suite, cs, cmt}
        | ts
      ],
      spec
    )
  end

  defp add_tests(
         [
           {:skip_cases, nodes, dir, suite, cs, cmt}
           | ts
         ],
         spec
       )
       when is_list(nodes) do
    ts1 = per_node(nodes, :skip_cases, [dir, suite, cs, cmt], ts, r_testspec(spec, :nodes))
    add_tests(ts1, spec)
  end

  defp add_tests(
         [{:skip_cases, node, dir, suite, cs, cmt} | ts],
         spec
       ) do
    tests = r_testspec(spec, :tests)

    tests1 =
      skip_cases(
        ref2node(node, r_testspec(spec, :nodes)),
        ref2dir(dir, spec),
        suite,
        cs,
        cmt,
        tests,
        r_testspec(spec, :merge_tests)
      )

    add_tests(ts, r_testspec(spec, tests: tests1))
  end

  defp add_tests([{:config, nodes, cfgDir, files} | ts], spec)
       when is_list(nodes) or nodes == :all_nodes do
    add_tests(
      [{:config, nodes, {cfgDir, files}} | ts],
      spec
    )
  end

  defp add_tests(
         [{:config, node, cfgDir, fileOrFiles} | ts],
         spec
       ) do
    add_tests(
      [{:config, node, {cfgDir, fileOrFiles}} | ts],
      spec
    )
  end

  defp add_tests(
         [{:config, cfgDir = [ch | _], files} | ts],
         spec
       )
       when is_integer(ch) do
    add_tests(
      [{:config, :all_nodes, {cfgDir, files}} | ts],
      spec
    )
  end

  defp add_tests([{:event_handler, nodes, hs, args} | ts], spec)
       when is_list(nodes) or nodes == :all_nodes do
    add_tests(
      [{:event_handler, nodes, {hs, args}} | ts],
      spec
    )
  end

  defp add_tests(
         [{:event_handler, node, hOrHs, args} | ts],
         spec
       ) do
    add_tests(
      [{:event_handler, node, {hOrHs, args}} | ts],
      spec
    )
  end

  defp add_tests([{:enable_builtin_hooks, bool} | ts], spec) do
    add_tests(ts, r_testspec(spec, enable_builtin_hooks: bool))
  end

  defp add_tests([{:release_shell, bool} | ts], spec) do
    add_tests(ts, r_testspec(spec, release_shell: bool))
  end

  defp add_tests([{:set_merge_tests, _} | ts], spec) do
    add_tests(ts, spec)
  end

  defp add_tests([{:define, _, _} | ts], spec) do
    add_tests(ts, spec)
  end

  defp add_tests([{:alias, _, _} | ts], spec) do
    add_tests(ts, spec)
  end

  defp add_tests([{:node, _, _} | ts], spec) do
    add_tests(ts, spec)
  end

  defp add_tests([{:merge_tests, _} | ts], spec) do
    add_tests(ts, spec)
  end

  defp add_tests([{:specs, _, _} | ts], spec) do
    add_tests(ts, spec)
  end

  defp add_tests([term = {tag, :all_nodes, data} | ts], spec) do
    case check_term(term) do
      :valid ->
        tests =
          for node <- list_nodes(spec),
              should_be_added(tag, node, data, spec) do
            {tag, node, data}
          end

        add_tests(tests ++ ts, spec)

      :invalid ->
        unknown = r_testspec(spec, :unknown)
        add_tests(ts, r_testspec(spec, unknown: unknown ++ [term]))
    end
  end

  defp add_tests([{tag, [], data} | ts], spec) do
    add_tests([{tag, :all_nodes, data} | ts], spec)
  end

  defp add_tests([{tag, string = [ch | _], data} | ts], spec)
       when is_integer(ch) do
    add_tests(
      [{tag, :all_nodes, {string, data}} | ts],
      spec
    )
  end

  defp add_tests([{tag, nodesOrOther, data} | ts], spec)
       when is_list(nodesOrOther) do
    case :lists.all(
           fn test ->
             is_node(test, r_testspec(spec, :nodes))
           end,
           nodesOrOther
         ) do
      true ->
        ts1 = per_node(nodesOrOther, tag, [data], ts, r_testspec(spec, :nodes))
        add_tests(ts1, spec)

      false ->
        add_tests(
          [
            {tag, :all_nodes, {nodesOrOther, data}}
            | ts
          ],
          spec
        )
    end
  end

  defp add_tests([term = {tag, nodeOrOther, data} | ts], spec) do
    case is_node(nodeOrOther, r_testspec(spec, :nodes)) do
      true ->
        case check_term(term) do
          :valid ->
            node = ref2node(nodeOrOther, r_testspec(spec, :nodes))
            nodeIxData = update_recorded(tag, node, spec) ++ handle_data(tag, node, data, spec)
            add_tests(ts, mod_field(spec, tag, nodeIxData))

          :invalid ->
            unknown = r_testspec(spec, :unknown)
            add_tests(ts, r_testspec(spec, unknown: unknown ++ [term]))
        end

      false ->
        add_tests(
          [{tag, :all_nodes, {nodeOrOther, data}} | ts],
          spec
        )
    end
  end

  defp add_tests([term = {tag, data} | ts], spec) do
    case check_term(term) do
      :valid ->
        add_tests([{tag, :all_nodes, data} | ts], spec)

      :invalid ->
        unknown = r_testspec(spec, :unknown)
        add_tests(ts, r_testspec(spec, unknown: unknown ++ [term]))
    end
  end

  defp add_tests([other | ts], spec) do
    case :erlang.get(:relaxed) do
      true ->
        unknown = r_testspec(spec, :unknown)
        add_tests(ts, r_testspec(spec, unknown: unknown ++ [other]))

      false ->
        throw({:error, {:undefined_term_in_spec, other}})
    end
  end

  defp add_tests([], spec) do
    spec
  end

  defp check_term(term) when is_tuple(term) do
    size = :erlang.size(term)
    [name | _] = :erlang.tuple_to_list(term)
    valid = valid_terms()

    case :lists.member({name, size}, valid) do
      true ->
        :valid

      false ->
        case :lists.keymember(name, 1, valid) do
          true ->
            throw({:error, {:bad_term_in_spec, term}})

          false ->
            case :erlang.get(:relaxed) do
              true ->
                case resembles_ct_term(name, :erlang.size(term)) do
                  true ->
                    :io.format('~nSuspicious term, please check:~n~tp~n', [term])
                    :invalid

                  false ->
                    :invalid
                end

              false ->
                throw({:error, {:undefined_term_in_spec, term}})
            end
        end
    end
  end

  defp handle_data(:logdir, node, dir, spec) do
    [{node, ref2dir(dir, spec)}]
  end

  defp handle_data(:cover, node, file, spec) do
    [{node, get_absfile(file, spec)}]
  end

  defp handle_data(:cover_stop, node, stop, _Spec) do
    [{node, stop}]
  end

  defp handle_data(:include, node, dirs = [d | _], spec)
       when is_list(d) do
    for dir <- dirs do
      {node, ref2dir(dir, spec)}
    end
  end

  defp handle_data(:include, node, dir = [ch | _], spec)
       when is_integer(ch) do
    handle_data(:include, node, [dir], spec)
  end

  defp handle_data(:config, node, file = [ch | _], spec)
       when is_integer(ch) do
    handle_data(:config, node, [file], spec)
  end

  defp handle_data(:config, node, {cfgDir, file = [ch | _]}, spec)
       when is_integer(ch) do
    handle_data(:config, node, {cfgDir, [file]}, spec)
  end

  defp handle_data(:config, node, files = [f | _], spec)
       when is_list(f) do
    for file <- files do
      {node, get_absfile(file, spec)}
    end
  end

  defp handle_data(:config, node, {cfgDir, files = [f | _]}, spec)
       when is_list(f) do
    for file <- files do
      {node, :filename.join(ref2dir(cfgDir, spec), file)}
    end
  end

  defp handle_data(:userconfig, node, cBs, spec)
       when is_list(cBs) do
    for {callback, config} <- cBs do
      {node, {callback, get_absfile(callback, config, spec)}}
    end
  end

  defp handle_data(:userconfig, node, cB, spec)
       when is_tuple(cB) do
    handle_data(:userconfig, node, [cB], spec)
  end

  defp handle_data(:event_handler, node, h, spec)
       when is_atom(h) do
    handle_data(:event_handler, node, {[h], []}, spec)
  end

  defp handle_data(:event_handler, node, {h, args}, spec)
       when is_atom(h) do
    handle_data(:event_handler, node, {[h], args}, spec)
  end

  defp handle_data(:event_handler, node, hs, _Spec)
       when is_list(hs) do
    for evH <- hs do
      {node, evH, []}
    end
  end

  defp handle_data(:event_handler, node, {hs, args}, _Spec)
       when is_list(hs) do
    for evH <- hs do
      {node, evH, args}
    end
  end

  defp handle_data(:ct_hooks, node, hooks, _Spec)
       when is_list(hooks) do
    for hook <- hooks do
      {node, hook}
    end
  end

  defp handle_data(:ct_hooks, node, hook, _Spec) do
    [{node, hook}]
  end

  defp handle_data(:stylesheet, node, cSSFile, spec) do
    [{node, get_absfile(cSSFile, spec)}]
  end

  defp handle_data(:verbosity, node, vLvls, _Spec)
       when is_integer(vLvls) do
    [{node, [{:"$unspecified", vLvls}]}]
  end

  defp handle_data(:verbosity, node, vLvls, _Spec)
       when is_list(vLvls) do
    vLvls1 =
      :lists.map(
        fn
          vLvl = {_Cat, _Lvl} ->
            vLvl

          lvl ->
            {:"$unspecified", lvl}
        end,
        vLvls
      )

    [{node, vLvls1}]
  end

  defp handle_data(:multiply_timetraps, node, mult, _Spec)
       when is_integer(mult) do
    [{node, mult}]
  end

  defp handle_data(:scale_timetraps, node, scale, _Spec)
       when scale == true or scale == false do
    [{node, scale}]
  end

  defp handle_data(:silent_connections, node, :all, _Spec) do
    [{node, [:all]}]
  end

  defp handle_data(:silent_connections, node, conn, _Spec)
       when is_atom(conn) do
    [{node, [conn]}]
  end

  defp handle_data(:silent_connections, node, conns, _Spec) do
    [{node, conns}]
  end

  defp handle_data(_Tag, node, data, _Spec) do
    [{node, data}]
  end

  defp should_be_added(tag, node, _Data, spec) do
    cond do
      tag == :logdir or tag == :logopts or
        tag == :basic_html or tag == :esc_chars or
        tag == :label or tag == :auto_compile or
        tag == :abort_if_missing_suites or tag == :stylesheet or
        tag == :verbosity or tag == :multiply_timetraps or
        tag == :scale_timetraps or tag == :silent_connections ->
        :lists.keymember(ref2node(node, r_testspec(spec, :nodes)), 1, read_field(spec, tag)) ==
          false

      true ->
        true
    end
  end

  defp update_recorded(tag, node, spec) do
    cond do
      tag == :config or tag == :userconfig or
        tag == :event_handler or tag == :ct_hooks or
          tag == :include ->
        read_field(spec, tag)

      true ->
        :lists.keydelete(node, 1, read_field(spec, tag))
    end
  end

  defp per_node(nodes, tag, data, tests, refs) do
    separated = per_node(nodes, tag, data, refs)
    separated ++ tests
  end

  defp per_node([n | ns], tag, data, refs) do
    [
      :erlang.list_to_tuple([
        [tag, ref2node(n, refs)]
        | data
      ])
      | per_node(ns, tag, data, refs)
    ]
  end

  defp per_node([], _, _, _) do
    []
  end

  def testspec_rec2list(rec) do
    {terms, _} =
      :lists.mapfoldl(
        fn
          :unknown, pos ->
            {:erlang.element(pos, rec), pos + 1}

          f, pos ->
            {{f, :erlang.element(pos, rec)}, pos + 1}
        end,
        2,
        Keyword.keys(r_testspec(r_testspec()))
      )

    :lists.flatten(terms)
  end

  def testspec_rec2list(field, rec) when is_atom(field) do
    [term] = testspec_rec2list([field], rec)
    term
  end

  def testspec_rec2list(fields, rec) do
    terms = testspec_rec2list(rec)

    for field <- fields do
      {field, :proplists.get_value(field, terms)}
    end
  end

  defp read_field(rec, fieldName) do
    try do
      :lists.foldl(
        fn
          f, pos when f == fieldName ->
            throw(:erlang.element(pos, rec))

          _, pos ->
            pos + 1
        end,
        2,
        Keyword.keys(r_testspec(r_testspec()))
      )
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp mod_field(rec, fieldName, newVal) do
    [_testspec | recList] = :erlang.tuple_to_list(rec)

    recList1 =
      try do
        :lists.foldl(
          fn
            f, {prev, [_OldVal | rest]}
            when f == fieldName ->
              throw(
                :lists.reverse(prev) ++
                  [
                    newVal
                    | rest
                  ]
              )

            _, {prev, [field | rest]} ->
              {[field | prev], rest}
          end,
          {[], recList},
          Keyword.keys(r_testspec(r_testspec()))
        )
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    :erlang.list_to_tuple([:testspec | recList1])
  end

  defp insert_suites(node, dir, [s | ss], tests, mergeTests) do
    tests1 = insert_cases(node, dir, s, :all, tests, mergeTests)
    insert_suites(node, dir, ss, tests1, mergeTests)
  end

  defp insert_suites(_Node, _Dir, [], tests, _MergeTests) do
    tests
  end

  defp insert_suites(node, dir, s, tests, mergeTests) do
    insert_suites(node, dir, [s], tests, mergeTests)
  end

  defp insert_groups(node, dir, suite, group, cases, tests, mergeTests)
       when is_atom(group) or is_tuple(group) do
    insert_groups(node, dir, suite, [group], cases, tests, mergeTests)
  end

  defp insert_groups(node, dir, suite, groups, cases, tests, false)
       when (cases == :all or is_list(cases)) and is_list(groups) do
    groups1 =
      for gr <- groups do
        cond do
          is_list(gr) ->
            {[gr], cases}

          true ->
            {gr, cases}
        end
      end

    append({{node, dir}, [{suite, groups1}]}, tests)
  end

  defp insert_groups(node, dir, suite, groups, cases, tests, true)
       when (cases == :all or is_list(cases)) and is_list(groups) do
    groups1 =
      for gr <- groups do
        cond do
          is_list(gr) ->
            {[gr], cases}

          true ->
            {gr, cases}
        end
      end

    {tests1, done} =
      :lists.foldr(
        fn
          all = {{n, d}, [{:all, _}]}, {replaced, _}
          when n == node and d == dir ->
            {[all | replaced], true}

          {{n, d}, suites0}, {replaced, _}
          when n == node and d == dir ->
            suites1 = insert_groups1(suite, groups1, suites0)
            {[{{n, d}, suites1} | replaced], true}

          t, {replaced, match} ->
            {[t | replaced], match}
        end,
        {[], false},
        tests
      )

    cond do
      not done ->
        tests ++ [{{node, dir}, [{suite, groups1}]}]

      true ->
        tests1
    end
  end

  defp insert_groups(node, dir, suite, groups, case__, tests, mergeTests)
       when is_atom(case__) do
    cases =
      cond do
        case__ == :all ->
          :all

        true ->
          [case__]
      end

    insert_groups(node, dir, suite, groups, cases, tests, mergeTests)
  end

  defp insert_groups1(_Suite, _Groups, :all) do
    :all
  end

  defp insert_groups1(suite, groups, suites0) do
    case :lists.keysearch(suite, 1, suites0) do
      {:value, {^suite, :all}} ->
        suites0

      {:value, {^suite, grAndCases0}} ->
        grAndCases = insert_groups2(groups, grAndCases0)
        insert_in_order({suite, grAndCases}, suites0, :replace)

      false ->
        insert_in_order({suite, groups}, suites0)
    end
  end

  defp insert_groups2(_Groups, :all) do
    :all
  end

  defp insert_groups2([group = {gr, cases} | groups], grAndCases) do
    case :lists.keysearch(gr, 1, grAndCases) do
      {:value, {^gr, :all}} ->
        grAndCases

      {:value, {^gr, cases0}} ->
        cases1 = insert_in_order(cases, cases0)

        insert_groups2(
          groups,
          insert_in_order({gr, cases1}, grAndCases)
        )

      false ->
        insert_groups2(
          groups,
          insert_in_order(group, grAndCases)
        )
    end
  end

  defp insert_groups2([], grAndCases) do
    grAndCases
  end

  defp insert_cases(node, dir, suite, cases, tests, false)
       when is_list(cases) do
    append({{node, dir}, [{suite, cases}]}, tests)
  end

  defp insert_cases(node, dir, suite, cases, tests, true)
       when is_list(cases) do
    {tests1, done} =
      :lists.foldr(
        fn
          all = {{n, d}, [{:all, _}]}, {merged, _}
          when n == node and d == dir ->
            {[all | merged], true}

          {{n, d}, suites0}, {merged, _}
          when n == node and d == dir ->
            suites1 = insert_cases1(suite, cases, suites0)
            {[{{n, d}, suites1} | merged], true}

          t, {merged, match} ->
            {[t | merged], match}
        end,
        {[], false},
        tests
      )

    cond do
      tests == [] ->
        [{{node, dir}, insert_cases1(suite, cases, [{suite, []}])}]

      not done ->
        tests ++ [{{node, dir}, [{suite, cases}]}]

      true ->
        tests1
    end
  end

  defp insert_cases(node, dir, suite, case__, tests, mergeTests)
       when is_atom(case__) do
    insert_cases(node, dir, suite, [case__], tests, mergeTests)
  end

  defp insert_cases1(_Suite, _Cases, :all) do
    :all
  end

  defp insert_cases1(suite, cases, suites0) do
    case :lists.keysearch(suite, 1, suites0) do
      {:value, {^suite, :all}} ->
        suites0

      {:value, {^suite, cases0}} ->
        cases1 = insert_in_order(cases, cases0)
        insert_in_order({suite, cases1}, suites0, :replace)

      false ->
        insert_in_order({suite, cases}, suites0)
    end
  end

  defp skip_suites(node, dir, [s | ss], cmt, tests, mergeTests) do
    tests1 = skip_cases(node, dir, s, :all, cmt, tests, mergeTests)
    skip_suites(node, dir, ss, cmt, tests1, mergeTests)
  end

  defp skip_suites(_Node, _Dir, [], _Cmt, tests, _MergeTests) do
    tests
  end

  defp skip_suites(node, dir, s, cmt, tests, mergeTests) do
    skip_suites(node, dir, [s], cmt, tests, mergeTests)
  end

  defp skip_groups(node, dir, suite, group, :all, cmt, tests, mergeTests)
       when is_atom(group) do
    skip_groups(node, dir, suite, [group], :all, cmt, tests, mergeTests)
  end

  defp skip_groups(node, dir, suite, group, cases, cmt, tests, mergeTests)
       when is_atom(group) do
    skip_groups(node, dir, suite, [group], cases, cmt, tests, mergeTests)
  end

  defp skip_groups(node, dir, suite, groups, case__, cmt, tests, mergeTests)
       when is_atom(case__) and case__ !== :all do
    skip_groups(node, dir, suite, groups, [case__], cmt, tests, mergeTests)
  end

  defp skip_groups(node, dir, suite, groups, cases, cmt, tests, false)
       when (cases == :all or is_list(cases)) and is_list(groups) do
    suites1 =
      skip_groups1(
        suite,
        for gr <- groups do
          {gr, cases}
        end,
        cmt,
        []
      )

    append({{node, dir}, suites1}, tests)
  end

  defp skip_groups(node, dir, suite, groups, cases, cmt, tests, true)
       when (cases == :all or is_list(cases)) and is_list(groups) do
    {tests1, done} =
      :lists.foldr(
        fn
          {{n, d}, suites0}, {merged, _}
          when n == node and d == dir ->
            suites1 =
              skip_groups1(
                suite,
                for gr <- groups do
                  {gr, cases}
                end,
                cmt,
                suites0
              )

            {[{{n, d}, suites1} | merged], true}

          t, {merged, match} ->
            {[t | merged], match}
        end,
        {[], false},
        tests
      )

    cond do
      not done ->
        tests ++
          [
            {{node, dir},
             skip_groups1(
               suite,
               for gr <- groups do
                 {gr, cases}
               end,
               cmt,
               []
             )}
          ]

      true ->
        tests1
    end
  end

  defp skip_groups(node, dir, suite, groups, case__, cmt, tests, mergeTests)
       when is_atom(case__) do
    cases =
      cond do
        case__ == :all ->
          :all

        true ->
          [case__]
      end

    skip_groups(node, dir, suite, groups, cases, cmt, tests, mergeTests)
  end

  defp skip_groups1(suite, groups, cmt, suites0) do
    skipGroups =
      :lists.map(
        fn group ->
          {group, {:skip, cmt}}
        end,
        groups
      )

    case :lists.keysearch(suite, 1, suites0) do
      {:value, {^suite, grAndCases0}} ->
        grAndCases1 = grAndCases0 ++ skipGroups
        insert_in_order({suite, grAndCases1}, suites0, :replace)

      false ->
        case suites0 do
          [{:all, _} = all | skips] ->
            [all | skips ++ [{suite, skipGroups}]]

          _ ->
            insert_in_order({suite, skipGroups}, suites0, :replace)
        end
    end
  end

  defp skip_cases(node, dir, suite, cases, cmt, tests, false)
       when is_list(cases) do
    suites1 = skip_cases1(suite, cases, cmt, [])
    append({{node, dir}, suites1}, tests)
  end

  defp skip_cases(node, dir, suite, cases, cmt, tests, true)
       when is_list(cases) do
    {tests1, done} =
      :lists.foldr(
        fn
          {{n, d}, suites0}, {merged, _}
          when n == node and d == dir ->
            suites1 = skip_cases1(suite, cases, cmt, suites0)
            {[{{n, d}, suites1} | merged], true}

          t, {merged, match} ->
            {[t | merged], match}
        end,
        {[], false},
        tests
      )

    cond do
      not done ->
        tests ++ [{{node, dir}, skip_cases1(suite, cases, cmt, [])}]

      true ->
        tests1
    end
  end

  defp skip_cases(node, dir, suite, case__, cmt, tests, mergeTests)
       when is_atom(case__) do
    skip_cases(node, dir, suite, [case__], cmt, tests, mergeTests)
  end

  defp skip_cases1(suite, cases, cmt, suites0) do
    skipCases =
      :lists.map(
        fn c ->
          {c, {:skip, cmt}}
        end,
        cases
      )

    case :lists.keysearch(suite, 1, suites0) do
      {:value, {^suite, cases0}} ->
        cases1 = cases0 ++ skipCases
        insert_in_order({suite, cases1}, suites0, :replace)

      false ->
        case suites0 do
          [{:all, _} = all | skips] ->
            [all | skips ++ [{suite, skipCases}]]

          _ ->
            insert_in_order({suite, skipCases}, suites0, :replace)
        end
    end
  end

  defp append(elem, list) do
    list ++ [elem]
  end

  defp insert_in_order(elems, dest) do
    insert_in_order1(elems, dest, false)
  end

  defp insert_in_order(elems, dest, :replace) do
    insert_in_order1(elems, dest, true)
  end

  defp insert_in_order1([_E | es], :all, replace) do
    insert_in_order1(es, :all, replace)
  end

  defp insert_in_order1([e | es], list, replace) do
    list1 = insert_elem(e, list, [], replace)
    insert_in_order1(es, list1, replace)
  end

  defp insert_in_order1([], list, _Replace) do
    list
  end

  defp insert_in_order1(e, list, replace) do
    insert_elem(e, list, [], replace)
  end

  defp insert_elem({key, _} = e, [{key, _} | rest], soFar, true) do
    :lists.reverse([e | soFar]) ++ rest
  end

  defp insert_elem({e, _}, [e | rest], soFar, true) do
    :lists.reverse([e | soFar]) ++ rest
  end

  defp insert_elem(e, [e | rest], soFar, true) do
    :lists.reverse([e | soFar]) ++ rest
  end

  defp insert_elem({:all, _} = e, _, soFar, _Replace) do
    :lists.reverse([e | soFar])
  end

  defp insert_elem(_E, [:all | _], soFar, _Replace) do
    :lists.reverse(soFar)
  end

  defp insert_elem(_E, [{:all, _}], soFar, _Replace) do
    :lists.reverse(soFar)
  end

  defp insert_elem({key, _} = e, [{key, []} | rest], soFar, _Replace) do
    :lists.reverse([e | soFar]) ++ rest
  end

  defp insert_elem(e, [e1 | rest], soFar, replace) do
    insert_elem(e, rest, [e1 | soFar], replace)
  end

  defp insert_elem(e, [], soFar, _Replace) do
    :lists.reverse([e | soFar])
  end

  defp ref2node(:all_nodes, _Refs) do
    :all_nodes
  end

  defp ref2node(:master, _Refs) do
    :master
  end

  defp ref2node(refOrNode, refs) do
    case :lists.member(
           ?@,
           :erlang.atom_to_list(refOrNode)
         ) do
      false ->
        case :lists.keysearch(refOrNode, 1, refs) do
          {:value, {^refOrNode, node}} ->
            node

          false ->
            throw({:error, {:noderef_missing, refOrNode}})
        end

      true ->
        refOrNode
    end
  end

  defp ref2dir(ref, spec) do
    ref2dir(ref, r_testspec(spec, :alias), spec)
  end

  defp ref2dir(ref, refs, spec) when is_atom(ref) do
    case :lists.keysearch(ref, 1, refs) do
      {:value, {^ref, dir}} ->
        get_absdir(dir, spec)

      false ->
        throw({:error, {:alias_missing, ref}})
    end
  end

  defp ref2dir(dir, _, spec) when is_list(dir) do
    get_absdir(dir, spec)
  end

  defp ref2dir(what, _, _) do
    throw({:error, {:invalid_directory_name, what}})
  end

  defp is_node(what, nodes) when is_atom(what) do
    is_node([what], nodes)
  end

  defp is_node([:master | _], _Nodes) do
    true
  end

  defp is_node(what = {n, h}, nodes)
       when is_atom(n) and
              is_atom(h) do
    is_node([what], nodes)
  end

  defp is_node([what | _], nodes) do
    case :erlang.or(
           :lists.keymember(what, 1, nodes),
           :lists.keymember(what, 2, nodes)
         ) do
      true ->
        true

      false ->
        false
    end
  end

  defp is_node([], _) do
    false
  end

  defp valid_terms() do
    [
      {:set_merge_tests, 2},
      {:define, 3},
      {:specs, 3},
      {:node, 3},
      {:cover, 2},
      {:cover, 3},
      {:cover_stop, 2},
      {:cover_stop, 3},
      {:config, 2},
      {:config, 3},
      {:config, 4},
      {:userconfig, 2},
      {:userconfig, 3},
      {:alias, 3},
      {:merge_tests, 2},
      {:logdir, 2},
      {:logdir, 3},
      {:logopts, 2},
      {:logopts, 3},
      {:basic_html, 2},
      {:basic_html, 3},
      {:esc_chars, 2},
      {:esc_chars, 3},
      {:verbosity, 2},
      {:verbosity, 3},
      {:silent_connections, 2},
      {:silent_connections, 3},
      {:label, 2},
      {:label, 3},
      {:event_handler, 2},
      {:event_handler, 3},
      {:event_handler, 4},
      {:ct_hooks, 2},
      {:ct_hooks, 3},
      {:enable_builtin_hooks, 2},
      {:release_shell, 2},
      {:multiply_timetraps, 2},
      {:multiply_timetraps, 3},
      {:scale_timetraps, 2},
      {:scale_timetraps, 3},
      {:include, 2},
      {:include, 3},
      {:auto_compile, 2},
      {:auto_compile, 3},
      {:abort_if_missing_suites, 2},
      {:abort_if_missing_suites, 3},
      {:stylesheet, 2},
      {:stylesheet, 3},
      {:suites, 3},
      {:suites, 4},
      {:groups, 4},
      {:groups, 5},
      {:groups, 6},
      {:cases, 4},
      {:cases, 5},
      {:skip_suites, 4},
      {:skip_suites, 5},
      {:skip_groups, 5},
      {:skip_groups, 6},
      {:skip_groups, 7},
      {:skip_cases, 5},
      {:skip_cases, 6},
      {:create_priv_dir, 2},
      {:create_priv_dir, 3}
    ]
  end

  defp resembles_ct_term(name, size) when is_atom(name) do
    resembles_ct_term2(:erlang.atom_to_list(name), size)
  end

  defp resembles_ct_term(_Name, _) do
    false
  end

  defp resembles_ct_term2(name, size) when length(name) > 3 do
    cTTerms =
      for {tag, sz} <- valid_terms() do
        {:erlang.atom_to_list(tag), sz}
      end

    compare_names(name, size, cTTerms)
  end

  defp resembles_ct_term2(_, _) do
    false
  end

  defp compare_names(name, size, [{term, sz} | ts]) do
    cond do
      abs(size - sz) > 0 ->
        compare_names(name, size, ts)

      true ->
        diff = abs(length(name) - length(term))

        cond do
          diff > 1 ->
            compare_names(name, size, ts)

          true ->
            common = common_letters(name, term, 0)
            bad = abs(length(name) - common)

            cond do
              bad > 2 ->
                compare_names(name, size, ts)

              true ->
                true
            end
        end
    end
  end

  defp compare_names(_, _, []) do
    false
  end

  defp common_letters(_, [], count) do
    count
  end

  defp common_letters([l | ls], term, count) do
    case :lists.member(l, term) do
      true ->
        term1 = :lists.delete(l, term)
        common_letters(ls, term1, count + 1)

      false ->
        common_letters(ls, term, count)
    end
  end

  defp common_letters([], _, count) do
    count
  end
end
