defmodule :m_reltool_target do
  use Bitwise
  require Record

  Record.defrecord(:r_common, :common,
    sys_debug: :undefined,
    wx_debug: :undefined,
    trap_exit: :undefined
  )

  Record.defrecord(:r_mod, :mod,
    name: :undefined,
    app_name: :undefined,
    incl_cond: :undefined,
    debug_info: :undefined,
    is_app_mod: :undefined,
    is_ebin_mod: :undefined,
    uses_mods: :undefined,
    exists: :undefined,
    status: :ok,
    used_by_mods: [],
    is_pre_included: :undefined,
    is_included: :undefined
  )

  Record.defrecord(:r_app_info, :app_info,
    description: '',
    id: '',
    vsn: '',
    modules: [],
    maxP: :infinity,
    maxT: :infinity,
    registered: [],
    incl_apps: [],
    applications: [],
    env: [],
    mod: :undefined,
    start_phases: :undefined,
    runtime_dependencies: []
  )

  Record.defrecord(:r_regexp, :regexp,
    source: :undefined,
    compiled: :undefined
  )

  Record.defrecord(:r_app, :app,
    name: :undefined,
    is_escript: :undefined,
    use_selected_vsn: :undefined,
    active_dir: :undefined,
    sorted_dirs: :undefined,
    vsn: :undefined,
    label: :undefined,
    info: :undefined,
    mods: :undefined,
    mod_cond: :undefined,
    incl_cond: :undefined,
    debug_info: :undefined,
    app_file: :undefined,
    app_type: :undefined,
    incl_app_filters: :undefined,
    excl_app_filters: :undefined,
    incl_archive_filters: :undefined,
    excl_archive_filters: :undefined,
    archive_opts: :undefined,
    status: :undefined,
    uses_mods: :undefined,
    used_by_mods: :undefined,
    uses_apps: :undefined,
    used_by_apps: :undefined,
    is_pre_included: :undefined,
    is_included: :undefined,
    rels: :undefined
  )

  Record.defrecord(:r_rel_app, :rel_app,
    name: :undefined,
    app_type: :undefined,
    incl_apps: :undefined
  )

  Record.defrecord(:r_rel, :rel,
    name: :undefined,
    vsn: :undefined,
    rel_apps: :undefined,
    load_dot_erlang: true
  )

  Record.defrecord(:r_sys, :sys,
    root_dir: :undefined,
    lib_dirs: :undefined,
    escripts: :undefined,
    mod_cond: :undefined,
    incl_cond: :undefined,
    apps: :undefined,
    boot_rel: :undefined,
    rels: :undefined,
    emu_name: :undefined,
    profile: :undefined,
    excl_lib: :undefined,
    incl_sys_filters: :undefined,
    excl_sys_filters: :undefined,
    incl_app_filters: :undefined,
    excl_app_filters: :undefined,
    incl_archive_filters: :undefined,
    excl_archive_filters: :undefined,
    archive_opts: :undefined,
    relocatable: :undefined,
    rel_app_type: :undefined,
    embedded_app_type: :undefined,
    app_file: :undefined,
    debug_info: :undefined
  )

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

  defp mandatory_modules() do
    [:error_handler]
  end

  defp kernel_processes(kernelApp) do
    [
      {:kernelProcess, :heart, {:heart, :start, []}},
      {:kernelProcess, :logger, {:logger_server, :start_link, []}},
      {:kernelProcess, :application_controller, {:application_controller, :start, [kernelApp]}}
    ]
  end

  def gen_config(sys, inclDefs) do
    {:ok, do_gen_config(sys, inclDefs)}
  end

  defp do_gen_config(
         r_sys(
           root_dir: rootDir,
           lib_dirs: libDirs,
           mod_cond: modCond,
           incl_cond: appCond,
           apps: apps,
           boot_rel: bootRel,
           rels: rels,
           emu_name: emuName,
           profile: profile,
           incl_sys_filters: inclSysFiles,
           excl_sys_filters: exclSysFiles,
           incl_app_filters: inclAppFiles,
           excl_app_filters: exclAppFiles,
           incl_archive_filters: inclArchiveDirs,
           excl_archive_filters: exclArchiveDirs,
           archive_opts: archiveOpts,
           relocatable: relocatable,
           rel_app_type: relAppType,
           embedded_app_type: inclAppType,
           app_file: appFile,
           debug_info: debugInfo
         ),
         inclDefs
       ) do
    ertsItems =
      case :lists.keyfind(:erts, r_app(:name), apps) do
        false ->
          []

        erts ->
          [{:erts, do_gen_config(erts, inclDefs)}]
      end

    appsItems =
      for a <- apps,
          r_app(a, :name) !== :"*MISSING*",
          r_app(a, :name) !== :erts,
          r_app(a, :is_escript) !== true do
        do_gen_config(a, inclDefs)
      end

    escriptItems =
      for a <- apps, r_app(a, :is_escript) do
        {:escript, r_app(a, :active_dir),
         emit(:incl_cond, r_app(a, :incl_cond), :undefined, inclDefs)}
      end

    defaultRels = :reltool_utils.default_rels()

    relsItems =
      for r <- rels do
        do_gen_config(r, inclDefs)
      end

    defaultRelsItems =
      for r <- defaultRels do
        do_gen_config(r, inclDefs)
      end

    relsItems2 =
      case inclDefs do
        true ->
          relsItems

        false ->
          relsItems -- defaultRelsItems
      end

    x = fn list ->
      for r_regexp(source: re) <- list do
        re
      end
    end

    {:sys,
     emit(:root_dir, rootDir, :code.root_dir(), inclDefs) ++
       emit(:lib_dirs, libDirs, [], inclDefs) ++
       escriptItems ++
       emit(
         :mod_cond,
         modCond,
         :all,
         inclDefs
       ) ++
       emit(
         :incl_cond,
         appCond,
         :derived,
         inclDefs
       ) ++
       ertsItems ++
       :lists.flatten(appsItems) ++
       emit(
         :boot_rel,
         bootRel,
         'start_clean',
         inclDefs
       ) ++
       relsItems2 ++
       emit(
         :emu_name,
         emuName,
         'beam',
         inclDefs
       ) ++
       emit(
         :relocatable,
         relocatable,
         true,
         inclDefs
       ) ++
       emit(
         :profile,
         profile,
         :development,
         inclDefs
       ) ++
       emit(
         :incl_sys_filters,
         x.(inclSysFiles),
         :reltool_utils.choose_default(
           :incl_sys_filters,
           profile,
           inclDefs
         ),
         inclDefs
       ) ++
       emit(
         :excl_sys_filters,
         x.(exclSysFiles),
         :reltool_utils.choose_default(
           :excl_sys_filters,
           profile,
           inclDefs
         ),
         inclDefs
       ) ++
       emit(
         :incl_app_filters,
         x.(inclAppFiles),
         :reltool_utils.choose_default(
           :incl_app_filters,
           profile,
           inclDefs
         ),
         inclDefs
       ) ++
       emit(
         :excl_app_filters,
         x.(exclAppFiles),
         :reltool_utils.choose_default(
           :excl_app_filters,
           profile,
           inclDefs
         ),
         inclDefs
       ) ++
       emit(
         :incl_archive_filters,
         x.(inclArchiveDirs),
         ['.*'],
         inclDefs
       ) ++
       emit(
         :excl_archive_filters,
         x.(exclArchiveDirs),
         ['^include$', '^priv$'],
         inclDefs
       ) ++
       emit(
         :archive_opts,
         archiveOpts,
         [],
         inclDefs
       ) ++
       emit(
         :rel_app_type,
         relAppType,
         :permanent,
         inclDefs
       ) ++
       emit(
         :embedded_app_type,
         inclAppType,
         :reltool_utils.choose_default(
           :embedded_app_type,
           profile,
           inclDefs
         ),
         inclDefs
       ) ++
       emit(
         :app_file,
         appFile,
         :keep,
         inclDefs
       ) ++
       emit(
         :debug_info,
         debugInfo,
         :keep,
         inclDefs
       )}
  end

  defp do_gen_config(
         r_app(
           name: name,
           mod_cond: modCond,
           incl_cond: appCond,
           debug_info: debugInfo,
           app_file: appFile,
           incl_app_filters: inclAppFiles,
           excl_app_filters: exclAppFiles,
           incl_archive_filters: inclArchiveDirs,
           excl_archive_filters: exclArchiveDirs,
           archive_opts: archiveOpts,
           use_selected_vsn: useSelected,
           vsn: vsn,
           active_dir: activeDir,
           mods: mods,
           is_included: isIncl
         ),
         inclDefs
       ) do
    appConfig = [
      emit(:mod_cond, modCond, :undefined, inclDefs),
      emit(:incl_cond, appCond, :undefined, inclDefs),
      emit(:debug_info, debugInfo, :undefined, inclDefs),
      emit(:app_file, appFile, :undefined, inclDefs),
      emit(:incl_app_filters, inclAppFiles, :undefined, inclDefs),
      emit(:excl_app_filters, exclAppFiles, :undefined, inclDefs),
      emit(:incl_archive_filters, inclArchiveDirs, :undefined, inclDefs),
      emit(:excl_archive_filters, exclArchiveDirs, :undefined, inclDefs),
      emit(:archive_opts, archiveOpts, :undefined, inclDefs),
      cond do
        isIncl and inclDefs ->
          [{:vsn, vsn}, {:lib_dir, activeDir}]

        useSelected === :vsn ->
          [{:vsn, vsn}]

        useSelected === :dir ->
          [{:lib_dir, activeDir}]

        true ->
          []
      end,
      for m <- mods do
        do_gen_config(m, inclDefs)
      end
    ]

    case :lists.flatten(appConfig) do
      flatAppConfig when flatAppConfig !== [] or isIncl ->
        [{:app, name, flatAppConfig}]

      [] ->
        []
    end
  end

  defp do_gen_config(
         r_mod(name: name, incl_cond: appCond, debug_info: debugInfo, is_included: isIncl),
         inclDefs
       ) do
    modConfig = [
      emit(:incl_cond, appCond, :undefined, inclDefs),
      emit(:debug_info, debugInfo, :undefined, inclDefs)
    ]

    case :lists.flatten(modConfig) do
      flatModConfig when flatModConfig !== [] or isIncl ->
        [{:mod, name, flatModConfig}]

      _ ->
        []
    end
  end

  defp do_gen_config(
         r_rel(name: name, vsn: vsn, rel_apps: relApps, load_dot_erlang: loadDotErlang),
         inclDefs
       ) do
    relAppsConfig =
      for rA <- relApps do
        do_gen_config(rA, inclDefs)
      end

    cond do
      loadDotErlang === false ->
        {:rel, name, vsn, relAppsConfig, [{:load_dot_erlang, false}]}

      inclDefs === true ->
        {:rel, name, vsn, relAppsConfig, [{:load_dot_erlang, true}]}

      loadDotErlang === true ->
        {:rel, name, vsn, relAppsConfig}
    end
  end

  defp do_gen_config(
         r_rel_app(name: name, app_type: type, incl_apps: inclApps),
         _InclDefs
       ) do
    case {type, inclApps} do
      {:undefined, :undefined} ->
        name

      {:undefined, _} ->
        {name, inclApps}

      {_, :undefined} ->
        {name, type}

      {_, _} ->
        {name, type, inclApps}
    end
  end

  defp do_gen_config({tag, val}, inclDefs) do
    emit(tag, val, :undefined, inclDefs)
  end

  defp do_gen_config([], _InclDefs) do
    []
  end

  defp do_gen_config([h | t], inclDefs) do
    :lists.flatten([do_gen_config(h, inclDefs), do_gen_config(t, inclDefs)])
  end

  defp emit(tag, val, default, inclDefs) do
    cond do
      val == :undefined ->
        []

      inclDefs ->
        [{tag, val}]

      val !== default ->
        [{tag, val}]

      true ->
        []
    end
  end

  def gen_app(
        r_app(
          name: name,
          info:
            r_app_info(
              description: desc,
              id: id,
              vsn: vsn,
              modules: mods,
              maxP: maxP,
              maxT: maxT,
              registered: regs,
              incl_apps: inclApps,
              applications: reqApps,
              env: env,
              mod: startMod,
              start_phases: startPhases
            )
        )
      ) do
    startPhases2 =
      case startPhases do
        :undefined ->
          []

        _ ->
          [{:start_phases, startPhases}]
      end

    tail =
      case startMod do
        :undefined ->
          startPhases2

        _ ->
          [{:mod, startMod} | startPhases2]
      end

    {:application, name,
     [
       [
         {:description, desc},
         {:vsn, vsn},
         {:id, id},
         {:modules, mods},
         {:registered, regs},
         {:applications, reqApps},
         {:included_applications, inclApps},
         {:env, env},
         {:maxT, maxT},
         {:maxP, maxP}
       ]
       | tail
     ]}
  end

  def gen_rel(rel, sys) do
    try do
      mergedApps = merge_apps(rel, sys)
      {:ok, do_gen_rel(rel, sys, mergedApps)}
    catch
      {:error, text} ->
        {:error, text}
    end
  end

  defp do_gen_rel(
         r_rel(name: relName, vsn: relVsn, rel_apps: relApps),
         r_sys(apps: apps),
         mergedApps
       ) do
    ertsName = :erts

    case :lists.keysearch(ertsName, r_app(:name), apps) do
      {:value, erts} ->
        {:release, {relName, relVsn}, {ertsName, r_app(erts, :vsn)},
         for app <- mergedApps do
           strip_rel_info(app, relApps)
         end}

      false ->
        :reltool_utils.throw_error('Mandatory application ~w is not included', [ertsName])
    end
  end

  defp strip_rel_info(
         r_app(name: name, vsn: vsn, app_type: type, info: r_app_info(incl_apps: appInclApps)),
         relApps
       )
       when type !== :undefined do
    relInclApps =
      case :lists.keyfind(name, r_rel_app(:name), relApps) do
        r_rel_app(incl_apps: rIA) when rIA !== :undefined ->
          rIA

        _ ->
          :undefined
      end

    case {type, relInclApps} do
      {:permanent, :undefined} ->
        {name, vsn}

      {:permanent, _} ->
        {name, vsn, appInclApps}

      {_, :undefined} ->
        {name, vsn, type}

      {_, _} ->
        {name, vsn, type, appInclApps}
    end
  end

  defp merge_apps(
         r_rel(name: relName, rel_apps: relApps),
         r_sys(apps: apps, rel_app_type: relAppType, embedded_app_type: embAppType)
       ) do
    mandatory = [:kernel, :stdlib]
    mergedApps = do_merge_apps(relName, mandatory, apps, :permanent, [])
    mergedApps2 = do_merge_apps(relName, relApps, apps, relAppType, mergedApps)

    embedded =
      for a <- apps,
          embAppType !== :undefined,
          r_app(a, :is_included),
          r_app(a, :name) !== :erts,
          r_app(a, :name) !== :"*MISSING*",
          not :lists.keymember(r_app(a, :name), r_app(:name), mergedApps2) do
        r_app(a, :name)
      end

    mergedApps3 = do_merge_apps(relName, embedded, apps, embAppType, mergedApps2)
    revMerged = :lists.reverse(mergedApps3)

    mergedSortedUsedAndIncs =
      sort_used_and_incl_apps(
        revMerged,
        revMerged
      )

    sort_apps(mergedSortedUsedAndIncs)
  end

  defp do_merge_apps(relName, [r_rel_app(name: name) = rA | relApps], apps, relAppType, acc) do
    case is_already_merged(name, relApps, acc) do
      true ->
        do_merge_apps(relName, relApps, apps, relAppType, acc)

      false ->
        {:value, app} = :lists.keysearch(name, r_app(:name), apps)
        mergedApp = merge_app(relName, rA, relAppType, app)
        reqNames = r_app_info(r_app(mergedApp, :info), :applications)
        incNames = r_app_info(r_app(mergedApp, :info), :incl_apps)
        acc2 = [mergedApp | acc]
        do_merge_apps(relName, reqNames ++ incNames ++ relApps, apps, relAppType, acc2)
    end
  end

  defp do_merge_apps(relName, [name | relApps], apps, relAppType, acc) do
    case is_already_merged(name, relApps, acc) do
      true ->
        do_merge_apps(relName, relApps, apps, relAppType, acc)

      false ->
        relApp = r_rel_app(name: name)
        do_merge_apps(relName, [relApp | relApps], apps, relAppType, acc)
    end
  end

  defp do_merge_apps(_RelName, [], _Apps, _RelAppType, acc) do
    acc
  end

  defp merge_app(
         relName,
         r_rel_app(name: name, app_type: type, incl_apps: inclApps0),
         relAppType,
         app
       ) do
    type2 =
      case {type, r_app(app, :app_type)} do
        {:undefined, :undefined} ->
          relAppType

        {:undefined, appAppType} ->
          appAppType

        {_, _} ->
          type
      end

    info = r_app(app, :info)

    inclApps =
      case inclApps0 do
        :undefined ->
          r_app_info(info, :incl_apps)

        _ ->
          inclApps0
      end

    case inclApps -- r_app_info(info, :incl_apps) do
      [] ->
        r_app(app,
          app_type: type2,
          info: r_app_info(info, incl_apps: inclApps)
        )

      badIncl ->
        :reltool_utils.throw_error(
          '~w: These applications are used by release ~ts but are missing as included_applications in the app file: ~p',
          [name, relName, badIncl]
        )
    end
  end

  defp is_already_merged(name, [name | _], _MergedApps) do
    true
  end

  defp is_already_merged(name, [r_rel_app(name: name) | _], _MergedApps) do
    true
  end

  defp is_already_merged(name, [_ | relApps], mergedApps) do
    is_already_merged(name, relApps, mergedApps)
  end

  defp is_already_merged(name, [], [r_app(name: name) | _MergedApps]) do
    true
  end

  defp is_already_merged(name, [] = relApps, [_ | mergedApps]) do
    is_already_merged(name, relApps, mergedApps)
  end

  defp is_already_merged(_Name, [], []) do
    false
  end

  def gen_boot({:script, {_, _}, _} = script) do
    {:ok, :erlang.term_to_binary(script)}
  end

  def gen_script(rel, sys, pathFlag, variables) do
    try do
      mergedApps = merge_apps(rel, sys)
      do_gen_script(rel, sys, mergedApps, pathFlag, variables)
    catch
      {:error, text} ->
        {:error, text}
    end
  end

  defp do_gen_script(
         r_rel(name: relName, vsn: relVsn, load_dot_erlang: loadErlangRc),
         r_sys(apps: apps),
         mergedApps,
         pathFlag,
         variables
       ) do
    {:value, erts} = :lists.keysearch(:erts, r_app(:name), apps)

    preloaded =
      for mod <- r_app(erts, :mods) do
        r_mod(mod, :name)
      end

    mandatory = mandatory_modules()
    early = mandatory ++ preloaded
    {:value, kernelApp} = :lists.keysearch(:kernel, r_app(:name), mergedApps)

    inclApps =
      :lists.flatmap(
        fn r_app(info: r_app_info(incl_apps: i)) ->
          i
        end,
        mergedApps
      )

    deepList = [
      {:preLoaded, :lists.sort(preloaded)},
      {:progress, :preloaded},
      {:path, create_mandatory_path(mergedApps, pathFlag, variables)},
      {:primLoad, :lists.sort(mandatory)},
      {:kernel_load_completed},
      {:progress, :kernel_load_completed},
      for a <- mergedApps do
        load_app_mods(a, early, pathFlag, variables)
      end,
      {:progress, :modules_loaded},
      {:path, create_path(mergedApps, pathFlag, variables)},
      kernel_processes(gen_app(kernelApp)),
      {:progress, :init_kernel_started},
      for a = r_app(name: name, app_type: type) <- mergedApps, name !== :kernel, type !== :none do
        {:apply, {:application, :load, [gen_app(a)]}}
      end,
      {:progress, :applications_loaded},
      for r_app(name: name, app_type: type) <- mergedApps,
          type !== :none,
          type !== :load,
          not :lists.member(name, inclApps) do
        {:apply, {:application, :start_boot, [name, type]}}
      end,
      case loadErlangRc do
        true ->
          {:apply, {:c, :erlangrc, []}}

        false ->
          []
      end,
      {:progress, :started}
    ]

    {:ok, {:script, {relName, relVsn}, :lists.flatten(deepList)}}
  end

  defp load_app_mods(r_app(mods: mods0) = app, mand, pathFlag, variables) do
    path = cr_path(app, pathFlag, variables)

    mods =
      for r_mod(name: m, is_included: true) <- mods0,
          not :lists.member(m, mand) do
        m
      end

    [{:path, [:filename.join([path])]}, {:primLoad, :lists.sort(mods)}]
  end

  defp sort_used_and_incl_apps([r_app(info: info) = app | apps], orderedApps) do
    incls2 =
      case r_app_info(info, :incl_apps) do
        incls when length(incls) > 1 ->
          sort_appl_list(incls, orderedApps)

        incls ->
          incls
      end

    uses2 =
      case r_app_info(info, :applications) do
        uses when length(uses) > 1 ->
          sort_appl_list(uses, orderedApps)

        uses ->
          uses
      end

    app2 =
      r_app(app,
        info:
          r_app_info(info,
            incl_apps: incls2,
            applications: uses2
          )
      )

    [app2 | sort_used_and_incl_apps(apps, orderedApps)]
  end

  defp sort_used_and_incl_apps([], _OrderedApps) do
    []
  end

  defp sort_appl_list(list, order) do
    indexedList = find_pos(list, order)
    sortedIndexedList = :lists.keysort(1, indexedList)

    :lists.map(
      fn {_Index, name} ->
        name
      end,
      sortedIndexedList
    )
  end

  defp find_pos([name | incs], orderedApps) do
    [
      find_pos(1, name, orderedApps)
      | find_pos(
          incs,
          orderedApps
        )
    ]
  end

  defp find_pos([], _OrderedApps) do
    []
  end

  defp find_pos(n, name, [r_app(name: name) | _OrderedApps]) do
    {n, name}
  end

  defp find_pos(n, name, [_OtherAppl | orderedApps]) do
    find_pos(n + 1, name, orderedApps)
  end

  defp sort_apps(apps) do
    sort_apps(apps, [], [], [])
  end

  defp sort_apps([r_app(name: name, info: info) = app | apps], missing, circular, visited) do
    {uses, apps1, notFnd1} =
      find_all(name, :lists.reverse(r_app_info(info, :applications)), apps, visited, [], [])

    {incs, apps2, notFnd2} =
      find_all(name, :lists.reverse(r_app_info(info, :incl_apps)), apps1, visited, [], [])

    missing1 = notFnd1 ++ notFnd2 ++ missing

    case uses ++ incs do
      [] ->
        [app | sort_apps(apps, missing1, circular, [name | visited])]

      l ->
        newCircular =
          for r_app(name: n) <- l, n2 <- visited, n === n2 do
            n
          end

        circular1 =
          case newCircular do
            [] ->
              circular

            _ ->
              [name | newCircular] ++ circular
          end

        apps3 = del_apps(newCircular, l ++ [app | apps2])
        sort_apps(apps3, missing1, circular1, [name | visited])
    end
  end

  defp sort_apps([], [], [], _) do
    []
  end

  defp sort_apps([], missing, [], _) do
    :reltool_utils.throw_error('Undefined applications: ~p', [make_set(missing)])
  end

  defp sort_apps([], [], circular, _) do
    :reltool_utils.throw_error('Circular dependencies: ~p', [make_set(circular)])
  end

  defp sort_apps([], missing, circular, _) do
    :reltool_utils.throw_error(
      'Circular dependencies: ~pUndefined applications: ~p\n',
      [make_set(circular), make_set(missing)]
    )
  end

  defp find_all(checkingApp, [name | names], apps, visited, found, notFound) do
    case :lists.keyfind(name, r_app(:name), apps) do
      r_app(info: info) = app ->
        case :lists.member(
               checkingApp,
               r_app_info(info, :incl_apps)
             ) do
          true ->
            case :lists.member(name, visited) do
              true ->
                find_all(checkingApp, names, apps, visited, found, notFound)

              false ->
                find_all(checkingApp, names, apps, visited, found, [name | notFound])
            end

          false ->
            find_all(checkingApp, names, apps -- [app], visited, [app | found], notFound)
        end

      false ->
        case :lists.member(name, visited) do
          true ->
            find_all(checkingApp, names, apps, visited, found, notFound)

          false ->
            find_all(checkingApp, names, apps, visited, found, [name | notFound])
        end
    end
  end

  defp find_all(_CheckingApp, [], apps, _Visited, found, notFound) do
    {found, apps, notFound}
  end

  defp del_apps([name | names], apps) do
    del_apps(names, :lists.keydelete(name, r_app(:name), apps))
  end

  defp del_apps([], apps) do
    apps
  end

  defp create_path(apps, pathFlag, variables) do
    make_set(
      for app <- apps do
        cr_path(app, pathFlag, variables)
      end
    )
  end

  defp cr_path(r_app(label: label), true, []) do
    :filename.join(['$ROOT', 'lib', label, 'ebin'])
  end

  defp cr_path(r_app(name: name, vsn: vsn, label: label, active_dir: dir), true, variables) do
    tail = [label, 'ebin']

    case variable_dir(dir, :erlang.atom_to_list(name), vsn, variables) do
      {:ok, varDir} ->
        :filename.join([varDir] ++ tail)

      _ ->
        :filename.join(['$ROOT', 'lib'] ++ tail)
    end
  end

  defp cr_path(r_app(name: name), :otp_build, _) do
    :filename.join(['$ROOT', 'lib', :erlang.atom_to_list(name), 'ebin'])
  end

  defp cr_path(r_app(active_dir: dir), _, _) do
    :filename.join([dir, 'ebin'])
  end

  defp variable_dir(dir, name, vsn, [{var, path} | variables]) do
    case :lists.prefix(path, dir) do
      true ->
        d0 = strip_prefix(path, dir)

        case strip_name_ebin(d0, name, vsn) do
          {:ok, d} ->
            {:ok, :filename.join(['$' ++ var] ++ d)}

          _ ->
            {:ok, :filename.join(['$' ++ var] ++ d0)}
        end

      false ->
        variable_dir(dir, name, vsn, variables)
    end
  end

  defp variable_dir(_Dir, _, _, []) do
    false
  end

  defp strip_prefix(path, dir) do
    l = length(:filename.split(path))
    :lists.nthtail(l, :filename.split(dir))
  end

  defp strip_name_ebin(dir, name, vsn) do
    fullName = name ++ '-' ++ vsn

    case :lists.reverse(dir) do
      [['ebin', ^name] | d] ->
        {:ok, :lists.reverse(d)}

      [['ebin', ^fullName] | d] ->
        {:ok, :lists.reverse(d)}

      [^name | d] ->
        {:ok, :lists.reverse(d)}

      [^fullName | d] ->
        {:ok, :lists.reverse(d)}

      _ ->
        false
    end
  end

  defp create_mandatory_path(apps, pathFlag, variables) do
    mandatory = [:kernel, :stdlib]

    make_set(
      :lists.map(
        fn r_app(name: name) = app ->
          case :lists.member(name, mandatory) do
            true ->
              cr_path(app, pathFlag, variables)

            false ->
              ''
          end
        end,
        apps
      )
    )
  end

  defp make_set([]) do
    []
  end

  defp make_set(['' | t]) do
    make_set(t)
  end

  defp make_set([h | t]) do
    [
      h
      | for y <- make_set(t), y !== h do
          y
        end
    ]
  end

  def gen_rel_files(sys, targetDir) do
    try do
      spec = spec_rel_files(sys)
      eval_spec(spec, r_sys(sys, :root_dir), targetDir)
    catch
      {:error, text} ->
        {:error, text}
    end
  end

  defp spec_rel_files(r_sys(rels: rels) = sys) do
    :lists.append(
      for r <- rels do
        do_spec_rel_files(r, sys)
      end
    )
  end

  defp do_spec_rel_files(r_rel(name: relName) = rel, sys) do
    relFile = relName ++ '.rel'
    scriptFile = relName ++ '.script'
    bootFile = relName ++ '.boot'
    mergedApps = merge_apps(rel, sys)
    genRel = do_gen_rel(rel, sys, mergedApps)

    variables =
      case r_sys(sys, :excl_lib) do
        :otp_root ->
          for libDir <- r_sys(sys, :lib_dirs) do
            {'RELTOOL_EXT_LIB', libDir}
          end ++
            for r_app(
                  active_dir: appLibDir,
                  use_selected_vsn: :dir
                ) <- mergedApps do
              {'RELTOOL_EXT_LIB', :filename.dirname(appLibDir)}
            end

        _ ->
          []
      end

    pathFlag = true
    {:ok, script} = do_gen_script(rel, sys, mergedApps, pathFlag, variables)
    {:ok, bootBin} = gen_boot(script)
    date = :erlang.date()
    time = :erlang.time()
    relIoList = :io_lib.format('%% rel generated at ~w ~w\n~tp.\n\n', [date, time, genRel])
    scriptIoList = :io_lib.format('%% script generated at ~w ~w\n~tp.\n\n', [date, time, script])

    [
      {:write_file, relFile, to_utf8_bin_with_enc_comment(relIoList)},
      {:write_file, scriptFile, to_utf8_bin_with_enc_comment(scriptIoList)},
      {:write_file, bootFile, bootBin}
    ]
  end

  defp to_utf8_bin_with_enc_comment(ioList) when is_list(ioList) do
    :unicode.characters_to_binary('%% ' ++ :epp.encoding_to_string(:utf8) ++ '\n' ++ ioList)
  end

  def gen_target(sys, targetDir) do
    try do
      spec = do_gen_spec(sys)
      eval_spec(spec, r_sys(sys, :root_dir), targetDir)
    catch
      {:error, text} ->
        {:error, text}
    end
  end

  def gen_spec(sys) do
    try do
      {:ok, do_gen_spec(sys)}
    catch
      {:error, text} ->
        {:error, text}
    end
  end

  defp do_gen_spec(
         r_sys(
           root_dir: rootDir,
           excl_lib: exclLib,
           incl_sys_filters: inclRegexps,
           excl_sys_filters: exclRegexps,
           relocatable: relocatable,
           apps: apps
         ) = sys
       ) do
    relFiles = spec_rel_files(sys)

    {sysFiles, inclRegexps2, exclRegexps2, mandatory} =
      case exclLib do
        :otp_root ->
          {[], inclRegexps, exclRegexps, ['lib']}

        _ ->
          {:create_dir, _, sF} = spec_dir(rootDir)
          {eR2, sF2} = strip_sys_files(relocatable, sF, apps, exclRegexps)
          {iR2, binFiles} = spec_bin_files(sys, sF, sF2, relFiles, inclRegexps)
          sF3 = [{:create_dir, 'bin', binFiles}] ++ sF2
          {sF3, iR2, eR2, ['bin', 'erts', 'lib']}
      end

    libFiles = spec_lib_files(sys)
    {bootVsn, startFile} = spec_start_file(sys)

    sysFiles2 =
      [{:create_dir, 'releases', [startFile, {:create_dir, bootVsn, relFiles}]}] ++ sysFiles

    sysFiles3 = filter_spec(sysFiles2, inclRegexps2, exclRegexps2)
    sysFiles4 = sysFiles3 ++ [{:create_dir, 'lib', libFiles}]
    check_sys(mandatory, sysFiles4)
    sysFiles4
  end

  defp strip_sys_files(relocatable, sysFiles, apps, exclRegexps) do
    exclRegexps2 =
      case relocatable do
        true ->
          extraExcl = ['^erts.*/bin/.*src$']

          :reltool_utils.decode_regexps(
            :excl_sys_filters,
            {:add, extraExcl},
            exclRegexps
          )

        false ->
          exclRegexps
      end

    {:value, erts} = :lists.keysearch(:erts, r_app(:name), apps)

    filterErts = fn spec ->
      file = :erlang.element(2, spec)

      case file do
        'erts' ->
          :reltool_utils.throw_error(
            'This system is not installed. The directory ~ts is missing.',
            [r_app(erts, :label)]
          )

        _ when file === r_app(erts, :label) ->
          replace_dyn_erl(relocatable, spec)

        'erts-' ++ _ ->
          false

        _ ->
          true
      end
    end

    sysFiles2 = :lists.zf(filterErts, sysFiles)

    sysFiles3 =
      :lists.foldl(
        fn f, acc ->
          :lists.keydelete(f, 2, acc)
        end,
        sysFiles2,
        ['releases', 'lib', 'bin']
      )

    {exclRegexps2, sysFiles3}
  end

  defp replace_dyn_erl(false, _ErtsSpec) do
    true
  end

  defp replace_dyn_erl(true, {:create_dir, ertsDir, ertsFiles}) do
    [{:create_dir, _, binFiles}] =
      safe_lookup_spec(
        'bin',
        ertsFiles
      )

    case lookup_spec('dyn_erl', binFiles) do
      [] ->
        case lookup_spec('erl.ini', binFiles) do
          [] ->
            true

          [{:copy_file, erlIni}] ->
            binFiles2 = :lists.keydelete(erlIni, 2, binFiles)
            ertsFiles2 = :lists.keyreplace('bin', 2, ertsFiles, {:create_dir, 'bin', binFiles2})
            {true, {:create_dir, ertsDir, ertsFiles2}}
        end

      [{:copy_file, dynErlExe}] ->
        erlExe = 'erl' ++ :filename.extension(dynErlExe)
        binFiles2 = :lists.keydelete(dynErlExe, 2, binFiles)
        dynErlExe2 = :filename.join([ertsDir, 'bin', dynErlExe])
        binFiles3 = :lists.keyreplace(erlExe, 2, binFiles2, {:copy_file, erlExe, dynErlExe2})
        ertsFiles2 = :lists.keyreplace('bin', 2, ertsFiles, {:create_dir, 'bin', binFiles3})
        {true, {:create_dir, ertsDir, ertsFiles2}}
    end
  end

  defp spec_bin_files(sys, allSysFiles, strippedSysFiles, relFiles, inclRegexps) do
    [{:create_dir, ertsLabel, ertsFiles}] = safe_lookup_spec('erts', strippedSysFiles)

    [{:create_dir, _, binFiles}] =
      safe_lookup_spec(
        'bin',
        ertsFiles
      )

    ertsBin = :filename.join([ertsLabel, 'bin'])
    escripts = spec_escripts(sys, ertsBin, binFiles)

    map = fn
      {:copy_file, file} ->
        {:copy_file, file, :filename.join([ertsBin, file])}

      {:copy_file, newFile, oldFile} ->
        {_, oldFile2} =
          abs_to_rel_path(
            ertsBin,
            :filename.join([ertsBin, oldFile])
          )

        {:copy_file, newFile, oldFile2}
    end

    [{:create_dir, _, oldBinFiles}] =
      safe_lookup_spec(
        'bin',
        allSysFiles
      )

    goodNames =
      for {:copy_file, f} <- oldBinFiles,
          not :lists.suffix('.boot', f),
          not :lists.suffix('.script', f) do
        f
      end

    binFiles2 =
      for s <- binFiles,
          :lists.member(:erlang.element(2, s), goodNames) do
        map.(s)
      end

    bootFiles =
      for f <- relFiles,
          :lists.suffix('.boot', :erlang.element(2, f)) do
        f
      end

    [{:write_file, _, bootRel}] =
      safe_lookup_spec(
        r_sys(sys, :boot_rel) ++ '.boot',
        bootFiles
      )

    bootFiles2 = :lists.keystore('start.boot', 2, bootFiles, {:write_file, 'start.boot', bootRel})

    makeRegexp = fn file ->
      '^bin/' ++ :erlang.element(2, file) ++ '(|.escript)$'
    end

    extraIncl = :lists.map(makeRegexp, escripts)

    inclRegexps2 =
      :reltool_utils.decode_regexps(
        :incl_sys_filters,
        {:add, extraIncl},
        inclRegexps
      )

    {inclRegexps2, escripts ++ binFiles2 ++ bootFiles2}
  end

  defp spec_escripts(r_sys(apps: apps), ertsBin, binFiles) do
    filter = fn r_app(
                  is_escript: isEscript,
                  is_included: isIncl,
                  is_pre_included: isPre,
                  name: name,
                  active_dir: file
                ) ->
      cond do
        name === :"*MISSING*" ->
          false

        isEscript !== true ->
          false

        isIncl or isPre ->
          {true, do_spec_escript(file, ertsBin, binFiles)}

        true ->
          false
      end
    end

    :lists.flatten(:lists.zf(filter, apps))
  end

  defp do_spec_escript(file, ertsBin, binFiles) do
    [{:copy_file, escriptExe}] =
      safe_lookup_spec(
        'escript',
        binFiles
      )

    escriptExt = '.escript'
    base = :filename.basename(file, escriptExt)
    exeExt = :filename.extension(escriptExe)

    [
      {:copy_file, base ++ escriptExt, file},
      {:copy_file, base ++ exeExt, :filename.join([ertsBin, escriptExe])}
    ]
  end

  defp check_sys(mandatory, sysFiles) do
    :lists.foreach(
      fn m ->
        do_check_sys(m, sysFiles)
      end,
      mandatory
    )
  end

  defp do_check_sys(prefix, specs) do
    case lookup_spec(prefix, specs) do
      [] ->
        :reltool_utils.throw_error('Mandatory system directory ~ts is not included', [prefix])

      _ ->
        :ok
    end
  end

  defp spec_start_file(r_sys(boot_rel: bootRelName, rels: rels, apps: apps)) do
    {:value, erts} = :lists.keysearch(:erts, r_app(:name), apps)
    {:value, bootRel} = :lists.keysearch(bootRelName, r_rel(:name), rels)
    data = r_app(erts, :vsn) ++ ' ' ++ r_rel(bootRel, :vsn) ++ '\n'
    {r_rel(bootRel, :vsn), {:write_file, 'start_erl.data', :unicode.characters_to_binary(data)}}
  end

  defp lookup_spec(prefix, specs) do
    :lists.filter(
      fn s ->
        :lists.prefix(prefix, :erlang.element(2, s))
      end,
      specs
    )
  end

  defp safe_lookup_spec(prefix, specs) do
    case lookup_spec(prefix, specs) do
      [] ->
        :reltool_utils.throw_error('Mandatory system file ~ts is not included', [prefix])

      match ->
        match
    end
  end

  defp spec_lib_files(r_sys(root_dir: rootDir, apps: apps, excl_lib: exclLib) = sys) do
    filter = fn r_app(
                  is_escript: isEscript,
                  is_included: isIncl,
                  is_pre_included: isPre,
                  name: name,
                  active_dir: activeDir
                ) ->
      cond do
        name === :"*MISSING*" ->
          false

        isEscript !== false ->
          false

        isIncl or isPre ->
          case exclLib do
            :otp_root ->
              not :lists.prefix(rootDir, activeDir)

            _ ->
              true
          end

        true ->
          false
      end
    end

    selectedApps = :lists.filter(filter, apps)

    case exclLib do
      :otp_root ->
        :ok

      _ ->
        check_apps([:kernel, :stdlib], selectedApps)
    end

    :lists.flatten(
      for app <- selectedApps do
        spec_app(app, sys)
      end
    )
  end

  defp check_apps([mandatory | names], apps) do
    case :lists.keymember(mandatory, r_app(:name), apps) do
      false ->
        :reltool_utils.throw_error('Mandatory application ~w is not included in ~p', [
          mandatory,
          apps
        ])

      true ->
        check_apps(names, apps)
    end
  end

  defp check_apps([], _) do
    :ok
  end

  defp spec_app(
         r_app(
           name: name,
           mods: mods,
           active_dir: sourceDir,
           incl_app_filters: appInclRegexps,
           excl_app_filters: appExclRegexps
         ) = app,
         r_sys(
           incl_app_filters: sysInclRegexps,
           excl_app_filters: sysExclRegexps,
           debug_info: sysDebugInfo
         ) = sys
       ) do
    {:create_dir, _, appFiles} = spec_dir(sourceDir)
    appUpFilename = :erlang.atom_to_list(name) ++ '.appup'
    ebinDir = :filename.join([sourceDir, 'ebin'])

    optAppUpFileSpec =
      spec_opt_copy_file(
        ebinDir,
        appUpFilename
      )

    optAppFileSpec = spec_app_file(app, sys, ebinDir)

    modSpecs =
      for m <- mods, r_mod(m, :is_included), r_mod(m, :exists) do
        spec_mod(m, sysDebugInfo)
      end

    newEbin = {:create_dir, 'ebin', optAppUpFileSpec ++ optAppFileSpec ++ modSpecs}
    appFiles2 = :lists.keystore('ebin', 2, appFiles, newEbin)

    inclRegexps =
      :reltool_utils.default_val(
        appInclRegexps,
        sysInclRegexps
      )

    exclRegexps =
      :reltool_utils.default_val(
        appExclRegexps,
        sysExclRegexps
      )

    appFiles3 = filter_spec(appFiles2, inclRegexps, exclRegexps)
    spec_archive(app, sys, appFiles3)
  end

  defp spec_archive(
         r_app(
           label: label,
           active_dir: sourceDir,
           incl_archive_filters: appInclArchiveDirs,
           excl_archive_filters: appExclArchiveDirs,
           archive_opts: appArchiveOpts
         ),
         r_sys(
           root_dir: rootDir,
           incl_archive_filters: sysInclArchiveDirs,
           excl_archive_filters: sysExclArchiveDirs,
           archive_opts: sysArchiveOpts
         ),
         files
       ) do
    inclArchiveDirs =
      :reltool_utils.default_val(
        appInclArchiveDirs,
        sysInclArchiveDirs
      )

    exclArchiveDirs =
      :reltool_utils.default_val(
        appExclArchiveDirs,
        sysExclArchiveDirs
      )

    archiveOpts =
      :reltool_utils.default_val(
        appArchiveOpts,
        sysArchiveOpts
      )

    match = fn f ->
      match(:erlang.element(2, f), inclArchiveDirs, exclArchiveDirs)
    end

    case :lists.filter(match, files) do
      [] ->
        [spec_create_dir(rootDir, sourceDir, label, files)]

      archiveFiles ->
        optDir =
          case files -- archiveFiles do
            [] ->
              []

            externalFiles ->
              [spec_create_dir(rootDir, sourceDir, label, externalFiles)]
          end

        ^archiveOpts =
          :reltool_utils.default_val(
            appArchiveOpts,
            sysArchiveOpts
          )

        archiveDir = spec_create_dir(rootDir, sourceDir, label, archiveFiles)

        [
          {:archive, label ++ '.ez', archiveOpts, [archiveDir]}
          | optDir
        ]
    end
  end

  defp spec_dir(dir) do
    base = :filename.basename(dir)

    case :erl_prim_loader.read_file_info(dir) do
      {:ok, r_file_info(type: :directory)} ->
        case :erl_prim_loader.list_dir(dir) do
          {:ok, files} ->
            {:create_dir, base,
             for f <- files do
               spec_dir(:filename.join([dir, f]))
             end}

          :error ->
            :reltool_utils.throw_error('list dir ~ts failed', [dir])
        end

      {:ok, r_file_info(type: :regular)} ->
        {:copy_file, base}

      _ ->
        :reltool_utils.throw_error('read file info ~ts failed', [dir])
    end
  end

  defp spec_mod(mod, debugInfo) do
    file = :erlang.atom_to_list(r_mod(mod, :name)) ++ :code.objfile_extension()

    case :reltool_utils.default_val(
           r_mod(mod, :debug_info),
           debugInfo
         ) do
      :keep ->
        {:copy_file, file}

      :strip ->
        {:strip_beam, file}
    end
  end

  defp spec_app_file(
         r_app(name: name, info: info, mods: mods, app_file: appFile) = app,
         r_sys(app_file: sysAppFile),
         ebinDir
       ) do
    appFilename = :erlang.atom_to_list(name) ++ '.app'

    case :reltool_utils.default_val(
           appFile,
           sysAppFile
         ) do
      :keep ->
        spec_opt_copy_file(ebinDir, appFilename)

      :strip ->
        modNames =
          for m <- mods,
              r_mod(m, :is_included),
              :lists.member(r_mod(m, :name), r_app_info(info, :modules)) do
            r_mod(m, :name)
          end

        app2 = r_app(app, info: r_app_info(info, modules: modNames))
        contents = gen_app(app2)

        appIoList =
          :io_lib.format(
            '%% app generated at ~w ~w\n~tp.\n\n',
            [:erlang.date(), :erlang.time(), contents]
          )

        [{:write_file, appFilename, to_utf8_bin_with_enc_comment(appIoList)}]

      :all ->
        modNames =
          for m <- mods, r_mod(m, :is_included) do
            r_mod(m, :name)
          end

        app2 = r_app(app, info: r_app_info(info, modules: modNames))
        contents = gen_app(app2)

        appIoList =
          :io_lib.format(
            '%% app generated at ~w ~w\n~tp.\n\n',
            [:erlang.date(), :erlang.time(), contents]
          )

        [{:write_file, appFilename, to_utf8_bin_with_enc_comment(appIoList)}]
    end
  end

  defp spec_opt_copy_file(dirName, baseName) do
    case :filelib.is_regular(
           :filename.join([dirName, baseName]),
           :erl_prim_loader
         ) do
      true ->
        [{:copy_file, baseName}]

      false ->
        []
    end
  end

  defp spec_create_dir(rootDir, sourceDir, baseDir, files) do
    libDir = :filename.join([rootDir, 'lib'])

    case abs_to_rel_path(libDir, sourceDir) do
      {:relative, dir} ->
        {:create_dir, dir, files}

      {:absolute, dir} ->
        {:create_dir, baseDir, dir, files}
    end
  end

  defp abs_to_rel_path(rootDir, sourcePath) do
    r = :filename.split(rootDir)
    s = :filename.split(sourcePath)
    abs_to_rel_path(r, s, sourcePath)
  end

  defp abs_to_rel_path([h | r], [h | s], sourcePath) do
    abs_to_rel_path(r, s, sourcePath)
  end

  defp abs_to_rel_path([], s, _SourcePath) do
    {:relative, :filename.join(s)}
  end

  defp abs_to_rel_path(_, _, sourcePath) do
    {:absolute, sourcePath}
  end

  def eval_spec(spec, sourceDir, targetDir) do
    sourceDir2 = :filename.absname(sourceDir)
    targetDir2 = :filename.absname(targetDir)

    try do
      case :filelib.is_dir(targetDir2) do
        true ->
          do_eval_spec(spec, sourceDir2, sourceDir2, targetDir2)
          :ok

        false ->
          {:error, targetDir2 ++ ': ' ++ :file.format_error(:enoent)}
      end
    catch
      {:error, text} ->
        cleanup_spec(spec, targetDir2)
        {:error, text}
    end
  end

  defp do_eval_spec(list, origSourceDir, sourceDir, targetDir)
       when is_list(list) do
    :lists.foreach(
      fn f ->
        do_eval_spec(f, origSourceDir, sourceDir, targetDir)
      end,
      list
    )
  end

  defp do_eval_spec({:create_dir, dir, files}, origSourceDir, sourceDir, targetDir) do
    sourceDir2 = :filename.join([sourceDir, dir])
    targetDir2 = :filename.join([targetDir, dir])
    :reltool_utils.create_dir(targetDir2)
    do_eval_spec(files, origSourceDir, sourceDir2, targetDir2)
  end

  defp do_eval_spec({:create_dir, dir, oldDir, files}, origSourceDir, _SourceDir, targetDir) do
    sourceDir2 = :filename.join([origSourceDir, oldDir])
    targetDir2 = :filename.join([targetDir, dir])
    :reltool_utils.create_dir(targetDir2)
    do_eval_spec(files, sourceDir2, sourceDir2, targetDir2)
  end

  defp do_eval_spec({:archive, archive, options, files}, origSourceDir, sourceDir, targetDir) do
    tmpSpec = {:create_dir, 'tmp', files}
    tmpDir = :filename.join([targetDir, 'tmp'])
    :reltool_utils.create_dir(tmpDir)
    do_eval_spec(files, origSourceDir, sourceDir, tmpDir)
    archiveFile = :filename.join([targetDir, archive])

    files2 =
      for f <- files do
        :erlang.element(2, f)
      end

    res = :zip.create(archiveFile, files2, [{:cwd, tmpDir} | options])
    cleanup_spec(tmpSpec, targetDir)

    case res do
      {:ok, _} ->
        :ok

      {:error, reason} ->
        :reltool_utils.throw_error('create archive ~ts failed: ~tp', [archiveFile, reason])
    end
  end

  defp do_eval_spec({:copy_file, file}, _OrigSourceDir, sourceDir, targetDir) do
    sourceFile = :filename.join([sourceDir, file])
    targetFile = :filename.join([targetDir, file])
    :reltool_utils.copy_file(sourceFile, targetFile)
  end

  defp do_eval_spec({:copy_file, file, oldFile}, origSourceDir, _SourceDir, targetDir) do
    sourceFile = :filename.join([origSourceDir, oldFile])
    targetFile = :filename.join([targetDir, file])
    :reltool_utils.copy_file(sourceFile, targetFile)
  end

  defp do_eval_spec({:write_file, file, bin}, _OrigSourceDir, _SourceDir, targetDir) do
    targetFile = :filename.join([targetDir, file])
    :reltool_utils.write_file(targetFile, bin)
  end

  defp do_eval_spec({:strip_beam, file}, _OrigSourceDir, sourceDir, targetDir) do
    sourceFile = :filename.join([sourceDir, file])
    targetFile = :filename.join([targetDir, file])
    beamBin = :reltool_utils.read_file(sourceFile)
    {:ok, {_, beamBin2}} = :beam_lib.strip(beamBin)
    :reltool_utils.write_file(targetFile, beamBin2)
  end

  defp cleanup_spec(list, targetDir) when is_list(list) do
    :lists.foreach(
      fn f ->
        cleanup_spec(f, targetDir)
      end,
      list
    )
  end

  defp cleanup_spec({:create_dir, dir, files}, targetDir) do
    targetDir2 = :filename.join([targetDir, dir])
    cleanup_spec(files, targetDir2)
    :file.del_dir(targetDir2)
  end

  defp cleanup_spec(
         {:create_dir, dir, _OldDir, files},
         targetDir
       ) do
    targetDir2 = :filename.join([targetDir, dir])
    cleanup_spec(files, targetDir2)
    :file.del_dir(targetDir2)
  end

  defp cleanup_spec(
         {:archive, archive, _Options, files},
         targetDir
       ) do
    targetFile = :filename.join([targetDir, archive])
    :file.delete(targetFile)
    tmpDir = :filename.join([targetDir, 'tmp'])
    cleanup_spec(files, tmpDir)
    :file.del_dir(tmpDir)
  end

  defp cleanup_spec({:copy_file, file}, targetDir) do
    targetFile = :filename.join([targetDir, file])
    :file.delete(targetFile)
  end

  defp cleanup_spec({:copy_file, newFile, _OldFile}, targetDir) do
    targetFile = :filename.join([targetDir, newFile])
    :file.delete(targetFile)
  end

  defp cleanup_spec({:write_file, file, _}, targetDir) do
    targetFile = :filename.join([targetDir, file])
    :file.delete(targetFile)
  end

  defp cleanup_spec({:strip_beam, file}, targetDir) do
    targetFile = :filename.join([targetDir, file])
    :file.delete(targetFile)
  end

  defp filter_spec(list, inclRegexps, exclRegexps) do
    do_filter_spec('', list, inclRegexps, exclRegexps)
  end

  defp do_filter_spec(path, list, inclRegexps, exclRegexps)
       when is_list(list) do
    :lists.zf(
      fn file ->
        do_filter_spec(path, file, inclRegexps, exclRegexps)
      end,
      list
    )
  end

  defp do_filter_spec(path, {:create_dir, dir, files}, inclRegexps, exclRegexps) do
    path2 = opt_join(path, dir)

    case do_filter_spec(path2, files, inclRegexps, exclRegexps) do
      [] ->
        case match(path2, inclRegexps, exclRegexps) do
          true ->
            {true, {:create_dir, dir, []}}

          false ->
            false
        end

      files2 when is_list(files2) ->
        {true, {:create_dir, dir, files2}}
    end
  end

  defp do_filter_spec(path, {:create_dir, newDir, oldDir, files}, inclRegexps, exclRegexps) do
    path2 = opt_join(path, newDir)

    case do_filter_spec(path2, files, inclRegexps, exclRegexps) do
      [] ->
        case match(path2, inclRegexps, exclRegexps) do
          true ->
            {true, {:create_dir, newDir, oldDir, []}}

          false ->
            false
        end

      files2 when is_list(files2) ->
        {true, {:create_dir, newDir, oldDir, files2}}
    end
  end

  defp do_filter_spec(path, {:archive, archive, options, files}, inclRegexps, exclRegexps) do
    case do_filter_spec(path, files, inclRegexps, exclRegexps) do
      [] ->
        case match(path, inclRegexps, exclRegexps) do
          true ->
            {true, {:archive, archive, options, []}}

          false ->
            false
        end

      files2 when is_list(files2) ->
        {true, {:archive, archive, options, files2}}
    end
  end

  defp do_filter_spec(path, {:copy_file, file}, inclRegexps, exclRegexps) do
    path2 = opt_join(path, file)
    match(path2, inclRegexps, exclRegexps)
  end

  defp do_filter_spec(path, {:copy_file, newFile, _OldFile}, inclRegexps, exclRegexps) do
    path2 = opt_join(path, newFile)
    match(path2, inclRegexps, exclRegexps)
  end

  defp do_filter_spec(path, {:write_file, file, _}, inclRegexps, exclRegexps) do
    path2 = opt_join(path, file)
    match(path2, inclRegexps, exclRegexps)
  end

  defp do_filter_spec(path, {:strip_beam, file}, inclRegexps, exclRegexps) do
    path2 = opt_join(path, file)
    match(path2, inclRegexps, exclRegexps)
  end

  defp opt_join([], file) do
    file
  end

  defp opt_join(path, file) do
    :filename.join([path, file])
  end

  defp match(string, inclRegexps, exclRegexps) do
    match(string, inclRegexps) and not match(string, exclRegexps)
  end

  defp match(_String, []) do
    false
  end

  defp match(
         string,
         [r_regexp(source: _, compiled: mP) | regexps]
       ) do
    case :re.run(string, mP, [{:capture, :none}]) do
      :nomatch ->
        match(string, regexps)

      :match ->
        true
    end
  end

  def install(relName, targetDir) do
    try do
      do_install(relName, targetDir)
    catch
      {:error, text} ->
        {:error, text}
    end
  end

  defp do_install(relName, targetDir) do
    targetDir2 = :filename.absname(targetDir)
    relDir = :filename.join([targetDir2, 'releases'])
    dataFile = :filename.join([relDir, 'start_erl.data'])
    bin = :reltool_utils.read_file(dataFile)

    case :string.lexemes(
           :unicode.characters_to_list(bin),
           ' \n'
         ) do
      [[erlVsn, relVsn] | _] ->
        ertsBinDir = :filename.join([targetDir2, 'erts-' ++ erlVsn, 'bin'])
        binDir = :filename.join([targetDir2, 'bin'])

        case :os.type() do
          {:win32, _} ->
            nativeRootDir = nativename(targetDir2)
            nativeErtsBinDir = nativename(ertsBinDir)

            iniData0 = [
              '[erlang]\r\n',
              'Bindir=',
              nativeErtsBinDir,
              '\r\n',
              'Progname=erl\r\n',
              'Rootdir=',
              nativeRootDir,
              '\r\n'
            ]

            iniData = :unicode.characters_to_binary(iniData0)
            iniFile = :filename.join([binDir, 'erl.ini'])
            :ok = :file.write_file(iniFile, iniData)

          _ ->
            subst_src_scripts(
              start_scripts(),
              ertsBinDir,
              binDir,
              [{'FINAL_ROOTDIR', targetDir2}, {'EMU', 'beam'}],
              [:preserve]
            )
        end

        relFile = :filename.join([relDir, relVsn, relName ++ '.rel'])

        :ok =
          :release_handler.create_RELEASES(
            targetDir2,
            relFile
          )

        :ok

      _ ->
        :reltool_utils.throw_error('~ts: Illegal data file syntax', [dataFile])
    end
  end

  defp nativename(dir) do
    escape_backslash(:filename.nativename(dir))
  end

  defp escape_backslash([?\\ | t]) do
    [[?\\, ?\\] | escape_backslash(t)]
  end

  defp escape_backslash([h | t]) do
    [h | escape_backslash(t)]
  end

  defp escape_backslash([]) do
    []
  end

  defp subst_src_scripts(scripts, srcDir, destDir, vars, opts) do
    fun = fn script ->
      subst_src_script(script, srcDir, destDir, vars, opts)
    end

    :lists.foreach(fun, scripts)
  end

  defp subst_src_script(script, srcDir, destDir, vars, opts) do
    subst_file(
      :filename.join([srcDir, script ++ '.src']),
      :filename.join([destDir, script]),
      vars,
      opts
    )
  end

  defp subst_file(src, dest, vars, opts) do
    bin = :reltool_utils.read_file(src)
    chars = subst(:unicode.characters_to_list(bin), vars)

    :reltool_utils.write_file(
      dest,
      :unicode.characters_to_binary(chars)
    )

    case :lists.member(:preserve, opts) do
      true ->
        fileInfo = :reltool_utils.read_file_info(src)
        :reltool_utils.write_file_info(dest, fileInfo)

      false ->
        :ok
    end
  end

  defp subst(str, vars) do
    subst(str, vars, [])
  end

  defp subst([[?%, c] | rest], vars, result)
       when ?A <= c and c <= ?Z do
    subst_var([c | rest], vars, result, [])
  end

  defp subst([[?%, c] | rest], vars, result)
       when ?a <= c and c <= ?z do
    subst_var([c | rest], vars, result, [])
  end

  defp subst([[?%, c] | rest], vars, result) when c == ?_ do
    subst_var([c | rest], vars, result, [])
  end

  defp subst([c | rest], vars, result) do
    subst(rest, vars, [c | result])
  end

  defp subst([], _Vars, result) do
    :lists.reverse(result)
  end

  defp subst_var([?% | rest], vars, result, varAcc) do
    key = :lists.reverse(varAcc)

    case :lists.keyfind(key, 1, vars) do
      {^key, value} ->
        subst(rest, vars, :lists.reverse(value, result))

      false ->
        subst(rest, vars, [?% | varAcc ++ [?% | result]])
    end
  end

  defp subst_var([c | rest], vars, result, varAcc) do
    subst_var(rest, vars, result, [c | varAcc])
  end

  defp subst_var([], vars, result, varAcc) do
    subst([], vars, [varAcc ++ [?% | result]])
  end

  defp start_scripts() do
    ['erl', 'start', 'start_erl']
  end
end
