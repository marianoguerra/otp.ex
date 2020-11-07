defmodule :m_systools_make do
  use Bitwise

  import :lists,
    only: [
      append: 1,
      filter: 2,
      foldl: 3,
      foreach: 2,
      keysearch: 3,
      keysort: 2,
      map: 2,
      member: 2,
      reverse: 1
    ]

  require Record

  Record.defrecord(:r_release, :release,
    name: :undefined,
    vsn: :undefined,
    erts_vsn: :undefined,
    applications: :undefined,
    incl_apps: :undefined
  )

  Record.defrecord(:r_application, :application,
    name: :undefined,
    type: :permanent,
    vsn: '',
    id: '',
    description: '',
    modules: [],
    uses: [],
    includes: [],
    regs: [],
    env: [],
    maxT: :infinity,
    maxP: :infinity,
    mod: [],
    start_phases: :undefined,
    dir: ''
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

  def make_script(relName) when is_list(relName) do
    make_script(relName, [])
  end

  def make_script(relName) do
    badarg(relName, [relName])
  end

  def make_script(relName, flags)
      when is_list(relName) and
             is_list(flags) do
    scriptName = get_script_name(relName, flags)

    case get_outdir(flags) do
      '' ->
        make_script(relName, scriptName, flags)

      outDir ->
        output =
          :filename.join(
            outDir,
            :filename.basename(scriptName)
          )

        make_script(relName, output, flags)
    end
  end

  def make_script(relName, output, flags)
      when is_list(relName) and is_list(output) and
             is_list(flags) do
    case check_args_script(flags) do
      [] ->
        path0 = get_path(flags)
        path1 = mk_path(path0)
        path = make_set(path1 ++ :code.get_path())
        modTestP = {member(:src_tests, flags), xref_p(flags)}

        case get_release(relName, path, modTestP) do
          {:ok, release, appls, warnings0} ->
            warnings = wsasl(flags, warnings0)

            case :systools_lib.werror(flags, warnings) do
              true ->
                warnings1 =
                  for {:warning, w} <- warnings do
                    w
                  end

                return(
                  {:error, :systools_make, {:warnings_treated_as_errors, warnings1}},
                  warnings,
                  flags
                )

              false ->
                case generate_script(output, release, appls, flags) do
                  :ok ->
                    return(:ok, warnings, flags)

                  error ->
                    return(error, warnings, flags)
                end
            end

          error ->
            return(error, [], flags)
        end

      errorVars ->
        badarg(errorVars, [relName, flags])
    end
  end

  def make_script(relName, _Output, flags) when is_list(flags) do
    badarg(relName, [relName, flags])
  end

  def make_script(relName, _Output, flags) do
    badarg(flags, [relName, flags])
  end

  defp wsasl(options, warnings) do
    case :lists.member(:no_warn_sasl, options) do
      true ->
        :lists.delete({:warning, :missing_sasl}, warnings)

      false ->
        warnings
    end
  end

  defp badarg(badArg, args) do
    :erlang.error({:badarg, badArg}, args)
  end

  defp get_script_name(relName, flags) do
    case get_flag(:script_name, flags) do
      {:script_name, scriptName} when is_list(scriptName) ->
        scriptName

      _ ->
        relName
    end
  end

  defp get_path(flags) do
    case get_flag(:path, flags) do
      {:path, path} when is_list(path) ->
        path

      _ ->
        []
    end
  end

  defp get_outdir(flags) do
    case get_flag(:outdir, flags) do
      {:outdir, outDir} when is_list(outDir) ->
        outDir

      _ ->
        ''
    end
  end

  defp return(:ok, warnings, flags) do
    case member(:silent, flags) do
      true ->
        {:ok, :systools_make, warnings}

      _ ->
        :io.format('~ts', [format_warning(warnings)])
        :ok
    end
  end

  defp return({:error, mod, error}, _, flags) do
    case member(:silent, flags) do
      true ->
        {:error, mod, error}

      _ ->
        :io.format('~ts', [mod.format_error(error)])
        :error
    end
  end

  def make_hybrid_boot(tmpVsn, boot1, boot2, args) do
    try do
      do_make_hybrid_boot(tmpVsn, boot1, boot2, args)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp do_make_hybrid_boot(tmpVsn, oldBoot, newBoot, args) do
    {:script, {_RelName1, _RelVsn1}, oldScript} = :erlang.binary_to_term(oldBoot)
    {:script, {newRelName, _RelVsn2}, newScript} = :erlang.binary_to_term(newBoot)

    fun1 = fn
      {:progress, :kernel_load_completed} ->
        false

      _ ->
        true
    end

    {_OldKernelLoad, oldRest1} =
      :lists.splitwith(
        fun1,
        oldScript
      )

    {newKernelLoad, newRest1} =
      :lists.splitwith(
        fun1,
        newScript
      )

    fun2 = fn
      {:progress, :modules_loaded} ->
        false

      _ ->
        true
    end

    {oldModLoad, oldRest2} =
      :lists.splitwith(
        fun2,
        oldRest1
      )

    {newModLoad, newRest2} =
      :lists.splitwith(
        fun2,
        newRest1
      )

    fun3 = fn
      {:kernelProcess, _, _} ->
        false

      _ ->
        true
    end

    {oldPaths, oldRest3} = :lists.splitwith(fun3, oldRest2)
    {newPaths, newRest3} = :lists.splitwith(fun3, newRest2)

    fun4 = fn
      {:progress, :init_kernel_started} ->
        false

      _ ->
        true
    end

    {_OldKernelProcs, oldApps} =
      :lists.splitwith(
        fun4,
        oldRest3
      )

    {newKernelProcs, newApps} =
      :lists.splitwith(
        fun4,
        newRest3
      )

    matchPaths = get_regexp_path()
    modLoad = replace_module_load(oldModLoad, newModLoad, matchPaths)
    paths = replace_paths(oldPaths, newPaths, matchPaths)
    {stdlib, sasl} = get_apps(newApps, :undefined, :undefined)
    apps0 = replace_apps(oldApps, stdlib, sasl)
    apps = add_apply_upgrade(apps0, args)
    script = newKernelLoad ++ modLoad ++ paths ++ newKernelProcs ++ apps
    boot = :erlang.term_to_binary({:script, {newRelName, tmpVsn}, script})
    {:ok, boot}
  end

  defp get_regexp_path() do
    {:ok, kernelMP} = :re.compile('kernel-[0-9.]+', [:unicode])
    {:ok, stdlibMP} = :re.compile('stdlib-[0-9.]+', [:unicode])
    {:ok, saslMP} = :re.compile('sasl-[0-9.]+', [:unicode])
    [kernelMP, stdlibMP, saslMP]
  end

  defp replace_module_load(old, new, [mP | matchPaths]) do
    replace_module_load(do_replace_module_load(old, new, mP), new, matchPaths)
  end

  defp replace_module_load(script, _, []) do
    script
  end

  defp do_replace_module_load(
         [
           [{:path, [oldAppPath]}, {:primLoad, oldMods}]
           | oldRest
         ],
         new,
         mP
       ) do
    case :re.run(oldAppPath, mP, [{:capture, :none}]) do
      :nomatch ->
        [
          [{:path, [oldAppPath]}, {:primLoad, oldMods}]
          | do_replace_module_load(oldRest, new, mP)
        ]

      :match ->
        get_module_load(new, mP) ++ oldRest
    end
  end

  defp do_replace_module_load([other | rest], new, mP) do
    [other | do_replace_module_load(rest, new, mP)]
  end

  defp do_replace_module_load([], _, _) do
    []
  end

  defp get_module_load(
         [
           [{:path, [appPath]}, {:primLoad, mods}]
           | rest
         ],
         mP
       ) do
    case :re.run(appPath, mP, [{:capture, :none}]) do
      :nomatch ->
        get_module_load(rest, mP)

      :match ->
        [{:path, [appPath]}, {:primLoad, mods}]
    end
  end

  defp get_module_load([_ | rest], mP) do
    get_module_load(rest, mP)
  end

  defp get_module_load([], _) do
    []
  end

  defp replace_paths([{:path, oldPaths} | old], new, matchPaths) do
    {:path, newPath} = :lists.keyfind(:path, 1, new)

    [
      {:path, do_replace_paths(oldPaths, newPath, matchPaths)}
      | old
    ]
  end

  defp replace_paths([other | old], new, matchPaths) do
    [other | replace_paths(old, new, matchPaths)]
  end

  defp do_replace_paths(old, new, [mP | matchPaths]) do
    do_replace_paths(do_replace_paths1(old, new, mP), new, matchPaths)
  end

  defp do_replace_paths(paths, _, []) do
    paths
  end

  defp do_replace_paths1([p | ps], new, mP) do
    case :re.run(p, mP, [{:capture, :none}]) do
      :nomatch ->
        [p | do_replace_paths1(ps, new, mP)]

      :match ->
        get_path(new, mP) ++ ps
    end
  end

  defp do_replace_paths1([], _, _) do
    []
  end

  defp get_path([p | ps], mP) do
    case :re.run(p, mP, [{:capture, :none}]) do
      :nomatch ->
        get_path(ps, mP)

      :match ->
        [p]
    end
  end

  defp get_path([], _) do
    []
  end

  defp get_apps(
         [
           {:apply, {:application, :load, [{:application, :stdlib, _}]}} = stdlib
           | script
         ],
         _,
         sasl
       ) do
    get_apps(script, stdlib, sasl)
  end

  defp get_apps(
         [
           {:apply, {:application, :load, [{:application, :sasl, _}]}} = sasl
           | _Script
         ],
         stdlib,
         _
       ) do
    {stdlib, sasl}
  end

  defp get_apps([_ | script], stdlib, sasl) do
    get_apps(script, stdlib, sasl)
  end

  defp get_apps([], :undefined, _) do
    throw({:error, {:app_not_found, :stdlib}})
  end

  defp get_apps([], _, :undefined) do
    throw({:error, {:app_not_found, :sasl}})
  end

  defp replace_apps(
         [
           {:apply, {:application, :load, [{:application, :stdlib, _}]}}
           | script
         ],
         stdlib,
         sasl
       ) do
    [stdlib | replace_apps(script, :undefined, sasl)]
  end

  defp replace_apps(
         [
           {:apply, {:application, :load, [{:application, :sasl, _}]}}
           | script
         ],
         _Stdlib,
         sasl
       ) do
    [sasl | script]
  end

  defp replace_apps([stuff | script], stdlib, sasl) do
    [stuff | replace_apps(script, stdlib, sasl)]
  end

  defp replace_apps([], :undefined, _) do
    throw({:error, {:app_not_replaced, :sasl}})
  end

  defp replace_apps([], _, _) do
    throw({:error, {:app_not_replaced, :stdlib}})
  end

  defp add_apply_upgrade(script, args) do
    [
      {:progress, :started}
      | revScript
    ] = :lists.reverse(script)

    :lists.reverse([
      [{:progress, :started}, {:apply, {:release_handler, :new_emulator_upgrade, args}}]
      | revScript
    ])
  end

  def make_tar(relName) when is_list(relName) do
    make_tar(relName, [])
  end

  def make_tar(relName) do
    badarg(relName, [relName])
  end

  def make_tar(relName, flags)
      when is_list(relName) and
             is_list(flags) do
    case check_args_tar(flags) do
      [] ->
        path0 = get_path(flags)
        path1 = mk_path(path0)
        path = make_set(path1 ++ :code.get_path())
        modTestP = {member(:src_tests, flags), xref_p(flags)}

        case get_release(relName, path, modTestP) do
          {:ok, release, appls, warnings0} ->
            warnings = wsasl(flags, warnings0)

            case :systools_lib.werror(flags, warnings) do
              true ->
                warnings1 =
                  for {:warning, w} <- warnings do
                    w
                  end

                return(
                  {:error, :systools_make, {:warnings_treated_as_errors, warnings1}},
                  warnings,
                  flags
                )

              false ->
                case (try do
                        mk_tar(relName, release, appls, flags, path1)
                      catch
                        :error, e -> {:EXIT, {e, __STACKTRACE__}}
                        :exit, e -> {:EXIT, e}
                        e -> e
                      end) do
                  :ok ->
                    return(:ok, warnings, flags)

                  error ->
                    return(error, warnings, flags)
                end
            end

          error ->
            return(error, [], flags)
        end

      errorVars ->
        badarg(errorVars, [relName, flags])
    end
  end

  def make_tar(relName, flags) when is_list(flags) do
    badarg(relName, [relName, flags])
  end

  def make_tar(relName, flags) do
    badarg(flags, [relName, flags])
  end

  def get_release(file, path) do
    get_release(file, path, {false, false})
  end

  def get_release(file, path, modTestP) do
    case (try do
            get_release1(file, path, modTestP)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, error} ->
        {:error, :systools_make, error}

      {:EXIT, why} ->
        {:error, :systools_make, {:EXIT, why}}

      answer ->
        answer
    end
  end

  defp get_release1(file, path, modTestP) do
    {:ok, release, warnings1} = read_release(file, path)
    {:ok, appls0} = collect_applications(release, path)
    {:ok, appls1} = check_applications(appls0)

    {:ok, appls2} =
      sort_used_and_incl_appls(
        appls1,
        release
      )

    {:ok, warnings2} = check_modules(appls2, path, modTestP)
    {:ok, appls} = sort_appls(appls2)
    {:ok, release, appls, warnings1 ++ warnings2}
  end

  def read_release(file, path) do
    case read_file(file ++ '.rel', ['.' | path]) do
      {:ok, release, _FullName} ->
        check_rel(release)

      {:error, error} ->
        throw({:error, :systools_make, error})
    end
  end

  defp check_rel(release) do
    case (try do
            check_rel1(release)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, {name, vsn, evsn, appl, incl}, ws} ->
        {:ok,
         r_release(name: name, vsn: vsn, erts_vsn: evsn, applications: appl, incl_apps: incl), ws}

      {:error, error} ->
        throw({:error, :systools_make, error})

      error ->
        throw({:error, :systools_make, error})
    end
  end

  defp check_rel1({:release, {name, vsn}, {:erts, eVsn}, appl})
       when is_list(appl) do
    ^name = check_name(name)
    ^vsn = check_vsn(vsn)
    ^eVsn = check_evsn(eVsn)
    {{appls, incls}, ws} = check_appl(appl)
    {:ok, {name, vsn, eVsn, appls, incls}, ws}
  end

  defp check_rel1(_) do
    {:error, :badly_formatted_release}
  end

  defp check_name(name) do
    case string_p(name) do
      true ->
        name

      _ ->
        throw({:error, {:illegal_name, name}})
    end
  end

  defp check_vsn(vsn) do
    case string_p(vsn) do
      true ->
        vsn

      _ ->
        throw({:error, {:illegal_form, vsn}})
    end
  end

  defp check_evsn(vsn) do
    case string_p(vsn) do
      true ->
        vsn

      _ ->
        throw({:error, {:illegal_form, {:erts, vsn}}})
    end
  end

  defp check_appl(appl) do
    case filter(
           fn
             {app, vsn} when is_atom(app) ->
               not string_p(vsn)

             {app, vsn, incl} when is_atom(app) and is_list(incl) ->
               case {string_p(vsn), a_list_p(incl)} do
                 {true, true} ->
                   false

                 _ ->
                   true
               end

             {app, vsn, type} when is_atom(app) and is_atom(type) ->
               case {string_p(vsn), is_app_type(type)} do
                 {true, true} ->
                   false

                 _ ->
                   true
               end

             {app, vsn, type, incl}
             when is_atom(app) and
                    is_atom(type) and
                    is_list(incl) ->
               case {string_p(vsn), is_app_type(type), a_list_p(incl)} do
                 {true, true, true} ->
                   false

                 _ ->
                   true
               end

             _ ->
               true
           end,
           appl
         ) do
      [] ->
        {applsNoIncls, incls} = split_app_incl(appl)
        {:ok, ws} = mandatory_applications(applsNoIncls, :undefined, :undefined, :undefined)
        {{applsNoIncls, incls}, ws}

      illegal ->
        throw({:error, {:illegal_applications, illegal}})
    end
  end

  defp mandatory_applications([{:kernel, _, type} | apps], :undefined, stdlib, sasl) do
    mandatory_applications(apps, type, stdlib, sasl)
  end

  defp mandatory_applications([{:stdlib, _, type} | apps], kernel, :undefined, sasl) do
    mandatory_applications(apps, kernel, type, sasl)
  end

  defp mandatory_applications([{:sasl, _, type} | apps], kernel, stdlib, :undefined) do
    mandatory_applications(apps, kernel, stdlib, type)
  end

  defp mandatory_applications([_ | apps], kernel, stdlib, sasl) do
    mandatory_applications(apps, kernel, stdlib, sasl)
  end

  defp mandatory_applications([], type, _, _) when type !== :permanent do
    error_mandatory_application(:kernel, type)
  end

  defp mandatory_applications([], _, type, _) when type !== :permanent do
    error_mandatory_application(:stdlib, type)
  end

  defp mandatory_applications([], _, _, :undefined) do
    {:ok, [{:warning, :missing_sasl}]}
  end

  defp mandatory_applications([], _, _, _) do
    {:ok, []}
  end

  defp error_mandatory_application(app, :undefined) do
    throw({:error, {:missing_mandatory_app, app}})
  end

  defp error_mandatory_application(app, type) do
    throw({:error, {:mandatory_app, app, type}})
  end

  defp split_app_incl(appl) do
    split_app_incl(appl, [], [])
  end

  defp split_app_incl([{app, vsn} | appls], apps, incls) do
    split_app_incl(appls, [{app, vsn, :permanent} | apps], incls)
  end

  defp split_app_incl([{app, vsn, incl} | appls], apps, incls)
       when is_list(incl) do
    split_app_incl(appls, [{app, vsn, :permanent} | apps], [{app, incl} | incls])
  end

  defp split_app_incl([{app, vsn, type} | appls], apps, incls) do
    split_app_incl(appls, [{app, vsn, type} | apps], incls)
  end

  defp split_app_incl([{app, vsn, type, incl} | appls], apps, incls)
       when is_list(incl) do
    split_app_incl(appls, [{app, vsn, type} | apps], [{app, incl} | incls])
  end

  defp split_app_incl([], apps, incls) do
    {reverse(apps), reverse(incls)}
  end

  defp collect_applications(release, path) do
    appls = r_release(release, :applications)
    incls = r_release(release, :incl_apps)

    x =
      foldl(
        fn {name, vsn, type}, {ok, errs} ->
          case read_application(to_list(name), vsn, path, incls) do
            {:ok, a} ->
              case {r_application(a, :name), r_application(a, :vsn)} do
                {^name, ^vsn} ->
                  {[{{name, vsn}, r_application(a, type: type)} | ok], errs}

                e ->
                  {ok, [{:bad_application_name, {name, e}} | errs]}
              end

            {:error, what} ->
              {ok, [{:error_reading, {name, what}} | errs]}
          end
        end,
        {[], []},
        appls
      )

    case x do
      {a, []} ->
        {:ok, reverse(a)}

      {_, errs} ->
        throw({:error, errs})
    end
  end

  def read_application(name, vsn, path, incls) do
    read_application(name, vsn, path, incls, false, :no_fault)
  end

  defp read_application(name, vsn, [dir | path], incls, found, firstError) do
    case read_file(name ++ '.app', [dir]) do
      {:ok, term, fullName} ->
        case parse_application(term, fullName, vsn, incls) do
          {:error, {:no_valid_version, {^vsn, otherVsn}}}
          when firstError == :no_fault ->
            nFE =
              {:no_valid_version,
               {{'should be', vsn}, {'found file', :filename.join(dir, name ++ '.app'), otherVsn}}}

            read_application(name, vsn, path, incls, true, nFE)

          {:error, {:no_valid_version, {^vsn, _OtherVsn}}} ->
            read_application(name, vsn, path, incls, true, firstError)

          res ->
            res
        end

      {:error, {:parse, _File, {line, _Mod, err}}}
      when firstError == :no_fault ->
        read_application(
          name,
          vsn,
          path,
          incls,
          found,
          {:parse_error, {:filename.join(dir, name ++ '.app'), line, err}}
        )

      {:error, {:parse, _File, _Err}} ->
        read_application(name, vsn, path, incls, found, firstError)

      {:error, _Err} ->
        read_application(name, vsn, path, incls, found, firstError)
    end
  end

  defp read_application(name, vsn, [], _, true, :no_fault) do
    {:error, {:application_vsn, {name, vsn}}}
  end

  defp read_application(_Name, _Vsn, [], _, true, firstError) do
    {:error, firstError}
  end

  defp read_application(name, _, [], _, _, :no_fault) do
    {:error, {:not_found, name ++ '.app'}}
  end

  defp read_application(_Name, _, [], _, _, firstError) do
    {:error, firstError}
  end

  defp parse_application({:application, name, dict}, file, vsn, incls)
       when is_atom(name) and is_list(dict) do
    items = [
      :vsn,
      :id,
      :description,
      :modules,
      :registered,
      :applications,
      :included_applications,
      :mod,
      :start_phases,
      :env,
      :maxT,
      :maxP
    ]

    case (try do
            get_items(items, dict)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      [^vsn, id, desc, mods, regs, apps, incs0, mod, phases, env, maxT, maxP] ->
        case override_include(name, incs0, incls) do
          {:ok, incs} ->
            {:ok,
             r_application(
               name: name,
               vsn: vsn,
               id: id,
               description: desc,
               modules: mods,
               uses: apps,
               includes: incs,
               regs: regs,
               mod: mod,
               start_phases: phases,
               env: env,
               maxT: maxT,
               maxP: maxP,
               dir: :filename.dirname(file)
             )}

          {:error, incApps} ->
            {:error, {:override_include, incApps}}
        end

      [otherVsn, _, _, _, _, _, _, _, _, _, _, _] ->
        {:error, {:no_valid_version, {vsn, otherVsn}}}

      err ->
        {:error, {err, {:application, name, dict}}}
    end
  end

  defp parse_application(other, _, _, _) do
    {:error, {:badly_formatted_application, other}}
  end

  defp override_include(name, incs, incls) do
    case keysearch(name, 1, incls) do
      {:value, {^name, i}} ->
        case specified(i, incs) do
          [] ->
            {:ok, i}

          notSpec ->
            {:error, notSpec}
        end

      _ ->
        {:ok, incs}
    end
  end

  defp specified([app | incls], spec) do
    case member(app, spec) do
      true ->
        specified(incls, spec)

      _ ->
        [app | specified(incls, spec)]
    end
  end

  defp specified([], _) do
    []
  end

  defp get_items([h | t], dict) do
    item = check_item(keysearch(h, 1, dict), h)
    [item | get_items(t, dict)]
  end

  defp get_items([], _Dict) do
    []
  end

  defp check_item({_, {:mod, {m, a}}}, _) when is_atom(m) do
    {m, a}
  end

  defp check_item({_, {:mod, []}}, _) do
    []
  end

  defp check_item({_, {:vsn, vsn}}, i) do
    case string_p(vsn) do
      true ->
        vsn

      _ ->
        throw({:bad_param, i})
    end
  end

  defp check_item({_, {:id, id}}, i) do
    case string_p(id) do
      true ->
        id

      _ ->
        throw({:bad_param, i})
    end
  end

  defp check_item({_, {:description, desc}}, i) do
    case string_p(desc) do
      true ->
        desc

      _ ->
        throw({:bad_param, i})
    end
  end

  defp check_item({_, {:applications, apps}}, i) do
    case a_list_p(apps) do
      true ->
        apps

      _ ->
        throw({:bad_param, i})
    end
  end

  defp check_item({_, {:included_applications, apps}}, i) do
    case a_list_p(apps) do
      true ->
        apps

      _ ->
        throw({:bad_param, i})
    end
  end

  defp check_item({_, {:registered, regs}}, i) do
    case a_list_p(regs) do
      true ->
        regs

      _ ->
        throw({:bad_param, i})
    end
  end

  defp check_item({_, {:modules, mods}}, i) do
    case a_list_p(mods) do
      true ->
        mods

      _ ->
        throw({:bad_param, i})
    end
  end

  defp check_item({_, {:start_phases, :undefined}}, _) do
    :undefined
  end

  defp check_item({_, {:start_phases, phase}}, i) do
    case t_list_p(phase) do
      true ->
        phase

      _ ->
        throw({:bad_param, i})
    end
  end

  defp check_item({_, {:env, env}}, i) do
    case t_list_p(env) do
      true ->
        env

      _ ->
        throw({:bad_param, i})
    end
  end

  defp check_item({_, {:maxT, maxT}}, i) do
    case maxT do
      ^maxT when is_integer(maxT) and maxT > 0 ->
        maxT

      :infinity ->
        :infinity

      _ ->
        throw({:bad_param, i})
    end
  end

  defp check_item({_, {:maxP, maxP}}, i) do
    case maxP do
      ^maxP when is_integer(maxP) and maxP > 0 ->
        maxP

      :infinity ->
        :infinity

      _ ->
        throw({:bad_param, i})
    end
  end

  defp check_item(false, :included_applications) do
    []
  end

  defp check_item(false, :mod) do
    []
  end

  defp check_item(false, :env) do
    []
  end

  defp check_item(false, :id) do
    []
  end

  defp check_item(false, :start_phases) do
    :undefined
  end

  defp check_item(false, :maxT) do
    :infinity
  end

  defp check_item(false, :maxP) do
    :infinity
  end

  defp check_item(_, item) do
    throw({:missing_param, item})
  end

  defp check_applications(appls) do
    undef_appls(appls)
    dupl_regs(appls)

    incs =
      for {{app, appv}, a} <- appls,
          incApp <- r_application(a, :includes) do
        {incApp, app, appv, r_application(a, :dir)}
      end

    dupl_incls(incs)
    res = add_top_apps_to_uses(incs, appls, [])
    {:ok, res}
  end

  defp undef_appls(appls) do
    case undefined_applications(appls) do
      [] ->
        :ok

      l ->
        throw({:error, {:undefined_applications, make_set(l)}})
    end
  end

  defp dupl_regs(appls) do
    regs =
      for {{app, appv}, a} <- appls,
          name <- r_application(a, :regs) do
        {name, app, appv, r_application(a, :dir)}
      end

    case duplicates(regs) do
      [] ->
        :ok

      dups ->
        throw({:error, {:duplicate_register, dups}})
    end
  end

  defp dupl_incls(incs) do
    case duplicates(incs) do
      [] ->
        :ok

      dups ->
        throw({:error, {:duplicate_include, dups}})
    end
  end

  defp add_top_apps_to_uses(_InclApps, [], res) do
    res
  end

  defp add_top_apps_to_uses(inclApps, [{name, appl} | appls], res) do
    myTop = find_top_app(r_application(appl, :name), inclApps)

    f = fn
      usedApp, accIn when usedApp == myTop ->
        accIn -- [myTop]

      usedApp, accIn ->
        case :lists.keysearch(usedApp, 1, inclApps) do
          false ->
            accIn

          {:value, {_, dependApp, _, _}} ->
            usedAppTop = find_top_app(dependApp, inclApps)

            case {:lists.member(usedAppTop, accIn), myTop} do
              {true, _} ->
                accIn -- [usedApp]

              {_, ^usedAppTop} ->
                accIn

              _ ->
                accIn1 = accIn -- [usedApp]
                accIn1 ++ [usedAppTop]
            end
        end
    end

    newUses = foldl(f, r_application(appl, :uses), r_application(appl, :uses))
    add_top_apps_to_uses(inclApps, appls, res ++ [{name, r_application(appl, uses: newUses)}])
  end

  defp find_top_app(app, inclApps) do
    case :lists.keysearch(app, 1, inclApps) do
      false ->
        app

      {:value, {_, topApp, _, _}} ->
        find_top_app(topApp, inclApps)
    end
  end

  defp undefined_applications(appls) do
    uses =
      append(
        map(
          fn {_, a} ->
            r_application(a, :uses) ++ r_application(a, :includes)
          end,
          appls
        )
      )

    defined =
      map(
        fn {{x, _}, _} ->
          x
        end,
        appls
      )

    filter(
      fn x ->
        not member(x, defined)
      end,
      uses
    )
  end

  defp sort_used_and_incl_appls(applications, release) when is_tuple(release) do
    {:ok,
     sort_used_and_incl_appls(
       applications,
       r_release(release, :applications)
     )}
  end

  defp sort_used_and_incl_appls([{tuple, appl} | appls], orderedAppls) do
    incls2 =
      case r_application(appl, :includes) do
        incls when length(incls) > 1 ->
          sort_appl_list(incls, orderedAppls)

        incls ->
          incls
      end

    uses2 =
      case r_application(appl, :uses) do
        uses when length(uses) > 1 ->
          sort_appl_list(uses, orderedAppls)

        uses ->
          uses
      end

    appl2 = r_application(appl, includes: incls2, uses: uses2)

    [
      {tuple, appl2}
      | sort_used_and_incl_appls(
          appls,
          orderedAppls
        )
    ]
  end

  defp sort_used_and_incl_appls([], _OrderedAppls) do
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

  defp find_pos([name | incs], orderedAppls) do
    [
      find_pos(1, name, orderedAppls)
      | find_pos(
          incs,
          orderedAppls
        )
    ]
  end

  defp find_pos([], _OrderedAppls) do
    []
  end

  defp find_pos(n, name, [{name, _Vsn, _Type} | _OrderedAppls]) do
    {n, name}
  end

  defp find_pos(n, name, [_OtherAppl | orderedAppls]) do
    find_pos(n + 1, name, orderedAppls)
  end

  defp check_modules(appls, path, testP) do
    m1 =
      for {{app, _Appv}, a} <- appls,
          mod <- r_application(a, :modules) do
        {mod, app, r_application(a, :dir)}
      end

    case duplicates(m1) do
      [] ->
        case check_mods(m1, appls, path, testP) do
          {:error, errors} ->
            throw({:error, {:modules, errors}})

          return ->
            return
        end

      dups ->
        throw({:error, {:duplicate_modules, dups}})
    end
  end

  defp check_mods(modules, appls, path, {srcTestP, xrefP}) do
    srcTestRes = check_src(modules, appls, path, srcTestP)
    xrefRes = check_xref(appls, path, xrefP)
    res = srcTestRes ++ xrefRes

    case filter(
           fn
             {:error, _} ->
               true

             _ ->
               false
           end,
           res
         ) do
      [] ->
        {:ok,
         filter(
           fn
             {:warning, _} ->
               true

             _ ->
               false
           end,
           res
         )}

      errors ->
        {:error, errors}
    end
  end

  defp check_src(modules, appls, path, true) do
    ext = :code.objfile_extension()
    incPath = create_include_path(appls, path)

    append(
      map(
        fn modT ->
          {mod, app, dir} = modT

          case check_mod(mod, app, dir, ext, incPath) do
            :ok ->
              []

            {:error, error} ->
              [{:error, {error, modT}}]

            {:warning, warn} ->
              [{:warning, {warn, modT}}]
          end
        end,
        modules
      )
    )
  end

  defp check_src(_, _, _, _) do
    []
  end

  defp check_xref(_Appls, _Path, false) do
    []
  end

  defp check_xref(appls, path, xrefP) do
    appDirsL =
      for {{app, _Appv}, a} <- appls do
        {app, r_application(a, :dir)}
      end

    appDirs0 = :sofs.relation(appDirsL)

    appDirs =
      case xrefP do
        true ->
          appDirs0

        {true, apps} ->
          :sofs.restriction(appDirs0, :sofs.set(apps))
      end

    xrefArgs = [{:xref_mode, :modules}]

    case (try do
            :xref.start(:systools_make, xrefArgs)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, _Pid} ->
        :ok

      {:error, {:already_started, _Pid}} ->
        :xref.stop(:systools_make)
        {:ok, _} = :xref.start(:systools_make, xrefArgs)
        :ok
    end

    {:ok, _} = :xref.set_default(:systools_make, :verbose, false)

    libPath =
      case path == :code.get_path() do
        true ->
          :code_path

        false ->
          path
      end

    :ok = :xref.set_library_path(:systools_make, libPath)
    check_xref(:sofs.to_external(appDirs))
  end

  defp check_xref([{app, appDir} | appls]) do
    case :xref.add_application(:systools_make, appDir, {:name, app}) do
      {:ok, _App} ->
        check_xref(appls)

      error ->
        :xref.stop(:systools_make)
        [{:error, error}]
    end
  end

  defp check_xref([]) do
    r =
      case :xref.analyze(
             :systools_make,
             :undefined_functions
           ) do
        {:ok, []} ->
          []

        {:ok, undefined} ->
          adjust_for_hipe(undefined)

        error ->
          [{:error, error}]
      end

    :xref.stop(:systools_make)
    r
  end

  defp adjust_for_hipe(undef) do
    case :erlang.system_info(:hipe_architecture) do
      :undefined ->
        u =
          :lists.filter(
            fn
              {:hipe_bifs, _, _} ->
                false

              {:hipe, _, _} ->
                false

              _ ->
                true
            end,
            undef
          )

        cond do
          [] == u ->
            []

          true ->
            [{:warning, {:exref_undef, u}}]
        end

      _Arch ->
        u =
          :lists.filter(
            fn
              {:hipe_bifs, :write_u64, 2} ->
                false

              _ ->
                true
            end,
            undef
          )

        [{:warning, {:exref_undef, u}}]
    end
  end

  defp xref_p(flags) do
    case member(:exref, flags) do
      true ->
        exists_xref(true)

      _ ->
        case get_flag(:exref, flags) do
          {:exref, appls} when is_list(appls) ->
            case a_list_p(appls) do
              true ->
                exists_xref({true, appls})

              _ ->
                false
            end

          _ ->
            false
        end
    end
  end

  defp exists_xref(flag) do
    case :code.ensure_loaded(:xref) do
      {:error, _} ->
        false

      _ ->
        flag
    end
  end

  defp check_mod(mod, app, dir, ext, incPath) do
    objFile = mod_to_filename(dir, mod, ext)

    case :file.read_file_info(objFile) do
      {:ok, fileInfo} ->
        lastModTime = r_file_info(fileInfo, :mtime)
        check_module(mod, dir, lastModTime, incPath)

      _ ->
        {:error, {:module_not_found, app, mod}}
    end
  end

  defp mod_to_filename(dir, mod, ext) do
    :filename.join(dir, :erlang.atom_to_list(mod) ++ ext)
  end

  defp check_module(mod, dir, objModTime, incPath) do
    {srcDirs, _IncDirs} = smart_guess(dir, incPath)

    case locate_src(mod, srcDirs) do
      {:ok, _FDir, _File, lastModTime} ->
        cond do
          lastModTime > objModTime ->
            {:warning, :obj_out_of_date}

          true ->
            :ok
        end

      _ ->
        {:warning, :source_not_found}
    end
  end

  defp locate_src(mod, [dir | dirs]) do
    file = mod_to_filename(dir, mod, '.erl')

    case :file.read_file_info(file) do
      {:ok, fileInfo} ->
        lastModTime = r_file_info(fileInfo, :mtime)
        {:ok, dir, file, lastModTime}

      _ ->
        locate_src(mod, dirs)
    end
  end

  defp locate_src(_, []) do
    false
  end

  defp smart_guess(dir, incPath) do
    case reverse(:filename.split(dir)) do
      ['ebin' | d] ->
        d1 = reverse(d)
        dirs = [:filename.join(d1 ++ ['src']), :filename.join(d1 ++ ['src', 'e_src'])]
        {dirs, dirs ++ incPath}

      _ ->
        {[dir], [dir] ++ incPath}
    end
  end

  defp generate_script(output, release, appls, flags) do
    pathFlag = path_flag(flags)
    variables = get_variables(flags)
    preloaded = preloaded()
    mandatory = mandatory_modules()

    script =
      {:script, {r_release(release, :name), r_release(release, :vsn)},
       [
         {:preLoaded, preloaded},
         {:progress, :preloaded},
         {:path, create_mandatory_path(appls, pathFlag, variables)},
         {:primLoad, mandatory},
         {:kernel_load_completed},
         {:progress, :kernel_load_completed}
       ] ++
         load_appl_mods(
           appls,
           mandatory ++ preloaded,
           pathFlag,
           variables
         ) ++
         [
           {:path,
            create_path(
              appls,
              pathFlag,
              variables
            )}
         ] ++
         create_kernel_procs(appls) ++
         create_load_appls(appls) ++
         create_start_appls(appls) ++
         script_end(
           :lists.member(
             :no_dot_erlang,
             flags
           )
         )}

    scriptFile = output ++ '.script'

    case :file.open(
           scriptFile,
           [:write, {:encoding, :utf8}]
         ) do
      {:ok, fd} ->
        :io.format(fd, '%% ~s\n%% script generated at ~w ~w\n~tp.\n', [
          :epp.encoding_to_string(:utf8),
          :erlang.date(),
          :erlang.time(),
          script
        ])

        case :file.close(fd) do
          :ok ->
            bootFile = output ++ '.boot'

            case :file.write_file(
                   bootFile,
                   :erlang.term_to_binary(script)
                 ) do
              :ok ->
                :ok

              {:error, reason} ->
                {:error, :systools_make, {:open, bootFile, reason}}
            end

          {:error, reason} ->
            {:error, :systools_make, {:close, scriptFile, reason}}
        end

      {:error, reason} ->
        {:error, :systools_make, {:open, scriptFile, reason}}
    end
  end

  defp path_flag(flags) do
    case {member(:local, flags), member(:otp_build, flags)} do
      {true, _} ->
        :local

      {_, true} ->
        :otp_build

      {_, _} ->
        true
    end
  end

  defp get_variables(flags) do
    case get_flag(:variables, flags) do
      {:variables, variables} when is_list(variables) ->
        valid_variables(variables)

      _ ->
        []
    end
  end

  defp valid_variables([{var, path} | variables])
       when is_list(var) and is_list(path) do
    [{var, rm_tlsl(path)} | valid_variables(variables)]
  end

  defp valid_variables([{var, path} | variables])
       when is_atom(var) and is_list(path) do
    [
      {to_list(var), rm_tlsl(path)}
      | valid_variables(variables)
    ]
  end

  defp valid_variables([_ | variables]) do
    valid_variables(variables)
  end

  defp valid_variables(_) do
    []
  end

  defp rm_tlsl(p) do
    rm_tlsl1(reverse(p))
  end

  defp rm_tlsl1([?/ | p]) do
    rm_tlsl1(p)
  end

  defp rm_tlsl1(p) do
    reverse(p)
  end

  defp create_start_appls(appls) do
    included =
      append(
        map(
          fn {_, a} ->
            r_application(a, :includes)
          end,
          appls
        )
      )

    create_start_appls(appls, included)
  end

  defp create_start_appls([{_, a} | t], incl) do
    app = r_application(a, :name)

    case :lists.member(app, incl) do
      false when r_application(a, :type) == :none ->
        create_start_appls(t, incl)

      false when r_application(a, :type) == :load ->
        create_start_appls(t, incl)

      false ->
        [
          {:apply, {:application, :start_boot, [app, r_application(a, :type)]}}
          | create_start_appls(t, incl)
        ]

      _ ->
        create_start_appls(t, incl)
    end
  end

  defp create_start_appls([], _) do
    []
  end

  defp create_load_appls([{{:kernel, _}, _} | t]) do
    create_load_appls(t)
  end

  defp create_load_appls([{_, a} | t]) when r_application(a, :type) == :none do
    create_load_appls(t)
  end

  defp create_load_appls([{_, a} | t]) do
    [
      {:apply, {:application, :load, [pack_app(a)]}}
      | create_load_appls(t)
    ]
  end

  defp create_load_appls([]) do
    [{:progress, :applications_loaded}]
  end

  defp script_end(false) do
    [{:apply, {:c, :erlangrc, []}}, {:progress, :started}]
  end

  defp script_end(true) do
    [{:progress, :started}]
  end

  defp sort_appls(appls) do
    {:ok, sort_appls(appls, [], [], [])}
  end

  defp sort_appls([{n, a} | t], missing, circular, visited) do
    {name, _Vsn} = n

    {uses, t1, notFnd1} =
      find_all(name, :lists.reverse(r_application(a, :uses)), t, visited, [], [])

    {incs, t2, notFnd2} =
      find_all(name, :lists.reverse(r_application(a, :includes)), t1, visited, [], [])

    missing1 = notFnd1 ++ notFnd2 ++ missing

    case uses ++ incs do
      [] ->
        [{n, a} | sort_appls(t, missing1, circular, [n | visited])]

      l ->
        newCircular =
          for {n1, _} <- l, n2 <- visited, n1 == n2 do
            n1
          end

        circular1 =
          case newCircular do
            [] ->
              circular

            _ ->
              [n | newCircular] ++ circular
          end

        apps = del_apps(newCircular, l ++ [{n, a} | t2])
        sort_appls(apps, missing1, circular1, [n | visited])
    end
  end

  defp sort_appls([], [], [], _) do
    []
  end

  defp sort_appls([], missing, [], _) do
    throw({:error, {:undefined_applications, make_set(missing)}})
  end

  defp sort_appls([], [], circular, _) do
    throw({:error, {:circular_dependencies, make_set(circular)}})
  end

  defp sort_appls([], missing, circular, _) do
    throw(
      {:error,
       {:apps,
        [
          {:circular_dependencies, make_set(circular)},
          {:undefined_applications, make_set(missing)}
        ]}}
    )
  end

  defp find_all(checkingApp, [name | t], l, visited, found, notFound) do
    case find_app(name, l) do
      {:value, app} ->
        {_A, r} = app

        case :lists.member(checkingApp, r_application(r, :includes)) do
          true ->
            case :lists.keymember(name, 1, visited) do
              true ->
                find_all(checkingApp, t, l, visited, found, notFound)

              false ->
                find_all(checkingApp, t, l, visited, found, [name | notFound])
            end

          false ->
            find_all(checkingApp, t, l -- [app], visited, [app | found], notFound)
        end

      false ->
        case :lists.keymember(name, 1, visited) do
          true ->
            find_all(checkingApp, t, l, visited, found, notFound)

          false ->
            find_all(checkingApp, t, l, visited, found, [name | notFound])
        end
    end
  end

  defp find_all(_CheckingApp, [], l, _Visited, found, notFound) do
    {found, l, notFound}
  end

  defp find_app(name, [{{name, vsn}, application} | _]) do
    {:value, {{name, vsn}, application}}
  end

  defp find_app(name, [_ | t]) do
    find_app(name, t)
  end

  defp find_app(_Name, []) do
    false
  end

  defp del_apps([name | t], l) do
    del_apps(t, :lists.keydelete(name, 1, l))
  end

  defp del_apps([], l) do
    l
  end

  defp create_path(appls, pathFlag, variables) do
    make_set(
      map(
        fn {{name, vsn}, app} ->
          cr_path(name, vsn, app, pathFlag, variables)
        end,
        appls
      )
    )
  end

  defp cr_path(name, vsn, _, true, []) do
    :filename.join(['$ROOT', 'lib', to_list(name) ++ '-' ++ vsn, 'ebin'])
  end

  defp cr_path(name, vsn, app, true, variables) do
    dir = r_application(app, :dir)
    n = to_list(name)
    tail = [n ++ '-' ++ vsn, 'ebin']

    case variable_dir(dir, n, vsn, variables) do
      {:ok, varDir} ->
        :filename.join([varDir] ++ tail)

      _ ->
        :filename.join(['$ROOT', 'lib'] ++ tail)
    end
  end

  defp cr_path(name, _, _, :otp_build, _) do
    :filename.join(['$ROOT', 'lib', to_list(name), 'ebin'])
  end

  defp cr_path(_, _, app, _, _) do
    :filename.absname(r_application(app, :dir))
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

      _ ->
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

    case reverse(dir) do
      [['ebin', ^name] | d] ->
        {:ok, reverse(d)}

      [['ebin', ^fullName] | d] ->
        {:ok, reverse(d)}

      _ ->
        false
    end
  end

  defp create_mandatory_path(appls, pathFlag, variables) do
    dirs = [:kernel, :stdlib]

    make_set(
      map(
        fn {{name, vsn}, a} ->
          case :lists.member(name, dirs) do
            true ->
              cr_path(name, vsn, a, pathFlag, variables)

            _ ->
              ''
          end
        end,
        appls
      )
    )
  end

  defp load_appl_mods([{{name, vsn}, a} | appls], mand, pathFlag, variables) do
    mods = r_application(a, :modules)

    load_commands(
      filter(
        fn mod ->
          not member(mod, mand)
        end,
        mods
      ),
      cr_path(name, vsn, a, pathFlag, variables)
    ) ++ load_appl_mods(appls, mand, pathFlag, variables)
  end

  defp load_appl_mods([], _, _, _) do
    [{:progress, :modules_loaded}]
  end

  defp load_commands(mods, path) do
    [{:path, [:filename.join([path])]}, {:primLoad, :lists.sort(mods)}]
  end

  def pack_app(
        r_application(
          name: name,
          vsn: v,
          id: id,
          description: d,
          modules: m,
          uses: app,
          includes: incs,
          regs: regs,
          mod: mod,
          start_phases: sF,
          env: env,
          maxT: maxT,
          maxP: maxP
        )
      ) do
    {:application, name,
     [
       [
         {:description, d},
         {:vsn, v},
         {:id, id},
         {:modules, m},
         {:registered, regs},
         {:applications, app},
         {:included_applications, incs},
         {:env, env},
         {:maxT, maxT},
         {:maxP, maxP}
       ]
       | behave([{:start_phases, sF}, {:mod, mod}])
     ]}
  end

  defp behave([{:mod, []} | t]) do
    behave(t)
  end

  defp behave([{:start_phases, :undefined} | t]) do
    behave(t)
  end

  defp behave([h | t]) do
    [h | behave(t)]
  end

  defp behave([]) do
    []
  end

  defp mandatory_modules() do
    [
      :error_handler,
      :application,
      :application_controller,
      :application_master,
      :code,
      :code_server,
      :erl_eval,
      :erl_lint,
      :erl_parse,
      :error_logger,
      :ets,
      :file,
      :filename,
      :file_server,
      :file_io_server,
      :gen,
      :gen_event,
      :gen_server,
      :heart,
      :kernel,
      :logger,
      :logger_filters,
      :logger_server,
      :logger_backend,
      :logger_config,
      :logger_simple_h,
      :lists,
      :proc_lib,
      :supervisor
    ]
  end

  def preloaded() do
    :lists.sort(
      [] ++
        [
          :atomics,
          :counters,
          :erl_init,
          :erl_prim_loader,
          :erl_tracer,
          :erlang,
          :erts_code_purger,
          :erts_dirty_process_signal_handler,
          :erts_internal,
          :erts_literal_area_collector,
          :init,
          :persistent_term,
          :prim_buffer,
          :prim_eval,
          :prim_file,
          :prim_inet,
          :prim_zip,
          :zlib
        ]
    )
  end

  defp erts_binary_filter() do
    cmds = ['typer', 'dialyzer', 'ct_run', 'yielding_c_fun', 'erlc']

    case :os.type() do
      {:unix, _} ->
        cmds

      {:win32, _} ->
        for cmd <- cmds do
          [cmd, '.exe']
        end
    end
  end

  defp kernel_processes() do
    [
      {:heart, :heart, :start, []},
      {:logger, :logger_server, :start_link, []},
      {:application_controller, :application_controller, :start,
       fn appls ->
         [{_, app}] =
           filter(
             fn
               {{:kernel, _}, _App} ->
                 true

               _ ->
                 false
             end,
             appls
           )

         [pack_app(app)]
       end}
    ]
  end

  defp create_kernel_procs(appls) do
    map(
      fn
        {name, mod, func, args} when is_function(args) ->
          {:kernelProcess, name, {mod, func, args.(appls)}}

        {name, mod, func, args} ->
          {:kernelProcess, name, {mod, func, args}}
      end,
      kernel_processes()
    ) ++ [{:progress, :init_kernel_started}]
  end

  defp mk_tar(relName, release, appls, flags, path1) do
    tarName =
      case get_outdir(flags) do
        '' ->
          relName ++ '.tar.gz'

        outDir ->
          :filename.join(outDir, :filename.basename(relName)) ++ '.tar.gz'
      end

    tar = open_main_tar(tarName)

    case (try do
            mk_tar(tar, relName, release, appls, flags, path1)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, error} ->
        _ = del_tar(tar, tarName)
        {:error, :systools_make, error}

      {:EXIT, reason} ->
        _ = del_tar(tar, tarName)
        {:error, :systools_make, reason}

      _ ->
        case :erl_tar.close(tar) do
          :ok ->
            :ok

          {:error, reason} ->
            {:error, :systools_make, {:close, tarName, reason}}
        end
    end
  end

  defp open_main_tar(tarName) do
    case (try do
            open_tar(tarName)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, error} ->
        throw({:error, :systools_make, error})

      tar ->
        tar
    end
  end

  defp mk_tar(tar, relName, release, appls, flags, path1) do
    variables = get_variables(flags)
    add_applications(appls, tar, variables, flags, false)
    add_variable_tars(variables, appls, tar, flags)
    add_system_files(tar, relName, release, path1)
    add_erts_bin(tar, release, flags)
    add_additional_files(tar, flags)
  end

  defp add_additional_files(tar, flags) do
    case get_flag(:extra_files, flags) do
      {:extra_files, toAdd} ->
        for {from, to} <- toAdd do
          add_to_tar(tar, from, to)
        end

      _ ->
        :ok
    end
  end

  defp add_applications(appls, tar, variables, flags, var) do
    res =
      foldl(
        fn {{name, vsn}, app}, errs ->
          case (try do
                  add_appl(to_list(name), vsn, app, tar, variables, flags, var)
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            :ok ->
              errs

            {:error, what} ->
              [{:error_add_appl, {name, what}} | errs]
          end
        end,
        [],
        appls
      )

    case res do
      [] ->
        :ok

      errors ->
        throw({:error, errors})
    end
  end

  defp add_variable_tars([variable | variables], appls, tar, flags) do
    add_variable_tar(variable, appls, tar, flags)
    add_variable_tars(variables, appls, tar, flags)
  end

  defp add_variable_tars([], _, _, _) do
    :ok
  end

  defp add_variable_tar({variable, p}, appls, tar, flags) do
    case var_tar_flag(flags) do
      :omit ->
        :ok

      flag ->
        tarName = variable ++ '.tar.gz'
        varTar = open_tar(tarName)

        case (try do
                add_applications(appls, varTar, [{variable, p}], flags, variable)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          :ok when flag == :include ->
            close_tar(varTar, tarName)
            add_to_tar(tar, tarName, tarName)
            del_file(tarName)

          :ok when flag == :ownfile ->
            close_tar(varTar, tarName)

          error ->
            _ = del_tar(varTar, tarName)
            throw(error)
        end
    end
  end

  defp var_tar_flag(flags) do
    case get_flag(:var_tar, flags) do
      {:var_tar, flag} ->
        case member(flag, [:include, :ownfile, :omit]) do
          true ->
            flag

          _ ->
            :include
        end

      _ ->
        :include
    end
  end

  defp add_system_files(tar, relName, release, path1) do
    sVsn = r_release(release, :vsn)
    relName0 = :filename.basename(relName)
    relVsnDir = :filename.join('releases', sVsn)
    add_to_tar(tar, relName ++ '.rel', :filename.join('releases', relName0 ++ '.rel'))
    add_to_tar(tar, relName ++ '.rel', :filename.join(relVsnDir, relName0 ++ '.rel'))

    path =
      case :filename.dirname(relName) do
        '.' ->
          ['.' | path1]

        relDir ->
          [[relDir, '.'] | path1]
      end

    case lookup_file('start.boot', path) do
      false ->
        case lookup_file(relName0 ++ '.boot', path) do
          false ->
            throw({:error, {:tar_error, {:add, :boot, relName, :enoent}}})

          boot ->
            add_to_tar(tar, boot, :filename.join(relVsnDir, 'start.boot'))
        end

      boot ->
        add_to_tar(tar, boot, :filename.join(relVsnDir, 'start.boot'))
    end

    case lookup_file('relup', path) do
      false ->
        :ignore

      relup ->
        check_relup(relup)
        add_to_tar(tar, relup, :filename.join(relVsnDir, 'relup'))
    end

    case lookup_file('sys.config.src', path) do
      false ->
        case lookup_file('sys.config', path) do
          false ->
            :ignore

          sys ->
            check_sys_config(sys)
            add_to_tar(tar, sys, :filename.join(relVsnDir, 'sys.config'))
        end

      sysSrc ->
        add_to_tar(tar, sysSrc, :filename.join(relVsnDir, 'sys.config.src'))
    end

    :ok
  end

  defp lookup_file(name, [dir | path]) do
    file = :filename.join(dir, name)

    case :filelib.is_file(file) do
      true ->
        file

      false ->
        lookup_file(name, path)
    end
  end

  defp lookup_file(_Name, []) do
    false
  end

  defp check_relup(file) do
    case :file.consult(file) do
      {:ok, [{vsn, upFrom, downTo}]}
      when is_list(vsn) and
             is_integer(hd(vsn)) and
             is_list(upFrom) and
             is_list(downTo) ->
        :ok

      {:ok, _} ->
        throw({:error, {:tar_error, {:add, 'relup', [:invalid_format]}}})

      other ->
        throw({:error, {:tar_error, {:add, 'relup', [other]}}})
    end
  end

  defp check_sys_config(file) do
    case :file.consult(file) do
      {:ok, [sysConfig]} ->
        case :lists.all(
               fn
                 {app, keyVals}
                 when is_atom(app) and is_list(keyVals) ->
                   true

                 otherConfig
                 when is_list(otherConfig) and
                        is_integer(hd(otherConfig)) ->
                   true

                 _ ->
                   false
               end,
               sysConfig
             ) do
          true ->
            :ok

          false ->
            throw({:error, {:tar_error, {:add, 'sys.config', [:invalid_format]}}})
        end

      {:ok, _} ->
        throw({:error, {:tar_error, {:add, 'sys.config', [:invalid_format]}}})

      other ->
        throw({:error, {:tar_error, {:add, 'sys.config', [other]}}})
    end
  end

  defp add_appl(name, vsn, app, tar, variables, flags, var) do
    appDir = r_application(app, :dir)

    case add_to(appDir, name, vsn, variables, var) do
      false ->
        :ok

      {:ok, toDir} ->
        aDir = appDir(appDir)
        add_priv(aDir, toDir, tar)

        case get_flag(:dirs, flags) do
          {:dirs, dirs} ->
            add_dirs(aDir, dirs, toDir, tar)

          _ ->
            :ok
        end

        binDir = :filename.join(toDir, 'ebin')

        add_to_tar(
          tar,
          :filename.join(appDir, name ++ '.app'),
          :filename.join(binDir, name ++ '.app')
        )

        add_modules(
          map(
            fn mod ->
              to_list(mod)
            end,
            r_application(app, :modules)
          ),
          tar,
          appDir,
          binDir,
          :code.objfile_extension()
        )
    end
  end

  defp add_to(appDir, name, vsn, variables, variable) do
    case var_dir(appDir, name, vsn, variables) do
      {:ok, ^variable, restPath} ->
        {:ok, :filename.join(restPath ++ [name ++ '-' ++ vsn])}

      {:ok, _, _} ->
        false

      _ when variable == false ->
        {:ok, :filename.join('lib', name ++ '-' ++ vsn)}

      _ ->
        false
    end
  end

  defp var_dir(dir, name, vsn, [{var, path} | variables]) do
    case :lists.prefix(path, dir) do
      true ->
        d0 = strip_prefix(path, dir)

        case strip_name_ebin(d0, name, vsn) do
          {:ok, d} ->
            {:ok, var, d}

          _ ->
            false
        end

      _ ->
        var_dir(dir, name, vsn, variables)
    end
  end

  defp var_dir(_Dir, _, _, []) do
    false
  end

  defp appDir(appDir) do
    case :filename.basename(appDir) do
      'ebin' ->
        :filename.dirname(appDir)

      _ ->
        appDir
    end
  end

  defp add_modules(modules, tar, appDir, toDir, ext) do
    foreach(
      fn mod ->
        add_to_tar(tar, :filename.join(appDir, mod ++ ext), :filename.join(toDir, mod ++ ext))
      end,
      modules
    )
  end

  defp add_dirs(appDir, dirs, toDir, tar) do
    foreach(
      fn dir ->
        try do
          add_dir(appDir, to_list(dir), toDir, tar)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end
      end,
      dirs
    )
  end

  defp add_dir(topDir, dir, toDir, tar) do
    fromD = :filename.join(topDir, dir)

    case dirp(fromD) do
      true ->
        add_to_tar(tar, fromD, :filename.join(toDir, dir))

      _ ->
        :ok
    end
  end

  defp add_priv(aDir, toDir, tar) do
    priv = :filename.join(aDir, 'priv')

    case dirp(priv) do
      true ->
        add_to_tar(tar, priv, :filename.join(toDir, 'priv'))

      _ ->
        :ok
    end
  end

  defp add_erts_bin(tar, release, flags) do
    case {get_flag(:erts, flags), member(:erts_all, flags)} do
      {{:erts, ertsDir}, true} ->
        add_erts_bin(tar, release, ertsDir, [])

      {{:erts, ertsDir}, false} ->
        add_erts_bin(tar, release, ertsDir, erts_binary_filter())

      _ ->
        :ok
    end
  end

  defp add_erts_bin(tar, release, ertsDir, filters) do
    flattenedFilters =
      for filter <- filters do
        :filename.flatten(filter)
      end

    eVsn = r_release(release, :erts_vsn)
    fromDir = :filename.join([to_list(ertsDir), 'erts-' ++ eVsn, 'bin'])
    toDir = :filename.join('erts-' ++ eVsn, 'bin')
    {:ok, bins} = :file.list_dir(fromDir)

    for bin <- bins,
        not :lists.member(bin, flattenedFilters) do
      add_to_tar(tar, :filename.join(fromDir, bin), :filename.join(toDir, bin))
    end

    :ok
  end

  defp open_tar(tarName) do
    case :erl_tar.open(tarName, [:write, :compressed]) do
      {:ok, tar} ->
        tar

      {:error, error} ->
        throw({:error, {:tar_error, {:open, tarName, error}}})
    end
  end

  defp close_tar(tar, file) do
    case :erl_tar.close(tar) do
      :ok ->
        :ok

      {:error, reason} ->
        throw({:error, {:close, file, reason}})
    end
  end

  defp del_tar(tar, tarName) do
    _ = :erl_tar.close(tar)
    :file.delete(tarName)
  end

  defp add_to_tar(tar, fromFile, toFile) do
    case (try do
            :erl_tar.add(tar, fromFile, toFile, [:compressed, :dereference])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :ok ->
        :ok

      {:EXIT, reason} ->
        throw({:error, {:tar_error, {:add, fromFile, reason}}})

      {:error, error} ->
        throw({:error, {:tar_error, {:add, fromFile, error}}})
    end
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

  defp to_list(a) when is_atom(a) do
    :erlang.atom_to_list(a)
  end

  defp to_list(l) do
    l
  end

  defp mk_path(path0) do
    path1 =
      map(
        fn
          dir when is_atom(dir) ->
            :erlang.atom_to_list(dir)

          dir ->
            dir
        end,
        path0
      )

    :systools_lib.get_path(path1)
  end

  defp duplicates(x) do
    duplicates(keysort(1, x), [])
  end

  defp duplicates([[h1, h2] | t], l) do
    case {:erlang.element(1, h1), :erlang.element(1, h2)} do
      {x, x} ->
        duplicates([h2 | t], [{h1, h2} | l])

      _ ->
        duplicates([h2 | t], l)
    end
  end

  defp duplicates(_, l) do
    l
  end

  defp read_file(file, path) do
    case :file.path_open(path, file, [:read]) do
      {:ok, stream, fullName} ->
        return =
          case :systools_lib.read_term_from_stream(
                 stream,
                 file
               ) do
            {:ok, term} ->
              {:ok, term, fullName}

            other ->
              other
          end

        case :file.close(stream) do
          :ok ->
            return

          {:error, error} ->
            {:error, {:close, file, error}}
        end

      _Other ->
        {:error, {:not_found, file}}
    end
  end

  defp del_file(file) do
    case :file.delete(file) do
      :ok ->
        :ok

      {:error, error} ->
        throw({:error, {:delete, file, error}})
    end
  end

  defp dirp(dir) do
    case :file.read_file_info(dir) do
      {:ok, fileInfo} ->
        r_file_info(fileInfo, :type) == :directory

      _ ->
        false
    end
  end

  defp create_include_path(appls, path) do
    foundAppDirs =
      map(
        fn {_, a} ->
          r_application(a, :dir)
        end,
        appls
      )

    map(
      fn dir ->
        case reverse(:filename.split(dir)) do
          ['ebin' | d] ->
            :filename.join(reverse(d) ++ ['include'])

          _ ->
            :filename.join(dir, 'include')
        end
      end,
      foundAppDirs ++ no_dupl(path, foundAppDirs)
    )
  end

  defp no_dupl([dir | path], foundAppDirs) do
    case member(dir, foundAppDirs) do
      true ->
        no_dupl(path, foundAppDirs)

      _ ->
        [dir | no_dupl(path, foundAppDirs)]
    end
  end

  defp no_dupl([], _) do
    []
  end

  defp is_app_type(:permanent) do
    true
  end

  defp is_app_type(:transient) do
    true
  end

  defp is_app_type(:temporary) do
    true
  end

  defp is_app_type(:none) do
    true
  end

  defp is_app_type(:load) do
    true
  end

  defp is_app_type(_) do
    false
  end

  defp string_p(s) do
    case :unicode.characters_to_list(s) do
      ^s ->
        true

      _ ->
        false
    end
  end

  defp t_list_p([{a, _} | t]) when is_atom(a) do
    t_list_p(t)
  end

  defp t_list_p([]) do
    true
  end

  defp t_list_p(_) do
    false
  end

  defp a_list_p([a | t]) when is_atom(a) do
    a_list_p(t)
  end

  defp a_list_p([]) do
    true
  end

  defp a_list_p(_) do
    false
  end

  defp get_flag(f, [{f, d} | _]) do
    {f, d}
  end

  defp get_flag(f, [_ | fs]) do
    get_flag(f, fs)
  end

  defp get_flag(_, _) do
    false
  end

  defp check_args_script(args) do
    cas(args, [])
  end

  defp cas([], x) do
    x
  end

  defp cas([{:path, p} | args], x) when is_list(p) do
    case check_path(p) do
      :ok ->
        cas(args, x)

      :error ->
        cas(args, x ++ [{:path, p}])
    end
  end

  defp cas([:silent | args], x) do
    cas(args, x)
  end

  defp cas([:local | args], x) do
    cas(args, x)
  end

  defp cas([:src_tests | args], x) do
    cas(args, x)
  end

  defp cas([{:variables, v} | args], x) when is_list(v) do
    case check_vars(v) do
      :ok ->
        cas(args, x)

      :error ->
        cas(args, x ++ [{:variables, v}])
    end
  end

  defp cas([:exref | args], x) do
    cas(args, x)
  end

  defp cas([{:exref, apps} | args], x)
       when is_list(apps) do
    case check_apps(apps) do
      :ok ->
        cas(args, x)

      :error ->
        cas(args, x ++ [{:exref, apps}])
    end
  end

  defp cas([{:outdir, dir} | args], x) when is_list(dir) do
    cas(args, x)
  end

  defp cas([:otp_build | args], x) do
    cas(args, x)
  end

  defp cas([:warnings_as_errors | args], x) do
    cas(args, x)
  end

  defp cas([:no_warn_sasl | args], x) do
    cas(args, x)
  end

  defp cas([:no_module_tests | args], x) do
    cas(args, x)
  end

  defp cas([:no_dot_erlang | args], x) do
    cas(args, x)
  end

  defp cas([{:script_name, name} | args], x)
       when is_list(name) do
    cas(args, x)
  end

  defp cas([y | args], x) do
    cas(args, x ++ [y])
  end

  defp check_args_tar(args) do
    cat(args, [])
  end

  defp cat([], x) do
    x
  end

  defp cat([{:path, p} | args], x) when is_list(p) do
    case check_path(p) do
      :ok ->
        cat(args, x)

      :error ->
        cat(args, x ++ [{:path, p}])
    end
  end

  defp cat([:silent | args], x) do
    cat(args, x)
  end

  defp cat([{:dirs, d} | args], x) do
    case check_dirs(d) do
      :ok ->
        cat(args, x)

      :error ->
        cat(args, x ++ [{:dirs, d}])
    end
  end

  defp cat([{:erts, e} | args], x) when is_list(e) do
    cat(args, x)
  end

  defp cat([:erts_all | args], x) do
    cat(args, x)
  end

  defp cat([:src_tests | args], x) do
    cat(args, x)
  end

  defp cat([{:variables, v} | args], x) when is_list(v) do
    case check_vars(v) do
      :ok ->
        cat(args, x)

      :error ->
        cat(args, x ++ [{:variables, v}])
    end
  end

  defp cat([{:var_tar, vT} | args], x)
       when vT == :include or vT == :ownfile or vT == :omit do
    cat(args, x)
  end

  defp cat([:exref | args], x) do
    cat(args, x)
  end

  defp cat([{:exref, apps} | args], x)
       when is_list(apps) do
    case check_apps(apps) do
      :ok ->
        cat(args, x)

      :error ->
        cat(args, x ++ [{:exref, apps}])
    end
  end

  defp cat([{:outdir, dir} | args], x) when is_list(dir) do
    cat(args, x)
  end

  defp cat([:otp_build | args], x) do
    cat(args, x)
  end

  defp cat([:warnings_as_errors | args], x) do
    cat(args, x)
  end

  defp cat([:no_warn_sasl | args], x) do
    cat(args, x)
  end

  defp cat([:no_module_tests | args], x) do
    cat(args, x)
  end

  defp cat([{:extra_files, extraFiles} | args], x)
       when is_list(extraFiles) do
    cat(args, x)
  end

  defp cat([y | args], x) do
    cat(args, x ++ [y])
  end

  defp check_path([]) do
    :ok
  end

  defp check_path([h | t]) when is_list(h) do
    check_path(t)
  end

  defp check_path([_H | _T]) do
    :error
  end

  defp check_dirs([]) do
    :ok
  end

  defp check_dirs([h | t]) when is_atom(h) do
    check_dirs(t)
  end

  defp check_dirs([_H | _T]) do
    :error
  end

  defp check_vars([]) do
    :ok
  end

  defp check_vars([{name, dir} | t]) do
    cond do
      is_atom(name) and is_list(dir) ->
        check_vars(t)

      is_list(name) and is_list(dir) ->
        check_vars(t)

      true ->
        :error
    end
  end

  defp check_vars(_) do
    :error
  end

  defp check_apps([]) do
    :ok
  end

  defp check_apps([h | t]) when is_atom(h) do
    check_apps(t)
  end

  defp check_apps(_) do
    :error
  end

  def format_error(:badly_formatted_release) do
    :io_lib.format('Syntax error in the release file~n', [])
  end

  def format_error({:illegal_name, name}) do
    :io_lib.format('Illegal name (~tp) in the release file~n', [name])
  end

  def format_error({:illegal_form, form}) do
    :io_lib.format('Illegal tag in the release file: ~tp~n', [form])
  end

  def format_error({:missing_parameter, par}) do
    :io_lib.format('Missing parameter (~p) in the release file~n', [par])
  end

  def format_error({:illegal_applications, names}) do
    :io_lib.format('Illegal applications in the release file: ~p~n', [names])
  end

  def format_error({:missing_mandatory_app, name}) do
    :io_lib.format('Mandatory application ~w must be specified in the release file~n', [name])
  end

  def format_error({:mandatory_app, name, type}) do
    :io_lib.format(
      'Mandatory application ~w must be of type \'permanent\' in the release file. Is \'~p\'.~n',
      [name, type]
    )
  end

  def format_error({:duplicate_register, dups}) do
    :io_lib.format(
      'Duplicated register names: ~n~ts',
      [
        map(
          fn {{reg, app1, _, _}, {reg, app2, _, _}} ->
            :io_lib.format('\t~tw registered in ~w and ~w~n', [reg, app1, app2])
          end,
          dups
        )
      ]
    )
  end

  def format_error({:undefined_applications, apps}) do
    :io_lib.format('Undefined applications: ~p~n', [apps])
  end

  def format_error({:duplicate_modules, dups}) do
    :io_lib.format(
      'Duplicated modules: ~n~ts',
      [
        map(
          fn {{mod, app1, _}, {mod, app2, _}} ->
            :io_lib.format('\t~w specified in ~w and ~w~n', [mod, app1, app2])
          end,
          dups
        )
      ]
    )
  end

  def format_error({:included_and_used, dups}) do
    :io_lib.format('Applications both used and included: ~p~n', [dups])
  end

  def format_error({:duplicate_include, dups}) do
    :io_lib.format(
      'Duplicated application included: ~n~ts',
      [
        map(
          fn {{name, app1, _, _}, {name, app2, _, _}} ->
            :io_lib.format('\t~w included in ~w and ~w~n', [name, app1, app2])
          end,
          dups
        )
      ]
    )
  end

  def format_error({:modules, modErrs}) do
    format_errors(modErrs)
  end

  def format_error({:circular_dependencies, apps}) do
    :io_lib.format('Circular dependencies among applications: ~p~n', [apps])
  end

  def format_error({:not_found, file}) do
    :io_lib.format('File not found: ~tp~n', [file])
  end

  def format_error({:parse, file, {line, mod, what}}) do
    str = mod.format_error(what)
    :io_lib.format('~ts:~w: ~ts\n', [file, line, str])
  end

  def format_error({:read, file}) do
    :io_lib.format('Cannot read ~tp~n', [file])
  end

  def format_error({:open, file, error}) do
    :io_lib.format('Cannot open ~tp - ~ts~n', [file, :file.format_error(error)])
  end

  def format_error({:close, file, error}) do
    :io_lib.format('Cannot close ~tp - ~ts~n', [file, :file.format_error(error)])
  end

  def format_error({:delete, file, error}) do
    :io_lib.format('Cannot delete ~tp - ~ts~n', [file, :file.format_error(error)])
  end

  def format_error({:tar_error, what}) do
    form_tar_err(what)
  end

  def format_error({:warnings_treated_as_errors, warnings}) do
    :io_lib.format(
      'Warnings being treated as errors:~n~ts',
      [
        map(
          fn w ->
            form_warn('', w)
          end,
          warnings
        )
      ]
    )
  end

  def format_error(listOfErrors) when is_list(listOfErrors) do
    format_errors(listOfErrors)
  end

  def format_error(e) do
    :io_lib.format('~tp~n', [e])
  end

  defp format_errors(listOfErrors) do
    map(
      fn
        {:error, e} ->
          form_err(e)

        e ->
          form_err(e)
      end,
      listOfErrors
    )
  end

  defp form_err({:bad_application_name, {name, found}}) do
    :io_lib.format('~p: Mismatched application id: ~p~n', [name, found])
  end

  defp form_err({:error_reading, {name, what}}) do
    :io_lib.format('~p: ~ts~n', [name, form_reading(what)])
  end

  defp form_err({:module_not_found, app, mod}) do
    :io_lib.format('~w: Module (~w) not found~n', [app, mod])
  end

  defp form_err({:error_add_appl, {name, {:tar_error, what}}}) do
    :io_lib.format('~p: ~ts~n', [name, form_tar_err(what)])
  end

  defp form_err(e) do
    :io_lib.format('~tp~n', [e])
  end

  defp form_reading({:not_found, file}) do
    :io_lib.format('File not found: ~tp~n', [file])
  end

  defp form_reading({:application_vsn, {name, vsn}}) do
    :io_lib.format('Application ~ts with version ~tp not found~n', [name, vsn])
  end

  defp form_reading({:parse, file, {line, mod, what}}) do
    str = mod.format_error(what)
    :io_lib.format('~ts:~w: ~ts\n', [file, line, str])
  end

  defp form_reading({:read, file}) do
    :io_lib.format('Cannot read ~tp~n', [file])
  end

  defp form_reading({{:bad_param, p}, _}) do
    :io_lib.format('Bad parameter in .app file: ~tp~n', [p])
  end

  defp form_reading({{:missing_param, p}, _}) do
    :io_lib.format('Missing parameter in .app file: ~p~n', [p])
  end

  defp form_reading({:badly_formatted_application, _}) do
    :io_lib.format('Syntax error in .app file~n', [])
  end

  defp form_reading({:override_include, apps}) do
    :io_lib.format('Tried to include not (in .app file) specified applications: ~p~n', [apps])
  end

  defp form_reading({:no_valid_version, {{_, sVsn}, {_, file, fVsn}}}) do
    :io_lib.format(
      'No valid version (~tp) of .app file found. Found file ~tp with version ~tp~n',
      [sVsn, file, fVsn]
    )
  end

  defp form_reading({:parse_error, {file, line, error}}) do
    :io_lib.format('Parse error in file: ~tp.  Line: ~w  Error: ~tp; ~n', [file, line, error])
  end

  defp form_reading(w) do
    :io_lib.format('~tp~n', [w])
  end

  defp form_tar_err({:open, file, error}) do
    :io_lib.format('Cannot open tar file ~ts - ~ts~n', [file, :erl_tar.format_error(error)])
  end

  defp form_tar_err({:add, :boot, relName, :enoent}) do
    :io_lib.format(
      'Cannot find file start.boot or ~ts to add to tar file - ~ts~n',
      [relName, :erl_tar.format_error(:enoent)]
    )
  end

  defp form_tar_err({:add, file, error}) do
    :io_lib.format('Cannot add file ~ts to tar file - ~ts~n', [file, :erl_tar.format_error(error)])
  end

  def format_warning(warnings) do
    map(
      fn {:warning, w} ->
        form_warn('*WARNING* ', w)
      end,
      warnings
    )
  end

  defp form_warn(prefix, {:source_not_found, {mod, app, _}}) do
    :io_lib.format('~ts~w: Source code not found: ~w.erl~n', [prefix, app, mod])
  end

  defp form_warn(
         prefix,
         {{:parse_error, file}, {_, _, app, _, _}}
       ) do
    :io_lib.format('~ts~w: Parse error: ~tp~n', [prefix, app, file])
  end

  defp form_warn(prefix, {:obj_out_of_date, {mod, app, _}}) do
    :io_lib.format('~ts~w: Object code (~w) out of date~n', [prefix, app, mod])
  end

  defp form_warn(prefix, {:exref_undef, undef}) do
    f = fn {m, f, a} ->
      :io_lib.format('~tsUndefined function ~w:~tw/~w~n', [prefix, m, f, a])
    end

    map(f, undef)
  end

  defp form_warn(prefix, :missing_sasl) do
    :io_lib.format('~tsMissing application sasl. Can not upgrade with this release~n', [prefix])
  end

  defp form_warn(prefix, what) do
    :io_lib.format('~ts~tp~n', [prefix, what])
  end
end
