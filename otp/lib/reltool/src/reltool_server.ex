defmodule :m_reltool_server do
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

  Record.defrecord(:r_state, :state,
    options: :undefined,
    parent_pid: :undefined,
    common: :undefined,
    sys: :undefined,
    old_sys: :undefined,
    status: :undefined,
    old_status: :undefined,
    app_tab: :undefined,
    old_app_tab: :undefined,
    mod_tab: :undefined,
    old_mod_tab: :undefined,
    mod_used_by_tab: :undefined
  )

  def start_link() do
    start_link([])
  end

  def start_link(options) do
    :proc_lib.start_link(:reltool_server, :init, [[{:parent, self()} | options]], :infinity, [])
  end

  def get_config(pid, inclDef, inclDeriv) do
    :reltool_utils.call(
      pid,
      {:get_config, inclDef, inclDeriv}
    )
  end

  def load_config(pid, filenameOrConfig) do
    :reltool_utils.call(
      pid,
      {:load_config, filenameOrConfig}
    )
  end

  def save_config(pid, filename, inclDef, inclDeriv) do
    :reltool_utils.call(
      pid,
      {:save_config, filename, inclDef, inclDeriv}
    )
  end

  def reset_config(pid) do
    :reltool_utils.call(pid, :reset_config)
  end

  def undo_config(pid) do
    :reltool_utils.call(pid, :undo_config)
  end

  def get_rel(pid, relName) do
    :reltool_utils.call(pid, {:get_rel, relName})
  end

  def get_script(pid, relName) do
    :reltool_utils.call(pid, {:get_script, relName})
  end

  def get_mod(pid, modName) do
    :reltool_utils.call(pid, {:get_mod, modName})
  end

  def get_app(pid, appName) do
    :reltool_utils.call(pid, {:get_app, appName})
  end

  def set_app(pid, app) do
    :reltool_utils.call(pid, {:set_app, app})
  end

  def get_apps(pid, kind) do
    :reltool_utils.call(pid, {:get_apps, kind})
  end

  def set_apps(pid, apps) do
    :reltool_utils.call(pid, {:set_apps, apps})
  end

  def get_sys(pid) do
    :reltool_utils.call(pid, :get_sys)
  end

  def set_sys(pid, sys) do
    :reltool_utils.call(pid, {:set_sys, sys})
  end

  def get_status(pid) do
    :reltool_utils.call(pid, :get_status)
  end

  def gen_rel_files(pid, dir) do
    :reltool_utils.call(pid, {:gen_rel_files, dir})
  end

  def gen_target(pid, dir) do
    :reltool_utils.call(pid, {:gen_target, dir})
  end

  def gen_spec(pid) do
    :reltool_utils.call(pid, :gen_spec)
  end

  def init([{:parent, parent} | _] = options) do
    try do
      do_init(options)
    catch
      {:error, reason} ->
        :proc_lib.init_ack(parent, {:error, reason})

      :error, reason ->
        exit({reason, __STACKTRACE__})
    end
  end

  defp do_init(options) do
    appTab =
      :ets.new(
        :reltool_apps1,
        [:public, :ordered_set, {:keypos, r_app(:name)}]
      )

    oldAppTab =
      :ets.new(
        :reltool_apps2,
        [:public, :ordered_set, {:keypos, r_app(:name)}]
      )

    modTab =
      :ets.new(
        :reltool_mods1,
        [:public, :ordered_set, {:keypos, r_mod(:name)}]
      )

    oldModTab =
      :ets.new(
        :reltool_mods2,
        [:public, :ordered_set, {:keypos, r_mod(:name)}]
      )

    modUsesTab =
      :ets.new(
        :reltool_mod_uses,
        [:public, :bag, {:keypos, 1}]
      )

    s =
      r_state(
        options: options,
        app_tab: appTab,
        old_app_tab: oldAppTab,
        mod_tab: modTab,
        old_mod_tab: oldModTab,
        mod_used_by_tab: modUsesTab
      )

    s2 = parse_options(s)
    {s3, apps, status2} = refresh(s2)
    status3 = analyse(s3, apps, status2)
    fakeBackup = {:ets.tab2list(r_state(s3, :app_tab)), :ets.tab2list(r_state(s3, :mod_tab))}
    s4 = save_old(s3, s3, fakeBackup, status3)
    r_state(parent_pid: parent, sys: sys, common: c) = s4

    :proc_lib.init_ack(
      parent,
      {:ok, self(), c, r_sys(sys, apps: :undefined)}
    )

    loop(s4)
  end

  defp parse_options(s) do
    sys = default_sys()
    c = r_common(sys_debug: [], wx_debug: 0, trap_exit: true)
    parse_options(r_state(s, :options), s, c, sys)
  end

  defp default_sys() do
    r_sys(
      root_dir: :reltool_utils.root_dir(),
      lib_dirs: :reltool_utils.erl_libs(),
      escripts: [],
      incl_cond: :derived,
      mod_cond: :all,
      apps: [],
      boot_rel: 'start_clean',
      rels: :reltool_utils.default_rels(),
      emu_name: 'beam',
      profile: :development,
      incl_sys_filters: dec_re(:incl_sys_filters, ['.*'], []),
      excl_sys_filters: dec_re(:excl_sys_filters, [], []),
      incl_app_filters: dec_re(:incl_app_filters, ['.*'], []),
      excl_app_filters: dec_re(:excl_app_filters, [], []),
      relocatable: true,
      rel_app_type: :permanent,
      embedded_app_type: :undefined,
      app_file: :keep,
      incl_archive_filters: dec_re(:incl_archive_filters, ['.*'], []),
      excl_archive_filters: dec_re(:excl_archive_filters, ['^include$', '^priv$'], []),
      archive_opts: [],
      debug_info: :keep
    )
  end

  defp dec_re(key, regexps, old) do
    :reltool_utils.decode_regexps(key, regexps, old)
  end

  defp parse_options([{key, val} | keyVals], s, c, sys) do
    case key do
      :parent ->
        parse_options(keyVals, r_state(s, parent_pid: val), c, sys)

      :sys_debug ->
        parse_options(keyVals, s, r_common(c, sys_debug: val), sys)

      :wx_debug ->
        parse_options(keyVals, s, r_common(c, wx_debug: val), sys)

      :trap_exit ->
        parse_options(keyVals, s, r_common(c, trap_exit: val), sys)

      :config ->
        sys2 = read_config(sys, val)
        parse_options(keyVals, s, c, sys2)

      :sys ->
        sys2 = read_config(sys, {:sys, val})
        parse_options(keyVals, s, c, sys2)

      _ ->
        :reltool_utils.throw_error('Illegal option: ~tp', [{key, val}])
    end
  end

  defp parse_options([], s, c, sys) do
    r_state(s, common: c, sys: sys)
  end

  defp parse_options(keyVals, _S, _C, _Sys) do
    :reltool_utils.throw_error('Illegal option: ~tp', [keyVals])
  end

  def loop(r_state(sys: sys) = s) do
    receive do
      {:system, from, msg} ->
        :sys.handle_system_msg(
          msg,
          from,
          r_state(s, :parent_pid),
          :reltool_server,
          r_common(r_state(s, :common), :sys_debug),
          s
        )

      {:call, replyTo, ref, {:get_config, inclDef, inclDeriv}} ->
        reply = do_get_config(s, inclDef, inclDeriv)
        :reltool_utils.reply(replyTo, ref, reply)
        :reltool_server.loop(s)

      {:call, replyTo, ref, {:load_config, sysConfig}} ->
        fun = fn ->
          do_load_config(s, sysConfig)
        end

        {s3, status2} = config_and_refresh(s, fun)
        :reltool_utils.reply(replyTo, ref, status2)
        :reltool_server.loop(s3)

      {:call, replyTo, ref, {:save_config, filename, inclDef, inclDeriv}} ->
        reply = do_save_config(s, filename, inclDef, inclDeriv)
        :reltool_utils.reply(replyTo, ref, reply)
        :reltool_server.loop(s)

      {:call, replyTo, ref, :reset_config} ->
        fun = fn ->
          parse_options(s)
        end

        {s3, status2} = config_and_refresh(s, fun)
        :reltool_utils.reply(replyTo, ref, status2)
        :reltool_server.loop(s3)

      {:call, replyTo, ref, :undo_config} ->
        s2 =
          r_state(s,
            sys: r_state(s, :old_sys),
            old_sys: sys,
            status: r_state(s, :old_status),
            old_status: r_state(s, :status),
            app_tab: r_state(s, :old_app_tab),
            old_app_tab: r_state(s, :app_tab),
            mod_tab: r_state(s, :old_mod_tab),
            old_mod_tab: r_state(s, :mod_tab)
          )

        :reltool_utils.reply(replyTo, ref, :ok)
        :reltool_server.loop(s2)

      {:call, replyTo, ref, {:get_rel, relName}} ->
        reply =
          case :lists.keysearch(relName, r_rel(:name), r_sys(sys, :rels)) do
            {:value, rel} ->
              :reltool_target.gen_rel(rel, sys_all_apps(s))

            false ->
              {:error, 'No such release: ' ++ relName}
          end

        :reltool_utils.reply(replyTo, ref, reply)
        :reltool_server.loop(s)

      {:call, replyTo, ref, {:get_script, relName}} ->
        reply =
          case :lists.keysearch(relName, r_rel(:name), r_sys(sys, :rels)) do
            {:value, rel} ->
              pathFlag = true
              vars = []
              :reltool_target.gen_script(rel, sys_all_apps(s), pathFlag, vars)

            false ->
              {:error, 'No such release: ' ++ relName}
          end

        :reltool_utils.reply(replyTo, ref, reply)
        :reltool_server.loop(s)

      {:call, replyTo, ref, {:get_mod, modName}} ->
        reply =
          case :ets.lookup(r_state(s, :mod_tab), modName) do
            [m] ->
              {:ok, m}

            [] ->
              {:ok, missing_mod(modName, :"*MISSING*")}
          end

        :reltool_utils.reply(replyTo, ref, reply)
        :reltool_server.loop(s)

      {:call, replyTo, ref, {:get_app, appName}}
      when is_atom(appName) ->
        reply =
          case :ets.lookup(r_state(s, :app_tab), appName) do
            [app] ->
              {:ok, app}

            [] ->
              {:error, 'No such application: ' ++ :erlang.atom_to_list(appName)}
          end

        :reltool_utils.reply(replyTo, ref, reply)
        :reltool_server.loop(s)

      {:call, replyTo, ref, {:set_app, app}} ->
        fun = fn ->
          do_set_apps(s, [app])
        end

        {s3, status2} = config_and_refresh(s, fun)

        reply =
          case status2 do
            {:ok, warnings} ->
              [app2] = :ets.lookup(r_state(s3, :app_tab), r_app(app, :name))
              {:ok, app2, warnings}

            {:error, _} ->
              status2
          end

        :reltool_utils.reply(replyTo, ref, reply)
        :reltool_server.loop(s3)

      {:call, replyTo, ref, {:get_apps, kind}} ->
        appNames =
          case kind do
            :whitelist ->
              :ets.select(
                r_state(s, :app_tab),
                [{r_app(is_pre_included: true, _: :_), [], [:"$_"]}]
              )

            :blacklist ->
              :ets.select(
                r_state(s, :app_tab),
                [{r_app(is_pre_included: false, _: :_), [], [:"$_"]}]
              )

            :source ->
              :ets.select(
                r_state(s, :app_tab),
                [
                  {r_app(is_included: :"$1", is_pre_included: :"$2", _: :_),
                   [{:"=/=", :"$1", true}, {:"=/=", :"$2", false}], [:"$_"]}
                ]
              )

            :derived ->
              :ets.select(
                r_state(s, :app_tab),
                [
                  {r_app(is_included: :"$1", is_pre_included: :"$2", _: :_),
                   [{:"=:=", :"$1", true}, {:"=/=", :"$2", true}], [:"$_"]}
                ]
              )
          end

        :reltool_utils.reply(replyTo, ref, {:ok, appNames})
        :reltool_server.loop(s)

      {:call, replyTo, ref, {:set_apps, apps}} ->
        fun = fn ->
          do_set_apps(s, apps)
        end

        {s3, status2} = config_and_refresh(s, fun)
        :reltool_utils.reply(replyTo, ref, status2)
        :reltool_server.loop(s3)

      {:call, replyTo, ref, :get_sys} ->
        :reltool_utils.reply(replyTo, ref, {:ok, r_sys(sys, apps: :undefined)})
        :reltool_server.loop(s)

      {:call, replyTo, ref, {:set_sys, sys2}} ->
        fun = fn ->
          r_state(s, sys: r_sys(sys2, apps: r_sys(sys, :apps)))
        end

        {s3, status} = config_and_refresh(s, fun)
        :reltool_utils.reply(replyTo, ref, status)
        :reltool_server.loop(s3)

      {:call, replyTo, ref, :get_status} ->
        :reltool_utils.reply(replyTo, ref, r_state(s, :status))
        :reltool_server.loop(s)

      {:call, replyTo, ref, {:gen_rel_files, dir}} ->
        status =
          case :reltool_target.gen_rel_files(
                 sys_all_apps(s),
                 dir
               ) do
            :ok ->
              {:ok, []}

            {:error, reason} ->
              {:error, reason}
          end

        :reltool_utils.reply(replyTo, ref, status)
        :reltool_server.loop(s)

      {:call, replyTo, ref, {:gen_target, dir}} ->
        reply = :reltool_target.gen_target(sys_all_apps(s), dir)
        :reltool_utils.reply(replyTo, ref, reply)
        :reltool_server.loop(s)

      {:call, replyTo, ref, :gen_spec} ->
        reply = :reltool_target.gen_spec(sys_all_apps(s))
        :reltool_utils.reply(replyTo, ref, reply)
        :reltool_server.loop(s)

      {:EXIT, pid, reason} when pid === r_state(s, :parent_pid) ->
        exit(reason)

      {:call, replyTo, ref, msg}
      when is_pid(replyTo) and
             is_reference(ref) ->
        :error_logger.format('~w~w got unexpected call:\n\t~tp\n', [:reltool_server, self(), msg])
        :reltool_utils.reply(replyTo, ref, {:error, {:invalid_call, msg}})
        :reltool_server.loop(s)

      msg ->
        :error_logger.format('~w~w got unexpected message:\n\t~tp\n', [
          :reltool_server,
          self(),
          msg
        ])

        :reltool_server.loop(s)
    end
  end

  defp do_set_apps(r_state(sys: sys) = s, changedApps) do
    sysApps = app_update_config(changedApps, r_sys(sys, :apps))
    r_state(s, sys: r_sys(sys, apps: sysApps))
  end

  defp app_update_config(
         [
           r_app(
             name: name,
             is_escript: {:inlined, escript}
           )
           | _
         ],
         _SysApps
       ) do
    :reltool_utils.throw_error(
      'Application ~w is inlined in ~w. Can not change configuration for an inlined application.',
      [name, escript]
    )
  end

  defp app_update_config([config | configs], sysApps) do
    newSysApps =
      case app_set_config_only(config) do
        {:delete, name} ->
          :lists.keydelete(name, r_app(:name), sysApps)

        new ->
          :lists.ukeymerge(r_app(:name), [new], sysApps)
      end

    app_update_config(configs, newSysApps)
  end

  defp app_update_config([], sysApps) do
    sysApps
  end

  defp app_set_config_only(r_app(mods: configMods) = config) do
    app_set_config_only(
      mod_set_config_only(configMods),
      config
    )
  end

  defp app_set_config_only(
         [],
         r_app(
           name: name,
           incl_cond: :undefined,
           mod_cond: :undefined,
           use_selected_vsn: :undefined,
           debug_info: :undefined,
           app_file: :undefined,
           app_type: :undefined,
           incl_app_filters: :undefined,
           excl_app_filters: :undefined,
           incl_archive_filters: :undefined,
           excl_archive_filters: :undefined,
           archive_opts: :undefined,
           is_escript: false
         )
       ) do
    {:delete, name}
  end

  defp app_set_config_only(
         mods,
         r_app(
           name: name,
           incl_cond: inclCond,
           mod_cond: modCond,
           use_selected_vsn: useSelectedVsn,
           debug_info: debugInfo,
           app_file: appFile,
           app_type: appType,
           incl_app_filters: inclAppFilters,
           excl_app_filters: exclAppFilters,
           incl_archive_filters: inclArchiveFilters,
           excl_archive_filters: exclArchiveFilters,
           archive_opts: archiveOpts,
           vsn: vsn,
           is_escript: isEscript,
           label: label,
           info: info,
           active_dir: activeDir,
           sorted_dirs: sortedDirs
         )
       ) do
    app =
      r_app(default_app(name),
        incl_cond: inclCond,
        mod_cond: modCond,
        use_selected_vsn: useSelectedVsn,
        debug_info: debugInfo,
        app_file: appFile,
        app_type: appType,
        incl_app_filters: inclAppFilters,
        excl_app_filters: exclAppFilters,
        incl_archive_filters: inclArchiveFilters,
        excl_archive_filters: exclArchiveFilters,
        archive_opts: archiveOpts,
        vsn: vsn,
        mods: mods
      )

    cond do
      isEscript ->
        r_app(app,
          is_escript: isEscript,
          active_dir: activeDir,
          sorted_dirs: sortedDirs,
          label: label,
          info: info
        )

      useSelectedVsn === :dir ->
        r_app(app, active_dir: activeDir, sorted_dirs: [activeDir])

      true ->
        app
    end
  end

  defp mod_set_config_only(configMods) do
    for r_mod(name: name, incl_cond: inclCond, debug_info: debugInfo) <- configMods,
        inclCond !== :undefined or debugInfo !== :undefined do
      r_mod(name: name, incl_cond: inclCond, debug_info: debugInfo)
    end
  end

  defp analyse(r_state(sys: sys) = s, apps, status) do
    relApps = apps_in_rels(r_sys(sys, :rels), apps)
    status2 = apps_init_is_included(s, apps, relApps, status)
    app_propagate_is_included(s)
    mod_propagate_is_included(s)
    propagate_is_used_by(s)
    app_recap_dependencies(s)
    verify_config(s, relApps, status2)
  end

  defp apps_in_rels(rels, apps) do
    allRelApps =
      :lists.foldl(
        fn rel, relApps ->
          moreRelApps = apps_in_rel(rel, apps)
          moreRelApps ++ relApps
        end,
        [],
        rels
      )

    :lists.reverse(allRelApps)
  end

  defp apps_in_rel(r_rel(name: relName, rel_apps: relApps), apps) do
    mandatory = [{relName, :kernel}, {relName, :stdlib}]

    explicit0 =
      for r_rel_app(name: appName) <- relApps do
        {relName, appName}
      end

    explicit = mandatory ++ explicit0

    deps =
      for rA <- relApps,
          appName <-
            (case :lists.keyfind(r_rel_app(rA, :name), r_app(:name), apps) do
               app = r_app(info: r_app_info(applications: aA)) ->
                 iA =
                   case r_rel_app(rA, :incl_apps) do
                     :undefined ->
                       r_app_info(r_app(app, :info), :incl_apps)

                     relIA ->
                       relIA
                   end

                 aA ++ iA

               false ->
                 :reltool_utils.throw_error(
                   'Release ~tp uses non existing application ~w',
                   [relName, r_rel_app(rA, :name)]
                 )
             end),
          not :lists.keymember(appName, 2, explicit) do
        {relName, appName}
      end

    more_apps_in_rels(deps, apps, explicit)
  end

  defp more_apps_in_rels([{relName, appName} = rA | relApps], apps, acc) do
    case :lists.member(rA, acc) do
      true ->
        more_apps_in_rels(relApps, apps, acc)

      false ->
        case :lists.keyfind(appName, r_app(:name), apps) do
          r_app(info: r_app_info(applications: aA, incl_apps: iA)) ->
            extra =
              for n <- aA ++ iA do
                {relName, n}
              end

            acc2 = more_apps_in_rels(extra, apps, [rA | acc])
            more_apps_in_rels(relApps, apps, acc2)

          false ->
            :reltool_utils.throw_error('Release ~tp uses non existing application ~w', [
              relName,
              appName
            ])
        end
    end
  end

  defp more_apps_in_rels([], _Apps, acc) do
    acc
  end

  defp apps_init_is_included(s, apps, relApps, status) do
    :lists.foldl(
      fn app, accStatus ->
        app_init_is_included(s, app, relApps, accStatus)
      end,
      status,
      apps
    )
  end

  defp app_init_is_included(
         r_state(app_tab: appTab, mod_tab: modTab, sys: sys),
         r_app(name: appName, mods: mods) = a,
         relApps,
         status
       ) do
    appCond =
      case r_app(a, :incl_cond) do
        :undefined ->
          r_sys(sys, :incl_cond)

        _ ->
          r_app(a, :incl_cond)
      end

    modCond =
      case r_app(a, :mod_cond) do
        :undefined ->
          r_sys(sys, :mod_cond)

        _ ->
          r_app(a, :mod_cond)
      end

    rels =
      for {relName, aN} <- relApps, aN === appName do
        relName
      end

    {default, isPreIncl, isIncl, status2} =
      case {appCond, rels} do
        {:include, _} ->
          {:undefined, true, true, status}

        {:exclude, []} ->
          {:undefined, false, false, status}

        {:exclude, [relName | _]} ->
          :reltool_utils.throw_error(
            'Application ~w is used in release ~tp and cannot be excluded',
            [appName, relName]
          )

        {:derived, []} ->
          {:undefined, :undefined, :undefined, status}

        {:derived, [_ | _]} ->
          {true, :undefined, true, status}
      end

    {mods2, status3} =
      :lists.mapfoldl(
        fn mod, acc ->
          mod_init_is_included(modTab, mod, modCond, appCond, default, acc)
        end,
        status2,
        mods
      )

    a2 = r_app(a, mods: mods2, is_pre_included: isPreIncl, is_included: isIncl, rels: rels)
    :ets.insert(appTab, a2)
    status3
  end

  defp mod_init_is_included(modTab, m, modCond, appCond, default, status) do
    isIncl =
      case appCond do
        :include ->
          case r_mod(m, :incl_cond) do
            :include ->
              true

            :exclude ->
              false

            :derived ->
              :undefined

            :undefined ->
              case modCond do
                :all ->
                  true

                :app ->
                  false_to_undefined(r_mod(m, :is_app_mod))

                :ebin ->
                  false_to_undefined(r_mod(m, :is_ebin_mod))

                :derived ->
                  default

                :none ->
                  false
              end
          end

        :exclude ->
          false

        :derived ->
          case r_mod(m, :incl_cond) do
            :include ->
              true

            :exclude ->
              false

            :derived ->
              :undefined

            :undefined ->
              default
          end
      end

    m2 = r_mod(m, is_pre_included: isIncl, is_included: isIncl)

    status2 =
      case :ets.lookup(modTab, r_mod(m, :name)) do
        [existing] ->
          case {r_mod(existing, :is_included), isIncl} do
            {false, _} ->
              :ets.insert(modTab, m2)

              :reltool_utils.add_warning(
                'Module ~w exists in applications ~w and ~w. Using module from application ~w.',
                [
                  r_mod(m, :name),
                  r_mod(existing, :app_name),
                  r_mod(m, :app_name),
                  r_mod(m, :app_name)
                ],
                status
              )

            {_, false} ->
              :reltool_utils.add_warning(
                'Module ~w exists in applications ~w and ~w. Using module from application ~w.',
                [
                  r_mod(m, :name),
                  r_mod(existing, :app_name),
                  r_mod(m, :app_name),
                  r_mod(existing, :app_name)
                ],
                status
              )

            {_, _} ->
              :reltool_utils.throw_error(
                'Module ~w potentially included by two different applications: ~w and ~w.',
                [r_mod(m, :name), r_mod(existing, :app_name), r_mod(m, :app_name)]
              )
          end

        [] ->
          :ets.insert(modTab, m2)
          status
      end

    {m2, status2}
  end

  defp false_to_undefined(bool) do
    case bool do
      false ->
        :undefined

      _ ->
        bool
    end
  end

  defp get_no_rel_apps_and_dependencies(s) do
    :ets.select(
      r_state(s, :app_tab),
      [
        {r_app(
           name: :"$1",
           is_included: true,
           info: r_app_info(applications: :"$2", incl_apps: :"$3", _: :_),
           rels: [],
           _: :_
         ), [], [{{:"$1", :"$2", :"$3"}}]}
      ]
    )
  end

  defp app_propagate_is_included(s) do
    :lists.foreach(
      fn {appName, depNames1, depNames2} ->
        app_mark_is_included(s, appName, depNames1 ++ depNames2)
      end,
      get_no_rel_apps_and_dependencies(s)
    )
  end

  defp app_mark_is_included(r_state(app_tab: appTab, mod_tab: modTab, sys: sys) = s, usedByName, [
         appName | appNames
       ]) do
    case :ets.lookup(appTab, appName) do
      [a] ->
        case r_app(a, :is_included) do
          :undefined ->
            a2 =
              case r_app(a, :incl_cond) do
                :include ->
                  r_app(a, is_pre_included: true, is_included: true)

                :exclude ->
                  r_app(a, is_pre_included: false, is_included: false)

                appInclCond
                when appInclCond == :undefined or
                       appInclCond == :derived ->
                  r_app(a, is_included: true)
              end

            :ets.insert(appTab, a2)

            modCond =
              case r_app(a, :mod_cond) do
                :undefined ->
                  r_sys(sys, :mod_cond)

                _ ->
                  r_app(a, :mod_cond)
              end

            filter = fn m ->
              case modCond do
                :all ->
                  true

                :app ->
                  r_mod(m, :is_app_mod)

                :ebin ->
                  r_mod(m, :is_ebin_mod)

                :derived ->
                  false

                :none ->
                  false
              end
            end

            mods = :lists.filter(filter, r_app(a, :mods))

            for m <- mods do
              case r_mod(m, :is_included) do
                :undefined ->
                  m2 =
                    case r_mod(m, :incl_cond) do
                      :include ->
                        r_mod(m, is_pre_included: true, is_included: true)

                      :exclude ->
                        r_mod(m, is_pre_included: false, is_included: false)

                      modInclCond
                      when modInclCond == :undefined or
                             modInclCond == :derived ->
                        r_mod(m, is_included: true)
                    end

                  :ets.insert(modTab, m2)

                _ ->
                  :ok
              end
            end

            r_app(
              info:
                r_app_info(
                  applications: depNames1,
                  incl_apps: depNames2
                )
            ) = a

            app_mark_is_included(s, appName, depNames1 ++ depNames2)

          _ ->
            :ok
        end

      [] ->
        :reltool_utils.throw_error('Application ~tp uses non existing application ~w', [
          usedByName,
          appName
        ])
    end

    app_mark_is_included(s, usedByName, appNames)
  end

  defp app_mark_is_included(_S, _UsedByName, []) do
    :ok
  end

  defp get_all_mods_and_dependencies(s) do
    :ets.select(
      r_state(s, :mod_tab),
      [{r_mod(name: :"$1", uses_mods: :"$2", is_included: true, _: :_), [], [{{:"$1", :"$2"}}]}]
    )
  end

  defp mod_propagate_is_included(s) do
    case :lists.flatmap(
           fn {modName, usesModNames} ->
             mod_mark_is_included(s, modName, usesModNames, [])
           end,
           get_all_mods_and_dependencies(s)
         ) do
      [] ->
        :ok

      missingMods ->
        missingApp = default_app(:"*MISSING*", 'missing')

        missingApp2 =
          r_app(missingApp,
            label: '*MISSING*',
            info: missing_app_info(''),
            mods: missingMods,
            status: :missing,
            uses_mods: []
          )

        :ets.insert(r_state(s, :app_tab), missingApp2)
        :ok
    end
  end

  defp mod_mark_is_included(
         r_state(app_tab: appTab, mod_tab: modTab, sys: sys) = s,
         usedByName,
         [modName | modNames],
         acc
       ) do
    acc3 =
      case :ets.lookup(modTab, modName) do
        [m] ->
          case r_mod(m, :is_included) do
            :undefined ->
              m2 =
                case r_mod(m, :incl_cond) do
                  :include ->
                    r_mod(m, is_pre_included: true, is_included: true)

                  :exclude ->
                    r_mod(m, is_pre_included: false, is_included: false)

                  modInclCond
                  when modInclCond == :undefined or
                         modInclCond == :derived ->
                    r_mod(m, is_included: true)
                end

              :ets.insert(modTab, m2)
              [a] = :ets.lookup(appTab, r_mod(m2, :app_name))

              acc2 =
                case r_app(a, :is_included) do
                  :undefined ->
                    modCond =
                      case r_app(a, :mod_cond) do
                        :undefined ->
                          r_sys(sys, :mod_cond)

                        _ ->
                          r_app(a, :mod_cond)
                      end

                    filter = fn m3 ->
                      case modCond do
                        :all ->
                          true

                        :app ->
                          r_mod(m3, :is_app_mod)

                        :ebin ->
                          r_mod(m3, :is_ebin_mod)

                        :derived ->
                          false

                        :none ->
                          false
                      end
                    end

                    mods = :lists.filter(filter, r_app(a, :mods))
                    a2 = r_app(a, is_included: true)
                    :ets.insert(appTab, a2)

                    mod_mark_is_included(
                      s,
                      modName,
                      for m3 <- mods do
                        r_mod(m3, :name)
                      end,
                      acc
                    )

                  _ ->
                    acc
                end

              mod_mark_is_included(s, modName, r_mod(m2, :uses_mods), acc2)

            _ ->
              acc
          end

        [] ->
          m = missing_mod(modName, :"*MISSING*")
          m2 = r_mod(m, is_included: true)
          :ets.insert(modTab, m2)
          [m2 | acc]
      end

    mod_mark_is_included(s, usedByName, modNames, acc3)
  end

  defp mod_mark_is_included(_S, _UsedByName, [], acc) do
    acc
  end

  defp propagate_is_used_by(s) do
    :lists.foreach(
      fn {mod, usesMods} ->
        :lists.foreach(
          fn usedMod ->
            :ets.insert(
              r_state(s, :mod_used_by_tab),
              {usedMod, mod}
            )
          end,
          usesMods
        )
      end,
      get_all_mods_and_dependencies(s)
    )
  end

  defp app_recap_dependencies(s) do
    :ets.foldl(
      fn app, _ ->
        app_recap_dependencies(s, app)
      end,
      :ok,
      r_state(s, :app_tab)
    )
  end

  defp app_recap_dependencies(s, r_app(mods: mods, is_included: isIncl) = a) do
    {mods2, isIncl2} = mod_recap_dependencies(s, a, mods, [], isIncl)

    appStatus =
      case :lists.keymember(:missing, r_mod(:status), mods2) do
        true ->
          :missing

        false ->
          :ok
      end

    usesMods =
      for m <- mods2,
          r_mod(m, :is_included) === true do
        r_mod(m, :uses_mods)
      end

    usesMods2 = :lists.usort(:lists.flatten(usesMods))

    usesApps =
      for modName <- usesMods2,
          m <- :ets.lookup(r_state(s, :mod_tab), modName) do
        r_mod(m, :app_name)
      end

    usesApps2 = :lists.usort(usesApps)

    usedByMods =
      for m <- mods2,
          r_mod(m, :is_included) === true do
        r_mod(m, :used_by_mods)
      end

    usedByMods2 = :lists.usort(:lists.flatten(usedByMods))

    usedByApps =
      for modName <- usedByMods2,
          m <- :ets.lookup(r_state(s, :mod_tab), modName) do
        r_mod(m, :app_name)
      end

    usedByApps2 = :lists.usort(usedByApps)

    a2 =
      r_app(a,
        mods: mods2,
        status: appStatus,
        uses_mods: usesMods2,
        used_by_mods: usedByMods2,
        uses_apps: usesApps2,
        used_by_apps: usedByApps2,
        is_included: isIncl2
      )

    :ets.insert(r_state(s, :app_tab), a2)
    :ok
  end

  defp mod_recap_dependencies(s, a, [r_mod(name: modName) = m1 | mods], acc, isIncl) do
    case :ets.lookup(r_state(s, :mod_tab), modName) do
      [m2] when r_mod(m2, :app_name) === r_app(a, :name) ->
        modStatus = do_get_status(m2)

        {isIncl2, m3} =
          case r_mod(m2, :is_included) do
            true ->
              usedByMods =
                for {_, n} <-
                      :ets.lookup(
                        r_state(s, :mod_used_by_tab),
                        modName
                      ) do
                  n
                end

              {true,
               r_mod(m2,
                 status: modStatus,
                 used_by_mods: usedByMods
               )}

            _ ->
              {isIncl, r_mod(m2, status: modStatus, used_by_mods: [])}
          end

        :ets.insert(r_state(s, :mod_tab), m3)
        mod_recap_dependencies(s, a, mods, [m3 | acc], isIncl2)

      [_]
      when r_app(a, :is_included) == false or
             r_mod(m1, :incl_cond) == :exclude ->
        mod_recap_dependencies(s, a, mods, [m1 | acc], isIncl)

      [m2] ->
        :reltool_utils.throw_error(
          'Module ~w potentially included by two different applications: ~w and ~w.',
          [modName, r_app(a, :name), r_mod(m2, :app_name)]
        )
    end
  end

  defp mod_recap_dependencies(_S, _A, [], acc, isIncl) do
    {:lists.reverse(acc), isIncl}
  end

  defp do_get_status(m) do
    cond do
      r_mod(m, :exists) === false and
          r_mod(m, :is_included) !== false ->
        :missing

      true ->
        :ok
    end
  end

  defp verify_config(
         r_state(
           app_tab: appTab,
           sys: r_sys(boot_rel: bootRel, rels: rels)
         ),
         relApps,
         status
       ) do
    case :lists.keymember(bootRel, r_rel(:name), rels) do
      true ->
        status2 =
          :lists.foldl(
            fn rA, acc ->
              check_app(appTab, rA, acc)
            end,
            status,
            relApps
          )

        :lists.foldl(
          fn r_rel(name: relName), acc ->
            check_rel(relName, relApps, acc)
          end,
          status2,
          rels
        )

      false ->
        :reltool_utils.throw_error('Release ~tp is mandatory (used as boot_rel)', [bootRel])
    end
  end

  defp check_app(appTab, {relName, appName}, status) do
    case :ets.lookup(appTab, appName) do
      [r_app(is_pre_included: isPreIncl, is_included: isIncl)]
      when isPreIncl or isIncl ->
        status

      _ ->
        :reltool_utils.throw_error('Release ~tp uses non included application ~w', [
          relName,
          appName
        ])
    end
  end

  defp check_rel(relName, relApps, status) do
    ensureApp = fn appName, acc ->
      case :lists.member({relName, appName}, relApps) do
        true ->
          acc

        false ->
          :reltool_utils.throw_error('Mandatory application ~w is not included in release ~tp', [
            appName,
            relName
          ])
      end
    end

    mandatory = [:kernel, :stdlib]
    :lists.foldl(ensureApp, status, mandatory)
  end

  defp refresh_app(
         r_app(
           name: appName,
           is_escript: isEscript,
           active_dir: activeDir,
           label: optLabel,
           mods: mods,
           status: appStatus
         ) = app,
         force,
         status
       ) do
    cond do
      force or optLabel === :undefined ->
        {appInfo, ebinMods, status3} =
          case isEscript do
            false ->
              base = get_base(appName, activeDir)

              defaultVsn =
                get_vsn_from_dir(
                  appName,
                  base
                )

              ebin = :filename.join([activeDir, 'ebin'])
              appFile = :filename.join([ebin, :erlang.atom_to_list(appName) ++ '.app'])

              {aI, status2} =
                read_app_info(
                  appFile,
                  appFile,
                  appName,
                  activeDir,
                  appStatus,
                  defaultVsn,
                  status
                )

              {aI, read_ebin_mods(ebin, appName), status2}

            _ ->
              {r_app(app, :info), mods, status}
          end

        appInfoMods = :lists.usort(r_app_info(appInfo, :modules))

        status4 =
          case r_app_info(appInfo, :modules) -- appInfoMods do
            [] ->
              status3

            duplicatedMods ->
              :lists.foldl(
                fn m, s ->
                  :reltool_utils.add_warning(
                    'Module ~w duplicated in app file for application ~w.',
                    [m, appName],
                    s
                  )
                end,
                status3,
                duplicatedMods
              )
          end

        appModNames =
          case r_app_info(appInfo, :mod) do
            {startModName, _} ->
              case :lists.member(startModName, appInfoMods) do
                true ->
                  appInfoMods

                false ->
                  [startModName | appInfoMods]
              end

            :undefined ->
              appInfoMods
          end

        missingMods = add_missing_mods(appName, ebinMods, appModNames)
        mods2 = add_mod_config(missingMods ++ ebinMods, mods)
        mods3 = set_mod_flags(mods2, appModNames)
        appVsn = r_app_info(appInfo, :vsn)

        appLabel =
          case appVsn do
            '' ->
              :erlang.atom_to_list(appName)

            _ ->
              :erlang.atom_to_list(appName) ++ '-' ++ appVsn
          end

        app2 =
          r_app(app,
            vsn: appVsn,
            label: appLabel,
            info: appInfo,
            mods: :lists.keysort(r_mod(:name), mods3)
          )

        {app2, status4}

      true ->
        {app, status}
    end
  end

  defp missing_app_info(vsn) do
    r_app_info(vsn: vsn)
  end

  defp read_app_info(_AppFileOrBin, _AppFile, :erts, _ActiveDir, _AppStatus, defaultVsn, status) do
    {missing_app_info(defaultVsn), status}
  end

  defp read_app_info(_AppFileOrBin, _AppFile, _AppName, :undefined, :missing, defaultVsn, status) do
    {missing_app_info(defaultVsn), status}
  end

  defp read_app_info(appFileOrBin, appFile, appName, _ActiveDir, _AppStatus, defaultVsn, status) do
    enoentText = :file.format_error(:enoent)

    case :reltool_utils.prim_consult(appFileOrBin) do
      {:ok, [{:application, ^appName, info}]} ->
        aI = r_app_info(vsn: defaultVsn)
        parse_app_info(appFile, info, aI, status)

      {:ok, _BadApp} ->
        {missing_app_info(defaultVsn),
         :reltool_utils.add_warning(
           '~w: Illegal contents in app file ~tp, application tuple with arity 3 expected.',
           [appName, appFile],
           status
         )}

      {:error, text} when text === enoentText ->
        {missing_app_info(defaultVsn),
         :reltool_utils.add_warning('~w: Missing app file ~tp.', [appName, appFile], status)}

      {:error, text} ->
        {missing_app_info(defaultVsn),
         :reltool_utils.add_warning(
           '~w: Cannot parse app file ~tp (~tp).',
           [appName, appFile, text],
           status
         )}
    end
  end

  defp parse_app_info(file, [{key, val} | keyVals], aI, status) do
    case key do
      :description ->
        parse_app_info(file, keyVals, r_app_info(aI, description: val), status)

      :id ->
        parse_app_info(file, keyVals, r_app_info(aI, id: val), status)

      :vsn ->
        parse_app_info(file, keyVals, r_app_info(aI, vsn: val), status)

      :modules ->
        parse_app_info(file, keyVals, r_app_info(aI, modules: val), status)

      :maxP ->
        parse_app_info(file, keyVals, r_app_info(aI, maxP: val), status)

      :maxT ->
        parse_app_info(file, keyVals, r_app_info(aI, maxT: val), status)

      :registered ->
        parse_app_info(file, keyVals, r_app_info(aI, registered: val), status)

      :included_applications ->
        parse_app_info(file, keyVals, r_app_info(aI, incl_apps: val), status)

      :applications ->
        parse_app_info(file, keyVals, r_app_info(aI, applications: val), status)

      :env ->
        parse_app_info(file, keyVals, r_app_info(aI, env: val), status)

      :mod ->
        parse_app_info(file, keyVals, r_app_info(aI, mod: val), status)

      :start_phases ->
        parse_app_info(file, keyVals, r_app_info(aI, start_phases: val), status)

      :runtime_dependencies ->
        parse_app_info(file, keyVals, r_app_info(aI, runtime_dependencies: val), status)

      _ ->
        status2 =
          :reltool_utils.add_warning('Unexpected item ~tp in app file ~tp.', [key, file], status)

        parse_app_info(file, keyVals, aI, status2)
    end
  end

  defp parse_app_info(_, [], aI, status) do
    {aI, status}
  end

  defp read_ebin_mods(ebin, appName) do
    case :erl_prim_loader.list_dir(ebin) do
      {:ok, files} ->
        ext = :code.objfile_extension()

        initMod = fn f ->
          file = :filename.join([ebin, f])
          init_mod(appName, file, file, ext)
        end

        files2 =
          for f <- files,
              :filename.extension(f) === ext do
            f
          end

        pmap(initMod, files2)

      :error ->
        []
    end
  end

  defp pmap(fun, list) do
    :lists.map(fun, list)
  end

  defp init_mod(appName, file, fileOrBin, ext) do
    usesMods = xref_mod(fileOrBin)
    base = :filename.basename(file, ext)
    modName = :erlang.list_to_atom(base)

    r_mod(
      name: modName,
      app_name: appName,
      incl_cond: :undefined,
      is_ebin_mod: true,
      uses_mods: usesMods,
      exists: true
    )
  end

  defp xref_mod({base, bin}) when is_binary(bin) do
    dir = :filename.absname('reltool_server.tmp')
    :ok = :reltool_utils.recursive_delete(dir)
    :ok = :file.make_dir(dir)
    file = :filename.join([dir, base])
    :ok = :file.write_file(file, bin)
    res = xref_mod(file)
    :ok = :reltool_utils.recursive_delete(dir)
    res
  end

  defp xref_mod(file) when is_list(file) do
    {:ok, pid} = :xref.start([{:xref_mode, :modules}])
    :erlang.link(pid)

    :ok =
      :xref.set_default(
        pid,
        [{:verbose, false}, {:warnings, false}]
      )

    :ok = :xref.set_library_path(pid, [])
    {:ok, _} = :xref.add_module(pid, file, [])
    {:ok, unknownMods} = :xref.q(pid, 'UM', [])
    ref = :erlang.monitor(:process, pid)
    :erlang.unlink(pid)
    :xref.stop(pid)
    wait_for_processto_die(ref, pid, file)
    unknownMods
  end

  defp wait_for_processto_die(ref, pid, file) do
    receive do
      {:DOWN, ^ref, _Type, _Object, _Info} ->
        :ok
    after
      :timer.seconds(30) ->
        :error_logger.error_msg(
          '~w(~w): Waiting for process ~w to die ~tp\n',
          [:reltool_server, 1326, pid, file]
        )

        wait_for_processto_die(ref, pid, file)
    end
  end

  defp add_missing_mods(appName, ebinMods, appModNames) do
    ebinModNames =
      for m <- ebinMods do
        r_mod(m, :name)
      end

    missingModNames = appModNames -- ebinModNames

    for modName <- missingModNames do
      missing_mod(modName, appName)
    end
  end

  defp missing_mod(modName, appName) do
    r_mod(
      name: modName,
      app_name: appName,
      incl_cond: :undefined,
      is_ebin_mod: false,
      exists: false,
      status: :missing,
      uses_mods: []
    )
  end

  defp add_mod_config(mods, modConfigs) do
    addConfig = fn config, acc ->
      case :lists.keyfind(r_mod(config, :name), r_mod(:name), mods) do
        r_mod() = m ->
          m2 = r_mod(m, incl_cond: r_mod(config, :incl_cond))
          :lists.keystore(r_mod(config, :name), r_mod(:name), acc, m2)

        false ->
          config2 = r_mod(config, uses_mods: [], exists: false)
          [config2 | acc]
      end
    end

    :lists.foldl(addConfig, mods, modConfigs)
  end

  defp set_mod_flags(mods, appModNames) do
    setFlags = fn r_mod(name: n) = m ->
      r_mod(m, is_app_mod: :lists.member(n, appModNames))
    end

    :lists.map(setFlags, mods)
  end

  defp do_get_config(s, inclDef, inclDeriv) do
    appTab = r_state(s, :app_tab)

    sys =
      case inclDeriv do
        false ->
          apps =
            for r_app(name: name) <- r_sys(r_state(s, :sys), :apps),
                app <- :ets.lookup(appTab, name) do
              shrink_app(app)
            end

          r_sys(r_state(s, :sys), apps: apps)

        true ->
          sys_all_apps(s)
      end

    :reltool_target.gen_config(sys, inclDef)
  end

  defp shrink_app(a) do
    mods =
      for m <- r_app(a, :mods),
          r_mod(m, :incl_cond) !== :undefined do
        r_mod(m,
          is_app_mod: :undefined,
          is_ebin_mod: :undefined,
          uses_mods: :undefined,
          exists: false
        )
      end

    cond do
      r_app(a, :is_escript) ->
        r_app(a,
          vsn: :undefined,
          label: :undefined,
          info: :undefined,
          mods: [],
          uses_mods: :undefined
        )

      true ->
        {dir, dirs, optVsn} =
          case r_app(a, :use_selected_vsn) do
            :undefined ->
              {:undefined, [], :undefined}

            :vsn ->
              {:undefined, [], r_app(a, :vsn)}

            :dir ->
              {r_app(a, :active_dir), [r_app(a, :active_dir)], :undefined}
          end

        r_app(a,
          active_dir: dir,
          sorted_dirs: dirs,
          vsn: optVsn,
          label: :undefined,
          info: :undefined,
          mods: mods,
          uses_mods: :undefined
        )
    end
  end

  defp do_save_config(s, filename, inclDef, inclDeriv) do
    {:ok, config} = do_get_config(s, inclDef, inclDeriv)

    ioList =
      :io_lib.format(
        '%% ~s\n%% config generated at ~w ~w\n~tp.\n\n',
        [:epp.encoding_to_string(:utf8), :erlang.date(), :erlang.time(), config]
      )

    bin = :unicode.characters_to_binary(ioList)
    :file.write_file(filename, bin)
  end

  defp do_load_config(s, sysConfig) do
    r_state(s, sys: read_config(default_sys(), sysConfig))
  end

  defp read_config(oldSys, filename) when is_list(filename) do
    case :file.consult(filename) do
      {:ok, [sysConfig | _]} ->
        read_config(oldSys, sysConfig)

      {:ok, content} ->
        :reltool_utils.throw_error('Illegal file content: ~p', [content])

      {:error, reason} ->
        :reltool_utils.throw_error(
          'Illegal config file ~tp: ~ts',
          [filename, :file.format_error(reason)]
        )
    end
  end

  defp read_config(oldSys, {:sys, keyVals}) do
    newSys = decode(r_sys(oldSys, apps: [], rels: []), keyVals)

    apps =
      for a <- r_sys(newSys, :apps) do
        r_app(a, mods: :lists.sort(r_app(a, :mods)))
      end

    rels =
      case r_sys(newSys, :rels) do
        [] ->
          :reltool_utils.default_rels()

        rs ->
          rs
      end

    newSys2 =
      r_sys(newSys,
        apps: :lists.sort(apps),
        rels: :lists.sort(rels)
      )

    case :lists.keymember(r_sys(newSys2, :boot_rel), r_rel(:name), r_sys(newSys2, :rels)) do
      true ->
        newSys2

      false ->
        :reltool_utils.throw_error('Release ~tp is mandatory (used as boot_rel)', [
          r_sys(newSys2, :boot_rel)
        ])
    end
  end

  defp read_config(_OldSys, badConfig) do
    :reltool_utils.throw_error('Illegal content: ~tp', [badConfig])
  end

  defp decode(
         r_sys(apps: apps) = sys,
         [{:erts = name, appKeyVals} | sysKeyVals]
       )
       when is_atom(name) and is_list(appKeyVals) do
    app = default_app(name)
    app2 = decode(app, appKeyVals)
    decode(r_sys(sys, apps: [app2 | apps]), sysKeyVals)
  end

  defp decode(
         r_sys(apps: apps) = sys,
         [{:app, name, appKeyVals} | sysKeyVals]
       )
       when is_atom(name) and is_list(appKeyVals) do
    app = default_app(name)
    app2 = decode(app, appKeyVals)
    decode(r_sys(sys, apps: [app2 | apps]), sysKeyVals)
  end

  defp decode(
         r_sys(apps: apps, escripts: escripts) = sys,
         [{:escript, file0, appKeyVals} | sysKeyVals]
       )
       when is_list(file0) and is_list(appKeyVals) do
    file = :filename.absname(file0)
    app = default_escript_app(file)
    app2 = decode(app, appKeyVals)

    decode(
      r_sys(sys,
        apps: [app2 | apps],
        escripts: [file | escripts]
      ),
      sysKeyVals
    )
  end

  defp decode(
         r_sys(rels: rels) = sys,
         [{:rel, name, vsn, relApps} | sysKeyVals]
       )
       when is_list(name) and is_list(vsn) and
              is_list(relApps) do
    rel = r_rel(name: name, vsn: vsn, rel_apps: [])
    rel2 = decode(rel, relApps)
    decode(r_sys(sys, rels: [rel2 | rels]), sysKeyVals)
  end

  defp decode(
         r_sys(rels: rels) = sys,
         [{:rel, name, vsn, relApps, opts} | sysKeyVals]
       )
       when is_list(name) and is_list(vsn) and
              is_list(relApps) and is_list(opts) do
    rel1 =
      :lists.foldl(
        fn opt, rel0 ->
          case opt do
            {:load_dot_erlang, value} when is_boolean(value) ->
              r_rel(rel0, load_dot_erlang: value)

            _ ->
              :reltool_utils.throw_error('Illegal rel option: ~tp', [opt])
          end
        end,
        r_rel(name: name, vsn: vsn, rel_apps: []),
        opts
      )

    rel2 = decode(rel1, relApps)
    decode(r_sys(sys, rels: [rel2 | rels]), sysKeyVals)
  end

  defp decode(r_sys() = sys, [{key, val} | keyVals]) do
    sys3 =
      case key do
        :root_dir when is_list(val) ->
          r_sys(sys, root_dir: val)

        :lib_dirs when is_list(val) ->
          r_sys(sys, lib_dirs: val)

        :mod_cond
        when val === :all or val === :app or
               val === :ebin or val === :derived or
               val === :none ->
          r_sys(sys, mod_cond: val)

        :incl_cond
        when val === :include or val === :exclude or
               val === :derived ->
          r_sys(sys, incl_cond: val)

        :boot_rel when is_list(val) ->
          r_sys(sys, boot_rel: val)

        :emu_name when is_list(val) ->
          r_sys(sys, emu_name: val)

        :profile
        when val === :development or
               val === :embedded or val === :standalone ->
          inclSys = :reltool_utils.choose_default(:incl_sys_filters, val, false)
          exclSys = :reltool_utils.choose_default(:excl_sys_filters, val, false)
          inclApp = :reltool_utils.choose_default(:incl_app_filters, val, false)
          exclApp = :reltool_utils.choose_default(:excl_app_filters, val, false)
          appType = :reltool_utils.choose_default(:embedded_app_type, val, false)

          r_sys(sys,
            profile: val,
            incl_sys_filters: dec_re(:incl_sys_filters, inclSys, r_sys(sys, :incl_sys_filters)),
            excl_sys_filters: dec_re(:excl_sys_filters, exclSys, r_sys(sys, :excl_sys_filters)),
            incl_app_filters: dec_re(:incl_app_filters, inclApp, r_sys(sys, :incl_app_filters)),
            excl_app_filters: dec_re(:excl_app_filters, exclApp, r_sys(sys, :excl_app_filters)),
            embedded_app_type: appType
          )

        :excl_lib when val === :otp_root ->
          r_sys(sys, excl_lib: val)

        :incl_sys_filters ->
          r_sys(sys, incl_sys_filters: dec_re(key, val, r_sys(sys, :incl_sys_filters)))

        :excl_sys_filters ->
          r_sys(sys, excl_sys_filters: dec_re(key, val, r_sys(sys, :excl_sys_filters)))

        :incl_app_filters ->
          r_sys(sys, incl_app_filters: dec_re(key, val, r_sys(sys, :incl_app_filters)))

        :excl_app_filters ->
          r_sys(sys, excl_app_filters: dec_re(key, val, r_sys(sys, :excl_app_filters)))

        :incl_archive_filters ->
          r_sys(sys, incl_archive_filters: dec_re(key, val, r_sys(sys, :incl_archive_filters)))

        :excl_archive_filters ->
          r_sys(sys, excl_archive_filters: dec_re(key, val, r_sys(sys, :excl_archive_filters)))

        :archive_opts when is_list(val) ->
          r_sys(sys, archive_opts: val)

        :relocatable when val === true or val === false ->
          r_sys(sys, relocatable: val)

        :rel_app_type
        when val === :permanent or
               val === :transient or val === :temporary or
               val === :load or val === :none ->
          r_sys(sys, rel_app_type: val)

        :embedded_app_type
        when val === :permanent or
               val === :transient or
               val === :temporary or val === :load or
               val === :none or val === :undefined ->
          r_sys(sys, embedded_app_type: val)

        :app_file
        when val === :keep or val === :strip or
               val === :all ->
          r_sys(sys, app_file: val)

        :debug_info when val === :keep or val === :strip ->
          r_sys(sys, debug_info: val)

        _ ->
          :reltool_utils.throw_error('Illegal option: ~tp', [{key, val}])
      end

    decode(sys3, keyVals)
  end

  defp decode(r_app() = app, [{key, val} | keyVals]) do
    app2 =
      case key do
        :mod_cond
        when val === :all or val === :app or
               val === :ebin or val === :derived or
               val === :none ->
          r_app(app, mod_cond: val)

        :incl_cond
        when val === :include or val === :exclude or
               val === :derived ->
          r_app(app, incl_cond: val)

        :debug_info when val === :keep or val === :strip ->
          r_app(app, debug_info: val)

        :app_file
        when val === :keep or val === :strip or
               val === :all ->
          r_app(app, app_file: val)

        :app_type
        when val === :permanent or
               val === :transient or val === :temporary or
               val === :load or val === :none or
               val === :undefined ->
          r_app(app, app_type: val)

        :incl_app_filters ->
          r_app(app, incl_app_filters: dec_re(key, val, r_app(app, :incl_app_filters)))

        :excl_app_filters ->
          r_app(app, excl_app_filters: dec_re(key, val, r_app(app, :excl_app_filters)))

        :incl_archive_filters ->
          r_app(app, incl_archive_filters: dec_re(key, val, r_app(app, :incl_archive_filters)))

        :excl_archive_filters ->
          r_app(app, excl_archive_filters: dec_re(key, val, r_app(app, :excl_archive_filters)))

        :archive_opts when is_list(val) ->
          r_app(app, archive_opts: val)

        :vsn
        when is_list(val) and
               r_app(app, :use_selected_vsn) === :undefined ->
          r_app(app, use_selected_vsn: :vsn, vsn: val)

        :lib_dir
        when is_list(val) and
               r_app(app, :use_selected_vsn) === :undefined ->
          case :filelib.is_dir(val) do
            true ->
              dir = :reltool_utils.normalize_dir(val)
              r_app(app, use_selected_vsn: :dir, active_dir: dir, sorted_dirs: [dir])

            false ->
              :reltool_utils.throw_error('Illegal lib dir for ~w: ~tp', [r_app(app, :name), val])
          end

        selectVsn
        when selectVsn === :vsn or
               selectVsn === :lib_dir ->
          :reltool_utils.throw_error('Mutual exclusive options \'vsn\' and \'lib_dir\'', [])

        _ ->
          :reltool_utils.throw_error('Illegal option: ~tp', [{key, val}])
      end

    decode(app2, keyVals)
  end

  defp decode(
         r_app(mods: mods) = app,
         [{:mod, name, modKeyVals} | appKeyVals]
       ) do
    mod = decode(r_mod(name: name), modKeyVals)
    decode(r_app(app, mods: [mod | mods]), appKeyVals)
  end

  defp decode(r_mod() = mod, [{key, val} | keyVals]) do
    mod2 =
      case key do
        :incl_cond
        when val === :include or val === :exclude or
               val === :derived ->
          r_mod(mod, incl_cond: val)

        :debug_info when val === :keep or val === :strip ->
          r_mod(mod, debug_info: val)

        _ ->
          :reltool_utils.throw_error('Illegal option: ~tp', [{key, val}])
      end

    decode(mod2, keyVals)
  end

  defp decode(
         r_rel(rel_apps: relApps) = rel,
         [relApp | keyVals]
       ) do
    {validTypesAssigned, rA} =
      case relApp do
        name when is_atom(name) ->
          {true, r_rel_app(name: name)}

        {name, inclApps}
        when is_atom(name) and
               is_list(inclApps) ->
          vI =
            :lists.all(
              &:erlang.is_atom/1,
              inclApps
            )

          {vI, r_rel_app(name: name, incl_apps: inclApps)}

        {name, type} when is_atom(name) ->
          {is_type(type), r_rel_app(name: name, app_type: type)}

        {name, type, inclApps}
        when is_atom(name) and
               is_list(inclApps) ->
          vT = is_type(type)

          vI =
            :lists.all(
              &:erlang.is_atom/1,
              inclApps
            )

          {vT and vI, r_rel_app(name: name, app_type: type, incl_apps: inclApps)}

        _ ->
          {false, r_rel_app()}
      end

    case validTypesAssigned do
      true ->
        decode(r_rel(rel, rel_apps: relApps ++ [rA]), keyVals)

      false ->
        :reltool_utils.throw_error('Illegal option: ~tp', [relApp])
    end
  end

  defp decode(acc, []) do
    acc
  end

  defp decode(_Acc, keyVal) do
    :reltool_utils.throw_error('Illegal option: ~tp', [keyVal])
  end

  defp is_type(type) do
    case type do
      :undefined ->
        true

      :permanent ->
        true

      :transient ->
        true

      :temporary ->
        true

      :load ->
        true

      :none ->
        true

      _ ->
        false
    end
  end

  defp split_escript_name(file) when is_list(file) do
    label = :filename.basename(file, '.escript')
    {:erlang.list_to_atom('*escript* ' ++ label), label}
  end

  defp default_escript_app(file) do
    {name, label} = split_escript_name(file)
    app = default_app(name, file)
    r_app(app, is_escript: true, label: label, info: missing_app_info(''))
  end

  defp refresh(r_state(sys: sys) = s) do
    rootDir = :filename.absname(r_sys(sys, :root_dir))

    libDirs =
      for d <- r_sys(sys, :lib_dirs) do
        :filename.absname(d)
      end

    escripts =
      for e <- r_sys(sys, :escripts) do
        :filename.absname(e)
      end

    sourceDirs = libs_to_dirs(rootDir, libDirs)
    mergedApps = merge_app_dirs(sourceDirs, r_sys(sys, :apps))
    {allApps, status2} = escripts_to_apps(escripts, mergedApps, {:ok, []})
    {refreshedApps, status3} = refresh_apps(r_sys(sys, :apps), allApps, [], true, status2)
    {patchedApps, status4} = patch_erts_version(rootDir, refreshedApps, status3)

    escripts2 =
      for a <- patchedApps, r_app(a, :is_escript) do
        r_app(a, :active_dir)
      end

    sys2 = r_sys(sys, root_dir: rootDir, lib_dirs: libDirs, escripts: escripts2)
    {r_state(s, sys: sys2), patchedApps, status4}
  end

  defp patch_erts_version(rootDir, apps, status) do
    appName = :erts

    case :lists.keyfind(appName, r_app(:name), apps) do
      r_app(vsn: vsn) = erts ->
        localRoot = :code.root_dir()

        cond do
          localRoot === rootDir and vsn === '' ->
            vsn2 = :erlang.system_info(:version)
            erts2 = r_app(erts, vsn: vsn2, label: 'erts-' ++ vsn2)
            apps2 = :lists.keystore(appName, r_app(:name), apps, erts2)
            {apps2, status}

          vsn === '' ->
            {apps, :reltool_utils.add_warning('erts has no version', [], status)}

          true ->
            {apps, status}
        end

      false ->
        :reltool_utils.throw_error('erts cannot be found in the root directory ~tp', [rootDir])
    end
  end

  defp libs_to_dirs(rootDir, libDirs) do
    case :file.list_dir(rootDir) do
      {:ok, rootFiles} ->
        rootLibDir = :filename.join([rootDir, 'lib'])
        allLibDirs = [rootLibDir | libDirs]

        case allLibDirs -- :lists.usort(allLibDirs) do
          [] ->
            fun = fn base ->
              appDir = :filename.join([rootLibDir, base])

              case :filelib.is_dir(
                     :filename.join([appDir, 'ebin']),
                     :erl_prim_loader
                   ) do
                true ->
                  appDir

                false ->
                  :filename.join([rootDir, base, 'preloaded'])
              end
            end

            ertsFiles =
              for f <- rootFiles, :lists.prefix('erts', f) do
                {:erts, fun.(f)}
              end

            app_dirs2(allLibDirs, [ertsFiles])

          [duplicate | _] ->
            :reltool_utils.throw_error('Duplicate library: ~tp', [duplicate])
        end

      {:error, reason} ->
        :reltool_utils.throw_error(
          'Missing root library ~tp: ~ts',
          [rootDir, :file.format_error(reason)]
        )
    end
  end

  defp app_dirs2([lib | libs], acc) do
    case :file.list_dir(lib) do
      {:ok, files} ->
        filter = fn base ->
          appDir = :filename.join([lib, base])
          ebinDir = :filename.join([appDir, 'ebin'])

          case :filelib.is_dir(ebinDir, :erl_prim_loader) do
            true ->
              name = find_app_name(base, ebinDir)

              case name do
                :erts ->
                  false

                _ ->
                  {true, {name, appDir}}
              end

            false ->
              false
          end
        end

        files2 = :lists.zf(filter, files)
        app_dirs2(libs, [files2 | acc])

      {:error, reason} ->
        :reltool_utils.throw_error(
          'Illegal library ~tp: ~ts',
          [lib, :file.format_error(reason)]
        )
    end
  end

  defp app_dirs2([], acc) do
    :lists.sort(:lists.append(acc))
  end

  defp find_app_name(base, ebinDir) do
    {:ok, ebinFiles} = :erl_prim_loader.list_dir(ebinDir)

    appFile =
      case (for f <- ebinFiles,
                :filename.extension(f) === '.app' do
              f
            end) do
        [aF] ->
          aF

        _ ->
          :undefined
      end

    find_app_name1(base, appFile)
  end

  defp find_app_name1(base, :undefined) do
    {name, _} = :reltool_utils.split_app_name(base)
    name
  end

  defp find_app_name1(_Base, appFile) do
    :erlang.list_to_atom(:filename.rootname(appFile))
  end

  defp get_vsn_from_dir(appName, base) do
    prefix = :erlang.atom_to_list(appName) ++ '-'

    case :lists.prefix(prefix, base) do
      true ->
        :lists.nthtail(length(prefix), base)

      false ->
        ''
    end
  end

  defp escripts_to_apps([escript | escripts], apps, status) do
    {escriptAppName, _Label} = split_escript_name(escript)
    ext = :code.objfile_extension()

    appFun = fn fullName, _GetInfo, _GetBin, appFiles ->
      components = :filename.split(fullName)

      case components do
        [appLabel, 'ebin', file] ->
          case :filename.extension(file) do
            '.app' ->
              [{appLabel, file} | appFiles]

            _ ->
              appFiles
          end

        _ ->
          appFiles
      end
    end

    appFiles =
      case :reltool_utils.escript_foldl(appFun, [], escript) do
        {:ok, aF} ->
          aF

        {:error, reason1} ->
          :reltool_utils.throw_error('Illegal escript ~tp: ~tp', [escript, reason1])
      end

    fun = fn fullName, _GetInfo, getBin, {fileAcc, statusAcc} ->
      components = :filename.split(fullName)

      case components do
        [appLabel, 'ebin', file] ->
          case :filename.extension(file) do
            '.app' ->
              appName = :erlang.list_to_atom(:filename.rootname(file))
              defaultVsn = get_vsn_from_dir(appName, appLabel)
              appFileName = :filename.join([escript, fullName])
              dir = :filename.join([escript, appName])

              {info, statusAcc2} =
                read_app_info(getBin.(), appFileName, appName, dir, :ok, defaultVsn, status)

              {[{appName, :app, dir, info} | fileAcc], statusAcc2}

            e when e === ext ->
              appFile = :proplists.get_value(appLabel, appFiles)
              appName = find_app_name1(appLabel, appFile)
              mod = init_mod(appName, file, {file, getBin.()}, ext)
              dir = :filename.join([escript, appName])
              {[{appName, :mod, dir, mod} | fileAcc], statusAcc}

            _ ->
              {fileAcc, statusAcc}
          end

        ['.'] ->
          bin = getBin.()
          {:ok, {modName, _}} = :beam_lib.version(bin)
          modStr = :erlang.atom_to_list(modName) ++ ext
          mod = init_mod(escriptAppName, modStr, {modStr, getBin.()}, ext)
          {[{escriptAppName, :mod, escript, mod} | fileAcc], statusAcc}

        [file] ->
          case :filename.extension(file) do
            e when e === ext ->
              mod = init_mod(escriptAppName, file, {file, getBin.()}, ext)
              {[{escriptAppName, :mod, file, mod} | fileAcc], statusAcc}

            _ ->
              {fileAcc, statusAcc}
          end

        _ ->
          {fileAcc, statusAcc}
      end
    end

    case :reltool_utils.escript_foldl(fun, {[], status}, escript) do
      {:ok, {files, status2}} ->
        escriptApp =
          case :lists.keyfind(escriptAppName, r_app(:name), apps) do
            false ->
              default_escript_app(escript)

            eA ->
              eA
          end

        {apps2, status3} =
          escript_files_to_apps(escriptAppName, :lists.sort(files), [escriptApp], apps, status2)

        escripts_to_apps(escripts, apps2, status3)

      {:error, reason2} ->
        :reltool_utils.throw_error('Illegal escript ~tp: ~tp', [escript, reason2])
    end
  end

  defp escripts_to_apps([], apps, status) do
    {apps, status}
  end

  defp escript_files_to_apps(
         escriptAppName,
         [{appName, type, dir, modOrInfo} | files],
         acc,
         apps,
         status
       ) do
    {newAcc, status3} =
      case type do
        :mod ->
          case acc do
            [app | acc2]
            when r_app(app, :name) === r_mod(modOrInfo, :app_name) ->
              mods = :lists.ukeymerge(r_mod(:name), [modOrInfo], r_app(app, :mods))
              {[r_app(app, mods: mods) | acc2], status}

            ^acc ->
              {newApp, status2} =
                init_escript_app(
                  appName,
                  escriptAppName,
                  dir,
                  missing_app_info(''),
                  [modOrInfo],
                  apps,
                  status
                )

              {[newApp | acc], status2}
          end

        :app ->
          {app, status2} =
            init_escript_app(appName, escriptAppName, dir, modOrInfo, [], apps, status)

          {[app | acc], status2}
      end

    escript_files_to_apps(escriptAppName, files, newAcc, apps, status3)
  end

  defp escript_files_to_apps(_EscriptAppName, [], acc, apps, status) do
    {:lists.ukeymerge(r_app(:name), :lists.reverse(acc), apps), status}
  end

  defp init_escript_app(appName, escriptAppName, dir, info, mods, apps, status) do
    app1 = default_app(appName, dir)

    isEscript =
      cond do
        appName === escriptAppName ->
          true

        true ->
          {:inlined, escriptAppName}
      end

    inclCond =
      r_app(
        :lists.keyfind(escriptAppName, r_app(:name), apps),
        :incl_cond
      )

    app2 =
      r_app(app1,
        is_escript: isEscript,
        label: :filename.basename(dir, '.escript'),
        info: info,
        mods: mods,
        active_dir: dir,
        sorted_dirs: [dir],
        incl_cond: inclCond
      )

    case :lists.keymember(appName, r_app(:name), apps) do
      true ->
        :reltool_utils.throw_error(
          '~w: Application name clash. Escript ~tp contains application ~w.',
          [appName, dir, appName]
        )

      false ->
        {app2, status}
    end
  end

  defp merge_app_dirs([{name, dir} | rest], apps) do
    app =
      case :lists.keyfind(name, r_app(:name), apps) do
        false ->
          default_app(name, dir)

        oldApp ->
          sortedDirs =
            :lists.umerge(&:reltool_utils.app_dir_test/2, [dir], r_app(oldApp, :sorted_dirs))

          r_app(oldApp, sorted_dirs: sortedDirs)
      end

    apps2 = :lists.ukeymerge(r_app(:name), [app], apps)
    merge_app_dirs(rest, apps2)
  end

  defp merge_app_dirs([], apps) do
    set_active_dirs(apps)
  end

  defp set_active_dirs([r_app(use_selected_vsn: :dir) = app | apps]) do
    [app | set_active_dirs(apps)]
  end

  defp set_active_dirs([
         r_app(sorted_dirs: [activeDir | _]) = app
         | apps
       ]) do
    [r_app(app, active_dir: activeDir) | set_active_dirs(apps)]
  end

  defp set_active_dirs([r_app(sorted_dirs: []) = app | apps]) do
    [r_app(app, active_dir: :undefined) | set_active_dirs(apps)]
  end

  defp set_active_dirs([]) do
    []
  end

  defp default_app(name, dir) do
    app = default_app(name)
    r_app(app, active_dir: dir, sorted_dirs: [dir])
  end

  defp default_app(name) do
    r_app(name: name, is_escript: false, sorted_dirs: [], mods: [], status: :missing)
  end

  defp refresh_apps(configApps, [new | newApps], acc, force, status) do
    {new2, status3} =
      case :lists.keymember(r_app(new, :name), r_app(:name), configApps) do
        true ->
          {info, activeDir, status2} =
            ensure_app_info(
              new,
              status
            )

          optLabel =
            case r_app_info(info, :vsn) === r_app(new, :vsn) do
              true ->
                r_app(new, :label)

              false ->
                :undefined
            end

          refresh_app(
            r_app(new,
              label: optLabel,
              active_dir: activeDir,
              vsn: r_app_info(info, :vsn),
              info: info
            ),
            force,
            status2
          )

        false ->
          refresh_app(new, force, status)
      end

    refresh_apps(configApps, newApps, [new2 | acc], force, status3)
  end

  defp refresh_apps(_ConfigApps, [], acc, _Force, status) do
    {:lists.reverse(acc), status}
  end

  defp ensure_app_info(
         r_app(is_escript: isEscript, active_dir: dir, info: info),
         status
       )
       when isEscript !== false do
    {info, dir, status}
  end

  defp ensure_app_info(r_app(name: name, sorted_dirs: []) = app, status) do
    reason = '~w: Missing application directory.'

    case app do
      r_app(incl_cond: :exclude, status: :missing, active_dir: dir) ->
        status2 = :reltool_utils.add_warning(reason, [name], status)
        {missing_app_info(''), dir, status2}

      _ ->
        :reltool_utils.throw_error(reason, [name])
    end
  end

  defp ensure_app_info(
         r_app(
           name: name,
           vsn: vsn,
           use_selected_vsn: useSelectedVsn,
           active_dir: activeDir,
           sorted_dirs: dirs,
           info: :undefined,
           status: appStatus
         ),
         status
       ) do
    readInfo = fn dir, statusAcc ->
      base = get_base(name, dir)
      ebin = :filename.join([dir, 'ebin'])
      defaultVsn = get_vsn_from_dir(name, base)
      appFile = :filename.join([ebin, :erlang.atom_to_list(name) ++ '.app'])
      read_app_info(appFile, appFile, name, activeDir, appStatus, defaultVsn, statusAcc)
    end

    {allInfo, status2} = :lists.mapfoldl(readInfo, status, dirs)

    allVsns =
      for i <- allInfo do
        r_app_info(i, :vsn)
      end

    status3 =
      case allVsns -- :lists.usort(allVsns) do
        [] ->
          status2

        [badVsn | _] ->
          :reltool_utils.throw_error(
            '~w: Application version clash. Multiple directories contain version ~tp.',
            [name, badVsn]
          )
      end

    firstInfo = hd(allInfo)
    firstDir = hd(dirs)

    cond do
      useSelectedVsn === :dir ->
        cond do
          activeDir === firstDir ->
            {firstInfo, firstDir, status3}

          true ->
            info = find_dir(activeDir, allInfo, dirs)
            {info, activeDir, status3}
        end

      useSelectedVsn === :vsn ->
        cond do
          vsn === r_app_info(firstInfo, :vsn) ->
            {firstInfo, firstDir, status3}

          true ->
            case find_vsn(vsn, allInfo, dirs) do
              {info, vsnDir} ->
                {info, vsnDir, status3}

              false ->
                :reltool_utils.throw_error(
                  '~w: No application directory contains selected version ~tp',
                  [name, vsn]
                )
            end
        end

      true ->
        {firstInfo, firstDir, status3}
    end
  end

  defp ensure_app_info(r_app(active_dir: dir, info: info), status) do
    {info, dir, status}
  end

  defp find_vsn(vsn, [r_app_info(vsn: vsn) = info | _], [dir | _]) do
    {info, dir}
  end

  defp find_vsn(vsn, [_ | moreInfo], [_ | moreDirs]) do
    find_vsn(vsn, moreInfo, moreDirs)
  end

  defp find_vsn(_, [], []) do
    false
  end

  defp find_dir(dir, [info | _], [dir | _]) do
    info
  end

  defp find_dir(dir, [_ | moreInfo], [_ | moreDirs]) do
    find_dir(dir, moreInfo, moreDirs)
  end

  defp get_base(name, dir) do
    case name do
      :erts ->
        case :filename.basename(dir) do
          'preloaded' ->
            :filename.basename(:filename.dirname(dir))

          tmpBase ->
            tmpBase
        end

      _ ->
        :filename.basename(dir)
    end
  end

  defp sys_all_apps(r_state(app_tab: appTab, sys: sys)) do
    r_sys(sys, apps: :ets.match_object(appTab, :_))
  end

  defp config_and_refresh(oldS, fun) do
    try do
      s = fun.()
      {s2, apps, status2} = refresh(s)
      backup = backup(oldS)

      try do
        status3 = analyse(s2, apps, status2)
        s3 = save_old(oldS, s2, backup, status3)
        {s3, status3}
      catch
        {:error, _} = error1 ->
          restore(backup, oldS)
          throw(error1)
      end
    catch
      {:error, _} = error2 ->
        {oldS, error2}
    end
  end

  defp backup(s) do
    apps = :ets.tab2list(r_state(s, :app_tab))
    mods = :ets.tab2list(r_state(s, :mod_tab))
    :ets.delete_all_objects(r_state(s, :app_tab))
    :ets.delete_all_objects(r_state(s, :mod_tab))
    :ets.delete_all_objects(r_state(s, :mod_used_by_tab))
    {apps, mods}
  end

  defp restore({apps, mods}, s) do
    insert_all(r_state(s, :app_tab), apps)
    insert_all(r_state(s, :mod_tab), mods)
  end

  defp save_old(r_state(status: oldStatus, sys: oldSys), newS, {oldApps, oldMods}, newStatus) do
    :ets.delete_all_objects(r_state(newS, :old_app_tab))
    :ets.delete_all_objects(r_state(newS, :old_mod_tab))
    insert_all(r_state(newS, :old_app_tab), oldApps)
    insert_all(r_state(newS, :old_mod_tab), oldMods)
    r_state(newS, old_sys: oldSys, old_status: oldStatus, status: newStatus)
  end

  defp insert_all(tab, items) do
    :lists.foreach(
      fn item ->
        :ets.insert(tab, item)
      end,
      items
    )
  end

  def system_continue(_Parent, _Debug, s) do
    :reltool_server.loop(s)
  end

  def system_terminate(reason, _Parent, _Debug, _S) do
    exit(reason)
  end

  def system_code_change(s, _Module, _OldVsn, _Extra) do
    {:ok, s}
  end
end
