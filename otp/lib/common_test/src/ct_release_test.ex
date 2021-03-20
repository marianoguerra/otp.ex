defmodule :m_ct_release_test do
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

  Record.defrecord(:r_ct_data, :ct_data,
    from: :undefined,
    to: :undefined
  )

  def init(config) do
    try do
      init_upgrade_test()
    catch
      thrown ->
        thrown
    else
      {major, minor} ->
        [
          {:release_test, [{:major, major}, {:minor, minor}]}
          | config
        ]
    end
  end

  def upgrade(app, level, callback, config)
      when is_atom(app) do
    upgrade([app], level, callback, config)
  end

  def upgrade(apps, level, callback, config) do
    dir = :proplists.get_value(:priv_dir, config)
    createDir = :filename.join([dir, level, :create])
    installDir = :filename.join([dir, level, :install])
    :ok = :filelib.ensure_dir(:filename.join(createDir, '*'))
    :ok = :filelib.ensure_dir(:filename.join(installDir, '*'))

    try do
      upgrade(apps, level, callback, createDir, installDir, config)
    catch
      {:fail, reason} ->
        :ct.fail(reason)

      {:skip, reason} ->
        rm_rf(createDir)
        rm_rf(installDir)
        {:skip, reason}
    else
      :ok ->
        tars = :filelib.wildcard(:filename.join(createDir, '*.tar.gz'))

        _ =
          for tar <- tars do
            :file.delete(tar)
          end

        rm_rf(installDir)
        :ok
    after
      nodes =
        :lists.filter(
          fn node ->
            case :erlang.atom_to_list(node) do
              'ct_release_test-' ++ _ ->
                true

              _ ->
                false
            end
          end,
          :erlang.nodes()
        )

      for node <- nodes do
        :rpc.call(node, :erlang, :halt, [])
      end
    end
  end

  def cleanup(config) do
    allNodes = [node_name(:"ct_release_test-upgrade") | :erlang.nodes()]

    nodes =
      :lists.filter(
        fn node ->
          case :erlang.atom_to_list(node) do
            'ct_release_test-' ++ _ ->
              true

            _ ->
              false
          end
        end,
        allNodes
      )

    _ =
      for node <- nodes do
        :rpc.call(node, :erlang, :halt, [])
      end

    config
  end

  def get_app_vsns(r_ct_data(from: fromApps, to: toApps), app) do
    case {:lists.keyfind(app, 1, fromApps), :lists.keyfind(app, 1, toApps)} do
      {{^app, fromVsn, _}, {^app, toVsn, _}} ->
        {:ok, {fromVsn, toVsn}}

      _ ->
        {:error, {:app_not_found, app}}
    end
  end

  def get_appup(r_ct_data(from: fromApps, to: toApps), app) do
    case :lists.keyfind(app, 1, toApps) do
      {^app, toVsn, toDir} ->
        appup = :filename.join([toDir, 'ebin', :erlang.atom_to_list(app) ++ '.appup'])
        {:ok, [{^toVsn, ups, downs}]} = :file.consult(appup)
        {^app, fromVsn, _} = :lists.keyfind(app, 1, fromApps)

        case {:systools_relup.appup_search_for_version(
                fromVsn,
                ups
              ),
              :systools_relup.appup_search_for_version(
                fromVsn,
                downs
              )} do
          {{:ok, up}, {:ok, down}} ->
            {:ok, {fromVsn, toVsn, up, down}}

          _ ->
            {:error, {:vsn_not_found, {app, fromVsn}}}
        end

      false ->
        {:error, {:app_not_found, app}}
    end
  end

  defp init_upgrade_test() do
    :ok = :application.ensure_started(:sasl)

    case :release_handler.which_releases() do
      [{_, _, [], _}] ->
        throw({:skip, 'Need a real release running to create other releases'})

      _ ->
        major = init_upgrade_test(:major)
        minor = init_upgrade_test(:minor)
        {major, minor}
    end
  end

  defp init_upgrade_test(level) do
    {fromVsn, toVsn} = get_rels(level)

    oldRel =
      case :test_server.is_release_available(fromVsn) do
        true ->
          {:release, fromVsn}

        false ->
          case :ct.get_config({:otp_releases, :erlang.list_to_atom(fromVsn)}) do
            :undefined ->
              false

            prog0 ->
              case :os.find_executable(prog0) do
                false ->
                  false

                prog ->
                  {:prog, prog}
              end
          end
      end

    case oldRel do
      false ->
        :ct.log('Release ~tp is not available. Upgrade on \'~p\' level cannot be tested.', [
          fromVsn,
          level
        ])

        :undefined

      _ ->
        init_upgrade_test(fromVsn, toVsn, oldRel)
    end
  end

  defp get_rels(:major) do
    current = :erlang.system_info(:otp_release)
    previousMajor = previous_major(current)
    {previousMajor, current}
  end

  defp get_rels(:minor) do
    currentMajor = :erlang.system_info(:otp_release)
    current = currentMajor ++ '_patched'
    {currentMajor, current}
  end

  defp init_upgrade_test(fromVsn, toVsn, oldRel) do
    name = :erlang.list_to_atom('ct_release_test-otp-' ++ fromVsn)
    :ct.log('Starting node to fetch application versions to upgrade from')
    {:ok, node} = :test_server.start_node(name, :peer, [{:erl, [oldRel]}])
    {apps, path} = fetch_all_apps(node)
    :test_server.stop_node(node)
    {fromVsn, toVsn, apps, path}
  end

  defp fetch_all_apps(node) do
    paths = :rpc.call(node, :code, :get_path, [])

    appFiles =
      :lists.flatmap(
        fn p ->
          :filelib.wildcard(:filename.join(p, '*.app'))
        end,
        paths
      )

    appVsns =
      :lists.flatmap(
        fn f ->
          a = :erlang.list_to_atom(:filename.basename(:filename.rootname(f)))
          _ = :rpc.call(node, :application, :load, [a])

          case :rpc.call(node, :application, :get_key, [a, :vsn]) do
            {:ok, v} ->
              [{a, v, :rpc.call(node, :code, :lib_dir, [a])}]

            _ ->
              []
          end
        end,
        appFiles
      )

    ertsVsn = :rpc.call(node, :erlang, :system_info, [:version])
    {[{:erts, ertsVsn} | appVsns], paths}
  end

  defp upgrade(apps, level, callback, createDir, installDir, config) do
    :ct.log('Test upgrade of the following applications: ~p', [apps])
    :ct.log('.rel files and start scripts are created in:~n~ts', [createDir])
    :ct.log('The release is installed in:~n~ts', [installDir])

    case :proplists.get_value(:release_test, config) do
      :undefined ->
        throw({:fail, 'ct_release_test:init/1 not run'})

      rTConfig ->
        case :proplists.get_value(level, rTConfig) do
          :undefined ->
            throw({:skip, 'Old release not available'})

          data ->
            {fromVsn, fromRel, fromAppsVsns} = target_system(apps, createDir, installDir, data)

            {toVsn, toRel, toAppsVsns} =
              upgrade_system(apps, fromRel, createDir, installDir, data)

            :ct.log('Upgrade from: OTP-~ts, ~tp', [fromVsn, fromAppsVsns])
            :ct.log('Upgrade to: OTP-~ts, ~tp', [toVsn, toAppsVsns])
            do_upgrade(callback, fromVsn, fromAppsVsns, toRel, toAppsVsns, installDir)
        end
    end
  end

  defp target_system(apps, createDir, installDir, {fromVsn, _, allAppsVsns, path}) do
    relName0 = 'otp-' ++ fromVsn

    appsVsns =
      for {a, v, d} <- allAppsVsns,
          :lists.member(a, apps) do
        {a, v, d}
      end

    {relName, ertsVsn} = create_relfile(appsVsns, createDir, relName0, fromVsn)
    :ok = systools(:make_script, [relName, [{:path, path}]])

    :ok =
      systools(
        :make_tar,
        [relName, [{:erts, :code.root_dir()}, {:path, path}]]
      )

    :ok =
      :erl_tar.extract(
        relName ++ '.tar.gz',
        [{:cwd, installDir}, :compressed]
      )

    binDir = :filename.join([installDir, 'bin'])
    :ok = make_dir(binDir)
    :ok = make_dir(:filename.join(installDir, 'log'))
    ertsBinDir = :filename.join([installDir, 'erts-' ++ ertsVsn, 'bin'])
    :ok = delete_file(:filename.join([ertsBinDir, 'erl']))
    :ok = delete_file(:filename.join([ertsBinDir, 'start']))
    :ok = delete_file(:filename.join([ertsBinDir, 'start_erl']))
    copy_file(relName ++ '.boot', :filename.join([binDir, 'start.boot']))
    copy_file(:filename.join([ertsBinDir, 'epmd']), :filename.join([binDir, 'epmd']), [:preserve])

    copy_file(:filename.join([ertsBinDir, 'run_erl']), :filename.join([binDir, 'run_erl']), [
      :preserve
    ])

    copy_file(:filename.join([ertsBinDir, 'to_erl']), :filename.join([binDir, 'to_erl']), [
      :preserve
    ])

    startErlData = :filename.join([installDir, 'releases', 'start_erl.data'])

    :ok =
      write_file(
        startErlData,
        :io_lib.fwrite('~s ~s~n', [ertsVsn, fromVsn])
      )

    sysConfig = :filename.join([installDir, 'releases', fromVsn, 'sys.config'])
    :ok = write_file(sysConfig, '[].')
    startSrc = :filename.join(ertsBinDir, 'start.src')
    :ok = write_file(startSrc, start_script())
    :ok = :file.change_mode(startSrc, 493)

    :ok =
      :file.change_mode(
        :filename.join(ertsBinDir, 'start_erl.src'),
        493
      )

    subst_src_scripts(
      ['erl', 'start', 'start_erl'],
      ertsBinDir,
      binDir,
      [{'FINAL_ROOTDIR', installDir}, {'EMU', 'beam'}],
      [:preserve]
    )

    relFile = :filename.join([installDir, 'releases', :filename.basename(relName) ++ '.rel'])
    :release_handler.create_RELEASES(installDir, relFile)
    {fromVsn, relName, appsVsns}
  end

  defp systools(func, args) do
    case apply(:systools, func, args) do
      :ok ->
        :ok

      :error ->
        throw({:fail, {:systools, func, args}})
    end
  end

  defp start_script() do
    [
      '#!/bin/sh\nROOTDIR=%FINAL_ROOTDIR%\n\nif [ -z "$RELDIR" ]\nthen\n   RELDIR=$ROOTDIR/releases\nfi\n\nSTART_ERL_DATA=${1:-$RELDIR/start_erl.data}\n\n$ROOTDIR/bin/run_erl -daemon /tmp/ $ROOTDIR/log "exec $ROOTDIR/bin/start_erl $ROOTDIR $RELDIR $START_ERL_DATA -sname ',
      :erlang.atom_to_list(:"ct_release_test-upgrade"),
      ' -heart"\n'
    ]
  end

  defp upgrade_system(apps, fromRel, createDir, installDir, {_, toVsn, _, _}) do
    :ct.log('Generating release to upgrade to.')
    relName0 = 'otp-' ++ toVsn
    appsVsns = get_vsns(apps)
    {relName, _} = create_relfile(appsVsns, createDir, relName0, toVsn)
    fromPath = :filename.join([installDir, :lib, '*', :ebin])
    :ok = systools(:make_script, [relName])

    :ok =
      systools(
        :make_relup,
        [relName, [fromRel], [fromRel], [{:path, [fromPath]}, {:outdir, createDir}]]
      )

    sysConfig = :filename.join([createDir, 'sys.config'])
    :ok = write_file(sysConfig, '[].')

    :ok =
      systools(
        :make_tar,
        [relName, [{:erts, :code.root_dir()}]]
      )

    {toVsn, relName, appsVsns}
  end

  defp do_upgrade({cb, initState}, fromVsn, fromAppsVsns, toRel, toAppsVsns, installDir) do
    :ct.log('Upgrade test attempting to start node.~nIf test fails, logs can be found in:~n~ts', [
      :filename.join(installDir, :log)
    ])

    start = :filename.join([installDir, :bin, :start])
    {:ok, node} = start_node(start, fromVsn, fromAppsVsns)
    :ct.log('Node started: ~p', [node])
    ctData = r_ct_data(from: fromAppsVsns, to: toAppsVsns)
    state1 = do_callback(node, cb, :upgrade_init, [ctData, initState])

    [{'OTP upgrade test', ^fromVsn, _, :permanent}] =
      :rpc.call(node, :release_handler, :which_releases, [])

    toRelName = :filename.basename(toRel)

    copy_file(
      toRel ++ '.tar.gz',
      :filename.join([installDir, :releases, toRelName ++ '.tar.gz'])
    )

    :ct.log('Unpacking new release')
    {:ok, toVsn} = :rpc.call(node, :release_handler, :unpack_release, [toRelName])

    [{'OTP upgrade test', ^toVsn, _, :unpacked}, {'OTP upgrade test', ^fromVsn, _, :permanent}] =
      :rpc.call(
        node,
        :release_handler,
        :which_releases,
        []
      )

    :ct.log('Installing new release')

    case :rpc.call(node, :release_handler, :install_release, [toVsn]) do
      {:ok, ^fromVsn, _} ->
        :ok

      {:continue_after_restart, ^fromVsn, _} ->
        :ct.log('Waiting for node restart')
    end

    {:ok, _} = wait_node_up(:current, toVsn, toAppsVsns)

    [{'OTP upgrade test', ^toVsn, _, :current}, {'OTP upgrade test', ^fromVsn, _, :permanent}] =
      :rpc.call(
        node,
        :release_handler,
        :which_releases,
        []
      )

    :ct.log('Permanenting new release')
    :ok = :rpc.call(node, :release_handler, :make_permanent, [toVsn])

    [{'OTP upgrade test', ^toVsn, _, :permanent}, {'OTP upgrade test', ^fromVsn, _, :old}] =
      :rpc.call(node, :release_handler, :which_releases, [])

    state2 = do_callback(node, cb, :upgrade_upgraded, [ctData, state1])
    :ct.log('Re-installing old release')

    case :rpc.call(node, :release_handler, :install_release, [fromVsn]) do
      {:ok, ^fromVsn, _} ->
        :ok

      {:continue_after_restart, ^fromVsn, _} ->
        :ct.log('Waiting for node restart')
    end

    {:ok, _} = wait_node_up(:current, fromVsn, fromAppsVsns)

    [{'OTP upgrade test', ^toVsn, _, :permanent}, {'OTP upgrade test', ^fromVsn, _, :current}] =
      :rpc.call(
        node,
        :release_handler,
        :which_releases,
        []
      )

    :ct.log('Permanenting old release')
    :ok = :rpc.call(node, :release_handler, :make_permanent, [fromVsn])

    [{'OTP upgrade test', ^toVsn, _, :old}, {'OTP upgrade test', ^fromVsn, _, :permanent}] =
      :rpc.call(node, :release_handler, :which_releases, [])

    _State3 = do_callback(node, cb, :upgrade_downgraded, [ctData, state2])
    :ct.log('Terminating node ~p', [node])
    :erlang.monitor_node(node, true)
    _ = :rpc.call(node, :init, :stop, [])

    receive do
      {:nodedown, ^node} ->
        :ok
    end

    :ct.log('Node terminated')
    :ok
  end

  defp do_callback(node, mod, func, args) do
    dir = :filename.dirname(:code.which(mod))
    _ = :rpc.call(node, :code, :add_path, [dir])
    :ct.log('Calling ~p:~tp/1', [mod, func])
    r = :rpc.call(node, mod, func, args)
    :ct.log('~p:~tp/~w returned: ~tp', [mod, func, length(args), r])

    case r do
      {:badrpc, error} ->
        throw({:fail, {:test_upgrade_callback, mod, func, args, error}})

      newState ->
        newState
    end
  end

  defp previous_major('17') do
    'r16b'
  end

  defp previous_major(rel) do
    :erlang.integer_to_list(:erlang.list_to_integer(rel) - 1)
  end

  defp create_relfile(appsVsns, createDir, relName0, relVsn) do
    upgradeAppsVsns =
      for {a, v, _D} <- appsVsns do
        {a, v, restart_type(a)}
      end

    coreAppVsns0 = get_vsns([:kernel, :stdlib, :sasl])

    coreAppVsns =
      for {a, v, _D} <- coreAppVsns0,
          false == :lists.keymember(a, 1, appsVsns) do
        {a, v, restart_type(a)}
      end

    apps =
      for {app, _, _} <- appsVsns do
        app
      end

    startDepsVsns = get_start_deps(apps, coreAppVsns)

    startApps =
      for {startApp, _, _} <- startDepsVsns do
        startApp
      end ++ apps

    {runtimeDepsVsns, _} = get_runtime_deps(startApps, startApps, [], [])
    allAppsVsns0 = startDepsVsns ++ upgradeAppsVsns ++ runtimeDepsVsns
    testToolAppsVsns0 = get_vsns([:common_test])

    testToolAppsVsns =
      for {a, v, _D} <- testToolAppsVsns0,
          false == :lists.keymember(a, 1, allAppsVsns0) do
        {a, v, :none}
      end

    allAppsVsns1 = allAppsVsns0 ++ testToolAppsVsns

    allAppsVsns =
      for aV = {a, _, _} <- allAppsVsns1,
          false == :lists.member(a, [:hipe, :dialyzer]) do
        aV
      end

    ertsVsn = :erlang.system_info(:version)
    relContent = {:release, {'OTP upgrade test', relVsn}, {:erts, ertsVsn}, allAppsVsns}
    relName = :filename.join(createDir, relName0)
    relFile = relName ++ '.rel'

    {:ok, fd} =
      :file.open(
        relFile,
        [:write, {:encoding, :utf8}]
      )

    :io.format(fd, '~tp.~n', [relContent])
    :ok = :file.close(fd)
    {relName, ertsVsn}
  end

  defp get_vsns(apps) do
    for a <- apps do
      _ = :application.load(a)
      {:ok, v} = :application.get_key(a, :vsn)
      {a, v, :code.lib_dir(a)}
    end
  end

  defp get_start_deps([app | apps], acc) do
    _ = :application.load(app)

    {:ok, startDeps} =
      :application.get_key(
        app,
        :applications
      )

    startDepsVsns =
      for startApp <- startDeps,
          false == :lists.keymember(startApp, 1, acc) do
        _ = :application.load(startApp)
        {:ok, startVsn} = :application.get_key(startApp, :vsn)
        {startApp, startVsn, restart_type(startApp)}
      end

    depsStartDeps =
      get_start_deps(
        startDeps,
        acc ++ startDepsVsns
      )

    get_start_deps(apps, depsStartDeps)
  end

  defp get_start_deps([], acc) do
    acc
  end

  defp get_runtime_deps([app | apps], startApps, acc, visited) do
    case :lists.member(app, visited) do
      true ->
        get_runtime_deps(apps, startApps, acc, visited)

      false ->
        appFile = :code.where_is_file(:erlang.atom_to_list(app) ++ '.app')
        {:ok, [{:application, ^app, attrs}]} = :file.consult(appFile)

        runtimeDeps =
          :lists.flatmap(
            fn str ->
              [runtimeAppStr, _] = :string.lexemes(str, '-')
              runtimeApp = :erlang.list_to_atom(runtimeAppStr)

              case {:lists.keymember(runtimeApp, 1, acc),
                    :lists.member(
                      runtimeApp,
                      startApps
                    )} do
                {false, false}
                when runtimeApp !== :erts ->
                  [runtimeApp]

                _ ->
                  []
              end
            end,
            :proplists.get_value(:runtime_dependencies, attrs, [])
          )

        runtimeDepsVsns =
          for runtimeApp <- runtimeDeps do
            _ = :application.load(runtimeApp)

            {:ok, runtimeVsn} =
              :application.get_key(
                runtimeApp,
                :vsn
              )

            {runtimeApp, runtimeVsn, :none}
          end

        {depsRuntimeDeps, newVisited} =
          get_runtime_deps(runtimeDeps, startApps, acc ++ runtimeDepsVsns, [app | visited])

        get_runtime_deps(apps, startApps, depsRuntimeDeps, newVisited)
    end
  end

  defp get_runtime_deps([], _, acc, visited) do
    {acc, visited}
  end

  defp restart_type(app)
       when app == :kernel or app == :stdlib or
              app == :sasl do
    :permanent
  end

  defp restart_type(_) do
    :temporary
  end

  defp copy_file(src, dest) do
    copy_file(src, dest, [])
  end

  defp copy_file(src, dest, opts) do
    {:ok, _} = :file.copy(src, dest)

    case :lists.member(:preserve, opts) do
      true ->
        {:ok, fileInfo} = :file.read_file_info(src)
        :ok = :file.write_file_info(dest, fileInfo)

      false ->
        :ok
    end
  end

  defp write_file(fName, conts) do
    :file.write_file(
      fName,
      :unicode.characters_to_binary(conts)
    )
  end

  defp subst_src_scripts(scripts, srcDir, destDir, vars, opts) do
    :lists.foreach(
      fn script ->
        subst_src_script(script, srcDir, destDir, vars, opts)
      end,
      scripts
    )
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
    {:ok, bin} = :file.read_file(src)
    conts = :unicode.characters_to_list(bin)
    nConts = subst(conts, vars)
    :ok = write_file(dest, nConts)

    case :lists.member(:preserve, opts) do
      true ->
        {:ok, fileInfo} = :file.read_file_info(src)
        :file.write_file_info(dest, fileInfo)

      false ->
        :ok
    end
  end

  defp subst(str, [{var, val} | vars]) do
    subst(
      :re.replace(str, '%' ++ var ++ '%', val, [{:return, :list}, :unicode]),
      vars
    )
  end

  defp subst(str, []) do
    str
  end

  defp start_node(start, expVsn, expAppsVsns) do
    port = :erlang.open_port({:spawn_executable, start}, [])
    :erlang.unlink(port)
    :erlang.port_close(port)
    wait_node_up(:permanent, expVsn, expAppsVsns)
  end

  defp wait_node_up(expStatus, expVsn, expAppsVsns0) do
    node = node_name(:"ct_release_test-upgrade")

    expAppsVsns =
      for {a, v, _D} <- expAppsVsns0 do
        {a, v}
      end

    wait_node_up(node, expStatus, expVsn, :lists.keysort(1, expAppsVsns), 60)
  end

  defp wait_node_up(node, expStatus, expVsn, expAppsVsns, 0) do
    :test_server.fail(
      {:node_not_started, :app_check_failed, expVsn, expAppsVsns,
       :rpc.call(node, :release_handler, :which_releases, [expStatus]),
       :rpc.call(node, :application, :which_applications, [])}
    )
  end

  defp wait_node_up(node, expStatus, expVsn, expAppsVsns, n) do
    case {:rpc.call(node, :release_handler, :which_releases, [expStatus]),
          :rpc.call(node, :application, :which_applications, [])} do
      {[{_, ^expVsn, _, _}], apps} when is_list(apps) ->
        case (for {a, _, v} <- :lists.keysort(1, apps),
                  :lists.keymember(a, 1, expAppsVsns) do
                {a, v}
              end) do
          ^expAppsVsns ->
            {:ok, node}

          _ ->
            :timer.sleep(2000)
            wait_node_up(node, expStatus, expVsn, expAppsVsns, n - 1)
        end

      _ ->
        :timer.sleep(2000)
        wait_node_up(node, expStatus, expVsn, expAppsVsns, n - 1)
    end
  end

  defp node_name(sname) do
    {:ok, host} = :inet.gethostname()
    :erlang.list_to_atom(:erlang.atom_to_list(sname) ++ '@' ++ host)
  end

  defp rm_rf(dir) do
    case :file.read_file_info(dir) do
      {:ok, r_file_info(type: :directory)} ->
        {:ok, content} = :file.list_dir_all(dir)

        for c <- content do
          rm_rf(:filename.join(dir, c))
        end

        :ok = :file.del_dir(dir)
        :ok

      {:ok, r_file_info()} ->
        :ok = :file.delete(dir)

      _ ->
        :ok
    end
  end

  defp delete_file(fileName) do
    case :file.delete(fileName) do
      {:error, :enoent} ->
        :ok

      else__ ->
        else__
    end
  end

  defp make_dir(dir) do
    case :file.make_dir(dir) do
      {:error, :eexist} ->
        :ok

      else__ ->
        else__
    end
  end
end
