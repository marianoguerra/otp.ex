defmodule :m_release_handler do
  use Bitwise
  @behaviour :gen_server
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

  Record.defrecord(:r_state, :state,
    unpurged: [],
    root: :undefined,
    rel_dir: :undefined,
    releases: :undefined,
    timer: :undefined,
    start_prg: :undefined,
    masters: false,
    client_dir: false,
    static_emulator: false,
    pre_sync_nodes: []
  )

  Record.defrecord(:r_release, :release,
    name: :undefined,
    vsn: :undefined,
    erts_vsn: :undefined,
    libs: [],
    status: :undefined
  )

  def start_link() do
    :gen_server.start_link({:local, :release_handler}, :release_handler, [], [])
  end

  def unpack_release(releaseName) do
    call({:unpack_release, releaseName})
  end

  def check_install_release(vsn) do
    check_install_release(vsn, [])
  end

  def check_install_release(vsn, opts) do
    case check_check_install_options(opts, false) do
      {:ok, purge} ->
        call({:check_install_release, vsn, purge})

      error ->
        error
    end
  end

  defp check_check_install_options([:purge | opts], _) do
    check_check_install_options(opts, true)
  end

  defp check_check_install_options([illegal | _], _Purge) do
    {:error, {:illegal_option, illegal}}
  end

  defp check_check_install_options([], purge) do
    {:ok, purge}
  end

  def install_release(vsn) do
    call({:install_release, vsn, :restart, []})
  end

  def install_release(vsn, opt) do
    case check_install_options(opt, :restart, []) do
      {:ok, errorAction, installOpt} ->
        call({:install_release, vsn, errorAction, installOpt})

      error ->
        error
    end
  end

  defp check_install_options([opt | opts], errAct, instOpts) do
    case install_option(opt) do
      {:error_action, eAct} ->
        check_install_options(opts, eAct, instOpts)

      true ->
        check_install_options(opts, errAct, [opt | instOpts])

      false ->
        {:error, {:illegal_option, opt}}
    end
  end

  defp check_install_options([], errAct, instOpts) do
    {:ok, errAct, instOpts}
  end

  defp install_option(opt = {:error_action, :reboot}) do
    opt
  end

  defp install_option(opt = {:error_action, :restart}) do
    opt
  end

  defp install_option({:code_change_timeout, timeOut}) do
    check_timeout(timeOut)
  end

  defp install_option({:suspend_timeout, timeOut}) do
    check_timeout(timeOut)
  end

  defp install_option({:update_paths, bool})
       when bool == true or
              bool == false do
    true
  end

  defp install_option(_Opt) do
    false
  end

  defp check_timeout(:infinity) do
    true
  end

  defp check_timeout(int) when is_integer(int) and int > 0 do
    true
  end

  defp check_timeout(_Else) do
    false
  end

  def new_emulator_upgrade(vsn, opts) do
    result = call({:install_release, vsn, :reboot, opts})

    :error_logger.info_msg(
      '~w:install_release(~p,~p) completed after node restart with new emulator version~nResult: ~p~n',
      [:release_handler, vsn, opts, result]
    )

    result
  end

  def make_permanent(vsn) do
    call({:make_permanent, vsn})
  end

  def reboot_old_release(vsn) do
    call({:reboot_old_release, vsn})
  end

  def remove_release(vsn) do
    call({:remove_release, vsn})
  end

  def set_unpacked(relFile, libDirs) do
    call({:set_unpacked, relFile, libDirs})
  end

  def set_removed(vsn) do
    call({:set_removed, vsn})
  end

  def install_file(vsn, file) when is_list(file) do
    call({:install_file, file, vsn})
  end

  def which_releases() do
    call(:which_releases)
  end

  def which_releases(status) do
    releases = which_releases()
    get_releases_with_status(releases, status, [])
  end

  defp check_script(script, libDirs) do
    :release_handler_1.check_script(script, libDirs)
  end

  defp eval_script(script, apps, libDirs, newLibs, opts) do
    try do
      :release_handler_1.eval_script(script, apps, libDirs, newLibs, opts)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  def create_RELEASES([root, relFile | libDirs]) do
    create_RELEASES(root, :filename.join(root, 'releases'), relFile, libDirs)
  end

  def create_RELEASES(root, relFile) do
    create_RELEASES(root, :filename.join(root, 'releases'), relFile, [])
  end

  def create_RELEASES(root, relDir, relFile, libDirs) do
    case (try do
            check_rel(root, relFile, libDirs, false)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, reason} ->
        {:error, reason}

      rel ->
        rel2 = r_release(rel, status: :permanent)

        try do
          write_releases(relDir, [rel2], false)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end
    end
  end

  def upgrade_app(app, newDir) do
    try do
      upgrade_script(app, newDir)
    catch
      reason ->
        {:error, reason}
    else
      {:ok, newVsn, script} ->
        eval_appup_script(app, newVsn, newDir, script)
    end
  end

  def downgrade_app(app, oldDir) do
    case :string.lexemes(:filename.basename(oldDir), '-') do
      [_AppS, oldVsn] ->
        downgrade_app(app, oldVsn, oldDir)

      _ ->
        {:error, {:unknown_version, app}}
    end
  end

  def downgrade_app(app, oldVsn, oldDir) do
    try do
      downgrade_script(app, oldVsn, oldDir)
    catch
      reason ->
        {:error, reason}
    else
      {:ok, script} ->
        eval_appup_script(app, oldVsn, oldDir, script)
    end
  end

  def upgrade_script(app, newDir) do
    oldVsn = ensure_running(app)
    oldDir = :code.lib_dir(app)
    {newVsn, script} = find_script(app, newDir, oldVsn, :up)
    oldAppl = read_app(app, oldVsn, oldDir)
    newAppl = read_app(app, newVsn, newDir)

    case :systools_rc.translate_scripts(:up, [script], [newAppl], [oldAppl]) do
      {:ok, lowLevelScript} ->
        {:ok, newVsn, lowLevelScript}

      {:error, _SystoolsRC, reason} ->
        throw(reason)
    end
  end

  def downgrade_script(app, oldVsn, oldDir) do
    newVsn = ensure_running(app)
    newDir = :code.lib_dir(app)
    {^newVsn, script} = find_script(app, newDir, oldVsn, :down)
    oldAppl = read_app(app, oldVsn, oldDir)
    newAppl = read_app(app, newVsn, newDir)

    case :systools_rc.translate_scripts(:dn, [script], [oldAppl], [newAppl]) do
      {:ok, lowLevelScript} ->
        {:ok, lowLevelScript}

      {:error, _SystoolsRC, reason} ->
        throw(reason)
    end
  end

  def eval_appup_script(app, toVsn, toDir, script) do
    envBefore = :application_controller.prep_config_change()
    appSpecL = read_appspec(app, toDir)

    res =
      :release_handler_1.eval_script(script, [], [{app, toVsn, toDir}], [{app, toVsn, toDir}], [])

    case res do
      {:ok, _Unpurged} ->
        :application_controller.change_application_data(
          appSpecL,
          []
        )

        :application_controller.config_change(envBefore)

      _Res ->
        :ignore
    end

    res
  end

  defp ensure_running(app) do
    case :lists.keysearch(app, 1, :application.which_applications()) do
      {:value, {_App, _Descr, vsn}} ->
        vsn

      false ->
        throw({:app_not_running, app})
    end
  end

  defp find_script(app, dir, oldVsn, upOrDown) do
    appup = :filename.join([dir, 'ebin', :erlang.atom_to_list(app) ++ '.appup'])

    case :file.consult(appup) do
      {:ok, [{newVsn, upFromScripts, downToScripts}]} ->
        scripts =
          case upOrDown do
            :up ->
              upFromScripts

            :down ->
              downToScripts
          end

        case :systools_relup.appup_search_for_version(
               oldVsn,
               scripts
             ) do
          {:ok, script} ->
            {newVsn, script}

          :error ->
            throw({:version_not_in_appup, oldVsn})
        end

      {:error, :enoent} ->
        throw(:no_appup_found)

      {:error, reason} ->
        throw(reason)
    end
  end

  defp read_app(app, vsn, dir) do
    appS = :erlang.atom_to_list(app)
    path = [:filename.join(dir, 'ebin')]

    case :systools_make.read_application(appS, vsn, path, []) do
      {:ok, appl} ->
        appl

      {:error, {:not_found, _AppFile}} ->
        throw({:no_app_found, vsn, dir})

      {:error, reason} ->
        throw(reason)
    end
  end

  defp read_appspec(app, dir) do
    appS = :erlang.atom_to_list(app)
    path = [:filename.join(dir, 'ebin')]

    case :file.path_consult(path, appS ++ '.app') do
      {:ok, appSpecL, _File} ->
        appSpecL

      {:error, reason} ->
        throw(reason)
    end
  end

  defp call(req) do
    :gen_server.call(:release_handler, req, :infinity)
  end

  def init([]) do
    {:ok, [[root]]} = :init.get_argument(:root)
    {cliDir, masters} = is_client()

    releaseDir =
      case :application.get_env(
             :sasl,
             :releases_dir
           ) do
        :undefined ->
          case :os.getenv('RELDIR') do
            false ->
              cond do
                cliDir == false ->
                  :filename.join([root, 'releases'])

                true ->
                  :filename.join([cliDir, 'releases'])
              end

            rELDIR ->
              rELDIR
          end

        {:ok, dir} ->
          dir
      end

    releases =
      case consult(
             :filename.join(releaseDir, 'RELEASES'),
             masters
           ) do
        {:ok, [term]} ->
          transform_release(releaseDir, term, masters)

        _ ->
          {name, vsn} = :init.script_id()
          [r_release(name: name, vsn: vsn, status: :permanent)]
      end

    startPrg =
      case :application.get_env(:start_prg) do
        {:ok, found2} when is_list(found2) ->
          {:do_check, found2}

        _ ->
          {:no_check, :filename.join([root, 'bin', 'start'])}
      end

    static =
      case :application.get_env(:static_emulator) do
        {:ok, sFlag} when is_atom(sFlag) ->
          sFlag

        _ ->
          false
      end

    {:ok,
     r_state(
       root: root,
       rel_dir: releaseDir,
       releases: releases,
       start_prg: startPrg,
       masters: masters,
       client_dir: cliDir,
       static_emulator: static
     )}
  end

  def handle_call({:unpack_release, releaseName}, _From, s)
      when r_state(s, :masters) == false do
    case (try do
            do_unpack_release(
              r_state(s, :root),
              r_state(s, :rel_dir),
              releaseName,
              r_state(s, :releases)
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newReleases, vsn} ->
        {:reply, {:ok, vsn}, r_state(s, releases: newReleases)}

      {:error, reason} ->
        {:reply, {:error, reason}, s}

      {:EXIT, reason} ->
        {:reply, {:error, reason}, s}
    end
  end

  def handle_call({:unpack_release, _ReleaseName}, _From, s) do
    {:reply, {:error, :client_node}, s}
  end

  def handle_call({:check_install_release, vsn, purge}, _From, s) do
    case (try do
            do_check_install_release(
              r_state(s, :rel_dir),
              vsn,
              r_state(s, :releases),
              r_state(s, :masters),
              purge
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, currentVsn, descr} ->
        {:reply, {:ok, currentVsn, descr}, s}

      {:error, reason} ->
        {:reply, {:error, reason}, s}

      {:EXIT, reason} ->
        {:reply, {:error, reason}, s}
    end
  end

  def handle_call({:install_release, vsn, errorAction, opts}, from, s) do
    nS = resend_sync_nodes(s)

    case (try do
            do_install_release(s, vsn, opts)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newReleases, [], currentVsn, descr} ->
        {:reply, {:ok, currentVsn, descr}, r_state(nS, releases: newReleases)}

      {:ok, newReleases, unpurged, currentVsn, descr} ->
        timer =
          case r_state(s, :timer) do
            :undefined ->
              {:ok, ref} = :timer.send_interval(10000, :timeout)
              ref

            ref ->
              ref
          end

        newS = r_state(nS, releases: newReleases, unpurged: unpurged, timer: timer)
        {:reply, {:ok, currentVsn, descr}, newS}

      {:error, reason} ->
        {:reply, {:error, reason}, nS}

      {:restart_emulator, currentVsn, descr} ->
        :gen_server.reply(from, {:ok, currentVsn, descr})
        :init.reboot()
        {:noreply, nS}

      {:restart_new_emulator, currentVsn, descr} ->
        :gen_server.reply(
          from,
          {:continue_after_restart, currentVsn, descr}
        )

        :init.reboot()
        {:noreply, nS}

      {:EXIT, reason} ->
        :io.format('release_handler:install_release(Vsn=~tp Opts=~tp) failed, Reason=~tp~n', [
          vsn,
          opts,
          reason
        ])

        :gen_server.reply(from, {:error, reason})

        case errorAction do
          :restart ->
            :init.restart()

          :reboot ->
            :init.reboot()
        end

        {:noreply, nS}
    end
  end

  def handle_call({:make_permanent, vsn}, _From, s) do
    case (try do
            do_make_permanent(s, vsn)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, releases, unpurged} ->
        {:reply, :ok, r_state(s, releases: releases, unpurged: unpurged)}

      {:error, reason} ->
        {:reply, {:error, reason}, s}

      {:EXIT, reason} ->
        {:reply, {:error, reason}, s}
    end
  end

  def handle_call({:reboot_old_release, vsn}, from, s) do
    case (try do
            do_reboot_old_release(s, vsn)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :ok ->
        :gen_server.reply(from, :ok)
        :init.reboot()
        {:noreply, s}

      {:error, reason} ->
        {:reply, {:error, reason}, s}

      {:EXIT, reason} ->
        {:reply, {:error, reason}, s}
    end
  end

  def handle_call({:remove_release, vsn}, _From, s)
      when r_state(s, :masters) == false do
    case (try do
            do_remove_release(r_state(s, :root), r_state(s, :rel_dir), vsn, r_state(s, :releases))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newReleases} ->
        {:reply, :ok, r_state(s, releases: newReleases)}

      {:error, reason} ->
        {:reply, {:error, reason}, s}

      {:EXIT, reason} ->
        {:reply, {:error, reason}, s}
    end
  end

  def handle_call({:remove_release, _Vsn}, _From, s) do
    {:reply, {:error, :client_node}, s}
  end

  def handle_call({:set_unpacked, relFile, libDirs}, _From, s) do
    root = r_state(s, :root)

    case (try do
            do_set_unpacked(
              root,
              r_state(s, :rel_dir),
              relFile,
              libDirs,
              r_state(s, :releases),
              r_state(s, :masters)
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newReleases, vsn} ->
        {:reply, {:ok, vsn}, r_state(s, releases: newReleases)}

      {:error, reason} ->
        {:reply, {:error, reason}, s}

      {:EXIT, reason} ->
        {:reply, {:error, reason}, s}
    end
  end

  def handle_call({:set_removed, vsn}, _From, s) do
    case (try do
            do_set_removed(r_state(s, :rel_dir), vsn, r_state(s, :releases), r_state(s, :masters))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newReleases} ->
        {:reply, :ok, r_state(s, releases: newReleases)}

      {:error, reason} ->
        {:reply, {:error, reason}, s}

      {:EXIT, reason} ->
        {:reply, {:error, reason}, s}
    end
  end

  def handle_call({:install_file, file, vsn}, _From, s) do
    reply =
      case :lists.keysearch(vsn, r_release(:vsn), r_state(s, :releases)) do
        {:value, _} ->
          dir = :filename.join([r_state(s, :rel_dir), vsn])

          try do
            copy_file(file, dir, r_state(s, :masters))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        _ ->
          {:error, {:no_such_release, vsn}}
      end

    {:reply, reply, s}
  end

  def handle_call(:which_releases, _From, s) do
    reply =
      :lists.map(
        fn r_release(name: name, vsn: vsn, libs: libs, status: status) ->
          {name, vsn, mk_lib_name(libs), status}
        end,
        r_state(s, :releases)
      )

    {:reply, reply, s}
  end

  defp mk_lib_name([{libName, vsn, _Dir} | t]) do
    [:lists.concat([libName, '-', vsn]) | mk_lib_name(t)]
  end

  defp mk_lib_name([]) do
    []
  end

  def handle_info(:timeout, s) do
    case soft_purge(r_state(s, :unpurged)) do
      [] ->
        _ = :timer.cancel(r_state(s, :timer))
        {:noreply, r_state(s, unpurged: [], timer: :undefined)}

      unpurged ->
        {:noreply, r_state(s, unpurged: unpurged)}
    end
  end

  def handle_info({:sync_nodes, id, node}, s) do
    pSN = r_state(s, :pre_sync_nodes)
    {:noreply, r_state(s, pre_sync_nodes: [{:sync_nodes, id, node} | pSN])}
  end

  def handle_info(msg, state) do
    :error_logger.info_msg('release_handler: got unknown message: ~p~n', [msg])
    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def handle_cast(_Msg, state) do
    {:noreply, state}
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp is_client() do
    case :application.get_env(:masters) do
      {:ok, masters} ->
        alive = :erlang.is_alive()

        case atom_list(masters) do
          true when alive == true ->
            case :application.get_env(:client_directory) do
              {:ok, clientDir} ->
                case int_list(clientDir) do
                  true ->
                    {clientDir, masters}

                  _ ->
                    exit({:bad_parameter, :client_directory, clientDir})
                end

              _ ->
                {false, false}
            end

          _ ->
            exit({:bad_parameter, :masters, masters})
        end

      _ ->
        {false, false}
    end
  end

  defp atom_list([a | t]) when is_atom(a) do
    atom_list(t)
  end

  defp atom_list([]) do
    true
  end

  defp atom_list(_) do
    false
  end

  defp int_list([i | t]) when is_integer(i) do
    int_list(t)
  end

  defp int_list([]) do
    true
  end

  defp int_list(_) do
    false
  end

  defp resend_sync_nodes(s) do
    :lists.foreach(
      fn msg ->
        send(self(), msg)
      end,
      r_state(s, :pre_sync_nodes)
    )

    r_state(s, pre_sync_nodes: [])
  end

  defp soft_purge(unpurged) do
    :lists.filter(
      fn {mod, _PostPurgeMethod} ->
        case :code.soft_purge(mod) do
          true ->
            false

          false ->
            true
        end
      end,
      unpurged
    )
  end

  defp brutal_purge(unpurged) do
    :lists.filter(
      fn
        {mod, :brutal_purge} ->
          :code.purge(mod)
          false

        _ ->
          true
      end,
      unpurged
    )
  end

  defp do_unpack_release(root, relDir, releaseName, releases) do
    tar = :filename.join(relDir, releaseName ++ '.tar.gz')
    do_check_file(tar, :regular)
    rel = releaseName ++ '.rel'
    _ = extract_rel_file(:filename.join('releases', rel), tar, root)
    relFile = :filename.join(relDir, rel)
    release = check_rel(root, relFile, false)
    r_release(vsn: vsn) = release

    case :lists.keysearch(vsn, r_release(:vsn), releases) do
      {:value, _} ->
        throw({:error, {:existing_release, vsn}})

      _ ->
        :ok
    end

    extract_tar(root, tar)
    newReleases = [r_release(release, status: :unpacked) | releases]
    write_releases(relDir, newReleases, false)
    dir = :filename.join([relDir, vsn])
    copy_file(relFile, dir, false)
    _ = :file.delete(tar)
    _ = :file.delete(relFile)
    {:ok, newReleases, vsn}
  end

  defp check_rel(root, relFile, masters) do
    check_rel(root, relFile, [], masters)
  end

  defp check_rel(root, relFile, libDirs, masters) do
    case consult(relFile, masters) do
      {:ok, [relData]} ->
        check_rel_data(relData, root, libDirs, masters)

      {:ok, _} ->
        throw({:error, {:bad_rel_file, relFile}})

      {:error, reason} when is_tuple(reason) ->
        throw({:error, {:bad_rel_file, relFile}})

      {:error, fileError} ->
        throw({:error, {fileError, relFile}})
    end
  end

  defp check_rel_data({:release, {name, vsn}, {:erts, eVsn}, libs}, root, libDirs, masters) do
    libs2 =
      :lists.map(
        fn libSpec ->
          lib = :erlang.element(1, libSpec)
          libVsn = :erlang.element(2, libSpec)
          libName = :lists.concat([lib, '-', libVsn])

          libDir =
            case :lists.keysearch(lib, 1, libDirs) do
              {:value, {_Lib, _Vsn, dir}} ->
                path = :filename.join(dir, libName)
                check_path(path, masters)
                path

              _ ->
                :filename.join([root, 'lib', libName])
            end

          {lib, libVsn, libDir}
        end,
        libs
      )

    r_release(name: name, vsn: vsn, erts_vsn: eVsn, libs: libs2, status: :unpacking)
  end

  defp check_rel_data(relData, _Root, _LibDirs, _Masters) do
    throw({:error, {:bad_rel_data, relData}})
  end

  defp check_path(path) do
    check_path_response(path, :file.read_file_info(path))
  end

  defp check_path(path, false) do
    check_path(path)
  end

  defp check_path(path, masters) do
    check_path_master(masters, path)
  end

  defp check_path_master([master | ms], path) do
    case :rpc.call(master, :file, :read_file_info, [path]) do
      {:badrpc, _} ->
        consult_master(ms, path)

      res ->
        check_path_response(path, res)
    end
  end

  defp check_path_master([], _Path) do
    {:error, :no_master}
  end

  defp check_path_response(_Path, {:ok, info})
       when r_file_info(info, :type) == :directory do
    :ok
  end

  defp check_path_response(path, {:ok, _Info}) do
    throw({:error, {:not_a_directory, path}})
  end

  defp check_path_response(path, {:error, _Reason}) do
    throw({:error, {:no_such_directory, path}})
  end

  defp do_check_install_release(relDir, vsn, releases, masters, purge) do
    case :lists.keysearch(vsn, r_release(:vsn), releases) do
      {:value, r_release(status: :current)} ->
        {:error, {:already_installed, vsn}}

      {:value, release} ->
        latestRelease = get_latest_release(releases)
        vsnDir = :filename.join([relDir, vsn])
        check_file(:filename.join(vsnDir, 'start.boot'), :regular, masters)
        isRelup = check_opt_file(:filename.join(vsnDir, 'relup'), :regular, masters)
        check_opt_file(:filename.join(vsnDir, 'sys.config'), :regular, masters)
        libs = r_release(release, :libs)

        :lists.foreach(
          fn {_Lib, _LibVsn, libDir} ->
            check_file(libDir, :directory, masters)
            ebin = :filename.join(libDir, 'ebin')
            check_file(ebin, :directory, masters)
          end,
          libs
        )

        cond do
          isRelup ->
            case get_rh_script(latestRelease, release, relDir, masters) do
              {:ok, {currentVsn, descr, script}} ->
                case (try do
                        check_script(script, libs)
                      catch
                        :error, e -> {:EXIT, {e, __STACKTRACE__}}
                        :exit, e -> {:EXIT, e}
                        e -> e
                      end) do
                  {:ok, softPurgeMods} when purge === true ->
                    {:ok, brutalPurgeMods} =
                      :release_handler_1.check_old_processes(
                        script,
                        :brutal_purge
                      )

                    :lists.foreach(
                      fn mod ->
                        try do
                          :erlang.purge_module(mod)
                        catch
                          :error, e -> {:EXIT, {e, __STACKTRACE__}}
                          :exit, e -> {:EXIT, e}
                          e -> e
                        end
                      end,
                      softPurgeMods ++ brutalPurgeMods
                    )

                    {:ok, currentVsn, descr}

                  {:ok, _} ->
                    {:ok, currentVsn, descr}

                  else__ ->
                    else__
                end

              error ->
                error
            end

          true ->
            {:ok, vsn, ''}
        end

      _ ->
        {:error, {:no_such_release, vsn}}
    end
  end

  defp do_install_release(
         r_state(
           start_prg: startPrg,
           root: rootDir,
           rel_dir: relDir,
           releases: releases,
           masters: masters,
           static_emulator: static
         ),
         vsn,
         opts
       ) do
    case :lists.keysearch(vsn, r_release(:vsn), releases) do
      {:value, r_release(status: :current)} ->
        {:error, {:already_installed, vsn}}

      {:value, release} ->
        latestRelease = get_latest_release(releases)

        case get_rh_script(latestRelease, release, relDir, masters) do
          {:ok, {_CurrentVsn, _Descr, [:restart_new_emulator | _Script]}}
          when static == true ->
            throw(:static_emulator)

          {:ok, {currentVsn, descr, [:restart_new_emulator | _Script]}} ->
            {tmpVsn, tmpRelease} =
              new_emulator_make_tmp_release(latestRelease, release, relDir, opts, masters)

            nReleases = [tmpRelease | releases]

            prepare_restart_new_emulator(
              startPrg,
              rootDir,
              relDir,
              tmpVsn,
              tmpRelease,
              nReleases,
              masters
            )

            {:restart_new_emulator, currentVsn, descr}

          {:ok, {currentVsn, descr, script}} ->
            nReleases =
              new_emulator_rm_tmp_release(
                r_release(latestRelease, :vsn),
                r_release(latestRelease, :erts_vsn),
                vsn,
                relDir,
                releases,
                masters
              )

            mon_nodes(true)
            envBefore = :application_controller.prep_config_change()
            apps = change_appl_data(relDir, release, masters)
            libDirs = r_release(release, :libs)

            newLibs =
              get_new_libs(
                r_release(latestRelease, :libs),
                r_release(release, :libs)
              )

            case eval_script(script, apps, libDirs, newLibs, opts) do
              {:ok, unpurged} ->
                :application_controller.config_change(envBefore)
                mon_nodes(false)
                nReleases1 = set_status(vsn, :current, nReleases)
                {:ok, nReleases1, unpurged, currentVsn, descr}

              :restart_emulator when static == true ->
                throw(:static_emulator)

              :restart_emulator ->
                mon_nodes(false)

                prepare_restart_new_emulator(
                  startPrg,
                  rootDir,
                  relDir,
                  vsn,
                  release,
                  nReleases,
                  masters
                )

                {:restart_emulator, currentVsn, descr}

              else__ ->
                :application_controller.config_change(envBefore)
                mon_nodes(false)
                else__
            end

          error ->
            error
        end

      _ ->
        {:error, {:no_such_release, vsn}}
    end
  end

  defp new_emulator_make_tmp_release(currentRelease, toRelease, relDir, opts, masters) do
    currentVsn = r_release(currentRelease, :vsn)
    toVsn = r_release(toRelease, :vsn)
    tmpVsn = '__new_emulator__' ++ currentVsn

    case get_base_libs(r_release(toRelease, :libs)) do
      {:ok, {kernel, stdlib, sasl}, _} ->
        case get_base_libs(r_release(currentRelease, :libs)) do
          {:ok, _, restLibs} ->
            tmpErtsVsn = r_release(toRelease, :erts_vsn)
            tmpLibs = [kernel, stdlib, sasl | restLibs]

            tmpRelease =
              r_release(currentRelease,
                vsn: tmpVsn,
                erts_vsn: tmpErtsVsn,
                libs: tmpLibs,
                status: :unpacked
              )

            new_emulator_make_hybrid_boot(currentVsn, toVsn, tmpVsn, relDir, opts, masters)
            new_emulator_make_hybrid_config(currentVsn, toVsn, tmpVsn, relDir, masters)
            {tmpVsn, tmpRelease}

          {:error, {:missing, missing}} ->
            throw({:error, {:missing_base_app, currentVsn, missing}})
        end

      {:error, {:missing, missing}} ->
        throw({:error, {:missing_base_app, toVsn, missing}})
    end
  end

  defp get_base_libs(libs) do
    get_base_libs(libs, :undefined, :undefined, :undefined, [])
  end

  defp get_base_libs([{:kernel, _, _} = kernel | libs], :undefined, stdlib, sasl, rest) do
    get_base_libs(libs, kernel, stdlib, sasl, rest)
  end

  defp get_base_libs([{:stdlib, _, _} = stdlib | libs], kernel, :undefined, sasl, rest) do
    get_base_libs(libs, kernel, stdlib, sasl, rest)
  end

  defp get_base_libs([{:sasl, _, _} = sasl | libs], kernel, stdlib, :undefined, rest) do
    get_base_libs(libs, kernel, stdlib, sasl, rest)
  end

  defp get_base_libs([lib | libs], kernel, stdlib, sasl, rest) do
    get_base_libs(libs, kernel, stdlib, sasl, [lib | rest])
  end

  defp get_base_libs([], :undefined, _Stdlib, _Sasl, _Rest) do
    {:error, {:missing, :kernel}}
  end

  defp get_base_libs([], _Kernel, :undefined, _Sasl, _Rest) do
    {:error, {:missing, :stdlib}}
  end

  defp get_base_libs([], _Kernel, _Stdlib, :undefined, _Rest) do
    {:error, {:missing, :sasl}}
  end

  defp get_base_libs([], kernel, stdlib, sasl, rest) do
    {:ok, {kernel, stdlib, sasl}, :lists.reverse(rest)}
  end

  defp new_emulator_make_hybrid_boot(currentVsn, toVsn, tmpVsn, relDir, opts, masters) do
    fromBootFile = :filename.join([relDir, currentVsn, 'start.boot'])
    toBootFile = :filename.join([relDir, toVsn, 'start.boot'])
    tmpBootFile = :filename.join([relDir, tmpVsn, 'start.boot'])
    ensure_dir(tmpBootFile, masters)
    args = [toVsn, opts]
    {:ok, fromBoot} = read_file(fromBootFile, masters)
    {:ok, toBoot} = read_file(toBootFile, masters)

    case :systools_make.make_hybrid_boot(tmpVsn, fromBoot, toBoot, args) do
      {:ok, tmpBoot} ->
        write_file(tmpBootFile, tmpBoot, masters)

      {:error, reason} ->
        throw({:error, {:could_not_create_hybrid_boot, reason}})
    end
  end

  defp new_emulator_make_hybrid_config(currentVsn, toVsn, tmpVsn, relDir, masters) do
    fromFile = :filename.join([relDir, currentVsn, 'sys.config'])
    toFile = :filename.join([relDir, toVsn, 'sys.config'])
    tmpFile = :filename.join([relDir, tmpVsn, 'sys.config'])

    fromConfig =
      case consult(fromFile, masters) do
        {:ok, [fC]} ->
          fC

        {:error, error1} ->
          :io.format('Warning: ~w cannot read ~tp: ~tp~n', [:release_handler, fromFile, error1])
          []
      end

    [kernel, stdlib, sasl] =
      case consult(
             toFile,
             masters
           ) do
        {:ok, [toConfig]} ->
          for app <- [:kernel, :stdlib, :sasl] do
            :lists.keyfind(app, 1, toConfig)
          end

        {:error, error2} ->
          :io.format(
            'Warning: ~w cannot read ~tp: ~tp~n',
            [:release_handler, toFile, error2]
          )

          [false, false, false]
      end

    config1 = replace_config(:kernel, fromConfig, kernel)
    config2 = replace_config(:stdlib, config1, stdlib)
    config3 = replace_config(:sasl, config2, sasl)

    configStr =
      :io_lib.format(
        '%% ~s~n~tp.~n',
        [:epp.encoding_to_string(:utf8), config3]
      )

    write_file(tmpFile, :unicode.characters_to_binary(configStr), masters)
  end

  defp replace_config(app, config, false) do
    :lists.keydelete(app, 1, config)
  end

  defp replace_config(app, config, appConfig) do
    :lists.keystore(app, 1, config, appConfig)
  end

  defp new_emulator_rm_tmp_release(
         '__new_emulator__' ++ _ = tmpVsn,
         eVsn,
         newVsn,
         relDir,
         releases,
         masters
       ) do
    case :os.type() do
      {:win32, :nt} ->
        rename_tmp_service(eVsn, tmpVsn, newVsn)

      _ ->
        :ok
    end

    remove_dir(:filename.join(relDir, tmpVsn), masters)
    :lists.keydelete(tmpVsn, r_release(:vsn), releases)
  end

  defp new_emulator_rm_tmp_release(_, _, _, _, releases, _) do
    releases
  end

  defp rename_tmp_service(eVsn, tmpVsn, newVsn) do
    fromName =
      hd(
        :string.lexemes(
          :erlang.atom_to_list(node()),
          '@'
        )
      ) ++ '_' ++ tmpVsn

    toName =
      hd(
        :string.lexemes(
          :erlang.atom_to_list(node()),
          '@'
        )
      ) ++ '_' ++ newVsn

    case :erlsrv.get_service(eVsn, toName) do
      {:error, _Error} ->
        :ok

      _Data ->
        {:ok, _} = :erlsrv.remove_service(toName)
        :ok
    end

    rename_service(eVsn, fromName, toName)
  end

  defp rename_service(eVsn, fromName, toName) do
    case :erlsrv.rename_service(eVsn, fromName, toName) do
      {:ok, _} ->
        case :erlsrv.get_service(eVsn, toName) do
          {:error, error1} ->
            throw({:error, error1})

          _Data2 ->
            :ok
        end

      error2 ->
        throw({:error, {:service_rename_failed, error2}})
    end
  end

  defp do_make_services_permanent(permanentVsn, vsn, permanentEVsn, eVsn) do
    permName =
      hd(
        :string.lexemes(
          :erlang.atom_to_list(node()),
          '@'
        )
      ) ++ '_' ++ permanentVsn

    name =
      hd(
        :string.lexemes(
          :erlang.atom_to_list(node()),
          '@'
        )
      ) ++ '_' ++ vsn

    case :erlsrv.get_service(eVsn, name) do
      {:error, _Error} ->
        case :os.getenv('ERLSRV_SERVICE_NAME') == permName do
          true ->
            rename_service(eVsn, permName, name)
            :os.putenv('ERLSRV_SERVICE_NAME', name)
            :heart.cycle()

          false ->
            throw({:error, :service_name_missmatch})
        end

      data ->
        updData = :erlsrv.new_service(name, data, [])

        case :erlsrv.store_service(eVsn, updData) do
          :ok ->
            {:ok, _} =
              :erlsrv.disable_service(
                permanentEVsn,
                permName
              )

            {:ok, _} = :erlsrv.enable_service(eVsn, name)
            {:ok, _} = :erlsrv.remove_service(permName)
            :os.putenv('ERLSRV_SERVICE_NAME', name)
            :ok = :heart.cycle()

          error4 ->
            throw(error4)
        end
    end
  end

  defp do_make_permanent(
         r_state(
           releases: releases,
           rel_dir: relDir,
           unpurged: unpurged,
           masters: masters,
           static_emulator: static
         ),
         vsn
       ) do
    case :lists.keysearch(vsn, r_release(:vsn), releases) do
      {:value, r_release(erts_vsn: eVsn, status: status)}
      when status != :unpacked and status != :old and
             status != :permanent ->
        dir = :filename.join([relDir, vsn])

        sys =
          case (try do
                  check_file(:filename.join(dir, 'sys.config'), :regular, masters)
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            :ok ->
              :filename.join(dir, 'sys')

            _ ->
              false
          end

        boot = :filename.join(dir, 'start.boot')
        check_file(boot, :regular, masters)
        set_permanent_files(relDir, eVsn, vsn, masters, static)
        newReleases = set_status(vsn, :permanent, releases)
        write_releases(relDir, newReleases, masters)

        case :os.type() do
          {:win32, :nt} ->
            {:value, permanentRelease} =
              :lists.keysearch(:permanent, r_release(:status), releases)

            permanentVsn = r_release(permanentRelease, :vsn)
            permanentEVsn = r_release(permanentRelease, :erts_vsn)

            case (try do
                    do_make_services_permanent(permanentVsn, vsn, permanentEVsn, eVsn)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:error, reason} ->
                throw({:error, {:service_update_failed, reason}})

              _ ->
                :ok
            end

          _ ->
            :ok
        end

        :ok = :init.make_permanent(:filename.join(dir, 'start'), sys)
        {:ok, newReleases, brutal_purge(unpurged)}

      {:value, r_release(status: :permanent)} ->
        {:ok, releases, unpurged}

      {:value, r_release(status: status)} ->
        {:error, {:bad_status, status}}

      false ->
        {:error, {:no_such_release, vsn}}
    end
  end

  defp do_back_service(oldVersion, currentVersion, oldEVsn, currentEVsn) do
    nN =
      hd(
        :string.lexemes(
          :erlang.atom_to_list(node()),
          '@'
        )
      )

    oldName = nN ++ '_' ++ oldVersion
    currentName = nN ++ '_' ++ currentVersion

    updData =
      case :erlsrv.get_service(
             currentEVsn,
             currentName
           ) do
        {:error, error} ->
          throw({:error, error})

        data ->
          :erlsrv.new_service(oldName, data, [])
      end

    _ =
      case :erlsrv.store_service(oldEVsn, updData) do
        :ok ->
          {:ok, _} =
            :erlsrv.disable_service(
              currentEVsn,
              currentName
            )

          {:ok, _} = :erlsrv.enable_service(oldEVsn, oldName)

        error2 ->
          throw(error2)
      end

    oldErlSrv = :filename.nativename(:erlsrv.erlsrv(oldEVsn))
    currentErlSrv = :filename.nativename(:erlsrv.erlsrv(currentEVsn))

    case :heart.set_cmd(
           currentErlSrv ++
             ' remove ' ++ currentName ++ ' & ' ++ oldErlSrv ++ ' start ' ++ oldName
         ) do
      :ok ->
        :ok

      error3 ->
        throw({:error, {:"heart:set_cmd() error", error3}})
    end
  end

  defp do_reboot_old_release(
         r_state(releases: releases, rel_dir: relDir, masters: masters, static_emulator: static),
         vsn
       ) do
    case :lists.keysearch(vsn, r_release(:vsn), releases) do
      {:value, r_release(erts_vsn: eVsn, status: :old)} ->
        currentRunning =
          case :os.type() do
            {:win32, :nt} ->
              case :lists.keysearch(:permanent, r_release(:status), releases) do
                false ->
                  :lists.keysearch(:current, r_release(:status), releases)

                {:value, cR} ->
                  cR
              end

            _ ->
              false
          end

        set_permanent_files(relDir, eVsn, vsn, masters, static)
        newReleases = set_status(vsn, :permanent, releases)
        write_releases(relDir, newReleases, masters)

        case :os.type() do
          {:win32, :nt} ->
            do_back_service(
              vsn,
              r_release(currentRunning, :vsn),
              eVsn,
              r_release(currentRunning, :erts_vsn)
            )

          _ ->
            :ok
        end

        :ok

      {:value, r_release(status: status)} ->
        {:error, {:bad_status, status}}

      false ->
        {:error, {:no_such_release, vsn}}
    end
  end

  defp set_permanent_files(relDir, eVsn, vsn, false, _) do
    write_start(:filename.join([relDir, 'start_erl.data']), eVsn ++ ' ' ++ vsn, false)
  end

  defp set_permanent_files(relDir, eVsn, vsn, masters, false) do
    write_start(:filename.join([relDir, 'start_erl.data']), eVsn ++ ' ' ++ vsn, masters)
  end

  defp set_permanent_files(relDir, _EVsn, vsn, masters, _Static) do
    vsnDir = :filename.join([relDir, vsn])
    set_static_files(vsnDir, relDir, masters)
  end

  defp do_remove_service(vsn) do
    serviceName =
      hd(
        :string.lexemes(
          :erlang.atom_to_list(node()),
          '@'
        )
      ) ++ '_' ++ vsn

    case :erlsrv.get_service(serviceName) do
      {:error, _Error} ->
        :ok

      _Data ->
        {:ok, _} = :erlsrv.remove_service(serviceName)
        :ok
    end
  end

  defp do_remove_release(root, relDir, vsn, releases) do
    case :lists.keysearch(vsn, r_release(:vsn), releases) do
      {:value, r_release(status: :permanent)} ->
        {:error, {:permanent, vsn}}

      {:value, r_release(libs: removeLibs, vsn: ^vsn, erts_vsn: eVsn)} ->
        case :os.type() do
          {:win32, :nt} ->
            do_remove_service(vsn)

          _ ->
            :ok
        end

        newReleases = :lists.keydelete(vsn, r_release(:vsn), releases)

        removeThese =
          :lists.foldl(
            fn r_release(libs: libs), remove ->
              diff_dir(remove, libs)
            end,
            removeLibs,
            newReleases
          )

        :lists.foreach(
          fn {_Lib, _LVsn, lDir} ->
            remove_file(lDir)
          end,
          removeThese
        )

        remove_file(:filename.join([relDir, vsn]))

        case :lists.keysearch(eVsn, r_release(:erts_vsn), newReleases) do
          {:value, _} ->
            :ok

          false ->
            remove_file(:filename.join(root, 'erts-' ++ eVsn))
        end

        write_releases(relDir, newReleases, false)
        {:ok, newReleases}

      false ->
        {:error, {:no_such_release, vsn}}
    end
  end

  defp do_set_unpacked(root, relDir, relFile, libDirs, releases, masters) do
    release = check_rel(root, relFile, libDirs, masters)
    r_release(vsn: vsn) = release

    case :lists.keysearch(vsn, r_release(:vsn), releases) do
      {:value, _} ->
        throw({:error, {:existing_release, vsn}})

      false ->
        :ok
    end

    newReleases = [r_release(release, status: :unpacked) | releases]
    vsnDir = :filename.join([relDir, vsn])
    make_dir(vsnDir, masters)
    write_releases(relDir, newReleases, masters)
    {:ok, newReleases, vsn}
  end

  defp do_set_removed(relDir, vsn, releases, masters) do
    case :lists.keysearch(vsn, r_release(:vsn), releases) do
      {:value, r_release(status: :permanent)} ->
        {:error, {:permanent, vsn}}

      {:value, _} ->
        newReleases = :lists.keydelete(vsn, r_release(:vsn), releases)
        write_releases(relDir, newReleases, masters)
        {:ok, newReleases}

      false ->
        {:error, {:no_such_release, vsn}}
    end
  end

  defp get_rh_script(
         r_release(vsn: '__new_emulator__' ++ currentVsn),
         r_release(vsn: toVsn),
         relDir,
         masters
       ) do
    {:ok,
     {vsn, descr,
      [
        :restart_new_emulator
        | script
      ]}} = do_get_rh_script(currentVsn, toVsn, relDir, masters)

    {:ok, {vsn, descr, script}}
  end

  defp get_rh_script(r_release(vsn: currentVsn), r_release(vsn: toVsn), relDir, masters) do
    do_get_rh_script(currentVsn, toVsn, relDir, masters)
  end

  defp do_get_rh_script(currentVsn, toVsn, relDir, masters) do
    relup = :filename.join([relDir, toVsn, 'relup'])

    case try_upgrade(toVsn, currentVsn, relup, masters) do
      {:ok, rhScript} ->
        {:ok, rhScript}

      _ ->
        relup2 = :filename.join([relDir, currentVsn, 'relup'])

        case try_downgrade(toVsn, currentVsn, relup2, masters) do
          {:ok, rhScript} ->
            {:ok, rhScript}

          _ ->
            throw({:error, {:no_matching_relup, toVsn, currentVsn}})
        end
    end
  end

  defp try_upgrade(toVsn, currentVsn, relup, masters) do
    case consult(relup, masters) do
      {:ok, [{^toVsn, listOfRhScripts, _}]} ->
        case :lists.keysearch(currentVsn, 1, listOfRhScripts) do
          {:value, rhScript} ->
            {:ok, rhScript}

          _ ->
            :error
        end

      {:ok, _} ->
        throw({:error, {:bad_relup_file, relup}})

      {:error, reason} when is_tuple(reason) ->
        throw({:error, {:bad_relup_file, relup}})

      {:error, :enoent} ->
        :error

      {:error, fileError} ->
        throw({:error, {fileError, relup}})
    end
  end

  defp try_downgrade(toVsn, currentVsn, relup, masters) do
    case consult(relup, masters) do
      {:ok, [{^currentVsn, _, listOfRhScripts}]} ->
        case :lists.keysearch(toVsn, 1, listOfRhScripts) do
          {:value, rhScript} ->
            {:ok, rhScript}

          _ ->
            :error
        end

      {:ok, _} ->
        throw({:error, {:bad_relup_file, relup}})

      {:error, reason} when is_tuple(reason) ->
        throw({:error, {:bad_relup_file, relup}})

      {:error, fileError} ->
        throw({:error, {fileError, relup}})
    end
  end

  defp set_status(vsn, status, releases) do
    :lists.zf(
      fn
        release
        when r_release(release, :vsn) == vsn and
               r_release(release, :status) == :permanent ->
          true

        release when r_release(release, :vsn) == vsn ->
          {true, r_release(release, status: status)}

        release when r_release(release, :status) == status ->
          {true, r_release(release, status: :old)}

        _ ->
          true
      end,
      releases
    )
  end

  defp get_latest_release(releases) do
    case :lists.keysearch(:current, r_release(:status), releases) do
      {:value, release} ->
        release

      false ->
        {:value, release} = :lists.keysearch(:permanent, r_release(:status), releases)
        release
    end
  end

  defp diff_dir([h | t], l) do
    case memlib(h, l) do
      true ->
        diff_dir(t, l)

      false ->
        [h | diff_dir(t, l)]
    end
  end

  defp diff_dir([], _) do
    []
  end

  defp memlib({lib, vsn, _Dir}, [{lib, vsn, _Dir2} | _T]) do
    true
  end

  defp memlib(lib, [_H | t]) do
    memlib(lib, t)
  end

  defp memlib(_Lib, []) do
    false
  end

  def remove_file(file) do
    case :file.read_link_info(file) do
      {:ok, info} when r_file_info(info, :type) == :directory ->
        case :file.list_dir(file) do
          {:ok, files} ->
            :lists.foreach(
              fn file2 ->
                remove_file(:filename.join(file, file2))
              end,
              files
            )

            case :file.del_dir(file) do
              :ok ->
                :ok

              {:error, reason} ->
                throw({:error, reason})
            end

          {:error, reason} ->
            throw({:error, reason})
        end

      {:ok, _Info} ->
        case :file.delete(file) do
          :ok ->
            :ok

          {:error, reason} ->
            throw({:error, reason})
        end

      {:error, _Reason} ->
        throw({:error, {:no_such_file, file}})
    end
  end

  def do_write_file(file, str) do
    do_write_file(file, str, [])
  end

  def do_write_file(file, str, fileOpts) do
    case :file.open(file, [:write | fileOpts]) do
      {:ok, fd} ->
        :io.put_chars(fd, str)
        :ok = :file.close(fd)

      {:error, reason} ->
        {:error, {reason, file}}
    end
  end

  defp change_appl_data(relDir, r_release(vsn: vsn), masters) do
    dir = :filename.join([relDir, vsn])
    bootFile = :filename.join(dir, 'start.boot')

    case read_file(bootFile, masters) do
      {:ok, bin} ->
        config =
          case consult(
                 :filename.join(dir, 'sys.config'),
                 masters
               ) do
            {:ok, [conf]} ->
              conf

            _ ->
              []
          end

        appls = get_appls(:erlang.binary_to_term(bin))

        case :application_controller.change_application_data(
               appls,
               config
             ) do
          :ok ->
            appls

          {:error, reason} ->
            exit({:change_appl_data, reason})
        end

      {:error, _Reason} ->
        throw({:error, {:no_such_file, bootFile}})
    end
  end

  defp get_appls({:script, _, script}) do
    get_appls(script, [])
  end

  defp get_appls(
         [
           {:kernelProcess, :application_controller, {:application_controller, :start, [app]}}
           | t
         ],
         res
       ) do
    get_appls(t, [app | res])
  end

  defp get_appls(
         [{:apply, {:application, :load, [app]}} | t],
         res
       ) do
    get_appls(t, [app | res])
  end

  defp get_appls([_ | t], res) do
    get_appls(t, res)
  end

  defp get_appls([], res) do
    res
  end

  defp mon_nodes(true) do
    :ok = :net_kernel.monitor_nodes(true)
  end

  defp mon_nodes(false) do
    :ok = :net_kernel.monitor_nodes(false)
    flush()
  end

  defp flush() do
    receive do
      {:nodedown, _} ->
        flush()

      {:nodeup, _} ->
        flush()
    after
      0 ->
        :ok
    end
  end

  defp prepare_restart_nt(
         r_release(erts_vsn: eVsn, vsn: vsn),
         r_release(erts_vsn: permEVsn, vsn: permVsn),
         dataFileName
       ) do
    currentServiceName =
      hd(
        :string.lexemes(
          :erlang.atom_to_list(node()),
          '@'
        )
      ) ++ '_' ++ permVsn

    futureServiceName =
      hd(
        :string.lexemes(
          :erlang.atom_to_list(node()),
          '@'
        )
      ) ++ '_' ++ vsn

    currentService =
      case :erlsrv.get_service(
             permEVsn,
             currentServiceName
           ) do
        {:error, _} = error1 ->
          throw(error1)

        cS ->
          cS
      end

    futureService =
      :erlsrv.new_service(
        futureServiceName,
        currentService,
        :filename.nativename(dataFileName),
        currentServiceName
      )

    case :erlsrv.store_service(eVsn, futureService) do
      {:error, _} = error2 ->
        throw(error2)

      _X ->
        {:ok, _} =
          :erlsrv.disable_service(
            eVsn,
            futureServiceName
          )

        erlSrv = :filename.nativename(:erlsrv.erlsrv(eVsn))
        startDisabled = erlSrv ++ ' start_disabled ' ++ futureServiceName

        case :heart.set_cmd(startDisabled) do
          :ok ->
            :ok

          error3 ->
            throw({:error, {:"heart:set_cmd() error", error3}})
        end
    end
  end

  defp prepare_restart_new_emulator(startPrg, rootDir, relDir, vsn, release, releases, masters) do
    {:value, pRelease} = :lists.keysearch(:permanent, r_release(:status), releases)
    nReleases1 = set_status(vsn, :current, releases)
    nReleases2 = set_status(vsn, :tmp_current, nReleases1)
    write_releases(relDir, nReleases2, masters)
    prepare_restart_new_emulator(startPrg, rootDir, relDir, release, pRelease, masters)
  end

  defp prepare_restart_new_emulator(startPrg, rootDir, relDir, release, pRelease, masters) do
    r_release(erts_vsn: eVsn, vsn: vsn) = release
    data = eVsn ++ ' ' ++ vsn
    dataFile = write_new_start_erl(data, relDir, masters)

    case :os.type() do
      {:win32, :nt} ->
        write_ini_file(rootDir, eVsn, masters)
        prepare_restart_nt(release, pRelease, dataFile)

      {:unix, _} ->
        startP = check_start_prg(startPrg, masters)

        case :heart.set_cmd(startP ++ ' ' ++ dataFile) do
          :ok ->
            :ok

          error ->
            throw({:error, {:"heart:set_cmd() error", error}})
        end
    end
  end

  defp check_start_prg({:do_check, startPrg}, masters) do
    check_file(startPrg, :regular, masters)
    startPrg
  end

  defp check_start_prg({_, startPrg}, _) do
    startPrg
  end

  defp write_new_start_erl(data, relDir, masters) do
    dataFile = :filename.join([relDir, 'new_start_erl.data'])
    write_file(dataFile, data, masters)
    dataFile
  end

  defp transform_release(releaseDir, releases, masters) do
    case :init.script_id() do
      {name, '__new_emulator__' ++ _ = tmpVsn} ->
        dReleases = :lists.keydelete(tmpVsn, r_release(:vsn), releases)
        write_releases(releaseDir, dReleases, masters)
        set_current({name, tmpVsn}, releases)

      scriptId ->
        f = fn
          release when r_release(release, :status) == :tmp_current ->
            r_release(release, status: :unpacked)

          release ->
            release
        end

        case :lists.map(f, releases) do
          ^releases ->
            releases

          dReleases ->
            write_releases(releaseDir, dReleases, masters)
            set_current(scriptId, releases)
        end
    end
  end

  defp set_current(scriptId, releases) do
    f1 = fn
      release when r_release(release, :status) == :tmp_current ->
        case scriptId do
          {_Name, vsn} when r_release(release, :vsn) == vsn ->
            r_release(release, status: :current)

          _ ->
            r_release(release, status: :unpacked)
        end

      release ->
        release
    end

    :lists.map(f1, releases)
  end

  defp check_opt_file(fileName, type, masters) do
    case (try do
            check_file(fileName, type, masters)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :ok ->
        true

      _Error ->
        :io.format('Warning: ~tp missing (optional)~n', [fileName])
        false
    end
  end

  defp check_file(fileName, type, false) do
    do_check_file(fileName, type)
  end

  defp check_file(fileName, type, masters) do
    check_file_masters(fileName, type, masters)
  end

  defp check_file_masters(fileName, type, [master | masters]) do
    do_check_file(master, fileName, type)
    check_file_masters(fileName, type, masters)
  end

  defp check_file_masters(_FileName, _Type, []) do
    :ok
  end

  defp do_check_file(fileName, type) do
    case :file.read_file_info(fileName) do
      {:ok, info} when r_file_info(info, :type) == type ->
        :ok

      {:error, _Reason} ->
        throw({:error, {:no_such_file, fileName}})
    end
  end

  defp do_check_file(master, fileName, type) do
    case :rpc.call(master, :file, :read_file_info, [fileName]) do
      {:ok, info} when r_file_info(info, :type) == type ->
        :ok

      _ ->
        throw({:error, {:no_such_file, {master, fileName}}})
    end
  end

  defp extract_rel_file(rel, tar, root) do
    _ =
      :erl_tar.extract(
        tar,
        [{:files, [rel]}, {:cwd, root}, :compressed]
      )
  end

  defp extract_tar(root, tar) do
    case :erl_tar.extract(
           tar,
           [:keep_old_files, {:cwd, root}, :compressed]
         ) do
      :ok ->
        :ok

      {:error, {name, reason}} ->
        throw({:error, {:cannot_extract_file, name, reason}})
    end
  end

  defp write_releases(dir, releases, masters) do
    newReleases =
      :lists.zf(
        fn
          release
          when r_release(release, :status) == :current ->
            {true, r_release(release, status: :unpacked)}

          _ ->
            true
        end,
        releases
      )

    write_releases_1(dir, newReleases, masters)
  end

  defp write_releases_1(dir, newReleases, false) do
    case do_write_release(dir, 'RELEASES', newReleases) do
      :ok ->
        :ok

      error ->
        throw(error)
    end
  end

  defp write_releases_1(dir, newReleases, masters) do
    all_masters(masters)
    write_releases_m(dir, newReleases, masters)
  end

  def do_write_release(dir, rELEASES, newReleases) do
    case :file.open(
           :filename.join(dir, rELEASES),
           [:write, {:encoding, :utf8}]
         ) do
      {:ok, fd} ->
        :ok = :io.format(fd, '%% ~s~n~tp.~n', [:epp.encoding_to_string(:utf8), newReleases])
        :ok = :file.close(fd)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp write_releases_m(dir, newReleases, masters) do
    relFile = :filename.join(dir, 'RELEASES')
    backup = :filename.join(dir, 'RELEASES.backup')
    change = :filename.join(dir, 'RELEASES.change')
    ensure_RELEASES_exists(masters, relFile)

    case at_all_masters(masters, :release_handler, :do_copy_files, [relFile, [backup, change]]) do
      :ok ->
        case at_all_masters(masters, :release_handler, :do_write_release, [
               dir,
               'RELEASES.change',
               newReleases
             ]) do
          :ok ->
            case at_all_masters(masters, :file, :rename, [change, relFile]) do
              :ok ->
                remove_files(:all, [backup, change], masters)
                :ok

              {:error, {master, r}} ->
                takewhile(master, masters, :file, :rename, [backup, relFile])
                remove_files(:all, [backup, change], masters)
                throw({:error, {master, r, :move_releases}})
            end

          {:error, {master, r}} ->
            remove_files(:all, [backup, change], masters)
            throw({:error, {master, r, :update_releases}})
        end

      {:error, {master, r}} ->
        remove_files(master, [backup, change], masters)
        throw({:error, {master, r, :backup_releases}})
    end
  end

  defp ensure_RELEASES_exists(masters, relFile) do
    case at_all_masters(masters, :release_handler, :do_ensure_RELEASES, [relFile]) do
      :ok ->
        :ok

      {:error, {master, r}} ->
        throw({:error, {master, r, :ensure_RELEASES_exists}})
    end
  end

  defp copy_file(file, dir, false) do
    case do_copy_file(file, dir) do
      :ok ->
        :ok

      error ->
        throw(error)
    end
  end

  defp copy_file(file, dir, masters) do
    all_masters(masters)
    copy_file_m(file, dir, masters)
  end

  defp copy_file_m(file, dir, [master | masters]) do
    case :rpc.call(master, :release_handler, :do_copy_file, [file, dir]) do
      :ok ->
        copy_file_m(file, dir, masters)

      {:error, {reason, f}} ->
        throw({:error, {master, reason, f}})

      other ->
        throw({:error, {master, other, file}})
    end
  end

  defp copy_file_m(_File, _Dir, []) do
    :ok
  end

  def do_copy_file(file, dir) do
    file2 = :filename.join(dir, :filename.basename(file))
    do_copy_file1(file, file2)
  end

  defp do_copy_file1(file, file2) do
    case :file.read_file(file) do
      {:ok, bin} ->
        case :file.write_file(file2, bin) do
          :ok ->
            :ok

          {:error, reason} ->
            {:error, {reason, file2}}
        end

      {:error, reason} ->
        {:error, {reason, file}}
    end
  end

  def do_copy_files(file, [toFile | toFiles]) do
    case do_copy_file1(file, toFile) do
      :ok ->
        do_copy_files(file, toFiles)

      error ->
        error
    end
  end

  def do_copy_files(_, []) do
    :ok
  end

  def do_copy_files([{src, dest} | files]) do
    case do_copy_file1(src, dest) do
      :ok ->
        do_copy_files(files)

      error ->
        error
    end
  end

  def do_copy_files([]) do
    :ok
  end

  def do_rename_files([{src, dest} | files]) do
    case :file.rename(src, dest) do
      :ok ->
        do_rename_files(files)

      error ->
        error
    end
  end

  def do_rename_files([]) do
    :ok
  end

  def do_remove_files([file | files]) do
    _ = :file.delete(file)
    do_remove_files(files)
  end

  def do_remove_files([]) do
    :ok
  end

  def do_ensure_RELEASES(relFile) do
    case :file.read_file_info(relFile) do
      {:ok, _} ->
        :ok

      _ ->
        do_write_file(relFile, '[]. ')
    end
  end

  defp make_dir(dir, false) do
    _ = :file.make_dir(dir)
    :ok
  end

  defp make_dir(dir, masters) do
    :lists.foreach(
      fn master ->
        :rpc.call(master, :file, :make_dir, [dir])
      end,
      masters
    )
  end

  defp all_masters(masters) do
    case :rpc.multicall(masters, :erlang, :info, [:version]) do
      {_, []} ->
        :ok

      {_, badNodes} ->
        throw({:error, {:bad_masters, badNodes}})
    end
  end

  defp at_all_masters([master | masters], m, f, a) do
    case :rpc.call(master, m, f, a) do
      :ok ->
        at_all_masters(masters, m, f, a)

      error ->
        {:error, {master, error}}
    end
  end

  defp at_all_masters([], _, _, _) do
    :ok
  end

  defp takewhile(master, masters, m, f, a) do
    _ =
      :lists.takewhile(
        fn
          ma when ma == master ->
            false

          ma ->
            :rpc.call(ma, m, f, a)
            true
        end,
        masters
      )

    :ok
  end

  defp consult(file, false) do
    :file.consult(file)
  end

  defp consult(file, masters) do
    consult_master(masters, file)
  end

  defp consult_master([master | ms], file) do
    case :rpc.call(master, :file, :consult, [file]) do
      {:badrpc, _} ->
        consult_master(ms, file)

      res ->
        res
    end
  end

  defp consult_master([], _File) do
    {:error, :no_master}
  end

  defp read_file(file, false) do
    :file.read_file(file)
  end

  defp read_file(file, masters) do
    read_master(masters, file)
  end

  defp write_file(file, data, false) do
    case :file.write_file(file, data) do
      :ok ->
        :ok

      error ->
        throw(error)
    end
  end

  defp write_file(file, data, masters) do
    case at_all_masters(masters, :file, :write_file, [file, data]) do
      :ok ->
        :ok

      error ->
        throw(error)
    end
  end

  defp ensure_dir(file, false) do
    case :filelib.ensure_dir(file) do
      :ok ->
        :ok

      error ->
        throw(error)
    end
  end

  defp ensure_dir(file, masters) do
    case at_all_masters(masters, :filelib, :ensure_dir, [file]) do
      :ok ->
        :ok

      error ->
        throw(error)
    end
  end

  defp remove_dir(dir, false) do
    remove_file(dir)
  end

  defp remove_dir(dir, masters) do
    case at_all_masters(masters, :release_handler, :remove_file, [dir]) do
      :ok ->
        :ok

      error ->
        throw(error)
    end
  end

  defp remove_files(master, files, masters) do
    takewhile(master, masters, :release_handler, :do_remove_files, [files])
  end

  defp read_master([master | ms], file) do
    case :rpc.call(master, :file, :read_file, [file]) do
      {:badrpc, _} ->
        read_master(ms, file)

      res ->
        res
    end
  end

  defp read_master([], _File) do
    {:error, :no_master}
  end

  defp write_start(file, data, false) do
    case do_write_file(file, data) do
      :ok ->
        :ok

      error ->
        throw(error)
    end
  end

  defp write_start(file, data, masters) do
    all_masters(masters)
    safe_write_file_m(file, data, masters)
  end

  defp set_static_files(srcDir, destDir, masters) do
    all_masters(masters)
    boot = 'start.boot'
    config = 'sys.config'
    srcBoot = :filename.join(srcDir, boot)
    destBoot = :filename.join(destDir, boot)
    backupBoot = :filename.join(destDir, 'start.backup')
    srcConf = :filename.join(srcDir, config)
    destConf = :filename.join(destDir, config)
    backupConf = :filename.join(destDir, 'sys.backup')

    case at_all_masters(masters, :release_handler, :do_copy_files, [
           [{destBoot, backupBoot}, {destConf, backupConf}]
         ]) do
      :ok ->
        case at_all_masters(masters, :release_handler, :do_copy_files, [
               [{srcBoot, destBoot}, {srcConf, destConf}]
             ]) do
          :ok ->
            remove_files(:all, [backupBoot, backupConf], masters)
            :ok

          {:error, {master, r}} ->
            takewhile(master, masters, :release_handler, :do_rename_files, [
              {backupBoot, destBoot},
              {backupConf, destConf}
            ])

            remove_files(:all, [backupBoot, backupConf], masters)
            throw({:error, {master, r, :copy_start_config}})
        end

      {:error, {master, r}} ->
        remove_files(master, [backupBoot, backupConf], masters)
        throw({:error, {master, r, :backup_start_config}})
    end
  end

  defp write_ini_file(rootDir, eVsn, masters) do
    binDir = :filename.join([rootDir, 'erts-' ++ eVsn, 'bin'])

    str0 =
      :io_lib.format(
        '[erlang]~nBindir=~ts~nProgname=erl~nRootdir=~ts~n',
        [:filename.nativename(binDir), :filename.nativename(rootDir)]
      )

    str = :re.replace(str0, '\\\\', '\\\\\\\\', [{:return, :list}, :global, :unicode])
    iniFile = :filename.join(binDir, 'erl.ini')
    do_write_ini_file(iniFile, str, masters)
  end

  defp do_write_ini_file(file, data, false) do
    case do_write_file(file, data, [{:encoding, :utf8}]) do
      :ok ->
        :ok

      error ->
        throw(error)
    end
  end

  defp do_write_ini_file(file, data, masters) do
    all_masters(masters)
    safe_write_file_m(file, data, [{:encoding, :utf8}], masters)
  end

  defp safe_write_file_m(file, data, masters) do
    safe_write_file_m(file, data, [], masters)
  end

  defp safe_write_file_m(file, data, fileOpts, masters) do
    backup = file ++ '.backup'
    change = file ++ '.change'

    case at_all_masters(masters, :release_handler, :do_copy_files, [file, [backup]]) do
      :ok ->
        case at_all_masters(masters, :release_handler, :do_write_file, [change, data, fileOpts]) do
          :ok ->
            case at_all_masters(masters, :file, :rename, [change, file]) do
              :ok ->
                remove_files(:all, [backup, change], masters)
                :ok

              {:error, {master, r}} ->
                takewhile(master, masters, :file, :rename, [backup, file])
                remove_files(:all, [backup, change], masters)

                throw(
                  {:error,
                   {master, r, :rename, :filename.basename(change), :filename.basename(file)}}
                )
            end

          {:error, {master, r}} ->
            remove_files(:all, [backup, change], masters)
            throw({:error, {master, r, :write, :filename.basename(change)}})
        end

      {:error, {master, r}} ->
        remove_files(master, [backup], masters)

        throw(
          {:error, {master, r, :backup, :filename.basename(file), :filename.basename(backup)}}
        )
    end
  end

  defp get_new_libs([{app, vsn, _LibDir} | currentLibs], newLibs) do
    case :lists.keyfind(app, 1, newLibs) do
      {^app, newVsn, _} = libInfo when newVsn !== vsn ->
        [libInfo | get_new_libs(currentLibs, newLibs)]

      _ ->
        get_new_libs(currentLibs, newLibs)
    end
  end

  defp get_new_libs([], _) do
    []
  end

  defp get_releases_with_status([], _, acc) do
    acc
  end

  defp get_releases_with_status([{_, _, _, releaseStatus} = head | tail], status, acc)
       when releaseStatus == status do
    get_releases_with_status(tail, status, [head | acc])
  end

  defp get_releases_with_status([_ | tail], status, acc) do
    get_releases_with_status(tail, status, acc)
  end
end
