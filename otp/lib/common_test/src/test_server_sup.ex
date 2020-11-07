defmodule :m_test_server_sup do
  use Bitwise
  require Record

  Record.defrecord(:r_target_info, :target_info,
    os_family: :undefined,
    os_type: :undefined,
    host: :undefined,
    version: :undefined,
    system_version: :undefined,
    root_dir: :undefined,
    emulator: :undefined,
    otp_release: :undefined,
    username: :undefined,
    cookie: :undefined,
    naming: :undefined,
    master: :undefined
  )

  Record.defrecord(:r_par, :par,
    type: :undefined,
    target: :undefined,
    naming: :undefined,
    master: :undefined,
    cookie: :undefined
  )

  Record.defrecord(:r_cover, :cover,
    app: :undefined,
    file: :undefined,
    incl: :undefined,
    excl: :undefined,
    level: :undefined,
    mods: :undefined,
    stop: true,
    cross: :undefined
  )

  Record.defrecord(:r_util_state, :util_state,
    starter: :undefined,
    latest_name: :undefined
  )

  def timetrap(timeout0, pid) do
    timetrap(timeout0, timeout0, true, pid)
  end

  def timetrap(timeout0, scale, pid) do
    timetrap(timeout0, timeout0, scale, pid)
  end

  def timetrap(timeout0, reportTVal, scale, pid) do
    :erlang.process_flag(:priority, :max)
    :ct_util.mark_process()

    timeout =
      cond do
        not scale ->
          timeout0

        true ->
          :test_server.timetrap_scale_factor() * timeout0
      end

    truncTO = trunc(timeout)

    receive do
    after
      truncTO ->
        kill_the_process(pid, timeout0, truncTO, reportTVal)
    end
  end

  defp kill_the_process(pid, timeout0, truncTO, reportTVal) do
    case :erlang.is_process_alive(pid) do
      true ->
        timeToReport =
          cond do
            timeout0 == reportTVal ->
              truncTO

            true ->
              reportTVal
          end

        mFLs = :test_server.get_loc(pid)
        mon = :erlang.monitor(:process, pid)
        trap = {:timetrap_timeout, timeToReport, mFLs}
        :erlang.exit(pid, trap)

        receive do
          {:DOWN, ^mon, :process, ^pid, _} ->
            :ok
        after
          10000 ->
            try do
              :error_logger.warning_msg(
                'Testcase process ~w not responding to timetrap timeout:~n  ~tp.~nKilling testcase...~n',
                [pid, trap]
              )
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end

            :erlang.exit(pid, :kill)
        end

      false ->
        :ok
    end
  end

  def timetrap_cancel(handle) do
    :erlang.unlink(handle)
    monRef = :erlang.monitor(:process, handle)
    :erlang.exit(handle, :kill)

    receive do
      {:DOWN, ^monRef, _, _, _} ->
        :ok
    after
      2000 ->
        :erlang.demonitor(monRef, [:flush])
        :ok
    end
  end

  def capture_get(msgs) do
    receive do
      {:captured, msg} ->
        capture_get([msg | msgs])
    after
      0 ->
        :lists.reverse(msgs)
    end
  end

  def messages_get(msgs) do
    receive do
      msg ->
        messages_get([msg | msgs])
    after
      0 ->
        :lists.reverse(msgs)
    end
  end

  def timecall(m, f, a) do
    {elapsed, val} = :timer.tc(m, f, a)
    {elapsed / 1_000_000, val}
  end

  def call_crash(time, crash, m, f, a) do
    oldTrapExit = :erlang.process_flag(:trap_exit, true)
    pid = spawn_link(m, f, a)

    answer =
      receive do
        {:EXIT, ^crash} ->
          :ok

        {:EXIT, ^pid, ^crash} ->
          :ok

        {:EXIT, _Reason} when crash == :any ->
          :ok

        {:EXIT, ^pid, _Reason} when crash == :any ->
          :ok

        {:EXIT, reason} ->
          :test_server.format(12, 'Wrong crash reason. Wanted ~tp, got ~tp.', [crash, reason])
          exit({:wrong_crash_reason, reason})

        {:EXIT, ^pid, reason} ->
          :test_server.format(12, 'Wrong crash reason. Wanted ~tp, got ~tp.', [crash, reason])
          exit({:wrong_crash_reason, reason})

        {:EXIT, otherPid, reason} when oldTrapExit == false ->
          exit({:EXIT, otherPid, reason})
      after
        do_trunc(time) ->
          exit(:call_crash_timeout)
      end

    :erlang.process_flag(:trap_exit, oldTrapExit)
    answer
  end

  defp do_trunc(:infinity) do
    :infinity
  end

  defp do_trunc(t) do
    trunc(t)
  end

  def app_test(application, mode) do
    case is_app(application) do
      {:ok, appFile} ->
        do_app_tests(appFile, application, mode)

      error ->
        :test_server.fail(error)
    end
  end

  defp is_app(application) do
    case :file.consult(
           :filename.join([
             :code.lib_dir(application),
             'ebin',
             :erlang.atom_to_list(application) ++ '.app'
           ])
         ) do
      {:ok, [{:application, ^application, appFile}]} ->
        {:ok, appFile}

      _ ->
        :test_server.format(
          :minor,
          'Application (.app) file not found, or it has very bad syntax.~n'
        )

        {:error, :not_an_application}
    end
  end

  defp do_app_tests(appFile, appName, mode) do
    dictList = [
      {:missing_fields, []},
      {:missing_mods, []},
      {:superfluous_mods_in_ebin, []},
      {:export_all_mods, []},
      {:missing_apps, []}
    ]

    fill_dictionary(dictList)

    check_fields(
      [:description, :modules, :registered, :applications],
      appFile
    )

    {:value, {:modules, mods}} = :lists.keysearch(:modules, 1, appFile)
    eBinList = :lists.sort(get_ebin_modnames(appName))
    {missing, extra} = common(:lists.sort(mods), eBinList)
    :erlang.put(:superfluous_mods_in_ebin, extra)
    :erlang.put(:missing_mods, missing)
    app_check_export_all(mods)
    {:value, {:applications, apps}} = :lists.keysearch(:applications, 1, appFile)
    check_apps(apps)
    a = check_dict(:missing_fields, 'Inconsistent app file, missing fields')
    b = check_dict(:missing_mods, 'Inconsistent app file, missing modules')

    c =
      check_dict_tolerant(
        :superfluous_mods_in_ebin,
        'Inconsistent app file, Modules not included in app file.',
        mode
      )

    d = check_dict(:export_all_mods, 'Inconsistent app file, Modules have `export_all\'.')
    e = check_dict(:missing_apps, 'Inconsistent app file, missing applications.')
    erase_dictionary(dictList)

    case a + b + c + d + e do
      5 ->
        :ok

      _ ->
        :test_server.fail()
    end
  end

  defp app_check_export_all([]) do
    :ok
  end

  defp app_check_export_all([mod | mods]) do
    case (try do
            apply(mod, :module_info, [:compile])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, {:undef, _}} ->
        app_check_export_all(mods)

      cOpts ->
        case :lists.keysearch(:options, 1, cOpts) do
          false ->
            app_check_export_all(mods)

          {:value, {:options, list}} ->
            case :lists.member(:export_all, list) do
              true ->
                :erlang.put(
                  :export_all_mods,
                  [mod | :erlang.get(:export_all_mods)]
                )

                app_check_export_all(mods)

              false ->
                app_check_export_all(mods)
            end
        end
    end
  end

  def appup_test(application) do
    case is_app(application) do
      {:ok, appFile} ->
        case is_appup(
               application,
               :proplists.get_value(:vsn, appFile)
             ) do
          {:ok, up, down} ->
            startMod = :proplists.get_value(:mod, appFile)
            modules = :proplists.get_value(:modules, appFile)
            do_appup_tests(startMod, application, up, down, modules)

          error ->
            :test_server.fail(error)
        end

      error ->
        :test_server.fail(error)
    end
  end

  defp is_appup(application, version) do
    appupFile = :erlang.atom_to_list(application) ++ '.appup'
    appupPath = :filename.join([:code.lib_dir(application), 'ebin', appupFile])

    case :file.consult(appupPath) do
      {:ok, [{^version, up, down}]}
      when is_list(up) and
             is_list(down) ->
        {:ok, up, down}

      _ ->
        :test_server.format(
          :minor,
          'Application upgrade (.appup) file not found, or it has very bad syntax.~n'
        )

        {:error, :appup_not_readable}
    end
  end

  defp do_appup_tests(:undefined, application, up, down, _Modules) do
    case up do
      [{".*", [{:restart_application, ^application}]}] ->
        case down do
          [{".*", [{:restart_application, ^application}]}] ->
            :ok

          _ ->
            :test_server.format(
              :minor,
              'Library application needs restart_application downgrade instruction.~n'
            )

            {:error, :library_downgrade_instruction_malformed}
        end

      _ ->
        :test_server.format(
          :minor,
          'Library application needs restart_application upgrade instruction.~n'
        )

        {:error, :library_upgrade_instruction_malformed}
    end
  end

  defp do_appup_tests(_, _Application, up, down, modules) do
    case check_appup_clauses_plausible(up, :up, modules) do
      :ok ->
        case check_appup_clauses_plausible(down, :down, modules) do
          :ok ->
            :test_server.format(:minor, 'OK~n')

          error ->
            :test_server.format(:minor, 'ERROR ~tp~n', [error])
            :test_server.fail(error)
        end

      error ->
        :test_server.format(:minor, 'ERROR ~tp~n', [error])
        :test_server.fail(error)
    end
  end

  defp check_appup_clauses_plausible([], _Direction, _Modules) do
    :ok
  end

  defp check_appup_clauses_plausible([{re, instrs} | rest], direction, modules)
       when is_binary(re) do
    case :re.compile(re, [:unicode]) do
      {:ok, _} ->
        case check_appup_instructions(instrs, direction, modules) do
          :ok ->
            check_appup_clauses_plausible(rest, direction, modules)

          error ->
            error
        end

      {:error, error} ->
        {:error, {:version_regex_malformed, re, error}}
    end
  end

  defp check_appup_clauses_plausible([{v, instrs} | rest], direction, modules)
       when is_list(v) do
    case check_appup_instructions(instrs, direction, modules) do
      :ok ->
        check_appup_clauses_plausible(rest, direction, modules)

      error ->
        error
    end
  end

  defp check_appup_clauses_plausible(clause, _Direction, _Modules) do
    {:error, {:clause_malformed, clause}}
  end

  defp check_appup_instructions(instrs, direction, modules) do
    case check_instructions(direction, instrs, instrs, [], [], modules) do
      {_Good, []} ->
        :ok

      {_, bad} ->
        {:error, {:bad_instructions, bad}}
    end
  end

  defp check_instructions(_, [], _, good, bad, _) do
    {:lists.reverse(good), :lists.reverse(bad)}
  end

  defp check_instructions(upDown, [instr | rest], all, good, bad, modules) do
    case (try do
            check_instruction(upDown, instr, all, modules)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :ok ->
        check_instructions(upDown, rest, all, [instr | good], bad, modules)

      {:error, reason} ->
        newBad = [{instr, reason} | bad]
        check_instructions(upDown, rest, all, good, newBad, modules)
    end
  end

  defp check_instruction(:up, {:add_module, module}, _, modules) do
    check_module(module, modules)
  end

  defp check_instruction(:down, {:add_module, module}, _, modules) do
    case (try do
            check_module(module, modules)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, {:unknown_module, ^module, ^modules}} ->
        :ok

      :ok ->
        throw({:error, {:existing_readded_module, module}})
    end
  end

  defp check_instruction(_, {:load_module, module}, _, modules) do
    check_module(module, modules)
  end

  defp check_instruction(_, {:load_module, module, depMods}, _, modules) do
    check_module(module, modules)
    check_depend(depMods)
  end

  defp check_instruction(_, {:load_module, module, pre, post, depMods}, _, modules) do
    check_module(module, modules)
    check_depend(depMods)
    check_purge(pre)
    check_purge(post)
  end

  defp check_instruction(:up, {:delete_module, module}, _, modules) do
    case (try do
            check_module(module, modules)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, {:unknown_module, ^module, ^modules}} ->
        :ok

      :ok ->
        throw({:error, {:existing_module_deleted, module}})
    end
  end

  defp check_instruction(:down, {:delete_module, module}, _, modules) do
    check_module(module, modules)
  end

  defp check_instruction(_, {:update, module}, _, modules) do
    check_module(module, modules)
  end

  defp check_instruction(_, {:update, module, :supervisor}, _, modules) do
    check_module(module, modules)
  end

  defp check_instruction(_, {:update, module, depMods}, _, modules)
       when is_list(depMods) do
    check_module(module, modules)
  end

  defp check_instruction(_, {:update, module, change}, _, modules) do
    check_module(module, modules)
    check_change(change)
  end

  defp check_instruction(_, {:update, module, change, depMods}, _, modules) do
    check_module(module, modules)
    check_change(change)
    check_depend(depMods)
  end

  defp check_instruction(_, {:update, module, change, pre, post, depMods}, _, modules) do
    check_module(module, modules)
    check_change(change)
    check_purge(pre)
    check_purge(post)
    check_depend(depMods)
  end

  defp check_instruction(_, {:update, module, timeout, change, pre, post, depMods}, _, modules) do
    check_module(module, modules)
    check_timeout(timeout)
    check_change(change)
    check_purge(pre)
    check_purge(post)
    check_depend(depMods)
  end

  defp check_instruction(
         _,
         {:update, module, modType, timeout, change, pre, post, depMods},
         _,
         modules
       ) do
    check_module(module, modules)
    check_mod_type(modType)
    check_timeout(timeout)
    check_change(change)
    check_purge(pre)
    check_purge(post)
    check_depend(depMods)
  end

  defp check_instruction(_, {:restart_application, application}, _, _) do
    check_application(application)
  end

  defp check_instruction(_, {:remove_application, application}, _, _) do
    check_application(application)
  end

  defp check_instruction(_, {:add_application, application}, _, _) do
    check_application(application)
  end

  defp check_instruction(_, {:add_application, application, type}, _, _) do
    check_application(application)
    check_restart_type(type)
  end

  defp check_instruction(_, instr, _, _) do
    throw({:error, {:low_level_or_invalid_instruction, instr}})
  end

  defp check_module(module, modules) do
    case {is_atom(module), :lists.member(module, modules)} do
      {true, true} ->
        :ok

      {true, false} ->
        throw({:error, {:unknown_module, module}})

      {false, _} ->
        throw({:error, {:bad_module, module}})
    end
  end

  defp check_application(app) do
    case is_atom(app) do
      true ->
        :ok

      false ->
        throw({:error, {:bad_application, app}})
    end
  end

  defp check_depend(dep) when is_list(dep) do
    :ok
  end

  defp check_depend(dep) do
    throw({:error, {:bad_depend, dep}})
  end

  defp check_restart_type(:permanent) do
    :ok
  end

  defp check_restart_type(:transient) do
    :ok
  end

  defp check_restart_type(:temporary) do
    :ok
  end

  defp check_restart_type(:load) do
    :ok
  end

  defp check_restart_type(:none) do
    :ok
  end

  defp check_restart_type(type) do
    throw({:error, {:bad_restart_type, type}})
  end

  defp check_timeout(t) when is_integer(t) and t > 0 do
    :ok
  end

  defp check_timeout(:default) do
    :ok
  end

  defp check_timeout(:infinity) do
    :ok
  end

  defp check_timeout(t) do
    throw({:error, {:bad_timeout, t}})
  end

  defp check_mod_type(:static) do
    :ok
  end

  defp check_mod_type(:dynamic) do
    :ok
  end

  defp check_mod_type(type) do
    throw({:error, {:bad_mod_type, type}})
  end

  defp check_purge(:soft_purge) do
    :ok
  end

  defp check_purge(:brutal_purge) do
    :ok
  end

  defp check_purge(purge) do
    throw({:error, {:bad_purge, purge}})
  end

  defp check_change(:soft) do
    :ok
  end

  defp check_change({:advanced, _}) do
    :ok
  end

  defp check_change(change) do
    throw({:error, {:bad_change, change}})
  end

  defp common(l1, l2) do
    common(l1, l2, [], [])
  end

  defp common([x | rest1], [x | rest2], a1, a2) do
    common(rest1, rest2, a1, a2)
  end

  defp common([x | rest1], [y | rest2], a1, a2) when x < y do
    common(rest1, [y | rest2], [x | a1], a2)
  end

  defp common([x | rest1], [y | rest2], a1, a2) do
    common([x | rest1], rest2, a1, [y | a2])
  end

  defp common([], l, a1, a2) do
    {a1, l ++ a2}
  end

  defp common(l, [], a1, a2) do
    {l ++ a1, a2}
  end

  defp check_apps([]) do
    :ok
  end

  defp check_apps([app | apps]) do
    case is_app(app) do
      {:ok, _AppFile} ->
        :ok

      {:error, _} ->
        :erlang.put(
          :missing_apps,
          [app | :erlang.get(:missing_apps)]
        )
    end

    check_apps(apps)
  end

  defp check_fields([], _AppFile) do
    :ok
  end

  defp check_fields([l | ls], appFile) do
    check_field(l, appFile)
    check_fields(ls, appFile)
  end

  defp check_field(fieldName, appFile) do
    case :lists.keymember(fieldName, 1, appFile) do
      true ->
        :ok

      false ->
        :erlang.put(
          :missing_fields,
          [fieldName | :erlang.get(:missing_fields)]
        )

        :ok
    end
  end

  defp check_dict(dict, reason) do
    case :erlang.get(dict) do
      [] ->
        1

      list ->
        :io.format('** ~ts (~ts) ->~n~tp~n', [reason, dict, list])
        0
    end
  end

  defp check_dict_tolerant(dict, reason, mode) do
    case :erlang.get(dict) do
      [] ->
        1

      list ->
        :io.format('** ~ts (~ts) ->~n~tp~n', [reason, dict, list])

        case mode do
          :pedantic ->
            0

          _ ->
            1
        end
    end
  end

  defp get_ebin_modnames(appName) do
    wc = :filename.join([:code.lib_dir(appName), 'ebin', '*' ++ :code.objfile_extension()])

    theFun = fn x, acc ->
      [
        :erlang.list_to_atom(:filename.rootname(:filename.basename(x)))
        | acc
      ]
    end

    _Files = :lists.foldl(theFun, [], :filelib.wildcard(wc))
  end

  def cleanup_crash_dumps() do
    dir = crash_dump_dir()
    dumps = :filelib.wildcard(:filename.join(dir, 'erl_crash_dump*'))
    delete_files(dumps)
  end

  def crash_dump_dir() do
    {:ok, dir} = :test_server_sup.framework_call(:get_log_dir, [], :file.get_cwd())
    dir
  end

  def tar_crash_dumps() do
    dir = crash_dump_dir()

    case :filelib.wildcard(:filename.join(dir, 'erl_crash_dump*')) do
      [] ->
        {:error, :no_crash_dumps}

      dumps ->
        tarFileName = :filename.join(dir, 'crash_dumps.tar.gz')

        {:ok, tar} =
          :erl_tar.open(
            tarFileName,
            [:write, :compressed]
          )

        :lists.foreach(
          fn file ->
            :ok = :erl_tar.add(tar, file, :filename.basename(file), [])
          end,
          dumps
        )

        :ok = :erl_tar.close(tar)
        delete_files(dumps)
        {:ok, tarFileName}
    end
  end

  def check_new_crash_dumps() do
    dir = crash_dump_dir()
    dumps = :filelib.wildcard(:filename.join(dir, 'erl_crash_dump*'))

    case length(dumps) do
      0 ->
        :ok

      num ->
        :test_server_ctrl.format(:minor, 'Found ~w crash dumps:~n', [num])
        append_files_to_logfile(dumps)
        delete_files(dumps)
    end
  end

  defp append_files_to_logfile([]) do
    :ok
  end

  defp append_files_to_logfile([file | files]) do
    nodeName = from(?., file)
    :test_server_ctrl.format(:minor, 'Crash dump from node ~tp:~n', [nodeName])
    fd = :erlang.get(:test_server_minor_fd)

    case :file.read_file(file) do
      {:ok, bin} ->
        case :file.write(fd, bin) do
          :ok ->
            :ok

          {:error, error} ->
            :io.format(fd, 'Unable to write the crash dump to this file: ~tp~n', [
              :file.format_error(error)
            ])
        end

      _Error ->
        :io.format(fd, 'Failed to read: ~ts\n', [file])
    end

    append_files_to_logfile(files)
  end

  defp delete_files([]) do
    :ok
  end

  defp delete_files([file | files]) do
    :io.format('Deleting file: ~ts~n', [file])

    case :file.delete(file) do
      {:error, _} ->
        case :file.rename(file, file ++ '.old') do
          {:error, error} ->
            :io.format('Could neither delete nor rename file ~ts: ~ts.~n', [file, error])

          _ ->
            :ok
        end

      _ ->
        :ok
    end

    delete_files(files)
  end

  defp erase_dictionary([{var, _Val} | vars]) do
    :erlang.erase(var)
    erase_dictionary(vars)
  end

  defp erase_dictionary([]) do
    :ok
  end

  defp fill_dictionary([{var, val} | vars]) do
    :erlang.put(var, val)
    fill_dictionary(vars)
  end

  defp fill_dictionary([]) do
    []
  end

  def get_username() do
    getenv_any(['USER', 'USERNAME'])
  end

  defp getenv_any([key | rest]) do
    case (try do
            :os.getenv(key)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      string when is_list(string) ->
        string

      false ->
        getenv_any(rest)
    end
  end

  defp getenv_any([]) do
    ''
  end

  def get_os_family() do
    {osFamily, _OsName} = :os.type()
    osFamily
  end

  def hostatom() do
    hostatom(node())
  end

  def hostatom(node) do
    :erlang.list_to_atom(hoststr(node))
  end

  def hoststr() do
    hoststr(node())
  end

  def hoststr(node) when is_atom(node) do
    hoststr(:erlang.atom_to_list(node))
  end

  def hoststr(node) when is_list(node) do
    from(?@, node)
  end

  defp from(h, [h | t]) do
    t
  end

  defp from(h, [_ | t]) do
    from(h, t)
  end

  defp from(_H, []) do
    []
  end

  def framework_call(func, args) do
    framework_call(func, args, :ok)
  end

  def framework_call(func, args, defaultReturn) do
    cB = :os.getenv('TEST_SERVER_FRAMEWORK')
    framework_call(cB, func, args, defaultReturn)
  end

  def framework_call(fW, _Func, _Args, defaultReturn)
      when fW === false or fW === 'undefined' do
    defaultReturn
  end

  def framework_call(callback, func, args, defaultReturn) do
    mod = :erlang.list_to_atom(callback)

    _ =
      case :code.is_loaded(mod) do
        false ->
          :code.load_file(mod)

        _ ->
          :ok
      end

    case :erlang.function_exported(mod, func, length(args)) do
      true ->
        eH = fn reason ->
          exit({:fw_error, {mod, func, reason}})
        end

        setTcState =
          case func do
            :end_tc ->
              true

            :init_tc ->
              true

            _ ->
              false
          end

        case setTcState do
          true ->
            :test_server.set_tc_state({:framework, {mod, func, args}})

          false ->
            :ok
        end

        :ct_util.mark_process()

        try do
          apply(mod, func, args)
        catch
          :exit, why ->
            eH.(why)

          :error, why ->
            eH.({why, __STACKTRACE__})

          why ->
            eH.(why)
        else
          result ->
            result
        end

      false ->
        defaultReturn
    end
  end

  def format_loc([{mod, func, line}]) do
    [format_loc1({mod, func, line})]
  end

  def format_loc([{mod, func, line} | rest]) do
    [
      ['[', format_loc1({mod, func, line}), ',\n']
      | format_loc1(rest)
    ]
  end

  def format_loc([{mod, lineOrFunc}]) do
    format_loc({mod, lineOrFunc})
  end

  def format_loc({mod, func}) when is_atom(func) do
    :io_lib.format('{~w,~tw}', [mod, func])
  end

  def format_loc(loc) do
    :io_lib.format('~tp', [loc])
  end

  defp format_loc1([{mod, func, line}]) do
    ['              ', format_loc1({mod, func, line}), ']']
  end

  defp format_loc1([{mod, func, line} | rest]) do
    [
      ['              ', format_loc1({mod, func, line}), ',\n']
      | format_loc1(rest)
    ]
  end

  defp format_loc1({mod, func, line}) do
    modStr = :erlang.atom_to_list(mod)

    case {:lists.member(
            :no_src,
            :erlang.get(:test_server_logopts)
          ), :lists.reverse(modStr)} do
      {false, [[?E, ?T, ?I, ?U, ?S, ?_] | _]} ->
        link =
          cond do
            is_integer(line) ->
              :erlang.integer_to_list(line)

            line == :last_expr ->
              :erlang.list_to_atom(:erlang.atom_to_list(func) ++ '-last_expr')

            is_atom(line) ->
              :erlang.atom_to_list(line)

            true ->
              line
          end

        :io_lib.format(
          '{~w,~tw,<a href="~ts~ts#~ts">~tw</a>}',
          [mod, func, :test_server_ctrl.uri_encode(downcase(modStr)), '.src.html', link, line]
        )

      _ ->
        :io_lib.format('{~w,~tw,~tw}', [mod, func, line])
    end
  end

  defp downcase(s) do
    downcase(s, [])
  end

  defp downcase([uc | rest], result)
       when ?A <= uc and
              uc <= ?Z do
    downcase(rest, [uc - ?A + ?a | result])
  end

  defp downcase([c | rest], result) do
    downcase(rest, [c | result])
  end

  defp downcase([], result) do
    :lists.reverse(result)
  end

  def util_start() do
    starter = self()

    case :erlang.whereis(:test_server_sup) do
      :undefined ->
        spawn_link(fn ->
          :erlang.register(:test_server_sup, self())
          :erlang.put(:app, :common_test)
          util_loop(r_util_state(starter: starter))
        end)

        :ok

      _Pid ->
        :ok
    end
  end

  def util_stop() do
    try do
      send(:test_server_sup, {self(), :stop})
    catch
      _, _ ->
        :ok
    else
      _ ->
        receive do
          {:test_server_sup, :stopped} ->
            :ok
        after
          5000 ->
            :erlang.exit(:erlang.whereis(:test_server_sup), :kill)
        end
    end
  end

  def unique_name() do
    send(:test_server_sup, {self(), :unique_name})

    receive do
      {:test_server_sup, name} ->
        name
    after
      5000 ->
        exit({:test_server_sup, :no_util_process})
    end
  end

  defp util_loop(state) do
    receive do
      {from, :unique_name} ->
        nr = :erlang.unique_integer([:positive])
        name = :erlang.integer_to_list(nr)

        cond do
          name == r_util_state(state, :latest_name) ->
            :timer.sleep(1)
            send(self(), {from, :unique_name})
            util_loop(state)

          true ->
            send(from, {:test_server_sup, name})
            util_loop(r_util_state(state, latest_name: name))
        end

      {from, :stop} ->
        try do
          :erlang.unlink(r_util_state(state, :starter))
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        send(from, {:test_server_sup, :stopped})
        :ok
    end
  end

  def call_trace(traceSpec) do
    case (try do
            try_call_trace(traceSpec)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        :erlang.display(reason)
        exit(reason)

      ok ->
        ok
    end
  end

  defp try_call_trace(traceSpec) do
    case :file.consult(traceSpec) do
      {:ok, terms} ->
        :dbg.tracer()
        :dbg.p(self(), [:sos, :call])

        :lists.foreach(
          fn
            {:m, m} ->
              case :dbg.tpl(m, [{:_, [], [{:return_trace}]}]) do
                {:error, what} ->
                  exit({:error, {:tracing_failed, what}})

                _ ->
                  :ok
              end

            {:f, m, f} ->
              case :dbg.tpl(m, f, [{:_, [], [{:return_trace}]}]) do
                {:error, what} ->
                  exit({:error, {:tracing_failed, what}})

                _ ->
                  :ok
              end

            huh ->
              exit({:error, {:unrecognized_trace_term, huh}})
          end,
          terms
        )

        :ok

      {_, error} ->
        exit({:error, {:tracing_failed, traceSpec, error}})
    end
  end
end
