defmodule :m_release_handler_1 do
  use Bitwise
  require Record

  Record.defrecord(:r_eval_state, :eval_state,
    bins: [],
    stopped: [],
    suspended: [],
    apps: [],
    libdirs: :undefined,
    unpurged: [],
    vsns: [],
    newlibs: [],
    opts: []
  )

  def check_script([:restart_new_emulator | script], libDirs) do
    do_check_script(script, libDirs, [])
  end

  def check_script(script, libDirs) do
    case (try do
            check_old_processes(script, :soft_purge)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, purgeMods} ->
        do_check_script(script, libDirs, purgeMods)

      {:error, mod} ->
        {:error, {:old_processes, mod}}
    end
  end

  defp do_check_script(script, libDirs, purgeMods) do
    {before, after__} = split_instructions(script)

    case (try do
            :lists.foldl(
              fn instruction, evalState1 ->
                eval(instruction, evalState1)
              end,
              r_eval_state(libdirs: libDirs),
              before
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      evalState2 when elem(evalState2, 0) === :eval_state ->
        case (try do
                syntax_check_script(after__)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          :ok ->
            {:ok, purgeMods}

          other ->
            {:error, other}
        end

      {:error, error} ->
        {:error, error}

      other ->
        {:error, other}
    end
  end

  def eval_script(script) do
    eval_script(script, [], [], [], [])
  end

  def eval_script(script, apps, libDirs, newLibs, opts) do
    case (try do
            check_old_processes(script, :soft_purge)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, _} ->
        {before, after__} = split_instructions(script)

        case (try do
                :lists.foldl(
                  fn instruction, evalState1 ->
                    eval(instruction, evalState1)
                  end,
                  r_eval_state(apps: apps, libdirs: libDirs, newlibs: newLibs, opts: opts),
                  before
                )
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          evalState2 when elem(evalState2, 0) === :eval_state ->
            case (try do
                    syntax_check_script(after__)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              :ok ->
                case (try do
                        :lists.foldl(
                          fn instruction, evalState3 ->
                            eval(instruction, evalState3)
                          end,
                          evalState2,
                          after__
                        )
                      catch
                        :error, e -> {:EXIT, {e, __STACKTRACE__}}
                        :exit, e -> {:EXIT, e}
                        e -> e
                      end) do
                  evalState4 when elem(evalState4, 0) === :eval_state ->
                    {:ok, r_eval_state(evalState4, :unpurged)}

                  :restart_emulator ->
                    :restart_emulator

                  error ->
                    {:EXIT, error}
                end

              other ->
                {:error, other}
            end

          {:error, error} ->
            {:error, error}

          other ->
            {:error, other}
        end

      {:error, mod} ->
        {:error, {:old_processes, mod}}
    end
  end

  defp split_instructions(script) do
    split_instructions(script, [])
  end

  defp split_instructions([:point_of_no_return | t], before) do
    {:lists.reverse(before), [:point_of_no_return | t]}
  end

  defp split_instructions([h | t], before) do
    split_instructions(t, [h | before])
  end

  defp split_instructions([], before) do
    {[], :lists.reverse(before)}
  end

  def check_old_processes(script, prePurgeMethod) do
    procs = :erlang.processes()

    {:ok,
     :lists.flatmap(
       fn
         {:load, {mod, pPM, _PostPurgeMethod}}
         when pPM == prePurgeMethod ->
           check_old_code(mod, procs, prePurgeMethod)

         {:remove, {mod, pPM, _PostPurgeMethod}}
         when pPM == prePurgeMethod ->
           check_old_code(mod, procs, prePurgeMethod)

         _ ->
           []
       end,
       script
     )}
  end

  defp check_old_code(mod, procs, prePurgeMethod) do
    case :erlang.check_old_code(mod) do
      true when prePurgeMethod == :soft_purge ->
        do_check_old_code(mod, procs)

      true when prePurgeMethod == :brutal_purge ->
        case (try do
                do_check_old_code(mod, procs)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:error, ^mod} ->
            []

          r ->
            r
        end

      false ->
        []
    end
  end

  defp do_check_old_code(mod, procs) do
    :lists.foreach(
      fn pid ->
        case :erlang.check_process_code(pid, mod) do
          false ->
            :ok

          true ->
            throw({:error, mod})
        end
      end,
      procs
    )

    [mod]
  end

  defp syntax_check_script([:point_of_no_return | script]) do
    syntax_check_script(script)
  end

  defp syntax_check_script([{:load, {_, _, _}} | script]) do
    syntax_check_script(script)
  end

  defp syntax_check_script([{:remove, {_, _, _}} | script]) do
    syntax_check_script(script)
  end

  defp syntax_check_script([{:purge, _} | script]) do
    syntax_check_script(script)
  end

  defp syntax_check_script([{:suspend, _} | script]) do
    syntax_check_script(script)
  end

  defp syntax_check_script([{:resume, _} | script]) do
    syntax_check_script(script)
  end

  defp syntax_check_script([{:code_change, _} | script]) do
    syntax_check_script(script)
  end

  defp syntax_check_script([{:code_change, _, _} | script]) do
    syntax_check_script(script)
  end

  defp syntax_check_script([{:stop, _} | script]) do
    syntax_check_script(script)
  end

  defp syntax_check_script([{:start, _} | script]) do
    syntax_check_script(script)
  end

  defp syntax_check_script([{:sync_nodes, _, {_, _, _}} | script]) do
    syntax_check_script(script)
  end

  defp syntax_check_script([{:sync_nodes, _, _} | script]) do
    syntax_check_script(script)
  end

  defp syntax_check_script([{:apply, {_, _, _}} | script]) do
    syntax_check_script(script)
  end

  defp syntax_check_script([:restart_emulator | script]) do
    syntax_check_script(script)
  end

  defp syntax_check_script([illegal | _Script]) do
    throw({:illegal_instruction_after_point_of_no_return, illegal})
  end

  defp syntax_check_script([]) do
    :ok
  end

  defp eval(
         {:load_object_code, {lib, libVsn, modules}},
         evalState
       ) do
    case :lists.keysearch(lib, 1, r_eval_state(evalState, :libdirs)) do
      {:value, {^lib, ^libVsn, libDir} = libInfo} ->
        ext = :code.objfile_extension()

        {newBins, newVsns} =
          :lists.foldl(
            fn mod, {bins, vsns} ->
              file = :lists.concat([mod, ext])
              fName = :filename.join([libDir, 'ebin', file])

              case :erl_prim_loader.get_file(fName) do
                {:ok, bin, fName2} ->
                  nVsns = add_vsns(mod, bin, vsns)
                  {[{mod, bin, fName2} | bins], nVsns}

                :error ->
                  throw({:error, {:no_such_file, fName}})
              end
            end,
            {r_eval_state(evalState, :bins), r_eval_state(evalState, :vsns)},
            modules
          )

        newLibs = :lists.keystore(lib, 1, r_eval_state(evalState, :newlibs), libInfo)
        r_eval_state(evalState, bins: newBins, newlibs: newLibs, vsns: newVsns)

      {:value, {^lib, libVsn2, _LibDir}} ->
        throw({:error, {:bad_lib_vsn, lib, libVsn2}})
    end
  end

  defp eval(:point_of_no_return, evalState) do
    libs =
      case get_opt(:update_paths, evalState, false) do
        false ->
          r_eval_state(evalState, :newlibs)

        true ->
          r_eval_state(evalState, :libdirs)
      end

    :lists.foreach(
      fn {lib, _LibVsn, libDir} ->
        ebin = :filename.join(libDir, 'ebin')
        :code.replace_path(lib, ebin)
      end,
      libs
    )

    evalState
  end

  defp eval(
         {:load, {mod, _PrePurgeMethod, postPurgeMethod}},
         evalState
       ) do
    bins = r_eval_state(evalState, :bins)
    {:value, {_Mod, bin, file}} = :lists.keysearch(mod, 1, bins)
    {:module, _} = :code.load_binary(mod, file, bin)
    unpurged = do_soft_purge(mod, postPurgeMethod, r_eval_state(evalState, :unpurged))

    r_eval_state(evalState,
      bins: :lists.keydelete(mod, 1, bins),
      unpurged: unpurged
    )
  end

  defp eval(
         {:remove, {mod, _PrePurgeMethod, postPurgeMethod}},
         evalState
       ) do
    :code.purge(mod)
    :code.delete(mod)
    unpurged = do_soft_purge(mod, postPurgeMethod, r_eval_state(evalState, :unpurged))
    r_eval_state(evalState, unpurged: unpurged)
  end

  defp eval({:purge, modules}, evalState) do
    :lists.foreach(
      fn mod ->
        :code.purge(mod)
      end,
      modules
    )

    evalState
  end

  defp eval({:suspend, modules}, evalState) do
    procs = get_supervised_procs()

    newSuspended =
      :lists.foldl(
        fn modSpec, suspended ->
          {module, def__} =
            case modSpec do
              {mod, modTimeout} ->
                {mod, modTimeout}

              mod ->
                {mod, :default}
            end

          timeout = get_opt(:suspend_timeout, evalState, def__)
          pids = suspend(module, procs, timeout)
          [{module, pids} | suspended]
        end,
        r_eval_state(evalState, :suspended),
        modules
      )

    r_eval_state(evalState, suspended: newSuspended)
  end

  defp eval({:resume, modules}, evalState) do
    newSuspended =
      :lists.foldl(
        fn mod, suspended ->
          :lists.filter(
            fn
              {mod2, pids}
              when mod2 == mod ->
                resume(pids)
                false

              _ ->
                true
            end,
            suspended
          )
        end,
        r_eval_state(evalState, :suspended),
        modules
      )

    r_eval_state(evalState, suspended: newSuspended)
  end

  defp eval({:code_change, modules}, evalState) do
    eval({:code_change, :up, modules}, evalState)
  end

  defp eval({:code_change, mode, modules}, evalState) do
    suspended = r_eval_state(evalState, :suspended)
    vsns = r_eval_state(evalState, :vsns)
    timeout = get_opt(:code_change_timeout, evalState, :default)

    :lists.foreach(
      fn {mod, extra} ->
        vsn =
          case :lists.keysearch(mod, 1, vsns) do
            {:value, {^mod, oldVsn, _NewVsn}}
            when mode == :up ->
              oldVsn

            {:value, {^mod, _OldVsn, newVsn}}
            when mode == :down ->
              {:down, newVsn}

            _ when mode == :up ->
              :undefined

            _ ->
              {:down, :undefined}
          end

        case :lists.keysearch(mod, 1, suspended) do
          {:value, {_Mod, pids}} ->
            change_code(pids, mod, vsn, extra, timeout)

          _ ->
            :ok
        end
      end,
      modules
    )

    evalState
  end

  defp eval({:stop, modules}, evalState) do
    procs = get_supervised_procs()

    newStopped =
      :lists.foldl(
        fn mod, stopped ->
          procs2 = stop(mod, procs)
          [{mod, procs2} | stopped]
        end,
        r_eval_state(evalState, :stopped),
        modules
      )

    r_eval_state(evalState, stopped: newStopped)
  end

  defp eval({:start, modules}, evalState) do
    newStopped =
      :lists.foldl(
        fn mod, stopped ->
          :lists.filter(
            fn
              {mod2, procs}
              when mod2 == mod ->
                start(procs)
                false

              _ ->
                true
            end,
            stopped
          )
        end,
        r_eval_state(evalState, :stopped),
        modules
      )

    r_eval_state(evalState, stopped: newStopped)
  end

  defp eval({:sync_nodes, id, {m, f, a}}, evalState) do
    sync_nodes(id, apply(m, f, a))
    evalState
  end

  defp eval({:sync_nodes, id, nodes}, evalState) do
    sync_nodes(id, nodes)
    evalState
  end

  defp eval({:apply, {m, f, a}}, evalState) do
    apply(m, f, a)
    evalState
  end

  defp eval(:restart_emulator, _EvalState) do
    throw(:restart_emulator)
  end

  defp eval(:restart_new_emulator, _EvalState) do
    throw(:restart_new_emulator)
  end

  defp get_opt(tag, evalState, default) do
    case :lists.keysearch(tag, 1, r_eval_state(evalState, :opts)) do
      {:value, {_Tag, value}} ->
        value

      false ->
        default
    end
  end

  defp suspend(mod, procs, timeout) do
    :lists.zf(
      fn {_Sup, _Name, pid, mods} ->
        case :lists.member(mod, mods) do
          true ->
            case (try do
                    sys_suspend(pid, timeout)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              :ok ->
                {true, pid}

              _ ->
                try do
                  :sys.resume(pid)
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end

                false
            end

          false ->
            false
        end
      end,
      procs
    )
  end

  defp sys_suspend(pid, :default) do
    :sys.suspend(pid)
  end

  defp sys_suspend(pid, timeout) do
    :sys.suspend(pid, timeout)
  end

  defp resume(pids) do
    :lists.foreach(
      fn pid ->
        try do
          :sys.resume(pid)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end
      end,
      pids
    )
  end

  defp change_code(pids, mod, vsn, extra, timeout) do
    fun = fn pid ->
      case sys_change_code(pid, mod, vsn, extra, timeout) do
        :ok ->
          :ok

        {:error, reason} ->
          throw({:code_change_failed, pid, mod, vsn, reason})
      end
    end

    :lists.foreach(fun, pids)
  end

  defp sys_change_code(pid, mod, vsn, extra, :default) do
    :sys.change_code(pid, mod, vsn, extra)
  end

  defp sys_change_code(pid, mod, vsn, extra, timeout) do
    :sys.change_code(pid, mod, vsn, extra, timeout)
  end

  defp stop(mod, procs) do
    :lists.zf(
      fn
        {:undefined, _Name, _Pid, _Mods} ->
          false

        {sup, name, _Pid, mods} ->
          case :lists.member(mod, mods) do
            true ->
              case (try do
                      :supervisor.terminate_child(sup, name)
                    catch
                      :error, e -> {:EXIT, {e, __STACKTRACE__}}
                      :exit, e -> {:EXIT, e}
                      e -> e
                    end) do
                :ok ->
                  {true, {sup, name}}

                _ ->
                  false
              end

            false ->
              false
          end
      end,
      procs
    )
  end

  defp start(procs) do
    :lists.foreach(
      fn {sup, name} ->
        try do
          :supervisor.restart_child(sup, name)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end
      end,
      procs
    )
  end

  def get_supervised_procs() do
    :lists.foldl(
      fn application, procs ->
        get_master_procs(application, procs, :application_controller.get_master(application))
      end,
      [],
      get_application_names()
    )
  end

  defp get_supervised_procs(_, root, procs, {:ok, supMod}) do
    get_procs(
      maybe_supervisor_which_children(root, supMod, root),
      root
    ) ++
      [
        {:undefined, :undefined, root, [supMod]}
        | procs
      ]
  end

  defp get_supervised_procs(application, root, procs, {:error, _}) do
    :error_logger.error_msg('release_handler: cannot find top supervisor for application ~w~n', [
      application
    ])

    get_procs(
      maybe_supervisor_which_children(root, application, root),
      root
    ) ++ procs
  end

  defp get_application_names() do
    :lists.map(
      fn {application, _Name, _Vsn} ->
        application
      end,
      :application.which_applications()
    )
  end

  defp get_master_procs(application, procs, pid) when is_pid(pid) do
    {root, _AppMod} = :application_master.get_child(pid)
    get_supervised_procs(application, root, procs, get_supervisor_module(root))
  end

  defp get_master_procs(_, procs, _) do
    procs
  end

  defp get_procs([{name, pid, :worker, :dynamic} | t], sup)
       when is_pid(pid) do
    mods = maybe_get_dynamic_mods(name, pid)
    [{sup, name, pid, mods} | get_procs(t, sup)]
  end

  defp get_procs([{name, pid, :worker, mods} | t], sup)
       when is_pid(pid) and is_list(mods) do
    [{sup, name, pid, mods} | get_procs(t, sup)]
  end

  defp get_procs([{name, pid, :supervisor, mods} | t], sup)
       when is_pid(pid) do
    [
      {sup, name, pid, mods}
      | get_procs(
          t,
          sup
        )
    ] ++
      get_procs(
        maybe_supervisor_which_children(
          pid,
          name,
          pid
        ),
        pid
      )
  end

  defp get_procs([_H | t], sup) do
    get_procs(t, sup)
  end

  defp get_procs(_, _Sup) do
    []
  end

  defp maybe_supervisor_which_children(proc, name, pid) do
    case get_proc_state(proc) do
      :noproc ->
        :error_logger.warning_msg(
          'release_handler: a process (~p) exited during supervision tree interrogation. Continuing ...~n',
          [proc]
        )

        []

      :suspended ->
        :error_logger.error_msg(
          'release_handler: a which_children call to ~p (~w) was avoided. This supervisor is suspended and should likely be upgraded differently. Exiting ...~n',
          [name, pid]
        )

        :erlang.error(:suspended_supervisor)

      :running ->
        case (try do
                :supervisor.which_children(pid)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          res when is_list(res) ->
            res

          other ->
            :error_logger.error_msg(
              'release_handler: ~p~nerror during a which_children call to ~p (~w). [State: running] Exiting ... ~n',
              [other, name, pid]
            )

            :erlang.error(:which_children_failed)
        end
    end
  end

  defp get_proc_state(proc) do
    try do
      :sys.get_status(proc)
    catch
      :exit, {reason, {:sys, :get_status, [^proc]}}
      when reason !== :timeout and
             not (is_tuple(reason) and
                      :erlang.element(
                        1,
                        reason
                      ) === :nodedown) ->
        :noproc
    else
      {:status, _, {:module, _}, [_, state, _, _, _]}
      when state == :running or state == :suspended ->
        state
    end
  end

  defp maybe_get_dynamic_mods(name, pid) do
    try do
      :gen.call(pid, self(), :get_modules)
    catch
      :exit, reason
      when reason !== :timeout and
             not (is_tuple(reason) and
                      :erlang.element(
                        1,
                        reason
                      ) === :nodedown) ->
        []

      :exit, other ->
        :error_logger.error_msg(
          'release_handler: {\'EXIT\',~p}~nerror during a get_modules call to ~p (~w), there may be an error in it\'s childspec. Exiting ...~n',
          [other, name, pid]
        )

        :erlang.error(:get_modules_failed)
    else
      {:ok, res} ->
        res
    end
  end

  defp get_supervisor_module(supPid) do
    case (try do
            :supervisor.get_callback_module(supPid)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      module when is_atom(module) ->
        {:ok, module}

      _Other ->
        :io.format('~w: reason: ~w~n', [supPid, _Other])
        {:error, :undefined}
    end
  end

  defp do_soft_purge(mod, postPurgeMethod, unpurged) do
    isNoOldProcsLeft = :code.soft_purge(mod)

    case :lists.keymember(mod, 1, unpurged) do
      true when isNoOldProcsLeft == true ->
        :lists.keydelete(mod, 1, unpurged)

      true ->
        unpurged

      false when isNoOldProcsLeft == true ->
        unpurged

      false ->
        [{mod, postPurgeMethod} | unpurged]
    end
  end

  defp sync_nodes(id, nodes) do
    nNodes = :lists.delete(node(), nodes)

    :lists.foreach(
      fn node ->
        send({:release_handler, node}, {:sync_nodes, id, node()})
      end,
      nNodes
    )

    :lists.foreach(
      fn node ->
        receive do
          {:sync_nodes, ^id, ^node} ->
            :ok

          {:nodedown, ^node} ->
            throw({:sync_error, {:nodedown, node}})
        end
      end,
      nNodes
    )
  end

  defp add_vsns(mod, newBin, vsns) do
    oldVsn = get_current_vsn(mod)
    newVsn = get_vsn(newBin)

    case :lists.keysearch(mod, 1, vsns) do
      {:value, {^mod, oldVsn0, newVsn0}} ->
        :lists.keyreplace(
          mod,
          1,
          vsns,
          {mod, replace_undefined(oldVsn0, oldVsn), replace_undefined(newVsn0, newVsn)}
        )

      false ->
        [{mod, oldVsn, newVsn} | vsns]
    end
  end

  defp replace_undefined(:undefined, vsn) do
    vsn
  end

  defp replace_undefined(vsn, _) do
    vsn
  end

  def get_current_vsn(mod) do
    file = :code.which(mod)

    case :erl_prim_loader.get_file(file) do
      {:ok, bin, _File2} ->
        get_vsn(bin)

      :error ->
        :undefined
    end
  end

  defp get_vsn(bin) do
    {:ok, {_Mod, vsn}} = :beam_lib.version(bin)

    case :misc_supp.is_string(vsn) do
      true ->
        vsn

      false ->
        case vsn do
          [vsnTerm] ->
            vsnTerm

          _ ->
            vsn
        end
    end
  end
end
