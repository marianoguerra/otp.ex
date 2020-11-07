defmodule :m_test_server do
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

  def init_target_info() do
    [?. | emu] = :code.objfile_extension()
    {_, oTPRel} = :init.script_id()

    r_target_info(
      os_family: :test_server_sup.get_os_family(),
      os_type: :os.type(),
      version: :erlang.system_info(:version),
      system_version: :erlang.system_info(:system_version),
      root_dir: :code.root_dir(),
      emulator: emu,
      otp_release: oTPRel,
      username: :test_server_sup.get_username(),
      cookie: :erlang.atom_to_list(:erlang.get_cookie())
    )
  end

  def init_valgrind() do
    valgrind_new_leaks()
  end

  def cover_compile(coverInfo = r_cover(app: :none, incl: include, cross: cross)) do
    crossMods =
      :lists.flatmap(
        fn {_, m} ->
          m
        end,
        cross
      )

    compileMods = include ++ crossMods

    case length(compileMods) do
      0 ->
        :io.fwrite('WARNING: No modules to cover compile!\n\n', [])
        {:ok, _} = start_cover()
        {:ok, r_cover(coverInfo, mods: [])}

      n ->
        :io.fwrite('Cover compiling ~w modules - this may take some time... ', [n])
        do_cover_compile(compileMods)
        :io.fwrite('done\n\n', [])
        {:ok, r_cover(coverInfo, mods: include)}
    end
  end

  def cover_compile(coverInfo = r_cover(app: app, excl: :all, incl: include, cross: cross)) do
    crossMods =
      :lists.flatmap(
        fn {_, m} ->
          m
        end,
        cross
      )

    compileMods = include ++ crossMods

    case length(compileMods) do
      0 ->
        :io.fwrite('WARNING: No modules to cover compile!\n\n', [])
        {:ok, _} = start_cover()
        {:ok, r_cover(coverInfo, mods: [])}

      n ->
        :io.fwrite('Cover compiling \'~w\' (~w files) - this may take some time... ', [app, n])

        :io.format(
          '\nWARNING: All modules in \'~w\' are excluded\nOnly cover compiling modules in include list and the modules\nin the cross cover file:\n~tp\n',
          [app, compileMods]
        )

        do_cover_compile(compileMods)
        :io.fwrite('done\n\n', [])
        {:ok, r_cover(coverInfo, mods: include)}
    end
  end

  def cover_compile(coverInfo = r_cover(app: app, excl: exclude, incl: include, cross: cross)) do
    crossMods =
      :lists.flatmap(
        fn {_, m} ->
          m
        end,
        cross
      )

    case :code.lib_dir(app) do
      {:error, :bad_name} ->
        case include ++ crossMods do
          [] ->
            :io.format('\nWARNING: Can\'t find lib_dir for \'~w\'\nNot cover compiling!\n\n', [
              app
            ])

            {:error, :application_not_found}

          compileMods ->
            :io.fwrite('Cover compiling \'~w\' (~w files) - this may take some time... ', [
              app,
              length(compileMods)
            ])

            :io.format(
              '\nWARNING: Can\'t find lib_dir for \'~w\'\nOnly cover compiling modules in include list: ~tp\n',
              [app, include]
            )

            do_cover_compile(compileMods)
            :io.fwrite('done\n\n', [])
            {:ok, r_cover(coverInfo, mods: include)}
        end

      libDir ->
        ebinDir = :filename.join([libDir, 'ebin'])
        wC = :filename.join(ebinDir, '*.beam')
        allMods = module_names(:filelib.wildcard(wC))
        analyseMods = (allMods ++ include) -- exclude
        compileMods = analyseMods ++ crossMods

        case length(compileMods) do
          0 ->
            :io.fwrite('WARNING: No modules to cover compile!\n\n', [])
            {:ok, _} = start_cover()
            {:ok, r_cover(coverInfo, mods: [])}

          n ->
            :io.fwrite('Cover compiling \'~w\' (~w files) - this may take some time... ', [app, n])

            do_cover_compile(compileMods)
            :io.fwrite('done\n\n', [])
            {:ok, r_cover(coverInfo, mods: analyseMods)}
        end
    end
  end

  defp module_names(beams) do
    for beam <- beams do
      :erlang.list_to_atom(:filename.basename(:filename.rootname(beam)))
    end
  end

  defp do_cover_compile(modules) do
    {:ok, _} = start_cover()
    sticky = prepare_cover_compile(modules, [])
    r = :cover.compile_beam(modules)

    _ =
      for error <- r,
          :erlang.element(1, error) !== :ok do
        warn_compile(error)
      end

    _ =
      for m <- sticky do
        :code.stick_mod(m)
      end

    :ok
  end

  defp warn_compile({:error, {reason, module}}) do
    :io.fwrite('\nWARNING: Could not cover compile ~ts: ~tp\n', [module, {:error, reason}])
  end

  defp prepare_cover_compile([m | ms], sticky) do
    case {:code.is_sticky(m), :code.is_loaded(m)} do
      {true, _} ->
        :code.unstick_mod(m)
        prepare_cover_compile(ms, [m | sticky])

      {false, false} ->
        case :code.load_file(m) do
          {:module, _} ->
            prepare_cover_compile([m | ms], sticky)

          error ->
            :io.fwrite('\nWARNING: Could not load ~w: ~tp\n', [m, error])
            prepare_cover_compile(ms, sticky)
        end

      {false, _} ->
        prepare_cover_compile(ms, sticky)
    end
  end

  defp prepare_cover_compile([], sticky) do
    sticky
  end

  def cover_analyse(
        dir,
        r_cover(level: analyse, mods: modules, stop: stop)
      ) do
    :io.fwrite(:user, 'Cover analysing... ', [])

    {aTFOk, aTFFail} =
      case analyse do
        :details ->
          case :cover.export(:filename.join(dir, 'all.coverdata')) do
            :ok ->
              {:result, ok1, fail1} =
                :cover.analyse_to_file(
                  modules,
                  [{:outdir, dir}, :html]
                )

              {:lists.map(
                 fn outFile ->
                   m =
                     :erlang.list_to_atom(
                       :filename.basename(
                         :filename.rootname(
                           outFile,
                           '.COVER.html'
                         )
                       )
                     )

                   {m, {:file, outFile}}
                 end,
                 ok1
               ),
               :lists.map(
                 fn {reason, m} ->
                   {m, {:error, reason}}
                 end,
                 fail1
               )}

            error ->
              {[],
               :lists.map(
                 fn m ->
                   {m, error}
                 end,
                 modules
               )}
          end

        :overview ->
          case :cover.export(:filename.join(dir, 'all.coverdata')) do
            :ok ->
              {[],
               :lists.map(
                 fn m ->
                   {m, :undefined}
                 end,
                 modules
               )}

            error ->
              {[],
               :lists.map(
                 fn m ->
                   {m, error}
                 end,
                 modules
               )}
          end
      end

    {:result, aOk, aFail} = :cover.analyse(modules, :module)

    r0 =
      merge_analysis_results(aOk, aTFOk ++ aTFFail, []) ++
        for {reason, m} <- aFail do
          {m, {:error, reason}}
        end

    r = :lists.sort(r0)
    :io.fwrite(:user, 'done\n\n', [])

    case stop do
      true ->
        sticky = unstick_all_sticky(node())
        :cover.stop()
        stick_all_sticky(node(), sticky)

      false ->
        :ok
    end

    r
  end

  defp merge_analysis_results([{m, {cov, notCov}} | t], aTF, acc) do
    case :lists.keytake(m, 1, aTF) do
      {:value, {_, r}, aTF1} ->
        merge_analysis_results(t, aTF1, [{m, {cov, notCov, r}} | acc])

      false ->
        merge_analysis_results(t, aTF, acc)
    end
  end

  defp merge_analysis_results([], _, acc) do
    acc
  end

  defp do_cover_for_node(node, coverFunc) do
    do_cover_for_node(node, coverFunc, true)
  end

  defp do_cover_for_node(node, coverFunc, stickUnstick) do
    mainCoverNode = :cover.get_main_node()

    sticky =
      cond do
        stickUnstick ->
          unstick_all_sticky(mainCoverNode, node)

        true ->
          :ok
      end

    :rpc.call(mainCoverNode, :cover, coverFunc, [node])

    cond do
      stickUnstick ->
        stick_all_sticky(node, sticky)

      true ->
        :ok
    end
  end

  defp unstick_all_sticky(node) do
    unstick_all_sticky(node(), node)
  end

  defp unstick_all_sticky(mainCoverNode, node) do
    :lists.filter(
      fn m ->
        case :code.is_sticky(m) do
          true ->
            :rpc.call(node, :code, :unstick_mod, [m])
            true

          false ->
            false
        end
      end,
      :rpc.call(mainCoverNode, :cover, :modules, [])
    )
  end

  defp stick_all_sticky(node, sticky) do
    :lists.foreach(
      fn m ->
        :rpc.call(node, :code, :stick_mod, [m])
      end,
      sticky
    )
  end

  def run_test_case_apply({caseNum, mod, func, args, name, runInit, timetrapData}) do
    case is_valgrind() do
      false ->
        :ok

      true ->
        valgrind_format('Test case #~w ~w:~w/1', [caseNum, mod, func])

        :os.putenv(
          'VALGRIND_LOGFILE_INFIX',
          :erlang.atom_to_list(mod) ++ '.' ++ :erlang.atom_to_list(func) ++ '-'
        )
    end

    procBef = :erlang.system_info(:process_count)
    result = run_test_case_apply(mod, func, args, name, runInit, timetrapData)
    procAft = :erlang.system_info(:process_count)
    valgrind_new_leaks()
    detFail = :erlang.get(:test_server_detected_fail)
    {result, detFail, procBef, procAft}
  end

  Record.defrecord(:r_st, :st,
    ref: :undefined,
    pid: :undefined,
    mf: :undefined,
    last_known_loc: :undefined,
    status: :undefined,
    ret_val: :undefined,
    comment: :undefined,
    timeout: :undefined,
    config: :undefined,
    end_conf_pid: :undefined
  )

  defp run_test_case_apply(mod, func, args, name, runInit, timetrapData) do
    print_timestamp(:minor, 'Started at ')
    print(:minor, '', [], :internal_raw)
    tCCallback = :erlang.get(:test_server_testcase_callback)
    logOpts = :erlang.get(:test_server_logopts)
    ref = make_ref()

    pid =
      spawn_link(
        run_test_case_eval_fun(
          mod,
          func,
          args,
          name,
          ref,
          runInit,
          timetrapData,
          logOpts,
          tCCallback
        )
      )

    :erlang.put(:test_server_detected_fail, [])

    st =
      r_st(
        ref: ref,
        pid: pid,
        mf: {mod, func},
        last_known_loc: :unknown,
        status: :starting,
        ret_val: [],
        comment: '',
        timeout: :infinity,
        config: hd(args)
      )

    :ct_util.mark_process()
    run_test_case_msgloop(st)
  end

  defp run_test_case_msgloop(r_st(ref: ref, pid: pid, end_conf_pid: endConfPid0) = st0) do
    receive do
      {:set_tc_state = tag, from, {status, config0}} ->
        config =
          case config0 do
            :unknown ->
              r_st(st0, :config)

            _ ->
              config0
          end

        st = r_st(st0, status: status, config: config)
        send(from, {self(), tag, :ok})
        run_test_case_msgloop(st)

      {:abort_current_testcase, _, _} = abort
      when r_st(st0, :status) === :starting ->
        send(self(), abort)
        :erlang.yield()
        run_test_case_msgloop(st0)

      {:abort_current_testcase, reason, from} ->
        line =
          case :erlang.is_process_alive(pid) do
            true ->
              get_loc(pid)

            false ->
              :unknown
          end

        mon = :erlang.monitor(:process, pid)
        :erlang.exit(pid, {:testcase_aborted, reason, line})
        :erlang.yield()
        send(from, {self(), :abort_current_testcase, :ok})

        st =
          receive do
            {:DOWN, ^mon, :process, ^pid, _} ->
              st0
          after
            10000 ->
              :erlang.exit(pid, :kill)
              error = :lists.flatten(:io_lib.format('Aborted: ~tp', [reason]))

              error1 =
                :lists.flatten(
                  for s <-
                        :string.lexemes(
                          error,
                          [?\n]
                        ) do
                    :string.trim(s, :leading, ' ')
                  end
                )

              errorLength = :string.length(error1)

              comment =
                cond do
                  errorLength > 63 ->
                    :string.slice(error1, 0, 60) ++ '...'

                  true ->
                    error1
                end

              r_st(st0, comment: comment)
          end

        run_test_case_msgloop(st)

      {:sync_apply, from, mFA} ->
        do_sync_apply(false, from, mFA)
        run_test_case_msgloop(st0)

      {:sync_apply_proxy, proxy, from, mFA} ->
        do_sync_apply(proxy, from, mFA)
        run_test_case_msgloop(st0)

      {:comment, newComment0} ->
        newComment1 = :test_server_ctrl.to_string(newComment0)

        newComment =
          :test_server_sup.framework_call(
            :format_comment,
            [newComment1],
            newComment1
          )

        run_test_case_msgloop(r_st(st0, comment: newComment))

      {:read_comment, from} ->
        send(from, {self(), :read_comment, r_st(st0, :comment)})
        run_test_case_msgloop(st0)

      {:make_priv_dir, from} ->
        config =
          case r_st(st0, :config) do
            :undefined ->
              []

            config0 ->
              config0
          end

        result =
          case :proplists.get_value(
                 :priv_dir,
                 config
               ) do
            :undefined ->
              {:error, :no_priv_dir_in_config}

            privDir ->
              case :file.make_dir(privDir) do
                :ok ->
                  :ok

                {:error, :eexist} ->
                  :ok

                mkDirError ->
                  {:error, {mkDirError, privDir}}
              end
          end

        send(from, {self(), :make_priv_dir, result})
        run_test_case_msgloop(st0)

      {:EXIT, ^pid, {^ref, time, value, loc, opts}} ->
        retVal = {time / 1_000_000, value, loc, opts}

        st =
          setup_termination(
            retVal,
            r_st(st0, config: :undefined)
          )

        run_test_case_msgloop(st)

      {:EXIT, ^pid, reason} ->
        st =
          case reason do
            {what, [loc0 = {_M, _F, a, [{:file, _} | _]} | _]}
            when is_integer(a) ->
              loc = rewrite_loc_item(loc0)
              handle_tc_exit(what, r_st(st0, last_known_loc: [loc]))

            {what, [[details, loc0 = {_M, _F, a, [{:file, _} | _]}] | _]}
            when is_integer(a) ->
              loc = rewrite_loc_item(loc0)

              handle_tc_exit(
                {what, details},
                r_st(st0, last_known_loc: [loc])
              )

            _ ->
              handle_tc_exit(reason, st0)
          end

        run_test_case_msgloop(st)

      {^endConfPid0, {:call_end_conf, data, _Result}} ->
        r_st(mf: {mod, func}, config: currConf) = st0

        case currConf do
          _ when is_list(currConf) ->
            {_Mod, _Func, tCPid, tCExitReason, loc} = data
            spawn_fw_call(mod, func, currConf, tCPid, tCExitReason, loc, self())

            st =
              r_st(st0,
                config: :undefined,
                end_conf_pid: :undefined
              )

            run_test_case_msgloop(st)

          _ ->
            run_test_case_msgloop(st0)
        end

      {_FwCallPid, :fw_notify_done, {t, value, loc, opts, addToComment}} ->
        retVal = {t, value, loc, opts}
        comment0 = r_st(st0, :comment)

        comment =
          case addToComment do
            :undefined ->
              comment0

            _ ->
              cond do
                comment0 === '' ->
                  addToComment

                true ->
                  comment0 ++
                    :test_server_ctrl.xhtml(
                      '<br>',
                      '<br />'
                    ) ++ addToComment
              end
          end

        st =
          setup_termination(
            retVal,
            r_st(st0, comment: comment, config: :undefined)
          )

        run_test_case_msgloop(st)

      {:EXIT, _FwCallPid, {:fw_notify_done, func, error}} ->
        cB = :os.getenv('TEST_SERVER_FRAMEWORK')

        loc =
          case cB do
            fW when fW === false or fW === 'undefined' ->
              [{:test_server, func}]

            _ ->
              [{:erlang.list_to_atom(cB), func}]
          end

        retVal = {:died, {:framework_error, loc, error}, loc}

        st =
          setup_termination(
            retVal,
            r_st(st0, comment: 'Framework error', config: :undefined)
          )

        run_test_case_msgloop(st)

      {:failed, file, line} ->
        :erlang.put(
          :test_server_detected_fail,
          [
            {file, line}
            | :erlang.get(:test_server_detected_fail)
          ]
        )

        run_test_case_msgloop(st0)

      {:user_timetrap, ^pid, _TrapTime, startTime, e = {:user_timetrap_error, _}, _} ->
        case update_user_timetraps(pid, startTime) do
          :proceed ->
            send(self(), {:abort_current_testcase, e, pid})
            :ok

          :ignore ->
            :ok
        end

        run_test_case_msgloop(st0)

      {:user_timetrap, ^pid, trapTime, startTime, elapsedTime, scale} ->
        case update_user_timetraps(pid, startTime) do
          :proceed ->
            totalTime =
              cond do
                is_integer(trapTime) ->
                  trapTime + elapsedTime

                true ->
                  trapTime
              end

            _ = timetrap(trapTime, totalTime, pid, scale)
            :ok

          :ignore ->
            :ok
        end

        run_test_case_msgloop(st0)

      {:timetrap_cancel_one, handle, _From} ->
        timetrap_cancel_one(handle, false)
        run_test_case_msgloop(st0)

      {:timetrap_cancel_all, tCPid, _From} ->
        timetrap_cancel_all(tCPid, false)
        run_test_case_msgloop(st0)

      {:get_timetrap_info, from, tCPid} ->
        info = get_timetrap_info(tCPid, false)
        send(from, {self(), :get_timetrap_info, info})
        run_test_case_msgloop(st0)

      _Other when not is_tuple(_Other) ->
        run_test_case_msgloop(st0)

      _Other
      when :erlang.element(1, _Other) != :EXIT and
             :erlang.element(1, _Other) != :started and
             :erlang.element(1, _Other) != :finished and
             :erlang.element(1, _Other) != :print ->
        run_test_case_msgloop(st0)
    after
      r_st(st0, :timeout) ->
        r_st(ret_val: retVal, comment: comment) = st0
        :erlang.append_element(retVal, comment)
    end
  end

  defp setup_termination(retVal, r_st(pid: pid) = st) do
    timetrap_cancel_all(pid, false)
    r_st(st, ret_val: retVal, timeout: 20)
  end

  def set_tc_state(state) do
    set_tc_state(state, :unknown)
  end

  defp set_tc_state(state, config) do
    tc_supervisor_req(:set_tc_state, {state, config})
  end

  defp handle_tc_exit(:killed, st) do
    r_st(config: config, mf: {mod, func}, pid: pid) = st
    msg = :testcase_aborted_or_killed
    spawn_fw_call(mod, func, config, pid, msg, :unknown, self())
    st
  end

  defp handle_tc_exit(
         {:testcase_aborted, {:user_timetrap_error, _} = msg, _},
         st
       ) do
    r_st(config: config, mf: {mod, func}, pid: pid) = st
    spawn_fw_call(mod, func, config, pid, msg, :unknown, self())
    st
  end

  defp handle_tc_exit(
         reason,
         r_st(
           status: {:framework, {fwMod, fwFunc, _} = fwMFA},
           config: config,
           mf: {mod, func},
           pid: pid
         ) = st
       ) do
    r =
      case reason do
        {:timetrap_timeout, tVal, _} ->
          {:timetrap, tVal}

        {:testcase_aborted = e, abortReason, _} ->
          {e, abortReason}

        {:fw_error, {^fwMod, ^fwFunc, fwError}} ->
          fwError

        other ->
          other
      end

    error = {:framework_error, r}
    spawn_fw_call(mod, func, config, pid, {error, fwMFA}, :unknown, self())
    st
  end

  defp handle_tc_exit(
         reason,
         r_st(status: :tc, config: config0, mf: {mod, func}, pid: pid) = st
       )
       when is_list(config0) do
    {r, loc1, f} =
      case reason do
        {:timetrap_timeout = e, tVal, loc0} ->
          {{e, tVal}, loc0, e}

        {:testcase_aborted = e, abortReason, loc0} ->
          msg = {e, abortReason}
          {msg, loc0, msg}

        other ->
          {{:EXIT, other}, :unknown, other}
      end

    timeout = end_conf_timeout(reason, st)
    config = [{:tc_status, {:failed, f}} | config0]
    endConfPid = call_end_conf(mod, func, pid, r, loc1, config, timeout)
    r_st(st, end_conf_pid: endConfPid)
  end

  defp handle_tc_exit(
         reason,
         r_st(config: config, mf: {mod, func0}, pid: pid, status: status) = st
       ) do
    {r, loc1} =
      case reason do
        {:timetrap_timeout = e, tVal, loc0} ->
          {{e, tVal}, loc0}

        {:testcase_aborted = e, abortReason, loc0} ->
          {{e, abortReason}, loc0}

        other ->
          {{:EXIT, other}, r_st(st, :last_known_loc)}
      end

    func =
      case status do
        :init_per_testcase = f ->
          {f, func0}

        :end_per_testcase = f ->
          {f, func0}

        _ ->
          func0
      end

    spawn_fw_call(mod, func, config, pid, r, loc1, self())
    st
  end

  defp end_conf_timeout({:timetrap_timeout, timeout, _}, _) do
    timeout
  end

  defp end_conf_timeout(_, r_st(config: config)) when is_list(config) do
    :proplists.get_value(:default_timeout, config, 60 * 1000)
  end

  defp end_conf_timeout(_, _) do
    60 * 1000
  end

  defp call_end_conf(mod, func, tCPid, tCExitReason, loc, conf, tVal) do
    starter = self()
    data = {mod, func, tCPid, tCExitReason, loc}

    case :erlang.function_exported(mod, :end_per_testcase, 2) do
      false ->
        spawn_link(fn ->
          send(starter, {self(), {:call_end_conf, data, :ok}})
        end)

      true ->
        do_call_end_conf(starter, mod, func, data, tCExitReason, conf, tVal)
    end
  end

  defp do_call_end_conf(starter, mod, func, data, tCExitReason, conf, tVal) do
    endConfProc = fn ->
      :erlang.process_flag(:trap_exit, true)
      supervisor = self()

      endConfApply = fn ->
        _ = timetrap(tVal)

        endConf =
          case do_init_tc_call(
                 mod,
                 {:end_per_testcase, func},
                 [conf],
                 {tCExitReason, [conf]}
               ) do
            {_, [ePTCInit]}
            when is_list(ePTCInit) ->
              ePTCInit

            _ ->
              conf
          end

        try do
          apply(mod, :end_per_testcase, [func, endConf])
        catch
          _, error ->
            :timer.sleep(1)
            print_end_conf_result(mod, func, conf, 'crashed', error)
        else
          _ ->
            :ok
        end

        send(supervisor, {self(), :end_conf})
      end

      pid = spawn_link(endConfApply)

      receive do
        {^pid, :end_conf} ->
          send(starter, {self(), {:call_end_conf, data, :ok}})

        {:EXIT, ^pid, reason} ->
          print_end_conf_result(mod, func, conf, 'failed', reason)
          send(starter, {self(), {:call_end_conf, data, {:error, reason}}})

        {:EXIT, _OtherPid, reason} ->
          exit(reason)
      end
    end

    spawn_link(endConfProc)
  end

  defp print_end_conf_result(mod, func, conf, cause, error) do
    str2Print = fn
      noHTML
      when noHTML == :stdout or
             noHTML == :major ->
        :io_lib.format('WARNING! ~w:end_per_testcase(~tw, ~tp) ~s!\n\tReason: ~tp\n', [
          mod,
          func,
          conf,
          cause,
          error
        ])

      :minor ->
        errorStr = :test_server_ctrl.escape_chars(error)

        :io_lib.format('WARNING! ~w:end_per_testcase(~tw, ~tp) ~s!\n\tReason: ~ts\n', [
          mod,
          func,
          conf,
          cause,
          errorStr
        ])
    end

    send(:erlang.group_leader(), {:printout, 12, str2Print})
    :ok
  end

  defp spawn_fw_call(mod, iPTC = {:init_per_testcase, func}, currConf, pid, why, loc, sendTo) do
    fwCall = fn ->
      :ct_util.mark_process()
      skip = {:skip, {:failed, {mod, :init_per_testcase, why}}}

      try do
        do_end_tc_call(mod, iPTC, {pid, skip, [currConf]}, why)
        do_init_tc_call(mod, {:end_per_testcase_not_run, func}, [currConf], {:ok, [currConf]})
        do_end_tc_call(mod, {:end_per_testcase_not_run, func}, {pid, skip, [currConf]}, why)
      catch
        _, fwEndTCErr ->
          exit({:fw_notify_done, :end_tc, fwEndTCErr})
      else
        _ ->
          :ok
      end

      time =
        case why do
          {:timetrap_timeout, tVal} ->
            tVal / 1000

          _ ->
            :died
        end

      send(
        :erlang.group_leader(),
        {:printout, 12, 'ERROR! ~w:init_per_testcase(~tw, ~tp) failed!\n\tReason: ~tp\n',
         [mod, func, currConf, why]}
      )

      send(sendTo, {self(), :fw_notify_done, {time, skip, loc, [], :undefined}})
    end

    spawn_link(fwCall)
  end

  defp spawn_fw_call(mod, ePTC = {:end_per_testcase, func}, endConf, pid, why, _Loc, sendTo) do
    fwCall = fn ->
      :ct_util.mark_process()

      {retVal, report} =
        case :proplists.get_value(
               :tc_status,
               endConf
             ) do
          :undefined ->
            e = {:failed, {mod, :end_per_testcase, why}}
            {e, e}

          e = {:failed, reason} ->
            {e, {:error, reason}}

          result ->
            e = {:failed, {mod, :end_per_testcase, why}}
            {result, e}
        end

      {time, warn} =
        case why do
          {:timetrap_timeout, tVal} ->
            send(
              :erlang.group_leader(),
              {:printout, 12,
               'WARNING! ~w:end_per_testcase(~tw, ~tp) failed!\n\tReason: timetrap timeout after ~w ms!\n',
               [mod, func, endConf, tVal]}
            )

            w = '<font color="red">WARNING: end_per_testcase timed out!</font>'
            {tVal / 1000, w}

          _ ->
            send(
              :erlang.group_leader(),
              {:printout, 12, 'WARNING! ~w:end_per_testcase(~tw, ~tp) failed!\n\tReason: ~tp\n',
               [mod, func, endConf, why]}
            )

            w = '<font color="red">WARNING: end_per_testcase failed!</font>'
            {:died, w}
        end

      failLoc0 = :proplists.get_value(:tc_fail_loc, endConf)

      {retVal1, failLoc} =
        try do
          do_end_tc_call(mod, ePTC, {pid, report, [endConf]}, why)
        catch
          _, fwEndTCErr ->
            exit({:fw_notify_done, :end_tc, fwEndTCErr})
        else
          ^why ->
            {retVal, failLoc0}

          {:failed, _} = r ->
            {r, [{mod, func}]}

          r ->
            {r, failLoc0}
        end

      send(sendTo, {self(), :fw_notify_done, {time, retVal1, failLoc, [], warn}})
    end

    spawn_link(fwCall)
  end

  defp spawn_fw_call(
         mod,
         func,
         conf,
         pid,
         {{:framework_error, fwError}, {fwMod, fwFunc, [[a1, a2] | _]} = fwMFA},
         _,
         sendTo
       ) do
    fwCall = fn ->
      :ct_util.mark_process()

      time =
        case fwError do
          {:timetrap, tVal} ->
            tVal / 1000

          _ ->
            :died
        end

      {ret, loc, warnOrError} = cleanup_after_fw_error(mod, func, conf, pid, fwError, fwMFA)

      comment =
        case warnOrError do
          :warn ->
            send(
              :erlang.group_leader(),
              {:printout, 12, 'WARNING! ~w:~tw(~w,~tw,...) failed!\n    Reason: ~tp\n',
               [fwMod, fwFunc, a1, a2, fwError]}
            )

            :lists.flatten(
              :io_lib.format(
                '<font color="red">WARNING! ~w:~tw(~w,~tw,...) failed!</font>',
                [fwMod, fwFunc, a1, a2]
              )
            )

          :error ->
            send(
              :erlang.group_leader(),
              {:printout, 12, 'Error! ~w:~tw(~w,~tw,...) failed!\n    Reason: ~tp\n',
               [fwMod, fwFunc, a1, a2, fwError]}
            )

            :lists.flatten(
              :io_lib.format(
                '<font color="red">ERROR! ~w:~tw(~w,~tw,...) failed!</font>',
                [fwMod, fwFunc, a1, a2]
              )
            )
        end

      send(sendTo, {self(), :fw_notify_done, {time, ret, loc, [], comment}})
    end

    spawn_link(fwCall)
  end

  defp spawn_fw_call(mod, func, currConf, pid, error, loc, sendTo) do
    :ct_util.mark_process()

    {func1, endTCFunc} =
      case func do
        cF
        when cF == :init_per_suite or
               cF == :end_per_suite or
               cF == :init_per_group or
               cF == :end_per_group ->
          {cF, cF}

        tC ->
          {tC, {:end_per_testcase, tC}}
      end

    fwCall = fn ->
      try do
        fw_error_notify(mod, func1, [], error, loc)
      catch
        _, fwErrorNotifyErr ->
          exit({:fw_notify_done, :error_notification, fwErrorNotifyErr})
      else
        _ ->
          :ok
      end

      conf = [{:tc_status, {:failed, error}} | currConf]

      {time, retVal, loc1} =
        try do
          do_end_tc_call(mod, endTCFunc, {pid, error, [conf]}, error)
        catch
          _, fwEndTCErr ->
            exit({:fw_notify_done, :end_tc, fwEndTCErr})
        else
          ^error ->
            {:died, error, loc}

          {:failed, reason} = newReturn ->
            fw_error_notify(mod, func1, conf, reason)
            {:died, newReturn, [{mod, func}]}

          newReturn ->
            t =
              case error do
                {:timetrap_timeout, tT} ->
                  tT

                _ ->
                  0
              end

            {t, newReturn, loc}
        end

      send(sendTo, {self(), :fw_notify_done, {time, retVal, loc1, [], :undefined}})
    end

    spawn_link(fwCall)
  end

  defp cleanup_after_fw_error(
         _Mod,
         _Func,
         conf,
         pid,
         fwError,
         {fwMod, fwFunc = :init_tc, [[mod, {:init_per_testcase, func} = iPTC] | _]}
       ) do
    skip = {:auto_skip, {:failed, {fwMod, fwFunc, fwError}}}

    try do
      do_end_tc_call(mod, iPTC, {pid, skip, [conf]}, fwError)
      do_init_tc_call(mod, {:end_per_testcase_not_run, func}, [conf], {:ok, [conf]})
      do_end_tc_call(mod, {:end_per_testcase_not_run, func}, {pid, skip, [conf]}, fwError)
    catch
      _, fwEndTCErr ->
        exit({:fw_notify_done, :end_tc, fwEndTCErr})
    else
      _ ->
        :ok
    end

    {skip, {fwMod, fwFunc}, :error}
  end

  defp cleanup_after_fw_error(
         _Mod,
         _Func,
         conf,
         pid,
         fwError,
         {fwMod, fwFunc = :end_tc, [[mod, {:init_per_testcase, func}] | _]}
       ) do
    skip = {:auto_skip, {:failed, {fwMod, fwFunc, fwError}}}

    try do
      do_init_tc_call(mod, {:end_per_testcase_not_run, func}, [conf], {:ok, [conf]})
      do_end_tc_call(mod, {:end_per_testcase_not_run, func}, {pid, skip, [conf]}, fwError)
    catch
      _, fwEndTCErr ->
        exit({:fw_notify_done, :end_tc, fwEndTCErr})
    else
      _ ->
        :ok
    end

    {skip, {fwMod, fwFunc}, :error}
  end

  defp cleanup_after_fw_error(
         _Mod,
         _Func,
         conf,
         pid,
         fwError,
         {fwMod, fwFunc = :init_tc, [[mod, {:end_per_testcase, func}] | _]}
       ) do
    {retVal, loc} =
      case {:proplists.get_value(
              :tc_status,
              conf
            ), :proplists.get_value(:tc_fail_loc, conf, :unknown)} do
        {:undefined, _} ->
          {{:failed, {fwMod, fwFunc, fwError}}, {fwMod, fwFunc}}

        {e = {:failed, _Reason}, :unknown} ->
          {e, [{mod, func}]}

        {result, failLoc} ->
          {result, failLoc}
      end

    try do
      do_end_tc_call(mod, {:end_per_testcase_not_run, func}, {pid, retVal, [conf]}, fwError)
    catch
      _, fwEndTCErr ->
        exit({:fw_notify_done, :end_tc, fwEndTCErr})
    else
      _ ->
        :ok
    end

    {retVal, loc, :warn}
  end

  defp cleanup_after_fw_error(
         mod,
         func,
         conf,
         pid,
         fwError,
         {fwMod, fwFunc = :end_tc, [[mod, {:end_per_testcase, func}] | _]}
       ) do
    {retVal, report, loc} =
      case {:proplists.get_value(:tc_status, conf),
            :proplists.get_value(:tc_fail_loc, conf, :unknown)} do
        {:undefined, _} ->
          {{:failed, {fwMod, fwFunc, fwError}}, {{fwMod, fwError}, fwError}, {fwMod, fwFunc}}

        {e = {:failed, _Reason}, :unknown} ->
          {e, {mod, func, e}, [{mod, func}]}

        {result, failLoc} ->
          {result, {mod, func, result}, failLoc}
      end

    try do
      do_end_tc_call(
        mod,
        {:cleanup, {:end_per_testcase_not_run, func}},
        {pid, retVal, [conf]},
        fwError
      )
    catch
      _, fwEndTCErr ->
        exit({:fw_notify_done, :end_tc, fwEndTCErr})
    else
      _ ->
        :ok
    end

    :test_server_sup.framework_call(
      :report,
      [:framework_error, report]
    )

    {retVal, loc, :warn}
  end

  defp cleanup_after_fw_error(mod, func, conf, pid, fwError, {fwMod, fwFunc = :init_tc, _})
       when func === :init_per_suite or
              func === :init_per_group do
    retVal = {:failed, {fwMod, fwFunc, fwError}}

    try do
      do_end_tc_call(mod, func, {pid, retVal, [conf]}, fwError)
    catch
      _, fwEndTCErr ->
        exit({:fw_notify_done, :end_tc, fwEndTCErr})
    else
      _ ->
        :ok
    end

    {retVal, {fwMod, fwFunc}, :error}
  end

  defp cleanup_after_fw_error(mod, func, conf, pid, fwError, {fwMod, fwFunc = :end_tc, _})
       when func === :init_per_suite or
              func === :init_per_group do
    retVal = {:failed, {fwMod, fwFunc, fwError}}

    try do
      do_end_tc_call(mod, {:cleanup, func}, {pid, retVal, [conf]}, fwError)
    catch
      _, fwEndTCErr ->
        exit({:fw_notify_done, :end_tc, fwEndTCErr})
    else
      _ ->
        :ok
    end

    reportFunc =
      case func do
        :init_per_group ->
          case :proplists.get_value(
                 :tc_group_properties,
                 conf
               ) do
            :undefined ->
              {func, :unknown, []}

            gProps ->
              name = :proplists.get_value(:name, gProps)
              {func, name, :proplists.delete(:name, gProps)}
          end

        _ ->
          func
      end

    :test_server_sup.framework_call(
      :report,
      [:framework_error, {mod, reportFunc, retVal}]
    )

    {retVal, {fwMod, fwFunc}, :error}
  end

  defp cleanup_after_fw_error(mod, func, conf, pid, fwError, {fwMod, fwFunc = :init_tc, _})
       when func === :end_per_suite or
              func === :end_per_group do
    retVal = {:failed, {fwMod, fwFunc, fwError}}

    try do
      do_end_tc_call(mod, func, {pid, retVal, [conf]}, fwError)
    catch
      _, fwEndTCErr ->
        exit({:fw_notify_done, :end_tc, fwEndTCErr})
    else
      _ ->
        :ok
    end

    {retVal, {fwMod, fwFunc}, :error}
  end

  defp cleanup_after_fw_error(mod, func, conf, pid, fwError, {fwMod, fwFunc = :end_tc, _})
       when func === :end_per_suite or
              func === :end_per_group do
    retVal = {:failed, {fwMod, fwFunc, fwError}}

    try do
      do_end_tc_call(mod, {:cleanup, func}, {pid, retVal, [conf]}, fwError)
    catch
      _, fwEndTCErr ->
        exit({:fw_notify_done, :end_tc, fwEndTCErr})
    else
      _ ->
        :ok
    end

    reportFunc =
      case func do
        :end_per_group ->
          case :proplists.get_value(
                 :tc_group_properties,
                 conf
               ) do
            :undefined ->
              {func, :unknown, []}

            gProps ->
              name = :proplists.get_value(:name, gProps)
              {func, name, :proplists.delete(:name, gProps)}
          end

        _ ->
          func
      end

    :test_server_sup.framework_call(
      :report,
      [:framework_error, {mod, reportFunc, retVal}]
    )

    {retVal, {fwMod, fwFunc}, :error}
  end

  defp cleanup_after_fw_error(_Mod, _Func, _Conf, _Pid, fwError, {fwMod, fwFunc, _}) do
    :test_server_sup.framework_call(
      :report,
      [:framework_error, {{fwMod, fwFunc}, fwError}]
    )

    {fwError, {fwMod, fwFunc}, :error}
  end

  defp start_job_proxy() do
    :erlang.group_leader(
      spawn(fn ->
        job_proxy_msgloop()
      end),
      self()
    )

    :ok
  end

  defp io_reply_proxy(replyTo) do
    :ct_util.mark_process()

    receive do
      ioReply
      when is_tuple(ioReply) and
             :erlang.element(1, ioReply) == :io_reply ->
        send(replyTo, ioReply)

      _ ->
        io_reply_proxy(replyTo)
    end
  end

  defp job_proxy_msgloop() do
    :ct_util.mark_process()

    receive do
      ioReq
      when tuple_size(ioReq) >= 2 and
             :erlang.element(1, ioReq) == :io_request ->
        replyProxy =
          spawn(fn ->
            io_reply_proxy(:erlang.element(2, ioReq))
          end)

        send(:erlang.group_leader(), :erlang.setelement(2, ioReq, replyProxy))

      {:sync_apply, from, mFA} ->
        send(:erlang.group_leader(), {:sync_apply_proxy, self(), from, mFA})

      {:sync_result_proxy, to, result} ->
        send(to, {:sync_result, result})

      msg ->
        send(:erlang.group_leader(), msg)
    end

    job_proxy_msgloop()
  end

  defp run_test_case_eval_fun(
         mod,
         func,
         args,
         name,
         ref,
         runInit,
         timetrapData,
         logOpts,
         tCCallback
       ) do
    fn ->
      run_test_case_eval(mod, func, args, name, ref, runInit, timetrapData, logOpts, tCCallback)
    end
  end

  defp run_test_case_eval(mod, func, args0, name, ref, runInit, timetrapData, logOpts, tCCallback) do
    :erlang.put(
      :test_server_multiply_timetraps,
      timetrapData
    )

    :erlang.put(:test_server_logopts, logOpts)
    where = [{mod, func}]
    :erlang.put(:test_server_loc, where)

    fWInitFunc =
      case runInit do
        :run_init ->
          {:init_per_testcase, func}

        _ ->
          func
      end

    fWInitResult0 = do_init_tc_call(mod, fWInitFunc, args0, {:ok, args0})
    set_tc_state(:running)

    {{time, value}, loc, opts} =
      case fWInitResult0 do
        {:ok, args} ->
          run_test_case_eval1(mod, func, args, name, runInit, tCCallback)

        error = {:error, _Reason} ->
          newResult =
            do_end_tc_call(
              mod,
              fWInitFunc,
              {error, args0},
              {:auto_skip, {:failed, error}}
            )

          {{0, newResult}, where, []}

        {:fail, reason} ->
          conf = [
            {:tc_status, {:failed, reason}}
            | hd(args0)
          ]

          fw_error_notify(mod, func, conf, reason)

          newResult =
            do_end_tc_call(
              mod,
              fWInitFunc,
              {{:error, reason}, [conf]},
              {:fail, reason}
            )

          {{0, newResult}, where, []}

        skip = {skipType, _Reason}
        when skipType == :skip or
               skipType == :skipped ->
          newResult =
            do_end_tc_call(
              mod,
              fWInitFunc,
              {skip, args0},
              skip
            )

          {{0, newResult}, where, []}

        autoSkip = {:auto_skip, _Reason} ->
          newResult =
            do_end_tc_call(
              mod,
              fWInitFunc,
              {autoSkip, args0},
              autoSkip
            )

          {{0, newResult}, where, []}
      end

    exit({ref, time, value, loc, opts})
  end

  defp run_test_case_eval1(mod, func, args, name, runInit, tCCallback) do
    case runInit do
      :run_init ->
        set_tc_state(:init_per_testcase, hd(args))
        ensure_timetrap(args)

        case init_per_testcase(mod, func, args) do
          skip = {skipType, reason}
          when skipType == :skip or
                 skipType == :skipped ->
            line = get_loc()
            conf = [{:tc_status, {:skipped, reason}} | hd(args)]
            newRes = do_end_tc_call(mod, {:init_per_testcase, func}, {skip, [conf]}, skip)
            {{0, newRes}, line, []}

          {:skip_and_save, reason, saveCfg} ->
            line = get_loc()

            conf = [
              [{:tc_status, {:skipped, reason}}, {:save_config, saveCfg}]
              | hd(args)
            ]

            newRes =
              do_end_tc_call(
                mod,
                {:init_per_testcase, func},
                {{:skip, reason}, [conf]},
                {:skip, reason}
              )

            {{0, newRes}, line, []}

          failTC = {:fail, reason} ->
            endConf = [{:tc_status, {:failed, reason}} | hd(args)]
            fw_error_notify(mod, func, endConf, reason)

            newRes =
              do_end_tc_call(
                mod,
                {:init_per_testcase, func},
                {{:error, reason}, [endConf]},
                failTC
              )

            {{0, newRes}, [{mod, func}], []}

          {:ok, newConf} ->
            iPTCEndRes =
              do_end_tc_call(mod, {:init_per_testcase, func}, {:ok, [newConf]}, newConf)

            {{t, return}, loc, newConf1} =
              cond do
                not is_list(iPTCEndRes) ->
                  {{0, iPTCEndRes}, :undefined, newConf}

                true ->
                  newConfUC =
                    user_callback(
                      tCCallback,
                      mod,
                      func,
                      :init,
                      iPTCEndRes
                    )

                  set_tc_state(:tc, newConfUC)
                  {ts_tc(mod, func, [newConfUC]), get_loc(), newConfUC}
              end

            {endConf, tSReturn, fWReturn} =
              case return do
                {e, tCError}
                when e == :EXIT or
                       e == :failed ->
                  fw_error_notify(mod, func, newConf1, tCError, loc)

                  {[
                     [{:tc_status, {:failed, tCError}}, {:tc_fail_loc, loc}]
                     | newConf1
                   ], return, {:error, tCError}}

                saveCfg = {:save_config, _} ->
                  {[
                     [{:tc_status, :ok}, saveCfg]
                     | newConf1
                   ], return, :ok}

                {:skip_and_save, why, saveCfg} ->
                  skip = {:skip, why}

                  {[
                     [{:tc_status, {:skipped, why}}, {:save_config, saveCfg}]
                     | newConf1
                   ], skip, skip}

                {skipType, why}
                when skipType == :skip or
                       skipType == :skipped ->
                  {[
                     {:tc_status, {:skipped, why}}
                     | newConf1
                   ], return, return}

                _ ->
                  {[
                     {:tc_status, :ok}
                     | newConf1
                   ], return, :ok}
              end

            endConf1 = user_callback(tCCallback, mod, func, :end, endConf)
            set_tc_state(:tc, endConf1)

            endConf2 =
              case do_init_tc_call(
                     mod,
                     {:end_per_testcase, func},
                     [endConf1],
                     {:ok, [endConf1]}
                   ) do
                {:ok, [ePTCInitRes]} when is_list(ePTCInitRes) ->
                  ePTCInitRes

                _ ->
                  endConf1
              end

            {fWReturn1, tSReturn1, endConf3} =
              case end_per_testcase(mod, func, endConf2) do
                saveCfg1 = {:save_config, _} ->
                  {fWReturn, tSReturn, [saveCfg1 | :lists.keydelete(:save_config, 1, endConf2)]}

                {:fail, reasonToFail} ->
                  fw_error_notify(mod, func, endConf2, reasonToFail)
                  {{:error, reasonToFail}, {:failed, reasonToFail}, endConf2}

                {:failed, {_, :end_per_testcase, _}} = failure
                when fWReturn == :ok ->
                  {failure, tSReturn, endConf2}

                _ ->
                  {fWReturn, tSReturn, endConf2}
              end

            case do_end_tc_call(
                   mod,
                   {:end_per_testcase, func},
                   {fWReturn1, [endConf3]},
                   tSReturn1
                 ) do
              {:failed, reason} = newReturn ->
                fw_error_notify(mod, func, endConf3, reason)
                {{t, newReturn}, [{mod, func}], []}

              newReturn ->
                {{t, newReturn}, loc, []}
            end
        end

      :skip_init ->
        set_tc_state(:running, hd(args))
        args1 = user_callback(tCCallback, mod, func, :init, args)
        ensure_timetrap(args1)

        args2 =
          cond do
            name == :undefined ->
              args1

            true ->
              [name | args1]
          end

        {{t, return}, loc} = {ts_tc(mod, func, args2), get_loc()}
        return1 = user_callback(tCCallback, mod, func, :end, return)
        {return2, opts} = process_return_val([return1], mod, func, args1, [{mod, func}], return1)
        {{t, return2}, loc, opts}
    end
  end

  defp do_init_tc_call(mod, func, res, return) do
    :test_server_sup.framework_call(:init_tc, [mod, func, res], return)
  end

  defp do_end_tc_call(mod, iPTC = {:init_per_testcase, func}, res, return) do
    case return do
      {nOk, _}
      when nOk == :auto_skip or nOk == :fail or
             nOk == :skip or nOk == :skipped ->
        {_, args} = res

        {newConfig, iPTCEndRes} =
          case do_end_tc_call1(mod, iPTC, res, return) do
            iPTCEndConfig when is_list(iPTCEndConfig) ->
              {iPTCEndConfig, iPTCEndConfig}

            {:failed, retReason}
            when return === {:fail, retReason} ->
              {args, return}

            {sF, _} = iPTCEndResult
            when sF === :skip or sF === :skipped or
                   sF === :fail or sF === :failed ->
              {args, iPTCEndResult}

            _ ->
              {args, return}
          end

        ePTCInitRes =
          case do_init_tc_call(mod, {:end_per_testcase_not_run, func}, newConfig, iPTCEndRes) do
            {:ok, ePTCInitConfig} when is_list(ePTCInitConfig) ->
              {iPTCEndRes, ePTCInitConfig}

            _ ->
              {iPTCEndRes, newConfig}
          end

        do_end_tc_call1(mod, {:end_per_testcase_not_run, func}, ePTCInitRes, iPTCEndRes)

      _Ok ->
        do_end_tc_call1(mod, iPTC, res, return)
    end
  end

  defp do_end_tc_call(mod, func, res, return) do
    do_end_tc_call1(mod, func, res, return)
  end

  defp do_end_tc_call1(mod, func, res, return) do
    fwMod = :os.getenv('TEST_SERVER_FRAMEWORK')
    ref = make_ref()

    cond do
      fwMod == 'ct_framework' or fwMod == 'undefined' or fwMod == false ->
        case :test_server_sup.framework_call(
               :end_tc,
               [mod, func, res, return],
               :ok
             ) do
          {:fail, fWReason} ->
            {:failed, fWReason}

          :ok ->
            case return do
              {:fail, reason} ->
                {:failed, reason}

              ^return ->
                return
            end

          newReturn ->
            newReturn
        end

      true ->
        case :test_server_sup.framework_call(fwMod, :end_tc, [mod, func, res], ref) do
          {:fail, fWReason} ->
            {:failed, fWReason}

          _Else ->
            return
        end
    end
  end

  defp process_return_val([return], m, f, a, loc, final)
       when is_list(return) do
    returnTags = [:skip, :skip_and_save, :save_config, :comment, :return_group_result]

    case :lists.all(
           fn
             val when is_tuple(val) ->
               :lists.any(
                 fn t ->
                   t == :erlang.element(1, val)
                 end,
                 returnTags
               )

             :ok ->
               true

             _ ->
               false
           end,
           return
         ) do
      true ->
        process_return_val1(return, m, f, a, loc, final, [])

      false ->
        case do_end_tc_call(m, f, {:ok, a}, return) do
          {:failed, fWReason} = failed ->
            fw_error_notify(m, f, a, fWReason)
            {failed, []}

          newReturn ->
            {newReturn, []}
        end
    end
  end

  defp process_return_val(return, m, f, a, loc, final) do
    process_return_val1(return, m, f, a, loc, final, [])
  end

  defp process_return_val1([failed = {e, tCError} | _], m, f, a = [args], loc, _, saveOpts)
       when e == :EXIT or e == :failed do
    fw_error_notify(m, f, a, tCError, loc)

    case do_end_tc_call(
           m,
           f,
           {{:error, tCError}, [[{:tc_status, {:failed, tCError}} | args]]},
           failed
         ) do
      {:failed, fWReason} ->
        {{:failed, fWReason}, saveOpts}

      newReturn ->
        {newReturn, saveOpts}
    end
  end

  defp process_return_val1(
         [saveCfg = {:save_config, _} | opts],
         m,
         f,
         [args],
         loc,
         final,
         saveOpts
       ) do
    process_return_val1(opts, m, f, [[saveCfg | args]], loc, final, saveOpts)
  end

  defp process_return_val1(
         [{:skip_and_save, why, saveCfg} | opts],
         m,
         f,
         [args],
         loc,
         _,
         saveOpts
       ) do
    process_return_val1(
      opts,
      m,
      f,
      [[{:save_config, saveCfg} | args]],
      loc,
      {:skip, why},
      saveOpts
    )
  end

  defp process_return_val1([gR = {:return_group_result, _} | opts], m, f, a, loc, final, saveOpts) do
    process_return_val1(opts, m, f, a, loc, final, [gR | saveOpts])
  end

  defp process_return_val1([retVal = {tag, _} | opts], m, f, a, loc, _, saveOpts)
       when tag == :skip or tag == :comment do
    process_return_val1(opts, m, f, a, loc, retVal, saveOpts)
  end

  defp process_return_val1([_ | opts], m, f, a, loc, final, saveOpts) do
    process_return_val1(opts, m, f, a, loc, final, saveOpts)
  end

  defp process_return_val1([], m, f, a, _Loc, final, saveOpts) do
    case do_end_tc_call(m, f, {final, a}, final) do
      {:failed, fWReason} ->
        {{:failed, fWReason}, saveOpts}

      newReturn ->
        {newReturn, :lists.reverse(saveOpts)}
    end
  end

  defp user_callback(:undefined, _, _, _, args) do
    args
  end

  defp user_callback({cBMod, cBFunc}, mod, func, initOrEnd, [args])
       when is_list(args) do
    case (try do
            apply(cBMod, cBFunc, [initOrEnd, mod, func, args])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      args1 when is_list(args1) ->
        [args1]

      _ ->
        [args]
    end
  end

  defp user_callback({cBMod, cBFunc}, mod, func, initOrEnd, args) do
    case (try do
            apply(cBMod, cBFunc, [initOrEnd, mod, func, args])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      args1 when is_list(args1) ->
        args1

      _ ->
        args
    end
  end

  defp init_per_testcase(mod, func, args) do
    case :code.is_loaded(mod) do
      false ->
        _ = :code.load_file(mod)
        :ok

      _ ->
        :ok
    end

    case :erlang.function_exported(mod, :init_per_testcase, 2) do
      true ->
        do_init_per_testcase(mod, [func | args])

      false ->
        [config] = args
        {:ok, config}
    end
  end

  defp do_init_per_testcase(mod, args) do
    try do
      apply(mod, :init_per_testcase, args)
    catch
      {skip, reason} when skip === :skip or skip === :skipped ->
        {:skip, reason}

      :exit, {skip, reason}
      when skip === :skip or
             skip === :skipped ->
        {:skip, reason}

      other ->
        set_loc(__STACKTRACE__)
        line = get_loc()
        print_init_conf_result(line, 'thrown', other)
        {:skip, {:failed, {mod, :init_per_testcase, other}}}

      _, reason0 ->
        reason = {reason0, __STACKTRACE__}
        set_loc(__STACKTRACE__)
        line = get_loc()
        print_init_conf_result(line, 'crashed', reason)
        {:skip, {:failed, {mod, :init_per_testcase, reason}}}
    else
      {skip, reason} when skip === :skip or skip === :skipped ->
        {:skip, reason}

      {:skip_and_save, _, _} = res ->
        res

      newConf when is_list(newConf) ->
        case :lists.filter(
               fn
                 t when is_tuple(t) ->
                   false

                 _ ->
                   true
               end,
               newConf
             ) do
          [] ->
            {:ok, newConf}

          bad ->
            send(
              :erlang.group_leader(),
              {:printout, 12,
               'ERROR! init_per_testcase has returned bad elements in Config: ~tp\n', [bad]}
            )

            {:skip, {:failed, {mod, :init_per_testcase, :bad_return}}}
        end

      {:fail, _Reason} = res ->
        res

      _Other ->
        send(
          :erlang.group_leader(),
          {:printout, 12, 'ERROR! init_per_testcase did not return a Config list.\n', []}
        )

        {:skip, {:failed, {mod, :init_per_testcase, :bad_return}}}
    end
  end

  defp print_init_conf_result(line, cause, reason) do
    formattedLoc = :test_server_sup.format_loc(line)

    str2Print = fn
      noHTML
      when noHTML == :stdout or
             noHTML == :major ->
        :io_lib.format('ERROR! init_per_testcase ~s!\n\tLocation: ~tp\n\tReason: ~tp\n', [
          cause,
          line,
          reason
        ])

      :minor ->
        reasonStr = :test_server_ctrl.escape_chars(reason)

        :io_lib.format('ERROR! init_per_testcase ~s!\n\tLocation: ~ts\n\tReason: ~ts\n', [
          cause,
          formattedLoc,
          reasonStr
        ])
    end

    send(:erlang.group_leader(), {:printout, 12, str2Print})
    :ok
  end

  defp end_per_testcase(mod, func, conf) do
    case :erlang.function_exported(mod, :end_per_testcase, 2) do
      true ->
        do_end_per_testcase(mod, :end_per_testcase, func, conf)

      false ->
        case :erlang.function_exported(mod, :fin_per_testcase, 2) do
          true ->
            do_end_per_testcase(mod, :fin_per_testcase, func, conf)

          false ->
            :ok
        end
    end
  end

  defp do_end_per_testcase(mod, endFunc, func, conf) do
    set_tc_state(:end_per_testcase, conf)

    try do
      apply(mod, endFunc, [func, conf])
    catch
      other ->
        comment0 =
          case read_comment() do
            '' ->
              ''

            cmt ->
              cmt ++ :test_server_ctrl.xhtml('<br>', '<br />')
          end

        set_loc(__STACKTRACE__)

        comment(
          :io_lib.format('~ts<font color="red">WARNING: ~w thrown!</font>\n', [comment0, endFunc])
        )

        print_end_tc_warning(endFunc, other, 'thrown', get_loc())
        {:failed, {mod, :end_per_testcase, other}}

      class, reason ->
        set_loc(__STACKTRACE__)

        why =
          case class do
            :exit ->
              {:EXIT, reason}

            :error ->
              {:EXIT, {reason, __STACKTRACE__}}
          end

        comment0 =
          case read_comment() do
            '' ->
              ''

            cmt ->
              cmt ++ :test_server_ctrl.xhtml('<br>', '<br />')
          end

        comment(
          :io_lib.format('~ts<font color="red">WARNING: ~w crashed!</font>\n', [comment0, endFunc])
        )

        print_end_tc_warning(endFunc, reason, 'crashed', get_loc())
        {:failed, {mod, :end_per_testcase, why}}
    else
      {:save_config, _} = saveCfg ->
        saveCfg

      {:fail, _} = fail ->
        fail

      _ ->
        :ok
    end
  end

  defp print_end_tc_warning(endFunc, reason, cause, loc) do
    formattedLoc = :test_server_sup.format_loc(loc)

    str2Print = fn
      noHTML
      when noHTML == :stdout or
             noHTML == :major ->
        :io_lib.format('WARNING: ~w ~s!\nReason: ~tp\nLine: ~tp\n', [endFunc, cause, reason, loc])

      :minor ->
        reasonStr = :test_server_ctrl.escape_chars(reason)

        :io_lib.format(
          'WARNING: ~w ~s!\nReason: ~ts\nLine: ~ts\n',
          [endFunc, cause, reasonStr, formattedLoc]
        )
    end

    send(:erlang.group_leader(), {:printout, 12, str2Print})
    :ok
  end

  defp get_loc() do
    :erlang.get(:test_server_loc)
  end

  def get_loc(pid) do
    [{:current_stacktrace, stk0}, {:dictionary, dict}] =
      :erlang.process_info(
        pid,
        [:current_stacktrace, :dictionary]
      )

    :lists.foreach(
      fn {key, val} ->
        :erlang.put(key, val)
      end,
      dict
    )

    stk =
      for loc <- stk0 do
        rewrite_loc_item(loc)
      end

    case :erlang.get(:test_server_loc) do
      [{suite, case__}] ->
        case (for {s, c, _L} <- stk, s == suite, c == case__ do
                :match
              end) do
          [:match | _] ->
            :erlang.put(:test_server_loc, stk)

          _ ->
            {preTC, postTC} =
              :lists.splitwith(
                fn
                  {:test_server, :ts_tc, _} ->
                    false

                  _ ->
                    true
                end,
                stk
              )

            cond do
              postTC == [] ->
                :ok

              true ->
                :erlang.put(
                  :test_server_loc,
                  preTC ++ [{suite, case__, :last_expr} | postTC]
                )
            end
        end

      _ ->
        :erlang.put(:test_server_loc, stk)
    end

    get_loc()
  end

  defp fw_error_notify(mod, func, args, error) do
    :test_server_sup.framework_call(
      :error_notification,
      [mod, func, [args], {error, :unknown}]
    )
  end

  defp fw_error_notify(mod, func, args, error, loc) do
    :test_server_sup.framework_call(
      :error_notification,
      [mod, func, [args], {error, loc}]
    )
  end

  defp print(detail, format, args, printer) do
    :test_server_ctrl.print(detail, format, args, printer)
  end

  defp print_timestamp(detail, leader) do
    :test_server_ctrl.print_timestamp(detail, leader)
  end

  def lookup_config(key, config) do
    case :lists.keysearch(key, 1, config) do
      {:value, {^key, val}} ->
        val

      _ ->
        :io.format('Could not find element ~tp in Config.~n', [key])
        :undefined
    end
  end

  defp ts_tc(m, f, a) do
    before = :erlang.monotonic_time()

    result =
      try do
        apply(m, f, a)
      catch
        {:skip, reason} ->
          {:skip, reason}

        {:skipped, reason} ->
          {:skip, reason}

        :exit, {:skip, reason} ->
          {:skip, reason}

        :exit, {:skipped, reason} ->
          {:skip, reason}

        type, reason ->
          set_loc(__STACKTRACE__)

          case type do
            :throw ->
              {:failed, {:thrown, reason}}

            :error ->
              {:EXIT, {reason, __STACKTRACE__}}

            :exit ->
              {:EXIT, reason}
          end
      end

    after__ = :erlang.monotonic_time()
    elapsed = :erlang.convert_time_unit(after__ - before, :native, :micro_seconds)
    {elapsed, result}
  end

  defp set_loc(stk) do
    loc =
      case (for {_, _, _, _} = i <- stk do
              rewrite_loc_item(i)
            end) do
        [{m, f, 0} | stack] ->
          [{m, f} | stack]

        other ->
          other
      end

    :erlang.put(:test_server_loc, loc)
  end

  defp rewrite_loc_item({m, f, _, loc}) do
    {m, f, :proplists.get_value(:line, loc, 0)}
  end

  def format(format) do
    format(:minor, format, [])
  end

  def format(:major, format) do
    format(:major, format, [])
  end

  def format(:minor, format) do
    format(:minor, format, [])
  end

  def format(detail, format) when is_integer(detail) do
    format(detail, format, [])
  end

  def format(format, args) do
    format(:minor, format, args)
  end

  def format(detail, format, args) do
    str =
      case (try do
              :io_lib.format(format, args)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:EXIT, _} ->
          :io_lib.format('illegal format; ~tp with args ~tp.\n', [format, args])

        valid ->
          valid
      end

    log({detail, str})
  end

  defp log(msg) do
    send(:erlang.group_leader(), {:structured_io, self(), msg})
    :ok
  end

  def capture_start() do
    send(:erlang.group_leader(), {:capture, self()})
    :ok
  end

  def capture_stop() do
    send(:erlang.group_leader(), {:capture, false})
    :ok
  end

  def capture_get() do
    :test_server_sup.capture_get([])
  end

  def messages_get() do
    :test_server_sup.messages_get([])
  end

  def permit_io(groupLeader, fromPid) do
    send(groupLeader, {:permit_io, fromPid})
    :ok
  end

  def sleep(:infinity) do
    receive do
    after
      :infinity ->
        :ok
    end
  end

  def sleep(mSecs) do
    receive do
    after
      trunc(mSecs) ->
        :ok
    end

    :ok
  end

  def adjusted_sleep(:infinity) do
    receive do
    after
      :infinity ->
        :ok
    end
  end

  def adjusted_sleep(mSecs) do
    {multiplier, scaleFactor} =
      case :test_server_ctrl.get_timetrap_parameters() do
        {:undefined, :undefined} ->
          {1, 1}

        {:undefined, false} ->
          {1, 1}

        {:undefined, true} ->
          {1, timetrap_scale_factor()}

        {:infinity, _} ->
          {:infinity, 1}

        {mult, :undefined} ->
          {mult, 1}

        {mult, false} ->
          {mult, 1}

        {mult, true} ->
          {mult, timetrap_scale_factor()}
      end

    receive do
    after
      trunc(mSecs * multiplier * scaleFactor) ->
        :ok
    end

    :ok
  end

  def fail(reason) do
    comment(cast_to_list(reason))

    try do
      exit({:suite_failed, reason})
    catch
      class, r ->
        case __STACKTRACE__ do
          [{:test_server, :fail, 1, _} | stk] ->
            :ok

          stk ->
            :ok
        end

        :erlang.raise(class, r, stk)
    end
  end

  defp cast_to_list(x) when is_list(x) do
    x
  end

  defp cast_to_list(x) when is_atom(x) do
    :erlang.atom_to_list(x)
  end

  defp cast_to_list(x) do
    :lists.flatten(:io_lib.format('~tp', [x]))
  end

  def fail() do
    try do
      exit(:suite_failed)
    catch
      class, r ->
        case __STACKTRACE__ do
          [{:test_server, :fail, 0, _} | stk] ->
            :ok

          stk ->
            :ok
        end

        :erlang.raise(class, r, stk)
    end
  end

  def break(comment) do
    break(:test_server, comment)
  end

  def break(cBM, comment) do
    break(cBM, :"", comment)
  end

  def break(cBM, testCase, comment) do
    timetrap_cancel()

    {tCName, cntArg, pName} =
      cond do
        testCase == :"" ->
          {'', '', :test_server_break_process}

        true ->
          str = :erlang.atom_to_list(testCase)
          {[32 | str], str, :erlang.list_to_atom('test_server_break_process_' ++ str)}
      end

    :io.format(
      :user,
      '\n\n\n--- SEMIAUTOMATIC TESTING ---\nThe test case~ts executes on process ~w\n\n\n~ts\n\n\n-----------------------------\n\nContinue with --> ~w:continue(~ts).\n',
      [tCName, self(), comment, cBM, cntArg]
    )

    case :erlang.whereis(pName) do
      :undefined ->
        spawn_break_process(self(), pName)

      oldBreakProcess ->
        send(oldBreakProcess, :cancel)
        spawn_break_process(self(), pName)
    end

    receive do
      :continue ->
        :ok
    end
  end

  defp spawn_break_process(pid, pName) do
    spawn(fn ->
      :erlang.register(pName, self())
      :ct_util.mark_process()

      receive do
        :continue ->
          continue(pid)

        :cancel ->
          :ok
      end
    end)
  end

  def continue() do
    case :erlang.whereis(:test_server_break_process) do
      :undefined ->
        :ok

      breakProcess ->
        send(breakProcess, :continue)
    end
  end

  def continue(testCase) when is_atom(testCase) do
    pName = :erlang.list_to_atom('test_server_break_process_' ++ :erlang.atom_to_list(testCase))

    case :erlang.whereis(pName) do
      :undefined ->
        :ok

      breakProcess ->
        send(breakProcess, :continue)
    end
  end

  def continue(pid) when is_pid(pid) do
    send(pid, :continue)
  end

  def timetrap_scale_factor() do
    timetrap_scale_factor([
      {2,
       fn ->
         has_lock_checking()
       end},
      {3,
       fn ->
         has_superfluous_schedulers()
       end},
      {6,
       fn ->
         is_debug()
       end},
      {10,
       fn ->
         is_cover()
       end},
      {10,
       fn ->
         is_valgrind()
       end}
    ])
  end

  defp timetrap_scale_factor(scales) do
    :lists.foldl(
      fn s, o ->
        o * s
      end,
      1,
      for {s, f} <- scales, f.() do
        s
      end
    )
  end

  def timetrap(timeout) do
    multAndScale =
      case :erlang.get(:test_server_multiply_timetraps) do
        :undefined ->
          {fn t ->
             t
           end, true}

        {:undefined, false} ->
          {fn t ->
             t
           end, false}

        {:undefined, _} ->
          {fn t ->
             t
           end, true}

        {:infinity, _} ->
          {fn _ ->
             :infinity
           end, false}

        {int, scale} ->
          {fn
             :infinity ->
               :infinity

             t ->
               t * int
           end, scale}
      end

    timetrap(timeout, timeout, self(), multAndScale)
  end

  defp timetrap(timeout, tCPid, multAndScale) do
    timetrap(timeout, timeout, tCPid, multAndScale)
  end

  defp timetrap(timeout0, timeToReport0, tCPid, multAndScale = {multiplier, scale}) do
    timeout = time_ms(timeout0, tCPid, multAndScale)
    timeout1 = multiplier.(timeout)

    timeToReport =
      cond do
        timeout0 == timeToReport0 ->
          timeout1

        true ->
          time_ms_check(timeToReport0)
      end

    cancel_default_timetrap(self() == tCPid)

    handle =
      case timeout1 do
        :infinity ->
          :infinity

        _ ->
          spawn_link(:test_server_sup, :timetrap, [timeout1, timeToReport, scale, tCPid])
      end

    case :erlang.get(:test_server_timetraps) do
      :undefined ->
        :erlang.put(
          :test_server_timetraps,
          [{handle, tCPid, {timeToReport, scale}}]
        )

      list ->
        list1 =
          :lists.delete(
            {:infinity, tCPid, {:infinity, false}},
            list
          )

        :erlang.put(
          :test_server_timetraps,
          [{handle, tCPid, {timeToReport, scale}} | list1]
        )
    end

    handle
  end

  defp ensure_timetrap(config) do
    case :erlang.get(:test_server_timetraps) do
      [_ | _] ->
        :ok

      _ ->
        case :erlang.get(:test_server_default_timetrap) do
          :undefined ->
            :ok

          garbage ->
            :erlang.erase(:test_server_default_timetrap)
            format('=== WARNING: garbage in test_server_default_timetrap: ~tp~n', [garbage])
        end

        dTmo =
          case :lists.keysearch(:default_timeout, 1, config) do
            {:value, {:default_timeout, tmo}} ->
              tmo

            _ ->
              60
          end

        format('=== test_server setting default timetrap of ~p seconds~n', [dTmo])

        :erlang.put(
          :test_server_default_timetrap,
          timetrap(seconds(dTmo))
        )
    end
  end

  defp cancel_default_timetrap(false) do
    :ok
  end

  defp cancel_default_timetrap(true) do
    case :erlang.get(:test_server_default_timetrap) do
      :undefined ->
        :ok

      timeTrap when is_pid(timeTrap) ->
        timetrap_cancel(timeTrap)
        :erlang.erase(:test_server_default_timetrap)
        format('=== test_server canceled default timetrap since another timetrap was set~n')
        :ok

      garbage ->
        :erlang.erase(:test_server_default_timetrap)
        format('=== WARNING: garbage in test_server_default_timetrap: ~tp~n', [garbage])
        :error
    end
  end

  defp time_ms({:hours, n}, _, _) do
    hours(n)
  end

  defp time_ms({:minutes, n}, _, _) do
    minutes(n)
  end

  defp time_ms({:seconds, n}, _, _) do
    seconds(n)
  end

  defp time_ms({other, _N}, _, _) do
    format(
      '=== ERROR: Invalid time specification: ~tp. Should be seconds, minutes, or hours.~n',
      [other]
    )

    exit({:invalid_time_format, other})
  end

  defp time_ms(ms, _, _) when is_integer(ms) do
    ms
  end

  defp time_ms(:infinity, _, _) do
    :infinity
  end

  defp time_ms(fun, tCPid, multAndScale)
       when is_function(fun) do
    time_ms_apply(fun, tCPid, multAndScale)
  end

  defp time_ms({m, f, a} = mFA, tCPid, multAndScale)
       when is_atom(m) and is_atom(f) and is_list(a) do
    time_ms_apply(mFA, tCPid, multAndScale)
  end

  defp time_ms(other, _, _) do
    exit({:invalid_time_format, other})
  end

  defp time_ms_check(mFA = {m, f, a})
       when is_atom(m) and
              is_atom(f) and is_list(a) do
    mFA
  end

  defp time_ms_check(fun) when is_function(fun) do
    fun
  end

  defp time_ms_check(other) do
    time_ms(other, :undefined, :undefined)
  end

  defp time_ms_apply(func, tCPid, multAndScale) do
    {_, gL} = :erlang.process_info(tCPid, :group_leader)
    whoAmI = self()
    t0 = :erlang.monotonic_time()

    userTTSup =
      spawn(fn ->
        user_timetrap_supervisor(func, whoAmI, tCPid, gL, t0, multAndScale)
      end)

    receive do
      {^userTTSup, :infinity} ->
        save_user_timetrap(tCPid, userTTSup, t0)
        timetrap(:infinity, tCPid, multAndScale)
    after
      5000 ->
        :erlang.exit(userTTSup, :kill)

        cond do
          whoAmI != gL ->
            exit({:user_timetrap_error, :time_ms_apply})

          true ->
            format('=== ERROR: User timetrap execution failed!', [])
            :ignore
        end
    end
  end

  defp user_timetrap_supervisor(func, spawner, tCPid, gL, t0, multAndScale) do
    :erlang.process_flag(:trap_exit, true)
    :ct_util.mark_process()
    send(spawner, {self(), :infinity})
    monRef = :erlang.monitor(:process, tCPid)
    userTTSup = self()
    :erlang.group_leader(gL, userTTSup)

    userTT =
      spawn_link(fn ->
        call_user_timetrap(func, userTTSup)
      end)

    receive do
      {^userTT, result} ->
        :erlang.demonitor(monRef, [:flush])
        t1 = :erlang.monotonic_time()
        elapsed = :erlang.convert_time_unit(t1 - t0, :native, :milli_seconds)

        try do
          time_ms_check(result)
        catch
          _, _ ->
            send(gL, {:user_timetrap, tCPid, 0, t0, elapsed, multAndScale})
        else
          timeVal ->
            send(gL, {:user_timetrap, tCPid, timeVal, t0, elapsed, multAndScale})
        end

      {:EXIT, ^userTT, error} when error != :normal ->
        :erlang.demonitor(monRef, [:flush])
        send(gL, {:user_timetrap, tCPid, 0, t0, {:user_timetrap_error, error}, multAndScale})

      {:DOWN, ^monRef, _, _, _} ->
        :erlang.demonitor(monRef, [:flush])
        :erlang.exit(userTT, :kill)
    end
  end

  defp call_user_timetrap(func, sup) when is_function(func) do
    try do
      func.()
    catch
      _, error ->
        exit({error, __STACKTRACE__})
    else
      result ->
        send(sup, {self(), result})
    end
  end

  defp call_user_timetrap({m, f, a}, sup) do
    try do
      apply(m, f, a)
    catch
      _, error ->
        exit({error, __STACKTRACE__})
    else
      result ->
        send(sup, {self(), result})
    end
  end

  defp save_user_timetrap(tCPid, userTTSup, startTime) do
    newUserTT = {tCPid, {userTTSup, startTime}}

    case :erlang.get(:test_server_user_timetrap) do
      :undefined ->
        :erlang.put(:test_server_user_timetrap, [newUserTT])

      userTTSups ->
        case :proplists.get_value(tCPid, userTTSups) do
          :undefined ->
            :erlang.put(
              :test_server_user_timetrap,
              [newUserTT | userTTSups]
            )

          prevTTSup ->
            remove_user_timetrap(prevTTSup)

            :erlang.put(
              :test_server_user_timetrap,
              [newUserTT | :proplists.delete(tCPid, userTTSups)]
            )
        end
    end
  end

  defp update_user_timetraps(tCPid, startTime) do
    case :erlang.get(:test_server_user_timetrap) do
      :undefined ->
        :proceed

      userTTs ->
        case :proplists.get_value(tCPid, userTTs) do
          {_UserTTSup, ^startTime} ->
            :erlang.put(
              :test_server_user_timetrap,
              :proplists.delete(tCPid, userTTs)
            )

            :proceed

          {otherUserTTSup, otherStartTime} ->
            case otherStartTime - startTime do
              diff when diff >= 0 ->
                :ignore

              _ ->
                :erlang.exit(otherUserTTSup, :kill)

                :erlang.put(
                  :test_server_user_timetrap,
                  :proplists.delete(tCPid, userTTs)
                )

                :proceed
            end

          :undefined ->
            :proceed
        end
    end
  end

  defp remove_user_timetrap(tTSup) do
    :erlang.exit(tTSup, :kill)
  end

  def timetrap_cancel(handle) do
    timetrap_cancel_one(handle, true)
  end

  defp timetrap_cancel_one(:infinity, _SendToServer) do
    :ok
  end

  defp timetrap_cancel_one(handle, sendToServer) do
    case :erlang.get(:test_server_timetraps) do
      :undefined ->
        :ok

      [{^handle, _, _}] ->
        :erlang.erase(:test_server_timetraps)

      timers ->
        case :lists.keysearch(handle, 1, timers) do
          {:value, _} ->
            :erlang.put(
              :test_server_timetraps,
              :lists.keydelete(handle, 1, timers)
            )

          false when sendToServer == true ->
            send(:erlang.group_leader(), {:timetrap_cancel_one, handle, self()})

          false ->
            :ok
        end
    end

    :test_server_sup.timetrap_cancel(handle)
  end

  def timetrap_cancel() do
    timetrap_cancel_all(self(), true)
  end

  defp timetrap_cancel_all(tCPid, sendToServer) do
    case :erlang.get(:test_server_timetraps) do
      :undefined ->
        :ok

      timers ->
        for {handle, pid, _} <- timers, pid == tCPid do
          timetrap_cancel_one(handle, false)
        end

        :ok
    end

    case :erlang.get(:test_server_user_timetrap) do
      :undefined ->
        :ok

      userTTs ->
        case :proplists.get_value(tCPid, userTTs) do
          {userTTSup, _StartTime} ->
            remove_user_timetrap(userTTSup)

            :erlang.put(
              :test_server_user_timetrap,
              :proplists.delete(tCPid, userTTs)
            )

            :ok

          :undefined ->
            :ok
        end
    end

    cond do
      sendToServer == true ->
        send(:erlang.group_leader(), {:timetrap_cancel_all, tCPid, self()})
        :ok

      true ->
        :ok
    end

    :ok
  end

  def get_timetrap_info() do
    get_timetrap_info(self(), true)
  end

  defp get_timetrap_info(tCPid, sendToServer) do
    case :erlang.get(:test_server_timetraps) do
      :undefined ->
        :undefined

      timers ->
        case (for {handle, pid, info} <- timers, pid == tCPid, handle != :infinity do
                info
              end) do
          [{tVal, true} | _] ->
            {tVal, {true, :test_server.timetrap_scale_factor()}}

          [{tVal, false} | _] ->
            {tVal, {false, 1}}

          [] when sendToServer == true ->
            case tc_supervisor_req({:get_timetrap_info, tCPid}) do
              {tVal, true} ->
                {tVal, {true, :test_server.timetrap_scale_factor()}}

              {tVal, false} ->
                {tVal, {false, 1}}

              error ->
                error
            end

          [] ->
            :undefined
        end
    end
  end

  def hours(n) do
    trunc(n * 1000 * 60 * 60)
  end

  def minutes(n) do
    trunc(n * 1000 * 60)
  end

  def seconds(n) do
    trunc(n * 1000)
  end

  defp tc_supervisor_req(tag) do
    pid = :test_server_gl.get_tc_supervisor(:erlang.group_leader())
    send(pid, {tag, self()})

    receive do
      {^pid, ^tag, result} ->
        result
    after
      5000 ->
        :erlang.error(:no_answer_from_tc_supervisor)
    end
  end

  defp tc_supervisor_req(tag, msg) do
    pid = :test_server_gl.get_tc_supervisor(:erlang.group_leader())
    send(pid, {tag, self(), msg})

    receive do
      {^pid, ^tag, result} ->
        result
    after
      5000 ->
        :erlang.error(:no_answer_from_tc_supervisor)
    end
  end

  def timecall(m, f, a) do
    :test_server_sup.timecall(m, f, a)
  end

  def do_times(n, m, f, a) when n > 0 do
    apply(m, f, a)
    do_times(n - 1, m, f, a)
  end

  def do_times(0, _, _, _) do
    :ok
  end

  def do_times(n, fun) when n > 0 do
    fun.()
    do_times(n - 1, fun)
  end

  def do_times(0, _) do
    :ok
  end

  def m_out_of_n(0, _, _) do
    :ok
  end

  def m_out_of_n(m, 0, _) do
    exit({:m_out_of_n_failed, {m, :left_to_do}})
  end

  def m_out_of_n(m, n, fun) do
    case (try do
            fun.()
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        m_out_of_n(m, n - 1, fun)

      _Other ->
        m_out_of_n(m - 1, n - 1, fun)
    end
  end

  def call_crash(m, f, a) do
    call_crash(:infinity, m, f, a)
  end

  def call_crash(time, m, f, a) do
    call_crash(time, :any, m, f, a)
  end

  def call_crash(time, crash, m, f, a) do
    :test_server_sup.call_crash(time, crash, m, f, a)
  end

  def start_node(name, type, options) do
    :lists.foreach(
      fn n ->
        case firstname(n) do
          ^name ->
            format(
              '=== WARNING: Trying to start node \'~w\' when node with same first name exists: ~w',
              [name, n]
            )

          _other ->
            :ok
        end
      end,
      :erlang.nodes()
    )

    send(
      :erlang.group_leader(),
      {:sync_apply, self(), {:test_server_ctrl, :start_node, [name, type, options]}}
    )

    result =
      receive do
        {:sync_result, r} ->
          r
      end

    case result do
      {:ok, node} ->
        cover =
          case is_cover(node) do
            true ->
              :proplists.get_value(:start_cover, options, true)

            false ->
              false
          end

        :net_adm.ping(node)

        case cover do
          true ->
            do_cover_for_node(node, :start)

          _ ->
            :ok
        end

        {:ok, node}

      {:fail, reason} ->
        fail(reason)

      error ->
        error
    end
  end

  defp firstname(n) do
    :erlang.list_to_atom(upto(?@, :erlang.atom_to_list(n)))
  end

  defp upto(h, [h | _T]) do
    []
  end

  defp upto(h, [x | t]) do
    [x | upto(h, t)]
  end

  def wait_for_node(slave) do
    send(
      :erlang.group_leader(),
      {:sync_apply, self(), {:test_server_ctrl, :wait_for_node, [slave]}}
    )

    result =
      receive do
        {:sync_result, r} ->
          r
      end

    case result do
      :ok ->
        :net_adm.ping(slave)

        case is_cover(slave) do
          true ->
            do_cover_for_node(slave, :start)

          _ ->
            :ok
        end

      _ ->
        :ok
    end

    result
  end

  def stop_node(slave) do
    cover = is_cover(slave)

    cond do
      cover ->
        do_cover_for_node(slave, :flush, false)

      true ->
        :ok
    end

    send(:erlang.group_leader(), {:sync_apply, self(), {:test_server_ctrl, :stop_node, [slave]}})

    result =
      receive do
        {:sync_result, r} ->
          r
      end

    case result do
      :ok ->
        :erlang.monitor_node(slave, true)
        :slave.stop(slave)

        receive do
          {:nodedown, ^slave} ->
            format(:minor, 'Stopped slave node: ~w', [slave])
            format(:major, '=node_stop     ~w', [slave])

            cond do
              cover ->
                do_cover_for_node(slave, :stop, false)

              true ->
                :ok
            end

            true
        after
          30000 ->
            format('=== WARNING: Node ~w does not seem to terminate.', [slave])
            :erlang.monitor_node(slave, false)

            receive do
              {:nodedown, ^slave} ->
                :ok
            after
              0 ->
                :ok
            end

            false
        end

      {:error, _Reason} ->
        format(
          '=== WARNING: Attempt to stop a nonexisting slavenode (~w)~n===          Trying to kill it anyway!!!',
          [slave]
        )

        case :net_adm.ping(slave) do
          :pong ->
            :erlang.monitor_node(slave, true)
            :slave.stop(slave)

            receive do
              {:nodedown, ^slave} ->
                format(:minor, 'Stopped slave node: ~w', [slave])
                format(:major, '=node_stop     ~w', [slave])

                cond do
                  cover ->
                    do_cover_for_node(slave, :stop, false)

                  true ->
                    :ok
                end

                true
            after
              30000 ->
                format('=== WARNING: Node ~w does not seem to terminate.', [slave])
                :erlang.monitor_node(slave, false)

                receive do
                  {:nodedown, ^slave} ->
                    :ok
                after
                  0 ->
                    :ok
                end

                false
            end

          :pang ->
            cond do
              cover ->
                do_cover_for_node(slave, :stop, false)

              true ->
                :ok
            end

            false
        end
    end
  end

  def is_release_available(release) do
    send(
      :erlang.group_leader(),
      {:sync_apply, self(), {:test_server_ctrl, :is_release_available, [release]}}
    )

    receive do
      {:sync_result, r} ->
        r
    end
  end

  def run_on_shielded_node(fun, cArgs)
      when is_function(fun) and
             is_list(cArgs) do
    nr = :erlang.unique_integer([:positive])
    name = 'shielded_node-' ++ :erlang.integer_to_list(nr)

    node =
      case start_node(name, :slave, [{:args, '-hidden ' ++ cArgs}]) do
        {:ok, n} ->
          n

        err ->
          fail({:failed_to_start_shielded_node, err})
      end

    master = self()
    ref = make_ref()

    slave =
      :erlang.spawn(
        node,
        start_job_proxy_fun(master, fun)
      )

    mRef = :erlang.monitor(:process, slave)
    send(slave, ref)

    receive do
      {:DOWN, ^mRef, _, _, info} ->
        stop_node(node)
        fail(info)

      {^ref, res} ->
        stop_node(node)

        receive do
          {:DOWN, ^mRef, _, _, _} ->
            res
        end
    end
  end

  defp start_job_proxy_fun(master, fun) do
    fn ->
      :ct_util.mark_process()
      _ = start_job_proxy()

      receive do
        ref ->
          send(master, {ref, fun.()})
          :ok
      end

      receive do
      after
        :infinity ->
          :infinity
      end
    end
  end

  defp is_shielded(name) do
    case {cast_to_list(name), :erlang.atom_to_list(node())} do
      {'shielded_node' ++ _, _} ->
        true

      {_, 'shielded_node' ++ _} ->
        true

      _ ->
        false
    end
  end

  defp same_version(name) do
    thisVersion = :erlang.system_info(:version)
    otherVersion = :rpc.call(name, :erlang, :system_info, [:version])
    thisVersion === otherVersion
  end

  defp is_cover(name) do
    case is_cover() do
      true ->
        not is_shielded(name) and same_version(name)

      false ->
        false
    end
  end

  def temp_name(stem) do
    num = :erlang.unique_integer([:positive])
    randomName = stem ++ :erlang.integer_to_list(num)
    {:ok, files} = :file.list_dir(:filename.dirname(stem))

    case :lists.member(randomName, files) do
      true ->
        temp_name(stem)

      false ->
        randomName
    end
  end

  def app_test(app) do
    app_test(app, :pedantic)
  end

  def app_test(app, mode) do
    :test_server_sup.app_test(app, mode)
  end

  def appup_test(app) do
    :test_server_sup.appup_test(app)
  end

  def is_native(mod) do
    try do
      mod.module_info(:native)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end === true
  end

  def comment(string) do
    send(:erlang.group_leader(), {:comment, string})
    :ok
  end

  defp read_comment() do
    tc_supervisor_req(:read_comment)
  end

  def make_priv_dir() do
    tc_supervisor_req(:make_priv_dir)
  end

  def os_type() do
    :os.type()
  end

  def is_cover() do
    case :erlang.whereis(:cover_server) do
      :undefined ->
        false

      _ ->
        true
    end
  end

  def is_debug() do
    case (try do
            :erlang.system_info(:debug_compiled)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        case :string.find(
               :erlang.system_info(:system_version),
               'debug'
             ) do
          :nomatch ->
            false

          _ ->
            true
        end

      res ->
        res
    end
  end

  defp has_lock_checking() do
    case (try do
            :erlang.system_info(:lock_checking)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        false

      res ->
        res
    end
  end

  defp has_superfluous_schedulers() do
    case (try do
            {:erlang.system_info(:schedulers), :erlang.system_info(:logical_processors)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {s, p} when is_integer(s) and is_integer(p) and s > p ->
        true

      _ ->
        false
    end
  end

  def is_commercial() do
    case :string.find(
           :erlang.system_info(:system_version),
           'source'
         ) do
      :nomatch ->
        true

      _ ->
        false
    end
  end

  def is_valgrind() do
    case (try do
            :erlang.system_info({:valgrind, :running})
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        false

      res ->
        res
    end
  end

  def valgrind_new_leaks() do
    try do
      :erlang.system_info({:valgrind, :memory})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def valgrind_format(format, args) do
    try do
      :erlang.system_info({:valgrind, :io_lib.format(format, args)})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  defp do_sync_apply(proxy, from, {m, f, a}) do
    result = apply(m, f, a)

    cond do
      is_pid(proxy) ->
        send(proxy, {:sync_result_proxy, from, result})
        :ok

      true ->
        send(from, {:sync_result, result})
        :ok
    end
  end

  defp start_cover() do
    case :cover.start() do
      {:error, {:already_started, pid}} ->
        {:ok, pid}

      else__ ->
        else__
    end
  end
end
