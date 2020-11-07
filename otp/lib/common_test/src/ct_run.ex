defmodule :m_ct_run do
  use Bitwise
  require Record
  Record.defrecord(:r_event, :event, name: :undefined, node: :undefined, data: :undefined)

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

  Record.defrecord(:r_opts, :opts,
    label: :undefined,
    profile: :undefined,
    shell: :undefined,
    cover: :undefined,
    cover_stop: :undefined,
    coverspec: :undefined,
    step: :undefined,
    logdir: :undefined,
    logopts: [],
    basic_html: :undefined,
    esc_chars: true,
    verbosity: [],
    config: [],
    event_handlers: [],
    ct_hooks: [],
    enable_builtin_hooks: :undefined,
    include: [],
    auto_compile: :undefined,
    abort_if_missing_suites: :undefined,
    silent_connections: [],
    stylesheet: :undefined,
    multiply_timetraps: :undefined,
    scale_timetraps: :undefined,
    create_priv_dir: :undefined,
    testspec_files: [],
    current_testspec: :undefined,
    tests: :undefined,
    starter: :undefined
  )

  def script_start() do
    :erlang.process_flag(:trap_exit, true)
    init = :init.get_arguments()

    ctArgs =
      :lists.takewhile(
        fn
          {:ct_erl_args, _} ->
            false

          _ ->
            true
        end,
        init
      )

    rel_to_abs(ctArgs)

    args =
      case :application.get_env(
             :common_test,
             :run_test_start_opts
           ) do
        {:ok, envStartOpts} ->
          flagFilter = fn flags ->
            :lists.filter(
              fn
                {:root, _} ->
                  false

                {:progname, _} ->
                  false

                {:home, _} ->
                  false

                {:noshell, _} ->
                  false

                {:noinput, _} ->
                  false

                _ ->
                  true
              end,
              flags
            )
          end

          :io.format(:user, '~n-------------------- START ARGS --------------------~n', [])
          :io.format(:user, '--- Init args:~n~tp~n', [flagFilter.(init)])
          :io.format(:user, '--- CT args:~n~tp~n', [flagFilter.(ctArgs)])
          envArgs = opts2args(envStartOpts)
          :io.format(:user, '--- Env opts -> args:~n~tp~n   =>~n~tp~n', [envStartOpts, envArgs])
          merged = merge_arguments(ctArgs ++ envArgs)
          :io.format(:user, '--- Merged args:~n~tp~n', [flagFilter.(merged)])
          :io.format(:user, '----------------------------------------------------~n~n', [])
          merged

        _ ->
          merge_arguments(ctArgs)
      end

    case :proplists.get_value(:help, args) do
      :undefined ->
        script_start(args)

      _ ->
        script_usage()
    end
  end

  defp script_start(args) do
    tracing = start_trace(args)

    case :ct_repeat.loop_test(:script, args) do
      false ->
        {:ok, cwd} = :file.get_cwd()

        cTVsn =
          case :filename.basename(:code.lib_dir(:common_test)) do
            cTBase when is_list(cTBase) ->
              case :string.lexemes(cTBase, '-') do
                ['common_test', vsn] ->
                  ' v' ++ vsn

                _ ->
                  ''
              end
          end

        :io.format('~nCommon Test~s starting (cwd is ~ts)~n~n', [cTVsn, cwd])
        self = self()

        pid =
          spawn_link(fn ->
            script_start1(self, args)
          end)

        receive do
          {:EXIT, ^pid, reason} ->
            case reason do
              {:user_error, what} ->
                :io.format('\nTest run failed!\nReason: ~tp\n\n\n', [what])
                finish(tracing, 2, args)

              _ ->
                :io.format(
                  'Test run crashed! This could be an internal error - please report!\n\n~tp\n\n\n',
                  [reason]
                )

                finish(tracing, 2, args)
            end

          {^pid, {:error, reason}} ->
            :io.format('\nTest run failed! Reason:\n~tp\n\n\n', [reason])
            finish(tracing, 2, args)

          {^pid, result} ->
            :io.nl()
            finish(tracing, analyze_test_result(result, args), args)
        end

      {:error, _LoopReason} ->
        finish(tracing, 2, args)

      result ->
        :io.nl()
        finish(tracing, analyze_test_result(result, args), args)
    end
  end

  defp analyze_test_result(:ok, _) do
    0
  end

  defp analyze_test_result({:error, _Reason}, _) do
    2
  end

  defp analyze_test_result(
         {_Ok, failed, {_UserSkipped, autoSkipped}},
         args
       ) do
    cond do
      failed > 0 ->
        1

      true ->
        case autoSkipped do
          0 ->
            0

          _ ->
            case get_start_opt(
                   :exit_status,
                   fn [exitOpt] ->
                     exitOpt
                   end,
                   args
                 ) do
              :undefined ->
                1

              'ignore_config' ->
                0
            end
        end
    end
  end

  defp analyze_test_result([result | rs], args) do
    case analyze_test_result(result, args) do
      0 ->
        analyze_test_result(rs, args)

      other ->
        other
    end
  end

  defp analyze_test_result([], _) do
    0
  end

  defp analyze_test_result(:interactive_mode, _) do
    :interactive_mode
  end

  defp analyze_test_result(unknown, _) do
    :io.format('\nTest run failed! Reason:\n~tp\n\n\n', [unknown])
    2
  end

  defp finish(tracing, exitStatus, args) do
    stop_trace(tracing)
    :timer.sleep(1000)

    cond do
      exitStatus == :interactive_mode ->
        :interactive_mode

      true ->
        case get_start_opt(
               :halt_with,
               fn [haltMod, haltFunc] ->
                 {:erlang.list_to_atom(haltMod), :erlang.list_to_atom(haltFunc)}
               end,
               args
             ) do
          :undefined ->
            :erlang.halt(exitStatus)

          {m, f} ->
            apply(m, f, [exitStatus])
        end
    end
  end

  def script_start1(parent, args) do
    :ct_util.mark_process()

    label =
      get_start_opt(
        :label,
        fn [lbl] ->
          lbl
        end,
        args
      )

    profile =
      get_start_opt(
        :profile,
        fn [prof] ->
          prof
        end,
        args
      )

    shell = get_start_opt(:shell, true, args)

    cover =
      get_start_opt(
        :cover,
        fn [coverFile] ->
          :filename.absname(coverFile)
        end,
        args
      )

    coverStop =
      get_start_opt(
        :cover_stop,
        fn [cS] ->
          :erlang.list_to_atom(cS)
        end,
        args
      )

    logDir =
      get_start_opt(
        :logdir,
        fn [logD] ->
          logD
        end,
        args
      )

    logOpts =
      get_start_opt(
        :logopts,
        fn os ->
          for o <- os do
            :erlang.list_to_atom(o)
          end
        end,
        [],
        args
      )

    verbosity = verbosity_args2opts(args)

    multTT =
      get_start_opt(
        :multiply_timetraps,
        fn [mT] ->
          :erlang.list_to_integer(mT)
        end,
        args
      )

    scaleTT =
      get_start_opt(
        :scale_timetraps,
        fn
          [cT] ->
            :erlang.list_to_atom(cT)

          [] ->
            true
        end,
        args
      )

    createPrivDir =
      get_start_opt(
        :create_priv_dir,
        fn
          [pD] ->
            :erlang.list_to_atom(pD)

          [] ->
            :auto_per_tc
        end,
        args
      )

    evHandlers = event_handler_args2opts(args)
    cTHooks = ct_hooks_args2opts(args)

    enableBuiltinHooks =
      get_start_opt(
        :enable_builtin_hooks,
        fn
          [cT] ->
            :erlang.list_to_atom(cT)

          [] ->
            :undefined
        end,
        :undefined,
        args
      )

    case :proplists.get_value(:ct_decrypt_key, args) do
      [decryptKey] ->
        :application.set_env(:common_test, :decrypt, {:key, decryptKey})

      :undefined ->
        case :proplists.get_value(:ct_decrypt_file, args) do
          [decryptFile] ->
            :application.set_env(:common_test, :decrypt, {:file, :filename.absname(decryptFile)})

          :undefined ->
            :application.unset_env(:common_test, :decrypt)
        end
    end

    {autoCompile, includeDirs} =
      case :proplists.get_value(
             :no_auto_compile,
             args
           ) do
        :undefined ->
          :application.set_env(:common_test, :auto_compile, true)

          inclDirs =
            case :proplists.get_value(
                   :include,
                   args
                 ) do
              incls when is_list(hd(incls)) ->
                for iDir <- incls do
                  :filename.absname(iDir)
                end

              incl when is_list(incl) ->
                [:filename.absname(incl)]

              :undefined ->
                []
            end

          case :os.getenv('CT_INCLUDE_PATH') do
            false ->
              :application.set_env(:common_test, :include, inclDirs)
              {:undefined, inclDirs}

            ctInclPath ->
              allInclDirs =
                :string.lexemes(
                  ctInclPath,
                  [?:, ?\s, ?,]
                ) ++ inclDirs

              :application.set_env(:common_test, :include, allInclDirs)
              {:undefined, allInclDirs}
          end

        _ ->
          :application.set_env(:common_test, :auto_compile, false)
          {false, []}
      end

    abortIfMissing =
      get_start_opt(
        :abort_if_missing_suites,
        fn
          [] ->
            true

          [bool] ->
            :erlang.list_to_atom(bool)
        end,
        false,
        args
      )

    silentConns =
      get_start_opt(
        :silent_connections,
        fn
          ['all'] ->
            [:all]

          conns ->
            for conn <- conns do
              :erlang.list_to_atom(conn)
            end
        end,
        [],
        args
      )

    stylesheet =
      get_start_opt(
        :stylesheet,
        fn [sS] ->
          :filename.absname(sS)
        end,
        args
      )

    basicHtml =
      case :proplists.get_value(
             :basic_html,
             args
           ) do
        :undefined ->
          :application.set_env(:common_test, :basic_html, false)
          :undefined

        _ ->
          :application.set_env(:common_test, :basic_html, true)
          true
      end

    escChars =
      case :proplists.get_value(
             :no_esc_chars,
             args
           ) do
        :undefined ->
          :application.set_env(:common_test, :esc_chars, true)
          :undefined

        _ ->
          :application.set_env(:common_test, :esc_chars, false)
          false
      end

    case :proplists.get_value(:disable_log_cache, args) do
      :undefined ->
        :application.set_env(:common_test, :disable_log_cache, false)

      _ ->
        :application.set_env(:common_test, :disable_log_cache, true)
    end

    keepLogs = get_start_opt(:keep_logs, &:ct_logs.parse_keep_logs/1, :all, args)
    :application.set_env(:common_test, :keep_logs, keepLogs)

    opts =
      r_opts(
        label: label,
        profile: profile,
        shell: shell,
        cover: cover,
        cover_stop: coverStop,
        logdir: logDir,
        logopts: logOpts,
        basic_html: basicHtml,
        esc_chars: escChars,
        verbosity: verbosity,
        event_handlers: evHandlers,
        ct_hooks: cTHooks,
        enable_builtin_hooks: enableBuiltinHooks,
        auto_compile: autoCompile,
        abort_if_missing_suites: abortIfMissing,
        include: includeDirs,
        silent_connections: silentConns,
        stylesheet: stylesheet,
        multiply_timetraps: multTT,
        scale_timetraps: scaleTT,
        create_priv_dir: createPrivDir,
        starter: :script
      )

    result = run_or_refresh(opts, args)
    send(parent, {self(), result})
  end

  defp run_or_refresh(opts = r_opts(logdir: logDir), args) do
    case :proplists.get_value(:refresh_logs, args) do
      :undefined ->
        script_start2(opts, args)

      refresh ->
        logDir1 =
          case refresh do
            [] ->
              which(:logdir, logDir)

            [refreshDir] ->
              :filename.absname(refreshDir)
          end

        {:ok, cwd} = :file.get_cwd()
        :ok = :file.set_cwd(logDir1)
        :timer.sleep(500)
        :io.nl()

        case (try do
                :ct_logs.make_all_runs_index(:refresh)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, aRReason} ->
            :ok = :file.set_cwd(cwd)
            {:error, {:all_runs_index, aRReason}}

          _ ->
            case (try do
                    :ct_logs.make_all_suites_index(:refresh)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:EXIT, aSReason} ->
                :ok = :file.set_cwd(cwd)
                {:error, {:all_suites_index, aSReason}}

              _ ->
                :ok = :file.set_cwd(cwd)
                :io.format('Logs in ~ts refreshed!~n~n', [logDir1])
                :timer.sleep(500)
                :ok
            end
        end
    end
  end

  defp script_start2(opts = r_opts(shell: :undefined), args) do
    case :proplists.get_value(:spec, args) do
      specs when specs !== [] and specs !== :undefined ->
        specs1 = get_start_opt(:join_specs, [specs], specs, args)
        relaxed = get_start_opt(:allow_user_terms, true, false, args)

        try do
          :ct_testspec.collect_tests_from_file(specs1, relaxed)
        catch
          {:error, reason} ->
            {:error, {:invalid_testspec, {reason, __STACKTRACE__}}}

          _, reason ->
            {:error, {:invalid_testspec, {reason, __STACKTRACE__}}}
        else
          testSpecData ->
            execute_all_specs(testSpecData, opts, args, [])
        end

      [] ->
        {:error, :no_testspec_specified}

      _ ->
        initConfig = :ct_config.prepare_config_list(args)
        theLogDir = which(:logdir, r_opts(opts, :logdir))

        case check_and_install_configfiles(initConfig, theLogDir, opts) do
          :ok ->
            script_start3(
              r_opts(opts,
                config: initConfig,
                logdir: theLogDir
              ),
              args
            )

          error ->
            error
        end
    end
  end

  defp script_start2(opts, args) do
    initConfig = :ct_config.prepare_config_list(args)
    logDir = which(:logdir, r_opts(opts, :logdir))

    case check_and_install_configfiles(initConfig, logDir, opts) do
      :ok ->
        script_start3(
          r_opts(opts,
            config: initConfig,
            logdir: logDir
          ),
          args
        )

      error ->
        error
    end
  end

  defp execute_all_specs([], _, _, result) do
    result1 = :lists.reverse(result)

    case :lists.keysearch(:EXIT, 1, result1) do
      {:value, {_, _, exitReason}} ->
        exit(exitReason)

      false ->
        case :lists.keysearch(:error, 1, result1) do
          {:value, error} ->
            error

          false ->
            :lists.foldl(
              fn {ok, fail, {userSkip, autoSkip}}, {ok1, fail1, {userSkip1, autoSkip1}} ->
                {ok1 + ok, fail1 + fail, {userSkip1 + userSkip, autoSkip1 + autoSkip}}
              end,
              {0, 0, {0, 0}},
              result1
            )
        end
    end
  end

  defp execute_all_specs([{specs, tS} | tSs], opts, args, result) do
    combinedOpts = combine_test_opts(tS, specs, opts)

    try do
      execute_one_spec(tS, combinedOpts, args)
    catch
      _, exitReason ->
        execute_all_specs(tSs, opts, args, [{:EXIT, self(), exitReason} | result])
    else
      execResult ->
        execute_all_specs(tSs, opts, args, [execResult | result])
    end
  end

  defp execute_one_spec(tS, opts, args) do
    initConfig = :ct_config.prepare_config_list(args)
    theLogDir = which(:logdir, r_opts(opts, :logdir))
    allConfig = merge_vals([initConfig, r_opts(opts, :config)])

    case check_and_install_configfiles(allConfig, theLogDir, opts) do
      :ok ->
        {run, skip} = :ct_testspec.prepare_tests(tS, node())

        result =
          do_run(
            run,
            skip,
            r_opts(opts, config: allConfig, logdir: theLogDir, current_testspec: tS),
            args
          )

        :ct_util.delete_testdata(:testspec)
        result

      error ->
        error
    end
  end

  defp combine_test_opts(tS, specs, opts) do
    tSOpts = get_data_for_node(tS, node())
    label = choose_val(r_opts(opts, :label), r_opts(tSOpts, :label))

    profile =
      choose_val(
        r_opts(opts, :profile),
        r_opts(tSOpts, :profile)
      )

    logDir =
      choose_val(
        r_opts(opts, :logdir),
        r_opts(tSOpts, :logdir)
      )

    allLogOpts = merge_vals([r_opts(opts, :logopts), r_opts(tSOpts, :logopts)])
    allVerbosity = merge_keyvals([r_opts(opts, :verbosity), r_opts(tSOpts, :verbosity)])

    allSilentConns =
      merge_vals([r_opts(opts, :silent_connections), r_opts(tSOpts, :silent_connections)])

    cover = choose_val(r_opts(opts, :cover), r_opts(tSOpts, :cover))

    coverStop =
      choose_val(
        r_opts(opts, :cover_stop),
        r_opts(tSOpts, :cover_stop)
      )

    multTT =
      choose_val(
        r_opts(opts, :multiply_timetraps),
        r_opts(tSOpts, :multiply_timetraps)
      )

    scaleTT =
      choose_val(
        r_opts(opts, :scale_timetraps),
        r_opts(tSOpts, :scale_timetraps)
      )

    createPrivDir =
      choose_val(
        r_opts(opts, :create_priv_dir),
        r_opts(tSOpts, :create_priv_dir)
      )

    allEvHs = merge_vals([r_opts(opts, :event_handlers), r_opts(tSOpts, :event_handlers)])
    allCTHooks = merge_vals([r_opts(opts, :ct_hooks), r_opts(tSOpts, :ct_hooks)])

    enableBuiltinHooks =
      choose_val(
        r_opts(opts, :enable_builtin_hooks),
        r_opts(tSOpts, :enable_builtin_hooks)
      )

    stylesheet =
      choose_val(
        r_opts(opts, :stylesheet),
        r_opts(tSOpts, :stylesheet)
      )

    allInclude = merge_vals([r_opts(opts, :include), r_opts(tSOpts, :include)])
    :application.set_env(:common_test, :include, allInclude)

    autoCompile =
      case choose_val(
             r_opts(opts, :auto_compile),
             r_opts(tSOpts, :auto_compile)
           ) do
        :undefined ->
          true

        aCBool ->
          :application.set_env(:common_test, :auto_compile, aCBool)
          aCBool
      end

    abortIfMissing =
      choose_val(
        r_opts(opts, :abort_if_missing_suites),
        r_opts(tSOpts, :abort_if_missing_suites)
      )

    basicHtml =
      case choose_val(
             r_opts(opts, :basic_html),
             r_opts(tSOpts, :basic_html)
           ) do
        :undefined ->
          false

        bHBool ->
          :application.set_env(:common_test, :basic_html, bHBool)
          bHBool
      end

    escChars =
      case choose_val(
             r_opts(opts, :esc_chars),
             r_opts(tSOpts, :esc_chars)
           ) do
        :undefined ->
          true

        eCBool ->
          :application.set_env(:common_test, :esc_chars, eCBool)
          eCBool
      end

    r_opts(opts,
      label: label,
      profile: profile,
      testspec_files: specs,
      cover: cover,
      cover_stop: coverStop,
      logdir: which(:logdir, logDir),
      logopts: allLogOpts,
      basic_html: basicHtml,
      esc_chars: escChars,
      verbosity: allVerbosity,
      silent_connections: allSilentConns,
      config: r_opts(tSOpts, :config),
      event_handlers: allEvHs,
      ct_hooks: allCTHooks,
      enable_builtin_hooks: enableBuiltinHooks,
      stylesheet: stylesheet,
      auto_compile: autoCompile,
      abort_if_missing_suites: abortIfMissing,
      include: allInclude,
      multiply_timetraps: multTT,
      scale_timetraps: scaleTT,
      create_priv_dir: createPrivDir
    )
  end

  defp check_and_install_configfiles(
         configs,
         logDir,
         r_opts(
           event_handlers: evHandlers,
           ct_hooks: cTHooks,
           enable_builtin_hooks: enableBuiltinHooks
         )
       ) do
    case :ct_config.check_config_files(configs) do
      false ->
        install(
          [
            {:config, configs},
            {:event_handler, evHandlers},
            {:ct_hooks, cTHooks},
            {:enable_builtin_hooks, enableBuiltinHooks}
          ],
          logDir
        )

      {:value, {:error, {:nofile, file}}} ->
        {:error, {:cant_read_config_file, file}}

      {:value, {:error, {:wrong_config, message}}} ->
        {:error, {:wrong_config, message}}

      {:value, {:error, {:callback, info}}} ->
        {:error, {:cant_load_callback_module, info}}
    end
  end

  defp script_start3(opts, args) do
    opts1 =
      get_start_opt(
        :step,
        fn step ->
          r_opts(opts, step: step, cover: :undefined)
        end,
        opts,
        args
      )

    case {:proplists.get_value(:dir, args), :proplists.get_value(:suite, args),
          groups_and_cases(
            :proplists.get_value(:group, args),
            :proplists.get_value(:testcase, args)
          )} do
      {_, _, error = {:error, _}} ->
        error

      {_, [], _} ->
        {:error, :no_suite_specified}

      {[], _, _} ->
        {:error, :no_dir_specified}

      {dirs, :undefined, []} when is_list(dirs) ->
        script_start4(r_opts(opts, tests: tests(dirs)), args)

      {:undefined, suites, []} when is_list(suites) ->
        ts =
          tests(
            for s <- suites do
              suite_to_test(s)
            end
          )

        script_start4(r_opts(opts1, tests: ts), args)

      {:undefined, suite, gsAndCs} when is_list(suite) ->
        case (for s <- suite do
                suite_to_test(s)
              end) do
          dirMods = [_] ->
            ts = tests(dirMods, gsAndCs)
            script_start4(r_opts(opts1, tests: ts), args)

          [[_, _] | _] ->
            {:error, :multiple_suites_and_cases}

          _ ->
            {:error, :incorrect_start_options}
        end

      {[[_, _] | _], suite, []} when is_list(suite) ->
        {:error, :multiple_dirs_and_suites}

      {[dir], suite, gsAndCs}
      when is_list(dir) and
             is_list(suite) ->
        case (for s <- suite do
                suite_to_test(dir, s)
              end) do
          dirMods when gsAndCs == [] ->
            ts = tests(dirMods)
            script_start4(r_opts(opts1, tests: ts), args)

          dirMods = [_] when gsAndCs != [] ->
            ts = tests(dirMods, gsAndCs)
            script_start4(r_opts(opts1, tests: ts), args)

          [[_, _] | _] when gsAndCs != [] ->
            {:error, :multiple_suites_and_cases}

          _ ->
            {:error, :incorrect_start_options}
        end

      {:undefined, :undefined, gsAndCs} when gsAndCs != [] ->
        {:error, :incorrect_start_options}

      {:undefined, :undefined, _} ->
        cond do
          r_opts(opts, :shell) ->
            script_start4(r_opts(opts, tests: []), args)

          true ->
            {:ok, dir} = :file.get_cwd()
            :io.format('ct_run -dir ~ts~n~n', [dir])
            script_start4(r_opts(opts, tests: tests([dir])), args)
        end
    end
  end

  defp script_start4(
         r_opts(
           label: label,
           profile: profile,
           shell: true,
           config: config,
           event_handlers: evHandlers,
           ct_hooks: cTHooks,
           logopts: logOpts,
           verbosity: verbosity,
           enable_builtin_hooks: enableBuiltinHooks,
           logdir: logDir,
           testspec_files: specs
         ),
         _Args
       ) do
    :application.set_env(:common_test, :test_label, label)
    :application.set_env(:common_test, :profile, profile)

    cond do
      config == [] ->
        :ok

      true ->
        :io.format('\nInstalling: ~tp\n\n', [config])
    end

    case install([
           {:config, config},
           {:event_handler, evHandlers},
           {:ct_hooks, cTHooks},
           {:enable_builtin_hooks, enableBuiltinHooks}
         ]) do
      :ok ->
        _ = :ct_util.start(:interactive, logDir, add_verbosity_defaults(verbosity))
        :ct_util.set_testdata({:logopts, logOpts})
        log_ts_names(specs)
        :io.nl()
        :interactive_mode

      error ->
        error
    end
  end

  defp script_start4(opts = r_opts(tests: tests), args) do
    do_run(tests, [], opts, args)
  end

  def script_usage() do
    :io.format('\nUsage:\n\n')

    :io.format(
      'Run tests from command line:\n\n\tct_run -dir TestDir1 TestDir2 .. TestDirN |\n\t  [-dir TestDir] -suite Suite1 Suite2 .. SuiteN\n\t   [-group Group1 Group2 .. GroupN] [-case Case1 Case2 .. CaseN]\n\t [-step [config | keep_inactive]]\n\t [-config ConfigFile1 ConfigFile2 .. ConfigFileN]\n\t [-userconfig CallbackModule ConfigFile1 .. ConfigFileN]\n\t [-decrypt_key Key] | [-decrypt_file KeyFile]\n\t [-logdir LogDir]\n\t [-logopts LogOpt1 LogOpt2 .. LogOptN]\n\t [-verbosity GenVLvl | [CategoryVLvl1 .. CategoryVLvlN]]\n\t [-silent_connections [ConnType1 ConnType2 .. ConnTypeN]]\n\t [-stylesheet CSSFile]\n\t [-cover CoverCfgFile]\n\t [-cover_stop Bool]\n\t [-event_handler EvHandler1 EvHandler2 .. EvHandlerN]\n\t [-ct_hooks CTHook1 CTHook2 .. CTHookN]\n\t [-include InclDir1 InclDir2 .. InclDirN]\n\t [-no_auto_compile]\n\t [-abort_if_missing_suites]\n\t [-multiply_timetraps N]\n\t [-scale_timetraps]\n\t [-create_priv_dir auto_per_run | auto_per_tc | manual_per_tc]\n\t [-basic_html]\n\t [-no_esc_chars]\n\t [-repeat N] |\n\t [-duration HHMMSS [-force_stop [skip_rest]]] |\n\t [-until [YYMoMoDD]HHMMSS [-force_stop [skip_rest]]]\n\t [-exit_status ignore_config]\n\t [-help]\n\n'
    )

    :io.format(
      'Run tests using test specification:\n\n\tct_run -spec TestSpec1 TestSpec2 .. TestSpecN\n\t [-config ConfigFile1 ConfigFile2 .. ConfigFileN]\n\t [-decrypt_key Key] | [-decrypt_file KeyFile]\n\t [-logdir LogDir]\n\t [-logopts LogOpt1 LogOpt2 .. LogOptN]\n\t [-verbosity GenVLvl | [CategoryVLvl1 .. CategoryVLvlN]]\n\t [-allow_user_terms]\n\t [-join_specs]\n\t [-silent_connections [ConnType1 ConnType2 .. ConnTypeN]]\n\t [-stylesheet CSSFile]\n\t [-cover CoverCfgFile]\n\t [-cover_stop Bool]\n\t [-event_handler EvHandler1 EvHandler2 .. EvHandlerN]\n\t [-ct_hooks CTHook1 CTHook2 .. CTHookN]\n\t [-include InclDir1 InclDir2 .. InclDirN]\n\t [-no_auto_compile]\n\t [-abort_if_missing_suites]\n\t [-multiply_timetraps N]\n\t [-scale_timetraps]\n\t [-create_priv_dir auto_per_run | auto_per_tc | manual_per_tc]\n\t [-basic_html]\n\t [-no_esc_chars]\n\t [-repeat N] |\n\t [-duration HHMMSS [-force_stop [skip_rest]]] |\n\t [-until [YYMoMoDD]HHMMSS [-force_stop [skip_rest]]]\n\n'
    )

    :io.format(
      'Refresh the HTML index files:\n\n\tct_run -refresh_logs [LogDir] [-logdir LogDir]  [-basic_html]\n\n'
    )

    :io.format(
      'Run CT in interactive mode:\n\n\tct_run -shell\n\t [-config ConfigFile1 ConfigFile2 .. ConfigFileN]\n\t [-decrypt_key Key] | [-decrypt_file KeyFile]\n\n'
    )

    :io.format(
      'Run tests in web based GUI:\n\n\n\t [-config ConfigFile1 ConfigFile2 .. ConfigFileN]\n\t [-decrypt_key Key] | [-decrypt_file KeyFile]\n\t [-dir TestDir1 TestDir2 .. TestDirN] |\n\t [-suite Suite [-case Case]]\n\t [-logopts LogOpt1 LogOpt2 .. LogOptN]\n\t [-verbosity GenVLvl | [CategoryVLvl1 .. CategoryVLvlN]]\n\t [-include InclDir1 InclDir2 .. InclDirN]\n\t [-no_auto_compile]\n\t [-abort_if_missing_suites]\n\t [-multiply_timetraps N]\n\t [-scale_timetraps]\n\t [-create_priv_dir auto_per_run | auto_per_tc | manual_per_tc]\n\t [-basic_html]\n\t [-no_esc_chars]\n\n'
    )
  end

  def install(opts) do
    install(opts, '.')
  end

  def install(opts, logDir) do
    confOpts = :ct_config.add_default_callback(opts)

    case :application.get_env(:common_test, :decrypt) do
      {:ok, _} ->
        :ok

      _ ->
        case :lists.keysearch(:decrypt, 1, opts) do
          {:value, {_, keyOrFile}} ->
            :application.set_env(:common_test, :decrypt, keyOrFile)

          false ->
            :application.unset_env(:common_test, :decrypt)
        end
    end

    case :erlang.whereis(:ct_util_server) do
      :undefined ->
        varFile = variables_file_name(logDir)

        case :file.open(
               varFile,
               [:write, {:encoding, :utf8}]
             ) do
          {:ok, fd} ->
            _ =
              for opt <- confOpts do
                :io.format(fd, '~tp.\n', [opt])
              end

            :ok = :file.close(fd)

          {:error, reason} ->
            :io.format(
              'CT failed to install configuration data. Please verify that the log directory exists and that write permission is set.\n\n',
              []
            )

            {:error, {varFile, reason}}
        end

      _ ->
        :io.format(
          'It is not possible to install CT while running in interactive mode.\nTo exit this mode, run ct:stop_interactive().\nTo enter the interactive mode again, run ct:start_interactive()\n\n',
          []
        )

        {:error, :interactive_mode}
    end
  end

  def variables_file_name(dir) do
    :filename.join(dir, 'variables-' ++ :erlang.atom_to_list(node()))
  end

  def run_test(startOpt) when is_tuple(startOpt) do
    run_test([startOpt])
  end

  def run_test(startOpts) when is_list(startOpts) do
    cTPid = spawn(run_test1_fun(startOpts))
    ref = :erlang.monitor(:process, cTPid)

    receive do
      {:DOWN, ^ref, :process, ^cTPid, {:user_error, error}} ->
        {:error, error}

      {:DOWN, ^ref, :process, ^cTPid, other} ->
        other
    end
  end

  defp run_test1_fun(startOpts) do
    fn ->
      :ct_util.mark_process()
      run_test1(startOpts)
    end
  end

  defp run_test1(startOpts) when is_list(startOpts) do
    case :proplists.get_value(:refresh_logs, startOpts) do
      :undefined ->
        tracing = start_trace(startOpts)
        {:ok, cwd} = :file.get_cwd()
        :io.format('~nCommon Test starting (cwd is ~ts)~n~n', [cwd])

        res =
          case :ct_repeat.loop_test(:func, startOpts) do
            false ->
              case (try do
                      run_test2(startOpts)
                    catch
                      :error, e -> {:EXIT, {e, __STACKTRACE__}}
                      :exit, e -> {:EXIT, e}
                      e -> e
                    end) do
                {:EXIT, reason} ->
                  :ok = :file.set_cwd(cwd)
                  {:error, reason}

                result ->
                  result
              end

            result ->
              result
          end

        stop_trace(tracing)
        exit(res)

      refreshDir ->
        keepLogs = get_start_opt(:keep_logs, &:ct_logs.parse_keep_logs/1, :all, startOpts)
        :application.set_env(:common_test, :keep_logs, keepLogs)
        :ok = refresh_logs(:filename.absname(refreshDir))
        exit(:done)
    end
  end

  def run_test2(startOpts) do
    label =
      get_start_opt(
        :label,
        fn
          lbl when is_list(lbl) ->
            lbl

          lbl when is_atom(lbl) ->
            :erlang.atom_to_list(lbl)
        end,
        startOpts
      )

    profile =
      get_start_opt(
        :profile,
        fn
          prof when is_list(prof) ->
            prof

          prof when is_atom(prof) ->
            :erlang.atom_to_list(prof)
        end,
        startOpts
      )

    logDir =
      get_start_opt(
        :logdir,
        fn lD when is_list(lD) ->
          lD
        end,
        startOpts
      )

    logOpts = get_start_opt(:logopts, :value, [], startOpts)

    verbosity =
      get_start_opt(
        :verbosity,
        fn
          vLvls when is_list(vLvls) ->
            :lists.map(
              fn
                vLvl = {_Cat, _Lvl} ->
                  vLvl

                lvl ->
                  {:"$unspecified", lvl}
              end,
              vLvls
            )

          vLvl when is_integer(vLvl) ->
            [{:"$unspecified", vLvl}]
        end,
        [],
        startOpts
      )

    cfgFiles = :ct_config.get_config_file_list(startOpts)

    evHandlers =
      case :proplists.get_value(
             :event_handler,
             startOpts
           ) do
        :undefined ->
          []

        h when is_atom(h) ->
          [{h, []}]

        h ->
          hs =
            cond do
              is_tuple(h) ->
                [h]

              is_list(h) ->
                h

              true ->
                []
            end

          :lists.flatten(
            :lists.map(
              fn
                eH when is_atom(eH) ->
                  {eH, []}

                {hL, args} when is_list(hL) ->
                  for eH <- hL do
                    {eH, args}
                  end

                {eH, args} when is_atom(eH) ->
                  {eH, args}

                _ ->
                  []
              end,
              hs
            )
          )
      end

    cTHooks = get_start_opt(:ct_hooks, :value, [], startOpts)

    enableBuiltinHooks =
      get_start_opt(
        :enable_builtin_hooks,
        fn eBH when eBH == true or eBH == false ->
          eBH
        end,
        :undefined,
        startOpts
      )

    silentConns =
      get_start_opt(
        :silent_connections,
        fn
          :all ->
            [:all]

          conns ->
            conns
        end,
        [],
        startOpts
      )

    stylesheet =
      get_start_opt(
        :stylesheet,
        fn sS ->
          :filename.absname(sS)
        end,
        startOpts
      )

    cover =
      get_start_opt(
        :cover,
        fn coverFile ->
          :filename.absname(coverFile)
        end,
        startOpts
      )

    coverStop = get_start_opt(:cover_stop, :value, startOpts)
    multiplyTT = get_start_opt(:multiply_timetraps, :value, startOpts)
    scaleTT = get_start_opt(:scale_timetraps, :value, startOpts)
    createPrivDir = get_start_opt(:create_priv_dir, :value, startOpts)

    {autoCompile, include} =
      case :proplists.get_value(
             :auto_compile,
             startOpts
           ) do
        :undefined ->
          :application.set_env(:common_test, :auto_compile, true)

          inclDirs =
            case :proplists.get_value(
                   :include,
                   startOpts
                 ) do
              :undefined ->
                []

              incls when is_list(hd(incls)) ->
                for iDir <- incls do
                  :filename.absname(iDir)
                end

              incl when is_list(incl) ->
                [:filename.absname(incl)]
            end

          case :os.getenv('CT_INCLUDE_PATH') do
            false ->
              :application.set_env(:common_test, :include, inclDirs)
              {:undefined, inclDirs}

            ctInclPath ->
              inclDirs1 =
                :string.lexemes(
                  ctInclPath,
                  [?:, ?\s, ?,]
                )

              allInclDirs = inclDirs1 ++ inclDirs
              :application.set_env(:common_test, :include, allInclDirs)
              {:undefined, allInclDirs}
          end

        aCBool ->
          :application.set_env(:common_test, :auto_compile, aCBool)
          {aCBool, []}
      end

    abortIfMissing = get_start_opt(:abort_if_missing_suites, :value, false, startOpts)

    case :proplists.get_value(:decrypt, startOpts) do
      :undefined ->
        :application.unset_env(:common_test, :decrypt)

      key = {:key, _} ->
        :application.set_env(:common_test, :decrypt, key)

      {:file, keyFile} ->
        :application.set_env(:common_test, :decrypt, {:file, :filename.absname(keyFile)})
    end

    basicHtml =
      case :proplists.get_value(
             :basic_html,
             startOpts
           ) do
        :undefined ->
          :application.set_env(:common_test, :basic_html, false)
          :undefined

        basicHtmlBool ->
          :application.set_env(:common_test, :basic_html, basicHtmlBool)
          basicHtmlBool
      end

    escChars =
      case :proplists.get_value(
             :esc_chars,
             startOpts
           ) do
        :undefined ->
          :application.set_env(:common_test, :esc_chars, true)
          :undefined

        escCharsBool ->
          :application.set_env(:common_test, :esc_chars, escCharsBool)
          escCharsBool
      end

    case :proplists.get_value(
           :disable_log_cache,
           startOpts
         ) do
      :undefined ->
        :application.set_env(:common_test, :disable_log_cache, false)

      disableCacheBool ->
        :application.set_env(:common_test, :disable_log_cache, disableCacheBool)
    end

    keepLogs = get_start_opt(:keep_logs, &:ct_logs.parse_keep_logs/1, :all, startOpts)
    :application.set_env(:common_test, :keep_logs, keepLogs)
    step = get_start_opt(:step, :value, startOpts)

    opts =
      r_opts(
        label: label,
        profile: profile,
        cover: cover,
        cover_stop: coverStop,
        step: step,
        logdir: logDir,
        logopts: logOpts,
        basic_html: basicHtml,
        esc_chars: escChars,
        config: cfgFiles,
        verbosity: verbosity,
        event_handlers: evHandlers,
        ct_hooks: cTHooks,
        enable_builtin_hooks: enableBuiltinHooks,
        auto_compile: autoCompile,
        abort_if_missing_suites: abortIfMissing,
        include: include,
        silent_connections: silentConns,
        stylesheet: stylesheet,
        multiply_timetraps: multiplyTT,
        scale_timetraps: scaleTT,
        create_priv_dir: createPrivDir,
        starter: :ct
      )

    case :proplists.get_value(:spec, startOpts) do
      :undefined ->
        case :lists.keysearch(:prepared_tests, 1, startOpts) do
          {:value, {_, {run, skip}, specs}} ->
            run_prepared(run, skip, r_opts(opts, testspec_files: specs), startOpts)

          false ->
            run_dir(opts, startOpts)
        end

      specs ->
        relaxed = get_start_opt(:allow_user_terms, :value, false, startOpts)
        run_spec_file(relaxed, r_opts(opts, testspec_files: specs), startOpts)
    end
  end

  defp run_spec_file(relaxed, opts = r_opts(testspec_files: specs), startOpts) do
    specs1 =
      case specs do
        [x | _] when is_integer(x) ->
          [specs]

        _ ->
          specs
      end

    absSpecs =
      :lists.map(
        fn sF ->
          :filename.absname(sF)
        end,
        specs1
      )

    absSpecs1 = get_start_opt(:join_specs, [absSpecs], absSpecs, startOpts)

    try do
      :ct_testspec.collect_tests_from_file(absSpecs1, relaxed)
    catch
      {:error, cTReason} ->
        exit({:error, {:invalid_testspec, {cTReason, __STACKTRACE__}}})

      _, cTReason ->
        exit({:error, {:invalid_testspec, {cTReason, __STACKTRACE__}}})
    else
      testSpecData ->
        run_all_specs(testSpecData, opts, startOpts, [])
    end
  end

  defp run_all_specs([], _, _, totResult) do
    totResult1 = :lists.reverse(totResult)

    case :lists.keysearch(:EXIT, 1, totResult1) do
      {:value, {_, _, exitReason}} ->
        exit(exitReason)

      false ->
        case :lists.keysearch(:error, 1, totResult1) do
          {:value, error} ->
            error

          false ->
            :lists.foldl(
              fn
                {ok, fail, {userSkip, autoSkip}}, {ok1, fail1, {userSkip1, autoSkip1}} ->
                  {ok1 + ok, fail1 + fail, {userSkip1 + userSkip, autoSkip1 + autoSkip}}

                pid, acc when is_pid(pid) ->
                  acc
              end,
              {0, 0, {0, 0}},
              totResult1
            )
        end
    end
  end

  defp run_all_specs([{specs, tS} | tSs], opts, startOpts, totResult) do
    combined = r_opts(config: tSConfig) = combine_test_opts(tS, specs, opts)
    allConfig = merge_vals([r_opts(opts, :config), tSConfig])

    try do
      run_one_spec(
        tS,
        r_opts(combined, config: allConfig, current_testspec: tS),
        startOpts
      )
    catch
      _, reason ->
        run_all_specs(tSs, opts, startOpts, [{:error, reason} | totResult])
    else
      result ->
        run_all_specs(tSs, opts, startOpts, [result | totResult])
    end
  end

  defp run_one_spec(tS, combinedOpts, startOpts) do
    r_opts(logdir: logdir, config: config) = combinedOpts

    case check_and_install_configfiles(config, logdir, combinedOpts) do
      :ok ->
        {run, skip} = :ct_testspec.prepare_tests(tS, node())

        reformat_result(
          try do
            do_run(run, skip, combinedOpts, startOpts)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        )

      error ->
        error
    end
  end

  defp run_prepared(run, skip, opts = r_opts(logdir: logDir, config: cfgFiles), startOpts) do
    logDir1 = which(:logdir, logDir)

    case check_and_install_configfiles(cfgFiles, logDir1, opts) do
      :ok ->
        reformat_result(
          try do
            do_run(run, skip, r_opts(opts, logdir: logDir1), startOpts)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        )

      {:error, _Reason} = error ->
        exit(error)
    end
  end

  defp check_config_file(callback, file) do
    case :code.is_loaded(callback) do
      false ->
        case :code.load_file(callback) do
          {:module, _} ->
            :ok

          {:error, why} ->
            exit({:error, {:cant_load_callback_module, why}})
        end

      _ ->
        :ok
    end

    case callback.check_parameter(file) do
      {:ok, {:file, ^file}} ->
        :filename.absname(file)

      {:ok, {:config, _}} ->
        file

      {:error, {:wrong_config, message}} ->
        exit({:error, {:wrong_config, {callback, message}}})

      {:error, {:nofile, ^file}} ->
        exit({:error, {:no_such_file, :filename.absname(file)}})
    end
  end

  defp run_dir(
         opts =
           r_opts(
             logdir: logDir,
             config: cfgFiles,
             event_handlers: evHandlers,
             ct_hooks: cTHook,
             enable_builtin_hooks: enableBuiltinHooks
           ),
         startOpts
       ) do
    logDir1 = which(:logdir, logDir)
    opts1 = r_opts(opts, logdir: logDir1)

    absCfgFiles =
      :lists.map(
        fn {callback, fileList} ->
          case :code.is_loaded(callback) do
            {:file, _Path} ->
              :ok

            false ->
              case :code.load_file(callback) do
                {:module, ^callback} ->
                  :ok

                {:error, _} ->
                  exit({:error, {:no_such_module, callback}})
              end
          end

          {callback,
           :lists.map(
             fn file ->
               check_config_file(
                 callback,
                 file
               )
             end,
             fileList
           )}
        end,
        cfgFiles
      )

    case install(
           [
             {:config, absCfgFiles},
             {:event_handler, evHandlers},
             {:ct_hooks, cTHook},
             {:enable_builtin_hooks, enableBuiltinHooks}
           ],
           logDir1
         ) do
      :ok ->
        :ok

      {:error, _IReason} = iError ->
        exit(iError)
    end

    case {:proplists.get_value(:dir, startOpts), :proplists.get_value(:suite, startOpts),
          groups_and_cases(
            :proplists.get_value(
              :group,
              startOpts
            ),
            :proplists.get_value(:testcase, startOpts)
          )} do
      {_, _, error = {:error, _}} ->
        error

      {_, [], _} ->
        {:error, :no_suite_specified}

      {[], _, _} ->
        {:error, :no_dir_specified}

      {dirs = [hd | _], :undefined, []}
      when is_list(dirs) and not is_integer(hd) ->
        dirs1 =
          for d <- dirs do
            cond do
              is_atom(d) ->
                :erlang.atom_to_list(d)

              true ->
                d
            end
          end

        reformat_result(
          try do
            do_run(tests(dirs1), [], opts1, startOpts)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        )

      {dir = [hd | _], :undefined, []}
      when is_list(dir) and is_integer(hd) ->
        reformat_result(
          try do
            do_run(tests(dir), [], opts1, startOpts)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        )

      {dir, :undefined, []}
      when is_atom(dir) and dir != :undefined ->
        reformat_result(
          try do
            do_run(tests(:erlang.atom_to_list(dir)), [], opts1, startOpts)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        )

      {:undefined, suites = [hd | _], []} when not is_integer(hd) ->
        suites1 =
          for s <- suites do
            suite_to_test(s)
          end

        reformat_result(
          try do
            do_run(tests(suites1), [], opts1, startOpts)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        )

      {:undefined, suite, []}
      when is_atom(suite) and suite != :undefined ->
        {dir, mod} = suite_to_test(suite)

        reformat_result(
          try do
            do_run(tests(dir, mod), [], opts1, startOpts)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        )

      {:undefined, suite, gsAndCs}
      when is_atom(suite) and suite != :undefined ->
        {dir, mod} = suite_to_test(suite)

        reformat_result(
          try do
            do_run(tests(dir, mod, gsAndCs), [], opts1, startOpts)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        )

      {:undefined, [[hd, _] | _], _GsAndCs} when not is_integer(hd) ->
        exit({:error, :multiple_suites_and_cases})

      {:undefined, suite = [hd | tl], gsAndCs}
      when is_integer(hd) or (is_list(hd) and tl == []) or
             (is_atom(hd) and tl == []) ->
        {dir, mod} = suite_to_test(suite)

        reformat_result(
          try do
            do_run(tests(dir, mod, gsAndCs), [], opts1, startOpts)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        )

      {[[hd, _] | _], _Suites, []}
      when is_list(hd) or
             not is_integer(hd) ->
        exit({:error, :multiple_dirs_and_suites})

      {:undefined, :undefined, gsAndCs} when gsAndCs != [] ->
        exit({:error, :incorrect_start_options})

      {dir, suite, gsAndCs}
      when is_integer(hd(dir)) or
             (is_atom(dir) and dir != :undefined) or
             (length(dir) == 1 and is_atom(hd(dir))) or
             (length(dir) == 1 and is_list(hd(dir))) ->
        dir1 =
          cond do
            is_atom(dir) ->
              :erlang.atom_to_list(dir)

            true ->
              dir
          end

        cond do
          suite == :undefined ->
            exit({:error, :incorrect_start_options})

          is_integer(hd(suite)) or
            (is_atom(suite) and suite != :undefined) or
            (length(suite) == 1 and is_atom(hd(suite))) or
              (length(suite) == 1 and is_list(hd(suite))) ->
            {dir2, mod} = suite_to_test(dir1, suite)

            case gsAndCs do
              [] ->
                reformat_result(
                  try do
                    do_run(tests(dir2, mod), [], opts1, startOpts)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end
                )

              _ ->
                reformat_result(
                  try do
                    do_run(tests(dir2, mod, gsAndCs), [], opts1, startOpts)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end
                )
            end

          is_list(suite) ->
            case (for s <- suite do
                    suite_to_test(dir1, s)
                  end) do
              [[_, _] | _] when gsAndCs != [] ->
                exit({:error, :multiple_suites_and_cases})

              [{dir2, mod}] when gsAndCs != [] ->
                reformat_result(
                  try do
                    do_run(tests(dir2, mod, gsAndCs), [], opts1, startOpts)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end
                )

              dirMods ->
                reformat_result(
                  try do
                    do_run(tests(dirMods), [], opts1, startOpts)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end
                )
            end
        end

      {:undefined, :undefined, []} ->
        {:ok, dir} = :file.get_cwd()

        reformat_result(
          try do
            do_run(tests(dir), [], opts1, startOpts)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        )

      {dir, suite, gsAndCs} ->
        exit({:error, {:incorrect_start_options, {dir, suite, gsAndCs}}})
    end
  end

  def run_testspec(testSpec) do
    cTPid = spawn(run_testspec1_fun(testSpec))
    ref = :erlang.monitor(:process, cTPid)

    receive do
      {:DOWN, ^ref, :process, ^cTPid, {:user_error, error}} ->
        error

      {:DOWN, ^ref, :process, ^cTPid, other} ->
        other
    end
  end

  defp run_testspec1_fun(testSpec) do
    fn ->
      :ct_util.mark_process()
      run_testspec1(testSpec)
    end
  end

  defp run_testspec1(testSpec) do
    {:ok, cwd} = :file.get_cwd()
    :io.format('~nCommon Test starting (cwd is ~ts)~n~n', [cwd])

    case (try do
            run_testspec2(testSpec)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        :ok = :file.set_cwd(cwd)
        exit({:error, reason})

      result ->
        exit(result)
    end
  end

  defp run_testspec2(file)
       when is_list(file) and
              is_integer(hd(file)) do
    case :file.read_file_info(file) do
      {:ok, _} ->
        exit('Bad argument, use ct:run_test([{spec,' ++ file ++ '}])')

      _ ->
        exit(
          'Bad argument, list of tuples expected, use ct:run_test/1 for test specification files'
        )
    end
  end

  defp run_testspec2(testSpec) do
    case (try do
            :ct_testspec.collect_tests_from_list(testSpec, false)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {e, cTReason} when e == :error or e == :EXIT ->
        exit({:error, cTReason})

      tS ->
        opts = get_data_for_node(tS, node())

        allInclude =
          case :os.getenv('CT_INCLUDE_PATH') do
            false ->
              r_opts(opts, :include)

            ctInclPath ->
              envInclude =
                :string.lexemes(
                  ctInclPath,
                  [?:, ?\s, ?,]
                )

              envInclude ++ r_opts(opts, :include)
          end

        :application.set_env(:common_test, :include, allInclude)
        logDir1 = which(:logdir, r_opts(opts, :logdir))

        case check_and_install_configfiles(r_opts(opts, :config), logDir1, opts) do
          :ok ->
            opts1 = r_opts(opts, testspec_files: [], logdir: logDir1, include: allInclude)
            {run, skip} = :ct_testspec.prepare_tests(tS, node())

            reformat_result(
              try do
                do_run(run, skip, opts1, [])
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end
            )

          {:error, _GCFReason} = gCFError ->
            exit(gCFError)
        end
    end
  end

  defp get_data_for_node(
         r_testspec(
           label: labels,
           profile: profiles,
           logdir: logDirs,
           logopts: logOptsList,
           basic_html: bHs,
           esc_chars: escChs,
           stylesheet: sSs,
           verbosity: vLvls,
           silent_connections: silentConnsList,
           cover: coverFs,
           cover_stop: coverStops,
           config: cfgs,
           userconfig: usrCfgs,
           event_handler: evHs,
           ct_hooks: cTHooks,
           enable_builtin_hooks: enableBuiltinHooks,
           auto_compile: aCs,
           abort_if_missing_suites: aiMSs,
           include: incl,
           multiply_timetraps: mTs,
           scale_timetraps: sTs,
           create_priv_dir: pDs
         ),
         node
       ) do
    label = :proplists.get_value(node, labels)
    profile = :proplists.get_value(node, profiles)

    logDir =
      case :proplists.get_value(node, logDirs) do
        :undefined ->
          '.'

        dir ->
          dir
      end

    logOpts =
      case :proplists.get_value(
             node,
             logOptsList
           ) do
        :undefined ->
          []

        lOs ->
          lOs
      end

    basicHtml = :proplists.get_value(node, bHs)
    escChars = :proplists.get_value(node, escChs)
    stylesheet = :proplists.get_value(node, sSs)

    verbosity =
      case :proplists.get_value(node, vLvls) do
        :undefined ->
          []

        lvls ->
          lvls
      end

    silentConns =
      case :proplists.get_value(
             node,
             silentConnsList
           ) do
        :undefined ->
          []

        sCs ->
          sCs
      end

    cover = :proplists.get_value(node, coverFs)
    coverStop = :proplists.get_value(node, coverStops)
    mT = :proplists.get_value(node, mTs)
    sT = :proplists.get_value(node, sTs)
    createPrivDir = :proplists.get_value(node, pDs)

    configFiles =
      for {n, f} <- cfgs, n == node do
        {:ct_config_plain, f}
      end ++
        for {n, cBF} <- usrCfgs, n == node do
          cBF
        end

    evHandlers =
      for {n, h, a} <- evHs, n == node do
        {h, a}
      end

    filtCTHooks =
      for {n, hook} <- cTHooks, n == node do
        hook
      end

    autoCompile = :proplists.get_value(node, aCs)
    abortIfMissing = :proplists.get_value(node, aiMSs)

    include =
      for {n, i} <- incl, n == node do
        i
      end

    r_opts(
      label: label,
      profile: profile,
      logdir: logDir,
      logopts: logOpts,
      basic_html: basicHtml,
      esc_chars: escChars,
      stylesheet: stylesheet,
      verbosity: verbosity,
      silent_connections: silentConns,
      cover: cover,
      cover_stop: coverStop,
      config: configFiles,
      event_handlers: evHandlers,
      ct_hooks: filtCTHooks,
      enable_builtin_hooks: enableBuiltinHooks,
      auto_compile: autoCompile,
      abort_if_missing_suites: abortIfMissing,
      include: include,
      multiply_timetraps: mT,
      scale_timetraps: sT,
      create_priv_dir: createPrivDir
    )
  end

  def refresh_logs(logDir) do
    {:ok, cwd} = :file.get_cwd()

    case :file.set_cwd(logDir) do
      e = {:error, _Reason} ->
        e

      _ ->
        case (try do
                :ct_logs.make_all_suites_index(:refresh)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, aSReason} ->
            :ok = :file.set_cwd(cwd)
            {:error, {:all_suites_index, aSReason}}

          _ ->
            case (try do
                    :ct_logs.make_all_runs_index(:refresh)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:EXIT, aRReason} ->
                :ok = :file.set_cwd(cwd)
                {:error, {:all_runs_index, aRReason}}

              _ ->
                :ok = :file.set_cwd(cwd)
                :io.format('Logs in ~ts refreshed!~n', [logDir])
                :ok
            end
        end
    end
  end

  defp which(:logdir, :undefined) do
    '.'
  end

  defp which(:logdir, dir) do
    dir
  end

  defp choose_val(:undefined, v1) do
    v1
  end

  defp choose_val(v0, _V1) do
    v0
  end

  defp merge_vals(vs) do
    :lists.append(vs)
  end

  defp merge_keyvals(vs) do
    make_unique(:lists.append(vs))
  end

  defp make_unique([elem = {key, _} | elems]) do
    [elem | make_unique(:proplists.delete(key, elems))]
  end

  defp make_unique([]) do
    []
  end

  defp listify([c | _] = str) when is_integer(c) do
    [str]
  end

  defp listify(l) when is_list(l) do
    l
  end

  defp listify(e) do
    [e]
  end

  defp delistify([e]) do
    e
  end

  defp delistify(e) do
    e
  end

  def run(testDir, suite, cases) do
    case install([]) do
      :ok ->
        reformat_result(
          try do
            do_run(tests(testDir, suite, cases), [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        )

      error ->
        error
    end
  end

  def run(testDir, suite)
      when is_list(testDir) and
             is_integer(hd(testDir)) do
    case install([]) do
      :ok ->
        reformat_result(
          try do
            do_run(tests(testDir, suite), [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        )

      error ->
        error
    end
  end

  def run(testDirs) do
    case install([]) do
      :ok ->
        reformat_result(
          try do
            do_run(tests(testDirs), [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        )

      error ->
        error
    end
  end

  defp reformat_result({:EXIT, {:user_error, reason}}) do
    {:error, reason}
  end

  defp reformat_result({:user_error, reason}) do
    {:error, reason}
  end

  defp reformat_result(result) do
    result
  end

  defp suite_to_test(suite) when is_atom(suite) do
    suite_to_test(:erlang.atom_to_list(suite))
  end

  defp suite_to_test(suite) when is_list(suite) do
    {:filename.dirname(suite),
     :erlang.list_to_atom(:filename.rootname(:filename.basename(suite)))}
  end

  defp suite_to_test(dir, suite) when is_atom(suite) do
    suite_to_test(dir, :erlang.atom_to_list(suite))
  end

  defp suite_to_test(dir, suite) when is_list(suite) do
    case :filename.dirname(suite) do
      '.' ->
        {dir, :erlang.list_to_atom(:filename.rootname(suite))}

      dirName ->
        file = :filename.basename(suite)
        {dirName, :erlang.list_to_atom(:filename.rootname(file))}
    end
  end

  defp groups_and_cases(gs, cs)
       when (gs == :undefined or gs == []) and (cs == :undefined or cs == []) do
    []
  end

  defp groups_and_cases(gs, cs) when gs == :undefined or gs == [] do
    cond do
      cs == :all or cs == [:all] or cs == ['all'] ->
        :all

      true ->
        for c <- listify(cs) do
          ensure_atom(c)
        end
    end
  end

  defp groups_and_cases(gOrGs, cs)
       when is_atom(gOrGs) or
              (is_list(gOrGs) and
                 (is_atom(hd(gOrGs)) or (is_list(hd(gOrGs)) and is_atom(hd(hd(gOrGs)))))) do
    cond do
      cs == :undefined or cs == [] or cs == :all or cs == [:all] or cs == ['all'] ->
        [{gOrGs, :all}]

      true ->
        [
          {gOrGs,
           for c <- listify(cs) do
             ensure_atom(c)
           end}
        ]
    end
  end

  defp groups_and_cases(gs, cs) when is_integer(hd(hd(gs))) do
    gs1 =
      cond do
        gs == [:all] or gs == ['all'] ->
          :all

        true ->
          :lists.map(
            fn g ->
              {:ok, ts, _} = :erl_scan.string(g ++ '.')
              {:ok, term} = :erl_parse.parse_term(ts)
              term
            end,
            gs
          )
      end

    groups_and_cases(gs1, cs)
  end

  defp groups_and_cases(gs, cs) do
    {:error, {:incorrect_group_or_case_option, gs, cs}}
  end

  defp tests(testDir, suites, [])
       when is_list(testDir) and
              is_integer(hd(testDir)) do
    [{:ct_util.get_testdir(testDir, suites), ensure_atom(suites), :all}]
  end

  defp tests(testDir, suite, cases)
       when is_list(testDir) and is_integer(hd(testDir)) do
    [{:ct_util.get_testdir(testDir, suite), ensure_atom(suite), cases}]
  end

  defp tests([testDir], suite, cases)
       when is_list(testDir) and is_integer(hd(testDir)) do
    [{:ct_util.get_testdir(testDir, suite), ensure_atom(suite), cases}]
  end

  defp tests([{dir, suite}], cases) do
    [{:ct_util.get_testdir(dir, suite), ensure_atom(suite), cases}]
  end

  defp tests(testDir, suite)
       when is_list(testDir) and
              is_integer(hd(testDir)) do
    tests(testDir, ensure_atom(suite), :all)
  end

  defp tests([testDir], suite)
       when is_list(testDir) and
              is_integer(hd(testDir)) do
    tests(testDir, ensure_atom(suite), :all)
  end

  defp tests(dirSuites)
       when is_list(dirSuites) and
              is_tuple(hd(dirSuites)) do
    for {dir, suite} <- dirSuites do
      {:ct_util.get_testdir(dir, suite), ensure_atom(suite), :all}
    end
  end

  defp tests(testDir)
       when is_list(testDir) and
              is_integer(hd(testDir)) do
    tests([testDir])
  end

  defp tests(testDirs)
       when is_list(testDirs) and
              is_list(hd(testDirs)) do
    for testDir <- testDirs do
      {:ct_util.get_testdir(testDir, :all), :all, :all}
    end
  end

  defp do_run(tests, misc) when is_list(misc) do
    do_run(tests, misc, '.', [])
  end

  defp do_run(tests, misc, logDir, logOpts)
       when is_list(misc) and is_list(logDir) and
              is_list(logOpts) do
    opts =
      case :proplists.get_value(:step, misc) do
        :undefined ->
          r_opts()

        stepOpts ->
          r_opts(step: stepOpts)
      end

    do_run(tests, [], r_opts(opts, logdir: logDir), [])
  end

  defp do_run(tests, skip, opts, args)
       when elem(opts, 0) === :opts do
    r_opts(label: label, profile: profile, verbosity: vLvls) = opts

    testLabel =
      cond do
        label == :undefined ->
          :undefined

        is_atom(label) ->
          :erlang.atom_to_list(label)

        is_list(label) ->
          label

        true ->
          :undefined
      end

    :application.set_env(:common_test, :test_label, testLabel)

    testProfile =
      cond do
        profile == :undefined ->
          :undefined

        is_atom(profile) ->
          :erlang.atom_to_list(profile)

        is_list(profile) ->
          profile

        true ->
          :undefined
      end

    :application.set_env(:common_test, :profile, testProfile)

    case :code.which(:test_server) do
      :non_existing ->
        {:error, :no_path_to_test_server}

      _ ->
        case :os.getenv('TEST_SERVER_FRAMEWORK') do
          false ->
            :os.putenv('TEST_SERVER_FRAMEWORK', 'ct_framework')
            :os.putenv('TEST_SERVER_FRAMEWORK_NAME', 'common_test')

          'ct_framework' ->
            :ok

          other ->
            :erlang.display(:erlang.list_to_atom('Note: TEST_SERVER_FRAMEWORK = ' ++ other))
        end

        verbosity = add_verbosity_defaults(vLvls)

        case :ct_util.start(r_opts(opts, :logdir), verbosity) do
          {:error, :interactive_mode} ->
            :io.format(
              'CT is started in interactive mode. To exit this mode, run ct:stop_interactive().\nTo enter the interactive mode again, run ct:start_interactive()\n\n',
              []
            )

            {:error, :interactive_mode}

          _Pid ->
            :ct_util.set_testdata({:starter, r_opts(opts, :starter)})
            compile_and_run(tests, skip, r_opts(opts, verbosity: verbosity), args)
        end
    end
  end

  defp compile_and_run(tests, skip, opts, args) do
    :ct_util.set_testdata({:stylesheet, r_opts(opts, :stylesheet)})
    :ct_util.set_testdata({:logopts, r_opts(opts, :logopts)})
    :ct_util.set_testdata({:testspec, r_opts(opts, :current_testspec)})

    case r_opts(opts, :silent_connections) do
      [] ->
        :ok

      conns ->
        case :lists.member(:all, conns) do
          true ->
            conns1 = :ct_util.override_silence_all_connections()
            :ct_logs.log('Silent connections', '~tp', [conns1])

          false ->
            :ct_util.override_silence_connections(conns)
            :ct_logs.log('Silent connections', '~tp', [conns])
        end
    end

    log_ts_names(r_opts(opts, :testspec_files))
    testSuites = suite_tuples(tests)

    {_TestSuites1, suiteMakeErrors, allMakeErrors} =
      case :application.get_env(
             :common_test,
             :auto_compile
           ) do
        {:ok, false} ->
          {testSuites1, suitesNotFound} = verify_suites(testSuites)
          {testSuites1, suitesNotFound, suitesNotFound}

        _ ->
          {suiteErrs, helpErrs} = auto_compile(testSuites)
          {testSuites, suiteErrs, suiteErrs ++ helpErrs}
      end

    case continue(
           allMakeErrors,
           r_opts(opts, :abort_if_missing_suites)
         ) do
      true ->
        savedErrors = save_make_errors(suiteMakeErrors)
        :ct_repeat.log_loop_info(args)

        try do
          final_tests(tests, skip, savedErrors)
        catch
          _, badFormat ->
            {:error, badFormat}
        else
          {tests1, skip1} ->
            releaseSh = :proplists.get_value(:release_shell, args)
            :ct_util.set_testdata({:release_shell, releaseSh})
            testResult = possibly_spawn(releaseSh == true, tests1, skip1, opts)

            case testResult do
              {ok, errors, skipped} ->
                noOfMakeErrors =
                  :lists.foldl(
                    fn {_, badMods}, x ->
                      x + length(badMods)
                    end,
                    0,
                    suiteMakeErrors
                  )

                {ok, errors + noOfMakeErrors, skipped}

              errorResult ->
                errorResult
            end
        end

      false ->
        :io.nl()
        :ct_util.stop(:clean)

        badMods =
          :lists.foldr(
            fn {{_, _}, ms}, acc ->
              ms ++
                :lists.foldl(
                  fn m, acc1 ->
                    :lists.delete(
                      m,
                      acc1
                    )
                  end,
                  acc,
                  ms
                )
            end,
            [],
            allMakeErrors
          )

        {:error, {:make_failed, badMods}}
    end
  end

  defp possibly_spawn(false, tests, skip, opts) do
    testResult =
      try do
        do_run_test(tests, skip, opts)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    case testResult do
      {eType, _} = error
      when eType == :user_error or
             eType == :error ->
        :ct_util.stop(:clean)
        exit(error)

      _ ->
        :ct_util.stop(:normal)
        testResult
    end
  end

  defp possibly_spawn(true, tests, skip, opts) do
    cTUtilSrv = :erlang.whereis(:ct_util_server)

    supervisor = fn ->
      :ct_util.mark_process()
      :erlang.process_flag(:trap_exit, true)
      :erlang.link(cTUtilSrv)

      testRun = fn ->
        :ct_util.mark_process()

        testResult =
          try do
            do_run_test(tests, skip, opts)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        case testResult do
          {eType, _} = error
          when eType == :user_error or
                 eType == :error ->
            :ct_util.stop(:clean)
            exit(error)

          _ ->
            :ct_util.stop(:normal)
            exit({:ok, testResult})
        end
      end

      testRunPid = spawn_link(testRun)

      receive do
        {:EXIT, ^testRunPid, {:ok, testResult}} ->
          :io.format(:user, '~nCommon Test returned ~tp~n~n', [testResult])

        {:EXIT, ^testRunPid, error} ->
          exit(error)
      end
    end

    :erlang.unlink(cTUtilSrv)
    supPid = spawn(supervisor)
    :io.format(:user, '~nTest control handed over to process ~w~n~n', [supPid])
    supPid
  end

  defp auto_compile(testSuites) do
    :io.format('~nCommon Test: Running make in test directories...~n')

    userInclude =
      case :application.get_env(
             :common_test,
             :include
           ) do
        {:ok, userInclDirs} when length(userInclDirs) > 0 ->
          :io.format('Including the following directories:~n')

          for userInclDir <- userInclDirs do
            :io.format('~tp~n', [userInclDir])
            {:i, userInclDir}
          end

        _ ->
          []
      end

    suiteMakeErrors =
      :lists.flatmap(
        fn {testDir, suite} = tS ->
          case run_make(:suites, testDir, suite, userInclude, [:nowarn_export_all]) do
            {:error, {:make_failed, bad}} ->
              [{tS, bad}]

            {:error, _} ->
              [{tS, [:filename.join(testDir, '*_SUITE')]}]

            _ ->
              []
          end
        end,
        testSuites
      )

    {_, helpMakeErrors} =
      :lists.foldl(
        fn {dir, suite}, {done, failed} ->
          case :lists.member(dir, done) do
            false ->
              failed1 =
                case run_make(
                       :helpmods,
                       dir,
                       suite,
                       userInclude,
                       []
                     ) do
                  {:error, {:make_failed, badMods}} ->
                    [
                      {{dir, :all}, badMods}
                      | failed
                    ]

                  {:error, _} ->
                    [
                      {{dir, :all}, [dir]}
                      | failed
                    ]

                  _ ->
                    failed
                end

              {[dir | done], failed1}

            true ->
              {done, failed}
          end
        end,
        {[], []},
        testSuites
      )

    {suiteMakeErrors, :lists.reverse(helpMakeErrors)}
  end

  defp verify_suites(testSuites) do
    :io.nl()

    verify = fn {dir, suite} = dS, {found, notFound} ->
      case locate_test_dir(dir, suite) do
        {:ok, testDir} ->
          cond do
            suite == :all ->
              {[dS | found], notFound}

            true ->
              beam =
                :filename.join(
                  testDir,
                  :erlang.atom_to_list(suite) ++ '.beam'
                )

              case :filelib.is_regular(beam) do
                true ->
                  {[dS | found], notFound}

                false ->
                  case :code.is_loaded(suite) do
                    {:file, suiteFile} ->
                      actualDir = :filename.dirname(suiteFile)
                      {[{actualDir, suite} | found], notFound}

                    false ->
                      name =
                        :filename.join(
                          testDir,
                          :erlang.atom_to_list(suite)
                        )

                      :io.format(:user, 'Suite ~w not found in directory ~ts~n', [suite, testDir])
                      {found, [{dS, [name]} | notFound]}
                  end
              end
          end

        {:error, _Reason} ->
          case :code.is_loaded(suite) do
            {:file, suiteFile} ->
              actualDir = :filename.dirname(suiteFile)
              {[{actualDir, suite} | found], notFound}

            false ->
              :io.format(:user, 'Directory ~ts is invalid~n', [dir])

              name =
                :filename.join(
                  dir,
                  :erlang.atom_to_list(suite)
                )

              {found, [{dS, [name]} | notFound]}
          end
      end
    end

    {actualFound, missing} = :lists.foldl(verify, {[], []}, testSuites)
    {:lists.reverse(actualFound), :lists.reverse(missing)}
  end

  defp save_make_errors([]) do
    []
  end

  defp save_make_errors(errors) do
    suites = get_bad_suites(errors, [])

    :ct_logs.log('MAKE RESULTS', 'Error compiling or locating the following suites: ~n~p', [
      suites
    ])

    :ok =
      :file.write_file(
        'missing_suites.info',
        :erlang.term_to_binary(errors)
      )

    errors
  end

  defp get_bad_suites(
         [{{_TestDir, _Suite}, failed} | errors],
         badSuites
       ) do
    get_bad_suites(errors, badSuites ++ failed)
  end

  defp get_bad_suites([], badSuites) do
    badSuites
  end

  def step(testDir, suite, case__) do
    step(testDir, suite, case__, [])
  end

  def step(testDir, suite, case__, opts)
      when is_list(testDir) and is_atom(suite) and
             is_atom(case__) and suite !== :all and
             case__ !== :all do
    do_run([{testDir, suite, case__}], [{:step, opts}])
  end

  defp suite_tuples([{testDir, suites, _} | tests])
       when is_list(suites) do
    :lists.map(
      fn s ->
        {testDir, s}
      end,
      suites
    ) ++ suite_tuples(tests)
  end

  defp suite_tuples([{testDir, suite, _} | tests])
       when is_atom(suite) do
    [{testDir, suite} | suite_tuples(tests)]
  end

  defp suite_tuples([]) do
    []
  end

  defp final_tests(tests, skip, bad) do
    {tests1, skip1} = final_tests1(tests, [], skip, bad)
    skip2 = final_skip(skip1, [])
    {tests1, skip2}
  end

  defp final_tests1([{testDir, suites, _} | tests], final, skip, bad)
       when is_list(suites) and is_atom(hd(suites)) do
    skip1 =
      for {{tD, s}, _} <- bad, s1 <- suites, s == s1, tD == testDir do
        {tD, s, :make_failed}
      end

    final1 =
      for s <- suites do
        {testDir, s, :all}
      end

    final_tests1(tests, :lists.reverse(final1) ++ final, skip ++ skip1, bad)
  end

  defp final_tests1([{testDir, :all, :all} | tests], final, skip, bad) do
    missingSuites =
      case :lists.keysearch({testDir, :all}, 1, bad) do
        {:value, {_, failed}} ->
          for f <- failed do
            :erlang.list_to_atom(:filename.basename(f))
          end

        false ->
          []
      end

    missing =
      for s <- missingSuites do
        {testDir, s, :make_failed}
      end

    final1 = [{testDir, :all, :all} | final]
    final_tests1(tests, final1, skip ++ missing, bad)
  end

  defp final_tests1([{testDir, suite, cases} | tests], final, skip, bad)
       when cases == [] or cases == :all do
    final_tests1([{testDir, [suite], :all} | tests], final, skip, bad)
  end

  defp final_tests1([{testDir, suite, grsOrCs} | tests], final, skip, bad)
       when is_list(grsOrCs) do
    case :lists.keymember({testDir, suite}, 1, bad) do
      true ->
        skip1 = skip ++ [{testDir, suite, :all, :make_failed}]
        final_tests1(tests, [{testDir, suite, :all} | final], skip1, bad)

      false ->
        grsOrCs1 =
          :lists.flatmap(
            fn
              {:all, :all} ->
                [:ct_groups.make_conf(testDir, suite, :all, [], :all)]

              {:skipped, group, tCs} ->
                [:ct_groups.make_conf(testDir, suite, group, [:skipped], tCs)]

              {:skipped, tC} ->
                case :lists.member(tC, grsOrCs) do
                  true ->
                    []

                  false ->
                    [tC]
                end

              {grSpec = {groupName, _}, tCs} ->
                props = [{:override, grSpec}]
                [:ct_groups.make_conf(testDir, suite, groupName, props, tCs)]

              {grSpec = {groupName, _, _}, tCs} ->
                props = [{:override, grSpec}]
                [:ct_groups.make_conf(testDir, suite, groupName, props, tCs)]

              {groupOrGroups, tCs} ->
                [:ct_groups.make_conf(testDir, suite, groupOrGroups, [], tCs)]

              tC ->
                [tC]
            end,
            grsOrCs
          )

        do__ = {testDir, suite, grsOrCs1}
        final_tests1(tests, [do__ | final], skip, bad)
    end
  end

  defp final_tests1([], final, skip, _Bad) do
    {:lists.reverse(final), skip}
  end

  defp final_skip(
         [
           {testDir, suite, {:all, :all}, reason}
           | skips
         ],
         final
       ) do
    skipConf = :ct_groups.make_conf(testDir, suite, :all, [], :all)
    skip = {testDir, suite, skipConf, reason}
    final_skip(skips, [skip | final])
  end

  defp final_skip(
         [
           {testDir, suite, {group, tCs}, reason}
           | skips
         ],
         final
       ) do
    conf = :ct_groups.make_conf(testDir, suite, group, [], tCs)
    skip = {testDir, suite, conf, reason}
    final_skip(skips, [skip | final])
  end

  defp final_skip([skip | skips], final) do
    final_skip(skips, [skip | final])
  end

  defp final_skip([], final) do
    :lists.reverse(final)
  end

  defp continue([], _) do
    true
  end

  defp continue(_MakeErrors, true) do
    false
  end

  defp continue(_MakeErrors, _AbortIfMissingSuites) do
    :io.nl()
    oldGL = :erlang.group_leader()

    case set_group_leader_same_as_shell(oldGL) do
      true ->
        s = self()

        :io.format(
          'Failed to compile or locate one or more test suites\nPress \'c\' to continue or \'a\' to abort.\nWill continue in 15 seconds if no answer is given!\n'
        )

        pid =
          spawn(fn ->
            case :io.get_line(:"(c/a) ") do
              'c\n' ->
                send(s, true)

              _ ->
                send(s, false)
            end
          end)

        :erlang.group_leader(oldGL, self())

        receive do
          r when r == true or r == false ->
            r
        after
          15000 ->
            :erlang.exit(pid, :kill)
            :io.format('... timeout - continuing!!\n')
            true
        end

      false ->
        true
    end
  end

  defp set_group_leader_same_as_shell(oldGL) do
    gS2or3 = fn p ->
      case :erlang.process_info(p, :initial_call) do
        {:initial_call, {:group, :server, x}}
        when x == 2 or
               x == 3 ->
          true

        _ ->
          false
      end
    end

    case (for p <- :erlang.processes(),
              gS2or3.(p),
              true ==
                :lists.keymember(
                  :shell,
                  1,
                  :erlang.element(
                    2,
                    :erlang.process_info(
                      p,
                      :dictionary
                    )
                  )
                ) do
            p
          end) do
      [gL | _] ->
        cond do
          node(oldGL) != node(gL) ->
            false

          true ->
            :erlang.group_leader(gL, self())
        end

      [] ->
        false
    end
  end

  defp check_and_add([{testDir0, m, _} | tests], added, pA) do
    case locate_test_dir(testDir0, m) do
      {:ok, testDir} ->
        case :lists.member(testDir, added) do
          true ->
            check_and_add(tests, added, pA)

          false ->
            case :lists.member(
                   rm_trailing_slash(testDir),
                   :code.get_path()
                 ) do
              false ->
                true = :code.add_patha(testDir)
                check_and_add(tests, [testDir | added], [testDir | pA])

              true ->
                check_and_add(tests, [testDir | added], pA)
            end
        end

      {:error, _} ->
        {:error, {:invalid_directory, testDir0}}
    end
  end

  defp check_and_add([], _, pA) do
    {:ok, pA}
  end

  defp do_run_test(tests, skip, opts0) do
    case check_and_add(tests, [], []) do
      {:ok, addedToPath} ->
        :ct_util.set_testdata({:stats, {0, 0, {0, 0}}})

        inclPath =
          case :application.get_env(
                 :common_test,
                 :include
               ) do
            {:ok, incls} ->
              incls

            _ ->
              []
          end

        :application.set_env(:test_server, :include, inclPath)

        escChars =
          case :application.get_env(
                 :common_test,
                 :esc_chars
               ) do
            {:ok, eCBool} ->
              eCBool

            _ ->
              true
          end

        :application.set_env(:test_server, :esc_chars, escChars)
        {:ok, _} = :test_server_ctrl.start_link(:local)
        {suites, noOfCases} = count_test_cases(tests, skip)
        suites1 = delete_dups(suites)
        noOfTests = length(tests)
        noOfSuites = length(suites1)
        :ct_util.warn_duplicates(suites1)
        {:ok, cwd} = :file.get_cwd()
        :io.format('~nCWD set to: ~tp~n', [cwd])

        cond do
          noOfCases == :unknown ->
            :io.format('~nTEST INFO: ~w test(s), ~w suite(s)~n~n', [noOfTests, noOfSuites])
            :ct_logs.log('TEST INFO', '~w test(s), ~w suite(s)', [noOfTests, noOfSuites])

          true ->
            :io.format('~nTEST INFO: ~w test(s), ~w case(s) in ~w suite(s)~n~n', [
              noOfTests,
              noOfCases,
              noOfSuites
            ])

            :ct_logs.log('TEST INFO', '~w test(s), ~w case(s) in ~w suite(s)', [
              noOfTests,
              noOfCases,
              noOfSuites
            ])
        end

        case :proplists.get_value(
               :default,
               r_opts(opts0, :verbosity)
             ) do
          vLvl when is_integer(vLvl) and 50 < 100 - vLvl ->
            :test_server_ctrl.reject_io_reqs(true)

          _Lower ->
            :ok
        end

        case r_opts(opts0, :multiply_timetraps) do
          :undefined ->
            multTT = 1

          multTT ->
            multTT
        end

        case r_opts(opts0, :scale_timetraps) do
          :undefined ->
            scaleTT = false

          scaleTT ->
            scaleTT
        end

        :ct_logs.log(
          'TEST INFO',
          'Timetrap time multiplier = ~w~nTimetrap scaling enabled = ~w',
          [multTT, scaleTT]
        )

        :test_server_ctrl.multiply_timetraps(multTT)
        :test_server_ctrl.scale_timetraps(scaleTT)

        :test_server_ctrl.create_priv_dir(
          choose_val(
            r_opts(opts0, :create_priv_dir),
            :auto_per_run
          )
        )

        {:ok, logDir} = :ct_logs.get_log_dir(true)
        {tsCoverInfo, opts} = maybe_start_cover(opts0, logDir)

        :ct_event.notify(
          r_event(name: :start_info, node: node(), data: {noOfTests, noOfSuites, noOfCases})
        )

        cleanUp = add_jobs(tests, skip, opts, [])
        :erlang.unlink(:erlang.whereis(:test_server_ctrl))

        try do
          :test_server_ctrl.wait_finish()
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        maybe_stop_cover(opts, tsCoverInfo, logDir)

        case :ct_util.get_testdata(:interpret) do
          {_What, :kill, {tCPid, attPid}} ->
            :ct_util.kill_attached(tCPid, attPid)

          _ ->
            :ok
        end

        :lists.foreach(
          fn suite ->
            maybe_cleanup_interpret(suite, r_opts(opts, :step))
          end,
          cleanUp
        )

        _ =
          for dir <- addedToPath do
            :code.del_path(dir)
          end

        case :ct_util.get_testdata(:severe_error) do
          :undefined ->
            :ok

          severeError ->
            :ct_logs.log('SEVERE ERROR', '~tp\n', [severeError])
            exit(severeError)
        end

        case :ct_util.get_testdata(:stats) do
          stats = {_Ok, _Failed, {_UserSkipped, _AutoSkipped}} ->
            stats

          _ ->
            {:error, :test_result_unknown}
        end

      error ->
        exit(error)
    end
  end

  defp maybe_start_cover(
         opts = r_opts(cover: cover, cover_stop: coverStop0),
         logDir
       ) do
    cond do
      cover == :undefined ->
        {:undefined, opts}

      true ->
        case :ct_cover.get_spec(cover) do
          {:error, reason} ->
            exit({:error, reason})

          coverSpec ->
            coverStop =
              case coverStop0 do
                :undefined ->
                  true

                stop ->
                  stop
              end

            start_cover(
              r_opts(opts,
                coverspec: coverSpec,
                cover_stop: coverStop
              ),
              logDir
            )
        end
    end
  end

  defp start_cover(
         opts =
           r_opts(
             coverspec: covData,
             cover_stop: covStop
           ),
         logDir
       ) do
    {covFile, covNodes, covImport, _CovExport,
     r_cover(
       app: covApp,
       local_only: localOnly,
       level: covLevel,
       excl_mods: covExcl,
       incl_mods: covIncl,
       cross: covCross,
       src: _CovSrc
     )} = covData

    case localOnly do
      true ->
        :cover.local_only()

      false ->
        :ok
    end

    :ct_logs.log(
      'COVER INFO',
      'Using cover specification file: ~ts~nApp: ~w~nLocal only: ~w~nCross cover: ~w~nIncluding ~w modules~nExcluding ~w modules',
      [covFile, covApp, localOnly, covCross, length(covIncl), length(covExcl)]
    )

    :test_server_ctrl.cover({:log, logDir})

    {:ok, tsCoverInfo} =
      :test_server_ctrl.cover_compile(
        covApp,
        covFile,
        covExcl,
        covIncl,
        covCross,
        covLevel,
        covStop
      )

    :ct_logs.log('COVER INFO', 'Compilation completed - test_server cover info: ~tp', [
      tsCoverInfo
    ])

    cond do
      covNodes != [] and covNodes != :undefined ->
        :ct_logs.log('COVER INFO', 'Nodes included in cover session: ~tw', [covNodes])
        :cover.start(covNodes)

      true ->
        :ok
    end

    :lists.foreach(
      fn imp ->
        case :cover.import(imp) do
          :ok ->
            :ok

          {:error, reason} ->
            :ct_logs.log('COVER INFO', 'Importing cover data from: ~ts fails! Reason: ~tp', [
              imp,
              reason
            ])
        end
      end,
      covImport
    )

    {tsCoverInfo, opts}
  end

  defp maybe_stop_cover(_, :undefined, _) do
    :ok
  end

  defp maybe_stop_cover(r_opts(coverspec: covData), tsCoverInfo, logDir) do
    {_CovFile, _CovNodes, _CovImport, covExport, _AppData} = covData

    case covExport do
      :undefined ->
        :ok

      _ ->
        :ct_logs.log('COVER INFO', 'Exporting cover data to ~tp', [covExport])
        :cover.export(covExport)
    end

    :ct_logs.log('COVER INFO', 'Analysing cover data to ~tp', [logDir])
    :test_server_ctrl.cover_analyse(tsCoverInfo, logDir)
    :ct_logs.log('COVER INFO', 'Analysis completed.', [])
    :ok
  end

  defp delete_dups([s | suites]) do
    suites1 = :lists.delete(s, suites)
    [s | delete_dups(suites1)]
  end

  defp delete_dups([]) do
    []
  end

  defp count_test_cases(tests, skip) do
    sendResult = fn me, result ->
      send(me, {:no_of_cases, result})
    end

    tSPid = :test_server_ctrl.start_get_totals(sendResult)
    ref = :erlang.monitor(:process, tSPid)
    _ = add_jobs(tests, skip, r_opts(), [])

    counted =
      try do
        count_test_cases1(length(tests), 0, [], ref)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    :erlang.demonitor(ref, [:flush])

    case counted do
      {:error, {:test_server_died}} = error ->
        throw(error)

      {:error, reason} ->
        :erlang.unlink(:erlang.whereis(:test_server_ctrl))
        :test_server_ctrl.stop()
        throw({:user_error, reason})

      result ->
        :test_server_ctrl.stop_get_totals()
        result
    end
  end

  defp count_test_cases1(0, n, suites, _) do
    {:lists.flatten(suites), n}
  end

  defp count_test_cases1(jobs, n, suites, ref) do
    receive do
      {_, {:error, _Reason} = error} ->
        throw(error)

      {:no_of_cases, {ss, n1}} ->
        count_test_cases1(jobs - 1, add_known(n, n1), [ss | suites], ref)

      {:DOWN, ^ref, _, _, info} ->
        throw({:error, {:test_server_died, info}})
    end
  end

  defp add_known(:unknown, _) do
    :unknown
  end

  defp add_known(_, :unknown) do
    :unknown
  end

  defp add_known(n, n1) do
    n + n1
  end

  defp add_jobs([{testDir, :all, _} | tests], skip, opts, cleanUp) do
    name = get_name(testDir)

    case (try do
            :test_server_ctrl.add_dir_with_skip(name, testDir, skiplist(testDir, skip))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        cleanUp

      _ ->
        case wait_for_idle() do
          :ok ->
            add_jobs(tests, skip, opts, cleanUp)

          _ ->
            cleanUp
        end
    end
  end

  defp add_jobs([{testDir, [suite], :all} | tests], skip, opts, cleanUp)
       when is_atom(suite) do
    add_jobs([{testDir, suite, :all} | tests], skip, opts, cleanUp)
  end

  defp add_jobs([{testDir, suites, :all} | tests], skip, opts, cleanUp)
       when is_list(suites) do
    name = get_name(testDir) ++ '.suites'

    case (try do
            :test_server_ctrl.add_module_with_skip(name, suites, skiplist(testDir, skip))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        cleanUp

      _ ->
        case wait_for_idle() do
          :ok ->
            add_jobs(tests, skip, opts, cleanUp)

          _ ->
            cleanUp
        end
    end
  end

  defp add_jobs([{testDir, suite, :all} | tests], skip, opts, cleanUp) do
    case maybe_interpret(suite, :all, opts) do
      :ok ->
        name = get_name(testDir) ++ '.' ++ :erlang.atom_to_list(suite)

        case (try do
                :test_server_ctrl.add_module_with_skip(
                  name,
                  [suite],
                  skiplist(
                    testDir,
                    skip
                  )
                )
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            cleanUp

          _ ->
            case wait_for_idle() do
              :ok ->
                add_jobs(tests, skip, opts, [suite | cleanUp])

              _ ->
                cleanUp
            end
        end

      error ->
        error
    end
  end

  defp add_jobs([{testDir, suite, confs} | tests], skip, opts, cleanUp)
       when :erlang.element(1, hd(confs)) == :conf do
    group = fn conf ->
      :proplists.get_value(:name, :erlang.element(2, conf))
    end

    testCases = fn conf ->
      :erlang.element(4, conf)
    end

    tCTestName = fn
      :all ->
        ''

      [c] when is_atom(c) ->
        '.' ++ :erlang.atom_to_list(c)

      cs when is_list(cs) ->
        '.cases'
    end

    grTestName =
      case confs do
        [conf] ->
          case group.(conf) do
            grName when is_atom(grName) ->
              '.' ++ :erlang.atom_to_list(grName) ++ tCTestName.(testCases.(conf))

            _ ->
              '.groups' ++ tCTestName.(testCases.(conf))
          end

        _ ->
          '.groups'
      end

    testName = get_name(testDir) ++ '.' ++ :erlang.atom_to_list(suite) ++ grTestName

    case maybe_interpret(suite, :init_per_group, opts) do
      :ok ->
        case (try do
                :test_server_ctrl.add_conf_with_skip(
                  testName,
                  suite,
                  confs,
                  skiplist(testDir, skip)
                )
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            cleanUp

          _ ->
            case wait_for_idle() do
              :ok ->
                add_jobs(tests, skip, opts, [suite | cleanUp])

              _ ->
                cleanUp
            end
        end

      error ->
        error
    end
  end

  defp add_jobs([{testDir, suite, [case__]} | tests], skip, opts, cleanUp)
       when is_atom(case__) do
    add_jobs([{testDir, suite, case__} | tests], skip, opts, cleanUp)
  end

  defp add_jobs([{testDir, suite, cases} | tests], skip, opts, cleanUp)
       when is_list(cases) do
    cases1 =
      :lists.map(
        fn
          {groupName, _}
          when is_atom(groupName) ->
            groupName

          case__ ->
            case__
        end,
        cases
      )

    case maybe_interpret(suite, cases1, opts) do
      :ok ->
        name = get_name(testDir) ++ '.' ++ :erlang.atom_to_list(suite) ++ '.cases'

        case (try do
                :test_server_ctrl.add_cases_with_skip(
                  name,
                  suite,
                  cases1,
                  skiplist(testDir, skip)
                )
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            cleanUp

          _ ->
            case wait_for_idle() do
              :ok ->
                add_jobs(tests, skip, opts, [suite | cleanUp])

              _ ->
                cleanUp
            end
        end

      error ->
        error
    end
  end

  defp add_jobs([{testDir, suite, case__} | tests], skip, opts, cleanUp)
       when is_atom(case__) do
    case maybe_interpret(suite, case__, opts) do
      :ok ->
        name =
          get_name(testDir) ++
            '.' ++ :erlang.atom_to_list(suite) ++ '.' ++ :erlang.atom_to_list(case__)

        case (try do
                :test_server_ctrl.add_case_with_skip(name, suite, case__, skiplist(testDir, skip))
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            cleanUp

          _ ->
            case wait_for_idle() do
              :ok ->
                add_jobs(tests, skip, opts, [suite | cleanUp])

              _ ->
                cleanUp
            end
        end

      error ->
        error
    end
  end

  defp add_jobs([], _, _, cleanUp) do
    cleanUp
  end

  defp wait_for_idle() do
    :ct_util.update_last_run_index()

    notify = fn me, idleState ->
      send(me, {:idle, idleState})

      receive do
        {^me, :proceed} ->
          :ok
      after
        30000 ->
          :ok
      end
    end

    case (try do
            :test_server_ctrl.idle_notify(notify)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        :error

      tSPid ->
        ref = :erlang.monitor(:process, tSPid)

        result =
          receive do
            {:idle, :abort} ->
              :aborted

            {:idle, _} ->
              :ok

            {:DOWN, ^ref, _, _, _} ->
              :error
          end

        :erlang.demonitor(ref, [:flush])
        :ct_util.update_last_run_index()
        send(tSPid, {self(), :proceed})
        result
    end
  end

  defp skiplist(dir, [{dir, :all, cmt} | skip]) do
    ss = :filelib.wildcard(:filename.join(dir, '*_SUITE.beam'))

    for s <- ss do
      {:erlang.list_to_atom(:filename.basename(s, '.beam')), cmt}
    end ++ skiplist(dir, skip)
  end

  defp skiplist(dir, [{dir, s, cmt} | skip]) do
    [{s, cmt} | skiplist(dir, skip)]
  end

  defp skiplist(dir, [{dir, s, c, cmt} | skip]) do
    [{s, c, cmt} | skiplist(dir, skip)]
  end

  defp skiplist(dir, [_ | skip]) do
    skiplist(dir, skip)
  end

  defp skiplist(_Dir, []) do
    []
  end

  defp get_name(dir) do
    testDir =
      case :filename.basename(dir) do
        'test' ->
          :filename.dirname(dir)

        _ ->
          dir
      end

    base = :filename.basename(testDir)

    case :filename.basename(:filename.dirname(testDir)) do
      '' ->
        base

      topDir ->
        topDir ++ '.' ++ base
    end
  end

  def run_make(testDir, mod, userInclude) do
    run_make(:suites, testDir, mod, userInclude, [:nowarn_export_all])
  end

  defp run_make(targets, testDir0, mod, userInclude, cOpts)
       when is_list(mod) do
    run_make(targets, testDir0, :erlang.list_to_atom(mod), userInclude, cOpts)
  end

  defp run_make(targets, testDir0, mod, userInclude, cOpts) do
    case locate_test_dir(testDir0, mod) do
      {:ok, testDir} ->
        :ct_event.sync_notify(r_event(name: :start_make, node: node(), data: testDir))
        {:ok, cwd} = :file.get_cwd()
        :ok = :file.set_cwd(testDir)
        ctInclude = get_dir(:common_test, 'include')
        xmerlInclude = get_dir(:xmerl, 'include')
        erlFlags = userInclude ++ [{:i, ctInclude}, {:i, xmerlInclude}, :debug_info] ++ cOpts

        result =
          cond do
            mod == :all or targets == :helpmods ->
              case (try do
                      :ct_make.all([:noexec | erlFlags])
                    catch
                      :error, e -> {:EXIT, {e, __STACKTRACE__}}
                      :exit, e -> {:EXIT, e}
                      e -> e
                    end) do
                {:EXIT, _} = failure ->
                  failure

                makeInfo ->
                  fileTest = fn
                    f, :suites ->
                      is_suite(f)

                    f, :helpmods ->
                      not is_suite(f)
                  end

                  files =
                    :lists.flatmap(
                      fn
                        {f, :out_of_date} ->
                          case fileTest.(
                                 f,
                                 targets
                               ) do
                            true ->
                              [f]

                            false ->
                              []
                          end

                        _ ->
                          []
                      end,
                      makeInfo
                    )

                  try do
                    :ct_make.files(files, [:load | erlFlags])
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end
              end

            true ->
              try do
                :ct_make.files([mod], [:load | erlFlags])
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end
          end

        :ok = :file.set_cwd(cwd)
        :ct_event.notify(r_event(name: :finished_make, node: node(), data: testDir))

        case result do
          {:up_to_date, _} ->
            :ok

          {:EXIT, reason} ->
            :io.format('{error,{make_crashed,~tp}\n', [reason])
            {:error, {:make_crashed, testDir, reason}}

          {:error, modInfo} ->
            :io.format('{error,make_failed}\n', [])

            bad =
              for {m, r} <- modInfo, r == :error do
                :filename.join(testDir, m)
              end

            {:error, {:make_failed, bad}}
        end

      {:error, _} ->
        :io.format('{error,{invalid_directory,~tp}}\n', [testDir0])
        {:error, {:invalid_directory, testDir0}}
    end
  end

  defp get_dir(app, dir) do
    :filename.join(:code.lib_dir(app), dir)
  end

  defp maybe_interpret(suite, cases, r_opts(step: stepOpts))
       when stepOpts !== :undefined do
    case :ct_util.get_testdata(:interpret) do
      {_What, :kill, {tCPid, attPid}} ->
        :ct_util.kill_attached(tCPid, attPid)

      _ ->
        :ok
    end

    maybe_interpret1(suite, cases, stepOpts)
  end

  defp maybe_interpret(_, _, _) do
    :ok
  end

  defp maybe_interpret1(suite, :all, stepOpts) do
    case :i.ii(suite) do
      {:module, _} ->
        :i.iaa([:break])

        case get_all_testcases(suite) do
          {:error, _} ->
            {:error, :no_testcases_found}

          cases ->
            maybe_interpret2(suite, cases, stepOpts)
        end

      :error ->
        {:error, :could_not_interpret_module}
    end
  end

  defp maybe_interpret1(suite, case__, stepOpts) when is_atom(case__) do
    maybe_interpret1(suite, [case__], stepOpts)
  end

  defp maybe_interpret1(suite, cases, stepOpts) when is_list(cases) do
    case :i.ii(suite) do
      {:module, _} ->
        :i.iaa([:break])
        maybe_interpret2(suite, cases, stepOpts)

      :error ->
        {:error, :could_not_interpret_module}
    end
  end

  defp maybe_interpret2(suite, cases, stepOpts) do
    set_break_on_config(suite, stepOpts)

    _ =
      for case__ <- cases, is_atom(case__) do
        try do
          :i.ib(suite, case__, 1)
        catch
          _, _Error ->
            :io.format(:user, 'Invalid breakpoint: ~w:~tw/1~n', [suite, case__])
        else
          _ ->
            :ok
        end
      end

    :test_server_ctrl.multiply_timetraps(:infinity)

    winOp =
      case :lists.member(
             :keep_inactive,
             ensure_atom(stepOpts)
           ) do
        true ->
          :no_kill

        false ->
          :kill
      end

    :ct_util.set_testdata({:interpret, {{suite, cases}, winOp, {:undefined, :undefined}}})
    :ok
  end

  defp set_break_on_config(suite, stepOpts) do
    case :lists.member(:config, ensure_atom(stepOpts)) do
      true ->
        setBPIfExists = fn f, a ->
          case :erlang.function_exported(suite, f, a) do
            true ->
              :i.ib(suite, f, a)

            false ->
              :ok
          end
        end

        :ok = setBPIfExists.(:init_per_suite, 1)
        :ok = setBPIfExists.(:init_per_group, 2)
        :ok = setBPIfExists.(:init_per_testcase, 2)
        :ok = setBPIfExists.(:end_per_testcase, 2)
        :ok = setBPIfExists.(:end_per_group, 2)
        :ok = setBPIfExists.(:end_per_suite, 1)

      false ->
        :ok
    end
  end

  defp maybe_cleanup_interpret(_, :undefined) do
    :ok
  end

  defp maybe_cleanup_interpret(suite, _) do
    :i.iq(suite)
  end

  defp log_ts_names([]) do
    :ok
  end

  defp log_ts_names(specs) do
    list =
      :lists.map(
        fn name ->
          name ++ ' '
        end,
        specs
      )

    :ct_logs.log('Test Specification file(s)', '~ts', [:lists.flatten(list)])
  end

  defp merge_arguments(args) do
    merge_arguments(args, [])
  end

  defp merge_arguments([logDir = {:logdir, _} | args], merged) do
    merge_arguments(
      args,
      handle_arg(:replace, logDir, merged)
    )
  end

  defp merge_arguments([coverFile = {:cover, _} | args], merged) do
    merge_arguments(
      args,
      handle_arg(:replace, coverFile, merged)
    )
  end

  defp merge_arguments(
         [coverStop = {:cover_stop, _} | args],
         merged
       ) do
    merge_arguments(
      args,
      handle_arg(:replace, coverStop, merged)
    )
  end

  defp merge_arguments([{:case, tC} | args], merged) do
    merge_arguments(
      args,
      handle_arg(:merge, {:testcase, tC}, merged)
    )
  end

  defp merge_arguments([arg | args], merged) do
    merge_arguments(args, handle_arg(:merge, arg, merged))
  end

  defp merge_arguments([], merged) do
    merged
  end

  defp handle_arg(:replace, {key, elems}, [{key, _} | merged]) do
    [{key, elems} | merged]
  end

  defp handle_arg(:merge, {:event_handler_init, elems}, [
         {:event_handler_init, prevElems} | merged
       ]) do
    [
      {:event_handler_init, prevElems ++ ['add' | elems]}
      | merged
    ]
  end

  defp handle_arg(:merge, {:userconfig, elems}, [{:userconfig, prevElems} | merged]) do
    [{:userconfig, prevElems ++ ['add' | elems]} | merged]
  end

  defp handle_arg(:merge, {key, elems}, [{key, prevElems} | merged]) do
    [{key, prevElems ++ elems} | merged]
  end

  defp handle_arg(op, arg, [other | merged]) do
    [other | handle_arg(op, arg, merged)]
  end

  defp handle_arg(_, arg, []) do
    [arg]
  end

  defp get_start_opt(key, ifExists, args) do
    get_start_opt(key, ifExists, :undefined, args)
  end

  defp get_start_opt(key, ifExists, ifNotExists, args) do
    try do
      try_get_start_opt(key, ifExists, ifNotExists, args)
    catch
      :error, _ ->
        exit({:user_error, {:bad_argument, key}})
    else
      result ->
        result
    end
  end

  defp try_get_start_opt(key, ifExists, ifNotExists, args) do
    case :lists.keysearch(key, 1, args) do
      {:value, {^key, val}} when is_function(ifExists) ->
        ifExists.(val)

      {:value, {^key, val}} when ifExists == :value ->
        val

      {:value, {^key, _Val}} ->
        ifExists

      _ ->
        ifNotExists
    end
  end

  defp ct_hooks_args2opts(args) do
    :lists.foldl(
      fn
        {:ct_hooks, hooks}, acc ->
          ct_hooks_args2opts(hooks, acc)

        _, acc ->
          acc
      end,
      [],
      args
    )
  end

  defp ct_hooks_args2opts([[cTH, arg, prio, 'and'] | rest], acc)
       when arg != 'and' do
    ct_hooks_args2opts(
      rest,
      [
        {:erlang.list_to_atom(cTH), parse_cth_args(arg), parse_cth_args(prio)}
        | acc
      ]
    )
  end

  defp ct_hooks_args2opts([[cTH, arg, 'and'] | rest], acc) do
    ct_hooks_args2opts(
      rest,
      [
        {:erlang.list_to_atom(cTH), parse_cth_args(arg)}
        | acc
      ]
    )
  end

  defp ct_hooks_args2opts([cTH], acc) do
    ct_hooks_args2opts([cTH, 'and'], acc)
  end

  defp ct_hooks_args2opts([[cTH, 'and'] | rest], acc) do
    ct_hooks_args2opts(
      rest,
      [:erlang.list_to_atom(cTH) | acc]
    )
  end

  defp ct_hooks_args2opts([cTH, args], acc) do
    ct_hooks_args2opts([cTH, args, 'and'], acc)
  end

  defp ct_hooks_args2opts([cTH, args, prio], acc) do
    ct_hooks_args2opts([cTH, args, prio, 'and'], acc)
  end

  defp ct_hooks_args2opts([], acc) do
    :lists.reverse(acc)
  end

  defp parse_cth_args(string) do
    try do
      true = :io_lib.printable_unicode_list(string)
      {:ok, toks, _} = :erl_scan.string(string ++ '.')
      {:ok, args} = :erl_parse.parse_term(toks)
      args
    catch
      _, _ ->
        string
    end
  end

  defp event_handler_args2opts(args) do
    case :proplists.get_value(:event_handler, args) do
      :undefined ->
        event_handler_args2opts([], args)

      eHs ->
        event_handler_args2opts(
          for eH <- eHs do
            {:erlang.list_to_atom(eH), []}
          end,
          args
        )
    end
  end

  defp event_handler_args2opts(default, args) do
    case :proplists.get_value(
           :event_handler_init,
           args
         ) do
      :undefined ->
        default

      eHs ->
        event_handler_init_args2opts(eHs)
    end
  end

  defp event_handler_init_args2opts([[eH, arg, 'and'] | eHs]) do
    [
      {:erlang.list_to_atom(eH), :lists.flatten(:io_lib.format('~ts', [arg]))}
      | event_handler_init_args2opts(eHs)
    ]
  end

  defp event_handler_init_args2opts([eH, arg]) do
    [{:erlang.list_to_atom(eH), :lists.flatten(:io_lib.format('~ts', [arg]))}]
  end

  defp event_handler_init_args2opts([]) do
    []
  end

  defp verbosity_args2opts(args) do
    case :proplists.get_value(:verbosity, args) do
      :undefined ->
        []

      vArgs ->
        getVLvls = fn
          'and', {:new, soFar} when is_list(soFar) ->
            {:new, soFar}

          'and', {lvl, soFar} when is_list(soFar) ->
            {:new, [{:"$unspecified", :erlang.list_to_integer(lvl)} | soFar]}

          catOrLvl, {:new, soFar} when is_list(soFar) ->
            {catOrLvl, soFar}

          lvl, {cat, soFar} ->
            {:new,
             [
               {:erlang.list_to_atom(cat), :erlang.list_to_integer(lvl)}
               | soFar
             ]}
        end

        case :lists.foldl(getVLvls, {:new, []}, vArgs) do
          {:new, parsed} ->
            parsed

          {lvl, parsed} ->
            [{:"$unspecified", :erlang.list_to_integer(lvl)} | parsed]
        end
    end
  end

  defp add_verbosity_defaults(vLvls) do
    case {:proplists.get_value(:"$unspecified", vLvls), :proplists.get_value(:default, vLvls)} do
      {:undefined, :undefined} ->
        [{:default, 100}, {:"$unspecified", 100}] ++ vLvls

      {lvl, :undefined} ->
        [{:default, lvl} | vLvls]

      {:undefined, _Lvl} ->
        [{:"$unspecified", 100} | vLvls]

      _ ->
        vLvls
    end
  end

  defp rel_to_abs(ctArgs) do
    {pA, pZ} = get_pa_pz(ctArgs, [], [])

    _ =
      for d <- pZ do
        dir = rm_trailing_slash(d)
        abs = make_abs(dir)

        _ =
          cond do
            dir != abs ->
              _ = :code.del_path(dir)
              _ = :code.del_path(abs)

              :io.format(:user, 'Converting ~tp to ~tp and re-inserting with add_pathz/1~n', [
                dir,
                abs
              ])

            true ->
              _ = :code.del_path(dir)
          end

        :code.add_pathz(abs)
      end

    _ =
      for d <- pA do
        dir = rm_trailing_slash(d)
        abs = make_abs(dir)

        _ =
          cond do
            dir != abs ->
              _ = :code.del_path(dir)
              _ = :code.del_path(abs)

              :io.format(:user, 'Converting ~tp to ~tp and re-inserting with add_patha/1~n', [
                dir,
                abs
              ])

            true ->
              _ = :code.del_path(dir)
          end

        :code.add_patha(abs)
      end

    :io.format(:user, '~n', [])
  end

  defp rm_trailing_slash(dir) do
    :filename.join(:filename.split(dir))
  end

  defp get_pa_pz([{:pa, dirs} | args], pA, pZ) do
    get_pa_pz(args, pA ++ dirs, pZ)
  end

  defp get_pa_pz([{:pz, dirs} | args], pA, pZ) do
    get_pa_pz(args, pA, pZ ++ dirs)
  end

  defp get_pa_pz([_ | args], pA, pZ) do
    get_pa_pz(args, pA, pZ)
  end

  defp get_pa_pz([], pA, pZ) do
    {pA, pZ}
  end

  defp make_abs(relDir) do
    tokens = :filename.split(:filename.absname(relDir))
    :filename.join(:lists.reverse(make_abs1(tokens, [])))
  end

  defp make_abs1(['..' | dirs], [_Dir | path]) do
    make_abs1(dirs, path)
  end

  defp make_abs1(['.' | dirs], path) do
    make_abs1(dirs, path)
  end

  defp make_abs1([dir | dirs], path) do
    make_abs1(dirs, [dir | path])
  end

  defp make_abs1([], path) do
    path
  end

  defp opts2args(envStartOpts) do
    :lists.flatmap(
      fn
        {:exit_status, exitStatusOpt}
        when is_atom(exitStatusOpt) ->
          [{:exit_status, [:erlang.atom_to_list(exitStatusOpt)]}]

        {:halt_with, {haltM, haltF}} ->
          [{:halt_with, [:erlang.atom_to_list(haltM), :erlang.atom_to_list(haltF)]}]

        {:interactive_mode, true} ->
          [{:shell, []}]

        {:config, cfgFile} when is_integer(hd(cfgFile)) ->
          [{:ct_config, [cfgFile]}]

        {:config, cfgFiles} when is_list(hd(cfgFiles)) ->
          [{:ct_config, cfgFiles}]

        {:userconfig, {cBM, cfgStr = [x | _]}}
        when is_integer(x) ->
          [{:userconfig, [:erlang.atom_to_list(cBM), cfgStr]}]

        {:userconfig, {cBM, cfgStrs}} when is_list(cfgStrs) ->
          [{:userconfig, [:erlang.atom_to_list(cBM) | cfgStrs]}]

        {:userconfig, userCfg} when is_list(userCfg) ->
          strs =
            :lists.map(
              fn
                {cBM, cfgStr = [x | _]}
                when is_integer(x) ->
                  [:erlang.atom_to_list(cBM), cfgStr, 'and']

                {cBM, cfgStrs}
                when is_list(cfgStrs) ->
                  [
                    :erlang.atom_to_list(cBM)
                    | cfgStrs
                  ] ++ ['and']
              end,
              userCfg
            )

          [
            _LastAnd
            | strsR
          ] = :lists.reverse(:lists.flatten(strs))

          [{:userconfig, :lists.reverse(strsR)}]

        {:group, g} when is_atom(g) ->
          [{:group, [:erlang.atom_to_list(g)]}]

        {:group, gs} when is_list(gs) ->
          lOfGStrs =
            for g <- gs do
              :lists.flatten(:io_lib.format('~tw', [g]))
            end

          [{:group, lOfGStrs}]

        {:testcase, case__} when is_atom(case__) ->
          [{:case, [:erlang.atom_to_list(case__)]}]

        {:testcase, cases} ->
          [
            {:case,
             for c <- cases do
               :erlang.atom_to_list(c)
             end}
          ]

        {:case, cases} ->
          [
            {:case,
             for c <- cases do
               :erlang.atom_to_list(c)
             end}
          ]

        {:allow_user_terms, true} ->
          [{:allow_user_terms, []}]

        {:allow_user_terms, false} ->
          []

        {:join_specs, true} ->
          [{:join_specs, []}]

        {:join_specs, false} ->
          []

        {:auto_compile, false} ->
          [{:no_auto_compile, []}]

        {:auto_compile, true} ->
          []

        {:scale_timetraps, true} ->
          [{:scale_timetraps, []}]

        {:scale_timetraps, false} ->
          []

        {:create_priv_dir, :auto_per_run} ->
          []

        {:create_priv_dir, pD} when is_atom(pD) ->
          [{:create_priv_dir, [:erlang.atom_to_list(pD)]}]

        {:force_stop, :skip_rest} ->
          [{:force_stop, ['skip_rest']}]

        {:force_stop, true} ->
          [{:force_stop, []}]

        {:force_stop, false} ->
          []

        {:decrypt, {:key, key}} ->
          [{:ct_decrypt_key, [key]}]

        {:decrypt, {:file, file}} ->
          [{:ct_decrypt_file, [file]}]

        {:basic_html, true} ->
          [{:basic_html, []}]

        {:basic_html, false} ->
          []

        {:esc_chars, false} ->
          [{:no_esc_chars, []}]

        {:esc_chars, true} ->
          []

        {:event_handler, eH} when is_atom(eH) ->
          [{:event_handler, [:erlang.atom_to_list(eH)]}]

        {:event_handler, eHs} when is_list(eHs) ->
          [
            {:event_handler,
             for eH <- eHs do
               :erlang.atom_to_list(eH)
             end}
          ]

        {:event_handler, {eH, arg}} when is_atom(eH) ->
          argStr = :lists.flatten(:io_lib.format('~tp', [arg]))
          [{:event_handler_init, [:erlang.atom_to_list(eH), argStr]}]

        {:event_handler, {eHs, arg}} when is_list(eHs) ->
          argStr = :lists.flatten(:io_lib.format('~tp', [arg]))

          strs =
            :lists.flatmap(
              fn eH ->
                [:erlang.atom_to_list(eH), argStr, 'and']
              end,
              eHs
            )

          [_LastAnd | strsR] = :lists.reverse(strs)
          [{:event_handler_init, :lists.reverse(strsR)}]

        {:logopts, lOs} when is_list(lOs) ->
          [
            {:logopts,
             for lO <- lOs do
               :erlang.atom_to_list(lO)
             end}
          ]

        {:verbosity, [{:default, 100}, {:"$unspecified", 100}]} ->
          []

        {:verbosity, vLvl} when is_integer(vLvl) ->
          [{:verbosity, [:erlang.integer_to_list(vLvl)]}]

        {:verbosity, vLvls} when is_list(vLvls) ->
          vLvlArgs =
            :lists.flatmap(
              fn
                {:"$unspecified", lvl} ->
                  [:erlang.integer_to_list(lvl), 'and']

                {cat, lvl} ->
                  [:erlang.atom_to_list(cat), :erlang.integer_to_list(lvl), 'and']

                lvl ->
                  [:erlang.integer_to_list(lvl), 'and']
              end,
              vLvls
            )

          [_LastAnd | vLvlArgsR] = :lists.reverse(vLvlArgs)
          [{:verbosity, :lists.reverse(vLvlArgsR)}]

        {:ct_hooks, []} ->
          []

        {:ct_hooks, cTHs} when is_list(cTHs) ->
          :io.format(:user, 'ct_hooks: ~tp', [cTHs])

          strs =
            :lists.flatmap(
              fn
                {cTH, arg, prio} ->
                  [
                    :erlang.atom_to_list(cTH),
                    :lists.flatten(
                      :io_lib.format(
                        '~tp',
                        [arg]
                      )
                    ),
                    :lists.flatten(
                      :io_lib.format(
                        '~tp',
                        [prio]
                      )
                    ),
                    'and'
                  ]

                {cTH, arg} ->
                  [
                    :erlang.atom_to_list(cTH),
                    :lists.flatten(
                      :io_lib.format(
                        '~tp',
                        [arg]
                      )
                    ),
                    'and'
                  ]

                cTH when is_atom(cTH) ->
                  [:erlang.atom_to_list(cTH), 'and']
              end,
              cTHs
            )

          [_LastAnd | strsR] = :lists.reverse(strs)
          :io.format(:user, 'return: ~tp', [:lists.reverse(strsR)])
          [{:ct_hooks, :lists.reverse(strsR)}]

        {opt, as = [a | _]} when is_atom(a) ->
          [
            {opt,
             for atom <- as do
               :erlang.atom_to_list(atom)
             end}
          ]

        {opt, strs = [s | _]} when is_list(s) ->
          [{opt, strs}]

        {opt, a} when is_atom(a) ->
          [{opt, [:erlang.atom_to_list(a)]}]

        {opt, i} when is_integer(i) ->
          [{opt, [:erlang.integer_to_list(i)]}]

        {opt, s} when is_list(s) ->
          [{opt, [s]}]

        opt ->
          opt
      end,
      envStartOpts
    )
  end

  defp locate_test_dir(dir, suite) do
    testDir =
      case :ct_util.is_test_dir(dir) do
        true ->
          dir

        false ->
          :ct_util.get_testdir(dir, suite)
      end

    case :filelib.is_dir(testDir) do
      true ->
        {:ok, testDir}

      false ->
        {:error, :invalid}
    end
  end

  defp is_suite(mod) when is_atom(mod) do
    is_suite(:erlang.atom_to_list(mod))
  end

  defp is_suite(modOrFile) when is_list(modOrFile) do
    case :lists.reverse(
           :filename.basename(
             modOrFile,
             '.erl'
           )
         ) do
      [[?E, ?T, ?I, ?U, ?S, ?_] | _] ->
        true

      _ ->
        case :lists.reverse(
               :filename.basename(
                 modOrFile,
                 '.beam'
               )
             ) do
          [[?E, ?T, ?I, ?U, ?S, ?_] | _] ->
            true

          _ ->
            false
        end
    end
  end

  defp get_all_testcases(suite) do
    try do
      :ct_framework.get_all_cases(suite)
    catch
      _, error ->
        {:error, error}
    else
      {:error, _Reason} = error ->
        error

      suiteCases ->
        cases =
          for {_S, c} <- suiteCases do
            c
          end

        try do
          suite.sequences()
        catch
          _, _ ->
            cases
        else
          [] ->
            cases

          seqs ->
            tCs1 =
              :lists.flatten(
                for {_, tCs} <- seqs do
                  tCs
                end
              )

            :lists.reverse(
              :lists.foldl(
                fn tC, acc ->
                  case :lists.member(tC, acc) do
                    true ->
                      acc

                    false ->
                      [tC | acc]
                  end
                end,
                [],
                cases ++ tCs1
              )
            )
        end
    end
  end

  defp start_trace(args) do
    case :lists.keysearch(:ct_trace, 1, args) do
      {:value, {:ct_trace, file}} ->
        traceSpec = delistify(file)

        case :file.consult(traceSpec) do
          {:ok, terms} ->
            case (try do
                    do_trace(terms)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              :ok ->
                true

              {_, error} ->
                :io.format('Warning! Tracing not started. Reason: ~tp~n~n', [error])
                false
            end

          {_, error} ->
            :io.format('Warning! Tracing not started. Reason: ~ts~n~n', [
              :file.format_error(error)
            ])

            false
        end

      false ->
        false
    end
  end

  defp do_trace(terms) do
    :dbg.tracer()
    :dbg.p(self(), [:sos, :call])

    :lists.foreach(
      fn
        {:m, m} ->
          case :dbg.tpl(m, :x) do
            {:error, what} ->
              exit({:error, {:tracing_failed, what}})

            _ ->
              :ok
          end

        {:me, m} ->
          case :dbg.tp(
                 m,
                 [{:_, [], [{:exception_trace}, {:message, {:caller}}]}]
               ) do
            {:error, what} ->
              exit({:error, {:tracing_failed, what}})

            _ ->
              :ok
          end

        {:f, m, f} ->
          case :dbg.tpl(m, f, [{:_, [], [{:exception_trace}, {:message, {:caller}}]}]) do
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
  end

  defp stop_trace(true) do
    :dbg.stop_clear()
  end

  defp stop_trace(false) do
    :ok
  end

  defp ensure_atom(atom) when is_atom(atom) do
    atom
  end

  defp ensure_atom(string)
       when is_list(string) and
              is_integer(hd(string)) do
    :erlang.list_to_atom(string)
  end

  defp ensure_atom(list) when is_list(list) do
    for item <- list do
      ensure_atom(item)
    end
  end

  defp ensure_atom(other) do
    other
  end
end
