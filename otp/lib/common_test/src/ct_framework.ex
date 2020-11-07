defmodule :m_ct_framework do
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

  def init_tc(_, {:end_per_testcase_not_run, _}, [config]) do
    {:ok, [config]}
  end

  def init_tc(mod, ePTC = {:end_per_testcase, _}, [config]) do
    suite = get_suite_name(mod, config)

    case :ct_hooks.init_tc(suite, ePTC, config) do
      newConfig when is_list(newConfig) ->
        {:ok, [newConfig]}

      other ->
        other
    end
  end

  def init_tc(mod, func0, args) do
    suite = get_suite_name(mod, args)

    {func, hookFunc} =
      case func0 do
        {:init_per_testcase, f} ->
          {f, func0}

        _ ->
          {func0, func0}
      end

    case :ct_util.get_testdata(:interpret) do
      {what, :kill, {tCPid, attPid}} ->
        :ct_util.kill_attached(tCPid, attPid)
        :ct_util.set_testdata({:interpret, {what, :kill, {:undefined, :undefined}}})

      _ ->
        :ok
    end

    case func !== :end_per_suite and func !== :end_per_group and :ct_util.get_testdata(:skip_rest) do
      true ->
        initialize(false, mod, func, args)
        {:auto_skip, 'Repeated test stopped by force_stop option'}

      _ ->
        case :ct_util.get_testdata(:curr_tc) do
          {^suite, {:suite0_failed, {:require, reason}}} ->
            initialize(false, mod, func, args)
            {:auto_skip, {:require_failed_in_suite0, reason}}

          {^suite, {:suite0_failed, _} = failure} ->
            initialize(false, mod, func, args)
            {:fail, failure}

          _ ->
            :ct_util.update_testdata(
              :curr_tc,
              fn
                :undefined ->
                  [{suite, func}]

                running ->
                  [{suite, func} | running]
              end,
              [:create]
            )

            case :ct_util.read_suite_data({:seq, suite, func}) do
              :undefined ->
                init_tc1(mod, suite, func, hookFunc, args)

              seq when is_atom(seq) ->
                case :ct_util.read_suite_data({:seq, suite, seq}) do
                  [^func | tCs] ->
                    :lists.foreach(
                      fn tC ->
                        :ct_util.save_suite_data(
                          {:seq, suite, tC},
                          seq
                        )
                      end,
                      tCs
                    )

                  _ ->
                    :ok
                end

                init_tc1(mod, suite, func, hookFunc, args)

              {:failed, seq, badFunc} ->
                initialize(false, mod, func, args)
                {:auto_skip, {:sequence_failed, seq, badFunc}}
            end
        end
    end
  end

  defp init_tc1(:ct_framework, _, :error_in_suite, _, [config0])
       when is_list(config0) do
    initialize(false, :ct_framework, :error_in_suite)
    _ = ct_suite_init(:ct_framework, :error_in_suite, [], config0)

    case :proplists.get_value(:error, config0) do
      :undefined ->
        {:fail, 'unknown_error_in_suite'}

      reason ->
        {:fail, reason}
    end
  end

  defp init_tc1(mod, suite, func, hookFunc, [config0])
       when is_list(config0) do
    config1 =
      case :ct_util.read_suite_data(:last_saved_config) do
        {{^suite, lastFunc}, savedConfig} ->
          [
            {:saved_config, {lastFunc, savedConfig}}
            | :lists.keydelete(:saved_config, 1, config0)
          ]

        {{lastSuite, initOrEnd}, savedConfig}
        when initOrEnd == :init_per_suite or
               initOrEnd == :end_per_suite ->
          [
            {:saved_config, {lastSuite, savedConfig}}
            | :lists.keydelete(:saved_config, 1, config0)
          ]

        :undefined ->
          :lists.keydelete(:saved_config, 1, config0)
      end

    :ct_util.delete_suite_data(:last_saved_config)
    config = :lists.keydelete(:watchdog, 1, config1)

    cond do
      func == :init_per_suite ->
        :ct_config.delete_default_config(:suite)
        :ct_config.release_allocated()

      func != :init_per_suite ->
        :ok
    end

    groupPath = :proplists.get_value(:tc_group_path, config, [])

    allGroups = [
      :proplists.get_value(:tc_group_properties, config, [])
      | groupPath
    ]

    funcSpec = group_or_func(func, config0)

    hookFunc1 =
      cond do
        is_tuple(funcSpec) ->
          funcSpec

        true ->
          :ct_config.delete_default_config(:testcase)
          hookFunc
      end

    case add_defaults(mod, func, allGroups) do
      error = {:suite0_failed, _} ->
        initialize(false, mod, funcSpec)
        :ct_util.set_testdata({:curr_tc, {suite, error}})
        {:error, error}

      error = {:group0_failed, _} ->
        initialize(false, mod, funcSpec)
        {:auto_skip, error}

      error = {:testcase0_failed, _} ->
        initialize(false, mod, funcSpec)
        {:auto_skip, error}

      {suiteInfo, mergeResult} ->
        case mergeResult do
          {:error, reason} ->
            initialize(false, mod, funcSpec)
            {:fail, reason}

          _ ->
            init_tc2(mod, suite, func, hookFunc1, suiteInfo, mergeResult, config)
        end
    end
  end

  defp init_tc1(_Mod, _Suite, _Func, _HookFunc, args) do
    {:ok, args}
  end

  defp init_tc2(mod, suite, func, hookFunc, suiteInfo, mergeResult, config) do
    mergedInfo = timetrap_first(mergeResult, [], [])

    _ =
      case :lists.keysearch(:stylesheet, 1, mergeResult ++ config) do
        {:value, {:stylesheet, sSFile}} ->
          :ct_logs.set_stylesheet(
            func,
            add_data_dir(sSFile, config)
          )

        _ ->
          case :ct_util.get_testdata(:stylesheet) do
            :undefined ->
              :ct_logs.clear_stylesheet(func)

            sSFile ->
              :ct_logs.set_stylesheet(func, sSFile)
          end
      end

    case :ct_util.get_overridden_silenced_connections() do
      :undefined ->
        case :lists.keysearch(:silent_connections, 1, mergeResult ++ config) do
          {:value, {:silent_connections, conns}} ->
            :ct_util.silence_connections(conns)

          _ ->
            :ok
        end

      conns ->
        :ct_util.silence_connections(conns)
    end

    funcSpec = group_or_func(func, config)
    initialize(func == :init_per_suite, mod, funcSpec)

    case (try do
            configure(mergedInfo, mergedInfo, suiteInfo, funcSpec, [], config)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:suite0_failed, reason} ->
        :ct_util.set_testdata({:curr_tc, {mod, {:suite0_failed, {:require, reason}}}})
        {:auto_skip, {:require_failed_in_suite0, reason}}

      {:error, reason} ->
        {:auto_skip, {:require_failed, reason}}

      {:EXIT, reason} ->
        {:fail, reason}

      {:ok, postInitHook, config1} ->
        case :erlang.get(:"$test_server_framework_test") do
          :undefined ->
            ct_suite_init(suite, hookFunc, postInitHook, config1)

          fun ->
            postInitHookResult =
              do_post_init_hook(
                postInitHook,
                config1
              )

            case fun.(
                   :init_tc,
                   [postInitHookResult ++ config1]
                 ) do
              newConfig when is_list(newConfig) ->
                {:ok, newConfig}

              else__ ->
                else__
            end
        end
    end
  end

  defp initialize(refreshLogs, mod, func, [config])
       when is_list(config) do
    initialize(refreshLogs, mod, group_or_func(func, config))
  end

  defp initialize(refreshLogs, mod, func, _) do
    initialize(refreshLogs, mod, func)
  end

  defp initialize(refreshLogs, mod, funcSpec) do
    :ct_logs.init_tc(refreshLogs)
    :ct_event.notify(r_event(name: :tc_start, node: node(), data: {mod, funcSpec}))
  end

  defp ct_suite_init(suite, hookFunc, postInitHook, config)
       when is_list(config) do
    case :ct_hooks.init_tc(suite, hookFunc, config) do
      newConfig when is_list(newConfig) ->
        postInitHookResult =
          do_post_init_hook(
            postInitHook,
            newConfig
          )

        {:ok, [postInitHookResult ++ newConfig]}

      else__ ->
        else__
    end
  end

  defp do_post_init_hook(postInitHook, config) do
    :lists.flatmap(
      fn {tag, fun} ->
        case :lists.keysearch(tag, 1, config) do
          {:value, _} ->
            []

          false ->
            case fun.() do
              {:error, _} ->
                []

              result ->
                [{tag, result}]
            end
        end
      end,
      postInitHook
    )
  end

  defp add_defaults(mod, func, groupPath) do
    suite = get_suite_name(mod, groupPath)

    case (try do
            suite.suite()
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, {:undef, _}} ->
        suiteInfo = merge_with_suite_defaults(suite, [])

        suiteInfoNoCTH =
          for i <- suiteInfo,
              :erlang.element(1, i) !== :ct_hooks do
            i
          end

        case add_defaults1(mod, func, groupPath, suiteInfoNoCTH) do
          error = {:group0_failed, _} ->
            error

          error = {:testcase0_failed, _} ->
            error

          error = {:error, _} ->
            {suiteInfo, error}

          mergedInfo ->
            {suiteInfo, mergedInfo}
        end

      {:EXIT, reason} ->
        errStr = :io_lib.format('~n*** ERROR *** ~w:suite/0 failed: ~tp~n', [suite, reason])
        :io.format('~ts', [errStr])
        :io.format(:ct_default_gl, '~ts', [errStr])
        {:suite0_failed, {:exited, reason}}

      suiteInfo when is_list(suiteInfo) ->
        case :lists.all(
               fn
                 e when is_tuple(e) ->
                   true

                 _ ->
                   false
               end,
               suiteInfo
             ) do
          true ->
            suiteInfo1 = merge_with_suite_defaults(suite, suiteInfo)

            suiteInfoNoCTH =
              for i <- suiteInfo1,
                  :erlang.element(1, i) !== :ct_hooks do
                i
              end

            case add_defaults1(mod, func, groupPath, suiteInfoNoCTH) do
              error = {:group0_failed, _} ->
                error

              error = {:testcase0_failed, _} ->
                error

              error = {:error, _} ->
                {suiteInfo1, error}

              mergedInfo ->
                {suiteInfo1, mergedInfo}
            end

          false ->
            errStr =
              :io_lib.format('~n*** ERROR *** Invalid return value from ~w:suite/0: ~tp~n', [
                suite,
                suiteInfo
              ])

            :io.format('~ts', [errStr])
            :io.format(:ct_default_gl, '~ts', [errStr])
            {:suite0_failed, :bad_return_value}
        end

      suiteInfo ->
        errStr =
          :io_lib.format('~n*** ERROR *** Invalid return value from ~w:suite/0: ~tp~n', [
            suite,
            suiteInfo
          ])

        :io.format('~ts', [errStr])
        :io.format(:ct_default_gl, '~ts', [errStr])
        {:suite0_failed, :bad_return_value}
    end
  end

  defp add_defaults1(mod, func, groupPath, suiteInfo) do
    suite = get_suite_name(mod, groupPath)

    groupPathInfo =
      :lists.map(
        fn groupProps ->
          case :proplists.get_value(
                 :name,
                 groupProps
               ) do
            :undefined ->
              []

            name ->
              case (try do
                      suite.group(name)
                    catch
                      :error, e -> {:EXIT, {e, __STACKTRACE__}}
                      :exit, e -> {:EXIT, e}
                      e -> e
                    end) do
                grInfo when is_list(grInfo) ->
                  grInfo

                {:EXIT, {:undef, _}} ->
                  []

                badGr0 ->
                  {:error, badGr0, name}
              end
          end
        end,
        groupPath
      )

    case :lists.keysearch(:error, 1, groupPathInfo) do
      {:value, {:error, badGr0Val, grName}} ->
        gr0ErrStr =
          :io_lib.format('~n*** ERROR *** Invalid return value from ~w:group(~tw): ~tp~n', [
            mod,
            grName,
            badGr0Val
          ])

        :io.format('~ts', [gr0ErrStr])
        :io.format(:ct_default_gl, '~ts', [gr0ErrStr])
        {:group0_failed, :bad_return_value}

      _ ->
        args =
          cond do
            func == :init_per_group or func == :end_per_group ->
              [:proplists.get_value(:name, hd(groupPath))]

            true ->
              []
          end

        testCaseInfo =
          case (try do
                  apply(mod, func, args)
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            tCInfo when is_list(tCInfo) ->
              tCInfo

            {:EXIT, {:undef, _}} ->
              []

            badTC0 ->
              {:error, badTC0}
          end

        case testCaseInfo do
          {:error, badTC0Val} ->
            tC0ErrStr =
              :io_lib.format('~n*** ERROR *** Invalid return value from ~w:~tw/0: ~tp~n', [
                mod,
                func,
                badTC0Val
              ])

            :io.format('~ts', [tC0ErrStr])
            :io.format(:ct_default_gl, '~ts', [tC0ErrStr])
            {:testcase0_failed, :bad_return_value}

          _ ->
            tCAndGroupInfo = [
              testCaseInfo
              | remove_info_in_prev(
                  testCaseInfo,
                  groupPathInfo
                )
            ]

            suiteReqs =
              for sDDef <- suiteInfo,
                  :erlang.or(
                    :require == :erlang.element(1, sDDef),
                    :default_config ==
                      :erlang.element(
                        1,
                        sDDef
                      )
                  ) do
                sDDef
              end

            case check_for_clashes(testCaseInfo, groupPathInfo, suiteReqs) do
              [] ->
                add_defaults2(mod, func, tCAndGroupInfo, suiteInfo, suiteReqs)

              clashes ->
                {:error, {:config_name_already_in_use, clashes}}
            end
        end
    end
  end

  defp get_suite_name(:ct_framework, [cfg | _])
       when is_list(cfg) and
              cfg != [] do
    get_suite_name(:ct_framework, cfg)
  end

  defp get_suite_name(:ct_framework, cfg)
       when is_list(cfg) and
              cfg != [] do
    case :proplists.get_value(
           :tc_group_properties,
           cfg
         ) do
      :undefined ->
        case :proplists.get_value(:suite, cfg) do
          :undefined ->
            :ct_framework

          suite ->
            suite
        end

      grProps ->
        case :proplists.get_value(:suite, grProps) do
          :undefined ->
            :ct_framework

          suite ->
            suite
        end
    end
  end

  defp get_suite_name(mod, _) do
    mod
  end

  defp check_for_clashes(tCInfo, [currGrInfo | path], suiteInfo) do
    reqNames = fn info ->
      for r <- info, :erlang.size(r) == 3, :require == :erlang.element(1, r) do
        :erlang.element(2, r)
      end
    end

    existingNames =
      :lists.flatten(
        for l <- [
              suiteInfo
              | path
            ] do
          reqNames.(l)
        end
      )

    currGrReqNs = reqNames.(currGrInfo)

    grClashes =
      for name <- currGrReqNs,
          true == :lists.member(name, existingNames) do
        name
      end

    allReqNs = currGrReqNs ++ existingNames

    tCClashes =
      for name <- reqNames.(tCInfo),
          true == :lists.member(name, allReqNs) do
        name
      end

    tCClashes ++ grClashes
  end

  defp remove_info_in_prev(terms, [[] | rest]) do
    [[] | remove_info_in_prev(terms, rest)]
  end

  defp remove_info_in_prev(terms, [info | rest]) do
    uniqueInInfo =
      for u <- info,
          :erlang.or(
            :erlang.or(
              :erlang.and(
                :timetrap ==
                  :erlang.element(
                    1,
                    u
                  ),
                not :lists.keymember(
                  :timetrap,
                  1,
                  terms
                )
              ),
              :erlang.and(
                :require ==
                  :erlang.element(
                    1,
                    u
                  ),
                not :lists.member(
                  u,
                  terms
                )
              )
            ),
            :erlang.and(
              :default_config ==
                :erlang.element(
                  1,
                  u
                ),
              not keysmember(
                [
                  :default_config,
                  1,
                  :erlang.element(
                    2,
                    u
                  ),
                  2
                ],
                terms
              )
            )
          ) do
        u
      end

    otherTermsInInfo =
      for t <- info,
          :timetrap != :erlang.element(1, t),
          :require != :erlang.element(1, t),
          :default_config != :erlang.element(1, t),
          false == :lists.keymember(:erlang.element(1, t), 1, terms) do
        t
      end

    keptInfo = uniqueInInfo ++ otherTermsInInfo

    [
      keptInfo
      | remove_info_in_prev(
          terms ++ keptInfo,
          rest
        )
    ]
  end

  defp remove_info_in_prev(_, []) do
    []
  end

  defp keysmember([[key, pos] | next], list) do
    case (for elem <- list,
              key == :erlang.element(pos, elem) do
            elem
          end) do
      [] ->
        false

      found ->
        keysmember(next, found)
    end
  end

  defp keysmember([], _) do
    true
  end

  defp add_defaults2(_Mod, :init_per_suite, iPSInfo, suiteInfo, suiteReqs) do
    info = :lists.flatten([iPSInfo, suiteReqs])
    :lists.flatten([info, remove_info_in_prev(info, [suiteInfo])])
  end

  defp add_defaults2(_Mod, :init_per_group, iPGAndGroupInfo, suiteInfo, suiteReqs) do
    suiteInfo1 =
      remove_info_in_prev(
        :lists.flatten([iPGAndGroupInfo, suiteReqs]),
        [suiteInfo]
      )

    case iPGAndGroupInfo do
      [iPGInfo] ->
        :lists.flatten([iPGInfo, suiteInfo1])

      [[iPGInfo, currGroupInfo] | prevGroupInfo] ->
        prevGroupInfo1 = delete_require_terms(prevGroupInfo)
        :lists.flatten([iPGInfo, currGroupInfo, prevGroupInfo1, suiteInfo1])
    end
  end

  defp add_defaults2(_Mod, _Func, tCAndGroupInfo, suiteInfo, suiteReqs) do
    suiteInfo1 =
      remove_info_in_prev(
        :lists.flatten([tCAndGroupInfo, suiteReqs]),
        [suiteInfo]
      )

    case tCAndGroupInfo do
      [tCInfo] ->
        :lists.flatten([tCInfo, suiteInfo1])

      [[tCInfo, currGroupInfo] | prevGroupInfo] ->
        prevGroupInfo1 = delete_require_terms(prevGroupInfo)
        :lists.flatten([tCInfo, currGroupInfo, prevGroupInfo1, suiteInfo1])
    end
  end

  defp delete_require_terms([info | prev]) do
    info1 =
      for t <- info,
          :require != :erlang.element(1, t),
          :default_config != :erlang.element(1, t) do
        t
      end

    [info1 | delete_require_terms(prev)]
  end

  defp delete_require_terms([]) do
    []
  end

  defp merge_with_suite_defaults(mod, suiteInfo) do
    case :lists.keysearch(:suite_defaults, 1, mod.module_info(:attributes)) do
      {:value, {:suite_defaults, defaults}} ->
        sDReqs =
          for sDDef <- defaults,
              :require == :erlang.element(1, sDDef),
              false == :lists.keymember(:erlang.element(2, sDDef), 2, suiteInfo) do
            sDDef
          end

        suiteInfo ++
          sDReqs ++
          for sDDef <- defaults,
              :require != :erlang.element(1, sDDef),
              false ==
                :lists.keymember(
                  :erlang.element(
                    1,
                    sDDef
                  ),
                  1,
                  suiteInfo
                ) do
            sDDef
          end

      false ->
        suiteInfo
    end
  end

  defp timetrap_first([trap = {:timetrap, _} | rest], info, found) do
    timetrap_first(rest, info, [trap | found])
  end

  defp timetrap_first([other | rest], info, found) do
    timetrap_first(rest, [other | info], found)
  end

  defp timetrap_first([], info, []) do
    [{:timetrap, {:minutes, 30}} | :lists.reverse(info)]
  end

  defp timetrap_first([], info, found) do
    :lists.reverse(found) ++ :lists.reverse(info)
  end

  defp configure([{:require, required} | rest], info, suiteInfo, scope, postInitHook, config) do
    case :ct.require(required) do
      :ok ->
        configure(rest, info, suiteInfo, scope, postInitHook, config)

      error = {:error, reason} ->
        case required_default(:_UNDEF, required, info, suiteInfo, scope) do
          :ok ->
            configure(rest, info, suiteInfo, scope, postInitHook, config)

          _ ->
            case :lists.keymember(required, 2, suiteInfo) do
              true ->
                {:suite0_failed, reason}

              false ->
                error
            end
        end
    end
  end

  defp configure(
         [{:require, name, required} | rest],
         info,
         suiteInfo,
         scope,
         postInitHook,
         config
       ) do
    case :ct.require(name, required) do
      :ok ->
        configure(rest, info, suiteInfo, scope, postInitHook, config)

      error = {:error, reason} ->
        case required_default(name, required, info, suiteInfo, scope) do
          :ok ->
            configure(rest, info, suiteInfo, scope, postInitHook, config)

          _ ->
            case :lists.keymember(name, 2, suiteInfo) do
              true ->
                {:suite0_failed, reason}

              false ->
                error
            end
        end
    end
  end

  defp configure([{:timetrap, :off} | rest], info, suiteInfo, scope, postInitHook, config) do
    configure(rest, info, suiteInfo, scope, postInitHook, config)
  end

  defp configure([{:timetrap, time} | rest], info, suiteInfo, scope, postInitHook, config) do
    postInitHook1 = [
      {:watchdog,
       fn ->
         case :test_server.get_timetrap_info() do
           :undefined ->
             :test_server.timetrap(time)

           _ ->
             {:error, :already_set}
         end
       end}
      | postInitHook
    ]

    configure(rest, info, suiteInfo, scope, postInitHook1, config)
  end

  defp configure([{:ct_hooks, hook} | rest], info, suiteInfo, scope, postInitHook, config) do
    configure(rest, info, suiteInfo, scope, postInitHook, [{:ct_hooks, hook} | config])
  end

  defp configure([_ | rest], info, suiteInfo, scope, postInitHook, config) do
    configure(rest, info, suiteInfo, scope, postInitHook, config)
  end

  defp configure([], _, _, _, postInitHook, config) do
    {:ok, postInitHook, config}
  end

  defp required_default(name, key, info, _, :init_per_suite) do
    try_set_default(name, key, info, :suite)
  end

  defp required_default(name, key, info, _, {:init_per_group, grName, _}) do
    try_set_default(name, key, info, {:group, grName})
  end

  defp required_default(name, key, info, _, _FuncSpec) do
    try_set_default(name, key, info, :testcase)
  end

  defp try_set_default(name, key, info, where) do
    cfgElems =
      case :lists.keysearch(name, 1, info) do
        {:value, {^name, val}} ->
          [val]

        false ->
          case (try do
                  for elem <- info,
                      :erlang.element(1, elem) == :default_config,
                      :erlang.element(2, elem) == key do
                    {key, :erlang.element(3, elem)}
                  end
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            {:EXIT, _} ->
              []

            result ->
              result
          end
      end

    case {name, cfgElems} do
      {_, []} ->
        :no_default

      {:_UNDEF, _} ->
        _ =
          for cfgVal <- cfgElems do
            :ct_config.set_default_config([cfgVal], where)
          end

        :ok

      _ ->
        _ =
          for cfgVal <- cfgElems do
            :ct_config.set_default_config(name, [cfgVal], where)
          end

        :ok
    end
  end

  def end_tc(mod, fun, args) do
    end_tc(mod, fun, args, :"$end_tc_dummy")
  end

  def end_tc(:ct_framework, :error_in_suite, {result, [args]}, return) do
    finalNotify =
      case :ct_hooks.end_tc(:ct_framework, :error_in_suite, args, result, return) do
        :"$ct_no_change" ->
          result

        hookResult ->
          hookResult
      end

    event =
      r_event(
        name: :tc_done,
        node: node(),
        data: {:ct_framework, :error_in_suite, tag(finalNotify)}
      )

    :ct_event.sync_notify(event)
    :ok
  end

  def end_tc(mod, func, {tCPid, result, [args]}, return)
      when is_pid(tCPid) do
    end_tc(mod, func, tCPid, result, args, return)
  end

  def end_tc(mod, func, {result, [args]}, return) do
    end_tc(mod, func, self(), result, args, return)
  end

  defp end_tc(mod, iPTC = {:init_per_testcase, _Func}, _TCPid, result, args, return) do
    case end_hook_func(iPTC, return, iPTC) do
      :undefined ->
        :ok

      _ ->
        suite = get_suite_name(mod, args)

        case :ct_hooks.end_tc(suite, iPTC, args, result, return) do
          :"$ct_no_change" ->
            :ok

          hookResult ->
            hookResult
        end
    end
  end

  defp end_tc(mod, func00, tCPid, result, args, return) do
    suite = get_suite_name(mod, args)

    {onlyCleanup, func0} =
      case func00 do
        {:cleanup, f0} ->
          {true, f0}

        _ ->
          {false, func00}
      end

    {func, funcSpec, hookFunc} =
      case func0 do
        {:end_per_testcase_not_run, f} ->
          {f, f, :undefined}

        {:end_per_testcase, f} ->
          {f, f, func0}

        _ ->
          fS = group_or_func(func0, args)
          hF = end_hook_func(func0, return, fS)
          {func0, fS, hF}
      end

    :test_server.timetrap_cancel()

    case :ct_util.get_testdata(:interpret) do
      {what, :kill, _} ->
        attPid = :ct_util.get_attached(self())
        :ct_util.set_testdata({:interpret, {what, :kill, {self(), attPid}}})

      _ ->
        :ok
    end

    cond do
      func == :end_per_group or func == :end_per_suite ->
        :ct_util.match_delete_testdata({:comment, :_})

      true ->
        case :erlang.process_info(tCPid, :group_leader) do
          {:group_leader, tCGL} ->
            :ct_util.delete_testdata({:comment, tCGL})

          _ ->
            :ok
        end
    end

    :ct_util.delete_suite_data(:last_saved_config)

    {result1, finalNotify} =
      case hookFunc do
        :undefined ->
          {:ok, result}

        _ when onlyCleanup ->
          {:ok, result}

        _ ->
          case :ct_hooks.end_tc(suite, hookFunc, args, result, return) do
            :"$ct_no_change" ->
              {:ok, result}

            hookResult ->
              {hookResult, hookResult}
          end
      end

    finalResult =
      case :erlang.get(:"$test_server_framework_test") do
        _ when onlyCleanup ->
          result1

        :undefined ->
          event = r_event(name: :tc_done, node: node(), data: {mod, funcSpec, tag(finalNotify)})
          :ct_event.sync_notify(event)
          result1

        fun ->
          event =
            r_event(
              name: :tc_done,
              node: node(),
              data: {mod, funcSpec, tag({:"$test_server_framework_test", finalNotify})}
            )

          :ct_event.sync_notify(event)
          fun.(:end_tc, return)
      end

    case funcSpec do
      {_, groupName, _Props} ->
        cond do
          func == :end_per_group ->
            :ct_config.delete_default_config({:group, groupName})

          true ->
            :ok
        end

        case :lists.keysearch(:save_config, 1, args) do
          {:value, {:save_config, saveConfig}} ->
            :ct_util.save_suite_data(:last_saved_config, {suite, {:group, groupName}}, saveConfig)

          false ->
            :ok
        end

      _ ->
        case :lists.keysearch(:save_config, 1, args) do
          {:value, {:save_config, saveConfig}} ->
            :ct_util.save_suite_data(:last_saved_config, {suite, func}, saveConfig)

          false ->
            :ok
        end
    end

    :ct_util.reset_silent_connections()

    clearCurrTC = fn
      running = [[_, _] | _] ->
        :lists.keydelete(func, 2, running)

      {_, {:suite0_failed, _}} ->
        :undefined

      [{_, _}] ->
        :undefined

      :undefined ->
        :undefined

      unexpected ->
        {:error, {:reset_curr_tc, {mod, func}, unexpected}}
    end

    case :ct_util.update_testdata(
           :curr_tc,
           clearCurrTC
         ) do
      {:error, _} = clearError ->
        exit(clearError)

      _ ->
        :ok
    end

    case finalResult do
      {:auto_skip, {:sequence_failed, _, _}} ->
        :ok

      _ ->
        case :ct_logs.end_tc(tCPid) do
          {:error, reason} ->
            exit({:error, {:logger, reason}})

          _ ->
            :ok
        end
    end

    case func do
      :end_per_suite ->
        :ct_util.match_delete_suite_data({:seq, suite, :_})

      _ ->
        :ok
    end

    finalResult
  end

  defp end_hook_func({:init_per_testcase, _}, {:auto_skip, {:sequence_failed, _, _}}, _) do
    :undefined
  end

  defp end_hook_func(
         {:init_per_testcase, _},
         {:auto_skip, 'Repeated test stopped by force_stop option'},
         _
       ) do
    :undefined
  end

  defp end_hook_func({:init_per_testcase, _}, {:fail, {:config_name_already_in_use, _}}, _) do
    :undefined
  end

  defp end_hook_func({:init_per_testcase, _}, {:auto_skip, {infoFuncError, _}}, _)
       when infoFuncError == :testcase0_failed or
              infoFuncError == :require_failed do
    :undefined
  end

  defp end_hook_func(:init_per_group, {:auto_skip, {infoFuncError, _}}, _)
       when infoFuncError == :group0_failed or
              infoFuncError == :require_failed do
    :undefined
  end

  defp end_hook_func(:init_per_suite, {:auto_skip, {:require_failed_in_suite0, _}}, _) do
    :undefined
  end

  defp end_hook_func(
         :init_per_suite,
         {:auto_skip, {:failed, {:error, {:suite0_failed, _}}}},
         _
       ) do
    :undefined
  end

  defp end_hook_func(_, _, default) do
    default
  end

  defp tag({:"$test_server_framework_test", result}) do
    case tag(result) do
      :ok ->
        result

      failure ->
        failure
    end
  end

  defp tag({:skipped, reason = {:failed, {_, :init_per_testcase, _}}}) do
    {:auto_skipped, reason}
  end

  defp tag({sTag, reason})
       when sTag == :skip or
              sTag == :skipped do
    case reason do
      {:failed, {_, :init_per_testcase, _}} ->
        {:auto_skipped, reason}

      _ ->
        {:skipped, reason}
    end
  end

  defp tag({:auto_skip, reason}) do
    {:auto_skipped, reason}
  end

  defp tag({:fail, reason}) do
    {:failed, {:error, reason}}
  end

  defp tag(failed = {:failed, _Reason}) do
    failed
  end

  defp tag(e = {eTag, _})
       when eTag == :error or
              eTag == :EXIT or eTag == :timetrap_timeout or
              eTag == :testcase_aborted do
    {:failed, e}
  end

  defp tag(e = :testcase_aborted_or_killed) do
    {:failed, e}
  end

  defp tag(userTimetrap = {:user_timetrap_error, _Reason}) do
    userTimetrap
  end

  defp tag(_Other) do
    :ok
  end

  def error_notification(mod, func, _Args, {error, loc}) do
    errorSpec =
      case error do
        {what = {_E, _R}, trace} when is_list(trace) ->
          what

        what ->
          what
      end

    errorStr =
      case errorSpec do
        {:badmatch, descr} ->
          descr1 = :io_lib.format('~tP', [descr, 10])
          descrLength = :string.length(descr1)

          cond do
            descrLength > 50 ->
              descr2 = :string.slice(descr1, 0, 50)
              :io_lib.format('{badmatch,~ts...}', [descr2])

            true ->
              :io_lib.format('{badmatch,~ts}', [descr1])
          end

        {:test_case_failed, reason} ->
          case (try do
                  :io_lib.format('{test_case_failed,~ts}', [reason])
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            {:EXIT, _} ->
              :io_lib.format('{test_case_failed,~tp}', [reason])

            result ->
              result
          end

        other ->
          :io_lib.format('~tP', [other, 5])
      end

    errorHtml = '<font color="brown">' ++ :ct_logs.escape_chars(errorStr) ++ '</font>'

    case {mod, error} do
      {_, {:timetrap_timeout, _TVal}} ->
        :ok

      {_, {:testcase_aborted, _Info}} ->
        :ok

      {_, :testcase_aborted_or_killed} ->
        :ok

      {:undefined, _OtherError} ->
        :ok

      _ ->
        case :ct_util.get_testdata({:comment, :erlang.group_leader()}) do
          :undefined ->
            :test_server.comment(errorHtml)

          comment ->
            commentHtml =
              '<font color="green">' ++
                '(' ++ '</font>' ++ comment ++ '<font color="green">' ++ ')' ++ '</font>'

            str = :io_lib.format('~ts   ~ts', [errorHtml, commentHtml])
            :test_server.comment(str)
        end
    end

    printError = fn errorFormat, errorArgs ->
      div = '\n- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n'
      errorStr2 = :io_lib.format(errorFormat, errorArgs)
      :io.format(:ct_default_gl, '~ts~n', [:lists.concat([div, errorStr2, div])])
      link = '\n\n<a href="#end">Full error description and stacktrace</a>'
      errorHtml2 = :ct_logs.escape_chars(errorStr2)

      :ct_logs.tc_log(
        :ct_error_notify,
        99,
        'CT Error Notification',
        '~ts',
        [errorHtml2 ++ link],
        []
      )
    end

    case loc do
      [{:ct_framework, :error_in_suite}] ->
        printError.('Error in suite detected: ~ts', [errorStr])

      r when r == :unknown or r == :undefined ->
        printError.('Error detected: ~ts', [errorStr])

      [{lastMod, lastFunc} | _] when errorStr == 'undef' ->
        printError.('~w:~tw could not be executed~nReason: ~ts', [lastMod, lastFunc, errorStr])

      [{lastMod, lastFunc} | _] ->
        printError.('~w:~tw failed~nReason: ~ts', [lastMod, lastFunc, errorStr])

      [{lastMod, lastFunc, lastLine} | _] ->
        printError.('~w:~tw failed on line ~w~nReason: ~ts', [
          lastMod,
          lastFunc,
          lastLine,
          errorStr
        ])

        case :ct_util.read_suite_data({:seq, mod, func}) do
          :undefined ->
            :ok

          seq ->
            seqTCs = :ct_util.read_suite_data({:seq, mod, seq})
            mark_as_failed(seq, mod, func, seqTCs)
        end
    end

    :ok
  end

  defp mark_as_failed(seq, mod, func, [func | tCs]) do
    mark_as_failed1(seq, mod, func, tCs)
  end

  defp mark_as_failed(seq, mod, func, [_TC | tCs]) do
    mark_as_failed(seq, mod, func, tCs)
  end

  defp mark_as_failed(_, _, _, []) do
    :ok
  end

  defp mark_as_failed(_, _, _, :undefined) do
    :ok
  end

  defp mark_as_failed1(seq, mod, func, [tC | tCs]) do
    :ct_util.save_suite_data(
      {:seq, mod, tC},
      {:failed, seq, func}
    )

    mark_as_failed1(seq, mod, func, tCs)
  end

  defp mark_as_failed1(_, _, _, []) do
    :ok
  end

  defp group_or_func(func, config)
       when func == :init_per_group or
              func == :end_per_group do
    case :proplists.get_value(
           :tc_group_properties,
           config
         ) do
      :undefined ->
        {func, :unknown, []}

      grProps ->
        grName = :proplists.get_value(:name, grProps)
        {func, grName, :proplists.delete(:name, grProps)}
    end
  end

  defp group_or_func(func, _Config) do
    func
  end

  def get_suite(mod, :all) do
    case safe_apply_groups_0(mod, {:ok, []}) do
      {:ok, groupDefs} ->
        try do
          :ct_groups.find_groups(mod, :all, :all, groupDefs)
        catch
          {:error, error} ->
            [{:ct_framework, :error_in_suite, [[{:error, error}]]}]

          _, error ->
            [{:ct_framework, :error_in_suite, [[{:error, {error, __STACKTRACE__}}]]}]
        else
          confTests when is_list(confTests) ->
            get_all(mod, confTests)
        end

      {:error, {:bad_return, _Bad}} ->
        e = 'Bad return value from ' ++ :erlang.atom_to_list(mod) ++ ':groups/0'
        [{:ct_framework, :error_in_suite, [[{:error, :erlang.list_to_atom(e)}]]}]

      {:error, {:bad_hook_return, bad}} ->
        e = 'Bad return value from post_groups/2 hook function'
        [{:ct_framework, :error_in_suite, [[{:error, {:erlang.list_to_atom(e), bad}}]]}]

      {:error, {:failed, exitReason}} ->
        case :ct_util.get_testdata({:error_in_suite, mod}) do
          :undefined ->
            errStr = :io_lib.format('~n*** ERROR *** ~w:groups/0 failed: ~p~n', [mod, exitReason])
            :io.format(:ct_default_gl, errStr, [])
            :ct_util.set_testdata_async({{:error_in_suite, mod}, exitReason})

          _ExitReason ->
            :ct_util.delete_testdata({:error_in_suite, mod})
        end

        reason = :erlang.list_to_atom(:erlang.atom_to_list(mod) ++ ':groups/0 failed')
        [{:ct_framework, :error_in_suite, [[{:error, reason}]]}]

      {:error, what} ->
        [{:ct_framework, :error_in_suite, [[{:error, what}]]}]
    end
  end

  def get_suite(mod, group = {:conf, props, _Init, tCs, _End}) do
    case safe_apply_groups_0(mod, {:ok, [group]}) do
      {:ok, groupDefs} ->
        name = :proplists.get_value(:name, props)

        try do
          :ct_groups.find_groups(mod, name, tCs, groupDefs)
        catch
          {:error, error} ->
            [{:ct_framework, :error_in_suite, [[{:error, error}]]}]

          _, error ->
            [{:ct_framework, :error_in_suite, [[{:error, {error, __STACKTRACE__}}]]}]
        else
          [] ->
            []

          confTests when is_list(confTests) ->
            case :lists.member(:skipped, props) do
              true ->
                try do
                  :proplists.get_value(
                    :name,
                    :erlang.element(2, hd(confTests))
                  )
                catch
                  _, _ ->
                    []
                else
                  ^name ->
                    :ct_groups.delete_subs(confTests, confTests)

                  _ ->
                    []
                end

              false ->
                confTests1 =
                  :ct_groups.delete_subs(
                    confTests,
                    confTests
                  )

                case :proplists.get_value(:override, props) do
                  :undefined ->
                    confTests1

                  [] ->
                    confTests1

                  oRSpec ->
                    oRSpec1 =
                      cond do
                        is_tuple(oRSpec) ->
                          [oRSpec]

                        true ->
                          oRSpec
                      end

                    :ct_groups.search_and_override(confTests1, oRSpec1, mod)
                end
            end
        end

      {:error, {:bad_return, _Bad}} ->
        e = 'Bad return value from ' ++ :erlang.atom_to_list(mod) ++ ':groups/0'
        [{:ct_framework, :error_in_suite, [[{:error, :erlang.list_to_atom(e)}]]}]

      {:error, {:bad_hook_return, bad}} ->
        e = 'Bad return value from post_groups/2 hook function'
        [{:ct_framework, :error_in_suite, [[{:error, {:erlang.list_to_atom(e), bad}}]]}]

      {:error, {:failed, exitReason}} ->
        case :ct_util.get_testdata({:error_in_suite, mod}) do
          :undefined ->
            errStr = :io_lib.format('~n*** ERROR *** ~w:groups/0 failed: ~p~n', [mod, exitReason])
            :io.format(:ct_default_gl, errStr, [])
            :ct_util.set_testdata_async({{:error_in_suite, mod}, exitReason})

          _ExitReason ->
            :ct_util.delete_testdata({:error_in_suite, mod})
        end

        reason = :erlang.list_to_atom(:erlang.atom_to_list(mod) ++ ':groups/0 failed')
        [{:ct_framework, :error_in_suite, [[{:error, reason}]]}]

      {:error, what} ->
        [{:ct_framework, :error_in_suite, [[{:error, what}]]}]
    end
  end

  def get_suite(mod, name) do
    get_seq(mod, name)
  end

  def get_all_cases(suite) do
    case get_suite(suite, :all) do
      [{:ct_framework, :error_in_suite, [[{:error, _} = error]]}] ->
        error

      [{:ct_framework, :error_in_suite, [[error]]}] ->
        {:error, error}

      tests ->
        cases = get_all_cases1(suite, tests)

        :lists.reverse(
          :lists.foldl(
            fn tC, tCs ->
              case :lists.member(tC, tCs) do
                true ->
                  tCs

                false ->
                  [tC | tCs]
              end
            end,
            [],
            cases
          )
        )
    end
  end

  defp get_all_cases1(
         suite,
         [{:conf, _Props, _Init, grTests, _End} | tests]
       ) do
    get_all_cases1(suite, grTests) ++
      get_all_cases1(
        suite,
        tests
      )
  end

  defp get_all_cases1(suite, [test | tests]) when is_atom(test) do
    [{suite, test} | get_all_cases1(suite, tests)]
  end

  defp get_all_cases1(suite, [test | tests]) do
    [test | get_all_cases1(suite, tests)]
  end

  defp get_all_cases1(_, []) do
    []
  end

  defp get_all(mod, confTests) do
    case safe_apply_all_0(mod) do
      {:ok, allTCs} ->
        try do
          :ct_groups.expand_groups(allTCs, confTests, mod)
        catch
          {:error, error} ->
            [{:ct_framework, :error_in_suite, [[{:error, error}]]}]

          _, error ->
            [{:ct_framework, :error_in_suite, [[{:error, {error, __STACKTRACE__}}]]}]
        else
          {:error, _} = error ->
            [{:ct_framework, :error_in_suite, [[error]]}]

          tests0 ->
            tests = :ct_groups.delete_subs(tests0, tests0)
            expand_tests(mod, tests)
        end

      skip = {:skip, _Reason} ->
        skip

      {:error, :undef} ->
        reason =
          case :code.which(mod) do
            :non_existing ->
              :erlang.list_to_atom(:erlang.atom_to_list(mod) ++ ' cannot be compiled or loaded')

            _ ->
              :erlang.list_to_atom(:erlang.atom_to_list(mod) ++ ':all/0 is missing')
          end

        [{:ct_framework, :error_in_suite, [[{:error, reason}]]}]

      {:error, {:bad_return, _Bad}} ->
        reason =
          :erlang.list_to_atom('Bad return value from ' ++ :erlang.atom_to_list(mod) ++ ':all/0')

        [{:ct_framework, :error_in_suite, [[{:error, reason}]]}]

      {:error, {:bad_hook_return, bad}} ->
        reason = :erlang.list_to_atom('Bad return value from post_all/3 hook function')
        [{:ct_framework, :error_in_suite, [[{:error, {reason, bad}}]]}]

      {:error, {:failed, exitReason}} ->
        case :ct_util.get_testdata({:error_in_suite, mod}) do
          :undefined ->
            errStr = :io_lib.format('~n*** ERROR *** ~w:all/0 failed: ~tp~n', [mod, exitReason])
            :io.format(:ct_default_gl, '~ts', [errStr])
            :ct_util.set_testdata_async({{:error_in_suite, mod}, exitReason})

          _ExitReason ->
            :ct_util.delete_testdata({:error_in_suite, mod})
        end

        reason = :erlang.list_to_atom(:erlang.atom_to_list(mod) ++ ':all/0 failed')
        [{:ct_framework, :error_in_suite, [[{:error, reason}]]}]

      {:error, what} ->
        [{:ct_framework, :error_in_suite, [[{:error, what}]]}]
    end
  end

  defp get_seq(mod, func) do
    case :ct_util.read_suite_data({:seq, mod, func}) do
      :undefined ->
        case (try do
                apply(mod, :sequences, [])
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            []

          seqs ->
            case :lists.keysearch(func, 1, seqs) do
              {:value, {^func, seqTCs}} ->
                case (try do
                        save_seq(mod, func, seqTCs)
                      catch
                        :error, e -> {:EXIT, {e, __STACKTRACE__}}
                        :exit, e -> {:EXIT, e}
                        e -> e
                      end) do
                  {:error, what} ->
                    [{:ct_framework, :error_in_suite, [[{:error, what}]]}]

                  _ ->
                    seqTCs
                end

              false ->
                []
            end
        end

      tCs when is_list(tCs) ->
        tCs

      _ ->
        []
    end
  end

  defp save_seqs(mod, allTCs) do
    case :lists.keymember(:sequence, 1, allTCs) do
      true ->
        case (try do
                apply(mod, :sequences, [])
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            reason = :erlang.list_to_atom(:erlang.atom_to_list(mod) ++ ':sequences/0 is missing')
            throw({:error, reason})

          seqs ->
            save_seqs(mod, allTCs, seqs, allTCs)
        end

      false ->
        allTCs
    end
  end

  defp save_seqs(mod, [{:sequence, seq} | tCs], seqs, all) do
    case :lists.keysearch(seq, 1, seqs) do
      {:value, {^seq, seqTCs}} ->
        save_seq(mod, seq, seqTCs, all)
        [seq | save_seqs(mod, tCs, seqs, all)]

      false ->
        reason =
          :erlang.list_to_atom(
            :erlang.atom_to_list(seq) ++ ' is missing in ' ++ :erlang.atom_to_list(mod)
          )

        throw({:error, reason})
    end
  end

  defp save_seqs(mod, [tC | tCs], seqs, all) do
    [tC | save_seqs(mod, tCs, seqs, all)]
  end

  defp save_seqs(_, [], _, _) do
    []
  end

  defp save_seq(mod, seq, seqTCs) do
    save_seq(mod, seq, seqTCs, apply(mod, :all, []))
  end

  defp save_seq(mod, seq, seqTCs, all) do
    check_private(seq, seqTCs, all)
    check_multiple(mod, seq, seqTCs)
    :ct_util.save_suite_data({:seq, mod, seq}, seqTCs)

    :lists.foreach(
      fn tC ->
        :ct_util.save_suite_data({:seq, mod, tC}, seq)
      end,
      seqTCs
    )
  end

  defp check_private(seq, tCs, all) do
    bad =
      :lists.filter(
        fn tC ->
          :lists.member(tC, all)
        end,
        tCs
      )

    cond do
      bad != [] ->
        reason = :io_lib.format('regular test cases not allowed in sequence ~tp: ~tp', [seq, bad])
        throw({:error, :erlang.list_to_atom(:lists.flatten(reason))})

      true ->
        :ok
    end
  end

  defp check_multiple(mod, seq, tCs) do
    bad =
      :lists.filter(
        fn tC ->
          case :ct_util.read_suite_data({:seq, mod, tC}) do
            seq1 when seq1 != :undefined and seq1 != seq ->
              true

            _ ->
              false
          end
        end,
        tCs
      )

    cond do
      bad != [] ->
        reason = :io_lib.format('test cases found in multiple sequences: ~tp', [bad])
        throw({:error, :erlang.list_to_atom(:lists.flatten(reason))})

      true ->
        :ok
    end
  end

  def error_in_suite(config) do
    reason = :test_server.lookup_config(:error, config)
    exit(reason)
  end

  def init_per_suite(config) do
    config
  end

  def end_per_suite(_Config) do
    :ok
  end

  def init_per_group(groupName, config) do
    :ct.comment(:io_lib.format('start of ~tp', [groupName]))

    :ct_logs.log('TEST INFO', 'init_per_group/2 for ~tw missing in suite, using default.', [
      groupName
    ])

    config
  end

  def end_per_group(groupName, _) do
    :ct.comment(:io_lib.format('end of ~tp', [groupName]))

    :ct_logs.log('TEST INFO', 'end_per_group/2 for ~tw missing in suite, using default.', [
      groupName
    ])

    :ok
  end

  def report(what, data) do
    case what do
      :loginfo ->
        testName =
          :filename.basename(
            :proplists.get_value(
              :topdir,
              data
            ),
            '.logs'
          )

        runDir = :proplists.get_value(:rundir, data)
        _ = :ct_logs.make_all_suites_index({testName, runDir})
        :ok

      :tests_start ->
        :ok

      :tests_done ->
        :ok

      :severe_error ->
        :ct_event.sync_notify(r_event(name: what, node: node(), data: data))
        :ct_util.set_testdata({what, data})
        :ok

      :tc_start ->
        data1 =
          case data do
            {{suite, {func, :undefined}}, lFN} ->
              {{suite, func}, lFN}

            _ ->
              data
          end

        :ct_event.sync_notify(r_event(name: :tc_logfile, node: node(), data: data1))
        :ok

      :tc_done ->
        {suite, {func, grName}, result} = data

        funcSpec =
          cond do
            grName == :undefined ->
              func

            true ->
              {func, grName}
          end

        reportingPid = self()

        :ct_logs.register_groupleader(
          reportingPid,
          :erlang.group_leader()
        )

        case result do
          {:failed, reason} ->
            :ct_hooks.on_tc_fail(what, {suite, funcSpec, reason})

          {:skipped, {:failed, {_, :init_per_testcase, _}} = reason} ->
            :ct_hooks.on_tc_skip(
              :tc_auto_skip,
              {suite, funcSpec, reason}
            )

          {:skipped, {:require_failed, _} = reason} ->
            :ct_hooks.on_tc_skip(
              :tc_auto_skip,
              {suite, funcSpec, reason}
            )

          {:skipped, reason} ->
            :ct_hooks.on_tc_skip(
              :tc_user_skip,
              {suite, funcSpec, reason}
            )

          {:auto_skipped, reason} ->
            :ct_hooks.on_tc_skip(
              :tc_auto_skip,
              {suite, funcSpec, reason}
            )

          _Else ->
            :ok
        end

        :ct_logs.unregister_groupleader(reportingPid)

        case {func, result} do
          {:error_in_suite, _} when suite == :ct_framework ->
            :ok

          {:init_per_suite, _} ->
            :ok

          {:end_per_suite, _} ->
            :ok

          {:init_per_group, _} ->
            :ok

          {:end_per_group, _} ->
            :ok

          {_, :ok} ->
            add_to_stats(:ok)

          {_, {:skipped, {:failed, {_, :init_per_testcase, _}}}} ->
            add_to_stats(:auto_skipped)

          {_, {:skipped, {:require_failed, _}}} ->
            add_to_stats(:auto_skipped)

          {_, {:skipped, {:timetrap_error, _}}} ->
            add_to_stats(:auto_skipped)

          {_, {:skipped, {:invalid_time_format, _}}} ->
            add_to_stats(:auto_skipped)

          {_, {:skipped, _}} ->
            add_to_stats(:user_skipped)

          {_, {:auto_skipped, _}} ->
            add_to_stats(:auto_skipped)

          {_, {skipOrFail, _Reason}} ->
            add_to_stats(skipOrFail)
        end

      :tc_user_skip ->
        {func, data1} =
          case data do
            {suite, {f, :undefined}, comment} ->
              {f, {suite, f, comment}}

            d = {_, {f, _}, _} ->
              {f, d}
          end

        :ct_event.sync_notify(r_event(name: :tc_user_skip, node: node(), data: data1))
        :ct_hooks.on_tc_skip(what, data1)

        cond do
          func != :init_per_suite and func != :init_per_group and
            func != :end_per_suite and func != :end_per_group ->
            add_to_stats(:user_skipped)

          true ->
            :ok
        end

      :tc_auto_skip ->
        {func, data1} =
          case data do
            {suite, {f, :undefined}, comment} ->
              {f, {suite, f, comment}}

            d = {_, {f, _}, _} ->
              {f, d}
          end

        :ct_event.sync_notify(r_event(name: :tc_auto_skip, node: node(), data: data1))
        :ct_hooks.on_tc_skip(what, data1)

        cond do
          func != :end_per_suite and func != :end_per_group ->
            add_to_stats(:auto_skipped)

          true ->
            :ok
        end

      :framework_error ->
        case data do
          {{m, f}, e} ->
            :ct_event.sync_notify(
              r_event(name: :tc_done, node: node(), data: {m, f, {:framework_error, e}})
            )

          _ ->
            :ct_event.sync_notify(r_event(name: :tc_done, node: node(), data: data))
        end

      _ ->
        :ok
    end
  end

  defp add_to_stats(result) do
    update = fn {ok, failed, skipped = {userSkipped, autoSkipped}} ->
      stats =
        case result do
          :ok ->
            {ok + 1, failed, skipped}

          :failed ->
            {ok, failed + 1, skipped}

          :skipped ->
            {ok, failed, {userSkipped + 1, autoSkipped}}

          :user_skipped ->
            {ok, failed, {userSkipped + 1, autoSkipped}}

          :auto_skipped ->
            {ok, failed, {userSkipped, autoSkipped + 1}}
        end

      :ct_event.sync_notify(r_event(name: :test_stats, node: node(), data: stats))
      stats
    end

    :ct_util.update_testdata(:stats, update)
  end

  def warn(what)
      when what == :nodes or
             what == :processes do
    false
  end

  def warn(_What) do
    true
  end

  defp add_data_dir(file, config) when is_atom(file) do
    add_data_dir(:erlang.atom_to_list(file), config)
  end

  defp add_data_dir(file, config) when is_list(file) do
    case :filename.split(file) do
      [^file] ->
        case :lists.keysearch(:data_dir, 1, config) do
          {:value, {:data_dir, dataDir}} ->
            :filename.join(dataDir, file)

          _ ->
            file
        end

      _ ->
        file
    end
  end

  def get_logopts() do
    case :ct_util.get_testdata(:logopts) do
      :undefined ->
        []

      logOpts ->
        logOpts
    end
  end

  def format_comment(comment) do
    '<font color="green">' ++ comment ++ '</font>'
  end

  def get_html_wrapper(testName, printLabel, cwd, tableCols) do
    get_html_wrapper(testName, printLabel, cwd, tableCols, :utf8)
  end

  defp get_html_wrapper(testName, printLabel, cwd, tableCols, encoding) do
    :ct_logs.get_ts_html_wrapper(testName, printLabel, cwd, tableCols, encoding)
  end

  def get_log_dir() do
    :ct_logs.get_log_dir(true)
  end

  defp safe_apply_all_0(mod) do
    try do
      apply(mod, :all, [])
    catch
      _, reason ->
        handle_callback_crash(reason, __STACKTRACE__, mod, :all, {:error, :undef})
    else
      allTCs0 when is_list(allTCs0) ->
        try do
          save_seqs(mod, allTCs0)
        catch
          {:error, what} ->
            {:error, what}
        else
          seqsAndTCs when is_list(seqsAndTCs) ->
            all_hook(mod, seqsAndTCs)
        end

      {:skip, _} = skip ->
        all_hook(mod, skip)

      bad ->
        {:error, {:bad_return, bad}}
    end
  end

  defp all_hook(mod, all) do
    case :ct_hooks.all(mod, all) do
      allTCs when is_list(allTCs) ->
        {:ok, allTCs}

      {:skip, _} = skip ->
        skip

      {:fail, reason} ->
        {:error, reason}

      bad ->
        {:error, {:bad_hook_return, bad}}
    end
  end

  defp safe_apply_groups_0(mod, default) do
    try do
      apply(mod, :groups, [])
    catch
      _, reason ->
        handle_callback_crash(reason, __STACKTRACE__, mod, :groups, default)
    else
      groupDefs when is_list(groupDefs) ->
        case :ct_hooks.groups(mod, groupDefs) do
          groupDefs1 when is_list(groupDefs1) ->
            {:ok, groupDefs1}

          {:fail, reason} ->
            {:error, reason}

          bad ->
            {:error, {:bad_hook_return, bad}}
        end

      bad ->
        {:error, {:bad_return, bad}}
    end
  end

  defp handle_callback_crash(:undef, [{mod, func, [], _} | _], mod, func, default) do
    case apply(:ct_hooks, func, [mod, []]) do
      [] ->
        default

      list when is_list(list) ->
        {:ok, list}

      {:fail, reason} ->
        {:error, reason}

      bad ->
        {:error, {:bad_hook_return, bad}}
    end
  end

  defp handle_callback_crash(reason, stacktrace, _Mod, _Func, _Default) do
    {:error, {:failed, {reason, stacktrace}}}
  end

  defp expand_tests(mod, [{:testcase, case__, [prop]} | tests]) do
    [
      {:repeat, {mod, case__}, prop}
      | expand_tests(
          mod,
          tests
        )
    ]
  end

  defp expand_tests(mod, [test | tests]) do
    [test | expand_tests(mod, tests)]
  end

  defp expand_tests(_Mod, []) do
    []
  end
end
