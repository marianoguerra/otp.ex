defmodule :m_application_controller do
  use Bitwise
  import Kernel, except: [to_string: 1]

  import :lists,
    only: [foldl: 3, foreach: 2, keydelete: 3, keyfind: 3, keyreplace: 4, map: 2, zf: 2]

  require Record

  Record.defrecord(:r_appl_data, :appl_data,
    name: :undefined,
    regs: [],
    phases: :undefined,
    mod: :undefined,
    mods: [],
    maxP: :infinity,
    maxT: :infinity
  )

  Record.defrecord(:r_state, :state,
    loading: [],
    starting: [],
    start_p_false: [],
    running: [],
    control: [],
    started: [],
    start_req: [],
    conf_data: :undefined
  )

  Record.defrecord(:r_appl, :appl,
    name: :undefined,
    appl_data: :undefined,
    descr: :undefined,
    id: :undefined,
    vsn: :undefined,
    restart_type: :undefined,
    inc_apps: :undefined,
    apps: :undefined
  )

  def start(kernelApp) do
    init = self()

    aC =
      spawn_link(fn ->
        init(init, kernelApp)
      end)

    receive do
      {:ack, ^aC, :ok} ->
        {:ok, aC}

      {:ack, ^aC, {:error, reason}} ->
        to_string(reason)

      {:EXIT, _Pid, reason} ->
        to_string(reason)
    end
  end

  def load_application(application) do
    :gen_server.call(:application_controller, {:load_application, application}, :infinity)
  end

  def unload_application(appName) do
    :gen_server.call(:application_controller, {:unload_application, appName}, :infinity)
  end

  def start_application(appName, restartType) do
    :gen_server.call(
      :application_controller,
      {:start_application, appName, restartType},
      :infinity
    )
  end

  def start_boot_application(application, restartType) do
    case {:application.load(application), restartType} do
      {:ok, _} ->
        appName = get_appl_name(application)

        :gen_server.call(
          :application_controller,
          {:start_application, appName, restartType},
          :infinity
        )

      {{:error, {:already_loaded, appName}}, _} ->
        :gen_server.call(
          :application_controller,
          {:start_application, appName, restartType},
          :infinity
        )

      {{:error, {:bad_environment_value, env}}, :permanent} ->
        txt = :io_lib.format('Bad environment variable: ~tp  Application: ~p', [env, application])
        exit({:error, :erlang.list_to_atom(:lists.flatten(txt))})

      {error, _} ->
        error
    end
  end

  def stop_application(appName) do
    :gen_server.call(:application_controller, {:stop_application, appName}, :infinity)
  end

  def which_applications() do
    :gen_server.call(
      :application_controller,
      :which_applications
    )
  end

  def which_applications(timeout) do
    :gen_server.call(:application_controller, :which_applications, timeout)
  end

  def loaded_applications() do
    :ets.select(
      :ac_tab,
      [
        {{{:loaded, :"$1"}, r_appl(descr: :"$2", vsn: :"$3", _: :_)}, [],
         [{{:"$1", :"$2", :"$3"}}]}
      ]
    )
  end

  def info() do
    :gen_server.call(:application_controller, :info)
  end

  def control_application(appName) do
    :gen_server.call(:application_controller, {:control_application, appName}, :infinity)
  end

  def change_application_data(applications, config) do
    :gen_server.call(
      :application_controller,
      {:change_application_data, applications, config},
      :infinity
    )
  end

  def prep_config_change() do
    :gen_server.call(:application_controller, :prep_config_change, :infinity)
  end

  def config_change(envPrev) do
    :gen_server.call(:application_controller, {:config_change, envPrev}, :infinity)
  end

  def get_pid_env(master, key) do
    case :ets.match(
           :ac_tab,
           {{:application_master, :"$1"}, master}
         ) do
      [[appName]] ->
        get_env(appName, key)

      _ ->
        :undefined
    end
  end

  def get_env(appName, key) do
    case :ets.lookup(:ac_tab, {:env, appName, key}) do
      [{_, val}] ->
        {:ok, val}

      _ ->
        :undefined
    end
  end

  def get_pid_all_env(master) do
    case :ets.match(
           :ac_tab,
           {{:application_master, :"$1"}, master}
         ) do
      [[appName]] ->
        get_all_env(appName)

      _ ->
        []
    end
  end

  def get_all_env(appName) do
    map(
      fn [key, val] ->
        {key, val}
      end,
      :ets.match(:ac_tab, {{:env, appName, :"$1"}, :"$2"})
    )
  end

  def get_pid_key(master, key) do
    case :ets.match(
           :ac_tab,
           {{:application_master, :"$1"}, master}
         ) do
      [[appName]] ->
        get_key(appName, key)

      _ ->
        :undefined
    end
  end

  def get_key(appName, key) do
    case :ets.lookup(:ac_tab, {:loaded, appName}) do
      [{_, appl}] ->
        case key do
          :description ->
            {:ok, r_appl(appl, :descr)}

          :id ->
            {:ok, r_appl(appl, :id)}

          :vsn ->
            {:ok, r_appl(appl, :vsn)}

          :modules ->
            {:ok, r_appl_data(r_appl(appl, :appl_data), :mods)}

          :maxP ->
            {:ok, r_appl_data(r_appl(appl, :appl_data), :maxP)}

          :maxT ->
            {:ok, r_appl_data(r_appl(appl, :appl_data), :maxT)}

          :registered ->
            {:ok, r_appl_data(r_appl(appl, :appl_data), :regs)}

          :included_applications ->
            {:ok, r_appl(appl, :inc_apps)}

          :applications ->
            {:ok, r_appl(appl, :apps)}

          :env ->
            {:ok, get_all_env(appName)}

          :mod ->
            {:ok, r_appl_data(r_appl(appl, :appl_data), :mod)}

          :start_phases ->
            {:ok, r_appl_data(r_appl(appl, :appl_data), :phases)}

          _ ->
            :undefined
        end

      _ ->
        :undefined
    end
  end

  def get_pid_all_key(master) do
    case :ets.match(
           :ac_tab,
           {{:application_master, :"$1"}, master}
         ) do
      [[appName]] ->
        get_all_key(appName)

      _ ->
        []
    end
  end

  def get_all_key(appName) do
    case :ets.lookup(:ac_tab, {:loaded, appName}) do
      [{_, appl}] ->
        {:ok,
         [
           {:description, r_appl(appl, :descr)},
           {:id, r_appl(appl, :id)},
           {:vsn, r_appl(appl, :vsn)},
           {:modules, r_appl_data(r_appl(appl, :appl_data), :mods)},
           {:maxP, r_appl_data(r_appl(appl, :appl_data), :maxP)},
           {:maxT, r_appl_data(r_appl(appl, :appl_data), :maxT)},
           {:registered, r_appl_data(r_appl(appl, :appl_data), :regs)},
           {:included_applications, r_appl(appl, :inc_apps)},
           {:applications, r_appl(appl, :apps)},
           {:env, get_all_env(appName)},
           {:mod, r_appl_data(r_appl(appl, :appl_data), :mod)},
           {:start_phases, r_appl_data(r_appl(appl, :appl_data), :phases)}
         ]}

      _ ->
        :undefined
    end
  end

  def start_type(master) do
    case :ets.match(
           :ac_tab,
           {{:application_master, :"$1"}, master}
         ) do
      [[appName]] ->
        :gen_server.call(:application_controller, {:start_type, appName}, :infinity)

      _X ->
        :undefined
    end
  end

  def get_master(appName) do
    case :ets.lookup(
           :ac_tab,
           {:application_master, appName}
         ) do
      [{_, pid}] ->
        pid

      _ ->
        :undefined
    end
  end

  def get_application(master) do
    case :ets.match(
           :ac_tab,
           {{:application_master, :"$1"}, master}
         ) do
      [[appName]] ->
        {:ok, appName}

      _ ->
        :undefined
    end
  end

  def get_application_module(module) do
    applDataPattern = r_appl_data(mods: :"$2", _: :_)
    applPattern = r_appl(appl_data: applDataPattern, _: :_)

    appModules =
      :ets.match(
        :ac_tab,
        {{:loaded, :"$1"}, applPattern}
      )

    get_application_module(module, appModules)
  end

  defp get_application_module(module, [[appName, modules] | appModules]) do
    case :lists.member(module, modules) do
      true ->
        {:ok, appName}

      false ->
        get_application_module(module, appModules)
    end
  end

  defp get_application_module(_Module, []) do
    :undefined
  end

  def permit_application(applName, flag) do
    :gen_server.call(:application_controller, {:permit_application, applName, flag}, :infinity)
  end

  def set_env(config, opts) do
    case check_conf_data(config) do
      :ok ->
        timeout = :proplists.get_value(:timeout, opts, 5000)
        :gen_server.call(:application_controller, {:set_env, config, opts}, timeout)

      {:error, _} = error ->
        error
    end
  end

  def set_env(appName, key, val) do
    :gen_server.call(
      :application_controller,
      {:set_env, appName, key, val, []}
    )
  end

  def set_env(appName, key, val, opts) do
    timeout = :proplists.get_value(:timeout, opts, 5000)
    :gen_server.call(:application_controller, {:set_env, appName, key, val, opts}, timeout)
  end

  def unset_env(appName, key) do
    :gen_server.call(
      :application_controller,
      {:unset_env, appName, key, []}
    )
  end

  def unset_env(appName, key, opts) do
    timeout = :proplists.get_value(:timeout, opts, 5000)
    :gen_server.call(:application_controller, {:unset_env, appName, key, opts}, timeout)
  end

  defp init(init, kernel) do
    :erlang.register(:application_controller, self())
    :erlang.process_flag(:trap_exit, true)
    :erlang.put(:"$ancestors", [init])
    :erlang.put(:"$initial_call", {:application_controller, :start, 1})

    case (try do
            check_conf()
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, confData} ->
        case check_conf_data(confData) do
          :ok ->
            _ =
              :ets.new(
                :ac_tab,
                [:set, :public, :named_table, {:read_concurrency, true}]
              )

            s = r_state(conf_data: confData)
            {:ok, kAppl} = make_appl(kernel)

            case (try do
                    load(s, kAppl)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:EXIT, loadError} ->
                reason = {:"load error", loadError}
                send(init, {:ack, self(), {:error, to_string(reason)}})

              {:error, error} ->
                send(init, {:ack, self(), {:error, to_string(error)}})

              {:ok, newS} ->
                send(init, {:ack, self(), :ok})

                :gen_server.enter_loop(
                  :application_controller,
                  [],
                  newS,
                  {:local, :application_controller}
                )
            end

          {:error, errorStr} ->
            str = :lists.flatten(:io_lib.format('invalid config data: ~ts', [errorStr]))
            send(init, {:ack, self(), {:error, to_string(str)}})
        end

      {:error, {file, line, str}} ->
        reasonStr =
          :lists.flatten(
            :io_lib.format(
              'error in config file ~tp (~w): ~ts',
              [file, line, str]
            )
          )

        send(init, {:ack, self(), {:error, to_string(reasonStr)}})
    end
  end

  defp check_conf_data([]) do
    :ok
  end

  defp check_conf_data(confData) when is_list(confData) do
    [application | confDataRem] = confData

    case application do
      {appName, list}
      when is_atom(appName) and
             is_list(list) ->
        case :lists.keymember(appName, 1, confDataRem) do
          true ->
            {:error, 'duplicate application config: ' ++ :erlang.atom_to_list(appName)}

          false ->
            case check_para(list, appName) do
              :ok ->
                check_conf_data(confDataRem)

              error ->
                error
            end
        end

      {appName, list} when is_list(list) ->
        errMsg =
          'application: ' ++
            :lists.flatten(
              :io_lib.format(
                '~tp',
                [appName]
              )
            ) ++ '; application name must be an atom'

        {:error, errMsg}

      {appName, _List} ->
        errMsg =
          'application: ' ++
            :lists.flatten(
              :io_lib.format(
                '~tp',
                [appName]
              )
            ) ++ '; parameters must be a list'

        {:error, errMsg}

      else__ ->
        errMsg =
          'invalid application config: ' ++
            :lists.flatten(
              :io_lib.format(
                '~tp',
                [else__]
              )
            )

        {:error, errMsg}
    end
  end

  defp check_conf_data(_ConfData) do
    {:error, 'configuration must be a list ended by <dot><whitespace>'}
  end

  defp check_para([], _AppName) do
    :ok
  end

  defp check_para([{para, val} | paraList], appName)
       when is_atom(para) do
    case :lists.keymember(para, 1, paraList) do
      true ->
        errMsg =
          'application: ' ++
            :erlang.atom_to_list(appName) ++
            '; duplicate parameter: ' ++ :erlang.atom_to_list(para)

        {:error, errMsg}

      false ->
        case check_para_value(para, val, appName) do
          :ok ->
            check_para(paraList, appName)

          {:error, _} = error ->
            error
        end
    end
  end

  defp check_para([{para, _Val} | _ParaList], appName) do
    {:error,
     'application: ' ++
       :erlang.atom_to_list(appName) ++
       '; invalid parameter name: ' ++
       :lists.flatten(
         :io_lib.format(
           '~tp',
           [para]
         )
       )}
  end

  defp check_para([else__ | _ParaList], appName) do
    {:error,
     'application: ' ++
       :erlang.atom_to_list(appName) ++
       '; invalid parameter: ' ++
       :lists.flatten(
         :io_lib.format(
           '~tp',
           [else__]
         )
       )}
  end

  defp check_para_value(:distributed, apps, :kernel) do
    check_distributed(apps)
  end

  defp check_para_value(_Para, _Val, _AppName) do
    :ok
  end

  defp check_distributed([]) do
    :ok
  end

  defp check_distributed([{app, list} | apps])
       when is_atom(app) and
              is_list(list) do
    check_distributed(apps)
  end

  defp check_distributed([{app, :infinity, list} | apps])
       when is_atom(app) and is_list(list) do
    check_distributed(apps)
  end

  defp check_distributed([{app, time, list} | apps])
       when is_atom(app) and is_integer(time) and
              is_list(list) do
    check_distributed(apps)
  end

  defp check_distributed(_Else) do
    {:error, 'application: kernel; erroneous parameter: distributed'}
  end

  def handle_call({:load_application, application}, from, s) do
    case (try do
            do_load_application(application, s)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newS} ->
        appName = get_appl_name(application)

        case cntrl(appName, s, {:ac_load_application_req, appName}) do
          true ->
            {:noreply, r_state(s, loading: [{appName, from} | r_state(s, :loading)])}

          false ->
            {:reply, :ok, newS}
        end

      {:error, _} = error ->
        {:reply, error, s}

      {:EXIT, r} ->
        {:reply, {:error, r}, s}
    end
  end

  def handle_call({:unload_application, appName}, _From, s) do
    case :lists.keymember(appName, 1, r_state(s, :running)) do
      true ->
        {:reply, {:error, {:running, appName}}, s}

      false ->
        case get_loaded(appName) do
          {true, _} ->
            newS = unload(appName, s)
            cntrl(appName, s, {:ac_application_unloaded, appName})
            {:reply, :ok, newS}

          false ->
            {:reply, {:error, {:not_loaded, appName}}, s}
        end
    end
  end

  def handle_call({:start_application, appName, restartType}, from, s) do
    r_state(
      running: running,
      starting: starting,
      start_p_false: sPF,
      started: started,
      start_req: start_req
    ) = s

    case :lists.keyfind(appName, 1, start_req) do
      false ->
        case (try do
                check_start_cond(appName, restartType, started, running)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:ok, appl} ->
            cntrl = cntrl(appName, s, {:ac_start_application_req, appName})
            perm = :application.get_env(:kernel, :permissions)

            case {cntrl, perm} do
              {true, _} ->
                {:noreply,
                 r_state(s,
                   starting: [
                     {appName, restartType, :normal, from}
                     | starting
                   ],
                   start_req: [{appName, from} | start_req]
                 )}

              {false, :undefined} ->
                spawn_starter(from, appl, s, :normal)

                {:noreply,
                 r_state(s,
                   starting: [
                     {appName, restartType, :normal, from}
                     | starting
                   ],
                   start_req: [{appName, from} | start_req]
                 )}

              {false, {:ok, perms}} ->
                case :lists.member({appName, false}, perms) do
                  false ->
                    spawn_starter(from, appl, s, :normal)

                    {:noreply,
                     r_state(s,
                       starting: [
                         {appName, restartType, :normal, from}
                         | starting
                       ],
                       start_req: [{appName, from} | start_req]
                     )}

                  true ->
                    sS =
                      r_state(s,
                        start_p_false: [
                          {appName, restartType, :normal, from}
                          | sPF
                        ]
                      )

                    {:reply, :ok, sS}
                end
            end

          {:error, _R} = error ->
            {:reply, error, s}
        end

      {^appName, _FromX} ->
        sS = r_state(s, start_req: [{appName, from} | start_req])
        {:noreply, sS}
    end
  end

  def handle_call({:permit_application, appName, bool}, from, s) do
    control = r_state(s, :control)
    starting = r_state(s, :starting)
    sPF = r_state(s, :start_p_false)
    started = r_state(s, :started)
    running = r_state(s, :running)
    start_req = r_state(s, :start_req)
    isLoaded = get_loaded(appName)
    isStarting = :lists.keysearch(appName, 1, starting)
    isSPF = :lists.keysearch(appName, 1, sPF)
    isStarted = :lists.keysearch(appName, 1, started)
    isRunning = :lists.keysearch(appName, 1, running)

    case :lists.keymember(appName, 1, control) do
      true ->
        case {isLoaded, isStarting, isStarted} do
          {false, _, _} ->
            {:reply, {:error, {:not_loaded, appName}}, s}

          {{true, _Appl}, false, false} ->
            update_permissions(appName, bool)
            {:reply, {:distributed_application, :only_loaded}, s}

          _ ->
            update_permissions(appName, bool)
            {:reply, :distributed_application, s}
        end

      false ->
        case {bool, isLoaded, isStarting, isSPF, isStarted, isRunning} do
          {true, _, _, _, _, {:value, _Tuple}} ->
            {:reply, :ok, s}

          {true, false, _, _, _, _} ->
            {:reply, {:error, {:not_loaded, appName}}, s}

          {true, {true, _Appl}, false, false, false, false} ->
            update_permissions(appName, bool)
            {:reply, :ok, s}

          {true, {true, _Appl}, {:value, _Tuple}, false, false, false} ->
            update_permissions(appName, bool)
            {:reply, :ok, s}

          {true, {true, appl}, false, {:value, tuple}, false, false} ->
            update_permissions(appName, bool)
            {_AppName2, restartType, :normal, _From} = tuple
            spawn_starter(from, appl, s, :normal)

            sS =
              r_state(s,
                starting: [
                  {appName, restartType, :normal, from}
                  | starting
                ],
                start_p_false: keydelete(appName, 1, sPF),
                start_req: [{appName, from} | start_req]
              )

            {:noreply, sS}

          {true, {true, appl}, _, _, {:value, {^appName, restartType}}, false} ->
            update_permissions(appName, bool)
            spawn_starter(from, appl, s, :normal)

            sS =
              r_state(s,
                starting: [
                  {appName, restartType, :normal, from}
                  | starting
                ],
                started: keydelete(appName, 1, started),
                start_req: [{appName, from} | start_req]
              )

            {:noreply, sS}

          {false, _, _, _, _, {:value, {_AppName, id}}} ->
            {_AppName2, type} = :lists.keyfind(appName, 1, started)
            stop_appl(appName, id, type)
            nRunning = keydelete(appName, 1, running)
            {:reply, :ok, r_state(s, running: nRunning)}

          {false, false, _, _, _, _} ->
            {:reply, {:error, {:not_loaded, appName}}, s}

          {false, {true, _Appl}, false, false, false, false} ->
            update_permissions(appName, bool)
            {:reply, :ok, s}

          {false, {true, _Appl}, {:value, _Tuple}, false, false, false} ->
            update_permissions(appName, bool)
            {:reply, :ok, s}

          {false, {true, _Appl}, false, {:value, _Tuple}, false, false} ->
            update_permissions(appName, bool)
            sS = r_state(s, start_p_false: keydelete(appName, 1, sPF))
            {:reply, :ok, sS}

          {false, {true, _Appl}, _, _, {:value, _Tuple}, false} ->
            update_permissions(appName, bool)
            {:reply, :ok, s}
        end
    end
  end

  def handle_call({:stop_application, appName}, _From, s) do
    r_state(running: running, started: started) = s

    case :lists.keyfind(appName, 1, running) do
      {_AppName, id} ->
        {_AppName2, type} = :lists.keyfind(appName, 1, started)
        stop_appl(appName, id, type)
        nRunning = keydelete(appName, 1, running)
        nStarted = keydelete(appName, 1, started)
        cntrl(appName, s, {:ac_application_stopped, appName})
        {:reply, :ok, r_state(s, running: nRunning, started: nStarted)}

      false ->
        case :lists.keymember(appName, 1, started) do
          true ->
            nStarted = keydelete(appName, 1, started)
            cntrl(appName, s, {:ac_application_stopped, appName})
            {:reply, :ok, r_state(s, started: nStarted)}

          false ->
            {:reply, {:error, {:not_started, appName}}, s}
        end
    end
  end

  def handle_call({:change_application_data, applications, config}, _From, s) do
    oldAppls =
      :ets.filter(
        :ac_tab,
        fn
          [{{:loaded, _AppName}, appl}] ->
            {true, appl}

          _ ->
            false
        end,
        []
      )

    case (try do
            do_change_apps(applications, config, oldAppls)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, _} = error ->
        {:reply, error, s}

      {:EXIT, r} ->
        {:reply, {:error, r}, s}

      {newAppls, newConfig} ->
        :lists.foreach(
          fn appl ->
            :ets.insert(
              :ac_tab,
              {{:loaded, r_appl(appl, :name)}, appl}
            )
          end,
          newAppls
        )

        {:reply, :ok, r_state(s, conf_data: newConfig)}
    end
  end

  def handle_call(:prep_config_change, _From, s) do
    runningApps = r_state(s, :running)
    envBefore = :lists.reverse(do_prep_config_change(runningApps))
    {:reply, envBefore, s}
  end

  def handle_call({:config_change, envBefore}, _From, s) do
    runningApps = r_state(s, :running)
    r = do_config_change(runningApps, envBefore)
    {:reply, r, s}
  end

  def handle_call(:which_applications, _From, s) do
    reply =
      zf(
        fn {name, id} ->
          case id do
            {:distributed, _Node} ->
              false

            _ ->
              {true, r_appl(descr: descr, vsn: vsn)} = get_loaded(name)
              {true, {name, descr, vsn}}
          end
        end,
        r_state(s, :running)
      )

    {:reply, reply, s}
  end

  def handle_call({:set_env, config, opts}, _From, s) do
    _ =
      for {appName, env} <- config do
        add_env(appName, env)
      end

    case :proplists.get_value(:persistent, opts, false) do
      true ->
        {:reply, :ok, r_state(s, conf_data: merge_env(r_state(s, :conf_data), config))}

      false ->
        {:reply, :ok, s}
    end
  end

  def handle_call({:set_env, appName, key, val, opts}, _From, s) do
    :ets.insert(:ac_tab, {{:env, appName, key}, val})

    case :proplists.get_value(:persistent, opts, false) do
      true ->
        fun = fn env ->
          :lists.keystore(key, 1, env, {key, val})
        end

        {:reply, :ok, r_state(s, conf_data: change_app_env(r_state(s, :conf_data), appName, fun))}

      false ->
        {:reply, :ok, s}
    end
  end

  def handle_call({:unset_env, appName, key, opts}, _From, s) do
    :ets.delete(:ac_tab, {:env, appName, key})

    case :proplists.get_value(:persistent, opts, false) do
      true ->
        fun = fn env ->
          :lists.keydelete(key, 1, env)
        end

        {:reply, :ok, r_state(s, conf_data: change_app_env(r_state(s, :conf_data), appName, fun))}

      false ->
        {:reply, :ok, s}
    end
  end

  def handle_call({:control_application, appName}, {pid, _Tag}, s) do
    control = r_state(s, :control)

    case :lists.keymember(appName, 1, control) do
      false ->
        :erlang.link(pid)
        {:reply, true, r_state(s, control: [{appName, pid} | control])}

      true ->
        {:reply, false, s}
    end
  end

  def handle_call({:start_type, appName}, _From, s) do
    starting = r_state(s, :starting)

    startType =
      case :lists.keyfind(appName, 1, starting) do
        false ->
          :local

        {_AppName, _RestartType, type, _F} ->
          type
      end

    {:reply, startType, s}
  end

  def handle_call(:info, _From, s) do
    reply = [
      {:loaded, loaded_applications()},
      {:loading, r_state(s, :loading)},
      {:started, r_state(s, :started)},
      {:start_p_false, r_state(s, :start_p_false)},
      {:running, r_state(s, :running)},
      {:starting, r_state(s, :starting)}
    ]

    {:reply, reply, s}
  end

  def handle_cast({:application_started, appName, res}, s) do
    handle_application_started(appName, res, s)
  end

  defp handle_application_started(appName, res, s) do
    r_state(starting: starting, running: running, started: started, start_req: start_req) = s
    start_reqN = reply_to_requester(appName, start_req, res)
    {_AppName, restartType, _Type, _From} = :lists.keyfind(appName, 1, starting)

    case res do
      {:ok, id} ->
        case appName do
          :kernel ->
            check_user()

          _ ->
            :ok
        end

        info_started(appName, nd(id))
        notify_cntrl_started(appName, id, s, :ok)
        nRunning = keyreplaceadd(appName, 1, running, {appName, id})
        nStarted = keyreplaceadd(appName, 1, started, {appName, restartType})

        newS =
          r_state(s,
            starting: keydelete(appName, 1, starting),
            running: nRunning,
            started: nStarted,
            start_req: start_reqN
          )

        perm = :application.get_env(:kernel, :permissions)

        case {perm, id} do
          {:undefined, _} ->
            {:noreply, newS}

          {{:ok, perms}, {:distributed, startNode}}
          when startNode === node() ->
            case :lists.member({appName, false}, perms) do
              true ->
                r_state(running: stopRunning, started: stopStarted) = newS

                case :lists.keyfind(appName, 1, stopRunning) do
                  {^_AppName, ^id} ->
                    {_AppName2, type} = :lists.keyfind(appName, 1, stopStarted)
                    stop_appl(appName, id, type)
                    nStopRunning = keydelete(appName, 1, stopRunning)
                    cntrl(appName, newS, {:ac_application_stopped, appName})
                    {:noreply, r_state(newS, running: nStopRunning, started: stopStarted)}

                  false ->
                    {:noreply, newS}
                end

              false ->
                {:noreply, newS}
            end

          _ ->
            {:noreply, newS}
        end

      {:error, r} = error when restartType === :temporary ->
        notify_cntrl_started(appName, :undefined, s, error)
        info_exited(appName, r, restartType)

        {:noreply,
         r_state(s,
           starting: keydelete(appName, 1, starting),
           start_req: start_reqN
         )}

      {:info, r} when restartType === :temporary ->
        notify_cntrl_started(appName, :undefined, s, {:error, r})

        {:noreply,
         r_state(s,
           starting: keydelete(appName, 1, starting),
           start_req: start_reqN
         )}

      {errInf, r}
      when (restartType === :transient and
              errInf === :error) or
             (restartType === :transient and errInf === :info) ->
        notify_cntrl_started(appName, :undefined, s, {:error, r})

        case errInf do
          :error ->
            info_exited(appName, r, restartType)

          :info ->
            :ok
        end

        case r do
          {{:EXIT, :normal}, _Call} ->
            {:noreply,
             r_state(s,
               starting: keydelete(appName, 1, starting),
               start_req: start_reqN
             )}

          _ ->
            reason = {:application_start_failure, appName, r}
            {:stop, to_string(reason), s}
        end

      {:error, r} = error ->
        notify_cntrl_started(appName, :undefined, s, error)
        info_exited(appName, r, restartType)
        reason = {:application_start_failure, appName, r}
        {:stop, to_string(reason), s}

      {:info, r} ->
        notify_cntrl_started(appName, :undefined, s, {:error, r})
        reason = {:application_start_failure, appName, r}
        {:stop, to_string(reason), s}
    end
  end

  def handle_info({:ac_load_application_reply, appName, res}, s) do
    case keysearchdelete(appName, 1, r_state(s, :loading)) do
      {:value, {_AppName, from}, loading} ->
        :gen_server.reply(from, res)

        case res do
          :ok ->
            {:noreply, r_state(s, loading: loading)}

          {:error, _R} ->
            newS = unload(appName, s)
            {:noreply, r_state(newS, loading: loading)}
        end

      false ->
        {:noreply, s}
    end
  end

  def handle_info(
        {:ac_start_application_reply, appName, res},
        s
      ) do
    start_req = r_state(s, :start_req)

    case :lists.keyfind(appName, 1, starting = r_state(s, :starting)) do
      {_AppName, restartType, type, from} ->
        case res do
          :start_it ->
            {true, appl} = get_loaded(appName)
            spawn_starter(from, appl, s, type)
            {:noreply, s}

          {:started, node} ->
            handle_application_started(appName, {:ok, {:distributed, node}}, s)

          :not_started ->
            started = r_state(s, :started)
            start_reqN = reply_to_requester(appName, start_req, :ok)

            {:noreply,
             r_state(s,
               starting: keydelete(appName, 1, starting),
               started: [{appName, restartType} | started],
               start_req: start_reqN
             )}

          {:takeover, _Node} = takeover ->
            {true, appl} = get_loaded(appName)
            spawn_starter(from, appl, s, takeover)
            newStarting1 = keydelete(appName, 1, starting)

            newStarting = [
              {appName, restartType, takeover, from}
              | newStarting1
            ]

            {:noreply, r_state(s, starting: newStarting)}

          {:error, reason} = error when restartType === :permanent ->
            start_reqN = reply_to_requester(appName, start_req, error)
            {:stop, to_string(reason), r_state(s, start_req: start_reqN)}

          {:error, _Reason} = error ->
            start_reqN = reply_to_requester(appName, start_req, error)

            {:noreply,
             r_state(s,
               starting: keydelete(appName, 1, starting),
               start_req: start_reqN
             )}
        end

      false ->
        {:noreply, s}
    end
  end

  def handle_info({:ac_change_application_req, appName, msg}, s) do
    running = r_state(s, :running)
    started = r_state(s, :started)
    starting = r_state(s, :starting)

    case {keyfind(appName, 1, running), keyfind(appName, 1, started)} do
      {{^appName, id}, {_AppName2, type}} ->
        case msg do
          {:started, node} ->
            stop_appl(appName, id, type)

            nRunning = [
              {appName, {:distributed, node}}
              | keydelete(appName, 1, running)
            ]

            {:noreply, r_state(s, running: nRunning)}

          {:takeover, _Node, _RT} when is_pid(id) ->
            notify_cntrl_started(appName, id, s, :ok)
            {:noreply, s}

          {:takeover, node, rT} ->
            newS = do_start(appName, rT, {:takeover, node}, :undefined, s)
            {:noreply, newS}

          {:failover, _Node, _RT} when is_pid(id) ->
            notify_cntrl_started(appName, id, s, :ok)
            {:noreply, s}

          {:failover, node, rT} ->
            case :application.get_key(appName, :start_phases) do
              {:ok, :undefined} ->
                newS = do_start(appName, rT, :normal, :undefined, s)
                {:noreply, newS}

              {:ok, _StartPhases} ->
                newS = do_start(appName, rT, {:failover, node}, :undefined, s)
                {:noreply, newS}
            end

          :stop_it ->
            stop_appl(appName, id, type)
            cntrl(appName, s, {:ac_application_not_run, appName})
            nRunning = keyreplace(appName, 1, running, {appName, {:distributed, []}})
            {:noreply, r_state(s, running: nRunning)}

          :start_it when is_pid(id) ->
            notify_cntrl_started(appName, id, s, :ok)
            {:noreply, s}

          :start_it ->
            newS = do_start(appName, :undefined, :normal, :undefined, s)
            {:noreply, newS}

          :not_running ->
            nRunning = keydelete(appName, 1, running)
            {:noreply, r_state(s, running: nRunning)}

          _ ->
            {:noreply, s}
        end

      _ ->
        isLoaded = get_loaded(appName)
        isStarting = :lists.keysearch(appName, 1, starting)
        isStarted = :lists.keysearch(appName, 1, started)
        isRunning = :lists.keysearch(appName, 1, running)

        case msg do
          :start_it ->
            case {isLoaded, isStarting, isStarted, isRunning} do
              {_, _, _, {:value, _Tuple}} ->
                {:noreply, s}

              {false, _, _, _} ->
                {:noreply, s}

              {{true, _Appl}, false, false, false} ->
                {:noreply, s}

              {{true, _Appl}, {:value, tuple}, false, false} ->
                {_AppName, _RStype, _Type, from} = tuple
                newS = do_start(appName, :undefined, :normal, from, s)
                {:noreply, newS}

              {{true, _Appl}, _, {:value, {^appName, _RestartType}}, false} ->
                newS = do_start(appName, :undefined, :normal, :undefined, s)
                sS = r_state(newS, started: keydelete(appName, 1, started))
                {:noreply, sS}
            end

          {:started, node} ->
            nRunning = [
              {appName, {:distributed, node}}
              | keydelete(appName, 1, running)
            ]

            {:noreply, r_state(s, running: nRunning)}

          _ ->
            {:noreply, s}
        end
    end
  end

  def handle_info({:EXIT, pid, reason}, s) do
    :ets.match_delete(
      :ac_tab,
      {{:application_master, :_}, pid}
    )

    nRunning = keydelete(pid, 2, r_state(s, :running))
    newS = r_state(s, running: nRunning)

    case :lists.keyfind(pid, 2, r_state(s, :running)) do
      {appName, _AmPid} ->
        cntrl(appName, s, {:ac_application_stopped, appName})

        case :lists.keyfind(appName, 1, r_state(s, :started)) do
          {_AppName, :temporary} ->
            info_exited(appName, reason, :temporary)
            {:noreply, newS}

          {_AppName, :transient} when reason === :normal ->
            info_exited(appName, reason, :transient)
            {:noreply, newS}

          {_AppName, type} ->
            info_exited(appName, reason, type)
            {:stop, to_string({:application_terminated, appName, reason}), newS}
        end

      false ->
        {:noreply, r_state(s, control: del_cntrl(r_state(s, :control), pid))}
    end
  end

  def handle_info(_, s) do
    {:noreply, s}
  end

  def terminate(reason, s) do
    case :application.get_env(:kernel, :shutdown_func) do
      {:ok, {m, f}} ->
        try do
          apply(m, f, [reason])
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      _ ->
        :ok
    end

    shutdownTimeout =
      case :application.get_env(
             :kernel,
             :shutdown_timeout
           ) do
        :undefined ->
          :infinity

        {:ok, t} ->
          t
      end

    foreach(
      fn
        {_AppName, id} when is_pid(id) ->
          ref = :erlang.monitor(:process, id)
          :erlang.unlink(id)
          :erlang.exit(id, :shutdown)

          receive do
            {:EXIT, ^id, _} ->
              :ok
          after
            0 ->
              receive do
                {:DOWN, ^ref, :process, ^id, _} ->
                  :ok
              after
                shutdownTimeout ->
                  :erlang.exit(id, :kill)

                  receive do
                    {:DOWN, ^ref, :process, ^id, _} ->
                      :ok
                  end
              end
          end

        _ ->
          :ok
      end,
      r_state(s, :running)
    )

    true = :ets.delete(:ac_tab)
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp cntrl(appName, r_state(control: control), msg) do
    case :lists.keyfind(appName, 1, control) do
      {_AppName, pid} ->
        send(pid, msg)
        true

      false ->
        false
    end
  end

  defp notify_cntrl_started(_AppName, {:distributed, _Node}, _S, _Res) do
    :ok
  end

  defp notify_cntrl_started(appName, _Id, s, res) do
    cntrl(appName, s, {:ac_application_run, appName, res})
  end

  defp del_cntrl([{_, pid} | t], pid) do
    del_cntrl(t, pid)
  end

  defp del_cntrl([h | t], pid) do
    [h | del_cntrl(t, pid)]
  end

  defp del_cntrl([], _Pid) do
    []
  end

  def get_loaded(app) do
    appName = get_appl_name(app)

    case :ets.lookup(:ac_tab, {:loaded, appName}) do
      [{_Key, appl}] ->
        {true, appl}

      _ ->
        false
    end
  end

  defp do_load_application(application, s) do
    case get_loaded(application) do
      {true, _} ->
        throw({:error, {:already_loaded, application}})

      false ->
        case make_appl(application) do
          {:ok, appl} ->
            load(s, appl)

          error ->
            error
        end
    end
  end

  defp load(
         s,
         {applData, applEnv, incApps, descr, id, vsn, apps}
       ) do
    name = r_appl_data(applData, :name)
    confEnv = get_env_i(name, s)
    newEnv = merge_app_env(applEnv, confEnv)
    cmdLineEnv = get_cmd_env(name)
    newEnv2 = merge_app_env(newEnv, cmdLineEnv)
    add_env(name, newEnv2)

    appl =
      r_appl(
        name: name,
        descr: descr,
        id: id,
        vsn: vsn,
        appl_data: applData,
        inc_apps: incApps,
        apps: apps
      )

    :ets.insert(:ac_tab, {{:loaded, name}, appl})

    newS =
      foldl(
        fn app, s1 ->
          case get_loaded(app) do
            {true, _} ->
              s1

            false ->
              case do_load_application(app, s1) do
                {:ok, s2} ->
                  s2

                error ->
                  throw(error)
              end
          end
        end,
        s,
        incApps
      )

    {:ok, newS}
  end

  defp unload(appName, s) do
    {:ok, incApps} =
      get_key(
        appName,
        :included_applications
      )

    del_env(appName)
    :ets.delete(:ac_tab, {:loaded, appName})

    foldl(
      fn app, s1 ->
        case get_loaded(app) do
          false ->
            s1

          {true, _} ->
            unload(app, s1)
        end
      end,
      s,
      incApps
    )
  end

  defp check_start_cond(appName, restartType, started, running) do
    validRestartType(restartType)

    case get_loaded(appName) do
      {true, appl} ->
        case :lists.keymember(appName, 1, running) do
          true ->
            {:error, {:already_started, appName}}

          false ->
            foreach(
              fn appName2 ->
                case :lists.keymember(appName2, 1, started) do
                  true ->
                    :ok

                  false ->
                    throw({:error, {:not_started, appName2}})
                end
              end,
              r_appl(appl, :apps)
            )

            {:ok, appl}
        end

      false ->
        {:error, {:not_loaded, appName}}
    end
  end

  defp do_start(appName, rT, type, from, s) do
    restartType =
      case :lists.keyfind(appName, 1, r_state(s, :started)) do
        {_AppName2, oldRT} ->
          get_restart_type(rT, oldRT)

        false ->
          rT
      end

    case :lists.keymember(appName, 1, r_state(s, :start_req)) do
      false ->
        {true, appl} = get_loaded(appName)
        start_req = r_state(s, :start_req)
        spawn_starter(:undefined, appl, s, type)

        starting =
          case :lists.keymember(appName, 1, r_state(s, :starting)) do
            false ->
              [{appName, restartType, type, from} | r_state(s, :starting)]

            true ->
              r_state(s, :starting)
          end

        r_state(s,
          starting: starting,
          start_req: [{appName, from} | start_req]
        )

      true ->
        s
    end
  end

  defp spawn_starter(from, appl, s, type) do
    spawn_link(:application_controller, :init_starter, [from, appl, s, type])
  end

  def init_starter(_From, appl, s, type) do
    :erlang.process_flag(:trap_exit, true)
    appName = r_appl(appl, :name)

    :gen_server.cast(
      :application_controller,
      {:application_started, appName,
       try do
         start_appl(appl, s, type)
       catch
         :error, e -> {:EXIT, {e, __STACKTRACE__}}
         :exit, e -> {:EXIT, e}
         e -> e
       end}
    )
  end

  defp reply(:undefined, _Reply) do
    :ok
  end

  defp reply(from, reply) do
    :gen_server.reply(from, reply)
  end

  defp start_appl(appl, s, type) do
    applData = r_appl(appl, :appl_data)

    case r_appl_data(applData, :mod) do
      [] ->
        {:ok, :undefined}

      _ ->
        running = r_state(s, :running)

        foreach(
          fn appName ->
            case :lists.keymember(appName, 1, running) do
              true ->
                :ok

              false ->
                throw({:info, {:not_running, appName}})
            end
          end,
          r_appl(appl, :apps)
        )

        case :application_master.start_link(applData, type) do
          {:ok, _Pid} = ok ->
            ok

          {:error, _Reason} = error ->
            throw(error)
        end
    end
  end

  defp stop_appl(appName, id, type) when is_pid(id) do
    :erlang.unlink(id)
    :application_master.stop(id)
    info_exited(appName, :stopped, type)
    :ets.delete(:ac_tab, {:application_master, appName})
  end

  defp stop_appl(appName, :undefined, type) do
    info_exited(appName, :stopped, type)
  end

  defp stop_appl(_AppName, _Id, _Type) do
    :ok
  end

  defp keysearchdelete(key, pos, list) do
    ksd(key, pos, list, [])
  end

  defp ksd(key, pos, [h | t], rest)
       when :erlang.element(pos, h) === key do
    {:value, h, rest ++ t}
  end

  defp ksd(key, pos, [h | t], rest) do
    ksd(key, pos, t, [h | rest])
  end

  defp ksd(_Key, _Pos, [], _Rest) do
    false
  end

  defp keyreplaceadd(key, pos, list, new) do
    case :lists.keymember(key, pos, list) do
      true ->
        keyreplace(key, pos, list, new)

      false ->
        [new | list]
    end
  end

  defp validRestartType(:permanent) do
    true
  end

  defp validRestartType(:temporary) do
    true
  end

  defp validRestartType(:transient) do
    true
  end

  defp validRestartType(restartType) do
    throw({:error, {:invalid_restart_type, restartType}})
  end

  defp nd({:distributed, node}) do
    node
  end

  defp nd(_) do
    node()
  end

  defp get_restart_type(:undefined, oldRT) do
    oldRT
  end

  defp get_restart_type(rT, _OldRT) do
    rT
  end

  defp get_appl_name(name) when is_atom(name) do
    name
  end

  defp get_appl_name({:application, name, _}) when is_atom(name) do
    name
  end

  defp get_appl_name(appl) do
    throw({:error, {:bad_application, appl}})
  end

  defp make_appl(name) when is_atom(name) do
    fName = :erlang.atom_to_list(name) ++ '.app'

    case :code.where_is_file(fName) do
      :non_existing ->
        {:error, {:file.format_error(:enoent), fName}}

      fullName ->
        case prim_consult(fullName) do
          {:ok, [application]} ->
            {:ok, make_appl_i(application)}

          {:error, reason} ->
            {:error, {:file.format_error(reason), fName}}

          :error ->
            {:error, 'bad encoding'}
        end
    end
  end

  defp make_appl(application) do
    {:ok, make_appl_i(application)}
  end

  defp prim_consult(fullName) do
    case :erl_prim_loader.get_file(fullName) do
      {:ok, bin, _} ->
        case file_binary_to_list(bin) do
          {:ok, string} ->
            case :erl_scan.string(string) do
              {:ok, tokens, _EndLine} ->
                prim_parse(tokens, [])

              {:error, reason, _EndLine} ->
                {:error, reason}
            end

          :error ->
            :error
        end

      :error ->
        {:error, :enoent}
    end
  end

  defp prim_parse(tokens, acc) do
    case :lists.splitwith(
           fn t ->
             :erlang.element(1, t) !== :dot
           end,
           tokens
         ) do
      {[], []} ->
        {:ok, :lists.reverse(acc)}

      {tokens2, [{:dot, _} = dot | rest]} ->
        case :erl_parse.parse_term(tokens2 ++ [dot]) do
          {:ok, term} ->
            prim_parse(rest, [term | acc])

          {:error, _R} = error ->
            error
        end

      {tokens2, []} ->
        case :erl_parse.parse_term(tokens2) do
          {:ok, term} ->
            {:ok, :lists.reverse([term | acc])}

          {:error, _R} = error ->
            error
        end
    end
  end

  defp make_appl_i({:application, name, opts})
       when is_atom(name) and is_list(opts) do
    descr = get_opt(:description, opts, '')
    id = get_opt(:id, opts, '')
    vsn = get_opt(:vsn, opts, '')
    mods = get_opt(:modules, opts, [])
    regs = get_opt(:registered, opts, [])
    apps = get_opt(:applications, opts, [])

    mod =
      case get_opt(:mod, opts, []) do
        {m, _A} = mA when is_atom(m) ->
          mA

        [] ->
          []

        other ->
          throw({:error, {:badstartspec, other}})
      end

    phases = get_opt(:start_phases, opts, :undefined)
    env = get_opt(:env, opts, [])
    maxP = get_opt(:maxP, opts, :infinity)
    maxT = get_opt(:maxT, opts, :infinity)
    incApps = get_opt(:included_applications, opts, [])

    {r_appl_data(
       name: name,
       regs: regs,
       mod: mod,
       phases: phases,
       mods: mods,
       maxP: maxP,
       maxT: maxT
     ), env, incApps, descr, id, vsn, apps}
  end

  defp make_appl_i({:application, name, opts})
       when is_list(opts) do
    throw({:error, {:invalid_name, name}})
  end

  defp make_appl_i({:application, _Name, opts}) do
    throw({:error, {:invalid_options, opts}})
  end

  defp make_appl_i(appl) do
    throw({:error, {:bad_application, appl}})
  end

  defp do_change_apps(applications, config, oldAppls) do
    {:ok, sysConfig, errors} = check_conf_sys(config)

    :lists.foreach(
      fn {:error, {sysFName, line, str}} ->
        case :logger.allow(:error, :application_controller) do
          true ->
            :erlang.apply(:logger, :macro_log, [
              %{
                mfa: {:application_controller, :do_change_apps, 3},
                line: 1553,
                file: 'otp/lib/kernel/src/application_controller.erl'
              },
              :error,
              '~tp: ~w: ~ts~n',
              [sysFName, line, str],
              %{error_logger: %{tag: :error}}
            ])

          false ->
            :ok
        end
      end,
      errors
    )

    {map(
       fn appl ->
         appName = r_appl(appl, :name)

         case is_loaded_app(appName, applications) do
           {true, application} ->
             do_change_appl(make_appl(application), appl, sysConfig)

           false ->
             appl
         end
       end,
       oldAppls
     ), sysConfig}
  end

  defp is_loaded_app(appName, [{:application, appName, app} | _]) do
    {true, {:application, appName, app}}
  end

  defp is_loaded_app(appName, [_ | t]) do
    is_loaded_app(appName, t)
  end

  defp is_loaded_app(_AppName, []) do
    false
  end

  defp do_change_appl({:ok, {applData, env, incApps, descr, id, vsn, apps}}, oldAppl, config) do
    appName = r_appl(oldAppl, :name)
    confEnv = get_opt(appName, config, [])
    newEnv1 = merge_app_env(env, confEnv)
    cmdLineEnv = get_cmd_env(appName)
    newEnv2 = merge_app_env(newEnv1, cmdLineEnv)
    del_env(appName)
    add_env(appName, newEnv2)

    r_appl(oldAppl,
      appl_data: applData,
      descr: descr,
      id: id,
      vsn: vsn,
      inc_apps: incApps,
      apps: apps
    )
  end

  defp do_change_appl({:error, _R} = error, _Appl, _ConfData) do
    throw(error)
  end

  defp get_opt(key, list, default) do
    case :lists.keyfind(key, 1, list) do
      {_Key, val} ->
        val

      _ ->
        default
    end
  end

  defp get_cmd_env(name) do
    case :init.get_argument(name) do
      {:ok, args} ->
        foldl(
          fn list, res ->
            conv(list) ++ res
          end,
          [],
          args
        )

      _ ->
        []
    end
  end

  defp conv([key, val | t]) do
    [{make_term(key), make_term(val)} | conv(t)]
  end

  defp conv(_) do
    []
  end

  defp make_term(str) do
    case :erl_scan.string(str) do
      {:ok, tokens, _} ->
        case :erl_parse.parse_term(tokens ++ [{:dot, :erl_anno.new(1)}]) do
          {:ok, term} ->
            term

          {:error, {_, m, reason}} ->
            handle_make_term_error(m, reason, str)
        end

      {:error, {_, m, reason}, _} ->
        handle_make_term_error(m, reason, str)
    end
  end

  defp handle_make_term_error(mod, reason, str) do
    case :logger.allow(:error, :application_controller) do
      true ->
        :erlang.apply(:logger, :macro_log, [
          %{
            mfa: {:application_controller, :handle_make_term_error, 3},
            line: 1633,
            file: 'otp/lib/kernel/src/application_controller.erl'
          },
          :error,
          'application_controller: ~ts: ~ts~n',
          [mod.format_error(reason), str],
          %{error_logger: %{tag: :error}}
        ])

      false ->
        :ok
    end

    throw({:error, {:bad_environment_value, str}})
  end

  defp get_env_i(name, r_state(conf_data: confData))
       when is_list(confData) do
    case :lists.keyfind(name, 1, confData) do
      {_Name, env} ->
        env

      _ ->
        []
    end
  end

  defp get_env_i(_Name, _) do
    []
  end

  defp merge_env(env1, env2) do
    merge_env(env1, env2, [])
  end

  defp merge_env([{app, appEnv1} | t], env2, res) do
    case get_env_key(app, env2) do
      {:value, appEnv2, restEnv2} ->
        newAppEnv = merge_app_env(appEnv1, appEnv2)
        merge_env(t, restEnv2, [{app, newAppEnv} | res])

      _ ->
        merge_env(t, env2, [{app, appEnv1} | res])
    end
  end

  defp merge_env([], env2, res) do
    env2 ++ res
  end

  defp change_app_env(env, app, fun) do
    case get_env_key(app, env) do
      {:value, appEnv, restEnv} ->
        [{app, fun.(appEnv)} | restEnv]

      _ ->
        [{app, fun.([])} | env]
    end
  end

  defp merge_app_env(env1, env2) do
    merge_app_env(env1, env2, [])
  end

  defp merge_app_env([{key, val} | t], env2, res) do
    case get_env_key(key, env2) do
      {:value, newVal, restEnv} ->
        merge_app_env(t, restEnv, [{key, newVal} | res])

      _ ->
        merge_app_env(t, env2, [{key, val} | res])
    end
  end

  defp merge_app_env([], env2, res) do
    env2 ++ res
  end

  defp get_env_key(key, env) do
    get_env_key(env, key, [])
  end

  defp get_env_key([{key, val} | t], key, res) do
    {:value, val, t ++ res}
  end

  defp get_env_key([h | t], key, res) do
    get_env_key(t, key, [h | res])
  end

  defp get_env_key([], _Key, res) do
    res
  end

  defp add_env(name, env) do
    foreach(
      fn {key, value} ->
        :ets.insert(:ac_tab, {{:env, name, key}, value})
      end,
      env
    )
  end

  defp del_env(name) do
    :ets.match_delete(:ac_tab, {{:env, name, :_}, :_})
  end

  defp check_user() do
    case :erlang.whereis(:user) do
      user when is_pid(user) ->
        :erlang.group_leader(user, self())

      _ ->
        :ok
    end
  end

  defp do_prep_config_change(apps) do
    do_prep_config_change(apps, [])
  end

  defp do_prep_config_change([], envBefore) do
    envBefore
  end

  defp do_prep_config_change([{app, _Id} | apps], envBefore) do
    env = :application.get_all_env(app)
    do_prep_config_change(apps, [{app, env} | envBefore])
  end

  defp do_config_change(apps, envBefore) do
    do_config_change(apps, envBefore, [])
  end

  defp do_config_change([], _EnvBefore, []) do
    :ok
  end

  defp do_config_change([], _EnvBefore, errors) do
    {:error, errors}
  end

  defp do_config_change([{app, _Id} | apps], envBefore, errors) do
    appEnvNow = :lists.sort(:application.get_all_env(app))

    appEnvBefore =
      case :lists.keyfind(app, 1, envBefore) do
        false ->
          []

        {^app, appEnvBeforeT} ->
          :lists.sort(appEnvBeforeT)
      end

    res =
      case appEnvNow do
        ^appEnvBefore ->
          :ok

        _ ->
          case do_config_diff(appEnvNow, appEnvBefore) do
            {[], [], []} ->
              :ok

            {changed, new, removed} ->
              case :application.get_key(app, :mod) do
                {:ok, {mod, _Para}} ->
                  case (try do
                          mod.config_change(changed, new, removed)
                        catch
                          :error, e -> {:EXIT, {e, __STACKTRACE__}}
                          :exit, e -> {:EXIT, e}
                          e -> e
                        end) do
                    :ok ->
                      :ok

                    {:EXIT, {:undef, _}} ->
                      :ok

                    {:error, _} = error ->
                      error

                    else__ ->
                      {:error, else__}
                  end

                {:ok, []} ->
                  {:error, {:module_not_defined, app}}

                :undefined ->
                  {:error, {:application_not_found, app}}
              end
          end
      end

    case res do
      :ok ->
        do_config_change(apps, envBefore, errors)

      {:error, newError} ->
        do_config_change(apps, envBefore, [newError | errors])
    end
  end

  def do_config_diff(appEnvNow, appEnvBefore) do
    do_config_diff(appEnvNow, appEnvBefore, {[], []})
  end

  defp do_config_diff([], appEnvBefore, {changed, new}) do
    removed =
      :lists.foldl(
        fn {env, _Value}, acc ->
          [env | acc]
        end,
        [],
        appEnvBefore
      )

    {changed, new, removed}
  end

  defp do_config_diff(appEnvNow, [], {changed, new}) do
    {changed, appEnvNow ++ new, []}
  end

  defp do_config_diff([{env, value} | appEnvNow], appEnvBefore, {changed, new}) do
    case :lists.keyfind(env, 1, appEnvBefore) do
      {^env, ^value} ->
        do_config_diff(appEnvNow, :lists.keydelete(env, 1, appEnvBefore), {changed, new})

      {^env, _OtherValue} ->
        do_config_diff(
          appEnvNow,
          :lists.keydelete(env, 1, appEnvBefore),
          {[{env, value} | changed], new}
        )

      false ->
        do_config_diff(appEnvNow, appEnvBefore, {changed, [{env, value} | new]})
    end
  end

  defp check_conf() do
    case :init.get_argument(:config) do
      {:ok, files} ->
        {:ok,
         :lists.foldl(
           fn file, env ->
             bFName = :filename.basename(file, '.config')

             fName =
               :filename.join(
                 :filename.dirname(file),
                 bFName ++ '.config'
               )

             case load_file(fName) do
               {:ok, newEnv} ->
                 cond do
                   bFName === 'sys' ->
                     dName = :filename.dirname(fName)
                     {:ok, sysEnv, errors} = check_conf_sys(newEnv, [], [], dName)

                     case errors do
                       [] ->
                         merge_env(env, sysEnv)

                       [{:error, {sysFName, line, str}} | _] ->
                         throw({:error, {sysFName, line, str}})
                     end

                   true ->
                     merge_env(env, newEnv)
                 end

               {:error, {line, _Mod, str}} ->
                 throw({:error, {fName, line, str}})
             end
           end,
           [],
           :lists.append(files)
         )}

      _ ->
        {:ok, []}
    end
  end

  defp check_conf_sys(env) do
    check_conf_sys(env, [], [], [])
  end

  defp check_conf_sys([file | t], sysEnv, errors, dName)
       when is_list(file) and is_list(dName) do
    bFName = :filename.basename(file, '.config')

    fName =
      :filename.join(
        :filename.dirname(file),
        bFName ++ '.config'
      )

    lName =
      case :filename.pathtype(fName) do
        :relative when dName !== [] ->
          rName = :filename.join(dName, fName)

          case :erl_prim_loader.read_file_info(rName) do
            {:ok, _} ->
              rName

            :error ->
              fName
          end

        _ ->
          fName
      end

    case load_file(lName) do
      {:ok, newEnv} ->
        check_conf_sys(t, merge_env(sysEnv, newEnv), errors, dName)

      {:error, {line, _Mod, str}} ->
        check_conf_sys(t, sysEnv, [{:error, {lName, line, str}} | errors], dName)
    end
  end

  defp check_conf_sys([tuple | t], sysEnv, errors, dName) do
    check_conf_sys(t, merge_env(sysEnv, [tuple]), errors, dName)
  end

  defp check_conf_sys([], sysEnv, errors, _) do
    {:ok, sysEnv, :lists.reverse(errors)}
  end

  defp load_file(file) do
    case :erl_prim_loader.get_file(file) do
      {:ok, bin, _FileName} ->
        case file_binary_to_list(bin) do
          {:ok, string} ->
            scan_file(string ++ ' ')

          :error ->
            {:error, {:none, :scan_file, 'bad encoding'}}
        end

      :error ->
        {:error, {:none, :open_file, 'configuration file not found'}}
    end
  end

  defp scan_file(str) do
    case :erl_scan.tokens([], str, 1) do
      {:done, {:ok, tokens, _}, left} ->
        case :erl_parse.parse_term(tokens) do
          {:ok, l} = res when is_list(l) ->
            case only_ws(left) do
              true ->
                res

              false ->
                config_error()
            end

          {:ok, _} ->
            config_error()

          error ->
            error
        end

      {:done, result, _} ->
        {:error, {:none, :parse_file, :erlang.tuple_to_list(result)}}

      {:more, _} ->
        {:error, {:none, :load_file, 'no ending <dot> found'}}
    end
  end

  defp only_ws([c | cs]) when c <= ?\s do
    only_ws(cs)
  end

  defp only_ws([?% | cs]) do
    only_ws(strip_comment(cs))
  end

  defp only_ws([_ | _]) do
    false
  end

  defp only_ws([]) do
    true
  end

  defp strip_comment([?\n | cs]) do
    cs
  end

  defp strip_comment([_ | cs]) do
    strip_comment(cs)
  end

  defp strip_comment([]) do
    []
  end

  defp config_error() do
    {:error, {:none, :load_file, 'configuration file must contain ONE list ended by <dot>'}}
  end

  defp info_started(name, node) do
    case :logger.allow(:info, :application_controller) do
      true ->
        :erlang.apply(:logger, :macro_log, [
          %{
            mfa: {:application_controller, :info_started, 2},
            line: 1931,
            file: 'otp/lib/kernel/src/application_controller.erl'
          },
          :info,
          %{
            label: {:application_controller, :progress},
            report: [{:application, name}, {:started_at, node}]
          },
          %{
            domain: [:otp, :sasl],
            report_cb: &:application_controller.format_log/2,
            logger_formatter: %{title: 'PROGRESS REPORT'},
            error_logger: %{
              tag: :info_report,
              type: :progress,
              report_cb: &:application_controller.format_log/1
            }
          }
        ])

      false ->
        :ok
    end
  end

  defp info_exited(name, reason, type) do
    case :logger.allow(
           :notice,
           :application_controller
         ) do
      true ->
        :erlang.apply(:logger, :macro_log, [
          %{
            mfa: {:application_controller, :info_exited, 3},
            line: 1943,
            file: 'otp/lib/kernel/src/application_controller.erl'
          },
          :notice,
          %{
            label: {:application_controller, :exit},
            report: [{:application, name}, {:exited, reason}, {:type, type}]
          },
          %{
            domain: [:otp],
            report_cb: &:application_controller.format_log/2,
            error_logger: %{
              tag: :info_report,
              type: :std_info,
              report_cb: &:application_controller.format_log/1
            }
          }
        ])

      false ->
        :ok
    end
  end

  def format_log(logReport) do
    depth = :error_logger.get_format_depth()
    formatOpts = %{chars_limit: :unlimited, depth: depth, single_line: false, encoding: :utf8}

    format_log_multi(
      limit_report(logReport, depth),
      formatOpts
    )
  end

  defp limit_report(logReport, :unlimited) do
    logReport
  end

  defp limit_report(
         %{
           label: {:application_controller, :progress},
           report: [{:application, _} = application, {:started_at, node}]
         } = logReport,
         depth
       ) do
    Map.put(logReport, :report, [application, {:started_at, :io_lib.limit_term(node, depth)}])
  end

  defp limit_report(
         %{
           label: {:application_controller, :exit},
           report: [{:application, _} = application, {:exited, reason}, {:type, type}]
         } = logReport,
         depth
       ) do
    Map.put(logReport, :report, [
      application,
      {:exited, :io_lib.limit_term(reason, depth)},
      {:type, :io_lib.limit_term(type, depth)}
    ])
  end

  def format_log(report, formatOpts0) do
    default = %{chars_limit: :unlimited, depth: :unlimited, single_line: false, encoding: :utf8}
    formatOpts = :maps.merge(default, formatOpts0)

    ioOpts =
      case formatOpts do
        %{chars_limit: :unlimited} ->
          []

        %{chars_limit: limit} ->
          [{:chars_limit, limit}]
      end

    {format, args} = format_log_single(report, formatOpts)
    :io_lib.format(format, args, ioOpts)
  end

  defp format_log_single(
         %{
           label: {:application_controller, :progress},
           report: [{:application, name}, {:started_at, node}]
         },
         %{single_line: true, depth: depth} = formatOpts
       ) do
    p = p(formatOpts)
    format = 'Application: ' ++ p ++ '. Started at: ' ++ p ++ '.'

    args =
      case depth do
        :unlimited ->
          [name, node]

        _ ->
          [name, depth, node, depth]
      end

    {format, args}
  end

  defp format_log_single(
         %{
           label: {:application_controller, :exit},
           report: [{:application, name}, {:exited, reason}, {:type, type}]
         },
         %{single_line: true, depth: depth} = formatOpts
       ) do
    p = p(formatOpts)
    format = :lists.append(['Application: ', p, '. Exited: ', p, '. Type: ', p, '.'])

    args =
      case depth do
        :unlimited ->
          [name, reason, type]

        _ ->
          [name, depth, reason, depth, type, depth]
      end

    {format, args}
  end

  defp format_log_single(report, formatOpts) do
    format_log_multi(report, formatOpts)
  end

  defp format_log_multi(
         %{
           label: {:application_controller, :progress},
           report: [{:application, name}, {:started_at, node}]
         },
         %{depth: depth} = formatOpts
       ) do
    p = p(formatOpts)
    format = :lists.append(['    application: ', p, '~n', '    started_at: ', p, '~n'])

    args =
      case depth do
        :unlimited ->
          [name, node]

        _ ->
          [name, depth, node, depth]
      end

    {format, args}
  end

  defp format_log_multi(
         %{
           label: {:application_controller, :exit},
           report: [{:application, name}, {:exited, reason}, {:type, type}]
         },
         %{depth: depth} = formatOpts
       ) do
    p = p(formatOpts)

    format =
      :lists.append(['    application: ', p, '~n', '    exited: ', p, '~n', '    type: ', p, '~n'])

    args =
      case depth do
        :unlimited ->
          [name, reason, type]

        _ ->
          [name, depth, reason, depth, type, depth]
      end

    {format, args}
  end

  defp p(%{single_line: single, depth: depth, encoding: enc}) do
    '~' ++ single(single) ++ mod(enc) ++ p(depth)
  end

  defp p(:unlimited) do
    'p'
  end

  defp p(_Depth) do
    'P'
  end

  defp single(true) do
    '0'
  end

  defp single(false) do
    ''
  end

  defp mod(:latin1) do
    ''
  end

  defp mod(_) do
    't'
  end

  defp reply_to_requester(appName, start_req, res) do
    r =
      case res do
        {:ok, _Id} ->
          :ok

        {:info, reason} ->
          {:error, reason}

        error ->
          error
      end

    :lists.foldl(
      fn sp, accIn ->
        case sp do
          {^appName, from} ->
            reply(from, r)
            accIn

          _ ->
            [sp | accIn]
        end
      end,
      [],
      start_req
    )
  end

  defp update_permissions(appName, bool) do
    t = {:env, :kernel, :permissions}

    case :ets.lookup(:ac_tab, t) do
      [] ->
        :ets.insert(:ac_tab, {t, [{appName, bool}]})

      [{_, perm}] ->
        perm2 = :lists.keydelete(appName, 1, perm)
        :ets.insert(:ac_tab, {t, [{appName, bool} | perm2]})
    end
  end

  def test_change_apps(apps, conf) do
    res = test_make_apps(apps, [])
    test_do_change_appl(apps, conf, res)
  end

  defp test_do_change_appl([], _, _) do
    :ok
  end

  defp test_do_change_appl([a | apps], [], [r | res]) do
    _ = do_change_appl(r, r_appl(name: a), [])
    test_do_change_appl(apps, [], res)
  end

  defp test_do_change_appl([a | apps], [c | conf], [r | res]) do
    _ = do_change_appl(r, r_appl(name: a), c)
    test_do_change_appl(apps, conf, res)
  end

  defp test_make_apps([], res) do
    :lists.reverse(res)
  end

  defp test_make_apps([a | apps], res) do
    test_make_apps(apps, [make_appl(a) | res])
  end

  defp file_binary_to_list(bin) do
    enc =
      case :epp.read_encoding_from_binary(bin) do
        :none ->
          :epp.default_encoding()

        encoding ->
          encoding
      end

    case (try do
            :unicode.characters_to_list(bin, enc)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      string when is_list(string) ->
        {:ok, string}

      _ ->
        :error
    end
  end

  defp to_string(term) do
    case :io_lib.printable_list(term) do
      true ->
        term

      false ->
        :lists.flatten(:io_lib.format('~0p', [term]))
    end
  end
end
