defmodule :m_dist_ac do
  use Bitwise

  import :lists,
    only: [
      filter: 2,
      foldl: 3,
      foreach: 2,
      keydelete: 3,
      keyreplace: 4,
      keysearch: 3,
      map: 2,
      mapfoldl: 3,
      member: 2,
      zf: 2
    ]

  @behaviour :gen_server
  require Record

  Record.defrecord(:r_state, :state,
    appls: [],
    tmp_locals: [],
    remote_started: [],
    known: [],
    started: [],
    tmp_weights: [],
    dist_loaded: [],
    t_reqs: [],
    s_reqs: [],
    p_reqs: []
  )

  Record.defrecord(:r_appl, :appl,
    name: :undefined,
    id: :undefined,
    restart_time: 0,
    nodes: [],
    run: []
  )

  def start_link() do
    case :gen_server.start_link({:local, :dist_ac}, :dist_ac, [], []) do
      {:ok, pid} ->
        :gen_server.cast(:dist_ac, :init_sync)
        {:ok, pid}

      else__ ->
        else__
    end
  end

  def load_application(appName, distNodes) do
    :gen_server.call(:dist_ac, {:load_application, appName, distNodes}, :infinity)
  end

  def takeover_application(appName, restartType) do
    case valid_restart_type(restartType) do
      true ->
        wait_for_sync_dacs()
        nodes = get_nodes(appName)

        :global.trans(
          {:dist_ac, self()},
          fn ->
            :gen_server.call(
              :dist_ac,
              {:takeover_application, appName, restartType},
              :infinity
            )
          end,
          nodes
        )

      false ->
        {:error, {:invalid_restart_type, restartType}}
    end
  end

  def permit_application(appName, bool) do
    wait_for_sync_dacs()
    lockId = {:dist_ac, self()}

    :global.trans(
      lockId,
      fn ->
        :gen_server.call(
          :dist_ac,
          {:permit_application, appName, bool, lockId, :started},
          :infinity
        )
      end
    )
  end

  def permit_only_loaded_application(appName, bool) do
    wait_for_sync_dacs()
    lockId = {:dist_ac, self()}

    :global.trans(
      lockId,
      fn ->
        :gen_server.call(
          :dist_ac,
          {:permit_application, appName, bool, lockId, :only_loaded},
          :infinity
        )
      end
    )
  end

  defp get_nodes(appName) do
    :gen_server.call(:dist_ac, {:get_nodes, appName}, :infinity)
  end

  def get_known_nodes() do
    :gen_server.call(:dist_ac, :get_known_nodes)
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    {:ok, r_state()}
  end

  defp sync_dacs(appls) do
    res =
      :global.trans(
        {:dist_ac, :sync_dacs},
        fn ->
          nodes = introduce_me(:erlang.nodes(), appls)
          wait_dacs(nodes, [node()], appls, [])
        end
      )

    :ets.insert(:ac_tab, {:sync_dacs, :ok})
    res
  end

  defp introduce_me(nodes, appls) do
    msg = {:dist_ac_new_node, 1, node(), appls, []}

    filter(
      fn node ->
        case :rpc.call(node, :erlang, :whereis, [:dist_ac]) do
          pid when is_pid(pid) ->
            send(pid, msg)
            true

          _ ->
            false
        end
      end,
      nodes
    )
  end

  defp wait_dacs([node | nodes], knownNodes, appls, rStarted) do
    :erlang.monitor_node(node, true)

    receive do
      {:dist_ac_new_node, _Vsn, ^node, hisAppls, hisStarted} ->
        :erlang.monitor_node(node, false)
        nRStarted = rStarted ++ hisStarted
        nAppls = dist_merge(appls, hisAppls, node)
        wait_dacs(nodes, [node | knownNodes], nAppls, nRStarted)

      {:nodedown, ^node} ->
        :erlang.monitor_node(node, false)
        wait_dacs(nodes, knownNodes, appls, rStarted)
    end
  end

  defp wait_dacs([], knownNodes, appls, rStarted) do
    {knownNodes, appls, rStarted}
  end

  def info() do
    :gen_server.call(:dist_ac, :info)
  end

  defp wait_for_sync_dacs() do
    case (try do
            :ets.lookup(:ac_tab, :sync_dacs)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      [{:sync_dacs, :ok}] ->
        :ok

      _ ->
        receive do
        after
          100 ->
            :ok
        end

        wait_for_sync_dacs()
    end
  end

  def handle_cast(:init_sync, _S) do
    receive do
      {:go, kernelConfig} ->
        appls =
          case :application.get_env(
                 :kernel,
                 :distributed
               ) do
            {:ok, d} ->
              dist_check(d)

            :undefined ->
              []
          end

        dist_take_control(appls)
        send(kernelConfig, :dist_ac_took_control)
        :ok = :net_kernel.monitor_nodes(true)
        {known, nAppls, rStarted} = sync_dacs(appls)
        {:noreply, r_state(appls: nAppls, known: known, remote_started: rStarted)}
    end
  end

  def handle_call(:info, _From, s) do
    {:reply, s, s}
  end

  def handle_call({:load_application, appName, distNodes}, _From, s) do
    appls = r_state(s, :appls)

    case (try do
            dist_replace(distNodes, appName, appls)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, error} ->
        {:reply, {:error, error}, s}

      {:EXIT, r} ->
        {:stop, r, {:error, r}, s}

      nAppls ->
        newS =
          case dist_find_nodes(nAppls, appName) do
            [] ->
              s

            _Nodes ->
              ensure_take_control(appName, appls)
              {:ok, s2} = load(appName, r_state(s, appls: nAppls))
              s2
          end

        {:reply, :ok, newS}
    end
  end

  def handle_call({:takeover_application, appName, restartType}, from, s) do
    appls = r_state(s, :appls)

    case keysearch(appName, r_appl(:name), appls) do
      {:value, appl}
      when :erlang.element(
             1,
             r_appl(appl, :id)
           ) === :distributed ->
        {:distributed, node} = r_appl(appl, :id)
        _ = ac_takeover(:req, appName, node, restartType)
        nAppl = r_appl(appl, id: :takeover)
        nAppls = keyreplace(appName, r_appl(:name), appls, nAppl)
        tR = r_state(s, :t_reqs)
        {:noreply, r_state(s, appls: nAppls, t_reqs: [{appName, from} | tR])}

      {:value, r_appl(id: :local)} ->
        {:reply, {:error, {:already_running_locally, appName}}, s}

      _ ->
        {:reply, {:error, {:not_running_distributed, appName}}, s}
    end
  end

  def handle_call({:permit_application, appName, bool, lockId, startInfo}, from, s) do
    case :lists.keymember(appName, r_appl(:name), r_state(s, :appls)) do
      false ->
        case :application_controller.get_loaded(appName) do
          {true, _} when not bool ->
            _ = ac_stop_it(appName)
            {:reply, :ok, s}

          {true, _} when bool ->
            _ = ac_start_it(:req, appName)
            {:reply, :ok, s}

          false ->
            {:reply, {:error, {:not_loaded, appName}}, s}
        end

      true ->
        nAppls = dist_update_run(r_state(s, :appls), appName, node(), bool)
        newS = r_state(s, appls: nAppls)
        isRunning = keysearch(appName, r_appl(:name), nAppls)

        isMyApp =
          case isRunning do
            {:value, r_appl(id: :local)} ->
              true

            _ ->
              false
          end

        nodes = dist_flat_nodes(nAppls, appName)
        msg = {:dist_ac_new_permission, node(), appName, bool, isMyApp}
        send_msg(msg, nodes)

        case startInfo do
          :only_loaded ->
            {:reply, :ok, newS}

          :started ->
            permit(bool, isRunning, appName, from, newS, lockId)
        end
    end
  end

  def handle_call({:distribution_changed, newDistribution}, _From, s) do
    appls = r_state(s, :appls)
    newAppls = dist_change_update(appls, newDistribution)
    newS = r_state(s, appls: newAppls)
    {:reply, :ok, newS}
  end

  def handle_call({:get_nodes, appName}, _From, s) do
    alive =
      intersection(
        dist_flat_nodes(
          r_state(s, :appls),
          appName
        ),
        r_state(s, :known)
      )

    {:reply, alive, s}
  end

  def handle_call(:get_known_nodes, _From, s) do
    {:reply, r_state(s, :known), s}
  end

  def handle_info({:ac_load_application_req, appName}, s) do
    {:ok, newS} = load(appName, s)
    send(:application_controller, {:ac_load_application_reply, appName, :ok})
    {:noreply, newS}
  end

  def handle_info({:ac_application_unloaded, appName}, s) do
    {:ok, newS} = unload(appName, s)
    {:noreply, newS}
  end

  def handle_info({:ac_start_application_req, appName}, s) do
    lock = {:dist_ac, self()}

    case :global.set_lock(lock, [node()], 0) do
      true ->
        s2 =
          case (try do
                  start_appl(appName, s, :reply)
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            {:ok, newS, _} ->
              newS

            {:error, r} ->
              send(:application_controller, {:ac_start_application_reply, appName, {:error, r}})
              s
          end

        :global.del_lock(lock)
        {:noreply, s2}

      false ->
        send_after(100, {:ac_start_application_req, appName})
        {:noreply, s}
    end
  end

  def handle_info({:ac_application_run, appName, res}, s) do
    appls = r_state(s, :appls)
    nodes = r_state(s, :known)

    send_msg(
      {:dist_ac_app_started, node(), appName, res},
      nodes
    )

    nId =
      case res do
        :ok ->
          :local

        {:error, _R} ->
          :undefined
      end

    {:value, appl} = keysearch(appName, r_appl(:name), appls)
    nTReqs = del_t_reqs(appName, r_state(s, :t_reqs), res)
    nAppl = r_appl(appl, id: nId)
    nAppls = keyreplace(appName, r_appl(:name), appls, nAppl)
    {:noreply, r_state(s, appls: nAppls, t_reqs: nTReqs)}
  end

  def handle_info({:ac_application_not_run, appName}, s) do
    {:value, appl} = keysearch(appName, r_appl(:name), appls = r_state(s, :appls))
    nTReqs = del_t_reqs(appName, r_state(s, :t_reqs), {:error, :stopped})

    sReqs =
      filter(
        fn
          {name, from2} when name === appName ->
            :gen_server.reply(from2, :ok)
            false

          _ ->
            true
        end,
        r_state(s, :s_reqs)
      )

    rS =
      case r_appl(appl, :id) do
        :local ->
          send_msg({:dist_ac_app_stopped, appName}, r_state(s, :known))
          r_state(s, :remote_started)

        {:distributed, node} ->
          [{node, appName} | r_state(s, :remote_started)]

        _ ->
          r_state(s, :remote_started)
      end

    nAppl = r_appl(appl, id: :undefined)
    nAppls = keyreplace(appName, r_appl(:name), appls, nAppl)
    {:noreply, r_state(s, appls: nAppls, t_reqs: nTReqs, s_reqs: sReqs, remote_started: rS)}
  end

  def handle_info({:ac_application_stopped, appName}, s) do
    {:value, appl} = keysearch(appName, r_appl(:name), appls = r_state(s, :appls))
    nTReqs = del_t_reqs(appName, r_state(s, :t_reqs), {:error, :stopped})

    sReqs =
      filter(
        fn
          {name, from2} when name === appName ->
            :gen_server.reply(from2, :ok)
            false

          _ ->
            true
        end,
        r_state(s, :s_reqs)
      )

    rS =
      case r_appl(appl, :id) do
        :local ->
          send_msg({:dist_ac_app_stopped, appName}, r_state(s, :known))
          r_state(s, :remote_started)

        {:distributed, node} ->
          [{node, appName} | r_state(s, :remote_started)]

        _ ->
          r_state(s, :remote_started)
      end

    nAppl = r_appl(appl, id: :undefined)
    nAppls = keyreplace(appName, r_appl(:name), appls, nAppl)
    started = :lists.delete(appName, r_state(s, :started))

    {:noreply,
     r_state(s, appls: nAppls, started: started, t_reqs: nTReqs, s_reqs: sReqs, remote_started: rS)}
  end

  def handle_info(
        {:dist_ac_new_node, _Vsn, node, hisAppls, []},
        s
      ) do
    appls = r_state(s, :appls)

    myStarted =
      zf(
        fn
          appl when r_appl(appl, :id) === :local ->
            {true, {node(), r_appl(appl, :name)}}

          _ ->
            false
        end,
        appls
      )

    send({:dist_ac, node}, {:dist_ac_new_node, 1, node(), appls, myStarted})
    nAppls = dist_merge(appls, hisAppls, node)
    {:noreply, r_state(s, appls: nAppls, known: [node | r_state(s, :known)])}
  end

  def handle_info({:dist_ac_app_started, node, name, res}, s) do
    case {keysearch(name, r_appl(:name), r_state(s, :appls)),
          :lists.member(name, r_state(s, :started))} do
      {{:value, appl}, true} ->
        appls = r_state(s, :appls)

        nId =
          case r_appl(appl, :id) do
            _ when :erlang.element(1, res) === :error ->
              :undefined

            {:distributed, _} ->
              {:distributed, node}

            :local ->
              {:distributed, node}

            _ ->
              {:distributed, node}
          end

        _ = ac_started(:req, name, node)
        nAppl = r_appl(appl, id: nId)
        nAppls = keyreplace(name, r_appl(:name), appls, nAppl)
        tmpWeights = keydelete_all(name, 1, r_state(s, :tmp_weights))
        newS = r_state(s, appls: nAppls, tmp_weights: tmpWeights)

        nPermitReq =
          req_del_permit_false(
            r_state(newS, :p_reqs),
            name
          )

        case (try do
                req_start_app(r_state(newS, p_reqs: nPermitReq), name)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:error, r} ->
            {:stop, r}

          {:ok, newS2} ->
            {:noreply, newS2}
        end

      {_, _} ->
        nRStarted = [{node, name} | r_state(s, :remote_started)]
        {:noreply, r_state(s, remote_started: nRStarted)}
    end
  end

  def handle_info({:dist_ac_app_stopped, appName}, s) do
    appls = r_state(s, :appls)

    case keysearch(appName, r_appl(:name), appls) do
      false ->
        rStarted = keydelete(appName, 2, r_state(s, :remote_started))
        {:noreply, r_state(s, remote_started: rStarted)}

      {:value, appl} ->
        nAppl = r_appl(appl, id: :undefined)
        nAppls = keyreplace(appName, r_appl(:name), appls, nAppl)
        rStarted = keydelete(appName, 2, r_state(s, :remote_started))
        {:noreply, r_state(s, appls: nAppls, remote_started: rStarted)}
    end
  end

  def handle_info({:dist_ac_weight, name, weight, node}, s) do
    case keysearch(name, r_appl(:name), r_state(s, :appls)) do
      {:value, appl} ->
        id = r_appl(appl, :id)

        case id do
          :run_waiting ->
            send({:dist_ac, node}, {:dist_ac_weight, name, 0, node()})
            {:noreply, s}

          :undefined ->
            {:noreply,
             r_state(s,
               tmp_locals: [
                 {name, weight, node}
                 | r_state(s, :tmp_locals)
               ]
             )}

          {:takeover, _} ->
            {:noreply,
             r_state(s,
               tmp_locals: [
                 {name, weight, node}
                 | r_state(s, :tmp_locals)
               ]
             )}

          {:failover, _} ->
            {:noreply,
             r_state(s,
               tmp_locals: [
                 {name, weight, node}
                 | r_state(s, :tmp_locals)
               ]
             )}

          _ ->
            myWeight = get_cached_weight(name, s)
            send({:dist_ac, node}, {:dist_ac_weight, name, myWeight, node()})
            nTWs = keyreplaceadd(name, 1, r_state(s, :tmp_weights), {name, myWeight})
            {:noreply, r_state(s, tmp_weights: nTWs)}
        end

      _ ->
        {:noreply,
         r_state(s,
           tmp_locals: [
             {name, weight, node}
             | r_state(s, :tmp_locals)
           ]
         )}
    end
  end

  def handle_info({:nodedown, node}, s) do
    appNames = dist_get_runnable(r_state(s, :appls))

    hisAppls =
      filter(
        fn
          r_appl(
            name: name,
            id: {:distributed, n}
          )
          when node === n ->
            :lists.member(name, appNames)

          _ ->
            false
        end,
        r_state(s, :appls)
      )

    appls2 =
      zf(
        fn
          appl when r_appl(appl, :id) === {:distributed, node} ->
            case :lists.member(r_appl(appl, :name), appNames) do
              true ->
                {true, r_appl(appl, id: {:failover, node})}

              false ->
                _ = ac_not_running(r_appl(appl, :name))
                {true, r_appl(appl, id: :undefined)}
            end

          _ ->
            true
        end,
        r_state(s, :appls)
      )

    rStarted =
      filter(
        fn
          {node2, _Name} when node2 === node ->
            false

          _ ->
            true
        end,
        r_state(s, :remote_started)
      )

    appls3 = dist_del_node(appls2, node)
    {nPermitReq, appls4, sReqs} = req_del_node(s, node, appls3)
    nKnown = :lists.delete(node, r_state(s, :known))

    newS =
      r_state(s,
        appls: appls4,
        p_reqs: nPermitReq,
        known: nKnown,
        s_reqs: sReqs,
        remote_started: rStarted
      )

    restart_appls(hisAppls)
    {:noreply, newS}
  end

  def handle_info(
        {:dist_ac_app_loaded, node, name, hisNodes, permission, heKnowsMe},
        s
      ) do
    nodes = dist_find_nodes(appls = r_state(s, :appls), name)

    case is_loaded(name, s) do
      true ->
        case equal_nodes(nodes, hisNodes) do
          true ->
            nAppls = dist_update_run(appls, name, node, permission)

            cond do
              not heKnowsMe ->
                msg =
                  {:dist_ac_app_loaded, node(), name, nodes, dist_is_runnable(appls, name), true}

                send({:dist_ac, node}, msg)
                :ok

              true ->
                :ok
            end

            {:noreply, r_state(s, appls: nAppls)}

          false ->
            dist_mismatch(name, node)
        end

      false ->
        load = [
          {{name, node}, hisNodes, permission}
          | r_state(s, :dist_loaded)
        ]

        {:noreply, r_state(s, dist_loaded: load)}
    end
  end

  def handle_info({:dist_ac_app_unloaded, node, name}, s) do
    appls = dist_update_run(r_state(s, :appls), name, node, :undefined)
    load = keydelete({name, node}, 1, r_state(s, :dist_loaded))
    {:noreply, r_state(s, appls: appls, dist_loaded: load)}
  end

  def handle_info(
        {:dist_ac_new_permission, node, appName, false, isHisApp},
        s
      ) do
    appls = dist_update_run(r_state(s, :appls), appName, node, false)
    newS = r_state(s, appls: appls)

    case dist_is_runnable(appls, appName) do
      true when isHisApp ->
        case (try do
                start_appl(appName, newS, :req)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:ok, newS2, _} ->
            {:noreply, newS2}

          {:error, _R} ->
            {:noreply, newS}
        end

      _ ->
        {:noreply, newS}
    end
  end

  def handle_info(
        {:dist_ac_new_permission, node, appName, true, _IsHisApp},
        s
      ) do
    appls = dist_update_run(r_state(s, :appls), appName, node, true)
    {:noreply, r_state(s, appls: appls)}
  end

  def handle_info({:internal_restart_appl, name}, s) do
    case restart_appl(name, s) do
      {:error, r} ->
        {:stop, {:error, r}, s}

      newS ->
        {:noreply, newS}
    end
  end

  def handle_info(_, s) do
    {:noreply, s}
  end

  def terminate(_Reason, _S) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp load(appName, s) do
    appls0 = r_state(s, :appls)

    distLoaded =
      get_dist_loaded(
        appName,
        load1 = r_state(s, :dist_loaded)
      )

    nodes = dist_find_nodes(appls0, appName)
    fNodes = flat_nodes(nodes)
    permission = get_default_permission(appName)
    appls1 = dist_update_run(appls0, appName, node(), permission)

    {loadedNodes, appls2} =
      mapfoldl(
        fn {node, hisNodes, hisPermission}, appls ->
          case equal_nodes(nodes, hisNodes) do
            true ->
              {node, dist_update_run(appls, appName, node, hisPermission)}

            _ ->
              dist_mismatch(appName, node)
          end
        end,
        appls1,
        distLoaded
      )

    load2 = del_dist_loaded(appName, load1)

    foreach(
      fn
        node when node !== node() ->
          msg =
            {:dist_ac_app_loaded, node(), appName, nodes, permission, member(node, loadedNodes)}

          send({:dist_ac, node}, msg)

        _ ->
          :ok
      end,
      fNodes
    )

    {:ok, r_state(s, appls: appls2, dist_loaded: load2)}
  end

  defp ensure_take_control(appName, appls) do
    case :lists.keymember(appName, r_appl(:name), appls) do
      true ->
        :ok

      false ->
        :application_controller.control_application(appName)
    end
  end

  defp unload(appName, s) do
    appls = r_state(s, :appls)
    nodes = dist_flat_nodes(appls, appName)
    msg = {:dist_ac_app_unloaded, node(), appName}
    send_msg(msg, nodes)
    {:value, appl} = keysearch(appName, r_appl(:name), appls)
    nAppl = r_appl(appl, id: :undefined, run: [])
    {:ok, r_state(s, appls: keyreplace(appName, r_appl(:name), appls, nAppl))}
  end

  defp start_appl(appName, s, type) do
    appl =
      case keysearch(appName, r_appl(:name), appls = r_state(s, :appls)) do
        {:value, a} ->
          a

        _ ->
          throw({:error, {:unknown_application, appName}})
      end

    case r_appl(appl, :id) do
      :local ->
        {:ok, s, false}

      _ ->
        {id, isWaiting} =
          case dist_get_all_nodes(appl) do
            {:ok, distNodes, permittedNodes} ->
              start_distributed(appl, appName, distNodes, permittedNodes, s, type)

            error ->
              throw(error)
          end

        nAppl = r_appl(appl, id: id)
        nAppls = keyreplaceadd(appName, r_appl(:name), appls, nAppl)

        {:ok, newS} =
          req_start_app(
            r_state(s, appls: nAppls),
            appName
          )

        tmpLocals = keydelete_all(appName, 1, r_state(newS, :tmp_locals))
        tmpWeights = keydelete_all(appName, 1, r_state(newS, :tmp_weights))
        rStarted = keydelete(appName, 2, r_state(s, :remote_started))
        started = replaceadd(appName, r_state(newS, :started))

        {:ok,
         r_state(newS,
           started: started,
           tmp_locals: tmpLocals,
           tmp_weights: tmpWeights,
           remote_started: rStarted
         ), isWaiting}
    end
  end

  defp start_distributed(appl, name, nodes, permittedNodes, s, type) do
    case find_start_node(nodes, permittedNodes, name, s) do
      {:ok, node} when node === node() ->
        _ =
          case r_appl(appl, :id) do
            {:failover, foNode} when type === :req ->
              ac_failover(name, foNode, :undefined)

            {:distributed, node2} when type === :req ->
              ac_takeover(:req, name, node2, :undefined)

            _ when type === :reply ->
              case :lists.keysearch(name, 2, r_state(s, :remote_started)) do
                {:value, {node3, _}} ->
                  ac_takeover(:reply, name, node3, :undefined)

                _ ->
                  ac_start_it(type, name)
              end

            _ ->
              ac_start_it(type, name)
          end

        {:run_waiting, true}

      {:already_started, node} ->
        _ = ac_started(type, name, node)
        {{:distributed, node}, false}

      {:ok, node} ->
        case keysearch(name, r_appl(:name), r_state(s, :appls)) do
          {:value, r_appl(id: {:distributed, ^node})} ->
            _ = ac_started(type, name, node)
            {{:distributed, node}, false}

          _ ->
            wait_dist_start(node, appl, name, nodes, permittedNodes, s, type)
        end

      :not_started ->
        wait_dist_start2(appl, name, nodes, permittedNodes, s, type)

      :no_permission ->
        _ = ac_not_started(type, name)
        {:undefined, false}
    end
  end

  defp wait_dist_start(node, appl, name, nodes, permittedNodes, s, type) do
    :erlang.monitor_node(node, true)

    receive do
      {:dist_ac_app_started, ^node, ^name, :ok} ->
        _ = ac_started(type, name, node)
        :erlang.monitor_node(node, false)
        {{:distributed, node}, false}

      {:dist_ac_app_started, ^node, ^name, {:error, r}} ->
        _ = ac_error(type, name, {node, r})
        :erlang.monitor_node(node, false)
        {r_appl(appl, :id), false}

      {:dist_ac_weight, ^name, _Weigth, ^node} ->
        :erlang.monitor_node(node, false)
        send({:dist_ac, node}, {:dist_ac_weight, name, get_cached_weight(name, s), node()})
        wait_dist_start(node, appl, name, nodes, permittedNodes, s, type)

      {:nodedown, ^node} ->
        :erlang.monitor_node(node, false)

        tmpLocals =
          filter(
            fn
              {name2, _Weight, node2}
              when node2 === node and name2 === name ->
                false

              _ ->
                true
            end,
            r_state(s, :tmp_locals)
          )

        newS = r_state(s, tmp_locals: tmpLocals)
        start_distributed(appl, name, nodes, :lists.delete(node, permittedNodes), newS, type)
    end
  end

  defp wait_dist_start2(appl, name, nodes, permittedNodes, s, type) do
    receive do
      {:dist_ac_app_started, node, ^name, :ok} ->
        _ = ac_started(type, name, node)
        {{:distributed, node}, false}

      {:dist_ac_app_started, node, ^name, {:error, r}} ->
        _ = ac_error(type, name, {node, r})
        {r_appl(appl, :id), false}

      {:nodedown, node} ->
        tmpLocals =
          filter(
            fn
              {name2, _Weight, node2}
              when node2 === node and name2 === name ->
                false

              _ ->
                true
            end,
            r_state(s, :tmp_locals)
          )

        newS = r_state(s, tmp_locals: tmpLocals)
        start_distributed(appl, name, nodes, :lists.delete(node, permittedNodes), newS, type)
    end
  end

  defp ac_start_it(:reply, name) do
    send(:application_controller, {:ac_start_application_reply, name, :start_it})
  end

  defp ac_start_it(:req, name) do
    send(:application_controller, {:ac_change_application_req, name, :start_it})
  end

  defp ac_started(:reply, name, node) do
    send(:application_controller, {:ac_start_application_reply, name, {:started, node}})
  end

  defp ac_started(:req, name, node) do
    send(:application_controller, {:ac_change_application_req, name, {:started, node}})
  end

  defp ac_error(:reply, name, error) do
    send(:application_controller, {:ac_start_application_reply, name, {:error, error}})
  end

  defp ac_error(:req, _Name, _Error) do
    :ok
  end

  defp ac_not_started(:reply, name) do
    send(:application_controller, {:ac_start_application_reply, name, :not_started})
  end

  defp ac_not_started(:req, name) do
    send(:application_controller, {:ac_change_application_req, name, :stop_it})
  end

  defp ac_stop_it(name) do
    send(:application_controller, {:ac_change_application_req, name, :stop_it})
  end

  defp ac_takeover(:reply, name, node, _RestartType) do
    send(:application_controller, {:ac_start_application_reply, name, {:takeover, node}})
  end

  defp ac_takeover(:req, name, node, restartType) do
    send(
      :application_controller,
      {:ac_change_application_req, name, {:takeover, node, restartType}}
    )
  end

  defp ac_failover(name, node, restartType) do
    send(
      :application_controller,
      {:ac_change_application_req, name, {:failover, node, restartType}}
    )
  end

  defp ac_not_running(name) do
    send(:application_controller, {:ac_change_application_req, name, :not_running})
  end

  defp restart_appls(appls) do
    foreach(
      fn appl ->
        appName = r_appl(appl, :name)

        send_after(
          r_appl(appl, :restart_time),
          {:internal_restart_appl, appName}
        )
      end,
      :lists.reverse(appls)
    )
  end

  defp restart_appl(appName, s) do
    case keysearch(appName, r_appl(:name), r_state(s, :appls)) do
      {:value, appl}
      when :erlang.element(
             1,
             r_appl(appl, :id)
           ) === :failover ->
        case (try do
                start_appl(appName, s, :req)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:ok, newS, _} ->
            newS

          {:error, r} ->
            error_msg('Error when restarting application ~p: ~p~n', [appName, r])
            s
        end

      _ ->
        s
    end
  end

  defp permit(false, {:value, r_appl(id: :undefined)}, _AppName, _From, s, _LockId) do
    {:reply, :ok, s}
  end

  defp permit(false, {:value, r_appl(id: id)}, _AppName, _From, s, _LockId)
       when :erlang.element(1, id) === :distributed do
    {:reply, :ok, s}
  end

  defp permit(false, {:value, _}, appName, from, s, _LockId) do
    case dist_get_runnable_nodes(r_state(s, :appls), appName) do
      [] ->
        _ = ac_stop_it(appName)
        sReqs = [{appName, from} | r_state(s, :s_reqs)]
        {:noreply, r_state(s, s_reqs: sReqs)}

      nodes ->
        pR = req_del_permit_true(r_state(s, :p_reqs), appName)
        nPReqs = [{from, appName, false, nodes} | pR]
        {:noreply, r_state(s, p_reqs: nPReqs)}
    end
  end

  defp permit(true, {:value, r_appl(id: :local)}, _AppName, _From, s, _LockId) do
    {:reply, :ok, s}
  end

  defp permit(true, _, appName, from, s, lockId) do
    case (try do
            start_appl(appName, s, :req)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {_ErrorTag, {:not_running, app}} ->
        pR = req_del_permit_false(r_state(s, :p_reqs), appName)
        nPReqs = [{false, appName, true, app} | pR]
        {:reply, :ok, r_state(s, p_reqs: nPReqs)}

      {:ok, newS, true} ->
        tR = r_state(newS, :t_reqs)
        :global.del_lock(lockId)
        {:noreply, r_state(newS, t_reqs: [{appName, from} | tR])}

      {:ok, _S, false} ->
        {:reply, :ok, s}

      {_ErrorTag, r} ->
        {:stop, r, {:error, r}, s}
    end
  end

  defp do_start_appls(startApps, s) do
    sortedStartApps = startApps
    appls = r_state(s, :appls)

    {:ok,
     foldl(
       fn appName, newS ->
         case (try do
                 start_appl(appName, newS, :req)
               catch
                 :error, e -> {:EXIT, {e, __STACKTRACE__}}
                 :exit, e -> {:EXIT, e}
                 e -> e
               end) do
           {:error, r} ->
             throw({{:error, newS}, r})

           {:ok, newS2, _} ->
             newS2
         end
       end,
       r_state(s, appls: appls),
       :lists.reverse(sortedStartApps)
     )}
  end

  defp find_start_node(nodes, permittedNodes, name, s) do
    allNodes =
      intersection(
        flat_nodes(nodes),
        permittedNodes
      )

    case :lists.member(node(), allNodes) do
      true ->
        weight = get_cached_weight(name, s)
        find_start_node(nodes, name, s, weight, allNodes)

      false ->
        case keysearch(name, 2, r_state(s, :remote_started)) do
          {:value, {node, _Name}} ->
            {:already_started, node}

          _ when allNodes !== [] ->
            :not_started

          _ ->
            :no_permission
        end
    end
  end

  defp find_start_node([anyNodes | nodes], name, s, weight, allNodes)
       when is_tuple(anyNodes) do
    case find_any_node(:erlang.tuple_to_list(anyNodes), name, s, weight, allNodes) do
      false ->
        find_start_node(nodes, name, s, weight, allNodes)

      res ->
        res
    end
  end

  defp find_start_node([node | nodes], name, s, weight, allNodes) do
    case :lists.member(node, allNodes) do
      true ->
        case keysearch(name, r_appl(:name), r_state(s, :appls)) do
          {:value, r_appl(id: {:distributed, ^node})} ->
            {:already_started, node}

          _ ->
            case keysearch(name, 2, r_state(s, :remote_started)) do
              {:value, {^node, _Name}} ->
                {:already_started, node}

              _ ->
                {:ok, node}
            end
        end

      false ->
        find_start_node(nodes, name, s, weight, allNodes)
    end
  end

  defp find_start_node([], _Name, _S, _Weight, _AllNodes) do
    :not_started
  end

  defp find_any_node(anyNodes, name, s, weight, allNodes) do
    case check_running(name, s, intersection(anyNodes, allNodes)) do
      {:already_started, node} ->
        {:already_started, node}

      false ->
        send_nodes(
          allNodes,
          {:dist_ac_weight, name, weight, node()}
        )

        answers = [{weight, node()} | collect_answers(allNodes, name, s, [])]

        find_alive_node(
          :lists.sort(answers),
          intersection(anyNodes, r_state(s, :known))
        )
    end
  end

  defp check_running(name, r_state(remote_started: rStarted, appls: appls), anyNodes) do
    case keysearch(name, 2, rStarted) do
      {:value, {node, _Name}} ->
        case :lists.member(node, anyNodes) do
          true ->
            {:already_started, node}

          false ->
            false
        end

      false ->
        case keysearch(name, r_appl(:name), appls) do
          {:value, r_appl(id: {:distributed, node})} ->
            case :lists.member(node, anyNodes) do
              true ->
                {:already_started, node}

              false ->
                false
            end

          _ ->
            false
        end
    end
  end

  defp find_alive_node([{_, node} | nodes], aliveNodes) do
    case :lists.member(node, aliveNodes) do
      true ->
        {:ok, node}

      false ->
        find_alive_node(nodes, aliveNodes)
    end
  end

  defp find_alive_node([], _AliveNodes) do
    false
  end

  defp collect_answers([node | nodes], name, s, res)
       when node !== node() do
    case keysearch(node, 3, r_state(s, :tmp_locals)) do
      {:value, {^name, weight, ^node}} ->
        collect_answers(nodes, name, s, [{weight, node} | res])

      _ ->
        :erlang.monitor_node(node, true)

        receive do
          {:dist_ac_weight, ^name, weight, ^node} ->
            :erlang.monitor_node(node, false)
            collect_answers(nodes, name, s, [{weight, node} | res])

          {:nodedown, ^node} ->
            :erlang.monitor_node(node, false)
            collect_answers(nodes, name, s, res)
        end
    end
  end

  defp collect_answers([_ThisNode | nodes], name, s, res) do
    collect_answers(nodes, name, s, res)
  end

  defp collect_answers([], _Name, _S, res) do
    res
  end

  defp send_nodes(nodes, msg) do
    flatNodes = flat_nodes(nodes)

    foreach(
      fn
        node when node !== node() ->
          send({:dist_ac, node}, msg)

        _ThisNode ->
          :ok
      end,
      flatNodes
    )
  end

  defp send_after(time, msg)
       when is_integer(time) and
              time >= 0 do
    _Pid = spawn_link(:dist_ac, :send_timeout, [self(), time, msg])
    :ok
  end

  defp send_after(_, _) do
    :ok
  end

  def send_timeout(to, time, msg) do
    receive do
    after
      time ->
        send(to, msg)
    end
  end

  defp send_msg(msg, nodes) do
    foreach(
      fn
        node when node !== node() ->
          send({:dist_ac, node}, msg)

        _ ->
          :ok
      end,
      nodes
    )
  end

  defp replaceadd(item, list) do
    case member(item, list) do
      true ->
        list

      false ->
        [item | list]
    end
  end

  defp keyreplaceadd(key, pos, list, new) do
    case :lists.keymember(key, pos, list) do
      true ->
        :lists.keyreplace(key, pos, list, new)

      false ->
        [new | list]
    end
  end

  defp keydelete_all(key, n, [h | t])
       when :erlang.element(
              n,
              h
            ) === key do
    keydelete_all(key, n, t)
  end

  defp keydelete_all(key, n, [h | t]) do
    [h | keydelete_all(key, n, t)]
  end

  defp keydelete_all(_Key, _N, []) do
    []
  end

  defp equal_nodes([h | t1], [h | t2]) when is_atom(h) do
    equal_nodes(t1, t2)
  end

  defp equal_nodes([h1 | t1], [h2 | t2])
       when is_tuple(h1) and
              is_tuple(h2) do
    case equal(
           :erlang.tuple_to_list(h1),
           :erlang.tuple_to_list(h2)
         ) do
      true ->
        equal_nodes(t1, t2)

      false ->
        false
    end
  end

  defp equal_nodes([], []) do
    true
  end

  defp equal_nodes(_, _) do
    false
  end

  defp equal([h | t], s) do
    case :lists.member(h, s) do
      true ->
        equal(t, :lists.delete(h, s))

      false ->
        false
    end
  end

  defp equal([], []) do
    true
  end

  defp equal(_, _) do
    false
  end

  defp flat_nodes(nodes) when is_list(nodes) do
    foldl(
      fn
        node, res when is_atom(node) ->
          [node | res]

        tuple, res when is_tuple(tuple) ->
          :erlang.tuple_to_list(tuple) ++ res
      end,
      [],
      nodes
    )
  end

  defp flat_nodes(nodes) do
    throw({:error, {:badarg, nodes}})
  end

  defp get_cached_weight(name, s) do
    case :lists.keysearch(name, 1, r_state(s, :tmp_weights)) do
      {:value, {_, w}} ->
        w

      _ ->
        get_weight()
    end
  end

  defp get_weight() do
    length(:application.which_applications())
  end

  defp get_dist_loaded(
         name,
         [{{name, node}, hisNodes, permission} | t]
       ) do
    [
      {node, hisNodes, permission}
      | get_dist_loaded(
          name,
          t
        )
    ]
  end

  defp get_dist_loaded(name, [_H | t]) do
    get_dist_loaded(name, t)
  end

  defp get_dist_loaded(_Name, []) do
    []
  end

  defp del_dist_loaded(
         name,
         [{{name, _Node}, _HisNodes, _Permission} | t]
       ) do
    del_dist_loaded(name, t)
  end

  defp del_dist_loaded(name, [h | t]) do
    [h | del_dist_loaded(name, t)]
  end

  defp del_dist_loaded(_Name, []) do
    []
  end

  defp req_start_app(state, name) do
    {:ok,
     foldl(
       fn
         {false, appName, true, name2}, s
         when name === name2 ->
           pR = keydelete(appName, 2, r_state(s, :p_reqs))
           nS = r_state(s, p_reqs: pR)

           case (try do
                   do_start_appls([appName], nS)
                 catch
                   :error, e -> {:EXIT, {e, __STACKTRACE__}}
                   :exit, e -> {:EXIT, e}
                   e -> e
                 end) do
             {_ErrorTag, {:not_running, app}} ->
               nRequests = [{false, appName, true, app} | pR]
               r_state(s, p_reqs: nRequests)

             {:ok, newS} ->
               newS

             {_ErrorTag, r} ->
               throw({:error, r})
           end

         _, s ->
           s
       end,
       state,
       r_state(state, :p_reqs)
     )}
  end

  defp req_del_permit_true(reqs, name) do
    filter(
      fn
        {from, name2, true, _} when name2 === name ->
          :gen_server.reply(from, :ok)
          false

        _ ->
          true
      end,
      reqs
    )
  end

  defp req_del_permit_false(reqs, name) do
    filter(
      fn
        {from, name2, false, _Nodes}
        when name2 === name ->
          :gen_server.reply(from, :ok)
          false

        _ ->
          true
      end,
      reqs
    )
  end

  defp req_del_node(s, node, appls) do
    check_waiting(r_state(s, :p_reqs), s, node, appls, [], r_state(s, :s_reqs))
  end

  defp del_t_reqs(appName, tReqs, res) do
    :lists.filter(
      fn
        {aN, from} when appName === aN ->
          :gen_server.reply(from, res)
          false

        _ ->
          true
      end,
      tReqs
    )
  end

  defp check_waiting([{from, appName, false, nodes} | reqs], s, node, appls, res, sReqs) do
    case :lists.delete(node, nodes) do
      [] ->
        _ = ac_stop_it(appName)
        nSReqs = [{appName, from} | sReqs]
        check_waiting(reqs, node, s, appls, res, nSReqs)

      nNodes ->
        check_waiting(reqs, node, s, appls, [{from, appName, false, nNodes} | res], sReqs)
    end
  end

  defp check_waiting([h | reqs], s, node, appls, res, sReqs) do
    check_waiting(reqs, node, s, appls, [h | res], sReqs)
  end

  defp check_waiting([], _Node, _S, appls, res, sReqs) do
    {res, appls, sReqs}
  end

  defp intersection([], _) do
    []
  end

  defp intersection(_, []) do
    []
  end

  defp intersection(l1, l2) do
    l1 -- l1 -- l2
  end

  defp get_default_permission(appName) do
    case :application.get_env(:kernel, :permissions) do
      {:ok, permissions} ->
        case keysearch(appName, 1, permissions) do
          {:value, {_, true}} ->
            true

          {:value, {_, false}} ->
            false

          {:value, {_, x}} ->
            exit({:bad_permission, {appName, x}})

          false ->
            true
        end

      :undefined ->
        true
    end
  end

  defp dist_check([{appName, nodes} | t]) do
    p = get_default_permission(appName)

    [
      r_appl(name: appName, nodes: nodes, run: [{node(), p}])
      | dist_check(t)
    ]
  end

  defp dist_check([{appName, time, nodes} | t])
       when is_integer(time) and time >= 0 do
    p = get_default_permission(appName)

    [
      r_appl(name: appName, restart_time: time, nodes: nodes, run: [{node(), p}])
      | dist_check(t)
    ]
  end

  defp dist_check([{appName, :infinity, nodes} | t]) do
    p = get_default_permission(appName)

    [
      r_appl(name: appName, restart_time: :infinity, nodes: nodes, run: [{node(), p}])
      | dist_check(t)
    ]
  end

  defp dist_check([_ | t]) do
    dist_check(t)
  end

  defp dist_check([]) do
    []
  end

  defp dist_take_control(appls) do
    foreach(
      fn r_appl(name: appName) ->
        :application_controller.control_application(appName)
      end,
      appls
    )
  end

  defp dist_replace(:default, _Name, appls) do
    appls
  end

  defp dist_replace({appName, nodes}, appName, appls) do
    run =
      for node <- flat_nodes(nodes) do
        {node, :undefined}
      end

    keyreplaceadd(
      appName,
      r_appl(:name),
      appls,
      r_appl(name: appName, restart_time: 0, nodes: nodes, run: run)
    )
  end

  defp dist_replace({appName, time, nodes}, appName, appls)
       when is_integer(time) and time >= 0 do
    run =
      for node <- flat_nodes(nodes) do
        {node, :undefined}
      end

    keyreplaceadd(
      appName,
      r_appl(:name),
      appls,
      r_appl(name: appName, restart_time: time, nodes: nodes, run: run)
    )
  end

  defp dist_replace(bad, _Name, _Appls) do
    throw({:error, {:bad_distribution_spec, bad}})
  end

  defp dist_update_run(appls, appName, node, permission) do
    map(
      fn
        appl when r_appl(appl, :name) === appName ->
          run = r_appl(appl, :run)
          nRun = keyreplaceadd(node, 1, run, {node, permission})
          r_appl(appl, run: nRun)

        appl ->
          appl
      end,
      appls
    )
  end

  defp dist_change_update(appls, []) do
    appls
  end

  defp dist_change_update(appls, [{appName, newNodes} | newDist]) do
    newAppls = do_dist_change_update(appls, appName, 0, newNodes)
    dist_change_update(newAppls, newDist)
  end

  defp dist_change_update(
         appls,
         [{appName, newTime, newNodes} | newDist]
       ) do
    newAppls = do_dist_change_update(appls, appName, newTime, newNodes)
    dist_change_update(newAppls, newDist)
  end

  defp do_dist_change_update(appls, appName, newTime, newNodes) do
    map(
      fn
        appl when r_appl(appl, :name) === appName ->
          r_appl(appl, restart_time: newTime, nodes: newNodes)

        appl ->
          appl
      end,
      appls
    )
  end

  defp dist_merge(myAppls, hisAppls, hisNode) do
    zf(
      fn appl ->
        r_appl(name: appName, run: run) = appl
        heIsMember = true

        case keysearch(appName, r_appl(:name), hisAppls) do
          {:value, r_appl(run: hisRun)} when heIsMember ->
            case keysearch(hisNode, 1, hisRun) do
              {:value, val} ->
                nRun = keyreplaceadd(hisNode, 1, run, val)
                {true, r_appl(appl, run: nRun)}

              false ->
                val = {hisNode, :undefined}
                {true, r_appl(appl, run: [val | run])}
            end

          _ ->
            true
        end
      end,
      myAppls
    )
  end

  defp dist_get_runnable_nodes(appls, appName) do
    case keysearch(appName, r_appl(:name), appls) do
      {:value, r_appl(run: run)} ->
        zf(
          fn
            {node, true} ->
              {true, node}

            _ ->
              false
          end,
          run
        )

      false ->
        []
    end
  end

  defp dist_is_runnable(appls, appName) do
    case keysearch(appName, r_appl(:name), appls) do
      {:value, r_appl(run: run)} ->
        case keysearch(node(), 1, run) do
          {:value, {_, true}} ->
            true

          _ ->
            false
        end

      false ->
        false
    end
  end

  defp is_loaded(appName, r_state(appls: appls)) do
    case keysearch(appName, r_appl(:name), appls) do
      {:value, r_appl(run: run)} ->
        case keysearch(node(), 1, run) do
          {:value, {_Node, :undefined}} ->
            false

          {:value, _} ->
            true

          false ->
            false
        end

      false ->
        false
    end
  end

  defp dist_get_runnable(appls) do
    zf(
      fn r_appl(name: appName, run: run) ->
        case keysearch(node(), 1, run) do
          {:value, {_, true}} ->
            {true, appName}

          _ ->
            false
        end
      end,
      appls
    )
  end

  defp dist_get_all_nodes(r_appl(name: appName, nodes: nodes, run: run)) do
    {res, badNodes} = check_nodes(run, [], [])

    case intersection(
           badNodes,
           :erlang.nodes(:connected)
         ) do
      [] ->
        {:ok, nodes, res}

      _ ->
        {:error, {:app_not_loaded, appName, badNodes}}
    end
  end

  defp check_nodes([{node, :undefined} | t], res, badNodes) do
    check_nodes(t, res, [node | badNodes])
  end

  defp check_nodes([{node, true} | t], res, badNodes) do
    check_nodes(t, [node | res], badNodes)
  end

  defp check_nodes([{_Node, false} | t], res, badNodes) do
    check_nodes(t, res, badNodes)
  end

  defp check_nodes([], res, badNodes) do
    {res, badNodes}
  end

  defp dist_find_nodes([r_appl(name: name, nodes: nodes) | _], name) do
    nodes
  end

  defp dist_find_nodes([_ | t], name) do
    dist_find_nodes(t, name)
  end

  defp dist_find_nodes([], _Name) do
    []
  end

  defp dist_flat_nodes(appls, name) do
    flat_nodes(dist_find_nodes(appls, name))
  end

  defp dist_del_node(appls, node) do
    map(
      fn appl ->
        nRun =
          filter(
            fn
              {n, _Runnable} when n === node ->
                false

              _ ->
                true
            end,
            r_appl(appl, :run)
          )

        r_appl(appl, run: nRun)
      end,
      appls
    )
  end

  defp valid_restart_type(:permanent) do
    true
  end

  defp valid_restart_type(:temporary) do
    true
  end

  defp valid_restart_type(:transient) do
    true
  end

  defp valid_restart_type(_RestartType) do
    false
  end

  defp dist_mismatch(appName, node) do
    error_msg('Distribution mismatch for application "~p" on nodes ~p and ~p~n', [
      appName,
      node(),
      node
    ])

    exit({:distribution_mismatch, appName, node})
  end

  defp error_msg(format, argList)
       when is_list(format) and
              is_list(argList) do
    :error_logger.error_msg('dist_ac on node ~p:~n' ++ format, [node() | argList])
  end
end
