defmodule :m_global_group do
  use Bitwise
  import Kernel, except: [send: 2]
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_state, :state,
    sync_state: :no_conf,
    connect_all: :undefined,
    group_name: [],
    nodes: [],
    no_contact: [],
    sync_error: [],
    other_grps: [],
    node_name: node(),
    monitor: [],
    publish_type: :normal,
    group_publish_type: :normal
  )

  def global_groups() do
    request(:global_groups)
  end

  def monitor_nodes(flag) do
    case flag do
      true ->
        request({:monitor_nodes, flag})

      false ->
        request({:monitor_nodes, flag})

      _ ->
        {:error, :not_boolean}
    end
  end

  def own_nodes() do
    request(:own_nodes)
  end

  def registered_names(arg) do
    request({:registered_names, arg})
  end

  def send(name, msg) do
    request({:send, name, msg})
  end

  def send(group, name, msg) do
    request({:send, group, name, msg})
  end

  def whereis_name(name) do
    request({:whereis_name, name})
  end

  def whereis_name(group, name) do
    request({:whereis_name, group, name})
  end

  def global_groups_changed(newPara) do
    request({:global_groups_changed, newPara})
  end

  def global_groups_added(newPara) do
    request({:global_groups_added, newPara})
  end

  def global_groups_removed(newPara) do
    request({:global_groups_removed, newPara})
  end

  def sync() do
    request(:sync)
  end

  def ng_add_check(node, othersNG) do
    ng_add_check(node, :normal, othersNG)
  end

  def ng_add_check(node, pubType, othersNG) do
    request({:ng_add_check, node, pubType, othersNG})
  end

  def info() do
    request(:info, 3000)
  end

  def registered_names_test(arg) do
    request({:registered_names_test, arg})
  end

  def send_test(name, msg) do
    request({:send_test, name, msg})
  end

  def whereis_name_test(name) do
    request({:whereis_name_test, name})
  end

  defp request(req) do
    request(req, :infinity)
  end

  defp request(req, time) do
    case :erlang.whereis(:global_group) do
      p when is_pid(p) ->
        :gen_server.call(:global_group, req, time)

      _Other ->
        {:error, :global_group_not_runnig}
    end
  end

  def start() do
    :gen_server.start({:local, :global_group}, :global_group, [], [])
  end

  def start_link() do
    :gen_server.start_link({:local, :global_group}, :global_group, [], [])
  end

  def stop() do
    :gen_server.call(:global_group, :stop, :infinity)
  end

  def init([]) do
    :erlang.process_flag(:priority, :max)
    :ok = :net_kernel.monitor_nodes(true)
    :erlang.put(:registered_names, [:undefined])
    :erlang.put(:send, [:undefined])
    :erlang.put(:whereis_name, [:undefined])
    :erlang.process_flag(:trap_exit, true)

    ca =
      case :init.get_argument(:connect_all) do
        {:ok, [['false']]} ->
          false

        _ ->
          true
      end

    pT = publish_arg()

    case :application.get_env(:kernel, :global_groups) do
      :undefined ->
        update_publish_nodes(pT)
        {:ok, r_state(publish_type: pT, connect_all: ca)}

      {:ok, []} ->
        update_publish_nodes(pT)
        {:ok, r_state(publish_type: pT, connect_all: ca)}

      {:ok, nodeGrps} ->
        {defGroupName, pubTpGrp, defNodes, defOther} =
          case (try do
                  config_scan(nodeGrps, :publish_type)
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            {:error, _Error2} ->
              update_publish_nodes(pT)
              exit({:error, {:"invalid global_groups definition", nodeGrps}})

            {defGroupNameT, pubType, defNodesT, defOtherT} ->
              update_publish_nodes(pT, {pubType, defNodesT})
              disconnect_nodes(:erlang.nodes(:connected) -- defNodesT)

              :lists.foreach(
                fn node ->
                  :erlang.monitor_node(node, true)
                end,
                defNodesT
              )

              {defGroupNameT, pubType, :lists.delete(node(), defNodesT), defOtherT}
          end

        {:ok,
         r_state(
           publish_type: pT,
           group_publish_type: pubTpGrp,
           sync_state: :synced,
           group_name: defGroupName,
           no_contact: :lists.sort(defNodes),
           other_grps: defOther,
           connect_all: ca
         )}
    end
  end

  def handle_call(:sync, _From, s) do
    case :application.get_env(:kernel, :global_groups) do
      :undefined ->
        update_publish_nodes(r_state(s, :publish_type))
        {:reply, :ok, s}

      {:ok, []} ->
        update_publish_nodes(r_state(s, :publish_type))
        {:reply, :ok, s}

      {:ok, nodeGrps} ->
        {defGroupName, pubTpGrp, defNodes, defOther} =
          case (try do
                  config_scan(nodeGrps, :publish_type)
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            {:error, _Error2} ->
              exit({:error, {:"invalid global_groups definition", nodeGrps}})

            {defGroupNameT, pubType, defNodesT, defOtherT} ->
              update_publish_nodes(
                r_state(s, :publish_type),
                {pubType, defNodesT}
              )

              disconnect_nodes(:erlang.nodes(:connected) -- defNodesT)
              kill_global_group_check()

              pid =
                spawn_link(:global_group, :sync_init, [:sync, defGroupNameT, pubType, defNodesT])

              :erlang.register(:global_group_check, pid)
              {defGroupNameT, pubType, :lists.delete(node(), defNodesT), defOtherT}
          end

        {:reply, :ok,
         r_state(s,
           sync_state: :synced,
           group_name: defGroupName,
           no_contact: :lists.sort(defNodes),
           other_grps: defOther,
           group_publish_type: pubTpGrp
         )}
    end
  end

  def handle_call(:global_groups, _From, s) do
    result =
      case r_state(s, :sync_state) do
        :no_conf ->
          :undefined

        :synced ->
          other =
            :lists.foldl(
              fn {n, _L}, acc ->
                acc ++ [n]
              end,
              [],
              r_state(s, :other_grps)
            )

          {r_state(s, :group_name), other}
      end

    {:reply, result, s}
  end

  def handle_call({:monitor_nodes, flag}, {pid, _}, stateIn) do
    {res, state} = monitor_nodes(flag, pid, stateIn)
    {:reply, res, state}
  end

  def handle_call(:own_nodes, _From, s) do
    nodes =
      case r_state(s, :sync_state) do
        :no_conf ->
          [node() | :erlang.nodes()]

        :synced ->
          get_own_nodes()
      end

    {:reply, nodes, s}
  end

  def handle_call({:registered_names, {:group, group}}, _From, s)
      when group === r_state(s, :group_name) do
    res = :global.registered_names()
    {:reply, res, s}
  end

  def handle_call({:registered_names, {:group, group}}, from, s) do
    case :lists.keysearch(group, 1, r_state(s, :other_grps)) do
      false ->
        {:reply, [], s}

      {:value, {^group, []}} ->
        {:reply, [], s}

      {:value, {^group, nodes}} ->
        pid =
          :global_search.start(
            :names,
            {:group, nodes, from}
          )

        wait = :erlang.get(:registered_names)
        :erlang.put(:registered_names, [{pid, from} | wait])
        {:noreply, s}
    end
  end

  def handle_call({:registered_names, {:node, node}}, _From, s)
      when node === node() do
    res = :global.registered_names()
    {:reply, res, s}
  end

  def handle_call({:registered_names, {:node, node}}, from, s) do
    pid = :global_search.start(:names, {:node, node, from})
    wait = :erlang.get(:registered_names)
    :erlang.put(:registered_names, [{pid, from} | wait])
    {:noreply, s}
  end

  def handle_call({:send, name, msg}, from, s) do
    case :global.whereis_name(name) do
      :undefined ->
        pid =
          :global_search.start(
            :send,
            {:any, r_state(s, :other_grps), name, msg, from}
          )

        wait = :erlang.get(:send)
        :erlang.put(:send, [{pid, from, name, msg} | wait])
        {:noreply, s}

      found ->
        send(found, msg)
        {:reply, found, s}
    end
  end

  def handle_call({:send, {:group, grp}, name, msg}, _From, s)
      when grp === r_state(s, :group_name) do
    case :global.whereis_name(name) do
      :undefined ->
        {:reply, {:badarg, {name, msg}}, s}

      pid ->
        send(pid, msg)
        {:reply, pid, s}
    end
  end

  def handle_call({:send, {:group, group}, name, msg}, from, s) do
    case :lists.keysearch(group, 1, r_state(s, :other_grps)) do
      false ->
        {:reply, {:badarg, {name, msg}}, s}

      {:value, {^group, []}} ->
        {:reply, {:badarg, {name, msg}}, s}

      {:value, {^group, nodes}} ->
        pid =
          :global_search.start(
            :send,
            {:group, nodes, name, msg, from}
          )

        wait = :erlang.get(:send)
        :erlang.put(:send, [{pid, from, name, msg} | wait])
        {:noreply, s}
    end
  end

  def handle_call({:send, {:node, node}, name, msg}, from, s) do
    pid =
      :global_search.start(
        :send,
        {:node, node, name, msg, from}
      )

    wait = :erlang.get(:send)
    :erlang.put(:send, [{pid, from, name, msg} | wait])
    {:noreply, s}
  end

  def handle_call({:whereis_name, name}, from, s) do
    case :global.whereis_name(name) do
      :undefined ->
        pid =
          :global_search.start(
            :whereis,
            {:any, r_state(s, :other_grps), name, from}
          )

        wait = :erlang.get(:whereis_name)
        :erlang.put(:whereis_name, [{pid, from} | wait])
        {:noreply, s}

      found ->
        {:reply, found, s}
    end
  end

  def handle_call({:whereis_name, {:group, group}, name}, _From, s)
      when group === r_state(s, :group_name) do
    res = :global.whereis_name(name)
    {:reply, res, s}
  end

  def handle_call({:whereis_name, {:group, group}, name}, from, s) do
    case :lists.keysearch(group, 1, r_state(s, :other_grps)) do
      false ->
        {:reply, :undefined, s}

      {:value, {^group, []}} ->
        {:reply, :undefined, s}

      {:value, {^group, nodes}} ->
        pid =
          :global_search.start(
            :whereis,
            {:group, nodes, name, from}
          )

        wait = :erlang.get(:whereis_name)
        :erlang.put(:whereis_name, [{pid, from} | wait])
        {:noreply, s}
    end
  end

  def handle_call({:whereis_name, {:node, node}, name}, from, s) do
    pid =
      :global_search.start(
        :whereis,
        {:node, node, name, from}
      )

    wait = :erlang.get(:whereis_name)
    :erlang.put(:whereis_name, [{pid, from} | wait])
    {:noreply, s}
  end

  def handle_call({:global_groups_changed, newPara}, _From, s) do
    {newGroupName, pubTpGrp, newNodes, newOther} =
      case (try do
              config_scan(newPara, :publish_type)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:error, _Error2} ->
          exit({:error, {:"invalid global_groups definition", newPara}})

        {defGroupName, pubType, defNodes, defOther} ->
          update_publish_nodes(
            r_state(s, :publish_type),
            {pubType, defNodes}
          )

          {defGroupName, pubType, defNodes, defOther}
      end

    nN = newNodes -- newNodes -- r_state(s, :nodes)
    nNC = (newNodes -- r_state(s, :nodes)) -- r_state(s, :sync_error)
    nSE = newNodes -- newNodes -- r_state(s, :sync_error)
    force_nodedown(:erlang.nodes(:connected) -- newNodes)

    newS =
      r_state(s,
        group_name: newGroupName,
        nodes: :lists.sort(nN),
        no_contact: :lists.sort(:lists.delete(node(), nNC)),
        sync_error: :lists.sort(nSE),
        other_grps: newOther,
        group_publish_type: pubTpGrp
      )

    {:reply, :ok, newS}
  end

  def handle_call({:global_groups_added, newPara}, _From, s) do
    {newGroupName, pubTpGrp, newNodes, newOther} =
      case (try do
              config_scan(newPara, :publish_type)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:error, _Error2} ->
          exit({:error, {:"invalid global_groups definition", newPara}})

        {defGroupName, pubType, defNodes, defOther} ->
          update_publish_nodes(
            r_state(s, :publish_type),
            {pubType, defNodes}
          )

          {defGroupName, pubType, defNodes, defOther}
      end

    force_nodedown(:erlang.nodes(:connected) -- newNodes)
    ownNG = get_own_nodes()

    nGACArgs =
      case r_state(s, :group_publish_type) do
        :normal ->
          [node(), ownNG]

        _ ->
          [node(), r_state(s, :group_publish_type), ownNG]
      end

    {nN, nNC, nSE} =
      :lists.foldl(
        fn node, {nN_acc, nNC_acc, nSE_acc} ->
          case :rpc.call(node, :global_group, :ng_add_check, nGACArgs) do
            {:badrpc, _} ->
              {nN_acc, [node | nNC_acc], nSE_acc}

            :agreed ->
              {[node | nN_acc], nNC_acc, nSE_acc}

            :not_agreed ->
              {nN_acc, nNC_acc, [node | nSE_acc]}
          end
        end,
        {[], [], []},
        :lists.delete(node(), newNodes)
      )

    newS =
      r_state(s,
        sync_state: :synced,
        group_name: newGroupName,
        nodes: :lists.sort(nN),
        sync_error: :lists.sort(nSE),
        no_contact: :lists.sort(nNC),
        other_grps: newOther,
        group_publish_type: pubTpGrp
      )

    {:reply, :ok, newS}
  end

  def handle_call({:global_groups_removed, _NewPara}, _From, s) do
    update_publish_nodes(r_state(s, :publish_type))

    newS =
      r_state(s,
        sync_state: :no_conf,
        group_name: [],
        nodes: [],
        sync_error: [],
        no_contact: [],
        other_grps: []
      )

    {:reply, :ok, newS}
  end

  def handle_call({:ng_add_check, node, pubType, othersNG}, _From, s) do
    ownNG = get_own_nodes()

    case r_state(s, :group_publish_type) === pubType do
      true ->
        case ownNG do
          ^othersNG ->
            nN = [node | r_state(s, :nodes)]
            nSE = :lists.delete(node, r_state(s, :sync_error))
            nNC = :lists.delete(node, r_state(s, :no_contact))
            newS = r_state(s, nodes: :lists.sort(nN), sync_error: nSE, no_contact: nNC)
            {:reply, :agreed, newS}

          _ ->
            {:reply, :not_agreed, s}
        end

      _ ->
        {:reply, :not_agreed, s}
    end
  end

  def handle_call(:info, _From, s) do
    reply = [
      {:state, r_state(s, :sync_state)},
      {:own_group_name, r_state(s, :group_name)},
      {:own_group_nodes, get_own_nodes()},
      {:synced_nodes, r_state(s, :nodes)},
      {:sync_error, r_state(s, :sync_error)},
      {:no_contact, r_state(s, :no_contact)},
      {:other_groups, r_state(s, :other_grps)},
      {:monitoring, r_state(s, :monitor)}
    ]

    {:reply, reply, s}
  end

  def handle_call(:get, _From, s) do
    {:reply, :erlang.get(), s}
  end

  def handle_call({:registered_names_test, {:node, :test3844zty}}, from, s) do
    pid =
      :global_search.start(
        :names_test,
        {:node, :test3844zty}
      )

    wait = :erlang.get(:registered_names)
    :erlang.put(:registered_names, [{pid, from} | wait])
    {:noreply, s}
  end

  def handle_call({:registered_names_test, {:node, _Node}}, _From, s) do
    {:reply, {:error, :illegal_function_call}, s}
  end

  def handle_call({:send_test, name, :test3844zty}, from, s) do
    pid = :global_search.start(:send_test, :test3844zty)
    wait = :erlang.get(:send)

    :erlang.put(
      :send,
      [{pid, from, name, :test3844zty} | wait]
    )

    {:noreply, s}
  end

  def handle_call({:send_test, _Name, _Msg}, _From, s) do
    {:reply, {:error, :illegal_function_call}, s}
  end

  def handle_call({:whereis_name_test, :test3844zty}, from, s) do
    pid = :global_search.start(:whereis_test, :test3844zty)
    wait = :erlang.get(:whereis_name)
    :erlang.put(:whereis_name, [{pid, from} | wait])
    {:noreply, s}
  end

  def handle_call({:whereis_name_test, _Name}, _From, s) do
    {:reply, {:error, :illegal_function_call}, s}
  end

  def handle_call(call, _From, s) do
    {:reply, {:illegal_message, call}, s}
  end

  def handle_cast({:registered_names, user}, s) do
    res = :global.registered_names()
    send(user, {:registered_names_res, res})
    {:noreply, s}
  end

  def handle_cast({:registered_names_res, result, pid, from}, s) do
    :erlang.unlink(pid)
    send(pid, :kill)
    wait = :erlang.get(:registered_names)
    newWait = :lists.delete({pid, from}, wait)
    :erlang.put(:registered_names, newWait)
    :gen_server.reply(from, result)
    {:noreply, s}
  end

  def handle_cast({:send_res, result, name, msg, pid, from}, s) do
    case result do
      {:badarg, {^name, ^msg}} ->
        :continue

      toPid ->
        send(toPid, msg)
    end

    :erlang.unlink(pid)
    send(pid, :kill)
    wait = :erlang.get(:send)
    newWait = :lists.delete({pid, from, name, msg}, wait)
    :erlang.put(:send, newWait)
    :gen_server.reply(from, result)
    {:noreply, s}
  end

  def handle_cast({:find_name, user, name}, s) do
    res = :global.whereis_name(name)
    send(user, {:find_name_res, res})
    {:noreply, s}
  end

  def handle_cast({:find_name_res, result, pid, from}, s) do
    :erlang.unlink(pid)
    send(pid, :kill)
    wait = :erlang.get(:whereis_name)
    newWait = :lists.delete({pid, from}, wait)
    :erlang.put(:whereis_name, newWait)
    :gen_server.reply(from, result)
    {:noreply, s}
  end

  def handle_cast({:synced, noContact}, s) do
    kill_global_group_check()
    nodes = get_own_nodes() -- [node() | noContact]
    {:noreply, r_state(s, nodes: :lists.sort(nodes), sync_error: [], no_contact: noContact)}
  end

  def handle_cast({:sync_error, noContact, errorNodes}, s) do
    txt =
      :io_lib.format(
        'Global group: Could not synchronize with these nodes ~p~nbecause global_groups were not in agreement. ~n',
        [errorNodes]
      )

    :error_logger.error_report(txt)
    kill_global_group_check()

    nodes =
      (get_own_nodes() --
         [
           node()
           | noContact
         ]) -- errorNodes

    {:noreply,
     r_state(s, nodes: :lists.sort(nodes), sync_error: errorNodes, no_contact: noContact)}
  end

  def handle_cast(
        {:conf_check, vsn, node, from, :sync, cCName, cCNodes},
        s
      ) do
    handle_cast(
      {:conf_check, vsn, node, from, :sync, cCName, :normal, cCNodes},
      s
    )
  end

  def handle_cast(
        {:conf_check, vsn, node, from, :sync, cCName, pubType, cCNodes},
        s
      ) do
    curNodes = r_state(s, :nodes)

    nS =
      case :application.get_env(
             :kernel,
             :global_groups
           ) do
        :undefined ->
          update_publish_nodes(r_state(s, :publish_type))
          disconnect_nodes([node])
          send({:global_group_check, node}, {:config_error, vsn, from, node()})
          s

        {:ok, []} ->
          update_publish_nodes(r_state(s, :publish_type))
          disconnect_nodes([node])
          send({:global_group_check, node}, {:config_error, vsn, from, node()})
          s

        {:ok, nodeGrps} ->
          case (try do
                  config_scan(nodeGrps, :publish_type)
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            {:error, _Error2} ->
              disconnect_nodes([node])
              send({:global_group_check, node}, {:config_error, vsn, from, node()})
              r_state(s, nodes: :lists.delete(node, curNodes))

            {^cCName, ^pubType, ^cCNodes, _OtherDef} ->
              update_publish_nodes(
                r_state(s, :publish_type),
                {pubType, cCNodes}
              )

              send(:global_name_server, {:nodeup, node})
              send({:global_group_check, node}, {:config_ok, vsn, from, node()})

              case :lists.member(node, curNodes) do
                false ->
                  newNodes = :lists.sort([node | curNodes])
                  nSE = :lists.delete(node, r_state(s, :sync_error))
                  nNC = :lists.delete(node, r_state(s, :no_contact))
                  r_state(s, nodes: newNodes, sync_error: nSE, no_contact: nNC)

                true ->
                  s
              end

            _ ->
              disconnect_nodes([node])
              send({:global_group_check, node}, {:config_error, vsn, from, node()})
              nN = :lists.delete(node, r_state(s, :nodes))
              nSE = :lists.delete(node, r_state(s, :sync_error))
              nNC = :lists.delete(node, r_state(s, :no_contact))
              r_state(s, nodes: nN, sync_error: nSE, no_contact: nNC)
          end
      end

    {:noreply, nS}
  end

  def handle_cast(_Cast, s) do
    {:noreply, s}
  end

  def handle_info({:nodeup, node}, s)
      when r_state(s, :sync_state) === :no_conf do
    send_monitor(r_state(s, :monitor), {:nodeup, node}, r_state(s, :sync_state))
    send(:global_name_server, {:nodeup, node})
    {:noreply, s}
  end

  def handle_info({:nodeup, node}, s) do
    othersNG =
      case r_state(s, :sync_state) do
        :synced ->
          x =
            try do
              :rpc.call(node, :global_group, :get_own_nodes, [])
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end

          case x do
            ^x when is_list(x) ->
              :lists.sort(x)

            _ ->
              []
          end

        :no_conf ->
          []
      end

    nNC = :lists.delete(node, r_state(s, :no_contact))
    nSE = :lists.delete(node, r_state(s, :sync_error))
    ownNG = get_own_nodes()

    case ownNG do
      ^othersNG ->
        send_monitor(r_state(s, :monitor), {:nodeup, node}, r_state(s, :sync_state))
        send(:global_name_server, {:nodeup, node})

        case :lists.member(node, r_state(s, :nodes)) do
          false ->
            nN = :lists.sort([node | r_state(s, :nodes)])
            {:noreply, r_state(s, nodes: nN, no_contact: nNC, sync_error: nSE)}

          true ->
            {:noreply, r_state(s, no_contact: nNC, sync_error: nSE)}
        end

      _ ->
        case {:lists.member(node, get_own_nodes()), :lists.member(node, r_state(s, :sync_error))} do
          {true, false} ->
            nSE2 = :lists.sort([node | r_state(s, :sync_error)])
            {:noreply, r_state(s, no_contact: nNC, sync_error: nSE2)}

          _ ->
            {:noreply, s}
        end
    end
  end

  def handle_info({:nodedown, node}, s)
      when r_state(s, :sync_state) === :no_conf do
    send_monitor(r_state(s, :monitor), {:nodedown, node}, r_state(s, :sync_state))
    send(:global_name_server, {:nodedown, node})
    {:noreply, s}
  end

  def handle_info({:nodedown, node}, s) do
    send_monitor(r_state(s, :monitor), {:nodedown, node}, r_state(s, :sync_state))
    send(:global_name_server, {:nodedown, node})
    nN = :lists.delete(node, r_state(s, :nodes))
    nSE = :lists.delete(node, r_state(s, :sync_error))

    nNC =
      case {:lists.member(node, get_own_nodes()), :lists.member(node, r_state(s, :no_contact))} do
        {true, false} ->
          [node | r_state(s, :no_contact)]

        _ ->
          r_state(s, :no_contact)
      end

    {:noreply, r_state(s, nodes: nN, no_contact: nNC, sync_error: nSE)}
  end

  def handle_info({:disconnect_node, node}, s) do
    case {r_state(s, :sync_state), :lists.member(node, r_state(s, :nodes))} do
      {:synced, true} ->
        send_monitor(r_state(s, :monitor), {:nodedown, node}, r_state(s, :sync_state))

      _ ->
        :cont
    end

    send(:global_name_server, {:nodedown, node})
    nN = :lists.delete(node, r_state(s, :nodes))
    nNC = :lists.delete(node, r_state(s, :no_contact))
    nSE = :lists.delete(node, r_state(s, :sync_error))
    {:noreply, r_state(s, nodes: nN, no_contact: nNC, sync_error: nSE)}
  end

  def handle_info({:EXIT, exitPid, reason}, s) do
    check_exit(exitPid, reason)
    {:noreply, s}
  end

  def handle_info(_Info, s) do
    {:noreply, s}
  end

  def terminate(_Reason, _S) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  def config_scan(nodeGrps) do
    config_scan(nodeGrps, :original)
  end

  def config_scan(nodeGrps, :original) do
    case config_scan(nodeGrps, :publish_type) do
      {defGroupName, _, defNodes, defOther} ->
        {defGroupName, defNodes, defOther}

      error ->
        error
    end
  end

  def config_scan(nodeGrps, :publish_type) do
    config_scan(node(), :normal, nodeGrps, :no_name, [], [])
  end

  defp config_scan(_MyNode, pubType, [], own_name, ownNodes, otherNodeGrps) do
    {own_name, pubType, :lists.sort(ownNodes), :lists.reverse(otherNodeGrps)}
  end

  defp config_scan(myNode, pubType, [grpTuple | nodeGrps], own_name, ownNodes, otherNodeGrps) do
    {name, pubTypeGroup, nodes} = grp_tuple(grpTuple)

    case :lists.member(myNode, nodes) do
      true ->
        case own_name do
          :no_name ->
            config_scan(myNode, pubTypeGroup, nodeGrps, name, nodes, otherNodeGrps)

          _ ->
            {:error, {:"node defined twice", {own_name, name}}}
        end

      false ->
        config_scan(myNode, pubType, nodeGrps, own_name, ownNodes, [{name, nodes} | otherNodeGrps])
    end
  end

  defp grp_tuple({name, nodes}) do
    {name, :normal, nodes}
  end

  defp grp_tuple({name, :hidden, nodes}) do
    {name, :hidden, nodes}
  end

  defp grp_tuple({name, :normal, nodes}) do
    {name, :normal, nodes}
  end

  def sync_init(type, cname, pubType, nodes) do
    {up, down} =
      sync_check_node(
        :lists.delete(
          node(),
          nodes
        ),
        [],
        []
      )

    sync_check_init(type, up, cname, nodes, down, pubType)
  end

  defp sync_check_node([], up, down) do
    {up, down}
  end

  defp sync_check_node([node | nodes], up, down) do
    case :net_adm.ping(node) do
      :pang ->
        sync_check_node(nodes, up, [node | down])

      :pong ->
        sync_check_node(nodes, [node | up], down)
    end
  end

  defp sync_check_init(type, up, cname, nodes, down, pubType) do
    sync_check_init(type, up, cname, nodes, 3, [], down, pubType)
  end

  defp sync_check_init(_Type, noContact, _Cname, _Nodes, 0, errorNodes, down, _PubType) do
    case errorNodes do
      [] ->
        :gen_server.cast(
          :global_group,
          {:synced, :lists.sort(noContact ++ down)}
        )

      _ ->
        :gen_server.cast(
          :global_group,
          {:sync_error, :lists.sort(noContact ++ down), errorNodes}
        )
    end

    receive do
      :kill ->
        exit(:normal)
    after
      5000 ->
        exit(:normal)
    end
  end

  defp sync_check_init(type, up, cname, nodes, n, errorNodes, down, pubType) do
    confCheckMsg =
      case pubType do
        :normal ->
          {:conf_check, 2, node(), self(), type, cname, nodes}

        _ ->
          {:conf_check, 2, node(), self(), type, cname, pubType, nodes}
      end

    :lists.foreach(
      fn node ->
        :gen_server.cast({:global_group, node}, confCheckMsg)
      end,
      up
    )

    case sync_check(up) do
      {:ok, :synced} ->
        sync_check_init(type, [], cname, nodes, 0, errorNodes, down, pubType)

      {:error, newErrorNodes} ->
        sync_check_init(type, [], cname, nodes, 0, errorNodes ++ newErrorNodes, down, pubType)

      {:more, rem, newErrorNodes} ->
        sync_check_init(
          type,
          rem,
          cname,
          nodes,
          n - 1,
          errorNodes ++ newErrorNodes,
          down,
          pubType
        )
    end
  end

  defp sync_check(up) do
    sync_check(up, up, [])
  end

  defp sync_check([], _Up, []) do
    {:ok, :synced}
  end

  defp sync_check([], _Up, errorNodes) do
    {:error, errorNodes}
  end

  defp sync_check(rem, up, errorNodes) do
    receive do
      {:config_ok, 2, pid, node} when pid === self() ->
        send(:global_name_server, {:nodeup, node})
        sync_check(rem -- [node], up, errorNodes)

      {:config_error, 2, pid, node} when pid === self() ->
        sync_check(rem -- [node], up, [node | errorNodes])

      {:no_global_group_configuration, 2, pid, node}
      when pid === self() ->
        sync_check(rem -- [node], up, [node | errorNodes])

      _ ->
        sync_check(rem, up, errorNodes)
    after
      2000 ->
        {:more, rem, errorNodes}
    end
  end

  defp monitor_nodes(true, pid, state) do
    :erlang.link(pid)
    monitor = r_state(state, :monitor)
    {:ok, r_state(state, monitor: [pid | monitor])}
  end

  defp monitor_nodes(false, pid, state) do
    monitor = r_state(state, :monitor)
    state1 = r_state(state, monitor: delete_all(pid, monitor))
    do_unlink(pid, state1)
    {:ok, state1}
  end

  defp monitor_nodes(_, _, state) do
    {:error, state}
  end

  defp delete_all(from, [from | tail]) do
    delete_all(from, tail)
  end

  defp delete_all(from, [h | tail]) do
    [h | delete_all(from, tail)]
  end

  defp delete_all(_, []) do
    []
  end

  defp do_unlink(pid, state) do
    case :lists.member(pid, r_state(state, :monitor)) do
      true ->
        false

      _ ->
        :erlang.unlink(pid)
    end
  end

  defp send_monitor([p | t], m, :no_conf) do
    _ = safesend_nc(p, m)
    send_monitor(t, m, :no_conf)
  end

  defp send_monitor([p | t], m, syncState) do
    _ = safesend(p, m)
    send_monitor(t, m, syncState)
  end

  defp send_monitor([], _, _) do
    :ok
  end

  defp safesend(name, {msg, node}) when is_atom(name) do
    case :lists.member(node, get_own_nodes()) do
      true ->
        case :erlang.whereis(name) do
          :undefined ->
            {msg, node}

          p when is_pid(p) ->
            send(p, {msg, node})
        end

      false ->
        :not_own_group
    end
  end

  defp safesend(pid, {msg, node}) do
    case :lists.member(node, get_own_nodes()) do
      true ->
        send(pid, {msg, node})

      false ->
        :not_own_group
    end
  end

  defp safesend_nc(name, {msg, node}) when is_atom(name) do
    case :erlang.whereis(name) do
      :undefined ->
        {msg, node}

      p when is_pid(p) ->
        send(p, {msg, node})
    end
  end

  defp safesend_nc(pid, {msg, node}) do
    send(pid, {msg, node})
  end

  defp check_exit(exitPid, reason) do
    check_exit_reg(:erlang.get(:registered_names), exitPid, reason)
    check_exit_send(:erlang.get(:send), exitPid, reason)
    check_exit_where(:erlang.get(:whereis_name), exitPid, reason)
  end

  defp check_exit_reg(:undefined, _ExitPid, _Reason) do
    :ok
  end

  defp check_exit_reg(reg, exitPid, reason) do
    case :lists.keysearch(exitPid, 1, :lists.delete(:undefined, reg)) do
      {:value, {^exitPid, from}} ->
        newReg = :lists.delete({exitPid, from}, reg)
        :erlang.put(:registered_names, newReg)
        :gen_server.reply(from, {:error, reason})

      false ->
        :not_found_ignored
    end
  end

  defp check_exit_send(:undefined, _ExitPid, _Reason) do
    :ok
  end

  defp check_exit_send(send, exitPid, _Reason) do
    case :lists.keysearch(exitPid, 1, :lists.delete(:undefined, send)) do
      {:value, {^exitPid, from, name, msg}} ->
        newSend =
          :lists.delete(
            {exitPid, from, name, msg},
            send
          )

        :erlang.put(:send, newSend)
        :gen_server.reply(from, {:badarg, {name, msg}})

      false ->
        :not_found_ignored
    end
  end

  defp check_exit_where(:undefined, _ExitPid, _Reason) do
    :ok
  end

  defp check_exit_where(where, exitPid, reason) do
    case :lists.keysearch(exitPid, 1, :lists.delete(:undefined, where)) do
      {:value, {^exitPid, from}} ->
        newWhere = :lists.delete({exitPid, from}, where)
        :erlang.put(:whereis_name, newWhere)
        :gen_server.reply(from, {:error, reason})

      false ->
        :not_found_ignored
    end
  end

  defp kill_global_group_check() do
    case :erlang.whereis(:global_group_check) do
      :undefined ->
        :ok

      pid ->
        :erlang.unlink(pid)
        send(:global_group_check, :kill)
        :erlang.unregister(:global_group_check)
    end
  end

  defp disconnect_nodes(disconnectNodes) do
    :lists.foreach(
      fn node ->
        send({:global_group, node}, {:disconnect_node, node()})
        :global.node_disconnected(node)
      end,
      disconnectNodes
    )
  end

  defp force_nodedown(disconnectNodes) do
    :lists.foreach(
      fn node ->
        :erlang.disconnect_node(node)
        :global.node_disconnected(node)
      end,
      disconnectNodes
    )
  end

  def get_own_nodes_with_errors() do
    case :application.get_env(:kernel, :global_groups) do
      :undefined ->
        {:ok, :all}

      {:ok, []} ->
        {:ok, :all}

      {:ok, nodeGrps} ->
        case (try do
                config_scan(nodeGrps, :publish_type)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:error, error} ->
            {:error, error}

          {_, _, nodesDef, _} ->
            {:ok, :lists.sort(nodesDef)}
        end
    end
  end

  def get_own_nodes() do
    case get_own_nodes_with_errors() do
      {:ok, :all} ->
        []

      {:error, _} ->
        []

      {:ok, nodes} ->
        nodes
    end
  end

  defp publish_arg() do
    case :net_kernel.dist_listen() do
      false ->
        :hidden

      _ ->
        case :init.get_argument(:hidden) do
          {:ok, [[]]} ->
            :hidden

          {:ok, [['true']]} ->
            :hidden

          _ ->
            :normal
        end
    end
  end

  defp own_group() do
    case :application.get_env(:kernel, :global_groups) do
      :undefined ->
        :no_group

      {:ok, []} ->
        :no_group

      {:ok, nodeGrps} ->
        case (try do
                config_scan(nodeGrps, :publish_type)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:error, _} ->
            :no_group

          {_, pubTpGrp, nodesDef, _} ->
            {pubTpGrp, nodesDef}
        end
    end
  end

  defp publish_on_nodes(:normal, :no_group) do
    :all
  end

  defp publish_on_nodes(:hidden, :no_group) do
    []
  end

  defp publish_on_nodes(:normal, {:normal, _}) do
    :all
  end

  defp publish_on_nodes(:hidden, {_, nodes}) do
    nodes
  end

  defp publish_on_nodes(_, {:hidden, nodes}) do
    nodes
  end

  defp update_publish_nodes(pubArg) do
    update_publish_nodes(pubArg, :no_group)
  end

  defp update_publish_nodes(pubArg, myGroup) do
    :net_kernel.update_publish_nodes(
      publish_on_nodes(
        pubArg,
        myGroup
      )
    )
  end

  def publish_on_nodes() do
    publish_on_nodes(publish_arg(), own_group())
  end
end
