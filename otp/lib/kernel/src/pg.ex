defmodule :m_pg do
  use Bitwise

  def start_link() do
    start_link(:pg)
  end

  def start(scope) when is_atom(scope) do
    :gen_server.start({:local, scope}, :pg, [scope], [])
  end

  def start_link(scope) when is_atom(scope) do
    :gen_server.start_link({:local, scope}, :pg, [scope], [])
  end

  def join(group, pidOrPids) do
    join(:pg, group, pidOrPids)
  end

  def join(scope, group, pidOrPids)
      when is_pid(pidOrPids) or is_list(pidOrPids) do
    :ok = ensure_local(pidOrPids)
    :gen_server.call(scope, {:join_local, group, pidOrPids}, :infinity)
  end

  def leave(group, pidOrPids) do
    leave(:pg, group, pidOrPids)
  end

  def leave(scope, group, pidOrPids)
      when is_pid(pidOrPids) or is_list(pidOrPids) do
    :ok = ensure_local(pidOrPids)
    :gen_server.call(scope, {:leave_local, group, pidOrPids}, :infinity)
  end

  def get_members(group) do
    get_members(:pg, group)
  end

  def get_members(scope, group) do
    try do
      :ets.lookup_element(scope, group, 2)
    catch
      :error, :badarg ->
        []
    end
  end

  def get_local_members(group) do
    get_local_members(:pg, group)
  end

  def get_local_members(scope, group) do
    try do
      :ets.lookup_element(scope, group, 3)
    catch
      :error, :badarg ->
        []
    end
  end

  def which_groups() do
    which_groups(:pg)
  end

  def which_groups(scope) when is_atom(scope) do
    for [g] <- :ets.match(scope, {:"$1", :_, :_}) do
      g
    end
  end

  def which_local_groups() do
    which_local_groups(:pg)
  end

  def which_local_groups(scope) when is_atom(scope) do
    :ets.select(
      scope,
      [{{:"$1", :_, :"$2"}, [{:"=/=", :"$2", []}], [:"$1"]}]
    )
  end

  require Record
  Record.defrecord(:r_state, :state, scope: :undefined, monitors: %{}, nodes: %{})

  def init([scope]) do
    :ok = :net_kernel.monitor_nodes(true)

    broadcast(
      for node <- :erlang.nodes() do
        {scope, node}
      end,
      {:discover, self()}
    )

    ^scope =
      :ets.new(
        scope,
        [:set, :protected, :named_table, {:read_concurrency, true}]
      )

    {:ok, r_state(scope: scope)}
  end

  def handle_call(
        {:join_local, group, pidOrPids},
        _From,
        r_state(scope: scope, monitors: monitors, nodes: nodes) = state
      ) do
    newMons = join_monitors(pidOrPids, group, monitors)
    join_local_group(scope, group, pidOrPids)

    broadcast(
      :maps.keys(nodes),
      {:join, self(), group, pidOrPids}
    )

    {:reply, :ok, r_state(state, monitors: newMons)}
  end

  def handle_call(
        {:leave_local, group, pidOrPids},
        _From,
        r_state(scope: scope, monitors: monitors, nodes: nodes) = state
      ) do
    case leave_monitors(pidOrPids, group, monitors) do
      ^monitors ->
        {:reply, :not_joined, state}

      newMons ->
        leave_local_group(scope, group, pidOrPids)

        broadcast(
          :maps.keys(nodes),
          {:leave, self(), pidOrPids, [group]}
        )

        {:reply, :ok, r_state(state, monitors: newMons)}
    end
  end

  def handle_call(_Request, _From, _S) do
    :erlang.error(:badarg)
  end

  def handle_cast(
        {:sync, peer, groups},
        r_state(scope: scope, nodes: nodes) = state
      ) do
    {:noreply, r_state(state, nodes: handle_sync(scope, peer, nodes, groups))}
  end

  def handle_cast(_, _State) do
    :erlang.error(:badarg)
  end

  def handle_info(
        {:join, peer, group, pidOrPids},
        r_state(scope: scope, nodes: nodes) = state
      ) do
    case :maps.get(peer, nodes, []) do
      {mRef, remoteGroups} ->
        join_remote(scope, group, pidOrPids)
        newRemoteGroups = join_remote_map(group, pidOrPids, remoteGroups)
        {:noreply, r_state(state, nodes: %{nodes | peer => {mRef, newRemoteGroups}})}

      [] ->
        {:noreply, state}
    end
  end

  def handle_info(
        {:leave, peer, pidOrPids, groups},
        r_state(scope: scope, nodes: nodes) = state
      ) do
    case :maps.get(peer, nodes, []) do
      {mRef, remoteMap} ->
        _ = leave_remote(scope, pidOrPids, groups)

        newRemoteMap =
          :lists.foldl(
            fn group, acc ->
              case :maps.get(group, acc) do
                ^pidOrPids ->
                  acc

                [^pidOrPids] ->
                  acc

                existing when is_pid(pidOrPids) ->
                  %{
                    acc
                    | group =>
                        :lists.delete(
                          pidOrPids,
                          existing
                        )
                  }

                existing ->
                  %{acc | group => existing -- pidOrPids}
              end
            end,
            remoteMap,
            groups
          )

        {:noreply, r_state(state, nodes: %{nodes | peer => {mRef, newRemoteMap}})}

      [] ->
        {:noreply, state}
    end
  end

  def handle_info(
        {:discover, peer},
        r_state(scope: scope, nodes: nodes) = state
      ) do
    :gen_server.cast(
      peer,
      {:sync, self(), all_local_pids(scope)}
    )

    case :maps.is_key(peer, nodes) do
      true ->
        {:noreply, state}

      false ->
        mRef = :erlang.monitor(:process, peer)
        :erlang.send(peer, {:discover, self()}, [:noconnect])
        {:noreply, r_state(state, nodes: %{nodes | peer => {mRef, %{}}})}
    end
  end

  def handle_info(
        {:DOWN, mRef, :process, pid, _Info},
        r_state(scope: scope, monitors: monitors, nodes: nodes) = state
      )
      when node(pid) === node() do
    case :maps.take(pid, monitors) do
      :error ->
        {:noreply, state}

      {{^mRef, groups}, newMons} ->
        for group <- groups do
          leave_local_group(scope, group, pid)
        end

        broadcast(
          :maps.keys(nodes),
          {:leave, self(), pid, groups}
        )

        {:noreply, r_state(state, monitors: newMons)}
    end
  end

  def handle_info(
        {:DOWN, mRef, :process, pid, _Info},
        r_state(scope: scope, nodes: nodes) = state
      ) do
    {{^mRef, remoteMap}, newNodes} = :maps.take(pid, nodes)

    _ =
      :maps.map(
        fn group, pids ->
          leave_remote(scope, pids, [group])
        end,
        remoteMap
      )

    {:noreply, r_state(state, nodes: newNodes)}
  end

  def handle_info({:nodedown, _Node}, state) do
    {:noreply, state}
  end

  def handle_info({:nodeup, node}, state) when node === node() do
    {:noreply, state}
  end

  def handle_info({:nodeup, node}, r_state(scope: scope) = state) do
    send({scope, node}, {:discover, self()})
    {:noreply, state}
  end

  def handle_info(_Info, _State) do
    :erlang.error(:badarg)
  end

  def terminate(_Reason, r_state(scope: scope)) do
    true = :ets.delete(scope)
  end

  defp ensure_local(pid)
       when is_pid(pid) and
              node(pid) === node() do
    :ok
  end

  defp ensure_local(pids) when is_list(pids) do
    :lists.foreach(
      fn
        pid
        when is_pid(pid) and
               node(pid) === node() ->
          :ok

        bad ->
          :erlang.error({:nolocal, bad})
      end,
      pids
    )
  end

  defp ensure_local(bad) do
    :erlang.error({:nolocal, bad})
  end

  defp handle_sync(scope, peer, nodes, groups) do
    {mRef, remoteGroups} =
      case :maps.find(
             peer,
             nodes
           ) do
        :error ->
          {:erlang.monitor(:process, peer), %{}}

        {:ok, mRef0} ->
          mRef0
      end

    _ = sync_groups(scope, remoteGroups, groups)
    %{nodes | peer => {mRef, :maps.from_list(groups)}}
  end

  defp sync_groups(scope, remoteGroups, []) do
    for {group, pids} <- :maps.to_list(remoteGroups) do
      leave_remote(scope, pids, [group])
    end
  end

  defp sync_groups(scope, remoteGroups, [{group, pids} | tail]) do
    case :maps.take(group, remoteGroups) do
      {^pids, newRemoteGroups} ->
        sync_groups(scope, newRemoteGroups, tail)

      {oldPids, newRemoteGroups} ->
        [{^group, allOldPids, localPids}] =
          :ets.lookup(
            scope,
            group
          )

        allNewPids = pids ++ (allOldPids -- oldPids)

        true =
          :ets.insert(
            scope,
            {group, allNewPids, localPids}
          )

        sync_groups(scope, newRemoteGroups, tail)

      :error ->
        join_remote(scope, group, pids)
        sync_groups(scope, remoteGroups, tail)
    end
  end

  defp join_monitors(pid, group, monitors) when is_pid(pid) do
    case :maps.find(pid, monitors) do
      {:ok, {mRef, groups}} ->
        :maps.put(pid, {mRef, [group | groups]}, monitors)

      :error ->
        mRef = :erlang.monitor(:process, pid)
        %{monitors | pid => {mRef, [group]}}
    end
  end

  defp join_monitors([], _Group, monitors) do
    monitors
  end

  defp join_monitors([pid | tail], group, monitors) do
    join_monitors(tail, group, join_monitors(pid, group, monitors))
  end

  defp join_local_group(scope, group, pid) when is_pid(pid) do
    case :ets.lookup(scope, group) do
      [{^group, all, local}] ->
        :ets.insert(scope, {group, [pid | all], [pid | local]})

      [] ->
        :ets.insert(scope, {group, [pid], [pid]})
    end
  end

  defp join_local_group(scope, group, pids) do
    case :ets.lookup(scope, group) do
      [{^group, all, local}] ->
        :ets.insert(scope, {group, pids ++ all, pids ++ local})

      [] ->
        :ets.insert(scope, {group, pids, pids})
    end
  end

  defp join_remote(scope, group, pid) when is_pid(pid) do
    case :ets.lookup(scope, group) do
      [{^group, all, local}] ->
        :ets.insert(scope, {group, [pid | all], local})

      [] ->
        :ets.insert(scope, {group, [pid], []})
    end
  end

  defp join_remote(scope, group, pids) do
    case :ets.lookup(scope, group) do
      [{^group, all, local}] ->
        :ets.insert(scope, {group, pids ++ all, local})

      [] ->
        :ets.insert(scope, {group, pids, []})
    end
  end

  defp join_remote_map(group, pid, remoteGroups) when is_pid(pid) do
    :maps.update_with(
      group,
      fn list ->
        [pid | list]
      end,
      [pid],
      remoteGroups
    )
  end

  defp join_remote_map(group, pids, remoteGroups) do
    :maps.update_with(
      group,
      fn list ->
        pids ++ list
      end,
      pids,
      remoteGroups
    )
  end

  defp leave_monitors(pid, group, monitors) when is_pid(pid) do
    case :maps.find(pid, monitors) do
      {:ok, {mRef, [^group]}} ->
        :erlang.demonitor(mRef)
        :maps.remove(pid, monitors)

      {:ok, {mRef, groups}} ->
        case :lists.member(group, groups) do
          true ->
            :maps.put(pid, {mRef, :lists.delete(group, groups)}, monitors)

          false ->
            monitors
        end

      _ ->
        monitors
    end
  end

  defp leave_monitors([], _Group, monitors) do
    monitors
  end

  defp leave_monitors([pid | tail], group, monitors) do
    leave_monitors(tail, group, leave_monitors(pid, group, monitors))
  end

  defp leave_local_group(scope, group, pid) when is_pid(pid) do
    case :ets.lookup(scope, group) do
      [{^group, [^pid], [^pid]}] ->
        :ets.delete(scope, group)

      [{^group, all, local}] ->
        :ets.insert(
          scope,
          {group, :lists.delete(pid, all), :lists.delete(pid, local)}
        )

      [] ->
        true
    end
  end

  defp leave_local_group(scope, group, pids) do
    case :ets.lookup(scope, group) do
      [{^group, all, local}] ->
        case all -- pids do
          [] ->
            :ets.delete(scope, group)

          newAll ->
            :ets.insert(scope, {group, newAll, local -- pids})
        end

      [] ->
        true
    end
  end

  defp leave_remote(scope, pid, groups) when is_pid(pid) do
    _ =
      for group <- groups do
        case :ets.lookup(scope, group) do
          [{^group, [^pid], []}] ->
            :ets.delete(scope, group)

          [{^group, all, local}] ->
            :ets.insert(
              scope,
              {group, :lists.delete(pid, all), local}
            )

          [] ->
            true
        end
      end
  end

  defp leave_remote(scope, pids, groups) do
    _ =
      for group <- groups do
        case :ets.lookup(scope, group) do
          [{^group, all, local}] ->
            case all -- pids do
              [] when local === [] ->
                :ets.delete(scope, group)

              newAll ->
                :ets.insert(scope, {group, newAll, local})
            end

          [] ->
            true
        end
      end
  end

  defp all_local_pids(scope) do
    :ets.select(
      scope,
      [{{:"$1", :_, :"$2"}, [{:"=/=", :"$2", []}], [{{:"$1", :"$2"}}]}]
    )
  end

  defp broadcast([], _Msg) do
    :ok
  end

  defp broadcast([dest | tail], msg) do
    :erlang.send(dest, msg, [:noconnect])
    broadcast(tail, msg)
  end
end
