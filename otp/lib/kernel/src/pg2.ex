defmodule :m_pg2 do
  use Bitwise

  def start_link() do
    :gen_server.start_link({:local, :pg2}, :pg2, [], [])
  end

  def start() do
    ensure_started()
  end

  def create(name) do
    _ = ensure_started()

    case :ets.member(:pg2_table, {:group, name}) do
      false ->
        :global.trans(
          {{:pg2, name}, self()},
          fn ->
            :gen_server.multi_call(:pg2, {:create, name})
          end
        )

        :ok

      true ->
        :ok
    end
  end

  def delete(name) do
    _ = ensure_started()

    :global.trans(
      {{:pg2, name}, self()},
      fn ->
        :gen_server.multi_call(:pg2, {:delete, name})
      end
    )

    :ok
  end

  def join(name, pid) when is_pid(pid) do
    _ = ensure_started()

    case :ets.member(:pg2_table, {:group, name}) do
      false ->
        {:error, {:no_such_group, name}}

      true ->
        :global.trans(
          {{:pg2, name}, self()},
          fn ->
            :gen_server.multi_call(:pg2, {:join, name, pid})
          end
        )

        :ok
    end
  end

  def leave(name, pid) when is_pid(pid) do
    _ = ensure_started()

    case :ets.member(:pg2_table, {:group, name}) do
      false ->
        {:error, {:no_such_group, name}}

      true ->
        :global.trans(
          {{:pg2, name}, self()},
          fn ->
            :gen_server.multi_call(:pg2, {:leave, name, pid})
          end
        )

        :ok
    end
  end

  def get_members(name) do
    _ = ensure_started()

    case :ets.member(:pg2_table, {:group, name}) do
      true ->
        group_members(name)

      false ->
        {:error, {:no_such_group, name}}
    end
  end

  def get_local_members(name) do
    _ = ensure_started()

    case :ets.member(:pg2_table, {:group, name}) do
      true ->
        local_group_members(name)

      false ->
        {:error, {:no_such_group, name}}
    end
  end

  def which_groups() do
    _ = ensure_started()
    all_groups()
  end

  def get_closest_pid(name) do
    case get_local_members(name) do
      [pid] ->
        pid

      [] ->
        case get_members(name) do
          [] ->
            {:error, {:no_process, name}}

          members ->
            random_element(members)
        end

      members when is_list(members) ->
        random_element(members)

      else__ ->
        else__
    end
  end

  defp random_element(list) do
    x = abs(:erlang.monotonic_time() ^^^ :erlang.unique_integer())
    :lists.nth(rem(x, length(list)) + 1, list)
  end

  require Record
  Record.defrecord(:r_state, :state, [])

  def init([]) do
    ns = :erlang.nodes()
    :ok = :net_kernel.monitor_nodes(true)

    :lists.foreach(
      fn n ->
        send({:pg2, n}, {:new_pg2, node()})
        send(self(), {:nodeup, n})
      end,
      ns
    )

    :pg2_table =
      :ets.new(
        :pg2_table,
        [:ordered_set, :protected, :named_table]
      )

    {:ok, r_state()}
  end

  def handle_call({:create, name}, _From, s) do
    assure_group(name)
    {:reply, :ok, s}
  end

  def handle_call({:join, name, pid}, _From, s) do
    :ets.member(
      :pg2_table,
      {:group, name}
    ) and join_group(name, pid)

    {:reply, :ok, s}
  end

  def handle_call({:leave, name, pid}, _From, s) do
    :ets.member(
      :pg2_table,
      {:group, name}
    ) and leave_group(name, pid)

    {:reply, :ok, s}
  end

  def handle_call({:delete, name}, _From, s) do
    delete_group(name)
    {:reply, :ok, s}
  end

  def handle_call(request, from, s) do
    :error_logger.warning_msg(
      'The pg2 server received an unexpected message:\nhandle_call(~tp, ~tp, _)\n',
      [request, from]
    )

    {:noreply, s}
  end

  def handle_cast({:exchange, _Node, list}, s) do
    store(list)
    {:noreply, s}
  end

  def handle_cast(_, s) do
    {:noreply, s}
  end

  def handle_info({:DOWN, monitorRef, :process, _Pid, _Info}, s) do
    member_died(monitorRef)
    {:noreply, s}
  end

  def handle_info({:nodeup, node}, s) do
    :gen_server.cast(
      {:pg2, node},
      {:exchange, node(), all_members()}
    )

    {:noreply, s}
  end

  def handle_info({:new_pg2, node}, s) do
    :gen_server.cast(
      {:pg2, node},
      {:exchange, node(), all_members()}
    )

    {:noreply, s}
  end

  def handle_info(_, s) do
    {:noreply, s}
  end

  def terminate(_Reason, _S) do
    true = :ets.delete(:pg2_table)
    :ok
  end

  defp store(list) do
    _ =
      for [name, members] <- list do
        assure_group(name) and
          for p <- members -- group_members(name) do
            join_group(name, p)
          end
      end

    :ok
  end

  defp assure_group(name) do
    key = {:group, name}

    :ets.member(
      :pg2_table,
      key
    ) or true === :ets.insert(:pg2_table, {key})
  end

  defp delete_group(name) do
    _ =
      for pid <- group_members(name) do
        leave_group(name, pid)
      end

    true = :ets.delete(:pg2_table, {:group, name})
    :ok
  end

  defp member_died(ref) do
    [{{:ref, ^ref}, pid}] =
      :ets.lookup(
        :pg2_table,
        {:ref, ref}
      )

    names = member_groups(pid)

    _ =
      for name <- names,
          p <- member_in_group(pid, name) do
        leave_group(name, p)
      end

    _ =
      for name <- names do
        :gen_server.abcast(:erlang.nodes(), :pg2, {:del_member, name, pid})
      end

    :ok
  end

  defp join_group(name, pid) do
    ref_Pid = {:ref, pid}

    try do
      _ = :ets.update_counter(:pg2_table, ref_Pid, {4, +1})
      true
    catch
      _, _ ->
        {rPid, ref} = do_monitor(pid)
        true = :ets.insert(:pg2_table, {ref_Pid, rPid, ref, 1})
        true = :ets.insert(:pg2_table, {{:ref, ref}, pid})
    end

    member_Name_Pid = {:member, name, pid}

    try do
      _ = :ets.update_counter(:pg2_table, member_Name_Pid, {2, +1})
    catch
      _, _ ->
        true = :ets.insert(:pg2_table, {member_Name_Pid, 1})

        _ =
          for _ <- [:EFE_DUMMY_GEN], node(pid) === node() do
            :ets.insert(:pg2_table, {{:local_member, name, pid}})
          end

        true = :ets.insert(:pg2_table, {{:pid, pid, name}})
    end
  end

  defp leave_group(name, pid) do
    member_Name_Pid = {:member, name, pid}

    try do
      :ets.update_counter(:pg2_table, member_Name_Pid, {2, -1})
    catch
      _, _ ->
        :ok
    else
      n ->
        cond do
          n === 0 ->
            true = :ets.delete(:pg2_table, {:pid, pid, name})

            _ =
              for _ <- [:EFE_DUMMY_GEN], node(pid) === node() do
                :ets.delete(:pg2_table, {:local_member, name, pid})
              end

            true = :ets.delete(:pg2_table, member_Name_Pid)

          true ->
            :ok
        end

        ref_Pid = {:ref, pid}

        case :ets.update_counter(:pg2_table, ref_Pid, {4, -1}) do
          0 ->
            [{^ref_Pid, rPid, ref, 0}] =
              :ets.lookup(
                :pg2_table,
                ref_Pid
              )

            true = :ets.delete(:pg2_table, {:ref, ref})
            true = :ets.delete(:pg2_table, ref_Pid)
            true = :erlang.demonitor(ref, [:flush])
            kill_monitor_proc(rPid, pid)

          _ ->
            :ok
        end
    end
  end

  defp all_members() do
    for g <- all_groups() do
      [g, group_members(g)]
    end
  end

  defp group_members(name) do
    for [p, n] <-
          :ets.match(
            :pg2_table,
            {{:member, name, :"$1"}, :"$2"}
          ),
        _ <- :lists.seq(1, n) do
      p
    end
  end

  defp local_group_members(name) do
    for [pid] <-
          :ets.match(
            :pg2_table,
            {{:local_member, name, :"$1"}}
          ),
        p <- member_in_group(pid, name) do
      p
    end
  end

  defp member_in_group(pid, name) do
    case :ets.lookup(:pg2_table, {:member, name, pid}) do
      [] ->
        []

      [{{:member, ^name, ^pid}, n}] ->
        :lists.duplicate(n, pid)
    end
  end

  defp member_groups(pid) do
    for [name] <-
          :ets.match(
            :pg2_table,
            {{:pid, pid, :"$1"}}
          ) do
      name
    end
  end

  defp all_groups() do
    for [n] <- :ets.match(:pg2_table, {{:group, :"$1"}}) do
      n
    end
  end

  defp ensure_started() do
    case :erlang.whereis(:pg2) do
      :undefined ->
        c = {:pg2, {:pg2, :start_link, []}, :permanent, 1000, :worker, [:pg2]}
        :supervisor.start_child(:kernel_safe_sup, c)

      pg2Pid ->
        {:ok, pg2Pid}
    end
  end

  defp kill_monitor_proc(rPid, pid) do
    rPid === pid or :erlang.exit(rPid, :kill)
  end

  defp do_monitor(pid) do
    case node(pid) === node() or
           :lists.member(
             node(pid),
             :erlang.nodes()
           ) do
      true ->
        {pid, :erlang.monitor(:process, pid)}

      false ->
        f = fn ->
          ref = :erlang.monitor(:process, pid)

          receive do
            {:DOWN, ^ref, :process, ^pid, _Info} ->
              exit(:normal)
          end
        end

        :erlang.spawn_monitor(f)
    end
  end
end
