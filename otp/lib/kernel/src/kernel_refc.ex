defmodule :m_kernel_refc do
  use Bitwise
  @behaviour :gen_server
  def start_link() do
    :gen_server.start_link({:local, :kernel_refc}, :kernel_refc, [], [])
  end

  def scheduler_wall_time(bool) do
    :gen_server.call(:kernel_refc, {:scheduler_wall_time, self(), bool}, :infinity)
  end

  def init([]) do
    resource(:scheduler_wall_time, false)
    {:ok, %{scheduler_wall_time: %{}}}
  end

  def handle_call({what, who, false}, _From, state) do
    {reply, cnt} = do_stop(what, :maps.get(what, state), who)
    {:reply, reply, %{state | what => cnt}}
  end

  def handle_call({what, who, true}, _From, state) do
    {reply, cnt} = do_start(what, :maps.get(what, state), who)
    {:reply, reply, %{state | what => cnt}}
  end

  def handle_call(_, _From, state) do
    {:reply, :badarg, state}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  def handle_info({:DOWN, _Ref, :process, pid, _}, state) do
    cleanup = fn resource, cnts ->
      cleanup(resource, cnts, pid)
    end

    {:noreply, :maps.map(cleanup, state)}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp do_start(resource, cnt, pid) do
    case :maps.get(pid, cnt, :undefined) do
      :undefined ->
        ref = :erlang.monitor(:process, pid)

        case any(cnt) do
          true ->
            {true, Map.put(cnt, pid, {1, ref})}

          false ->
            resource(resource, true)
            {false, Map.put(cnt, pid, {1, ref})}
        end

      {n, ref} ->
        {true, Map.put(cnt, pid, {n + 1, ref})}
    end
  end

  defp do_stop(resource, cnt0, pid) do
    case :maps.get(pid, cnt0, :undefined) do
      :undefined ->
        {any(cnt0), cnt0}

      {1, ref} ->
        :erlang.demonitor(ref, [:flush])
        cnt = :maps.remove(pid, cnt0)

        case any(cnt) do
          true ->
            {true, cnt}

          false ->
            resource(resource, false)
            {true, cnt}
        end

      {n, ref} ->
        {true, Map.put(cnt0, pid, {n - 1, ref})}
    end
  end

  defp cleanup(resource, cnt0, pid) do
    case :maps.is_key(pid, cnt0) do
      true ->
        cnt = :maps.remove(pid, cnt0)

        case any(cnt) do
          true ->
            cnt

          false ->
            resource(resource, false)
            cnt
        end

      false ->
        cnt0
    end
  end

  defp any(cnt) do
    :maps.size(cnt) > 0
  end

  defp resource(:scheduler_wall_time, enable) do
    _ = :erts_internal.scheduler_wall_time(enable)
  end
end
