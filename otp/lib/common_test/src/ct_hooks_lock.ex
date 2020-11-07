defmodule :m_ct_hooks_lock do
  use Bitwise
  @behaviour :gen_server
  require Record
  Record.defrecord(:r_state, :state, id: :undefined, locked: false, requests: [])

  def start(id) do
    case :gen_server.start({:local, :ct_hooks_lock}, :ct_hooks_lock, id, []) do
      {:error, {:already_started, pid}} ->
        {:ok, pid}

      else__ ->
        else__
    end
  end

  def stop(id) do
    try do
      :gen_server.call(:ct_hooks_lock, {:stop, id})
    catch
      :exit, {:noproc, _} ->
        :stopped
    end
  end

  def request() do
    try do
      :gen_server.call(:ct_hooks_lock, {:request, self()}, :infinity)
    catch
      :exit, {:noproc, _} ->
        :locked
    end
  end

  def release() do
    try do
      :gen_server.call(:ct_hooks_lock, {:release, self()})
    catch
      :exit, {:noproc, _} ->
        :unlocked
    end
  end

  def init(id) do
    :ct_util.mark_process()
    {:ok, r_state(id: id)}
  end

  def handle_call({:stop, id}, _From, r_state(id: id, requests: reqs) = state) do
    _ =
      for {req, _ReqId} <- reqs do
        :gen_server.reply(req, :locker_stopped)
      end

    {:stop, :normal, :stopped, state}
  end

  def handle_call({:stop, _Id}, _From, state) do
    {:reply, :stopped, state}
  end

  def handle_call({:request, pid}, _From, r_state(locked: false, requests: []) = state) do
    ref = :erlang.monitor(:process, pid)
    {:reply, :locked, r_state(state, locked: {true, pid, ref})}
  end

  def handle_call({:request, pid}, from, r_state(requests: reqs) = state) do
    {:noreply, r_state(state, requests: reqs ++ [{from, pid}])}
  end

  def handle_call({:release, pid}, _From, r_state(locked: {true, pid, ref}, requests: []) = state) do
    :erlang.demonitor(ref, [:flush])
    {:reply, :unlocked, r_state(state, locked: false)}
  end

  def handle_call(
        {:release, pid},
        _From,
        r_state(
          locked: {true, pid, ref},
          requests: [{nextFrom, nextPid} | rest]
        ) = state
      ) do
    :erlang.demonitor(ref, [:flush])
    :gen_server.reply(nextFrom, :locked)
    nextRef = :erlang.monitor(:process, nextPid)

    {:reply, :unlocked,
     r_state(state,
       locked: {true, nextPid, nextRef},
       requests: rest
     )}
  end

  def handle_call({:release, _Pid}, _From, state) do
    {:reply, :not_locked, state}
  end

  def handle_cast(_Msg, state) do
    {:noreply, state}
  end

  def handle_info(
        {:DOWN, ref, :process, pid, _},
        r_state(
          locked: {true, pid, ref},
          requests: [{nextFrom, nextPid} | rest]
        ) = state
      ) do
    :gen_server.reply(nextFrom, :locked)
    nextRef = :erlang.monitor(:process, nextPid)

    {:noreply,
     r_state(state,
       locked: {true, nextPid, nextRef},
       requests: rest
     )}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end
end
