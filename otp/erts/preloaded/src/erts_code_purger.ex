defmodule :m_erts_code_purger do
  use Bitwise

  def start() do
    :erlang.register(:erts_code_purger, self())
    :erlang.process_flag(:trap_exit, true)
    wait_for_request()
  end

  defp wait_for_request() do
    handle_request(
      receive do
        msg ->
          msg
      end,
      []
    )
  end

  defp handle_request({:purge, mod, from, ref}, reqs)
       when is_atom(mod) and is_pid(from) do
    {res, newReqs} = do_purge(mod, reqs)
    send(from, {:reply, :purge, res, ref})
    check_requests(newReqs)
  end

  defp handle_request({:soft_purge, mod, from, ref}, reqs)
       when is_atom(mod) and is_pid(from) do
    {res, newReqs} = do_soft_purge(mod, reqs)
    send(from, {:reply, :soft_purge, res, ref})
    check_requests(newReqs)
  end

  defp handle_request(
         {:finish_after_on_load, {mod, keep}, from, ref},
         reqs
       )
       when is_atom(mod) and is_boolean(keep) and
              is_pid(from) do
    newReqs = do_finish_after_on_load(mod, keep, reqs)
    send(from, {:reply, :finish_after_on_load, :ok, ref})
    check_requests(newReqs)
  end

  defp handle_request({:test_purge, mod, from, type, ref}, reqs)
       when is_atom(mod) and is_pid(from) do
    newReqs = do_test_purge(mod, from, type, ref, reqs)
    check_requests(newReqs)
  end

  defp handle_request(_Garbage, reqs) do
    check_requests(reqs)
  end

  defp check_requests([]) do
    wait_for_request()
  end

  defp check_requests([r | rs]) do
    handle_request(r, rs)
  end

  def pending_purge_lambda(_Module, fun, args) do
    apply(fun, args)
  end

  def purge(mod) when is_atom(mod) do
    ref = make_ref()
    send(:erts_code_purger, {:purge, mod, self(), ref})

    receive do
      {:reply, :purge, result, ^ref} ->
        result
    end
  end

  defp do_purge(mod, reqs) do
    case :erts_internal.purge_module(mod, :prepare) do
      false ->
        {{false, false}, reqs}

      true ->
        {didKill, newReqs} = check_proc_code(:erlang.processes(), mod, true, reqs)
        true = :erts_internal.purge_module(mod, :complete)
        {{true, didKill}, newReqs}
    end
  end

  def soft_purge(mod) do
    ref = make_ref()
    send(:erts_code_purger, {:soft_purge, mod, self(), ref})

    receive do
      {:reply, :soft_purge, result, ^ref} ->
        result
    end
  end

  defp do_soft_purge(mod, reqs) do
    case :erts_internal.purge_module(mod, :prepare) do
      false ->
        {true, reqs}

      true ->
        {purgeOp, newReqs} = check_proc_code(:erlang.processes(), mod, false, reqs)
        {:erts_internal.purge_module(mod, purgeOp), newReqs}
    end
  end

  def finish_after_on_load(mod, keep) do
    ref = make_ref()
    send(:erts_code_purger, {:finish_after_on_load, {mod, keep}, self(), ref})

    receive do
      {:reply, :finish_after_on_load, result, ^ref} ->
        result
    end
  end

  defp do_finish_after_on_load(mod, keep, reqs) do
    :erlang.finish_after_on_load(mod, keep)

    case keep do
      true ->
        reqs

      false ->
        case :erts_internal.purge_module(
               mod,
               :prepare_on_load
             ) do
          false ->
            reqs

          true ->
            {_DidKill, newReqs} = check_proc_code(:erlang.processes(), mod, true, reqs)
            true = :erts_internal.purge_module(mod, :complete)
            newReqs
        end
    end
  end

  require Record

  Record.defrecord(:r_cpc_static, :cpc_static,
    hard: :undefined,
    module: :undefined,
    tag: :undefined,
    purge_requests: :undefined
  )

  Record.defrecord(:r_cpc_kill, :cpc_kill,
    outstanding: [],
    no_outstanding: 0,
    waiting: [],
    killed: false
  )

  defp check_proc_code(pids, mod, hard, pReqs) do
    tag = :erlang.make_ref()
    cpcS = r_cpc_static(hard: hard, module: mod, tag: tag, purge_requests: pReqs)
    cpc_receive(cpcS, cpc_init(cpcS, pids, 0), r_cpc_kill(), [])
  end

  defp cpc_receive(
         r_cpc_static(hard: true) = cpcS,
         0,
         r_cpc_kill(outstanding: [], waiting: [], killed: killed),
         pReqs
       ) do
    cpc_result(cpcS, pReqs, killed)
  end

  defp cpc_receive(r_cpc_static(hard: false) = cpcS, 0, _KillState, pReqs) do
    cpc_result(cpcS, pReqs, :complete)
  end

  defp cpc_receive(r_cpc_static(tag: tag) = cpcS, noReq, killState0, pReqs) do
    receive do
      {:check_process_code, {^tag, _Pid}, false} ->
        cpc_receive(cpcS, noReq - 1, killState0, pReqs)

      {:check_process_code, {^tag, pid}, true} ->
        case r_cpc_static(cpcS, :hard) do
          false ->
            cpc_result(cpcS, pReqs, :abort)

          true ->
            killState1 = cpc_sched_kill(pid, killState0)
            cpc_receive(cpcS, noReq - 1, killState1, pReqs)
        end

      {:DOWN, monRef, :process, _, _} ->
        killState1 = cpc_handle_down(monRef, killState0)
        cpc_receive(cpcS, noReq, killState1, pReqs)

      pReq
      when :erlang.element(1, pReq) == :purge or
             :erlang.element(1, pReq) == :soft_purge or
             :erlang.element(1, pReq) == :test_purge ->
        cpc_receive(cpcS, noReq, killState0, [pReq | pReqs])

      _Garbage ->
        cpc_receive(cpcS, noReq, killState0, pReqs)
    end
  end

  defp cpc_result(r_cpc_static(purge_requests: pReqs), newPReqs, res) do
    {res, pReqs ++ cpc_reverse(newPReqs)}
  end

  defp cpc_reverse([_] = l) do
    l
  end

  defp cpc_reverse(xs) do
    cpc_reverse(xs, [])
  end

  defp cpc_reverse([], ys) do
    ys
  end

  defp cpc_reverse([x | xs], ys) do
    cpc_reverse(xs, [x | ys])
  end

  defp cpc_handle_down(
         r,
         r_cpc_kill(outstanding: rs, no_outstanding: n) = killState0
       ) do
    try do
      newOutst = cpc_list_rm(r, rs)

      killState1 =
        r_cpc_kill(killState0,
          outstanding: newOutst,
          no_outstanding: n - 1
        )

      cpc_sched_kill_waiting(killState1)
    catch
      :undefined ->
        killState0
    end
  end

  defp cpc_list_rm(_R, []) do
    throw(:undefined)
  end

  defp cpc_list_rm(r, [r | rs]) do
    rs
  end

  defp cpc_list_rm(r0, [r1 | rs]) do
    [r1 | cpc_list_rm(r0, rs)]
  end

  defp cpc_sched_kill_waiting(r_cpc_kill(waiting: []) = killState) do
    killState
  end

  defp cpc_sched_kill_waiting(
         r_cpc_kill(outstanding: rs, no_outstanding: n, waiting: [p | ps]) = killState
       ) do
    r = :erlang.monitor(:process, p)
    :erlang.exit(p, :kill)
    r_cpc_kill(killState, outstanding: [r | rs], no_outstanding: n + 1, waiting: ps, killed: true)
  end

  defp cpc_sched_kill(
         pid,
         r_cpc_kill(no_outstanding: n, waiting: pids) = killState
       )
       when n >= 10 do
    r_cpc_kill(killState, waiting: [pid | pids])
  end

  defp cpc_sched_kill(
         pid,
         r_cpc_kill(outstanding: rs, no_outstanding: n) = killState
       ) do
    r = :erlang.monitor(:process, pid)
    :erlang.exit(pid, :kill)
    r_cpc_kill(killState, outstanding: [r | rs], no_outstanding: n + 1, killed: true)
  end

  defp cpc_request(r_cpc_static(tag: tag, module: mod), pid) do
    :erts_internal.check_process_code(pid, mod, [{:async, {tag, pid}}])
  end

  defp cpc_init(_CpcS, [], noReqs) do
    noReqs
  end

  defp cpc_init(cpcS, [pid | pids], noReqs) do
    cpc_request(cpcS, pid)
    cpc_init(cpcS, pids, noReqs + 1)
  end

  defp do_test_purge(mod, from, true, ref, reqs) do
    {res, newReqs} = do_test_hard_purge(mod, from, ref, reqs)
    send(from, {:test_purge, res, ref})
    newReqs
  end

  defp do_test_purge(mod, from, false, ref, reqs) do
    {res, newReqs} = do_test_soft_purge(mod, from, ref, reqs)
    send(from, {:test_purge, res, ref})
    newReqs
  end

  defp do_test_purge(_, _, _, _, reqs) do
    reqs
  end

  defp do_test_soft_purge(mod, from, ref, reqs) do
    prepRes = :erts_internal.purge_module(mod, :prepare)
    testRes = test_progress(:started, from, ref, :ok)

    case prepRes do
      false ->
        _ = test_progress(:continued, from, ref, testRes)
        {true, reqs}

      true ->
        {purgeOp, newReqs} = check_proc_code(:erlang.processes(), mod, false, reqs)
        _ = test_progress(:continued, from, ref, testRes)
        {:erts_internal.purge_module(mod, purgeOp), newReqs}
    end
  end

  defp do_test_hard_purge(mod, from, ref, reqs) do
    prepRes = :erts_internal.purge_module(mod, :prepare)
    testRes = test_progress(:started, from, ref, :ok)

    case prepRes do
      false ->
        _ = test_progress(:continued, from, ref, testRes)
        {{false, false}, reqs}

      true ->
        {didKill, newReqs} = check_proc_code(:erlang.processes(), mod, true, reqs)
        _ = test_progress(:continued, from, ref, testRes)
        true = :erts_internal.purge_module(mod, :complete)
        {{true, didKill}, newReqs}
    end
  end

  defp test_progress(_State, _From, _Ref, :died) do
    :died
  end

  defp test_progress(:started, from, ref, :ok) do
    send(from, {:started, ref})
    mon = :erlang.monitor(:process, from)

    receive do
      {:DOWN, ^mon, :process, ^from, _} ->
        :died

      {:continue, ^ref} ->
        :erlang.demonitor(mon, [:flush])
        :ok
    end
  end

  defp test_progress(:continued, from, ref, :ok) do
    send(from, {:continued, ref})
    mon = :erlang.monitor(:process, from)

    receive do
      {:DOWN, ^mon, :process, ^from, _} ->
        :died

      {:complete, ^ref} ->
        :erlang.demonitor(mon, [:flush])
        :ok
    end
  end
end
