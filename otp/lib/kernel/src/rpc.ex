defmodule :m_rpc do
  use Bitwise
  @behaviour :gen_server
  def start() do
    :gen_server.start({:local, :rex}, :rpc, [], [{:spawn_opt, [{:message_queue_data, :off_heap}]}])
  end

  def start_link() do
    :gen_server.start_link({:local, :rex}, :rpc, [], [
      {:spawn_opt, [{:message_queue_data, :off_heap}]}
    ])
  end

  def stop() do
    stop(:rex)
  end

  defp stop(rpc) do
    :gen_server.call(rpc, :stop, :infinity)
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    {:ok, %{nodes_observer: start_nodes_observer()}}
  end

  def handle_call({:call, mod, fun, args, gleader}, to, s) do
    execCall = fn ->
      set_group_leader(gleader)
      reply = execute_call(mod, fun, args)
      :gen_server.reply(to, reply)
    end

    try do
      {_, mon} = spawn_monitor(execCall)
      {:noreply, :maps.put(mon, to, s)}
    catch
      :error, :system_limit ->
        {:reply, {:badrpc, {:EXIT, :system_limit}}, s}
    end
  end

  def handle_call({:block_call, mod, fun, args, gleader}, _To, s) do
    myGL = :erlang.group_leader()
    set_group_leader(gleader)
    reply = execute_call(mod, fun, args)
    :erlang.group_leader(myGL, self())
    {:reply, reply, s}
  end

  def handle_call(:stop, _To, s) do
    {:stop, :normal, :stopped, s}
  end

  def handle_call(_, _To, s) do
    {:noreply, s}
  end

  def handle_cast({:cast, mod, fun, args, gleader}, s) do
    _ =
      try do
        spawn(fn ->
          set_group_leader(gleader)
          :erpc.execute_cast(mod, fun, args)
        end)
      catch
        :error, :system_limit ->
          :ok
      end

    {:noreply, s}
  end

  def handle_cast(_, s) do
    {:noreply, s}
  end

  def handle_info(
        {:DOWN, m, :process, p, _},
        %{nodes_observer: {p, m}} = s
      ) do
    {:noreply, Map.put(s, :nodes_observer, start_nodes_observer())}
  end

  def handle_info({:DOWN, m, :process, _, :normal}, s) do
    {:noreply, :maps.remove(m, s)}
  end

  def handle_info({:DOWN, m, :process, _, reason}, s) do
    case :maps.get(m, s, :undefined) do
      :undefined ->
        {:noreply, s}

      {_, _} = to ->
        :gen_server.reply(to, {:badrpc, {:EXIT, reason}})
        {:noreply, :maps.remove(m, s)}
    end
  end

  def handle_info({from, {:sbcast, name, msg}}, s) do
    _ =
      case (try do
              send(name, msg)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:EXIT, _} ->
          send(from, {:rex, node(), {:nonexisting_name, name}})

        _ ->
          send(from, {:rex, node(), node()})
      end

    {:noreply, s}
  end

  def handle_info({from, {:send, name, msg}}, s) do
    _ =
      case (try do
              send(name, {from, msg})
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:EXIT, _} ->
          send(from, {:rex, node(), {:nonexisting_name, name}})

        _ ->
          :ok
      end

    {:noreply, s}
  end

  def handle_info(
        {from, {:call, _Mod, _Fun, _Args, _Gleader} = request},
        s
      ) do
    to = {from, :rex}

    case handle_call(request, to, s) do
      {:noreply, _NewS} = return ->
        return

      {:reply, reply, newS} ->
        :gen_server.reply(to, reply)
        {:noreply, newS}
    end
  end

  def handle_info({from, :features_request}, s) do
    send(from, {:features_reply, node(), [:erpc]})
    {:noreply, s}
  end

  def handle_info(_, s) do
    {:noreply, s}
  end

  def terminate(_, _S) do
    :ok
  end

  def code_change(_, s, _) do
    {:ok, s}
  end

  defp execute_call(mod, fun, args) do
    try do
      {:return, return} = :erpc.execute_call(mod, fun, args)
      return
    catch
      result ->
        result

      :exit, reason ->
        {:badrpc, {:EXIT, reason}}

      :error, reason ->
        case :erpc.is_arg_error(reason, mod, fun, args) do
          true ->
            {:badrpc, {:EXIT, reason}}

          false ->
            rpcStack = :erpc.trim_stack(__STACKTRACE__, mod, fun, args)
            {:badrpc, {:EXIT, {reason, rpcStack}}}
        end
    end
  end

  defp set_group_leader(gleader) when is_pid(gleader) do
    :erlang.group_leader(gleader, self())
  end

  defp set_group_leader(:user) do
    gleader =
      case :erlang.whereis(:user) do
        pid when is_pid(pid) ->
          pid

        :undefined ->
          proxy_user()
      end

    :erlang.group_leader(gleader, self())
  end

  defp proxy_user() do
    case :erlang.whereis(:rex_proxy_user) do
      pid when is_pid(pid) ->
        pid

      :undefined ->
        pid =
          spawn(fn ->
            proxy_user_loop()
          end)

        try do
          :erlang.register(:rex_proxy_user, pid)
        catch
          :error, _ ->
            :erlang.exit(pid, :kill)
            proxy_user()
        else
          true ->
            pid
        end
    end
  end

  defp proxy_user_loop() do
    :timer.sleep(200)

    case :erlang.whereis(:user) do
      pid when is_pid(pid) ->
        proxy_user_flush()

      :undefined ->
        proxy_user_loop()
    end
  end

  def proxy_user_flush() do
    receive do
      msg ->
        send(:user, msg)
    after
      10 * 1000 ->
        :erlang.hibernate(:rpc, :proxy_user_flush, [])
    end

    proxy_user_flush()
  end

  defp start_nodes_observer() do
    init = fn ->
      :erlang.process_flag(:priority, :high)
      :erlang.process_flag(:trap_exit, true)

      tab =
        :ets.new(
          :rex_nodes_observer,
          [{:read_concurrency, true}, :protected]
        )

      :persistent_term.put(:rex_nodes_observer, tab)
      :ok = :net_kernel.monitor_nodes(true)

      :lists.foreach(
        fn n ->
          send(self(), {:nodeup, n})
        end,
        [node() | :erlang.nodes()]
      )

      nodes_observer_loop(tab)
    end

    spawn_monitor(init)
  end

  defp nodes_observer_loop(tab) do
    receive do
      {:nodeup, :nonode@nohost} ->
        :ok

      {:nodeup, n} ->
        send({:rex, n}, {self(), :features_request})

      {:nodedown, n} ->
        :ets.delete(tab, n)

      {:features_reply, n, featureList} ->
        try do
          spawnRpc = :lists.member(:erpc, featureList)
          :ets.insert(tab, {n, spawnRpc})
        catch
          _, _ ->
            :ets.insert(tab, {n, false})
        end

      _ ->
        :ignore
    end

    nodes_observer_loop(tab)
  end

  defp node_has_feature(n, :erpc) when n == node() do
    true
  end

  defp node_has_feature(n, :erpc) do
    try do
      tab = :persistent_term.get(:rex_nodes_observer)
      :ets.lookup_element(tab, n, 2)
    catch
      _, _ ->
        false
    end
  end

  defp node_has_feature(_N, _Feature) do
    false
  end

  def call(n, m, f, a) do
    call(n, m, f, a, :infinity)
  end

  def call(n, m, f, a, t) do
    dL =
      try do
        deadline(t)
      catch
        :error, _ ->
          :erlang.error(:badarg)
      end

    case (try do
            :erpc.call(n, m, f, a, t)
          catch
            class_, reason_ ->
              rpcify_exception(class_, reason_)
          else
            {:EXIT, _} = badRpc_ ->
              {:badrpc, badRpc_}

            result_ ->
              result_
          end) do
      {:badrpc, :notsup} ->
        case time_left(dL) do
          0 ->
            {:badrpc, :timeout}

          timeout ->
            do_srv_call(n, {:call, m, f, a, :erlang.group_leader()}, timeout)
        end

      res ->
        res
    end
  end

  def block_call(n, m, f, a) do
    block_call(n, m, f, a, :infinity)
  end

  def block_call(n, m, f, a, timeout)
      when (is_atom(n) and
              is_atom(m) and is_list(a) and
              timeout == :infinity) or
             (is_integer(timeout) and 0 <= timeout and timeout <= 4_294_967_295) do
    do_srv_call(n, {:block_call, m, f, a, :erlang.group_leader()}, timeout)
  end

  defp rpcify_exception(:throw, {:EXIT, _} = badRpc) do
    {:badrpc, badRpc}
  end

  defp rpcify_exception(:throw, return) do
    return
  end

  defp rpcify_exception(:exit, {:exception, exit}) do
    {:badrpc, {:EXIT, exit}}
  end

  defp rpcify_exception(:exit, {:signal, reason}) do
    {:badrpc, {:EXIT, reason}}
  end

  defp rpcify_exception(:exit, reason) do
    exit(reason)
  end

  defp rpcify_exception(:error, {:exception, error, stack}) do
    {:badrpc, {:EXIT, {error, stack}}}
  end

  defp rpcify_exception(:error, {:erpc, :badarg}) do
    :erlang.error(:badarg)
  end

  defp rpcify_exception(:error, {:erpc, :noconnection}) do
    {:badrpc, :nodedown}
  end

  defp rpcify_exception(:error, {:erpc, :timeout}) do
    {:badrpc, :timeout}
  end

  defp rpcify_exception(:error, {:erpc, :notsup}) do
    {:badrpc, :notsup}
  end

  defp rpcify_exception(:error, {:erpc, error}) do
    {:badrpc, {:EXIT, error}}
  end

  defp rpcify_exception(:error, reason) do
    :erlang.error(reason)
  end

  defp do_srv_call(node, request, :infinity) do
    rpc_check(
      try do
        :gen_server.call({:rex, node}, request, :infinity)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end
    )
  end

  defp do_srv_call(node, request, timeout) do
    tag = make_ref()

    {receiver, mref} =
      :erlang.spawn_monitor(fn ->
        :erlang.process_flag(
          :trap_exit,
          true
        )

        result =
          :gen_server.call(
            {:rex, node},
            request,
            timeout
          )

        exit({self(), tag, result})
      end)

    receive do
      {:DOWN, ^mref, _, _, {^receiver, ^tag, result}} ->
        rpc_check(result)

      {:DOWN, ^mref, _, _, reason} ->
        rpc_check_t({:EXIT, reason})
    end
  end

  defp rpc_check_t({:EXIT, {:timeout, _}}) do
    {:badrpc, :timeout}
  end

  defp rpc_check_t({:EXIT, {:timeout_value, _}}) do
    :erlang.error(:badarg)
  end

  defp rpc_check_t(x) do
    rpc_check(x)
  end

  defp rpc_check({:EXIT, {{:nodedown, _}, _}}) do
    {:badrpc, :nodedown}
  end

  defp rpc_check({:EXIT, _} = exit) do
    {:badrpc, exit}
  end

  defp rpc_check(x) do
    x
  end

  def server_call(node, name, replyWrapper, msg)
      when is_atom(node) and is_atom(name) do
    cond do
      node() === :nonode@nohost and
          node !== :nonode@nohost ->
        {:error, :nodedown}

      true ->
        ref = :erlang.monitor(:process, {name, node})
        send({name, node}, {self(), msg})

        receive do
          {:DOWN, ^ref, _, _, _} ->
            {:error, :nodedown}

          {^replyWrapper, ^node, reply} ->
            :erlang.demonitor(ref, [:flush])
            reply
        end
    end
  end

  def cast(node, mod, fun, args)
      when is_atom(node) and
             is_atom(mod) and is_atom(fun) and
             is_list(args) do
    _ =
      case node_has_feature(node, :erpc) do
        false ->
          :gen_server.cast(
            {:rex, node},
            {:cast, mod, fun, args, :erlang.group_leader()}
          )

        true ->
          try do
            :ok = :erpc.cast(node, mod, fun, args)
          catch
            :error, {:erpc, :badarg} ->
              :erlang.error(:badarg)
          end
      end

    true
  end

  def cast(_, _, _, _) do
    :erlang.error(:badarg)
  end

  def abcast(name, mess) do
    abcast([node() | :erlang.nodes()], name, mess)
  end

  def abcast([node | tail], name, mess) do
    dest = {name, node}

    try do
      :erlang.send(dest, mess)
    catch
      :error, _ ->
        :ok
    end

    abcast(tail, name, mess)
  end

  def abcast([], _, _) do
    :abcast
  end

  def sbcast(name, mess) do
    sbcast([node() | :erlang.nodes()], name, mess)
  end

  def sbcast(nodes, name, mess) do
    monitors = send_nodes(nodes, :rex, {:sbcast, name, mess}, [])
    rec_nodes(:rex, monitors)
  end

  def eval_everywhere(mod, fun, args) do
    eval_everywhere([node() | :erlang.nodes()], mod, fun, args)
  end

  def eval_everywhere(nodes, mod, fun, args) do
    :lists.foreach(
      fn node ->
        cast(node, mod, fun, args)
      end,
      nodes
    )

    :abcast
  end

  defp send_nodes([node | tail], name, msg, monitors)
       when is_atom(node) do
    monitor = start_monitor(node, name)

    try do
      send({name, node}, {self(), msg})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    send_nodes(tail, name, msg, [monitor | monitors])
  end

  defp send_nodes([_Node | tail], name, msg, monitors) do
    send_nodes(tail, name, msg, monitors)
  end

  defp send_nodes([], _Name, _Req, monitors) do
    monitors
  end

  defp start_monitor(node, name) do
    cond do
      node() === :nonode@nohost and
          node !== :nonode@nohost ->
        ref = make_ref()
        send(self(), {:DOWN, ref, :process, {name, node}, :noconnection})
        {node, ref}

      true ->
        {node, :erlang.monitor(:process, {name, node})}
    end
  end

  def multicall(m, f, a) do
    multicall(m, f, a, :infinity)
  end

  def multicall(nodes, m, f, a) when is_list(nodes) do
    multicall(nodes, m, f, a, :infinity)
  end

  def multicall(m, f, a, timeout) do
    multicall([node() | :erlang.nodes()], m, f, a, timeout)
  end

  def multicall(nodes, m, f, a, timeout) do
    try do
      true = is_atom(m)
      true = is_atom(f)
      true = is_list(a)
      deadline = deadline(timeout)
      res = make_ref()
      mFA = {m, f, a}
      {nRs, reqMap0} = mc_requests(res, nodes, m, f, a, [], %{})
      reqMap1 = mc_spawn_replies(res, :maps.size(reqMap0), reqMap0, mFA, deadline)
      mc_results(res, nRs, [], [], reqMap1, mFA, deadline)
    catch
      :error, notIError when notIError != :internal_error ->
        :erlang.error(:badarg)
    end
  end

  defp mc_requests(_Res, [], _M, _F, _A, nRs, reqMap) do
    {nRs, reqMap}
  end

  defp mc_requests(res, [n | ns], m, f, a, nRs, reqMap) do
    reqId =
      try do
        :erlang.spawn_request(n, :erpc, :execute_call, [res, m, f, a], [
          {:reply_tag, {:spawn_reply, res, n}},
          :monitor
        ])
      catch
        _, _ ->
          mc_fail_requests(res, nRs)
      end

    nR = {n, reqId}
    mc_requests(res, ns, m, f, a, [nR | nRs], Map.put(reqMap, reqId, :spawn_request))
  end

  defp mc_requests(res, _Error, _M, _F, _A, nRs, _ReqMap) do
    mc_fail_requests(res, nRs)
  end

  defp mc_fail_requests(_Res, []) do
    :erlang.error(:badarg)
  end

  defp mc_fail_requests(res, [{node, reqId} | nRs]) do
    case :erlang.spawn_request_abandon(reqId) do
      true ->
        :ok

      false ->
        receive do
          {{:spawn_reply, ^res, ^node}, ^reqId, :error, _} ->
            :ok

          {{:spawn_reply, ^res, ^node}, ^reqId, :ok, pid} ->
            case :erlang.demonitor(reqId, [:info]) do
              true ->
                :ok

              false ->
                receive do
                  {:DOWN, ^reqId, :process, ^pid, _} ->
                    :ok
                after
                  0 ->
                    :erlang.error(:internal_error)
                end
            end
        after
          0 ->
            :erlang.error(:internal_error)
        end
    end

    mc_fail_requests(res, nRs)
  end

  defp mc_spawn_replies(_Res, 0, reqMap, _MFA, _Deadline) do
    reqMap
  end

  defp mc_spawn_replies(res, outstanding, reqMap, mFA, deadline) do
    timeout = time_left(deadline)

    receive do
      {{:spawn_reply, ^res, _}, _, _, _} = reply ->
        newReqMap = mc_handle_spawn_reply(reply, reqMap, mFA, deadline)
        mc_spawn_replies(res, outstanding - 1, newReqMap, mFA, deadline)
    after
      timeout ->
        reqMap
    end
  end

  defp mc_handle_spawn_reply(
         {{:spawn_reply, _Res, _Node}, reqId, :ok, pid},
         reqMap,
         _MFA,
         _Deadline
       ) do
    Map.put(reqMap, reqId, {:spawn, pid})
  end

  defp mc_handle_spawn_reply(
         {{:spawn_reply, _Res, node}, reqId, :error, :notsup},
         reqMap,
         mFA,
         :infinity
       ) do
    {m, f, a} = mFA

    srvReqId =
      :gen_server.send_request(
        {:rex, node},
        {:call, m, f, a, :erlang.group_leader()}
      )

    Map.put(reqMap, reqId, {:server, srvReqId})
  end

  defp mc_handle_spawn_reply(
         {{:spawn_reply, res, node}, reqId, :error, :notsup},
         reqMap,
         mFA,
         deadline
       ) do
    {m, f, a} = mFA

    try do
      {pid, mon} =
        spawn_monitor(fn ->
          :erlang.process_flag(:trap_exit, true)
          request = {:call, m, f, a, :erlang.group_leader()}
          timeout = time_left(deadline)

          result =
            :gen_server.call(
              {:rex, node},
              request,
              timeout
            )

          exit({res, result})
        end)

      Map.put(reqMap, reqId, {:spawn_server, mon, pid})
    catch
      :error, :system_limit ->
        Map.put(reqMap, reqId, {:error, {:badrpc, {:EXIT, :system_limit}}})
    end
  end

  defp mc_handle_spawn_reply(
         {{:spawn_reply, _Res, _Node}, reqId, :error, :noconnection},
         reqMap,
         _MFA,
         _Deadline
       ) do
    Map.put(reqMap, reqId, {:error, :badnode})
  end

  defp mc_handle_spawn_reply(
         {{:spawn_reply, _Res, _Node}, reqId, :error, reason},
         reqMap,
         _MFA,
         _Deadline
       ) do
    Map.put(reqMap, reqId, {:error, {:badrpc, {:EXIT, reason}}})
  end

  defp mc_results(_Res, [], okAcc, errAcc, _ReqMap, _MFA, _Deadline) do
    {okAcc, errAcc}
  end

  defp mc_results(res, [{n, reqId} | nRs] = origNRs, okAcc, errAcc, reqMap, mFA, deadline) do
    case :maps.get(reqId, reqMap) do
      {:error, :badnode} ->
        mc_results(res, nRs, okAcc, [n | errAcc], reqMap, mFA, deadline)

      {:error, badRpc} ->
        mc_results(res, nRs, [badRpc | okAcc], errAcc, reqMap, mFA, deadline)

      :spawn_request ->
        case :erlang.spawn_request_abandon(reqId) do
          true ->
            mc_results(res, nRs, okAcc, [n | errAcc], reqMap, mFA, deadline)

          false ->
            receive do
              {{:spawn_reply, ^res, _}, ^reqId, _, _} = reply ->
                newReqMap = mc_handle_spawn_reply(reply, reqMap, mFA, deadline)
                mc_results(res, origNRs, okAcc, errAcc, newReqMap, mFA, deadline)
            after
              0 ->
                :erlang.error(:internal_error)
            end
        end

      {:spawn, pid} ->
        timeout = time_left(deadline)

        receive do
          {:DOWN, ^reqId, :process, ^pid, reason} ->
            case (try do
                    :erpc.call_result(:down, reqId, res, reason)
                  catch
                    class_, reason_ ->
                      rpcify_exception(class_, reason_)
                  else
                    {:EXIT, _} = badRpc_ ->
                      {:badrpc, badRpc_}

                    result_ ->
                      result_
                  end) do
              {:badrpc, :nodedown} ->
                mc_results(res, nRs, okAcc, [n | errAcc], reqMap, mFA, deadline)

              callRes ->
                mc_results(res, nRs, [callRes | okAcc], errAcc, reqMap, mFA, deadline)
            end
        after
          timeout ->
            case :erlang.demonitor(reqId, [:info]) do
              true ->
                mc_results(res, nRs, okAcc, [n | errAcc], reqMap, mFA, deadline)

              false ->
                receive do
                  {:DOWN, ^reqId, :process, ^pid, reason} ->
                    case (try do
                            :erpc.call_result(:down, reqId, res, reason)
                          catch
                            class_, reason_ ->
                              rpcify_exception(class_, reason_)
                          else
                            {:EXIT, _} = badRpc_ ->
                              {:badrpc, badRpc_}

                            result_ ->
                              result_
                          end) do
                      {:badrpc, :nodedown} ->
                        mc_results(res, nRs, okAcc, [n | errAcc], reqMap, mFA, deadline)

                      callRes ->
                        mc_results(res, nRs, [callRes | okAcc], errAcc, reqMap, mFA, deadline)
                    end
                after
                  0 ->
                    :erlang.error(:internal_error)
                end
            end
        end

      {:spawn_server, mon, pid} ->
        result =
          receive do
            {:DOWN, ^mon, :process, ^pid, {^res, callRes}} ->
              rpc_check(callRes)

            {:DOWN, ^mon, :process, ^pid, reason} ->
              rpc_check_t({:EXIT, reason})
          end

        case result do
          {:badrpc, badRpcReason}
          when badRpcReason == :timeout or
                 badRpcReason == :nodedown ->
            mc_results(res, nRs, okAcc, [n | errAcc], reqMap, mFA, deadline)

          _ ->
            mc_results(res, nRs, [result | okAcc], errAcc, reqMap, mFA, deadline)
        end

      {:server, srvReqId} ->
        case :gen_server.wait_response(srvReqId, :infinity) do
          {:reply, reply} ->
            result = rpc_check(reply)
            mc_results(res, nRs, [result | okAcc], errAcc, reqMap, mFA, deadline)

          {:error, {:noconnection, _}} ->
            mc_results(res, nRs, okAcc, [n | errAcc], reqMap, mFA, deadline)

          {:error, {reason, _}} ->
            badRpc = {:badrpc, {:EXIT, reason}}
            mc_results(res, nRs, [badRpc | okAcc], errAcc, reqMap, mFA, deadline)
        end
    end
  end

  defp deadline(:infinity) do
    :infinity
  end

  defp deadline(4_294_967_295) do
    :erlang.convert_time_unit(
      :erlang.monotonic_time(:millisecond) + 4_294_967_295,
      :millisecond,
      :native
    )
  end

  defp deadline(t)
       when is_integer(t) and 0 <= t and t <= 4_294_967_295 do
    now = :erlang.monotonic_time()
    nativeTmo = :erlang.convert_time_unit(t, :millisecond, :native)
    now + nativeTmo
  end

  defp time_left(:infinity) do
    :infinity
  end

  defp time_left(deadline) do
    case deadline - :erlang.monotonic_time() do
      timeLeft when timeLeft <= 0 ->
        0

      timeLeft ->
        :erlang.convert_time_unit(timeLeft - 1, :native, :millisecond) + 1
    end
  end

  def multi_server_call(name, msg) do
    multi_server_call([node() | :erlang.nodes()], name, msg)
  end

  def multi_server_call(nodes, name, msg)
      when is_list(nodes) and
             is_atom(name) do
    monitors = send_nodes(nodes, name, msg, [])
    rec_nodes(name, monitors)
  end

  defp rec_nodes(name, nodes) do
    rec_nodes(name, nodes, [], [])
  end

  defp rec_nodes(_Name, [], badnodes, replies) do
    {replies, badnodes}
  end

  defp rec_nodes(name, [{n, r} | tail], badnodes, replies) do
    receive do
      {:DOWN, ^r, _, _, _} ->
        rec_nodes(name, tail, [n | badnodes], replies)

      {:rex, ^n, {:nonexisting_name, _}} ->
        :erlang.demonitor(r, [:flush])
        rec_nodes(name, tail, [n | badnodes], replies)

      {^name, ^n, reply} ->
        :erlang.demonitor(r, [:flush])
        rec_nodes(name, tail, badnodes, [reply | replies])
    end
  end

  def async_call(node, mod, fun, args) do
    try do
      true = is_atom(node)
      true = is_atom(mod)
      true = is_atom(fun)
      true = is_integer(length(args))
    catch
      _, _ ->
        :erlang.error(:badarg)
    end

    caller = self()

    spawn_monitor(fn ->
      :erlang.process_flag(:trap_exit, true)
      r = call(node, mod, fun, args)
      exit({:async_call_result, caller, r})
    end)
  end

  def yield({pid, ref} = key)
      when is_pid(pid) and
             is_reference(ref) do
    {:value, r} = nb_yield(key, :infinity)
    r
  end

  def nb_yield({pid, ref} = key)
      when is_pid(pid) and
             is_reference(ref) do
    nb_yield(key, 0)
  end

  def nb_yield({proxy, mon}, tmo)
      when (is_pid(proxy) and
              is_reference(mon) and
              tmo == :infinity) or (is_integer(tmo) and 0 <= tmo and tmo <= 4_294_967_295) do
    me = self()

    receive do
      {:DOWN, ^mon, :process, ^proxy, {:async_call_result, ^me, r}} ->
        {:value, r}

      {:DOWN, ^mon, :process, ^proxy, reason} ->
        {:value, {:badrpc, {:EXIT, reason}}}
    after
      tmo ->
        :timeout
    end
  end

  def parallel_eval(argL) do
    nodes = [node() | :erlang.nodes()]
    keys = map_nodes(argL, nodes, nodes)

    for k <- keys do
      yield(k)
    end
  end

  defp map_nodes([], _, _) do
    []
  end

  defp map_nodes(argL, [], original) do
    map_nodes(argL, original, original)
  end

  defp map_nodes([{m, f, a} | tail], [node | moreNodes], original) do
    [:rpc.async_call(node, m, f, a) | map_nodes(tail, moreNodes, original)]
  end

  def pmap({m, f}, as, list) do
    check(parallel_eval(build_args(m, f, as, list, [])), [])
  end

  defp build_args(m, f, as, [arg | tail], acc) do
    build_args(m, f, as, tail, [{m, f, [arg | as]} | acc])
  end

  defp build_args(m, f, _, [], acc)
       when is_atom(m) and
              is_atom(f) do
    acc
  end

  defp check([{:badrpc, _} | _], _) do
    exit(:badrpc)
  end

  defp check([x | t], ack) do
    check(t, [x | ack])
  end

  defp check([], ack) do
    ack
  end

  def pinfo(pid) when node(pid) === node() do
    :erlang.process_info(pid)
  end

  def pinfo(pid) do
    call(node(pid), :erlang, :process_info, [pid])
  end

  def pinfo(pid, item) when node(pid) === node() do
    :erlang.process_info(pid, item)
  end

  def pinfo(pid, item) do
    block_call(node(pid), :erlang, :process_info, [pid, item])
  end
end
