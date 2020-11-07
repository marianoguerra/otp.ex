defmodule :m_erpc do
  use Bitwise

  def call(n, fun) do
    call(n, fun, :infinity)
  end

  def call(n, fun, timeout) when is_function(fun, 0) do
    call(n, :erlang, :apply, [fun, []], timeout)
  end

  def call(_N, _Fun, _Timeout) do
    :erlang.error({:erpc, :badarg})
  end

  def call(n, m, f, a) do
    call(n, m, f, a, :infinity)
  end

  def call(n, m, f, a, :infinity)
      when node() === n and
             is_atom(m) and is_atom(f) and
             is_list(a) do
    try do
      {:return, return} = execute_call(m, f, a)
      return
    catch
      :exit, reason ->
        exit({:exception, reason})

      :error, reason ->
        case is_arg_error(reason, m, f, a) do
          true ->
            :erlang.error({:erpc, reason})

          false ->
            erpcStack = trim_stack(__STACKTRACE__, m, f, a)
            :erlang.error({:exception, reason, erpcStack})
        end
    end
  end

  def call(n, m, f, a, t)
      when (is_atom(n) and
              is_atom(m) and is_atom(f) and is_list(a) and
              t == :infinity) or (is_integer(t) and 0 <= t and t <= 4_294_967_295) do
    res = make_ref()

    reqId =
      :erlang.spawn_request(n, :erpc, :execute_call, [res, m, f, a], [
        {:reply, :error_only},
        :monitor
      ])

    receive do
      {:spawn_reply, ^reqId, :error, reason} ->
        result(:spawn_reply, reqId, res, reason)

      {:DOWN, ^reqId, :process, _Pid, reason} ->
        result(:down, reqId, res, reason)
    after
      t ->
        result(:timeout, reqId, res, :undefined)
    end
  end

  def call(_N, _M, _F, _A, _T) do
    :erlang.error({:erpc, :badarg})
  end

  def send_request(n, f) when is_function(f, 0) do
    send_request(n, :erlang, :apply, [f, []])
  end

  def send_request(_N, _F) do
    :erlang.error({:erpc, :badarg})
  end

  def send_request(n, m, f, a)
      when is_atom(n) and is_atom(m) and
             is_atom(f) and is_list(a) do
    res = make_ref()

    reqId =
      :erlang.spawn_request(n, :erpc, :execute_call, [res, m, f, a], [
        {:reply, :error_only},
        :monitor
      ])

    {res, reqId}
  end

  def send_request(_N, _M, _F, _A) do
    :erlang.error({:erpc, :badarg})
  end

  def receive_response({res, reqId} = rId)
      when is_reference(res) and
             is_reference(reqId) do
    receive_response(rId, :infinity)
  end

  def receive_response(_) do
    :erlang.error({:erpc, :badarg})
  end

  def receive_response({res, reqId}, tmo)
      when (is_reference(res) and
              is_reference(reqId) and
              tmo == :infinity) or (is_integer(tmo) and 0 <= tmo and tmo <= 4_294_967_295) do
    receive do
      {:spawn_reply, ^reqId, :error, reason} ->
        result(:spawn_reply, reqId, res, reason)

      {:DOWN, ^reqId, :process, _Pid, reason} ->
        result(:down, reqId, res, reason)
    after
      tmo ->
        result(:timeout, reqId, res, :undefined)
    end
  end

  def receive_response(_, _) do
    :erlang.error({:erpc, :badarg})
  end

  def wait_response({res, reqId} = rId)
      when is_reference(res) and
             is_reference(reqId) do
    wait_response(rId, 0)
  end

  def wait_response({res, reqId}, wT)
      when (is_reference(res) and
              is_reference(reqId) and
              wT == :infinity) or (is_integer(wT) and 0 <= wT and wT <= 4_294_967_295) do
    receive do
      {:spawn_reply, ^reqId, :error, reason} ->
        result(:spawn_reply, reqId, res, reason)

      {:DOWN, ^reqId, :process, _Pid, reason} ->
        {:response, result(:down, reqId, res, reason)}
    after
      wT ->
        :no_response
    end
  end

  def wait_response(_, _) do
    :erlang.error({:erpc, :badarg})
  end

  def check_response(
        {:spawn_reply, reqId, :error, reason},
        {res, reqId}
      )
      when is_reference(res) and is_reference(reqId) do
    result(:spawn_reply, reqId, res, reason)
  end

  def check_response(
        {:DOWN, reqId, :process, _Pid, reason},
        {res, reqId}
      )
      when is_reference(res) and is_reference(reqId) do
    {:response, result(:down, reqId, res, reason)}
  end

  def check_response(_Msg, {res, reqId})
      when is_reference(res) and
             is_reference(reqId) do
    :no_response
  end

  def check_response(_, _) do
    :erlang.error({:erpc, :badarg})
  end

  def multicall(ns, fun) do
    multicall(ns, fun, :infinity)
  end

  def multicall(ns, fun, timeout) when is_function(fun, 0) do
    multicall(ns, :erlang, :apply, [fun, []], timeout)
  end

  def multicall(_Ns, _Fun, _Timeout) do
    :erlang.error({:erpc, :badarg})
  end

  def multicall(ns, m, f, a) do
    multicall(ns, m, f, a, :infinity)
  end

  def multicall(ns, m, f, a, t) do
    try do
      true = is_atom(m)
      true = is_atom(f)
      true = is_list(a)
      deadline = deadline(t)
      {reqIds, lC} = mcall_send_requests(ns, m, f, a, [], t, false)

      lRes =
        case lC do
          false ->
            :undefined

          true ->
            try do
              {:return, return} = execute_call(m, f, a)
              {:ok, return}
            catch
              thrown ->
                {:throw, thrown}

              :exit, reason ->
                {:exit, {:exception, reason}}

              :error, reason ->
                case is_arg_error(reason, m, f, a) do
                  true ->
                    {:error, {:erpc, reason}}

                  false ->
                    erpcStack = trim_stack(__STACKTRACE__, m, f, a)
                    {:error, {:exception, reason, erpcStack}}
                end
            end
        end

      mcall_receive_replies(reqIds, [], lRes, deadline)
    catch
      :error, notIErr when notIErr != :internal_error ->
        :erlang.error({:erpc, :badarg})
    end
  end

  def multicast(n, fun) do
    multicast(n, :erlang, :apply, [fun, []])
  end

  def multicast(nodes, mod, fun, args) do
    try do
      true = is_atom(mod)
      true = is_atom(fun)
      true = is_list(args)
      multicast_send_requests(nodes, mod, fun, args)
    catch
      :error, _ ->
        :erlang.error({:erpc, :badarg})
    end
  end

  defp multicast_send_requests([], _Mod, _Fun, _Args) do
    :ok
  end

  defp multicast_send_requests([node | nodes], mod, fun, args) do
    _ = :erlang.spawn_request(node, :erpc, :execute_cast, [mod, fun, args], [{:reply, :no}])
    multicast_send_requests(nodes, mod, fun, args)
  end

  def cast(n, fun) do
    cast(n, :erlang, :apply, [fun, []])
  end

  def cast(node, mod, fun, args)
      when is_atom(node) and
             is_atom(mod) and is_atom(fun) and
             is_list(args) do
    _ = :erlang.spawn_request(node, :erpc, :execute_cast, [mod, fun, args], [{:reply, :no}])
    :ok
  end

  def cast(_Node, _Mod, _Fun, _Args) do
    :erlang.error({:erpc, :badarg})
  end

  def execute_call(ref, m, f, a) do
    reply =
      try do
        {ref, :return, apply(m, f, a)}
      catch
        reason ->
          {ref, :throw, reason}

        :exit, reason ->
          {ref, :exit, reason}

        :error, reason ->
          case is_arg_error(reason, m, f, a) do
            true ->
              {ref, :error, {:erpc, reason}}

            false ->
              erpcStack = trim_stack(__STACKTRACE__, m, f, a)
              {ref, :error, reason, erpcStack}
          end
      end

    exit(reply)
  end

  def execute_call(m, f, a) do
    {:return, apply(m, f, a)}
  end

  def execute_cast(m, f, a) do
    try do
      apply(m, f, a)
    catch
      :error, reason ->
        case is_arg_error(reason, m, f, a) do
          true ->
            :erlang.error({:erpc, reason})

          false ->
            erpcStack = trim_stack(__STACKTRACE__, m, f, a)
            :erlang.error({:exception, {reason, erpcStack}})
        end
    end
  end

  def call_result(type, reqId, res, reason) do
    result(type, reqId, res, reason)
  end

  def is_arg_error(:system_limit, _M, _F, a) do
    try do
      apply(:erpc, :nonexisting, a)
      false
    catch
      :error, :system_limit ->
        true

      _, _ ->
        false
    end
  end

  def is_arg_error(_R, _M, _F, _A) do
    false
  end

  def trim_stack([cF | _], m, f, a)
      when :erlang.element(
             1,
             cF
           ) == :erpc and
             (:erlang.element(
                2,
                cF
              ) == :execute_call or
                :erlang.element(
                  2,
                  cF
                ) == :execute_cast) do
    [{m, f, a, []}]
  end

  def trim_stack([[{m, f, a, _} = sF, cF] | _], m, f, a)
      when :erlang.element(
             1,
             cF
           ) == :erpc and
             (:erlang.element(
                2,
                cF
              ) == :execute_call or
                :erlang.element(
                  2,
                  cF
                ) == :execute_cast) do
    [sF]
  end

  def trim_stack(s, m, f, a) do
    try do
      trim_stack_aux(s, m, f, a)
    catch
      :use_all ->
        s
    end
  end

  defp trim_stack_aux([], _M, _F, _A) do
    throw(:use_all)
  end

  defp trim_stack_aux([[{m, f, aL, _} = sF, cF] | _], m, f, a)
       when :erlang.element(
              1,
              cF
            ) == :erpc and
              (:erlang.element(
                 2,
                 cF
               ) == :execute_call or
                 :erlang.element(
                   2,
                   cF
                 ) == :execute_cast) and
              aL == length(a) do
    [sF]
  end

  defp trim_stack_aux([cF | _], m, f, a)
       when :erlang.element(
              1,
              cF
            ) == :erpc and
              (:erlang.element(
                 2,
                 cF
               ) == :execute_call or
                 :erlang.element(
                   2,
                   cF
                 ) == :execute_cast) do
    try do
      [{m, f, length(a), []}]
    catch
      _, _ ->
        []
    end
  end

  defp trim_stack_aux([sF | sFs], m, f, a) do
    [sF | trim_stack_aux(sFs, m, f, a)]
  end

  defp call_abandon(reqId) do
    case :erlang.spawn_request_abandon(reqId) do
      true ->
        true

      false ->
        :erlang.demonitor(reqId, [:info])
    end
  end

  defp result(:down, _ReqId, res, {res, :return, return}) do
    return
  end

  defp result(:down, _ReqId, res, {res, :throw, throw}) do
    throw(throw)
  end

  defp result(:down, _ReqId, res, {res, :exit, exit}) do
    exit({:exception, exit})
  end

  defp result(:down, _ReqId, res, {res, :error, error, stack}) do
    :erlang.error({:exception, error, stack})
  end

  defp result(:down, _ReqId, res, {res, :error, {:erpc, _} = erpcErr}) do
    :erlang.error(erpcErr)
  end

  defp result(:down, _ReqId, _Res, :noconnection) do
    :erlang.error({:erpc, :noconnection})
  end

  defp result(:down, _ReqId, _Res, reason) do
    exit({:signal, reason})
  end

  defp result(:spawn_reply, _ReqId, _Res, reason) do
    :erlang.error({:erpc, reason})
  end

  defp result(:timeout, reqId, res, _Reason) do
    case call_abandon(reqId) do
      true ->
        :erlang.error({:erpc, :timeout})

      false ->
        receive do
          {:spawn_reply, ^reqId, :error, reason} ->
            result(:spawn_reply, reqId, res, reason)

          {:DOWN, ^reqId, :process, _Pid, reason} ->
            result(:down, reqId, res, reason)
        after
          0 ->
            :erlang.error({:erpc, :badarg})
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

  defp mcall_send_requests([], _M, _F, _A, rIDs, _T, lC) do
    {rIDs, lC}
  end

  defp mcall_send_requests([n | ns], m, f, a, rIDs, :infinity, false)
       when n == node() do
    mcall_send_requests(ns, m, f, a, [:local_call | rIDs], :infinity, true)
  end

  defp mcall_send_requests([n | ns], m, f, a, rIDs, t, lC) do
    rID =
      try do
        send_request(n, m, f, a)
      catch
        _, _ ->
          mcall_failure_abandon(rIDs)
      end

    mcall_send_requests(ns, m, f, a, [rID | rIDs], t, lC)
  end

  defp mcall_send_requests(_, _, _, _, rIDs, _T, _LC) do
    mcall_failure_abandon(rIDs)
  end

  defp mcall_failure_abandon([]) do
    :erlang.error(:badarg)
  end

  defp mcall_failure_abandon([:local_call | rIDs]) do
    mcall_failure_abandon(rIDs)
  end

  defp mcall_failure_abandon([rID | rIDs]) do
    try do
      _ = receive_response(rID, 0)
      :ok
    catch
      _, _ ->
        :ok
    end

    mcall_failure_abandon(rIDs)
  end

  defp mcall_receive_replies([], replies, :undefined, _Deadline) do
    replies
  end

  defp mcall_receive_replies([:local_call | rIDs], replies, lRes, deadline) do
    mcall_receive_replies(rIDs, [lRes | replies], :undefined, deadline)
  end

  defp mcall_receive_replies([rID | rIDs], replies, lRes, deadline) do
    reply =
      try do
        {:ok, receive_response(rID, time_left(deadline))}
      catch
        class, reason ->
          {class, reason}
      end

    mcall_receive_replies(rIDs, [reply | replies], lRes, deadline)
  end
end
