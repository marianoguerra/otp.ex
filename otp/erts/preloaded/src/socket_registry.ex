defmodule :m_socket_registry do
  use Bitwise
  require Record

  Record.defrecord(:r_esock_info, :esock_info,
    sock: :undefined,
    atime: :undefined
  )

  def start() do
    :erlang.register(:socket_registry, self())
    :erlang.process_flag(:trap_exit, true)
    loop([])
  end

  def number_of() do
    await_reply(request(:number_of))
  end

  def which_sockets(filter) when is_function(filter, 1) do
    await_reply(request({:which_sockets, filter}))
  end

  defp loop(dB) do
    receive do
      {:"$socket", :add, socket} ->
        loop(handle_add_socket(dB, socket))

      {:"$socket", :del, socket} ->
        loop(handle_delete_socket(dB, socket))

      {:socket_registry, :request, from, reqId, req} = _REQ ->
        {newDB, reply} = handle_request(dB, req)
        reply(reqId, from, reply)
        loop(newDB)

      msg ->
        newDB = handle_unexpected_msg(dB, msg)
        loop(newDB)
    end
  end

  defp handle_add_socket(dB, sock) do
    [r_esock_info(sock: sock, atime: timestamp()) | dB]
  end

  defp handle_delete_socket(dB, sock) do
    :lists.keydelete(sock, r_esock_info(:sock), dB)
  end

  defp handle_request(dB, :number_of) do
    {dB, db_size(dB)}
  end

  defp handle_request(dB, {:which_sockets, filter}) do
    {dB, do_which_sockets(dB, filter)}
  end

  defp handle_request(dB, badRequest) do
    {dB, {:error, {:bad_request, badRequest}}}
  end

  defp handle_unexpected_msg(dB, {:EXIT, pid, reason}) do
    f = 'socket-registry received unexpected exit from ~p:~n   ~p'
    a = [pid, reason]
    handle_unexpected_msg(:warning, f, a)
    dB
  end

  defp handle_unexpected_msg(dB, x) do
    f = 'socket-registry received unexpected:~n   ~p'
    a = [x]
    handle_unexpected_msg(:warning, f, a)
    dB
  end

  defp handle_unexpected_msg(:warning, f, a) do
    do_handle_unexpected_msg(
      mk_unexpected_warning_msg(
        f,
        a
      )
    )
  end

  defp do_handle_unexpected_msg(msg) do
    try do
      send(:logger, msg)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp mk_unexpected_warning_msg(f, a) do
    mk_unexpected_msg(:warning, :warning_msg, f, a)
  end

  defp mk_unexpected_msg(level, tag, f, a) do
    meta = %{
      :pid => self(),
      :gl => :erlang.group_leader(),
      :time => :os.system_time(:microsecond),
      :error_logger => %{:tag => tag}
    }

    {:log, level, f, a, meta}
  end

  defp db_size(dB) do
    length(dB)
  end

  defp do_which_sockets(dB, filter) do
    try do
      socksInfo =
        for r_esock_info(sock: sock) <- dB do
          {sock, :socket.info(sock)}
        end

      for {sock, sockInfo} <- socksInfo, filter.(sockInfo) do
        sock
      end
    catch
      _, _ ->
        for r_esock_info(sock: sock) <- dB do
          sock
        end
    end
  end

  defp request(req) do
    reqId = make_ref()
    reqMsg = {:socket_registry, :request, self(), reqId, req}
    registry = whoami()
    :erlang.send(registry, reqMsg)
    reqId
  end

  defp reply(reqId, from, reply) do
    repMsg = {:socket_registry, :reply, self(), reqId, reply}
    :erlang.send(from, repMsg)
  end

  defp await_reply(reqId) do
    registry = whoami()

    receive do
      {:socket_registry, :reply, ^registry, ^reqId, reply} ->
        reply
    end
  end

  defp whoami() do
    :erlang.whereis(:socket_registry)
  end

  defp timestamp() do
    :erlang.monotonic_time(:milli_seconds)
  end
end
