defmodule :m_ct_default_gl do
  use Bitwise

  def start_link(parentGL) do
    do_start(parentGL, 3)
  end

  defp do_start(_ParentGL, 0) do
    exit({:ct_default_gl, :startup})
  end

  defp do_start(parentGL, retries) do
    case :erlang.whereis(:ct_default_gl) do
      :undefined ->
        case :gen_server.start_link(:ct_default_gl, [parentGL], []) do
          {:ok, pid} ->
            {:ok, pid}

          other ->
            other
        end

      pid ->
        :erlang.exit(pid, :kill)
        :timer.sleep(1000)
        do_start(parentGL, retries - 1)
    end
  end

  def stop() do
    :gen_server.cast(:erlang.whereis(:ct_default_gl), :stop)
  end

  def init([parentGL]) do
    :erlang.register(:ct_default_gl, self())
    :ct_util.mark_process()
    {:ok, %{parent_gl_pid: parentGL, parent_gl_monitor: :erlang.monitor(:process, parentGL)}}
  end

  def handle_cast(:stop, st) do
    {:stop, :normal, st}
  end

  def handle_info(
        {:DOWN, ref, :process, _, _Reason},
        %{parent_gl_monitor: ref} = st
      ) do
    user = :erlang.whereis(:user)

    {:noreply,
     Map.merge(st, %{parent_gl_pid: user, parent_gl_monitor: :erlang.monitor(:process, user)})}
  end

  def handle_info(
        {:io_request, _From, _ReplyAs, _Req} = ioReq,
        %{parent_gl_pid: parentGL} = st
      ) do
    send(parentGL, ioReq)
    {:noreply, st}
  end

  def handle_info(msg, st) do
    :io.format(:user, 'Common Test Group Leader process got: ~tp~n', [msg])
    {:noreply, st}
  end

  def handle_call(_Req, _From, st) do
    {:reply, :ok, st}
  end

  def terminate(_, _) do
    :ok
  end
end
