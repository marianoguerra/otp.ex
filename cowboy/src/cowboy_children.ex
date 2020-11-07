defmodule :cowboy_children do
  use Bitwise
  require Record

  Record.defrecord(:r_child, :child,
    pid: :undefined,
    streamid: :undefined,
    shutdown: :undefined,
    timer: :undefined
  )

  def init() do
    []
  end

  def up(children, pid, streamID, shutdown) do
    [
      r_child(pid: pid, streamid: streamID, shutdown: shutdown)
      | children
    ]
  end

  def down(children0, pid) do
    case :lists.keytake(pid, r_child(:pid), children0) do
      {:value, r_child(streamid: streamID, timer: ref), children} ->
        _ =
          case ref do
            :undefined ->
              :ok

            _ ->
              :erlang.cancel_timer(
                ref,
                [{:async, true}, {:info, false}]
              )
          end

        {:ok, streamID, children}

      false ->
        :error
    end
  end

  def shutdown(children0, streamID) do
    for child <- children0 do
      case child do
        r_child(pid: pid, streamid: ^streamID, shutdown: shutdown) ->
          :erlang.exit(pid, :shutdown)
          ref = :erlang.start_timer(shutdown, self(), {:shutdown, pid})
          r_child(child, streamid: :undefined, timer: ref)

        _ ->
          child
      end
    end
  end

  def shutdown_timeout(children, ref, pid) do
    case :lists.keyfind(pid, r_child(:pid), children) do
      r_child(timer: ^ref) ->
        :erlang.exit(pid, :kill)
        :ok

      _ ->
        :ok
    end
  end

  def terminate(children) do
    _ =
      for r_child(pid: pid, timer: tRef) <- children do
        case tRef do
          :undefined ->
            :erlang.exit(pid, :shutdown)

          _ ->
            :erlang.cancel_timer(
              tRef,
              [{:async, true}, {:info, false}]
            )
        end
      end

    before_terminate_loop(children)
  end

  defp before_terminate_loop([]) do
    :ok
  end

  defp before_terminate_loop(children) do
    time = longest_shutdown_time(children, 0)

    tRef =
      case time do
        :infinity ->
          :undefined

        _ ->
          :erlang.start_timer(time, self(), :terminate)
      end

    terminate_loop(children, tRef)
  end

  defp terminate_loop([], tRef) do
    case tRef do
      :undefined ->
        :ok

      _ ->
        _ =
          :erlang.cancel_timer(
            tRef,
            [{:async, true}, {:info, false}]
          )

        :ok
    end
  end

  defp terminate_loop(children, tRef) do
    receive do
      {:EXIT, pid, _} when tRef === :undefined ->
        {:value, r_child(shutdown: shutdown), children1} =
          :lists.keytake(pid, r_child(:pid), children)

        case shutdown do
          :infinity ->
            before_terminate_loop(children1)

          _ ->
            terminate_loop(children1, tRef)
        end

      {:EXIT, pid, _} ->
        terminate_loop(
          :lists.keydelete(pid, r_child(:pid), children),
          tRef
        )

      {:timeout, ^tRef, :terminate} ->
        _ =
          for r_child(pid: pid) <- children do
            :erlang.exit(pid, :kill)
          end

        :ok
    end
  end

  defp longest_shutdown_time([], time) do
    time
  end

  defp longest_shutdown_time([r_child(shutdown: childTime) | tail], time)
       when childTime > time do
    longest_shutdown_time(tail, childTime)
  end

  defp longest_shutdown_time([_ | tail], time) do
    longest_shutdown_time(tail, time)
  end

  def handle_supervisor_call(:which_children, {from, tag}, children, module) do
    send(from, {tag, which_children(children, module)})
    :ok
  end

  def handle_supervisor_call(:count_children, {from, tag}, children, _) do
    send(from, {tag, count_children(children)})
    :ok
  end

  def handle_supervisor_call({:start_child, _}, {from, tag}, _, _) do
    send(from, {tag, {:error, :start_child_disabled}})
    :ok
  end

  def handle_supervisor_call(_, {from, tag}, _, _) do
    send(from, {tag, {:error, :not_found}})
    :ok
  end

  defp which_children(children, module) do
    for r_child(pid: pid) <- children do
      {module, pid, :worker, [module]}
    end
  end

  defp count_children(children) do
    count = length(children)
    [{:specs, 1}, {:active, count}, {:supervisors, 0}, {:workers, count}]
  end
end
