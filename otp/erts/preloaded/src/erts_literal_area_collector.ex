defmodule :m_erts_literal_area_collector do
  use Bitwise

  def start() do
    :erlang.process_flag(:trap_exit, true)
    msg_loop(:undefined, 0, 0, [])
  end

  defp msg_loop(area, outstnd, gcOutstnd, needGC) do
    receive do
      :copy_literals when outstnd == 0 ->
        switch_area()

      {:copy_literals, {^area, _GcAllowed, _Pid}, :ok}
      when outstnd == 1 ->
        switch_area()

      {:copy_literals, {^area, false, _Pid}, :ok} ->
        msg_loop(area, outstnd - 1, gcOutstnd, needGC)

      {:copy_literals, {^area, true, _Pid}, :ok}
      when needGC == [] ->
        msg_loop(area, outstnd - 1, gcOutstnd - 1, [])

      {:copy_literals, {^area, true, _Pid}, :ok} ->
        send_copy_req(hd(needGC), area, true)
        msg_loop(area, outstnd - 1, gcOutstnd, tl(needGC))

      {:copy_literals, {^area, false, pid}, :need_gc}
      when gcOutstnd < 2 ->
        send_copy_req(pid, area, true)
        msg_loop(area, outstnd, gcOutstnd + 1, needGC)

      {:copy_literals, {^area, false, pid}, :need_gc} ->
        msg_loop(area, outstnd, gcOutstnd, [pid | needGC])

      {:copy_literals, {^area, _, _}, _} = msg
      when :erlang.is_reference(area) ->
        exit({:not_handled_message, msg})

      _Ignore ->
        msg_loop(area, outstnd, gcOutstnd, needGC)
    end
  end

  defp switch_area() do
    res = :erts_internal.release_literal_area_switch()
    :erlang.garbage_collect()

    case res do
      false ->
        msg_loop(:undefined, 0, 0, [])

      true ->
        area = make_ref()
        outstnd = send_copy_reqs(:erlang.processes(), area, false)
        msg_loop(area, outstnd, 0, [])
    end
  end

  defp send_copy_reqs(ps, area, gC) do
    send_copy_reqs(ps, area, gC, 0)
  end

  defp send_copy_reqs([], _Area, _GC, n) do
    n
  end

  defp send_copy_reqs([p | ps], area, gC, n) do
    send_copy_req(p, area, gC)
    send_copy_reqs(ps, area, gC, n + 1)
  end

  defp send_copy_req(p, area, gC) do
    :erts_internal.request_system_task(p, :normal, {:copy_literals, {area, gC, p}, gC})
  end
end
