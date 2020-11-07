defmodule :m_application_master do
  use Bitwise
  require Record

  Record.defrecord(:r_appl_data, :appl_data,
    name: :undefined,
    regs: [],
    phases: :undefined,
    mod: :undefined,
    mods: [],
    maxP: :infinity,
    maxT: :infinity
  )

  Record.defrecord(:r_state, :state,
    child: :undefined,
    appl_data: :undefined,
    children: [],
    procs: 0,
    gleader: :undefined,
    req: []
  )

  def start_link(applData, type) do
    parent = :erlang.whereis(:application_controller)
    :proc_lib.start_link(:application_master, :init, [parent, self(), applData, type])
  end

  def start_type() do
    send(:erlang.group_leader(), {:start_type, self()})

    receive do
      {:start_type, type} ->
        type
    after
      5000 ->
        {:error, :timeout}
    end
  end

  def stop(appMaster) do
    call(appMaster, :stop)
  end

  def get_child(appMaster) do
    call(appMaster, :get_child)
  end

  defp call(appMaster, req) do
    tag = make_ref()
    ref = :erlang.monitor(:process, appMaster)
    send(appMaster, {req, tag, self()})

    receive do
      {:DOWN, ^ref, :process, _, _Info} ->
        :ok

      {^tag, res} ->
        :erlang.demonitor(ref, [:flush])
        res
    end
  end

  def init(parent, starter, applData, type) do
    :erlang.link(parent)
    :erlang.process_flag(:trap_exit, true)
    oldGleader = :erlang.group_leader()
    :erlang.group_leader(self(), self())
    name = r_appl_data(applData, :name)

    :ets.insert(
      :ac_tab,
      {{:application_master, name}, self()}
    )

    state = r_state(appl_data: applData, gleader: oldGleader)

    case start_it(state, type) do
      {:ok, pid} ->
        :ok = set_timer(r_appl_data(applData, :maxT))
        :erlang.unlink(starter)
        :proc_lib.init_ack(starter, {:ok, self()})
        main_loop(parent, r_state(state, child: pid))

      {:error, reason} ->
        exit(reason)

      else__ ->
        exit(else__)
    end
  end

  defp start_it(state, type) do
    tag = make_ref()
    pid = spawn_link(:application_master, :start_it, [tag, state, self(), type])
    init_loop(pid, tag, state, type)
  end

  defp init_loop(pid, tag, state, type) do
    receive do
      ioReq when :erlang.element(1, ioReq) === :io_request ->
        send(r_state(state, :gleader), ioReq)
        init_loop(pid, tag, state, type)

      {^tag, res} ->
        res

      {:EXIT, ^pid, reason} ->
        {:error, reason}

      {:start_type, from} ->
        send(from, {:start_type, type})
        init_loop(pid, tag, state, type)

      other ->
        newState = handle_msg(other, state)
        init_loop(pid, tag, newState, type)
    end
  end

  defp main_loop(parent, state) do
    receive do
      ioReq when :erlang.element(1, ioReq) === :io_request ->
        send(r_state(state, :gleader), ioReq)
        main_loop(parent, state)

      {:EXIT, ^parent, reason} ->
        terminate(reason, state)

      {:EXIT, child, reason} when r_state(state, :child) === child ->
        terminate(reason, r_state(state, child: :undefined))

      {:EXIT, _, :timeout} ->
        terminate(:normal, state)

      {:EXIT, pid, _Reason} ->
        children = :lists.delete(pid, r_state(state, :children))
        procs = r_state(state, :procs) - 1

        main_loop(
          parent,
          r_state(state, children: children, procs: procs)
        )

      {:start_type, from} ->
        send(from, {:start_type, :local})
        main_loop(parent, state)

      other ->
        newState = handle_msg(other, state)
        main_loop(parent, newState)
    end
  end

  defp terminate_loop(child, state) do
    receive do
      ioReq when :erlang.element(1, ioReq) === :io_request ->
        send(r_state(state, :gleader), ioReq)
        terminate_loop(child, state)

      {:EXIT, ^child, _} ->
        :ok

      other ->
        newState = handle_msg(other, state)
        terminate_loop(child, newState)
    end
  end

  defp handle_msg({:get_child, tag, from}, state) do
    get_child_i(state, tag, from)
  end

  defp handle_msg({:stop, tag, from}, state) do
    try do
      terminate(:normal, state)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    send(from, {tag, :ok})
    exit(:normal)
  end

  defp handle_msg(
         {:child, ref, grandChild, mod},
         r_state(req: reqs0) = state
       ) do
    {:value, {_, tag, from}, reqs} = :lists.keytake(ref, 1, reqs0)
    send(from, {tag, {grandChild, mod}})
    r_state(state, req: reqs)
  end

  defp handle_msg(_, state) do
    state
  end

  defp terminate(
         reason,
         state = r_state(child: child, children: children, req: reqs)
       ) do
    _ =
      for {_, tag, from} <- reqs do
        send(from, {tag, :error})
      end

    terminate_child(child, state)
    kill_children(children)
    exit(reason)
  end

  def start_it(tag, state, from, type) do
    :erlang.process_flag(:trap_exit, true)
    applData = r_state(state, :appl_data)

    case {r_appl_data(applData, :phases), r_appl_data(applData, :mod)} do
      {:undefined, _} ->
        start_it_old(tag, from, type, applData)

      {phases, {:application_starter, [m, a]}} ->
        start_it_new(tag, from, type, m, a, phases, [r_appl_data(applData, :name)])

      {phases, {m, a}} ->
        start_it_new(tag, from, type, m, a, phases, [r_appl_data(applData, :name)])

      {otherP, otherM} ->
        send(from, {tag, {:error, {:bad_keys, {{:mod, otherM}, {:start_phases, otherP}}}}})
    end
  end

  defp start_it_old(tag, from, type, applData) do
    {m, a} = r_appl_data(applData, :mod)

    case (try do
            m.start(type, a)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, pid} ->
        :erlang.link(pid)
        send(from, {tag, {:ok, self()}})
        loop_it(from, pid, m, [])

      {:ok, pid, appState} ->
        :erlang.link(pid)
        send(from, {tag, {:ok, self()}})
        loop_it(from, pid, m, appState)

      {:EXIT, :normal} ->
        send(from, {tag, {:error, {{:EXIT, :normal}, {m, :start, [type, a]}}}})

      {:error, reason} ->
        send(from, {tag, {:error, {reason, {m, :start, [type, a]}}}})

      other ->
        send(from, {tag, {:error, {:bad_return, {{m, :start, [type, a]}, other}}}})
    end
  end

  defp start_it_new(tag, from, type, m, a, phases, apps) do
    case (try do
            start_the_app(type, m, a, phases, apps)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, pid, appState} ->
        send(from, {tag, {:ok, self()}})
        loop_it(from, pid, m, appState)

      error ->
        send(from, {tag, error})
    end
  end

  defp start_the_app(type, m, a, phases, apps) do
    case start_supervisor(type, m, a) do
      {:ok, pid, appState} ->
        :erlang.link(pid)

        case :application_starter.start(phases, type, apps) do
          :ok ->
            {:ok, pid, appState}

          error2 ->
            :erlang.unlink(pid)
            error2
        end

      error ->
        error
    end
  end

  defp start_supervisor(type, m, a) do
    case (try do
            m.start(type, a)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, pid} ->
        {:ok, pid, []}

      {:ok, pid, appState} ->
        {:ok, pid, appState}

      {:error, reason} ->
        {:error, {reason, {m, :start, [type, a]}}}

      {:EXIT, :normal} ->
        {:error, {{:EXIT, :normal}, {m, :start, [type, a]}}}

      other ->
        {:error, {:bad_return, {{m, :start, [type, a]}, other}}}
    end
  end

  defp loop_it(parent, child, mod, appState) do
    receive do
      {^parent, :get_child, ref} ->
        send(parent, {:child, ref, child, mod})
        loop_it(parent, child, mod, appState)

      {^parent, :terminate} ->
        newAppState = prep_stop(mod, appState)
        :erlang.exit(child, :shutdown)

        receive do
          {:EXIT, ^child, _} ->
            :ok
        end

        try do
          mod.stop(newAppState)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        exit(:normal)

      {:EXIT, ^parent, reason} ->
        newAppState = prep_stop(mod, appState)
        :erlang.exit(child, reason)

        receive do
          {:EXIT, ^child, _} ->
            :ok
        end

        try do
          mod.stop(newAppState)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        exit(reason)

      {:EXIT, ^child, reason} ->
        newAppState = prep_stop(mod, appState)

        try do
          mod.stop(newAppState)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        exit(reason)

      _ ->
        loop_it(parent, child, mod, appState)
    end
  end

  defp prep_stop(mod, appState) do
    case (try do
            mod.prep_stop(appState)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, {:undef, _}} ->
        appState

      {:EXIT, reason} ->
        :error_logger.error_report([
          {:application_master, :shutdown_error},
          {mod, {:prep_stop, [appState]}},
          {:error_info, reason}
        ])

        appState

      newAppState ->
        newAppState
    end
  end

  defp get_child_i(r_state(child: child, req: reqs) = state, tag, from) do
    ref = :erlang.make_ref()

    case :erlang.is_process_alive(child) do
      true ->
        send(child, {self(), :get_child, ref})
        r_state(state, req: [{ref, tag, from} | reqs])

      false ->
        send(from, {tag, :error})
        state
    end
  end

  defp terminate_child_i(child, state) do
    send(child, {self(), :terminate})
    terminate_loop(child, state)
  end

  defp terminate_child(:undefined, _) do
    :ok
  end

  defp terminate_child(child, state) do
    terminate_child_i(child, state)
  end

  defp kill_children(children) do
    :lists.foreach(
      fn pid ->
        :erlang.exit(pid, :kill)
      end,
      children
    )

    kill_all_procs()
  end

  defp kill_all_procs() do
    kill_all_procs_1(:erlang.processes(), self(), 0)
  end

  defp kill_all_procs_1([self | ps], self, n) do
    kill_all_procs_1(ps, self, n)
  end

  defp kill_all_procs_1([p | ps], self, n) do
    case :erlang.process_info(p, :group_leader) do
      {:group_leader, ^self} ->
        :erlang.exit(p, :kill)
        kill_all_procs_1(ps, self, n + 1)

      _ ->
        kill_all_procs_1(ps, self, n)
    end
  end

  defp kill_all_procs_1([], _, 0) do
    :ok
  end

  defp kill_all_procs_1([], _, _) do
    kill_all_procs()
  end

  defp set_timer(:infinity) do
    :ok
  end

  defp set_timer(time) do
    {:ok, _} = :timer.exit_after(time, :timeout)
    :ok
  end
end
