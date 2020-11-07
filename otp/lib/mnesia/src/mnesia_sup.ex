defmodule :m_mnesia_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link(args) do
    :supervisor.start_link({:local, :mnesia_sup}, :mnesia_sup, [args])
  end

  def init([[]]) do
    init()
  end

  def init(badArg) do
    {:error, {:badarg, badArg}}
  end

  defp init() do
    flags = {:one_for_all, 0, 3600}
    event = event_procs()
    ext = ext_procs()
    kernel = kernel_procs()
    {:ok, {flags, event ++ ext ++ kernel}}
  end

  defp event_procs() do
    killAfter = :timer.seconds(30)
    kA = :mnesia_kernel_sup.supervisor_timeout(killAfter)
    e = :mnesia_event
    [{e, {:mnesia_sup, :start_event, []}, :permanent, kA, :worker, [e, :gen_event]}]
  end

  defp kernel_procs() do
    k = :mnesia_kernel_sup
    kA = :infinity
    [{k, {k, :start, []}, :permanent, kA, :supervisor, [k, :supervisor]}]
  end

  defp ext_procs() do
    k = :mnesia_ext_sup
    kA = :infinity
    [{k, {k, :start, []}, :permanent, kA, :supervisor, [k, :supervisor]}]
  end

  def start_event() do
    case :gen_event.start_link({:local, :mnesia_event}) do
      {:ok, pid} ->
        case add_event_handler() do
          :ok ->
            {:ok, pid}

          error ->
            error
        end

      error ->
        error
    end
  end

  defp add_event_handler() do
    handler = :mnesia_monitor.get_env(:event_module)
    :gen_event.add_handler(:mnesia_event, handler, [])
  end

  def kill() do
    mnesia = [:mnesia_fallback | :mnesia.ms()]

    kill = fn name ->
      try do
        :erlang.exit(:erlang.whereis(name), :kill)
      catch
        _, _ ->
          :ok
      end
    end

    :lists.foreach(kill, mnesia)
    :lists.foreach(&ensure_dead/1, mnesia)
    :timer.sleep(10)

    case :lists.keymember(:mnesia, 1, :application.which_applications()) do
      true ->
        kill()

      false ->
        :ok
    end
  end

  defp ensure_dead(name) do
    case :erlang.whereis(name) do
      :undefined ->
        :ok

      pid when is_pid(pid) ->
        :erlang.exit(pid, :kill)
        :timer.sleep(10)
        ensure_dead(name)
    end
  end
end
