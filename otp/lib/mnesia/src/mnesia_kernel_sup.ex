defmodule :m_mnesia_kernel_sup do
  use Bitwise
  @behaviour :supervisor
  def start() do
    :supervisor.start_link({:local, :mnesia_kernel_sup}, :mnesia_kernel_sup, [])
  end

  def init([]) do
    procLib = [:mnesia_monitor, :proc_lib]
    flags = {:one_for_all, 0, :timer.hours(24)}

    workers = [
      worker_spec(:mnesia_monitor, :timer.seconds(3), [:gen_server]),
      worker_spec(:mnesia_subscr, :timer.seconds(3), [:gen_server]),
      worker_spec(:mnesia_locker, :timer.seconds(3), procLib),
      worker_spec(:mnesia_recover, :timer.minutes(3), [:gen_server]),
      worker_spec(:mnesia_tm, :timer.seconds(30), procLib),
      worker_spec(:mnesia_rpc, :timer.seconds(3), [:gen_server]),
      supervisor_spec(:mnesia_checkpoint_sup),
      worker_spec(
        :mnesia_controller,
        :timer.seconds(3),
        [:gen_server]
      ),
      worker_spec(
        :mnesia_late_loader,
        :timer.seconds(3),
        procLib
      )
    ]

    {:ok, {flags, workers}}
  end

  defp worker_spec(name, killAfter, modules) do
    kA = supervisor_timeout(killAfter)
    {name, {name, :start, []}, :permanent, kA, :worker, [name] ++ modules}
  end

  defp supervisor_spec(name) do
    {name, {name, :start, []}, :permanent, :infinity, :supervisor, [name, :supervisor]}
  end

  def supervisor_timeout(killAfter) do
    killAfter
  end
end
