defmodule :m_mnesia_checkpoint_sup do
  use Bitwise
  @behaviour :supervisor
  def start() do
    :supervisor.start_link({:local, :mnesia_checkpoint_sup}, :mnesia_checkpoint_sup, [])
  end

  def init([]) do
    flags = {:simple_one_for_one, 0, :timer.hours(24)}
    mFA = {:mnesia_checkpoint, :start, []}
    modules = [:mnesia_checkpoint_sup, :mnesia_checkpoint, :supervisor]
    killAfter = :mnesia_kernel_sup.supervisor_timeout(:timer.seconds(3))
    workers = [{:mnesia_checkpoint_sup, mFA, :transient, killAfter, :worker, modules}]
    {:ok, {flags, workers}}
  end
end
