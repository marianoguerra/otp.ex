defmodule :m_disk_log_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :disk_log_sup}, :disk_log_sup, [])
  end

  def init([]) do
    supFlags = {:simple_one_for_one, 4, 3600}
    child = {:disk_log, {:disk_log, :istart_link, []}, :temporary, 1000, :worker, [:disk_log]}
    {:ok, {supFlags, [child]}}
  end
end
