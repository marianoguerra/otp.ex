defmodule :m_dets_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :dets_sup}, :dets_sup, [])
  end

  def init([]) do
    supFlags = {:simple_one_for_one, 4, 3600}
    child = {:dets, {:dets, :istart_link, []}, :temporary, 30000, :worker, [:dets]}
    {:ok, {supFlags, [child]}}
  end
end
