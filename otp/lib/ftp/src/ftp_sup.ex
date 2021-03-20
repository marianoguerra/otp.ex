defmodule :m_ftp_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :ftp_sup}, :ftp_sup, [])
  end

  def start_child(args) do
    :supervisor.start_child(:ftp_sup, args)
  end

  def init(_) do
    supFlags = %{strategy: :simple_one_for_one, intensity: 0, period: 3600}
    {:ok, {supFlags, child_specs()}}
  end

  defp child_specs() do
    [
      %{
        id: :undefined,
        start: {:ftp, :start_link, []},
        restart: :temporary,
        shutdown: 4000,
        type: :worker,
        modules: [:ftp]
      }
    ]
  end
end
