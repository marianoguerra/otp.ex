defmodule :m_logger_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :logger_sup}, :logger_sup, [])
  end

  def init([]) do
    supFlags = %{strategy: :one_for_one, intensity: 1, period: 5}

    watcher = %{
      id: :logger_handler_watcher,
      start: {:logger_handler_watcher, :start_link, []},
      shutdown: :brutal_kill
    }

    proxy = :logger_proxy.child_spec()
    {:ok, {supFlags, [watcher, proxy]}}
  end
end
