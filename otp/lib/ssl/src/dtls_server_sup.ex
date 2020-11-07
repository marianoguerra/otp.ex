defmodule :m_dtls_server_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :dtls_server_sup}, :dtls_server_sup, [])
  end

  def init([]) do
    dTLSListeners = dtls_listeners_spec()
    pre_1_3SessionTracker = ssl_server_session_child_spec()
    {:ok, {{:one_for_all, 10, 3600}, [dTLSListeners, pre_1_3SessionTracker]}}
  end

  defp dtls_listeners_spec() do
    name = :dtls_listener
    startFunc = {:dtls_listener_sup, :start_link, []}
    restart = :permanent
    shutdown = 4000
    modules = []
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end

  defp ssl_server_session_child_spec() do
    name = :dtls_server_session_cache_sup
    startFunc = {:dtls_server_session_cache_sup, :start_link, []}
    restart = :permanent
    shutdown = 4000
    modules = [:dtls_server_session_cache_sup]
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end
end
