defmodule :m_tls_server_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :tls_server_sup}, :tls_server_sup, [])
  end

  def init([]) do
    listenTracker = listen_options_tracker_child_spec()
    sessionTracker = tls_server_session_child_spec()
    pre_1_3SessionTracker = ssl_server_session_child_spec()
    {:ok, {{:one_for_all, 10, 3600}, [listenTracker, sessionTracker, pre_1_3SessionTracker]}}
  end

  defp listen_options_tracker_child_spec() do
    name = :tls_socket
    startFunc = {:ssl_listen_tracker_sup, :start_link, []}
    restart = :permanent
    shutdown = 4000
    modules = [:ssl_listen_tracker_sup]
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end

  defp tls_server_session_child_spec() do
    name = :tls_server_session_ticket
    startFunc = {:tls_server_session_ticket_sup, :start_link, []}
    restart = :permanent
    shutdown = 4000
    modules = [:tls_server_session_ticket_sup]
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end

  defp ssl_server_session_child_spec() do
    name = :ssl_server_session_cache_sup
    startFunc = {:ssl_server_session_cache_sup, :start_link, []}
    restart = :permanent
    shutdown = 4000
    modules = [:ssl_server_session_cache_sup]
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end
end
