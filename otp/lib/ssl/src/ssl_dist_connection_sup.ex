defmodule :m_ssl_dist_connection_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :ssl_dist_connection_sup}, :ssl_dist_connection_sup, [])
  end

  def init([]) do
    tLSConnetionManager = tls_connection_manager_child_spec()
    listenOptionsTracker = listen_options_tracker_child_spec()
    pre_1_3SessionTracker = ssl_server_session_child_spec()

    {:ok,
     {{:one_for_one, 10, 3600},
      [tLSConnetionManager, listenOptionsTracker, pre_1_3SessionTracker]}}
  end

  defp tls_connection_manager_child_spec() do
    name = :dist_tls_connection
    startFunc = {:tls_connection_sup, :start_link_dist, []}
    restart = :permanent
    shutdown = 4000
    modules = [:tls_connection_sup]
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end

  defp listen_options_tracker_child_spec() do
    name = :dist_tls_socket
    startFunc = {:ssl_listen_tracker_sup, :start_link_dist, []}
    restart = :permanent
    shutdown = 4000
    modules = [:tls_socket]
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end

  defp ssl_server_session_child_spec() do
    name = :dist_ssl_server_session_cache_sup
    startFunc = {:ssl_server_session_cache_sup, :start_link_dist, []}
    restart = :permanent
    shutdown = 4000
    modules = [:ssl_server_session_cache_sup]
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end
end
