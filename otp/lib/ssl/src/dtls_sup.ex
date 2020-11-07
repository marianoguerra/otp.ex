defmodule :m_dtls_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :dtls_sup}, :dtls_sup, [])
  end

  def init([]) do
    dTLSConnectionManager = dtls_connection_manager_child_spec()
    dTLSServers = dtls_server_spec()
    {:ok, {{:one_for_one, 10, 3600}, [dTLSConnectionManager, dTLSServers]}}
  end

  defp dtls_server_spec() do
    name = :dtls_servers
    startFunc = {:dtls_server_sup, :start_link, []}
    restart = :permanent
    shutdown = 4000
    modules = [:dtls_server_sup]
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end

  defp dtls_connection_manager_child_spec() do
    name = :dtls_connection
    startFunc = {:dtls_connection_sup, :start_link, []}
    restart = :permanent
    shutdown = 4000
    modules = [:dtls_connection_sup]
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end
end
