defmodule :m_tls_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :tls_sup}, :tls_sup, [])
  end

  def init([]) do
    tLSConnetionManager = tls_connection_manager_child_spec()
    serverInstanceSup = server_instance_child_spec()
    {:ok, {{:one_for_one, 10, 3600}, [tLSConnetionManager, serverInstanceSup]}}
  end

  defp tls_connection_manager_child_spec() do
    name = :tls_connection
    startFunc = {:tls_connection_sup, :start_link, []}
    restart = :permanent
    shutdown = 4000
    modules = [:tls_connection_sup]
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end

  defp server_instance_child_spec() do
    name = :tls_server_sup
    startFunc = {:tls_server_sup, :start_link, []}
    restart = :permanent
    shutdown = 4000
    modules = [:tls_server_sup]
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end
end
