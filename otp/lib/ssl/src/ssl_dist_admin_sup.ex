defmodule :m_ssl_dist_admin_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :ssl_dist_admin_sup}, :ssl_dist_admin_sup, [])
  end

  def init([]) do
    pEMCache = pem_cache_child_spec()
    sessionCertManager = session_and_cert_manager_child_spec()
    {:ok, {{:rest_for_one, 10, 3600}, [pEMCache, sessionCertManager]}}
  end

  defp pem_cache_child_spec() do
    name = :ssl_pem_cache_dist
    startFunc = {:ssl_pem_cache, :start_link_dist, [[]]}
    restart = :permanent
    shutdown = 4000
    modules = [:ssl_pem_cache]
    type = :worker
    {name, startFunc, restart, shutdown, type, modules}
  end

  defp session_and_cert_manager_child_spec() do
    opts = :ssl_admin_sup.manager_opts()
    name = :ssl_dist_manager
    startFunc = {:ssl_manager, :start_link_dist, [opts]}
    restart = :permanent
    shutdown = 4000
    modules = [:ssl_manager]
    type = :worker
    {name, startFunc, restart, shutdown, type, modules}
  end
end
