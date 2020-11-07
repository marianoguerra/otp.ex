defmodule :m_ssl_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :ssl_sup}, :ssl_sup, [])
  end

  def init([]) do
    {:ok, {{:rest_for_one, 10, 3600}, [ssl_admin_child_spec(), ssl_connection_sup()]}}
  end

  defp ssl_admin_child_spec() do
    name = :ssl_admin_sup
    startFunc = {:ssl_admin_sup, :start_link, []}
    restart = :permanent
    shutdown = 4000
    modules = [:ssl_admin_sup]
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end

  defp ssl_connection_sup() do
    name = :ssl_connection_sup
    startFunc = {:ssl_connection_sup, :start_link, []}
    restart = :permanent
    shutdown = 4000
    modules = [:ssl_connection_sup]
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end
end
