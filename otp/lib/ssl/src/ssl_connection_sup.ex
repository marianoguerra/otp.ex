defmodule :m_ssl_connection_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :ssl_connection_sup}, :ssl_connection_sup, [])
  end

  def init([]) do
    tLSSup = tls_sup_child_spec()
    dTLSSup = dtls_sup_child_spec()
    {:ok, {{:one_for_one, 10, 3600}, [tLSSup, dTLSSup]}}
  end

  defp tls_sup_child_spec() do
    name = :tls_sup
    startFunc = {:tls_sup, :start_link, []}
    restart = :permanent
    shutdown = 4000
    modules = [:tls_sup]
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end

  defp dtls_sup_child_spec() do
    name = :dtls_sup
    startFunc = {:dtls_sup, :start_link, []}
    restart = :permanent
    shutdown = 4000
    modules = [:dtls_sup]
    type = :supervisor
    {name, startFunc, restart, shutdown, type, modules}
  end
end
