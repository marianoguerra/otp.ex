defmodule :m_tls_connection_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :tls_connection_sup}, :tls_connection_sup, [])
  end

  def start_link_dist() do
    :supervisor.start_link({:local, :ssl_connection_sup_dist}, :tls_connection_sup, [])
  end

  def start_child(args) do
    :supervisor.start_child(:tls_connection_sup, args)
  end

  def start_child_dist(args) do
    :supervisor.start_child(:ssl_connection_sup_dist, args)
  end

  def init(_O) do
    restartStrategy = :simple_one_for_one
    maxR = 0
    maxT = 3600
    name = :undefined
    startFunc = {:tls_connection, :start_link, []}
    restart = :temporary
    shutdown = 4000
    modules = [:tls_connection, :ssl_connection]
    type = :worker
    childSpec = {name, startFunc, restart, shutdown, type, modules}
    {:ok, {{restartStrategy, maxR, maxT}, [childSpec]}}
  end
end
