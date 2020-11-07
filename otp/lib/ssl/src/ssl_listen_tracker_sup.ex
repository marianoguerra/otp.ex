defmodule :m_ssl_listen_tracker_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, tracker_name(:normal)}, :ssl_listen_tracker_sup, [])
  end

  def start_link_dist() do
    :supervisor.start_link({:local, tracker_name(:dist)}, :ssl_listen_tracker_sup, [])
  end

  def start_child(args) do
    :supervisor.start_child(tracker_name(:normal), args)
  end

  def start_child_dist(args) do
    :supervisor.start_child(tracker_name(:dist), args)
  end

  def init(_O) do
    restartStrategy = :simple_one_for_one
    maxR = 0
    maxT = 3600
    name = :undefined
    startFunc = {:tls_socket, :start_link, []}
    restart = :temporary
    shutdown = 4000
    modules = [:tls_socket]
    type = :worker
    childSpec = {name, startFunc, restart, shutdown, type, modules}
    {:ok, {{restartStrategy, maxR, maxT}, [childSpec]}}
  end

  defp tracker_name(:normal) do
    :ssl_listen_tracker_sup
  end

  defp tracker_name(:dist) do
    :erlang.list_to_atom(:erlang.atom_to_list(:ssl_listen_tracker_sup) ++ 'dist')
  end
end
