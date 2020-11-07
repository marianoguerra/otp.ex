defmodule :m_tls_server_session_ticket_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, tracker_name(:normal)}, :tls_server_session_ticket_sup, [])
  end

  def start_link_dist() do
    :supervisor.start_link({:local, tracker_name(:dist)}, :tls_server_session_ticket_sup, [])
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
    startFunc = {:tls_server_session_ticket, :start_link, []}
    restart = :temporary
    shutdown = 4000
    modules = [:tls_server_session_ticket]
    type = :worker
    childSpec = {name, startFunc, restart, shutdown, type, modules}
    {:ok, {{restartStrategy, maxR, maxT}, [childSpec]}}
  end

  defp tracker_name(:normal) do
    :tls_server_session_ticket_sup
  end

  defp tracker_name(:dist) do
    :erlang.list_to_atom(:erlang.atom_to_list(:tls_server_session_ticket_sup) ++ 'dist')
  end
end
