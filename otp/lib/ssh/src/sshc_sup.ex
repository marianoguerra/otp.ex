defmodule :m_sshc_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :sshc_sup}, :sshc_sup, [])
  end

  def start_child(address, port, profile, options) do
    spec = child_spec(address, port, profile, options)
    :supervisor.start_child(:sshc_sup, spec)
  end

  def stop_child(childId) when is_tuple(childId) do
    :supervisor.terminate_child(:sshc_sup, childId)
  end

  def stop_child(childPid) when is_pid(childPid) do
    stop_child(system_name(childPid))
  end

  def init(_) do
    supFlags = %{strategy: :one_for_one, intensity: 0, period: 3600}
    childSpecs = []
    {:ok, {supFlags, childSpecs}}
  end

  defp child_spec(address, port, profile, options) do
    %{
      id: id(address, port, profile),
      start: {:ssh_system_sup, :start_link, [:client, address, port, profile, options]},
      restart: :temporary,
      type: :supervisor
    }
  end

  defp id(address, port, profile) do
    {:client, :ssh_system_sup, address, port, profile}
  end

  defp system_name(sysSup) do
    case :lists.keyfind(sysSup, 2, :supervisor.which_children(:sshc_sup)) do
      {name, ^sysSup, _, _} ->
        name

      false ->
        :undefind
    end
  end
end
