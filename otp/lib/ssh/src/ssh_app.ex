defmodule :m_ssh_app do
  use Bitwise
  @behaviour :application
  def start(_Type, _State) do
    :supervisor.start_link({:local, :ssh_sup}, :ssh_sup, [])
  end

  def stop(_State) do
    :ok
  end
end
