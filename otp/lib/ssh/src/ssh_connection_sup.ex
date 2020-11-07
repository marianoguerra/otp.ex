defmodule :m_ssh_connection_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link(args) do
    :supervisor.start_link(:ssh_connection_sup, [args])
  end

  def start_child(sup, args) do
    :supervisor.start_child(sup, args)
  end

  def init(_) do
    supFlags = %{:strategy => :simple_one_for_one, :intensity => 0, :period => 3600}

    childSpecs = [
      %{
        :id => :undefined,
        :start => {:ssh_connection_handler, :start_link, []},
        :restart => :temporary
      }
    ]

    {:ok, {supFlags, childSpecs}}
  end
end
