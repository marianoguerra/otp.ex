defmodule :m_ssh_sup do
  use Bitwise
  @behaviour :supervisor
  def init(_) do
    supFlags = %{:strategy => :one_for_one, :intensity => 10, :period => 3600}

    childSpecs = [
      %{:id => :sshd_sup, :start => {:sshd_sup, :start_link, []}, :type => :supervisor},
      %{:id => :sshc_sup, :start => {:sshc_sup, :start_link, []}, :type => :supervisor}
    ]

    {:ok, {supFlags, childSpecs}}
  end
end
