defmodule :cowboy_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :cowboy_sup}, :cowboy_sup, [])
  end

  def init([]) do
    procs = [
      {:cowboy_clock, {:cowboy_clock, :start_link, []}, :permanent, 5000, :worker,
       [:cowboy_clock]}
    ]

    {:ok, {{:one_for_one, 10, 10}, procs}}
  end
end
