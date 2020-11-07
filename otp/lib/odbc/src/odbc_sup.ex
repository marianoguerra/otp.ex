defmodule :m_odbc_sup do
  use Bitwise
  @behaviour :supervisor
  def init([name]) do
    restartStrategy = :simple_one_for_one
    maxR = 0
    maxT = 3600
    startFunc = {:odbc, :start_link_sup, []}
    restart = :temporary
    shutdown = 7000
    modules = [:odbc]
    type = :worker
    childSpec = {name, startFunc, restart, shutdown, type, modules}
    {:ok, {{restartStrategy, maxR, maxT}, [childSpec]}}
  end
end
