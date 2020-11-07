defmodule :m_runtime_tools_sup do
  use Bitwise
  @behaviour :supervisor
  def init(_AutoModArgs) do
    flags = {:one_for_one, 0, 3600}

    children = [
      {:ttb_autostart, {:ttb_autostart, :start_link, []}, :temporary, 3000, :worker,
       [:ttb_autostart]}
    ]

    {:ok, {flags, children}}
  end
end
