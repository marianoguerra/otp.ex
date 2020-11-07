defmodule :m_observer do
  use Bitwise

  def start() do
    :observer_wx.start()
  end

  def stop() do
    :observer_wx.stop()
  end
end
