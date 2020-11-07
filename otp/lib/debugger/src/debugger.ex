defmodule :m_debugger do
  use Bitwise

  def start() do
    start(:global, :default, :default)
  end

  def start(mode) when mode == :local or mode == :global do
    start(mode, :default, :default)
  end

  def start(gui) when gui == :wx do
    start(:global, :default, gui)
  end

  def start(sFile)
      when is_list(sFile) and
             is_integer(hd(sFile)) do
    start(:global, sFile, :default)
  end

  def start(mode, sFile) do
    start(mode, sFile, :default)
  end

  defp start(mode, sFile, :wx) do
    :dbg_wx_mon.start(mode, sFile)
  end

  defp start(mode, sFile, :default) do
    gui = which_gui()
    start(mode, sFile, gui)
  end

  def stop() do
    :dbg_wx_mon.stop()
  end

  def quick(m, f, a) do
    :int.i(m)
    auto_attach([:init])
    apply(m, f, a)
  end

  def auto_attach(flags) do
    case which_gui() do
      :wx ->
        :int.auto_attach(flags, {:dbg_wx_trace, :start, []})
    end
  end

  defp which_gui() do
    :wx
  end
end
