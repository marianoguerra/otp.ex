defmodule :m_dbg_wx_break do
  use Bitwise

  def start(wx, pos, type) do
    start(wx, pos, type, '', '')
  end

  def start(wx, pos, type, mod) do
    start(wx, pos, type, mod, '')
  end

  def start(wx, pos, type, mod, line) do
    env = :wx.get_env()
    spawn_link(:dbg_wx_break, :init, [wx, env, pos, type, mod, line])
  end

  def init(wx, env, pos, type, mod, line) do
    :wx.set_env(env)

    win =
      :wx.batch(fn ->
        :dbg_wx_break_win.create_win(wx, pos, type, mod, line)
      end)

    cond do
      type == :function and is_atom(mod) ->
        win2 = gui_cmd({:module, mod}, win)
        loop(win2)

      true ->
        loop(win)
    end
  end

  defp loop(win) do
    receive do
      guiEvent
      when :erlang.element(1, guiEvent) == :gs or
             :erlang.element(1, guiEvent) == :wx ->
        cmd =
          :wx.batch(fn ->
            :dbg_wx_break_win.handle_event(guiEvent, win)
          end)

        win2 = gui_cmd(cmd, win)
        loop(win2)
    end
  end

  defp gui_cmd(:ignore, win) do
    win
  end

  defp gui_cmd(:stopped, _Win) do
    exit(:normal)
  end

  defp gui_cmd({:win, win2}, _Win) do
    win2
  end

  defp gui_cmd({:module, mod}, win) do
    funcs = :int.functions(mod)
    :dbg_wx_break_win.update_functions(win, funcs)
  end

  defp gui_cmd({:break, dataL, action}, _Win) do
    fun = fn data ->
      case data do
        [mod, line] ->
          :int.break(mod, line)
          :int.action_at_break(mod, line, action)

        [mod, line, cMod, cFunc] ->
          :int.break(mod, line)
          :int.test_at_break(mod, line, {cMod, cFunc})
          :int.action_at_break(mod, line, action)

        [mod, func, arity] ->
          :int.break_in(mod, func, arity)
      end
    end

    :lists.foreach(fun, dataL)
    exit(:normal)
  end
end
