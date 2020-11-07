defmodule :m_dbg_wx_view do
  use Bitwise
  require Record

  Record.defrecord(:r_state, :state,
    gs: :undefined,
    win: :undefined,
    coords: :undefined,
    mod: :undefined
  )

  def start(gS, mod) do
    title = 'View Module ' ++ :erlang.atom_to_list(mod)

    case :dbg_wx_winman.is_started(title) do
      true ->
        :ignore

      false ->
        env = :wx.get_env()

        spawn_link(fn ->
          init(gS, env, mod, title)
        end)
    end
  end

  defp stop() do
    exit(:normal)
  end

  defp init(gS, env, mod, title) do
    :wx.set_env(env)
    :int.subscribe()
    win1 = :dbg_wx_trace_win.create_win(gS, title, [:"Code Area", :"Search Area"], menus())
    window = :dbg_wx_trace_win.get_window(win1)
    :dbg_wx_winman.insert(title, window)
    win2 = gui_load_module(win1, mod)

    win3 =
      :lists.foldl(
        fn break, win ->
          :dbg_wx_trace_win.add_break(win, :Break, break)
        end,
        win2,
        :int.all_breaks(mod)
      )

    try do
      loop(r_state(gs: gS, win: win3, coords: {-1, -1}, mod: mod))
    catch
      _E, :normal ->
        exit(:normal)

      _E, _R ->
        :io.format('~p:~p ~p~n', [:dbg_wx_view, _E, _R])
        exit(_R)
    end
  end

  defp loop(state) do
    receive do
      guiEvent when :erlang.element(1, guiEvent) === :wx ->
        cmd =
          :wx.batch(fn ->
            :dbg_wx_trace_win.handle_event(
              guiEvent,
              r_state(state, :win)
            )
          end)

        state2 = gui_cmd(cmd, state)
        loop(state2)

      {:gui, cmd} ->
        state2 = gui_cmd(cmd, state)
        loop(state2)

      {:int, cmd} ->
        state2 = int_cmd(cmd, state)
        loop(state2)

      {:dbg_ui_winman, :update_windows_menu, data} ->
        window = :dbg_wx_trace_win.get_window(r_state(state, :win))
        :dbg_wx_winman.update_windows_menu(window, data)
        loop(state)

      {:dbg_ui_winman, :destroy} ->
        :dbg_wx_trace_win.stop(r_state(state, :win))
        exit(:stop)

      {:EXIT, _Pid, _Reason} ->
        loop(state)
    end
  end

  defp gui_cmd(:ignore, state) do
    state
  end

  defp gui_cmd({:win, win}, state) do
    r_state(state, win: win)
  end

  defp gui_cmd(:stopped, _State) do
    stop()
  end

  defp gui_cmd({:coords, coords}, state) do
    r_state(state, coords: coords)
  end

  defp gui_cmd({:shortcut, key}, state) do
    case shortcut(key) do
      false ->
        state

      cmd ->
        gui_cmd(cmd, state)
    end
  end

  defp gui_cmd(:Close, state) do
    :dbg_wx_trace_win.stop(r_state(state, :win))
    stop()
  end

  defp gui_cmd(:"Go To Line", state) do
    win =
      :dbg_wx_trace_win.helpwin(
        :gotoline,
        r_state(state, :win)
      )

    r_state(state, win: win)
  end

  defp gui_cmd({:gotoline, line}, state) do
    win =
      :dbg_wx_trace_win.select_line(
        r_state(state, :win),
        line
      )

    r_state(state, win: win)
  end

  defp gui_cmd(:Search, state) do
    win = :dbg_wx_trace_win.helpwin(:search, r_state(state, :win))
    r_state(state, win: win)
  end

  defp gui_cmd(:"Line Break...", state) do
    add_break(
      r_state(state, :gs),
      r_state(state, :coords),
      :line,
      r_state(state, :mod),
      :dbg_wx_trace_win.selected_line(r_state(state, :win))
    )

    state
  end

  defp gui_cmd(:"Conditional Break...", state) do
    add_break(
      r_state(state, :gs),
      r_state(state, :coords),
      :conditional,
      r_state(state, :mod),
      :dbg_wx_trace_win.selected_line(r_state(state, :win))
    )

    state
  end

  defp gui_cmd(:"Function Break...", state) do
    add_break(
      r_state(state, :gs),
      r_state(state, :coords),
      :function,
      r_state(state, :mod),
      :undefined
    )

    state
  end

  defp gui_cmd(:"Enable All", state) do
    breaks = :int.all_breaks()
    thisMod = r_state(state, :mod)

    :lists.foreach(
      fn
        {{mod, line}, _Options}
        when mod === thisMod ->
          :int.enable_break(mod, line)

        _Break ->
          :ignore
      end,
      breaks
    )

    state
  end

  defp gui_cmd(:"Disable All", state) do
    breaks = :int.all_breaks()
    thisMod = r_state(state, :mod)

    :lists.foreach(
      fn
        {{mod, line}, _Options}
        when mod === thisMod ->
          :int.disable_break(mod, line)

        _Break ->
          :ignore
      end,
      breaks
    )

    state
  end

  defp gui_cmd(:"Delete All", state) do
    :int.no_break(r_state(state, :mod))
    state
  end

  defp gui_cmd({:break, {mod, line}, what}, state) do
    case what do
      :add ->
        :int.break(mod, line)

      :delete ->
        :int.delete_break(mod, line)

      {:status, :inactive} ->
        :int.disable_break(mod, line)

      {:status, :active} ->
        :int.enable_break(mod, line)

      {:trigger, action} ->
        :int.action_at_break(mod, line, action)
    end

    state
  end

  defp gui_cmd(:Debugger, state) do
    window = :dbg_wx_trace_win.get_window(r_state(state, :win))
    helpFile = :filename.join([:code.lib_dir(:debugger), 'doc', 'html', 'part_frame.html'])
    :dbg_wx_win.open_help(window, helpFile)
    state
  end

  defp add_break(gS, coords, type, :undefined, _Line) do
    :dbg_wx_break.start(gS, coords, type)
  end

  defp add_break(gS, coords, type, mod, :undefined) do
    :dbg_wx_break.start(gS, coords, type, mod)
  end

  defp add_break(gS, coords, type, mod, line) do
    :dbg_wx_break.start(gS, coords, type, mod, line)
  end

  defp int_cmd(
         {:new_break, {{mod, _Line}, _Options} = break},
         r_state(mod: mod, win: win) = state
       ) do
    r_state(state, win: :dbg_wx_trace_win.add_break(win, :Break, break))
  end

  defp int_cmd(
         {:delete_break, {mod, _Line} = point},
         r_state(mod: mod, win: win) = state
       ) do
    r_state(state,
      win:
        :dbg_wx_trace_win.delete_break(
          win,
          point
        )
    )
  end

  defp int_cmd(
         {:break_options, {{mod, _Line}, _Options} = break},
         r_state(mod: mod, win: win) = state
       ) do
    r_state(state,
      win:
        :dbg_wx_trace_win.update_break(
          win,
          break
        )
    )
  end

  defp int_cmd(:no_break, r_state(win: win) = state) do
    r_state(state, win: :dbg_wx_trace_win.clear_breaks(win))
  end

  defp int_cmd({:no_break, _Mod}, r_state(win: win) = state) do
    r_state(state, win: :dbg_wx_trace_win.clear_breaks(win))
  end

  defp int_cmd(_, state) do
    state
  end

  defp menus() do
    [
      {:File, [{:Close, 0}]},
      {:Edit, [{:"Go To Line", 0}, {:Search, 0}]},
      {:Break,
       [
         {:"Line Break...", 5},
         {:"Conditional Break...", 13},
         {:"Function Break...", 0},
         :separator,
         {:"Enable All", :no},
         {:"Disable All", :no},
         {:"Delete All", 0},
         :separator
       ]},
      {:Windows, []},
      {:Help, [{:Debugger, :no}]}
    ]
  end

  defp shortcut(:c) do
    :Close
  end

  defp shortcut(:g) do
    :"Go To Line"
  end

  defp shortcut(:s) do
    :Search
  end

  defp shortcut(:b) do
    :"Line Break..."
  end

  defp shortcut(:r) do
    :"Conditional Break..."
  end

  defp shortcut(:f) do
    :"Function Break..."
  end

  defp shortcut(:d) do
    :"Delete All"
  end

  defp shortcut(_) do
    false
  end

  defp gui_load_module(win, mod) do
    :dbg_wx_trace_win.display(win, {:text, 'Loading module...'})

    case :dbg_iserver.call({:raw_contents, mod, :any}) do
      {:ok, contents} ->
        win2 = :dbg_wx_trace_win.show_code(win, mod, contents)
        :dbg_wx_trace_win.display(win, {:text, ''})
        win2

      :not_found ->
        :dbg_wx_trace_win.show_no_code(win)
    end
  end
end
