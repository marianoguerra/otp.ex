defmodule :m_dbg_wx_mon do
  use Bitwise
  require Record

  Record.defrecord(:r_file_info, :file_info,
    size: :undefined,
    type: :undefined,
    access: :undefined,
    atime: :undefined,
    mtime: :undefined,
    ctime: :undefined,
    mode: :undefined,
    links: :undefined,
    major_device: :undefined,
    minor_device: :undefined,
    inode: :undefined,
    uid: :undefined,
    gid: :undefined
  )

  Record.defrecord(:r_file_descriptor, :file_descriptor,
    module: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_wx, :wx,
    id: :undefined,
    obj: :undefined,
    userData: :undefined,
    event: :undefined
  )

  Record.defrecord(:r_wxActivate, :wxActivate,
    type: :undefined,
    active: :undefined
  )

  Record.defrecord(:r_wxAuiManager, :wxAuiManager,
    type: :undefined,
    manager: :undefined,
    pane: :undefined,
    button: :undefined,
    veto_flag: :undefined,
    canveto_flag: :undefined,
    dc: :undefined
  )

  Record.defrecord(:r_wxAuiNotebook, :wxAuiNotebook,
    type: :undefined,
    old_selection: :undefined,
    selection: :undefined,
    drag_source: :undefined
  )

  Record.defrecord(:r_wxCalendar, :wxCalendar,
    type: :undefined,
    wday: :undefined,
    date: :undefined
  )

  Record.defrecord(:r_wxChildFocus, :wxChildFocus, type: :undefined)
  Record.defrecord(:r_wxClipboardText, :wxClipboardText, type: :undefined)
  Record.defrecord(:r_wxClose, :wxClose, type: :undefined)

  Record.defrecord(:r_wxColourPicker, :wxColourPicker,
    type: :undefined,
    colour: :undefined
  )

  Record.defrecord(:r_wxCommand, :wxCommand,
    type: :undefined,
    cmdString: :undefined,
    commandInt: :undefined,
    extraLong: :undefined
  )

  Record.defrecord(:r_wxContextMenu, :wxContextMenu,
    type: :undefined,
    pos: :undefined
  )

  Record.defrecord(:r_wxDate, :wxDate,
    type: :undefined,
    date: :undefined
  )

  Record.defrecord(:r_wxDisplayChanged, :wxDisplayChanged, type: :undefined)

  Record.defrecord(:r_wxDropFiles, :wxDropFiles,
    type: :undefined,
    noFiles: :undefined,
    pos: :undefined,
    files: :undefined
  )

  Record.defrecord(:r_wxErase, :wxErase,
    type: :undefined,
    dc: :undefined
  )

  Record.defrecord(:r_wxFileDirPicker, :wxFileDirPicker,
    type: :undefined,
    path: :undefined
  )

  Record.defrecord(:r_wxFocus, :wxFocus,
    type: :undefined,
    win: :undefined
  )

  Record.defrecord(:r_wxFontPicker, :wxFontPicker,
    type: :undefined,
    font: :undefined
  )

  Record.defrecord(:r_wxGrid, :wxGrid,
    type: :undefined,
    row: :undefined,
    col: :undefined,
    x: :undefined,
    y: :undefined,
    selecting: :undefined,
    control: :undefined,
    meta: :undefined,
    shift: :undefined,
    alt: :undefined
  )

  Record.defrecord(:r_wxHelp, :wxHelp, type: :undefined)

  Record.defrecord(:r_wxHtmlLink, :wxHtmlLink,
    type: :undefined,
    linkInfo: :undefined
  )

  Record.defrecord(:r_wxIconize, :wxIconize,
    type: :undefined,
    iconized: :undefined
  )

  Record.defrecord(:r_wxIdle, :wxIdle, type: :undefined)
  Record.defrecord(:r_wxInitDialog, :wxInitDialog, type: :undefined)

  Record.defrecord(:r_wxJoystick, :wxJoystick,
    type: :undefined,
    pos: :undefined,
    zPosition: :undefined,
    buttonChange: :undefined,
    buttonState: :undefined,
    joyStick: :undefined
  )

  Record.defrecord(:r_wxKey, :wxKey,
    type: :undefined,
    x: :undefined,
    y: :undefined,
    keyCode: :undefined,
    controlDown: :undefined,
    shiftDown: :undefined,
    altDown: :undefined,
    metaDown: :undefined,
    scanCode: :undefined,
    uniChar: :undefined,
    rawCode: :undefined,
    rawFlags: :undefined
  )

  Record.defrecord(:r_wxList, :wxList,
    type: :undefined,
    code: :undefined,
    oldItemIndex: :undefined,
    itemIndex: :undefined,
    col: :undefined,
    pointDrag: :undefined
  )

  Record.defrecord(:r_wxMaximize, :wxMaximize, type: :undefined)
  Record.defrecord(:r_wxMenu, :wxMenu, type: :undefined, menuId: :undefined, menu: :undefined)
  Record.defrecord(:r_wxMouseCaptureChanged, :wxMouseCaptureChanged, type: :undefined)
  Record.defrecord(:r_wxMouseCaptureLost, :wxMouseCaptureLost, type: :undefined)

  Record.defrecord(:r_wxMouse, :wxMouse,
    type: :undefined,
    x: :undefined,
    y: :undefined,
    leftDown: :undefined,
    middleDown: :undefined,
    rightDown: :undefined,
    controlDown: :undefined,
    shiftDown: :undefined,
    altDown: :undefined,
    metaDown: :undefined,
    wheelRotation: :undefined,
    wheelDelta: :undefined,
    linesPerAction: :undefined
  )

  Record.defrecord(:r_wxMove, :wxMove, type: :undefined, pos: :undefined, rect: :undefined)

  Record.defrecord(:r_wxNavigationKey, :wxNavigationKey,
    type: :undefined,
    flags: :undefined,
    focus: :undefined
  )

  Record.defrecord(:r_wxNotebook, :wxNotebook,
    type: :undefined,
    nSel: :undefined,
    nOldSel: :undefined
  )

  Record.defrecord(:r_wxPaint, :wxPaint, type: :undefined)
  Record.defrecord(:r_wxPaletteChanged, :wxPaletteChanged, type: :undefined)
  Record.defrecord(:r_wxQueryNewPalette, :wxQueryNewPalette, type: :undefined)

  Record.defrecord(:r_wxSash, :wxSash,
    type: :undefined,
    edge: :undefined,
    dragRect: :undefined,
    dragStatus: :undefined
  )

  Record.defrecord(:r_wxScroll, :wxScroll,
    type: :undefined,
    commandInt: :undefined,
    extraLong: :undefined
  )

  Record.defrecord(:r_wxScrollWin, :wxScrollWin,
    type: :undefined,
    commandInt: :undefined,
    extraLong: :undefined
  )

  Record.defrecord(:r_wxSetCursor, :wxSetCursor,
    type: :undefined,
    x: :undefined,
    y: :undefined,
    cursor: :undefined
  )

  Record.defrecord(:r_wxShow, :wxShow,
    type: :undefined,
    show: :undefined
  )

  Record.defrecord(:r_wxSize, :wxSize, type: :undefined, size: :undefined, rect: :undefined)

  Record.defrecord(:r_wxSpin, :wxSpin,
    type: :undefined,
    commandInt: :undefined
  )

  Record.defrecord(:r_wxSplitter, :wxSplitter, type: :undefined)

  Record.defrecord(:r_wxStyledText, :wxStyledText,
    type: :undefined,
    position: :undefined,
    key: :undefined,
    modifiers: :undefined,
    modificationType: :undefined,
    text: :undefined,
    length: :undefined,
    linesAdded: :undefined,
    line: :undefined,
    foldLevelNow: :undefined,
    foldLevelPrev: :undefined,
    margin: :undefined,
    message: :undefined,
    wParam: :undefined,
    lParam: :undefined,
    listType: :undefined,
    x: :undefined,
    y: :undefined,
    dragText: :undefined,
    dragAllowMove: :undefined,
    dragResult: :undefined
  )

  Record.defrecord(:r_wxSysColourChanged, :wxSysColourChanged, type: :undefined)
  Record.defrecord(:r_wxTaskBarIcon, :wxTaskBarIcon, type: :undefined)

  Record.defrecord(:r_wxTree, :wxTree,
    type: :undefined,
    item: :undefined,
    itemOld: :undefined,
    pointDrag: :undefined
  )

  Record.defrecord(:r_wxUpdateUI, :wxUpdateUI, type: :undefined)
  Record.defrecord(:r_wxWindowCreate, :wxWindowCreate, type: :undefined)
  Record.defrecord(:r_wxWindowDestroy, :wxWindowDestroy, type: :undefined)

  Record.defrecord(:r_wxMouseState, :wxMouseState,
    x: :undefined,
    y: :undefined,
    leftDown: :undefined,
    middleDown: :undefined,
    rightDown: :undefined,
    controlDown: :undefined,
    shiftDown: :undefined,
    altDown: :undefined,
    metaDown: :undefined,
    cmdDown: :undefined
  )

  Record.defrecord(:r_wxHtmlLinkInfo, :wxHtmlLinkInfo,
    href: :undefined,
    target: :undefined
  )

  Record.defrecord(:r_pinfo, :pinfo,
    pid: :undefined,
    status: :undefined
  )

  Record.defrecord(:r_state, :state,
    mode: :undefined,
    starter: :undefined,
    win: :undefined,
    focus: :undefined,
    coords: :undefined,
    intdir: :undefined,
    pinfos: :undefined,
    tracewin: :undefined,
    backtrace: :undefined,
    strings: :undefined,
    attach: :undefined,
    sfile: :undefined,
    changed: :undefined
  )

  def start(mode, sFile) do
    case :erlang.whereis(:dbg_wx_mon) do
      :undefined ->
        callingPid = self()

        pid =
          spawn(fn ->
            init(callingPid, mode, sFile)
          end)

        receive do
          {:initialization_complete, ^pid} ->
            {:ok, pid}

          error ->
            error
        end

      pid ->
        {:error, {:already_started, pid}}
    end
  end

  def stop() do
    case :erlang.whereis(:dbg_wx_mon) do
      :undefined ->
        :ok

      pid ->
        flag = :erlang.process_flag(:trap_exit, true)
        :erlang.link(pid)
        send(pid, :stop)

        receive do
          {:EXIT, ^pid, :stop} ->
            :erlang.process_flag(:trap_exit, flag)
            :ok
        end
    end
  end

  defp init(callingPid, mode, sFile) do
    :erlang.register(:dbg_wx_mon, self())

    case (try do
            :dbg_wx_mon_win.init()
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        send(callingPid, {:error, reason})

      gS ->
        try do
          init2(callingPid, mode, sFile, gS)
        catch
          :exit, :stop ->
            :stop

          error, reason ->
            :io.format(
              '~p: Crashed {~p,~p} in~n  ~p',
              [:dbg_wx_mon, error, reason, __STACKTRACE__]
            )
        end
    end
  end

  defp init2(callingPid, mode, sFile, gS) do
    bool =
      case :int.start() do
        {:ok, _Int} ->
          true

        {:error, {:already_started, _Int}} ->
          false
      end

    :int.subscribe()
    :dbg_wx_win.init()
    _ = :dbg_wx_winman.start()
    title = 'Monitor'
    win = :dbg_wx_mon_win.create_win(gS, title, menus())
    window = :dbg_wx_mon_win.get_window(win)
    :dbg_wx_winman.insert(title, window)

    state1 =
      r_state(
        mode: mode,
        starter: bool,
        win: win,
        focus: :undefined,
        coords: {-1, -1},
        intdir: :erlang.element(2, :file.get_cwd()),
        pinfos: [],
        sfile: sFile,
        changed: false
      )

    state2 =
      init_options(
        [:"Search Area", :"Button Area", :"Evaluator Area", :"Bindings Area"],
        :int.auto_attach(),
        :int.stack_trace(),
        100,
        [:str_on],
        state1
      )

    state3 = init_contents(:int.interpreted(), :int.all_breaks(), :int.snapshot(), state2)
    gui_enable_functions(r_state(state3, :focus))
    send(callingPid, {:initialization_complete, self()})

    cond do
      sFile === :default ->
        loop(state3)

      true ->
        loop(load_settings(sFile, state3))
    end
  end

  defp init_options(traceWin, autoAttach, stackTrace, backTrace, strings, state) do
    :lists.foreach(
      fn area ->
        :dbg_wx_mon_win.select(area, true)
      end,
      traceWin
    )

    case autoAttach do
      false ->
        :ignore

      {flags, _Function} ->
        :dbg_wx_mon_win.show_option(r_state(state, :win), :auto_attach, flags)
        select(flags, [:init, :exit, :break])
    end

    :dbg_wx_mon_win.show_option(r_state(state, :win), :stack_trace, stackTrace)
    :dbg_wx_mon_win.select(map(stackTrace), true)
    :dbg_wx_mon_win.show_option(r_state(state, :win), :back_trace, backTrace)
    select(strings, [:str_on])
    :dbg_wx_mon_win.show_option(r_state(state, :win), :strings, strings)
    r_state(state, tracewin: traceWin, backtrace: backTrace, strings: strings)
  end

  defp init_contents(mods, breaks, processes, state) do
    win2 =
      :lists.foldl(
        fn mod, win ->
          :dbg_wx_mon_win.add_module(win, :Module, mod)
        end,
        r_state(state, :win),
        mods
      )

    win3 =
      :lists.foldl(
        fn break, win ->
          :dbg_wx_mon_win.add_break(win, :Break, break)
        end,
        win2,
        breaks
      )

    :lists.foldl(
      fn pidTuple, state0 ->
        int_cmd({:new_process, pidTuple}, state0)
      end,
      r_state(state, win: win3),
      processes
    )
  end

  defp loop(state) do
    receive do
      :stop ->
        gui_cmd(:stopped, state)

      r_wx() = guiEvent ->
        cmd =
          :dbg_wx_mon_win.handle_event(
            guiEvent,
            r_state(state, :win)
          )

        state2 = gui_cmd(cmd, state)
        loop(state2)

      {:int, cmd} ->
        state2 = int_cmd(cmd, state)
        loop(state2)

      {:dbg_ui_interpret, dir} ->
        loop(r_state(state, intdir: dir))

      {:dbg_ui_winman, :update_windows_menu, data} ->
        window = :dbg_wx_mon_win.get_window(r_state(state, :win))
        :dbg_wx_winman.update_windows_menu(window, data)
        loop(state)

      {:dbg_wx_trace, from, :get_env} ->
        send(
          from,
          {:env, self(), :wx.get_env(), :dbg_wx_mon_win.get_window(r_state(state, :win))}
        )

        loop(state)
    end
  end

  defp gui_cmd(:ignore, state) do
    state
  end

  defp gui_cmd(:stopped, state) do
    cond do
      r_state(state, :starter) === true ->
        :int.stop()

      true ->
        :int.auto_attach(false)
    end

    exit(:stop)
  end

  defp gui_cmd({:coords, coords}, state) do
    r_state(state, coords: coords)
  end

  defp gui_cmd({:shortcut, key}, state) do
    case shortcut(key) do
      {:always, cmd} ->
        gui_cmd(cmd, state)

      {:if_enabled, cmd} ->
        case :dbg_wx_mon_win.is_enabled(cmd) do
          true ->
            gui_cmd(cmd, state)

          false ->
            state
        end

      false ->
        state
    end
  end

  defp gui_cmd(:"Load Settings...", state) do
    window = :dbg_wx_mon_win.get_window(r_state(state, :win))

    case :dbg_wx_settings.load(window, r_state(state, :coords), r_state(state, :sfile)) do
      :cancel ->
        state

      {:ok, file} ->
        load_settings(file, state)
    end
  end

  defp gui_cmd(:"Save Settings...", state) do
    window = :dbg_wx_mon_win.get_window(r_state(state, :win))

    case :dbg_wx_settings.save(window, r_state(state, :coords), r_state(state, :sfile)) do
      :cancel ->
        state

      {:ok, file} ->
        save_settings(file, state)
    end
  end

  defp gui_cmd(:Exit, state) do
    gui_cmd(:stopped, state)
  end

  defp gui_cmd(:Refresh, state) do
    :int.clear()
    win = :dbg_wx_mon_win.clear_processes(r_state(state, :win))
    gui_enable_functions(:undefined)
    state2 = r_state(state, win: win, focus: :undefined, pinfos: [])

    :lists.foldl(
      fn pidTuple, s ->
        int_cmd({:new_process, pidTuple}, s)
      end,
      state2,
      :int.snapshot()
    )
  end

  defp gui_cmd(:"Kill All", state) do
    :lists.foreach(
      fn pInfo ->
        case r_pinfo(pInfo, :status) do
          :exit ->
            :ignore

          _Status ->
            :erlang.exit(r_pinfo(pInfo, :pid), :kill)
        end
      end,
      r_state(state, :pinfos)
    )

    state
  end

  defp gui_cmd(:"Interpret...", state) do
    window = :dbg_wx_mon_win.get_window(r_state(state, :win))

    :dbg_wx_interpret.start(
      window,
      r_state(state, :coords),
      r_state(state, :intdir),
      r_state(state, :mode)
    )

    state
  end

  defp gui_cmd(:"Delete All Modules", state) do
    :lists.foreach(
      fn mod ->
        :int.nn(mod)
      end,
      :int.interpreted()
    )

    state
  end

  defp gui_cmd({:module, mod, what}, state) do
    _ =
      case what do
        :delete ->
          :int.nn(mod)

        :view ->
          window = :dbg_wx_mon_win.get_window(r_state(state, :win))
          :dbg_wx_view.start(window, mod)
      end

    state
  end

  defp gui_cmd(:Step, state) do
    :int.step(r_pinfo(r_state(state, :focus), :pid))
    state
  end

  defp gui_cmd(:Next, state) do
    :int.next(r_pinfo(r_state(state, :focus), :pid))
    state
  end

  defp gui_cmd(:Continue, state) do
    :int.continue(r_pinfo(r_state(state, :focus), :pid))
    state
  end

  defp gui_cmd(:"Finish ", state) do
    :int.finish(r_pinfo(r_state(state, :focus), :pid))
    state
  end

  defp gui_cmd(:Attach, state) do
    pid = r_pinfo(r_state(state, :focus), :pid)

    case :dbg_wx_winman.is_started(:dbg_wx_trace.title(pid)) do
      true ->
        :ignore

      false ->
        :int.attach(pid, trace_function(state))
    end

    state
  end

  defp gui_cmd(:Kill, state) do
    :erlang.exit(r_pinfo(r_state(state, :focus), :pid), :kill)
    state
  end

  defp gui_cmd(:"Line Break...", state) do
    window = :dbg_wx_mon_win.get_window(r_state(state, :win))
    :dbg_wx_break.start(window, r_state(state, :coords), :line)
    state
  end

  defp gui_cmd(:"Conditional Break...", state) do
    window = :dbg_wx_mon_win.get_window(r_state(state, :win))
    :dbg_wx_break.start(window, r_state(state, :coords), :conditional)
    state
  end

  defp gui_cmd(:"Function Break...", state) do
    window = :dbg_wx_mon_win.get_window(r_state(state, :win))
    :dbg_wx_break.start(window, r_state(state, :coords), :function)
    state
  end

  defp gui_cmd(:"Enable All", state) do
    breaks = :int.all_breaks()

    :lists.foreach(
      fn {{mod, line}, _Options} ->
        :int.enable_break(mod, line)
      end,
      breaks
    )

    state
  end

  defp gui_cmd(:"Disable All", state) do
    breaks = :int.all_breaks()

    :lists.foreach(
      fn {{mod, line}, _Options} ->
        :int.disable_break(mod, line)
      end,
      breaks
    )

    state
  end

  defp gui_cmd(:"Delete All", state) do
    :int.no_break()
    state
  end

  defp gui_cmd({:break, {mod, line}, what}, state) do
    case what do
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

  defp gui_cmd({:"Trace Window", traceWin}, state) do
    state2 = r_state(state, tracewin: traceWin)

    case r_state(state, :attach) do
      false ->
        :ignore

      {flags, {:dbg_wx_trace, :start, startFlags}} ->
        case trace_function(state2) do
          {_, _, ^startFlags} ->
            :ignore

          newFunction ->
            :int.auto_attach(flags, newFunction)
        end

      _AutoAttach ->
        :ignore
    end

    state2
  end

  defp gui_cmd({:"Auto Attach", when__}, state) do
    cond do
      when__ === [] ->
        :int.auto_attach(false)

      true ->
        flags =
          for name <- when__ do
            map(name)
          end

        :int.auto_attach(flags, trace_function(state))
    end

    state
  end

  defp gui_cmd({:"Stack Trace", [name]}, state) do
    :int.stack_trace(map(name))
    state
  end

  defp gui_cmd(:"Back Trace Size...", state) do
    window = :dbg_wx_mon_win.get_window(r_state(state, :win))
    what = {:integer, :erlang.integer_to_list(r_state(state, :backtrace))}

    case :dbg_wx_win.entry(window, 'Backtrace', :"Backtrace:", what) do
      :cancel ->
        state

      {_, backTrace} ->
        :dbg_wx_mon_win.show_option(r_state(state, :win), :back_trace, backTrace)
        r_state(state, backtrace: backTrace)
    end
  end

  defp gui_cmd({:Strings, flags}, state) do
    names =
      for flag <- flags do
        map(flag)
      end

    :dbg_wx_mon_win.show_option(r_state(state, :win), :strings, names)
    select(names, [:str_on])
    r_state(state, strings: names)
  end

  defp gui_cmd(:Debugger, state) do
    helpFile = :filename.join([:code.lib_dir(:debugger), 'doc', 'html', 'index.html'])
    window = :dbg_wx_mon_win.get_window(r_state(state, :win))
    :dbg_wx_win.open_help(window, helpFile)
    state
  end

  defp gui_cmd({:focus, pid, win}, state) do
    {:value, pInfo} = :lists.keysearch(pid, r_pinfo(:pid), r_state(state, :pinfos))
    gui_enable_functions(pInfo)
    r_state(state, win: win, focus: pInfo)
  end

  defp gui_cmd(:default, state) do
    case :lists.member(
           :Attach,
           menus(:enabled, r_state(state, :focus))
         ) do
      true ->
        gui_cmd(:Attach, state)

      false ->
        state
    end
  end

  defp int_cmd({:interpret, mod}, state) do
    win = :dbg_wx_mon_win.add_module(r_state(state, :win), :Module, mod)
    r_state(state, win: win)
  end

  defp int_cmd({:no_interpret, mod}, state) do
    win = :dbg_wx_mon_win.delete_module(r_state(state, :win), mod)
    r_state(state, win: win)
  end

  defp int_cmd(
         {:new_process, {pid, function, status, info}},
         state
       ) do
    name = registered_name(pid)
    pInfo = r_pinfo(pid: pid, status: status)
    win = :dbg_wx_mon_win.add_process(r_state(state, :win), pid, name, function, status, info)
    pInfos = r_state(state, :pinfos) ++ [pInfo]
    r_state(state, win: win, pinfos: pInfos)
  end

  defp int_cmd({:new_status, pid, status, info}, state) do
    pInfos = r_state(state, :pinfos)
    {:value, pInfo} = :lists.keysearch(pid, r_pinfo(:pid), pInfos)
    pInfo2 = r_pinfo(pInfo, status: status)
    pInfos2 = :lists.keyreplace(pid, r_pinfo(:pid), pInfos, pInfo2)
    state2 = r_state(state, pinfos: pInfos2)
    :dbg_wx_mon_win.update_process(r_state(state2, :win), pid, status, info)

    case r_state(state2, :focus) do
      r_pinfo(pid: ^pid) ->
        gui_enable_functions(pInfo2)
        r_state(state2, focus: pInfo2)

      _ ->
        state2
    end
  end

  defp int_cmd({:new_break, break}, state) do
    win = :dbg_wx_mon_win.add_break(r_state(state, :win), :Break, break)
    r_state(state, win: win)
  end

  defp int_cmd({:delete_break, point}, state) do
    win =
      :dbg_wx_mon_win.delete_break(
        r_state(state, :win),
        point
      )

    r_state(state, win: win)
  end

  defp int_cmd({:break_options, break}, state) do
    :dbg_wx_mon_win.update_break(r_state(state, :win), break)
    state
  end

  defp int_cmd(:no_break, state) do
    win = :dbg_wx_mon_win.clear_breaks(r_state(state, :win))
    r_state(state, win: win)
  end

  defp int_cmd({:no_break, mod}, state) do
    win = :dbg_wx_mon_win.clear_breaks(r_state(state, :win), mod)
    r_state(state, win: win)
  end

  defp int_cmd({:auto_attach, autoAttach}, state) do
    onFlags =
      case autoAttach do
        false ->
          []

        {flags, _Function} ->
          flags
      end

    :dbg_wx_mon_win.show_option(r_state(state, :win), :auto_attach, onFlags)
    select(onFlags, [:init, :exit, :break])
    r_state(state, attach: autoAttach)
  end

  defp int_cmd({:stack_trace, flag}, state) do
    :dbg_wx_mon_win.show_option(r_state(state, :win), :stack_trace, flag)
    :dbg_wx_mon_win.select(map(flag), true)
    state
  end

  defp menus() do
    [
      {:File, [{:"Load Settings...", 0}, {:"Save Settings...", 2}, :separator, {:Exit, 0}]},
      {:Edit, [{:Refresh, :no}, {:"Kill All", :no}]},
      {:Module, [{:"Interpret...", 0}, {:"Delete All Modules", :no}, :separator]},
      {:Process,
       [
         {:Step, 0},
         {:Next, 0},
         {:Continue, 0},
         {:"Finish ", 0},
         :separator,
         {:Attach, 0},
         {:Kill, :no}
       ]},
      {:Break,
       [
         {:"Line Break...", 5},
         {:"Conditional Break...", :no},
         {:"Function Break...", :no},
         :separator,
         {:"Enable All", :no},
         {:"Disable All", :no},
         {:"Delete All", 0},
         :separator
       ]},
      {:Options,
       [
         {:"Trace Window", :no, :cascade,
          [
            {:"Search Area", :no, :check},
            {:"Button Area", :no, :check},
            {:"Evaluator Area", :no, :check},
            {:"Bindings Area", :no, :check},
            {:"Trace Area", :no, :check}
          ]},
         {:"Auto Attach", :no, :cascade,
          [{:"First Call", :no, :check}, {:"On Break", :no, :check}, {:"On Exit", :no, :check}]},
         {:"Stack Trace", :no, :cascade,
          [
            {:"Stack On, Tail", :no, :radio},
            {:"Stack On, No Tail", :no, :radio},
            {:"Stack Off", :no, :radio}
          ]},
         {:Strings, :no, :cascade, [{:"Use range of +pc flag", :no, :check}]},
         {:"Back Trace Size...", :no}
       ]},
      {:Windows, []},
      {:Help, [{:Debugger, :no}]}
    ]
  end

  defp menus(:enabled, :undefined) do
    []
  end

  defp menus(:disabled, :undefined) do
    [:Step, :Next, :Continue, :"Finish ", :Attach, :Kill]
  end

  defp menus(:enabled, r_pinfo(status: :exit)) do
    [:Attach]
  end

  defp menus(:disabled, r_pinfo(status: :exit)) do
    [:Step, :Next, :Continue, :"Finish ", :Kill]
  end

  defp menus(:enabled, r_pinfo(status: :break)) do
    [:Step, :Next, :Continue, :"Finish ", :Attach, :Kill]
  end

  defp menus(:disabled, r_pinfo(status: :break)) do
    []
  end

  defp menus(:enabled, _PInfo) do
    [:Attach, :Kill]
  end

  defp menus(:disabled, _PInfo) do
    [:Step, :Next, :Continue, :"Finish "]
  end

  defp shortcut(:l) do
    {:always, :"Load Settings..."}
  end

  defp shortcut(:v) do
    {:always, :"Save Settings..."}
  end

  defp shortcut(:e) do
    {:always, :Exit}
  end

  defp shortcut(:i) do
    {:always, :"Interpret..."}
  end

  defp shortcut(:s) do
    {:if_enabled, :Step}
  end

  defp shortcut(:n) do
    {:if_enabled, :Next}
  end

  defp shortcut(:c) do
    {:if_enabled, :Continue}
  end

  defp shortcut(:f) do
    {:if_enabled, :"Finish "}
  end

  defp shortcut(:a) do
    {:if_enabled, :Attach}
  end

  defp shortcut(:b) do
    {:always, :"Line Break..."}
  end

  defp shortcut(:d) do
    {:always, :"Delete All"}
  end

  defp shortcut(_) do
    false
  end

  defp gui_enable_functions(pInfo) do
    enabled = menus(:enabled, pInfo)
    disabled = menus(:disabled, pInfo)
    :dbg_wx_mon_win.enable(enabled, true)
    :dbg_wx_mon_win.enable(disabled, false)
  end

  defp map(:"First Call") do
    :init
  end

  defp map(:"On Exit") do
    :exit
  end

  defp map(:"On Break") do
    :break
  end

  defp map(:init) do
    :"First Call"
  end

  defp map(:exit) do
    :"On Exit"
  end

  defp map(:break) do
    :"On Break"
  end

  defp map(:"Stack On, Tail") do
    :all
  end

  defp map(:"Stack On, No Tail") do
    :no_tail
  end

  defp map(:"Stack Off") do
    false
  end

  defp map(:all) do
    :"Stack On, Tail"
  end

  defp map(true) do
    :"Stack On, Tail"
  end

  defp map(:no_tail) do
    :"Stack On, No Tail"
  end

  defp map(false) do
    :"Stack Off"
  end

  defp map(:"Use range of +pc flag") do
    :str_on
  end

  defp map(:str_on) do
    :"Use range of +pc flag"
  end

  defp select(flags, allFlags) do
    offFlags = allFlags -- flags

    :lists.foreach(
      fn flag ->
        :dbg_wx_mon_win.select(map(flag), false)
      end,
      offFlags
    )

    :lists.foreach(
      fn flag ->
        :dbg_wx_mon_win.select(map(flag), true)
      end,
      flags
    )
  end

  defp load_settings(sFile, state) do
    case :file.read_file(sFile) do
      {:ok, binary} ->
        case (try do
                :erlang.binary_to_term(binary)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:debugger_settings, settings} ->
            load_settings2(
              settings,
              r_state(state, sfile: sFile, changed: false)
            )

          _Error ->
            state
        end

      {:error, _Reason} ->
        state
    end
  end

  defp load_settings2(settings, state) do
    vals = loaded(settings)
    [traceWin, autoAttach, stackTrace, backTrace, strings, files, breaks] = vals
    traceWinAll = [:"Button Area", :"Evaluator Area", :"Bindings Area", :"Trace Area"]

    :lists.foreach(
      fn area ->
        :dbg_wx_mon_win.select(area, true)
      end,
      traceWin
    )

    :lists.foreach(
      fn area ->
        :dbg_wx_mon_win.select(area, false)
      end,
      traceWinAll -- traceWin
    )

    case autoAttach do
      false ->
        :int.auto_attach(false)

      {flags, function} ->
        :int.auto_attach(flags, function)
    end

    :int.stack_trace(stackTrace)

    cond do
      strings === :keep ->
        :ok

      true ->
        :dbg_wx_mon_win.show_option(r_state(state, :win), :strings, strings)
        select(strings, [:str_on])
    end

    :dbg_wx_mon_win.show_option(r_state(state, :win), :back_trace, backTrace)

    case r_state(state, :mode) do
      :local ->
        :lists.foreach(
          fn file ->
            :int.i(file)
          end,
          files
        )

      :global ->
        :lists.foreach(
          fn file ->
            :int.ni(file)
          end,
          files
        )
    end

    :lists.foreach(
      fn break ->
        {{mod, line}, [status, action, _, cond__]} = break
        :int.break(mod, line)

        cond do
          status === :inactive ->
            :int.disable_break(mod, line)

          true ->
            :ignore
        end

        cond do
          action !== :enable ->
            :int.action_at_break(mod, line, action)

          true ->
            :ignore
        end

        case cond__ do
          cFunction when is_tuple(cFunction) ->
            :int.test_at_break(mod, line, cFunction)

          :null ->
            :ignore
        end
      end,
      breaks
    )

    r_state(state, tracewin: traceWin, backtrace: backTrace, strings: strings)
  end

  defp loaded({traceWin, autoAttach, stackTrace, backTrace, files, breaks}) do
    [traceWin, autoAttach, stackTrace, backTrace, :keep, files, breaks]
  end

  defp loaded(settings) when is_list(settings) do
    keys = [:trace_win, :auto_attach, :stack_trace, :back_trace, :strings, :files, :breaks]

    for key <- keys do
      :proplists.get_value(key, settings)
    end
  end

  defp save_settings(sFile, state) do
    settings = saved(state)
    binary = :erlang.term_to_binary({:debugger_settings, settings})

    case :file.write_file(sFile, binary) do
      :ok ->
        r_state(state, sfile: sFile, changed: false)

      {:error, _Reason} ->
        state
    end
  end

  defp saved(r_state(tracewin: traceWin, backtrace: backTrace, strings: strings)) do
    [
      {:trace_win, traceWin},
      {:auto_attach, :int.auto_attach()},
      {:stack_trace, :int.stack_trace()},
      {:back_trace, backTrace},
      {:strings, strings},
      {:files,
       for mod <- :int.interpreted() do
         :int.file(mod)
       end},
      {:breaks, :int.all_breaks()}
    ]
  end

  defp registered_name(pid) do
    :timer.sleep(200)
    node = node(pid)

    cond do
      node === node() ->
        case :erlang.process_info(pid, :registered_name) do
          {:registered_name, name} ->
            name

          _ ->
            :undefined
        end

      true ->
        case :rpc.call(node, :erlang, :process_info, [pid, :registered_name]) do
          {:registered_name, name} ->
            name

          _ ->
            :undefined
        end
    end
  end

  defp trace_function(state) do
    r_state(tracewin: win, backtrace: bT, strings: str) = state
    {:dbg_wx_trace, :start, [win, bT, str]}
  end
end
