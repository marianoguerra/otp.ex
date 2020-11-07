defmodule :m_dbg_wx_trace_win do
  use Bitwise
  require Record

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

  Record.defrecord(:r_breakInfo, :breakInfo,
    point: :undefined,
    status: :undefined,
    break: :undefined
  )

  Record.defrecord(:r_break, :break,
    mb: :undefined,
    smi: :undefined,
    emi: :undefined,
    dimi: :undefined,
    demi: :undefined
  )

  Record.defrecord(:r_winInfo, :winInfo,
    window: :undefined,
    size: :undefined,
    find: :undefined,
    m_szr: :undefined,
    e_szr: :undefined,
    code: :undefined,
    sb: :undefined,
    sg: :undefined,
    bs: :undefined,
    eval: :undefined,
    bind: :undefined,
    trace: :undefined,
    marked_line: 0,
    selected_line: 0,
    breaks: [],
    editor: :undefined,
    editors: []
  )

  Record.defrecord(:r_sub, :sub,
    enable: true,
    win: :undefined,
    in: :undefined,
    out: :undefined,
    name: :undefined
  )

  Record.defrecord(:r_sa, :sa, search: :undefined, goto: :undefined, radio: :undefined)
  Record.defrecord(:r_find, :find, start: :undefined, strlen: :undefined, found: :undefined)

  def init() do
    _ = :dbg_wx_win.init()
    :ok
  end

  def stop(r_winInfo(window: win)) do
    try do
      :wxFrame.destroy(win)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def create_win(parent, title, windows, menus) do
    do__ = fn ->
      win = :wxFrame.new(parent, -1, :dbg_wx_win.to_string(title), [{:size, {700, 650}}])
      panel = :wxPanel.new(win, [{:size, {700, 650}}])
      menuBar = :wxMenuBar.new()
      :dbg_wx_win.create_menus(menuBar, menus, win, 1)
      :wxFrame.setMenuBar(win, menuBar)
      sizer = :wxBoxSizer.new(8)
      code = code_area(panel)

      _ =
        :wxSizer.add(sizer, r_sub(code, :win), [
          {:proportion, 1},
          {:border, 2},
          {:flag, 8192 ||| 128}
        ])

      :wxSizer.setVirtualSizeHints(sizer, r_sub(code, :win))
      expandWithBorder = [{:border, 3}, {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)}]
      search = search_area(panel)
      _ = :wxSizer.add(sizer, r_sub(search, :win), expandWithBorder)
      bs = button_area(panel)
      _ = :wxSizer.add(sizer, r_sub(bs, :win), expandWithBorder)
      infoArea = :wxBoxSizer.new(4)
      :wxSizer.setMinSize(infoArea, {100, 200})
      eval = eval_area(panel)
      _ = :wxSizer.add(infoArea, r_sub(eval, :win), [{:proportion, 1}, {:flag, 8192}])
      bind = bind_area(panel)

      _ =
        :wxSizer.add(infoArea, r_sub(bind, :win), [
          {:proportion, 1},
          {:border, 2},
          {:flag, 8192 ||| 16}
        ])

      _ = :wxSizer.add(sizer, infoArea, expandWithBorder)
      trace = trace_area(panel)
      _ = :wxSizer.add(sizer, r_sub(trace, :win), expandWithBorder)
      sB = :wxFrame.createStatusBar(win, [])
      :wxFrame.connect(win, :sash_dragged, [{:id, 425}, {:lastId, 427}])
      :wxFrame.connect(win, :close_window, [{:skip, true}])
      :wxFrame.connect(win, :size, [{:skip, true}])
      :wxWindow.connect(win, :key_up, [{:skip, true}])
      :wxWindow.setFocus(r_sub(code, :out))

      wi0 =
        r_winInfo(
          window: win,
          m_szr: {panel, sizer},
          e_szr: {true, infoArea},
          code: code,
          sb: sB,
          sg: search,
          bs: bs,
          eval: eval,
          trace: trace,
          bind: bind,
          editor: {:"$top", r_sub(code, :out)},
          editors: [{:"$top", r_sub(code, :out)}]
        )

      wi = show_windows(enable_windows(wi0, windows))
      :wxWindow.setSizer(panel, sizer)
      _ = :wxSizer.fit(sizer, win)
      :wxSizer.setSizeHints(sizer, win)
      iconFile = :dbg_wx_win.find_icon('erlang_bug.png')
      icon = :wxIcon.new(iconFile, [{:type, 2 + 13}])
      :wxFrame.setIcon(win, icon)
      :wxIcon.destroy(icon)
      :wxFrame.show(win)
      :erlang.put(:window, win)
      :erlang.put(:strings, [:str_on])
      wi
    end

    try do
      :wx.batch(do__)
    catch
      e, r ->
        :io.format('Crashed ~p ~p', [e, r])
        :erlang.error(e)
    end
  end

  def get_window(winInfo) do
    r_winInfo(winInfo, :window)
  end

  defp configure(wi = r_winInfo(window: win, m_szr: {panel, sizer})) do
    :wx.batch(fn ->
      _ = show_windows(wi)
      :wxSizer.layout(sizer)
      :wxWindow.setSizer(panel, sizer)
      _ = :wxSizer.fit(sizer, win)
      :wxSizer.setSizeHints(sizer, win)
      wi
    end)
  end

  def configure(
        wi0 = r_winInfo(window: win, m_szr: {panel, sizer}),
        windows
      ) do
    :wx.batch(fn ->
      wi = enable_windows(wi0, windows)
      _ = show_windows(wi)
      :wxSizer.layout(sizer)
      :wxWindow.setSizer(panel, sizer)
      _ = :wxSizer.fit(sizer, win)
      :wxSizer.setSizeHints(sizer, win)
      wi
    end)
  end

  defp enable_windows(
         wi =
           r_winInfo(
             e_szr: {_, infoArea},
             bs: bs0,
             sg: sG0,
             eval: eval0,
             trace: trace0,
             bind: bind0
           ),
         windows
       ) do
    subs =
      for window <- [sG0, bs0, eval0, trace0, bind0] do
        r_sub(window,
          enable:
            :lists.member(
              r_sub(window, :name),
              windows
            )
        )
      end

    [sG, bs, eval, trace, bind] = subs
    eSzr = r_sub(eval, :enable) or r_sub(bind, :enable)
    r_winInfo(wi, e_szr: {eSzr, infoArea}, sg: sG, bs: bs, eval: eval, trace: trace, bind: bind)
  end

  defp show_windows(
         wi =
           r_winInfo(
             m_szr: {_, sizer},
             e_szr: {_, infoArea},
             bs: bs,
             sg: sG,
             eval: eval,
             trace: trace,
             bind: bind
           )
       ) do
    case r_sub(sG, :enable) do
      false ->
        :wxSizer.hide(sizer, r_sub(sG, :win))

      _ ->
        :wxSizer.show(sizer, r_sub(sG, :win))
    end

    case r_sub(bs, :enable) do
      false ->
        :wxSizer.hide(sizer, r_sub(bs, :win))

      _ ->
        :wxSizer.show(sizer, r_sub(bs, :win))
    end

    cond do
      not r_sub(eval, :enable) and not r_sub(bind, :enable) ->
        :wxSizer.hide(sizer, infoArea)

      not r_sub(eval, :enable) ->
        :wxSizer.show(sizer, infoArea)
        :wxSizer.hide(infoArea, r_sub(eval, :win))
        :wxSizer.show(infoArea, r_sub(bind, :win))

      not r_sub(bind, :enable) ->
        [evalSI | _] = :wxSizer.getChildren(infoArea)
        :wxSizerItem.setProportion(evalSI, 1)
        :wxSizer.show(sizer, infoArea)
        :wxSizer.hide(infoArea, r_sub(bind, :win))
        :wxSizer.show(infoArea, r_sub(eval, :win))
        true

      true ->
        :wxSizer.show(sizer, infoArea)
        :wxSizer.show(infoArea, r_sub(eval, :win))
        :wxSizer.show(infoArea, r_sub(bind, :win))
    end

    case r_sub(trace, :enable) do
      false ->
        :wxSizer.hide(sizer, r_sub(trace, :win))

      _ ->
        :wxSizer.show(sizer, r_sub(trace, :win))
    end

    wi
  end

  def enable(menuItems, bool) do
    :wx.foreach(
      fn menuItem ->
        mI = :erlang.get(menuItem)
        :wxMenuItem.enable(mI, [{:enable, bool}])

        case is_button(menuItem) do
          {true, buttonId} ->
            parent = :erlang.get(:window)

            butt =
              :wxWindow.findWindowById(
                buttonId,
                [{:parent, parent}]
              )

            case :wx.is_null(butt) do
              true ->
                :ignore

              false ->
                :wxButton.enable(butt, [{:enable, bool}])
            end

          _ ->
            :ignore
        end
      end,
      menuItems
    )
  end

  def is_enabled(menuItem) do
    mI = :erlang.get(menuItem)
    :wxMenuItem.isEnabled(mI)
  end

  def select(menuItem, bool) do
    mI = :erlang.get(menuItem)
    :wxMenuItem.check(mI, [{:check, bool}])
  end

  def add_break(winInfo, menu, {{mod, line}, [status | _Options]} = break) do
    case r_winInfo(winInfo, :editor) do
      {^mod, editor} ->
        :dbg_wx_code.add_break_to_code(editor, line, status)

      _ ->
        :ok
    end

    add_break_to_menu(winInfo, menu, break)
  end

  defp add_break_to_menu(winInfo, menu, {point, [status | _Options] = options}) do
    break = :dbg_wx_win.add_break(r_winInfo(winInfo, :window), menu, point)
    :dbg_wx_win.update_break(break, options)
    breakInfo = r_breakInfo(point: point, status: status, break: break)
    r_winInfo(winInfo, breaks: [breakInfo | r_winInfo(winInfo, :breaks)])
  end

  def update_break(
        winInfo,
        {{mod, line}, [status | _Options]} = break
      ) do
    case r_winInfo(winInfo, :editor) do
      {^mod, editor} ->
        :dbg_wx_code.add_break_to_code(editor, line, status)

      _ ->
        :ok
    end

    update_break_in_menu(winInfo, break)
  end

  defp update_break_in_menu(
         winInfo,
         {point, [status | _Options] = options}
       ) do
    {:value, breakInfo} =
      :lists.keysearch(point, r_breakInfo(:point), r_winInfo(winInfo, :breaks))

    :dbg_wx_win.update_break(r_breakInfo(breakInfo, :break), options)
    breakInfo2 = r_breakInfo(breakInfo, status: status)

    r_winInfo(winInfo,
      breaks:
        :lists.keyreplace(point, r_breakInfo(:point), r_winInfo(winInfo, :breaks), breakInfo2)
    )
  end

  def delete_break(winInfo, {mod, line} = point) do
    case r_winInfo(winInfo, :editor) do
      {^mod, editor} ->
        :dbg_wx_code.del_break_from_code(editor, line)

      _ ->
        :ignore
    end

    delete_break_from_menu(winInfo, point)
  end

  defp delete_break_from_menu(winInfo, point) do
    {:value, breakInfo} =
      :lists.keysearch(point, r_breakInfo(:point), r_winInfo(winInfo, :breaks))

    :dbg_wx_win.delete_break(r_breakInfo(breakInfo, :break))

    r_winInfo(winInfo,
      breaks: :lists.keydelete(point, r_breakInfo(:point), r_winInfo(winInfo, :breaks))
    )
  end

  def clear_breaks(winInfo) do
    clear_breaks(winInfo, :all)
  end

  def clear_breaks(winInfo, mod) do
    remove =
      cond do
        mod === :all ->
          r_winInfo(winInfo, :breaks)

        true ->
          :lists.filter(
            fn r_breakInfo(point: {mod2, _L}) ->
              cond do
                mod2 === mod ->
                  true

                true ->
                  false
              end
            end,
            r_winInfo(winInfo, :breaks)
          )
      end

    :lists.foreach(
      fn r_breakInfo(point: point) ->
        delete_break(winInfo, point)
      end,
      remove
    )

    remain = r_winInfo(winInfo, :breaks) -- remove
    r_winInfo(winInfo, breaks: remain)
  end

  def display(r_winInfo(window: win, sb: sb), arg) do
    str =
      case arg do
        :idle ->
          'State: uninterpreted'

        {:exit, {mod, line}, reason} ->
          :wxWindow.raise(win)
          :dbg_wx_win.to_string('State: EXITED [~w.erl/~w], Reason:~w', [mod, line, reason])

        {:exit, :null, reason} ->
          :wxWindow.raise(win)
          :dbg_wx_win.to_string('State: EXITED [uninterpreted], Reason:~w', [reason])

        {level, :null, _Line} when is_integer(level) ->
          :dbg_wx_win.to_string('*** Call level #~w (in non-interpreted code)', [level])

        {level, mod, line} when is_integer(level) ->
          :dbg_wx_win.to_string('*** Call level #~w [~w.erl/~w]', [level, mod, line])

        {status, mod, line} ->
          what =
            case status do
              :wait ->
                :receive

              _ ->
                status
            end

          :dbg_wx_win.to_string('State: ~w [~w.erl/~w]', [what, mod, line])

        {:running, mod} ->
          :dbg_wx_win.to_string('State: running [~w.erl]', [mod])

        {:text, text} ->
          :dbg_wx_win.to_string(text)
      end

    :wxStatusBar.setStatusText(sb, str)
  end

  def is_shown(_WinInfo, _Mod) do
    false
  end

  def show_code(winInfo = r_winInfo(editor: {_, ed}), mod, contents) do
    :dbg_wx_code.load_code(ed, contents)

    :lists.foreach(
      fn breakInfo ->
        case r_breakInfo(breakInfo, :point) do
          {mod2, line} when mod2 === mod ->
            status = r_breakInfo(breakInfo, :status)
            :dbg_wx_code.add_break_to_code(ed, line, status)

          _Point ->
            :ignore
        end
      end,
      r_winInfo(winInfo, :breaks)
    )

    r_winInfo(winInfo, editor: {mod, ed}, find: :undefined)
  end

  def show_no_code(winInfo = r_winInfo(editor: {_, ed})) do
    :dbg_wx_code.unload_code(ed)
    r_winInfo(winInfo, editor: {:"$top", ed})
  end

  def remove_code(winInfo, _Mod) do
    winInfo
  end

  def mark_line(winInfo = r_winInfo(editor: {_, ed}), line, _How) do
    :dbg_wx_code.mark_line(ed, r_winInfo(winInfo, :marked_line), line)
    r_winInfo(winInfo, marked_line: line)
  end

  def unmark_line(winInfo) do
    mark_line(winInfo, 0, false)
  end

  def select_line(winInfo, line) do
    {_Mod, ed} = r_winInfo(winInfo, :editor)
    size = :dbg_wx_code.get_no_lines(ed)

    cond do
      line === 0 ->
        :dbg_wx_code.goto_line(ed, 1)
        r_winInfo(winInfo, selected_line: 0)

      line < size ->
        :dbg_wx_code.goto_line(ed, line)
        r_winInfo(winInfo, selected_line: line)

      true ->
        winInfo
    end
  end

  def selected_line(r_winInfo(editor: {_, ed})) do
    :wxStyledTextCtrl.getCurrentLine(ed) + 1
  end

  def eval_output(r_winInfo(eval: r_sub(out: log)), text, _Face) do
    :wxTextCtrl.appendText(log, :dbg_wx_win.to_string(text))
    :ok
  end

  def update_bindings(r_winInfo(bind: r_sub(out: bA)), bs) do
    :wxListCtrl.deleteAllItems(bA)

    :wx.foldl(
      fn {var, val}, row ->
        :wxListCtrl.insertItem(bA, row, '')
        :wxListCtrl.setItem(bA, row, 0, :dbg_wx_win.to_string(var))

        format =
          case :erlang.get(:strings) do
            [] ->
              '~0ltP'

            [:str_on] ->
              '~0tP'
          end

        :wxListCtrl.setItem(
          bA,
          row,
          1,
          :dbg_wx_win.to_string(
            format,
            [val, 20]
          )
        )

        row + 1
      end,
      0,
      bs
    )

    :erlang.put(:bindings, bs)
    :ok
  end

  def update_strings(strings) do
    _ = :erlang.put(:strings, strings)
    :ok
  end

  def trace_output(r_winInfo(trace: r_sub(out: log)), text) do
    :wxTextCtrl.appendText(log, :dbg_wx_win.to_string(text))
    :ok
  end

  def handle_event(_Ev = r_wx(event: r_wxClose()), _WinInfo) do
    :stopped
  end

  def handle_event(r_wx(event: r_wxSize(size: size)), wi0) do
    wi = r_winInfo(wi0, size: size)
    resize(wi)
    {:win, wi}
  end

  def handle_event(r_wx(event: r_wxSash(dragStatus: 1)), _Wi) do
    :ignore
  end

  def handle_event(
        r_wx(id: 425, event: r_wxSash(dragRect: {_X, _Y, _W, h})),
        wi
      ) do
    r_winInfo(code: code, m_szr: {_, sizer}, e_szr: {enable, infoSzr}, trace: trace) = wi

    case enable or r_sub(trace, :enable) do
      false ->
        :ignore

      true ->
        {_, cMH} = :wxWindow.getMinSize(r_sub(code, :win))

        case cMH > h do
          true ->
            :wxSashWindow.setMinSize(r_sub(code, :win), {500, h})

          _ ->
            :ignore
        end

        {_, cH} = :wxWindow.getSize(r_sub(code, :win))
        change = cH - h

        changeH = fn item ->
          {itemW, itemH} = :wxSizerItem.getMinSize(item)

          :wxSizerItem.setInitSize(
            item,
            itemW,
            :erlang.max(
              itemH + change,
              -1
            )
          )
        end

        cond do
          enable ->
            {iW, iH} = :wxSizer.getMinSize(infoSzr)

            for child <- :wxSizer.getChildren(infoSzr) do
              changeH.(child)
            end

            :wxSizer.setMinSize(infoSzr, {iW, iH + change})
            :ok

          r_sub(trace, :enable) ->
            {tW, tH} = :wxWindow.getMinSize(r_sub(trace, :win))
            :wxWindow.setMinSize(r_sub(trace, :win), {tW, tH + change})
            :ok
        end

        :wxSizer.layout(sizer)
        :ignore
    end
  end

  def handle_event(
        r_wx(id: 426, event: r_wxSash(dragRect: {_X, _Y, w, _H})),
        wi
      ) do
    r_winInfo(
      m_szr: {_, sizer},
      e_szr: {enable, infoSzr},
      eval: r_sub(enable: enable, win: evalSzr)
    ) = wi

    case enable do
      false ->
        :ignore

      true ->
        [eval, bind] = :wxSizer.getChildren(infoSzr)
        {tot, _} = :wxSizer.getSize(infoSzr)
        evalWidth = tot - w

        change = fn szr, width ->
          {_EW, eH} = :wxSizerItem.getMinSize(szr)
          :wxSizerItem.setInitSize(szr, width, eH)
        end

        change.(eval, evalWidth)

        for kid <- :wxSizer.getChildren(evalSzr) do
          change.(kid, evalWidth)
        end

        change.(bind, w)
        :wxSizerItem.setProportion(eval, 0)
        :wxSizer.layout(infoSzr)
        :wxSizer.layout(sizer)
        resize(wi)
        :ignore
    end
  end

  def handle_event(
        r_wx(id: 427, event: r_wxSash(dragRect: {_X, _Y, _W, h})),
        wi
      ) do
    r_winInfo(code: code, m_szr: {_, sizer}, e_szr: {enable, infoSzr}, trace: trace) = wi
    {tW, tH} = :wxWindow.getSize(r_sub(trace, :win))
    change = tH - h

    case enable do
      false ->
        {_, cH} = :wxWindow.getSize(r_sub(code, :win))
        {_, cMH} = :wxWindow.getMinSize(r_sub(code, :win))

        case cMH > cH + change do
          true ->
            :wxSashWindow.setMinSize(
              r_sub(code, :win),
              {500, cH + change}
            )

          _ ->
            :ignore
        end

        :wxWindow.setMinSize(r_sub(trace, :win), {tW, h})
        :wxSizer.layout(sizer)
        :ignore

      true ->
        changeH = fn item ->
          {itemW, itemH} = :wxSizerItem.getMinSize(item)

          :wxSizerItem.setInitSize(
            item,
            itemW,
            :erlang.max(
              itemH + change,
              -1
            )
          )
        end

        {iW, iH} = :wxSizer.getMinSize(infoSzr)

        for child <- :wxSizer.getChildren(infoSzr) do
          changeH.(child)
        end

        wanted = iH + change
        :wxSizer.setMinSize(infoSzr, {iW, wanted})
        {_, rH} = :wxSizer.getMinSize(infoSzr)

        case rH > wanted do
          true ->
            {_, cH} = :wxWindow.getSize(r_sub(code, :win))
            {_, cMH} = :wxWindow.getMinSize(r_sub(code, :win))
            cC = cH - (rH - wanted)

            case cMH > cC do
              true when cC > 50 ->
                :wxWindow.setMinSize(r_sub(trace, :win), {tW, h})
                :wxSashWindow.setMinSize(r_sub(code, :win), {500, cC})

              _ when cC < 50 ->
                :ignore

              _ ->
                :wxWindow.setMinSize(r_sub(trace, :win), {tW, h})
            end

            :ok

          false ->
            :wxWindow.setMinSize(r_sub(trace, :win), {tW, h})
        end

        :wxSizer.layout(sizer)
        :ignore
    end
  end

  def handle_event(
        _Ev =
          r_wx(
            event:
              r_wxKey(
                keyCode: key,
                controlDown: true
              )
          ),
        _WinInfo
      ) do
    cond do
      key != 315 and key != 317 and key != 13 ->
        try do
          {:shortcut, :erlang.list_to_atom([key + (?a - ?A)])}
        catch
          _, _ ->
            :ignore
        end

      true ->
        :ignore
    end
  end

  def handle_event(
        r_wx(
          userData: {:dbg_ui_winman, win},
          event: r_wxCommand(type: :command_menu_selected)
        ),
        _WinInfo
      ) do
    :dbg_wx_winman.raise(win)
    :ignore
  end

  def handle_event(
        r_wx(
          userData: {:break, point, :status},
          event: r_wxCommand(type: :command_menu_selected)
        ),
        winInfo
      ) do
    {:value, breakInfo} =
      :lists.keysearch(point, r_breakInfo(:point), r_winInfo(winInfo, :breaks))

    r_breakInfo(break: r_break(smi: smi)) = breakInfo

    case :wxMenuItem.getText(smi) do
      'Enable' ->
        {:break, point, {:status, :active}}

      'Disable' ->
        {:break, point, {:status, :inactive}}
    end
  end

  def handle_event(
        r_wx(
          userData: data,
          event: _Cmd = r_wxCommand(type: :command_menu_selected)
        ),
        _WinInfo
      ) do
    data
  end

  def handle_event(
        r_wx(event: r_wxStyledText(type: :stc_doubleclick)),
        winInfo = r_winInfo(editor: {mod, ed})
      ) do
    line = :wxStyledTextCtrl.getCurrentLine(ed)
    point = {mod, line + 1}

    case :lists.keymember(point, r_breakInfo(:point), r_winInfo(winInfo, :breaks)) do
      true ->
        {:break, point, :delete}

      false ->
        {:break, point, :add}
    end
  end

  def handle_event(r_wx(id: 414, event: r_wxCommand(cmdString: str)), winInfo) do
    try do
      line = :erlang.list_to_integer(str)
      {:gotoline, line}
    catch
      _, _ ->
        display(winInfo, {:text, 'Not a line number'})
        :ignore
    end
  end

  def handle_event(r_wx(id: 413, event: r_wxFocus()), wi) do
    {:win, r_winInfo(wi, find: :undefined)}
  end

  def handle_event(
        r_wx(
          id: 413,
          event: r_wxCommand(type: :command_text_enter, cmdString: str)
        ),
        wi = r_winInfo(code: code, find: find, sg: r_sub(in: r_sa(radio: {nextO, _, caseO})))
      )
      when find !== :undefined do
    dir =
      :erlang.xor(
        :wxRadioButton.getValue(nextO),
        :wx_misc.getKeyState(306)
      )

    case__ = :wxCheckBox.getValue(caseO)

    pos =
      cond do
        r_find(find, :found) and dir ->
          :wxStyledTextCtrl.getAnchor(r_sub(code, :out))

        r_find(find, :found) ->
          :wxStyledTextCtrl.getCurrentPos(r_sub(code, :out))

        dir ->
          0

        true ->
          :wxStyledTextCtrl.getLength(r_sub(code, :out))
      end

    :dbg_wx_code.goto_pos(r_sub(code, :out), pos)

    case :dbg_wx_code.find(r_sub(code, :out), str, case__, dir) do
      true ->
        display(wi, {:text, ''})
        {:win, r_winInfo(wi, find: r_find(find, found: true))}

      false ->
        display(wi, {:text, 'Not found (Hit Enter to wrap search)'})
        {:win, r_winInfo(wi, find: r_find(find, found: false))}
    end
  end

  def handle_event(
        r_wx(id: 413, event: r_wxCommand(cmdString: '')),
        wi = r_winInfo(code: code)
      ) do
    pos = :dbg_wx_code.current_pos(r_sub(code, :out))
    :dbg_wx_code.goto_pos(r_sub(code, :out), pos)
    {:win, r_winInfo(wi, find: :undefined)}
  end

  def handle_event(
        r_wx(id: 413, event: r_wxCommand(cmdString: str)),
        wi = r_winInfo(code: code, find: find, sg: r_sub(in: r_sa(radio: {nextO, _, caseO})))
      ) do
    dir = :wxRadioButton.getValue(nextO)
    case__ = :wxCheckBox.getValue(caseO)

    cont =
      case find do
        :undefined ->
          pos = :dbg_wx_code.current_pos(r_sub(code, :out))
          r_find(start: pos, strlen: length(str))

        r_find(strlen: old) when old < length(str) ->
          r_find(find, strlen: length(str))

        _ ->
          :dbg_wx_code.goto_pos(r_sub(code, :out), r_find(find, :start))
          r_find(find, strlen: length(str))
      end

    case :dbg_wx_code.find(r_sub(code, :out), str, case__, dir) do
      true ->
        display(wi, {:text, ''})
        {:win, r_winInfo(wi, find: r_find(cont, found: true))}

      false ->
        display(wi, {:text, 'Not found (Hit Enter to wrap search)'})
        {:win, r_winInfo(wi, find: r_find(cont, found: false))}
    end
  end

  def handle_event(
        r_wx(
          id: iD,
          event: r_wxCommand(type: :command_button_clicked)
        ),
        _Wi
      ) do
    {button, _} = :lists.keyfind(iD, 2, buttons())
    button
  end

  def handle_event(
        r_wx(id: 410, event: r_wxCommand(type: :command_text_enter)),
        wi = r_winInfo(eval: r_sub(in: tC))
      ) do
    case :wxTextCtrl.getValue(tC) do
      [10] ->
        eval_output(wi, '\n', :normal)
        :ignore

      cmd ->
        eval_output(wi, [?>, cmd, 10], :normal)
        :wxTextCtrl.setValue(tC, '')
        {:user_command, cmd}
    end
  end

  def handle_event(
        r_wx(
          event:
            r_wxList(
              type: :command_list_item_selected,
              itemIndex: row
            )
        ),
        wi
      ) do
    bs = :erlang.get(:bindings)
    {var, val} = :lists.nth(row + 1, bs)

    str =
      case :erlang.get(:strings) do
        [] ->
          :io_lib.format('< ~s = ~ltp~n', [var, val])

        [:str_on] ->
          :io_lib.format('< ~s = ~tp~n', [var, val])
      end

    eval_output(wi, str, :bold)
    :ignore
  end

  def handle_event(
        r_wx(
          event:
            r_wxList(
              type: :command_list_item_activated,
              itemIndex: row
            )
        ),
        _Wi
      ) do
    bs = :erlang.get(:bindings)
    binding = :lists.nth(row + 1, bs)
    {:edit, binding}
  end

  def handle_event(_GSEvent, _WinInfo) do
    :ignore
  end

  defp resize(r_winInfo(bind: bind)) do
    cond do
      r_sub(bind, :enable) === false ->
        :ok

      r_sub(bind, :enable) ->
        {eW, _} = :wxWindow.getClientSize(r_sub(bind, :out))
        b0W = :wxListCtrl.getColumnWidth(r_sub(bind, :out), 0)
        :wxListCtrl.setColumnWidth(r_sub(bind, :out), 1, eW - b0W)
        :ok
    end
  end

  defp code_area(win) do
    codeWin =
      :wxSashWindow.new(
        win,
        [{:id, 425}, {:size, {700, 400}}, {:style, 64 ||| 128}]
      )

    code = :dbg_wx_code.code_area(codeWin)
    :wxSashWindow.setSashVisible(codeWin, 2, true)
    :wxWindow.setMinSize(codeWin, {600, 400})
    r_sub(name: :"Code Area", enable: true, win: codeWin, out: code)
  end

  defp buttons() do
    [
      {:Step, 401},
      {:Next, 402},
      {:Continue, 403},
      {:Finish, 404},
      {:Where, 405},
      {:Up, 406},
      {:Down, 407}
    ]
  end

  defp is_button(name) do
    case :lists.keyfind(name, 1, buttons()) do
      {^name, button} ->
        {true, button}

      false ->
        false
    end
  end

  defp button_area(parent) do
    sz = :wxBoxSizer.new(4)

    :wx.foreach(
      fn {name, button} ->
        b = :wxButton.new(parent, button, [{:label, :dbg_wx_win.to_string(name)}])
        id = :wxWindow.getId(b)
        _ = :wxSizer.add(sz, b, [])
        :wxButton.connect(b, :command_button_clicked, [{:id, id}])
      end,
      buttons()
    )

    r_sub(name: :"Button Area", win: sz)
  end

  defp search_area(parent) do
    hSz = :wxBoxSizer.new(4)
    _ = :wxSizer.add(hSz, :wxStaticText.new(parent, -1, 'Find:'), [{:flag, 2048}])
    tC1 = :wxTextCtrl.new(parent, 413, [{:style, 1024}])
    _ = :wxSizer.add(hSz, tC1, [{:proportion, 3}, {:flag, 8192}])
    nbtn = :wxRadioButton.new(parent, -1, 'Next')
    :wxRadioButton.setValue(nbtn, true)
    _ = :wxSizer.add(hSz, nbtn, [{:flag, 2048}])
    pbtn = :wxRadioButton.new(parent, -1, 'Previous')
    _ = :wxSizer.add(hSz, pbtn, [{:flag, 2048}])
    cbtn = :wxCheckBox.new(parent, -1, 'Match Case')
    _ = :wxSizer.add(hSz, cbtn, [{:flag, 2048}])
    _ = :wxSizer.add(hSz, 15, 15, [{:proportion, 1}, {:flag, 8192}])
    _ = :wxSizer.add(hSz, :wxStaticText.new(parent, -1, 'Goto Line:'), [{:flag, 2048}])
    tC2 = :wxTextCtrl.new(parent, 414, [{:style, 1024}])
    _ = :wxSizer.add(hSz, tC2, [{:proportion, 0}, {:flag, 8192}])
    :wxTextCtrl.connect(tC1, :command_text_updated)
    :wxTextCtrl.connect(tC1, :command_text_enter)
    :wxTextCtrl.connect(tC1, :kill_focus)
    :wxTextCtrl.connect(tC2, :command_text_enter)
    :wxWindow.connect(parent, :command_button_clicked)

    r_sub(
      name: :"Search Area",
      win: hSz,
      in: r_sa(search: tC1, goto: tC2, radio: {nbtn, pbtn, cbtn})
    )
  end

  defp eval_area(parent) do
    vSz = :wxBoxSizer.new(8)
    hSz = :wxBoxSizer.new(4)
    _ = :wxSizer.add(hSz, :wxStaticText.new(parent, -1, 'Evaluator:'), [{:flag, 2048}])
    tC = :wxTextCtrl.new(parent, 410, [{:style, 1024}])
    _ = :wxSizer.add(hSz, tC, [{:proportion, 1}, {:flag, 8192}])
    _ = :wxSizer.add(vSz, hSz, [{:flag, 8192}])
    tL = :wxTextCtrl.new(parent, 411, [{:style, 1_073_741_824 ||| 32 ||| 16}])
    _ = :wxSizer.add(vSz, tL, [{:proportion, 5}, {:flag, 8192}])
    :wxTextCtrl.connect(tC, :command_text_enter)
    r_sub(name: :"Evaluator Area", win: vSz, in: tC, out: tL)
  end

  defp bind_area(parent) do
    style = {:style, 64 ||| 128 ||| 4_194_304}
    win = :wxSashWindow.new(parent, [{:id, 426}, style])
    :wxSashWindow.setSashVisible(win, 3, true)
    bA = :wxListCtrl.new(win, [{:style, 32 ||| 8192}])
    lI = :wxListItem.new()
    :wxListItem.setText(lI, 'Name')
    :wxListItem.setAlign(lI, 0)
    :wxListCtrl.insertColumn(bA, 0, lI)
    :wxListItem.setText(lI, 'Value')
    :wxListCtrl.insertColumn(bA, 1, lI)
    :wxListItem.destroy(lI)
    :wxListCtrl.setColumnWidth(bA, 0, 100)
    :wxListCtrl.setColumnWidth(bA, 1, 150)
    :wxListCtrl.connect(bA, :command_list_item_selected)
    :wxListCtrl.connect(bA, :command_list_item_activated)
    r_sub(name: :"Bindings Area", win: win, out: bA)
  end

  defp trace_area(parent) do
    style = {:style, 64 ||| 128 ||| 4_194_304}

    win =
      :wxSashWindow.new(
        parent,
        [{:id, 427}, {:size, {700, 100}}, style]
      )

    :wxSashWindow.setSashVisible(win, 0, true)
    :wxWindow.setMinSize(win, {500, 100})
    tC = :wxTextCtrl.new(win, -1, [{:style, 32 ||| 16}])
    r_sub(name: :"Trace Area", win: win, out: tC)
  end

  def helpwin(type, winInfo = r_winInfo(sg: sg = r_sub(in: sa))) do
    wi =
      case r_sub(sg, :enable) do
        false ->
          configure(r_winInfo(winInfo, sg: r_sub(sg, enable: true)))

        true ->
          winInfo
      end

    case type do
      :gotoline ->
        :wxWindow.setFocus(r_sa(sa, :goto))

      :search ->
        :wxWindow.setFocus(r_sa(sa, :search))
    end

    wi
  end
end
