defmodule :m_dbg_wx_mon_win do
  use Bitwise
  import Kernel, except: [to_string: 1]
  import :dbg_wx_win, only: [to_string: 1, to_string: 2]
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

  Record.defrecord(:r_moduleInfo, :moduleInfo,
    module: :undefined,
    menubtn: :undefined
  )

  Record.defrecord(:r_procInfo, :procInfo,
    pid: :undefined,
    row: :undefined
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
    grid: :undefined,
    row: :undefined,
    focus: :undefined,
    modules: [],
    processes: [],
    breaks: [],
    listbox: :undefined,
    fbutton: :undefined,
    bbutton: :undefined,
    ebutton: :undefined,
    selected: [],
    stringsbutton: :undefined,
    slabel: :undefined,
    blabel: :undefined
  )

  def init() do
    :dbg_wx_win.init()
  end

  def create_win(_Wx, title, menus) do
    :wx.batch(fn ->
      create_win_batch(title, menus)
    end)
  end

  defp create_win_batch(title, menus) do
    win = :wxFrame.new(:wx.null(), -1, title, [{:size, {800, 390}}])
    :wxFrame.connect(win, :close_window, [{:skip, true}])
    menuBar = :wxMenuBar.new()
    :dbg_wx_win.create_menus(menuBar, menus, win, 1)
    :wxFrame.setMenuBar(win, menuBar)
    mainSz = :wxBoxSizer.new(4)
    leftSz = :wxBoxSizer.new(8)
    panel = :wxPanel.new(win)
    hlb = 200
    listbox = :wxListBox.new(panel, -1, [{:size, {150, hlb}}, {:style, 32}])
    _ = :wxSizer.add(leftSz, listbox, [{:proportion, 1}, {:border, 3}, {:flag, 8192}])

    :wxListBox.connect(
      listbox,
      :command_listbox_doubleclicked
    )

    :wxListBox.connect(listbox, :right_down)
    sBox = :wxStaticBox.new(panel, -1, 'Auto Attach:')
    sBS = :wxStaticBoxSizer.new(sBox, 8)
    fbtn = :wxCheckBox.new(panel, 314, 'First Call')
    _ = :wxSizer.add(sBS, fbtn)
    bbtn = :wxCheckBox.new(panel, 314, 'On Break')
    _ = :wxSizer.add(sBS, bbtn)
    ebtn = :wxCheckBox.new(panel, 314, 'On Exit')
    _ = :wxSizer.add(sBS, ebtn)
    :wxFrame.connect(panel, :command_checkbox_clicked)
    _ = :wxSizer.add(leftSz, sBS, [{:flag, 8192}])
    sLabel = :wxStaticText.new(panel, -1, 'Stack Trace:\n On (with tail)')
    _ = :wxSizer.add(leftSz, sLabel)
    bLabel = :wxStaticText.new(panel, -1, 'Back Trace Size:\n 50000')
    _ = :wxSizer.add(leftSz, bLabel)
    stringsBox = :wxStaticBox.new(panel, -1, 'Strings:')
    stringsBS = :wxStaticBoxSizer.new(stringsBox, 8)
    stringsbtn = :wxCheckBox.new(panel, 271, 'Use range of +pc flag')
    _ = :wxSizer.add(stringsBS, stringsbtn)
    _ = :wxSizer.add(leftSz, stringsBS, [{:flag, 8192}])

    grid =
      :wxListCtrl.new(
        panel,
        [{:winid, 1000}, {:style, 32 ||| 8192 ||| 2}, {:size, {600, -1}}]
      )

    lI = :wxListItem.new()
    :wxListItem.setText(lI, 'Pid')
    :wxListItem.setAlign(lI, 2)
    :wxListCtrl.insertColumn(grid, 0, lI)
    :wxListItem.setText(lI, 'Initial Call')
    :wxListItem.setAlign(lI, 0)
    :wxListCtrl.insertColumn(grid, 1, lI)
    :wxListItem.setText(lI, 'Name')
    :wxListCtrl.insertColumn(grid, 2, lI)
    :wxListItem.setAlign(lI, 2)
    :wxListItem.setText(lI, 'Status')
    :wxListCtrl.insertColumn(grid, 3, lI)
    :wxListItem.setText(lI, 'Information')
    :wxListItem.setAlign(lI, 0)
    :wxListCtrl.insertColumn(grid, 4, lI)
    :wxListItem.destroy(lI)
    :wxListCtrl.setColumnWidth(grid, 0, 80)
    :wxListCtrl.setColumnWidth(grid, 1, 150)
    :wxListCtrl.setColumnWidth(grid, 2, 100)
    :wxListCtrl.setColumnWidth(grid, 3, 70)
    :wxListCtrl.setColumnWidth(grid, 4, 200)
    :wxListCtrl.connect(grid, :command_list_item_activated)
    :wxListCtrl.connect(grid, :command_list_item_selected)
    :wxListCtrl.connect(grid, :size, [{:skip, true}])
    :wxListCtrl.connect(grid, :key_up, [{:id, 1000}, {:skip, true}])
    :wxWindow.connect(win, :enter_window, [{:skip, true}])
    :wxWindow.setFocus(grid)
    _ = :wxSizer.add(mainSz, leftSz, [{:border, 3}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}])

    _ =
      :wxSizer.add(mainSz, grid, [
        {:border, 3},
        {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192},
        {:proportion, 1}
      ])

    :wxWindow.setSizer(panel, mainSz)
    _ = :wxSizer.fit(mainSz, win)
    :wxSizer.setSizeHints(mainSz, win)
    iconFile = :dbg_wx_win.find_icon('erlang_bug.png')
    icon = :wxIcon.new(iconFile, [{:type, 2 + 13}])
    :wxFrame.setIcon(win, icon)
    :wxIcon.destroy(icon)
    :wxFrame.show(win)
    :dbg_wx_winman.raise(win)

    r_winInfo(
      window: win,
      grid: grid,
      row: 0,
      focus: 0,
      listbox: listbox,
      fbutton: fbtn,
      bbutton: bbtn,
      ebutton: ebtn,
      stringsbutton: stringsbtn,
      slabel: sLabel,
      blabel: bLabel
    )
  end

  def get_window(winInfo) do
    r_winInfo(winInfo, :window)
  end

  def show_option(winInfo, option, value) do
    case option do
      :auto_attach ->
        :wx.foreach(
          fn button ->
            :wxCheckBox.setValue(button, false)
          end,
          option_buttons(winInfo, [:init, :break, :exit])
        )

        :wx.foreach(
          fn button ->
            :wxCheckBox.setValue(button, true)
          end,
          option_buttons(winInfo, value)
        )

      :stack_trace ->
        text =
          case value do
            :all ->
              'Stack Trace:\n On (with tail)'

            true ->
              'Stack Trace:\n On (with tail)'

            :no_tail ->
              'Stack Trace:\n On (no tail)'

            false ->
              'Stack Trace:\n Off'
          end

        :wxStaticText.setLabel(r_winInfo(winInfo, :slabel), text)

      :back_trace ->
        text = 'Back Trace Size:\n ' ++ :erlang.integer_to_list(value)
        :wxStaticText.setLabel(r_winInfo(winInfo, :blabel), text)

      :strings ->
        :wx.foreach(
          fn button ->
            :wxCheckBox.setValue(button, false)
          end,
          option_buttons(winInfo, [:str_on])
        )

        :wx.foreach(
          fn button ->
            :wxCheckBox.setValue(button, true)
          end,
          option_buttons(winInfo, value)
        )
    end
  end

  defp option_buttons(winInfo, [:init | flags]) do
    [r_winInfo(winInfo, :fbutton) | option_buttons(winInfo, flags)]
  end

  defp option_buttons(winInfo, [:break | flags]) do
    [r_winInfo(winInfo, :bbutton) | option_buttons(winInfo, flags)]
  end

  defp option_buttons(winInfo, [:exit | flags]) do
    [r_winInfo(winInfo, :ebutton) | option_buttons(winInfo, flags)]
  end

  defp option_buttons(winInfo, [:str_on | flags]) do
    [
      r_winInfo(winInfo, :stringsbutton)
      | option_buttons(
          winInfo,
          flags
        )
    ]
  end

  defp option_buttons(_WinInfo, []) do
    []
  end

  def enable(menuItems, bool) do
    :lists.foreach(
      fn menuItem ->
        mI = :erlang.get(menuItem)
        :wxMenuItem.enable(mI, [{:enable, bool}])
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

  def add_module(winInfo, menuName, mod) do
    win = r_winInfo(winInfo, :window)
    modules = r_winInfo(winInfo, :modules)

    case :lists.keymember(mod, r_moduleInfo(:module), modules) do
      false ->
        menu = :erlang.get(menuName)
        sub = :wxMenu.new([])
        viewItem = :wxMenu.append(sub, -1, 'View')
        viewId = :wxMenuItem.getId(viewItem)

        :wxMenu.connect(win, :command_menu_selected, [
          {:id, viewId},
          {:userData, {:module, mod, :view}}
        ])

        delItem = :wxMenu.append(sub, -1, 'Delete')
        delId = :wxMenuItem.getId(delItem)

        :wxMenu.connect(win, :command_menu_selected, [
          {:id, delId},
          {:userData, {:module, mod, :delete}}
        ])

        menuBtn = :wxMenu.append(menu, -1, :erlang.atom_to_list(mod), sub)

        :wxListBox.append(
          r_winInfo(winInfo, :listbox),
          :erlang.atom_to_list(mod)
        )

        modInfo = r_moduleInfo(module: mod, menubtn: {menu, menuBtn})
        r_winInfo(winInfo, modules: [modInfo | modules])

      true ->
        winInfo
    end
  end

  def delete_module(winInfo, mod) do
    {:value, modInfo} = :lists.keysearch(mod, r_moduleInfo(:module), r_winInfo(winInfo, :modules))
    {menu, menuBtn} = r_moduleInfo(modInfo, :menubtn)
    :wxMenu."Destroy"(menu, menuBtn)
    listBox = r_winInfo(winInfo, :listbox)

    id =
      :wxListBox.findString(
        listBox,
        :erlang.atom_to_list(mod)
      )

    :wxListBox.delete(listBox, id)

    r_winInfo(winInfo,
      modules: :lists.keydelete(mod, r_moduleInfo(:module), r_winInfo(winInfo, :modules))
    )
  end

  def add_process(winInfo, pid, name, {mod, func, args}, status, info) do
    grid = r_winInfo(winInfo, :grid)
    row = r_winInfo(winInfo, :row)

    name2 =
      case name do
        :undefined ->
          ''

        _ ->
          to_string(name)
      end

    funcS = to_string('~w:~tw/~w', [mod, func, length(args)])

    info2 =
      case info do
        {} ->
          ''

        _ ->
          to_string(info)
      end

    pid2 = to_string('~p', [pid])

    add = fn ->
      _Dbg = :wxListCtrl.insertItem(grid, row, '')

      cond do
        rem(row, 2) === 0 ->
          :wxListCtrl.setItemBackgroundColour(grid, row, {240, 240, 255})

        true ->
          :ignore
      end

      :wxListCtrl.setItem(grid, row, 0, pid2)
      :wxListCtrl.setItem(grid, row, 1, funcS)
      :wxListCtrl.setItem(grid, row, 2, name2)
      :wxListCtrl.setItem(grid, row, 3, to_string(status))
      :wxListCtrl.setItem(grid, row, 4, info2)
      :ok
    end

    :wx.batch(add)
    procInfo = r_procInfo(pid: pid, row: row)

    r_winInfo(winInfo,
      processes: [
        procInfo
        | r_winInfo(winInfo, :processes)
      ],
      row: row + 1
    )
  end

  def update_process(winInfo, pid, status, info) do
    {:value, procInfo} = :lists.keysearch(pid, r_procInfo(:pid), r_winInfo(winInfo, :processes))
    grid = r_winInfo(winInfo, :grid)
    row = r_procInfo(procInfo, :row)

    info2 =
      case info do
        {} ->
          ''

        _ ->
          info
      end

    :wxListCtrl.setItem(grid, row, 3, to_string(status))
    :wxListCtrl.setItem(grid, row, 4, to_string(info2))
  end

  def clear_processes(winInfo) do
    grid = r_winInfo(winInfo, :grid)
    max = r_winInfo(winInfo, :row)

    :wx.batch(fn ->
      clear_processes(grid, max - 1)
    end)

    r_winInfo(winInfo, row: 0, focus: 0, processes: [])
  end

  defp clear_processes(grid, row) when row >= 0 do
    item = :wxListItem.new()
    :wxListItem.setId(item, row)
    :wxListItem.setColumn(item, 3)

    case :wxListCtrl.getItem(grid, item) do
      true ->
        case :wxListItem.getText(item) do
          'exit' ->
            :wxListItem.setColumn(item, 0)
            :wxListCtrl.getItem(grid, item)
            pid = :erlang.list_to_pid(:wxListItem.getText(item))
            :dbg_wx_winman.clear_process(:dbg_wx_trace.title(pid))

          _ ->
            :ok
        end

      false ->
        :ignore
    end

    :wxListItem.destroy(item)
    :wxListCtrl.deleteItem(grid, row)
    clear_processes(grid, row - 1)
  end

  defp clear_processes(_Grid, _Row) do
    :done
  end

  def add_break(winInfo, menu, {point, options}) do
    break = :dbg_wx_win.add_break(r_winInfo(winInfo, :window), menu, point)
    :dbg_wx_win.update_break(break, options)
    breakInfo = r_breakInfo(point: point, break: break)
    r_winInfo(winInfo, breaks: [breakInfo | r_winInfo(winInfo, :breaks)])
  end

  def update_break(winInfo, {point, options}) do
    {:value, breakInfo} =
      :lists.keysearch(point, r_breakInfo(:point), r_winInfo(winInfo, :breaks))

    :dbg_wx_win.update_break(r_breakInfo(breakInfo, :break), options)
  end

  def delete_break(winInfo, point) do
    {:value, breakInfo} =
      :lists.keysearch(point, r_breakInfo(:point), r_winInfo(winInfo, :breaks))

    :dbg_wx_win.delete_break(r_breakInfo(breakInfo, :break))

    r_winInfo(winInfo,
      breaks: :lists.keydelete(point, r_breakInfo(:point), r_winInfo(winInfo, :breaks))
    )
  end

  def clear_breaks(winInfo) do
    :lists.foreach(
      fn breakInfo ->
        :dbg_wx_win.delete_break(r_breakInfo(breakInfo, :break))
      end,
      r_winInfo(winInfo, :breaks)
    )

    r_winInfo(winInfo, breaks: [])
  end

  def clear_breaks(winInfo, mod) do
    fun = fn breakInfo ->
      case r_breakInfo(breakInfo, :point) do
        {^mod, _Line} ->
          :dbg_wx_win.delete_break(r_breakInfo(breakInfo, :break))
          false

        _ ->
          true
      end
    end

    breaks = :lists.filter(fun, r_winInfo(winInfo, :breaks))
    r_winInfo(winInfo, breaks: breaks)
  end

  def handle_event(r_wx(event: r_wxSize(size: {w, _})), r_winInfo(grid: grid)) do
    :wx.batch(fn ->
      tot =
        :wx.foldl(
          fn c, sum ->
            sum +
              :wxListCtrl.getColumnWidth(
                grid,
                c
              )
          end,
          0,
          [0, 1, 2, 3]
        )

      :wxListCtrl.setColumnWidth(grid, 4, w - tot - 4)
    end)

    :ignore
  end

  def handle_event(_Ev = r_wx(event: r_wxClose()), _WinInfo) do
    :stopped
  end

  def handle_event(
        r_wx(
          userData: {:dbg_ui_winman, win},
          event: r_wxCommand(type: :command_menu_selected)
        ),
        _Wi
      ) do
    :dbg_wx_winman.raise(win)
    :ignore
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
          event:
            r_wxCommand(
              type: :command_listbox_doubleclicked,
              cmdString: modS
            )
        ),
        _WinInfo
      ) do
    {:module, :erlang.list_to_atom(modS), :view}
  end

  def handle_event(
        r_wx(
          obj: listBox,
          event: r_wxMouse(type: :right_down, x: x, y: y)
        ),
        r_winInfo(listbox: listBox)
      ) do
    case :wxListBox.hitTest(listBox, {x, y}) do
      -1 ->
        :ignore

      row ->
        modS = :wxListBox.getString(listBox, row)
        :io.format('Re-loading/interpreting: ~s~n', [modS])
        :int.i(:erlang.list_to_atom(modS))
        :ignore
    end
  end

  def handle_event(
        r_wx(
          id: 314,
          event: r_wxCommand(type: :command_checkbox_clicked)
        ),
        winInfo
      ) do
    check = fn button, namesAcc ->
      case :wxCheckBox.isChecked(button) do
        true ->
          name = :wxCheckBox.getLabel(button)
          [:erlang.list_to_atom(name) | namesAcc]

        false ->
          namesAcc
      end
    end

    names =
      :wx.foldl(check, [], [
        r_winInfo(winInfo, :ebutton),
        r_winInfo(winInfo, :bbutton),
        r_winInfo(winInfo, :fbutton)
      ])

    {:"Auto Attach", names}
  end

  def handle_event(
        r_wx(
          id: 271,
          event: r_wxCommand(type: :command_checkbox_clicked)
        ),
        winInfo
      ) do
    check = fn button, namesAcc ->
      case :wxCheckBox.isChecked(button) do
        true ->
          name = :wxCheckBox.getLabel(button)
          [:erlang.list_to_atom(name) | namesAcc]

        false ->
          namesAcc
      end
    end

    names = :wx.foldl(check, [], [r_winInfo(winInfo, :stringsbutton)])
    {:Strings, names}
  end

  def handle_event(
        r_wx(
          event:
            r_wxList(
              type: :command_list_item_selected,
              itemIndex: row
            )
        ),
        winInfo
      ) do
    r_winInfo(processes: pids) = winInfo
    r_procInfo(pid: pid) = :lists.keyfind(row, r_procInfo(:row), pids)
    {:focus, pid, r_winInfo(winInfo, focus: row)}
  end

  def handle_event(
        r_wx(event: r_wxList(type: :command_list_item_activated)),
        _WinInfo
      ) do
    :default
  end

  def handle_event(
        r_wx(event: r_wxMouse(type: :enter_window)),
        r_winInfo(grid: grid)
      ) do
    :wxWindow.setFocus(grid)
    :ignore
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

  def handle_event(_Event, _WinInfo) do
    :ignore
  end
end
