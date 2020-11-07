defmodule :m_dbg_wx_break_win do
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

  Record.defrecord(:r_winInfo, :winInfo,
    type: :undefined,
    win: :undefined,
    entries: :undefined,
    trigger: :undefined,
    listbox: :undefined,
    text: :undefined,
    ok: :undefined,
    funcs: []
  )

  def create_win(parent, pos, :function, mod, _Line) do
    win =
      :wxDialog.new(parent, -1, 'Function Break', [
        {:pos, pos},
        {:style, 536_870_912 ||| 2048 ||| 4096 ||| 64}
      ])

    mainS = :wxBoxSizer.new(8)
    label = :wxStaticText.new(win, -1, 'Module:')
    int = :int.interpreted()

    intStrs =
      for m <- int do
        :erlang.atom_to_list(m)
      end

    text = :wxComboBox.new(win, -1, [{:value, :dbg_wx_win.to_string(mod)}, {:choices, intStrs}])
    expand = [{:border, 5}, {:flag, 16 ||| 32 ||| 8192}]
    _ = :wxSizer.add(mainS, label, [{:border, 5}, {:flag, 64 ||| 16 ||| 32}])
    _ = :wxSizer.add(mainS, text, expand)
    funLabel = :wxStaticText.new(win, -1, 'Function:')
    lB = :wxListBox.new(win, -1, [{:size, {-1, 100}}, {:style, 64}])
    _ = :wxSizer.add(mainS, funLabel, expand)
    _ = :wxSizer.add(mainS, lB, [{:proportion, 1} | expand])
    :wxSizer.setMinSize(mainS, 300, 400)
    oK = :wxDialog.createStdDialogButtonSizer(win, 4 ||| 16)
    _ = :wxSizer.add(mainS, oK, [{:border, 5}, {:flag, 64 ||| 128 ||| 32 ||| 16}])
    :wxDialog.setSizer(win, mainS)
    _ = :wxSizer.fit(mainS, win)
    :wxSizer.setSizeHints(mainS, win)
    :wxComboBox.setFocus(text)
    :wxDialog.connect(win, :command_button_clicked)
    :wxComboBox.connect(text, :command_text_updated)
    :wxListBox.connect(lB, :command_listbox_selected)
    :wxListBox.connect(lB, :command_listbox_doubleclicked)
    oKId = :wxDialog.getAffirmativeId(win)

    oKButt =
      :wxWindow.findWindowById(
        oKId,
        [{:parent, win}]
      )

    :wxWindow.disable(oKButt)
    :wxDialog.centreOnParent(win)
    :wxDialog.show(win)

    r_winInfo(
      type: :function,
      win: win,
      text: text,
      ok: oKButt,
      entries: [],
      trigger: :enable,
      listbox: lB,
      funcs: []
    )
  end

  def create_win(parent, pos, type, mod, line) do
    title =
      case type do
        :line ->
          'Line Break'

        :conditional ->
          'Conditional Break'
      end

    style = 536_870_912 ||| 2048 ||| 4096 ||| 64
    win = :wxDialog.new(parent, -1, title, [{:pos, pos}, {:style, style}])
    mainS = :wxBoxSizer.new(8)
    int = :int.interpreted()

    intStrs =
      for m <- int do
        :erlang.atom_to_list(m)
      end

    modT = :wxComboBox.new(win, -1, [{:choices, intStrs}])
    modSz = create_label_of_control(win, 'Module:', modT, mod)
    _ = :wxSizer.add(mainS, modSz, [{:flag, 8192}])

    add = fn {iType, label, def__} ->
      {sz, text} = create_sizer_with_text(win, label, def__)
      _ = :wxSizer.add(mainS, sz, [{:flag, 8192}])
      {text, iType}
    end

    inputs =
      case type do
        :line ->
          [{:integer, 'Line:', line}]

        :conditional ->
          [{:integer, 'Line:', line}, {:atom, 'C-Module:', ''}, {:atom, 'C-Function:', ''}]
      end

    entries = :wx.map(add, inputs)
    {triggerBox, trigger} = create_trigger_box(win)

    _ =
      :wxSizer.add(mainS, triggerBox, [{:border, 5}, {:flag, 64 ||| 128 ||| 32 ||| 16 ||| 8192}])

    _ = :wxSizer.addStretchSpacer(mainS)
    oK = :wxDialog.createStdDialogButtonSizer(win, 4 ||| 16)
    _ = :wxSizer.add(mainS, oK, [{:border, 5}, {:flag, 64 ||| 128 ||| 32 ||| 16}])
    :wxSizer.setMinSize(mainS, 300, -1)
    :wxDialog.setSizer(win, mainS)
    _ = :wxSizer.fit(mainS, win)
    :wxSizer.setSizeHints(mainS, win)
    :wxComboBox.setFocus(modT)
    :wxDialog.connect(win, :command_button_clicked)
    :wxDialog.connect(win, :command_text_updated)
    oKId = :wxDialog.getAffirmativeId(win)
    oKButt = :wxWindow.findWindowById(oKId)
    :wxWindow.disable(oKButt)
    :wxDialog.centreOnParent(win)
    :wxDialog.show(win)
    r_winInfo(type: type, win: win, text: modT, entries: entries, trigger: trigger, ok: oKButt)
  end

  def update_functions(winInfo, funcs) do
    items =
      :lists.map(
        fn [n, a] ->
          :lists.flatten(:io_lib.format('~tw/~w', [n, a]))
        end,
        funcs
      )

    :wxListBox.set(r_winInfo(winInfo, :listbox), items)
    r_winInfo(winInfo, funcs: funcs)
  end

  def handle_event(r_wx(id: 5101), r_winInfo(win: win)) do
    :wxDialog.destroy(win)
    :stopped
  end

  def handle_event(
        r_wx(event: r_wxCommand(type: :command_text_updated)),
        r_winInfo(type: :function, text: text, ok: oK)
      ) do
    module = :wxComboBox.getValue(text)
    :wxWindow.disable(oK)
    {:module, :erlang.list_to_atom(module)}
  end

  def handle_event(
        r_wx(event: r_wxCommand(type: :command_text_updated)),
        r_winInfo(text: text, ok: oK, entries: es)
      ) do
    module = :wxComboBox.getValue(text)

    case check_input(es) do
      :error ->
        :wxWindow.disable(oK)

      _Data when module !== '' ->
        :wxWindow.enable(oK)

      _ ->
        :wxWindow.disable(oK)
    end

    :ignore
  end

  def handle_event(
        r_wx(event: r_wxCommand(type: :command_listbox_selected)),
        r_winInfo(type: :function, listbox: lB, ok: oK)
      ) do
    case :wxListBox.getSelections(lB) do
      {n, _} when n > 0 ->
        :wxWindow.enable(oK)

      _ ->
        :wxWindow.disable(oK)
    end

    :ignore
  end

  def handle_event(
        r_wx(
          id: oKorListBox,
          event: r_wxCommand(type: oKorDoubleClick)
        ),
        r_winInfo(type: :function, win: win, listbox: lB, funcs: funcs, text: text)
      )
      when oKorListBox === 5100 or
             oKorDoubleClick === :command_listbox_doubleclicked do
    mod = :wxComboBox.getValue(text)
    {_, indexL} = :wxListBox.getSelections(lB)

    breaks =
      for index <- indexL do
        [
          :erlang.list_to_atom(mod)
          | :lists.nth(
              index + 1,
              funcs
            )
        ]
      end

    :wxDialog.destroy(win)
    {:break, breaks, :enable}
  end

  def handle_event(
        r_wx(id: 5100),
        r_winInfo(win: win, text: text, entries: es, trigger: trigger)
      ) do
    mod = :wxComboBox.getValue(text)
    data = check_input(es)
    trigged = get_trigger(trigger)
    :wxDialog.destroy(win)
    {:break, [[:erlang.list_to_atom(mod) | data]], trigged}
  end

  def handle_event(_WxEvent, _WinInfo) do
    :ignore
  end

  defp check_input(entries) do
    check_input(entries, [])
  end

  defp check_input([{entry, type} | entries], data) do
    str = :wxTextCtrl.getValue(entry)

    case :erl_scan.string(str) do
      {:ok, [{^type, _Line, val}], _EndLine} ->
        check_input(entries, [val | data])

      _Error ->
        :error
    end
  end

  defp check_input([], data) do
    :lists.reverse(data)
  end

  defp create_sizer_with_text(parent, label, def__) do
    text = :wxTextCtrl.new(parent, -1)
    sz = create_label_of_control(parent, label, text, def__)
    {sz, text}
  end

  defp create_label_of_control(parent, label, control, def__) do
    sizer = :wxBoxSizer.new(4)
    text = :wxStaticText.new(parent, -1, label)
    border = {:border, 5}
    flag = 32 ||| 16 ||| 2048
    _ = :wxSizer.add(sizer, text, [{:proportion, 1}, {:flag, flag}, border])
    _ = :wxSizer.add(sizer, control, [{:proportion, 3}, {:flag, flag ||| 8192}, border])

    :wxControl.setLabel(
      control,
      :dbg_wx_win.to_string(def__)
    )

    sizer
  end

  defp create_trigger_box(win) do
    sBox = :wxStaticBox.new(win, -1, 'Trigger Action:')
    sBS = :wxStaticBoxSizer.new(sBox, 8)
    ebtn = :wxRadioButton.new(win, -1, 'Enable')
    _ = :wxSizer.add(sBS, ebtn)
    dibtn = :wxRadioButton.new(win, -1, 'Disable')
    _ = :wxSizer.add(sBS, dibtn)
    debtn = :wxRadioButton.new(win, -1, 'Delete')
    _ = :wxSizer.add(sBS, debtn)
    :wxRadioButton.setValue(ebtn, true)
    {sBS, [{ebtn, :enable}, {dibtn, :disable}, {debtn, :delete}]}
  end

  defp get_trigger([{btn, op} | r]) do
    case :wxRadioButton.getValue(btn) do
      true ->
        op

      false ->
        get_trigger(r)
    end
  end
end
