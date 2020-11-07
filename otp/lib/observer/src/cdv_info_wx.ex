defmodule :m_cdv_info_wx do
  use Bitwise
  @behaviour :wx_object
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

  Record.defrecord(:r_match_spec, :match_spec, name: '', term: [], str: [], func: '')
  Record.defrecord(:r_tpattern, :tpattern, m: :undefined, fa: :undefined, ms: :undefined)

  Record.defrecord(:r_traced_func, :traced_func,
    func_name: :undefined,
    arity: :undefined,
    match_spec: :EFE_TODO_NESTED_RECORD
  )

  Record.defrecord(:r_create_menu, :create_menu,
    id: :undefined,
    text: :undefined,
    help: [],
    type: :append,
    check: false
  )

  Record.defrecord(:r_colors, :colors, fg: :undefined, even: :undefined, odd: :undefined)

  Record.defrecord(:r_attrs, :attrs,
    even: :undefined,
    odd: :undefined,
    searched: :undefined,
    deleted: :undefined,
    changed_odd: :undefined,
    changed_even: :undefined,
    new_odd: :undefined,
    new_even: :undefined
  )

  Record.defrecord(:r_ti, :ti, tick: 0, disp: 10 / 2, fetch: 2, secs: 60)

  Record.defrecord(:r_win, :win,
    name: :undefined,
    panel: :undefined,
    size: :undefined,
    geom: :undefined,
    graphs: [],
    no_samples: 0,
    max: :undefined,
    state: :undefined,
    info: []
  )

  Record.defrecord(:r_state, :state,
    panel: :undefined,
    sizer: :undefined,
    fpanel: :undefined,
    callback: :undefined,
    trunc_warn: []
  )

  def start_link(parentWin, info) do
    :wx_object.start_link(:cdv_info_wx, [parentWin, info], [])
  end

  def init([parentWin, callback]) when is_atom(callback) do
    {infoFields, info, tW} = callback.get_info()
    {panel, sizer, fPanel} = create_box(parentWin, infoFields, info)

    {panel,
     r_state(panel: panel, sizer: sizer, fpanel: fPanel, callback: callback, trunc_warn: tW)}
  end

  def init([parentWin, {infoFields, info, tW}]) do
    {panel, sizer, fPanel} = create_box(parentWin, infoFields, info)
    {panel, r_state(panel: panel, sizer: sizer, fpanel: fPanel, trunc_warn: tW)}
  end

  def handle_info(:active, state) do
    :cdv_wx.set_status(r_state(state, :trunc_warn))
    {:noreply, state}
  end

  def handle_info(info, state) do
    :io.format('~p:~p: Unhandled info: ~tp~n', [:cdv_info_wx, 68, info])
    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_, _, state) do
    {:ok, state}
  end

  def handle_call(
        :new_dump,
        _From,
        r_state(callback: callback, panel: panel, sizer: sizer, fpanel: fPanel) = state
      ) do
    {infoFields, info, tW} = callback.get_info()

    newFPanel =
      :wx.batch(fn ->
        :wxWindow.destroy(fPanel)
        fP = create_field_panel(panel, sizer, infoFields, info)
        :wxSizer.layout(sizer)
        fP
      end)

    {:reply, :ok, r_state(state, fpanel: newFPanel, trunc_warn: tW)}
  end

  def handle_call(msg, _From, state) do
    :io.format('~p~p: Unhandled Call ~tp~n', [:cdv_info_wx, 91, msg])
    {:reply, :ok, state}
  end

  def handle_cast(msg, state) do
    :io.format('~p~p: Unhandled cast ~tp~n', [:cdv_info_wx, 95, msg])
    {:noreply, state}
  end

  def handle_event(
        r_wx(obj: moreEntry, event: r_wxMouse(type: :left_down), userData: {:more, more}),
        state
      ) do
    :observer_lib.add_scroll_entries(moreEntry, more)
    {:noreply, state}
  end

  def handle_event(
        r_wx(event: r_wxMouse(type: :left_down), userData: target),
        state
      ) do
    :cdv_virtual_list_wx.start_detail_win(target)
    {:noreply, state}
  end

  def handle_event(
        r_wx(obj: obj, event: r_wxMouse(type: :enter_window)),
        state
      ) do
    :wxTextCtrl.setForegroundColour(obj, {0, 0, 100, 255})
    {:noreply, state}
  end

  def handle_event(
        r_wx(obj: obj, event: r_wxMouse(type: :leave_window)),
        state
      ) do
    :wxTextCtrl.setForegroundColour(
      obj,
      :wxe_util.get_const(:wxBLUE)
    )

    {:noreply, state}
  end

  def handle_event(event, state) do
    :io.format('~p:~p: Unhandled event ~tp\n', [:cdv_info_wx, 115, event])
    {:noreply, state}
  end

  defp create_box(parentWin, infoFields, info) do
    panel = :wxPanel.new(parentWin)
    sizer = :wxBoxSizer.new(8)
    fPanel = create_field_panel(panel, sizer, infoFields, info)
    :wxPanel.setSizer(panel, sizer)
    {panel, sizer, fPanel}
  end

  defp create_field_panel(panel, sizer, infoFields, info0) do
    info = :observer_lib.fill_info(infoFields, info0)
    {fPanel, _FSizer, _Fields} = :observer_lib.display_info(panel, info)
    borderFlags = 16 ||| 32

    :wxSizer.add(sizer, fPanel, [
      {:flag, 8192 ||| borderFlags ||| 64},
      {:proportion, 0},
      {:border, 5}
    ])

    fPanel
  end
end
