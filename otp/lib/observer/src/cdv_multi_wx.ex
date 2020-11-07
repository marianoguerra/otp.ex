defmodule :m_cdv_multi_wx do
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
    main_panel: :undefined,
    main_sizer: :undefined,
    menu: :undefined,
    menu_sizer: :undefined,
    callback: :undefined,
    pages: :undefined,
    dyn_panel: :undefined,
    dyn_sizer: :undefined,
    dyn_page: :undefined
  )

  def start_link(notebook, info) do
    :wx_object.start_link(:cdv_multi_wx, [notebook, info], [])
  end

  def init([notebook, callback]) when is_atom(callback) do
    pages = callback.get_info()
    {mainPanel, state0} = init([notebook, pages])
    {mainPanel, r_state(state0, callback: callback)}
  end

  def init([notebook, pages]) do
    mainPanel = :wxPanel.new(notebook)
    sizer = :wxBoxSizer.new(4)
    leftMenuSizer = :wxStaticBoxSizer.new(8, mainPanel, [{:label, 'Please select'}])

    leftMenu =
      :wxListBox.new(mainPanel, -1, [
        {:style, 32},
        {:choices,
         for {t, _, _} <- pages do
           t
         end}
      ])

    :wxListBox.setSelection(leftMenu, 0)
    :wxListBox.connect(leftMenu, :command_listbox_selected)
    :wxSizer.add(leftMenuSizer, leftMenu, [{:flag, 8192}, {:proportion, 2}])
    dynPanel = :wxScrolledWindow.new(mainPanel)
    :wxScrolledWindow.enableScrolling(dynPanel, true, true)
    :wxScrolledWindow.setScrollbars(dynPanel, 1, 1, 0, 0)
    borderFlags = 16 ||| 32

    :wxSizer.add(sizer, leftMenuSizer, [
      {:flag, 8192 ||| borderFlags ||| 64},
      {:proportion, 0},
      {:border, 5}
    ])

    :wxSizer.add(sizer, dynPanel, [
      {:flag, 8192 ||| borderFlags ||| 64},
      {:proportion, 1},
      {:border, 5}
    ])

    :wxPanel.setSizer(mainPanel, sizer)

    state =
      load_dyn_page(
        r_state(
          main_panel: mainPanel,
          main_sizer: sizer,
          menu: leftMenu,
          menu_sizer: leftMenuSizer,
          pages: pages,
          dyn_panel: dynPanel
        )
      )

    {mainPanel, state}
  end

  def handle_info(:active, state) do
    newState =
      :wx.batch(fn ->
        update_dyn_page(state)
      end)

    {:noreply, newState}
  end

  def handle_info(info, state) do
    :io.format('~p:~p: Unhandled info: ~tp~n', [:cdv_multi_wx, 97, info])
    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_, _, state) do
    {:ok, state}
  end

  def handle_call(:new_dump, _From, state) do
    newState =
      :wx.batch(fn ->
        update_left_menu(state)
      end)

    {:reply, :ok, newState}
  end

  def handle_call(msg, _From, state) do
    :io.format('~p:~p: Unhandled Call ~tp~n', [:cdv_multi_wx, 115, msg])
    {:reply, :ok, state}
  end

  def handle_cast(msg, state) do
    :io.format('~p:~p: Unhandled cast ~tp~n', [:cdv_multi_wx, 119, msg])
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          event:
            r_wxCommand(
              type: :command_listbox_selected,
              cmdString: []
            )
        ),
        state
      ) do
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          event:
            r_wxCommand(
              type: :command_listbox_selected,
              cmdString: _DynName
            )
        ),
        state
      ) do
    newState =
      :wx.batch(fn ->
        update_dyn_page(state)
      end)

    {:noreply, newState}
  end

  def handle_event(event, state) do
    :io.format('~p:~p: Unhandled event ~tp\n', [:cdv_multi_wx, 139, event])
    {:noreply, state}
  end

  defp update_left_menu(
         r_state(main_panel: panel, callback: callback, menu: oldMenu, menu_sizer: menuSizer) =
           state
       ) do
    pages = callback.get_info()
    :wxListBox.disconnect(oldMenu)
    :wxWindow.destroy(oldMenu)

    newMenu =
      :wxListBox.new(panel, -1, [
        {:style, 32},
        {:choices,
         for {t, _, _} <- pages do
           t
         end}
      ])

    :wxListBox.setSelection(newMenu, 0)
    :wxListBox.connect(newMenu, :command_listbox_selected)
    :wxSizer.add(menuSizer, newMenu, [{:flag, 8192}, {:proportion, 2}])
    :wxSizer.layout(menuSizer)
    r_state(state, pages: pages, menu: newMenu)
  end

  defp update_dyn_page(r_state(dyn_page: :undefined) = state) do
    load_dyn_page(state)
  end

  defp update_dyn_page(
         r_state(
           dyn_page: oldDynPage,
           dyn_sizer: oldDynSizer
         ) = state
       ) do
    :wxSizer.detach(oldDynSizer, oldDynPage)
    :wxWindow.destroy(oldDynPage)
    load_dyn_page(state)
  end

  defp load_dyn_page(
         r_state(main_sizer: mainSizer, dyn_panel: dynPanel, menu: menu, pages: pages) = state
       ) do
    doFreeze =
      [:wxe_util.get_const(:wxMAJOR_VERSION), :wxe_util.get_const(:wxMINOR_VERSION)] < [2, 9]

    doFreeze and :wxWindow.freeze(dynPanel)
    name = :wxListBox.getStringSelection(menu)
    {page, sizer} = load_dyn_page(dynPanel, name, pages)
    :wxSizer.layout(mainSizer)
    doFreeze and :wxWindow.thaw(dynPanel)
    send(:wx_object.get_pid(page), :active)
    r_state(state, dyn_page: page, dyn_sizer: sizer)
  end

  defp load_dyn_page(panel, name, pages) do
    sizer = :wxStaticBoxSizer.new(8, panel, [{:label, name}])
    {_, callback, info} = :lists.keyfind(name, 1, pages)
    dynPage = callback.start_link(panel, info)
    :wxSizer.add(sizer, dynPage, [{:flag, 8192}, {:proportion, 1}])
    :wxPanel.setSizerAndFit(panel, sizer, [{:deleteOld, true}])
    {dynPage, sizer}
  end
end
