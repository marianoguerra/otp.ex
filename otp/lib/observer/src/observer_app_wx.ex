defmodule :m_observer_app_wx do
  use Bitwise

  import :observer_perf_wx,
    only: [
      destroy_gc: 1,
      drawRoundedRectangle: 6,
      drawText: 4,
      getTextExtent: 2,
      haveGC: 0,
      make_gc: 2,
      setBrush: 2,
      setFont: 3,
      setPen: 2,
      strokeLine: 5,
      strokeLines: 2
    ]

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
    parent: :undefined,
    panel: :undefined,
    apps_w: :undefined,
    app_w: :undefined,
    paint: :undefined,
    current: :undefined,
    app: :undefined,
    sel: :undefined,
    appmon: :undefined,
    usegc: false
  )

  Record.defrecord(:r_paint, :paint,
    font: :undefined,
    fg: :undefined,
    pen: :undefined,
    brush: :undefined,
    sel: :undefined,
    links: :undefined
  )

  Record.defrecord(:r_app, :app,
    ptree: :undefined,
    n2p: :undefined,
    links: :undefined,
    dim: :undefined
  )

  Record.defrecord(:r_box, :box,
    x: :undefined,
    y: :undefined,
    w: :undefined,
    h: :undefined,
    s1: :undefined
  )

  Record.defrecord(:r_str, :str, x: :undefined, y: :undefined, text: :undefined, pid: :undefined)

  def start_link(notebook, parent, config) do
    :wx_object.start_link(:observer_app_wx, [notebook, parent, config], [])
  end

  def init([notebook, parent, _Config]) do
    panel =
      :wxPanel.new(
        notebook,
        [{:size, :wxWindow.getClientSize(notebook)}, {:winid, 1}]
      )

    main = :wxBoxSizer.new(4)

    splitter =
      :wxSplitterWindow.new(
        panel,
        [{:size, :wxWindow.getClientSize(panel)}, {:style, 128 ||| 0 ||| 256}, {:id, 2}]
      )

    apps = :wxListBox.new(splitter, 3, [])
    p2 = :wxPanel.new(splitter, [{:winid, 4}])
    extra = :wxBoxSizer.new(8)

    drawingArea =
      :wxScrolledWindow.new(
        p2,
        [{:winid, 5}, {:style, 65536}]
      )

    bG = :wxWindow.getBackgroundColour(apps)

    :wxWindow.setBackgroundStyle(
      drawingArea,
      :wxe_util.get_const(:wxBG_STYLE_PAINT)
    )

    :wxWindow.setVirtualSize(drawingArea, 800, 800)
    :wxSplitterWindow.setMinimumPaneSize(splitter, 50)
    :wxSizer.add(extra, drawingArea, [{:flag, 8192}, {:proportion, 1}])
    :wxWindow.setSizer(p2, extra)
    :wxSplitterWindow.splitVertically(splitter, apps, p2, [{:sashPosition, 150}])
    :wxWindow.setSizer(panel, main)

    :wxSizer.add(main, splitter, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:proportion, 1},
      {:border, 5}
    ])

    :wxWindow.setSizer(panel, main)
    :wxListBox.connect(apps, :command_listbox_selected)
    :wxPanel.connect(drawingArea, :paint, [:callback])
    :wxPanel.connect(drawingArea, :size, [{:skip, true}])
    :wxPanel.connect(drawingArea, :left_up)
    :wxPanel.connect(drawingArea, :left_dclick)
    :wxPanel.connect(drawingArea, :right_down)

    case :os.type() do
      {:win32, _} ->
        :wxPanel.connect(drawingArea, :erase_background, [
          {:callback,
           fn _, _ ->
             :ok
           end}
        ])

      _ ->
        :ok
    end

    useGC = haveGC()

    version28 =
      :wxe_util.get_const(:wxMAJOR_VERSION) === 2 and :wxe_util.get_const(:wxMINOR_VERSION) === 8

    scale = :observer_wx.get_scale()

    font =
      case :os.type() do
        {:unix, _} when useGC and version28 ->
          :wxFont.new(scale * 12, 71, 90, :wxe_util.get_const(:wxFONTWEIGHT_NORMAL))

        _ ->
          font0 = :wxSystemSettings.getFont(17)

          :wxFont.setPointSize(
            font0,
            scale * :wxFont.getPointSize(font0)
          )

          font0
      end

    selCol = :wxSystemSettings.getColour(1 + 12)

    {fg, bGBrush, pen} =
      case :observer_lib.is_darkmode(bG) do
        false ->
          {:wxSystemSettings.getColour(1 + 14 + 1 + 2),
           :wxBrush.new(:wxSystemSettings.getColour(1 + 14 + 1)),
           :wxPen.new({80, 80, 80}, [{:width, scale * 2}])}

        true ->
          {:wxSystemSettings.getColour(1 + 14 + 1 + 2),
           :wxBrush.new(:wxSystemSettings.getColour(1 + 14 + 1)),
           :wxPen.new({0, 0, 0}, [{:width, scale * 2}])}
      end

    selBrush = :wxBrush.new(selCol)
    linkPen = :wxPen.new(selCol, [{:width, scale * 2}])
    :erlang.process_flag(:trap_exit, true)

    {panel,
     r_state(
       parent: parent,
       panel: panel,
       apps_w: apps,
       app_w: drawingArea,
       usegc: useGC,
       paint: r_paint(font: font, fg: fg, pen: pen, brush: bGBrush, sel: selBrush, links: linkPen)
     )}
  end

  defp setup_scrollbar(appWin, app) do
    setup_scrollbar(:wxWindow.getClientSize(appWin), appWin, app)
  end

  defp setup_scrollbar({cW, cH}, appWin, r_app(dim: {w0, h0})) do
    w = max(w0, cW)
    h = max(h0, cH)
    pPC = 20

    cond do
      w0 <= cW and h0 <= cH ->
        :wxScrolledWindow.setScrollbars(appWin, w, h, 0, 0)

      h0 <= cH ->
        :wxScrolledWindow.setScrollbars(appWin, pPC, h, div(w, pPC) + 1, 0)

      w0 <= cW ->
        :wxScrolledWindow.setScrollbars(appWin, w, pPC, 0, div(h, pPC) + 1)

      true ->
        :wxScrolledWindow.setScrollbars(appWin, pPC, pPC, div(w, pPC) + 1, div(h, pPC) + 1)
    end
  end

  defp setup_scrollbar(_, _, :undefined) do
    :ok
  end

  def handle_event(
        r_wx(
          event:
            r_wxCommand(
              type: :command_listbox_selected,
              cmdString: appStr
            )
        ),
        state = r_state(appmon: appMon, current: prev)
      ) do
    case appStr do
      [] ->
        {:noreply, state}

      _ ->
        app = :erlang.list_to_atom(appStr)
        prev !== :undefined and :appmon_info.app(appMon, prev, false, [])
        :appmon_info.app(appMon, app, true, [])
        {:noreply, r_state(state, current: app)}
    end
  end

  def handle_event(
        r_wx(id: id, event: _Sz = r_wxSize(size: size)),
        state = r_state(app: app, app_w: appWin)
      ) do
    id === 5 and setup_scrollbar(size, appWin, app)
    {:noreply, state}
  end

  def handle_event(
        r_wx(event: r_wxMouse(type: type, x: x0, y: y0)),
        s0 = r_state(app: app, app_w: appWin)
      ) do
    case app do
      r_app(ptree: tree) ->
        {x, y} = :wxScrolledWindow.calcUnscrolledPosition(appWin, x0, y0)
        hit = locate_node(x, y, [tree])
        state = handle_mouse_click(hit, type, s0)
        {:noreply, state}

      _ ->
        {:noreply, s0}
    end
  end

  def handle_event(
        r_wx(event: r_wxCommand(type: :command_menu_selected)),
        state = r_state(panel: panel, sel: :undefined)
      ) do
    :observer_lib.display_info_dialog(panel, 'Select process first')
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 101,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state = r_state(sel: {r_box(s1: r_str(pid: pid)), _})
      ) do
    send(:observer, {:open_link, pid})
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 102,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state =
          r_state(
            panel: panel,
            sel: {r_box(s1: r_str(pid: pid)), _}
          )
      ) do
    case :observer_lib.user_term(panel, 'Enter message', '') do
      :cancel ->
        :ok

      {:ok, term} ->
        send(pid, term)

      {:error, error} ->
        :observer_lib.display_info_dialog(panel, error)
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 103,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state =
          r_state(
            panel: panel,
            sel: {r_box(s1: r_str(pid: pid)), _}
          )
      ) do
    case :observer_lib.user_term(panel, 'Enter Exit Reason', 'kill') do
      :cancel ->
        :ok

      {:ok, term} ->
        :erlang.exit(pid, term)

      {:error, error} ->
        :observer_lib.display_info_dialog(panel, error)
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 104,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state = r_state(sel: {box, _})
      ) do
    :observer_trace_wx.add_processes([box_to_pid(box)])
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 105,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state = r_state(sel: {box, _})
      ) do
    :observer_trace_wx.add_processes([box_to_reg(box)])
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 106,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state = r_state(sel: sel)
      ) do
    get = fn box ->
      box_to_pid(box)
    end

    :observer_trace_wx.add_processes(tree_map(sel, get))
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 107,
          event: r_wxCommand(type: :command_menu_selected)
        ),
        state = r_state(sel: sel)
      ) do
    get = fn box ->
      box_to_reg(box)
    end

    :observer_trace_wx.add_processes(tree_map(sel, get))
    {:noreply, state}
  end

  def handle_event(event, _State) do
    :erlang.error({:unhandled_event, event})
  end

  def handle_sync_event(
        r_wx(event: r_wxPaint()),
        _,
        r_state(app_w: dA, app: app, sel: sel, paint: paint, usegc: useGC)
      ) do
    gC = {gC0, dC} = make_gc(dA, useGC)

    case useGC do
      false ->
        :wxScrolledWindow.doPrepareDC(dA, dC)

      true ->
        {sx, sy} =
          :wxScrolledWindow.calcScrolledPosition(
            dA,
            {0, 0}
          )

        :wxGraphicsContext.translate(gC0, sx, sy)
    end

    draw(gC, app, sel, paint)
    destroy_gc(gC)
    :ok
  end

  def handle_call(:get_config, _, state) do
    {:reply, %{}, state}
  end

  def handle_call(event, from, _State) do
    :erlang.error({:unhandled_call, event, from})
  end

  def handle_cast(event, _State) do
    :erlang.error({:unhandled_cast, event})
  end

  def handle_info(
        {:active, node},
        state = r_state(parent: parent, current: curr, appmon: appmon)
      ) do
    create_menus(parent, [])

    pid =
      try do
        ^node = node(appmon)
        appmon
      catch
        _, _ ->
          {:ok, p} = :appmon_info.start_link(node, self(), [])
          p
      end

    :appmon_info.app_ctrl(pid, node, true, [])
    curr !== :undefined and :appmon_info.app(pid, curr, true, [])
    {:noreply, r_state(state, appmon: pid)}
  end

  def handle_info(:not_active, state = r_state(appmon: appMon)) do
    :appmon_info.app_ctrl(appMon, node(appMon), false, [])

    :lists.member(
      node(appMon),
      :erlang.nodes()
    ) and :erlang.exit(appMon, :normal)

    :observer_wx.set_status('')
    {:noreply, r_state(state, appmon: :undefined)}
  end

  def handle_info(
        {:delivery, pid, :app_ctrl, _, apps0},
        state = r_state(appmon: pid, apps_w: lBox, current: curr0)
      ) do
    apps =
      for {_, app, {_, _, _}} <- apps0 do
        :erlang.atom_to_list(app)
      end

    :wxListBox.clear(lBox)

    :wxListBox.appendStrings(
      lBox,
      for app <- :lists.sort(apps) do
        app
      end
    )

    case apps do
      [app | _] when curr0 === :undefined ->
        curr = :erlang.list_to_atom(app)
        :appmon_info.app(pid, curr, true, [])
        {:noreply, r_state(state, current: curr)}

      _ ->
        {:noreply, state}
    end
  end

  def handle_info(
        {:delivery, _Pid, :app, _Curr, {[], [], [], []}},
        state = r_state(panel: panel)
      ) do
    :wxWindow.refresh(panel)
    {:noreply, r_state(state, app: :undefined, sel: :undefined)}
  end

  def handle_info(
        {:delivery, pid, :app, curr, appData},
        state =
          r_state(
            panel: panel,
            appmon: pid,
            current: curr,
            usegc: useGC,
            app_w: appWin,
            paint: r_paint(fg: fg, font: font)
          )
      ) do
    gC =
      cond do
        useGC ->
          {:wxGraphicsContext.create(appWin), false}

        true ->
          {false, :wxWindowDC.new(appWin)}
      end

    setFont(gC, font, fg)
    app = build_tree(appData, gC)
    destroy_gc(gC)
    setup_scrollbar(appWin, app)
    :wxWindow.refresh(panel)
    :wxWindow.layout(panel)
    {:noreply, r_state(state, app: app, sel: :undefined)}
  end

  def handle_info({:EXIT, _, :noconnection}, state) do
    {:noreply, state}
  end

  def handle_info({:EXIT, _, :normal}, state) do
    {:noreply, state}
  end

  def handle_info(_Event, state) do
    {:noreply, state}
  end

  def terminate(_Event, _State) do
    :ok
  end

  def code_change(_, _, state) do
    state
  end

  defp handle_mouse_click(
         node = {r_box(s1: r_str(pid: pid)), _},
         type,
         state = r_state(app_w: appWin, panel: panel)
       ) do
    case type do
      :left_dclick ->
        send(:observer, {:open_link, pid})

      :right_down ->
        popup_menu(panel)

      _ ->
        :ok
    end

    :observer_wx.set_status(:io_lib.format('Pid: ~p', [pid]))
    :wxWindow.refresh(appWin)
    r_state(state, sel: node)
  end

  defp handle_mouse_click(_, _, state = r_state(sel: :undefined)) do
    state
  end

  defp handle_mouse_click(_, :right_down, state = r_state(panel: panel)) do
    popup_menu(panel)
    state
  end

  defp handle_mouse_click(_, _, state = r_state(app_w: appWin)) do
    :observer_wx.set_status('')
    :wxWindow.refresh(appWin)
    r_state(state, sel: :undefined)
  end

  defp create_menus(parent, _) do
    menuEntries = [
      {'File',
       [
         r_create_menu(id: 101, text: 'Process info'),
         r_create_menu(id: 102, text: 'Send Msg'),
         r_create_menu(id: 103, text: 'Kill process')
       ]},
      {'Trace',
       [
         r_create_menu(id: 104, text: 'Trace process'),
         r_create_menu(id: 105, text: 'Trace named process'),
         r_create_menu(id: 106, text: 'Trace process tree'),
         r_create_menu(id: 107, text: 'Trace named process tree')
       ]}
    ]

    :observer_wx.create_menus(parent, menuEntries)
  end

  defp popup_menu(panel) do
    menu = :wxMenu.new()
    :wxMenu.append(menu, 101, 'Process info')
    :wxMenu.append(menu, 104, 'Trace process')
    :wxMenu.append(menu, 105, 'Trace named process')
    :wxMenu.append(menu, 106, 'Trace process tree')
    :wxMenu.append(menu, 107, 'Trace named process tree')
    :wxMenu.append(menu, 102, 'Send Msg')
    :wxMenu.append(menu, 103, 'Kill process')
    :wxWindow.popupMenu(panel, menu)
    :wxMenu.destroy(menu)
  end

  defp locate_node(x, _Y, [{box = r_box(x: bX), _Chs} | _Rest])
       when x < bX do
    {:left, box}
  end

  defp locate_node(x, y, [
         node = {box = r_box(x: bX, y: bY, w: bW, h: bH), _Chs}
         | rest
       ])
       when x <= bX + bW do
    cond do
      y < bY ->
        {:above, box}

      y <= bY + bH ->
        node

      true ->
        locate_node(x, y, rest)
    end
  end

  defp locate_node(x, y, [{_, chs} | rest]) do
    case locate_node(x, y, chs) do
      node = {r_box(), _} ->
        node

      _Miss ->
        locate_node(x, y, rest)
    end
  end

  defp locate_node(_, _, []) do
    false
  end

  defp locate_box(from, [{box = r_box(s1: r_str(pid: from)), _} | _]) do
    box
  end

  defp locate_box(from, [{_, chs} | rest]) do
    case locate_box(from, chs) do
      box = r_box() ->
        box

      _ ->
        locate_box(from, rest)
    end
  end

  defp locate_box(from, []) do
    {false, from}
  end

  defp build_tree({root, p2Name, links, xLinks0}, fontW) do
    fam = :sofs.relation_to_family(:sofs.relation(links))

    name2P =
      :gb_trees.from_orddict(
        :lists.sort(
          for {pid, name} <- p2Name do
            {name, pid}
          end
        )
      )

    lookup = :gb_trees.from_orddict(:sofs.to_external(fam))
    {_, tree0} = build_tree2(root, lookup, name2P, fontW)
    {tree, dim} = calc_tree_size(tree0)

    fetch = fn {from, to}, acc ->
      try do
        {:value, toPid} = :gb_trees.lookup(to, name2P)
        fromPid = :gb_trees.get(from, name2P)

        [
          {locate_box(fromPid, [tree]), locate_box(toPid, [tree])}
          | acc
        ]
      catch
        _, _ ->
          acc
      end
    end

    xLinks = :lists.foldl(fetch, [], xLinks0)
    r_app(ptree: tree, dim: dim, links: xLinks)
  end

  defp build_tree2(root, tree0, n2P, fontW) do
    case :gb_trees.lookup(root, tree0) do
      :none ->
        {tree0, {box(root, n2P, fontW), []}}

      {:value, children} ->
        tree1 = :gb_trees.delete(root, tree0)

        {tree, cHs} =
          :lists.foldr(
            fn
              'port ' ++ _, acc ->
                acc

              child, {t0, acc} ->
                {t, c} = build_tree2(child, t0, n2P, fontW)
                {t, [c | acc]}
            end,
            {tree1, []},
            children
          )

        {tree, {box(root, n2P, fontW), cHs}}
    end
  end

  defp calc_tree_size(tree) do
    cols = calc_col_start(tree, [0])
    {boxes, {w, hs}} = calc_tree_size(tree, cols, 16, [12])
    {boxes, {w, :lists.max(hs)}}
  end

  defp calc_col_start({r_box(w: w), chs}, [max | acc0]) do
    acc =
      cond do
        acc0 == [] ->
          [0]

        true ->
          acc0
      end

    depth =
      :lists.foldl(
        fn child, mDepth ->
          calc_col_start(child, mDepth)
        end,
        acc,
        chs
      )

    [max(w, max) | depth]
  end

  defp calc_tree_size({box = r_box(w: w, h: h), []}, _, x, [y | ys]) do
    {{r_box(box, x: x, y: y), []}, {x + w + 16, [y + h + 12 | ys]}}
  end

  defp calc_tree_size({box, children}, [col | cols], x, [h0 | hs0]) do
    hs1 = calc_row_start(children, h0, hs0)
    startX = x + col + 16
    {boxes, {w, hs}} = calc_tree_sizes(children, cols, startX, startX, hs1, [])
    y = middle(boxes, h0)
    h = y + r_box(box, :h) + 12
    {{r_box(box, x: x, y: y), boxes}, {w, [h | hs]}}
  end

  defp calc_tree_sizes([child | chs], cols, x0, w0, hs0, acc) do
    {tree, {w, hs}} = calc_tree_size(child, cols, x0, hs0)
    calc_tree_sizes(chs, cols, x0, max(w, w0), hs, [tree | acc])
  end

  defp calc_tree_sizes([], _, _, w, hs, acc) do
    {:lists.reverse(acc), {w, hs}}
  end

  defp calc_row_start(chs = [{r_box(h: h), _} | _], start, hs0) do
    nChs = length(chs)
    wanted = div(h * nChs + 12 * (nChs - 1), 2) - div(h, 2)

    case hs0 do
      [] ->
        [max(12, start - wanted)]

      [next | hs] ->
        [max(next, start - wanted) | hs]
    end
  end

  defp middle([], y) do
    y
  end

  defp middle([{r_box(y: y), _}], _) do
    y
  end

  defp middle([{r_box(y: y0), _} | list], _) do
    {r_box(y: y1), _} = :lists.last(list)
    div(y0 + y1, 2)
  end

  defp box(str0, n2P, fontW) do
    pid = :gb_trees.get(str0, n2P)

    str =
      cond do
        hd(str0) === ?< ->
          :lists.append(:io_lib.format('~w', [pid]))

        true ->
          str0
      end

    {tW, tH} = getTextExtent(fontW, str)
    data = r_str(text: str, x: div(10, 2), y: div(10, 2), pid: pid)
    r_box(w: round(tW) + 10, h: round(tH) + 10, s1: data)
  end

  defp box_to_pid(r_box(s1: r_str(pid: pid))) do
    pid
  end

  defp box_to_reg(r_box(s1: r_str(text: [?< | _], pid: pid))) do
    pid
  end

  defp box_to_reg(r_box(s1: r_str(text: name))) do
    :erlang.list_to_atom(name)
  end

  defp tree_map({box, chs}, fun) do
    tree_map(chs, fun, [fun.(box)])
  end

  defp tree_map([{box, chs} | rest], fun, acc0) do
    acc = tree_map(chs, fun, [fun.(box) | acc0])
    tree_map(rest, fun, acc)
  end

  defp tree_map([], _, acc) do
    acc
  end

  defp draw(_DC, :undefined, _, _) do
    :ok
  end

  defp draw(
         dC,
         r_app(dim: {_W, _H}, ptree: tree, links: links),
         sel,
         r_paint(font: font, fg: fg, pen: pen, brush: brush, links: lPen, sel: selBrush)
       ) do
    setPen(dC, lPen)

    for link <- links do
      draw_xlink(link, dC)
    end

    setPen(dC, pen)
    setBrush(dC, brush)
    setFont(dC, font, fg)
    draw_tree(tree, :root, dC)

    case sel do
      :undefined ->
        :ok

      {r_box(x: x, y: y, w: w, h: h, s1: str1), _} ->
        setBrush(dC, selBrush)
        drawRoundedRectangle(dC, x - 1, y - 1, w + 2, h + 2, 8.0)
        draw_str(dC, str1, x, y)
    end
  end

  defp draw_tree({box = r_box(x: x, y: y, w: w, h: h, s1: str1), chs}, parent, dC) do
    drawRoundedRectangle(dC, x, y, w, h, 8.0)
    draw_str(dC, str1, x, y)

    dot =
      case chs do
        [] ->
          :ok

        [{r_box(x: cX0), _} | _] ->
          cY = y + div(h, 2)
          cX = cX0 - div(16, 2)
          strokeLine(dC, x + w, cY, cX, cY)
          {cX, cY}
      end

    draw_link(parent, box, dC)

    for child <- chs do
      draw_tree(child, dot, dC)
    end
  end

  defp draw_link({cX, cY}, r_box(x: x, y: y0, h: h), dC) do
    y = y0 + div(h, 2)

    case y === cY do
      true ->
        strokeLine(dC, cX, cY, x, cY)

      false ->
        strokeLines(dC, [{cX, cY}, {cX, y}, {x, y}])
    end
  end

  defp draw_link(_, _, _) do
    :ok
  end

  defp draw_xlink({r_box(x: x0, y: y0, h: bH), r_box(x: x1, y: y1)}, dC)
       when x0 === x1 do
    draw_xlink(x0, y0, x1, y1, bH, dC)
  end

  defp draw_xlink(
         {r_box(x: x0, y: y0, h: bH, w: bW), r_box(x: x1, y: y1)},
         dC
       )
       when x0 < x1 do
    draw_xlink(x0 + bW, y0, x1, y1, bH, dC)
  end

  defp draw_xlink(
         {r_box(x: x0, y: y0, h: bH), r_box(x: x1, w: bW, y: y1)},
         dC
       )
       when x0 > x1 do
    draw_xlink(x1 + bW, y1, x0, y0, bH, dC)
  end

  defp draw_xlink({_From, _To}, _DC) do
    :ignore
  end

  defp draw_xlink(x0, y00, x1, y11, bH, dC) do
    {y0, y1} =
      cond do
        y00 < y11 ->
          {y00 + bH - 6, y11 + 6}

        true ->
          {y00 + 6, y11 + bH - 6}
      end

    strokeLines(
      dC,
      [{x0, y0}, {x0 + 5, y0}, {x1 - 5, y1}, {x1, y1}]
    )
  end

  defp draw_str(dC, r_str(x: sx, y: sy, text: text), x, y) do
    drawText(dC, text, x + sx, y + sy)
  end

  defp draw_str(_, _, _, _) do
    :ok
  end
end
