defmodule :m_observer_trace_wx do
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
    parent: :undefined,
    panel: :undefined,
    n_view: :undefined,
    proc_view: :undefined,
    port_view: :undefined,
    m_view: :undefined,
    f_view: :undefined,
    logwin: :undefined,
    nodes: [],
    toggle_button: :undefined,
    tpids: [],
    tports: [],
    def_proc_flags: [],
    def_port_flags: [],
    output: [],
    tpatterns: :dict.new(),
    match_specs: []
  )

  Record.defrecord(:r_titem, :titem,
    id: :undefined,
    opts: :undefined
  )

  def start_link(notebook, parentPid, config) do
    :wx_object.start_link(:observer_trace_wx, [notebook, parentPid, config], [])
  end

  def add_processes(pids) when is_list(pids) do
    :wx_object.cast(
      :observer_wx.get_tracer(),
      {:add_processes, pids}
    )
  end

  def add_ports(ports) when is_list(ports) do
    :wx_object.cast(
      :observer_wx.get_tracer(),
      {:add_ports, ports}
    )
  end

  def init([notebook, parentPid, config]) do
    :wx.batch(fn ->
      create_window(notebook, parentPid, config)
    end)
  end

  defp create_window(notebook, parentPid, config) do
    panel =
      :wxPanel.new(
        notebook,
        [{:size, :wxWindow.getClientSize(notebook)}]
      )

    sizer = :wxBoxSizer.new(8)

    splitter =
      :wxSplitterWindow.new(
        panel,
        [{:size, :wxWindow.getClientSize(panel)}, {:style, 128 ||| 0 ||| 256}]
      )

    {nodeProcView, nodeView, processView, portView} = create_proc_port_view(splitter)
    {matchSpecView, modView, funcView} = create_matchspec_view(splitter)
    :wxSplitterWindow.setSashGravity(splitter, 0.5)
    :wxSplitterWindow.setMinimumPaneSize(splitter, 50)

    :wxSplitterWindow.splitHorizontally(splitter, nodeProcView, matchSpecView, [
      {:sashPosition, 368}
    ])

    :wxSizer.add(sizer, splitter, [
      {:flag, 8192 ||| (64 ||| 128 ||| 32 ||| 16)},
      {:border, 5},
      {:proportion, 1}
    ])

    buttons = :wxBoxSizer.new(4)
    toggleButton = :wxToggleButton.new(panel, 307, 'Start Trace', [])
    :wxSizer.add(buttons, toggleButton, [{:flag, 2048}])
    :wxSizer.addSpacer(buttons, 15)

    :wxSizer.add(
      buttons,
      :wxButton.new(panel, 331, [{:label, 'Add Nodes'}])
    )

    :wxSizer.add(
      buttons,
      :wxButton.new(panel, 308, [{:label, 'Add \'new\' Processes'}])
    )

    :wxSizer.add(
      buttons,
      :wxButton.new(panel, 309, [{:label, 'Add \'new\' Ports'}])
    )

    :wxSizer.add(
      buttons,
      :wxButton.new(panel, 310, [{:label, 'Add Trace Pattern'}])
    )

    :wxMenu.connect(panel, :command_togglebutton_clicked, [{:skip, true}])
    :wxMenu.connect(panel, :command_button_clicked, [{:skip, true}])
    :wxSizer.add(sizer, buttons, [{:flag, 16 ||| 32 ||| 128}, {:border, 5}, {:proportion, 0}])
    :wxWindow.setSizer(panel, sizer)

    mS =
      parse_ms(
        :maps.get(:match_specs, config, []),
        default_matchspecs()
      )

    {panel,
     r_state(
       parent: parentPid,
       panel: panel,
       n_view: nodeView,
       proc_view: processView,
       port_view: portView,
       m_view: modView,
       f_view: funcView,
       toggle_button: toggleButton,
       output: :maps.get(:output, config, []),
       def_proc_flags: :maps.get(:procflags, config, []),
       def_port_flags: :maps.get(:portflags, config, []),
       match_specs: mS
     )}
  end

  defp default_matchspecs() do
    for key <- [:funcs, :send, :receive] do
      {key, default_matchspecs(key)}
    end
  end

  defp default_matchspecs(key) do
    ms = get_default_matchspecs(key)

    for {name, term, funStr} <- ms do
      make_ms(name, term, funStr)
    end
  end

  defp get_default_matchspecs(:funcs) do
    [
      {'Return Trace', [{:_, [], [{:return_trace}]}], 'fun(_) -> return_trace() end'},
      {'Exception Trace', [{:_, [], [{:exception_trace}]}], 'fun(_) -> exception_trace() end'},
      {'Message Caller', [{:_, [], [{:message, {:caller}}]}], 'fun(_) -> message(caller()) end'},
      {'Message Dump', [{:_, [], [{:message, {:process_dump}}]}],
       'fun(_) -> message(process_dump()) end'}
    ]
  end

  defp get_default_matchspecs(:send) do
    [
      {'To local node', [{[:"$1", :_], [{:==, {:node, :"$1"}, {:node}}], []}],
       'fun([Pid,_]) when node(Pid)==node() ->\n    true\nend'},
      {'To remote node', [{[:"$1", :_], [{:"=/=", {:node, :"$1"}, {:node}}], []}],
       'fun([Pid,_]) when node(Pid)=/=node() ->\n    true\nend'}
    ]
  end

  defp get_default_matchspecs(:receive) do
    [
      {'From local node', [{[:"$1", :_, :_], [{:==, :"$1", {:node}}], []}],
       'fun([Node,_,_]) when Node==node() ->\n    true\nend'},
      {'From remote node', [{[:"$1", :_, :_], [{:"=/=", :"$1", {:node}}], []}],
       'fun([Node,_,_]) when Node=/=node() ->\n    true\nend'}
    ]
  end

  defp create_proc_port_view(parent) do
    panel = :wxPanel.new(parent)
    mainSz = :wxBoxSizer.new(4)
    style = 32 ||| 2

    splitter =
      :wxSplitterWindow.new(
        panel,
        [{:style, 128 ||| 0 ||| 256}]
      )

    nodes =
      :wxListCtrl.new(
        splitter,
        [{:winid, 330}, {:style, style}]
      )

    procsPortsSplitter =
      :wxSplitterWindow.new(
        splitter,
        [{:style, 128 ||| 0 ||| 256}]
      )

    procs =
      :wxListCtrl.new(
        procsPortsSplitter,
        [{:winid, 340}, {:style, style}]
      )

    ports =
      :wxListCtrl.new(
        procsPortsSplitter,
        [{:winid, 350}, {:style, style}]
      )

    li = :wxListItem.new()
    :wxListItem.setText(li, 'Nodes')
    :wxListCtrl.insertColumn(nodes, 0, li)

    addProc = fn {name, align, defSize}, col ->
      :wxListItem.setText(li, name)
      :wxListItem.setAlign(li, align)
      :wxListCtrl.insertColumn(procs, col, li)
      :wxListCtrl.setColumnWidth(procs, col, defSize)
      col + 1
    end

    scale = :observer_wx.get_scale()
    procListItems = [{'Process Id', 2, scale * 120}, {'Trace Options', 0, scale * 300}]
    :lists.foldl(addProc, 0, procListItems)

    addPort = fn {name, align, defSize}, col ->
      :wxListItem.setText(li, name)
      :wxListItem.setAlign(li, align)
      :wxListCtrl.insertColumn(ports, col, li)
      :wxListCtrl.setColumnWidth(ports, col, defSize)
      col + 1
    end

    portListItems = [{'Port Id', 2, scale * 120}, {'Trace Options', 0, scale * 300}]
    :lists.foldl(addPort, 0, portListItems)
    :wxListItem.destroy(li)
    :wxSplitterWindow.setSashGravity(splitter, 0.0)
    :wxSplitterWindow.setMinimumPaneSize(splitter, 50)
    :wxSplitterWindow.splitVertically(splitter, nodes, procsPortsSplitter, [{:sashPosition, 155}])
    :wxSizer.add(mainSz, splitter, [{:flag, 8192}, {:proportion, 1}])

    :wxSplitterWindow.setSashGravity(
      procsPortsSplitter,
      0.5
    )

    :wxSplitterWindow.setMinimumPaneSize(
      procsPortsSplitter,
      50
    )

    :wxSplitterWindow.splitHorizontally(procsPortsSplitter, procs, ports, [{:sashPosition, 182}])

    :wxListCtrl.connect(
      procs,
      :command_list_item_right_click
    )

    :wxListCtrl.connect(
      ports,
      :command_list_item_right_click
    )

    :wxListCtrl.connect(
      nodes,
      :command_list_item_right_click
    )

    :wxListCtrl.connect(nodes, :command_list_item_selected)
    :wxListCtrl.connect(procs, :size, [{:skip, true}])
    :wxListCtrl.connect(ports, :size, [{:skip, true}])
    :wxListCtrl.connect(nodes, :size, [{:skip, true}])
    :wxListCtrl.setToolTip(nodes, 'Right click to add nodes')
    :wxListCtrl.setToolTip(procs, 'Add items from Processes/Ports tab')
    :wxListCtrl.setToolTip(ports, 'Add items from Processes/Ports tab')
    :wxPanel.setSizer(panel, mainSz)
    :wxWindow.setFocus(procs)
    {panel, nodes, procs, ports}
  end

  defp create_matchspec_view(parent) do
    panel = :wxPanel.new(parent)
    mainSz = :wxBoxSizer.new(4)
    style = 32 ||| 2

    splitter =
      :wxSplitterWindow.new(
        panel,
        [{:style, 128 ||| 0 ||| 256}]
      )

    modules =
      :wxListCtrl.new(
        splitter,
        [{:winid, 360}, {:style, style ||| 8192}]
      )

    funcs =
      :wxListCtrl.new(
        splitter,
        [{:winid, 370}, {:style, style}]
      )

    li = :wxListItem.new()
    scale = :observer_wx.get_scale()
    :wxListItem.setText(li, 'Modules')
    :wxListCtrl.insertColumn(modules, 0, li)
    :wxListItem.setText(li, 'Functions')
    :wxListCtrl.insertColumn(funcs, 0, li)
    :wxListCtrl.setColumnWidth(funcs, 0, scale * 150)
    :wxListItem.setText(li, 'Match Spec')
    :wxListCtrl.insertColumn(funcs, 1, li)
    :wxListCtrl.setColumnWidth(funcs, 1, scale * 300)
    :wxListItem.destroy(li)
    :wxSplitterWindow.setSashGravity(splitter, 0.0)
    :wxSplitterWindow.setMinimumPaneSize(splitter, 50)
    :wxSplitterWindow.splitVertically(splitter, modules, funcs, [{:sashPosition, 155}])
    :wxSizer.add(mainSz, splitter, [{:flag, 8192}, {:proportion, 1}])
    :wxListCtrl.connect(modules, :size, [{:skip, true}])
    :wxListCtrl.connect(funcs, :size, [{:skip, true}])

    :wxListCtrl.connect(
      modules,
      :command_list_item_selected
    )

    :wxListCtrl.connect(
      modules,
      :command_list_item_right_click
    )

    :wxListCtrl.connect(
      funcs,
      :command_list_item_right_click
    )

    :wxListCtrl.setToolTip(panel, 'Add trace pattern with button below')
    :wxPanel.setSizer(panel, mainSz)
    {panel, modules, funcs}
  end

  defp create_menues(parent) do
    menus = [
      {'File',
       [
         r_create_menu(id: 306, text: 'Load settings'),
         r_create_menu(id: 305, text: 'Save settings')
       ]},
      {'Options',
       [
         r_create_menu(id: 311, text: 'Output'),
         r_create_menu(id: 312, text: 'Default Match Specifications for Functions'),
         r_create_menu(
           id: 313,
           text: 'Default Match Specifications for \'send\''
         ),
         r_create_menu(
           id: 314,
           text: 'Default Match Specifications for \'receive\''
         ),
         r_create_menu(
           id: 315,
           text: 'Default Process Options'
         ),
         r_create_menu(
           id: 316,
           text: 'Default Port Options'
         )
       ]}
    ]

    :observer_wx.create_menus(parent, menus)
  end

  def handle_event(r_wx(obj: obj, event: r_wxSize(size: {w, _})), state) do
    case :wx.getObjectType(obj) === :wxListCtrl do
      true ->
        :observer_lib.set_listctrl_col_size(obj, w)

      false ->
        :ok
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 308),
        state = r_state(panel: parent, def_proc_flags: traceOpts)
      ) do
    try do
      opts =
        :observer_traceoptions_wx.process_trace(
          parent,
          traceOpts
        )

      process = r_titem(id: :new_processes, opts: opts)

      {:noreply,
       do_add_processes(
         [process],
         r_state(state, def_proc_flags: opts)
       )}
    catch
      :cancel ->
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(id: 309),
        state = r_state(panel: parent, def_port_flags: traceOpts)
      ) do
    try do
      opts =
        :observer_traceoptions_wx.port_trace(
          parent,
          traceOpts
        )

      port = r_titem(id: :new_ports, opts: opts)
      {:noreply, do_add_ports([port], r_state(state, def_port_flags: opts))}
    catch
      :cancel ->
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(id: 310),
        state = r_state(panel: parent, nodes: nodes, match_specs: ms)
      ) do
    node =
      case nodes do
        [n | _] ->
          n

        [] ->
          node()
      end

    case :observer_traceoptions_wx.trace_pattern(self(), parent, node, ms) do
      :cancel ->
        {:noreply, state}

      patterns ->
        {:noreply, do_add_patterns(patterns, state)}
    end
  end

  def handle_event(
        r_wx(
          id: 360,
          event:
            r_wxList(
              type: :command_list_item_selected,
              itemIndex: row
            )
        ),
        state = r_state(tpatterns: tPs, m_view: mview, f_view: fview)
      ) do
    module =
      :erlang.list_to_atom(
        :wxListCtrl.getItemText(
          mview,
          row
        )
      )

    update_functions_view(:dict.fetch(module, tPs), fview)
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 330,
          event: r_wxList(type: :command_list_item_selected)
        ),
        state =
          r_state(
            tpids: tpids,
            tports: tports,
            n_view: nview,
            proc_view: procView,
            port_view: portView,
            nodes: ns
          )
      ) do
    nodes = get_selected_items(nview, ns)
    update_p_view(tpids, procView, nodes)
    update_p_view(tports, portView, nodes)
    {:noreply, state}
  end

  def handle_event(
        r_wx(
          event:
            r_wxCommand(
              type: :command_togglebutton_clicked,
              commandInt: 1
            )
        ),
        r_state(
          panel: panel,
          nodes: nodes,
          tpids: tProcs,
          tports: tPorts,
          tpatterns: tPs0,
          toggle_button: toggleBtn,
          output: opts
        ) = state
      ) do
    try do
      tPs = :dict.to_list(tPs0)
      tProcs == [] and tPorts == [] and throw({:error, 'No processes or ports traced'})
      nodes == [] and throw({:error, 'No nodes traced'})

      haveCallTrace = fn r_titem(opts: os) ->
        :lists.member(:functions, os)
      end

      wStr = 'Call trace actived but no trace patterns used'

      tPs == [] and
        :lists.any(
          haveCallTrace,
          tProcs
        ) and
        :observer_wx.create_txt_dialog(
          panel,
          wStr,
          'Warning',
          256
        )

      {tTB, logWin} = ttb_output_args(panel, opts)
      {:ok, _} = :ttb.tracer(nodes, tTB)
      setup_ttb(tPs, tProcs, tPorts)
      :wxToggleButton.setLabel(toggleBtn, 'Stop Trace')
      {:noreply, r_state(state, logwin: logWin)}
    catch
      {:error, msg} ->
        :observer_wx.create_txt_dialog(panel, msg, 'Error', 512)
        :wxToggleButton.setValue(toggleBtn, false)
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(
          event:
            r_wxCommand(
              type: :command_togglebutton_clicked,
              commandInt: 0
            )
        ),
        r_state(toggle_button: toggleBtn) = state
      ) do
    :ttb.stop(:nofetch)
    :wxToggleButton.setLabel(toggleBtn, 'Start Trace')
    :wxToggleButton.setValue(toggleBtn, false)
    {:noreply, r_state(state, logwin: false)}
  end

  def handle_event(
        r_wx(id: id, obj: logWin, event: ev),
        r_state(toggle_button: toggleBtn, logwin: latest) = state
      )
      when id === 380 or elem(ev, 0) === :wxClose do
    case logWin do
      ^latest ->
        :ttb.stop(:nofetch)
        :wxToggleButton.setLabel(toggleBtn, 'Start Trace')
        :wxToggleButton.setValue(toggleBtn, false)
        {:noreply, r_state(state, logwin: false)}

      _ ->
        {:noreply, state}
    end
  end

  def handle_event(r_wx(id: 382, userData: tCtrl), state) do
    :wxTextCtrl.clear(tCtrl)
    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 381, userData: tCtrl),
        r_state(panel: panel) = state
      ) do
    dialog = :wxFileDialog.new(panel, [{:style, 2 ||| 4}])

    case :wxFileDialog.showModal(dialog) do
      5100 ->
        path = :wxFileDialog.getPath(dialog)
        :wxDialog.destroy(dialog)
        :wxTextCtrl.saveFile(tCtrl, [{:file, path}])

      _ ->
        :wxDialog.destroy(dialog)
        :ok
    end

    {:noreply, state}
  end

  def handle_event(r_wx(id: 305), r_state(panel: panel) = state) do
    dialog = :wxFileDialog.new(panel, [{:style, 2 ||| 4}])

    case :wxFileDialog.showModal(dialog) do
      5100 ->
        path = :wxFileDialog.getPath(dialog)
        write_file(panel, path, get_config(state))

      _ ->
        :ok
    end

    :wxDialog.destroy(dialog)
    {:noreply, state}
  end

  def handle_event(r_wx(id: 306), r_state(panel: panel) = state) do
    dialog = :wxFileDialog.new(panel, [{:style, 16}])

    state2 =
      case :wxFileDialog.showModal(dialog) do
        5100 ->
          path = :wxFileDialog.getPath(dialog)
          read_settings(path, state)

        _ ->
          state
      end

    :wxDialog.destroy(dialog)
    {:noreply, state2}
  end

  def handle_event(
        r_wx(
          id: 340,
          event: r_wxList(type: :command_list_item_right_click)
        ),
        state = r_state(panel: panel, proc_view: lCtrl, tpids: tpids, n_view: nview, nodes: nodes)
      ) do
    case get_visible_ps(tpids, nodes, nview) do
      [] ->
        :ok

      visible ->
        case get_selected_items(lCtrl, visible) do
          [] ->
            :ok

          _ ->
            create_right_click_menu(panel, [
              {341, 'Edit process options'},
              {342, 'Remove processes'}
            ])
        end
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 350,
          event: r_wxList(type: :command_list_item_right_click)
        ),
        state =
          r_state(panel: panel, port_view: lCtrl, tports: tports, n_view: nview, nodes: nodes)
      ) do
    case get_visible_ps(tports, nodes, nview) do
      [] ->
        :ok

      visible ->
        case get_selected_items(lCtrl, visible) do
          [] ->
            :ok

          _ ->
            create_right_click_menu(panel, [{351, 'Edit port options'}, {352, 'Remove ports'}])
        end
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 360,
          event: r_wxList(type: :command_list_item_right_click)
        ),
        state = r_state(panel: panel, m_view: mview, tpatterns: tPs)
      ) do
    case get_selected_items(
           mview,
           :lists.sort(:dict.fetch_keys(tPs))
         ) do
      [] ->
        :ok

      _ ->
        create_right_click_menu(panel, [{361, 'Remove trace patterns'}])
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 370,
          event: r_wxList(type: :command_list_item_right_click)
        ),
        state = r_state(panel: panel, m_view: mview, f_view: fview, tpatterns: tPs)
      ) do
    case get_selected_items(
           mview,
           :lists.sort(:dict.fetch_keys(tPs))
         ) do
      [] ->
        :ok

      [module] ->
        case get_selected_items(
               fview,
               :dict.fetch(module, tPs)
             ) do
          [] ->
            :ok

          _ ->
            create_right_click_menu(panel, [
              {371, 'Edit matchspecs'},
              {372, 'Remove trace patterns'}
            ])
        end
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(
          id: 330,
          event: r_wxList(type: :command_list_item_right_click)
        ),
        state = r_state(panel: panel, n_view: nview, nodes: nodes)
      ) do
    menu =
      case get_selected_items(nview, nodes) do
        [] ->
          [{331, 'Add nodes'}]

        _ ->
          [{331, 'Add nodes'}, {332, 'Remove nodes'}]
      end

    create_right_click_menu(panel, menu)
    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 341),
        r_state(panel: panel, tpids: tpids, proc_view: procs) = state
      ) do
    try do
      [
        r_titem(opts: defOpts)
        | _
      ] = selected = get_selected_items(procs, tpids)

      opts =
        :observer_traceoptions_wx.process_trace(
          panel,
          defOpts
        )

      changed =
        for tpid <- selected do
          r_titem(tpid, opts: opts)
        end

      {:noreply,
       do_add_processes(
         changed,
         r_state(state, def_proc_flags: opts)
       )}
    catch
      _, _ ->
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(id: 342),
        r_state(tpids: tpids, proc_view: lCtrl, n_view: nview, nodes: nodes) = state
      ) do
    selected = get_selected_items(lCtrl, tpids)
    pids = tpids -- selected
    update_p_view(pids, lCtrl, nodes, nview)
    {:noreply, r_state(state, tpids: pids)}
  end

  def handle_event(
        r_wx(id: 351),
        r_state(panel: panel, tports: tports, port_view: ports) = state
      ) do
    try do
      [
        r_titem(opts: defOpts)
        | _
      ] = selected = get_selected_items(ports, tports)

      opts =
        :observer_traceoptions_wx.port_trace(
          panel,
          defOpts
        )

      changed =
        for tport <- selected do
          r_titem(tport, opts: opts)
        end

      {:noreply, do_add_ports(changed, r_state(state, def_port_flags: opts))}
    catch
      _, _ ->
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(id: 352),
        r_state(tports: tports, port_view: lCtrl, n_view: nview, nodes: nodes) = state
      ) do
    selected = get_selected_items(lCtrl, tports)
    ports = tports -- selected
    update_p_view(ports, lCtrl, nodes, nview)
    {:noreply, r_state(state, tports: ports)}
  end

  def handle_event(
        r_wx(id: 315),
        r_state(panel: panel, def_proc_flags: pO) = state
      ) do
    try do
      opts =
        :observer_traceoptions_wx.process_trace(
          panel,
          pO
        )

      {:noreply, r_state(state, def_proc_flags: opts)}
    catch
      _, _ ->
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(id: 316),
        r_state(panel: panel, def_port_flags: pO) = state
      ) do
    try do
      opts = :observer_traceoptions_wx.port_trace(panel, pO)
      {:noreply, r_state(state, def_port_flags: opts)}
    catch
      _, _ ->
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(id: 312),
        r_state(panel: panel, match_specs: ms) = state
      ) do
    try do
      :observer_traceoptions_wx.select_matchspec(self(), panel, ms, :funcs)
    catch
      _, _ ->
        :cancel
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 313),
        r_state(panel: panel, match_specs: ms) = state
      ) do
    try do
      :observer_traceoptions_wx.select_matchspec(self(), panel, ms, :send)
    catch
      _, _ ->
        :cancel
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 314),
        r_state(panel: panel, match_specs: ms) = state
      ) do
    try do
      :observer_traceoptions_wx.select_matchspec(self(), panel, ms, :receive)
    catch
      _, _ ->
        :cancel
    end

    {:noreply, state}
  end

  def handle_event(
        r_wx(id: 371),
        r_state(panel: panel, tpatterns: tPs, f_view: lCtrl, m_view: mview, match_specs: mss) =
          state
      ) do
    try do
      case get_selected_items(
             mview,
             :lists.sort(:dict.fetch_keys(tPs))
           ) do
        [] ->
          throw({:error, 'No module selected'})

        [module] ->
          selected =
            get_selected_items(
              lCtrl,
              :dict.fetch(module, tPs)
            )

          key =
            case module do
              :Events ->
                selectedEvents =
                  for r_tpattern(fa: event) <- selected do
                    event
                  end

                e1 = hd(selectedEvents)

                case :lists.all(
                       fn
                         e when e == e1 ->
                           true

                         _ ->
                           false
                       end,
                       selectedEvents
                     ) do
                  true ->
                    e1

                  false ->
                    throw({:error, 'Can not set match specs for multiple event types'})
                end

              _ ->
                :funcs
            end

          ms = :observer_traceoptions_wx.select_matchspec(self(), panel, mss, key)

          changed =
            for tP <- selected do
              r_tpattern(tP, ms: ms)
            end

          {:noreply, do_add_patterns({module, changed}, state)}
      end
    catch
      {:error, msg} ->
        :observer_wx.create_txt_dialog(panel, msg, 'Error', 512)
        {:noreply, state}

      :cancel ->
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(id: 372),
        r_state(tpatterns: tPs0, f_view: lCtrl, m_view: mview) = state
      ) do
    case get_selected_items(
           mview,
           :lists.sort(:dict.fetch_keys(tPs0))
         ) do
      [] ->
        {:noreply, state}

      [module] ->
        fMs0 = :dict.fetch(module, tPs0)
        selected = get_selected_items(lCtrl, fMs0)
        fMs = fMs0 -- selected
        update_functions_view(fMs, lCtrl)

        tPs =
          case fMs do
            [] ->
              new = :dict.erase(module, tPs0)
              update_modules_view(:lists.sort(:dict.fetch_keys(new)), module, mview)
              new

            _ ->
              :dict.store(module, fMs, tPs0)
          end

        {:noreply, r_state(state, tpatterns: tPs)}
    end
  end

  def handle_event(
        r_wx(id: 361),
        r_state(tpatterns: tPs0, f_view: lCtrl, m_view: mview) = state
      ) do
    case get_selected_items(
           mview,
           :lists.sort(:dict.fetch_keys(tPs0))
         ) do
      [] ->
        {:noreply, state}

      [module] ->
        update_functions_view([], lCtrl)
        tPs = :dict.erase(module, tPs0)
        update_modules_view(:lists.sort(:dict.fetch_keys(tPs)), module, mview)
        {:noreply, r_state(state, tpatterns: tPs)}
    end
  end

  def handle_event(
        r_wx(id: 311),
        r_state(panel: panel, output: out0) = state
      ) do
    try do
      out = :observer_traceoptions_wx.output(panel, out0)
      {:noreply, r_state(state, output: out)}
    catch
      _, _ ->
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(id: 331),
        r_state(panel: panel, n_view: nview, nodes: ns0) = state
      ) do
    try do
      possible = [node() | :erlang.nodes()] -- ns0

      case possible do
        [] ->
          msg =
            'Already selected all connected nodes\nUse the Nodes menu to connect to new nodes first.'

          :observer_wx.create_txt_dialog(panel, msg, 'No available nodes', 2048)
          throw(:cancel)

        _ ->
          ns =
            :lists.usort(
              ns0 ++
                :observer_traceoptions_wx.select_nodes(
                  panel,
                  possible
                )
            )

          update_nodes_view(ns, nview)
          {:noreply, r_state(state, nodes: ns)}
      end
    catch
      :cancel ->
        {:noreply, state}
    end
  end

  def handle_event(
        r_wx(id: 332),
        r_state(n_view: nview, nodes: ns0) = state
      ) do
    sel = get_selected_items(nview, ns0)
    ns = ns0 -- sel
    update_nodes_view(ns, nview)
    {:noreply, r_state(state, nodes: ns)}
  end

  def handle_event(r_wx(id: iD, event: what), state) do
    :io.format('~p:~p: Unhandled event: ~p, ~tp ~n', [:observer_trace_wx, 688, iD, what])
    {:noreply, state}
  end

  def handle_call(:get_config, _, state) do
    config0 = get_config(state)
    config = :lists.keydelete(:trace_p, 1, config0)
    {:reply, :maps.from_list(config), state}
  end

  def handle_call(msg, from, _State) do
    :erlang.error({:unhandled_call, msg, from})
  end

  def handle_cast(
        {:add_processes, pids},
        state = r_state(panel: parent, def_proc_flags: traceOpts)
      ) do
    try do
      opts =
        :observer_traceoptions_wx.process_trace(
          parent,
          traceOpts
        )

      pOpts =
        for pid <- pids do
          r_titem(id: pid, opts: opts)
        end

      s =
        do_add_processes(
          pOpts,
          r_state(state, def_proc_flags: opts)
        )

      {:noreply, s}
    catch
      :cancel ->
        {:noreply, state}
    end
  end

  def handle_cast(
        {:add_ports, ports},
        state = r_state(panel: parent, def_port_flags: traceOpts)
      ) do
    try do
      opts =
        :observer_traceoptions_wx.port_trace(
          parent,
          traceOpts
        )

      pOpts =
        for id <- ports do
          r_titem(id: id, opts: opts)
        end

      s = do_add_ports(pOpts, r_state(state, def_port_flags: opts))
      {:noreply, s}
    catch
      :cancel ->
        {:noreply, state}
    end
  end

  def handle_cast(msg, _State) do
    :erlang.error({:unhandled_cast, msg})
  end

  def handle_info({:active, _Node}, state = r_state(parent: parent)) do
    create_menues(parent)
    {:noreply, state}
  end

  def handle_info(:not_active, state) do
    {:noreply, state}
  end

  def handle_info({:update_ms, newMs}, state) do
    {:noreply, r_state(state, match_specs: newMs)}
  end

  def handle_info(any, state) do
    :io.format('~p~p: received unexpected message: ~tp\n', [:observer_trace_wx, self(), any])
    {:noreply, state}
  end

  def terminate(_Reason, r_state(nodes: _Nodes)) do
    :ttb.stop(:nofetch)
    :ok
  end

  def code_change(_, _, state) do
    {:ok, state}
  end

  defp do_add_patterns(
         {module, newPs},
         state = r_state(tpatterns: tPs0, m_view: mview, f_view: fview)
       ) do
    old =
      case :dict.find(module, tPs0) do
        {:ok, prev} ->
          prev

        :error ->
          []
      end

    case merge_patterns(newPs, old) do
      {^old, [], []} ->
        state

      {mPatterns, _New, _Changed} ->
        tPs = :dict.store(module, mPatterns, tPs0)
        update_modules_view(:lists.sort(:dict.fetch_keys(tPs)), module, mview)
        update_functions_view(:dict.fetch(module, tPs), fview)
        r_state(state, tpatterns: tPs)
    end
  end

  defp do_add_processes(
         pOpts,
         s0 = r_state(n_view: nview, proc_view: lCtrl, tpids: oldPids, nodes: oldNodes)
       ) do
    checkFun = fn pid ->
      is_pid(pid)
    end

    {pids, nodes} = do_add_pid_or_port(pOpts, nview, lCtrl, oldPids, oldNodes, checkFun)
    r_state(s0, tpids: pids, nodes: nodes)
  end

  defp do_add_ports(
         pOpts,
         s0 = r_state(n_view: nview, port_view: lCtrl, tports: oldPorts, nodes: oldNodes)
       ) do
    checkFun = fn port ->
      is_port(port)
    end

    {ports, nodes} = do_add_pid_or_port(pOpts, nview, lCtrl, oldPorts, oldNodes, checkFun)
    r_state(s0, tports: ports, nodes: nodes)
  end

  defp do_add_pid_or_port(pOpts, nview, lCtrl, oldPs, ns0, check) do
    case merge_trace_items(pOpts, oldPs) do
      {^oldPs, [], []} ->
        {oldPs, ns0}

      {ps, new, _Changed} ->
        ns1 =
          :lists.usort(
            for r_titem(id: id) <- new, check.(id) do
              node(id)
            end
          )

        nodes =
          case :ordsets.subtract(ns1, ns0) do
            [] when ns0 == [] ->
              [:observer_wx.get_active_node()]

            [] ->
              ns0

            newNs ->
              :ordsets.union(newNs, ns0)
          end

        update_nodes_view(nodes, nview)
        update_p_view(ps, lCtrl, nodes, nview)
        {ps, nodes}
    end
  end

  defp get_visible_ps(pidsOrPorts, [node], _Nview) do
    get_visible_ps(pidsOrPorts, [node])
  end

  defp get_visible_ps(pidsOrPorts, nodes, nview) do
    get_visible_ps(
      pidsOrPorts,
      get_selected_items(nview, nodes)
    )
  end

  defp get_visible_ps(pidsOrPorts, nodes) do
    for p <- pidsOrPorts,
        is_atom(r_titem(p, :id)) or
          :lists.member(
            node(r_titem(p, :id)),
            nodes
          ) do
      p
    end
  end

  defp update_p_view(pidsOrPorts, lCtrl, nodes, nview) do
    update_p_view(
      get_visible_ps(pidsOrPorts, nodes, nview),
      lCtrl
    )
  end

  defp update_p_view(pidsOrPorts, lCtrl, nodes) do
    update_p_view(get_visible_ps(pidsOrPorts, nodes), lCtrl)
  end

  defp update_p_view(pidsOrPorts, lCtrl) do
    :wxListCtrl.deleteAllItems(lCtrl)

    :wx.foldl(
      fn r_titem(id: id, opts: opts), row ->
        _Item = :wxListCtrl.insertItem(lCtrl, row, '')

        rem(row, 2) === 0 and
          :wxListCtrl.setItemBackgroundColour(
            lCtrl,
            row,
            {230, 230, 250}
          )

        :wxListCtrl.setItem(lCtrl, row, 0, :observer_lib.to_str(id))
        :wxListCtrl.setItem(lCtrl, row, 1, :observer_lib.to_str(opts))
        row + 1
      end,
      0,
      pidsOrPorts
    )

    case pidsOrPorts do
      [] ->
        :wxListCtrl.setToolTip(lCtrl, 'Add items from Processes/Ports tab')

      _ ->
        :wxListCtrl.setToolTip(lCtrl, 'Select nodes to see traced processes and ports')
    end
  end

  defp update_nodes_view(nodes, lCtrl) do
    selected =
      case nodes do
        [_] ->
          nodes

        _ ->
          get_selected_items(lCtrl, nodes)
      end

    :wxListCtrl.deleteAllItems(lCtrl)

    :wx.foldl(
      fn node, row ->
        _Item = :wxListCtrl.insertItem(lCtrl, row, '')

        rem(row, 2) === 0 and
          :wxListCtrl.setItemBackgroundColour(
            lCtrl,
            row,
            {230, 230, 250}
          )

        :wxListCtrl.setItem(lCtrl, row, 0, :observer_lib.to_str(node))

        :lists.member(
          node,
          selected
        ) and
          :wxListCtrl.setItemState(
            lCtrl,
            row,
            65535,
            4
          )

        row + 1
      end,
      0,
      nodes
    )

    case nodes do
      [] ->
        :wxListCtrl.setToolTip(lCtrl, 'Right click to add nodes')

      _ ->
        :wxListCtrl.setToolTip(lCtrl, 'Select nodes to see traced processes and ports')
    end
  end

  defp update_modules_view(mods, module, lCtrl) do
    :wxListCtrl.deleteAllItems(lCtrl)

    :wx.foldl(
      fn mod, row ->
        _Item = :wxListCtrl.insertItem(lCtrl, row, '')

        rem(row, 2) === 0 and
          :wxListCtrl.setItemBackgroundColour(
            lCtrl,
            row,
            {230, 230, 250}
          )

        :wxListCtrl.setItem(lCtrl, row, 0, :observer_lib.to_str(mod))
        mod === module and :wxListCtrl.setItemState(lCtrl, row, 65535, 4)
        row + 1
      end,
      0,
      mods
    )

    parent = :wxListCtrl.getParent(lCtrl)

    case mods do
      [] ->
        :wxListCtrl.setToolTip(parent, 'Add trace pattern with button below')

      _ ->
        :wxListCtrl.setToolTip(parent, 'Select module to see trace patterns')
    end
  end

  defp update_functions_view(funcs, lCtrl) do
    :wxListCtrl.deleteAllItems(lCtrl)

    :wx.foldl(
      fn r_tpattern(m: m, fa: fA, ms: r_match_spec(str: ms)), row ->
        _Item = :wxListCtrl.insertItem(lCtrl, row, '')

        rem(row, 2) === 0 and
          :wxListCtrl.setItemBackgroundColour(
            lCtrl,
            row,
            {230, 230, 250}
          )

        funcStr =
          case m do
            :Events ->
              :observer_lib.to_str(fA)

            _ ->
              :observer_lib.to_str({:func, fA})
          end

        :wxListCtrl.setItem(lCtrl, row, 0, funcStr)
        :wxListCtrl.setItem(lCtrl, row, 1, ms)
        row + 1
      end,
      0,
      funcs
    )
  end

  defp merge_trace_items(
         [n1 = r_titem(id: newP) | ns],
         [n2 = r_titem(id: newP) | old]
       )
       when newP == :new_processes or newP == :new_ports do
    {ids, new, changed} = merge_trace_items_1(ns, old)
    {[n1 | ids], new, [{n2, n2} | changed]}
  end

  defp merge_trace_items([n1 = r_titem(id: newP) | ns], old)
       when newP == :new_processes or newP == :new_ports do
    {ids, new, changed} = merge_trace_items_1(ns, old)
    {[n1 | ids], [n1 | new], changed}
  end

  defp merge_trace_items(ns, [n2 = r_titem(id: newP) | old])
       when newP == :new_processes or newP == :new_ports do
    {ids, new, changed} = merge_trace_items_1(ns, old)
    {[n2 | ids], new, changed}
  end

  defp merge_trace_items(new, old) do
    merge_trace_items_1(new, old)
  end

  defp merge_trace_items_1(new, old) do
    merge(:lists.sort(new), old, r_titem(:id), [], [], [])
  end

  defp merge_patterns(new, old) do
    merge(:lists.sort(new), old, r_tpattern(:fa), [], [], [])
  end

  defp merge([n | ns], [n | os], el, new, ch, all) do
    merge(ns, os, el, new, ch, [n | all])
  end

  defp merge([n | ns], [o | os], el, new, ch, all)
       when :erlang.element(el, n) == :erlang.element(el, o) do
    merge(ns, os, el, new, [{o, n} | ch], [n | all])
  end

  defp merge([n | ns], os = [o | _], el, new, ch, all)
       when :erlang.element(el, n) < :erlang.element(el, o) do
    merge(ns, os, el, [n | new], ch, [n | all])
  end

  defp merge(ns = [n | _], [o | os], el, new, ch, all)
       when :erlang.element(el, n) > :erlang.element(el, o) do
    merge(ns, os, el, new, ch, [o | all])
  end

  defp merge([], os, _El, new, ch, all) do
    {:lists.reverse(all, os), new, ch}
  end

  defp merge(ns, [], _El, new, ch, all) do
    {:lists.reverse(all, ns), ns ++ new, ch}
  end

  defp ttb_output_args(parent, opts) do
    toWindow = :proplists.get_value(:window, opts, true)
    toShell = :proplists.get_value(:shell, opts, false)
    toFile = :proplists.get_value(:file, opts, false)
    toWindow or toShell or toFile or throw({:error, 'No output of trace'})
    {logWin, text} = create_logwindow(parent, toWindow)
    write = output_fun(text, toShell)
    shell = output_shell(toFile, write)
    fileOpts = output_file(toFile, :proplists.get_value(:wrap, opts, false), opts)
    {[{:file, {:local, fileOpts}} | shell], logWin}
  end

  defp output_shell(true, false) do
    []
  end

  defp output_shell(true, write) when is_function(write) do
    [{:shell, write}]
  end

  defp output_shell(false, write) when is_function(write) do
    [{:shell, {:only, write}}]
  end

  defp output_fun(false, false) do
    false
  end

  defp output_fun(false, true) do
    fn trace ->
      :io.put_chars(textformat(trace))
    end
  end

  defp output_fun(text, false) do
    env = :wx.get_env()

    fn trace ->
      :wx.set_env(env)
      :wxTextCtrl.appendText(text, textformat(trace))
    end
  end

  defp output_fun(text, true) do
    env = :wx.get_env()

    fn trace ->
      :wx.set_env(env)
      ioList = textformat(trace)
      :wxTextCtrl.appendText(text, ioList)
      :io.put_chars(textformat(trace))
    end
  end

  defp output_file(false, _, _Opts) do
    'ttb'
  end

  defp output_file(true, false, opts) do
    :proplists.get_value(:filename, opts, 'ttb')
  end

  defp output_file(true, true, opts) do
    name = :proplists.get_value(:filename, opts, 'ttb')
    size = :proplists.get_value(:wrap_sz, opts, 128)
    count = :proplists.get_value(:wrap_c, opts, 8)
    {:wrap, name, size * 1024, count}
  end

  defp create_logwindow(_Parent, false) do
    {false, false}
  end

  defp create_logwindow(parent, true) do
    scale = :observer_wx.get_scale()
    logWin = :wxFrame.new(parent, 380, 'Trace Log', [{:size, {750 * scale, 800 * scale}}])
    mB = :wxMenuBar.new()
    file = :wxMenu.new()
    :wxMenu.append(file, 382, 'Clear Log\tCtrl-C')
    :wxMenu.append(file, 381, 'Save Log\tCtrl-S')
    :wxMenu.append(file, 5001, 'Close')
    :wxMenuBar.append(mB, file, 'File')
    :wxFrame.setMenuBar(logWin, mB)
    text = :wxTextCtrl.new(logWin, -1, [{:style, 32 ||| 32768 ||| 1_073_741_824 ||| 16}])
    font = :observer_wx.get_attrib({:font, :fixed})

    attr =
      :wxTextAttr.new(
        :wxe_util.get_const(:wxBLACK),
        [{:font, font}]
      )

    true = :wxTextCtrl.setDefaultStyle(text, attr)
    :wxFrame.connect(logWin, :close_window, [{:skip, true}])
    :wxFrame.connect(logWin, :command_menu_selected, [{:userData, text}])
    :wxFrame.show(logWin)
    {logWin, text}
  end

  defp setup_ttb(tPs, tPids, tPorts) do
    _R1 =
      for {_, fTP} <- tPs do
        setup_tps(fTP, [])
      end

    _R2 =
      for r_titem(id: pid, opts: flags) <- tPids do
        :ttb.p(pid, dbg_flags(:proc, flags))
      end

    _R3 =
      for r_titem(id: port, opts: flags) <- tPorts do
        :ttb.p(port, dbg_flags(:port, flags))
      end

    :ok
  end

  defp setup_tps([first = r_tpattern(fa: {_, :_}) | rest], prev) do
    setup_tp(first)

    for tP <- :lists.reverse(prev) do
      setup_tp(tP)
    end

    setup_tps(rest, [])
  end

  defp setup_tps(
         [first = r_tpattern(fa: {f, _}) | rest],
         prev = [r_tpattern(fa: {f, _}) | _]
       ) do
    setup_tps(rest, [first | prev])
  end

  defp setup_tps([first | rest], prev) do
    for tP <- :lists.reverse(prev) do
      setup_tp(tP)
    end

    setup_tps(rest, [first])
  end

  defp setup_tps([], prev) do
    for tP <- :lists.reverse(prev) do
      setup_tp(tP)
    end
  end

  defp setup_tp(r_tpattern(m: :Events, fa: event, ms: r_match_spec(term: ms))) do
    :ttb.tpe(event, ms)
  end

  defp setup_tp(r_tpattern(m: m, fa: {f, a}, ms: r_match_spec(term: ms))) do
    :ttb.tpl(m, f, a, ms)
  end

  defp dbg_flags(type, flags) do
    for flag <- flags do
      dbg_flag(type, flag)
    end
  end

  defp dbg_flag(_, :send) do
    :s
  end

  defp dbg_flag(_, :receive) do
    :r
  end

  defp dbg_flag(:proc, :functions) do
    :c
  end

  defp dbg_flag(:proc, :on_spawn) do
    :sos
  end

  defp dbg_flag(:proc, :on_link) do
    :sol
  end

  defp dbg_flag(:proc, :on_first_spawn) do
    :sofs
  end

  defp dbg_flag(:proc, :on_first_link) do
    :sofl
  end

  defp dbg_flag(:proc, :events) do
    :p
  end

  defp dbg_flag(:port, :events) do
    :ports
  end

  defp dbg_flag(_, flag) do
    flag
  end

  defp textformat(trace)
       when :erlang.element(
              1,
              trace
            ) == :trace_ts and
              tuple_size(trace) >= 4 do
    format_trace(trace, tuple_size(trace) - 1, :erlang.element(tuple_size(trace), trace))
  end

  defp textformat(trace)
       when :erlang.element(
              1,
              trace
            ) == :drop and
              tuple_size(trace) === 2 do
    :io_lib.format('*** Dropped ~p messages.~n', [:erlang.element(2, trace)])
  end

  defp textformat(trace)
       when :erlang.element(
              1,
              trace
            ) == :seq_trace and
              tuple_size(trace) >= 3 do
    :io_lib.format('*** Seq trace not implmented.~n', [])
  end

  defp textformat(_) do
    ''
  end

  defp format_trace(trace, size, tS0 = {_, _, mS}) do
    {_, {h, m, s}} = :calendar.now_to_local_time(tS0)
    tS = :io_lib.format('~.2.0w:~.2.0w:~.2.0w:~.6.0w', [h, m, s, mS])
    from = :erlang.element(2, trace)

    case :erlang.element(3, trace) do
      :receive ->
        case :erlang.element(4, trace) do
          {:dbg, :ok} ->
            ''

          message ->
            :io_lib.format('~s (~100p) << ~100tp~n', [tS, from, message])
        end

      :send ->
        message = :erlang.element(4, trace)
        to = :erlang.element(5, trace)
        :io_lib.format('~s (~100p) ~100p ! ~100tp~n', [tS, from, to, message])

      :call ->
        case :erlang.element(4, trace) do
          mFA when size == 5 ->
            message = :erlang.element(5, trace)
            :io_lib.format('~s (~100p) call ~ts (~100tp) ~n', [tS, from, ffunc(mFA), message])

          mFA ->
            :io_lib.format('~s (~100p) call ~ts~n', [tS, from, ffunc(mFA)])
        end

      :return_from ->
        mFA = :erlang.element(4, trace)
        ret = :erlang.element(5, trace)
        :io_lib.format('~s (~100p) returned from ~ts -> ~100tp~n', [tS, from, ffunc(mFA), ret])

      :return_to ->
        mFA = :erlang.element(4, trace)
        :io_lib.format('~s (~100p) returning to ~ts~n', [tS, from, ffunc(mFA)])

      :spawn when size == 5 ->
        pid = :erlang.element(4, trace)
        mFA = :erlang.element(5, trace)
        :io_lib.format('~s (~100p) spawn ~100p as ~ts~n', [tS, from, pid, ffunc(mFA)])

      op ->
        :io_lib.format('~s (~100p) ~100p ~ts~n', [tS, from, op, ftup(trace, 4, size)])
    end
  end

  defp ffunc({m, f, argl}) when is_list(argl) do
    :io_lib.format('~100p:~100tp(~ts)', [m, f, fargs(argl)])
  end

  defp ffunc({m, f, arity}) do
    :io_lib.format('~100p:~100tp/~100p', [m, f, arity])
  end

  defp ffunc(x) do
    :io_lib.format('~100tp', [x])
  end

  defp fargs(arity) when is_integer(arity) do
    :erlang.integer_to_list(arity)
  end

  defp fargs([]) do
    []
  end

  defp fargs([a]) do
    :io_lib.format('~100tp', [a])
  end

  defp fargs([a | args]) do
    [:io_lib.format('~100tp,', [a]) | fargs(args)]
  end

  defp fargs(a) do
    :io_lib.format('~100tp', [a])
  end

  defp ftup(trace, index, index) do
    :io_lib.format('~100tp', [:erlang.element(index, trace)])
  end

  defp ftup(trace, index, size) do
    [
      :io_lib.format('~100tp ', [:erlang.element(index, trace)])
      | ftup(trace, index + 1, size)
    ]
  end

  defp get_config(
         r_state(
           def_proc_flags: procFlags,
           def_port_flags: portFlags,
           match_specs: matchSpecs0,
           tpatterns: tracePatterns,
           output: output
         )
       ) do
    mSToList = fn r_match_spec(name: id, term: t, func: f) ->
      [{:name, id}, {:term, t}, {:func, f}]
    end

    matchSpecs =
      for {key, mSs} <- matchSpecs0 do
        {:ms, key,
         for mS <- mSs do
           mSToList.(mS)
         end}
      end

    tPToTuple = fn r_tpattern(fa: {f, a}, ms: ms) ->
      {f, a, mSToList.(ms)}
    end

    moduleTermList =
      for {module, fTPs} <- :dict.to_list(tracePatterns) do
        {:tp, module,
         for fTP <- fTPs do
           tPToTuple.(fTP)
         end}
      end

    [
      {:procflags, procFlags},
      {:portflags, portFlags},
      {:match_specs, matchSpecs},
      {:output, output},
      {:trace_p, moduleTermList}
    ]
  end

  defp write_file(frame, filename, config) do
    str = [
      '%%% ',
      :epp.encoding_to_string(:utf8),
      '\n%%%\n%%% This file is generated by Observer\n',
      '%%%\n%%% DO NOT EDIT!\n%%%\n',
      for mSTerm <-
            :proplists.get_value(
              :match_specs,
              config
            ) do
        :io_lib.format(
          '~tp.~n',
          [mSTerm]
        )
      end,
      :io_lib.format(
        '~p.~n',
        [
          :lists.keyfind(
            :procflags,
            1,
            config
          )
        ]
      ),
      :io_lib.format(
        '~p.~n',
        [
          :lists.keyfind(
            :portflags,
            1,
            config
          )
        ]
      ),
      :io_lib.format(
        '~tp.~n',
        [
          :lists.keyfind(
            :output,
            1,
            config
          )
        ]
      ),
      for moduleTerm <-
            :proplists.get_value(
              :trace_p,
              config
            ) do
        :io_lib.format(
          '~tp.~n',
          [moduleTerm]
        )
      end
    ]

    case :file.write_file(
           filename,
           :unicode.characters_to_binary(str)
         ) do
      :ok ->
        :success

      {:error, reason} ->
        failMsg = :file.format_error(reason)
        :observer_wx.create_txt_dialog(frame, failMsg, 'Error', 512)
    end
  end

  defp read_settings(
         filename,
         r_state(match_specs: ms0, def_proc_flags: procFs0, def_port_flags: portFs0) = state
       ) do
    case :file.consult(filename) do
      {:ok, terms} ->
        ms = parse_ms(terms, ms0)

        procFs1 =
          :proplists.get_value(:procflags, terms, []) ++
            :proplists.get_value(
              :traceopts,
              terms,
              []
            )

        procFs = :lists.usort(procFs0 ++ procFs1)
        portFs = :lists.usort(portFs0 ++ :proplists.get_value(:portflags, terms, []))
        out = :proplists.get_value(:output, terms, [])

        :lists.foldl(
          &parse_tp/2,
          r_state(state,
            match_specs: ms,
            def_proc_flags: procFs,
            def_port_flags: portFs,
            output: out
          ),
          terms
        )

      {:error, _} ->
        :observer_wx.create_txt_dialog(
          r_state(state, :panel),
          'Could not load settings',
          'Error',
          512
        )

        state
    end
  end

  defp parse_ms(terms, oldMSs) do
    mSs =
      case (for {:ms, key, mSs} <- terms do
              {key,
               for mS <- mSs do
                 make_ms(mS)
               end}
            end) do
        [] ->
          case (for {:ms, mS} <- terms do
                  make_ms(mS)
                end) do
            [] ->
              []

            funcMSs ->
              [{:funcs, funcMSs}]
          end

        keyMSs ->
          keyMSs
      end

    parse_ms_1(mSs, :dict.from_list(oldMSs))
  end

  defp parse_ms_1([{key, mSs} | t], dict) do
    parse_ms_1(t, :dict.append_list(key, mSs, dict))
  end

  defp parse_ms_1([], dict) do
    for {key, mSs} <- :dict.to_list(dict) do
      {key, rm_dups(mSs, [])}
    end
  end

  defp rm_dups([h | t], acc) do
    case :lists.member(h, acc) do
      true ->
        rm_dups(t, acc)

      false ->
        rm_dups(t, [h | acc])
    end
  end

  defp rm_dups([], acc) do
    :lists.reverse(acc)
  end

  defp make_ms(mS) do
    [{:func, funStr}, {:name, name}, {:term, term}] = :lists.keysort(1, mS)
    make_ms(name, term, funStr)
  end

  defp make_ms(name, term, funStr) do
    r_match_spec(name: name, term: term, str: :io_lib.format('~tw', [term]), func: funStr)
  end

  defp parse_tp({:tp, mod, fAs}, state) do
    patterns =
      for {f, a, list} <- fAs do
        r_tpattern(m: mod, fa: {f, a}, ms: make_ms(list))
      end

    do_add_patterns({mod, patterns}, state)
  end

  defp parse_tp(_, state) do
    state
  end

  defp get_selected_items(grid, data) do
    get_indecies(get_selected_items(grid, -1, []), data)
  end

  defp get_selected_items(grid, index, itemAcc) do
    item = :wxListCtrl.getNextItem(grid, index, [{:geometry, 1}, {:state, 4}])

    case item do
      -1 ->
        :lists.reverse(itemAcc)

      _ ->
        get_selected_items(grid, item, [item | itemAcc])
    end
  end

  defp get_indecies(items, data) do
    get_indecies(items, 0, data)
  end

  defp get_indecies([i | rest], i, [h | t]) do
    [h | get_indecies(rest, i + 1, t)]
  end

  defp get_indecies(rest = [_ | _], i, [_ | t]) do
    get_indecies(rest, i + 1, t)
  end

  defp get_indecies(_, _, _) do
    []
  end

  defp create_right_click_menu(panel, menus) do
    menu = :wxMenu.new()

    for {id, str} <- menus do
      :wxMenu.append(menu, id, str)
    end

    :wxWindow.popupMenu(panel, menu)
    :wxMenu.destroy(menu)
  end
end
